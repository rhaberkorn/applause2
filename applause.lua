local sndfile = require "sndfile"
local bit = require "bit"
local ffi = require "ffi"
-- According to LuaJIT docs, it makes sense to cache
-- the FFI namespace
local C = ffi.C

-- Make table.new()/table.clear() available (a LuaJIT extension)
require "table.new"
require "table.clear"

-- Useful in order to make the module reloadable
-- NOTE: This is global, so it can be used in reloadable
-- submodules
function cdef_safe(def)
	local state, msg = pcall(ffi.cdef, def)
	if not state then
		io.stderr:write("WARNING: ", msg, "\n")
	end
end

function cdef_include(file)
	local hnd = assert(io.open(file))
	cdef_safe(hnd:read('*a'))
	hnd:close()
end

--
-- Define C functions for benchmarking (POSIX libc)
--
cdef_safe[[
typedef long time_t;

struct timespec {
	time_t	tv_sec;        /* seconds */
	long	tv_nsec;       /* nanoseconds */
};

typedef enum {
	CLOCK_REALTIME =              0,
	CLOCK_MONOTONIC =             1,
	CLOCK_PROCESS_CPUTIME_ID =    2,
	CLOCK_THREAD_CPUTIME_ID =     3,
	CLOCK_MONOTONIC_RAW =         4,
	CLOCK_REALTIME_COARSE =       5,
	CLOCK_MONOTONIC_COARSE =      6,
	CLOCK_BOOTTIME =              7
} clockid_t;

int clock_gettime(clockid_t clk_id, struct timespec *tp);
]]

-- Measure time required to execute fnc()
-- See also Stream:benchmark()
function benchmark(fnc)
	local t1 = ffi.new("struct timespec[1]")
	local t2 = ffi.new("struct timespec[1]")

	-- See Stream:play(): Try to be more realtime-friendly
	collectgarbage("collect")
	local old_pause = collectgarbage("setpause", 100)
	local old_stepmul = collectgarbage("setstepmul", 100)

	C.clock_gettime("CLOCK_PROCESS_CPUTIME_ID", t1)
	fnc()
	C.clock_gettime("CLOCK_PROCESS_CPUTIME_ID", t2)

	collectgarbage("setpause", old_pause)
	collectgarbage("setstepmul", old_stepmul)

	local t1_ms = t1[0].tv_sec*1000 + t1[0].tv_nsec/1000000
	local t2_ms = t2[0].tv_sec*1000 + t2[0].tv_nsec/1000000

	print("Elapsed CPU time: "..tonumber(t2_ms - t1_ms).."ms")
end

--
-- Define the Lua FFI part of Applause's C core.
-- These functions and types are defined in applause.c
-- FIXME: Could be read from a common file.
--
cdef_safe[[
enum applause_audio_state {
	APPLAUSE_AUDIO_OK = 0,
	APPLAUSE_AUDIO_INTERRUPTED,
	APPLAUSE_AUDIO_XRUN,
	APPLAUSE_AUDIO_INVALID_PORT
};

enum applause_audio_state applause_push_sample(int output_port_id,
                                               double sample_double);

// FIXME: Perhaps a struct would be easier to handle?
typedef uint32_t applause_midi_sample;

applause_midi_sample applause_pull_midi_sample(void);

// Useful in various situations
void free(void *ptr);
]]

-- Sample rate
-- This is overwritten by the C core
samplerate = 44100

-- Time units: Convert between time and sample numbers
-- These are functions, so we can round the result
-- automatically
function sec(x) return math.floor(samplerate*(x or 1)) end
function msec(x) return sec((x or 1)/1000) end

-- The sample cache used to implement CachedStream.
-- We don't know how large it must be, but once it is
-- allocated we only table.clear() it.
local sampleCache = {}

-- Reload the main module: Useful for hacking it without
-- restarting applause
function reload()
	dofile "applause.lua"
	collectgarbage()
end

-- FIXME: Inconsistent naming. Use all-lower case for functions
-- and methods?
function DeriveClass(base)
	local class = {base = base}

	if base then
		-- we cannot derive metamethod tables, so we
		-- copy all relevant metamethods
		for _, m in pairs{"len", "tostring",
		                  "add", "sub", "mul", "div",
		                  "mod", "pow", "unm",
		                  "concat", "lt", "le", "gc"} do
			class["__"..m] = base["__"..m]
		end
	end

	-- Metamethods should work even on root class tables
	-- However, this way we cannot use metatables to track the
	-- class inheritance. This is done using the `base` field.
	setmetatable(class, class)

	function class:new(...)
		-- Try to call the parent constructor
		local obj = base and base:new() or {}

		obj.base = self
		setmetatable(obj, self)

		-- Allow constructors to return something else
		-- than an instance of the class.
		return obj.ctor and obj:ctor(...) or obj
	end

	-- The call metamethod is synonymous to :new()
	class.__call = class.new

	-- All objects have the class table as their metatable,
	-- so it must look into the class and possibly invoke metamethods
	-- on the base class.
	-- A simple `class.__index = base` does not work since
	-- we want indexing to create IndexStreams.
	-- NOTE: __index methods get the original table looked up
	-- as their self argument (e.g. the stream object).
	function class:__index(key)
		if type(key) == "string" then
			return rawget(class, key) or (base and base[key])
		end

		-- non-string keys create IndexStreams
		return IndexStream:new(self, key)
	end

	-- Checks whether object is instance of other_class.
	-- Will work with class templates as well.
	-- This is not using getmetatable() since class metatables
	-- point to themselves (see above).
	function class:instanceof(other_class)
		repeat
			-- Better use rawequal() in case we support the
			-- __eq metamethod someday.
			if rawequal(self, other_class) then return true end
			self = self.base
		until not self

		return false
	end

	return class
end

-- Stream base class
Stream = DeriveClass()

function Stream:ctor(value)
	self.value = tonumber(value) or 0
end

-- There is Stream:instanceof(), but testing Stream.is_a_stream
-- is sometimes faster (for generator functions) and can be done
-- without knowing that the table at hand is an object
Stream.is_a_stream = true

-- All streams except the special MuxStream are mono
Stream.channels = 1

-- A stream, produces an infinite number of the same value by default
-- (eternal quietness by default)
function Stream:gtick()
	return function()
		return self.value
	end
end

-- Cache this stream value to avoid recalculation within
-- the same tick (ie. point in time). This may happen when
-- a stream is used multiple times in the same "patch".
-- FIXME: This should be done automatically by an optimizer stage.
-- FIXME: This is counter-productive for simple number streams
-- (anything simpler than a table lookup)
function Stream:cache()
	return CachedStream:new(self)
end

function Stream:rep(repeats)
	return RepeatStream:new(self, repeats)
end

function Stream:map(fnc)
	return MapStream:new(self, fnc)
end
Stream["\u{00A8}"] = Stream.map -- APL Double-Dot

-- Register all unary functions from the math package
-- as stream operations/methods (creates a stream that calls
-- the function on every sample)
for _, fnc in pairs{"abs", "acos", "asin", "atan",
                    "ceil", "cos", "cosh", "deg",
                    "exp", "floor", "log", "log10",
                    "rad", "sin", "sinh", "sqrt",
                    "tan", "tanh"} do
	Stream[fnc] = function(self)
		return self:map(math[fnc])
	end
end

-- Some binary functions from the math package
for _, name in pairs{"min", "max"} do
	local fnc = math[name]

	Stream[name] = function(self, v)
		return type(v) == "number" and
		       self:map(function(x) return fnc(x, v) end) or
		       self:zip(fnc, v)
	end
end

function Stream:bnot()
	return self:map(bit.bnot)
end

-- Register all binary operators of the "bit" module
for _, name in pairs{"bor", "band", "bxor",
                     "lshift", "rshift", "arshift",
                     "rol", "ror"} do
	local fnc = bit[name]

	Stream[name] = function(self, v)
		return type(v) == "number" and
		       self:map(function(x) return fnc(x, v) end) or
		       self:zip(fnc, v)
	end
end

function Stream:clip(min, max)
	min = min or -1
	max = max or 1

	return self:max(min):min(max)
end

-- Scale [-1,+1] signal to [lower,upper]
-- lower is optional and defaults to 0
function Stream:scale(v1, v2)
	local lower = v2 and v1 or 0
	local upper = v2 or v1

	if type(lower) == "number" and type(upper) == "number" then
		return self:map(function(x)
			return (x + 1)*(upper - lower)/2 + lower
		end)
	else
		return (self + 1)*((upper - lower)/2) + lower
	end
end

-- same as Stream:scale() but for values between [0, 127]
-- (ie. MIDI CC values)
-- FIXME: If Stream:CC() would output between [-1, 1], there would be no need
-- for Stream:ccscale().
function Stream:ccscale(v1, v2)
	local lower = v2 and v1 or 0
	local upper = v2 or v1

	if type(lower) == "number" and type(upper) == "number" then
		return self:map(function(x)
			return (x/127)*(upper - lower) + lower
		end)
	else
		return self*((upper - lower)/127) + lower
	end
end

function Stream:scan(fnc)
	return ScanStream:new(self, fnc)
end

function Stream:fold(fnc)
	return FoldStream:new(self, fnc)
end

function Stream:zip(fnc, ...)
	return ZipStream:new(fnc, self, ...)
end

function Stream:sub(i, j)
	return SubStream:new(self, i, j)
end

function Stream:ravel()
	return RavelStream:new(self)
end

function Stream:mix(other, wetness)
	wetness = wetness or 0.5
	return self*(1 - wetness) + other*wetness
end

function Stream:pan(location)
	location = location or 0
	local cached = self:cache()

	if type(location) == "number" then
		return MuxStream:new(cached * (1-math.max(location, 0)),
		                     cached * (1+math.min(location, 0)))
	else
		local location_cached = tostream(location):cache()

		return MuxStream:new(cached * (1-location_cached:max(0)),
		                     cached * (1+location_cached:min(0)))
	end
end

function Stream:delay(length)
	return DelayStream:new(self, length)
end
function Stream:delayx(length, max_length)
	return DelayXStream:new(self, length, max_length)
end

function Stream:echo(length, wetness)
	local cached = self:cache()
	return cached:mix(cached:delay(length), wetness)
end
function Stream:echox(length, wetness, max_length)
	local cached = self:cache()
	return cached:mix(cached:delayx(length, max_length), wetness)
end

-- This is a linear resampler thanks to the
-- semantics of IndexStream
function Stream:resample(factor)
	-- FIXME FIXME FIXME
	-- self:len()-1 is a workaround for a mysterious bug in LuaJIT
	-- (still not fixed in v2.1 branch) where a comparison in
	-- IndexStream would mysteriously fail.
	-- A better workaround would probably be to disable the optimization
	-- responsible...
	return self[line(1, math.floor(self:len() / factor), self:len()-1)]
end

--
-- Wave forms with names derived from ChucK:
-- Can be written freq:SawOsc() or Stream.SawOsc(freq)
-- depending on the use case. The latter form may
-- be useful for constant frequencies.
--

-- Ramp from 0 to 1
function Stream.Phasor(freq, phase)
	phase = phase or 0

	return ScanStream:new(freq, function(accu, f)
		return ((accu or phase) + f/samplerate) % 1
	end)
end

-- Saw tooth wave from -1 to 1
function Stream.SawOsc(freq, phase)
	phase = (phase or 0)*2+1

	return ScanStream:new(freq, function(accu, f)
		return ((accu or phase) + 2*f/samplerate) % 2
	end) - 1
end

function Stream.SinOsc(freq, phase)
	return Stream.Phasor(freq, phase):mul(2*math.pi):sin()
end
Stream["\u{25CB}"] = Stream.SinOsc -- APL Circle

-- Pulse between 0 and 1 in half a period (width = 0.5)
function Stream.PulseOsc(freq, phase)
	return Stream.Phasor(freq, phase):map(function(x)
		return x < 0.5 and 1 or 0
	end)
end

function Stream.SqrOsc(freq, phase)
	return Stream.Phasor(freq, phase):map(function(x)
		return x < 0.5 and 1 or -1
	end)
end

function Stream.TriOsc(freq, phase)
	local abs = math.abs

	return Stream.SawOsc(freq, phase):map(function(x)
		return abs(x)*2 - 1
	end)
end

--
-- Filter shortcuts.
-- They have their own classes
--

function Stream:LPF(freq)
	return LPFStream:new(self, freq)
end

function Stream:HPF(freq)
	return HPFStream:new(self, freq)
end

function Stream:BPF(freq, quality)
	return BPFStream:new(self, freq, quality)
end

function Stream:BRF(freq, quality)
	return BRFStream:new(self, freq, quality)
end

-- Bit crusher effect
function Stream:crush(bits)
	bits = bits or 8
	local floor = math.floor

	return self:map(function(x)
		return floor(x * 2^bits + 0.5) / 2^bits
	end)
end

-- The len() method is the main way to get a stream's
-- length (at least in this code) and classes should overwrite
-- this method.
-- The __len metamethod is also defined but it currently cannot
-- work since Lua 5.1 does not consider a table's metamethod when
-- evaluating the length (#) operator.
function Stream:len()
	return math.huge -- infinity
end

function Stream:play(first_port)
	first_port = first_port or 1
	first_port = first_port - 1

	-- Make sure JIT compilation is turned on for the generator function
	-- and all subfunctions.
	-- This should not be necessary theoretically.
	jit.on(true, true)

	-- Perform garbage collection cycle and tweak it
	-- to be more realtime friendly.
	-- FIXME: Since every stream that does not lag will have
	-- times when it is idle, it may be clever to stop the
	-- garbage collector and step it manually whenever
	-- the Jack sample queue is full. However, how to guarantee
	-- that we step it fast enough to prevent leaks?
	collectgarbage("collect")
	local old_pause = collectgarbage("setpause", 100)
	local old_stepmul = collectgarbage("setstepmul", 100)

	local channels = self.channels
	local state
	self:foreach(function(frame)
		-- Loop should get unrolled automatically
		for i = 1, channels do
			local sample = tonumber(frame[i])
			assert(sample ~= nil)

			-- NOTE: Invalid port Ids are currently silently
			-- ignored. Perhaps it's better to check state or
			-- to access output_ports_count from applause.c.
			state = C.applause_push_sample(first_port+i, sample)

			-- React to buffer underruns.
			-- This is done here instead of in the realtime thread
			-- even though it is already overloaded, so as not to
			-- affect other applications in the Jack graph.
			if state == C.APPLAUSE_AUDIO_XRUN then
				io.stderr:write("WARNING: Buffer underrun detected\n")
			end

			if state == C.APPLAUSE_AUDIO_INTERRUPTED then return true end
		end
	end)

	collectgarbage("setpause", old_pause)
	collectgarbage("setstepmul", old_stepmul)

	if state == C.APPLAUSE_AUDIO_INTERRUPTED then
		error("SIGINT received", 2)
	end
end

-- implemented in applause.c
function Stream:fork()
	error("C function not registered!")
end

-- NOTE: This implementation is for single-channel streams
-- only. See also MuxStream:foreach()
function Stream:foreach(fnc)
	local clear = table.clear

	local frame = table.new(1, 0)
	local tick = self:gtick()

	while true do
		frame[1] = tick()
		clear(sampleCache)
		if not frame[1] or fnc(frame) then break end
	end
end

function Stream:benchmark()
	if self:len() == math.huge then
		error("Cannot benchmark infinite stream")
	end

	benchmark(function()
		self:foreach(function() end)
	end)
end

-- Dump bytecode of tick function.
function Stream:jbc(out, all)
	-- Load the utility library on-demand.
	-- Its API is not stable according to luajit docs.
	require("jit.bc").dump(self:gtick(), out, all)
end

function Stream:jdump(opt, outfile)
	local dump = require("jit.dump")
	local tick = self:gtick()

	-- Make sure we discard any existing traces to
	-- arrive at more or less reproducible results
	jit.flush(tick, true)
	jit.on(tick, true)

	dump.on(opt, outfile)
	-- FIXME: A single tick() call will not get jit-compiled
	-- and there appears to be no way to force compilation of a function.
	-- Getting any output at all would require saving the stream and
	-- force some bulk calculations, so instead we always generate
	-- up to 1s of samples here.
	local _, err = pcall(function()
		for _ = 1, samplerate do tick() end
	end)
	dump.off()
	if err then error(err) end
end

-- TODO: Use a buffer to improve perfomance (e.g. 1024 samples)
function Stream:save(filename, format)
	if self:len() == math.huge then
		error("Cannot save infinite stream")
	end

	local channels = self.channels
	local hnd = sndfile:new(filename, "SFM_WRITE",
	                        samplerate, channels, format)

	local frame_buffer = sndfile.frame_type(channels)

	self:foreach(function(frame)
		-- NOTE: This should be (hopefully) automatically
		-- unrolled for single-channel streams
		-- Otherwise each loop copies an entire frame.
		-- This should be faster than letting LuaJIT translate
		-- the frame directly.
		for i = 1, channels do
			local sample = tonumber(frame[i])
			assert(sample ~= nil)
			frame_buffer[i-1] = sample
		end

		-- NOTE: Apparently we cannot use hnd:write() if a frame is larger than one sample
		-- (i.e. multichannel streams)
		-- FIXME: Check return value
		hnd:writef(frame_buffer)
	end)

	hnd:close()
end

function Stream:tonumber() return self:map(tonumber) end
function Stream:tostring() return self:map(tostring) end

function Stream:totable()
	if self:len() == math.huge then
		error("Cannot serialize infinite stream")
	end

	local channels = self.channels
	local channel_vectors = table.new(channels, 0)

	for i = 1, channels do
		channel_vectors[i] = table.new(self:len(), 0)
	end

	self:foreach(function(frame)
		-- Loop should be unrolled automatically
		for i = 1, channels do
			channel_vectors[i][#channel_vectors[i] + 1] = frame[i]
		end
	end)

	-- Return a list of vectors, one per channel
	return unpack(channel_vectors)
end

-- Effectively eager-evaluates the stream returning
-- an array-backed stream.
function Stream:eval()
	return MuxStream:new(self:totable())
end

-- NOTE: This will only plot the stream's first channel
function Stream:toplot(rows, cols)
	rows = rows or 25
	cols = cols or 80

	local scaled = self:resample(self:len() / cols)
	                   :add(1):mul((rows-1)/2):floor():add(1):totable()
	local plot = {}

	for i = 1, #scaled do
		plot[i] = {}
		for j = 1, rows do plot[i][j] = " " end

		-- middle line (represents 0)
		plot[i][math.ceil(rows/2)] = "-"

		plot[i][scaled[i]] = "+" -- data point

		-- connect with last data point
		if i > 1 then
			if scaled[i-1] < scaled[i] then
				for j = scaled[i-1]+1, scaled[i]-1 do
					plot[i][j] = "|"
				end
			elseif scaled[i-1] > scaled[i] then
				for j = scaled[i-1]-1, scaled[i]+1, -1 do
					plot[i][j] = "|"
				end
			end
		end
	end

	local str = ""
	for j = rows, 1, -1 do
		for i = 1, cols do str = str..plot[i][j] end
		str = str.."\n"
	end

	return str
end

function Stream:pipe(prog, vbufmode, vbufsize)
	local hnd = io.popen(prog, "w")
	hnd:setvbuf(vbufmode or "full", vbufsize)

	self:foreach(function(frame)
		hnd:write(unpack(frame))
		hnd:write("\n")
	end)

	hnd:close()
end

function Stream:gnuplot()
	if self:len() == math.huge then
		error("Cannot plot infinite stream")
	end

	-- NOTE: We're not using Stream:pipe() here, so we can
	-- efficiently calculate a time index.
	-- FIXME: Using something like libplplot would be more
	-- efficient
	local hnd = io.popen("feedgnuplot --exit --lines --ymin -1 --ymax 1 --domain", "w")
	hnd:setvbuf("full")

	local second = sec()
	local i = 1
	self:foreach(function(frame)
		hnd:write(i/second, " ", unpack(frame))
		hnd:write("\n")
		i = i + 1
	end)

	hnd:close()
end

function Stream:mux(...)
	return MuxStream:new(self, ...)
end

function Stream:dupmux(channels)
	return DupMux(self, channels)
end

-- For single-channel streams only, see also MuxStream:demux()
function Stream:demux(i, j)
	j = j or i
	assert(i == 1 and j == 1,
	       "Invalid channel range specified (mono-channel stream)")
	return self
end

-- Stream metamethods

-- NOTE: Currently non-functional since Lua 5.1 does not
-- consider metamethods when evaluating the length operator.
function Stream:__len()	return self:len() end

-- NOTE: Will only convert the first channel
function Stream:__tostring()
	local stream = self:tostring()

	if self:len() > 1024 then
		stream = stream:sub(1, 1024)..tostream{"..."}
	end

	local t = stream:totable()
	return "{"..table.concat(t, ", ").."}"
end

-- NOTE: These operators work with scalars and streams.
-- The semantics of e.g. adding Stream(x) is compatible
-- with a map that adds x. Maps are preferred since
-- they are (slightly).
-- NOTE: Named addOp() and similar functions below
-- are necessary instead of lambdas so consecutive
-- operations can be collapsed by ZipStream (which
-- tests for function equivalence)

do
	local function addOp(x1, x2) return x1+x2 end

	function Stream.add(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x+v2 end) or
		       ZipStream:new(addOp, v1, v2)
	end
	Stream.__add = Stream.add
end

do
	-- FIXME: Minus is not associative, so we use an unique
	-- function for every ZipStream to prevent them being
	-- collapsed. E.g. s1 - (s2 - s3) must not result in a single
	-- ZipStream. Perhaps there should be a special stream for
	-- associative operations, or collapsing should happen in Stream.minus().
	-- It is also possible that the collapsing is slower than having
	-- ZipStreams with only two operands since the resulting stream
	-- tick arrays cannot be inlined well.
	--
	--local function subOp(x1, x2) return x1-x2 end

	function Stream.minus(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x-v2 end) or
		       ZipStream:new(function(x1, x2) return x1-x2 end, v1, v2)
	end
	Stream.__sub = Stream.minus
end

do
	local function mulOp(x1, x2) return x1*x2 end

	function Stream.mul(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x*v2 end) or
		       ZipStream:new(mulOp, v1, v2)
	end
	Stream.gain = Stream.mul
	Stream["\u{00D7}"] = Stream.mul -- APL Multiply/Signum
	Stream.__mul = Stream.mul
end

do
	-- FIXME: See above minus()
	--local function divOp(x1, x2) return x1/x2 end

	function Stream.div(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x/v2 end) or
		       ZipStream:new(function(x1, x2) return x1/x2 end, v1, v2)
	end
	Stream["\u{00F7}"] = Stream.div -- APL Divide
	Stream.__div = Stream.div
end

do
	-- FIXME: See above minus()
	--local function modOp(x1, x2) return x1%x2 end

	function Stream.mod(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x%v2 end) or
		       ZipStream:new(function(x1, x2) return x1%x2 end, v1, v2)
	end
	Stream.__mod = Stream.mod
end

do
	-- FIXME: See above minus()
	--local function powOp(x1, x2) return x1^x2 end

	function Stream.pow(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x^v2 end) or
		       ZipStream:new(function(x1, x2) return x1^x2 end, v1, v2)
	end
	Stream["\u{22C6}"] = Stream.pow -- APL Exponentiation
	Stream.__pow = Stream.pow
end

function Stream:__unm() return self * -1 end

function Stream.__concat(op1, op2)
	return ConcatStream:new(op1, op2)
end

-- FIXME: Length comparisions can already be written
-- elegantly - perhaps these operators should have
-- more APLish semantics instead?
-- However Lua practically demands these metamethods
-- (as well as __eq) to return booleans.

function Stream.__lt(op1, op2)
	return op1:len() < op2:len()
end

function Stream.__le(op1, op2)
	return op1:len() <= op2:len()
end

MuxStream = DeriveClass(Stream)

function MuxStream:ctor(...)
	self.streams = {}
	for k, stream in ipairs{...} do
		stream = tostream(stream)
		if stream.channels == 1 then
			table.insert(self.streams, stream)
		else
			for _, v in ipairs(stream.streams) do
				table.insert(self.streams, v)
			end
		end
		if stream:len() ~= self.streams[1]:len() then
			error("Incompatible length of stream "..k)
		end
	end
	self.channels = #self.streams

	-- Single-channel streams must not be MuxStream!
	-- This means that MuxStream:new() can be used as a
	-- kind of multi-channel aware tostream() and is also
	-- the inverse of totable()
	if self.channels == 1 then return self.streams[1] end
end

function MuxStream:gtick()
	error("MuxStreams cannot be ticked")
end

function MuxStream:len()
	-- All channel streams have the same length
	return self.streams[1]:len()
end

-- Overrides Stream:demux()
function MuxStream:demux(i, j)
	j = j or i
	assert(1 <= i and i <= self.channels and
	       1 <= j and j <= self.channels and i <= j,
	       "Invalid channel range specified")

	-- NOTE: We cannot create single-channel MuxStreams
	return i == j and self.streams[i]
	              or MuxStream:new(unpack(self.streams, i, j))
end

-- Overrides Stream:foreach()
-- NOTE: This could easily be integrated into Stream:foreach(),
-- however this results in the loop to be unrolled explicitly
-- for single-channel streams.
function MuxStream:foreach(fnc)
	local clear = table.clear

	local ticks = {}
	for i = 1, #self.streams do
		ticks[i] = self.streams[i]:gtick()
	end

	local channels = self.channels
	local frame = table.new(channels, 0)

	while true do
		clear(sampleCache)

		for i = 1, channels do
			frame[i] = ticks[i]()
			-- Since all streams must have the same
			-- length, if one ends all end
			if not frame[i] then return end
		end

		if fnc(frame) then break end
	end
end

-- FIXME: This should perhaps be a class
function DupMux(stream, channels)
	channels = channels or 2

	local cached = tostream(stream):cache()
	-- FIXME: May need a list creation function
	local streams = {}
	for j = 1, channels do
		streams[j] = cached
	end

	return MuxStream:new(unpack(streams))
end

-- Base class for all streams that operate on arbitrary numbers
-- of other streams. Handles muxing opaquely.
MuxableStream = DeriveClass(Stream)

-- Describes the part of the muxableCtor's signature
-- containing muxable streams.
-- By default all arguments are muxable streams.
MuxableStream.sig_first_stream = 1
MuxableStream.sig_last_stream = -1

function MuxableStream:ctor(...)
	local args = {...}

	-- automatic base constructor call, ignore
	if #args == 0 then return end

	local first_stream = self.sig_first_stream
	local last_stream = self.sig_last_stream > 0 and
	                    self.sig_last_stream or #args
	local channels

	for i = first_stream, last_stream do
		-- Streamify all stream arguments
		args[i] = tostream(args[i])
		-- The first non-mono stream determines the number of
		-- channels to check for
		channels = channels or (args[i].channels > 1 and args[i].channels)
	end

	if not channels then
		-- all mono-streams
		return self:muxableCtor(unpack(args))
	end

	for i = first_stream, last_stream do
		-- Single-channel (non-MuxStream) streams are blown up
		-- to the final number of channels
		if args[i].channels == 1 then
			args[i] = args[i]:dupmux(channels)
		end

		-- Otherwise all stream arguments must have the same number of channels
		assert(args[i].channels == channels,
		       "Incompatible number of channels")
	end

	local channel_streams = {}
	local mono_args = {...}

	for channel = 1, args[first_stream].channels do
		for i = first_stream, last_stream do
			assert(args[i]:instanceof(MuxStream))
			mono_args[i] = args[i].streams[channel]
		end

		channel_streams[channel] = self.base:new(unpack(mono_args))
	end

	return MuxStream:new(unpack(channel_streams))
end

CachedStream = DeriveClass(MuxableStream)

function CachedStream:muxableCtor(stream)
	self.stream = stream
end

function CachedStream:gtick()
	local tick = self.stream:gtick()

	return function()
		local sample = sampleCache[self]
		if not sample then
			sample = tick()
			sampleCache[self] = sample
		end
		return sample
	end
end

function CachedStream:len()
	return self.stream:len()
end

VectorStream = DeriveClass(Stream)

-- NOTE: This is mono-streams only, the inverse of Stream:totable()
-- is MuxStream:new() which will also work for single streams
function VectorStream:ctor(vector)
	self.vector = vector
end

function VectorStream:gtick()
	local vector = self.vector
	local i = 0

	return function()
		i = i + 1
		return vector[i]
	end
end

function VectorStream:len()
	return #self.vector
end

SndfileStream = DeriveClass(Stream)

function SndfileStream:ctor(filename, samplerate, channels, format)
	-- FIXME: This fails if the file is not at the
	-- correct sample rate. Need to resample...
	-- NOTE: samplerate and channels are ignored unless SF_FORMAT_RAW
	-- files are read.
	local handle = sndfile:new(filename, "SFM_READ",
	                           samplerate, channels, format)
	self.filename = filename
	self.samplerate = handle.info.samplerate
	self.channel_no = handle.info.channels
	self.format = handle.info.format
	self.frames = tonumber(handle.info.frames)
	handle:close()

	if self.channel_no > 1 then
		local cached = self:cache()
		local streams = {}
		for i = 0, self.channel_no-1 do
			streams[i+1] = cached:map(function(frame)
				return tonumber(frame[i])
			end)
		end
		return MuxStream:new(unpack(streams))
	end
end

function SndfileStream:gtick()
	-- The file is reopened, so each tick has an independent
	-- read pointer which is important when reusing the stream.
	-- NOTE: We could do this with a single handle per object but
	-- by maintaining our own read position and seeking before reading.
	local handle = sndfile:new(self.filename, "SFM_READ",
	                           self.samplerate, self.channel_no, self.format)

	-- Make sure that we are still reading the same file;
	-- at least with the same meta-data.
	-- Theoretically, the file could have changed since object
	-- construction.
	assert(handle.info.channels == self.channel_no and
	       handle.info.frames == self.frames,
	       "Sndfile changed")

	if self.channel_no == 1 then
		local read = handle.read

		return function()
			return read(handle)
		end
	else
		-- For multi-channel audio files, we generate a stream
		-- of frame buffers.
		-- However, the user never sees these since they are translated
		-- to a MuxStream automatically (see ctor())
		local readf = handle.readf
		local frame = sndfile.frame_type(self.channel_no)

		return function()
			return readf(handle, frame) and frame or nil
		end
	end
end

function SndfileStream:len() return self.frames end

ConcatStream = DeriveClass(MuxableStream)

function ConcatStream:muxableCtor(...)
	self.streams = {}
	for _, v in ipairs{...} do
		if v:instanceof(ConcatStream) then
			-- Optimization: Avoid redundant
			-- ConcatStream objects
			for _, s in ipairs(v.streams) do
				table.insert(self.streams, s)
			end
		else
			table.insert(self.streams, v)
		end
	end

	-- all but the last stream must be finite
	-- (it makes no sense to append something to
	-- an infinite stream)
	for i = 1, #self.streams - 1 do
		if self.streams[i]:len() == math.huge then
			error("Stream "..i.." is infinite")
		end
	end
end

function ConcatStream:gtick()
	local i = 1
	local ticks = {}

	for k = 1, #self.streams do
		ticks[k] = self.streams[k]:gtick()
	end

	-- NOTE: Binding each tick function to a variable
	-- is faster since it allows the JIT compiler
	-- to inline functions.
	local tick = ticks[1]

	return function()
		while tick do
			local sample = tick()
			if sample then return sample end

			-- try next stream
			i = i + 1
			tick = ticks[i]
		end
	end
end

function ConcatStream:len()
	local len = 0

	-- if last stream is infinite, len will also be infinite
	for _, stream in pairs(self.streams) do
		len = len + stream:len()
	end

	return len
end

RepeatStream = DeriveClass(MuxableStream)

-- we have a trailing non-stream argument
RepeatStream.sig_last_stream = 1

function RepeatStream:muxableCtor(stream, repeats)
	self.stream = stream
	self.repeats = repeats or math.huge
end

function RepeatStream:gtick()
	local i = 1
	local stream = self.stream
	local tick = stream:gtick()
	local repeats = self.repeats

	return function()
		while i <= repeats do
			local sample = tick()
			if sample then return sample end

			-- next iteration
			i = i + 1
			-- FIXME: The tick() method itself may be too
			-- inefficient for realtime purposes.
			-- Also, we may slowly leak memory.
			tick = stream:gtick()
		end
	end
end

function RepeatStream:len()
	return self.stream:len() * self.repeats
end

-- Ravel operation inspired by APL.
-- This removes one level of nesting from nested streams
-- (e.g. streams of streams), and is semantically similar
-- to folding the stream with the Concat operation.
-- FIXME: This needs special support for MuxStreams
-- (ie. a stream of stereo-streams)
RavelStream = DeriveClass(MuxableStream)

function RavelStream:muxableCtor(stream)
	self.stream = stream
end

function RavelStream:gtick()
	local stream_tick = self.stream:gtick()
	local current_tick = nil

	return function()
		while true do
			if current_tick then
				local value = current_tick()
				if value then return value end
				current_tick = nil
			end

			local value = stream_tick()

			-- NOTE: We don't use instanceof() here for performance
			-- reasons
			if type(value) == "table" and value.is_a_stream then
				current_tick = value:gtick()
			else
				return value
			end
		end
	end
end

function RavelStream:len()
	if self.stream:len() == math.huge then
		-- FIXME: Actually, it is possible that the stream
		-- is infinite but consists only of empty streams.
		-- In this case, tick() will be stuck in an infinite loop...
		return math.huge
	end

	local len = 0
	local t = self.stream:totable()

	for i = 1, #t do
		len = len + (type(t[i]) == "table" and t[i].is_a_stream and
		             t[i]:len() or 1)
	end

	return len
end

IotaStream = DeriveClass(Stream)

function IotaStream:ctor(v1, v2)
	if not v2 then
		self.from = 1
		self.to = v1 or math.huge
	else
		self.from = v1
		self.to = v2
	end

	if self.from < 1 or self.to < 1 or
	   self.from > self.to then
		error("Invalid iota range ["..self.from..","..self.to.."]")
	end
end

function IotaStream:gtick()
	local i = self.from-1

	return function()
		if i >= self.to then return end
		i = i + 1
		return i
	end
end

function IotaStream:len()
	return self.to == math.huge and math.huge or
	       self.to - self.from + 1
end

-- i and j have the same semantics as in string.sub()
SubStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
SubStream.sig_last_stream = 1

function SubStream:muxableCtor(stream, i, j)
	self.stream = stream
	self.i = i
	self.j = j or -1

	local stream_len = self.stream:len()

	if self.i < 0 then self.i = self.i + stream_len + 1 end
	if self.j < 0 then self.j = self.j + stream_len + 1 end

	if self.i > stream_len or self.j > stream_len or
	   self.i > self.j then
		error("Invalid sub-stream range ["..self.i..","..self.j.."]")
	end
end

function SubStream:gtick()
	local tick = self.stream:gtick()

	-- OPTIMIZE: Perhaps ask stream to skip the first
	-- self.i-1 samples
	for _ = 1, self.i-1 do tick() end

	local i = self.i

	return function()
		if i > self.j then return end
		i = i + 1
		return tick()
	end
end

function SubStream:len()
	return self.j == math.huge and math.huge or
	       self.j - self.i + 1
end

-- FIXME: Will not work for non-samlpe streams
-- This should be split into a generic (index) and
-- sample-only (interpolate) operation
IndexStream = DeriveClass(MuxableStream)

function IndexStream:muxableCtor(stream, index_stream)
	self.stream = stream
	self.index_stream = index_stream
end

function IndexStream:gtick()
	local stream_tick = self.stream:gtick()
	local index_tick = self.index_stream:gtick()

	local stream_len = self.stream:len()

	-- avoid math table lookup at sample rate
	local huge = math.huge
	local floor = math.floor
	local ceil = math.ceil

	-- cache of samples generated by stream
	local cache = {}

	return function()
		local index_sample = index_tick()

		if not index_sample then return end

		if index_sample < 1 or index_sample > stream_len or
		   index_sample == huge then
			error("Index "..index_sample.." out of range")
		end

		local index_floor, index_ceil = floor(index_sample),
		                                ceil(index_sample)

		while #cache < index_ceil do
			table.insert(cache, stream_tick())
		end

		-- applies linear interpolation if index_sample is
		-- not an integer
		return cache[index_floor] +
		       (cache[index_ceil] - cache[index_floor])*
		       (index_sample - index_floor)
	end
end

function IndexStream:len()
	return self.index_stream:len()
end

MapStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
MapStream.sig_last_stream = 1

function MapStream:muxableCtor(stream, fnc)
	self.stream = stream
	self.fnc = fnc
end

function MapStream:gtick()
	local tick = self.stream:gtick()
	local fnc = self.fnc

	return function()
		local sample = tick()
		return sample and fnc(sample)
	end
end

function MapStream:len()
	return self.stream:len()
end

ScanStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
ScanStream.sig_last_stream = 1

function ScanStream:muxableCtor(stream, fnc)
	self.stream = stream
	self.fnc = fnc
end

function ScanStream:gtick()
	local tick = self.stream:gtick()
	local fnc = self.fnc
	local last_sample = nil

	return function()
		local sample = tick()
		if not sample then return end

		last_sample = fnc(last_sample, sample)
		return last_sample
	end
end

function ScanStream:len()
	return self.stream:len()
end

FoldStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
FoldStream.sig_last_stream = 1

function FoldStream:muxableCtor(stream, fnc)
	self.stream = stream
	self.fnc = fnc
end

function FoldStream:gtick()
	local tick = self.stream:gtick()
	local fnc = self.fnc

	return function()
		local l, r

		while true do
			r = tick()
			if not r then break end

			l = l and fnc(l, r) or r
		end

		return l
	end
end

function FoldStream:len()
	return self.stream:len() > 0 and 1 or 0
end

-- ZipStream combines any number of streams into a single
-- stream using a function. This is the basis of the "+"
-- and "*" operations.
--
-- NOTE (FIXME?): This "inlines" ZipStream arguments with
-- the same function as an optimization. This ONLY WORKS
-- for associative operations and more than 3 operands are
-- probably slower than two ZipStreams except for very large
-- numbers of ZipStreams (should be benchmarked).
-- In this case, we might just remove that optimization.
ZipStream = DeriveClass(MuxableStream)

-- We have a leading non-stream argument
ZipStream.sig_first_stream = 2

function ZipStream:muxableCtor(fnc, ...)
	self.fnc = fnc

	self.streams = {}
	for _, v in ipairs{...} do
		if v:instanceof(ZipStream) and v.fnc == fnc then
			-- Optimization: Avoid redundant
			-- ZipStream objects
			for _, s in ipairs(v.streams) do
				table.insert(self.streams, s)
			end
		else
			table.insert(self.streams, v)
		end
	end
end

function ZipStream:gtick()
	local fnc = self.fnc

	if #self.streams == 2 then
		-- 2 streams are common, so use an unrolled
		-- version here
		--
		-- NOTE: Unrolling the ticks array here
		-- almost halves the overhead when calculating
		-- something like Stream(0)+Stream(1), making
		-- it almost as fast as
		-- Stream(0):map(function(x) return x+1 end)
		local tick1 = self.streams[1]:gtick()
		local tick2 = self.streams[2]:gtick()

		return function()
			local sample1 = tick1()
			if not sample1 then return end

			local sample2 = tick2()
			if not sample2 then return sample1 end

			return fnc(sample1, sample2)
		end
	else
		-- NOTE: Unfortunately, functions in the
		-- ticks array cannot be inlined
		local ticks = {}
		for i = 1, #self.streams do
			ticks[i] = self.streams[i]:gtick()
		end

		return function()
			local result = ticks[1]()
			if not result then return end

			for i = 2, #ticks do
				local sample = ticks[i]()

				if sample then
					result = fnc(result, sample)
				end
			end

			return result
		end
	end
end

function ZipStream:len()
	return self.streams[1]:len()
end

NoiseStream = DeriveClass(Stream)

function NoiseStream:gtick()
	local random = math.random

	return function()
		return random()*2 - 1
	end
end

-- NOTE: Adapted from the algorithm used here:
-- http://vellocet.com/dsp/noise/VRand.html
function BrownNoise()
	return NoiseStream():scan(function(brown, white)
		brown = (brown or 0) + white
		return (brown < -8 or brown > 8) and brown - white or brown
	end) * 0.0625
end

PinkNoiseStream = DeriveClass(Stream)

-- NOTE: Adapted from the algorithm used here:
-- http://vellocet.com/dsp/noise/VRand.html
function PinkNoiseStream:gtick()
	local random = math.random
	local band, rshift = bit.band, bit.rshift
	local max = math.max

	local store = table.new(16, 0)
	for i = 1, 16 do store[i] = 0 end

	local pink = 0
	local count = 0

	return function()
		local k = 0

		-- Find the first bit set. This is still way faster
		-- than doing it using the libc's ffs() function.
		-- Someday the bit library will hopefully support ffs
		while band(rshift(count, k), 1) == 0 and k < 4 do
			k = k + 1
		end
		k = band(k, 0x0F) + 1

		local last_r = store[k]

		while true do
			local r = random()*2 - 1

			store[k] = r

			local next_pink = pink + r - last_r

			if next_pink >= -4 and next_pink <= 4 then
				pink = next_pink
				break
			end
		end

		-- Make sure count wraps. This is for some reason much slower
		-- than using a FFI integer.
		count = band(count + 1, 0x0F)

		return (random()*2 - 1 + pink)*0.125
	end
end

--
-- Delay Lines
-- NOTE: Echoing could be implemented here as well since
-- delay lines are only an application of echoing with a wetness of 1.0.
-- However this complicates matters because we have to handle nil.
--

DelayStream = DeriveClass(MuxableStream)

DelayStream.sig_last_stream = 1

function DelayStream:muxableCtor(stream, length)
	self.stream = stream
	self.length = length
	if length < 1 then error("Invalid delay line length") end
end

function DelayStream:gtick()
	local tick = self.stream:gtick()
	local length = self.length
	local buffer = table.new(length, 0)
	local buffer_pos = 1

	for i = 1, length do buffer[i] = 0 end

	return function()
		local sample = buffer[buffer_pos]
		buffer[buffer_pos] = tick()
		buffer_pos = (buffer_pos % length) + 1
		return sample
	end
end

function DelayStream:len()
	return self.length + self.stream:len()
end

--
-- Delay line with a variable (stream) length and
-- maximum length. Non-interpolating.
-- This is not part of DelayStream, mainly because
-- we cannot accurately calculate the length of the resulting
-- stream.
-- FIXME: Probably currently broken.
--
DelayXStream = DeriveClass(MuxableStream)

DelayXStream.sig_last_stream = 2

function DelayXStream:muxableCtor(stream, length, max_length)
	self.stream = stream
	self.length = length
	self.max_length = max_length or sec(1)
end

function DelayXStream:gtick()
	local ceil = math.ceil
	local tick = self.stream:gtick()
	local length_tick = self.length:gtick()
	local max_length = self.max_length
	local buffer = table.new(max_length, 0)
	local write_pos = 1
	local read_pos = 1

	for i = 1, max_length do buffer[i] = 0 end

	return function()
		local sample = buffer[ceil(read_pos)]
		buffer[write_pos] = tick()
		write_pos = (write_pos % max_length) + 1
		read_pos = (read_pos + max_length/length_tick()) % max_length
		return sample
	end
end

function DelayXStream:len()
	return self.max_length + self.stream:len()
end

--
-- MIDI Support
--

MIDIStream = DeriveClass(Stream)

function MIDIStream:gtick()
	return function()
		-- This is always cached since there is only one MIDI event queue
		-- and it must not be pulled more than once per tick.
		local sample = sampleCache[self]
		if not sample then
			sample = C.applause_pull_midi_sample()
			sampleCache[self] = sample
		end
		return sample
	end
end

-- Last value of a specific control channel
function Stream:CC(control, channel)
	channel = channel or 0

	assert(0 <= control and control <= 127,
	       "MIDI control number out of range (0 <= x <= 127)")
	assert(0 <= channel and channel <= 15,
	       "MIDI channel out of range (0 <= x <= 15)")

	local filter = bit.bor(0xB0, channel, bit.lshift(control, 8))
	local value = 0
	local band, rshift = bit.band, bit.rshift

	return self:map(function(sample)
		value = band(sample, 0xFFFF) == filter and
		        tonumber(rshift(sample, 16)) or value
		return value
	end)
end

-- Velocity of NOTE ON for a specific note on a channel
function Stream:mvelocity(note, channel)
	-- `note` may be a note name like "A4"
	note = type(note) == "string" and ntom(note) or note
	channel = channel or 0

	assert(0 <= note and note <= 127,
	       "MIDI note out of range (0 <= x <= 127)")
	assert(0 <= channel and channel <= 15,
	       "MIDI channel out of range (0 <= x <= 15)")

	local on_filter  = bit.bor(0x90, channel, bit.lshift(note, 8))
	local off_filter = bit.bor(0x80, channel, bit.lshift(note, 8))
	local value = 0
	local band, rshift = bit.band, bit.rshift

	return self:map(function(sample)
		value = band(sample, 0xFFFF) == on_filter and
		        rshift(sample, 16) or
		        band(sample, 0xFFFF) == off_filter and
		        0 or value
		return value
	end)
end

--
-- MIDI primitives
--

do
	local band = bit.band
	local floor, log = math.floor, math.log

	local note_names = {
		"C", "C#", "D", "D#", "E", "F",
		"F#", "G", "G#", "A", "A#", "B"
	}

	-- MIDI note number to name
	-- NOTE: mton() can handle the words as generated by MIDINoteStream
	function mton(note)
		note = band(note, 0xFF)
		local octave = floor(note / 12)-1
		return note_names[(note % 12)+1]..octave
	end

	function Stream:mton() return self:map(mton) end

	local ntom_offsets = {}
	for i, name in ipairs(note_names) do
		ntom_offsets[name] = i-1
		-- Saving the offsets for the lower-cased note names
		-- avoids a string.upper() call in ntom()
		ntom_offsets[name:lower()] = i-1
	end

	-- Note name to MIDI note number
	function ntom(name)
		local octave = name:byte(-1) - 48 + 1
		return octave*12 + ntom_offsets[name:sub(1, -2)]
	end

	function Stream:ntom() return self:map(ntom) end

	-- There are only 128 possible MIDI notes,
	-- so their frequencies can and should be cached.
	-- We do this once instead of on-demand, so the lookup
	-- table consists of consecutive numbers.
	local mtof_cache = table.new(128, 0)
	for note = 0, 127 do
		-- MIDI NOTE 69 corresponds to 440 Hz
		mtof_cache[note] = 440*math.pow(2, (note - 69)/12)
	end

	-- Convert from MIDI note to frequency
	-- NOTE: mtof() can handle the words as generated by MIDINoteStream
	function mtof(note)
		return mtof_cache[band(note, 0xFF)]
	end

	function Stream:mtof() return self:map(mtof) end

	-- Convert from frequency to closest MIDI note
	function ftom(freq)
		-- NOTE: math.log/2 is a LuaJIT extension
		return floor(12*log(freq/440, 2) + 0.5)+69
	end

	function Stream:ftom() return self:map(ftom) end
end

-- Convert from MIDI name to frequency
function ntof(name) return mtof(ntom(name)) end
function Stream:ntof() return self:map(ntof) end

-- Convert from frequency to closest MIDI note name
function fton(freq) return mton(ftom(freq)) end
function Stream:fton() return self:map(fton) end

-- Tick an instrument only when an inputstream (note_stream),
-- gets ~= 0. When it changes back to 0 again, an "off"-stream
-- is triggered. This allows the construction of instruments with
-- Attack-Sustain and Decay phases based on real-time control signals.
-- The note values can be passed into the constructor by using functions
-- for the "on" and "off" streams.
-- Usually, the note stream will be a MIDIStream:mvelocity(), so the two
-- instrument streams can be based on the MIDI velocity (but don't have
-- to be if the velocity is not important).
InstrumentStream = DeriveClass(MuxableStream)

InstrumentStream.sig_last_stream = 1

function InstrumentStream:muxableCtor(note_stream, on_stream, off_stream)
	note_stream = tostream(note_stream)
	local note_stream_cached

	if type(on_stream) == "function" then
		note_stream_cached = note_stream:cache()
		self.on_stream = on_stream(note_stream_cached)
	else
		self.on_stream = tostream(on_stream)
	end
	if type(off_stream) == "function" then
		note_stream_cached = note_stream_cached or note_stream:cache()
		self.off_stream = off_stream(note_stream_cached)
	else
		-- The "off" stream is optional
		self.off_stream = off_stream and tostream(off_stream)
	end

	-- `note_stream` is cached only when required
	self.note_stream = note_stream_cached or note_stream
end

function InstrumentStream:gtick()
	local note_tick = self.note_stream:gtick()
	local on_stream = self.on_stream
	local on_stream_inf = on_stream:len() == math.huge
	local off_stream = self.off_stream
	local on_tick
	local function off_tick() return 0 end

	return function()
		local note = note_tick()
		if not note then return end

		if on_tick == nil then -- no note
			if note == 0 then return off_tick() or 0 end

			-- FIXME: This is not strictly real-time safe
			on_tick = on_stream:gtick()
			return on_tick() or 0
		else -- note on
			if note ~= 0 then
				local sample = on_tick()
				if sample then return sample end

				-- on_stream must be finite, retrigger
				on_tick = on_stream:gtick()
				return on_tick() or 0
			elseif not on_stream_inf then
				-- don't cut off finite on_streams
				local sample = on_tick()
				if sample then return sample end
			end

			-- FIXME: This is not strictly real-time safe
			on_tick = nil
			if off_stream then off_tick = off_stream:gtick() end
			return off_tick() or 0
		end
	end
end

function InstrumentStream:len()
	return self.note_stream:len()
end

function Stream:instrument(on_stream, off_stream)
	return InstrumentStream:new(self, on_stream, off_stream)
end

--
-- Primitives
--

function tostream(v)
	if type(v) == "table" then
		if v.is_a_stream then return v end
		-- assume to be vector
		return VectorStream:new(v)
	else
		return Stream:new(v)
	end
end

function iota(...) return IotaStream:new(...) end
_G["\u{2373}"] = iota -- APL Iota

function line(v1, t, v2)
	return iota(t) * ((v2-v1)/t) + v1
end

-- Derived from RTcmix' "curve" table
-- Generates a single linear or logarithmic line segment
-- See http://www.music.columbia.edu/cmc/Rtcmix/docs/scorefile/maketable.html#curve
function curve(v1, alpha, t, v2)
	v2 = v2 or 0
	if not alpha or alpha == 0 then return line(v1, t, v2) end

	local exp = math.exp
	local denom = 1/(1 - exp(alpha))
	local delta = v2 - v1

	return iota(t):map(function(x)
		return v1 + delta*(1 - exp(x/t * alpha))*denom
	end)
end

-- Generates a variable number of concatenated line segments
-- E.g. curves(0, 0, sec(1), 1, 0, sec(1), 0)
function curves(...)
	local args = {...}
	local ret
	for i = 1, #args-1, 3 do
		local c = curve(unpack(args, i, i+3))
		ret = ret and ret..c or c
	end
	return ret
end

--
-- Filters
--

--[==[
--
-- Non-working FIR filters (FIXME)
--

-- Normalized Sinc function
local function Sinc(x)
	return x == 0 and 1 or
	       math.sin(2*math.pi*x)/(2*math.pi*x)
end

local function Hamming(n, window)
	local alpha = 0.54
	return alpha - (1-alpha)*math.cos((2*math.pi*n)/(window-1))
end
local function Blackman(n, window)
	local alpha = 0.16
	return (1-alpha)/2 -
	       0.5*math.cos((2*math.pi*n)/(window-1)) +
	       alpha*0.5*math.cos((4*math.pi*n)/(window-1))
end

FIRStream = DeriveClass(Stream)

function FIRStream:ctor(stream, freq_stream)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq_stream)
end

function FIRStream:gtick()
	local window = {}

	-- window size (max. 1024 samples)
	-- this is the max. latency introduced by the filter
	-- since the window must be filled before we can generate
	-- (filtered) samples
	local window_size = math.min(1024, self.stream:len())
	local window_p = window_size-1
	local accu = 0

	local blackman = {}
	for i = 1, window_size do blackman[i] = Blackman(i-1, window_size) end

	local tick = self.stream:gtick()
	local freq_tick = self.freq_stream:gtick()

	return function()
		-- fill buffer (initial)
		while #window < window_size-1 do
			table.insert(window, tick())
		end

		window[window_p+1] = tick()
		window_p = (window_p + 1) % window_size

		local period = freq_tick()/samplerate

		local sample = 0
		local i = window_p
		repeat
			-- FIXME
			sample = sample + window[(i % window_size)+1] *
			         Sinc((i-window_p - window_size/2)/period) *
			         blackman[i-window_p+1]
			i = i + 1
		until (i % window_size) == window_p

		return sample
	end
end

function FIRStream:len()
	return self.stream:len()
end
]==]

--
-- General-purpose IIR filters:
-- These are direct translations of ChucK's LPF, HPF, BPF and BRF
-- ugens which are in turn adapted from SuperCollider 3.
--

-- De-denormalize function adapted from ChucK.
-- Not quite sure why this is needed - properly to make the
-- IIR filters numerically more stable.
local function ddn(f)
	return f >= 0 and (f > 1e-15 and f < 1e15 and f or 0) or
	                  (f < -1e-15 and f > -1e15 and f or 0)
end

LPFStream = DeriveClass(MuxableStream)

function LPFStream:muxableCtor(stream, freq)
	self.stream = stream
	self.freq_stream = freq
end

function LPFStream:gtick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan

	local tick = self.stream:gtick()
	local freq_tick = self.freq_stream:gtick()
	local cur_freq = nil

	return function()
		local sample = tick()
		local freq = freq_tick()

		if sample == nil or freq == nil then
			-- don't filter if we run out of frequency samples
			return sample
		elseif freq ~= cur_freq then
			-- calculate filter coefficients
			-- avoid recalculation for constant frequencies
			cur_freq = freq

			local pfreq = cur_freq * radians_per_sample * 0.5

			local C = 1/tan(pfreq)
			local C2 = C*C
			local sqrt2C = C * sqrt2

			a0 = 1/(1 + sqrt2C + C2)
			b1 = -2.0 * (1.0 - C2) * a0
			b2 = -(1.0 - sqrt2C + C2) * a0
		end

		local y0 = sample + b1*y1 + b2*y2
		local result = a0 * (y0 + 2*y1 + y2)

		y2 = ddn(y1)
		y1 = ddn(y0)

		return result
	end
end

function LPFStream:len()
	return self.stream:len()
end

HPFStream = DeriveClass(MuxableStream)

function HPFStream:muxableCtor(stream, freq)
	self.stream = stream
	self.freq_stream = freq
end

function HPFStream:gtick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan

	local tick = self.stream:gtick()
	local freq_tick = self.freq_stream:gtick()
	local cur_freq = nil

	-- NOTE: Very similar to LPFStream.gtick()
	-- Can we factor out the similarity without sacrificing
	-- too much performance?
	return function()
		local sample = tick()
		local freq = freq_tick()

		if sample == nil or freq == nil then
			-- don't filter if we run out of frequency samples
			return sample
		elseif freq ~= cur_freq then
			-- calculate filter coefficients
			-- avoid recalculation for constant frequencies
			cur_freq = freq

			local pfreq = cur_freq * radians_per_sample * 0.5

			local C = tan(pfreq)
			local C2 = C*C
			local sqrt2C = C * sqrt2

			a0 = 1/(1 + sqrt2C + C2)
			b1 = 2.0 * (1.0 - C2) * a0
			b2 = -(1.0 - sqrt2C + C2) * a0
		end

		local y0 = sample + b1*y1 + b2*y2
		local result = a0 * (y0 - 2*y1 + y2)

		y2 = ddn(y1)
		y1 = ddn(y0)

		return result
	end
end

function HPFStream:len()
	return self.stream:len()
end

-- NOTE: The quality factor, indirectly proportional
-- to the passband width
BPFStream = DeriveClass(MuxableStream)

function BPFStream:muxableCtor(stream, freq, quality)
	self.stream = stream
	self.freq_stream = freq
	self.quality_stream = quality
end

function BPFStream:gtick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan
	local cos = math.cos

	local tick = self.stream:gtick()
	local freq_tick = self.freq_stream:gtick()
	local quality_tick = self.quality_stream:gtick()
	local cur_freq, cur_quality

	return function()
		local sample = tick()
		local freq = freq_tick()
		local quality = quality_tick()

		if sample == nil or freq == nil or quality == nil then
			-- don't filter if we run out of frequency samples
			return sample
		elseif freq ~= cur_freq or quality ~= cur_quality then
			-- calculate filter coefficients
			-- avoid recalculation for constant frequencies
			-- and quality factors
			cur_freq = freq
			cur_quality = quality

			local pfreq = cur_freq * radians_per_sample
			local pbw = 1 / cur_quality*pfreq*0.5

			local C = 1/tan(pbw)
			local D = 2*cos(pfreq);

			a0 = 1/(1 + C)
			b1 = C*D*a0
			b2 = (1 - C)*a0
		end

		local y0 = sample + b1*y1 + b2*y2
		local result = a0 * (y0 - y2)

		y2 = ddn(y1)
		y1 = ddn(y0)

		return result
	end
end

function BPFStream:len()
	return self.stream:len()
end

-- NOTE: The quality factor, indirectly proportional
-- to the passband width
BRFStream = DeriveClass(MuxableStream)

function BRFStream:muxableCtor(stream, freq, quality)
	self.stream = stream
	self.freq_stream = freq
	self.quality_stream = quality
end

function BRFStream:gtick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan
	local cos = math.cos

	local tick = self.stream:gtick()
	local freq_tick = self.freq_stream:gtick()
	local quality_tick = self.quality_stream:gtick()
	local cur_freq, cur_quality

	-- NOTE: Very similar to BPFStream.gtick()
	return function()
		local sample = tick()
		local freq = freq_tick()
		local quality = quality_tick()

		if sample == nil or freq == nil or quality == nil then
			-- don't filter if we run out of frequency samples
			return sample
		elseif freq ~= cur_freq then
			-- calculate filter coefficients
			-- avoid recalculation for constant frequencies
			-- and quality factors
			cur_freq = freq
			cur_quality = quality

			local pfreq = cur_freq * radians_per_sample
			local pbw = 1 / cur_quality*pfreq*0.5

			local C = tan(pbw)
			local D = 2*cos(pfreq);

			a0 = 1/(1 + C)
			b1 = -D*a0
			b2 = (1 - C)*a0
		end

		local y0 = sample - b1*y1 - b2*y2
		local result = a0 * (y0 + y2) + b1*y1

		y2 = ddn(y1)
		y1 = ddn(y0)

		return result
	end
end

function BRFStream:len()
	return self.stream:len()
end

--
-- Jack client abstractions. This passes low level signals
-- and works only with clients created via Stream.fork()
--

cdef_safe[[
int kill(int pid, int sig);
]]

Client = DeriveClass()

function Client:ctor(pid)
	self.pid = pid
end

function Client:play()
	C.kill(self.pid, 10); -- SIGUSR1
end

function Client:stop()
	C.kill(self.pid, 12); -- SIGUSR2
end

function Client:kill()
	C.kill(self.pid, 15); -- SIGTERM
end

Client.__gc = Client.kill

--
-- Additional modules are loaded with dofile(),
-- so they react to reload()
--
dofile "dssi.lua"
dofile "evdev.lua"
