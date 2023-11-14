---
--- This "module" lists all symbols available on a REPL prompt.
--- It does not have to and cannot be externally included using `require`.
--- @module applause
--- @author Robin Haberkorn
---
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

cdef_include "applause.h"

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

// Useful in various situations
void free(void *ptr);
]]

--- Measure time required to execute fnc()
-- @func fnc Function to benchmark
-- @see Stream:benchmark
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

--- Sample rate in Hz.
-- This variable is overwritten by the C core.
samplerate = 44100

--- Convert seconds to sample numbers
-- These are functions, so we can round the result.
-- automatically.
-- @number[opt=1] x Number of seconds
-- @treturn int Number of samples
function sec(x) return math.floor(samplerate*(x or 1)) end
--- Convert milliseconds to sample numbers.
-- @number[opt=1] x Number of milliseconds
-- @treturn int Number of samples
function msec(x) return sec((x or 1)/1000) end

--- The sample cache.
-- This maps @{Stream} objects to arbitrary values (usually numbers) and
-- can be used by Stream implementations to avoid recalculations during a single tick.
-- It is cleared after every tick.
-- You usually **don't** have to access this table manually, but *should* use
-- @{Stream:cache} instead.
-- This is only seldom useful when implementing new Stream classes.
--
-- We don't know how large it must be, but once it is
-- allocated we only table.clear() it.
sampleCache = {}

--- Reload the main module.
-- Useful for hacking it without having to restart the application.
-- @local
function reload()
	dofile "applause.lua"
	collectgarbage()
end

--- Derive class
-- @tparam[opt] Class base
--   Base class.
--   This should usually be @{Stream} or @{MuxableStream} when deriving custom Stream classes.
-- @return Derived class table
--
-- @fixme Inconsistent naming. Use all-lower case for functions
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

--- Stream base class.
-- This can be instantiated to generate plain values (although you should usually
-- use @{tostream} instead).
-- This class is also important to derive from in order to implement custom Streams.
-- Most importantly, it defines all common Stream operations.
-- @type Stream
Stream = DeriveClass()

--- Create a stream for generating values.
-- The stream will produce the same value in every tick.
-- @within Class Stream
-- @fixme The @within tag is necessary to fix later additions to the Stream class
-- after the end of the section.
-- @function Stream:new
-- @param[opt=0] value Value to generate
-- @treturn Stream
-- @see tostream
-- @usage Stream:new(23)
-- @usage Stream(23)
function Stream:ctor(value)
	-- @fixme Why does this convert everything to a number?
	self.value = tonumber(value) or 0
end

--- Stream constructor.
-- This is an **abstract** method that must be implemented when deriving custom classes.
-- It is never invoked directly - use the corresponding @{Stream:new} method instead.
-- @function ctor
-- @param ... Arbitrary parameters, passed on from the @{Stream:new} method.
-- @treturn ?Stream
--   You can optionally return a @{Stream} instance to competely replace the object table.
--   If given, this value will be returned by the @{Stream:new} method instead.

-- There is Stream:instanceof(), but testing Stream.is_a_stream
-- is sometimes faster (for generator functions) and can be done
-- without knowing that the table at hand is an object
Stream.is_a_stream = true

-- All streams except the special MuxStream are mono
Stream.channels = 1

-- A stream, produces an infinite number of the same value by default
-- (eternal quietness by default)
-- @fixme This should probably be renamed and return a function in a function,
-- so you can do non-realtime safe stuff in Stream:gtick and real-time
-- safe initialization in the returned function.
-- This would allow "resetting" dependant streams from real-time safe tick
-- functions.
function Stream:gtick()
	local value = self.value

	return function()
		return value
	end
end

-- Register all unary functions from the math package
-- as stream operations/methods (creates a stream that calls
-- the function on every sample)
-- FIXME: There is actually no need to do this using a loop anymore
-- since we document every method individually anyway.

--- Get absolute value of all samples
-- @function abs
-- @treturn Stream
-- @see math.abs

--- Get arc cosine of all samples (in radians)
-- @function acos
-- @treturn Stream
-- @see math.acos

--- Get the arc sine of all samples (in radians)
-- @function asin
-- @treturn Stream
-- @see math.asin

--- Get the arc tangent of all samples (in radians)
-- @function atan
-- @treturn Stream
-- @see math.atan

--- Get the smallest integer larger than or equal to all samples
-- @function ceil
-- @treturn Stream
-- @see math.ceil

--- Get the cosine of all samples (assumed to be in radians)
-- @function cos
-- @treturn Stream
-- @see math.cos

--- Get the hyperbolic cosine of all samples
-- @function cosh
-- @treturn Stream
-- @see math.cosh

--- Get the angle of all samples (given in radians) in degrees
-- @function deg
-- @treturn Stream
-- @see math.deg

--- Get the value e^^x for all samples
-- @function exp
-- @treturn Stream
-- @see math.exp

--- Get the largest integer smaller than or equal to all samples
-- @function floor
-- @treturn Stream
-- @see math.floor

--- Get the natural logarithm of all samples
-- @function log
-- @treturn Stream
-- @see math.log

--- Get the base-10 logarithm of all samples
-- @function log10
-- @treturn Stream
-- @see math.log10

--- Get the angle of all samples (given in degrees) in radians
-- @function rad
-- @treturn Stream
-- @see math.rad

--- Get the sine of all samples (assumed to be in radians).
-- @function sin
-- @treturn Stream
-- @see math.sin

--- Get the hyperbolic sine of all samples.
-- @function sinh
-- @treturn Stream
-- @see math.sinh

--- Get the square root of all samples.
-- (You can also use the expression x^^0.5 to compute this value.)
-- @function sqrt
-- @treturn Stream
-- @see math.sqrt

--- Get the tangent of all samples (assumed to be in radians)
-- @function tan
-- @treturn Stream
-- @see math.tan

--- Get the hyperbolic tangent of all samples
-- @function tanh
-- @treturn Stream
-- @see math.tanh
for _, fnc in pairs{"abs", "acos", "asin", "atan",
                    "ceil", "cos", "cosh", "deg",
                    "exp", "floor", "log", "log10",
                    "rad", "sin", "sinh", "sqrt",
                    "tan", "tanh"} do
	Stream[fnc] = function(self)
		return self:map(math[fnc])
	end
end

--
-- Some binary functions from the math package
--

--- Returns the minimum value between two streams
-- @function min
-- @StreamableNumber v Serves as the right argument to @{math.min}.
-- @treturn Stream
-- @see ZipStream

--- Returns the maximum value between two streams
-- @function max
-- @StreamableNumber v Serves as the right argument to @{math.max}.
-- @treturn Stream
-- @see ZipStream
for _, name in pairs{"min", "max"} do
	local fnc = math[name]

	Stream[name] = function(self, v)
		return type(v) == "number" and
		       self:map(function(x) return fnc(x, v) end) or
		       self:zip(fnc, v)
	end
end

--
-- Register all binary operators of the "bit" module
--

--- Get the bitwise **not** of all samples
-- @treturn Stream
-- @see bit.bnot
function Stream:bnot()
	return self:map(bit.bnot)
end

--- Perform bitwise **or** between two streams
-- @function bor
-- @StreamableNumber v Serves as the right argument to @{bit.bor}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **and** between two streams
-- @function band
-- @StreamableNumber v Serves as the right argument to @{bit.band}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **xor** between two streams
-- @function xor
-- @StreamableNumber v Serves as the right argument to @{bit.xor}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **logical left-shift** between two streams
-- @function lshift
-- @StreamableNumber v Serves as the right argument to @{bit.lshift}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **logical right-shift** between two streams
-- @function rshift
-- @StreamableNumber v Serves as the right argument to @{bit.rshift}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **arithmetic right-shift** between two streams
-- @function arshift
-- @StreamableNumber v Serves as the right argument to @{bit.arshift}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **left rotation** between two streams
-- @function rol
-- @StreamableNumber v Serves as the right argument to @{bit.rol}.
-- @treturn Stream
-- @see ZipStream

--- Perform bitwise **right rotation** between two streams
-- @function ror
-- @StreamableNumber v Serves as the right argument to @{bit.ror}.
-- @treturn Stream
-- @see ZipStream
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

--- Clip all samples between two values (between [-1,+1] by default).
-- @StreamableNumber[opt=-1] min Serves as the lower bound
-- @StreamableNumber[opt=+1] max Serves as the upper bound
-- @treturn Stream
function Stream:clip(min, max)
	min = min or -1
	max = max or 1

	return self:max(min):min(max)
end

--- Scale stream with values between [-1,+1] to [lower,upper]
-- @StreamableNumber[opt=0] v1 Delivers the lower value.
-- @StreamableNumber v2 Delivers the upper value
-- @treturn Stream
-- @fixme The API is not documentable easily.
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

--- Mix two streams
-- @Stream other Other stream to mix in
-- @StreamableNumber[opt=0.5] wetness
--   Wetness factor between [0,1].
--   This determines the loudness of the other stream.
-- @treturn Stream
function Stream:mix(other, wetness)
	wetness = wetness or 0.5
	return self*(1 - wetness) + other*wetness
end

--- Distribute mono-stream between two stereo channels
-- @StreamableNumber[opt=0] location
--   Provides the location (between [0,1]) of the source stream in the resulting stereo stream.
-- @treturn MuxStream
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

--- Resample stream.
-- This is a linear resampler thanks to the semantics of IndexStream.
-- @number factor
--   The resampling factor.
--   If lower than 1, the stream will be slowed.
--   If higher than 1, it will be sped up.
-- @treturn Stream
-- @todo It would be useful if factor would be StreamableNumber.
-- However the time in line() would consequently also have to be StreamableNumber.
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

--- Create a ramp between [0,1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function Phasor
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.Phasor(440)
function Stream.Phasor(freq, phase)
	phase = phase or 0

	return ScanStream:new(freq, function(accu, f)
		return ((accu or phase) + f/samplerate) % 1
	end)
end

--- Saw tooth wave between [-1,+1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function SawOsc
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.SawOsc(440)
function Stream.SawOsc(freq, phase)
	phase = (phase or 0)*2+1

	return ScanStream:new(freq, function(accu, f)
		return ((accu or phase) + 2*f/samplerate) % 2
	end) - 1
end

--- Sinusiod wave between [-1,+1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function SinOsc
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.SinOsc(440)
function Stream.SinOsc(freq, phase)
	return Stream.Phasor(freq, phase):mul(2*math.pi):sin()
end
Stream["\u{25CB}"] = Stream.SinOsc -- APL Circle

--- Pulse between [0,1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function PulseOsc
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.PulseOsc(440)
function Stream.PulseOsc(freq, phase)
	return Stream.Phasor(freq, phase):map(function(x)
		return x < 0.5 and 1 or 0
	end)
end

--- Square wave between [-1,+1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function SqrOsc
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.SqrOsc(440)
function Stream.SqrOsc(freq, phase)
	return Stream.Phasor(freq, phase):map(function(x)
		return x < 0.5 and 1 or -1
	end)
end

--- Triangle wave between [-1,+1] with the frequency taken from the source stream.
-- It can also be invoked as a regular function to pass constant frequencies.
-- @function TriOsc
-- @number[opt=0] phase The phase between [0,1].
-- @treturn Stream
-- @usage Stream.TriOsc(440)
function Stream.TriOsc(freq, phase)
	local abs = math.abs

	return Stream.SawOsc(freq, phase):map(function(x)
		return abs(x)*2 - 1
	end)
end

--- Bit crusher effect.
-- This reduces the bit depth of the source stream.
-- @number[opt=8] bits The resulting streams bit depth.
-- @treturn Stream
function Stream:crush(bits)
	bits = bits or 8
	local floor = math.floor

	return self:map(function(x)
		return floor(x * 2^bits + 0.5) / 2^bits
	end)
end

--- Get a stream's length in samples.
-- @treturn int Stream length in samples (@{math.huge} for infinite streams).
-- @fixme __len metamethod is also defined but it currently cannot
-- work since Lua 5.1 does not consider a table's metamethod when
-- evaluating the length (#) operator.
-- This however would work when building LuaJIT with -DLUAJIT_ENABLE_LUA52COMPAT
function Stream:len()
	return math.huge -- infinity
end

--- Send samples to the Jack output ports, ie. play the stream.
-- This will block and can be interrupted by pressing Ctrl+C (SIGINT).
-- @int[opt=1] first_port
--   First Jack output port to use.
--   The first stream in a multi-channel stream will go to this port,
--   the next one to first_port+1 and so on.
function Stream:play(first_port)
	first_port = (first_port or 1) - 1

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

--- Execute function for each frame generated by the stream.
-- This will process samples as fast as possible and may therefore
-- not be well suited to process real-time input.
-- @func fnc
--   Function to execute.
--   It gets passed an array of samples, one for each channel.
-- @fixme This is not currently interruptable and therefore not suitable
-- to be executed dynamically at the command-line.
function Stream:foreach(fnc)
	local clear = table.clear

	-- NOTE: This implementation is for single-channel streams
	-- only. See also MuxStream:foreach().
	local frame = table.new(1, 0)
	local tick = self:gtick()

	while true do
		frame[1] = tick()
		clear(sampleCache)
		if not frame[1] or fnc(frame) then break end
	end
end

--- Benchmark stream (time to generate all samples).
-- This does not work for infinite streams.
-- Naturally, this calculates samples as fast as possible
-- and it does not make sense to benchmark streams with real-time input.
-- @see benchmark
function Stream:benchmark()
	if self:len() == math.huge then
		error("Cannot benchmark infinite stream")
	end

	benchmark(function()
		self:foreach(function() end)
	end)
end

--- Dump bytecode for stream (its tick function).
-- See also the undocumented `jit.bc` module in LuaJIT.
-- This does not return a string, so it can contain color output.
-- @tparam[opt=io.stdout] file out File stream to print to (can also be a table of functions).
-- @bool[opt=true] all Whether to dump all subfunctions as well.
function Stream:jbc(out, all)
	-- Load the utility library on-demand.
	-- Its API is not stable according to luajit docs.
	require("jit.bc").dump(self:gtick(), out, all)
end

--- Dump bytecode, traces and machine code of stream (its tick function).
-- See also the undocumented `jit.dump` module in LuaJIT.
-- This does not return a string, so it can contain color output.
-- @string[opt="tbim"] opt Output flags
-- @tparam[opt=io.stdout] file outfile File stream to print to (can also be a table of functions).
function Stream:jdump(opt, outfile)
	local dump = require("jit.dump")
	local tick = self:gtick()

	jit.off(true, true)
	jit.on(tick, true)
	-- Make sure we discard any existing traces to
	-- arrive at more or less reproducible results
	jit.flush(tick, true)

	dump.on(opt, outfile)
	-- NOTE: A single tick() call may not get JIT-compiled
	-- and there appears to be no way to force compilation of a function.
	-- Getting any output at all would require saving the stream and
	-- force some bulk calculations, so instead we always generate
	-- up to 1s of samples here.
	-- See also the "hotloop" optimization parameter.
	local _, err = pcall(function()
		for _ = 1, samplerate do tick() end
	end)
	dump.off()
	if err then error(err) end
end

--- Convert all values to Lua numbers
-- @treturn Stream
-- @see tonumber
function Stream:tonumber() return self:map(tonumber) end
--- Convert all values to Lua strings
-- @treturn Stream
-- @see tostring
function Stream:tostring() return self:map(tostring) end

--- Calculate all values of a stream and return them as Lua arrays/tables.
-- This naturally does not work for infinite streams and the eagerly evaluated
-- stream must not depend on real-time input (MIDI controllers etc).
-- @return For multi-channel streams, every channel will be returned in its own return value.
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

--- Evaluate stream eagerly.
-- This precalculates all values for non-infinite streams, which may be useful
-- to lower CPU load during real-time playback.
-- @treturn Stream
-- @see Stream:totable
function Stream:eval()
	return MuxStream:new(self:totable())
end

--- Plot stream with numbers between [-1,+1] to ASCII graphics.
-- @int[opt=25] rows Rows of the ASCII diagram.
-- @int[opt=80] cols Columns of the ASCII diagram.
-- @treturn string ASCII diagram formatted as a string.
-- @usage =Stream.SinOsc(440):sub(1, samplerate/440):toplot()
-- @warning This will only plot the stream's first channel
function Stream:toplot(rows, cols)
	-- @fixme Perhaps default to $ROWS and $COLUMNS?
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

--- Pipe stream values to external program.
-- This sends one frame per tick on their own line.
-- Every line can contain multiple numbers separated by tabs depending on the number of channels.
-- @string prog The program to launch and receive the stream data.
-- @string[opt="full"] vbufmode Buffering mode.
-- @int[opt] vbufsize The buffer size.
-- @see file:setvbuf
-- @fixme This is currently allowed for infinite streams as well,
-- but there is no way to interrupt Stream:foreach().
function Stream:pipe(prog, vbufmode, vbufsize)
	local hnd = io.popen(prog, "w")
	hnd:setvbuf(vbufmode or "full", vbufsize)

	self:foreach(function(frame)
		hnd:write(unpack(frame))
		hnd:write("\n")
	end)

	hnd:close()
end

--- Plot stream using [gnuplot](http://www.gnuplot.info/).
-- This is not allowed for infinite streams.
-- @warning This requires the feedgnuplot script.
-- @fixme gnuplot is not the ideal tool for plotting audio data.
function Stream:gnuplot()
	if self:len() == math.huge then
		error("Cannot plot infinite stream")
	end

	local cmd = "feedgnuplot --exit --lines --ymin -1 --ymax 1 --domain"

	local svg_file
	if _G._send_display_data then
		-- Some extremely crude support for plotting directly into Jupyter ILua cells.
		-- NOTE: With io.popen() we cannot read and write to the pipe at the
		-- same time, so we must dump the file to disk.
		svg_file = os.tmpname()
		cmd = cmd.." --terminal svg >"..svg_file
	end

	-- NOTE: We're not using Stream:pipe() here, so we can
	-- efficiently calculate a time index.
	-- FIXME: Using something like libplplot would be more
	-- efficient
	local hnd = io.popen(cmd, "w")
	hnd:setvbuf("full")

	local second = sec()
	local i = 1
	self:foreach(function(frame)
		hnd:write(i/second, " ", unpack(frame))
		hnd:write("\n")
		i = i + 1
	end)

	hnd:close()

	if _G._send_display_data then
		_G._send_display_data{
			['image/svg+xml'] = io.open(svg_file):read('*a')
		}
		os.remove(svg_file)
	end
end

--- Print values of stream when they change.
-- This is useful to debug slowly or seldom changing streams like those
-- produced by @{MIDIStream} or @{EvdevStream}.
-- @string[opt='%s'] format
--   A format string to format the samples.
--   This can be used to prefix text or tweak the number formatting.
-- @treturn Stream
-- @see string.format
function Stream:print(format)
	format = format or "%s"

	return self:scan(function(last, sample)
		if sample ~= last then
			print(string.format(format, sample))
		end
		return sample
	end)
end

--- Check whether stream is an instance of a particular class.
-- @function instanceof
-- @tparam Class other_class The other object or class to check against.
-- @treturn bool Whether the stream is an instance of other_class.
-- @see DeriveClass

--
-- Stream metamethods
--

--- Create new stream using the `()` operator (metamethod).
-- This is equivalent to calling the @{Stream:new} method.
-- @metamethod __call
-- @local
-- @treturn Stream
-- @usage Stream(23)
-- @see Stream:new
-- @see DeriveClass

--- Extract and interpolate samples using the `[]` operator (metamethod).
-- @metamethod __index
-- @StreamableNumber index
--   The stream that will generate indices into the base stream.
--   If these numbers have fractions, the output sample will be linearilly interpolated.
--   The maximum number generated by this stream determines the memory requirements
--   of the index operation, so this *should* never produce very large numbers or
--   unboundedly growing numbers.
-- @treturn Stream
--   The resulting stream will have a the same length as the index-stream (`index:len()`).
-- @usage SndfileStream("test.wav")[Stream.SinOsc(0.5):scale(1, sec(5))]
-- @usage iota(2, 10)[{2, 5, 8}]
-- @fixme This will only work for number streams at the moment.
-- @fixme The memory requirements could be lifted by having a way to arbitrarily
-- seek the streams. This however would complicate the design significantly.

--- Get length of stream via `#` operator (metamethod).
-- @metamethod __len
-- @local
-- @treturn int
-- @see Stream:len
-- @usage #tostream{1, 2, 3}
-- @fixme Currently non-functional since Lua 5.1 does not
-- consider metamethods when evaluating the length operator
-- unless you compile with -DLUAJIT_ENABLE_LUA52COMPAT.
function Stream:__len()	return self:len() end

--- Format stream as string (metamethod).
-- This will only format the first channel and not more than 1024 samples.
-- @metamethod __tostring
-- @treturn string
-- @usage tostring(tostream{1, 2, 3})
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
-- they are (slightly) faster.
-- NOTE: Named addOp() and similar functions below
-- are necessary instead of lambdas so consecutive
-- operations can be collapsed by ZipStream (which
-- tests for function equivalence)

do
	local function addOp(x1, x2) return x1+x2 end

	--- Add samples of two streams
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the add operation.
	-- @StreamableNumber v2 Provides right values of the add operation.
	-- @treturn Stream
	-- @usage tostream(23):add(5)
	-- @usage tostream(23) + 5
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

	--- Subtract samples of two streams
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the subtraction operation.
	-- @StreamableNumber v2 Provides right values of the subtraction operation.
	-- @treturn Stream
	-- @usage tostream(23):minus(5)
	-- @usage tostream(23) - 5
	function Stream.minus(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x-v2 end) or
		       ZipStream:new(function(x1, x2) return x1-x2 end, v1, v2)
	end
	Stream.__sub = Stream.minus
end

do
	local function mulOp(x1, x2) return x1*x2 end

	--- Multiply samples of two streams
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the multiplication operation.
	-- @StreamableNumber v2 Provides right values of the multiplication operation.
	-- @treturn Stream
	-- @usage tostream(23):mul(5)
	-- @usage tostream(23) * 5
	function Stream.mul(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x*v2 end) or
		       ZipStream:new(mulOp, v1, v2)
	end
	--- Change volume (gain) of stream.
	-- This is an alias of @{Stream.mul}.
	-- @function gain
	-- @StreamableNumber volume The volume of the resulting stream.
	-- @treturn Stream
	Stream.gain = Stream.mul
	Stream["\u{00D7}"] = Stream.mul -- APL Multiply/Signum
	Stream.__mul = Stream.mul
end

do
	-- FIXME: See above minus()
	--local function divOp(x1, x2) return x1/x2 end

	--- Divide samples of two streams
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the division operation.
	-- @StreamableNumber v2 Provides right values of the division operation.
	-- @treturn Stream
	-- @usage tostream(23):div(5)
	-- @usage tostream(23) / 5
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

	--- Calculate modulus of two streams
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the modulo operation.
	-- @StreamableNumber v2 Provides right values of the modulo operation.
	-- @treturn Stream
	-- @usage tostream(23):mod(5)
	-- @usage tostream(23) % 5
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

	--- Take one stream to the power of another stream.
	-- This can be called as a Stream method or as an operator.
	-- @StreamableNumber v1 Provides left values of the power operation.
	-- @StreamableNumber v2 Provides right values of the power operation.
	-- @treturn Stream
	-- @usage tostream(23):pow(5)
	-- @usage tostream(23)^5
	function Stream.pow(v1, v2)
		return type(v2) == "number" and
		       MapStream:new(v1, function(x) return x^v2 end) or
		       ZipStream:new(function(x1, x2) return x1^x2 end, v1, v2)
	end
	Stream["\u{22C6}"] = Stream.pow -- APL Exponentiation
	Stream.__pow = Stream.pow
end

--- Negate all samples of stream via `-` operator (metamethod)
-- @metamethod __unm
-- @treturn Stream
-- @usage -tostream(23)
function Stream:__unm() return self * -1 end

-- FIXME: Length comparisions can already be written
-- elegantly - perhaps these operators should have
-- more APLish semantics instead?
-- However Lua practically demands these metamethods
-- (as well as __eq) to return booleans.

--- Check whether one stream is short than second via `<` operator (metamethod)
-- @metamethod __lt
-- @Stream op1 Left stream
-- @Stream op2 Right stream
-- @treturn bool
-- @usage tostream{1, 2, 3} < tostream(23)
function Stream.__lt(op1, op2) return op1:len() < op2:len() end

--- Check whether one stream is short than or equal to second via `<=` operator (metamethod)
-- @metamethod __le
-- @Stream op1 Left stream
-- @Stream op2 Right stream
-- @treturn bool
-- @usage tostream{1, 2, 3} <= tostream(23)
function Stream.__le(op1, op2) return op1:len() <= op2:len() end

--- Concatenate two streams via `..` operator (metamethod)
-- @metamethod __concat
-- @StreamableNumber op1 First stream
-- @StreamableNumber op2 Second stream
-- @treturn Stream
-- @see ConcatStream:new
-- @usage tostream{1, 2, 3}..23
function Stream.__concat(op1, op2) return ConcatStream:new(op1, op2) end

--- Class for muxing multiple streams into a multi-channel stream.
-- @type MuxStream
MuxStream = DeriveClass(Stream)

--- Create new MuxStream, ie combine multiple streams given as parameters into a single multi-channel stream.
-- The individual parameters can also be scalars and tables converted via @{tostream}.
-- The individual streams can both be a plain single-channel @{Stream}s or MuxStreams with an arbitrary number of channels.
-- The resulting stream's number of channels is the sum of all constituent streams' channels.
-- @function new
-- @param ... Streams to mux
-- @treturn MuxStream
-- @see Stream:mux
-- @see tostream
-- @see Stream:totable
-- @usage MuxStream(Stream.SinOsc(440), Stream.SinOsc(880)):play()
-- @fixme This could actually be hidden (@local) from end users.
-- The only advantage over Stream:mux is that it is more elegant when creating from
-- constants.
-- We could however use Stream.mux(...) as well and it would work exactly like MuxStream:new().
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

--- Extract one or more channels
-- @within Class Stream
-- @int i Id of first channel to extract.
-- @int[opt=i] j
--   Id of last channel to extract.
--   If omitted, only one channel (i) is extracted.
-- @treturn Stream|MuxStream
function Stream:demux(i, j)
	j = j or i
	-- For single-channel streams only, see also MuxStream:demux()
	assert(i == 1 and j == 1,
	       "Invalid channel range specified (mono-channel stream)")
	return self
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

--- Mux several streams with the source stream into a multi-channel stream.
-- @within Class Stream
-- @param ... Streams to mux
-- @treturn MuxStream
-- @see MuxStream:new
-- @usage Stream.SinOsc(440):mux(Stream.SinOsc(880)):play()
function Stream:mux(...)
	return MuxStream:new(self, ...)
end

--- Duplicate channel in single-channel stream (or suitable scalar value).
-- This can be called as a function or method.
-- @within Class Stream
-- @StreamableNumber stream Source of single channel data.
-- @int[opt=2] channels Number of channels of resulting stream.
-- @treturn MuxStream
-- @see MuxStream:new
function Stream.dupmux(stream, channels)
	channels = channels or 2

	local cached = tostream(stream):cache()
	-- FIXME: May need a list creation function
	local streams = {}
	for j = 1, channels do
		streams[j] = cached
	end

	return MuxStream:new(unpack(streams))
end

--- Base class for all streams that operate on multi-channel streams.
-- It handles muxing opaquely, by applying the tick() function on every
-- channel individually.
-- This allows writing multi-channel-capable Streams without complicating
-- things with having to handle frames etc.
-- This class is **abstract**, you are only supposed to derive from it,
-- not to instantiate it directly.
-- @type MuxableStream
-- @see DeriveClass
MuxableStream = DeriveClass(Stream)

--- The first muxable stream in @{MuxableStream:muxableCtor}'s signature.
-- If negative, this refers to an argument at the end of the parameter list.
-- This is 1 (first argument) by default.
MuxableStream.sig_first_stream = 1
--- The last muxable stream in @{MuxableStream:muxableCtor}'s signature.
-- If negative, this refers to an argument at the end of the parameter list.
-- This is -1 (last argument) by default.
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

--- Constructor for classes derived form @{MuxableStream}.
-- This is an **abstract** method, you **must** implement it in your subclass.
-- @function muxableCtor
-- @param ...
--   The arguments passed on from the @{Stream:new} method.
--   All arguments between @{MuxableStream.sig_first_stream} and
--   @{MuxableStream.sig_last_stream} are automatically converted
--   to Streams and demuxed (see @{Stream:demux}), so you will only
--   get passed plain single-channel streams.
-- @treturn ?Stream
--   You can optionally return a @{Stream} instance to competely replace the object table.
--   If given, this value will be returned by the @{Stream:new} method instead.
-- @see Stream:ctor

--- Class for caching streams.
-- Cached streams are calculated only once per tick.
-- @type CachedStream
-- @local
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

--- Cache this stream value to avoid recalculation within
-- the same tick (ie. point in time).
-- This may happen when a stream is used multiple times in the same "patch".
-- @within Class Stream
-- @treturn Stream
-- @todo This could be done automatically by an optimizer stage.
-- @fixme This is counter-productive for simple number streams
-- (anything simpler than a table lookup).
function Stream:cache()
	return CachedStream:new(self)
end

--- Class for streams based on vectors (arrays).
-- @type VectorStream
-- @local
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

--- Class for creating streams consisting of multiple concatenated base streams.
-- @type ConcatStream
-- @fixme The only advantage of using ConcatStream:new instead of the `..` operator
-- is that you do not need braces to apply other Stream methods.
-- This could be circumvented by introducing a Stream:append() method.
-- In this case, ConcatStream could be @local.
ConcatStream = DeriveClass(MuxableStream)

--- Create new ConcatStream, ie concatenation of serveral streams.
-- @function new
-- @param ...
--   Individual @{Stream}s (or numbers or tables, that can be converted to streams).
--   All but the last one must not be infinite.
-- @treturn ConcatStream
-- @see Stream.__concat
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
			-- @fixme The Stream:gtick() method itself may be too
			-- inefficient for realtime purposes.
			-- Also, we may slowly leak memory.
			-- It would be possible to precalculate the tick functions,
			-- but that wouldn't work for infinite repeats.
			-- This could be circumvented by supporting an optional reset function
			-- returned by Stream:gtick.
			-- This however would complicate all higher-order streams.
			-- Another solution would be a two-stage Stream:gtick which allows real-time
			-- safe gticking.
			tick = stream:gtick()
		end
	end
end

function RepeatStream:len()
	return self.stream:len() * self.repeats
end

--- Repeat stream. After the stream runs out of samples, it will start all over again.
-- @within Class Stream
-- @int[opt] repeats Number of repeats. If missing, the stream will be repeated indefinitely.
-- @treturn Stream
-- @fixme See restrictions of RepeatStream:gtick and Stream:gtick.
function Stream:rep(repeats)
	return RepeatStream:new(self, repeats)
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
				-- @fixme This is not real-time safe!
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

--- Ravel stream.
-- This takes a stream of streams and concatenates them.
-- @within Class Stream
-- @treturn Stream
-- @usage tostream{Stream.SinOsc(440):sub(1, sec()), Stream.SinOsc(880):sub(1, sec())}:ravel():play()
function Stream:ravel()
	return RavelStream:new(self)
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

---
--- @section end
---

--- Generate sequences of integers between [v1,v2].
-- @function iota
-- @int[opt=1] v1
--   If this is the only argument, the lower bound is 1 and v1 specifies the upper bound.
--   So without arguments, an infinite sequence of integers beginning with 1 will be generated.
-- @int[opt=math.huge] v2 If a second argument is specified, v1 is the lower bound and v2 the upper bound.
-- @treturn Stream
-- @usage iota(23)
-- @usage iota(1, 23)
iota = IotaStream
_G["\u{2373}"] = iota -- APL Iota

SubStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
SubStream.sig_last_stream = 1

-- i and j have the same semantics as in string.sub()
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

	-- @fixme There is room for optimization.
	-- Perhaps ask stream to skip the first self.i-1 samples.
	-- There should be a Stream:gtick_skip(self.i-1) method
	-- which defaults to ticking the stream, but could be overwritten
	-- in certain Stream subclasses.
	-- Since this functionality is really only required for SubStream,
	-- we could alternatively add offset-parameters to special
	-- stream constructors (like SndfileStream:new()) and overwrite SndfileStream:sub().
	-- A future optimizer stage could move down :sub() as close to the
	-- "source" as possible.
	for _ = 1, self.i-1 do
		tick()
		-- self.stream might be cached (somewhere down the line).
		-- Therefore it is essential to clear the cache in order to progress
		-- the stream.
		-- This however might clear more than necessary,
		-- resulting in superfluous recalculations.
		table.clear(sampleCache)
	end

	local i, j = self.i, self.j

	return function()
		if i > j then return end
		i = i + 1
		return tick()
	end
end

function SubStream:len()
	return self.j == math.huge and math.huge or
	       self.j - self.i + 1
end

--- Get substream (restrict stream in length).
-- This both allows discarding samples at the beginning and restricting the length
-- (even of infinite streams).
-- The semantics of the parameters are similar to @{string.sub}.
-- @within Class Stream
-- @int i
--   Start sample (1 is the first sample).
--   It may be negative to specify positions from the end, whereas -1 refers to the last sample.
-- @int[opt=-1] j
--   The last sample to include in the resulting stream.
--   It can also be negative to refer to samples at the end of the stream.
-- @treturn Stream
-- @see sec
-- @see msec
-- @usage Stream.SinOsc(440):sub(1, sec()):play()
function Stream:sub(i, j)
	return SubStream:new(self, i, j)
end

-- @fixme Will not work for non-sample streams
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

--- Map function to every sample of stream
-- @within Class Stream
-- @func fnc Function to apply to every sample.
-- @treturn Stream
-- @usage Stream.Phasor(440):mul(2*math.pi):map(math.sin)
function Stream:map(fnc)
	return MapStream:new(self, fnc)
end
Stream["\u{00A8}"] = Stream.map -- APL Double-Dot

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

--- Scan stream with function.
-- Every function call will receive the last and current sample,
-- so it is possible to "accumulate" values.
-- @within Class Stream
-- @func fnc
--   The function that gets called with the last and current sample to determine the output sample.
-- @treturn Stream
-- @usage =iota(10):scan(function(last, sample) return last+sample end)
function Stream:scan(fnc)
	return ScanStream:new(self, fnc)
end

FoldStream = DeriveClass(MuxableStream)

-- We have trailing non-stream arguments
FoldStream.sig_last_stream = 1

function FoldStream:muxableCtor(stream, fnc)
	if stream:len() == math.huge then
		error("An infinite stream cannot be folded!")
	end

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

--- Fold stream by calling function between all samples.
-- This will pass the current sample as the *right* argument and the last returned
-- value as the *left* argument.
-- In other words, it is similar to `fnc(fnc(fnc(sample[1], sample[2]), sample[3]), ...)`.
-- This naturally cannot work on infinite streams.
-- @within Class Stream
-- @func fnc Function that gets called with the *left* and *right* arguments.
-- @treturn Stream The returned stream has either length 0 or 1.
-- @usage =iota(10):fold(function(last, sample) return last+sample end)
function Stream:fold(fnc)
	return FoldStream:new(self, fnc)
end

---
-- ZipStream combines any number of streams into a single
-- stream using a function.
-- This is the basis of the `+` and `*` stream operations.
-- @type ZipStream
--
-- @fixme This "inlines" ZipStream arguments with
-- the same function as an optimization. This ONLY WORKS
-- for associative operations and more than 3 operands are
-- probably slower than two ZipStreams except for very large
-- numbers of ZipStreams (should be benchmarked).
-- In this case, we might just remove that optimization.
ZipStream = DeriveClass(MuxableStream)

-- We have a leading non-stream argument
ZipStream.sig_first_stream = 2

--- Create a ZipStream.
-- This is a stream that combines two or more substituent streams by
-- applying a function between each of their samples.
-- @function new
-- @func fnc Function to apply between samples.
-- @param ... Streams to combine.
-- @treturn ZipStream
-- @see Stream:zip
-- @usage ZipStream(function(l, r) return l+r end), Stream.SinOsc(440):gain(0.5), Stream.SinOsc(880):gain(0.5))
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
		local ticks = table.new(#self.streams, 0)
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

--- Zip stream with one or more other streams.
-- @within Class Stream
-- @func fnc Function to apply between samples.
-- @param ... Streams to zip with the calling stream.
-- @see ZipStream:new
function Stream:zip(fnc, ...)
	return ZipStream:new(fnc, self, ...)
end

--- A stream generating random values (noise) between [-1,1].
-- Since it does not have parameters, you don't necessarily have to instantiate it.
-- The class table itself is a valid stream object.
-- @type NoiseStream
-- @usage NoiseStream:play()
NoiseStream = DeriveClass(Stream)

--- Create new NoiseStream
-- @function new
-- @treturn NoiseStream

function NoiseStream:gtick()
	local random = math.random

	return function()
		return random()*2 - 1
	end
end

---
--- @section end
---

--- Create a brown noise stream.
-- @treturn Stream
-- @see NoiseStream
-- @fixme This is inconsistent. Perhaps we should define a BrownNoiseStream itself.
function BrownNoise()
	-- NOTE: Adapted from the algorithm used here:
	-- http://vellocet.com/dsp/noise/VRand.html
	return NoiseStream():scan(function(brown, white)
		brown = (brown or 0) + white
		return (brown < -8 or brown > 8) and brown - white or brown
	end) * 0.0625
end

--- Stream generating pink noise.
-- Since it does not have parameters, you don't necessarily have to instantiate it.
-- The class table itself is a valid stream object.
-- @type PinkNoiseStream
-- @see NoiseStream
-- @usage PinkNoiseStream:play()
PinkNoiseStream = DeriveClass(Stream)

--- Create new NoiseStream
-- @function new
-- @treturn PinkNoiseStream

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

--- Delay stream by buffering (delay line).
-- This is different to prepending a `Stream(0):sub(1, length)` in that it can be used to
-- produce feedback lines and delay real-time input.
-- @within Class Stream
-- @int length Length of delay line in samples.
-- @treturn Stream
function Stream:delay(length)
	return DelayStream:new(self, length)
end

--- Add echo to stream.
-- @within Class Stream
-- @int length How much to delay the echo.
-- @StreamableNumber[opt=0.5] wetness
--   Wetness/loadness of the echo (between [0,1]).
-- @treturn Stream
-- @see Stream:delay
-- @usage SndfileStream("voice.wav"):echo(msec(200)):play()
function Stream:echo(length, wetness)
	local cached = self:cache()
	return cached:mix(cached:delay(length), wetness)
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

--- Delay line with variable length.
-- @within Class Stream
-- @StreamableNumber length Stream generating the echo length.
-- @int[opt=sec()] max_length Maximum length of the delay line.
-- @see Stream:delay
-- @fixme This is probably broken.
-- @fixme It may be possible to merge this into Stream:delay.
function Stream:delayx(length, max_length)
	return DelayXStream:new(self, length, max_length)
end

--- Echo with variable delay.
-- @within Class Stream
-- @StreamableNumber length Stream generating the echo delay.
-- @StreamableNumber[opt=0.5] wetness
--   Wetness/loadness of the echo (between [0,1]).
-- @int[opt=sec()] max_length Maximum length of the delay line.
-- @see Stream:delay
-- @fixme This is probably broken.
-- @fixme It may be possible to merge this into Stream:delay.
function Stream:echox(length, wetness, max_length)
	local cached = self:cache()
	return cached:mix(cached:delayx(length, max_length), wetness)
end

---
--- @section end
---

--
-- Primitives
--

--- Convert value to @{Stream}.
-- Streams are returned unchanged.
-- A table/array will be converted to a stream, that produces all of its elements consecutively.
-- All other Lua values are generated infinitely.
-- @param v Value to convert.
-- @treturn Stream
-- @usage tostream(440):SinOsc():play()
-- @usage tostream{"A4", "B4", "C4"}:mtof()
function tostream(v)
	if type(v) == "table" then
		if v.is_a_stream then return v end
		-- assume to be vector
		return VectorStream:new(v)
	else
		return Stream:new(v)
	end
end

--- Generate a linear line segment.
-- It will start with v1 and linearilly slide to v2 in the given time.
-- @StreamableNumber v1 Start value.
-- @int t Duration of the value change in samples.
-- @StreamableNumber v2 End value.
-- @treturn Stream The resulting stream will have length t.
-- @usage Stream.SinOsc(440):gain(line(0, sec(5), 1)):play()
function line(v1, t, v2)
	return iota(t) * ((v2-v1)/t) + v1
end

--- Generates a single linear or logarithmic line segment.
-- It will start at v1 and logarithmically slide to v2 in the given time.
-- @number v1 Start value.
-- @number alpha
--   Line curvature.
--   A value of nil or 0, creates a straight line.
--   If smaller than 0, makes a logarithmic (convex) curve; larger negative values make "sharper" curves.
--   If larger than 0, makes a logarithmic (concave) curve; larger values make "sharper" curves.
-- @int t Duration of the value change in samples.
-- @number[opt=0] v2 End value.
-- @treturn Stream The resulting stream will have length t.
-- @see line
-- @usage Stream.SinOsc(440):gain(curve(0, -1, sec(5), 1)):play()
-- @fixme Could v1 and v2 be StreamableNumbers?
function curve(v1, alpha, t, v2)
	-- Derived from RTcmix' "curve" table
	-- See http://www.music.columbia.edu/cmc/Rtcmix/docs/scorefile/maketable.html#curve
	v2 = v2 or 0
	if not alpha or alpha == 0 then return line(v1, t, v2) end

	local exp = math.exp
	local denom = 1/(1 - exp(alpha))
	local delta = v2 - v1

	return iota(t):map(function(x)
		return v1 + delta*(1 - exp(x/t * alpha))*denom
	end)
end

--- Generates a variable number of concatenated line segments.
-- @number v1 Start value.
-- @number alpha Line curvature.
-- @int t Duration of the value change in samples.
-- @number v2 End value.
-- @param ... There can be more line segments starting with v2.
-- @treturn Stream
-- @see curve
-- @usage Stream.SinOsc(440):gain(curves(0, 0, sec(1), 1, 0, sec(1), 0)):play()
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
-- Jack client abstractions. This passes low level signals
-- and works only with clients created via Stream.fork()
--
-- @fixme This is currently broken.
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

-- implemented in applause.c
function Stream:fork()
	error("C function not registered!")
end

--
-- Additional modules are loaded with dofile(),
-- so they react to reload()
--
dofile "sndfile-stream.lua"
dofile "filters.lua"
dofile "dssi.lua"
dofile "midi.lua"
dofile "evdev.lua"
