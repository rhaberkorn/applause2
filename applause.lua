local sndfile = require "sndfile"
local ffi = require "ffi"
local bit = require "bit"

-- Make table.new() available (a LuaJIT extension)
require "table.new"

--
-- Define C functions for benchmarking (POSIX libc)
--
ffi.cdef[[
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

-- measure time required to execute fnc()
function benchmark(fnc)
	local t1 = ffi.new("struct timespec[1]")
	local t2 = ffi.new("struct timespec[1]")

	ffi.C.clock_gettime("CLOCK_PROCESS_CPUTIME_ID", t1)
	fnc()
	ffi.C.clock_gettime("CLOCK_PROCESS_CPUTIME_ID", t2)

	local t1_ms = t1[0].tv_sec*1000 + t1[0].tv_nsec/1000000
	local t2_ms = t2[0].tv_sec*1000 + t2[0].tv_nsec/1000000

	print("Elapsed CPU time: "..(t2_ms - t1_ms).."ms")
end

-- Sample rate
-- This is overwritten by the C core
samplerate = 44100

-- Time units: Convert between time and sample numbers
-- These are functions, so we can round the result
-- automatically
function sec(x) return math.floor(samplerate*(x or 1)) end
function msec(x) return sec((x or 1)/1000) end

function DeriveClass(base, ctor)
	local class = {}

	if base then
		class = base:new()

		-- we cannot derive metamethod tables, so we
		-- copy all relevant metamethods
		for _, m in pairs{"len", "call", "tostring",
		                  "add", "sub", "mul", "div",
		                  "mod", "pow", "unm",
		                  "concat", "lt", "le", "gc"} do
			class["__"..m] = base["__"..m]
		end
	end

	-- objects constructed from new class get the
	-- class table as their metatable, so __index is set up
	-- to look into the class table
	function class:__index(key)
		if type(key) == "string" then return getmetatable(self)[key] end

		-- non-string keys create IndexTables
		return IndexStream:new(self, key)
	end

	function class:new(...)
		local obj = base and base:new() or {}

		setmetatable(obj, self)

		if ctor then ctor(obj, ...) end

		return obj
	end

	return class
end

-- Stream base class
Stream = DeriveClass(nil, function(self, value)
	self.value = tonumber(value) or 0
end)

-- used by tostream():
Stream.is_a_stream = true

-- A stream, produces an infinite number of the same value by default
-- (eternal quietness by default)
function Stream:tick()
	return function()
		return self.value
	end
end

function Stream:map(fnc)
	return MapStream:new(self, fnc)
end

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

function Stream:bnot()
	return self:map(bit.bnot)
end

-- Register all binary operators of the "bit" module
for _, name in pairs{"bor", "band", "bxor",
                     "lshift", "rshift", "arshift",
                     "rol", "ror"} do
	local fnc = bit[name]

	Stream[name] = function(self, v)
		return self:map(function(x) return fnc(x, v) end)
	end
end

-- Scalar operations
-- In contrast to stream operations (based on ZipStream),
-- these work only with scalars and do not
-- extend the stream length
function Stream:add(n)
	return self:map(function(x) return x+n end)
end

function Stream:sub(n)
	return self:map(function(x) return x-n end)
end

function Stream:mul(n)
	return self:map(function(x) return x*n end)
end
Stream.gain = Stream.mul

function Stream:div(n)
	return self:map(function(x) return x/n end)
end

function Stream:mod(n)
	return self:map(function(x) return x%n end)
end

function Stream:pow(n)
	return self:map(function(x) return x^n end)
end

function Stream:clip(min, max)
	min = min or -1
	max = max or 1

	local math_min = math.min
	local math_max = math.max

	return self:map(function(x)
		return math_min(math_max(x, min), max)
	end)
end

-- Scale [-1,+1] signal to [lower,upper]
-- lower is optional and defaults to 0
function Stream:scale(v1, v2)
	local lower = v2 and v1 or 0
	local upper = v2 or v1

	return self:map(function(x)
		return (x + 1)*(upper - lower)/2 + lower
	end)
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

-- This is a linear resampler thanks to the
-- semantics of IndexStream
function Stream:resample(factor)
	return self[iota(math.floor(self:len() * factor)):div(factor)]
end

--
-- Wave forms with names derived from ChucK:
-- Can be written freq:SawOsc() or Stream.SawOsc(freq)
-- depending on the use case. The latter form may
-- be useful for constant frequencies.
--

-- Ramp from 0 to 1
function Stream.Phasor(freq)
	return ScanStream:new(freq, function(accu, f)
		return ((accu or 0) + f/samplerate) % 1
	end)
end

-- Saw tooth wave from -1 to 1
function Stream.SawOsc(freq)
	return ScanStream:new(freq, function(accu, f)
		return ((accu or 1) + 2*f/samplerate) % 2
	end):sub(1)
end

function Stream.SinOsc(freq)
	return Stream.Phasor(freq):mul(2*math.pi):sin()
end

-- Pulse between 0 and 1 in half a period (width = 0.5)
function Stream.PulseOsc(freq)
	return Stream.Phasor(freq):map(function(x)
		return x < 0.5 and 1 or 0
	end)
end

function Stream.SqrOsc(freq)
	return Stream.Phasor(freq):map(function(x)
		return x < 0.5 and 1 or -1
	end)
end

function Stream.TriOsc(freq)
	local abs = math.abs

	return Stream.SawOsc(freq):map(function(x)
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
-- this method since LuaJIT has problems
-- with invoking the __len metamethod (# operator).
function Stream:len()
	return math.huge -- infinity
end

-- implemented in applause.c
function Stream:play()
	error("C function not registered!")
end

-- implemented in applause.c
function Stream:fork()
	error("C function not registered!")
end

function Stream:save(filename, format)
	if self:len() == math.huge then
		error("Cannot save infinite stream")
	end

	local hnd = sndfile:new(filename, "SFM_WRITE",
	                        samplerate, 1, format)

	local tick = self:tick()

	while true do
		local sample = tick()
		if not sample then break end

		hnd:write(sample)
	end

	hnd:close()
end

function Stream:totable()
	if self:len() == math.huge then
		error("Cannot serialize infinite stream")
	end

	local tick = self:tick()
	local vector = table.new(self:len(), 0)

	while true do
		local value = tick()

		if not value then break end
		table.insert(vector, value)
	end

	return vector
end

function Stream:toplot(rows, cols)
	rows = rows or 25
	cols = cols or 80

	local scaled = self:resample(cols / self:len())
	                   :add(1):mul(rows/2):floor():totable()
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

-- Stream metamethods

function Stream:__len()	return self:len() end

function Stream:__call()
	return VectorStream:new(self:totable())
end

function Stream:__tostring()
	local t

	if self:len() > 1024 then
		t = self:sub(1, 1024):totable()
		table.insert(t, "...")
	else
		t = self:totable()
	end

	for i = 1, #t do t[i] = tostring(t[i]) end

	return "{"..table.concat(t, ", ").."}"
end

function Stream.__add(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1+x2
	end, op1, op2)
end

function Stream.__sub(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1-x2
	end, op1, op2)
end

function Stream.__mul(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1*x2
	end, op1, op2)
end

function Stream.__div(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1/x2
	end, op1, op2)
end

function Stream.__mod(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1%x2
	end, op1, op2)
end

function Stream.__pow(op1, op2)
	return ZipStream:new(function(x1, x2)
		return x1^x2
	end, op1, op2)
end

function Stream:__unm()	return self:mul(-1) end

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

VectorStream = DeriveClass(Stream, function(self, vector)
	self.vector = vector
end)

function VectorStream:tick()
	local i = 0

	return function()
		i = i + 1
		return self.vector[i]
	end
end

function VectorStream:len()
	return #self.vector
end

SndfileStream = DeriveClass(Stream, function(self, filename)
	self.filename = filename
end)

function SndfileStream:tick()
	-- NOTE: Opening the file here has the advantage
	-- that the stream can be reused multiple times
	-- FIXME: This fails if the file is not at the
	-- correct sample rate. Need to resample...
	local handle = sndfile:new(self.filename, "SFM_READ")

	return function()
		if not handle then return end

		local sample = handle:read()
		if not sample then
			handle:close()
			handle = nil
		end

		return sample
	end
end

function SndfileStream:len()
	-- Since the file is not yet opened,
	-- we must assume that its size never changes between
	-- calling len() and depending on the length reported
	local handle = sndfile:new(self.filename, "SFM_READ")
	local len = tonumber(handle.info.frames)

	handle:close()
	return len
end

ConcatStream = DeriveClass(Stream, function(self, ...)
	self.is_concatstream = true

	self.streams = {}
	for _, v in ipairs{...} do
		if v.is_concatstream then
			-- Optimization: Avoid redundant
			-- ConcatStream objects
			for _, s in ipairs(v.streams) do
				table.insert(self.streams, s)
			end
		else
			table.insert(self.streams, tostream(v))
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
end)

function ConcatStream:tick()
	local i = 1
	local ticks = {}

	for k = 1, #self.streams do
		ticks[k] = self.streams[k]:tick()
	end

	return function()
		while i <= #ticks do
			local sample = ticks[i]()

			if sample then return sample end

			-- try next stream
			i = i + 1
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

-- Ravel operation inspired by APL.
-- This removes one level of nesting from nested streams
-- (e.g. streams of streams), and is semantically similar
-- to folding the stream with the Concat operation.
RavelStream = DeriveClass(Stream, function(self, stream)
	self.stream = tostream(stream)
end)

function RavelStream:tick()
	local stream_tick = self.stream:tick()
	local current_tick = nil

	return function()
		while true do
			if current_tick then
				local value = current_tick()
				if value then return value end
				current_tick = nil
			end

			local value = stream_tick()

			if type(value) == "table" and value.is_a_stream then
				current_tick = value:tick()
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
	local t = self.stream()

	for i = 1, #t do
		len = len + (type(t[i]) == "table" and t[i].is_a_stream and
		             t[i]:len() or 1)
	end

	return len
end

IotaStream = DeriveClass(Stream, function(self, v1, v2)
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
end)

function IotaStream:tick()
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
SubStream = DeriveClass(Stream, function(self, stream, i, j)
	self.stream = tostream(stream)
	self.i = i
	self.j = j or -1

	local stream_len = self.stream:len()

	if self.i < 0 then self.i = self.i + stream_len + 1 end
	if self.j < 0 then self.j = self.j + stream_len + 1 end

	if self.i > stream_len or self.j > stream_len or
	   self.i > self.j then
		error("Invalid sub-stream range ["..self.i..","..self.j.."]")
	end
end)

function SubStream:tick()
	local tick = self.stream:tick()

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
IndexStream = DeriveClass(Stream, function(self, stream, index_stream)
	self.stream = tostream(stream)
	self.index_stream = tostream(index_stream)
end)

function IndexStream:tick()
	local stream_tick = self.stream:tick()
	local index_tick = self.index_stream:tick()

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

MapStream = DeriveClass(Stream, function(self, stream, fnc)
	self.stream = tostream(stream)
	self.fnc = fnc
end)

function MapStream:tick()
	local tick = self.stream:tick()

	return function()
		local sample = tick()
		return sample and self.fnc(sample)
	end
end

function MapStream:len()
	return self.stream:len()
end

ScanStream = DeriveClass(Stream, function(self, stream, fnc)
	self.stream = tostream(stream)
	self.fnc = fnc
end)

function ScanStream:tick()
	local tick = self.stream:tick()
	local last_sample = nil

	return function()
		local sample = tick()
		if not sample then return end

		last_sample = self.fnc(last_sample, sample)
		return last_sample
	end
end

function ScanStream:len()
	return self.stream:len()
end

FoldStream = DeriveClass(Stream, function(self, stream, fnc)
	self.stream = tostream(stream)
	self.fnc = fnc
end)

function FoldStream:tick()
	local tick = self.stream:tick()

	return function()
		local l, r

		while true do
			r = tick()
			if not r then break end

			l = l and self.fnc(l, r) or r
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
ZipStream = DeriveClass(Stream, function(self, fnc, ...)
	self.is_zipstream = true

	self.fnc = fnc

	self.streams = {}
	for _, v in ipairs{...} do
		v = tostream(v)
		if v.is_zipstream and v.fnc == fnc then
			-- Optimization: Avoid redundant
			-- ZipStream objects
			for _, s in ipairs(v.streams) do
				table.insert(self.streams, s)
			end
		else
			table.insert(self.streams, v)
		end
	end
end)

function ZipStream:tick()
	local running = true
	local ticks = {}

	for i = 1, #self.streams do
		ticks[i] = self.streams[i]:tick()
	end

	if #ticks == 2 then
		-- 2 streams are common, so use an unrolled
		-- version here
		return function()
			if not running then return end

			local sample1, sample2 = ticks[1](), ticks[2]()

			if not sample1 then
				running = sample2
				return sample2
			elseif not sample2 then
				-- have sample1, keep running
				return sample1
			end

			return self.fnc(sample1, sample2)
		end
	else
		return function()
			if not running then return end

			local result = nil

			for i = 1, #ticks do
				local sample = ticks[i]()

				if sample then
					result = result and self.fnc(result, sample)
					                or sample
				end
			end

			-- if all streams have ended, `result` will be nil
			running = result

			return result
		end
	end
end

function ZipStream:len()
	local max = 0

	for _, stream in pairs(self.streams) do
		max = math.max(max, stream:len())
	end

	return max
end

NoiseStream = DeriveClass(Stream)

function NoiseStream:tick()
	local random = math.random

	return function()
		return random()*2 - 1
	end
end

--
-- MIDI Support
--

-- Velocity of NOTE ON for a specific note on a channel
MIDIVelocityStream = DeriveClass(Stream, function(self, note, channel)
	self.note = note
	self.channel = channel or 1
end)

-- implemented in applause.c, private!
function MIDIVelocityStream.getValue(note, channel)
	error("C function not registered!")
end

function MIDIVelocityStream:tick()
	local note = self.note
	local channel = self.channel
	local getValue = self.getValue

	return function()
		return getValue(note, channel)
	end
end

-- Stream of integer words representing the last MIDI note
-- triggered on a channel with its corresponding velocity
-- (of the NOTE ON message).
-- The MIDI note is the lower byte and the velocity the
-- upper byte of the word.
MIDINoteStream = DeriveClass(Stream, function(self, channel)
	self.channel = channel or 1
end)

-- implemented in applause.c, private!
function MIDINoteStream.getValue(channel)
	error("C function not registered!")
end

function MIDINoteStream:tick()
	local channel = self.channel
	local getValue = self.getValue

	return function()
		return getValue(channel)
	end
end

MIDICCStream = DeriveClass(Stream, function(self, control, channel)
	self.control = control
	self.channel = channel or 1
end)

-- implemented in applause.c, private!
function MIDICCStream.getValue(control, channel)
	error("C function not registered!")
end

function MIDICCStream:tick()
	local control = self.control
	local channel = self.channel
	local getValue = self.getValue

	return function()
		return getValue(control, channel)
	end
end

-- MIDI primitives

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
	return mtof_cache[bit.band(note, 0xFF)]
end

function Stream:mtof() return self:map(mtof) end

-- Convert from frequency to closest MIDI note
function ftom(freq)
	-- NOTE: math.log/2 is a LuaJIT extension
	return math.floor(12*math.log(freq/440, 2) + 0.5)+69
end

function Stream:ftom() return self:map(ftom) end

-- primitives

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

FIRStream = DeriveClass(Stream, function(self, stream, freq_stream)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq_stream)
end)

function FIRStream:tick()
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

	local tick = self.stream:tick()
	local freq_tick = self.freq_stream:tick()

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

LPFStream = DeriveClass(Stream, function(self, stream, freq)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq)
end)

function LPFStream:tick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan

	local tick = self.stream:tick()
	local freq_tick = self.freq_stream:tick()
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

HPFStream = DeriveClass(Stream, function(self, stream, freq)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq)
end)

function HPFStream:tick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan

	local tick = self.stream:tick()
	local freq_tick = self.freq_stream:tick()
	local cur_freq = nil

	-- NOTE: Very similar to LPFStream.tick()
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

		local sample = tick()

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
BPFStream = DeriveClass(Stream, function(self, stream, freq, quality)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq)
	-- FIXME: Does this make sense to be a stream?
	self.quality = quality
end)

function BPFStream:tick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan
	local cos = math.cos

	local tick = self.stream:tick()
	local freq_tick = self.freq_stream:tick()
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

			local pfreq = cur_freq * radians_per_sample
			local pbw = 1 / self.quality*pfreq*0.5

			local C = 1/tan(pbw)
			local D = 2*cos(pfreq);

			a0 = 1/(1 + C)
			b1 = C*D*a0
			b2 = (1 - C)*a0
		end

		local sample = tick()

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
BRFStream = DeriveClass(Stream, function(self, stream, freq, quality)
	self.stream = tostream(stream)
	self.freq_stream = tostream(freq)
	-- FIXME: Does this make sense to be a stream?
	self.quality = quality
end)

function BRFStream:tick()
	local a0, b1, b2
	local y1, y2 = 0, 0

	-- some cached constants
	local radians_per_sample = (2*math.pi)/samplerate
	local sqrt2 = math.sqrt(2)

	-- some cached math table lookups
	local tan = math.tan
	local cos = math.cos

	local tick = self.stream:tick()
	local freq_tick = self.freq_stream:tick()
	local cur_freq = nil

	-- NOTE: Very similar to BPFStream.tick()
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

			local pfreq = cur_freq * radians_per_sample
			local pbw = 1 / self.quality*pfreq*0.5

			local C = tan(pbw)
			local D = 2*cos(pfreq);

			a0 = 1/(1 + C)
			b1 = -D*a0
			b2 = (1 - C)*a0
		end

		local sample = tick()

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

ffi.cdef[[
int kill(int pid, int sig);
]]

Client = DeriveClass(null, function(self, pid)
	self.pid = pid
end)

function Client:play()
	ffi.C.kill(self.pid, 10); -- SIGUSR1
end

function Client:stop()
	ffi.C.kill(self.pid, 12); -- SIGUSR2
end

function Client:kill()
	ffi.C.kill(self.pid, 15); -- SIGTERM
end

Client.__gc = Client.kill
