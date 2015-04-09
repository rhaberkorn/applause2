posix = require "posix"

-- measure time required to execute fnc()
function benchmark(fnc)
	local t1_s, t1_ns = posix.clock_gettime("process_cputime_id")
	fnc()
	local t2_s, t2_ns = posix.clock_gettime("process_cputime_id")

	local t1_ms = t1_s*1000 + t1_ns/1000000
	local t2_ms = t2_s*1000 + t2_ns/1000000

	print("Elapsed CPU time: "..(t2_ms - t1_ms).."ms")
end

function DeriveClass(base, ctor)
	local class = {}

	if base then
		class = base:new()

		-- we cannot derive metamethod tables, so we
		-- copy all relevant metamethods
		for _, m in pairs{"len", "call", "tostring",
		                  "unm", "add", "sub", "mul", "div",
		                  "concat", "lt", "le"} do
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

function Stream:clip(min, max)
	min = min or -1
	max = max or 1

	return self:map(function(x)
		return math.min(math.max(x, min), max)
	end)
end

-- Scale [-1,+1] signal to [lower,upper]
-- lower is optional and defaults to 0
function Stream:scale(v1, v2)
	local lower = v2 and v1 or 0
	local upper = v2 or v1

	-- return (self + 1)*(0.5 * (upper - lower)) + lower
	-- This requires less streams and is thus faster:
	return self*((upper - lower)/2) + ((upper + lower)/2)
end

function Stream:scan(fnc)
	return ScanStream:new(self, fnc)
end

function Stream:fold(fnc)
	return FoldStream:new(self, fnc)
end

function Stream:sub(i, j)
	return SubStream:new(self, i, j)
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

-- Stream metamethods

function Stream:__len()	return self:len() end

function Stream:__call()
	if self:len() == math.huge then error("Cannot serialize infinite stream") end

	local tick = self:tick()
	local vector = {}

	while true do
		local sample = tick()

		if not sample then break end
		table.insert(vector, sample)
	end

	return vector
end

function Stream:__tostring()
	if self:len() > 1024 then
		return table.concat(self:sub(1, 1024)(), " ").."..."
	else
		return table.concat(self(), " ")
	end
end

function Stream:__unm()
	-- should be efficient enough,
	-- no need to implemenent a NegateStream and
	-- certainly better than MulStream:new(self, -1)
	return self:map(function(x) return -x end)
end

function Stream.__add(op1, op2)
	return AddStream:new(op1, op2)
end

function Stream.__sub(op1, op2)
	-- FIXME: May be made more efficient if we use
	-- a higher order stream composition
	return AddStream:new(op1, -tostream(op2))
end

function Stream.__mul(op1, op2)
	return MulStream:new(op1, op2)
end

function Stream.__div(op1, op2)
	-- FIXME: May be made more efficient if we use
	-- a higher order stream composition
	return MulStream:new(op1, MapStream:new(op2, function(x)
		return 1/x
	end))
end

function Stream.__concat(op1, op2)
	return ConcatStream:new(op1, op2)
end

-- FIXME: Length comparisions can already be written
-- elegantly - perhaps these operators should have
-- more APLish semantics instead?

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

ConcatStream = DeriveClass(Stream, function(self, ...)
	self.streams = {}
	for k, v in pairs{...} do
		self.streams[k] = tostream(v)
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
		while i <= #self.streams do
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

IndexStream = DeriveClass(Stream, function(self, stream, index_stream)
	self.stream = tostream(stream)
	self.index_stream = tostream(index_stream)
end)

function IndexStream:tick()
	local stream_tick = self.stream:tick()
	local index_tick = self.index_stream:tick()

	local stream_len = self.stream:len()

	-- cache of samples generated by stream
	local cache = {}

	return function()
		local index_sample = index_tick()

		if not index_sample then return end

		if index_sample < 1 or index_sample > stream_len or
		   index_sample == math.huge then
			error("Index "..index_sample.." out of range")
		end

		local index_floor, index_ceil = math.floor(index_sample),
		                                math.ceil(index_sample)

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

-- FIXME: Perhaps AddStream and MulStream could be unified
-- into one higher order stream that applies a function to
-- a number of samples

AddStream = DeriveClass(Stream, function(self, ...)
	self.streams = {}
	for k, v in pairs{...} do
		self.streams[k] = tostream(v)
	end
end)

function AddStream:tick()
	local running = true
	local ticks = {}

	for i = 1, #self.streams do
		ticks[i] = self.streams[i]:tick()
	end

	return function()
		if not running then return end

		local sum = 0

		running = nil
		for i = 1, #ticks do
			local sample = ticks[i]()

			if sample then
				running = true
				sum = sum + sample
			end
		end

		return running and sum
	end
end

function AddStream:len()
	local max = 0

	for _, stream in pairs(self.streams) do
		max = math.max(max, stream:len())
	end

	return max
end

MulStream = DeriveClass(Stream, function(self, ...)
	self.streams = {}
	for k, v in pairs{...} do
		self.streams[k] = tostream(v)
	end
end)

function MulStream:tick()
	local running = true
	local ticks = {}

	for i = 1, #self.streams do
		ticks[i] = self.streams[i]:tick()
	end

	return function()
		if not running then return end

		local product = 1

		running = nil
		for i = 1, #ticks do
			local sample = ticks[i]()

			if sample then
				running = true
				product = product * sample
			end
		end

		return running and product
	end
end

function MulStream:len()
	local max = 0

	for _, stream in pairs(self.streams) do
		max = math.max(max, stream:len())
	end

	return max
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

NoiseStream = DeriveClass(Stream)

function NoiseStream:tick()
	return function()
		return math.random()*2 - 1
	end
end

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

-- Sample rate
-- This is overwritten by the C core
samplerate = 44100

-- Time units: Convert between time and sample numbers
-- These are functions, so we can round the result
-- automatically
function sec(x) return math.floor(samplerate*(x or 1)) end
function msec(x) return sec((x or 1)/1000) end

-- Wave forms
function SawOsc(freq)
	return ScanStream:new(freq, function(accu, f)
		return ((accu or 0) + f/samplerate) % 1
	end)
end

function SinOsc(freq)
	return (SawOsc(freq)*(2*math.pi)):sin()
end

function PulseOsc(freq)
	return SawOsc(freq):map(function(x)
		return x > 0.5 and 1 or 0
	end)
end
