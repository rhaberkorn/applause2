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
	while true do coroutine.yield(self.value) end	
end

function Stream:map(fnc)
	return MapStream:new(self, fnc)
end

function Stream:ceil()  return self:map(math.ceil) end
function Stream:cos()   return self:map(math.cos) end
function Stream:cosh()  return self:map(math.cosh) end
function Stream:exp()   return self:map(math.exp) end
function Stream:floor() return self:map(math.floor) end
function Stream:sin()   return self:map(math.sin) end
function Stream:sinh()  return self:map(math.sinh) end
function Stream:sqrt()  return self:map(math.sqrt) end
function Stream:tan()   return self:map(math.tan) end
function Stream:tanh()  return self:map(math.tanh) end

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

-- Stream metamethods

function Stream:__len()	return self:len() end

function Stream:__call()
	if self:len() == math.huge then error("Cannot serialize infinite stream") end

	local co = coroutine.wrap(self.tick)
	local vector = {}

	while true do
		local sample = co(self)
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
	for _, v in ipairs(self.vector) do
		coroutine.yield(tonumber(v))
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
	for _, stream in ipairs(self.streams) do
		stream:tick()		
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
	for i = self.from, self.to do coroutine.yield(i) end
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
	local co = coroutine.wrap(self.stream.tick)

	-- skip until self.i
	for _ = 1, self.i - 1 do co(self.stream) end

	-- produce samples i to j
	for _ = self.i, self.j do coroutine.yield(co(self.stream)) end
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
	local stream_len = self.stream:len()
	local co = coroutine.wrap(self.stream.tick)
	local index_co = coroutine.wrap(self.index_stream.tick)

	-- cache of samples generated by stream
	local cache = {}

	while true do
		local index_sample = index_co(self.index_stream)
		if not index_sample then return end

		if index_sample < 1 or index_sample > stream_len or
		   index_sample == math.huge then
			error("Index "..index_sample.." out of range")
		end

		local index_floor, index_ceil = math.floor(index_sample),
		                                math.ceil(index_sample)

		while #cache < index_ceil do
			table.insert(cache, co(self.stream))
		end

		-- applies linear interpolation if index_sample is
		-- not an integer
		coroutine.yield(cache[index_floor] +
		                (cache[index_ceil] - cache[index_floor])*
		                (index_sample - index_floor))
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
	local coroutines = {}

	for k, stream in pairs(self.streams) do
		coroutines[k] = coroutine.create(stream.tick)
	end

	while true do
		local sum = 0
		local running = false

		for k = 1, #self.streams do
			if coroutine.status(coroutines[k]) ~= "dead" then
				local state, sample = coroutine.resume(coroutines[k], self.streams[k])
				if not state then error(sample) end
			
				running = running or sample
				sum = sum + (sample or 0)
			end
		end
		if not running then return end

		coroutine.yield(sum)
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
	local coroutines = {}

	for k, stream in pairs(self.streams) do
		coroutines[k] = coroutine.create(stream.tick)
	end

	while true do
		local product = 1
		local running = false

		for k = 1, #self.streams do
			if coroutine.status(coroutines[k]) ~= "dead" then
				local state, sample = coroutine.resume(coroutines[k], self.streams[k])
				if not state then error(sample) end
			
				running = running or sample
				product = product * (sample or 1)
			end
		end
		if not running then return end

		coroutine.yield(product)
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
	local co = coroutine.wrap(self.stream.tick)

	while true do
		local sample = co(self.stream)
		if not sample then return end
		coroutine.yield(self.fnc(sample))
	end
end

function MapStream:len()
	return self.stream:len()
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
-- FIXME: Get this from sound backend
samplerate = 44100

-- Time units: Convert between time and sample numbers
-- These are functions, so we can round the result
function seconds(x) return math.floor(samplerate*x) end
function mseconds(x) return seconds(x/1000) end

-- Wave forms
function SawOsc(freq)
	local accu = 0

	return MapStream:new(freq, function(x)
		accu = (accu + x/samplerate) % 1
		return accu
	end)
end

function SinOsc(freq)
	-- The following mapping is equivalent to but more efficient as:
	-- return (SawOsc(freq)*(2*math.pi)):sin()

	local accu = 0

	return MapStream:new(freq, function(x)
		accu = (accu + x/samplerate) % 1
		return math.sin(accu*2*math.pi)
	end)
end
