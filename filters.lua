---
--- @module applause
---

--
-- General-purpose IIR filters:
-- These are direct translations of ChucK's LPF, HPF, BPF and BRF
-- ugens which are in turn adapted from SuperCollider 3.
-- See also https://chuck.stanford.edu/doc/program/ugen_full.html#LPF
--

--- De-denormalize function adapted from ChucK.
-- Not quite sure why this is needed - properly to make the
-- IIR filters numerically more stable.
local function ddn(f)
	return f >= 0 and (f > 1e-15 and f < 1e15 and f or 0) or
	                  (f < -1e-15 and f > -1e15 and f or 0)
end

--- Low Pass Filter streams.
-- @type LPFStream
-- @local
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

--- Apply Low Pass Filter to stream.
-- This is a resonant filter (2nd order Butterworth).
-- @within Class Stream
-- @StreamableNumber freq Cutoff frequency.
-- @treturn Stream
function Stream:LPF(freq)
	return LPFStream:new(self, freq)
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

--- Apply High Pass Filter to stream.
-- This is a resonant filter (2nd order Butterworth).
-- @within Class Stream
-- @StreamableNumber freq Cutoff frequency.
-- @treturn Stream
function Stream:HPF(freq)
	return HPFStream:new(self, freq)
end

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

--- Apply Band Pass Filter to stream (2nd order Butterworth).
-- @within Class Stream
-- @StreamableNumber freq Center frequency.
-- @StreamableNumber quality
--   The quality factor, indirectly proportional to the passband width.
-- @treturn Stream
function Stream:BPF(freq, quality)
	return BPFStream:new(self, freq, quality)
end

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

--- Apply Band Reject Filter to stream (2nd order Butterworth).
-- @within Class Stream
-- @StreamableNumber freq Center frequency.
-- @StreamableNumber quality
--   The quality factor, indirectly proportional to the rejectband width.
-- @treturn Stream
function Stream:BRF(freq, quality)
	return BRFStream:new(self, freq, quality)
end

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
