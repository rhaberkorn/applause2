---
--- @module applause
---
-- TODO:
-- Windowing
-- Jupyter Notebook
-- Documentation
local bit = require "bit"
local ffi = require "ffi"

local complex = ffi.typeof('double complex')

-- NOTE: LuaJIT 2.1 does not yet support complex number arithmetics
local function complex_add(a, b)
	return complex(a.re + b.re, a.im + b.im)
end
local function complex_sub(a, b)
	return complex(a.re - b.re, a.im - b.im)
end
local function complex_mul(a, b)
	return complex(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re)
end
local function complex_conj(x)
	return complex(x.re, -x.im)
end

-- The FFT() function returns the complex frequency spectrum, but may modify the
-- input array in-place.
-- This is important in order to avoid reallocations when performing FFT at real-time.

-- Direct application of the DFT formula.
-- This has complexity O(n^2) and is here only for reference.
--[[
function FFT(samples, invert)
	local ret = table.new(#samples, 0)

	-- FIXME: For real-valued signals, the spectrum is mirrored,
	-- so it would suffice to calculate #samples/2.
	for k = 1, #samples do
		ret[k] = complex()

		for n = 1, #samples do
			local sample = complex(samples[n])
			-- Euler's formula: exp(I*x) = cos(x) + I*sin(x)
			-- exp(-2*pi*I/N * n*k) == cos(-2*pi/N * n*k) + I*sin(-2*pi/N * n*k)
			local twiddle = (inverse and 1 or -1)*2*math.pi/#samples * (n-1) * (k-1)
			local q = complex(math.cos(twiddle), math.sin(twiddle))

			q = complex_mul(sample, q)
			ret[k] = complex_add(ret[k], q)
		end
	end

	return ret
end
]]

-- Naive implementation of a radix-2 decimation-in-time (DIT) FFT
-- via the Cooley-Tukey algorithm.
-- This has complexity O(n*log2(n)), but requires lots of allocations.
-- I didn't even try to optimize it.
-- Samples can be real-values (audio samples), but also complex values
-- (the frequency spectrum).
-- #samples must be a power of 2.
-- A frequency spectrum result should always be mirrored.
--
-- Each bin i in the resulting frequency spectrum represents frequency
-- (i-1)*samplerate/#samples.
--[[
function FFT(samples, inverse)
	if #samples == 1 then return {complex(samples[1])} end

	local samples_even = table.new(#samples/2, 0)
	local samples_odd  = table.new(#samples/2, 0)
	for i = 1, #samples/2 do
		samples_even[i], samples_odd[i] = samples[2*i], samples[2*i-1]
	end

	samples_even, samples_odd = FFT(samples_even, inverse), FFT(samples_odd, inverse)
	for k = 1, #samples/2 do
		-- The odd and even sequences are swapped compared to Wikipedia since
		-- our arrays have origin 1.
		local p = samples_odd[k]
		-- Euler's formula: exp(I*x) = cos(x) + I*sin(x)
		-- exp(-2*pi*I/N * k) == cos(-2*pi/N * k) + I*sin(-2*pi/N * k)
		local twiddle = (inverse and 1 or -1)*2*math.pi/#samples * (k-1)
		local q = complex(math.cos(twiddle), math.sin(twiddle))

		q = complex_mul(samples_even[k], q)

		samples[k] = complex_add(p, q)
		samples[k + #samples/2] = complex_sub(p, q)
	end

	return samples
end
]]

-- Implementation of a radix-2 decimation-in-time (DIT) FFT
-- via the Cooley-Tukey algorithm.
-- This is still an out-of-place algorithm, but requires allocations
-- of auxilliary memory (2*N) only once.
-- On the other hand, this version does not require a potentially costly
-- bit reversal operation.
-- This has complexity O(N*log2(N)).
-- For real inputs, the complex output array will conjugate symmetrical.
-- But the second half is actually cheap to calculate.
-- See https://literateprograms.org/cooley-tukey_fft_algorithm__c_.html
local function FFT_internal(x, x_start, N, skip, X, X_start, E, E_start, twiddles)
	if N == 1 then
		-- NOTE: This is responsible for the mirrorring, important for FFT_backward()
		X[X_start] = x_start <= #x and complex(x[x_start]) or complex_conj(x[2*#x - x_start])
		return
	end

	local D, D_start = E, E_start+N/2

	-- calculate the odd/even values
	FFT_internal(x, x_start, N/2, skip*2, E, E_start, X, X_start, twiddles)
	FFT_internal(x, x_start+skip, N/2, skip*2, D, D_start, X, X_start, twiddles)

	for k = 0, N/2-1 do
		-- FIXME: Is it really important to overwrite D?
		D[D_start+k] = complex_mul(twiddles[k*skip+1], D[D_start+k])
		X[X_start+k] = complex_add(E[E_start+k], D[D_start+k])
		X[X_start+k + N/2] = complex_sub(E[E_start+k], D[D_start+k])
	end
end

-- For real-valued inputs, complex outputs (audio samples to frequency spectrums):
-- The frequency spectrum is always conjugate symmetric, so `out` can be #samples/2+1
-- which significantly simplifies manipulations in the frequency domain.
local function FFT_forward(samples, out, E, twiddles)
	local D, D_start = E, 1+#samples/2

	FFT_internal(samples, 1, #samples/2, 2, E, 1, out, 1, twiddles)
	FFT_internal(samples, 2, #samples/2, 2, D, D_start, out, 1, twiddles)

	local t = complex_mul(twiddles[1], D[D_start])
	out[1] = complex_add(E[1], t)
	out[1 + #samples/2] = complex_sub(E[1], t)

	for k = 1, #samples/2-1 do
		t = complex_mul(twiddles[k+1], D[D_start+k])
		out[1+k] = complex_add(E[1+k], t)
	end

	return out
end

-- For complex-valued inputs and real-valued outputs (frequency spectrums to audio samples):
-- The frequency spectrum is made to be mirrored, so `out` will be twice the length of `samples` minus 2.
-- NOTE: You will still need to generate twiddles with get_twiddles(true).
local function FFT_backward(samples, out, E, twiddles)
	local size = (#samples-1)*2 -- number of output samples
	local D, D_start = E, 1+size/2

	FFT_internal(samples, 1, size/2, 2, E, 1, out, 1, twiddles)
	FFT_internal(samples, 2, size/2, 2, D, D_start, out, 1, twiddles)

	for k = 0, size/2-1 do
		-- see complex_mul(): we need only the real part
		local t = twiddles[k+1].re*D[D_start+k].re - twiddles[k+1].im*D[D_start+k].im
		out[1+k] = (E[1+k].re + t)/size
		out[1+k + size/2] = (E[1+k].re - t)/size
	end

	return out
end

local function get_twiddles(N, inverse)
	local ret = table.new(N/2, 0)
	for k = 1, N/2 do
		-- Euler's formula: exp(I*x) = cos(x) + I*sin(x)
		-- exp(-2*pi*I/N * n*k) == cos(-2*pi/N * n*k) + I*sin(-2*pi/N * n*k)
		local twiddle = (inverse and 1 or -1)*2*math.pi*(k-1)/N
		ret[k] = complex(math.cos(twiddle), math.sin(twiddle))
	end
	return ret
end

function FFT(samples)
	assert(type(samples) == "table")
	if samples.is_a_stream then samples = samples:totable() end

	assert(bit.band(#samples, #samples-1) == 0, "Array length needs to be a power of 2")

	local out = table.new(#samples/2+1, 0)
	local scratch = table.new(#samples, 0)
	local twiddles = get_twiddles(#samples)

	return FFT_forward(samples, out, scratch, twiddles)
end

-- Convert complex frequency spectrum to magnitude/amplitude per frequency.
-- FIXME: Should this be a Stream method?
-- Would also resolve having to choose between inplace and out-of-place calculation.
function magnitude(spectrum)
	for i = 1, #spectrum do
		-- This is also the absolute value.
		spectrum[i] = math.sqrt(spectrum[i].re^2 + spectrum[i].im^2)
	end
	return spectrum
end
-- Convert frequency spectrum to phase in value between [0,1].
-- Why the threshold is necessary, see
-- https://www.gaussianwaves.com/2015/11/interpreting-fft-results-obtaining-magnitude-and-phase-information/
function phase(spectrum, threshold)
	threshold = threshold or 0.1
	for i = 1, #spectrum do
		local abs = math.sqrt(spectrum[i].re^2 + spectrum[i].im^2)
		spectrum[i] = abs > threshold and math.atan2(spectrum[i].im, spectrum[i].re)/math.pi or 0
	end
	return spectrum
end

-- Inverse FFT.
-- Converts complex samples in the frequency domain to real-values samples in the
-- time domain (ie. sound).
function IFFT(spectrum)
	assert(type(spectrum) == "table")
	if spectrum.is_a_stream then spectrum = spectrum:totable() end

	local size = (#spectrum-1)*2
	assert(bit.band(size, size-1) == 0, "Spectrum length-1 needs to be a power of 2")

	local out = table.new(size, 0)
	local scratch = table.new(size, 0)
	local twiddles = get_twiddles(size, true)

	return tostream(FFT_backward(spectrum, out, scratch, twiddles))
end

-- Analyze a potentially realtime stream
-- This gives a stream of frequency spectrums
-- (of half the length).
-- FIXME: It's unelegant to pass in the window function as an argument.
-- This could only be avoided by moving the partitioning out of this method
-- and repeating the buffer size once again. E.g.:
-- foo:partition(1024):map(Hamming):FFT(1024)...
-- Of course, if we had another FFT implementation like Danielson-Lanczos,
-- that does not require preallocation of a few arrays, this problem also wouldn't
-- exist.
-- These usually represent the complex spectrum as 2 real entries, so they can entirely
-- work inplace.
function Stream:FFT(size, window_fnc)
	assert(bit.band(size, size-1) == 0, "Size needs to be a power of 2")

	local out = table.new(size/2+1, 0)
	local scratch = table.new(size, 0)
	local twiddles = get_twiddles(size)

	return self:partition(size):map(function(samples)
		assert(#samples == size)
		if window_fnc then
			samples = window_fnc(samples)
		end
		return FFT_forward(samples, out, scratch, twiddles)
	end)
end

-- NOTE: Here `size` refers to the size of the time domain chunks,
-- ie. the same value you pass into Stream:FFT().
-- A different FFT implementation like Danielson-Lanczos that works entirely
-- inplace, might not need repitition of these sizes.
function Stream:IFFT(size)
	-- NOTE: The size could be inferred from the first spectrum array,
	-- but we try to avoid allocations at tick() time at all costs.
	local out = table.new(size, 0)
	local scratch = table.new(size, 0)
	local twiddles = get_twiddles(size, true)

	return self:map(function(spectrum)
		assert(#spectrum == size/2+1)
		-- FIXME: Creating a VectorStream is not fully real-time-safe
		-- because of the allocations involved.
		-- RavelStream also shouldn't really unravel streams of arrays.
		-- This could be worked around with a new IFFTStream class optimized
		-- for our usecase.
		return VectorStream:new(FFT_backward(spectrum, out, scratch, twiddles))
	end):ravel()
end

--
-- Windowing functions
-- See also the commented-out code in filters.lua.
--

function Hamming(samples)
	assert(type(samples) == "table")
	if samples.is_a_stream then samples = samples:totable() end

	--assert(bit.band(#samples, #samples-1) == 0, "Array length needs to be a power of 2")

	local cos = math.cos
	local pi2 = 2*math.pi

	for i = 1, #samples do
		local alpha = 0.54
		samples[i] = samples[i]*(alpha - (1-alpha)*cos((pi2*i)/(#samples-1)))
	end

	return samples
end
