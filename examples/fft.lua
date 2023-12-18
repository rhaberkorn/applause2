-- Plot magnitude of frequency spectrum.
-- Exactly 20 sine cycles in 1024 samples.
-- So this does not need windowing.
tostream(magnitude(FFT(Stream.SinOsc(samplerate*20/1024):sub(1, 1024)))):gnuplot()

IFFT(FFT(Stream.SinOsc(samplerate*20/1024):sub(1, 1024))):gnuplot()

-- Here the results are much better with a windowing function.
-- For some strange reason, the window is not necessary to reconstruct the original wave
-- via IFFT().
tostream(magnitude(FFT(Hamming(Stream.SinOsc(samplerate*20.5/1024):sub(1, 1024))))):gnuplot()

-- Phase plotting
tostream(phase(FFT(Stream.SinOsc(samplerate*20/1024, 0.7):sub(1, 1024)))):gnuplot()

-- Naive pitch shifting
-- This is not easy to get right. See https://www.reddit.com/r/DSP/comments/k6t24c/pitch_shifting_algorithm_in_frequency_domain/
haiku = SndfileStream("examples/haiku.flac")
haiku:FFT(1024):map(function(spectrum)
	assert(#spectrum == 513)
	-- NOTE: We cannot use Stream:resample() as it won't work with complex samples.
	for i = 1, 512/2 do spectrum[i] = spectrum[i*2] end
	for i = 512/2+1, 512 do spectrum[i] = 0i end
	return spectrum
end):IFFT(1024):play()

-- Noisy stream
noisy = Stream.SinOsc(440):mix(NoiseStream, 0.4)
tostream(magnitude(FFT(Hamming(noisy:sub(1, 1024))))):mul(0.05):gnuplot()

-- Naive noise cancelling
noisy:FFT(1024, Hamming):map(function(spectrum)
	assert(#spectrum == 513)
	for i = 1, #spectrum do
		if (spectrum[i].re^2 + spectrum[i].im^2)^.5*0.05 < 0.8 then spectrum[i] = 0i end
	end
	return spectrum
end):IFFT(1024):play()
