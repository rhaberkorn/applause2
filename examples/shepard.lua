--
-- adapted from ChucK's shepard.ck (Ge Wang, 2016)
--
local min, max, exp = math.min, math.max, math.exp

local function dbtorms(f)
	f = min(max(f, 0), 485)
	return exp((2.302585092994 * 0.05) * (f-100))
end

local SQRT_PI2 = math.sqrt(2*math.pi)
local function gauss(x, mu, sd)
	return (1.0 / (sd*SQRT_PI2)) * exp(-(x-mu)^2 / (2*sd^2))
end

-- built-in mtof() caches all 128 MIDI notes
local function mtof_raw(note)
	return 440 * 2^((note - 69)/12)
end

-- mean for normal intensity curve
local MU = 66
-- standard deviation for normal intensity curve
local SIGMA = 42
-- normalize to 1.0 at x==MU
local SCALE = 1 / gauss(MU, MU, SIGMA)
-- increment per sample (use negative for descending)
--local INC = tostream(.004 / msec(1))
local INC = EvdevStream("TrackPoint"):evrel('REL_X'):scale(-0.008, 0.008):div(msec(1)):cache()

-- starting pitches (in MIDI note numbers, octaves apart)
local pitches = {12, 24, 36, 48, 60, 72, 84, 96, 108}

local function mtoamp(note)
	-- compute loundess for each tone
	local intensity = gauss(note, MU, SIGMA)*SCALE
	-- map intensity to amplitude
	return dbtorms(intensity*96)
end

tostream(pitches):map(function(m)
	local tone = INC:scan(function(last, inc)
		local next = (last or m)+inc
		if next > 120 then
			next = next - 108
		elseif next < 12 then
			next = next + 108
		end
		return next
	end):cache()
	return tone:map(mtof_raw):TriOsc() * tone:map(mtoamp)
end):fold(function(a, b) return a+b end):ravel():gain(1/#pitches):play()
