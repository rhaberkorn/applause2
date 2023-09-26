---
--- @module applause
---
local bit = require "bit"
local ffi = require "ffi"
local C = ffi.C

cdef_include "midi.h"

--- MIDI event stream.
-- Since it does not have parameters, you don't necessarily have to instantiate it.
-- The class table itself is a valid stream object.
--
-- MIDI events are 24-bit words with the
-- [following structure](https://www.codecademy.com/resources/docs/markdown/tables):
--
-- | Bit 23-16 | 15-8 | 7-4 | 3-0 |
-- | --------- | ---- | --- | --- |
-- | Controller value / velocity | Controller number / key | Command code | Channel number |
--
-- MIDI streams can and are usually ticked at the sample rate but will generate 0 words
-- in absence of events.
--
-- @type MIDIStream
-- @usage Stream.SinOsc(440):gain(MIDIStream:CC(0):scale()):play()
-- @fixme Theoretically, we could pass on a C structure as well.
-- On the other hand, bit manipulations are necessary anyway to parse the message.
MIDIStream = DeriveClass(Stream)

--- Create new MIDIStream
-- @function new
-- @treturn MIDIStream
-- @see Stream:CC
-- @see Stream:mvelocity

function MIDIStream:gtick()
	return function()
		-- This is always cached since there is only one MIDI event queue
		-- and it must not be pulled more than once per tick.
		local sample = sampleCache[MIDIStream]
		if not sample then
			sample = C.applause_pull_midi_sample()
			sampleCache[MIDIStream] = sample
		end
		return sample
	end
end

--- Filter out last value of a specific MIDI control channel, scaled to [-1,+1].
-- This remembers the last value and is therefore a stream, representing the controller state.
-- It is usually applied on @{MIDIStream}.
-- @within Class Stream
-- @int control Controller number between [0,127].
-- @int[opt=0] channel MIDI channel between [0,15].
-- @treturn Stream Stream of numbers between [-1,+1].
-- @see MIDIStream
-- @usage Stream.SinOsc(440):gain(MIDIStream:CC(0):scale()):play()
-- @fixme Most MIDI software appears to use origin 1 channels and control ids.
-- @fixme Is there actually any reason to keep Stream:CC instead of Stream:CC14?
-- Stream:CC14 will be slightly slower in case of MIDI events, but those are rare.
-- Or are there any controllers that will use controller ids >= 0x20 for other purposes?
function Stream:CC(control, channel)
	channel = channel or 0

	assert(0 <= control and control <= 127,
	       "MIDI control number out of range (0 <= x <= 127)")
	assert(0 <= channel and channel <= 15,
	       "MIDI channel out of range (0 <= x <= 15)")

	local filter = bit.bor(0xB0, channel, bit.lshift(control, 8))
	local band, rshift = bit.band, bit.rshift

	return self:scan(function(last, sample)
		last = last or 0
		local sample_masked = band(sample, 0xFFFF)
		if sample_masked == filter then
			return tonumber(rshift(sample, 16)*2)/0x7F - 1
		end
		return last
	end)
end

--- Filter out last value of a specific **14-bit** MIDI control channel, scaled to [-1,+1].
-- This remembers the last value and is therefore a stream, representing the controller state.
-- It is usually applied on @{MIDIStream}.
-- In contrast to @{Stream.CC}, this supports 14-bit controllers where the
-- least-significant byte is sent on controller with offset 0x20.
-- @within Class Stream
-- @int control Controller number between [0,127].
-- @int[opt=0] channel MIDI channel between [0,15].
-- @treturn Stream Stream of numbers between [-1,+1].
-- @see MIDIStream
-- @see Stream:CC
-- @usage Stream.SinOsc(440):gain(MIDIStream:CC14(0):scale()):play()
-- @fixme Most MIDI software appears to use origin 1 channels and control ids.
function Stream:CC14(control, channel)
	channel = channel or 0

	assert(0 <= control and control <= 127,
	       "MIDI control number out of range (0 <= x <= 127)")
	assert(0 <= channel and channel <= 15,
	       "MIDI channel out of range (0 <= x <= 15)")

	local filter_msb = bit.bor(0xB0, channel, bit.lshift(control, 8))
	local value_msb = 0
	local filter_lsb = bit.bor(0xB0, channel, bit.lshift(bit.bor(0x20, control), 8))
	local value_lsb = 0
	local band, bor, rshift = bit.band, bit.bor, bit.rshift

	return self:scan(function(last, sample)
		last = last or 0
		local sample_masked = band(sample, 0xFFFF)
		if sample_masked == filter_msb then
			value_msb = rshift(band(sample, 0xFF0000), 8+1)
			-- There is some redundancy, but it's important to scale here
			-- in order to optimize the common case (unchanged CC).
			return tonumber(bor(value_msb, value_lsb)*2)/0x3FFF - 1
		elseif sample_masked == filter_lsb then
			value_lsb = rshift(sample, 16)
			return tonumber(bor(value_msb, value_lsb)*2)/0x3FFF - 1
		end
		return last
	end)
end

--- Filter out last value of a MIDI note velocity.
-- This will be a stream of velocity values as long as the given note is on and otherwise 0.
-- It is usually applied on @{MIDIStream}.
-- @within Class Stream
-- @tparam string|int note A MIDI note name (eg. "A4") or number between [0,127].
-- @int[opt=0] channel MIDI channel between [0,15].
-- @treturn Stream
--   Stream of velocities between [0,127].
--   The note is on as long as the value is not 0.
-- @see MIDIStream
-- @see ntom
-- @usage Stream.SinOsc(ntof("C4")):gain(MIDIStream:mvelocity("C4") / 127):play()
-- @fixme Perhaps it also makes sense to scale to [-1,+1]?
-- A velocity will very seldom be used in situations where such a signal is useful, though.
function Stream:mvelocity(note, channel)
	note = type(note) == "string" and ntom(note) or note
	channel = channel or 0

	assert(0 <= note and note <= 127,
	       "MIDI note out of range (0 <= x <= 127)")
	assert(0 <= channel and channel <= 15,
	       "MIDI channel out of range (0 <= x <= 15)")

	local filter_on  = bit.bor(0x90, channel, bit.lshift(note, 8))
	local filter_off = bit.bor(0x80, channel, bit.lshift(note, 8))
	local band, rshift = bit.band, bit.rshift

	return self:scan(function(last, sample)
		last = last or 0
		local sample_masked = band(sample, 0xFFFF)
		return sample_masked == filter_on and rshift(sample, 16) or
		       sample_masked == filter_off and 0 or last
	end)
end

---
--- @section end
---

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

	--- Convert MIDI note number to name.
	-- See also the [conversion table](https://newt.phys.unsw.edu.au/jw/notes.html).
	-- @int note MIDI note number.
	-- @treturn string MIDI note name in all upper case.
	-- @see Stream:mton
	-- @see ntom
	function mton(note)
		note = band(note, 0xFF)
		local octave = floor(note / 12)-1
		return note_names[(note % 12)+1]..octave
	end

	--- Convert stream of MIDI note numbers to names.
	-- @within Class Stream
	-- @treturn Stream
	-- @see mton
	function Stream:mton() return self:map(mton) end

	local ntom_offsets = {}
	for i, name in ipairs(note_names) do
		ntom_offsets[name] = i-1
		-- Saving the offsets for the lower-cased note names
		-- avoids a string.upper() call in ntom()
		ntom_offsets[name:lower()] = i-1
	end

	--- Convert MIDI note name to number.
	-- See also the [conversion table](https://newt.phys.unsw.edu.au/jw/notes.html).
	-- @string name MIDI note name (case insensitive).
	-- @treturn int MIDI note number between [0,127].
	-- @see Stream:ntom
	-- @see mton
	function ntom(name)
		local octave = name:byte(-1) - 48 + 1
		return octave*12 + ntom_offsets[name:sub(1, -2)]
	end

	--- Convert stream of MIDI note names to numbers.
	-- @within Class Stream
	-- @treturn Stream
	-- @see ntom
	function Stream:ntom() return self:map(ntom) end

	-- There are only 128 possible MIDI notes,
	-- so their frequencies can and should be cached.
	-- We do this once instead of on-demand, so the lookup
	-- table consists of consecutive numbers.
	local mtof_cache = table.new(128, 0)
	for note = 0, 127 do
		-- MIDI NOTE 69 corresponds to 440 Hz
		mtof_cache[note] = 440 * 2^((note - 69)/12)
	end

	--- Convert from MIDI note to frequency.
	-- See also the [conversion table](https://newt.phys.unsw.edu.au/jw/notes.html).
	-- @int note MIDI note number between [0,127].
	-- @treturn number Frequency
	-- @see Stream:mtof
	-- @see ftom
	function mtof(note)
		return mtof_cache[band(note, 0xFF)]
	end

	--- Convert stream of MIDI note numbers to frequencies.
	-- @within Class Stream
	-- @treturn Stream
	-- @see mtof
	function Stream:mtof() return self:map(mtof) end

	--- Convert from frequency to closest MIDI note.
	-- @number freq Arbitrary frequency.
	-- @treturn int Closest MIDI note number.
	-- @see Stream:ftom
	-- @see mtof
	function ftom(freq)
		-- NOTE: math.log/2 is a LuaJIT extension
		return floor(12*log(freq/440, 2) + 0.5)+69
	end

	--- Convert stream of frequencies to closest MIDI note numbers.
	-- @within Class Stream
	-- @treturn Stream
	-- @see ftom
	function Stream:ftom() return self:map(ftom) end
end

--- Convert from MIDI name to frequency.
-- @string name MIDI name (case-insensitive).
-- @treturn number Frequency
-- @see Stream:ntof
-- @see ntom
-- @see mtof
function ntof(name) return mtof(ntom(name)) end

--- Convert stream of MIDI names to frequencies.
-- @within Class Stream
-- @treturn Stream
-- @see ntof
-- @usage =tostream{"A4", "B4", "C4"}:ntof()
function Stream:ntof() return self:map(ntof) end

--- Convert from frequency to closest MIDI note name.
-- @number freq Frequency
-- @treturn string The all-upper-case MIDI note name.
-- @see Stream:fton
-- @see ftom
-- @see mton
function fton(freq) return mton(ftom(freq)) end

--- Convert stream of frequencies to closest MIDI note names.
-- @within Class Stream
-- @treturn Stream
-- @see fton
function Stream:fton() return self:map(fton) end

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

--- Tick instrument streams based on an input stream.
-- Once the input stream is unequal to 0, the "on"-stream is activated.
-- When it changes back to 0 again, the "off"-stream gets triggered.
-- This allows the construction of instruments with
-- Attack-Sustain and Decay phases based on real-time control signals.
-- Usually, the instrument stream will be applied on @{Stream:mvelocity}, so the two
-- instrument streams can be based on the MIDI velocity (but don't have
-- to be if the velocity is not important).
-- @within Class Stream
-- @tparam StreamableNumber|func on_stream
--   Stream to trigger when the input value becomes unequal to 0.
--   If this is a function, it will be called with the input value (usually a MIDI velocity)
--   to generate the "on"-stream.
-- @tparam[opt] StreamableNumber|func off_stream
--   Stream to trigger when the input value becomes 0.
--   If this is a function, it will be called with the previous input value (usually a MIDI velocity)
--   to generate the "off"-stream.
-- @treturn Stream
-- @see Stream:mvelocity
-- @usage MIDIStream:mvelocity("C4"):instrument(Stream.SinOsc(ntof("C4"))):play()
function Stream:instrument(on_stream, off_stream)
	return InstrumentStream:new(self, on_stream, off_stream)
end
