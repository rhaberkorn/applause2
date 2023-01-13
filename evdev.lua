local ffi = require "ffi"
local C = ffi.C

cdef_include "evdev.h"
cdef_safe[[
/*
 * These definitions are copied from input-event-codes.h
 */
enum applause_evdev_type {
	EV_KEY			= 0x01,
	EV_REL			= 0x02,
	EV_ABS			= 0x03
};

enum applause_evdev_rel {
	REL_X			= 0x00,
	REL_Y			= 0x01,
	REL_Z			= 0x02,
	REL_RX			= 0x03,
	REL_RY			= 0x04,
	REL_RZ			= 0x05,
	REL_HWHEEL		= 0x06,
	REL_DIAL		= 0x07,
	REL_WHEEL		= 0x08,
	REL_MISC		= 0x09,
	REL_WHEEL_HI_RES	= 0x0b,
	REL_HWHEEL_HI_RES	= 0x0c
};

enum applause_evdev_abs {
	ABS_X			= 0x00,
	ABS_Y			= 0x01,
	ABS_Z			= 0x02
};
]]

EvdevStream = DeriveClass(Stream)

function EvdevStream:ctor(id, grab)
	local grab = grab == nil or grab

	local node
	if type(id) == "number" then
		node = "/dev/input/event"..id
	else
		assert(type(id) == "string")
		-- id is assumed to be a Lua pattern to match against the device name
		local name
		local i = 0
		repeat
			node = "/dev/input/event"..i
			local buffer = ffi.gc(C.applause_evdev_getname(node), C.free)
			if buffer == nil then error("Evdev device not found!") end
			name = ffi.string(buffer)
			i = i + 1
		until name:match(id)
	end

	-- Creating only one object has the advantage that the device can be
	-- grabbed.
	-- NOTE: To reliably ungrab the device, the entire object needs to be niled
	-- and you have to drive the garbage collector manually.
	self.evdev = ffi.gc(C.applause_evdev_new(node, grab), C.applause_evdev_free)
	if self.evdev == nil then error("Evdev device not found!") end
end

function EvdevStream:gtick()
	local evdev = self.evdev
	local sample = ffi.new("applause_evdev_sample[1]")

	return function()
		-- EvdevStreams only have a single event queue no matter how often they are
		-- gticked. That's why it must always be cached.
		local cached_sample = sampleCache[self]
		if not cached_sample then
			C.applause_evdev_pull(evdev, sample)
			sampleCache[self] = sample[0]
			cached_sample = sample[0]
		end
		return cached_sample
	end
end

-- Relative devices like some mouses and the trackpoint.
-- The coordinate is returned in `resolution` steps between [-1, 1]
-- NOTE: `code` is optional (default: REL_X) and you can specify a number (0 or 1)
-- or string ('REL_X', 'REL_Y') as well.
function Stream:evrel(code, resolution)
	code = ffi.cast("enum applause_evdev_rel", code)
	resolution = resolution or 1000
	local min, max = math.min, math.max

	return self:scan(function(last, sample)
		last = last or 0
		return sample.type == C.EV_REL and sample.code == code and
		       min(max(last+sample.value, 0), resolution) or last
	end) / (resolution/2) - 1
end

-- FIXME: min and max can be read from the properties of the corresponding code.
-- This would however require us to do all postprocessing already in EvdevStream:gtick()
-- or even in applause_evdev_new().
function Stream:evabs(code, min, max)
	code = ffi.cast("enum applause_evdev_abs", code)

	return self:scan(function(last, sample)
		last = last or 0
		return sample.type == C.EV_ABS and sample.code == code and
		       (sample.value - min)/((max-min)/2) - 1 or last
	end)
end

-- FIXME: We haven't got constants for all KEY_X constants,#
-- so you will have to look up the key code in input-event-codes.h.
function Stream:evkey(key)
	return self:scan(function(last, sample)
		last = last or 0
		return sample.type == C.EV_KEY and sample.code == key and
		       sample.value or last
	end)
end
