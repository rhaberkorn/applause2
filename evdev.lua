---
--- @module applause
---
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
	ABS_Z			= 0x02,
	ABS_RX			= 0x03,
	ABS_RY			= 0x04,
	ABS_RZ			= 0x05,
	ABS_THROTTLE		= 0x06,
	ABS_RUDDER		= 0x07,
	ABS_WHEEL		= 0x08,
	ABS_GAS			= 0x09,
	ABS_BRAKE		= 0x0a,
	ABS_HAT0X		= 0x10,
	ABS_HAT0Y		= 0x11,
	ABS_HAT1X		= 0x12,
	ABS_HAT1Y		= 0x13,
	ABS_HAT2X		= 0x14,
	ABS_HAT2Y		= 0x15,
	ABS_HAT3X		= 0x16,
	ABS_HAT3Y		= 0x17,
	ABS_PRESSURE		= 0x18,
	ABS_DISTANCE		= 0x19,
	ABS_TILT_X		= 0x1a,
	ABS_TILT_Y		= 0x1b,
	ABS_TOOL_WIDTH		= 0x1c,
	ABS_VOLUME		= 0x20,
	ABS_MISC		= 0x28
};

/*
 * From dirent.h
 */
typedef struct __dirstream DIR;

DIR *opendir(const char *name);
struct dirent *readdir(DIR *dirp);
int closedir(DIR *dirp);
]]

--- Stream of Evdev (HID device) events.
-- This allows using ordinary keyboards, trackpads, trackpoints, mice etc. as
-- real-time controllers.
-- See also [input-event-codes.h](https://raw.githubusercontent.com/torvalds/linux/master/include/uapi/linux/input-event-codes.h)
-- for useful constants.
-- @type EvdevStream
-- @see MIDIStream
EvdevStream = DeriveClass(Stream)

--- List all HID devices on stdout.
-- This is an alternative to `evtest` and displays node id and device names,
-- that can be used with @{EvdevStream:new}.
function EvdevStream.list()
	local dir = ffi.gc(C.opendir("/dev/input"), C.closedir)
	while true do
		local entry = C.readdir(dir)
		if entry == nil then break end
		local filename = ffi.string(C.applause_dirent_name(entry))
		local devname = ffi.gc(C.applause_evdev_getname("/dev/input/"..filename), C.free)
		if devname ~= nil then
			print(filename:match("%d+$")..": "..ffi.string(devname))
		end
	end
end

--- Create EvdevStream (stream HID device events).
-- @function new
-- @tparam int|string id
--   Either an integer referring to the device node (`/dev/input/eventX`) or
--   a Lua pattern matched against the device names.
--   The first matching device is used.
-- @bool[opt=true] grab
--   Whether to "grab" the device.
--   A "grabbed" device will get its events delivered exclusively to Applause,
--   so it will not interfer with the operating system
--   (pressed keyboard keys or mouse movement will not cause any trouble).
--   Consequently there can only be one EvdevStream per grabbed device.
--   The device will be "ungrabbed" once the object is garbage collected.
-- @treturn EvdevStream
--   A stream of C structures, describing the event in detail.
--   Its fields will be 0 if no event ocurred.
-- @see Stream:evrel
-- @see Stream:evabs
-- @see Stream:evkey
-- @todo Document the C structure as a Lua table.
function EvdevStream:ctor(id, grab)
	local grab = grab == nil or grab

	local node
	if type(id) == "number" then
		node = "/dev/input/event"..id
	else
		assert(type(id) == "string")
		-- id is assumed to be a Lua pattern to match against the device name
		local dir = ffi.gc(C.opendir("/dev/input"), C.closedir)
		while true do
			local entry = C.readdir(dir)
			if entry == nil then error("Evdev device not found!") end
			node = "/dev/input/"..ffi.string(C.applause_dirent_name(entry))
			local name = ffi.gc(C.applause_evdev_getname(node), C.free)
			if name ~= nil and ffi.string(name):match(id) then break end
		end
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

--- Filter Evdev event stream to get the last value of a device with relative positioning (EV_REL).
-- This can be used to retrieve the position of mice for instance.
-- The relative movements are automatically converted to absolute positions given a resolution and
-- the stream "holds" the last value.
-- It is usally applied on streams returned by @{EvdevStream:new}.
-- @within Class Stream
-- @tparam[opt='REL_X'] applause_evdev_rel|int|string code
--   This is a C value, integer or string ('REL_X', 'REL_Y'...), specifying the axis to extract.
--   The possible values correspond to the C header `input-event-codes.h`.
-- @number[opt=1000] resolution
--   The device resolution, ie. the number of steps between [-1,+1].
--   The larger this value, the longer it takes to move from the minimum to the maximum position.
-- @treturn Stream Stream of numbers between [-1,+1].
-- @see EvdevStream:new
-- @see Stream:scale
-- @usage EvdevStream("TrackPoint"):evrel():scale(440,880):SinOsc():play()
function Stream:evrel(code, resolution)
	code = ffi.cast("enum applause_evdev_rel", code)
	resolution = (resolution or 1000)/2

	local min, max = math.min, math.max

	return self:scan(function(last, sample)
		last = last or -1
		return sample.type == C.EV_REL and sample.code == code and
		       min(max(last+tonumber(sample.value)/resolution, -1), 1) or last
	end)
end

--- Filter Evdev event stream to get the last value of a device with absolute positioning (EV_ABS).
-- This can be used to retrieve the position of touchpads for instance.
-- @within Class Stream
-- @tparam[opt='ABS_X'] applause_evdev_abs|int|string code
--   This is a C value, integer or string ('ABS_X', 'ABS_Y'...), specifying the axis to extract.
--   The possible values correspond to the C header `input-event-codes.h`.
-- @int min
--   Minimum value (absolute position).
--   This will currently have to be looked up manually using `evtest`.
-- @int max
--   Maximum value (absolute position).
--   This will currently have to be looked up manually using `evtest`.
-- @treturn Stream Stream of numbers between [-1,1].
-- @see EvdevStream:new
-- @see Stream:scale
-- @usage EvdevStream("TouchPad"):evabs('ABS_X', 1232, 5712):scale(440,880):SinOsc():play()
-- @fixme min and max can be read from the properties of the corresponding code.
-- This would however require us to do all postprocessing already in EvdevStream:gtick()
-- or even in applause_evdev_new().
-- Or simply overwrite this function in EvdevStream as an optimization.
function Stream:evabs(code, min, max)
	code = ffi.cast("enum applause_evdev_abs", code)

	return self:scan(function(last, sample)
		last = last or 0
		return sample.type == C.EV_ABS and sample.code == code and
		       tonumber((sample.value - min)*2)/(max-min) - 1 or last
	end)
end

--- Filter Evdev event stream to get the last key code (EV_KEY).
-- This can be used to turn PC keyboards into controllers.
-- The key code will be generated as long as the key is pressed,
-- otherwise it will be 0.
-- @within Class Stream
-- @int key
--   The key code to extract.
--   This must currently always be an integer, corresponding to the
--   `KEY_X` constants from `input-event-codes.h`.
-- @treturn Stream Stream of key codes or 0 if the key is not currently pressed.
-- @see EvdevStream:new
-- @usage EvdevStream(10):evkey(16):instrument(Stream.SinOsc(440)):play()
function Stream:evkey(key)
	return self:scan(function(last, sample)
		last = last or 0
		return sample.type == C.EV_KEY and sample.code == key and
		       (sample.value ~= 0 and key or 0) or last
	end)
end
