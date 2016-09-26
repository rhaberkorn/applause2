local bit = require "bit"
local ffi = require "ffi"

-- ladspa.h/dssi.h extracts.
-- Comments have been removed for simplicity and defines
-- have been converted into enums.
cdef_safe[[
/*
 * From ladspa.h (Version 1.1)
 */

typedef float LADSPA_Data;

typedef int LADSPA_Properties;

typedef int LADSPA_PortDescriptor;

enum {
	LADSPA_PORT_INPUT		= 0x1,
	LADSPA_PORT_OUTPUT		= 0x2,
	LADSPA_PORT_CONTROL		= 0x4,
	LADSPA_PORT_AUDIO		= 0x8
};

typedef int LADSPA_PortRangeHintDescriptor;

enum {
	LADSPA_HINT_BOUNDED_BELOW	= 0x1,
	LADSPA_HINT_BOUNDED_ABOVE	= 0x2,
	LADSPA_HINT_TOGGLED		= 0x4,
	LADSPA_HINT_SAMPLE_RATE		= 0x8,
	LADSPA_HINT_LOGARITHMIC		= 0x10,
	LADSPA_HINT_INTEGER		= 0x20,
	LADSPA_HINT_DEFAULT_MASK	= 0x3C0,
	LADSPA_HINT_DEFAULT_NONE	= 0x0,
	LADSPA_HINT_DEFAULT_MINIMUM	= 0x40,
	LADSPA_HINT_DEFAULT_LOW		= 0x80,
	LADSPA_HINT_DEFAULT_MIDDLE	= 0xC0,
	LADSPA_HINT_DEFAULT_HIGH	= 0x100,
	LADSPA_HINT_DEFAULT_MAXIMUM	= 0x140,
	LADSPA_HINT_DEFAULT_0		= 0x200,
	LADSPA_HINT_DEFAULT_1		= 0x240,
	LADSPA_HINT_DEFAULT_100		= 0x280,
	LADSPA_HINT_DEFAULT_440		= 0x2C0
};

typedef struct _LADSPA_PortRangeHint {
  LADSPA_PortRangeHintDescriptor HintDescriptor;
  LADSPA_Data LowerBound;
  LADSPA_Data UpperBound;
} LADSPA_PortRangeHint;

typedef void * LADSPA_Handle;

typedef struct _LADSPA_Descriptor { 
  unsigned long UniqueID;

  const char * Label;

  LADSPA_Properties Properties;

  const char * Name;
  const char * Maker;
  const char * Copyright;

  unsigned long PortCount;
  const LADSPA_PortDescriptor * PortDescriptors;
  const char * const * PortNames;
  const LADSPA_PortRangeHint * PortRangeHints;

  void * ImplementationData;

  LADSPA_Handle (*instantiate)(const struct _LADSPA_Descriptor * Descriptor,
                               unsigned long                     SampleRate);

   void (*connect_port)(LADSPA_Handle Instance,
                        unsigned long Port,
                        LADSPA_Data * DataLocation);

  void (*activate)(LADSPA_Handle Instance);

  void (*run)(LADSPA_Handle Instance,
              unsigned long SampleCount);
  void (*run_adding)(LADSPA_Handle Instance,
                     unsigned long SampleCount);
  void (*set_run_adding_gain)(LADSPA_Handle Instance,
                              LADSPA_Data   Gain);

  void (*deactivate)(LADSPA_Handle Instance);
  void (*cleanup)(LADSPA_Handle Instance);
} LADSPA_Descriptor;

const LADSPA_Descriptor *ladspa_descriptor(unsigned long Index);

/*
 * From dssi.h (Version 1.0)
 */

/*
 * Dummy type for snd_seq_event_t.
 * Originally defined in alsa/seq_event.h.
 * We don't need it since we do not support delivery of MIDI events.
 */
typedef void snd_seq_event_t;

typedef struct _DSSI_Program_Descriptor {
    unsigned long Bank;
    unsigned long Program;
    const char * Name;
} DSSI_Program_Descriptor;

typedef struct _DSSI_Descriptor {
    int DSSI_API_Version;

    const LADSPA_Descriptor *LADSPA_Plugin;

    char *(*configure)(LADSPA_Handle Instance,
		       const char *Key,
		       const char *Value);

    const DSSI_Program_Descriptor *(*get_program)(LADSPA_Handle Instance,
						  unsigned long Index);
    void (*select_program)(LADSPA_Handle Instance,
			   unsigned long Bank,
			   unsigned long Program);

    int (*get_midi_controller_for_port)(LADSPA_Handle Instance,
					unsigned long Port);

    void (*run_synth)(LADSPA_Handle    Instance,
		      unsigned long    SampleCount,
		      snd_seq_event_t *Events,
		      unsigned long    EventCount);
    void (*run_synth_adding)(LADSPA_Handle    Instance,
			     unsigned long    SampleCount,
			     snd_seq_event_t *Events,
			     unsigned long    EventCount);
    void (*run_multiple_synths)(unsigned long     InstanceCount,
                                LADSPA_Handle    *Instances,
                                unsigned long     SampleCount,
                                snd_seq_event_t **Events,
                                unsigned long    *EventCounts);
    void (*run_multiple_synths_adding)(unsigned long     InstanceCount,
                                       LADSPA_Handle    *Instances,
                                       unsigned long     SampleCount,
                                       snd_seq_event_t **Events,
                                       unsigned long    *EventCounts);
} DSSI_Descriptor;

const DSSI_Descriptor *dssi_descriptor(unsigned long Index);
]]

-- Check for symbol existence in C library table.
-- There does not seem to be a more elegant way to do this.
local function checkClibSymbol(lib, symbol)
	return pcall(getmetatable(lib).__index, lib, symbol) == true
end

-- NOTE: Not a LADSPAStream method, so we can call it from ctor()
local function getPortDefault(hint)
	local default = bit.band(hint.HintDescriptor,
	                         ffi.C.LADSPA_HINT_DEFAULT_MASK)

	-- This map contains only the simple defaults since we want
	-- to avoid more complex calculations if possible
	local default_to_value = {
		[ffi.C.LADSPA_HINT_DEFAULT_MINIMUM]	= hint.LowerBound,
		[ffi.C.LADSPA_HINT_DEFAULT_MAXIMUM]	= hint.UpperBound,
		[ffi.C.LADSPA_HINT_DEFAULT_0]		= 0,
		[ffi.C.LADSPA_HINT_DEFAULT_1]		= 1,
		[ffi.C.LADSPA_HINT_DEFAULT_100]		= 100,
		[ffi.C.LADSPA_HINT_DEFAULT_440]		= 440
	}

	local value = default_to_value[default]
	if value then return value end

	-- Could still be a value dependent on LowerBound/UpperBound...

	local logarithmic = bit.band(hint.HintDescriptor,
	                             ffi.C.LADSPA_HINT_LOGARITHMIC) ~= 0

	if default == ffi.C.LADSPA_HINT_DEFAULT_LOW then
		return logarithmic and
		       math.exp(math.log(hint.LowerBound)*0.75 + math.log(hint.UpperBound)*0.25) or
		       hint.LowerBound*0.75 + hint.UpperBound*0.25
	elseif default == ffi.C.LADSPA_HINT_DEFAULT_MIDDLE then
		return logarithmic and
		       math.exp(math.log(hint.LowerBound)*0.5 + math.log(hint.UpperBound)*0.5) or
		       hint.LowerBound*0.5 + hint.UpperBound*0.5
	elseif default == ffi.C.LADSPA_HINT_DEFAULT_HIGH then
		return logarithmic and
		       math.exp(math.log(hint.LowerBound)*0.25 + math.log(hint.UpperBound)*0.75) or
		       hint.LowerBound*0.25 + hint.UpperBound*0.75
	end

	-- We can still return nil, if there is no default value
	-- (or an unknown default)
end

local function mangleInputPorts(input_ports, ...)
	-- NOTE: Theoretically, an array can be converted to a
	-- stream like any other type and used to provide the
	-- first plugin input port.
	-- We prevent this (at least for the first stream) to allow
	-- passing in all input ports in a single table argument.
	if type(input_ports) ~= "table" or
	   input_ports.is_a_stream then
		input_ports = {input_ports}
	end
	for _, stream in ipairs{...} do
		table.insert(input_ports, stream)
	end

	return input_ports
end

LADSPAStream = DeriveClass(Stream)

-- `file` is either the full path to a plugin library or a basename
-- looked up in $DSSI_PATH and $LADSPA_PATH.
-- It may be followed by an optional ":Label" to select a plugin by type
-- from this file (otherwise, the first one is used).
--
-- `input_ports` are tables defining the mapping from
-- LADSPA port names to Streams or constants for audio and control input ports.
-- This host does not make a difference between audio and control ports.
-- A mapping from port Id to Streams (ie. an array of Streams corresponding
-- with the ports) is also allowed.
-- All additional arguments are added to this table as an array,
-- so port mappings can be specified as a list of arguments as well.
-- Every plugin input port must either be mapped or have a default
-- value.
-- Constants are handled specially and are faster than streams.
-- Multi-channel input streams do not result in muxing of the LADSPAStream,
-- so every input stream must be mono.
-- However, to ease binding the individual channels of a multi-channel
-- stream, they are automatically expanded to consecutive input streams.
--
-- Multi-channel output plugins are always muxed. But you may use
-- LADSPAStream(...):demux(...) to discard uninteresting output channels.
--
-- There is some limited support for DSSI plugins.
-- Currently, DSSI plugins are simply handled like wrappers around
-- LADSPA plugins, which should work for DSSI-based effects that
-- do not expose a LADSPA entry point.
-- However to trigger a soft-synth, some kind of MIDI event delivery
-- appears to be necessary.
-- FIXME: This may be resolved by adding support for a special
-- MIDI event input stream that, e.g. raw MIDI commands, which are
-- parsed into snd_seq_event_t's.
--
-- FIXME: We could simplify things by just assuming a flat array of
-- input ports
function LADSPAStream:ctor(file, ...)
	local plugin_file, label = file:match("^([^:]+):(.+)")
	plugin_file = plugin_file or file

	local input_ports = mangleInputPorts(...)

	-- Expand all multi-channel input streams in arrays.
	-- This is handy, since often stereo inputs are defined
	-- as consecutive input ports.
	do
		local i = 1
		while i <= table.maxn(input_ports) do
			local port = input_ports[i]

			if type(port) == "table" and port.is_a_stream and
			   port.channels > 1 then
				table.remove(input_ports, i)
				for c = port.channels, 1, -1 do
					table.insert(input_ports, i, port:demux(c))
				end
			end

			i = i + 1
		end
	end

	-- NOTE: The FFI clib is saved in the object
	-- to keep it alive even though we call only function pointers
	-- after the constructor.
	if plugin_file:sub(1,1) == "/" then
		-- Absolute path
		self.lib = ffi.load(plugin_file)
	else
		-- Search in $DSSI_PATH:$LADSPA_PATH
		local DSSI_PATH = os.getenv("DSSI_PATH") or
		                  "/usr/local/lib/dssi:/usr/lib/dssi"
		local LADSPA_PATH = os.getenv("LADSPA_PATH") or
		                    "/usr/local/lib/ladspa:/usr/lib/ladspa"

		for dir in string.gmatch(DSSI_PATH..":"..LADSPA_PATH, "[^:]+") do
			if dir:sub(-1) ~= "/" then dir = dir.."/" end

			-- Simply try to load the plugin library in this
			-- directory. We have no standard way of
			-- checking for file existence anyway.
			local state, lib = pcall(ffi.load, dir..plugin_file..".so")

			-- If it could be loaded, still make sure it is a DSSI/LADSPA
			-- library
			if state and
			   (checkClibSymbol(lib, "dssi_descriptor") or
			    checkClibSymbol(lib, "ladspa_descriptor")) then
				self.lib = lib
				break
			end
		end

		if not self.lib then
			error('LADSPA plugin library "'..plugin_file..'" not found')
		end
	end

	-- Look up plugin by label or just take the first one
	do
		local i = 0
		repeat
			-- Look for a DSSI entry point
			if checkClibSymbol(self.lib, "dssi_descriptor") then
				local dssi_descriptor = self.lib.dssi_descriptor(i)
				if dssi_descriptor ~= nil then
					self.descriptor = dssi_descriptor.LADSPA_Plugin
				end
			else
				-- Otherwise, we are guaranteed to have a LADSPA entry point
				self.descriptor = self.lib.ladspa_descriptor(i)
			end

			if self.descriptor == nil then
				error('No matching plugin found for "'..file..'"')
			end

			i = i + 1
		until not label or ffi.string(self.descriptor.Label) == label
	end

	-- Array of all input port numbers (origin 0)
	-- with a corresponding Stream
	self.input_ports = {}
	-- Array of streams connected to the input ports.
	-- Each element corresponds with an port number in self.input_ports
	self.input_streams = {}

	-- Array of input port numbers with constant values.
	self.const_input_ports = {}
	-- Array of constants connected to the `const_input_ports`.
	local const_input_data = {}

	-- List of output port numbers (origin 0)
	self.output_ports = {}

	local input_port_count = 0
	for i = 0, tonumber(self.descriptor.PortCount)-1 do
		local port_descriptor = self.descriptor.PortDescriptors[i]

		if bit.band(port_descriptor, ffi.C.LADSPA_PORT_INPUT) ~= 0 then
			input_port_count = input_port_count + 1

			local port_name = ffi.string(self.descriptor.PortNames[i])

			-- We must connect all ports, so if the user does not provide
			-- an input stream or constant, we try to provide a default.
			-- There may be no default, in which case we throw an error.
			local data = input_ports[input_port_count] or input_ports[port_name] or
			             getPortDefault(self.descriptor.PortRangeHints[i]) or
			             error('Input stream/constant for port "'..port_name..'" in plugin '..
			                   '"'..file..'" required')

			if type(data) == "table" and data.is_a_stream then
				-- Every LADSPA port is single channel, so for the time being
				-- we allow only single channel input Streams
				assert(data.channels == 1)
				-- Since LADSPA plugins can always produce data infinitely,
				-- the LADSPAStream is infinite as well.
				-- To avoid problems with input streams ending early,
				-- we enforce them to be infinite as well.
				-- FIXME: Perhaps LADSPAStream should be bounded to
				-- the shortest input stream.
				assert(data:len() == math.huge)

				table.insert(self.input_ports, i)
				table.insert(self.input_streams, data)
			else
				table.insert(self.const_input_ports, i)
				table.insert(const_input_data, data)
			end
		elseif bit.band(port_descriptor, ffi.C.LADSPA_PORT_OUTPUT) ~= 0 then
			table.insert(self.output_ports, i)
		end
	end
	assert(#self.output_ports > 0)

	-- Constant input data can be converted to LADSPA_Data array and shared
	-- among all instances of this Stream.
	self.const_input_buffers = ffi.new("LADSPA_Data[?]", #const_input_data,
	                                   unpack(const_input_data))

	-- Just like in SndfileStreams, plugins with multiple output channels
	-- must be wrapped in a MuxStream
	if #self.output_ports > 1 then
		local cached = self:cache()
		local streams = {}
		for i = 0, #self.output_ports-1 do
			streams[i+1] = cached:map(function(frame)
				return tonumber(frame[i])
			end)
		end
		return MuxStream:new(unpack(streams))
	end
end

function LADSPAStream:getName()
	return ffi.string(self.descriptor.Name)
end

function LADSPAStream:gtick()
	-- Get the tick for every (non-constant) input port stream
	local ticks = table.new(#self.input_streams, 0)
	for i = 1, #self.input_streams do
		ticks[i] = self.input_streams[i]:gtick()
	end

	-- Every input and output port has its own
	-- one-sample buffer, so we simply allocate a consecutive
	-- array of LADSPA_Data.
	local input_buffers = ffi.new("LADSPA_Data[?]", #self.input_ports)
	-- For output buffers, this also has the advantage that they can
	-- be returned like an output frame.
	local output_buffers = ffi.new("LADSPA_Data[?]", #self.output_ports)
	-- If true, we output frames (multi-channel output)
	local output_frames = #self.output_ports > 1

	-- The deactivate() handler, if it exists and must be called
	-- before cleanup.
	local deactivate

	-- Instantiate plugin. This may fail.
	-- It is done in gtick(), so the stream can be reused multiple
	-- times.
	local handle = self.descriptor:instantiate(samplerate)
	if handle == nil then
		error('Instantiating LADSPA plugin "'..self:getName()..'" failed')
	end
	handle = ffi.gc(handle, function(handle)
		-- Make sure that deactivate() is called, but only
		-- after activate(). ladspa.h is unclear whether
		-- deactivate() can be called without activate().
		if deactivate then deactivate(handle) end
		self.descriptor.cleanup(handle)

		-- This makes sure that the buffers are only garbage
		-- collected AFTER cleanup().
		input_buffers, output_buffers = nil, nil
	end)

	-- Connect all non-constant input ports
	for i = 1, #self.input_ports do
		self.descriptor.connect_port(handle, self.input_ports[i],
		                             input_buffers + i - 1)
	end

	-- Connect all constant input ports
	for i = 1, #self.const_input_ports do
		self.descriptor.connect_port(handle, self.const_input_ports[i],
		                             self.const_input_buffers + i - 1)
	end

	-- Connect output ports
	for i = 1, #self.output_ports do
		self.descriptor.connect_port(handle, self.output_ports[i],
		                             output_buffers + i - 1)
	end

	local run = self.descriptor.run

	-- Activate plugin.
	-- It should be safe to do here instead of in the tick function.
	if self.descriptor.activate ~= nil then
		deactivate = self.descriptor.deactivate ~= nil and
		             self.descriptor.deactivate
		self.descriptor.activate(handle)
	end

	return function()
		-- Fill each input buffer.
		-- NOTE: Constants have their own buffers and are initialized
		-- only once in ctor().
		-- NOTE: Currently every input stream is guaranteed to be
		-- infinite.
		for i = 1, #ticks do
			input_buffers[i-1] = ticks[i]()
		end

		-- Run for 1 sample
		run(handle, 1)

		-- For multi-channel output plugins, we return frames.
		-- This is an intermediate output that the user never sees
		-- since it is wrapped in a MuxStream (see ctor()).
		return output_frames and output_buffers or
		       tonumber(output_buffers[0])
	end
end

-- LADSPA plugins always seem to be infinite,
-- and we force all input streams to be infinite as well
function LADSPAStream:len() return math.huge end

-- For the Stream method, we just assume that the
-- subject stream is passed in as the first input port
function Stream:LADSPA(file, ...)
	local input_ports = mangleInputPorts(...)
	table.insert(input_ports, 1, self)

	return LADSPAStream:new(file, input_ports)
end
