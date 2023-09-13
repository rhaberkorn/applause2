---
--- @module applause
---
local bit = require "bit"
local ffi = require "ffi"
local C = ffi.C

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
 * From alsa/seq_event.h.
 * This is practically the entire header but we have to reproduce it
 * to replace macros with enums.
 */

typedef unsigned char snd_seq_event_type_t;

/** Sequencer event type */
enum snd_seq_event_type {
	/** system status; event data type = #snd_seq_result_t */
	SND_SEQ_EVENT_SYSTEM = 0,
	/** returned result status; event data type = #snd_seq_result_t */
	SND_SEQ_EVENT_RESULT,

	/** note on and off with duration; event data type = #snd_seq_ev_note_t */
	SND_SEQ_EVENT_NOTE = 5,
	/** note on; event data type = #snd_seq_ev_note_t */
	SND_SEQ_EVENT_NOTEON,
	/** note off; event data type = #snd_seq_ev_note_t */
	SND_SEQ_EVENT_NOTEOFF,
	/** key pressure change (aftertouch); event data type = #snd_seq_ev_note_t */
	SND_SEQ_EVENT_KEYPRESS,

	/** controller; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_CONTROLLER = 10,
	/** program change; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_PGMCHANGE,
	/** channel pressure; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_CHANPRESS,
	/** pitchwheel; event data type = #snd_seq_ev_ctrl_t; data is from -8192 to 8191) */
	SND_SEQ_EVENT_PITCHBEND,
	/** 14 bit controller value; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_CONTROL14,
	/** 14 bit NRPN;  event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_NONREGPARAM,
	/** 14 bit RPN; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_REGPARAM,

	/** SPP with LSB and MSB values; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_SONGPOS = 20,
	/** Song Select with song ID number; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_SONGSEL,
	/** midi time code quarter frame; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_QFRAME,
	/** SMF Time Signature event; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_TIMESIGN,
	/** SMF Key Signature event; event data type = #snd_seq_ev_ctrl_t */
	SND_SEQ_EVENT_KEYSIGN,

	/** MIDI Real Time Start message; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_START = 30,
	/** MIDI Real Time Continue message; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_CONTINUE,
	/** MIDI Real Time Stop message; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_STOP,
	/** Set tick queue position; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_SETPOS_TICK,
	/** Set real-time queue position; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_SETPOS_TIME,
	/** (SMF) Tempo event; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_TEMPO,
	/** MIDI Real Time Clock message; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_CLOCK,
	/** MIDI Real Time Tick message; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_TICK,
	/** Queue timer skew; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_QUEUE_SKEW,
	/** Sync position changed; event data type = #snd_seq_ev_queue_control_t */
	SND_SEQ_EVENT_SYNC_POS,

	/** Tune request; event data type = none */
	SND_SEQ_EVENT_TUNE_REQUEST = 40,
	/** Reset to power-on state; event data type = none */
	SND_SEQ_EVENT_RESET,
	/** Active sensing event; event data type = none */
	SND_SEQ_EVENT_SENSING,

	/** Echo-back event; event data type = any type */
	SND_SEQ_EVENT_ECHO = 50,
	/** OSS emulation raw event; event data type = any type */
	SND_SEQ_EVENT_OSS,

	/** New client has connected; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_CLIENT_START = 60,
	/** Client has left the system; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_CLIENT_EXIT,
	/** Client status/info has changed; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_CLIENT_CHANGE,
	/** New port was created; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_PORT_START,
	/** Port was deleted from system; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_PORT_EXIT,
	/** Port status/info has changed; event data type = #snd_seq_addr_t */
	SND_SEQ_EVENT_PORT_CHANGE,

	/** Ports connected; event data type = #snd_seq_connect_t */
	SND_SEQ_EVENT_PORT_SUBSCRIBED,
	/** Ports disconnected; event data type = #snd_seq_connect_t */
	SND_SEQ_EVENT_PORT_UNSUBSCRIBED,

	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR0 = 90,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR1,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR2,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR3,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR4,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR5,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR6,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR7,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR8,
	/** user-defined event; event data type = any (fixed size) */
	SND_SEQ_EVENT_USR9,

	/** system exclusive data (variable length);  event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_SYSEX = 130,
	/** error event;  event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_BOUNCE,
	/** reserved for user apps;  event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_USR_VAR0 = 135,
	/** reserved for user apps; event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_USR_VAR1,
	/** reserved for user apps; event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_USR_VAR2,
	/** reserved for user apps; event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_USR_VAR3,
	/** reserved for user apps; event data type = #snd_seq_ev_ext_t */
	SND_SEQ_EVENT_USR_VAR4,

	/** NOP; ignored in any case */
	SND_SEQ_EVENT_NONE = 255
};

/** Sequencer event address */
typedef struct snd_seq_addr {
	unsigned char client;	/**< Client id */
	unsigned char port;	/**< Port id */
} snd_seq_addr_t;

/** Connection (subscription) between ports */
typedef struct snd_seq_connect {
	snd_seq_addr_t sender;	/**< sender address */
	snd_seq_addr_t dest;	/**< destination address */
} snd_seq_connect_t;


/** Real-time data record */
typedef struct snd_seq_real_time {
	unsigned int tv_sec;		/**< seconds */
	unsigned int tv_nsec;		/**< nanoseconds */
} snd_seq_real_time_t;

/** (MIDI) Tick-time data record */
typedef unsigned int snd_seq_tick_time_t;

/** unioned time stamp */
typedef union snd_seq_timestamp {
	snd_seq_tick_time_t tick;	/**< tick-time */
	struct snd_seq_real_time time;	/**< real-time */
} snd_seq_timestamp_t;

/**
 * Event mode flags
 *
 * NOTE: only 8 bits available!
 *
 * NOTE: These were #defines in the original
 * header.
 */
enum {
	SND_SEQ_TIME_STAMP_TICK		= (0<<0),	/**< timestamp in clock ticks */
	SND_SEQ_TIME_STAMP_REAL		= (1<<0),	/**< timestamp in real time */
	SND_SEQ_TIME_STAMP_MASK		= (1<<0),	/**< mask for timestamp bits */

	SND_SEQ_TIME_MODE_ABS		= (0<<1),	/**< absolute timestamp */
	SND_SEQ_TIME_MODE_REL		= (1<<1),	/**< relative to current time */
	SND_SEQ_TIME_MODE_MASK		= (1<<1),	/**< mask for time mode bits */

	SND_SEQ_EVENT_LENGTH_FIXED	= (0<<2),	/**< fixed event size */
	SND_SEQ_EVENT_LENGTH_VARIABLE	= (1<<2),	/**< variable event size */
	SND_SEQ_EVENT_LENGTH_VARUSR	= (2<<2),	/**< variable event size - user memory space */
	SND_SEQ_EVENT_LENGTH_MASK	= (3<<2),	/**< mask for event length bits */

	SND_SEQ_PRIORITY_NORMAL		= (0<<4),	/**< normal priority */
	SND_SEQ_PRIORITY_HIGH		= (1<<4),	/**< event should be processed before others */
	SND_SEQ_PRIORITY_MASK		= (1<<4)	/**< mask for priority bits */
};

/** Note event */
typedef struct snd_seq_ev_note {
	unsigned char channel;		/**< channel number */
	unsigned char note;		/**< note */
	unsigned char velocity;		/**< velocity */
	unsigned char off_velocity;	/**< note-off velocity; only for #SND_SEQ_EVENT_NOTE */
	unsigned int duration;		/**< duration until note-off; only for #SND_SEQ_EVENT_NOTE */
} snd_seq_ev_note_t;

/** Controller event */
typedef struct snd_seq_ev_ctrl {
	unsigned char channel;		/**< channel number */
	unsigned char unused[3];	/**< reserved */
	unsigned int param;		/**< control parameter */
	signed int value;		/**< control value */
} snd_seq_ev_ctrl_t;

/** generic set of bytes (12x8 bit) */
typedef struct snd_seq_ev_raw8 {
	unsigned char d[12];		/**< 8 bit value */
} snd_seq_ev_raw8_t;

/** generic set of integers (3x32 bit) */
typedef struct snd_seq_ev_raw32 {
	unsigned int d[3];		/**< 32 bit value */
} snd_seq_ev_raw32_t;

/** external stored data */
struct snd_seq_ev_ext {
	unsigned int len;		/**< length of data */
	void *ptr;			/**< pointer to data (note: can be 64-bit) */
} __attribute__((packed));
/** external stored data */
typedef struct snd_seq_ev_ext snd_seq_ev_ext_t;

/** Result events */
typedef struct snd_seq_result {
	int event;		/**< processed event type */
	int result;		/**< status */
} snd_seq_result_t;

/** Queue skew values */
typedef struct snd_seq_queue_skew {
	unsigned int value;	/**< skew value */
	unsigned int base;	/**< skew base */
} snd_seq_queue_skew_t;

/** queue timer control */
typedef struct snd_seq_ev_queue_control {
	unsigned char queue;			/**< affected queue */
	unsigned char unused[3];		/**< reserved */
	union {
		signed int value;		/**< affected value (e.g. tempo) */
		snd_seq_timestamp_t time;	/**< time */
		unsigned int position;		/**< sync position */
		snd_seq_queue_skew_t skew;	/**< queue skew */
		unsigned int d32[2];		/**< any data */
		unsigned char d8[8];		/**< any data */
	} param;				/**< data value union */
} snd_seq_ev_queue_control_t;

/** Sequencer event */
typedef struct snd_seq_event {
	snd_seq_event_type_t type;	/**< event type */
	unsigned char flags;		/**< event flags */
	unsigned char tag;		/**< tag */

	unsigned char queue;		/**< schedule queue */
	snd_seq_timestamp_t time;	/**< schedule time */

	snd_seq_addr_t source;		/**< source address */
	snd_seq_addr_t dest;		/**< destination address */

	union {
		snd_seq_ev_note_t note;		/**< note information */
		snd_seq_ev_ctrl_t control;	/**< MIDI control information */
		snd_seq_ev_raw8_t raw8;		/**< raw8 data */
		snd_seq_ev_raw32_t raw32;	/**< raw32 data */
		snd_seq_ev_ext_t ext;		/**< external data */
		snd_seq_ev_queue_control_t queue; /**< queue control */
		snd_seq_timestamp_t time;	/**< timestamp */
		snd_seq_addr_t addr;		/**< address */
		snd_seq_connect_t connect;	/**< connect information */
		snd_seq_result_t result;	/**< operation result code */
	} data;				/**< event data... */
} snd_seq_event_t;

/*
 * From alsa/seq_midi_event.h
 */

/** container for sequencer midi event parsers */
typedef struct snd_midi_event snd_midi_event_t;

int snd_midi_event_new(size_t bufsize, snd_midi_event_t **rdev);
void snd_midi_event_free(snd_midi_event_t *dev);
void snd_midi_event_reset_encode(snd_midi_event_t *dev);

int snd_midi_event_encode_byte(snd_midi_event_t *dev, int c, snd_seq_event_t *ev);

/*
 * From dssi.h (Version 1.0)
 */

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

local asound = ffi.load("asound")

--- Check for symbol existence in C library table.
-- There does not seem to be a more elegant way to do this.
local function checkClibSymbol(lib, symbol)
	return pcall(getmetatable(lib).__index, lib, symbol) == true
end

-- NOTE: Not a DSSIStream method, so we can call it from ctor()
local function getPortDefault(hint)
	local default = bit.band(hint.HintDescriptor,
	                         C.LADSPA_HINT_DEFAULT_MASK)

	-- This map contains only the simple defaults since we want
	-- to avoid more complex calculations if possible
	local default_to_value = {
		[C.LADSPA_HINT_DEFAULT_MINIMUM]	= hint.LowerBound,
		[C.LADSPA_HINT_DEFAULT_MAXIMUM]	= hint.UpperBound,
		[C.LADSPA_HINT_DEFAULT_0]	= 0,
		[C.LADSPA_HINT_DEFAULT_1]	= 1,
		[C.LADSPA_HINT_DEFAULT_100]	= 100,
		[C.LADSPA_HINT_DEFAULT_440]	= 440
	}

	local value = default_to_value[default]
	if value then return value end

	-- Could still be a value dependent on LowerBound/UpperBound...

	local logarithmic = bit.band(hint.HintDescriptor,
	                             C.LADSPA_HINT_LOGARITHMIC) ~= 0

	if default == C.LADSPA_HINT_DEFAULT_LOW then
		return logarithmic and
		       math.exp(math.log(hint.LowerBound)*0.75 + math.log(hint.UpperBound)*0.25) or
		       hint.LowerBound*0.75 + hint.UpperBound*0.25
	elseif default == C.LADSPA_HINT_DEFAULT_MIDDLE then
		return logarithmic and
		       math.exp(math.log(hint.LowerBound)*0.5 + math.log(hint.UpperBound)*0.5) or
		       hint.LowerBound*0.5 + hint.UpperBound*0.5
	elseif default == C.LADSPA_HINT_DEFAULT_HIGH then
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

--- Stream wrapper for external DSSI and LADSPA plugins.
-- @type DSSIStream
DSSIStream = DeriveClass(Stream)

--- Create a DSSI or LADSPA stream.
-- @function new
-- @string file
--   `file` is either the full path to a plugin library or a basename
--   looked up in $DSSI_PATH and $LADSPA_PATH.
--   It may be followed by an optional ":Label" to select a plugin by type
--   from this file (otherwise, the first one is used).
-- @Stream[opt] midi_event_stream
--   If the plugin is DSSI, this may be a @{MIDIStream}, but may also be nil.
--   For LADSPA plugins, this argument already specifies the first input port.
-- @tab[opt] input_ports
--   A table defining the mapping from
--   LADSPA port names to Streams or constants for audio and control input ports.
--   This host does not make a difference between audio and control ports.
--   A mapping from port Id to Streams (ie. an array of Streams corresponding
--   with the ports) is also allowed.
--   Every plugin input port must either be mapped or have a default value.
--   Constants are handled specially and are faster than streams.
--   Multi-channel input streams do not result in muxing of the DSSIStream,
--   so every input stream must be mono.
--   However, to ease binding the individual channels of a multi-channel
--   stream, they are automatically expanded to consecutive input streams.
-- @param ...
--   All additional arguments are added to this table as an array,
--   so port mappings can be specified as a list of arguments as well.
-- @treturn DSSIStream|MuxStream
--   Multi-channel output plugins are always muxed. But you may use
--   @{Stream:demux} to discard uninteresting output channels.
-- @see Stream:DSSI
--
-- @fixme We could simplify things by just assuming a flat array of
-- input ports.
function DSSIStream:ctor(file, midi_event_stream, ...)
	local plugin_file, label = file:match("^([^:]+):(.+)")
	plugin_file = plugin_file or file

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
			error('DSSI/LADSPA plugin library "'..plugin_file..'" not found')
		end
	end

	-- Look up plugin by label or just take the first one
	do
		local i = 0
		repeat
			-- Look for a DSSI entry point
			if checkClibSymbol(self.lib, "dssi_descriptor") then
				self.dssi_descriptor = self.lib.dssi_descriptor(i)
				if self.dssi_descriptor ~= nil then
					self.ladspa_descriptor = self.dssi_descriptor.LADSPA_Plugin
				end
			else
				-- Otherwise, we are guaranteed to have a LADSPA entry point
				self.ladspa_descriptor = self.lib.ladspa_descriptor(i)
			end

			if self.ladspa_descriptor == nil then
				error('No matching plugin found for "'..file..'"')
			end

			i = i + 1
		until not label or ffi.string(self.ladspa_descriptor.Label) == label
	end

	local input_ports

	if self.dssi_descriptor == nil then
		input_ports = mangleInputPorts(midi_event_stream, ...)
	else
		input_ports = mangleInputPorts(...)
		self.midi_event_stream = tostream(midi_event_stream)
	end

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
	for i = 0, tonumber(self.ladspa_descriptor.PortCount)-1 do
		local port_descriptor = self.ladspa_descriptor.PortDescriptors[i]

		if bit.band(port_descriptor, C.LADSPA_PORT_INPUT) ~= 0 then
			input_port_count = input_port_count + 1

			local port_name = ffi.string(self.ladspa_descriptor.PortNames[i])

			-- We must connect all ports, so if the user does not provide
			-- an input stream or constant, we try to provide a default.
			-- There may be no default, in which case we throw an error.
			local data = input_ports[input_port_count] or input_ports[port_name] or
			             getPortDefault(self.ladspa_descriptor.PortRangeHints[i]) or
			             error('Input stream/constant for port "'..port_name..'" in plugin '..
			                   '"'..file..'" required')

			if type(data) == "table" and data.is_a_stream then
				-- Every LADSPA port is single channel, so for the time being
				-- we allow only single channel input Streams
				assert(data.channels == 1)
				-- Since LADSPA plugins can always produce data infinitely,
				-- the DSSIStream is infinite as well.
				-- To avoid problems with input streams ending early,
				-- we enforce them to be infinite as well.
				-- FIXME: Perhaps DSSIStream should be bounded to
				-- the shortest input stream.
				assert(data:len() == math.huge)

				table.insert(self.input_ports, i)
				table.insert(self.input_streams, data)
			else
				table.insert(self.const_input_ports, i)
				table.insert(const_input_data, data)
			end
		elseif bit.band(port_descriptor, C.LADSPA_PORT_OUTPUT) ~= 0 then
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

--- Get name of DSSI/LADSPA plugin.
-- @treturn string
function DSSIStream:getName()
	return ffi.string(self.ladspa_descriptor.Name)
end

function DSSIStream:gtick()
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
	local handle = self.ladspa_descriptor:instantiate(samplerate)
	if handle == nil then
		error('Instantiating LADSPA plugin "'..self:getName()..'" failed')
	end
	handle = ffi.gc(handle, function(handle)
		-- Make sure that deactivate() is called, but only
		-- after activate(). ladspa.h is unclear whether
		-- deactivate() can be called without activate().
		if deactivate then deactivate(handle) end
		self.ladspa_descriptor.cleanup(handle)

		-- This makes sure that the buffers are only garbage
		-- collected AFTER cleanup().
		input_buffers, output_buffers = nil, nil
	end)

	-- Connect all non-constant input ports
	for i = 1, #self.input_ports do
		self.ladspa_descriptor.connect_port(handle, self.input_ports[i],
		                                    input_buffers + i - 1)
	end

	-- Connect all constant input ports
	for i = 1, #self.const_input_ports do
		self.ladspa_descriptor.connect_port(handle, self.const_input_ports[i],
		                                    self.const_input_buffers + i - 1)
	end

	-- Connect output ports
	for i = 1, #self.output_ports do
		self.ladspa_descriptor.connect_port(handle, self.output_ports[i],
		                                    output_buffers + i - 1)
	end

	local run

	if self.dssi_descriptor ~= nil and
	   self.dssi_descriptor.run_synth ~= nil then
		local run_synth = self.dssi_descriptor.run_synth
		local midi_event_tick = self.midi_event_stream:gtick()
		local seq_event = ffi.new("snd_seq_event_t[1]")
		local parser = ffi.new("snd_midi_event_t*[1]")

		-- NOTE: Every MIDI stream sample contains one
		-- MIDI message which is usually 2 or 3 bytes long.
		-- The first byte is the LSB.
		assert(asound.snd_midi_event_new(4, parser) == 0)

		parser = ffi.gc(parser[0], asound.snd_midi_event_free)

		local band, rshift = bit.band, bit.rshift

		function run(instance, sample_count)
			local midi_event = midi_event_tick()
			local seq_event_num = 0

			if midi_event ~= 0 then
				asound.snd_midi_event_reset_encode(parser)

				for i = 1, 4 do
					if asound.snd_midi_event_encode_byte(parser, band(midi_event, 0xFF),
					                                     seq_event) ~= 0 then
						seq_event_num = 1
						break
					end

					midi_event = rshift(midi_event, 8)
				end
			end

			run_synth(instance, sample_count, seq_event, seq_event_num)
		end
	else
		-- Any LADSPA and DSSI plugin must have the run() method.
		run = self.ladspa_descriptor.run
	end

	-- Activate plugin.
	-- It should be safe to do here instead of in the tick function.
	if self.ladspa_descriptor.activate ~= nil then
		deactivate = self.ladspa_descriptor.deactivate ~= nil and
		             self.ladspa_descriptor.deactivate
		self.ladspa_descriptor.activate(handle)
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

--- Apply DSSI or LADSPA plugin to stream.
-- For a DSSI plugin, this must usually be called on a @{MIDIStream}.
-- Otherwise, the object stream will be the plugin's first input stream.
-- You cannot use this method to map the object stream to a symbolic port, though.
-- @within Class Stream
-- @string file File name of plugin.
-- @tab[opt] input_ports
--   A table defining the mapping from
--   LADSPA port names to Streams or constants for audio and control input ports.
-- @param ...
--   All additional arguments are added to this table as an array,
--   so port mappings can be specified as a list of arguments as well.
-- @treturn Stream
-- @see DSSIStream:new
function Stream:DSSI(file, ...)
	return DSSIStream:new(file, self, ...)
end
