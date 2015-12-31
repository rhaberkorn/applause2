#define _XOPEN_SOURCE
#define _POSIX_C_SOURCE 199506L

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <signal.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include <pthread.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <readline/readline.h>
#include <readline/history.h>

#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>

#define LUA_MODULE "applause.lua"

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

static jack_port_t		*output_port;
static jack_client_t		*client = NULL;

#define DEFAULT_BUFFER_SIZE	100	/* milliseconds */
static jack_ringbuffer_t	*buffer = NULL;
static int			buffer_sem;
/**
 * True if a buffer underrun occurs.
 * FIXME: sig_atomic_t is probably the wrong type.
 * Perhaps use the Mintomic types.
 */
static sig_atomic_t		buffer_xrun = 0;

static sig_atomic_t		interrupted = 0;

static jack_port_t		*midi_port;

/**
 * State of all the MIDI notes as updated by
 * NOTE ON/OFF commands.
 * The values are the NOTE ON velocities.
 * Access must be syncronized with `midi_mutex`.
 */
static uint8_t			midi_notes[16][128];
/** The MIDI note triggered last on a channel */
static int			midi_notes_last[16];

/**
 * State of all the MIDI controls as updated by
 * CC commands.
 * Access must be synchronized with `midi_mutex`.
 * Perhaps this should use atomic operations instead
 * (wasting a few kilobytes).
 */
static uint8_t			midi_controls[16][128];

/**
 * Mutex for synchronizing access to `midi_controls`.
 * This MUST have the PTHREAD_PRIO_INHERIT protocol
 * since it will be locked from a realtime thread.
 */
static pthread_mutex_t		midi_mutex;

static int
svsem_init(size_t value)
{
	int id;

	/*
	 * This is not in sem.h but required since
	 * sizeof(int) could be unequal sizeof(void*)
	 */
	union semun {
		int              val;    /* Value for SETVAL */
		struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
		unsigned short  *array;  /* Array for GETALL, SETALL */
		struct seminfo  *__buf;  /* Buffer for IPC_INFO
		                                           (Linux-specific) */
	} arg = {
		.val = value
	};

	id = semget(IPC_PRIVATE, 1, IPC_CREAT | 0777);
	if (id < 0)
		return -1;

	if (semctl(id, 0, SETVAL, arg) < 0)
		return -1;

	return id;
}

static inline int
svsem_op(int id, int value)
{
	struct sembuf op = {
		.sem_num = 0,
		.sem_op = value,
		.sem_flg = 0
	};
	int rc;

	/*
	 * Repeat operation when it is interrupted.
	 */
	while ((rc = semop(id, &op, 1)) < 0 &&
	       errno == EINTR);

	return rc;
}

static inline int
svsem_free(int id)
{
	return semctl(id, 0, IPC_RMID);
}

/**
 * Handler for SIGINT signals.
 *
 * This handler is invoked e.g. when the user presses
 * CTRL+C.
 * It sets `interrupted` which is polled in the Lua play()
 * method which allows us to interrupt long (possibly infinite)
 * sound playback.
 */
static void
sigint_handler(int signum)
{
	interrupted = 1;
}

/**
 * The process callback for this JACK application is called in a
 * special realtime thread once for each audio cycle.
 */
static int
jack_process(jack_nframes_t nframes, void *arg)
{
	jack_default_audio_sample_t *out;
	size_t len = sizeof(*out)*nframes;
	size_t r;

	void *midi_in;
	jack_nframes_t midi_events;

	out = (jack_default_audio_sample_t *)
		jack_port_get_buffer(output_port, nframes);

	/**
	 * @bug Do we have to care about more than one
	 * channel per frame?
	 */
	r = jack_ringbuffer_read(buffer, (char *)out, len);

	/*
	 * The semaphor value corresponds with the number of
	 * writable bytes in buffer, i.e. the available space.
	 * This operation should never block and is supposed to
	 * be real-time safe :-)
	 */
	if (r > 0)
		svsem_op(buffer_sem, r);

	/*
	 * Here we're assuming that memset() is realtime-capable.
	 * It might not be on every UNIX!?
	 */
	memset((char *)out + r, 0, len - r);
	buffer_xrun |= len - r > 0;

	/*
	 * MIDI processing.
	 * NOTE: This uses a priority inheriting mutex to
	 * remain realtime capable.
	 */
	midi_in = jack_port_get_buffer(midi_port, nframes);
	midi_events = jack_midi_get_event_count(midi_in);

	for (int i = 0; i < midi_events; i++) {
		jack_midi_event_t event;
		int channel;

		jack_midi_event_get(&event, midi_in, i);
		channel = event.buffer[0] & 0x0F;

		switch (event.buffer[0] & 0xF0) {
		case 0x80: /* NOTE OFF */
			pthread_mutex_lock(&midi_mutex);
			/* NOTE: The NOTE OFF velocity is currently ignored */
			midi_notes[channel]
			          [event.buffer[1]] = 0;
			midi_notes_last[channel] = event.buffer[1];
			pthread_mutex_unlock(&midi_mutex);
			break;

		case 0x90: /* NOTE ON */
			pthread_mutex_lock(&midi_mutex);
			/* NOTE: Velocity of 0 has the same effect as NOTE OFF */
			midi_notes[channel]
			          [event.buffer[1]] = event.buffer[2];
			midi_notes_last[channel] = event.buffer[1];
			pthread_mutex_unlock(&midi_mutex);
			break;

		case 0xB0: /* Control Change */
			pthread_mutex_lock(&midi_mutex);
			midi_controls[channel]
			             [event.buffer[1]] = event.buffer[2];
			pthread_mutex_unlock(&midi_mutex);
			break;
		}
	}

	return 0;
}

/**
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
static void
jack_shutdown(void *arg)
{
	exit(EXIT_FAILURE);
}

static int
init_audio(int buffer_size)
{
	size_t buffer_bytes;
	const char **ports;
	const char *client_name = "applause";
	const char *server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;

	/* open a client connection to the JACK server */

	client = jack_client_open(client_name, options, &status, server_name);
	if (client == NULL) {
		fprintf(stderr, "jack_client_open() failed, "
			        "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf(stderr, "Unable to connect to JACK server\n");
		}
		return 1;
	}
	if (status & JackServerStarted)
		fprintf(stderr, "JACK server started\n");

	if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf(stderr, "unique name `%s' assigned\n", client_name);
	}

	/* tell the JACK server to call `process()' whenever
	   there is work to be done.
	*/

	jack_set_process_callback(client, jack_process, 0);

	/* tell the JACK server to call `jack_shutdown()' if
	   it ever shuts down, either entirely, or if it
	   just decides to stop calling us.
	*/

	jack_on_shutdown (client, jack_shutdown, 0);

	/*
	 * Create output ports
	 */
	output_port = jack_port_register (client, "output",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);
	if (output_port == NULL) {
		fprintf(stderr, "no more JACK ports available\n");
		return 1;
	}

	/*
	 * Create MIDI input port
	 */
	midi_port = jack_port_register(client, "midi_input",
	                               JACK_DEFAULT_MIDI_TYPE,
	                               JackPortIsInput, 0);
	if (midi_port == NULL) {
		fprintf(stderr, "no more JACK ports available\n");
		return 1;
	}

	memset(&midi_controls, 0, sizeof(midi_controls));

	pthread_mutexattr_t prioinherit;
	if (pthread_mutexattr_setprotocol(&prioinherit, PTHREAD_PRIO_INHERIT)) {
		fprintf(stderr, "Error initializing MIDI mutex with priority inheritance!\n");
		return 1;
	}

	pthread_mutex_init(&midi_mutex, &prioinherit);

	/*
	 * Calculate the buffer size in bytes given the `buffer_size`
	 * in milliseconds.
	 * Make sure it is at least the Jack server's buffer size,
	 * else jack_process() is very likely not able to provide
	 * enough bytes.
	 * FIXME: The Jack server's sample rate and buffer size can
	 * theoretically change at runtime but currently,
	 * the buffer size is not adapted
	 * which means it could be too small after the change.
	 */
	buffer_bytes = sizeof(jack_default_audio_sample_t)*
	               MAX(jack_get_sample_rate(client)*buffer_size/1000,
	                   jack_get_buffer_size(client));

	/*
	 * Initialize ring buffer of samples.
	 * The semaphore is initialized with the same size
	 * since it represents the available bytes in the ring
	 * buffer.
	 */
	buffer = jack_ringbuffer_create(buffer_bytes);
	if (!buffer) {
		fprintf(stderr, "cannot create ringbuffer\n");
		return 1;
	}

	buffer_sem = svsem_init(buffer_bytes);
	if (buffer_sem < 0) {
		fprintf(stderr, "error initializing semaphore\n");
		return 1;
	}

	/* Tell the JACK server that we are ready to roll.  Our
	 * process() callback will start running now. */

	if (jack_activate(client)) {
		fprintf (stderr, "cannot activate client\n");
		return 1;
	}

	/* Connect the ports.  You can't do this before the client is
	 * activated, because we can't make connections to clients
	 * that aren't running.  Note the confusing (but necessary)
	 * orientation of the driver backend ports: playback ports are
	 * "input" to the backend, and capture ports are "output" from
	 * it.
	 */

	ports = jack_get_ports (client, NULL, NULL,
				JackPortIsPhysical|JackPortIsInput);
	if (ports == NULL) {
		fprintf(stderr, "no physical playback ports\n");
		return 1;
	}

	if (jack_connect (client, jack_port_name (output_port), ports[0])) {
		fprintf (stderr, "cannot connect output ports\n");
	}

	free (ports);
	return 0;
}

static int
l_MIDIVelocityStream_getValue(lua_State *L)
{
	int top = lua_gettop(L);
	lua_Integer channel, note, value;

	luaL_argcheck(L, top == 2, top, "Note and channel number expected");
	luaL_checktype(L, 1, LUA_TNUMBER);
	luaL_checktype(L, 2, LUA_TNUMBER);

	note = lua_tointeger(L, 1);
	luaL_argcheck(L, 0 <= note && note <= 127, note,
	              "Invalid note number range (0 <= x <= 127)");
	channel = lua_tointeger(L, 2);
	luaL_argcheck(L, 1 <= channel && channel <= 16, channel,
	              "Invalid channel range (1 <= x <= 16)");
	/* The NOTE arrays are 0-based */
	channel--;

	pthread_mutex_lock(&midi_mutex);
	/*
	 * This thread might be lifted to realtime priority
	 * since this is a priority inheritance mutex.
	 * We will block a realtime thread while we're in the
	 * critical section.
	 * Therefore it is crucial that the following code
	 * is realtime-safe.
	 */
	value = midi_notes[channel][note];
	pthread_mutex_unlock(&midi_mutex);

	lua_pushinteger(L, value);
	return 1;
}

static int
l_MIDINoteStream_getValue(lua_State *L)
{
	int top = lua_gettop(L);
	lua_Integer channel, value;

	luaL_argcheck(L, top == 1, top, "Channel number expected");
	luaL_checktype(L, 1, LUA_TNUMBER);

	channel = lua_tointeger(L, 1);
	luaL_argcheck(L, 1 <= channel && channel <= 16, channel,
	              "Invalid channel range (1 <= x <= 16)");
	/* The NOTE arrays are 0-based */
	channel--;

	pthread_mutex_lock(&midi_mutex);
	/*
	 * This thread might be lifted to realtime priority
	 * since this is a priority inheritance mutex.
	 * We will block a realtime thread while we're in the
	 * critical section.
	 * Therefore it is crucial that the following code
	 * is realtime-safe.
	 */
	value = midi_notes_last[channel] |
	        (midi_notes[channel][midi_notes_last[channel]] << 8);
	pthread_mutex_unlock(&midi_mutex);

	lua_pushinteger(L, value);
	return 1;
}

static int
l_MIDICCStream_getValue(lua_State *L)
{
	int top = lua_gettop(L);
	lua_Integer channel, control, value;

	luaL_argcheck(L, top == 2, top, "Control and channel number expected");
	luaL_checktype(L, 1, LUA_TNUMBER);
	luaL_checktype(L, 2, LUA_TNUMBER);

	control = lua_tointeger(L, 1);
	luaL_argcheck(L, 0 <= control && control <= 127, control,
	              "Invalid control number range (0 <= x <= 127)");
	channel = lua_tointeger(L, 2);
	luaL_argcheck(L, 1 <= channel && channel <= 16, channel,
	              "Invalid channel range (1 <= x <= 16)");
	/* The NOTE arrays are 0-based */
	channel--;

	pthread_mutex_lock(&midi_mutex);
	/*
	 * This thread might be lifted to realtime priority
	 * since this is a priority inheritance mutex.
	 * We will block a realtime thread while we're in the
	 * critical section.
	 * Therefore it is crucial that the following code
	 * is realtime-safe.
	 */
	value = midi_controls[channel][control];
	pthread_mutex_unlock(&midi_mutex);

	lua_pushinteger(L, value);
	return 1;
}

static int
l_Stream_play(lua_State *L)
{
	int top = lua_gettop(L);

	luaL_argcheck(L, top == 1, top, "Stream object expected");
	luaL_checktype(L, 1, LUA_TTABLE);

	/* get tick() method */
	lua_getfield(L, -1, "tick");
	luaL_checktype(L, -1, LUA_TFUNCTION);
	/* move in front of stream table since it needs a `self' argument */
	lua_insert(L, 1);

	lua_call(L, 1, 1);
	/* the tick generator function should now be on top of the stack */
	luaL_checktype(L, -1, LUA_TFUNCTION);

	/*
	 * Perform garbage collection cycle and turn it off
	 * temporarily. This improves the realtime properties
	 * of the sample generation loop below.
	 */
	lua_gc(L, LUA_GCCOLLECT, 0);
	lua_gc(L, LUA_GCSTOP, 0);

	interrupted = 0;

	while (!interrupted) {
		jack_default_audio_sample_t sample;

		/*
		 * React to buffer underruns.
		 * This is done here instead of in the realtime thread
		 * even though it is already overloaded, so as not to
		 * affect other applications in the Jack graph.
		 */
		if (buffer_xrun) {
			fprintf(stderr, "WARNING: Buffer underrun detected!\n");
			buffer_xrun = 0;
		}

		/* duplicate generator function */
		lua_pushvalue(L, -1);

		/* generate next sample */
		lua_call(L, 0, 1);

		if (lua_isnil(L, -1))
			/* stream has ended */
			break;

		/* copy sample into ring buffer */
		/*
		 * FIXME: What if sample isn't a number. This
		 * should be handled. But don't forget to restart
		 * the garbage collector
		 */
		sample = (jack_default_audio_sample_t)lua_tonumber(L, -1);

		/*
		 * We are about to "consume" one free sample in the buffer.
		 * This can block when the buffer is full.
		 * After this operation, there is __at least__ one sample free
		 * in buffer since jack_process() will only read from the
		 * buffer.
		 */
		svsem_op(buffer_sem, -(int)sizeof(sample));

		jack_ringbuffer_write(buffer, (const char *)&sample,
		                      sizeof(sample));

		/* pop sample, the function dup has already been popped */
		lua_pop(L, 1);
	}

	lua_gc(L, LUA_GCRESTART, 0);

	if (interrupted)
		return luaL_error(L, "SIGINT received");

	/* any remaining stack elements are automatically popped */
	return 0;
}

typedef struct NativeMethod {
	const char *class_name;
	const char *method_name;
	lua_CFunction func;
} NativeMethod;

static const NativeMethod native_methods[] = {
	{"Stream",             "play",     l_Stream_play},
	{"MIDIVelocityStream", "getValue", l_MIDIVelocityStream_getValue},
	{"MIDINoteStream",     "getValue", l_MIDINoteStream_getValue},
	{"MIDICCStream",       "getValue", l_MIDICCStream_getValue},
	{NULL, NULL, NULL}
};

int
main(int argc, char **argv)
{
	int buffer_size = DEFAULT_BUFFER_SIZE;
	struct sigaction sigint_action;

	lua_State *L;

	/*
	 * FIXME: Support --help
	 */
	if (argc > 1)
		buffer_size = atoi(argv[1]);

	/*
	 * Register sigint_handler() as the SIGINT handler.
	 * This sets `interrupted`. Currently this is only polled
	 * in the Lua play() method in order to interrupt long
	 * sound playing.
	 * Otherwise it is ignored.
	 */
	memset(&sigint_action, 0, sizeof(sigint_action));
	sigint_action.sa_handler = sigint_handler;
	sigaction(SIGINT, &sigint_action, NULL);

	L = luaL_newstate();
	if (!L) {
		fprintf(stderr, "Error creating Lua state.\n");
		exit(EXIT_FAILURE);
	}

	luaL_openlibs(L);

	if (luaL_loadfile(L, LUA_MODULE) || lua_pcall(L, 0, 0, 0)) {
		/* FIXME: pop error message */
		fprintf(stderr, "Error loading Lua module %s.\n",
		        LUA_MODULE);
		exit(EXIT_FAILURE);
	}

	init_audio(buffer_size);

	/*
	 * Register native C functions.
	 * This may be unefficient when registering multiple
	 * methods in the same class.
	 */
	for (const NativeMethod *method = native_methods;
	     method->class_name;
	     method++) {
		lua_getglobal(L, method->class_name);
		lua_pushcfunction(L, method->func);
		lua_setfield(L, -2, method->method_name);
		lua_pop(L, 1); /* pop class table */
	}

	/*
	 * Set global `samplerate`
	 */
	lua_pushinteger(L, (lua_Integer)jack_get_sample_rate(client));
	lua_setglobal(L, "samplerate");

	for (;;) {
		int stack_top;
		char *line = readline("> ");

		if (!line) {
			putchar('\n');
			exit(EXIT_SUCCESS);
		}

		/*
		 * Push print command
		 */
		lua_getglobal(L, "print");
		stack_top = lua_gettop(L);

		if (luaL_dostring(L, line)) {
			fprintf(stderr, "Error.\n");
		}

		/*
		 * Automatically print values left on the stack
		 */
		if (lua_pcall(L, lua_gettop(L) - stack_top, 0, 0)) {
			fprintf(stderr, "Error.\n");
		}

		if (*line)
			add_history(line);

		free(line);
	}

	svsem_free(buffer_sem);

	lua_close(L);
}
