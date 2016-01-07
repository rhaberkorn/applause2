#define _XOPEN_SOURCE
#define _POSIX_C_SOURCE 199506L
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include <arpa/inet.h>

#include <pthread.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <luajit.h>

#include <readline/readline.h>
#include <readline/history.h>

#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>

#define LUA_MODULE "applause.lua"

#define CMD_SERVER_IP   "127.0.0.1"
#define CMD_SERVER_PORT 10000

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

static sig_atomic_t		playback = 0;

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
 * Handler for SIGINT, SIGUSR1 and SIGUSR2 signals.
 *
 * SIGINT is delivered e.g. when the user presses
 * CTRL+C.
 * It sets `interrupted` which is polled in the Lua play()
 * method which allows us to interrupt long (possibly infinite)
 * sound playback.
 *
 * SIGUSR1 and SIGUSR2 are sent by parent to child
 * clients in order to control playback.
 */
static void
signal_handler(int signum)
{
	switch (signum) {
	case SIGINT: interrupted = 1; break;
	case SIGUSR1: playback = 1; break;
	case SIGUSR2: playback = 0; break;
	}
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
	memset(&midi_notes, 0, sizeof(midi_notes));
	memset(&midi_notes_last, 0, sizeof(midi_notes_last));

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

/**
 * Get the MIDI velocity of the `note` last
 * triggered on `channel` due to a NOTE ON message.
 * This function is meant to be called using LuaJIT's
 * FFI interface.
 */
int
applause_midi_velocity_getvalue(int note, int channel)
{
	int value;

	/*
	 * It's enough to assert() here since this
	 * function should only be called by the
	 * MIDIVelocityStream generator.
	 */
	assert(0 <= note && note <= 127);
	assert(1 <= channel && channel <= 16);

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

	return value;
}

/**
 * Get the MIDI note and velocity of the note last
 * triggered on `channel`.
 * This function is meant to be called using LuaJIT's
 * FFI interface.
 *
 * @return A MIDI note number (least significant byte) and
 *         velocity (second least significant byte).
 */
int
applause_midi_note_getvalue(int channel)
{
	int value;

	/*
	 * It's enough to assert() here since this
	 * function should only be called by the
	 * MIDINoteStream generator.
	 */
	assert(1 <= channel && channel <= 16);

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

	return value;
}

/**
 * Get the last value of the MIDI control `control`
 * on `channel`.
 * This function is meant to be called using LuaJIT's
 * FFI interface.
 */
int
applause_midi_cc_getvalue(int control, int channel)
{
	int value;

	/*
	 * It's enough to assert() here since this
	 * function should only be called by the
	 * MIDICCStream generator.
	 */
	assert(0 <= control && control <= 127);
	assert(1 <= channel && channel <= 16);

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

	return value;
}

enum applause_audio_state {
	APPLAUSE_AUDIO_OK = 0,
	APPLAUSE_AUDIO_INTERRUPTED,
	APPLAUSE_AUDIO_XRUN
};

/**
 * Push one Jack sample into the ring buffer.
 *
 * This function should be called from the Stream:play()
 * implementation, which is faster than calling Lua functions
 * from a C implementation of Stream:play() using the Lua C API
 * (supposedly, I could not reproduce this).
 *
 * @param sample_double The audio sample as a double (compatible
 *                      with lua_Number).
 *                      This is speed relevant since otherwise
 *                      LuaJIT tries to translate to Jack's default
 *                      float type.
 */
enum applause_audio_state
applause_push_sample(double sample_double)
{
	jack_default_audio_sample_t sample =
			(jack_default_audio_sample_t)sample_double;

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

	if (interrupted) {
		interrupted = 0;
		return APPLAUSE_AUDIO_INTERRUPTED;
	}
	if (buffer_xrun) {
		buffer_xrun = 0;
		return APPLAUSE_AUDIO_XRUN;
	}

	return APPLAUSE_AUDIO_OK;
}

static int
l_Stream_fork(lua_State *L)
{
	int top = lua_gettop(L);
	pid_t child_pid;

	luaL_argcheck(L, top == 1, top, "Stream object expected");
	luaL_checktype(L, 1, LUA_TTABLE);

	/*
	 * Query the parent client's ports.
	 * We need to clone them in the child.
	 */
	//port_names = jack_get_ports(client, NULL, NULL, 0);

	/*
	 * Deactivate parent client temporarily, so forking works
	 * safely.
	 * This is possible since we can assume that fork() is only
	 * called interactively - so the parent client is not currently
	 * generating samples.
	 */
	jack_client_close(client);
	//jack_deactivate(client);

	child_pid = fork();
	if (child_pid != 0) {
		/* in the parent process */
		//jack_activate(client);
		init_audio(DEFAULT_BUFFER_SIZE);

		/* call Client:new(child_pid) */
		lua_getglobal(L, "Client");
		lua_getfield(L, -1, "new");
		/* make Client parameter to new() */
		lua_insert(L, -2);
		lua_pushinteger(L, child_pid);
		lua_call(L, 2, 1);

		return 1;
	}

	//jack_client_close(client);

	init_audio(DEFAULT_BUFFER_SIZE);

	for (;;) {
		/*
		 * Wait for start of playback (SIGUSR1)
		 */
		while (!playback)
			pause();

		/* get play() method */
		lua_getfield(L, -1, "play");
		luaL_checktype(L, -1, LUA_TFUNCTION);
		/* duplicate the object table */
		lua_pushvalue(L, 1);

		lua_call(L, 1, 0);
	}

	/* never reached */
	return 0;
}

/* stolen from luajit.c :-) */
static int
traceback(lua_State *L)
{
	if (!lua_isstring(L, 1)) { /* Non-string error object? Try metamethod. */
		if (lua_isnoneornil(L, 1) ||
		    !luaL_callmeta(L, 1, "__tostring") ||
		    !lua_isstring(L, -1))
			return 1;  /* Return non-string error object. */
		lua_remove(L, 1);  /* Replace object by result of __tostring metamethod. */
	}

	luaL_traceback(L, L, lua_tostring(L, 1), 1);

	return 1;
}

static void
do_command(lua_State *L, const char *command)
{
	int stack_top;
	char *buffer;

	/*
	 * EXPERIMENTAL: I found that performance decreases
	 * progressively when running one and the same command.
	 * This is probably because the limited space for the
	 * compiled code gets filled up rapidly and LuaJIT
	 * resorts to using the interpreter.
	 * This flashes the entire code cache for every
	 * new command and improves matters.
	 * TODO: It would probably be a good idea to increase the
	 * total size of the code cache and give some indication
	 * of its usage (e.g. in the prompt).
	 */
	luaJIT_setmode(L, 0, LUAJIT_MODE_ENGINE | LUAJIT_MODE_FLUSH);

	/* the error hanlder function for lua_pcall() */
	lua_pushcfunction(L, traceback);

	stack_top = lua_gettop(L);

	/*
	 * AFAIK, we cannot support automatic printing of values
	 * returned by expressions.
	 * The chunk must return something (using `return`).
	 * We cannot always prepend a `return` since AFAIK it is
	 * impossible to embedded statements into expressions.
	 * Therefore we support "=" as a shortcut to "return" just
	 * like the luajit shell does.
	 */
	if (*command == '=') {
		if (asprintf(&buffer, "return %s", command+1) < 0)
			buffer = NULL;
	} else {
		buffer = strdup(command);
	}
	assert(buffer != NULL);

	if (luaL_loadstring(L, buffer) || lua_pcall(L, 0, LUA_MULTRET, -2)) {
		fprintf(stderr, "Error.\n");
	}

	free(buffer);

	/*
	 * Print values left on the stack:
	 * This includes error messages left by lua_pcall()
	 */
	if (lua_gettop(L) > stack_top) {
		lua_getglobal(L, "print");
		lua_insert(L, stack_top + 1);

		if (lua_pcall(L, lua_gettop(L) - stack_top - 1, 0, 0)) {
			fprintf(stderr, "Error executing print().\n");
			/* try to continue */
		}
	}

	/* pop the traceback function */
	lua_remove(L, -1);
}

/**
 * Mutex protecting Lua state access after the command server
 * has been launched.
 */
static pthread_mutex_t lua_mutex = PTHREAD_MUTEX_INITIALIZER;

/*
 * FIXME: This should perhaps be the only interface to the
 * applause Lua state with the REPL loop implemented as a Lua
 * script. This solution would simplify the C code and avoid
 * threading. Also, then we could have multiple running
 * REPL loops.
 */
static void *
command_server(void *user_data)
{
	lua_State *L = user_data;
	int socket_fd;
	struct sockaddr_in server;

	socket_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (socket_fd < 0) {
		perror("socket");
		return NULL;
	}

	server.sin_addr.s_addr = inet_addr(CMD_SERVER_IP);
	server.sin_family = AF_INET;
	server.sin_port = htons(CMD_SERVER_PORT);

	if (bind(socket_fd, (struct sockaddr *)&server,
	    sizeof(server)) < 0) {
		perror("bind");
		close(socket_fd);
		return NULL;
	}

	if (listen(socket_fd, 1) < 0) {
		perror("listen");
		close(socket_fd);
		return NULL;
	}

	for (;;) {
		char length_buf[5 + 1];
		size_t length;
		char *command;

		int old_stdout, old_stderr;

		int client_fd = accept(socket_fd, NULL, NULL);

		if (client_fd < 0) {
			perror("accept");
			break;
		}

		if (read(client_fd, length_buf, sizeof(length_buf)) <
		    sizeof(length_buf)) {
			/* just close the connection and continue */
			/*
			 * FIXME: We might want to handle errors
			 * (e.g. signal interruptions)
			 */
			close(client_fd);
			continue;
		}

		/* parse message length */
		length = atoi(length_buf);
		command = malloc(length);
		assert(command != NULL);

		if (read(client_fd, command, length) < length) {
			/* just close the connection and continue */
			/*
			 * FIXME: We might want to handle errors
			 * (e.g. signal interruptions)
			 */
			free(command);
			close(client_fd);
			continue;
		}

		pthread_mutex_lock(&lua_mutex);

		/*
		 * Redirect stdout and stderr to client_fd.
		 * This way, we capture all the output that the command
		 * may have.
		 */
		old_stdout = dup(1);
		close(1);
		dup2(client_fd, 1);
		old_stderr = dup(2);
		close(2);
		dup2(client_fd, 2);

		/*
		 * FIXME: It would be nice to catch broken sockets
		 * (e.g. waiting for play() has been interrupted), in order
		 * to set the interrupted flag.
		 * However it seems we'd need another thread for that running
		 * do_command(), so we can select() the fd and eventually
		 * join the thread.
		 */
		do_command(L, command);
		free(command);

		/*
		 * Restore stdout/stderr.
		 */
		close(1);
		dup2(old_stdout, 1);
		close(old_stdout);
		close(2);
		dup2(old_stderr, 2);
		close(old_stderr);

		pthread_mutex_unlock(&lua_mutex);

		close(client_fd);
	}

	close(socket_fd);
	return NULL;
}

typedef struct NativeMethod {
	const char *class_name;
	const char *method_name;
	lua_CFunction func;
} NativeMethod;

static const NativeMethod native_methods[] = {
	{"Stream",             "fork",     l_Stream_fork},
	{NULL, NULL, NULL}
};

int
main(int argc, char **argv)
{
	int buffer_size = DEFAULT_BUFFER_SIZE;
	struct sigaction signal_action;

	lua_State *L;

	pthread_t command_server_thread;

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
	memset(&signal_action, 0, sizeof(signal_action));
	signal_action.sa_handler = signal_handler;
	sigaction(SIGINT, &signal_action, NULL);
	sigaction(SIGUSR1, &signal_action, NULL);
	sigaction(SIGUSR2, &signal_action, NULL);

	signal_action.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &signal_action, NULL);

	L = luaL_newstate();
	if (!L) {
		fprintf(stderr, "Error creating Lua state.\n");
		exit(EXIT_FAILURE);
	}

	luaL_openlibs(L);

	lua_pushcfunction(L, traceback);

	if (luaL_loadfile(L, LUA_MODULE) || lua_pcall(L, 0, 0, -2)) {
		fprintf(stderr, "Error loading Lua module %s: %s\n",
		        LUA_MODULE, lua_tostring(L, -1));
		exit(EXIT_FAILURE);
	}

	/* remove traceback function */
	lua_remove(L, -1);

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

	/*
	 * Launch the command server.
	 */
	if (pthread_create(&command_server_thread, NULL, command_server, L)) {
		perror("pthread_create");
		exit(EXIT_FAILURE);
	}

	/*
	 * Main REPL loop.
	 * Since the command server has been launched, all lua state access
	 * must be synchronized using lua_mutex.
	 */
	for (;;) {
		/*
		 * FIXME: Get global _PROMPT or _PROMPT2
		 */
		char *line = readline("> ");

		if (!line) {
			putchar('\n');
			break;
		}

		pthread_mutex_lock(&lua_mutex);
		do_command(L, line);
		pthread_mutex_unlock(&lua_mutex);

		if (*line)
			add_history(line);

		free(line);
	}

	/*
	 * FIXME: Shut down connection server.
	 */

	svsem_free(buffer_sem);

	lua_close(L);
	return 0;
}
