#include <stdio.h>
#include <stdlib.h>

//#include <semaphore.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <readline/readline.h>
#include <readline/history.h>

#include <jack/jack.h>
#include <jack/ringbuffer.h>

#define LUA_MODULE "applause.lua"

static jack_port_t *output_port;
static jack_client_t *client = NULL;

#define BUFFER_SIZE     (44100*60)      /* 1m samples */
static jack_ringbuffer_t *buffer = NULL;

/**
 * The process callback for this JACK application is called in a
 * special realtime thread once for each audio cycle.
 *
 * This client does nothing more than copy data from its input
 * port to its output port. It will exit when stopped by 
 * the user (e.g. using Ctrl-C on a unix-ish operating system)
 */
static int
jack_process(jack_nframes_t nframes, void *arg)
{
	jack_default_audio_sample_t *out;
	size_t len = sizeof(*out)*nframes;
	size_t r;

	out = (jack_default_audio_sample_t *)
		jack_port_get_buffer(output_port, nframes);

	/**
	 * @bug Do we have to care about more than one
	 * channel per frame?
	 */
	r = jack_ringbuffer_read(buffer, (char *)out, len);
	memset((char *)out + r, 0, len-r);

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
init_audio(void)
{
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

	/* create two ports */

	output_port = jack_port_register (client, "output",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	if (output_port == NULL) {
		fprintf(stderr, "no more JACK ports available\n");
		return 1;
	}

	/*
	 * Initialize ring buffer of samples
	 */
	buffer = jack_ringbuffer_create(sizeof(jack_default_audio_sample_t)*BUFFER_SIZE);
	if (!buffer) {
		fprintf(stderr, "cannot create ringbuffer\n");
		return 1;
	}

	/* Tell the JACK server that we are ready to roll.  Our
	 * process() callback will start running now. */

	if (jack_activate (client)) {
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

	for (;;) {
		jack_default_audio_sample_t sample;

		/* duplicate generator function */
		lua_pushvalue(L, -1);

		/* generate next sample */
		lua_call(L, 0, 1);

		if (lua_isnil(L, -1))
			/* stream has ended */
			break;

		/* copy sample into ring buffer */
		sample = (jack_default_audio_sample_t)luaL_checknumber(L, -1);
		/*
		 * FIXME: Buffer may be full -- perhaps we should wait on a
		 * semaphore
		 */
		jack_ringbuffer_write(buffer, (const char *)&sample, sizeof(sample));

		/* pop sample, the function dup has already been popped */
		lua_pop(L, 1);
	}

	/* any remaining stack elements are automatically popped */
	return 0;
}

int
main(int argc, char **argv)
{
	static const luaL_Reg stream_methods[] = {
		{"play", l_Stream_play},
		{NULL, NULL}
	};

	lua_State *L;

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

	init_audio();

	/*
	 * Register C functions in the `Stream` class
	 */
	lua_getglobal(L, "Stream");
	luaL_register(L, NULL, stream_methods);
	lua_pop(L, 1);

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

	lua_close(L);
}
