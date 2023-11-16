/* This header is included from C and LuaJIT. */

enum applause_audio_state {
	APPLAUSE_AUDIO_OK = 0,
	APPLAUSE_AUDIO_XRUN,
	APPLAUSE_AUDIO_INVALID_PORT
};

enum applause_audio_state applause_push_sample(int output_port_id,
                                               double sample_double);

int applause_is_interrupted(void);
