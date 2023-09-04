/* This header is included from C and LuaJIT. */

// FIXME: Perhaps a struct would be easier to handle?
typedef uint32_t applause_midi_sample;

applause_midi_sample applause_pull_midi_sample(void);
