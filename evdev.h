/* This header is included from C and LuaJIT. */

typedef struct applause_evdev_sample {
	uint16_t type;
	uint16_t code;
	int32_t value;
} applause_evdev_sample;

typedef struct applause_evdev applause_evdev;

char *applause_evdev_getname(const char *node);
applause_evdev *applause_evdev_new(const char *node, bool grab);
void applause_evdev_pull(applause_evdev *evdev, applause_evdev_sample *sample);
void applause_evdev_free(applause_evdev *evdev);
