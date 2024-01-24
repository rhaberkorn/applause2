#define _GNU_SOURCE

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <linux/input.h>

#include <pthread.h>

#include <jack/ringbuffer.h>

#include "evdev.h"

struct applause_evdev {
	int			fd;
	jack_ringbuffer_t	*buffer;
	pthread_t		thread;
};

const char *
applause_dirent_name(const struct dirent *entry)
{
	return entry->d_name;
}

char *
applause_evdev_getname(const char *node)
{
	int rc, fd;

	fd = open(node, O_RDONLY);
	if (fd < 0)
		return NULL;

	char name[256];
	rc = ioctl(fd, EVIOCGNAME(sizeof(name)), name);
	close(fd);

	return rc < 0 ? NULL : strdup(name);
}

static void *
applause_evdev_thread_cb(void *userdata)
{
	applause_evdev *evdev = userdata;

	for (;;) {
		struct input_event event;
		applause_evdev_sample sample;

		/*
		 * FIXME: How to detect thread cancellation reliably?
		 */
		if (read(evdev->fd, &event, sizeof(event)) != sizeof(event))
			break;

		/*
		 * FIXME: Why do we get one "empty" event after each successful read?
		 */
		if (event.type == 0)
			continue;

		sample.type = event.type;
		sample.code = event.code;
		sample.value = event.value;
		jack_ringbuffer_write(evdev->buffer, (const char *)&sample, sizeof(sample));
	}

	/* never reached */
	return NULL;
}

applause_evdev *
applause_evdev_new(const char *node, bool grab)
{
	applause_evdev *evdev = calloc(1, sizeof(applause_evdev));
	if (!evdev)
		return NULL;

	evdev->fd = open(node, O_RDONLY);
	if (evdev->fd < 0)
		goto error;

	/*
	 * This will fail if the device is already grabbed
	 * which makes sense since you cannot receive events for such a device.
	 */
	if (grab && ioctl(evdev->fd, EVIOCGRAB, true))
		goto error;

	evdev->buffer = jack_ringbuffer_create(sizeof(applause_evdev_sample)*1024);
	if (!evdev->buffer)
		goto error;

	if (pthread_create(&evdev->thread, NULL, applause_evdev_thread_cb, evdev))
		goto error;

	return evdev;

error:
	if (evdev->buffer)
		jack_ringbuffer_free(evdev->buffer);
	if (evdev->fd > 0)
		close(evdev->fd);
	free(evdev);
	return NULL;
}

void
applause_evdev_pull(applause_evdev *evdev, applause_evdev_sample *sample)
{
	memset(sample, 0, sizeof(*sample));
	jack_ringbuffer_read(evdev->buffer, (char *)sample, sizeof(*sample));
}

void
applause_evdev_free(applause_evdev *evdev)
{
	/*
	 * NOTE: It's important to support evdev == NULL so that applause_evdev_free()
	 * can be passed safely to ffi.gc().
	 */
	if (!evdev)
		return;

	pthread_cancel(evdev->thread);
	pthread_join(evdev->thread, NULL);
	jack_ringbuffer_free(evdev->buffer);
	close(evdev->fd);
	free(evdev);
}
