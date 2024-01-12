CC = cc

CFLAGS = -std=c99 -Wall -O2 -g

LUA_CFLAGS = $(shell pkg-config --cflags luajit)
LUA_LDFLAGS = $(shell pkg-config --libs luajit)

READLINE_CFLAGS =
READLINE_LDFLAGS = -lreadline -lhistory

JACK_CFLAGS := $(shell pkg-config --cflags jack)
JACK_LDFLAGS := $(shell pkg-config --libs jack)

CFLAGS += $(LUA_CFLAGS) $(READLINE_CFLAGS) $(JACK_CFLAGS)
LDFLAGS += $(LUA_LDFLAGS) $(READLINE_LDFLAGS) $(JACK_LDFLAGS) \
           -lpthread

# For exporting function from applause.c that can be called
# with the LuaJIT FFI interface:
LDFLAGS += -rdynamic

.PHONY: all doc

all : applause

applause : applause.o evdev.o
	$(CC) -o $@ $^ $(LDFLAGS)

doc:
	ldoc .

clean:
	$(RM) *.o applause
