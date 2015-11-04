CC = gcc

CFLAGS = -std=c99 -Wall -O2

LUAJIT = yes

ifeq ($(LUAJIT),yes)
LUA_CFLAGS = $(shell pkg-config --cflags luajit)
LUA_LDFLAGS = $(shell pkg-config --libs luajit)
else
LUA_CFLAGS = $(shell pkg-config --cflags lua5.1)
LUA_LDFLAGS = $(shell pkg-config --libs lua5.1)
endif

READLINE_CFLAGS =
READLINE_LDFLAGS = -lreadline -lhistory

JACK_CFLAGS := $(shell pkg-config --cflags jack)
JACK_LDFLAGS := $(shell pkg-config --libs jack)

CFLAGS += $(LUA_CFLAGS) $(READLINE_CFLAGS) $(JACK_CFLAGS)
LDFLAGS += $(LUA_LDFLAGS) $(READLINE_LDFLAGS) $(JACK_LDFLAGS) \
           -lpthread

all : applause

applause : applause.o
	$(CC) -o $@ $< $(LDFLAGS)

clean:
	$(RM) *.o applause
