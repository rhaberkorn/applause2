CC = gcc

CFLAGS = -std=c99 -Wall -O2

LUA_CFLAGS = $(shell pkg-config --cflags luajit)
LUA_LDFLAGS = $(shell pkg-config --libs luajit)

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
