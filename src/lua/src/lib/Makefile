
# Modified by Kamband to remove floating-point support.

# makefile for Lua standard library

LUA= ../..

include $(LUA)/config

# actually only used in liolib.c
EXTRA_DEFS= $(POPEN)

OBJS= lauxlib.o lbaselib.o ldblib.o lstrlib.o #lmathlib.o liolib.o
SRCS= lauxlib.c lbaselib.c ldblib.c lstrlib.c #lmathlib.c liolib.c

T= $(LIB)/liblualib.a

all: $T

$T: $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean:
	rm -f $(OBJS) $T

co:
	co -q -f -M $(SRCS)

klean:	clean
	rm -f $(SRCS)
