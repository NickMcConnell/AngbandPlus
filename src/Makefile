MKPATH=../mk/
include $(MKPATH)buildsys.mk

# import source list
include Makefile.src
INCLUDE = $(HDRS)
OBJECTS = $(ANGFILES) $(CFILES) $(ZFILES)
SRCS    = ${OBJECTS:.o=.c} ${MAINFILES:.o=.c}
PROG    = poschengband$(PROG_SUFFIX)
# VERSION := $(shell ../scripts/version.sh)

#Use this for debug code:
#Note: configure is not setting _GNU_SOURCE for us, but is in vanilla Angband.
#      I can't figure out how, though. Perhaps sdl is doing it:
#      >sdl-config --cflags
#      -I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT
CFLAGS += -D_GNU_SOURCE=1 -DBUILD_ID=${VERSION} -I. -std=c99 -Wdeclaration-after-statement -O0 -g -fno-omit-frame-pointer

#Use this for release code:
#CFLAGS += -D_GNU_SOURCE=1 -DBUILD_ID=${VERSION} -I. -std=c99 -Wdeclaration-after-statement -DNDEBUG -O2 -fno-omit-frame-pointer

CLEAN = poschengband.o $(OBJECTS) win/poschengband.res
DISTCLEAN = autoconf.h

export CFLAGS LDFLAGS LIBS

$(PROG): poschengband.o $(MAINFILES)
	$(CC) -o $@ poschengband.o $(MAINFILES) $(LDFLAGS) $(LDADD) $(LIBS)
	@printf "%10s %-20s\n" LINK $@

win/poschengband.res: win/poschengband.rc
	$(RC) $< -O coff -o $@

poschengband.o: $(OBJECTS)
	$(LD) -nostdlib -Wl,-r -o $@ $(OBJECTS)
	@printf "%10s %-20s\n" LINK $@

splint:
	splint -f .splintrc ${OBJECTS:.o=.c} main.c main-gcu.c

