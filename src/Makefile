MKPATH=../mk/
include $(MKPATH)buildsys.mk

# import source list
include Makefile.src
INCLUDE = $(HDRS)
OBJECTS = $(ANGFILES) $(CFILES) $(ZFILES)
SRCS    = ${OBJECTS:.o=.c} ${MAINFILES:.o=.c}
PROG    = $(PROGNAME)$(PROG_SUFFIX)
# VERSION := $(shell ../scripts/version.sh)
ifneq (${VERSION},)
	CFLAGS += -DBUILD_ID=${VERSION}
endif

CFLAGS += -I. -std=c99 -O0 -Wdeclaration-after-statement -fno-omit-frame-pointer

CLEAN = poschengband.o $(OBJECTS) win/poschengband.res
DISTCLEAN = autoconf.h

export CFLAGS LDFLAGS LIBS

$(PROG): $(PROGNAME).o $(MAINFILES)
	$(CC) -o $@ $(PROGNAME).o $(MAINFILES) $(LDFLAGS) $(LDADD) $(LIBS)
	@printf "%10s %-20s\n" LINK $@

$(PROGNAME).res: $(PROGNAME).rc
	$(RC) $< -O coff -o $@

$(PROGNAME).o: $(OBJECTS)
	$(LD) -nostdlib -Wl,-r -o $@ $(OBJECTS)
	@printf "%10s %-20s\n" LINK $@

splint:
	splint -f .splintrc ${OBJECTS:.o=.c} main.c main-gcu.c

