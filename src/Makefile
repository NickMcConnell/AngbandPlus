MKPATH=../mk/
include $(MKPATH)buildsys.mk

# import source list
include Makefile.src
#INCLUDE = $(HDRS) $(INCS)
CLIENT_OBJECTS = $(CLIENT_ANGFILES) $(CLIENT_ZFILES)
SERVER_OBJECTS = $(SERVER_ANGFILES) $(SERVER_ZFILES)
CLIENT_SRCS    = ${CLIENT_OBJECTS:.o=.c} ${CLIENT_MAINFILES:.o=.c}
SERVER_SRCS    = ${SERVER_OBJECTS:.o=.c} ${SERVER_MAINFILES:.o=.c}
SERVER_PROGNAME = $(PROGNAME)
CLIENT_PROG = $(CLIENT_PROGNAME)$(PROG_SUFFIX)
SERVER_PROG = $(SERVER_PROGNAME)
PROG = $(CLIENT_PROGNAME) $(SERVER_PROGNAME)
VERSION := $(shell ../scripts/version.sh)
ifneq (${VERSION},)
	CFLAGS += -DBUILD_ID=${VERSION}
endif

CFLAGS += -I. -std=c99 -O0

# gcov intermediate data
GCOBJS = $(OBJECTS:.o=.gcno) $(OBJECTS:.o=.gcda)
GCOVS = $(OBJECTS:.o=.c.gcov)

CLEAN = angband.o $(CLIENT_OBJECTS) $(SERVER_OBJECTS) win/angband.res
DISTCLEAN = autoconf.h

export CFLAGS LDFLAGS LIBS

$(SERVER_PROG): $(SERVER_PROGNAME).o $(SERVER_MAINFILES)
	$(CC) -o $@ $(SERVER_PROGNAME).o $(SERVER_MAINFILES) $(LDFLAGS) $(LDADD) $(LIBS)
	@printf "%10s %-20s\n" LINK $@

$(CLIENT_PROG): $(CLIENT_PROGNAME).o $(CLIENT_MAINFILES)
	$(CC) -o $@ $(CLIENT_PROGNAME).o $(CLIENT_MAINFILES) $(LDFLAGS) $(LDADD) $(LIBS)
	@printf "%10s %-20s\n" LINK $@

win/$(PROGNAME).res: win/$(PROGNAME).rc
	$(RC) $< -O coff -o $@

$(SERVER_PROGNAME).o: $(SERVER_OBJECTS)
	$(LD) -nostdlib -r -o $@ $(SERVER_OBJECTS)
	@printf "%10s %-20s\n" LINK $@

$(CLIENT_PROGNAME).o: $(CLIENT_OBJECTS)
	$(LD) -nostdlib -r -o $@ $(CLIENT_OBJECTS)
	@printf "%10s %-20s\n" LINK $@

tests: $(PROGNAME).o
	$(MAKE) -C tests all

test-clean:
	$(MAKE) -C tests clean

splint:
	splint -f .splintrc ${OBJECTS:.o=.c} main.c main-gcu.c

clean-coverage:
	rm -f tests/ran-already
	rm -f $(GCOVS) $(GCOBJS)

coverage: CFLAGS+=-fprofile-arcs -ftest-coverage
coverage: LDFLAGS+=-lgcov
coverage: clean-coverage clean gen-covs

tests/ran-already : tests
	touch $@

gen-covs: tests/ran-already $(GCOVS)
	./gen-coverage $(GCOVS)

%.gcov: %
	(gcov -o $(dir $^) -p $^ >/dev/null)

.PHONY : tests coverage clean-coverage tests/ran-already

#all: $(SERVER_PROG) $(CLIENT_PROG)
#	echo "DONE"
