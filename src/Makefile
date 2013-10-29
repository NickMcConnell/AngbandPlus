MKPATH=../mk/
include $(MKPATH)buildsys.mk

# import source list
include Makefile.src
INCLUDE = $(HDRS) $(INCS)
OBJECTS = $(ANGFILES) $(ZFILES)
SRCS    = ${OBJECTS:.o=.c} ${MAINFILES:.o=.c}
SUBDIRS = gtk monster object player
PROG    = reposband
CFLAGS += -I. -std=c99 -Wdeclaration-after-statement

reposband: reposband.o $(MAINFILES)
	$(CC) -o reposband reposband.o $(MAINFILES) $(LDFLAGS) $(LDADD)
	@printf "%10s %-20s\n" LINK $@

reposband.o: $(OBJECTS)
	$(LD) -r -o $@ $(OBJECTS)
	@printf "%10s %-20s\n" LINK $@

clean: test-clean

clean-extra:
	-rm autoconf.h

tests:
	$(MAKE) -C tests all

test-clean:
	$(MAKE) -C tests clean

.PHONY : tests
