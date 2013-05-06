MKPATH=../mk/
include $(MKPATH)buildsys.mk

# import source list
include Makefile.src
INCLUDE = $(HDRS) $(INCS)
OBJECTS = $(ANGFILES) $(MAINFILES) $(ZFILES)
SRCS    = ${OBJECTS:.o=.c}
PROG    = nppangband
CFLAGS += -I.
LDFLAGS += -lSDL


$(PROG): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) $(LDADD) -o $@ 
	@printf "%10s %-20s\n" LINK $@

clean-extra:
	-rm autoconf.h
