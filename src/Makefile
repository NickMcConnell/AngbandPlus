include ../mk/rules.mk
include ../mk/init.mk
include ../mk/objective.mk

# import source list
include Makefile.src
HEADERS = $(HDRS) $(INCS)
OBJECTS = $(ANGFILES) $(MAINFILES) $(ZFILES)
SOURCES = ${OBJECTS:.o=.c}

# These are for OS-specific ports; ignore 'em until we use autoconf everywhere.
# SUBDIRS = osx win

OBJECTIVE_BINS = oangband

CFLAGS += \
	-I.

oangband: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LDADD) -o $@ 
	@printf "%10s     %-20s\n" LINK $@

localinstall: oangband
	@if test "${DATA_PATH}" != "./lib/"; then \
		echo; \
		echo "WARNING: data path is ${DATA_PATH}"; \
		echo "If this isn't './lib/', your oangband may not function correctly."; \
	fi
	cp oangband ..
