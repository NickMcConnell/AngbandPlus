#
# File: Makefile.inc
#
# Includes Makefile.src and contains dependency info
#

include Makefile.src

BASEOBJS := $(ANGFILES) $(CFILES) $(ZFILES)

depgen:
	head -n 19 Makefile.inc > Makefile.new
	@for i in $(BASEOBJS:.o=.c); do \
	     DIR=`echo ./$$i | sed 's|/[^/]*$$||'`; \
	     gcc -MM -I. $$i | \
	     sed "s|^\\([^ ]\\)|$$DIR/\\1|" >> Makefile.new; \
	done;

