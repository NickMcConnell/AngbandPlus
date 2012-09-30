##
## This makefile will hopefully replace the old ways of
## building zangband.  Using automake is crufty, and using
## makefile.std doesn't quite cover all the nice things
## that autoconf does.
##
##
##
## Please do not edit "makefile", as it is auto-generated
## by ./configure from makefile.in  Edit makefile.in instead.
##
## makefile.  Generated from makefile.in by configure.

CC := gcc

CFLAGS := -Wnested-externs -Wundef -Wuninitialized -Wunused -Wswitch -Wreturn-type -Wsequence-point -Wimplicit -Wchar-subscripts -Wredundant-decls -Wstrict-prototypes -Waggregate-return -Wbad-function-cast -Wpointer-arith -Wwrite-strings -Wno-long-long -Wmissing-declarations -Wmissing-prototypes -Wall -W -pedantic -L/usr/local/lib  -I/usr/X11R6/include -g -O2 -fno-strength-reduce -I/usr/include/gtk-1.2 -I/usr/X11R6/include -I/usr/include/glib-1.2 -I/usr/lib/glib/include   -DHAVE_CONFIG_H
CPPFLAGS := -I/usr/include/tcl8.4/tk-private/generic -I/usr/include/tcl8.4/tk-private/generic -I/usr/include/tcl8.4/tcl-private/unix -I/usr/include/tcl8.4/tcl-private/generic -I/usr/include/tcl8.4 -I/usr/include/tcl8.4  -I/usr/X11R6/include
LIBS := -ltk8.4 -ltcl8.4  -lSM -lICE  -L/usr/X11R6/lib   -lncurses -lX11 -lXaw -L/usr/X11R6/lib -lgtk -lgdk -lXi -lXext -lX11 -lm -lglib  
LDFLAGS :=  -L/usr/X11R6/lib 

prefix = /usr/local
exec_prefix = ${prefix}


bindir = ${exec_prefix}/bin
datadir = ${prefix}/share

DESTDIR = $(datadir)/games/zangband/
GAMEGROUP = games

CFLAGS += -DDEFAULT_PATH=\"$(DESTDIR)lib/\"


cygwin = 
TK_PORT = y

# subdir = $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
subdir = 
scandir = $(addprefix $(subdir),$(addsuffix /makefile.zb,$(dirs)))

files = angdos.cfg readme z_faq.txt z_update.txt

clean-files = zangband
distclean-files = *.bak gmon.out config.log config.status

srcfiles = bootstrap configure configure.in makefile makefile.in

##
## Default target
##
## Compile zangband.
##
default: zangband

INSTALL = 

dirs := lib src
dirlist := lib
include $(scandir)



##
## Default target
##
## Compile zangband.
##
zangband: $(objs-y)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(LIBS) $(DEFS)

##
## Hack - make sure the build system is consistant.
##
makefile: configure makefile.in
	./configure

configure: configure.in
	aclocal
	autoheader
	autoconf

dirs:
	-mkdir $(DESTDIR)
	-mkdir $(addprefix $(DESTDIR),$(filter-out $(srcdirlist), $(dirlist)))

installbase: dirs zangband
	for i in $(files) ; do \
	    cp $$i $(DESTDIR)$$i; \
	done
	$(INSTALL)

install: installbase
	cp zangband $(bindir)/zangband
	chgrp $(GAMEGROUP) $(bindir)/zangband
	chmod g+s $(bindir)/zangband

uninstall:
	-rm -f $(bindir)/zangband
	-rm -rf $(DESTDIR)

dist: installbase
	-mkdir $(addprefix $(DESTDIR),$(srcdirlist))
	for i in $(srcfiles) ; do \
	    cp $$i $(DESTDIR)$$i; \
	done

distcheck: dist
	cp -a $(DESTDIR) temp1
	mv $(DESTDIR) temp2
	cd temp1 && $(MAKE) dist
	diff -ur temp2 $(DESTDIR)
	-rm -rf temp1
	-rm -rf temp2
	-rm -rf $(DESTDIR)

VER_MAJOR := `grep "\#define VER_MAJOR" src/defines.h | sed s/\#define\ VER_MAJOR\ //`
VER_MINOR := `grep "\#define VER_MINOR" src/defines.h | sed s/\#define\ VER_MINOR\ //`
VER_PATCH := `grep "\#define VER_PATCH" src/defines.h | sed s/\#define\ VER_PATCH\ //`
VER_EXTRA := \
	`grep "\#define VER_EXTRA" src/defines.h | sed s/\#define\ VER_EXTRA\ /pre/ | sed s/pre0//`

VERSION := $(VER_MAJOR).$(VER_MINOR).$(VER_PATCH)$(VER_EXTRA)


distgz: dist
	cd $(datadir)/games && tar -cvf zangband.tar zangband
	mv $(datadir)/games/zangband.tar ./zangband-$(VERSION).tar
	gzip -9 zangband-$(VERSION).tar
	-rm -rf $(DESTDIR)

clean:
	-rm -f $(clean-files)

distclean: clean
	-rm -f $(distclean-files)
	
test:
	@echo I will build:
	@echo $(objs-y)
	@echo
	@echo I will clean:
	@echo $(clean-files)
	@echo
	@echo I will install:
	@echo $(dirlist) $(files)
	@echo
	@echo I will distinstall:
	@echo $(srcdirlist) $(srcfiles)

.PHONY: dirs installbase install uninstall dist distcheck distgz \
		 clean distclean test
