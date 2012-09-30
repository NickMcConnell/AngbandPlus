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

CC_AUX := gcc

CFLAGS := -Wsign-compare -Wnested-externs -Wundef -Wuninitialized -Wunused -Wswitch -Wreturn-type -Wsequence-point -Wparentheses -Wimplicit -Wchar-subscripts -Wredundant-decls -Wstrict-prototypes -Waggregate-return -Wbad-function-cast -Wpointer-arith -Wwrite-strings -Wno-long-long -Wmissing-declarations -Wmissing-prototypes -Wall -W -pedantic -g -O2 -I/usr/include/gtk-1.2 -I/usr/include/glib-1.2 -I/usr/lib/glib/include -I/usr/X11R6/include -DHAVE_CONFIG_H
CPPFLAGS := 
LIBS := -lz -lrpcsvc -lbsd -ltk8.4 -ltcl8.4 -lncurses -lXaw -lICE -lXpm -lSM -lXt -lXmu -lXm -lXext 
LDFLAGS :=  -L/usr/lib -L/usr/X11R6/lib -lgtk -lgdk -rdynamic -lgmodule -lglib -ldl -lXi -lXext -lX11 -lm

prefix = /usr/local
exec_prefix = ${prefix}


bindir = ${exec_prefix}/bin
datadir = ${prefix}/share

DESTDIR = $(datadir)/games/zangband/
GAMEGROUP = games

CFLAGS += -DDEFAULT_PATH=\"$(DESTDIR)lib/\"

#
# Default verbose settings
#
ifdef V
  ifeq ("$(origin V)", "command line")
    VERBOSE = $(V)
  endif
endif
ifndef VERBOSE
  VERBOSE = 0
endif

#
# Standard configuration method.
#
define CONFIGURE
	if [ -x ./config.status ] ; then \
	    ./config.status --recheck && ./config.status; \
	else \
		./configure; \
	fi
endef

cygwin = 
TK_PORT = y

# subdir = $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
subdir = 
scandir = $(addprefix $(subdir),$(addsuffix /makefile.zb,$(dirs)))

files = angdos.cfg readme z_faq.txt z_update.txt

clean-files = zangband .default_path
distclean-files = *.bak gmon.out config.log config.status

srcfiles = configure configure.in makefile makefile.in

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


ifeq ($(VERBOSE), 0)
define CC
	@ echo CC $@
	@ $(CC_AUX) 
endef
define LINK
	@ echo LINK $@
	@ $(CC_AUX)
endef
else
CC = $(CC_AUX)
LINK = $(CC_AUX)
endif



##
## Default target
##
## Compile zangband.
##
default: zangband .makefiles

zangband: $(objs-y)
	$(LINK) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(LIBS) $(DEFS)

##
## .c files
##
%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DEFS) $< -c -o $@ $(DEFS)

##
## Hack - make sure the build system is consistant.
##
makefile: configure makefile.in
	$(CONFIGURE)
	
configure: configure.in
	aclocal
	autoheader
	autoconf

dirs:
	-mkdir $(DESTDIR)
	-mkdir $(addprefix $(DESTDIR),$(filter-out $(srcdirlist), $(dirlist)))

base: dirs
	for i in $(files) ; do \
	    cp $$i $(DESTDIR)$$i; \
	done
	
installbase: base
	$(INSTALL)

install: installbase zangband
	cp zangband $(bindir)/zangband
	if [ -n "$(GAMEGROUP)" ] ; then \
		chgrp $(GAMEGROUP) $(bindir)/zangband; \
		chmod g+s $(bindir)/zangband; \
	fi

uninstall:
	-rm -f $(bindir)/zangband
	-rm -rf $(DESTDIR)

dist: base
	-mkdir $(addprefix $(DESTDIR),$(srcdirlist))
	for i in $(srcfiles) ; do \
		cp $$i $(DESTDIR)$$i; \
	done
	-for i in $(notsrcfiles) ; do \
		rm $(DESTDIR)$$i; \
	done

	chmod -R +rw $(DESTDIR)

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
VER_AFTER := \
	`grep "\#define VER_AFTER" src/defines.h | sed s/\#define\ VER_AFTER\ // | sed s/\"//g`

#
# Expand now, so don't have quoting problems
#
VERSION := $(shell echo $(VER_MAJOR).$(VER_MINOR).$(VER_PATCH)$(VER_EXTRA)$(VER_AFTER))


distgz: dist
	cd $(datadir)/games && tar -cvf zangband.tar zangband
	mv $(datadir)/games/zangband.tar ./zangband-$(VERSION).tar
	gzip -9 zangband-$(VERSION).tar
	-rm -rf $(DESTDIR)

# Minor cleaning
dust:
	-rm -f $(dust-files)
	
clean: dust
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

# Hack to remake files depending on DEFAULT_PATH
.default_path: makefile
	@if [ ! -r .default_path ]; then \
		echo "$(DESTDIR)" > .default_path; \
	fi
	@if [ x"$(DESTDIR)" != x`cat .default_path` ]; then \
		echo "$(DESTDIR)" > .default_path; \
	fi

# Hack to remake configure.in if the version number changes
.version: src/defines.h
	@if [ ! -r .version ]; then \
		echo "$(VERSION)" > .version; \
	fi
	@if [ x"$(VERSION)" != x`cat .version` ]; then \
		echo "$(VERSION)" > .version; \
	fi

configure.in: .version
	cat configure.in | sed -e "s/AC_INIT.*$$/AC_INIT\(Zangband,\ `cat .version`,\ bugs@zangband.org\)/" > configure.new
	mv configure.new configure.in
		
# Test to see that all source files are in the other makefiles.
.makefiles: $(makefiles)
	for i in $(notdir $(NORMOBJS)) ; do \
		for j in $(makefiles) ; do \
			if [ x"`grep $$i $$j | sed 1q`" == x ]; then \
				echo "Error: makefile $$j has $$i missing"; \
				k="stop"; \
			fi; \
		done; \
	done; \
	if [ "x$$k" == "xstop" ]; then \
		exit 1; \
	fi; \
	touch .makefiles;


.PHONY: dirs base installbase install uninstall dist distcheck distgz \
		 dust clean distclean test
