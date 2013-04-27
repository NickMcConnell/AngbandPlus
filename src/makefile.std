# File: Makefile

#
# Note that you may have to make modifications below according
# to your machine, available libraries, compilation options,
# and your "visual module" of choice.  This Makefile was originally
# intended for use with Unix machines running X11, Curses, Ncurses,
# or Vt100, or possibly for "Atari" or "Amiga" computers with
# "Curses" ports, see below for more information.  It was extended
# work on most ports by Steven Fuerst.  This makefile probably
# requires a recent version of gnu make in order to work.
#
#
# USAGE:  Simply type in "make portname" where portname is the name
#         of your port.  ie. "make openbsd".  The default port can
#         be changed by editing the DEFAULT variable.
#
#
# Note that "main-mac.c", the visual module for the Macintosh,
# must be compiled in a special way, see elsewhere.
#
# Note that "main-win.c", the visual module for Windows,
# might have to be compiled with another makefile (depending
# upon which compiler you use.)
#
# Note that "main-lsl.c", the visual module for linux svga,
# is depreciated.  Do not use this unless you feel like making
# a heap of new tiles.  It also is a security problem, since
# svgalib only works if you run as root.
#
# If you are able to construct "main-xxx.c" and/or "Makefile.xxx"
# files for a currently unsupported system, please send them to me
# (rr9@angband.org) for inclusion in future versions.
#


##
## The list of ports available
##
PORTS := default_port linux linux-svga linux-no-gtk openbsd freebsd main-cap \
		 vt100 solaris sgi dec isc next aix dos ibm borland gcu

OTHER_PORTS := amiga cygwin emx lcc linux-tk freebsd-tk mingw osx

##
## This is the default port to compile.
##
## Change this to your favourite port if you compile the game often.
##
DEFAULT := linux


##
## This is my compiler of choice, it seems to work most everywhere
##
CC := gcc


## Generic compile flags used by most ports.
CFLAGS := -Wall -O2 -g

## Variation
# CFLAGS := -O1 -g

##
## Default Lua location
##

LUA := lua/

##
## ##### Zangband Files to compile #####
##


##
## The wrapper auto-generated files
##

LUAWOBJS := \
	l-monst.o l-object.o l-player.o l-random.o l-ui.o \
	l-misc.o l-spell.o l-field.o

##
## The Zangband source files
##

ANGOBJS := $(LUAWOBJS) \
	variable.o tables.o util.o cave.o \
	object1.o object2.o monster1.o monster2.o \
	xtra1.o xtra2.o spells1.o spells2.o \
	melee1.o melee2.o save.o files.o fields.o\
	cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
	store.o birth.o load.o ui.o\
	wizard1.o wizard2.o grid.o streams.o rooms.o \
	generate.o dungeon.o init1.o init2.o \
	effects.o quest.o racial.o run.o script.o \
	artifact.o mutation.o flavor.o spells3.o \
	mspells1.o mspells2.o scores.o mind.o maid-x11.o\
	bldg.o obj_kind.o wild1.o wild2.o wild3.o avatar.o notes.o\
	main-cap.o main-gcu.o main-x11.o main-xaw.o main-xpj.o\
	main-lsl.o main-vcs.o main-gtk.o main-win.o main.o \
	maid-grf.o main-dos.o main-ibm.o main-emx.o \
	main-ami.o main-tnb.o

##
## The "Utility" files
##

ZUTILOBJS := z-util.o z-virt.o z-form.o z-rand.o z-term.o

##
## The Borg files
##

BORGOBJS := \
	zborg1.o zborg2.o zborg3.o zborg4.o zborg5.o \
	zborg6.o zborg7.o zborg8.o zborg9.o \
	zbmagic1.o zbmagic2.o zbmagic3.o


##
## The Tk port files
##
TKOBJS := \
	tk/cmdinfo.o tk/icon.o \
	tk/describe.o tk/interp.o tk/plat.o \
	tk/widget.o tk/tk-util.o tk/tcltk.o

##
## The "source" and "object" files.
##

##
## Lua Object Files
##

LUAOBJS := $(addprefix $(LUA), \
  lapi.o ldebug.o lmem.o lstrlib.o lvm.o \
  tolua_lb.o lauxlib.o ldo.o lobject.o ltable.o \
  lzio.o tolua_rg.o lbaselib.o lfunc.o lparser.o \
  ltests.o tolua_bd.o tolua_tm.o lcode.o lgc.o \
  lstate.o ltm.o tolua_eh.o tolua_tt.o ldblib.o \
  llex.o lstring.o lundump.o tolua_gp.o)


TOLUAOBJS := \
  $(addprefix $(LUA), tolua.o tolualua.o liolib.o) $(LUAOBJS)

##
## Get the list of .c files from the .o files
##

ANGSRCS := $(ANGOBJS:.o=.c)
ZUTILSRCS := $(ZUTILOBJS:.o=.c)
BORGSRCS := $(BORGOBJS:.o=.c)
LUAWSRCS := $(LUAWOBJS:.o=.c)

##
## Main list of objects and source files.
##

SRCS := $(ANGSRCS) $(ZUTILSRCS) $(BORGSRCS)

OBJS = $(ANGOBJS) $(ZUTILOBJS) $(BORGOBJS) $(LUAOBJS)


##
## Select the default port to use.
##
default_port: $(DEFAULT)

##
## (Installing directly is much nicer if you don't use the
## autoconf generated makefile system.)
##
## Do not call this directly - you need to specify a port.
## (This is why this isn't called "install".)
##
install_game: zangband
	$(INSTALL)


##
## Build the "ZAngband" program
##

MAKE-ZANGBAND = $(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(LIBS)

zangband: $(OBJS)
	$(MAKE-ZANGBAND)


##
## Following are some "system" definitions
##
## No changes are needed to compile a version that will run on both
## X11 and Curses, in debugging mode, with maximal warnings, on many
## normal Unix machines of the Sun OS variety (non-solaris).
##
##
## See also "z-config.h" and "h-config.h" for important information.
##

##
## You may have to add various X11 include/library directories to the
## "CFLAGS", if your machine places files in a weird location.
##
## You may be able to remove "-ltermcap" on some machines (ex: Solaris).
##
## You may have to replace "-lcurses" with "-lncurses" to use the
## "new curses" library instead of the "old curses" library, and
## you may have to add "-l/usr/include/ncurses" to the "CFLAGS".
##
## See "main-gcu.c" and "z-config.h" for some optional "curses" defines,
## including "USE_GETCH" and "USE_CURS_SET".  Note that "z-config.h" will
## attempt to "guess" at many of these flags based on your system.
##


##
## All 'Ports' depend on install_game so they get compiled.
## All 'Other-ports' must install themselves manually.
##
$(PORTS): install_game

##
## Hack - default install action.
## (This can be overridden below.)
##
INSTALL = cp zangband$(EXEC) ..

##
## Variation -- All the ports that work in Linux at once.
## (Use the -m option to start the one you want.)
##
##
## Graphics under X11 in linux.  (Run with the -g option)
## Note: Get the 16x16.bmp file, and put in [Z]directory/lib/xtra/graf
##       to get 256 colours.
##
linux: CFLAGS += -D"USE_GCU" -D"USE_VCS" -D"USE_XPJ" -D"USE_X11" \
                 -D"USE_XAW" `gtk-config --cflags` -D"USE_GTK" \
                 -pedantic -W -Wmissing-prototypes -Wmissing-declarations \
				 -Wno-long-long -Wwrite-strings -Wpointer-arith \
				 -Wbad-function-cast -Wsign-compare -DHAVE_MKSTEMP\
				 -Waggregate-return -Wstrict-prototypes \
				 -Wredundant-decls #-Wunreachable-code
 
linux: LIBS := -lX11 -lcurses -lncurses -L/usr/X11R6/lib \
               -lXaw -lXext -lSM -lICE -lXmu -lXt -lgtk `gtk-config --libs`

##
## Variation -- All X11 ports for linux except gtk
##
linux-no-gtk: CFLAGS += -D"USE_GCU" -D"USE_XPJ" -D"USE_X11" -D"USE_XAW" \
                 -pedantic -W -Wmissing-prototypes -Wmissing-declarations \
				 -Wno-long-long -Wwrite-strings -Wpointer-arith \
				 -Wbad-function-cast -Wsign-compare -DHAVE_MKSTEMP\
				 -Waggregate-return -Wstrict-prototypes \
				 -Wredundant-decls #-Wunreachable-code
 
linux-no-gtk: LIBS := -lX11 -lcurses -lncurses -L/usr/X11R6/lib \
               -lXaw -lXext -lSM -lICE -lXmu -lXt

##
## Variation -- tk port to linux
##
linux-tk: CFLAGS += -D"USE_TNB" -D"USE_X11" -D"PLATFORM_X11"\
				 -I/usr/include/tcl8.4 \
				 -I/usr/include/tcl8.4/tk-private/unix \
				 -I/usr/include/tcl8.4/tk-private/generic \
				 -I/usr/include/tcl8.4/tcl-private/generic \
                 -pedantic -W -Wmissing-prototypes -Wmissing-declarations \
				 -Wno-long-long -Wwrite-strings -Wpointer-arith \
				 -Wbad-function-cast -Wsign-compare -DHAVE_MKSTEMP\
				 -Waggregate-return -Wstrict-prototypes \
				 -Wredundant-decls #-Wunreachable-code
 
linux-tk: LIBS := -lX11 -lcurses -lncurses -L/usr/X11R6/lib \
               -lXaw -lXext -lSM -lICE -lXmu -lXt -ltcl8.4 -ltk8.4
linux-tk: tk-objs
	cp $^ ../zangband


##
## Linux with the (depreciated) svga port.
##
linux-svga: CFLAGS += -Wno-long-long -D"USE_GCU" -D"USE_VCS" -D"USE_XPJ"\
                 -D"USE_X11" -D"USE_XAW" -D"USE_LSL" \
                  `gtk-config --cflags` -D"USE_GTK" -DHAVE_MKSTEMP\
                 -pedantic -W -Wmissing-prototypes -Wmissing-declarations
 
linux-svga: LIBS := -lX11 -lcurses -lncurses -L/usr/X11R6/lib -lz -lvgagl \
                    -lvga -lXaw -lXext -lSM -lICE -lXmu -lXt \
					-lgtk `gtk-config --libs`


##
## Variation -- Cygwin
##
cygwin: CFLAGS += -W -pedantic -mno-cygwin -DWINDOWS
cygwin: LIBS := -s -mno-cygwin -mwindows -e _mainCRTStartup -lwinmm
cygwin: INSTALL := mv -f zangband.exe ../zangband.exe
cygwin: RES := windres
cygwin: RESCMD = $< -O coff -o $@
cygwin-objs: $(OBJS) angband.res readdib.o
	$(MAKE-ZANGBAND)
cygwin: cygwin-objs
	cp $^ ../zangband

##
## Variation -- Cross compile with mingw
##
mingw: MINGPREFIX := i586-mingw32msvc-
mingw: CC := $(MINGPREFIX)cc
mingw: LD := $(MINGPREFIX)ld
mingw: CFLAGS += -W -pedantic -mno-cygwin -DWINDOWS
mingw: LIBS := -s -mno-cygwin -mwindows -e _mainCRTStartup -lwinmm
mingw: INSTALL := mv -f zangband.exe ../zangband.exe
mingw: RES := $(MINGPREFIX)windres
mingw: RESCMD = $< -O coff -o $@
mingw-objs: $(OBJS) angband.res readdib.o
	$(MAKE-ZANGBAND)
mingw: mingw-objs
	cp $^ ../zangband



##
## Variation -- OpenBSD 
##

openbsd: CFLAGS += -I/usr/X11R6/include  -D"USE_X11" -D"USE_GCU"
openbsd: LIBS := -lX11 -lcurses -ltermcap -L/usr/X11R6/lib

##
## Variation -- FreeBSD
##
freebsd: CFLAGS += -D"USE_GCU" -D"USE_XPJ" -D"USE_X11" -D"USE_XAW" \
					`gtk12-config --cflags` -D"USE_GTK" \
				   -pedantic -W -Wmissing-prototypes -Wmissing-declarations

freebsd: LIBS := -lX11 -lncurses -L/usr/X11R6/lib \
				 -lXaw -lXext -lSM -lICE -lXmu -lXt `gtk12-config --libs`

##
## Variation -- tk port to FreeBSD
##
freebsd-tk: CFLAGS += -D"USE_TNB" -D"USE_X11" -D"PLATFORM_X11"\
				 -I/usr/local/include/tcl8.4/generic \
				 -I/usr/local/include/tk8.4/generic \
				 -I/usr/local/include/tk8.4/unix \
				 -I/usr/X11R6/include \
                 -pedantic -W -Wmissing-prototypes -Wmissing-declarations \
				 -Wno-long-long -Wwrite-strings -Wpointer-arith \
				 -Wbad-function-cast -Wsign-compare -DHAVE_MKSTEMP\
				 -Waggregate-return -Wstrict-prototypes \
				 -Wredundant-decls #-Wunreachable-code
 
freebsd-tk: LIBS := -lX11 -lcurses -lncurses -L/usr/X11R6/lib -L/usr/local/lib \
               -lXaw -lXext -lSM -lICE -lXmu -lXt -ltcl84 -ltk84
freebsd-tk: tk-objs
	cp $^ ../zangband

##
## Variation -- Use "main-cap.c" instead of "main-gcu.c"
##
main-cap: CFLAGS += -D"USE_X11" -D"USE_CAP"
main-cap: LIBS := -lX11 -ltermcap

##
## Variation -- Work without X installed
##
gcu: CFLAGS += -D"USE_GCU"
gcu: LIBS := -lcurses -lncurses


##
## Variation -- Only work on simple vt100 terminals
##
vt100: CFLAGS += -D"USE_CAP" -D"USE_HARDCODE"

##
## Variation -- compile for Solaris
##

solaris: CFLAGS += -D"USE_X11" -D"USE_GCU" -D"SOLARIS"
solaris: LIBS := -lX11 -lcurses -L/usr/openwin/lib

##
## Variation -- compile for SGI Indigo runnig Irix
##

sgi: CFLAGS += -D"USE_X11" -D"USE_GCU" -D"SGI"
sgi: LIBS := -lX11 -lcurses -ltermcap -lsun


##
## Variation -- compile for Dec ALPHA OSF/1 v2.0
##

dec: CFLAGS += -std -Olimit 4000 -D"USE_X11" -D"USE_GCU"
dec: LIBS := -lX11 -lcurses -ltermcap -lrpcsvc

##
## Variation -- compile for Interactive Unix (ISC) systems
##

isc: CFLAGS += -D"USE_X11" -D"USE_GCU" -D"ISC"
isc: LIBS := -lX11 -lcurses -lnsl_s -linet -lcposix

##
## Variation -- Support fat binaries under NEXTSTEP
##

next: CFLAGS += -D"USE_GCU" -arch m68k -arch i386
next: LIBS := -lcurses -ltermcap

##
## Variation -- compile for AIX 4.2.1 systems
## (Tested on an IBM SP2)
##

aix: CFLAGS += -bnoquiet -D"USE_X11" -D"SYS_V"
aix: LIBS := -lX11 -ltermcap -lcurses -lbsd -lXm -lXmu -lXaw -lXt

##
## Variation -- Dos using "main-dos.c" and the allegro library, with djgpp
##

dos: CFLAGS += -s -DUSE_DOS -DUSE_IBM -D"USE_BACKGROUND"
dos: LIBS := -lpc -lalleg
dos: EXEC := .exe
dos: INSTALL := copy zangband.exe ..
dos_clean:
	-del *.o
	-del lua\*.o
	-del l-*.c
	-del lua\tolua.exe

##
## Variation -- Compile using the main-ibm.c port with djgpp
##

ibm: CFLAGS += -s -DUSE_IBM
ibm: LIBS := -lpc
ibm: EXEC := .exe
ibm: INSTALL := copy zangband.exe ..
ibm_clean: dos_clean


##
## Variation -- Compile using the lcc32 compiler
## (This is untested and probably broken)
##

lcc: LCC_PATH := C:\lcc
lcc: CC	:= $(LCC_PATH)\bin\lcc.exe
lcc: LIBS := $(LCC_PATH)\lib\winmm.lib -I$(LCC_PATH)\include
lcc: LDFLAGS := -s -subsystem windows
lcc: RES := $(LCC_PATH)\bin\lrc.exe
lcc: RESCMD := -I$(LCC_PATH)\include angband.rc
lcc: LUA := $(subst /, \, $(LUA))
lcc: EXEC := .exe
lcc-objs: $(OBJS) angband.res readdib.o
	$(MAKE-ZANGBAND)
lcc: lcc-objs
	copy $^ ..\zangband.exe


##
## Variation -- Compile for OS2  (based on old emx makefile)
## Use 'dmake -B -r -f makefile.emx' to compile
## (see "main-emx.c" for details).
##

emx: CFLAGS := -MMD -O3 -DUSE_EMX -Zmt -Wall
emx: LDFLAGS := -lvideo
emx: AR := ar
emx: EXEC := .exe
emx-objs: $(OBJS) main-epm.o zangband.a
	$(MAKE-ZANGBAND)
emx: emx-objs
	copy $^.exe ..\zangband.exe 
	emxbind -s ..\zangband.exe
	copy aclient.exe ..
	emxbind -s ..\aclient.exe


##
## Special EMX dependancies
##

aclient.exe: main-emx.c
	$(CC) $(CFLAGS) -D__EMX__CLIENT__ -o $@ $< $(LDFLAGS)

main-epm.o: main-emx.c
	$(CC) $(CFLAGS) -DEMXPM -c -o $@ $<

zangband.a: $(OBJS)
	$(AR) r $@ $(OBJS)

##
## Variation -- Compile for Amiga
## (Untested - probably needs to be fixed)
##

amiga: LD := slink QUIET WITH zangband.lnk
amiga: CC := sc
amiga: CFLAGS := DEFINE AMIGA DEFINE USE_AMI
amiga-objs: $(OBJS) sound-ami.o
	$(MAKE-ZANGBAND)
amiga: amiga-objs
	cp $^ ../zangband #Fixme
	rm -f zangband


##
## Variation -- Compile using Borland C++ 5.5 (Win32)
## (Untested - may require lots of effort to get this to work)
##
borland: CC := bcc32
borland: LD := ilink32 -aa -x
borland: RES := brc32
borland: RESCMD := -r angband.rc
borland: OTHEROBJS := angband.res c0w32.obj
borland: CFLAGS := -Hc -tW -lGn -e$(EXE_FILE) -w- \
		-D_WIN32_WINNT=0x0400 -DWINVER=0x0400
borland: LDFLAGS := ,,
borland: LIBS := cw32.lib import32.lib
borland: INSTALL := copy zangband.exe ..
borland: EXEC := .exe

##
## Variation -- Compile using gcc on Mac OS X
##
osx osx-dist: TOOLDIR := /Developer/Tools
osx osx-dist: REZ := $(TOOLDIR)/Rez
osx osx-dist: SETFILE := $(TOOLDIR)/SetFile

osx: CFLAGS := -Wall -O2 -fpascal-strings -DMACH_O_CARBON \
               -DPRIVATE_USER_PATH=\"~/Library/Preferences\" -DUSE_PRIVATE_PATHS
osx: LIBS := -framework CoreFoundation -framework QuickTime -framework Carbon

# Application bundle -- a directory looking like an ordinary application

# Name of the game
osx osx-dist osx-install-tiles: APPNAME := ZAngband.app

# Bundle of the game (it must be a directory whose name ends in ".app")
osx osx-dist osx-install-tiles: APPBNDL := ../$(APPNAME)

# A bundle must have these subdirectories
osx osx-install-tiles: APPCONT := $(APPBNDL)/Contents
osx: APPBIN := $(APPCONT)/MacOS
osx osx-install-tiles: APPRES := $(APPCONT)/Resources

osx: ICONFILES := ZAngband.icns Save.icns Edit.icns Data.icns
osx: PLIST := ZAngband.xml
osx osx-dist: RESSRCS := ZAngband.r
osx osx-dist: SOUNDSRCS := Sound.r
osx osx-dist: RFLAGS := -i /Developer/Headers/FlatCarbon -d MACH_O_CARBON

osx: LIBFILES := \
  ../lib/edit/*.txt \
  ../lib/file/*.txt \
  ../lib/help/*.txt \
  ../lib/help/*.hlp \
  ../lib/pref/*.prf \
  ../lib/script/*.lua

osx-objs: $(OBJS) main-crb.o $(RESSRCS) $(ICONFILES) $(PLIST) $(LIBFILES)
	$(MAKE-ZANGBAND)
osx: osx-objs
	[ -d $(APPBNDL) ] || mkdir $(APPBNDL)
	[ -d $(APPCONT) ] || mkdir $(APPCONT)
	[ -d $(APPBIN) ] || mkdir $(APPBIN)
	[ -d $(APPRES) ] || mkdir $(APPRES)

	[ -d $(APPRES)/lib ] || mkdir $(APPRES)/lib
	[ -d $(APPRES)/lib/edit ] || mkdir $(APPRES)/lib/edit
	[ -d $(APPRES)/lib/file ] || mkdir $(APPRES)/lib/file
	[ -d $(APPRES)/lib/help ] || mkdir $(APPRES)/lib/help
	[ -d $(APPRES)/lib/pref ] || mkdir $(APPRES)/lib/pref
	[ -d $(APPRES)/lib/script ] || mkdir $(APPRES)/lib/script

	cp ../lib/edit/*.txt $(APPRES)/lib/edit
	cp ../lib/file/*.txt $(APPRES)/lib/file
	cp ../lib/help/*.txt $(APPRES)/lib/help
	cp ../lib/help/*.hlp $(APPRES)/lib/help
	cp ../lib/pref/*.prf $(APPRES)/lib/pref
	cp ../lib/script/*.lua $(APPRES)/lib/script

	install -m 755 $^ $(APPBIN)/zangband
	install -m 644 $(ICONFILES) $(APPRES)
	
	$(REZ) $(RFLAGS) -o $(APPRES)/zangband.rsrc $(RESSRCS)
	install -m 644 $(PLIST) $(APPCONT)/Info.plist
	$(SETFILE) -a B $(APPBNDL)

osx-install-tiles: TILES := 8x8.png 16x16.png 32x32.png
osx-install-tiles: osx $(TILES)
	install -m 644 $(TILES) $(APPRES)


osx-dist: PACKAGE_NAME := ZAngband-275-alpha1
osx-dist: osx osx-install-tiles $(SOUNDSRCS)
	-rm -rf disttemp
	-rm -f "$(PACKAGE_NAME)-osx.dmg"

	mkdir disttemp
	mkdir disttemp/Docs

	cp ../readme disttemp/Docs/ReadMe.txt
	cp ../z_faq.txt disttemp/Docs/FAQ.txt
	cp ../z_update.txt disttemp/Docs/Changes.txt

	cp -R -p "$(APPBNDL)" disttemp
	$(REZ) $(RFLAGS) -o disttemp/$(APPNAME)/Contents/Resources/zangband.rsrc $(RESSRCS) $(SOUNDSRCS)
	$(SETFILE) -a B disttemp/$(APPNAME)

	hdiutil create -fs HFS+ -volname $(PACKAGE_NAME) -srcfolder disttemp "$(PACKAGE_NAME)-osx.dmg"

	rm -rf disttemp

##########################################################

##
## Hack - GCC requires other flags
##
ifeq (gcc, $(findstring gcc, $(CC)))
    CFLAGS += -fno-strength-reduce
endif

##
## Clean up old junk
##

clean:
	-$(RM) *.bak *.o
	-$(RM) ./lua/*.bak ./lua/*.o
	-$(RM) ./l-*.c
	-$(RM) ./lua/tolua
	-$(RM) ./tk/*.bak ./tk/*.o
	-$(RM) *-objs zangband

##
## Generate dependencies automatically
##

depend:
	makedepend -D__MAKEDEPEND__ $(SRCS)


##
## Lua stuff
##

$(LUA)tolua: $(TOLUAOBJS)
	$(CC) $(CFLAGS) -o $(LUA)tolua$(EXEC) $(TOLUAOBJS) $(LDFLAGS) $(LIBS)

##
## Low-level Headers
##

HDRS := \
	h-basic.h \
	h-define.h h-type.h h-system.h h-config.h

##
## Angband includes
##

INCS := \
	angband.h \
	z-config.h defines.h types.h externs.h \
	z-term.h z-rand.h z-util.h z-virt.h z-form.h $(HDRS)

##
## Generic dependancy information
##

$(ANGOBJS): $(INCS)
$(ZUTILOBJS): $(HDRS)
$(BORGOBJS): $(INCS)

##
## Extra dependancies
##

cmd6.o: script.h
init1.c: init.h
init2.c: init.h script.h
generate.o: grid.h generate.h rooms.h streams.h
grid.o: grid.h generate.h
rooms.o: grid.h generate.h rooms.h
streams.o: grid.h generate.h
fields.o: grid.h script.h
main-ami.o: maid-grf.h
maid-grf.o: maid-grf.h
maid-x11.o: maid-grf.h
main-gtk.o: maid-grf.h
main-tnb.o: maid-grf.h
main-win.o: maid-grf.h
main-x11.o: maid-grf.h maid-x11.h
main-xaw.o: maid-grf.h maid-x11.h
main-xpj.o: maid-grf.h maid-x11.h
script.o: script.h
quest.o: wild.h grid.h
readdib.o: readdib.h
wild1.o: wild.h grid.h
wild2.o: wild.h grid.h
wild3.o: wild.h grid.h
wizard2.o: script.h
z-form.o: z-form.h z-util.h z-virt.h
z-rand.o: z-rand.h
z-term.o: z-term.h z-virt.h
z-util.o: z-util.h
z-virt.o: z-virt.h z-util.h

## Each borg file depends on the ones before it.
## Hack - the dependance on the .o files encodes this.

zborg1.o: zborg1.h maid-grf.h
zborg2.o: zborg1.h zborg2.h maid-grf.h
zborg3.o: zborg1.h zborg3.h maid-grf.h
zborg4.o: zborg1.h zborg2.h zborg3.h zborg4.h maid-grf.h
zborg5.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h maid-grf.h
zborg6.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h maid-grf.h \
			zbmagic.h
zborg7.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h zborg7.h \
			maid-grf.h
zborg8.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h zborg7.h \
			zborg8.h maid-grf.h
zborg9.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h zborg7.h \
			zborg8.h zborg9.h maid-grf.h
zbmagic1.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h maid-grf.h \
			zbmagic.h
zbmagic2.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h maid-grf.h \
			zbmagic.h
zbmagic3.o: zborg1.h zborg2.h zborg3.h zborg4.h zborg5.h zborg6.h maid-grf.h \
			zbmagic.h


##
## Compiling angband.rc
##
## Note: this format seems to work but the alternative actually used
## is the one recommended by Cygnus
##
## angband.res : angband.rc
##       $(WRES) angband.rc angband.res
##

angband.res : angband.rc
	$(RES) $(RESCMD)

##
## Build wrappers
##
## $(subst l-,,$*) removes the leading "l-", and
## trailing ".c" from the filename.
##

$(LUAWSRCS): %.c: %.pkg $(LUA)tolua$(EXEC)
	$(LUA)tolua -n $(subst l-,,$*) -o $@ $<

##
## Extra dependancies for the tk port
##
$(TKOBJS): $(INCS) tk/tnb.h
main-tnb.o: tk/tnb.h

TKICONOBJS := \
	tk/icon.o tk/interp.o

$(TKICONOBJS): tk/icon.h

##
## Compile the tk stuff
##
tk-objs: $(TKOBJS) $(OBJS)
	$(MAKE-ZANGBAND)

##
## Hack... .o files can be created from .obj files
##
%.o:%.obj
	move $@ $<

#LUALIST := basic.lua feature.lua verbatim.lua code.lua typedef.lua \
#	container.lua package.lua module.lua define.lua enumerate.lua \
#	declaration.lua variable.lua array.lua function.lua operator.lua \
#	class.lua clean.lua doit.lua
	
#LUALIST := $(addprefix $(LUA), $(LUALIST))

##
## We sometimes want to redo the lua stuff
##
## This doesn't work though...
##
#$(LUA)tolualua.c: $(LUALIST)
#	./tolua -n tolualua -o tolualua.c tolualua.pkg

##
## Phony targets
##
.PHONY: install_game clean depend $(PORTS) $(OTHER-PORTS)
