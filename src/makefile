# File: Makefile

# This is not a very "nice" Makefile, but it usually works.

#
# Note that you may have to make modifications below according
# to your machine, available libraries, compilation options,
# and your "visual module" of choice.  This Makefile is intended
# for use with Unix machines running X11, Curses, Ncurses, or Vt100,
# or possibly for "Atari" or "Amiga" computers with "Curses" ports,
# see below for more information.
#
# Note that "main-mac.c", the visual module for the Macintosh,
# must be compiled in a special way, see elsewhere.
#
# Note that "main-win.c", the visual module for Windows,
# must be compiled in a special way, see elsewhere.
#
# Note that "main-ibm.c", "main-emx.c", and "main-286.c",
# the visual modules for various types of IBM-PC computers,
# must be compiled with special Makefiles, see elsewhere.
#
# Note that "main-lsl.c", the visual module for linux svga,
# is depreciated.  Do not use this unless you feel like making
# a heap of new tiles.
#
# Note that "main-acn.c", the visual module for Risc Acorn,
# must be compiled with "Makefile.acn", see elsewhere.
#
# If you are able to construct "main-xxx.c" and/or "Makefile.xxx"
# files for a currently unsupported system, please send them to me
# (rr9@angband.org) for inclusion in future versions.
#
# This Makefile comes with "default" dependancies that may be obsolete.
#


LUAOBJS = \
  lua/lapi.o lua/ldebug.o lua/lmem.o lua/lstrlib.o lua/lvm.o \
  lua/tolua_lb.o lua/lauxlib.o lua/ldo.o lua/lobject.o lua/ltable.o \
  lua/lzio.o lua/tolua_rg.o lua/lbaselib.o lua/lfunc.o lua/lparser.o \
  lua/ltests.o lua/tolua_bd.o lua/tolua_tm.o lua/lcode.o lua/lgc.o \
  lua/lstate.o lua/ltm.o lua/tolua_eh.o lua/tolua_tt.o lua/ldblib.o \
  lua/llex.o lua/lstring.o lua/lundump.o lua/tolua_gp.o

TOLUAOBJS = \
  lua/tolua.o lua/tolualua.o lua/liolib.o \
  $(LUAOBJS)


#
# The "source" and "object" files.
#

SRCS = \
	z-util.c z-virt.c z-form.c z-rand.c z-term.c \
	variable.c tables.c util.c cave.c \
	object1.c object2.c monster1.c monster2.c \
	xtra1.c xtra2.c spells1.c spells2.c \
	melee1.c melee2.c save.c files.c fields.c\
	cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
	store.c birth.c load.c \
	wizard1.c wizard2.c grid.c streams.c rooms.c \
	generate.c dungeon.c init1.c init2.c \
	effects.c quest.c racial.c script.c \
	artifact.c mutation.c flavor.c spells3.c \
	mspells1.c mspells2.c scores.c mind.c maid-x11.c\
	bldg.c obj_kind.c wild1.c wild2.c avatar.c notes.c\
	main-cap.c main-gcu.c main-x11.c main-xaw.c main-xpj.c\
	l-misc.c l-spell.c \
	main-lsl.c main-vcs.c main-gtk.c main.c \
	l-monst.c l-object.c l-player.c l-random.c l-ui.c \
	l-misc.c l-spell.c

OBJS = \
	z-util.o z-virt.o z-form.o z-rand.o z-term.o \
	variable.o tables.o util.o cave.o \
	object1.o object2.o monster1.o monster2.o \
	xtra1.o xtra2.o spells1.o spells2.o \
	melee1.o melee2.o save.o files.o fields.o\
	cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
	store.o birth.o load.o \
	wizard1.o wizard2.o grid.o streams.o rooms.o \
	generate.o dungeon.o init1.o init2.o \
	effects.o quest.o racial.o script.o \
	artifact.o mutation.o flavor.o spells3.o \
	mspells1.o mspells2.o scores.o mind.o maid-x11.o\
	bldg.o obj_kind.o wild1.o wild2.o avatar.o notes.o\
	main-cap.o main-gcu.o main-x11.o main-xaw.o main-xpj.o\
	main-lsl.o main-vcs.o main-gtk.o main.o\
	l-monst.o l-object.o l-player.o l-random.o l-ui.o \
	l-misc.o l-spell.o \
	$(LUAOBJS)

##
## Following are some "system" definitions
##
## No changes are needed to compile a version that will run on both
## X11 and Curses, in debugging mode, with maximal warnings, on many
## normal Unix machines of the Sun OS variety (non-solaris).
##
## To use an "alternative" definition, simply "modify" (or "replace")
## the definition below with one that you like.  For example, you can
## change the compiler to "cc", or remove the "debugging" options, or
## remove the X11 or Curses support, etc, as desired.
##
## See also "z-config.h" and "h-config.h" for important information.
##
## Some "examples" are given below, they can be used by simply
## removing the FIRST column of "#" signs from the "block" of lines
## you wish to use, and commenting out "standard" block below.
##
## This is not intended to be a "good" Makefile, just a "simple" one.
##


#
# This is my compiler of choice, it seems to work most everywhere
#
CC = gcc-3.0

#
# Standard version (see main-x11.c and main-gcu.c)
#
# This version supports both "X11" and "curses" in a single executable.
#
# You may have to add various X11 include/library directories to the
# "CFLAGS", if your machine places files in a weird location.
#
# You may be able to remove "-ltermcap" on some machines (ex: Solaris).
#
# You may have to replace "-lcurses" with "-lncurses" to use the
# "new curses" library instead of the "old curses" library, and
# you may have to add "-l/usr/include/ncurses" to the "CFLAGS".
#
# See "main-gcu.c" and "z-config.h" for some optional "curses" defines,
# including "USE_GETCH" and "USE_CURS_SET".  Note that "z-config.h" will
# attempt to "guess" at many of these flags based on your system.
#

##
## Graphics under X11 in linux.  (Run with the -g option)
## Note: Get the 16x16.bmp file, and put in [Z]directory/lib/xtra/graf
##       to get 256 colours.
##
#CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g \
#         -D"USE_X11" -D"USE_GCU" -D"USE_VCS" -D"USE_TRANSPARENCY"
#LIBS = -lX11 -lcurses -ltermcap -L/usr/X11R6/lib

## Option: Use XAW instead of pure X11
#CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g\
#          -D"USE_XAW" -D"USE_GCU" -D"USE_VCS" -D"USE_TRANSPARENCY"
		
#LIBS = -L/usr/X11R6/lib -lXaw -lXext -lSM -lICE -lXmu -lXt\
#          -lX11 -lcurses -lncurses

##
## Variation - 3D Projected View
## (This requires the 16x16.bmp file to exist for it to work)
#
#CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g \
#         -D"USE_GCU" -D"USE_TRANSPARENCY" -D"USE_XPJ"
#		 
#LIBS = -lX11 -lcurses -lncurses -L/usr/X11R6/lib


##
## Variation -- All the ports that work in Linux at once.
## (Use the -m option to start the one you want.)
##
CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g  -D"USE_GCU" -D"USE_VCS"\
         -D"USE_TRANSPARENCY" -D"USE_XPJ" -D"USE_X11" -D"USE_XAW" -D"USE_LSL"\
         `gtk-config --cflags` -D"USE_GTK" \
		 -pedantic -W -Wmissing-prototypes -Wmissing-declarations
#		 
LIBS = -lX11 -lcurses -lncurses -L/usr/X11R6/lib -lz -lvgagl -lvga \
       -lXaw -lXext -lSM -lICE -lXmu -lXt -lgtk `gtk-config --libs`

##
## Variation -- OpenBSD 
##
#
#CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g -I/usr/x11r6/include \
#         -D"USE_X11" -D"USE_GCU" -D"USE_TRANSPARENCY"
#LIBS = -lX11 -lcurses -ltermcap -L/usr/X11R6/lib


##
## Variation -- Only support "main-x11.c" (not "main-gcu.c")
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_X11"
#LIBS = -lX11


##
## Variation -- Only support "main-gcu.c" (not "main-x11.c")
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_GCU"
#LIBS = -lcurses -ltermcap


##
## Variation -- Use "main-xaw.c" instead of "main-x11.c"
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_XAW" -D"USE_GCU"
#LIBS = -lXaw -lXmu -lXt -lX11 -lcurses -ltermcap


##
## Variation -- Use "main-gtk.c"
##
#CFLAGS = -Wall -O2 -fno-strength-reduce -pipe -g  -D"USE_GCU"\
#         -D"USE_TRANSPARENCY" `gtk-config --cflags` -D"USE_GTK" 
#		 
#LIBS = -lcurses -lncurses -L/usr/X11R6/lib \
#       -lgtk `gtk-config --libs`


##
## Variation -- Use "main-cap.c" instead of "main-gcu.c"
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_X11" -D"USE_CAP"
#LIBS = -lX11 -ltermcap


##
## Variation -- Only work on simple vt100 terminals
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_CAP" -D"USE_HARDCODE"


##
## Variation -- this might work for Linux 1.1.93 using ncurses-1.8.7.
##
#CFLAGS = -I/usr/X11R6/include -I/usr/include/ncurses \
#         -Wall -O2 -fno-strength-reduce -fomit-frame-pointer \
#         -D"USE_X11" -D"USE_GCU"
#LIBS = -L/usr/X11R6/lib -lX11 -lncurses -ltermcap

##
## Variation -- this might work better than the suggestion above
##
#CFLAGS = -I/usr/include/ncurses \
#         -Wall -O2 -fno-strength-reduce -fomit-frame-pointer \
#         -D"USE_X11" -D"USE_GCU" \
#         -D"USE_TPOSIX" -D"USE_CURS_SET"
#LIBS = -lX11 -lncurses
#LDFLAGS = -s


##
## Variation -- compile for Solaris
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_X11" -D"USE_GCU" -D"SOLARIS"
#LIBS = -lX11 -lsocket -lcurses


##
## Variation -- compile for SGI Indigo runnig Irix
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_X11" -D"USE_GCU" -D"SGI"
#LIBS = -lX11 -lcurses -ltermcap -lsun



##
## Variation -- compile for Dec ALPHA OSF/1 v2.0
##
#CC     = cc
##CFLAGS = -std -O -g3 -Olimit 4000 -D"USE_X11" -D"USE_GCU"
#CFLAGS = -std -g -D"USE_X11" -D"USE_GCU"
#LIBS   = -lX11 -lcurses -ltermcap -lrpcsvc


##
## Variation -- compile for Interactive Unix (ISC) systems
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_X11" -D"USE_GCU" -D"ISC"
#LIBS = -lX11 -lcurses -lnsl_s -linet -lcposix


##
## Variation -- Support fat binaries under NEXTSTEP
##
#CFLAGS = -Wall -O1 -pipe -g -D"USE_GCU" -arch m68k -arch i386
#LIBS = -lcurses -ltermcap


##
## Variation -- compile for AIX 4.2.1 systems
## (Tested on an IBM SP2)
##
#
#CC = cc
#CFLAGS = -bnoquiet -O2  -D"USE_X11" -D"SYS_V"
#LIBS = -lX11 -ltermcap -lcurses -lbsd -lXm -lXmu -lXaw -lXt


##
## Variation -- compile for Linux + SVGA Library.
## Do not use this unless you wnat to play as root in a console.
## The graphics is really out of date - this combination probably
## should not be used.  Does anyone feel like fixing main_lsl.c to
## use the normal scaled graphics?
#
#CFLAGS = -Wall -D"USE_LSL"
#LIBS = -lz -lvgagl -lvga

#
# Hack -- "install" as the base target
# (Installing is much nicer if you don't use the autoconf generated
# makefile system.)
#

install: zangband
	cp zangband ..

#
# Build the "ZAngband" program
#

zangband: $(OBJS)
	$(CC) $(CFLAGS) -o zangband $(OBJS) $(LDFLAGS) $(LIBS)



#
# Clean up old junk
#

clean:
	-rm -f *.bak *.o
	-rm -f ./lua/*.bak ./lua/*.o
	-rm -f ./l-*.c
	-rm -f ./lua/tolua
	-rm -f ./lua/tolua_boot

#
# Generate dependencies automatically
#

depend:
	makedepend -D__MAKEDEPEND__ $(SRCS)


#
# Lua stuff
#

#
# Bootstrap tolua 
#

#
# Lua stuff
#

lua/tolua: $(TOLUAOBJS)
	$(CC) -o lua/tolua $(TOLUAOBJS) $(LDFLAGS) $(LIBS)

#
# Hack -- some file dependencies
#

HDRS = \
	h-basic.h \
	h-define.h h-type.h h-system.h h-config.h

INCS = \
	angband.h \
	z-config.h defines.h types.h externs.h \
	z-term.h z-rand.h z-util.h z-virt.h z-form.h $(HDRS)


artifact.o: artifact.c $(INCS)
avatar.o: avatar.c $(INCS)
bldg.o: bldg.c $(INCS)
birth.o: birth.c $(INCS)
cave.o: cave.c $(INCS)
cmd1.o: cmd1.c $(INCS)
cmd2.o: cmd2.c $(INCS)
cmd3.o: cmd3.c $(INCS)
cmd4.o: cmd4.c $(INCS)
cmd5.o: cmd5.c $(INCS)
cmd6.o: cmd6.c $(INCS)
dungeon.o: dungeon.c $(INCS)
effects.o: effects.c $(INCS)
fields.o: fields.c $(INCS)
files.o: files.c $(INCS)
flavor.o: flavor.c $(INCS)
generate.o: generate.c $(INCS) generate.h grid.h rooms.h streams.h
grid.o: grid.c $(INCS) grid.h generate.h
rooms.o: rooms.c $(INCS) grid.h rooms.h generate.h
streams.o: streams.c $(INCS) grid.h generate.h
init1.o: init1.c $(INCS)
init2.o: init2.c $(INCS)
load.o: load.c $(INCS)
maid-x11.o: maid-x11.c $(INCS)
main-cap.o: main-cap.c $(INCS)
main-gtk.o: main-gtk.c $(INCS)
main-gcu.o: main-gcu.c $(INCS)
main-lsl.o: main-lsl.c $(INCS)
main-vcs.o: main-vcs.c $(INCS)
main-x11.o: main-x11.c $(INCS) maid-x11.h
main-xaw.o: main-xaw.c $(INCS) maid-x11.h
main-xpj.o: main-xpj.c $(INCS) maid-x11.h
main.o: main.c $(INCS)
melee1.o: melee1.c $(INCS)
melee2.o: melee2.c $(INCS)
mind.o: mind.c $(INCS)
monster1.o: monster1.c $(INCS)
monster2.o: monster2.c $(INCS)
mspells1.o: mspells1.c $(INCS)
mspells2.o: mspells2.c $(INCS)
mutation.o: mutation.c $(INCS)
notes.o: notes.c $(INCS)
object1.o: object1.c $(INCS)
object2.o: object2.c $(INCS)
obj_kind.o: obj_kind.c $(INCS)
quest.o: quest.c $(INCS) wild.h grid.h
racial.o: racial.c $(INCS)
save.o: save.c $(INCS)
scores.o: scores.c $(INCS)
script.o: script.c $(INCS)
spells1.o: spells1.c $(INCS)
spells2.o: spells2.c $(INCS)
spells3.o: spells3.c $(INCS)
store.o: store.c $(INCS)
tables.o: tables.c $(INCS)
util.o: util.c $(INCS)
variable.o: variable.c $(INCS)
wild1.o: wild1.c $(INCS) wild.h grid.h
wild2.o: wild2.c $(INCS) wild.h grid.h
wizard1.o: wizard1.c $(INCS)
wizard2.o: wizard2.c $(INCS)
xtra1.o: xtra1.c $(INCS)
xtra2.o: xtra2.c $(INCS)
z-form.o: z-form.c $(HDRS) z-form.h z-util.h z-virt.h
z-rand.o: z-rand.c $(HDRS) z-rand.h
z-term.o: z-term.c $(HDRS) z-term.h z-virt.h
z-util.o: z-util.c $(HDRS) z-util.h
z-virt.o: z-virt.c $(HDRS) z-virt.h z-util.h

#
# Build wrappers
#
# (This probably should be cleaned up a little.)
#

l-monst.c: lua/tolua l-monst.pkg
	lua/tolua -n monster -o l-monst.c l-monst.pkg

l-object.c: lua/tolua l-object.pkg
	lua/tolua -n object -o l-object.c l-object.pkg

l-player.c: lua/tolua l-player.pkg
	lua/tolua -n player -o l-player.c l-player.pkg

l-random.c: lua/tolua l-random.pkg
	lua/tolua -n random -o l-random.c l-random.pkg

l-ui.c: lua/tolua l-ui.pkg
	lua/tolua -n ui -o l-ui.c l-ui.pkg
	
l-misc.c: lua/tolua l-misc.pkg
	lua/tolua -n misc -o l-misc.c l-misc.pkg

l-spell.c: lua/tolua l-spell.pkg
	lua/tolua -n spell -o l-spell.c l-spell.pkg
