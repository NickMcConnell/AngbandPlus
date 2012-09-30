# CVS: Last edit by $Author: rr9 $ on $Date: 2000/05/28 15:07:47 $
# File: Makefile.lsl

# Purpose: Makefile for Linux + SVGA library

SRCS = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c monster1.c monster2.c \
  xtra1.c xtra2.c spells1.c spells2.c \
  melee1.c melee2.c save.c files.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load.c \
  wizard1.c wizard2.c grid.c streams.c rooms.c \
  generate.c dungeon.c init1.c init2.c \
  effects.c racial.c notes.c \
  artifact.c mutation.c flavor.c spells3.c \
  mspells1.c mspells2.c scores.c mind.c \
  bldg.c obj_kind.c wild.c avatar.c\
  main-lsl.c main.c

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o monster1.o monster2.o \
  xtra1.o xtra2.o spells1.o spells2.o \
  melee1.o melee2.o save.o files.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o load.o \
  wizard1.o wizard2.o grid.o streams.o rooms.o \
  generate.o dungeon.o init1.o init2.o \
  effects.o racial.o notes.o \
  artifact.o mutation.o flavor.o spells3.o \
  mspells1.o mspells2.o scores.o mind.o \
  bldg.o obj_kind.o wild.o avatar.o\
  main-lsl.o main.o

CC = gcc

CFLAGS = -Wall -D"USE_LSL"
LIBS = -lz -lvgagl -lvga

#
# Hack -- "install" as the base target
#

install: testing
	cp testing ..


#
# Build the "Angband" program
#

testing: $(OBJS)
	$(CC) $(CFLAGS) -o testing $(OBJS) $(LDFLAGS) $(LIBS)


#
# Clean up old junk
#

clean:
	\rm *.bak *.o


#
# Generate dependencies automatically
#

depend:
	makedepend -D__MAKEDEPEND__ $(SRCS)


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
files.o: files.c $(INCS)
flavor.o: flavor.c $(INCS)
generate.o: generate.c $(INCS) generate.h grid.h rooms.h streams.h
grid.o: grid.c $(INCS) grid.h generate.h
rooms.o: rooms.c $(INCS) grid.h rooms.h generate.h
streams.o: streams.c $(INCS) grid.h generate.h
init1.o: init1.c $(INCS)
init2.o: init2.c $(INCS)
load.o: load.c $(INCS)
main-cap.o: main-cap.c $(INCS)
main-gcu.o: main-gcu.c $(INCS)
main-x11.o: main-x11.c $(INCS)
main-xaw.o: main-xaw.c $(INCS)
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
racial.o: racial.c $(INCS)
scores.o: scores.c $(INCS)
save.o: save.c $(INCS)
spells1.o: spells1.c $(INCS)
spells2.o: spells2.c $(INCS)
spells3.o: spells3.c $(INCS)
store.o: store.c $(INCS)
tables.o: tables.c $(INCS)
util.o: util.c $(INCS)
variable.o: variable.c $(INCS)
wild.o: wild.c $(INCS)
wizard1.o: wizard1.c $(INCS)
wizard2.o: wizard2.c $(INCS)
xtra1.o: xtra1.c $(INCS)
xtra2.o: xtra2.c $(INCS)
z-form.o: z-form.c $(HDRS) z-form.h z-util.h z-virt.h
z-rand.o: z-rand.c $(HDRS) z-rand.h
z-term.o: z-term.c $(HDRS) z-term.h z-virt.h
z-util.o: z-util.c $(HDRS) z-util.h
z-virt.o: z-virt.c $(HDRS) z-virt.h z-util.h

