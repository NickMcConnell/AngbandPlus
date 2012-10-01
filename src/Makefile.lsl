# File: Makefile.lsl

# Purpose: Makefile for Linux + SVGA library

SRCS = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c monster1.c monster2.c monster3.c\
  xtra1.c xtra2.c effects.c spells1.c spells2.c \
  melee1.c melee2.c save.c files.c squelch.c\
  cmd-attk.c cmd-book.c cmd-item.c cmd-know.c cmd-misc.c cmd-util.c \
  store.c birth.c load.c quest.c powers.c \
  wizard.c info.c traps.c \
  generate.c dungeon.c init1.c init2.c randart.c \
  main-lsl.c main.c

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o monster1.o monster2.o monster3.o\
  xtra1.o xtra2.o effects.o spells1.o spells2.o \
  melee1.o melee2.o save.o files.o squelch.o\
  cmd-attk.o cmd-book.o cmd-item.o cmd-know.o cmd-misc.o cmd-util.o \
  store.o birth.o load.o quest.o powers.o \
  wizard.o info.o traps.o \
  generate.o dungeon.o init1.o init2.o randart.o \
  main-lsl.o main.o

CC = gcc

CFLAGS = -O2 -fno-strength-reduce -Wall -D"USE_LSL"

LIBS = -lz -lvgagl -lvga



# 
# Build the "Angband" program 
# 
angband: $(OBJS) 
	$(CC) $(CFLAGS) -o testing $(OBJS) $(LDFLAGS) $(LIBS) 


#
# install Angband
#
install: angband
	cp angband ..

#
# Clean up old junk
#
clean:
	\rm *.o angband


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
  config.h defines.h types.h externs.h \
  z-term.h z-rand.h z-util.h z-virt.h z-form.h $(HDRS)


birth.o: birth.c $(INCS)
cave.o: cave.c $(INCS)
cmd-attk.o: cmd-attk.c $(INCS)
cmd-book.o: cmd-book.c $(INCS)
cmd-item.o: cmd-item.c $(INCS)
cmd-know.o: cmd-know.c $(INCS)
cmd-misc.o: cmd-misc.c $(INCS)
cmd-util.o: cmd-util.c $(INCS)
dungeon.o: dungeon.c $(INCS)
effects.o: effects.c $(INCS)
files.o: files.c $(INCS)
generate.o: generate.c $(INCS)
info.o: info.c $(INCS)
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
monster1.o: monster1.c $(INCS)
monster2.o: monster2.c $(INCS)
monster3.o: monster3.c $(INCS)
object1.o: object1.c $(INCS)
object2.o: object2.c $(INCS)
powers.o: powers.c $(INCS)
quest.o: quest.c $(INCS)
randart.o: randart.c $(INCS)
save.o: save.c $(INCS)
spells1.o: spells1.c $(INCS)
spells2.o: spells2.c $(INCS)
squelch.o: squelch.c $(INCS)
store.o: store.c $(INCS)
tables.o: tables.c $(INCS)
traps.o: traps.c $(INCS)
util.o: util.c $(INCS)
variable.o: variable.c $(INCS)
wizard.o: wizard.c $(INCS)
xtra1.o: xtra1.c $(INCS)
xtra2.o: xtra2.c $(INCS)
z-form.o: z-form.c $(HDRS) z-form.h z-util.h z-virt.h
z-rand.o: z-rand.c $(HDRS) z-rand.h
z-term.o: z-term.c $(HDRS) z-term.h z-virt.h
z-util.o: z-util.c $(HDRS) z-util.h
z-virt.o: z-virt.c $(HDRS) z-virt.h z-util.h
