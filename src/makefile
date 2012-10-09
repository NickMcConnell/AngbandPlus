# File: Makefile
#
# this makefile is for compiling the sources under Linux
#

#
# The "source" and "object" files.
#

SRCS = \
  compres2.c z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c object3.c object4.c monster1.c monster2.c monster3.c \
  xtra1.c xtra2.c spells1.c spells2.c \
  melee1.c melee2.c save.c files.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load.c gentown.c special.c \
  wizard1.c wizard2.c \
  generate.c dungeon.c init1.c init2.c \
  main-gcu.c main-x11.c main.c

SRCS_UNCHECKED = \
  compres1.c

OBJS = \
  compres2.o z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o object3.o object4.o monster1.o monster2.o monster3.o \
  xtra1.o xtra2.o spells1.o spells2.o \
  melee1.o melee2.o save.o files.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o load.o gentown.o special.o \
  wizard1.o wizard2.o \
  generate.o dungeon.o init1.o init2.o \
  main-gcu.o main-x11.o main.o

CC = gcc
CFLAGS = -Wall -g -DUSE_X11 -DUSE_GCU -DUSG $(EXTRAFLAGS)
ifeq "$(EXTRAFLAGS)" "-DMAKE_DIST"
C_CHECK_FLAGS = -O2
else
C_CHECK_FLAGS = -fbounds-checking
endif
CFLAGS_NOCHECK = -Wall -g -O3 -DUSE_X11 -DUSE_GCU -DUSG $(EXTRAFLAGS)

LIBS = -L/usr/X11R6/lib -L. -lXmu -lXt -lX11 -lncurses -lbfd -liberty

#
# Build the "Angband" program
#

install: angbnd64

angbnd64: depend.gnu $(OBJS) compres1.o
	$(CC) $(CFLAGS) $(C_CHECK_FLAGS) -o angbnd64 $(OBJS) compres1.o $(LIBS)

compres1.o: compres1.c
	$(CC) $(CFLAGS_NOCHECK) -c compres1.c

.c.o: depend.gnu
	$(CC) $(CFLAGS) $(C_CHECK_FLAGS) -c $<

depend.gnu:
	$(CC) -M $(SRCS) $(SRCS_UNCKECKED) > depend.gnu

ifeq (depend.gnu, $(wildcard depend.gnu))
  include depend.gnu
endif
