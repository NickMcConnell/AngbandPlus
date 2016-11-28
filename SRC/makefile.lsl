# CVS: Last edit by $Author: remco $ on $Date: 1999/11/14 14:25:49 $
# File: Makefile.lsl

# Purpose: Makefile for Linux + SVGA library

SRCS = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c monster1.c monster2.c \
  xtra1.c xtra2.c xtra3.c trap.c spells1.c spells2.c \
  melee1.c melee2.c save.c files.c xtra3.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load2.c \
  wizard1.c wizard2.c \
  generate.c dungeon.c init1.c init2.c \
  bldg.c \
  main-lsl.c main.c

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o monster1.o monster2.o \
  xtra1.o xtra2.o xtra3.o trap.o spells1.o spells2.o \
  melee1.o melee2.o save.o files.o xtra3.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o load2.o \
  wizard1.o wizard2.o \
  generate.o dungeon.o init1.o init2.o \
  bldg.o \
  main-lsl.o main.o

CC = gcc

CFLAGS = -Wall -D"USE_LSL"
LIBS = -lz -lvgagl -lvga

# Build the program

angsvga: $(SRCS) $(OBJS)
	$(CC) $(CFLAGS)  -o angband $(OBJS) $(LDFLAGS) $(LIBS)

