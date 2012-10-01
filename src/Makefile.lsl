# File: Makefile.lsl

# Purpose: Makefile for Linux + SVGA library

SRCS = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c info.c \
  object1.c object2.c monster1.c monster2.c \
  xtra1.c xtra2.c spells1.c spells2.c \
  monmove.c monattk.c save.c files.c randart.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load.c \
  attack.c wizard1.c wizard2.c squlech.c \
  generate.c dungeon.c init1.c init2.c \
  main-ami.c main.c

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o info.o \
  object1.o object2.o monster1.o monster2.o \
  xtra1.o xtra2.o spells1.o spells2.o \
  monmove.o monattk.o save.o files.o randart.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o load.o \
  attack.o wizard1.o wizard2.o squelch.o \
  generate.o dungeon.o init1.o init2.o \
  main-lsl.o main.o

CC = gcc

CFLAGS = -Wall -O6 -D"USE_LSL"
LIBS = -lvgagl -lvga

# Build the program

angsvga: $(SRCS) $(OBJS)
	$(CC) $(CFLAGS)  -o oangband $(OBJS) $(LDFLAGS) $(LIBS)

