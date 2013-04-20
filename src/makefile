# File: Makefile.ibm

# Purpose: Makefile support for "main-ibm.c" and Gnu C

# Note: Rename to "Makefile" before using
# Added extra targets: mrmarcel@eos.ncsu.edu (Mike Marcelais)

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o monster1.o monster2.o \
  xtra1.o xtra2.o spells1.o spells2.o spells3.o \
  melee1.o melee2.o load1.o load2.o save.o files.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o wizard1.o wizard2.o \
  generate.o dungeon.o init1.o init2.o \
  quest.o mindcrft.o mutation.o artifact.o \
  main-ibm.o main.o


# Compiler

CC = gcc

# Compiler flags

CFLAGS = -W -Wall -pedantic -O2 -DUSE_IBM
#CFLAGS = -W -Wall -pedantic -g -DUSE_IBM

# Libraries

LIBS = -lpc
GLIBS= -lgrx20

# Name of Program

PROGRAM = gumband.exe

default: gumband.exe
	copy gumband.exe ..
	del gumband.exe
	del gumband.bin

install: gumband.exe
	copy gumband.exe ..

all: $(PROGRAM)

# Generate binary file

gumband.exe: $(OBJS)
	$(CC) $(CFLAGS) -o gumband.bin $(OBJS) $(LIBS)

# Compile source files

.c.o:
	$(CC) $(CFLAGS) -c $*.c


# Clean up

clean:
	del *.o $(PROGRAM) *.bin
