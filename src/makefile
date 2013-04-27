# File: Makefile.ibm

# Purpose: Makefile support for "main-ibm.c" and Gnu C

# Note: Rename to "Makefile" before using

OBJS = \
  sys-dos/main.o sys-dos/irqwrap.o sys-dos/timer.o sys-dos/irq.o \
  birth.o cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o console.o debug.o \
  dungeon.o entity.o files.o generate.o gui.o init1.o init2.o living.o \
  load.o map.o melee1.o melee2.o monster.o mon-desc.o object.o obj-desc.o \
  player.o random.o save.o spells1.o spells2.o store.o tables.o util.o \
  variable.o xtra1.o xtra2.o z-form.o


# Compiler

CC = gcc

# Compiler flags

CFLAGS = -Wall -O2
SFLAGS = -Wall -O2

# Libraries

LIBS = -lpc
GLIBS= -lgrx20

# Name of Program

PROGRAM = utumno.exe


.SUFFIXES: .c .h .o .cpp .s

all: $(PROGRAM)

# Generate binary file

utumno.exe: $(OBJS)
	$(CC) $(CFLAGS) -o utumno.bin $(OBJS) $(LIBS)
	copy utumno.exe ..

# Compile source files

.cpp.o:
	$(CC) $(CFLAGS) -o $*.o -c $*.cpp


.s.o:
	$(CC) $(SFLAGS) -o $*.o -c $*.S

# Clean up

clean:
	del *.o $(PROGRAM) *.bin

