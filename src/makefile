# File: Makefile.dos

#
# Makefile support for "main-dos.c" and "main-ibm.c"
#
# See "main-dos.c" and "main-ibm.c" for more information.
#


#
# Basic definitions
#

# Objects
OBJS = \
  main.o main-dos.o main-ibm.o \
  generate.o dungeon.o init1.o init2.o \
  store.o birth.o wizard1.o wizard2.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  load1.o load2.o save.o files.o \
  xtra1.o xtra2.o spells1.o spells2.o melee1.o melee2.o \
  object1.o object2.o monster1.o monster2.o \
  variable.o tables.o util.o cave.o \
  z-term.o z-rand.o z-form.o z-virt.o z-util.o \
  borg1.o borg2.o borg3.o borg4.o borg5.o \
  borg6.o borg7.o borg8.o borg9.o \
  randart.o

# Compiler
CC = gcc

# Compiler flags
CFLAGS = -Wall -O2 -s -DUSE_DOS -DUSE_IBM -DUSE_BACKGROUND \
-DUSE_TRANSPARENCY

# Libraries
LIBS = -lpc -lalleg


#
# Targets
#

default: angband.exe
	copy angband.exe ..
	del angband.exe

install: angband.exe
	copy angband.exe ..

all: angband.exe
	@echo All done.  Use 'make install' to install.


#
# Link executables
#

angband.exe: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)


#
# Compile source files
#

.c.o:
	$(CC) $(CFLAGS) -c $*.c


#
# Clean up
#

clean:
	del *.o
	del *.bin

cleanall: clean
	del *.exe

