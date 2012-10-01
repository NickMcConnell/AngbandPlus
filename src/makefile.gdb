# File: Makefile.dos
# By DarkGod, to create a pernang.bin to be used with gdb

# Purpose: Makefile support for "main-dos.c"

#
# Note: Rename to "Makefile" before using
#
# Allegro support by Robert Ruehlmann (rr9@angband.org)
#

# Compiling with MOD-file support:
# - Get the JG-MOD library from http://www.jgmod.home.ml.org and install it.
# - Insert -ljgmod in front of -lalleg to the Libraries section.
# - Add -DUSE_MOD_FILES to the compiler flags.
# - Copy your MOD-files into the "lib/xtra/music" folder.


#
# Basic definitions
#

# Objects
OBJS = \
  main.o main-dos.o main-ibm.o \
  generate.o dungeon.o init1.o init2.o \
  store.o birth.o wizard1.o wizard2.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o cmd7.o \
  load2.o save.o files.o levels.o notes.o \
  xtra1.o xtra2.o spells1.o spells2.o melee1.o melee2.o \
  object1.o object2.o traps.o monster1.o monster2.o \
  variable.o tables.o util.o cave.o \
  z-term.o z-rand.o z-form.o z-virt.o z-util.o \
  bldg.o eventmod.o iomod.o cavemod.o monmod.o playmod.o \
  miscmod.o kindmod.o objmod.o spellmod.o questmod.o racemod.o \
  dunmod.o

# Compiler
CC = gcc

# Compiler flags
CFLAGS = -Wall -g -DUSE_DOS -DUSE_IBM -DUSE_BACKGROUND \
-DUSE_TRANSPARENCY -DUSE_PYTHON


# Libraries
LIBS = -lpc -lalleg -lpython -lparser -lobjects


#
# Targets
#

default: ../pernang.bin
#         copy pernang.bin ..
#         del pernang.bin

install: ../pernang.bin
#        copy pernang.bin ..

all: ../pernang.bin
#        @echo All done.  Use 'make install' to install.


#
# Link executables
#

../pernang.bin: $(OBJS)
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


