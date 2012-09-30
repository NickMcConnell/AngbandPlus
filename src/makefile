# File: Makefile.dos

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
  generate.o dungeon.o init1.o init2.o plots.o help.o \
  store.o birth.o wizard1.o wizard2.o bldg.o cmovie.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o cmd7.o \
  loadsave.o files.o levels.o notes.o squeltch.o \
  xtra1.o xtra2.o spells1.o spells2.o melee1.o melee2.o \
  object1.o object2.o traps.o monster1.o monster2.o monster3.o \
  variable.o tables.o util.o cave.o ghost.o wild.o powers.o \
  z-term.o z-rand.o z-form.o z-virt.o z-util.o

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

default: ../pernang.exe

release: ../pernang.exe
	upx -9 ../pernang.exe
#         copy pernang.exe ..
#         del pernang.exe

install: ../pernang.exe
#        copy pernang.exe ..

all: ../pernang.exe
#        @echo All done.  Use 'make install' to install.


#
# Link executables
#

../pernang.exe: $(OBJS)
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

cleanall: clean
	del *.exe

plots.o: q_rand.c q_main.c \
	q_thief.c q_hobbit.c q_nazgul.c q_troll.c q_wight.c \
	q_spider.c q_poison.c \
	q_eol.c q_nirna.c q_invas.c \
	q_betwen.c \
	q_narsil.c
