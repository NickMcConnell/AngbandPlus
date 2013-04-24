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
# main-dos removed; main-ibm terminal will hopefully replace.
# -KRP
OBJS = \
  main.o main-ibm.o \
  generate.o dungeon.o init1.o init2.o \
  store.o birth.o wizard1.o wizard2.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  load1.o load2.o save.o files.o \
  xtra1.o xtra2.o spells1.o spells2.o melee1.o melee2.o \
  object1.o object2.o monster1.o monster2.o \
  variable.o tables.o util.o cave.o \
  z-term.o z-rand.o z-form.o z-virt.o z-util.o

# Compiler
CC = gcc

# Compiler flags
# -DUSE_DOS removed -KRP 
CFLAGS = -Wall -O2 -s -DUSE_IBM

# Libraries
LIBS = -lpc


#
# Targets
#

default: tangband.exe
	copy angband.exe ..
	del angband.exe

#
# I have no idea what these lines do, so I'm commenting them out.
#       -KRP
#
#install: angband.exe makepref.exe gredit.exe
#        copy angband.exe ..
#        copy makepref.exe ..
#        copy gredit.exe ..
#
#all: angband.exe makepref.exe gredit.exe
#        @echo All done.  Use 'make install' to install.


#
# Link executables
#
# -lalleg removed -KRP

tangband.exe: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

#
# I don't know what this does either.  -KRP
#
# makepref.exe: makepref.o
#        $(CC) $(CFLAGS) -o $@ makepref.o $(LIBS)
#
# gredit.exe: gredit.o
#        $(CC) $(CFLAGS) -o $@ gredit.o $(LIBS) -lgrx20


#
# Compile source files
#

.c.o:
	$(CC) $(CFLAGS) -c $*.c


#
# Clean up
#
# I don't believe in cleaning. -KRP
#
# clean:
#        del *.o
#        del *.bin
#
# cleanall: clean
#       del *.exe

