# File: Makefile.wat

# Purpose: Makefile support for "main-ibm.c" for Watcom C/C++

# From: akemi@netcom.com (David Boeren)
# Extra program targets by: mrmarcel@eos.ncsu.edu (Mike Marcelais)

CC = wcc386

CFLAGS  = /mf /3r /3 /wx /s /oneatx /DUSE_IBM /DUSE_WAT
# CFLAGS  = /mf /3r /3 /wx /oaeilmnrt /DUSE_IBM /DUSE_WAT

OBJS = &
  z-util.obj z-virt.obj z-form.obj z-rand.obj z-term.obj &
  variable.obj tables.obj util.obj cave.obj &
  object1.obj object2.obj monster1.obj monster2.obj &
  xtra1.obj xtra2.obj spells1.obj spells2.obj spells3.obj &
  melee1.obj melee2.obj load1.obj load2.obj save.obj files.obj &
  cmd1.obj cmd2.obj cmd3.obj cmd4.obj cmd5.obj cmd6.obj &
  store.obj birth.obj wizard1.obj wizard2.obj &
  generate.obj dungeon.obj init1.obj init2.obj &
  quest.obj mindcrft.obj mutation.obj artifact.obj &
  main-ibm.obj main.obj

all: Gumband.exe

Gumband.exe: $(OBJS) Gumband.lnk
   wlink system dos4g @Gumband.lnk

Gumband.lnk:
    %create  Gumband.lnk
#   @%append Gumband.lnk debug all
    @%append Gumband.lnk OPTION CASEEXACT
    @%append Gumband.lnk OPTION STACK=16k
    @%append Gumband.lnk name Gumband
    @for %i in ($(OBJS)) do @%append Gumband.lnk file %i

.c.obj:
    $(CC) $(CFLAGS) $[*.c

clean:
    del *.err *.obj *.exe *.lnk
