# File: Makefile.wat

# Purpose: Makefile support for "main-ibm.c" for Watcom C/C++

# From: akemi@netcom.com (David Boeren)
# Extra program targets by: michmarc@microsoft.com (Mike Marcelais)

CC = wcc386

# For Watcom v11
CFLAGS  = /mf /3r /3 /wx /s /oabhiklrsx /DUSE_IBM /DUSE_WAT

# For Watcom v10
# CFLAGS = /mf /3r /3 /wx /s /oneasx /DUSE_IBM /DUSE_WAT

# For debugging
# CFLAGS  = /mf /3r /3 /wx /d2 /od /DUSE_IBM /DUSE_WAT

OBJS = &
  z-util.obj z-virt.obj z-form.obj z-rand.obj z-term.obj &
  variable.obj tables.obj util.obj cave.obj &
  object1.obj object2.obj monster1.obj monster2.obj &
  xtra1.obj xtra2.obj spells1.obj spells2.obj melee1.obj melee2.obj &
  melee3.obj load2.obj save.obj files.obj init3.obj &
  cmd1.obj cmd2.obj cmd3.obj cmd4.obj cmd5.obj cmd6.obj &
  store.obj birth.obj wizard1.obj wizard2.obj &
  generate.obj dungeon.obj init1.obj init2.obj randart.obj &
  bldg.obj spells3.obj main-ibm.obj main.obj

all: kangband.exe gredit.exe makepref.exe

# Use whichever of these two you wish...
kangband.exe: $(OBJS) kangband.lnk
#   wlink system dos4g @kangband.lnk
   wlink system pmodew @kangband.lnk

# Use whichever of these two you wish...
gredit.exe: gredit.obj gredit.lnk
#   wlink system dos4g @gredit.lnk
   wlink system pmodew @gredit.lnk

# Use whichever of these two you wish...
makepref.exe: makepref.obj makepref.lnk
#   wlink system dos4g @makepref.lnk
   wlink system pmodew @makepref.lnk

kangband.lnk:
    %create  kangband.lnk
#   @%append kangband.lnk debug all
    @%append kangband.lnk OPTION CASEEXACT
    @%append kangband.lnk OPTION STACK=16k
    @%append kangband.lnk name kangband
    @for %i in ($(OBJS)) do @%append kangband.lnk file %i

makepref.lnk:
    %create  makepref.lnk
#   @%append makepref.lnk debug all
    @%append makepref.lnk OPTION CASEEXACT
    @%append makepref.lnk OPTION STACK=16k
    @%append makepref.lnk name makepref
    @%append makepref.lnk file makepref.obj

gredit.lnk:
    %create  gredit.lnk
#   @%append gredit.lnk debug all
    @%append gredit.lnk OPTION CASEEXACT
    @%append gredit.lnk OPTION STACK=16k
    @%append gredit.lnk name gredit
    @%append gredit.lnk file gredit.obj

.c.obj:
    $(CC) $(CFLAGS) $[*.c

clean:
    del *.err
    del *.obj
    del *.exe
    del *.lnk

