# File: Makefile.wat

# Purpose: Makefile support for "main-ibm.c" for Watcom C/C++

CC = wcc386

# For Watcom v11
CFLAGS  = /mf /3r /3 /wx /s /oabhiklrsx /DUSE_IBM /DUSE_WAT

# For Watcom v10
# CFLAGS = /mf /3r /3 /wx /s /oneasx /DUSE_IBM /DUSE_WAT

# For debugging
# CFLAGS  = /mf /3r /3 /wx /d2 /od /DUSE_IBM /DUSE_WAT

OBJS = &
  main.obj &
  main-ibm.obj &
  birth.obj &
  cave.obj &
  cmd-attk.obj &
  cmd-book.obj &
  cmd-item.obj &
  cmd-know.obj &
  cmd-misc.obj &
  cmd-util.obj &
  dungeon.obj &
  effects.obj &
  files.obj &
  generate.obj &
  info.obj &
  init1.obj &
  init2.obj &
  load.obj &
  melee1.obj &
  melee2.obj &
  monster1.obj &
  monster2.obj &
  monster3.obj &
  object1.obj &
  object2.obj &
  powers.obj &
  quest.obj &
  save.obj &
  spells1.obj &
  spells2.obj &
  squelch.obj &
  store.obj &
  tables.obj &
  traps.obj &
  util.obj &
  variable.obj &
  wizard.obj &
  xtra1.obj &
  xtra2.obj &
  z-form.obj &
  z-rand.obj &
  z-term.obj &
  z-util.obj &
  z-virt.obj 

all: angband.exe

# Use whichever of these two you wish...
angband.exe: $(OBJS) angband.lnk
#   wlink system dos4g @angband.lnk
   wlink system pmodew @angband.lnk

angband.lnk:
    %create  angband.lnk
#   @%append angband.lnk debug all
    @%append angband.lnk OPTION CASEEXACT
    @%append angband.lnk OPTION STACK=16k
    @%append angband.lnk name angband
    @for %i in ($(OBJS)) do @%append angband.lnk file %i

.c.objbj:
    $(CC) $(CFLAGS) $[*.c

clean:
    del *.err
    del *.obj
    del *.exe
    del *.lnk

