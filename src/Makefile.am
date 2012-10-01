## Makefile.am -- Process this file with automake to process Makefile.in

bin_PROGRAMS = oangband

WRES = windres

SUFFIXES = .rc
angband.res: angband.rc
	$(WRES) $< -O coff -o $@

oangband_SOURCES = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c monster1.c monster2.c \
  xtra1.c xtra2.c spells1.c spells2.c \
  monmove.c monattk.c save.c files.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load1.c load2.c \
  wizard1.c wizard2.c \
  main-cap.c main-gcu.c main-x11.c main-xaw.c main.c \
  main-win.c \
  generate.c dungeon.c init1.c init2.c randart.c \
  squelch.c info.c attack.c \
  angband.h config.h defines.h externs.h h-basic.h \
  h-config.h h-define.h h-system.h h-type.h init.h \
  types.h z-form.h z-rand.h z-term.h z-util.h \
  z-virt.h main.h 

EXTRA_oangband_SOURCES = \
  main-ros.c main-ami.c main-dos.c main-emx.c \
  main-ibm.c main-lsl.c main-mac.c main-sla.c main-vme.c \
  main-xxx.c main-crb.c \
  Makefile.ros Makefile.ami Makefile.bcc Makefile.cyg \
  Makefile.dos Makefile.emx Makefile.gtk Makefile.ibm \
  Makefile.lcc Makefile.lsl Makefile.std Makefile.wat \
  Makefile.win Makefile.mpw Makefile.osx \
  angband.rc angband.ico readdib.c readdib.h \
  maid-x11.c maid-x11.h 

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/oangband"
	chmod g+s "$(DESTDIR)$(bindir)/oangband"
endif

