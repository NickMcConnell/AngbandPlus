## Makefile.am -- Process this file with automake to process Makefile.in

AUTOMAKE_OPTIONS = foreign

bin_PROGRAMS = tinyangband

tinyangband_SOURCES = \
	artifact.c autopick.c birth.c bldg.c cave.c chuukei.c \
	cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
	dungeon.c effects.c files.c flavor.c generate.c \
	grid.c init1.c init2.c japanese.c load.c \
	main.c main-cap.c main-gcu.c main-x11.c main-xaw.c \
	melee1.c melee2.c monster1.c monster2.c \
	mspells1.c mspells2.c mutation.c notes.c \
	obj_kind.c object1.c object2.c racial.c rooms.c \
	save.c scores.c spells1.c spells2.c spells3.c \
	store.c streams.c tables.c util.c variable.c \
	wild.c wizard1.c wizard2.c xtra1.c xtra2.c \
	z-form.c z-rand.c z-term.c z-util.c z-virt.c \
	$(SDL_FILE) \
	angband.h defines.h externs.h generate.h grid.h \
	h-basic.h h-config.h h-define.h h-system.h h-type.h \
	init.h rooms.h streams.h types.h \
	z-config.h z-form.h z-rand.h z-term.h z-util.h z-virt.h

EXTRA_tinyangband_SOURCES = \
	angband.rc ang_eng.rc angband.ico angband.xpm maid-x11.c \
	main-mac-carbon.c main-mac.c main-mac.new main-win.c \
	Makefile.cyg makefile.bcc makefile.org makefile.std \
	readdib.c readdib.h wall.bmp

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/$(bin_PROGRAMS)"
	chmod g+s "$(DESTDIR)$(bindir)/$(bin_PROGRAMS)"
endif
