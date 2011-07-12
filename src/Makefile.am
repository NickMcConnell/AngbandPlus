## Makefile.am -- Process this file with automake to process Makefile.in

AUTOMAKE_OPTIONS = foreign

bin_PROGRAMS = hengband

hengband_SOURCES = \
        archaeologist.c archer.c artifact.c autopick.c avatar.c \
        bard.c birth.c bldg.c blood_knight.c blue_mage.c cavalry.c \
        cave.c chuukei.c classes.c cmd1.c cmd2.c cmd3.c cmd4.c \
        cmd5.c cmd6.c do-spell.c duelist.c dungeon.c effects.c \
        files.c flavor.c floors.c force_trainer.c generate.c grid.c hex.c hissatsu.c \
        inet.c init1.c init2.c japanese.c load.c magic_eater.c \
        main.c mane.c melee1.c melee2.c menu.c mind.c mindcrafter.c \
        mirror.c monk.c monster1.c monster2.c mspells1.c mspells2.c \
        mspells3.c mut.c mut_a.c ninja.c object1.c object2.c obj_kind.c \
        main-gcu.c races.c racial.c report.c rooms.c rune_knight.c \
        samurai.c save.c scores.c smith.c snipe.c spells.c spells1.c \
        spells2.c spells3.c spells_a.c store.c streams.c tables.c \
        util.c variable.c variant.c warlock.c wild.c wild_talent.c \
        wizard1.c wizard2.c xtra1.c xtra2.c z-form.c z-rand.c \
        z-term.c z-util.c z-virt.c \
        main-cap.c main-x11.c main-xaw.c \
        angband.h defines.h externs.h generate.h grid.h h-basic.h \
        h-config.h h-define.h h-system.h h-type.h init.h kajitips.h \
        menu.h mindtips.h mut.h rooms.h spells.h streams.h \
        types.h z-config.h z-form.h z-rand.h z-term.h z-util.h z-virt.h

EXTRA_hengband_SOURCES = \
	angband.ico angband.rc ang_eng.rc maid-x11.c main-mac.c main-win.c \
	main-dos.c main-ibm.c \
	makefile.bcc makefile.std makefile.dos makefile.ibm \
	readdib.c wall.bmp

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/hengband"
	chmod g+s "$(DESTDIR)$(bindir)/hengband"
endif
