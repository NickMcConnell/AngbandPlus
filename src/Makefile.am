## Makefile.am -- Process this file with automake to process Makefile.in

bin_PROGRAMS = posband

posband.res: posband.rc
	@WRES@ $< -O coff -o $@

posband_SOURCES = \
    birth.c \
    cave.c \
    cmd-magic.c \
    cmd-misc.c \
    cmd-move-attack.c \
    cmd-object.c \
    files.c \
    game.c \
    generate.c \
    init.c \
    load.c \
    maid-x11.c \
    main-gcu.c \
    main-lfb.c \
    main-vcs.c \
    main-xaw.c \
    main-win.c \
    readdib.c \
    readdib.h \
    main.c \
    menus.c \
    mon-info.c \
    mon-attack.c \
    mon-misc.c \
    mon-ai.c \
    mon-summ.c \
    obj-info.c \
    object.c \
    parse.c \
    player-misc.c \
    player-state.c \
    powers.c \
    quest.c \
    randart.c \
    save.c \
    spells-util.c \
    spells.c \
    spoilers.c \
    squelch.c \
    store.c \
    tables.c \
    target.c \
    use-obj.c \
    util.c \
    variable.c \
    wizard.c \
    z-form.c \
    z-rand.c \
    z-term.c \
    z-util.c \
    z-virt.c \
    cave.h \
    init.h \
    lib.h \
    maid-x11.h \
    main.h \
    misc.h \
    monster.h \
    object.h \
    options.h \
    player.h \
    posband.h \
    quest.h \
    spells.h \
    store.h \
    system.h

if WITH_WIN
posband_LDADD = posband.res
endif
   
EXTRA_posband_SOURCES = \
    main-xxx.c \
    posband.rc \
    posband.ico \
    powers.m4 \
    powers_strings.inc \
    powers_defines.inc \
    powers_info.inc \
    powers_code.inc \
    powers_titles.inc

# Rules for m4-generated files

BUILT_SOURCES = \
    powers_strings.inc \
    powers_defines.inc \
    powers_info.inc \
    powers_code.inc \
    powers_titles.inc

powers_defines.inc: powers.m4
	@M4@ -DOUT=defines $< >$@

powers_strings.inc: powers.m4
	@M4@ -DOUT=strings $< >$@

powers_titles.inc: powers.m4
	@M4@ -DOUT=titles $< >$@

powers_info.inc: powers.m4
	@M4@ -DOUT=info $< >$@

powers_code.inc: powers.m4
	@M4@ -DOUT=code $< >$@

install-exec-hook:
	chgrp games "$(DESTDIR)$(bindir)/posband"
	chmod g+s "$(DESTDIR)$(bindir)/posband"
