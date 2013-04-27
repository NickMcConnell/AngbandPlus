AUTOMAKE_OPTIONS = subdir-objects

#-----------------------------------------------------------------------------
## Rules similar to `automake 1.2f' for building ObjC files, with dependencies
OBJC = gcc
SUFFIXES = .m .rc

# AM_OBJCFLAGS = $(CFLAGS)

OBJCCOMPILE = $(OBJC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(OBJCFLAGS)
LTOBJCCOMPILE = $(LIBTOOL) --mode=compile $(OBJCCOMPILE)

# ** This has to be called `LINK', not `OBJCLINK'
LINK = $(LIBTOOL) --mode=link $(OBJC) $(OBJCFLAGS) $(LDFLAGS) -o $@

gen_deps_o = @MAINT@-Wp,-MD,.deps/$(*F).P
gen_deps_lo = @MAINT@-Wp,-MD,.deps/$(*F).p

.m.o:
	$(OBJCCOMPILE) $(gen_deps_o) -c $<

.m.lo:
	$(LTOBJCCOMPILE) $(gen_deps_lo) -c $<
	@MAINT@@-sed -e 's/^\([^:]*\)\.o:/\1.lo \1.o:/' \
	@MAINT@ < .deps/$(*F).p > .deps/$(*F).P
	@MAINT@@-rm -f .deps/$(*F).p

#-----------------------------------------------------------------------------

SUBDIRS = angband engines overlay platform render scene

bin_PROGRAMS = ironhells

WRES = windres

ironhells.res: ironhells.rc
	$(WRES) $< -O coff -o $@

ironhells_LDADD = ../lua/liblua.a $(SDLTTF_LIBS) $(SDLDRAW_LIBS) $(SDLIMAGE_LIBS) $(SDL_LIBS)
ironhells_DEPENDENCIES = ../lua/liblua.a
ironhells_CFLAGS = $(SDL_CFLAGS) $(SDLTTF_CFLAGS) $(SDLDRAW_CFLAGS) $(SDLIMAGE_CFLAGS) -DTILE_HACK -DEXT_MAP_INFO -Werror-implicit-function-declaration

ironhells_SOURCES = \
	main.c \
	engines.c engines.h \
	ipc.c ipc.h \
	ironhells.c ironhells.h \
	list.c list.h \
	object.c object.h \
	overlay.c overlay.h \
	path.c path.h \
	prefs.c prefs.h \
	scene.c scene.h \
	strings.h \
	thread.c thread.h \
	angband/angband.h \
	angband/ang-main.c angband/birth.c angband/cave.c angband/cmd1.c \
	angband/cmd2.c angband/cmd3.c angband/cmd4.c angband/cmd5.c angband/cmd6.c \
	angband/config.h angband/defines.h angband/dungeon.c angband/externs.h \
	angband/files.c angband/generate.c angband/h-basic.h angband/h-config.h \
	angband/h-define.h angband/h-system.h angband/h-type.h angband/init.h \
	angband/init1.c angband/init2.c angband/l-misc.c angband/l-monst.c \
	angband/l-object.c angband/l-player.c angband/l-random.c angband/l-spell.c \
	angband/l-ui.c angband/load.c angband/main.h angband/melee1.c \
	angband/melee2.c angband/monster1.c angband/monster2.c angband/obj-info.c \
	angband/object1.c angband/object2.c angband/randart.c angband/readdib.c \
	angband/readdib.h angband/save.c angband/script.c angband/script.h \
	angband/spells1.c angband/spells2.c angband/store.c angband/tables.c \
	angband/types.h angband/use-obj.c angband/util.c angband/variable.c \
	angband/wizard1.c angband/wizard2.c angband/x-spell.c angband/xtra1.c \
	angband/xtra2.c angband/z-disp.c angband/z-disp.h angband/z-form.c \
	angband/z-form.h angband/z-rand.c angband/z-rand.h angband/z-util.c \
	angband/z-util.h angband/z-virt.c angband/z-virt.h \
	engines/3d/helper.c engines/3d/helper.h \
	engines/3d/icon.c engines/3d/icon.h \
	engines/3d/init.c engines/3d/init.h \
	engines/3d/map.c engines/3d/map.h \
	engines/3d/misc.c engines/3d/misc.h \
	engines/3d/pointer.c engines/3d/pointer.h \
	engines/3d/text.c engines/3d/text.h \
	engines/iso/helper.c engines/iso/helper.h \
	engines/iso/icon.c engines/iso/icon.h \
	engines/iso/init.c engines/iso/init.h \
	engines/iso/map.c engines/iso/map.h \
	engines/iso/misc.c engines/iso/misc.h \
	engines/iso/pointer.c engines/iso/pointer.h \
	engines/iso/text.c engines/iso/text.h \
	overlay/book.c overlay/browser.c overlay/character.c \
	overlay/dialog.c overlay/equipment.c overlay/error.c \
	overlay/inventory.c overlay/map.c overlay/memory.c \
	overlay/messages.c overlay/opponent.c overlay/options.c \
	overlay/shop.c overlay/showfile.c \
	platform/platform.h \
	platform/linux/file-linux.c \
	platform/mac/osx/SDLMain.m platform/mac/osx/SDLMain.h \
	platform/mac/osx/file-osx.m \
	platform/win/file-win.c \
	render/icon.c render/icon.h \
	render/map.c render/map.h \
	render/misc.c render/misc.h \
	render/pointer.c render/pointer.h \
	render/text.c render/text.h \
	scene/grave.c scene/grave.h \
	scene/intro.c scene/intro.h \
	scene/mhost.c scene/mhost.h \
	scene/mjoin.c scene/mjoin.h \
	scene/newchar.c scene/newchar.h \
	scene/play.c scene/play.h \
	scene/selchar.c scene/selchar.h \
	scene/splash.c scene/splash.h \
	scene/title.c scene/title.h \
	scene/video.c scene/video.h

EXTRA_ironhells_SOURCES = \
	angband/l-monst.pkg angband/l-object.pkg angband/l-player.pkg \
	angband/l-random.pkg angband/l-ui.pkg angband/l-misc.pkg \
	angband/l-spell.pkg

INCLUDES = -I../.. -I.. -I$(srcdir) -I$(srcdir)/.. $(SDL_CFLAGS) $(SDLTTF_CFLAGS) $(SDLIMAGE_CFLAGS) $(SDLDRAW_CFLAGS)

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/ironhells"
	chmod g+s "$(DESTDIR)$(bindir)/ironhells"
endif

