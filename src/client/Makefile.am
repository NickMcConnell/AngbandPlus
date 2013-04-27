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

SUBDIRS = angband displays lua platform

bin_PROGRAMS = ironhells

WRES = windres

angband/angband.res: angband/angband.rc
	$(WRES) $< -O coff -o $@

ironhells_LDADD = lua/liblua.a $(SDLTTF_LIBS) $(SDLDRAW_LIBS) $(SDLIMAGE_LIBS) $(SDL_LIBS)
ironhells_DEPENDENCIES = lua/liblua.a
ironhells_CFLAGS = $(SDL_CFLAGS) $(SDLTTF_CFLAGS) $(SDLDRAW_CFLAGS) $(SDLIMAGE_CFLAGS) -DTILE_HACK -DEXT_MAP_INFO -Werror-implicit-function-declaration

ironhells_SOURCES = \
	main.c ironhells.h strings.h \
	ipc.c ipc.h \
	list.c list.h \
	path.c path.h \
	prefs.c prefs.h \
	thread.c thread.h \
	angband/angband.h angband/birth.c angband/cave.c angband/cmd1.c \
	angband/cmd2.c angband/cmd3.c angband/cmd4.c angband/cmd5.c \
	angband/cmd6.c angband/config.h angband/defines.h angband/dungeon.c \
	angband/externs.h angband/files.c angband/generate.c angband/h-basic.h \
	angband/h-config.h angband/h-define.h angband/h-system.h \
	angband/h-type.h angband/init.h angband/init1.c angband/init2.c \
	angband/l-misc.c angband/l-monst.c angband/l-object.c \
	angband/l-player.c angband/l-random.c angband/l-spell.c \
	angband/l-ui.c angband/load.c angband/ang-main.c angband/main.h \
	angband/melee1.c angband/melee2.c angband/monster1.c \
	angband/monster2.c angband/obj-info.c angband/object1.c \
	angband/object2.c angband/randart.c angband/readdib.c \
	angband/readdib.h angband/save.c angband/script.c angband/script.h \
	angband/spells1.c angband/spells2.c angband/store.c angband/tables.c \
	angband/types.h angband/use-obj.c angband/util.c angband/variable.c \
	angband/wizard1.c angband/wizard2.c angband/x-spell.c angband/xtra1.c \
	angband/xtra2.c angband/z-form.c angband/z-form.h angband/z-rand.c \
	angband/z-rand.h angband/z-disp.c angband/z-disp.h angband/z-util.c \
	angband/z-util.h angband/z-virt.c angband/z-virt.h \
	displays/iso/bfont.c displays/iso/bfont.h \
	displays/iso/display.c displays/iso/display.h \
	displays/iso/helper.c displays/iso/helper.h \
	displays/iso/icon.c displays/iso/icon.h \
	displays/iso/misc.c displays/iso/misc.h \
	displays/iso/overlay/book.c displays/iso/overlay/browser.c \
	displays/iso/overlay/character.c displays/iso/overlay/dialog.c \
	displays/iso/overlay/equipment.c displays/iso/overlay/error.c \
	displays/iso/overlay/showfile.c displays/iso/overlay/inventory.c \
	displays/iso/overlay/map.c displays/iso/overlay/memory.c \
	displays/iso/overlay/messages.c displays/iso/overlay/opponent.c \
	displays/iso/overlay/options.c displays/iso/overlay/shop.c \
	displays/iso/overlay.c displays/iso/overlay.h \
	displays/iso/pointer.c displays/iso/pointer.h \
	displays/iso/render.c displays/iso/render.h \
	displays/iso/scene.c displays/iso/scene.h \
	displays/iso/text.c displays/iso/text.h \
	displays/iso/tile.c displays/iso/tile.h \
	displays/iso/scene/grave.c displays/iso/scene/grave.h \
	displays/iso/scene/intro.c displays/iso/scene/intro.h \
	displays/iso/scene/mhost.c displays/iso/scene/mhost.h \
	displays/iso/scene/mjoin.c displays/iso/scene/mjoin.h \
	displays/iso/scene/newchar.c displays/iso/scene/newchar.h \
	displays/iso/scene/play.c displays/iso/scene/play.h \
	displays/iso/scene/video.c displays/iso/scene/video.h \
	displays/iso/scene/selchar.c displays/iso/scene/selchar.h \
	displays/iso/scene/splash.c displays/iso/scene/splash.h \
	displays/iso/scene/title.c displays/iso/scene/title.h \
	platform/platform.h \
	platform/linux/file-linux.c \
	platform/mac/osx/SDLMain.m platform/mac/osx/SDLMain.h \
	platform/mac/osx/file-osx.m \
	platform/win/file-win.c

EXTRA_ironhells_SOURCES = \
	angband/l-monst.pkg angband/l-object.pkg angband/l-player.pkg \
	angband/l-random.pkg angband/l-ui.pkg angband/l-misc.pkg \
	angband/l-spell.pkg

INCLUDES = -I$(srcdir) $(SDL_CFLAGS) $(SDLTTF_CFLAGS) $(SDLIMAGE_CFLAGS) $(SDLDRAW_CFLAGS)

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/ironhells"
	chmod g+s "$(DESTDIR)$(bindir)/ironhells"
endif

