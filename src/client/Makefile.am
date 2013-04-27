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

SUBDIRS = angband platform sdl

bin_PROGRAMS = ironhells

WRES = windres

angband/angband.res: angband/angband.rc
	$(WRES) $< -O coff -o $@

ironhells_LDADD = angband/lua/liblua.a $(SDLTTF_LIBS) $(SDLDRAW_LIBS) $(SDLIMAGE_LIBS) $(SDL_LIBS)
ironhells_DEPENDENCIES = angband/lua/liblua.a
ironhells_CFLAGS = $(SDL_CFLAGS) $(SDLTTF_CFLAGS) $(SDLDRAW_CFLAGS) $(SDLIMAGE_CFLAGS)

ironhells_SOURCES = \
	angband/angband.h angband/birth.c angband/cave.c angband/cmd1.c \
	angband/cmd2.c angband/cmd3.c angband/cmd4.c angband/cmd5.c \
	angband/cmd6.c angband/config.h angband/defines.h angband/dungeon.c \
	angband/externs.h angband/files.c angband/generate.c angband/h-basic.h \
	angband/h-config.h angband/h-define.h angband/h-system.h \
	angband/h-type.h angband/init.h angband/init1.c angband/init2.c \
	angband/l-misc.c angband/l-monst.c angband/l-object.c \
	angband/l-player.c angband/l-random.c angband/l-spell.c \
	angband/l-ui.c angband/load.c angband/main.c angband/main.h \
	angband/melee1.c angband/melee2.c angband/monster1.c \
	angband/monster2.c angband/obj-info.c angband/object1.c \
	angband/object2.c angband/randart.c angband/readdib.c \
	angband/readdib.h angband/save.c angband/script.c angband/script.h \
	angband/spells1.c angband/spells2.c angband/store.c angband/tables.c \
	angband/types.h angband/use-obj.c angband/util.c angband/variable.c \
	angband/wizard1.c angband/wizard2.c angband/x-spell.c angband/xtra1.c \
	angband/xtra2.c angband/z-form.c angband/z-form.h angband/z-rand.c \
	angband/z-rand.h angband/z-term.c angband/z-term.h angband/z-util.c \
	angband/z-util.h angband/z-virt.c angband/z-virt.h \
	file.h init.c init.h ipc.c ipc.h ironhells.h list.c list.h main.c \
	path.c path.h prefs.c prefs.h term.c term.h thread.c thread.h \
	platform/linux/file-linux.c platform/mac/osx/SDLMain.h \
	platform/mac/osx/SDLMain.m platform/mac/osx/file-osx.m \
	platform/win/file-win.c \
	sdl/render/icon.c sdl/render/icon.h sdl/render/misc.c sdl/render/misc.h \
	sdl/render/overlay.c sdl/render/overlay.h sdl/render/pointer.c \
	sdl/render/pointer.h sdl/render/text.c sdl/render/text.h sdl/render/tile.c \
	sdl/render/tile.h \
	sdl/bfont.c sdl/bfont.h sdl/helper.c sdl/helper.h sdl/render.c \
	sdl/render.h sdl/scene.c sdl/scene.h sdl/setup.c sdl/setup.h \
	sdl/strings.h \
	sdl/scene/grave.c sdl/scene/grave.h sdl/scene/intro.c sdl/scene/intro.h \
	sdl/scene/mhost.c sdl/scene/mhost.h sdl/scene/mjoin.c sdl/scene/mjoin.h \
	sdl/scene/newchar.c sdl/scene/newchar.h sdl/scene/play.c sdl/scene/play.h \
	sdl/scene/selchar.c sdl/scene/selchar.h sdl/scene/splash.c sdl/scene/splash.h \
	sdl/scene/title.c sdl/scene/title.h

#
# Build wrappers
#
# (This probably should be cleaned up a little.)
#
# $(subst l-,,$*) removes the leading "l-", and
# trailing ".c" from the filename.
#
$(srcdir)/angband/l-monst.c: angband/l-monst.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-object.c: angband/l-object.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-player.c: angband/l-player.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-random.c: angband/l-random.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-ui.c: angband/l-ui.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-misc.c: angband/l-misc.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

$(srcdir)/angband/l-spell.c: angband/l-spell.pkg angband/lua/tolua
	angband/lua/tolua -n $(subst angband/l-,,$*) -o $@ $<

EXTRA_ironhells_SOURCES = \
	angband/l-monst.pkg angband/l-object.pkg angband/l-player.pkg \
	angband/l-random.pkg angband/l-ui.pkg angband/l-misc.pkg \
	angband/l-spell.pkg

INCLUDES = -I$(srcdir) -I$(srcdir)/angband 

install-exec-hook:
if SET_GID
	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/ironhells"
	chmod g+s "$(DESTDIR)$(bindir)/ironhells"
endif

