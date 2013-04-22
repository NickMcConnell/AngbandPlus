HDRS = \
  h-basic.h \
  h-define.h h-type.h h-system.h h-config.h

INCS = \
  angband.h \
  config.h defines.h types.h externs.h \
  z-term.h z-rand.h z-util.h z-virt.h z-form.h $(HDRS)


birth.o: birth.c $(INCS)
cave.o: cave.c $(INCS)
cmd1.o: cmd1.c $(INCS)
cmd2.o: cmd2.c $(INCS)
cmd3.o: cmd3.c $(INCS)
cmd4.o: cmd4.c $(INCS)
cmd5.o: cmd5.c $(INCS) script.h
cmd6.o: cmd6.c $(INCS) script.h
dungeon.o: dungeon.c $(INCS)
files.o: files.c $(INCS)
generate.o: generate.c $(INCS)
init1.o: init1.c $(INCS) init.h
init2.o: init2.c $(INCS) init.h script.h
#l-misc.o: l-misc.c $(INCS) lua/tolua.h
#l-monst.o: l-monst.c $(INCS) lua/tolua.h
#l-object.o: l-object.c $(INCS) lua/tolua.h
#l-player.o: l-player.c $(INCS) lua/tolua.h
#l-random.o: l-random.c $(INCS) lua/tolua.h
#l-spell.o: l-spell.c $(INCS) lua/tolua.h
#l-ui.o: l-ui.c $(INCS) lua/tolua.h
load.o: load.c $(INCS)
maid-x11.o: maid-x11.c $(INCS) maid-x11.h
main-cap.o: main-cap.c $(INCS) main.h
main-gcu.o: main-gcu.c $(INCS) main.h
main-gtk.o: main-gtk.c $(INCS) main.h maid-x11.h
main-lsl.o: main-lsl.c $(INCS) main.h
main-sla.o: main-sla.c $(INCS) main.h
main-vcs.o: main-vcs.c $(INCS) main.h
main-x11.o: main-x11.c $(INCS) main.h maid-x11.h
main-xaw.o: main-xaw.c $(INCS) main.h maid-x11.h
main-xpj.o: main-xpj.c $(INCS) main.h maid-x11.h
main.o: main.c $(INCS) main.h
melee1.o: melee1.c $(INCS)
melee2.o: melee2.c $(INCS)
monster1.o: monster1.c $(INCS)
monster2.o: monster2.c $(INCS)
object1.o: object1.c $(INCS)
object2.o: object2.c $(INCS)
randart.o: randart.c $(INCS) init.h
save.o: save.c $(INCS)
#script.o: script.c $(INCS) script.h \
#		lua/lauxlib.h lua/lua.h lua/luadebug.h lua/lualib.h lua/tolua.h \
#		lua/luadebug.h
spells1.o: spells1.c $(INCS)
spells2.o: spells2.c $(INCS)
store.o: store.c $(INCS)
tables.o: tables.c $(INCS)
use-obj.o: use-obj.c $(INCS)
util.o: util.c $(INCS)
variable.o: variable.c $(INCS)
wizard1.o: wizard1.c $(INCS)
wizard2.o: wizard2.c $(INCS) script.h
xtra1.o: xtra1.c $(INCS) script.h
xtra2.o: xtra2.c $(INCS)
z-form.o: z-form.c $(HDRS) z-form.h z-util.h z-virt.h
z-rand.o: z-rand.c $(HDRS) z-rand.h
z-term.o: z-term.c $(HDRS) z-term.h z-virt.h
z-util.o: z-util.c $(HDRS) z-util.h
z-virt.o: z-virt.c $(HDRS) z-virt.h z-util.h

#
# Build wrappers
#

#l-monst.c: lua/tolua l-monst.pkg
#	lua/tolua -n monster -o l-monst.c l-monst.pkg

#l-object.c: lua/tolua l-object.pkg
#	lua/tolua -n object -o l-object.c l-object.pkg

#l-player.c: lua/tolua l-player.pkg
#	lua/tolua -n player -o l-player.c l-player.pkg

#l-random.c: lua/tolua l-random.pkg
#	lua/tolua -n random -o l-random.c l-random.pkg

#l-ui.c: lua/tolua l-ui.pkg
#	lua/tolua -n ui -o l-ui.c l-ui.pkg

#l-misc.c: lua/tolua l-misc.pkg
#	lua/tolua -n misc -o l-misc.c l-misc.pkg

#l-spell.c: lua/tolua l-spell.pkg
#	lua/tolua -n spell -o l-spell.c l-spell.pkg
