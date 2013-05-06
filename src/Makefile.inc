#
# File: Makefile.inc
#
# Includes Makefile.src and contains dependency info
#

include Makefile.src

BASEOBJS := $(ANGFILES) $(ZFILES)

depgen:
	head -n 19 Makefile.inc > Makefile.new
	@for i in $(BASEOBJS:.o=.c); do \
		DIR=`echo ./$$i | sed 's|/[^/]*$$||'`; \
			gcc -MM -I. $$i | \
			sed "s|^\\([^ ]\\)|$$DIR/\\1|" >> Makefile.new; \
	done;

# Dependencies
./attack.o: attack.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./birth.o: birth.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./button.o: button.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./calcs.o: calcs.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cave.o: cave.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd0.o: cmd0.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd1.o: cmd1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd2.o: cmd2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd3.o: cmd3.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd4.o: cmd4.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./cmd5.o: cmd5.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd-know.o: cmd-know.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./cmd-obj.o: cmd-obj.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./death.o: death.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./dump_items.o: dump_items.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./dungeon.o: dungeon.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h game-event.h

./effect.o: effect.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./feature.o: feature.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./files.o: files.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./game-cmd.o: game-cmd.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./game-event.o: game-event.c z-virt.h h-basic.h game-event.h

./generate.o: generate.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./identify.o: identify.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./init1.o: init1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./init2.o: init2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./load.o: load.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./melee1.o: melee1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./melee2.o: melee2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./monster1.o: monster1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./monster2.o: monster2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./object1.o: object1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./object2.o: object2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./obj-info.o: obj-info.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./obj-ui.o: obj-ui.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./obj-util.o: obj-util.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./pathfind.o: pathfind.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./prefs.o: prefs.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./quest.o: quest.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./randart.o: randart.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./score.o: score.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./signals.o: signals.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./save.o: save.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./spells1.o: spells1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./spells2.o: spells2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./squelch.o: squelch.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./store.o: store.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./tables.o: tables.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./target.o: target.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./timed.o: timed.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./ui.o: ui.c angband.h angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./ui-birth.o: ui-birth.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./ui-event.o: ui-event.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./ui-menu.o: ui-menu.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./util.o: util.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./variable.o: variable.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./wizard1.o: wizard2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./wizard2.o: wizard2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./x-char.o: x-char.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./x-spell.o: x-spell.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./xtra1.o: xtra1.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./xtra2.o: xtra2.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./xtra3.o : xtra3.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h

./z-file.o: z-file.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h

./z-form.o: z-form.c angband.h h-basic.h z-file.h z-form.h z-util.h \
  z-virt.h z-rand.h z-term.h ui-event.h z-quark.h z-msg.h config.h \
  defines.h types.h object.h monster.h player.h store.h cmds.h \
  ui.h z-type.h externs.h game-cmd.h game-event.h init.h terrain.h \
  ui-birth.h ui-menu.h x-char.h


./z-msg.o: z-msg.c z-virt.h h-basic.h z-term.h ui-event.h z-msg.h
./z-quark.o: z-quark.c z-virt.h h-basic.h z-quark.h
./z-rand.o: z-rand.c z-rand.h h-basic.h
./z-term.o: z-term.c z-term.h h-basic.h ui-event.h z-virt.h x-char.h
./z-type.o: z-type.c h-basic.h z-form.h z-term.h ui-event.h ui.h z-type.h
./z-util.o: z-util.c z-util.h h-basic.h
./z-virt.o: z-virt.c z-virt.h h-basic.h z-util.h
