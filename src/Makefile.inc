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

./archaeologist.o: archaeologist.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./archer.o: archer.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./artifact.o: artifact.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./autopick.o: autopick.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./virtue.o: virtue.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./bard.o: bard.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./beastmaster.o: beastmaster.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./berserker.o: berserker.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./birth.o: birth.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./bldg.o: bldg.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./blood_knight.o: blood_knight.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./blood_mage.o: blood_mage.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./blue_mage.o: blue_mage.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./cavalry.o: cavalry.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cave.o: cave.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./chaos_warrior.o: chaos_warrior.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./classes.o: classes.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd1.o: cmd1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd2.o: cmd2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd3.o: cmd3.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd4.o: cmd4.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd5.o: cmd5.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./cmd6.o: cmd6.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./combat.o: combat.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./demigod.o: demigod.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./devicemaster.o: devicemaster.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./devices.o: devices.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./do-spell.o: do-spell.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./doppelganger.o: doppelganger.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./duelist.o: duelist.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./dungeon.o: dungeon.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./effects.o: effects.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./equip.o: equip.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./fear.o: fear.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./files.o: files.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./flavor.o: flavor.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./floors.o: floors.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h grid.h
./force_trainer.o: force_trainer.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./generate.o: generate.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h generate.h grid.h rooms.h streams.h
./grid.o: grid.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h generate.h grid.h
./hex.o: hex.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./high_mage.o: high_mage.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./hissatsu.o: hissatsu.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./imitator.o: imitator.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./init1.o: init1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h init.h
./init2.o: init2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h init.h
./load.o: load.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mage.o: mage.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./magic_eater.o: magic_eater.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./mauler.o: mauler.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./melee1.o: melee1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./melee2.o: melee2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./menu.o: menu.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mindcrafter.o: mindcrafter.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./mirror_master.o: mirror_master.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./monk.o: monk.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./monster.o: monster.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./monster1.o: monster1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./monster2.o: monster2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mspells1.o: mspells1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mspells2.o: mspells2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mspells3.o: mspells3.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mut.o: mut.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mut_a.o: mut_a.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./mystic.o: mystic.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./necromancer.o: necromancer.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./ninja.o: ninja.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./object1.o: object1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./object2.o: object2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h kajitips.h
./object3.o: object3.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./obj_kind.o: obj_kind.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./paladin.o: paladin.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./priest.o: priest.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./psion.o: psion.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./races.o: races.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./races_a.o: races_a.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./races_k.o: races_k.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./races_mimic.o: races_mimic.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_angel.o: race_angel.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_beholder.o: race_beholder.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./race_demon.o: race_demon.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_dragon.o: race_dragon.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_elemental.o: race_elemental.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./race_giant.o: race_giant.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_golem.o: race_golem.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_hound.o: race_hound.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_hydra.o: race_hydra.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_jelly.o: race_jelly.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_leprechaun.o: race_leprechaun.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./race_lich.o: race_lich.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_possessor.o: race_possessor.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./race_quylthulg.o: race_quylthulg.c angband.h h-basic.h h-config.h \
  h-system.h h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h \
  z-term.h z-config.h savefile.h defines.h variant.h resist.h types.h \
  externs.h mut.h spells.h menu.h fear.h equip.h
./race_spider.o: race_spider.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_sword.o: race_sword.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_troll.o: race_troll.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./race_xorn.o: race_xorn.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./racial.o: racial.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./rage-mage.o: rage-mage.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./ranger.o: ranger.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./red_mage.o: red_mage.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./resist.o: resist.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./rogue.o: rogue.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./rooms.o: rooms.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h generate.h grid.h rooms.h
./rune_knight.o: rune_knight.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./samurai.o: samurai.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./save.o: save.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./savefile.o: savefile.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./scores.o: scores.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./scout.o: scout.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./skills.o: skills.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./sniper.o: sniper.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./sorcerer.o: sorcerer.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells.o: spells.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells1.o: spells1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells2.o: spells2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h grid.h
./spells3.o: spells3.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells_a.o: spells_a.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells_c.o: spells_c.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells_h.o: spells_h.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells_m.o: spells_m.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spells_s.o: spells_s.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./spoilers.o: spoilers.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./store.o: store.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./streams.o: streams.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h generate.h streams.h grid.h
./tables.o: tables.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./time_lord.o: time_lord.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./tourist.o: tourist.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./util.o: util.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./variable.o: variable.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./variant.o: variant.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./warlock.o: warlock.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./warrior.o: warrior.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./warrior_mage.o: warrior_mage.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./weaponmaster.o: weaponmaster.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./weaponsmith.o: weaponsmith.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./wild.o: wild.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./wild_realm.o: wild_realm.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./wild_talent.o: wild_talent.c angband.h h-basic.h h-config.h h-system.h \
  h-type.h h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h \
  z-config.h savefile.h defines.h variant.h resist.h types.h externs.h \
  mut.h spells.h menu.h fear.h equip.h
./wizard1.o: wizard1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./wizard2.o: wizard2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./xtra1.o: xtra1.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./xtra2.o: xtra2.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./z-term.o: z-term.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./int-map.o: int-map.c int-map.h
./str-map.o: str-map.c str-map.h
./z-form.o: z-form.c z-form.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h
./z-rand.o: z-rand.c z-rand.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h
./z-util.o: z-util.c z-util.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h
./z-virt.o: z-virt.c z-virt.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h
./main-gcu.o: main-gcu.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./z-form.o: z-form.c z-form.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h
./z-rand.o: z-rand.c z-rand.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h
./z-term.o: z-term.c angband.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h z-virt.h z-form.h z-rand.h z-term.h z-config.h \
  savefile.h defines.h variant.h resist.h types.h externs.h mut.h \
  spells.h menu.h fear.h equip.h
./z-util.o: z-util.c z-util.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h
./z-virt.o: z-virt.c z-virt.h h-basic.h h-config.h h-system.h h-type.h \
  h-define.h z-util.h
