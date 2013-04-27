#
# File: Makefile.inc
#
# Includes Makefile.src and contains dependency info
#

include Makefile.src

BASEOBJS := $(ANGFILES)
ZOBJS := $(ZFILES)

# Note dependencies
$(BASEOBJS): $(HEADERS)
$(ZOBJS): $(HDRS)
BASEOBJS += $(ZOBJS)

flow.o: keypad.h types2.h
keypad.o: keypad.h types2.h
game-event.o: z-virt.h game-event.h
z-form.o: z-form.h z-util.h z-virt.h
z-probability.o : z-rand.h z-probability.h
z-quark.o: z-quark.h z-virt.h
z-rand.o: z-rand.h
z-term.o: z-term.h z-virt.h
z-util.o: z-util.h
z-virt.o: z-virt.h z-util.h
z-file.o: z-file.h

# Various files depend on various headers
$(addsuffix .o, attack cave, spells1): flow.h
$(addsuffix .o, attack cave cmd1 cmd2 cmd4 generate melee2 monster2 spells2 use-obj xtra2 xtra3): keypad.h
$(addsuffix .o, object2): POD.hpp
$(addsuffix .o, flow): rational.hpp
$(addsuffix .o, attack birth cave cmd3 cmd4 dungeon generate melee1 melee2 monster1 monster2 spells1 spells2 wizard1 xtra2): bitflag.h

$(addsuffix .o, load spells2): ego.h
$(addsuffix .o, melee1 melee2 monster2 spells1): learn.h
$(addsuffix .o, cmd4 files init2 macro util): macro.h
$(addsuffix .o, melee1 melee2 monster1): melee.h
$(addsuffix .o, birth cave cmd1 cmd2 cmd3 cmd4 cmd5 dungeon files generate init2 load melee2 monster1 monster2 obj-info object1 object2 option save score spells1 spells2 store use-obj util wizard2 xtra1 xtra2 xtra3 xtra4): option.h
$(addsuffix .o, melee2 spells1 spells2 use-obj x-spell): project.h
$(addsuffix .o, object1 randart randname):randname.h
$(addsuffix .o, attack birth cave cmd3 cmd4 dungeon generate melee1 melee2 monster1 monster2 spells1 spells2 wizard1 xtra2): raceflag.h
$(addsuffix .o, birth dungeon files generate init2 load save store): store.h 
$(addsuffix .o, melee2 monster2 use-obj): summon.h
$(addsuffix .o, attack birth cmd1 cmd2 cmd3 cmd5 cmd-obj dungeon files init1 init2 load melee1 melee2 object1 obj-info randart spells1 spells2 store use-obj wizard1 wizard2 x-spell xtra1 xtra2):tvalsval.h
$(addsuffix .o, cave cmd4 cmd5 object1 xtra1 xtra3): wind_flg.h
$(addsuffix .o, store x-spell): x-spell.h
$(addsuffix .o, dungeon init2 main-win): game-cmd.h
$(addsuffix .o, cmd-obj cmd3 cmd6 dungeon main-sdl store use-obj): cmds.h
$(addsuffix .o, init1 init2 randart): init.h
$(addsuffix .o, cave cmd3 dungeon init2 xtra1 xtra2 xtra3): game-event.h
$(addsuffix .o, cmd4 files score): score.h
$(addsuffix .o, cmd4 files init1 init2 util squelch save load wizard1): z-file.h
$(addsuffix .o, cmd1 cmd3 dungeon init2 load object1 save util): z-quark.h

