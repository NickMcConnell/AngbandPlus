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
z-quark.o: z-quark.h z-virt.h
z-rand.o: z-rand.h
z-term.o: z-term.h z-virt.h
z-util.o: z-util.h
z-virt.o: z-virt.h z-util.h
z-file.o: z-file.h

# Various files depend on various headers
$(addsuffix .o, attack, cave, spells1): flow.h
$(addsuffix .o, attack, cave, cmd1, cmd2, cmd4, generate, melee2, monster2, spells2, use-obj, xtra2): keypad.h
$(addsuffix .o, dungeon init2 main-win): game-cmd.h
$(addsuffix .o, cmd-obj cmd3 cmd6 dungeon main-sdl store use-obj): cmds.h
$(addsuffix .o, init1 init2 randart): init.h
$(addsuffix .o, init2 xtra1 xtra3): game-event.h
$(addsuffix .o, cmd4 files init1 init2 util squelch save load wizard1): z-file.h
$(addsuffix .o, cmd1 cmd3 dungeon init2 load object1 save util): z-quark.h

