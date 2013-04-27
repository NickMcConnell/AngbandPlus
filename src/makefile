DEFAULT: install

zplusangband.exe : $(COREOBJS) $(LUAOBJS)

CC := lcc.exe
CC64 := lcc64.exe
CFLAGS := -O -g -IC:\lcc\include
LINK := lcclnk.exe
LDFLAGS :=
LFLAGS := -s -subsystem Windows
RES := lrc.exe
RESCMD := -IC:\lcc\include zplus.rc

LUAWOBJS := \
	l-monst.o l-object.o l-player.o l-random.o l-ui.o \
	l-misc.o l-spell.o l-field.o

COREOBJS := $(LUAWOBJS) \
	variable.o tables.o cave.o object1.o \
	object2.o monster1.o xtra2.o \
	spells1.o spells2.o melee1.o melee2.o save.o \
	fields.o cmd1.o cmd2.o cmd3.o cmd5.o cmd6.o cmd7.o \
	store.o birth.o load.o hero.o wizard1.o wizard2.o \
	grid.o streams.o rooms.o generate.o dungeon.o init1.o \
	init2.o effects.o quest1.o quest2.o racial.o run.o \
	artifact.o mutation.o spells3.o \
	mspells1.o mspells2.o scores.o mind.o maid-x11.o bldg.o \
	obj_kind.o wild1.o wild2.o wild3.o avatar.o notes.o maid-grf.o \
	main-cap.o main-gcu.o \
	main-x11.o main-xaw.o main-xpj.o \
	main-lsl.o main-vcs.o main-gtk.o main-win.o main.o \
	main-dos.o main-ibm.o main-emx.o main-ami.o main-tnb.o \
	z-util.o z-virt.o z-rand.o z-term.o readdib.o \
	ui.o cmd4.o files.o xtra1.o monster2.o util.o \
	script.o flavor.o z-form.o

LUAOBJS := \
	lua\lapi.o lua\ldebug.o lua\lmem.o lua\lstrlib.o lua\lvm.o \
	lua\tolua_lb.o lua\lauxlib.o lua\ldo.o lua\lobject.o lua\ltable.o \
	lua\lzio.o lua\tolua_rg.o lua\lbaselib.o lua\lfunc.o lua\lparser.o \
	lua\ltests.o lua\tolua_bd.o lua\tolua_tm.o lua\lcode.o lua\lgc.o \
	lua\lstate.o lua\ltm.o lua\tolua_eh.o lua\tolua_tt.o lua\ldblib.o \
	lua\llex.o lua\lstring.o lua\lundump.o lua\tolua_gp.o lua\liolib.o \

CORESRCS := $(COREOBJS:.o=.c)
LUAWSRCS := $(LUAWOBJS:.o=.c)
LUASRCS := $(LUAOBJS:.o=.c)

HDRS := \
	h-basic.h h-define.h h-type.h h-system.h h-config.h angband.h \
	z-config.h z-term.h z-rand.h z-util.h z-virt.h z-form.h \
	defines.h types.h externs.h version.h

$(COREOBJS) : $(HDRS) $(CORESRCS)
$(LUAOBJS) : $(LUASRCS)

SRCS := $(CORESRCS)
OBJS := $(COREOBJS) $(LUAOBJS)

%.o64 : %.c
	$(CC64) $(CFLAGS) $< -o $@

%.o : %.c
	$(CC) $(CFLAGS) $< -o $@

install : zplusangband.exe

MAKE-ZANGBAND = $(LINK) $(LFLAGS) -o $@ $^ $(LDFLAGS) $(OBJS)

zplusangband.exe : $(OBJS) zplus.res
	$(MAKE-ZANGBAND)

zplus.res : zplus.rc
	$(RES) $(RESCMD)

