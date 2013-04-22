SRCS = \
  z-util.c z-virt.c z-form.c z-rand.c z-term.c \
  variable.c tables.c util.c cave.c \
  object1.c object2.c monster1.c monster2.c \
  xtra1.c xtra2.c spells1.c spells2.c \
  melee1.c melee2.c save.c files.c \
  cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c \
  store.c birth.c load.c h-spell.c x-spell.c \
  wizard1.c wizard2.c \
  generate.c dungeon.c init1.c init2.c randart.c \
  main-cap.c main-gcu.c main-x11.c main-xaw.c main.c \
  main-gtk.c main-lsl.c main-sla.c main-vcs.c main-xpj.c \
  maid-x11.c use-obj.c \
  script.c

OBJS = \
  z-util.o z-virt.o z-form.o z-rand.o z-term.o \
  variable.o tables.o util.o cave.o \
  object1.o object2.o monster1.o monster2.o \
  xtra1.o xtra2.o spells1.o spells2.o \
  melee1.o melee2.o save.o files.o \
  cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o \
  store.o birth.o load.o h-spell.o x-spell.o\
  wizard1.o wizard2.o \
  generate.o dungeon.o init1.o init2.o randart.o \
  main-cap.o main-gcu.o main-x11.o main-xaw.o main.o \
  main-gtk.o main-lsl.o main-sla.o main-vcs.o main-xpj.o \
  maid-x11.o use-obj.o \
  script.o

#LUAOBJS = \
#  lua/lapi.o lua/ldebug.o lua/lmem.o lua/lstrlib.o lua/lvm.o \
#  lua/tolua_lb.o lua/lauxlib.o lua/ldo.o lua/lobject.o lua/ltable.o \
#  lua/lzio.o lua/tolua_rg.o lua/lbaselib.o lua/lfunc.o lua/lparser.o \
#  lua/ltests.o lua/tolua_bd.o lua/tolua_tm.o lua/lcode.o lua/lgc.o \
#  lua/lstate.o lua/ltm.o lua/tolua_eh.o lua/tolua_tt.o lua/ldblib.o \
#  lua/llex.o lua/lstring.o lua/lundump.o lua/tolua_gp.o

#TOLUAOBJS = \
#  lua/tolua.o lua/tolualua.o lua/liolib.o \
#  $(LUAOBJS)
