# Wedit Makefile for project DaJAngband
SRCDIR="c:\documents and settings\all users\documents\shared games\dajband\angband-3.0.9-src\angband-3.0.9"
CFLAGS=-I"C:\Documents and Settings\All Users\Documents\Shared Downloads\Ccompiler\lcc\include"  -g2 
CC=$(LCCROOT)\bin\lcc.exe
LINKER=$(LCCROOT)\bin\lcclnk.exe
OBJS=\
	z-virt.obj \
	z-util.obj \
	z-type.obj \
	z-term.obj \
	z-rand.obj \
	z-form.obj \
	z-file.obj \
	xtra2.obj \
	xtra1.obj \
	x-spell.obj \
	wizard2.obj \
	wizard1.obj \
	angband.res \
	variable.obj \
	util.obj \
	use-obj.obj \
	ui.obj \
	tables.obj \
	store.obj \
	squelch.obj \
	spells2.obj \
	spells1.obj \
	snd-sdl.obj \
	signals.obj \
	save.obj \
	randname.obj \
	randart.obj \
	pathfind.obj \
	obj-info.obj \
	object2.obj \
	object1.obj \
	monster2.obj \
	monster1.obj \
	melee2.obj \
	melee1.obj \
	main-win.obj \
	load.obj \
	init2.obj \
	init1.obj \
	generate.obj \
	files.obj \
	dungeon.obj \
	cmd6.obj \
	cmd5.obj \
	cmd4.obj \
	cmd3.obj \
	cmd2.obj \
	cmd1.obj \
	cmd0.obj \
	cave.obj \
	birth.obj

LIBS=winmm.lib
EXE="C:\Documents and Settings\All Users\Documents\Shared Games\DAJband\DaJAngband\dajangband.exe"

$(EXE):	$(OBJS) Makefile
	$(LINKER)  -subsystem windows -o $(SRCDIR)\"C:\Documents and Settings\All Users\Documents\Shared Games\DAJband\DaJAngband\dajangband.exe" $(OBJS) $(LIBS)

# Build z-virt.c
Z-VIRT_C=\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\

z-virt.obj: $(Z-VIRT_C) $(SRCDIR)\src\z-virt.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-virt.c

# Build z-util.c
Z-UTIL_C=\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\

z-util.obj: $(Z-UTIL_C) $(SRCDIR)\src\z-util.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-util.c

# Build z-type.c
Z-TYPE_C=\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\

z-type.obj: $(Z-TYPE_C) $(SRCDIR)\src\z-type.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-type.c

# Build z-term.c
Z-TERM_C=\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\

z-term.obj: $(Z-TERM_C) $(SRCDIR)\src\z-term.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-term.c

# Build z-rand.c
Z-RAND_C=\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\

z-rand.obj: $(Z-RAND_C) $(SRCDIR)\src\z-rand.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-rand.c

# Build z-form.c
Z-FORM_C=\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\

z-form.obj: $(Z-FORM_C) $(SRCDIR)\src\z-form.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-form.c

# Build z-file.c
Z-FILE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\z-file.h\

z-file.obj: $(Z-FILE_C) $(SRCDIR)\src\z-file.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\z-file.c

# Build xtra2.c
XTRA2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

xtra2.obj: $(XTRA2_C) $(SRCDIR)\src\xtra2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\xtra2.c

# Build xtra1.c
XTRA1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

xtra1.obj: $(XTRA1_C) $(SRCDIR)\src\xtra1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\xtra1.c

# Build x-spell.c
X-SPELL_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

x-spell.obj: $(X-SPELL_C) $(SRCDIR)\src\x-spell.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\x-spell.c

# Build wizard2.c
WIZARD2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

wizard2.obj: $(WIZARD2_C) $(SRCDIR)\src\wizard2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\wizard2.c

# Build wizard1.c
WIZARD1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\cmds.h\

wizard1.obj: $(WIZARD1_C) $(SRCDIR)\src\wizard1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\wizard1.c

# Build angband.res
ANGBAND_RC=\

angband.res:	$(ANGBAND_RC) $(SRCDIR)\src\win\angband.rc
	$(LCCROOT)\bin\lrc.exe -I$(SRCDIR) -I"C:\Documents and Settings\All Users\Documents\Shared Downloads\Ccompiler\lcc\include"  $(SRCDIR)\src\win\angband.rc

# Build variable.c
VARIABLE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

variable.obj: $(VARIABLE_C) $(SRCDIR)\src\variable.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\variable.c

# Build util.c
UTIL_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\randname.h\

util.obj: $(UTIL_C) $(SRCDIR)\src\util.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\util.c

# Build use-obj.c
USE-OBJ_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

use-obj.obj: $(USE-OBJ_C) $(SRCDIR)\src\use-obj.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\use-obj.c

# Build ui.c
UI_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

ui.obj: $(UI_C) $(SRCDIR)\src\ui.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\ui.c

# Build tables.c
TABLES_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

tables.obj: $(TABLES_C) $(SRCDIR)\src\tables.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\tables.c

# Build store.c
STORE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\cmds.h\

store.obj: $(STORE_C) $(SRCDIR)\src\store.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\store.c

# Build squelch.c
SQUELCH_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

squelch.obj: $(SQUELCH_C) $(SRCDIR)\src\squelch.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\squelch.c

# Build spells2.c
SPELLS2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

spells2.obj: $(SPELLS2_C) $(SRCDIR)\src\spells2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\spells2.c

# Build spells1.c
SPELLS1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

spells1.obj: $(SPELLS1_C) $(SRCDIR)\src\spells1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\spells1.c

# Build snd-sdl.c
SND-SDL_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

snd-sdl.obj: $(SND-SDL_C) $(SRCDIR)\src\snd-sdl.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\snd-sdl.c

# Build signals.c
SIGNALS_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

signals.obj: $(SIGNALS_C) $(SRCDIR)\src\signals.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\signals.c

# Build save.c
SAVE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

save.obj: $(SAVE_C) $(SRCDIR)\src\save.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\save.c

# Build randname.c
RANDNAME_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\randname.h\

randname.obj: $(RANDNAME_C) $(SRCDIR)\src\randname.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\randname.c

# Build randart.c
RANDART_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\init.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\randname.h\

randart.obj: $(RANDART_C) $(SRCDIR)\src\randart.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\randart.c

# Build pathfind.c
PATHFIND_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

pathfind.obj: $(PATHFIND_C) $(SRCDIR)\src\pathfind.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\pathfind.c

# Build obj-info.c
OBJ-INFO_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

obj-info.obj: $(OBJ-INFO_C) $(SRCDIR)\src\obj-info.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\obj-info.c

# Build object2.c
OBJECT2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

object2.obj: $(OBJECT2_C) $(SRCDIR)\src\object2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\object2.c

# Build object1.c
OBJECT1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\randname.h\

object1.obj: $(OBJECT1_C) $(SRCDIR)\src\object1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\object1.c

# Build monster2.c
MONSTER2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

monster2.obj: $(MONSTER2_C) $(SRCDIR)\src\monster2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\monster2.c

# Build monster1.c
MONSTER1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

monster1.obj: $(MONSTER1_C) $(SRCDIR)\src\monster1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\monster1.c

# Build melee2.c
MELEE2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

melee2.obj: $(MELEE2_C) $(SRCDIR)\src\melee2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\melee2.c

# Build melee1.c
MELEE1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

melee1.obj: $(MELEE1_C) $(SRCDIR)\src\melee1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\melee1.c

# Build main-win.c
MAIN-WIN_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\win\readdib.h\

main-win.obj: $(MAIN-WIN_C) $(SRCDIR)\src\main-win.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\main-win.c

# Build load.c
LOAD_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

load.obj: $(LOAD_C) $(SRCDIR)\src\load.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\load.c

# Build init2.c
INIT2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\init.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\cmds.h\

init2.obj: $(INIT2_C) $(SRCDIR)\src\init2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\init2.c

# Build init1.c
INIT1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\init.h\
	$(SRCDIR)\src\h-basic.h\

init1.obj: $(INIT1_C) $(SRCDIR)\src\init1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\init1.c

# Build generate.c
GENERATE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

generate.obj: $(GENERATE_C) $(SRCDIR)\src\generate.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\generate.c

# Build files.c
FILES_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

files.obj: $(FILES_C) $(SRCDIR)\src\files.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\files.c

# Build dungeon.c
DUNGEON_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\cmds.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

dungeon.obj: $(DUNGEON_C) $(SRCDIR)\src\dungeon.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\dungeon.c

# Build cmd6.c
CMD6_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

cmd6.obj: $(CMD6_C) $(SRCDIR)\src\cmd6.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd6.c

# Build cmd5.c
CMD5_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

cmd5.obj: $(CMD5_C) $(SRCDIR)\src\cmd5.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd5.c

# Build cmd4.c
CMD4_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\ui.h\

cmd4.obj: $(CMD4_C) $(SRCDIR)\src\cmd4.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd4.c

# Build cmd3.c
CMD3_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

cmd3.obj: $(CMD3_C) $(SRCDIR)\src\cmd3.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd3.c

# Build cmd2.c
CMD2_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

cmd2.obj: $(CMD2_C) $(SRCDIR)\src\cmd2.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd2.c

# Build cmd1.c
CMD1_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

cmd1.obj: $(CMD1_C) $(SRCDIR)\src\cmd1.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd1.c

# Build cmd0.c
CMD0_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\

cmd0.obj: $(CMD0_C) $(SRCDIR)\src\cmd0.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cmd0.c

# Build cave.c
CAVE_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\

cave.obj: $(CAVE_C) $(SRCDIR)\src\cave.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\cave.c

# Build birth.c
BIRTH_C=\
	$(SRCDIR)\src\angband.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-file.h\
	$(SRCDIR)\src\z-form.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-util.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-virt.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-rand.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\z-term.h\
	$(SRCDIR)\src\h-basic.h\
	$(SRCDIR)\src\config.h\
	$(SRCDIR)\src\defines.h\
	$(SRCDIR)\src\types.h\
	$(SRCDIR)\src\ui.h\
	$(SRCDIR)\src\z-type.h\
	$(SRCDIR)\src\externs.h\
	$(SRCDIR)\src\cmds.h\
	$(SRCDIR)\src\script.h\
	$(SRCDIR)\src\angband.h\

birth.obj: $(BIRTH_C) $(SRCDIR)\src\birth.c
	$(CC) -c $(CFLAGS) $(SRCDIR)\src\birth.c

link:
	$(LINKER)  -subsystem windows -o $(SRCDIR)\"C:\Documents and Settings\All Users\Documents\Shared Games\DAJband\DaJAngband\dajangband.exe" $(OBJS) $(LIBS)

clean:
	del $(OBJS) dajangband.exe"
