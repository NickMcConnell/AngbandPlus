## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  abilattr.txt angdesc.txt basctask.txt \
  birth.hlp boptdesc.txt cmddesc.txt cmdlist.txt \
  combat.hlp combat.txt command.hlp defence.txt \
  dungeon.txt equip.txt explore.hlp general.hlp \
  getinfo.txt help.hlp macro.txt magic.txt \
  newbie.txt oangdesc.txt objs-mon.txt optdesc.txt \
  raceclas.txt specialt.txt symbols.txt terminal.txt \
  town.txt

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@help

oangband_DATA = \
  $(oangband_files)
endif
