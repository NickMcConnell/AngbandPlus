## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  abilattr.txt angdesc.txt basctask.txt \
  birth.hlp boptdesc.txt cmddesc.txt cmdlist.txt \
  combat.hlp combat.txt command.hlp defence.txt \
  equip.txt explore.hlp fadesc.txt general.hlp \
  getinfo.txt help.hlp macro.txt magic.txt memap.txt \
  newbie.txt oangdesc.txt objs-mon.txt optdesc.txt \
  raceclas.txt specialt.txt symbols.txt terminal.txt \
  town.txt wild-dun.txt

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@help

faangband_DATA = \
  $(faangband_files)
endif
