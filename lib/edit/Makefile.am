## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  artifact.txt \
  cost_adj.txt \
  ego_item.txt \
  monster.txt \
  object.txt \
  p_class.txt \
  p_hist.txt \
  p_race.txt \
  set_item.txt \
  shop_own.txt \
  terrain.txt \
  themed.txt \
  vault.txt

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@edit

faangband_DATA = \
  $(faangband_files)
endif

