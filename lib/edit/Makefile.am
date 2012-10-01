## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  artifact.txt \
  cost_adj.txt \
  ego_item.txt \
  monster.txt \
  object.txt \
  p_hist.txt \
  p_race.txt \
  shop_own.txt \
  terrain.txt \
  themed.txt \
  vault.txt

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@edit

oangband_DATA = \
  $(oangband_files)
endif

