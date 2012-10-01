## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  dead.txt news.txt a_normal.txt a_cursed.txt w_cursed.txt w_normal.txt bravado.txt names.txt

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@file

oangband_DATA = \
  $(oangband_files)
endif

