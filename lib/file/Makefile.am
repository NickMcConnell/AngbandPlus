## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  dead.txt news.txt a_normal.txt a_cursed.txt w_cursed.txt w_normal.txt bravado.txt names.txt

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@file

faangband_DATA = \
  $(faangband_files)
endif

