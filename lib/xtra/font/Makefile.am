## Makefile.am -- Process this file with automake to process Makefile.in

unangband_files = \
  10x20.fon 6x10.fon 6x13b.fon 7x13b.fon 9x15.fon \
  12x24.fon 6x12.fon 6x9.fon 8x13.fon 9x15b.fon \
  5x8.fon 6x13.fon 7x13.fon 8x13b.fon  

EXTRA_DIST = \
  $(unangband_files)

if SET_GID
unangbanddir = @DEFAULT_PATH@xtra@font

unangband_DATA = \
  $(unangband_files)
endif

