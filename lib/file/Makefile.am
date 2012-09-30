## Makefile.am -- Process this file with automake to process Makefile.in

angband_files = \
  a_low.txt crime.txt elvish.txt monfear.txt \
  news.txt sample.txt timenorm.txt w_low.txt \
  a_cursed.txt a_med.txt dead.txt error.txt \
  monfrien.txt readme.txt silly.txt w_cursed.txt \
  w_med.txt a_high.txt chainswd.txt death.txt \
  mondeath.txt monspeak.txt rumors.txt timefun.txt \
  w_high.txt

EXTRA_DIST = \
  $(angband_files)

if SET_GID
angbanddir = @DEFAULT_PATH@file

angband_DATA = \
  $(angband_files)
endif
