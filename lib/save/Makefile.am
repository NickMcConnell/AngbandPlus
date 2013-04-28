## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  delete.me

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@save

faangband_DATA = \
  $(faangband_files)
endif

install-data-hook:
if SET_GID
	chown -R root:@GAMEGROUP@ "$(DESTDIR)$(faangbanddir)"
	chmod -R 070 "$(DESTDIR)$(faangbanddir)"
endif
