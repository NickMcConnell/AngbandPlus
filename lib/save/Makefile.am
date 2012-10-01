## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  delete.me

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@save

oangband_DATA = \
  $(oangband_files)
endif

install-data-hook:
if SET_GID
	chown -R root:@GAMEGROUP@ "$(DESTDIR)$(oangbanddir)"
	chmod -R 070 "$(DESTDIR)$(oangbanddir)"
endif
