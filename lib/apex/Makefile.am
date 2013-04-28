## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  delete.me

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@apex

faangband_DATA = \
  $(faangband_files)
endif

install-data-hook:
if SET_GID
	touch "$(DESTDIR)$(faangbanddir)/scores.raw"
	chown -R root:@GAMEGROUP@ "$(DESTDIR)$(faangbanddir)"
	chmod -R g+w "$(DESTDIR)$(faangbanddir)"
endif

