## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  bone.025 bone.030 bone.035 bone.040 bone.045 bone.050 \
  bone.055 bone.060 bone.065 bone.070 bone.075 bone.080 \
  bone.085 bone.090 bone.095

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@bone

faangband_DATA = \
  $(faangband_files)
endif

install-data-hook:
if SET_GID
	chown -R root:@GAMEGROUP@ "$(DESTDIR)$(faangbanddir)"
	chmod -R 070 "$(DESTDIR)$(faangbanddir)"
endif

