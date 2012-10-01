## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  bone.030 bone.035 bone.040 bone.045 bone.050 bone.055 \
  bone.060 bone.065 bone.070 bone.075 bone.080 bone.085 \
  bone.090 bone.095

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@bone

oangband_DATA = \
  $(oangband_files)
endif

install-data-hook:
if SET_GID
	chown -R root:@GAMEGROUP@ "$(DESTDIR)$(oangbanddir)"
	chmod -R 070 "$(DESTDIR)$(oangbanddir)"
endif

