## Makefile.am -- Process this file with automake to process Makefile.in

angband_files = \
  delete.me

EXTRA_DIST = \
  $(angband_files)

if SET_GID
angbanddir = @DEFAULT_PATH@data

angband_DATA = \
  $(angband_files)
endif


install-data-hook:
if SET_GID
	if test "@GAMEGROUP@" != ""; then \
	  touch "$(DESTDIR)$(angbanddir)/a_info.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/e_info.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/f_info.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/k_info.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/r_info.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/v_info.raw" && \
	  chown -R root.@GAMEGROUP@ "$(DESTDIR)$(angbanddir)" && \
	  chmod -R g+w "$(DESTDIR)$(angbanddir)" ;\
	fi
endif
