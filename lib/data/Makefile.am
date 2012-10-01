## Makefile.am -- Process this file with automake to process Makefile.in

oangband_files = \
  delete.me

EXTRA_DIST = \
  $(oangband_files)

if SET_GID
oangbanddir = @DEFAULT_PATH@data

oangband_DATA = \
  $(oangband_files)
endif


install-data-hook:
if SET_GID
	if test "@GAMEGROUP@" != ""; then \
	  touch "$(DESTDIR)$(oangbanddir)/artifact.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/cost_adj.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/ego_item.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/monster.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/object.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/p_hist.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/p_race.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/prices.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/shop_own.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/terrain.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/themed.raw" && \
	  touch "$(DESTDIR)$(oangbanddir)/vault.raw" && \
	  chown -R root:@GAMEGROUP@ "$(DESTDIR)$(oangbanddir)" && \
	  chmod -R g+w "$(DESTDIR)$(oangbanddir)" ;\
	fi
endif

