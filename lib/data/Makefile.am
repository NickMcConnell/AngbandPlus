## Makefile.am -- Process this file with automake to process Makefile.in

faangband_files = \
  delete.me

EXTRA_DIST = \
  $(faangband_files)

if SET_GID
faangbanddir = @DEFAULT_PATH@data

faangband_DATA = \
  $(faangband_files)
endif


install-data-hook:
if SET_GID
	if test "@GAMEGROUP@" != ""; then \
	  touch "$(DESTDIR)$(faangbanddir)/artifact.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/cost_adj.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/ego_item.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/monster.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/object.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/p_hist.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/p_race.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/prices.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/shop_own.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/terrain.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/themed.raw" && \
	  touch "$(DESTDIR)$(faangbanddir)/vault.raw" && \
	  chown -R root:@GAMEGROUP@ "$(DESTDIR)$(faangbanddir)" && \
	  chmod -R g+w "$(DESTDIR)$(faangbanddir)" ;\
	fi
endif

