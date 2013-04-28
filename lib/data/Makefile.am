## Makefile.am -- Process this file with automake to process Makefile.in

angbanddir = $(datadir)/games/$(PACKAGE)/data

clean-local:
	-rm *.raw

install-data-hook:
	mkdir -p "$(DESTDIR)$(angbanddir)"
	if test "@GAMEGROUP@" != ""; then \
	  touch "$(DESTDIR)$(angbanddir)/artifact.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/cost_adj.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/ego_item.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/flavor.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/limits.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/monster.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/object.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/p_class.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/p_hist.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/p_race.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/shop_own.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/terrain.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/uniq_art.raw" && \
	  touch "$(DESTDIR)$(angbanddir)/vault.raw" && \
	  chown -R root:games "$(DESTDIR)$(angbanddir)" && \
	  chmod -R g+w "$(DESTDIR)$(angbanddir)" ;\
	fi
