## Makefile.am -- Process this file with automake to process Makefile.in

ironhells_files = \
	delete.me

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@lib/data

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
if SET_GID
	if test "@GAMEGROUP@" != ""; then \
	  touch "$(DESTDIR)$(ironhellsdir)/artifact.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/cost_adj.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/ego_item.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/flavor.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/limits.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/monster.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/object.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/p_class.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/p_hist.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/p_race.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/prices.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/shop_own.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/terrain.raw" && \
	  touch "$(DESTDIR)$(ironhellsdir)/vault.raw" && \
	  chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)" && \
	  chmod -R g+w "$(DESTDIR)$(ironhellsdir)" ;\
	fi
endif

