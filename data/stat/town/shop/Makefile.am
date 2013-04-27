## Makefile.am -- Process this file with automake to process Makefile.in
## stat/town/shop

ironhells_files = \
	cost_adj.txt \
	shop_own.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/town/shop

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/town/shopdir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

