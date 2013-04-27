## Makefile.am -- Process this file with automake to process Makefile.in
## stat/item/flavor

ironhells_files = \
	flavor.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/item/flavor

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/item/flavordir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

