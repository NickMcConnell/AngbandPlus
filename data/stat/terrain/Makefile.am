## Makefile.am -- Process this file with automake to process Makefile.in
## stat/terrain

ironhells_files = \
	terrain.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/terrain

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/terraindir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

