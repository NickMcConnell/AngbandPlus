## Makefile.am -- Process this file with automake to process Makefile.in
## stat/dungeon/normal

ironhells_files = \
	delete.me

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/dungeon/normal

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "dungeonnormaldir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

