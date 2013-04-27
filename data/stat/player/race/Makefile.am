## Makefile.am -- Process this file with automake to process Makefile.in
## stat/player/race

ironhells_files = \
	p_race.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/player/race

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/player/racedir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

