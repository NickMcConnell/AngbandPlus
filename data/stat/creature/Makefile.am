## Makefile.am -- Process this file with automake to process Makefile.in
## stat/creature

ironhells_files = \
	monster.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/creature

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "creaturedir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

