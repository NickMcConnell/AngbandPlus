## Makefile.am -- Process this file with automake to process Makefile.in
## stat

SUBDIRS = creature dungeon item player terrain town

ironhells_files = \
	limits.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "statdir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

