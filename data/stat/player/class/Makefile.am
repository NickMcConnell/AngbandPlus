## Makefile.am -- Process this file with automake to process Makefile.in
## stat/player/class

ironhells_files = \
	p_class.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/player/class

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/player/classdir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

