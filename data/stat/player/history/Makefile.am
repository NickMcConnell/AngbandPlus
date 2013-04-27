## Makefile.am -- Process this file with automake to process Makefile.in
## stat/player/history

ironhells_files = \
	p_hist.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/player/history

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/player/historydir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

