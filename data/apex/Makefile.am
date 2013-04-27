## Makefile.am -- Process this file with automake to process Makefile.in
## apex

ironhells_files = \
	delete.me

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@apex

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
if SET_GID
	touch "$(DESTDIR)$(ironhellsdir)/scores.raw"
	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

