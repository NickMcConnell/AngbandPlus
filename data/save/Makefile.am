## Makefile.am -- Process this file with automake to process Makefile.in
## save

ironhells_files = \
	delete.me

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@save

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "savedir = $(savedir)"
#	touch "$(DESTDIR)$(ironhellsdir)/delete.me"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

