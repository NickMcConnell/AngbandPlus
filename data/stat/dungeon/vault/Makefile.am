## Makefile.am -- Process this file with automake to process Makefile.in
## stat/dungeon/vault

ironhells_files = \
	vault.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/dungeon/vault

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/dungeon/vaultdir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

