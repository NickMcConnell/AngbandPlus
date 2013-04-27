## Makefile.am -- Process this file with automake to process Makefile.in
## stat/item/object

ironhells_files = \
	object.txt

EXTRA_DIST = \
	$(ironhells_files)

ironhellsdir = @DATA_PATH@stat/item/object

ironhells_DATA = \
	$(ironhells_files)

install-data-hook:
	echo "stat/item/objectdir = $(ironhellsdir)"
if SET_GID
#	chown -R root.@GAMEGROUP@ "$(DESTDIR)$(ironhellsdir)"
#	chmod -R g+w "$(DESTDIR)$(ironhellsdir)"
endif

