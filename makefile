# File: Makefile
#
# this is the main makefile for angband/64. This will help you if you want to develop the game
# in an environment like mine in Linux.
#
# if you just want to compile the sources, try the makefiles in src/.
#

#
# Some definitions
#

PACKAGE 	= angband64
SHORTVERSION    = 7
VERSION 	= beta-$(SHORTVERSION)
RELEASE         = 3
ARCH		= i386
PKG		= $(PACKAGE)-$(VERSION)-$(RELEASE)
SRC_PKG 	= $(PKG).src.tar
RPM_PKG 	= $(PKG).$(ARCH).rpm
PREFIX		= /usr
BIN		= $(PREFIX)/bin
STORE_DIR       = ./upload/
BIN_DOS_ZIP     = a64bn_$(SHORTVERSION)$(RELEASE)
BIN_WIN32_ZIP   = a64bn_win32_$(SHORTVERSION)$(RELEASE)
SRC_ZIP         = a64sr_$(SHORTVERSION)$(RELEASE)
LIB_ZIP		= a64lb_$(SHORTVERSION)$(RELEASE)

#
# Build the "Angband" program
#
help:
	@echo "Try make install to install binaries on linux"
	@echo "    make backup     create a backup of the current sources/libraries"
	@echo "    make clean      remove any temporary files"
	@echo "    make depend     create the dependency file"
	@echo "    make dist       create everything for an upload"
	@echo "    make html       update the html-pages"
	@echo "    make dosbin     create a DOS executable using the msdosdjgpp-crosscompiler"
	@echo "    make win32bin   create a Win32 executable using the cygwin32-crosscompiler"
	@echo "    make dossrc     create a DOS source archive (zip-file)"
	@echo "    make libdist    create a libraries archive (zip-file)"
	@echo "    make linuxsrc   create a unix source archive (.tar.bz2 file)"
	@echo "    make linuxrpm   create a unix binary file (rpm archive)"
	@echo "    make angbnd64   compile the unix-executable"
	@echo ""

# build the linux executable
# defining MAKE_DIST so no debugging should be on!
angbnd64:
	@{ \
	cd src ; \
	make -j 5 EXTRAFLAGS="-DMAKE_DIST"; \
	cd .. ; \
	}

# install the linux executable
install:	angbnd64
	sudo rm -f $(BIN)/$(PACKAGE)-$(VERSION)-$(RELEASE)
	sudo install -m 0755 src/angbnd64 $(BIN)/$(PACKAGE)-$(VERSION)-$(RELEASE)

# a quick and dirty backup
backup: clean
	tar cvf backupfile.tar `ls ./ | egrep -v "backup|upload|save|old_releases"` ;
	bzip2 -vv -9 backupfile.tar
	cp backupfile.tar.bz2 backup/$(SRC_ZIP)-`date +%y%m%d_%H%M`.tar.bz2
	mv backupfile.tar.bz2 /dos/d/dosgames/ang/myang/backup/`date +%m%d%H%M`.tb2

# Clean up old junk, also remove lib/save/
realclean: clean
	rm -f lib/data/*

# Clean up old junk, keep lib/save
clean:
	@rm -f lib/data/* src/*.o alog* olog* *.log src/*.bin src/angbnd64.exe src/angbnd64.upx src/angbnd64 
	@rm -f src/angband src/.depend ang*.spec make.err src/make.err backupfile.tar.bz2 files.txt angband.exe
	@rm -f src/angband.exe src/depend.gnu src/cscope.out
	@rm -f *zip 
	@rm -f lib/temp/index*
	find . -name "*~" -print | xargs rm -f
	find . -name "core" -print | xargs rm -f
	touch src/*.c
	touch src/*.h
	@./protect_empty_dirs.sh

# rebuild dependencies
depend:
	@{ \
	cd src ; \
	make depend; \
	cd .. ; \
	}

# make everything right for distribution	
dist:	version libdist linuxrpm dosbin win32bin linuxsrc dossrc html

# update the version file, the html-files, 
version:
	echo "/* automatically generated version file */" > src/version.h
	echo "#define VERSION_BETA " $(SHORTVERSION) >> src/version.h
	echo "#define VERSION_RELEASE " $(RELEASE) >> src/version.h
	cat lib/file/newsbase.txt |\
        sed -e"s/__shortdate__/$(shell date +'%d %b %Y')/g" |\
	sed -e"s/__version__/$(SHORTVERSION)/g;s/__release__/$(RELEASE)/g" > lib/file/news.txt
	cat baseang.sh | \
	sed -e"s/__version__/$(SHORTVERSION)/g" |\
	sed -e"s/__release__/$(RELEASE)/g" > ang.sh

# update the version file, the html-files, 
html:
#	LEN_BIN_DOS := $(shell echo `du -hs upload/$(BIN_DOS_ZIP).zip | cut -f1`)
	cat baseang64.html | \
	sed -e"s/__date__/$(shell date)/g;\
		s/__version__/$(VERSION)/g;\
		s/__release__/$(RELEASE)/g;\
		s/__dosbin__/$(BIN_DOS_ZIP).zip/g;\
		s/__lendosbin__/$(shell echo `du -hs upload/$(BIN_DOS_ZIP).zip | cut -f1`)/g;\
		s/__win32bin__/$(BIN_WIN32_ZIP).zip/g;\
		s/__lenwin32bin__/$(shell echo `du -hs upload/$(BIN_WIN32_ZIP).zip | cut -f1`)/g;\
		s/__dossrc__/$(SRC_ZIP).zip/g;\
		s/__lendossrc__/$(shell echo `du -hs upload/$(SRC_ZIP).zip | cut -f1`)/g;\
		s/__libdist__/$(LIB_ZIP).zip/g;\
		s/__lenlibdist__/$(shell echo `du -hs upload/$(LIB_ZIP).zip | cut -f1`)/g;\
		s/__linuxbin__/$(RPM_PKG)/g;\
		s/__lenlinuxbin__/$(shell echo `du -hs upload/$(RPM_PKG) | cut -f1`)/g;\
		s/__linuxsrc__/$(SRC_PKG).bz2/g;\
		s/__lenlinuxsrc__/$(shell echo `du -hs upload/$(SRC_PKG).bz2 | cut -f1`)/g;" > upload/ang64.html

#make a dos binary using the makefile.linux_msdosdjgpp for a linux-to-dos/msdosdjgpp cross-compiler
dosbin:
	make clean
	@{ cd src ; mmake -j 5 -fmakefile.linux_msdosdjgpp EXTRAFLAGS="-DMAKE_DIST" ; cd .. ; }
# use compression program upx to get a very small binary
	upx -8 --mono ./angband.exe
	zip -9 $(BIN_DOS_ZIP) ./angband.exe;
	zip -9 -l $(BIN_DOS_ZIP) ./QuickStart ./changes.* ./readme ./guide.txt ./todo.txt ./angdos.cfg;
	cp $(BIN_DOS_ZIP).zip /dos/d/dosgames/ang64/
	mv $(BIN_DOS_ZIP).zip $(STORE_DIR)

#make a win32 binary
win32bin:
	make clean
	rm -f src/depend.gnu
	@{ cd src ; mmake -j 5 -fmakefile.linux_cygwin32 depend.gnu ; cd .. ; }
	@{ cd src ; mmake -j 5 -fmakefile.linux_cygwin32 EXTRAFLAGS="-DMAKE_DIST" ; cd .. ; }
# use compression program upx to get a very small binary
	upx -8 --mono ./angband.exe
	cp /usr/local/cygb20/bin/cygwin1_stripped.dll ./cygwin1.dll
	zip -9 $(BIN_WIN32_ZIP) ./angband.exe ./cygwin1.dll;
        # translate unix end-of-line to DOS end-of-line here
	zip -9 -l $(BIN_WIN32_ZIP) ./angband.ini ./guide.txt ./todo.txt ./readme ./changes.* ./QuickStart;
	cp -v $(BIN_WIN32_ZIP).zip /dos/e/ang64/
	rm ./cygwin1.dll
	mv $(BIN_WIN32_ZIP).zip $(STORE_DIR)

# build a dos source archive
dossrc:
	make clean
	find src/ ./ -name "*" -maxdepth 1 -type f -print | egrep -v "^src/do|^\./base|\./*spec" | xargs zip $(SRC_ZIP)
	cp -v $(SRC_ZIP).zip /dos/d/dosgames/ang64/
	mv $(SRC_ZIP).zip $(STORE_DIR)

# build a libraries archive
libdist:
	make clean
	find lib/ -name "*" -not -name "*.spo" -not -name "*.hdr" -not -name "*.raw" -not -name "[0-9][0-9][0-9].*" -not -name "crash*" -print | xargs zip -9 $(LIB_ZIP)
	cp -v $(LIB_ZIP).zip /dos/d/dosgames/ang64/
	cp -v $(LIB_ZIP).zip /dos/e/ang64/
	mv $(LIB_ZIP).zip $(STORE_DIR)

# build a linux executable
linuxbin:
	make 'MAKE=make -j5' install C_CHECK_FLAGS=""
	make clean
	find . -maxdepth 1 -type f | egrep -v "log|exe" | xargs tar cvf a64bn_$(VERSION)_$(RELEASE).tar
	tar cvfP a64bn_$(VERSION)_$(RELEASE).tar /usr/bin/$(PACKAGE)-$(VERSION)-$(RELEASE)
	gzip -9 ./a64bn_$(VERSION)_$(RELEASE).tar
	mv ./a64bn_$(VERSION)_$(RELEASE).tar.gz upload/

# build a linux executable
linuxrpm:
	make install
	make clean
	cat base.spec | \
	sed -e"s/__date__/$(shell date)/g;s/__version__/$(SHORTVERSION)/g;s/__release__/$(RELEASE)/g" |\
	sed -e"s/__dosbin__/$(BIN_DOS_ZIP).zip/g;s/__win32bin__/$(BIN_WIN32_ZIP).zip/g;" |\
	sed -e"s/__dossrc__/$(SRC_ZIP).zip/g;s/__libdist__/$(LIB_ZIP).zip/g;s/__linuxbin__/$(RPM_PKG)/g" |\
	sed -e"s/__linuxsrc__/$(SRC_PKG).bz2/g" > $(PACKAGE)-$(VERSION)-$(RELEASE).spec
	echo "/usr/bin/$(PACKAGE)-$(VERSION)-$(RELEASE)" >> $(PACKAGE)-$(VERSION)-$(RELEASE).spec
	find /usr/local/games/ang64/* -maxdepth 0 -type f | egrep -v "log|exe" >> $(PACKAGE)-$(VERSION)-$(RELEASE).spec
	sudo cp /usr/bin/$(PACKAGE)-$(VERSION)-$(RELEASE) /usr/src/packages/SOURCES/
#	unset LD_ELF_PRELOAD ; rpm_old --rcfile /home/jurriaan/rpm_oldrc -bb $(PKG).spec
	sudo rpm -bb -vv $(PKG).spec
	mv /usr/src/packages/RPMS/$(ARCH)/$(RPM_PKG) $(STORE_DIR)


# build a linux source archive
linuxsrc:
	make clean
	tar cvf ../$(SRC_PKG) `ls ./ | egrep -v "backup|upload|lib|exe|extra|old_releases"` ; \
	mv ../$(SRC_PKG) ./ ; \
	bzip2 -v -9 $(SRC_PKG)
	mv $(SRC_PKG).bz2 $(STORE_DIR)

ifeq (src/.depend, $(wildcard src/.depend))
  include src/.depend
endif
