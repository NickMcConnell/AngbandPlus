# Really simple Makefile which works ok on Linux

CLEANFILES=*.o *.so *.fasl *.x86f *.err *.fas *.ufsl *.lib *.sparcf

CLISP_PATH=/usr/lib/clisp
CLISP_LINKKIT=${CLISP_PATH}/linkkit
CLISP_LINKER=${CLISP_PATH}/clisp-link
CLISP_BASE=${CLISP_PATH}/base
LINK_INFO_DIR=linking
LINK_TARGET_DIR=linked
#THIS_DIR=$(PWD)

all: 
	cd zterm; $(MAKE) all

lisp-clean:
	cd lib; $(MAKE) lisp-clean
	cd tools; $(MAKE) clean
	cd binary-types && $(RM) $(CLEANFILES)
	cd tests && $(RM) $(CLEANFILES)
	cd ffi && $(RM) $(CLEANFILES)
	cd variants; $(MAKE) lisp-clean
	$(RM) $(CLEANFILES) *.data prof.dump

packages:
	dpkg-buildpackage -rfakeroot -us -uc

clean: lisp-clean
#	cd doc; $(MAKE) clean
	cd zterm; $(MAKE) clean
	cd lib; $(MAKE) clean
	cd variants; $(MAKE) clean

	rm -rf ${LINK_TARGET_DIR}

clisp-link:
	cd ${LINK_INFO_DIR}; $(MAKE) all
	rm -rf ${LINK_TARGET_DIR}
	${CLISP_LINKER} add-module-set ${LINK_INFO_DIR} ${CLISP_BASE} ${LINK_TARGET_DIR}

export CLISP_LINKKIT

