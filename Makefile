# Really simple Makefile which works ok on Linux

CLISP_PATH=/usr/lib/clisp
CLISP_LINKKIT=${CLISP_PATH}/linkkit
CLISP_LINKER=${CLISP_PATH}/clisp-link
CLISP_BASE=${CLISP_PATH}/base
LINK_INFO_DIR=linking
LINK_TARGET_DIR=linked
#THIS_DIR=$(PWD)

all: libraries 


libraries:
	cd lib; $(MAKE) all

lisp-clean:
	cd lib; $(MAKE) lisp-clean
	cd tools; $(MAKE) clean
	$(RM) *.o *.so *.fasl *.fas *.ufsl *.lib *.x86f *.err *.data prof.dump

clean: lisp-clean
#	cd doc; $(MAKE) clean
	cd lib; $(MAKE) clean
	rm -rf ${LINK_TARGET_DIR}

clisp-link:
	cd ${LINK_INFO_DIR}; $(MAKE) all
	rm -rf ${LINK_TARGET_DIR}
	${CLISP_LINKER} add-module-set ${LINK_INFO_DIR} ${CLISP_BASE} ${LINK_TARGET_DIR}

export CLISP_LINKKIT

