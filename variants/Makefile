CLEANFILES=*.o *.so *.fasl *.faslmt *.x86f *.err *.fas *.fsl *.ufsl *.lib *.sparcf *~ *.dfsl

all: 


lisp-clean:
	cd vanilla && $(RM) $(CLEANFILES)
	cd vanilla/config && $(RM) $(CLEANFILES)
	if test -d contraband; then cd contraband && $(MAKE) lisp-clean; fi

clean: lisp-clean
	$(RM) $(CLEANFILES)
