CLEANFILES=*.o *.so *.fasl *.x86f *.err *.fas *.fsl *.ufsl *.lib *.sparcf *~ *.dfsl

all: 


lisp-clean:
	cd vanilla && $(RM) $(CLEANFILES)
	cd vanilla/config && $(RM) $(CLEANFILES)
	cd contraband && $(RM) $(CLEANFILES)
	cd contraband/config && $(RM) $(CLEANFILES)

clean: lisp-clean
	$(RM) $(CLEANFILES)
