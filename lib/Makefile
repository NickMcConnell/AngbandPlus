CLEANFILES=*.o *.so *.fasl *.x86f *.err

all: 
	cd zterm; $(MAKE) all

lisp-clean:
	cd common && $(RM) $(CLEANFILES)
	cd compat && $(RM) $(CLEANFILES)
	cd vanilla && $(RM) $(CLEANFILES)
	cd foreign && $(RM) $(CLEANFILES)
	$(RM) $(CLEANFILES)

clean: lisp-clean
	cd zterm; $(MAKE) clean

