# Really simple Makefile which works ok on Linux

all: 
	cd lib; $(MAKE) all

clean:
	cd lib; $(MAKE) clean
	cd doc; $(MAKE) clean
	$(RM) *.o *.so *.fasl *.x86f *.err

