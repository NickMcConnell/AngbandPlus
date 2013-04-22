# File: Makefile.lsl

# Purpose: Makefile for Linux + SVGA library

include Srcobj.make

CC = gcc

CFLAGS = -O2 -fno-strength-reduce -Wall -D"USE_LSL"

LIBS = -lz -lvgagl -lvga



# 
# Build the "Angband" program 
# 
angband: $(OBJS) 
	$(CC) $(CFLAGS) -o angband $(OBJS) $(LDFLAGS) $(LIBS) 


#
# install Angband
#
install: angband
	cp angband ..

#
# Clean up old junk
#
clean:
	-rm -f *.o angband
	-rm -f ./lua/*.o ./lua/tolua


#
# Generate dependencies automatically
#
depend:
	makedepend -D__MAKEDEPEND__ $(SRCS)

#
# Lua stuff
#

lua/tolua: $(TOLUAOBJS)
	$(CC) -o lua/tolua $(TOLUAOBJS) $(LDFLAGS) $(LIBS)


#
# Hack -- some file dependencies
#

include Dep.make