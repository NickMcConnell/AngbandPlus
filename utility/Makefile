#
# Generic Makefile 
# compiles every *.c source in the directory
# 
# Marco Vecchiocattivi - 2002
#

CC = gcc

PROGS = $(patsubst %.c,%,$(wildcard *.c))
OBJS = $(patsubst %.c,%.o,$(wildcard *.c)) 
#STYLE_OPT= -bad -bap -bbb -bl -bli0 -cli4 -cbi0 -cs -npcs -nbc -i4 -nbbo 
STYLE_OPT= -gnu -i4 -bl -bli0

CFLAGS = -Wall -O2 -ggdb
# CFLAGS = -Werror -Wall -O2 -ggdb

all: ${PROGS}

clean:
	rm -f ${PROGS} ${OBJS}
	(cd ..; rm -f  ${PROGS})

indent:
	indent $(wildcard *.c) ${STYLE_OPT} 

install:
	cp $(PROGS) ../
