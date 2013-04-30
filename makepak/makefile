#
# makefile for makepak
#
# Author: Hj. Malthaner
# Date:   05-Mar-2002
# Update: 12-Dec-2003

CC=gcc
LN=gcc

CFLAGS=-O

SRC=dr_rdppm.c makepak.c
OBJ=dr_rdppm.o makepak.o

all: $(OBJ)
	$(LN) -o makepak $(OBJ)


clean:
	rm *.o