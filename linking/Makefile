# Makefile for CLISP module set linking

CLISP = clisp
CLISP_PATH=/usr/lib/clisp
CLISP_LINKKIT=${CLISP_PATH}/linkkit
CLISP_LINKER=${CLISP_PATH}/clisp-link

CC = gcc
CFLAGS =
INCLUDES=${CLISP_LINKKIT}



SHELL = /bin/sh

all: clisp-module

libs:
	-ln -s ../zterm/liblangband_ui.so liblangband_ui.so
	-ln -s ../zterm/liblangband_dc.so liblangband_dc.so

ffi-clisp.c:
	-ln -s ../ffi/ffi-clisp.c ffi-clisp.c

ffi-clisp.o : ffi-clisp.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c ffi-clisp.c

clisp-module : libs ffi-clisp.o

