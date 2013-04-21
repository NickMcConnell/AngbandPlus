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
	-ln -s ../lib/zterm/liblang_ui.so liblang_ui.so

gen-clisp.c:
	-ln -s ../lib/foreign/gen-clisp.c gen-clisp.c

gen-clisp.o : gen-clisp.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c gen-clisp.c

clisp-module : libs gen-clisp.o
