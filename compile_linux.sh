#!/bin/sh
rm friendband_linux_64
cd src
make -f Makefile.std clean
make -f Makefile.std
cp friendband_linux_64 ../
cd ..
ddd friendband_linux_64
