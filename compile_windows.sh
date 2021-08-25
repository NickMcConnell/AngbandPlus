#!/bin/sh
rm friendband_windows_64.exe
cd src
make -f Makefile.cyg clean
make -f Makefile.cyg
cp friendband_windows_64 ../friendband_windows_64.exe
cd ..
wine friendband_windows_64.exe
