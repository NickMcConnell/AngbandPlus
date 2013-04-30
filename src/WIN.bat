cd ..\..\zlib-1.2.7
make -f win32\Makefile.bor
pause
copy zlib.lib ..\PWMAngband\src\win
pause
cd ..\lpng1512
make -f scripts\makefile.bc32
pause
copy libpng.lib ..\PWMAngband\src\win
pause
copy png.h ..\PWMAngband\src\win
copy pnglibconf.h ..\PWMAngband\src\win
copy pngconf.h ..\PWMAngband\src\win
pause