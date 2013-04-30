del client\main.obj
make -f makefile.gcu
pause
del client\main.obj
make -f makefile.sdl
pause
make -f makefile.bcc
pause
move mangclient_gcu.exe ..\mangclient_gcu.exe
move mangclient_sdl.exe ..\mangclient_sdl.exe
move mangclient.exe ..\mangclient.exe