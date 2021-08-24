del client\c-cmd.obj
del client\main.obj
del client\set_focus.obj
del client\sound-core.obj
del client\ui-display.obj
make -f makefile.gcu
pause
del client\c-cmd.obj
del client\main.obj
del client\set_focus.obj
del client\sound-core.obj
del client\ui-display.obj
make -f makefile.sdl
pause
del client\c-cmd.obj
del client\main.obj
del client\set_focus.obj
del client\sound-core.obj
del client\ui-display.obj
make -f makefile.bcc
pause
move mangclient_gcu.exe ..\mangclient_gcu.exe
move mangclient_sdl.exe ..\mangclient_sdl.exe
move mangclient.exe ..\mangclient.exe