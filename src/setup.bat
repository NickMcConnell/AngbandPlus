call cleansetup.bat
copy ..\doc\manual\*.* ..\setup\doc\manual
copy ..\lib\edit\*.* ..\setup\lib\edit
copy ..\lib\file\*.* ..\setup\lib\file
copy ..\lib\help\*.* ..\setup\lib\help
copy ..\lib\pref\*.* ..\setup\lib\pref
copy ..\lib\save\server.level.* ..\setup\lib\save
copy ..\lib\user\user.prf ..\setup\lib\user
copy ..\lib\xtra\font\*.* ..\setup\lib\xtra\font
copy ..\lib\xtra\graf\*.* ..\setup\lib\xtra\graf
copy ..\lib\xtra\icon\*.* ..\setup\lib\xtra\icon
copy ..\lib\xtra\sound\*.* ..\setup\lib\xtra\sound
copy ..\noteye\*.* ..\setup\noteye
copy *.* ..\setup\src
copy client\*.* ..\setup\src\client
copy common\*.* ..\setup\src\common
copy curses\*.* ..\setup\src\curses
copy SDL\*.* ..\setup\src\SDL
copy server\*.* ..\setup\src\server
copy server\monster\*.* ..\setup\src\server\monster
copy server\object\*.* ..\setup\src\server\object
copy server\player\*.* ..\setup\src\server\player
copy win\*.* ..\setup\src\win
pause
call server.bat
call client.bat
copy ..\copying.txt ..\setup
copy ..\noteye.bat ..\setup
copy ..\*.dll ..\setup
copy ..\*.exe ..\setup
pause