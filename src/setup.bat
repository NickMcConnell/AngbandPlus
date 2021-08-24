call cleansetup.bat
copy ..\docs\*.* ..\setup\docs
copy ..\docs\_static\*.* ..\setup\docs\_static
copy ..\docs\_templates\*.* ..\setup\docs\_templates
copy ..\docs\hacking\*.* ..\setup\docs\hacking
copy ..\lib\readme.txt ..\setup\lib
copy ..\lib\customize\*.* ..\setup\lib\customize
copy ..\lib\fonts\*.* ..\setup\lib\fonts
copy ..\lib\gamedata\*.* ..\setup\lib\gamedata
copy ..\lib\help\*.* ..\setup\lib\help
copy ..\lib\icons\*.* ..\setup\lib\icons
copy ..\lib\screens\*.* ..\setup\lib\screens
copy ..\lib\sounds\*.* ..\setup\lib\sounds
copy ..\lib\tiles\*.* ..\setup\lib\tiles
copy ..\lib\tiles\gervais\*.* ..\setup\lib\tiles\gervais
copy ..\lib\tiles\shockbolt\*.* ..\setup\lib\tiles\shockbolt
copy ..\lib\user\save\*.* ..\setup\lib\user\save
copy ..\noteye\*.* ..\setup\noteye
copy *.* ..\setup\src
copy client\*.* ..\setup\src\client
copy common\*.* ..\setup\src\common
copy curses\*.* ..\setup\src\curses
copy fix\*.* ..\setup\src\fix
copy SDL\*.* ..\setup\src\SDL
copy server\*.* ..\setup\src\server
copy win\*.* ..\setup\src\win
pause
call server.bat
call client.bat
copy ..\changes.txt ..\setup
copy ..\noteye.bat ..\setup
copy ..\*.dll ..\setup
copy ..\*.exe ..\setup
copy ..\Manual.html ..\setup
pause