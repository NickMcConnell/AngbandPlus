call cleansetup.bat
copy ..\doc\manual\*.* ..\setup\doc\manual
copy ..\lib\readme.txt ..\setup\lib
copy ..\lib\customize\*.* ..\setup\lib\customize
copy ..\lib\fonts\*.* ..\setup\lib\fonts
copy ..\lib\gamedata\*.* ..\setup\lib\gamedata
copy ..\lib\help\*.* ..\setup\lib\help
copy ..\lib\icons\*.* ..\setup\lib\icons
copy ..\lib\screens\*.* ..\setup\lib\screens
copy ..\lib\sounds\*.* ..\setup\lib\sounds
copy ..\lib\tiles\*.* ..\setup\lib\tiles
copy ..\lib\tiles\adam-bolt\*.* ..\setup\lib\tiles\adam-bolt
copy ..\lib\tiles\gervais\*.* ..\setup\lib\tiles\gervais
copy ..\lib\tiles\nomad\*.* ..\setup\lib\tiles\nomad
copy ..\lib\tiles\old\*.* ..\setup\lib\tiles\old
copy ..\lib\tiles\pseudo\*.* ..\setup\lib\tiles\pseudo
copy ..\lib\tiles\shockbolt\*.* ..\setup\lib\tiles\shockbolt
copy ..\lib\user\save\server.town.* ..\setup\lib\user\save
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
copy ..\copying.txt ..\setup
copy ..\noteye.bat ..\setup
copy ..\*.dll ..\setup
copy ..\*.exe ..\setup
pause