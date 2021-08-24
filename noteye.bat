echo off

set /P NOTEYE_HOME=Full path of the Necklace of the Eye frontend: 
set /P UPDATE_FILES=Update PWMAngband files (y/N)? 

if not exist "%NOTEYE_HOME%\noteye.exe" goto nohome
if not exist "%NOTEYE_HOME%\pwmangband\pwmangband.exe" goto install
if "%UPDATE_FILES%" == "y" goto install
goto run

:install
copy /Y noteye\noteye.noe "%NOTEYE_HOME%\common"
copy /Y noteye\pwmangband.noe "%NOTEYE_HOME%\games"
mkdir "%NOTEYE_HOME%\pwmangband"
xcopy /E /Y /I lib "%NOTEYE_HOME%\pwmangband\lib"
copy /Y noteye\font-gcu.prf "%NOTEYE_HOME%\pwmangband\lib\customize"
copy /Y noteye\pwmangband.ini "%NOTEYE_HOME%\pwmangband"
copy /Y pdcurses.dll "%NOTEYE_HOME%\pwmangband"
copy /Y mangclient_gcu.exe "%NOTEYE_HOME%\pwmangband\pwmangband.exe"
pause

:run
cd "%NOTEYE_HOME%"
noteye
goto end

:nohome
echo Please install the Necklace of the Eye frontend first
pause

:end