<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating command line "rc.exe /l 0x409 /fo"ARMV4Rel/angband.res" /i "src" /d UNDER_CE=420 /d _WIN32_WCE=420 /d "NDEBUG" /d "UNICODE" /d "_UNICODE" /d "WIN32_PLATFORM_PSPC=400" /d "ARM" /d "_ARM_" /d "ARMV4" /r "C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\angband.rc"" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP3CDB.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "NDEBUG" /Fp"ARMV4Rel/FA.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\angbandcw.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\attack.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd3.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd4.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd5.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd6.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\dungeon.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\emulfunc.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\files.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\generate.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\info.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\load.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monattk.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monmove.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\object1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\object2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\randart.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\readdib.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\save.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\spells1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\spells2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\squelch.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\store.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\tables.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\variable.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\wizard1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\wizard2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-form.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-rand.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-term.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-virt.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP3CDB.tmp" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP3CDC.tmp" with contents
[
commctrl.lib coredll.lib aygshell.lib /nologo /base:"0x00010000" /stack:0x10000,0x1000 /entry:"WinMainCRTStartup" /incremental:no /pdb:"ARMV4Rel/FA.pdb" /nodefaultlib:"libc.lib /nodefaultlib:libcd.lib /nodefaultlib:libcmt.lib /nodefaultlib:libcmtd.lib /nodefaultlib:msvcrt.lib /nodefaultlib:msvcrtd.lib" /out:"ARMV4Rel/FA.exe" /subsystem:windowsce,4.20 /align:"4096" /MACHINE:ARM 
".\ARMV4Rel\angbandcw.obj"
".\ARMV4Rel\attack.obj"
".\ARMV4Rel\birth.obj"
".\ARMV4Rel\cave.obj"
".\ARMV4Rel\cmd1.obj"
".\ARMV4Rel\cmd2.obj"
".\ARMV4Rel\cmd3.obj"
".\ARMV4Rel\cmd4.obj"
".\ARMV4Rel\cmd5.obj"
".\ARMV4Rel\cmd6.obj"
".\ARMV4Rel\dungeon.obj"
".\ARMV4Rel\emulfunc.obj"
".\ARMV4Rel\files.obj"
".\ARMV4Rel\generate.obj"
".\ARMV4Rel\info.obj"
".\ARMV4Rel\init1.obj"
".\ARMV4Rel\init2.obj"
".\ARMV4Rel\load.obj"
".\ARMV4Rel\main-wce.obj"
".\ARMV4Rel\main.obj"
".\ARMV4Rel\monattk.obj"
".\ARMV4Rel\monmove.obj"
".\ARMV4Rel\monster1.obj"
".\ARMV4Rel\monster2.obj"
".\ARMV4Rel\object1.obj"
".\ARMV4Rel\object2.obj"
".\ARMV4Rel\randart.obj"
".\ARMV4Rel\readdib.obj"
".\ARMV4Rel\save.obj"
".\ARMV4Rel\spells1.obj"
".\ARMV4Rel\spells2.obj"
".\ARMV4Rel\squelch.obj"
".\ARMV4Rel\store.obj"
".\ARMV4Rel\tables.obj"
".\ARMV4Rel\util.obj"
".\ARMV4Rel\variable.obj"
".\ARMV4Rel\wizard1.obj"
".\ARMV4Rel\wizard2.obj"
".\ARMV4Rel\xtra1.obj"
".\ARMV4Rel\xtra2.obj"
".\ARMV4Rel\z-form.obj"
".\ARMV4Rel\z-rand.obj"
".\ARMV4Rel\z-term.obj"
".\ARMV4Rel\z-util.obj"
".\ARMV4Rel\z-virt.obj"
".\ARMV4Rel\angband.res"
]
Creating command line "link.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP3CDC.tmp"
<h3>Output Window</h3>
Compiling resources...
Compiling...
angbandcw.c
attack.c
birth.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(780) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(785) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(786) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cave.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4542) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4543) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4672) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4673) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd1.c
cmd2.c
cmd3.c
cmd4.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd4.c(5764) : warning C4244: 'initializing' : conversion from 'unsigned long ' to 'char ', possible loss of data
cmd5.c
cmd6.c
dungeon.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\dungeon.c(2550) : warning C4018: '>' : signed/unsigned mismatch
emulfunc.c
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(720) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(836) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(836) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(795) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(892) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(889) : warning C4101: 'fileHandle' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(887) : warning C4101: 'keyValue' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(888) : warning C4101: 'wcFileName' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(890) : warning C4101: 'bResult' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(990) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(990) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face\src\emulfunc.c(967) : warning C4101: 'checkStr' : unreferenced local variable
files.c
c:\documents and settings\jim\my documents\nick\face\src\files.c(3688) : warning C4018: '<' : signed/unsigned mismatch
c:\documents and settings\jim\my documents\nick\face\src\files.c(4522) : warning C4013: 'fake_time' undefined; assuming extern returning int
c:\documents and settings\jim\my documents\nick\face\src\files.c(5071) : warning C4098: 'display_scores_aux' : 'void' function returning a value
c:\documents and settings\jim\my documents\nick\face\src\files.c(5726) : warning C4142: benign redefinition of type
generate.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\generate.c(9542) : warning C4018: '<' : signed/unsigned mismatch
info.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\info.c(3642) : warning C4101: 'tile_hgt' : unreferenced local variable
init1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(4287) : warning C4101: 't' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(4285) : warning C4101: 'j' : unreferenced local variable
init2.c
load.c
main-wce.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1111) : warning C4013: 'WritePrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1277) : warning C4013: 'GetPrivateProfileInt_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1282) : warning C4013: 'GetPrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1311) : warning C4013: 'TestWinCEFakeFont' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(2454) : warning C4101: 'buf' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(2453) : warning C4101: 'i' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(3599) : warning C4013: 'DoSHFullScreen' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(3895) : warning C4101: 'p' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4070) : warning C4013: 'UpdateFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4096) : warning C4013: 'HandleFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4372) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5223) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5223) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5225) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5225) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5526) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5526) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5528) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5528) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5540) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5540) : warning C4024: 'FreeLibrary' : different types for formal and actual parameter 1
main.c
monattk.c
monmove.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monmove.c(4559) : warning C4018: '>=' : signed/unsigned mismatch
monster1.c
monster2.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster2.c(1840) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
object1.c
object2.c
randart.c
readdib.c
c:\documents and settings\jim\my documents\nick\face\src\readdib.c(193) : warning C4101: 'dwSize' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\readdib.c(234) : warning C4101: 'fh' : unreferenced local variable
save.c
spells1.c
spells2.c
squelch.c
store.c
tables.c
util.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(661) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(709) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(802) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(805) : warning C4022: 'SetFilePointer' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(811) : warning C4018: '!=' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(860) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(868) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(888) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(939) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(947) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(967) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(1017) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(1020) : warning C4022: 'CloseHandle' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3543) : warning C4018: '>' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3543) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3545) : warning C4018: '>' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3546) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3615) : warning C4018: '>' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3615) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3617) : warning C4018: '>' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3618) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3620) : warning C4018: '>' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3621) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(4101) : warning C4244: '=' : conversion from 'short ' to 'char ', possible loss of data
variable.c
wizard1.c
wizard2.c
xtra1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c(442) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c(444) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
xtra2.c
z-form.c
z-rand.c
z-term.c
z-util.c
z-virt.c
Linking...




<h3>Results</h3>
FA.exe - 0 error(s), 80 warning(s)
</pre>
</body>
</html>
