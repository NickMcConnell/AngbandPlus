<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating command line "rc.exe /l 0x409 /fo"ARMV4Rel/angband.res" /i "src" /d UNDER_CE=420 /d _WIN32_WCE=420 /d "NDEBUG" /d "UNICODE" /d "_UNICODE" /d "WIN32_PLATFORM_PSPC=400" /d "ARM" /d "_ARM_" /d "ARMV4" /r "C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\angband.rc"" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1B.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "NDEBUG" /FR"ARMV4Rel/" /Fp"ARMV4Rel/FA.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\angbandcw.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\attack.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\birth.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cave.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd0.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd3.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd4.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd5.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd6.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\dungeon.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\emulfunc.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\files.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\generate.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\info.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\init1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\init2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\load.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monattk.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monster1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monster2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\object1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\object2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\randart.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\randname.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\readdib.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\save.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\spells1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\spells2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\squelch.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\store.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\tables.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\ui.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\variable.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\wizard1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\wizard2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\xtra1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\xtra2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-file.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-form.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-rand.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-term.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-type.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\z-virt.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1B.tmp" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1C.tmp" with contents
[
commctrl.lib coredll.lib aygshell.lib /nologo /base:"0x00010000" /stack:0x10000,0x1000 /entry:"WinMainCRTStartup" /incremental:no /pdb:"ARMV4Rel/FA.pdb" /nodefaultlib:"libc.lib /nodefaultlib:libcd.lib /nodefaultlib:libcmt.lib /nodefaultlib:libcmtd.lib /nodefaultlib:msvcrt.lib /nodefaultlib:msvcrtd.lib" /out:"ARMV4Rel/FA.exe" /subsystem:windowsce,4.20 /align:"4096" /MACHINE:ARM 
".\ARMV4Rel\angbandcw.obj"
".\ARMV4Rel\attack.obj"
".\ARMV4Rel\birth.obj"
".\ARMV4Rel\cave.obj"
".\ARMV4Rel\cmd0.obj"
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
".\ARMV4Rel\monattk.obj"
".\ARMV4Rel\monmove.obj"
".\ARMV4Rel\monster1.obj"
".\ARMV4Rel\monster2.obj"
".\ARMV4Rel\object1.obj"
".\ARMV4Rel\object2.obj"
".\ARMV4Rel\prefs.obj"
".\ARMV4Rel\randart.obj"
".\ARMV4Rel\randname.obj"
".\ARMV4Rel\readdib.obj"
".\ARMV4Rel\save.obj"
".\ARMV4Rel\spells1.obj"
".\ARMV4Rel\spells2.obj"
".\ARMV4Rel\squelch.obj"
".\ARMV4Rel\store.obj"
".\ARMV4Rel\tables.obj"
".\ARMV4Rel\ui.obj"
".\ARMV4Rel\util.obj"
".\ARMV4Rel\variable.obj"
".\ARMV4Rel\wizard1.obj"
".\ARMV4Rel\wizard2.obj"
".\ARMV4Rel\xtra1.obj"
".\ARMV4Rel\xtra2.obj"
".\ARMV4Rel\z-file.obj"
".\ARMV4Rel\z-form.obj"
".\ARMV4Rel\z-rand.obj"
".\ARMV4Rel\z-term.obj"
".\ARMV4Rel\z-type.obj"
".\ARMV4Rel\z-util.obj"
".\ARMV4Rel\z-virt.obj"
".\ARMV4Rel\angband.res"
]
Creating command line "link.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1C.tmp"
<h3>Output Window</h3>
Compiling resources...
Compiling...
angbandcw.c
attack.c
birth.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\birth.c(791) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cave.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cave.c(4481) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cave.c(4482) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cave.c(4611) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cave.c(4612) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd0.c
cmd1.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd1.c(384) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd2.c
cmd3.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd3.c(519) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd3.c(2445) : warning C4018: '<' : signed/unsigned mismatch
cmd4.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd4.c(758) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd4.c(758) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
cmd5.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd5.c(347) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\cmd5.c(955) : warning C4018: '<' : signed/unsigned mismatch
cmd6.c
dungeon.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\dungeon.c(2106) : warning C4018: '>' : signed/unsigned mismatch
emulfunc.c
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(720) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(836) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(836) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(795) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(892) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(889) : warning C4101: 'fileHandle' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(887) : warning C4101: 'keyValue' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(888) : warning C4101: 'wcFileName' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(890) : warning C4101: 'bResult' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(990) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(990) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face100beta\src\emulfunc.c(967) : warning C4101: 'checkStr' : unreferenced local variable
files.c
c:\documents and settings\jim\my documents\nick\face100beta\src\files.c(2535) : warning C4018: '<' : signed/unsigned mismatch
c:\documents and settings\jim\my documents\nick\face100beta\src\files.c(3329) : warning C4013: 'fake_time' undefined; assuming extern returning int
c:\documents and settings\jim\my documents\nick\face100beta\src\files.c(4531) : warning C4142: benign redefinition of type
generate.c
info.c
init1.c
init2.c
load.c
main-wce.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1124) : warning C4013: 'WritePrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1294) : warning C4013: 'GetPrivateProfileInt_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1299) : warning C4013: 'GetPrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1328) : warning C4013: 'TestWinCEFakeFont' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1532) : warning C4101: 'lppeSize' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1872) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(1872) : warning C4024: 'string_free' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(2481) : warning C4101: 'buf' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(2480) : warning C4101: 'i' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(3597) : warning C4013: 'DoSHFullScreen' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(3890) : warning C4101: 'p' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(4074) : warning C4013: 'UpdateFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(4100) : warning C4013: 'HandleFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5267) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5267) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5269) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5269) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5281) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5281) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5581) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5581) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5583) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5583) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5595) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(5595) : warning C4024: 'FreeLibrary' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(6237) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(6237) : warning C4024: 'string_free' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(6310) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(6310) : warning C4024: 'string_free' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\main-wce.c(6209) : warning C4101: 'j' : unreferenced local variable
monattk.c
monmove.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4344) : warning C4244: '=' : conversion from 'unsigned short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4345) : warning C4244: '=' : conversion from 'unsigned short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4734) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4827) : warning C4018: '>=' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4877) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monmove.c(4931) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
monster1.c
monster2.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\monster2.c(1950) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
object1.c
object2.c
prefs.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(654) : warning C4244: 'function' : conversion from 'long ' to 'short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(669) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(680) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(689) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(750) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\prefs.c(751) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
randart.c
randname.c
readdib.c
c:\documents and settings\jim\my documents\nick\face100beta\src\readdib.c(193) : warning C4101: 'dwSize' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face100beta\src\readdib.c(234) : warning C4101: 'fh' : unreferenced local variable
save.c
c:\documents and settings\jim\my documents\nick\face100beta\src\save.c(1048) : warning C4244: 'function' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
c:\documents and settings\jim\my documents\nick\face100beta\src\save.c(1049) : warning C4244: 'function' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
spells1.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\spells1.c(1351) : warning C4244: 'return' : conversion from 'long ' to 'unsigned char ', possible loss of data
spells2.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\spells2.c(916) : warning C4018: '<' : signed/unsigned mismatch
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\spells2.c(2501) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned char ', possible loss of data
squelch.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\squelch.c(891) : warning C4018: '<' : signed/unsigned mismatch
store.c
tables.c
ui.c
util.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\util.c(3536) : warning C4550: expression evaluates to a function which is missing an argument list
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\util.c(4477) : warning C4244: '=' : conversion from 'short ' to 'char ', possible loss of data
variable.c
wizard1.c
wizard2.c
xtra1.c
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\xtra1.c(441) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\face100beta\src\xtra1.c(443) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
xtra2.c
z-file.c
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(364) : warning C4133: 'function' : incompatible types - from 'char [260]' to 'const unsigned short *'
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(987) : warning C4133: 'function' : incompatible types - from 'char *' to 'const unsigned short *'
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(996) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(1026) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(1027) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
c:\documents and settings\jim\my documents\nick\face100beta\src\z-file.c(1035) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
z-form.c
z-rand.c
c:\documents and settings\jim\my documents\nick\face100beta\src\z-rand.c(332) : warning C4013: 'fake_time' undefined; assuming extern returning int
z-term.c
z-type.c
z-util.c
z-virt.c
Linking...
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1F.tmp" with contents
[
/nologo /o"ARMV4Rel/FA.bsc" 
".\ARMV4Rel\angbandcw.sbr"
".\ARMV4Rel\attack.sbr"
".\ARMV4Rel\birth.sbr"
".\ARMV4Rel\cave.sbr"
".\ARMV4Rel\cmd0.sbr"
".\ARMV4Rel\cmd1.sbr"
".\ARMV4Rel\cmd2.sbr"
".\ARMV4Rel\cmd3.sbr"
".\ARMV4Rel\cmd4.sbr"
".\ARMV4Rel\cmd5.sbr"
".\ARMV4Rel\cmd6.sbr"
".\ARMV4Rel\dungeon.sbr"
".\ARMV4Rel\emulfunc.sbr"
".\ARMV4Rel\files.sbr"
".\ARMV4Rel\generate.sbr"
".\ARMV4Rel\info.sbr"
".\ARMV4Rel\init1.sbr"
".\ARMV4Rel\init2.sbr"
".\ARMV4Rel\load.sbr"
".\ARMV4Rel\main-wce.sbr"
".\ARMV4Rel\monattk.sbr"
".\ARMV4Rel\monmove.sbr"
".\ARMV4Rel\monster1.sbr"
".\ARMV4Rel\monster2.sbr"
".\ARMV4Rel\object1.sbr"
".\ARMV4Rel\object2.sbr"
".\ARMV4Rel\prefs.sbr"
".\ARMV4Rel\randart.sbr"
".\ARMV4Rel\randname.sbr"
".\ARMV4Rel\readdib.sbr"
".\ARMV4Rel\save.sbr"
".\ARMV4Rel\spells1.sbr"
".\ARMV4Rel\spells2.sbr"
".\ARMV4Rel\squelch.sbr"
".\ARMV4Rel\store.sbr"
".\ARMV4Rel\tables.sbr"
".\ARMV4Rel\ui.sbr"
".\ARMV4Rel\util.sbr"
".\ARMV4Rel\variable.sbr"
".\ARMV4Rel\wizard1.sbr"
".\ARMV4Rel\wizard2.sbr"
".\ARMV4Rel\xtra1.sbr"
".\ARMV4Rel\xtra2.sbr"
".\ARMV4Rel\z-file.sbr"
".\ARMV4Rel\z-form.sbr"
".\ARMV4Rel\z-rand.sbr"
".\ARMV4Rel\z-term.sbr"
".\ARMV4Rel\z-type.sbr"
".\ARMV4Rel\z-util.sbr"
".\ARMV4Rel\z-virt.sbr"]
Creating command line "bscmake.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP1F.tmp"
Creating browse info file...
<h3>Output Window</h3>




<h3>Results</h3>
FA.exe - 0 error(s), 90 warning(s)
</pre>
</body>
</html>
