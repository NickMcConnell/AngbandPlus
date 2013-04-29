<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating command line "rc.exe /l 0x409 /fo"ARMV4Rel/angband.res" /i "src" /d UNDER_CE=420 /d _WIN32_WCE=420 /d "NDEBUG" /d "UNICODE" /d "_UNICODE" /d "WIN32_PLATFORM_PSPC=400" /d "ARM" /d "_ARM_" /d "ARMV4" /r "C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\angband.rc"" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSPF6B3.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "NDEBUG" /Fp"ARMV4Rel/FA.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\angbandcw.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\attack.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\birth.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cave.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd0.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd3.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd4.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd5.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd6.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\dungeon.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\emulfunc.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\files.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\generate.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\info.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\load.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monattk.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monmove.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monster1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monster2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\object1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\object2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\randart.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\randname.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\readdib.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\save.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\spells1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\spells2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\squelch.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\store.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\tables.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\ui.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\variable.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\wizard1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\wizard2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\xtra1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\xtra2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-form.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-rand.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-term.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-type.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-virt.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSPF6B3.tmp" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSPF6B4.tmp" with contents
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
Creating command line "link.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSPF6B4.tmp"
<h3>Output Window</h3>
Compiling resources...
Compiling...
angbandcw.c
attack.c
birth.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\birth.c(792) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\birth.c(797) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\birth.c(798) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cave.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cave.c(4507) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cave.c(4508) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cave.c(4637) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cave.c(4638) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd0.c
cmd1.c
cmd2.c
cmd3.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd3.c(23) : warning C4101: 'j' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd3.c(2382) : warning C4018: '<' : signed/unsigned mismatch
cmd4.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd4.c(740) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd4.c(740) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
cmd5.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\cmd5.c(342) : warning C4018: '<' : signed/unsigned mismatch
cmd6.c
dungeon.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\dungeon.c(1847) : warning C4018: '>' : signed/unsigned mismatch
emulfunc.c
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(720) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(836) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(836) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(795) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(892) : warning C4101: 'checkStr' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(889) : warning C4101: 'fileHandle' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(887) : warning C4101: 'keyValue' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(888) : warning C4101: 'wcFileName' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(890) : warning C4101: 'bResult' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(990) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(990) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
c:\documents and settings\jim\my documents\nick\face031\src\emulfunc.c(967) : warning C4101: 'checkStr' : unreferenced local variable
files.c
c:\documents and settings\jim\my documents\nick\face031\src\files.c(295) : warning C4244: 'function' : conversion from 'long ' to 'short ', possible loss of data
c:\documents and settings\jim\my documents\nick\face031\src\files.c(3362) : warning C4018: '<' : signed/unsigned mismatch
c:\documents and settings\jim\my documents\nick\face031\src\files.c(4148) : warning C4013: 'fake_time' undefined; assuming extern returning int
c:\documents and settings\jim\my documents\nick\face031\src\files.c(4718) : warning C4098: 'display_scores_aux' : 'void' function returning a value
c:\documents and settings\jim\my documents\nick\face031\src\files.c(5370) : warning C4142: benign redefinition of type
generate.c
info.c
init1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(710) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1056) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1102) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1178) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1283) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1472) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(1790) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(2041) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(2077) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(2267) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(2573) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\init1.c(3327) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
init2.c
load.c
main-wce.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(1111) : warning C4013: 'WritePrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(1277) : warning C4013: 'GetPrivateProfileInt_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(1282) : warning C4013: 'GetPrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(1311) : warning C4013: 'TestWinCEFakeFont' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(2457) : warning C4101: 'buf' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(2456) : warning C4101: 'i' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(3602) : warning C4013: 'DoSHFullScreen' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(3898) : warning C4101: 'p' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(4073) : warning C4013: 'UpdateFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(4099) : warning C4013: 'HandleFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(4375) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5226) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5226) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5228) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5228) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5529) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5529) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5531) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5531) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5543) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\main-wce.c(5543) : warning C4024: 'FreeLibrary' : different types for formal and actual parameter 1
monattk.c
monmove.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monmove.c(4563) : warning C4018: '>=' : signed/unsigned mismatch
monster1.c
monster2.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\monster2.c(1840) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
object1.c
object2.c
randart.c
randname.c
readdib.c
c:\documents and settings\jim\my documents\nick\face031\src\readdib.c(193) : warning C4101: 'dwSize' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face031\src\readdib.c(234) : warning C4101: 'fh' : unreferenced local variable
save.c
spells1.c
spells2.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\spells2.c(704) : warning C4018: '<' : signed/unsigned mismatch
squelch.c
store.c
tables.c
ui.c
util.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\util.c(1692) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\util.c(1692) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\util.c(3492) : warning C4550: expression evaluates to a function which is missing an argument list
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\util.c(4427) : warning C4244: '=' : conversion from 'short ' to 'char ', possible loss of data
variable.c
wizard1.c
wizard2.c
xtra1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\xtra1.c(442) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\xtra1.c(444) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
xtra2.c
z-file.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(633) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(622) : warning C4101: 'fd' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(673) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(736) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(739) : warning C4022: 'SetFilePointer' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(783) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(791) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(811) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(863) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(871) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(891) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(941) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(944) : warning C4022: 'CloseHandle' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1108) : warning C4133: 'function' : incompatible types - from 'char [260]' to 'const unsigned short *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1336) : warning C4047: 'initializing' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1345) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1366) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1410) : warning C4047: 'initializing' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1418) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1441) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1629) : warning C4133: 'function' : incompatible types - from 'char *' to 'const unsigned short *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1641) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1673) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1674) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE031\src\z-file.c(1682) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
z-form.c
z-rand.c
c:\documents and settings\jim\my documents\nick\face031\src\z-rand.c(332) : warning C4013: 'fake_time' undefined; assuming extern returning int
z-term.c
z-type.c
z-util.c
z-virt.c
Linking...




<h3>Results</h3>
FA.exe - 0 error(s), 100 warning(s)
</pre>
</body>
</html>
