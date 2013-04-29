<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Debug--------------------
</h3>
<h3>Command Lines</h3>
Creating command line "rc.exe /l 0x409 /fo"ARMV4Dbg/angband.res" /i "src" /d UNDER_CE=420 /d _WIN32_WCE=420 /d "DEBUG" /d "UNICODE" /d "_UNICODE" /d "WIN32_PLATFORM_PSPC=400" /d "ARM" /d "_ARM_" /d "ARMV4" /r "C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\angband.rc"" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP9FF9.tmp" with contents
[
/nologo /W3 /Zi /Od /D "DEBUG" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "UNICODE" /D "_UNICODE" /Fp"ARMV4Dbg/FA.pch" /YX /Fo"ARMV4Dbg/" /Fd"ARMV4Dbg/" /MC /c 
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\angbandcw.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\attack.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd0.c"
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
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\maid-x11.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monattk.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monmove.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\object1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\object2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\randart.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\randname.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\readdib.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\save.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\spells1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\spells2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\squelch.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\store.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\tables.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\ui.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\variable.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\wizard1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\wizard2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra2.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-form.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-rand.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-term.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-type.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-util.c"
"C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-virt.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP9FF9.tmp" 
Creating temporary file "C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP9FFA.tmp" with contents
[
commctrl.lib coredll.lib aygshell.lib /nologo /base:"0x00010000" /stack:0x10000,0x1000 /entry:"WinMainCRTStartup" /incremental:yes /pdb:"ARMV4Dbg/FA.pdb" /debug /nodefaultlib:"libc.lib /nodefaultlib:libcd.lib /nodefaultlib:libcmt.lib /nodefaultlib:libcmtd.lib /nodefaultlib:msvcrt.lib /nodefaultlib:msvcrtd.lib" /out:"ARMV4Dbg/FA.exe" /subsystem:windowsce,4.20 /align:"4096" /MACHINE:ARM 
".\ARMV4Dbg\angbandcw.obj"
".\ARMV4Dbg\attack.obj"
".\ARMV4Dbg\birth.obj"
".\ARMV4Dbg\cave.obj"
".\ARMV4Dbg\cmd0.obj"
".\ARMV4Dbg\cmd1.obj"
".\ARMV4Dbg\cmd2.obj"
".\ARMV4Dbg\cmd3.obj"
".\ARMV4Dbg\cmd4.obj"
".\ARMV4Dbg\cmd5.obj"
".\ARMV4Dbg\cmd6.obj"
".\ARMV4Dbg\dungeon.obj"
".\ARMV4Dbg\emulfunc.obj"
".\ARMV4Dbg\files.obj"
".\ARMV4Dbg\generate.obj"
".\ARMV4Dbg\info.obj"
".\ARMV4Dbg\init1.obj"
".\ARMV4Dbg\init2.obj"
".\ARMV4Dbg\load.obj"
".\ARMV4Dbg\maid-x11.obj"
".\ARMV4Dbg\main-wce.obj"
".\ARMV4Dbg\main.obj"
".\ARMV4Dbg\monattk.obj"
".\ARMV4Dbg\monmove.obj"
".\ARMV4Dbg\monster1.obj"
".\ARMV4Dbg\monster2.obj"
".\ARMV4Dbg\object1.obj"
".\ARMV4Dbg\object2.obj"
".\ARMV4Dbg\randart.obj"
".\ARMV4Dbg\randname.obj"
".\ARMV4Dbg\readdib.obj"
".\ARMV4Dbg\save.obj"
".\ARMV4Dbg\spells1.obj"
".\ARMV4Dbg\spells2.obj"
".\ARMV4Dbg\squelch.obj"
".\ARMV4Dbg\store.obj"
".\ARMV4Dbg\tables.obj"
".\ARMV4Dbg\ui.obj"
".\ARMV4Dbg\util.obj"
".\ARMV4Dbg\variable.obj"
".\ARMV4Dbg\wizard1.obj"
".\ARMV4Dbg\wizard2.obj"
".\ARMV4Dbg\xtra1.obj"
".\ARMV4Dbg\xtra2.obj"
".\ARMV4Dbg\z-file.obj"
".\ARMV4Dbg\z-form.obj"
".\ARMV4Dbg\z-rand.obj"
".\ARMV4Dbg\z-term.obj"
".\ARMV4Dbg\z-type.obj"
".\ARMV4Dbg\z-util.obj"
".\ARMV4Dbg\z-virt.obj"
".\ARMV4Dbg\angband.res"
]
Creating command line "link.exe @C:\DOCUME~1\Jim\LOCALS~1\Temp\RSP9FFA.tmp"
<h3>Output Window</h3>
Compiling resources...
Compiling...
angbandcw.c
attack.c
birth.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(792) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(797) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\birth.c(798) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cave.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4487) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4488) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4617) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cave.c(4618) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd0.c
cmd1.c
cmd2.c
cmd3.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd3.c(2319) : warning C4018: '<' : signed/unsigned mismatch
cmd4.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd4.c(740) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd4.c(740) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
cmd5.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\cmd5.c(342) : warning C4018: '<' : signed/unsigned mismatch
cmd6.c
dungeon.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\dungeon.c(1846) : warning C4018: '>' : signed/unsigned mismatch
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
c:\documents and settings\jim\my documents\nick\face\src\files.c(295) : warning C4244: 'function' : conversion from 'long ' to 'short ', possible loss of data
c:\documents and settings\jim\my documents\nick\face\src\files.c(3350) : warning C4018: '<' : signed/unsigned mismatch
c:\documents and settings\jim\my documents\nick\face\src\files.c(4136) : warning C4013: 'fake_time' undefined; assuming extern returning int
c:\documents and settings\jim\my documents\nick\face\src\files.c(4706) : warning C4098: 'display_scores_aux' : 'void' function returning a value
c:\documents and settings\jim\my documents\nick\face\src\files.c(5355) : warning C4142: benign redefinition of type
generate.c
info.c
init1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(710) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1056) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1102) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1178) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1283) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1472) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(1783) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(2034) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(2070) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(2260) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(2566) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\init1.c(3337) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
init2.c
load.c
maid-x11.c
main-wce.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1111) : warning C4013: 'WritePrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1277) : warning C4013: 'GetPrivateProfileInt_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1282) : warning C4013: 'GetPrivateProfileString_Reg' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(1311) : warning C4013: 'TestWinCEFakeFont' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(2457) : warning C4101: 'buf' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(2456) : warning C4101: 'i' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(3602) : warning C4013: 'DoSHFullScreen' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(3898) : warning C4101: 'p' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4073) : warning C4013: 'UpdateFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4099) : warning C4013: 'HandleFontMenuChoices' undefined; assuming extern returning int
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(4375) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5226) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5226) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5228) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5228) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5529) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5529) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5531) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5531) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5543) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main-wce.c(5543) : warning C4024: 'FreeLibrary' : different types for formal and actual parameter 1
main.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\main.c(333) : warning C4101: 'j' : unreferenced local variable
monattk.c
monmove.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monmove.c(4562) : warning C4018: '>=' : signed/unsigned mismatch
monster1.c
monster2.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\monster2.c(1840) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
object1.c
object2.c
randart.c
randname.c
readdib.c
c:\documents and settings\jim\my documents\nick\face\src\readdib.c(193) : warning C4101: 'dwSize' : unreferenced local variable
c:\documents and settings\jim\my documents\nick\face\src\readdib.c(234) : warning C4101: 'fh' : unreferenced local variable
save.c
spells1.c
spells2.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\spells2.c(704) : warning C4018: '<' : signed/unsigned mismatch
squelch.c
store.c
tables.c
ui.c
util.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(1686) : warning C4090: 'function' : different 'const' qualifiers
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(1686) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(3180) : warning C4550: expression evaluates to a function which is missing an argument list
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\util.c(4107) : warning C4244: '=' : conversion from 'short ' to 'char ', possible loss of data
variable.c
wizard1.c
wizard2.c
xtra1.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c(442) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\xtra1.c(444) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
xtra2.c
z-file.c
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(633) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(622) : warning C4101: 'fd' : unreferenced local variable
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(673) : warning C4047: 'return' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(736) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(739) : warning C4022: 'SetFilePointer' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(783) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(791) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(811) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(863) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(871) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(891) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(941) : warning C4047: '==' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(944) : warning C4022: 'CloseHandle' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1108) : warning C4133: 'function' : incompatible types - from 'char [260]' to 'const unsigned short *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1336) : warning C4047: 'initializing' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1345) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1367) : warning C4022: 'ReadFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1410) : warning C4047: 'initializing' : 'int ' differs in levels of indirection from 'void *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1418) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1441) : warning C4022: 'WriteFile' : pointer mismatch for actual parameter 1
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1628) : warning C4133: 'function' : incompatible types - from 'char *' to 'const unsigned short *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1640) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1672) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1673) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
C:\Documents and Settings\Jim\My Documents\Nick\FACE\src\z-file.c(1681) : warning C4133: 'function' : incompatible types - from 'unsigned short [260]' to 'const char *'
z-form.c
z-rand.c
c:\documents and settings\jim\my documents\nick\face\src\z-rand.c(332) : warning C4013: 'fake_time' undefined; assuming extern returning int
z-term.c
z-type.c
z-util.c
z-virt.c
Linking...
cmd4.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x0002FA7C
files.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x000571A0
generate.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x00064A64
info.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x00080C74
monmove.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x000AF948
object1.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x000CB368
spells1.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x000F3190
spells2.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x00105464
xtra1.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x00131BD0
xtra2.obj : warning LNK1166: cannot adjust code at offset=0x00001000, rva=0x001429A0




<h3>Results</h3>
FA.exe - 0 error(s), 110 warning(s)
</pre>
</body>
</html>
