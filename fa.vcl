<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP4.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "NDEBUG" /FR"ARMV4Rel/" /Fp"ARMV4Rel/FA.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\angbandcw.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\attack.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\birth.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cave.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd0.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd3.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd4.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd5.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd6.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\dungeon.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\emulfunc.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\files.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\generate.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\identify.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\info.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\init1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\init2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\jewel.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\load.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monattk.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monster1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monster2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\object1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\object2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\randart.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\randname.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\save.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\score.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\spells1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\spells2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\squelch.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\store.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\tables.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\ui.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\util.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\variable.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\wizard1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\wizard2.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\xtra1.c"
"\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\xtra2.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP4.tmp" 
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP5.tmp" with contents
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
".\ARMV4Rel\identify.obj"
".\ARMV4Rel\info.obj"
".\ARMV4Rel\init1.obj"
".\ARMV4Rel\init2.obj"
".\ARMV4Rel\jewel.obj"
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
".\ARMV4Rel\score.obj"
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
Creating command line "link.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP5.tmp"
<h3>Output Window</h3>
Compiling...
angbandcw.c
attack.c
birth.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\birth.c(808) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cave.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cave.c(4490) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cave.c(4491) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cave.c(4620) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cave.c(4621) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd0.c
cmd1.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd1.c(407) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
cmd2.c
cmd3.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd3.c(531) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd3.c(2486) : warning C4018: '<' : signed/unsigned mismatch
cmd4.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd4.c(759) : warning C4090: 'function' : different 'const' qualifiers
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd4.c(759) : warning C4022: 'rnfree' : pointer mismatch for actual parameter 1
cmd5.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd5.c(357) : warning C4018: '<' : signed/unsigned mismatch
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\cmd5.c(968) : warning C4018: '<' : signed/unsigned mismatch
cmd6.c
dungeon.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\dungeon.c(1961) : warning C4018: '>' : signed/unsigned mismatch
emulfunc.c
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(720) : warning C4101: 'checkStr' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(836) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(836) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(795) : warning C4101: 'checkStr' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(892) : warning C4101: 'checkStr' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(889) : warning C4101: 'fileHandle' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(887) : warning C4101: 'keyValue' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(888) : warning C4101: 'wcFileName' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(890) : warning C4101: 'bResult' : unreferenced local variable
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(990) : warning C4047: 'function' : 'unsigned long ' differs in levels of indirection from 'void *'
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(990) : warning C4024: 'RegSetValueExW' : different types for formal and actual parameter 3
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\emulfunc.c(967) : warning C4101: 'checkStr' : unreferenced local variable
files.c
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\files.c(2560) : warning C4018: '<' : signed/unsigned mismatch
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\files.c(3343) : warning C4013: 'fake_time' undefined; assuming extern returning int
\\vmware-host\shared folders\my desktop\faangband112-src-wince\src\files.c(3882) : warning C4142: benign redefinition of type
generate.c
identify.c
info.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\info.c(884) : warning C4244: '=' : conversion from 'unsigned long ' to 'char ', possible loss of data
init1.c
init2.c
jewel.c
load.c
main-wce.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1124) : warning C4013: 'WritePrivateProfileString_Reg' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1294) : warning C4013: 'GetPrivateProfileInt_Reg' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1299) : warning C4013: 'GetPrivateProfileString_Reg' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1328) : warning C4013: 'TestWinCEFakeFont' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1532) : warning C4101: 'lppeSize' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1872) : warning C4090: 'function' : different 'const' qualifiers
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(1872) : warning C4024: 'string_free' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(2481) : warning C4101: 'buf' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(2480) : warning C4101: 'i' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(3597) : warning C4013: 'DoSHFullScreen' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(3890) : warning C4101: 'p' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(4074) : warning C4013: 'UpdateFontMenuChoices' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(4100) : warning C4013: 'HandleFontMenuChoices' undefined; assuming extern returning int
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5229) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5229) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5231) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5231) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5243) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5243) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5543) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5543) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5545) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5545) : warning C4024: 'GetProcAddressW' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5557) : warning C4047: 'function' : 'struct HINSTANCE__ *' differs in levels of indirection from 'unsigned long '
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(5557) : warning C4024: 'FreeLibrary' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(6199) : warning C4090: 'function' : different 'const' qualifiers
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(6199) : warning C4024: 'string_free' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(6272) : warning C4090: 'function' : different 'const' qualifiers
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(6272) : warning C4024: 'string_free' : different types for formal and actual parameter 1
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\main-wce.c(6171) : warning C4101: 'j' : unreferenced local variable
monattk.c
monmove.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4388) : warning C4244: '=' : conversion from 'unsigned short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4389) : warning C4244: '=' : conversion from 'unsigned short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4778) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4871) : warning C4018: '>=' : signed/unsigned mismatch
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4921) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monmove.c(4975) : warning C4244: '=' : conversion from 'short ' to 'unsigned char ', possible loss of data
monster1.c
monster2.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\monster2.c(2001) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
object1.c
object2.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\object2.c(5194) : warning C4101: 'tag_num' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\object2.c(5193) : warning C4101: 'num' : unreferenced local variable
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\object2.c(5721) : warning C4018: '<' : signed/unsigned mismatch
prefs.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(664) : warning C4244: 'function' : conversion from 'long ' to 'short ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(679) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(690) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(699) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(760) : warning C4244: '=' : conversion from 'long ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\prefs.c(761) : warning C4244: '=' : conversion from 'long ' to 'char ', possible loss of data
randart.c
randname.c
save.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\save.c(1058) : warning C4244: 'function' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\save.c(1059) : warning C4244: 'function' : conversion from 'unsigned long ' to 'unsigned short ', possible loss of data
score.c
spells1.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\spells1.c(1361) : warning C4244: 'return' : conversion from 'long ' to 'unsigned char ', possible loss of data
spells2.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\spells2.c(931) : warning C4018: '<' : signed/unsigned mismatch
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\spells2.c(2519) : warning C4244: '=' : conversion from 'unsigned long ' to 'unsigned char ', possible loss of data
squelch.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\squelch.c(909) : warning C4018: '<' : signed/unsigned mismatch
store.c
tables.c
ui.c
util.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\util.c(3546) : warning C4550: expression evaluates to a function which is missing an argument list
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\util.c(4487) : warning C4244: '=' : conversion from 'short ' to 'char ', possible loss of data
variable.c
wizard1.c
wizard2.c
xtra1.c
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\xtra1.c(450) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
\\vmware-host\Shared Folders\My Desktop\FAangband112-src-WinCE\src\xtra1.c(452) : warning C4244: 'function' : conversion from 'short ' to 'unsigned char ', possible loss of data
xtra2.c
Linking...
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP8.tmp" with contents
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
".\ARMV4Rel\identify.sbr"
".\ARMV4Rel\info.sbr"
".\ARMV4Rel\init1.sbr"
".\ARMV4Rel\init2.sbr"
".\ARMV4Rel\jewel.sbr"
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
".\ARMV4Rel\score.sbr"
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
Creating command line "bscmake.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP8.tmp"
Creating browse info file...
<h3>Output Window</h3>




<h3>Results</h3>
FA.exe - 0 error(s), 85 warning(s)
</pre>
</body>
</html>
