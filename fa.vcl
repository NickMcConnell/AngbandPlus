<html>
<body>
<pre>
<h1>Build Log</h1>
<h3>
--------------------Configuration: FA - Win32 (WCE ARMV4) Release--------------------
</h3>
<h3>Command Lines</h3>
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP14.tmp" with contents
[
/nologo /W3 /D _WIN32_WCE=420 /D "WIN32_PLATFORM_PSPC=400" /D "ARM" /D "_ARM_" /D "ARMV4" /D UNDER_CE=420 /D "UNICODE" /D "_UNICODE" /D "NDEBUG" /FR"ARMV4Rel/" /Fp"ARMV4Rel/FA.pch" /YX /Fo"ARMV4Rel/" /O2 /MC /c 
"\\VMWARE-HOST\SHARED FOLDERS\My Desktop\face11\src\score.c"
]
Creating command line "clarm.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP14.tmp" 
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP15.tmp" with contents
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
".\ARMV4Rel\identify.obj"
".\ARMV4Rel\jewel.obj"
".\ARMV4Rel\score.obj"
]
Creating command line "link.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP15.tmp"
<h3>Output Window</h3>
Compiling...
score.c
Linking...
Creating temporary file "C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP18.tmp" with contents
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
".\ARMV4Rel\z-virt.sbr"
".\ARMV4Rel\identify.sbr"
".\ARMV4Rel\jewel.sbr"
".\ARMV4Rel\score.sbr"]
Creating command line "bscmake.exe @C:\DOCUME~1\Owner\LOCALS~1\Temp\RSP18.tmp"
Creating browse info file...
<h3>Output Window</h3>




<h3>Results</h3>
FA.exe - 0 error(s), 0 warning(s)
</pre>
</body>
</html>
