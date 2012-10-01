# Microsoft Developer Studio Project File - Name="Angband" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Angband - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Angband.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Angband.mak" CFG="Angband - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Angband - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Angband - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Angband - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib /nologo /subsystem:windows /machine:I386 /out:"Angband.exe"

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /ZI /GZ /c
# ADD CPP /nologo /W3 /GX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /FD /ZI /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /out:"Angband.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Angband - Win32 Release"
# Name "Angband - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\angband.rc
# End Source File
# Begin Source File

SOURCE=.\src\birth.c
# End Source File
# Begin Source File

SOURCE=.\src\cave.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd1.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd2.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd3.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd4.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd5.c
# End Source File
# Begin Source File

SOURCE=.\src\cmd6.c
# End Source File
# Begin Source File

SOURCE=.\src\dungeon.c
# End Source File
# Begin Source File

SOURCE=.\src\files.c
# End Source File
# Begin Source File

SOURCE=.\src\generate.c
# End Source File
# Begin Source File

SOURCE=.\src\init1.c
# End Source File
# Begin Source File

SOURCE=.\src\init2.c
# End Source File
# Begin Source File

SOURCE=".\src\l-misc.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-monst.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-object.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-player.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-random.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-spell.c"
# End Source File
# Begin Source File

SOURCE=".\src\l-ui.c"
# End Source File
# Begin Source File

SOURCE=.\src\lua\lapi.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lauxlib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lbaselib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lcode.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldblib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldebug.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldo.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldump.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lfunc.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lgc.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\liolib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\llex.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lmem.c
# End Source File
# Begin Source File

SOURCE=.\src\load.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lobject.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lopcodes.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lparser.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lstate.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lstring.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lstrlib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ltable.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ltablib.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\ltm.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lundump.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lvm.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\lzio.c
# End Source File
# Begin Source File

SOURCE=".\src\main-win.c"
# End Source File
# Begin Source File

SOURCE=.\src\melee1.c
# End Source File
# Begin Source File

SOURCE=.\src\melee2.c
# End Source File
# Begin Source File

SOURCE=.\src\monster1.c
# End Source File
# Begin Source File

SOURCE=.\src\monster2.c
# End Source File
# Begin Source File

SOURCE=".\src\obj-info.c"
# End Source File
# Begin Source File

SOURCE=.\src\object1.c
# End Source File
# Begin Source File

SOURCE=.\src\object2.c
# End Source File
# Begin Source File

SOURCE=.\src\randart.c
# End Source File
# Begin Source File

SOURCE=.\src\readdib.c
# End Source File
# Begin Source File

SOURCE=.\src\save.c
# End Source File
# Begin Source File

SOURCE=.\src\script.c
# End Source File
# Begin Source File

SOURCE=.\src\spells1.c
# End Source File
# Begin Source File

SOURCE=.\src\spells2.c
# End Source File
# Begin Source File

SOURCE=.\src\store.c
# End Source File
# Begin Source File

SOURCE=.\src\tables.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_event.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_event.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_is.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_map.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_push.c
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_to.c
# End Source File
# Begin Source File

SOURCE=".\src\use-obj.c"
# End Source File
# Begin Source File

SOURCE=.\src\util.c
# End Source File
# Begin Source File

SOURCE=.\src\variable.c
# End Source File
# Begin Source File

SOURCE=.\src\wizard1.c
# End Source File
# Begin Source File

SOURCE=.\src\wizard2.c
# End Source File
# Begin Source File

SOURCE=".\src\x-spell.c"
# End Source File
# Begin Source File

SOURCE=.\src\xtra1.c
# End Source File
# Begin Source File

SOURCE=.\src\xtra2.c
# End Source File
# Begin Source File

SOURCE=".\src\z-form.c"
# End Source File
# Begin Source File

SOURCE=".\src\z-rand.c"
# End Source File
# Begin Source File

SOURCE=".\src\z-term.c"
# End Source File
# Begin Source File

SOURCE=".\src\z-util.c"
# End Source File
# Begin Source File

SOURCE=".\src\z-virt.c"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\src\angband.h
# End Source File
# Begin Source File

SOURCE=.\src\config.h
# End Source File
# Begin Source File

SOURCE=.\src\defines.h
# End Source File
# Begin Source File

SOURCE=.\src\externs.h
# End Source File
# Begin Source File

SOURCE=".\src\h-basic.h"
# End Source File
# Begin Source File

SOURCE=".\src\h-config.h"
# End Source File
# Begin Source File

SOURCE=".\src\h-define.h"
# End Source File
# Begin Source File

SOURCE=".\src\h-system.h"
# End Source File
# Begin Source File

SOURCE=".\src\h-type.h"
# End Source File
# Begin Source File

SOURCE=.\src\init.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lapi.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lauxlib.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lcode.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldebug.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\ldo.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lfunc.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lgc.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\llex.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\llimits.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lmem.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lobject.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lopcodes.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lparser.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lstate.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lstring.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\ltable.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\ltm.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lua.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\luadebug.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lualib.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lundump.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lvm.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\lzio.h
# End Source File
# Begin Source File

SOURCE=.\src\main.h
# End Source File
# Begin Source File

SOURCE=.\src\readdib.h
# End Source File
# Begin Source File

SOURCE=.\src\script.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_eh.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_rg.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_tm.h
# End Source File
# Begin Source File

SOURCE=.\src\lua\tolua_tt.h
# End Source File
# Begin Source File

SOURCE=.\src\types.h
# End Source File
# Begin Source File

SOURCE=".\src\z-form.h"
# End Source File
# Begin Source File

SOURCE=".\src\z-rand.h"
# End Source File
# Begin Source File

SOURCE=".\src\z-term.h"
# End Source File
# Begin Source File

SOURCE=".\src\z-util.h"
# End Source File
# Begin Source File

SOURCE=".\src\z-virt.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "Lua wrappers"

# PROP Default_Filter "pkg"
# Begin Source File

SOURCE=".\src\l-misc.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=".\src\l-misc.pkg"
InputName=l-misc

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n misc -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-misc.pkg
InputName=l-misc

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n misc -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-monst.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-monst.pkg
InputName=l-monst

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n monster -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-monst.pkg
InputName=l-monst

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n monster -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-object.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-object.pkg
InputName=l-object

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n object -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-object.pkg
InputName=l-object

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n object -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-player.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-player.pkg
InputName=l-player

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n player -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-player.pkg
InputName=l-player

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n player -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-random.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-random.pkg
InputName=l-random

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n random -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-random.pkg
InputName=l-random

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n random -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-spell.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-spell.pkg
InputName=l-spell

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n spell -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-spell.pkg
InputName=l-spell

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n spell -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\src\l-ui.pkg"

!IF  "$(CFG)" == "Angband - Win32 Release"

# Begin Custom Build
InputPath=.\src\l-ui.pkg
InputName=l-ui

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n ui -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ELSEIF  "$(CFG)" == "Angband - Win32 Debug"

# Begin Custom Build
InputPath=.\src\l-ui.pkg
InputName=l-ui

".\src\$(InputName).c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	.\src\tolua.exe -n ui -o .\src\$(InputName).c .\src\$(InputName).pkg

# End Custom Build

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
