# Microsoft Developer Studio Project File - Name="Angband" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Angband - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit\
 NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den\
 Befehl
!MESSAGE 
!MESSAGE NMAKE /f "Angband.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "Angband.mak" CFG="Angband - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "Angband - Win32 Release" (basierend auf  "Win32 (x86) Application")
!MESSAGE "Angband - Win32 Debug" (basierend auf  "Win32 (x86) Application")
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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "USE_TRANSPARENCY" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /machine:I386 /out:"../Angband.exe"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "USE_TRANSPARENCY" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o NUL /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /out:"../Angband.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Angband - Win32 Release"
# Name "Angband - Win32 Debug"
# Begin Source File

SOURCE=.\angband.h
# End Source File
# Begin Source File

SOURCE=.\angband.rc
# End Source File
# Begin Source File

SOURCE=.\birth.c
# End Source File
# Begin Source File

SOURCE=.\cave.c
# End Source File
# Begin Source File

SOURCE=.\cmd1.c
# End Source File
# Begin Source File

SOURCE=.\cmd2.c
# End Source File
# Begin Source File

SOURCE=.\cmd3.c
# End Source File
# Begin Source File

SOURCE=.\cmd4.c
# End Source File
# Begin Source File

SOURCE=.\cmd5.c
# End Source File
# Begin Source File

SOURCE=.\cmd6.c
# End Source File
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\defines.h
# End Source File
# Begin Source File

SOURCE=.\dungeon.c
# End Source File
# Begin Source File

SOURCE=.\externs.h
# End Source File
# Begin Source File

SOURCE=.\files.c
# End Source File
# Begin Source File

SOURCE=.\generate.c
# End Source File
# Begin Source File

SOURCE=".\h-basic.h"
# End Source File
# Begin Source File

SOURCE=".\h-config.h"
# End Source File
# Begin Source File

SOURCE=".\h-define.h"
# End Source File
# Begin Source File

SOURCE=".\h-system.h"
# End Source File
# Begin Source File

SOURCE=".\h-type.h"
# End Source File
# Begin Source File

SOURCE=.\init1.c
# End Source File
# Begin Source File

SOURCE=.\init2.c
# End Source File
# Begin Source File

SOURCE=.\load1.c
# End Source File
# Begin Source File

SOURCE=.\load2.c
# End Source File
# Begin Source File

SOURCE=".\main-win.c"
# End Source File
# Begin Source File

SOURCE=.\melee1.c
# End Source File
# Begin Source File

SOURCE=.\melee2.c
# End Source File
# Begin Source File

SOURCE=.\monster1.c
# End Source File
# Begin Source File

SOURCE=.\monster2.c
# End Source File
# Begin Source File

SOURCE=.\object1.c
# End Source File
# Begin Source File

SOURCE=.\object2.c
# End Source File
# Begin Source File

SOURCE=.\randart.c
# End Source File
# Begin Source File

SOURCE=.\readdib.c
# End Source File
# Begin Source File

SOURCE=.\readdib.h
# End Source File
# Begin Source File

SOURCE=.\save.c
# End Source File
# Begin Source File

SOURCE=.\spells1.c
# End Source File
# Begin Source File

SOURCE=.\spells2.c
# End Source File
# Begin Source File

SOURCE=.\store.c
# End Source File
# Begin Source File

SOURCE=.\tables.c
# End Source File
# Begin Source File

SOURCE=.\types.h
# End Source File
# Begin Source File

SOURCE=.\util.c
# End Source File
# Begin Source File

SOURCE=.\variable.c
# End Source File
# Begin Source File

SOURCE=.\wizard1.c
# End Source File
# Begin Source File

SOURCE=.\wizard2.c
# End Source File
# Begin Source File

SOURCE=.\xtra1.c
# End Source File
# Begin Source File

SOURCE=.\xtra2.c
# End Source File
# Begin Source File

SOURCE=".\z-form.c"
# End Source File
# Begin Source File

SOURCE=".\z-form.h"
# End Source File
# Begin Source File

SOURCE=".\z-rand.c"
# End Source File
# Begin Source File

SOURCE=".\z-rand.h"
# End Source File
# Begin Source File

SOURCE=".\z-term.c"
# End Source File
# Begin Source File

SOURCE=".\z-term.h"
# End Source File
# Begin Source File

SOURCE=".\z-util.c"
# End Source File
# Begin Source File

SOURCE=".\z-util.h"
# End Source File
# Begin Source File

SOURCE=".\z-virt.c"
# End Source File
# Begin Source File

SOURCE=".\z-virt.h"
# End Source File
# End Target
# End Project
