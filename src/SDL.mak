# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.06.00
# ---------------------------------------------------------------------------
PROJECT = ..\SDL.dll
OBJFILES = ..\..\SDL\SDL-1.2.15\src\stdlib\SDL_getenv.obj ..\..\SDL\SDL-1.2.15\src\SDL_error.obj ..\..\SDL\SDL-1.2.15\src\SDL_fatal.obj \
    ..\..\SDL\SDL-1.2.15\src\SDL.obj ..\..\SDL\SDL-1.2.15\src\audio\SDL_wave.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\SDL_audiocvt.obj ..\..\SDL\SDL-1.2.15\src\audio\SDL_audiodev.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\SDL_mixer.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\SDL_audio.obj ..\..\SDL\SDL-1.2.15\src\audio\windib\SDL_dibaudio.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\windx5\SDL_dx5audio.obj \
    ..\..\SDL\SDL-1.2.15\src\cdrom\win32\SDL_syscdrom.obj ..\..\SDL\SDL-1.2.15\src\cdrom\SDL_cdrom.obj \
    ..\..\SDL\SDL-1.2.15\src\events\SDL_resize.obj \
    ..\..\SDL\SDL-1.2.15\src\events\SDL_events.obj ..\..\SDL\SDL-1.2.15\src\events\SDL_expose.obj \
    ..\..\SDL\SDL-1.2.15\src\events\SDL_keyboard.obj ..\..\SDL\SDL-1.2.15\src\events\SDL_mouse.obj \
    ..\..\SDL\SDL-1.2.15\src\events\SDL_quit.obj ..\..\SDL\SDL-1.2.15\src\events\SDL_active.obj \
    ..\..\SDL\SDL-1.2.15\src\file\SDL_rwops.obj ..\..\SDL\SDL-1.2.15\src\joystick\SDL_joystick.obj \
    ..\..\SDL\SDL-1.2.15\src\joystick\win32\SDL_mmjoystick.obj \
    ..\..\SDL\SDL-1.2.15\src\thread\SDL_thread.obj ..\..\SDL\SDL-1.2.15\src\thread\win32\SDL_sysmutex.obj \
    ..\..\SDL\SDL-1.2.15\src\thread\win32\SDL_syssem.obj \
    ..\..\SDL\SDL-1.2.15\src\thread\win32\SDL_systhread.obj ..\..\SDL\SDL-1.2.15\src\timer\SDL_timer.obj \
    ..\..\SDL\SDL-1.2.15\src\timer\win32\SDL_systimer.obj \
    ..\..\SDL\SDL-1.2.15\src\video\wincommon\SDL_wingl.obj \
    ..\..\SDL\SDL-1.2.15\src\video\wincommon\SDL_sysmouse.obj \
    ..\..\SDL\SDL-1.2.15\src\video\wincommon\SDL_syswm.obj \
    ..\..\SDL\SDL-1.2.15\src\video\wincommon\SDL_sysevents.obj \
    ..\..\SDL\SDL-1.2.15\src\video\windib\SDL_dibvideo.obj \
    ..\..\SDL\SDL-1.2.15\src\video\windib\SDL_dibevents.obj \
    ..\..\SDL\SDL-1.2.15\src\video\windx5\SDL_dx5yuv.obj \
    ..\..\SDL\SDL-1.2.15\src\video\windx5\SDL_dx5video.obj \
    ..\..\SDL\SDL-1.2.15\src\video\windx5\SDL_dx5events.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_yuv_sw.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_blit_0.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_blit_1.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_blit_A.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_blit_N.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_bmp.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_cursor.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_gamma.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_pixels.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_RLEaccel.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_stretch.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_surface.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_video.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_yuv.obj ..\..\SDL\SDL-1.2.15\src\video\SDL_yuv_mmx.obj \
    ..\..\SDL\SDL-1.2.15\src\video\SDL_blit.obj \
    ..\..\SDL\SDL-1.2.15\src\stdlib\SDL_string.obj \
    ..\..\SDL\SDL-1.2.15\src\cpuinfo\SDL_cpuinfo.obj \
    ..\..\SDL\SDL-1.2.15\src\stdlib\SDL_iconv.obj \
    ..\..\SDL\SDL-1.2.15\src\video\dummy\SDL_nullvideo.obj \
    ..\..\SDL\SDL-1.2.15\src\video\dummy\SDL_nullevents.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\disk\SDL_diskaudio.obj \
    ..\..\SDL\SDL-1.2.15\src\audio\dummy\SDL_dummyaudio.obj \
    ..\..\SDL\SDL-1.2.15\src\loadso\win32\SDL_sysloadso.obj
RESFILES = 
MAINSOURCE = SDL.bpf
RESDEPEN = $(RESFILES)
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = 
SPARELIBS = 
DEFFILE =
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = WIN32;BUILD_SDL;__FREEBCC__;ENABLE_WINDIB;ENABLE_DIRECTX;HAVE_ALLOCA=1;HAVE_OPENGL
SYSDEFINES = NO_STRICT;_NO_VCL
INCLUDEPATH = ..\..\SDL\SDL-1.2.15\include;..\..\SDL\SDL-1.2.15\src\video;..\..\SDL\SDL-1.2.15\src\video\windx5;..\..\SDL\SDL-1.2.15\src\video\windib;..\..\SDL\SDL-1.2.15\src\video\wincommon;..\..\SDL\SDL-1.2.15\src\timer\win32;..\..\SDL\SDL-1.2.15\src\timer;..\..\SDL\SDL-1.2.15\src\thread\win32;..\..\SDL\SDL-1.2.15\src\thread;..\..\SDL\SDL-1.2.15\src\joystick\win32;..\..\SDL\SDL-1.2.15\src\joystick;..\..\SDL\SDL-1.2.15\src\file;..\..\SDL\SDL-1.2.15\src\events;..\..\SDL\SDL-1.2.15\src\endian;..\..\SDL\SDL-1.2.15\src\cdrom;..\..\SDL\SDL-1.2.15\src\cdrom\win32;..\..\SDL\SDL-1.2.15\src\audio\windx5;..\..\SDL\SDL-1.2.15\src\audio\windib;..\..\SDL\SDL-1.2.15\src\audio;..\..\SDL\SDL-1.2.15\src;$(BCB)\include
LIBPATH = "..\..\SDL\SDL-1.2.15\src\stdlib";"..\..\SDL\SDL-1.2.15\src\cpuinfo";"..\..\SDL\SDL-1.2.15\src\video\dummy";"..\..\SDL\SDL-1.2.15\src\audio\disk";"..\..\SDL\SDL-1.2.15\src\audio\dummy";"..\..\SDL\SDL-1.2.15\src\video";"..\..\SDL\SDL-1.2.15\src\video\windx5";"..\..\SDL\SDL-1.2.15\src\video\windib";"..\..\SDL\SDL-1.2.15\src\video\wincommon";"..\..\SDL\SDL-1.2.15\src\timer\win32";"..\..\SDL\SDL-1.2.15\src\timer";"..\..\SDL\SDL-1.2.15\src\thread\win32";"..\..\SDL\SDL-1.2.15\src\thread";"..\..\SDL\SDL-1.2.15\src\joystick\win32";"..\..\SDL\SDL-1.2.15\src\joystick";"..\..\SDL\SDL-1.2.15\src\file";"..\..\SDL\SDL-1.2.15\src\events";"..\..\SDL\SDL-1.2.15\src\endian";"..\..\SDL\SDL-1.2.15\src\cdrom";"..\..\SDL\SDL-1.2.15\src\cdrom\win32";"..\..\SDL\SDL-1.2.15\src\audio\windx5";"..\..\SDL\SDL-1.2.15\src\audio\windib";"..\..\SDL\SDL-1.2.15\src\audio";"..\..\SDL\SDL-1.2.15\src";$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-par
PATHCPP = .;..\..\SDL\SDL-1.2.15\src;..\..\SDL\SDL-1.2.15\src\stdlib;..\..\SDL\SDL-1.2.15\src\cpuinfo;..\..\SDL\SDL-1.2.15\src\video\dummy;..\..\SDL\SDL-1.2.15\src\audio\disk;..\..\SDL\SDL-1.2.15\src\audio\dummy;..\..\SDL\SDL-1.2.15\src\audio;..\..\SDL\SDL-1.2.15\src\audio\windib;..\..\SDL\SDL-1.2.15\src\audio\windx5;..\..\SDL\SDL-1.2.15\src\cdrom\win32;..\..\SDL\SDL-1.2.15\src\cdrom;..\..\SDL\SDL-1.2.15\src\endian;..\..\SDL\SDL-1.2.15\src\events;..\..\SDL\SDL-1.2.15\src\file;..\..\SDL\SDL-1.2.15\src\joystick;..\..\SDL\SDL-1.2.15\src\joystick\win32;..\..\SDL\SDL-1.2.15\src\thread;..\..\SDL\SDL-1.2.15\src\thread\win32;..\..\SDL\SDL-1.2.15\src\timer;..\..\SDL\SDL-1.2.15\src\timer\win32;..\..\SDL\SDL-1.2.15\src\video\wincommon;..\..\SDL\SDL-1.2.15\src\video\windib;..\..\SDL\SDL-1.2.15\src\video\windx5;..\..\SDL\SDL-1.2.15\src\video;..\..\SDL\SDL-1.2.15\src\loadso\win32
PATHASM = .;
PATHPAS = .;
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -WD -O2 -Hc -w- -Vx -Ve -X- \
    -a8 -b -k- -vi -tWD -tWM -c -DENABLE_WINDIB -DHAVE_ALLOCA=1
IDLCFLAGS = -I..\..\SDL\SDL-1.2.15\include -I..\..\SDL\SDL-1.2.15\src\video -I..\..\SDL\SDL-1.2.15\src\video\windx5 \
    -I..\..\SDL\SDL-1.2.15\src\video\windib -I..\..\SDL\SDL-1.2.15\src\video\wincommon \
    -I..\..\SDL\SDL-1.2.15\src\timer\win32 -I..\..\SDL\SDL-1.2.15\src\timer -I..\..\SDL\SDL-1.2.15\src\thread\win32 \
    -I..\..\SDL\SDL-1.2.15\src\thread -I..\..\SDL\SDL-1.2.15\src\joystick\win32 -I..\..\SDL\SDL-1.2.15\src\joystick \
    -I..\..\SDL\SDL-1.2.15\src\file -I..\..\SDL\SDL-1.2.15\src\events -I..\..\SDL\SDL-1.2.15\src\endian -I..\..\SDL\SDL-1.2.15\src\cdrom \
    -I..\..\SDL\SDL-1.2.15\src\cdrom\win32 -I..\..\SDL\SDL-1.2.15\src\audio\windx5 -I..\..\SDL\SDL-1.2.15\src\audio\windib \
    -I..\..\SDL\SDL-1.2.15\src\audio -I..\..\SDL\SDL-1.2.15\src -I$(BCB)\include \
    -src_suffix cpp -DWIN32 -DBUILD_SDL -D__FREEBCC__ -DENABLE_WINDIB \
    -DENABLE_DIRECTX -DHAVE_ALLOCA=1 -DHAVE_OPENGL -DNO_STRICT -D_NO_VCL -boa
PFLAGS = -$Y- -$L- -$D- -$A8 -v -JPHNE -M
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"" -aa -Tpd -x -Gn -Gi
# ---------------------------------------------------------------------------
ALLOBJ = c0d32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cw32mt.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif

!if $d(PATHOBJ)
.PATH.OBJ  = $(PATHOBJ)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<



# ---------------------------------------------------------------------------




