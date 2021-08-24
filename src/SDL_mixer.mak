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
PROJECT = ..\SDL_mixer.dll
OBJFILES = ..\..\SDL\SDL_mixer-1.2.12\dynamic_flac.obj \
  ..\..\SDL\SDL_mixer-1.2.12\dynamic_mod.obj ..\..\SDL\SDL_mixer-1.2.12\dynamic_mp3.obj \
  ..\..\SDL\SDL_mixer-1.2.12\dynamic_ogg.obj ..\..\SDL\SDL_mixer-1.2.12\effect_position.obj \
  ..\..\SDL\SDL_mixer-1.2.12\effect_stereoreverse.obj ..\..\SDL\SDL_mixer-1.2.12\effects_internal.obj \
  ..\..\SDL\SDL_mixer-1.2.12\load_aiff.obj \
  ..\..\SDL\SDL_mixer-1.2.12\load_flac.obj ..\..\SDL\SDL_mixer-1.2.12\load_ogg.obj \
  ..\..\SDL\SDL_mixer-1.2.12\load_voc.obj ..\..\SDL\SDL_mixer-1.2.12\mixer.obj \
  ..\..\SDL\SDL_mixer-1.2.12\music.obj ..\..\SDL\SDL_mixer-1.2.12\music_cmd.obj \
  ..\..\SDL\SDL_mixer-1.2.12\music_flac.obj ..\..\SDL\SDL_mixer-1.2.12\music_mad.obj \
  ..\..\SDL\SDL_mixer-1.2.12\music_mod.obj ..\..\SDL\SDL_mixer-1.2.12\music_ogg.obj \
  ..\..\SDL\SDL_mixer-1.2.12\wavestream.obj \
  ..\..\SDL\libmad-0.15.1b\bit.obj ..\..\SDL\libmad-0.15.1b\decoder.obj \
  ..\..\SDL\libmad-0.15.1b\fixed.obj ..\..\SDL\libmad-0.15.1b\frame.obj \
  ..\..\SDL\libmad-0.15.1b\huffman.obj ..\..\SDL\libmad-0.15.1b\layer3.obj \
  ..\..\SDL\libmad-0.15.1b\layer12.obj ..\..\SDL\libmad-0.15.1b\stream.obj \
  ..\..\SDL\libmad-0.15.1b\synth.obj ..\..\SDL\libmad-0.15.1b\timer.obj \
  ..\..\SDL\libmad-0.15.1b\version.obj
RESFILES = 
MAINSOURCE = SDL_mixer.bpf
RESDEPEN = $(RESFILES)
LIBFILES = _SDL\SDL.lib
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
USERDEFINES = WIN32;_WINDOWS;_CRT_SECURE_NO_WARNINGS;WAV_MUSIC;MP3_MAD_MUSIC;BUILD_SDL;FPM_DEFAULT
SYSDEFINES = NO_STRICT;_NO_VCL
INCLUDEPATH = $(BCB)\include;_SDL;..\..\SDL\libmad-0.15.1b
LIBPATH = $(BCB)\lib\obj;$(BCB)\lib;_SDL
WARNINGS= -w-par
PATHCPP= .;..\..\SDL\SDL_mixer-1.2.12;..\..\SDL\libmad-0.15.1b
PATHASM = .;
PATHPAS = .;
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -WD -O2 -Hc -w- -Vx -Ve -X- \
    -a8 -b -k- -vi -tWD -tWM -c
IDLCFLAGS = -I$(BCB)\include -I_SDL \
    -I..\..\SDL\libmad-0.15.1b \
    -src_suffix cpp -DWIN32 -D_WINDOWS -D_CRT_SECURE_NO_WARNINGS -DWAV_MUSIC \
    -DMP3_MAD_MUSIC -DBUILD_SDL -DFPM_DEFAULT -DNO_STRICT -D_NO_VCL -boa
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
AUSERDEFINES = -d$(USERDEFINES:;= -d)

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




