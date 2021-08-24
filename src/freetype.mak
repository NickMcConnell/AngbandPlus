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
FREETYPE = freetype-2.10.2
# ---------------------------------------------------------------------------
PROJECT = ..\freetype.dll
OBJFILES = ..\..\SDL\$(FREETYPE)\src\autofit\autofit.obj \
      ..\..\SDL\$(FREETYPE)\src\bdf\bdf.obj \
      ..\..\SDL\$(FREETYPE)\src\cff\cff.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftbase.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftbitmap.obj \
      ..\..\SDL\$(FREETYPE)\src\cache\ftcache.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftdebug.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftfstype.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftgasp.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftglyph.obj \
      ..\..\SDL\$(FREETYPE)\src\gzip\ftgzip.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftinit.obj \
      ..\..\SDL\$(FREETYPE)\src\lzw\ftlzw.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftstroke.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftsystem.obj \
      ..\..\SDL\$(FREETYPE)\src\smooth\smooth.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftbbox.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftfntfmt.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftmm.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftpfr.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftsynth.obj \
      ..\..\SDL\$(FREETYPE)\src\base\fttype1.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftwinfnt.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftlcdfil.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftgxval.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftotval.obj \
      ..\..\SDL\$(FREETYPE)\src\base\ftpatent.obj \
      ..\..\SDL\$(FREETYPE)\src\pcf\pcf.obj \
      ..\..\SDL\$(FREETYPE)\src\pfr\pfr.obj \
      ..\..\SDL\$(FREETYPE)\src\psaux\psaux.obj \
      ..\..\SDL\$(FREETYPE)\src\pshinter\pshinter.obj \
      ..\..\SDL\$(FREETYPE)\src\psnames\psmodule.obj \
      ..\..\SDL\$(FREETYPE)\src\raster\raster.obj \
      ..\..\SDL\$(FREETYPE)\src\sfnt\sfnt.obj \
      ..\..\SDL\$(FREETYPE)\src\truetype\truetype.obj \
      ..\..\SDL\$(FREETYPE)\src\type1\type1.obj \
      ..\..\SDL\$(FREETYPE)\src\cid\type1cid.obj \
      ..\..\SDL\$(FREETYPE)\src\type42\type42.obj \
      ..\..\SDL\$(FREETYPE)\src\winfonts\winfnt.obj

RESFILES = 
MAINSOURCE = freetype.bpf
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
USERDEFINES = WIN32;_LIB;_CRT_SECURE_NO_WARNINGS;FT2_BUILD_LIBRARY;
SYSDEFINES = NO_STRICT;_NO_VCL
INCLUDEPATH = $(BCB)\include;..\..\SDL\$(FREETYPE)\include;..\..\SDL\$(FREETYPE)\src\autofit;..\..\SDL\$(FREETYPE)\src\bdf;..\..\SDL\$(FREETYPE)\src\cff;..\..\SDL\$(FREETYPE)\src\base;..\..\SDL\$(FREETYPE)\src\cache;..\..\SDL\$(FREETYPE)\src\gzip;..\..\SDL\$(FREETYPE)\src\lzw;..\..\SDL\$(FREETYPE)\src\pcf;..\..\SDL\$(FREETYPE)\src\pfr;..\..\SDL\$(FREETYPE)\src\psaux;..\..\SDL\$(FREETYPE)\src\pshinter;..\..\SDL\$(FREETYPE)\src\psnames;..\..\SDL\$(FREETYPE)\src\raster;..\..\SDL\$(FREETYPE)\src\sfnt;..\..\SDL\$(FREETYPE)\src\smooth;..\..\SDL\$(FREETYPE)\src\truetype;..\..\SDL\$(FREETYPE)\src\type1;..\..\SDL\$(FREETYPE)\src\cid;..\..\SDL\$(FREETYPE)\src\type42;..\..\SDL\$(FREETYPE)\src\winfonts
LIBPATH = $(BCB)\lib\obj;$(BCB)\lib;"..\..\SDL\$(FREETYPE)\src\autofit";"..\..\SDL\$(FREETYPE)\src\bdf";"..\..\SDL\$(FREETYPE)\src\cff";"..\..\SDL\$(FREETYPE)\src\base";"..\..\SDL\$(FREETYPE)\src\cache";"..\..\SDL\$(FREETYPE)\src\gzip";"..\..\SDL\$(FREETYPE)\src\lzw";"..\..\SDL\$(FREETYPE)\src\pcf";"..\..\SDL\$(FREETYPE)\src\pfr";"..\..\SDL\$(FREETYPE)\src\psaux";"..\..\SDL\$(FREETYPE)\src\pshinter";"..\..\SDL\$(FREETYPE)\src\psnames";"..\..\SDL\$(FREETYPE)\src\raster";"..\..\SDL\$(FREETYPE)\src\sfnt";"..\..\SDL\$(FREETYPE)\src\smooth";"..\..\SDL\$(FREETYPE)\src\truetype";"..\..\SDL\$(FREETYPE)\src\type1";"..\..\SDL\$(FREETYPE)\src\cid";"..\..\SDL\$(FREETYPE)\src\type42";"..\..\SDL\$(FREETYPE)\src\winfonts"
WARNINGS= -w-par
PATHCPP = .;..\..\SDL\$(FREETYPE)\src\autofit;..\..\SDL\$(FREETYPE)\src\bdf;..\..\SDL\$(FREETYPE)\src\cff;..\..\SDL\$(FREETYPE)\src\base;..\..\SDL\$(FREETYPE)\src\cache;..\..\SDL\$(FREETYPE)\src\gzip;..\..\SDL\$(FREETYPE)\src\lzw;..\..\SDL\$(FREETYPE)\src\pcf;..\..\SDL\$(FREETYPE)\src\pfr;..\..\SDL\$(FREETYPE)\src\psaux;..\..\SDL\$(FREETYPE)\src\pshinter;..\..\SDL\$(FREETYPE)\src\psnames;..\..\SDL\$(FREETYPE)\src\raster;..\..\SDL\$(FREETYPE)\src\sfnt;..\..\SDL\$(FREETYPE)\src\smooth;..\..\SDL\$(FREETYPE)\src\truetype;..\..\SDL\$(FREETYPE)\src\type1;..\..\SDL\$(FREETYPE)\src\cid;..\..\SDL\$(FREETYPE)\src\type42;..\..\SDL\$(FREETYPE)\src\winfonts
PATHASM = .;
PATHPAS = .;
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -WD -O2 -Hc -w- -Vx -Ve -X- \
    -a8 -b -k- -vi -tWD -tWM -c
IDLCFLAGS = -I$(BCB)\include \
      -I..\..\SDL\$(FREETYPE)\include \
      -I..\..\SDL\$(FREETYPE)\src\autofit -I..\..\SDL\$(FREETYPE)\src\bdf \
      -I..\..\SDL\$(FREETYPE)\src\cff -I..\..\SDL\$(FREETYPE)\src\base \
      -I..\..\SDL\$(FREETYPE)\src\cache -I..\..\SDL\$(FREETYPE)\src\gzip \
      -I..\..\SDL\$(FREETYPE)\src\lzw -I..\..\SDL\$(FREETYPE)\src\pcf \
      -I..\..\SDL\$(FREETYPE)\src\pfr -I..\..\SDL\$(FREETYPE)\src\psaux \
      -I..\..\SDL\$(FREETYPE)\src\pshinter -I..\..\SDL\$(FREETYPE)\src\psnames \
      -I..\..\SDL\$(FREETYPE)\src\raster -I..\..\SDL\$(FREETYPE)\src\sfnt \
      -I..\..\SDL\$(FREETYPE)\src\smooth -I..\..\SDL\$(FREETYPE)\src\truetype \
      -I..\..\SDL\$(FREETYPE)\src\type1 -I..\..\SDL\$(FREETYPE)\src\cid \
      -I..\..\SDL\$(FREETYPE)\src\type42 -I..\..\SDL\$(FREETYPE)\src\winfonts \
    -src_suffix cpp -DWIN32 -D_LIB -D_CRT_SECURE_NO_WARNINGS -DFT2_BUILD_LIBRARY -DNO_STRICT -D_NO_VCL -boa
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




