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
PROJECT = ..\freetype.dll
OBJFILES = ..\..\SDL\freetype-2.6.2\src\autofit\autofit.obj \
      ..\..\SDL\freetype-2.6.2\src\bdf\bdf.obj \
      ..\..\SDL\freetype-2.6.2\src\cff\cff.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftbase.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftbitmap.obj \
      ..\..\SDL\freetype-2.6.2\src\cache\ftcache.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftdebug.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftfstype.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftgasp.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftglyph.obj \
      ..\..\SDL\freetype-2.6.2\src\gzip\ftgzip.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftinit.obj \
      ..\..\SDL\freetype-2.6.2\src\lzw\ftlzw.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftstroke.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftsystem.obj \
      ..\..\SDL\freetype-2.6.2\src\smooth\smooth.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftbbox.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftfntfmt.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftmm.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftpfr.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftsynth.obj \
      ..\..\SDL\freetype-2.6.2\src\base\fttype1.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftwinfnt.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftlcdfil.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftgxval.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftotval.obj \
      ..\..\SDL\freetype-2.6.2\src\base\ftpatent.obj \
      ..\..\SDL\freetype-2.6.2\src\pcf\pcf.obj \
      ..\..\SDL\freetype-2.6.2\src\pfr\pfr.obj \
      ..\..\SDL\freetype-2.6.2\src\psaux\psaux.obj \
      ..\..\SDL\freetype-2.6.2\src\pshinter\pshinter.obj \
      ..\..\SDL\freetype-2.6.2\src\psnames\psmodule.obj \
      ..\..\SDL\freetype-2.6.2\src\raster\raster.obj \
      ..\..\SDL\freetype-2.6.2\src\sfnt\sfnt.obj \
      ..\..\SDL\freetype-2.6.2\src\truetype\truetype.obj \
      ..\..\SDL\freetype-2.6.2\src\type1\type1.obj \
      ..\..\SDL\freetype-2.6.2\src\cid\type1cid.obj \
      ..\..\SDL\freetype-2.6.2\src\type42\type42.obj \
      ..\..\SDL\freetype-2.6.2\src\winfonts\winfnt.obj

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
INCLUDEPATH = $(BCB)\include;..\..\SDL\freetype-2.6.2\include;..\..\SDL\freetype-2.6.2\src\autofit;..\..\SDL\freetype-2.6.2\src\bdf;..\..\SDL\freetype-2.6.2\src\cff;..\..\SDL\freetype-2.6.2\src\base;..\..\SDL\freetype-2.6.2\src\cache;..\..\SDL\freetype-2.6.2\src\gzip;..\..\SDL\freetype-2.6.2\src\lzw;..\..\SDL\freetype-2.6.2\src\pcf;..\..\SDL\freetype-2.6.2\src\pfr;..\..\SDL\freetype-2.6.2\src\psaux;..\..\SDL\freetype-2.6.2\src\pshinter;..\..\SDL\freetype-2.6.2\src\psnames;..\..\SDL\freetype-2.6.2\src\raster;..\..\SDL\freetype-2.6.2\src\sfnt;..\..\SDL\freetype-2.6.2\src\smooth;..\..\SDL\freetype-2.6.2\src\truetype;..\..\SDL\freetype-2.6.2\src\type1;..\..\SDL\freetype-2.6.2\src\cid;..\..\SDL\freetype-2.6.2\src\type42;..\..\SDL\freetype-2.6.2\src\winfonts
LIBPATH = $(BCB)\lib\obj;$(BCB)\lib;"..\..\SDL\freetype-2.6.2\src\autofit";"..\..\SDL\freetype-2.6.2\src\bdf";"..\..\SDL\freetype-2.6.2\src\cff";"..\..\SDL\freetype-2.6.2\src\base";"..\..\SDL\freetype-2.6.2\src\cache";"..\..\SDL\freetype-2.6.2\src\gzip";"..\..\SDL\freetype-2.6.2\src\lzw";"..\..\SDL\freetype-2.6.2\src\pcf";"..\..\SDL\freetype-2.6.2\src\pfr";"..\..\SDL\freetype-2.6.2\src\psaux";"..\..\SDL\freetype-2.6.2\src\pshinter";"..\..\SDL\freetype-2.6.2\src\psnames";"..\..\SDL\freetype-2.6.2\src\raster";"..\..\SDL\freetype-2.6.2\src\sfnt";"..\..\SDL\freetype-2.6.2\src\smooth";"..\..\SDL\freetype-2.6.2\src\truetype";"..\..\SDL\freetype-2.6.2\src\type1";"..\..\SDL\freetype-2.6.2\src\cid";"..\..\SDL\freetype-2.6.2\src\type42";"..\..\SDL\freetype-2.6.2\src\winfonts"
WARNINGS= -w-par
PATHCPP = .;..\..\SDL\freetype-2.6.2\src\autofit;..\..\SDL\freetype-2.6.2\src\bdf;..\..\SDL\freetype-2.6.2\src\cff;..\..\SDL\freetype-2.6.2\src\base;..\..\SDL\freetype-2.6.2\src\cache;..\..\SDL\freetype-2.6.2\src\gzip;..\..\SDL\freetype-2.6.2\src\lzw;..\..\SDL\freetype-2.6.2\src\pcf;..\..\SDL\freetype-2.6.2\src\pfr;..\..\SDL\freetype-2.6.2\src\psaux;..\..\SDL\freetype-2.6.2\src\pshinter;..\..\SDL\freetype-2.6.2\src\psnames;..\..\SDL\freetype-2.6.2\src\raster;..\..\SDL\freetype-2.6.2\src\sfnt;..\..\SDL\freetype-2.6.2\src\smooth;..\..\SDL\freetype-2.6.2\src\truetype;..\..\SDL\freetype-2.6.2\src\type1;..\..\SDL\freetype-2.6.2\src\cid;..\..\SDL\freetype-2.6.2\src\type42;..\..\SDL\freetype-2.6.2\src\winfonts
PATHASM = .;
PATHPAS = .;
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -WD -O2 -Hc -w- -Vx -Ve -X- \
    -a8 -b -k- -vi -tWD -tWM -c
IDLCFLAGS = -I$(BCB)\include \
      -I..\..\SDL\freetype-2.6.2\include \
      -I..\..\SDL\freetype-2.6.2\src\autofit -I..\..\SDL\freetype-2.6.2\src\bdf \
      -I..\..\SDL\freetype-2.6.2\src\cff -I..\..\SDL\freetype-2.6.2\src\base \
      -I..\..\SDL\freetype-2.6.2\src\cache -I..\..\SDL\freetype-2.6.2\src\gzip \
      -I..\..\SDL\freetype-2.6.2\src\lzw -I..\..\SDL\freetype-2.6.2\src\pcf \
      -I..\..\SDL\freetype-2.6.2\src\pfr -I..\..\SDL\freetype-2.6.2\src\psaux \
      -I..\..\SDL\freetype-2.6.2\src\pshinter -I..\..\SDL\freetype-2.6.2\src\psnames \
      -I..\..\SDL\freetype-2.6.2\src\raster -I..\..\SDL\freetype-2.6.2\src\sfnt \
      -I..\..\SDL\freetype-2.6.2\src\smooth -I..\..\SDL\freetype-2.6.2\src\truetype \
      -I..\..\SDL\freetype-2.6.2\src\type1 -I..\..\SDL\freetype-2.6.2\src\cid \
      -I..\..\SDL\freetype-2.6.2\src\type42 -I..\..\SDL\freetype-2.6.2\src\winfonts \
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




