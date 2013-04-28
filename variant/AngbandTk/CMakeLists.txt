SET (d ${OmnibandTk_SOURCE_DIR}/src/common/)
SET (common_SRCS
    ${d}bind.c ${d}birth-tnb.c ${d}canv-widget.c ${d}const.c ${d}debug.c 
    ${d}describe.c ${d}file_character.c ${d}icon1.c ${d}icon2.c ${d}interp1.c 
    ${d}interp2.c ${d}map.c ${d}mon_memory.c ${d}plat.c ${d}r_info.c ${d}savefile.c ${d}setting.c ${d}sound.c ${d}struct.c
    ${d}town.c ${d}util-tnb.c ${d}widget.c ${d}widget-iso.c ${d}wor.c ${d}treectrl.c
)
#SET_SOURCE_FILES_PROPERTIES (${libangtk_SRCS} PROPERTIES LOCATION ${COMMONTK}/src/common)

SET (d src/)
SET (variant_SRCS
    ${d}birth.c ${d}cave.c ${d}cmd1.c ${d}cmd2.c ${d}cmd3.c ${d}cmd4.c ${d}cmd5.c 
    ${d}cmd6.c ${d}dungeon.c ${d}files.c ${d}generate.c ${d}init1.c ${d}init2.c ${d}load1.c ${d}load2.c
    ${d}melee1.c ${d}melee2.c ${d}monster1.c ${d}monster2.c ${d}object1.c ${d}object2.c ${d}randart.c
    ${d}save.c ${d}spells1.c ${d}spells2.c ${d}store.c ${d}tables.c ${d}util.c ${d}variable.c 
    ${d}wizard1.c ${d}wizard2.c ${d}xtra1.c ${d}xtra2.c ${d}z-form.c ${d}z-rand.c ${d}z-term.c 
    ${d}z-util.c ${d}z-virt.c
)
#SET_SOURCE_FILES_PROPERTIES (${libangtk_SRCS} PROPERTIES LOCATION src)

SET_PROPERTY (
    DIRECTORY
    PROPERTY COMPILE_DEFINITIONS
     ${PLATFORM}
     ANGBANDTK
     TNB_BACKUP
     TNB_SQUELCH 
     ALLOW_BORG
     BORG_TK
     ${SDL_CFLAGS}
     ${TREECTRL_CFLAGS}
)

IF (CMAKE_HOST_UNIX)
	ADD_DEFINITIONS (${TCL_DEFS})
ENDIF (CMAKE_HOST_UNIX)

INCLUDE_DIRECTORIES (
    src
    ${COMMONTK}/src/common
    ${COMMONTK}/src/common-dll
    ${COMMONTK}/src/misc
    ${TREECTRL_HDRS}
    ${DBWIN_HDRS}
)

ADD_LIBRARY (angband_library
    SHARED
    ${OmnibandTk_SOURCE_DIR}/src/common/main-tnb.c
    ${common_SRCS}
    ${variant_SRCS}
)
# Clear LINK_INTERFACE_LIBRARIES is so the borg.dll doesn't link with all the stuff angband.dll links with
SET_TARGET_PROPERTIES (angband_library
	PROPERTIES
	OUTPUT_NAME angband
	PREFIX ""
	LINK_INTERFACE_LIBRARIES ""
)
IF (MINGW OR CYGWIN)
    SET_TARGET_PROPERTIES (angband_library
		PROPERTIES
		LINK_FLAGS ${CMAKE_CURRENT_SOURCE_DIR}/angband.def
	)
ENDIF (MINGW OR CYGWIN)

TARGET_LINK_LIBRARIES (angband_library
    common
    misc
    ${TCL_LIBRARY}
    ${TK_LIBRARY}
    ${DBWIN_LIB}
    comdlg32
)

INSTALL (
	TARGETS angband_library
	RUNTIME DESTINATION variant/AngbandTk
)

ADD_SUBDIRECTORY (borg)
