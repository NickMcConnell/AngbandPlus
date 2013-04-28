ADD_DEFINITIONS (
    -D${PLATFORM}
    -DANGBANDTK
    -DBORG_BUILD
    -DBORG_TK
    -DALLOW_BORG
)

INCLUDE_DIRECTORIES (
    ${COMMONTK}/variant/AngbandTk/src
    ${COMMONTK}/src/common
    ${COMMONTK}/src/common-dll
)

SET (SRCS_ borg1.c borg2.c borg3.c borg4.c borg5.c borg6.c borg7.c borg8.c borg9.c)
FOREACH (SRC ${SRCS_})
    LIST (APPEND SRCS src/${SRC})
ENDFOREACH (SRC)

ADD_LIBRARY (borg SHARED
    ${SRCS}
)

SET_TARGET_PROPERTIES (borg PROPERTIES PREFIX "")

TARGET_LINK_LIBRARIES (borg
    angband_library
)

INSTALL (
	TARGETS borg
	RUNTIME DESTINATION variant/AngbandTk/borg/apw292
)
