PROJECT (OmnibandTk)

SET (PLATFORM_SDL 0)
IF (PLATFORM_SDL)
	SET (SDL_CFLAGS PLATFORM_SDL REDEFINE_X_FUNCS)
ENDIF (PLATFORM_SDL)

CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

GET_FILENAME_COMPONENT (ROOT
    ${CMAKE_SOURCE_DIR}
    PATH
)
        
SET (TCL_INSTALL_DIR
    ${ROOT}/TclTk-8.5.7
    CACHE PATH
    "Installation prefix of Tcl/Tk"
)
SET (TCL_SOURCE_DIR
    ${ROOT}/tcl8.5.7
    CACHE PATH
    "Path to Tcl sources"
)

IF (PLATFORM_SDL)
	SET (TK_SOURCE_DIR
		${ROOT}/SDLTk-8.5.7
		CACHE PATH
		"Path to Tk sources"
	)
ELSE (PLATFORM_SDL)
	SET (TK_SOURCE_DIR
		${ROOT}/tk8.5.7
		CACHE PATH
		"Path to Tk sources"
	)
ENDIF (PLATFORM_SDL)

FIND_LIBRARY (TCL_LIBRARY
    NAMES
    tcl85 tcl85g tcl8.5 tcl8.5g
    PATHS
    "${TCL_INSTALL_DIR}/lib"
)

IF (PLATFORM_SDL)
	FIND_LIBRARY (TK_LIBRARY
		NAMES
		sdltk85 sdltk85g sdltk8.5 sdltk8.5g
		PATHS
		"${TCL_INSTALL_DIR}/lib"
	)
ELSE (PLATFORM_SDL)
	FIND_LIBRARY (TK_LIBRARY
		NAMES
		tk85 tk85g tk8.5 tk8.5g
		PATHS
		"${TCL_INSTALL_DIR}/lib"
	)
ENDIF (PLATFORM_SDL)

FIND_LIBRARY (TCL_STUB_LIBRARY
    NAMES
    tclstub85 tclstub85g tclstub8.5 tclstub8.5.g
    PATHS
    "${TCL_INSTALL_DIR}/lib"
)

IF (PLATFORM_SDL)
	FIND_LIBRARY (TK_STUB_LIBRARY
		NAMES
		sdltkstub85 sdltkstub85g sdltkstub8.5 sdltkstub8.5g
		PATHS
		"${TCL_INSTALL_DIR}/lib"
	)
ELSE (PLATFORM_SDL)
	FIND_LIBRARY (TK_STUB_LIBRARY
		NAMES
		tkstub85 tkstub85g tkstub8.5 tkstub8.5g
		PATHS
		"${TCL_INSTALL_DIR}/lib"
	)
ENDIF (PLATFORM_SDL)

FIND_PROGRAM (TCL_TCLSH
    NAMES
    tclsh85 tclsh85g tclsh8.5 tclsh8.5g
    PATHS
    "${TCL_INSTALL_DIR}/bin"
)   

FIND_PROGRAM (TK_WISH
    NAMES
    wish85 wish85g wish8.5 wish8.5g
    PATHS
    "${TCL_INSTALL_DIR}/bin"
)   

IF (CMAKE_HOST_UNIX AND NOT CYGWIN)

# - Find library containing dlopen()
# The following variables are set if dlopen is found. If dlopen is not
# found, DLOPEN_FOUND is set to false.
#  DLOPEN_FOUND     - System has dlopen.
#  DLOPEN_LIBRARIES - Link these to use dlopen.
 
# Copyright (c) 2007 Erik Johansson <[EMAIL PROTECTED]>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
 
include(CheckLibraryExists)
 
 # Assume dlopen is not found.
set(DLOPEN_FOUND FALSE)
 
 foreach (library c c_r dl)
  if (NOT DLOPEN_FOUND)
    check_library_exists(${library} dlopen "" DLOPEN_IN_${library})
 
    if (DLOPEN_IN_${library})
      set(DLOPEN_LIBRARIES ${library} CACHE STRING "Library containing dlopen")
      set(DLOPEN_FOUND TRUE)
    endif (DLOPEN_IN_${library})
 
  endif (NOT DLOPEN_FOUND)
 endforeach (library)
 
if (DLOPEN_FOUND)
   if (NOT Dlopen_FIND_QUIETLY)
    message(STATUS "Found dlopen in: ${DLOPEN_LIBRARIES}")
   endif (NOT Dlopen_FIND_QUIETLY)
else (DLOPEN_FOUND)
   if (Dlopen_FIND_REQUIRED)
     message(FATAL_ERROR "Could not find the library containing dlopen")
   endif (Dlopen_FIND_REQUIRED)
endif (DLOPEN_FOUND)
 
mark_as_advanced(DLOPEN_LIBRARIES)

	#
	# Read tclConfig.sh and extract TCL_DEFS
	#
	FILE(READ ${TCL_INSTALL_DIR}/lib/tclConfig.sh contents)
	STRING(REGEX REPLACE ";" "\\\\;" contents "${contents}")
	STRING(REGEX REPLACE "\r?\n" ";" contents "${contents}")
	FOREACH(line ${contents})
		IF("${line}" MATCHES "^TCL_DEFS='([^\n]+)'$")
			STRING(REGEX REPLACE "^TCL_DEFS='([^\n]+)'$" "\\1" TCL_DEFS ${line})
		ENDIF("${line}" MATCHES "^TCL_DEFS='([^\n]+)'$")
	ENDFOREACH(line)
	STRING(REGEX REPLACE "\\\\ " "{HACK}" TCL_DEFS "${TCL_DEFS}")
	STRING(REGEX REPLACE "\\\\" "" TCL_DEFS "${TCL_DEFS}")
	MESSAGE(STATUS "TCL_DEFS#1=${TCL_DEFS}")
	SEPARATE_ARGUMENTS(TCL_DEFS)
	STRING(REGEX REPLACE "{HACK}" " " TCL_DEFS "${TCL_DEFS}")
	MESSAGE(STATUS "TCL_DEFS=${TCL_DEFS}")

	INCLUDE_DIRECTORIES (
		${TCL_SOURCE_DIR}/generic
		${TCL_SOURCE_DIR}/unix
		${TK_SOURCE_DIR}/generic
		${TK_SOURCE_DIR}/unix
	)

	SET (PLATFORM PLATFORM_X11)

ENDIF (CMAKE_HOST_UNIX AND NOT CYGWIN)

IF (CMAKE_HOST_WIN32)
	INCLUDE_DIRECTORIES (
		${TCL_SOURCE_DIR}/generic
		${TCL_SOURCE_DIR}/win
		${TK_SOURCE_DIR}/generic
		${TK_SOURCE_DIR}/win
		${TK_SOURCE_DIR}/xlib
	)
	SET (PLATFORM PLATFORM_WIN)
ENDIF (CMAKE_HOST_WIN32)

IF (PLATFORM_SDL)
	INCLUDE_DIRECTORIES (
		${TCL_SOURCE_DIR}/generic
		${TCL_SOURCE_DIR}/win
		${TK_SOURCE_DIR}/generic
		${TK_SOURCE_DIR}/sdl
		${TK_SOURCE_DIR}/xlib
		${ROOT}/SDL-1.2.8-all/include
	)
	SET (PLATFORM PLATFORM_X11)
ENDIF (PLATFORM_SDL)

SET (DBWIN_HDRS
	${CMAKE_CURRENT_SOURCE_DIR}/src/dbwin
	CACHE PATH
	"Location of dbwin headers"
)
SET (DBWIN_LIB libdbwin)

SET (TREECTRL_CFLAGS "")
# SET (TREECTRL_CFLAGS TREECTRL_DEBUG)
FIND_PATH (TREECTRL_HDRS
	NAMES tkTreeCtrl.h
	PATHS ${ROOT}/tktreectrl/generic ${ROOT}/tktreectrl-2.2.9/generic
	NO_DEFAULT_PATH
)

FIND_PATH (ZLIB_HDRS 
	NAMES zlib.h
	PATHS ${ROOT}/zlib-122
	NO_DEFAULT_PATH
)

SET (COMMONTK ${CMAKE_CURRENT_SOURCE_DIR})

IF (CYGWIN)
	# Remove -Wl,--export-all-symbols -Wl,--enable-auto-import
	SET(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS "-shared")
ENDIF (CYGWIN)

IF (CMAKE_HOST_UNIX AND NOT CYGWIN)
	SET (CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
	SET (CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
ENDIF (CMAKE_HOST_UNIX AND NOT CYGWIN)

ADD_SUBDIRECTORY (src)
ADD_SUBDIRECTORY (variant)
