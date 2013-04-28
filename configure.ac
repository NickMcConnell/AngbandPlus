dnl Process this file with autoconf to produce a configure script.

dnl configure.ac script for PosBand
dnl Copyright (c) 2005 Alexander Ulyanov.  See COPYING.txt for details.
dnl Based on Angband 3.0.5 configure.in, but almost completely reworked.

AC_INIT([PosBand], [0.9.9a1], [posband@psd-solutions.com])
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADERS([src/config.h])
AM_INIT_AUTOMAKE(posband, 0.9.9a1)

dnl *** Try to detect Cygwin build ***

AC_CANONICAL_HOST
AC_CANONICAL_TARGET
case $target_os in
  *cygwin* ) CYGWIN=yes;;
	 * ) CYGWIN=no;;
esac

dnl *** Options ***

dnl Display modules

WITH_GCU=yes
WITH_LFB=no
WITH_VCS=no
WITH_WIN=no
WITH_XAW=yes

AC_ARG_WITH([gcu],
AS_HELP_STRING([--without-gcu], [disable curses display module]),
[
  case "${withval}" in
    no)  WITH_GCU=no ;;
    yes) WITH_GCU=yes ;;
    *)   AC_MSG_ERROR([unknown argument for --with-gcu]) ;;
  esac
])

AC_ARG_WITH([lfb],
AS_HELP_STRING([--with-lfb], [enable Linux framebuffer console display module]),
[
  case "${withval}" in
    no)  WITH_LFB=no ;;
    yes)
      AC_DEFINE([USE_LFB], 1, [Use the Linux framebuffer front-end])
      WITH_LFB=yes
      ;;
    *)   AC_MSG_ERROR([unknown argument for --with-lfb]) ;;
  esac
])


AC_ARG_WITH([xaw],
AS_HELP_STRING([--without-xaw], [disable X Athena display module]),
[
  case "${withval}" in
   no)  WITH_XAW=no ;;
   yes) WITH_XAW=yes ;;
   *)   AC_MSG_ERROR([unknown argument for --with-xaw]) ;;
  esac
])

AC_ARG_WITH([vcs],
AS_HELP_STRING([--with-vcs], [enable virtual console display module]),
[
  case "${withval}" in
    no)  WITH_VCS=no ;;
    yes)
      AC_DEFINE([USE_VCS], 1, [Use the Linux virtual console front-end])
      WITH_VCS=yes
      ;;
    *)   AC_MSG_ERROR([unknown argument for --with-vcs]) ;;
  esac
])

AC_ARG_WITH([win],
AS_HELP_STRING([--with-win], [enable Windows display module (disables all others)]),
[
  case "${withval}" in
    no)  WITH_WIN=no ;;
    yes)
      if test "$CYGWIN" = "no"
      then
	AC_MSG_ERROR([Windows display module is available only for Windows.])
      fi
      AC_DEFINE([WINDOWS], 1, [Use the Windows front-end])
      WITH_WIN=yes
      ;;
    *)   AC_MSG_ERROR([unknown argument for --with-win]) ;;
  esac
])

AC_ARG_ENABLE(debug,
AS_HELP_STRING([--enable-debug], [enable debugging features, not recommended]),
[
  case "${enableval}" in
    no)  ENABLE_DEBUG=no  ;;
    yes) ENABLE_DEBUG=yes ;;
    *)   AC_MSG_ERROR([unknown argument for --enable-debug]) ;;
  esac
])

dnl *** Checks for programs. ***

AC_PROG_CC
AC_CHECK_TOOL([M4], [m4], [false])

if test "$WITH_WIN" = "yes"
then
  AC_CHECK_TOOL([WRES], [windres])
fi

AC_EXEEXT

dnl *** Checks for header files. ***

AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h memory.h netdb.h stdlib.h string.h \
		  sys/file.h sys/ioctl.h sys/param.h sys/time.h \
		  sys/timeb.h termio.h termios.h unistd.h])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

dnl *** Checks for library functions. ***

AC_PROG_GCC_TRADITIONAL
AC_FUNC_MEMCMP
AC_TYPE_SIGNAL
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([atexit memmove memset mkdir mkstemp select strchr strrchr strstr strtol])
if test "$WITH_LFB" = "yes"
then
  AC_CHECK_HEADERS([linux/fb.h])
fi

dnl Windows version shouldn't use Cygwin
if test "$WITH_WIN" = "yes"
then
  LIBS="$LIBS -lwinmm"
  LDFLAGS="$LDFLAGS -s -mno-cygwin -mwindows -e _mainCRTStartup"
  CFLAGS="$CFLAGS -mno-cygwin"
else
  
  dnl Unix or Cygwin
  AC_DEFINE([SET_GID], 1, [Multiuser system])
  
  dnl SGI (IRIX) may need -lsun for the NIS version of getpwnam
  AC_CHECK_LIB(sun, getpwnam)
  
  dnl Check for ncurses or curses
  dnl Try -lncurses, -lcurses, -lcurses -ltermcap
  if test x"$WITH_GCU" = "xyes" ; then
    use_ncurses=no
    AC_CHECK_LIB(ncurses, initscr,
      [AC_CHECK_HEADER(ncurses.h, use_ncurses=yes,
        [AC_CHECK_HEADER(/usr/include/ncurses/ncurses.h,
          [CFLAGS="-I/usr/include/ncurses $CFLAGS" use_ncurses=yes])])])
    if test "$use_ncurses" = yes; then
      AC_DEFINE([USE_GCU], 1, [Use the curses front-end])
      AC_DEFINE([USE_NCURSES], 1, [Use the ncurses library])
      LIBS="-lncurses $LIBS"
    else
      AC_CHECK_LIB(curses, initscr,
        [AC_DEFINE([USE_GCU], 1, [Use the curses front-end])
         LIBS="-lcurses $LIBS"],
        [AC_CHECK_LIB(curses, endwin,
           [AC_DEFINE([USE_GCU], 1, [Use the curses front-end])
             LIBS="-lcurses -ltermcap $LIBS"])])
    fi

    dnl Check for some curses functions
    AC_CHECK_FUNCS([getch curs_set])
  fi
  
  dnl Check for X Athena
  if test x"$WITH_XAW" = "xyes" ; then
    AC_PATH_XTRA
    LIBS="-lX11 $X_EXTRA_LIBS $LIBS"
    CFLAGS="$CFLAGS $X_CFLAGS"
    LDFLAGS="$LDFLAGS $X_LIBS"
    dnl X_PRE_LIBS is prepended to LIBS below.
  
    dnl Check for the Athena widget set
    dnl
    dnl Does anyone know if it is OK to put '-lXmu -lXt' *before* X_PRE_LIBS?
    dnl If so this code can be simplified by inserting X_PRE_LIBS in LIBS above.
    dnl Today libraries come in the same sequence as in the old src/Makefile.std,
    dnl since that is presumably the way which has been tested most.
    x_more_libs="$X_PRE_LIBS -lXt"
    AC_CHECK_LIB(Xmu, main, x_more_libs="$X_PRE_LIBS -lXmu -lXt", , $x_more_libs)
    AC_CHECK_LIB(Xext, main, x_more_libs="-lXext $x_more_libs", , $x_more_libs)
    
    dnl Note: Both Xaw libraries and includes may be in nostandard places.
    ac_save_LIBS="$LIBS"
    LIBS="-lXaw $x_more_libs $LIBS"
    AC_CACHE_CHECK(for X11 Athena widget set, ac_cv_athena_widget,
  	         [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
    #include <X11/IntrinsicP.h>
    #include <X11/Xaw/SimpleP.h>
    #include <X11/Xaw/Simple.h>
    #include <X11/Xaw/XawInit.h>]], [[XawInitializeWidgetSet();]])],[ac_cv_athena_widget=yes],[ac_cv_athena_widget=no])])
    case "$ac_cv_athena_widget" in
    yes) AC_DEFINE([USE_XAW], 1, [Use the Athena Widget set front-end]) ;;
    no) if test x"$FORCE_XAW" = "xyes" ; then AC_MSG_ERROR([Xaw library not found]) ; fi ;;
    *)	  LIBS="$X_PRE_LIBS $ac_save_LIBS" ;;
    esac
  fi
fi

dnl --enable-debug disables signal handling (to allow core dumps to be
dnl generated), and sets the data directory to ./lib instead of system-wide
dnl location.
dnl By the coincidence, these settings are also required for Windows build :)
if test x"$ENABLE_DEBUG" = "xyes" -o x"$WITH_WIN" = "xyes"
then
  MY_EXPAND_DIR(game_datadir, "./lib/")
else
  AC_DEFINE([HANDLE_SIGNALS], 1, [Handle signals])
  MY_EXPAND_DIR(game_datadir, "$datadir/games/$PACKAGE/")
fi

AC_DEFINE_UNQUOTED([DEFAULT_PATH], "$game_datadir", [Path to the lib directory])  
AM_CONDITIONAL(WITH_WIN, test "$WITH_WIN" = "yes")

dnl *** Checks for zlib (is this the right place?) ***

AC_CHECK_HEADER([zlib.h], [
  AC_CHECK_LIB([z], [gzopen], 
    AC_DEFINE([GZIP_SAVEFILES], 1, [Compress savefiles with zlib])
    LIBS="-lz $LIBS"
  )
])

AC_CONFIG_FILES([Makefile src/Makefile lib/Makefile lib/apex/Makefile lib/bone/Makefile lib/data/Makefile lib/edit/Makefile lib/file/Makefile lib/help/Makefile lib/help/orig/Makefile lib/info/Makefile lib/pref/Makefile lib/save/Makefile lib/user/Makefile lib/xtra/Makefile lib/xtra/font/Makefile lib/xtra/sound/Makefile])
AC_OUTPUT


if test x"$ENABLE_DEBUG" = "xyes"
then
  AC_MSG_WARN([--enable-debug option should be used by maintainer only.])
fi

dnl vim:sw=2:
