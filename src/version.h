/* File: version.h */

/* Purpose: version-related global constants */

/*
 * Name of the version/variant
 */
#define VERSION_NAME "Z+Angband"

/* User-visible version */
#define VER_MAJOR 0
#define VER_MINOR 3
#define VER_PATCH 3
#define VER_EXTRA 0

/* Versions after release */
#define VER_AFTER ""

/* Stringify argument */
#define Z_STR(a) Z_STR1(a)

#define Z_STR1(a)\
	# a

/* Pre-release version string */
#if VER_EXTRA != 0
	#define PRE_VERSION \
		"pre" Z_STR(VER_EXTRA)
#else
	#define PRE_VERSION
#endif /* VER_EXTRA != 0 */


/*
 * Current version string
 */
#define VERSION_STRING \
	Z_STR(VER_MAJOR) "." Z_STR(VER_MINOR) "." Z_STR(VER_PATCH) PRE_VERSION VER_AFTER




