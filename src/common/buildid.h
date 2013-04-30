/*
 * File: buildid.h
 * Purpose: Version strings
 */

#define VERSION_NAME  "PWMAngband"

/*
 * Current version number of PWMAngband
 */
#define VERSION_MAJOR    1
#define VERSION_MINOR    1
#define VERSION_PATCH    9
#define VERSION_EXTRA    2

#define CUR_VERSION ((VERSION_MAJOR << 12) | (VERSION_MINOR << 8) | \
    (VERSION_PATCH << 4) | VERSION_EXTRA)

/*
 * Minimum version number of PWMAngband client allowed
 */
#define MIN_VERSION_MAJOR   1
#define MIN_VERSION_MINOR   1
#define MIN_VERSION_PATCH   9
#define MIN_VERSION_EXTRA   1

#define MIN_VERSION ((MIN_VERSION_MAJOR << 12) | (MIN_VERSION_MINOR << 8) | \
    (MIN_VERSION_PATCH << 4) | MIN_VERSION_EXTRA)

extern char *get_buildid(bool full);
extern char *get_buildver(void);
