/*
 * File: buildid.h
 * Purpose: Version strings
 */

/*
 * Name of this Angband variant
 */
#define VERSION_NAME    "PWMAngband"

extern bool beta_version(void);
extern u16b current_version(void);
extern u16b min_version(void);
extern char *version_build(const char *label, bool build);
