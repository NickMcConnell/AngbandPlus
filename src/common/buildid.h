/*
 * File: buildid.h
 * Purpose: Version strings
 */

extern bool beta_version(void);
extern u16b current_version(void);
extern u16b min_version(void);

/*
 * Modes for version_build().
 */
enum
{
    VB_BASE     = 0x00, /* Base version string */
    VB_BUILD    = 0x01, /* Add the build (or beta) number */
    VB_NAME     = 0x02  /* Add the name */
};

extern char *version_build(int mode);
