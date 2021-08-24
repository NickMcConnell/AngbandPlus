/*
 * File: buildid.c
 * Purpose: Version strings
 *
 * Copyright (c) 2011 Andi Sidwell
 * Copyright (c) 2016 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"


/*
 * Define for Beta version, undefine for stable build
 */
/*#define VERSION_BETA*/


bool beta_version(void)
{
#ifdef VERSION_BETA
    return true;
#else
    return false;
#endif
}


/*
 * Current version number of PWMAngband
 */
#define VERSION_MAJOR   1
#define VERSION_MINOR   1
#define VERSION_PATCH   11
#define VERSION_EXTRA   10


u16b current_version(void)
{
    return ((VERSION_MAJOR << 12) | (VERSION_MINOR << 8) | (VERSION_PATCH << 4) | VERSION_EXTRA);
}


/*
 * Minimum version number of PWMAngband client allowed
 */
#define MIN_VERSION_MAJOR   1
#define MIN_VERSION_MINOR   1
#define MIN_VERSION_PATCH   11
#define MIN_VERSION_EXTRA   9


u16b min_version(void)
{
    return ((MIN_VERSION_MAJOR << 12) | (MIN_VERSION_MINOR << 8) |
        (MIN_VERSION_PATCH << 4) | MIN_VERSION_EXTRA);
}


/*
 * Name of this Angband variant
 */
#define VERSION_NAME    "PWMAngband "


static char version[32];


char *version_build(int mode)
{
    bool name = ((mode & VB_NAME)? true: false);

    if (mode & VB_BUILD)
    {
        strnfmt(version, sizeof(version), "%s%d.%d.%d (%s %d)", (name? VERSION_NAME: ""),
            VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, (beta_version()? "Beta": "Build"),
            VERSION_EXTRA);
    }
    else
    {
        strnfmt(version, sizeof(version), "%s%d.%d.%d", (name? VERSION_NAME: ""), VERSION_MAJOR,
            VERSION_MINOR, VERSION_PATCH);
    }

    return version;
}




