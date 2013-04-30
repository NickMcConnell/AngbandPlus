/*
 * File: buildid.c
 * Purpose: Version strings
 *
 * Copyright (c) 2011 Angband, MAngband and PWMAngband Developers
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
#include "buildid.h"


static char version[32];


char *get_buildid(bool full)
{
    if (full)
    {
        int build = VERSION_EXTRA;

        if (build > 0)
            strnfmt(version, sizeof(version), "%s %d.%d.%d (Build %d)", VERSION_NAME,
                VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_EXTRA);
        else
            strnfmt(version, sizeof(version), "%s %d.%d.%d (Beta)", VERSION_NAME,
                VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
    }
    else
        strnfmt(version, sizeof(version), "%s %d.%d.%d", VERSION_NAME, VERSION_MAJOR,
            VERSION_MINOR, VERSION_PATCH);

    return version;
}


char *get_buildver(void)
{
    strnfmt(version, sizeof(version), "%d.%d.%d", VERSION_MAJOR, VERSION_MINOR,
        VERSION_PATCH);

    return version;
}





