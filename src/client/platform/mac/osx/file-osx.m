/* File: file-osx.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#if defined(MACINTOSH) || defined(MACOSX)

#include <Cocoa/Cocoa.h>

#include "angband.h"
#include "path.h"

cptr IH_GetDataDir(cptr dir)
{
     NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
     cptr path, utf8;

     utf8 = [[[NSBundle mainBundle] bundlePath] UTF8String];
     path = IH_PathBuild(utf8, "..", dir, NULL);

     [pool release];
     
     return path;
}

bool IH_CreateConfigDir(void)
{
}

cptr IH_GetConfigDir(void)
{
}

#endif /* MACINTOSH/MACOSX */
