/* File: file-win.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#ifdef WINDOWS

#include "angband.h"
#include "path.h"

cptr IH_GetDataDir(cptr dir)
{
     cptr path, utf8;
#if 0
     NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

     utf8 = [[[NSBundle mainBundle] bundlePath] UTF8String];
     path = IH_PathBuild(utf8, "..", dir, NULL);

     [pool release];
#endif 
     return path;
}

#endif /* WINDOWS */
