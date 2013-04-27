
/* $Id: file-win.c,v 1.4 2003/03/17 22:45:32 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/h-config.h"

#ifdef __WIN32__

#include "angband/angband.h"
#include "ironhells.h"
#include "path.h"

cptr
IH_GetDataDir(cptr dir)
{
}

bool
IH_CreateConfigDir(void)
{
}

cptr
IH_GetConfigDir(void)
{
}

cptr
IH_GetManifestFilename(cptr path,
                       int item_num)
{
}

#endif /* __WIN32__ */
