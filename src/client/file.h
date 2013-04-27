
/* $Id: file.h,v 1.9 2003/03/24 06:04:50 cipher Exp $ */

#ifndef IH_FILE_H
#define IH_FILE_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "list.h"

/* Function prototypes.
*/

char           *IH_GetDataDir(cptr dir);
bool            IH_CreateConfigDir(void);
char           *IH_GetConfigDir(void);
ihList         *IH_GetSaveFiles(void);
int             IH_GetFileContents(cptr filename,
                                   char **buf);

#endif /* IH_FILE_H */
