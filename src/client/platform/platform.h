
/* $Id: platform.h,v 1.3 2003/04/07 20:53:58 cipher Exp $ */

#ifndef IH_FILE_H
#define IH_FILE_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers */
#include "angband/angband.h"

/* Function prototypes.
*/

char           *IH_GetDataDir(cptr dir);
bool            IH_CreateConfigDir(void);
char           *IH_GetConfigDir(void);
int             IH_GetSaveFiles(char **list,
                                int size);
int             IH_GetFileContents(cptr filename,
                                   char **buf);

#endif /* IH_FILE_H */
