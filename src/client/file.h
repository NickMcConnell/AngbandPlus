#ifndef IH_FILE_H
#define IH_FILE_H

/* File: file.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* Function prototypes.
*/

cptr IH_GetDataDir(cptr dir);
bool IH_CreateConfigDir(void);
cptr IH_GetConfigDir(void);

#endif /* IH_FILE_H */
