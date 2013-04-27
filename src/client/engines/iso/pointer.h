
/* $Id: pointer.h,v 1.3 2003/04/16 17:30:42 cipher Exp $ */

#ifndef IH_ENGINES_ISO_POINTER_H
#define IH_ENGINES_ISO_POINTER_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers.
 */
#include "angband/h-type.h"

/* Function prototypes (callbacks).
 */
void            IH_ISO_RenderPointer(void);

/* Function prototypes (setup).
 */
errr            IH_ISO_LoadPointers(void);
void            IH_ISO_FreePointers(void);

/* Data definitions.
 */

#endif /* IH_ENGINES_ISO_POINTER_H */
