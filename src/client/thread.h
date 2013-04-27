
/* $Id: thread.h,v 1.4 2003/03/17 22:45:22 cipher Exp $ */

#ifndef IH_THREAD_H
#define IH_THREAD_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

int             IH_GameThread(void *data);
bool            IH_InitSemaphores(void);
void            IH_DestroySemaphores(void);

#endif /* IH_THREAD_H */
