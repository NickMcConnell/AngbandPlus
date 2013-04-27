
/* $Id: ipc.h,v 1.4 2003/03/17 22:45:22 cipher Exp $ */

#ifndef IH_IPC_H
#define IH_IPC_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Function prototypes.
 */
bool            IH_GetIPCValue(long *value);
bool            IH_GetIPCPtr(void *ptr);
bool            IH_GetIPCEvent(SDL_Event * event);
void            IH_SetIPCValue(long value);
void            IH_SetIPCPtr(void *ptr);
void            IH_SetIPCEvent(SDL_Event * event);
void            IH_CreateFakeEvent(SDL_Event * event,
                                   Uint8 type,
                                   ...);

/* Data structures.
 */
enum
{
     IH_IPC_TYPE_VALUE,
     IH_IPC_TYPE_PTR,
     IH_IPC_TYPE_EVENT,

     IH_IPC_TYPE_END
};

#endif /* IH_IPC_H */
