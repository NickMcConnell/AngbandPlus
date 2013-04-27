
/* $Id: ipc.c,v 1.7 2003/04/15 05:02:18 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "ipc.h"

bool
IH_GetIPCValue(long *value)
{
     bool            valid = FALSE;

     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          valid = ih.ipc.is_valid;

          if(ih.ipc.type != IH_IPC_TYPE_VALUE)
               valid = FALSE;

          if(valid && value)
          {
               ih.ipc.is_valid = FALSE;
               *value = ih.ipc.data.value;
               ih.ipc.data.value = 0;
          }

          SDL_SemPost(ih.ipc.sem.talk);
     }

     return valid;
}

bool
IH_GetIPCPtr(void *ptr)
{
     bool            valid = FALSE;

     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          valid = ih.ipc.is_valid;

          if(ih.ipc.type != IH_IPC_TYPE_PTR)
               valid = FALSE;

          if(valid && ptr)
          {
               ih.ipc.is_valid = FALSE;
               ptr = ih.ipc.data.ptr;
               ih.ipc.data.ptr = NULL;
          }

          SDL_SemPost(ih.ipc.sem.talk);
     }

     return valid;
}

bool
IH_GetIPCEvent(SDL_Event * event)
{
     bool            valid = FALSE;

     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          valid = ih.ipc.is_valid;

          if(ih.ipc.type != IH_IPC_TYPE_EVENT)
               valid = FALSE;

          if(valid && event)
          {
               ih.ipc.is_valid = FALSE;
               memcpy(event, &ih.ipc.data.event, sizeof(SDL_Event));
               memset(&ih.ipc.data.event, 0, sizeof(SDL_Event));
          }

          SDL_SemPost(ih.ipc.sem.talk);
     }

     return valid;
}

void
IH_SetIPCValue(long value)
{
     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          ih.ipc.data.value = value;
          ih.ipc.type = IH_IPC_TYPE_VALUE;
          ih.ipc.is_valid = TRUE;

          SDL_SemPost(ih.ipc.sem.talk);
     }
}

void
IH_SetIPCPtr(void *ptr)
{
     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          ih.ipc.data.ptr = ptr;
          ih.ipc.type = IH_IPC_TYPE_PTR;
          ih.ipc.is_valid = TRUE;

          SDL_SemPost(ih.ipc.sem.talk);
     }
}

void
IH_SetIPCEvent(SDL_Event * event)
{
     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          if(event)
               memcpy(&ih.ipc.data.event, event, sizeof(SDL_Event));
          else
               memset(&ih.ipc.data.event, 0, sizeof(SDL_Event));

          ih.ipc.type = IH_IPC_TYPE_EVENT;
          ih.ipc.is_valid = TRUE;

          SDL_SemPost(ih.ipc.sem.talk);
     }
}

void
IH_CreateFakeEvent(SDL_Event * event,
                   Uint8 type,
                   ...)
{
     va_list         ap;

     if(!event)
          return;

     va_start(ap, type);

     switch (type)
     {
          case SDL_KEYDOWN:
               event->key.type = type;
               event->key.state = SDL_PRESSED;
               event->key.keysym.sym = va_arg(ap, SDLKey);
               event->key.keysym.mod = va_arg(ap, SDLMod);
               break;
     }

     va_end(ap);
}