
/* $Id: init.h,v 1.1 2003/04/16 00:34:42 cipher Exp $ */

#ifndef IH_ENGINES_ISO_INIT_H
#define IH_ENGINES_ISO_INIT_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"

/* Internal headers */
#include "angband/angband.h"
#include "scene.h"
#include "render/misc.h"

/* Function prototypes.
 */
errr            IH_ISO_Init(void);
errr            IH_ISO_InitDisplays(void);
void            IH_ISO_Init(void);

/* Data definitions.
 */
typedef struct _isoEngineData isoEngineData;

struct _isoEngineData
{
     SDL_Surface    *pointers[IH_POINTER_MAX];

     SDL_Surface    *tinting;
     SDL_Surface    *shader;
};

#endif /* IH_ENGINES_ISO_INIT_H */
