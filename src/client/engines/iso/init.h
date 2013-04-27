
/* $Id: init.h,v 1.4 2003/04/18 21:45:11 cipher Exp $ */

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
#include "SDL_ttf.h"

/* Internal headers */
#include "angband/angband.h"
#include "scene.h"
#include "render/misc.h"
#include "render/pointer.h"

/* Function prototypes.
 */
errr            IH_ISO_Init(void);
errr            IH_ISO_InitDisplays(void);
void            IH_ISO_Cleanup(void);

/* Data definitions.
 */
typedef struct _isoEngineData isoEngineData;

struct _isoEngineData
{
     SDL_Surface    *pointers[IH_POINTER_MAX];

     SDL_Surface    *tinting;
     SDL_Surface    *shader;

     SDL_Surface    *background;
     SDL_Surface    *splash;
     SDL_Surface    *title;

     SceneObject    *player_image;

     TTF_Font       *normal_font;
     TTF_Font       *large_font;
};

#endif /* IH_ENGINES_ISO_INIT_H */
