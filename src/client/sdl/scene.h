
/* $Id: scene.h,v 1.6 2003/03/18 19:17:40 cipher Exp $ */

#ifndef IH_SDL_SCENE_H
#define IH_SDL_SCENE_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "angband.h"
#include "sdl/render/text.h"

typedef struct _SceneObject SceneObject;
typedef struct _SceneButton SceneButton;

/* Function prototypes.
 */
void            IH_InitScene(void);
void            IH_ProcessScene(SDL_Event * event);
SceneObject    *IH_GetIconAtPosition(int x,
                                     int y);
void            IH_CalcIconPosition(int icon,
                                    int *x,
                                    int *y);

SceneObject    *IH_SceneObjectAlloc(int type,
                                    int object);
void            IH_SceneObjectFree(SceneObject * object);

void            IH_SetLoadMessage(cptr msg);
void            IH_SetErrorMessage(cptr msg);
void            IH_SetScene(int scene);
void            IH_SetStage(int stage);

/* Data definitions.
 */
struct _SceneObject
{
     int             type;
     int             x, y;
     int             object;

     int             slot;      // for spell books or inventory items
     int             hilite;
     bool            shown;

     union
     {
          SDL_Surface    *image;
          char           *text;
     }
     data;
};

/* SceneObject types.
 */
enum
{
     IH_SCENE_OBJECT_TYPE_NONE,
     IH_SCENE_OBJECT_TYPE_ICON,
     IH_SCENE_OBJECT_TYPE_MISC,
     IH_SCENE_OBJECT_TYPE_TEXT,
     IH_SCENE_OBJECT_TYPE_TILE,
     IH_SCENE_OBJECT_TYPE_OBJECT,

     IH_SCENE_OBJECT_TYPE_END
};

/* SceneObject objects.
 */
enum
{
     IH_SCENE_OBJECT_TILE,
     IH_SCENE_OBJECT_OBJECT,
     IH_SCENE_OBJECT_CHARACTER,
     IH_SCENE_OBJECT_ARMOR_CLASS,
     IH_SCENE_OBJECT_CHARACTER_LEVEL,
     IH_SCENE_OBJECT_STRENGTH,
     IH_SCENE_OBJECT_INTELLIGENCE,
     IH_SCENE_OBJECT_WISDOM,
     IH_SCENE_OBJECT_DEXTERITY,
     IH_SCENE_OBJECT_CONSTITUTION,
     IH_SCENE_OBJECT_CHARISMA,
     IH_SCENE_OBJECT_GOLD,
     IH_SCENE_OBJECT_BURDEN,
     IH_SCENE_OBJECT_LMXDECO,
     IH_SCENE_OBJECT_LIFE,
     IH_SCENE_OBJECT_MANA,
     IH_SCENE_OBJECT_XP,
     IH_SCENE_OBJECT_INVENTORY,
     IH_SCENE_OBJECT_SPELL_BOOK,
     IH_SCENE_OBJECT_INVENTORY_SLOT,
     IH_SCENE_OBJECT_DEPTH,
     IH_SCENE_OBJECT_HUNGRY,
     IH_SCENE_OBJECT_GRAZE,
     IH_SCENE_OBJECT_BLIND,
     IH_SCENE_OBJECT_CONFUSED,
     IH_SCENE_OBJECT_AFRAID,
     IH_SCENE_OBJECT_POISONED,
     IH_SCENE_OBJECT_STUN,
     IH_SCENE_OBJECT_STUDY,

     IH_SCENE_OBJECT_END
};

enum
{
     IH_SCENE_OBJECT_HILITE_NONE,
     IH_SCENE_OBJECT_HILITE_HOVER,
     IH_SCENE_OBJECT_HILITE_CLICK,
     IH_SCENE_OBJECT_HILITE_ACTIVE,

     IH_SCENE_OBJECT_HILITE_END
};

/* SceneButton
 */
struct _SceneButton
{
     ihFontPos       pos;
     SDL_Rect        rect;
     SDL_Color       color;
     SDL_Event       event;
     cptr            text;
     int             type;
     int             hilite;
};

/* Scene types.
 */
enum
{
     IH_SCENE_SPLASH,

     IH_SCENE_INTRO,

     IH_SCENE_TITLE,
     IH_SCENE_TITLE_STAGE_ICONS,
     IH_SCENE_TITLE_STAGE_TILES,
     IH_SCENE_TITLE_STAGE_MISC,
     IH_SCENE_TITLE_STAGE_COMPLETE,

     IH_SCENE_SELECT_CHARACTER,

     IH_SCENE_NEW_CHARACTER,
     IH_SCENE_NEW_CHARACTER_STAGE_GENDER,
     IH_SCENE_NEW_CHARACTER_STAGE_RACE,
     IH_SCENE_NEW_CHARACTER_STAGE_CLASS,
     IH_SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL,
     IH_SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED,
     IH_SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER,
     IH_SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE,
     IH_SCENE_NEW_CHARACTER_STAGE_NAME,
     IH_SCENE_NEW_CHARACTER_STAGE_FINALIZE,

     IH_SCENE_MULTIPLAYER_HOST,

     IH_SCENE_MULTIPLAYER_JOIN,

     IH_SCENE_OPTIONS,

     IH_SCENE_PLAY,

     IH_SCENE_GRAVE,

     IH_SCENE_END
};

#endif /* IH_SDL_SCENE_H */
