
/* $Id: scene.h,v 1.5 2003/04/07 04:16:05 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_SCENE_H
#define IH_DISPLAYS_ISO_SCENE_H

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
#include "angband/z-disp.h"
#include "displays/iso/text.h"

typedef struct _SceneObject SceneObject;
typedef struct _SceneButton SceneButton;

/* Function prototypes.
 */
void            IH_Scene_Init(void);
void            IH_Scene_Process(SDL_Event * event);
void            IH_Scene_Render(void);

SceneObject    *IH_GetIconAtPosition(int x,
                                     int y);
void            IH_CalcIconPosition(int icon,
                                    int *x,
                                    int *y);

void            IH_SceneButton_SetText(SceneButton * button,
                                       cptr text);
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
     int             object;
     int             x, y;

     int             slot;      // for spell books or inventory items
     int             hilite;

     struct
     {
          SDL_Color       tint_color;
          SDL_Surface    *surface;
          float           alpha;
          int             shown;
          int             tint;
     }
     image;

     struct
     {
          SDL_Color       color;
          char            string[100];
          int             is_dynamic;
          int             mode;
          int             x_align; // Where does the associated text go?
          int             y_align;
          int             font;
          int             shown; // For "hover" text, is it shown?
     }
     text;
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
     IH_SCENE_OBJECT_TYPE_CREATURE,

     IH_SCENE_OBJECT_TYPE_END
};

/* SceneObject objects.
 */
enum
{
     IH_SCENE_OBJECT_TILE,
     IH_SCENE_OBJECT_OBJECT,
     IH_SCENE_OBJECT_CREATURE,
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

/* SceneObject highlight modes.
 */
enum
{
     IH_SCENE_OBJECT_HILITE_NONE,
     IH_SCENE_OBJECT_HILITE_HOVER,
     IH_SCENE_OBJECT_HILITE_CLICK,
     IH_SCENE_OBJECT_HILITE_ACTIVE,

     IH_SCENE_OBJECT_HILITE_END
};

/* SceneObject.text modes.
 */
enum
{
     IH_SCENE_OBJECT_TEXT_MODE_NONE,
     IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
     IH_SCENE_OBJECT_TEXT_MODE_HOVER,

     IH_SCENE_OBJECT_TEXT_MODE_END
};

/* SceneObject text alignments.
 */
enum
{
     IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP,
     IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_LEFT,
     IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_TOP,
     IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_LEFT,
     IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
     IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_BOTTOM,
     IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_RIGHT,
     IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM,
     IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,

     IH_SCENE_OBJECT_TEXT_ALIGN_END
};

/* SceneButton
 */
struct _SceneButton
{
     int             id;
     ihFontPos       pos;
     SDL_Rect        rect;
     SDL_Color       color;
     SDL_Event       event;
     cptr            text;
     int             font_size;
     int             type;
     int             hilite;
     int             selected;
     int             init;
     int             (*activate_func) (SceneButton * button);
};

/* Scene types.
 */
#if 0
enum
{
     IH_IH_SCENE_SPLASH,

     IH_IH_SCENE_INTRO,

     IH_IH_SCENE_TITLE,
     IH_IH_SCENE_TITLE_STAGE_ICONS,
     IH_IH_SCENE_TITLE_STAGE_TILES,
     IH_IH_SCENE_TITLE_STAGE_MISC,
     IH_IH_SCENE_TITLE_STAGE_COMPLETE,

     IH_IH_SCENE_SELECT_CHARACTER,

     IH_IH_SCENE_SELECT_VIDEO,

     IH_IH_SCENE_NEW_CHARACTER,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_GENDER,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_RACE,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_CLASS,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_OPTIONS,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_NAME,
     IH_IH_SCENE_NEW_CHARACTER_STAGE_FINALIZE,

     IH_IH_SCENE_MULTIPLAYER_HOST,

     IH_IH_SCENE_MULTIPLAYER_JOIN,

     IH_IH_SCENE_PLAY,

     IH_IH_SCENE_GRAVE,

     IH_IH_SCENE_END
};
#endif

#endif /* IH_DISPLAYS_ISO_SCENE_H */
