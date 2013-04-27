
/* $Id: scene.h,v 1.9 2003/04/18 21:45:08 cipher Exp $ */

#ifndef IH_SCENE_H
#define IH_SCENE_H

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
#include "ironhells.h"
#include "scene.h"
#include "render/misc.h"
#include "render/text.h"

/* Forward declarations */
typedef struct _SceneObject SceneObject;
typedef struct _SceneButton SceneButton;

/* Function prototypes.
 */
void            IH_Scene_Init(void);
void            IH_Scene_Process(SDL_Event * event);
void            IH_Scene_Render(void);

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
          ihColor         tint;
          void           *bits;
          float           alpha;
          int             width, height;

          unsigned        is_shown:1;
          unsigned        is_tinted:1;
     }
     image;

     struct
     {
          ihColor         color;
          char            string[100];
          int             mode;
          u32b            flags;
          int             x_align; // Where does the associated text go?
          int             y_align;
          int             font;

          unsigned        is_shown:1; // For "hover" text, is it shown?
          unsigned        is_dynamic:1;
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
     SDL_Rect        rect;
     SDL_Event       event;
     ihFontPos       pos;
     ihColor         color;
     cptr            text;
     int             font_size;
     int             type;
     int             hilite;
     int             selected;
     int             init;
     int             (*activate_func) (SceneButton * button);
};

#endif /* IH_SCENE_H */
