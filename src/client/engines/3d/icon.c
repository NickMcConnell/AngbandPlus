
/* $Id: icon.c,v 1.3 2003/04/16 17:30:16 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

#ifdef BUILD_3D_ENGINE

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "displays/iso/icon.h"
#include "displays/iso/misc.h"
#include "displays/iso/scene.h"
#include "displays/iso/render.h"
#include "platform/platform.h"

static SceneObject *icons[IH_MAX_ICONS];
static int      icon_count = 0;

static SDL_Surface *tinting = NULL;

struct IconObjectInit
{
     const char     *name;
     int             object;
     int             is_dynamic;
     int             mode;
     int             x_align;
     int             y_align;
};

struct IconObjectPos
{
     int             object;
     int             dir, anchor;
     int             x_offset, y_offset;
};

enum
{
     IH_ICON_DIR_DOWN = 0,
     IH_ICON_DIR_UP,
     IH_ICON_DIR_LEFT,
     IH_ICON_DIR_RIGHT
};

enum
{
     IH_ICON_ANCHOR_TOPLEFT = 0,
     IH_ICON_ANCHOR_TOPRIGHT,
     IH_ICON_ANCHOR_BOTTOMLEFT,
     IH_ICON_ANCHOR_BOTTOMRIGHT
};

static struct IconObjectInit general_icons[] = {
     {NULL}
};

static struct IconObjectInit size_icons[] = {
     {"armor-class", IH_SCENE_OBJECT_ARMOR_CLASS, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"character-level", IH_SCENE_OBJECT_CHARACTER_LEVEL, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"strength", IH_SCENE_OBJECT_STRENGTH, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"intelligence", IH_SCENE_OBJECT_INTELLIGENCE, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"wisdom", IH_SCENE_OBJECT_WISDOM, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"dexterity", IH_SCENE_OBJECT_DEXTERITY, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"constitution", IH_SCENE_OBJECT_CONSTITUTION, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"charisma", IH_SCENE_OBJECT_CHARISMA, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"gold", IH_SCENE_OBJECT_GOLD, TRUE, IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"burden", IH_SCENE_OBJECT_BURDEN, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"inventory", IH_SCENE_OBJECT_INVENTORY, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"book", IH_SCENE_OBJECT_SPELL_BOOK, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"hungry", IH_SCENE_OBJECT_HUNGRY, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_HOVER, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"graze", IH_SCENE_OBJECT_GRAZE, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_HOVER, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"blind", IH_SCENE_OBJECT_BLIND, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"confused", IH_SCENE_OBJECT_CONFUSED, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"afraid", IH_SCENE_OBJECT_AFRAID, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"poisoned", IH_SCENE_OBJECT_POISONED, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"stun", IH_SCENE_OBJECT_STUN, FALSE, IH_SCENE_OBJECT_TEXT_MODE_HOVER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"study", IH_SCENE_OBJECT_STUDY, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"depth", IH_SCENE_OBJECT_DEPTH, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {NULL, 0}
};

static struct IconObjectPos icon_pos[] = {
     {IH_SCENE_OBJECT_CHARACTER, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 0},
     {IH_SCENE_OBJECT_ARMOR_CLASS, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 1},
     {IH_SCENE_OBJECT_CHARACTER_LEVEL, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 2},
     {IH_SCENE_OBJECT_STRENGTH, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 3},
     {IH_SCENE_OBJECT_INTELLIGENCE, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 4},
     {IH_SCENE_OBJECT_WISDOM, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      5},
     {IH_SCENE_OBJECT_DEXTERITY, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 6},
     {IH_SCENE_OBJECT_CONSTITUTION, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 7},
     {IH_SCENE_OBJECT_CHARISMA, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 8},
     {IH_SCENE_OBJECT_GOLD, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      9},
     {IH_SCENE_OBJECT_BURDEN, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      10},
     {IH_SCENE_OBJECT_INVENTORY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_TOPRIGHT,
      0, 0},
     {IH_SCENE_OBJECT_INVENTORY_SLOT, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPRIGHT, 0, 1},
     {IH_SCENE_OBJECT_SPELL_BOOK, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_TOPRIGHT, 1, 0},
     {IH_SCENE_OBJECT_DEPTH, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      0, 0},
     {IH_SCENE_OBJECT_HUNGRY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      1, 0},
     {IH_SCENE_OBJECT_GRAZE, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      2, 0},
     {IH_SCENE_OBJECT_BLIND, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      3, 0},
     {IH_SCENE_OBJECT_CONFUSED, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_BOTTOMRIGHT, 4, 0},
     {IH_SCENE_OBJECT_AFRAID, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      5, 0},
     {IH_SCENE_OBJECT_POISONED, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_BOTTOMRIGHT, 6, 0},
     {IH_SCENE_OBJECT_STUN, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      7, 0},
     {IH_SCENE_OBJECT_STUDY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      8, 0},
     {IH_SCENE_OBJECT_END}
};

static void
format_stat(int val,
            char *out_val)
{
     /* Above 18 */
     if(val > 18)
     {
          int             bonus = (val - 18);

          if(bonus >= 100)
          {
               sprintf(out_val, "18/%03d", bonus);
          }
          else
          {
               sprintf(out_val, "18/%02d", bonus);
          }
     }

     /* From 3 to 18 */
     else
     {
          sprintf(out_val, "%2d", val);
     }
}

static void
icon_create_tint(int w,
                 int h,
                 SDL_Color * color)
{
     SDL_Rect        irect;

     if(!color)
          return;

     /* Make sure the size of the tinting surface is correct.
      */
     if(tinting)
     {
          if(w != tinting->w || h != tinting->h)
          {
               /* Size is incorrect, so free the old surface.
                */
               SDL_FreeSurface(tinting);
               tinting = NULL;
          }
     }

     /* Create a new surface?
      */
     if(!tinting)
     {
          Uint32          rmask, gmask, bmask, amask;

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
          rmask = 0xff000000;
          gmask = 0x00ff0000;
          bmask = 0x0000ff00;
          amask = 0x000000ff;
#else
          rmask = 0x000000ff;
          gmask = 0x0000ff00;
          bmask = 0x00ff0000;
          amask = 0xff000000;
#endif

          tinting = SDL_CreateRGBSurface(SDL_SWSURFACE | SDL_SRCALPHA,
                                         w, h, 32, rmask, gmask, bmask,
                                         amask);
     }

     /* Paranoia.
      */
     if(!tinting)
          return;

     irect.x = 0;
     irect.y = 0;
     irect.w = w;
     irect.h = h;

     /* Fill the square with color.
      */
     SDL_FillRect(tinting,
                  &irect,
                  SDL_MapRGBA(tinting->format,
                              color->r, color->g, color->b,
                              IH_ALPHA_VALUE));

     /* Set the alpha on the shader surface.
      */
     SDL_SetAlpha(tinting, SDL_SRCALPHA | SDL_RLEACCEL, IH_ALPHA_VALUE);
}

SDL_Surface    *
IH_LoadIcon(int icon_size,
            cptr name)
{
     SDL_Surface    *image = NULL;
     char           *path_data, *path_size, *filename, *file;
     cptr            size = NULL;
     int             filename_len;

     if(!name)
          return NULL;

     path_data = IH_GetDataDir("gfx");

     if(icon_size == IH_ICON_SIZE_CURRENT)
          icon_size = ih.icon_size;

     switch (icon_size)
     {
          case IH_ICON_SIZE_LARGE:
               size = "large";
               break;

          case IH_ICON_SIZE_MEDIUM:
               size = "medium";
               break;

          case IH_ICON_SIZE_SMALL:
               size = "small";
               break;

          default:
               size = NULL;
               break;
     }
     if(size)
          path_size = IH_PathBuild(path_data, "icon", size, NULL);
     else
          path_size = IH_PathBuild(path_data, "icon", NULL);

     filename_len = strlen(name) + strlen(IH_IMAGE_FORMAT_EXT) + 2;
     filename = ralloc(filename_len);
     my_strcpy(filename, name, filename_len);
     my_strcat(filename, "." IH_IMAGE_FORMAT_EXT, filename_len);

     file = IH_PathBuild(path_size, filename, NULL);
     rnfree(filename);

     if(file)
     {
          SDL_Surface    *t_image;

          /* Load the image.
           */
          t_image = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);

          /* Convert the surface to match the display.
           */
          if(t_image)
          {
               image = SDL_DisplayFormatAlpha(t_image);

               SDL_FreeSurface(t_image);
          }

          rnfree(file);
     }

     rnfree(path_size);
     rnfree(path_data);

     return image;
}

errr
IH_LoadIcons(void)
{
     errr            rc = 0;
     int             i;

     memset(icons, 0, sizeof(icons));
     icon_count = 0;

     /* Load character image according to class.
      */
     // IH_LoadCharacterIcon();

     /* Load the general icon images.
      */

     /* Load the size icon images.
      */
     for(i = 0; size_icons[i].name; i++)
     {
          SceneObject    *object;

          object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_ICON,
                                       size_icons[i].object);
          if(object)
          {
               icons[icon_count++] = object;

               object->text.is_dynamic = size_icons[i].is_dynamic;
               object->text.mode = size_icons[i].mode;
               object->text.x_align = size_icons[i].x_align;
               object->text.y_align = size_icons[i].y_align;

               object->image.surface =
                   IH_LoadIcon(IH_ICON_SIZE_CURRENT, size_icons[i].name);
               if(!object->image.surface)
               {
#ifdef DEBUG
                    fprintf(stderr,
                            "Unable to load icon image: %s: %s\n",
                            size_icons[i].name, IMG_GetError());
#endif
                    rc = IH_ERROR_CANT_LOAD_ICON;
               }
          }
     }

     /* Do icon positioning.
      */
     IH_PositionIcons();

     return rc;
}

errr
IH_LoadCharacterIcon(void)
{
     errr            rc = 0;

     return rc;
}

void
IH_PositionIcons(void)
{
     int             i;

#ifdef DEBUG
     fprintf(stderr, "IH_PositionIcons()\n");
#endif

     for(i = 0; i < icon_count; i++)
     {
          SceneObject    *object;
          struct IconObjectPos *pos;
          int             j;
          int             base_x = 0, base_y = 0;
          int             dir_x = 0, dir_y = 0;
          int             slot_x = 0, slot_y = 0;
          int             icon_size;

          object = (SceneObject *) icons[i];
          if(!object)
               continue;

#ifdef DEBUG
          fprintf(stderr, "object->type = %d\n", object->type);
          fprintf(stderr, "object->object = %d\n", object->object);
#endif

          pos = NULL;

          for(j = 0; icon_pos[j].object != IH_SCENE_OBJECT_END; j++)
          {
               if(icon_pos[j].object == object->object)
               {
#ifdef DEBUG
                    fprintf(stderr, "Object matches positioning data.\n");
#endif

                    pos = &icon_pos[j];
                    break;
               }
          }

          if(!pos)
               continue;

#ifdef DEBUG
          fprintf(stderr, "Getting icon size value.\n");
#endif
          switch (ih.icon_size)
          {
               default:
               case IH_ICON_SIZE_SMALL:
                    icon_size = IH_ICON_SIZE_SMALL_VALUE;
                    break;

               case IH_ICON_SIZE_MEDIUM:
                    icon_size = IH_ICON_SIZE_MEDIUM_VALUE;
                    break;

               case IH_ICON_SIZE_LARGE:
                    icon_size = IH_ICON_SIZE_LARGE_VALUE;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr, "icon_size = %d\n", icon_size);
#endif

#ifdef DEBUG
          fprintf(stderr, "Getting anchor positions.\n");
#endif
          switch (pos->anchor)
          {
               case IH_ICON_ANCHOR_TOPLEFT:
                    base_x = 0;
                    base_y = 0;
                    break;

               case IH_ICON_ANCHOR_BOTTOMLEFT:
                    base_x = 0;
                    base_y = ih.display_height - icon_size;
                    break;

               case IH_ICON_ANCHOR_TOPRIGHT:
                    base_x = ih.display_width - icon_size;
                    base_y = 0;
                    break;

               case IH_ICON_ANCHOR_BOTTOMRIGHT:
                    base_x = ih.display_width - icon_size;
                    base_y = ih.display_height - icon_size;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr, "base_x = %d\nbase_y = %d\n", base_x, base_y);
#endif

#ifdef DEBUG
          fprintf(stderr, "Getting directional info.\n");
#endif
          switch (pos->dir)
          {
               case IH_ICON_DIR_DOWN:
                    base_y = 0;
                    dir_x = 0;
                    dir_y = 1;
                    if(object->slot > 0)
                         slot_y = object->slot - 1;
                    break;

               case IH_ICON_DIR_UP:
                    base_y = ih.display_height - icon_size;
                    dir_x = 0;
                    dir_y = -1;
                    if(object->slot > 0)
                         slot_y = object->slot - 1;
                    break;

               case IH_ICON_DIR_RIGHT:
                    base_x = 0;
                    dir_x = 1;
                    dir_y = 0;
                    if(object->slot > 0)
                         slot_x = object->slot - 1;
                    break;

               case IH_ICON_DIR_LEFT:
                    base_x = ih.display_width - icon_size;
                    dir_x = -1;
                    dir_y = 0;
                    if(object->slot > 0)
                         slot_x = object->slot - 1;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr,
                  "base_x = %d\nbase_y = %d\ndir_x = %d\ndir_y = %d\nslot_x = %d\nslot_y = %d\n",
                  base_x, base_y, dir_x, dir_y, slot_x, slot_y);
#endif

          object->x =
              base_x + (pos->x_offset * icon_size * dir_x) +
              (slot_x * icon_size * dir_x);
#ifdef DEBUG
          fprintf(stderr, "object->x = %d\n", object->x);
#endif

          object->y =
              base_y + (pos->y_offset * icon_size * dir_y) +
              (slot_y * icon_size * dir_y);
#ifdef DEBUG
          fprintf(stderr, "object->y = %d\n", object->y);
#endif
     }
}

void
IH_UpdateSceneObject(SceneObject * object)
{
     SDL_Rect        rect;

     if(!object)
          return;

     if(object->type != IH_SCENE_OBJECT_TYPE_ICON &&
        object->type != IH_SCENE_OBJECT_TYPE_MISC)
          return;

     /* Get rid of the old dynamically-generated text.
      */
#if 0
     if(object->text.is_dynamic)
     {
          memset(object->text.string, 0, sizeof(object->text.string));
     }
#endif

     /* Set some defaults.
      */
     IH_AttrToColor(COLOR_WHITE, &object->text.color);
     IH_AttrToColor(COLOR_WHITE, &object->image.tint_color);
     object->image.tint = FALSE;

     /* Update the text.
      */
     switch (object->object)
     {
          case IH_SCENE_OBJECT_CHARACTER:
               my_strcpy(object->text.string, op_ptr->full_name,
                         sizeof(object->text.string));
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_ARMOR_CLASS:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "[%d,%+d]", p_ptr->dis_ac, p_ptr->dis_to_a);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_CHARACTER_LEVEL:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%d", p_ptr->lev);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_STRENGTH:
               format_stat(p_ptr->stat_use[0], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_INTELLIGENCE:
               format_stat(p_ptr->stat_use[1], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_WISDOM:
               format_stat(p_ptr->stat_use[2], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_DEXTERITY:
               format_stat(p_ptr->stat_use[3], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_CONSTITUTION:
               format_stat(p_ptr->stat_use[4], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_CHARISMA:
               format_stat(p_ptr->stat_use[5], object->text.string);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_GOLD:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%ld", p_ptr->au);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_BURDEN:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%ld.%ld lbs",
                       p_ptr->total_weight / 10L,
                       p_ptr->total_weight % 10L);
               object->text.shown = TRUE;
               break;

          case IH_SCENE_OBJECT_LIFE:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%d/%d", p_ptr->chp, p_ptr->mhp);

               if(p_ptr->chp >= p_ptr->mhp)
               {
                    /* White */
                    IH_AttrToColor(COLOR_WHITE, &object->text.color);
               }
               else if(p_ptr->chp >
                       (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
               {
                    /* Yellow */
                    IH_AttrToColor(COLOR_YELLOW, &object->text.color);
               }
               else
               {
                    /* Red */
                    IH_AttrToColor(COLOR_RED, &object->text.color);
               }
               break;

          case IH_SCENE_OBJECT_MANA:
               if(cp_ptr->spell_book)
               {
                    strnfmt(object->text.string,
                            sizeof(object->text.string), "%d/%d",
                            p_ptr->csp, p_ptr->msp);

                    if(p_ptr->csp >= p_ptr->msp)
                    {
                         /* White */
                         IH_AttrToColor(COLOR_WHITE, &object->text.color);
                    }
                    else if(p_ptr->csp >
                            (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
                    {
                         /* Yellow */
                         IH_AttrToColor(COLOR_YELLOW, &object->text.color);
                    }
                    else
                    {
                         /* Red */
                         IH_AttrToColor(COLOR_RED, &object->text.color);
                    }
               }
               break;

          case IH_SCENE_OBJECT_XP:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%ld [%ld]", (long) p_ptr->exp,
                       (long) 0 /* FIXME */ );
               break;

          case IH_SCENE_OBJECT_INVENTORY:
               break;

          case IH_SCENE_OBJECT_SPELL_BOOK:
               break;

          case IH_SCENE_OBJECT_INVENTORY_SLOT:
               break;

          case IH_SCENE_OBJECT_DEPTH:
               object->text.shown = TRUE;
               if(!p_ptr->depth)
               {
                    strcpy(object->text.string, "Town");
               }
               else if(depth_in_feet)
               {
                    sprintf(object->text.string, "%d'", p_ptr->depth * 50);
               }
               else
               {
                    sprintf(object->text.string, "%d", p_ptr->depth);
               }
               break;

          case IH_SCENE_OBJECT_STUDY:
               if(p_ptr->new_spells)
               {
                    object->image.shown = TRUE;
                    object->text.shown = TRUE;
                    strnfmt(object->text.string,
                            sizeof(object->text.string), "%d",
                            (int) p_ptr->new_spells);
               }
               else
               {
                    object->image.shown = FALSE;
                    object->text.shown = FALSE;
               }
               break;

          case IH_SCENE_OBJECT_HUNGRY:
               {
                    int             shown = TRUE;

                    object->image.alpha = 1.0f;
                    object->image.tint = FALSE;

                    /* Fainting / Starving */
                    if(p_ptr->food < PY_FOOD_FAINT)
                    {
                         IH_AttrToColor(COLOR_RED,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Weak",
                                   sizeof(object->text.string));
                    }

                    /* Weak */
                    else if(p_ptr->food < PY_FOOD_WEAK)
                    {
                         IH_AttrToColor(COLOR_ORANGE,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Weak",
                                   sizeof(object->text.string));
                    }

                    /* Hungry */
                    else if(p_ptr->food < PY_FOOD_ALERT)
                    {
                         IH_AttrToColor(COLOR_YELLOW,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Hungry",
                                   sizeof(object->text.string));
                    }

                    /* Normal */
                    else if(p_ptr->food < PY_FOOD_FULL)
                    {
                         shown = FALSE;
                    }

                    /* Full */
                    else if(p_ptr->food < PY_FOOD_MAX)
                    {
                         IH_AttrToColor(COLOR_L_GREEN,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Full",
                                   sizeof(object->text.string));
                    }

                    /* Gorged */
                    else
                    {
                         IH_AttrToColor(COLOR_GREEN,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Gorged",
                                   sizeof(object->text.string));
                    }

                    object->image.shown = shown;
               }
               break;

          case IH_SCENE_OBJECT_GRAZE:
               {
                    int             c = p_ptr->cut;

                    if(c)
                    {
                         object->image.shown = TRUE;
                         object->image.alpha =
                             (float) (CLAMP(c, 0, 1001) / 1001.0f);
                    }

                    object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;

                    if(c > 1000)
                    {
                         my_strcpy(object->text.string, "Mortal wound",
                                   sizeof(object->text.string));
                    }
                    else if(c > 200)
                    {
                         my_strcpy(object->text.string, "Deep gash",
                                   sizeof(object->text.string));
                    }
                    else if(c > 100)
                    {
                         my_strcpy(object->text.string, "Severe cut",
                                   sizeof(object->text.string));
                    }
                    else if(c > 50)
                    {
                         my_strcpy(object->text.string, "Nasty cut",
                                   sizeof(object->text.string));
                    }
                    else if(c > 25)
                    {
                         my_strcpy(object->text.string, "Bad cut",
                                   sizeof(object->text.string));
                    }
                    else if(c > 10)
                    {
                         my_strcpy(object->text.string, "Light cut",
                                   sizeof(object->text.string));
                    }
                    else if(c)
                    {
                         my_strcpy(object->text.string, "Graze",
                                   sizeof(object->text.string));
                    }
               }
               break;

          case IH_SCENE_OBJECT_BLIND:
               object->image.shown = !!p_ptr->blind;
               break;

          case IH_SCENE_OBJECT_CONFUSED:
               object->image.shown = !!p_ptr->confused;
               break;

          case IH_SCENE_OBJECT_AFRAID:
               object->image.shown = !!p_ptr->afraid;
               break;

          case IH_SCENE_OBJECT_POISONED:
               object->image.shown = !!p_ptr->poisoned;
               break;

          case IH_SCENE_OBJECT_STUN:
               {
                    int             s = p_ptr->stun;

                    object->image.tint = FALSE;
                    if(s)
                    {
                         object->image.shown = TRUE;
                         object->image.alpha =
                             (float) (CLAMP(s, 0, 101) / 101.0f);
                    }

                    if(s > 100)
                    {
                         IH_AttrToColor(COLOR_RED,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Knocked out",
                                   sizeof(object->text.string));
                    }
                    else if(s > 50)
                    {
                         IH_AttrToColor(COLOR_ORANGE,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Heavy stun",
                                   sizeof(object->text.string));
                    }
                    else if(s)
                    {
                         IH_AttrToColor(COLOR_ORANGE,
                                        &object->image.tint_color);
                         object->image.tint = TRUE;
                         my_strcpy(object->text.string, "Stun",
                                   sizeof(object->text.string));
                    }
               }
               break;
     }
}

void
IH_RenderIcons(void)
{
     int             i;

     for(i = 0; i < IH_MAX_ICONS; i++)
     {
          SceneObject    *object;

          object = icons[i];
          if(object)
          {
               SDL_Rect        rect;

               if(object->type != IH_SCENE_OBJECT_TYPE_ICON)
                    continue;

               /* Make sure the object's text is up-to-date.
                */
               IH_UpdateSceneObject(object);

               /* Render the icon image.
                */
               if(object->image.surface && object->image.shown)
               {
                    rect.x = object->x;
                    rect.y = object->y;

                    /* Handle alpha.
                     */
                    SDL_SetAlpha(object->image.surface, SDL_SRCALPHA,
                                 (Uint8) (object->image.alpha * 255));

                    /* Draw the icon.
                     */
                    SDL_BlitSurface(object->image.surface, NULL, ih.screen,
                                    &rect);

                    /* Handle tinting.
                     */
                    if(object->image.tint)
                    {
                         icon_create_tint(object->image.surface->w,
                                          object->image.surface->h,
                                          &object->image.tint_color);

                         if(tinting)
                              SDL_BlitSurface(tinting, NULL, ih.screen,
                                              &rect);
                    }
               }

               /* Render the text string.
                */
               if(object->text.shown)
                    IH_RenderSceneObjectText(object);
          }
     }
}

int
IH_GetIconSize(int size)
{
     int             size_val = 0;

     if(size == IH_ICON_SIZE_CURRENT)
          size = ih.icon_size;

     switch (size)
     {
          case IH_ICON_SIZE_SMALL:
               size_val = IH_ICON_SIZE_SMALL_VALUE;
               break;

          case IH_ICON_SIZE_MEDIUM:
               size_val = IH_ICON_SIZE_MEDIUM_VALUE;
               break;

          case IH_ICON_SIZE_LARGE:
               size_val = IH_ICON_SIZE_LARGE_VALUE;
               break;
     }

     return size_val;
}

SceneObject    *
IH_GetIconAtPosition(int x,
                     int y)
{
     int             i;

     /* Iterate over the list of graphic elements and find out which icon
      * the mouse is over.  Must account for layering.
      */
     for(i = 0; i < IH_MAX_ICONS; i++)
     {
          SceneObject    *object;

          object = icons[i];
          if(!object)
               continue;

          if((x >= object->x) &&
             (x < (object->x + ih.icon_size)) &&
             (y >= object->y) && (y < (object->y + ih.icon_size)))
               return object;
     }

     return NULL;
}

void
IH_CalcIconPosition(int icon,
                    int *x,
                    int *y)
{

}

#endif /* BUILD_3D_ENGINE */
