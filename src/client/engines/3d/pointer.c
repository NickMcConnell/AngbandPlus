
/* $Id: pointer.c,v 1.3 2003/04/16 17:30:23 cipher Exp $ */

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

/* Standard headers */
#include <stdio.h>

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"

/* Internal headers */
#include "ironhells.h"
#include "displays/iso/pointer.h"
#include "displays/iso/render.h"
#include "platform/platform.h"
#include "path.h"
#include "displays/iso/icon.h"

struct ihPointerInit
{
     const char     *name;
     int             array_pos;
};

static struct ihPointerInit pointers_init[] = {
     {"standard", IH_POINTER_STANDARD},
     {"forbid", IH_POINTER_FORBID},
     {"attack", IH_POINTER_ATTACK},
     {"dig", IH_POINTER_DIG},
     {"target", IH_POINTER_TARGET},
     {"open", IH_POINTER_OPEN},
     {"close", IH_POINTER_CLOSE},
     {"disarm", IH_POINTER_DISARM},
     {"spell", IH_POINTER_SPELL},
     {NULL}
};

static SDL_Surface *pointers[IH_POINTER_MAX];

errr
IH_LoadPointers(void)
{
     errr            rc = 0;
     char           *path_data;
     char           *path_misc;
     cptr            size;
     int             i;

#ifdef DEBUG
     fprintf(stderr, "IH_LoadPointers()\n");
#endif

     path_data = IH_GetDataDir("gfx");
#ifdef DEBUG
     fprintf(stderr, "path_data = %s\n", path_data);
#endif
     switch (ih.icon_size)
     {
          case IH_ICON_SIZE_LARGE:
               size = "large";
               break;

          case IH_ICON_SIZE_MEDIUM:
               size = "medium";
               break;

          case IH_ICON_SIZE_SMALL:
          default:
               size = "small";
               break;
     }
     path_misc = IH_PathBuild(path_data, "misc", size, NULL);
#ifdef DEBUG
     fprintf(stderr, "path_misc = %s\n", path_misc);
#endif

     for(i = 0; pointers_init[i].name; i++)
     {
          char            pointer_name[60];
          char           *path_pointer;

          my_strcpy(pointer_name, "pointer-", sizeof(pointer_name) - 1);
          my_strcat(pointer_name, pointers_init[i].name,
                    sizeof(pointer_name) - 1);
          my_strcat(pointer_name, "." IH_IMAGE_FORMAT_EXT,
                    sizeof(pointer_name) - 1);
          fprintf(stderr, "pointer_name = %s\n", pointer_name);

          path_pointer = IH_PathBuild(path_misc, pointer_name, NULL);
#ifdef DEBUG
          fprintf(stderr, "path_pointer = %s\n", path_pointer);
#endif

          pointers[pointers_init[i].array_pos] =
              IMG_Load_RW(SDL_RWFromFile(path_pointer, "rb"), 1);
          if(!pointers[pointers_init[i].array_pos])
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load pointer image: %s: %s\n",
                       path_pointer, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_POINTER;
          }

          rnfree(path_pointer);
     }

     rnfree(path_misc);
     rnfree(path_data);

     return rc;
}

void
IH_RenderPointer(void)
{
     SDL_Surface    *pointer;
     SDL_Rect        rect;

#ifdef DEBUG
     fprintf(stderr, "IH_RenderPointer()\n");
#endif

     if(ih.pointer == IH_POINTER_NONE)
          return;

     if(!ih.screen)
          return;

#ifdef DEBUG
     fprintf(stderr, "mouse x = %d, mouse y = %d\n", ih.mouse_x,
             ih.mouse_y);
#endif
     rect.x = ih.mouse_x;
     rect.y = ih.mouse_y;
     pointer = pointers[ih.pointer];
     if(!pointer)
          pointer = pointers[IH_POINTER_STANDARD];

     if(!pointer)
          return;

#ifdef DEBUG
     fprintf(stderr, "Blitting pointer to screen.\n");
#endif
     SDL_BlitSurface(pointer, NULL, ih.screen, &rect);
}

#endif /* BUILD_3D_ENGINE */
