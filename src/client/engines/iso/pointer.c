
/* $Id: pointer.c,v 1.5 2003/04/18 03:32:56 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

#ifdef BUILD_ISO_ENGINE

/* Standard headers */
#include <stdio.h>

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "engines.h"
#include "platform/platform.h"
#include "render/pointer.h"
#include "render/icon.h"

/* Display engine headers */
#include "engines/iso/init.h"
#include "engines/iso/pointer.h"

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

errr
IH_ISO_LoadPointers(void)
{
     displayData    *display_data;
     isoEngineData  *engine_data;
     errr            rc = 0;
     char           *path_data;
     char           *path_misc;
     cptr            size;
     int             i;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     path_data = IH_GetDataDir("gfx");
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

          engine_data->pointers[pointers_init[i].array_pos] =
              IMG_Load_RW(SDL_RWFromFile(path_pointer, "rb"), 1);
          if(!engine_data->pointers[pointers_init[i].array_pos] && !i)
          {
               rc = IH_ERROR_CANT_LOAD_POINTER;
          }

          rnfree(path_pointer);
     }

     rnfree(path_misc);
     rnfree(path_data);

     return rc;
}

void
IH_ISO_FreePointers(void)
{
// FIXME
}

void
IH_ISO_RenderPointer(void)
{
     displayData    *display_data;
     isoEngineData  *engine_data;
     SDL_Surface    *pointer;
     SDL_Rect        rect;

     if(ih.pointer == IH_POINTER_NONE)
          return;

     if(!ih.display.screen)
          return;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     rect.x = ih.display.mouse_x;
     rect.y = ih.display.mouse_y;
     pointer = engine_data->pointers[ih.pointer];
     if(!pointer)
          pointer = engine_data->pointers[IH_POINTER_STANDARD];

     if(!pointer)
          return;

     SDL_BlitSurface(pointer, NULL, ih.display.screen, &rect);
}

#endif /* BUILD_ISO_ENGINE */
