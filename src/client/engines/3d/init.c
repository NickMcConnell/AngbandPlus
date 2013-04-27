
/* $Id: init.c,v 1.2 2003/04/16 17:30:19 cipher Exp $ */

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
#include "SDL_draw.h"

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "strings.h"
#include "engines.h"
#include "engines/iso/init.h"
#include "engines/iso/text.h"
#include "engines/iso/map.h"
#include "engines/iso/icon.h"
#include "engines/iso/misc.h"

static isoEngineData engine_data;

static          errr
IH_ISO_SetupDisplay(void)
{
     errr            rc = 0;

     ih.display.flags = SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.display.is_fullscreen)
          ih.display.flags |= SDL_FULLSCREEN;
     ih.display.flags |=
         ih.display.is_hw_accel ? SDL_HWSURFACE : SDL_SWSURFACE;

     return rc;
}

static          errr
IH_ISO_Setup(void)
{
     errr            rc = 0;

     /* Initialize SDL_draw.
      */
     Draw_Init();

     ih.load_message = IH_TEXT_LOAD_FONTS;
     rc = IH_ISO_LoadFonts();
     if(rc)
          return rc;

     ih.load_message = IH_TEXT_LOAD_MAP;
     rc = IH_ISO_LoadMapImages();
     if(rc)
          return rc;

     ih.load_message = IH_TEXT_LOAD_POINTERS;
     rc = IH_ISO_LoadPointers();
     if(rc)
          return rc;

     ih.load_message = IH_TEXT_LOAD_ICONS;
     rc = IH_ISO_LoadIcons();
     if(rc)
          return rc;

     ih.load_message = IH_TEXT_LOAD_MISC;
     rc = IH_ISO_LoadMisc();
     if(rc)
          return rc;

     ih.load_message = IH_TEXT_LOAD_COMPLETE;

     return 0;
}

static void
IH_ISO_Cleanup(void)
{
     IH_ISO_FreeMisc();

     IH_ISO_FreeIcons();

     IH_ISO_FreePointers();

     IH_ISO_FreeMapImages();

     IH_ISO_FreeFonts();
}

errr
IH_ISO_Init(void)
{
     errr            rc = 0;

     /* Setup init/cleanup callbacks */
     ih.engine.setup_display = IH_ISO_SetupDisplay;
     ih.engine.setup_func = IH_ISO_Setup;
     ih.engine.cleanup_func = IH_ISO_Cleanup;

     /* Setup rendering callbacks (misc.c) */
     ih.engine.clear_buffer_func = IH_ISO_ClearDrawingBuffer;
     ih.engine.swap_buffer_func = IH_ISO_SwapBuffers;
     ih.engine.render_bg_func = IH_ISO_RenderBackground;
     ih.engine.shade_area_func = IH_ISO_ShadeArea;
     ih.engine.frame_area_func = IH_ISO_FrameArea;

     /* Setup rendering callbacks (text.c) */
     ih.engine.render_text_func = IH_ISO_RenderText;
     ih.engine.get_text_width_func = IH_ISO_GetTextWidth;
     ih.engine.get_font_height_func = IH_ISO_GetFontHeight;

     /* Setup rendering callbacks (icon.c) */
     ih.engine.render_icon_func = IH_ISO_RenderIcon;

     /* Setup rendering callbacks (pointer.c) */
     ih.engine.render_pointer_func = IH_ISO_RenderPointer;

     /* Setup rendering callbacks (map.c) */
     ih.engine.draw_map_func = IH_ISO_DrawMap;

     /* Setup data pointer */
     memset(engine_data, 0, sizeof(engine_data));
     ((displayData *) ih.display.data).engine_data = (void *) &engine_data;

     return rc;
}

#endif /* BUILD_3D_ENGINE */
