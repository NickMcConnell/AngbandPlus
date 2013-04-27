
/* $Id: init.c,v 1.4 2003/04/18 21:45:11 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

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
#include "engines/iso/pointer.h"

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

     fprintf(stderr, "IH_ISO_Setup()\n");

     /* Initialize SDL_draw.
      */
     fprintf(stderr, "IH_ISO_Setup(): init SDL_draw\n");
     Draw_Init();

     IH_SetLoadMessage(IH_TEXT_LOAD_FONTS);
     IH_SetStage(SCENE_TITLE_STAGE_ICONS);
     fprintf(stderr, "IH_ISO_Setup(): load fonts\n");
     rc = IH_ISO_LoadFonts();
     fprintf(stderr, "IH_ISO_Setup(): rc = %d\n", rc);
     if(rc)
          return rc;

     IH_SetLoadMessage(IH_TEXT_LOAD_MAP);
     IH_SetStage(SCENE_TITLE_STAGE_TILES);
     fprintf(stderr, "IH_ISO_Setup(): load map images\n");
     rc = IH_ISO_LoadMapImages();
     fprintf(stderr, "IH_ISO_Setup(): rc = %d\n", rc);
     if(rc)
          return rc;

     IH_SetLoadMessage(IH_TEXT_LOAD_POINTERS);
     IH_SetStage(SCENE_TITLE_STAGE_MISC);
     fprintf(stderr, "IH_ISO_Setup(): load pointers\n");
     rc = IH_ISO_LoadPointers();
     fprintf(stderr, "IH_ISO_Setup(): rc = %d\n", rc);
     if(rc)
          return rc;

     IH_SetLoadMessage(IH_TEXT_LOAD_ICONS);
     fprintf(stderr, "IH_ISO_Setup(): load icons\n");
     rc = IH_ISO_LoadIcons();
     fprintf(stderr, "IH_ISO_Setup(): rc = %d\n", rc);
#if 0                           // FIXME
     if(rc)
          return rc;
#endif

     IH_SetLoadMessage(IH_TEXT_LOAD_MISC);
     fprintf(stderr, "IH_ISO_Setup(): load miscellaneous\n");
     rc = IH_ISO_LoadMisc();
     fprintf(stderr, "IH_ISO_Setup(): rc = %d\n", rc);
#if 0                           // FIXME
     if(rc)
          return rc;
#endif

     fprintf(stderr, "IH_ISO_Setup(): set load complete message\n");
     IH_SetLoadMessage(IH_TEXT_LOAD_COMPLETE);
     IH_SetStage(SCENE_TITLE_STAGE_COMPLETE);

     fprintf(stderr, "IH_ISO_Setup(): return 0\n");
     return 0;
}

void
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

     fprintf(stderr, "IH_ISO_Init()\n");

     /* Setup init/cleanup callbacks */
     fprintf(stderr, "IH_ISO_Init(): setup init/cleanup callbacks\n");
     ih.display.setup_display_func = IH_ISO_SetupDisplay;
     ih.display.setup_func = IH_ISO_Setup;
     ih.display.cleanup_func = IH_ISO_Cleanup;

     /* Setup rendering callbacks (misc.c) */
     fprintf(stderr, "IH_ISO_Init(): setup misc rendering\n");
     ih.display.clear_buffer_func = IH_ISO_ClearDrawingBuffer;
     ih.display.swap_buffer_func = IH_ISO_SwapBuffers;
     ih.display.render_bg_func = IH_ISO_RenderBackground;
     ih.display.render_title_func = IH_ISO_RenderTitle;
     ih.display.render_splash_func = IH_ISO_RenderSplash;
     ih.display.shade_area_func = IH_ISO_ShadeArea;
     ih.display.frame_area_func = IH_ISO_FrameArea;
     ih.display.render_image_func = IH_ISO_RenderImage;
     ih.display.fill_area_func = IH_ISO_FillArea;
     ih.display.draw_line_func = IH_ISO_DrawLine;

     /* Setup rendering callbacks (text.c) */
     fprintf(stderr, "IH_ISO_Init(): setup text rendering\n");
     ih.display.render_text_func = IH_ISO_RenderText;
     ih.display.get_text_width_func = IH_ISO_GetTextWidth;
     ih.display.get_font_height_func = IH_ISO_GetFontHeight;

     /* Setup rendering callbacks (icon.c) */
     fprintf(stderr, "IH_ISO_Init(): setup icon rendering\n");
     ih.display.render_icon_func = IH_ISO_RenderIcon;

     /* Setup rendering callbacks (pointer.c) */
     fprintf(stderr, "IH_ISO_Init(): setup pointer rendering\n");
     ih.display.render_pointer_func = IH_ISO_RenderPointer;

     /* Setup rendering callbacks (map.c) */
     fprintf(stderr, "IH_ISO_Init(): setup map rendering\n");
     ih.display.draw_map_func = IH_ISO_DrawMap;

     /* Setup data pointer */
     fprintf(stderr, "IH_ISO_Init(): clear engine data\n");
     memset(&engine_data, 0, sizeof(engine_data));
     fprintf(stderr, "IH_ISO_Init(): set the engine data pointer\n");
     ((displayData *) ih.display.data)->engine_data =
         (void *) &engine_data;

     fprintf(stderr, "IH_ISO_Init(): return %d\n", rc);
     return rc;
}
