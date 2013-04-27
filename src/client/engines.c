
/* $Id: engines.c,v 1.4 2003/04/18 21:45:04 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "engines.h"

/* Display Engine headers */
#ifdef BUILD_ISO_ENGINE
#include "engines/iso/init.h"
#endif
#ifdef BUILD_3D_ENGINE
#include "engines/3d/init.h"
#endif

static displayData display_data;

static          errr
IH_Engines_Load(void)
{
     errr            rc = 0;

     switch (ih.display.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
#ifdef DYNAMICALLY_LOADED_ENGINES
#endif
               break;
#endif

#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
#ifdef DYNAMICALLY_LOADED_ENGINES
#endif
               break;
#endif
     }

     return rc;
}

static void
IH_Engines_Unload(void)
{
     switch (ih.display.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
#ifdef DYNAMICALLY_LOADED_ENGINES
#endif
               break;
#endif

#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
#ifdef DYNAMICALLY_LOADED_ENGINES
#endif
               break;
#endif
     }
}

errr
IH_Engines_Init(void)
{
     errr            rc = 0;

     fprintf(stderr, "IH_Engines_Init()\n");

     /* Clear the display data structure.
      */
     fprintf(stderr, "IH_Engines_Init(): clear data structure\n");
     memset(&display_data, 0, sizeof(display_data));

     ih.display.data = (void *) &display_data;

     /* Make sure the display engine is loaded, if necessary.
      */
     fprintf(stderr, "IH_Engines_Init(): load display engines\n");
     IH_Engines_Load();

     fprintf(stderr, "IH_Engines_Init(): initialize the proper one\n");
     switch (ih.display.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
               fprintf(stderr, "IH_Engines_Init(): use the ISO engine\n");
               rc = IH_ISO_Init();
               fprintf(stderr, "IH_Engines_Init(): init returned %d\n",
                       rc);
               break;
#endif

#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
               rc = IH_3D_Init();
               break;
#endif
     }

     fprintf(stderr, "IH_Engines_Init(): return %d\n", rc);
     return rc;
}

void
IH_Engines_Cleanup(void)
{
     switch (ih.display.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
               IH_ISO_Cleanup();
               break;
#endif

#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
               IH_3D_Cleanup();
               break;
#endif
     }

     IH_Engines_Unload();
}
