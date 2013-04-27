
/* $Id: setup.c,v 1.4 2003/03/17 22:45:38 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "ironhells.h"
#include "sdl/setup.h"

int
IH_CreateShader(int w,
                int h)
{
     SDL_Rect        irect;
     Uint32          rmask, gmask, bmask, amask;

     fprintf(stderr, "IH_CreateShader()\n");

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
     fprintf(stderr, "IH_CreateShader(): set masks\n");
     rmask = 0xff000000;
     gmask = 0x00ff0000;
     bmask = 0x0000ff00;
     amask = 0x000000ff;
#else
     fprintf(stderr, "IH_CreateShader(): set masks\n");
     rmask = 0x000000ff;
     gmask = 0x0000ff00;
     bmask = 0x00ff0000;
     amask = 0xff000000;
#endif
     fprintf(stderr, "IH_CreateShader(): check for existing shader\n");
     if(ih.shader)
     {
          fprintf(stderr, "IH_CreateShader(): free ih.shader\n");
          SDL_FreeSurface(ih.shader);
          ih.shader = NULL;
     }

     fprintf(stderr, "IH_CreateShader(): create surface\n");
     ih.shader = SDL_CreateRGBSurface(SDL_SWSURFACE | SDL_SRCALPHA,
                                      w, h,
                                      ih.display_depth,
                                      rmask, gmask, bmask, amask);

     fprintf(stderr, "IH_CreateShader(): double-check ih.shader\n");
     if(!ih.shader)
          return -1;

     fprintf(stderr, "IH_CreateShader(): setup the rectangle\n");
     irect.x = 0;
     irect.y = 0;
     irect.w = w;
     irect.h = h;

     /* Fill the square with black.
      */
     fprintf(stderr, "IH_CreateShader(): fill the rectangle\n");
     SDL_FillRect(ih.shader,
                  &irect,
                  SDL_MapRGBA(ih.shader->format, 0, 0, 0, IH_ALPHA_VALUE));

     /* Set the alpha on the shader surface.
      */
     fprintf(stderr, "IH_CreateShader(): set alpha\n");
     SDL_SetAlpha(ih.shader, SDL_SRCALPHA | SDL_RLEACCEL, 128);

     fprintf(stderr, "IH_CreateShader(): return\n");
     return 0;
}
