
/* $Id: helper.h,v 1.2 2003/04/06 15:22:11 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_HELPER_H
#define IH_DISPLAYS_ISO_HELPER_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

inline errr     SDL_GetPixel(SDL_Surface * f,
                             Uint32 x,
                             Uint32 y,
                             Uint8 * r,
                             Uint8 * g,
                             Uint8 * b);
inline errr     SDL_PutPixel(SDL_Surface * f,
                             Uint32 x,
                             Uint32 y,
                             Uint8 r,
                             Uint8 g,
                             Uint8 b);
errr            SDL_ScaleBlit(SDL_Surface * src,
                              SDL_Rect * sr,
                              SDL_Surface * dst,
                              SDL_Rect * dr);

errr            SDL_FastScaleBlit(SDL_Surface * src,
                                  SDL_Rect * sr,
                                  SDL_Surface * dst,
                                  SDL_Rect * dr);
SDL_Surface    *SDL_ScaleTiledBitmap(SDL_Surface * src,
                                     Uint32 t_oldw,
                                     Uint32 t_oldh,
                                     Uint32 t_neww,
                                     Uint32 t_newh,
                                     int dealloc_src);
inline Uint32   ifloor(Uint32 i);
inline Uint32   iceil(Uint32 i);

#endif /* IH_DISPLAYS_ISO_HELPER_H */
