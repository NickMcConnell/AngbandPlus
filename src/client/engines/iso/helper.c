
/* $Id: helper.c,v 1.5 2003/04/18 21:45:11 cipher Exp $ */

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
#include <math.h>

/* Internal headers */
#include "ironhells.h"
#include "engines/iso/helper.h"

/*************************************
* SDL helper functions.
*/

/* The most pedantic-a%& getpixel and putpixel ever, hopefully. */

/* There may still be endianness bugs! These will be fixed after adequte testing. XXX XXX XXX */
inline          errr
SDL_GetPixel(SDL_Surface * f,
             Uint32 x,
             Uint32 y,
             Uint8 * r,
             Uint8 * g,
             Uint8 * b)
{
     /*const Uint32 mask[] = {0x0, 0xff, 0xffff, 0xffffff, 0xffffffff}; */
     Uint32          pixel;
     Uint8          *pp;
     int             n;         /* general purpose 'n'. */

     if(f == NULL)
          return -1;
     pp = (Uint8 *) f->pixels;
     if(x >= f->w || y >= f->h)
          return -1;
     pp += (f->pitch * y);
     pp += (x * f->format->BytesPerPixel);

     /* we do not lock the surface here, it would be inefficient XXX */
     /* this reads the pixel as though it was a big-endian integer XXX */
     /* I'm trying to avoid reading part the end of the pixel data by
      * using a data-type that's larger than the pixels */
     for(n = 0, pixel = 0; n < f->format->BytesPerPixel; ++n, ++pp)
     {
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
          pixel >>= 8;
          pixel |= *pp << (f->format->BitsPerPixel - 8);
#else
          pixel |= *pp;
          pixel <<= 8;
#endif
     }

     SDL_GetRGB(pixel, f->format, r, g, b);

     return 0;
}

/* This function looks remarkably similar to the one above. Yes, it's cut
* and paste. */
inline          errr
SDL_PutPixel(SDL_Surface * f,
             Uint32 x,
             Uint32 y,
             Uint8 r,
             Uint8 g,
             Uint8 b)
{
     Uint32          pixel;
     Uint8          *pp;
     int             n;

     if(f == NULL)
          return -1;

     pp = (Uint8 *) f->pixels;

     if(x >= f->w || y >= f->h)
          return -1;

     pp += (f->pitch * y);
     pp += (x * f->format->BytesPerPixel);

     pixel = SDL_MapRGB(f->format, r, g, b);

     for(n = 0; n < f->format->BytesPerPixel; ++n, ++pp)
     {
          *pp = (Uint8) (pixel & 0xFF);
          pixel >>= 8;
     }

     return 0;
}

/* This routine performs a scaling blit. It will shrink and magnify. :) */

/* It uses floating point arithmetic (because I am lazy) so it's not too fast
* but I only intend for it to be used in pre-processing, that is image
* processing at load time. It's fast enough for that, at least.
* Actually on my machine it IS fast enough to scale fonts and bitmaps
* in real-time. :)
* This routine uses a weighted average, the weight being based on overlapping
* pixel area.
*/
errr
SDL_ScaleBlit(SDL_Surface * src,
              SDL_Rect * sr,
              SDL_Surface * dst,
              SDL_Rect * dr)
{
     Uint8           r, g, b;
     float           rs, gs, bs; /* sums */
     float           area;
     float           sx, sy;    /* current source x and y */
     float           dsx, dsy;  /* source increment, per increment of 1 in destination */
     float           wsx, wsy;  /* width of source box. Equal to dsx,dsy except when either of then are
                                 * smaller than 1. This is a hack for smoother magnification. XXX */
     float           x, y;      /* x and y in sub-area */
     Uint32          tx, ty;    /* "to" x and y */
     Uint32          lx, ly;
     float           w, e, n, s; /* some temporary variables, named after orientations */

     if(src == NULL || sr == NULL || dst == NULL || dr == NULL)
          return -1;

     /* these are meaningless and would cause a divide by zero: */
     if(!dr->w || !dr->h)
          return -1;

     wsx = dsx = ((float) sr->w) / dr->w;
     if(wsx < 1)
          wsx = 1;
     wsy = dsy = ((float) sr->h) / dr->h;
     if(wsy < 1)
          wsy = 1;
     lx = dr->x + dr->w;
     ly = dr->y + dr->h;
     area = wsx * wsy;

     for(ty = dr->y, sy = (float) sr->y; ty < ly; ++ty, sy += dsy)
     {
          for(tx = dr->x, sx = (float) sr->x; tx < lx; ++tx, sx += dsx)
          {
               rs = gs = bs = 0.0;

               for(y = floor(sy) - 1; ceil(sy + wsy) > y; ++y)
               {
                    for(x = floor(sx) - 1; ceil(sx + wsx) > x; ++x)
                    {
                         w = (x > sx) ? 0 : sx - x;
                         n = (y > sy) ? 0 : sy - y;
                         e = (sx + wsx >= x + 1) ? 1 : sx + wsx - x;
                         s = (sy + wsy >= y + 1) ? 1 : sy + wsy - y;

                         if(w > e || s < n)
                              continue;

#define gsx ((Uint32)x >= sr->x+sr->w ? sr->x+sr->w-1 : (Uint32)x)
#define gsy ((Uint32)y >= sr->y+sr->h ? sr->y+sr->h-1 : (Uint32)y)

                         SDL_GetPixel(src, gsx, gsy, &r, &g, &b);

                         rs += (e - w) * (s - n) * (float) r;
                         gs += (e - w) * (s - n) * (float) g;
                         bs += (e - w) * (s - n) * (float) b;
                    }
               }

               rs /= area;
               gs /= area;
               bs /= area;

               if(rs >= 256.0 || gs >= 256.0 || bs > 256.0)
               {
#if 0
                    plog("weighted average error!");
                    plog(format("Values: %f, %f, %f\n", rs, gs, bs));

                    /**((char *)0) = 0;*/
#endif
               }

               if(rs > 255.0)
                    rs = 255.0;
               if(gs > 255.0)
                    gs = 255.0;
               if(bs > 255.0)
                    bs = 255.0;

               r = (Uint8) rs;
               g = (Uint8) gs;
               b = (Uint8) bs;

               SDL_PutPixel(dst, tx, ty, r, g, b);
          }
     }

     return 0;
#undef gsx
#undef gsy
}

/* Integer math version of SDL_ScaleBlit().
* Where necessary, a number uses the 16 high bits for the integer
 * and the 16 low bits for the decimal portion.
 *
 * eg:
 * float a = (float) (b >> 16) + (b & 0xFFFF)/65536.0;
 */
inline          Uint32
ifloor(Uint32 i)
{
     return i & 0xFFFF0000;
}

inline          Uint32
iceil(Uint32 i)
{
     return (i & 0xFFFF) ? i : ifloor(i) + (1 << 16);
}

errr
SDL_FastScaleBlit(SDL_Surface * src,
                  SDL_Rect * sr,
                  SDL_Surface * dst,
                  SDL_Rect * dr)
{
     Uint8           r, g, b;
     Uint32          rs, gs, bs; /* sums. */

     /* temp storage for large int multiplies. Uint64 doen't exist anywhere */
     double          farea;
     Uint32          area;
     Uint32          sx, sy;
     Uint32          dsx, dsy;
     Uint32          wsx, wsy;
     Uint32          x, y;      /* x and y, for sub-area */
     Uint32          tx, ty;    /* normal integers */
     Uint32          lx, ly;    /* normal integers */
     Uint32          w, e, n, s; /* temp variables, named after compass directions */

     if(src == NULL || sr == NULL || dst == NULL || dr == NULL)
          return -1;
     if(!dr->w || !dr->h)
          return -1;

     /* TODO FIXME check for possible overflows! */
     wsx = dsx = (sr->w << 16) / dr->w;
     if(!(wsx & 0xFFFF0000))
          wsx = 1 << 16;
     wsy = dsy = (sr->h << 16) / dr->h;
     if(!(wsy & 0xFFFF0000))
          wsy = 1 << 16;
     lx = dr->x + dr->w;
     ly = dr->y + dr->h;

     /* lazy multiplication. Hey, it's only once per blit. :P */
     farea = ((double) wsx) * ((double) wsy);
     farea /= (double) (1 << 16);
     area = (Uint32) farea;

     /* For optimization, those setup routines should be moved into
      * SDL_ScaleTiledBitmap() for that function.
      */
     for(ty = dr->y, sy = sr->y << 16; ty < ly; ++ty, sy += dsy)
     {
          for(tx = dr->x, sx = sr->x << 16; tx < lx; ++tx, sx += dsx)
          {
               rs = gs = bs = 0;

               for(y = ifloor(sy); iceil(sy + wsy) > y; y += (1 << 16))
               {
                    for(x = ifloor(sx);
                        iceil(sx + wsx) > x; x += (1 << 16))
                    {
                         w = (x > sx) ? 0 : sx - x;
                         n = (y > sy) ? 0 : sy - y;
                         e = (sx + wsx >=
                              x + (1 << 16)) ? 1 << 16 : sx + wsx - x;
                         s = (sy + wsy >=
                              y + (1 << 16)) ? 1 << 16 : sy + wsy - y;

                         if(w > e || s < n)
                              continue;

#define gsx ((x >> 16) >= sr->x+sr->w ? sr->x+sr->w-1 : x >> 16)
#define gsy ((y >> 16) >= sr->y+sr->h ? sr->y+sr->h-1 : y >> 16)

                         SDL_GetPixel(src, gsx, gsy, &r, &g, &b);

                         rs += ((e - w) >> 8) * ((s - n) >> 8) * r;
                         gs += ((e - w) >> 8) * ((s - n) >> 8) * g;
                         bs += ((e - w) >> 8) * ((s - n) >> 8) * b;
                    }
               }

               rs /= area;
               gs /= area;
               bs /= area;

               if(rs >= 256 || gs >= 256 || bs >= 256)
               {
#if 0
                    plog("fixed point weighted average overflow!");
                    plog(format("Values: %d, %d, %d\n", rs, gs, bs));
#endif
               }

               r = (Uint8) rs;
               g = (Uint8) gs;
               b = (Uint8) bs;

               SDL_PutPixel(dst, tx, ty, r, g, b);
          }
     }

     return 0;
#undef gsx
#undef gsy
}

#if 0                           /* the procedure above is a more generalized version of the one below */

/* The following is a function to perform a Blit while magnifying */

/* Anti-aliasing is performed. :) */

/* It is probably very SLOW on most systems. Use it for pre-processing. XXX */

/* a Blit while shrinking is handled by a different function */
errr
SDL_StretchBlit(SDL_Surface * src,
                SDL_Rect * sr,
                SDL_Surface * dst,
                SDL_Rect * dr)
{
     double          sx, sy;    /* current source x and y */
     Uint32          isx, isy;  /* temp. values for convenience in calculation code */
     double          dsx, dsy;  /* source increment, per increment of 1 in destination */
     double          wx, wy;    /* temp. weight values for the color mixing calculations */
     double          weight;    /* temp. weight of pixel */

     /* coordinates to get pixels from: ... */
#undef gsx
#define gsx (isx >= sr->x+sr->w ? sr->x+sr->w-1 : isx)
#undef gsy
#define gsy (isy >= sr->y+sr->h ? sr->y+sr->h-1 : isy)

     Uint32          tx, ty;    /* "to" x and y. "dx, dy" would be too confusing. */
     Uint32          lx, ly;    /* end x and y in destination, not inclusive */
     double          r, g, b;   /* temporary values on which we perform calculations */

     /*double s; *//* scale factor calculation thing. gross hack. don't ask. */
     Uint8           ir, ig, ib; /* same here. */

     if(src == NULL || sr == NULL || dst == NULL || dr == NULL)
          return -1;
     /* these are meaningless and would cause a divide by zero: */

     if(!dr->w || !dr->h)
          return -1;

     dsx = ((double) sr->w) / dr->w;
     dsy = ((double) sr->h) / dr->h;
     lx = dr->x + dr->w;
     ly = dr->y + dr->h;

     for(ty = dr->y, sy = (double) sr->y; ty < ly; ++ty, sy += dsy)
     {
          for(tx = dr->x, sx = (double) sr->x; tx < lx; ++tx, sx += dsx)
          {
               /* here we must consider four pixels and mix them together */
               /* the further away we are from a pixel, the less weight it has
                * when we mix in its color. Hence the "1 - hypot(..." etc.
                * Actually, no. Let's not use hypot().
                */
               /*
                * upper left pixel
                */
               wx = ((floor(sx) + 1) - sx);
               wy = ((floor(sy) + 1) - sy);
               isx = (Uint32) floor(sx);
               isy = (Uint32) floor(sy);

               if(SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib))
                    return -1;

               weight = wx * wy;
               /* the area of the overlap of our hypothetical and real pixel!!! */
               if(weight < 1 / 1024.0)
                    weight = 0;
               r = weight * (double) ir;
               g = weight * (double) ig;
               b = weight * (double) ib;
               /*s = weight * 255.0; */
               /*
                * upper right pixel
                */
               wx = 1 - wx;
               isx += 1;
               if(SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib))
                    return -1;
               weight = wx * wy;
               if(weight < 1 / 1024.0)
                    weight = 0;
               r += weight * (double) ir;
               g += weight * (double) ig;
               b += weight * (double) ib;
               /*s += weight * 255.0; */
               /*
                * lower right pixel
                */
               wy = 1 - wy;
               isy += 1;
               if(SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib))
                    return -1;
               weight = wx * wy;
               if(weight < 1 / 1024.0)
                    weight = 0;
               r += weight * (double) ir;
               g += weight * (double) ig;
               b += weight * (double) ib;
               /*s += weight * 255.0; */
               /*
                * lower left pixel
                */
               wx = 1 - wx;
               isx -= 1;
               if(SDL_GetPixel(src, gsx, gsy, &ir, &ig, &ib))
                    return -1;
               weight = wx * wy;
               if(weight < 1 / 1024.0)
                    weight = 0;
               r += weight * (double) ir;
               g += weight * (double) ig;
               b += weight * (double) ib;
               /*s += weight * 255.0; */
               /* 
                * r = 255 * (r/s);
                * g = 255 * (g/s);
                * b = 255 * (b/s);
                */
               if(r >= 256.0 || g >= 256.0 || b > 256.0)
               {
#if 0
                    plog("mixing error!");
                    plog(format
                         ("Values: %f, %f, %f\n", (double) r, (double) g,
                          (double) b));

                    /**((char *)0) = 0;*/
#endif
               }
               if(r > 255.0)
                    r = 255.0;
               if(g > 255.0)
                    g = 255.0;
               if(b > 255.0)
                    b = 255.0;
               ir = (Uint8) r;
               ig = (Uint8) g;
               ib = (Uint8) b;
               SDL_PutPixel(dst, tx, ty, ir, ig, ib);
          }
     }

     return 0;
}
#endif

/* This function will take an SDL_Surface, allocate a new surface to hold
* the resized surface, perform the scaling operation, free the old surface
 * and return the new one. This behaviour is vaguely modeled after C library
 * string functions. Returns NULL on grievous errors!
 *
 * The scaling operation is performed one or more times to accomodate
 * images comprised by a number of sub-images whose edges must not be blurred
 * with the edges of adjacent sub-images. (Think fonts and tile sets.)
 *
 * If t_oldw and t_oldh are set to src->w and src->h respectively
 *
 * t_oldw, t_oldh are the size of the old tiles
 */
SDL_Surface    *
SDL_ScaleTiledBitmap(SDL_Surface * src,
                     Uint32 t_oldw,
                     Uint32 t_oldh,
                     Uint32 t_neww,
                     Uint32 t_newh,
                     int dealloc_src)
{
     SDL_Surface    *dst;
     SDL_Rect        sr, dr;
     Uint32          x, y;
     Uint32          nx, ny;

     if(!t_oldw || !t_oldh || !t_neww || !t_newh || !src)
          return NULL;          /*dummy! */
     if(t_oldw == t_neww && t_oldh == t_newh)
          return src;           /* OK... */
     /* Get the number of tiles in the image.
      * Any possible clipped tiles at the edges are ignored.
      */
     nx = src->w / t_oldw;
     ny = src->h / t_oldh;
     /* Allocate a new SDL_Surface of appropriate size, with settings otherwise
      * identical to src.
      */
     dst = SDL_CreateRGBSurface(src->flags, nx * t_neww, ny * t_newh,
                                /*src->format->BitsPerPixel, */
                                16,
                                src->format->Rmask,
                                src->format->Gmask,
                                src->format->Bmask, src->format->Amask);

     for(y = 0; y < ny; ++y)
     {
          for(x = 0; x < nx; ++x)
          {
               sr.w = t_oldw;
               sr.h = t_oldh;
               sr.x = x * t_oldw;
               sr.y = y * t_oldh;
               dr.w = t_neww;
               dr.h = t_newh;
               dr.x = x * t_neww;
               dr.y = y * t_newh;
               /*printf("%d,%d -> %d,%d   (%d,%d -> %d, %d)\n", sr.x, sr.y, dr.x, dr.y, sr.w, sr.h, dr.w, dr.h); */
               /* scale-blit one tile and check for error
                * although SDl_ScaleBlit() might not have any errors to return.
                */
               if(SDL_FastScaleBlit(src, &sr, dst, &dr))
                    return NULL;
          }
     }
     if(dealloc_src)
          SDL_FreeSurface(src);
     return dst;
}

#endif /* BUILD_ISO_ENGINE */
