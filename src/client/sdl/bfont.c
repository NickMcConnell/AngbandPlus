
/************************************************************/

/*                                                          */

/*   BFONT.c v. 1.0.4-1 - Billi Font Library by Diego Billi */

/*                                                          */

/*   mail: dbilli@cs.unibo.it                               */

/*   home: http://www.cs.unibo.it/~dbilli (ITALIAN)         */

/*                                                          */

/************************************************************/

#ifdef HAVE_CONFIG_H
#include "autoconf.h"
#endif

#ifndef USE_SDLTTF

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "SDL_image.h"

#include "angband.h"
#include "bfont.h"

// ATTENTION: MS Visual C++ do not declarate vsnprintf in <stdio.h> 
#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

/* Current font */
static BFont_Info *CurrentFont;

/* buffer size  for buffered prints*/
#define BFONT_BUFFER_LEN 1024

/* Single global var for buffered prints */
static char     bfont_buffer[BFONT_BUFFER_LEN];

/* utility functions */
static Uint32   GetPixel(SDL_Surface * Surface,
                         Sint32 X,
                         Sint32 Y);
static void     PutPixel(SDL_Surface * surface,
                         int x,
                         int y,
                         Uint32 pixel);

int
InitFont(BFont_Info * Font)
{
     int             x = 0, i = 0;
     Uint32          sentry;

     i = '!';

     sentry = GetPixel(Font->Surface, 0, 0);
     /* sentry = SDL_MapRGB(Font->Surface->format, 255, 0, 255); */

     if(Font->Surface == NULL)
     {
          fprintf(stderr, "BFont: The font has not been loaded!\n");
          return 1;
     }
     if(SDL_MUSTLOCK(Font->Surface))
          SDL_LockSurface(Font->Surface);
     x = 0;
     while(x < (Font->Surface->w - 1))
     {
          if(GetPixel(Font->Surface, x, 0) != sentry)
          {
               Font->Chars[i].x = x;
               Font->Chars[i].y = 1;
               Font->Chars[i].h = Font->Surface->h;
               for(;
                   GetPixel(Font->Surface, x, 0) != sentry &&
                   x < (Font->Surface->w); ++x) ;
               Font->Chars[i].w = (x - Font->Chars[i].x);
               i++;
          }
          else
          {
               x++;
          }
     }
     Font->Chars[' '].x = 0;
     Font->Chars[' '].y = 0;
     Font->Chars[' '].h = Font->Surface->h;
     Font->Chars[' '].w = Font->Chars['!'].w;

     if(SDL_MUSTLOCK(Font->Surface))
          SDL_UnlockSurface(Font->Surface);

     Font->h = Font->Surface->h;

     SDL_SetColorKey(Font->Surface, SDL_SRCCOLORKEY,
                     GetPixel(Font->Surface, 0, Font->Surface->h - 1));

     return 0;
}

/* Load the font and stores it in the BFont_Info structure */
BFont_Info     *
LoadFont(const char *filename)
{
     SDL_Surface    *surface = NULL;
     int             x;
     BFont_Info     *Font = NULL;

     if(filename != NULL)
     {
          Font = (BFont_Info *) malloc(sizeof(BFont_Info));

          if(Font != NULL)
          {

               surface = (SDL_Surface *) IMG_Load(filename);
               if(surface != NULL)
               {
                    Font->Surface = surface;
                    for(x = 0; x < 256; x++)
                    {
                         Font->Chars[x].x = 0;
                         Font->Chars[x].y = 0;
                         Font->Chars[x].h = 0;
                         Font->Chars[x].w = 0;
                    }
                    /* Init the font */
                    InitFont(Font);
                    /* Set the font as the current font */
                    SetCurrentFont(Font);
               }
               else
               {
                    /* free memory allocated for the BFont_Info structure */
                    free(Font);
                    Font = NULL;
               }
          }
     }

     return Font;
}

BFont_Info     *
LoadFontFromSurface(SDL_Surface * Surface)
{
     int             x;
     BFont_Info     *Font = NULL;

     if(Surface != NULL)
     {
          Font = (BFont_Info *) malloc(sizeof(BFont_Info));

          if(Font != NULL)
          {
               Font->Surface = Surface;
               for(x = 0; x < 256; x++)
               {
                    Font->Chars[x].x = 0;
                    Font->Chars[x].y = 0;
                    Font->Chars[x].h = 0;
                    Font->Chars[x].w = 0;
               }

               /* Init the font */
               InitFont(Font);
               /* Set the font as the current font */
               SetCurrentFont(Font);
          }
     }

     return Font;
}

void
FreeFont(BFont_Info * Font)
{
     fprintf(stderr, "FreeFont()\n");
     if(!Font)
          return;

     fprintf(stderr, "Freeing font surface.\n");
     if(Font->Surface)
          SDL_FreeSurface(Font->Surface);

     fprintf(stderr, "Freeing font structure.\n");
     free(Font);
}

BFont_Info     *
SetFontColor(BFont_Info * Font,
             Uint8 r,
             Uint8 g,
             Uint8 b)
{
     int             x, y;
     BFont_Info     *newfont;
     SDL_Surface    *surface = NULL;
     Uint32          pixel;
     Uint8           old_r, old_g, old_b;
     Uint8           new_r, new_g, new_b;
     Uint32          color_key;

     newfont = (BFont_Info *) malloc(sizeof(BFont_Info));
     memset(newfont, 0, sizeof(BFont_Info));
     if(newfont != NULL)
     {

          newfont->h = Font->h;

          for(x = 0; x < 256; x++)
          {
               newfont->Chars[x].x = Font->Chars[x].x;
               newfont->Chars[x].y = Font->Chars[x].y;
               newfont->Chars[x].h = Font->Chars[x].h;
               newfont->Chars[x].w = Font->Chars[x].w;
          }

          surface =
              SDL_ConvertSurface(Font->Surface, Font->Surface->format,
                                 Font->Surface->flags);
          if(surface != NULL)
          {

               if(SDL_MUSTLOCK(surface))
                    SDL_LockSurface(surface);
               if(SDL_MUSTLOCK(Font->Surface))
                    SDL_LockSurface(Font->Surface);

               color_key = GetPixel(surface, 0, surface->h - 1);

               printf("looking...\n");
               for(x = 0; x < Font->Surface->w; x++)
               {
                    for(y = 0; y < Font->Surface->h; y++)
                    {
                         old_r = old_g = old_b = 0;
                         pixel = GetPixel(Font->Surface, x, y);

                         if(pixel != color_key)
                         {
                              SDL_GetRGB(pixel, surface->format, &old_r,
                                         &old_g, &old_b);

                              new_r = (Uint8) ((old_r * r) / 255);
                              new_g = (Uint8) ((old_g * g) / 255);
                              new_b = (Uint8) ((old_b * b) / 255);

                              pixel =
                                  SDL_MapRGB(surface->format, new_r, new_g,
                                             new_b);
                              PutPixel(surface, x, y, pixel);
                         }
                    }
               }
               printf("unlooking...\n");
               if(SDL_MUSTLOCK(surface))
                    SDL_UnlockSurface(surface);
               if(SDL_MUSTLOCK(Font->Surface))
                    SDL_UnlockSurface(Font->Surface);

               SDL_SetColorKey(surface, SDL_SRCCOLORKEY, color_key);
          }

          newfont->Surface = surface;
     }

     return newfont;
}

/* Set the current font */
void
SetCurrentFont(BFont_Info * Font)
{
     CurrentFont = Font;
}

/* Returns the pointer to the current font strucure in use */
BFont_Info     *
GetCurrentFont(void)
{
     return CurrentFont;
}

/* Return the font height */
int
FontHeight(BFont_Info * Font)
{
     return (Font->h);
}

void
SetFontHeight(BFont_Info * Font,
              int height)
{
     Font->h = height;
}

/* Return the width of the "c" character */
int
CharWidth(BFont_Info * Font,
          int c)
{
     return Font->Chars[c].w;
}

/* Puts a single char on the surface */
int
PutChar(SDL_Surface * Surface,
        int x,
        int y,
        int c)
{
     return PutCharFont(Surface, CurrentFont, x, y, c);
}

/* Puts a single char on the surface with the specified font */
int
PutCharFont(SDL_Surface * Surface,
            BFont_Info * Font,
            int x,
            int y,
            int c)
{
     int             r = 0;
     SDL_Rect        dest;

     dest.w = CharWidth(Font, ' ');
     dest.h = FontHeight(Font);
     dest.x = x;
     dest.y = y;
     if(c != ' ')
     {
          SDL_BlitSurface(Font->Surface, &Font->Chars[c], Surface, &dest);
     }
     r = dest.w;

     return r;
}

void
PutString(SDL_Surface * Surface,
          int x,
          int y,
          const char *text)
{
     PutStringFont(Surface, CurrentFont, x, y, text);
}

void
PutStringFont(SDL_Surface * Surface,
              BFont_Info * Font,
              int x,
              int y,
              const char *text)
{
     int             i = 0;

     while(text[i] != '\0')
     {
          x += PutCharFont(Surface, Font, x, y, text[i]);
          i++;
     }
}

int
TextWidth(const char *text)
{
     return TextWidthFont(CurrentFont, text);
}

int
TextWidthFont(BFont_Info * Font,
              const char *text)
{
     int             i = 0, x = 0;

     while(text[i] != '\0')
     {
          x += CharWidth(Font, text[i]);
          i++;
     }

     return x;
}

/* counts the spaces of the strings */
static int
count(const char *text)
{
     char           *p = NULL;
     int             pos = -1;
     int             i = 0;

     /* Calculate the space occupied by the text without spaces */
     while((p = strchr(&text[pos + 1], ' ')) != NULL)
     {
          i++;
          pos = p - text;
     }

     return i;
}

void
JustifiedPutString(SDL_Surface * Surface,
                   int y,
                   const char *text)
{
     JustifiedPutStringFont(Surface, CurrentFont, y, text);
}

void
JustifiedPutStringFont(SDL_Surface * Surface,
                       BFont_Info * Font,
                       int y,
                       const char *text)
{
     int             spaces = 0;
     int             gap;
     int             single_gap;
     int             dif;
     char           *strtmp;
     char           *p;
     int             pos = -1;
     int             xpos = 0;

     if(strchr(text, ' ') == NULL)
     {
          PutStringFont(Surface, Font, 0, y, text);
     }
     else
     {
          gap = (Surface->w - 1) - TextWidthFont(Font, text);

          if(gap <= 0)
          {
               PutStringFont(Surface, Font, 0, y, text);
          }
          else
          {
               spaces = count(text);
               dif = gap % spaces;
               single_gap = (gap - dif) / spaces;
               xpos = 0;
               pos = -1;
               while(spaces > 0)
               {
                    p = strstr(&text[pos + 1], " ");
                    strtmp = NULL;
                    strtmp =
                        (char *) calloc((p - &text[pos + 1]) + 1,
                                        sizeof(char));
                    if(strtmp != NULL)
                    {
                         strncpy(strtmp, &text[pos + 1],
                                 (p - &text[pos + 1]));
                         PutStringFont(Surface, Font, xpos, y, strtmp);
                         xpos =
                             xpos + TextWidthFont(Font,
                                                  strtmp) + single_gap +
                             CharWidth(Font, ' ');
                         if(dif >= 0)
                         {
                              xpos++;
                              dif--;
                         }
                         pos = p - text;
                         spaces--;
                         free(strtmp);
                    }
               }
               strtmp = NULL;
               strtmp =
                   (char *) calloc(strlen(&text[pos + 1]) + 1,
                                   sizeof(char));

               if(strtmp != NULL)
               {
                    strncpy(strtmp, &text[pos + 1],
                            strlen(&text[pos + 1]));
                    PutStringFont(Surface, Font, xpos, y, strtmp);
                    free(strtmp);
               }
          }
     }
}

void
CenteredPutString(SDL_Surface * Surface,
                  int y,
                  const char *text)
{
     CenteredPutStringFont(Surface, CurrentFont, y, text);
}

void
CenteredPutStringFont(SDL_Surface * Surface,
                      BFont_Info * Font,
                      int y,
                      const char *text)
{
     PutStringFont(Surface, Font,
                   Surface->w / 2 - TextWidthFont(Font, text) / 2, y,
                   text);
}

void
RightPutString(SDL_Surface * Surface,
               int y,
               const char *text)
{
     RightPutStringFont(Surface, CurrentFont, y, text);
}

void
RightPutStringFont(SDL_Surface * Surface,
                   BFont_Info * Font,
                   int y,
                   const char *text)
{
     PutStringFont(Surface, Font,
                   Surface->w - TextWidthFont(Font, text) - 1, y, text);
}

void
LeftPutString(SDL_Surface * Surface,
              int y,
              const char *text)
{
     LeftPutStringFont(Surface, CurrentFont, y, text);
}

void
LeftPutStringFont(SDL_Surface * Surface,
                  BFont_Info * Font,
                  int y,
                  const char *text)
{
     PutStringFont(Surface, Font, 0, y, text);
}

/******/

void
PrintString(SDL_Surface * Surface,
            int x,
            int y,
            const char *fmt,
            ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     PutStringFont(Surface, CurrentFont, x, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);

//        PutStringFont(Surface, CurrentFont, x, y, temp);

//        free (temp);
//    }
//    va_end(args);
}

void
PrintStringFont(SDL_Surface * Surface,
                BFont_Info * Font,
                int x,
                int y,
                const char *fmt,
                ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     PutStringFont(Surface, Font, x, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        PutStringFont(Surface, Font, x, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
CenteredPrintString(SDL_Surface * Surface,
                    int y,
                    const char *fmt,
                    ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     CenteredPutString(Surface, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        CenteredPutString(Surface, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
CenteredPrintStringFont(SDL_Surface * Surface,
                        BFont_Info * Font,
                        int y,
                        const char *fmt,
                        ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     CenteredPutStringFont(Surface, Font, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        CenteredPutStringFont(Surface, Font, y, temp);
//        free (temp);
//    }
//    va_end(args);

}

void
RightPrintString(SDL_Surface * Surface,
                 int y,
                 const char *fmt,
                 ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     RightPutString(Surface, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        RightPutString(Surface, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
RightPrintStringFont(SDL_Surface * Surface,
                     BFont_Info * Font,
                     int y,
                     const char *fmt,
                     ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     RightPutStringFont(Surface, Font, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        RightPutStringFont(Surface, Font, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
LeftPrintString(SDL_Surface * Surface,
                int y,
                const char *fmt,
                ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     LeftPutString(Surface, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        LeftPutString(Surface, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
LeftPrintStringFont(SDL_Surface * Surface,
                    BFont_Info * Font,
                    int y,
                    const char *fmt,
                    ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     LeftPutStringFont(Surface, Font, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        LeftPutStringFont(Surface, Font, y, temp);
//        free (temp);
//    }
//    va_end(args);
}

void
JustifiedPrintString(SDL_Surface * Surface,
                     int y,
                     const char *fmt,
                     ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     JustifiedPutString(Surface, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        JustifiedPutString( Surface,  y,temp);
//        free (temp);
//    }
//    va_end(args);
}

void
JustifiedPrintStringFont(SDL_Surface * Surface,
                         BFont_Info * Font,
                         int y,
                         const char *fmt,
                         ...)
{
     va_list         args;

     va_start(args, fmt);
     vsnprintf(bfont_buffer, BFONT_BUFFER_LEN, fmt, args);
     va_end(args);

     bfont_buffer[BFONT_BUFFER_LEN - 1] = '\0';
     JustifiedPutStringFont(Surface, Font, y, bfont_buffer);

//    va_list args;
//    char *temp;
//    va_start (args,fmt);

//    if ( (temp = (char *) malloc(1000+1)) != NULL) {
//        vsprintf(temp,fmt,args);
//        JustifiedPutStringFont( Surface, Font, y,temp);
//        free (temp);
//    }
//    va_end(args);
}

/*********************************************************************************************************/

/*********************************************************************************************************/

/*********************************************************************************************************/

void
PutPixel(SDL_Surface * surface,
         int x,
         int y,
         Uint32 pixel)
{
     int             bpp = surface->format->BytesPerPixel;

     /* Here p is the address to the pixel we want to set */
     Uint8          *p =
         (Uint8 *) surface->pixels + y * surface->pitch + x * bpp;

     switch (bpp)
     {
          case 1:
               *p = pixel;
               break;

          case 2:
               *(Uint16 *) p = pixel;
               break;

          case 3:
               if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
               {
                    p[0] = (pixel >> 16) & 0xff;
                    p[1] = (pixel >> 8) & 0xff;
                    p[2] = pixel & 0xff;
               }
               else
               {
                    p[0] = pixel & 0xff;
                    p[1] = (pixel >> 8) & 0xff;
                    p[2] = (pixel >> 16) & 0xff;
               }
               break;

          case 4:
               *(Uint32 *) p = pixel;
               break;
     }
}

Uint32
GetPixel(SDL_Surface * Surface,
         Sint32 X,
         Sint32 Y)
{

     Uint8          *bits;
     Uint32          Bpp;

     if(X < 0)
          puts("x too small in GetPixel!");
     if(X >= Surface->w)
          puts("x too big in GetPixel!");

     Bpp = Surface->format->BytesPerPixel;

     bits = ((Uint8 *) Surface->pixels) + Y * Surface->pitch + X * Bpp;

     // Get the pixel
     switch (Bpp)
     {
          case 1:
               return *((Uint8 *) Surface->pixels + Y * Surface->pitch +
                        X);
               break;
          case 2:
               return *((Uint16 *) Surface->pixels +
                        Y * Surface->pitch / 2 + X);
               break;
          case 3:
               {                // Format/endian independent
                    Uint8           r, g, b;

                    r = *((bits) + Surface->format->Rshift / 8);
                    g = *((bits) + Surface->format->Gshift / 8);
                    b = *((bits) + Surface->format->Bshift / 8);
                    return SDL_MapRGB(Surface->format, r, g, b);
               }
               break;
          case 4:
               return *((Uint32 *) Surface->pixels +
                        Y * Surface->pitch / 4 + X);
               break;
     }

     return -1;
}

#endif /* USE_SDLTTF */
