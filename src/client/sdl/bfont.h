#ifndef IH_BFONT_H
#define IH_BFONT_H


/************************************************************/ 

/*                                                          */ 

/*   BFONT.h v. 1.0.4-1 - Billi Font Library by Diego Billi */ 

/*                                                          */ 

/*   mail: dbilli@cs.unibo.it                               */ 

/*   home: http://www.cs.unibo.it/~dbilli (ITALIAN)         */ 

/*                                                          */ 

/************************************************************/ 
    
#include "SDL.h"
    
#ifdef __cplusplus
extern          "C"
{
     
#endif                          /*  */
     typedef struct
     {
          
              /* font height */ 
          int             h;
                         
              /* font surface */ 
                          SDL_Surface * Surface;
                         
              /* characters width */ 
                          SDL_Rect Chars[256];
                    }
     BFont_Info;
                 

/* BFont v. 1.0.2 */ 
                   

/* Load and store the font in the BFont_Info structure */ 
                     BFont_Info * LoadFont(const char *filename);
                   

/* Free memory */ 
     void            FreeFont(BFont_Info * Font);
                   

/* Returns a pointer to the current font structure */ 
                     BFont_Info * GetCurrentFont(void);
                   

/* Set the current font */ 
     void            SetCurrentFont(BFont_Info * Font);
                   

/* Returns the font height */ 
     int             FontHeight(BFont_Info * Font);
                   

/* Change the font height */ 
     void            SetFontHeight(BFont_Info * Font,
                                   int height);
                   

/* Returns the character width of the specified font */ 
     int             CharWidth(BFont_Info * Font,
                               int c);
                   

/* Write a single character on the "Surface" with the current font */ 
     int             PutChar(SDL_Surface * Surface,
                             int x,
                             int y,
                             int c);
                   

/* Write a single character on the "Surface" with the specified font */ 
     int             PutCharFont(SDL_Surface * Surface,
                                 BFont_Info * Font,
                                 int x,
                                 int y,
                                 int c);
                   

/* Returns the width, in pixels, of the text calculated with the current font*/ 
     int             TextWidth(const char *text);
                   

/* Returns the width, in pixels, of the text calculated with the specified font*/ 
     int             TextWidthFont(BFont_Info * Font,
                                   const char *text);
                   

/* Write a string on the "Surface" with the current font */ 
     void            PutString(SDL_Surface * Surface,
                               int x,
                               int y,
                               const char *text);
                   

/* Write a string on the "Surface" with the specified font */ 
     void            PutStringFont(SDL_Surface * Surface,
                                   BFont_Info * Font,
                                   int x,
                                   int y,
                                   const char *text);
                   

/* Write a left-aligned string on the "Surface" with the current font */ 
     void            LeftPutString(SDL_Surface * Surface,
                                   int y,
                                   const char *text);
                   

/* Write a left-aligned string on the "Surface" with the specified font */ 
     void            LeftPutStringFont(SDL_Surface * Surface,
                                       BFont_Info * Font,
                                       int y,
                                       const char *text);
                   

/* Write a center-aligned string on the "Surface" with the current font */ 
     void            CenteredPutString(SDL_Surface * Surface,
                                       int y,
                                       const char *text);
                   

/* Write a center-aligned string on the "Surface" with the specified font */ 
     void            CenteredPutStringFont(SDL_Surface * Surface,
                                           BFont_Info * Font,
                                           int y,
                                           const char *text);
                   

/* Write a right-aligned string on the "Surface" with the specified font */ 
     void            RightPutString(SDL_Surface * Surface,
                                    int y,
                                    const char *text);
                   

/* Write a right-aligned string on the "Surface" with the specified font */ 
     void            RightPutStringFont(SDL_Surface * Surface,
                                        BFont_Info * Font,
                                        int y,
                                        const char *text);
                   

/* Write a justify-aligned string on the "Surface" with the specified font */ 
     void            JustifiedPutString(SDL_Surface * Surface,
                                        int y,
                                        const char *text);
                   

/* Write a justify-aligned string on the "Surface" with the specified font */ 
     void            JustifiedPutStringFont(SDL_Surface * Surface,
                                            BFont_Info * Font,
                                            int y,
                                            const char *text);
                  

/* The following functions do the same task but have the classic "printf" sintax */ 
                    void PrintString(SDL_Surface * Surface,
                                      int x,
                                      int y,
                                      const char *fmt,
                                      ...);
                    void PrintStringFont(SDL_Surface * Surface,
                                          BFont_Info * Font,
                                          int x,
                                          int y,
                                          const char *fmt,
                                          ...);
                   void CenteredPrintString(SDL_Surface * Surface,
                                              int y,
                                              const char *fmt,
                                              ...);
                    void CenteredPrintStringFont(SDL_Surface * Surface,
                                                  BFont_Info * Font,
                                                  int y,
                                                  const char *fmt,
                                                  ...);
                   void RightPrintString(SDL_Surface * Surface,
                                           int y,
                                           const char *fmt,
                                           ...);
                    void RightPrintStringFont(SDL_Surface * Surface,
                                               BFont_Info * Font,
                                               int y,
                                               const char *fmt,
                                               ...);
                   void LeftPrintString(SDL_Surface * Surface,
                                          int y,
                                          const char *fmt,
                                          ...);
                    void LeftPrintStringFont(SDL_Surface * Surface,
                                              BFont_Info * Font,
                                              int y,
                                              const char *fmt,
                                              ...);
                   void JustifiedPrintString(SDL_Surface * Surface,
                                               int y,
                                               const char *fmt,
                                               ...);
                    void JustifiedPrintStringFont(SDL_Surface * Surface,
                                                   BFont_Info * Font,
                                                   int y,
                                                   const char *fmt,
                                                   ...);
                  

/* BFont v. 1.0.3 */ 
                   

/* Returns a new font colored with the color (r,g,b) */ 
                     BFont_Info * SetFontColor(BFont_Info * Font,
                                               Uint8 r,
                                               Uint8 g,
                                               Uint8 b);
                 

/* BFont v. 1.0.4 */ 
                   

/* Load and store the font int the BFont_Info structure from a SDL surface */ 
      
         
         
         
         
                   BFont_Info * LoadFontFromSurface(SDL_Surface * Surface);
                 
#ifdef __cplusplus
}
               
#endif                          /*  */
               
#endif  /* IH_BFONT_H */
               
