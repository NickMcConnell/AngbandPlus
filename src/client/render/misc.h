
/* $Id: misc.h,v 1.7 2003/04/18 21:45:45 cipher Exp $ */

#ifndef IH_RENDER_MISC_H
#define IH_RENDER_MISC_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */

/* Internal headers */

/* Forward declarations */
typedef struct _ihColor ihColor;

/* Function prototypes.
 */
errr            IH_InitMisc(void);
void            IH_PositionMisc(void);
void            IH_RenderMisc(void);
void            IH_RenderError(void);
void            IH_RenderBackground(void);
void            IH_RenderTitle(void);
void            IH_RenderSplash(void);
void            IH_FrameArea(int x,
                             int y,
                             int w,
                             int h,
                             ihColor * color);
void            IH_ShadeArea(int x,
                             int y,
                             int w,
                             int h,
                             ihColor * color);
void            IH_FillArea(SDL_Rect * rect,
                            ihColor * color);
void            IH_RenderImage(SDL_Surface * surface,
                               SDL_Rect * srect,
                               SDL_Rect * drect);
void            IH_DrawLine(int x1,
                            int y1,
                            int x2,
                            int y2,
                            ihColor * color);
void            IH_SwapBuffers(void);
void            IH_AttrToColor(byte attr,
                               ihColor * color);
void            IH_GetButtonColor(int hilite,
                                  ihColor * color);
int             IH_IsPointerInRect(int x,
                                   int y,
                                   SDL_Rect * rect);
void            IH_ClearDrawingBuffer(void);

/* Data definitions.
 */
#define IH_MAX_MISC_OBJECTS 1024
#define IH_MAX_TILES 1024
#define IH_MAX_OBJECTS 1024
#define IH_MAX_CREATURES 1024

enum
{
     IH_HILITE_NORMAL,
     IH_HILITE_HOVER,
     IH_HILITE_SELECT,

     IH_HILITE_END
};

struct _ihColor
{
     float           red;
     float           green;
     float           blue;
     float           alpha;
};

#endif /* IH_RENDER_MISC_H */
