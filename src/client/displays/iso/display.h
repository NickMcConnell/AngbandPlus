
/* $Id: display.h,v 1.1 2003/04/01 22:26:12 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_DISPLAY_H
#define IH_DISPLAYS_ISO_DISPLAY_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/angband.h"
#include "angband/z-disp.h"

/* Forward declare
 */
typedef struct disp_data disp_data;

/* Function prototypes.
 */
bool            IH_InitDisplays(void);
errr            IH_ISO_Init(void);
void            IH_ISO_Cleanup(void);

/* Extra "term" data.
 */
struct disp_data
{
     disp            t;

#if 0
     Rect            r;
     WindowPtr       w;

#ifdef ANGBAND_LITE_MAC
     /* Nothing */
#else                           /* ANGBAND_LITE_MAC */
     short           padding;
     short           pixelDepth;
     GWorldPtr       theGWorld;
     GDHandle        theGDH;
     GDHandle        mainSWGDH;
#endif                          /* ANGBAND_LITE_MAC */

     Str15           title;
     s16b            oops;
     s16b            keys;
     s16b            last;
     s16b            mapped;
     s16b            rows;
     s16b            cols;
     s16b            font_id;
     s16b            font_size;
     s16b            font_face;
     s16b            font_mono;
     s16b            font_o_x;
     s16b            font_o_y;
     s16b            font_wid;
     s16b            font_hgt;
     s16b            tile_o_x;
     s16b            tile_o_y;
     s16b            tile_wid;
     s16b            tile_hgt;
     s16b            size_wid;
     s16b            size_hgt;
     s16b            size_ow1;
     s16b            size_oh1;
     s16b            size_ow2;
     s16b            size_oh2;
#endif
};

/*
 * Maximum number of displays
 */
#define MAX_DISP_DATA 8

#endif /* IH_DISPLAYS_ISO_DISPLAY_H */
