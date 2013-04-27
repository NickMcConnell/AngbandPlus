
/* $Id: video.h,v 1.2 2003/04/16 17:30:45 cipher Exp $ */

#ifndef IH_SCENE_VIDEO_H
#define IH_SCENE_VIDEO_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Function prototypes.
*/
void            IH_InitScene_Video(void);
void            IH_ProcessScene_Video(SDL_Event * event);
void            IH_RenderScene_Video(void);
void            IH_CleanupScene_Video(void);

/* Data definitions.
*/

#endif /* IH_SCENE_VIDEO_H */
