
/* $Id: newchar.h,v 1.1 2003/04/01 22:26:20 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_SCENE_NEWCHAR_H
#define IH_DISPLAYS_ISO_SCENE_NEWCHAR_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

void            IH_InitScene_NewChar(void);
void            IH_ProcessScene_NewChar(SDL_Event * event);
void            IH_RenderScene_NewChar(void);
void            IH_CleanupScene_NewChar(void);

#endif /* IH_DISPLAYS_ISO_SCENE_NEWCHAR_H */
