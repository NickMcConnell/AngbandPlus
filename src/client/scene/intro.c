
/* $Id: intro.c,v 1.3 2003/04/18 21:45:46 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"

/* Internal headers */
#include "ironhells.h"
#include "scene/intro.h"
#include "render/pointer.h"

void
IH_InitScene_Intro(void)
{
     ih.pointer = IH_POINTER_NONE;

}

void
IH_ProcessScene_Intro(SDL_Event * event)
{

}

void
IH_RenderScene_Intro(void)
{
}

void
IH_CleanupScene_Intro(void)
{
}
