
/* $Id: pointer.c,v 1.3 2003/04/18 21:45:45 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */

/* Internal headers */
#include "ironhells.h"
#include "render/pointer.h"

void
IH_RenderPointer(void)
{
     if(ih.display.render_pointer_func)
          (*ih.display.render_pointer_func) ();
}
