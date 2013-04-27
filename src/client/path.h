
/* $Id: path.h,v 1.6 2003/04/01 22:26:00 cipher Exp $ */

#ifndef IH_PATH_H
#define IH_PATH_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/angband.h"

char           *IH_PathBuild(cptr dir,
                             ...);

#endif /* IH_PATH_H */