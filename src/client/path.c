
/* $Id: path.c,v 1.7 2003/03/18 19:17:39 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include <stdarg.h>

#include "angband.h"
#include "path.h"
#include "z-util.h"

char           *
IH_PathBuild(cptr dir,
             ...)
{
     va_list         ap;
     char           *path = NULL;
     char           *item;
     int             len;

     len = strlen(dir) + 1;
     path = ralloc(len);
     my_strcpy(path, dir, len);

     va_start(ap, dir);
     while(item = va_arg(ap, char *))
     {
          char           *tmp, *exp = NULL;
          int             len;

          /* Check for expandable items.
           */
          if(!strcmp(item, "~"))
          {
               exp = IH_PathBuild(getenv("HOME"), NULL);
          }

          len =
              strlen(path) + strlen(exp ? exp : item) + strlen(PATH_SEP) +
              1;

          tmp = ralloc(len);
          if(tmp)
          {
               path_build(tmp, len, path, exp ? exp : item);

               rnfree(path);
               path = tmp;
          }

          if(exp)
               rnfree(exp);
     }
     va_end(ap);

     return path;
}
