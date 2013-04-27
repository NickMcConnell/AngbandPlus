
/* $Id: path.c,v 1.9 2003/04/01 22:26:00 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Standard headers */
#include <stdarg.h>

/* Internal headers */
#include "angband/angband.h"
#include "path.h"
#include "angband/z-util.h"

char           *
IH_PathBuild(cptr dir,
             ...)
{
     va_list         ap;
     char           *path = NULL;
     char           *item;
     cptr            first;
     int             len;

     first = dir;

     /* Check if dir is the "home directory" (~).
      */
     if(!strcmp(first, "~"))
     {
          first = getenv("HOME");
     }

     len = strlen(first) + 1;
     path = ralloc(len);
     my_strcpy(path, first, len);

     va_start(ap, dir);
     while(item = va_arg(ap, char *))
     {
          char           *tmp, *exp = NULL;
          int             len;

          /* Check for expandable items.
           */

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
