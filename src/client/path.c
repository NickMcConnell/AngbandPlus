/* File: path.c */

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

#undef DEBUG

cptr IH_PathBuild(cptr dir, ...)
{
     va_list ap;
     cptr path = NULL;
     cptr item;
     int len;

#ifdef DEBUG
     fprintf(stderr, "initial dir = %s\n", dir);
#endif
     
     len = strlen(dir) + 1;
     path = ralloc(len);
     my_strcpy(path, dir, len);
#ifdef DEBUG
     fprintf(stderr, "initial path = %s\n", path);
#endif
     
     va_start(ap, dir);
     while(item = va_arg(ap, cptr))
     {
          cptr tmp;
          int len;
          
#ifdef DEBUG
          fprintf(stderr, "item = %s\n", item);
#endif

          len = strlen(path) + strlen(item) + strlen(PATH_SEP) + 1;
#ifdef DEBUG
          fprintf(stderr, "len = %d\n", len);
#endif
          tmp = ralloc(len);
          if(tmp)
          {
               path_build(tmp, len, path, item);
#ifdef DEBUG
               fprintf(stderr, "tmp = %s\n", tmp);
#endif
               rnfree(path);
               path = tmp;
#ifdef DEBUG
               fprintf(stderr, "path = %s\n", path);
#endif
          }
     }
     va_end(ap);

#ifdef DEBUG
     fprintf(stderr, "final path = %s\n", path);
#endif
     
     return path;     
}
