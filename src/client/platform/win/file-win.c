
/* $Id: file-win.c,v 1.4 2003/03/17 22:45:32 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#ifdef __WIN32__

#include "angband.h"
#include "path.h"

cptr
IH_GetDataDir(cptr dir)
{
     cptr            path, utf8;

#if 0
     NSAutoreleasePool *pool =[[NSAutoreleasePool alloc] init];

     utf8 =[[[NSBundle mainBundle] bundlePath] UTF8String];
     path = IH_PathBuild(utf8, "..", dir, NULL);

     [pool release];
#endif
     return path;
}

bool
IH_CreateConfigDir(void)
{
     cptr            path_home;
     bool            success = FALSE;

#ifdef PRIVATE_USER_PATH

     success = !mkdir(PRIVATE_USER_PATH, 0700);

#else /* PRIVATE_USER_PATH */

     path_home = getenv("HOME");
     if(path_home)
     {
          cptr            path_ih, path_config;

          path_ih = IH_PathBuild(path_home, ".ironhells", NULL);
          path_config = IH_PathBuild(path_ih, "config", NULL);

          mkdir(path_ih, 0700);
          success = !mkdir(path_config, 0700);

          rnfree(path_config);
          rnfree(path_ih);
     }

#endif /* PRIVATE_USER_PATH */

     return success;
}

cptr
IH_GetConfigDir(void)
{
     cptr            path_config = NULL;
     cptr            path_home;

#ifdef PRIVATE_USER_PATH

     path_config = IH_PathBuild(PRIVATE_USER_PATH, NULL);

#else /* PRIVATE_USER_PATH */

     path_home = getenv("HOME");
     if(path_home)
     {
          path_config =
              IH_PathBuild(path_home, ".ironhells", "config", NULL);
     }

#endif /* PRIVATE_USER_PATH */

     return path_config;
}

cptr
IH_GetManifestFilename(cptr path,
                       int item_num)
{
     cptr            manifest_file, file = NULL;
     FILE           *fp;

     manifest_file = IH_PathBuild(path, "MANIFEST", NULL);
     if(!manifest_file)
          return NULL;

     fp = fopen(manifest_file, "r");
     if(fp)
     {
          char            buf[FILENAME_MAX];

          while(fgets(buf, sizeof(buf), fp))
          {
               cptr            num = NULL, f = NULL, c;

               /* Skip blank lines and "comments."
                */
               if(!buf[0] ||
                  buf[0] == '\n' || buf[0] == '\r' || buf[0] == '#')
                    continue;

               if(c = strchr(buf, '\r'))
                    *c = 0;
               if(c = strchr(buf, '\n'))
                    *c = 0;

               if(c = strchr(buf, ' '))
               {
                    *c = 0;
                    num = buf;
                    f = c + 1;
               }

               if(num && buf)
               {
                    int             inum;

                    inum = atoi(num);
                    if(inum == item_num)
                    {
                         int             len;

                         len = strlen(f);

                         file = ralloc(len + 2);
                         my_strcpy(file, f, len + 1);
                    }
               }
          }

          fclose(fp);
     }

     rnfree(manifest_file);

     return file;
}

#endif /* __WIN32__ */
