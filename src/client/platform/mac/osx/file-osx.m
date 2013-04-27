/* $Id: file-osx.m,v 1.12 2003/03/19 04:31:23 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#if defined(MACOSX) && defined(__APPLE__)

#include <sys/types.h>
#include <dirent.h>

#include <Cocoa/Cocoa.h>

#include "angband.h"
#include "file.h"
#include "path.h"
#include "list.h"

char *IH_GetDataDir(cptr dir)
{
     NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
     char *path;
     const char *utf8;

//     utf8 = [[[NSBundle mainBundle] bundlePath] UTF8String];
     utf8 = [[[NSBundle mainBundle] resourcePath] UTF8String];
     path = IH_PathBuild(utf8, dir, NULL);

     [pool release];

     fprintf(stderr, "path = '%s'\n", path);
     return path;
}

bool IH_CreateConfigDir(void)
{
     /* The config directory is ~/Library/Preferences, which we
      * assume already exists.
      */
     return TRUE;
}

char *IH_GetConfigDir(void)
{
     return IH_PathBuild("~", "Library", "Preferences", NULL);
}

char *IH_GetManifestFilename(cptr path, int item_num)
{
     char *manifest_file, *file = NULL;
     FILE *fp;
     
     manifest_file = IH_PathBuild(path, "MANIFEST", NULL);
     if(!manifest_file)
          return NULL;
     
     fp = fopen(manifest_file, "r");
     if(fp)
     {
          char buf[FILENAME_MAX];

          while(fgets(buf, sizeof(buf), fp))
          {
               byte *num = NULL, *f = NULL, *c;
               
               /* Skip blank lines and "comments."
                */
               if(!buf[0] ||
                  buf[0] == '\n' ||
                  buf[0] == '\r' ||
                  buf[0] == '#')
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
               
               if(num &&
                  buf)
               {
                    int inum;
                    
                    inum = atoi(num);
                    
                    if(inum == item_num)
                    {
                         int len;

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

ihList *
IH_GetSaveFiles(void)
{
     ihList *list = NULL;
     char *path_save;
     
     list = ralloc(sizeof(ihList));
     if(!list)
          return NULL;
     
     IH_ListInit(list);
     
     path_save = IH_GetDataDir("save");
     if(path_save)
     {
          DIR *dir;
          
          dir = opendir(path_save);
          if(dir)
          {
               struct dirent *de;

               while(de = readdir(dir))
               {
                    char *file;
                    int len;

                    if(de->d_type != DT_REG)
                         continue;

                    len = strlen(de->d_name) + 1;

                    file = ralloc(len);
                    if(file)
                    {
                         my_strcpy(file, de->d_name, len);

                         IH_ListAppend(list, file);
                    }
               }

               closedir(dir);
          }
     }

     rnfree(path_save);

     return list;
}

#endif /* MACOSX/__APPLE__ */
