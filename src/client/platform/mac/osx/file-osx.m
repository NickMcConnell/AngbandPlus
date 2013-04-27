/* $Id: file-osx.m,v 1.17 2003/04/07 20:53:59 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/h-config.h"

#if defined(MACOSX) && defined(__APPLE__)

#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#include <Cocoa/Cocoa.h>

#include "angband/angband.h"
#include "platform/platform.h"
#include "path.h"
#include "list.h"

char *IH_GetDataDir(cptr dir)
{
     struct stat st;
     NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
     char *path;
     const char *res_path;
     
     if(!pool)
          return NULL;

     /* Try "~/Library/Application Support/Iron Hells".
      */
     path = IH_PathBuild("~", 
                         "Library", 
                         "Application Support", 
                         "Iron Hells", 
                         dir, 
                         NULL);
     
     if(path)
     {
          int er;
          
          if(!stat(path, &st))
          {
               if(S_ISDIR(st.st_mode))
                    return path;
          }

          rnfree(path);
     }

     res_path = [[[NSBundle mainBundle] resourcePath] UTF8String];
     path = IH_PathBuild(res_path, dir, NULL);

     [pool release];

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

int
IH_GetSaveFiles(char **list, int size)
{
     char *path_save;
     int count = 0;

     /* Get the save directory.
      */     
     path_save = IH_GetDataDir("save");
     if(path_save)
     {
          DIR *dir;
        
          /* Open it.
           */  
          dir = opendir(path_save);
          if(dir)
          {
               struct dirent *de;

               /* Go over the contents of the directory.
                */
               while(de = readdir(dir))
               {
                    char *file;
                    int len;

                    /* Skip entries that aren't regular files.
                     */
                    if(de->d_type != DT_REG)
                         continue;
        
                    /* Don't put too many items in the list.
                     */                 
                    if(count >= size)
                         break;

                    len = strlen(de->d_name) + 1;

                    /* Save the name.
                     */
                    file = ralloc(len);
                    if(file)
                    {
                         my_strcpy(file, de->d_name, len);
                         list[count++] = file;
                    }
               }

               closedir(dir);
          }
     }

     rnfree(path_save);

     return count;
}

#endif /* MACOSX/__APPLE__ */
