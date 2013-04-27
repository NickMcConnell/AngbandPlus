
/* $Id: file-linux.c,v 1.19 2003/04/07 20:53:59 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/h-config.h"

#ifdef __linux__

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#include "angband/angband.h"
#include "list.h"
#include "path.h"
#include "platform/platform.h"

char           *
IH_GetDataDir(cptr dir)
{
     char           *path = NULL;
     char           *data_path;

     data_path = getenv("IRONHELLS_DATA_PATH");
     if(!data_path || !*data_path)
     {
          struct stat     st;
          char           *try_path;
          char           *path_home;

          /* Perhaps try an ".ironhells/data" directory in the user's
           * home directory?
           */
          path_home = getenv("HOME");
          try_path = IH_PathBuild(path_home, ".ironhells", NULL);

          if(try_path)
          {
               if(!stat(try_path, &st))
               {
                    if(S_ISDIR(st.st_mode))
                         return try_path;
               }

               rnfree(try_path);
          }

          data_path = ".";
     }

     path = IH_PathBuild(data_path, dir, NULL);

     return path;
}

bool
IH_CreateConfigDir(void)
{
     char           *path_home;
     bool            success = FALSE;

#ifdef PRIVATE_USER_PATH

     success = !mkdir(PRIVATE_USER_PATH, 0700);

#else /* PRIVATE_USER_PATH */

     path_home = getenv("HOME");
     if(path_home)
     {
          char           *path_ih, *path_config;

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

char           *
IH_GetConfigDir(void)
{
     char           *path_config = NULL;
     char           *path_home;

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

int
IH_GetSaveFiles(char **list,
                int size)
{
     char           *path_save;
     int             count = 0;

     /* Get the save directory.
      */
     path_save = IH_GetDataDir("save");
     if(path_save)
     {
          DIR            *dir;

          /* Open it.
           */
          dir = opendir(path_save);
          if(dir)
          {
               struct dirent  *de;

               /* Go over the contents of the directory.
                */
               while(de = readdir(dir))
               {
                    char           *file;
                    int             len;

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

#endif /* __linux__ */