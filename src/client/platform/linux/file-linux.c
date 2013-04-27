
/* $Id: file-linux.c,v 1.14 2003/03/18 22:03:35 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#ifdef __linux__

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#include "angband.h"
#include "list.h"
#include "path.h"
#include "file.h"

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

char           *
IH_GetManifestFilename(cptr path,
                       int item_num)
{
     char           *manifest_file, *file = NULL;
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
               char           *num = NULL, *f = NULL, *c;

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

ihList         *
IH_GetSaveFiles(void)
{
     ihList         *list = NULL;
     char           *path_save;

     list = ralloc(sizeof(ihList));
     if(!list)
          return NULL;

     IH_ListInit(list);

     path_save = IH_GetDataDir("save");

     if(path_save)
     {
          DIR            *dir;

          dir = opendir(path_save);
          if(dir)
          {
               struct dirent  *de;

               while(de = readdir(dir))
               {
                    char           *file;
                    int             len;

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

#endif /* __linux__ */
