/* File: file-linux.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-config.h"

#ifdef __linux__

#include <stdlib.h>
#include <sys/stat.h>

#include "angband.h"
#include "path.h"
#include "file.h"

cptr IH_GetDataDir(cptr dir)
{
     cptr path = NULL;
	cptr data_path;

	data_path = getenv("IRONHELLS_DATA_PATH");
	if(!data_path)
	{
		struct stat st;
		cptr try_path;
		cptr path_home;

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
if(data_path)
	fprintf(stderr, "using data_path '%s'\n", data_path);

	path = IH_PathBuild(data_path, dir, NULL);
     
if(path)
	fprintf(stderr, "using data path '%s'\n", path);

     return path;
}

bool IH_CreateConfigDir(void)
{
     cptr path_home;
     bool success = FALSE;

#ifdef PRIVATE_USER_PATH

     success = !mkdir(PRIVATE_USER_PATH, 0700);

#else /* PRIVATE_USER_PATH */

     path_home = getenv("HOME");
     if(path_home)
     {
          cptr path_ih, path_config;

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

cptr IH_GetConfigDir(void)
{
     cptr path_config = NULL;
     cptr path_home;

#ifdef PRIVATE_USER_PATH

     path_config = IH_PathBuild(PRIVATE_USER_PATH, NULL);

#else /* PRIVATE_USER_PATH */

     path_home = getenv("HOME");
     if(path_home)
     {
          path_config = IH_PathBuild(path_home, ".ironhells", "config", NULL);
     }

#endif /* PRIVATE_USER_PATH */


     return path_config;
}

#endif /* __linux__ */
