/* This is the Fun and Games 'front end' for all games.  It checks the load
   average (15 min) and number of users before allowing play.  The limits are
   defined in the file RESTRICT_FILE (see header).  If this file does not
   exist, or the host has no entry, games are derestricted on that machine.
   Probably best sgid fun, with execute perms for 'others' removed on all
   games executables.  Create a symbolic link to this named after the
   game */

#include <utmp.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include "gamecheck.h"

char gamepath[1024]; 		/* Path to game file */
int maxusers, maxload; 		/* Limits */


int g_access();
void unlock();
void log_game();

/* Scan the list of directories looking for the game */
int game_exists(gamename)
     char *gamename;
{
  struct stats	buf;
  int i=0;

  while (strcmp(dir_list[i],"END")) { /* Go through list of possibles */
    (void)strcpy(gamepath, dir_list[i]);
    (void)strcat(gamepath, gamename);	/* Tack the name to the directory */
    if (!stat(gamepath, &buf))	/* Crude, but works */
      return OK_GAME;		/* Okey Dokey, found it */
    i++;
  }
  return NO_GAME;		/* Possibilities exhausted */
}


int restrictions()
{
  return 0;
}


main(argc,argv)
     int argc;
     char **argv;
{
  char *stroke = 0;
  int 		count=0, load=0;

  /* Peel off the basename to avoid people like Alfie being awkward */
  if ((stroke = strrchr(argv[0], '/')) != 0)
    argv[0] = stroke + 1;

  if (!restrictions())
    goto nochecks;		/* Skip checking - no restrictions file */

  if (maxusers == 0 && maxload == 0) {
    (void)printf("Sorry, this machine may not be used for playing games.\n");
    exit(1);
  }

  }

 nochecks:

  switch (game_exists(argv[0])) {

  case NO_GAME:
      (void)printf("Game doesn't exist - '%s'\n",argv[0]);
      break;

  case OK_GAME:
      log_game(argv[0]);
      execv(gamepath,&argv[0]);
      (void)printf("Exec failed - please mail fun\n");
      break;
  }
  exit(1);
}

void log_game(game)
    char *game;
{
    char user[9];
    struct passwd *userinfo;
    char buff[256];
    int fd,uid;

    if ((fd = open(GAME_LOG, O_APPEND | O_WRONLY)) <0 ) {
	perror(GAME_LOG);
	exit(1);
    }
    if (!(userinfo = getpwuid(uid = getuid()))) {
	perror("");
	exit(1);
    }
    sprintf(buff,"%-8s: %s\n",userinfo->pw_name,game);

    /* Dunno what would be best to do if we get error - so ignore it :-) */
    write(fd,buff,strlen(buff));

    /* Quick kludge - open seems to chgrp GAME_LOG back to fun if this program
     * is ran under uid fun.
     */
    if (uid == 1078)
	chown(GAME_LOG,1078,130);
}
