## Makefile.am -- Process this file with automake to process Makefile.in

SUBDIRS = lua

l-monst.c: lua/tolua l-monst.pkg
	lua/tolua -n monster -o l-monst.c l-monst.pkg

l-object.c: lua/tolua l-object.pkg
	lua/tolua -n object -o l-object.c l-object.pkg

l-player.c: lua/tolua l-player.pkg
	lua/tolua -n player -o l-player.c l-player.pkg

l-random.c: lua/tolua l-random.pkg
	lua/tolua -n random -o l-random.c l-random.pkg

l-ui.c: lua/tolua l-ui.pkg
	lua/tolua -n ui -o l-ui.c l-ui.pkg

l-misc.c: lua/tolua l-misc.pkg
	lua/tolua -n misc -o l-misc.c l-misc.pkg

l-spell.c: lua/tolua l-spell.pkg
	lua/tolua -n spell -o l-spell.c l-spell.pkg

#install-exec-hook:
#if SET_GID
#	chgrp "@GAMEGROUP@" "$(DESTDIR)$(bindir)/angband"
#	chmod g+s "$(DESTDIR)$(bindir)/angband"
#endif

