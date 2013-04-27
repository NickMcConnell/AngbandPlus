
#include "angband.h"
#include "game-cmd.h"

/*
 * A function called by the game to get a command from the UI.
 * Just a hook, with the real function supplied by the UI.
 */
command_fetch* get_game_command = NULL;
