/* File: bookless.c
 * Purpose: commands for bookless spells
 */

#include "angband.h"
#include "bookless.h"

void do_cmd_bookless()
{
	if player_has(PF_CAST_PYRO)
		do_cmd_pyro();
	
	else msg_print("You have no magical powers.");
	return;
}
