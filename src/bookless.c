/* File: bookless.c
 * Purpose: commands for bookless spells
 */

#include "angband.h"
#include "bookless.h"

void do_cmd_bookless()
{
	/* Disallow repeat to prevent nasty surprises */
	cmd_disable_repeat();
	if player_has(PF_CAST_PYRO)
		do_cmd_pyro();
	else if player_has(PF_CAST_AVATAR)
		do_cmd_avatar();
	else if player_has(PF_CAST_SAPPER)
		do_cmd_sapper();
	
	else msg_print("You have no magical powers.");
	return;
}
