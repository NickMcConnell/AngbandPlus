/* File: help.c */

/* Purpose: ingame help */

/*
 * Copyright (c) 2001 DarkGod
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Display a context-sensitive help text
 */
static void print_help(cptr *text)
{
	/* Paranoia */
	if (text == NULL) return;

	/* Display all lines until terminating NULL is reached */
	while (*text)
	{
		/* Display a line in yellow */
		cmsg_print(TERM_YELLOW, *text);

		/* Next line */
		text++;
	}
}


/*
 * Movement help texts -- Used unusual indentation so that anyone can see
 * if a line fits in 80 columns
 */
static cptr help_between[] =
{
"Between Gates can be entered by pressing the > key. They will transport",
"you to an other gate, but beware of the cold damage that might kill you.",
NULL
};

static cptr help_altar[] =
{
"Altars are the way to reach the Valar and the Maiars, powers of the world,",
"usualy called Gods. You can press O to offer a god a sacrifice and to",
"become his/her follower. Beware that once you follow a god it is hard to change.",
NULL
};

static cptr help_fountain[] =
{
"Fountains are always magical, you can quaff from them by pressing H.",
"Beware that unlike potions they cannot be identified.",
NULL
};

static cptr help_identify[] =
{
"So you found your first item, nice eh ? Now when you stumble across",
"objects you can pick them up by pressing g, and if you are wondering",
"what they do press I to get some basic information.",
"You may also want to identify them with scrolls, staves, rods or spells.",
NULL
};


/*
 * Display context-sensitive help text for current grid
 */
bool help_move(char *fmt)
{
	s32b y, x;
	cave_type *c_ptr;

	/* Get current coordinate */
	y = get_next_arg(fmt);
	x = get_next_arg(fmt);

	/* Access current player grid */
	c_ptr = &cave[y][x];

	if (c_ptr->feat == FEAT_BETWEEN)
	{
		if (!(p_ptr->help.help1 & HELP1_BETWEEN))
		{
			p_ptr->help.help1 |= (HELP1_BETWEEN);
			print_help(help_between);
		}
	}
	else if ((c_ptr->feat >= FEAT_ALTAR_HEAD) &&
	         (c_ptr->feat <= FEAT_ALTAR_TAIL))
	{
		if (!(p_ptr->help.help1 & HELP1_ALTAR))
		{
			p_ptr->help.help1 |= HELP1_ALTAR;
			print_help(help_altar);
		}
	}
	else if (c_ptr->feat == FEAT_FOUNTAIN)
	{
		if (!(p_ptr->help.help1 & HELP1_FOUNTAIN))
		{
			p_ptr->help.help1 |= HELP1_FOUNTAIN;
			print_help(help_fountain);
		}
	}

	if (c_ptr->o_idx)
	{
		if (!(p_ptr->help.help1 & HELP1_IDENTIFY))
		{
			p_ptr->help.help1 |= HELP1_IDENTIFY;
			print_help(help_identify);
		}
	}

	return (FALSE);
}


/*
 * End-of-turn help texts -- Used unusual indentation so that anyone can see
 * if a line fits in 80 columns
 */
cptr help_wildmode[] =
{
"Ahh wilderness travel... The overview mode will allow you to travel",
"fast, but that comes to the cost of GREATLY increased food cunsumption.",
"So you should really watch your hungriness status.",
NULL
};


/*
 * Display context-sensitive help text at the end of user turn
 */
bool help_end_turn(char *fmt)
{
	if (p_ptr->wild_mode)
	{
		if (!(p_ptr->help.help1 & HELP1_WILD_MODE))
		{
			p_ptr->help.help1 |= HELP1_WILD_MODE;
			print_help(help_wildmode);
		}
	}

	return (FALSE);
}


/*
 * Driver for the context-sensitive help system
 */
void ingame_help(bool enable)
{
	if (enable)
	{
		ingame_help(FALSE);
		add_hook(HOOK_MOVE, help_move, "help_move");
		add_hook(HOOK_END_TURN, help_end_turn, "help_end_turn");
	}
	else
	{
		del_hook(HOOK_MOVE, help_move);
		del_hook(HOOK_END_TURN, help_end_turn);
	}
}

