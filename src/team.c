/* File: team.c */

/*
 * Copyright (c) 1999 Karl R. Peters
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * And yes, I am too lazy to write my own copyright statements. 
 */
 
#include "angband.h"

/*
 * Have player follow another player -KRP
 */
void follow_leader(void)
{
	int mm[5];
	int i, d, ny, nx;
	bool do_move = FALSE;

	/* Find out which way(s) to go */
	get_moves((-1) - p_ptr->whoami, mm);

	for (i = 0; (i < 5) && (do_move == FALSE); i++)
	{
		d = mm[i];
		ny = p_ptr->py + ddy[d];
		nx = p_ptr->px + ddx[d];

/*		msg_print("Trying %d,%d to %d,%d. ",
 *			p_ptr->px, p_ptr->py, nx, ny);
 */
		if (do_cmd_walk_test(ny, nx))
		{
			do_move = TRUE;		/* We can move */
			p_ptr->command_dir = d; /* Go this way */
		}
	}

	if (do_move) do_cmd_walk();	/* Move player */
	else
	{
/*		disturb(1, 0);		*/ /* Player can't follow */
		/* Disturbing is making a mess. -KRP */
		msg_format("%s is scratchig %s head: Which way do I go? ",
			op_ptr->full_name, sp_ptr->gen);
		
		/* We don't want ifinite cycle, do we? -IB */
		p_ptr->energy_use = 100;
	}
}
   
/*
 * Tell characters to follow current character -KRP
 */
void do_cmd_leader(void)
{
	/* Wake everyone up */
	FOR_EACH_CHAR
	(
		disturb(1, 0);
	)

	if (leader == NOT_LEADING)
	{
		/* Set leader */
		leader = p_ptr->whoami;
		msg_print("OK guys, follow me! ");
		FOR_EACH_CHAR
		(
			if (p_ptr->whoami != leader)
				p_ptr->following = TRUE;
		)
	}
	else if (leader == p_ptr->whoami)
	{
		/* Exit leader mode */
		leader = NOT_LEADING;
		msg_print("Quit following me, already! ");
		FOR_EACH_CHAR
		(
			p_ptr->following = FALSE;
		)
	}
	else
		msg_print("You're not the leader. ");
}

/*
 * Tell everyone to take a nap -KRP
 */
void do_cmd_nap(void)
{
	leader = p_ptr->whoami;
	FOR_EACH_CHAR
	(
		if (p_ptr->whoami != leader)
			p_ptr->resting = 1024;
	)
	msg_print("Take a break, guys. ");
}

/*
 * Try again to start following the leader -KRP
 */
void do_cmd_tagalong(void)
{
	if (leader == NOT_LEADING)
		msg_print("There's no leader. ");
	else if (leader == p_ptr->whoami)
		msg_print("But you're the leader! ");
	else
	{
		p_ptr->following = TRUE;
		follow_leader();
	}
}

/*
 * Disturb all team members
 * -KRP
 */
void do_cmd_disturb_all(void)
{
	FOR_EACH_CHAR
	(
		disturb(1,0);
	)
	msg_print("Everybody, wake up! ");
	msg_print("Here comes trouble! ");
	leader = NOT_LEADING;
}
