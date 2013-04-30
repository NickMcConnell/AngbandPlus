/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"

/*
 * Check if action permissable here.
 */

bool do_cmd_test(int y, int x, int action)
{
	u32b bitzero = 0x01;
	u32b flag;

	cptr act;

	cptr here = (((p_ptr->px == x ) && (p_ptr->py == y))?"here":"there");

	feature_type *f_ptr;
	int feat;

	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_format("You see nothing %s.",here);

		/* Nope */
		return (FALSE);
	}

	/* Get memorised feature */
	feat = f_info[cave_feat[y][x]].mimic;

	f_ptr = &f_info[feat];

	act=NULL;

	switch (action)
	{
		case FS_SECRET: break;
		case FS_OPEN: act=" to open"; break;    
		case FS_CLOSE: act=" to close"; break;
		case FS_BASH: act=" to bash"; break;
		case FS_DISARM: act=" to disarm"; break;
		case FS_SPIKE: act=" to spike"; break;
		case FS_ENTER: act=" to enter"; break;
		case FS_TUNNEL: act=" to tunnel"; break;
		case FS_LESS: act=" to climb up"; break;
		case FS_MORE: act=" to climb down"; break;
		case FS_RUN: act=" to run on"; break;
		case FS_KILL_MOVE: act=" to disturb"; break;
		case FS_FLOOR: act=" to set a trap on"; break;
		default: break;
	}


	if (action < FS_FLAGS2)
	{
		flag = bitzero << (action - FS_FLAGS1);
		if (!(f_ptr->flags1 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}

	else if (action < FS_FLAGS3)
	{       
		flag = bitzero << (action - FS_FLAGS2);
		if (!(f_ptr->flags2 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}
	
	else if (action < FS_FLAGS_END)
	{       
		flag = bitzero << (action - FS_FLAGS3);
		if (!(f_ptr->flags2 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}

	return (TRUE);  

}

/*
 * Travel to a different dungeon.
 *
 * This whole thing is a hack -- I haven't decided how elegant it is yet.
 */
static void do_cmd_travel(void)
{
	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone1 = &t_ptr->zone[0];
	dungeon_zone *zone2 = &t_ptr->zone[0];

	cptr q, s;

	int item;

	int journey = 0;

	int by = p_ptr->py / BLOCK_HGT;
	int bx = p_ptr->px / BLOCK_WID;

	bool edge_y = ((by < 2) || (by > ((DUNGEON_HGT/BLOCK_HGT)-3)));
	bool edge_x = ((bx < 2) || (bx > ((DUNGEON_WID/BLOCK_WID)-3)));

	/* Get the top of the dungeon */
	get_zone(&zone1,p_ptr->dungeon,min_depth(p_ptr->dungeon));

	/* Get the bottom of the dungeon */
	get_zone(&zone2,p_ptr->dungeon,max_depth(p_ptr->dungeon));

	if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		/* Need to be full to travel for trip */
		if (p_ptr->food < PY_FOOD_FULL)
		{
			msg_print("You'll need a full stomach for the road ahead.");
			msg_print("Tip: Press 'E' to eat some food.");
		}
		else if (p_ptr->blind)
		{
			msg_print("You can't read any maps.");
		}
		else if (p_ptr->confused)
		{
			msg_print("You are too confused.");
		}
		else if (p_ptr->afraid)
		{
			msg_print("You are too afraid.");
		}
		else if (p_ptr->image)
		{
			msg_print("The pink mice don't want you to leave.");
		}
		else if ((p_ptr->poisoned) || (p_ptr->cut) || (p_ptr->stun))
		{
			msg_print("You need to recover from any poison, cuts or stun damage.");
		}
		else if (!edge_y && !edge_x && zone1->fill)
		{
			msg_format("You need to be further from %s.",t_name + t_ptr->name);
		}
		else
		{
			int selection = p_ptr->dungeon;

			/* Restrict choices to scrolls */
			item_tester_tval = TV_MAP;

			/* Get an item */
			q = "Follow which map? ";
			s = "You have no maps to guide you.";
			if (get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) selection = inventory[item].sval;

			/* Make sure we can get back */
			if ((selection != p_ptr->dungeon) && (!(adult_campaign) || (t_info[selection].nearby == p_ptr->dungeon)))
			{
				 msg_format("This map will lead you to %s.",t_name + t_info[selection].name);
				if (get_check("Journey there? "))
				{
					journey = damroll(2,4);
				}
				else
				{
					selection = p_ptr->dungeon;
				}
			}
			else if (selection != p_ptr->dungeon)
			{
				/* XXX Bit wordy, but we may have many locations with 'identical' names. */
				msg_print("You cannot follow this map yet.");
				msg_format("But %s seems the best place to start.", t_name + t_info[t_info[selection].nearby].name);
				selection = p_ptr->dungeon;
			}

			/* Hack -- need a map to leave dungeon */
			/* if (p_ptr->dungeon == 0) return; */
			if (!adult_campaign && p_ptr->dungeon == 0) return;

			if ((selection==p_ptr->dungeon) && !(adult_campaign))
			{
				msg_format("You see a trail back to %s.",t_name + t_info[0].name);
				if (get_check("Journey there? "))
				{
					selection = 0;
					
					journey = damroll(2,4);
				}
			}
			else if ((selection==p_ptr->dungeon) && (t_ptr->nearby != p_ptr->dungeon))
			{
				msg_format("You see a well worn trail to %s.",t_name + t_info[t_ptr->nearby].name);
				if (get_check("Journey there? "))
				{
					selection = t_ptr->nearby;
					
					journey = damroll(2,4);
				}
			}

			if ((selection==p_ptr->dungeon)
			 && (t_ptr->distant != p_ptr->dungeon)
			 && ((!zone2->guard) || (!r_info[zone2->guard].max_num)))
			{

				if (!zone2->guard) msg_format("In the distance lies %s.",t_name + t_info[t_ptr->distant].name);
				else msg_format("By defeating %s, you have opened the way to %s.",
				r_name + r_info[zone2->guard].name, t_name + t_info[t_ptr->distant].name);

				if (get_check("Journey there? "))
				{
					selection = t_ptr->distant;

					journey = damroll(3,4);

				}

			}
			else if ((zone2->guard) && (selection == p_ptr->dungeon))
			{
				/* XXX Reveal monster name? */
				msg_print("All other ways are guarded.");
			}

			/* Do we travel? */
			if (selection != p_ptr->dungeon)
			{
				if (journey < 4)
				{
					msg_print("You have a mild and pleasant journey.");
				}
				else if (journey < 7)
				{
					msg_print("Your travels are without incident.");
				}
				else if (journey < 10)
				{
					msg_print("You have a long and arduous trip.");
				}
				else
				{
					msg_print("You get lost in the wilderness!");
					/* XXX Fake a wilderness location? */
				}

				/* Hack -- Get hungry/tired/sore */
				set_food(p_ptr->food-(PY_FOOD_FULL/10*journey));

				/* Hack -- Time passes (at 4* food use rate) */
				turn += PY_FOOD_FULL/10*journey*4;

				/* XXX Recharges, stop temporary speed etc. */

				/* Change the dungeon */
				p_ptr->dungeon = selection;

				/* Set the new depth */
				p_ptr->depth = min_depth(p_ptr->dungeon);

#if 0
				/* Reset the recall depth */
				p_ptr->max_depth = min_depth(p_ptr->dungeon);
#endif

				/* Mega-hack */
				if ((adult_campaign) && (p_ptr->dungeon == z_info->t_max -1))
				{

					p_ptr->total_winner = TRUE;

					/* Redraw the "title" */
					p_ptr->redraw |= (PR_TITLE);

					/* Congratulations */
					msg_print("*** CONGRATULATIONS ***");
					msg_print("You have won the game!");
					msg_print("You may retire (commit suicide) when you are ready.");
				}

				/* Leaving */
				p_ptr->leaving = TRUE;

			}				

		}

		return;

	}



}


/*
 * Go up one level, or choose a different dungeon.
 */
void do_cmd_go_up(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	feature_type *f_ptr= &f_info[cave_feat[py][px]];

	/* Verify stairs */
	if (!(f_ptr->flags1 & (FF1_STAIRS)) || !(f_ptr->flags1 & (FF1_LESS)))
	{

		/* Travel if possible */
		if (p_ptr->depth == min_depth(p_ptr->dungeon))
		{
			do_cmd_travel();
			return;
		}

		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if ((adult_ironman) && !(adult_campaign))
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* Hack -- tower level increases depth */
	if (t_info[p_ptr->dungeon].zone[0].tower)
	{
		/* New depth */
		p_ptr->depth++;
	}
	else
	{
		/* New depth */
		p_ptr->depth--;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	feature_type *f_ptr= &f_info[cave_feat[py][px]];

	/* Verify stairs */
	if (!(f_ptr->flags1 & (FF1_STAIRS)) || !(f_ptr->flags1 & (FF1_MORE)))
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Hack -- travel through wilderness */
	if ((adult_campaign) && (p_ptr->depth == max_depth(p_ptr->dungeon)))
	{

		message(MSG_STAIRS,0,format("You have found a way through %s.",t_name + t_info[p_ptr->dungeon].name));

		/* Change the dungeon */
		p_ptr->dungeon = t_info[p_ptr->dungeon].distant;

		/* Set the new depth */
		p_ptr->depth = min_depth(p_ptr->dungeon);

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else
	{

		/* Success */
		message(MSG_STAIRS, 0, "You enter a maze of down staircases.");

		/* Create a way back */
		p_ptr->create_up_stair = TRUE;

		/* Hack -- tower level decreases depth */
		if (t_info[p_ptr->dungeon].zone[0].tower)
		{
			/* New depth */
			p_ptr->depth--;
		}
		else
		{
			/* New depth */
			p_ptr->depth++;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{

	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Catch breath */
	if (!(f_ptr->flags2 & (FF2_FILLED)))
	{
		/* Rest the player */
		set_rest(p_ptr->rest + PY_REST_RATE - p_ptr->tiring);
	}

	/* Search */
	search();
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{

	/* Hack - Check if we are holding a song */
	if (p_ptr->held_song)
	{
		/* Finish song */
		p_ptr->held_song = 0;

		/* Tell the player */
		msg_print("You finish your song.");
	}

	/* Stop searching */
	if (p_ptr->searching)
	{
		/* Clear the searching flag */
		p_ptr->searching = FALSE;

		/* Clear the last disturb */
		p_ptr->last_disturb = turn;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start searching */
	else
	{
		/* Set the searching flag */
		p_ptr->searching = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
}



#if defined(ALLOW_EASY_OPEN)

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 * ANDY - Counts features that allow action.
 */
static int count_feats(int *y, int *x, int action)
{
	int d, count;

	int feat;

	feature_type *f_ptr;

	u32b flag, bitzero = 0x000000001L;


	/* Count how many matches */
	count = 0;

	/* Check around the character */
	for (d = 0; d < 8; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Get the feature */
		feat = cave_feat[yy][xx];

		/* Get the mimiced feature */
		feat = f_info[feat].mimic;

		f_ptr = &f_info[feat];

		if (action < FS_FLAGS2)
		{
			flag = bitzero << (action - FS_FLAGS1);
			if (!(f_ptr->flags1 & flag)) continue;  
		}

		else if (action < FS_FLAGS_END)
		{       
			flag = bitzero << (action - FS_FLAGS2);
			if (!(f_ptr->flags2 & flag)) continue;  
		}
	
		/* Count it */
		++count;

		/* Remember the location of the last door found */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}

/*
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
 */
static int coords_to_dir(int y, int x)
{
	return (motion_dir(p_ptr->py, p_ptr->px, y, x));
}

#endif /* ALLOW_EASY_OPEN */


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int i, j;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_test(y, x,FS_OPEN)) return (FALSE);


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_OPEN)) return (FALSE);

	/* Unknown trapped door */
	if (f_info[cave_feat[y][x]].flags3 & (FF3_PICK_DOOR))
	{
		pick_door(y,x);
	}

	/* Trapped door */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}


	/* Secrets on door/permanent doors */
	else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{

		/* Stuck */
		find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Locked door */
	else if ((f_info[cave_feat[y][x]].flags1 & (FF1_OPEN)) && (f_info[cave_feat[y][x]].power >0))
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = f_info[cave_feat[y][x]].power;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Open the door */
			cave_alter_feat(y, x, FS_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_alter_feat(y, x, FS_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_OPENDOOR);
	}

	/* Result */
	return (more);
}



/*
 * Open a closed/locked/jammed door.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Open */
	if (easy_open)
	{
		/* Handle a single closed door  */
		if (count_feats(&y, &x, FS_OPEN)  == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x,FS_OPEN)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply stuck / confusion */
	if (stuck_player(&dir) || confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(y, x);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_test(y, x,FS_CLOSE)) return (FALSE);

	/* Trapped door */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Secrets on door/permanent doors */
	else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{
		/* Stuck */
		find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Close door */
	else
	{
		/* Close the door */
		cave_alter_feat(y, x, FS_CLOSE);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Close */
	if (easy_open)
	{
		/* Handle a single open door */
		if (count_feats(&y, &x, FS_CLOSE) == 1)
		{
			/* Don't close door player is on */
			if ((y != py) || (x != px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_CLOSE)) return;

	/* Take a turn */
	p_ptr->energy_use = 50;

	/* Apply stuck / confusion */
	if (stuck_player(&dir) || confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Close door */
		more = do_cmd_close_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}






/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Do not use twall anymore --- ANDY
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;

	int i,j;

	cptr name;

	int feat;
	
	feat = cave_feat[y][x];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_TUNNEL)) return (FALSE);

	i = p_ptr->skill_dig;

	j = f_info[cave_feat[y][x]].power;      

	/* Hack - bump up power for doors */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_DOOR)) j = 30;

	/* Trapped door */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Permanent doors/rock */
	else if (f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT))

	{
		/* Stuck */
		find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Dig or tunnel */
	else if (f_info[cave_feat[y][x]].flags2 & (FF2_CAN_DIG))
	{

		/* Dig */
		if (p_ptr->skill_dig > rand_int(200 * j))
		{
			sound(SOUND_DIG);

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* Give the message */  
			msg_format("You have removed the %s.",name);
			
			cave_alter_feat(y,x,FS_TUNNEL);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Keep trying */
		else
		{

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* We may continue tunelling */
			msg_format("You dig into the %s.",name);
			more = TRUE;
		}

	}

	else
	{

		/* Tunnel -- much harder */
		if (p_ptr->skill_dig > (j + rand_int(400 * j)))
		{
			sound(SOUND_DIG);

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* Give the message */  
			msg_print("You have finished the tunnel.");
			
			cave_alter_feat(y,x,FS_TUNNEL);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Keep trying */
		else
		{

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* We may continue tunelling */
			msg_format("You tunnel into the %s.",name);
			more = TRUE;
		}
	}

	/* Result */
	return (more);
}

/*
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Tunnel */
	if (easy_open)
	{
		/* Handle a single open door */
		if (count_feats(&y, &x, FS_TUNNEL) == 1)
		{
			/* Don't close door player is on */
			if ((y != py) || (x != px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Hack -- Apply stuck */
	stuck_player(&dir);

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Oops */
	if (!do_cmd_test(y, x, FS_TUNNEL)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Walls */
	else
	{
		/* Tunnel through walls */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}

/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int y, int x, bool disarm)
{
	int i, j, power;

	cptr name, act;

	bool more = FALSE;

	/* Arm or disarm */
	if (disarm) act = "disarm";
	else act = "arm";

	/* Verify legality */
	if (!do_cmd_test(y, x, (disarm ? FS_DISARM : FS_TRAP))) return (FALSE);

	/* Get the trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = f_info[cave_feat[y][x]].power;

	/* Player trap */
	if (cave_o_idx[y][x])
	{
		/* Use object level instead */
		power = k_info[o_list[cave_o_idx[y][x]].k_idx].level;
	}

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (rand_int(100) < j)
	{
		/* Message */
		msg_format("You have %sed the %s.", act, name);

		/* Reward */
		gain_exp(power);

		/* Remove the trap */
		if (disarm)
		{
			/* Remove the trap */
			cave_alter_feat(y, x, FS_DISARM);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
		}
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to %s the %s.", act, name);

		/* We may keep trying */
		more = TRUE;

		/* Remove the trap */
		if (!disarm)
		{
			/* Remove the trap */
			cave_alter_feat(y, x, FS_DISARM);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
		}
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", name);

		/* Hit the trap */
		hit_trap(y, x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap
 */
void do_cmd_disarm(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Disarm */
	if (easy_open)
	{
		/* Handle a single visible trap or trapped chest */
		if (count_feats(&y, &x, FS_DISARM) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_DISARM)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply stuck / confusion */
	if (stuck_player(&dir) || confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(y, x, TRUE);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}



/*
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x)
{
	int bash, temp;

	int feat;

	cptr name;

	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_test(y, x,FS_BASH)) return (FALSE);

	/* Get mimiced feature */
	feat = f_info[cave_feat[y][x]].mimic;

	/* Get the name */
	name = (f_name + f_info[feat].name);

	/* Message */
	msg_format("You smash into the %s!",name);

	/* Trapped door */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}


	/* Secrets on door/permanent doors */
	else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{
		/* Stuck */
		find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = f_info[cave_feat[y][x]].power;

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Message */
		msg_format("The %s crashes open!",name);

		/* Break down the door */
		if (rand_int(100) < 50)
		{
			cave_alter_feat(y, x, FS_BASH);
		}

		/* Open the door */
		else
		{
			cave_alter_feat(y, x, FS_OPEN);
		}

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(SOUND_OPENDOOR);

	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
		 p_ptr->lev)
	{
		/* Message */
		msg_format("The %s holds firm.",name);

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		/* Hack -- Lose balance ala paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + rand_int(2));
	}

	/* Result */
	return (more);
}


/*
 * Bash open a door, success based on character strength
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

#ifdef ALLOW_EASY_OPEN

	/* Easy Bash */
	if (easy_open)
	{
		/* Handle a single visible trap */
		if (count_feats(&y, &x, FS_BASH)==1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_BASH)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (stuck_player(&dir) || confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Bash the door */
		if (!do_cmd_bash_aux(y, x))
		{
			/* Cancel repeat */
			disturb(0, 0);
		}
	}
}



/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool more = FALSE;


	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Hack -- Apply stuck */
	stuck_player(&dir);

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Original feature */
	feat = cave_feat[y][x];

	/* Get mimiced feature */
	feat = f_info[feat].mimic;

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) feat = FEAT_NONE;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	/* Disarm traps */
	else if (f_info[feat].flags1 & (FF1_DISARM))
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(y, x, TRUE);
	}

	/* Open closed doors */
	else if (f_info[feat].flags1 & (FF1_OPEN))
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}
#if 0
	/* Bash jammed doors */
	else if (f_info[feat].flags1 & (FF1_BASH))
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}
#endif
	/* Tunnel through walls */
	else if (f_info[feat].flags1 & (FF1_TUNNEL))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

#if 0

	/* Close open doors */
	else if (f_info[feat].flags1 & (FF1_CLOSE))
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}

#endif

	/* Oops */
	else
	{
		/* Oops */
		msg_print("You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Hook to determine if an object is good to throw
 */
static bool item_tester_hook_throwing(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_THROWING)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/*
 * Hack -- Set a trap or jam a door closed with a spike.
 *
 * This command may not be repeated.
 *
 * See pick_trap for how traps are chosen, and hit_trap and mon_hit_trap for what
 * player set traps will do.
 */
void do_cmd_set_trap_or_spike(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir, item, action;

	object_type *o_ptr;

	cptr q,s;

	/* Get an item */
	q = "Spike/Set trap with which item? ";
	s = "You have nothing to set a trap or spike with.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the feature */
	if (item >= INVEN_TOTAL+1)
	{
		object_type object_type_body;

		o_ptr = &object_type_body;

		if (!make_feat(o_ptr, cave_feat[p_ptr->py][p_ptr->px])) return;
	}

	/* Get the item (in the pack) */
	else if (item >= 0)
        {

		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Spiking or setting trap? */
	if (o_ptr->tval == TV_SPIKE)
	{
		/* We are spiking */
		action = FS_SPIKE;
	}
	else
	{
		/* Hack -- only set traps on floors (at this stage) XXX */
		action = FS_FLOOR;
	}

#ifdef ALLOW_EASY_OPEN

	/* Easy Bash */
	if (easy_open)
	{
		/* Handle a single visible trap */
		if (count_feats(&y, &x, action)==1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, action)) return;


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;


	/* Apply stuck / confusion */
	if (stuck_player(&dir) || confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}


	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Go for it */
	else
	{
		/* Verify legality */
		if (!do_cmd_test(y, x, action)) return;


		/* Trapped door */
		if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
		{
			hit_trap(y,x);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}


		/* Secrets on door/permanent doors */
		else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
			(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
		{
			/* Stuck */
			find_secret(y,x);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Spike the door */
		else if (action == FS_SPIKE)
		{
			cave_alter_feat(y,x,FS_SPIKE);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Destroy the feature */
			if (item >= INVEN_TOTAL+1)
			{
				cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
			}
			/* Destroy a spike in the pack */
			else if (item >= 0)
			{
				inven_item_increase(item, -1);
				inven_item_describe(item);
				inven_item_optimize(item);
			}

			/* Destroy a spike on the floor */
			else
			{
				floor_item_increase(0 - item, -1);
				floor_item_describe(0 - item);
				floor_item_optimize(0 - item);
			}
		}

		/* Set the trap */
		/* We only let the player set traps when there are no existing
               objects in the grid OR the existing objects in the grid have
               the same tval and sval as the trap being set OR the trap
               being set is TV_BOW and all the objects in the grid can be fired
               by the bow in question */
		else
		{
			int this_o_idx, next_o_idx;

			bool trap_allowed = TRUE;

			object_type object_type_body;

			object_type *j_ptr;

			/* Get object body */
			j_ptr = &object_type_body;

			/* Structure Copy */
			object_copy(j_ptr, o_ptr);

			/* Set one object only */
			j_ptr->number = 1;

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *i_ptr;

				/* Get the object */
				i_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = i_ptr->next_o_idx;

				/* Check if fired */
				if (j_ptr->tval == TV_BOW)
				{
					switch(j_ptr->sval)
					{
						case SV_LONG_BOW:
						case SV_SHORT_BOW:
						{
							if (i_ptr->tval != TV_ARROW) trap_allowed = FALSE;
							break;
						}
						case SV_LIGHT_XBOW:
						case SV_HEAVY_XBOW:
						{
							if (i_ptr->tval != TV_BOLT) trap_allowed = FALSE;
							break;
						}
						default:
						{
							if (!item_tester_hook_throwing(i_ptr)) trap_allowed = FALSE;
							break;
						}
					}
				}
				else if ((j_ptr->tval != i_ptr->tval) || (j_ptr->sval != i_ptr->sval))
				{
					/* Not allowed */
					trap_allowed = FALSE;
				}
			}

			/* Trap allowed? */
			if ((trap_allowed) && (floor_carry(y,x,j_ptr)))
			{
				/* Hack -- ensure trap is created */
				object_level = 128;

				/* Set the floor trap */
				cave_set_feat(y,x,FEAT_INVIS);

				/* Set the trap */
				pick_trap(y,x);

				/* Reset object level */
				object_level = p_ptr->depth;

				/* Check if we can arm it? */
				do_cmd_disarm_aux(y,x, FALSE);
	
				/* Destroy the feature */
				if (item >= INVEN_TOTAL+1)
				{
					cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
				}
				/* Destroy a food in the pack */
				else if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Destroy a food on the floor */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}
}


static bool do_cmd_walk_test(int y, int x)
{
	int feat;

	cptr name; 

	/* Get feature */
	feat = cave_feat[y][x];

	/* Get mimiced feature */
	feat = f_info[feat].mimic;

	/* Get the name */
	name = (f_name + f_info[feat].name);

	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Hack -- walking allows attacking XXX XXX */
	if (cave_m_idx[y][x] >0) return (TRUE);


	/* Player can not walk through "walls" */
	/* Also cannot climb over unknown "trees/rubble" */
	if (!(f_info[feat].flags1 & (FF1_MOVE))
	&& (!(f_info[feat].flags3 & (FF3_EASY_CLIMB))
	|| !(cave_info[y][x] & (CAVE_MARK))))
	{
#ifdef ALLOW_EASY_ALTER

		if (easy_alter) {

			if (f_info[feat].flags1 & (FF1_BASH)) return(TRUE);
			if (f_info[feat].flags1 & (FF1_OPEN)) return(TRUE);
		}

#endif /* ALLOW_EASY_ALTER */

		/* Message */
		msg_format("There is a %s in the way.",name);

		/* Nope */
		return (FALSE);

	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int jumping)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Take time */
	if ((variant_fast_moves) && !(p_ptr->searching)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Hack -- handle stuck players */
	if (stuck_player(&dir))
	{
		/* Get the mimiced feature */
		int mimic = f_info[cave_feat[py][px]].mimic;

		/* Get the feature name */
		cptr name = (f_name + f_info[mimic].name);

		/* Tell the player */
		msg_format("You are stuck %s%s.",
			((f_info[mimic].flags2 & (FF2_FILLED)) ? "" :
				(is_a_vowel(name[0]) ? "inside an " : "inside a ")),name);
	}

	/* Apply confusion */
	else if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

	}


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Move the player */
	move_player(dir, jumping);
}


/*
 * Walk into a grid.
 */
void do_cmd_walk(void)
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}


/*
 * Jump into a grid.
 */
void do_cmd_jump(void)
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Hack -- handle stuck players */
	if (stuck_player(&dir))
	{
		int mimic = f_info[cave_feat[py][px]].mimic;

		/* Get the feature name */
		cptr name = (f_name + f_info[mimic].name);

		/* Use up energy */
		p_ptr->energy_use = 100;

		/* Tell the player */
		msg_format("You are stuck %s%s.",
			((f_info[mimic].flags2 & (FF2_FILLED)) ? "" :
				(is_a_vowel(name[0]) ? "inside an " : "inside a ")),name);
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Start run */
	run_step(dir);
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take time */
	if ((variant_fast_moves) && !(p_ptr->searching)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Catch breath */
	if (!(f_ptr->flags2 & (FF2_FILLED)))
	{
		/* Rest the player */
		set_rest(p_ptr->rest + PY_REST_RATE - p_ptr->tiring);
	}

	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}

	/* Handle "objects" */
	py_pickup(pickup);

	/* Hack -- enter a store if we are on one */
	if (f_info[cave_feat[py][px]].flags1 & (FF1_ENTER))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->energy_use = 0;
	}
}


/*
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(always_pickup);
}


/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */
	do_cmd_hold_or_stay(!always_pickup);
}


/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Get the feature */
	feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 5)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			p_ptr->command_arg = (-1);
		}

		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0) return;
		}
	}


	/* Paranoia */
	if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

	/* Catch breath */
	if (!(f_ptr->flags2 & (FF2_FILLED)))
	{
		/* Rest the player */
		set_rest(p_ptr->rest + PY_REST_RATE * 2 - p_ptr->tiring);
	}

	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = 100;

	/* Save the rest code */
	p_ptr->resting = p_ptr->command_arg;

	/* Cancel the arg */
	p_ptr->command_arg = 0;

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh XXX XXX XXX */
	if (fresh_before) Term_fresh();
}






/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
int breakage_chance(object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_SPELL:
		case TV_POTION:
		case TV_HOLD:
		case TV_FOOD:
		case TV_JUNK:
		case TV_SKIN:
		case TV_FIGURE:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_BONE:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_ARROW:
		{
			return (35);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		case TV_BODY:
		{
			return (25);
		}
	}

	/* Rarely break */
	return (10);
}

/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Note styles now benefit player.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance, power;

	int style_hit=0;
	int style_dam=0;
	int style_crit=0;
	u32b shoot_style;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;
	object_type object_type_feat;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Require throwing weapon */
	if (!item_tester_tval) item_tester_hook = item_tester_hook_throwing;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATG))) return;

	/* Get the feature */
	if (item >= INVEN_TOTAL+1)
	{
		o_ptr = &object_type_feat;

		if (!make_feat(o_ptr, cave_feat[p_ptr->py][p_ptr->px])) return;
	}
	/* Get the object */
	else if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Hack -- if no bow, make object count for double */
	if (j_ptr->tval != TV_BOW) j_ptr = o_ptr;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Check usage */
	object_usage(INVEN_BOW);

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Single object */
	i_ptr->number = 1;

	/* Reset stack counter */
	i_ptr->stackc = 0;

	/* Sometimes use lower stack object */
	if (!object_known_p(o_ptr) && (rand_int(o_ptr->number)< o_ptr->stackc))
	{
		if (i_ptr->pval) i_ptr->pval--;

		if (i_ptr->timeout) i_ptr->timeout = 0;

		o_ptr->stackc--;
	}

	/* Forget information on dropped object */
	drop_may_flags(i_ptr);

	/* Get the feature */
	if (item >= INVEN_TOTAL+1)
	{
		cave_alter_feat(p_ptr->py,p_ptr->px,FS_GET_FEAT);
	}

	/* Reduce and describe inventory */
	else if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Sound */
	sound(MSG_SHOOT);


	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Check shooting styles only */
	shoot_style = p_ptr->cur_style & (WS_SHOOT_FLAGS);

	/*** Handle styles ***/
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		/* Check for styles */
		if ((w_info[i].styles==0) || (w_info[i].styles & (shoot_style & (1L << p_ptr->pstyle))))
		{
			switch (w_info[i].benefit)
			{

				case WB_HIT:
					style_hit += (p_ptr->lev - w_info[i].level) /2;
					break;

				case WB_DAM:
					style_dam += (p_ptr->lev - w_info[i].level) /2;
					break;

				case WB_CRITICAL:
					style_crit++;
					break;
			}
		}

	}



	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds);

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h + style_hit);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range XXX XXX */
	tdis = 10 + 5 * tmul;


	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = (50 / thits);
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = (50 / thits);
	else p_ptr->energy_use = (100 / thits);

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(py, px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_fire(chance2, r_ptr->ac * (r_ptr->flags2 & (RF2_ARMOR) ? 2 : 1), m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
				    (r_ptr->flags2 & (RF2_STUPID)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);

				/* Apply critical damage */
				tdam = critical_shot(i_ptr->weight, (i_ptr->to_h + j_ptr->to_h + style_crit *30), tdam);

				/* Apply launcher and missile bonus */
				tdam += i_ptr->to_d + j_ptr->to_d + style_dam;

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
						   tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
							       "%^s flees in terror!", m_name);
					}

					/* Get item effect */
					get_spell(&power, "use", i_ptr, FALSE);

					/* Has a power */
					/* Always apply powers if ammunition */
					if (power > 0)
					{
						spell_type *s_ptr = &s_info[power];

						int ap_cnt;

						/* Object is used */
						if (k_info[i_ptr->k_idx].used < MAX_SHORT) k_info[i_ptr->k_idx].used++;

						/* Scan through all four blows */
						for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
						{
							int damage = 0;

							/* Extract the attack infomation */
							int effect = s_ptr->blow[ap_cnt].effect;
							int method = s_ptr->blow[ap_cnt].method;
							int d_dice = s_ptr->blow[ap_cnt].d_dice;
							int d_side = s_ptr->blow[ap_cnt].d_side;
							int d_plus = s_ptr->blow[ap_cnt].d_plus;

							/* Hack -- no more attacks */
							if (!method) break;

							/* Mega hack -- dispel evil/undead objects */
							if (!d_side)
							{
								d_plus += 25 * d_dice;
							}

							/* Roll out the damage */
							if ((d_dice) && (d_side))
							{
								damage = damroll(d_dice, d_side) + d_plus;
							}
							else
							{
								damage = d_plus;
							}

							(void)project_m(-1,0,y,x,damage, effect);
							(void)project_f(-1,0,y,x,damage, effect);
						}
					}
				}

				/* Check usage */
				object_usage(item);
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}



/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void do_cmd_throw(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int mul, div;
	int power;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;
	object_type object_type_feat;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATG))) return;

	/* Get the object */
	if (item >= INVEN_TOTAL+1)
	{
		o_ptr = &object_type_feat;

		if (!make_feat(o_ptr, cave_feat[p_ptr->py][p_ptr->px])) return;
	}
	else if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Single object */
	i_ptr->number = 1;

	/* Reset stack count*/
	i_ptr->stackc = 0;

	/* Sometimes use lower stack object */
	if (!object_known_p(o_ptr) && (rand_int(o_ptr->number)< o_ptr->stackc))
	{
		if (i_ptr->pval) i_ptr->pval--;

		if (i_ptr->timeout) i_ptr->timeout = 0;

		o_ptr->stackc--;
	}

	/* Forget information on dropped object */
	drop_may_flags(i_ptr);

	/* Reduce and describe inventory */
	if (item >= INVEN_TOTAL+1)
	{
		cave_alter_feat(p_ptr->py,p_ptr->px,FS_GET_FEAT);
	}
	else if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Description */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds);

	/* Chance of hitting */
	chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));

	/* Take a (partial) turn */
	if ((variant_fast_floor) && (item < 0)) p_ptr->energy_use = 50;
	else if ((variant_fast_equip) && (item >= INVEN_WIELD)) p_ptr->energy_use = 50;
	else p_ptr->energy_use = 100;

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(py, px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2, r_ptr->ac * (r_ptr->flags2 & (RF2_ARMOR) ? 2 : 1), m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
				    (r_ptr->flags2 & (RF2_STUPID)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);

				/* Apply critical damage */
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

				/* Apply launcher and missile bonus */
				tdam += i_ptr->to_d;

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
						   tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
							       "%^s flees in terror!", m_name);
					}

					/* Get item effect */
					get_spell(&power, "use", i_ptr, FALSE);

					/* Has a power */
					/* Always apply powers if ammunition */
					if (power > 0)
					{
						spell_type *s_ptr = &s_info[power];

						int ap_cnt;

						/* Object is used */
						if (k_info[i_ptr->k_idx].used < MAX_SHORT) k_info[i_ptr->k_idx].used++;

						/* Scan through all four blows */
						for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
						{
							int damage = 0;

							/* Extract the attack infomation */
							int effect = s_ptr->blow[ap_cnt].effect;
							int method = s_ptr->blow[ap_cnt].method;
							int d_dice = s_ptr->blow[ap_cnt].d_dice;
							int d_side = s_ptr->blow[ap_cnt].d_side;
							int d_plus = s_ptr->blow[ap_cnt].d_plus;

							/* Hack -- no more attacks */
							if (!method) break;

							/* Mega hack -- dispel evil/undead objects */
							if (!d_side)
							{
								d_plus += 25 * d_dice;
							}

							/* Roll out the damage */
							if ((d_dice) && (d_side))
							{
								damage = damroll(d_dice, d_side) + d_plus;
							}
							else
							{
								damage = d_plus;
							}

							(void)project_m(-1,0,y,x,damage, effect);
							(void)project_f(-1,0,y,x,damage, effect);
						}
					}
				}

				/* Check usage */
				object_usage(item);

			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}


