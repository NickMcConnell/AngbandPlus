/* File: spells1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#define MAX_DAMAGE	1600


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
void teleport_away(int m_idx, int dis)
{
	int ny, nx, oy, ox, d, i, min;

	bool look = TRUE;

	monster_type *m_ptr = &mon_list[m_idx];

	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				ny = rand_spread(oy, dis);
				nx = rand_spread(ox, dis);
				d = distance(oy, ox, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(ny, nx)) continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave_ff1_match(ny, nx, FF1_GLYPH)) continue;

			/* Require safe terrain */
			if (f_info[cave_feat[ny][nx]].dam_non_native > 0)
			{
				/* Get the race */
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Check nativity */
				if (!is_monster_native(ny, nx, r_ptr)) continue;
			}

			/* No teleporting into vaults and such */
			/* if (cave_info[ny][nx] & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
	}

	/* Sound */
	sound(MSG_TPOTHER);

	/*the monster should re-evaluate their target*/
	m_ptr->target_y = 0;
	m_ptr->target_x = 0;

	/* Swap the monsters */
	monster_swap(oy, ox, ny, nx);
}

/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 */
void teleport_player(int dis)
{
	int x_location_tables [20];
	int y_location_tables [20];
	int spot_counter = 0;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int d, i, min, y, x;

	bool look = TRUE;

	/* First, take damage from terrain */
	process_player_terrain_damage();

	/* Player could have died. */
	if (p_ptr->is_dead) return;

	/* Minimum distance */
	min = dis / 2;

	/*guage the dungeon size*/
	d = distance(p_ptr->cur_map_hgt, p_ptr->cur_map_wid, 0, 0);

	/*first start with a realistic range*/
	if (dis > d) dis = d;

	/*must have a realistic minimum*/
	if (min > (d * 4 / 10))
	{
		min = (d * 4 / 10);
	}

	/* Look until done */
	while (look)
	{

		/*find the allowable range*/
		int min_y = MAX((py - dis), 0);
		int min_x = MAX((px - dis), 0);
		int max_y = MIN((py + dis), (p_ptr->cur_map_hgt - 1));
		int max_x = MIN((px + dis), (p_ptr->cur_map_wid - 1));

		/* Try several locations */
		for (i = 0; i < 10000; i++)
		{

			/* Pick a location */
			y = rand_range(min_y, max_y);
			x = rand_range(min_x, max_x);
			d = distance(py, px, y, x);
			if ((d <= min) || (d >= dis)) continue;

			/* Require "start" floor space */
			if (!cave_start_bold(y, x)) continue;

			/* No teleporting into vaults and such */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/*don't go over size of array*/
			if (spot_counter < 20)
			{
				x_location_tables[spot_counter] = x;
				y_location_tables[spot_counter] = y;

				/*increase the counter*/
				spot_counter++;
			}

			/*we have enough spots, keep looking*/
			if (spot_counter == 20)
			{
				/* This grid looks good */
				look = FALSE;

				/* Stop looking */
				break;
			}
		}

		/*we have enough random spots*/
		if (spot_counter > 3) break;

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min * 6 / 10;

	}

	i = rand_int(spot_counter);

	/* Mark the location */
	x = x_location_tables[i];
	y = y_location_tables[i];

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	int dis = 0, ctr = 0;

	/* Initialize */
	y = py;
	x = px;

	/* Find a usable location */
	while (1)
	{
		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds_fully(y, x)) break;
		}

		/* Require "start" floor space */
		if (cave_start_bold(y, x)) break;

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}


/*
 * Teleport monster to a grid near the given location.  This function is
 * used in the monster spell "TELE_SELF_TO", to allow monsters both to
 * suddenly jump near the character, and to make them "dance" around the
 * character.
 *
 * Usually, monster will teleport to a grid that is not more than 4
 * squares away from the given location, and not adjacent to the given
 * location.  These restrictions are relaxed if necessary.
 *
 * This function allows teleporting into vaults.
 */
void teleport_towards(int oy, int ox, int ny, int nx)
{
	int y, x;

	int dist;
	int ctr = 0;
	int min = 2, max = 4;

	monster_type *m_ptr = &mon_list[cave_m_idx[oy][ox]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Find a usable location */
	while (TRUE)
	{
		/* Pick a nearby legal location */
		while (TRUE)
		{
			y = rand_spread(ny, max);
			x = rand_spread(nx, max);
			if (in_bounds_fully(y, x)) break;
		}

		/* Consider all empty grids */
		if (cave_empty_bold(y, x))
		{
			/* Ignore dangerous locations */
			if (!cave_no_dam_for_mon(y, x, r_ptr)) continue;

			/* Calculate distance between target and current grid */
			dist = distance(ny, nx, y, x);

			/* Accept grids that are the right distance away. */
			if ((dist >= min) && (dist <= max)) break;
		}

		/* Occasionally relax the constraints */
		if (++ctr > 15)
		{
			ctr = 0;

			max++;
			if (max > 5) min = 0;
		}
	}

	/* Sound (assumes monster is moving) */
	sound(MSG_TPOTHER);

	/* Move monster */
	monster_swap(oy, ox, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}




/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(int who)
{
	byte kind_of_quest = quest_check(p_ptr->depth);

	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

	bool go_up = FALSE;
	bool go_down = FALSE;

	int was_challenged;

	/* Take damage from terrain */
	process_player_terrain_damage();

	/* Dead player? */
	if (p_ptr->is_dead) return;

	if (adult_ironman)
	{
		msg_print("Nothing happens.");
		return;
	}

	if (!p_ptr->depth) go_down = TRUE;

	/*
	 * Fixed quests where the player can't go lower until they finish the quest, or the
	 * bottom of the dungeon.
	 */
	if ((kind_of_quest == QUEST_FIXED) ||
	    (kind_of_quest == QUEST_FIXED_U) ||
	    (p_ptr->depth >= MAX_DEPTH-1))
	{
		go_up = TRUE;
	}

	/*Not fair to fail the quest if the monster teleports the player off*/
	if ((who >= SOURCE_MONSTER_START) &&
		((kind_of_quest == QUEST_MONSTER) ||
		 (kind_of_quest == QUEST_UNIQUE) ||
		 (kind_of_quest == QUEST_PIT) ||
		 (kind_of_quest == QUEST_NEST) ||
		 (kind_of_quest == QUEST_THEMED_LEVEL)))
	{
		/*de-activate the quest*/
		q_ptr->started = FALSE;

		go_up = TRUE;

	}

	/* Same as above, but don't re-set the quest if the player already has the quest chest*/
	if ((who >= SOURCE_MONSTER_START) && (kind_of_quest == QUEST_VAULT))
	{
		/* Player is holding the quest item?  If so, player can go up or down*/
		if (quest_item_slot() == -1)
		{
			q_ptr->started = FALSE;
		}

		/*
		 * We found the item, but go up.
		 */
		else go_up = TRUE;
	}

	/*We don't have a direction yet, pick one at random*/
	if ((!go_up) && (!go_down))
	{
		if (one_in_(2)) go_up = TRUE;
		else go_down = TRUE;
	}

	/*up*/
	if (go_up == TRUE)
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;

	}

	else
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		was_challenged = challenged();
				
		p_ptr->depth++;

		potential_effect_on_stats();

		if (!was_challenged && challenged()){
			msg_print("You feel more challenged now.");
		}

		regenmana(100);

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

}



/*
 * Draw some projections in multi-hued colors.
 * -TY-, -EB-
 */
static byte mh_attr(void)
{
	switch (randint(8))
	{
		case 1:  return (TERM_RED);
		case 2:  return (TERM_GREEN);
		case 3:  return (TERM_BLUE);
		case 4:  return (TERM_YELLOW);
		case 5:  return (TERM_ORANGE);
		case 6:  return (TERM_L_RED);
		case 7:  return (TERM_L_GREEN);
		case 8:  return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte acid_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_SLATE);
		case 2: return (TERM_L_DARK);
	}
	return (TERM_WHITE);
}

static byte elec_color(void)
{
	byte base;

	switch (rand_int(3))
	{
		case 0: case 1: base = (TERM_BLUE); break;
		default: base = (TERM_L_BLUE); break;
	}

	if (one_in_(4)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte fire_color(void)
{
	byte base;

	switch (rand_int(3))
	{
		case 0: case 1: base = (TERM_RED); break;
		default: base = (TERM_L_RED); break;
	}

	if (one_in_(4)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte cold_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_WHITE);
	}
	return (TERM_WHITE);
}

static byte pois_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_GREEN);
		case 2: return (TERM_L_GREEN);
	}
	return (TERM_WHITE);
}

static byte plasma_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte ice_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte lite_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_ORANGE);
	}

	return (TERM_WHITE);
}

static byte confu_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_L_UMBER);
		case 3: return (TERM_UMBER);
		case 4: return (TERM_WHITE);
	}

	return (TERM_WHITE);
}

static byte grav_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_DARK);
		case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_WHITE);
}

static byte meteor_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: return (TERM_L_DARK);
		case 2: return (TERM_WHITE);
		case 3: return (TERM_RED);
		case 4: return (TERM_ORANGE);
		case 5: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte orb_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_L_DARK);
}

static byte mana_color(void)
{
	byte base;

	switch (rand_int(2))
	{
		case 1: base = (TERM_BLUE); break;
		default: base = (TERM_RED); break;
	}

	if (one_in_(2)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte fog_color(void)
{
	switch (rand_int(5))
	{
		case 0: return (TERM_VIOLET);
		case 1: return (MAKE_EXTENDED_COLOR(TERM_VIOLET, 1));
	}

	return (TERM_SLATE);
}

static byte bwater_color(void)
{
	if (one_in_(3)) return (TERM_L_BLUE);

	return (TERM_WHITE);
}


static byte lava_color(void)
{
	if (one_in_(4)) return (TERM_L_RED);

	return (TERM_RED);
}

/*
 * Return a color to use for the bolt/ball spells
 */
byte gf_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_MISSILE:	return (TERM_VIOLET);
		case GF_ACID:		return (acid_color());
		case GF_ELEC:		return (elec_color());
		case GF_FIRE:		return (fire_color());
		case GF_COLD:		return (cold_color());
		case GF_POIS:		return (pois_color());
		case GF_HOLY_ORB:	return (orb_color());
		case GF_MANA:		return (mana_color());
		case GF_STATIC:		return (TERM_WHITE);
		case GF_ARROW:		return (TERM_WHITE);
		case GF_WATER:		return (TERM_SLATE);
		case GF_EXTINGUISH:	return (TERM_BLUE);
		case GF_CLEAR_AIR:	return (TERM_WHITE);
		case GF_NETHER:		return (TERM_L_GREEN);
		case GF_CHAOS:		return (mh_attr());
		case GF_DISENCHANT:	return (TERM_VIOLET);
		case GF_STERILIZE:	return (TERM_YELLOW);
		case GF_NEXUS:		return (TERM_L_RED);
		case GF_CONFUSION:	return (confu_color());
		case GF_SOUND:		return (TERM_YELLOW);
		case GF_SHARD:		return (TERM_UMBER);
		case GF_FORCE:		return (TERM_UMBER);
		case GF_INERTIA:	return (TERM_L_WHITE);
		case GF_GRAVITY:	return (grav_color());
		case GF_TIME:		return (TERM_L_BLUE);
		case GF_LITE_WEAK:	return (lite_color());
		case GF_LITE:		return (lite_color());
		case GF_DARK_WEAK:	return (TERM_L_DARK);
		case GF_DARK:		return (TERM_L_DARK);
		case GF_PLASMA:		return (plasma_color());
		case GF_METEOR:		return (meteor_color());
		case GF_ICE:		return (ice_color());
		case GF_MASS_IDENTIFY:	return (TERM_WHITE);
		case GF_SMOKE:		return (TERM_L_DARK);
		case GF_FOG:		return (fog_color());
		case GF_SAND:		return (TERM_YELLOW);
		case GF_BMUD:		return (TERM_ORANGE);
		case GF_BWATER:		return (bwater_color());
		case GF_LAVA:		return (lava_color());
		case GF_LIFE_DRAIN:  return (TERM_VIOLET);
	}

	/* Standard "color" */
	return (TERM_WHITE);
}



/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
	int base;

	byte k;

	byte a;
	char c;

	if (!(use_graphics && (arg_graphics == GRAPHICS_DAVID_GERVAIS)))
	{
		/* No motion (*) */
		if ((ny == y) && (nx == x)) c = '*';

		/* Vertical (|) */
		else if (nx == x) c = '|';

		/* Horizontal (-) */
		else if (ny == y) c = '-';

		/* Diagonal (/) */
		else if ((ny-y) == (x-nx)) c = '/';

		/* Diagonal (\) */
		else if ((ny-y) == (nx-x)) c = '\\';

		/* Weird (*) */
		else c = '*';

		/* Basic spell color */
		a = gf_color(typ);

	}
	else
	{
		int add;

		/* No motion (*) */
		if ((ny == y) && (nx == x)) {base = 0x00; add = 0;}

		/* Vertical (|) */
		else if (nx == x) {base = 0x40; add = 0;}

		/* Horizontal (-) */
		else if (ny == y) {base = 0x40; add = 1;}

		/* Diagonal (/) */
		else if ((ny-y) == (x-nx)) {base = 0x40; add = 2;}

		/* Diagonal (\) */
		else if ((ny-y) == (nx-x)) {base = 0x40; add = 3;}

		/* Weird (*) */
		else {base = 0x00; add = 0;}

		if (typ >= 0x40) k = 0;
		else k = typ;

		/* Obtain attr/char */
		a = misc_to_attr[base+k];
		c = misc_to_char[base+k] + add;
	}

	/* Create pict */
	return (PICT(a,c));
}


/*
 * Hurt the player (maybe killing him/her). The damage comes from the given
 * feature. kb_str is the cause of the death.
 */
void take_terrain_hit(int dam, int feat, cptr kb_str)
{
	char name[80];

	cptr action = "hurts";

	/* Default spell type, it should never happen */
	byte gf_type = GF_MANA;

	/* Get the feature */
	feature_type *f_ptr = &f_info[feat];
	feature_lore *f_l_ptr = &f_l_list[feat];

	/*Paranoia*/
	if (dam == 0) return;

	/*Count the number of times this damage has been felt*/
	if (f_l_ptr->f_l_dam_non_native < MAX_UCHAR) f_l_ptr->f_l_dam_non_native ++;

	/* Take the worst damage from the feature */
	if ((_feat_ff3_match(f_ptr, TERRAIN_MASK) == ELEMENT_BWATER) &&
		((p_ptr->p_native & ELEMENT_BWATER) != ELEMENT_BWATER))
	{
		gf_type = GF_BWATER;
		action = "burns";
	}
	else if ((_feat_ff3_match(f_ptr, TERRAIN_MASK) == ELEMENT_BMUD) &&
		((p_ptr->p_native & ELEMENT_BMUD) != ELEMENT_BMUD))
	{
		gf_type = GF_BMUD;
		action = "burns";
	}
	else if (_feat_ff3_match(f_ptr, FF3_ICE) && !(p_ptr->p_native & P_NATIVE_ICE))
	{
		gf_type = GF_ICE;
		action = "freezes";
	}
	else if (_feat_ff3_match(f_ptr, FF3_LAVA) && !(p_ptr->p_native & P_NATIVE_LAVA))
	{
		gf_type = GF_LAVA;
		action = "burns";
	}
	else if (_feat_ff3_match(f_ptr, FF3_FIRE) && !(p_ptr->p_native & P_NATIVE_FIRE))
	{
		gf_type = GF_FIRE;
		action = "burns";
	}
	else if (_feat_ff3_match(f_ptr, FF3_ACID) && !(p_ptr->p_native & P_NATIVE_ACID))
	{
		gf_type = GF_ACID;
	}
	else if (_feat_ff3_match(f_ptr, FF3_WATER) && !(p_ptr->p_native & P_NATIVE_WATER))
	{
		gf_type = GF_WATER;
	}

	/* Ignore certain spell types if the player is immune to them */
	if (is_player_immune(gf_type)) return;

	/* Show a message */
	if (!p_ptr->blind)
	{
		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, FALSE, TRUE);

		msg_format("The %s %s you!", name, action);
	}

	/* Take the hit */
	(void)project_p(SOURCE_OTHER, p_ptr->py, p_ptr->px, dam, gf_type, kb_str);
}


/*
 * Decreases players hit points and sets death flag if necessary
 *
 * Invulnerability needs to be changed into a "shield" XXX XXX XXX
 *
 * Hack -- this function allows the user to save (or quit) the game
 * when he dies, since the "You die." message is shown before setting
 * the player to "dead".
 */
void take_hit(int dam, cptr kb_str)
{
	int old_chp = p_ptr->chp;

	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);

	/* Paranoia */
	if (p_ptr->is_dead) return;

	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (dam < 9000)) return;

	if ((dam > p_ptr->mhp/6) && (dam > p_ptr->chp) && (p_ptr->chp > 1) && (dam < 2*p_ptr->mhp)){
		if (randint(100)<=adj_luc_survive[p_ptr->stat_ind[A_LUC]]){
			dam = p_ptr->chp / 2;
			p_ptr->redraw |= (PR_STATS);
			new_dec_stat(A_LUC,randint(3)+2,1);
			msg_print("You feel you have had a lucky escape!");
			if (adult_take_notes){
		    	char note[120];
		    	char *fmt = "Was lucky to escape death.";

		    	strnfmt(note, sizeof(note), fmt);

		    	do_cmd_note(note, p_ptr->depth);
			}
		}
	}

	/* Hurt the player */
	p_ptr->chp -= dam;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Hack -- Note death */
		message(MSG_DEATH, 0, "You die.");
		message_flush();

		/* Note cause of death */
		my_strcpy(p_ptr->died_from, kb_str, sizeof(p_ptr->died_from));

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Write a note */
		if (adult_take_notes)
		{
			time_t ct = time((time_t*)0);
			char long_day[25];
			char buf[120];

 		  	/* Get time */
 		  	(void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));

 		  	/* Add note */

		  	fprintf(notes_file, "============================================================\n");

			/*killed by */
 		  	sprintf(buf, "Killed by %s.", p_ptr->died_from);

			/* Write message */
            do_cmd_note(buf,  p_ptr->depth);

			/* date and time*/
			sprintf(buf, "Killed on %s.", long_day);

			/* Write message */
            do_cmd_note(buf,  p_ptr->depth);

			fprintf(notes_file, "============================================================\n");

		}

		/* Dead */
		return;
	} else if (p_ptr->chp==0){
		msg_format("You are on the verge of death!");
		if (adult_take_notes){
		    	char note[120];
		    	char *fmt = "Came close to death.";

		    	strnfmt(note, sizeof(note), fmt);

		    	do_cmd_note(note, p_ptr->depth);
		}
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning)
		{
			bell("Low hitpoint warning!");
		}

		/* Message */
		message(MSG_HITPOINT_WARN, 0, "*** LOW HITPOINT WARNING! ***");
		message_flush();
	}
}





/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
static bool hates_acid(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Scrolls are paper */
		case TV_SCROLL:
		{
			return (TRUE);
		}

		/* Ouch */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Junk is useless */
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
static bool hates_elec(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object hate electricity in really big doses?
 */
static bool dislikes_elec(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		case TV_WAND:
		{
			return (TRUE);
		}
		case TV_TALISMAN:
		{
			return (TRUE);
		}

	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
static bool hates_fire(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
		case TV_LITE:
		case TV_ARROW:
		case TV_BOW:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Chests */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Scrolls burn */
		case TV_SCROLL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object hate fire in really big doses?
 */
static bool dislikes_fire(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
		case TV_MAGIC_BOOK:
		{
			return (TRUE);
		}

	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate cold?
 */
static bool hates_cold(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_POTION:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given class of objects (usually) hate sand?
 */
static bool hates_sand(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Destroys Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate boiling mud?
 */
static bool hates_boiling_mud(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{

		/* Scrolls get destroyed */
		case TV_SCROLL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate boiling mud?
 */
static bool hates_boiling_water(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{

		/* Potions and flasks get destroyed */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate lava?
 */
static bool hates_lava(const object_type *o_ptr)
{

	/*Most everything*/
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_RING:
		case TV_WAND:
		case TV_ROD:
		case TV_LITE:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
		case TV_CHEST:
		case TV_TALISMAN:
		case TV_SCROLL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Return TRUE if the object can't exist in the given location
 */
bool hates_location(int y, int x, const object_type *o_ptr)
{
	/* Get the element flags of the location */
	u32b terrain_native = cave_ff3_match(y, x, TERRAIN_MASK);

	u32b f1, f2, f3, fn;

	/* Get object flags (we'll check object resistance) */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/*Artifacts are comfortable anywhere*/
	if (artifact_p(o_ptr)) return (FALSE);

	/*Check if object is native*/
	if (fn)
	{
		u32b filtered_native = terrain_native;

		/*Strip out unneeded ones*/
		filtered_native &= fn;

		/*if we have a match, the object is comfortable at the location*/
		if (fn == filtered_native) return (FALSE);
	}

	/* Check fire and lava */
	if ((terrain_native & (ELEMENT_FIRE | ELEMENT_LAVA)) &&
		!(f3 & (TR3_IGNORE_FIRE)) && (hates_fire(o_ptr) || dislikes_fire(o_ptr)))
	{
		return (TRUE);
	}
	/* Check acid */
	else if ((terrain_native & (ELEMENT_ACID)) &&
		!(f3 & (TR3_IGNORE_ACID)) && hates_acid(o_ptr))
	{
		return (TRUE);
	}
	/* Check ice */
	else if ((terrain_native & (ELEMENT_ICE)) &&
		!(f3 & (TR3_IGNORE_COLD)) && hates_cold(o_ptr))
	{
		return (TRUE);
	}
	/* The object can exist in the grid */
	else
	{
		return (FALSE);
	}
}

/*
 * destroy cursed items
 */
static void holy_orb_destroy(int damage)
{
	int i;

	object_type *o_ptr;

	char o_name[80];

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give any crused item a shot at destruction */
		if ((cursed_p(o_ptr)) && (rand_int(100) < damage))
		{
			int amt = o_ptr->number;

			/* Get a description */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

			/* Message */
			message_format(MSG_DESTROY, 0, "%sour %s (%c) %s destroyed!",
				       ((amt > 1) ? "All of y" : "Y"),
				       o_name,
					   index_to_label(i),
					   ((amt > 1) ? "were" : "was"));

			/* Destroy "amt" items */
			inven_item_increase(i, -amt);
			inven_item_optimize(i);

		}
	}
}




/*
 * Melt something
 */
static int set_acid_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_ACID)) return (FALSE);
	if (fn & (ELEMENT_ACID)) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
static int set_elec_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}

/*
 * Electrical damage can destroy wands and talismans if enough
 */
static int set_elec_destroy_tough(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_elec(o_ptr) && !dislikes_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}

/*
 * Burn something
 */
static int set_fire_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);
	if (fn & (ELEMENT_LAVA)) return (FALSE);
	return (TRUE);
}

/*
 * Fire damage can destroy books if enough
 */
static int set_fire_destroy_tough(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_fire(o_ptr) && !dislikes_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);
	if (fn & (ELEMENT_LAVA)) return (FALSE);
	return (TRUE);
}

/*
 * Freeze things
 */
static int set_cold_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_cold(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_COLD)) return (FALSE);
	if (fn & (ELEMENT_ICE)) return (FALSE);
	return (TRUE);
}

/*
 * Melt something
 */
static int set_lava_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_lava(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (fn & (ELEMENT_LAVA)) return (FALSE);
	return (TRUE);
}

/*
 * Destroy something
 */
static int set_boiling_water_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_boiling_water(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if ((fn & (ELEMENT_BWATER)) == ELEMENT_BWATER) return (FALSE);
	return (TRUE);
}

/*
 * Destroy something
 */
static int set_boiling_mud_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_boiling_mud(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if ((fn & (ELEMENT_BMUD)) == ELEMENT_BMUD) return (FALSE);
	return (TRUE);
}


/* Drain an activation.
 * If the timeout is increased, return true.
 * This function assumes object can be activated
 */
static bool drain_activation(object_type *o_ptr, int time)
{
	s16b old_timeout = o_ptr->timeout;

	/*Add the penalty*/
	o_ptr->timeout += (time / 5);

	/* Limit timeout*/

	/*Artifact*/
	if ((o_ptr->art_num) && (o_ptr->art_num < z_info->art_norm_max))
	{
		artifact_type *a_ptr = &a_info[o_ptr->art_num];

		if (o_ptr->timeout > (a_ptr->time + a_ptr->randtime)) o_ptr->timeout = a_ptr->time + a_ptr->randtime;
	}
	/*Dragon Armor*/
	else if ((o_ptr->tval == TV_DRAG_ARMOR) ||
				(o_ptr->tval == TV_DRAG_SHIELD))
	{
		/*The *2 is the random time element of the dragon armor re-charge*/
		u16b value = o_ptr->sval * 2;

		/*Armor is more powerful than shields*/
		if (o_ptr->tval == TV_DRAG_ARMOR) value *= 2;

		/* Branch on the sub-type */
		switch (o_ptr->ego_num)
		{
			case EGO_DRAGON_BLACK:
			case EGO_DRAGON_BLUE:
			case EGO_DRAGON_WHITE:
			case EGO_DRAGON_GREEN:
			case EGO_DRAGON_RED:
			case EGO_DRAGON_BRONZE:
			case EGO_DRAGON_GOLD:
			{
				value *= 50;

				break;
			}

			case EGO_DRAGON_MULTIHUED:
			case EGO_DRAGON_BALANCE:
			{
				value *= 75;
				break;
			}

			case EGO_DRAGON_CHAOS:
			case EGO_DRAGON_LAW:
			{
				value *= 60;

				break;
			}

			case EGO_DRAGON_PSEUDO:
			{
				value *= 65;
				break;
			}

			case EGO_DRAGON_POWER:
			{
				value *= 100;
				break;
			}
		}

		if (o_ptr->timeout > value) o_ptr->timeout = value;
	}

	/* Hack -- certain Rings - currently all the rings have the same actication timeout*/
	else if (o_ptr->tval == TV_RING)
	{
		if (o_ptr->timeout > 100) o_ptr->timeout = 100;
	}

	/* The timeout was increased*/
	if (o_ptr->timeout > old_timeout) return (TRUE);

	/*else*/
	return (FALSE);
}


/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(const object_type *);

/*
 * Drain the chargable and activable items
 *
 * Returns number of slots drained.
 */
static int inven_drain(int dam)
{
	int i, k;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[80];

	/* Count the casualties */
	k = 0;

	/* Scan through the equipment and slots backwards */
 	for (i = 0; i < INVEN_TOTAL; i++)
	{

		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/*
		 * No messages needed.
		 * We do not notice this with objects sitting on the ground.
		 */
		if (!(item_tester_hook_activate(o_ptr))) continue;

		/*Allow objects a saving throw*/
		if (rand_int((dam / 2)) <= k_ptr->k_level) continue;

		/*Artifacts get another saving throw*/
		if (o_ptr->art_num)
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			if (rand_int((dam / 2)) <= a_ptr->a_level) continue;
		}

		(void)drain_activation(o_ptr, dam);

		/*Count it*/
		k++;

		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Message */
		msg_format("Your %s (%c) %s drained!", o_name, index_to_label(i), ((o_ptr->number > 1) ? "were" : "was"));

	}

	/* Return the casualty count */
	return (k);
}



/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 *
 * Returns number of items destroyed.
 */
static int inven_damage(inven_func typ, int perc, bool protected)
{
	int i, j, k, amt;

	object_type *o_ptr;

	char o_name[80];

	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Damage only inventory and quiver */
		if ((i >= INVEN_PACK) && !IS_QUIVER_SLOT(i)) continue;

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give this item slot a shot at death */
		if ((*typ)(o_ptr))
		{
			int percent = 125;

			if (protected)
			{
				percent *= 5;
				percent /= 3;
			}

			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (rand_int(percent) < perc) amt++;
			}

			/* Some casualities */
			if (amt)
			{
				int old_charges = 0;

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

				/* Message */
				message_format(MSG_DESTROY, 0, "%sour %s (%c) %s destroyed!",
				           ((o_ptr->number > 1) ?
				            ((amt == o_ptr->number) ? "All of y" :
				             (amt > 1 ? "Some of y" : "One of y")) : "Y"),
				           o_name, index_to_label(i),
				           ((amt > 1) ? "were" : "was"));

				/* Destroy "amt" items */
				inven_item_increase(i, -amt);
				inven_item_optimize(i);

				/* Count the casualties */
				k += amt;
			}
		}
	}

	/* Return the casualty count */
	return (k);
}




/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type *o_ptr = NULL;

	u32b f1, f2, f3, fn;

	char o_name[80];


	/* Pick a (possibly empty) inventory slot */
	switch (randint(6))
	{
		case 1: o_ptr = &inventory[INVEN_BODY]; break;
		case 2: o_ptr = &inventory[INVEN_ARM]; break;
		case 3: o_ptr = &inventory[INVEN_OUTER]; break;
		case 4: o_ptr = &inventory[INVEN_HANDS]; break;
		case 5: o_ptr = &inventory[INVEN_HEAD]; break;
		case 6: o_ptr = &inventory[INVEN_FEET]; break;
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return (FALSE);

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Object resists */
	if (f3 & (TR3_IGNORE_ACID))
	{
		msg_format("Your %s is unaffected!", o_name);

		return (TRUE);
	}

	/* Message */
	msg_format("Your %s is damaged!", o_name);

	/* Damage the item */
	o_ptr->to_a--;

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Item was damaged */
	return (TRUE);
}


/*
 * Hurt the player with Acid
 */
void acid_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total Immunity */
	if (p_ptr->immune_acid || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->resist_acid) && (p_ptr->oppose_acid)) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = (dam + 2) / 3;
	if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_acid_destroy, inv, double_resist);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total immunity */
	if (p_ptr->immune_elec || (dam <= 0)) return;

	/*inventory gets protected better with double resistance*/
	if ((p_ptr->resist_elec) && (p_ptr->oppose_elec)) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
	if (p_ptr->resist_elec) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_elec_destroy, inv, double_resist);

	if (dam>=50){
		inv = (dam < 100) ? 1 : (dam < 150) ? 2 : 3;
		inven_damage(set_elec_destroy_tough, inv, double_resist);
	}
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Totally immune */
	if (p_ptr->immune_fire || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->resist_fire) && (p_ptr->oppose_fire)) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = (dam + 2) / 3;
	if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_fire_destroy, inv, double_resist);

	if (dam>=50){
		inv = (dam < 100) ? 1 : (dam < 150) ? 2 : 3;
		inven_damage(set_fire_destroy_tough, inv, double_resist);
	}
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total immunity */
	if (p_ptr->immune_cold || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->resist_cold) && (p_ptr->oppose_cold)) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = (dam + 2) / 3;
	if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_cold_destroy, inv, double_resist);
}


/*
 * Hurt the player with Fire
 */
void lava_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage */
	if (player_native)
	{
		dam /= 9;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_lava_destroy, inv, double_resist);
}

/*
 * Hurt the player with Boiling Water
 */
void boiling_water_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage */
	if (player_native)
	{
		dam /= 9;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_boiling_water_destroy, inv, double_resist);
}

/*
 * Hurt the player with Boiling Mud
 */
void boiling_mud_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage */
	if (player_native)
	{
		dam /= 9;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_boiling_mud_destroy, inv, double_resist);
}


/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat)
{
	int value, gain;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above 18/100 */
	if (value < 18+100)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			value += 1;
			gain = 1;
		}

		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18+98)
		{
			value += 10;
			gain = 10;
			if (value > 18+100){
				gain = gain + value - (18+100);
				value = 18 + 100;
			}
		}

		/* Gain one point at a time */
		else
		{
			value++;
			gain = 1;
		}

		/* Save the new value */
		p_ptr->stat_cur[stat] = value;
	}

	value = p_ptr->stat_max[stat];

	/* Cannot go above 18/100 */
	if (value < 18+100)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			value += 1;
			gain = 1;
		}

		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18+98)
		{
			value += 10;
			gain = 10;
			if (value > 18+100){
				gain = gain + value - (18+100);
				value = 18 + 100;
			}
		}

		/* Gain one point at a time */
		else
		{
			value++;
			gain = 1;
		}

		/* Save the new value */
		p_ptr->stat_max[stat] = value;

		if (p_ptr->stat_max[stat] < p_ptr->stat_cur[stat]){
			p_ptr->stat_max[stat] = p_ptr->stat_cur[stat];
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}


bool new_dec_stat(int stat, int amount, bool permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Get the current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
 	same = (cur == max);

	/* Damage "current" value */
	if (cur > 3)
	{
		/* Handle "low" values */
		if (cur <= 18)
		{
			cur -= amount;
		}

		/* Handle "high" values */
		else
		{
			if (cur >= 18+10*amount){
				cur -= 10*amount;
			} else {
				cur=18-(amount - ((cur-19)/10+1));
			}
		}

		/* Prevent illegal values */
		if (cur < 3) cur = 3;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 3))
	{
		/* Handle "low" values */
		if (max <= 18)
		{
			max-=amount;
		}

		/* Handle "high" values */
		else
		{
			if (max >= 18+10*amount){
				max -= 10*amount;
			} else {
				max=18-(amount - ((max-19)/10+1));
			}
		}

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);
	}

	/* Done */
	return (res);
}


/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, bool permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Get the current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
 	same = (cur == max);

	/* Damage "current" value */
	if (cur > 3)
	{
		/* Handle "low" values */
		if (cur <= 18)
		{
			if (amount > 90) cur--;
			if (amount > 50) cur--;
			if (amount > 20) cur--;
			cur--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount */
			loss = randint(amount/2) + amount/2;

			/* Lose some points */
			cur = cur - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (cur < 18) cur = (amount <= 20) ? 18 : 17;
		}

		/* Prevent illegal values */
		if (cur < 3) cur = 3;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 3))
	{
		/* Handle "low" values */
		if (max <= 18)
		{
			if (amount > 90) max--;
			if (amount > 50) max--;
			if (amount > 20) max--;
			max--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount */
			loss = randint(amount/2) + amount/2;


			/* Lose some points */
			max = max - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (max < 18) max = (amount <= 20) ? 18 : 17;
		}

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);
	}

	/* Done */
	return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}

/*
 * Inflict disease on the character.
 */
void disease(int *damage)
{
	int con, attempts;
	int i;

	/* Get current constitution */
	con = p_ptr->stat_cur[A_CON];

	/* Adjust damage and choose message based on constitution */
	if (con < 8)
	{
		msg_print("You feel deathly ill.");
		*damage *= 2;
	}

	else if (con < 14)
	{
		msg_print("You feel seriously ill.");
	}

	else if (con < 18)
	{
		msg_print("You feel quite ill.");
		*damage = *damage * 2 / 3;
	}

	/* CON is at least 18, and less than 18/50 */
	else if (con < 68)
	{
		msg_print("You feel ill.");
		*damage /= 2;
	}

	/* CON is at least 18/50, and less than 18/100 */
	else if (con < 118)
	{
		msg_print("You feel sick.");
		*damage /= 3;
	}

	/* CON is at least 18/100 */
	else
	{
		msg_print("You feel a bit sick.");
		*damage /= 4;
	}

	/* Infect the character (fully cumulative) */
	if (!(p_ptr->immune_pois)) set_poisoned(p_ptr->poisoned + *damage + 1);

	/* Determine # of stat-reduction attempts */
	attempts = (5 + *damage) / 5;

	/* Attack stats */
	for (i = 0; i < attempts; i++)
	{
		/* Each attempt has a 10% chance of success */
		if (one_in_(10))
		{
			/* Damage a random stat */
			(void)do_dec_stat(rand_int(A_MAX));
		}
	}
}



/*
 * Apply disenchantment to the player's stuff
 *
 * This function is also called from the "melee" code.
 *
 * The "mode" is currently unused.
 *
 * Return "TRUE" if the player notices anything.
 *
 * Also used for sand (with mode TRUE.  False is disenchantment.
 */
bool apply_disenchant(int mode)
{
	int t = 0;

	object_type *o_ptr;

	char o_name[80];

	/* Pick a random slot */
	switch (randint(8))
	{
		case 1: t = INVEN_WIELD; break;
		case 2: t = INVEN_BOW; break;
		case 3: t = INVEN_BODY; break;
		case 4: t = INVEN_OUTER; break;
		case 5: t = INVEN_ARM; break;
		case 6: t = INVEN_HEAD; break;
		case 7: t = INVEN_HANDS; break;
		case 8: t = INVEN_FEET; break;
	}

	/* Get the item */
	o_ptr = &inventory[t];

	/* No item, nothing happens */
	if (!o_ptr->k_idx) return (FALSE);

	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
	{
		/* Nothing to notice */
		return (FALSE);
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Artifacts have 60% chance to resist */
	if (artifact_p(o_ptr) && (rand_int(100) < 60))
	{
		/* Message */
		msg_format("Your %s (%c) resist%s %s!",
		           o_name, index_to_label(t),
		           ((o_ptr->number != 1) ? "" : "s"),
				   ((!mode) ? "disenchantment" : "damage"));

		/* Notice */
		return (TRUE);
	}

	/* Disenchant tohit */
	if (o_ptr->to_h > 0) o_ptr->to_h--;
	if ((o_ptr->to_h > 5) && (rand_int(100) < 20)) o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0) o_ptr->to_d--;
	if ((o_ptr->to_d > 5) && (rand_int(100) < 20)) o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0) o_ptr->to_a--;
	if ((o_ptr->to_a > 5) && (rand_int(100) < 20)) o_ptr->to_a--;

	/* Message */
	msg_format("Your %s (%c) %s %s!",
	           o_name, index_to_label(t),
	           ((o_ptr->number != 1) ? "were" : "was"),
			   ((!mode) ? "disenchanted" : "damaged"));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Notice */
	return (TRUE);
}


/*
 * Apply Nexus
 */
static void apply_nexus(int who)
{

	int x, y, i;

	int max1, cur1, max2, cur2, ii, jj;

	if (who > SOURCE_OTHER)
	{
		/* Source monster */
		monster_type *m_ptr;

		/* Get the source monster */
		m_ptr = &mon_list[who];

		y = m_ptr->fy;
		x = m_ptr->fx;
	}
	else
	{
		y = p_ptr->py;
		x = p_ptr->px;
	}

	while (TRUE)
	{
		i = randint(6);

		/*This is a monster*/
		if (who > SOURCE_MONSTER_START) break;

		/*Hack - don't teleport_player_to if not a monster*/
		if ((i == 4) || (i == 5)) continue;

		/*We are fine*/
		break;

	}


	switch (i)
	{
		case 1: case 2: case 3:
		{
			teleport_player(200);
			break;
		}

		case 4: case 5:
		{

			teleport_player_to(y, x);
			break;
		}

		case 6:
		{
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Teleport Level */
			teleport_player_level(who);
			break;
		}

		case 7:
		{
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
				break;
			}

			msg_print("Your body starts to scramble...");

			/* Pick a pair of stats */
			ii = rand_int(A_MAX);
			for (jj = ii; jj == ii; jj = rand_int(A_MAX)) /* loop */;

			max1 = p_ptr->stat_max[ii];
			cur1 = p_ptr->stat_cur[ii];
			max2 = p_ptr->stat_max[jj];
			cur2 = p_ptr->stat_cur[jj];

			p_ptr->stat_max[ii] = max2;
			p_ptr->stat_cur[ii] = cur2;
			p_ptr->stat_max[jj] = max1;
			p_ptr->stat_cur[jj] = cur1;

			p_ptr->update |= (PU_BONUS);

			break;
		}
	}
}


static int charisma_adjustment(int y, int x)
{
	return 0;
}

/*
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;


/*reveal a mimic, re-light the spot, and print a message if asked for*/
void reveal_mimic(int y, int x, bool message)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

	/*no longer a mimic*/
	m_ptr->mimic_k_idx = 0;

	/* Mimic no longer acts as a detected object */
	m_ptr->mflag &= ~(MFLAG_MIMIC);

	/* Message  XXX */
	if (message) msg_print("There is a mimic!");

	/* Redraw */
	lite_spot(y, x);

	/* Disturb */
	disturb(0, 0);
}

/*
 * Temporarily light a grid.
 *
 * Memorise a monster or terrain if visible.
 *
 * We employ hacks here in order to temporarily make
 * the floor visible.
 *
 */
static bool temp_lite(int y, int x)
{
	/* Grid is in line of sight */
	if (player_has_los_bold(y, x))
	{
		if (!(cave_info[y][x] & (CAVE_SEEN))
		    && !(p_ptr->blind))
		{
			/* Temporarily seen */
			cave_info[y][x] |= (CAVE_SEEN);

			/* Remember? */
			note_spot(y,x);

			/* Temporarily seen */
			cave_info[y][x] &= ~(CAVE_SEEN);

			/* Light? */
			lite_spot(y,x);

			/* Get monster */
			if (cave_m_idx[y][x] > 0 )
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Detect all non-invisible monsters */
				if ((!(r_ptr->flags2 & (RF2_INVISIBLE)) || (p_ptr->see_inv)) &&
					(!(m_ptr->mflag & (MFLAG_HIDE))))
				{
					/* Optimize -- Repair flags */
					repair_mflag_mark = repair_mflag_show = TRUE;

					/* Hack -- Detect the monster */
					m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

					/* Update the monster */
					update_mon(cave_m_idx[y][x], FALSE);
				}
			}
		}

		/* Something seen */
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Apply bonuses/penalties to the damage made by spells, based on terrain.
 * You should supply the native element flags of the target, since we
 * inhibite some increments to damage based on those flags.
 * This function shouldn't be used if the source of the spell is the terrain
 * itself.
 */
static int terrain_adjust_damage(int dam, int spell_type, int y, int x,
	u32b target_native)
{
	/* Get the element flags of the location */
	u32b terrain_element = cave_ff3_match(y, x, TERRAIN_MASK);

	/* Some spells get bonuses/penalties based on terrain */
	switch (spell_type)
	{
		case GF_ACID:
		{
			/* +50% */
			if ((terrain_element & (ELEMENT_ACID)) &&
				!(target_native & (ELEMENT_ACID)))
			{
				dam = 3 * dam / 2;
			}

			break;
		}

		case GF_FIRE:
		case GF_SMOKE:
		{
			/* +75% */
			if ((terrain_element == (ELEMENT_LAVA)) &&
				!(target_native & (ELEMENT_LAVA)))
			{
				dam = 7 * dam / 4;
			}
			/* +50% */
			else if ((terrain_element & (ELEMENT_FIRE | ELEMENT_LAVA)) &&
				!(target_native & (ELEMENT_FIRE | ELEMENT_LAVA)))
			{
				dam = 3 * dam / 2;
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_COLD:
		{
			/* +50% */
			if ((terrain_element & (ELEMENT_ICE)) &&
				!(target_native & (ELEMENT_ICE)))
			{
				dam = 3 * dam / 2;
			}
			/* +25% */
			else if ((terrain_element == (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER | ELEMENT_ICE)))
			{
				dam = 5 * dam / 4;
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_FIRE))
			{
				dam = 3 * dam / 4;
			}
			/* -50% */
			else if (terrain_element & (ELEMENT_LAVA))
			{
				dam /= 2;
			}

			break;
		}

		case GF_ELEC:
		{
			/* +50% */
			if ((terrain_element & (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER)))
			{
				dam = 3 * dam / 2;
			}

			break;
		}

		case GF_ICE:
		{
			/* +50% */
			if ((terrain_element & (ELEMENT_ICE)) &&
				!(target_native & (ELEMENT_ICE)))
			{
				dam = 3 * dam / 2;
			}
			/* +25% */
			else if ((terrain_element == (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER | ELEMENT_ICE)))
			{
				dam = 5 * dam / 4;
			}
			/* -25% */
			else if (terrain_element == (ELEMENT_LAVA))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_WATER:
		{
			/* +50% */
			if ((terrain_element & (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER)))
			{
				dam = 3 * dam / 2;
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_LAVA | ELEMENT_ICE))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_SAND:
		{
			/* +50% or +75% */
			if ((terrain_element & (ELEMENT_SAND)) &&
				!(target_native & (ELEMENT_SAND)))
			{
				/* +75% for deep sand */
				if (cave_ff2_match(y, x, FF2_DEEP))
				{
					dam = 7 * dam / 4;
				}
				/* +50% for shallow sand */
				else
				{
					dam = 3 * dam / 2;
				}
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_BWATER:
		{
			/* +50% */
			if ((terrain_element == (ELEMENT_BWATER)) &&
				((target_native & (ELEMENT_BWATER)) != (ELEMENT_BWATER)))
			{
				dam = 3 * dam / 2;
			}
			/* +25% */
			else if ((terrain_element & (ELEMENT_LAVA | ELEMENT_FIRE)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam = 5 * dam / 4;
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_BMUD:
		{
			/* +50% */
			if ((terrain_element == (ELEMENT_BMUD)) &&
				((target_native & (ELEMENT_BMUD)) != (ELEMENT_BMUD)))
			{
				dam = 3 * dam / 2;
			}
			/* +25% */
			else if ((terrain_element & (ELEMENT_LAVA | ELEMENT_FIRE)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam = 5 * dam / 4;
			}
			/* -25% */
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam = 3 * dam / 4;
			}

			break;
		}

		case GF_LAVA:
		{
			/* +50% or +75% */
			if ((terrain_element == (ELEMENT_LAVA)) &&
				!(target_native & (ELEMENT_LAVA)))
			{
				/* +75% for deep lava */
				if (cave_ff2_match(y, x, FF2_DEEP))
				{
					dam = 7 * dam / 4;
				}
				/* +50% for shallow lava */
				else
				{
					dam = 3 * dam / 2;
				}
			}
			/* +25% */
			else if ((terrain_element & (ELEMENT_FIRE | ELEMENT_LAVA)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam = 5 * dam / 4;
			}

			break;
		}

	}

	/* Check bounds */
	if (dam < 0) dam = 0;

	if (dam > MAX_DAMAGE) dam = MAX_DAMAGE;

	/* Return adjusted damage */
	return (dam);
}


/*
 * We are called from "project()" to "damage" terrain features
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 */
static bool project_f(int who, int y, int x, int dist, int dam, int typ)
{
	bool hurt_feature = FALSE;
	int action = FS_FLAGS_END;

	bool obvious = FALSE;

	char name[80];

	/* Get the name */
	feature_desc(name, sizeof(name), cave_feat[y][x], FALSE, TRUE);

	/* Unused parameters */
	(void)dist;

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		dam = terrain_adjust_damage(dam, typ, y, x, 0);
	}

	/* Some features aren't always affected by certain attacks */
	switch (typ)
	{
		case GF_ACID:
		case GF_VAPOUR:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ACID))
			{
				action = FS_HURT_ACID;
			}

			break;
		}
		case GF_FIRE:
		case GF_SMOKE:
		case GF_LAVA:
		case GF_PLASMA:
		{
			if (cave_ff2_match(y, x, FF2_HURT_FIRE))
			{
				action = FS_HURT_FIRE;
			}

			break;
		}
		case GF_COLD:
		case GF_ICE:
		{
			if (cave_ff2_match(y, x, FF2_HURT_COLD))
			{
				action = FS_HURT_COLD;
			}

			break;
		}
		case GF_ELEC:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ELEC))
			{
				action = FS_HURT_ELEC;
			}

			break;
		}
		case GF_EXTINGUISH:
		{
			if (cave_ff2_match(y, x, FF2_HURT_COLD))
			{
				action = FS_HURT_COLD;
			}

			break;
		}
		case GF_WATER:
		{
			if (cave_ff2_match(y, x, FF2_HURT_WATER))
			{
				action = FS_HURT_WATER;
			}

			break;
		}
		case GF_STEAM:
		case GF_BWATER:
		{
			if (cave_ff3_match(y, x, FF3_HURT_BOIL_WATER))
			{
				action = FS_HURT_BWATER;
			}

			break;
		}
		case GF_POIS:
		{
			if (cave_ff3_match(y, x, FF3_HURT_POIS))
			{
				action = FS_HURT_POIS;
			}

			break;
		}
		case GF_KILL_WALL:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ROCK))
			{
				/*
				 * Hack -- If the terrain has an explicit K: line for HURT_ROCK
				 * it can resists the stone to mud spell (example: sand dunes)
				 */
				if (feat_state_explicit_power(cave_feat[y][x], FS_HURT_ROCK) > 0)
				{
					action = FS_HURT_ROCK;
				}
				/*
				 * Usual case
				 */
				else
				{
					hurt_feature = TRUE;
				}
			}

			break;
		}
	}

	/* We have to fetch the power of the state action */
	if (action != FS_FLAGS_END)
	{
		/* Get the power */
		int power = feat_state_power(cave_feat[y][x], action);

		/* Analyze resistance to the action */
		if ((power < 1) || (rand_int(power) < dam))
		{
			/* The feature can't resist */
			hurt_feature = TRUE;
		}
	}

	/* Analyze the type */
	switch (typ)
	{
		/* Ignore most effects */
		case GF_ACID:
		case GF_VAPOUR:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ACID);

					msg_format("The %s dissolves.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ACID);
			}

			break;
		}
		case GF_FIRE:
		case GF_SMOKE:
		case GF_LAVA:
		case GF_PLASMA:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_FIRE);

					/* Show a proper message */
					if (cave_ff3_match(y, x, FF3_ICE))
					{
						msg_format("The %s melts.", name);
					}
					else if (cave_ff3_match(y, x, FF3_WATER))
					{
						msg_format("The %s evaporates.", name);
					}
					else
					{
 					msg_format("The %s burns up.", name);
					}

					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_FIRE);
			}

			if (temp_lite(y, x)) obvious = TRUE;

			break;
		}
		case GF_COLD:
		case GF_ICE:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_COLD);

					msg_format("The %s freezes.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_COLD);
			}

			break;
		}
		case GF_ELEC:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ELEC);

					msg_format("The %s is struck by lightning.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ELEC);

			}

			if (temp_lite(y, x)) obvious = TRUE;

			break;
		}
		case GF_EXTINGUISH:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_COLD);

					if (cave_ff3_match(y, x, FF3_FIRE))
					{
						msg_format("The %s is put out.", name);
					}
					else msg_format("The %s cools.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_COLD);
			}

			break;
		}
		/* Water */
		case GF_WATER:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_WATER);

					/* Show a proper message */
					if (cave_ff3_match(y, x, FF3_FIRE | FF3_LAVA))
					{
						msg_format("The %s is extinguished.", name);
					}
					else
					{
 					msg_format("The %s floods.", name);
					}

					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_WATER);
			}

			break;
		}
		case GF_METEOR:
		case GF_SHARD:
		case GF_FORCE:
		case GF_SOUND:
		case GF_MANA:
		case GF_HOLY_ORB:
		{
			break;
		}
		/* Boiling water */
		case GF_STEAM:
		case GF_BWATER:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags3 |= (FF3_HURT_BOIL_WATER);

					msg_format("The %s evapourates.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_BWATER);
			}

			break;
		}
		case GF_BMUD:
		{
			/*Do nothing??*/

			break;
		}
		case GF_SAND:
		{
			/*Do nothing??*/

			break;
		}
		case GF_POIS:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags3 |= (FF3_HURT_POIS);

					msg_format("The %s is poisoned.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_POIS);
			}

			break;
		}

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (cave_secret_door_bold(y, x))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					obvious = TRUE;
				}

				/* Create closed door */
				find_secret(y, x);
			}

			/* Destroy traps */
			if (cave_player_trap_bold(y, x))
			{
				effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[x_ptr->x_f_idx];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);

					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}

				/* Destroy the trap */
				delete_effect_idx(cave_x_idx[y][x]);

				/* Forget the trap */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Redraw the grid */
				note_spot(y, x);

				lite_spot(y, x);
			}

			/* Locked doors are unlocked */
			else if (cave_ff3_match(y, x, FF3_DOOR_LOCKED))
			{
				/* Check line of sound */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_OPEN);

					msg_print("Click!");

					obvious = TRUE;
				}

				/* Unlock the door */
				cave_alter_feat(y, x, FS_OPEN);
			}

			break;
		}

		/* Destroy Doors (and traps) */
		case GF_KILL_DOOR:
		{
			if (cave_door_bold(y, x) || cave_player_trap_bold(y, x))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Destroy the door */
					if (cave_door_bold(y, x))
					{
						/*Mark the feature lore*/
						feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
						f_l_ptr->f_l_flags1 |= (FF1_CAN_TUNNEL);
					}
					/* Destroy the trap */
					else
					{
						/*Mark the feature lore*/
						feature_lore *f_l_ptr = &f_l_list[x_list[cave_x_idx[y][x]].x_f_idx];
						f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);
					}


					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

				}

				/* Destroy the door */
				if (cave_door_bold(y, x))
				{
					cave_alter_feat(y, x, FS_TUNNEL);
				}
				/* Destroy the trap/effect*/
				else
				{
					/* Destroy the trap */
					delete_effect_idx(cave_x_idx[y][x]);

					/* Forget the trap */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Redraw the grid */
					note_spot(y, x);

					lite_spot(y, x);
				}
			}

			break;
		}

		/* Jam Doors */
		case GF_LOCK_DOOR:
		{
			/* Check doors */
			if (!cave_door_bold(y, x)) break;

			/* Close doors */
			if (cave_ff1_match(y, x, FF1_CAN_CLOSE))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_CLOSE);

					obvious = TRUE;
				}

				/* Close the door */
				cave_alter_feat(y, x, FS_CLOSE);
			}

			/* Jam doors */
			while (cave_ff1_match(y, x, FF1_CAN_SPIKE))
			{
				int feat = cave_feat[y][x];

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_SPIKE);

					obvious = TRUE;
				}

				/* Jam the door */
				cave_alter_feat(y, x, FS_SPIKE);

				/* Paranoia */
				if (feat == cave_feat[y][x]) break;
			}

			break;
		}

		/* Destroy walls (and doors) */
		case GF_KILL_WALL:
		{
			/* Destroy walls/doors */
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ROCK);

					msg_format("The %s dissolves.", name);
					obvious = TRUE;

				}

				/* Destroy the wall/door */
				cave_alter_feat(y, x, FS_HURT_ROCK);
			}

			break;
		}

		/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Create closed door */
			place_boring_closed_door(y, x);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK)) obvious = TRUE;

			break;
		}

		/* Make features */
		case GF_FEATURE:
		{
			/* Require a "floor or ground" grid */
			if (!cave_ff1_match(y, x, FF1_FLOOR)) break;

			/* Don't hit caster */
			if (cave_m_idx[y][x] == who) break;

			/* Place a feature */
			if (dam) cave_set_feat(y, x, dam);

			/* Check line of sight */
			if (player_has_los_bold(y, x))
			{
				obvious = TRUE;
			}

			break;
		}

		/* Make bridge */
		case GF_BRIDGE:
		{
			int old_feat = cave_feat[y][x];

			if (cave_ff1_match(y, x, FF1_SECRET))
			{
				cave_alter_feat(y, x, FS_SECRET);
			}

			cave_alter_feat(y, x, FS_BRIDGE);

			feature_desc(name, sizeof(name), cave_feat[y][x],
				FALSE, TRUE);

			if (!strstr(name,"stone bridge"))
			{
				cave_set_feat(y, x, old_feat);
			}
			else if (player_has_los_bold(y, x))
			{
				obvious = TRUE;
			}

			break;
		}

		/* Lite up the grid */
		case GF_LITE_WEAK:
		case GF_LITE:
		{
			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				if (!p_ptr->blind)
				{
					/* Observe */
					obvious = TRUE;
				}

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Darken the grid */
		case GF_DARK_WEAK:
		case GF_DARK:
		{
			/* Turn off the light */
			cave_info[y][x] &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if (((cave_info[y][x] & (CAVE_MARK | CAVE_HALO)) == (CAVE_MARK)) &&
				!cave_ff1_match(y, x, FF1_REMEMBER) &&
				(!cave_any_trap_bold(y, x) ||
					(x_list[cave_x_idx[y][x]].x_flags &
					(EF1_HIDDEN))))
			{
				/* Forget */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Grid is in line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Observe */
					obvious = TRUE;

					/* Hack -- A monster is turning off the light */
					if ((who > SOURCE_MONSTER_START) && CHECK_DISTURB(TRUE))
					{
						/* Stop everything */
						disturb(1, 0);
					}

					/* Fully update the visuals */
					p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
				}
			}

			/* All done */
			break;
		}


	}

	/* Return "Anything seen?" */
	return (obvious);
}



/*
 * We are called from "project()" to "affect" objects
 *
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * Hack -- We also "see" objects which are "memorized".
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int y, int x, int dam, int typ)
{
	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	u32b f1, f2, f3, fn;

	char o_name[80];

	/* Glaciers and other unpassable effects protect objects */
	if (!cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (FALSE);

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		dam = terrain_adjust_damage(dam, typ, y, x, 0);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;
		object_kind *k_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;

		cptr note_kill = NULL;

		/* Get the object & object type*/
		o_ptr = &o_list[this_o_idx];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if (artifact_p(o_ptr)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
			case GF_ACID:
			{

				if (hates_acid(o_ptr) && dam > rand_int(50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (f3 & (TR3_IGNORE_ACID)) ignore = TRUE;
					if (fn & (ELEMENT_ACID)) ignore = TRUE;
				}
				break;
			}

			/* Elec -- Rings, Rods, Wands */
			case GF_ELEC:
			{

				if (hates_elec(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				if (dislikes_elec(o_ptr) && dam > (50+rand_int(200)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire -- Flammable objects */
			case GF_FIRE:
			{

				if (hates_fire(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA | ELEMENT_FIRE)) ignore = TRUE;
				}
				if (dislikes_fire(o_ptr) && dam > (50+rand_int(200)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA | ELEMENT_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{

				if (hates_cold(o_ptr) && dam > rand_int(40))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
					if (fn & (ELEMENT_ICE)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Elec */
			case GF_PLASMA:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(40)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (dislikes_fire(o_ptr) && (dam > (50+rand_int(200))))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (hates_elec(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				if (dislikes_elec(o_ptr) && dam > (50+rand_int(200)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Cold */
			case GF_METEOR:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(80)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (dislikes_fire(o_ptr) && (dam > (50+rand_int(200))))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (hates_cold(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " shatter!" : " shatters!");
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARD:
			case GF_FORCE:
			case GF_SOUND:
			{
				if (hates_cold(o_ptr) && (dam > rand_int(40)))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}

			/* Mana -- destroys everything */
			case GF_MANA:
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				break;
			}

			/* Drains charges of staves/wands, activatable objects/ego items/artifacts */
			case GF_STATIC:
			{
				if (dam > rand_int(k_ptr->k_level * 2))
				{

					/*
				 	 * No messages needed.
				 	 * We do not notice this with objects sitting on the ground.
					 */
					if (item_tester_hook_activate(o_ptr))
					{

						(void)drain_activation(o_ptr, dam);
					}

				}
				break;
			}


			/* Holy Orb -- destroys cursed non-artifacts */
			case GF_HOLY_ORB:
			{
				if (cursed_p(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			/* Unlock chests */
			case GF_KILL_TRAP:
			case GF_KILL_DOOR:
			{
				/* Chests are noticed only if trapped or locked */
				if (o_ptr->tval == TV_CHEST)
				{
					/* Disarm/Unlock traps */
					if (o_ptr->pval > 0)
					{
						/* Disarm or Unlock */
						o_ptr->pval = (0 - o_ptr->pval);

						/* Identify */
						object_known(o_ptr);

						/* Notice */
						if (o_ptr->marked)
						{
							msg_print("Click!");
							obvious = TRUE;
						}
					}
				}

				break;
			}

			/* Mass-identify */
			case GF_MASS_IDENTIFY:
			{
			  	int squelch;

				/* Ignore hidden objects */
			  	if (!o_ptr->marked) continue;

				/*Don't identify gold*/
				if (o_ptr->tval == TV_GOLD) continue;

				/* Ignore known objects */
				if (object_known_p(o_ptr)) continue;

			  	/* Identify object and get squelch setting */
				/* Note the first argument */
			  	squelch = do_ident_item(-1, o_ptr, 0);

				/* Redraw purple dots */
				lite_spot(y, x);

				/* Squelch? */
				if (squelch == SQUELCH_YES) do_kill = TRUE;

 				break;
			}

			case GF_SAND:
			{
				if (hates_sand(o_ptr) && dam > rand_int(150))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			case GF_BMUD:
			{
				if (hates_boiling_mud(o_ptr) && dam > rand_int(75))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			case GF_BWATER:
			{
				if (hates_boiling_water(o_ptr) && dam > rand_int(150))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}


			case GF_LAVA:
			{
				if (hates_lava(o_ptr) && dam > rand_int(50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

		}

		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
			}

			/* Artifacts, and other objects, get to resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					msg_format("The %s %s unaffected!",
					           o_name, (plural ? "are" : "is"));
				}
			}

			/* Kill it */
			else
			{
				/* Describe if needed */
				if (o_ptr->marked && note_kill)
				{
					message_format(MSG_DESTROY, 0, "The %s%s", o_name, note_kill);
				}

				/* Delete the object */
				delete_object_idx(this_o_idx);

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
bool project_m(int who, int y, int x, int dam, int typ, u32b flg)
{
	int tmp;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	cptr name;

	/* Is the monster "seen"? */
	bool seen = FALSE;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;

	/* Polymorph setting (true or false) */
	bool do_poly = FALSE;

	/* Teleport setting (max distance) */
	int do_dist = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;

	/* Slow setting (amount to haste) */
	int do_slow = 0;

	/* Haste setting (amount to haste) */
	int do_haste = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;

	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	byte note = MON_MSG_NONE;

	/* Assume a default death */
	byte note_dies = MON_MSG_DIE;

	/* Unused parameter*/
	(void)flg;

	/* Walls protect monsters */
	if (!cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (FALSE);

	/* No monster here */
	if (!(cave_m_idx[y][x] > 0)) return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who) return (FALSE);

	/* Obtain monster info */
	m_ptr = &mon_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	name = (r_name + r_ptr->name);
	if (m_ptr->ml) seen = TRUE;

	/* Special case. Hidden monsters */
	if (m_ptr->mflag & (MFLAG_HIDE))
	{
		if (!(flg & (PROJECT_GRID))) return (FALSE);
	}

	/* Some monsters get "destroyed" */
	if (monster_nonliving(r_ptr))
	{
		/* Special note at death */
		note_dies = MON_MSG_DESTROYED;
	}

	/* Monster goes active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/*Mark the monster as attacked by the player, if the player is in sight. */
	if ((who == SOURCE_PLAYER) && player_has_los_bold(y, x)) m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		dam = terrain_adjust_damage(dam, typ, y, x, r_ptr->r_native);
	}

	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile -- pure damage */
		case GF_ARROW:
		case GF_MISSILE:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Acid */
		case GF_ACID:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_ELEC);
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_FIRE);
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
				note = MON_MSG_HIT_HARD;
				dam = dam * 4 / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				note = MON_MSG_HIT_HARD;
				dam  = (dam * 4) / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_POIS);
			}
			break;
		}

		/* Holy Orb -- hurts Evil */
		case GF_HOLY_ORB:
		{

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam *= 2;
				note = MON_MSG_HIT_HARD;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Plasma -- perhaps check ELEC or FIRE XXX */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_PLAS))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_PLAS);
			}
			else if (prefix(name, "Plasma") ||
			    (r_ptr->flags4 & (RF4_BRTH_PLAS)))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if ((seen) && (r_ptr->flags4 & (RF4_BRTH_PLAS)))
					l_ptr->r_l_flags4 |= (RF4_BRTH_PLAS);
			}
			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				note = MON_MSG_IMMUNE;
				dam = 0;
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
			}
			else if (r_ptr->flags3 & (RF3_RES_NETHR))

			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_NETHR);
			}

			else if (r_ptr->flags4 & (RF4_BRTH_NETHR))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_NETHR);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam /= 2;
				note = MON_MSG_RESIST_SOMEWHAT;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Water (acid) damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->d_char == 'E') && prefix(name, "W"))
			{
				note = MON_MSG_IMMUNE;
				dam = 0;
			}
			else if (r_ptr->flags3 & (RF3_RES_WATER))
			{
				note = MON_MSG_RESIST;
				dam /= 9;
				if ((seen) && (r_ptr->flags3 & (RF3_RES_WATER)))
					l_ptr->r_l_flags3 |= (RF3_RES_WATER);
			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;
			/*handle confusion resist*/
			if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				if (seen) l_ptr->r_l_flags3 |= (RF3_NO_CONF);
			}
			else
			{
				do_conf = rand_range(15, 15 + (dam > 600 ? 60 : dam / 10));
			}

			if (r_ptr->flags3 & (RF3_RES_CHAOS))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				do_poly = FALSE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_CHAOS);
			}
			else if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				do_poly = FALSE;
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_CHAOS);
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_SHARD))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_SHARD);
			}
			break;
		}

		/* Sound -- Sound breathers resist */
		case GF_SOUND:
		{
			if (seen) obvious = TRUE;
			do_stun = randint(dam > 300 ? 30 : dam / 10);
			if (r_ptr->flags4 & (RF4_BRTH_SOUND))
			{
				note = MON_MSG_RESIST;
				dam *= 2; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_SOUND);
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			if (seen) obvious = TRUE;
			do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
			if (r_ptr->flags4 & (RF4_BRTH_CONFU))
			{
				note = MON_MSG_RESIST;
				dam *= 2; dam /= (randint(6)+6);
				l_ptr->r_l_flags4 |= (RF4_BRTH_CONFU);
				do_conf = 0;
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = MON_MSG_RESIST_SOMEWHAT;
				dam /= 2;
				l_ptr->r_l_flags3 |= (RF3_NO_CONF);
				do_conf = 0;
			}
			break;
		}

		/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_DISEN))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_DISEN);
			}
			else if ((r_ptr->flags4 & (RF4_BRTH_DISEN)) ||
			    prefix(name, "Disen"))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen)l_ptr->r_l_flags4 |= (RF4_BRTH_DISEN);
			}
			break;
		}

		/* Keeps monsters from multiplying */
		case GF_STERILIZE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags2 & (RF2_MULTIPLY))
			{
				note = MON_MSG_STERILIZE;
				m_ptr->mflag |= MFLAG_STERILE;
				if (seen) l_ptr->r_l_flags2 |= (RF2_MULTIPLY);
			}

			dam = 0;

			break;
		}

		/* Nexus -- Breathers and Existers resist */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_NEXUS))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_NEXUS);
			}
			else if ((r_ptr->flags4 & (RF4_BRTH_NEXUS)) ||
			    prefix(name, "Nexus"))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_NEXUS);
			}
			break;
		}

		/* Force */
		case GF_FORCE:
		{
			if (seen) obvious = TRUE;
			if (one_in_(2)) do_stun = randint(dam > 240 ? 20 : dam / 12);
			if (r_ptr->flags4 & (RF4_BRTH_FORCE))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_FORCE);
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_INER))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_INER);
			}
			else
			{
				do_slow = rand_int(4) + 4;
			}
			break;
		}

		/* Drains monsters mana */
		case GF_STATIC:
		{
			int drain = MAX(dam / 25,1);

			if (seen) obvious = TRUE;
			if (m_ptr->mana > 0)
			{

				if (drain > m_ptr->mana) m_ptr->mana = 0;
				else m_ptr->mana -= drain;

				note = MON_MSG_MANA_DRAIN;

			}

			break;
		}


		/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_TIME))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_TIME);
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			if (seen) obvious = TRUE;

			/* Higher level monsters can resist the teleportation better */
			if (randint(127) > r_ptr->level) do_dist = 10;

			if (r_ptr->flags4 & (RF4_BRTH_GRAV))
			{
				note = MON_MSG_RESIST;
				dam *= 3; dam /= (randint(6)+6);
				do_dist = 0;
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_GRAV);
			}
			else
			{
				do_slow = rand_int(4) + 4;
			}
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Meteor -- powerful magic missile */
		case GF_METEOR:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = MON_MSG_RESIST_A_LOT;
				dam /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				note = MON_MSG_HIT_HARD;
				dam  = (dam * 4) / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
			}
			if (!(r_ptr->flags3 & (RF3_IM_COLD)))
			{
				do_stun = randint(dam > 240 ? 20 : dam / 12);
			}


			break;
		}

		/* Drain Life */
		case GF_LIFE_DRAIN:
		{
			if (seen) obvious = TRUE;
			if (monster_nonliving(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
				dam = 0;
			}

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_OLD_POLY:
		{
			if (seen) obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* Powerful monsters and questors resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (m_ptr->mflag & (MFLAG_QUEST)) ||
			    (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				note = MON_MSG_UNAFFECTED;
				do_poly = FALSE;
				obvious = FALSE;
			}

			/* No "real" damage */
			dam = 0;

			break;
		}


		/* Clone monsters (Ignore "dam") */
		case GF_OLD_CLONE:
		{
			if (seen) obvious = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			do_haste = 25 + rand_int(25);

			/* Attempt to clone. */
			if (multiply_monster(cave_m_idx[y][x]))
			{
				note = MON_MSG_SPAWN;
			}

			/* No "real" damage */
			dam = 0;

			break;
		}


		/* Heal Monster (use "dam" as amount of healing) */
		case GF_OLD_HEAL:
		{
			bool healed = TRUE;

			/*does monster need healing?*/
			if (m_ptr->hp == m_ptr->maxhp) healed = FALSE;

			if (seen) obvious = TRUE;

			/* Wake up */
			m_ptr->csleep = 0;

			/* Monster goes active */
			m_ptr->mflag |= (MFLAG_ACTV);

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

			/*monster was at full hp to begin*/
			if (!healed)
			{
				obvious = FALSE;

				note = MON_MSG_UNAFFECTED;

			}

			/* Message */
			else note = MON_MSG_HEALTHIER;

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Speed Monster (Ignore "dam") */
		case GF_OLD_SPEED:
		{

			if (seen) obvious = TRUE;

			/* Speed up */
			do_haste = 50 + rand_int(50);

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Slow Monster (Use "dam" as "power") */
		case GF_OLD_SLOW:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_VULN_SLOW)){
				dam = MAX(dam + 10,(dam*3)/2);
				note = MON_MSG_HIT_HARD;
			}

			do_slow = dam;

			/* Powerful monsters can resist */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(2))) ||
			    ((r_ptr->level/2 + randint(MAX(4, r_ptr->level / 3))) >
				 (randint(dam+3) )))
			{
				note = MON_MSG_RESIST_SOMEWHAT;
				do_slow = MIN(do_slow,damroll(3,2));
			}

			/* Powerful monsters can prevent */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(3))) ||
				(r_ptr->flags3 & (RF3_NO_SLOW)) ||
			    ((r_ptr->level/2 + randint(MAX(4, r_ptr->level / 3))) >
				 (randint(dam+7) )))
			{
				note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
				if ((seen) && (r_ptr->flags3 & (RF3_NO_SLOW)))
				    l_ptr->r_l_flags3 |= (RF3_NO_SLOW);
				do_slow = 0;
			}

			if (do_slow && (r_ptr->flags3 & (RF3_VULN_SLOW))){
				l_ptr->r_l_flags3 |= (RF3_VULN_SLOW);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Sleep (Use "dam" as "power") */
		case GF_OLD_SLEEP:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_VULN_SLEEP)){
				dam = MAX(dam + 10,(dam*3)/2);
				note = MON_MSG_HIT_HARD;
			}

			/* Attempt a saving throw */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(3))) ||
			    (r_ptr->flags3 & (RF3_NO_SLEEP)) ||
			    ((r_ptr->level/2 + randint(MAX(4, r_ptr->level / 3))) >
				 (randint(dam+7) )))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_NO_SLEEP);
				}

				/* No obvious effect */
				note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
				note = MON_MSG_FALL_ASLEEP;
				do_sleep = 500;
			}

			if (do_sleep && (r_ptr->flags3 & (RF3_VULN_SLEEP))){
				l_ptr->r_l_flags3 |= (RF3_VULN_SLEEP);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Confusion (Use "dam" as "power") */
		case GF_OLD_CONF:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_VULN_CONF)){
				dam = MAX(dam + 10,(dam*3)/2);
				note = MON_MSG_HIT_HARD;
			}

			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw to reduce */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(2))) ||
			    ((r_ptr->level/2 + randint(MAX(4, r_ptr->level / 3))) >
				 (randint(dam+3))))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_NO_CONF);
				}

				/* Resist partly */
				do_conf = MIN(damroll(3,2),do_conf);

				note = MON_MSG_RESIST_SOMEWHAT;
			}

			/* Attempt a saving throw to prevent */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(3))) ||
			    (r_ptr->flags3 & (RF3_NO_CONF)) ||
			    ((r_ptr->level/2 + randint(MAX(4, r_ptr->level / 3))) >
				 (randint(dam+7))))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				do_conf = 0;

				/* No obvious effect */
				note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
			}

			if (do_conf && (r_ptr->flags3 & (RF3_VULN_CONF))){
				l_ptr->r_l_flags3 |= (RF3_VULN_CONF);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}



		/* Lite, but only hurts susceptible creatures */
		case GF_LITE_WEAK:
		{
			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Obvious effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_LITE);

				/* Special effect */
				note = MON_MSG_CRINGE_LIGHT;
				note_dies = MON_MSG_SHRIVEL_LIGHT;
			}

			/* Normally no damage */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}



		/* Lite -- opposite of Dark */
		case GF_LITE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_LITE))
			{
				note = MON_MSG_RESIST;
				dam *= 2; dam /= (randint(6)+6);
			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_LITE);
				note = MON_MSG_CRINGE_LIGHT;
				note_dies = MON_MSG_SHRIVEL_LIGHT;
				dam = (dam * 5) / 2;
			}
			break;
		}


		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_DARK))
			{
				note = MON_MSG_RESIST;
				dam *= 2; dam /= (randint(6)+6);
			}
			break;
		}


		/* Stone to Mud */
		case GF_KILL_WALL:
		{
			/* Hurt by rock remover */
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
			{
				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
				note = MON_MSG_LOSE_SKIN;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Usually, ignore the effects */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}


		/* Teleport undead (Use "dam" as "power") */
		case GF_AWAY_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				do_dist = dam;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Teleport evil (Use "dam" as "power") */
		case GF_AWAY_EVIL:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
				do_dist = dam;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Teleport monster (Use "dam" as "power") */
		case GF_AWAY_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Prepare to teleport */
			do_dist = dam;

			if (r_ptr->flags3 & (RF3_RES_NEXUS))
			{
				note = MON_MSG_RESIST;
				do_dist = 0;
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_NEXUS);
			}
			else if ((r_ptr->flags4 & (RF4_BRTH_NEXUS)) ||
			    prefix(name, "Nexus"))
			{
				note = MON_MSG_RESIST;
				do_dist = 0;
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_NEXUS);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn undead (Use "dam" as "power") */
		case GF_TURN_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((r_ptr->level + randint(MAX(4, r_ptr->level / 2))) >
				 	(randint(dam) ))
				{
					/* No obvious effect */
					note = MON_MSG_UNAFFECTED;
					obvious = FALSE;
					do_fear = 0;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn evil (Use "dam" as "power") */
		case GF_TURN_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((r_ptr->level + randint(MAX(4, r_ptr->level / 2))) >
				 	(randint(dam) ))
				{
					/* No obvious effect */
					note = MON_MSG_UNAFFECTED;
					obvious = FALSE;
					do_fear = 0;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn monster (Use "dam" as "power") */
		case GF_TURN_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Apply some fear */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if (((r_ptr->flags1 & (RF1_UNIQUE)) && (one_in_(2))) ||
			    (r_ptr->flags3 & (RF3_NO_FEAR)) ||
			    ((r_ptr->level + randint(MAX(4, r_ptr->level / 2))) >
				 (randint(dam) )))
			{
				/* No obvious effect */
				note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
				do_fear = 0;
				if ((seen) && (r_ptr->flags3 & (RF3_NO_FEAR)))
					l_ptr->r_l_flags3 |= (RF3_NO_FEAR);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Dispel undead */
		case GF_DISP_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = MON_MSG_SHUDDER;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}


		/* Dispel evil */
		case GF_DISP_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = MON_MSG_SHUDDER;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/* make_wary */
		case GF_MAKE_WARY:
		{
			/*Sleeping, confused, or afraid monsters never notice anything*/
			if ((m_ptr->csleep) || (m_ptr->confused) || (m_ptr->monfear)) skipped = TRUE;

			/*certain monsters never notice anything either*/
			else if (r_ptr->flags2 & (RF2_STUPID)) skipped = TRUE;
			else if (r_ptr->flags1 & (RF1_NEVER_MOVE)) skipped = TRUE;
			else if (r_ptr->flags2 & (RF2_EMPTY_MIND)) skipped = TRUE;
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND)) skipped = TRUE;
			else if ((strchr("BFIJKSXabceijlrsmvwjz,", r_ptr->d_char)) &&
				(!r_ptr->flags2 & (RF2_SMART))) skipped = TRUE;

			/*don't make monsters wary again to avoid the message*/
			else if (m_ptr->mflag & (MFLAG_WARY)) skipped = TRUE;;

			/* Obvious */
			if (seen) obvious = TRUE;

			/*don't mention anything if they are already*/
			if (!skipped)
			{

				m_ptr->mflag |= (MFLAG_WARY);

				/*senitent beings get a more human-like message*/
				if ((strchr("phntyPdDGLOoTuUvV", r_ptr->d_char)) || (r_ptr->flags2 & (RF2_SMART)))
				{
					if (one_in_(2))	note = MON_MSG_AWARE_OF_CRAFTY_ABILITIES;
					else note = MON_MSG_AWARE_OF_CUNNING_TACTICS;
				}

				/* or else a more message based on instinct*/
				else
				{
				    if (one_in_(2)) note = MON_MSG_SENSE_CRAFTY_ABILITIES;
					else note = MON_MSG_SENSE_CUNNING_FOE;
				}

			}

			break;
		}


		/* Dispel monster */
		case GF_DISP_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			note = MON_MSG_SHUDDER;
			note_dies = MON_MSG_DISSOLVE;

			break;
		}

		case GF_SAND:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if (r_ptr->r_native & (FF3_SAND))
			{
				note = MON_MSG_RESIST;
				dam /= 3;
				if (seen) l_ptr->r_l_native |= (FF3_SAND);
			}

			break;
		}
		case GF_BMUD:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if ((r_ptr->r_native & (ELEMENT_BMUD)) == ELEMENT_BMUD)
			{
				note = MON_MSG_RESIST;
				dam /= 9;
				if (seen) l_ptr->r_l_native |= (ELEMENT_BMUD);
			}

			break;
		}
		case GF_BWATER:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if ((r_ptr->r_native & (ELEMENT_BWATER)) == ELEMENT_BWATER)
			{
				note = MON_MSG_RESIST;
				dam /= 9;
				if (seen) l_ptr->r_l_native |= (ELEMENT_BWATER);
			}

			break;
		}
		case GF_LAVA:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if (r_ptr->r_native & (FF3_LAVA))
			{
				note = MON_MSG_RESIST;
				dam /= 30;
				if (seen) l_ptr->r_l_native |= (ELEMENT_LAVA);
			}

			break;
		}


		/* Default */
		default:
		{
			/* Irrelevant */
			skipped = TRUE;

			/* No damage */
			dam = 0;

			break;
		}
	}


	/* Absolutely no effect */
	if (skipped) return (FALSE);

	/* "Unique" monsters or quest monsters cannot be polymorphed */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (m_ptr->mflag & (MFLAG_QUEST))) do_poly = FALSE;

	/* "Unique" monsters can only be "killed" by the player */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Uniques may only be killed by the player */
		if ((who > SOURCE_MONSTER_START) && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}

	/* Hack - "Quest" monsters can only be hurt by the player */
	if (m_ptr->mflag & (MFLAG_QUEST))
	{
		/* Questors may only be killed by the player, or player trap. */
		if (who != SOURCE_PLAYER) dam = 0;
	}

	/* Reveal mimics */
	if (m_ptr->mimic_k_idx)
	{
		/* Reveal it */
		reveal_mimic(m_ptr->fy, m_ptr->fx, seen);
	}

	/* Get the actual monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Check for death */
	if (dam > m_ptr->hp)
	{
		/* Extract method of death */
		note = note_dies;
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint(90) > r_ptr->level))
	{
		/* Default -- assume no polymorph */
		note = MON_MSG_UNAFFECTED;

		/* Pick a "new" monster race */
		tmp = poly_r_idx(m_ptr);

		/* Handle polymorph */
		if (tmp != m_ptr->r_idx)
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Monster polymorphs */
			note = MON_MSG_CHANGE;

			/* Add the message now before changing the monster race */
			add_monster_message(m_name, m_ptr->r_idx, note);

			/* No more messages */
			note = MON_MSG_NONE;

			/* Turn off the damage */
			dam = 0;

			/* "Kill" the "old" monster */
			delete_monster_idx(cave_m_idx[y][x]);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE);

			/* Hack -- Assume success XXX XXX XXX */

			/* Hack -- Get new monster */
			m_ptr = &mon_list[cave_m_idx[y][x]];

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];

			/* Hack -- Get the new monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);
		}
	}

	/* Handle "teleport" */
	else if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Native terrain grants resistance to hostile teleportation */
		if (cave_ff3_match(y, x, TERRAIN_MASK) && is_monster_native(y, x, r_ptr) && one_in_(5))
		{
			/*Mark the lore*/
			if ((m_ptr->ml) && player_can_observe() && (cave_info[y][x] & (CAVE_MARK)))
			{
				u32b native = f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3;
				native &= r_ptr->r_native;
				l_ptr->r_l_native |= native;
			}

			/* Message */
			note = MON_MSG_RESIST;
		}
		/* Teleport the monster */
		else
		{

			/* Message */
			note = MON_MSG_DISAPPEAR;

			/* Teleport */
			teleport_away(cave_m_idx[y][x], do_dist);

			/* Hack -- get new location */
			y = m_ptr->fy;
			x = m_ptr->fx;
		}
	}

	/* Sound and Impact breathers never stun */
	else if (do_stun &&
	         !(r_ptr->flags4 & (RF4_BRTH_SOUND)) &&
	         !(r_ptr->flags4 & (RF4_BRTH_FORCE)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Get confused */
		if (m_ptr->stunned)
		{
			note = MON_MSG_MORE_DAZED;
			tmp = m_ptr->stunned + (do_stun / 2);
		}
		else
		{
			note = MON_MSG_DAZED;
			tmp = do_stun;
		}

		/*some creatures are resistant to stunning*/
		if (r_ptr->flags3 & RF3_NO_STUN)
		{
			/*mark the lore*/
			if (seen) l_ptr->r_l_flags3 |= (RF3_NO_STUN);

			note = MON_MSG_UNAFFECTED;

		}

		/* Apply stun */
		else m_ptr->stunned = (tmp < 200) ? tmp : 200;

		/*possibly update the monster health bar*/
		if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
					p_ptr->redraw |= (PR_HEALTH);
	}

	/* Confusion and Chaos breathers (and sleepers) never confuse */
	else if (do_conf &&
	         !(r_ptr->flags3 & (RF3_NO_CONF)) &&
	         !(r_ptr->flags4 & (RF4_BRTH_CONFU)) &&
	         !(r_ptr->flags4 & (RF4_BRTH_CHAOS)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Already partially confused */
		if (m_ptr->confused)
		{
			note = MON_MSG_MORE_CONFUSED;
			tmp = m_ptr->confused + (do_conf / 2);
		}

		/* Was not confused */
		else
		{
			note = MON_MSG_CONFUSED;
			tmp = do_conf;
		}

		/* Apply confusion */
		m_ptr->confused = (tmp < 200) ? tmp : 200;

		if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])
		p_ptr->redraw |= (PR_HEALTH);
	}

	/*Slowing*/
	else if (do_slow)
	{

		/* Increase fear */
		tmp = m_ptr->slowed + do_slow;

		/* set or add to slow counter */
		set_monster_slow(cave_m_idx[m_ptr->fy][m_ptr->fx],
						tmp, seen);
	}

	/* Hasting */
	else if (do_haste)
	{
		/* Increase fear */
		tmp = m_ptr->hasted + do_haste;

		/* set or add to slow counter */
		set_monster_haste(cave_m_idx[m_ptr->fy][m_ptr->fx],
						tmp, seen);
	}


	/* Fear */
	if (do_fear)
	{
		/* Increase fear */
		tmp = m_ptr->monfear + do_fear;

		/* Panic the monster */
		set_mon_fear(m_ptr, tmp, TRUE);

		/*a monster can't be wary and afraid*/
		m_ptr->mflag &= ~(MFLAG_WARY);

	}

	/* If another monster did the damage, hurt the monster by hand */
	if (who != SOURCE_PLAYER)
	{
		/* Redraw (later) if needed */
		if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up */
		m_ptr->csleep = 0;

		/* Monster goes active */
		m_ptr->mflag |= (MFLAG_ACTV);

		/* Hurt the monster */
		m_ptr->hp -= dam;

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			/* Give detailed messages if destroyed */
			if (!seen) note_dies = MON_MSG_MORIA_DEATH;

			/* dump the note*/
			add_monster_message(m_name, m_ptr->r_idx, note_dies);

			/* Generate treasure, etc */
			monster_death(cave_m_idx[y][x], who);

			/* Delete the monster */
			delete_monster_idx(cave_m_idx[y][x]);
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if ((note != MON_MSG_NONE) && seen)
			{
				/* dump the note*/
				add_monster_message(m_name, m_ptr->r_idx, note);
			}

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(cave_m_idx[y][x], dam);

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;

		/* The monster is going to be killed */
		if (dam > m_ptr->hp)
		{
			/* Adjust message for unseen monsters */
			if (!seen) note_dies = MON_MSG_MORIA_DEATH;

			/* Save the death notification for later */
			add_monster_message(m_name, m_ptr->r_idx, note_dies);
		}

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(cave_m_idx[y][x], dam, &fear, "", who))
		{
			/* Dead monster. Empty statement */
			;
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if ((note != MON_MSG_NONE) && seen)
			{
				add_monster_message(m_name, m_ptr->r_idx, note);
			}

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(cave_m_idx[y][x], dam);

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}


	/* Verify this code XXX XXX XXX */

	/* Update the monster */
	update_mon(cave_m_idx[y][x], FALSE);

	/* Redraw the monster grid */
	lite_spot(y, x);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}

	/* Track it */
	project_m_n++;
	project_m_x = x;
	project_m_y = y;

	/* Hack -- Sound attacks are extremely noisy. */
	if (typ == GF_SOUND) add_wakeup_chance = 10000;

	/*
	 * Otherwise, if this is the first monster hit, the spell was capable
	 * of causing damage, and the player was the source of the spell,
	 * make noise. -LM-
	 */
	else if ((project_m_n == 1) && (who == SOURCE_PLAYER) && (dam))
	{
		add_wakeup_chance += p_ptr->base_wakeup_chance / 2 + 1000;
	}

	/* Return "Anything seen?" */
	return (obvious);
}






/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.
 *
 * Actually, for historical reasons, we just assume that the effects were
 * obvious.  XXX XXX XXX
 */
bool project_p(int who, int y, int x, int dam, int typ, cptr msg)
{
	int k = 0, m_lvl;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Monster name (for damage) */
	char killer[80];

	bool player_native = FALSE;

	/* Hack -- messages */
	cptr act = NULL;

	bool is_terrain = FALSE;

	/* No player here */
	if ((y != p_ptr->py) || (p_ptr->px != x)) return (FALSE);

	/* Limit maximum damage XXX XXX XXX */
	if (dam > MAX_DAMAGE) dam = MAX_DAMAGE;

	if (who > SOURCE_MONSTER_START)
	{
		/* Source monster */
		monster_type *m_ptr;

		/* Monster name (for attacks) */
		char m_name[80];

		/* Get the source monster */
		m_ptr = &mon_list[who];

		/* Get the monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, sizeof(killer), m_ptr, 0x88);
	}
	else if (who == SOURCE_TRAP)
	{

		/* Get the effect name */
		feature_desc(killer, sizeof(killer), x_list[cave_x_idx[y][x]].x_f_idx, TRUE, TRUE);
	}
	else
	{
		/* Remember dangerous terrain */
		if (who == SOURCE_OTHER)
		{
 			is_terrain = TRUE;
		}

 		my_strcpy(killer, msg ? msg: "the dungeon", sizeof(killer));
	}

	/*
	 * Apply bonuses/penalties from terrain, only if damage doesn't come
	 * from terrain already
	 */
	if (!is_terrain)
	{
		/* Adjust damage */
		dam = terrain_adjust_damage(dam, typ, y, x, p_ptr->p_native);
	}

	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by acid!");
			   else msg_format("You are %s", msg);
			}

			acid_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by fire!");
			   else msg_format("You are %s", msg);
			}

			fire_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by cold!");
			   else msg_format("You are %s", msg);
			}

			cold_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by lightning!");
			   else msg_format("You are %s", msg);
			}

			elec_dam(dam, killer);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			/*player is immune*/
			if (p_ptr->immune_pois) break;
			if (blind) msg_print("You are hit by poison!");
			if (p_ptr->resist_pois) dam = (dam + 2) / 3;
			if (p_ptr->oppose_pois) dam = (dam + 2) / 3;
			take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois || p_ptr->immune_pois))
			{
				(void)set_poisoned(p_ptr->poisoned + rand_int(dam) + 10);
			}
			break;
		}

		/* Standard damage */
		case GF_MISSILE:
		{
			if ((blind) && (!is_terrain)) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* No resist to Lava damage, unless native. */
		case GF_LAVA:
		{

			/*Player is native*/
			if (p_ptr->p_native & (FF3_LAVA))
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are seared by lava!");
				}
				else
				{
					msg_print("You are hit by lava!");
				}

			}

			lava_dam(dam, killer, player_native);
			break;
		}

		/* Boiling water  */
		case GF_BWATER:
		{
			/*Player is native*/
			if ((p_ptr->p_native & (ELEMENT_BWATER)) == ELEMENT_BWATER)
			{
				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Take nominal damage*/
				dam /= 9;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are scorched by boiling water!");
				}
				else
				{
					msg_print("You are hit with boiling water!");
				}

			}

			boiling_water_dam(dam, killer, player_native);
			break;
		}

		/* Boiling mud  */
		case GF_BMUD:
		{
			/*Player is native*/
			if ((p_ptr->p_native & (ELEMENT_BMUD)) == ELEMENT_BMUD)
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Take nominal damage*/
				dam /= 9;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are seared by boiling mud!");
				}
				else
				{
					msg_print("You are hit with boiling mud!");
				}

			}

			boiling_mud_dam(dam, killer, player_native);
			break;
		}

		/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_ORB:
		{
			if (blind) msg_print("You are hit by a holy force!");
			take_hit(dam, killer);
			holy_orb_destroy(dam);
			break;
		}

		/* Arrow --  */
		case GF_ARROW:
		{
			if (who > SOURCE_MONSTER_START)
			{

				/* Source monster */
				monster_type *m_ptr;
				monster_race *r_ptr;

				/* Get the source monster */
				m_ptr = &mon_list[who];

				/* Get the monster race. */
				r_ptr = &r_info[m_ptr->r_idx];

				/* Test for a miss or armour deflection. */
				m_lvl = r_ptr->level;
				if (r_ptr->flags1 & RF1_QUESTOR){
					m_lvl = m_lvl + 30;
				}
				if ((p_ptr->ac + ((p_ptr->to_a < 150) ? p_ptr->ac + p_ptr->to_a : 150)) >
			   	  randint((10 + m_lvl) * 5))
				{
					if ((p_ptr->ac > 9) && (one_in_(2)))
					{
						msg_print("It glances off your armour.");
					}
					else msg_print("It misses.");

					/* No damage. */
					dam = 0;
				}
			}

			if (dam)
			{
				/* Test for a deflection. */
				if ((inventory[INVEN_ARM].k_idx) &&
					(inventory[INVEN_ARM].ac  > rand_int(50)))
				{

					msg_print("It ricochets off your shield.");

					/* No damage. */
					dam = 0;
				}

				/* Reduce damage if missile did not get deflected. */
				else dam -= (dam * ((p_ptr->ac + (p_ptr->to_a < 150) ?
				                 p_ptr->ac + p_ptr->to_a : 150)) / 250);
			}

			if (dam)
			{
				if (blind) msg_print("You are hit by something sharp!");
				take_hit(dam, killer);
			}
			break;
		}

		/* Plasma -- No resist XXX */
		case GF_PLASMA:
		{
			if ((blind)  && (!is_terrain)) msg_print("You are hit by something!");
			take_hit(dam, killer);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}
			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->resist_nethr)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				s32b d = 200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

				if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Water -- stun/confuse */
		case GF_WATER:
		{
			if (blind) msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint(40));
			}
			if (allow_player_confusion())
			{
				(void)set_confused(p_ptr->confused + randint(5) + 5);
			}
			take_hit(dam, killer);
			break;
		}

		/* Chaos -- many effects */
		case GF_CHAOS:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			if (allow_player_confusion())
			{
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			if (!p_ptr->resist_chaos)
			{
				(void)set_image(p_ptr->image + randint(10));
			}
			if (!p_ptr->resist_nethr && !p_ptr->resist_chaos)
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else
				{
					s32b d = 200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

					if (p_ptr->hold_life)
					{
						msg_print("You feel your life slipping away!");
						lose_exp(d / 10);
					}
					else
					{
						msg_print("You feel your life draining away!");
						lose_exp(d);
					}
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Shards -- mostly cutting */
		case GF_SHARD:
		{
			if (blind) msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)set_cut(p_ptr->cut + dam);
			}
			take_hit(dam, killer);
			break;
		}

		/* Sound -- mostly stunning */
		case GF_SOUND:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->resist_sound)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer);
			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->resist_confu || p_ptr->oppose_conf)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			if (allow_player_confusion())
			{
				(void)set_confused(p_ptr->confused + randint(20) + 10);
			}
			take_hit(dam, killer);
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->resist_disen)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)apply_disenchant(0);
			}
			take_hit(dam, killer);
			break;
		}

		/* Nexus -- see above */
		case GF_NEXUS:
		{
			if (blind) msg_print("You are hit by something strange!");

			if (p_ptr->resist_nexus)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				apply_nexus(who);
			}

			take_hit(dam, killer);
			break;
		}

		/*Static - drain player mana, rods, wands, staves, activatable items*/
		case GF_STATIC:
		{
			int drain = MAX(dam / 25,1);
			if (blind) msg_print("You are hit by something!");

			if (p_ptr->csp > 0)
			{
				if (blind) msg_print("You feel your head cloud up!");
				if (drain > p_ptr->csp) p_ptr->csp = 0;
				else p_ptr->csp -= drain;

			}

			take_hit(dam, killer);

			/*Drain the inventory if needed*/
			if (!p_ptr->is_dead) inven_drain(dam);

			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
			if (blind) msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint(20));
			}
			take_hit(dam, killer);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (blind) msg_print("You are hit by something strange!");
			(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			take_hit(dam, killer);
			break;
		}

		/* Lite -- blinding */
		case GF_LITE:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->resist_lite)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint(5) + 2);
			}
			take_hit(dam, killer);
			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->resist_dark)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint(5) + 2);
			}
			take_hit(dam, killer);
			break;
		}

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
			if (blind) msg_print("You are hit by something strange!");

			switch (randint(10))
			{
				case 1: case 2: case 3: case 4: case 5:
				{
					msg_print("You feel life has clocked back.");
					lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
					break;
				}

				case 6: case 7: case 8: case 9:
				{
					switch (randint(8))
					{
						case 1: k = A_STR; act = "strong"; break;
						case 2: k = A_INT; act = "bright"; break;
						case 3: k = A_WIS; act = "wise"; break;
						case 4: k = A_DEX; act = "dextrous"; break;
						case 5: k = A_CON; act = "hale"; break;
						case 6: k = A_AGI; act = "agile"; break;
						case 7: k = A_STE; act = "sneaky"; break;
						case 8: k = A_PER; act = "clear-sighted"; break;
					}

					msg_format("You're not as %s as you used to be...", act);

					p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
					if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					p_ptr->update |= (PU_BONUS);
					break;
				}

				case 10:
				{
					msg_print("You're not as powerful as you used to be...");

					for (k = 0; k < A_MAX; k++)
					{
						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					}
					p_ptr->update |= (PU_BONUS);
					break;
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY:
		{
			if (blind) msg_print("You are hit by something strange!");
			msg_print("Gravity warps around you.");

			/* Higher level players can resist the teleportation better */
			if (randint(127) > p_ptr->lev) teleport_player(5);

			(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}

			take_hit(dam, killer);

			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (blind) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		{
			if (blind) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by something sharp!");
			   else msg_format("You are %s", msg);
			}

			cold_dam(dam, killer);
			if (!p_ptr->resist_shard)
			{
				(void)set_cut(p_ptr->cut + damroll(5, 8));
			}
			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint(15));
			}
			break;
		}

		/* Spores - poison, cause disease */
		case GF_SPORE:
		{
			int power;

			if (who > SOURCE_MONSTER_START)
			{
				/* Source monster */
				monster_type *m_ptr;
				monster_race *r_ptr;

				/* Get the source monster */
				m_ptr = &mon_list[who];

				/* Get the monster race. */
				r_ptr = &r_info[m_ptr->r_idx];

				power = r_ptr->level;

			}
			else power = danger(p_ptr->depth);

			if (blind) msg_print("You feel spores all around you...");

			take_hit(dam, killer);

			/* Poison */
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois || p_ptr->immune_pois))
			{
				set_poisoned(p_ptr->poisoned + randint(dam * 2));
			}

			/* Disease */
			if (randint(power) >= 15)
			{
				int dummy = randint(dam * 2);
				disease(&dummy);
			}

			break;
		}

		case GF_SAND:
		{
			/*Player is native*/
			if (p_ptr->p_native & (FF3_SAND))
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Reduce the damage a bit.*/
				dam /= 2;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are blasted by sand!");
				}
				else
				{
					msg_print("You are hit by sand!");
				}

			}

			/*Sand wears down the equipment*/
			apply_disenchant(TRUE);

			take_hit(dam, killer);
			break;
		}

		/* Default */
		default:
		{
			take_hit(dam, killer);

			break;
		}
	}


	/* Disturb */
	disturb(1, 0);


	/* Return "Anything seen?" */
	return (obvious);
}

/*
 * Helper function for "project()" below.
 *
 * Create effects based on the spell type.
 *
 *
 * We return "TRUE" if any "obvious" effects were observed.
 *
 */
bool project_x(int who, int y, int x, int dam, int typ, u32b project_flg)
{
	u16b effect_flag = 0L;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	bool always = FALSE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	int source = who;

	if (project_flg & (PROJECT_CLOUD)) always = TRUE;

	/*No effects in walls*/
	if (!cave_ff1_match(y, x, FF1_MOVE)) return (FALSE);

	/*We can't see this square*/
	if (!player_can_see_bold(y, x)) obvious = FALSE;

	/*
	 * Apply bonuses/penalties from terrain, only if damage doesn't come
	 * from terrain already
	 */
	if (who != SOURCE_OTHER)
	{
		/* Adjust damage */
		dam = terrain_adjust_damage(dam, typ, y, x, 0);
	}

	/* Remember if the player is the source. */
	if (source == SOURCE_PLAYER) 		effect_flag |= EF1_CHARACTER;
	if (project_flg & (PROJECT_PLAY)) 	effect_flag |= EF1_HURT_PLAY;

	/*If a monster, make the source the race of the monster*/
	if (who > SOURCE_MONSTER_START)
	{
		monster_type *m_ptr = &mon_list[who];
		source = m_ptr->r_idx;
	}

	/* Analyze the damage */
	switch (typ)
	{

		/* Fire and lava leave smoke */
		case GF_FIRE:
		case GF_LAVA:
		{
			/*Lingering Smoke Cloud*/
			if ((dam > 200) || always) (void)set_effect_lingering_cloud(FEAT_SMOKE, y, x, dam, source, effect_flag);
			break;
		}
		/* Fire leaves smoke */
		case GF_ACID:
		case GF_ELEC:
		{
			int feat = ((typ == GF_ACID) ? FEAT_SMOKE : FEAT_SPARKS);

			/*Lingering Cloud*/
			if ((dam > 400) || always)  (void)set_effect_lingering_cloud(feat, y, x, (dam / 3), source, effect_flag);
			break;
		}
		case GF_GRAVITY:
		{
			/*Not noticed*/
			obvious = FALSE;

			/*Leave residual effects if damage is high enough*/
			if ((dam > 300) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(FEAT_GRAVITY, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			/*Lingering Poison Cloud*/
			if ((dam > 50) || always)  (void)set_effect_lingering_cloud(FEAT_POISON_CLOUD, y, x, dam, source, effect_flag);
			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/*Not noticed*/
			obvious = FALSE;

			place_trap(y, x, 0);

			break;
		}

		/* Boiling water or mud -- on player */
		case GF_BWATER:
		case GF_BMUD:
		{
			/* Steam Cloud */
			if ((dam > 50) || always)  (void)set_effect_lingering_cloud(FEAT_STEAM, y, x, dam, source, effect_flag);
			break;
		}

		/* Lite  */
		case GF_LITE:
		case GF_DARK:
		{
			int feat = ((typ == GF_LITE) ? FEAT_LITE : FEAT_DARK);

			/*Not noticed*/
			obvious = FALSE;

			if ((dam > 200) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(feat, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_LIFE_DRAIN:
		{
			/*Leave residual effects if a player spell*/
			if (always)
			{
				int dam1 = (dam * f_info[FEAT_LIFE_DRAIN].x_damage) / 100;

				int repeats = ((dam1 / 25) + randint(dam1 / 25));

				(void)set_effect_shimmering_cloud(FEAT_LIFE_DRAIN, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}


		/* Spores - poison, */
		case GF_SPORE:
		{
			/*Lingering Poison Cloud*/
			if ((dam > 100) || always)  (void)set_effect_lingering_cloud(FEAT_POISON_CLOUD, y, x, dam, source, effect_flag);
			break;
		}

		/*Clear the air of cloud effects*/
		case GF_CLEAR_AIR:
		{

			/* Get the first effect */
			u16b x_idx = cave_x_idx[y][x];

			/* Scan the effects on that grid */
			while (x_idx)
			{
				u16b this_x_idx = x_idx;

				/* Get the effect data */
				effect_type *x_ptr = &x_list[x_idx];

				/* Point to the next effect */
				x_idx = x_ptr->next_x_idx;

				if ((x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
					(x_ptr->x_type == EFFECT_SHIMMERING_CLOUD) ||
					(x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
				{
					/*Delete it*/
					delete_effect_idx(this_x_idx);
				}


			}

			break;

		}

		case GF_VAPOUR:
		case GF_STEAM:

		{
			/*Lingering Steam Cloud*/
			(void)set_effect_lingering_cloud(FEAT_STEAM, y, x, dam, source, effect_flag);
			break;
		}

		case GF_SMOKE:
		{
			/*Lingering Smoke Cloud*/
			(void)set_effect_lingering_cloud(FEAT_SMOKE, y, x, dam, source, effect_flag);
			break;
		}
		case GF_STATIC:
		{
			/*Not noticed*/
			obvious = FALSE;

			if ((dam > 200) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(FEAT_STATIC, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_NETHER:
		case GF_CHAOS:
		case GF_DISENCHANT:
		case GF_NEXUS:
		case GF_TIME:
		case GF_CONFUSION:
		{
			int feat;

			if (typ == GF_NETHER) 			feat = FEAT_NETHER;
			else if (typ == GF_CHAOS) 		feat = FEAT_CHAOS;
			else if (typ == GF_DISENCHANT) 		feat = FEAT_DISENCHANTMENT;
			else if (typ == GF_NEXUS) 		feat = FEAT_NEXUS;
			else if (typ == GF_TIME) 		feat = FEAT_TIME;
			else if (typ == GF_CONFUSION) 	feat = FEAT_CONFUSION;
			/*Paranoia*/
			else break;

			/*Lingering Cloud*/
			if ((dam > 400) || always)  (void)set_effect_lingering_cloud(feat, y, x, (dam / 3), source, effect_flag);
			break;
		}

		/* Default */
		default:
		{
			return (FALSE);
		}
	}

	/* Return "Anything seen?" */
	return ((obvious) && (!blind));
}

/*
 * Calculate and store the arcs used to make starbursts.
 */
static void calc_starburst(int height, int width, byte *arc_first,
	byte *arc_dist, int *arc_num)
{
	int i;
	int size, dist, vert_factor;
	int degree_first, center_of_arc;


	/* Note the "size" */
	size = 2 + div_round(width + height, 22);

	/* Ask for a reasonable number of arcs. */
	*arc_num = 8 + (height * width / 80);
	*arc_num = rand_spread(*arc_num, 3);
	if (*arc_num < 8)  *arc_num = 8;
	if (*arc_num > 45) *arc_num = 45;

	/* Determine the start degrees and expansion distance for each arc. */
	for (degree_first = 0, i = 0; i < *arc_num; i++)
	{
		/* Get the first degree for this arc (using 180-degree circles). */
		arc_first[i] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(180, *arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 180 * (i+1) / *arc_num)
		    degree_first = 180 * (i+1) / *arc_num;
		if (degree_first > (180 + *arc_num) * (i+1) / *arc_num)
		    degree_first = (180 + *arc_num) * (i+1) / *arc_num;


		/* Get the center of the arc (convert from 180 to 360 circle). */
		center_of_arc = degree_first + arc_first[i];

		/* Get arc distance from the horizontal (0 and 180 degrees) */
		if      (center_of_arc <=  90) vert_factor = center_of_arc;
		else if (center_of_arc >= 270) vert_factor = ABS(center_of_arc - 360);
		else                           vert_factor = ABS(center_of_arc - 180);

		/*
		 * Usual case -- Calculate distance to expand outwards.  Pay more
		 * attention to width near the horizontal, more attention to height
		 * near the vertical.
		 */
		dist = ((height * vert_factor) + (width * (90 - vert_factor))) / 90;

		/* Randomize distance (should never be greater than radius) */
		arc_dist[i] = rand_range(dist / 4, dist / 2);

		/* Keep variability under control (except in special cases). */
		if ((dist != 0) && (i != 0))
		{
			int diff = arc_dist[i] - arc_dist[i-1];

			if (ABS(diff) > size)
			{
				if (diff > 0)
					arc_dist[i] = arc_dist[i-1] + size;
				else
					arc_dist[i] = arc_dist[i-1] - size;
			}
		}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (TRUE)
	{
		int diff = arc_dist[*arc_num - 1] - arc_dist[0];

		if (ABS(diff) > size)
		{
			if (diff > 0)
				arc_dist[*arc_num - 1] = arc_dist[0] + size;
			else
				arc_dist[*arc_num - 1] = arc_dist[0] - size;
		}
	}
}


/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (negative for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   y,x: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *   degrees: How wide an arc spell is (in degrees).
 *   source_diameter: how wide the source diameter is.
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
* At present, there are five major types of projections:
 *
 * Point-effect projection:  (no PROJECT_BEAM flag, radius of zero, and either
 *   jumps directly to target or has a single source and target grid)
 * A point-effect projection has no line of projection, and only affects one
 *   grid.  It is used for most area-effect spells (like dispel evil) and
 *   pinpoint strikes.
 *
 * Bolt:  (no PROJECT_BEAM flag, radius of zero, has to travel from source to
 *   target)
 * A bolt travels from source to target and affects only the last grid it
 *   enters.  If given the PROJECT_STOP flag, it is stopped by any
 *   monster or character in its path (at present, all bolts use this flag).
 *
 * Beam:  (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through
 *   with full damage.  It is never stopped by monsters in its path.  Beams
 *   may never be combined with any other projection type.
 *
 * Ball:  (positive radius, no PROJECT_ARC flag)
 * A ball travels from source towards the target, and always explodes.  Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in
 *   its path.  If given a target, it will explode on that target.  If a
 *   wall is in the way, it will explode against the wall.  If a ball reaches
 *   MAX_RANGE without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc:  (positive radius, with PROJECT_ARC flag)
 * An arc is a portion of a source-centered ball that explodes outwards
 *   towards the target grid.  Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area.  The width of arc spells is con-
 *   trolled by tthe variable "degrees".
 * An arc is created by rejecting all grids that form the endpoints of lines
 *   whose angular difference (in degrees) from the centerline of the arc is
 *   greater than one-half the input "degrees".  See the table "get_
 *   angle_to_grid" in "util.c" for more information.
 * Note:  An arc with a value for degrees of zero is actually a beam of
 *   defined length.
 *
 * Projections that affect all monsters in LOS are handled through the use
 *   of "project_los()", which applies a single-grid projection to individual
 *   monsters.  Projections that light up rooms or affect all monsters on the
 *   level are more efficiently handled through special functions.
 *
 *
 * Variations:
 *
 * PROJECT_STOP forces a path of projection to stop at the first occupied
 *   grid it hits.  This is used with bolts, and also by ball spells
 *   travelling in a specific direction rather than towards a target.
 *
 * PROJECT_THRU allows a path of projection towards a target to continue
 *   past that target.  This is appropriate for physical missiles (crossbow
 *   bolts, arrows, etc.)
 *
 * PROJECT_JUMP allows a projection to immediately set the source of the pro-
 *   jection to the target.  This is used for all area effect spells (like
 *   dispel evil), and can also be used for bombardments.
 *
 * PROJECT_WALL allows a projection, not just to affect one layer of any
 *   passable wall (rubble, trees), but to affect the surface of any wall.
 *   Certain projection types always have this flag.
 *
 * PROJECT_PASS allows projections to ignore walls completely.
 *   Certain projection types always have this flag.
 *
 * PROJECT_HIDE erases all graphical effects, making the projection
 *   invisible.
 *
 * PROJECT_GRID allows projections to affect terrain features.
 *
 * PROJECT_ITEM allows projections to affect objects on the ground.
 *
 * PROJECT_KILL allows projections to affect monsters.
 *
 * PROJECT_PLAY allows projections to affect the player.
 *
 * degrees controls the width of arc spells.  With a value for
 *   degrees of zero, arcs act like beams of defined length.
 *
 * source_diameter controls how quickly explosions lose strength with dis-
 *   tance from the target.  Most ball spells have a source diameter of 10,
 *   which means that they do 1/2 damage at range 1, 1/3 damage at range 2,
 *   and so on.   Caster-centered balls usually have a source diameter of 20,
 *   which allows them to do full damage to all adjacent grids.   Arcs have
 *   source diameters ranging up to 200, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *   It is expected, but not required, that wide arcs lose strength more
 *   quickly over distance.   (see the math in the "fire_arc()" function).
 *
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Implementation notes:
 *
 * If the source grid is not the same as the target, we project along the path
 *   between them.  Bolts stop if they hit anything, beams stop if they hit a
 *   wall, and balls and arcs may exhibit either behavior.  When they reach
 *   the final grid in the path, balls and arcs explode.  We do not allow beams
 *   to be combined with explosions.
 * Balls affect all floor grids in LOS (optionally, also wall grids adjacent
 *   to a grid in LOS) within their radius.  Arcs do the same, but only within
 *   their cone of projection.
 * Because affected grids are only scanned once, and it is really helpful to
 *   have explosions that travel outwards from the source, they are sorted by
 *   distance.  For each distance, an adjusted damage is calculated.
 * In successive passes, the code then displays explosion graphics, erases
 *   these graphics, marks terrain for possible later changes, affects
 *   objects, monsters, the character, and finally changes features and
 *   teleports monsters and characters in marked grids.
 *
 *
 * Usage and graphics notes:
 *
 * If the delay factor is anything other
 * than zero, bolt and explosion pictures will be momentarily shown on screen.
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * radius of standard ball attacks to nine units (diameter nineteen).  Arcs
 * can have larger radii; an arc capable of going out to range 20 should not
 * be wider than 70 degrees.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters on
 * both sides of a wall.
 *
 * Note that for consistency, we pretend that the bolt actually takes time
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the illumination of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
			 u32b flg, int degrees, byte source_diameter)
{
	int i, j, k, m;
	int dist = 0;

	u32b dam_temp;
	int centerline = 0;

	int y = y0;
	int x = x0;
	int n1y = 0;
	int n1x = 0;
	int y2, x2;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[256], gy[256];

	/* Distance to each of the affected grids. */
	byte gd[256];

	/* Precalculated damage values for each distance. */
	int dam_at_dist[MAX_RANGE+1];

	/*
	 * Starburst projections only --
	 * Holds first degree of arc, maximum effect distance in arc.
	 */
	byte arc_first[45];
	byte arc_dist[45];

	/* Number (max 45) of arcs. */
	int arc_num = 0;

	int degree, max_dist;

	/* Hack -- Flush any pending output */
	handle_stuff();

	/* Make certain that the radius is not too large */
	if (rad > MAX_SIGHT) rad = MAX_SIGHT;

	/* Some projection types always PROJECT_WALL. */
	if ((typ == GF_KILL_WALL) || (typ == GF_KILL_DOOR))
	{
		flg |= (PROJECT_WALL);
	}

	/* Hack -- Jump to target, but require a valid target */
	if ((flg & (PROJECT_JUMP)) && (y1) && (x1))
	{
		x = y0 = y1;
		x = x0 = x1;

	}

	/* If a single grid is both source and destination, store it. */
	if ((x1 == x0) && (y1 == y0))
	{
		gy[grids] = y0;
		gx[grids] = x0;
		gd[grids++] = 0;
	}

	/* Otherwise, travel along the projection path (unless arc or star). */
	else if (!(flg & (PROJECT_ARC | PROJECT_STAR)))
	{
		/* Determine maximum length of projection path */
		if (flg & (PROJECT_BOOM)) dist = MAX_RANGE;
		else if (rad <= 0)        dist = MAX_RANGE;
		else                      dist = rad;

		/* Calculate the projection path */
		(void)project_path(dist, y0, x0, &y1, &x1, flg);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int oy = y;
			int ox = x;

			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);

			/* Hack -- Balls explode before reaching walls. */
			if ((flg & (PROJECT_BOOM)) && (!cave_project_bold(ny, nx)))
			{
				break;
			}

			/* Advance */
			y = ny;
			x = nx;

			/* Grid is in projection path */
			if (path_gx[i] < PATH_G_NONE)
			{
				/* If a beam, collect all grids in the path. */
				if (flg & (PROJECT_BEAM))
				{
					gy[grids] = y;
					gx[grids] = x;
					gd[grids++] = 0;
				}

				/* Otherwise, collect only the final grid in the path. */
				else if (i == path_n - 1)
				{
					gy[grids] = y;
					gx[grids] = x;
					gd[grids++] = 0;
				}
			}

			/* Only do visuals if requested */
			if (!blind && !(flg & (PROJECT_HIDE)))
			{
				/* Only do visuals if the player can "see" the projection */
				if (panel_contains(y, x) && player_has_los_bold(y, x) &&
				    (path_gx[i] < PATH_G_NONE))
				{
					u16b p;

					byte a;
					char c;

					/* Obtain the bolt or explosion pict */
					if (flg & (PROJECT_BEAM)) p = bolt_pict(y, x, y, x, typ);
					else                      p = bolt_pict(oy, ox, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Display the visual effects */
					print_rel(c, a, y, x);
					move_cursor_relative(y, x);
					if (op_ptr->delay_factor)
					{
					    (void)Term_fresh();
						if (p_ptr->window) window_stuff();
					}

					/* Extra delay if monster in way, if PROJECT_STOP */
					if ((flg & (PROJECT_STOP)) && (cave_m_idx[y][x] != 0))
					{
						m = 10 + msec;

					}
					else m = msec;

					/* Delay */
					Term_xtra(TERM_XTRA_DELAY, m);

					/* Erase the visual effects, unless a beam */
					if (!(flg & (PROJECT_BEAM)))
					{
						lite_spot(y, x);
						if (op_ptr->delay_factor) (void)Term_fresh();
					}

					/* If a beam, erase later */
					else
					{
						drawn = TRUE;
					}

					/* Hack -- Activate delay */
					visual = TRUE;
				}

				/* Hack -- Always delay for consistency */
				else if (visual)
				{
					/* Delay for consistency */
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
		}
	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Beams have already stored all the grids they will affect. */
	if (flg & (PROJECT_BEAM))
	{
		/* No special actions */
	}

	/* Handle explosions */
	else if (flg & (PROJECT_BOOM))
	{
		/* Some projection types always PROJECT_WALL. */
		if (typ == GF_ACID)
		{
			/* Note that acid only affects monsters if it melts the wall. */
			flg |= (PROJECT_WALL);
		}

		/* Pre-calculate some things for starbursts. */
		if (flg & (PROJECT_STAR))
		{
			calc_starburst(1 + rad * 2, 1 + rad * 2, arc_first, arc_dist,
				&arc_num);

			/* Mark the area nearby -- limit range, ignore rooms */
			spread_cave_temp(y0, x0, rad, FALSE, (flg & (PROJECT_PASS)) != 0);

		}

		/* Pre-calculate some things for arcs. */
		if (flg & (PROJECT_ARC))
		{
			/* Calculate the first part of the projection path  XXX XX */
			(void)project_path(rad/2, y0, x0, &y1, &x1, flg);

			/* Store the grids  XXX XXX */
			for (i = 0; i < path_n; i++)
			{
				/* Grid is not skipped, and is not a wall */
				if ((path_gx[i] < PATH_G_NONE) &&
				    (path_gx[i] != PATH_G_WALL))
				{
					/* Save the grid */
					gy[grids] = GRID_Y(path_g[i]);
					gx[grids] = GRID_X(path_g[i]);
					gd[grids++] = i;

					/* Mark the grid (it will not be used again) */
					cave_temp_mark(gy[grids-1], gx[grids-1], FALSE);
				}
			}


			/* The radius of arcs cannot be more than 20 */
			if (rad > 20) rad = 20;

			/* Reorient the grid forming the end of the arc's centerline */
			n1y = y1 - y0 + 20;
			n1x = x1 - x0 + 20;

			/* Correct overly large or small values */
			if (n1y > 40) n1y = 40;
			if (n1x > 40) n1x = 40;
			if (n1y <  0) n1y =  0;
			if (n1x <  0) n1x =  0;

			/* Get the angle of the arc's centerline */
			centerline = 90 - get_angle_to_grid[n1y][n1x];
		}


		/*
		 * If the center of the explosion hasn't been
		 * saved already, save it now.
		 */
		if (grids == 0)
		{
			gy[grids] = y2;
			gx[grids] = x2;
			gd[grids++] = 0;
		}

		/*
		 * Scan every grid that might possibly
		 * be in the blast radius.
		 */
		for (y = y2 - rad; y <= y2 + rad; y++)
		{
			for (x = x2 - rad; x <= x2 + rad; x++)
			{
				/* Center grid has already been stored. */
				if ((y == y2) && (x == x2)) continue;

				/* Precaution: Stay within area limit. */
				if (grids >= 255) break;

				/* Ignore "illegal" locations */
				if (!in_bounds(y, x)) continue;

				/* This is a non-projectable grid */
				if (!cave_project_bold(y, x))

				{
					/* Spell with PROJECT_PASS ignore these grids */
					if (!(flg & (PROJECT_PASS)))
					{
						/* PROJECT_WALL is active or terrain is passable */
						if ((flg & (PROJECT_WALL)) || cave_passable_bold(y, x))
						{
							/* Allow grids next to grids in LOS of explosion center */
							for (i = 0, k = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Stay within dungeon */
								if (!in_bounds(yy, xx)) continue;

								if (generic_los(y2, x2, yy, xx, CAVE_PROJECT))
								{
									k++;
									break;
								}
							}

							/* Require at least one adjacent grid in LOS */
							if (!k) continue;
						}

						/* We can't affect this non-passable wall */
						else continue;
					}
				}

				/* Must be within maximum distance. */
				dist = (distance(y2, x2, y, x));
				if (dist > rad) continue;


				/* Projection is a starburst */
				if (flg & (PROJECT_STAR))
				{
					/* Grid is within effect range */
					if (cave_info[y][x] & (CAVE_TEMP))
					{
						/* Reorient current grid for table access. */
						int ny = y - y2 + 20;
						int nx = x - x2 + 20;

						/* Illegal table access is bad. */
						if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
							continue;

						/* Get angle to current grid. */
						degree = get_angle_to_grid[ny][nx];

						/* Scan arcs to find the one that applies here. */
						for (i = arc_num - 1; i >= 0; i--)
						{
							if (arc_first[i] <= degree)
							{
								max_dist = arc_dist[i];

								/* Must be within effect range. */
								if (max_dist >= dist)
								{
									gy[grids] = y;
									gx[grids] = x;
									gd[grids] = 0;
									grids++;
								}

								/* Arc found.  End search */
								break;
							}
						}
					}
				}

				/* Use angle comparison to delineate an arc. */
				else if (flg & (PROJECT_ARC))
				{
					int n2y, n2x, tmp, diff;

					/* Skip already marked grids */
					if (cave_info[y][x] & (CAVE_TEMP)) continue;

					/* Reorient current grid for table access. */
					n2y = y - y2 + 20;
					n2x = x - x2 + 20;

					/*
					 * Find the angular difference (/2) between
					 * the lines to the end of the arc's center-
					 * line and to the current grid.
					 */
					tmp = ABS(get_angle_to_grid[n2y][n2x] + centerline) % 180;
					diff = ABS(90 - tmp);

					/*
					 * If difference is not greater then that
					 * allowed, and the grid is in LOS, accept it.
					 */
					if (diff < (degrees + 6) / 4)
					{
						if (generic_los(y2, x2, y, x, CAVE_PROJECT))
						{
							gy[grids] = y;
							gx[grids] = x;
							gd[grids] = dist;
							grids++;
						}
					}
				}

				/* Standard ball spell -- accept all grids in LOS. */
				else
				{
					if (flg & (PROJECT_PASS) || generic_los(y2, x2, y, x, CAVE_PROJECT))
					{
						gy[grids] = y;
						gx[grids] = x;
						gd[grids] = dist;
						grids++;
					}
				}
			}
		}
	}

	/* Clear the "temp" array  XXX */
	clear_temp_array();

	/* Calculate and store the actual damage at each distance. */
	for (i = 0; i <= MAX_RANGE; i++)
	{
		/* No damage outside the radius. */
		if (i > rad) dam_temp = 0;

		/* Standard damage calc. for 10' source diameters, or at origin. */
		else if ((!source_diameter) || (i == 0))
		{
			dam_temp = (dam + i) / (i + 1);
		}

		/*
		 * If a particular diameter for the source of the explosion's
		 * energy is given, calculate an adjusted damage.
		 */
		else
		{
			dam_temp = (source_diameter * dam) / ((i + 1) * 10);
			if (dam_temp > (u32b)dam) dam_temp = dam;
		}

		/* Store it. */
		dam_at_dist[i] = dam_temp;
	}

	/* Sort the blast grids by distance, starting at the origin. */
	for (i = 0, k = 0; i < rad; i++)
	{
		int tmp_y, tmp_x, tmp_d;

		/* Collect all the grids of a given distance together. */
		for (j = k; j < grids; j++)
		{
			if (gd[j] == i)
			{
				tmp_y = gy[k];
				tmp_x = gx[k];
				tmp_d = gd[k];

				gy[k] = gy[j];
				gx[k] = gx[j];
				gd[k] = gd[j];

				gy[j] = tmp_y;
				gx[j] = tmp_x;
				gd[j] = tmp_d;

				/* Write to next slot */
				k++;
			}
		}
	}


	/* Display the blast area if allowed. (unless a bolt) */
	if (!blind && !(flg & (PROJECT_HIDE)) && ((grids > 1) || (dist == 0)))
	{
		/* Do the blast from inside out */
		for (i = 0; i < grids; i++)
		{
			/* Extract the location */
			y = gy[i];
			x = gx[i];

			/* Only do visuals if the player can "see" the blast */
			if (player_has_los_bold(y, x))
			{
				u16b p;

				byte a;
				char c;

				drawn = TRUE;

				/* Obtain the explosion pict */
				p = bolt_pict(y, x, y, x, typ);

				/* Extract attr/char */
				a = PICT_A(p);
				c = PICT_C(p);

				/* Visual effects -- Display */
				print_rel(c, a, y, x);
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* New radius is about to be drawn */
			if ((i == grids - 1) || ((i < grids - 1) && (gd[i + 1] > gd[i])))
			{
				/* Flush each radius separately */
				if (op_ptr->delay_factor)
				{
				    (void)Term_fresh();
					if (p_ptr->window) window_stuff();
				}

				/* Flush */
				if (p_ptr->window) window_stuff();

				/* Delay (efficiently) */
				if (visual || drawn)
				{
					m = (rad <= 4 ? msec : (rad <= 8 ? 2*msec/3 : msec/2));
					Term_xtra(TERM_XTRA_DELAY, m);
				}
			}
		}

		/* Delay for a while if there are pretty graphics to show */
		/* Delay for a while if there are pretty graphics to show */
		if ((grids > 1) && (visual || drawn))
		{
			if (!op_ptr->delay_factor) (void)Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, who <= 0 ? msec*2 : msec);
		}
	}

	/* Flush the erasing -- except if we specify lingering graphics */
	if ((drawn) && (!(flg & (PROJECT_NO_REDRAW))))
	{
		/* Erase the explosion drawn above */
		for (i = 0; i < grids; i++)
		{
			/* Extract the location */
			y = gy[i];
			x = gx[i];

			/* Hack -- Erase if needed */
			if (player_has_los_bold(y, x))
			{
				lite_spot(y, x);
			}
		}

		/* Hack -- center the cursor */
		move_cursor_relative(y2, x2);

		/* Flush the explosion */
		if (op_ptr->delay_factor)
		{
			(void)Term_fresh();
			if (p_ptr->window) window_stuff();
		}
	}

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

	/* Check objects */
	if (flg & (PROJECT_ITEM))
	{

		/* Scan for objects */
		for (i = 0; i < grids; i++)
		{

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the object in the grid */
			if (project_o(who, y, x, dam_at_dist[gd[i]], typ)) notice = TRUE;
		}
	}


	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* No monster here */
			if (cave_m_idx[y][x] <= SOURCE_MONSTER_START) continue;

			/* Affect the monster in the grid */
			if (project_m(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}

		/* Player affected one monster (without "jumping") */
		if ((who == SOURCE_PLAYER) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave_m_idx[y][x] > SOURCE_MONSTER_START)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);
			}
		}
	}

	/* Check player */
	if (flg & (PROJECT_PLAY))
	{

		/* Scan for player */
		for (i = 0; i < grids; i++)
		{

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Player is in this grid */
			if (cave_m_idx[y][x] < 0)
			{

				/* Affect the player */
				if (project_p(who, y, x, dam_at_dist[gd[i]], typ, NULL))
				{
					notice = TRUE;

					/* Only affect the player once */
					break;
				}
			}
		}
	}

	/* Check features */
	if (flg & (PROJECT_GRID))
	{

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_f(who, y, x, gd[i], dam_at_dist[gd[i]], typ))
				notice = TRUE;
		}
	}

	/* Check effects */
	if (flg & (PROJECT_EFCT))
	{

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_x(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}
	}

	/* Clear the "temp" array  (paranoia is good) */
	clear_temp_array();

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

	/* Return "something was noticed" */
	return (notice);
}




