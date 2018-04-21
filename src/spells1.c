/* File: spells1.c */

/* Purpose: Spell code (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version),
 * or under the terms of the traditional Angband license.
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2),
 * or under the terms of the traditional Angband license.
 */

#include "angband.h"

/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16

/* 1/x chance of hurting even if invulnerable!*/
#define PENETRATE_INVULNERABILITY 13


extern void do_poly_self();
extern bool item_tester_hook_armour(object_type *o_ptr);
extern int get_spell(int *sn, cptr prompt, int use_realm , int sval, bool known );
extern void do_cmd_rerate(void);

/*
* Helper function -- return a "nearby" race for polymorphing
*
* Note that this function is one of the more "dangerous" ones...
*/
s16b poly_r_idx(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	int i, r, lev1, lev2;

	/* Hack -- Uniques never polymorph */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint(20)/randint(9))+1);
	lev2 = r_ptr->level + ((randint(20)/randint(9))+1);

	/* Pick a (possibly new) non-unique race */
	for (i = 0; i < 1000; i++)
	{
		/* Pick a new race, using a level calculation */
		r = get_mon_num((dun_level + r_ptr->level) / 2 + 5);

		/* Handle failure */
		if (!r) break;

		/* Obtain race */
		r_ptr = &r_info[r];

		/* Ignore unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Ignore monsters with incompatible levels */
		if ((r_ptr->level < lev1) || (r_ptr->level > lev2)) continue;

		/* Use that index */
		r_idx = r;

		/* Done */
		break;
	}

	/* Result */
	return (r_idx);
}


/*
* Teleport a monster, normally up to "dis" grids away.
*
* Attempt to move the monster at least "dis/2" grids away.
*
* But allow variation to prevent infinite loops.
*/
void teleport_away(int m_idx, int dis)
{
	int                     ny, nx, oy, ox, d, i, min;

	bool            look = TRUE;

	monster_type    *m_ptr = &m_list[m_idx];


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
			if (!in_bounds(ny, nx)) continue;

			/* Require "empty" floor space */
			/*if (!cave_empty_bold(ny, nx) || (cave[ny][nx].feat == FEAT_WATER && !water_ok(m_ptr->r_idx))) continue;*/
			if(!can_place_monster(ny,nx,m_ptr->r_idx))continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave[ny][nx].feat == FEAT_GLYPH) continue;
			if (cave[ny][nx].feat == FEAT_MINOR_GLYPH) continue;

			/* No teleporting into vaults and such */
			/* if (cave[ny][nx].info & (CAVE_ICKY)) continue; */

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
	sound(SOUND_TPOTHER);

	/* Update the new location */
	cave[ny][nx].m_idx = m_idx;

	/* Update the old location */
	cave[oy][ox].m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(oy, ox);

	/* Redraw the new grid */
	lite_spot(ny, nx);
}



/*
* Teleport monster next to the player
*/
void teleport_to_player(int m_idx)
{
	int  oy, ox, d, i, min;
	int  nx   = 0;
	int  ny   = 0;
	int  dis  = 2;

	bool look = TRUE;

	monster_type    *m_ptr = &m_list[m_idx];
	int attempts = 500;


	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* "Skill" test */
	if (randint(100) > r_info[m_ptr->r_idx].level) return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while ((look) && --attempts)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				ny = rand_spread(py, dis);
				nx = rand_spread(px, dis);
				d = distance(py, px, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds(ny, nx)) continue;

			/* Require "empty" floor space */
			/*if (!cave_empty_bold(ny, nx) || (cave[ny][nx].feat == FEAT_WATER && !water_ok(m_ptr->r_idx))) continue;*/
			if(!can_place_monster(ny,nx,m_ptr->r_idx))continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave[ny][nx].feat == FEAT_GLYPH) continue;
			if (cave[ny][nx].feat == FEAT_MINOR_GLYPH) continue;

			/* No teleporting into vaults and such */
			/* if (cave[ny][nx].info & (CAVE_ICKY)) continue; */

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

	if (attempts < 1) return;

	/* Sound */
	sound(SOUND_TPOTHER);

	/* Update the new location */
	cave[ny][nx].m_idx = m_idx;

	/* Update the old location */
	cave[oy][ox].m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(oy, ox);

	/* Redraw the new grid */
	lite_spot(ny, nx);
}


/*
* Teleport the player to a location up to "dis" grids away.
*
* If no such spaces are readily available, the distance may increase.
* Try very hard to move the player at least a quarter that distance.
*/
void teleport_player(int dis)
{
	int d, i, min, ox, oy, x = py, y = px;

	int xx = -1, yy = -1;

	bool look = TRUE;

	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	if (dis > 200) dis = 200; /* To be on the safe side... */

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
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Require "naked" floor space */
			if (!cave_naked_bold(y, x)) continue;

			/* No teleporting into vaults and such */
			if (cave[y][x].info & (CAVE_ICKY)) continue;

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
	sound(SOUND_TELEPORT);

	/* Save the old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	/* Redraw the old spot */
	lite_spot(oy, ox);

	while (xx < 2)
	{
		yy = -1;

		while (yy < 2)
		{
			if (xx == 0 && yy == 0)
			{
				/* Do nothing */
			}
			else
			{
				if (cave[oy+yy][ox+xx].m_idx)
				{
					if ((r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags6
						& RF6_TPORT) &&
						!(r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags3
						& RF3_RES_TELE))
						/* The latter limitation is to avoid totally
						unkillable suckers... */
					{
						if (!(m_list[cave[oy+yy][ox+xx].m_idx].csleep))
							teleport_to_player(cave[oy+yy][ox+xx].m_idx);
					}
				}
			}
			yy++;
		}
		xx++;
	}

	/* Redraw the new spot */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

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
	int y, x, oy, ox, dis = 0, ctr = 0;

	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	/* Find a usable location */
	while (1)
	{
		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds(y, x)) break;
		}

		/* Accept "naked" floor grids */
		if (cave_naked_bold(y, x)) break;

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
	sound(SOUND_TELEPORT);

	/* Save the old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	/* Redraw the old spot */
	lite_spot(oy, ox);

	/* Redraw the new spot */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
* Teleport the player one level up or down (random when legal)
*/
void teleport_player_level(void)
{
	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	if (dun_level <= 0)
	{
		msg_print("You sink through the floor.");
		if (autosave_l)
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
		dun_level++;
		new_level_flag = TRUE;
	}
	else if (is_quest(dun_level) || (dun_level >= MAX_DEPTH))
	{
		msg_print("You rise up through the ceiling.");
		if (autosave_l)
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
		dun_level--;
		new_level_flag = TRUE;
	}
	else if (rand_int(100) < 50)
	{
		msg_print("You rise up through the ceiling.");
		if (autosave_l)
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
		dun_level--;
		new_level_flag = TRUE;
		came_from=START_RANDOM;
	}
	else
	{
		msg_print("You sink through the floor.");
		if (autosave_l)
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
		dun_level++;
		new_level_flag = TRUE;
	}

	/* Sound */
	sound(SOUND_TPLEVEL);
}

/*
* Get a legal "multi-hued" colour for drawing "spells"
*/
static byte mh_attr(int max)
{
	switch (randint(max))
	{
	case 1: return (TERM_RED);
	case 2: return (TERM_GREEN);
	case 3: return (TERM_BLUE);
	case 4: return (TERM_YELLOW);
	case 5: return (TERM_ORANGE);
	case 6: return (TERM_VIOLET);
	case 7: return (TERM_L_RED);
	case 8: return (TERM_L_GREEN);
	case 9: return (TERM_L_BLUE);
	case 10: return (TERM_UMBER);
	case 11: return (TERM_L_UMBER);
	case 12: return (TERM_SLATE);
	case 13: return (TERM_WHITE);
	case 14: return (TERM_L_WHITE);
	case 15: return (TERM_L_DARK);
	}

	return (TERM_WHITE);
}


/*
* Return a colour to use for the bolt/ball spells in ascii mode
*/
static byte spell_colour(int type)
{
	/* Hack -- fake monochrome */
	if (!use_colour) return (TERM_WHITE);

	/* Analyze */
	switch (type)
	{
	case GF_MISSILE:        return (TERM_SLATE);
	case GF_EXP:	        return (TERM_YELLOW);
	case GF_ACID:           return (randint(5)<3?TERM_YELLOW:TERM_L_GREEN);
	case GF_ELEC:           return (randint(7)<6?TERM_WHITE:(randint(4)==1?TERM_BLUE:TERM_L_BLUE));
	case GF_FIRE:           return (randint(6)<4?TERM_YELLOW:(randint(4)==1?TERM_RED:TERM_L_RED));
	case GF_COLD:           return (randint(6)<4?TERM_WHITE:TERM_L_WHITE);
	case GF_POIS:           return (randint(5)<3?TERM_L_GREEN:TERM_GREEN);
	case GF_HOLY_FIRE:       return (randint(5)==1?TERM_ORANGE:TERM_WHITE);
	case GF_HELL_FIRE:       return (randint(6)==1?TERM_RED:TERM_L_DARK);
	case GF_MANA:           return (randint(5)!=1?TERM_VIOLET:TERM_L_BLUE);
	case GF_ARROW:          return (TERM_L_UMBER);
	case GF_WATER:          return (randint(4)==1?TERM_L_BLUE:TERM_BLUE);
	case GF_NETHER:         return (randint(4)==1?TERM_SLATE:TERM_L_DARK);
	case GF_CHAOS:          return (mh_attr(15));
	case GF_DISENCHANT:     return (randint(5)!=1?TERM_L_BLUE:TERM_VIOLET);
	case GF_NEXUS:          return (randint(5)<3?TERM_L_RED:TERM_VIOLET);
	case GF_CONFUSION:      return (mh_attr(4));
	case GF_SOUND:          return (randint(4)==1?TERM_VIOLET:TERM_WHITE);
	case GF_SHARDS:         return (randint(5)<3?TERM_UMBER:TERM_SLATE);
	case GF_FORCE:          return (randint(5)<3?TERM_L_WHITE:TERM_ORANGE);
	case GF_INERTIA:        return (randint(5)<3?TERM_SLATE:TERM_L_WHITE);
	case GF_GRAVITY:        return (randint(3)==1?TERM_L_UMBER:TERM_UMBER);
	case GF_TIME:           return (randint(2)==1?TERM_WHITE:TERM_L_DARK);
	case GF_LITE_WEAK:      return (randint(3)==1?TERM_ORANGE:TERM_YELLOW);
	case GF_LITE:           return (randint(4)==1?TERM_ORANGE:TERM_YELLOW);
	case GF_DARK_WEAK:      return (randint(3)==1?TERM_DARK:TERM_L_DARK);
	case GF_DARK:           return (randint(4)==1?TERM_DARK:TERM_L_DARK);
	case GF_PLASMA:         return (randint(5)==1?TERM_RED:TERM_L_RED);
	case GF_METEOR:         return (randint(3)==1?TERM_RED:TERM_UMBER);
	case GF_ICE:            return (randint(4)==1?TERM_L_BLUE:TERM_WHITE);
	case GF_SHARD:         return (randint(6)<4?TERM_L_RED:(randint(4)==1?TERM_RED:TERM_L_UMBER));
	case GF_DEATH_RAY:      return (TERM_L_DARK);
	case GF_HELLSLIME:           return (mh_attr(2));
	case GF_DISINTEGRATE:   return (randint(3)!=1?TERM_L_DARK:(randint(2)==1?TERM_ORANGE:TERM_L_UMBER));
	case GF_PSI:
	case GF_PSI_DRAIN:
	case GF_TELEKINESIS:
	case GF_DOMINATION:		return (randint(3)!=1?TERM_L_BLUE:TERM_WHITE);
    case GF_HECATE:           return (randint(4)==1?TERM_ORANGE:TERM_YELLOW);;
	}

	/* Standard "colour" */
	return (TERM_WHITE);
}
/*
* Return an attr to use for the bolt spells in graphics mode
*/
static byte bolt_graf_attr(int type)
{
	/* Analyze */
	switch (type)
	{
	case GF_MISSILE:        return (144);
	case GF_ACID:           return (143);
	case GF_ELEC:           return (143);
	case GF_FIRE:           return (143);
	case GF_COLD:           return (143);
	case GF_POIS:           return (143);
	case GF_HOLY_FIRE:       return (144);
	case GF_HELL_FIRE:       return (144);
	case GF_MANA:           return (143);
	case GF_ARROW:          return (144);
	case GF_WATER:          return (143);
	case GF_NETHER:         return (143);
	case GF_CHAOS:          return (144);
	case GF_DISENCHANT:     return (143);
	case GF_NEXUS:          return (144);
	case GF_CONFUSION:      return (144);
	case GF_SOUND:          return (144);
	case GF_SHARDS:         return (145);
	case GF_FORCE:          return (144);
	case GF_INERTIA:        return (145);
	case GF_GRAVITY:        return (145);
	case GF_TIME:           return (144);
	case GF_LITE_WEAK:      return (143);
	case GF_LITE:           return (143);
	case GF_HECATE:         return (143);
	case GF_DARK_WEAK:      return (143);
	case GF_DARK:           return (143);
	case GF_PLASMA:         return (144);
	case GF_METEOR:         return (145);
	case GF_ICE:            return (143);
	case GF_SHARD:         return (145);
	case GF_DEATH_RAY:      return (144);
	case GF_HELLSLIME:           return (144);
	case GF_DISINTEGRATE:   return (144);
	case GF_PSI:
	case GF_PSI_DRAIN:
	case GF_TELEKINESIS:
	case GF_DOMINATION:		return (144);
	case GF_MAGEBOLT:       return (143);
	}

	/* Standard attr */
	return (144);
}
/*
* Return an attr to use for the ball spells in graphics mode
*/
static byte ball_graf_attr(int type)
{
	(void)type;/* For now, we will always use the same attr */
	return(145);
}

/*
* Return a char to use for the ball spells in graphics mode
*/
static byte ball_graf_char(int type)
{
	/* Analyze */
	switch (type)
	{
	case GF_MISSILE:        return (151);
	case GF_ACID:           return (140);
	case GF_ELEC:           return (138);
	case GF_FIRE:           return (136);
	case GF_COLD:           return (137);
	case GF_POIS:           return (139);
	case GF_HOLY_FIRE:       return (147);
	case GF_HELL_FIRE:       return (148);
	case GF_MANA:           return (141);
	case GF_ARROW:          return (151);
	case GF_WATER:          return (140);
	case GF_NETHER:         return (143);
	case GF_CHAOS:          return (149);
	case GF_DISENCHANT:     return (141);
	case GF_NEXUS:          return (150);
	case GF_CONFUSION:      return (149);
	case GF_SOUND:          return (146);
	case GF_SHARDS:         return (152);
	case GF_FORCE:          return (150);
	case GF_INERTIA:        return (153);
	case GF_GRAVITY:        return (153);
	case GF_TIME:           return (145);
	case GF_LITE_WEAK:      return (142);
	case GF_LITE:           return (142);
	case GF_HECATE:         return (142);
	case GF_DARK_WEAK:      return (143);
	case GF_DARK:           return (143);
	case GF_PLASMA:         return (147);
	case GF_METEOR:         return (152);
	case GF_ICE:            return (137);
	case GF_SHARD:         return (152);
	case GF_DEATH_RAY:      return (TERM_L_DARK);
	case GF_HELLSLIME:           return (147);
	case GF_DISINTEGRATE:   return (145);
	case GF_PSI:
	case GF_PSI_DRAIN:
	case GF_TELEKINESIS:
	case GF_DOMINATION:		return (144);
	case GF_MAGEBOLT:       return (141);
	}

	/* Standard "char" */
	return (151);
}
/*
* Return a base char to use for the bolt spells in graphics mode
*/
static byte base_bolt_char(int type)
{
	/* Analyze */
	switch (type)
	{
	case GF_MISSILE:        return (156);
	case GF_ACID:           return (144);
	case GF_ELEC:           return (136);
	case GF_FIRE:           return (128);
	case GF_COLD:           return (132);
	case GF_POIS:           return (140);
	case GF_HOLY_FIRE:       return (140);
	case GF_HELL_FIRE:       return (144);
	case GF_MANA:           return (148);
	case GF_ARROW:          return (156);
	case GF_WATER:          return (144);
	case GF_NETHER:         return (156);
	case GF_CHAOS:          return (148);
	case GF_DISENCHANT:     return (148);
	case GF_NEXUS:          return (152);
	case GF_CONFUSION:      return (148);
	case GF_SOUND:          return (136);
	case GF_SHARDS:         return (128);
	case GF_FORCE:          return (152);
	case GF_INERTIA:        return (132);
	case GF_GRAVITY:        return (132);
	case GF_TIME:           return (132);
	case GF_LITE_WEAK:      return (152);
	case GF_LITE:           return (152);
	case GF_HECATE:         return (152);
	case GF_DARK_WEAK:      return (156);
	case GF_DARK:           return (156);
	case GF_PLASMA:         return (140);
	case GF_METEOR:         return (128);
	case GF_ICE:            return (132);
	case GF_SHARD:         return (128);
	case GF_DEATH_RAY:      return (132);
	case GF_HELLSLIME:           return (140);
	case GF_DISINTEGRATE:   return (132);
	case GF_PSI:
	case GF_PSI_DRAIN:
	case GF_TELEKINESIS:
	case GF_DOMINATION:		return (128);
	case GF_MAGEBOLT:       return (148);
	}

	/* Standard "char" */
	return (156);
}


/*
* Decreases players hit points and sets death flag if necessary
*
* XXX XXX XXX Invulnerability needs to be changed into a "shield"
*
* XXX XXX XXX Hack -- this function allows the user to save (or quit)
* the game when he dies, since the "You die." message is shown before
* setting the player to "dead".
*/
void take_hit(int damage, cptr hit_from)
{
	int old_chp = p_ptr->chp;

	bool pen_invuln = FALSE;

	/*char death_message[80];*/

	int warning = (p_ptr->mhp * hitpoint_warn / 10);

	/* Paranoia */
	if (death) return;

	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (damage < 9000))
	{ if (randint(PENETRATE_INVULNERABILITY)==1)
	{
		pen_invuln = TRUE;
	}
	else
	{
		return;
	}
	}
	/* Wraith form is awesome for preventing damage ;] */
	if (p_ptr->wraith_form)
	{
		damage /= 10;
		if ((damage==0) && (randint(10)==1))
			damage = 1;
	}

	/* Hurt the player */
	p_ptr->chp -= damage;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	if (pen_invuln)
		msg_print("The attack penetrates your shield of invulnerability!");

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Sound */
		sound(SOUND_DEATH);
		/* Don't give confusing death message if player has ritual */
		if(p_ptr->ritual == 0)
		{
			/* Hack -- Note death */
			msg_print("You die.");
			msg_print(NULL);
		}
		/* Note cause of death */
		(void)strcpy(died_from, hit_from);

		if (p_ptr->image) strcat(died_from,"(?)");

		/* No longer a winner */
		total_winner = FALSE;

		/* Note death */
		death = TRUE;

		/* Again, don't print message if player has ritual */
		if(p_ptr->ritual == 0)
		{
			if (get_check("Dump the screen? "))
			{
				do_cmd_save_screen();
			}
		}
		/* Dead */
		return;
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_chp > warning)) bell();

		sound(SOUND_WARN);

		/* Message */
		msg_print("*** LOW HITPOINT WARNING! ***");
		msg_print(NULL);
	}
}





/*
* Note that amulets, rods, and high-level spell books are immune
* to "inventory damage" of any kind.  Also sling ammo and shovels.
*/


/*
* Does a given class of objects (usually) hate acid?
* Note that acid can either melt or corrode something.
*/
static bool hates_acid(object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable items */
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
	case TV_DRAG_ARMOR:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls are wood/paper */
	case TV_STAFF:
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
static bool hates_elec(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_RING:
	case TV_WAND:
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
static bool hates_fire(object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
	case TV_LITE:
	case TV_ARROW:
	case TV_BOW:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_CLOAK:
	case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Books */
	case TV_MIRACLES_BOOK:
	case TV_SORCERY_BOOK:
	case TV_NATURE_BOOK:
	case TV_CHAOS_BOOK:
	case TV_DEATH_BOOK:
	case TV_TAROT_BOOK:
	case TV_CHARMS_BOOK:
	case TV_SOMATIC_BOOK:
    /* Demonic books due their Infernal nature do not burn, muhahah */
		{
			return (TRUE);
		}

		/* Chests */
	case TV_CHEST:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls burn */
	case TV_STAFF:
	case TV_SCROLL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
* Does a given object (usually) hate cold?
*/
static bool hates_cold(object_type *o_ptr)
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
* Melt something
*/
static int set_acid_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & (TR3_IGNORE_ACID)) return (FALSE);
	return (TRUE);
}


/*
* Electrical damage
*/
static int set_elec_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & (TR3_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}


/*
* Burn something
*/
static int set_fire_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);
	return (TRUE);
}


/*
* Freeze things
*/
static int set_cold_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_cold(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & (TR3_IGNORE_COLD)) return (FALSE);
	return (TRUE);
}




/*
* This seems like a pretty standard "typedef"
*/
typedef int (*inven_func)(object_type *);

/*
* Destroys a type of item on a given percent chance
* Note that missiles are no longer necessarily all destroyed
* Destruction taken from "melee.c" code for "stealing".
* Returns number of items destroyed.
*/
static int inven_damage(inven_func typ, int perc)
{
	int             i, j, k, amt;

	object_type     *o_ptr;

	char    o_name[80];


	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artefacts */
		if (artefact_p(o_ptr) || o_ptr->art_name) continue;

		/* Give this item slot a shot at death */
		if ((*typ)(o_ptr))
		{
			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (rand_int(100) < perc) amt++;
			}

			/* Some casualities */
			if (amt)
			{
				/* Get a description */
				object_desc(o_name, o_ptr, FALSE, 3);

				/* Message */
				msg_format("%sour %s (%c) %s destroyed!",
					((o_ptr->number > 1) ?
					((amt == o_ptr->number) ? "All of y" :
				(amt > 1 ? "Some of y" : "One of y")) : "Y"),
					o_name, index_to_label(i),
					((amt > 1) ? "were" : "was"));

				/* Potions smash open */
				if (k_info[o_ptr->k_idx].tval == TV_POTION) {
					(void)potion_smash_effect(0, py, px, o_ptr, NULL);
				}


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
* Acid has hit the player, attempt to affect some armour.
*
* Note that the "base armour" of an object never changes.
*
* If any armour is damaged (or resists), the player takes less damage.
*/
static int minus_ac(void)
{
	object_type             *o_ptr = NULL;

	u32b            f1, f2, f3;

	char            o_name[80];


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
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

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
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	/* Item was damaged */
	return (TRUE);
}


/*
* Hurt the player with Acid
*/
void acid_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total Immunity */
	if (p_ptr->immune_acid || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & COR3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = (dam + 2) / 3;
	if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_acid || p_ptr->resist_acid))
		&& randint(HURT_CHANCE)==1)
		(void) do_dec_stat(A_CHA);

	/* If any armour gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (!(p_ptr->oppose_acid && p_ptr->resist_acid))
		inven_damage(set_acid_destroy, inv);
}


/*
* Hurt the player with electricity
*/
void elec_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_elec || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & COR3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
	if (p_ptr->resist_elec) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_elec || p_ptr->resist_elec))
		&& randint(HURT_CHANCE)==1)
		(void) do_dec_stat(A_DEX);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (!(p_ptr->oppose_elec && p_ptr->resist_elec))
		inven_damage(set_elec_destroy, inv);
}




/*
* Hurt the player with Fire
*/
void fire_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Totally immune */
	if (p_ptr->immune_fire || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & COR3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = (dam + 2) / 3;
	if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_fire || p_ptr->resist_fire))
		&& randint(HURT_CHANCE)==1)
		(void) do_dec_stat(A_STR);


	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (!(p_ptr->resist_fire && p_ptr->oppose_fire))
		inven_damage(set_fire_destroy, inv);
}


/*
* Hurt the player with Cold
*/
void cold_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_cold || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_ptr->muta3 & COR3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = (dam + 2) / 3;
	if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_cold || p_ptr->resist_cold))
		&& randint(HURT_CHANCE)==1)
		(void) do_dec_stat(A_STR);


	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (!(p_ptr->resist_cold && p_ptr->oppose_cold))
		inven_damage(set_cold_destroy, inv);
}





/*
* Increases a stat by one randomized level             -RAK-
*
* Note that this function (used by stat potions) now restores
* the stat BEFORE increasing it.
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
			gain = ((rand_int(100) < 75) ? 1 : 2);
			value += gain;
		}

		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18+98)
		{
			/* Approximate gain value */
			gain = (((18+100) - value) / 2 + 3) / 2;

			/* Paranoia */
			if (gain < 1) gain = 1;

			/* Apply the bonus */
			value += randint(gain) + gain / 2;

			/* Maximal value */
			if (value > 18+99) value = 18 + 99;
		}

		/* Gain one point at a time */
		else
		{
			value++;
		}

		/* Save the new value */
		p_ptr->stat_cur[stat] = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat_max[stat])
		{
			p_ptr->stat_max[stat] = value;
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}



/*
* Decreases a stat by an amount indended to vary from 0 to 100 percent.
*
* Amount could be a little higher in extreme cases to mangle very high
* stats from massive assaults.  -CWS
*
* Note that "permanent" means that the *given* amount is permanent,
* not that the new value becomes permanent.  This may not work exactly
* as expected, due to "weirdness" in the algorithm, but in general,
* if your stat is already drained, the "max" value will not drop all
* the way down to the "cur" value.
*/
bool dec_stat(int stat, int amount, int permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Acquire current value */
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
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((cur-18) / 2 + 1) / 2 + 1);

			/* Paranoia */
			if (loss < 1) loss = 1;

			/* Randomize the loss */
			loss = ((randint(loss) + loss) * amount) / 100;

			/* Maximal loss */
			if (loss < amount/2) loss = amount/2;

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
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((max-18) / 2 + 1) / 2 + 1);
			loss = ((randint(loss) + loss) * amount) / 100;
			if (loss < amount/2) loss = amount/2;

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

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}




/*
* Apply disenchantment to the player's stuff
*
* XXX XXX XXX This function is also called from the "melee" code
*
* The "mode" is currently unused.
*
* Return "TRUE" if the player notices anything
*/
bool apply_disenchant(int mode)
{
	int                     t = 0;

	object_type             *o_ptr;

	char            o_name[80];


	/* Unused, for now we only accept 0 */
	if(mode != 0){
		return (FALSE);
	}

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
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Artifacts have 71% chance to resist */
	if ((artefact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 71))
	{
		/* Message */
		msg_format("Your %s (%c) resist%s disenchantment!",
			o_name, index_to_label(t),
			((o_ptr->number != 1) ? "" : "s"));

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
	msg_format("Your %s (%c) %s disenchanted!",
		o_name, index_to_label(t),
		((o_ptr->number != 1) ? "were" : "was"));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	/* Notice */
	return (TRUE);
}


void corruption_shuffle(void)

{

	int max1, cur1, max2, cur2, ii, jj;
	/* Pick a pair of stats */
	ii = rand_int(6);
	for (jj = ii; jj == ii; jj = rand_int(6)) /* loop */;

	max1 = p_ptr->stat_max[ii];
	cur1 = p_ptr->stat_cur[ii];
	max2 = p_ptr->stat_max[jj];
	cur2 = p_ptr->stat_cur[jj];

	p_ptr->stat_max[ii] = max2;
	p_ptr->stat_cur[ii] = cur2;
	p_ptr->stat_max[jj] = max1;
	p_ptr->stat_cur[jj] = cur1;

	p_ptr->update |= (PU_BONUS);
}


/*
* Apply Nexus
*/
static void apply_nexus(monster_type *m_ptr)
{
	switch (randint(7)){

	case 1: case 2: case 3: teleport_player(200); break;
	case 4: case 5:	teleport_player_to(m_ptr->fy, m_ptr->fx); break;
	case 6:
		if (rand_int(100) < p_ptr->skill_sav)
			msg_print("You resist the effects!");
		else
			teleport_player_level();
		break;
	case 7:
		if (rand_int(100) < p_ptr->skill_sav){
			msg_print("You resist the effects!");
		} else {
			msg_print("Your body starts to scramble...");
			corruption_shuffle();
		}
		break;
	}
}

/*
* Mega-Hack -- track "affected" monsters (see "project()" comments)
*/
static int project_m_n;
static int project_m_x;
static int project_m_y;


/*
* We are called from "project()" to "damage" terrain features
*
* We are called both for "beam" effects and "ball" effects.
*
* The "r" parameter is the "distance from ground zero".
*
* Note that we determine if the player can "see" anything that happens
* by taking into account: blindness, line-of-sight, and illumination.
*
* We return "TRUE" if the effect of the projection is "obvious".
*
* XXX XXX XXX We also "see" grids which are "memorized", probably a hack
*
* XXX XXX XXX Perhaps we should affect doors?
*/
static bool project_f(int who, int r, int y, int x, int dam, int typ)
{
	cave_type       *c_ptr = &cave[y][x];

	bool    obvious = FALSE;

	/* XXX XXX XXX */
	who = who ? who : 0;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* Analyze the type */
	switch (typ)
	{
		/* Ignore most effects */
	case GF_ACID:
	case GF_ELEC:
	case GF_FIRE:
	case GF_COLD:
	case GF_PLASMA:
	case GF_METEOR:
	case GF_ICE:
	case GF_SHARDS:
	case GF_FORCE:
	case GF_SOUND:
	case GF_MANA:
	case GF_HOLY_FIRE:
	case GF_HELL_FIRE:
	case GF_DISINTEGRATE:
	case GF_PSI:
	case GF_PSI_DRAIN:
	case GF_TELEKINESIS:
	case GF_DOMINATION:
	case GF_MAGEBOLT:
		{
			break;
		}

		/* Destroy Traps (and Locks) */
	case GF_KILL_TRAP:
		{
			/* Destroy traps */
			if ((c_ptr->feat == FEAT_INVIS) ||
				((c_ptr->feat >= FEAT_TRAP_HEAD) &&
				(c_ptr->feat <= FEAT_TRAP_TAIL)))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}

				/* Forget the trap */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the trap */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Secret / Locked doors are found and unlocked */
			else if ((c_ptr->feat == FEAT_SECRET) ||
				((c_ptr->feat >= FEAT_DOOR_HEAD + 0x01) &&
				(c_ptr->feat <= FEAT_DOOR_HEAD + 0x07)))
			{
				/* Unlock the door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

				/* Check line of sound */
				if (player_has_los_bold(y, x))
				{
					msg_print("Click!");
					obvious = TRUE;
				}
			}

			break;
		}

		/* Destroy Doors (and traps) */
	case GF_KILL_DOOR:
		{
			/* Destroy all doors and traps */
			if ((c_ptr->feat == FEAT_OPEN) ||
				(c_ptr->feat == FEAT_BROKEN) ||
				(c_ptr->feat == FEAT_INVIS) ||
				((c_ptr->feat >= FEAT_TRAP_HEAD) &&
				(c_ptr->feat <= FEAT_TRAP_TAIL)) ||
				((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_TAIL)))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

					/* Visibility change */
					if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
						(c_ptr->feat <= FEAT_DOOR_TAIL))
					{
						/* Update some things */
						p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
					}
				}

				/* Forget the door */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			break;
		}

	case GF_JAM_DOOR: /* Jams a door (as if with a spike) */
		{
			if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_TAIL))
			{
				/* Convert "locked" to "stuck" XXX XXX XXX */
				if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

				/* Add one spike to the door */
				if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("The door seems stuck.");
					obvious = TRUE;
				}

			}
			break;
		}

		/* Destroy walls (and doors) */
	case GF_KILL_WALL:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x)) break;

			/* Permanent walls */
			if (c_ptr->feat >= FEAT_PERM_BUILDING) break;

			/* Granite */
			if (c_ptr->feat >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (c_ptr->info & (CAVE_MARK))
				{
					msg_print("The wall turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma with treasure */
			else if (c_ptr->feat >= FEAT_MAGMA_H)
			{
				/* Message */
				if (c_ptr->info & (CAVE_MARK))
				{
					msg_print("The vein turns into mud!");
					msg_print("You have found something!");
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Place some gold */
				place_gold(y, x);
			}

			/* Quartz / Magma */
			else if (c_ptr->feat >= FEAT_MAGMA)
			{
				/* Message */
				if (c_ptr->info & (CAVE_MARK))
				{
					msg_print("The vein turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Rubble */
			else if (c_ptr->feat == FEAT_RUBBLE)
			{
				/* Message */
				if (c_ptr->info & (CAVE_MARK))
				{
					msg_print("The rubble turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Hack -- place an object */
				if (rand_int(100) < 10)
				{
					/* Found something */
					if (player_can_see_bold(y, x))
					{
						msg_print("There was something buried in the rubble!");
						obvious = TRUE;
					}

					/* Place gold */
					place_object(y, x, FALSE, FALSE);
				}
			}

			/* Destroy doors (and secret doors) */
			else /* if (c_ptr->feat >= FEAT_DOOR_HEAD) */
			{
				/* Hack -- special message */
				if (c_ptr->info & (CAVE_MARK))
				{
					msg_print("The door turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

			break;
		}
	case GF_BLASTED_TOWER:
	{
		/* Non-walls (etc) */
		if (cave_floor_bold(y, x)) break;

		/* Permanent walls */
		if (c_ptr->feat >= FEAT_PERM_BUILDING) break;

		/* Only give a message when we see the thing happening*/
		if (c_ptr->info & (CAVE_MARK))
		{
			obvious = TRUE;
			/* Granite */
			if (c_ptr->feat >= FEAT_WALL_EXTRA)
				msg_print("The wall turns into rubble!");
			/* Quartz / Magma ( with treasure ) */
			else if (c_ptr->feat >= FEAT_MAGMA_H || c_ptr->feat >= FEAT_MAGMA)
				msg_print("The vein turns into rubble!");
			/* Destroy doors (and secret doors) */
			else /* if (c_ptr->feat >= FEAT_DOOR_HEAD) */
				msg_print("The door turns into rubble!");
		}
		/* Gold drops whether we see it or not*/
		if (c_ptr->feat >= FEAT_MAGMA_H)
			place_gold(y, x);
		/* Forget the wall */
		c_ptr->info &= ~(CAVE_MARK);
		/* Destroy and set rubble */
		cave_set_feat(y, x, FEAT_RUBBLE);
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

		break;
	}

		/* Make doors */
	case GF_MAKE_DOOR:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Create a closed door */
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (c_ptr->info & (CAVE_MARK)) obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			break;
		}

		/* Make traps */
	case GF_MAKE_TRAP:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Place a trap */
			place_trap(y, x);

			break;
		}

	case GF_MAKE_GLYPH:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			cave_set_feat(y, x, FEAT_GLYPH);

			break;
		}

	case GF_STONE_WALL:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Place a trap */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);

			break;
		}

		/* Lite up the grid */
	case GF_LITE_WEAK:
	case GF_LITE:
    case GF_HECATE:
		{
			/* Turn on the light */
			c_ptr->info |= (CAVE_GLOW);

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);

			/* Observe */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

			break;
		}

		/* Darken the grid */
	case GF_DARK_WEAK:
	case GF_DARK:
		{
			/* Notice */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			/* Turn off the light. */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if (c_ptr->feat <= FEAT_INVIS)
			{
				/* Forget */
				c_ptr->info &= ~(CAVE_MARK);

				/* Notice */
				note_spot(y, x);
			}

			/* Redraw */
			lite_spot(y, x);

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

			/* All done */
			break;
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}



/*
* We are called from "project()" to "damage" objects
*
* We are called both for "beam" effects and "ball" effects.
*
* Perhaps we should only SOMETIMES damage things on the ground.
*
* The "r" parameter is the "distance from ground zero".
*
* Note that we determine if the player can "see" anything that happens
* by taking into account: blindness, line-of-sight, and illumination.
*
* XXX XXX XXX We also "see" grids which are "memorized", probably a hack
*
* We return "TRUE" if the effect of the projection is "obvious".
*/
static bool project_o(int who, int r, int y, int x, int dam, int typ)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	u32b f1, f2, f3;

	char o_name[80];

	/*int o_sval = 0; UNUSED*/
	bool is_potion = FALSE;


	/* XXX XXX XXX */
	who = who ? who : 0;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;

		cptr note_kill = NULL;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artefact */
		if ((artefact_p(o_ptr) || o_ptr->art_name)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
		case GF_ACID:
			{
				if (hates_acid(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (f3 & (TR3_IGNORE_ACID)) ignore = TRUE;
				}
				break;
			}

			/* Elec -- Rings and Wands */
		case GF_ELEC:
			{
				if (hates_elec(o_ptr))
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
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
		case GF_COLD:
			{
				if (hates_cold(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Elec */
		case GF_PLASMA:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				if (hates_elec(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Cold */
		case GF_METEOR:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
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
		case GF_SHARDS:
		case GF_FORCE:
		case GF_SOUND:
			{
				if (hates_cold(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}

			/* Mana and Primal Chaos -- destroy everything */
		case GF_MANA:
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
			}
			break;

		case GF_DISINTEGRATE:
			{
				do_kill = TRUE;
				note_kill = (plural ? " evaporate!" : " evaporates!");
			}

			break;

		case GF_CHAOS:
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				if (f2 & (TR2_RES_CHAOS)) ignore = TRUE;
			}
			break;
		case GF_EXP:
			{
				if( (f3 & (TR3_XP)) && o_ptr->elevel < 50 )
				{
					o_ptr->elevel++;
					o_ptr->exp = player_exp[o_ptr->elevel-1];
					object_desc( o_name , o_ptr , FALSE , 0 );
					msg_format( "The %s grows stronger!" , o_name );
				}
			}
			/* Holy Fire and Hell Fire -- destroys cursed non-artefacts */
		case GF_HOLY_FIRE:
		case GF_HELL_FIRE:
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
						object_known(o_ptr,TRUE);

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
		}

		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
				object_desc(o_name, o_ptr, FALSE, 0);
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
					msg_format("The %s%s", o_name, note_kill);
				}

				/*o_sval = o_ptr->sval; UNUSED*/
				is_potion = (k_info[o_ptr->k_idx].tval == TV_POTION);

				/* Potions produce effects when 'shattered' */
				if (is_potion) {
					(void)potion_smash_effect(who, y, x, o_ptr, NULL);
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


int project_m_helper( bool seen , u32b monsterflags , u32b *recallflags , u32b specific_monsterflag , int *dam , int potential_dam , cptr *note , cptr potential_note)
{
	if (monsterflags & (specific_monsterflag))
	{
		*dam  = potential_dam;
		*note = potential_note;
		if (seen) *recallflags |= (specific_monsterflag);
		return TRUE;
	}
	return FALSE;
}

/*
* Helper function for "project()" below.
*
* Handle a beam/bolt/ball causing damage to a monster.
*
* This routine takes a "source monster" (by index) which is mostly used to
* determine if the player is causing the damage, and a "radius" (see below),
* which is used to decrease the power of explosions with distance, and a
* location, via integers which are modified by certain types of attacks
* (polymorph and teleport being the obvious ones), a default damage, which
* is modified as needed based on various properties, and finally a "damage
* type" (see below).
*
* Note that this routine can handle "no damage" attacks (like teleport) by
* taking a "zero" damage, and can even take "parameters" to attacks (like
* confuse) by accepting a "damage", using it to calculate the effect, and
* then setting the damage to zero.  Note that the "damage" parameter is
* divided by the radius, so monsters not at the "epicenter" will not take
* as much damage (or whatever)...
*
* Note that "polymorph" is dangerous, since a failure in "place_monster()"'
* may result in a dereference of an invalid pointer.  XXX XXX XXX
*
* Various messages are produced, and damage is applied.
*
* Just "casting" a substance (i.e. plasma) does not make you immune, you must
* actually be "made" of that substance, or "breathe" big balls of it.
*
* We assume that "Plasma" monsters, and "Plasma" breathers, are immune
* to plasma.
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
static bool project_m(int who, int r, int y, int x, int dam, int typ)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	monster_type *m_ptr = &m_list[c_ptr->m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char killer [80];

	/* Is the monster "seen"? */
	bool seen = m_ptr->ml;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;


	/* Polymorph setting (true or false) */
	int do_poly = 0;

	/* Teleport setting (max distance) */
	int do_dist = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;


	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	cptr note = NULL;

	/* Assume a default death */
	cptr note_dies = " dies.";

	/* Nobody here */
	if (!c_ptr->m_idx) return (FALSE);

	/* Never affect projector */
	if (who && (c_ptr->m_idx == who)) return (FALSE);

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);

	/* Some monsters get "destroyed" */
	if ((r_ptr->flags3 & (RF3_DEMON)) ||
		(r_ptr->flags3 & (RF3_UNDEAD)) ||
		(r_ptr->flags3 & (RF3_DEVIL)) ||
		(r_ptr->flags2 & (RF2_STUPID)) ||
		(r_ptr->flags3 & (RF3_NONLIVING)) ||
		(strchr("Evg", r_ptr->d_char)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}

	if ((!who) && (is_potential_hater(m_ptr)) ) {
		bool get_angry = FALSE;
		/* Grrr? */
		switch (typ) {
		case GF_AWAY_UNDEAD:
		case GF_AWAY_EVIL:
		case GF_AWAY_ALL:
		case GF_CHARM:
		case GF_CONTROL_UNDEAD:
		case GF_CONTROL_ANIMAL:
		case GF_OLD_HEAL:
		case GF_OLD_SPEED:
		case GF_DARK_WEAK:
		case GF_JAM_DOOR:
			break;             /* none of the above anger */
		case GF_KILL_WALL:
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
				get_angry = TRUE;
			break;
		case GF_HOLY_FIRE:
			if (!(r_ptr->flags3 & (RF3_GOOD)))
				get_angry = TRUE;
			break;
		case GF_TURN_UNDEAD:
		case GF_DISP_UNDEAD:
			if (r_ptr->flags3 & RF3_UNDEAD)
				get_angry = TRUE;
			break;
		case GF_TURN_EVIL:
		case GF_DISP_EVIL:
			if (r_ptr->flags3 & RF3_EVIL)
				get_angry = TRUE;
			break;
		case GF_DISP_GOOD:
			if (r_ptr->flags3 & RF3_GOOD)
				get_angry = TRUE;
			break;
		case GF_DISP_DEMON:
			if (r_ptr->flags3 & RF3_DEMON)
				get_angry = TRUE;
			break;
		case GF_DISP_FALLEN_ANGEL:
			if (r_ptr->flags3 & RF3_FALLEN_ANGEL)
				get_angry = TRUE;
			break;
		case GF_DISP_DEVIL:
			if (r_ptr->flags3 & RF3_DEVIL)
				get_angry = TRUE;
			break;
		case GF_DISP_LIVING:
			if (!(r_ptr->flags3 & (RF3_UNDEAD)) &&
				!(r_ptr->flags3 & (RF3_NONLIVING)))
				get_angry = TRUE;
			break;
		case GF_PSI: case GF_PSI_DRAIN:
			if (!(r_ptr->flags2 & (RF2_EMPTY_MIND)))
				get_angry = TRUE;
			break;
		case GF_DOMINATION:
			{
				if (!(r_ptr->flags3 & (RF3_NO_CONF)))
					get_angry = TRUE;
			}
			break;
		case GF_OLD_POLY:
		case GF_OLD_CLONE:
			if (randint(8) == 1)
				get_angry = TRUE;
			break;
		case GF_LITE:
		case GF_LITE_WEAK:
			if (r_ptr->flags3 & RF3_HURT_LITE)
				get_angry = TRUE;
			break;
		default:
			get_angry = TRUE;
		}

		/* Now anger it if appropriate */
		if (get_angry == TRUE && !(who)) {
			msg_format("%^s gets angry!", m_name);
			set_hate_player( m_ptr );
		}
	}

	/* I cannot believe how many times this statement was in the code, have they lost their interest after a while ? */
	if (seen) obvious = TRUE;

	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile -- pure damage */
	case GF_MISSILE:
		{
			break;
		}

		/* Acid */
	case GF_ACID:
		{
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_ACID	 , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_ACID	 , &dam , dam /9 , &note , " resists a lot.");
			break;
		}

		/* Electricity */
	case GF_ELEC:
		{
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_ELEC  , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_ELEC	 , &dam , dam /9 , &note , " resists a lot.");
			/*project_m_helper( r_ptr->flags3 , &r_ptr->r_flags3 , RF3_HURT_ELEC , &dam , dam *3 , &note , " shakes violently.");*/
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_ELEC , &dam , -dam   , &note , " heals up.");
			break;
		}

		/* Fire damage */
	case GF_FIRE:
		{
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_FIRE  , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_FIRE   , &dam , dam /9 , &note , " resists a lot.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_HURT_FIRE , &dam , dam *3 , &note , " bursts in flames.");
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_FIRE , &dam , -dam   , &note , " heals up.");
			break;
		}

		/* Cold */
	case GF_COLD:
		{
			project_m_helper( seen , r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_COLD  , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen , r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_COLD  , &dam , dam /9 , &note , " resists a lot.");
			project_m_helper( seen , r_ptr->flags3 , &r_ptr->r_flags3 , RF3_HURT_COLD , &dam , dam *3 , &note , " freezes up.");
			project_m_helper( seen , r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_COLD , &dam , -dam   , &note , " heals up.");
			break;
		}

		/* Poison */
	case GF_POIS:
		{
			project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_POIS  , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_POIS   , &dam , dam /9 , &note , " resists a lot.");
			break;
		}

		/* Nuclear waste */
	case GF_HELLSLIME:
		{

			if(!project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_POIS   , &dam , dam * 3 / (randint(6)+6) , &note , " resists.") )
				if (randint(3)==1) do_poly = TRUE;
 			break;
		}

		/* Hellfire, hurts evil and good except demons, devils and dragons */
	case GF_HELL_FIRE:
		{
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_EVIL    , &dam , dam * 2 , &note , " is hit hard.");
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_GOOD    , &dam , dam * 2 , &note , " is hit hard.");
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_DEMON   , &dam , 0 , &note , " is immune.");
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_DEVIL   , &dam , 0 , &note , " is immune.");
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_DRAGON  , &dam , 0 , &note , " is immune.");
			break;
		}
		/* Holy Fire -- hurts Evil, Good are immune, others _resist_ */
	case GF_HOLY_FIRE:
		{
			if( !project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_EVIL    , &dam , dam * 2 , &note , " is hit hard.") &&
				!project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_GOOD    , &dam , 0 , &note , " is immune.") )
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			break;
		}
		/* Arrow -- XXX no defense */
	case GF_ARROW:
		{
			break;
		}
		/* Plasma --  */
	case GF_PLASMA:
		{
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_FIRE  , &dam , dam / 10 * 7 , &note , " resists a little.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_ELEC  , &dam , dam / 10 * 7 , &note , " resists a little.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_PLAS , &dam , dam * 3 / (randint(6)+6) , &note , " resists.");
			break;
		}
		/* Nether -- Evil and nether resisters resists a little , undead are immune */
	case GF_NETHER:
		{
			if(project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_NETH , &dam , -dam , &note , " heals up!"))
				break;
			if(!project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_EVIL     , &dam , dam / 2 , &note , " resists a little.") )
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_NETH , &dam , dam * 3 / (randint(6)+6) , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_UNDEAD   , &dam , 0 , &note , " is immune.");
			break;
		}

		/* Water (acid) damage -- Water spirits/elementals are immune */
	case GF_WATER:
		{

			if ( (strstr((r_name + r_ptr->name),"Lasha, Mistress of Water")) ||
				 (strstr((r_name + r_ptr->name),"Rahab, Dragon of the Waters")) ||
				 (strstr((r_name + r_ptr->name),"Vinea")) ||
				 (strstr((r_name + r_ptr->name),"Leviathan, Admiral of the Devil's Navy")) ||
				 (strstr((r_name + r_ptr->name),"Behemoth, the Cupbearer"))
				 )
			{
				note = " is immune.";
				dam = 0;
			}
			else
				project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_WATE   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}

		/* Primal Chaos -- Chaos breathers resist 1 time in 3*/
	case GF_CHAOS:
		{
			do_poly = TRUE;
			do_conf = (5 + randint(11) + r) / (r + 1);
			if( (randint(3)==1) )
			{
				if( project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_DEMON	   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.") ||
				    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_CHAO   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.") )
					do_poly = FALSE;
			}
			break;
		}

		/* Shards -- Shard breathers resist */
	case GF_SHARDS:
		{
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_SHAR   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.")	;
			break;
		}
		/* Shard ball */
	case GF_SHARD:
		{
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_SHAR   , &dam , dam / 2 , &note , " resists somewhat.");
			break;
		}
		/* Sound -- Sound breathers resist */
	case GF_SOUND:
		{
			do_stun = (10 + randint(15) + r) / (r + 1);
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_SOUN   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}
		/* Confusion */
	case GF_CONFUSION:
		{
			do_conf = (10 + randint(15) + r) / (r + 1);
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_CONF   , &dam , dam * 2 / (randint(6) + 6) , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_NO_CONF   , &dam , dam / 2 , &note , " resists somewhat.");
			break;
		}
		/* Disenchantment -- Breathers and Disenchanters resist */
	case GF_DISENCHANT:
		{
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_DISE   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_DISE  , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}
		/* Nexus -- Breathers and Existers resist */
	case GF_NEXUS:
		{
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_NEXU   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_NEXU  , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}
		/* Force */
	case GF_FORCE:
		{
			do_stun = (randint(15) + r) / (r + 1);
		    project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_WALL   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}

		/* Inertia -- breathers resist */
	case GF_INERTIA:
		{
			if ( !project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_INER   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists."))
			{
				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (m_ptr->mspeed > 60)
					{
						m_ptr->mspeed -= 10;
						note = " starts moving slower.";
					}
					else
					{
						note = " is already slow.";
					}
				}
			}
			break;
		}
		/* Time -- breathers resist */
	case GF_TIME:
		{
			project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_TIME   , &dam , dam * 3 / (randint(6) + 6) , &note , " resists.");
			break;
		}

		/* Gravity -- breathers resist damage, teleport resisters do not resist damage, but resists teleportation */
	case GF_GRAVITY:
		{
			bool resist_tele = FALSE;
			if(	project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_RES_TELE  , &dam , dam  , &note , " resists.") )
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					note = " is unaffected!";
					resist_tele = TRUE;
				}
				if (r_ptr->level > randint(100))
				{
					note = " resists!";
					resist_tele = TRUE;
				}
				else
				{
					seen = FALSE;
				}
			}

			if (!resist_tele) do_dist = 10;
			else do_dist = 0;

			/* If we resisted, dont do this rigamarole */
			if(	do_dist > 0 && project_m_helper( seen, r_ptr->flags4 , &r_ptr->r_flags4 , RF4_BR_GRAV  , &dam , dam * 3 / (randint(6)+6)  , &note , " resists.") )
			{
				do_dist = 0;
			}
			else
			{
				/*Stun*/
				do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;
				/* 1. stun & slowness, powerful monsters can resist */
				if ( (r_ptr->flags1 & (RF1_UNIQUE)) || dam < 11 || r_ptr->level > randint(dam - 10) )
				{
					obvious = FALSE;	/* No obvious effect */
					do_stun = 0;		/* Resist */
					note = " is unaffected!";
				}
				/* Normal monsters slow down */
				else
				{
					if (m_ptr->mspeed > 60)
					{
						m_ptr->mspeed -= 10;
						note = " starts moving slower.";
					}
					else
					{
						note = " is already slow.";
					}
				}
			}
			break;
		}
		/* Pure damage */
	case GF_MANA:
		{
			break;
		}
		/* Pure damage */
	case GF_DISINTEGRATE:
		{
			if(	project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_HURT_ROCK  , &dam , dam * 2  , &note , " disintegrates!.") )
			{
				note_dies = " evaporates!";
			}
			else
			{
				if ( (r_ptr->flags1 & RF1_UNIQUE) && rand_int(r_ptr->level + 10) > rand_int(p_ptr->lev))
				{
						note = " resists.";
						dam >>= 3;
				}
			}
			break;
		}
		/*  psi attack as in mind blast, potentially backfiring, potentially confusing/stunning/paralyzing */
	case GF_PSI:
		{
			if(	!project_m_helper( seen, r_ptr->flags2 , &r_ptr->r_flags2 , RF2_EMPTY_MIND  , &dam , 0 , &note , " is immune!.") )
			{
				if ((r_ptr->flags2 & RF2_STUPID) ||	(r_ptr->flags2 & RF2_WEIRD_MIND) ||	(r_ptr->flags3 & RF3_ANIMAL) || (r_ptr->level > randint(3 * dam)))
				{
					dam /= 3;
					note = " resists.";
				}
				/* Powerful demons & undead can turn a orphic's attacks back on them */
				if (((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_DEMON)) && (r_ptr->level > p_ptr->lev/2) && (randint(2) == 1))
				{
					note = NULL;
					msg_format("%^s%s corrupted mind backlashes your attack!", m_name, (seen ? "'s" : "s"));
					/* Saving throw */
					if (rand_int(100) < p_ptr->skill_sav) {
						msg_print("You resist the effects!");
					} else {
						/* Injure +/- confusion */
						monster_desc(killer, m_ptr, 0x88);
						take_hit(dam, killer);  /* has already been /3 */
						if (randint(4) == 1)
						{
							switch (randint(4)) {
								case 1:
									set_timed_effect( TIMED_CONFUSED , p_ptr->confused + 3 + randint(dam));
									break;
								case 2:
									set_timed_effect( TIMED_STUN, p_ptr->stun + randint(dam)); break;
								case 3:
									{
									if (r_ptr->flags3 & (RF3_NO_FEAR))
										note = " is unaffected.";
									else
										set_timed_effect( TIMED_AFRAID , p_ptr->afraid + 3 + randint(dam));
									}
								break;
								default:
									if (!p_ptr->free_act)
									(void)set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(dam));
									break;
							}/* End switch for player effects of backlash*/
						}/* End of potential backlash effects */
						dam = 0;
					}/* End of backlash*/
				}/*End of powerful undead or demon*/
				/* Work out potential damage from a psi attack*/
				if ((dam > 0) && (randint(4) == 1))
				{
					switch (randint(4)) {
						case 1:
							do_conf  = 3 + randint(dam);  break;
						case 2:
							do_stun  = 3 + randint(dam); break;
						case 3:
							do_fear  = 3 + randint(dam);  break;
						default:
							do_sleep = 3 + randint(dam);  break;
					}
				}
				note_dies = " collapses, a mindless husk.";
			}/*End of mega-if*/
			break;
		}
	case GF_PSI_DRAIN:
		{
			if(	!project_m_helper( seen, r_ptr->flags2 , &r_ptr->r_flags2 , RF2_EMPTY_MIND  , &dam , 0 , &note , " is immune!.") )
			{
				if ((r_ptr->flags2 & RF2_STUPID) ||	(r_ptr->flags2 & RF2_WEIRD_MIND) ||	(r_ptr->flags3 & RF3_ANIMAL) || (r_ptr->level > randint(3 * dam)))
				{
					dam /= 3;
					note = " resists.";
				}
				/* Powerful demons & undead can turn a orphic's attacks back on them */
				if ( ( (r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_DEMON) ) &&	(r_ptr->level > p_ptr->lev/2) && (randint(2) == 1) )
				{
					note = NULL;
					msg_format("%^s%s corrupted mind backlashes your attack!", m_name, (seen ? "'s" : "s") );
					/* Saving throw */
					if (rand_int(100) < p_ptr->skill_sav) {
						msg_print("You resist the effects!");
					} else {
						/* Injure + mana drain */
						monster_desc(killer, m_ptr, 0x88);
						msg_print("Your psychic energy is drained!");
						p_ptr->csp = MAX(0, p_ptr->csp - damroll(5, dam)/2);
						p_ptr->redraw |= PR_MANA;
						take_hit(dam, killer);  /* has already been /3 */
					}
					dam = 0;
				}
				if (dam > 0) {
					int b = damroll(5, dam) / 4;
					msg_format("You convert %s%s pain into psychic energy!", m_name, (seen ? "'s" : "s"));
					b = MIN(p_ptr->msp, p_ptr->csp + b);
					p_ptr->csp = b;
					p_ptr->redraw |= PR_MANA;
				}
				note_dies = " collapses, a mindless husk.";
			}
			break;
		}
	case GF_TELEKINESIS:
		{
			do_dist = 7;
			/* 1. stun */
			do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;
			/* Attempt a saving throw */
			if ( (r_ptr->flags1 & (RF1_UNIQUE)) ||	(r_ptr->level > 5 + randint(dam)) )
			{
				/* Resist */
				do_stun = 0;
				/* No obvious effect */
				obvious = FALSE;
			}
			break;
		}
		/* Meteor -- powerful magic missile */
	case GF_METEOR:
		{
			break;
		}
	case GF_DOMINATION:
		{
			if (is_ally( m_ptr ))
			{
				obvious = FALSE;
				break;
			}
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags3 & (RF3_NO_CONF)) || dam < 11 || (r_ptr->level > randint(dam - 10)) )
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF) && seen ) r_ptr->r_flags3 |= (RF3_NO_CONF);
				/* Resist */
				do_conf = 0;
				/* Powerful demons & undead can turn a orphic's
				* attacks back on them */
				if (((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_DEMON)) && (r_ptr->level > p_ptr->lev/2) && (randint(2) == 1) )
				{
					note = NULL;
					msg_format("%^s%s corrupted mind backlashes your attack!", m_name, (seen ? "'s" : "s"));
					/* Saving throw */
					if (rand_int(100) < p_ptr->skill_sav) {
						msg_print("You resist the effects!");
					}
					else
					{
						/* Confuse, stun, terrify */
						switch (randint(4)) {
							case 1:
								set_timed_effect( TIMED_STUN, p_ptr->stun + dam / 2);
								break;
							case 2:
								set_timed_effect( TIMED_CONFUSED , p_ptr->confused + dam / 2);
								break;
							default:
								{
									if (r_ptr->flags3 & (RF3_NO_FEAR))
										note = " is unaffected.";
									else
										set_timed_effect( TIMED_AFRAID , p_ptr->afraid + dam);
								}
						}/*End of switch*/
					}/*End of backlash*/
				}
				else
				{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
				}
			}
			else
			{
				if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD) )
				{
					note = " hates you too much!";
				}
				else
				{
					if ((dam > 29) && (randint(100) < dam)) {
						note = " is in your thrall!";
						/*Set monster charmed, but not super charmed*/
						set_ally( m_ptr, ALLY_PLAYER);
					}
					else
					{
						switch (randint(4)) {
							case 1:
								do_stun = dam/2;
								break;
							case 2:
								do_conf = dam/2;
								break;
							default:
								do_fear = dam;
						}/*End switch*/
					}/*Successful attack*/
				}/*Guardian or not*/
			}/* Not unique, not confusion resist or really powerfull */
			/* No "real" damage */
			break;
		}
		/* Ice -- Cold + Cuts + Stun */
	case GF_ICE:
		{
			do_stun = (randint(15) + 1) / (r + 1);
			project_m_helper( seen , r_ptr->flags7  , &r_ptr->r_flags7  , RF7_RES_COLD  , &dam , dam /2 , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_IM_COLD	, &dam , dam /9 , &note , " resists a lot.");
			project_m_helper( seen , r_ptr->flags3  , &r_ptr->r_flags3  , RF3_HURT_COLD , &dam , dam *3 , &note , " freezes up.");
			project_m_helper( seen , r_ptr->flags7  , &r_ptr->r_flags7  , RF7_HEAL_COLD , &dam , -dam   , &note , " heals up.");
			break;
		}
		/* Drain Life */
	case GF_OLD_DRAIN:
		{
			if(
				project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_UNDEAD	, &dam , 0 , &note , " is unaffected.") ||
				project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_DEMON		, &dam , 0 , &note , " is unaffected.") ||
				project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_NONLIVING	, &dam , 0 , &note , " is unaffected.") ||
				(strchr("Egv", r_ptr->d_char)))
			{
				obvious = FALSE;
				note = " is unaffected!";  /* Silly 4th if clause ;{ */
			}
			break;
		}
		/* Death Ray */
	case GF_DEATH_RAY:
		{
			if(
				project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_UNDEAD	, &dam , 0 , &note , " is unaffected.") ||
				project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_NONLIVING	, &dam , 0 , &note , " is unaffected.") )
				obvious = FALSE;
			/* if the monster is unique AND on a 1/888 chance OR the level plus d20 is > the damage + d10 AND you have a 1 in 66 chance , thanks Elly*/
			else if ( ( (r_ptr->flags1 & (RF1_UNIQUE)) && (randint(888) != 666)) || ( ( (r_ptr->level + randint(20) ) > randint( (dam)+randint(10) ) ) && randint(100) != 66 ) )
			{
				note = " resists!";
				obvious = FALSE;
				dam = 0;
			}
			else dam = (p_ptr->lev) * 200;
			break;
		}
		/* Polymorph monster (Use "dam" as "power") */
	case GF_OLD_POLY:
		{
			/* Attempt to polymorph (see below) */
			do_poly = TRUE;
			/* Powerful monsters can resist */
			if ( (r_ptr->flags1 & (RF1_UNIQUE)) || dam < 11 || r_ptr->level > randint(dam - 10) )
			{
				note = " is unaffected!";
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
			bool is_friend = FALSE;
			/*Cloned friends are friendly 2 in 3 or 1 in 4*/
			if (is_ally(m_ptr) && (randint(3)!=1))
				is_friend = TRUE;
			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;
			/* Speed up */
			if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;
			/* Attempt to clone. */
			if (multiply_monster(c_ptr->m_idx, is_friend, TRUE))
			{
				note = " spawns!";
			}
			/* No "real" damage */
			dam = 0;
			break;
		}
		/* Heal Monster (use "dam" as amount of healing) */
	case GF_OLD_HEAL:
		{
			dam = -dam;
			/* Wake up */
			/*m_ptr->csleep = 0;*/
			/* Heal */
			/*m_ptr->hp += dam;*/
			/* No overflow */
			/*if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;*/
			/* Redraw (later) if needed */
			/*if (health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);*/

			/* Message */
			note = " looks healthier.";
			break;
		}
		/* Speed Monster (Ignore "dam") */
	case GF_OLD_SPEED:
		{
			/* Speed up */
			if (m_ptr->mspeed < 150) {
				m_ptr->mspeed += 10;
				note = " starts moving faster.";
			}else{
				note = " is unaffected";
				obvious = FALSE;
			}
			/* No "real" damage */
			dam = 0;
			break;
		}
		/* Slow Monster (Use "dam" as "power") */
	case GF_OLD_SLOW:
		{
			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || dam < 11 || (r_ptr->level > randint(dam - 10)) )
			{
				note = " is unaffected!";
				obvious = FALSE;
			}
			/* Normal monsters slow down */
			else
			{
				if (m_ptr->mspeed > 60)
				{
					m_ptr->mspeed -= 10;
					note = " starts moving slower.";
				}
				else{
					note = " is unaffected";
					obvious = FALSE;
				}
			}

			/* No "real" damage */
			dam = 0;
			break;
		}
		/* Sleep (Use "dam" as "power") */
	case GF_OLD_SLEEP:
		{
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags3 & (RF3_NO_SLEEP)) || dam < 11 || (r_ptr->level > randint(dam - 10)) )
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP) && seen ) r_ptr->r_flags3 |= (RF3_NO_SLEEP);
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
				note = " falls asleep!";
				do_sleep = 500;
			}
			/* No "real" damage */
			dam = 0;
			break;
		}
		/* Sleep (Use "dam" as "power") */
	case GF_STASIS:
		{
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || dam < 11 || (r_ptr->level > randint(dam - 10)) )
			{
				note = " is unaffected!";
				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
				note = " is suspended!";
				do_sleep = 500;
			}
			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Charm monster */
	case GF_CHARM:
		{
			/*Not all charm attacks work on every one*/
			if (monster_filter_hook && !((*monster_filter_hook)(m_ptr->r_idx)))
			{
				note = " is unaffected!";
				obvious = FALSE;
				dam = 0;
				break;
			}

			dam += (adj_stat[p_ptr->stat_ind[A_CHA]][ADJ_REGEN] - 1);
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_CONF)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 5))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) {
				note = " hates you too much!";
			} else {
				note = " suddenly seems friendly!";
				set_ally( m_ptr, ALLY_PLAYER);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Control undead */
	case GF_CONTROL_UNDEAD:
		{
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(!(r_ptr->flags3 & (RF3_UNDEAD))) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{

				/* Resist */
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) {
				note = " hates you too much!";
			} else {
				note = " is in your thrall!";
				set_ally( m_ptr, ALLY_PLAYER);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Tame animal */
	case GF_CONTROL_ANIMAL:
		{
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(!(r_ptr->flags3 & (RF3_ANIMAL))) ||
				(r_ptr->flags3 & (RF3_NO_CONF)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) {
				note = " hates you too much!";
			} else {
				note = " is tamed!";
				set_ally( m_ptr, ALLY_COMPANION);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Confusion (Use "dam" as "power") */
	case GF_OLD_CONF:
		{
			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_CONF)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				do_conf = 0;

				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

	case GF_STUN:
		{
			do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;
			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Resist */
				do_stun = 0;

				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}
			/* No "real" damage */
			dam = 0;
			break;
		}
		/* Lite, but only hurts susceptible creatures */
	case GF_LITE_WEAK:
		{
			if( project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_HURT_LITE	, &dam , dam , &note , " cringes from the light!.")	)
				note_dies = " shrivels away in the light!";
			else
				dam = 0; /* No damage */
			break;
		}
	case GF_HECATE:
    {
        if( project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_HURT_LITE	, &dam , dam , &note , " cringes from the light!.")	)
            note_dies = " shrivels away in the light!";
        /* Attempt a saving throw and excluding some silly cases*/
        if ( (r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags3 & (RF3_NO_CONF)) || (r_ptr->level > randint(dam)) || is_ally(m_ptr) || (r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD) )
        {
            /* Memorize a flag */
            if (r_ptr->flags3 & (RF3_NO_CONF) && seen ) r_ptr->r_flags3 |= (RF3_NO_CONF);
        }
        else if (randint(100) < dam)
        {
            note = " is in your thrall!";
			set_ally( m_ptr, ALLY_PLAYER);
        }
        if (!(is_ally(m_ptr)))
        {
            if (randint(100) < dam)
			{
                do_stun = dam;
            }
            else if (randint(100) < dam)
			{
                do_conf = dam;
            }
            else if (randint(100) < dam)
			{
                do_fear = dam;
            }
        }
        /* Reset damage to 0 unless we really dont like light*/
        if( !(r_ptr->flags3 & RF3_HURT_LITE) )
            dam = 0;
        break;
    }
		/* Lite -- opposite of Dark */
	case GF_LITE:
		{
			if( project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_HURT_LITE	, &dam , dam * 2  , &note , " cringes from the light!.")	)
				note_dies = " shrivels away in the light!";
			project_m_helper( seen ,  r_ptr->flags4 , &r_ptr->r_flags4  , RF4_BR_LITE	, &dam , dam * 2 / (randint(6)+6) , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_RES_LITE	, &dam , dam / 3 , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_IM_LITE	, &dam , dam / 9 , &note , " resists a lot.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_HEAL_LITE	, &dam , -dam    , &note , " heals up!.");
			break;
		}
		/* Dark -- opposite of Lite */
	case GF_DARK:
		{
			project_m_helper( seen ,  r_ptr->flags4 , &r_ptr->r_flags4  , RF4_BR_DARK	, &dam , dam * 2 / (randint(6)+6) , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_HURT_LITE	, &dam , dam * 2 / (randint(6)+6) , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_TROLL		, &dam , dam * 2 / (randint(6)+6) , &note , " resists.");

			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_RES_DARK	, &dam , dam / 3 , &note , " resists.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_IM_DARK	, &dam , dam / 9 , &note , " resists a lot.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_HURT_DARK	, &dam , dam * 3 , &note , " fades!.");
			project_m_helper( seen ,  r_ptr->flags7 , &r_ptr->r_flags7  , RF7_HEAL_DARK	, &dam , -dam    , &note , " heals up!.");

			break;
		}
		/* Stone to Mud */
	case GF_BLASTED_TOWER:
	{
		project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_ELEC  , &dam , dam /2 , &note , " resists.");
		project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_RES_FIRE  , &dam , dam /2 , &note , " resists.");

		project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_ELEC	 , &dam , dam /9 , &note , " resists a lot.");
		project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_IM_FIRE   , &dam , dam /9 , &note , " resists a lot.");

		project_m_helper( seen, r_ptr->flags3 , &r_ptr->r_flags3 , RF3_HURT_FIRE , &dam , dam *3 , &note , " bursts in flames.");

		project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_ELEC , &dam , -dam   , &note , " heals up.");
		project_m_helper( seen, r_ptr->flags7 , &r_ptr->r_flags7 , RF7_HEAL_FIRE , &dam , -dam   , &note , " heals up.");

		break;
	}
	case GF_KILL_WALL:
		{
			if(project_m_helper( seen ,  r_ptr->flags3 , &r_ptr->r_flags3  , RF3_HURT_ROCK		, &dam , dam  , &note , " crumbles.") )
				note_dies = " dissolves!";
			else{
				/* No damage */
				dam = 0;
				obvious = FALSE;
			}
			break;
		}
		/* Teleport undead (Use "dam" as "power") */
	case GF_AWAY_UNDEAD:
		{

			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & (RF3_RES_TELE))
				{
					if (r_ptr->flags1 & (RF1_UNIQUE))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
						note = " is unaffected!";
						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint(100))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
						note = " resists!";
						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
					do_dist = dam;
				}
				else
				{
					obvious = FALSE;
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


		/* Teleport evil (Use "dam" as "power") */
	case GF_AWAY_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & (RF3_RES_TELE))
				{
					if (r_ptr->flags1 & (RF1_UNIQUE))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
						note = " is unaffected!";
						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint(100))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
						note = " resists!";
						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
					do_dist = dam;
				}
				else
				{
					obvious = FALSE;
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


		/* Teleport monster (Use "dam" as "power") */
	case GF_AWAY_ALL:
		{
			bool resists_tele = FALSE;

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
					note = " is unaffected!";
					resists_tele = TRUE;
				}
				else if (r_ptr->level > randint(100))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
					note = " resists!";
					resists_tele = TRUE;
				}
			}

			if (!resists_tele)
			{
				/* Prepare to teleport */
				do_dist = dam;
			}
			else
			{
				obvious = FALSE;
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
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
				{
					/* No obvious effect */
					note = " is unaffected!";
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
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
				{
					/* No obvious effect */
					note = " is unaffected!";
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
			/* Apply some fear */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_FEAR)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
				do_fear = 0;
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
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " shudders.";
				note_dies = " dissolves!";
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
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " shudders.";
				note_dies = " dissolves!";
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

		/* Dispel good */
	case GF_DISP_GOOD:
		{
			/* Only affect good */
			if (r_ptr->flags3 & (RF3_GOOD))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_GOOD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " shudders.";
				note_dies = " dissolves!";
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



		/* Dispel living */
	case GF_DISP_LIVING:
		{
			/* Only affect non-undead */
			if (!(r_ptr->flags3 & (RF3_UNDEAD)) && !(r_ptr->flags3 & (RF3_NONLIVING)))
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " shudders.";
				note_dies = " dissolves!";
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

		/* Dispel demons */
	case GF_DISP_DEMON:
		{
			/* Only affect demons */
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_DEMON);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " shudders.";
				note_dies = " dissolves!";
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
		/*Dispel fallen angels*/
	case GF_DISP_FALLEN_ANGEL:
	{
		/* Only affect fallen angels */
		if (r_ptr->flags3 & (RF3_FALLEN_ANGEL))
		{
			/* Learn about type */
			if (seen) r_ptr->r_flags3 |= (RF3_FALLEN_ANGEL);

			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";
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
		/*Dispel devils*/
	case GF_DISP_DEVIL:
	{
		/* Only affect devils */
		if (r_ptr->flags3 & (RF3_DEVIL))
		{
			/* Learn about type */
			if (seen) r_ptr->r_flags3 |= (RF3_DEVIL);

			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";
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
		/* Dispel monster */
	case GF_DISP_ALL:
		{
			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";

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


	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE)) do_poly = FALSE;

	/* "Quest" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_GUARDIAN)) do_poly = FALSE;


	/* "Unique" monsters can only be "killed" by the player */
	if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD))
	{
		/* Uniques may only be killed by the player */
		if (who && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}

	/* "Quest" monsters can only be "killed" by the player */
	if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD))
	{
		if ((who > 0) && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}

	/* Check for death */
	if ( dam > m_ptr->hp)
	{
		/* Extract method of death */
		note = note_dies;
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint(90) > r_ptr->level))
	{
		bool charm = FALSE;

		/* Default -- assume no polymorph */
		note = " is unaffected!";

		charm = is_ally(m_ptr);

		/* Pick a "new" monster race */
		tmp = poly_r_idx(m_ptr->r_idx);

		/* Handle polymorh */
		if (tmp != m_ptr->r_idx)
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Monster polymorphs */
			note = " changes!";

			/* Turn off the damage */
			dam = 0;

			/* "Kill" the "old" monster */
			delete_monster_idx(c_ptr->m_idx,TRUE);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE, charm);

			/* XXX XXX XXX Hack -- Assume success */

			/* Hack -- Get new monster */
			m_ptr = &m_list[c_ptr->m_idx];

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];
		}
	}

	/* Handle "teleport" */
	else if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Message */
		note = " disappears!";

		/* Teleport */
		teleport_away(c_ptr->m_idx, do_dist);

		/* Hack -- get new location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Hack -- get new grid */
		c_ptr = &cave[y][x];
	}

	/* Sound and Impact breathers never stun */
	else if (do_stun &&
		!(r_ptr->flags4 & (RF4_BR_SOUN)) &&
		!(r_ptr->flags4 & (RF4_BR_WALL)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Get confused */
		if (m_ptr->stunned)
		{
			note = " is more dazed.";
			tmp = m_ptr->stunned + (do_stun / 2);
		}
		else
		{
			note = " is dazed.";
			tmp = do_stun;
		}

		/* Apply stun */
		m_ptr->stunned = (tmp < 200) ? tmp : 200;
	}

	/* Confusion and Chaos breathers (and sleepers) never confuse */
	else if (do_conf &&
		!(r_ptr->flags3 & (RF3_NO_CONF)) &&
		!(r_ptr->flags4 & (RF4_BR_CONF)) &&
		!(r_ptr->flags4 & (RF4_BR_CHAO)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Already partially confused */
		if (m_ptr->confused)
		{
			note = " looks more confused.";
			tmp = m_ptr->confused + (do_conf / 2);
		}

		/* Was not confused */
		else
		{
			note = " looks confused.";
			tmp = do_conf;
		}

		/* Apply confusion */
		m_ptr->confused = (tmp < 200) ? tmp : 200;
	}


	/* Fear */
	if (do_fear)
	{
		/* Increase fear */
		tmp = m_ptr->monfear + do_fear;

		/* Set fear */
		m_ptr->monfear = (tmp < 200) ? tmp : 200;
	}


	/* If another monster did the damage, hurt the monster by hand */
	if (who)
	{
		/* Wake the monster up */
		m_ptr->csleep = 0;
		/* Hurt the monster */
		m_ptr->hp -= dam;
		/* No overflow */
		if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
		/* Redraw (later) if needed */
		if (health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			bool sad = FALSE;

			if ( (is_ally(m_ptr)) && !(m_ptr->ml))
				sad = TRUE;

			/* Generate treasure, etc */
			monster_death(c_ptr->m_idx);

			/* Delete the monster */
			delete_monster_idx(c_ptr->m_idx,TRUE);

			/* Give detailed messages if destroyed */
			if (note) msg_format("%^s%s", m_name, note);

			if (sad)
			{
				msg_print("You feel sad for a moment.");
			}
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam != 0) message_pain(c_ptr->m_idx, dam);

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;

		if( dam < 0 )
		{
			/* Wake the monster up */
			m_ptr->csleep = 0;
			/* Heal the monster */
			m_ptr->hp -= dam;
			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
			/* Redraw (later) if needed */
			if (health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);
			dam = 0;
		}

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(c_ptr->m_idx, dam, &fear, note_dies))
		{
			/* Dead monster */
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam != 0) message_pain(c_ptr->m_idx, dam);

			/* Take note */
			if ((fear || do_fear) && (m_ptr->ml))
			{
				/* Sound */
				sound(SOUND_FLEE);

				/* Message */
				msg_format("%^s flees in terror!", m_name);
			}

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}


	/* XXX XXX XXX Verify this code */

	/* Update the monster */
	update_mon(c_ptr->m_idx, FALSE);

	/* Redraw the monster grid */
	lite_spot(y, x);


	/* Update monster recall window */
	if (monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}


	/* Track it */
	project_m_n++;
	project_m_x = x;
	project_m_y = y;


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
* NOTE: 'Bolt' attacks can be reflected back, so we need to know
* if this is actually a ball or a bolt spell
*
*
* We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
* we just assume that the effects were obvious, for historical reasons.
*/
static bool project_p(int who, int r, int y, int x, int dam, int typ, int a_rad)
{
	int k = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster */
	monster_type *m_ptr;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;


	/* Player is not here */
	if ((x != px) || (y != py)) return (FALSE);

	/* Player cannot hurt himself */
	if (!who) return (FALSE);


	if (p_ptr->reflect && !a_rad && !(randint(10)==1) && !(typ & GF_MAGEBOLT))
	{
		byte t_y, t_x;
		int max_attempts = 10;


		if (blind)
			msg_print("Something bounces!");
		else
			msg_print("The attack bounces!");

		/* Choose 'new' target */
		do
		{
			t_y = m_list[who].fy - 1 + (byte_hack)randint(3);
			t_x = m_list[who].fx - 1 + (byte_hack)randint(3);
			max_attempts--;
		}

		while (  (max_attempts > 0)
				 &&
				 in_bounds2_unsigned(t_y, t_x)
				 &&
			     !(player_has_los_bold(t_y, t_x))
			  );

		if (max_attempts < 1)
		{

			t_y = m_list[who].fy;
			t_x = m_list[who].fx;
		}

		project(0, 0, t_y, t_x, dam, typ,
			(PROJECT_STOP|PROJECT_KILL));
		disturb(1, 0);
		return TRUE;
	}

	/* XXX XXX XXX */
	/* Limit maximum damage */
	if (dam > 1600) dam = 1600;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;


	/* Get the source monster */
	m_ptr = &m_list[who];

	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster's real name */
	monster_desc(killer, m_ptr, 0x88);


	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
	case GF_ACID:
		{
			if (fuzzy) msg_print("You are hit by acid!");
			acid_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
	case GF_FIRE:
		{
			if (fuzzy) msg_print("You are hit by fire!");
			fire_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
	case GF_COLD:
		{
			if (fuzzy) msg_print("You are hit by cold!");
			cold_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
	case GF_ELEC:
		{
			if (fuzzy) msg_print("You are hit by lightning!");
			elec_dam(dam, killer);
			break;
		}

		/* Standard damage -- also poisons player */
	case GF_POIS:
		{
			if (fuzzy) msg_print("You are hit by poison!");
			if (p_ptr->resist_pois) dam = (dam + 2) / 3;
			if (p_ptr->oppose_pois) dam = (dam + 2) / 3;
			if ((!(p_ptr->oppose_pois || p_ptr->resist_pois))
				&& randint(HURT_CHANCE)==1)
				(void) do_dec_stat(A_CON);
			take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void)set_timed_effect( TIMED_POISONED , p_ptr->poisoned + rand_int(dam) + 10);
			}
			break;
		}

		/* Standard damage -- also poisons / mutates player */
	case GF_HELLSLIME:
		{
			if (fuzzy) msg_print("You are hit by radiation!");
			if (p_ptr->resist_pois) dam = (2 * dam + 2) / 5;
			if (p_ptr->oppose_pois) dam = (2 * dam + 2) / 5;
			take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void)set_timed_effect( TIMED_POISONED , p_ptr->poisoned + rand_int(dam) + 10);
				if (randint(5)==1) /* 6 */
				{ msg_print("You undergo a freakish metamorphosis!");
				if (randint(4)==1) /* 4 */
					do_poly_self();
				else
					corruption_shuffle();
				}
				if (randint(6)==1)
				{
					inven_damage(set_acid_destroy, 2);
				}
			}
			break;
		}

		/* Standard damage */
	case GF_MISSILE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Holy Orb -- Player only takes partial damage */
	case GF_HOLY_FIRE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->realm1 == 1 || p_ptr->realm2 == 1)
				dam /= 2;
			else if (p_ptr->realm1 == 5 || p_ptr->realm2 == 5)
				dam *= 2;
			take_hit(dam, killer);
			break;
		}

	case GF_HELL_FIRE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->realm1 == 5 || p_ptr->realm2 == 5)
				dam /= 2;
			else if (p_ptr->realm1 == 1 || p_ptr->realm2 == 1)
				dam *= 2;
			take_hit(dam, killer);
			break;
		}

		/* Arrow -- XXX no dodging */
	case GF_ARROW:
		{
			if (fuzzy) msg_print("You are hit by something sharp!");
			take_hit(dam, killer);
			break;
		}

		/* Plasma -- XXX No resist */
	case GF_PLASMA:
		{
			if (fuzzy) msg_print("You are hit by something *HOT*!");
			take_hit(dam, killer);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + k);
			}
			if (!(p_ptr->resist_fire || p_ptr->oppose_fire
				|| p_ptr->immune_fire))
			{
				inven_damage(set_acid_destroy, 3);
			}
			break;
		}

		/* Nether -- drain experience */
	case GF_NETHER:
		{
			if (fuzzy) msg_print("You are hit by nether forces!");
			if (p_ptr->resist_neth)
			{
				if (p_ptr->prace != SPECTRE)
				{
					dam *= 6; dam /= (randint(6) + 6);
				}
			}
			else
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
				}
			}
			if (p_ptr->prace == SPECTRE) {
				msg_print("You feel invigorated!");
				hp_player(dam / 4);
			} else {
				take_hit(dam, killer);
			}
			break;
		}

		/* Water -- stun/confuse */
	case GF_WATER:
		{
			if (fuzzy) msg_print("You are hit by something wet!");
			if (!p_ptr->resist_sound)
			{
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(40));
			}
			if (!p_ptr->resist_conf)
			{
				(void)set_timed_effect( TIMED_CONFUSED , p_ptr->confused + randint(5) + 5);
			}

			if (randint(5)==1)
			{
				inven_damage(set_cold_destroy, 3);
			}

			take_hit(dam, killer);
			break;
		}

		/* Primal Chaos -- many effects */
	case GF_CHAOS:
		{
			if (fuzzy) msg_print("You are hit by a wave of anarchy!");
			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)set_timed_effect( TIMED_CONFUSED , p_ptr->confused + rand_int(20) + 10);
			}
			if (!p_ptr->resist_chaos)
			{
				(void)set_timed_effect( TIMED_IMAGE , p_ptr->image + randint(10));
				if (randint(3)==1)
				{
					msg_print("Your body is twisted by chaos!");
					(void) gain_corruption(0);
				}
			}
			if (!p_ptr->resist_neth && !p_ptr->resist_chaos)
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(500 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(5000 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
				}
			}
			if ((!p_ptr->resist_chaos) || (randint(9)==1))
			{
				inven_damage(set_elec_destroy, 2);
				inven_damage(set_fire_destroy, 2);
			}
			take_hit(dam, killer);
			break;
		}

		/* Shards -- mostly cutting */
	case GF_SHARDS:
		{
			if (fuzzy) msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)set_timed_effect( TIMED_CUT, p_ptr->cut + dam);
			}
			if ((!p_ptr->resist_shard) || (randint(13)==1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			take_hit(dam, killer);
			break;
		}

		/* Sound -- mostly stunning */
	case GF_SOUND:
		{
			if (fuzzy) msg_print("You are hit by a loud noise!");
			if (p_ptr->resist_sound)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + k);
			}
			if ((!p_ptr->resist_sound) || (randint(13)==1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			take_hit(dam, killer);
			break;
		}

		/* Pure confusion */
	case GF_CONFUSION:
		{
			if (fuzzy) msg_print("You are hit by something puzzling!");
			if (p_ptr->resist_conf)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)set_timed_effect( TIMED_CONFUSED , p_ptr->confused + randint(20) + 10);
			}
			take_hit(dam, killer);
			break;
		}

		/* Disenchantment -- see above */
	case GF_DISENCHANT:
		{
			if (fuzzy) msg_print("You are hit by something static!");
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
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_nexus)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				apply_nexus(m_ptr);
			}
			take_hit(dam, killer);
			break;
		}

		/* Force -- mostly stun */
	case GF_FORCE:
		{
			if (fuzzy) msg_print("You are hit by kinetic force!");
			if (!p_ptr->resist_sound)
			{
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(20));
			}
			take_hit(dam, killer);
			break;
		}


		/* Shard -- stun, cut */
	case GF_SHARD:
		{
			if (fuzzy) msg_print("You are hit by shards!");
			if (!p_ptr->resist_sound)
			{
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(20));
			}
			if (p_ptr->resist_shard)
			{
				dam /= 2;
			}
			else
			{
				(void)set_timed_effect( TIMED_CUT, p_ptr->  cut + ( dam / 2) );
			}

			if ((!p_ptr->resist_shard) || (randint(12)==1))
			{
				inven_damage(set_cold_destroy, 3);
			}

			take_hit(dam, killer);
			break;
		}

		/* Inertia -- slowness */
	case GF_INERTIA:
		{
			if (fuzzy) msg_print("You are hit by something slow!");
			(void)set_timed_effect( TIMED_SLOW, p_ptr->slow + rand_int(4) + 4);
			take_hit(dam, killer);
			break;
		}

		/* Lite -- blinding */
	case GF_LITE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_lite)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_timed_effect( TIMED_BLIND , p_ptr->blind + randint(5) + 2);
			}
			if (rp_ptr->hates_light) {
				msg_print("The light scorches your flesh!");
				dam *= 2;
			}
			take_hit(dam, killer);
			if (p_ptr->wraith_form)
			{
				p_ptr->wraith_form = 0;
				msg_print("The light forces you out of your incorporeal shadow form.");
				p_ptr->redraw |= PR_MAP;
				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);
				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD);

			}

			break;
		}

		/* Dark -- blinding */
	case GF_DARK:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_dark)
			{
				dam *= 4; dam /= (randint(6) + 6);
				if (rp_ptr->hates_light)
					dam = 0;
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_timed_effect( TIMED_BLIND , p_ptr->blind + randint(5) + 2);
			}
			if (p_ptr->wraith_form)
				hp_player(dam);
			else
				take_hit(dam, killer);
			break;
		}

		/* Time -- bolt fewer effects XXX */
	case GF_TIME:
		{
			if (fuzzy) msg_print("You are hit by a blast from the past!");

			if (p_ptr->muta3 & COR3_RES_TIME)
			{
				dam *= 4;
				dam /= (randint(6) + 6);
				msg_print("You feel as if time is passing you by.");
			}
			else
			{

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
						switch (randint(6))
						{
						case 1: k = A_STR; act = "strong"; break;
						case 2: k = A_INT; act = "bright"; break;
						case 3: k = A_WIS; act = "wise"; break;
						case 4: k = A_DEX; act = "agile"; break;
						case 5: k = A_CON; act = "hale"; break;
						case 6: k = A_CHA; act = "beautiful"; break;
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

						for (k = 0; k < 6; k++)
						{
							p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
							if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						}
						p_ptr->update |= (PU_BONUS);
						break;
					}
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Gravity -- stun plus slowness plus teleport */
	case GF_GRAVITY:
		{
			if (fuzzy) msg_print("You are hit by something heavy!");
			msg_print("Gravity warps around you.");
			teleport_player(5);
			if (!p_ptr->ffall)
				(void)set_timed_effect( TIMED_SLOW, p_ptr->slow + rand_int(4) + 4);
			if (!(p_ptr->resist_sound || p_ptr->ffall))
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + k);
			}
			if (p_ptr->ffall)
			{
				dam = (dam * 2) / 3;
			}

			if ((!p_ptr->ffall) || (randint(13)==1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			take_hit(dam, killer);
			break;
		}

		/* Standard damage */
	case GF_DISINTEGRATE:
		{
			if (fuzzy) msg_print("You are hit by pure energy!");
			take_hit(dam, killer);
			break;
		}

	case GF_OLD_HEAL:
		{
			if (fuzzy) msg_print("You are hit by something invigorating!");
			(void)hp_player(dam);
			dam = 0;
			break;
		}

	case GF_OLD_SPEED:
		{
			if (fuzzy)  msg_print("You are hit by something!");
			(void)set_timed_effect( TIMED_FAST, p_ptr->fast + randint(5));
			dam = 0;
			break;
		}

	case GF_OLD_SLOW:
		{
			if (fuzzy) msg_print("You are hit by something slow!");
			(void)set_timed_effect( TIMED_SLOW, p_ptr->slow + rand_int(4) + 4);
			break;
		}
	case GF_OLD_SLEEP:
		{
			if (p_ptr->free_act)  break;
			if (fuzzy) msg_print("You fall asleep!");
			set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + dam);
			dam = 0;
			break;
		}

		/* Pure damage */
	case GF_MANA:
		{
			if (fuzzy) msg_print("You are hit by an aura of magic!");
			take_hit(dam, killer);
			break;
		}

		/* Pure damage */
	case GF_METEOR:
		{
			if (fuzzy) msg_print("Something falls from the sky on you!");
			take_hit(dam, killer);
			if ((!p_ptr->resist_shard) || (randint(13)==1))
			{
				if(!p_ptr->immune_fire) inven_damage(set_fire_destroy, 2);
				inven_damage(set_cold_destroy, 2);
			}

			break;
		}

		/* Ice -- cold plus stun plus cuts */
	case GF_ICE:
		{
			if (fuzzy) msg_print("You are hit by something sharp and cold!");
			cold_dam(dam, killer);
			if (!p_ptr->resist_shard)
			{
				(void)set_timed_effect( TIMED_CUT, p_ptr->cut + damroll(5, 8));
			}
			if (!p_ptr->resist_sound)
			{
				(void)set_timed_effect( TIMED_STUN, p_ptr->stun + randint(15));
			}

			if ((!(p_ptr->resist_cold || p_ptr->oppose_cold)) || (randint(12)==1))
			{
				if(!(p_ptr->immune_cold)) inven_damage(set_cold_destroy, 3);
			}

			break;
		}


		/* Default */
	default:
		{
			/* No damage */
			dam = 0;

			break;
		}
	}


	/* Disturb */
	disturb(1, 0);


	/* Return "Anything seen?" */
	return (obvious);
}









/*
* Find the char to use to draw a moving bolt using ASCII
* It is moving (or has moved) from (x,y) to (nx,ny).
* If the distance is not "one", we (may) return "*".
*/
static char bolt_char(int y, int x, int ny, int nx)
{
	if ((ny == y) && (nx == x)) return '*';
	if (ny == y) return '-';
	if (nx == x) return '|';
	if ((ny-y) == (x-nx)) return '/';
	if ((ny-y) == (nx-x)) return '\\';
	return '*';
}

/*
* Find the char to use to draw a moving bolt using graphics
* It is moving (or has moved) from (x,y) to (nx,ny).
* If the distance is not "one", we (may) return "*".
*/
static char bolt_graf_char(int y, int x, int ny, int nx, int typ)
{
	byte base;
	base=base_bolt_char(typ);
	if ((ny == y) && (nx == x)) return (ball_graf_char(typ));
	if (ny == y) return (base+1);
	if (nx == x) return (base);
	if ((ny-y) == (x-nx)) return (base+2);
	if ((ny-y) == (nx-x)) return (base+3);
	return (ball_graf_char(typ));
}


/*
* Generic "beam"/"bolt"/"ball" projection routine.  -BEN-
*
* Input:
*   who: Index of "source" monster (or "zero" for "player")
*   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
*   y,x: Target location (or location to travel "towards")
*   dam: Base damage roll to apply to affected monsters (or player)
*   typ: Type of damage to apply to monsters (and objects)
*   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
*
* Return:
*   TRUE if any "effects" of the projection were observed, else FALSE
*
* Allows a monster (or player) to project a beam/bolt/ball of a given kind
* towards a given location (optionally passing over the heads of interposing
* monsters), and have it do a given amount of damage to the monsters (and
* optionally objects) within the given radius of the final location.
*
* A "bolt" travels from source to target and affects only the target grid.
* A "beam" travels from source to target, affecting all grids passed through.
* A "ball" travels from source to the target, exploding at the target, and
*   affecting everything within the given radius of the target location.
*
* Traditionally, a "bolt" does not affect anything on the ground, and does
* not pass over the heads of interposing monsters, much like a traditional
* missile, and will "stop" abruptly at the "target" even if no monster is
* positioned there, while a "ball", on the other hand, passes over the heads
* of monsters between the source and target, and affects everything except
* the source monster which lies within the final radius, while a "beam"
* affects every monster between the source and target, except for the casting
* monster (or player), and rarely affects things on the ground.
*
* Two special flags allow us to use this function in special ways, the
* "PROJECT_HIDE" flag allows us to perform "invisible" projections, while
* the "PROJECT_JUMP" flag allows us to affect a specific grid, without
* actually projecting from the source monster (or player).
*
* The player will only get "experience" for monsters killed by himself
* Unique monsters can only be destroyed by attacks from the player
*
* Only 256 grids can be affected per projection, limiting the effective
* "radius" of standard ball attacks to nine units (diameter nineteen).
*
* One can project in a given "direction" by combining PROJECT_THRU with small
* offsets to the initial location (see "line_spell()"), or by calculating
* "virtual targets" far away from the player.
*
* One can also use PROJECT_THRU to send a beam/bolt along an angled path,
* continuing until it actually hits somethings (useful for "stone to mud").
*
* Bolts and Beams explode INSIDE walls, so that they can destroy doors.
*
* Balls must explode BEFORE hitting walls, or they would affect monsters
* on both sides of a wall.  Some bug reports indicate that this is still
* happening in 2.7.8 for Windows, though it appears to be impossible.
*
* We "pre-calculate" the blast area only in part for efficiency.
* More importantly, this lets us do "explosions" from the "inside" out.
* This results in a more logical distribution of "blast" treasure.
* It also produces a better (in my opinion) animation of the explosion.
* It could be (but is not) used to have the treasure dropped by monsters
* in the middle of the explosion fall "outwards", and then be damaged by
* the blast as it spreads outwards towards the treasure drop location.
*
* Walls and doors are included in the blast area, so that they can be
* "burned" or "melted" in later versions.
*
* This algorithm is intended to maximize simplicity, not necessarily
* efficiency, since this function is not a bottleneck in the code.
*
* We apply the blast effect from ground zero outwards, in several passes,
* first affecting features, then objects, then monsters, then the player.
* This allows walls to be removed before checking the object or monster
* in the wall, and protects objects which are dropped by monsters killed
* in the blast, and allows the player to see all affects before he is
* killed or teleported away.  The semantics of this method are open to
* various interpretations, but they seem to work well in practice.
*
* We process the blast area from ground-zero outwards to allow for better
* distribution of treasure dropped by monsters, and because it provides a
* pleasing visual effect at low cost.
*
* Note that the damage done by "ball" explosions decreases with distance.
* This decrease is rapid, grids at radius "dist" take "1/dist" damage.
*
* Notice the "napalm" effect of "beam" weapons.  First they "project" to
* the target, and then the damage "flows" along this beam of destruction.
* The damage at every grid is the same as at the "center" of a "ball"
* explosion, since the "beam" grids are treated as if they ARE at the
* center of a "ball" explosion.
*
* Currently, specifying "beam" plus "ball" means that locations which are
* covered by the initial "beam", and also covered by the final "ball", except
* for the final grid (the epicenter of the ball), will be "hit twice", once
* by the initial beam, and once by the exploding ball.  For the grid right
* next to the epicenter, this results in 150% damage being done.  The center
* does not have this problem, for the same reason the final grid in a "beam"
* plus "bolt" does not -- it is explicitly removed.  Simply removing "beam"
* grids which are covered by the "ball" will NOT work, as then they will
* receive LESS damage than they should.  Do not combine "beam" with "ball".
*
* The array "gy[],gx[]" with current size "grids" is used to hold the
* collected locations of all grids in the "blast area" plus "beam path".
*
* Note the rather complex usage of the "gm[]" array.  First, gm[0] is always
* zero.  Second, for N>1, gm[N] is always the index (in gy[],gx[]) of the
* first blast grid (see above) with radius "N" from the blast center.  Note
* that only the first gm[1] grids in the blast area thus take full damage.
* Also, note that gm[rad+1] is always equal to "grids", which is the total
* number of blast grids.
*
* Note that once the projection is complete, (y2,x2) holds the final location
* of bolts/beams, and the "epicenter" of balls.
*
* Note also that "rad" specifies the "inclusive" radius of projection blast,
* so that a "rad" of "one" actually covers 5 or 9 grids, depending on the
* implementation of the "distance" function.  Also, a bolt can be properly
* viewed as a "ball" with a "rad" of "zero".
*
* Note that if no "target" is reached before the beam/bolt/ball travels the
* maximum distance allowed (MAX_RANGE), no "blast" will be induced.  This
* may be relevant even for bolts, since they have a "1x1" mini-blast.
*
* Some people have requested an "auto-explode ball attacks at max range"
* option, which should probably be handled by this function.  XXX XXX XXX
*
* Note that for consistency, we "pretend" that the bolt actually takes "time"
* to move from point A to point B, even if the player cannot see part of the
* projection path.  Note that in general, the player will *always* see part
* of the path, since it either starts at the player or ends on the player.
*
* Hack -- we assume that every "projection" is "self-illuminating".
*
* Mega-Hack -- when only a single monster is affected, we automatically track
* (and recall) that monster, unless "PROJECT_JUMP" is used.  XXX XXX XXX
*/
bool project(int who, int rad, int y, int x, int dam, int typ, int flg)
{
	int i, t, dist;
	int y1, x1, y2, x2;
	int /*y0, x0, UNUSED*/ y9, x9;
	int dist_hack = 0;
	int y_saver, x_saver; /* For reflecting monsters */

	int msec = delay_factor * delay_factor * delay_factor;

	/* Affected location(s) */
	cave_type *c_ptr;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Assume to be a normal ball spell */
	bool breath = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[256], gy[256];

	/* Encoded "radius" info (see above) */
	byte gm[32];

	/* Actual radius encoded in gm[] */
	int gm_rad = rad;


	/* Location of player */
	/* y0 = py; x0 = px; UNUSED */

	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;
	}

	/* Hack -- Start at player */
	else if (!who)
	{
		x1 = px;
		y1 = py;
	}

	/* Start at a monster */
	else
	{
		x1 = m_list[who].fx;
		y1 = m_list[who].fy;
	}

	y_saver = y1;
	x_saver = x1;


	/* Default "destination" */
	y2 = y; x2 = x;


	/* Hack -- verify stuff */
	if (flg & (PROJECT_THRU))
	{
		if ((x1 == x2) && (y1 == y2))
		{
			flg &= ~(PROJECT_THRU);
		}
	}

	/* Handle a breath attack */
	if (rad < 0)
	{
		rad = 0 - rad;
		breath = TRUE;
		flg |= PROJECT_HIDE;
	}

	/* Hack -- Assume there will be no blast (max radius 32) */
	for (dist = 0; dist < 32; dist++) gm[dist] = 0;


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Start at the source */
	x = x9 = x1;
	y = y9 = y1;
	dist = 0;

	/* Project until done */
	while (1)
	{
		/* Gather beam grids */
		if (flg & (PROJECT_BEAM))
		{
			gy[grids] = y;
			gx[grids] = x;
			grids++;
		}

		/* XXX XXX Hack -- Display "beam" grids */
		if (!blind && !(flg & (PROJECT_HIDE)) &&
			dist && (flg & (PROJECT_BEAM)) &&
			panel_contains(y, x) && player_has_los_bold(y, x))
		{
			if(!use_graphics)
			{
				/* Hack -- Visual effect -- "explode" the grids */
				print_rel('*', spell_colour(typ), y, x);
			}
			else
			{
				print_rel(ball_graf_char(typ),ball_graf_attr(typ),y,x);
			}
		}

		/* Check the grid */
		c_ptr = &cave[y][x];

		/* Never pass through walls */
		if (dist && !cave_floor_bold(y, x)) break;

		/* Check for arrival at "final target" (if desired) */
		if (!(flg & (PROJECT_THRU)) && (x == x2) && (y == y2)) break;

		/* If allowed, and we have moved at all, stop when we hit anybody */
		if (c_ptr->m_idx && dist && (flg & (PROJECT_STOP))) break;


		/* Calculate the new location */
		y9 = y;
		x9 = x;
		mmove2(&y9, &x9, y1, x1, y2, x2);

		/* Hack -- Balls explode BEFORE reaching walls or doors */
		if (!cave_floor_bold(y9, x9) && (rad > 0)) break;

		/* Keep track of the distance traveled */
		dist++;

		/* Nothing can travel furthur than the maximal distance */
		if (dist > MAX_RANGE) break;

		/* Only do visual effects (and delay) if requested */
		if (!blind && !(flg & (PROJECT_HIDE)))
		{
			/* Only do visuals if the player can "see" the bolt */
			if (player_has_los_bold(y9, x9) && panel_contains(y9, x9))
			{
				if(!use_graphics)
				{
					/* Visual effects -- Display, Highlight, Flush, Pause, Erase */
					print_rel(bolt_char(y, x, y9, x9), spell_colour(typ), y9, x9);
				}
				else
				{
					print_rel(bolt_graf_char(y,x,y9,x9,typ),bolt_graf_attr(typ),y9,x9);
				}
				move_cursor_relative(y9, x9);
				Term_fresh();
				visual = TRUE;
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(y9, x9);
				Term_fresh();
			}

			/* Hack -- make sure to delay anyway for consistency */
			else if (visual)
			{
				/* Delay for consistency */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}

		/* Save the new location */
		y = y9;
		x = x9;
	}


	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Start the "explosion" */
	gm[0] = 0;

	/* Hack -- make sure beams get to "explode" */
	gm[1] = grids;

	dist_hack = dist;

	/* If we found a "target", explode there */
	if (dist <= MAX_RANGE)
	{
		/* Mega-Hack -- remove the final "beam" grid */
		if ((flg & (PROJECT_BEAM)) && (grids > 0)) grids--;

		/*
		* Create a conical breath attack
		*
		*         ***
		*     ********
		* D********@**
		*     ********
		*         ***
		*/
		if (breath)
		{
			int by, bx;
			int brad = 0;
			int bdis = 0;
			int cdis;

			/* Not done yet */
			bool done = FALSE;

			flg &= ~(PROJECT_HIDE);

			by = y1;
			bx = x1;

			while (bdis <= dist + rad)
			{
				/* Travel from center outward */
				for (cdis = 0; cdis <= brad; cdis++)
				{
					/* Scan the maximal blast area of radius "cdis" */
					for (y = by - cdis; y <= by + cdis; y++)
					{
						for (x = bx - cdis; x <= bx + cdis; x++)
						{
							/* Ignore "illegal" locations */
							if (!in_bounds(y, x)) continue;

							/* Enforce a circular "ripple" */
							if (distance(y1, x1, y, x) != bdis) continue;

							/* Enforce an arc */
							if (distance(by, bx, y, x) != cdis) continue;

							/* The blast is stopped by walls */
							if (!los(by, bx, y, x)) continue;

							/* Save this grid */
							gy[grids] = y;
							gx[grids] = x;
							grids++;
						}
					}
				}

				/* Encode some more "radius" info */
				gm[bdis + 1] = grids;

				/* Stop moving */
				if ((by == y2) && (bx == x2)) done = TRUE;

				/* Finish */
				if (done)
				{
					bdis++;
					continue;
				}

				/* Ripple outwards */
				mmove2(&by, &bx, y1, x1, y2, x2);

				/* Find the next ripple */
				bdis++;

				/* Increase the size */
				brad = (rad * bdis) / dist;
			}

			/* Store the effect size */
			gm_rad = bdis;
		}

		else
		{
			/* Determine the blast area, work from the inside out */
			for (dist = 0; dist <= rad; dist++)
			{
				/* Scan the maximal blast area of radius "dist" */
				for (y = y2 - dist; y <= y2 + dist; y++)
				{
					for (x = x2 - dist; x <= x2 + dist; x++)
					{
						/* Ignore "illegal" locations */
						if (!in_bounds2(y, x)) continue;

						/* Enforce a "circular" explosion */
						if (distance(y2, x2, y, x) != dist) continue;

						/* Ball explosions are stopped by walls */
						if (typ == GF_DISINTEGRATE)
						{
							if (cave_valid_bold(y,x))
								cave_set_feat(y, x, FEAT_FLOOR);

							/* Update some things -- similar to GF_KILL_WALL */
							p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

						}

						else

						{
							if (!los(y2, x2, y, x)) continue;
						}



						/* Save this grid */
						gy[grids] = y;
						gx[grids] = x;
						grids++;
					}
				}

				/* Encode some more "radius" info */
				gm[dist+1] = grids;
			}
		}
	}

	/* Speed -- ignore "non-explosions" */
	if (!grids) return (FALSE);


	/* Display the "blast area" */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Then do the "blast", from inside out */
		for (t = 0; t <= gm_rad; t++)
		{
			/* Dump everything with this radius */
			for (i = gm[t]; i < gm[t+1]; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* The player can see it */
				if (player_has_los_bold(y, x) &&
					panel_contains(y, x))
				{
					drawn = TRUE;
					if(!use_graphics)
					{
						/* Hack -- Visual effect -- "explode" the grids */
						print_rel('*', spell_colour(typ), y, x);
					}
					else
					{
						print_rel(ball_graf_char(typ),ball_graf_attr(typ),y,x);
					}
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush each "radius" seperately */
			Term_fresh();

			/* Delay (efficiently) */
			if (visual || drawn)
			{
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}

		/* Flush the erasing */
		if (drawn)
		{
			/* Erase the explosion drawn above */
			for (i = 0; i < grids; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* Erase if needed */
				if (player_has_los_bold(y, x) &&
					panel_contains(y, x))
				{
					lite_spot(y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush the explosion */
			Term_fresh();
		}
	}


	/* Check features */
	if (flg & (PROJECT_GRID))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_f(who, dist, y, x, dam, typ)) notice = TRUE;
		}
	}


	/* Check objects */
	if (flg & (PROJECT_ITEM))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for objects */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the object in the grid */
			if (project_o(who, dist, y, x, dam, typ)) notice = TRUE;
		}
	}


	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;

		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			if (grids > 1)
			{
				/* Affect the monster in the grid */
				if (project_m(who, dist, y, x, dam, typ)) notice = TRUE;
			}
			else
			{
				monster_race *ref_ptr = &r_info[m_list[c_ptr->m_idx].r_idx];
				if ((ref_ptr->flags2 & (RF2_REFLECTING)) && (randint(10)!=1)
					&& (dist_hack > 1) && !(typ & GF_MAGEBOLT))
				{
					byte t_y, t_x;
					int max_attempts = 10;

					/* Choose 'new' target */
					do
					{
						t_y = y_saver - 1 + (byte_hack)randint(3);
						t_x = x_saver - 1 + (byte_hack)randint(3);
						max_attempts--;
					}
				/* One day, I will know what this means :  */
					while ( (max_attempts > 0)
						    &&
							in_bounds2_unsigned(t_y, t_x)
							&&
						    !(los(y, x, t_y, t_x))
						  );

					if (max_attempts < 1)
					{

						t_y = y_saver;
						t_x = x_saver;
					}

					if (m_list[c_ptr->m_idx].ml)
					{
						msg_print("The attack bounces!");
						ref_ptr->r_flags2 |= RF2_REFLECTING;
					}
					project(c_ptr->m_idx, 0, t_y, t_x,  dam, typ, flg);
				}
				else
				{
					if (project_m(who, dist, y, x, dam, typ)) notice = TRUE;
				}

			}
		}

		/* Player affected one monster (without "jumping") */
		if (!who && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Access */
			c_ptr = &cave[y][x];

			/* Track if possible */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(c_ptr->m_idx);
			}
		}
	}


	/* Check player */
	if (flg & (PROJECT_KILL))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for player */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the player */
			if (project_p(who, dist, y, x, dam, typ, rad)) notice = TRUE;
		}
	}


	/* Return "something was noticed" */
	return (notice);
}




/*
* Potions "smash open" and cause an area effect when
* (1) they are shattered while in the player's inventory,
* due to cold (etc) attacks;
* (2) they are thrown at a monster, or obstacle;
* (3) they are shattered by a "cold ball" or other such spell
* while lying on the floor.
*
* Arguments:
*    who   ---  who caused the potion to shatter (0=player)
*               potions that smash on the floor are assumed to
*               be caused by no-one (who = 1), as are those that
*               shatter inside the player inventory.
*              (Not anymore -- I changed this; TY)
*    y, x  --- coordinates of the potion (or player if
*          the potion was in her inventory);
*    o_ptr --- pointer to the potion object.
*/
bool potion_smash_effect(int who, int y, int x, object_type *o_ptr, monster_type *m_ptr)
{
	int radius = 2;
	int     dt = 0;
	int    dam = 0;
	bool ident = FALSE;
	bool angry = FALSE;
	char m_name[80];
	monster_race *r_ptr;

	if(m_ptr!=NULL)
	{
		/* Get the monster name (or "it") */
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);
	}

	int o_sval = o_ptr->sval;

	/* Check for the human applicable ones up front */
	if(m_ptr!=NULL && r_ptr->d_char == 'p' && o_sval >= SV_POTION_DEC_STR && o_sval <= SV_POTION_DEC_CHA)
	{
		if (o_sval == SV_POTION_DEC_STR)
		{
			msg_format("%^s weakens in front of you!", m_name);
		}else if (o_sval == SV_POTION_DEC_CON)
		{
			msg_format("%^s shrivels in front of you!", m_name);
		}else if (o_sval == SV_POTION_DEC_DEX)
		{
			msg_format("%^s slows in front of you!", m_name);
		}else if (o_sval == SV_POTION_DEC_INT)
		{
			msg_format("%^s drools in front of you!", m_name);
		}else if (o_sval == SV_POTION_DEC_WIS)
		{
			msg_format("%^s slobbers in front of you!", m_name);
		}else if (o_sval == SV_POTION_DEC_CHA)
		{
			msg_format("%^s becomes uglier in front of you!", m_name);
		}else if (o_sval == SV_POTION_LOSE_MEMORIES)
		{
			msg_format("%^s becomes lesser in front of you!", m_name);
		}
		/*Do 12 percent damage to current hitpoints*/
		project(who, 0, y, x, m_ptr->hp >> 3, GF_ARROW, (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));
		/*User becomes aware*/
		object_aware(o_ptr);
		object_known(o_ptr, FALSE);
		return TRUE;
	}

	/* Check for the human applicable ones up front */
	if(m_ptr!=NULL && o_sval >= SV_POTION_INC_STR && o_sval <= SV_POTION_INC_CHA)
	{
		if (o_sval == SV_POTION_INC_STR || o_sval == SV_POTION_INC_CON)
		{
			msg_format("%^s bulks up in front of you!", m_name);
		}else if (o_sval == SV_POTION_INC_DEX)
		{
			msg_format("%^s speeds up in front of you!", m_name);
		}else if (o_sval == SV_POTION_INC_INT || o_sval == SV_POTION_INC_WIS)
		{
			msg_format("%^s glares at you with new understanding!", m_name);
		}else if (o_sval == SV_POTION_INC_CHA)
		{
			msg_format("%^s becomes more beautiful by the second!", m_name);
		}
		/*Add 12 percent damage to current hitpoints*/
		m_ptr->maxhp = m_ptr->maxhp + (m_ptr->maxhp >> 3);
		project(who, 0, y, x, m_ptr->hp >> 3, GF_OLD_HEAL, (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));
		/*Gives some extra speed ;]*/
		m_ptr->mspeed = 150;
		/*User becomes aware*/
		object_aware(o_ptr);
		object_known(o_ptr, FALSE);
		return TRUE;
	}


	switch(o_sval) {
	/* These do upset monsters , without actually damaging them */
		/*This is in case the monster is not a human*/
		case SV_POTION_LOSE_MEMORIES:
		case SV_POTION_DEC_STR:
		case SV_POTION_DEC_INT:
		case SV_POTION_DEC_WIS:
		case SV_POTION_DEC_DEX:
		case SV_POTION_DEC_CON:
		case SV_POTION_DEC_CHA:
			return TRUE;
		/*I cant help it.. Getting salty, lol!*/
		case SV_POTION_SALT_WATER:
			if(m_ptr!=NULL) msg_format("%^s gets salty!", m_name);
		case SV_POTION_WATER: /* perhaps a 'water' attack? */
			if(m_ptr!=NULL) msg_format("%^s gets wet!", m_name);
			object_aware(o_ptr);
			object_known(o_ptr, FALSE);
			return FALSE;
		/* Slime mold, water and apple juice used to upset allies ?? */
		case SV_POTION_SLIME_MOLD:
		case SV_POTION_APPLE_JUICE:
		/* These do not upset monsters ever */
		case SV_POTION_INFRAVISION:
		case SV_POTION_DETECT_INVIS:
		case SV_POTION_SLOW_POISON:
		case SV_POTION_CURE_POISON:
		case SV_POTION_BOLDNESS:
		case SV_POTION_RESIST_HEAT:
		case SV_POTION_RESIST_COLD:
		case SV_POTION_HEROISM:
		case SV_POTION_BESERK_STRENGTH:
		case SV_POTION_RESTORE_EXP:
		case SV_POTION_RES_STR:
		case SV_POTION_RES_INT:
		case SV_POTION_RES_WIS:
		case SV_POTION_RES_DEX:
		case SV_POTION_RES_CON:
		case SV_POTION_RES_CHA:
		case SV_POTION_INC_STR:
		case SV_POTION_INC_INT:
		case SV_POTION_INC_WIS:
		case SV_POTION_INC_DEX:
		case SV_POTION_INC_CON:
		case SV_POTION_INC_CHA:
		case SV_POTION_ENLIGHTENMENT:
		case SV_POTION_STAR_ENLIGHTENMENT:
		case SV_POTION_SELF_KNOWLEDGE:
		case SV_POTION_RESISTANCE:
		case SV_POTION_INVULNERABILITY:
		case SV_POTION_NEW_LIFE:
		/* All of the above potions have no effect when shattered */
			return FALSE;
		case SV_POTION_SLOWNESS:
			dt = GF_OLD_SLOW;
			dam = 5;
			angry = TRUE;
			break;
		case SV_POTION_POISON:
			dt = GF_POIS;
			dam = 3;
			angry = TRUE;
			break;
		case SV_POTION_BLINDNESS:
			dt = GF_DARK;
			angry = TRUE;
			break;
		case SV_POTION_CONFUSION: /* Booze */
			dt = GF_OLD_CONF;
			angry = TRUE;
			break;
		case SV_POTION_SLEEP:
			dt = GF_OLD_SLEEP;
			angry = TRUE;
			break;
		case SV_POTION_RUINATION:
		case SV_POTION_DETONATIONS:
			dt = GF_SHARDS;
			dam = damroll(25, 25);
			angry = TRUE;
			break;
		case SV_POTION_IOCAINE:
			dt = GF_DEATH_RAY; /* !! */
			angry = TRUE;
			radius = 1;
			break;
		case SV_POTION_SPEED:
			dt = GF_OLD_SPEED;
			break;
		case SV_POTION_CURE_LIGHT:
			dt = GF_OLD_HEAL;
			dam = damroll(2,3);
			break;
		case SV_POTION_CURE_SERIOUS:
			dt = GF_OLD_HEAL;
			dam = damroll(4,3);
			break;
		case SV_POTION_CURE_CRITICAL:
		case SV_POTION_CURING:
			dt = GF_OLD_HEAL;
			dam = damroll(6,3);
			break;
		case SV_POTION_HEALING:
			dt = GF_OLD_HEAL;
			dam = damroll(10,10);
			break;
		case SV_POTION_EXPERIENCE:
		case SV_POTION_AUGMENTATION:
			dt = GF_EXP;
			dam = 1;
			radius = 1;
			break;
		case SV_POTION_STAR_HEALING:
		case SV_POTION_LIFE:
			dt = GF_OLD_HEAL;
			dam = damroll(50,50);
			radius = 1;
			break;
		case SV_POTION_RESTORE_MANA: /* MANA */
			dt = GF_MANA;
			dam = damroll(10,10);
			radius = 1;
			break;
	default:
			/* Do nothing */	;
	}

	if(dt)
	{
		project(who, radius, y, x, dam, dt, (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));
		ident = TRUE;
	}

	if(ident)
	{
		object_aware(o_ptr);
		object_known(o_ptr,FALSE);
	}

	return angry;
}

/*Helper function for whether we forgot a spell*/
bool spell_forgotten( u16b realm  , int spell )
{
	/*Silly 0 is no realm except if it is Miracles ...*/
	realm++;
	/*Check if we are checking for realm1 and return value accordingly*/
	if(realm==p_ptr->realm1)
	{
		return (spell_forgotten1 & (1L << spell))?TRUE:FALSE;
	}
	/*Check if we are checking for realm2 and return value accordingly*/
	if(realm==p_ptr->realm2)
	{
		return (spell_forgotten2 & (1L << spell))?TRUE:FALSE;
	}
	return FALSE;
}


/*Helper function for whether we learned a spell*/
bool spell_learned( u16b realm  , int spell )
{
	/*Silly 0 is no realm except if it is Miracles ...*/
	realm++;
	/*Check if we are checking for realm1 and return value accordingly*/
	if(realm==p_ptr->realm1)
	{
		return (spell_learned1 & (1L << spell))?TRUE:FALSE;
	}
	/*Check if we are checking for realm2 and return value accordingly*/
	if(realm==p_ptr->realm2)
	{
		return (spell_learned2 & (1L << spell))?TRUE:FALSE;
	}
	return FALSE;
}

/*Helper function for whether we tried a spell*/
bool spell_worked( u16b realm  , int spell )
{
	/*Silly 0 is no realm except if it is Miracles ...*/
	realm++;
	/*Check if we are checking for realm1 and return value accordingly*/
	if(realm==p_ptr->realm1)
	{
		return (spell_worked1 & (1L << spell))?TRUE:FALSE;
	}
	/*Check if we are checking for realm2 and return value accordingly*/
	if(realm==p_ptr->realm2)
	{
		return (spell_worked2 & (1L << spell))?TRUE:FALSE;
	}
	return FALSE;
}

void get_spell_info( u16b realm  , int spell , magic_type *s_ptr )
{
	int spell_skill;
	spell_type *spell_info;

	/* How well can the player cast spells from this realm ?*/
	spell_skill = mp_ptr->skill[realm]-1;
	/* Browsing realms with skill NO, have now -1 in spell_skill, not good for array access*/
	if(spell_skill<0)spell_skill = 0;
	/* Get the information of the actual spell */
	spell_info = &spells[realm][spell];
	/*Copy over the name*/
	s_ptr->name = spell_info->name;
	/*Copy over the macro*/
	s_ptr->macro = spell_info->macro;
	/*Copy over the spoiler*/
	s_ptr->spoiler = spell_info->spoiler;
	/*Copy over the experience gained, super casters gain double xp*/
	s_ptr->sexp  = spell_skill==SUPER?spell_info->sexp*2:spell_info->sexp;
	/* Translate the spell chance, better skill, lower percentage of failure etc.*/
	switch( spell_skill )
	{
		case POOR:
			s_ptr->sfail = spell_info->sfail - 10;
			break;
		case WORSE:
			s_ptr->sfail = spell_info->sfail - 5;
			break;
		case SAME:
			s_ptr->sfail = spell_info->sfail;
			break;
		case BETTER:
			s_ptr->sfail = spell_info->sfail + 10;
			break;
		case SUPER:
			s_ptr->sfail = spell_info->sfail - 15;
			break;
	}
	/* Translate the level*/
	/*msg_format( "TODO%d with %d -> %d" , spell_info->slevel
		, spell_skill
		, spell_skill_level[ spell_info->slevel - 1 ][spell_skill]);	*/
	s_ptr->slevel = spell_skill_level[ spell_info->slevel - 1 ][spell_skill];
	/* Translate the mana requirement*/
	if( spell_info->smana > 25 )
		s_ptr->smana = spell_info->smana + spell_skill_mana[25][spell_skill];
	else
		s_ptr->smana = spell_skill_mana[spell_info->smana - 1][spell_skill];
	/*Some safe values for the non extended stuff in case of*/
	s_ptr->info = "";
	s_ptr->attr_info = TERM_WHITE;
	s_ptr->attr_realm = TERM_WHITE;
	s_ptr->forgotten = FALSE;
	s_ptr->learned = FALSE;
	s_ptr->worked	= FALSE;
}

void get_extended_spell_info( u16b realm  , int spell , magic_type *s_ptr )
{
	get_spell_info( realm , spell ,s_ptr );
	/*Set up some boolean flags*/
	s_ptr->forgotten = spell_forgotten(realm,spell);
	s_ptr->learned   = spell_learned(realm,spell);
	s_ptr->worked    = spell_worked(realm,spell);
	/* Get color of realm */
	s_ptr->attr_realm = tval_to_attr[ TV_MIRACLES_BOOK + realm ];
	/* Default info color to white */
	s_ptr->attr_info = TERM_WHITE;
   /* Get info */
	spell_info_short( short_info , spell, realm);
	s_ptr->info = short_info; /* Used to be initialized with NULL */

	/* Check for illegible */
	if (s_ptr->slevel >= 99)
	{
		/* Illegible */
		s_ptr->name = "(illegible)";
		/* Unusable */
		s_ptr->attr_info = TERM_L_DARK;
	}
	else if ( s_ptr->forgotten )
	{
		s_ptr->attr_info = TERM_ORANGE;
		s_ptr->info      = "forgotten";
	}
	else if ( !s_ptr->learned  )
	{
		s_ptr->attr_info = TERM_L_DARK;
		s_ptr->info      = "unknown";
	}
	else if ( !s_ptr->worked )
	{
		s_ptr->attr_info = TERM_YELLOW;
		s_ptr->info      = "untried";
	}
}

static void phlogiston (void)
{

	int max_flog = 0;
	object_type * o_ptr = &inventory[INVEN_LITE];

	/* It's a lamp */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{
		max_flog = FUEL_LAMP;
	}

	/* It's a torch */
	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{
		max_flog = FUEL_TORCH;
	}

	/* No torch to refill */
	else
	{
		msg_print("You are not wielding anything which uses phlogiston.");
		return;
	}

	if (o_ptr->pval >= max_flog)
	{
		msg_print("No more phlogiston can be put in this item.");
		return;
	}

	/* Refuel */
	o_ptr->pval += (max_flog / 2);

	/* Message */
	msg_print("You add phlogiston to your light item.");

	/* Comment */
	if (o_ptr->pval >= max_flog)
	{
		o_ptr->pval = max_flog;
		msg_print("Your light item is full.");
	}


	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


static void brand_weapon(int brand_type)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artefacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if ((o_ptr->k_idx) &&
		(!artefact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		(!(o_ptr->art_name)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;

		char o_name[80];
		object_desc(o_name, o_ptr, FALSE, 0); /* Let's get the name before
											  it is changed... */

		switch (brand_type)
		{
		case 5:
			act = "can barely contain the elemental violence within.";
			o_ptr->name2 = EGO_ELEMENTS_MINCHIATE;
			break;
		case 4:
			act = "seems very unstable now.";
			o_ptr->name2 = EGO_PLANAR;
			o_ptr->pval = (s16b)randint(2);
			break;
		case 3:
			act = "thirsts for blood!";
			o_ptr->name2 = EGO_VAMPIRIC;
			break;
		case 2:
			act = "is coated with poison.";
			o_ptr->name2 = EGO_BRAND_POIS;
			break;
		case 1:
			act = "is engulfed in raw chaos!";
			o_ptr->name2 = EGO_CHAOTIC;
			break;
		default:
			if (rand_int(100) < 25)
			{
				act = "is covered in a fiery shield!";
				o_ptr->name2 = EGO_BRAND_FIRE;
			}

			else
			{
				act = "glows deep, icy blue!";
				o_ptr->name2 = EGO_BRAND_COLD;
			}
		}

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();

		msg_print("The Branding failed.");
	}
}


static void call_the_(void)
{
	int i;

	if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
		cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
		cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
		cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))

	{
		for (i = 1; i < 10; i++)
			if (i-5) fire_ball(GF_SHARD, i,
				175, 2);

		for (i = 1; i < 10; i++)
			if (i-5) fire_ball(GF_MANA, i,
				175, 3);

		for (i = 1; i < 10; i++)
			if (i-5) fire_ball(GF_HELLSLIME, i,
				175, 4);
	}




	else
	{
		msg_format("You %s the %s too close to a wall!",
			((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "recite" : "cast"),
			((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "prayer" : "spell"));
		msg_print("There is a loud explosion!");
		destroy_area(py, px, 20+(p_ptr->lev), TRUE);
		msg_print("The dungeon collapses...");
		take_hit(100 + (randint(150)), "a suicidal Call the Void");
	}
}

void rustproof(void)
{
	int		item;

	object_type	*o_ptr;

	char		o_name[80];

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Rustproof which piece of armour? ", "You have nothing to rustproof." , USE_INVEN | USE_EQUIP | USE_FLOOR))
	{
		return;
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	o_ptr->art_flags3 |= TR3_IGNORE_ACID;

	if ((o_ptr->to_a < 0) && !(o_ptr->ident & IDENT_CURSED))
	{
		msg_format("%s %s look%s as good as new!",
			((item >= 0) ? "Your" : "The"), o_name,
			((o_ptr->number > 1) ? "" : "s"));
		o_ptr->to_a = 0;
	}

	msg_format("%s %s %s now protected against corrosion.",
		((item >= 0) ? "Your" : "The"), o_name,
		((o_ptr->number > 1) ? "are" : "is"));

}

void wild_magic(int spell)
{
	int counter = 0;
	int type = FILTER_BIZARRE1 - 1 + (randint(6));
	if (type < FILTER_BIZARRE1) type = FILTER_BIZARRE1;
	else if (type > FILTER_BIZARRE6) type = FILTER_BIZARRE6;

	switch(randint(spell) + randint(8) + 1)

	{
	case 1: case 2: case 3:
		teleport_player(10);
		break;
	case 4: case 5: case 6:
		teleport_player(100);
		break;
	case 7: case 8:
		teleport_player(200);
		break;
	case 9: case 10: case 11:
		unlite_area(10,3);
		break;
	case 12: case 13: case 14:
		lite_area(damroll(2,3),2);
		break;
	case 15:
		destroy_doors_touch();
		break;
	case 16: case 17:
		wall_breaker();
	case 18:
		sleep_monsters_touch();
		break;
	case 19: case 20:
		trap_creation();
		break;
	case 21: case 22:
		door_creation();
		break;
	case 23: case 24: case 25:
		aggravate_monsters(1);
		break;
	case 26:
		earthquake(py, px, 5);
		break;
	case 27: case 28:
		(void) gain_corruption(0);
		break;
	case 29: case 30:
		apply_disenchant(0);
		break;
	case 31:
		lose_all_info();
		break;
	case 32:
		fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
		break;
	case 33:
		wall_stone();
		break;
	case 34: case 35:
		while (counter++ < 8)
		{
			(void) summon_specific(py, px, (dun_level * 3) / 2, type);
		}
		break;
	case 36: case 37:
		activate_hi_summon();
		break;
	case 38:
		summon_reaver();
	default:
		activate_ty_curse();
	}
	return;
}

/*
 *  Checks odds of a friendly monster, if the odds are good, try to summon the monster ( return success of it, sometimes noone appears )
 *  Otherwise try to summon an enemy of the same type and show a message that the monster is not coming in peace
 *  Return success of summoning an angry monster as well.
 */
bool summon_specific_potential_ally( int type , int treshold_friendly , int dice , cptr angry )
{
	if (randint(dice)>treshold_friendly)
	{
		return (summon_specific_friendly(py, px, p_ptr->lev, type, FALSE));
	}
	else
	{
		if (summon_specific(py, px, p_ptr->lev, FILTER_DEMON))
		{
			msg_print( angry );
			return TRUE;
		}
	}
	return FALSE;
}

/* Fetch an item (teleport it right underneath the caster) */
void fetch(int dir, int wgt, bool require_los)
{
	int ty, tx, i;
	/*bool flag; UNUSED*/
	cave_type *c_ptr;
	object_type *o_ptr;

	/* Check to see if an object is already there */
	if(cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if(dir==5 && target_okay())
	{
		tx = target_col;
		ty = target_row;
		if(distance(py, px, ty, tx)>MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}
		c_ptr = &cave[ty][tx];

		if (require_los && (!player_has_los_bold(ty,tx)))
		{
			msg_print("You have no direct line of sight to that location.");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;
		/*flag = FALSE; UNUSED*/
		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];
			if ((distance(py, px, ty, tx)> MAX_RANGE)
				|| !cave_floor_bold(ty, tx)) return;
		} while(!c_ptr->o_idx);
	}
	o_ptr = &o_list[c_ptr->o_idx];
	if (o_ptr->weight > wgt)
	{   /* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}
	i = c_ptr->o_idx;
	c_ptr->o_idx = 0;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->iy = (byte)py;
	o_ptr->ix = (byte)px;


	note_spot(py,px);
	p_ptr->redraw |= PR_MAP;

}

/*
* Cast a spell
*/
void do_cmd_cast(void)
{
	int	item, sval, spell, dir, realm;
	int	chance, beam;
	int	plev = p_ptr->lev;
	int	increment = 0, dummy = 0;
	int	use_realm, i;

	bool  first_time =  FALSE;
	int   xp_gain = 0;
	bool  none_came = FALSE;
	const cptr prayer = ((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "prayer" : "spell");

	object_type	*o_ptr;
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	char	ppp[80];

	char	tmp_val[160];

	/* Require spell ability */
	if (p_ptr->realm1 == 0)
	{
		msg_print("You cannot cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Restrict choices to spell books */
	item_tester_tval = (byte)mp_ptr->spell_book;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Use which book? ", "You have no spell books!", USE_INVEN | USE_FLOOR))
	{
		return;
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == p_ptr->realm2+89) increment = 32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2-1;
	else realm = p_ptr->realm1-1;

	/* Ask for a spell */
	if (!get_spell(&spell, ((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "recite" : "cast"), (increment?p_ptr->realm2:p_ptr->realm1), sval, TRUE ) )
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}

	/* Access the spell */
	use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

	get_extended_spell_info( use_realm-1  , spell , s_ptr );

	/* Verify "dangerous" spells */

	if( p_ptr->pclass != CLASS_BLOOD_MAGE )
	{
		if (s_ptr->smana > p_ptr->csp)
		{
			/* Warning */
			msg_format("You do not have enough mana to %s this %s.",
				((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "recite" : "cast"),
				prayer);
			/* Verify */
			if (!get_check("Attempt it anyway? ")) return;
		}
	}
	else
	{
		if (s_ptr->smana > p_ptr->chp)
		{
			/* Warning */
			msg_format("You do not have enough blood to %s this %s.",
				((mp_ptr->spell_book == TV_MIRACLES_BOOK) ? "recite" : "cast"),
				prayer);
			/* Verify */
			if (!get_check("Attempt it anyway? ")) return;
		}
	}

	/* Spell failure chance */
	chance = spell_chance(spell,use_realm-1);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);

		if (o_ptr->tval == TV_CHAOS_BOOK && (randint(100)<spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
		if (o_ptr->tval == TV_DEMONIC_BOOK && (randint(100)<spell))
		{
			msg_print("You anger your patron!");
			summon_specific(py,px,dun_level,FILTER_DEMON);
		}
		else if (o_ptr->tval == TV_DEATH_BOOK && (randint(100)<spell))
		{
				msg_print("It hurts!");
				take_hit(damroll((o_ptr->sval)+1,6), "a miscast Death spell");
				if (spell>15 && randint(6)==1 && !(p_ptr->hold_life))
					lose_exp(spell * 250);
		}
	}

	/* Process spell */
	else
	{

		if (p_ptr->pclass == CLASS_MAGE) beam = plev;
		else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
		else if (p_ptr->pclass == CLASS_BLOOD_MAGE) beam = plev + 10;
		else beam = plev / 2;


		/* Spells.  */
		switch (realm)
		{
		case 0: /* * LIFE * */
			switch (spell)
			{
			case 0: /* Detect Evil */
				(void)detect_monsters_evil();
				break;
			case 1: /* Cure Light Wounds */
				(void)hp_player(damroll(2, 10));
				(void)set_timed_effect( TIMED_CUT, p_ptr->cut - 10);
				break;
			case 2: /* Bless */
				(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(12) + 12);
				break;
			case 3: /* Remove Fear */
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 4: /* Call Light */
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			case 5: /* Detect Traps + Secret Doors */
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			case 6: /* Cure Medium Wounds */
				(void)hp_player(damroll(4, 10));
				(void)set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 20);
				break;
			case 7: /* Satisfy Hunger */
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			case 8: /* Remove Curse */
				remove_curse();
				break;
			case 9: /* Cure Poison */
				(void)set_timed_effect( TIMED_POISONED , 0);
				break;
			case 10: /* Cure Critical Wounds */
				(void)hp_player(damroll(8, 10));
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 11: /* Sense Unseen */
				(void)set_timed_effect( TIMED_TIM_INVIS, p_ptr->tim_invis + randint(24) + 24);
				break;
			case 12: /* Orb or Draining */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_FIRE, dir,
					(damroll(3, 6) + plev +
					(plev / ((p_ptr->pclass == CLASS_PRIEST
					       || p_ptr->pclass == CLASS_HIGH_MAGE
						   || p_ptr->pclass == CLASS_BLOOD_MAGE ) ? 2 : 4))),
					((plev < 30) ? 2 : 3));
				break;
			case 13: /* Protection from Evil */
				(void)set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
				break;
			case 14: /* Healing */
				(void)hp_player(300);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 15: /* Glyph of Warding */
				warding_glyph();
				break;
			case 16: /* Exorcism */
				(void) dispel_undead(plev);
				(void) dispel_demons(plev);
				(void) turn_evil(plev);
				break;
			case 17: /* Dispel Curse */
				(void)remove_all_curse();
				break;
			case 18: /* Dispel Undead + Demons */
				(void)dispel_undead(plev * 3);
				(void)dispel_demons(plev * 3);
				break;
			case 19: /* 'Day of the Dove' */
				charm_monsters(plev * 2);
				break;
			case 20: /* Dispel Evil */
				(void)dispel_evil(plev * 4);
				break;
			case 21: /* Banishment */
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			case 22: /* Holy Word */
				(void)dispel_evil(plev * 4);
				(void)hp_player(1000);
				(void)set_timed_effect( TIMED_AFRAID , 0);
				(void)set_timed_effect( TIMED_POISONED , 0);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 23: /* Warding True */
				warding_glyph();
				glyph_creation();
				break;
			case 24: /* Heroism */
				(void)set_timed_effect( TIMED_HERO, p_ptr->hero + randint(25) + 25);
				(void)hp_player(10);
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 25: /* Prayer */
				(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(48) + 48);
				break;
			case 26:
				bless_weapon();
				break;
			case 27: /* Restoration */
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHA);
				(void)restore_level();
				break;
			case 28: /* Healing True */
				(void)hp_player(2000);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 29: /* Holy Vision */
				identify_fully();
				break;
			case 30: /* Divine Intervention */
				project(0, 1, py, px, 777, GF_HOLY_FIRE,   PROJECT_KILL);
				dispel_monsters(plev * 4);
				slow_monsters();
				stun_monsters(plev*4);
				confuse_monsters(plev*4);
				turn_monsters(plev*4);
				stasis_monsters(plev*4);
				(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(25) + 25);
				(void)hp_player(300);
				if (!p_ptr->fast) {   /* Haste */
					(void)set_timed_effect( TIMED_FAST, randint(20 + (plev) ) + plev);
				} else {
					(void)set_timed_effect( TIMED_FAST, p_ptr->fast + randint(5));
				}
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 31: /* Holy Invulnerability */
				(void)set_timed_effect( TIMED_INVULN, p_ptr->invuln + randint(7) + 7);
				break;
			default:
				msg_format("You cast an unknown Miracle: %d.", spell);
				msg_print(NULL);
			}
			break;

		case 1: /* * SORCERY * */
			switch (spell)
			{
			case 0: /* Detect Monsters */
				(void)detect_monsters_normal();
				break;
			case 1: /* Phase Door */
				teleport_player(10);
				break;
			case 2: /* Detect Doors and Traps */
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			case 3: /* Light Area */
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			case 4: /* Confuse Monster */
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, ( plev * 3) / 2 );
				break;
			case 5: /* Teleport Self */
				teleport_player(plev * 5);
				break;
			case 6: /* Sleep Monster */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			case 7: /* Recharging */
				(void)recharge(plev * 2);
				break;
			case 8: /* Magic Mapping */
				map_area();
				break;
			case 9: /* Identify */
				(void)ident_spell();
				break;
			case 10: /* Slow Monster */
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			case 11: /* Mass Sleep */
				(void)sleep_monsters();
				break;
			case 12: /* Teleport Away */
				if (!get_aim_dir(&dir)) return;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
				break;
			case 13: /* Haste Self */
				if (!p_ptr->fast)
				{
					(void)set_timed_effect( TIMED_FAST, randint(20 + (plev) ) + plev);
				}
				else
				{
					(void)set_timed_effect( TIMED_FAST, p_ptr->fast + randint(5));
				}
				break;
			case 14: /* Detection True */
				(void)detect_all();
				break;
			case 15: /* Identify True */
				identify_fully();
				break;
			case 16: /* Detect Objects and Treasure*/
				(void)detect_objects_normal();
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			case 17: /* Detect Enchantment */
				(void)detect_objects_magic(FALSE,FALSE);
				break;
			case 18: /* Charm Monster */
				if (!get_aim_dir(&dir)) return;
				(void) charm_monster(dir, plev);
				break;
			case 19: /* Dimension Door */
				{
					msg_print("You open a dimensional gate. ");
					if (!spell_dimensional_gate( plev )) return;
					break;
				}

			case 20: /* Sense Minds */
				(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
				break;
			case 21: /* Self knowledge */
				(void)self_knowledge();
				break;
			case 22: /* Teleport Level */
				(void)teleport_player_level();
				break;
			case 23: /* Word of Recall */
				{
					do_recall( NULL );
					break;
				}
			case 24: /* Stasis */
				if (!get_aim_dir(&dir)) return;
				(void)stasis_monster(dir);
				break;
			case 25: /* Telekinesis */
				if (!get_aim_dir(&dir)) return;
				fetch(dir, plev*15, FALSE);
				break;
			case 26: /* Recharging True -- replaced by Explosive Rune */
				explosive_rune();
				break;
			case 27: /* Clairvoyance */
				wiz_lite();
				if (!(p_ptr->telepathy))
				{
					(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
				}
				break;
			case 28: /* Enchant Weapon */
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			case 29: /* Enchant Armour */
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			case 30: /* Alchemy */
				(void) alchemy();
				break;
			case 31: /* Globe of Invulnerability */
				(void)set_timed_effect( TIMED_INVULN, p_ptr->invuln + randint(8) + 8);
				break;
			default:
				msg_format("You cast an unknown Sorcery spell: %d.", spell);
				msg_print(NULL);
			}
			break;
		case 2: /* * NATURE * */
			switch (spell)
			{
			case 0: /* Detect Creatures */
				(void)detect_monsters_normal();
				break;
			case 1: /* First Aid */
				(void)hp_player(damroll(2, 8));
				(void)set_timed_effect( TIMED_CUT, p_ptr->cut - 15);
				break;
			case 2: /* Detect Doors + Traps */
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			case 3: /* Produce Food */
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			case 4: /* Daylight */
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				/*some races do not like light very much*/
				if ((rp_ptr->hates_light) && !(p_ptr->resist_lite))
				{
					msg_print("The daylight scorches your flesh!");
					take_hit(damroll(2,2), "daylight");
				}
				break;
			case 5: /* Animal Taming */
				if (!get_aim_dir(&dir)) return;
				(void) charm_animal(dir, plev);
				break;
			case 6: /* Resist Environment */
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(20) + 20);
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(20) + 20);
				(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(20) + 20);
				break;
			case 7: /* Cure Wounds + Poison */
				(void)set_timed_effect( TIMED_CUT, 0);
				(void)set_timed_effect( TIMED_POISONED , 0);
				break;
			case 8: /* Stone to Mud */
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			case 9: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					damroll(3+((plev-5)/4), 8));
				break;
			case 10: /* Nature Awareness -- downgraded */
				map_area();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				(void)detect_monsters_normal();
				break;
			case 11: /* Frost Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
					damroll(5+((plev-5)/4), 8));
				break;
			case 12: /* Ray of Sunlight */
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of bleak sunlight appears.");
				lite_line(dir);
				break;
			case 13: /* Entangle */
				slow_monsters();
				break;
			case 14: /* Summon Animals */
				if (!(summon_specific_friendly(py, px, plev, FILTER_ANIMAL_RANGER, TRUE)))
					none_came = TRUE;
				break;
			case 15: /* Herbal Healing */
				(void)hp_player(1000);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				(void)set_timed_effect( TIMED_POISONED , 0);
				break;
			case 16: /* Door Building */
				(void)door_creation();
				break;
			case 17: /* Stair Building */
				(void)stair_creation();
				break;
			case 18: /* Stone Skin */
				(void)set_timed_effect( TIMED_SHIELD, p_ptr->shield + randint(20) + 30);
				break;
			case 19: /* Resistance True */
				(void) spell_basic_resistance( 20 );
				break;
			case 20: /* Animal Friendship */
				(void) charm_animals(plev * 2);
				break;
			case 21: /* Stone Tell */
				identify_fully();
				break;
			case 22: /* Wall of Stone */
				(void)wall_stone();
				break;
			case 23: /* Protection from Corrosion */
				rustproof();
				break;
			case 24: /* Earthquake */
				earthquake(py, px, 10);
				break;
			case 25: /* Whirlwind Attack */
				{
					int y = 0, x = 0;
					cave_type       *c_ptr;
					monster_type    *m_ptr;

					for (dir = 0; dir <= 9; dir++) {
						y = py + ddy[dir];
						x = px + ddx[dir];
						c_ptr = &cave[y][x];

						/* Get the monster */
						m_ptr = &m_list[c_ptr->m_idx];

						/* Hack -- attack monsters */
						if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
							py_attack(y, x);
					}
				}
				break;
			case 26: /* Blizzard */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,	70 + (plev), (plev/12)+1);
				break;
			case 27: /* Lightning Storm */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir,	90 + (plev), (plev/12)+1);
				break;
			case 28: /* Whirlpool */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_WATER, dir,100 + (plev), (plev/12)+1);
				break;
			case 29: /* Call Sunlight */

				fire_ball(GF_LITE, 0, 150, 8);
				wiz_lite();
				if ((rp_ptr->hates_light) && !(p_ptr->resist_lite))
				{
					msg_print("The sunlight scorches your flesh!");
					take_hit(50, "sunlight");
				}
				break;
			case 30: /* Elemental Brand */
				brand_weapon(0);
				break;
			case 31: /* Nature's Wrath */
				(void)dispel_monsters(plev * 4);
				earthquake(py, px, 20 + (plev / 2) );
				project(0, 1+plev/12, py, px,
					100+plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM);
				break;
			default:
				msg_format("You cast an unknown Nature spell: %d.", spell);
				msg_print(NULL);
			}
			if (none_came)
				msg_print("No animals arrive.");
			break;

		case 3: /* * CHAOS * */
			switch (spell)
			{
			case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
				break;
			case 1: /* Trap / Door destruction, was: Blink */
				(void)destroy_doors_touch();
				break;
			case 2: /* Flash of Light == Light Area */
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			case 3: /* Touch of Confusion */
				if (!(p_ptr->confusing))
				{
					msg_print("Your hands start glowing.");
					p_ptr->confusing = TRUE;
				}
				break;
			case 4: /* Manaburst */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MISSILE, dir,
					(damroll(3, 5) + plev +
					(plev / (((p_ptr->pclass == CLASS_MAGE)
				       	   || (p_ptr->pclass == CLASS_HIGH_MAGE)
						   || (p_ptr->pclass == CLASS_BLOOD_MAGE)) ? 2 : 4))),
					((plev < 30) ? 2 : 3));
				/* Shouldn't actually use GF_MANA, as it will destroy all
				* items on the floor */
				break;
			case 5: /* Fire Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
					damroll(8+((plev-5)/4), 8));
				break;
			case 6: /* Fist of Force ("Fist of Fun") */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DISINTEGRATE, dir,
					damroll(8+((plev-5)/4), 8), 0);
				break;
			case 7: /* Teleport Self */
				teleport_player(plev * 5);
				break;
			case 8: /* Wonder */
				{
					/* This spell should become more useful (more
					controlled) as the player gains experience levels.
					Thus, add 1/5 of the player's level to the die roll.
					This eliminates the worst effects later on, while
					keeping the results quite random.  It also allows
					some potent effects only at high level. */

					int die = randint(100) + plev / 5;

					if (!get_aim_dir(&dir)) return;
					if (die > 100)
						msg_print ("You feel a surge of power!");
					if (die < 8) clone_monster (dir);
					else if (die < 14) speed_monster (dir);
					else if (die < 26) heal_monster (dir);
					else if (die < 31) poly_monster (dir);
					else if (die < 36)
						fire_bolt_or_beam (beam - 10,
						GF_MISSILE, dir,
						damroll(3 + ((plev - 1) / 5), 4));
					else if (die < 41) confuse_monster (dir, plev);
					else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
					else if (die < 51) lite_line (dir);
					else if (die < 56)
						fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
						damroll(3+((plev-5)/4),8));
					else if (die < 61)
						fire_bolt_or_beam (beam - 10, GF_COLD, dir,
						damroll(5+((plev-5)/4),8));
					else if (die < 66)
						fire_bolt_or_beam (beam, GF_ACID, dir,
						damroll(6+((plev-5)/4),8));
					else if (die < 71)
						fire_bolt_or_beam (beam, GF_FIRE, dir,
						damroll(8+((plev-5)/4),8));
					else if (die < 76) drain_life (dir, 75);
					else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
					else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev, 2);
					else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev, 3);
					else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev, 3);
					else if (die < 101) drain_life (dir, 100 + plev);
					else if (die < 104) earthquake (py, px, 12);
					else if (die < 106) destroy_area (py, px, 15, TRUE);
					else if (die < 108) genocide(TRUE);
					else if (die < 110) dispel_monsters (120);
					else /* RARE */
					{
						dispel_monsters (150);
						slow_monsters();
						sleep_monsters();
						hp_player (300);
					}
					break;
				}
				break;
			case 9: /* Chaos Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_CHAOS, dir,
					damroll(10+((plev-5)/4), 8));
				break;
			case 10: /* Sonic Boom */
				project(0, 2+plev/10, py, px,
					45+plev, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
				break;
			case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8));
				break;
			case 12: /* Fire Ball */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
					55 + (plev), 2);
				break;
			case 13: /* Teleport Other */
				if (!get_aim_dir(&dir)) return;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
				break;
			case 14: /* Word of Destruction */
				destroy_area(py, px, 15, TRUE);
				break;
			case 15: /* Invoke chaos */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS, dir,
					66 + (plev), (plev / 5));
				break;
			case 16: /* Polymorph Other */
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			case 17: /* Chain Lightning */
				for (dir = 0; dir <= 9; dir++)
					fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8));
				break;
			case 18: /* Arcane Binding == Charging */
				(void)recharge(40);
				break;
			case 19: /* Disintegration */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DISINTEGRATE, dir,
					80 + (plev), 3 + (plev/40));
				break;
				break;
			case 20: /* Alter Reality */
				msg_print("The world changes!");
				if (autosave_l)
				{
					is_autosave = TRUE;
					msg_print("Autosaving the game...");
					do_cmd_save_game();
					is_autosave = FALSE;
				}
				new_level_flag = TRUE;
				came_from=START_RANDOM;
				break;
			case 21: /* Polymorph Self */
				do_poly_self();
				break;
			case 22: /* Chaos Branding */
				brand_weapon(1);
				break;
			case 23: /* Summon monster, demon */
				if (randint(3) == 1)
				{
					if (summon_specific(py, px, (plev*3)/2, FILTER_DEMON))
					{
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
					}
				}
				else
				{
					if (summon_specific_friendly((int)py,(int) px, (plev*3)/2,
						FILTER_DEMON, (bool)(plev == 50 ? TRUE : FALSE)))
					{
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'What is thy bidding... Master?'");
					}
				}
				break;
			case 24: /* Beam of Gravity */
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8));
				break;
			case 25: /* Meteor Swarm  */
				{
					int x, y, dx, dy, d, count = 0;
					int b = 10 + randint(10);
					for (i = 0; i < b; i++) {
						do {
							count++;
							if (count > 1000)  break;
							x = px - 5 + randint(10);
							y = py - 5 + randint(10);
							dx = (px > x) ? (px - x) : (x - px);
							dy = (py > y) ? (py - y) : (y - py);
							/* Approximate distance */
							d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
						} while ((d > 5) || (!(player_has_los_bold(y, x))));

						if (count > 1000)   break;
						count = 0;
						project(0, 2, y, x, (plev*3)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
					}
				}
				break;
			case 26: /* Flame Strike */
				fire_ball(GF_FIRE, 0, 150 + (2*plev), 8);
				break;
			case 27: /* Call Primal Chaos */
				call_chaos();
				break;
			case 28: /* Shard Ball */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SHARD, dir, 120 + (plev), 2);
				break;
			case 29: /* Mana Storm */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,	300 + (plev * 2), 4);
				break;
			case 30: /* Breathe chaos */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_CHAOS,dir,p_ptr->chp,2);
				break;
			case 31: /* Call the Void */
				call_the_();
				break;
			default:
				msg_format("You cast an unknown demonic spell: %d.", spell);
				msg_print(NULL);
			}
			break;

		case 4: /* * DEATH * */
			switch (spell)
			{
			case 0: /* Detect Undead & Demons -> Unlife*/
				(void) detect_monsters_nonliving();
				break;
			case 1: /* Malediction */
				if (!get_aim_dir(&dir)) return;
				/* A radius-0 ball may (1) be aimed at objects etc.,
				* and will affect them; (2) may be aimed at ANY
				* visible monster, unlike a 'bolt' which must travel
				* to the monster. */

				fire_ball(GF_HELL_FIRE, dir,
					damroll(3 + ((plev - 1) / 5), 3), 0);
				if (randint(5)==1) {   /* Special effect first */
					dummy = randint(1000);
					if (dummy == 666)
						fire_bolt(GF_DEATH_RAY, dir, plev);
					else if (dummy < 500)
						fire_bolt(GF_TURN_ALL, dir, plev);
					else if (dummy < 800)
						fire_bolt(GF_OLD_CONF, dir, plev);
					else
						fire_bolt(GF_STUN, dir, plev);
				}
				break;
			case 2: /* Detect Evil */
				(void)detect_monsters_evil();
				break;
			case 3: /* Stinking Cloud */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,	10 + (plev / 2), 2);
				break;
			case 4: /* Black Sleep */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			case 5: /* Resist Poison */
				(void)set_timed_effect( TIMED_OPPOSE_POIS, p_ptr->oppose_pois + randint(20) + 20);
				break;
			case 6: /* Horrify */
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				(void) stun_monster(dir, plev);
				break;
			case 7: /* Enslave the Undead */
				if (!get_aim_dir(&dir)) return;
				(void)control_one_undead(dir, plev);
				break;
			case 8: /* Orb of Entropy */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_OLD_DRAIN, dir,
					(damroll(3, 6) + plev +
					(plev / (((p_ptr->pclass == CLASS_MAGE)
					       || (p_ptr->pclass == CLASS_HIGH_MAGE)
						   || (p_ptr->pclass == CLASS_BLOOD_MAGE)) ? 2 : 4))),
					((plev < 30) ? 2 : 3));
				break;
			case 9: /* Nether Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_NETHER, dir,
					damroll(6+((plev-5)/4), 8));
				break;
			case 10: /* Terror */
				turn_monsters(30+plev);
				break;
			case 11: /* Vampiric Drain */
				if (!get_aim_dir(&dir)) return;
				dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
				if (drain_life(dir, dummy)) {
					(void)hp_player(dummy);
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
						(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
				}
				break;
			case 12: /* Poison Branding */
				brand_weapon(2);
				break;
			case 13: /* Dispel Good */
				(void)dispel_good(plev * 4);
				break;
			case 14: /* Genocide */
				(void)genocide(TRUE);
				break;
			case 15: /* Restore Life */
				(void)restore_level();
				break;
			case 16: /* Berserk */
				(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(25) + 25);
				(void)hp_player(30);
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 17: /* Invoke Spirits */
				{
					int die = randint(100) + plev / 5;
					if (!get_aim_dir(&dir)) return;

					msg_print("You call on the power of the dead...");
					if (die > 100)
						msg_print ("You feel a surge of eldritch force!");

					if (die < 8) {
						msg_print("Oh no! Mouldering forms rise from the earth around you!");
						(void) summon_specific(py, px, dun_level, FILTER_UNDEAD);
					} else if (die < 14) {
						msg_print("An unnamable evil brushes against your mind...");
						set_timed_effect( TIMED_AFRAID , p_ptr->afraid + randint(4) + 4);
					} else if (die < 26) {
						msg_print("Your head is invaded by a horde of gibbering spectral voices...");
						set_timed_effect( TIMED_CONFUSED , p_ptr->confused + randint(4) + 4);
					} else if (die < 31) {
						poly_monster (dir);
					} else if (die < 36) {
						fire_bolt_or_beam (beam - 10,
							GF_MISSILE, dir,
							damroll(3 + ((plev - 1) / 5), 4));
					} else if (die < 41) {
						confuse_monster (dir, plev);
					} else if (die < 46) {
						fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
					} else if (die < 51) {
						lite_line (dir);
					} else if (die < 56) {
						fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
							damroll(3+((plev-5)/4),8));
					} else if (die < 61) {
						fire_bolt_or_beam (beam - 10, GF_COLD, dir,
							damroll(5+((plev-5)/4),8));
					} else if (die < 66) {
						fire_bolt_or_beam (beam, GF_ACID, dir,
							damroll(6+((plev-5)/4),8));
					} else if (die < 71) {
						fire_bolt_or_beam (beam, GF_FIRE, dir,
							damroll(8+((plev-5)/4),8));
					} else if (die < 76) {
						drain_life (dir, 75);
					} else if (die < 81) {
						fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
					} else if (die < 86) {
						fire_ball (GF_ACID, dir, 40 + plev, 2);
					} else if (die < 91) {
						fire_ball (GF_ICE, dir, 70 + plev, 3);
					} else if (die < 96) {
						fire_ball (GF_FIRE, dir, 80 + plev, 3);
					} else if (die < 101) {
						drain_life (dir, 100 + plev);
					} else if (die < 104) {
						earthquake (py, px, 12);
					} else if (die < 106) {
						destroy_area (py, px, 15, TRUE);
					} else if (die < 108) {
						genocide(TRUE);
					} else if (die < 110) {
						dispel_monsters (120);
					} else { /* RARE */
						dispel_monsters (150);
						slow_monsters();
						sleep_monsters();
						hp_player (300);
					}

					if (die < 31)
						msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
					break;
				}
			case 18: /* Dark Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_DARK, dir, damroll(4+((plev-5)/4), 8));
				break;
			case 19: /* Battle Frenzy */
				(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(25) + 25);
				(void)hp_player(30);
				(void)set_timed_effect( TIMED_AFRAID , 0);
				if (!p_ptr->fast)
				{
					(void)set_timed_effect( TIMED_FAST, randint(20 + (plev / 2) ) + (plev / 2));
				}
				else
				{
					(void)set_timed_effect( TIMED_FAST, p_ptr->fast + randint(5));
				}
				break;
			case 20: /* Vampirism True */
				if (!get_aim_dir(&dir)) return;
				for (dummy = 0; dummy < 3; dummy++)
				{
					if (drain_life(dir, 100))
						hp_player(100);
				}
				break;
			case 21: /* Vampiric Branding */
				brand_weapon(3);
				break;
			case 22: /* Darkness Storm */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir,	120, 4);
				break;
			case 23: /* Mass Genocide */
				(void)mass_genocide(TRUE);
				break;
			case 24: /* Death Ray */
				if (!get_aim_dir(&dir)) return;
				(void)death_ray(dir, plev);
				break;
			case 25: /* Raise the Dead */
				if (randint(3) == 1) {
					if (summon_specific(py, px, (plev*3)/2,
						(plev > 47 ? FILTER_HI_UNDEAD : FILTER_UNDEAD))) {
							msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
							msg_print("'The dead arise... to punish you for disturbing them!'");
						}
				} else {
					if (summon_specific_friendly((int)py,(int)px, (plev*3)/2,
						(plev > 47 ? FILTER_HI_UNDEAD_NO_UNIQUES : FILTER_UNDEAD),
						(bool)(((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE))) {
							msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
							msg_print("Ancient, long-dead forms arise from the ground to serve you!");
						}
				}
				break;
			case 26: /* Esoteria */
				if (randint(50)>plev)
					(void) ident_spell();
				else
					identify_fully();
				break;
			case 27: /* Word of Death */
				(void)dispel_living(plev * 3);
				break;
			case 28: /* Evocation       */
				(void)dispel_monsters(plev * 4);
				turn_monsters(plev*4);
				banish_monsters(plev*4);
				break;
			case 29: /* Hellfire */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HELL_FIRE, dir,
					666, 3);
				take_hit(50+randint(50), "the strain of casting Hellfire");
				break;
			case 30: /* Omnicide */
				p_ptr->csp -= 100;  /* Display doesn't show mana cost (100)
									* as deleted until the spell has finished. This gives a
									* false impression of how high your mana is climbing.
									* Therefore, 'deduct' the cost temporarily before entering the
									* loop, then add it back at the end so that the rest of the
									* program can deduct it properly */
				for (i = 1; i < m_max; i++)
				{
					monster_type    *m_ptr = &m_list[i];
					monster_race    *r_ptr = &r_info[m_ptr->r_idx];

					/* Paranoia -- Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Hack -- Skip Unique Monsters */
					if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

					/* Skip Quest Monsters */
					if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) continue;

					/* Delete the monster */
					delete_monster_idx(i,TRUE);

					/* Take damage */
					take_hit(randint(4), "the strain of casting Omnicide");

					/* Absorb power of dead soul */
					p_ptr->csp++;

					/* Visual feedback */
					move_cursor_relative(py, px);

					/* Redraw */
					p_ptr->redraw |= (PR_HP | PR_MANA);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER);
					p_ptr->window |=(PW_SPELL);

					/* Handle */
					handle_stuff();

					/* Fresh */
					Term_fresh();

					/* Delay */
					Term_xtra(TERM_XTRA_DELAY,
						delay_factor * delay_factor * delay_factor);
				}
				p_ptr->csp += 100;   /* Restore, ready to be deducted properly */

				break;
			case 31: /* Wraithform */
				set_timed_effect( TIMED_WRAITH_FORM, p_ptr->wraith_form + randint(plev/2) + (plev/2));
				break;
			default:
				msg_format("You cast an unknown Death spell: %d.", spell);
				msg_print(NULL);
			}
			break;

		case 5: /* TAROT */
			switch (spell)
			{
			case 0: /* Shift */
				teleport_player(10);
				break;
			case 1: /* The Challenge */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_PSI, dir,
					damroll(3 + ((plev - 1) / 5), 3));
				break;
			case 2: /* Fears & Hopes */

				{
					/* A limited power 'wonder' spell */

					int die = randint(120);

					if ((p_ptr->pclass == CLASS_ROGUE) ||
						(p_ptr->pclass == CLASS_HIGH_MAGE))
						die = (randint(110)) + plev / 5;
					/* Card sharks and high mages get a level bonus */

					msg_print("You shuffle your Tarot deck and draw a card...");

					if (die < 7 )
					{
						msg_print("Oh no! It's the Blasted Tower!");
						for (dummy = 0; dummy < randint(3); dummy++)
							(void)activate_hi_summon();
					}
					else if (die < 14)
					{
						msg_print("Oh no! It's the Devil!");
						(void) summon_specific(py, px, dun_level, FILTER_DEMON);
					}
					else if (die < 18 )
					{
						msg_print("Oh no! It's the Hanged Man.");
						activate_ty_curse();
					}
					else if (die < 22 )
					{
						msg_print("It's the swords of discord.");
						aggravate_monsters(1);
					}
					else if (die < 26)
					{
						msg_print("It's the Fool.");
						(void) do_dec_stat(A_INT);
						(void) do_dec_stat(A_WIS);
					}
					else if (die < 30)
					{
						msg_print("It's a picture of a strange monster.");
						if (!(summon_specific(py, px, (dun_level * 3) / 2, 32 + randint(6))))
							none_came = TRUE;
					}
					else if (die < 33)
					{
						msg_print("It's the Moon.");
						unlite_area(10,3);
					}
					else if (die < 38)
					{
						msg_print("It's the Wheel of Fortune.");
						wild_magic((randint(32))-1);
					}
					else if (die < 40)
					{
						msg_print("It's a teleport card.");
						teleport_player(10);
					}
					else if (die <42)
					{
						msg_print("It's the Star.");
						set_timed_effect( TIMED_BLESSED, p_ptr->blessed + p_ptr->lev);
					}
					else if (die <47)
					{
						msg_print("It's a teleport card.");
						teleport_player(100);
					}
					else if (die <52)
					{
						msg_print("It's a teleport card.");
						teleport_player(200);
					}
					else if (die <60)
					{
						msg_print("It's the Tower.");
						wall_breaker();
					}
					else if (die <72)
					{
						msg_print("It's Temperance.");
						sleep_monsters_touch();
					}
					else if (die <80)
					{
						msg_print("It's the Tower.");
						earthquake(py, px, 5);
					}
					else if (die<82)
					{
						msg_print("It's a picture of a friendly monster.");
						if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, FILTER_BIZARRE1, FALSE)))
							none_came = TRUE;
					}
					else if (die<84)
					{
						msg_print("It's a picture of a friendly monster.");
						if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, FILTER_BIZARRE2, FALSE)))
							none_came = TRUE;
					}
					else if (die<86)
					{
						msg_print("It's a picture of a friendly monster.");
						if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, FILTER_BIZARRE4, FALSE)))
							none_came = TRUE;
					}
					else if (die<88)
					{
						msg_print("It's a picture of a friendly monster.");
						if (!(summon_specific_friendly(py, px, (dun_level * 3) / 2, FILTER_BIZARRE5, FALSE)))
							none_came = TRUE;
					}
					else if (die<96)
					{
						msg_print("It's the Lovers.");
						if (!get_aim_dir(&dir)) return;
						(void) charm_monster(dir, MIN(p_ptr->lev, 20));
					}
					else if (die<101)
					{
						msg_print("It's the Hermit.");
						wall_stone();
					}
					else if (die< 111)
					{
						msg_print("It's the Judgement.");
						do_cmd_rerate();
						if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
						{
							msg_print("You are cured of all corruptions.");
							p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
							p_ptr->update |= PU_BONUS;
							handle_stuff();
						}

					}
					else if (die < 120)
					{
						msg_print("It's the Sun.");
						wiz_lite();
					}
					else
					{
						msg_print("It's the World.");
						if (p_ptr->exp < PY_MAX_EXP)
						{
							s32b ee = (p_ptr->exp / 25) + 1;
							if (ee > 5000) ee = 5000;
							msg_print("You feel more experienced.");
							gain_exp(ee);
						}
					}

				}
				break;
			case 3: /* Restack */
				{
					/* Prompt */
					sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_dun_level);

					/* Default */
					sprintf(tmp_val, "%d", MAX(dun_level,1));

					/* Ask for a level */
					if (!get_string(ppp, tmp_val, 10)) return;

					/* Extract request */
					dummy = atoi(tmp_val);

					/* Paranoia */
					if (dummy < 1) dummy = 1;

					/* Paranoia */
					if (dummy > p_ptr->max_dun_level) dummy = p_ptr->max_dun_level;

					/*Actually set the new depth*/
					p_ptr->max_dun_level= dummy;

					/* Accept request */
					msg_format("Recall depth set to level %d (%d').", p_ptr->max_dun_level, p_ptr->max_dun_level * 50 );
				}
				break;
			case 4: /* Fool's Journey */
				teleport_player(plev * 4);
				break;
			case 5: /* Sleight of Hand */
				{
					msg_print("You open a dimensional gate. ");
					if (!spell_dimensional_gate( plev )) return;
					break;
				}
			case 6: /* The High Priestess"*/
				(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
				break;
			case 7: /* The Chariot */
				if (!get_aim_dir(&dir)) return;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
				break;
			case 8: /* Wheel of Fortune */
				if (!get_aim_dir(&dir)) return;
				fetch(dir, plev*15, TRUE);
				break;
			case 9: /* Temperance */
				{
					(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(20) + 20);
					(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(20) + 20);
					(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(20) + 20);
				}
					break;
			case 10: /* King of Swords */
				if (summon_specific_friendly(py, px, (plev*3)/2, FILTER_DEMON, FALSE))
					msg_print ("'Your wish, master?'");
				else
					none_came = TRUE;
				break;
			case 11: /* The lovers */
				{
					if (!get_aim_dir(&dir)) return;
					(void)charm_monster_type( dir, MIN(p_ptr->lev, 20), FILTER_HUMAN);
				}
				break;
			case 12: /* Elements of The Minchiate */
				{
					if (!(summon_specific_friendly(py, px, plev, FILTER_ELEMENTAL, FALSE)))
					none_came = TRUE;
				}
				break;
			case 13: /* The Hermit */
				msg_print("You withdraw yourself.");
				(void)teleport_player_level();
				break;
			case 14: /* Search for the Self */
				{
					do_recall( NULL );
					break;
				}
			case 15: /* Shuffle */
				banish_monsters(plev*4);
				break;
			case 16: /* Ink Blot */
				msg_print("You concentrate on a joker card...");
				switch(randint(4))
				{
					case 1: dummy = FILTER_BIZARRE1; break;
					case 2: dummy = FILTER_BIZARRE2; break;
					case 3: dummy = FILTER_BIZARRE4; break;
					case 4: dummy = FILTER_BIZARRE5; break;

				}
					if (randint(2)==1)
					{
						if (summon_specific(py, px, plev, dummy))
							msg_print("The summoned creature gets angry!");
						else
							none_came = TRUE;
					}
					else
					{
						if (!(summon_specific_friendly(py, px, plev, dummy, FALSE)))
							none_came = TRUE;
					}
					break;
			case 17: /* The Star */
				{
					(void)hp_player(150);
					(void)set_timed_effect( TIMED_STUN, 0);
					(void)set_timed_effect( TIMED_CUT, 0);
					break;
				}
				break;
			case 18: /* Fortitude */
				{
					(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + plev);
					(void)set_timed_effect( TIMED_HERO, p_ptr->hero + plev);
					(void)set_timed_effect( TIMED_FAST, p_ptr->hero + plev);
				}
				break;
			case 19: /* The Emperor */
				{
					charm_monsters(p_ptr->lev * 2);
					break;
				}
				break;
			case 20: /* Branding of the Minchiate */
				brand_weapon(5);
				break;
			case 21: /* Tarot Ascension */
				if ( gain_corruption(8) || gain_corruption(16) )
					msg_print("You have turned into a Planar Being.");
				break;
			case 22: /* Death */
				(void)dispel_living(plev * 3);
				break;
			case 23: /* The Devil */
				{
					msg_print ("You concentrate on an image of The Devil...");
					if (randint(10)>3)
					{
						if (!(summon_specific_friendly(py, px, plev, FILTER_DEVIL, FALSE)))
							none_came = TRUE;
					}
					else
					{
						if (summon_specific(py, px, plev, FILTER_DEVIL))
						{
							msg_print("The summoned Black Reaver gets angry!");
						}
						else
						{
							none_came = TRUE;
						}
					}
				}
				break;
			case 24: /* Read The Lay */
				(void)detect_all();
				break;
			case 25: /* Tarot Lore */
				identify_fully();
				break;
			case 26: /* Patter */
				{
					fire_ball(GF_CONFUSION  , 5, 200, 5);
					teleport_player(10);
				}
				break;
			case 27: /* The Tower */
				{
					fire_ball(GF_BLASTED_TOWER , 5, 200, 7);
				}
				break;
			case 28: /* Lay of The Celtic Cross */
				{
					none_came = TRUE;
					msg_print ("You concentrate on several images at once...");
					for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
					{
						if (randint(10)>3)
						{
							if (summon_specific_friendly(py, px, plev, FILTER_NO_UNIQUES, FALSE))
								none_came = FALSE;
						}
						else
						{
							if (summon_specific(py, px, plev, 0))
							{
								msg_print("A summoned creature gets angry!");
								none_came = FALSE;
							}
						}
					}
				}
				break;
			case 29: /* Ten of Pentacles */
				{
					msg_print ("You concentrate on the image of a demon...");
					if (randint(10)>3)
					{
						if (!(summon_specific_friendly(py, px, plev, FILTER_DEMON, FALSE)))
							none_came = TRUE;
					}
					else
					{
						if (summon_specific(py, px, plev, FILTER_DEMON))
						{
							msg_print("The summoned demon gets angry!");
						}
						else
						{
							none_came = TRUE;
						}
					}
				}
				break;
			case 30: /* The Traitor */
			{

				none_came = !summon_specific_potential_ally( FILTER_FALLEN_ANGELS, 3, 10, "The Fallen Angel gets angry!");
				break;
			}
			case 31: /* Justice */
				{
					(void)dispel_demons(plev * 3);
					(void)dispel_devils(plev * 3);
					(void)dispel_fallen_angels(plev * 3);
				}
				break;
			default:
				msg_format("You cast an unknown Tarot spell: %d.", spell);
				msg_print(NULL);
			}
			if (none_came)
			{
				msg_print("Nobody answers to your call.");
			}
			break;
		case 6: /* Charms */
			switch (spell)
			{
			case 0: /* Zap */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
					damroll(3 + ((plev - 1) / 5), 3));
				break;
			case 1: /* Wizard Lock */
				if (!(get_aim_dir(&dir))) break;
				(void) wizard_lock(dir);
				break;
			case 2: /* Detect Invisibility */
				(void)detect_monsters_invis();
				break;
			case 3: /* Detect Monsters */
				(void)detect_monsters_normal();
				break;
			case 4: /* Blink */
				teleport_player(10);
				break;
			case 5: /* Light Area */
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			case 6: /* Trap & Door Destruction */
				if (!(get_aim_dir(&dir))) return;
				(void) destroy_door(dir);
				break;
			case 7: /* Cure Light Wounds */
				(void) hp_player(damroll(2, 8));
				(void) set_timed_effect( TIMED_CUT, p_ptr->cut - 10);
				break;
			case 8: /* Detect Doors & Traps */
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			case 9: /* Phlogiston */
				phlogiston();
				break;
			case 10: /* Detect Treasure */
				(void)detect_treasure();
				(void)detect_objects_gold();

				break;
			case 11: /* Detect Enchantment */
				(void)detect_objects_magic(FALSE,FALSE);
				break;
			case 12: /* Detect Object */
				(void)detect_objects_normal();
				break;
			case 13: /* Cure Poison */
				(void)set_timed_effect( TIMED_POISONED , 0);
				break;
			case 14: /* Resist Cold */
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(20) + 20);
				break;
			case 15: /* Resist Fire */
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(20) + 20);
				break;
			case 16: /* Resist Lightning */
				(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(20) + 20);
				break;
			case 17: /* Resist Acid */
				(void)set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(20) + 20);
				break;
			case 18: /* Cure Medium Wounds */
				(void)hp_player(damroll(4, 8));
				(void)set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 50);
				break;
			case 19: /* Teleport */
				teleport_player(plev * 5);
				break;
			case 20: /* Stone to Mud */
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			case 21: /* Ray of Light */
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of light appears.");
				lite_line(dir);
				break;
			case 22: /* Satisfy Hunger */
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			case 23: /* See Invisible */
				(void)set_timed_effect( TIMED_TIM_INVIS, p_ptr->tim_invis + randint(24) + 24);
				break;
			case 24: /* Recharging */
				(void)recharge(plev * 2);
				break;
			case 25: /* Teleport Level */
				(void)teleport_player_level();
				break;
			case 26: /* Identify */
				(void)ident_spell();
				break;
			case 27: /* Teleport Away */
				if (!get_aim_dir(&dir)) return;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
				break;
			case 28: /* Elemental Ball */
				if (!get_aim_dir(&dir)) return;
				switch (randint(4))
				{
				case 1: dummy = GF_FIRE;
				case 2: dummy = GF_ELEC;
				case 3: dummy = GF_COLD;
				default: dummy = GF_ACID;
				}
				fire_ball(dummy, dir,
					75 + (plev), 2);
				break;
			case 29: /* Detection */
				(void)detect_all();
				break;
			case 30: /* Word of Recall */
				{
					do_recall( NULL );
					break;
				}
			case 31: /* Clairvoyance */
				wiz_lite();
				if (!(p_ptr->telepathy))
				{
					(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
				}
				break;
			default:
				msg_format("You cast an unknown Charm: %d.", spell);
				msg_print(NULL);
			}
			break;
		case 7: /* * Somatic * */
			switch (spell)
			{
			case 0: /* Cure Light Wounds */
				(void)hp_player(damroll(2, 10));
				(void)set_timed_effect( TIMED_CUT, p_ptr->cut - 10);
				break;
			case 1: /* Shift */
				teleport_player(10);
				break;
			case 2: /* Bravery */
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 3: /* Bat's Sense */
				map_area();
				break;
			case 4: /* Eagle's Vision */
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			case 5: /* Mind Vision */
				(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
				break;
			case 6: /* Cure Medium Wounds */
				(void)hp_player(damroll(4, 10));
				(void)set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 20);
				break;
			case 7: /* Satisfy Hunger */
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			case 8: /* Burn Resistance */
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(20) + 20);
				(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(20) + 20);
				(void)set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(20) + 20);
				break;
			case 9: /* Detoxify */
				(void)set_timed_effect( TIMED_POISONED , 0);
				break;
			case 10: /* Cure Critical Wounds */
				(void)hp_player(damroll(8, 10));
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 11: /* See Invisible */
				(void)set_timed_effect( TIMED_TIM_INVIS, p_ptr->tim_invis + randint(24) + 24);
				break;
			case 12: /* Teleport */
				teleport_player(plev * 3);
				break;
			case 13: /* Haste */
				if (!p_ptr->fast)
				{
					(void)set_timed_effect( TIMED_FAST, randint(20 + (plev) ) + plev);
				}
				else
				{
					(void)set_timed_effect( TIMED_FAST, p_ptr->fast + randint(5));
				}
				break;
			case 14: /* Healing */
				(void)hp_player(300);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 15: /* Resist True */
				(void)set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(25) + 25);
				(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(25) + 25);
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(25) + 25);
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(25) + 25);
				(void)set_timed_effect( TIMED_OPPOSE_POIS, p_ptr->oppose_pois + randint(25) + 25);
				break;
			case 16: /* Horrific Visage */
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				(void) stun_monster(dir, plev);
				break;
			case 17: /* See Magic */
				(void)detect_objects_magic(FALSE,FALSE);
				break;
			case 18: /* Stone Skin */
				(void)set_timed_effect( TIMED_SHIELD, p_ptr->shield + randint(20) + 30);
				break;
			case 19: /* Move Body */
				msg_print("You focus your Chi. ");
				if (!spell_dimensional_gate( plev )) return;
				break;
			case 20: /* Corrupt Body */
				gain_corruption(0);
				break;
			case 21: /* Know Self */

				(void)self_knowledge();
				break;
			case 22: /* Teleport Level */
				(void)teleport_player_level();
				break;
			case 23: /* Word of Recall */
				{
					do_recall( NULL );
					break;
				}
			case 24: /* Heroism */
				(void)set_timed_effect( TIMED_HERO, p_ptr->hero + randint(25) + 25);
				(void)hp_player(10);
				(void)set_timed_effect( TIMED_AFRAID , 0);
				break;
			case 25: /* Wraithform */
				set_timed_effect( TIMED_WRAITH_FORM, p_ptr->wraith_form + randint(plev/2) + (plev/2));
				break;
			case 26: /* Attune to Magic */
				identify_fully();
				break;
			case 27: /* Restore Body */
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHA);
				break;
			case 28: /* Healing True */
				(void)hp_player(2000);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				break;
			case 29: /* Hypnotic Eyes */
				if (!get_aim_dir(&dir)) return;
				(void) charm_monster(dir, plev);
				break;
			case 30: /* Restore Soul */
				(void)restore_level();
				break;
			case 31: /* Invulnerability */
				(void)set_timed_effect( TIMED_INVULN, p_ptr->invuln + randint(7) + 7);
				break;
			default:
				msg_format("You cast an unknown Somatic spell: %d.", spell);
				msg_print(NULL);
			}
			break;
		case REALM_DEMONIC-1: /* * DEMONIC * */
			switch (spell)
			{
				case 0: /* Unholy strength */
					(void)set_timed_effect( TIMED_HERO, p_ptr->hero + randint(25) + 25);
					(void)take_hit( (p_ptr->lev/10)*5+5 , "Strain of Unholy Strength");
					break;
				case 1: /* Sense Evil */
					(void)detect_monsters_evil();
					break;
				case 2: /* Scorch */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_FIRE, dir, damroll(3 + ((plev - 1) / 5), 4));
					break;
				case 3: /* Perilous Shadows */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_DARK, dir, damroll(3 + ((plev - 1) / 5), 4));
					break;
				case 4: /* Teleport */
					teleport_player(75);
					break;
				case 5: /* Disintegrate  */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DISINTEGRATE, dir, damroll(8+((plev-5)/4), 8), 0);
					break;
				case 6: /* Demonic Sigil */
					msg_print("You carefully draw a sigil on the floor...");
					explosive_rune();
					break;
				case 7: /* Hecate's Radiance ( weak light damage + medium charm/confuse/fear spell ) */
					(void)lite_area_hecate(damroll(plev, 2), (plev / 10) + 1);
					break;
				case 8: /* Abaddon's Rage */
					(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(25) + 25);
					(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(25) + 25);
					(void)set_timed_effect( TIMED_AFRAID , 0);
					break;
				case 9: /* Mind Leech */
					(void)mind_leech();
					break;
				case 10: /* Body Leech*/
					(void)body_leech();
					break;
				case 11: /* Glyph of Warding */
					warding_glyph();
					break;
				case 12: /* Protection from Evil */
					(void)set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
					break;
				case 13: /* Summon Demons */
					if (!(summon_specific_friendly(py, px, plev, FILTER_DEMON, TRUE)))
						if (!(summon_specific_friendly(py, px, plev, FILTER_DEVIL, TRUE)))
							none_came = TRUE;
					break;
				case 14: /* Summon the Fallen */
					if (!(summon_specific_friendly(py, px, plev, FILTER_FALLEN_ANGELS, TRUE)))
						none_came = TRUE;
					break;
				case 15: /* Balm of the Cocytus */
					hp_player(300);
					/* Actually set the stat to its new value. */
					p_ptr->stat_cur[A_CON] = p_ptr->stat_cur[A_CON]-1;
					/* Recalculate bonuses */
					p_ptr->update |= (PU_BONUS);
					break;
				case 16: /* Araqiel's Wrath ( Earthquake )  */
					(void) earthquake(py,px,8);
					break;
				case 17: /*Kokabiel's Call  ( Summon Spirits, lots of them ) */
					none_came = summon_specific_friendly(py, px, plev, FILTER_SPIRITS, TRUE) +
					summon_specific_friendly(py, px, plev, FILTER_SPIRITS, TRUE) +
					summon_specific_friendly(py, px, plev, FILTER_SPIRITS, TRUE) +
					summon_specific_friendly(py, px, plev, FILTER_SPIRITS, TRUE);
					none_came = !none_came;
					break;
				case 18: /* Baraquiel's Guile (detect enchantment on entire level of excellents and specials) */
					(void)detect_objects_magic(TRUE,TRUE);
					break;
				case 19: /* Sariel's Ire  */
					dummy = randint(50) + 25;
					(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + dummy);
					(void)set_timed_effect( TIMED_HERO, p_ptr->hero + dummy);
					(void)set_timed_effect( TIMED_MAGIC_SHELL, p_ptr->magic_shell + dummy);
					break;
				case 20: /* Azazel's Rule */
					(void)charm_all_goats();
					break;
				case 21: /* Danel's Deluge */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, dir,
							  (damroll(3, 6) + plev +
							   (plev / ((p_ptr->pclass == CLASS_HELL_KNIGHT
										 || p_ptr->pclass == CLASS_WARLOCK
										 || p_ptr->pclass == CLASS_BLOOD_MAGE
										 || p_ptr->pclass == CLASS_HIGH_MAGE) ? 1 : 3))),
							  ((plev < 30) ? 2 : 3));
					break;
				case 22: /* Amaros' Grief  */
					(void) dispel_demons(plev*4);
					(void) dispel_fallen_angels(plev*4);
					break;
				case 23: /* Teachings of Kasyade */
					wiz_lite();
					break;
				case 24: /* Orb of Impending Doom */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_HELL_FIRE, dir, (damroll(3, 6) + plev +
												  (plev / (( p_ptr->pclass == CLASS_HELL_KNIGHT || p_ptr->pclass == CLASS_WARLOCK || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4))),
							  ((plev < 30) ? 2 : 3));
					break;
				case 25: /* Temperance */
					dummy = randint(50) + 50;
					(void)set_timed_effect( TIMED_OPPOSE_COLD,  dummy );
					(void)set_timed_effect( TIMED_OPPOSE_FIRE,  dummy );
					break;
				case 26: /* True Warding */
					warding_glyph();
					glyph_creation();
					break;
				case 27: /* Word of Destruction */
					destroy_area(py, px, 15, TRUE);
					break;
				case 28: /* Gift of Malphas ( Adding weapon flags, Malphas being a friend of artificers ] */
					(void)malphas_gift();
					break;
				case 29: /* Lilith's Kiss  */
					charm_monsters(p_ptr->lev * 4);
					break;
				case 30: /* Behemoth's Call */
					msg_print("You open your mouth while it deforms into a gaping hole, spewing wind and water!");
					(void)behemoth_call();
					break;
				case 31: /* Chaos Rift */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS,dir,p_ptr->mhp,2);
					break;
				default:
					msg_format("You cast an unknown Demonic spell: %d.", spell);
					msg_print(NULL);
			}
			break;
		default:
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}
		get_extended_spell_info( realm , spell , s_ptr);
		/* Did we ever cast this spell succesfully */
		if(!s_ptr->worked)
		{
			int spell_skill = mp_ptr->skill[realm]-1;
			first_time = TRUE;
			if( spell_skill == SUPER || spell_skill == BETTER )
				xp_gain = s_ptr->sexp;
			else
				xp_gain = s_ptr->sexp *  s_ptr->sexp;
			if (realm == p_ptr->realm1-1)
				spell_worked1 |= (1L << spell);
			else
				spell_worked2 |= (1L << spell);
		}
	}

	/* Take some time - a spell of your level takes 100, lower level spells take less */
	energy_use = 100-(5*(p_ptr->lev-s_ptr->slevel));
	if(energy_use < 10) energy_use = 10;

	if( p_ptr->pclass != CLASS_BLOOD_MAGE)
	{
		/* Sufficient mana */
		if (s_ptr->smana <= p_ptr->csp)
		{
			/* Use some mana */
			p_ptr->csp -= s_ptr->smana;
		}
		/* Over-exert the player */
		else
		{
			int oops = s_ptr->smana - p_ptr->csp;

			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;

			/* Message */
			msg_print("You faint from the effort!");

			/* Hack -- Bypass free action */
			(void)set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(5 * oops + 1));

			/* Damage CON (possibly permanently) */
			if (rand_int(100) < 50)
			{
				bool perm = (rand_int(100) < 25);

				/* Message */
				msg_print("You have damaged your health!");

				/* Reduce constitution */
				(void)dec_stat(A_CON, 15 + randint(10), perm);
			}
		}
	}
	else
	/*We are dealing with a blood mage*/
	{
		/* Sufficient hitpoints */
		if (s_ptr->smana <= p_ptr->chp)
		{
			/* Use some mana */
			p_ptr->chp -= s_ptr->smana;
		}
		/* Over-exert the player */
		else
		{
			int oops = s_ptr->smana - p_ptr->chp;

			/* Leave one hitpoint */
			p_ptr->chp = 1;
			p_ptr->chp_frac = 0;

			/* Message */
			msg_print("You faint from the effort!");
			/* Hack -- Bypass free action */
			(void)set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + randint(5 * oops + 1));
			/* Whack that constitution like there is no tomorrow */
			(void)dec_stat(A_CON, 25, TRUE);
		}
	}

	/* Redraw mana */
	if( p_ptr->pclass != CLASS_BLOOD_MAGE )
		p_ptr->redraw |= (PR_MANA);
	else
		p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |=(PW_SPELL);

	/*  Gain experience, we put this here since leveling screws up the spell info pointer,
		that is very bad karma and it is all my fault, big todo!
	*/
	if(first_time)
		gain_exp(xp_gain);
}
