/* File: spells1.c */

/* Purpose: Spell code (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16

/* 1/x chance of hurting even if invulnerable!*/
#define PENETRATE_INVULNERABILITY 13

/* Maximum number of tries for teleporting */
#define MAX_TRIES 100

/* Always return FALSE at the moment. Won't delete it since it's technically implemented... */
static bool too_weak(monster_type *m_ptr)
{
	/*monster_race *r_ptr = &r_info[m_ptr->r_idx];*/
	
	/*if (p_ptr->lev > (r_ptr->level * 4)) return (TRUE);*/

	return (FALSE);
}


/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
s16b poly_r_idx(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

        int i, r;

#if 0 /* No more -- hehehe -- DG */
	/* Allowable range of "levels" for resulting monster */
        int lev1 = r_ptr->level - ((randint(20) / randint(9)) + 1);
        int lev2 = r_ptr->level + ((randint(20) / randint(9)) + 1);
#endif

	/* Hack -- Uniques/Questors never polymorph */
	if ((r_ptr->flags1 & RF1_UNIQUE) ||
	    (r_ptr->flags1 & RF1_QUESTOR))
		return (r_idx);

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

#if 0
		/* Ignore monsters with incompatible levels */
		if ((r_ptr->level < lev1) || (r_ptr->level > lev2)) continue;
#endif

		/* Use that index */
		r_idx = r;

		/* Done */
		break;
	}

	/* Result */
	return (r_idx);
}

/*
 * Teleport player, using a distance and a direction as a rough guide.
 *
 * This function is not at all obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_directed(int rad, int dir)
{
  int y = py;
  int x = px;
  int yfoo = ddy[dir];
  int xfoo = ddx[dir];
  int min = rad / 4;
  int dis = rad;
  int i, d;
  bool look = TRUE;
  bool y_major = FALSE;
  bool x_major = FALSE;
  int y_neg = 1;
  int x_neg = 1;
  cave_type *c_ptr;

  if (xfoo == 0 && yfoo == 0) {
    teleport_player(rad);
    return;
  }
  if (yfoo == 0) {
    x_major = TRUE;
  }
  if (xfoo == 0) {
    y_major = TRUE;
  }
  if (yfoo < 0) {
    y_neg = -1;
  }
  if (xfoo < 0) {
    x_neg = -1;
  }
  /* Look until done */
  while (look) {

    /* Verify max distance */
    if (dis > 200) {
      teleport_player(rad);
      return;
    }
    /* Try several locations */
    for (i = 0; i < 500; i++) {

      /* Pick a (possibly illegal) location */
      while (1) {
	if (y_major) {
	  y = rand_spread(py + y_neg * dis / 2, dis / 2);
	} else {
	  y = rand_spread(py, dis / 3);
	}

	if (x_major) {
	  x = rand_spread(px + x_neg * dis / 2, dis / 2);
	} else {
	  x = rand_spread(px, dis / 3);
	}

	d = distance(py, px, y, x);
	if ((d >= min) && (d <= dis))
	  break;
      }

      /* Ignore illegal locations */
      if (!in_bounds(y, x))
	continue;

      /* Require "naked" floor space */
      if (!cave_empty_bold(y, x))
	continue;

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

  /* Move player */
  teleport_player_to(y,x);

  /* Handle stuff XXX XXX XXX */
  handle_stuff();

  c_ptr = &cave[y][x];
  /* Hack -- enter a store if we are on one */
  if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
      (c_ptr->feat <= FEAT_SHOP_TAIL)) {
    /* Disturb */
    disturb(0, 0);

    /* Hack -- enter store */
    command_new = '_';
  }
  /* Hack -- enter a building if we are on one -KMW- */
  if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
      (c_ptr->feat <= FEAT_BLDG_TAIL)) {
    /* Disturb */
    disturb(0, 0);

    /* Hack -- enter building */
    command_new = ']';
  }
  
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
	int ny, nx, oy, ox, d, i, min;
	int tries = 0;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];

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
		tries++;

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
			if (!cave_empty_bold(ny, nx)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave[ny][nx].feat == FEAT_GLYPH) continue;
			if (cave[ny][nx].feat == FEAT_MINOR_GLYPH) continue;

			/* ...nor onto the Pattern */
			if ((cave[ny][nx].feat >= FEAT_PATTERN_START) &&
			    (cave[ny][nx].feat <= FEAT_PATTERN_XTRA2)) continue;

			/* No teleporting into vaults and such */
			if (!(p_ptr->inside_quest))
				if (cave[ny][nx].info & (CAVE_ICKY)) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return;
	}

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
	int ny, nx, oy, ox, d, i, min;
	int dis = 1;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];
	int attempts = 500;

	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* "Skill" test */
	/*if (randint(100) > r_info[m_ptr->r_idx].level) return;*/

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	/*min = dis / 2;*/
	min = 1;

	/* Look until done */
	while (look && --attempts)
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
			if (!cave_empty_bold(ny, nx)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave[ny][nx].feat == FEAT_GLYPH) continue;
			if (cave[ny][nx].feat == FEAT_MINOR_GLYPH) continue;

			/* ...nor onto the Pattern */
			if ((cave[ny][nx].feat >= FEAT_PATTERN_START) &&
			    (cave[ny][nx].feat <= FEAT_PATTERN_XTRA2)) continue;

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
	int d, i, min, ox, oy, x, y;
	int tries = 0;

	int xx = -1, yy = -1;

	bool look = TRUE;

	if (p_ptr->abilities[(CLASS_MAGE * 10) + 8] >= 1 && dis <= (p_ptr->abilities[(CLASS_MAGE * 10) + 8] * 2))
	{
		char ch;

		msg_print("Use Advanced Teleportation? [y/n]");
		if (ch == 'y' || ch == 'Y')
		{
			call_lua("advanced_teleportation", "d", "", dis);
			return;
		}
	}

	if (p_ptr->inside_quest)
	{
		msg_print("You cannot teleport here.");
		return;
	}

	if (dis > 200) dis = 200; /* To be on the safe side... */

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		tries++;

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

			/* If there is ANY events, do not teleport */
			if (cave[y][x].event > 0) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return;
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
						/*
						 * The latter limitation is to avoid
						 * totally unkillable suckers...
						 */
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
 * get a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 */
void get_pos_player(int dis, int *ny, int *nx)
{
        int d, i, min, x, y;
	int tries = 0;

	bool look = TRUE;

	if (dis > 200) dis = 200; /* To be on the safe side... */

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		tries++;

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

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return;
	}

        *ny = y;
        *nx = x;
}

/*
 * Teleport a monster to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 */
void teleport_monster_to(int m_idx, int ny, int nx)
{
	int y, x, oy, ox, dis = 0, ctr = 0;
        monster_type *m_ptr = &m_list[m_idx];

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

                /* Not on the player's grid */
		/* Accept "naked" floor grids */
                if (cave_naked_bold(y, x) && (y != py) && (x != px)) break;

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
        sound(SOUND_TPOTHER);

        /* Save the old position */
        oy = m_ptr->fy;
        ox = m_ptr->fx;
        cave[oy][ox].m_idx = 0;

        /* Move the monster */
        m_ptr->fy = y;
        m_ptr->fx = x;
        cave[y][x].m_idx = m_idx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Redraw the old spot */
	lite_spot(oy, ox);

	/* Redraw the new spot */
        lite_spot(m_ptr->fy, m_ptr->fx);
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
                /*if (cave_naked_bold(y, x)) break; */
                break;

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
	/* No effect in arena or quest */
	if (p_ptr->inside_quest)
	{
		msg_print("There is no effect.");
		return;
	}

	if (!dun_level)
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

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else if (is_quest(dun_level) || (dun_level >= MAX_DEPTH-1))
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

		/* Leaving */
		p_ptr->leaving = TRUE;
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

		/* Leaving */
		p_ptr->leaving = TRUE;
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

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	/* Sound */
	sound(SOUND_TPLEVEL);
}



/*
 * Recall the player to town or dungeon
 */
void recall_player(void)
{
#if 0
        if (!p_ptr->town_num)
	{
		/* TODO: Recall the player to the last visited town */
		msg_print("Nothing happens.");
		return;
	}
#endif
        if (dun_level && (max_dlv[dungeon_type] > dun_level) && (!p_ptr->inside_quest))
	{
		if (get_check("Reset recall depth? "))
                        max_dlv[dungeon_type] = dun_level;
		
	}
	if (!p_ptr->word_recall)
	{
		p_ptr->word_recall = rand_int(21) + 15;
		msg_print("The air about you becomes charged...");
	}
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}
}



/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(int max)
{
	switch (randint(max))
	{
		case  1: return (TERM_RED);
		case  2: return (TERM_GREEN);
		case  3: return (TERM_BLUE);
		case  4: return (TERM_YELLOW);
		case  5: return (TERM_ORANGE);
		case  6: return (TERM_VIOLET);
		case  7: return (TERM_L_RED);
		case  8: return (TERM_L_GREEN);
		case  9: return (TERM_L_BLUE);
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
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
{
	/* Check if A.B.'s new graphics should be used (rr9) */
	if (strcmp(ANGBAND_GRAF, "new") == 0)
	{
		/* Analyze */
		switch (type)
		{
			case GF_MISSILE:        return (0x0F);
			case GF_ACID:           return (0x04);
		        case GF_ELEC:           return (0x02);
			case GF_FIRE:           return (0x00);
			case GF_COLD:           return (0x01);
			case GF_FROSTFIRE:      return (0x01);
			case GF_POIS:           return (0x03);
			case GF_TOXIC:          return (0x03);
			case GF_MANA:           return (0x0E);
			case GF_WATER:          return (0x04);
			case GF_CHAOS:          return (mh_attr(15));
			case GF_CONFUSION:      return (mh_attr(4));
			case GF_SOUND:          return (0x09);
			case GF_EARTH:          return (0x08);
			case GF_GREY:           return (0x09);
			case GF_LITE:           return (0x06);
			case GF_DARK:           return (0x07);
			case GF_MUD:            return (0x08);
			case GF_ICE:           return (0x01);
			case GF_ROCKET:         return (0x0F);
			case GF_DEATH_RAY:      return (0x07);
			case GF_RADIO:           return (mh_attr(2));
			case GF_TELEKINESIS:
			case GF_DOMINATION:
                        case GF_PSI_HITRATE:
                        case GF_PSI_FEAR:
						return (0x09);
		}

	}
	/* Normal tiles or ASCII */
	else if (use_color)
	{
		/* Analyze */
		switch (type)
		{
			case GF_MISSILE:        return (TERM_SLATE);
			case GF_ACID:           return (randint(5)<3?TERM_YELLOW:TERM_L_GREEN);
		        case GF_ELEC:           return (randint(7)<6?TERM_WHITE:(randint(4)==1?TERM_BLUE:TERM_L_BLUE));
			case GF_FIRE:           return (randint(6)<4?TERM_YELLOW:(randint(4)==1?TERM_RED:TERM_L_RED));
			case GF_COLD:           return (randint(6)<4?TERM_WHITE:TERM_L_WHITE);
			case GF_FROSTFIRE:      return (randint(6)<4?TERM_WHITE:TERM_L_WHITE);
			case GF_POIS:           return (randint(5)<3?TERM_L_GREEN:TERM_GREEN);
			case GF_TOXIC:           return (randint(5)<3?TERM_L_GREEN:TERM_GREEN);
			case GF_MANA:           return (randint(5)!=1?TERM_VIOLET:TERM_L_BLUE);
			case GF_WATER:          return (randint(4)==1?TERM_L_BLUE:TERM_BLUE);
			case GF_CHAOS:          return (mh_attr(15));
			case GF_CONFUSION:      return (mh_attr(4));
			case GF_SOUND:          return (randint(4)==1?TERM_VIOLET:TERM_WHITE);
			case GF_EARTH:         return (randint(5)<3?TERM_UMBER:TERM_SLATE);
			case GF_GREY:        return (randint(5)<3?TERM_SLATE:TERM_L_WHITE);
			case GF_LITE:           return (randint(4)==1?TERM_ORANGE:TERM_YELLOW);
			case GF_DARK:           return (randint(4)==1?TERM_DARK:TERM_L_DARK);
			case GF_MUD:         return (randint(5)<3?TERM_UMBER:TERM_L_BLUE);
			case GF_ICE:           return (randint(6)<4?TERM_WHITE:TERM_L_WHITE);
			case GF_ROCKET:         return (randint(6)<4?TERM_L_RED:(randint(4)==1?TERM_RED:TERM_L_UMBER));
                        case GF_DEATH:
			case GF_DEATH_RAY:
                                                return (TERM_L_DARK);
			case GF_RADIO:           return (mh_attr(2));
			case GF_TELEKINESIS:
			case GF_DOMINATION:
                        case GF_PSI_HITRATE:
                        case GF_PSI_FEAR:
						return (randint(3)!=1?TERM_L_BLUE:TERM_WHITE);
		}
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
static u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
	int base;

	byte k;

	byte a;
	char c;

	/* No motion (*) */
	if ((ny == y) && (nx == x)) base = 0x30;

	/* Vertical (|) */
	else if (nx == x) base = 0x40;

	/* Horizontal (-) */
	else if (ny == y) base = 0x50;

	/* Diagonal (/) */
	else if ((ny-y) == (x-nx)) base = 0x60;

	/* Diagonal (\) */
	else if ((ny-y) == (nx-x)) base = 0x70;

	/* Weird (*) */
	else base = 0x30;

	/* Basic spell color */
	k = spell_color(typ);

	/* Obtain attr/char */
	a = misc_to_attr[base+k];
	c = misc_to_char[base+k];

	/* Create pict */
	return (PICT(a,c));
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
void take_hit(s32b damage, cptr hit_from)
{
        int old_chp = p_ptr->chp;

        bool monster_take = FALSE;

	char death_message[80];

	int warning = (p_ptr->mhp * hitpoint_warn / 10);


	/* Paranoia */
	if (death) return;

	/* If we're leaving, no damage. */
	if (p_ptr->leaving) return;

	/* Disturb */
	disturb(1, 0);

        /* Protection item? */
        if (protection_check()) damage = damage / 2;
	
	{
		char total_damages[80];
		c_put_str(TERM_L_GREEN, "                          ", 23, 64);
		sprintf(total_damages, "Dam: %ld", damages_counter);
		if (damages_counter_player_damages) c_put_str(TERM_L_GREEN, total_damages, 23, 64);
		else c_put_str(TERM_L_RED, total_damages, 23, 64);
		damages_counter = damage;
		damages_counter_player_damages = FALSE;
		damages_counter_duration = 3;
	}

	/* Hurt the player */
        if(!monster_take) p_ptr->chp -= damage;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		bool willsurvive = FALSE;

		/* Restless Dead may allow you to survive. */
		if (p_ptr->abilities[(CLASS_NIGHT1 * 10) + 4] >= 1)
		{
			int ppower;
			int dpower;

			ppower = (p_ptr->cursed / 3) * p_ptr->abilities[(CLASS_NIGHT1 * 10) + 4];
			dpower = (dun_level * 3);

			if (randint(ppower) >= randint(dpower)) willsurvive = TRUE;
		}

		if ((willsurvive) && !(death))
		{
			msg_print("You refuse to die!");
			p_ptr->chp = 0;
			update_and_handle();
		}
		else
		{

			if ((p_ptr->inside_quest > 0) && (p_ptr->death_dialog > 0))
			{
				p_ptr->chp = 1;
				death = FALSE;
				if (p_ptr->eventdeath > 0) p_ptr->events[p_ptr->eventdeath] = p_ptr->eventdeathset;
				show_dialog(p_ptr->death_dialog);
			}
			else if (p_ptr->inside_secret > 0)
			{
				p_ptr->chp = 1;
				death = FALSE;
				show_dialog(20000);
				p_ptr->inside_quest = 0;
				p_ptr->inside_secret = 0;
				dun_level = 0;
				p_ptr->leaving = TRUE;
			}
			else
			{
                		cptr str;

                		/* You're dying! */
                		dying = TRUE;

                		/* Sound */
                		/*sound(SOUND_DEATH);*/

                		/* Hack -- Note death */
                		/*if (!last_words)
                		{
                        		msg_print("You die.");
                        		msg_print(NULL);
                		}
                		else
                		{
                        		(void)get_rnd_line("death.txt", death_message);
                        		msg_print(death_message);
                		}*/

                		/* Note cause of death */
                		(void)strcpy(died_from, hit_from);

                		if (p_ptr->image) strcat(died_from,"(?)");

                		/* Increase death count! */
                		/*p_ptr->deathcount += 1;*/

                		/* No longer a winner */
                		total_winner = FALSE;

                		/* Leaving */
				/* As of Portralis 0.4, our destination is the Limbo! :) */
				dun_level = 1;
				p_ptr->chp = 1;
				p_ptr->inside_quest = 30000;
				p_ptr->questx = 15;
				p_ptr->questy = 7;
				p_ptr->word_recall = 0;
                		p_ptr->leaving = TRUE;
				/*generate_cave();*/

				/*dying = FALSE;*/

				/*do_cmd_save_game();*/

				/* We won't resurrect in the wild... */
				/*p_ptr->wild_mode = FALSE;*/

                		/* Note death */
                		/*death = TRUE;*/

                		/*if (get_check("Dump the screen? "))
                		{
                        		do_cmd_save_screen();
                		}*/

                		/* Dead */
                		return;
			}
		}
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_chp > warning)) bell();

		sound(SOUND_WARN);		

		
                msg_print("*** LOW HITPOINT WARNING! ***");
		msg_print(NULL);
	}

	/* Run a script? */
	if (damage > 0) call_lua("player_take_damages", "(d)", "", damage);
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
		case TV_AMMO:
		case TV_RANGED:
		case TV_WEAPON:
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
                case TV_EGG:
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
                case TV_EGG:
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
		case TV_AMMO:
		case TV_RANGED:
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

		/* Staffs/Scrolls burn */
		case TV_STAFF:
		case TV_SCROLL:
                case TV_EGG:
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
                case TV_EGG:
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
        u32b f1, f2, f3, f4;
	if (!hates_acid(o_ptr)) return (FALSE);
        object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f3 & (TR3_IGNORE_ACID)) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
static int set_elec_destroy(object_type *o_ptr)
{
        u32b f1, f2, f3, f4;
	if (!hates_elec(o_ptr)) return (FALSE);
        object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f3 & (TR3_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}


/*
 * Burn something
 */
static int set_fire_destroy(object_type *o_ptr)
{
        u32b f1, f2, f3, f4;
	if (!hates_fire(o_ptr)) return (FALSE);
        object_flags(o_ptr, &f1, &f2, &f3, &f4);
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);
	return (TRUE);
}


/*
 * Freeze things
 */
static int set_cold_destroy(object_type *o_ptr)
{
        u32b f1, f2, f3, f4;
	if (!hates_cold(o_ptr)) return (FALSE);
        object_flags(o_ptr, &f1, &f2, &f3, &f4);
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

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr) || o_ptr->art_name) continue;

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
                
				/* Hack -- If rods or wand are destroyed, the total maximum 
				 * timeout or charges of the stack needs to be reduced, 
				 * unless all the items are being destroyed. -LM-
				 */
				if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) 
					&& (amt < o_ptr->number))
				{
					o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
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
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type     *o_ptr = NULL;

        u32b            f1, f2, f3, f4;

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
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

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
void acid_dam(s32b dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total Immunity */
	if ((dam <= 0)) return;

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_acid_destroy, inv);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(s32b dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if ((dam <= 0)) return;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_elec_destroy, inv);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(s32b dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Totally immune */
	if ((dam <= 0)) return;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_fire_destroy, inv);
}


/*
 * Hurt the player with Cold
 */
void cold_dam(s32b dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if ((dam <= 0)) return;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
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
        int value;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];
        value++;

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
bool dec_stat(int stat, int amount, int mode)
{
        int cur, max, loss = 0, same, res = FALSE;


	/* Acquire current value */
	cur = p_ptr->stat_cur[stat];        
        max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
        if (cur > 1)
	{
                cur -= (amount);

		/* Prevent illegal values */
                if (cur < 1) cur = 1;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
        if ((mode==STAT_DEC_PERMANENT) && (max > 1))
	{
                max -= (amount);

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

		if (mode==STAT_DEC_TEMPORARY)
		{
			u16b dectime;

			/* a little crude, perhaps */
			dectime = rand_int(max_dlv[dungeon_type]*50) + 50;
			
			/* prevent overflow, stat_cnt = u16b */
			/* or add another temporary drain... */
			if ( ((p_ptr->stat_cnt[stat]+dectime)<p_ptr->stat_cnt[stat]) ||
			    (p_ptr->stat_los[stat]>0) )

			{
				p_ptr->stat_cnt[stat] += dectime;
				p_ptr->stat_los[stat] += loss;
			}
			else
			{
				p_ptr->stat_cnt[stat] = dectime;
				p_ptr->stat_los[stat] = loss;
			}
		}

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

		/* Remove temporary drain */
		p_ptr->stat_cnt[stat] = 0;

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
 * If "mode is set to 0 then a random slot will be used, if not the "mode"
 * slot will be used.
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(int mode)
{
        int             t = mode;
	object_type     *o_ptr;
	char            o_name[80];

        if(!mode)
        {
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
	if ((artifact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 71))
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


void mutate_player(void)
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
	if (!special_flag)
	{
	switch (randint(7))
	{
		case 1: case 2: case 3:
		{
			teleport_player(200);
			break;
		}

		case 4: case 5:
		{
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

		case 6:
		{
                        if (rand_int(100) < p_ptr->stat_ind[A_WIS])
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Teleport Level */
			teleport_player_level();
			break;
		}

		case 7:
		{
                        if (rand_int(100) < p_ptr->stat_ind[A_WIS])
			{
				msg_print("You resist the effects!");
				break;
			}

			msg_print("Your body starts to scramble...");
			mutate_player();
			break;
		}
	}
        }
}

/*
 * Convert 2 couples of coordonates to a direction
 */
int yx_to_dir(int y2, int x2, int y1, int x1)
{
        int y = y2 - y1, x = x2 - x1;

        if((y == 0) && (x == 1)) return 6;
        if((y == 0) && (x == -1)) return 4;
        if((y == -1) && (x == 0)) return 8;
        if((y == 1) && (x == 0)) return 2;
        if((y == -1) && (x == -1)) return 7;
        if((y == -1) && (x == 1)) return 9;
        if((y == 1) && (x == 1)) return 3;
        if((y == 1) && (x == -1)) return 1;

        return 5;
}

/*
 * Give the opposate direction of the given one
 */
int invert_dir(int dir)
{
        if(dir == 4) return 6;
        if(dir == 6) return 4;
        if(dir == 8) return 2;
        if(dir == 2) return 8;
        if(dir == 7) return 3;
        if(dir == 9) return 1;
        if(dir == 1) return 9;
        if(dir == 3) return 7;
        return 5;
}

/*
 * Determine which way the mana path follow
 */
int get_mana_path_dir(int y, int x, int oy, int ox, int pdir, int mana)
{
        int dir[8] = {5, 5, 5, 5, 5, 5, 5, 5}, n = 0, i;
        int r = 0;

        /* Check which case are allowed */
        if(cave[y - 1][x].mana == mana) dir[n++] = 8;
        if(cave[y + 1][x].mana == mana) dir[n++] = 2;
        if(cave[y][x - 1].mana == mana) dir[n++] = 4;
        if(cave[y][x + 1].mana == mana) dir[n++] = 6;

        /* If only 2 possibilities select the only good one */
        if(n == 2)
        {
                if(invert_dir(yx_to_dir(y, x, oy, ox)) != dir[0]) return dir[0];
                if(invert_dir(yx_to_dir(y, x, oy, ox)) != dir[1]) return dir[1];

                /* Should never happen */
                return 5;
        }


        /* Check if it's not your last place */
        for(i = 0; i < n; i++)
        {
                if((oy == y + ddy[dir[i]]) && (ox == x + ddx[dir[i]]))
                {
                        if(dir[i] == 8) dir[i] = 2;
                        else if(dir[i] == 2) dir[i] = 8;
                        else if(dir[i] == 6) dir[i] = 4;
                        else if(dir[i] == 4) dir[i] = 6;
                }
        }

        /* Select the desired one if possible */
        for(i = 0; i < n; i++)
        {
                if((dir[i] == pdir) && (cave[y + ddy[dir[i]]][x + ddx[dir[i]]].mana == mana)) return dir[i];
        }

        /* If not select a random one */
        if(n > 2)
        {
                byte nb = 200;

                while(nb)
                {
                        nb--;

                        r = rand_int(n);
                        if((dir[r] != 5) && (yx_to_dir(y, x, oy, ox) != dir[r])) break;
                }
                return dir[r];
        }
        /* If nothing is found return 5 */
        else return 5;
}

/*
 * Determine the path taken by a projection.
 *
 * The projection will always start from the grid (y1,x1), and will travel
 * towards the grid (y2,x2), touching one grid per unit of distance along
 * the major axis, and stopping when it enters the destination grid or a
 * wall grid, or has travelled the maximum legal distance of "range".
 *
 * Note that "distance" in this function (as in the "update_view()" code)
 * is defined as "MAX(dy,dx) + MIN(dy,dx)/2", which means that the player
 * actually has an "octagon of projection" not a "circle of projection".
 *
 * The path grids are saved into the grid array pointed to by "gp", and
 * there should be room for at least "range" grids in "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the destination grid, respectively.
 *
 * The "PROJECT_JUMP" flag, which for the "project()" function means to
 * start at a special grid (which makes no sense in this function), means
 * that the path should be "angled" slightly if needed to avoid any wall
 * grids, allowing the player to "target" any grid which is in "view".
 * This flag is non-trivial and has not yet been implemented, but could
 * perhaps make use of the "vinfo" array (above).  XXX XXX XXX
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if (y1,x1) and (y2,x2) are equal.
 *
 * This algorithm is similar to, but slightly different from, the one used
 * by "update_view_los()", and very different from the one used by "los()".
 */
sint project_path(u16b *gp, int range, int y1, int x1, int y2, int x2, int flg)
{
        int y, x, mana = 0, dir = 0;

	int n = 0;
	int k = 0;

	/* Absolute */
	int ay, ax;

	/* Offsets */
	int sy, sx;

	/* Fractions */
	int frac;

	/* Scale factors */
	int full, half;

	/* Slope */
	int m;


	/* No path necessary (or allowed) */
	if ((x1 == x2) && (y1 == y2)) return (0);

        /* Hack -- to make a bolt/beam/ball follow a mana path */
        if(flg & PROJECT_MANA_PATH)
        {
                int oy = y1, ox = x1, pdir = yx_to_dir(y2, x2, y1, x1);

                /* Get the mana path level to follow */
                mana = cave[y1][x1].mana;

		/* Start */
                dir = get_mana_path_dir(y1, x1, y1, x1, pdir, mana);
                y = y1 + ddy[dir];
                x = x1 + ddx[dir];

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
                        if (n >= range + 10) return n;

			/* Always stop at non-initial wall grids */
                        if ((n > 0) && !cave_floor_bold_project(y, x)) return n;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
                                if ((n > 0) && (cave[y][x].m_idx != 0)) return n;
			}

                        /* Get the new direction */
                        dir = get_mana_path_dir(y, x, oy, ox, pdir, mana);
                        if(dir == 5) return n;
                        oy = y;
                        ox = x;
                        y += ddy[dir];
                        x += ddx[dir];
		}
        }

	/* Analyze "dy" */
	if (y2 < y1)
	{
		ay = (y1 - y2);
		sy = -1;
	}
	else
	{
		ay = (y2 - y1);
		sy = 1;
	}

	/* Analyze "dx" */
	if (x2 < x1)
	{
		ax = (x1 - x2);
		sx = -1;
	}
	else
	{
		ax = (x2 - x1);
		sx = 1;
	}


	/* Number of "units" in one "half" grid */
	half = (ay * ax);

	/* Number of "units" in one "full" grid */
	full = half << 1;


	/* Vertical */
	if (ay > ax)
	{
		/* Start at tile edge */
		frac = ax * ax;

		/* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1 + sy;
		x = x1;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
                        if ((n > 0) && !cave_floor_bold_project(y, x) && !(flg & PROJECT_WALL)) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (cave[y][x].m_idx != 0)) break;
			}

			/* Slant */
			if (m)
			{
				/* Advance (X) part 1 */
				frac += m;

				/* Horizontal change */
				if (frac >= half)
				{
					/* Advance (X) part 2 */
					x += sx;

					/* Advance (X) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (Y) */
			y += sy;
		}
	}

	/* Horizontal */
	else if (ax > ay)
	{
		/* Start at tile edge */
		frac = ay * ay;

		/* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
                        if ((n > 0) && !cave_floor_bold_project(y, x) && !(flg & PROJECT_WALL)) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (cave[y][x].m_idx != 0)) break;
			}

			/* Slant */
			if (m)
			{
				/* Advance (Y) part 1 */
				frac += m;

				/* Vertical change */
				if (frac >= half)
				{
					/* Advance (Y) part 2 */
					y += sy;

					/* Advance (Y) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (X) */
			x += sx;
		}
	}

	/* Diagonal */
	else
	{
		/* Start */
		y = y1 + sy;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (n >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
                        if ((n > 0) && !cave_floor_bold_project(y, x) && !(flg & PROJECT_WALL)) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (cave[y][x].m_idx != 0)) break;
			}

			/* Advance (Y) */
			y += sy;

			/* Advance (X) */
			x += sx;
		}
	}


	/* Length */
	return (n);
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
static bool project_f(int who, int r, int y, int x, s32b dam, int typ)
{
	cave_type       *c_ptr = &cave[y][x];

	bool            obvious = FALSE;

        bool flag = FALSE;

	/* XXX XXX XXX */
	who = who ? who : 0;

	/* Analyze the type */
	switch (typ)
	{
		/* Ignore most effects */
		case GF_ACID:
		case GF_ELEC:
		case GF_COLD:
		case GF_EARTH:
		case GF_SOUND:
		case GF_MANA:
		case GF_TELEKINESIS:
		case GF_DOMINATION:
                case GF_PSI_FEAR:
                case GF_PSI_HITRATE:
		{
			break;
		}

                case GF_BETWEEN_GATE:
                {
                        int y1 = randint(cur_hgt) - 1, x1 = randint(cur_wid) - 1, y2 = y1, x2 = x1;
                        int try = 1000;

                        if(!((f_info[cave[y][x].feat].flags1 & FF1_FLOOR) && !(f_info[cave[y][x].feat].flags1 & FF1_REMEMBER) && !(f_info[cave[y][x].feat].flags1 & FF1_PERMANENT))) break;

                        while(!((f_info[cave[y2][x2].feat].flags1 & FF1_FLOOR) && !(f_info[cave[y][x].feat].flags1 & FF1_REMEMBER) && !(f_info[cave[y][x].feat].flags1 & FF1_PERMANENT)) && try)
                        {
                                y2 = y1 = randint(cur_hgt) - 1;
                                x2 = x1 = randint(cur_wid) - 1;
                                scatter(&y2, &x2, y1, x1, 20, 0);
                                try --;
                        }
                        if(!try) break;

                        cave_set_feat(y, x, FEAT_BETWEEN);
                        cave[y][x].special = x2 + (y2 << 8);

                        cave_set_feat(y2, x2, FEAT_BETWEEN);
                        cave[y2][x2].special = x + (y << 8);
                        break;
                }

                /* Burn trees */
		case GF_FIRE:
                {
                        if (c_ptr->feat == FEAT_TREES)
                        {
				/* Forget the trap */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the trap */
                                cave_set_feat(y, x, FEAT_GRASS);
                        }
			else if (c_ptr->feat == FEAT_SNOW_TREES)
                        {
				/* Forget the trap */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the trap */
                                cave_set_feat(y, x, FEAT_SNOW);
                        }
			else if (c_ptr->feat == FEAT_ICE_WALL)
                        {
				/* Forget the trap */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the trap */
                                cave_set_feat(y, x, FEAT_SHAL_WATER);
                        }
                        break;
                }

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Destroy traps */
			if (c_ptr->t_idx != 0)
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
				c_ptr->t_idx = 0;
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
			    (c_ptr->t_idx != 0) ||
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

				/* Remove traps */
				c_ptr->t_idx = 0;
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
		case GF_STONE_TO_MUD:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold_project(y, x)) break;

			/* Permanent walls */
			if (c_ptr->feat >= FEAT_PERM_EXTRA) break;

			/* Granite */
			if (c_ptr->feat >= FEAT_WALL_EXTRA)
			{
				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma with treasure */
			else if (c_ptr->feat >= FEAT_MAGMA_H)
			{
				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma */
			else if (c_ptr->feat >= FEAT_MAGMA)
			{
				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Rubble */
			else if (c_ptr->feat == FEAT_RUBBLE)
			{

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

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

                        /* Place a wall */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);

			break;
		}

                case GF_WINDS_MANA:
                {
                        if(dam >= 256)
                        {
                                /* With erase mana */

                                /* Absorb some of the mana of the grid */
                                p_ptr->csp += cave[y][x].mana / 80;
                                if(p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

                                /* Set the new amount */
                                cave[y][x].mana = dam - 256;
                        }
                        else
                        {
                                /* Without erase mana */
                                int amt = cave[y][x].mana + dam;

                                /* Check if not overflow */
                                if(amt > 255) amt = 255;

                                /* Set the new amount */
                                cave[y][x].mana = amt;
                        }
                        break;
                }

                case GF_LAVA_FLOW:
		{
                        /* Shallow Lava */
                        if(dam == 1)
                        {
                                /* Require a "naked" floor grid */
                                if (!cave_naked_bold(y, x)) break;

                                /* Place a shallow lava */
                                cave_set_feat(y, x, FEAT_SHAL_LAVA);
                        }
                        /* Deep Lava */
                        else
                        {
                                /* Require a "naked" floor grid */
                                if (cave_perma_bold(y, x) || !dam) break;

                                /* Place a deep lava */
                                cave_set_feat(y, x, FEAT_DEEP_LAVA);

                                /* Dam is used as a counter for the number of grid to convert */
                                dam--;
                        }
			break;
		}

		/* Lite up the grid */
		case GF_LITE:
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
		case GF_DARK:
		{
			/* Notice */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			/* Turn off the light. */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if (c_ptr->feat == FEAT_FLOOR)
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
                case GF_DESTRUCTION:
                {
                        int t;

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);

			/* Hack -- Notice player affect */
			if ((x == px) && (y == py))
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
                                break;;
			}

			/* Delete the monster (if any) */
			delete_monster(y, x);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x))
			{
				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
                        obvious = TRUE;
                        break;
                }
	}

	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		if (!p_ptr->resist_blind)
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint(10));
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
static bool project_o(int who, int r, int y, int x, s32b dam, int typ)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

        u32b f1, f2, f3, f4;

	char o_name[80];

	int o_sval = 0;
	bool is_potion = FALSE;
        int xx, yy;


	/* XXX XXX XXX */
	who = who ? who : 0;

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
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if ((artifact_p(o_ptr) || o_ptr->art_name)) is_art = TRUE;

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

			/* Hack -- break potions and such */
			case GF_EARTH:
			case GF_SOUND:
			{
				if (hates_cold(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}

			/* Mana and Chaos -- destroy everything */
			case GF_MANA: 
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				break;
			}

			case GF_CHAOS:
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				if (f2 & (TR2_RES_CHAOS)) ignore = TRUE;
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
                        case GF_STAR_IDENTIFY:
                        {
                                /* Identify it fully */
                                object_aware(o_ptr);
                                object_known(o_ptr);

                                /* Mark the item as fully known */
                                o_ptr->ident |= (IDENT_MENTAL);
                                break;
                        }
                        case GF_IDENTIFY:
                        {
                                object_aware(o_ptr);
                                object_known(o_ptr);
                                break;
                        }
                        case GF_RAISE:
                        {
                                xx=x;
                                yy=y;
                                get_pos_player(100, &y, &x);
                                if(place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE, 0))
                                        msg_print("A monster raise from the grave!");
				do_kill = TRUE;
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

				o_sval = o_ptr->sval;
                                is_potion = (k_info[o_ptr->k_idx].tval == TV_POTION);


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
bool project_m(int who, int r, int y, int x, s32b dam, int typ)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	monster_type *m_ptr = &m_list[c_ptr->m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int m_idx = c_ptr->m_idx;

	char killer [80];

        /* cptr name = (r_name + r_ptr->name); */

	s32b            div, new_exp, new_exp_frac;

	/* Is the monster "seen"? */
	bool seen = m_ptr->ml;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

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

	/* Reset the "monster_died" variable. */
	monster_died = FALSE;

	/* Walls protect monsters */
	/* (No, they don't)  */
#if 0
	if (!cave_floor_bold(y,x)) return (FALSE);
#endif

	/* Nobody here */
	if (!c_ptr->m_idx) return (FALSE);

	/* Never affect projector */
	if (who && (c_ptr->m_idx == who)) return (FALSE);

	/* Don't affect already death monsters */
	/* Prevents problems with chain reactions of exploding monsters */
	if (m_ptr->hp < 0) return (FALSE);

	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);

	/* Some monsters get "destroyed" */
	if ((r_ptr->flags3 & (RF3_DEMON)) ||
	    (r_ptr->flags3 & (RF3_UNDEAD)) ||
	    (r_ptr->flags2 & (RF2_STUPID)) ||
	    (r_ptr->flags3 & (RF3_NONLIVING)) ||
	    (strchr("Evg", r_ptr->d_char)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}

	/* We call lua here. */
	call_lua("element_hit_monster", "(dddl)", "l", who, c_ptr->m_idx, typ, dam, &dam);

	/* Has the monster moved? */
	if (m_ptr->fx != x || m_ptr->fy != y)
	{
		x = m_ptr->fx;
		y = m_ptr->fy;
		c_ptr = &cave[y][x];
	}
		

	/* If damages are below or equal 0, return. */
	if (dam <= 0) return (FALSE);

	/* If another monster did the damage, hurt the monster by hand */
	if (who)
	{

		/* Redraw (later) if needed */
		if (health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up */
		m_ptr->csleep = 0;

		/* Hurt the monster */
		m_ptr->hp -= dam;

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			bool sad = FALSE;

			if (is_pet(m_ptr) && !(m_ptr->ml))
				sad = TRUE;

			if (m_ptr->lives > 0)
			{
				sound(SOUND_FLEE);
				m_ptr->lives -= 1;
				msg_format("%^s has lost a life point.", m_name);
				m_ptr->hp = m_ptr->maxhp;
				return (FALSE);
			}

			/* Immortality. */
			if ((r_ptr->flags7 & (RF7_IMMORTAL)) && (enemy_immortality))
			{
				msg_format("%^s has been knocked out.", m_name);
				m_ptr->hp = 1;
				m_ptr->seallight = 5;
				return (FALSE);
			}

#ifdef PET_GAIN_EXP

		/* Gain experience */
                gain_exp_kill(m_ptr);

#endif

			/* Generate treasure, etc */
			monster_death(c_ptr->m_idx);

			/* Delete the monster */
			delete_monster_idx(c_ptr->m_idx);

			/* Give detailed messages if destroyed */
			if (note) msg_format("%^s%s", m_name, note);
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(c_ptr->m_idx, dam);

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(c_ptr->m_idx, dam, &fear, note_dies))
		{
			/* Dead monster */
			monster_died = TRUE;
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(c_ptr->m_idx, dam);

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


/* Is the spell unsafe for the player ? */
bool unsafe = FALSE;


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
 * NOTE (Zangband): 'Bolt' attacks can be reflected back, so we need to know
 * if this is actually a ball or a bolt spell
 *
 *
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 */
static bool project_p(int who, int r, int y, int x, s32b dam, int typ, int a_rad)
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

	/* Monster race */
	monster_race *r_ptr;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;

	/* Player is not here */
	if ((x != px) || (y != py)) return (FALSE);

	/* Player cannot hurt himself */
        if ((!who)&&(!unsafe)) return (FALSE);

	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;

	/* If the player is hit by a trap, be more descritive */
	if (who == -2) fuzzy = TRUE;

	if (who != -2)
	{

		/* Get the source monster */
		m_ptr = &m_list[who];

		/* Extract monster race data. */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, m_ptr, 0x88);
	}

	/* We call lua here. */
	call_lua("element_hit_player", "(ddld)", "", who, typ, dam, a_rad);

	/* Disturb */
	disturb(1, 0);


	/* Return "Anything seen?" */
	return (obvious);
}



/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (negative for "player")
 *        jk -- -2 for traps, only used with project_jump
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
 * Note that for consistency, we "pretend" that the bolt actually takes "time"
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that all projections now "explode" at their final destination, even
 * if they were being projected at a more distant destination.  This means
 * that "ball" spells will *always* explode.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the "illumination" of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int y, int x, s32b dam, int typ, int flg)
{
	int i, t, dist;

	int y1, x1;
	int y2, x2;

	int dist_hack = 0;

	int y_saver, x_saver; /* For reflecting monsters */

	int msec = delay_factor * delay_factor * delay_factor;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
        u16b path_g[1024];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
        byte gx[1024], gy[1024];

	/* Encoded "radius" info (see above) */
        byte gm[64];

	/* Before we begin, let's apply "Weaken Elemental Attacks". */
	if (who > 0 && p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1)
	{
		monster_type *m_ptr;
		m_ptr = &m_list[who];

		if (m_ptr->abilities & (WEAKENED_ELEMENTAL) && is_elemental(typ) && typ != GF_PHYSICAL)
		{
			int ppower;
			int mpower;
			int totpower;
			int respercent;

			ppower = p_ptr->skill[22] / 2;
			ppower = ppower + multiply_divide(ppower, p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] * 10, 100);
			if (monster_physical) mpower = m_ptr->str + m_ptr->skill_attack;
			else if (monster_ranged) mpower = m_ptr->dex + m_ptr->skill_ranged;
			else mpower = m_ptr->mind + m_ptr->skill_magic;

			totpower = ppower + mpower;
			respercent = multiply_divide(100, ppower, totpower);

			dam = dam - multiply_divide(dam, respercent, 100);
			if (typ == p_ptr->elemlord) dam = dam / 2;
		}
	}

	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);
	}

	/* Start at player */
	else if (who <= 0)
	{
		x1 = px;
		y1 = py;
	}

	/* Start at monster */
	else if (who > 0)
	{
		x1 = m_list[who].fx;
		y1 = m_list[who].fy;
	}

	/* Oops */
	else
	{
		x1 = x;
		y1 = y;
	}

	y_saver = y1;
	x_saver = x1;

	/* Default "destination" */
	y2 = y;
	x2 = x;


	/* Hack -- verify stuff */
	if (flg & (PROJECT_THRU))
	{
		if ((x1 == x2) && (y1 == y2))
		{
			flg &= ~(PROJECT_THRU);
		}
	}


	/* Hack -- Assume there will be no blast (max radius 16) */
        for (dist = 0; dist < 64; dist++) gm[dist] = 0;


	/* Initial grid */
	y = y1;
	x = x1;
	dist = 0;

	/* Collect beam grids */
	if (flg & (PROJECT_BEAM))
	{
		gy[grids] = y;
		gx[grids] = x;
		grids++;
	}


	/* Calculate the projection path */
	path_n = project_path(path_g, MAX_RANGE, y1, x1, y2, x2, flg);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int oy = y;
		int ox = x;

		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Balls explode before reaching walls */
		if (!cave_floor_bold_project(ny, nx) && (rad > 0)) break;

		/* Advance */
		y = ny;
		x = nx;

		/* Collect beam grids */
		if (flg & (PROJECT_BEAM))
		{
			gy[grids] = y;
			gx[grids] = x;
			grids++;
		}

		/* Only do visuals if requested */
		if (!blind && !(flg & (PROJECT_HIDE)))
		{
			/* Only do visuals if the player can "see" the bolt */
			if (panel_contains(y, x) && player_has_los_bold(y, x))
			{
				u16b p;

				byte a;
				char c;

				/* Obtain the bolt pict */
				p = bolt_pict(oy, ox, y, x, typ);

				/* Extract attr/char */
				a = PICT_A(p);
				c = PICT_C(p);

				/* Visual effects */
				print_rel(c, a, y, x);
				move_cursor_relative(y, x);
				if (fresh_before) Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(y, x);
				if (fresh_before) Term_fresh();

				/* Display "beam" grids */
				if (flg & (PROJECT_BEAM))
				{
					/* Obtain the explosion pict */
					p = bolt_pict(y, x, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Visual effects */
					print_rel(c, a, y, x);
				}

				/* Hack -- Activate delay */
				visual = TRUE;
			}

			/* Hack -- delay anyway for consistency */
			else if (visual)
			{
				/* Delay for consistency */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}
	}


	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Start the "explosion" */
	gm[0] = 0;

	/* Hack -- make sure beams get to "explode" */
	gm[1] = grids;

	dist_hack = dist;

	/* Explode */
	if (TRUE)
	{
		/* Hack -- remove final beam grid */
		if (flg & (PROJECT_BEAM))
		{
			grids--;
		}

		/* Determine the blast area, work from the inside out */
		for (dist = 0; dist <= rad; dist++)
		{
			/* Scan the maximal blast area of radius "dist" */
			for (y = y2 - dist; y <= y2 + dist; y++)
			{
				for (x = x2 - dist; x <= x2 + dist; x++)
				{
					/* Ignore "illegal" locations */
					if (!in_bounds(y, x)) continue;

					/* Enforce a "circular" explosion */
					if (distance(y2, x2, y, x) != dist) continue;

					/* Ball explosions are stopped by walls */
					if (!los(y2, x2, y, x)) continue;

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


	/* Speed -- ignore "non-explosions" */
	if (!grids) return (FALSE);


	/* Display the "blast area" if requested */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Then do the "blast", from inside out */
		for (t = 0; t <= rad; t++)
		{
			/* Dump everything with this radius */
			for (i = gm[t]; i < gm[t+1]; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* Only do visuals if the player can "see" the blast */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
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
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush each "radius" seperately */
			if (fresh_before) Term_fresh();

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

				/* Hack -- Erase if needed */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					lite_spot(y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush the explosion */
			if (fresh_before) Term_fresh();
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


	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();


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
				monster_race *ref_ptr = &r_info[m_list[cave[y][x].m_idx].r_idx];

				if ((ref_ptr->flags2 & (RF2_REFLECTING)) && (randint(10)!=1)
				    && (dist_hack > 1))
				{
                                        int t_y, t_x;
					int max_attempts = 10;

					/* Choose 'new' target */
					do
					{
						t_y = y_saver - 1 + randint(3);
						t_x = x_saver - 1 + randint(3);
						max_attempts--;
					}

					while (max_attempts && in_bounds2(t_y, t_x) &&
					    !(los(y, x, t_y, t_x)));

					if (max_attempts < 1)
					{
						t_y = y_saver;
						t_x = x_saver;
					}

					if (m_list[cave[y][x].m_idx].ml)
					{
						msg_print("The attack bounces!");
						ref_ptr->r_flags2 |= RF2_REFLECTING;
					}

					project(cave[y][x].m_idx, 0, t_y, t_x,  dam, typ, flg);
				}
				else
				{
					if (project_m(who, dist, y, x, dam, typ)) notice = TRUE;
				}
			}
		}

		/* Player affected one monster (without "jumping") */
		if ((who < 0) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave[y][x].m_idx > 0)
			{
				monster_type *m_ptr = &m_list[cave[y][x].m_idx];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave[y][x].m_idx);
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

	/* Drop the ammo at the end. */
	if ((ranged_attack || throw_attack) && (dropnum > 0))
	{
		u32b f1, f2, f3, f4;
        	object_flags(drop_ranged, &f1, &f2, &f3, &f4);
		if (!(f4 & (TR4_RETURNING))) drop_near_ammo(drop_ranged, dropnum, y2, x2);
	}

	/* Return "something was noticed" */
	return (notice);
}

static const int destructive_attack_types[10] = {
  GF_KILL_WALL,
  GF_KILL_DOOR,
  GF_KILL_TRAP,
  GF_STONE_WALL,
  GF_MAKE_DOOR,
  GF_MAKE_TRAP,
  GF_DESTRUCTION,
  GF_DESTRUCTION,
  GF_DESTRUCTION,
  GF_DESTRUCTION,
};

static const int attack_types[25] = { 
  GF_MISSILE, 
  GF_MANA,
  GF_WATER, 
  GF_ACID, 
  GF_ELEC, 
  GF_FIRE, 
  GF_COLD, 
  GF_POIS, 
  GF_LITE, 
  GF_DARK, 
  GF_CONFUSION, 
  GF_SOUND, 
  GF_EARTH, 
  GF_CHAOS,
};

/*
 * Describe the attack using normal names. 
 */

void describe_attack_fully(int type, char* r) {
  switch (type) { 
  case GF_MISSILE:     strcpy(r, "magic missiles"); break;
  case GF_MANA:        strcpy(r, "mana"); break;
  case GF_WATER:       strcpy(r, "water"); break;
  case GF_ACID:        strcpy(r, "acid"); break;
  case GF_ELEC:        strcpy(r, "lightning"); break;
  case GF_FIRE:        strcpy(r, "flames"); break;
  case GF_COLD:        strcpy(r, "cold"); break;
  case GF_POIS:        strcpy(r, "poison"); break;
  case GF_LITE:        strcpy(r, "pure light"); break;
  case GF_DARK:        strcpy(r, "pure dark"); break;
  case GF_CONFUSION:   strcpy(r, "confusion"); break;
  case GF_SOUND:       strcpy(r, "sound"); break;
  case GF_EARTH:      strcpy(r, "shards"); break;
  case GF_CHAOS:       strcpy(r, "chaos"); break;
  case GF_KILL_WALL:   strcpy(r, "wall destruction"); break;
  case GF_KILL_DOOR:   strcpy(r, "door destruction"); break;
  case GF_KILL_TRAP:   strcpy(r, "trap destruction"); break;
  case GF_STONE_WALL:  strcpy(r, "wall creation"); break;
  case GF_MAKE_DOOR:   strcpy(r, "door creation"); break;
  case GF_MAKE_TRAP:   strcpy(r, "trap creation"); break;
  case GF_DESTRUCTION: strcpy(r, "destruction"); break;
    
  default:
    strcpy(r, "something unknown");
    break;
  }
}

/*
 * Give a randomly-generated spell a name.
 * Note that it only describes the first effect!
 */

static void name_spell(random_spell* s_ptr) {
  char buff[30];
  cptr buff2 = "???";

  if (s_ptr->proj_flags & PROJECT_STOP && s_ptr->radius == 0) {
    buff2 = "Bolt";

  } else if (s_ptr->proj_flags & PROJECT_BEAM) {
    buff2 = "Beam";

  } else if (s_ptr->proj_flags & PROJECT_STOP && s_ptr->radius > 0) {
    buff2 = "Ball";

  } else if (s_ptr->proj_flags & PROJECT_BLAST) {
    buff2 = "Blast";

  } else if (s_ptr->proj_flags & PROJECT_METEOR_SHOWER) {
    buff2 = "Area";

  } else if (s_ptr->proj_flags & PROJECT_VIEWABLE) {
    buff2 = "View";
  }

  describe_attack_fully(s_ptr->GF, buff);
  strnfmt(s_ptr->name, 30, "%s - %s", buff2, buff);
}

void generate_spell(int plev)
{
  random_spell* rspell;

  int dice, sides, chance, mana, power;

  bool destruc_gen = FALSE;
  bool simple_gen = TRUE;

  if (spell_num == MAX_SPELLS) return;

  rspell = &random_spells[spell_num];

  power = randnor(0, 10);

  dice = plev/5;
  sides = plev*2;
  mana = plev;

  /* Make the spell more or less powerful. */
  dice += power/5;
  sides += power/2;
  mana += (plev*power)/8;

  /* Stay within reasonable bounds. */
  if (dice < 1) dice = 1;
  if (dice > 10) dice = 10;

  if (sides < 1) sides = 1;
  if (sides > 100) sides = 100;

  if (mana < 1) mana = 1;

  rspell->level = plev;
  rspell->mana = mana;
  rspell->untried = TRUE;

  /* Spells are always maximally destructive. */
  rspell->proj_flags = PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID;

  chance = randint(100);

  /* Hack -- Always start with Magic Missile or derivative at lev. 1 */
  if (plev == 1 || chance < 25) {
    rspell->proj_flags |= PROJECT_STOP;
    rspell->dam_dice = dice;
    rspell->dam_sides = sides;
    rspell->radius = 0;

  } else if (chance < 50) {
    rspell->proj_flags |= PROJECT_BEAM;
    rspell->dam_dice = dice;
    rspell->dam_sides = sides;
    rspell->radius = 0;

  } else if (chance < 76) {
    rspell->proj_flags |= PROJECT_STOP;
    rspell->radius = dice;
    rspell->dam_dice = sides;
    rspell->dam_sides = 1;
    
  } else if (chance < 83) {
    rspell->proj_flags |= PROJECT_BLAST;
    rspell->radius = sides/3;
    rspell->dam_dice = dice;
    rspell->dam_sides = sides;

    destruc_gen = TRUE;
    simple_gen = FALSE;
      
  } else if (chance < 90) {
    rspell->proj_flags |= PROJECT_METEOR_SHOWER;
    rspell->dam_dice = dice;
    rspell->dam_sides = sides;
    rspell->radius = sides/3;
    if(rspell->radius < 4) rspell->radius = 4;

    destruc_gen = TRUE;

  } else {
    rspell->proj_flags |= PROJECT_VIEWABLE;
    rspell->dam_dice = dice;
    rspell->dam_sides = sides;
  }

  /* Both a destructive and a simple spell requested -- 
   * pick one or the other. */
  if (destruc_gen && simple_gen) {
    if (magik(25)) {
      simple_gen = FALSE;
    } else {
      destruc_gen = FALSE;
    }
  }

  /* Pick a simple spell */
  if (simple_gen) {
    rspell->GF = attack_types[rand_int(25)];

  /* Pick a destructive spell */
  } else {
    rspell->GF = destructive_attack_types[rand_int(10)];
  }

  /* Give the spell a name. */
  name_spell(rspell);
  sprintf(rspell->desc, "Damage: %dd%d, Power: %d", dice, sides, power);

  spell_num++;
}

/* Move the monster to a specific place */
void move_monster_spot(int m_idx, int xspot, int yspot)
{
        int ny, nx, oy, ox;

	monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Paranoia */
	if (!m_ptr->r_idx) return;

        /* Questors cannot be moved this way */
        if (r_ptr->flags1 & RF1_QUESTOR) return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

        ny = yspot;
        nx = xspot;

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

bool lord_piercing(int basechance, int factor, int typ, monster_type *m_ptr, int checktype)
{
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        if (typ != p_ptr->elemlord) return (FALSE);
        else
        {
                 int chance = basechance + (factor * (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 4] - 1));

                 if (randint(100) <= chance) return (TRUE);
        }
	/* Check type 0: elemental check */
	/* Check 1: Boss abilities check */
	/* If the resistance is negative, ALWAYS return true! */
	if (checktype == 0)
	{
		if (r_ptr->resistances[typ] < 0) return (TRUE);
	}

        /* Default */
        return (FALSE);
}

/* Teleport, but only on a lit grid. */
void teleport_away_light(int m_idx, int dis)
{
	int ny, nx, oy, ox, d, i, min;
	int tries = 0;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];

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
		tries++;

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
			if (!cave_empty_bold(ny, nx)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave[ny][nx].feat == FEAT_GLYPH) continue;
			if (cave[ny][nx].feat == FEAT_MINOR_GLYPH) continue;

			/* ...nor onto the Pattern */
			if ((cave[ny][nx].feat >= FEAT_PATTERN_START) &&
			    (cave[ny][nx].feat <= FEAT_PATTERN_XTRA2)) continue;

			/* No teleporting into vaults and such */
			if (!(p_ptr->inside_quest))
				if (cave[ny][nx].info & (CAVE_ICKY)) continue;

			/* Requires light. */
			if (!(cave[ny][nx].info & (CAVE_GLOW)) && !(cave[ny][nx].info & (CAVE_LITE)) && !(cave[ny][nx].info & (CAVE_MARK))) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;

		/* Stop after MAX_TRIES tries */
		if (tries > MAX_TRIES) return;
	}

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
