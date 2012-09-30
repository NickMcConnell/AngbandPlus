/* CVS: Last edit by $Author: rr9 $ on $Date: 2000/05/28 12:10:37 $ */
/* File: spells3.c */

/* Purpose: Spell code (part 3) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* Maximum number of tries for teleporting */
#define MAX_TRIES 100

/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_away(int m_idx, int dis)
{
	int ny=0, nx=0, oy, ox, d, i, min;
	int tries = 0;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];


	/* Paranoia */
	if (!m_ptr->r_idx) return (FALSE);

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	if ((((p_ptr->chp * 10) / p_ptr->mhp) < 5) &&
		(randint(5) > ((p_ptr->chp * 10) / p_ptr->mhp)))
	{
		chg_virtue(V_VALOUR, -1);
	}

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
			if (!(p_ptr->inside_quest || p_ptr->inside_arena))
				if (cave[ny][nx].info & CAVE_ICKY) continue;

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
		if (tries > MAX_TRIES) return (FALSE);
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

	return (TRUE);
}



/*
 * Teleport monster next to the player
 */
void teleport_to_player(int m_idx)
{
	int ny, nx, oy, ox, d, i, min;
	int attempts = 500;
	int dis = 2;
	bool look = TRUE;
	monster_type *m_ptr = &m_list[m_idx];


	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* "Skill" test */
	if (randint(100) > r_info[m_ptr->r_idx].level) return;

	/* Initialize */
	ny = m_ptr->fy;
	nx = m_ptr->fx;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

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
 *
 * When long-range teleport effects are considered, there is a nasty
 * tendency to "bounce" the player between two or three different spots
 * because these are the only spots that are "far enough" way to satisfy
 * the algorithm.  Therefore, if the teleport distance is more than 50,
 * we decrease the minimum acceptable distance to try to increase randomness.
 * -GJW
 */
void teleport_player(int dis)
{
	int d, i, min, ox, oy;
	int tries = 0;

	int xx = -1, yy = -1;

	/* Initialize */
	int y = py;
	int x = px;

	bool look = TRUE;

	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	if (dis > 200) dis = 200; /* To be on the safe side... */

	/* Minimum distance */
	min = dis / (dis > 50 ? 3 : 2);

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

			/* Require "naked" floor space or trees */
			if (!(cave_naked_bold(y, x) ||
			    (cave[y][x].feat == FEAT_TREES))) continue;

			/* No teleporting into vaults and such */
			if (cave[y][x].info & CAVE_ICKY) continue;

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

	/* Monsters with teleport ability may follow the player */
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
					if ((r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags6 & RF6_TPORT) &&
					    !(r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags3 & RF3_RES_TELE))
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
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
	/* No effect in arena or quest */
	if (p_ptr->inside_arena || p_ptr->inside_quest ||
	    (quest_number(dun_level) && (dun_level > 1) && ironman_downward))
	{
		msg_print("There is no effect.");
		return;
	}

	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	if (!dun_level || ironman_downward)
	{
		msg_print("You sink through the floor.");

		if (autosave_l) do_cmd_save_game(TRUE);

		dun_level++;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else if (quest_number(dun_level) || (dun_level >= MAX_DEPTH - 1))
	{
		msg_print("You rise up through the ceiling.");

		if (autosave_l) do_cmd_save_game(TRUE);

		dun_level--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else if (rand_int(100) < 50)
	{
		msg_print("You rise up through the ceiling.");

		if (autosave_l) do_cmd_save_game(TRUE);

		dun_level--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else
	{
		msg_print("You sink through the floor.");

		if (autosave_l) do_cmd_save_game(TRUE);

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
void recall_player(int turns)
{
	/*
	 * TODO: Recall the player to the last
	 * visited town when in the wilderness
	 */

	/* Ironman option */
	if (ironman_downward)
	{
		msg_print("Nothing happens.");
		return;
	}

	if (dun_level && (p_ptr->max_dlv > dun_level) && !p_ptr->inside_quest)
	{
		if (get_check("Reset recall depth? "))
			p_ptr->max_dlv = dun_level;

	}
	if (!p_ptr->word_recall)
	{
		p_ptr->word_recall = turns;
		msg_print("The air about you becomes charged...");
		p_ptr->redraw |= (PR_STATUS);
	}
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
		p_ptr->redraw |= (PR_STATUS);
	}
}


void word_of_recall(void)
{
	recall_player(rand_int(21) + 15);
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
	int             t = 0;
	object_type     *o_ptr;
	char            o_name[80];


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

	chg_virtue(V_HARMONY, 1);
	chg_virtue(V_ENCHANT, -2);

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
void apply_nexus(monster_type *m_ptr)
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
			if (rand_int(100) < p_ptr->skill_sav)
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
			if (rand_int(100) < p_ptr->skill_sav)
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


/*
 * Charge a lite (torch or latern)
 */
void phlogiston(void)
{
	int max_flog;
	object_type *o_ptr = &inventory[INVEN_LITE];

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


/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if (o_ptr->k_idx && !artifact_p(o_ptr) && !ego_item_p(o_ptr) &&
	    !o_ptr->art_name && !cursed_p(o_ptr))
	{
		cptr act;

		/* Let's get the name before it is changed... */
		char o_name[80];
		object_desc(o_name, o_ptr, FALSE, 0);

		switch (brand_type)
		{
		case 4:
			act = "seems very unstable now.";
			o_ptr->name2 = EGO_TRUMP;
			o_ptr->pval = randint(2);
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
			act = "is engulfed in raw Logrus!";
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

		chg_virtue(V_ENCHANT, -2);
	}
}


void call_the_(void)
{
	int i;

	if (cave_floor_bold(py - 1, px - 1) &&
	    cave_floor_bold(py - 1, px    ) &&
	    cave_floor_bold(py - 1, px + 1) &&
	    cave_floor_bold(py    , px - 1) &&
	    cave_floor_bold(py    , px + 1) &&
	    cave_floor_bold(py + 1, px - 1) &&
	    cave_floor_bold(py + 1, px    ) &&
	    cave_floor_bold(py + 1, px + 1))
	{
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_ROCKET, i, 175, 2);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_MANA, i, 175, 3);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_NUKE, i, 175, 4);
		}
	}
	else
	{
		msg_format("You %s the %s too close to a wall!",
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
			((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
		msg_print("There is a loud explosion!");

		if (destroy_area(py, px, 20 + p_ptr->lev, TRUE))
			msg_print("The dungeon collapses...");
		else
			msg_print("The dungeon trembles.");

		take_hit(100 + randint(150), "a suicidal Call the Void");
	}
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
	bool            flag;
	cave_type       *c_ptr;
	object_type     *o_ptr;
	char            o_name[80];

	/* Check to see if an object is already there */
	if (cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if (dir == 5 && target_okay())
	{
		tx = target_col;
		ty = target_row;

		if (distance(py, px, ty, tx) > MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}

		c_ptr = &cave[ty][tx];

		/* We need an item to fetch */
		if (!c_ptr->o_idx)
		{
			msg_print("There is no object at this place.");
			return;
		}

		/* No fetching from vault */
		if (c_ptr->info & CAVE_ICKY)
		{
			msg_print("The item slips from your control.");
			return;
		}

		/* We need to see the item */
		if (require_los && !player_has_los_bold(ty, tx))
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
		flag = FALSE;

		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];

			if ((distance(py, px, ty, tx) > MAX_RANGE) ||
			    !cave_floor_bold(ty, tx)) return;
		}
		while (!c_ptr->o_idx);
	}

	o_ptr = &o_list[c_ptr->o_idx];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}

	i = c_ptr->o_idx;
	c_ptr->o_idx = o_ptr->next_o_idx;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->next_o_idx = 0;
	o_ptr->iy = (byte)py;
	o_ptr->ix = (byte)px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s flies through the air to your feet.", o_name);

	note_spot(py, px);
	p_ptr->redraw |= PR_MAP;
}


void alter_reality(void)
{
	if (!quest_number(dun_level) && dun_level)
	{
		msg_print("The world changes!");

		if (autosave_l) do_cmd_save_game(TRUE);

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
	else
	{
		msg_print("The world seems to change for a moment!");
	}
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
bool warding_glyph(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return FALSE;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);

	return TRUE;
}


/*
 * Leave an "explosive rune" which prevents monster movement
 */
bool explosive_rune(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return FALSE;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_MINOR_GLYPH);

	return TRUE;
}


/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Identify it */
		identify_item(o_ptr);
	}
}


/*
 * Used by the "enchant" function (chance of failure)
 * (modified for Zangband, we need better stuff there...) -- TY
 */
static int enchant_table[16] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 650, 800,
	950, 987, 993, 995, 998,
	1000
};


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Heavily Cursed Items need a special spell */
		if (!all && (f3 & TR3_HEAVY_CURSE)) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & TR3_PERMA_CURSE) continue;

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		if (o_ptr->art_flags3 & TR3_CURSED)
			o_ptr->art_flags3 &= ~(TR3_CURSED);

		if (o_ptr->art_flags3 & TR3_HEAVY_CURSE)
			o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

		/* Take note */
		o_ptr->feeling = FEEL_UNCURSED;

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
}


/*
 * Turns an object into gold, gain some of its value in a shop
 */
bool alchemy(void)
{
	int item, amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[80];
	char out_val[160];

	cptr q, s;

	/* Hack -- force destruction */
	if (command_arg > 0) force = TRUE;

	/* Get an item */
	q = "Turn which item to gold? ";
	s = "You have nothing to turn to gold.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return FALSE;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (!(auto_destroy && (object_value(o_ptr) < 1)))
		{
			/* Make a verification */
			sprintf(out_val, "Really turn %s to gold? ", o_name);
			if (!get_check(out_val)) return FALSE;
		}
	}

	/* Check for artifacts */
	if (!can_player_destroy_object(o_ptr))
	{
		/* Message */
		msg_format("You fail to turn %s to gold!", o_name);

		/* Done */
		return FALSE;
	}

	price = object_value_real(o_ptr);

	if (price <= 0)
	{
		/* Message */
		msg_format("You turn %s to fool's gold.", o_name);
	}
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;
		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}


/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	/* Create a staircase */
	if (p_ptr->inside_arena || p_ptr->inside_quest)
	{
		/* arena or quest */
		msg_print("There is no effect!");
	}
	else if (!dun_level || ironman_downward)
	{
		/* Town/wilderness or Ironman */
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (quest_number(dun_level) || (dun_level >= MAX_DEPTH - 1))
	{
		/* Quest level */
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if (rand_int(100) < 50)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Check if an object is weapon or armour (but not arrow, bolt, or shot)
 */
bool item_tester_hook_weapon_armour(object_type *o_ptr)
{
	return (item_tester_hook_weapon(o_ptr) ||
	        item_tester_hook_armour(o_ptr));
}


/*
 * Break the curse of an item
 */
static void break_curse(object_type *o_ptr)
{
	u32b    f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (cursed_p(o_ptr) && !(f3 & TR3_PERMA_CURSE) && (rand_int(100) < 25))
	{
		msg_print("The curse is broken!");

		o_ptr->ident &= ~(IDENT_CURSED);
		o_ptr->ident |= (IDENT_SENSE);

		if (o_ptr->art_flags3 & TR3_CURSED)
			o_ptr->art_flags3 &= ~(TR3_CURSED);
		if (o_ptr->art_flags3 & TR3_HEAVY_CURSE)
			o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

		o_ptr->feeling = FEEL_UNCURSED;
	}
}


/*
 * Enchants a plus onto an item. -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item. -CFT-
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int     i, chance, prob;
	bool    res = FALSE;
	bool    a = (artifact_p(o_ptr) || o_ptr->art_name);
	bool    force = (eflag & ENCH_FORCE);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) ||
	    (o_ptr->tval == TV_ARROW) ||
	    (o_ptr->tval == TV_SHOT))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i = 0; i < n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (!force && rand_int(prob) >= 100) continue;

		/* Enchant to hit */
		if (eflag & ENCH_TOHIT)
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			if (force || ((randint(1000) > chance) && (!a || (rand_int(100) < 50))))
			{
				o_ptr->to_h++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_h >= 0)
					break_curse(o_ptr);
			}
		}

		/* Enchant to damage */
		if (eflag & ENCH_TODAM)
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			if (force || ((randint(1000) > chance) && (!a || (rand_int(100) < 50))))
			{
				o_ptr->to_d++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_d >= 0)
					break_curse(o_ptr);
			}
		}

		/* Enchant to armor class */
		if (eflag & ENCH_TOAC)
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			if (force || ((randint(1000) > chance) && (!a || (rand_int(100) < 50))))
			{
				o_ptr->to_a++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_a >= 0)
					break_curse(o_ptr);
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Success */
	return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	int         item;
	bool        okay = FALSE;
	object_type *o_ptr;
	char        o_name[80];
	cptr        q, s;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Describe */
	msg_format("%s %s glow%s brightly!",
		   ((item >= 0) ? "Your" : "The"), o_name,
		   ((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");

		if (randint(3) == 1) chg_virtue(V_ENCHANT, -1);
	}
	else
		chg_virtue(V_ENCHANT, 1);

	/* Something happened */
	return (TRUE);
}


bool artifact_scroll(void)
{
	int             item;
	bool            okay;
	object_type     *o_ptr;
	char            o_name[80];
	cptr            q, s;


	/* Enchant weapon/armour */
	item_tester_hook = item_tester_hook_weapon_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Describe */
	msg_format("%s %s radiate%s a blinding light!",
	          ((item >= 0) ? "Your" : "The"), o_name,
	          ((o_ptr->number > 1) ? "" : "s"));

	/* No artifact creation of Dragon Scale Mail */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* ToDo: Maybe allow some of the DSMs to be enchanted */
		msg_format("The %s %s already magical!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"));

		okay = FALSE;
	}

	else if (o_ptr->name1 || o_ptr->art_name)
	{
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "artifacts" : "an artifact"));
		okay = FALSE;
	}

	else if (o_ptr->name2)
	{
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "ego items" : "an ego item"));
		okay = FALSE;
	}

	else
	{
		if (o_ptr->number > 1)
		{
			msg_print("Not enough enough energy to enchant more than one object!");
			msg_format("%d of your %s %s destroyed!", (o_ptr->number) - 1,
			           o_name, ((o_ptr->number > 2) ? "were" : "was"));
			o_ptr->number = 1;
		}
		okay = create_artifact(o_ptr, TRUE);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
		if (randint(3) == 1) chg_virtue(V_ENCHANT, -1);
	}
	else
		chg_virtue(V_ENCHANT, 1);

	/* Something happened */
	return (TRUE);
}


/*
 * Apply good luck to an object
 */
static void good_luck(object_type *o_ptr)
{
	/* Objects become better sometimes */
	if (!rand_int(13))
	{
		int number = o_ptr->number;

		bool great = ego_item_p(o_ptr);

		/* Prepare it */
		object_prep(o_ptr, o_ptr->k_idx);

		/* Restore the number */
		o_ptr->number = number;

		/* Apply good magic (allow artifacts, good, great if an ego-item, no curse) */
		apply_magic(o_ptr, dun_level, TRUE, TRUE, great, FALSE);
	}

	/* Objects duplicate sometimes */
	if (!rand_int(777) && (o_ptr->number < 99))
	{
		o_ptr->number++;
	}
}


/*
 * Apply bad luck to an object
 */
static void bad_luck(object_type *o_ptr)
{
	bool is_art = artifact_p(o_ptr) || o_ptr->art_name;

	/* Objects become worse sometimes */
	if (!rand_int(13))
	{
		int number = o_ptr->number;

		bool great = ego_item_p(o_ptr);

		/* Non-artifacts get rerolled */
		if (!is_art)
		{
			o_ptr->ident |= IDENT_CURSED;

			/* Prepare it */
			object_prep(o_ptr, o_ptr->k_idx);

			/* Restore the number */
			o_ptr->number = number;

			/* Apply bad magic (disallow artifacts, good, great if an ego-item, cursed) */
			apply_magic(o_ptr, dun_level, FALSE, TRUE, great, TRUE);
		}

		/* Now curse it */
		o_ptr->ident |= IDENT_CURSED;
	}

	/* Objects are blasted sometimes */
	if (!rand_int(666) && (!is_art || !rand_int(3)))
	{
		/* Blast it */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		if (o_ptr->to_a) o_ptr->to_a = 0 - randint(5) - randint(5);
		if (o_ptr->to_h) o_ptr->to_h = 0 - randint(5) - randint(5);
		if (o_ptr->to_d) o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}
}


/*
 * Identify an object
 */
void identify_item(object_type *o_ptr)
{
	if ((p_ptr->muta3 & MUT3_GOOD_LUCK) &&
		 !artifact_p(o_ptr) && !o_ptr->art_name && !object_known_p(o_ptr))
	{
		good_luck(o_ptr);
	}

	if (p_ptr->muta3 & MUT3_BAD_LUCK)
	{
		bad_luck(o_ptr);
	}

	if (!(o_ptr->ident & (IDENT_MENTAL)))
	{
		if ((o_ptr->art_name) || (artifact_p(o_ptr)))
			chg_virtue(V_KNOWLEDGE, 3);
		else
			chg_virtue(V_KNOWLEDGE, 1);
	}

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	int             item;
	object_type     *o_ptr;
	char            o_name[80];
	cptr            q, s;


	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* Identify it */
	identify_item(o_ptr);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			   describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
			   o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
			   o_name);
	}

	/*
	 * If the item was an artifact, and if the
	 * auto-note is selected, write a message.
	 */
	if (auto_notes && take_notes && (artifact_p(o_ptr) || o_ptr->art_name) && a_info[o_ptr->name1].cur_num != 2)
	{
		char note[80];
		char item_name[80];
		object_desc(item_name, o_ptr, FALSE, 0);

		/* Build note and write */
		sprintf(note, "Found The %s", item_name);

		add_note(note, 'A');

		/* Mark item as found */
		a_info[o_ptr->name1].cur_num = 2;
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Mundanify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was mundanified, else FALSE.
 */
bool mundane_spell(void)
{
	int             item;
	object_type     *o_ptr;
	object_kind     *k_ptr;
	cptr            q, s;


	/* Get an item */
	q = "Use which item? ";
	s = "You have nothing you can use.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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

	k_ptr = &k_info[o_ptr->k_idx];

	/* Oops */
	msg_print("There is a bright flash of light!");

	/* No discount */
	o_ptr->discount = 0;

	/* No extra info */
	o_ptr->xtra1 = 0;
	o_ptr->xtra2 = 0;

	/* No artifact name (random artifacts) */
	o_ptr->art_name = 0;

	/* Not identified yet */
	o_ptr->ident = 0;

	/* Erase the inscription */
	o_ptr->inscription = 0;

	/* Erase the "feeling" */
	o_ptr->feeling = FEEL_NONE;

	/* Default "pval" */
	o_ptr->pval = k_ptr->pval;

	/* Default weight */
	o_ptr->weight = k_ptr->weight;

	/* Default magic */
	o_ptr->to_h = k_ptr->to_h;
	o_ptr->to_d = k_ptr->to_d;
	o_ptr->to_a = k_ptr->to_a;

	/* No longer artifact / ego item */
	o_ptr->name1 = 0;
	o_ptr->name2 = 0;

	/* Default power */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* No artifact powers */
	o_ptr->art_flags1 = 0;
	o_ptr->art_flags2 = 0;
	o_ptr->art_flags3 = 0;

	/* For rod-stacking */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->timeout = o_ptr->pval * o_ptr->number;
		o_ptr->pval = k_ptr->pval * o_ptr->number;
	}

	/* Hack -- worthless items are always "broken" */
	if (get_object_cost(o_ptr) <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

	/* Something happened */
	return (TRUE);
}



/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	int             item;
	object_type     *o_ptr;
	char            o_name[80];
	cptr            q, s;


	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Identify it */
	identify_item(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			   describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
			   o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
			   o_name);
	}

	/*
	 * If the item was an artifact, and if the
	 * auto-note is selected, write a message (if not written before).
	 */
	if (auto_notes && take_notes && (artifact_p(o_ptr) || o_ptr->art_name) && a_info[o_ptr->name1].cur_num != 2)
	{
		char note[80];
		char item_name[80];
		object_desc(item_name, o_ptr, FALSE, 0);

		/* Build note and write */
		sprintf(note, "Found The %s", item_name);

		add_note(note, 'A');

		/* Mark item as found */
		a_info[o_ptr->name1].cur_num = 2;
	}

	/* Describe it fully */
	(void)identify_fully_aux(o_ptr);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool item_tester_hook_recharge(object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 * This function has been rewritten in Oangband and ZAngband.
 *
 * Sorcery/Arcane -- Recharge  --> recharge(plev * 4)
 * Chaos -- Arcane Binding     --> recharge(90)
 *
 * Scroll of recharging        --> recharge(130)
 * Artifact activation/Thingol --> recharge(130)
 *
 * It is harder to recharge high level, and highly charged wands,
 * staffs, and rods.  The more wands in a stack, the more easily and
 * strongly they recharge.  Staffs, however, each get fewer charges if
 * stacked.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 */
bool recharge(int power)
{
	int item, lev;
	int recharge_strength, recharge_amount;

	object_type *o_ptr;
	object_kind *k_ptr;

	bool fail = FALSE;
	byte fail_type = 1;

	cptr q, s;
	char o_name[80];


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;


	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((power > lev) ? (power - lev) : 0) / 5;


		/* Back-fire */
		if (rand_int(recharge_strength) == 0)
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* Recharge */
		else
		{
			/* Recharge amount */
			recharge_amount = (power * damroll(3, 2));

			/* Recharge by that amount */
			if (o_ptr->timeout > recharge_amount)
				o_ptr->timeout -= recharge_amount;
			else
				o_ptr->timeout = 0;
		}
	}


	/* Recharge wand/staff */
	else
	{
		/* Extract a recharge strength by comparing object level to power.
		 * Divide up a stack of wands' charges to calculate charge penalty.
		 */
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			recharge_strength = (100 + power - lev -
			(8 * o_ptr->pval / o_ptr->number)) / 15;

		/* All staffs, unstacked wands. */
		else recharge_strength = (100 + power - lev -
			(8 * o_ptr->pval)) / 15;

		/* Paranoia */
		if (recharge_strength < 0) recharge_strength = 0;

		/* Back-fire */
		if (rand_int(recharge_strength) == 0)
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* If the spell didn't backfire, recharge the wand or staff. */
		else
		{
			/* Recharge based on the standard number of charges. */
			recharge_amount = randint(1 + k_ptr->pval / 2);

			/* Multiple wands in a stack increase recharging somewhat. */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			{
				recharge_amount +=
					(randint(recharge_amount * (o_ptr->number - 1))) / 2;
				if (recharge_amount < 1) recharge_amount = 1;
				if (recharge_amount > 12) recharge_amount = 12;
			}

			/* But each staff in a stack gets fewer additional charges,
			 * although always at least one.
			 */
			if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			{
				recharge_amount /= o_ptr->number;
				if (recharge_amount < 1) recharge_amount = 1;
			}

			/* Recharge the wand or staff. */
			o_ptr->pval += recharge_amount;


			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}


	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Artifacts are never destroyed. */
		if (artifact_p(o_ptr))
		{
			object_desc(o_name, o_ptr, TRUE, 0);
			msg_format("The recharging backfires - %s is completely drained!", o_name);

			/* Artifact rods. */
			if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < 10000))
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;

			/* Artifact wands and staffs. */
			else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
				o_ptr->pval = 0;
		}
		else
		{
			/* Get the object description */
			object_desc(o_name, o_ptr, FALSE, 0);

			/*** Determine Seriousness of Failure ***/

			/* Mages recharge objects more safely. */
			if (p_ptr->pclass == CLASS_MAGE)
			{
				/* 10% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (randint(10) == 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 75% chance to blow up one wand, otherwise draining. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (randint(3) != 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 50% chance to blow up one staff, otherwise no effect. */
				else if (o_ptr->tval == TV_STAFF)
				{
					if (randint(2) == 1) fail_type = 2;
					else fail_type = 0;
				}
			}

			/* All other classes get no special favors. */
			else
			{
				/* 33% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (randint(3) == 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 20% chance of the entire stack, else destroy one wand. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (randint(5) == 1) fail_type = 3;
					else fail_type = 2;
				}
				/* Blow up one staff. */
				else if (o_ptr->tval == TV_STAFF)
				{
					fail_type = 2;
				}
			}

			/*** Apply draining and destruction. ***/

			/* Drain object or stack of objects. */
			if (fail_type == 1)
			{
				if (o_ptr->tval == TV_ROD)
				{
					msg_print("The recharge backfires, draining the rod further!");
					if (o_ptr->timeout < 10000)
						o_ptr->timeout = (o_ptr->timeout + 100) * 2;
				}
				else if (o_ptr->tval == TV_WAND)
				{
					msg_format("You save your %s from destruction, but all charges are lost.", o_name);
					o_ptr->pval = 0;
				}
				/* Staffs aren't drained. */
			}

			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
					msg_format("Wild magic consumes one of your %s!", o_name);
				else
					msg_format("Wild magic consumes your %s!", o_name);

				/* Reduce rod stack maximum timeout, drain wands. */
				if (o_ptr->tval == TV_ROD) o_ptr->pval -= k_ptr->pval;
				if (o_ptr->tval == TV_WAND) o_ptr->pval = 0;

				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}

			/* Destroy all members of a stack of objects. */
			if (fail_type == 3)
			{
				if (o_ptr->number > 1)
					msg_format("Wild magic consumes all your %s!", o_name);
				else
					msg_format("Wild magic consumes your %s!", o_name);


				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -999);
					inven_item_describe(item);
					inven_item_optimize(item);
				}

				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -999);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}


/*
 * Bless a weapon
 */
bool bless_weapon(void)
{
	int             item;
	object_type     *o_ptr;
	u32b            f1, f2, f3;
	char            o_name[80];
	cptr            q, s;

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get an item */
	q = "Bless which weapon? ";
	s = "You have weapon to bless.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return FALSE;

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

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (o_ptr->ident & IDENT_CURSED)
	{
		if (((f3 & TR3_HEAVY_CURSE) && (randint(100) < 33)) ||
		    (f3 & TR3_PERMA_CURSE))
		{
			msg_format("The black aura on %s %s disrupts the blessing!",
			    ((item >= 0) ? "your" : "the"), o_name);
			return TRUE;
		}

		msg_format("A malignant aura leaves %s %s.",
		    ((item >= 0) ? "your" : "the"), o_name);

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Take note */
		o_ptr->feeling = FEEL_UNCURSED;

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/*
	 * Next, we try to bless it. Artifacts have a 1/3 chance of
	 * being blessed, otherwise, the operation simply disenchants
	 * them, godly power negating the magic. Ok, the explanation
	 * is silly, but otherwise priests would always bless every
	 * artifact weapon they find. Ego weapons and normal weapons
	 * can be blessed automatically.
	 */
	if (f3 & TR3_BLESSED)
	{
		msg_format("%s %s %s blessed already.",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "were" : "was"));
		return TRUE;
	}

	if (!(o_ptr->art_name || o_ptr->name1) || (randint(3) == 1))
	{
		/* Describe */
		msg_format("%s %s shine%s!",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "" : "s"));
		o_ptr->art_flags3 |= TR3_BLESSED;
	}
	else
	{
		bool dis_happened = FALSE;

		msg_print("The artifact resists your blessing!");

		/* Disenchant tohit */
		if (o_ptr->to_h > 0)
		{
			o_ptr->to_h--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_h > 5) && (rand_int(100) < 33)) o_ptr->to_h--;

		/* Disenchant todam */
		if (o_ptr->to_d > 0)
		{
			o_ptr->to_d--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_d > 5) && (rand_int(100) < 33)) o_ptr->to_d--;

		/* Disenchant toac */
		if (o_ptr->to_a > 0)
		{
			o_ptr->to_a--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_a > 5) && (rand_int(100) < 33)) o_ptr->to_a--;

		if (dis_happened)
		{
			msg_print("There is a static feeling in the air...");
			msg_format("%s %s %s disenchanted!",
			    ((item >= 0) ? "Your" : "The"), o_name,
			    ((o_ptr->number > 1) ? "were" : "was"));
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	return TRUE;
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
 *          potions that smash on the floor are assumed to
 *          be caused by no-one (who = 1), as are those that
 *          shatter inside the player inventory.
 *          (Not anymore -- I changed this; TY)
 *    y, x  --- coordinates of the potion (or player if
 *          the potion was in her inventory);
 *    o_ptr --- pointer to the potion object.
 */
bool potion_smash_effect(int who, int y, int x, int k_idx)
{
	int     radius = 2;
	int     dt = 0;
	int     dam = 0;
	bool    ident = FALSE;
	bool    angry = FALSE;

	object_kind *k_ptr = &k_info[k_idx];

	switch (k_ptr->sval)
	{
		case SV_POTION_SALT_WATER:
		case SV_POTION_SLIME_MOLD:
		case SV_POTION_LOSE_MEMORIES:
		case SV_POTION_DEC_STR:
		case SV_POTION_DEC_INT:
		case SV_POTION_DEC_WIS:
		case SV_POTION_DEC_DEX:
		case SV_POTION_DEC_CON:
		case SV_POTION_DEC_CHR:
		case SV_POTION_WATER:   /* perhaps a 'water' attack? */
		case SV_POTION_APPLE_JUICE:
			return TRUE;

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
		case SV_POTION_RES_CHR:
		case SV_POTION_INC_STR:
		case SV_POTION_INC_INT:
		case SV_POTION_INC_WIS:
		case SV_POTION_INC_DEX:
		case SV_POTION_INC_CON:
		case SV_POTION_INC_CHR:
		case SV_POTION_AUGMENTATION:
		case SV_POTION_ENLIGHTENMENT:
		case SV_POTION_STAR_ENLIGHTENMENT:
		case SV_POTION_SELF_KNOWLEDGE:
		case SV_POTION_EXPERIENCE:
		case SV_POTION_RESISTANCE:
		case SV_POTION_INVULNERABILITY:
		case SV_POTION_NEW_LIFE:
			/* All of the above potions have no effect when shattered */
			return FALSE;
		case SV_POTION_SLOWNESS:
			dt = GF_OLD_SLOW;
			dam = 5;
			ident = TRUE;
			angry = TRUE;
			break;
		case SV_POTION_POISON:
			dt = GF_POIS;
			dam = 3;
			ident = TRUE;
			angry = TRUE;
			break;
		case SV_POTION_BLINDNESS:
			dt = GF_DARK;
			ident = TRUE;
			angry = TRUE;
			break;
		case SV_POTION_CONFUSION: /* Booze */
			dt = GF_OLD_CONF;
			ident = TRUE;
			angry = TRUE;
			break;
		case SV_POTION_SLEEP:
			dt = GF_OLD_SLEEP;
			angry = TRUE;
			ident = TRUE;
			break;
		case SV_POTION_RUINATION:
		case SV_POTION_DETONATIONS:
			dt = GF_SHARDS;
			dam = damroll(25, 25);
			angry = TRUE;
			ident = TRUE;
			break;
		case SV_POTION_DEATH:
			dt = GF_DEATH_RAY;    /* !! */
			dam = k_ptr->level * 10;
			angry = TRUE;
			radius = 1;
			ident = TRUE;
			break;
		case SV_POTION_SPEED:
			dt = GF_OLD_SPEED;
			ident = TRUE;
			break;
		case SV_POTION_CURE_LIGHT:
			dt = GF_OLD_HEAL;
			dam = damroll(2, 3);
			ident = TRUE;
			break;
		case SV_POTION_CURE_SERIOUS:
			dt = GF_OLD_HEAL;
			dam = damroll(4, 3);
			ident = TRUE;
			break;
		case SV_POTION_CURE_CRITICAL:
		case SV_POTION_CURING:
			dt = GF_OLD_HEAL;
			dam = damroll(6, 3);
			ident = TRUE;
			break;
		case SV_POTION_HEALING:
			dt = GF_OLD_HEAL;
			dam = damroll(10, 10);
			ident = TRUE;
			break;
		case SV_POTION_STAR_HEALING:
		case SV_POTION_LIFE:
			dt = GF_OLD_HEAL;
			dam = damroll(50, 50);
			radius = 1;
			ident = TRUE;
			break;
		case SV_POTION_RESTORE_MANA:   /* MANA */
			dt = GF_MANA;
			dam = damroll(10, 10);
			radius = 1;
			ident = TRUE;
			break;
		default:
			/* Do nothing */  ;
	}

	(void)project(who, radius, y, x, dam, dt,
	    (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));

	/* An identification was made */
	if (ident && !(k_ptr->aware))
	{
		k_ptr->aware = TRUE;
		gain_exp((k_ptr->level + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	return angry;
}


/*
 * Hack -- Display all known spells in a window
 *
 * XXX XXX XXX Need to analyze size of the window.
 *
 * XXX XXX XXX Need more color coding.
 */
void display_spell_list(void)
{
	int             i, j;
	int             y, x;
	int             use_realm1 = p_ptr->realm1 - 1;
	int             use_realm2 = p_ptr->realm2 - 1;
	int             m[9];
	magic_type      *s_ptr;
	char            name[80];
	char            out_val[160];


	/* Erase window */
	clear_from(0);

	/* Warriors are illiterate */
	if (!mp_ptr->spell_book) return;

	/* Mindcrafter spell-list */
	if (p_ptr->pclass == CLASS_MINDCRAFTER)
	{
		int             i;
		int             y = 1;
		int             x = 1;
		int             minfail;
		int             plev = p_ptr->lev;
		int             chance;
		mindcraft_power spell;
		char            comment[80];
		char            psi_desc[80];

		/* Display a list of spells */
		prt("", y, x);
		put_str("Name", y, x + 5);
		put_str("Lv Mana Fail Info", y, x + 35);

		/* Dump the spells */
		for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
		{
			byte a = TERM_WHITE;

			/* Access the available spell */
			spell = mindcraft_powers[i];
			if (spell.min_lev > plev) break;

			/* Get the failure rate */
			chance = spell.fail;

			/* Reduce failure rate by "effective" level adjustment */
			chance -= 3 * (p_ptr->lev - spell.min_lev);

			/* Reduce failure rate by INT/WIS adjustment */
			chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

			/* Not enough mana to cast */
			if (spell.mana_cost > p_ptr->csp)
			{
				chance += 5 * (spell.mana_cost - p_ptr->csp);
				a = TERM_ORANGE;
			}

			/* Extract the minimum failure rate */
			minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

			/* Minimum failure rate */
			if (chance < minfail) chance = minfail;

			/* Stunning makes spells harder */
			if (p_ptr->stun > 50) chance += 25;
			else if (p_ptr->stun) chance += 15;

			/* Always a 5 percent chance of working */
			if (chance > 95) chance = 95;

			/* Get info */
			mindcraft_info(comment, i);

			/* Dump the spell */
			sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
			    I2A(i), spell.name,
			    spell.min_lev, spell.mana_cost, chance, comment);
			Term_putstr(x, y + i + 1, -1, a, psi_desc);
		}
		return;
	}

	/* Normal spellcaster with books */

	/* Scan books */
	for (j = 0; j < ((use_realm2 > -1) ? 2 : 1); j++)
	{
		int n = 0;

		/* Reset vertical */
		m[j] = 0;

		/* Vertical location */
		y = (j < 3) ? 0 : (m[j - 3] + 2);

		/* Horizontal location */
		x = 27 * (j % 3);

		/* Scan spells */
		for (i = 0; i < 32; i++)
		{
			byte a = TERM_WHITE;

			/* Access the spell */
			s_ptr = &mp_ptr->info[(j < 1) ? use_realm1 : use_realm2][i % 32];

			strcpy(name, spell_names[(j < 1) ? use_realm1 : use_realm2][i % 32]);

			/* Illegible */
			if (s_ptr->slevel >= 99)
			{
				/* Illegible */
				strcpy(name, "(illegible)");

				/* Unusable */
				a = TERM_L_DARK;
			}

			/* Forgotten */
			else if ((j < 1) ?
				((spell_forgotten1 & (1L << i))) :
				((spell_forgotten2 & (1L << (i % 32)))))
			{
				/* Forgotten */
				a = TERM_ORANGE;
			}

			/* Unknown */
			else if (!((j < 1) ?
				(spell_learned1 & (1L << i)) :
				(spell_learned2 & (1L << (i % 32)))))
			{
				/* Unknown */
				a = TERM_RED;
			}

			/* Untried */
			else if (!((j < 1) ?
				(spell_worked1 & (1L << i)) :
				(spell_worked2 & (1L << (i % 32)))))
			{
				/* Untried */
				a = TERM_YELLOW;
			}

			/* Dump the spell --(-- */
			sprintf(out_val, "%c/%c) %-20.20s",
				I2A(n / 8), I2A(n % 8), name);

			/* Track maximum */
			m[j] = y + n;

			/* Dump onto the window */
			Term_putstr(x, m[j], -1, a, out_val);

			/* Next */
			n++;
		}
	}
}



/*
 * Returns spell chance of failure for spell -RAK-
 */
s16b spell_chance(int spell, int realm)
{
	int             chance, minfail;
	magic_type      *s_ptr;


	/* Paranoia -- must be literate */
	if (!mp_ptr->spell_book) return (100);

	/* Access the spell */
	s_ptr = &mp_ptr->info[realm][spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

	/*
	 * Non mage/priest characters never get too good
	 * (added high mage, mindcrafter)
	 */
	if ((p_ptr->pclass != CLASS_PRIEST) &&
	    (p_ptr->pclass != CLASS_MAGE) &&
	    (p_ptr->pclass != CLASS_MINDCRAFTER) &&
	    (p_ptr->pclass != CLASS_HIGH_MAGE))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
	if ((p_ptr->pclass == CLASS_PRIEST) && p_ptr->icky_wield) chance += 25;

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}



/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known, int realm)
{
	magic_type *s_ptr;

	/* Access the spell */
	s_ptr = &mp_ptr->info[realm][spell];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if ((realm == p_ptr->realm2 - 1) ?
	    (spell_forgotten2 & (1L << spell)) :
	    (spell_forgotten1 & (1L << spell)))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if ((realm == p_ptr->realm2 - 1) ?
	    (spell_learned2 & (1L << spell)) :
	    (spell_learned1 & (1L << spell)))
	{
		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}



/*
 * Extra information on a spell -DRS-
 *
 * We can use up to 14 characters of the buffer 'p'
 *
 * The strings in this function were extracted from the code in the
 * functions "do_cmd_cast()" and "do_cmd_pray()" and may be dated.
 */
static void spell_info(char *p, int spell, int realm)
{
	/* Default */
	strcpy(p, "");

#ifdef DRS_SHOW_SPELL_INFO
	{
		int plev = p_ptr->lev;

		/* See below */
		int orb = (plev / ((p_ptr->pclass == CLASS_PRIEST ||
		                    p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4));

		/* Analyze the spell */
		switch (realm)
		{
			case 0: /* Life */
				switch (spell)
				{
					case  1: strcpy (p, " heal 2d10"); break;
					case  2: strcpy (p, " dur 12+d12 turns"); break;
					case  4: sprintf(p, " dam %d", 10 + (plev / 2)); break;
					case  6: strcpy (p, " heal 4d10"); break;
					case 10: strcpy (p, " heal 8d10"); break;
					case 11: strcpy (p, " dur 24+d24"); break;
					case 12: sprintf(p, " dam 3d6+%d", plev + orb); break;
					case 13: sprintf(p, " dur d25+%d", 3 * plev); break;
					case 14: strcpy (p, " heal 300"); break;
					case 16: sprintf(p, " dam %d+%d", plev, plev); break;
					case 18: sprintf(p, " dam %d+%d", 3 * plev, 3 * plev); break;
					case 20: sprintf(p, " dam %d", 4 * plev); break;
					case 22: sprintf(p, " d %d/h 1000", 4 * plev); break;
					case 24: strcpy (p, " dur 25+d25"); break;
					case 25: strcpy (p, " dur 48+d48"); break;
					case 28: strcpy (p, " heal 2000"); break;
					case 30: sprintf(p, " h300/d%d+388", plev * 4); break;
					case 31: strcpy (p, " dur 7+d7"); break;
				}
				break;

			case 1: /* Sorcery */
				switch (spell)
				{
					case  1: strcpy (p, " range 10"); break;
					case  3: sprintf(p, " dam %d", 10 + (plev / 2)); break;
					case  5: sprintf(p, " range %d", plev * 5); break;
					case 13: sprintf(p, " dur %d+d%d", plev, plev + 20); break;
					case 19: sprintf(p, " range %d", plev + 2); break;
					case 20: strcpy (p, " dur 25+d30"); break;
					case 23: strcpy (p, " delay 15+d21"); break;
					case 25: sprintf(p, " max wgt %d", plev * 15 / 10); break;
					case 26: sprintf(p, " dam 7d7+%d", plev / 2); break;
					case 27: strcpy (p, " dur 25+d30"); break;
					case 31: strcpy (p, " dur 8+d8"); break;
				}
				break;

			case 2: /* Nature */
				switch (spell)
				{
					case  1: strcpy (p, " heal 2d8"); break;
					case  4: sprintf(p, " dam %d", 10 + (plev / 2)); break;
					case  6: strcpy (p, " dur 20+d20"); break;
					case  9: sprintf(p, " dam %dd8", (3 + ((plev - 5) / 4))); break;
					case 11: sprintf(p, " dam %dd8", (5 + ((plev - 5) / 4))); break;
					case 12: strcpy (p, " dam 6d8"); break;
					case 15: strcpy (p, " heal 1000"); break;
					case 18: strcpy (p, " dur 20+d30"); break;
					case 19: strcpy (p, " dur 20+d20"); break;
					case 24: strcpy (p, " rad 10"); break;
					case 26: sprintf(p, " dam %d", 70 + plev); break;
					case 27: sprintf(p, " dam %d", 90 + plev); break;
					case 28: sprintf(p, " dam %d", 100 + plev); break;
					case 29: strcpy (p, " dam 75"); break;
					case 31: sprintf(p, " dam %d+%d", 4 * plev, 100 + plev); break;
				}
				break;

			case 3: /* Chaos */
				switch (spell)
				{
					case  0: sprintf(p, " dam %dd4", 3 + ((plev - 1) / 5)); break;
					case  2: sprintf(p, " dam %d", 10 + (plev / 2)); break;
					case  4: sprintf(p, " dam 3d5+%d", plev + (plev /
					     (((p_ptr->pclass == CLASS_MAGE) ||
					     (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))); break;
					case  5: sprintf(p, " dam %dd8", (6 + ((plev - 5) / 4))); break;
					case  6: sprintf(p, " dam %dd8", (8 + ((plev - 5) / 4))); break;
					case  7: sprintf(p, " range %d", plev * 5); break;
					case  8: strcpy (p, " random"); break;
					case  9: sprintf(p, " dam %dd8", (10 + ((plev - 5) / 4))); break;
					case 10: sprintf(p, " dam %d", 45 + plev); break;
					case 11: sprintf(p, " dam %dd8", (11 + ((plev - 5) / 4))); break;
					case 12: sprintf(p, " dam %d", 55 + plev); break;
					case 15: sprintf(p, " dam %d", 66 + plev); break;
					case 17: sprintf(p, " dam %dd8", (5 + (plev / 10))); break;
					case 19: sprintf(p, " dam %d", 80 + plev); break;
					case 24: sprintf(p, " dam %dd8", (9 + (plev / 10))); break;
					case 25: sprintf(p, " dam %d each", (3 * plev) / 2); break;
					case 26: sprintf(p, " dam %d", 75 + plev); break;
					case 27: strcpy (p, " dam 75 / 150"); break;
					case 28: sprintf(p, " dam %d", 120 + plev); break;
					case 29: sprintf(p, " dam %d", 300 + (plev * 2)); break;
					case 30: sprintf(p, " dam %d", p_ptr->chp); break;
					case 31: strcpy (p, " dam 3 * 175"); break;
				}
				break;

			case 4: /* Death */
				switch (spell)
				{
					case  1: sprintf(p, " dam %dd3", (3 + ((plev - 1) / 5))); break;
					case  3: sprintf(p, " dam %d", 10 + (plev / 2)); break;
					case  5: sprintf(p, " dur 20+d20"); break;
					case  8: sprintf(p, " dam 3d6+%d", plev +
					    (plev / (((p_ptr->pclass == CLASS_MAGE) ||
					    (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))); break;
					case  9: sprintf(p, " dam %dd8", (6 + ((plev - 5) / 4))); break;
					case 11: sprintf(p, " dm %d* 5+d15", 2 + (plev / 15)); break;
					case 13: sprintf(p, " dam %d", 4 * plev); break;
					case 16: strcpy (p, " dur 25+d25"); break;
					case 17: strcpy (p, " random"); break;
					case 18: sprintf(p, " dam %dd8", (4 + ((plev - 5) / 4))); break;
					case 19: strcpy (p, " max dur 50"); break;
					case 20: strcpy (p, " dam 3*100"); break;
					case 22: strcpy (p, " dam 120"); break;
					case 27: sprintf(p, " dam %d", plev * 3); break;
					case 28: sprintf(p, " dam %d", plev * 4); break;
					case 29: strcpy (p, " dam 666"); break;
					case 31: sprintf(p, " dur %d+d%d", (plev / 2), (plev / 2)); break;
				}
				break;

			case 5: /* Trump */
				switch (spell)
				{
					case  0: strcpy (p, " range 10"); break;
					case  1: sprintf(p, " dam %dd3", 3 + ((plev - 1) / 5)); break;
					case  2: strcpy (p, " random"); break;
					case  4: sprintf(p, " range %d", plev * 4); break;
					case  5: sprintf(p, " range %d", plev + 2); break;
					case  6: strcpy (p, " dur 25+d30"); break;
					case  8: sprintf(p, " max wgt %d", plev * 15 / 10); break;
					case 14: strcpy (p, " delay 15+d21"); break;
					case 22: sprintf(p, " dam %d", plev * 3); break;
				}
				break;

			case 6: /* Arcane */
				switch (spell)
				{
					case  0: sprintf(p, " dam %dd3", 3 + ((plev - 1) / 5)); break;
					case  4: strcpy (p, " range 10"); break;
					case  5: sprintf(p, " dam 2d%d", plev / 2); break;
					case  7: strcpy (p, " heal 2d8"); break;
					case 14:
					case 15:
					case 16:
					case 17: strcpy (p, " dur 20+d20"); break;
					case 18: strcpy (p, " heal 4d8"); break;
					case 19: sprintf(p, " range %d", plev * 5); break;
					case 21: strcpy (p, " dam 6d8"); break;
					case 23: strcpy (p, " dur 24+d24"); break;
					case 28: sprintf(p, " dam %d", 75 + plev); break;
					case 30: strcpy (p, " delay 15+d21"); break;
					case 31: strcpy (p, " dur 25+d30"); break;
				}
				break;

			default:
				sprintf(p, "Unknown type: %d.", realm);
		}
	}
#endif /* DRS_SHOW_SPELL_INFO */
}


/*
 * Print a list of spells (for browsing or casting or viewing)
 */
void print_spells(byte *spells, int num, int y, int x, int realm)
{
	int             i, spell;
	magic_type      *s_ptr;
	cptr            comment;
	char            info[80];
	char            out_val[160];
	byte            line_attr;


	if (((realm < 0) || (realm > MAX_REALM - 1)) && wizard)
		msg_print("Warning! print_spells called with null realm");

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);


	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Access the spell */
		spell = spells[i];

		/* Access the spell */
		s_ptr = &mp_ptr->info[realm][spell];

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
				sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
				c_prt(TERM_L_DARK, out_val, y + i + 1, x);
				continue;
		}

		/* XXX XXX Could label spells above the players level */

		/* Get extra info */
		spell_info(info, spell, realm);

		/* Use that info */
		comment = info;

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Analyze the spell */
		if ((realm + 1 == p_ptr->realm1) ?
		    ((spell_forgotten1 & (1L << spell))) :
		    ((spell_forgotten2 & (1L << spell))))
		{
			comment = " forgotten";
			line_attr = TERM_YELLOW;
		}
		else if (!((realm + 1 == p_ptr->realm1) ?
		    (spell_learned1 & (1L << spell)) :
		    (spell_learned2 & (1L << spell))))
		{
			comment = " unknown";
			line_attr = TERM_L_BLUE;
		}
		else if (!((realm + 1 == p_ptr->realm1) ?
		    (spell_worked1 & (1L << spell)) :
		    (spell_worked2 & (1L << spell))))
		{
			comment = " untried";
			line_attr = TERM_L_GREEN;
		}

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s%2d %4d %3d%%%s",
		    I2A(i), spell_names[realm][spell], /* realm, spell */
		    s_ptr->slevel, s_ptr->smana, spell_chance(spell, realm), comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}


/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind.  Also sling ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(object_type *o_ptr)
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
bool hates_elec(object_type *o_ptr)
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
bool hates_fire(object_type *o_ptr)
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
		case TV_LIFE_BOOK:
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
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
bool hates_cold(object_type *o_ptr)
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
int set_acid_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_IGNORE_ACID) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
int set_elec_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_IGNORE_ELEC) return (FALSE);
	return (TRUE);
}


/*
 * Burn something
 */
int set_fire_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_IGNORE_FIRE) return (FALSE);
	return (TRUE);
}


/*
 * Freeze things
 */
int set_cold_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_cold(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_IGNORE_COLD) return (FALSE);
	return (TRUE);
}


/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 * Destruction taken from "melee.c" code for "stealing".
 * New-style wands and rods handled correctly. -LM-
 * Returns number of items destroyed.
 */
int inven_damage(inven_func typ, int perc)
{
	int         i, j, k, amt;
	object_type *o_ptr;
	char        o_name[80];


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

				/* Potions smash open */
				if (object_is_potion(o_ptr))
				{
					(void)potion_smash_effect(0, py, px, o_ptr->k_idx);
				}

				/* Reduce the charges of rods/wands */
				reduce_charges(o_ptr, amt);

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
	u32b        f1, f2, f3;
	char        o_name[80];


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
	if (f3 & TR3_IGNORE_ACID)
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
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = (dam + 2) / 3;
	if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_acid || p_ptr->resist_acid)) &&
	    randint(HURT_CHANCE) == 1)
		(void)do_dec_stat(A_CHR);

	/* If any armor gets hit, defend the player */
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
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
	if (p_ptr->resist_elec) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_elec || p_ptr->resist_elec)) &&
	    randint(HURT_CHANCE) == 1)
		(void)do_dec_stat(A_DEX);

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
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = (dam + 2) / 3;
	if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_fire || p_ptr->resist_fire)) &&
	    randint(HURT_CHANCE) == 1)
		(void)do_dec_stat(A_STR);

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
	if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = (dam + 2) / 3;
	if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_cold || p_ptr->resist_cold)) &&
	    randint(HURT_CHANCE) == 1)
		(void)do_dec_stat(A_STR);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (!(p_ptr->resist_cold && p_ptr->oppose_cold))
		inven_damage(set_cold_destroy, inv);
}


bool rustproof(void)
{
	int         item;
	object_type *o_ptr;
	char        o_name[80];
	cptr        q, s;

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Rustproof which piece of armour? ";
	s = "You have nothing to rustproof.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return FALSE;

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

	return TRUE;
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if ((o_ptr->art_name || artifact_p(o_ptr)) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		chg_virtue(V_ENCHANT, -5);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if ((artifact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
					  "terrible black aura", "surround your weapon", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		chg_virtue(V_ENCHANT, -5);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;


		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	/* Notice */
	return (TRUE);
}


/*
 * Enchant some bolts
 */
bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (o_ptr->art_name || artifact_p(o_ptr) || ego_item_p(o_ptr))
			continue;

		/* Skip cursed/broken items */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) continue;

		/* Randomize */
		if (rand_int(100) < 75) continue;

		/* Message */
		msg_print("Your bolts are covered in a fiery aura!");

		/* Ego-item */
		o_ptr->name2 = EGO_FLAME;

		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Notice */
		return (TRUE);
	}

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msg_print("The fiery enchantment failed.");

	/* Notice */
	return (TRUE);
}


/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
static s16b poly_r_idx(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	int i, r, lev1, lev2;

	/* Hack -- Uniques/Questors never polymorph */
	if ((r_ptr->flags1 & RF1_UNIQUE) ||
	    (r_ptr->flags1 & RF1_QUESTOR))
		return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint(20) / randint(9)) + 1);
	lev2 = r_ptr->level + ((randint(20) / randint(9)) + 1);

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
		if (r_ptr->flags1 & RF1_UNIQUE) continue;

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


bool polymorph_monster(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];
	monster_type *m_ptr = &m_list[c_ptr->m_idx];
	bool friendly, pet;
	bool polymorphed = FALSE;
	int new_r_idx;
	int old_r_idx = m_ptr->r_idx;


	/* Get the monsters attitude */
	friendly = is_friendly(m_ptr);
	pet = is_pet(m_ptr);

	/* Pick a "new" monster race */
	new_r_idx = poly_r_idx(old_r_idx);

	/* Handle polymorph */
	if (new_r_idx != old_r_idx)
	{
		/* "Kill" the "old" monster */
		delete_monster_idx(c_ptr->m_idx);

		/* Create a new monster (no groups) */
		if (place_monster_aux(y, x, new_r_idx, FALSE, FALSE, friendly, pet))
		{
			/* Success */
			polymorphed = TRUE;
		}
		else
		{
			monster_terrain_sensitive = FALSE;

			/* Placing the new monster failed */
			place_monster_aux(y, x, old_r_idx, FALSE, FALSE, friendly, pet);

			monster_terrain_sensitive = TRUE;
		}
	}

	return polymorphed;
}


/*
 * Dimension Door
 */
bool dimension_door(void)
{
	int	plev = p_ptr->lev;
	int	x = 0, y = 0;

	if (!tgt_pt(&x, &y)) return FALSE;

	p_ptr->energy -= 60 - plev;

	if (!cave_empty_bold(y, x) || (cave[y][x].info & CAVE_ICKY) ||
		(distance(y, x, py, px) > plev + 2) ||
		(!rand_int(plev * plev / 2)))
	{
		msg_print("You fail to exit the astral plane correctly!");
		p_ptr->energy -= 100;
		teleport_player(10);
	}
	else teleport_player_to(y, x);

	return (TRUE);
}
