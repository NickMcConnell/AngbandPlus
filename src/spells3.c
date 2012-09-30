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
#include "script.h"

/* Maximum number of tries for teleporting */
#define MAX_TRIES 100

/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_away(int m_idx, int dis)
{
	int ny = 0, nx = 0, oy, ox, d, i, min;
	int tries = 0;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cave_type *c_ptr = NULL;

	/* Paranoia */
	if (!m_ptr->r_idx) return (FALSE);

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	if ((((p_ptr->chp * 10) / p_ptr->mhp) < 5) &&
		(randint1(5) > ((p_ptr->chp * 10) / p_ptr->mhp)))
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
				d = distance(ox, oy, nx, ny);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds2(nx, ny)) continue;

			c_ptr = area(nx, ny);

			/* Require "empty" floor space */
			if (!cave_empty_grid(c_ptr)) continue;

			/* Not on player */
			if ((ny == p_ptr->py) && (nx == p_ptr->px)) continue;

			/* Not on bad terrain */
			if (!test_monster_square(c_ptr, r_ptr)) continue;

			/* 
			 * Test for fields that will not allow monsters to
			 * be generated on them.  (i.e. Glyph of warding)
			 */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_MPLACE)) continue;

			/* No teleporting into vaults and such */
			if (c_ptr->info & CAVE_ICKY) continue;

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
	area(nx, ny)->m_idx = m_idx;

	/* Update the old location */
	area(ox, oy)->m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Process fields under the monster. */
	field_script(c_ptr, FIELD_ACT_MONSTER_ENTER, "");

	/* Redraw the old grid */
	lite_spot(ox, oy);

	/* Redraw the new grid */
	lite_spot(nx, ny);

	/* Notice changes in view */
	if (FLAG(r_ptr, RF_LITE_1) ||  FLAG(r_ptr, RF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	return (TRUE);
}



/*
 * Teleport monster next to the player
 */
void teleport_to_player(int m_idx)
{
	int ny, nx, oy, ox, px, py, d, i, min;
	int attempts = 500;
	int dis = 2;
	bool look = TRUE;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cave_type *c_ptr = NULL;

	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* "Skill" test */
	if (randint1(100) > r_info[m_ptr->r_idx].level) return;

	/* Initialize */
	ny = m_ptr->fy;
	nx = m_ptr->fx;

	/* Initialize */
	py = p_ptr->py;
	px = p_ptr->px;

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
				d = distance(px, py, nx, ny);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds2(nx, ny)) continue;

			c_ptr = area(nx, ny);

			/* Check for a field that blocks movement */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER))
			{
				continue;
			}

			/* 
			 * Test for fields that will not allow monsters to
			 * be generated on them.  (i.e. Glyph of warding)
			 */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_MPLACE)) continue;

			/* Require "empty" floor space */
			if (!cave_empty_grid(c_ptr)) continue;

			/* Not on player */
			if ((ny == py) && (nx == px)) continue;

			/* ...nor onto the Pattern */
			if (cave_pattern_grid(c_ptr)) continue;

			/* No teleporting into vaults and such */
			/* if (c_ptr->info & (CAVE_ICKY)) continue; */

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
	area(nx, ny)->m_idx = m_idx;

	/* Update the old location */
	area(ox, oy)->m_idx = 0;

	/* Move the monster */
	m_ptr->fy = ny;
	m_ptr->fx = nx;

	/* Update the monster (new location) */
	update_mon(m_idx, TRUE);

	/* Process fields under the monster. */
	field_script(c_ptr, FIELD_ACT_MONSTER_ENTER, "");

	/* Redraw the old grid */
	lite_spot(ox, oy);

	/* Redraw the new grid */
	lite_spot(nx, ny);

	/* Notice changes in view */
	if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}
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
	int px = p_ptr->px;
	int py = p_ptr->py;

	int d, i, min, ox, oy;
	int tries = 0;

	int xx, yy;

	/* Initialize */
	int y = py;
	int x = px;

	monster_type *m_ptr;
	u16b m_idx;

	bool look = TRUE;
	cave_type *c_ptr;

	if (FLAG(p_ptr, TR_NO_TELE))
	{
		msgf("A mysterious force prevents you from teleporting!");
		return;
	}

	if (dis > 200) dis = 200;	/* To be on the safe side... */

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
				d = distance(px, py, x, y);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds2(x, y)) continue;

			c_ptr = area(x, y);

			/* Require empty space */
			if (!cave_empty_grid(c_ptr)) continue;

			/* No non-movement */
			if ((y == py) && (x == px)) continue;

			/* Check for a field that blocks movement */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER))
			{
				continue;
			}

			/* No teleporting into vaults and such */
			if (c_ptr->info & CAVE_ICKY) continue;

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

	/* Save old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	/* Move the player */
	p_ptr->py = y;
	p_ptr->px = x;

	/* Notice movement */
	Term_move_player();

	if (!p_ptr->depth)
	{
		/* Scroll wilderness */
		p_ptr->wilderness_x = px;
		p_ptr->wilderness_y = py;
		move_wild();
	}

	/* Redraw the old spot */
	lite_spot(ox, oy);

	/* Redraw the new spot */
	lite_spot(px, py);

	/* Process fields under the player. */
	field_script(area(px, py), FIELD_ACT_PLAYER_ENTER, "");

	/* Monsters with teleport ability may follow the player */
	for (xx = -1; xx <= 1; xx++)
	{
		for (yy = -1; yy <= 1; yy++)
		{
			if ((xx == 0) && (yy == 0))
			{
				/* Do nothing */
			}
			else
			{
				x = ox + xx;
				y = oy + yy;

				if (in_bounds2(x, y) && area(x, y)->m_idx)
				{
					m_idx = area(x, y)->m_idx;
					m_ptr = &m_list[m_idx];

					if ((FLAG(&r_info[m_ptr->r_idx], RF_TPORT)) &&
						!(FLAG(&r_info[m_ptr->r_idx], RF_RES_TELE)) &&
						!(m_ptr->csleep))
						/*
						 * The RES_TELE limitation is to avoid
						 * totally unkillable suckers...
						 */
					{
						teleport_to_player(m_idx);
					}
				}
			}
		}
	}

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

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
void teleport_player_to(int nx, int ny)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, oy, ox, dis = 0, ctr = 0;

	cave_type *c_ptr;
	
	/* No movement at all */
	if ((ny == py) && (nx == px)) return;

	if (FLAG(p_ptr, TR_NO_TELE))
	{
		msgf("A mysterious force prevents you from teleporting!");
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
			if (in_bounds2(x, y)) break;
		}

		/* Accept "naked" floor grids */
		c_ptr = area(x, y);

		/* No non-movement */
		if ((y == py) && (x == px)) continue;

		/* Can enter grid? */
		if (cave_empty_grid(c_ptr) && !(c_ptr->info & CAVE_ICKY) && 
				!(fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER)))
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

	/* Save old location */
	oy = py;
	ox = px;

	/* Move the player */
	py = y;
	px = x;

	/* Move the player */
	p_ptr->py = y;
	p_ptr->px = x;

	/* Notice movement */
	Term_move_player();

	if (!p_ptr->depth)
	{
		/* Scroll wilderness */
		p_ptr->wilderness_x = px;
		p_ptr->wilderness_y = py;
		move_wild();
	}

	/* Redraw the old spot */
	lite_spot(ox, oy);

	/* Redraw the new spot */
	lite_spot(px, py);

	/* Process fields under the player. */
	field_script(area(px, py), FIELD_ACT_PLAYER_ENTER, "");

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

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
	/* No effect in final quest */
	if (is_special_level(p_ptr->depth) && ironman_downward)
	{
		msgf("There is no effect.");
		return;
	}

	if (!check_down_wild())
	{
		msgf("There is no effect.");
		return;
	}
	
	if (FLAG(p_ptr, TR_NO_TELE))
	{
		msgf("A mysterious force prevents you from teleporting!");
		return;
	}

	if (!p_ptr->depth || ironman_downward)
	{
		msgf(MSGT_TPLEVEL, "You sink through the floor.");
		
		/* Go down */
		move_dun_level(1);
	}
	else if (is_special_level(p_ptr->depth))
	{
		msgf(MSGT_TPLEVEL, "You rise up through the ceiling.");
		
		/* Go up */
		move_dun_level(-1);
	}
	else if (one_in_(2) || (p_ptr->depth >= dungeon()->max_level))
	{
		msgf(MSGT_TPLEVEL, "You rise up through the ceiling.");

		/* Go down */
		move_dun_level(-1);
	}
	else
	{
		msgf(MSGT_TPLEVEL, "You sink through the floor.");

		/* Go up */
	    move_dun_level(1);
	}

	/* Sound */
	sound(SOUND_TPLEVEL);
}


bool check_down_wild(void)
{
	place_type *pl_ptr;

	/* Can always recall from dungeon */
	if (p_ptr->depth) return (TRUE);

	/* Hack - no recalling in the middle of the wilderness */
	if (!p_ptr->place_num)
	{
		msgf("Nothing happens.");
		return (FALSE);
	}

	/* Cannot recall in towns with no dungeon */
	if (!vanilla_town)
	{
		pl_ptr = &place[p_ptr->place_num];
	
		/* Look for dungeon */
		if (!pl_ptr->dungeon)
		{
			msgf("Nothing happens.");
			return (FALSE);
		}
		
		/* Else we must have been down before */
		if (!pl_ptr->dungeon->recall_depth)
		{
			msgf("You need to visit the dungeon first.");
			return (FALSE);
		}
	}

	return (TRUE);
}


/*
 * Recall the player to town or dungeon
 */
void recall_player(int turns)
{
	dun_type *d_ptr = dungeon();

	/*
	 * TODO: Recall the player to the last
	 * visited town when in the wilderness
	 */

	/* Ironman option */
	if (ironman_downward)
	{
		msgf("Nothing happens.");
		return;
	}

	if (!check_down_wild()) return;

	if (p_ptr->depth && (d_ptr->recall_depth > p_ptr->depth))
	{
		if (get_check("Reset recall depth? "))
			d_ptr->recall_depth = (char) p_ptr->depth;

	}
	else if (p_ptr->depth > d_ptr->recall_depth)
	{
		d_ptr->recall_depth = (char) p_ptr->depth;
	}

	if (!p_ptr->tim.word_recall)
	{
		p_ptr->tim.word_recall = turns;
		msgf("The air about you becomes charged...");
		p_ptr->redraw |= (PR_STATUS);
	}
	else
	{
		p_ptr->tim.word_recall = 0;
		msgf("A tension leaves the air around you...");
		p_ptr->redraw |= (PR_STATUS);
	}
}


void word_of_recall(void)
{
	recall_player(rand_range(15, 35));
}


/*
 * Apply disenchantment to the player's stuff
 *
 * XXX XXX XXX This function is also called from the "melee" code
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(void)
{
	int t = 0;
	object_type *o_ptr;

	/* Pick a random slot */
	switch (randint1(8))
	{
		case 1:
		{
			t = EQUIP_WIELD;
			break;
		}
		case 2:
		{
			t = EQUIP_BOW;
			break;
		}
		case 3:
		{
			t = EQUIP_BODY;
			break;
		}
		case 4:
		{
			t = EQUIP_OUTER;
			break;
		}
		case 5:
		{
			t = EQUIP_ARM;
			break;
		}
		case 6:
		{
			t = EQUIP_HEAD;
			break;
		}
		case 7:
		{
			t = EQUIP_HANDS;
			break;
		}
		case 8:
		{
			t = EQUIP_FEET;
			break;
		}
	}

	/* Get the item */
	o_ptr = &p_ptr->equipment[t];

	/* No item, nothing happens */
	if (!o_ptr->k_idx) return (FALSE);


	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
	{
		/* Nothing to notice */
		return (FALSE);
	}


	/* Artifacts have 71% chance to resist */
	if ((FLAG(o_ptr, TR_INSTA_ART)) && (randint0(100) < 71))
	{
		/* Message */
		msgf("Your %v (%c) resist%s disenchantment!",
				OBJECT_FMT(o_ptr, FALSE, 0), I2A(t),
				((o_ptr->number != 1) ? "" : "s"));

		/* Notice */
		return (TRUE);
	}
	
	/* Message */
	msgf("Your %v (%c) %s disenchanted!",
			OBJECT_FMT(o_ptr, FALSE, 0), I2A(t),
			((o_ptr->number != 1) ? "were" : "was"));

	/* Disenchant tohit */
	if (o_ptr->to_h > 0) o_ptr->to_h--;
	if ((o_ptr->to_h > 10) && (randint0(100) < 20)) o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0) o_ptr->to_d--;
	if ((o_ptr->to_d > 10) && (randint0(100) < 20)) o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0) o_ptr->to_a--;
	if ((o_ptr->to_a > 10) && (randint0(100) < 20)) o_ptr->to_a--;

	/* Trigger scripts */
	apply_object_trigger(TRIGGER_ALTER, o_ptr, "");


	chg_virtue(V_HARMONY, 1);
	chg_virtue(V_ENCHANT, -2);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Notice changes */
	notice_equip();

	/* Notice */
	return (TRUE);
}


void mutate_player(void)
{
	int max1, cur1, max2, cur2, ii, jj;
	int bonus1, bonus2;

	/* Pick a pair of stats */
	ii = randint0(A_MAX);
	for (jj = ii; jj == ii; jj = randint0(A_MAX)) /* loop */ ;

	max1 = p_ptr->stat[ii].max;
	cur1 = p_ptr->stat[ii].cur;
	max2 = p_ptr->stat[jj].max;
	cur2 = p_ptr->stat[jj].cur;

	/* Adjust the swapped stats... */
	bonus1 = rp_ptr->r_adj[ii] + cp_ptr->c_adj[ii];
	bonus2 = rp_ptr->r_adj[jj] + cp_ptr->c_adj[jj];

	max1 = adjust_stat(jj, max1, bonus2 - bonus1);
	max2 = adjust_stat(ii, max2, bonus1 - bonus2);

	/* Hack - restore both stats rather than figure try to swap drainage */
	cur1 = max1;
	cur2 = max2;

	p_ptr->stat[ii].max = max2;
	p_ptr->stat[ii].cur = cur2;
	p_ptr->stat[jj].max = max1;
	p_ptr->stat[jj].cur = cur1;

	p_ptr->update |= (PU_BONUS);
}


/*
 * Apply Nexus
 */
void apply_nexus(const monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	switch (randint1(7))
	{
		case 1:  case 2:  case 3:
		{
			teleport_player(200);
			break;
		}

		case 4:  case 5:
		{
			teleport_player_to(m_ptr->fx, m_ptr->fy);
			break;
		}

		case 6:
		{
			if (player_save(r_ptr->hdice * 2))
			{
				msgf("You resist the effects!");
				break;
			}

			/* Teleport Level */
			teleport_player_level();
			break;
		}

		case 7:
		{
			if (player_save(r_ptr->hdice * 2))
			{
				msgf("You resist the effects!");
				break;
			}

			msgf("Your body starts to scramble...");
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
	object_type *o_ptr = &p_ptr->equipment[EQUIP_LITE];
	cptr lite_item = NULL;


	/* It's a lamp */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{
		max_flog = FUEL_LAMP;

		/* Remember what the item is */
		lite_item = "lantern";
	}

	/* It's a torch */
	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{
		max_flog = FUEL_TORCH;

		/* Remember what the item is */
		lite_item = "torch";
	}

	/* No torch to refill */
	else
	{
		msgf("You are not wielding anything which uses phlogiston.");
		return;
	}

	if (o_ptr->timeout >= max_flog)
	{
		msgf("No more phlogiston can be put in this %s.", lite_item);
		return;
	}

	/* Refuel */
	o_ptr->timeout += (max_flog / 2);

	/* Message */
	msgf("You add phlogiston to your %s.", lite_item);

	/* Comment */
	if (o_ptr->timeout >= max_flog)
	{
		o_ptr->timeout = max_flog;
		msgf("Your %s is full.", lite_item);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}


/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type)
{
	object_type *o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	byte ego = 0;

	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if (o_ptr->k_idx && !o_ptr->xtra_name && !cursed_p(o_ptr))
	{
		cptr act;

		switch (brand_type)
		{
			case 1:
			{
				act = "is engulfed in raw Logrus!";
				ego = EGO_CHAOTIC;
				break;
			}
			case 2:
			{
				act = "is coated with poison.";
				ego = EGO_BRAND_POIS;
				break;
			}
			case 3:
			{
				act = "thirsts for blood!";
				ego = EGO_VAMPIRIC;
				break;
			}
			case 4:
			{
				act = "seems very unstable now.";
				ego = EGO_TRUMP;
				o_ptr->pval = randint1(2);
				break;
			}

			default:
			{
				if (randint0(100) < 25)
				{
					act = "is covered in a fiery shield!";
					ego = EGO_BRAND_FIRE;
				}
				else
				{
					act = "glows deep, icy blue!";
					ego = EGO_BRAND_COLD;
				}
			}
		}

		msgf("Your %v %s", OBJECT_FMT(o_ptr, FALSE, 0), act);

		(void)enchant(o_ptr, rand_range(4, 6), ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();

		msgf("The Branding failed.");

		chg_virtue(V_ENCHANT, -2);
	}

	if (ego)
	{
		/* Hack - save the price */
		s32b cost = o_ptr->cost;

		add_ego_flags(o_ptr, ego);

		o_ptr->cost = cost;
		
		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();
	}
}


void call_the_(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i;

	if (in_bounds(px, py) &&
		cave_floor_grid(area(px - 1, py - 1)) &&
		cave_floor_grid(area(px - 1, py)) &&
		cave_floor_grid(area(px - 1, py + 1)) &&
		cave_floor_grid(area(px, py - 1)) &&
		cave_floor_grid(area(px, py + 1)) &&
		cave_floor_grid(area(px + 1, py - 1)) &&
		cave_floor_grid(area(px + 1, py)) &&
		cave_floor_grid(area(px + 1, py + 1)))
	{
		for (i = 1; i < 10; i++)
		{
			if (i != 5) (void)fire_ball(GF_ROCKET, i, 175, 2);
		}

		for (i = 1; i < 10; i++)
		{
			if (i != 5) (void)fire_ball(GF_MANA, i, 175, 3);
		}

		for (i = 1; i < 10; i++)
		{
			if (i != 5) (void)fire_ball(GF_NUKE, i, 175, 4);
		}
	}
	else
	{
		msgf("You %s the %s too close to a wall!",
				   ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
				   ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
		msgf("There is a loud explosion!");

		if (destroy_area(px, py, 20 + p_ptr->lev))
			msgf("The dungeon collapses...");
		else
			msgf("The dungeon trembles.");

		take_hit(rand_range(100, 250), "a suicidal Call the Void");
	}
}


/*
 * Fetch an item (teleport it right underneath the caster)
 *
 * This is a massive hack.
 */
void fetch(int dir, int wgt, bool require_los)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int tx, ty;
	cave_type *c_ptr;
	object_type *o_ptr;

	/* Check to see if an object is already there */
	if (area(px, py)->o_idx)
	{
		msgf("You can't fetch when you're already standing on something.");
		return;
	}

	/* Use a target */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;

		/* Paranoia */
		if ((distance(px, py, tx, ty) > MAX_RANGE) || (!in_bounds2(tx, ty)))
		{
			msgf("You can't fetch something that far away!");
			return;
		}

		c_ptr = area(tx, ty);

		/* We need an item to fetch */
		if (!c_ptr->o_idx)
		{
			msgf("There is no object at this place.");
			return;
		}

		/* No fetching from vault */
		if (c_ptr->info & CAVE_ICKY)
		{
			msgf("The item slips from your control.");
			return;
		}

		/* We need to see the item */
		if (require_los && !player_has_los_grid(parea(tx, ty)))
		{
			msgf("You have no direct line of sight to that location.");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py;				/* Where to drop the item */
		tx = px;

		while (TRUE)
		{
			ty += ddy[dir];
			tx += ddx[dir];

			/* paranoia */
			if (!in_bounds2(tx, ty)) continue;

			c_ptr = area(tx, ty);

			if ((distance(px, py, tx, ty) > MAX_RANGE) ||
				cave_wall_grid(c_ptr)) return;

			/* found a spot */
			if (!c_ptr->o_idx) break;
		}
	}

	o_ptr = &o_list[c_ptr->o_idx];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msgf("The object is too heavy.");
		return;
	}

	/* 
	 * Hack - do not get artifacts.
	 * This interacts badly with preserve mode.
	 */
	if (FLAG(o_ptr, TR_INSTA_ART))
	{
		msgf("The object seems to have a will of its own!");
		return;
	}

	/* Move the object */
	move_object(&area(px, py)->o_idx, &c_ptr->o_idx, o_ptr);

	/* Record the new location */
	o_ptr->ix = px;
	o_ptr->iy = py;

	msgf("%^v flies through the air to your feet.", OBJECT_FMT(o_ptr, TRUE, 0));

	/* Notice the moved object (The player gets redrawn) */
	note_spot(px, py);

	/* Redraw the map???  Can we just use lite_spot() a few times? */
	p_ptr->redraw |= PR_MAP;
}


void alter_reality(void)
{
	if (p_ptr->depth)
	{
		msgf("The world changes!");

		/* Leaving */
		p_ptr->state.leaving = TRUE;
	}
	else
	{
		msgf("The world seems to change for a moment!");
	}
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
bool warding_glyph(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	cave_type *c_ptr = area(px, py);

	/* XXX XXX XXX */
	if (!cave_naked_grid(c_ptr))
	{
		msgf("The object resists the spell.");
		return FALSE;
	}

	/* Not in a wall */
	if (cave_wall_grid(c_ptr))
	{
		msgf("You need open space to draw the rune.");
		return FALSE;
	}

	/* Add the glyph here as a field */
	(void)place_field(px, py, FT_GLYPH_WARDING);

	/* Notice it */
	note_spot(px, py);

	return TRUE;
}


/*
 * Leave an "explosive rune" which prevents monster movement
 */
bool explosive_rune(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	cave_type *c_ptr = area(px, py);

	/* XXX XXX XXX */
	if (!cave_naked_grid(c_ptr))
	{
		msgf("The object resists the spell.");
		return FALSE;
	}

	/* Not in a wall */
	if (cave_wall_grid(c_ptr))
	{
		msgf("You need open space to draw the rune.");
		return FALSE;
	}

	/* Add the glyph here as a field */
	(void)place_field(px, py, FT_GLYPH_EXPLODE);

	/* Notice it */
	note_spot(px, py);

	return TRUE;
}


/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int i;
	object_type *o_ptr;

	/* Identify equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Identify it */
		identify_item(o_ptr);
	}

	/* Identify inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Identify it */
		identify_item(o_ptr);
	}
	OBJ_ITT_END;
	
	/* Notice changes */
	notice_inven();
}

/*
 * Try to remove a curse from an item
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 *
 * If the item is heavily or permanently cursed, we add that flag
 * to the 'known' flags so the player can see that it is cursed on
 * the 'C'haracter screen. This may allow a player to learn that
 * an unidentified scroll is remove curse when it has no apparent
 * effect, in rare circumstances.
 */
static bool uncurse_item(object_type *o_ptr, bool all)
{
	bool heavy;

	/* Uncursed already */
	if (!cursed_p(o_ptr)) return (FALSE);

	/* Heavily Cursed Items need a special spell */
	if (!all && (FLAG(o_ptr, TR_HEAVY_CURSE)))
	{
		/* Let the player know */
		o_ptr->kn_flags[2] |= TR2_HEAVY_CURSE;

		/* Done */
		return (FALSE);
	}

	/* Perma-Cursed Items can NEVER be uncursed */
	if (FLAG(o_ptr, TR_PERMA_CURSE)) 
	{
		/* Let the player know */
		o_ptr->kn_flags[2] |= TR2_PERMA_CURSE;
		
		/* Done */
		return (FALSE);
	}
	
	/* Uncurse the item */
	o_ptr->flags[2] &= ~(TR2_CURSED | TR2_HEAVY_CURSE);
	o_ptr->kn_flags[2] &= ~(TR2_CURSED | TR2_HEAVY_CURSE);
	
	/* Heavy sensing? */
	heavy = class_info[p_ptr->rp.pclass].heavy_sense;
	
	/* Take away the feeling */
	o_ptr->feeling = FEEL_NONE;

	/* Strip awareness of feeling */
	o_ptr->info &= ~(OB_SENSE);

	/* Renew feeling */
	sense_item(o_ptr, heavy, TRUE, FALSE);

	/* Recalculate the bonuses */
	p_ptr->update |= (PU_BONUS);
	
	/* Notice changes */
	notice_item();

	return (TRUE);
}

/*
 * Removes curses from items in inventory
 */
static int remove_curse_aux(bool all)
{
	int i, cnt = 0;
	object_type *o_ptr;

	/* Attempt to uncurse equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Count the uncursings */
		if (uncurse_item(o_ptr, all)) cnt++;
	}

	/* Attempt to uncurse inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Count the uncursings */
		if (uncurse_item(o_ptr, all)) cnt++;
	}
	OBJ_ITT_END;

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
	int amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[256];

	cptr q, s;

	/* Hack -- force destruction */
	if (p_ptr->cmd.arg > 0) force = TRUE;

	/* Get an item */
	q = "Turn which item to gold? ";
	s = "You have nothing to turn to gold.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

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
	object_desc(o_name, o_ptr, TRUE, 3, 256);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (!(auto_destroy && (object_value(o_ptr) < 1)))
		{
			/* Make a verification */
			if (!get_check("Really turn %s to gold? ", o_name)) return FALSE;
		}
	}

	/* Check for artifacts */
	if (!can_player_destroy_object(o_ptr))
	{
		/* Message */
		msgf("You fail to turn %s to gold!", o_name);

		/* Done */
		return FALSE;
	}

	price = object_value_real(o_ptr);

	if (price <= 0)
	{
		/* Message */
		msgf("You turn %s to fool's gold.", o_name);
	}
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;
		msgf("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

	}

	/* Eliminate the item */
	item_increase(o_ptr, -amt);

	return TRUE;
}


/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	cave_type *c_ptr = area(px, py);

	/* XXX XXX XXX */
	if (!cave_valid_grid(c_ptr))
	{
		msgf("The object resists the spell.");
		return;
	}

	if (!check_down_wild()) return;

	/* XXX XXX XXX */
	delete_object(px, py);

	if (!p_ptr->depth || ironman_downward)
	{
		/* Town/wilderness or Ironman */
		cave_set_feat(px, py, FEAT_MORE);
	}
	else if (is_special_level(p_ptr->depth))
	{
		/* Quest level */
		cave_set_feat(px, py, FEAT_LESS);
	}
	else if (one_in_(2) || (p_ptr->depth >= dungeon()->max_level))
	{
		cave_set_feat(px, py, FEAT_LESS);
	}
	else
	{
		cave_set_feat(px, py, FEAT_MORE);
	}
}




/*
 * Break the curse of an item
 */
static void break_curse(object_type *o_ptr)
{
	if (cursed_p(o_ptr) && !(FLAG(o_ptr, TR_PERMA_CURSE))
		 && (randint0(100) < 25))
	{
		msgf("The curse is broken!");
		
		/* Uncurse it */
		uncurse_item(o_ptr, TRUE);
	}
}


#define ENCHANT_MAX_DAM 25
#define ENCHANT_MAX 15

/*
 * Used by the "enchant" function (chance of failure)
 *
 * Formula: 1000-0.064x^3
 */
static int enchant_table_dam[ENCHANT_MAX_DAM + 1] =
{
	0, 115, 221, 319, 407,
	488, 561, 627, 686, 738,
	784, 824, 859, 889, 914,
	936, 953, 967, 978, 986,
	992, 996, 998, 999, 999,
	1000
};

/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[ENCHANT_MAX + 1] =
{
	0, 10, 50, 100, 200,
	300, 400, 500, 650, 800,
	950, 987, 993, 995, 998,
	1000
};


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
	int i, chance, prob, change;
	bool res = FALSE;
	bool a = ((FLAG(o_ptr, TR_INSTA_ART)) ? TRUE : FALSE);
	bool force = (eflag & ENCH_FORCE);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) ||
		(o_ptr->tval == TV_ARROW) || (o_ptr->tval == TV_SHOT))
	{
		prob = prob / 50;
	}

	/* Some items are easier to enchant */
	if (FLAG(o_ptr, TR_EASY_ENCHANT))
	{
		/* Don't apply artifact failure chance */
		a = FALSE;

		/* Apply more enchantment attempts */
		n *= 2;
	}

	/* Try "n" times */
	for (i = 0; i < n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (!force && randint0(prob) >= 100) continue;

		/* Enchant to hit */
		if (eflag & ENCH_TOHIT)
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > ENCHANT_MAX) chance = 1000;
			else
				chance = enchant_table[o_ptr->to_h];

			if (force || ((randint1(1000) > chance) && (!a || one_in_(2))))
			{
				/* The amount you enchant varys */
				if ((o_ptr->to_h > 7) || force) change = 1;
				else if (o_ptr->to_h > 4) change = randint1(2);
				else
					change = randint1(3);

				o_ptr->to_h += change;
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
			else if (o_ptr->to_d > ENCHANT_MAX_DAM) chance = 1000;
			else
				chance = enchant_table_dam[o_ptr->to_d];

			if (force || ((randint1(1000) > chance) && (!a || one_in_(2))))
			{
				/* The amount you enchant varys */
				if ((o_ptr->to_d > 7) || force) change = 1;
				else if (o_ptr->to_d > 4) change = randint1(2);
				else
					change = randint1(3);

				o_ptr->to_d += change;
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
			else if (o_ptr->to_a > ENCHANT_MAX) chance = 1000;
			else
				chance = enchant_table[o_ptr->to_a];

			if (force || ((randint1(1000) > chance) && (!a || one_in_(2))))
			{
				/* The amount you enchant varys */
				if ((o_ptr->to_a > 7) || force) change = 1;
				else if (o_ptr->to_a > 4) change = randint1(2);
				else
					change = randint1(3);

				o_ptr->to_a += change;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (o_ptr->to_a >= 0)
					break_curse(o_ptr);
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Apply trigger */
	apply_object_trigger(TRIGGER_ALTER, o_ptr, "");

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	
	/* Notice changes */
	notice_item();

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
	bool okay = FALSE;
	object_type *o_ptr;
	cptr q, s;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Describe */
	msgf("The %v glow%s brightly!", OBJECT_FMT(o_ptr, FALSE, 0),
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
		msgf("The enchantment failed.");

		if (one_in_(3)) chg_virtue(V_ENCHANT, -1);
	}
	else
		chg_virtue(V_ENCHANT, 1);

	/* Something happened */
	return (TRUE);
}


bool artifact_scroll(void)
{
	bool okay;
	object_type *o_ptr;
	char o_name[256];
	cptr q, s;


	/* Enchant weapon/armour */
	item_tester_hook = item_tester_hook_weapon_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0, 256);

	/* Describe */
	msgf("The %s radiate%s a blinding light!", o_name,
			   ((o_ptr->number > 1) ? "" : "s"));

	/* No artifact creation of Dragon Scale Mail */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* ToDo: Maybe allow some of the DSMs to be enchanted */
		msgf("The %s %s already magical!",
				   o_name, ((o_ptr->number > 1) ? "are" : "is"));

		okay = FALSE;
	}

	else if (o_ptr->xtra_name)
	{
		msgf("The %s %s already %s!",
				   o_name, ((o_ptr->number > 1) ? "are" : "is"),
				   ((o_ptr->number >
					 1) ? "powerful items" : "a powerful item"));
		okay = FALSE;
	}

	else
	{
		if (o_ptr->number > 1)
		{
			msgf
				("Not enough enough energy to enchant more than one object!");
			msgf("%d of your %s %s destroyed!", (o_ptr->number) - 1,
					   o_name, ((o_ptr->number > 2) ? "were" : "was"));

			o_ptr->number = 1;

			/* Notice weight changes */
			p_ptr->update |= PU_WEIGHT;
		}

		/* The power of the generated artifact depends on player level */
		okay = create_artifact(o_ptr, p_ptr->lev * 2, TRUE);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msgf("The enchantment failed.");
		if (one_in_(3)) chg_virtue(V_ENCHANT, -1);
	}
	else
		chg_virtue(V_ENCHANT, 1);

	/* Something happened */
	return (TRUE);
}


/*
 * Apply bad luck to an object
 */
static void bad_luck(object_type *o_ptr)
{
	bool is_art = ((FLAG(o_ptr, TR_INSTA_ART)) ? TRUE : FALSE);

	object_type *q_ptr;

	/* Do not curse unwieldable items */
	if (wield_slot(o_ptr) == -1) return;

	/* Objects become worse sometimes */
	if (one_in_(13))
	{
		int number = o_ptr->number;

		/* Non-artifacts get rerolled */
		if (!is_art)
		{
			SET_FLAG(o_ptr, TR_CURSED);

			/* Prepare it */
			q_ptr = object_prep(o_ptr->k_idx);

			/* Swap it */
			swap_objects(o_ptr, q_ptr);

			/* Restore the number */
			o_ptr->number = number;

			/* Apply bad magic */
			apply_magic(o_ptr, p_ptr->depth, 0, OC_FORCE_BAD);
		}

		/* Now curse it */
		SET_FLAG(o_ptr, TR_CURSED);
	}

	/* Objects are blasted sometimes */
	if (one_in_(666) && (!is_art || one_in_(3)))
	{
		/* Blast it */
		if (o_ptr->to_a) o_ptr->to_a = 0 - (s16b)rand_range(5, 10);
		if (o_ptr->to_h) o_ptr->to_h = 0 - (s16b)rand_range(5, 10);
		if (o_ptr->to_d) o_ptr->to_d = 0 - (s16b)rand_range(5, 10);
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->flags[0] = 0;
		o_ptr->flags[1] = 0;
		o_ptr->flags[2] = 0;
		o_ptr->flags[3] = 0;

		add_ego_flags(o_ptr, EGO_BLASTED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();
	}
}


/*
 * Identify an object
 */
void identify_item(object_type *o_ptr)
{
	if (p_ptr->muta3 & MUT3_BAD_LUCK)
	{
		bad_luck(o_ptr);
	}

	if (!object_known_full(o_ptr))
	{
		if (FLAG(o_ptr, TR_INSTA_ART))
			chg_virtue(V_KNOWLEDGE, 3);
		else
			chg_virtue(V_KNOWLEDGE, 1);
	}

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Save knowledge of artifact */
	if (o_ptr->a_idx)
	{
		/* Have we seen it before? */
		if (a_info[o_ptr->a_idx].cur_num != 2)
		{
			int artifact = o_ptr->a_idx;
			
			/* Notice a quest for this artifact */
			trigger_quest_complete(QX_KNOW_ARTIFACT, &artifact);
		
			/*
			 * If the item was an artifact, and if the
			 * auto-note is selected, write a message.
			 */
			if (auto_notes && take_notes)
			{
				/* Write note */
				add_note('A', "Found The %v", OBJECT_FMT(o_ptr, FALSE, 0));
			}
		}

		a_info[o_ptr->a_idx].cur_num = 2;
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	
	/* Notice changes */
	notice_item();	
}


static bool item_tester_unknown(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Check to see if we don't know the flavor */
	if (k_ptr->flavor && !k_ptr->aware) return (TRUE);

	/* Check to see if we have identified the item */
	if (object_known_p(o_ptr)) return (FALSE);

	return (TRUE);
}


static bool item_tester_unknown_star(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Check to see if we don't know the flavor */
	if (k_ptr->flavor && !k_ptr->aware) return (TRUE);

	/* Check to see if we have identified the item */
	if (object_known_full(o_ptr)) return (FALSE);

	return (TRUE);
}


/*
 * Identify an object in the inventory (or on the floor)
 * Returns TRUE if something was identified, else FALSE.
 * As a side effect it also sorts and combines the objects in the inventory.
 *
 * This has been rewritten so that when the identification was of an inventory
 * object the correct inv slot (after sorting and combining) is shown in the message.
 * To do this the routine combines and sorts the objects right after the
 * identification and before the message is generated.  This way the combined
 * and sorted object will have the right letter for the slot in the message.
 * Except in the case where the player had 1 scroll of identify and that scroll
 * disappeared after use.  So then the letter for the slot in the message is
 * one too high.  This is solved by determining that the identification was by
 * scroll, there was only one scroll and so the letter must be one lower.
 */
static bool ident_spell_aux(int k_idx)
{
	cptr q, s;
	object_type *o_ptr, *j_ptr;
	bool disappear = FALSE, back_step = FALSE, skip = FALSE;

	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Identify it */
	identify_item(o_ptr);

	/* Hack.  Do the sorting now */
	o_ptr = reorder_pack_watch(o_ptr);

	/* Hack.  Do the combining now */
	o_ptr = combine_pack_watch(o_ptr);

	/* Find out if the id was by scroll */
	OBJ_ITT_START (p_ptr->inventory, j_ptr)
	{
		/* No need to skip anything now */
		skip = FALSE;

		/* Was it exactly one scroll? */
		if (j_ptr->k_idx == k_idx &&
			j_ptr->number == 1)
		{
			/* That will disappear */
			disappear = TRUE;

			/* If you read the id scroll on itself, list it just once */
			skip = TRUE;
		}

		/* Found the original */
		if (o_ptr == j_ptr)
		{
			/* The id scroll is located before object */
			if (disappear) back_step = TRUE;

			/* We know enough */
			break;
		}
	}
	OBJ_ITT_END;

	/* Do we need the hack for the right letter in the inventory? */
	if (back_step)
	{
		/* Not quite the description */
		if (!skip) item_describe_faux(o_ptr);
	}
	else
		/* Description */
		item_describe(o_ptr);

	/* Something happened */
	return (TRUE);
}


/* Identify an object by some non-scroll method. */
bool ident_spell(void)
{
	return (ident_spell_aux(0));
}

/* Identify an object by reading an identify scroll */
bool ident_scroll(int k_idx)
{
	return (ident_spell_aux(k_idx));
}


/*
 * Mundanify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was mundanified, else FALSE.
 */
bool mundane_spell(void)
{
	object_type *o_ptr;
	object_kind *k_ptr;
	cptr q, s;


	/* Get an item */
	q = "Use which item? ";
	s = "You have nothing you can use.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	k_ptr = &k_info[o_ptr->k_idx];

	/* Oops */
	msgf("There is a bright flash of light!");

	/* No discount */
	o_ptr->discount = 0;

	/* Not identified yet */
	o_ptr->info &= ~(OB_SENSE | OB_KNOWN | OB_EMPTY | OB_STOREB);

    /* Erase the inscription */
    quark_remove(&o_ptr->inscription);

	/* No longer a numbered artifact */
	o_ptr->a_idx = 0;

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
    quark_remove(&o_ptr->xtra_name);

	/* Default power */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* No artifact powers */
	o_ptr->flags[0] = k_ptr->flags[0];
	o_ptr->flags[1] = k_ptr->flags[1];
	o_ptr->flags[2] = k_ptr->flags[2];
	o_ptr->flags[3] = k_ptr->flags[3];

	/* For rod-stacking */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->timeout = o_ptr->pval * o_ptr->number;
		o_ptr->pval = k_ptr->pval * o_ptr->number;
	}

	/* Initialise cost */
	o_ptr->cost = k_ptr->cost;

	/* Something happened */
	return (TRUE);
}



/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to *identify*.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Identify it */
	identify_item(o_ptr);
	object_mental(o_ptr);

	/* Save all the known flags */
	o_ptr->kn_flags[0] = o_ptr->flags[0];
	o_ptr->kn_flags[1] = o_ptr->flags[1];
	o_ptr->kn_flags[2] = o_ptr->flags[2];
	o_ptr->kn_flags[3] = o_ptr->flags[3];

	/* Handle stuff */
	handle_stuff();

	/* Describe it fully */
	identify_fully_aux(o_ptr);

	/* Success */
	return (TRUE);
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
	int lev;
	int recharge_strength, recharge_amount;

	object_type *o_ptr;
	object_kind *k_ptr;

	bool fail = FALSE;
	byte fail_type = 1;

	cptr q, s;
	char o_name[256];


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];

	/*
	 * Extract the object "level"
	 * (Rescaled due to change in dungeon distribtuion)
	 */
	lev = k_info[o_ptr->k_idx].level / 2;

	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((power > lev) ? (power - lev) : 0) / 5;


		/* Back-fire */
		if (one_in_(recharge_strength))
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
		else
			recharge_strength = (100 + power - lev - (8 * o_ptr->pval)) / 15;

		/* Back-fire */
		if ((recharge_strength < 0) || one_in_(recharge_strength))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}

		/* If the spell didn't backfire, recharge the wand or staff. */
		else
		{
			/* Recharge based on the standard number of charges. */
			recharge_amount = randint1(1 + k_ptr->pval);

			/* Multiple wands in a stack increase recharging somewhat. */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			{
				recharge_amount +=
					(randint1(recharge_amount * (o_ptr->number - 1))) / 2;
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

			/* Reduce "used" charges */
			if (o_ptr->tval == TV_WAND)
			{
				o_ptr->ac -= recharge_amount;

				/* Never less than zero */
				if (o_ptr->ac < 0) o_ptr->ac = 0;
			}
			
			/* Hack -- we no longer "memorize" the item */
			o_ptr->info &= ~(OB_MENTAL);

			/* Hack -- we no longer "know" the item */
			o_ptr->info &= ~(OB_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->info &= ~(OB_EMPTY);
		}
	}


	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Artifacts are never destroyed. */
		if (FLAG(o_ptr, TR_INSTA_ART))
		{
			msgf("The recharging backfires - %v is completely drained!",
					   OBJECT_FMT(o_ptr, TRUE, 0));

			/* Artifact rods. */
			if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < 10000))
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;

			/* Artifact wands and staffs. */
			else
			{
				if (o_ptr->tval == TV_WAND)
				{
					o_ptr->ac += o_ptr->pval;
				}

				o_ptr->pval = 0;
			}
		}
		else
		{
			/* Get the object description */
			object_desc(o_name, o_ptr, FALSE, 0, 256);

			/*** Determine Seriousness of Failure ***/

			/* (High) Mages recharge objects more safely. */
			if ((p_ptr->rp.pclass == CLASS_MAGE) ||
				(p_ptr->rp.pclass == CLASS_HIGH_MAGE))
			{
				/* 10% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(10)) fail_type = 2;
					else
						fail_type = 1;
				}
				/* 75% chance to blow up one wand, otherwise draining. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (!one_in_(3)) fail_type = 2;
					else
						fail_type = 1;
				}
				/* 50% chance to blow up one staff, otherwise no effect. */
				else if (o_ptr->tval == TV_STAFF)
				{
					if (one_in_(2)) fail_type = 2;
					else
						fail_type = 0;
				}
			}

			/* All other classes get no special favors. */
			else
			{
				/* 33% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (one_in_(3)) fail_type = 2;
					else
						fail_type = 1;
				}
				/* 20% chance of the entire stack, else destroy one wand. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (one_in_(5)) fail_type = 3;
					else
						fail_type = 2;
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
					msgf("The recharge backfires, draining the rod further!");
					if (o_ptr->timeout < 10000)
						o_ptr->timeout = (o_ptr->timeout + 100) * 2;
				}
				else if (o_ptr->tval == TV_WAND)
				{
					msgf("You save your %s from destruction, but all charges are lost.",
						 o_name);
					o_ptr->ac += o_ptr->pval;
					o_ptr->pval = 0;
				}
				/* Staffs aren't drained. */
			}

			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
					msgf("Wild magic consumes one of your %s!", o_name);
				else
					msgf("Wild magic consumes your %s!", o_name);

				/* Reduce rod stack maximum timeout, drain wands. */
				if (o_ptr->tval == TV_ROD) o_ptr->pval -= k_ptr->pval;
				if (o_ptr->tval == TV_WAND)
				{
					o_ptr->ac += o_ptr->pval;
					o_ptr->pval = 0;

					reduce_charges(o_ptr, 1);
				}

				/* Reduce and describe */
				item_increase(o_ptr, -1);
			}

			/* Destroy all members of a stack of objects. */
			if (fail_type == 3)
			{
				if (o_ptr->number > 1)
					msgf("Wild magic consumes all your %s!", o_name);
				else
					msgf("Wild magic consumes your %s!", o_name);


				/* Reduce and describe */
				item_increase(o_ptr, -999);
			}
		}
	}

	/* Notice changes */
	notice_inven();

	/* Something was done */
	return (TRUE);
}


/*
 * Bless a weapon
 */
bool bless_weapon(void)
{
	object_type *o_ptr;
	char o_name[256];
	cptr q, s;

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get an item */
	q = "Bless which weapon? ";
	s = "You have weapon to bless.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return FALSE;

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0, 256);

	if (cursed_p(o_ptr))
	{
		if (((FLAG(o_ptr, TR_HEAVY_CURSE)) && (randint1(100) < 33)) ||
			(FLAG(o_ptr, TR_PERMA_CURSE)))
		{
			msgf("The black aura on the %s disrupts the blessing!",
					   o_name);
			return TRUE;
		}

		msgf("A malignant aura leaves the %s.", o_name);
		
		/* Uncurse it */
		uncurse_item(o_ptr, TRUE);
	}

	/*
	 * Next, we try to bless it. Artifacts have a 1/3 chance of
	 * being blessed, otherwise, the operation simply disenchants
	 * them, godly power negating the magic. Ok, the explanation
	 * is silly, but otherwise priests would always bless every
	 * artifact weapon they find. Ego weapons and normal weapons
	 * can be blessed automatically.
	 */
	if (FLAG(o_ptr, TR_BLESSED))
	{
		msgf("The %s %s blessed already.", o_name,
				   ((o_ptr->number > 1) ? "were" : "was"));
		return TRUE;
	}

	if (!(o_ptr->xtra_name) || one_in_(3))
	{
		/* Describe */
		msgf("The %s shine%s!", o_name, ((o_ptr->number > 1) ? "" : "s"));
		SET_FLAG(o_ptr, TR_BLESSED);
		o_ptr->kn_flags[2] |= TR2_BLESSED;
	}
	else
	{
		bool dis_happened = FALSE;

		msgf("The artifact resists your blessing!");

		/* Disenchant tohit */
		if (o_ptr->to_h > 0)
		{
			o_ptr->to_h--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_h > 5) && (randint0(100) < 33)) o_ptr->to_h--;

		/* Disenchant todam */
		if (o_ptr->to_d > 0)
		{
			o_ptr->to_d--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_d > 5) && (randint0(100) < 33)) o_ptr->to_d--;

		/* Disenchant toac */
		if (o_ptr->to_a > 0)
		{
			o_ptr->to_a--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_a > 5) && (randint0(100) < 33)) o_ptr->to_a--;

		if (dis_happened)
		{
			msgf("There is a static feeling in the air...");
			msgf("The %s %s disenchanted!", o_name,
					   ((o_ptr->number > 1) ? "were" : "was"));
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Notice changes */
	notice_item();

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
 *    k_idx --- type of object.
 */
bool potion_smash_effect(int who, int x, int y, object_type *o_ptr)
{
	int k_idx = o_ptr->k_idx;

	bool ident = FALSE;
	bool angry = FALSE;

	object_kind *k_ptr = &k_info[k_idx];

	bool result = FALSE;
	apply_object_trigger(TRIGGER_SMASH, o_ptr, "iii:bb",
			LUA_VAR(who), LUA_VAR(x), LUA_VAR(y),
			LUA_RETURN(result), LUA_RETURN(ident));
	angry = result;
		
	/* An identification was made */
	if (ident && !(k_ptr->aware))
	{
		k_ptr->aware = TRUE;
		gain_exp((k_ptr->level + p_ptr->lev / 2) / p_ptr->lev);
	}

	/* Notice changes */
	notice_item();

	return (angry);
}


/*
 * Hack -- Display all known spells in a window
 *
 * XXX XXX XXX Need more color coding.
 */
void display_spell_list(void)
{
	int i, j;
	int y = 0, x = 0;
	int use_realm[2];
	const magic_type *s_ptr;
	char name[80];
	char out_val[160];
	int row = 0, col = 0;
	unsigned int max_wid = 0;

	use_realm[0] = p_ptr->spell.r[0].realm - 1;
	use_realm[1] = p_ptr->spell.r[1].realm - 1;
	
	/* Erase window */
	clear_from(0);

	/* Warriors are illiterate */
	if (!mp_ptr->spell_book) return;

	/* Mindcrafter spell-list */
	if (p_ptr->rp.pclass == CLASS_MINDCRAFTER)
	{
		int minfail;
		int plev = p_ptr->lev;
		int chance;
		mindcraft_power spell;
		char comment[80];

		/* Display a list of spells */
		put_fstr(x + 3, y, "Name");
		put_fstr(x + 33, y, "Lv Mana Fail Info");

		/* Dump the spells */
		for (i = 0; (i < MINDCRAFT_MAX) && (i < Term->hgt - 1); i++)
		{
			cptr a = CLR_WHITE;

			/* Access the available spell */
			spell = mindcraft_powers[i];
			if (spell.min_lev > plev) break;

			/* Get the failure rate */
			chance = spell.fail;

			/* Reduce failure rate by "effective" level adjustment */
			chance -= 3 * (p_ptr->lev - spell.min_lev);

			/* Reduce failure rate by INT/WIS adjustment */
			chance -= adj_mag_stat[p_ptr->stat[mp_ptr->spell_stat].ind] - 3;

			/* Not enough mana to cast */
			if (spell.mana_cost > p_ptr->csp)
			{
				chance += 5 * (spell.mana_cost - p_ptr->csp);
				a = CLR_ORANGE;
			}

			/* Extract the minimum failure rate */
			minfail = adj_mag_fail[p_ptr->stat[mp_ptr->spell_stat].ind];

			/* Minimum failure rate */
			if (chance < minfail) chance = minfail;

			/* Stunning makes spells harder */
			if (p_ptr->tim.stun > 50) chance += 25;
			else if (p_ptr->tim.stun) chance += 15;

			/* Always a 5 percent chance of working */
			if (chance > 95) chance = 95;

			/* Get info */
			mindcraft_info(comment, i);

			/* Dump the spell */
			put_fstr(x, y + i + 1, "%s%c) %-30s%2d %4d %3d%%%s",
					a, I2A(i), spell.name,
					spell.min_lev, spell.mana_cost, chance, comment);
		}

		return;
	}

	/* Normal spellcaster with books */

	/* Scan books */
	for (j = 0; j < ((use_realm[1] > -1) ? 2 : 1); j++)
	{
		int n = 0;

		/* Scan spells */
		for (i = 0; i < 32; i++)
		{
			cptr a = CLR_WHITE;

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm[j]][i % 32];

			strcpy(name,
				   spell_names[use_realm[j]][i % 32]);

			/* Illegible */
			if (s_ptr->slevel >= 99)
			{
				/* Illegible */
				strcpy(name, "(illegible)");

				/* Unusable */
				a = CLR_L_DARK;
			}

			/* Forgotten */
			else if (p_ptr->spell.r[j].forgotten & (1L << (i % 32)))
			{
				/* Forgotten */
				a = CLR_ORANGE;
			}

			/* Unknown */
			else if (!(p_ptr->spell.r[j].learned & (1L << (i % 32))))
			{
				/* Unknown */
				a = CLR_RED;
			}

			/* Untried */
			else if (!(p_ptr->spell.r[j].worked & (1L << (i % 32))))
			{
				/* Untried */
				a = CLR_YELLOW;
			}

			/* Dump the spell --(-- */
			strnfmt(out_val, 160, "%c/%c) %s", I2A(n / 8), I2A(n % 8), name);

			max_wid = MAX(max_wid, strlen(out_val) + 1);

			/* Dump onto the window */
			put_fstr(col, row, "%s%s", a, out_val);

			/* Next row */
			row++;

			if (row >= Term->hgt)
			{
				row = 0;
				col += max_wid;
				max_wid = 0;
			}

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
	int chance, minfail;
	const magic_type *s_ptr;
	int smana;


	/* Paranoia -- must be literate */
	if (!mp_ptr->spell_book) return (100);

	/* Access the spell */
	s_ptr = &mp_ptr->info[realm][spell];

	/* Extract the base spell failure rate */
	if (realm == REALM_ARCANE-1)
		chance = s_ptr->slevel + 20;
	else
		chance = s_ptr->slevel * 3 / 2 + 20;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[p_ptr->stat[mp_ptr->spell_stat].ind];

	/* Get mana cost */
	smana = spell_mana(spell, realm);

	/* Not enough mana to cast */
	if (smana > p_ptr->csp)
	{
		chance += 5 * (smana - p_ptr->csp);
	}

	/* Some mutations increase spell failure */
	if ((p_ptr->muta3 & MUT3_MAGIC_RES) || (p_ptr->muta1 & MUT1_EAT_MAGIC))
	{
		chance += 5;
	}

	if (realm == REALM_DEATH-1 && (p_ptr->muta1 & MUT1_BANISH))
	{
		chance += 10;
	}

	if (p_ptr->muta3 & MUT3_SILLY_VOI)
	{
		chance += s_ptr->slevel;
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat[mp_ptr->spell_stat].ind];

	/*
	 * Non mage/priest characters never get too good
	 * (added high mage, mindcrafter)
	 */
	if ((p_ptr->rp.pclass != CLASS_PRIEST) &&
		(p_ptr->rp.pclass != CLASS_MAGE) &&
		(p_ptr->rp.pclass != CLASS_MINDCRAFTER) &&
		(p_ptr->rp.pclass != CLASS_HIGH_MAGE))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
	if ((p_ptr->rp.pclass == CLASS_PRIEST) && p_ptr->state.icky_wield) chance += 25;

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->tim.stun > 50) chance += 25;
	else if (p_ptr->tim.stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Returns spell mana cost for spell
 */
int spell_mana(int spell, int realm)
{
	const magic_type *s_ptr;
	int smana;


	/* Paranoia -- must be literate */
	if (!mp_ptr->spell_book) return (100);

	/* Access the spell */
	s_ptr = &mp_ptr->info[realm][spell];

	smana = s_ptr->smana;

	/* Chaos patrons improve chaos magic */
	if ((realm == REALM_CHAOS - 1) && (FLAG(p_ptr, TR_PATRON)))
	{
		smana = (smana * 2 + 2) / 3;
	}

	return (smana);
}



/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known, int realm)
{
	const magic_type *s_ptr;

	/* Access the spell */
	s_ptr = &mp_ptr->info[realm][spell];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if ((realm == p_ptr->spell.r[1].realm - 1) ?
		(p_ptr->spell.r[1].forgotten & (1L << spell)) :
		(p_ptr->spell.r[0].forgotten & (1L << spell)))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if ((realm == p_ptr->spell.r[1].realm - 1) ?
		(p_ptr->spell.r[1].learned & (1L << spell)) :
		(p_ptr->spell.r[0].learned & (1L << spell)))
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
void spell_info(char *p, int spell, int realm)
{
	/* Default */
	p[0] = 0;

	{
		int plev = p_ptr->lev;

		/* See below */
		int orb = (plev / ((p_ptr->rp.pclass == CLASS_PRIEST ||
							p_ptr->rp.pclass == CLASS_HIGH_MAGE) ? 2 : 4));

		/* Analyze the spell */
		switch (realm)
		{
			case 0:
			{
				/* Life */
				switch (spell)
				{
					case 1:
					{
						strcpy(p, " heal 2d10");
						break;
					}
					case 2:
					{
						/* Actually rand_range(12,24) */
						strcpy(p, " dur 12+d12 turns");
						break;
					}
					case 4:
					{
						strnfmt(p, 80, " dam 2d%d", (plev / 2));
						break;
					}
					case 6:
					{
						strcpy(p, " heal 4d10");
						break;
					}
					case 10:
					{
						strcpy(p, " heal 8d10");
						break;
					}
					case 11:
					{
						/* Actually rand_range(24,48) */
						strcpy(p, " dur 24+d24");
						break;
					}
					case 12:
					{
						strnfmt(p, 80, " dam %d+3d6", plev + orb);
						break;
					}
					case 13:
					{
						strnfmt(p, 80, " dur %d+d25", 3 * plev);
						break;
					}
					case 14:
					{
						strcpy(p, " heal 300");
						break;
					}
					case 16:
					{
						strnfmt(p, 80, " dam %d+%d", plev, plev);
						break;
					}
					case 18:
					{
						strnfmt(p, 80, " dam %d+%d", 3 * plev, 3 * plev);
						break;
					}
					case 20:
					{
						strnfmt(p, 80, " dam %d", 4 * plev);
						break;
					}
					case 22:
					{
						strnfmt(p, 80, " d %d/h 1000", 4 * plev);
						break;
					}
					case 24:
					{
						/* Actually rand_range(25,50) */
						strcpy(p, " dur 25+d25");
						break;
					}
					case 25:
					{
						/* Actually rand_range(50,100) */
						strcpy(p, " dur 50+d50");
						break;
					}
					case 28:
					{
						strcpy(p, " heal 2000");
						break;
					}
					case 30:
					{
						strnfmt(p, 80, " h300/d%d+388", plev * 4);
						break;
					}
					case 31:
					{
						/* Actually rand_range(7,14) */
						strcpy(p, " dur 7+d7");
						break;
					}
				}
				break;
			}

			case 1:
			{
				/* Sorcery */
				switch (spell)
				{
					case 1:
					{
						strcpy(p, " range 10");
						break;
					}
					case 3:
					{
						strnfmt(p, 80, " dam 2d%d", (plev / 2));
						break;
					}
					case 5:
					{
						strnfmt(p, 80, " range %d", plev * 5);
						break;
					}
					case 13:
					{
						strnfmt(p, 80, " dur %d+d%d", plev, plev + 20);
						break;
					}
					case 19:
					{
						strnfmt(p, 80, " range %d", plev + 2);
						break;
					}
					case 20:
					{
						/* Actually rand_range(25,55) */
						strcpy(p, " dur 25+d30");
						break;
					}
					case 23:
					{
						strcpy(p, " delay 15+d21");
						break;
					}
					case 25:
					{
						strnfmt(p, 80, " max wgt %d", plev * 15 / 10);
						break;
					}
					case 26:
					{
						strnfmt(p, 80, " dam %d+7d7", plev / 2);
						break;
					}
					case 27:
					{
						/* Actually rand_range(25,55) */
						strcpy(p, " dur 25+d30");
						break;
					}
					case 31:
					{
						/* Actually rand_range(8,16) */
						strcpy(p, " dur 8+d8");
						break;
					}
				}
				break;
			}

			case 2:
			{
				/* Nature */
				switch (spell)
				{
					case 1:
					{
						strcpy(p, " heal 2d8");
						break;
					}
					case 4:
					{
						strnfmt(p, 80, " dam 2d%d", (plev / 2));
						break;
					}
					case 6:
					{
						/* Actually rand_range(20,40) */
						strcpy(p, " dur 20+d20");
						break;
					}
					case 9:
					{
						strnfmt(p, 80, " dam %dd8", (3 + ((plev - 5) / 4)));
						break;
					}
					case 11:
					{
						strnfmt(p, 80, " dam %dd8", (5 + ((plev - 5) / 4)));
						break;
					}
					case 12:
					{
						strcpy(p, " dam 6d8");
						break;
					}
					case 15:
					{
						strcpy(p, " heal 1000");
						break;
					}
					case 18:
					{
						/* Actually rand_range(30,50) */
						strcpy(p, " dur 20+d30");
						break;
					}
					case 19:
					{
						/* Actually rand_range(20,40) */
						strcpy(p, " dur 20+d20");
						break;
					}
					case 24:
					{
						strcpy(p, " rad 10");
						break;
					}
					case 26:
					{
						strnfmt(p, 80, " dam %d", 70 + plev);
						break;
					}
					case 27:
					{
						strnfmt(p, 80, " dam %d", 90 + plev);
						break;
					}
					case 28:
					{
						strnfmt(p, 80, " dam %d", 100 + plev);
						break;
					}
					case 29:
					{
						strcpy(p, " dam 75");
						break;
					}
					case 31:
					{
						strnfmt(p, 80, " dam %d+%d", 4 * plev, (100 + plev) / 2);
						break;
					}
				}
				break;
			}

			case 3:
			{
				/* Chaos */
				switch (spell)
				{
					case 0:
					{
						strnfmt(p, 80, " dam %dd4", 3 + ((plev - 1) / 5));
						break;
					}
					case 2:
					{
						strnfmt(p, 80, " dam 2d%d", (plev / 2));
						break;
					}
					case 4:
					{
						strnfmt(p, 80, " dam %d+3d5", plev + (plev /
														  (((p_ptr->rp.pclass ==
															 CLASS_MAGE)
															|| (p_ptr->rp.pclass ==
																CLASS_HIGH_MAGE))
														   ? 2 : 4)));
						break;
					}
					case 5:
					{
						strnfmt(p, 80, " dam %dd8", (8 + ((plev - 5) / 4)));
						break;
					}
					case 6:
					{
						strnfmt(p, 80, " dam %dd8", (8 + ((plev - 5) / 4)));
						break;
					}
					case 7:
					{
						strnfmt(p, 80, " range %d", plev * 5);
						break;
					}
					case 8:
					{
						strcpy(p, " random");
						break;
					}
					case 9:
					{
						strnfmt(p, 80, " dam %dd8", (10 + ((plev - 5) / 4)));
						break;
					}
					case 10:
					{
						strnfmt(p, 80, " dam %d", (45 + plev) / 2);
						break;
					}
					case 11:
					{
						strnfmt(p, 80, " dam %dd8", (11 + ((plev - 5) / 4)));
						break;
					}
					case 12:
					{
						strnfmt(p, 80, " dam %d", 55 + plev);
						break;
					}
					case 15:
					{
						strnfmt(p, 80, " dam %d", 66 + plev);
						break;
					}
					case 17:
					{
						strnfmt(p, 80, " dam %dd8", (5 + (plev / 10)));
						break;
					}
					case 19:
					{
						strnfmt(p, 80, " dam %d", 80 + plev);
						break;
					}
					case 24:
					{
						strnfmt(p, 80, " dam %dd8", (9 + ((plev - 5) / 4)));
						break;
					}
					case 25:
					{
						strnfmt(p, 80, " dam %d each", (3 * plev) / 2);
						break;
					}
					case 26:
					{
						strnfmt(p, 80, " dam %d", 75 + plev);
						break;
					}
					case 27:
					{
						strcpy(p, " dam 75 / 150");
						break;
					}
					case 28:
					{
						strnfmt(p, 80, " dam %d", 120 + plev);
						break;
					}
					case 29:
					{
						strnfmt(p, 80, " dam %d", 300 + (plev * 2));
						break;
					}
					case 30:
					{
						strnfmt(p, 80, " dam %d", p_ptr->chp);
						break;
					}
					case 31:
					{
						strcpy(p, " dam 3 * 175");
						break;
					}
				}
				break;
			}

			case 4:
			{
				/* Death */
				switch (spell)
				{
					case 1:
					{
						strnfmt(p, 80, " dam %dd3", (3 + ((plev - 1) / 5)));
						break;
					}
					case 3:
					{
						strnfmt(p, 80, " dam %d", 10 + (plev / 2));
						break;
					}
					case 5:
					{
						/* Actually rand_range(20,40) */
						strnfmt(p, 80, " dur 20+d20");
						break;
					}
					case 8:
					{
						strnfmt(p, 80, " dam %d+3d6", plev +
								(plev / (((p_ptr->rp.pclass == CLASS_MAGE) ||
										  (p_ptr->rp.pclass ==
										   CLASS_HIGH_MAGE)) ? 2 : 4)));
						break;
					}
					case 9:
					{
						strnfmt(p, 80, " dam %dd8", (6 + ((plev - 5) / 4)));
						break;
					}
					case 11:
					{
						strnfmt(p, 80, " dm %d+%d*d15", plev, MAX(1, plev / 10));
						break;
					}
					case 13:
					{
						strnfmt(p, 80, " dam %d", 4 * plev);
						break;
					}
					case 16:
					{
						/* Actually rand_range(25,50) */
						strcpy(p, " dur 25+d25");
						break;
					}
					case 17:
					{
						strcpy(p, " random");
						break;
					}
					case 18:
					{
						strnfmt(p, 80, " dam %dd8", (4 + ((plev - 5) / 4)));
						break;
					}
					case 19:
					{
						/* This is too complicated to give accurately */
						strcpy(p, " max dur 50");
						break;
					}
					case 20:
					{
						strcpy(p, " dam 3*100");
						break;
					}
					case 22:
					{
						strcpy(p, " dam 120");
						break;
					}
					case 27:
					{
						strnfmt(p, 80, " dam %d", plev * 3);
						break;
					}
					case 28:
					{
						strnfmt(p, 80, " dam %d", plev * 4);
						break;
					}
					case 29:
					{
						strcpy(p, " dam 666");
						break;
					}
					case 31:
					{
						/* Actually rand_range(plev/2,plev) */
						strnfmt(p, 80, " dur %d+d%d", (plev / 2), (plev / 2));
						break;
					}
				}
				break;
			}

			case 5:
			{
				/* Trump */
				switch (spell)
				{
					case 0:
					{
						strcpy(p, " range 10");
						break;
					}
					case 1:
					{
						strnfmt(p, 80, " dam %dd3", 3 + ((plev - 1) / 5));
						break;
					}
					case 2:
					{
						strcpy(p, " random");
						break;
					}
					case 4:
					{
						strnfmt(p, 80, " range %d", plev * 4);
						break;
					}
					case 5:
					{
						strnfmt(p, 80, " range %d", plev + 2);
						break;
					}
					case 6:
					{
						/* Actually rand_range(25,55) */
						strcpy(p, " dur 25+d30");
						break;
					}
					case 8:
					{
						strnfmt(p, 80, " max wgt %d", plev * 15 / 10);
						break;
					}
					case 14:
					{
						strcpy(p, " delay 15+d21");
						break;
					}
					case 22:
					{
						strnfmt(p, 80, " dam %d", plev * 3);
						/* break; */
					}
				}
				break;
			}

			case 6:
			{
				/* Arcane */
				switch (spell)
				{
					case 0:
					{
						strnfmt(p, 80, " dam %dd3", 3 + ((plev - 1) / 5));
						break;
					}
					case 4:
					{
						strcpy(p, " range 10");
						break;
					}
					case 5:
					{
						strnfmt(p, 80, " dam 2d%d", plev / 2);
						break;
					}
					case 7:
					{
						strcpy(p, " heal 2d8");
						break;
					}
					case 14:
					case 15:
					case 16:
					case 17:
					{
						/* Actually rand_range(20,40) */
						strcpy(p, " dur 20+d20");
						break;
					}
					case 18:
					{
						strcpy(p, " heal 4d8");
						break;
					}
					case 19:
					{
						strnfmt(p, 80, " range %d", plev * 5);
						break;
					}
					case 21:
					{
						strcpy(p, " dam 6d8");
						break;
					}
					case 23:
					{
						/* Actually rand_range(24,48) */
						strcpy(p, " dur 24+d24");
						break;
					}
					case 28:
					{
						strnfmt(p, 80, " dam %d", 75 + plev);
						break;
					}
					case 30:
					{
						strcpy(p, " delay 15+d21");
						break;
					}
					case 31:
					{
						/* Actually rand_range(25,55) */
						strcpy(p, " dur 25+d30");
						break;
					}
				}
				break;
			}

			default:
			{
				strnfmt(p, 80, "Unknown type: %d.", realm);
			}
		}
	}
}


/*
 * Print a list of spells (for browsing or casting or viewing)
 */
void print_spells(byte *spells, int num, int x, int y, int realm)
{
	int i, spell;
	const magic_type *s_ptr;
	cptr comment;
	char info[80];

	if (((realm < 0) || (realm >= MAX_REALM)) && p_ptr->state.wizard)
		msgf("Warning! print_spells called with null realm");

	/* Title the list */
	prtf(x, y, "");
	put_fstr(x + 5, y, "Name");
	put_fstr(x + 35, y, "Lv Mana Fail Info");


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
			prtf(x, y + i + 1, CLR_L_DARK "  %c) %-30s",
					 I2A(i), "(illegible)");
			continue;
		}

		/* XXX XXX Could label spells above the players level */

		/* Get extra info */
		spell_info(info, spell, realm);

		/* Use that info */
		comment = info;

		/* Analyze the spell */
		if ((realm + 1 != p_ptr->spell.r[0].realm) && (realm + 1 != p_ptr->spell.r[1].realm))
		{
			comment = CLR_SLATE " uncastable";
		}

		/* We know these books */
		else if ((realm + 1 == p_ptr->spell.r[0].realm) ?
				 ((p_ptr->spell.r[0].forgotten & (1L << spell))) :
				 ((p_ptr->spell.r[1].forgotten & (1L << spell))))
		{
			comment = CLR_YELLOW " forgotten";
		}
		else if (!((realm + 1 == p_ptr->spell.r[0].realm) ?
				   (p_ptr->spell.r[0].learned & (1L << spell)) :
				   (p_ptr->spell.r[1].learned & (1L << spell))))
		{
			comment = CLR_L_BLUE " unknown";
		}
		else if (!((realm + 1 == p_ptr->spell.r[0].realm) ?
				   (p_ptr->spell.r[0].worked & (1L << spell)) :
				   (p_ptr->spell.r[1].worked & (1L << spell))))
		{
			comment = CLR_L_GREEN " untried";
		}

		/* Dump the spell --(-- */
		prtf(x, y + i + 1, "  %c) %-30s%2d %4d %3d%%%s",
				I2A(i), spell_names[realm][spell],
				(int)s_ptr->slevel, spell_mana(spell, realm),
				spell_chance(spell, realm), comment);
	}

	/* Clear the bottom line */
	prtf(x, y + i + 1, "");
}


/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind.  Also sling ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(const object_type *o_ptr)
{
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
			/* Analyze the type */
			return (TRUE);
		}

		case TV_STAFF:
		case TV_SCROLL:
		{
			/* Staffs/Scrolls are wood/paper */
			return (TRUE);
		}

		case TV_CHEST:
		{
			/* Ouch */
			return (TRUE);
		}

		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			/* Junk is useless */
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
bool hates_elec(const object_type *o_ptr)
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
bool hates_fire(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
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
			/* Wearable */
			return (TRUE);
		}

		case TV_LIFE_BOOK:
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
		{
			/* Books */
			return (TRUE);
		}

		case TV_CHEST:
		{
			/* Chests */
			return (TRUE);
		}

		case TV_STAFF:
		case TV_SCROLL:
		{
			/* Staffs/Scrolls burn */
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
bool hates_cold(const object_type *o_ptr)
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
	if (!hates_acid(o_ptr)) return (FALSE);
	if (FLAG(o_ptr, TR_IGNORE_ACID)) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
int set_elec_destroy(object_type *o_ptr)
{
	if (!hates_elec(o_ptr)) return (FALSE);
	if (FLAG(o_ptr, TR_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}


/*
 * Burn something
 */
int set_fire_destroy(object_type *o_ptr)
{
	if (!hates_fire(o_ptr)) return (FALSE);
	if (FLAG(o_ptr, TR_IGNORE_FIRE)) return (FALSE);
	return (TRUE);
}


/*
 * Freeze things
 */
int set_cold_destroy(object_type *o_ptr)
{
	if (!hates_cold(o_ptr)) return (FALSE);
	if (FLAG(o_ptr, TR_IGNORE_COLD)) return (FALSE);
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
	int j, k, amt;
	object_type *o_ptr;

	int slot;


	/* Count the casualties */
	k = 0;

	/* Scan the inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Hack -- for now, skip artifacts */
		if (FLAG(o_ptr, TR_INSTA_ART)) continue;

		/* Give this item slot a shot at death */
		if ((*typ) (o_ptr))
		{
			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (randint0(100) < perc) amt++;
			}

			/* Some casualities */
			if (amt)
			{
				/* Get slot */
				slot = get_item_position(p_ptr->inventory, o_ptr);

				/* Message */
				msgf("%sour %v (%c) %s destroyed!",
						   ((o_ptr->number > 1) ?
							((amt == o_ptr->number) ? "All of y" :
							 (amt > 1 ? "Some of y" : "One of y")) : "Y"),
						   OBJECT_FMT(o_ptr, FALSE, 3), I2A(slot),
						    ((amt > 1) ? "were" : "was"));

				/* Potions smash open */
				if (object_is_potion(o_ptr))
				{
					int px = p_ptr->px;
					int py = p_ptr->py;

					(void)potion_smash_effect(0, px, py, o_ptr);
				}

				/* Reduce the charges of rods/wands */
				reduce_charges(o_ptr, amt);

				/* Destroy "amt" items */
				item_increase(o_ptr, -amt);

				/* Count the casualties */
				k += amt;
			}
		}
	}
	OBJ_ITT_END;

	/* Return the casualty count */
	return (k);
}


bool rustproof(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour_no_acid;

	/* Get an item */
	q = "Rustproof which piece of armour? ";
	s = "You have nothing to rustproof.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	SET_FLAG(o_ptr, TR_IGNORE_ACID);
	o_ptr->kn_flags[2] |= TR2_IGNORE_ACID;

	if ((o_ptr->to_a < 0) && !(cursed_p(o_ptr)))
	{
		msgf("The %v look%s as good as new!", OBJECT_FMT(o_ptr, FALSE, 0),
				   ((o_ptr->number > 1) ? "" : "s"));
		o_ptr->to_a = 0;
	}

	msgf("The %v %s now protected against corrosion.", OBJECT_FMT(o_ptr, FALSE, 0),
			   ((o_ptr->number > 1) ? "are" : "is"));

	return TRUE;
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;


	/* Curse the body armor */
	o_ptr = &p_ptr->equipment[EQUIP_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Attempt a saving throw for artifacts */
	if ((FLAG(o_ptr, TR_INSTA_ART)) && !one_in_(3))
	{
		/* Cool */
		msgf("A terrible black aura tries to surround your armor, but your %v resists the effects!",
			OBJECT_FMT(o_ptr, FALSE, 3));
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msgf("A terrible black aura blasts your %v!", OBJECT_FMT(o_ptr, FALSE, 3));

		chg_virtue(V_ENCHANT, -5);

		/* Blast the armor */
		o_ptr->to_a = 0 - (s16b)rand_range(5, 10);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->flags[0] = 0;
		o_ptr->flags[1] = 0;
		o_ptr->flags[2] = 0;
		o_ptr->flags[3] = 0;
		
		/* Lose your feeling */
		o_ptr->feeling = FEEL_NONE;

		add_ego_flags(o_ptr, EGO_BLASTED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;

	/* Curse the weapon */
	o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);

	/* Attempt a saving throw */
	if ((FLAG(o_ptr, TR_INSTA_ART)) && !one_in_(3))
	{
		/* Cool */
		msgf("A terrible black aura tries to surround your weapon, but your %v resists the effects!",
				OBJECT_FMT(o_ptr, FALSE, 3));
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msgf("A terrible black aura blasts your %v!", OBJECT_FMT(o_ptr, FALSE, 3));

		chg_virtue(V_ENCHANT, -5);

		/* Shatter the weapon */
		o_ptr->to_h = 0 - (s16b)rand_range(5, 10);
		o_ptr->to_d = 0 - (s16b)rand_range(5, 10);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 1;
		o_ptr->ds = 1;
		o_ptr->flags[0] = 0;
		o_ptr->flags[1] = 0;
		o_ptr->flags[2] = 0;
		o_ptr->flags[3] = 0;
		
		/* Lose your feeling */
		o_ptr->feeling = FEEL_NONE;

		add_ego_flags(o_ptr, EGO_SHATTERED);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();
	}

	/* Notice */
	return (TRUE);
}


/*
 * Enchant some bolts
 */
bool brand_bolts(void)
{
	object_type *o_ptr;

	/* Use the first acceptable bolts */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (o_ptr->xtra_name) continue;

		/* Skip cursed/broken items */
		if (cursed_p(o_ptr) || !o_ptr->cost) continue;

		/* Randomize */
		if (randint0(100) < 75) continue;

		/* Message */
		msgf("Your bolts are covered in a fiery aura!");

		/* Ego-item */
		add_ego_flags(o_ptr, EGO_FLAME);

		/* Enchant */
		(void)enchant(o_ptr, rand_range(2, 6), ENCH_TOHIT | ENCH_TODAM);
		
		/* Notice changes */
		notice_inven();

		/* Notice */
		return (TRUE);
	}
	OBJ_ITT_END;

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msgf("The fiery enchantment failed.");

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
	if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_QUESTOR))
		return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint1(20) / randint1(9)) + 1);
	lev2 = r_ptr->level + ((randint1(20) / randint1(9)) + 1);

	/* Pick a (possibly new) non-unique race */
	for (i = 0; i < 1000; i++)
	{
		/* Pick a new race, using a level calculation */
		r = get_mon_num((p_ptr->depth + r_ptr->level) / 2 + 5);

		/* Handle failure */
		if (!r) break;

		/* Obtain race */
		r_ptr = &r_info[r];

		/* Ignore unique monsters */
		if (FLAG(r_ptr, RF_UNIQUE)) continue;

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


bool polymorph_monster(int x, int y)
{
	cave_type *c_ptr = area(x, y);
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
		if (place_monster_aux(x, y, new_r_idx, FALSE, FALSE, friendly, pet, TRUE))
		{
			/* Success */
			polymorphed = TRUE;
		}
		else
		{
			/* Placing the new monster failed - use the old. */
			(void)place_monster_aux(x, y, old_r_idx, FALSE, FALSE, friendly,
									pet, TRUE);
		}
	}

	/* Update some things */
	p_ptr->update |= (PU_MON_LITE);

	return (polymorphed);
}


/*
 * Dimension Door
 */
bool dimension_door(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int plev = p_ptr->lev;
	int x = 0, y = 0;
	cave_type *c_ptr;

	if (!tgt_pt(&x, &y)) return FALSE;

	p_ptr->energy -= 60 - plev;

	/* paranoia */
	if (!in_bounds2(x, y)) return FALSE;

	c_ptr = area(x, y);

	if (!cave_empty_grid(c_ptr) || (c_ptr->info & CAVE_ICKY) ||
		(distance(x, y, px, py) > plev + 2) || (one_in_(plev * plev / 2)))
	{
		msgf("You fail to exit the astral plane correctly!");
		p_ptr->energy -= 100;
		teleport_player(10);
	}
	else
		teleport_player_to(x, y);

	return (TRUE);
}


/*
 * Map the wilderness
 */
void map_wilderness(int radius, s32b x, s32b y)
{
	int i, j;
	int dist;

	/* Map a rough circle around the target position in the wilderness */
	for (i = x - radius; i < x + radius + 1; i++)
	{
		for (j = y - radius; j < y + radius + 1; j++)
		{
			/* In bounds? */
			if ((i >= 0) && (i < max_wild - 1) && (j >= 0) && (j < max_wild - 1))
			{
				dist = distance(i, j, x, y);

				if ((randint0(dist) < radius / 2) && (dist < radius))
				{
					/* Memorise the location */
					wild[j][i].done.info |= WILD_INFO_SEEN;

					/* Alert player to a new wilderness quest */
					discover_wild_quest(place[wild[j][i].done.place].quest_num);
				}
			}
		}
	}
}


void sanity_blast(const monster_type *m_ptr)
{
	int power = 100;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	power = r_ptr->hdice * 2 + 10;

	if (!FLAG(r_ptr, RF_UNIQUE))
	{
		if (FLAG(r_ptr, RF_FRIENDS))
			power -= 50;
	}
	else
	{
		power += 50;
	}

	/* Can we see it? */
	if (!m_ptr->ml) return;

	/* Paranoia */
	if (!FLAG(r_ptr, RF_ELDRITCH_HORROR)) return;

	/* Pet eldritch horrors are safe most of the time */
	if (is_pet(m_ptr) && !one_in_(8)) return;

	/* Do we pass the saving throw? */
	if (player_save(power)) return;

	if (p_ptr->tim.image)
	{
		/* Something silly happens... */
		msgf("You behold the %s visage of %v!",
				   funny_desc[randint0(MAX_SAN_FUNNY)], MONSTER_FMT(m_ptr, 0));

		if (one_in_(3))
		{
			msgf(funny_comments[randint0(MAX_SAN_COMMENT)]);
			(void)inc_image(randint1(r_ptr->hdice * 2));
		}

		/* Never mind; we can't see it clearly enough */
		return;
	}

	/* Something frightening happens... */
	msgf("You behold the %s visage of %v!",
			   horror_desc[randint0(MAX_SAN_HORROR)], MONSTER_FMT(m_ptr, 0));

	/* Monster memory */
	r_ptr->r_flags[3] |= RF3_ELDRITCH_HORROR;

	/* Demon characters are unaffected */
	if (p_ptr->rp.prace == RACE_IMP) return;

	/* Undead characters are 50% likely to be unaffected */
	if (((p_ptr->rp.prace == RACE_SKELETON) ||
		 (p_ptr->rp.prace == RACE_ZOMBIE) ||
		 (p_ptr->rp.prace == RACE_VAMPIRE) ||
		 (p_ptr->rp.prace == RACE_SPECTRE) ||
		 (p_ptr->rp.prace == RACE_GHOUL)) && saving_throw(25 + p_ptr->lev)) return;

	/* Mind blast */
	if (!player_save(power))
	{
		if ((!(FLAG(p_ptr, TR_RES_FEAR))) || one_in_(5))
		{
			/* Get afraid, even if have resist fear! */
			(void)inc_afraid(rand_range(10, 20));
		}
		if (!(FLAG(p_ptr, TR_RES_CHAOS)))
		{
			(void)inc_image(rand_range(150, 400));
		}
		return;
	}

	if (lose_all_info())
	{
		msgf("You forget everything in your utmost terror!");
	}

	p_ptr->update |= PU_BONUS;
	handle_stuff();
}
