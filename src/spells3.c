/* File: spells3.c */

/* Purpose: Spell code (part 3)
 *
 * Included functions, taken from cmd5.c, spells1.c, and spells2.c:
 *
 * item_tester_hook_weapon(); item_tester_hook_armour();
 * item_tester_hook_weapon_armour(); item_tester_hook_ring(); 
 * item_tester_hook_amulet(); item_tester_hook_ring_amulet();
 * item_tester_hook_we_ar_ri_am(); item_tester_hook_unknown();
 * item_tester_hook_unknown_star(); rustproof(); do_poly_wounds();
 * do_poly_self(); fetch(); teleport_away(); teleport_to_player();
 * teleport_player(); teleport_player_to(); teleport_player_level();
 * mutate_player(); warding_glyph(); explosive_rune(); identify_pack();
 * restore_level(); alchemy(); genocide(); mass_genocide(); probing();
 * enchant_table[]; enchant(); enchant_spell(); ident_level();
 * ident_spell(); identify_fully(); item_tester_hook_recharge(); recharge();
 * phlogiston(); brand_weapon(); call_the_(void); wild_magic();
 *
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

/* Hook to specify "weapon" */
bool item_tester_hook_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED: case TV_POLEARM: case TV_AXE: case TV_SWORD:
		case TV_DIGGING: case TV_BOW: case TV_BOLT: case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}


/* Hook to specify "armour" */
bool item_tester_hook_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR: case TV_HARD_ARMOR: case TV_SOFT_ARMOR:
		case TV_SHIELD: case TV_CLOAK: case TV_CROWN: case TV_HELM:
		case TV_BOOTS: case TV_GLOVES:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}


/* Check if an object is weapon or armour (but not arrow, bolt, or shot) */
bool item_tester_hook_weapon_armour(object_type *o_ptr)
{
	return(item_tester_hook_weapon(o_ptr) ||
	       item_tester_hook_armour(o_ptr));
}


/* Hook to specify "ring" -- Gumby */
bool item_tester_hook_ring(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING: return (TRUE);
	}
	return (FALSE);
}


/* Hook to specify "amulet" -- Gumby */
bool item_tester_hook_amulet(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_AMULET: return (TRUE);
	}
	return (FALSE);
}


/* Check to see if the item is a ring or amulet -- Gumby */
bool item_tester_hook_ring_amulet(object_type *o_ptr)
{
	return(item_tester_hook_ring(o_ptr) ||
	       item_tester_hook_amulet(o_ptr));
}


/* Check to see if the item is a weapon, armour, ring or amulet -- Gumby */
bool item_tester_hook_we_ar_ri_am(object_type *o_ptr)
{
	return(item_tester_hook_weapon(o_ptr) ||
	       item_tester_hook_armour(o_ptr) ||
	       item_tester_hook_ring(o_ptr) ||
	       item_tester_hook_amulet(o_ptr));
}


bool item_tester_hook_unknown(object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


bool item_tester_hook_unknown_star(object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
	else
		return TRUE;
}


bool rustproof(void)
{
	int		item;
	object_type	*o_ptr;
	char		o_name[80];

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Rustproof which piece of armour? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to rustproof.");
		return (FALSE);
	}

	if (item >= 0) /* Get the item (in the pack) */

	{
		o_ptr = &inventory[item];
	}
	else /* Get the item (on the floor) */
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

	return (TRUE);
}


void do_poly_wounds(void)
{
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = (randint(5)==1);

	if (!(wounds || hit_p || Nasty_effect)) return;

	if (Nasty_effect)
	{
		msg_print("A new wound was created!");
		take_hit(change, "a polymorphed wound");
		set_cut(change);
	}
	else
	{
		msg_print("Your wounds are polymorphed into less serious ones.");
		hp_player(change);
		set_cut((p_ptr->cut)-(change/2));
	}
}


void do_poly_self(void)
{
	int effects = randint(2);
	int tmp = 0;
	int new_race;
	int more_effects = TRUE;

	msg_print("You feel a change coming over you...");

	while (effects-- && more_effects)
	{
		switch (randint(15))
		{
			case 1: case 2:
				do_cmd_rerate();
				break;
			case 3: /* Lose all mutations -- Gumby */
				if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
				{
					msg_print("All of your lovely mutations go away!");
					p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
					p_ptr->update |= PU_BONUS;
					handle_stuff();
				}
				else
				{
					msg_print("You feel as if you almost lost something...");
				}
				break;
			case 4: case 5:
				do_poly_wounds();
				break;
			case 6: case 7: case 8:
				(void) gain_random_mutation(0);
				break;
			case 9: /* Racial polymorph! Uh oh... */
				{
					do
					{
						new_race = randint(MAX_RACES) -1;
					}
					while (new_race == p_ptr->prace);

					msg_format("You turn into a%s %s!",
					    ((new_race == RACE_ELF)?"n":""),
					    race_info[new_race].title);

					p_ptr->prace = new_race;
					rp_ptr = &race_info[p_ptr->prace];

					/* Experience factor */
					p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

					/* Change gender sometimes - G */
					p_ptr->psex = rand_int(1);

					/* Calculate new height/weight */
					if (p_ptr->psex == SEX_MALE)
					{
						p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
						p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
					}
					else if (p_ptr->psex == SEX_FEMALE)
					{
						p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
						p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
					}

					check_experience();
					p_ptr->max_plv = p_ptr->lev;
					p_ptr->redraw |= (PR_BASIC);
					p_ptr->update |= (PU_BONUS);
					handle_stuff();
				}
				lite_spot(py, px);
				more_effects = FALSE; /* Stop here! */
				break;
			case 10: case 11:
				lose_mutation(0);
				break;
			case 12: /* Purposedly "leaks" into default */
				msg_print("You polymorph into an abomination!");
				while (tmp < 6)
				{
					(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
					tmp++;
				}

				if (randint(6)==1)
				{
					msg_print("You find living difficult in your present form!");
					take_hit(damroll(randint(p_ptr->lev),p_ptr->lev), "a lethal mutation");
				}
				/* No break; here! */
			default:
				mutate_player();
		}
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

		if (!c_ptr->o_idx)
		{
			msg_print("There's nothing there to fetch!");
			return;
		}

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
		flag = FALSE;

		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];

			if ((distance(py, px, ty, tx)> MAX_RANGE)
			    || !cave_floor_bold(ty, tx)) return;
		}
		while(!c_ptr->o_idx);
	}

	o_ptr = &o_list[c_ptr->o_idx];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}

	i = c_ptr->o_idx;
	c_ptr->o_idx = 0;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->iy = py;
	o_ptr->ix = px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s appears at your feet.", o_name);

	note_spot(py,px);
	p_ptr->redraw |= PR_MAP;
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
	int ny, nx, oy, ox, d, i, min;
	int dis = 2;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];
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
		new_level_flag = TRUE;
	}
	else if (is_quest(dun_level, FALSE) || (dun_level >= MAX_DEPTH-1) ||
		 (p_ptr->astral && (dun_level == 97)))
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
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}


void explosive_rune(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_MINOR_GLYPH);
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

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}
}


/*
 * Restores any drained experience
 */
bool restore_level(void)
{
	/* Restore experience */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Message */
		msg_print("You feel your life energies returning.");

		/* Restore the experience */
		p_ptr->exp = p_ptr->max_exp;

		/* Check the experience */
		check_experience();

		/* Did something */
		return (TRUE);
	}

	/* No effect */
	return (FALSE);
}


/* Turns an object into 1/2 its value in gold */
bool alchemy(void)
{
	int item, amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[80];
	char out_val[160];

	/* Hack -- force destruction */
	if (command_arg > 0) force = TRUE;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Turn which item to gold? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to turn to gold.");
		return FALSE;
	}

	if (item >= 0) /* Get the item (in the pack) */
	{
		o_ptr = &inventory[item];
	}
	else /* Get the item (on the floor) */
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
		if (!((auto_destroy) && (object_value(o_ptr)<1)))
		{
			/* Make a verification */
			sprintf(out_val, "Really turn %s to gold? ", o_name);
			if (!get_check(out_val)) return FALSE;
		}
	}

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr) || o_ptr->art_name)
	{
		cptr feel = "special";

		/* Message */
		msg_format("You fail to turn %s to gold!", o_name);

		/* Hack -- Handle icky artifacts */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = "terrible";

		/* Hack -- inscribe the artifact */
		o_ptr->note = quark_add(feel);

		/* We have "felt" it (again) */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return FALSE;
	}

	price = object_value_real(o_ptr);

	if (price <= 0)
		/* Message */
		msg_format("You turn %s to fool's gold.", o_name);
	else
	{
		price /= 2;

		if (amt > 1) price *= amt;

		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

	}

	if (item >= 0) /* Eliminate the item (from the pack) */
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
	else /* Eliminate the item (from the floor) */
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}


/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
bool genocide(bool player_cast)
{
	int     i;
	char    typ;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;

	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (r_ptr->flags1 & RF1_QUESTOR ) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		if (player_cast)
		{
			/* Take damage */
			take_hit(randint(4), "the strain of casting Genocide");
		}

		/* Visual feedback */
		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

		/* Take note */
		result = TRUE;
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(bool player_cast)
{
	int     i;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;


	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (r_ptr->flags1 & RF1_QUESTOR) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		if (player_cast)
		{
			/* Hack -- visual feedback */
			take_hit(randint(3), "the strain of casting Mass Genocide");
		}

		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

		/* Note effect */
		result = TRUE;
	}

	return (result);
}


/* Find out information about monsters in line of sight */
bool probing(void)
{
	int     i;

	bool    probe = FALSE;


	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Start the message */
			if (!probe) msg_print("Probing...");

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(i);

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
		msg_print("That's all.");
	}

	/* Result */
	return (probe);
}


/*
 * Used by the "enchant" function (chance of failure)
 * (modified for Zangband, we need better stuff there...) -- TY
 */
static int enchant_table[21] =
{
	  0,  50, 100, 150, 200,
	300, 400, 500, 600, 700,
	900, 905, 910, 915, 920,
	930, 950, 970, 980, 990,
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
	int i, chance, prob;
	bool res = FALSE;
	bool a = (artifact_p(o_ptr) || o_ptr->art_name);
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) || (o_ptr->tval == TV_ARROW) ||
	    (o_ptr->tval == TV_SHOT))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i=0; i<n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (rand_int(prob) >= 100) continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0)
				chance = 0;
			else if (o_ptr->to_h > (20 - o_ptr->pval))
				chance = 1000;
			else
				chance = enchant_table[o_ptr->to_h];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_h++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);

					if (o_ptr->art_flags3 & (TR3_CURSED))
					    o_ptr->art_flags3 &= ~(TR3_CURSED);
					if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE))
					    o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

					o_ptr->note = quark_add("uncursed");
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0)
				chance = 0;
			else if (o_ptr->to_d > (20 - o_ptr->pval))
				chance = 1000;
			else
				chance = enchant_table[o_ptr->to_d];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_d++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);

					if (o_ptr->art_flags3 & (TR3_CURSED))
					    o_ptr->art_flags3 &= ~(TR3_CURSED);
					if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE))
					    o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

					o_ptr->note = quark_add("uncursed");
				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0)
				chance = 0;
			else if (o_ptr->to_a > (20 - o_ptr->pval))
				chance = 1000;
			else
				chance = enchant_table[o_ptr->to_a];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_a++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE);

					if (o_ptr->art_flags3 & (TR3_CURSED))
					    o_ptr->art_flags3 &= ~(TR3_CURSED);
					if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE))
					    o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

					o_ptr->note = quark_add("uncursed");
				}
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
	int item;
	bool okay = FALSE;
	object_type *o_ptr;
	char o_name[80];

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Enchant which item? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to enchant.");
		return (FALSE);
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
	}

	/* Something happened */
	return (TRUE);
}


/* Identifies *all* items on the level -- Gumby */
bool ident_level(void)
{
	int i;
	bool success = FALSE;

	/* Memorize objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		success = TRUE;
	}

	/* At least one item was identified */
	return (success);
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

	/* Only un-id'ed items, and only when requested */
	if (filter_identify)
	{
		item_tester_hook = item_tester_hook_unknown;
	}

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Identify which item? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to identify.");
		return (FALSE);
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

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
		msg_format("On the ground: %s.", o_name);
	}

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

	/* Only un-*id*'ed items, and only when requested */
	if (filter_identify)
	{
		item_tester_hook = item_tester_hook_unknown_star;
	}

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Identify which item? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to identify.");
		return (FALSE);
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

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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

	/* Describe it fully */
	identify_fully_aux(o_ptr);

	/* Success */
	return (TRUE);
}


/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(object_type *o_ptr)
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
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(int num)
{
	int		i, t, item, lev;
	object_type	*o_ptr;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Recharge which item? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to recharge.");
		return (FALSE);
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

	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;

	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge power */
		i = (100 - lev + num) / 5;

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire */
		if (rand_int(i) == 0)
		{
			/* Hack -- backfire */
			msg_print("The recharge backfires, draining the rod further!");

			/* Hack -- decharge the rod */
			if (o_ptr->pval < 10000) o_ptr->pval = (o_ptr->pval + 100) * 2;
		}

		/* Recharge */
		else
		{
			/* Rechange amount */
			t = (num * damroll(2, 4));

			/* Recharge by that amount */
			if (o_ptr->pval > t)
			{
				o_ptr->pval -= t;
			}

			/* Fully recharged */
			else
			{
				o_ptr->pval = 0;
			}
		}
	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 100 - lev - (10 * o_ptr->pval)) / 15;

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire XXX XXX XXX */
		if (rand_int(i) == 0)
		{
			/* Dangerous Hack -- Destroy the item */
			msg_print("There is a bright flash of light.");

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

		/* Recharge */
		else
		{
			/* Extract a "power" */
			t = (num / (lev + 2)) + 1;

			/* Recharge based on the power */
			if (t > 0) o_ptr->pval += 2 + randint(t);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
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
 * Charge a lite (torch or latern)
 */
void phlogiston(void)
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


bool brand_weapon(int brand_type)
{
	int		item;
	int		k = 0;
	bool		brand_exists = FALSE;
	char		buf[80];
	object_type	*o_ptr;
	ego_item_type	*e_ptr;

	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Taken from bless_weapon() to allow the branding of inven items */
	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Brand which weapon? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no weapon to brand.");
		return FALSE;
	}

	if (item >= 0) /* Get the item (in the pack) */
	{
		o_ptr = &inventory[item];
	}
	else /* Get the item (on the floor) */
	{
		o_ptr = &o_list[0 - item];
	}

	k = randint(4);

	/*
	 * Check to make sure the brand or ego-item type doesn't already
	 * exist on the item - if it does, branding must fail. -- Gumby
	 */
	switch (brand_type)
	{
		case 4:
		{
			if ((o_ptr->name2 == EGO_TRUMP) ||
			    ((o_ptr->art_flags1 & TR1_SLAY_EVIL) &&
			     (o_ptr->art_flags3 & TR3_TELEPORT)))
				brand_exists = TRUE;
				break;
		}

		case 3:
		{
			if ((o_ptr->name2 == EGO_VAMPIRIC) ||
			    (o_ptr->art_flags1 & TR1_VAMPIRIC))
				brand_exists = TRUE;
				break;
		}

		case 2:
		{
			if ((o_ptr->name2 == EGO_BRAND_POIS) ||
			    (o_ptr->art_flags1 & TR1_BRAND_POIS))
				brand_exists = TRUE;
				break;
		}

		case 1:
		{
			if ((o_ptr->name2 == EGO_CHAOTIC) ||
			    (o_ptr->art_flags1 & TR1_CHAOTIC))
				brand_exists = TRUE;
				break;
		}

		default:
		{
			if (k==1)
			{
				if ((o_ptr->name2 == EGO_BRAND_FIRE) ||
				    (o_ptr->art_flags1 & TR1_BRAND_FIRE))
				{
					brand_exists = TRUE;
				}
			}
			else if (k==2)
			{
				if ((o_ptr->name2 == EGO_BRAND_COLD) ||
				    (o_ptr->art_flags1 & TR1_BRAND_COLD))
				{
					brand_exists = TRUE;
				}
			}
			else if (k==3)
			{
				if ((o_ptr->name2 == EGO_BRAND_ELEC) ||
				    (o_ptr->art_flags1 & TR1_BRAND_ELEC))
				{
					brand_exists = TRUE;
				}
			}
			else if (k==4)
			{
				if ((o_ptr->name2 == EGO_BRAND_ACID) ||
				    (o_ptr->art_flags1 & TR1_BRAND_ACID))
				{
					brand_exists = TRUE;
				}
			}
			break;
		}
	}

	/*
	 * you can never modify artifacts or cursed items
	 * TY: You _can_ modify broken items (if you're silly enough)
	 *
	 * Gum:	You can only modify melee weapons.
	 *	You cannot modify stacks.
	 *	Using on an ego-item will add the brand, tho not any
	 *         supplementary abilities, to the weapon.
	 *	You cannot brand an item more than once.
	 *	Fail if the brand already exists on the item.
	 *	Using this on an ego-item *WILL* make that item unsellable,
	 *         just as the ego-items made from plain weapons cannot be
	 *         sold...
	 */
	if ((o_ptr->k_idx) && (!artifact_p(o_ptr)) && (!o_ptr->branded) &&
	    (!o_ptr->art_name) && (!cursed_p(o_ptr)) && (!brand_exists) &&
	    ((o_ptr->tval > TV_DIGGING) && (o_ptr->tval < TV_BOOTS)) &&
	    (o_ptr->number == 1))
	{
		cptr act = NULL;

		/* Let's get the name before it is changed... */
		char o_name[80];
		object_desc(o_name, o_ptr, FALSE, 0);

		switch (brand_type)
		{
			case 4: /* Trump */
			{
				act = "seems very unstable now.";

				if (ego_item_p(o_ptr))
				{
					o_ptr->art_flags1 |= TR1_SLAY_EVIL;
					o_ptr->art_flags3 |= TR3_TELEPORT;
				}
				else
				{
					o_ptr->name2 = EGO_TRUMP;
					o_ptr->pval = randint(3);
				}
				break;
			}

			case 3: /* Vampiric */
			{
				act = "thirsts for blood!";

				if (ego_item_p(o_ptr))
				{
					o_ptr->art_flags1 |= TR1_VAMPIRIC;
				}
				else
				{
					o_ptr->name2 = EGO_VAMPIRIC;
				}
				break;
			}

			case 2: /* Poison */
			{
				act = "begins to drip poison.";
				if (ego_item_p(o_ptr))
				{
					o_ptr->art_flags1 |= TR1_BRAND_POIS;
				}
				else
				{
					o_ptr->name2 = EGO_BRAND_POIS;
				}
				break;
			}

			case 1: /* Chaos */
			{
				act = "is engulfed in raw Chaos!";

				if (ego_item_p(o_ptr))
				{
					o_ptr->art_flags1 |= TR1_CHAOTIC;
				}
				else
				{
					o_ptr->name2 = EGO_CHAOTIC;
				}
				break;
			}

			default: /* Fire, Frost, Acid, or Lightning */
			{
				if (k==1)
				{
					act = "is engulfed in flames!";

					if (ego_item_p(o_ptr))
					{
						o_ptr->art_flags1 |= TR1_BRAND_FIRE;
					}
					else
					{
						o_ptr->name2 = EGO_BRAND_FIRE;
					}
				}
				else if (k==2)
				{
					act = "is engulfed in ice!";

					if (ego_item_p(o_ptr))
					{
						o_ptr->art_flags1 |= TR1_BRAND_COLD;
					}
					else
					{
						o_ptr->name2 = EGO_BRAND_COLD;
					}
				}
				else if (k==3)
				{
					act = "starts throwing off sparks!";

					if (ego_item_p(o_ptr))
					{
						o_ptr->art_flags1 |= TR1_BRAND_ELEC;
					}
					else
					{
						o_ptr->name2 = EGO_BRAND_ELEC;
					}
				}
				else if (k==4)
				{
					act = "starts dripping acid!";

					if (ego_item_p(o_ptr))
					{
						o_ptr->art_flags1 |= TR1_BRAND_ACID;
					}
					else
					{
						o_ptr->name2 = EGO_BRAND_ACID;
					}
				}
				break;
			}
		}

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, randint(3)+(p_ptr->lev/10), ENCH_TOHIT | ENCH_TODAM);

		/* Add any random extras to newly created ego-items - G */
		e_ptr = &e_info[o_ptr->name2];

		if (e_ptr->flags2 & (TR2_RAND_SUSTAIN))
			o_ptr->xtra1 = EGO_XTRA_SUSTAIN;

		if (e_ptr->flags2 & (TR2_RAND_ABILITY))
			o_ptr->xtra1 = EGO_XTRA_ABILITY;

		if (e_ptr->flags2 & (TR2_RAND_RESIST))
			o_ptr->xtra1 = EGO_XTRA_POWER;

		/* Randomize the "xtra" power */
		if (o_ptr->xtra1) o_ptr->xtra2 = randint(256);

		/* You can't add another brand, so mark it. -- Gumby */
		o_ptr->branded = TRUE;

		/* Don't want the player to profit at all. -- Gumby */
		o_ptr->ident |= IDENT_BROKEN;

		/* We don't want a typo to kill it, however. -- Gumby */
		if (o_ptr->note)
		{
			/* Find out the old inscription then add !k to it */
			sprintf(buf, "%s !k", quark_str(o_ptr->note));
			o_ptr->note = quark_add(buf);
		}
		else
		{
			/* Give it an inscription if it doesn't have one */
			o_ptr->note = quark_add("!k");
		}

		/* You made it, you know it. -- Gumby */
		o_ptr->ident |= IDENT_MENTAL;
	}
	else
	{
		if (flush_failure) flush();

		if (o_ptr->branded)
		{
			msg_print("You've already Branded this item.");
		}
		else
		{
			msg_print("The Branding failed.");
		}
		return FALSE;
	}

	return TRUE;
}


void call_the_(void)
{
	int i;
				
	if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
	    cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
	    cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
	    cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))
	{
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_ROCKET, i, 250, 2);
		}

		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_MANA, i, 250, 3);
		}
		
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_NUKE, i, 250, 4);
		}
	}
	else
	{
		msg_format("You %s the %s too close to a wall!",
		    ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
		    ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
		msg_print("There is a loud explosion!");
		destroy_area(py, px, 20+(p_ptr->lev), TRUE);
		msg_print("The dungeon collapses...");
		take_hit(100 + (randint(150)), "a suicidal Call the Void");
	}
}


void wild_magic(int spell)
{
	int counter = 0;
	int type = SUMMON_MOLD - 1 + (randint(6));

	if (type < SUMMON_MOLD) type = SUMMON_MOLD;
	else if (type > SUMMON_TREASURE) type = SUMMON_TREASURE;

	switch(randint(spell) + randint(8) + 1)
	{
		case 1: case 2: case 3:
			teleport_player(10); break;
		case 4: case 5: case 6:
			teleport_player(100); break;
		case 7: case 8:
			teleport_player(200); break;
		case 9: case 10: case 11:
			unlite_area(10,3); break;
		case 12: case 13: case 14:
			lite_area(damroll(2,3),2); break;
		case 15: destroy_doors_touch(); break;
		case 16: case 17:
			wall_breaker(); break;
		case 18:
			sleep_monsters_touch(); break;
		case 19: case 20:
			trap_creation(); break;
		case 21: case 22:
			door_creation(); break;
		case 23: case 24:
			aggravate_monsters(1, FALSE); break;
		case 25:
			aggravate_monsters(1, TRUE); break;
		case 26:
			earthquake(py, px, 5); break;
		case 27: case 28:
			(void) gain_random_mutation(0); break;
		case 29: case 30:
			apply_disenchant(0); break;
		case 31:
			lose_all_info(); break;
		case 32:
			fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
			break;
		case 33:
			wall_stone(); break;
		case 34: case 35:
			while (counter++ < 8)
			{
				(void) summon_specific(py, px, (dun_level * 3) / 2, type);
			}
			break;
		case 36: case 37:
			activate_hi_summon(); break;
		case 38:
			summon_cyber();
		default:
			activate_ty_curse();
	}
	return;
}
