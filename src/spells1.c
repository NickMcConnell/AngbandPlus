/* File: spells1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




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
	if (r_ptr->flags1 & (RF1_UNIQUE))
		return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint(20) / randint(9)) + 1);
	lev2 = r_ptr->level + ((randint(20) / randint(9)) + 1);

	/* Pick a (possibly new) non-unique race */
	for (i = 0; i < 1000; i++)
	{
		/* Pick a new race, using a level calculation */
		r = get_mon_num((p_ptr->depth + r_ptr->level) / 2 + 5);

		/* Handle failure */
		if (!r)
			break;

		/* Obtain race */
		r_ptr = &r_info[r];

		/* Ignore unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
			continue;

		/* Ignore monsters with incompatible levels */
		if ((r_ptr->level < lev1) || (r_ptr->level > lev2))
			continue;

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
	int ny, nx, oy, ox, d, i, min, foo;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];


	/* Paranoia */
	if (!m_ptr->r_idx)
		return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	foo = 0;

	while (look)
	{
		/* Verify max distance */
		if (dis > 200)
		{
			dis = 200;
			min = 100;
		}

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
		        /* Pick a (possibly illegal) location */
			while (1) {
				ny = rand_spread(oy, dis);
				nx = rand_spread(ox, dis);
				d = distance(oy, ox, ny, nx);

				if ((d >= min) && (d <= dis))
					break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(ny, nx))
				continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx))
				continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave_feat[ny][nx] == FEAT_GLYPH)
				continue;

			/* No teleporting into vaults and such */
			/* if (cave_info[ny][nx] & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		/* dis = dis * 2; */

		/* NOT! Fairly closed spaces are common, but being locked 
		 * inside a huge stone mountain is way exotic. 
		 * (c.f. First quest.) */
		dis = dis / 2;

		if (dis < 5) {
		  dis = 5;
		}

		/* Decrease the minimum distance */
		min = min / 2;

		foo++;

		/* It's not his day today. :( */
		if (foo >= 1000) {
		  return;
		}
	}

	/* Sound */
	sound(SOUND_TPOTHER);

	/* Swap the monsters */
	monster_swap(oy, ox, ny, nx);
}




/*
 * Teleport a monster to a specific grid.
 *
 */
void teleport_away_to(int m_idx, int y, int x)
{
	int ny = 0, nx = 0, oy, ox, i, iter;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];

	/* Paranoia */
	if (!m_ptr->r_idx)
		return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	iter = 0;

	/* Look until done */
	while (look)
	{
		/* Prevent infinite loops. */
		if (iter > 4000)
			break;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			ny = rand_spread(y, (iter / 10) + 1);
			nx = rand_spread(x, (iter / 10) + 1);

			/* Ignore illegal locations */
			if (!in_bounds_fully(ny, nx))
				continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx))
				continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave_feat[ny][nx] == FEAT_GLYPH)
				continue;

			/* No teleporting into vaults and such */
			/* if (cave_info[ny][nx] & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		iter++;
	}

	/* Sound */
	sound(SOUND_TPOTHER);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int d, i, min, y, x;

	bool look = TRUE;


	/* Initialize */
	y = py;
	x = px;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200)
			dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis))
					break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x))
				continue;

			/* Require "naked" floor space */
			if (!cave_naked_bold(y, x))
				continue;

			/* No teleporting into vaults and such */
			if (p_ptr->inside_special == 0)
				if (cave_info[y][x] & (CAVE_ICKY))
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
			if (in_bounds_fully(y, x))
				break;
		}

		/* Accept an empty floor grid. */

		if (cave_empty_bold(y, x) || (cave_feat[y][x] >= FEAT_QUEST_ENTER
				&& cave_feat[y][x] <= FEAT_QUEST_EXIT))
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

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	check_store_entering(y, x);
}


/*
 * Teleport player, using a distance and a direction as a rough guide.
 *
 * This function is not at all obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_directed(int rad, int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
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

	if (xfoo == 0 && yfoo == 0)
	{
		teleport_player(rad);
		return;
	}
	if (yfoo == 0)
	{
		x_major = TRUE;
	}
	if (xfoo == 0)
	{
		y_major = TRUE;
	}
	if (yfoo < 0)
	{
		y_neg = -1;
	}
	if (xfoo < 0)
	{
		x_neg = -1;
	}

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200)
		{
			teleport_player(rad);
			return;
		}

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				if (y_major)
				{
					y = rand_spread(py + y_neg * dis / 2, dis / 2);
				}
				else
				{
					y = rand_spread(py, dis / 3);
				}

				if (x_major)
				{
					x = rand_spread(px + x_neg * dis / 2, dis / 2);
				}
				else
				{
					x = rand_spread(px, dis / 3);
				}

				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis))
					break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x))
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
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	check_store_entering(y, x);
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
	if (p_ptr->inside_special == SPECIAL_ARENA || 
	    p_ptr->inside_special == SPECIAL_MAGIC_ARENA ||
	    p_ptr->inside_special == SPECIAL_STORE)
	{ /* arena or quest -KMW- */
		msg_print("There is no effect.");

	} else if (p_ptr->inside_special == SPECIAL_WILD) {
	  
	  msg_print("You sink through the ground.");

	  p_ptr->wilderness_depth = p_ptr->depth;

	  p_ptr->inside_special = FALSE;
	  p_ptr->depth++;
	  p_ptr->leaving = TRUE;

	} else if (!p_ptr->depth) {

	  msg_print("You sink through the floor.");

	  /* New depth */
	  p_ptr->depth++;

	  /* Leaving */
	  p_ptr->leaving = TRUE;

	} else if (p_ptr->inside_special == SPECIAL_QUEST ||
		   (p_ptr->depth >= MAX_DEPTH - 1)) {

	  msg_print("You rise up through the ceiling.");

	  p_ptr->inside_special = FALSE;
	  p_ptr->depth--;
	  p_ptr->leaving = TRUE;

	} else if (rand_int(100) < 50) {
	  msg_print("You rise up through the ceiling.");

	  /* New depth */
	  p_ptr->depth--;

	  /* Leaving */
	  p_ptr->leaving = TRUE;
	} else {
	  msg_print("You sink through the floor.");

	  /* New depth */
	  p_ptr->depth++;

	  /* Leaving */
	  p_ptr->leaving = TRUE;
	}

	/* Sound */
	sound(SOUND_TPLEVEL);
}






/*
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_ACID:
		case GF_WATER:
			return (TERM_SLATE);
			return (TERM_SLATE);

		case GF_ELEC:
			return (TERM_BLUE);

		case GF_FIRE:
			return (TERM_RED);

		case GF_POIS:
			return (TERM_GREEN);

		case GF_HOLY_ORB:
		case GF_MANA:
			return (TERM_L_DARK);
			return (TERM_L_DARK);

		case GF_NETHER:
		case GF_IDENT:
		case GF_HEAVY_IDENT:
		case GF_SUPER_IDENT:
			return (TERM_L_GREEN);

		case GF_NEXUS:
			return (TERM_L_RED);

		case GF_CONFUSION:
			return (TERM_L_UMBER);

		case GF_SOUND:
	        case GF_NOTHING:
			return (TERM_YELLOW);

		case GF_SHARD:
		case GF_FORCE:
			return (TERM_UMBER);
			return (TERM_UMBER);

		case GF_INERTIA:
		case GF_GRAVITY:
			return (TERM_L_WHITE);
			return (TERM_L_WHITE);

		case GF_TIME:
		case GF_ENCHANT_TO_HIT:
		case GF_ENCHANT_TO_DAM:
		case GF_ENCHANT_AC:
		case GF_BRAND_AMMO:
		case GF_BRAND_WEAPON:
		case GF_RECHARGE:
		case GF_BLESS:
			return (TERM_L_BLUE);

		case GF_DARK_WEAK:
		case GF_DARK:
		case GF_GENOCIDE:
		case GF_MASS_GENOCIDE:
			return (TERM_L_DARK);
			return (TERM_L_DARK);

		case GF_PLASMA:
		case GF_METEOR:
			return (TERM_RED);
			return (TERM_RED);

		case GF_ICE:
		case GF_COLD:
		case GF_ARROW:
			return (TERM_WHITE);
			return (TERM_WHITE);
			return (TERM_WHITE);

		case GF_EARTHQUAKE:
		case GF_WORD_OF_DESTRUCTION:
		case GF_LITE_WEAK:
		case GF_LITE:
			return TERM_ORANGE;

		case GF_CHAOS_DESTRUCTION:
		case GF_MISSILE:
		case GF_CHAOS:
		case GF_DISENCHANT:
			return TERM_VIOLET;

		case GF_MIND_BLAST:
		case GF_BRAIN_SMASH:
		case GF_QUAKE:
		case GF_ALTER:
		case GF_RANDOM:
			return rand_range(TERM_DARK, TERM_L_UMBER);
	}

	/* Standard "color" */
	return (TERM_WHITE);
}



/*
 * Acid has hit the player, attempt to affect some armor.
 */
static int damage_armor(int dam, bool acid)
{
	object_type *o_ptr = NULL;

	u32b f1, f2, f3;

	/* Pick a (possibly empty) inventory slot */
	switch (randint(6))
	{
		case 1:
			o_ptr = equipment[EQUIP_BODY];
			break;
		case 2:
			o_ptr = equipment[EQUIP_ARM];
			break;
		case 3:
			o_ptr = equipment[EQUIP_OUTER];
			break;
		case 4:
			o_ptr = equipment[EQUIP_HANDS];
			break;
		case 5:
			o_ptr = equipment[EQUIP_HEAD];
			break;
		case 6:
			o_ptr = equipment[EQUIP_FEET];
			break;
	}

	/* Nothing to damage */
	if (!o_ptr || !o_ptr->k_idx)
		return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Object resists */
	if (acid && !hates_acid(o_ptr))
	{
		return (TRUE);
	}

	/* Hurt the item. */
	object_take_hit(o_ptr, dam, "broke");

	/* Item was damaged */
	return (TRUE);
}




/*
 * Decreases players hit points and sets death flag if necessary
 *
 * Invulnerability needs to be changed into a "shield" XXX XXX XXX
 *
 * Hack -- this function allows the user to save (or quit) the game
 * when he dies, since the "You die." message is shown before setting
 * the player to "dead".  XXX XXX XXX
 */
void take_hit(int dam, cptr kb_str)
{
	int old_chp = p_ptr->chp;

	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);


	/* Paranoia */
	if (p_ptr->is_dead)
		return;


	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (dam < 9000))
		return;

	/* Hurt the player */
	p_ptr->chp -= dam;

	/* Hurt the player's armor. */
	if (dam >= 3)
	{
		damage_armor(dam / 3, FALSE);
	}

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Sound */
		sound(SOUND_DEATH);

		/* Hack -- Note death */
		mprint(MSG_DEADLY, "You die.");
		msg_print(NULL);

		/* Note cause of death */
		strcpy(p_ptr->died_from, kb_str);

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Dead */
		return;
	}
	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_chp > warning))
			bell();

		/* Message */
		mprint(MSG_URGENT, "*** LOW HITPOINT WARNING! ***");
		msg_print(NULL);
	}
}


/* Decrease player's sanity. This is a copy of the function above. */

void take_sanity_hit(int dam, cptr killer)
{
	int old_csane = p_ptr->csane;
	int warning = (p_ptr->msane * op_ptr->hitpoint_warn / 10);

	if (p_ptr->is_dead)
		return;

	disturb(1, 0);

	p_ptr->csane -= dam;

	p_ptr->redraw |= PR_SANITY;
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

	if (p_ptr->csane < 0)
	{
		sound(SOUND_DEATH);

		/* Note Death */
		mprint(MSG_DEADLY, "You turn into an unthinking vegetable.");
		msg_print(NULL);

		strcpy(p_ptr->died_from, killer);

		p_ptr->total_winner = FALSE;
		p_ptr->is_dead = TRUE;
		p_ptr->leaving = TRUE;

		return;
	}
	if (p_ptr->csane < warning)
	{
		if (alert_hitpoint && (old_csane > warning))
			bell();

		mprint(MSG_URGENT, "*** LOW SANITY WARNING! ***");
		msg_print(NULL);
	}
}

/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(object_type * o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_EARTH:
		case STUFF_CLOTH:
		case STUFF_LEATHER:
		case STUFF_FLESH:
		case STUFF_IRON:
		case STUFF_AMBER:
		case STUFF_SULFUR:
			return TRUE;
	}

	return FALSE;
}


/*
 * Does a given object (usually) hate electricity?
 */
bool hates_elec(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_FLESH:
		case STUFF_CRYSTAL:
		case STUFF_AMBER:
		case STUFF_SULFUR:
		case STUFF_WOOD:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 */
bool hates_fire(object_type * o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_CLOTH:
		case STUFF_LEATHER:
		case STUFF_FLESH:
		case STUFF_WOOD:
		case STUFF_AMBER:
		case STUFF_SULFUR:
			return TRUE;
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
bool hates_cold(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_LEATHER:
		case STUFF_FLESH:
			return (TRUE);
	}

	return (FALSE);
}

/*
 * Does the object hate plasma?
 */
bool hates_plasma(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_EARTH:
		case STUFF_CLOTH:
		case STUFF_LEATHER:
		case STUFF_FLESH:
		case STUFF_WOOD:
		case STUFF_GLASS:
		case STUFF_IRON:
		case STUFF_FLINT:
		case STUFF_GRAPHITE:
		case STUFF_EBONY:
		case STUFF_AMBER:
		case STUFF_SULFUR:
		case STUFF_COPPER:
		case STUFF_SILVER:
		case STUFF_GOLD:

			return TRUE;
	}

	return FALSE;
}

/*
 * Does the object hate meteorites?
 */
bool hates_meteor(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_EARTH:
		case STUFF_CLOTH:
		case STUFF_LEATHER:
		case STUFF_FLESH:
		case STUFF_WOOD:
		case STUFF_GLASS:
		case STUFF_GRAPHITE:
		case STUFF_EBONY:
		case STUFF_AMBER:
		case STUFF_SULFUR:
		case STUFF_COPPER:

			return TRUE;
	}

	return FALSE;
}

/*
 * Does the object hate shards?
 */
bool hates_shards(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_EARTH:
		case STUFF_CLOTH:
		case STUFF_LEATHER:
		case STUFF_FLESH:
		case STUFF_WOOD:
		case STUFF_GLASS:
		case STUFF_GRAPHITE:
		case STUFF_AMBER:
		case STUFF_SULFUR:

			return TRUE;
	}

	return FALSE;
}

/*
 * Does the object hate sound?
 */
bool hates_sound(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_EARTH:
		case STUFF_GLASS:
		case STUFF_CRYSTAL:
		case STUFF_GRAPHITE:
		case STUFF_AMBER:
		case STUFF_SULFUR:
		case STUFF_RUBY:
		case STUFF_SAPPHIRE:
		case STUFF_EMERALD:
		case STUFF_OPAL:
		case STUFF_GARNET:
		case STUFF_QUARTZ:

			return TRUE;
	}

	return FALSE;
}

/*
 * Does the object hate impact? (i.e. does it rip or shatter?)
 */
bool hates_impact(object_type * o_ptr)
{
	switch (o_ptr->stuff)
	{
		case STUFF_ETHER:
		case STUFF_PAPER:
		case STUFF_EARTH:
		case STUFF_WOOD:
		case STUFF_GLASS:
		case STUFF_CRYSTAL:
		case STUFF_SULFUR:
		case STUFF_AMBER:

			return TRUE;
	}

	return FALSE;
}


/*
 * This seems like a pretty standard "typedef"
 */
typedef bool(*inven_func) (object_type *);

/*
 * Loops through the whole inventory, and damages those items that
 * match the given funciton.
 */
void inven_damage(inven_func typ, int dam, cptr verb)
{
	object_type *o_ptr = inventory;
	object_type *o_nxt;

	/* Scan through the slots. */
	while (o_ptr != NULL)
	{

		/* Pre-load next object. */
		o_nxt = o_ptr->next;

		/* Skip non-objects */
		if (!o_ptr->k_idx)
		{
			o_ptr = o_nxt;
			continue;
		}

		/* Give this item slot a shot at death */
		if ((*typ) (o_ptr))
		{

			if (protect_equipment)
			{
				bool skip = FALSE;
				int i;

				for (i = 0; i < EQUIP_MAX; i++)
				{
					if (equipment[i] == o_ptr)
						skip = TRUE;
				}

				if (skip)
				{
					o_ptr = o_nxt;
					continue;
				}
			}

			/* Damage it. */
			object_take_hit(o_ptr, dam, verb);

		}

		o_ptr = o_nxt;
	}
}



/*
 * Hurt the player with Acid
 */
void acid_dam(int dam, cptr kb_str)
{
	/* Total Immunity */
	if (p_ptr->immune_acid || (dam <= 0))
		return;

	/* Resist the damage */
	if (p_ptr->resist_acid)
		dam = (dam + 2) / 3;
	if (p_ptr->oppose_acid)
		dam = (dam + 2) / 3;

	/* If any armor gets hit, defend the player */
	if (damage_armor(dam / 2 + 1, TRUE))
		dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(hates_acid, dam, "melted");
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, cptr kb_str)
{
	/* Total immunity */
	if (p_ptr->immune_elec || (dam <= 0))
		return;

	/* Resist the damage */
	if (p_ptr->oppose_elec)
		dam = (dam + 2) / 3;
	if (p_ptr->resist_elec)
		dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(hates_elec, dam, NULL);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, cptr kb_str)
{
	/* Totally immune */
	if (p_ptr->immune_fire || (dam <= 0))
		return;

	/* Resist the damage */
	if (p_ptr->resist_fire)
		dam = (dam + 2) / 3;
	if (p_ptr->oppose_fire)
		dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(hates_fire, dam, "burned");
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, cptr kb_str)
{
	/* Total immunity */
	if (p_ptr->immune_cold || (dam <= 0))
		return;

	/* Resist the damage */
	if (p_ptr->resist_cold)
		dam = (dam + 2) / 3;
	if (p_ptr->oppose_cold)
		dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(hates_cold, dam, NULL);
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
	if (value < 18 + 100)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			gain = ((rand_int(100) < 75) ? 1 : 2);
			value += gain;
		}
		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18 + 98)
		{
			/* Approximate gain value */
			gain = (((18 + 100) - value) / 2 + 3) / 2;

			/* Paranoia */
			if (gain < 1)
				gain = 1;

			/* Apply the bonus */
			value += randint(gain) + gain / 2;

			/* Maximal value */
			if (value > 18 + 99)
				value = 18 + 99;
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
			if (amount > 90)
				cur--;
			if (amount > 50)
				cur--;
			if (amount > 20)
				cur--;
			cur--;
		}
		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((cur - 18) / 2 + 1) / 2 + 1);

			/* Paranoia */
			if (loss < 1)
				loss = 1;

			/* Randomize the loss */
			loss = ((randint(loss) + loss) * amount) / 100;

			/* Maximal loss */
			if (loss < amount / 2)
				loss = amount / 2;

			/* Lose some points */
			cur = cur - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (cur < 18)
				cur = (amount <= 20) ? 18 : 17;
		}

		/* Prevent illegal values */
		if (cur < 3)
			cur = 3;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat])
			res = TRUE;
	}
	/* Damage "max" value */
	if (permanent && (max > 3))
	{
		/* Handle "low" values */
		if (max <= 18)
		{
			if (amount > 90)
				max--;
			if (amount > 50)
				max--;
			if (amount > 20)
				max--;
			max--;
		}
		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((max - 18) / 2 + 1) / 2 + 1);
			loss = ((randint(loss) + loss) * amount) / 100;
			if (loss < amount / 2)
				loss = amount / 2;

			/* Lose some points */
			max = max - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (max < 18)
				max = (amount <= 20) ? 18 : 17;
		}

		/* Hack -- keep it clean */
		if (same || (max < cur))
			max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat])
			res = TRUE;
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
 * Disenchant some item.
 */

static bool disen_item(object_type * o_ptr, int t)
{
	char o_name[80];


	/* No item, nothing happens */
	if (!o_ptr || !o_ptr->k_idx)
		return (FALSE);

	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
	{
		/* Nothing to notice */
		return (FALSE);
	}

	/* Describe the object */
	object_desc(o_name, o_ptr, FALSE, 0);


	/* Artifacts have 60% chance to resist */
	if (artifact_p(o_ptr) && (rand_int(100) < 60))
	{
		/* Message */
		if (o_ptr->stack == STACK_INVEN)
			msg_format("Your %s resist%s disenchantment!", o_name,
				((o_ptr->number != 1) ? "" : "s"));

		/* Notice */
		return (TRUE);
	}

	/* Disenchant tohit */
	if (o_ptr->to_h > 0)
		o_ptr->to_h--;
	if ((o_ptr->to_h > 5) && (rand_int(100) < 20))
		o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0)
		o_ptr->to_d--;
	if ((o_ptr->to_d > 5) && (rand_int(100) < 20))
		o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0)
		o_ptr->to_a--;
	if ((o_ptr->to_a > 5) && (rand_int(100) < 20))
		o_ptr->to_a--;

	/* Message */
	if (o_ptr->stack == STACK_INVEN)
	{
		if (t)
		{
			mformat(MSG_WARNING, "Your %s %s disenchanted!", o_name,
				((o_ptr->number != 1) ? "were" : "was"));
		}
		else
		{
			mformat(MSG_WARNING, "Your %s %s disenchanted!", o_name,
				((o_ptr->number != 1) ? "were" : "was"));
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Notice */
	return (TRUE);
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
	int t = 0;

	object_type *o_ptr;


	/* Unused */
	mode = mode;


	/* Pick a random slot */
	switch (randint(8))
	{
		case 1:
			t = EQUIP_WIELD;
			break;
		case 2:
			t = EQUIP_BOW;
			break;
		case 3:
			t = EQUIP_BODY;
			break;
		case 4:
			t = EQUIP_OUTER;
			break;
		case 5:
			t = EQUIP_ARM;
			break;
		case 6:
			t = EQUIP_HEAD;
			break;
		case 7:
			t = EQUIP_HANDS;
			break;
		case 8:
			t = EQUIP_FEET;
			break;
	}

	/* Get the item */
	o_ptr = equipment[t];

	return disen_item(o_ptr, t);
}


/*
 * Apply Nexus
 */
static void apply_nexus(monster_type * m_ptr)
{
	int max1, cur1, max2, cur2, ii, jj;

	switch (randint(7))
	{
		case 1:
		case 2:
		case 3:
		{
			teleport_player(200);
			break;
		}

		case 4:
		case 5:
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

		case 8:
		{
			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
				break;
			}
			mprint(MSG_WARNING, "Your body starts to scramble...");

			/* Pick a pair of stats */
			ii = rand_int(6);
			for (jj = ii; jj == ii; jj = rand_int(6)) /* loop */
				;

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


/*
 * Split the damage between physical/mental and apply it.
 */

static void apply_weird_damage(int dam, cptr killer)
{
	int perc = randint(100);
	int sand = perc * dam / 100;

	take_sanity_hit(sand, killer);
	take_hit(dam - sand, killer);
}


/*
 * Twiddle the terrain up/down one level.
 */
static void twiddle_terrain(int y, int x)
{
	int rn = randint(100);
	byte feat = cave_feat[y][x];

	/* Destroy "valid" grids */
	if (!cave_valid_bold(y, x) || !cave_naked_bold(y, x))
		return;

	/* Lose room and vault */
	cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

	/* Lose light and knowledge */
	cave_info[y][x] &= ~(CAVE_MARK | CAVE_GLOW);


	if (feat == FEAT_RUBBLE || (feat >= FEAT_DOOR_HEAD &&
			feat <= FEAT_DOOR_TAIL))
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_FLOOR);
		else
			cave_set_feat(y, x, FEAT_WALL_SOLID);


	}
	else if (feat >= FEAT_WALL_EXTRA && feat <= FEAT_WALL_SOLID)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_RUBBLE);
		else
			cave_set_feat(y, x, FEAT_MOUNTAIN);

	}
	else if (feat == FEAT_MOUNTAIN)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_WALL_SOLID);
		else
			cave_set_feat(y, x, FEAT_CHAOS_FOG);


	}
	else if (feat == FEAT_CHAOS_FOG)
	{
		/* Do nothing */

	}
	else if (feat == FEAT_TREES)
	{
		if (rn < 33)
			cave_set_feat(y, x, FEAT_FLOOR);
		else if (rn < 66)
		{ /* Do nothing */
		}
		else
			cave_set_feat(y, x, FEAT_WALL_SOLID);

	}
	else if (feat == FEAT_SHAL_WATER)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_FLOOR);
		else
			cave_set_feat(y, x, FEAT_DEEP_WATER);

	}
	else if (feat == FEAT_DEEP_WATER)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_SHAL_WATER);
		else
			cave_set_feat(y, x, FEAT_SHAL_LAVA);

	}
	else if (feat == FEAT_SHAL_LAVA)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_DEEP_WATER);
		else
			cave_set_feat(y, x, FEAT_DEEP_LAVA);

	}
	else if (feat == FEAT_DEEP_LAVA)
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_SHAL_LAVA);
		else
			cave_set_feat(y, x, FEAT_CHAOS_FOG);

	}
	else
	{
		if (rn < 50)
			cave_set_feat(y, x, FEAT_SHAL_WATER);
		else
			cave_set_feat(y, x, FEAT_RUBBLE);
	}

	/* Update some stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_FLOW);
}



/*
 * Ring of power effects. (On the player, that is.)
 */

static void ring_of_power(s16b dam)
{

	/* Pick random effect. */
	switch (randint(10))
	{
		case 1:
		case 2:
		case 3:
		case 4:
			set_confused(p_ptr->confused + dam);
			set_afraid(p_ptr->afraid + dam);
			mprint(MSG_URGENT,
				"Horrible visions of a giant blood-red Eye invade "
				"your mind!");
			break;

		case 5:
			dec_stat(A_WIS, 50, TRUE);
			dec_stat(A_INT, 50, TRUE);
			mprint(MSG_URGENT,
				"Your mind melts under the stress of the Red Eye's "
				"mental invasion.");
			mprint(MSG_URGENT,
				"All of a sudden the word ``precious'' echoes over "
				"and over in your mind.");
			break;

		case 6:
			dec_stat(A_STR, 50, TRUE);
			dec_stat(A_DEX, 50, TRUE);
			mprint(MSG_URGENT,
				"It seems like you have a giant weight around your "
				"neck now.");
			break;

		case 7:
			dec_stat(A_STR, 50, TRUE);
			dec_stat(A_INT, 50, TRUE);
			dec_stat(A_WIS, 50, TRUE);
			dec_stat(A_DEX, 50, TRUE);
			dec_stat(A_CON, 50, TRUE);
			dec_stat(A_CHR, 50, TRUE);
			mprint(MSG_URGENT,
				"Your body cannot tolerate the constant barrage from "
				"the Red Eye.");
			break;

		case 8:
			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->exp / 4);
			check_experience();

			mprint(MSG_URGENT,
				"Your mental defenses crumble as the Red Eye invades "
				"your mind.");
			break;

		case 9:
		case 10:
			if (summon_specific(p_ptr->py, p_ptr->px, 1, SUMMON_SAURON))
			{
				mprint(MSG_DEADLY,
					"The Master of the Ring has heard your call.");
				mprint(MSG_DEADLY, "Prepare to die.");
			}
			break;
	}
}


/*
 * Attempt to raise a corpse from the dead.
 *
 * Returns TRUE if the corpse should be destroyed.
 *
 */
static bool raise_dead(object_type * o_ptr, int chance)
{

	/* Paranoia. */
	if (o_ptr->tval != TV_CORPSE)
		return FALSE;

	/* It's not organic anymore. */
	if (o_ptr->stuff != STUFF_FLESH)
	{
		msg_print
			("The corpse doesn't seem to have any living essense left in it.");
		return FALSE;
	}

	/* Small body parts have a lesser chance. */
	switch (o_ptr->sval)
	{
		case SV_CORPSE_HEAD:
		case SV_CORPSE_HEART:
			chance += 5;
			break;

		case SV_CORPSE_WING:
			chance -= 5;
			break;

		case SV_CORPSE_LIVER:
		case SV_CORPSE_TONGUE:
			chance -= 10;
			break;

		case SV_CORPSE_SKIN:
		case SV_CORPSE_SCALE:
		case SV_CORPSE_HAIR:
			chance -= 15;
			break;
	}

	/* Many body parts together increase the chance. */
	if (o_ptr->number > 2)
		chance *= (o_ptr->number / 2) + 1;

	/* Success. */
	if (magik(chance))
	{
		if (place_monster_aux(o_ptr->iy, o_ptr->ix, o_ptr->pval,
				(MON_ALLOC_PET | MON_ALLOC_JUST_ONE)))
		{

			mprint(MSG_BONUS,
				"You have successfully brought back to life a willing slave.");
		}
		else
		{
			msg_print
				("The corpse tries to rise, but then crumples to dust.");
		}

		return TRUE;

	}
	else
	{

		switch (randint(6))
		{
			case 1:
				msg_print("Nothing happened.");
				return FALSE;
				break;

			case 2:
				mprint(MSG_STUPID, "The corpse rots away.");
				return TRUE;
				break;

			case 3:
				if (place_monster_aux(o_ptr->iy, o_ptr->ix, o_ptr->pval,
						(MON_ALLOC_JUST_ONE)))
				{
					mprint(MSG_WARNING,
						"The corpse rises, a murderous glint in it's eye!");
				}
				else
				{
					msg_print
						("The corpse tries to rise, but then crumples to dust.");
				}

				return TRUE;
				break;

			case 4:
				mprint(MSG_WARNING,
					"The corpse explodes in a shower of rotting flesh!");
				project(-100, 5, o_ptr->iy, o_ptr->ix, damroll(2,
						p_ptr->lev + 1), GF_MANA,
					PROJECT_KILL | PROJECT_ITEM | PROJECT_BLAST);
				return TRUE;
				break;

			case 5:
				msg_print("The corpse mutates horribly!");
				apply_magic(o_ptr, p_ptr->lev, FALSE, FALSE, FALSE);
				return FALSE;

			case 6:
				mprint(MSG_WARNING,
					"A valevolent spirit rises from the corpse!");
				mprint(MSG_WARNING, "You feel your sanity slipping away.");
				take_sanity_hit(damroll(3, p_ptr->lev + 1),
					"disturbing the underworld");
				return TRUE;

		}

	}

	return FALSE;
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
 * Hack -- We also "see" grids which are "memorized".
 *
 * Perhaps we should affect doors and/or walls. XXX XXX
 */
static bool project_f(int who, int r, int y, int x, int dam, int typ)
{
	bool obvious = FALSE;


	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Analyze the type */
	switch (typ)
	{

			/* Warp space-time :) */
		case GF_QUAKE:
		{
			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_MARK | CAVE_GLOW);

			/* Don't entomb the player. */
			if (p_ptr->py == y && p_ptr->px == x)
				break;

			if (cave_valid_bold(y, x) && cave_naked_bold(y, x))
			{
				int t = randint(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(y, x, FEAT_WALL_EXTRA);
				}
				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(y, x, FEAT_QUARTZ);
				}
				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(y, x, FEAT_MAGMA);
				}
				/* Water */
				else if (t < 125)
				{
					cave_set_feat(y, x, FEAT_SHAL_WATER);
				}
				/* Lava */
				else if (t < 150)
				{
					cave_set_feat(y, x, FEAT_SHAL_LAVA);
				}
				/* Floor */
				else
				{
					/* Create floor */
					cave_set_feat(y, x, FEAT_FLOOR);
				}

				/* Update some stuff */
				p_ptr->update |=
					(PU_VIEW | PU_LITE | PU_MONSTERS | PU_FLOW);
			}

			if (player_can_see_bold(y, x))
				obvious = TRUE;
			break;
		}

			/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Destroy traps */
			if ((cave_feat[y][x] == FEAT_INVIS) ||
				((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
					(cave_feat[y][x] <= FEAT_TRAP_TAIL)))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}
				/* Forget the trap */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the trap */
				cave_set_feat(y, x, FEAT_FLOOR);
			}
			/* Secret / Locked doors are found and unlocked */
			else if ((cave_feat[y][x] == FEAT_SECRET) ||
				((cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01) &&
					(cave_feat[y][x] <= FEAT_DOOR_HEAD + 0x07)))
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
			if ((cave_feat[y][x] == FEAT_OPEN) ||
				(cave_feat[y][x] == FEAT_BROKEN) ||
				(cave_feat[y][x] == FEAT_INVIS) ||
				((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
					(cave_feat[y][x] <= FEAT_TRAP_TAIL)) ||
				((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
					(cave_feat[y][x] <= FEAT_DOOR_TAIL)))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

					/* Visibility change */
					if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
						(cave_feat[y][x] <= FEAT_DOOR_TAIL))
					{
						/* Update some things */
						p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
					}
				}
				/* Forget the door */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}
			break;
		}

			/* Turn walls into chaos fog. */
		case GF_WALL_TO_CHAOS:
		{
			if (cave_floor_bold(y, x))
				break;

			if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
				break;

			if (cave_feat[y][x] >= FEAT_WALL_EXTRA ||
				cave_feat[y][x] >= FEAT_MAGMA_H ||
				cave_feat[y][x] >= FEAT_MAGMA ||
				cave_feat[y][x] == FEAT_RUBBLE)
			{

				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				cave_info[y][x] &= ~(CAVE_MARK);
				cave_set_feat(y, x, FEAT_CHAOS_FOG);

			}
			else if (cave_feat[y][x] >= FEAT_DOOR_HEAD)
			{

				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				cave_info[y][x] &= ~(CAVE_MARK);
				cave_set_feat(y, x, FEAT_CHAOS_FOG);
			}
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
			break;
		}

			/* Destroy walls (and doors) */
		case GF_KILL_WALL:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x))
				break;

			/* Permanent walls */
			if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
				break;

			/* Granite */
			if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma with treasure */
			else if (cave_feat[y][x] >= FEAT_MAGMA_H)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					mprint(MSG_BONUS, "You have found something!");

					/* Place gold */
					place_object(y, x, FALSE, FALSE);

					obvious = TRUE;
				}
				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Place some gold */
				place_object(y, x, FALSE, FALSE);
			}

			/* Quartz / Magma */
			else if (cave_feat[y][x] >= FEAT_MAGMA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}
			/* Rubble */
			else if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Hack -- place an object */
				if (rand_int(100) < 10)
				{
					/* Found something */
					if (player_can_see_bold(y, x))
					{
						mprint(MSG_BONUS,
							"There was something buried in the rubble!");
						obvious = TRUE;
					}
					/* Place gold */
					place_object(y, x, FALSE, FALSE);
				}
			}
			/* Destroy doors (and secret doors) */
			else
				/* if (cave_feat[y][x] >= FEAT_DOOR_HEAD) */
			{
				/* Hack -- special message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

			break;
		}

			/* Make monster */
		case GF_MAKE_MONSTER:
		{
			if (dam < SUMMON_ANT || 
			    dam > SUMMON_UNDEAD_KOBOLD)
				dam = 0;

			if (summon_specific(y, x, p_ptr->depth, dam))
				obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
			break;
		}

			/* Make pet */
		case GF_MAKE_PET:
		{
			if (p_ptr->inside_special == SPECIAL_ARENA)
				break;

			if (p_ptr->number_pets >
				adj_chr_pet_summon[p_ptr->stat_ind[A_CHR]])
				break;

			if (dam < SUMMON_ANT || dam > SUMMON_MOLD)
				dam = 0;

			if (summon_specific_friendly(y, x, p_ptr->depth, dam))
			{
				p_ptr->number_pets++;
				obvious = TRUE;
			}

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
			break;
		}

			/* Make walls */
		case GF_MAKE_WALL:
		{
			if (!cave_naked_bold(y, x))
				break;

			cave_set_feat(y, x, FEAT_WALL_EXTRA);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK))
				obvious = TRUE;

			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
			break;
		}

			/* Make glyphs */
		case GF_MAKE_GLYPH:
		{
			if (!cave_clean_bold(y, x))
				break;

			cave_set_feat(y, x, FEAT_GLYPH);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK))
				obvious = TRUE;
			break;
		}

			/* Make stairs */
		case GF_MAKE_STAIR:
		{
			if (cave_valid_bold(y, x) && !p_ptr->inside_special)
			{
				if (!p_ptr->depth)
					cave_set_feat(y, x, FEAT_MORE);
				else if (p_ptr->depth >= MAX_DEPTH - 1)
					cave_set_feat(y, x, FEAT_LESS);
				else if (rand_int(100) < 50)
					cave_set_feat(y, x, FEAT_MORE);
				else
					cave_set_feat(y, x, FEAT_LESS);
				break;
			}

			if (player_can_see_bold(y, x))
				obvious = TRUE;
			break;
		}

			/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x))
				break;

			/* Create closed door */
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK))
				obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			break;
		}

			/* Make traps */
		case GF_MAKE_TRAP:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x))
				break;

			/* Place a trap */
			place_trap(y, x);

			if (player_can_see_bold(y, x))
				obvious = TRUE;

			break;
		}

			/* Lite up the grid */
		case GF_LITE_WEAK:
		case GF_LITE:
		case GF_RING_OF_POWER:
		{
			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);

			/* Observe */
			if (player_can_see_bold(y, x))
				obvious = TRUE;

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (cave_m_idx[y][x] > 0)
				update_mon(cave_m_idx[y][x], FALSE);

			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
			break;
		}

			/* Darken the grid */
		case GF_DARK_WEAK:
		case GF_DARK:
		{
			/* Notice */
			if (player_can_see_bold(y, x))
				obvious = TRUE;

			/* Turn off the light. */
			cave_info[y][x] &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if (cave_feat[y][x] <= FEAT_INVIS)
			{
				/* Forget */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Notice */
				note_spot(y, x);
			}
			/* Redraw */
			lite_spot(y, x);

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (cave_m_idx[y][x] > 0)
				update_mon(cave_m_idx[y][x], FALSE);

			/* All done */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
			break;
		}



			/* Word of Destruction */
		case GF_WORD_OF_DESTRUCTION:
		{
			int t;

			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_MARK | CAVE_GLOW);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x) && cave_m_idx[y][x] == 0)
			{
				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(y, x, FEAT_WALL_EXTRA);
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(y, x, FEAT_QUARTZ);
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(y, x, FEAT_MAGMA);
				}

				/* Floor */
				else
				{
					/* Create floor */
					cave_set_feat(y, x, FEAT_FLOOR);
				}
			}

			if (player_can_see_bold(y, x))
				obvious = TRUE;

			/* Update some stuff */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_FLOW);
			break;
		}


			/* Place some chaos fog. */
		case GF_CHAOS_DESTRUCTION:
		{
			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_MARK | CAVE_GLOW);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x) && cave_m_idx[y][x] == 0)
			{
				cave_set_feat(y, x, FEAT_CHAOS_FOG);

				if (player_can_see_bold(y, x))
					obvious = TRUE;
			}
			break;
		}

			/* Twiddle the terrain ``height'' */
		case GF_ALTER:
		case GF_EARTHQUAKE:
		{
			twiddle_terrain(y, x);

			if (player_can_see_bold(y, x))
				obvious = TRUE;
			break;
		}



			/* Detect traps */
		case GF_DETECT_TRAP:
		{
			/* Detect invisible traps */
			if (cave_feat[y][x] == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect traps */
			if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
				(cave_feat[y][x] <= FEAT_TRAP_TAIL))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}
			break;
		}

		case GF_DETECT_DOOR:
		{
			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door XXX XXX XXX */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
			}

			/* Detect doors */
			if (((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
					(cave_feat[y][x] <= FEAT_DOOR_TAIL)) ||
				((cave_feat[y][x] == FEAT_OPEN) ||
					(cave_feat[y][x] == FEAT_BROKEN)))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}
			break;
		}



		case GF_DETECT_STAIR:
		{
			/* Detect stairs */
			if ((cave_feat[y][x] == FEAT_LESS) ||
			    (cave_feat[y][x] == FEAT_MORE) ||
			    (cave_feat[y][x] == FEAT_SHAFT))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}

			break;
		}


		case GF_DETECT_TREASURE:
		{
			/* Notice embedded gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
				(cave_feat[y][x] == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				cave_feat[y][x] += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_K) ||
				(cave_feat[y][x] == FEAT_QUARTZ_K))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}

			break;
		}

		case GF_DETECT_ANY:
		{
			/* Notice everything except floors and walls. */
			if (cave_feat[y][x] != FEAT_FLOOR &&
				(cave_feat[y][x] < FEAT_WALL_EXTRA ||
					cave_feat[y][x] > FEAT_WALL_SOLID))
			{

				cave_info[y][x] |= CAVE_MARK;
				lite_spot(y, x);

				obvious = TRUE;
			}
			break;
		}

		case GF_DETECT_GRIDS:
		{
			/* Notice interesting grids -- interesting meaning that it has 
			 * neighbors different from itself. */

			int x1, y1;

			for (y1 = -1; y1 <= 1; y1++)
			{
				for (x1 = -1; x1 <= 1; x1++)
				{

					if (in_bounds(y + y1, x + x1) &&
						cave_feat[y][x] != cave_feat[y + y1][x + x1])
					{

						cave_info[y][x] |= CAVE_MARK;
						lite_spot(y, x);

						obvious = TRUE;
					}
				}
			}

			break;
		}

		default:
		{
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
 * Hack -- We also "see" objects which are "memorized".
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int r, int y, int x, int dam, int typ)
{
	object_type *o_ptr;
	object_type *o_nxt;

	bool obvious = FALSE;

	u32b f1, f2, f3;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* Scan all objects in the grid */
	o_ptr = cave_o_idx[y][x];

	while (TRUE)
	{
		bool is_art = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;

		cptr note_kill = NULL;

		char o_name[80];

		/* Finished. */
		if (o_ptr == NULL)
			break;

		/* Get the next object beforehand. */
		o_nxt = o_ptr->next;

		/* Item is ``dead''. */
		if (!o_ptr->k_idx)
		{

			/* Advance to the next one. */
			o_ptr = o_nxt;
			continue;
		}

		object_desc(o_name, o_ptr, TRUE, 3);

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1)
			plural = TRUE;

		/* Check for artifact */
		if (artifact_p(o_ptr))
			is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
				/* Acid -- Lots of things */
			case GF_ACID:
			{
				if (hates_acid(o_ptr))
				{
					do_kill = TRUE;
					note_kill = "melted";
				}
				break;
			}

				/* Elec -- Rings and Wands */
			case GF_ELEC:
			{
				if (hates_elec(o_ptr))
				{
					do_kill = TRUE;
					note_kill = NULL;
				}
				break;
			}

				/* Fire -- Flammable objects */
			case GF_FIRE:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = "burned";
				}
				break;
			}

				/* Cold -- potions and flasks */
			case GF_COLD:
			{
				if (hates_cold(o_ptr))
				{
					note_kill = "shattered";
					do_kill = TRUE;
				}
				break;
			}

				/* Fire + Elec */
			case GF_PLASMA:
			{
				if (hates_plasma(o_ptr))
				{
					do_kill = TRUE;
					note_kill = "vaporized";
				}
				break;
			}

				/* Fire + Cold */
			case GF_METEOR:
			{
				if (hates_meteor(o_ptr))
				{
					do_kill = TRUE;
					note_kill = "broke apart";
				}
				break;
			}

				/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARD:
			{
				if (hates_shards(o_ptr))
				{
					note_kill = "shattered";
					do_kill = TRUE;
				}
				break;
			}

			case GF_FORCE:
			case GF_SOUND:
			{
				if (hates_sound(o_ptr))
				{
					note_kill = "shattered";
					do_kill = TRUE;
				}
				break;
			}

				/* Mana -- destroys everything */
				/* Earthquake -- ditto. */
			case GF_MANA:
			case GF_QUAKE:
			{
				do_kill = TRUE;
				dam = o_ptr->chp + 1;
				note_kill = NULL;
				break;
			}

				/* Holy Orb -- destroys cursed non-artifacts */
			case GF_BLESS:
			case GF_HOLY_ORB:
			case GF_PROT_EVIL:
			{
				if (cursed_p(o_ptr))
				{
					do_kill = TRUE;
					dam = o_ptr->chp + 1;
					note_kill = NULL;
				}
				break;
			}

				/* Ruination -- destroy everything BUT cursed stuff. */
			case GF_RUINATION:
			{
				if (!cursed_p(o_ptr))
				{
					do_kill = TRUE;
					dam = o_ptr->chp + 1;
					note_kill = "withered in the liquid essence of Ruin";
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

			case GF_CHAOS_DESTRUCTION:
			case GF_WORD_OF_DESTRUCTION:
			case GF_EARTHQUAKE:
			{
				do_kill = TRUE;
				dam = o_ptr->chp + 1;
				note_kill = "disintegrated";
				break;
			}


			case GF_DETECT_GOLD:
			{
				if (o_ptr->tval == TV_GOLD)
				{
					/* Memorize the gold */
					o_ptr->marked = TRUE;

					/* Draw it */
					lite_spot(o_ptr->iy, o_ptr->ix);
					obvious = TRUE;
				}
				break;
			}

			case GF_DETECT_OBJECT:
			{
				if (o_ptr->tval != TV_GOLD)
				{
					/* Memorize it */
					o_ptr->marked = TRUE;

					/* Draw it */
					lite_spot(o_ptr->iy, o_ptr->ix);
					obvious = TRUE;
				}
				break;
			}

			case GF_DETECT_MAGIC:
			{
				int tv = o_ptr->tval;

				/* Artifacts, misc magic items, or enchanted wearables */
				if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
					tv == TV_AMULET || tv == TV_RING || tv == TV_STAFF ||
					tv == TV_WAND || tv == TV_ROD || tv == TV_SCROLL ||
					tv == TV_POTION || tv == TV_SPELLBOOK ||
					tv == TV_MIMIC_BOOK || o_ptr->to_a > 0 ||
					o_ptr->to_h + o_ptr->to_d > 0)
				{
					/* Memorize the item */
					o_ptr->marked = TRUE;

					/* Redraw */
					lite_spot(y, x);
					obvious = TRUE;
				}
				break;
			}

			case GF_DETECT_ANY:
			{
				o_ptr->marked = TRUE;
				lite_spot(y, x);
				obvious = TRUE;
				break;
			}

				/* Polymorph an object. */
			case GF_ALTER:
			case GF_POLY:
			{
				if (!is_art)
				{

					/* Delete the current object. */
					remove_object(o_ptr);

					/* Make a new one. */
					place_object(y, x, FALSE, FALSE);
					lite_spot(y, x);

					if (player_has_los_bold(y, x))
						obvious = TRUE;
				}
				break;
			}

				/* Teleport an item to the player. */
			case GF_RECALL:
			{
				if (fetch_item(dam, y, x))
					obvious = TRUE;
				break;
			}

			case GF_UNCURSE:
			{
				if (uncurse_item(o_ptr, FALSE))
					obvious = TRUE;
				break;
			}

			case GF_TRANSMUTE:
			case GF_HEAVY_TRANSMUTE:
			{
				if (transmute_random(o_ptr, dam))
					obvious = TRUE;
				break;
			}

			case GF_HEAVY_UNCURSE:
			{
				if (uncurse_item(o_ptr, TRUE))
					obvious = TRUE;
				break;
			}

			case GF_SHIELD:
			case GF_ENCHANT_AC:
			{
				if (enchant(o_ptr, dam, ENCH_TOAC))
					obvious = TRUE;
				break;
			}

			case GF_HEROISM:
			case GF_ENCHANT_TO_HIT:
			{
				if (enchant(o_ptr, dam, ENCH_TOHIT))
					obvious = TRUE;
				break;
			}

			case GF_ENCHANT_TO_DAM:
			{
				if (enchant(o_ptr, dam, ENCH_TODAM))
					obvious = TRUE;
				break;
			}

			case GF_ENCHANT_EGO_ITEM:
			{
				if (enchant(o_ptr, dam, ENCH_MAKE_EGO))
					obvious = TRUE;
				break;
			}

			case GF_ENCHANT_ARTIFACT:
			{
				if (enchant(o_ptr, dam, ENCH_MAKE_ART))
					obvious = TRUE;
				break;
			}

			case GF_IDENT:
			{
				/* Identify it fully */
				object_aware(o_ptr);
				object_known(o_ptr);

				if (player_has_los_bold(y, x))
					obvious = TRUE;
				break;
			}

			case GF_DISENCHANT:
			{
				disen_item(o_ptr, 0);

				if (player_has_los_bold(y, x))
					obvious = TRUE;
				break;
			}

			case GF_HEAVY_IDENT:
			case GF_SUPER_IDENT:
			case GF_RING_OF_POWER:
			{
				/* Identify it fully */
				object_aware(o_ptr);
				object_known(o_ptr);

				/* Mark the item as fully known */
				o_ptr->ident |= (IDENT_MENTAL);

				if (player_has_los_bold(y, x))
					obvious = TRUE;
				break;
			}

			case GF_RECHARGE:
			{
				recharge_item(dam, o_ptr);

				if (player_has_los_bold(y, x))
					obvious = TRUE;
				break;
			}

			case GF_MAKE_ITEM:
			{
				acquirement(y, x, dam, TRUE);
				break;
			}

				/* Only works on corpses. */
			case GF_RAISE_DEAD:
			{
				if (o_ptr->tval == TV_CORPSE)
				{
					if (raise_dead(o_ptr, dam))
					{
						do_kill = TRUE;
						dam = o_ptr->chp + 1;
					}

					obvious = TRUE;
				}
				break;
			}

				/* Repair item. */
			case GF_REPAIR:
			{
				if (repair_object(o_ptr, dam) && player_has_los_bold(y, x))
				{
					obvious = TRUE;
				}
				break;
			}

			default:
			{
				break;
			}

		}


		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Damage the object. */
			object_take_hit(o_ptr, dam, note_kill);

			/* Redraw */
			lite_spot(y, x);
		}

		/* Advance to the next one. */
		o_ptr = o_nxt;
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
static bool project_m(int who, int r, int y, int x, int dam, int typ)
{
	int tmp;

	monster_type *m_ptr;
	monster_race *r_ptr;

	cptr name;

	/* Is the monster "seen"? */
	bool seen = FALSE;

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

	/* Delete the monster, not kill it. */
	bool do_wipe = FALSE;

	/* Wake up the monster, true/false */
	bool do_wake = FALSE;

	/* Recall monster, boolean. */
	bool do_dist_player = FALSE;

	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	cptr note = NULL;

	/* Assume a default death */
	cptr note_dies = " dies.";


	/* Walls protect monsters */
	/* Gag -- ugly hack, kill things through chaos fog. */
	if (!cave_floor_bold(y, x) && cave_feat[y][x] != FEAT_CHAOS_FOG)
		return (FALSE);


	/* No monster here */
	if (!(cave_m_idx[y][x] > 0))
		return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who)
		return (FALSE);


	/* Obtain monster info */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	name = (r_name + r_ptr->name);
	if (m_ptr->ml)
		seen = TRUE;


	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);


	/* Some monsters get "destroyed" */
	if ((r_ptr->flags3 & (RF3_DEMON)) || (r_ptr->flags3 & (RF3_UNDEAD)) ||
		(r_ptr->flags2 & (RF2_STUPID)) || (strchr("Evg", r_ptr->d_char)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}




	/* Analyze the damage type */
	switch (typ)
	{

	case GF_NOTHING:
	  {
	    dam = 0;

	    if (seen)
	      obvious = TRUE;
	    break;
	  }
	    
			/* Magic Missile -- pure damage */
		case GF_MISSILE:
		{
			if (seen)
				obvious = TRUE;
			break;
		}

			/* Acid */
		case GF_ACID:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_ACID);
			}
			break;
		}

			/* Electricity */
		case GF_ELEC:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_ELEC);
			}
			break;
		}

			/* Fire damage */
		case GF_FIRE:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_FIRE);
			}

			if (r_ptr->flags3 & RF3_HURT_FIRE)
			{
				note = " is hit hard!";
				dam *= 2;
				if (seen)
					r_ptr->r_flags3 |= RF3_HURT_FIRE;
			}
			break;
		}

			/* Cold */
		case GF_COLD:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_COLD);
			}

			if (r_ptr->flags3 & RF3_HURT_COLD)
			{
				note = " is hit hard!";
				dam *= 2;
				if (seen)
					r_ptr->r_flags3 |= RF3_HURT_COLD;
			}
			break;
		}

			/* Poison */
		case GF_POIS:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			break;
		}

			/* Holy Orb -- hurts Evil */
		case GF_HOLY_ORB:
		case GF_PROT_EVIL:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam *= 2;
				note = " is hit hard.";
				if (seen)
					r_ptr->r_flags3 |= (RF3_EVIL);
			}
			break;
		}

			/* Arrow -- XXX no defense */
		case GF_ARROW:
		{
			if (seen)
				obvious = TRUE;
			break;
		}

			/* Plasma -- XXX perhaps check ELEC or FIRE */
		case GF_PLASMA:
		{
			if (seen)
				obvious = TRUE;
			if (prefix(name, "Plasma") || (r_ptr->flags4 & (RF4_BR_PLAS)))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				note = " is immune.";
				dam = 0;
				if (seen)
					r_ptr->r_flags3 |= (RF3_UNDEAD);
			}
			else if (r_ptr->flags4 & (RF4_BR_NETH))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam /= 2;
				note = " resists somewhat.";
				if (seen)
					r_ptr->r_flags3 |= (RF3_EVIL);
			}
			break;
		}

			/* Water (acid) damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen)
				obvious = TRUE;
			if ((r_ptr->d_char == 'E') && prefix(name, "W"))
			{
				note = " is immune.";
				dam = 0;
			}
			break;
		}

			/* Cause mutation -- non-chaos breathers get polymorphed. */
		case GF_CAUSE_MUTATION:
		{
			if (!(r_ptr->flags1 & RF1_UNIQUE) &&
				r_ptr->flags4 & RF4_BR_CHAO)
			{

				do_poly = TRUE;
				note = " mutates!";
				dam = 0;

			}
			else
			{
				dam = 0;
				skipped = TRUE;
			}

			break;
		}

			/* Cure mutation -- polymorph and damage chaos breathers. */
		case GF_HEAL_MUTATION:
		{
			if (!(r_ptr->flags1 & RF1_UNIQUE) &&
				r_ptr->flags4 & RF4_BR_CHAO)
			{

				do_poly = TRUE;
				note = " writhes under the power of Order!";

			}
			else
			{
				skipped = TRUE;
				dam = 0;
			}

			break;
		}

			/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen)
				obvious = TRUE;
			/* Start with the assumption that the monster will be
			 * confused.  From GJW -KMW- */
			do_conf = (5 + randint(11) + r) / (r + 1);

			/* Then, factor in special powers.  From GJW -KMW- */
			if ((r_ptr->flags3 & RF3_NO_CONF) ||
				(r_ptr->flags1 & RF1_UNIQUE))
			{
				if (rand_int(100) < 70)
					do_conf = 0;
			}
			if (r_ptr->flags4 & (RF4_BR_CHAO))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			/* 60% chance to attempt polymorph; then monster gets a
			 * saving throw.  Note that the chaos-wielder's level
			 * is irrelevant, as is the damage done.  From GJW -KMW- */
			else
			{
				if (rand_int(100) < 60)
				{
					if (rand_int(3000) > r_ptr->level * r_ptr->level)
						do_poly = TRUE;
				}
			}
			break;
		}

			/* Shards -- Shard breathers resist */
		case GF_SHARD:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_SHAR))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Sound -- Sound breathers resist */
		case GF_SOUND:
		{
			if (seen)
				obvious = TRUE;
			do_stun = (10 + randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_SOUN))
			{
				note = " resists.";
				dam *= 2;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Confusion */
		case GF_CONFUSION:
		{
			if (seen)
				obvious = TRUE;
			do_conf = (10 + randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_CONF))
			{
				note = " resists.";
				dam *= 2;
				dam /= (randint(6) + 6);
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists somewhat.";
				dam /= 2;
				do_conf /= 2; /* Can now be confused!  From GJW -KMW- */
			}
			break;
		}

			/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen)
				obvious = TRUE;
			if ((r_ptr->flags4 & (RF4_BR_DISE)) || prefix(name, "Disen"))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Nexus -- Breathers and Existers resist */
		case GF_NEXUS:
		{
			if (seen)
				obvious = TRUE;
			if ((r_ptr->flags4 & (RF4_BR_NEXU)) || prefix(name, "Nexus"))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Force */
		case GF_FORCE:
		{
			if (seen)
				obvious = TRUE;
			do_stun = (randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_WALL))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_INER))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_TIME))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
			}
			break;
		}

			/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			if (seen)
				obvious = TRUE;
			do_dist = 10 - (r_ptr->level / 20);	/* From GJW -KMW- */
			if (r_ptr->flags4 & (RF4_BR_GRAV))
			{
				note = " resists.";
				dam *= 3;
				dam /= (randint(6) + 6);
				do_dist = 0;
			}
			break;
		}

			/* Space-time warp */
		case GF_QUAKE:
		{
			if (seen)
				obvious = TRUE;
			if (magik(30))
			{
				note = " holds together!";
				dam = 0;
			}
			note = " is scrambled!";
			note_dies = " disintegrates!";
			break;
		}

			/* Pure damage */
		case GF_MANA:
		{
			if (seen)
				obvious = TRUE;
			break;
		}

			/* Meteor -- powerful magic missile */
		case GF_METEOR:
		{
			if (seen)
				obvious = TRUE;
			break;
		}

			/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen)
				obvious = TRUE;
			do_stun = (randint(15) + 1) / (r + 1);
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen)
					r_ptr->r_flags3 |= (RF3_IM_COLD);
			}

			if (r_ptr->flags3 & RF3_HURT_COLD)
			{
				note = " is hit hard!";
				dam *= 2;
				if (seen)
					r_ptr->r_flags3 |= RF3_HURT_COLD;
			}
			break;
		}


			/* Drain Life */
		case GF_RUINATION:
		case GF_DRAIN:
		{
			if (seen)
				obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_UNDEAD)) ||
				(r_ptr->flags3 & (RF3_DEMON)) ||
				(strchr("Egv", r_ptr->d_char)))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (seen)
						r_ptr->r_flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (seen)
						r_ptr->r_flags3 |= (RF3_DEMON);
				}
				note = " is unaffected!";
				obvious = FALSE;
				dam = 0;
			}
			break;
		}

			/* Polymorph monster (Use "dam" as "power") */
		case GF_ALTER:
		case GF_POLY:
		{
			if (seen)
				obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) <
						1 ? 1 : (dam - 10)) + 10))
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
		case GF_CLONE:
		{
			if (seen)
				obvious = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			if (m_ptr->mspeed < 150)
				m_ptr->mspeed += 10;

			/* Attempt to clone. */
			if (multiply_monster(cave_m_idx[y][x]))
			{
				note = " spawns!";
			}
			/* No "real" damage */
			dam = 0;

			break;
		}


			/* Heal Monster (use "dam" as amount of healing) */
		case GF_HEAL:
		{
			if (seen)
				obvious = TRUE;

			/* Wake up */
			m_ptr->csleep = 0;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp)
				m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x])
				p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			note = " looks healthier.";

			/* No "real" damage */
			dam = 0;
			break;
		}


			/* Speed Monster (Ignore "dam") */
		case GF_SPEED:
		{
			if (seen)
				obvious = TRUE;

			/* Speed up */
			if (m_ptr->mspeed < 150)
				m_ptr->mspeed += 10;
			note = " starts moving faster.";

			/* No "real" damage */
			dam = 0;
			break;
		}


			/* Slow Monster (Use "dam" as "power") */
		case GF_SLOW:
		{
			if (seen)
				obvious = TRUE;

			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) <
						1 ? 1 : (dam - 10)) + 10))
			{
				note = " is unaffected!";
				obvious = FALSE;
			}
			/* Normal monsters slow down */
			else
			{
				if (m_ptr->mspeed > 60)
					m_ptr->mspeed -= 10;
				note = " starts moving slower.";
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

			/* Awaken. */
		case GF_WAKE:
		{
			if (seen)
				obvious = TRUE;

			note = " awakens!";
			do_sleep = -dam;
			break;
		}

			/* Sleep (Use "dam" as "power") */
		case GF_SLEEP:
		{
			if (seen)
				obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_SLEEP)) ||
				(r_ptr->level > randint((dam - 10) <
						1 ? 1 : (dam - 10)) + 10))
			{

				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen)
						r_ptr->r_flags3 |= (RF3_NO_SLEEP);
				}

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


			/* Confusion (Use "dam" as "power") */
		case GF_CONF:
		{
			if (seen)
				obvious = TRUE;

			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_CONF)) ||
				(r_ptr->level > randint((dam - 10) <
						1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen)
						r_ptr->r_flags3 |= (RF3_NO_CONF);
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



			/* Lite, but only hurts susceptible creatures */
		case GF_LITE_WEAK:
		{
			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Obvious effect */
				if (seen)
					obvious = TRUE;

				/* Memorize the effects */
				if (seen)
					r_ptr->r_flags3 |= (RF3_HURT_LITE);

				/* Special effect */
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
			}
			/* Normally no damage */
			else
			{
				/* No damage */
				dam = 0;
			}

			/* Try to wake up the monster. */
			do_wake = TRUE;

			break;
		}



			/* Lite -- opposite of Dark */
		case GF_LITE:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_LITE))
			{
				note = " resists.";
				dam *= 2;
				dam /= (randint(6) + 6);
			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (seen)
					r_ptr->r_flags3 |= (RF3_HURT_LITE);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam *= 2;
			}

			/* Try to wake up the monster. */
			do_wake = TRUE;

			break;
		}


			/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen)
				obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_DARK))
			{
				note = " resists.";
				dam *= 2;
				dam /= (randint(6) + 6);
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
				if (seen)
					obvious = TRUE;

				/* Memorize the effects */
				if (seen)
					r_ptr->r_flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
				note = " loses some skin!";
				note_dies = " dissolves!";
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
				if (seen)
					obvious = TRUE;
				if (seen)
					r_ptr->r_flags3 |= (RF3_UNDEAD);
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
				if (seen)
					obvious = TRUE;
				if (seen)
					r_ptr->r_flags3 |= (RF3_EVIL);
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
			if (seen)
				obvious = TRUE;

			/* Prepare to teleport */
			do_dist = dam;

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
				if (seen)
					r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen)
					obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw. Changed by GJW -KMW- */
				if (monster_saves(r_ptr->level, dam))
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
				if (seen)
					r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen)
					obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw. Changed by GJW -KMW- */
				if (monster_saves(r_ptr->level, dam))
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
			/* Obvious */
			if (seen)
				obvious = TRUE;

			/* Apply some fear */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_FEAR)) ||
				(r_ptr->level > randint((dam - 10) <
						1 ? 1 : (dam - 10)) + 10))
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
				if (seen)
					r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen)
					obvious = TRUE;

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
		case GF_BLESS:
		case GF_UNCURSE:
		case GF_HEAVY_UNCURSE:
		case GF_DISP_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen)
					r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen)
					obvious = TRUE;

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
			/* Obvious */
			if (seen)
				obvious = TRUE;

			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";

			break;
		}

			/* Brain smashes */
		case GF_BRAIN_SMASH:
		{
			if (seen)
				obvious = TRUE;

			if (randint(100) < r_ptr->level || r_ptr->flags2 & RF2_STUPID
				|| r_ptr->flags2 & RF2_WEIRD_MIND ||
				r_ptr->flags2 & RF2_EMPTY_MIND)
			{
				note = " resists.";
				dam = 0;
			}
			else
			{
				note = " looks stupider.";
				note_dies = " collapses, a mindless husk.";
				do_stun = dam;
				do_conf = dam;
			}
			break;
		}

			/* Mind blast */
		case GF_MIND_BLAST:
		{
			if (seen)
				obvious = TRUE;

			if (randint(100) < r_ptr->level || r_ptr->flags2 & RF2_STUPID
				|| r_ptr->flags2 & RF2_WEIRD_MIND ||
				r_ptr->flags2 & RF2_EMPTY_MIND)
			{
				note = " resists.";
				dam = 0;
			}
			else
			{
				note = " is blasted by psionic energy.";
				note_dies = " collapses, a mindless husk.";
				do_conf = dam;
			}
			break;
		}

		case GF_CHAOS_DESTRUCTION:
		case GF_WORD_OF_DESTRUCTION:
		{
			if (seen)
				obvious = TRUE;

			/* Delete the monster, not kill it. */
			do_wipe = TRUE;
			note = " disintegrates!";
			break;
		}

		case GF_AWAY_ALL_VERT:
		{
			if (seen)
				obvious = TRUE;

			do_wipe = TRUE;

			if (randint(100) < 50 || p_ptr->depth == 0)
			{
				note = " sinks though the floor!";
			}
			else
			{
				note = " rises through the ceiling!";
			}
			break;
		}

		case GF_RECALL:
		{
			if (seen)
				obvious = TRUE;

			do_dist_player = TRUE;
			break;
		}

		case GF_HEAL_FEAR:
		{
			if (seen)
				obvious = TRUE;

			do_fear = -dam;
			dam = 0;
			break;
		}

		case GF_HEAL_STUN:
		{
			if (seen)
				obvious = TRUE;

			do_stun = -dam;
			dam = 0;
			break;
		}

		case GF_HEAL_CONF:
		{
			if (seen)
				obvious = TRUE;

			do_conf = -dam;
			dam = 0;
			break;
		}

		case GF_EARTHQUAKE:
		{
			if (seen)
				obvious = TRUE;

			/* Delete the monster. */
			if (magik(dam))
			{
				do_wipe = TRUE;
				note = " falls into a chasm!";
			}
			else
			{
				skipped = TRUE;
			}
			break;
		}


		case GF_DETECT_MONSTER:
		{
			if (!(r_ptr->flags2 & RF2_INVISIBLE))
			{
				/* Optimize -- Repair flags */
				repair_mflag_mark = repair_mflag_show = TRUE;

				/* Hack -- Detect the monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

				obvious = TRUE;
			}
			dam = 0;
			break;
		}

		case GF_DETECT_INVIS:
		{
			if (r_ptr->flags2 & RF2_INVISIBLE)
			{
				/* Take note that they are invisible */
				r_ptr->r_flags2 |= (RF2_INVISIBLE);

				/* Optimize -- Repair flags */
				repair_mflag_mark = repair_mflag_show = TRUE;

				/* Hack -- Detect the monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

				obvious = TRUE;
			}
			dam = 0;
			break;
		}

		case GF_DETECT_EVIL:
		{
			if (r_ptr->flags2 & RF3_EVIL)
			{
				/* Take note that they are evil */
				r_ptr->r_flags2 |= (RF3_EVIL);

				/* Optimize -- Repair flags */
				repair_mflag_mark = repair_mflag_show = TRUE;

				/* Hack -- Detect the monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

				obvious = TRUE;
			}
			dam = 0;
			break;
		}

		case GF_DETECT_ANY:
		{
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;

			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			obvious = TRUE;
			dam = 0;
			break;
		}

			/* Probing */
		case GF_IDENT:
		case GF_HEAVY_IDENT:
		case GF_SUPER_IDENT:
		{
			if (seen)
				obvious = TRUE;

			lore_do_probe(cave_m_idx[y][x]);
			dam = 0;
			break;
		}

			/* Genocide */
		case GF_GENOCIDE:
		{
			do_wipe = TRUE;

			/* Take some damage */
			take_hit(randint(4), "the strain of casting Genocide");
			break;
		}

			/* Mass Genocide */
		case GF_MASS_GENOCIDE:
		{
			if (m_ptr->cdis > MAX_SIGHT)
			{
				skipped = TRUE;
				dam = 0;
			}
			else
			{
				do_wipe = TRUE;

				take_hit(randint(3),
					"the strain of casting Mass Genocide");
			}
			break;
		}


			/* Ring of Power. */
		case GF_RING_OF_POWER:
		{
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				skipped = TRUE;
				dam = 0;
				break;
			}

			if (monster_saves(r_ptr->level, p_ptr->lev))
			{
				note = " resists your evil influence!";
				do_conf = dam;
				do_fear = dam;
				dam = 0;

			}
			else
			{
				note = " bows down to your supreme evilness!";
				m_ptr->is_pet = TRUE;
				dam = 0;
			}
			break;
		}

			/* Imprison soul -- make sure a corpse will be generated at death.
			 * This won't work on uniques. Maybe monsters should be allowed a chance
			 * to resist. Or maybe not - you still have to kill the monster to get
			 * any effect. */
		case GF_SOUL_PRISON:
		{
			dam = 0;

			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				note = " resists!";

			}
			else
			{
				note = " looks duller.";

				m_ptr->mflag |= MFLAG_DROP_CORPSE;
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
	if (skipped)
		return (FALSE);

	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE))
		do_poly = FALSE;


	/* "Unique" monsters can only be "killed" by the player */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Uniques may only be killed by the player */
		if (who != -1 && dam > m_ptr->hp)
			dam = m_ptr->hp;
	}

	/* Check for death */
	if (dam > m_ptr->hp)
	{
		/* Extract method of death */
		note = note_dies;
	}


	/* Handle deletion. No deletion in the magical arena! */
	if (do_wipe && !(r_ptr->flags1 & RF1_UNIQUE) &&
		p_ptr->inside_special != SPECIAL_MAGIC_ARENA &&
		!(m_ptr->mflag & MFLAG_QUEST))
	{

		if (seen && note)
			msg_format("%^s%s", m_name, note);

		delete_monster_idx(cave_m_idx[y][x]);
		return TRUE;
	}


	/* Handle awakening */
	if (do_wake)
	{
		int chance = 25;

		/* Stupid monsters rarely wake up */
		if (r_ptr->flags2 & (RF2_STUPID))
			chance = 10;

		/* Smart monsters always wake up */
		if (r_ptr->flags2 & (RF2_SMART))
			chance = 100;

		/* Sometimes monsters wake up */
		if (m_ptr->csleep && (rand_int(100) < chance))
		{
			/* Wake up! */
			m_ptr->csleep = 0;

			note = " wakes up!";
		}
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	if (do_poly && (randint(90) > r_ptr->level))
	{
		/* Default -- assume no polymorph */
		note = " is unaffected!";

		/* Pick a "new" monster race */
		tmp = poly_r_idx(m_ptr->r_idx);

		/* Handle polymorh */
		if (tmp != m_ptr->r_idx)
		{
			/* Obvious */
			if (seen)
				obvious = TRUE;

			/* Monster polymorphs */
			note = " changes!";

			/* Note DON'T Turn off the damage! We want poly + damage at the
			 * same time. 
			 * dam = 0; */

			/* "Kill" the "old" monster */
			delete_monster_idx(cave_m_idx[y][x]);

			/* Create a new monster (no groups) */
			place_monster_aux(y, x, tmp, MON_ALLOC_JUST_ONE);

			/* XXX XXX XXX Hack -- Assume success */

			/* Hack -- Get new monster */
			m_ptr = &m_list[cave_m_idx[y][x]];

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];
		}
	}

	/* Handle "teleport" */
	if (do_dist)
	{
		/* Obvious */
		if (seen)
			obvious = TRUE;

		/* Message */
		note = " disappears!";

		/* Teleport */
		teleport_away(cave_m_idx[y][x], do_dist);

		/* Hack -- get new location */
		y = m_ptr->fy;
		x = m_ptr->fx;
	}

	/* Handle "teleport to player" */
	if (do_dist_player)
	{
		if (seen)
			obvious = TRUE;

		note = " is yanked towards you!";

		teleport_away_to(cave_m_idx[y][x], p_ptr->py, p_ptr->px);

		/* Hack -- update the new location */
		y = m_ptr->fy;
		x = m_ptr->fx;
	}

	/* Sound and Impact breathers never stun */
	if (do_stun && !(r_ptr->flags4 & (RF4_BR_SOUN)) &&
		!(r_ptr->flags4 & (RF4_BR_WALL)))
	{
		bool is_stunned = FALSE;

		/* Obvious */
		if (seen)
			obvious = TRUE;

		/* Get confused */
		if (m_ptr->stunned)
		{
			tmp = m_ptr->stunned + (do_stun / 2);
			is_stunned = TRUE;
			note = " is more dazed.";
		}
		else
		{
			tmp = do_stun;
			note = " is dazed.";
		}

		if (tmp < 0)
			tmp = 0;
		else if (tmp > 200)
			tmp = 200;

		/* Apply stun */
		m_ptr->stunned = tmp;

		if (is_stunned && !m_ptr->stunned)
		{
			note = " looks less stunned.";
		}
	}

	/* Confusion and Chaos breathers (and sleepers) never confuse */
	if (do_conf && !(r_ptr->flags3 & (RF3_NO_CONF)) &&
		!(r_ptr->flags4 & (RF4_BR_CONF)) &&
		!(r_ptr->flags4 & (RF4_BR_CHAO)))
	{
		bool is_confused = FALSE;

		/* Obvious */
		if (seen)
			obvious = TRUE;

		/* Already partially confused */
		if (m_ptr->confused)
		{
			note = " looks more confused.";
			tmp = m_ptr->confused + (do_conf / 2);
			is_confused = TRUE;
		}
		/* Was not confused */
		else
		{
			tmp = do_conf;
			note = " looks confused.";
		}

		if (tmp < 0)
			tmp = 0;
		else if (tmp > 200)
			tmp = 200;

		/* Apply confusion */
		m_ptr->confused = tmp;

		if (is_confused && !m_ptr->confused)
		{
			note = " looks less confused.";
		}
	}

	/* Fear */
	if (do_fear)
	{
		/* Increase fear */
		tmp = m_ptr->monfear + do_fear;

		if (tmp < 0)
			tmp = 0;
		else if (tmp > 200)
			tmp = 200;

		/* Set fear */
		m_ptr->monfear = 200;
	}

	/* Hack -- handle sleep */
	if (do_sleep < 0)
	{
		m_ptr->csleep -= do_sleep;

		if (m_ptr->csleep < 0)
			m_ptr->csleep = 0;

	}
	else if (do_sleep > 0)
	{
		m_ptr->csleep += do_sleep;
	}


	/* Important -- need a monster to hurt! 
	 * In a few rare cases monsters disappear, i.e. when polymorphing, 
	 * for example.
	 */

	if (!cave_m_idx[y][x])
	{
		lite_spot(y, x);
		return (obvious);
	}

	/* If another monster did the damage, hurt the monster by hand */
	/* Handle the case of pet breathers killing w/ spells correctly */

	if (who > 0 && !m_list[who].is_pet)
	{
		/* Redraw (later) if needed */
		if (p_ptr->health_who == cave_m_idx[y][x])
			p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up */
		if (!do_sleep)
			m_ptr->csleep = 0;

		/* Hurt the monster */
		m_ptr->hp -= dam;

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			/* Generate treasure, etc */
			monster_death(cave_m_idx[y][x]);

			/* Delete the monster */
			delete_monster_idx(cave_m_idx[y][x]);

			/* Give detailed messages if destroyed */
			if (note)
				msg_format("%^s%s", m_name, note);
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen)
				msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0 && seen)
				message_pain(cave_m_idx[y][x], dam);
		}
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;
		bool by_pet = FALSE;
		bool give_exp = TRUE;

		if (who > 0)
			by_pet = TRUE;

		if (who < -1)
			give_exp = FALSE;

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(cave_m_idx[y][x], dam, &fear, note_dies, give_exp,
				by_pet))
		{
			/* Dead monster */
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen)
				msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0 && seen)
				message_pain(cave_m_idx[y][x], dam);

			/* Take note */
			if ((fear || do_fear) && (m_ptr->ml))
			{
				/* Sound */
				sound(SOUND_FLEE);

				/* Message */
				msg_format("%^s flees in terror!", m_name);
			}

			/* Pet care. */
			if (dam > 0 && m_ptr->is_pet && !by_pet && give_exp)
			{
				hostile_monsters(cave_m_idx[y][x]);
			}
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
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 *
 * Changed this to allow attacks from gods.
 *
 */
static bool project_p(int who, int r, int y, int x, int dam, int typ)
{
	int k = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster */
	monster_type *m_ptr = NULL;

	/* Monster name (for attacks) */
	char m_name[80] = "something strange";

	/* Monster name (for damage) */
	char killer[80] = "something strange";

	/* Hack -- messages */
	cptr act = NULL;


	/* No player here */
	if (!(cave_m_idx[y][x] < 0))
		return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who)
		return (FALSE);


	/* XXX XXX XXX */
	/* Limit maximum damage */

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* If the player is blind, be more descriptive */
	if (blind)
		fuzzy = TRUE;

	/* Did ``God'' do it? */
	if (who >= -99 && who <= -1)
	{
		byte god = who + 99;

		sprintf(killer, "%s, God%s of %s", deity_info[god].name,
			(deity_info[god].female ? "dess" : ""),
			deity_info[god].god_of);

	}
	else if (who == -100)
	{
		char foo[80];

		strcpy(killer, "an explosion of ");
		describe_attack(typ, foo);
		strcat(killer, foo);
	}
	else
	{

		/* Get the source monster */
		m_ptr = &m_list[who];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, m_ptr, 0x88);
	}


	/* Analyze the damage */
	switch (typ)
	{

	case GF_NOTHING:
	  {
	    if (fuzzy) {
	      msg_print("You feel something.");
	    }
	    break;
	  }


			/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{
			if (fuzzy)
				msg_print("You are hit by acid!");
			acid_dam(dam, killer);
			break;
		}

			/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{
			if (fuzzy)
				msg_print("You are hit by fire!");
			fire_dam(dam, killer);

			break;
		}

			/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{
			if (fuzzy)
				msg_print("You are hit by cold!");
			cold_dam(dam, killer);
			break;
		}

			/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{
			if (fuzzy)
				msg_print("You are hit by lightning!");
			elec_dam(dam, killer);
			break;
		}

			/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			if (fuzzy)
				msg_print("You are hit by poison!");
			if (p_ptr->resist_pois)
				dam = (dam + 2) / 3;
			if (p_ptr->oppose_pois)
				dam = (dam + 2) / 3;
			take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void) set_poisoned(p_ptr->poisoned + rand_int(dam) + 10);
			}
			break;
		}

			/* Standard damage */
		case GF_MISSILE:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

			/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_ORB:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			dam /= 2;
			take_hit(dam, killer);
			break;
		}

			/* Arrow -- XXX no dodging */
		case GF_ARROW:
		{
			if (fuzzy)
				msg_print("You are hit by something sharp!");
			take_hit(dam, killer);
			break;
		}

			/* Plasma -- XXX No resist */
		case GF_PLASMA:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			take_hit(dam, killer);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));

				(void) set_stun(p_ptr->stun + k);
			}
			break;
		}

			/* Nether -- drain experience */
		case GF_NETHER:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");
			if (p_ptr->resist_nethr)
			{
				dam *= 6;
				dam /= (randint(6) + 6);
			}
			else
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					mprint(MSG_WARNING,
						"You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
				}
				else
				{
					mprint(MSG_WARNING,
						"You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
				}
			}
			apply_weird_damage(dam, killer);
			break;
		}

			/* Water -- stun/confuse */
		case GF_WATER:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				(void) set_stun(p_ptr->stun + randint(40));
			}
			if (!p_ptr->resist_confu)
			{
				(void) set_confused(p_ptr->confused + randint(5) + 5);
			}
			take_hit(dam, killer);
			break;
		}

			/* Chaos -- many effects */
		case GF_CHAOS:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");

			if (p_ptr->resist_chaos)
			{
				dam *= 6;
				dam /= (randint(6) + 6);
			}

			if (!p_ptr->resist_confu)
			{
				(void) set_confused(p_ptr->confused + rand_int(20) + 10);
			}

			if (!p_ptr->resist_chaos)
			{
				(void) set_image(p_ptr->image + randint(10));

				if (rand_int(2))
				{
					mprint(MSG_WARNING, "Your body mutates!");
					generate_mutation();
				}
			}

			if (!p_ptr->resist_nethr && !p_ptr->resist_chaos)
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					mprint(MSG_WARNING,
						"You feel your life slipping away!");
					lose_exp(500 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
				}
				else
				{
					mprint(MSG_WARNING,
						"You feel your life draining away!");
					lose_exp(5000 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
				}
			}

			apply_weird_damage(dam, killer);
			break;
		}

			/* Shards -- mostly cutting */
		case GF_SHARD:
		{
			if (fuzzy)
				msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				dam *= 6;
				dam /= (randint(6) + 6);
			}
			else
			{
				(void) set_cut(p_ptr->cut + dam);
			}
			take_hit(dam, killer);
			break;
		}

			/* Sound -- mostly stunning */
		case GF_SOUND:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (p_ptr->resist_sound)
			{
				dam *= 5;
				dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));

				(void) set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer);
			break;
		}

			/* Pure confusion */
		case GF_CONFUSION:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (p_ptr->resist_confu)
			{
				dam *= 5;
				dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_confu && rand_int(100) >= p_ptr->skill_sav)
			{
				(void) set_confused(p_ptr->confused + randint(20) + 10);
			}
			take_hit(dam, killer);
			break;
		}

			/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");
			if (p_ptr->resist_disen)
			{
				dam *= 6;
				dam /= (randint(6) + 6);
			}
			else
			{
				(void) apply_disenchant(0);
			}
			take_hit(dam, killer);
			break;
		}

			/* Nexus -- see above */
		case GF_NEXUS:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");
			if (p_ptr->resist_nexus)
			{
				dam *= 6;
				dam /= (randint(6) + 6);
			}
			else
			{
				if (who < 0)
					break;
				apply_nexus(m_ptr);
			}
			apply_weird_damage(dam, killer);
			break;
		}

			/* Force -- mostly stun */
		case GF_FORCE:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				(void) set_stun(p_ptr->stun + randint(20));
			}
			take_hit(dam, killer);
			break;
		}

			/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");
			(void) set_slow(p_ptr->slow + rand_int(4) + 4);
			take_hit(dam, killer);
			break;
		}

		case GF_LITE_WEAK:
		{
			if (dam && p_ptr->hates_light)
			{
				mprint(MSG_TEMP, "The cruel light scorches your skin!");
				take_hit(dam, killer);
			}
			else
			{
				dam = 0;
			}
			break;
		}


			/* Lite -- blinding */
		case GF_LITE:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (p_ptr->resist_lite)
			{
				dam *= 4;
				dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void) set_blind(p_ptr->blind + randint(5) + 2);
			}

			if (dam && p_ptr->hates_light)
			{
				mprint(MSG_TEMP, "The cruel light scorches your skin!");
				dam *= 2;
			}

			take_hit(dam, killer);
			break;
		}

			/* Dark -- blinding */
		case GF_DARK:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			if (p_ptr->resist_dark || rand_int(100) < p_ptr->skill_sav)
			{
				dam *= 4;
				dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void) set_blind(p_ptr->blind + randint(5) + 2);
			}
			take_hit(dam, killer);
			break;
		}

			/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");

			switch (randint(10))
			{
				case 1:
				case 2:
				case 3:
				case 4:
				case 5:
				{
					mprint(MSG_URGENT, "You feel life has clocked back.");
					lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
					break;
				}

				case 6:
				case 7:
				case 8:
				case 9:
				{
					switch (randint(6))
					{
						case 1:
							k = A_STR;
							act = "strong";
							break;
						case 2:
							k = A_INT;
							act = "bright";
							break;
						case 3:
							k = A_WIS;
							act = "wise";
							break;
						case 4:
							k = A_DEX;
							act = "agile";
							break;
						case 5:
							k = A_CON;
							act = "hale";
							break;
						case 6:
							k = A_CHR;
							act = "beautiful";
							break;
					}

					mformat(MSG_URGENT,
						"You're not as %s as you used to be...", act);

					p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
					if (p_ptr->stat_cur[k] < 3)
						p_ptr->stat_cur[k] = 3;
					p_ptr->update |= (PU_BONUS);
					break;
				}

				case 10:
				{
					mprint(MSG_URGENT,
						"You're not as powerful as you used to be...");

					for (k = 0; k < 6; k++)
					{
						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3)
							p_ptr->stat_cur[k] = 3;
					}
					p_ptr->update |= (PU_BONUS);
					break;
				}
			}
			apply_weird_damage(dam, killer);
			break;
		}

			/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY:
		{
			if (fuzzy)
				msg_print("You are hit by something strange!");
			msg_print("Gravity warps around you.");
			teleport_player(5);
			(void) set_slow(p_ptr->slow + rand_int(4) + 4);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));

				(void) set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer);
			break;
		}

			/* Space-time warp */
		case GF_QUAKE:
		{
			if (magik(70))
			{
				mprint(MSG_WARNING,
					"The molecules of your body scramble!");
				apply_weird_damage(dam, killer);
			}
			break;
		}

			/* Pure damage */
		case GF_MANA:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

			/* Pure damage */
		case GF_METEOR:
		{
			if (fuzzy)
				msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

			/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
			if (fuzzy)
				msg_print("You are hit by something sharp!");
			cold_dam(dam, killer);
			if (!p_ptr->resist_shard)
			{
				(void) set_cut(p_ptr->cut + damroll(5, 8));
			}
			if (!p_ptr->resist_sound)
			{
				(void) set_stun(p_ptr->stun + randint(15));
			}
			break;
		}

		case GF_BRAIN_SMASH:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You feel your brain melt!");

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_sanity_hit(dam, killer);

				if (!p_ptr->resist_blind)
				{
					(void) set_blind(p_ptr->blind + 8 + rand_int(8));
				}
				if (!p_ptr->resist_confu)
				{
					(void) set_confused(p_ptr->confused + rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void) set_paralyzed(p_ptr->paralyzed + rand_int(4) +
						4);
				}
				(void) set_slow(p_ptr->slow + rand_int(4) + 4);
			}

			break;
		}

		case GF_MIND_BLAST:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are blasted by psionic energy!");

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_sanity_hit(dam, killer);

				if (!p_ptr->resist_confu)
				{
					(void) set_confused(p_ptr->confused + rand_int(4) + 4);
				}
			}

			break;
		}

		case GF_AWAY_ALL_VERT:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You have a wrenching sensation.");

			teleport_player_level();
			break;
		}

		case GF_RECALL:
		{
			if (p_ptr->word_recall == 0)
			{
				p_ptr->word_recall = dam;
				mprint(MSG_BONUS, "The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			break;
		}

		case GF_HEAL:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating!");
			(void) hp_player(dam);
			break;
		}

		case GF_HEAL_INSANITY:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating!");
			heal_insanity(dam);
			break;
		}

		case GF_CAUSE_INSANITY:
		{
			mprint(MSG_WARNING, "Your brain feels like it's rotting!");
			take_sanity_hit(dam, killer);
			break;
		}

		case GF_CAUSE_HALLUC:
		{
			set_image(p_ptr->image + dam);
			break;
		}

		case GF_HEAL_HALLUC:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_image(p_ptr->image - dam);
			break;
		}

		case GF_HEAL_FEAR:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_afraid(p_ptr->afraid - dam);
			break;
		}

		case GF_CAUSE_FEAR:
		{
			if (fuzzy)
				mprint(MSG_WARNING,
					"You are hit by something very scary!");

			set_afraid(p_ptr->afraid + dam);
			break;
		}

		case GF_HEAL_CUT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_cut(p_ptr->cut - dam);
			break;
		}

		case GF_CAUSE_CUT:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something sharp.");

			set_cut(p_ptr->cut + dam);
			break;
		}

		case GF_HEAL_STUN:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_stun(p_ptr->stun - dam);
			break;
		}

		case GF_CAUSE_STUN:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			set_stun(p_ptr->stun + dam);
			break;
		}

		case GF_HEAL_POISON:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_poisoned(p_ptr->poisoned - dam);
			break;
		}

		case GF_CAUSE_POISON:
		{
			if (fuzzy)
				mprint(MSG_WARNING,
					"You are hit by something disgustingly slimy.");

			set_poisoned(p_ptr->poisoned + dam);
			break;
		}

		case GF_HEAL_CONF:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_confused(p_ptr->confused - dam);
			break;
		}

		case GF_CAUSE_CONF:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something confusing.");

			set_confused(p_ptr->confused + dam);
			break;
		}

		case GF_HEAL_BLIND:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_blind(p_ptr->blind - dam);
			break;
		}

		case GF_CAUSE_BLIND:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			set_blind(p_ptr->blind + dam);
			break;
		}

		case GF_HEAL_DEX:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_DEX);
			break;
		}

		case GF_HEAL_CHR:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_CHR);
			break;
		}

		case GF_HEAL_STR:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_STR);
			break;
		}

		case GF_HEAL_CON:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_CON);
			break;
		}

		case GF_HEAL_WIS:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_WIS);
			break;
		}

		case GF_HEAL_INT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			do_res_stat(A_INT);
			break;
		}





		case GF_DEC_DEX:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_DEX);
			break;
		}

		case GF_DEC_CHR:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_CHR);
			break;
		}

		case GF_DEC_STR:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_STR);
			break;
		}

		case GF_DEC_CON:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_CON);
			break;
		}

		case GF_DEC_WIS:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_WIS);
			break;
		}

		case GF_DEC_INT:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			do_dec_stat(A_INT);
			break;
		}



		case GF_INC_STR:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_STR);
			break;
		}

		case GF_INC_CON:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_CON);
			break;
		}

		case GF_INC_WIS:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_WIS);
			break;
		}

		case GF_INC_DEX:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_DEX);
			break;
		}

		case GF_INC_INT:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_INT);
			break;
		}

		case GF_INC_CHR:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating.");

			do_inc_stat(A_CHR);
			break;
		}



		case GF_HEAL_LIFE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			restore_level();
			break;
		}

		case GF_HEAL_MUTATION:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			remove_mutation();
			break;
		}

		case GF_CAUSE_MUTATION:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something!");

			mprint(MSG_WARNING, "Your body mutates!");
			generate_mutation();
			break;
		}

		case GF_SHIELD:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_shield(p_ptr->shield + dam);
			break;
		}

		case GF_HEROISM:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_hero(p_ptr->hero + dam);
			break;
		}

		case GF_BERSERK:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_shero(p_ptr->shero + dam);
			break;
		}

		case GF_BLESS:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			set_blessed(p_ptr->blessed + dam);
			break;
		}

		case GF_PROT_EVIL:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_protevil(p_ptr->protevil + dam);
			break;
		}

		case GF_FOOD:
		{
			set_food(p_ptr->food + dam);
			break;
		}

		case GF_MINUS_FOOD:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by something.");

			set_food(p_ptr->food - dam);
			break;
		}

		case GF_INFRA:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_tim_infra(p_ptr->tim_infra + dam);
			break;
		}

		case GF_SEE_INVIS:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_tim_invis(p_ptr->tim_invis + dam);
			break;
		}

		case GF_TRANSMUTE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "Your skin starts tingling.");

			transmute_spell(FALSE);
			break;
		}

		case GF_HEAVY_TRANSMUTE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "Your skin starts tingling.");

			transmute_spell(TRUE);
			break;
		}


		case GF_UNCURSE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			remove_curse();
			break;
		}

		case GF_CURSE_ARMOR:
		{
			if (fuzzy)
				mprint(MSG_WARNING,
					"You are hit by something very dangerous!");

			curse_armor();
			break;
		}

		case GF_CURSE_WEAPON:
		{
			if (fuzzy)
				mprint(MSG_WARNING,
					"You are hit by something very dangerous!");

			curse_weapon();
			break;
		}

		case GF_HEAVY_UNCURSE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something soothing.");

			remove_all_curse();
			break;
		}

		case GF_RECHARGE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			recharge(dam);
			break;
		}

		case GF_IDENT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			ident_spell();
			break;
		}

		case GF_HEAVY_IDENT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			identify_fully();
			break;
		}

		case GF_SUPER_IDENT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			self_knowledge();
			break;
		}

		case GF_RES_FIRE:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_oppose_fire(p_ptr->oppose_fire + dam);
			break;
		}

		case GF_RES_COLD:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_oppose_cold(p_ptr->oppose_cold + dam);
			break;
		}

		case GF_RES_ELEC:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_oppose_elec(p_ptr->oppose_elec + dam);
			break;
		}

		case GF_RES_ACID:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_oppose_acid(p_ptr->oppose_acid + dam);
			break;
		}

		case GF_RES_POIS:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			set_oppose_pois(p_ptr->oppose_pois + dam);
			break;
		}

			/* Mass Genocide -- Kill the player, tee hee hee... */
		case GF_MASS_GENOCIDE:
		{
			mformat(MSG_DEADLY, "Your life essence is snuffed...");

			/* Commit suicide */
			p_ptr->is_dead = TRUE;

			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Leaving */
			p_ptr->leaving = TRUE;

			/* Cause of death */
			strcpy(p_ptr->died_from, "Liquid Death");
			break;
		}

		case GF_ENCHANT_TO_HIT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			enchant_spell(dam, 0, 0);
			break;
		}

		case GF_ENCHANT_TO_DAM:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			enchant_spell(0, dam, 0);
			break;
		}

		case GF_ENCHANT_AC:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			enchant_spell(0, 0, dam);
			break;
		}

		case GF_ENCHANT_EGO_ITEM:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			enchant_spell2(dam, ENCH_MAKE_EGO);
			break;
		}

		case GF_ENCHANT_ARTIFACT:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			enchant_spell2(dam, ENCH_MAKE_ART);
			break;
		}

		case GF_BRAND_AMMO:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			brand_ammo(0, 0);
			break;
		}

		case GF_BRAND_WEAPON:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something.");

			brand_weapon();
			break;
		}

		case GF_ALTER:
		{
			msg_print("The world changes!");
			p_ptr->leaving = TRUE;
			break;
		}

		case GF_SPEED:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something!");
			(void) set_fast(p_ptr->fast + dam);
			break;
		}

		case GF_SLOW:
		{
			if (fuzzy)
				msg_print("You are hit by something slow!");

			if (!p_ptr->free_act || rand_int(100) >= p_ptr->skill_sav)
			{
				(void) set_slow(p_ptr->slow + dam);
			}
			break;
		}

		case GF_SLEEP:
		{
			if (p_ptr->free_act)
				break;
			if (fuzzy)
				msg_print("You fall asleep!");

			if (!p_ptr->free_act || rand_int(100) >= p_ptr->skill_sav)
			{
				set_paralyzed(p_ptr->paralyzed + dam);
			}
			break;
		}

		case GF_AWAY_ALL:
		{
			if (fuzzy)
				msg_print("You are hit by something very fast!");

			teleport_player(dam);
			break;
		}

		case GF_DISP_ALL:
		{
			if (fuzzy)
				msg_print("You are hit by holy chants!");

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				take_hit(dam, killer);
			}
			break;
		}

		case GF_TURN_ALL:
		{
			if (fuzzy)
				msg_print("You hear some very scary noises!");

			if (p_ptr->resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
				(void) set_afraid(p_ptr->afraid + dam);
			}
			break;
		}


		case GF_CHAOS_DESTRUCTION:
		case GF_WORD_OF_DESTRUCTION:
		{
			/* Message if not blind */
			if (!fuzzy)
				mprint(MSG_WARNING, "There is a searing blast of light!");

			/* Blind the player */
			if (!p_ptr->resist_blind && !p_ptr->resist_lite)
			{
				/* Become blind */
				(void) set_blind(p_ptr->blind + 10 + randint(10));
			}
			break;
		}

		case GF_EARTHQUAKE:
		{
			int foo = randint(3);

			switch (foo)
			{
				case 1:
					mprint(MSG_WARNING,
						"The cave quakes!  You are pummeled with debris!");
					break;

				case 2:
					mprint(MSG_WARNING,
						"The cave floor twists in an unnatural way!");
					break;

				default:
					mprint(MSG_WARNING, "The cave ceiling collapses!");
					break;
			}

			switch (randint(3))
			{
				case 1:
					mprint(MSG_BONUS, "You nimbly dodge the blast!");
					break;

				case 2:
					mprint(MSG_URGENT, "You are bashed by rubble!");

					take_hit(dam, "an earthquake");
					(void) set_stun(p_ptr->stun + randint(dam));
					break;

				case 3:
					mprint(MSG_URGENT,
						"You are crushed between the floor and ceiling!");
					take_hit(dam, "an earthquake");
					(void) set_stun(p_ptr->stun + randint(dam));
					break;
			}
			break;
		}


		case GF_RING_OF_POWER:
		{
			ring_of_power(dam);
			break;
		}

			/* Lose "p_ptr->exp/dam" experience points. */
		case GF_DRAIN:
		{
			if (dam)
			{
				mprint(MSG_WARNING, "You feel incompetent.");
				lose_exp((p_ptr->exp / dam) + 1);
			}

			break;
		}

			/* Gain "p_ptr->exp/dam" experience points. */
		case GF_GAIN_EXP:
		{
			if (dam)
			{
				long foo = p_ptr->exp / dam;

				if (foo < 10)
					foo = 10;
				if (foo > 100000L)
					foo = 100000L;

				gain_exp(foo);
			}

			break;
		}

			/* Learn "dam" number of recipes. */
		case GF_LEARN_RECIPE:
		{
			int i, j, k, num = 0;

			for (k = 0; k < dam; k++)
			{

				for (j = 0; j < 100; j++)
				{
					i = rand_int(MAX_RECIPES);

					if (!recipe_recall[i] && recipe_info[i].result_kind)
					{
						recipe_recall[i] = 1;
						num++;
						break;
					}
				}
			}

			mformat(MSG_BONUS, "You have learned %d arcane formulae.",
				num);
			break;
		}

			/* Activate ``glowing hands''. */
		case GF_GLOWING_HANDS:
		{
			if (!p_ptr->confusing)
			{
				p_ptr->confusing = 1;

				mprint(MSG_BONUS, "Your hands begin to glow.");
			}

			break;
		}

		case GF_RUINATION:
		{
			mprint(MSG_URGENT,
				"Your nerves and muscles feel weak and lifeless.");

			take_hit(dam, "Liquid Ruin");

			dec_stat(A_STR, 50, TRUE);
			dec_stat(A_INT, 50, TRUE);
			dec_stat(A_WIS, 50, TRUE);
			dec_stat(A_DEX, 50, TRUE);
			dec_stat(A_CON, 50, TRUE);
			dec_stat(A_CHR, 50, TRUE);

			break;
		}

			/* Heal mana. */
		case GF_HEAL_MANA:
		{
			if (fuzzy)
				mprint(MSG_BONUS,
					"You are hit by something invigorating!");

			p_ptr->csp += dam;

			if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
			}

			p_ptr->redraw |= (PR_MANA);
			p_ptr->window |= (PW_SPELL | PW_PLAYER);
		}

			/* Polymorph self. */
		case GF_POLY:
		{
			if (fuzzy)
				mprint(MSG_WARNING, "You are hit by pure chaos!");

			if (dam >= 1 && dam <= MAX_SHAPES)
			{
				change_shape(dam);
			}

			break;
		}

			/* Repair item. */
		case GF_REPAIR:
		{
			if (fuzzy)
				mprint(MSG_BONUS, "You are hit by something orderly.");

			repair_spell(dam);
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
 * Find the char to use to draw a moving bolt
 * It is moving (or has moved) from (x,y) to (nx,ny).
 * If the distance is not "one", we (may) return "*".
 */
static char bolt_char(int y, int x, int ny, int nx)
{
	if ((ny == y) && (nx == x))
		return '*';
	if (ny == y)
		return '-';
	if (nx == x)
		return '|';
	if ((ny - y) == (x - nx))
		return '/';
	if ((ny - y) == (nx - x))
		return '\\';
	return '*';
}



/*
 * The list of grids affected by the project_aux_foo fns.
 * x, y are points on the map, r is the ``radius'' -- i.e. power
 *
 * There are just enough grids to cover the whole level. If there are
 * nested ``project'' calls, the array of grids will be shared between them.
 */
static byte cave_proj_x[MAX_PROJECT_GRIDS];
static byte cave_proj_y[MAX_PROJECT_GRIDS];
static byte cave_proj_r[MAX_PROJECT_GRIDS];

static s16b max_project_grid = 0;


/* 
 * Add a new grid to the array.
 */
static bool project_grid(byte y, byte x, byte r)
{
	if (max_project_grid < MAX_PROJECT_GRIDS)
	{
		cave_proj_y[max_project_grid] = y;
		cave_proj_x[max_project_grid] = x;
		cave_proj_r[max_project_grid] = r;
		max_project_grid++;
		return TRUE;

	}
	else
	{
		return FALSE;
	}
}


/*
 * Draw some visual effects for spells.
 */

static void draw_spell_effects(byte y, byte x, byte y2, byte x2, int typ,
	int rad)
{
	if (!p_ptr->blind && player_has_los_bold(y, x))
	{

		/* Draw */
		print_rel(bolt_char(y, x, y2, x2), spell_color(typ), y, x);
		Term_fresh();
		Term_xtra(TERM_XTRA_DELAY,
			op_ptr->delay_factor * op_ptr->delay_factor / (rad + 1));
	}
}

/*
 * Affect all the projected grids.
 * Returns TRUE is any grid was affected.
 */
static bool project_finalize(s16b start, int who, int dam, int typ_inp,
	u32b flg)
{
	int i;
	int r, x, y, typ = typ_inp;
	bool ret = FALSE;

	/* Mega-Hack */
	project_m_n = 0;
	project_m_x = 0;
	project_m_y = 0;

	if (start == max_project_grid)
		return FALSE;

	for (i = start; i < max_project_grid; i++)
	{
		y = cave_proj_y[i];
		x = cave_proj_x[i];
		r = cave_proj_r[i];

		/* Mega-hack: Handle the ``RANDOM'' attack type. */
		if (typ_inp == GF_RANDOM)
		{
			typ = rand_range(GF_ARROW, GF_EARTHQUAKE);
		}

		/* Affect features. */
		if (flg & PROJECT_GRID)
		{
			if (project_f(who, r, y, x, dam, typ))
				ret = TRUE;
		}

		/* Affect items */
		if (flg & PROJECT_ITEM)
		{
			if (project_o(who, r, y, x, dam, typ))
				ret = TRUE;
		}

		/* Affect monsters */
		if (flg & PROJECT_KILL)
		{
			if (project_m(who, r, y, x, dam, typ))
				ret = TRUE;
			if (project_p(who, r, y, x, dam, typ))
				ret = TRUE;
		}
	}


	/* Player affected one monster */
	if (who == -1 && project_m_n == 1)
	{
		/* Location */
		x = project_m_x;
		y = project_m_y;

		/* Track if possible */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

			/* Hack -- auto-recall */
			if (m_ptr->ml)
				monster_race_track(m_ptr->r_idx);

			/* Hack - auto-track */
			if (m_ptr->ml)
				health_track(cave_m_idx[y][x]);
		}
	}

	/* Hack -- we're done with these grids. */
	max_project_grid = start;

	return ret;
}




/*
 * Project in the traditional ball/beam/bolt style.
 *
 */

static void project_bolt_beam_ball_aux(int who, int rad, byte y, byte x,
	int typ, u32b flg)
{
	int dist;
	int y9, x9;
	int y1, x1;
	int y2, x2;

	/* Start at player */
	if (who == -1)
	{
		x1 = p_ptr->px;
		y1 = p_ptr->py;
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


	/* Default "destination" */
	y2 = y;
	x2 = x;


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Start at the source */
	x = x1;
	y = y1;
	dist = 0;

	/* Project until done */
	while (1)
	{
		/* Reached the destination, useful only if there is no real source. */
		if (who < -1 && x == x2 && y == y2)
			break;

		/* Paranoia. */
		if (!in_bounds_fully(y, x))
			break;

		/* Never pass through walls */
		/* Hack - Yuck -- Spells pass through chaos fog. */

		if ((!(flg & PROJECT_ETHER) || cave_perma_bold(y, x)) && dist &&
			!cave_floor_bold(y, x) && cave_feat[y][x] != FEAT_CHAOS_FOG)
			break;

		/* If allowed, and we have moved at all, stop when we hit anybody */
		if ((flg & PROJECT_STOP) && dist && (cave_m_idx[y][x] != 0))
			break;

		/* Calculate the new location */
		y9 = y;
		x9 = x;

		mmove2(&y9, &x9, y1, x1, y2, x2);

		/* Paranoia. */
		if (!in_bounds_fully(y9, x9))
			break;

		/* Hack -- Balls explode BEFORE reaching walls or doors */
		if ((!(flg & PROJECT_ETHER) || cave_perma_bold(y, x)) &&
			!cave_floor_bold(y9, x9) && (rad > 0))
			break;

		/* Keep track of the distance traveled */
		dist++;

		/* Nothing can travel further than the maximal distance */
		if (dist > MAX_RANGE)
			break;


		/* Gather beam grids, draw spell effects */

		if (flg & PROJECT_BEAM)
		{
			project_grid(y, x, 0);
			draw_spell_effects(y, x, y, x, typ, 0);
		}
		else
		{
			draw_spell_effects(y, x, y9, x9, typ, 0);
		}

		/* Erase what was drawn. */
		lite_spot(y, x);
		Term_fresh();

		/* Save the new location */
		y = y9;
		x = x9;
	}


	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Determine the blast area, work from the inside out */
	for (dist = 0; dist <= rad; dist++)
	{

		/* Scan the maximal blast area of radius "dist" */
		for (y = y2 - dist; y <= y2 + dist; y++)
		{
			for (x = x2 - dist; x <= x2 + dist; x++)
			{

				/* Ignore "illegal" locations */
				if (!in_bounds(y, x))
					continue;

				/* Enforce a "circular" explosion */
				if (distance(y2, x2, y, x) != dist)
					continue;

				/*  Ball explosions are stopped by walls */
				if ((!(flg & PROJECT_ETHER) || cave_perma_bold(y, x)) &&
					!los(y2, x2, y, x))
					continue;

				/* Save this grid */
				project_grid(y, x, dist);

				/* Draw spell effects */
				draw_spell_effects(y, x, y, x, typ, dist);
			}
		}

		prt_map();
	}
}



/*
 * Affect all visible monsters. 
 * This function does not display spell effects.
 */
static void project_viewable_aux(void)
{
	int i, y, x;
	monster_type *m_ptr;


	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx)
			continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x))
			continue;

		/* Mark this grid */
		project_grid(y, x, 0);
	}
}


/*
 * Large blast of destruction.
 */

static void project_blast_aux(int y1, int x1, int rad, int typ, u32b flg)
{
	int x, y, k, i, j;

	/* Big area of affect */
	for (y = (y1 - rad); y <= (y1 + rad); y++)
	{
		for (x = (x1 - rad); x <= (x1 + rad); x++)
		{

			/* Skip illegal grids */
			if (!in_bounds_fully(y, x))
				continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > rad)
				continue;

			/* Stop if we hit a wall */
			if ((!(flg & PROJECT_ETHER) || cave_perma_bold(y, x)) &&
				!los(y1, x1, y, x))
				continue;

			/* Mark this grid */
			project_grid(y, x, k);
		}
	}

	/* Special effects */
	for (i = 1; i <= rad; i++)
	{
		for (j = 0; j < 5; j++)
		{
			while (TRUE)
			{
				y = rand_range(y1 - rad, y1 + rad);
				x = rand_range(x1 - rad, x1 + rad);

				if (in_bounds_fully(y, x) && distance(y1, x1, y, x) == i)
					break;
			}

			/* Show spell effect */
			draw_spell_effects(y, x, y, x, typ, 1);
		}
	}

	prt_map();
}


/*
 * Cover the whole level, randomly, with one-grid blasts.
 */

static void project_meteor_shower_aux(int typ)
{
	int x, y;
	int i;
	int num = rand_range(1000, 3000);

	for (i = 0; i < num; i++)
	{
		y = rand_range(1, DUNGEON_HGT - 1);
		x = rand_range(1, DUNGEON_WID - 1);

		project_grid(y, x, 0);
		draw_spell_effects(y, x, y, x, typ, 4);
		Term_fresh();
		lite_spot(y, x);
	}
}


/*
 * Affect all grids in the panel.
 */

static void project_panel_aux(void)
{
	int y, x;

	/* Scan the current panel */
	for (y = p_ptr->wy; y < p_ptr->wy + SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx + SCREEN_WID; x++)
		{
			project_grid(y, x, 0);
		}
	}
}


/*
 * Affect every grid in the level. WARNING: This could fail to
 * cover the level in it's entirety if MAX_PROJECT_GRIDS is lower than
 * the entire level area.
 */

static void project_all_aux(void)
{
	int x, y;

	for (y = 1; y < DUNGEON_HGT - 1; y++)
	{
		for (x = 1; x < DUNGEON_WID - 1; x++)
		{
			project_grid(y, x, 0);
		}
	}
}


/*
 * Affect every monster matching a certain letter.
 */

static void project_genocide_aux(void)
{
	int i;
	char typ;

	get_com("Choose a monster race (by symbol): ", &typ);

	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters. */
		if (!m_ptr->r_idx)
			continue;

		if (r_ptr->d_char != typ)
			continue;

		project_grid(m_ptr->fy, m_ptr->fx, 0);
	}
}

/*
 * Wrapper to actually project a spell over some grids.
 * This function calls various project_foo_aux fns. that will 
 * mark various grids and draw spell effects. Then project_finalize will
 * be called, which will go through the marked grids and apply spell
 * effects to them.
 */

bool project(int who, int rad, int y, int x, int dam, int typ, u32b flg)
{
	/* Mega-Hack -- Starting number of grids. This allows nested ``project''
	 * calls. */
	s16b start_grids = max_project_grid;
	bool ret = FALSE;

	/* Hack -- only one type of area effect for now. */
	if (flg & PROJECT_VIEWABLE)
	{
		project_viewable_aux();

	}
	else if (flg & PROJECT_BLAST)
	{
		project_blast_aux(y, x, rad, typ, flg);

	}
	else if (flg & PROJECT_PLAYER)
	{
		project_grid(p_ptr->py, p_ptr->px, 0);
		draw_spell_effects(p_ptr->py, p_ptr->px, p_ptr->py, p_ptr->px, typ,
			0);
		lite_spot(p_ptr->py, p_ptr->px);

	}
	else if (flg & PROJECT_METEOR_SHOWER)
	{
		project_meteor_shower_aux(typ);

	}
	else if (flg & PROJECT_PANEL)
	{
		project_panel_aux();

	}
	else if (flg & PROJECT_ALL)
	{
		project_all_aux();

	}
	else if (flg & PROJECT_GENOCIDE)
	{
		/* Hack -- assume ``dam'' will hold the monster char. */
		project_genocide_aux();

	}
	else
	{
		project_bolt_beam_ball_aux(who, rad, y, x, typ, flg);
	}

	ret = project_finalize(start_grids, who, dam, typ, flg);

	return ret;
}
