/* File: spells1.c */

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

			int by,bx;

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

			/* Require safe location for monster */
			if (!place_monster_here(ny, nx, m_ptr->r_idx)) continue;

			/* Don't allow teleporting into other monster or player */
			if (cave_m_idx[ny][nx]) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (f_info[cave_feat[ny][nx]].flags1 & (FF1_GLYPH)) continue;

			/* Don't allow teleporting into vaults */
			by = ny/BLOCK_HGT;
			bx = nx/BLOCK_HGT;

			/*if (room_info[dun_room[by][bx]].flags & (ROOM_ICKY)) continue; */

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
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			int by,bx;

			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "start" floor space */
			if (!cave_start_bold(y, x)) continue;

			/* Don't allow teleporting into vaults */
			by = y/BLOCK_HGT;
			bx = x/BLOCK_HGT;

			if (room_info[dun_room[by][bx]].flags & (ROOM_ICKY)) continue;			

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
		int by,bx;

		/* Occasionally advance the distance */
		if (ctr++ > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}

		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds_fully(y, x)) break;
		}

		/* Accept "naked" floor grids */
		if (!cave_naked_bold(y, x)) continue;

		/* Require "start" floor space */
		if (!cave_start_bold(y, x)) continue;

		/* Don't allow teleporting into vaults */
		by = y/BLOCK_HGT;
		bx = x/BLOCK_HGT;

		if (room_info[dun_room[by][bx]].flags & (ROOM_ICKY)) continue;

		break;
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport the player one level up or down (random when legal)
 *
 * Note hacks because we now support towers as well as dungeons.
 */
void teleport_player_level(void)
{
	if (adult_ironman)
	{
		msg_print("Nothing happens.");
		return;
	}

	if (!max_depth(p_ptr->dungeon))
	{
		msg_print("Nothing happens.");
		return;
	}
	else if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

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

	else if (is_quest(p_ptr->depth) || (p_ptr->depth >= max_depth(p_ptr->dungeon)))
	{
		/* Hack -- tower level increases depth */
		if (t_info[p_ptr->dungeon].zone[0].tower)
		{
			message(MSG_TPLEVEL, 0, "You sink through the floor.");
		}
		else
		{
			message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");
		}

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else if (rand_int(100) < 50)
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

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

	else
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* Hack -- tower level increases depth */
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
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_MISSILE:	return (TERM_VIOLET);
		case GF_EXPLODE:	return (TERM_VIOLET);
		case GF_ACID:	   return (TERM_SLATE);
		case GF_ELEC:	   return (TERM_BLUE);
		case GF_FIRE:	   return (TERM_RED);
		case GF_COLD:	   return (TERM_WHITE);
		case GF_POIS:	   return (TERM_GREEN);
		case GF_HOLY_ORB:       return (TERM_L_DARK);
		case GF_MANA:	   return (TERM_L_DARK);
		case GF_ARROW:	  return (TERM_WHITE);
		case GF_WATER_WEAK:	return (TERM_SLATE);
		case GF_WATER:	  return (TERM_SLATE);
		case GF_BWATER:	 return (TERM_VIOLET);
		case GF_BMUD:	   return (TERM_ORANGE);
		case GF_LAVA:	   return (TERM_RED);
		case GF_NETHER:	 return (TERM_L_GREEN);
		case GF_CHAOS:	  return (TERM_VIOLET);
		case GF_DISENCHANT:     return (TERM_VIOLET);
		case GF_NEXUS:	  return (TERM_L_RED);
		case GF_CONFUSION:      return (TERM_L_UMBER);
		case GF_SOUND:	  return (TERM_YELLOW);
		case GF_TERRIFY:	return (TERM_L_WHITE);
		case GF_SHARD:	  return (TERM_UMBER);
		case GF_FORCE:	  return (TERM_UMBER);
		case GF_INERTIA:	return (TERM_L_WHITE);
		case GF_GRAVITY:	return (TERM_L_WHITE);
		case GF_TIME:	   return (TERM_L_BLUE);
		case GF_LITE_WEAK:      return (TERM_ORANGE);
		case GF_LITE:	   return (TERM_ORANGE);
		case GF_DARK_WEAK:      return (TERM_L_DARK);
		case GF_DARK:	   return (TERM_L_DARK);
		case GF_PLASMA:	 return (TERM_RED);
		case GF_METEOR:	 return (TERM_RED);
		case GF_ICE:	    return (TERM_WHITE);
		case GF_SALT_WATER:     return (TERM_L_GREEN); /* Heh heh heh */
		case GF_STEAM:	return (TERM_L_WHITE);
		case GF_VAPOUR:	return (TERM_VIOLET);
		case GF_SMOKE:	return (TERM_L_DARK);
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
		if ((old_chp > warning))
		{
			bell("Low hitpoint warning!");
		}

		/* Message */
		msg_print("*** LOW HITPOINT WARNING! ***");
		msg_print(NULL);
	}
}





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
		case TV_MAP:
		{
			return (TRUE);
		}


		/* Junk is useless */
		case TV_BONE:
		case TV_BODY:
		case TV_SKIN:
		case TV_EGG:
		case TV_HOLD:
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
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_SONG_BOOK:
		case TV_INSTRUMENT:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls burn */
		case TV_STAFF:
		case TV_SCROLL:
		case TV_MAP:
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
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate water?
 */
static bool hates_water(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		/* Hack -- immerse vampire skeletons in running water */
		case TV_BONE:
		case TV_SCROLL:
		case TV_MAP:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_SONG_BOOK:
		case TV_INSTRUMENT:
		case TV_FOOD:
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
	if (f2 & (TR2_IGNORE_ACID))
	{
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_ACID,0x0L);
		return (FALSE);
	}
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
	if (f2 & (TR2_IGNORE_ELEC))
	{
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_ELEC,0x0L);
		return (FALSE);
	}
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
	if (f2 & (TR2_IGNORE_FIRE))
	{
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_FIRE,0x0L);
		return (FALSE);
	}
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
	if (f2 & (TR2_IGNORE_COLD))
	{
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_COLD,0x0L);
		return (FALSE);
	}
	return (TRUE);
}


/*
 * Soak things through
 */
static int set_water_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_water(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IGNORE_WATER))
	{
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_WATER,0x0L);
		return (FALSE);
	}
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
	int i, j, k, amt;

	object_type *o_ptr;

	char o_name[80];


	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

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
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

				/* Message */
				msg_format("%sour %s (%c) %s destroyed!",
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

	u32b f1, f2, f3;

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
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Object resists */
	if (f2 & (TR2_IGNORE_ACID))
	{
		/* Always notice */
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_ACID,0x0L);

		msg_format("Your %s is unaffected!", o_name);

		return (TRUE);
	}

	/* Always notice */
	object_not_flags(o_ptr,0x0L,TR2_IGNORE_ACID,0x0L);

	/* Message */
	msg_format("Your %s is damaged!", o_name);

	/* Damage the item */
	o_ptr->to_a--;

	/* Hack --- unsense the item */
	o_ptr->ident &= ~(IDENT_SENSE);	

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

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
void acid_dam(int dam, cptr kb_str, bool inven)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total Immunity */
	if (p_ptr->immune_acid)
	{
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_ACID,0x0L);

		return;
	}

	/* No damage */
	if (dam <= 0) return;

	/* Resist the damage */
	if (p_ptr->resist_acid)
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_ACID,0x0L);

		dam = (dam + 2) / 3;
	}
	else
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_ACID,0x0L);
	}

	if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (inven) inven_damage(set_acid_destroy, inv);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, cptr kb_str, bool inven)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_elec)
	{
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_ELEC,0x0L);

		return;
	}

	/* No damage */
	if (dam <= 0) return;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
	if (p_ptr->resist_elec)
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_ELEC,0x0L);

		dam = (dam + 2) / 3;
	}
	else
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_ELEC,0x0L);
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (inven) inven_damage(set_elec_destroy, inv);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, cptr kb_str, bool inven)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Totally immune */
	if (p_ptr->immune_fire)
	{
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_FIRE,0x0L);

		return;
	}

	/* No damage */
	if (dam <= 0) return;

	/* Resist the damage */
	if (p_ptr->resist_fire)
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_FIRE,0x0L);

		dam = (dam + 2) / 3;
	}
	else
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_FIRE,0x0L);
	}

	if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (inven) inven_damage(set_fire_destroy, inv);
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, cptr kb_str, bool inven)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_cold)
	{
		/* Always notice */
		equip_can_flags(0x0L,TR2_IM_COLD,0x0L);

		return;
	}

	/* No damage */
	if (dam <= 0) return;

	/* Resist the damage */
	if (p_ptr->resist_cold)
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_COLD,0x0L);

		dam = (dam + 2) / 3;
	}
	else
	{
		/* Sometimes notice */
		if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_COLD,0x0L);
	}
	if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (inven) inven_damage(set_cold_destroy, inv);
}

/*
 * Hurt the player with Water
 */
void water_dam(int dam, cptr kb_str, bool inven)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Check for light being wielded */
	object_type *o_ptr = &inventory[INVEN_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LITE)
	{
		/* Hack -- Use up fuel (except on artifacts) */
		if (!artifact_p(o_ptr) && (o_ptr->pval > 0))
		{
			/* Douse light */
			o_ptr->pval = 0;

			/* Hack -- Special treatment when blind */
			if (p_ptr->blind)
			{
				/* Hack -- save some light for later */
				o_ptr->pval++;
			}

			/* The light is now out */
			else
			{
				disturb(0, 0);
				msg_print("Your light has gone out!");
			}
		}
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (inven) inven_damage(set_water_destroy, inv);
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
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, int permanent)
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
			if (loss < 1) loss = 1;
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
 * This function is also called from the "melee" code.
 *
 * The "mode" is currently unused.
 *
 * Return "TRUE" if the player notices anything.
 */
bool apply_disenchant(int mode)
{
	int t = 0;

	object_type *o_ptr;

	char o_name[80];

	/* Prevent compiler warning */
	(void)mode;

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
		msg_format("Your %s (%c) resist%s disenchantment!",
			   o_name, index_to_label(t),
			   ((o_ptr->number != 1) ? "" : "s"));

		/* Always notice */
		object_can_flags(o_ptr,0x0L,TR2_IGNORE_MASK,TR3_INSTA_ART);

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

	/* Hack --- unsense the item */
	o_ptr->ident &= ~(IDENT_SENSE);	

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

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
static void apply_nexus(monster_type *m_ptr)
{
	int max1, cur1, max2, cur2, ii, jj;

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

			break;
		}
	}
}





/*
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;


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
		if (!(play_info[y][x] & (PLAY_SEEN))
		    && !(p_ptr->blind))
		{
			/* Temporarily seen */
			play_info[y][x] |= (PLAY_SEEN);

			/* Remember? */
			note_spot(y,x);

			/* Temporarily seen */
			play_info[y][x] &= ~(PLAY_SEEN);

			/* Light? */
			lite_spot(y,x);

			/* Get monster */
			if (cave_m_idx[y][x] > 0 )
			{
				monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Detect all non-invisible monsters */
				if ((!(r_ptr->flags2 & (RF2_INVISIBLE)) || (p_ptr->tim_invis) || (p_ptr->see_inv))
							&& !(m_ptr->mflag & (MFLAG_HIDE)))
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
 * Perhaps we should affect doors and/or walls.
 */
bool project_f(int who, int r, int y, int x, int dam, int typ)
{
	bool obvious = FALSE;
	bool burnout = FALSE;

	cptr f;

	/* Set feature name */
	f = (f_name + f_info[cave_feat[y][x]].name);

	/* This is dangerous when creating features at the moment */
#if 0
	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);
#else
	/* Prevent warning */
	r = 0;
#endif

	if (!variant_hurt_feats) dam = 0;

	/* Hack -- prevent smoke/vapour etc on floors */
	if ((typ != GF_FEATURE) && (who) &&
		(f_info[cave_feat[y][x]].flags1 & (FF1_FLOOR)))
	{
		burnout = TRUE;
	}


	/* Analyze the type */
	switch (typ)
	{
		case GF_ACID:
		case GF_VAPOUR:
		{

			/* Hack -- halve acid damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;
		
			/* Destroy hurt by acid */
			if ((f_info[cave_feat[y][x]].flags2 & (FF2_HURT_ACID))  &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s dissolves.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ACID);
			}
			break;
		}
		case GF_FIRE:
		case GF_SMOKE:
		{
			/* Hack -- halve fire damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;
		
			/* Drop through */
		}
		case GF_LAVA:
		case GF_PLASMA:
		{
			if ((f_info[cave_feat[y][x]].flags2 & (FF2_HURT_FIRE)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s burns up.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_FIRE);
			}

			obvious |= temp_lite(y,x);

			break;
		}
		case GF_COLD:
		{
			/* Hack -- double cold damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;

			/* Drop through */
		}
		case GF_ICE:
		{
			if ((f_info[cave_feat[y][x]].flags2 & (FF2_HURT_COLD)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s freezes.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_COLD);
			}
			break;
		}
		case GF_EXPLODE:
		case GF_METEOR:
		case GF_SHARD:
		case GF_FORCE:
		case GF_MANA:
		case GF_SOUND:
		{
			if ((f_info[cave_feat[y][x]].flags2 & (FF2_KILL_HUGE)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s shatters.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_KILL_HUGE);
			}
			break;
		}
		/* Electricity */
		case GF_ELEC:
		{
			/* Hack -- double electricy damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;
		
			if ((f_info[cave_feat[y][x]].flags3 & (FF3_HURT_ELEC)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s is struck by lightening.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ELEC);

			}

			obvious |= temp_lite(y,x);
			break;
		}

		/* Water */
		case GF_WATER_WEAK:
		case GF_SALT_WATER:
		case GF_WATER:
		{
			if ((f_info[cave_feat[y][x]].flags3 & (FF3_HURT_WATER)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s floods.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_WATER);
			}
			break;
		}
		
		/* Boiling water */
		case GF_STEAM:
		case GF_BWATER:
		{

			if ((f_info[cave_feat[y][x]].flags3 & (FF3_HURT_BWATER)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s evapourates.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_BWATER);
			}
			break;
		}
		case GF_POIS:
		{
			if ((f_info[cave_feat[y][x]].flags3 & (FF3_HURT_POIS)) &&
			       (dam > (f_info[cave_feat[y][x]].power*10)))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s is poisoned.",f);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_POIS);
			}
			break;
		}

		/* Ignore some effects */
		case GF_HOLY_ORB:
		{
			break;
		}


		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Create closed door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					obvious = TRUE;
				}
			}

			/* Destroy traps */
			if (f_info[cave_feat[y][x]].flags1 & (FF1_TRAP))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}

				/* Destroy the trap */
				cave_alter_feat(y, x, FS_DISARM);
			}

			/* Disarm other traps */
			else if (f_info[cave_feat[y][x]].flags1 & (FF1_DISARM))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_print("Click!");
					obvious = TRUE;
				}

				/* Destroy the trap */
				cave_alter_feat(y, x, FS_TUNNEL);
			}

			/* Locked doors are unlocked */
			else if (f_info[cave_feat[y][x]].flags1 & (FF1_OPEN))
			{
				/* Unlock the door */
				cave_alter_feat(y, x, FS_OPEN);

				/* Check line of sound */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
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

			/* Destroy traps */
			if (f_info[cave_feat[y][x]].flags1 & (FF1_DOOR | FF1_TRAP))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}

				/* Destroy the door */
				cave_alter_feat(y, x, FS_TUNNEL);
			}

			break;
		}

		/* Jam Doors */
		case GF_LOCK_DOOR:
		{

			/* Close doors/traps/chests */
			if (f_info[cave_feat[y][x]].flags1 & (FF1_CLOSE))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					obvious = TRUE;
				}

				/* Destroy the door */
				cave_alter_feat(y, x, FS_CLOSE);
			}

			/* Jam doors */
			while (f_info[cave_feat[y][x]].flags1 & (FF1_SPIKE))
			{
				int feat = cave_feat[y][x];

				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
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
			if (f_info[cave_feat[y][x]].flags2 & (FF2_HURT_ROCK))
			{
				/* Check line of sight */
				if ((player_has_los_bold(y, x)) && (f_info[cave_feat[y][x]].flags1 & FF1_NOTICE))
				{
					msg_format("The %s turns to mud.", f);
					obvious = TRUE;
				}

				/* Destroy the trap */
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
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (play_info[y][x] & (PLAY_MARK)) obvious = TRUE;

			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/* Place a trap */
			feat_near(FEAT_INVIS, y, x);

			break;
		}

		/* Make features */
		case GF_FEATURE:
		{
			/* Require a "floor or ground" grid */
			if (!(f_info[cave_feat[y][x]].flags1 & (FF1_FLOOR))
			 && !(f_info[cave_feat[y][x]].flags3 & (FF3_GROUND))) break;

			/* Don't hit caster */
			if (cave_m_idx[y][x]== who) break;

			/* Place a feature */
			if (dam) cave_set_feat(y,x,dam);

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
			char name[80];

			if (f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) cave_alter_feat(y,x,FS_SECRET);

			cave_alter_feat(y,x,FS_BRIDGE);

			strcpy(name,f_name+f_info[cave_feat[y][x]].name);

			if (!(strstr(name,"stone bridge"))) cave_set_feat(y,x,old_feat);

			break;
		}

		/* Raise water */
		case GF_RAISE:
		{
			int feat = cave_feat[y][x];

			if (f_info[feat].flags2 & (FF2_WATER) && !((f_info[feat].flags2 & (FF2_CHASM)) ||
				(f_info[feat].flags1 & (FF1_LESS)) || (f_info[feat].flags2 & (FF2_FILLED))))
			{
				cave_set_feat(y,x,FEAT_WATER_K);
			}
			break;
		}

		/* Lower water */
		case GF_LOWER:
		{
			int feat = cave_feat[y][x];

			if (prefix(f_name+f_info[feat].name,"stone bridge")) break;

			if (f_info[feat].flags2 & (FF2_CAN_SWIM))
			{
				cave_set_feat(y,x,FEAT_FLOOR_EARTH);
			}
			break;
		}


		/* Lite up the grid */
		case GF_LITE_WEAK:
		case GF_LITE:
		{
			int i;

			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

			/* Fully update the visuals */
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);


			}

			if (!(cave_info[y][x] & (CAVE_XLOS)))
				for (i = 0; i < 8; i++)
			{
				int yy = y + ddy_ddd[i];
				int xx = x + ddx_ddd[i];

				/* Ignore annoying locations */
				if (in_bounds_fully(yy, xx))
				{
					/* Turn on the light */
					cave_info[yy][xx] |= (CAVE_GLOW);
				}

				/* Grid is in line of sight */
				if (player_has_los_bold(yy, xx))
				{
					/* Observe */
					obvious = TRUE;

					/* Fully update the visuals */
					p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
				}

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
			if (cave_feat[y][x] <= FEAT_INVIS)
			{
				/* Forget */
				play_info[y][x] &= ~(PLAY_MARK);
			}

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
			}

			/* All done */
			break;
		}
		case GF_NOTHING:

			dam=0;
			break;


	}

	/* Apply burnout */
	if ((burnout) &&
		(f_info[cave_feat[y][x]].flags3 & (FF3_SPREAD)))
	{
		cave_alter_feat(y,x,FS_SPREAD);
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
	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	u32b f1, f2, f3;
	u32b if1=0;
	u32b if2=0;
	u32b if3=0;

	char o_name[80];

#if 0 /* unused */
	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);
#endif /* 0 */

	/* Prevent warning */
	who = 0;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;

		cptr note_kill = NULL;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

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

				/* Hack -- halve acid damage in water */
				if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;

				if (hates_acid(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (f2 & (TR2_IGNORE_ACID)) ignore = TRUE;
					if2 |= TR2_IGNORE_ACID;
				}
				break;
			}

			/* Elec -- Rings and Wands */
			case GF_ELEC:
			{

				/* Hack -- double electricy damage in water */
				if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;
			
				if (hates_elec(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f2 & (TR2_IGNORE_ELEC)) ignore = TRUE;
					if2 |= TR2_IGNORE_ELEC;
				}
				break;
			}

			/* Fire -- Flammable objects */
			case GF_FIRE:
			{

				/* Hack -- halve fire damage in water */
				if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;
			
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
					if2 |= TR2_IGNORE_FIRE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{

				/* Hack -- double cold damage in water */
				if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;

				if (hates_cold(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (f2 & (TR2_IGNORE_COLD)) ignore = TRUE;
					if2 |= TR2_IGNORE_COLD;
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
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
					if2 |= TR2_IGNORE_FIRE;
				}
				if (hates_elec(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f2 & (TR2_IGNORE_ELEC)) ignore = TRUE;
					if2 |= TR2_IGNORE_ELEC;
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
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
					if2 |= TR2_IGNORE_FIRE;
				}
				if (hates_cold(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " shatter!" : " shatters!");
					if (f2 & (TR2_IGNORE_COLD)) ignore = TRUE;
					if2 |= TR2_IGNORE_COLD;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARD:
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

			case GF_RAISE:
			{
				if ((cave_feat[y][x] == FEAT_WATER) && (hates_water(o_ptr)))
				{
					note_kill = (plural ? " soak through!" : " soaks through!");
					do_kill = TRUE;
					if (f2 & (TR2_IGNORE_WATER)) ignore = TRUE;
					if2 |= TR2_IGNORE_WATER;
				}
				break;
			}
			case GF_WATER:
			case GF_BWATER:
			case GF_WATER_WEAK:
			case GF_SALT_WATER:
			{
				if (hates_water(o_ptr))
				{
					note_kill = (plural ? " soak through!" : " soaks through!");
					do_kill = TRUE;
					if (f2 & (TR2_IGNORE_WATER)) ignore = TRUE;
					if2 |= TR2_IGNORE_WATER;
				}
				break;
			}

			/* Mana -- destroys everything */
			/* Explosion -- very destructive to objects */
			case GF_EXPLODE:
			case GF_MANA:
			{
				do_kill = TRUE;
				note_kill = (plural ? " melt!" : " melts!");
				if ((f2 & (TR2_IGNORE_ACID)) &&
				    (f2 & (TR2_IGNORE_COLD)) &&
				    (f2 & (TR2_IGNORE_ELEC)) &&
				    (f2 & (TR2_IGNORE_FIRE)) &&
				    (f2 & (TR2_IGNORE_WATER))) ignore = TRUE;

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

		}


		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Containers/figurines release contents */
			if (((o_ptr->tval == TV_FIGURE) || (o_ptr->tval == TV_HOLD))
				&& (o_ptr->name3 > 0))
			{
				while (o_ptr->number)
				{
					(void)(race_near(o_ptr->name3, y, x));
	
					o_ptr->number--;
				}
			}

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
				if (obvious)
				{
					/* Get known flags */
					u32b k1,n1;
					u32b k2,n2;
					u32b k3,n3;

					k1 = o_ptr->can_flags1;
					k2 = o_ptr->can_flags2;
					k3 = o_ptr->can_flags3;

					/* Learn about resistences */
					if (if1 | if2 | if3)
					{
						object_can_flags(o_ptr,if1,if2,if3);
					}

					/* Item is unbreakable */
					else
					{
						if (!object_known_p(o_ptr))
						{
							/* Sense the object */
							o_ptr->discount = INSCRIP_UNBREAKABLE;

							/* Hack -- for holy orb */
							if (typ == GF_HOLY_ORB) o_ptr->discount = INSCRIP_TERRIBLE;

							/* The object has been "sensed" */
							o_ptr->ident |= (IDENT_SENSE);
						}

						object_can_flags(o_ptr,0x0L,TR2_IGNORE_MASK,0x0L);
					}

					/* Check for new flags */
					n1 = o_ptr->can_flags1 & ~(k1);
					n2 = o_ptr->can_flags2 & ~(k2);
					n3 = o_ptr->can_flags3 & ~(k3);

					if (n1 || n2 || n3) msg_format("The %s%s unaffected!",
									o_name, 
									plural ? " are" : " is");

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

				/* Splash damage on terrain */
				(void)project_f(0, r, y, x, damroll(1, o_ptr->weight), typ);

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
bool project_m(int who, int r, int y, int x, int dam, int typ)
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

	/* Trapdoor setting (true or false) */
	int do_more = 0;

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

	/* No monster here */
	if (!(cave_m_idx[y][x] > 0)) return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who) return (FALSE);


	/* Obtain monster info */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	name = (r_name + r_ptr->name);
	if (m_ptr->ml) seen = TRUE;


	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);



	/* Some monsters get "destroyed" */
	if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
	    (r_ptr->flags2 & (RF2_STUPID)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}


	/* Analyze the damage type */
	switch (typ)
	{

		/* Magic missile -- pure damage */
		case GF_MISSILE:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Explosion -- destructive -- pure damage */
		case GF_EXPLODE:
		{
			if (seen) obvious = TRUE;
			break;
		}
		/* Acid */
		case GF_ACID:
		{

			/* Hack -- halve acid damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_ACID))) note = " is immune to acid.";
					l_ptr->flags3 |= (RF3_IM_ACID);
				}
			}
			else if (r_ptr->flags3 & (RF2_ARMOR))
			{
				dam /= 2;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF2_ARMOR))) note = " is protected by armour.";
					l_ptr->flags3 |= (RF2_ARMOR);
				}
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{

			/* Hack -- double electricy damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;


			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_ELEC))) note = " is immune to lightning.";
					l_ptr->flags3 |= (RF3_IM_ELEC);
				}
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			/* Hack -- halve fire damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " is immune to fire.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			/* Hack -- double cold damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_COLD))) note = " is immune to cold.";
					l_ptr->flags3 |= (RF3_IM_COLD);
				}
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_POIS))) note = " is immune to poison.";
					l_ptr->flags3 |= (RF3_IM_POIS);
				}
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
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_EVIL))) note = " is hit hard.";
					l_ptr->flags3 |= (RF3_EVIL);
				}
			}
			break;
		}

		/* Arrow -- armored monsters defend */
		case GF_ARROW:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags2 & (RF2_ARMOR)) && (rand_int(100) < r_ptr->ac /3))
			{
				note = " blocks it with a shield.";
				dam = 0;
				if (seen) l_ptr->flags3 |= (RF2_ARMOR);
			}
			break;
		}

		/* Plasma -- perhaps check ELEC or FIRE XXX */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_PLAS))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_RES_PLAS))) note = " resists plasma.";
					l_ptr->flags3 |= (RF3_RES_PLAS);
				}
			}
			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				dam = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_UNDEAD))) note = " is immune to nether.";
					l_ptr->flags3 |= (RF3_UNDEAD);
				}
			}
			else if (r_ptr->flags3 & (RF3_RES_NETH))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_RES_NETH))) note = " resists nether.";
					l_ptr->flags3 |= (RF3_RES_NETH);
				}
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam /= 2;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_EVIL))) note = " somewhat resists nether.";
					l_ptr->flags3 |= (RF3_EVIL);
				}
			}
			break;
		}

		/* Raise/lower water damage -- everything except Water spirits/elementals are immune */
		case GF_RAISE:
		case GF_LOWER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_WATER))
			{
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_WATER))) note = " is disrupted.";
					l_ptr->flags3 |= (RF3_IM_WATER);
				}
			}
			else
			{
				/* No damage */
				dam = 0;
			}
			break;
		}

		/* Water damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_WATER))
			{
				dam = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_WATER))) note = " is immune to water.";
					l_ptr->flags3 |= (RF3_IM_WATER);
				}
			}
			else if (r_ptr->flags2 & (RF2_CAN_SWIM))
			{
				dam /= 2;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF2_CAN_SWIM))) note = " swims through the water.";
					l_ptr->flags3 |= (RF2_CAN_SWIM);
				}
			}
			else
			{
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}
		/* Weak water damage -- Heavily stunned/confused take damage */
		/* Hack -- we stun monsters to make them slow down and drown */
		case GF_WATER_WEAK:
		{
			if (seen) obvious = TRUE;
			if (!(r_ptr->flags2 & (RF2_CAN_SWIM)) && ((m_ptr->stunned > 100) || (m_ptr->confused)))
			{
				note = " is drowning.";
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
                        else if (!(r_ptr->flags2 & (RF2_CAN_SWIM)))
			{
				dam = 0;
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}

 		/* Weak water damage -- Heavily stunned/confused take damage */
		/* Hack -- also worms that can't swim take lots of damage */
		/* Hack -- we stun monsters to make them slow down and drown */
		case GF_SALT_WATER:
		{
			if (seen) obvious = TRUE;
			if (!(r_ptr->flags2 & (RF2_CAN_SWIM)) && (r_ptr->d_char == 'w'))
			{
				dam *= 2;
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			else if (!(r_ptr->flags2 & (RF2_CAN_SWIM)) && ((m_ptr->stunned > 100) || (m_ptr->confused)))
			{
				note = " is drowning.";
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
                        else if (!(r_ptr->flags2 & (RF2_CAN_SWIM)))
			{
				dam = 0;
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}


		/* Boiling water damage -- Water spirits/elementals are immune */
		case GF_BWATER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " is immune to fire.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
			}

			if (r_ptr->flags3 & (RF3_IM_WATER))
			{
				dam = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_WATER))) note = " is immune to water.";
					l_ptr->flags3 |= (RF3_IM_WATER);
				}
			}
			else if (!(r_ptr->flags2 & (RF2_CAN_SWIM)) && ((m_ptr->stunned > 100) || (m_ptr->confused)))
			{
				note = " is drowning.";
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}


		/* Lava damage -- Magma spirits/elementals are resistant */
		case GF_LAVA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_LAVA))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_RES_LAVA))) note = " resists lava.";
					l_ptr->flags3 |= (RF3_RES_LAVA);
				}
			}
			else if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				dam /= 2;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " somewhat resists lava.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
			}
			break;
		}

		/* Boiling mud damage -- Ooze spirits/elementals are immune */
		case GF_BMUD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " somewhat resists boiling mud.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
			}
			if ((r_ptr->flags2 & (RF2_CAN_DIG)) && (r_ptr->flags3 & (RF3_OOZE)))
			{
				dam = 0;
				if (seen)
				{
					if (!(l_ptr->flags2 & (RF2_CAN_DIG))) note = " digs through the boiling mud.";
					if (!(l_ptr->flags3 & (RF3_OOZE))) note = " oozes through the boiling mud.";
					l_ptr->flags2 |= (RF2_CAN_DIG);
					l_ptr->flags3 |= (RF3_OOZE);
				}
			}
			else if (!(r_ptr->flags2 & (RF2_CAN_DIG)) && ((m_ptr->stunned > 100) || (m_ptr->confused)))
			{
				note = " is drowning.";
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}

		/* Suffocation damage for any monster in terrain they can't handle */
		/* eg fish out of water */
		/* Heavily stunned/confused take damage */
		case GF_SUFFOCATE:
		{
			if (seen) obvious = TRUE;
                        if (!(r_ptr->flags3 & (RF3_NONLIVING)) && ((m_ptr->stunned > 100) || (m_ptr->confused)))
			{
				note = " is drowning.";
				do_conf = (10 + randint(15) + r) / (r + 1);
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
                        else if (!(r_ptr->flags3 & (RF3_NONLIVING)))
			{
				dam = 0;
				do_stun = (10 + randint(15) + r) / (r + 1);
			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;
			do_conf = (5 + randint(11) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_CHAO))
			{
				dam *= 3; dam /= (randint(6)+6);
				do_poly = FALSE;
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_CHAO))) note = " resists chaos.";
					l_ptr->flags4 |= (RF4_BR_CHAO);
				}
			}
			break;
		}

		/* Hallucination -- Chaos breathers resist */
		case GF_HALLU:
		{
			if (seen) obvious = TRUE;
			do_conf = (5 + randint(11) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_CHAO))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_CHAO))) note = " resists hallucinations.";
					l_ptr->flags4 |= (RF4_BR_CHAO);
				}
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_SHAR))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_SHAR))) note = " resists shards.";
					l_ptr->flags4 |= (RF4_BR_SHAR);
				}
			}
			break;
		}

		/* Sound -- Sound breathers resist */
		case GF_SOUND:
		{
			if (seen) obvious = TRUE;
			do_stun = (10 + randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_SOUN))
			{
				dam *= 2; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_SOUN))) note = " resists sound.";
					l_ptr->flags4 |= (RF4_BR_SOUN);
				}
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			if (seen) obvious = TRUE;
			do_conf = (10 + randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_CONF))
			{
				dam *= 2; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_CONF))) note = " resists confusion.";
					l_ptr->flags4 |= (RF4_BR_CONF);
				}
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				dam /= 2;
				do_conf = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_NO_CONF))) note = " cannot be confused.";
					l_ptr->flags3 |= (RF3_NO_CONF);
				}
			}
			break;
		}

		/* Disenchantment */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_DISE))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_RES_DISE))) note = " resists disenchantment.";
					l_ptr->flags3 |= (RF3_RES_DISE);
				}
			}
			break;
		}

		/* Nexus */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_NEXU))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_RES_NEXU))) note = " resists nexus.";
					l_ptr->flags3 |= (RF3_RES_NEXU);
				}
			}
			break;
		}

		/* Force */
		case GF_FORCE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint(15) + r) / (r + 1);
			if (r_ptr->flags4 & (RF4_BR_WALL))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_WALL))) note = " resists force.";
					l_ptr->flags4 |= (RF4_BR_WALL);
				}
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_INER))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_INER))) note = " resists inertia.";
					l_ptr->flags4 |= (RF4_BR_INER);
				}
			}
			break;
		}

		/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_TIME))
			{
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_TIME))) note = " resists time.";
					l_ptr->flags4 |= (RF4_BR_TIME);
				}
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			if (seen) obvious = TRUE;
			do_dist = 10;
			if (r_ptr->flags4 & (RF4_BR_GRAV))
			{
				dam *= 3; dam /= (randint(6)+6);
				do_dist = 0;
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_GRAV))) note = " resists gravity.";
					l_ptr->flags4 |= (RF4_BR_GRAV);
				}
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
			do_stun = (randint(15) + 1) / (r + 1);
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_COLD))) note = " resists ice.";
					l_ptr->flags3 |= (RF3_IM_COLD);
				}
			}
			break;
		}


		/* Drain Life */
		case GF_OLD_DRAIN:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_UNDEAD))) note = " is unaffected.";
					l_ptr->flags3 |= (RF3_UNDEAD);
				}

				obvious = FALSE;
				dam=0;
			}
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_DEMON))) note = " is unaffected.";
					l_ptr->flags3 |= (RF3_DEMON);
				}

				obvious = FALSE;
				dam = 0;
			}
			if (r_ptr->flags3 & (RF3_NONLIVING))
			{
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_NONLIVING))) note = " is unaffected.";
					l_ptr->flags3 |= (RF3_NONLIVING);
				}

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

			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (rand_int(r_ptr->level+10) > dam))
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
			if (seen) obvious = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;

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
		case GF_OLD_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Wake up */
			m_ptr->csleep = 0;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			note = " looks healthier.";

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Speed Monster (Ignore "dam") */
		case GF_OLD_SPEED:
		{
			if (seen) obvious = TRUE;

			/* Speed up */
			if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;
			note = " starts moving faster.";

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Slow Monster (Use "dam" as "power") */
		case GF_OLD_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Powerful monsters can resist */
			if (rand_int(r_ptr->level+10) > dam)
			{
				note = " is unaffected!";
				obvious = FALSE;
			}

			/* Normal monsters slow down */
			else
			{
				if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
				note = " starts moving slower.";
			}

			/* No "real" damage */
			dam = 0;
			break;
		}



		/* Sleep (Use "dam" as "power") */
		case GF_OLD_SLEEP:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NO_SLEEP)) ||
			    (rand_int(r_ptr->level+10) > dam))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen)
					{
						if (!(l_ptr->flags3 & (RF3_NO_SLEEP))) note = " cannot be charmed or slept.";
						l_ptr->flags3 |= (RF3_NO_SLEEP);
					}
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
		case GF_OLD_CONF:
		{
			if (seen) obvious = TRUE;

			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NO_CONF)) ||
			    (rand_int(r_ptr->level+10) > dam))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen)
					{
						if (!(l_ptr->flags3 & (RF3_NO_CONF))) note = " cannot be confused.";
						l_ptr->flags3 |= (RF3_NO_CONF);
					}
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
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_HURT_LITE))) note = " cringes from the light!";
					l_ptr->flags3 |= (RF3_HURT_LITE);
				}

				/* Special effect */
				note_dies = " shrivels away in the light!";
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
			if (r_ptr->flags4 & (RF4_BR_LITE))
			{
				dam *= 2; dam /= (randint(6)+6);

				/* Memorize the effects */
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_LITE))) note = " resists.";
					l_ptr->flags4 |= (RF4_BR_LITE);
				}

			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Memorize the effects */
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_HURT_LITE))) note = " cringes from the light!";
					l_ptr->flags3 |= (RF3_HURT_LITE);
				}

				note_dies = " shrivels away in the light!";
				dam *= 2;
			}
			break;
		}


		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_DARK))
			{
				dam *= 2; dam /= (randint(6)+6);

				/* Memorize the effects */
				if (seen)
				{
					if (!(l_ptr->flags4 & (RF4_BR_DARK))) note = " resists.";
					l_ptr->flags4 |= (RF4_BR_DARK);
				}

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
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_HURT_ROCK))) note = " loses some skin!";
					l_ptr->flags3 |= (RF3_HURT_ROCK);
				}

				/* Cute little message */
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
				if (seen) obvious = TRUE;
				if (seen) l_ptr->flags3 |= (RF3_UNDEAD);
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
				if (seen) l_ptr->flags3 |= (RF3_EVIL);
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
				if (seen) l_ptr->flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (rand_int(r_ptr->level+10) > dam)
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
				if (seen) l_ptr->flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (rand_int(r_ptr->level+10) > dam)
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
			if (seen) obvious = TRUE;

			/* Apply some fear */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NO_FEAR)) ||
			    (rand_int(r_ptr->level+10) > dam))
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
				{
					if (!(l_ptr->flags3 & (RF3_UNDEAD))) note = " shudders.";
					l_ptr->flags3 |= (RF3_UNDEAD);

					obvious = TRUE;
				}

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
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
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_UNDEAD))) note = " shudders.";
					l_ptr->flags3 |= (RF3_UNDEAD);

					obvious = TRUE;
				}

				/* Message */
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
			if (seen) obvious = TRUE;

 			/* Message */
			note_dies = " dissolves!";

			break;
		}


		/* Melee attack - blind */
		case GF_BLIND:
		{
			if (seen) obvious = TRUE;

			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Hurt eyes alot */
			if (r_ptr->d_char == 'e')
			{
				dam *= 2;
				note = " is badly hurt.";
			}

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
			    (rand_int(r_ptr->level+10) > dam))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NONLIVING))
				{
					if (seen) l_ptr->flags3 |= (RF3_NONLIVING);
				}

				/* Resist */
				do_conf = 0;

				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}

		}

		/* Melee attack - fear */
		case GF_TERRIFY:
		{
			if (seen) obvious = TRUE;
			do_fear = (10 + randint(15) + r) / (r + 1);
			if (r_ptr->flags3 & (RF3_NO_FEAR))
			{
				dam = 0;
				do_fear = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_NO_FEAR))) note = " is immune to fear.";
					l_ptr->flags3 |= (RF3_NO_FEAR);
				}
			}
			else if (r_ptr->flags4 & (RF4_BR_FEAR))
			{
				dam *= 2; dam /= (randint(6)+6);
				do_fear = 0;
				if (seen)
				{
					if (seen)
					{
						if (!(l_ptr->flags3 & (RF3_NO_FEAR))) note = " resists fear.";
						l_ptr->flags3 |= (RF3_NO_FEAR);
					}
					l_ptr->flags4 |= (RF4_BR_FEAR);
				}
			}
			break;
		}

		/* Melee attack - paralyze */
		case GF_PARALYZE:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NO_SLEEP)) ||
			    (rand_int(r_ptr->level+10) > dam))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen)
					{
						if (!(l_ptr->flags3 & (RF3_NO_SLEEP))) note = " cannot be charmed or slept.";
						l_ptr->flags3 |= (RF3_NO_SLEEP);
					}
				}

				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
				note = " is paralyzed!";
				do_sleep = 500;
			}

			break;
		}


		/* Melee attack - slow */
		case GF_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags3 & (RF3_NO_SLEEP)) ||
			   (rand_int(r_ptr->level+10) > dam))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen)
					{
						if (!(l_ptr->flags3 & (RF3_NO_SLEEP))) note = " somewhat resists confusion.";
						l_ptr->flags3 |= (RF3_NO_SLEEP);
					}
				}

				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			}
			else
			{
				if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
				note = " starts moving slower.";
			}

			break;
		}

		/* Pits */
		case GF_FALL:
		case GF_FALL_SPIKE:
		case GF_FALL_POIS:
		{
			if (seen) obvious = TRUE;

			note = " falls into a pit.";
			break;
		}

		/* Trapdoor */
		case GF_FALL_MORE:
		{
			if (seen) obvious = TRUE;

			/* Hack -- no chasm/trap doors/down stairs on quest levels */
			if (is_quest(p_ptr->depth) ||  (p_ptr->depth == max_depth(p_ptr->dungeon)))
			{
				note = "falls into a chasm.";

				/* Hack -- prevent 'weird' messages */
				if (dam == 0) obvious = FALSE;

				/* Should probably make the monster fall back
				 * into the chasm.
				 */
			}
			else
			{
				note = " falls from sight.";

				do_more = TRUE;
			}

			break;
		}

		/* Melee attack - hurt */
		case GF_BATTER:
		case GF_WOUND:
		case GF_HURT:

			/* Hack -- Monster armor reduces total damage */
			if (variant_scale_dam) dam -= (dam * ((r_ptr->ac < 150) ? r_ptr->ac : 150) / 250);


		/* Melee attack - unbonus */
		case GF_UN_BONUS:

		/* Melee attack - unpower */
		case GF_UN_POWER:

		/* Melee attack - eat gold */
		case GF_EAT_GOLD:

		/* Melee attack - eat item */
		case GF_EAT_ITEM:

		/* Melee attack - eat food */
		case GF_EAT_FOOD:

		/* Melee attack - eat lite */
		case GF_EAT_LITE:

		/* Melee attack - lose strength */
		case GF_LOSE_STR:

		/* Melee attack - lose int */
		case GF_LOSE_INT:

		/* Melee attack - lose wisdom */
		case GF_LOSE_WIS:

		/* Melee attack - lose dex */
		case GF_LOSE_DEX:

		/* Melee attack - lose con */
		case GF_LOSE_CON:

		/* Melee attack - lose cha */
		case GF_LOSE_CHR:

		/* Melee attack - lose all */
		case GF_LOSE_ALL:

		/* Melee attack - shatter */
		case GF_SHATTER:

		/* Melee attack - exp 10 */
		case GF_EXP_10:

		/* Melee attack - exp 20 */
		case GF_EXP_20:

		/* Melee attack - exp 40 */
		case GF_EXP_40:

		/* Melee attack - exp 80 */
		case GF_EXP_80:

		/* Melee attack - lose mana */
		case GF_LOSE_MANA:

		/* Melee attack - hunger */
		case GF_HUNGER:

		/* Melee attack - hunger */
		case GF_DISEASE:
		{
			if (seen) obvious = TRUE;

			/* All do damage */
			break;
		}

		/* Probe visible monsters */
		case GF_PROBE:
		{
			dam = 0;

			if (m_ptr->ml)
			{
			char m_name[80];

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(cave_m_idx[y][x]);

			/* Probe worked */
			obvious = TRUE;
			}

			break;
		}

		/* Co-exist with a feature */
		case GF_FEATURE:
		{
			if (seen) obvious = TRUE;

			if (!place_monster_here(y,x,m_ptr->r_idx))
			{
				/* Assume not safe */
				int sn = 0;
				int sy = y;
				int sx = x;
				int i;

				/* Monster can move to escape the wall */
				if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
				{
					/* Look for safety */
					for (i = 0; i < 8; i++)
					{
						/* Get the grid */
						int yy = y + ddy_ddd[i];
						int xx = x + ddx_ddd[i];

						/* Skip non-empty grids */
						if (!cave_empty_bold(yy, xx)) continue;

						/* Hack -- no safety on glyph of warding */
						if (f_info[cave_feat[yy][xx]].flags1 & (FF1_GLYPH)) continue;

						/* Count "safe" grids, apply the randomizer */
						if ((++sn > 1) && (rand_int(sn) != 0)) continue;

						/* Save the safe grid */
						sy = yy;
						sx = xx;
					}

					/* Hack -- 'somewhat' avoidable*/
					if (f_info[cave_feat[y][x]].flags3 & (FF3_EASY_CLIMB)) sn++;

				}

				/* Hack -- 'easily' avoidable*/
				if (f_info[cave_feat[y][x]].flags1 & (FF1_MOVE)) sn++;

				/* Take damage from the quake */
				dam = (sn ? damroll(2,8) : damroll(6, 8));

				/* Delete (not kill) "dead" monsters */
				if (m_ptr->hp < dam)
				{
					/* "Kill" the monster */
					delete_monster_idx(cave_m_idx[y][x]);

					/* Paranoia --- Handle rest of monster routine here */

					/* Redraw the monster grid */
					lite_spot(y, x);

					return(obvious);
				}

				/* Hack -- Escape from the rock */
				if (sn)
				{
					/* Move the monster */
					monster_swap(y, x, sy, sx);

					/* Hack -- get new location */
					y = m_ptr->fy;
					x = m_ptr->fx;					
				}
			}

			break;
		}

		/* Steam -- weak boiling water*/
		case GF_STEAM:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " is immune to fire.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
			}
			if (r_ptr->flags3 & (RF3_IM_WATER))
			{
				dam = 0;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_WATER))) note = " is immune to water.";
					l_ptr->flags3 |= (RF3_IM_WATER);
				}
			}
			break;
		}
		/* Vapour -- weak acid */
		case GF_VAPOUR:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_ACID))) note = " is immune to acid.";
					l_ptr->flags3 |= (RF3_IM_ACID);
				}
			}
			break;
		}
		/* Smoke -- weak fire */
		case GF_SMOKE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				dam /= 9;
				if (seen)
				{
					if (!(l_ptr->flags3 & (RF3_IM_FIRE))) note = " is immune to fire.";
					l_ptr->flags3 |= (RF3_IM_FIRE);
				}
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


	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE)) do_poly = FALSE;


	/* "Unique" monsters can only be "killed" by the player */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Uniques may only be killed by the player */
		if ((who >= 0) && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}


	/* Check for death */
	if (dam > m_ptr->hp)
	{
		/* Extract method of death */
		note = note_dies;
	}

	/* Mega-Hack -- Handle "trapdoor"  */
	else if (do_more)
	{
			/* Turn off the damage */
			dam = 0;

			/* "Kill" the monster */
			delete_monster_idx(cave_m_idx[y][x]);

			/* Paranoia --- Handle rest of monster routine here */

			/* Give detailed messages if destroyed */
			if (note) msg_format("%^s%s", m_name, note);

			/* Redraw the monster grid */
			lite_spot(y, x);

			/* Hack --- no monster left */
			return(obvious);
	}
	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint(90) > r_ptr->level))
	{
		/* Default -- assume no polymorph */
		note = " is unaffected!";

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
			delete_monster_idx(cave_m_idx[y][x]);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE);

			/* Hack -- Assume success XXX XXX XXX */

			/* Hack -- Get new monster */
			m_ptr = &m_list[cave_m_idx[y][x]];

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
		teleport_away(cave_m_idx[y][x], do_dist);

		/* Hack -- get new location */
		y = m_ptr->fy;
		x = m_ptr->fx;
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
	if (who >= 0)
	{
		/* Redraw (later) if needed */
		if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up */
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
			if (note) msg_format("%^s%s", m_name, note);
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

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

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(cave_m_idx[y][x], dam, &fear, note_dies))
		{
			/* Dead monster */
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(cave_m_idx[y][x], dam);

			/* Take note */
			if ((fear || do_fear) && (m_ptr->ml))
			{
				/* Message */
				message_format(MSG_FLEE, m_ptr->r_idx,
					       "%^s flees in terror!", m_name);
			}

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
bool project_p(int who, int r, int y, int x, int dam, int typ)
{
	int k = 0;
	int i,j;

	s32b gold;


	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster */
	monster_type *m_ptr=NULL;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;

	/* Target object*/
	object_type *o_ptr;

	/* Object name (for drain) */
	char o_name[80];

	/* No player here */
	if (!(cave_m_idx[y][x] < 0)) return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who) return (FALSE);


	/* Limit maximum damage XXX XXX XXX */
	if (dam > 1600) dam = 1600;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;

	if (who > 0)
	{
		/* Get the source monster */
		m_ptr = &m_list[who];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, m_ptr, 0x88);
	}
	else if (who == 0)
	{
		feature_type *f_ptr = &f_info[cave_feat[y][x]];

		/* Get the feature description */
		strcpy(killer,f_name + f_ptr->name);

	}
	else
	{
		strcpy(killer, "yourself");
	}

	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{

			/* Hack -- halve acid damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;

			msg_print ("You are covered in acid!");
			acid_dam(dam, killer, TRUE);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{

			/* Hack -- halve fire damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam /= 2;

			msg_print ("You are enveloped in flames!");
			fire_dam(dam, killer, TRUE);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{

			/* Hack -- double cold damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;

			msg_print ("You are covered in frost!");
			cold_dam(dam, killer, TRUE);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{

			/* Hack -- double electricy damage in water */
			if (f_info[cave_feat[y][x]].flags2 & (FF2_WATER)) dam *= 2;

			msg_print("You are struck by electricity!");
			elec_dam(dam, killer, TRUE);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			if (fuzzy) msg_print("You are hit by poison!");
			if (p_ptr->resist_pois)
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_POIS,0x0L);

				dam = (dam + 2) / 3;
			}

			if (p_ptr->oppose_pois) dam = (dam + 2) / 3;
			take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				/* Always notice */
				if(!(p_ptr->resist_pois)) equip_not_flags(0x0L,TR2_RES_POIS,0x0L);

				(void)set_poisoned(p_ptr->poisoned + rand_int(dam) + 10);
			}
			else if (p_ptr->resist_pois)
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_POIS,0x0L);
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

		/* Standard damage */
		case GF_EXPLODE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}
		/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_ORB:
		{
			if (fuzzy) msg_print("You are hit by something!");
			dam /= 2;
			take_hit(dam, killer);
			break;
		}

		/* Arrow -- no dodging XXX */
		/* Shields have a % chance equal to total ac of stopping attack */
		case GF_ARROW:
		{
			object_type *i_ptr = &inventory[INVEN_ARM];

			if ((i_ptr->k_idx) && (i_ptr->tval == TV_SHIELD) && (rand_int(100) < i_ptr->ac + i_ptr->to_a))
			{
				msg_print("Your shield stops an arrow.");
				dam = 0; 
			}
			else
			{
				if (fuzzy) msg_print("You are hit by something sharp!");
				take_hit(dam, killer);
			}
			break;
		}

		/* Plasma -- No resist XXX */
		case GF_PLASMA:
		{
			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, killer);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));

				/* Sometimes notice */
				if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + k);
			}
			else
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_nethr)
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_NETHR,0x0L);

				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_NETHR,0x0L);

				if ((p_ptr->hold_life) && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");

					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);

					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp/100) * MON_DRAIN_LIFE);

					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Water -- stun/confuse/wet */
		case GF_WATER:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(40));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + randint(5) + 5);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
			}

			water_dam(dam, killer, TRUE);

			break;
		}


		/* Weak water -- wet only */
		case GF_SALT_WATER:
		case GF_WATER_WEAK:
		{
			if (fuzzy) msg_print("You are hit by something!");
			water_dam(0, killer, TRUE);

			break;
		}


		/* Chaos -- many effects */
		case GF_CHAOS:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
			}
			if (!p_ptr->resist_chaos)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CHAOS,0x0L);

				(void)set_image(p_ptr->image + randint(10));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CHAOS,0x0L);
			}

			if (!p_ptr->resist_nethr && !p_ptr->resist_chaos)
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_NETHR,0x0L);

				if ((p_ptr->hold_life) && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");

					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(500 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);

					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(5000 + (p_ptr->exp/100) * MON_DRAIN_LIFE);

					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);
				}
			}
			else
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_NETHR,0x0L);
			}

			take_hit(dam, killer);
			break;
		}

		/* Shards -- mostly cutting */
		case GF_SHARD:
		{
			if (fuzzy) msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SHARD,0x0L);

				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SHARD,0x0L);

				(void)set_cut(p_ptr->cut + dam);
			}
			take_hit(dam, killer);
			break;
		}

		/* Sound -- mostly stunning */
		case GF_SOUND:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_sound)
			{
				/* Always notice */
				if (!fuzzy) equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);

				dam *= 5; dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));

				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SHARD,0x0L);

				(void)set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer);
			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_confu)
			{
				/* Always notice */
				if (!fuzzy) equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);

				dam *= 5; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + randint(20) + 10);
			}

			take_hit(dam, killer);
			break;
		}

		case GF_HALLU:
		{
			if (fuzzy) msg_print("You are hit by something!");

			/* Take damage */
			take_hit(dam, killer);

			/* Increase "image" */
			if (!p_ptr->resist_chaos)
			{
				if (set_image(p_ptr->image + 6 + randint(dam / 2)))
				{
					obvious = TRUE;
				}

				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CHAOS,0x0L);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CHAOS,0x0L);
			}
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_disen)
			{
				/* Always notice */
				if ((!fuzzy)&&(rand_int(100)<dam)) equip_can_flags(0x0L,TR2_RES_DISEN,0x0L);

				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_not_flags(0x0L,TR2_RES_DISEN,0x0L);

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
				/* Always notice */
				if (!fuzzy) equip_can_flags(0x0L,TR2_RES_NEXUS,0x0L);

				dam *= 6; dam /= (randint(6) + 6);
			}
			else if (who > 0)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_NEXUS,0x0L);

				apply_nexus(m_ptr);
			}
			take_hit(dam, killer);
			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				if (!fuzzy) equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(20));
			}
			take_hit(dam, killer);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			take_hit(dam, killer);
			break;
		}

		/* Lite -- blinding */
		case GF_LITE:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_lite)
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_LITE,0x0L);

				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				/* Always notice */
				equip_not_flags(0x0L,(TR2_RES_BLIND|TR2_RES_LITE),0x0L);

				(void)set_blind(p_ptr->blind + randint(5) + 2);
			}
			take_hit(dam, killer);
			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			if (fuzzy) msg_print("You are hit by something!");
			if (p_ptr->resist_dark)
			{
				/* Sometimes notice */
				if (rand_int(100)<dam) equip_can_flags(0x0L,TR2_RES_DARK,0x0L);

				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				/* Always notice */
				equip_not_flags(0x0L,(TR2_RES_BLIND|TR2_RES_DARK),0x0L);

				(void)set_blind(p_ptr->blind + randint(5) + 2);
			}
			take_hit(dam, killer);
			break;
		}

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
			if (fuzzy) msg_print("You are hit by something strange!");

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
						case 6: k = A_CHR; act = "beautiful"; break;
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
			if (fuzzy) msg_print("You are hit by something strange!");
			msg_print("Gravity warps around you.");
			teleport_player(5);
			(void)set_slow(p_ptr->slow + rand_int(4) + 4);
			if (!p_ptr->resist_sound)
			{
				k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		{
			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
			if (fuzzy) msg_print("You are hit by something sharp!");

			cold_dam(dam, killer, TRUE);

			if (!p_ptr->resist_shard)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SHARD,0x0L);

				(void)set_cut(p_ptr->cut + damroll(5, 8));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SHARD,0x0L);

			}
			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(15));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			break;
		}

		case GF_BATTER:
		case GF_WOUND:
		case GF_HURT:
		{
			/* Obvious */
			obvious = TRUE;

			/* Hack -- Player armor reduces total damage */
			dam -= (dam * ((p_ptr->ac < 150) ? p_ptr->ac : 150) / 250);

			/* Take damage */
			take_hit(dam, killer);

			break;
		}

		case GF_UN_BONUS:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Allow complete resist */
			if (!p_ptr->resist_disen)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_DISEN,0x0L);

				/* Apply disenchantment */
				if (apply_disenchant(0)) obvious = TRUE;
			}
			else
			{
				/* Sometimes notice */
				if (rand_int(100)<30) equip_can_flags(0x0L,TR2_RES_DISEN,0x0L);
			}

			break;
		}

		case GF_UN_POWER:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Find an item */
			for (k = 0; k < 10; k++)
			{
				/* Pick an item */
				i = rand_int(INVEN_PACK);

				/* Obtain the item */
				o_ptr = &inventory[i];

				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Drain charged wands/staffs */
				if (((o_ptr->tval == TV_STAFF) ||
				     (o_ptr->tval == TV_WAND)) &&
				    (o_ptr->pval))
				{
					/* Message */
					msg_print("Energy drains from your pack!");

					/* Obvious */
					obvious = TRUE;

					if (who > 0)
					{
						/* Heal */
						j = dam/10;
						m_ptr->hp += j * o_ptr->pval * o_ptr->number;
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);
					}

					/* Uncharge */
					o_ptr->pval = 0;

					/* Combine / Reorder the pack */
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);

					/* Window stuff */
					p_ptr->window |= (PW_INVEN);

					/* Done */
					break;
				}
			}

			break;
		}

		case GF_EAT_GOLD:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Obvious */
			obvious = TRUE;

			/* Saving throw (unless paralyzed) based on dex and level */
			if (!p_ptr->paralyzed &&
			    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					      p_ptr->lev)))
			{
				/* Saving throw message */
				msg_print("You quickly protect your money pouch!");

				/* Occasional blink anyway */
				/*if (rand_int(3)) blinked = TRUE;*/
			}

			/* Eat gold */
			else
			{
				gold = (p_ptr->au / 10) + randint(25);
				if (gold < 2) gold = 2;
				if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
				if (gold > p_ptr->au) gold = p_ptr->au;
				p_ptr->au -= gold;
				if (gold <= 0)
				{
					msg_print("Nothing was stolen.");
				}
				else if (p_ptr->au)
				{
					msg_print("Your purse feels lighter.");
					msg_format("%ld coins were stolen!", (long)gold);
				}
				else
				{
					msg_print("Your purse feels lighter.");
					msg_print("All of your coins were stolen!");
				}

				/* Redraw gold */
				p_ptr->redraw |= (PR_GOLD);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

				/* Run away */
				if (who > 0) m_ptr->monfear = 100;
			}

			break;
		}

		case GF_EAT_ITEM:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Saving throw (unless paralyzed) based on dex and level */
			if (!p_ptr->paralyzed &&
			    (rand_int(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					      p_ptr->lev)))
			{
				/* Saving throw message */
				msg_print("You grab hold of your backpack!");

				/* Occasional "blink" anyway */
				/* blinked = TRUE;*/

				/* Obvious */
				obvious = TRUE;

				/* Done */
				break;
			}

			/* Find an item */
			for (k = 0; k < 10; k++)
			{
				object_type *i_ptr;
				object_type object_type_body;

				u32b f1;
				u32b f2;
				u32b f3;

				/* Pick an item */
				i = rand_int(INVEN_PACK);

				/* Obtain the item */
				o_ptr = &inventory[i];

				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Clear the flags */
				f1 = f2 = f3 = 0x0L;

				/* Get the flags */
				object_flags(o_ptr,&f1,&f2,&f3);

				/* Sometimes notice theft-protection */
				if ((rand_int(100)<10) && (f2 & (TR2_IGNORE_THEFT))) object_can_flags(o_ptr,0x0L,TR2_IGNORE_THEFT,0x0L);

				/* Skip artifacts */
				if (f2 & (TR2_IGNORE_THEFT)) continue;

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

				/* Message */
				msg_format("%sour %s (%c) was stolen!",
					   ((o_ptr->number > 1) ? "One of y" : "Y"),
					   o_name, index_to_label(i));

				/* Get local object */
				i_ptr = &object_type_body;

				/* Obtain local object */
				object_copy(i_ptr, o_ptr);

				/* Modify number */
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

				/* Forget about it */
				drop_may_flags(i_ptr);

				/* Carry the object */
				if (who > 0)
				{
					(void)monster_carry(who, i_ptr);
				}
				else
				{
					/* Hack --- 20% chance of lost forever */
					drop_near(i_ptr,20,p_ptr->py,p_ptr->px);
				}

				/* Steal the items */
				inven_item_increase(i, -1);
				inven_item_optimize(i);

				/* Obvious */
				obvious = TRUE;

				/* Run away */
				if (who > 0) m_ptr->monfear = 100;

				/* Done */
				break;
			}

			break;
		}

		case GF_EAT_FOOD:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Steal some food */
			for (k = 0; k < 10; k++)
			{
				/* Pick an item from the pack */
				i = rand_int(INVEN_PACK);

				/* Get the item */
				o_ptr = &inventory[i];

				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Skip non-food objects */
				if (o_ptr->tval != TV_FOOD) continue;

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				/* Message */
				msg_format("%sour %s (%c) was eaten!",
					   ((o_ptr->number > 1) ? "One of y" : "Y"),
					   o_name, index_to_label(i));

				/* Steal the items */
				inven_item_increase(i, -1);
				inven_item_optimize(i);

				/* Obvious */
				obvious = TRUE;

				/* Done */
				break;
			}

			break;
		}

		case GF_EAT_LITE:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Get the lite */
			o_ptr = &inventory[INVEN_LITE];

			/* Drain fuel */
			if ((o_ptr->pval > 0) && (!artifact_p(o_ptr)))
			{
				/* Reduce fuel */
				o_ptr->pval -= (250 + randint(250));
				if (o_ptr->pval < 1) o_ptr->pval = 1;

				/* Notice */
				if (!p_ptr->blind)
				{
					msg_print("Your light dims.");
					obvious = TRUE;
				}

				/* Window stuff */
				p_ptr->window |= (PW_EQUIP);
			}
			else if (artifact_p(o_ptr))
			{
				if (rand_int(100)<30) object_can_flags(o_ptr,0x0L,0x0L,TR3_INSTA_ART);
			}
			break;
		}

		case GF_BLIND:
		{
			/* Take damage */
			take_hit(dam, killer);

			msg_print("Your eyes begin to sting.");

			/* Increase "blind" */
			if (!p_ptr->resist_blind)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);

				if (set_blind(p_ptr->blind + 10 + randint(dam)))
				{
					obvious = TRUE;
				}
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
			}

			if (who > 0)
			{
				/* Learn about the player */
				update_smart_learn(who, DRS_RES_BLIND);
			}

			break;
		}


		case GF_TERRIFY:
		{

			/* Apply resistance */
			if (p_ptr->resist_fear)
			{
				/* Sometimes notice */
				if((p_ptr->resist_fear)&&(rand_int(100)<dam))equip_can_flags(0x0L,TR2_RES_FEAR,0x0L);

				dam *= 5; dam /= (randint(6) + 6);
			}

			/* Take damage */
			take_hit(dam, killer);

			/* Increase "afraid" */
			if (p_ptr->resist_fear)
			{
				/* Sometimes notice */
				if ((p_ptr->resist_fear) &&(rand_int(100)<30)) equip_can_flags(0x0L,TR2_RES_FEAR,0x0L);

				msg_print("You stand your ground!");
				obvious = TRUE;
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You stand your ground!");
				obvious = TRUE;
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_FEAR,0x0L);

				if (set_afraid(p_ptr->afraid + 3 + randint(dam)))
				{
					obvious = TRUE;
				}
			}

			if (who > 0)
			{
				/* Learn about the player */
				update_smart_learn(who, DRS_RES_FEAR);
			}

			break;
		}

		case GF_PARALYZE:
		{
			/* Hack -- Prevent perma-paralysis via damage */
			if (p_ptr->paralyzed && (dam < 1)) dam = 1;

			/* Take damage */
			take_hit(dam, killer);

			/* Increase "paralyzed" */
			if (p_ptr->free_act)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);

				msg_print("You are unaffected!");
				obvious = TRUE;
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
				obvious = TRUE;
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);

				if (set_paralyzed(p_ptr->paralyzed + 3 + randint(dam)))
				{
					obvious = TRUE;
				}
			}

			if (who > 0)
			{
				/* Learn about the player */
				update_smart_learn(who, DRS_FREE);
			}

			break;
		}

		case GF_SLOW:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Increase "paralyzed" */
			if (p_ptr->free_act)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);

				msg_print("You are unaffected!");
				obvious = TRUE;
			}
			else if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
				obvious = TRUE;
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);

				if (set_slow(p_ptr->slow + randint(25) + 15)) obvious = TRUE;
				{
					obvious = TRUE;
				}
			}
			if (who > 0)
			{
				/* Learn about the player */
				update_smart_learn(who, DRS_FREE);
			}
			break;
		}

		case GF_LOSE_STR:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_STR)) obvious = TRUE;

			break;
		}

		case GF_LOSE_INT:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_INT)) obvious = TRUE;

			break;
		}

		case GF_LOSE_WIS:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_WIS)) obvious = TRUE;

			break;
		}

		case GF_LOSE_DEX:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_DEX)) obvious = TRUE;

			break;
		}

		case GF_LOSE_CON:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_CON)) obvious = TRUE;

			break;
		}

		case GF_LOSE_CHR:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stat) */
			if (do_dec_stat(A_CHR)) obvious = TRUE;

			break;
		}

		case GF_LOSE_ALL:
		{
			/* Take damage */
			take_hit(dam, killer);

			/* Damage (stats) */
			if (do_dec_stat(A_STR)) obvious = TRUE;
			if (do_dec_stat(A_DEX)) obvious = TRUE;
			if (do_dec_stat(A_CON)) obvious = TRUE;
			if (do_dec_stat(A_INT)) obvious = TRUE;
			if (do_dec_stat(A_WIS)) obvious = TRUE;
			if (do_dec_stat(A_CHR)) obvious = TRUE;

			break;
		}

		case GF_SHATTER:
		{
			/* Obvious */
			obvious = TRUE;

			/* Hack -- Reduce damage based on the player armor class */
			dam -= (dam * ((p_ptr->ac < 150) ? p_ptr->ac : 150) / 250);

			/* Take damage */
			take_hit(dam, killer);

			if (who > 0)
			{
				/* Radius 8 earthquake centered at the monster */
				if (dam > 23) earthquake(m_ptr->fy, m_ptr->fx, 8);
			}

			else
			{
				/* Radius 8 earthquake centered at the player */
				if (dam > 23) earthquake(p_ptr->py, p_ptr->px, 8);

			}

			break;
		}

		case GF_EXP_10:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(dam, killer);

			if ((p_ptr->hold_life)  && (rand_int(100) < 95))
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

				if (p_ptr->hold_life) msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
				if (p_ptr->hold_life)
				{
					/* Always notice */
					equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					if (p_ptr->hold_life) msg_print("You feel your life slipping away!");
					lose_exp(d/10);
				}
				else
				{
					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case GF_EXP_20:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(dam, killer);

			if ((p_ptr->hold_life) && (rand_int(100) < 90))
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

				if (p_ptr->hold_life) msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = damroll(20, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
				if (p_ptr->hold_life) 
				{
					msg_print("You feel your life slipping away!");

					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					lose_exp(d/10);
				}
				else
				{
					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case GF_EXP_40:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(dam, killer);

			if ((p_ptr->hold_life) && (rand_int(100) < 75))
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

				if (p_ptr->hold_life) msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = damroll(40, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
				if (p_ptr->hold_life) 
				{
					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life slipping away!");
					lose_exp(d/10);
				}
				else
				{
					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case GF_EXP_80:
		{
			/* Obvious */
			obvious = TRUE;

			/* Take damage */
			take_hit(dam, killer);

			if ((p_ptr->hold_life) && (rand_int(100) < 50))
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

				if (p_ptr->hold_life) msg_print("You keep hold of your life force!");
			}
			else
			{
				s32b d = damroll(80, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
				if (p_ptr->hold_life) 
				{
					/* Always notice */
					if (p_ptr->hold_life) equip_can_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life slipping away!");
					lose_exp(d/10);
				}
				else
				{
					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_HOLD_LIFE);

					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			break;
		}

		case GF_FALL:
		{

			if (p_ptr->ffall)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FEATHER);

				msg_print("You float gently down to the bottom of the pit.");
				dam=0;
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FEATHER);

				take_hit(dam, killer);
			}

			break;
		}

		case GF_FALL_MORE:
		{
			if (p_ptr->ffall)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FEATHER);

				msg_print("You float gently down to the next level.");
				dam = 0;
			}
			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FEATHER);

				take_hit(dam, killer);
			}

			/* Hack -- tower level decreases depth */
			if (t_info[p_ptr->dungeon].zone[0].tower)
			{
				/* New depth */
				p_ptr->depth--;

				/* Leaving */
				p_ptr->leaving = TRUE;

			}
			/* Hack -- no chasm/trap doors/down stairs on quest levels */
			else if (is_quest(p_ptr->depth) || (p_ptr->depth == max_depth(p_ptr->dungeon)))
			{
				int i = rand_int(8);

				int k = 0;

				/* Scan all neighbors */
				while (cave_feat[y][x] != FEAT_CHASM)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Hack -- bounds check */
					if (++k>30) break;

					i = rand_int(8);

					if (cave_feat[yy][xx] != FEAT_CHASM) continue;

					/* Hack -- fall back into the chasm */
					monster_swap(p_ptr->py,p_ptr->px,yy,xx);

					break;
				}

			}
			else
			{
				/* New depth */
				p_ptr->depth++;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			break;
		}

		case GF_FALL_SPIKE:
		{
			if (p_ptr->ffall)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FEATHER);

				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FEATHER);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Take the damage */
				take_hit(dam, killer);
			}
			break;
		}

		case GF_FALL_POIS:
		{
			if (p_ptr->ffall)
			{
				/* Always notice */
				equip_can_flags(0x0L,0x0L,TR3_FEATHER);

				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Always notice */
				equip_not_flags(0x0L,0x0L,TR3_FEATHER);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					dam = dam * 2;
					(void)set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");

						/* Always notice */
						if(p_ptr->resist_pois) equip_can_flags(0x0L,TR2_RES_POIS,0x0L);
					}

					else
					{
						dam = dam * 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));

						/* Always notice */
							if(!(p_ptr->resist_pois)) equip_not_flags(0x0L,TR2_RES_POIS,0x0L);
					}
				}

				/* Take the damage */
				take_hit(dam, killer);
			}

			break;
		}

		/* Lava -- stun/confuse/fire */
		case GF_LAVA:
		{

			msg_print ("You are surrounded by lava!");
			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(40));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + randint(5) + 5);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
			}

			fire_dam(dam, killer, TRUE);

			break;
		}


		case GF_BWATER: /* Fire, water damage */
		{
			msg_print("You are scalded by boiling water.");

			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(40));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + randint(5) + 5);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
			}


			water_dam((dam*2)/3,killer, TRUE);
			fire_dam(dam/3,killer, FALSE);

			break;
		}
		case GF_BMUD: /* Fire, water damage */
		{
			msg_print("You are splashed with boiling mud.");

			if (!p_ptr->resist_sound)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_SOUND,0x0L);

				(void)set_stun(p_ptr->stun + randint(40));
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_SOUND,0x0L);
			}
			if (!p_ptr->resist_confu)
			{
				/* Always notice */
				equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);

				(void)set_confused(p_ptr->confused + randint(5) + 5);
			}
			else
			{
				/* Always notice */
				equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
			}


			water_dam((dam*2)/3,killer, FALSE);
			fire_dam(dam/3,killer, FALSE);

			break;
		}

		/* Heal the player */
		case GF_OLD_HEAL:
		{
			obvious = hp_player(dam);
			dam = 0;

			break;
		}

		/* Heal the player */
		case GF_OLD_DRAIN:
		{
			obvious = TRUE;
			take_hit(dam, killer);

			break;
		}

		/* Teleport the player -- use dam as power*/
		case GF_AWAY_ALL:
		{
			(void)teleport_player(dam);
			dam = 0;	 
			break;

		}

		case GF_STEAM: /* Fire, water damage */
		{
			msg_print("You are scalded by steam.");

			water_dam((dam*2)/3,killer, FALSE);
			fire_dam(dam/3,killer, FALSE);

			break;
		}

		case GF_VAPOUR: /* Acid damage */
		{
			msg_print("You are surrounded by acidic vapour.");

			acid_dam(dam, killer, FALSE);

			break;
		}

		case GF_SMOKE: /* Acid damage */
		{
			msg_print("You are surrounded by smoke.");

			fire_dam(dam, killer, FALSE);

			break;
		}

		case GF_LOSE_MANA:
		{
			int drain;

			/* Damage (mana) */
			if (p_ptr->csp)
			{
				/* Drain depends on maximum mana */
				drain = 2 + rand_int(p_ptr->msp / 10);

				/* Drain the mana */
				if (drain > p_ptr->csp)
				{
					p_ptr->csp = 0;
					p_ptr->csp_frac = 0;

					msg_print("Your mana is gone!");
				}
				else
				{
					p_ptr->csp -= drain;
					msg_print("Your mana drains away.");
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
			break;
		}

		case GF_HUNGER:
		{
			int resist = 2;

			obvious = TRUE;

			/* Take damage */
			take_hit(dam, killer);

			/* We're not dead yet */
			if (!p_ptr->is_dead)
			{
				/* Allow resistance */
				if (rand_int(100) < p_ptr->skill_sav) resist++;

				if (p_ptr->slow_digest)
				{
					resist += 2;
					equip_can_flags(0x0L,0x0L,TR3_SLOW_DIGEST);
					dam /= 3;
				}
				else
				{
					equip_not_flags(0x0L,0x0L,TR3_SLOW_DIGEST);
				}

				/* Message -- only if appropriate */
				if ((resist > 2) &&
				    (p_ptr->food > PY_FOOD_ALERT))
				{
					msg_print("You resist the effects!");
				}
				else
				{
					msg_print("You feel hungry...");
				}

				/* Reduce food counter, but not too much. */
				set_food(p_ptr->food -
					MIN(500 + p_ptr->food / 5, p_ptr->food / resist));
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
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (negative for "player")
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
 * optionally objects) within the given radius of the final location. *
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
bool project(int who, int rad, int y, int x, int dam, int typ, int flg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, dist;

	int y1, x1;
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

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[256], gy[256];

	/* Encoded "radius" info (see above) */
	byte gm[16];


	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);
	}

	/* Start at player */
	else if (who < 0)
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

	/* Hack -- lite up source location if appropriate */
	if (!(flg & (PROJECT_HIDE)))
	{
		/* Hack -- fire/lite/plasma/lava/electricity lites source location */
		if ((typ == GF_FIRE) || (typ == GF_LITE) ||
			(typ == GF_PLASMA) || (typ == GF_LAVA) ||
			(typ == GF_ELEC))
		{
			notice |= temp_lite(y1,x1);
		}
	}


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
	for (dist = 0; dist < 16; dist++) gm[dist] = 0;


	/* Initial grid */
	y = y1;
	x = x1;

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
		if (!(f_info[cave_feat[ny][nx]].flags1 & (FF1_PROJECT)) && (rad > 0)) break;

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

	/* Explode */
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

			/* Here is no monster or is player */
			if (cave_m_idx[y][x] <= 0) continue;

			/* Don't affect hidden monsters */
			if (m_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)) continue;

			/* Affect the monster in the grid */
			if (project_m(who, dist, y, x, dam, typ)) notice = TRUE;
		}

		/* Player affected one monster (without "jumping") */
		if ((who < 0) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave_m_idx[y][x] > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);
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
			if (project_p(who, dist, y, x, dam, typ)) notice = TRUE;
		}
	}


	/* Return "something was noticed" */
	return (notice);
}



