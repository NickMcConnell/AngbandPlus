#define SPELLS1_C
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
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags1 & RF1_GUARDIAN)) return (r_idx);

	/* Allowable range of "levels" for resulting monster */
	lev1 = r_ptr->level - ((randint(20)/randint(9))+1);
	lev2 = r_ptr->level + ((randint(20)/randint(9))+1);

	/* Pick a (possibly new) non-unique race */
	for (i = 0; i < 1000; i++)
	{
		/* Pick a new race, using a level calculation */
		r = get_mon_num(((dun_depth) + r_ptr->level) / 2 + 5);

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
static void teleport_to_player(int m_idx)
{
	int                     ny, nx, oy, ox, d, i, min;
	int dis = 2;

	monster_type    *m_ptr = &m_list[m_idx];
	int attempts;


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
	for (attempts = 500; attempts; attempts--)
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
			goto ttp_done;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
	}
ttp_done:

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

	/* Save old location. */
	oy = py;
	ox = px;

	/* Move the player */
	move_to(y,x);

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
	int y, x, dis = 0, ctr = 0;

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

	/* Move the player */
	move_to(y,x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
	bool into;
	if (p_ptr->anti_tele)
	{
		msg_print("A mysterious force prevents you from teleporting!");
		return;
	}

	/* Always go into the dungeon if outside. */
	if (dun_level <= 0) into = TRUE;
	/* Don't go any further if you can't. */
	else if (is_quest(dun_level) || (dun_level >= dun_defs[cur_dungeon].max_level)) into = FALSE;
	/* Else choose randomly. */
	else into = (percent(50));

	/* Get a special message if you leave a tower altogether. */
	if (!into && dun_defs[cur_dungeon].flags & DF_TOWER && dun_level == 1)
		msg_print("You fall out of the tower!");
	/* "into" is down in dungeons, up in towers. */
	else if (into ^ !!(dun_defs[cur_dungeon].flags & DF_TOWER))
		msg_print("You sink through the floor.");
	else
		msg_print("You rise up through the ceiling.");

	if (into)
		change_level(dun_level+1, START_RANDOM);
	else
		change_level(dun_level-1, START_RANDOM);

	/* Sound */
	sound(SOUND_TPLEVEL);
}






/*
 * Return the gf_type which describes this type of attack.
 */
gf_type *lookup_gf(int type)
{
	gf_type *ptr;

	FOR_ALL_IN(gf_info, ptr)
	{
		if (ptr->idx == type) return ptr;
	}

	/* Hack - gf_info[0] contains defaults for unknown types. */
	return gf_info;
}

/*
 * Return a color to use for the bolt/ball spells in ascii mode
 */
static byte spell_color(int type)
{
	gf_type *gf_ptr = lookup_gf(type);

	return gf_ptr->colour[rand_int(gf_ptr->colour[15])];
}

/*
 * Return an attr to use for the bolt spells in graphics mode
 */
static byte bolt_graf_attr(int type)
{
	return lookup_gf(type)->bolt_graf_attr;
}
/*
 * Return an attr to use for the ball spells in graphics mode
 */
static byte ball_graf_attr(int UNUSED type)
{
	return(145);
}

/*
 * Return a char to use for the ball spells in graphics mode
 */
static byte ball_graf_char(int type)
{
	return lookup_gf(type)->ball_graf_char;
}

/*
 * Return a base char to use for the bolt spells in graphics mode
 */
static byte base_bolt_char(int type)
{
	return lookup_gf(type)->base_bolt_char;
}


/*
 * Give the player a specific number of hit points.
 * Take care of p_ptr->chp_frac.
 */
static void set_p_hp(int new_hp)
{
	p_ptr->chp = new_hp;
	p_ptr->chp_frac = 0;
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
void take_hit(int damage, cptr hit_from, int monster)
{
	int old_chp = p_ptr->chp;

	bool pen_invuln = FALSE;

	int warning = (p_ptr->mhp * hitpoint_warn / 100);


	/* Paranoia */
	if (death) return;

	/* Nothing to de. */
	if (damage <= 0) return;

	/* Disturb */
	disturb(1);

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

	/* hit_from may be a format string, so copy it across before format__buf
	 * can be contaminated. */
	if (p_ptr->chp < 0)
	{
		cptr imstr = (p_ptr->image) ? "(?)" : "";
		died_from = safe_string_make(format("%s%s", hit_from, imstr));
	}

	if (pen_invuln)
			msg_print("The attack penetrates your shield of invulnerability!");

	/* Don't allow HP to fall below the minimum, if any. */
	if (p_ptr->chp < p_ptr->min_hp) set_p_hp(p_ptr->min_hp);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Sound */
		sound(SOUND_DEATH);
		/* Don't give confusing death message if player has ritual */
		if(p_ptr->ritual == TOWN_NONE)
		{
			/* Hack -- Note death */
			if (!last_words)
			{
				msg_print("You die.");
				msg_print(NULL);
			}
			else
			{
				msg_format("%v", get_rnd_line_f1, "death.txt");
			}
		}

		/* No longer a winner (can this make winning impossible?) */
		total_winner = FALSE;

		/* Note death */
		death = TRUE;

		/* Again, don't print message if player has ritual */
		if((p_ptr->ritual == TOWN_NONE) && !cheat_live)
		{
			/* Recognise the killer's efforts, if possible. */
			monster_race *r_ptr = r_info+monster;
			if (r_ptr->r_deaths < MAX_SHORT) r_ptr->r_deaths++;

			if (get_check("Dump the screen? "))
			{
				do_cmd_save_screen();
			}
		}
		/* Dead */
		return;
	}

	/* Hack - paralysis */
	if (p_ptr->paralyzed && one_in(2))
	{
		msg_print("The pain shakes you from your torpor!");
		no_msg_print = TRUE;
		set_flag(TIMED_PARALYZED, 0);
		no_msg_print = FALSE;
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning) bell(0);

		sound(SOUND_WARN);

		/* Message */
		msg_print("$r*** LOW HITPOINT WARNING! ***");
		msg_print(NULL);
	}
	if (damage > (old_chp/20))
	{
		skill_exp(SKILL_TOUGH);
	}
}














/*
 * Melt something
 */
static int set_acid_destroy(object_type *o_ptr)
{
	u32b f1, f2, f3;
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


	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		/* Hack - skip weapons, armour, lights and accessories. */
		if (i == INVEN_PACK) i = INVEN_POUCH_1;

		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (allart_p(o_ptr)) continue;

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
				/* Message */
				msg_format("%sour %v (%c) %s destroyed!",
					((o_ptr->number > 1) ?
					((amt == o_ptr->number) ? "All of y" :
					(amt > 1 ? "Some of y" : "One of y")) : "Y"),
					object_desc_f3, o_ptr, FALSE, 3, index_to_label(o_ptr),
					((amt > 1) ? "were" : "was"));

					/* Potions smash open */
					if (k_info[o_ptr->k_idx].tval == TV_POTION) {
					(void)potion_smash_effect(0, py, px, o_ptr->k_idx);
					}


				/* Destroy "amt" items */
				item_increase(o_ptr, -amt);
				item_optimize(o_ptr);

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
static bool minus_ac(void)
{
	object_type             *o_ptr = NULL;

	u32b            f1, f2, f3;
	int feel;

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
	if (!o_ptr->k_idx) return FALSE;

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Object resists */
	if (f3 & (TR3_IGNORE_ACID))
	{
		msg_format("Your %v is unaffected!", object_desc_f3, o_ptr, FALSE, 0);
		return (TRUE);
	}

	/* Message */
	msg_format("Your %v is damaged!", object_desc_f3, o_ptr, FALSE, 0);

	/* Hack - the player doesn't know whether a good item just became an average one. */
	feel = find_feeling(o_ptr);
	if (feel == SENSE_G_OBJ || feel == SENSE_GP_OBJ)
		o_ptr->ident &= ~(IDENT_SENSE_VALUE);

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
void acid_dam(int dam, cptr kb_str, int monster)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total Immunity */
	if (p_ptr->immune_acid || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_has_mutation(MUT_VULN_ELEM)) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = (dam + 2) / 3;
	if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_acid || p_ptr->resist_acid))
		&& randint(HURT_CHANCE)==1)
			(void) do_dec_stat(A_CHR);

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str, monster);

	/* Inventory damage */
	if (!(p_ptr->oppose_acid && p_ptr->resist_acid))
		inven_damage(set_acid_destroy, inv);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, cptr kb_str, int monster)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_elec || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_has_mutation(MUT_VULN_ELEM)) dam *= 2;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
	if (p_ptr->resist_elec) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_elec || p_ptr->resist_elec))
		&& randint(HURT_CHANCE)==1)
			(void) do_dec_stat(A_DEX);

	/* Take damage */
	take_hit(dam, kb_str, monster);

	/* Inventory damage */
	if (!(p_ptr->oppose_elec && p_ptr->resist_elec))
		inven_damage(set_elec_destroy, inv);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, cptr kb_str, int monster)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Totally immune */
	if (p_ptr->immune_fire || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_has_mutation(MUT_VULN_ELEM)) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = (dam + 2) / 3;
	if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_fire || p_ptr->resist_fire))
		&& randint(HURT_CHANCE)==1)
			(void) do_dec_stat(A_STR);


	/* Take damage */
	take_hit(dam, kb_str, monster);

	/* Inventory damage */
	if (!(p_ptr->resist_fire && p_ptr->oppose_fire))
		inven_damage(set_fire_destroy, inv);
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, cptr kb_str, int monster)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* Total immunity */
	if (p_ptr->immune_cold || (dam <= 0)) return;

	/* Vulnerability (Ouch!) */
	if (p_has_mutation(MUT_VULN_ELEM)) dam *= 2;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = (dam + 2) / 3;
	if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

	if ((!(p_ptr->oppose_cold || p_ptr->resist_cold))
		&& randint(HURT_CHANCE)==1)
			(void) do_dec_stat(A_STR);


	/* Take damage */
	take_hit(dam, kb_str, monster);

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
		p_ptr->redraw |= PR_STATS;
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
	int feel, t = 0;

	object_type             *o_ptr;

	/* Unused */
	mode = mode;


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

	/* Artifacts have 71% chance to resist */
	if (allart_p(o_ptr) && (rand_int(100) < 71))
	{
		/* Message */
		msg_format("Your %v (%c) resist%s disenchantment!",
				object_desc_f3, o_ptr, FALSE, 0, index_to_label(o_ptr),
				((o_ptr->number != 1) ? "" : "s"));

		/* Notice */
		return (TRUE);
	}

	/* Hack - the player doesn't know whether a good item just became an average one. */
	feel = find_feeling(o_ptr);
	if (feel == SENSE_G_OBJ || feel == SENSE_GP_OBJ)
		o_ptr->ident &= ~(IDENT_SENSE_VALUE);

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
	msg_format("Your %v (%c) %s disenchanted!",
			object_desc_f3, o_ptr, FALSE, 0, index_to_label(o_ptr),
			((o_ptr->number != 1) ? "were" : "was"));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	/* Notice */
	return (TRUE);
}


void chaos_feature_shuffle(void)

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
				skill_exp(SKILL_SAVE);
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
				skill_exp(SKILL_SAVE);
				break;
			}

			msg_print("Your body starts to scramble...");
			chaos_feature_shuffle();
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
static bool project_f(int r, int y, int x, int dam, int typ)
{
	cave_type       *c_ptr = &cave[y][x];

	bool    obvious = FALSE;


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
				int feat = c_ptr->feat;

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
				place_gold(y, x, FOUND_DIG, feat);
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
				int feat = c_ptr->feat;

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
					place_object(y, x, FALSE, FALSE, FOUND_DIG, feat);
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

			full_grid = MAX(full_grid, distance(y,x,py,px));

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
static bool project_o(monster_type *m_ptr, int r, int y, int x, int dam, int typ)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	u32b f1, f2, f3;

	bool is_potion = FALSE;


	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr, j_ptr[1];

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;

		cptr note_kill = NULL;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the known information */
		object_info_known(j_ptr, o_ptr);

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if (allart_p(o_ptr)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
			case GF_ACID:
			{
				if (set_acid_destroy(j_ptr))
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
				if (set_elec_destroy(j_ptr))
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
				if (set_fire_destroy(j_ptr))
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
				if (set_cold_destroy(j_ptr))
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
				if (set_fire_destroy(j_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				if (set_elec_destroy(j_ptr))
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
				if (set_fire_destroy(j_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				if (set_cold_destroy(j_ptr))
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
				if (set_cold_destroy(j_ptr))
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

			/* Holy Fire and Hell Fire -- destroys cursed non-artifacts */
			case GF_HOLY_FIRE:
			case GF_HELL_FIRE:
			{
				/* The player is told about any remaining cursed items. */
				o_ptr->ident |= IDENT_SENSE_CURSED;
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
		}


		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (o_ptr->marked) obvious = TRUE;

			/* Artifacts, and other objects, get to resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					msg_format("The %v %s unaffected!",
						object_desc_f3, o_ptr, FALSE, 0,
						(plural ? "are" : "is"));
				}
			}

			/* Kill it */
			else
			{
				int o_kidx = o_ptr->k_idx;

				/* Describe if needed */
				if (o_ptr->marked && note_kill)
				{
					msg_format("The %v%s",
						object_desc_f3, o_ptr, FALSE, 0, note_kill);
				}

				is_potion = (k_info[o_kidx].tval == TV_POTION);


				/* Delete the object */
				delete_dun_object(o_ptr);

					/* Potions produce effects when 'shattered' */
					if (is_potion) {
					(void)potion_smash_effect(m_ptr, y, x, o_kidx);
					}


				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}



/*
 * Make a formerly friendly monster angry.
 */
static void anger_monster(monster_type *m_ptr)
{
	if (m_ptr->smart & SM_ALLY)
	{
		msg_format("%^v gets angry!", monster_desc_f2, m_ptr, 0);
		m_ptr->smart &= ~SM_ALLY;
	}
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
static bool project_m(monster_type *mw_ptr, int r, int y, int x, int dam, int typ)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	monster_type *m_ptr = &m_list[c_ptr->m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	C_TNEW(m_name, MNAME_MAX, char);

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
	C_TNEW(killer, MNAME_MAX, char);

	/* Assume no note */
	cptr note = NULL;

	/* Assume a default death */
	cptr note_dies = " dies.";

	/* Nobody here */
	if (!c_ptr->m_idx ||

	/* Never affect projector */
		(mw_ptr == m_ptr))
	{
		TFREE(killer);
		TFREE(m_name);
		return (FALSE);
	}


	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Get the monster name (BEFORE polymorphing) */
	strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);



	/* Some monsters get "destroyed" */
	if ((!live_monster_p(r_ptr)) ||
		(r_ptr->flags2 & (RF2_STUPID)))
	{
		/* Special note at death */
		note_dies = " is destroyed.";
	}

	if ((!mw_ptr || mw_ptr == m_list) && (m_ptr->smart & SM_ALLY)) {
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
		case GF_DISP_LIVING:
		if (live_monster_wide_p(r_ptr)) get_angry = TRUE;
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
		if (get_angry == TRUE && !mw_ptr) anger_monster(m_ptr);
	}



	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile -- pure damage */
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
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_ELEC);
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			break;
		}

		/* Nuclear waste */
		case GF_NUKE:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				if (seen) r_ptr->r_flags3 |= (RF3_IM_POIS);
			}

			else if (randint(3)==1) do_poly = TRUE;
			break;
		}

		/* Holy Orb -- hurts Evil (replaced with Hellfire) */
		case GF_HELL_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam *= 2;
				note = " is hit hard.";
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Holy Fire -- hurts Evil, Good are immune, others _resist_ */
		case GF_HOLY_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_GOOD))
			{
				dam = 0;
				note = " is immune.";
				if (seen) r_ptr->r_flags3 |= (RF3_GOOD);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam *= 2;
				note = " is hit hard.";
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
			}
			else
			{

				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			break;
		}

		/* Arrow -- XXX no defense */
		case GF_ARROW:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Plasma -- XXX perhaps check ELEC or FIRE */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_PLAS))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
					r_ptr->r_flags3 |= (RF3_RES_PLAS);

			}
			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				note = " is immune.";
				dam = 0;
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
			}
			else if (r_ptr->flags3 & (RF3_RES_NETH))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);

				if (seen)
					r_ptr->r_flags3 |= (RF3_RES_NETH);

			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam /= 2;
				note = " resists somewhat.";
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Water (acid) damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_IM_WATER)
			{
				note = " is immune.";
				dam = 0;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_WATER);
			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;
			do_conf = (5 + randint(11) + r) / (r + 1);
			if ((r_ptr->flags4 & (RF4_BR_CHAO))
				|| ((r_ptr->flags3 & (RF3_DEMON)) && (randint(3)==1)))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				do_poly = FALSE;
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARDS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_SHAR))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			break;
		}

		/* Shard ball */
		case GF_SHARD:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags4 & (RF4_BR_SHAR))
			{
				note = " resists somewhat.";
				dam /= 2;
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
				note = " resists.";
				dam *= 2; dam /= (randint(6)+6);
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
				note = " resists.";
				dam *= 2; dam /= (randint(6)+6);
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists somewhat.";
				dam /= 2;
			}
			break;
		}

		/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_DISE))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
					r_ptr->r_flags3 |= (RF3_RES_DISE);
			}
			break;
		}

		/* Nexus -- Breathers and Existers resist */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_NEXU))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				if (seen)
					r_ptr->r_flags3 |= (RF3_RES_NEXU);
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
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_INER))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			else
			{

				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
					note = " starts moving slower.";
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
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			bool resist_tele = FALSE;

			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
					note = " is unaffected!";
					resist_tele = TRUE;
				}
				else if (r_ptr->level > randint(100))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
					note = " resists!";
					resist_tele = TRUE;
				}
			}

			if (!resist_tele) do_dist = 10;
			else do_dist = 0;

			if (r_ptr->flags4 & (RF4_BR_GRAV))
			{
				note = " resists.";
				dam *= 3; dam /= (randint(6)+6);
				do_dist = 0;
			}
			else
			{

				/* 1. slowness */
				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
					note = " starts moving slower.";
				}
				/* 2. stun */
				do_stun = damroll(6 , (dam)) + 1;

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
			}
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (seen) obvious = TRUE;
			break;
		}


		/* Pure damage */
		case GF_DISINTEGRATE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);
						note = " loses some skin!";
					note_dies = " evaporates!";
					dam *= 2;
				}

				if (r_ptr->flags3 & RF1_UNIQUE)
				{
				if (rand_int(r_ptr->level + 10) > rand_int(30))
									{
									note = " resists.";
									dam >>= 3;
									}
				}
		}
		break;

	case GF_PSI:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags2 & RF2_EMPTY_MIND) {
			dam = 0;
			note = " is immune!";
			} else if ((r_ptr->flags2 & RF2_STUPID) ||
					(r_ptr->flags2 & RF2_WEIRD_MIND) ||
					(r_ptr->flags3 & RF3_ANIMAL) ||
					(r_ptr->level > randint(3 * dam))) {
			dam /= 3;
			note = " resists.";

			/* Powerful demons & undead can turn a mindcrafter's
			 * attacks back on them */
			if (((r_ptr->flags3 & RF3_UNDEAD) ||
				(r_ptr->flags3 & RF3_DEMON)) &&
				(r_ptr->level > (skill_set[SKILL_MINDCRAFTING].value/4)) &&
				(randint(2) == 1)) {
				note = NULL;
				msg_format("%^s%s corrupted mind backlashes your attack!",
						m_name, (seen ? "'s" : "s"));
				/* Saving throw */
				if (rand_int(100) < p_ptr->skill_sav) {
				msg_print("You resist the effects!");
				skill_exp(SKILL_SAVE);
				} else {
				/* Injure +/- confusion */
				strnfmt(killer, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0x88);
				take_hit(dam, killer, m_ptr->r_idx);  /* has already been /3 */
				if (randint(4) == 1) {
					switch (randint(4)) {
					case 1:
					add_flag(TIMED_CONFUSED, 3 + randint(dam));
					break;
					case 2:
					add_flag(TIMED_STUN, randint(dam)); break;
					case 3:
					{
					if (r_ptr->flags3 & (RF3_NO_FEAR))
						note = " is unaffected.";
					else
						add_flag(TIMED_AFRAID, 3 + randint(dam));
					}
					break;
					default:
					if (!p_ptr->free_act)
						(void)add_flag(TIMED_PARALYZED, randint(dam));
					break;
					}
				}
				}
				dam = 0;
			}

			}

			if ((dam > 0) && (randint(4) == 1)) {
			switch (randint(4)) {
			case 1:
				do_conf = 3 + randint(dam);  break;
			case 2:
				do_stun = 3 + randint(dam); break;
			case 3:
				do_fear = 3 + randint(dam);  break;
			default:
				do_sleep = 3 + randint(dam);  break;
			}
			}

			note_dies = " collapses, a mindless husk.";
			break;
		}

	case GF_PSI_DRAIN:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags2 & RF2_EMPTY_MIND) {
			dam = 0;
			note = " is immune!";
			} else if ((r_ptr->flags2 & RF2_STUPID) ||
					(r_ptr->flags2 & RF2_WEIRD_MIND) ||
					(r_ptr->flags3 & RF3_ANIMAL) ||
					(r_ptr->level > randint(3 * dam))) {
			dam /= 3;
			note = " resists.";

			/* Powerful demons & undead can turn a mindcrafter's
			 * attacks back on them */
			if (((r_ptr->flags3 & RF3_UNDEAD) ||
				(r_ptr->flags3 & RF3_DEMON)) &&
				(r_ptr->level > skill_set[SKILL_MINDCRAFTING].value/4) &&
				(randint(2) == 1)) {
				note = NULL;
				msg_format("%^s%s corrupted mind backlashes your attack!",
						m_name, (seen ? "'s" : "s"));
				/* Saving throw */
				if (rand_int(100) < p_ptr->skill_sav) {
				msg_print("You resist the effects!");
				skill_exp(SKILL_SAVE);
				} else {
				/* Injure + mana drain */
				strnfmt(killer, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0x88);
				msg_print("Your psychic energy is drained!");
				p_ptr->cchi = MAX(0, p_ptr->cchi - damroll(5, dam)/2);
				p_ptr->redraw |= PR_MANA;
				take_hit(dam, killer, m_ptr->r_idx);  /* has already been /3 */
				}
				dam = 0;
			}

			} else if (dam > 0) {
			int b = damroll(5, dam) / 4;
			msg_format("You convert %s%s pain into psychic energy!",
					m_name, (seen ? "'s" : "s"));
			b = MIN(p_ptr->mchi, p_ptr->cchi + b);
			p_ptr->cchi = b;
			p_ptr->redraw |= PR_MANA;
			}

			note_dies = " collapses, a mindless husk.";
			break;
		}
	case GF_TELEKINESIS:
		{
			if (seen) obvious = TRUE;
			do_dist = 7;
			/* 1. stun */
			do_stun = damroll((skill_set[SKILL_MINDCRAFTING].value/20) + 3 , (dam)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			(r_ptr->level > 5 + randint(dam)))
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
			if (seen) obvious = TRUE;
			break;
		}

	case GF_DOMINATION:
		{
				if (m_ptr->smart & SM_ALLY)   break;
			if (seen) obvious = TRUE;

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

				/* Powerful demons & undead can turn a mindcrafter's
				 * attacks back on them */
				if (((r_ptr->flags3 & RF3_UNDEAD) ||
				(r_ptr->flags3 & RF3_DEMON)) &&
				(r_ptr->level > skill_set[SKILL_MINDCRAFTING].value/4) &&
				(randint(2) == 1)) {
				note = NULL;
				msg_format("%^s%s corrupted mind backlashes your attack!",
						m_name, (seen ? "'s" : "s"));
				/* Saving throw */
				if (rand_int(100) < p_ptr->skill_sav) {
						msg_print("You resist the effects!");
						skill_exp(SKILL_SAVE);
					} else {
						/* Confuse, stun, terrify */
						switch (randint(4)) {
						case 1:
						add_flag(TIMED_STUN, dam / 2);
						break;
						case 2:
						add_flag(TIMED_CONFUSED, dam / 2);
						break;
						default:
						{
						if (r_ptr->flags3 & (RF3_NO_FEAR))
							note = " is unaffected.";
						else
							add_flag(TIMED_AFRAID, dam);
						}
						}
					}
				} else {
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
				}
			} else {
				if (r_ptr->flags1 & RF1_GUARDIAN) {
					note = " hates you too much!";
				} else {
					if ((dam > 29) && (randint(100) < dam)) {
						note = " is in your thrall!";
						m_ptr->smart |= SM_ALLY;
					} else {
						switch (randint(4)) {
						case 1:
							do_stun = dam/2;
							break;
						case 2:
							do_conf = dam/2;
							break;
						default:
							do_fear = dam;
						}
					}
				}
			}

			/* No "real" damage */
			dam = 0;
			break;
		}



		/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint(15) + 1) / (r + 1);
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_COLD);
			}
			break;
		}


		/* Drain Life */
		case GF_OLD_DRAIN:
		{
			if (seen) obvious = TRUE;

			if (!live_monster_p(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_DEMON);
				}

				note = " is unaffected!";
				obvious = FALSE;
				dam = 0;
			}

			break;
		}
		/* Death Ray */
		case GF_DEATH_RAY:
		{
			if (seen) obvious = TRUE;
			if (!live_monster_wide_p(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				note = " is immune.";
				obvious = FALSE;
				dam = 0;
			}
			else if (((r_ptr->flags1 & (RF1_UNIQUE)) &&
					(randint(888) != 666)) ||
				(((r_ptr->level + randint(20))> randint((dam)+randint(10))) &&
					randint(100) != 66 ))
			{
				note = " resists!";
				obvious = FALSE;
				dam = 0;

			}

			else dam = 4000;

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
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
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

			if (seen) obvious = TRUE;
			if ((m_ptr->smart & SM_ALLY) && (randint(3)!=1))
					is_friend = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;

			/* Attempt to clone. */
			if (multiply_monster(m_ptr, is_friend, TRUE))
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
			if (health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);

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
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
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
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags3 & (RF3_NO_SLEEP)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_SLEEP);
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


		/* Sleep (Use "dam" as "power") */
		case GF_STASIS:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
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

			dam += (adj_con_fix[p_ptr->stat_ind[A_CHR]] - 1);

			if (seen) obvious = TRUE;

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
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN)) {
				note = " hates you too much!";
			} else {
				note = " suddenly seems friendly!";
				m_ptr->smart |= SM_ALLY;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Control undead */
		case GF_CONTROL_UNDEAD:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(!(r_ptr->flags3 & (RF3_UNDEAD))) ||
				(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{

				/* Resist */
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN)) {
				note = " hates you too much!";
			} else {
				note = " is in your thrall!";
				m_ptr->smart |= SM_ALLY;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Tame animal */
		case GF_CONTROL_ANIMAL:
		{
			if (seen) obvious = TRUE;

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
			} else if (p_ptr->aggravate || (r_ptr->flags1 & RF1_GUARDIAN)) {
				note = " hates you too much!";
			} else {
					note = " is tamed!";
					m_ptr->smart |= SM_ALLY;
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
			if (seen) obvious = TRUE;

			do_stun = damroll(6 , (dam)) + 1;

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
			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Obvious effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_LITE);

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

			break;
		}



		/* Lite -- opposite of Dark */
		case GF_LITE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_LITE))
			{
				note = " resists.";
				dam *= 2; dam /= (randint(6)+6);
			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_LITE);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam *= 2;
			}
			break;
		}


		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags4 & (RF4_BR_DARK)) || (r_ptr->flags3 & RF3_ORC)
					|| (r_ptr->flags3 & RF3_HURT_LITE)) /* Likes darkness... */
			{
				note = " resists.";
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
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);

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
					if (seen) obvious = TRUE;
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
					do_dist = dam;
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
					if (seen) obvious = TRUE;
					if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
					do_dist = dam;
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

					/* Obvious */
					if (seen) obvious = TRUE;

					/* Prepare to teleport */
					do_dist = dam;
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
			/* Obvious */
			if (seen) obvious = TRUE;

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
		if (live_monster_wide_p(r_ptr))
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

	/* Dispel monster */
		case GF_DISP_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

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
	if (skipped)
	{
		TFREE(killer);
		TFREE(m_name);
		return (FALSE);
	}


	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE)) do_poly = FALSE;


	/*
	 * "Quest" monsters cannot be polymorphed
	 * Heino Vander Sanden
	 */
	if (r_ptr->flags1 & (RF1_GUARDIAN)) do_poly = FALSE;


	/*
	 * "Quest" monsters can only be "killed" by the player
	 * Heino Vander Sanden
	 */
	if (r_ptr->flags1 & RF1_GUARDIAN)
	{
		if (mw_ptr && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}


	/* Check for death */
	if (dam > m_ptr->hp)
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

		charm = (m_ptr->smart & SM_ALLY) != 0;

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
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE, charm, FALSE);

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
	if (mw_ptr && mw_ptr != m_list)
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

			if ((m_ptr->smart & (SM_ALLY)) &&
				!(m_ptr->ml))
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
			else if (dam > 0) message_pain(m_ptr, dam);

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
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (dam > 0) message_pain(m_ptr, dam);

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

	TFREE(killer);
	TFREE(m_name);

	/* Return "Anything seen?" */
	return (obvious);
}





/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * m_ptr is the monster which caused the attack.
 * dam is the "base" damage of the attack.
 * typ is the type of attack from which its effects are derived.
 *
 * The game gives descriptions for many effects if the character is blind, as
 * the game does not display the effect graphically.
 */
static void project_p_aux(monster_type *m_ptr, int dam, int typ)
{
	/* Store blindness here for easy reference. */
	const bool blind = !!p_ptr->blind;

	/* Get the monster's real name */
	C_TNEW(killer, MNAME_MAX, char);
	strnfmt(killer, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0x88);

	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{
			if (blind) msg_print("You are hit by acid!");
			acid_dam(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{
			if (blind) msg_print("You are hit by fire!");
			fire_dam(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{
			if (blind) msg_print("You are hit by cold!");
			cold_dam(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{
			if (blind) msg_print("You are hit by lightning!");
			elec_dam(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			if (blind) msg_print("You are hit by poison!");
			if (p_ptr->resist_pois) dam = (dam + 2) / 3;
			if (p_ptr->oppose_pois) dam = (dam + 2) / 3;
			if ((!(p_ptr->oppose_pois || p_ptr->resist_pois))
				&& randint(HURT_CHANCE)==1)
				(void) do_dec_stat(A_CON);
			take_hit(dam, killer, m_ptr->r_idx);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void)add_flag(TIMED_POISONED, rand_int(dam) + 10);
			}
			break;
		}

		/* Standard damage -- also poisons / mutates player */
		case GF_NUKE:
		{
			if (blind) msg_print("You are hit by radiation!");
			if (p_ptr->resist_pois) dam = (2 * dam + 2) / 5;
			if (p_ptr->oppose_pois) dam = (2 * dam + 2) / 5;
			take_hit(dam, killer, m_ptr->r_idx);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void)add_flag(TIMED_POISONED, rand_int(dam) + 10);
				if (randint(5)==1) /* 6 */
				{ msg_print("You undergo a freakish metamorphosis!");
					if (randint(4)==1) /* 4 */
						do_poly_self();
					else
						chaos_feature_shuffle();
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
		case GF_HOLY_FIRE:
		case GF_HELL_FIRE:
		{
			if (blind) msg_print("You are hit by something!");
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Arrow -- XXX no dodging */
		case GF_ARROW:
		{
			if (blind) msg_print("You are hit by something sharp!");
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Plasma -- XXX No resist */
		case GF_PLASMA:
		{
			if (blind) msg_print("You are hit by something *HOT*!");
			take_hit(dam, killer, m_ptr->r_idx);
			if (!p_ptr->resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
				(void)add_flag(TIMED_STUN, k);
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
			if (blind) msg_print("You are hit by nether forces!");
			if (p_ptr->resist_neth)
			{
				/* Hmm... */
				if (!p_ptr->heal_nether) dam *= 6;
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
					msg_print("You feel your life slipping away!");
					lose_skills(2);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_skills(10);
				}
			}
			if (p_ptr->heal_nether)
			{
				msg_print("You feel invigorated!");
				hp_player(dam / 4);
			}
			else
			{
				take_hit(dam, killer, m_ptr->r_idx);
			}
			break;
		}

		/* Water -- stun/confuse */
		case GF_WATER:
		{
			if (blind) msg_print("You are hit by something wet!");
			if (!p_ptr->resist_sound)
			{
				(void)add_flag(TIMED_STUN, randint(40));
			}
			if (!p_ptr->resist_conf)
			{
				(void)add_flag(TIMED_CONFUSED, randint(5) + 5);
			}

			if (randint(5)==1)
				{
					inven_damage(set_cold_destroy, 3);
				}

			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Chaos -- many effects */
		case GF_CHAOS:
		{
			if (blind) msg_print("You are hit by a wave of anarchy!");
			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)add_flag(TIMED_CONFUSED, rand_int(20) + 10);
			}
			if (!p_ptr->resist_chaos)
			{
				(void)add_flag(TIMED_IMAGE, randint(10));
				if (randint(3)==1)
				{
					msg_print("Your body is twisted by chaos!");
					(void) gain_chaos_feature(0);
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
					lose_skills(2);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_skills(10);
				}
			}
				if ((!p_ptr->resist_chaos) || (randint(9)==1))
				{
					inven_damage(set_elec_destroy, 2);
					inven_damage(set_fire_destroy, 2);
				}
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Shards -- mostly cutting */
		case GF_SHARDS:
		{
			if (blind) msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)add_flag(TIMED_CUT, dam);
			}
				if ((!p_ptr->resist_shard) || (randint(13)==1))
				{
					inven_damage(set_cold_destroy, 2);
				}

			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Sound -- mostly stunning */
		case GF_SOUND:
		{
			if (blind) msg_print("You are hit by a loud noise!");
			if (p_ptr->resist_sound)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)add_flag(TIMED_STUN, k);
			}
				if ((!p_ptr->resist_sound) || (randint(13)==1))
				{
					inven_damage(set_cold_destroy, 2);
				}

			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
			if (blind) msg_print("You are hit by something puzzling!");
			if (p_ptr->resist_conf)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)add_flag(TIMED_CONFUSED, randint(20) + 10);
			}
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (blind) msg_print("You are hit by something static!");
			if (p_ptr->resist_disen)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)apply_disenchant(0);
			}
			take_hit(dam, killer, m_ptr->r_idx);
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
				apply_nexus(m_ptr);
			}
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
			if (blind) msg_print("You are hit by kinetic force!");
			if (!p_ptr->resist_sound)
			{
				(void)add_flag(TIMED_STUN, randint(20));
			}
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}


		/* Shard -- stun, cut */
		case GF_SHARD:
		{
			if (blind) msg_print("You are hit by shards!");
			if (!p_ptr->resist_sound)
			{
				(void)add_flag(TIMED_STUN, randint(20));
			}
			if (p_ptr->resist_shard)
			{
				dam /= 2;
			}
			else
			{
				(void)set_flag(TIMED_CUT, p_ptr->  cut + ( dam / 2) );
			}

				if ((!p_ptr->resist_shard) || (randint(12)==1))
				{
					inven_damage(set_cold_destroy, 3);
				}

			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (blind) msg_print("You are hit by something slow!");
			(void)add_flag(TIMED_SLOW, rand_int(4) + 4);
			take_hit(dam, killer, m_ptr->r_idx);
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
				(void)add_flag(TIMED_BLIND, randint(5) + 2);
			}
			if (p_ptr->hurt_light) {
			msg_print("The light scorches your flesh!");
			dam *= 2;
			}
			take_hit(dam, killer, m_ptr->r_idx);
			if (p_ptr->wraith_form)
			{
				p_ptr->wraith_form = 0;
				msg_print("The light forces you out of your incorporeal shadow form.");
				p_ptr->redraw |= PR_MAP;
				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);

			}

			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->immune_dark)
			{
				dam = 0;
			}
			else if (p_ptr->resist_dark)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)add_flag(TIMED_BLIND, randint(5) + 2);
			}
			if (p_ptr->wraith_form)
			hp_player(dam);
			else
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
			if (blind) msg_print("You are hit by a blast from the past!");

			if (p_has_mutation(MUT_RES_TIME))
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
						lose_skills(20);
						break;
					}

					case 6: case 7: case 8: case 9:
					{
						do_dec_stat_time(rand_int(A_MAX), TRUE);
						break;
					}

					case 10:
					{
						int k;
						msg_print("You're not as powerful as you used to be...");

						for (k = 0; k < A_MAX; k++) do_dec_stat_time(k, FALSE);
						break;
					}
				}
			}
		take_hit(dam, killer, m_ptr->r_idx);
		break;
	}

		/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY:
		{
			if (blind) msg_print("You are hit by something heavy!");
			msg_print("Gravity warps around you.");
			teleport_player(5);
			if (!p_ptr->ffall)
				(void)add_flag(TIMED_SLOW, rand_int(4) + 4);
			if (!(p_ptr->resist_sound || p_ptr->ffall))
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)add_flag(TIMED_STUN, k);
			}
			if (p_ptr->ffall)
			{
				dam = (dam * 2) / 3;
			}

				if ((!p_ptr->ffall) || (randint(13)==1))
				{
					inven_damage(set_cold_destroy, 2);
				}

			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Standard damage */
		case GF_DISINTEGRATE:
		{
			if (blind) msg_print("You are hit by pure energy!");
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

	case GF_OLD_HEAL:
		{
			if (blind) msg_print("You are hit by something invigorating!");
			(void)hp_player(dam);
			break;
		}

	case GF_OLD_SPEED:
		{
			if (blind)  msg_print("You are hit by something!");
			(void)add_flag(TIMED_FAST, randint(5));
			break;
		}

	case GF_OLD_SLOW:
		{
			if (blind) msg_print("You are hit by something slow!");
			(void)add_flag(TIMED_SLOW, rand_int(4) + 4);
			break;
		}
	case GF_OLD_SLEEP:
		{
			if (p_ptr->free_act)  break;
			if (blind) msg_print("You fall asleep!");
			add_flag(TIMED_PARALYZED, dam);
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (blind) msg_print("You are hit by an aura of magic!");
			take_hit(dam, killer, m_ptr->r_idx);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		{
			if (blind) msg_print("Something falls from the sky on you!");
			take_hit(dam, killer, m_ptr->r_idx);
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
			if (blind) msg_print("You are hit by something sharp and cold!");
			cold_dam(dam, killer, m_ptr->r_idx);
			if (!p_ptr->resist_shard)
			{
				(void)add_flag(TIMED_CUT, damroll(5, 8));
			}
			if (!p_ptr->resist_sound)
			{
				(void)add_flag(TIMED_STUN, randint(15));
			}

				if ((!(p_ptr->resist_cold || p_ptr->oppose_cold)) || (randint(12)==1))
				{
					if(!(p_ptr->immune_cold)) inven_damage(set_cold_destroy, 3);
				}

			break;
		}
	}

	TFREE(killer);
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
 * NOTE (Zangband): 'Bolt' attacks can be reflected back, so we need to know
 * if this is actually a ball or a bolt spell
 *
 *
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 */
static bool project_p(monster_type *m_ptr, int r, int y, int x, int dam, int typ, int a_rad)
{
	/* Player is not here */
	if ((x != px) || (y != py) ||

	/* Player cannot hurt himself */
		!m_ptr)
	{
		return (FALSE);
	}


	if (p_ptr->reflect && !a_rad && !one_in(10))
	{
		int t_y, t_x;
		int max_attempts = 10;


		if (p_ptr->blind)
			msg_print("Something bounces!");
		else
			msg_print("The attack bounces!");

		/* Choose 'new' target */
		do
		{
			t_y = m_ptr->fy + rand_int(3);
			t_x = m_ptr->fx + rand_int(3);
			max_attempts--;
		}

		while ((max_attempts > 0) && in_bounds2(t_y, t_x) &&
					!(player_has_los_bold(t_y, t_x)));

		if (max_attempts < 1)
		{

			t_y = m_ptr->fy;
			t_x = m_ptr->fx;
		}

		project(0, 0, t_y, t_x, dam, typ,
			(PROJECT_STOP|PROJECT_KILL));
		disturb(1);
		return TRUE;
	}

	/* XXX XXX XXX */
	/* Limit maximum damage */
	if (dam > 1600) dam = 1600;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);

	/* Actually carry out the effect. */
	project_p_aux(m_ptr, dam, typ);

	/* Disturb */
	disturb(1);

	/* Hack - assume something was seen. */
	return TRUE;
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
 * Find the distance from (x, y) to a line.
 */
static int dist_to_line(int x, int y, int x1, int y1, int x2, int y2)
{
	/* Vector from (x, y) to (x1, y1) */
	int py = y1 - y;
	int px = x1 - x;

	/* Normal vector */
	int ny = x2 - x1;
	int nx = y1 - y2;

	/* Length of N */
	int d = distance(x1, y1, x2, y2);

	/* Component of P on N */
	d = ((d) ? ((py * ny + px * nx) / d) : 0);

	/* Absolute value */
	return ((d >= 0) ? d : 0 - d);
}



/*
 * Generic "beam"/"bolt"/"ball/breath" projection routine.  -BEN-
 *
 * Input:
 *   mw_ptr: Index of "source" monster (or NULL for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball, -9 to -1 = breath)
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
 * See below for details about the path of a "breath".
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
 * Finally, note that the gm[] array is only used for ball attacks.
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
bool project(monster_type *mw_ptr, int rad, int y, int x, int dam, int typ, int flg)
{
	int i, t, dist;
	int y1, x1, y2, x2;
	int y0, x0, y9, x9;
	int dist_centre;
	int y_target, x_target; /* For reflecting monsters */

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
	y0 = py;
	x0 = px;


	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;
	}

	/* Hack -- Start at player */
	else if (!mw_ptr)
	{
		x1 = px;
		y1 = py;
	}

	/* Start at a monster */
	else
	{
		x1 = mw_ptr->fx;
		y1 = mw_ptr->fy;
	}

	y_target = y1;
	x_target = x1;


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
				print_rel('*', spell_color(typ), y, x);
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
					print_rel(bolt_char(y, x, y9, x9), spell_color(typ), y9, x9);
				}
				else
				{
					print_rel(bolt_graf_char(y,x,y9,x9,typ),bolt_graf_attr(typ),y9,x9);
				}
				move_cursor_relative(y9, x9);
				Term_fresh();
				visual = TRUE;
				Term_xtra(TERM_XTRA_DELAY, delay_factor);
				lite_spot(y9, x9);
				Term_fresh();
			}

			/* Hack -- make sure to delay anyway for consistency */
			else if (visual)
			{
				/* Delay for consistency */
				Term_xtra(TERM_XTRA_DELAY, delay_factor);
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

	dist_centre = dist;

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
							if (cave_valid_bold(y,x) &&
								(c_ptr->feat < FEAT_PATTERN_START
								|| c_ptr->feat > FEAT_PATTERN_XTRA2))
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
						print_rel('*', spell_color(typ), y, x);
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
				Term_xtra(TERM_XTRA_DELAY, delay_factor);
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

	/* Hack - handle reflective monsters. */
	if (flg & (PROJECT_KILL) && grids == 1)
	{
		monster_race *ref_ptr = &r_info[m_list[c_ptr->m_idx].r_idx];
		if ((ref_ptr->flags2 & (RF2_REFLECTING)) && (one_in(10))
			&& (dist_centre > 1))
		{
			int t_y, t_x, max_attempts;

			/* Choose 'new' target */
			for (max_attempts = 0; max_attempts < 10; max_attempts++)
			{

				t_y = y_target + rand_range(-1, 1);
				t_x = x_target + rand_range(-1, 1);

				/* Paranoia - bounced through the walls. */
				if (!in_bounds2(t_y, t_x)) goto done_reflect;

				/* Visible from the first grid. */
				if (los(y, x, t_y, t_x)) goto done_reflect;
			}

			/* Failure. */
			t_y = y_target;
			t_x = x_target;

done_reflect: /* Success */

			if (m_list[c_ptr->m_idx].ml)
			{
				msg_print("The attack bounces!");
				ref_ptr->r_flags2 |= RF2_REFLECTING;
				notice = TRUE;
			}
			notice |=
				project(&m_list[c_ptr->m_idx], 0, t_y, t_x,  dam, typ, flg);

			/* Don't affect this monster, but affect everything else here. */
			flg &= ~(PROJECT_KILL);
		}
	}

	/* No monsters have been hit yet. */
	project_m_n = 0;

	/* Start at a distance of nought. */
	dist = 0;

	/* Scan grids for stuff. */
	for (i = 0; i < grids; i++)
	{
		/* Breaths recalculate for each grid. */
		if (breath) dist = dist_to_line(x, y, x1, y1, x2, y2);

		/* Hack -- Notice new "dist" values */
		else if (gm[dist+1] == i) dist++;

		/* Get the grid location */
		y = gy[i];
		x = gx[i];

		/* Check features */
		if (flg & (PROJECT_GRID))
		{
			/* Affect the feature in that grid */
			if (project_f(dist, y, x, dam, typ)) notice = TRUE;
		}
		/* Check objects. */
		if (flg & (PROJECT_ITEM))
		{
			/* Affect the object in the grid */
			if (project_o(mw_ptr, dist, y, x, dam, typ)) notice = TRUE;
		}
		/* Check monsters */
		if (flg & (PROJECT_KILL))
		{
			/* Affect the monster in the grid */
			if (project_m(mw_ptr, dist, y, x, dam, typ)) notice = TRUE;

			/* Affect the player in the grid */
			if (project_p(mw_ptr, dist, y, x, dam, typ, rad)) notice = TRUE;
		}
	}

	/* Player affected one monster (without "jumping") */
	if (!mw_ptr && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
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

	/* Return "something was noticed" */
	return (notice);
}




typedef struct potion_smash_effect_type potion_smash_effect_type;
struct potion_smash_effect_type
{
	s16b k_idx;
	bool angry;
	bool ident;

	byte typ;
	byte dd;
	byte ds;
	byte rad;
};

static potion_smash_effect_type potion_smash_effects[] =
{
	{OBJ_POTION_SALT_WATER, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_SLIME_MOULD_JUICE, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_LOSE_MEMORIES, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_STR, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_INT, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_WIS, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_DEX, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_CON, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_DEC_CHR, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_WATER, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_APPLE_JUICE, TRUE, FALSE, 0, 0, 0, 2},
	{OBJ_POTION_SLOWNESS, TRUE, TRUE, GF_OLD_SLOW, 5, 1, 2},
	{OBJ_POTION_POISON, TRUE, TRUE, GF_POIS, 3, 1, 2},
	{OBJ_POTION_BLINDNESS, TRUE, TRUE, GF_DARK, 0, 0, 2},
	{OBJ_POTION_BOOZE, TRUE, TRUE, GF_OLD_CONF, 0, 0, 2},
	{OBJ_POTION_SLEEP, TRUE, TRUE, GF_OLD_SLEEP, 0, 0, 2},
	{OBJ_POTION_DETONATIONS, TRUE, TRUE, GF_SHARDS, 25, 25, 2},
	{OBJ_POTION_RUINATION, TRUE, TRUE, GF_SHARDS, 25, 25, 2},
	{OBJ_POTION_IOCAINE, TRUE, FALSE, GF_DEATH_RAY, 0, 0, 1},
	{OBJ_POTION_SPEED, FALSE, TRUE, GF_OLD_SPEED, 0, 0, 2},
	{OBJ_POTION_CURE_LIGHT, FALSE, TRUE, GF_OLD_HEAL, 2, 3, 2},
	{OBJ_POTION_CURE_SERIOUS, FALSE, TRUE, GF_OLD_HEAL, 4, 3, 2},
	{OBJ_POTION_CURE_CRITICAL, FALSE, TRUE, GF_OLD_HEAL, 6, 3, 2},
	{OBJ_POTION_CURING, FALSE, TRUE, GF_OLD_HEAL, 6, 3, 2},
	{OBJ_POTION_HEALING, FALSE, TRUE, GF_OLD_HEAL, 10, 10, 2},
	{OBJ_POTION_STAR_HEALING, FALSE, TRUE, GF_OLD_HEAL, 50, 50, 1},
	{OBJ_POTION_LIFE, FALSE, TRUE, GF_OLD_HEAL, 50, 50, 1},
	{OBJ_POTION_RES_MANA, FALSE, TRUE, GF_MANA, 10, 10, 1},
};

/*
 * Potions "smash open" and cause an area effect when
 * (1) they are shattered while in the player's inventory,
 * due to cold (etc) attacks;
 * (2) they are thrown at a monster, or obstacle;
 * (3) they are shattered by a "cold ball" or other such spell
 * while lying on the floor.
 *
 * Arguments:
 *   m_ptr:  who caused the potion to shatter.
 *           NULL represents the player, and cannot hurt him.
 *           m_list represents the player when it can hurt him.
 *           Anything else is assumed to be another monster.
 *    y, x:  coordinates of the potion (or player if
 *           the potion was in her inventory);
 *  o_kidx:  type of potion being smashed.
 *
 * Returns TRUE if the effect should annoy the monster at the current
 * location. This should only happen if the player caused the effect, but this
 * isn't checked here.
 */
void potion_smash_effect(monster_type *m_ptr, int y, int x, int o_kidx)
{
	potion_smash_effect_type *ptr;

	FOR_ALL_IN(potion_smash_effects, ptr)
	{
		if (ptr->k_idx == o_kidx)
		{
			int dam = damroll(ptr->dd, ptr->ds);

			if (ptr->typ)
			{
				project(m_ptr, ptr->rad, y, x, dam, ptr->typ,
					(PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));
			}

			if (ptr->ident) k_info[o_kidx].aware = TRUE;

			/* It's your fault, so they'll hate you for it. */
			if (ptr->angry && (!m_ptr || m_ptr == m_list))
			{
				int m_idx = cave[y][x].m_idx;

				if (m_idx) anger_monster(m_list+m_idx);
			}
		}
	}
}
