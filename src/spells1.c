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
 * Reduce damage according to resistance
 */
int apply_resistance(int dam, s16b res)
{
	/* Trivial cases */
 	if (!res || (dam <= 0)) return dam;

	/* Immunity */
	if (res >= 100) return 0;

	/* Apply percentile resistance */
	dam = (dam * (100 - res)) / 100;

	/* Lower boundary control - only full immunity can lower damage to 0 */
	if (dam < 1) dam = 1;

	return dam;
}

static int resist_steps[11] = {0,0,0,25,50,100,200,300,400,600,1000};

/*
 * Chance to avoid a resistance according to effect.
 * The higher the factor, the better the chance to avoid the effect.
 */
bool resist_effect(s16b res)
{
	int i, j;
	/* All of the resist_caps are 88 */
	int step = resist_caps[res].normal;
	int chance = 0;

	/* Full resistance, immune to the effect*/
	/* !!! The higher level powers have a lower cap!!! */
	/* Well, the higher level powers now have the same cap. What's happening */
	/* here is that if your resistance is higher than 88 you are resistant to the effect */
	if (p_ptr->res[res] >= step) return TRUE;

	/* Nearest lower whole step */
	i = (p_ptr->res[res]*10) / step;
        
	/* Fraction of the next step (j/step) */
	j = (p_ptr->res[res]*10) % step;

	/* I'm not sure this works - gets a chance to resist effect */
	/* unlikely for values under 33% */
	chance = resist_steps[i] + 
		(((100 * j ) / step) * (resist_steps[i + 1] - resist_steps[i])) / 100;
	
	/* HACK */
	if (p_ptr->res[res] < 0) chance = 0;

	return (rand_int(1000) <= chance);
}

/*
 * Does an item ignore the damage? Add a flag for when brands are reworked. 
 */
static bool ignores_damage_p(const object_type *o_ptr, int res_type)
{
	/* u32b f1, f2, f3; */
	/* byte slays[SL_MAX]; */

	/* Resistance */
	if (object_resist(o_ptr, res_type) > RST_IGNORE_ELEM) return TRUE;
	
	if (artifact_p(o_ptr)) return TRUE;
	
	/* Brand */
	/*	weapon_slays(o_ptr, slays); */
	/* if (slays[slay_type] > 10) return TRUE; */

	/* Ignores all elements */
	/* object_flags(o_ptr, &f1, &f2, &f3);	*/
	/* if (f3 & TR3_IGNORE_ACID &&			*/
	/*	f3 & TR3_IGNORE_ELEC &&				*/
	/*	f3 & TR3_IGNORE_FIRE &&				*/
	/*	f3 & TR3_IGNORE_COLD) return TRUE;	*/

	return FALSE;
}

/*
 * Draw some projections in multi-hued colors.
 * -TY-, -EB-
 */
static byte mh_attr(void)
{
	switch (randint(9))
	{
		case 1:  return (TERM_RED);
		case 2:  return (TERM_GREEN);
		case 3:  return (TERM_BLUE);
		case 4:  return (TERM_YELLOW);
		case 5:  return (TERM_ORANGE);
		case 6:  return (TERM_VIOLET);
		case 7:  return (TERM_L_RED);
		case 8:  return (TERM_L_GREEN);
		case 9:  return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte fire_color(void)
{
	switch (rand_int(7))
	{
		case 0: case 1: case 2: return (TERM_RED);
		case 3: case 4: return (TERM_L_RED);
		case 5: return (TERM_YELLOW);
		case 6: 
		{
			switch (rand_int(3))
			{
				case 0: return (TERM_WHITE);
				case 1: return (TERM_L_BLUE);
				case 2: return (TERM_L_GREEN);
			}
		}
	}

	return (TERM_WHITE);
}

static byte earth_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: return (TERM_UMBER);
		case 2: return (TERM_L_UMBER);
		case 3: return (TERM_YELLOW);
		case 4:
		{
			switch (rand_int(2))
			{
				case 0: return (TERM_ORANGE);
				case 1: return (TERM_L_DARK);				
			}
		}
	}

	return (TERM_WHITE);
}


static byte wind_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: case 2: return (TERM_WHITE);
		case 3: case 4: return (TERM_SLATE);
		case 5: return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte water_color(void)
{
	switch (rand_int(8))
	{
		case 0: case 1: case 3: return (TERM_BLUE);
		case 4: case 5: return (TERM_L_BLUE);
		case 6: return (TERM_SLATE);
		case 7: return (TERM_GREEN);
	}

	return (TERM_WHITE);
}

static byte elec_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_BLUE);
		case 4: return (TERM_L_BLUE);
		case 5: 
		{
			switch (rand_int(3))
			{
				case 0: return (TERM_SLATE);
				case 1: return (TERM_L_DARK);
				case 2: return (TERM_WHITE);
			}
		}
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

static byte acid_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_L_DARK);
}

static byte poison_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: return (TERM_GREEN);
		case 2: return (TERM_L_GREEN);
		case 3: return (TERM_L_UMBER);
		case 4: 
		{
			switch (rand_int(3))
			{
				case 0: return (TERM_SLATE);
				case 1: return (TERM_ORANGE);
				case 2: return (TERM_VIOLET);
			}
		}
	}

	return (TERM_WHITE);
}

static byte time_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_L_BLUE);
		case 2: return (TERM_WHITE);
		case 3: return (TERM_L_WHITE);
	}

	return (TERM_WHITE);
}
static byte nexus_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_L_RED);
		case 2: return (TERM_RED);
		case 3: return (TERM_VIOLET);
	}

	return (TERM_WHITE);
}
static byte sound_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_VIOLET);
	}

	return (TERM_WHITE);
}

static byte nether_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_VIOLET);
		case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_WHITE);
}

static byte forces_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_DARK);
		case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_WHITE);
}

static byte light_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_ORANGE);
	}

	return (TERM_WHITE);
}

static byte holy_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: return (TERM_YELLOW);
		case 2: return (TERM_RED);
		case 3: return (TERM_ORANGE);
		case 4: case 5: return (TERM_VIOLET);
		case 6: return (TERM_WHITE);
	}

	return (TERM_WHITE);
}


/*
 * Return a color to use for the bolt/ball spells
 */
byte spell_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_HURT:			return (TERM_UMBER);
		case GF_HEAT:			return (TERM_L_RED);
		case GF_FIRE:			return (fire_color());
		case GF_PLASMA:			return (fire_color());
		case GF_ROCK:			return (TERM_UMBER);
		case GF_EARTH:			return (earth_color());
		case GF_SHARDS:			return (earth_color());
		case GF_GUST:			return (TERM_WHITE);
		case GF_WIND:			return (wind_color());
		case GF_GALE:			return (wind_color());
		case GF_RUST:			return (TERM_SLATE);
		case GF_STEAM:			return (water_color());
		case GF_STORM:			return (water_color());
		
		case GF_SHOCK:			return (TERM_BLUE);
		case GF_ELEC:			return (elec_color());
		case GF_VOLT:			return (elec_color());
		case GF_CHILL:			return (TERM_WHITE);
		case GF_ICE:			return (ice_color());
		case GF_GLACIAL:		return (ice_color());
		case GF_CORROSIVE:		return (TERM_SLATE);
		case GF_ACID:			return (acid_color());
		case GF_LIQUESCE:		return (acid_color());
		case GF_CAUSTIC:		return (TERM_GREEN);
		case GF_POISON:			return (poison_color());
		case GF_CONTAGION:		return (poison_color());
		
		case GF_AGE:			return (TERM_L_BLUE);
		case GF_TIME:			return (time_color());
		case GF_CHRONOS:		return (time_color());
		case GF_VAPOR:			return (TERM_L_RED);
		case GF_ETHER:			return (nexus_color());
		case GF_NEXUS:			return (nexus_color());
		case GF_VIBE:			return (TERM_YELLOW);
		case GF_SOUND:			return (sound_color());
		case GF_SONIC:			return (sound_color());
		case GF_UNHOLY:			return (TERM_VIOLET);
		case GF_NETHER:			return (nether_color());
		case GF_ABYSS:			return (nether_color());
		case GF_DISP_EVIL:		return (light_color());
		case GF_HOLY_FIRE:		return (holy_color());
		
		case GF_EMP:			return (forces_color());
		case GF_MAGNETIC:		return (forces_color());
		case GF_GRAVITY:		return (forces_color());
		case GF_WEAK_RAD:		return (forces_color());
		case GF_STRONG_RAD:		return (forces_color());
		
		case GF_GLOW:			return (light_color());
		case GF_LIGHT:			return (light_color());
		case GF_BRILLIANCE:		return (light_color());
		case GF_DIM:			return (TERM_L_DARK);
		case GF_DARK:			return (TERM_L_DARK);
		case GF_TENEBROUS:		return (TERM_L_DARK);
		
		case GF_FEAR:			return (TERM_VIOLET);
		case GF_PSI:			return (TERM_VIOLET);
		case GF_DOMINATION:		return (TERM_VIOLET);
		case GF_STUN:			return (TERM_VIOLET);
		case GF_TK:				return (TERM_VIOLET);
		case GF_FORCE:			return (TERM_VIOLET);
		case GF_CONFUSION:		return (TERM_VIOLET);
		case GF_SPIRIT:			return (TERM_VIOLET);
		case GF_ECTOPLASM:		return (TERM_VIOLET);
		
		case GF_WHIP:			return (TERM_UMBER);
		case GF_ARROW:			return (TERM_UMBER);
		case GF_BULLET:			return (TERM_UMBER);
		case GF_SHOT:			return (TERM_UMBER);
		case GF_PYRO_SHOT:		return (fire_color());		
		case GF_ROCKET:			return (TERM_UMBER);
		case GF_MISSILE:		return (TERM_UMBER);

		case GF_CHARM:				return (TERM_L_RED);
		case GF_CONTROL_ANIMAL:		return (TERM_L_RED);
		case GF_CONTROL_PLANT:		return (TERM_L_RED);
		case GF_CONTROL_AUTO_CONST:	return (TERM_L_RED);
		case GF_CONTROL_XXX8:		return (TERM_L_RED);
		case GF_CONTROL_ELEMENTAL:	return (TERM_L_RED);
		case GF_CONTROL_DEMON:		return (TERM_L_RED);
		case GF_CONTROL_UNDEAD:		return (TERM_L_RED);
		case GF_CONTROL_BEASTMAN:	return (TERM_L_RED);
		case GF_CONTROL_ALIEN:		return (TERM_L_RED);
		
		case GF_CLONE:			return (TERM_WHITE);
		case GF_POLY:			return (TERM_WHITE);
		case GF_HEAL:			return (TERM_WHITE);
		case GF_SPEED:			return (TERM_WHITE);
		case GF_SLOW:			return (TERM_WHITE);
		case GF_SLEEP:			return (TERM_WHITE);
		case GF_DRAIN:			return (TERM_WHITE);
		
		case GF_KILL_WALL:		return (TERM_WHITE);	
		case GF_KILL_DOOR:		return (TERM_WHITE);
		case GF_KILL_TRAP:		return (TERM_WHITE);
		case GF_MAKE_WALL:		return (TERM_WHITE);
		case GF_MAKE_DOOR:		return (TERM_WHITE);
		case GF_MAKE_TRAP:		return (TERM_WHITE);

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
void take_hit(int dam, cptr kb_str, bool wounding)
{
	int wound_dam;
	int old_chp = p_ptr->chp;
	int old_cwp = p_ptr->cwp;
	
	/* This will give you the % of current health before the attack */
	/* 23/85 hp == 100 * 23 / 85 == 27% of total health */
	int health_chance = 100 * old_chp / p_ptr->mhp;
	int damage_chance = 100 * dam / (old_chp + 1);
	
	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);
	int wound_warning = (p_ptr->mwp * op_ptr->hitpoint_warn / 10);

	if (p_ptr->skills[SK_LUCK].skill_max > 0)
	{
		if (one_in_(60-(p_ptr->skills[SK_LUCK].skill_rank * 2)))
		{
			/* Message */
			message(MSG_GENERIC, 0, "You feel lucky!");
			return;
		}
	}
	/* figure out how many wound points the player loses */
	/* Damage past 100 isn't quite so devastating to wound damage */
	if (dam > 100) wound_dam = 7 + dam/30;
	else wound_dam = dam/10;

	/* Sometimes get a lucky hit for the monsters. */
	if (one_in_(8)) wound_dam *= 2;

	/* Paranoia */
	if (p_ptr->is_dead) return;
	if (wound_dam < 1) wound_dam = 1;

	/* Turn off wound damage */
	if (!wounding) wound_dam = 0;
	
	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (dam < 9000)) return;

	/* Hurt the player */
	p_ptr->chp -= dam;
	
	/* This is Luc's formula - basically it uses a ratio of damage to chp. */
	/* Using only this seems to rarely cause wounds, because most damage is */
	/* small in compairson to current hit points. - so checking the old way */
	/* also */
	if ((rand_int(100) < damage_chance) || 
		(randint(102) > health_chance) ||
		one_in_(20)) 
	{
		/* prevent insta-death if damage is greater than wp maximum */
		if (wound_dam >= p_ptr->mwp)
		{
				if (p_ptr->cwp < 2) p_ptr->cwp -= wound_dam;
				else
				{			
					message(MSG_HITPOINT_WARN, 0, "You take a *mortal* wound!");
					p_ptr->chp = 0;
					p_ptr->cwp = 1;
				}
		}
		else p_ptr->cwp -= wound_dam;
	
	}

	if (p_ptr->chp < 0) 
	{
		/* if you have no hit points, get an extra 2-4 wounds */
		p_ptr->cwp -= (short)rand_range(2, 4);
		p_ptr->chp = 0;
	}
	
	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Dead player */
	if (p_ptr->cwp < 0)
	{
		/* Hack -- Note death */
		message(MSG_DEATH, 0, "You die.");
		message_flush();

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
		{
			bell("Low hitpoint warning!");
		}
		else 
		{
			/* Message */
			msg_format("* low hitpoint warning! %d/%d *", p_ptr->chp, p_ptr->mhp);
		}
		message_flush();
	}

	if (p_ptr->cwp < wound_warning)
	{
		/* Hack -- bell on first notice */
		if (alert_hitpoint && (old_cwp > warning))
		{
			bell("Your health is low!");
		}
		else 
		{
		
			/* Message */
			msg_format("*** LOW WOUNDPOINT WARNING! %d/%d ***", p_ptr->cwp, p_ptr->mwp);
		}
		message_flush();
	}
	
}


/*
 * Does a given object (usually) hate fire?
 */
static bool hates_fire(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_CHEST:
		case TV_GUN:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_LEG:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_TEXT:
		case TV_MAGIC_BOOK:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate earth?
 */
static bool hates_earth(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_TOOL:
		case TV_MECHANISM:
		case TV_APPARATUS:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate air?
 * These objects aren't destroyed, they're unequipped.
 * (This breaks the rule about objects 'not being destroyed' but 
 * only because they aren't 'destroyed'. Plus it allows us to 
 * "Your hat blows off!" which works, and is sweet.
 */
static bool hates_air(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HELM:
		case TV_TEXT:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate water?
 */
static bool hates_water(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_LITE:
		/* New weapon types */
		case TV_MECHA_TORSO:
		case TV_MECHA_HEAD:
		case TV_MECHA_ARMS:
		case TV_MECHA_FEET:
		case TV_MECHANISM:
		case TV_FOOD:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate elec?
 */
static bool hates_elec(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING:
		case TV_RAY:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate ice?
 */
static bool hates_ice(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_TONIC:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate acid?
 */
static bool hates_acid(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_BULLET:
		case TV_SHOT:
		case TV_GUN:
		case TV_AMMO:
		/* Weapons go here */
		case TV_HELM:
		case TV_CROWN:
		case TV_LEG:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_CHEST:
		case TV_MECHANISM:
		case TV_TEXT:
		case TV_FOOD:
		case TV_MAGIC_BOOK:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Does a given object (usually) hate poison?
 */
static bool hates_poison(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_FOOD:
		case TV_TONIC:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

static bool hates_emp(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_MECHA_HEAD:
		case TV_MECHA_TORSO:
		case TV_MECHA_ARMS:
		case TV_MECHA_FEET:
		case TV_APPARATUS:
		case TV_MECHANISM:
		{
			return (TRUE);
		}
	}
	return (FALSE);
}

/*
 * Burn something
 */
static int set_fire_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_FIR)) return (FALSE);
	return (TRUE);
}

static int set_earth_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_earth(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_EAR)) return (FALSE);
	return (TRUE);
}

static int set_air_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_air(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_AIR)) return (FALSE);
	return (TRUE);
}

static int set_water_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_water(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_WTR)) return (FALSE);
	return (TRUE);
}

static int set_elec_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_ELC)) return (FALSE);
	return (TRUE);
}
static int set_ice_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_ice(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_ICE)) return (FALSE);
	return (TRUE);
}
static int set_acid_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_ACD)) return (FALSE);
	return (TRUE);
}
static int set_poison_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	if (!hates_poison(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f2 & (TR2_IMMUNE)) return (FALSE);
	if (ignores_damage_p(o_ptr, RS_PSN)) return (FALSE);
	return (TRUE);
}


/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(const object_type *);

/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 *
 * Returns number of items destroyed.
 */
static int inven_damage(inven_func typ, int perc)
{
	int i, j, k, amt, scan;

	object_type *o_ptr;

	char o_name[80];
	
	/* Hack -- Air only affects equipped hats */
	if (typ == set_air_destroy) scan = INVEN_TOTAL;
	else scan = INVEN_PACK;
	
	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < scan; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give this item slot a shot at death */
		/* Just a quick note to myself, this point *typ */
		/* Is actually where the set_xxx_destory is called */
		/* because it's chedked for the if() function below */
		/* Kind of surprising to me is all, should try to  */
		/* remember this trick. */
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

				/* Need special code for air */
				if (typ == set_air_destroy)
				{
						message_format(MSG_BELL, 0, "%sour %s (%c) %s blown %s!",
				           ((o_ptr->number > 1) ?
				            ((amt == o_ptr->number) ? "All of y" :
				             (amt > 1 ? "Some of y" : "One of y")) : "Y"),
				           o_name, index_to_label(i),
				           ((amt > 1) ? "were" : "was"),
				            ((i > 23) ? "off" : "away"));

						/* Reduce the charges of rods/wands */
						reduce_charges(o_ptr, amt);
		
						/* Take off equipment */
						/* This needs to be redone REDO */
						inven_drop(i, amt);
				}
				/* Message */
				else 
				{
					msg_format("%sour %s (%c) %s destroyed!",
			           ((o_ptr->number > 1) ?
			            ((amt == o_ptr->number) ? "All of y" :
			             (amt > 1 ? "Some of y" : "One of y")) : "Y"),
			           o_name, index_to_label(i),
			           ((amt > 1) ? "were" : "was"));

					/* Reduce the charges of rods/wands */
					reduce_charges(o_ptr, amt);
	
					/* Destroy "amt" items */
					inven_item_increase(i, -amt);
					inven_item_optimize(i);

				}
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
	object_type *o_ptr = NULL;

	u32b f1, f2, f3;

	char o_name[80];


	/* Pick a (possibly empty) inventory slot */
	switch (randint(6))
	{
		case 1: o_ptr = &inventory[INVEN_BODY]; break;
		case 2: o_ptr = &inventory[INVEN_LEG]; break;
		case 3: o_ptr = &inventory[INVEN_OUTER]; break;
		case 4: o_ptr = &inventory[INVEN_HANDS]; break;
		case 5: o_ptr = &inventory[INVEN_HEAD]; break;
		case 6: o_ptr = &inventory[INVEN_FEET]; break;
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return FALSE;

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return FALSE;


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Object resists - need to replace with % resist*/
	if (f2 & (TR2_IMMUNE))
	{
		msg_format("Your %s is unaffected!", o_name);

		return TRUE;
	}
	
	if (ignores_damage_p(o_ptr, RS_ACD))
	{
		msg_format("Your %s is unaffected!", o_name);

		return TRUE;
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
	return TRUE;
}

/*
 * Hurt the player with Fire
 */
bool fire_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_HEAT) inv -= 1;
	if (typ == GF_PLASMA) inv += randint(8);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_FIR]);
	
	if (dam <= 0) return FALSE;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	/* Inventory damage */
	if (!resist_effect(RS_FIR))
		inven_damage(set_fire_destroy, inv);

	return TRUE;
}

/*
 * Hurt the player with earth
 */
bool earth_dam(int dam, int typ, cptr kb_str)
{
	cptr p;
	int max_stun = 0;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_ROCK) inv -= 1; p = "rocks"; max_stun = 10;
	if (typ == GF_EARTH) p = "stones"; max_stun = 20;
	if (typ == GF_SHARDS) inv += randint(4); p = "boulders and shards"; max_stun = 40;
	if (inv < 1) inv = 1;
	
	
	/* Armor reduces damage of rock */
	dam = dam * 150 / (150 + p_ptr->ac + p_ptr->to_a);

	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_EAR]);	
	
	if (dam <= 0) return FALSE;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);
	
	if (resist_effect(RS_EAR))
	{
		if (!blind) 
			msg_format("You are crushed by some %s!", p);
		else 
			msg_format("You are crushed!");
		/* Be careful not to knock out the player immediately. */
		(void)set_stun(p_ptr->stun + ((dam / 2 > max_stun) ? max_stun : dam / 2));			

		/* Inventory damage */
		inven_damage(set_earth_destroy, inv);
	}
	return TRUE;
}

/*
 * Hurt the player with air
 */
bool air_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 3 : 8;

	/* adjust for damage type */
	if (typ == GF_GUST) inv -= 1;
	if (typ == GF_GALE) inv += randint(8);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_AIR]);
	
	if (dam <= 0) return FALSE;

	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	if (!resist_effect(RS_AIR)) 
	{
		/* RMG - following comment is about second randint() check  */
		/* Should probably split this out between the three wind skills */
		if (dam > randint(200) && !p_ptr->resist_confu && (randint(6) < 2))
		{
			msg_print("You are spun until dizzy!");
			(void)set_confused(p_ptr->confused + rand_range(2, 3));
		}
		/* Inventory damage */
		inven_damage(set_air_destroy, inv);
	}
	return TRUE;	
}

/*
 * Hurt the player with water
 */
bool water_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	int x;
	
	/* adjust for damage type -- water hates items. */
	if (typ == GF_RUST) inv += randint(3); x = 1;
	if (typ == GF_STEAM) x = 3;
	if (typ == GF_STORM) inv += randint(4); x = 4;
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_WTR]);
	
	if (dam <= 0) return FALSE;

	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	/* Sometimes, confuse the player. */
	if (!resist_effect(RS_WTR)) 
	{
		if ((randint(x) > 2) && (!p_ptr->resist_confu))
		{
			(void)set_confused(p_ptr->confused + rand_int(x + 6));
		}	

		/* Inventory damage */
		inven_damage(set_water_destroy, inv);
	}
	return TRUE;
}


/*
 * Hurt the player with electricty
 */
bool elec_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_SHOCK) inv -= 1;
	if (typ == GF_VOLT) inv += randint(3);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_ELC]);
	
	if (dam <= 0) return FALSE;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	if (!resist_effect(RS_ELC)) 
	{
		/* Inventory damage */
		inven_damage(set_elec_destroy, inv);
	}
	return TRUE;
}

/*
 * Hurt the player with ice
 */
bool ice_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_CHILL) inv -= 1;
	if (typ == GF_GLACIAL) inv += randint(3);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_ICE]);
	
	if (dam <= 0) return FALSE;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	if (!resist_effect(RS_ICE)) 
	{
		/* Inventory damage */
		inven_damage(set_ice_destroy, inv);
	}
	return TRUE;
}

/*
 * Hurt the player with acid
 */
bool acid_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_CORROSIVE) inv -= 1;
	if (typ == GF_LIQUESCE) inv += randint(3);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_ACD]);
	
	if (dam <= 0) return FALSE;
	
	/* If any armor gets hit, defend the player */
	/* XCCCX -- Need to extend this function */
	if (minus_ac()) dam = (dam + 1) / 2;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	if (!resist_effect(RS_ACD))
	{
		/* Inventory damage */
		inven_damage(set_acid_destroy, inv);
	}
	return TRUE;
}

/*
 * Hurt the player with poison
 */
bool poison_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	/* adjust for damage type */
	if (typ == GF_CAUSTIC) inv -= 1;
	if (typ == GF_CONTAGION) inv += randint(3);
	if (inv < 1) inv = 1;
	
	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_PSN]) / 8;
	
	if (dam <= 0) return FALSE;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);
	
	if (!resist_effect(RS_PSN)) ;
	{
		/* Poison the player. */
		set_poisoned(p_ptr->poisoned + randint(dam));

		/* Inventory damage */
		inven_damage(set_poison_destroy, inv);
	}

	return TRUE;
}

/*
 * Hurt the player with time
 */
void time_dam(int dam, int typ, cptr kb_str)
{
	/* Set up a percent chance for inventory reduction */
	/* int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3; */

	int severity, k;
	cptr act = NULL;
	bool automata;

	severity = k = 0;

	/* determine living state */
	if ((p_ptr->prace == RACE_AUTOMATA) || 
		(p_ptr->prace == RACE_STEAM_MECHA))
	{
		automata = TRUE;
	}
	else automata = FALSE;

	if (typ == GF_AGE) severity = randint(6);
	if (typ == GF_TIME) severity = randint(9);
	if (typ == GF_CHRONOS) severity = 3 + randint(10);

	/* Apply resistance */
	dam = apply_resistance(dam, p_ptr->res[RS_TIM]);
	
	if (dam <= 0) return;
	
	/* Take damage */
	take_hit(dam, kb_str, TRUE);

	if (resist_effect(RS_TIM)) return;
	else 
	{
		switch (severity)
		{
			case 1: case 2: case 3: case 4: case 5:
			{
				msg_print("You feel life has clocked back.");
				lose_exp((p_ptr->depth * 4) + (p_ptr->exp / 100) *
					MON_DRAIN_LIFE);
				break;
			}
	
			case 6: case 7: case 8: case 9:
			{

				if (automata)
				{
						automata_equipment_decay(1);
				}
				else
				{
						switch (randint(A_MAX))
						{
							case 1: k = A_MUS; act = "strong"; break;
							case 2: k = A_SCH; act = "bright"; break;
							case 3: k = A_EGO; act = "wise"; break;
							case 4: k = A_AGI; act = "agile"; break;
							case 5: k = A_VIG; act = "hale"; break;
							case 6: k = A_CHR; act = "beautiful"; break;
						}
			
						msg_format("You're not as %s as you used to be...", act);
			
						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						p_ptr->update |= (PU_BONUS);
						break;
				}
			}
	
			default:
			{
				msg_print("You're not as powerful as you used to be...");
	
				if (automata) 
				{
						automata_equipment_decay(randint(8));
				}
				else 
				{
						/* Drain all stats */
						for (k = 0; k < A_MAX; k++)
						{
							p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
							if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						}
						p_ptr->update |= (PU_BONUS);
				}
				break;
	
			}
		}
	}
}


/*
 * This function damages automata equipment in lieu of stat
 * loss.
 * 
 * Automata and steam mecha have difficulty with stat drain 
 * and so in order to have wounds damage automata (and other
 * stat drain effects that they may be subject to) we instead
 * damage their equipment. Badly. There is no way in game to 
 * restore pval of the equpiment.
 *
 * Consider adding a flag to the higher ego items to resist
 * or make them immune to this kind of destruction. XCCCX
 * 
 * The correct way to handle the below selection of traits on
 * items is to build a dynamic array which you fill with 
 * valid data items. You then select one of those items and
 * damage it. This is currently beyond my ability. So we have
 * the ever popular "I IS A REATARDED FOR LOOPZ, MAYBE I CAN
 * HAS A VALID RESULTS?"
 */
void automata_equipment_decay(int power)
{
	int i;
	int choice = 0;
	object_type *o_ptr = NULL;

	u32b f1, f2, f3;

	char o_name[80];
	
	bool ruin = FALSE;
	
	/* Pick a (possibly empty) automata body part slot */
	switch (randint(4))
	{
		case 1: o_ptr = &inventory[INVEN_BODY]; break;
		case 2: o_ptr = &inventory[INVEN_HEAD]; break;
		case 3: o_ptr = &inventory[INVEN_HANDS]; break;
		case 4: o_ptr = &inventory[INVEN_FEET]; break;
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return;

	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* select the type of damage */
	for (i = 0; i < 10; i++)
	{ 
		switch (randint(4))
		{
   		case 1: 
   		{
   			/* this is the armor class choice */
   			/* It will always be valid */
   			choice = 1; 
   			break;
   		}
   		case 2:
   		{
   			if (o_ptr->pval) choice = 2;
   			break;
   		}
   		case 3:
   		{
   			if (o_ptr->pval2) choice = 3;
   			break;
   		}
   		case 4:
   		{
   			if (o_ptr->pval3) choice = 4;
   			break;
   		}
   		default: choice = 0;
		}
		/* We have a valid damage type */				
		if (choice) break;			
	}
	
	/* Paranoia */
	if (!choice) return;
	
	switch (choice)
	{
		case 1:
		{	
			/* No damage left to be done */
			if (o_ptr->ac + o_ptr->to_a <= 0) break;

			/* double power */
			o_ptr->to_a -= (power * 2);
			
			/* Did we damage the armor? */
			ruin = TRUE;
			
		  /* need to insure that there is no to_a value */
		  if (o_ptr->to_a < 0)
		  {
		  	if (ABS(o_ptr->to_a) > o_ptr->ac)
		  	o_ptr->to_a = 0;
		  	o_ptr->to_a -= o_ptr->ac;
			}
		}	
	 	case 2:
		{	
			/* have a reasonable lower bound */
			if (o_ptr->pval <= -10) break;
			
			/* damage the item */
			o_ptr->pval -= power;

			/* Did we damage the armor? */
			ruin = TRUE;

			/* Paranoia */
			if (o_ptr->pval < -10) o_ptr->pval = -10;

		}	
	 	case 3:
		{	
			/* have a reasonable lower bound */
			if (o_ptr->pval2 <= -10) break;
			
			/* damage the item */
			o_ptr->pval2 -= power;

			/* Did we damage the armor? */
			ruin = TRUE;

			/* Paranoia */
			if (o_ptr->pval2 < -10) o_ptr->pval2 = -10;
		}	
	 	case 4:
		{	
			/* have a reasonable lower bound */
			if (o_ptr->pval3 <= -10) break;
			
			/* damage the item */
			o_ptr->pval3 -= power;

			/* Did we damage the armor? */
			ruin = TRUE;

			/* Paranoia */
			if (o_ptr->pval3 < -10) o_ptr->pval3 = -10;
		}	
		default: {}
		/* Nothing */
	}

	if (ruin)	message_format(MSG_BELL, 0, "Your %^s breaks!", o_name);	

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

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


	/* Unused parameter */
	(void)mode;

	/* Pick a random slot */
	switch (randint(8))
	{
		case 1: t = INVEN_WIELD; break;
		case 2: t = INVEN_GUN; break;
		case 3: t = INVEN_BODY; break;
		case 4: t = INVEN_OUTER; break;
		case 5: t = INVEN_LEG; break;
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


	/* Artifacts have 60% chance to resist */
	if (artifact_p(o_ptr) && (rand_int(100) < 60))
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
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Notice */
	return (TRUE);
}


/*
 * Apply Nexus
 */
static void apply_nexus(const monster_type *m_ptr)
{
	int max1, cur1, max2, cur2, ii, jj;
	int save, number;

	number = 6;

#if 0	
	/* Make automata safe from stat switching */
	if ((p_ptr->prace == RACE_AUTOMATA) || 
		(p_ptr->prace == RACE_STEAM_MECHA))
	{
		number = 6;
	}
	else
	{
		number = 7;
	}
#endif
	
	/* Get the "save" factor */
	save = p_ptr->skill_sav;

	switch (randint(number))
	{
		case 1: case 2: case 3:
		{
			teleport_player(200);
			break;
		}

		case 4: case 5: case 6:
		{
			teleport_player_to(m_ptr->fy, m_ptr->fx);
			break;
		}

	  case 7:
		{
			if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Teleport Level */
			teleport_player_level(FALSE);
			break;
		}

#if 0
		case 7:
		{
			if (rand_int(100) < save)
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
#endif		
	}
}



/*
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;

/*
 * Mega-Hack -- count number of monsters killed out of sight
 */
static int death_count;

/*
 * We are called from "project()" to alter terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * Hack -- We also "see" grids which are "memorized".
 *
 * Perhaps we should affect doors and/or walls.
 *
 * Handle special terrain.  Some terrain types that change the damage
 * done by various projections are marked (using the CAVE_TEMP flag) for
 * later processing.  This prevents a fire breath, for example, changing
 * floor to lava and then getting the damage bonuses that accrue to fire
 * spells on lava.  We use "dist" to keep terrain alteration under control.
 *
 * Currently most project types do not damage terrian - this will be handled
 * better in version .5 - Campbell
 *
 */
static bool project_f(int who, int y, int x, int dist, int dam, int typ)
{
	bool obvious = FALSE;
	
	/* Error msg handling */
	int a, b, c;
	a = who;
	b = dist;
	c = dam;
	
	/* Analyze the type */
	switch (typ)
	{
		/* Basic Damage - No effect */
		case GF_HURT: break;

		/* Fire - Should eventually have effects (Burn, Evaporate, and create Lava) */
		case GF_HEAT:
		case GF_FIRE:
		case GF_PLASMA:
		{
			/* Fire attacks currently do nothing to terrain.*/
			break;
		}
		/* Earth - Perhaps should create rubble? but doesn't do anything now. */
		case GF_ROCK:
		case GF_EARTH:
		case GF_SHARDS:
		{
			/* Earth attacks currently do nothing to terrain */
			break;
		}
		/* Wind - Small chance of drying up water (when we have it) */
		case GF_GUST:
		case GF_WIND:
		case GF_GALE:
		{
			/* Wind attacks currently do nothing to terrain. */
			break;
		}
		/* Water - Eventually make pools and solidify lava */
		case GF_RUST:
		case GF_STEAM:
		case GF_STORM:
		{
			/* Water attacks currently do nothing to terrain. */
			break;
		}
		/* Elec - ??? */
		case GF_SHOCK:
		case GF_ELEC:
		case GF_VOLT:
		{
			/* Electricity attacks currently do nothing */
			break;
		}
		/* Cold - Eventually solidify lava */
		case GF_CHILL:
		case GF_ICE:
		case GF_GLACIAL:
		{
			/* Cold attacks currently do nothing */
			break;
		}
		/* Acid - Can eat at walls.  See "project_t()".*/
		/* XCCCX don't quite want acid eating at walls quite yet */
		case GF_CORROSIVE:
		case GF_ACID:
		case GF_LIQUESCE:
		{
			/* Acid attacks currently do nothing */
			break;
		}
		/* Poison - ??? */
		case GF_CAUSTIC:
		case GF_POISON:
		case GF_CONTAGION:
		{
			/* Poison attacks will never likely affect terrain */
			break;
		}
		/* Time - Should have some random effects */
		case GF_AGE:
		case GF_TIME:
		case GF_CHRONOS:
		{
			/* Time attacks currently don't affect terrain */
			break;
		}
		/* Ether - ??? */
		case GF_VAPOR:
		case GF_ETHER:
		case GF_NEXUS:
		{
			/* Ether attacks (unsurprisingly) also don't affect terrain */
			break;
		}
		/* Sound - ??? */
		case GF_VIBE:
		case GF_SOUND:
		case GF_SONIC:
		{
			/* Are we sensing a pattern? */
			break;
		}
		/* Fields and Forces - Should have little effect on terrain */
		case GF_EMP:
		case GF_MAGNETIC:
		case GF_GRAVITY:
		case GF_WEAK_RAD:
		case GF_STRONG_RAD:
		{
			/* Here, let me tell you, we will probably affect terrain */
			break;
		}
		/* Light up the grid */
		case GF_GLOW:
		case GF_LIGHT:
		case GF_BRILLIANCE:
		{
			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Must not be blind */
				if (!p_ptr->blind)
				{
					/* Observe */
					obvious = TRUE;
				}

				/* Update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Darken the grid */
		case GF_DIM:
		case GF_DARK:
		case GF_TENEBROUS:
		{
			/* Turn off the light */
			cave_info[y][x] &= ~(CAVE_GLOW);

			/* Forget */
			cave_info[y][x] &= ~(CAVE_MARK);

			/* Re-learn sometimes */
			note_spot(y, x);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}
		
		/* Mental / Spiritual powers - should have little effect on terrain */
		case GF_FEAR:
		case GF_PSI:
		case GF_DOMINATION:
		case GF_STUN:
		case GF_TK:
		/* Force smashes walls */
		case GF_FORCE:
		case GF_CONFUSION:
		case GF_SPIRIT:
		case GF_ECTOPLASM:
		case GF_DISP_EVIL:
		case GF_HOLY_FIRE:
		{
			/* These will most likely never affect terrain */
			break;
		}
		/* Ranged attacks - should have little effect on terrain */
		case GF_WHIP:
		case GF_ARROW:
		case GF_BULLET:
		case GF_SHOT:
		case GF_PYRO_SHOT:
		{
			/* Only rocket and missle should affect terrain */
			break;
		}
			/* Only rocket and missle should affect terrain */
		case GF_ROCKET:
		case GF_MISSILE:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x)) break;

			/* Permanent walls and stores are immune. */
			if (cave_feat[y][x] >= FEAT_PERM_EXTRA) break;

			/* Granite */
			if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The wall is blasted down.");
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
				if ((cave_info[y][x] & (CAVE_MARK)))
				{
					msg_print("The vein melts.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}
			/* Quartz / Magma */
			else if (cave_feat[y][x] >= FEAT_MAGMA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The vein melts.");
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
					msg_print("The rubble scatters!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Destroy doors (and secret doors) */
			else if (cave_feat[y][x] >= FEAT_DOOR_HEAD)
			{
				/* Hack -- special message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The door shatters!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			break;

		}
		/* Charms - no effect */
		case GF_CHARM:
		{
			/* You can't charm terrain */
			break;
		}
		/* Kill Wall - Destroy walls, rubble, and doors */
		case GF_KILL_WALL:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x)) break;

			/* Trees are unaffected.  There are no trees. */
			/* if (cave_feat[y][x] == FEAT_TREE) break; */

			/* Permanent walls and stores are immune. */
			if (cave_feat[y][x] >= FEAT_PERM_EXTRA) break;

			/* Granite */
			if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The wall turns into mud.");
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
				if ((cave_info[y][x] & (CAVE_MARK)))
				{
					msg_print("The vein turns into mud.");
					msg_print("You have found something!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Place some gold */
				place_gold(y, x);
			}

			/* Quartz / Magma */
			else if (cave_feat[y][x] >= FEAT_MAGMA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The vein turns into mud.");
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
					msg_print("The rubble turns into mud.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Hack -- place an object. */
				if (randint(100) < 2)
				{
					/* Found something */
					if (player_can_see_bold(y, x))
					{
						msg_print("There was something buried in the rubble!");
						obvious = TRUE;
					}

					/* Place object */
					place_object(y, x, FALSE, FALSE);
				}
			}

			/* Destroy doors (and secret doors) */
			else if (cave_feat[y][x] >= FEAT_DOOR_HEAD)
			{
				/* Hack -- special message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					msg_print("The door turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			break;
		}
		/* Destroy Doors */
		case GF_KILL_DOOR:
		{
			int feat = cave_feat[y][x];

			/* Destroy all doors.  Traps are not affected */
			if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
			    (feat == FEAT_DOOR_HEAD) || (feat == FEAT_DOOR_TAIL))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

					/* Update the visuals */
					p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	
				}

				/* Forget the door */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			break;
		}
		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				place_closed_door(y, x);

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					obvious = TRUE;
				}
			}

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

			/* Locked doors are unlocked */
			else if ((cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01) &&
			          (cave_feat[y][x] <= FEAT_DOOR_HEAD + 0x07))
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
		/* Make a wall - strangely enough doesn't do anything yet */
		case GF_MAKE_WALL:
		{
			break;
		}
		/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* XXX - Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Create closed door */
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK)) obvious = TRUE;

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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
		/* Ignore most effects */
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
 * The "r" parameter is the "distance from ground zero".
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
	int factor;

	char o_name[120];

	u32b f1, f2, f3;


	/* "Who" is currently unused */
	who = 0;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;

		bool do_move = FALSE;
		bool do_kill = FALSE;
		cptr note_kill = NULL;
		bool do_change = FALSE;
		cptr note_change = NULL;

		int ny, nx;


		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Note multiple objects */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if (artifact_p(o_ptr)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{	
			/* Fire -- Flammable Objects burn */
			case GF_HEAT:
			{
				/* Heat has to be *hot* to burn things */
				if (hates_fire(o_ptr) && (dam > (rand_int(200) + 60)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (ignores_damage_p(o_ptr, RS_FIR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_FIRE:
			{
				/* Fire mostly likes to torch stuff */
				if (hates_fire(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (ignores_damage_p(o_ptr, RS_FIR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_PLASMA:
			{
				/* Plasma is bad */
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (ignores_damage_p(o_ptr, RS_FIR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_ROCK:
			{
				/* Rock is unlikely to break things */
				if (hates_earth(o_ptr) && (dam > rand_int(100) + 50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are crushed!" : " is crushed!");
					if (ignores_damage_p(o_ptr, RS_EAR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_EARTH:
			{
				/* mostly crushes stuff */
				if (hates_earth(o_ptr) && (dam > rand_int(40)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are crushed!" : " is crushed!");
					if (ignores_damage_p(o_ptr, RS_EAR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_SHARDS:
			{
				/* This is not pretty - likes to destroy things. */
				if (hates_earth(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are crushed!" : " is crushed!");
					if (ignores_damage_p(o_ptr, RS_EAR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_GUST:
			{
				/* Can blow light objects around */
				if (o_ptr->weight <= (dam / 2))
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 33, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_WIND:
			{
				/* Blows objects around */
				if (o_ptr->weight <= dam)
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 25, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_GALE:
			{
				/* Blows objects around a lot! */
				if (o_ptr->weight <= (dam * 2))
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 20, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_RUST:
			{
				/* Rust _loves_ to destroy things */
				if (hates_water(o_ptr) && dam > rand_int(30))
				{
					do_kill = TRUE;
					note_kill = (plural ? " is ruined by water!" : " are ruined by water!");
					if (ignores_damage_p(o_ptr, RS_WTR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_STEAM:
			{
				/* Steam is less likely to hurt items (does more damage) */
				if (hates_water(o_ptr) && dam > rand_int(70) + 30)
				{
					do_kill = TRUE;
					note_kill = (plural ? " is ruined by water!" : " are ruined by water!");
					if (ignores_damage_p(o_ptr, RS_WTR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_STORM:
			{
				/* Storm is not very likely to rust items (Does more damage) */
				if (hates_water(o_ptr) && dam > rand_int(100) + 50)
				{
					do_kill = TRUE;
					note_kill = (plural ? " is ruined by water!" : " are ruined by water!");
					if (ignores_damage_p(o_ptr, RS_WTR)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				/* Can blow light objects around */
				else if (o_ptr->weight <= (dam / 2))
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 33, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_SHOCK:
			{
				/* Not very likely to hurt items */
				if (hates_elec(o_ptr) && dam > (rand_int(200) + 200))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (ignores_damage_p(o_ptr, RS_ELC)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_ELEC:
			{
				if (hates_elec(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (ignores_damage_p(o_ptr, RS_ELC)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_VOLT:
			{
				/* Obliterates items */
				if (hates_elec(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (ignores_damage_p(o_ptr, RS_ELC)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			/* Ice - destroys potions and flasks */
			case GF_CHILL:
			{
				if (hates_ice(o_ptr) && dam > rand_int(200) + 100)
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (ignores_damage_p(o_ptr, RS_ICE)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_ICE:
			{
				if (hates_ice(o_ptr) && dam > rand_int(40))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (ignores_damage_p(o_ptr, RS_ICE)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_GLACIAL:
			{
				if (hates_ice(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (ignores_damage_p(o_ptr, RS_ICE)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			/* Acid -- Lots of things */
			case GF_CORROSIVE:
			{
				if (hates_acid(o_ptr) && dam > rand_int(100))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (ignores_damage_p(o_ptr, RS_ACD)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_ACID:
			{
				if (hates_acid(o_ptr) && dam > rand_int(50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (ignores_damage_p(o_ptr, RS_ACD)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_LIQUESCE:
			{
				if (hates_acid(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (ignores_damage_p(o_ptr, RS_ACD)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			/* Nothing yet */
			case GF_CAUSTIC:
			{
				if (hates_poison(o_ptr) && dam > rand_int(50) + 30)
				{
					do_kill = TRUE;
					note_kill = (plural ? " go bad!" : " goes bad!");
					if (ignores_damage_p(o_ptr, RS_PSN)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_POISON:
			{
				if (hates_poison(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " go bad!" : " goes bad!");
					if (ignores_damage_p(o_ptr, RS_PSN)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			case GF_CONTAGION:
			{
				if (hates_poison(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " go bad!" : " goes bad!");
					if (ignores_damage_p(o_ptr, RS_PSN)) ignore = TRUE;
					if (f2 & (TR2_IMMUNE)) ignore = TRUE;
				}
				break;
			}
			/* Time currently does nothing to objects */
			case GF_AGE:
			case GF_TIME:
			case GF_CHRONOS:
			{
				/* nothing */
				break;
			}
			/* perhaps these should carry a chance of do_change to the object */
			case GF_VAPOR:
			{
				/* can occasionally move objects around */
				if (o_ptr->weight <= (dam / 2) && (one_in_(3)))
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 33, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_ETHER:
			{
				/* Can occasionally change objects */
				if (one_in_(3))
				{
					do_change = TRUE;
					note_change = (plural ? " change!" : " changes!");
				}
				/* Can teleport light objects around */
				else if (o_ptr->weight <= (dam / 2))
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 33, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			case GF_NEXUS:
			{
				if (one_in_(2))
				{
					do_change = TRUE;
					note_change = (plural ? " change!" : " changes!");
				}
				/* Both do_move, and do_change shouldn't be */
				/* set on the same attack. . . */
				/* Can teleport objects around */
				else if (o_ptr->weight <= dam)
				{
					nx = 0;
					ny = 0;
					factor = dam - o_ptr->weight;
					if (factor < 0) factor = 0;

					scatter(&ny, &nx, y, x, 1 + (factor) / 25, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}
			
			case GF_VIBE:
			{
				if (hates_ice(o_ptr) && (dam > rand_int(80) + 10))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}
			case GF_SOUND:
			{
				if (hates_ice(o_ptr) && (dam > rand_int(40)))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}
			case GF_SONIC:
			{
				if (hates_ice(o_ptr))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}
			case GF_EMP:
			{
				if (hates_emp(o_ptr))
				{
					note_kill = (plural ? " stops working!" : " stop working!");
					do_kill = TRUE;
				}
				break;
			}
			case GF_MAGNETIC:
			/* Does nothing to items */
			case GF_GRAVITY:
			/* Does nothing to items */
			case GF_WEAK_RAD:
			case GF_STRONG_RAD:
			/* May eventually add a flag to items. */
			/* Unlock chests */
			case GF_KILL_TRAP:
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

			/* All (psi, tk, spirit) other kinds of projections have no effect */
			default:
			{
				break;
			}
		}


		/* Note that the object may not exist now  XXX XXX */
		/* Attempt to destroy the object (hack -- unless essence) */
		if (do_kill)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Some objects resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					object_desc(o_name, o_ptr, FALSE, 0);
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
					object_desc(o_name, o_ptr, FALSE, 0);
					msg_format("The %s%s", o_name, note_kill);
				}

				/* Blow the object up! */
				(void)item_smash_effect(0, y, x, o_ptr);
	
				/* Delete the object */
				delete_object_idx(this_o_idx);

				/* Redraw */
				lite_spot(y, x);
			}
		}

		/* Attempt to change the object.*/
		else if (do_change)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Some objects resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					object_desc(o_name, o_ptr, FALSE, 0);
					msg_format("The %s %s unaffected!",
						   o_name, (plural ? "are" : "is"));
				}
			}

			/* Change it (hack -- unless gold or essences) */
			else if (o_ptr->tval != TV_GOLD)
			{
				object_type *i_ptr;
				object_type forge;

				/* Get local object */
				i_ptr = &forge;

				/* Clear the record */
				WIPE(i_ptr, object_type);

				/* Require the same tval */
				required_tval = o_ptr->tval;

				/* Make a new object */
				make_object(i_ptr, FALSE, FALSE, TRUE);

				/* Cancel tval forcing */
				required_tval = 0;

				/* Require a valid new object */
				if (i_ptr->k_idx)
				{
					/* Describe original object if needed */
					if (o_ptr->marked && note_change)
					{
						object_desc(o_name, o_ptr, FALSE, 0);
						msg_format("The %s%s", o_name, note_change);
					}

					/* Delete the original object */
					delete_object_idx(this_o_idx);

					/* Drop the new object nearby */
					drop_near(i_ptr, -1, y, x);

					/* Redraw */
					lite_spot(y, x);
				}
			}
		}

		/* Move the object (hack -- unless essence) */
		if (do_move)
		{
			object_type *i_ptr;
			object_type forge;

			/* Get local object */
			i_ptr = &forge;

			/* Wipe the new object */
			object_wipe(i_ptr);

			object_copy(i_ptr, o_ptr);

			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Delete the object in its old location */
			delete_object_idx(this_o_idx);

			/* Redraw */
			lite_spot(y, x);

			/* Drop it near the new location */
			drop_near(i_ptr, -1, ny, nx);
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a projection causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a location, via
 * integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.
 *
 * Certain terrain types affect spells.  -LM- not yet -CCC-
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.   If can breath nether, then it resists it as well.
 *
 * XXX XXX - For monsters, Morgul-dark is the same as darkness.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
static bool project_m(int who, int y, int x, int dam, int typ, u32b flg)
{
	int tmp;

	monster_type *m_ptr;
	monster_type *n_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	s32b div, new_exp, new_exp_frac;

	cptr name;

	int old_sleep;

	/* Adjustment to damage caused by terrain, if applicable. */
	int terrain_adjustment = 0;
	
	/* variable to control spam from spur 'stun' attacks */
	int melee_message_saver = 0;
	
	/* Is the monster "seen"? */
	bool seen = FALSE;
	bool fully_seen = FALSE;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;
	
	/* Hack -- Can't delete m_idx before msg handleing */
	bool pet_kill = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;

	/* Is the monster friendly? */
	bool friendly = FALSE;
	
	/* Is the monster a pet? */
	bool pet = FALSE;

	/* Polymorph setting (true or false) */
	int do_poly = 0;

	/* Slow setting (amount to slow) */
	int do_slow = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;
	
	/* Stasis amount (amount to hold) */
	int do_stasis = 0;

	/* Mp drain amount (amount to drain) */
	int mp_drain = 0;
	
	/* Bribe toggle */
	int do_bribe = 0;

	/* Set up the saving throw for backlash from Psi powers */
	int save;
	
	/* Set up things that can increase the effectiveness of the attack */
	int willpower;

	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	cptr note = NULL;

	cptr note_dies;

	bool fear = FALSE;
		
	/* Get the "willpower" factor */
	willpower = 0;
	if (p_ptr->skills[SK_TMPR_WILL].skill_max > 0) willpower = p_ptr->skills[SK_TMPR_WILL].skill_rank;
	if (p_ptr->skills[SK_HARD_WILL].skill_max > 0) willpower += p_ptr->skills[SK_HARD_WILL].skill_rank;
	if (p_ptr->skills[SK_IRON_WILL].skill_max > 0) willpower += p_ptr->skills[SK_IRON_WILL].skill_rank;
	
	
	/* Insure good values */
	save = p_ptr->skill_sav;

	/* No monster here */
	if (!(cave_m_idx[y][x] > 0)) return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who) return (FALSE);

	/* Obtain monster info */
	m_ptr = &m_list[cave_m_idx[y][x]];
	n_ptr = &m_list[who];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	name = (r_name + r_ptr->name);

	/* Paranoia -- This monster is already dead */
	if (m_ptr->hp < 0) return (FALSE);

	/* Hack -- Monster race can be immune */
	/* if (project_immune)*/
	/* {*/
	/* 	   Skip monsters with the given racial index */
	/* 	if (project_immune == m_ptr->r_idx) return (FALSE);*/
	/* }*/

#if 0 /* I don't think I'm doing this yet. */

	/* Optionally - Only affect monsters with a specified flag */
	if (p_ptr->proj_mon_flags)
	{
		int i;
		bool hit = FALSE;

		/* Scan the flag set */
		for (i = 0; i < 32; i++)
		{
			/* We are looking for monsters with this flag */
			if (p_ptr->proj_mon_flags & (1L << i))
			{
				/* Monster has this flag */
				if (r_ptr->flags3 & (1L << i))
				{
					if (mon_fully_visible(m_ptr))
					{
						l_ptr->flags3 |= (1L << i);
					}

					hit = TRUE;
				}
			}
		}

		/* Monster has none of the flags for which we are looking */
		if (!hit) return (FALSE);
	}
#endif

	/* Get visibility */
	if (m_ptr->ml)
	{
		seen = TRUE;
		if (mon_fully_visible(m_ptr)) fully_seen = TRUE;
	}

	/* Some monsters are great at not getting hurt!  -EZ- */
	if (
		((r_ptr->flags2 & (RF2_IMPENT)) ||
		 (r_ptr->flags2 & (RF2_GASEOUS))) &&
		(!m_ptr->csleep) &&
	    (!m_ptr->confused)
	    )
	{
		/* Area-effect and jumping spells cannot be dodged */
		if (!(flg & (PROJECT_ARC | PROJECT_STAR | PROJECT_JUMP |
		             PROJECT_BOOM)))
		{
			/* Allow dodging */
			if ((rand_int(5 + m_ptr->cdis) >= 2) &&
				((typ == GF_ARROW) || (typ == GF_BULLET)  || (typ == GF_SHOT)))
			{
				if (fully_seen)
				{
					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					if (r_ptr->flags2 & (RF2_IMPENT)) msg_format("The missile bounces off %^s!", name);
					else msg_format("The missile passes right through %^s!", name);
	
					/* Learn that monster is bad-ass */
					if (r_ptr->flags2 & (RF2_IMPENT)) l_ptr->r_flags2 |= (RF2_IMPENT);
					else if (r_ptr->flags2 & (RF2_GASEOUS)) l_ptr->r_flags2 |= (RF2_GASEOUS);
				}

				/* Missed! */
				return (TRUE);
			}
		}
	}

	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Most monsters "die" */
	note_dies = " dies.";

	/* Some monsters get "destroyed" */
	if (monster_nonliving(r_ptr))
	{
		note_dies = " is destroyed.";
	}


	/* Determine if terrain is capable of adjusting physical damage. */
	switch (cave_feat[y][x])
	{
			default:
			/* Waiting for new terrain types */
			break;
	}


	/* Remember old sleep */
	old_sleep = m_ptr->csleep;

	/* Monster wakes up */
	m_ptr->csleep = 0;

	/* Monster goes active */
	m_ptr->mflag |= (MFLAG_ACTV);


	/* Analyze the damage type, determine effects */
	switch (typ)
	{
	
		/* Pure hurt */
		case GF_HURT:
		{
			/* Not affected by terrain  XXX */

			/* No resists, adjusts, etc. */

			break;
		}
		case GF_HEAT:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				note = " is immune!";
				dam = div_round(dam, 9);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_FIRE);
				}
			}
			else if (r_ptr->flags8 & (RF8_VUN_FIRE))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
			}
			
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* Heat doesn't do any extra damage to monsters */
		}
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				note = " resists a lot.";
				dam = div_round(dam, 7);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_FIRE);
				}
			}
			else if ((r_ptr->flags3 & (RF3_PLANT)) ||
					 (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				dam += (dam / 4);
				/*if (fully_seen) know nothing */
				/* Plants and undead just take extra damage from fire */
			}

			if (r_ptr->flags8 & (RF8_VUN_FIRE))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
			
		}
		case GF_PLASMA:
		{
			/* Affected by terrain. - Not yet */
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				note = " resists.";
				/* No one resists plasma well */
				dam = div_round(dam, 4);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_FIRE);
				}
			}
			else if ((r_ptr->flags3 & (RF3_PLANT)) ||
					 (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				dam += (dam / 4);
				note = " is hit hard.";
				/*if (fully_seen) know nothing */
				/* Plants and undead just take extra damage from fire */
			}

			if (r_ptr->flags8 & (RF8_VUN_FIRE))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		/* Boulders -- damage, possibly stunning.  Can miss. */
		case GF_ROCK:
		{
			/* We don't hurt creatures immune to earth, 	*/
			/* ones with earth auras (spines) or creatures	*/
			/* that can passwall (they are insubstantial)   */
			/* Some creatures are tough */
			if ((r_ptr->flags2 & (RF2_IMPENT)) &&
					 (one_in_(2)))
			{
				note = " is unaffected!";
				dam = 0;
				if (fully_seen && (r_ptr->flags2 & (RF2_IMPENT))) 
				{
					l_ptr->r_flags3 |= (RF2_IMPENT);
				}
			}
			else if ((r_ptr->flags3 & (RF3_IM_EARTH)) ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " is immune!";
				dam = div_round(dam, 9);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_EARTH))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_EARTH);
				}
			}
			else if (r_ptr->flags2 & (RF2_IMPENT))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}

			if (r_ptr->flags8 & (RF8_VUN_EARTH))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_EARTH);
			}

			/* Can rarely stun monsters - impentatrable monsters never stun. */
			if ((dam > 45) && (randint(6) < 2) && 
				!(r_ptr->flags2 & (RF2_IMPENT)))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			if (seen) obvious = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;

		}
		case GF_EARTH:
		{
			/* Some creatures are tough. */			
			if ((r_ptr->flags2 & (RF2_IMPENT)) &&
					 (one_in_(3)))
			{
				note = " is unaffected!";
				dam = 0;
				if (fully_seen && (r_ptr->flags2 & (RF2_IMPENT))) 
				{
					l_ptr->r_flags3 |= (RF2_IMPENT);
				}
			}
			else if ((r_ptr->flags3 & (RF3_IM_EARTH)) ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " resists a lot.";
				dam = div_round(dam, 7);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_EARTH))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_EARTH);
				}
			}
			else if (r_ptr->flags2 & (RF2_IMPENT))
			{
				note = " resists.";
				dam = div_round(dam, 4);
			}

			if (r_ptr->flags8 & (RF8_VUN_EARTH))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_EARTH);
			}

			/* Can stun monsters. - impentatrable monsters never stun.  */
			if ((dam > 25) && (randint(3) < 2) && 
				!(r_ptr->flags2 & (RF2_IMPENT)))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			if (seen) obvious = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;

		}
		case GF_SHARDS:
		{
			/* Some creatures are tough. */			
			if ((r_ptr->flags2 & (RF2_IMPENT)) &&
					 (one_in_(5)))
			{
				note = " is unaffected!";
				dam = 0;
				if (fully_seen && (r_ptr->flags2 & (RF2_IMPENT))) 
				{
					l_ptr->r_flags3 |= (RF2_IMPENT);
				}
			}
			else if ((r_ptr->flags3 & (RF3_IM_EARTH)) ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " resists.";
				/* No one resists shards well */
				dam = div_round(dam, 4);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_EARTH))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_EARTH);
				}
			}
			else if (r_ptr->flags2 & (RF2_IMPENT))
			{
				note = " resists a little.";
				dam = div_round(dam, 2);
			}

			if (r_ptr->flags8 & (RF8_VUN_EARTH))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_EARTH);
			}

			/* should stun monsters. - impentatrable monsters never stun.  */
			if ((dam > 200) && 
				!(r_ptr->flags2 & (RF2_IMPENT)))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			if (seen) obvious = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;

		}
		case GF_GUST:
		{
			/* Affected by terrain. --not yet. */
			/* dam += terrain_adjustment; */ 

			if (seen) obvious = TRUE;

			/* Wind doesn't affect anything with passwall, a wind aura */
			/* or anything that's IM_AIR, a plant or heavy very much */
			if ((r_ptr->flags3 & (RF3_IM_AIR)) ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " resists.";
				dam = div_round(dam, 9);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_AIR))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_AIR);
				}
			}
			
			if (r_ptr->flags8 & (RF8_VUN_AIR))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_AIR);
			}
			
			if (r_ptr->flags2 & (RF2_GASEOUS))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags2 |= (RF2_GASEOUS);
			}

			/* Mark grid for later processing if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_WIND:
		{
			/* Affected by terrain. --not yet. */
			/* dam += terrain_adjustment; */ 

			/* Wind doesn't affect anything with passwall, a wind aura */
			/* or anything that's IM_AIR  very much */
			if ((r_ptr->flags3 & (RF3_IM_AIR)) ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " resists.";
				dam = div_round(dam, 7);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_AIR))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_AIR);
				}
				break;
			}

			if (r_ptr->flags8 & (RF8_VUN_AIR))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_AIR);
			}

			if (r_ptr->flags2 & (RF2_GASEOUS))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags2 |= (RF2_GASEOUS);
			}
			
			/* Mark grid for later processing if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_GALE:
		{
			/* Affected by terrain. --not yet. */
			/* dam += terrain_adjustment; */ 

			/* Wind doesn't affect anything with passwall, a wind aura */
			/* or anything that's IM_AIR  very much */
			if ((r_ptr->flags3 & (RF3_IM_AIR))  ||
				(r_ptr->flags2 & (RF2_PASS_WALL)))
			{
				note = " resists.";
				dam = div_round(dam, 4);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_AIR))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_AIR);
				}
				break;
			}

			if (r_ptr->flags8 & (RF8_VUN_AIR))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_AIR);
			}

			if (r_ptr->flags2 & (RF2_GASEOUS))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags2 |= (RF2_GASEOUS);
			}

			/* Mark grid for later processing if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_RUST:
		{
			/* Water resistors and plants are immune to rust */
			/* (weak water damage) */
			if (seen) obvious = TRUE;

			if ((r_ptr->flags3 & (RF3_IM_WATER)) ||
				(r_ptr->flags3 & (RF3_PLANT)))
			{
				note = " is immune.";
				dam = 0;
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_WATER))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_WATER);
				}
			}
			
			/* Non mechanical monsters have damaged reduced */
			else if (!(r_ptr->flags3 & (RF3_AUTOMATA)))
			{
				note = " is practically unharmed.";
				dam = div_round(dam, 15);
			}

			if (r_ptr->flags8 & (RF8_VUN_WATER))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_WATER);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_STEAM:
		{
			/* Water resistors and plants are immune to rust */
			/* (weak water damage) */
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_WATER)) ||
				(r_ptr->flags3 & (RF3_PLANT)))
			{
				note = " is immune.";
				dam = 0;
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_WATER))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_WATER);
				}
			}
			
			/* Non mechanical monsters have damaged reduced */
			else if (!(r_ptr->flags3 & (RF3_AUTOMATA)))
			{
				note = " is hit by scalding steam.";
				dam = div_round(dam, 2);
			}

			if (r_ptr->flags8 & (RF8_VUN_WATER))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_WATER);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_STORM:
		{
			/* Water resistors and plants are immune to rust */
			/* (weak water damage) */
			if (seen) obvious = TRUE;

			if ((r_ptr->flags3 & (RF3_IM_WATER)))
			{
				note = " is immune.";
				dam = 0;
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_WATER))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_WATER);
				}
			}
			
			/* Rainstorms are bad for equipment */
			if (r_ptr->flags3 & (RF3_AUTOMATA))
			{
				dam += (dam / 4);
			}
			
			if (r_ptr->flags8 & (RF8_VUN_WATER))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_WATER);
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 50) && (randint(2) < 2))
				do_stun = randint(dam > 240 ? 20 : dam / 12);

			/* Can confuse, if monster can be confused. */
			if ((dam > 20) && randint(3) < 2)
			{
				/* Get confused later */
				do_conf = rand_range(15, 15 + (dam > 200 ? 20 : dam / 10));
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_SHOCK:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ELEC);
			}

			if (r_ptr->flags8 & (RF8_VUN_ELEC))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ELEC);
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 10) && (randint(6) < 2))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_ELEC:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ELEC);
			}

			if (r_ptr->flags8 & (RF8_VUN_ELEC))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ELEC);
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 20) && (randint(4) < 2))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_VOLT:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists a little.";
				dam = div_round(dam, 3);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ELEC);
			}

			if (r_ptr->flags8 & (RF8_VUN_ELEC))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ELEC);
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 50) && (randint(2) < 2))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}


		case GF_CHILL:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ICE))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ICE);
			}

			if (r_ptr->flags8 & (RF8_VUN_ICE))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ICE);
			}

			/* Can put cold blooded animals or beastmen, */
			/* who are not uniques to sleep 			 */
			if ((r_ptr->flags2 & (RF2_COLD_BLOOD)) &&
			    (randint(dam) > (8 * r_ptr->level)) &&
			    !(r_ptr->flags1 & (RF1_UNIQUE)) &&
			    ( (r_ptr->flags3 & (RF3_ANIMAL)) ||
			     (r_ptr->flags3 & (RF3_BEASTMAN))||
			     (r_ptr->flags3 & (RF3_DINOSAUR)) ) )
			{
				do_sleep = rand_range(30, 50);
				if (fully_seen) l_ptr->r_flags2 |= (RF2_COLD_BLOOD);
			}

			/* Slows down automata (cools steam) */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) &&
				(randint(dam) > (4 * r_ptr->level)))
			{
				do_slow = TRUE;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_ICE:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ICE))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ICE);
			}

			if (r_ptr->flags8 & (RF8_VUN_ICE))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ICE);
			}

			/* Can put cold blooded animals or beastmen, */
			/* who are not uniques to sleep 			 */
			if ((r_ptr->flags2 & (RF2_COLD_BLOOD)) &&
			    (randint(dam) > (4 * r_ptr->level)) &&
			    !(r_ptr->flags1 & (RF1_UNIQUE)) &&
			    ((r_ptr->flags3 & (RF3_ANIMAL)) ||
			     (r_ptr->flags3 & (RF3_BEASTMAN)) ||
			     (r_ptr->flags3 & (RF3_DINOSAUR)) ) )
			{
				do_sleep = rand_range(50, 80);
				if (fully_seen) l_ptr->r_flags2 |= (RF2_COLD_BLOOD);
			}

			/* Slows down automata (cools steam) */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) &&
				(randint(dam) > (2 * r_ptr->level)))
			{
				do_slow = TRUE;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_GLACIAL:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ICE))
			{
				note = " resists a little.";
				dam = div_round(dam, 3);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ICE);
			}
			if (r_ptr->flags8 & (RF8_VUN_ICE))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ICE);
			}

			/* Can put cold blooded animals or beastmen, */
			/* who are not uniques to sleep 			 */
			if ((r_ptr->flags2 & (RF2_COLD_BLOOD)) &&
			    (randint(dam) > (2 * r_ptr->level)) &&
			    !(r_ptr->flags1 & (RF1_UNIQUE)) &&
			    ( (r_ptr->flags3 & (RF3_ANIMAL)) ||
			     (r_ptr->flags3 & (RF3_BEASTMAN)) ||
			     (r_ptr->flags3 & (RF3_DINOSAUR)) ) )
			{
				do_sleep = rand_range(30, 50);
				if (fully_seen) l_ptr->r_flags2 |= (RF2_COLD_BLOOD);
			}
			/* Slows down automata (cools steam) */
			if (r_ptr->flags3 & (RF3_AUTOMATA)) do_slow = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_CORROSIVE:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ACID);
			}

			if (r_ptr->flags8 & (RF8_VUN_ACID))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ACID);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_ACID:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ACID);
			}

			if (r_ptr->flags8 & (RF8_VUN_ACID))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ACID);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_LIQUESCE:
		{
			/* Affected by terrain. -- not yet*/
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = " resists a little.";
				dam = div_round(dam, 3);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ACID);
			}

			if (r_ptr->flags8 & (RF8_VUN_ACID))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ACID);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_CAUSTIC:
		{
			/* Slightly affected by terrain. -- not yet*/
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			else if (r_ptr->flags3 & (RF3_ALIEN))
			{
				note = " is hit hard!";
				dam += dam/6;
			}

			if (r_ptr->flags8 & (RF8_VUN_POIS))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_POIS);
			}

			/* Automata, constructs, elementals, and undead */
			/* are unaffected by disease */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) || 
				(r_ptr->flags3 & (RF3_ELEMENTAL)) || 
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is unaffected!";
				dam = 0;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_POISON:
		{
			/* Slightly affected by terrain. -- not yet*/
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			else if (r_ptr->flags3 & (RF3_ALIEN))
			{
				note = " is hit hard!";
				dam += dam/4;
			}

			if (r_ptr->flags8 & (RF8_VUN_POIS))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_POIS);
			}

			/* Automata, constructs, elementals, and undead */
			/* are unaffected by disease */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) || 
				(r_ptr->flags3 & (RF3_ELEMENTAL)) || 
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is unaffected!";
				dam = 0;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_CONTAGION:
		{
			/* Slightly affected by terrain. -- not yet*/
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists a little.";
				dam = div_round(dam, 3);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			else if (r_ptr->flags3 & (RF3_ALIEN))
			{
				note = " is hit hard!";
				dam += dam/3;
			}

			if (r_ptr->flags8 & (RF8_VUN_POIS))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_POIS);
			}

			/* Automata, constructs, elementals, and undead */
			/* are unaffected by disease */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) || 
				(r_ptr->flags3 & (RF3_ELEMENTAL)) || 
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is unaffected!";
				dam = 0;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		
		case GF_AGE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_TIME))
			{
				note = " resists a lot.";
				dam = div_round(dam, 6);
			}
			/* mechanical monsters have damaged reduced */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) ||
				(r_ptr->flags3 & (RF3_ELEMENTAL)) ||
				(r_ptr->flags3 & (RF3_DEMON)) ||
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is almost immune.";
				dam = div_round(dam, 9);
			}

			if (r_ptr->flags8 & (RF8_VUN_TIME))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_TIME);
			}

			if (r_ptr->flags3 & (RF3_PLANT))
			{
				/* Good stuff happens to plants */
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_TIME))
			{
				note = " resists.";
				dam = div_round(dam, 4);
			}
			/* mechanical monsters have damaged reduced */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) ||
				(r_ptr->flags3 & (RF3_ELEMENTAL)) ||
				(r_ptr->flags3 & (RF3_DEMON)) ||
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is almost immune.";
				dam = div_round(dam, 9);
			}

			if (r_ptr->flags8 & (RF8_VUN_TIME))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_TIME);
			}

			if (r_ptr->flags3 & (RF3_PLANT))
			{
				/* Good stuff happens to plants */
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_CHRONOS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_TIME))
			{
				note = " resists a little.";
				dam = div_round(dam, 2);
			}
			/* mechanical monsters have damaged reduced */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) ||
				(r_ptr->flags3 & (RF3_ELEMENTAL)) ||
				(r_ptr->flags3 & (RF3_DEMON)) ||
				(r_ptr->flags3 & (RF3_UNDEAD)))
			{
				note = " is almost immune.";
				dam = div_round(dam, 9);
			}

			if (r_ptr->flags8 & (RF8_VUN_TIME))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_TIME);
			}

			if (r_ptr->flags3 & (RF3_PLANT))
			{
				/* Good stuff happens to plants */
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_VAPOR:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ETHER))
			{
				note = " resists a lot.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ETHER);

				break;
			}

			if (r_ptr->flags8 & (RF8_VUN_ETHER))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ETHER);
			}

			/* Mark grid for later processing, if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_ETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ETHER))
			{
				note = " resists.";
				dam = div_round(dam, 4);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ETHER);

				break;
			}

			if (r_ptr->flags8 & (RF8_VUN_ETHER))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ETHER);
			}

			/* Mark grid for later processing, if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ETHER))
			{
				note = " resists a little.";
				dam = div_round(dam, 2);
				if (fully_seen) l_ptr->r_flags3 |= (RF3_IM_ETHER);

				break;
			}

			if (r_ptr->flags8 & (RF8_VUN_ETHER))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_ETHER);
			}

			/* Mark grid for later processing, if not resisted. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_VIBE:
		{
			/* Slightly affected by terrain.-- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;

			do_stun = randint(dam > 300 ? 30 : dam / 10);

			if (r_ptr->flags3 & (RF3_IM_SOUND))
			{
				note = " resists a lot.";
				dam = div_round(dam, 6);
			}

			if (r_ptr->flags8 & (RF8_VUN_SOUND))
			{
				note = " is blasted!";
				dam *= 3;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_SOUND);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_SOUND:
		{
			/* Slightly affected by terrain.-- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;

			do_stun = randint(dam > 300 ? 30 : dam / 10);

			if (r_ptr->flags3 & (RF3_IM_SOUND))
			{
				note = " resists.";
				dam = div_round(dam, 4);
			}

			if (r_ptr->flags8 & (RF8_VUN_SOUND))
			{
				note = " is blasted!";
				dam *= 6;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_SOUND);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_SONIC:
		{
			/* Slightly affected by terrain.-- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;

			do_stun = randint(dam > 300 ? 30 : dam / 10);

			if (r_ptr->flags3 & (RF3_IM_SOUND))
			{
				note = " resists a little.";
				dam = div_round(dam, 2);
			}

			if (r_ptr->flags8 & (RF8_VUN_SOUND))
			{
				note = " is blasted!";
				dam *= 9;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_SOUND);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_EMP:
		{
			/* Is a machine */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)))
			{
				if (seen) obvious = TRUE;
				note = " Shuts down!";
				note_dies = " Explodes!";
				do_stasis = dam;
				/* Take some damage from the EMP blast */
				dam = dam / 4;
			}
			else
			{
				/* No damage */
				dam = 0;
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_MAGNETIC:
		{
			/* I really need to get this going */
			/* This doesn't do anything to monsters. Will affect metal floor */
			break;
		}

		case GF_GRAVITY:
		{
			if (seen) obvious = TRUE;

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_WEAK_RAD:
		{
			if (seen) obvious = TRUE;

			if (randint(6) == 1) do_poly = TRUE;
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_STRONG_RAD:
		{
			if (seen) obvious = TRUE;

			if (randint(2) == 1) do_poly = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_GLOW:
		{
			/* Slightly affected by terrain. -- not yet */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_LIGHT))
			{
				if (fully_seen) l_ptr->r_flags3 |= (RF3_HURT_LIGHT);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam += (dam / 4);
			}

			/* Not much is hurt by light */
			else dam = div_round(dam, 4);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;
			
			break;

		}	
		case GF_LIGHT:
		{
			/* Slightly affected by terrain. -- not yet */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_LIGHT))
			{
				if (fully_seen) l_ptr->r_flags3 |= (RF3_HURT_LIGHT);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam += (dam / 4);
			}
			/* Not much is hurt by light */
			else dam = div_round(dam, 3);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;
			break;
		}	
		case GF_BRILLIANCE:
		{
			/* Slightly affected by terrain. -- not yet */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_LIGHT))
			{
				if (fully_seen) l_ptr->r_flags3 |= (RF3_HURT_LIGHT);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam += (dam / 2);
			}
			/* Not much is hurt by light */
			else dam = div_round(dam, 2);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_DIM:
		{
			/* Slightly affected by terrain. -- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			/* Doesn't do much damage */
			dam = div_round(dam, 4);
			
			/* determine resistance */
			/* Uniques rarely get confused by other monsters */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || 
				(r_ptr->flags3 & (RF3_PLANT)))
				tmp = (r_ptr->level * 2) + 120;
			/* It is unlikely that undead, automata and constructs are */
			/* bothered by the dark */
			else if ((r_ptr->flags3 & (RF3_UNDEAD)) ||
					 (r_ptr->flags3 & (RF3_AUTOMATA)) ||
					 (r_ptr->flags3 & (RF3_CONSTRUCT)))
				tmp = r_ptr->level + 85;
			else
				tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (!dam)				note = NULL;
			else if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists confusion!";

			/* If it fails, become confused. */
			else do_conf = rand_range(20, 20 + (dam > 80 ? 20 : dam / 4));

			/* Notice resistance only if character is the caster */
			if (who >= 0) 
				note = NULL;

			break;
		}
		case GF_DARK:
		{
			/* Slightly affected by terrain. -- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			/* Doesn't do much damage */
			dam = div_round(dam, 3);
			
			/* determine resistance */
			/* Uniques rarely get confused by other monsters */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || 
				(r_ptr->flags3 & (RF3_PLANT)))
				tmp = (2 * r_ptr->level) + 120;
			/* It is unlikely that undead, automata and constructs are */
			/* bothered by the dark */
			else if ((r_ptr->flags3 & (RF3_UNDEAD)) ||
					 (r_ptr->flags3 & (RF3_AUTOMATA)) ||
					 (r_ptr->flags3 & (RF3_CONSTRUCT)))
				tmp = r_ptr->level + 85;
			else
				tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (!dam)				note = NULL;
			else if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists confusion!";

			/* If it fails, become confused. */
			else do_conf = rand_range(20, 20 + (dam > 80 ? 20 : dam / 4));

			/* Notice resistance only if character is the caster */
			if (who >= 0) 
				note = NULL;

			break;
		}
		case GF_TENEBROUS:
		{
			/* Slightly affected by terrain. -- not yet. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			
			/* Doesn't do much damage */
			dam = div_round(dam, 2);
			
			/* determine resistance */
			/* Uniques rarely get confused by other monsters */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || 
				(r_ptr->flags3 & (RF3_PLANT)))
				tmp = (2 * r_ptr->level) + 120;
			/* It is unlikely that undead, automata and constructs are */
			/* bothered by the dark */
			else if ((r_ptr->flags3 & (RF3_UNDEAD)) ||
					 (r_ptr->flags3 & (RF3_AUTOMATA)) ||
					 (r_ptr->flags3 & (RF3_CONSTRUCT)))
				tmp = r_ptr->level + 85;
			else
				tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (!dam)				note = NULL;
			else if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists confusion!";

			/* If it fails, become confused. */
			else do_conf = rand_range(20, 20 + (dam > 80 ? 20 : dam / 4));

			/* Notice resistance only if character is the caster */
			if (who >= 0) 
				note = NULL;

			break;
		}
		case GF_FEAR:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Apply some fear */
			/* Why all the fucked up math everywhere? */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			/* This is _normal_ fear - doesn't affect some things. */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (r_ptr->flags3 & (RF3_AUTOMATA)) ||
			    (r_ptr->flags3 & (RF3_CONSTRUCT)) ||
			    (r_ptr->flags3 & (RF3_PLANT)) ||
			    ((r_ptr->level * 2) > randint(dam)))
			{
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
				do_fear = 0;
			}
			
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No "real" damage */
			dam = 0;
			break;
		}
		case GF_PSI:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
				(r_ptr->flags3 & RF3_PLANT) ||
				(r_ptr->flags3 & RF3_AUTOMATA) ||
				(r_ptr->flags3 & RF3_CONSTRUCT) ||
				((r_ptr->level * 2) > randint(dam)))
			{
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
				/* no effect */
				dam = 0;
			}

			/*
			 * Powerful demons & undead can turn a mindcrafter's
			 * attacks back on them
			 */
			else if (((r_ptr->flags3 & RF3_UNDEAD) ||
					(r_ptr->flags3 & RF3_DEMON) ||
					(r_ptr->flags3 & RF3_ELEMENTAL) ||
					(r_ptr->flags2 & RF2_WEIRD_MIND))
					&& /* Saving throw */
					(((r_ptr->level * 2) > willpower) && (one_in_(5)))
					&& (who == -1))
			{
				note = NULL;
				msg_format("%^s%s corrupted mind backlashes your attack!",
						   m_name, (seen ? "'s" : "s"));
				/* Saving throw */
				if (randint(100) < save) msg_print("You resist the effects!");
				else
				{
					/* Confuse, stun, terrify */
					switch (randint(4))
					{
						case 1: (void)set_stun(p_ptr->stun + dam / 2); break;
						case 2: (void)set_confused(p_ptr->confused + dam / 2); break;
						default:
						{
							if (p_ptr->resist_fear) note = " is unaffected.";
							else (void)set_afraid(p_ptr->afraid + dam);							
						}
					}
				}
			}
			/* perhaps need to put something in here for monsters to resist */
			/* monster attacks. */
			else
			{
				switch (randint(4))
				{
					case 1: do_stun = dam / 2; if (who == -1) dam += willpower; break;
					case 2: do_conf = dam / 2; if (who == -1) dam += willpower;  break;
					default: do_fear = dam; if (who == -1) dam += willpower;
				}
				
			}
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;
			
			/* Psi doesn't do full damage */
			if (dam) dam = ((dam * 3) / 2);
			if (dam) dam += willpower;
			break;
		}

		case GF_DOMINATION:
		{
			if (!is_hostile(m_ptr)) break;
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
				(r_ptr->flags3 & RF3_PLANT) ||
				(r_ptr->flags3 & RF3_AUTOMATA) ||
				(r_ptr->flags3 & RF3_CONSTRUCT) ||
				((r_ptr->level * 2) > randint(dam * 2)))
			{
				/* No obvious effect */
				note = " is unaffected!";
				obvious = FALSE;
				/* Resist */
				dam = 0;
			}

			/*
			 * Powerful demons & undead can turn a mindcrafter's
			 * attacks back on them
			 */
			else if (((r_ptr->flags3 & RF3_UNDEAD) ||
					(r_ptr->flags3 & RF3_DEMON) ||
					(r_ptr->flags3 & RF3_ELEMENTAL) ||
					(r_ptr->flags2 & RF2_WEIRD_MIND))
					&& /* Saving throw */
					(((r_ptr->level * 2)> willpower) && (one_in_(2)))
					&& (who == -1))
			{
				note = NULL;
				msg_format("%^s%s corrupted mind backlashes your attack!",
				    m_name, (seen ? "'s" : "s"));
				/* Saving throw */
				if (rand_int(100) < save) msg_print("You resist the effects!");
				else
				{
				/* Confuse, stun, terrify */
					switch (randint(4))
					{
						case 1: (void)set_stun(p_ptr->stun + dam / 2); break;
						case 2: (void)set_confused(p_ptr->confused + dam / 2); break;
						default:
						{
							if (p_ptr->resist_fear) note = " is unaffected.";
							else (void)set_afraid(p_ptr->afraid + dam);							
						}
					}
				}
			}
			else
			{
				if ((dam > 29) && ((randint(100) + (r_ptr->level * 2)) < (dam + willpower))
					&& (who == -1))
				{
					note = " is in your thrall!";
					set_pet(m_ptr);
				}
				else
				{
					switch (randint(4))
					{
						case 1: do_stun = dam / 2; if (who == -1) dam += willpower; break;
						case 2: do_conf = dam / 2; if (who == -1) dam += willpower;  break;
						default: do_fear = dam; if (who == -1) dam += willpower; 
					}
				}
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No "real" damage */
			dam = 0;
			break;
		}

		case GF_STUN:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & RF3_NO_STUN) 
			{
				/* This is being special cased -- since GF_STUN against monsters  */
				/* is most often called from the Spur steamware enhancement this  */
				/* produces an awful lot of message spam. Therefore, if we know   */
				/* the monster is immune to stunning, we just neglect to mention  */
				/* it. The only place this may (currently) become a problem is    */
				/* with the officer spell that attempts to stun a monster         */
				if (l_ptr->r_flags3 & (RF3_NO_STUN)) note = NULL;
				else note = " is immune to stunning!";
				
				/* Note resistance */
				if (fully_seen) l_ptr->r_flags3 |= (RF3_NO_STUN);

				dam = 0;
				break;
			}

			/* Sometimes super-charge the spell. -- why? */
			/* if (randint(5) < 2) dam += dam / 2; */

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 35;
			else if (r_ptr->flags3 & (RF3_ELEMENTAL)) tmp = (2 * r_ptr->level) + 25;
			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = (2 * r_ptr->level) + 15;

			/* Automata are particularly affected by smashing */
			else if (r_ptr->flags3 & (RF3_AUTOMATA)) tmp = r_ptr->level;
			else tmp = r_ptr->level + 5;

			/* this is a physical stun */
			/* if (who == -1) dam += willpower; */
			
			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists stunning!";

			/* If it fails, become stunned. */
			else do_stun = rand_spread(dam / 2, dam / 4);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}


		case GF_TK:
		{
			/* Affected by terrain. */
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & RF3_NO_STUN)
			{
				note = " is immune to stunning!";
				dam = 0;
				break;
			}

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 35;
			else if (r_ptr->flags3 & (RF3_ELEMENTAL)) tmp = (2 * r_ptr->level) + 25;
			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = (2 * r_ptr->level) + 15;
			/* Automata are particularly affected by smashing */
			else if (r_ptr->flags3 & (RF3_AUTOMATA)) tmp = r_ptr->level;
			else tmp = r_ptr->level + 5;

			if (who == -1) dam += willpower; 
			
			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists stunning!";

			/* If it fails, become stunned. */
			else do_stun = rand_spread(dam / 2, dam / 4);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* low physical damage. */
			dam = dam / 2;

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);
			break;
		}
		case GF_FORCE:
		{
			/* Affected by terrain. */
			/* dam += terrain_adjustment; */

			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & RF3_NO_STUN) 
			{
				note = " is immune to stunning!";
				dam = 0;
				break;
			}

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 35;
			else if (r_ptr->flags3 & (RF3_ELEMENTAL)) tmp = (2 * r_ptr->level) + 25;
			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = (2 * r_ptr->level) + 15;
			/* Automata are particularly affected by smashing */
			else if (r_ptr->flags3 & (RF3_AUTOMATA)) tmp = r_ptr->level;
			else tmp = r_ptr->level + 5;

			if (who == -1) dam += willpower; 
			
			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists stunning!";

			/* If it fails, become stunned. */
			else do_stun = rand_spread(dam / 2, dam / 4);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* low physical damage. */
			dam = dam / 2;

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);
			break;
		}

		case GF_CONFUSION:
		{
			/* Slightly affected by terrain. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			
			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
				(r_ptr->flags3 & RF3_PLANT) ||
				(r_ptr->flags3 & RF3_AUTOMATA) ||
				(r_ptr->flags3 & RF3_CONSTRUCT))
			{
				note = " is unaffected!";
				dam = 0;
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists a lot!";
				dam = div_round(dam, 7);
			}
			else
			{
				do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
			}

			/* doesn't do full damage */
			dam = dam / 3;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}

		case GF_SPIRIT:
		{
			/* Slightly affected by terrain. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			
			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
				(r_ptr->flags3 & RF3_PLANT) ||
				(r_ptr->flags3 & RF3_AUTOMATA) ||
				(r_ptr->flags3 & RF3_CONSTRUCT))
			{
				note = " is unaffected!";
				dam = 0;
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists.";
				dam = div_round(dam, 3);
			}
			else
			{
				do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
			}

			/* doesn't do full damage */
			dam = dam / 2;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		case GF_ECTOPLASM:
		{
			/* Slightly affected by terrain. */
			/* dam += terrain_adjustment / 2; */

			if (seen) obvious = TRUE;
			
			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
				(r_ptr->flags3 & RF3_PLANT) ||
				(r_ptr->flags3 & RF3_AUTOMATA) ||
				(r_ptr->flags3 & RF3_CONSTRUCT))
			{
				note = " is unaffected!";
				dam = 0;
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists a lot!";
				dam = div_round(dam, 2);
			}
			else
			{
				do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
		}
		
		/* Assume Damage */
		case GF_WHIP:
		{
			if (seen) obvious = TRUE;
			break;			
		}
		case GF_ARROW:
		{
			/* Affected by terrain. -- not yet.*/
			/* dam += terrain_adjustment; */
			
			/* Some creatures are tough. */			
			if ((r_ptr->flags2 & (RF2_IMPENT)) ||
				(r_ptr->flags2 & (RF2_GASEOUS)))
			{
				note = " resists.";
				dam = div_round(dam, 10);
			}
			
			if (seen) obvious = TRUE;
			break;
		}
		case GF_BULLET:
		case GF_SHOT:
		{
			/* Affected by terrain. -- not yet.*/
			/* dam += terrain_adjustment; */
			
			/* Some creatures are tough. */			
			if ((r_ptr->flags2 & (RF2_IMPENT)) &&
					 (one_in_(3)))
			{
				note = " is unaffected!";
				dam = 0;
				if (fully_seen && (r_ptr->flags2 & (RF2_IMPENT))) 
				{
					l_ptr->r_flags3 |= (RF2_IMPENT);
				}
			}
			else if ((r_ptr->flags2 & (RF2_IMPENT)) ||
				(r_ptr->flags2 & (RF2_GASEOUS)))
			{
				note = " resists.";
				dam = div_round(dam, 4);
			}
			
			if (seen) obvious = TRUE;
			break;
		}
		case GF_ROCKET:
		case GF_MISSILE:
		{
			/* Affected by terrain. -- not yet.*/
			/* dam += terrain_adjustment; */
			
			/* Some creatures are tough. */			
			if ((r_ptr->flags2 & (RF2_IMPENT)) &&
					 (one_in_(6)))
			{
				note = " is unaffected!";
				dam = 0;
				if (fully_seen && (r_ptr->flags2 & (RF2_IMPENT))) 
				{
					l_ptr->r_flags3 |= (RF2_IMPENT);
				}
			}
			else if (r_ptr->flags2 & (RF2_IMPENT))
			{
				note = " resists.";
				dam = div_round(dam, 3);
			}
			
			if (seen) obvious = TRUE;
			break;
		}
		case GF_PYRO_SHOT:
		{
			if (seen) obvious = TRUE;

			/* HACK Increase the damage from fire XCCCX */
			/* This can't be called w/o pyrokinetics, but is unsafe */
			dam += (p_ptr->skills[SK_PYROKINETICS].skill_rank * 3);

			if (r_ptr->flags2 & (RF2_IMPENT))
			{
				note = " resists.";
				dam = div_round(dam, 2);
			}
			
			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				note = " resists a lot.";
				dam = div_round(dam, 2);
				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
				{
					l_ptr->r_flags3 |= (RF3_IM_FIRE);
				}
			}
			else if ((r_ptr->flags3 & (RF3_PLANT)) ||
					 (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				dam += (dam / 4);
				/*if (fully_seen) know nothing */
				/* Plants and undead just take extra damage from fire */
			}

			if (r_ptr->flags8 & (RF8_VUN_FIRE))
			{
				note = " is blasted!";
				dam *= 4;
				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
			}
			
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			break;
			
		}

		/* Monsters don't do charm */		
		case GF_CHARM:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
	
				/* Attempt a saving throw  - charm doesn't affect much */
				/* just animals and peopleish */
				if ((r_ptr->flags1 & RF1_UNIQUE) ||
				    (r_ptr->flags1 & RF1_QUESTOR) ||
				    (r_ptr->flags3 & RF3_NO_CONF) ||
				    (r_ptr->flags3 & RF3_AUTOMATA) ||
				    (r_ptr->flags3 & RF3_CONSTRUCT) ||
				    (r_ptr->flags3 & RF3_ELEMENTAL) ||
				    (r_ptr->flags3 & RF3_DEMON) ||
				    (r_ptr->flags3 & RF3_UNDEAD) ||
				    (r_ptr->flags3 & RF3_PLANT) ||
				    ((2 * r_ptr->level) > randint(dam)))
				{
					/* Memorize a flag */
					if (r_ptr->flags3 & RF3_NO_CONF)
					{
						if (seen) r_ptr->flags3 |= (RF3_NO_CONF);
					}
	
					/* Resist */
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
				}
				else if (p_ptr->aggravate)
				{
					note = " hates you too much!";
				}
				else
				{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
				}

			/* No "real" damage */
			dam = 0;
			break;
		}
		
		case GF_CONTROL_ANIMAL:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_ANIMAL) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}
		case GF_CONTROL_PLANT:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_PLANT) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}
		case GF_CONTROL_AUTO_CONST:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if (((r_ptr->flags3 & RF3_AUTOMATA) ||
					  (r_ptr->flags3 & RF3_CONSTRUCT)) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}
		case GF_CONTROL_ELEMENTAL:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_ELEMENTAL) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}
		case GF_CONTROL_DEMON:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_DEMON) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}
		case GF_CONTROL_BEASTMAN:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_BEASTMAN) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}

		case GF_CONTROL_ALIEN:
		{
			/* This step increased damage using chrasima as the stat */
			/* and using the constituion stat table) */
			dam += p_ptr->stat_use[A_CHR] / 80;
			dam += willpower;
			if (seen) obvious = TRUE;
			
			if (p_ptr->aggravate)
			{
				note = " hates you too much!";
			}
			/* Notice the reverse from regular charm */
			else if ((r_ptr->flags3 & RF3_ALIEN) &&
				((2 * r_ptr->level) < randint(dam)))
			{
					note = " suddenly seems friendly!";
					set_pet(m_ptr);
			}
			else 
			{
					/* No obvious effect */
					note = " is unaffected!";
					obvious = FALSE;
			}
			dam = 0;

			break;
		}

		/* Clone monsters (Ignore "dam") */
		case GF_CLONE:
		{
			bool friendly = FALSE;
			bool pet = FALSE;
			if (seen) obvious = TRUE;
			if (is_friendly(m_ptr) && (randint(3) != 1))
				friendly = TRUE;
			if (is_pet(m_ptr) && (randint(3) != 1))
				pet = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Attempt to clone. */
			/* Should perhaps allow pets to be cloned? */
			if (multiply_monster(cave_m_idx[y][x], pet, TRUE))
			{
				note = " spawns!";
			}

			/* No "real" damage */
			dam = 0;

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_POLY:
		{
			if (seen) obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Heal Monster (use "dam" as amount of healing) */
		case GF_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x])
				p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			note = " looks healthier.";

			/* No physical damage */
			dam = 0;
			break;
		}

		/* Speed Monster (Ignore "dam") */
		case GF_SPEED:
		{
			if (seen) obvious = TRUE;

			/* Speed up */
			if (m_ptr->mspeed < r_ptr->speed + 10) m_ptr->mspeed += 10;
			note = " starts moving faster.";

			/* No physical damage */
			dam = 0;
			break;
		}


		/* Slow Monster (Use "dam" as "power") */
		case GF_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Sometimes super-charge the spell. */
			if (randint(5) < 2) dam += dam / 2;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 20;
			else tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists slowing!";

			/* If it fails, slow down */
			else do_slow = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}
		/* Drain Life */
		case GF_DRAIN:
		{
			/* No affect on non-living creatures */
			if (monster_nonliving(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (fully_seen) l_ptr->r_flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (fully_seen) l_ptr->r_flags3 |= (RF3_DEMON);
				}
				if (r_ptr->flags3 & (RF3_AUTOMATA))
				{
					if (seen) l_ptr->r_flags3 |= (RF3_AUTOMATA);
				}
				if (r_ptr->flags3 & (RF3_CONSTRUCT))
				{
					if (seen) l_ptr->r_flags3 |= (RF3_CONSTRUCT);
				}

				note = " is unaffected!";
				obvious = FALSE;
				dam = 0;
			}

			/* Do not allow wimpy monsters to yield much profit */
			if (m_ptr->hp + 1 < dam) dam = m_ptr->hp + 1;

			/* Character has cast the spell */
			if (who < 0)
			{
				/* Spell is damaging, and has hit a warm-blooded creature. */
				if ((dam > 0) && (!(r_ptr->flags2 & (RF2_COLD_BLOOD))))
				{
					msg_print("You suck in some life force.");

					/* Heal caster */
					hp_player(randint(dam * 3 - 1));

#if 0
					/* Feed caster -- protect against bloating */
					if (p_ptr->food + dam * 8 < p_ptr->food_bloated)
						set_food(p_ptr->food + dam * 8);
#endif
				}
			}
			

			break;
		}
		/* Statis monster */
		case GF_STASIS:
		{
			if (seen) obvious = TRUE;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 20;
			else tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (randint(tmp) > dam) note = " resists stasis!";
			else do_stasis = dam;
			
			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}
		/* Sleep (Use "dam" as "power"). */
		case GF_SLEEP:
		{
			if (seen) obvious = TRUE;

			/* Hack -- Monster didn't wake up */
			m_ptr->csleep = old_sleep;

			/* Sometimes super-charge the spell -- why? */
			/* if (randint(5) < 2) dam += dam / 2; */

			/* Allow resistance -- sleep is also prevented by no_sleep */
			/* in the do_sleep part of this function */
			if ((r_ptr->flags3 & (RF3_NO_SLEEP)) ||
				(r_ptr->flags3 & (RF3_AUTOMATA)) ||
				(r_ptr->flags3 & (RF3_CONSTRUCT)) ||
				(r_ptr->flags3 & (RF3_UNDEAD)) ||
				(r_ptr->flags3 & (RF3_PLANT)) ||
				(r_ptr->flags3 & (RF3_ELEMENTAL)))
			{
				note = " cannot be lulled!";
				obvious = FALSE;

				/* Note resistance */
				if (fully_seen) l_ptr->r_flags3 |= (RF3_NO_SLEEP);
			}
			else
			{
				/* Determine monster's power to resist. */
				if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 20;
				else tmp = r_ptr->level + 5;

				/* Attempt a saving throw */
				if (tmp > dam)               note = " is unaffected!";
				else if (tmp > randint(dam)) note = " resists sleep!";

				/* If it fails, (usually) hit the hay. */
				else do_sleep = rand_range(30, 50);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}
		
		case GF_FORGET:
		{
			if (m_ptr->smart)
			{
				/* Erase monster memory of player */
				m_ptr->smart = 0L;

				/* Notice -- if fully visible */
				if (mon_fully_visible(m_ptr))
				{
					char m_poss[32];

					/* Get monster pronoun */
					monster_desc(m_poss, m_ptr, 0x22);

					obvious = TRUE;
					msg_format("%^s forgets all %s knows about you!", m_name, m_poss);
				}
			}
			break;
		}
			
		case GF_CURSE:
		{
			int curse = rand_int(5);

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 20;
			else                              tmp = r_ptr->level + 5;

			if (r_ptr->flags3 & (RF3_UNDEAD)) tmp += 10;

			/* Attempt a saving throw. */
			if (tmp > randint(dam))
			{
				strcpy(note, " resists the curse!");
				obvious = FALSE;
				dam = 0;

				break;
			}

			/* Effect 0 -- slow */
			if (curse == 0)
			{
					do_slow = TRUE;
			}

			/* Effect 1 -- confusion */
			if (curse == 1)
			{
					/* Obvious */
					if (seen) obvious = TRUE;

    			if ((r_ptr->flags2 & RF2_EMPTY_MIND) ||
    				(r_ptr->flags3 & RF3_PLANT) ||
    				(r_ptr->flags3 & RF3_AUTOMATA) ||
    				(r_ptr->flags3 & RF3_CONSTRUCT))
    			{
    				note = " is unaffected!";
    				dam = 0;
    			}
    			else if (r_ptr->flags3 & (RF3_NO_CONF))
    			{
    				note = " resists a lot!";
    				dam = div_round(dam, 7);
    			}
    			else
    			{
    				do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
    			}
    
    			/* doesn't do full damage */
    			dam = dam / 3;
    
    			/* Notice resistance only if character is the caster */
    			if (who >= 0) note = NULL;
    
    			break;
			}

			/* Effect 2 -- panic */
			if (curse == 2)
			{
    			/* Obvious */
    			if (seen) obvious = TRUE;
    
    			/* Apply some fear */
    			/* Why all the fucked up math everywhere? */
    			do_fear = damroll(3, (dam / 2)) + 1;
    
    			/* Attempt a saving throw */
    			/* This is _normal_ fear - doesn't affect some things. */
    			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
    			    (r_ptr->flags3 & (RF3_AUTOMATA)) ||
    			    (r_ptr->flags3 & (RF3_CONSTRUCT)) ||
    			    (r_ptr->flags3 & (RF3_PLANT)) ||
    			    ((r_ptr->level * 2) > randint(dam)))
    			{
    				/* No obvious effect */
    				note = " is unaffected!";
    				obvious = FALSE;
    				do_fear = 0;
    			}
    			
    			/* Notice resistance only if character is the caster */
    			if (who >= 0) note = NULL;
    
    			/* No "real" damage */
    			dam = 0;
    			break;
			}

			/* Effect 3 -- stun */
			if (curse == 3)
			{
    			if (seen) obvious = TRUE;
    
    			if (r_ptr->flags3 & RF3_NO_STUN) 
    			{
    				note = " is immune to stunning!";
    				dam = 0;
    				break;
    			}
    
    			/* Sometimes super-charge the spell. -- why? */
    			/* if (randint(5) < 2) dam += dam / 2; */
    
    			/* Determine monster's power to resist. */
    			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level) + 35;
    			else if (r_ptr->flags3 & (RF3_ELEMENTAL)) tmp = (2 * r_ptr->level) + 25;
    			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = (2 * r_ptr->level) + 15;
    
    			/* Automata are particularly affected by smashing */
    			else if (r_ptr->flags3 & (RF3_AUTOMATA)) tmp = r_ptr->level;
    			else tmp = r_ptr->level + 5;
    
    			if (who == -1) dam += willpower; 
    			
    			/* Attempt a saving throw */
    			if (tmp > dam)               note = " is unaffected!";
    			else if (tmp > randint(dam)) note = " resists stunning!";
    
    			/* If it fails, become stunned. */
    			else do_stun = rand_spread(dam / 2, dam / 4);
    
    			/* Notice resistance only if character is the caster */
    			if (who >= 0) note = NULL;
    
    			/* No physical damage. */
    			dam = 0;
    			break;
			}

			/* Strip away bonuses */
			if (curse == 4)
			{
				if (m_ptr->hp > m_ptr->maxhp)
				{
					m_ptr->hp = m_ptr->maxhp;
					strcpy(note, " is cut down to size!");
				}
				else if (m_ptr->mspeed >= r_ptr->speed)
				{
					/* No longer hasted */
					m_ptr->mspeed = r_ptr->speed;

					strcpy(note, " is no longer hasted.");
				}
			}

			/* No physical damage */
			dam = 0;

			break;
		}
		case GF_GAIN_LEVEL:
		{
			/* Gain *maximum* hps */
			long new_hps = ((long)m_ptr->maxhp * 5 / 4);
			if ((new_hps <= 32000L) && (new_hps >= m_ptr->maxhp))
				m_ptr->maxhp = (s16b)new_hps;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			strcpy(note, " looks more powerful!");

			if (seen) obvious = TRUE;

			/* No physical damage */
			dam = 0;

			break;
		}			
		case GF_ENLIGHTENMENT:
		{
			int i;
			int m_idx = cave_m_idx[y][x];

			/* Learn lots of things */
			for (i = 0; i < LRN_MAX; i++)
			{
				if (one_in_(2))
					update_smart_learn(m_idx, i);
			}

			if (seen) obvious = TRUE;

			/* No physical damage */
			dam = 0;

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
				if (fully_seen) l_ptr->r_flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
				note = " loses some skin!";
				note_dies = " dissolves!";
			}

			/* Usually, ignore the effects */
			else
			{
				/* Hack -- Monster didn't wake up */
				m_ptr->csleep = old_sleep;

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
				if (seen) l_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Mark grid for later processing. */
				cave_temp_mark(y, x, FALSE);
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

		/* Teleport undead (Use "dam" as "power") */
		case GF_AWAY_ALIEN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_ALIEN))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_flags3 |= (RF3_ALIEN);

				/* Mark grid for later processing. */
				cave_temp_mark(y, x, FALSE);
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

		/* Teleport automata (Use "dam" as "power") */
		case GF_AWAY_AUTOMATA:
		{
			/* Only affect automata */
			if (r_ptr->flags3 & (RF3_AUTOMATA))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_flags3 |= (RF3_AUTOMATA);

				/* Mark grid for later processing. */
				cave_temp_mark(y, x, FALSE);
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
		/* Teleport beastman (Use "dam" as "power") */
		case GF_AWAY_BEASTMAN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_BEASTMAN))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_flags3 |= (RF3_BEASTMAN);

				/* Mark grid for later processing. */
				cave_temp_mark(y, x, FALSE);
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
				if (seen) l_ptr->r_flags3 |= (RF3_EVIL);
				/* Mark grid for later processing. */
				cave_temp_mark(y, x, FALSE);
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

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

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
				if (seen) l_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((2 * r_ptr->level) > randint(dam))
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

		/* Turn alien (Use "dam" as "power") */
		case GF_TURN_ALIEN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_ALIEN))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_ALIEN);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((2 * r_ptr->level) > randint(dam))
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

		/* Turn automata (Use "dam" as "power") */
		case GF_TURN_AUTOMATA:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_AUTOMATA))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_AUTOMATA);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((2 * r_ptr->level) > randint(dam))
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

		/* Turn beastman (Use "dam" as "power") */
		case GF_TURN_BEASTMAN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_BEASTMAN))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_BEASTMAN);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((2 * r_ptr->level) > randint(dam))
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
				if (seen) l_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if ((2 * r_ptr->level) > randint(dam))
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
			    ((2 * r_ptr->level) > randint(dam)))
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
				if (seen) l_ptr->r_flags3 |= (RF3_UNDEAD);

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
		/* Dispel alien */
		case GF_DISP_ALIEN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_ALIEN))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_ALIEN);

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
		/* Dispel undead */
		case GF_DISP_AUTOMATA:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_AUTOMATA))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_AUTOMATA);

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
		/* Dispel beastman */
		case GF_DISP_BEASTMAN:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_BEASTMAN))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_BEASTMAN);

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
				if (seen) l_ptr->r_flags3 |= (RF3_EVIL);

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

		/* Holy Fire */
		case GF_HOLY_FIRE:
		{
			int holy = dam / 2;
			int fire = dam / 2;
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				note = " writhes.";
				note_dies = " incinerates!";
  
  			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
  			{
  				note = " resists a lot.";
  				dam = div_round(fire, 7);
  				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
  				{
  					l_ptr->r_flags3 |= (RF3_IM_FIRE);
  				}
  			}
  			else if ((r_ptr->flags3 & (RF3_PLANT)) ||
  					 (r_ptr->flags3 & (RF3_UNDEAD)))
  			{
  				dam += (fire / 4);
  				/*if (fully_seen) know nothing */
  				/* Plants and undead just take extra damage from fire */
  			}
  
  			if (r_ptr->flags8 & (RF8_VUN_FIRE))
  			{
  				note = " is blasted!";
  				fire *= 6;
  				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
  			}		

  			/* Notice resistance only if character is the caster */
  			if (who >= 0) note = NULL;
  			dam = fire + holy;
			}

			/* Others take half damage in fire */
			else
			{ 
    			if ((r_ptr->flags3 & (RF3_IM_FIRE)))
    			{
    				note = " resists a lot.";
    				dam = div_round(fire, 7);
    				if (fully_seen && (r_ptr->flags3 & (RF3_IM_FIRE))) 
    				{
    					l_ptr->r_flags3 |= (RF3_IM_FIRE);
    				}
    			}
    			else if ((r_ptr->flags3 & (RF3_PLANT)) ||
    					 (r_ptr->flags3 & (RF3_UNDEAD)))
    			{
    				dam += (fire / 4);
    				/*if (fully_seen) know nothing */
    				/* Plants and undead just take extra damage from fire */
    			}
    
    			if (r_ptr->flags8 & (RF8_VUN_FIRE))
    			{
    				note = " is blasted!";
    				fire *= 6;
    				if (fully_seen) l_ptr->r_flags8 |= (RF8_VUN_FIRE);
    			}		
					
					dam = fire;
					
    			/* Notice resistance only if character is the caster */
    			if (who >= 0) note = NULL;
			}
			break;
		}
		/* Dispel evil */
		case GF_DISP_DEMON:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				/* Learn about type */
				if (seen) l_ptr->r_flags3 |= (RF3_DEMON);

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
		/* Statis monster */
		case GF_MPDRAIN_MACHINE:
		{
			/* Only affect evil */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)) || 
				(r_ptr->flags3 & (RF3_CONSTRUCT)))
			{
				if (r_ptr->flags3 & (RF3_AUTOMATA))
				{
					if (seen) l_ptr->r_flags3 |= (RF3_AUTOMATA);
				}
				if (r_ptr->flags3 & (RF3_CONSTRUCT))
				{
					if (seen) l_ptr->r_flags3 |= (RF3_CONSTRUCT);
				}

				if (seen) obvious = TRUE;

				/* Determine monster's power to resist. */
				if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = (2 * r_ptr->level);
				else tmp = r_ptr->level / 2;

				/* Attempt a saving throw */
				if (randint(tmp) > dam) note = " resists drain!";
				else mp_drain = dam;
			
				/* Notice resistance only if character is the caster */
				if (who >= 0) note = NULL;

				/* No physical damage. */
				dam = 0;
				break;
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
		case GF_BRIBE:
		{
			/* Obvious */
			if (seen) obvious = TRUE;
			
			/* Get monster level, and calculate bribe cost */
			tmp = (r_ptr->level * 3) * (rand_range(128, 185) - (p_ptr->skills[SK_BRIBERY].skill_rank));
			
			/* Uniques are much harder to bribe */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp *= 3;
			
			/* Charm helps a great deal */
			tmp = (tmp * (100 - (p_ptr->stat_use[A_CHR] / 20))) / 100;

			if (p_ptr->au < tmp)
				note = " wants more gold than you have!";
			else
			{
				char tmp_val[160];

				/* Prompt */
				(void)strnfmt(tmp_val, 78, "Bribe %s for %d gold? ", m_name, tmp);
	
				/* Belay that order */
				if (!get_check(tmp_val)) break;

				/* Pay the gold */
				p_ptr->au -= tmp;

				/* Redraw gold */
				p_ptr->redraw |= (PR_GOLD);
				
				note = " leaves you alone.";
				
				do_bribe = 1;
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

	/* Hack -- some attacks are extremely noisy. */
	/* This needs some work -- better org of noise types */
	if (typ == GF_FIRE) add_wakeup_chance = 200;
	if (typ == GF_PLASMA) add_wakeup_chance = 1000;
	if (typ == GF_ROCK) add_wakeup_chance = 200;
	if (typ == GF_EARTH) add_wakeup_chance = 400;
	if (typ == GF_SHARDS) add_wakeup_chance = 1000;
	if (typ == GF_WIND) add_wakeup_chance = 20;
	if (typ == GF_GALE) add_wakeup_chance = 400;
	if (typ == GF_ELEC) add_wakeup_chance = 100;
	if (typ == GF_VOLT) add_wakeup_chance = 500;	
	if (typ == GF_VIBE) add_wakeup_chance = 3000;
	if (typ == GF_SOUND) add_wakeup_chance = 8000;
	if (typ == GF_SONIC) add_wakeup_chance = 10000;
	if (typ == GF_GLOW) add_wakeup_chance = 1000;
	if (typ == GF_LIGHT) add_wakeup_chance = 2000;
	if (typ == GF_BRILLIANCE) add_wakeup_chance = 5000;
	if (typ == GF_STORM) add_wakeup_chance = 3000;
	if (typ == GF_BULLET) add_wakeup_chance = 2000;
	if (typ == GF_SHOT) add_wakeup_chance = 3000;
	if (typ == GF_PYRO_SHOT) add_wakeup_chance = 8000;
	if (typ == GF_ROCKET) add_wakeup_chance = 5000;
	if (typ == GF_MISSILE) add_wakeup_chance = 8000;

	/*
	 * Otherwise, if this is the first monster hit, the spell was capable
	 * of causing damage, and the player was the source of the spell,
	 * make noise. -LM-
	 */
	if ((project_m_n == 1) && (who <= 0) && (dam))
	{
		add_wakeup_chance += p_ptr->base_wakeup_chance / 2 + 1000;
	}

	/*
	 * Mega-Hack -- "Unique" and special quest monsters may only be killed
	 * by the player.
	 * pulled this code from the if statement 
	 *   
	 *  || (m_ptr->r_idx == q_info[quest_num(p_ptr->depth)].r_idx)
	 */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		if ((who > 0) && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}



	/* Handle polymorph */
	if (do_poly)
	{
		/* Default -- assume no polymorph */
		if (!dam && !note)
		{
			note = " cannot be polymorphed!";
			obvious = TRUE;
		}

		/* Uniques can't be polymorphed */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			do_poly = FALSE;
			obvious = TRUE;
		}

		/*  ether resisters can't be polymorphed */
		else if (r_ptr->flags3 & (RF3_IM_ETHER))
		{
			do_poly = FALSE;
			obvious = TRUE;
		}

		/* Allow a saving throw */
		if (randint(100) < r_ptr->level + 50)
		{
			if (!dam) note = " resists polymorph!";
			do_poly = FALSE;
			obvious = FALSE;
		}

		/* Polymorph the monster */
		if (do_poly)
		{
			/* Note how wounded the old monster was */
			long perc = 100 - (100 * m_ptr->hp / m_ptr->maxhp);

			/* Introduce a little variance */
			perc = rand_range(2 * perc / 3, 4 * perc / 3);

			/* New monster cannot be too badly hurt */
			if (perc >= 75) perc = 75;

			/* Pick a new monster race */
			tmp = poly_r_idx(m_ptr->r_idx);

			/* Handle polymorph */
			if (tmp != m_ptr->r_idx)
			{
				/* Get the monsters attitude */
				friendly = is_friendly(m_ptr);
				pet = is_pet(m_ptr);

				/* Obvious */
				if (seen)
				{
					obvious = TRUE;

					/* Hack -- Note polymorph, ignore other messages */
					msg_format("%^s changes!", m_name);
				}

				/* Delete the "old" monster */
				delete_monster_idx(cave_m_idx[y][x]);

				/* Create a new monster (no groups) */
				(void)place_monster_aux(y, x, tmp, FALSE, FALSE, pet, FALSE);

				/* Hack -- Get new monster */
				m_ptr = &m_list[cave_m_idx[y][x]];

				/* New monster's wounds are much like those of the old monster */
				m_ptr->hp -= (perc * m_ptr->maxhp / 100);

				/* New monster is sometimes slightly confused and/or stunned */
				if (one_in_(2)) m_ptr->confused = 5;
				if (one_in_(2)) m_ptr->stunned  = 5;

				/* Cancel any other effects */
				return (obvious);
			}
		}
	}

	/* Handle slowing -- no cumulative slowing */
	if ((do_slow) && (m_ptr->mspeed >= r_ptr->speed - 5))
	{
		/* Inertia hounds and time elementals can't be slowed */
		if ((r_ptr->flags3 & (RF3_IM_TIME)))
		{
			if (!dam) note = " cannot be slowed!";
		}

		/* Slow the monster down */
		else
		{
			m_ptr->mspeed -= 10;
			if (!note) note = " starts moving slower.";
		}
	}

	/* Handle draining of mana from monsters */
	/* Monster must have mana */
	if ((mp_drain) && (r_ptr->mana > 0))
	{
		/* Check for mana to drain */
		if (m_ptr->mana > 0)
		{
			int magicdamage = randint(mp_drain);
			
			if (magicdamage > m_ptr->mana) m_ptr->mana = 0;

			/* Drain the mana */
			else m_ptr->mana -= magicdamage;

			if (!note) note = " is drained.";
		}
	}

	/* Handle stunning -- usually no cumulative stunning */
	/* the no stun flag needs to go here. */
	if ((do_stun) && ((!m_ptr->stunned) || (one_in_(3))))
	{
		/* Sound and Impact breathers never stun */
		if (r_ptr->flags3 & RF3_NO_STUN)
		{
			/* Note resistance */
			if (fully_seen) l_ptr->r_flags3 |= (RF3_NO_STUN);
			if (!dam) note = " is immune to stunning!";
		}
		/* Non-living monsters are harder to stun */
		if ((monster_nonliving(r_ptr)) && (randint(2) < 2))
		{
			if (!dam && !note && !m_ptr->stunned)
			{
				note = " is dazed, but quickly recovers.";
			}
		}

		/* Stun the monster */
		else
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Get confused */
			if (m_ptr->stunned)
			{
				if (!note) note = " is more dazed.";
				tmp = m_ptr->stunned + (do_stun / 3);
			}
			else
			{
				if (!note) note = " is dazed.";
				tmp = do_stun;
			}

			/* Apply stun */
			m_ptr->stunned = (tmp < 200) ? tmp : 200;
		}
	}

	/* Handle confusion -- no cumulative confusion */
	if ((do_conf) && (!m_ptr->confused))
	{
		/* Allow resistance */
		if ((r_ptr->flags3 & (RF3_NO_CONF)))
		{
			if (!dam) note = " cannot be confused!";

			/* Note resistance */
			if (fully_seen) l_ptr->r_flags3 |= (RF3_NO_CONF);
		}

		/* Confuse the monster */
		else
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Already confused  (not currently possible) */
			if (m_ptr->confused)
			{
				if (!note) note = " looks more confused.";
				tmp = m_ptr->confused + (do_conf / 3);
			}

			/* Was not confused */
			else
			{
				if (!note) note = " looks confused.";
				tmp = do_conf;
			}

			/* Apply confusion */
			m_ptr->confused = (tmp < 200) ? tmp : 200;
		}
	}

	/* Handle fear -- monster type handled earlier, not immunity  XXX */
	if ((do_fear) && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		/* Note new fear */
		if ((!note) && (!m_ptr->monfear)) note = " panics!";

		/* Increase fear (not fully cumulative) */
		if (m_ptr->monfear) do_fear /= 2;
		tmp = MIN(200, m_ptr->monfear + do_fear);

		/* Panic the monster */
		m_ptr->monfear += tmp;

	}

	/* Handle stasis -- no cumulative sleeping */
	if ((do_stasis))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* If it fails, (usually) hit the hay. */
		if (m_ptr->stasis) 
		{
			m_ptr->stasis += 1;
			note = "is unaffected!";
		}
		else 
		{
			/* hold the monster */
			m_ptr->stasis += do_stasis;
			
			/* Message */
			note = " is held!";
		}

		/* Monster will /not/ go inactive */
	}

	/* Handle sleeping -- no cumulative sleeping */
	if ((do_sleep) && (!m_ptr->csleep) && !(r_ptr->flags3 & RF3_NO_SLEEP))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Message */
		note = " falls asleep!";

		/* Apply sleeping */
		m_ptr->csleep = do_sleep;

		/* Monster will /not/ go inactive */
	}

	/* Give detailed messages if visible */
	if (note)
	{
		if (seen)
		{
			msg_format("%^s%s", m_name, note);

			/* Character notices monster */
			m_ptr->mflag &= ~(MFLAG_MIME);
		}
	}

	/* Standard pain messages */
	else
	{
		/* Monster is hurt, and is fairly close  XXX XXX */
		if ((dam > 0) && (distance(p_ptr->py, p_ptr->px, y, x) <= 12))
		{
			message_pain(cave_m_idx[y][x], dam);
		}
	}

	/* Fear message */
	if ((fear) && (m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}
	
	/* Hurt monster if damage is greater than zero */
	if (dam > 0)
	{
		bool can_hear = FALSE;

		/* Complex message */
		if ((p_ptr->wizard) && (who <= 0))
		{
			msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
		}

		/* Different (or no) death messages when not seen */
		if (!seen)
		{
			/* No death message */
			/* note_dies = ""; */

			/* Not in line of sight, too far away to hear through walls */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (player_has_los_bold(y, x)) ||
			    (distance(p_ptr->py, p_ptr->px, y, x) <= 10))
			{
				/* Can hear */
				can_hear = TRUE;
			}
		}
		
		/* If another monster did the damage, hurt the monster by hand */
		if (who > 0)
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
				bool sad = FALSE;
	
				/* Get the sad message if a non-visible pet dies */
				if (is_pet(m_ptr) && !(m_ptr->ml)) sad = TRUE;
					
				/* give experience if a pet kills a monster */
				if (is_pet(n_ptr))
				{
					/* Player level */
					div = p_ptr->lev;
			
					/* Give some experience for the kill */
					new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;
			
					/* Handle fractional experience */
					new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
					                * 0x10000L / div) + p_ptr->exp_frac;
			
					/* Keep track of experience */
					if (new_exp_frac >= 0x10000L)
					{
						new_exp++;
						p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
					}
					else
					{
						p_ptr->exp_frac = (u16b)new_exp_frac;
					}
			
					/* Gain experience */
					gain_exp(new_exp);
				}
				
				/* Hack -- can't delete the m_idx yet */
				pet_kill = TRUE;
				
				/* Hack -- If we're here the monster has died */
				/* Most monsters "die" */
				/* note = " dies."; */
			
				/* Some monsters get "destroyed" */
				/* if (monster_nonliving(r_ptr)) note = " is destroyed."; */
	
				/* MONSTER MESSAGE CODE */
				/* Give detailed messages if destroyed */
				/* if (note) msg_format("%^s%s", m_name, note); */
				
				if (sad)
				{
					msg_print("You feel sad for a moment.");
				}
			}

			/* Damaged monster */
			else
			{
				/* Nothing should need to be done here, msging */
				/* should be handled below, as well as sleep */

				/* Give detailed messages if visible or destroyed */
				/* if (note && seen) msg_format("%^s%s", m_name, note); */
	
				/* Hack -- Pain message */
#if 0
				else if ((dam > 0) && (distance(p_ptr->py, p_ptr->px, y, x) <= 12))
				{
					message_pain(cave_m_idx[y][x], dam);
				}
				/* Hack -- handle sleep */
				if (do_sleep) m_ptr->csleep = do_sleep;
#endif				
			}
		}
		
		/* Hurt the monster, check for fear and death */
		else if (mon_take_hit(cave_m_idx[y][x], dam, &fear, note_dies))
		{
			/* Note death */
			if (can_hear) death_count++;

			/* Return "Anything seen?" */
			/* return (obvious); */
		}
	}

	/* Hack -- handle deleting the monster if killed by a pet */
	if (pet_kill)
	{
				/* Generate treasure, etc */
				monster_death(cave_m_idx[y][x]);
	
				/* Delete the monster */
				delete_monster_idx(cave_m_idx[y][x]);
				
				/* Reset pet_kill */
				pet_kill = FALSE;
	}

	/* handle Bribe */
	if (do_bribe)
	{
		/* Delete the monster -- note, it will return. . .*/
		delete_monster(y, x);
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
 * Actually, for historical reasons, we just assume that the effects were
 * obvious.  XXX XXX XXX
 */
static bool project_p(int who, int y, int x, int dam, int typ)
{
	int k = 0;

	/* Adjustment to damage caused by terrain, if any. */
	int terrain_adjustment = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster and its race */
	monster_type *m_ptr;
	monster_race *r_ptr;
	
	bool nonliving;

	int save;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Player notices the attack effect */ 
	bool notice = FALSE;

	/* Hack -- messages */
	cptr act = NULL;
	cptr msg = NULL;


	u32b f1, f2, f3;
	player_flags(&f1, &f2, &f3);

	save = p_ptr->skill_sav;

	/* No player here */
	if (!(cave_m_idx[y][x] < 0)) return (FALSE);

	/* Player cannot harm himself  XXX XXX */
	if (who < 0) return (FALSE);


	/* Some attack types are affected by skills -- not yet */
	/* if (typ == GF_LITE) */ 


	/* Limit maximum damage XXX XXX XXX */
	if (dam > 1600) dam = 1600;

#if 0
	/* Determine if terrain is capable of adjusting physical damage. --not yet*/
	switch (cave_feat[y][x])
	{
		/* A player behind rubble takes less damage. */
		case FEAT_RUBBLE:
		{
			if (randint(10) < 2)
			{
				msg_print("You duck behind a boulder!");
				return (FALSE);
			}
			else terrain_adjustment -= dam / 6;
			break;
		}

		/*
		 * Fire-based spells suffer, but other spells benefit slightly
		 * (player is easier to hit).  Water spells come into their own.
		 */
		case FEAT_WATER:
		{
			if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
				(typ == GF_PLASMA)) terrain_adjustment -= dam / 4;
			else if ((typ == GF_WATER) || (GF_STORM))
				terrain_adjustment = dam / 2;
			else terrain_adjustment = dam / 10;
			break;
		}

		/* Cold and water-based spells suffer, and fire-based spells benefit. */
		case FEAT_LAVA:
		{
			if ((typ == GF_COLD) || (typ == GF_ICE) ||
				(typ == GF_WATER) || (typ == GF_STORM))
				terrain_adjustment -= dam / 4;
			else if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
				(typ == GF_PLASMA)) terrain_adjustment = dam / 4;
			break;
		}

		/* Characters skilled in nature lore can hide behind trees. */
		case FEAT_TREE:
		{
			int chance = 16 - get_skill(S_NATURE, 0, 10);

			if (randint(chance) < 2)
			{
				msg_print("You dodge behind a tree!");
				return (FALSE);
			}
			else terrain_adjustment -= dam / 6;
			break;
		}
	}
#endif

	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;

	/* Get the source monster */
	m_ptr = &m_list[who];

	/* Get the monster race. */
	r_ptr = &r_info[m_ptr->r_idx];
	
	/* Is the monster organic? */
	nonliving = (monster_nonliving(r_ptr));


	/* A real monster */
	if (who > 0)
	{
		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, m_ptr, 0x88);
	}

	/* An explosion  XXX */
	else
	{
		strcpy(m_name, "something");
		strcpy(killer, "an explosion");
	}

	/* Analyze the damage */
	switch (typ)
	{
		case GF_HURT:
		{
			/* Not affected by terrain  XXX */

			/* No resists, adjusts, etc. */
			take_hit(dam, killer, TRUE);

			break;
		}
		case GF_HEAT:
		case GF_FIRE:
		case GF_PLASMA:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			notice = fire_dam(dam, typ, killer);
			if (fuzzy && notice) msg_print("You are hit by fire!");
			break;
		}

		case GF_ROCK:
		case GF_EARTH:
		case GF_SHARDS:
		{
			/* Affected by terrain */
			dam += terrain_adjustment;
			
			notice = earth_dam(dam, typ, killer);
			if (fuzzy && notice) msg_print("You are hit by rocks!");
			break;
		}

		case GF_GUST:
		case GF_WIND:
		case GF_GALE:
		{
			if (fuzzy) msg_print("You are buffeted by winds!");

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			/* Take damage */
			air_dam(dam, typ, killer);

			break;
		}

		/* these also need fixing */
		case GF_RUST:
		case GF_STEAM:
		case GF_STORM:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Message */
			if (fuzzy) msg_print("You are hit by water!");

			/* Mark grid for later processing. */
			if (typ == GF_STORM) cave_temp_mark(y, x, FALSE);

			/* take damage */
			water_dam(dam, typ, killer);
			break;
		}

		
		case GF_SHOCK:
		case GF_ELEC:
		case GF_VOLT:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by lightning!");
			elec_dam(dam, typ, killer);
			break;
		}

		case GF_CHILL:
		case GF_ICE:
		case GF_GLACIAL:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by cold!");
			ice_dam(dam, typ, killer);
			break;
		}

		case GF_CORROSIVE:
		case GF_ACID:
		case GF_LIQUESCE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by acid!");
			acid_dam(dam, typ, killer);
			break;
		}
		
		case GF_CAUSTIC:
		case GF_POISON:
		case GF_CONTAGION:
		{
				
			/* Affected by terrain. */
			dam += terrain_adjustment;

			notice = poison_dam(dam, typ, killer);
			if (fuzzy && notice) msg_print("You are hit by poison!");

			break;
		}

		
		case GF_AGE:
		case GF_TIME:
		case GF_CHRONOS:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;
			
			if (fuzzy) msg_print("You are hit by something strange!");
			time_dam(dam, typ, killer);
			break;
		}

		case GF_VAPOR:
		case GF_ETHER:
		case GF_NEXUS:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			
			/* Apply resistance */
			dam = apply_resistance(dam, p_ptr->res[RS_ETH]);
			take_hit(dam, killer, TRUE);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}
		
		case GF_VIBE:
		case GF_SOUND:
		case GF_SONIC:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are blasted by sound.");
			dam = apply_resistance(dam, p_ptr->res[RS_SND]);

			if (resist_effect(RS_SND))
			{
				/* nothing */
			}
			/* Side effects of powerful sound attacks. */
			else if (dam > rand_int(300))
			{
				/* Confuse the player (a little). */
				if (!p_ptr->resist_confu)
				{
					k = (randint((dam > 400) ? 21 : (1 + dam / 20)));
					(void)set_confused(p_ptr->confused + k);
				}

				/* Stun the player. */
				k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}
			take_hit(dam, killer, TRUE);
			break;
		}
		
		case GF_UNHOLY:
		case GF_NETHER:
		case GF_ABYSS:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;
			if (fuzzy) msg_print("You are blasted by dark energies.");
			dam = apply_resistance(dam, p_ptr->res[RS_NTH]);
			if (!resist_effect(RS_NTH))
			{
				if (p_ptr->hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp / 500));
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp / 50));
				}
			}
			take_hit(dam, killer, TRUE);
			break;
		}
		
		case GF_EMP:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;
			if (fuzzy) msg_print("A strange force washes over you.");
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				/* NOTE THE TOTAL LACK OF A SAVEING THROW HERE!!! */
				/* MACHINES SHUT_DOWN WHEN HIT BY EMP */
				take_hit(dam, killer, TRUE);
				/* Make sure paralized counter is empty */
				if (!p_ptr->paralyzed) set_paralyzed(randint(2));
				if (!p_ptr->resist_confu) set_confused(p_ptr->confused + randint(dam / 10));
			}
			break;
		}
		case GF_MAGNETIC:
		{
			dam += terrain_adjustment;
			if (fuzzy) msg_print("A strange force washes over you.");
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				take_hit(dam / 10, killer, TRUE);
				if (!p_ptr->free_act) set_slow(p_ptr->slow + randint(dam));
				
			}
			break;
		}
		case GF_GRAVITY:
		{
			dam += terrain_adjustment;
			if (fuzzy) msg_print("A strange force washes over you.");
			take_hit(dam / 10, killer, TRUE);
			if (!p_ptr->free_act) set_slow(p_ptr->slow + randint(dam));
			break;		
		}
		case GF_WEAK_RAD:
		{
			/* Nothing special yet  XCCCX */
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA) ||
				(p_ptr->prace == RACE_GHOST))
			{
				break;
			}
			else 
			{
				if (randint(50) > save) 
				{
					if (randint(10) > 8) mutate_player();
					else do_poly_self();
	
					/* No resists, adjusts, etc. */
					take_hit(dam / 2, killer, TRUE);
				}
	
				msg_print("You feel queasy.");

				/* No resists, adjusts, etc. */
				take_hit(dam / 2, killer, TRUE);
			}
			break;
		}
		case GF_STRONG_RAD:
		{
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA) ||
				(p_ptr->prace == RACE_GHOST))
			{
				break;
			}
			else 
			{
				if (randint(100) > save) 
				{
					if (randint(10) > 8) mutate_player();
					else {do_poly_self();}
	
					/* No resists, adjusts, etc. */
					take_hit(dam / 2, killer, TRUE);
				}

				msg_print("You feel ill.");
	
				/* No resists, adjusts, etc. */
				take_hit(dam / 2, killer, TRUE);
			}
			break;
		}

		case GF_GLOW:
		case GF_LIGHT:
		case GF_BRILLIANCE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			dam = apply_resistance(dam, p_ptr->res[RS_LIT]);

			if (fuzzy) msg_print("You are hit by something!");
			if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(dam / 8, dam / 4));
			}
			/* Light can't cause wound damage */
			take_hit(dam, killer, FALSE);
			break;
		}
		
		case GF_DIM:
		case GF_DARK:
		case GF_TENEBROUS:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;
			dam = apply_resistance(dam, p_ptr->res[RS_DRK]);

			if (fuzzy) msg_print("You are hit by something!");
			if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(dam / 8, dam / 4));
			}
			/* Dark can't cause wound damage */
			take_hit(dam, killer, FALSE);
			break;
		}
		case GF_FEAR:
		{
			int i;
			/* Affected by terrain. */
			dam += terrain_adjustment;
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);
			if (fuzzy) msg_format("%^s makes scary noises.", m_name);

			if (p_ptr->resist_fear)
			{
				msg_print("You refuse to be frightened.");
			}
			else if ((rand_int(100) < save) || (resist_effect(RS_PSI)))
			{
				msg_print("You resist the effects!");
			}
			else 
			{
				if (!(f3 & (TR3_AUTOMATA)))
				{
					i = div_round((2 * r_ptr->level) + dam, 10);
					(void)set_afraid(p_ptr->afraid + i);
				}
			}
			break;
		}
		case GF_PSI:
		{
			int psi_resists = 0;
			int tmp;

			bool conf = FALSE;
			bool do_blind = FALSE;
			bool fear = FALSE;
			bool paralyze = FALSE;
			bool slow = FALSE;
			bool stun = FALSE;
			bool energy = FALSE;

			/* Hack -- limit damages */
			if (dam > 150) dam = 150;
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);

			if (fuzzy) msg_print("Your mind is hit by mental energy!");
			
			if (p_ptr->skills[SK_TELEPATHY].skill_max > 0)
					psi_resists += p_ptr->skills[SK_TELEPATHY].skill_rank;
			if (f3 & (TR3_AUTOMATA)) psi_resists += 100;
			if (p_ptr->resist_confu) psi_resists += 10;
			if (save > randint(100)) psi_resists += 3;
			if (p_ptr->res[RS_PSI]) psi_resists += (p_ptr->res[RS_PSI] / 10);
			if (p_ptr->confused) psi_resists--;
			if (p_ptr->image) psi_resists--;
			if (p_ptr->stun) psi_resists--;
			if ((p_ptr->afraid) && (save < randint(100)))
				 psi_resists--;

			/* Get effect factor (dam / resist) */
			if (psi_resists < -2) psi_resists = -2;
			tmp = 2 * dam / (psi_resists + 3);

			/* Mental attacks ignore most resists */

			/* Can be confused */
			if (tmp > rand_int(save)) conf = TRUE;

			/* Can be blinded */
			if (tmp > 5 + rand_int(save * 2)) do_blind = TRUE;

			/* Can be frightened */
			if (tmp > 2 + rand_int(save)) fear = TRUE;

			/* Can be paralyzed, but only without free action */
			if (!p_ptr->free_act)
			{
				if (tmp > 5 + rand_int(save)) paralyze = TRUE;
			}
			/* Those with free action can only be slowed */
			else if (tmp > 10 + rand_int(save)) slow = TRUE;

			/* Can be stunned */
			if (tmp > 5 + rand_int(save)) stun = TRUE;

			/* Can be robbed of energy */
			if (tmp > 5 + rand_int(save)) energy = TRUE;

			/* Something happened */
			if (conf || do_blind || fear || paralyze || slow || stun || energy)
			{
				if (!fuzzy) msg_print("Your mind is blasted by mental energy.");

				if ((conf) && (!p_ptr->resist_confu))
					set_confused(p_ptr->confused + 2 + rand_range(dam / 30, dam / 10));
				if ((do_blind) && (!p_ptr->resist_blind)) 
					set_blind(p_ptr->blind + 2 +rand_range(dam / 30, dam / 10));
				if ((fear) && (!p_ptr->resist_fear)) 
					set_afraid(p_ptr->afraid + 2 + rand_range(dam / 15, dam / 5));
				if ((paralyze) && (!p_ptr->free_act))  
					set_paralyzed(p_ptr->paralyzed + 2 + rand_range(dam / 30, dam / 10));
				if (slow) set_slow(p_ptr->slow + 2 +
					rand_range(dam / 30, dam / 10));
				if (stun) set_stun(p_ptr->stun + 5 +
					rand_range(dam / 20, dam / 10));
				if (energy)
				{
					msg_print("You lose control of your body for a moment!");
					p_ptr->energy -= randint(MIN(p_ptr->energy, 25));
				}
			}
			else
			{
				dam /= 3;
				if (p_ptr->chp > dam)
					msg_print("You resist the mental attack!");
			}
			take_hit(randint(dam), killer, TRUE);
			break;
		}
		case GF_DOMINATION:
		{
			int psi_resists = 0;
			int tmp;

			bool conf = FALSE;
			bool do_blind = FALSE;
			bool fear = FALSE;
			bool paralyze = FALSE;
			bool slow = FALSE;
			bool stun = FALSE;
			bool energy = FALSE;

			/* Hack -- limit damages */
			if (dam > 400) dam = 400;
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);

			if (fuzzy) msg_print("Your mind is overwhelmed by mental energy!");
			
			if (p_ptr->skills[SK_TELEPATHY].skill_max > 0)
					psi_resists += p_ptr->skills[SK_TELEPATHY].skill_rank;
			if (f3 & (TR3_AUTOMATA)) psi_resists += 200;
			if (p_ptr->resist_confu) psi_resists += 10;
			if (save > randint(100)) psi_resists += 3;
			if (p_ptr->res[RS_PSI]) psi_resists += (p_ptr->res[RS_PSI] / 10);
			if (p_ptr->confused) psi_resists--;
			if (p_ptr->image) psi_resists--;
			if (p_ptr->stun) psi_resists--;
			if ((p_ptr->afraid) && (save < randint(100)))
				 psi_resists--;

			/* Get effect factor (dam / resist) */
			if (psi_resists < -2) psi_resists = -2;
			tmp = 2 * dam / (psi_resists + 3);

			/* Mental attacks ignore most resists */

			/* Can be confused */
			if (tmp > rand_int(save)) conf = TRUE;

			/* Can be blinded */
			if (tmp > 5 + rand_int(save * 2)) do_blind = TRUE;

			/* Can be frightened */
			if (tmp > 2 + rand_int(save)) fear = TRUE;

			/* Can be paralyzed, but only without free action */
			if (!p_ptr->free_act)
			{
				if (tmp > 5 + rand_int(save)) paralyze = TRUE;
			}
			/* Those with free action can only be slowed */
			else if (tmp > 10 + rand_int(save)) slow = TRUE;

			/* Can be stunned */
			if (tmp > 5 + rand_int(save)) stun = TRUE;

			/* Can be robbed of energy */
			if (tmp > 5 + rand_int(save)) energy = TRUE;

			/* Something happened */
			if (conf || do_blind || fear || paralyze || slow || stun || energy)
			{
				if (!fuzzy) msg_print("Your mind is blasted by mental energy.");

				if ((conf) && (!p_ptr->resist_confu))
					set_confused(p_ptr->confused + 2 + rand_range(dam / 30, dam / 10));
				if ((do_blind) && (!p_ptr->resist_blind)) 
					set_blind(p_ptr->blind + 2 +rand_range(dam / 30, dam / 10));
				if ((fear) && (!p_ptr->resist_fear)) 
					set_afraid(p_ptr->afraid + 2 + rand_range(dam / 15, dam / 5));
				if ((paralyze) && (!p_ptr->free_act))  
					set_paralyzed(p_ptr->paralyzed + 2 + rand_range(dam / 30, dam / 10));
				if (slow) set_slow(p_ptr->slow + 2 +
					rand_range(dam / 30, dam / 10));
				if (stun) set_stun(p_ptr->stun + 5 +
					rand_range(dam / 20, dam / 10));
				if (energy)
				{
					msg_print("You lose control of your body for a moment!");
					p_ptr->energy -= randint(MIN(p_ptr->energy, 25));
				}
			}
			else
			{
				dam /= 3;
				if (p_ptr->chp > dam)
					msg_print("You resist the mental attack!");
			}
			take_hit(randint(dam), killer, TRUE);
			break;
		}
		case GF_STUN:
		{
			/* get special stun damge */
			int stundam;

			if (fuzzy) msg_print("You are hit by something!");
			if ((rand_int(100) < save) || (resist_effect(RS_TLK)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else 
			{
  			/* Fix damage to avoid insta-kill */
  			stundam = dam;
  			
  			/* Limit stun damage */
  			if (stundam > 99) stundam = 99;

  			/* set stun */	
				(void)set_stun(p_ptr->stun + randint(stundam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_TLK]);			
			take_hit(dam, killer, TRUE);
			break;
		}
		case GF_TK:
		{
			/* get special stun damge */
			int stundam;
			
			/* Affected by terrain. */
			dam += terrain_adjustment;
			
			if (fuzzy) msg_print("You are hit by a shock wave!");
			if ((rand_int(100) < save) || (resist_effect(RS_TLK)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else 
			{
  			/* Fix damage to avoid insta-kill */
  			stundam = dam;
  			
  			/* Limit stun damage */
  			if (stundam > 99) stundam = 99;
  				
  			/* set stun */	
				(void)set_stun(p_ptr->stun + randint(stundam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_TLK]);			
			take_hit(dam, killer, TRUE);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}
		case GF_FORCE:
		{
			/* get special stun damge */
			int stundam;

			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by a shock wave!");
			if ((rand_int(100) < save) || (resist_effect(RS_TLK)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else 
			{
  			/* Fix damage to avoid insta-kill */
  			stundam = dam;
  			
  			/* Limit stun damage */
  			if (stundam > 99) stundam = 99;
  				
  			/* set stun */	
				(void)set_stun(p_ptr->stun + randint(stundam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_TLK]);			
			take_hit(dam, killer, TRUE);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}
		case GF_CONFUSION:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something confusing!");
			if ((rand_int(100) < save) || (resist_effect(RS_PSI)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else if (p_ptr->resist_confu)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_range(dam / 2, dam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);			
			take_hit(dam / 4, killer, TRUE);
			break;
		}
		case GF_SPIRIT:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something confusing!");
			if ((rand_int(100) < save) || (resist_effect(RS_PSI)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else if (p_ptr->resist_confu)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_range(dam / 2, dam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);			
			take_hit(dam / 2, killer, TRUE);
			break;
		}
		case GF_ECTOPLASM:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something confusing!");
			if ((rand_int(100) < save) || (resist_effect(RS_PSI)))
			{
				msg_print("You resist the effects!");
				dam = dam / 5;
			}
			else if (p_ptr->resist_confu)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_range(dam / 2, dam));
			}
			dam = apply_resistance(dam, p_ptr->res[RS_PSI]);			
			take_hit(dam, killer, TRUE);
			break;
		}
		
		
		
		case GF_WHIP:
		case GF_ARROW:
		case GF_BULLET:
		case GF_SHOT:
		case GF_ROCKET:
		case GF_MISSILE:
		case GF_PYRO_SHOT:
		{
			/* Nothing special yet  XXX */

			dam = apply_resistance(dam, p_ptr->res[RS_TLK]);			
			/* No resists, adjusts, etc. */
			take_hit(dam, killer, TRUE);

			break;
		}

		/* Default */
		default:
		{
			/* No damage */
			dam = 0;
			return FALSE;
			
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
 * Handle movement of monsters and the player.  Handle the alteration of
 * grids that affect damage.  -LM-
 *
 * This function only checks grids marked with the CAVE_TEMP flag.  To
 * help creatures get out of each other's way, this function processes
 * from outside in.
 *
 * This accomplishes three things:  A creature now cannot be damaged/blinked
 * more than once in a single projection, if all teleport functions also
 * clear the CAVE_TEMP flag.  Also, terrain now affects damage taken, and
 * only then gets altered.  Also, any summoned creatures don't get hurt
 * by the magics that gave them birth.
 *
 * XXX XXX -- Hack -- because the CAVE_TEMP flag may be erased by certain
 * updates, we must be careful not to allow any of the teleport functions
 * called by this function to ask for one.  This works well in practice, but
 * is a definite hack.
 *
 * This function assumes that most messages have already been shown.
 */
static bool project_t(int who, int y, int x, int dam, int typ, u32b flg)
{
	monster_type *m_ptr = NULL;
	monster_race *r_ptr = NULL;

	char m_name[80];

	bool seen = FALSE;
	bool obvious = FALSE;

	bool affect_player = FALSE;
	bool affect_monster = FALSE;

	int do_dist = 0;

	/* Assume no note */
	cptr note = NULL;

	/* Only process marked grids. */
	if (!cave_info[y][x] & (CAVE_TEMP)) return (FALSE);

	/* Clear the cave_temp flag.  (this is paranoid) */
	cave_info[y][x] &= ~(CAVE_TEMP);


	/* Projection will be affecting a player. */
	if ((flg & (PROJECT_PLAY)) && (cave_m_idx[y][x] < 0))
		affect_player = TRUE;

	/* Projection will be affecting a monster. */
	if ((flg & (PROJECT_KILL)) && (cave_m_idx[y][x] > 0))
	{
		affect_monster = TRUE;
		m_ptr = &m_list[cave_m_idx[y][x]];
		r_ptr = &r_info[m_ptr->r_idx];
	}

	if (affect_player)
	{
		obvious = TRUE;
	}

	if (affect_monster)
	{
		/* Sight check. */
		if (m_ptr->ml) seen = TRUE;

		/* Get the monster name (before teleporting) */
		monster_desc(m_name, m_ptr, 0);
	}

	/* Analyze the type */
	switch (typ)
	{
		case GF_GUST:
		case GF_WIND:
		case GF_GALE:
		case GF_STORM:
		{
			if (affect_player)
			{
				if (!resist_effect(RS_AIR))
				{	
					/* Throw distance depends on weight and strength */
					int dist = div_round(25 * dam, p_ptr->wt);
					if (dist > 10) dist = 10;
	
					/* Feather fall greatly reduces the effect of wind */
					if (p_ptr->ffall) dist = (dist + 2) / 3;
	
					/* Messages */
					if (dist >= 6)
						msg_print("The wind grabs you, and whirls you around!");
					else if (dist >= 2)
						msg_print("The wind buffets you about.");
					
					/* Throw the player around unsafely. */
					teleport_player(dist);
				}
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 3 + div_round(dam, 25);

				/* Big, heavy monsters (or ghosts) */
				if (strchr("DGP#G", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("dgv", r_ptr->d_char)) do_dist /= 2;
			}

			break;
		}
		
		case GF_VAPOR:
		case GF_ETHER:
		case GF_NEXUS:
		{
			if (affect_player)
			{
				if (!resist_effect(RS_ETH))
				{
					/* Get caster */
					monster_type *n_ptr = &m_list[who];

					/* Various effects. */
					apply_nexus(n_ptr);
				}
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 4 + div_round(dam, 10);

				/* Resist even when affected */
				if      (r_ptr->flags3 & (RF3_IM_ETHER)) do_dist = 0;
				else if (r_ptr->flags4 & (RF4_BR_ETHER)) do_dist /= 4;
				else if ((r_ptr->flags5 & (RF5_BA_ETHER)) || 
						(r_ptr->flags5 & (RF5_BO_ETHER)))
							do_dist = 2 * do_dist / 3;
			}
			break;
		}
		case GF_TK:
		case GF_FORCE:
		{
			if (affect_monster)
			{
				/* Force breathers are immune */
				/* if (r_ptr->flags4 & (RF4_IM_FORCE)) break; */

				/* Big, heavy monsters */
				if (strchr("DGP#", r_ptr->d_char)) dam /= 3;
				else if (strchr("dgv", r_ptr->d_char)) dam /= 2;
			}

			if ((affect_monster) || (affect_player))
			{
				/* Thrust monster or player away. */
				thrust_away(who, y, x, 2 + div_round(dam, 15));

				/* Hack -- get new location */
				if (affect_monster)
				{
					y = m_ptr->fy;
					x = m_ptr->fx;
				}
			}

			break;
		}
		case GF_AWAY_UNDEAD:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				if (r_ptr->flags3 & (RF3_UNDEAD)) do_dist = dam;
			}
			break;
		}
		case GF_AWAY_ALIEN:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				if (r_ptr->flags3 & (RF3_ALIEN)) do_dist = dam;
			}
			break;
		}
		case GF_AWAY_AUTOMATA:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				if (r_ptr->flags3 & (RF3_AUTOMATA)) do_dist = dam;
			}
			break;
		}
		case GF_AWAY_BEASTMAN:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				if (r_ptr->flags3 & (RF3_BEASTMAN)) do_dist = dam;
			}
			break;
		}
		case GF_AWAY_EVIL:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				if (r_ptr->flags3 & (RF3_EVIL)) do_dist = dam;
			}
			break;
		}
		case GF_AWAY_ALL:
		{
			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				do_dist = dam;
			}
			break;
		}
		/* All other projection types have no effect. */
		default:
		{
			return (FALSE);
		}
	}

	/* Handle teleportation of monster */
	if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Teleport */
		teleport_away(cave_m_idx[y][x], do_dist);

		/* No movement */
		if ((y == m_ptr->fy) && (x == m_ptr->fx))
		{
			/* No message */
		}
		/* Visible (after teleport) */
		else if (m_ptr->ml)
		{
			/* No message */
		}
		else
		{
			/* Message */
			note = " disappears!";
		}

		/* Hack -- get new location */
		if (affect_monster)
		{
			y = m_ptr->fy;
			x = m_ptr->fx;
		}
	}

	if (affect_monster)
	{
		/* Give detailed messages if visible */
		if (note && seen)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Update the monster */
		(void)update_mon(cave_m_idx[y][x], FALSE, FALSE);

		/* Update monster recall window */
		if (p_ptr->monster_race_idx == m_ptr->r_idx)
		{
			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);
		}
	}

	return (obvious);
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
		arc_dist[i] = (unsigned char)(rand_range(dist / 4, dist / 2));

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
/* Replacing with Sang projection code by hand. Sang header follows. */
/* the above is old information. */
/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *   -BEN-, -LM-
 *
 * Input:
 *   who:             Index of "source" monster (negative for the character)
 *   rad:             Radius of explosion, or length of beam, or maximum
 *                    length of arc from the source.
 *   y0, x0:          Source location (Location to travel from)
 *   y1, x1:          Target location (Location to travel towards)
 *   dam:             Base damage to apply to monsters, terrain, objects,
 *                    or player
 *   typ:             Type of projection (fire, frost, dispel demons etc.)
 *   flg:             Bit flags that control projection behavior
 *   degrees:         How wide an arc spell is (in degrees).
 *   source_diameter: how wide the source diameter is.
 *
 * Return:
 *   TRUE if any effects of the projection were observed, else FALSE
 *
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
 * A bolt travels from source to target and affects only the final grid in its
 *   projection path.  If given the PROJECT_STOP flag, it is stopped by any
 *   monster or character in its path (at present, all bolts use this flag).
 *
 * Beam:  (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through
 *   with full damage.  It is never stopped by monsters in its path.  Beams
 *   may never be combined with any other projection type.
 *
 * Ball:  (positive radius, unless the PROJECT_ARC flag is set)
 * A ball travels from source towards the target, and always explodes.  Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in
 *   its path.  If given a target, it will explode on that target.  If a
 *   wall is in the way, it will explode against the wall.  If a ball reaches
 *   MAX_RANGE without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc:  (positive radius, with the PROJECT_ARC flag set)
 * An arc is a portion of a source-centered ball that explodes outwards
 *   towards the target grid.  Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area.  The width of arc spells is con-
 *   trolled by degrees.
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
 *   past that target.
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
 *   source diameters ranging up to 20, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *   It is expected, but not required, that wide arcs lose strength more
 *   quickly over distance.
 *
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
 * If the option "fresh_before" is on, or the delay factor is anything other
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
	int py = p_ptr->py;
	int px = p_ptr->px;
	

	int i, j, k, dist;

	u32b dam_temp;
	int centerline = 0;

	int y = y0;
	int x = x0;
	int n1y = 0;
	int n1x = 0;
	int y2, x2;

	int etheric = 0;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;

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

	/* HACK - XCCCX */
	if (p_ptr->skills[SK_ETHERIC_ATTUNE].skill_max > 0)
		etheric = p_ptr->skills[SK_ETHERIC_ATTUNE].skill_rank;

	/* Hack -- Flush any pending output */
	handle_stuff();

	/* Make certain that the radius is not too large */
	if (rad > MAX_SIGHT) rad = MAX_SIGHT;

	/* Handle effects to the caster of using a specific projection type. */
/*	if ((typ == GF_PROTECTION) && (who < 0))
 *	{
 *		if ((dam > 5) && (randint(2) < 2))
 *		{
 *			set_shield(p_ptr->shield + randint(dam / 2), NULL);
 *		}
 *	}
 */

	/* Some projection types always PROJECT_WALL. */
	/*  ||
		 (typ == GF_FORCE_DOOR) */
	if ((typ == GF_KILL_WALL) || (typ == GF_KILL_DOOR))
	{
		flg |= (PROJECT_WALL);
	}

	/* Hack -- Jump to target, but require a valid target */
	if ((flg & (PROJECT_JUMP)) && (y1) && (x1))
	{
		y0 = y1;
		x0 = x1;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);
	}

	/* If a single grid is both source and destination, store it. */
	if ((x1 == x0) && (y1 == y0))
	{
		gy[grids] = y0;
		gx[grids] = x0;
		gd[grids++] = 0;
	}

	/* Otherwise, unless an arc or a star, travel along the projection path. */
	else if (!(flg & (PROJECT_ARC | PROJECT_STAR)))
	{
		/* Determine maximum length of projection path */
		if (flg & (PROJECT_BOOM)) dist = MAX_RANGE;
		else if (rad <= 0)        dist = MAX_RANGE;
		else                      dist = rad;

		/* Monster is directing a projection at the character */
		/* I think I have to change this because */
		/* Monsters can project against other monsters */
		if ((who > 0)  && (flg & (PROJECT_PLAY)) && (y1 == py) && (x1 == px))
		{
			/* Projection is a bolt or beam that doesn't explode */
			if ((flg & (PROJECT_BEAM)) || !(flg & (PROJECT_BOOM)))
			{
				/* Get the source monster */
				monster_type *m_ptr = &m_list[who];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* paranoia */
				int player_ac = p_ptr->ac +  p_ptr->to_a;
				
				/* Take reaction time into account */
				int diff = 300 / (m_ptr->cdis + 2);
				if (!m_ptr->ml) diff += 100;
				if (diff < 21) diff = 21;

				/* Paranoia */
				if (player_ac < 1) player_ac = 1;
				
				/* Hack -- characters can sometimes dodge */
				if ((randint(player_ac) + (etheric * 2)) > 
					 (35 + rand_range(diff - 20, diff + 20) + r_ptr->level))
				{
					/* Missile whizzes right past the character */
					y1 += (y1 - y0) * MAX_SIGHT;
					x1 += (x1 - x0) * MAX_SIGHT;
					dist = MAX_SIGHT;

					flg &= ~(PROJECT_PLAY | PROJECT_STOP);

					msg_print("The missile misses!");
				}
			}
		}

		/* Calculate the projection path */
		path_n = project_path(path_g, dist, y0, x0, &y1, &x1, flg);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int oy = y;
			int ox = x;

			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);


			/* Hack -- Balls explode before reaching walls. */
			if ((flg & (PROJECT_BOOM)) && (!cave_floor_bold(ny, nx)))
			{
				break;
			}

			/* Advance */
			y = ny;
			x = nx;

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

			/* Only do visuals if requested */
			if (!blind && !(flg & (PROJECT_HIDE)))
			{
				/* Only do visuals if the player can "see" the projection */
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

					/* Display the visual effects */
					print_rel(c, a, y, x);
					move_cursor_relative(y, x);
					if (op_ptr->delay_factor) Term_fresh();

					/* Delay */
					Term_xtra(TERM_XTRA_DELAY, msec);

					/* Erase the visual effects */
					lite_spot(y, x);
					if (op_ptr->delay_factor) Term_fresh();

					/* Re-display the beam  XXX */
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
			spread_cave_temp(y0, x0, rad, FALSE);
		}

		/* Pre-calculate some things for arcs. */
		if (flg & (PROJECT_ARC))
		{
			/* The radius of arcs cannot be more than 20 */
			if (rad > 20) rad = 20;

			/* Reorient the grid forming the end of the arc's centerline. */
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

				/* This is a wall grid (whether passable or not). */
				if (!cave_floor_bold(y, x))
				{
					/* Spell with PROJECT_PASS ignore walls */
					if (!(flg & (PROJECT_PASS)))
					{
						/* This grid is passable, or PROJECT_WALL is active */
						if ((flg & (PROJECT_WALL)) || (cave_passable_bold(y, x)))
						{
							/* Allow grids next to grids in LOS of explosion center */
							for (i = 0, k = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Stay within dungeon */
								if (!in_bounds(yy, xx)) continue;

								if (los(y2, x2, yy, xx))
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
						if (los(y2, x2, y, x))
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
					if (flg & (PROJECT_PASS) || los(y2, x2, y, x))
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

		/* If a particular diameter for the source of the explosion's
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

	/* Display the blast area if allowed. */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Do the blast from inside out */
		for (i = 0; i < grids; i++)
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

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* New radius is about to be drawn */
			if ((i == grids - 1) || ((i < grids - 1) && (gd[i + 1] > gd[i])))
			{
				/* Flush each radius separately */
				if (op_ptr->delay_factor) Term_fresh();

				/* Delay (efficiently) */
				if (visual || drawn)
				{
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
		}

		/* Delay for a while if there are pretty graphics to show */
		if ((grids > 1) && (visual || drawn))
		{
			if (!op_ptr->delay_factor) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, 50 + msec);
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
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					lite_spot(y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush the explosion */
			if (op_ptr->delay_factor) Term_fresh();
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
			if (project_o(who, y, x, dam_at_dist[gd[i]], typ))
				notice = TRUE;
		}
	}

	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;
		death_count = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the monster in the grid */
			if (project_m(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
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

#if 0
		/* Hack -- Moria-style death messages for non-visible monsters */
		if ((death_count) && (who < 0))
		{
			/* One monster */
			if (death_count == 1)
			{
				msg_print("You hear a scream of agony!");
			}

			/* Several monsters */
			else
			{
				msg_print("You hear several screams of agony!");
			}
			/* Reset */
			death_count = 0;
		}
#endif
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
				if (project_p(who, y, x, dam_at_dist[gd[i]], typ))
					notice = TRUE;
			}
		}
	}

	/* Teleport monsters and player around, alter certain features. */
	for (i = 0; i < grids; i++)
	{
		/* Get the grid location */
		y = gy[i];
		x = gx[i];

		/* Grid must be marked. */
		if (!(cave_info[y][x] & (CAVE_TEMP))) continue;

		/* Affect marked grid */
		if (project_t(who, y, x, dam_at_dist[gd[i]], typ, flg)) notice = TRUE;
	}

	/* Clear the "temp" array  (paranoia is good) */
	clear_temp_array();

	/* Clear required flags */
	/* p_ptr->proj_mon_flags = 0L; */

	/* Clear racial immunity */
	/* project_immune = 0; */

	/* Allow potion-smashing and scroll activation */
	/* allow_activate = TRUE; */

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

	/* Return "something was noticed" */
	return (notice);
}
