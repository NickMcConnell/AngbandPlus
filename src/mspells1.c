

/* CVS: Last edit by $Author: rr9 $ on $Date: 1999/12/14 13:18:24 $ */
/* File: mspells2.c */

/* Purpose: Monster spells (attack monster) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Monster casts a breath (or ball) attack at another monster.
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void monst_ball_monst(int m_idx, int y, int x, int typ, int dam, int rad, bool jump)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	int my = m_ptr->fy;
	int mx = m_ptr->fx;


	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
	           PROJECT_PLAY;

	/* Optionally, allow the attack to "jump" to the player */
	if (jump) flg |= (PROJECT_JUMP);

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the monster with a ball attack */
	(void)project(m_idx, rad, my, mx, y, x, dam, typ, flg, 0, 0);
}

static void monst_arc_monst(int m_idx, int y, int x, int typ, int dam, int rad, int degrees_of_arc)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int my = m_ptr->fy;
	int mx = m_ptr->fx;

	u32b flg = PROJECT_ARC | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM |
	           PROJECT_KILL | PROJECT_PLAY;

	/* Diameter of source of energy is at least 20. */
	int diameter_of_source = 20;

	/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
	int degree_factor = (r_ptr->flags2 & (RF2_POWERFUL)) ? 120 : 60;

	/* Narrow arcs lose relatively little energy over distance. */
	if (degrees_of_arc < degree_factor)
	{
		if (degrees_of_arc <= 6) diameter_of_source = rad * 10;
		else diameter_of_source = diameter_of_source * degree_factor /
			degrees_of_arc;
	}

	/* Can optionally ignore monsters with the same r_idx. */
	/* if (noharm) project_immune = m_ptr->r_idx; */

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with an arc-shaped attack. */
	(void)project(m_idx, rad, my, mx, y, x, dam, typ, flg, degrees_of_arc,
		(byte)diameter_of_source);
}

static void monst_bolt_monst(int m_idx, int y, int x, int typ, int dam)
{
	monster_type *m_ptr = &m_list[m_idx];
	int my = m_ptr->fy;
	int mx = m_ptr->fx;

	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, my, mx, y, x, dam, typ, flg, 0, 0);
}


/*
 * Returns the radius of a ball spell given the power. 
 */
static int monst_spell_monst_getballrad(int spower)
{
	if (spower < 20)        
		return 1;
	else if (spower < 60)  
		return 2;
	else if (spower < 120) 
		return 3;
	else                    
		return 4;
}

/*
 * Returns the type of the ball spell given a minor and a major type.
 */
static int monst_spell_monst_getballtype(int spower, int minor_type, int major_type)
{
	if (spower < 60) 
		return minor_type;
	else             
		return major_type;
}

/*
 * Returns the type of the bolt spell given a minor and a major type.
 */
static int monst_spell_monst_getbolttype(int spower, int minor_type, int major_type)
{
	if (spower < 50)
		return minor_type;
	else             
		return major_type;
}

/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 *
 * The player is only disturbed if able to be affected by the spell.
 */
bool monst_spell_monst(int m_idx)
{
	int y = 0, x = 0;
	int i, t_idx;
	int thrown_spell, count = 0;
	int rlev, manacost, spower, rad;


	char m_name[160];
	char t_name[160];
	char m_poss[160];
	char ddesc[160];

	monster_type *m_ptr = &m_list[m_idx];
	monster_type *t_ptr;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *tr_ptr;

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u32b f4, f5, f6, f7;

	bool wake_up = FALSE;
	bool fear = FALSE;

	bool blind = (p_ptr->blind ? TRUE : FALSE);

	bool see_m = m_ptr->ml;
	bool see_t;
	bool see_either;
	bool see_both;
	bool known;
	bool stupid = FALSE;

	bool friendly = is_friendly(m_ptr);
	bool pet = is_pet(m_ptr);

	/* Is the monster organic? */
	bool nonliving = (monster_nonliving(r_ptr));

	/* Paranoia */
	rad = 1;
	
	/* Set stupid bool */
	if (r_ptr->flags2 & RF2_STUPID) stupid = TRUE;
	
	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Scan thru all monsters */
	for (i = 1; i < m_max; i++)
	{
		/* The monster itself isn't a target */
		if (i == m_idx) continue;

		t_idx = i;
		t_ptr = &m_list[t_idx];
		tr_ptr = &r_info[t_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, m_ptr, 0x22);

		/* Get the target's name (or "it") */
		monster_desc(t_name, t_ptr, 0);

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88);

		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Monster must be projectable */
		if (stupid)
		{
			if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, PROJECT_CHCK)) continue;
		}
		else
		{
			if (!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, PROJECT_CHCK)) continue;
			if (!clean_shot(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx, pet)) continue;
		}
		
		/* OK -- we've got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, m_ptr, 0x22);

		/* Get the target's name (or "it") */
		monster_desc(t_name, t_ptr, 0);

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88);

		/* Extract the racial spell flags */
		f4 = r_ptr->flags4;
		f5 = r_ptr->flags5;
		f6 = r_ptr->flags6;
		f7 = r_ptr->flags7;

		/* Stop if player is dead or gone */
		if (p_ptr->is_dead) return (FALSE);

		/* Handle "leaving" */
		if (p_ptr->leaving) return (FALSE);

		/* Choose a spell to cast */
		/* Spells we can not afford */
		remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

		/* No spells left */
		if (!f4 && !f5 && !f6 && !f7) return (0);

		/* Remove spells that have no benefit */
		/* Does not include the effects of player resists/immunities */
		remove_useless_spells(m_idx, &f4, &f5, &f6, &f7, FALSE);

		/* No spells left */
		if (!f4 && !f5 && !f6 && !f7) return (0);

		/* Monsters attacking other monsters are no longer dumb */
		/* due to anti-magic */
		thrown_spell = choose_ranged_attack(m_idx, &y, &x, FALSE, TRUE);
		
		see_t = t_ptr->ml;
		see_either = (see_m || see_t);
		see_both = (see_m && see_t);

		/* Can the player be aware of this attack? */
		known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

		/* Determine mana cost */
		if (thrown_spell >= 160) return (FALSE); /* this value was 224 */
		else if (thrown_spell >= 192) manacost = mana_cost_RF7[thrown_spell-192];
		else if (thrown_spell >= 160) manacost = mana_cost_RF6[thrown_spell-160]; 
		else if (thrown_spell >= 128) manacost = mana_cost_RF5[thrown_spell-128];
		else if (thrown_spell >=  96) manacost = mana_cost_RF4[thrown_spell- 96];
		else return (FALSE);
	
		/* Spend mana */
		m_ptr->mana -= manacost;

		/* Extract the monster level.  Must be at least 1. */
		/* This is apparently not used */
		rlev = MAX(1, r_ptr->level);
	
		/* Extract the monster's spell power.  Must be at least 1. */
		spower = MAX(1, r_ptr->spell_power);

		switch (thrown_spell)
		{
			/* RF4_SHRIEK */
			case 96+0:
			{
				if (known)
				{
					if (see_m) msg_format("%^s shrieks at %s.", m_name, t_name);
					else msg_format("%^s shrieks.", m_name);
				}
				aggravate_monsters(m_idx);
				break;
			}

			/* RF4_LASH */
			case 96+1:
			{
				/* Lash attacks don't work */ break;
			}

			/* RF4_ARROW */
			case 96+2:
			{
				if (known)
				{
					if (see_either)
					{
						if (spower < 8)
						{
							if (blind) msg_print("You hear a soft twang.");
							else msg_format("%^s fires a small arrow at %s.", m_name, t_name);
						}
						else if (spower < 15)
						{
							if (blind) msg_print("You hear a twang.");
							else msg_format("%^s fires an arrow at %s.", m_name, t_name);
						}
						else
						{
							if (blind) msg_print("You hear a loud thwang.");
							else msg_format("%^s fires a seeker arrow at %s.", m_name, t_name);
						}
			
					}
				}
				monst_bolt_monst(m_idx, y, x, GF_ARROW, get_dam(spower * 3, 6));
				break;
			}

			/* RF4_GUN */
			case 96+3:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (spower < 8)
						{
							if (blind) msg_print("You hear a pop!.");
							else msg_format ("%^s fires a small pistol at %s.", m_name, t_name);
						}
			
						else
						{
							if (blind) msg_print("You hear a loud bang!.");
							else msg_format ("%^s fires a pistol at %s.", m_name, t_name);
						}
					}	
				}
				monst_bolt_monst(m_idx, y, x, GF_BULLET, get_dam(spower * 4, 4));
				break;
			}

			/* RF4_RIFLE */
			case 96+4:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_print("You hear a rifle shot.");
						else msg_format("%^s fires a rifle at %s.", m_name, t_name);

					}
				}

				monst_bolt_monst(m_idx, y, x, GF_BULLET, get_dam(spower * 5, 4));
				break;
			}
			/* RF4_SHOTGUN */
			case 96+5:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_print("You hear a shotgun blast.");
						else msg_format("%^s fires a shotgun at %s!", m_name, t_name);
					}
				}

				monst_bolt_monst(m_idx, y, x, GF_SHOT, get_dam(spower * 7, 4));
				break;
			}
			/* RF4_ROCKET */
			case 96+6:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_print("You hear a loud roar.");
						else msg_format("%^s fires a rocket at %s!", m_name, t_name);
					}
				}
				monst_ball_monst(m_idx, y, x, GF_ROCKET, get_dam(spower * 7, 4), 3, FALSE);
				break;
			}

			/* RF4_MISSILE */
			case 96+7:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_print("You hear a loud roaring whine.");
						else msg_format("%^s fires a guided missile at %s!", m_name, t_name);
					}
				}
				monst_ball_monst(m_idx, y, x, GF_MISSILE, get_dam(spower * 11, 4), 3, FALSE);
				break;
			}

			/* RF4_BR_FIRE */
			case 96+8:
			{
				int typ;
				if (spower < 5) typ = GF_HEAT;
				else if (spower > 14) typ = GF_PLASMA;
				else typ = GF_FIRE;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes fire at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 500),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_ELEC */
			case 96+9:
			{
				int typ;
				if (spower < 5) typ = GF_ROCK;
				else if (spower > 14) typ = GF_SHARDS;
				else typ = GF_EARTH;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes earth at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 500),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_AIR */
			case 96+10:
			{
				int typ;
				if (spower < 5) typ = GF_GUST;
				else if (spower > 14) typ = GF_GALE;
				else typ = GF_WIND;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes wind at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 500),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_WATER */
			case 96+11:
			{
				int typ;
				if (spower < 5) typ = GF_RUST;
				else if (spower > 14) typ = GF_STORM;
				else typ = GF_STEAM;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else if (typ == GF_RUST) msg_format("%^s breathes rust at %s.", m_name, t_name);
						else if (typ == GF_STEAM) msg_format("%^s breathes steam at %s.", m_name, t_name);
						else if (typ == GF_STORM) msg_format("%^s breathes storm at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 1000),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_ELEC */
			case 96+12:
			{
				int typ;
				if (spower < 5) typ = GF_SHOCK;
				else if (spower > 14) typ = GF_VOLT;
				else typ = GF_ELEC;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes lightning at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 700),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_ICE */
			case 96+13:
			{
				int typ;
				if (spower < 5) typ = GF_CHILL;
				else if (spower > 14) typ = GF_GLACIAL;
				else typ = GF_ICE;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes ice at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 700),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_ACID */
			case 96+14:
			{
				int typ;
				if (spower < 5) typ = GF_CORROSIVE;
				else if (spower > 14) typ = GF_LIQUESCE;
				else typ = GF_ACID;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes acid at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 700),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_POISON */
			case 96+15:
			{
				int typ;
				if (spower < 5) typ = GF_CAUSTIC;
				else if (spower > 14) typ = GF_CONTAGION;
				else typ = GF_POISON;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes poison at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 700),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_TIME */
			case 96+16:
			{
				int typ;
				if (spower < 5) typ = GF_AGE;
				else if (spower > 14) typ = GF_CHRONOS;
				else typ = GF_TIME;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes temporal forces at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 900),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_ETHER */
			case 96+17:
			{
				int typ;
				if (spower < 5) typ = GF_VAPOR;
				else if (spower > 14) typ = GF_NEXUS;
				else typ = GF_ETHER;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes etheric forces at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 900),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_SOUND */
			case 96+18:
			{
				int typ;
				if (spower < 5) typ = GF_VIBE;
				else if (spower > 14) typ = GF_SONIC;
				else typ = GF_SOUND;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes sonic force at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 900),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_NETHER */
			case 96+19:
			{
				int typ;
				if (spower < 5) typ = GF_UNHOLY;
				else if (spower > 14) typ = GF_ABYSS;
				else typ = GF_NETHER;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes sonic force at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 900),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}


			/* RF4_BR_GRAVITY */
			case 96+20:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes gravity at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, GF_GRAVITY,
			       MIN(m_ptr->hp / 2, 350),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_RAD */
			case 96+21:
			{
				int typ;
				if (spower < 5) typ = GF_WEAK_RAD;
				else typ = GF_STRONG_RAD;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes radiation at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 250),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_LIGHT */
			case 96+22:
			{
				int typ;
				if (spower < 5) typ = GF_GLOW;
				else if (spower > 14) typ = GF_BRILLIANCE;
				else typ = GF_LIGHT;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes photons at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 150),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_BR_DARK */
			case 96+23:
			{
				int typ;
				if (spower < 5) typ = GF_DIM;
				else if (spower > 14) typ = GF_TENEBROUS;
				else typ = GF_DARK;
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s breathes at %s.", m_name, t_name);
						else msg_format("%^s breathes anti-photons at %s.", m_name, t_name);
					}
					else msg_format("%^s breathes.", m_name);
				}
				monst_arc_monst(m_idx, y, x, typ,
			       MIN(m_ptr->hp / 2, 200),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
				break;
			}

			/* RF4_CLOUD_RAD */
			case 96+24:
			{
				/* Nothing */
				/* Monsters breathing clouds at other monsters is bad */
				break;
			}

			/* RF4_XXX2 */
			case 96+25:
			{
				/* Nothing */
				/* Monsters breathing clouds at other monsters is bad */
				break;
			}

			/* RF4_XXX3 */
			case 96+26:
			{
				break;
			}

			/* RF4_XXX4 */
			case 96+27:
			{
				break;
			}

			/* RF4_XXX5 */
			case 96+28:
			{
				break;
			}

			/* RF4_XXX6 */
			case 96+29:
			{
				break;
			}

			/* RF4_XXX7 */
			case 96+30:
			{
				break;
			}

			/* RF4_XXX8 */
			case 96+31:
			{
				break;
			}

			/* RF5_BA_FIRE */
			case 128+0:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_FIRE, GF_PLASMA);
				
				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_FIRE)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a gout of fire at %s.", m_name, t_name);
								else msg_format("%^s casts an fireball at %s.", m_name, t_name);
							}
							else /* rad = 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sphere of fire at %s.", m_name, t_name);
								else msg_format("%^s casts an firesphere at %s.", m_name, t_name);
							}
						}
						else /* typ = GF_PLASMA */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a storm of super-heated plasma at %s.", m_name, t_name);
							else msg_format("%^s invokes a plasma-storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(5 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_EARTH */
			case 128+1:
			{
				int typ;

				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_EARTH, GF_SHARDS);

				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_EARTH)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges rocks and earth at %s.", m_name, t_name);
								else msg_format("%^s casts an rockball at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sphere of rocks and dirt at %s.", m_name, t_name);
								else msg_format("%^s casts an earthsphere at %s.", m_name, t_name);
							}
						}
						else /* typ = GF_SHARDS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a storm of deadly shards at %s.", m_name, t_name);
							else msg_format("%^s invokes a shard-storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(5 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_AIR */
			case 128+2:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_WIND, GF_GALE);
				
				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_WIND)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a blast of wind at %s.", m_name, t_name);
								else msg_format("%^s casts an windball at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a swirling winds at %s.", m_name, t_name);
								else msg_format("%^s casts an windsphere at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_GALE */
						{
							if (blind) msg_format("%^s makes a lot of noise at %s.", m_name, t_name);
							else if (nonliving) msg_format("%^s discharges a storm of high powered gales of wind at %s.", m_name, t_name);
							else msg_format("%^s invokes a gale-storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(5 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_WATER */
			case 128+3:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_STEAM, GF_STORM);
				
				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_STEAM)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a gout of steam at %s.", m_name, t_name);
								else msg_format("%^s casts an steamball at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a gouts of steam at %s.", m_name, t_name);
								else msg_format("%^s casts an steamsphere at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_STORM */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a storm of super-heated water at %s.", m_name, t_name);
							else msg_format("%^s invokes a storm of super-heated water at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(6 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_ELEC */
			case 128+4:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_ELEC, GF_VOLT);
				
				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_ELEC)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a spark at %s.", m_name, t_name);
								else msg_format("%^s casts an lightning ball at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a voltage shower at %s.", m_name, t_name);
								else msg_format("%^s casts an electricty ball at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_VOLT */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a storm of electricty at %s.", m_name, t_name);
							else msg_format("%^s invokes a lightning storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_ICE */
			case 128+5:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_ICE, GF_GLACIAL);
				
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_ICE)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges an icicle at %s.", m_name, t_name);
								else msg_format("%^s casts an snowball at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sphere of thermal statis at %s.", m_name, t_name);
								else msg_format("%^s casts an ice ball at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_GLACIAL */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges sphere of total entropic thermal statis at %s.", m_name, t_name);
							else msg_format("%^s invokes a glacial storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_ACID */
			case 128+6:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_ACID, GF_LIQUESCE);

				if (known)
				{
					if (see_either)
					{
						if (typ == GF_ACID)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a splash of acid at %s.", m_name, t_name);
								else msg_format("%^s casts an acid splash at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a gout of corrosive at %s.", m_name, t_name);
								else msg_format("%^s casts an acid ball at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_LIQUESCE */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a shower of molecular bond destroyer at %s.", m_name, t_name);
							else msg_format("%^s invokes a acid storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_POISON */
			case 128+7:
			{
				int typ;

				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_POISON, GF_CONTAGION);

				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_POISON)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges an organic toxin at %s.", m_name, t_name);
								else msg_format("%^s casts a poison splash at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a cloud of organic toxin at %s.", m_name, t_name);
								else msg_format("%^s casts a poison gas cloud at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_CONTAGION */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a giant cloud of toxins at %s.", m_name, t_name);
							else msg_format("%^s invokes a poison storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_TIME */
			case 128+8:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_TIME, GF_CHRONOS);

				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_TIME)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a taychon beam at %s.", m_name, t_name);
								else msg_format("%^s manipulates chronos at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sphere of taychon particles at %s.", m_name, t_name);
								else msg_format("%^s casts a time field at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_CHRONOS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges an intense storm of taychon energy at %s.", m_name, t_name);
							else msg_format("%^s invokes a time storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_ETHER */
			case 128+9:
			{
				int typ;
				
				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_ETHER, GF_NEXUS);
				
				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_ETHER)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges etheric forces at %s.", m_name, t_name);
								else msg_format("%^s manipulates etheric forces at %s.", m_name, t_name);
							}
							else /* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sphere of etheric energy at %s.", m_name, t_name);
								else msg_format("%^s casts an etheric field at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_NEXUS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s warps etheric energies at %s.", m_name, t_name);
							else msg_format("%^s invokes a ether storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_SOUND */
			case 128+10:
			{
				int typ;

				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_SOUND, GF_SONIC);

				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_SOUND)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								else if (nonliving) msg_format("%^s discharges a focused blast of sound at %s.", m_name, t_name);
								else msg_format("%^s casts a thunderclap at %s.", m_name, t_name);
							}
							/* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								else if (nonliving) msg_format("%^s discharges a sound-sphere at %s.", m_name, t_name);
								else msg_format("%^s casts a sonic field at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_SONIC */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s discharges a sonic blast at %s.", m_name, t_name);
							else msg_format("%^s invokes a sonic storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_NETHER */
			case 128+11:
			{
				int typ;

				rad = monst_spell_monst_getballrad(spower);
				typ = monst_spell_monst_getballtype(spower, GF_NETHER, GF_ABYSS);

				if (known)
				{
					if (see_either)
					{
						disturb (1, 0);
						if (typ == GF_NETHER)
						{
							if (rad == 1)
							{
								if (blind) msg_format("%^s makes noise.", m_name);
								/* Non-living things shouldn't use nether */
								/* else if (nonliving) msg_format("%^s discharges a taychon beam", m_name); */
								else msg_format("%^s draws on unholy forces.", m_name);
							}
							/* rad == 2 */
							{
								if (blind) msg_format("%^s makes deep noises.", m_name);
								/* Non-living things shouldn't use nether */
								/* else if (nonliving) msg_format("%^s discharges a taychon beam", m_name); */
								else msg_format("%^s casts a ball of dark forces at %s.", m_name, t_name);
							}
						}
						else /* typ == GF_ABYSS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							/* Non-living things shouldn't use nether */
							/* else if (nonliving) msg_format("%^s discharges a taychon beam", m_name); */
							else msg_format("%^s invokes a nether storm at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, typ, get_dam(7 * spower, 6), rad, FALSE);
				break;
			}

			/* RF5_BA_GRAVITY */
			case 128+12:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s makes noise.", m_name);
						else if (nonliving) msg_format("%^s creates a gravity wave at %s.", m_name, t_name);
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_ball_monst(m_idx, y, x, GF_GRAVITY, get_dam(8 * spower, 5), 3, FALSE);
				break;
			}

			/* RF5_BA_EMP */
			case 128+13:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						/* Caster of emp must be organic */
						if (blind) msg_format("%^s makes noise.", m_name);
						/* No non-living */
						else msg_format("%^s sets off an emp blast!", m_name);
					}
					else msg_format("%^s makes noise.", m_name);
				}
				/* Watch out! This attack is strong! */
				monst_ball_monst(m_idx, y, x, GF_EMP, get_dam(10 * spower, 4), 5, FALSE);
				break;
			}

			/* RF5_CAUSE_3 */
			case 128+14:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s makes noise.", m_name);
						else if (nonliving) msg_format("%^s emits radiation!", m_name);
						/* Should only be cthuloid */
						else msg_format("%^s draws on dark forces of chaos and mutation!", m_name);
					}
					else msg_format("%^s makes noise.", m_name);
				}
				if (spower < 60)
					monst_ball_monst(m_idx, y, x, GF_WEAK_RAD, get_dam(8 * spower, 4), 3, FALSE);
				else monst_ball_monst(m_idx, y, x, GF_STRONG_RAD, get_dam(9 * spower, 4), 4, FALSE);

				
				break;
			}

			/* RF5_XXX1 */
			case 128+15:
			{
				break;
			}

			/* RF5_BO_FIRE */
			case 128+16:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_FIRE, GF_PLASMA);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_FIRE)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of fire at %s.", m_name, t_name);
							else msg_format("%^s casts an fire bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_PLASMA */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of super-heated plasma at %s.", m_name, t_name);
							else msg_format("%^s casts a bolt of plasma at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(4 * spower, 6));
				break;
			}

			/* RF5_BO_EARTH */
			case 128+17:
			{              
				int typ = monst_spell_monst_getbolttype(spower, GF_EARTH, GF_SHARDS);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_EARTH)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots rocks at %s.", m_name, t_name);
							else msg_format("%^s casts an earth bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_SHARDS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots shards at %s.", m_name, t_name);
							else msg_format("%^s casts a bolt of shards at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(4 * spower, 6));
				break;
			}

			/* RF5_BO_AIR */
			case 128+18:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_WIND, GF_GALE);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_WIND)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s blows high-pressured air at %s.", m_name, t_name);
							else msg_format("%^s casts a gust of wind at %s.", m_name, t_name);
						}
						else /* typ == GF_GALE */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s blows gale winds at you at %s.", m_name, t_name);
							else msg_format("%^s casts a cyclone at you at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(4 * spower, 6));
				break;
			}

			/* RF5_BO_WATER */
			case 128+19:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_STEAM, GF_STORM);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_STEAM)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a blast of steam at %s.", m_name, t_name);
							else msg_format("%^s casts an steam bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_STORM */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of super-heated steam at %s.", m_name, t_name);
							else msg_format("%^s casts a storming bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(4 * spower, 6));
				break;
			}

			/* RF5_BO_ELEC */
			case 128+20:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_ELEC, GF_VOLT);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_ELEC)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of lightning. at %s.", m_name, t_name);
							else msg_format("%^s casts an lightning bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_VOLT */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of high-voltage lightning at %s.", m_name, t_name);
							else msg_format("%^s casts a powerful lightning bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_ICE */
			case 128+21:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_ICE, GF_GLACIAL);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_ICE)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of ice at %s.", m_name, t_name);
							else msg_format("%^s casts an ice bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_GLACIAL */
						{ 
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of freezing cold at %s.", m_name, t_name);
							else msg_format("%^s casts a glacial ice bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_ACID */
			case 128+22:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_ACID, GF_LIQUESCE);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_ACID)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of acid at %s.", m_name, t_name);
							else msg_format("%^s casts an acid bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_LIQUESCE */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of high-powered corrosive at %s.", m_name, t_name);
							else msg_format("%^s casts a corrosive bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_POISON */
			case 128+23:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_POISON, GF_CONTAGION);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_POISON)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of toxin at %s.", m_name, t_name);
							else msg_format("%^s casts an toxic bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_CONTAGION */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of poison at %s.", m_name, t_name);
							else msg_format("%^s casts a poison bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_TIME */
			case 128+24:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_TIME, GF_CHRONOS);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_TIME)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of taychons at %s.", m_name, t_name);
							else msg_format("%^s casts an time bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_CHRONOS */
						{
							/* bleah, need better visual/written cues */
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of taychons at %s.", m_name, t_name);
							typ = GF_CHRONOS;
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_ETHER */
			case 128+25:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_ETHER, GF_NEXUS);
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (typ == GF_ETHER)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of etheric forces at %s.", m_name, t_name);
							else msg_format("%^s casts an ether bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_NEXUS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of etheric forces at %s.", m_name, t_name);
							else msg_format("%^s casts a nexus bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_SOUND */
			case 128+26:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_SOUND, GF_SONIC);
				if (known)
				{
					if (see_either)
					{
						if (typ == GF_SOUND)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a blast of sound at %s.", m_name, t_name);
							else msg_format("%^s casts an sound bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_SONIC */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							else if (nonliving) msg_format("%^s shoots a bolt of sonic power at %s.", m_name, t_name);
							else msg_format("%^s casts a sonic bolt at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BO_NETHER */
			case 128+27:
			{
				int typ = monst_spell_monst_getbolttype(spower, GF_NETHER, GF_ABYSS);
				if (known)
				{
					if (see_either)
					{
						if (typ == GF_NETHER)
						{
							if (blind) msg_format("%^s makes noise.", m_name);
							/* NOPE- not if you're a machine, how are you getting in touch with */
							/* Dark forces anyway? */
							/* else if (nonliving) msg_format("%^s shoots a bolt of.", m_name); */
							else msg_format("%^s casts an nether bolt at %s.", m_name, t_name);
						}
						else /* typ == GF_ABYSS */
						{
							if (blind) msg_format("%^s makes a lot of noise.", m_name);
							/* NOPE- not if you're a machine, how are you getting in touch with */
							/* Dark forces anyway? */
							/* else if (nonliving) msg_format("%^s shoots a bolt of.", m_name); */
							else msg_format("%^s casts a bolt of nefarious dark energies at %s.", m_name, t_name);
						}
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, typ,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_BLIND */
			case 128+28:
			{
				if (known)
				{
					if (see_either)
					{
						disturb(1, 0);
						if (blind) msg_format("%^s makes a lot of noise.", m_name);
						else if (nonliving) msg_format("%^s shoots a bolt of graviton waves at %s.", m_name, t_name);
						else msg_format("%^s casts a gravity bolt at %s.", m_name, t_name);
					}
					else msg_format("%^s makes noise.", m_name);
				}
				monst_bolt_monst(m_idx, y, x, GF_GRAVITY,
					get_dam(5 * spower, 6));
				break;
			}

			/* RF5_XXX2 */
			case 128+29:
			{
				break;
			}

			/* RF5_XXX3 */
			case 128+30:
			{
				break;
			}

			/* RF5_XXX4 */
			case 128+31:
			{
				break;
			}
		}

		if (wake_up)
		{
			t_ptr->csleep = 0;
		}

		if (fear && see_t)
		{
			msg_format("%^s flees in terror!", t_name);
		}

		/* Remember what the monster did to us */
		if (see_m)
		{
			/* Innate spell */
			if (thrown_spell < 32*4)
			{
			l_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
			if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
			}
	
			/* Bolt or Ball */
			else if (thrown_spell < 32*5)
			{
			l_ptr->r_flags5 |= (1L << (thrown_spell - 32*4));
			if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
			}
	
			/* Special spell */
			else if (thrown_spell < 32*6)
			{
			l_ptr->r_flags6 |= (1L << (thrown_spell - 32*5));
			if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
			}
	
			/* Summon spell */
			else if (thrown_spell < 32*7)
			{
			l_ptr->r_flags7 |= (1L << (thrown_spell - 32*6));
			if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
			}
	
			/* Remember special flags */
			if (r_ptr->flags2 & (RF2_ARCHER)) l_ptr->r_flags2 |= RF2_ARCHER;
		}
	
	
		/* Always take note of monsters that kill you */
		if (p_ptr->is_dead && (l_ptr->r_deaths < MAX_SHORT))
		{
			l_ptr->r_deaths++;
		}

		/* A spell was cast */
		return (TRUE);
	}

	/* No enemy found */
	return (FALSE);
}
