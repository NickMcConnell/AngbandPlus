/* File: monster1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };
static cptr wd_his[3] =
{ "its", "his", "her" };


/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c,s,p) \
	(((c) == 1) ? (s) : (p))






/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = l_list[r_idx].r_tkills;

	/* Normal monsters */
	if (kills > 162 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if (kills > 162 / (38 + (5*level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "mana" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_mana(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = l_list[r_idx].r_tkills;

	/* Mages learn quickly. */
	/* if (cp_ptr->spell_book == TV_MAGIC_BOOK) kills *= 2; */

	/* Normal monsters */
	if (kills > 162 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if (kills > 162 / (38 + (5*level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(int r_idx, int i)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = (r_ptr->level * 2);

	s32b a = l_list[r_idx].r_blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* Hack - keep the target number reasonable */
	if (d > 100) d = 100;

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/* Should rewrite py_attack to return this information for us */
static int hit_chance()
{
	/* Calculate the chance to hit */
	int chance = 1;

	/* Calculate the "attack quality" */
	int bonus = 0;
	u32b f1, f2, f3;

	object_type *o_ptr;

	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);
	
	bonus = p_ptr->to_h + o_ptr->to_h;
	
	/* add in the accurate strike skill to the bonus */
	if (p_ptr->skills[SK_ACC_STRIKE].skill_max > 0)
	{
		bonus += p_ptr->skills[SK_ACC_STRIKE].skill_rank;
	}
	if (bonus < 0) bonus = 0;

	/* this should always be true, just checking */
	if (p_ptr->skills[SK_TOHIT].skill_max > 0)
	{
		chance = ((p_ptr->skills[SK_TOHIT].skill_rank * 5) + (bonus * BTH_PLUS_ADJ));
	}

	if (!o_ptr->k_idx)
	{
		if (p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 0)
		{
			chance += p_ptr->skills[SK_TOHIT_MARTIAL].skill_rank * 6;
		}
		if(p_ptr->skills[SK_INTER_MARTIAL].skill_max > 0)
		{
			chance += p_ptr->skills[SK_INTER_MARTIAL].skill_rank * 5;
		}
		if(p_ptr->skills[SK_ADV_MARTIAL].skill_max > 0)
		{
			chance += p_ptr->skills[SK_ADV_MARTIAL].skill_rank * 4;
		}
	}

	/* add in the relevant combat skills */
	else
	{
		if (p_ptr->skills[SK_INTER_COMBAT].skill_max > 0)
		{
			chance += p_ptr->skills[SK_INTER_COMBAT].skill_rank * 4;
		}
		if(p_ptr->skills[SK_ADV_COMBAT].skill_max > 0)
		{
			chance += p_ptr->skills[SK_ADV_COMBAT].skill_rank * 3;
		}
		if(p_ptr->skills[SK_MASTER_COMBAT].skill_max > 0)
		{
			chance += p_ptr->skills[SK_MASTER_COMBAT].skill_rank * 2;
		}
	}
	if (p_ptr->skills[SK_HAFTED].skill_max > 0 && o_ptr->tval == TV_HAFTED)
	{
		chance += p_ptr->skills[SK_HAFTED].skill_rank * 4;
	}
	if (p_ptr->skills[SK_HAFTED_MASTER].skill_max > 0 && o_ptr->tval == TV_HAFTED)
	{
		chance += p_ptr->skills[SK_HAFTED_MASTER].skill_rank * 6;
	}
	if (p_ptr->skills[SK_POLEARM].skill_max > 0 && o_ptr->tval == TV_POLEARM)
	{
		chance += p_ptr->skills[SK_POLEARM].skill_rank * 4;
	}
	if (p_ptr->skills[SK_POLEARM_MASTER].skill_max > 0 && o_ptr->tval == TV_POLEARM)
	{
		chance += p_ptr->skills[SK_POLEARM_MASTER].skill_rank * 6;
	}
	if (p_ptr->skills[SK_SWORD].skill_max > 0 && o_ptr->tval == TV_SWORD)
	{
		chance += p_ptr->skills[SK_SWORD].skill_rank * 4;
	}
	if (p_ptr->skills[SK_SWORD_MASTER].skill_max > 0 && o_ptr->tval == TV_SWORD)
	{
		chance += p_ptr->skills[SK_SWORD_MASTER].skill_rank * 6;
	}
	if (p_ptr->skills[SK_DAGGER].skill_max > 0 && o_ptr->tval == TV_DAGGER)
	{
		chance += p_ptr->skills[SK_DAGGER].skill_rank * 4;
	}
	if (p_ptr->skills[SK_DAGGER_MASTER].skill_max > 0 && o_ptr->tval == TV_DAGGER)
	{
		chance += p_ptr->skills[SK_DAGGER_MASTER].skill_rank * 6;
	}
	if (p_ptr->skills[SK_AXES].skill_max > 0 && o_ptr->tval == TV_AXES)
	{
		chance += p_ptr->skills[SK_AXES].skill_rank * 4;
	}
	if (p_ptr->skills[SK_AXES_MASTER].skill_max > 0 && o_ptr->tval == TV_AXES)
	{
		chance += p_ptr->skills[SK_AXES_MASTER].skill_rank * 6;
	}
	if (p_ptr->skills[SK_BLUNT].skill_max > 0 && o_ptr->tval == TV_BLUNT)
	{
		chance += p_ptr->skills[SK_BLUNT].skill_rank * 4;
	}
	if (p_ptr->skills[SK_BLUNT_MASTER].skill_max > 0 && o_ptr->tval == TV_BLUNT)
	{
		chance += p_ptr->skills[SK_BLUNT_MASTER].skill_rank * 6;
	}
	if (chance < 1) chance = 1;
	
	return chance;

}

/*
 * Helper functions for roff_aux, modelled after the ones in NPPAngband.
 * - CJN-
 */

/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(int r_idx, monster_lore *l_ptr)
{
	int m;

	const monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- Maximal kills */
	l_ptr->r_tkills = MAX_SHORT;
	
	/* Hack -- Maximal info */
	l_ptr->r_wake = l_ptr->r_ignore = MAX_UCHAR;
	
	/* Observe "maximal" attacks */
	for (m = 0; m < 4; m++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
		{
			/* Hack -- maximal observations */
			l_ptr->r_blows[m] = MAX_UCHAR;
		}
	}
	
	/* Hack -- maximal drops */
	l_ptr->r_drop_gold = l_ptr->r_drop_item =
		(((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_80)) ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_40)) ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_20)) ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_10))  ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_5))  ? 1 : 0));
	
	/* Hack -- but only "valid" drops */
	if (r_ptr->flags1 & (RF1_ONLY_GOLD)) l_ptr->r_drop_item = 0;
	if (r_ptr->flags1 & (RF1_ONLY_ITEM)) l_ptr->r_drop_gold = 0;
	
	/* Hack -- observe many spells */
	l_ptr->r_ranged = MAX_UCHAR;
	
	/* Hack -- know all the flags */
	l_ptr->r_flags1 = r_ptr->flags1;
	l_ptr->r_flags2 = r_ptr->flags2;
	l_ptr->r_flags3 = r_ptr->flags3;
	l_ptr->r_flags4 = r_ptr->flags4;
	l_ptr->r_flags5 = r_ptr->flags5;
	l_ptr->r_flags6 = r_ptr->flags6;
	l_ptr->r_flags7 = r_ptr->flags7;
	l_ptr->r_flags8 = r_ptr->flags8;

}

static void describe_monster_desc(int r_idx)
{
	const monster_race *r_ptr = &r_info[r_idx];
	char buf[2048];

	/* Simple method */
	strcpy(buf, r_text + r_ptr->text);

	/* Dump it */
	roff(buf);
	roff("  ");
		
}


static void describe_monster_spells(int r_idx, const monster_lore *l_ptr, 
				    bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];


	int m, n;
	int msex = 0;

	int spower;

	bool breath = FALSE;
	bool magic = FALSE;

	int vn;
	cptr vp[64];

	/* Paranoia */
	m = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	/* Get spell power */
	spower = r_ptr->spell_power;


	/* Collect inate attacks */
	vn = 0;
	if (l_ptr->r_flags4 & (RF4_SHRIEK))		vp[vn++] = "shriek for help";
	if (l_ptr->r_flags4 & (RF4_LASH))		vp[vn++] = "lash with a whip";
	if (l_ptr->r_flags4 & (RF4_ARROW))		vp[vn++] = "fire an arrow";
	if (l_ptr->r_flags4 & (RF4_GUN))			vp[vn++] = "fire a gun";
	if (l_ptr->r_flags4 & (RF4_RIFLE))		vp[vn++] = "fire a rifle";
	if (l_ptr->r_flags4 & (RF4_SHOTGUN))		vp[vn++] = "fire a shotgun";
	if (l_ptr->r_flags4 & (RF4_ROCKET))		vp[vn++] = "fire a rocket";
	if (l_ptr->r_flags4 & (RF4_MISSILE))		vp[vn++] = "fire a guided missile";

	if (table)
	{
		roff("Inate attacks:\t");
	}
	/* Describe inate attacks */
	if (vn)
	{
		if (!table) {
			/* Intro */
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0)
			{
				roff(" may ");
				if (l_ptr->r_flags2 & (RF2_ARCHER)) roff("frequently ");
			}
			else if (n < vn-1) roff(", ");
			else if (n == 1) roff(" or ");
			else roff(", or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}

	/* Collect breaths */
	vn = 0;

	if (l_ptr->r_flags4 & (RF4_BR_FIRE))
	{
		if (spower < 5)				vp[vn++] = "heat";
		else if (spower > 14)		vp[vn++] = "plasma";
		else 						vp[vn++] = "fire";
	}
	if (l_ptr->r_flags4 & (RF4_BR_EARTH))
	{
		if (spower < 5) 			vp[vn++] = "rocks";
		else if (spower > 14) 		vp[vn++] = "shards";
		else 						vp[vn++] = "earth";
	}
	if (l_ptr->r_flags4 & (RF4_BR_AIR))
	{
		if (spower < 5) 			vp[vn++] = "gusts";
		else if (spower > 14) 		vp[vn++] = "gales";
		else 						vp[vn++] = "wind";
	}
	if (l_ptr->r_flags4 & (RF4_BR_WATER))
	{
		if (spower < 5) 			vp[vn++] = "rust";
		else if (spower > 14) 		vp[vn++] = "storm";
		else 						vp[vn++] = "steam";
	}
	if (l_ptr->r_flags4 & (RF4_BR_ELEC))
	{
		if (spower < 5) 			vp[vn++] = "shocking electricty";
		else if (spower > 14) 		vp[vn++] = "voltaic energy";
		else 						vp[vn++] = "lightning";
	}
	if (l_ptr->r_flags4 & (RF4_BR_ICE))
	{
		if (spower < 5) 			vp[vn++] = "chill";
		else if (spower > 14) 		vp[vn++] = "glacial cold";
		else 						vp[vn++] = "ice";
	}
	if (l_ptr->r_flags4 & (RF4_BR_ACID))	
	{
		if (spower < 5) 			vp[vn++] = "corrosive";
		else if (spower > 14) 		vp[vn++] = "strong acid";
		else 						vp[vn++] = "acid";
	}
	if (l_ptr->r_flags4 & (RF4_BR_POISON))
	{
		if (spower < 5) 			vp[vn++] = "caustic poison";
		else if (spower > 14) 		vp[vn++] = "violent contagion";
		else 						vp[vn++] = "poison";
	}
	if (l_ptr->r_flags4 & (RF4_BR_TIME))		vp[vn++] = "temporal forces";
	if (l_ptr->r_flags4 & (RF4_BR_ETHER))	vp[vn++] = "etheric forces";
	if (l_ptr->r_flags4 & (RF4_BR_SOUND)) 	vp[vn++] = "sonic force";
	if (l_ptr->r_flags4 & (RF4_BR_NETHER)) 	vp[vn++] = "nether";
	if (l_ptr->r_flags4 & (RF4_BR_GRAVITY)) 	vp[vn++] = "gravity";
	if (l_ptr->r_flags4 & (RF4_BR_RAD))
	{
		if (spower < 5) 			vp[vn++] = "weak radiation";
		else vp[vn++] = "radiation";
	}
	if (l_ptr->r_flags4 & (RF4_BR_LIGHT)) 	vp[vn++] = "light";
	if (l_ptr->r_flags4 & (RF4_BR_DARK))		vp[vn++] = "darkness";
	if (l_ptr->r_flags4 & (RF4_CLOUD_RAD))
	{
		if (spower < 10) vp[vn++] = "a cloud of weak radiation";
		else vp[vn++] = "a cloud of radiation";
	}
	if (l_ptr->r_flags4 & (RF4_CLOUD_POISON))
	{
		if (spower < 5) 			vp[vn++] = "a cloud of caustic poison";
		else if (spower > 14) 		vp[vn++] = "a cloud of violent contagion";
		else 						vp[vn++] = "a cloud of poison";
	}
	if (l_ptr->r_flags4 & (RF4_XXX3))		vp[vn++] = "something";
	if (l_ptr->r_flags4 & (RF4_XXX4))		vp[vn++] = "something";
	if (l_ptr->r_flags4 & (RF4_XXX5))		vp[vn++] = "something";
	if (l_ptr->r_flags4 & (RF4_XXX6))		vp[vn++] = "something";
	if (l_ptr->r_flags4 & (RF4_XXX7)) 		vp[vn++] = "something";
	if (l_ptr->r_flags4 & (RF4_XXX8))		vp[vn++] = "something";

	if (table)
	{
		roff("Breaths:\t");
	}
	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		if (!table) {
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) {
				if (!table) {
					roff(" may breathe ");
				}
			}
			else if (n < vn-1) roff(", ");
			else if (n == 1) roff(" or ");
			else roff(", or ");

			/* Dump */
			roff(vp[n]);
		}
		if (l_ptr->r_flags2 & (RF2_POWERFUL)) roff(" powerfully");
	}


	/* Collect spells */
	vn = 0;

	if (l_ptr->r_flags5 & (RF5_BA_FIRE))			vp[vn++] = "produce fire balls";
	if (l_ptr->r_flags5 & (RF5_BA_EARTH))		vp[vn++] = "produce earth balls";
	if (l_ptr->r_flags5 & (RF5_BA_AIR))			vp[vn++] = "produce wind storms";
	if (l_ptr->r_flags5 & (RF5_BA_WATER))		vp[vn++] = "produce steam storms";
	if (l_ptr->r_flags5 & (RF5_BA_ELEC))			vp[vn++] = "produce electricty spheres";
	if (l_ptr->r_flags5 & (RF5_BA_ICE))			vp[vn++] = "produce ice storms";
	if (l_ptr->r_flags5 & (RF5_BA_ACID))			vp[vn++] = "produce acid balls";
	if (l_ptr->r_flags5 & (RF5_BA_POISON))		vp[vn++] = "produce poison balls";
	if (l_ptr->r_flags5 & (RF5_BA_TIME))			vp[vn++] = "invoke time storms";
	if (l_ptr->r_flags5 & (RF5_BA_ETHER))		vp[vn++] = "invoke ether storms";
	if (l_ptr->r_flags5 & (RF5_BA_SOUND))		vp[vn++] = "invoke sonic blasts";
	if (l_ptr->r_flags5 & (RF5_BA_NETHER))		vp[vn++] = "invoke nether storms";
	if (l_ptr->r_flags5 & (RF5_BA_GRAVITY))		vp[vn++] = "cause gravity waves";
	if (l_ptr->r_flags5 & (RF5_BA_EMP))			vp[vn++] = "cause emp blasts";
	if (l_ptr->r_flags5 & (RF5_BA_RAD))			vp[vn++] = "create radiation";
	if (l_ptr->r_flags5 & (RF5_XXX1))			vp[vn++] = "do something";
	if (l_ptr->r_flags5 & (RF5_BO_FIRE))			vp[vn++] = "produce fire bolts";
	if (l_ptr->r_flags5 & (RF5_BO_EARTH))		vp[vn++] = "produce earth bolts";
	if (l_ptr->r_flags5 & (RF5_BO_AIR))			vp[vn++] = "produce air blasts";
	if (l_ptr->r_flags5 & (RF5_BO_WATER))		vp[vn++] = "produce steam blasts";
	if (l_ptr->r_flags5 & (RF5_BO_ELEC))			vp[vn++] = "produce lightning bolts";
	if (l_ptr->r_flags5 & (RF5_BO_ICE))			vp[vn++] = "produce ice bolts";
	if (l_ptr->r_flags5 & (RF5_BO_ACID))			vp[vn++] = "produce acid bolts";
	if (l_ptr->r_flags5 & (RF5_BO_POISON))		vp[vn++] = "produce poison bolts";
	if (l_ptr->r_flags5 & (RF5_BO_TIME))			vp[vn++] = "produce time distortion";
	if (l_ptr->r_flags5 & (RF5_BO_ETHER))		vp[vn++] = "produce etheric distortion";
	if (l_ptr->r_flags5 & (RF5_BO_SOUND))		vp[vn++] = "produce sonic waves";
	if (l_ptr->r_flags5 & (RF5_BO_NETHER))		vp[vn++] = "produce nether bolts";
	if (l_ptr->r_flags5 & (RF5_BO_GRAVITY))		vp[vn++] = "produce gravity waves";
	if (l_ptr->r_flags5 & (RF5_BO_DARK))			vp[vn++] = "produce dark bolts";
	if (l_ptr->r_flags5 & (RF5_XXX3))			vp[vn++] = "do something";
	if (l_ptr->r_flags5 & (RF5_XXX4))			vp[vn++] = "do something";
	if (l_ptr->r_flags6 & (RF6_HASTE))			vp[vn++] = "haste-self";
	if (l_ptr->r_flags6 & (RF6_CURE))			vp[vn++] = "cure-self";
	if (l_ptr->r_flags6 & (RF6_HEAL))			vp[vn++] = "heal-self";
	if (l_ptr->r_flags6 & (RF6_ADD_MANA))		vp[vn++] = "regenerate mana";
	if (l_ptr->r_flags6 & (RF6_BLINK))			vp[vn++] = "blink-self";
	if (l_ptr->r_flags6 & (RF6_TPORT))			vp[vn++] = "teleport-self";
	if (l_ptr->r_flags6 & (RF6_XXX1))			vp[vn++] = "do something";
	if (l_ptr->r_flags6 & (RF6_XXX2))			vp[vn++] = "do something";
	if (l_ptr->r_flags6 & (RF6_TELE_TO))			vp[vn++] = "teleport to";
	if (l_ptr->r_flags6 & (RF6_TELE_AWAY))		vp[vn++] = "teleport away";
	if (l_ptr->r_flags6 & (RF6_TELE_LEVEL))		vp[vn++] = "teleport level";
	if (l_ptr->r_flags6 & (RF6_TELE_SELF_TO))	vp[vn++] = "teleport self to";
	if (l_ptr->r_flags6 & (RF6_DARKNESS))		vp[vn++] = "create darkness";
	if (l_ptr->r_flags6 & (RF6_TRAPS))			vp[vn++] = "create traps";
	if (l_ptr->r_flags6 & (RF6_FORGET))			vp[vn++] = "cause amnesia";
	if (l_ptr->r_flags6 & (RF6_FEAR))			vp[vn++] = "cause fear";
	if (l_ptr->r_flags6 & (RF6_PSI))				vp[vn++] = "mind blast";
	if (l_ptr->r_flags6 & (RF6_DOMINATION))		vp[vn++] = "mind smash";
	if (l_ptr->r_flags6 & (RF6_STUN))			vp[vn++] = "stun";
	if (l_ptr->r_flags6 & (RF6_TK))				vp[vn++] = "smash";
	if (l_ptr->r_flags6 & (RF6_FORCE))			vp[vn++] = "blast";
	if (l_ptr->r_flags6 & (RF6_CONFUSION))		vp[vn++] = "cause confusion";
	if (l_ptr->r_flags6 & (RF6_SPIRIT))			vp[vn++] = "create attacking spirits";
	if (l_ptr->r_flags6 & (RF6_ECTOPLASM))		vp[vn++] = "invoke spirit essence";
	if (l_ptr->r_flags6 & (RF6_BLIND))			vp[vn++] = "blind";
	if (l_ptr->r_flags6 & (RF6_SLOW))			vp[vn++] = "slow";
	if (l_ptr->r_flags6 & (RF6_HOLD))			vp[vn++] = "paralyze";
	if (l_ptr->r_flags6 & (RF6_DRAIN_MANA))		vp[vn++] = "drain mana";
	if (l_ptr->r_flags6 & (RF6_CHARGE))			vp[vn++] = "charge";
	if (l_ptr->r_flags6 & (RF6_XXX4))			vp[vn++] = "do something";
	if (l_ptr->r_flags6 & (RF6_XXX5))			vp[vn++] = "do something";
	if (l_ptr->r_flags6 & (RF6_MIRROR_IMAGE))	vp[vn++] = "clone self";
	if (l_ptr->r_flags7 & (RF7_BE_FIRE))			vp[vn++] = "fire a heat beam";
	if (l_ptr->r_flags7 & (RF7_BE_ELEC))			vp[vn++] = "fire an electric beam";
	if (l_ptr->r_flags7 & (RF7_BE_WATER))			vp[vn++] = "fire a geyser of water";
	if (l_ptr->r_flags7 & (RF7_XXX7X4))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X5))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X6))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X7))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X8))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X9))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X10))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X11))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X12))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X13))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_XXX7X14))			vp[vn++] = "do something";
	if (l_ptr->r_flags7 & (RF7_S_CUTTENCLIP))			vp[vn++] = "summon cuttenclip troops";
	if (l_ptr->r_flags7 & (RF7_S_BEASTMEN))		vp[vn++] = "summon beastmen";
	if (l_ptr->r_flags7 & (RF7_S_PLANTS))		vp[vn++] = "grow plants";
	if (l_ptr->r_flags7 & (RF7_S_KIN))			vp[vn++] = "summon similar monsters";
	if (l_ptr->r_flags7 & (RF7_S_HI_DEMON))		vp[vn++] = "summon greater demons";
	if (l_ptr->r_flags7 & (RF7_S_MONSTER))		vp[vn++] = "summon a monster";
	if (l_ptr->r_flags7 & (RF7_S_MONSTERS))		vp[vn++] = "summon monsters";
	if (l_ptr->r_flags7 & (RF7_S_AUTOMATA))		vp[vn++] = "summon automata";
	if (l_ptr->r_flags7 & (RF7_S_SPIDER))		vp[vn++] = "summon spiders";
	if (l_ptr->r_flags7 & (RF7_S_HOUND))			vp[vn++] = "summon hounds";
	if (l_ptr->r_flags7 & (RF7_S_MONKEY))		vp[vn++] = "summon monkeys";
	if (l_ptr->r_flags7 & (RF7_S_ALIEN))			vp[vn++] = "summon an alien";
	if (l_ptr->r_flags7 & (RF7_S_DEMON))			vp[vn++] = "summon a demon";
	if (l_ptr->r_flags7 & (RF7_S_UNDEAD))		vp[vn++] = "summon an undead";
	if (l_ptr->r_flags7 & (RF7_S_ELEMENTAL))		vp[vn++] = "summon a elemental";
	if (l_ptr->r_flags7 & (RF7_S_HI_UNDEAD))		vp[vn++] = "summon Greater Undead";
	if (l_ptr->r_flags7 & (RF7_S_HI_ELEMENTAL))	vp[vn++] = "summon Ancient Elementals";
	if (l_ptr->r_flags7 & (RF7_S_UNIQUE))		vp[vn++] = "summon Unique Monsters";
	if (table) {
		roff("\n");
		roff("Spells & Energies: ");
	}
	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (!table) {
			if (breath)
			{
				roff(", and is also");
			}
			else
			{
				roff(format("%^s is", wd_he[msex]));
			}

			/* Verb Phrase */
			if ((r_ptr->flags3 & (RF3_AUTOMATA)))
			{
				roff(" skillfully engineered, discharging energies");
			}
			else
			{
				roff(" magical, casting spells");
			}
		}
		
		/* Adverb */
		if (l_ptr->r_flags2 & (RF2_SMART)) roff(" intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if ((m) && (n == m))
			{
				if (m > 1) roff(",");
				/* roff(" or summon "); */
				roff(" or ");
			}
			else if (n == 0) {
				if (!table) {
					roff(" which ");
				} else {
					roff(" ");
				}
			}
			else if (n < vn-1) roff(", ");
			else if (n == 1) roff(" or ");
			else
			{
				if ((m) && (m == vn - 2)) roff(" or ");
				else roff(", or ");
			}

			/* Dump */
			roff(vp[n]);
		}
	}

	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = l_ptr->r_ranged;

		/* Average frequency */
		n = (r_ptr->freq_ranged);

		/* Describe the spell frequency */
		if (m > 50)
		{
			roff(format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			roff(format("; about 1 time in %d", 100 / n));
		}

		/* Describe monster mana */
		if (r_ptr->mana && know_mana(r_idx))
		{
			/* Mana */
			roff(format(" with a mana rating of %d",
				    r_ptr->mana));

		}

		/* End this sentence */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}
}

static void describe_monster_drop(int r_idx, const monster_lore *l_ptr,
				  bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int n;

	cptr p;

	int msex = 0;

	bool sin = FALSE;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	if (table)
	{
		roff("Drops:\t");
	}
	/* Drops gold and/or items */
	if (l_ptr->r_drop_gold || l_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		if (!table) {
			/* Intro */
			roff(format("%^s may carry", wd_he[msex]));
		}

		/* Count maximum drop */
		n = MAX(l_ptr->r_drop_gold, l_ptr->r_drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			roff(" a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			roff(" one or two");
		}

		/* Many drops */
		else
		{
			roff(format(" up to %d", n));
		}


		/* Great */
		if (l_ptr->r_flags1 & (RF1_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (l_ptr->r_flags1 & (RF1_DROP_GOOD))
		{
			p = " good";
			sin = FALSE;
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (l_ptr->r_drop_item)
		{
			/* Handle singular "an" */
			if (sin) roff("n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) roff(p);
			roff(" object");
			if (n != 1) roff("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->r_drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) roff("n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) roff(p);
			roff(" treasure");
			if (n != 1) roff("s");
		}

		/* End this sentence */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}


}

static void describe_monster_attack(int r_idx, const monster_lore *l_ptr,
				    bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int m, n, r;

	cptr p, q;
	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->r_blows[m]) n++;
	}

	if (table)
	{
		roff("Attacks:\t");
	}
	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!l_ptr->r_blows[m]) continue;


		/* Extract the attack info */
		method = r_ptr->blow[m].method;
		effect = r_ptr->blow[m].effect;
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;


		/* No method yet */
		p = NULL;

		/* Get the method */
		switch (method)
		{
			case RBM_HIT:		p = "hit"; break;
			case RBM_TOUCH:		p = "touch"; break;
			case RBM_PUNCH:		p = "punch"; break;
			case RBM_KICK:		p = "kick"; break;
			case RBM_SLASH:		p = "slash"; break;
			case RBM_PIERCE:	p = "pierce"; break;
			case RBM_BLUNT:		p = "bludgeon"; break;
			case RBM_LURE:		p = "lure"; break;
			case RBM_XXX2:		break;
			case RBM_HOWL:		p = "howl"; break;
			case RBM_CLAW:		p = "claw"; break;
			case RBM_KISS:		p = "kiss"; break;
			case RBM_GRAB:		p = "grab"; break;
			case RBM_BITE:		p = "bite"; break;
			case RBM_STING:		p = "sting"; break;
			case RBM_BUTT:		p = "butt"; break;
			case RBM_CRUSH:		p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			case RBM_CRAWL:		p = "crawl on you"; break;
			case RBM_DROOL:		p = "drool on you"; break;
			case RBM_SPIT:		p = "spit"; break;
			case RBM_XXX4:		break;
			case RBM_XXX5:		break;
			case RBM_GAZE:		p = "gaze"; break;
			case RBM_WAIL:		p = "wail"; break;
			case RBM_SPORE:		p = "release spores"; break;
			case RBM_XXX6:		break;
			case RBM_XXX7:		break;
			case RBM_ZAP:		p = "zap"; break;
			case RBM_PECK:		p = "peck"; break;
			case RBM_SPEAK:		p = "speak"; break;
			case RBM_BEG:		p = "beg"; break;
			case RBM_SEDUCE:	p = "seduce";	break;
			case RBM_XXX10:		break;
		}


		/* Default effect */
		q = NULL;

		/* Get the effect */
		switch (effect)
		{
			case RBE_HURT:		q = "attack"; break;
			case RBE_POISON:	q = "poison"; break;
			case RBE_UN_BONUS:	q = "disenchant"; break;
			case RBE_UN_POWER:	q = "drain charges"; break;
			case RBE_EAT_GOLD:	q = "steal gold"; break;
			case RBE_EAT_ITEM:	q = "steal items"; break;
			case RBE_EAT_FOOD:	q = "eat your food"; break;
			case RBE_EAT_LITE:	q = "absorb light"; break;
			case RBE_FIRE:		q = "burn"; break;
			case RBE_ACID:		q = "shoot acid"; break;
			case RBE_ELEC:		q = "electrify"; break;
			case RBE_COLD:		q = "freeze"; break;
			case RBE_STEAM:		q = "steam-blast"; break;
			case RBE_NETHER:	q = "disrupt life"; break;
			case RBE_ETHER:		q = "disrupt ether"; break;
			case RBE_BLIND:		q = "blind"; break;
			case RBE_CONFUSE:	q = "confuse"; break;
			case RBE_TERRIFY:	q = "terrify"; break;
			case RBE_PARALYZE:	q = "paralyze"; break;
			case RBE_LOSE_MUS:	q = "reduce muscle"; break;
			case RBE_LOSE_AGI:	q = "reduce agility"; break;
			case RBE_LOSE_VIG:	q = "reduce vigor"; break;
			case RBE_LOSE_SCH:	q = "reduce schooling"; break;
			case RBE_LOSE_EGO:	q = "reduce ego"; break;
			case RBE_LOSE_CHR:	q = "reduce charm"; break;
			case RBE_LOSE_ALL:	q = "reduce all stats"; break;
			case RBE_SHATTER:	q = "shatter"; break;
			case RBE_EXP_10:	q = "lower experience"; break;
			case RBE_EXP_20:	q = "lower experience"; break;
			case RBE_EXP_40:	q = "lower experience"; break;
			case RBE_EXP_80:	q = "lower experience"; break;
		}

		/* Introduce the attack description */
		if (!r)
		{
			if (!table) {
				roff(format("%^s can ", wd_he[msex]));
			}
		}
		else if (r < n-1)
		{
			roff(", ");
		}
		else
		{
			roff(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		roff(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			roff(" to ");
			roff(q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				roff(" with damage");
				roff(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		roff(".  ");
	}

	/* Notice lack of attacks */
	else if (l_ptr->r_flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}
	if (table)
	{
		roff("\n");
	}

}

static void describe_monster_abilities(int r_idx, const monster_lore *l_ptr,
				       bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int n;
	int msex = 0;

	int vn;
	cptr vp[64];

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	/* Collect special abilities. */
	vn = 0;
	if (l_ptr->r_flags2 & (RF2_OPEN_DOOR)) vp[vn++] = "open doors";
	if (l_ptr->r_flags2 & (RF2_BASH_DOOR)) vp[vn++] = "bash down doors";
	if (l_ptr->r_flags2 & (RF2_PASS_WALL)) vp[vn++] = "pass through walls";
	if (l_ptr->r_flags2 & (RF2_KILL_WALL)) vp[vn++] = "bore through walls";
	if (l_ptr->r_flags2 & (RF2_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
	if (l_ptr->r_flags2 & (RF2_TAKE_ITEM)) vp[vn++] = "pick up objects";
	if (l_ptr->r_flags2 & (RF2_KILL_ITEM)) vp[vn++] = "destroy objects";

	if (table)
	{
		roff("Special abilities:\t");
	}
	/* Describe special abilities. */
	if (vn)
	{
		if (!table) {
			/* Intro */
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" can ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}


	/* Describe special abilities. */
	if (table)
	{
		roff("Invisible:\t");
	}
	if (l_ptr->r_flags2 & (RF2_INVISIBLE))
	{
		if (!table) {
			roff(format("%^s is invisible.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}

	if (table)
	{
		roff("   Cold Blooded:\t");
	}
	if (l_ptr->r_flags2 & (RF2_COLD_BLOOD))
	{
		if (!table) {
			roff(format("%^s is cold blooded.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}
	if (table)
	{
		roff("\n");
	}
	if (table)
	{
		roff("Mind:\t");
	}
	if (l_ptr->r_flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->r_flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (table)
	{
		roff("\n");
	}

	if (table)
	{
		roff("Breeder:\t");
	}
	if (l_ptr->r_flags2 & (RF2_MULTIPLY))
	{
		if (!table) {
			roff(format("%^s breeds explosively.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}

	if (table)
	{
		roff("   Regenerates:\t");
	}
	if (l_ptr->r_flags2 & (RF2_REGENERATE))
	{
		if (!table) {
			roff(format("%^s regenerates quickly.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}

	if (table)
	{
		roff("   Impenetrable:\t");
	}
	if (l_ptr->r_flags2 & (RF2_IMPENT))
	{
		if (!table) {
			roff(format("%^s is hard to damage with projectiles.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}
	if (table)
	{
		roff("\n");
	}
	
	if (table)
	{
		roff("   Gaseous:\t");
	}
	if (l_ptr->r_flags2 & (RF2_GASEOUS))
	{
		if (!table) {
			roff(format("%^s is gaseous cloud.  ", wd_he[msex]));
		} else {
			roff("Yes");
		}
	} else if (table) {
		roff("   ");
	}
	if (table)
	{
		roff("\n");
	}
	
	if (table)
	{
		roff("Aura:\t");
	}
	if (l_ptr->r_flags8 & (RF8_CLOUD_SURROUND))
	{
		int typ = 0, dam = 0, rad = 0;

		/* Get type of cloud */
		cloud_surround(r_idx, &typ, &dam, &rad);

		/* We emit something */
		if (typ)
		{
			if (!table) {
				roff(format("%^s is surrounded by ", wd_he[msex]));
			}

			/* Describe cloud */
			if (typ == GF_FIRE)			roff("fire");
			else if (typ == GF_ICE)		roff("frost");
			else if (typ == GF_ELEC)	roff("lightning");
			else if (typ == GF_ACID)	roff("acidic smoke");
			else if (typ == GF_POISON)	roff("noxious gases");
			else if (typ == GF_SOUND)	roff("a cacophony of sound");
			else if (typ == GF_STEAM)       roff("a torrent of water");
			else if (typ == GF_EARTH)       roff("a hail of stones");
			else if (typ == GF_WIND)        roff("a gale of winds");
			else		       roff("powerful forces(missing description - FIXME!)");
			roff(".  ");
		}
	}
	if (table)
	{
		roff("\n");
	}


	/* Collect susceptibilities */
	vn = 0;
	if (l_ptr->r_flags3 & (RF3_HURT_ROCK)) vp[vn++] = "rock remover";
	if (l_ptr->r_flags3 & (RF3_HURT_LIGHT)) vp[vn++] = "bright light";
	if (l_ptr->r_flags8 & (RF8_VUN_FIRE)) vp[vn++] = "fire";
	if (l_ptr->r_flags8 & (RF8_VUN_EARTH)) vp[vn++] = "earth";
	if (l_ptr->r_flags8 & (RF8_VUN_AIR)) vp[vn++] = "air";
	if (l_ptr->r_flags8 & (RF8_VUN_WATER)) vp[vn++] = "water";
	if (l_ptr->r_flags8 & (RF8_VUN_ELEC)) vp[vn++] = "electricity";
	if (l_ptr->r_flags8 & (RF8_VUN_ICE)) vp[vn++] = "ice";
	if (l_ptr->r_flags8 & (RF8_VUN_ACID)) vp[vn++] = "acid";
	if (l_ptr->r_flags8 & (RF8_VUN_POIS)) vp[vn++] = "poison";
	if (l_ptr->r_flags8 & (RF8_VUN_TIME)) vp[vn++] = "time";
	if (l_ptr->r_flags8 & (RF8_VUN_ETHER)) vp[vn++] = "ether";
	if (l_ptr->r_flags8 & (RF8_VUN_SOUND)) vp[vn++] = "sound";
	if (l_ptr->r_flags8 & (RF8_VUN_NETHER)) vp[vn++] = "nether";
	if (l_ptr->r_flags8 & (RF8_VUN_PIERCE)) vp[vn++] = "piercing weapons";
	if (l_ptr->r_flags8 & (RF8_VUN_EDGED)) vp[vn++] = "edged weapons";
	if (l_ptr->r_flags8 & (RF8_VUN_BLUNT)) vp[vn++] = "blunt weapons";

	if (table)
	{
		roff("Vulnerable to:\t");
	}
	/* Describe susceptibilities */
	if (vn)
	{
		if (!table) {
			/* Intro */
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) {
				if (!table) {
					roff(" is hurt by ");
				}
			}
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}


	/* Collect immunities */
	vn = 0;
	if (l_ptr->r_flags3 & (RF3_IM_FIRE)) 	vp[vn++] = "fire";
	if (l_ptr->r_flags3 & (RF3_IM_EARTH))	vp[vn++] = "earth";
	if (l_ptr->r_flags3 & (RF3_IM_AIR)) 		vp[vn++] = "air";
	if (l_ptr->r_flags3 & (RF3_IM_WATER))	vp[vn++] = "water";
	if (l_ptr->r_flags3 & (RF3_IM_ELEC)) 	vp[vn++] = "electricty";
	if (l_ptr->r_flags3 & (RF3_IM_ICE)) 		vp[vn++] = "ice";
	if (l_ptr->r_flags3 & (RF3_IM_ACID)) 	vp[vn++] = "acid";
	if (l_ptr->r_flags3 & (RF3_IM_POIS)) 	vp[vn++] = "poison";
	if (l_ptr->r_flags3 & (RF3_IM_TIME)) 	vp[vn++] = "time";
	if (l_ptr->r_flags3 & (RF3_IM_ETHER))	vp[vn++] = "ether";
	if (l_ptr->r_flags3 & (RF3_IM_SOUND))	vp[vn++] = "sound";
	if (l_ptr->r_flags3 & (RF3_IM_NETHER))	vp[vn++] = "nether";
	if (l_ptr->r_flags3 & (RF3_IM_PIERCE)) 	vp[vn++] = "piercing weapons";
	if (l_ptr->r_flags3 & (RF3_IM_EDGED)) 	vp[vn++] = "edged weapons";
	if (l_ptr->r_flags3 & (RF3_IM_BLUNT)) 	vp[vn++] = "blunt weapons";


	if (table)
	{
		roff("Immune to:\t");
	}
	/* Describe immunities */
	if (vn)
	{
		if (!table) {
			/* Intro */
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) {
				if (!table) {
					roff(" resists ");
				}
			}
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}


	/* Collect non-effects */
	vn = 0;
	if (l_ptr->r_flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (l_ptr->r_flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
	if (l_ptr->r_flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (l_ptr->r_flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";

	if (table)
	{
		roff("Cannot be:\t");
	}
	/* Describe non-effects */
	if (vn)
	{
		if (!table) {
			/* Intro */
			roff(format("%^s", wd_he[msex]));
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) {
				if (!table) {
					roff(" cannot be ");
				}
			}
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}
	if (table)
	{
		roff("\n");
	}


	if (table)
	{
		roff("Awareness:\t");
	}
	/* Do we know how aware it is? */
	if ((((int)l_ptr->r_wake * (int)l_ptr->r_wake) > r_ptr->sleep) ||
	    (l_ptr->r_ignore == MAX_UCHAR) ||
	    ((r_ptr->sleep == 0) && (l_ptr->r_tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays very little attention to";
		}
		else if (r_ptr->sleep > 75)
		{
			act = "pays little attention to";
		}
		else if (r_ptr->sleep > 45)
		{
			act = "tends to overlook";
		}
		else if (r_ptr->sleep > 25)
		{
			act = "takes quite a while to see";
		}
		else if (r_ptr->sleep > 10)
		{
			act = "takes a while to see";
		}
		else if (r_ptr->sleep > 5)
		{
			act = "is fairly observant of";
		}
		else if (r_ptr->sleep > 3)
		{
			act = "is observant of";
		}
		else if (r_ptr->sleep > 1)
		{
			act = "is very observant of";
		}
		else if (r_ptr->sleep > 0)
		{
			act = "is vigilant for";
		}
		else
		{
			act = "is ever vigilant for";
		}

		roff(format("%^s %s intruders, which %s may notice from %d feet.  ",
		            wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
	}
	if (table)
	{
		roff("   Company:\t");
	}
	/* Describe escorts */
	if ((l_ptr->r_flags1 & (RF1_ESCORT)) || (l_ptr->r_flags1 & (RF1_ESCORTS)))
	{
		if (!table) {
			roff(format("%^s usually appears with escorts.  ",
				    wd_he[msex]));
		} else {
			roff("Escorts");
		}
	}

	/* Describe friends */
	else if ((l_ptr->r_flags1 & (RF1_FRIEND)) || (l_ptr->r_flags1 & (RF1_FRIENDS)))
	{
		if (!table) {
			roff(format("%^s usually appears in groups.  ",
				    wd_he[msex]));
		} else {
			roff("Friends");
		}
	}
	if (table)
	{
		roff("\n");
	}

}

static void describe_monster_kills(int r_idx, const monster_lore *l_ptr,
				   bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	if (table)
	{
		roff("Kills:\t");
	}
	/* Treat uniques differently */
	else if (l_ptr->r_flags1 & (RF1_UNIQUE))
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->r_deaths)
		{
			/* Killed ancestors */
			roff(format("%^s has slain %d of your ancestors",
			            wd_he[msex], l_ptr->r_deaths));

			/* But we've also killed it */
			if (dead)
			{
				roff(", but you have taken revenge!  ");
			}

			/* Unavenged (ever) */
			else
			{
				roff(format(", who %s unavenged.  ",
				            plural(l_ptr->r_deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			roff("You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (l_ptr->r_deaths)
	{
		/* Dead ancestors */
		roff(format("%d of your ancestors %s been killed by this creature, ",
		            l_ptr->r_deaths, plural(l_ptr->r_deaths, "has", "have")));

		/* Some kills this life */
		if (l_ptr->r_pkills)
		{
			roff(format("and you have exterminated at least %d of the creatures.  ",
			            l_ptr->r_pkills));
		}

		/* Some kills past lives */
		else if (l_ptr->r_tkills)
		{
			roff(format("and %s have exterminated at least %d of the creatures.  ",
			            "your ancestors", l_ptr->r_tkills));
		}

		/* No kills */
		else
		{
			roff(format("and %s is not ever known to have been defeated.  ",
			            wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->r_pkills)
		{
			roff(format("You have killed at least %d of these creatures.  ",
			            l_ptr->r_pkills));
		}

		/* Killed some last life */
		else if (l_ptr->r_tkills)
		{
			roff(format("Your ancestors have killed at least %d of these creatures.  ",
			            l_ptr->r_tkills));
		}

		/* Killed none */
		else
		{
			roff("No battles to the death are recalled.  ");
		}
	}

	if (table)
	{
		roff("\n");
	}
}

static void describe_monster_toughness(int r_idx, const monster_lore *l_ptr,
				       bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;


	/* Describe monster "toughness" */
	if (know_armour(r_idx))
	{
		/* Initalize variables */
		int percent_hit = 5;
		int chance = hit_chance();
		
		/* turn r_ptr->ac into an int */
		int monsterac = r_ptr->ac;
		
		/* Calculate Hit percentage */
		if (r_ptr->ac >= chance) percent_hit = 5;
		else percent_hit = (100 - ((monsterac * 100)/ chance));
		if (percent_hit > 95) percent_hit = 95;
		if (percent_hit < 5) percent_hit = 5;
		
		/* Armor */
		if (!table) {
			roff(format("%^s has an armor rating of %d",
				    wd_he[msex], r_ptr->ac));
		} else {
			roff(format("AC: %d    ", r_ptr->ac));
		}

		/* Maximized hitpoints */
		if (l_ptr->r_flags1 & (RF1_FIXED_HPS))
		{
			if (!table) {
				roff(format(" and a life rating of %d.  ",
					    r_ptr->hitpoints));
			} else {
				roff(format("HP: %d    ", r_ptr->hitpoints));
			}
		}

		/* Variable hitpoints */
		else
		{
			if (!table) {
				roff(format(" and an approximate life rating of %d.  ",
					    r_ptr->hitpoints));
			} else {
				roff(format("HP: ~%d   ", r_ptr->hitpoints));
			}
		}
		
		/* Hit % */
		if (!table) {
			roff(format("You have a %d%% chance of hitting this monster.  ", 
				    percent_hit));
		} else {
			roff(format("Hit chance: %d%%", percent_hit));
		}
	}
	if (table)
	{
		roff("\n");
	}

}

static void describe_monster_exp(int r_idx, const monster_lore *l_ptr,
				 bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	cptr p, q;
	long i, j;

	if (table)
	{
		roff("Type:\t");
	}
	/* Describe experience if known */
	if (l_ptr->r_tkills)
	{
		if (!table) {
			/* Introduction */
			if (l_ptr->r_flags1 & (RF1_UNIQUE))
			{
				roff("Killing this");
			}
			else
			{
				roff("A kill of this");
			}
		}

		/* Describe the "quality" */
		if (l_ptr->r_flags3 & (RF3_ANIMAL)) roff(" natural");
		if (l_ptr->r_flags3 & (RF3_EVIL)) roff(" evil");
		if (l_ptr->r_flags3 & (RF3_UNDEAD)) roff(" undead");

		/* Describe the "race" */
		if (l_ptr->r_flags3 & (RF3_ELEMENTAL)) roff(" elemental");
		else if (l_ptr->r_flags3 & (RF3_DEMON)) roff(" demon");
		else if (l_ptr->r_flags3 & (RF3_CONSTRUCT)) roff(" construct");
		else if (l_ptr->r_flags3 & (RF3_DINOSAUR)) roff(" dinosaur");
		else if (l_ptr->r_flags3 & (RF3_AUTOMATA)) roff(" automata");
		else if (l_ptr->r_flags3 & (RF3_ALIEN)) roff(" alien");
		else if (l_ptr->r_flags3 & (RF3_BEASTMAN)) roff(" beastman");
		else if (l_ptr->r_flags3 & (RF3_PLANT)) roff(" plant");
		else roff(" creature");

		/* calculate the integer exp part */
		i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

		/* calculate the fractional exp part scaled by 100, */
		/* must use long arithmetic to avoid overflow */
		j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
			  (long)1000 / p_ptr->lev + 5) / 10);

		/* Mention the experience */
		if (table) {
			roff(format("    Exp: %ld.%02ld", (long)i, (long)j));
		} else {
			roff(format(" is worth %ld.%02ld point%s",
				    (long)i, (long)j,
				    (((i == 1) && (j == 0)) ? "" : "s")));
		}

		/* Take account of annoying English */
		p = "th";
		i = p_ptr->lev % 10;
		if ((p_ptr->lev / 10) == 1) /* nothing */;
		else if (i == 1) p = "st";
		else if (i == 2) p = "nd";
		else if (i == 3) p = "rd";

		/* Take account of "leading vowels" in numbers */
		q = "";
		i = p_ptr->lev;
		if ((i == 8) || (i == 11) || (i == 18)) q = "n";

		/* Mention the dependance on the player's level */
		if (!table) {
			roff(format(" for a%s %lu%s level character.  ",
				    q, (long)i, p));
		}
	}
	if (table)
	{
		roff("\n");
	}
	

}

static void describe_monster_movement(int r_idx, const monster_lore *l_ptr,
				      bool table)
{
	const monster_race *r_ptr = &r_info[r_idx];

	bool old = FALSE;

	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	/* Nothing yet */
	old = FALSE;

	if (table)
	{
		roff("Level:\t");
	}
	/* Describe location */
	if (r_ptr->level == 0)
	{
		if (!table) {
			roff(format("%^s lives in the town", wd_he[msex]));
		} else {
			roff("town ");
		}
		old = TRUE;
	}
	else if (l_ptr->r_tkills)
	{
		if (!table) {
			if (depth_in_feet)
			{
				roff(format("%^s is normally found at depths of %d feet",
					    wd_he[msex], 2550-(r_ptr->level * 50)));
			}
			else
			{
				roff(format("%^s is normally found on dungeon level %d",
					    wd_he[msex], 51-r_ptr->level));
			}
		} else {
			roff(format("%d ", 51-r_ptr->level));

		}
		old = TRUE;
	}


	if (table)
	{
		roff(" Speed:\t");
	}
	/* Introduction */
	if (!table) {
		if (old)
		{
			roff(", and ");
		}
		else
		{
			roff(format("%^s ", wd_he[msex]));
			old = TRUE;
		}
	}
	roff("moves");

	/* Random-ness */
	if ((l_ptr->r_flags1 & (RF1_RAND_50)) || (l_ptr->r_flags1 & (RF1_RAND_25)))
	{
		/* Adverb */
		if ((l_ptr->r_flags1 & (RF1_RAND_50)) && (l_ptr->r_flags1 & (RF1_RAND_25)))
		{
			roff(" extremely");
		}
		else if (l_ptr->r_flags1 & (RF1_RAND_50))
		{
			roff(" somewhat");
		}
		else if (l_ptr->r_flags1 & (RF1_RAND_25))
		{
			roff(" a bit");
		}

		/* Adjective */
		roff(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) roff(", and");
	}

	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) roff(" incredibly");
		else if (r_ptr->speed > 120) roff(" very");
		else if (r_ptr->speed < 116) roff(" fairly");
		roff(" quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) roff(" extremely");
		else if (r_ptr->speed < 100) roff(" very");
		roff(" slowly");
	}
	else
	{
		roff(" at normal speed");
	}

	/* The code above includes "attack speed" */
	if (l_ptr->r_flags1 & (RF1_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			roff(", but ");
		}
		else
		{
			roff(format("%^s ", wd_he[msex]));
			old = TRUE;
		}

		/* Describe */
		roff("does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		roff(".  ");
		old = FALSE;
	}

	if (table)
	{
		roff("\n");
	}}

/*
 * Hack -- display monster information using "roff()"
 *
 * Note that there is now a compiler option to only read the monster
 * descriptions from the raw file when they are actually needed, which
 * saves about 60K of memory at the cost of disk access during monster
 * recall, which is optional to the user.
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen, on a cleared line, in which the recall is
 * to take place.  One extra blank line is left after the recall.
 *
 * Split up like in NPPAngband and added support for table view. -CJN-
 */
static void roff_aux(int r_idx, bool table)
{
	const monster_race *r_ptr;
	monster_lore *l_ptr;

/* 	u32b flags1; */
/* 	u32b flags2; */
/* 	u32b flags3; */
/* 	u32b flags4; */
/* 	u32b flags5; */
/* 	u32b flags6; */
/* 	u32b flags7; */
/* 	u32b flags8; */


	monster_lore save_mem;
	monster_lore lore; /* To avoid using separate flagsN */


	/* Get the race and lore */
	r_ptr = &r_info[r_idx];
	l_ptr = &l_list[r_idx];


	/* Cheat -- know everything */
	if (cheat_know)
	{
		/* XXX XXX XXX */

		/* Hack -- save memory */
		COPY(&save_mem, l_ptr, monster_lore);

		cheat_monster_lore(r_idx, l_ptr);
	}
	/* Hack -- create a copy of the monster-memory */
	COPY(&lore, l_ptr, monster_lore);


	/* (Obtain a copy of the "known" flags) */
	/* Paranoia, remove falsely memorized flags */
	lore.r_flags1 &= r_ptr->flags1;
	lore.r_flags2 &= r_ptr->flags2;
	lore.r_flags3 &= r_ptr->flags3;
	lore.r_flags4 &= r_ptr->flags4;
	lore.r_flags5 &= r_ptr->flags5;
	lore.r_flags6 &= r_ptr->flags6;
	lore.r_flags7 &= r_ptr->flags7;
	lore.r_flags8 &= r_ptr->flags8;
/* 	flags1 = (r_ptr->flags1 & l_ptr->r_flags1); */
/* 	flags2 = (r_ptr->flags2 & l_ptr->r_flags2); */
/* 	flags3 = (r_ptr->flags3 & l_ptr->r_flags3); */
/* 	flags4 = (r_ptr->flags4 & l_ptr->r_flags4); */
/* 	flags5 = (r_ptr->flags5 & l_ptr->r_flags5); */
/* 	flags6 = (r_ptr->flags6 & l_ptr->r_flags6); */
/* 	flags7 = (r_ptr->flags7 & l_ptr->r_flags7); */
/* 	flags8 = (r_ptr->flags8 & l_ptr->r_flags8); */


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE)) lore.r_flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & (RF1_QUESTOR)) lore.r_flags1 |= (RF1_QUESTOR);
	if (r_ptr->flags1 & (RF1_MALE)) lore.r_flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE)) lore.r_flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_FRIEND)) lore.r_flags1 |= (RF1_FRIEND);
	if (r_ptr->flags1 & (RF1_FRIENDS)) lore.r_flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT)) lore.r_flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS)) lore.r_flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (lore.r_tkills)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & (RF3_AUTOMATA)) 	
			lore.r_flags3 |= (RF3_AUTOMATA);
		if (r_ptr->flags3 & (RF3_DINOSAUR)) 	
			lore.r_flags3 |= (RF3_DINOSAUR);
		if (r_ptr->flags3 & (RF3_CONSTRUCT)) 	
			lore.r_flags3 |= (RF3_CONSTRUCT);
		if (r_ptr->flags3 & (RF3_ELEMENTAL)) 	
			lore.r_flags3 |= (RF3_ELEMENTAL);
		if (r_ptr->flags3 & (RF3_DEMON)) 		
			lore.r_flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & (RF3_UNDEAD)) 		
			lore.r_flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & (RF3_EVIL)) 		
			lore.r_flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & (RF3_ANIMAL)) 		
			lore.r_flags3 |= (RF3_ANIMAL);
		if (r_ptr->flags3 & (RF3_ALIEN)) 		
			lore.r_flags3 |= (RF3_ALIEN);
		if (r_ptr->flags3 & (RF3_BEASTMAN)) 	
			lore.r_flags3 |= (RF3_BEASTMAN);
		if (r_ptr->flags3 & (RF3_PLANT)) 		
			lore.r_flags3 |= (RF3_PLANT);
		

		/* Know "forced" flags */
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) 
			lore.r_flags1 |= (RF1_FORCE_DEPTH);
		if (r_ptr->flags1 & (RF1_FIXED_HPS)) 
			lore.r_flags1 |= (RF1_FIXED_HPS);
	}


	/* Require a flag to show kills */
	if (!(show_details))
	{
		/* nothing */
	}

	describe_monster_kills(r_idx, &lore, table);

	/* Descriptions */
	/* Ignored in tabled version */
	if (show_details && !(table))
	{
		describe_monster_desc(r_idx);
	}


	describe_monster_movement(r_idx, &lore, table);

	describe_monster_exp(r_idx, &lore, table);

	describe_monster_spells(r_idx, &lore, table);

	describe_monster_toughness(r_idx, &lore, table);

	describe_monster_abilities(r_idx, &lore, table);

	describe_monster_drop(r_idx, &lore, table);

	describe_monster_attack(r_idx, &lore, table);


	/* Remove totally? */
	if (!table)
	{
		/* Notice "Quest" monsters */
		if (lore.r_flags1 & (RF1_QUESTOR))
		{
			roff("You feel an intense desire to kill this monster...  ");
		}
	}

	/* All done */
	roff("\n");


	/* Cheat -- know everything */
	if (cheat_know)
	{
		/* Hack -- restore memory */
		COPY(l_ptr, &save_mem, monster_lore);
	}
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	Term_addstr(-1, TERM_WHITE, "'):");
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, bool table)
{
	int y;

	/* Flush messages */
	message_flush();

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Begin recall */
	/* Term_erase(0, 1, 255); */

	/* Recall monster */
	roff_aux(r_idx, table);

	/* Describe monster */
	roff_top(r_idx);
}




/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx)
{
	int y;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Recall monster */
	roff_aux(r_idx, p_ptr->monster_mem_fmt);

	/* Describe monster */
	roff_top(r_idx);
}

/*
 * Hack -- show a list of the visible monsters in the current "term" window
 */
void display_visible(void)
{
	int i, y;
	char c1, c2;
	byte a1, a2;
	monster_race *r_ptr;


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Are we hallucinating? */
	if (p_ptr->image)
	{
		/* Go to top left of screen */
		Term_gotoxy(0, 10);
	
		Term_addstr(-1, TERM_VIOLET, "Hallucinations");
	
		return;
	}
	
	i = p_ptr->max_seen_r_idx;

	/* Show the list */
	for (y = 0; y < Term->hgt; y++)
	{
		/* No more to display */
		if (!i) return;
		
		/* Go to left of screen */
		Term_gotoxy(0, y);
		
		/* Note we have assumed that r_info.txt has been sorted */
		
		/* Access monster */
		r_ptr = &r_info[i];
		
		/* Access the chars */
		c1 = r_ptr->d_char;
		c2 = r_ptr->x_char;

		/* Access the attrs */
		a1 = r_ptr->d_attr;
		a2 = r_ptr->x_attr;

		/* Dump the name */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			Term_addstr(-1, TERM_L_BLUE, (r_name + r_ptr->name));
		}
		else
		{
			Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));
		}		

		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(a1, c1);
		Term_addstr(-1, TERM_WHITE, "')");

		/* Append the "optional" attr/char info */
		Term_addstr(-1, TERM_WHITE, "/('");
		Term_addch(a2, c2);
		Term_addstr(-1, TERM_WHITE, "'):");

		/* Wizards get extra info */
		if (p_ptr->wizard)
		{
			char buf[6];

			sprintf(buf, "%d", i);
	
			Term_addstr(-1, TERM_WHITE, " (");
			Term_addstr(-1, TERM_L_BLUE, buf);
			Term_addch(TERM_WHITE, ')');
		}

		/* Append count */
		Term_addstr(-1, TERM_WHITE, format("[%d]",r_ptr->r_see));

		/* Look for the next one */
		while (i > 0)
		{
			i--;

			if (r_info[i].r_see)
			{
				break;
			}
		}
	}
}


/*
 * Given a starting position, find the 'n'th closest monster.
 *
 * Note:  "require_visible" only works when this function is looking around
 * the character.
 *
 * Set ty and tx to zero on failure.
 */
void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible)
{
	monster_type *m_ptr;

	int i, j;
	int r_idx;
	int dist = 100;

	int *monster_dist;
	int *monster_index;
	int monster_count = 0;

	bool use_view = FALSE;

	/* Allocate some arrays */
	C_MAKE(monster_dist, m_max, int);
	C_MAKE(monster_index, m_max, int);

	/* Note that we're looking from the character's grid */
	if ((y0 == p_ptr->py) && (x0 == p_ptr->px)) use_view = TRUE;

	/* Reset target grids */
	*ty = 0;  *tx = 0;

	/* N, as input, goes from 1+.  Map it to 0+ for table access */
	if (n > 0) n--;


	/* Check all the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Check for visibility */
		if (require_visible)
		{
			if (!m_ptr->ml || (m_ptr->mflag & (MFLAG_MIME))) continue;
		}

		/* Use CAVE_VIEW information (fast way) */
		if (use_view)
		{
			if (!(cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_VIEW))) continue;

			/* Get stored distance */
			dist = m_ptr->cdis;
		}

		/* Monster must be in los from the starting position (slower way) */
		else
		{
			/* Get distance from starting position */
			dist = distance(y0, x0, m_ptr->fy, m_ptr->fx);

			/* Monster location must be within range */
			if (dist > MAX_SIGHT) continue;

			/* Require line of sight */
			if (!los(y0, x0, m_ptr->fy, m_ptr->fx)) continue;
		}

		/* Remember this monster */
		monster_dist[monster_count] = dist;
		monster_index[monster_count++] = i;
	}

	/* Not enough monsters found */
	if (monster_count <= n)
	{
		/* Free some arrays */
		FREE(monster_dist, int);
		FREE(monster_index, int);

		return;
	}


	/* Sort the monsters in ascending order of distance */
	for (i = 0; i < monster_count - 1; i++)
	{
		for (j = 0; j < monster_count - 1; j++)
		{
			int this_dist = monster_dist[j];
			int next_dist = monster_dist[j + 1];

			/* Bubble sort */
			if (this_dist > next_dist)
			{
				int tmp_dist  = monster_dist[j];
				int tmp_index = monster_index[j];

				monster_dist[j] = monster_dist[j + 1];
				monster_dist[j + 1] = tmp_dist;

				monster_index[j] = monster_index[j + 1];
				monster_index[j + 1] = tmp_index;
			}
		}
	}


	/* Get the nth closest monster's index */
	r_idx = monster_index[n];

	/* Get the monster */
	m_ptr = &m_list[r_idx];

	/* Set the target to its location */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;

	/* Free some arrays */
	FREE(monster_dist, int);
	FREE(monster_index, int);
}

