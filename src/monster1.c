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
 * Space for storing temporary monster 
 */
monster_race monster_temp_roff;

/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(monster_lore *l_ptr, int r_idx, int u_idx)
{
	monster_race *r_ptr = get_monster_fake(r_idx, u_idx);
	s32b level = r_ptr->level;

	s32b kills = l_ptr->r_tkills;

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!u_idx) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5*level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}

/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(monster_lore *l_ptr, int r_idx, int u_idx, int i)
{
	monster_race *r_ptr = get_monster_fake(r_idx, u_idx);

	s32b level = r_ptr->level;

	s32b a = l_ptr->r_blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/* Skip non-uniques */
	if (!u_idx) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}

/*
 * Collect the special abilities - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_special(u32b flags2, cptr vp[64])
{
	int n;

	/* Collect innate */
	n = 0;
    if (flags2 & (RF2_HAS_LITE))  vp[n++] = "illuminate the dungeon";
	if (flags2 & (RF2_OPEN_DOOR)) vp[n++] = "open doors";
	if (flags2 & (RF2_BASH_DOOR)) vp[n++] = "bash down doors";
	if (flags2 & (RF2_PASS_WALL)) vp[n++] = "pass through walls";
	if (flags2 & (RF2_KILL_WALL)) vp[n++] = "bore through walls";
	if (flags2 & (RF2_MOVE_BODY)) vp[n++] = "push past weaker monsters";
	if (flags2 & (RF2_KILL_BODY)) vp[n++] = "destroy weaker monsters";
	if (flags2 & (RF2_TAKE_ITEM)) vp[n++] = "pick up objects";
	if (flags2 & (RF2_KILL_ITEM)) vp[n++] = "destroy objects";

	return n;
}

/*
 * Collect the innate names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_innate(u32b flags4, cptr vp[64])
{
	int n;

	/* Collect innate */
	n = 0;
	if (flags4 & (RF4_SHRIEK))		vp[n++] = "shriek for help";
	if (flags4 & (RF4_ARROW_1))		vp[n++] = "fire an arrow";
	if (flags4 & (RF4_ARROW_2))		vp[n++] = "fire arrows";
	if (flags4 & (RF4_ARROW_3))		vp[n++] = "fire a missile";
	if (flags4 & (RF4_ARROW_4))		vp[n++] = "fire missiles";

	return n;
}

/*
 * Collect the breath names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_breaths(u32b flags4, cptr vp[64])
{
	int n;

	/* Collect breaths */
	n = 0;
	if (flags4 & (RF4_BR_ACID))		vp[n++] = "acid";
	if (flags4 & (RF4_BR_ELEC))		vp[n++] = "lightning";
	if (flags4 & (RF4_BR_FIRE))		vp[n++] = "fire";
	if (flags4 & (RF4_BR_COLD))		vp[n++] = "frost";
	if (flags4 & (RF4_BR_POIS))		vp[n++] = "poison";
	if (flags4 & (RF4_BR_NETH))		vp[n++] = "nether";
	if (flags4 & (RF4_BR_LITE))		vp[n++] = "light";
	if (flags4 & (RF4_BR_DARK))		vp[n++] = "darkness";
	if (flags4 & (RF4_BR_CONF))		vp[n++] = "confusion";
	if (flags4 & (RF4_BR_SOUN))		vp[n++] = "sound";
	if (flags4 & (RF4_BR_CHAO))		vp[n++] = "chaos";
	if (flags4 & (RF4_BR_DISE))		vp[n++] = "disenchantment";
	if (flags4 & (RF4_BR_NEXU))		vp[n++] = "nexus";
	if (flags4 & (RF4_BR_TIME))		vp[n++] = "time";
	if (flags4 & (RF4_BR_INER))		vp[n++] = "inertia";
	if (flags4 & (RF4_BR_GRAV))		vp[n++] = "gravity";
	if (flags4 & (RF4_BR_SHAR))		vp[n++] = "shards";
	if (flags4 & (RF4_BR_PLAS))		vp[n++] = "plasma";
	if (flags4 & (RF4_BR_FORCE))	vp[n++] = "force";
	if (flags4 & (RF4_BR_MANA))		vp[n++] = "mana";
	if (flags4 & (RF4_BR_WATER))	vp[n++] = "water";
	if (flags4 & (RF4_BR_DISEASE))	vp[n++] = "disease";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_resists(u32b flags3, u32b flags4, cptr vp[64])
{
	int n;

	/* 
	 * Note - the following flags also appear under breaths which might be annoying 
	 * to some players and takes screen real estate when displaying the monster memory
	 * but is more intuitive for beginners. 
	 */
	n = 0;
	if ((flags4 & (RF4_BR_ACID))	|| (flags3 & (RF3_RES_ACID)))		vp[n++] = "acid";
	if ((flags4 & (RF4_BR_ELEC))	|| (flags3 & (RF3_RES_ELEC)))		vp[n++] = "lightning";
	if ((flags4 & (RF4_BR_FIRE))	|| (flags3 & (RF3_RES_FIRE)))		vp[n++] = "fire";
	if ((flags4 & (RF4_BR_COLD))	|| (flags3 & (RF3_RES_COLD)))		vp[n++] = "cold";
	if ((flags4 & (RF4_BR_POIS))	|| (flags3 & (RF3_RES_POIS)))		vp[n++] = "poison";
	if ((flags4 & (RF4_BR_LITE))	|| (flags3 & (RF3_RES_LITE)))		vp[n++] = "lite";
	if ((flags4 & (RF4_BR_DARK))	|| (flags3 & (RF3_RES_DARK)))		vp[n++] = "darkness";
	if ((flags4 & (RF4_BR_NETH))	|| (flags3 & (RF3_RES_NETH)))		vp[n++] = "nether";
	if ((flags4 & (RF4_BR_CONF))	|| (flags3 & (RF3_RES_CONF)))		vp[n++] = "confusion";
	if ((flags4 & (RF4_BR_SOUN))	|| (flags3 & (RF3_RES_SOUN)))		vp[n++] = "sound";
	if ((flags4 & (RF4_BR_CHAO))	|| (flags3 & (RF3_RES_CHAO)))		vp[n++] = "chaos";
	if ((flags4 & (RF4_BR_DISE))	|| (flags3 & (RF3_RES_DISE)))		vp[n++] = "disenchantment";
	if ((flags4 & (RF4_BR_NEXU))	|| (flags3 & (RF3_RES_NEXU)))		vp[n++] = "nexus";
	if ((flags4 & (RF4_BR_TIME))	|| (flags3 & (RF3_RES_TIME)))		vp[n++] = "time";
	if ((flags4 & (RF4_BR_INER))	|| (flags3 & (RF3_RES_INER)))		vp[n++] = "inertia";
	if ((flags4 & (RF4_BR_GRAV))	|| (flags3 & (RF3_RES_GRAV)))		vp[n++] = "gravity";
	if ((flags4 & (RF4_BR_SHAR))	|| (flags3 & (RF3_RES_SHAR)))		vp[n++] = "shards";
	if ((flags4 & (RF4_BR_PLAS))	|| (flags3 & (RF3_RES_PLAS)))		vp[n++] = "plasma";
	if ((flags4 & (RF4_BR_FORCE))	|| (flags3 & (RF3_RES_FORCE)))		vp[n++] = "force";
	if ((flags4 & (RF4_BR_MANA))	|| (flags3 & (RF3_RES_MANA)))		vp[n++] = "mana";
	if ((flags4 & (RF4_BR_WATER))	|| (flags3 & (RF3_RES_WATER)))		vp[n++] = "water";
	if ((flags4 & (RF4_BR_DISEASE)) || (flags3 & (RF3_RES_DISEASE)))	vp[n++] = "disease";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_vulnerabilities(u32b flags3, cptr vp[64])
{
	int n;

	/* Collect vulnerabilities */
	n = 0;
	if (flags3 & (RF3_HURT_ROCK)) vp[n++] = "rock remover";
	if (flags3 & (RF3_HURT_LITE)) vp[n++] = "bright light";
	if (flags3 & (RF3_HURT_DARK)) vp[n++] = "darkness";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_immunes(u32b flags3, u32b flags4, cptr vp[64])
{
	int n;

	/* Collect resistances */
	n = 0;
	if (flags3 & (RF3_NO_STUN))		vp[n++] = "stunned";
	if (flags3 & (RF3_NO_FEAR))		vp[n++] = "frightened";
	if (flags3 & (RF3_NO_SLEEP))	vp[n++] = "put to sleep";
	if (flags3 & (RF3_NO_CUT))		vp[n++] = "made to bleed";
	if (flags3 & (RF3_NO_BLIND))	vp[n++] = "blinded";
	if (flags3 & (RF3_NO_CALM))		vp[n++] = "pacified";

	/* 
	 * Note - the following flags also appear under resistances/breaths which might be annoying 
	 * to some players and takes screen real estate when displaying the monster memory
	 * but is more intuitive for beginners.
	 */

	if ((flags4 & (RF4_BR_POIS)) || (flags3 & (RF3_RES_POIS)))	vp[n++] = "poisoned";
	if ((flags4 & (RF4_BR_CONF)) || (flags3 & (RF3_RES_CONF)))	vp[n++] = "confused";

	return n;
}

/*
 * Collect the spell names - seperate function so it can also be used in spoiler creation
 */ 
int collect_mon_spells(u32b flags5, u32b flags6, cptr vp[64])
{
	int n;

	/* Collect spells */	
	n = 0;
	if (flags5 & (RF5_BA_ACID))		vp[n++] = "produce acid balls";
	if (flags5 & (RF5_BA_ELEC))		vp[n++] = "produce lightning balls";
	if (flags5 & (RF5_BA_FIRE))		vp[n++] = "produce fire balls";
	if (flags5 & (RF5_BA_COLD))		vp[n++] = "produce frost balls";
	if (flags5 & (RF5_BA_POIS))		vp[n++] = "produce poison balls";
	if (flags5 & (RF5_BA_NETH))		vp[n++] = "produce nether balls";
	if (flags5 & (RF5_BA_WATE))		vp[n++] = "produce water balls";
	if (flags5 & (RF5_BA_MANA))		vp[n++] = "produce mana storms";
	if (flags5 & (RF5_BA_DARK))		vp[n++] = "produce darkness storms";
	if (flags5 & (RF5_DRAIN_MANA))	vp[n++] = "drain mana";
	if (flags5 & (RF5_MIND_BLAST))	vp[n++] = "cause mind blasting";
	if (flags5 & (RF5_BRAIN_SMASH))	vp[n++] = "cause brain smashing";
	if (flags5 & (RF5_CAUSE_1))		vp[n++] = "cause light wounds";
	if (flags5 & (RF5_CAUSE_2))		vp[n++] = "cause serious wounds";
	if (flags5 & (RF5_CAUSE_3))		vp[n++] = "cause critical wounds";
	if (flags5 & (RF5_CAUSE_4))		vp[n++] = "cause mortal wounds";
	if (flags5 & (RF5_BO_ACID))		vp[n++] = "produce acid bolts";
	if (flags5 & (RF5_BO_ELEC))		vp[n++] = "produce lightning bolts";
	if (flags5 & (RF5_BO_FIRE))		vp[n++] = "produce fire bolts";
	if (flags5 & (RF5_BO_COLD))		vp[n++] = "produce frost bolts";
	if (flags5 & (RF5_BO_NETH))		vp[n++] = "produce nether bolts";
	if (flags5 & (RF5_BO_WATE))		vp[n++] = "produce water bolts";
	if (flags5 & (RF5_BO_MANA))		vp[n++] = "produce mana bolts";
	if (flags5 & (RF5_BO_PLAS))		vp[n++] = "produce plasma bolts";
	if (flags5 & (RF5_BO_ICEE))		vp[n++] = "produce ice bolts";
	if (flags5 & (RF5_MISSILE))		vp[n++] = "produce magic missiles";
	if (flags5 & (RF5_SCARE))		vp[n++] = "terrify";
	if (flags5 & (RF5_BLIND))		vp[n++] = "blind";
	if (flags5 & (RF5_CONF))		vp[n++] = "confuse";
	if (flags5 & (RF5_SLOW))		vp[n++] = "slow";
	if (flags5 & (RF5_HOLD))		vp[n++] = "paralyze";
	if (flags6 & (RF6_HASTE))		vp[n++] = "haste-self";
	if (flags6 & (RF6_HEAL))		vp[n++] = "heal-self";
	if (flags6 & (RF6_BLINK))		vp[n++] = "blink randomly";
	if (flags6 & (RF6_BLINK_TO))	vp[n++] = "blink near the player";
	if (flags6 & (RF6_TPORT))		vp[n++] = "teleport-self";
	if (flags6 & (RF6_TELE_TO))		vp[n++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY))	vp[n++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL))	vp[n++] = "teleport level";
	if (flags6 & (RF6_DARKNESS))	vp[n++] = "create darkness";
	if (flags6 & (RF6_TRAPS))		vp[n++] = "create traps";
	if (flags6 & (RF6_FORGET))		vp[n++] = "cause amnesia";
	if (flags6 & (RF6_S_KIN))		vp[n++] = "summon similar monsters";
	if (flags6 & (RF6_S_HI_DEMON))	vp[n++] = "summon greater demons";
	if (flags6 & (RF6_S_MONSTER))	vp[n++] = "summon a monster";
	if (flags6 & (RF6_S_MONSTERS))	vp[n++] = "summon monsters";
	if (flags6 & (RF6_S_ANIMALS))	vp[n++] = "summon natural creatures";
	if (flags6 & (RF6_S_SPIDER))	vp[n++] = "summon spiders";
	if (flags6 & (RF6_S_HOUND))		vp[n++] = "summon hounds";
	if (flags6 & (RF6_S_HYDRA))		vp[n++] = "summon hydras";
	if (flags6 & (RF6_S_HORROR))	vp[n++] = "summon a nameless horror";
	if (flags6 & (RF6_S_DEMON))		vp[n++] = "summon a demon";
	if (flags6 & (RF6_S_UNDEAD))	vp[n++] = "summon an undead";
	if (flags6 & (RF6_S_DRAGON))	vp[n++] = "summon a dragon";
	if (flags6 & (RF6_S_HI_UNDEAD))	vp[n++] = "summon greater undead";
	if (flags6 & (RF6_S_HI_DRAGON))	vp[n++] = "summon ancient dragons";
	if (flags6 & (RF6_S_WRAITH))	vp[n++] = "summon ring wraiths";
	if (flags6 & (RF6_S_UNIQUE))	vp[n++] = "summon unique monsters";

	return n;
}

/*
 * Describe the attacks - seperate function so it can also be used in spoiler creation
 */ 
void describe_mon_attacks(int method, int effect, cptr method_text[1], cptr effect_text[1])
{
	/* Get the method */
	switch (method)
	{
		case RBM_HIT:		method_text[0] = "hit"; break;
		case RBM_TOUCH:		method_text[0] = "touch"; break;
		case RBM_PUNCH:		method_text[0] = "punch"; break;
		case RBM_KICK:		method_text[0] = "kick"; break;
		case RBM_CLAW:		method_text[0] = "claw"; break;
		case RBM_BITE:		method_text[0] = "bite"; break;
		case RBM_STING:		method_text[0] = "sting"; break;
		case RBM_KISS:		method_text[0] = "kiss"; break;
		case RBM_BUTT:		method_text[0] = "butt"; break;
		case RBM_CRUSH:		method_text[0] = "crush"; break;
		case RBM_ENGULF:	method_text[0] = "engulf"; break;
		case RBM_CRAWL:		method_text[0] = "crawl on you"; break;
		case RBM_DROOL:		method_text[0] = "drool on you"; break;
		case RBM_SPIT:		method_text[0] = "spit"; break;
		case RBM_GAZE:		method_text[0] = "gaze"; break;
		case RBM_WAIL:		method_text[0] = "wail"; break;
		case RBM_SPORE:		method_text[0] = "release spores"; break;
		case RBM_BEG:		method_text[0] = "beg"; break;
		case RBM_INSULT:	method_text[0] = "insult"; break;
		case RBM_MOAN:		method_text[0] = "moan"; break;
	}

	/* Get the effect */
	switch (effect)
	{
		case RBE_HURT:		effect_text[0] = "attack"; break;
		case RBE_POISON:	effect_text[0] = "poison"; break;
		case RBE_DISEASE:	effect_text[0] = "spread disease"; break;
		case RBE_UN_BONUS:	effect_text[0] = "disenchant"; break;
		case RBE_UN_POWER:	effect_text[0] = "drain charges"; break;
		case RBE_EAT_GOLD:	effect_text[0] = "steal gold"; break;
		case RBE_EAT_ITEM:	effect_text[0] = "steal items"; break;
		case RBE_EAT_FOOD:	effect_text[0] = "eat your food"; break;
		case RBE_EAT_LITE:	effect_text[0] = "absorb light"; break;
		case RBE_ACID:		effect_text[0] = "shoot acid"; break;
		case RBE_ELEC:		effect_text[0] = "electrify"; break;
		case RBE_FIRE:		effect_text[0] = "burn"; break;
		case RBE_COLD:		effect_text[0] = "freeze"; break;
		case RBE_RUST:		effect_text[0] = "rust"; break;
		case RBE_ROT:		effect_text[0] = "rot"; break;
		case RBE_BLIND:		effect_text[0] = "blind"; break;
		case RBE_CONFUSE:	effect_text[0] = "confuse"; break;
		case RBE_TERRIFY:	effect_text[0] = "terrify"; break;
		case RBE_PARALYZE:	effect_text[0] = "paralyze"; break;
		case RBE_LOSE_STR:	effect_text[0] = "reduce strength"; break;
		case RBE_LOSE_INT:	effect_text[0] = "reduce intelligence"; break;
		case RBE_LOSE_WIS:	effect_text[0] = "reduce wisdom"; break;
		case RBE_LOSE_DEX:	effect_text[0] = "reduce dexterity"; break;
		case RBE_LOSE_CON:	effect_text[0] = "reduce constitution"; break;
		case RBE_LOSE_CHR:	effect_text[0] = "reduce charisma"; break;
		case RBE_LOSE_ALL:	effect_text[0] = "reduce all stats"; break;
		case RBE_SHATTER:	effect_text[0] = "shatter"; break;
		case RBE_EXP_1:		effect_text[0] = "lower experience (by 1%)"; break;
		case RBE_EXP_2:		effect_text[0] = "lower experience (by 2%)"; break;
		case RBE_EXP_3:		effect_text[0] = "lower experience (by 3%)"; break;
		case RBE_EXP_4:		effect_text[0] = "lower experience (by 4%)"; break;
	}
}

/*
 * Collect the types of the monster's companions 
 */ 
int collect_mon_group(u32b flags1, cptr vp[64])
{
	int n;

	/* Collect innate */
	n = 0;
	if (flags1 & (RF1_ESCORTS))			vp[n++] = "many escorts";
	else if (flags1 & (RF1_ESCORT))		vp[n++] = "escorts";
	if (flags1 & (RF1_FRIENDS))			vp[n++] = "many other similar companions";
	else if (flags1 & (RF1_FRIEND))		vp[n++] = "other similar companions";
	if (flags1 & (RF1_COMPANION))		vp[n++] = "a unique companion";

	return n;
}

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
 */
static void roff_main(int r_idx, int u_idx)
{
	monster_race *r_ptr = get_monster_fake(r_idx, u_idx);
	monster_lore *l_ptr = get_lore_idx(r_idx, u_idx);

	bool old = FALSE;
	bool sin = FALSE;

	int m, n, r;

	cptr p, q;

	int msex = 0;

	bool breath = FALSE;
	bool magic = FALSE;

	u32b flags1;
	u32b flags2;
	u32b flags3;
	u32b flags4;
	u32b flags5;
	u32b flags6;

	int vn;
	cptr vp[64];

#ifndef PREVENT_LOAD_R_TEXT
	char buf[2048];
#endif

	u32b i, j;

	/* Cheat -- know everything */
	if (cheat_know)
	{
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
		(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
		 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & (RF1_ONLY_GOLD)) l_ptr->r_drop_item = 0;
		if (r_ptr->flags1 & (RF1_ONLY_ITEM)) l_ptr->r_drop_gold = 0;

		/* Hack -- observe many spells */
		l_ptr->r_cast = MAX_UCHAR;

		/* Hack -- know all the flags */
		l_ptr->r_flags1 = r_ptr->flags1;
		l_ptr->r_flags2 = r_ptr->flags2;
		l_ptr->r_flags3 = r_ptr->flags3;
		l_ptr->r_flags4 = r_ptr->flags4;
		l_ptr->r_flags5 = r_ptr->flags5;
		l_ptr->r_flags6 = r_ptr->flags6;
	}

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;

	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & l_ptr->r_flags1);
	flags2 = (r_ptr->flags2 & l_ptr->r_flags2);
	/* Mega-hack - you can "know" a resistance even if the monster doesn't have it */
	flags3 = ((r_ptr->flags3 | RF3_RES_MASK) & l_ptr->r_flags3); 
	flags4 = (r_ptr->flags4 & l_ptr->r_flags4);
	flags5 = (r_ptr->flags5 & l_ptr->r_flags5);
	flags6 = (r_ptr->flags6 & l_ptr->r_flags6);

	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE))		flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & (RF1_MALE))			flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE))		flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_COMPANION))	flags1 |= (RF1_COMPANION);
	if (r_ptr->flags1 & (RF1_FRIEND))		flags1 |= (RF1_FRIEND);
	if (r_ptr->flags1 & (RF1_FRIENDS))		flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT))		flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS))		flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (l_ptr->r_tkills)
	{
		/* Know "race" flags */
		if (r_ptr->flags2 & (RF2_HUMANOID))	flags2 |= (RF2_HUMANOID);
		if (r_ptr->flags2 & (RF2_PERSON))	flags2 |= (RF2_PERSON);
		if (r_ptr->flags2 & (RF2_DRAGON))	flags2 |= (RF2_DRAGON);
		if (r_ptr->flags2 & (RF2_DEMON))	flags2 |= (RF2_DEMON);
		if (r_ptr->flags2 & (RF2_UNDEAD))	flags2 |= (RF2_UNDEAD);
		if (r_ptr->flags2 & (RF2_EVIL))		flags2 |= (RF2_EVIL);
		if (r_ptr->flags2 & (RF2_ANIMAL))	flags2 |= (RF2_ANIMAL);
		if (r_ptr->flags2 & (RF2_PLANT))	flags2 |= (RF2_PLANT);
		if (r_ptr->flags2 & (RF2_CHAOTIC))	flags2 |= (RF2_CHAOTIC);

		if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) flags1 |= (RF1_FORCE_MAXHP);
	}

	/* Treat uniques differently */
	if (u_idx)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (l_ptr->r_pkills > 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->r_deaths)
		{
			/* Killed ancestors */
			roff(format("%^s has slain ",
			            wd_he[msex], l_ptr->r_deaths));
			c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_deaths));
			roff("of your ancestors");

			/* But we've also killed it */
			if (dead)
			{
				roff(", but you have taken revenge!  ");
			}

			/* Unavenged */
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
		c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_deaths));
		roff(format("of your ancestors %s been killed by this creature, ",
		            plural(l_ptr->r_deaths, "has", "have")));

		/* Some kills this life */
		if (l_ptr->r_pkills)
		{
			roff("and you have exterminated at least ");
			c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_pkills));
			roff("of the creatures.  ");
		}

		/* Some kills past lives */
		else if (l_ptr->r_tkills)
		{
			roff("and your ancestors have exterminated at least ");
			c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_tkills));
			roff("of the creatures.  ");
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
			roff("You have killed at least ");
			c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_pkills));
			roff("of these creatures.  ");
		}

		/* Killed some last life */
		else if (l_ptr->r_tkills)
		{
			roff("Your ancestors have killed at least ");
			c_roff(TERM_L_GREEN,format("%d ",l_ptr->r_tkills));
			roff("of these creatures.  ");
		}

		/* Killed none */
		else
		{
			roff("No battles to the death are recalled.  ");
		}
	}

#ifndef PREVENT_LOAD_R_TEXT

	/* Simple method */
	strcpy(buf, monster_text(r_idx, u_idx));

	/* Dump it */
	roff(buf);
	roff("  ");

#endif /* PREVENT_LOAD_R_TEXT */

	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		roff(format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	else if (l_ptr->r_tkills)
	{
		if (depth_in_feet)
		{
			roff(format("%^s is normally found at depths of ",
			            wd_he[msex]));
			/* ~ different if it's out of depth or not */
			c_roff((byte)((r_ptr->level > p_ptr->max_depth) ? TERM_L_RED : TERM_L_GREEN),
			        format("%d ", r_ptr->level * 50));
			roff("feet");
		}
		else
		{
			roff(format("%^s is normally found on dungeon level ",
			            wd_he[msex]));
			/* ~ different if it's out of depth or not */
			c_roff((byte)((r_ptr->level > p_ptr->max_depth) ? TERM_L_RED : TERM_L_GREEN),
			        format("%d", r_ptr->level));
		}
		old = TRUE;
	}


	/* Introduction */
	if (old)
	{
		roff(", and ");
	}
	else
	{
		roff(format("%^s ", wd_he[msex]));
		old = TRUE;
	}
	roff("moves");

	/* Random-ness */
	if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
	{
		/* Adverb */
		if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
		{
			roff(" extremely");
		}
		else if (flags1 & (RF1_RAND_50))
		{
			roff(" somewhat");
		}
		else if (flags1 & (RF1_RAND_25))
		{
			roff(" a bit");
		}

		/* Adjective */
		roff(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) roff(", and");
	}

	/* Speed */
	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) roff(" incredibly");
		else if (r_ptr->speed > 120) roff(" very");
		roff(" quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) roff(" incredibly");
		else if (r_ptr->speed < 100) roff(" very");
		roff(" slowly");
	}
	else
	{
		roff(" at normal speed");
	}

	/* The code above includes "attack speed" */
	if (flags1 & (RF1_NEVER_MOVE))
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

	/* Describe experience if known */
	if (l_ptr->r_tkills)
	{
		/* Introduction */
		if (flags1 & (RF1_UNIQUE))
		{
			roff("Killing this");
		}
		else
		{
			roff("A kill of this");
		}

		/* Describe the "quality" */
		if (flags2 & (RF2_CHAOTIC))	roff(" chaotic");
		if (flags2 & (RF2_ANIMAL))	roff(" natural");
		if (flags2 & (RF2_EVIL))	roff(" evil");
		if (flags2 & (RF2_UNDEAD))	roff(" undead");

		/* Describe the "race" */
		if (flags2 & (RF2_DRAGON))			roff(" dragon");
		else if (flags2 & (RF2_DEMON))		roff(" demon");
		else if (flags2 & (RF2_HUMANOID))	roff(" humanoid");
		else if (flags2 & (RF2_PERSON))		roff(" person");
		else if (flags2 & (RF2_PLANT))		roff(" plant");
		else roff(" creature");

		/* Mention the experience */
		mon_exp(r_idx, u_idx, &i, &j);
		j = (j + 5) / 10;
		roff(" is worth ");
		if (j) c_roff(TERM_ORANGE,format("%ld.%02ld ", (long)i, (long)j));
		else c_roff(TERM_ORANGE,format("%ld ", (long)i));
		roff(format("point%s",
			        (((i == 1) && (j == 0)) ? "" : "s")));

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
		roff(format(" for a%s %lu%s level character.  ",
			        q, (long)i, p));
	}

	/* Collect monster associates */
	vn = collect_mon_group(flags1, vp);

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" usually appears with ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}

	/* Collect inate attacks */
	vn = collect_mon_innate(flags4, vp);

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}

	/* Collect breaths */
	vn = collect_mon_breaths(flags4, vp);

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" may breathe ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}
	}

	/* Collect spells */
	vn = collect_mon_spells(flags5, flags6, vp);

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			roff(", and is also");
		}
		else
		{
			roff(format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		roff(" magical, casting spells");

		/* Adverb */
		if (flags2 & (RF2_SMART)) roff(" intelligently");
		if (flags2 & (RF2_NEVER_FAIL)) roff(" without chance of failure");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" which ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}
	}

	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Average frequency */
		n = r_ptr->freq_spell;

		/* Describe the spell frequency */
		if (l_ptr->r_cast > 100)
		{
			roff("; ");
			c_roff(TERM_L_GREEN,"1");
			roff(" time in ");
			c_roff(TERM_L_GREEN,format("%d", 100 / n));
		}

		/* Guess at the frequency */
		else if (l_ptr->r_cast)
		{
			n = ((n + 9) / 10) * 10;
			roff("; about ");
			c_roff(TERM_L_GREEN,"1");
			roff(" time in ");
			c_roff(TERM_L_GREEN,format("%d", 100 / n));
		}

		/* End this sentence */
		roff(".  ");
	}

	/* Describe monster "toughness" */
	if (know_armour(l_ptr, r_idx, u_idx))
	{
		/* Armor */
		roff(format("%^s has an armor rating of ",wd_he[msex]));
		c_roff (TERM_L_GREEN,format("%d", r_ptr->ac));

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			roff(" and a life rating of ");
			c_roff(TERM_L_GREEN,format("%d",r_ptr->hdice * r_ptr->hside));
			roff(".  ");
		}

		/* Variable hitpoints */
		else
		{
			roff(" and a life rating of ");
			c_roff(TERM_L_GREEN,format("%dd%d",r_ptr->hdice, r_ptr->hside));
			roff(".  ");
		}
	}

	/* Collect special abilities. */
	vn = collect_mon_special(flags2, vp);

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

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

	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		roff(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		roff(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		roff(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		roff(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	/* -TM- */
	if (flags2 & (RF2_SEE_INVIS))
	{
		roff(format("%^s can see invisible enemies.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_MULTIPLY))
	{
		roff(format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_REGENERATE))
	{
		roff(format("%^s regenerates quickly.  ", wd_he[msex]));
	}

	/* Collect susceptibilities */
	vn = collect_mon_vulnerabilities(flags3, vp);

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" is hurt by ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}

	/* Collect immunities */
	vn = collect_mon_immunes(flags3, flags4, vp);

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" cannot be ");
			else if (n < vn-1) roff(", ");
			else roff(" or ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
	}

	/* Collect resistances */
	vn = collect_mon_resists(flags3, flags4, vp);

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		roff(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) roff(" resists ");
			else if (n < vn-1) roff(", ");
			else roff(" and ");

			/* Dump */
			roff(vp[n]);
		}

		/* End */
		roff(".  ");
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

		roff(format("%^s %s intruders, which %s may notice from ",wd_he[msex], act, wd_he[msex]));
		c_roff(TERM_L_GREEN,format("%d",10 * r_ptr->aaf));
	    roff(" feet.  ");
	}

	if (l_ptr->r_flags1 & RF1_DROP_MIMIC)
	{
		/* Intro */
		roff(format("%^s may be useful once dead.  ", wd_he[msex]));
	}

	/* Drops gold and/or items */
	else if (l_ptr->r_drop_gold || l_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		roff(format("%^s may carry", wd_he[msex]));

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
		if (flags1 & (RF1_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (flags1 & (RF1_DROP_GOOD))
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

	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->r_blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!l_ptr->r_blows[m]) continue;

		p = NULL;
		q = NULL;

		/* Extract the attack info */
		describe_mon_attacks(r_ptr->blow[m].method,r_ptr->blow[m].effect,&p,&q);
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;

		/* Introduce the attack description */
		if (!r)
		{
			roff(format("%^s can ", wd_he[msex]));
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
			if (d1 && d2 && know_damage(l_ptr, r_idx, u_idx, m))
			{
				/* Display the damage */
				roff(" with damage");
				c_roff(TERM_L_GREEN,format(" %dd%d", d1, d2));
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
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		roff(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		roff(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}

	/* All done */
	roff("\n");
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx, int u_idx)
{
	cptr name;
	
	byte a1, a2;
	char c1, c2;

	monster_race *r_ptr = get_monster_fake(r_idx, u_idx);

	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;

	name = monster_name_idx(r_idx, u_idx);

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* In wizard mode, show r_idx */
	if (cheat_wizard) 
	{
		if (u_idx) Term_addstr(-1, TERM_WHITE, format("%d/%d: ",r_idx,u_idx));
		else Term_addstr(-1, TERM_WHITE, format("%d: ",r_idx));
	}

	/* A title (use "The" for non-uniques) */
	if (!u_idx)
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, name);

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	Term_addstr(-1, TERM_WHITE, "'):");
}

static void roff_aux(int r_idx, int u_idx)
{
	/* Main body of lore */
	roff_main(r_idx, u_idx);

	/* Describe monster */
	roff_top(r_idx, u_idx);
}

/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, int u_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	roff_aux(r_idx, u_idx);	
}

/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx, int u_idx)
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
	roff_aux(r_idx, u_idx);
}

/*
 * Hack -- show a list of the visible monsters in the current "term" window
 */
void display_visible(void)
{
	int i, j;
	int c = 0;
	int items = 0;

	monster_list_entry *who;

	/* Clear */
	Term_clear();

	/* XXX Hallucination - no monster list */
	if (p_ptr->image)
	{		
		c_prt(TERM_WHITE,"You see a lot of pretty colours.",0,0);

		return;
	}

	/* Allocate the "who" array */
	C_MAKE(who, m_max, monster_list_entry);

	/* Count up the number visible in each race */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		bool found = FALSE;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip unseen monsters */
		if (!m_ptr->ml) continue;

		/* Increase for this race */
		if (items)
		{
			for (j = 0; j < items; j++)
			{
				if ((who[j].r_idx == m_ptr->r_idx) && (who[j].u_idx == m_ptr->u_idx))
				{
					who[j].amount++;
					
					found = TRUE;

					break;
				}
			}
		}
		
		if (!found)
		{
			who[items].r_idx = m_ptr->r_idx;
			who[items].u_idx = m_ptr->u_idx;
			who[items].amount = 1;

			items++;
		}

		/* Increase total Count */
		c++;
	}

	/* Are monsters visible? */
	if (items)
	{
		int w, h, num = 0;
		u16b why = 1;

		/* First, sort the monsters by expereince*/
		ang_sort_comp = ang_mon_sort_comp_hook;
		ang_sort_swap = ang_mon_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, items);

		/* Then, display them */
		(void)Term_get_size(&w, &h);

		c_prt(TERM_WHITE,format("You can see %d monster%s", c, (c > 1 ? "s:" : ":")), 0, 0);

		/* Print the monsters in reverse order */
		for (i = items - 1; i >= 0; i--)
		{
			monster_lore *l_ptr = get_lore_idx(who[i].r_idx, who[i].u_idx);
			monster_race *r_ptr = get_monster_fake(who[i].r_idx, who[i].u_idx);

			/* Default Colour */
			byte attr = TERM_WHITE;

			/* Uniques */
			if (who[i].u_idx)
			{
				attr = TERM_L_RED;
			}

			/* Have we ever killed one? */
			if (l_ptr->r_tkills)
			{
				if (r_ptr->level > p_ptr->depth)
				{
					attr = TERM_VIOLET;

					if (who[i].u_idx)
					{
						attr = TERM_RED;
					}
				}
			}
			else
			{
				if (!who[i].u_idx) attr = TERM_SLATE;
			}			
			
			/* Dump the monster name */
			if (who[i].amount == 1)
			{
				c_prt(attr, monster_name_idx(who[i].r_idx, who[i].u_idx), (num % (h - 1)) + 1, 
					(num / (h - 1) * 26));
			}
			else
			{
				c_prt(attr, format("%s (x%d)", monster_name_idx(who[i].r_idx, who[i].u_idx),
					who[i].amount), (num % (h - 1)) + 1, (num / (h - 1)) * 26);
			}

			num++;
		}
	}

	else
	{
		c_prt(TERM_WHITE,"You see no monsters.",0,0);
	}

	/* XXX XXX Free the "who" array */
	C_FREE(who, m_max, monster_list_entry);
}
