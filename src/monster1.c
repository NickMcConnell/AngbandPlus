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
monster_race monster_temp_text_out;

/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(monster_lore *l_ptr, int r_idx, int u_idx)
{
	monster_race *r_ptr = get_monster_fake(r_idx, 0, u_idx);
	s32b level = r_ptr->level;

	s32b kills = l_ptr->r_tkills;

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!u_idx) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4)) return (TRUE);

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
	monster_race *r_ptr = get_monster_fake(r_idx, 0, u_idx);

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
static int collect_mon_special(u32b flags2, cptr vp[64])
{
	int n;

	/* Collect innate */
	n = 0;
    if (flags2 & (RF2_HAS_LITE))  vp[n++] = "illuminate the dungeon";
	if (flags2 & (RF2_OPEN_DOOR)) vp[n++] = "open doors";
	if (flags2 & (RF2_BASH_DOOR)) vp[n++] = "bash down doors";
	if (flags2 & (RF2_PICK_LOCK)) vp[n++] = "pick locks";
	if (flags2 & (RF2_PASS_WALL)) vp[n++] = "pass through walls";
	if (flags2 & (RF2_KILL_WALL)) vp[n++] = "bore through walls";
	if (flags2 & (RF2_MOVE_BODY)) vp[n++] = "push past weaker monsters";
	if (flags2 & (RF2_KILL_BODY)) vp[n++] = "destroy weaker monsters";
	if (flags2 & (RF2_TAKE_ITEM)) vp[n++] = "pick up objects";
	if (flags2 & (RF2_KILL_ITEM)) vp[n++] = "destroy objects";
/*	if (flags2 & (RF2_NO_TRAP))	  vp[n++] = "avoid traps"; */

	return n;
}

/*
 * Collect the innate names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_innate(u32b s_flags1, cptr vp[64])
{
	int n;

	/* Collect innate */
	n = 0;
	if (s_flags1 & (SRF1_SHRIEK))		vp[n++] = "shriek for help";
	if (s_flags1 & (SRF1_ARROW_1))		vp[n++] = "fire an arrow";
	if (s_flags1 & (SRF1_ARROW_2))		vp[n++] = "fire arrows";
	if (s_flags1 & (SRF1_ARROW_3))		vp[n++] = "fire a missile";
	if (s_flags1 & (SRF1_ARROW_4))		vp[n++] = "fire missiles";

	return n;
}

/*
 * Collect the breath names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_breaths(u32b s_flags1, cptr vp[64])
{
	int n;

	/* Collect breaths */
	n = 0;
	if (s_flags1 & (SRF1_BR_ACID))		vp[n++] = "acid";
	if (s_flags1 & (SRF1_BR_ELEC))		vp[n++] = "lightning";
	if (s_flags1 & (SRF1_BR_FIRE))		vp[n++] = "fire";
	if (s_flags1 & (SRF1_BR_COLD))		vp[n++] = "frost";
	if (s_flags1 & (SRF1_BR_WATER))		vp[n++] = "water";
	if (s_flags1 & (SRF1_BR_POIS))		vp[n++] = "poison";
	if (s_flags1 & (SRF1_BR_DISEASE))	vp[n++] = "disease";
	if (s_flags1 & (SRF1_BR_LITE))		vp[n++] = "light";
	if (s_flags1 & (SRF1_BR_DARK))		vp[n++] = "darkness";
	if (s_flags1 & (SRF1_BR_SOUN))		vp[n++] = "sound";
	if (s_flags1 & (SRF1_BR_SHAR))		vp[n++] = "shards";
	if (s_flags1 & (SRF1_BR_NEXU))		vp[n++] = "nexus";
	if (s_flags1 & (SRF1_BR_NETH))		vp[n++] = "nether";
	if (s_flags1 & (SRF1_BR_CHAO))		vp[n++] = "chaos";
	if (s_flags1 & (SRF1_BR_DISE))		vp[n++] = "disenchantment";
	if (s_flags1 & (SRF1_BR_TIME))		vp[n++] = "time";
	if (s_flags1 & (SRF1_BR_MANA))		vp[n++] = "mana";
	if (s_flags1 & (SRF1_BR_FORCE))		vp[n++] = "force";
	if (s_flags1 & (SRF1_BR_INER))		vp[n++] = "inertia";
	if (s_flags1 & (SRF1_BR_PLAS))		vp[n++] = "plasma";
	if (s_flags1 & (SRF1_BR_GRAV))		vp[n++] = "gravity";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_resists(u32b flags3, cptr vp[64])
{
	int n;

	/* 
	 * Note - the following flags also appear under breaths which might be annoying 
	 * to some players and takes screen real estate when displaying the monster memory
	 * but is more intuitive for beginners. 
	 */
	n = 0;
	if (flags3 & (RF3_RES_ACID))		vp[n++] = "acid";
	if (flags3 & (RF3_RES_ELEC))		vp[n++] = "electricity";
	if (flags3 & (RF3_RES_FIRE))		vp[n++] = "fire";
	if (flags3 & (RF3_RES_COLD))		vp[n++] = "cold";
	if (flags3 & (RF3_RES_WATER))		vp[n++] = "water";
	if (flags3 & (RF3_RES_POIS))		vp[n++] = "poison";
	if (flags3 & (RF3_RES_DISEASE))		vp[n++] = "disease";
	if (flags3 & (RF3_RES_LITE))		vp[n++] = "light";
	if (flags3 & (RF3_RES_DARK))		vp[n++] = "darkness";
	if (flags3 & (RF3_RES_SOUN))		vp[n++] = "sound";
	if (flags3 & (RF3_RES_SHAR))		vp[n++] = "shards";
	if (flags3 & (RF3_RES_NEXU))		vp[n++] = "nexus";
	if (flags3 & (RF3_RES_NETH))		vp[n++] = "nether";
	if (flags3 & (RF3_RES_CHAO))		vp[n++] = "chaos";
	if (flags3 & (RF3_RES_DISE))		vp[n++] = "disenchantment";
	if (flags3 & (RF3_RES_TIME))		vp[n++] = "time";
	if (flags3 & (RF3_RES_MANA))		vp[n++] = "mana";
	if (flags3 & (RF3_RES_FORCE))		vp[n++] = "force";
	if (flags3 & (RF3_RES_INER))		vp[n++] = "inertia";
	if (flags3 & (RF3_RES_PLAS))		vp[n++] = "plasma";
	if (flags3 & (RF3_RES_GRAV))		vp[n++] = "gravity";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_vulnerabilities(u32b flags2, cptr vp[64])
{
	int n;

	/* Collect vulnerabilities */
	n = 0;
	if (flags2 & (RF2_HURT_ROCK)) vp[n++] = "rock remover";
	if (flags2 & (RF2_HURT_ACID)) vp[n++] = "acid";
	if (flags2 & (RF2_HURT_ELEC)) vp[n++] = "electricity";
	if (flags2 & (RF2_HURT_FIRE)) vp[n++] = "fire";
	if (flags2 & (RF2_HURT_COLD)) vp[n++] = "cold";
	if (flags2 & (RF2_HURT_LITE)) vp[n++] = "bright light";
	if (flags2 & (RF2_HURT_DARK)) vp[n++] = "darkness";

	return n;
}

/*
 * Collect the resist names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_immunes(u32b flags3, cptr vp[64])
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
	if (flags3 & (RF3_NO_CONF))	vp[n++] = "confused";

	/* 
	 * Note - the following flag also appears under resistances which might be annoying 
	 * to some players and takes screen real estate when displaying the monster memory
	 * but is more intuitive for beginners.
	 */

	if (flags3 & (RF3_RES_POIS))	vp[n++] = "poisoned";

	return n;
}

/*
 * Collect the spell names - seperate function so it can also be used in spoiler creation
 */ 
static int collect_mon_spells(u32b s_flags2, u32b s_flags3, cptr vp[64])
{
	int n;

	/* Collect spells */	
	n = 0;
	if (s_flags2 & (SRF2_BA_ACID))		vp[n++] = "produce acid balls";
	if (s_flags2 & (SRF2_BA_ELEC))		vp[n++] = "produce lightning balls";
	if (s_flags2 & (SRF2_BA_FIRE))		vp[n++] = "produce fire balls";
	if (s_flags2 & (SRF2_BA_COLD))		vp[n++] = "produce frost balls";
	if (s_flags2 & (SRF2_BA_POIS))		vp[n++] = "produce poison balls";
	if (s_flags2 & (SRF2_BA_WATE))		vp[n++] = "produce water balls";
	if (s_flags2 & (SRF2_BA_NETH))		vp[n++] = "produce nether balls";
	if (s_flags2 & (SRF2_BA_MANA))		vp[n++] = "produce mana storms";
	if (s_flags2 & (SRF2_BA_DARK))		vp[n++] = "produce darkness storms";
	if (s_flags2 & (SRF2_DRAIN_MANA))	vp[n++] = "drain mana";
	if (s_flags2 & (SRF2_MIND_BLAST))	vp[n++] = "cause mind blasting";
	if (s_flags2 & (SRF2_BRAIN_SMASH))	vp[n++] = "cause brain smashing";
	if (s_flags2 & (SRF2_CAUSE_1))		vp[n++] = "cause light wounds";
	if (s_flags2 & (SRF2_CAUSE_2))		vp[n++] = "cause serious wounds";
	if (s_flags2 & (SRF2_CAUSE_3))		vp[n++] = "cause critical wounds";
	if (s_flags2 & (SRF2_CAUSE_4))		vp[n++] = "cause mortal wounds";
	if (s_flags2 & (SRF2_BO_ACID))		vp[n++] = "produce acid bolts";
	if (s_flags2 & (SRF2_BO_ELEC))		vp[n++] = "produce lightning bolts";
	if (s_flags2 & (SRF2_BO_FIRE))		vp[n++] = "produce fire bolts";
	if (s_flags2 & (SRF2_BO_COLD))		vp[n++] = "produce frost bolts";
	if (s_flags2 & (SRF2_BO_WATE))		vp[n++] = "produce water bolts";
	if (s_flags2 & (SRF2_BO_NETH))		vp[n++] = "produce nether bolts";
	if (s_flags2 & (SRF2_BO_MANA))		vp[n++] = "produce mana bolts";
	if (s_flags2 & (SRF2_BO_PLAS))		vp[n++] = "produce plasma bolts";
	if (s_flags2 & (SRF2_BO_ICEE))		vp[n++] = "produce ice bolts";
	if (s_flags2 & (SRF2_MISSILE))		vp[n++] = "produce magic missiles";
	if (s_flags2 & (SRF2_SCARE))		vp[n++] = "terrify";
	if (s_flags2 & (SRF2_BLIND))		vp[n++] = "blind";
	if (s_flags2 & (SRF2_CONF))			vp[n++] = "confuse";
	if (s_flags2 & (SRF2_SLOW))			vp[n++] = "slow";
	if (s_flags2 & (SRF2_HOLD))			vp[n++] = "paralyze";
	if (s_flags3 & (SRF3_HASTE))		vp[n++] = "haste-self";
	if (s_flags3 & (SRF3_HEAL))			vp[n++] = "heal-self";
	if (s_flags3 & (SRF3_BLINK))		vp[n++] = "blink randomly";
	if (s_flags3 & (SRF3_BLINK_TO))		vp[n++] = "blink near the player";
	if (s_flags3 & (SRF3_TPORT))		vp[n++] = "teleport-self";
	if (s_flags3 & (SRF3_TELE_TO))		vp[n++] = "teleport to";
	if (s_flags3 & (SRF3_TELE_AWAY))	vp[n++] = "teleport away";
	if (s_flags3 & (SRF3_TELE_LEVEL))	vp[n++] = "teleport level";
	if (s_flags3 & (SRF3_DARKNESS))		vp[n++] = "create darkness";
	if (s_flags3 & (SRF3_TRAPS1))		vp[n++] = "create weak traps";
	if (s_flags3 & (SRF3_TRAPS2))		vp[n++] = "create moderate traps";
	if (s_flags3 & (SRF3_TRAPS3))		vp[n++] = "create powerful traps";
	if (s_flags3 & (SRF3_FORGET))		vp[n++] = "cause amnesia";
	if (s_flags3 & (SRF3_S_KIN))		vp[n++] = "summon similar monsters";
	if (s_flags3 & (SRF3_S_HI_DEMON))	vp[n++] = "summon greater demons";
	if (s_flags3 & (SRF3_S_MONSTER))	vp[n++] = "summon a monster";
	if (s_flags3 & (SRF3_S_MONSTERS))	vp[n++] = "summon monsters";
	if (s_flags3 & (SRF3_S_ANIMALS))	vp[n++] = "summon natural creatures";
	if (s_flags3 & (SRF3_S_SPIDER))		vp[n++] = "summon spiders";
	if (s_flags3 & (SRF3_S_FAERY))		vp[n++] = "summon faery creatures";
	if (s_flags3 & (SRF3_S_HOUND))		vp[n++] = "summon hounds";
	if (s_flags3 & (SRF3_S_HYDRA))		vp[n++] = "summon hydras";
	if (s_flags3 & (SRF3_S_HORROR))		vp[n++] = "summon a nameless horror";
	if (s_flags3 & (SRF3_S_DEMON))		vp[n++] = "summon a demon";
	if (s_flags3 & (SRF3_S_UNDEAD))		vp[n++] = "summon an undead";
	if (s_flags3 & (SRF3_S_DRAGON))		vp[n++] = "summon a dragon";
	if (s_flags3 & (SRF3_S_HI_UNDEAD))	vp[n++] = "summon greater undead";
	if (s_flags3 & (SRF3_S_HI_DRAGON))	vp[n++] = "summon ancient dragons";
	if (s_flags3 & (SRF3_S_UNIQUE))		vp[n++] = "summon unique monsters";

	return n;
}

/*
 * Describe the attacks - seperate function so it can also be used in spoiler creation
 */ 
static void describe_mon_attacks(int method, int effect, cptr method_text[1], cptr effect_text[1])
{
	/* Get the method */
	switch (method)
	{
		case RBM_HIT:		method_text[0] = "hit"; break;
		case RBM_TOUCH:		method_text[0] = "touch"; break;
		case RBM_PUNCH:		method_text[0] = "punch"; break;
		case RBM_KICK:		method_text[0] = "kick"; break;
		case RBM_GRAB:		method_text[0] = "grab"; break;
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
		case RBE_HALLU:		effect_text[0] = "cause hallucinations"; break;
		case RBE_TAINT:		effect_text[0] = "taint your soul"; break;
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
	if (flags1 & (RF1_GRP_ESCORT))
	{
		if (flags1 & (RF1_GRP_27)) vp[n++] = "many escorts";
		else if (flags1 & (RF1_GRP_18)) vp[n++] = "escorts";
		else if (flags1 & (RF1_GRP_9)) vp[n++] = "a few escorts";
	}
	else if (flags1 & (RF1_GRP_PEER))
	{
		if (flags1 & (RF1_GRP_27)) vp[n++] = "many companions";
		else if (flags1 & (RF1_GRP_18)) vp[n++] = "companions";
		else if (flags1 & (RF1_GRP_9)) vp[n++] = "a few companions";
	}
	else 
	{
		if (flags1 & (RF1_GRP_27)) vp[n++] = "many similar companions";
		else if (flags1 & (RF1_GRP_18)) vp[n++] = "similar companions";
		else if (flags1 & (RF1_GRP_9)) vp[n++] = "a few similar companions";
	}

	if (flags1 & (RF1_COMPANION)) vp[n++] = "a unique companion";

	return n;
}

/*
 * Hack -- display monster information using "text_out()"
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
void describe_monster(int r_idx, int u_idx, bool spoilers)
{
	monster_race *r_ptr = get_monster_fake(r_idx, 0, u_idx);
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
	u32b s_flags1;
	u32b s_flags2;
	u32b s_flags3;

	int vn;
	cptr vp[64];

	char buf[2048];

	u32b i, j;

	/* Spoilers or Cheat -- know everything */
	if (cheat_know || spoilers)
	{
		/* Hack -- Maximal kills */
		l_ptr->r_tkills = MAX_SHORT;

		/* Hack -- Maximal info */
		l_ptr->r_wake = l_ptr->r_ignore = MAX_UCHAR;

		/* Observe "maximal" attacks */
		for (m = 0; m < MONSTER_BLOW_MAX; m++)
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
		l_ptr->flags1 = r_ptr->flags1;
		l_ptr->flags2 = r_ptr->flags2;
		l_ptr->flags3 = r_ptr->flags3;
		l_ptr->flags4 = r_ptr->flags4;
		l_ptr->s_flags1 = r_ptr->s_flags1;
		l_ptr->s_flags2 = r_ptr->s_flags2;
		l_ptr->s_flags3 = r_ptr->s_flags3;
	}

	/* Extract a gender (if applicable) */
	if (r_ptr->flags4 & (RF4_FEMALE)) msex = 2;
	else if (r_ptr->flags4 & (RF4_MALE)) msex = 1;

	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & l_ptr->flags1);
	flags2 = (r_ptr->flags2 & l_ptr->flags2);
	flags3 = (r_ptr->flags3 & l_ptr->flags3); 
	flags4 = (r_ptr->flags4 & l_ptr->flags4);
	s_flags1 = (r_ptr->s_flags1 & l_ptr->s_flags1);
	s_flags2 = (r_ptr->s_flags2 & l_ptr->s_flags2);
	s_flags3 = (r_ptr->s_flags3 & l_ptr->s_flags3);

	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE))		flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags4 & (RF4_MALE))			flags4 |= (RF4_MALE);
	if (r_ptr->flags4 & (RF4_FEMALE))		flags4 |= (RF4_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_COMPANION))	flags1 |= (RF1_COMPANION);
	if (r_ptr->flags1 & (RF1_GRP_9))		flags1 |= (RF1_GRP_9);
	if (r_ptr->flags1 & (RF1_GRP_18))		flags1 |= (RF1_GRP_18);
	if (r_ptr->flags1 & (RF1_GRP_27))		flags1 |= (RF1_GRP_27);
	if (r_ptr->flags1 & (RF1_GRP_PEER))		flags1 |= (RF1_GRP_PEER);
	if (r_ptr->flags1 & (RF1_GRP_ESCORT))	flags1 |= (RF1_GRP_ESCORT);

	/* Killing a monster reveals some properties */
	if (l_ptr->r_tkills)
	{
		/* Know "race" flags */
		if (r_ptr->flags4 & RF4_HUMANOID)	flags4 |= RF4_HUMANOID;
		if (r_ptr->flags4 & RF4_PERSON)		flags4 |= RF4_PERSON;
		if (r_ptr->flags4 & RF4_FAERY)		flags4 |= RF4_FAERY;
		if (r_ptr->flags4 & RF4_DRAGON)		flags4 |= RF4_DRAGON;
		if (r_ptr->flags4 & RF4_DEMON)		flags4 |= RF4_DEMON;
		if (r_ptr->flags4 & RF4_UNDEAD)		flags4 |= RF4_UNDEAD;
		if (r_ptr->flags4 & RF4_EVIL)		flags4 |= RF4_EVIL;
		if (r_ptr->flags4 & RF4_ANIMAL)		flags4 |= RF4_ANIMAL;
		if (r_ptr->flags4 & RF4_PLANT)		flags4 |= RF4_PLANT;
		if (r_ptr->flags4 & RF4_CHAOTIC)	flags4 |= RF4_CHAOTIC;
	}

	/* Ancestor info */
	if (!spoilers)
	{
		/* Treat uniques differently */
		if (u_idx)
		{
			/* Hack -- Determine if the unique is "dead" */
			bool dead = (l_ptr->r_pkills > 0) ? TRUE : FALSE;

			/* We've been killed... */
			if (l_ptr->r_deaths)
			{
				/* Killed ancestors */
				text_out(format("%^s has slain ", wd_he[msex]));
				text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_deaths));
				text_out("of your ancestors");

				/* But we've also killed it */
				if (dead)
				{
					text_out(", but you have taken revenge!  ");
				}

				/* Unavenged */
				else
				{
					text_out(format(", who %s unavenged.  ", 
						plural(l_ptr->r_deaths, "remains", "remain")));
				}
			}

			/* Dead unique who never hurt us */
			else if (dead)
			{
				text_out("You have slain this foe.  ");
			}
		}

		/* Not unique, but killed us */
		else if (l_ptr->r_deaths)
		{
			/* Dead ancestors */
			text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_deaths));
			text_out(format("of your ancestors %s been killed by this creature, ",
						plural(l_ptr->r_deaths, "has", "have")));

			/* Some kills this life */
			if (l_ptr->r_pkills)
			{
				text_out("and you have exterminated at least ");
				text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_pkills));
				text_out("of the creatures.  ");
			}

			/* Some kills past lives */
			else if (l_ptr->r_tkills)
			{
				text_out("and your ancestors have exterminated at least ");
				text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_tkills));
				text_out("of the creatures.  ");
			}

			/* No kills */
			else
			{
				text_out(format("and %s is not ever known to have been defeated.  ",
							wd_he[msex]));
			}
		}

		/* Normal monsters */
		else
		{
			/* Killed some this life */
			if (l_ptr->r_pkills)
			{
				text_out("You have killed at least ");
				text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_pkills));
				text_out("of these creatures.  ");
			}

			/* Killed some last life */
			else if (l_ptr->r_tkills)
			{
				text_out("Your ancestors have killed at least ");
				text_out_c(TERM_L_GREEN,format("%d ",l_ptr->r_tkills));
				text_out("of these creatures.  ");
			}

			/* Killed none */
			else
			{
				text_out("No battles to the death are recalled.  ");
			}
		}
	}

	/* Simple method */
	my_strcpy(buf, monster_text(r_idx, u_idx), sizeof(buf));

	/* Dump it */
	text_out(buf);
	text_out("  ");

	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		text_out(format("%^s lives in the town", wd_he[msex]));
		old = TRUE;
	}
	else if (l_ptr->r_tkills)
	{
		if (depth_in_feet)
		{
			text_out(format("%^s is normally found at depths of ",
			            wd_he[msex]));
			/* ~ different if it's out of depth or not */
			text_out_c((byte)((r_ptr->level > p_ptr->max_depth) ? TERM_L_RED : TERM_L_GREEN),
			        format("%d ", r_ptr->level * 50));
			text_out("feet");
		}
		else
		{
			text_out(format("%^s is normally found on dungeon level ",
			            wd_he[msex]));
			/* ~ different if it's out of depth or not */
			text_out_c((byte)((r_ptr->level > p_ptr->max_depth) ? TERM_L_RED : TERM_L_GREEN),
			        format("%d", r_ptr->level));
		}
		old = TRUE;
	}


	/* Introduction */
	if (old)
	{
		text_out(", and ");
	}
	else
	{
		text_out(format("%^s ", wd_he[msex]));
		old = TRUE;
	}
	text_out("moves");

	/* Random-ness */
	if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
	{
		/* Adverb */
		if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
		{
			text_out(" extremely");
		}
		else if (flags1 & (RF1_RAND_50))
		{
			text_out(" somewhat");
		}
		else if (flags1 & (RF1_RAND_25))
		{
			text_out(" a bit");
		}

		/* Adjective */
		text_out(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) text_out(", and");
	}

	/* Speed */
	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) text_out(" incredibly");
		else if (r_ptr->speed > 120) text_out(" very");
		text_out(" quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) text_out(" incredibly");
		else if (r_ptr->speed < 100) text_out(" very");
		text_out(" slowly");
	}
	else
	{
		text_out(" at normal speed");
	}

	/* The code above includes "attack speed" */
	if (flags2 & (RF2_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			text_out(", but ");
		}
		else
		{
			text_out(format("%^s ", wd_he[msex]));
			old = TRUE;
		}

		/* Describe */
		text_out("does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		text_out(".  ");
		old = FALSE;
	}

	/* Describe experience if known */
	if (l_ptr->r_tkills)
	{
		/* Introduction */
		if (u_idx) text_out("Killing this");
		else text_out("A kill of this");

		/* Describe the "intelligence" */
		if (flags1 & (RF1_SMART))	text_out(" intelligent");
		if (flags1 & (RF1_STUPID))	text_out(" unintelligent");

		/* Describe the "quality" */
		if (flags4 & (RF4_CHAOTIC))	text_out(" chaotic");
		if (flags4 & (RF4_EVIL))	text_out(" evil");
		if (flags4 & (RF4_UNDEAD))	text_out(" undead");
		if (flags4 & (RF4_FAERY))	text_out(" faery");

		/* Describe the "race" */
		if (flags4 & (RF4_DRAGON))			text_out(" dragon");
		else if (flags4 & (RF4_DEMON))		text_out(" demon");
		else if (flags4 & (RF4_HUMANOID))	text_out(" humanoid");
		else if (flags4 & (RF4_PERSON))		text_out(" person");
		else if (flags4 & (RF4_PLANT))		text_out(" plant");
		else if (flags4 & (RF4_ANIMAL))		text_out(" animal");
		else text_out(" creature");

		/* Mention the experience */
		mon_exp(r_idx, 0, u_idx, &i, &j);
		j = (j + 5) / 10;
		text_out(" is worth ");
		if (j) text_out_c(TERM_ORANGE,format("%ld.%02ld ", (long)i, (long)j));
		else text_out_c(TERM_ORANGE,format("%ld ", (long)i));
		text_out(format("experience point%s",
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
		text_out(format(" for a%s %lu%s level character.  ", q, (long)i, p));
	}

	/* Collect monster associates */
	vn = collect_mon_group(flags1, vp);

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" usually appears with ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Collect inate attacks */
	vn = collect_mon_innate(s_flags1, vp);

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Collect breaths */
	vn = collect_mon_breaths(s_flags1, vp);

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may breathe ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Collect spells */
	vn = collect_mon_spells(s_flags2, s_flags3, vp);

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			text_out(", and is also");
		}
		else
		{
			text_out(format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		text_out(" magical, casting spells");

		/* Adverb */
		if (flags2 & (RF2_NEVER_FAIL)) text_out(" without chance of failure");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" which ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
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
			text_out("; ");
			text_out_c(TERM_L_GREEN, "1");
			text_out(" time in ");
			text_out_c(TERM_L_GREEN, format("%d", 100 / n));
		}

		/* Guess at the frequency */
		else if (l_ptr->r_cast)
		{
			n = ((n + 9) / 10) * 10;
			text_out("; about ");
			text_out_c(TERM_L_GREEN, "1");
			text_out(" time in ");
			text_out_c(TERM_L_GREEN, format("%d", 100 / n));
		}

		/* End this sentence */
		text_out(".  ");
	}

	/* Describe monster "toughness" */
	if (know_armour(l_ptr, r_idx, u_idx))
	{
		/* Armor */
		text_out(format("%^s has an armor rating of ",wd_he[msex]));
		text_out_c (TERM_L_GREEN,format("%d", r_ptr->ac));

		if (u_idx) text_out(" and a life rating of ");
		else text_out(" and a average life rating of ");
		text_out_c(TERM_L_GREEN,format("%d", r_ptr->life));
		text_out(".  ");
	}

	/* Collect special abilities. */
	vn = collect_mon_special(flags2, vp);

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		text_out(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		text_out(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		text_out(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		text_out(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	/* -TM- */
	if (flags2 & (RF2_SEE_INVIS))
	{
		text_out(format("%^s can see invisible enemies.  ", wd_he[msex]));
	}
	if (flags1 & (RF1_MULTIPLY))
	{
		text_out(format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_REGENERATE))
	{
		text_out(format("%^s regenerates quickly.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EVASIVE))
	{
		text_out(format("%^s can evade attacks.  ", wd_he[msex]));
	}
	/* Collect susceptibilities */
	vn = collect_mon_vulnerabilities(flags2, vp);

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" is vulnerable to ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Collect immunities */
	vn = collect_mon_immunes(flags3, vp);

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" cannot be ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Collect resistances */
	vn = collect_mon_resists(flags3, vp);

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" resists ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
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

		text_out(format("%^s %s intruders, which %s may notice from ",wd_he[msex], act, wd_he[msex]));
		text_out_c(TERM_L_GREEN,format("%d",10 * r_ptr->aaf));
	    text_out(" feet.  ");
	}

	if (l_ptr->flags1 & RF1_DROP_MIMIC)
	{
		/* Intro */
		text_out(format("%^s may be useful once dead.  ", wd_he[msex]));
	}

	/* Drops gold and/or items */
	else if (l_ptr->r_drop_gold || l_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		text_out(format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(l_ptr->r_drop_gold, l_ptr->r_drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			text_out(" a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			text_out(" one or two");
		}

		/* Many drops */
		else
		{
			text_out(format(" up to %d", n));
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
			if (sin) text_out("n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) text_out(p);
			text_out(" object");
			if (n != 1) text_out("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->r_drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) text_out("n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) text_out(p);
			text_out(" treasure");
			if (n != 1) text_out("s");
		}

		/* End this sentence */
		text_out(".  ");
	}

	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->r_blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
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
			text_out(format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}

		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		text_out(p);

		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			text_out(" to ");
			text_out(q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(l_ptr, r_idx, u_idx, m))
			{
				/* Display the damage */
				text_out(" with damage");
				text_out_c(TERM_L_GREEN,format(" %dd%d", d1, d2));
			}
		}

		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		text_out(".  ");
	}

	/* Notice lack of attacks */
	else if (flags2 & (RF2_NEVER_BLOW))
	{
		text_out(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		text_out(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}

	/* All done */
	text_out("\n");
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx, int u_idx)
{
	cptr name;
	
	byte a1, a2;
	char c1, c2;

	monster_race *r_ptr = get_monster_fake(r_idx, 0, u_idx);

	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;

	name = monster_name_idx(r_idx, 0, u_idx);

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
	describe_monster(r_idx, u_idx, FALSE);

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

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

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

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

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
	C_MAKE(who, mon_max, monster_list_entry);

	/* Count up the number visible in each race */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

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
			monster_race *r_ptr = get_monster_fake(who[i].r_idx, 0, who[i].u_idx);

			/* Default Colour */
			byte attr = TERM_WHITE;

			/* Uniques */
			if (who[i].u_idx) attr = TERM_L_RED;

			/* Have we ever killed one? */
			if (l_ptr->r_tkills)
			{
				if (r_ptr->level > p_ptr->depth)
				{
					if (who[i].u_idx) attr = TERM_RED;
					else attr = TERM_VIOLET;
				}
			}
			else
			{
				if (!who[i].u_idx) attr = TERM_SLATE;
			}			
			
			/* Dump the monster symbol and name */
			c_prt(r_ptr->x_attr, format("%c ",r_ptr->x_char), (num % (h - 1)) + 1, (num / (h - 1) * 26));
			if (who[i].amount == 1)
			{
				c_prt(attr, monster_name_idx(who[i].r_idx, 0, who[i].u_idx), (num % (h - 1)) + 1, 
					(num / (h - 1) * 26) + 2);
			}
			else
			{
				c_prt(attr, format("%s (x%d)", monster_name_idx(who[i].r_idx, 0, who[i].u_idx),
					who[i].amount), (num % (h - 1)) + 1, ((num / (h - 1)) * 26) + 2);
			}

			num++;
		}
	}

	else
	{
		c_prt(TERM_WHITE,"You see no monsters.",0,0);
	}

	/* XXX XXX Free the "who" array */
	FREE(who);
}
