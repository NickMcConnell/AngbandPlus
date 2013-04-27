/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones.
 * Copyright (c) 2007-2008 Kenneth 'Bessarion' Boyd 
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "learn.h"
#include "option.h"
#include "project.h"
#include "raceflag.h"
#include "summon.h"
#include "tvalsval.h"
#include "melee.h"


#include "flow.h"
#include "keypad.h"

/*
 * Misc constants
 */
#define BREAK_GLYPH		550		/* denominator for Rune of protection resistance */
#define MON_MULT_ADJ	8		/* High value slows multiplication */

/*
 * A monster can only "multiply" (reproduce) if there are fewer than 100
 * monsters on the level capable of such spontaneous reproduction.  This
 * is a hack which prevents the "mon_list[]" array from exploding due to
 * reproducing monsters.  Messy, but necessary.
 */
#define MAX_REPRO	100

/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeve Sward).
 *
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.  This will require
 * much less space, if I am not mistaken.  Thus, each monster gets a
 * set of 32 bit flags, "smart", build from the various "SM_*" flags.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart" field is always up to date,
 * while "smart_learn" means that the "smart" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 */




/*
 * Internal probability routine
 */
static bool int_outof(const monster_race *r_ptr, int prob)
{
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags[1] & RF1_SMART)) prob /= 2;

	/* Roll the dice */
	return (100<=prob || rand_int(100) < prob);
}

/*
 * AI assistant: diagnoses which attack types are harmless to the player 
 * TODO: fix this.
 */
bool 
player_type::harmless_projection(int dam, int typ, u32b smart)
{
	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID: return ((smart & SM_IMM_ACID) || (dam <= 0));

		/* Standard damage -- hurts inventory too */
		case GF_FIRE: return ((smart & SM_IMM_FIRE) || (dam <= 0));

		/* Standard damage -- hurts inventory too */
		case GF_COLD: return ((smart & SM_IMM_COLD) || (dam <= 0));

		/* Standard damage -- hurts inventory too */
		case GF_ELEC: return ((smart & SM_IMM_ELEC) || (dam <= 0));

		/* Standard damage -- also poisons player */
		case GF_POIS: return ((dam <= 0) && (smart & (SM_OPP_POIS | SM_RES_POIS)));

		/* Standard damage */
		case GF_MISSILE: return (dam <= 0);

		/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_ORB: return (dam <= 1);

		/* Arrow -- no dodging XXX */
		case GF_ARROW: return (dam <= 0);

		/* Plasma -- No resist XXX */
		case GF_PLASMA: return ((dam <= 0) && (smart & SM_RES_SOUND));

		/* Nether -- drain experience */
		case GF_NETHER: return ((smart & SM_RES_NETHR) && (dam <= 1));

		/* Water -- stun/confuse */
		case GF_WATER: return ((dam <= 0) && (smart & SM_RES_SOUND) && (smart & SM_RES_CONFU));

		/* Chaos -- many effects */
		case GF_CHAOS: return ((smart & SM_RES_CHAOS) && (dam <= 1));

		/* Shards -- mostly cutting */
		case GF_SHARD: return ((smart & SM_RES_SHARD) && (dam <= 1));

		/* Sound -- mostly stunning */
		case GF_SOUND: return ((smart & SM_RES_SOUND) && (dam <= 1));

		/* Pure confusion */
		case GF_CONFUSION: return ((smart & SM_RES_CONFU) && (dam <= 1));

		/* Disenchantment -- see above */
		case GF_DISENCHANT: return ((smart & SM_RES_DISEN) && (dam <= 1));

		/* Nexus -- see above */
		case GF_NEXUS: return ((smart & SM_RES_NEXUS) && (dam <= 1));

		/* Force -- mostly stun */
		case GF_FORCE: return ((smart & SM_RES_SOUND) && (dam <= 0));

		/* Inertia -- slowness */
		case GF_INERTIA: return FALSE;

		/* Lite -- blinding */
		case GF_LITE: 	if (smart & SM_RES_LITE) return (dam <= 1);
						return ((smart & SM_RES_BLIND) && (dam <= 0));

		/* Dark -- blinding */
		case GF_DARK: 	if (smart & SM_RES_DARK) return (dam <= 1);
						return ((smart & SM_RES_BLIND) && (dam <= 0));

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:	return FALSE;

		/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY: return FALSE;

		/* Pure damage */
		case GF_MANA: return (0 <= dam);

		/* Pure damage */
		case GF_METEOR: return (0 <= dam);

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE: return (((smart & SM_IMM_COLD) || (0 <= dam)) && (smart & SM_RES_SOUND) && (smart & SM_RES_SHARD));

		/* Default */
		default:	return TRUE;
	}
}

/*
 * KRB: Initialize damage for attack spells
 * Zaiband-specific.
 */
static void init_spell_damage(u32b* f_attack, range_spec* damage_spec, int rlev, int hp, u32b smart)
{
	C_WIPE(damage_spec, 32 * RACE_FLAG_SPELL_STRICT_UB);

#define _MISSILE_WEAPON_SPEC(I,N,D) if (f_attack[0] & RSF0_##I) {damage_spec[RFA_##I].range.set(N,D);}

	/* XXX rethink these, should use an ammunition and to-hit system XXX */
	/* XXX note that ARROW_3, at least, is also used to represent innate weapons */
	_MISSILE_WEAPON_SPEC(ARROW_1, 1 ,6);	/* \todo remap: short bow */
	_MISSILE_WEAPON_SPEC(ARROW_2, 3 ,6);	/* \todo remap: long bow */
	_MISSILE_WEAPON_SPEC(ARROW_3, 5 ,6);	/* \todo remap: light crossbow */
	_MISSILE_WEAPON_SPEC(ARROW_4, 7 ,6);	/* \todo remap: heavy crossbow */
	_MISSILE_WEAPON_SPEC(BOULDER, 1 + rlev / 7, 12);

#undef _MISSILE_WEAPON_SPEC

/* note that interesting bolt attacks always have enough damage to fail to be harmless */
/* should rearrange this to a series of specialized templates parametrized on GF_values to get rid of the switch */
/* need this to react to undisciplined monsters (maybe later point) */
	if (0>=hp/3)
	{	/* ahem...breath won't do damage anyway... */
		/* using immunities/resistances should require a discipline check */
		f_attack[0] &= ~(RSF0_BR_ACID | RSF0_BR_ELEC | RSF0_BR_FIRE | RSF0_BR_COLD);
	}

#define _BREATH_WEAPON_SPEC(I,DIV,LIMIT,TYPE) if (f_attack[0] & RSF0_BR_##I) {damage_spec[RFA_BR_##I].base = MIN(LIMIT, hp / DIV); if (player_type::harmless_projection(damage_spec[RFA_BR_##I].base, TYPE, smart)) {f_attack[0] &= ~(RSF0_BR_##I); damage_spec[RFA_BR_##I].base = 0;};}

	_BREATH_WEAPON_SPEC(ACID, 3, 1600, GF_ACID);
	_BREATH_WEAPON_SPEC(ELEC, 3, 1600, GF_ELEC);
	_BREATH_WEAPON_SPEC(FIRE, 3, 1600, GF_FIRE);
	_BREATH_WEAPON_SPEC(COLD, 3, 1600, GF_COLD);
	_BREATH_WEAPON_SPEC(POIS, 3, 800, GF_POIS);
	_BREATH_WEAPON_SPEC(NETH, 6, 550, GF_NETHER);
	_BREATH_WEAPON_SPEC(LITE, 6, 400, GF_LITE);
	_BREATH_WEAPON_SPEC(DARK, 6, 400, GF_DARK);
	_BREATH_WEAPON_SPEC(CONF, 6, 400, GF_CONFUSION);
	_BREATH_WEAPON_SPEC(SOUN, 6, 500, GF_SOUND);
	_BREATH_WEAPON_SPEC(CHAO, 6, 500, GF_CHAOS);
	_BREATH_WEAPON_SPEC(DISE, 6, 500, GF_DISENCHANT);
	_BREATH_WEAPON_SPEC(NEXU, 6, 400, GF_NEXUS);
	_BREATH_WEAPON_SPEC(TIME, 3, 150, GF_TIME);
	_BREATH_WEAPON_SPEC(INER, 6, 200, GF_INERTIA);
	_BREATH_WEAPON_SPEC(GRAV, 3, 200, GF_GRAVITY);
	_BREATH_WEAPON_SPEC(SHAR, 6, 500, GF_SHARD);
	_BREATH_WEAPON_SPEC(PLAS, 6, 150, GF_PLASMA);
	_BREATH_WEAPON_SPEC(WALL, 6, 200, GF_FORCE);

#undef _BREATH_WEAPON_SPEC

	if (f_attack[1] & RSF1_BA_ACID) {damage_spec[RFA_BA_ACID].set(15, 1 ,rlev * 3);};
	if (f_attack[1] & RSF1_BA_ELEC) {damage_spec[RFA_BA_ELEC].set(8, 1, rlev * 3 / 2);};
	if (f_attack[1] & RSF1_BA_FIRE) {damage_spec[RFA_BA_FIRE].set(10, 1,  rlev * 7 / 2);};
	if (f_attack[1] & RSF1_BA_COLD) {damage_spec[RFA_BA_COLD].set(10, 1,  rlev * 3 / 2);};

	if (f_attack[1] & RSF1_BA_POIS) {damage_spec[RFA_BA_POIS].range.set(12, 2);};
	if (f_attack[1] & RSF1_BA_NETH) {damage_spec[RFA_BA_NETH].set(50+rlev, 10, 10);};
	if (f_attack[1] & RSF1_BA_WATE) {damage_spec[RFA_BA_WATE].set(50, 1, rlev * 5 / 2);};
	if (f_attack[1] & RSF1_BA_MANA) {damage_spec[RFA_BA_MANA].set(rlev*5, 10, 10);};
	if (f_attack[1] & RSF1_BA_DARK) {damage_spec[RFA_BA_DARK].set(rlev*5, 10, 10);};
	if (f_attack[1] & RSF1_MIND_BLAST) {damage_spec[RFA_MIND_BLAST].range.set(8, 8);};
	if (f_attack[1] & RSF1_BRAIN_SMASH) {damage_spec[RFA_BRAIN_SMASH].range.set(12, 15);};
	if (f_attack[1] & RSF1_CAUSE_1) {damage_spec[RFA_CAUSE_1].range.set(3, 8);};
	if (f_attack[1] & RSF1_CAUSE_2) {damage_spec[RFA_CAUSE_2].range.set(8, 8);};
	if (f_attack[1] & RSF1_CAUSE_3) {damage_spec[RFA_CAUSE_3].range.set(10, 15);};
	if (f_attack[1] & RSF1_CAUSE_4) {damage_spec[RFA_CAUSE_4].range.set(15, 15);};

	if (f_attack[1] & RSF1_BO_ACID) {damage_spec[RFA_BO_ACID].set(rlev/3, 7, 8);};
	if (f_attack[1] & RSF1_BO_ELEC) {damage_spec[RFA_BO_ELEC].set(rlev/3, 4, 8);};
	if (f_attack[1] & RSF1_BO_FIRE) {damage_spec[RFA_BO_FIRE].set(rlev/3, 9, 8);};
	if (f_attack[1] & RSF1_BO_COLD) {damage_spec[RFA_BO_COLD].set(rlev/3, 6, 8);};

	if (f_attack[1] & RSF1_BO_NETH) {damage_spec[RFA_BO_NETH].set(30+ rlev*3/2, 5, 5);};
	if (f_attack[1] & RSF1_BO_WATE) {damage_spec[RFA_BO_WATE].set(rlev, 10, 10);};
	if (f_attack[1] & RSF1_BO_MANA) {damage_spec[RFA_BO_MANA].set(50, 1, rlev*7/2);};
	if (f_attack[1] & RSF1_BO_PLAS) {damage_spec[RFA_BO_PLAS].set(10+rlev, 8, 7);};
	if (f_attack[1] & RSF1_BO_ICEE) {damage_spec[RFA_BO_ICEE].set(rlev, 6, 6);};
	if (f_attack[1] & RSF1_MISSILE) {damage_spec[RFA_MISSILE].set(rlev/3, 2, 6);};
}


/*
 * Remove the "bad" spells from a spell list
 * KRB: old implementation gutted.
 */
static void remove_bad_spells(const m_idx_type m_idx, u32b *f_attack, range_spec* damage_spec)
{
	monster_type* const m_ptr = &mon_list[m_idx];
	const monster_race* const r_ptr = m_ptr->race();
	const int apparent_player_health = p_ptr->apparent_health();

	u32b f_attack2[RACE_FLAG_SPELL_STRICT_UB];
	u16b min_dam[32 * RACE_FLAG_SPELL_STRICT_UB];
	u16b median_dam[32 * RACE_FLAG_SPELL_STRICT_UB];
	u16b max_dam[32 * RACE_FLAG_SPELL_STRICT_UB];

	u32b smart = m_ptr->smart;
	size_t i;


	/* Too stupid to know anything */
	if (r_ptr->flags[1] & RF1_STUPID) return;

	/* Must be cheating or learning */
	if (!OPTION(adult_smart_cheat) && !OPTION(adult_smart_learn)) return;

	/* if cheating, know about perfect save */
	if (OPTION(adult_smart_cheat) && 100<=p_ptr->skills[SKILL_SAVE] && int_outof(r_ptr, 100))
	{
		f_attack[1] &= ~(RSF1_MIND_BLAST | RSF1_BRAIN_SMASH | RSF1_CAUSE_1 | RSF1_CAUSE_2 | RSF1_CAUSE_3 | RSF1_CAUSE_4 | RSF1_SCARE | RSF1_BLIND | RSF1_CONF | RSF1_SLOW | RSF1_HOLD);
		f_attack[2] &= ~(RSF2_TELE_LEVEL | RSF2_FORGET);
	}

	/* Nothing known */
	if (!smart) return;

	/* initialize f_attack2 mirror of f_attack, etc. */
	C_COPY(f_attack2, f_attack, RACE_FLAG_SPELL_STRICT_UB);

	for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; ++i)
	{
		min_dam[i] = damage_spec[i].minroll();
		median_dam[i] = damage_spec[i].medianroll();
		max_dam[i] = damage_spec[i].maxroll();
	}

	/* assume attacks that can be nullified by a save have minimum damage 0 */
	/* if cheating, check that save is actually possible first */
	if (!OPTION(adult_smart_cheat) || 0<=p_ptr->skills[SKILL_SAVE])
	{
		min_dam[RFA_MIND_BLAST] = 0;
		min_dam[RFA_BRAIN_SMASH] = 0;
		min_dam[RFA_CAUSE_1] = 0;
		min_dam[RFA_CAUSE_2] = 0;
		min_dam[RFA_CAUSE_3] = 0;
		min_dam[RFA_CAUSE_4] = 0;
	}

	if ((smart & (SM_IMM_ACID)) && int_outof(r_ptr, 100))
	{
		f_attack2[0] &= ~RSF0_BR_ACID;
		f_attack2[1] &= ~(RSF1_BA_ACID | RSF1_BO_ACID);
	};

	if ((f_attack2[0] & RSF0_BR_ACID) || (f_attack2[1] & (RSF1_BA_ACID | RSF1_BO_ACID)))
	{
		/* Zaiband: new damage-modeling code */
		if ((smart & (SM_OPP_ACID)) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_ACID] = DR<3>(min_dam[RFA_BR_ACID]);
			median_dam[RFA_BR_ACID] = DR<3>(median_dam[RFA_BR_ACID]);
			max_dam[RFA_BR_ACID] = DR<3>(max_dam[RFA_BR_ACID]);
			min_dam[RFA_BA_ACID] = DR<3>(min_dam[RFA_BA_ACID]);
			median_dam[RFA_BA_ACID] = DR<3>(median_dam[RFA_BA_ACID]);
			max_dam[RFA_BA_ACID] = DR<3>(max_dam[RFA_BA_ACID]);
			min_dam[RFA_BO_ACID] = DR<3>(min_dam[RFA_BO_ACID]);
			median_dam[RFA_BO_ACID] = DR<3>(median_dam[RFA_BO_ACID]);
			max_dam[RFA_BO_ACID] = DR<3>(max_dam[RFA_BO_ACID]);
		};
		if ((smart & (SM_RES_ACID)) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_ACID] = DR<3>(min_dam[RFA_BR_ACID]);
			median_dam[RFA_BR_ACID] = DR<3>(median_dam[RFA_BR_ACID]);
			max_dam[RFA_BR_ACID] = DR<3>(max_dam[RFA_BR_ACID]);
			min_dam[RFA_BA_ACID] = DR<3>(min_dam[RFA_BA_ACID]);
			median_dam[RFA_BA_ACID] = DR<3>(median_dam[RFA_BA_ACID]);
			max_dam[RFA_BA_ACID] = DR<3>(max_dam[RFA_BA_ACID]);
			min_dam[RFA_BO_ACID] = DR<3>(min_dam[RFA_BO_ACID]);
			median_dam[RFA_BO_ACID] = DR<3>(median_dam[RFA_BO_ACID]);
			max_dam[RFA_BO_ACID] = DR<3>(max_dam[RFA_BO_ACID]);
		};
	};

	if ((smart & (SM_IMM_ELEC)) && int_outof(r_ptr, 100))
	{
		f_attack2[0] &= ~RSF0_BR_ELEC;
		f_attack2[1] &= ~(RSF1_BA_ELEC | RSF1_BO_ELEC);
	};

	if ((f_attack2[0] & RSF0_BR_ELEC) || (f_attack2[1] & (RSF1_BA_ELEC | RSF1_BO_ELEC)))
	{
		if ((smart & (SM_OPP_ELEC)) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_ELEC] = DR<3>(min_dam[RFA_BR_ELEC]);
			median_dam[RFA_BR_ELEC] = DR<3>(median_dam[RFA_BR_ELEC]);
			max_dam[RFA_BR_ELEC] = DR<3>(max_dam[RFA_BR_ELEC]);
			min_dam[RFA_BA_ELEC] = DR<3>(min_dam[RFA_BA_ELEC]);
			median_dam[RFA_BA_ELEC] = DR<3>(median_dam[RFA_BA_ELEC]);
			max_dam[RFA_BA_ELEC] = DR<3>(max_dam[RFA_BA_ELEC]);
			min_dam[RFA_BO_ELEC] = DR<3>(min_dam[RFA_BO_ELEC]);
			median_dam[RFA_BO_ELEC] = DR<3>(median_dam[RFA_BO_ELEC]);
			max_dam[RFA_BO_ELEC] = DR<3>(max_dam[RFA_BO_ELEC]);
		};
		if ((smart & (SM_RES_ELEC)) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_ELEC] = DR<3>(min_dam[RFA_BR_ELEC]);
			median_dam[RFA_BR_ELEC] = DR<3>(median_dam[RFA_BR_ELEC]);
			max_dam[RFA_BR_ELEC] = DR<3>(max_dam[RFA_BR_ELEC]);
			min_dam[RFA_BA_ELEC] = DR<3>(min_dam[RFA_BA_ELEC]);
			median_dam[RFA_BA_ELEC] = DR<3>(median_dam[RFA_BA_ELEC]);
			max_dam[RFA_BA_ELEC] = DR<3>(max_dam[RFA_BA_ELEC]);
			min_dam[RFA_BO_ELEC] = DR<3>(min_dam[RFA_BO_ELEC]);
			median_dam[RFA_BO_ELEC] = DR<3>(median_dam[RFA_BO_ELEC]);
			max_dam[RFA_BO_ELEC] = DR<3>(max_dam[RFA_BO_ELEC]);
		};
	};

	if ((smart & (SM_IMM_FIRE)) && int_outof(r_ptr, 100))
	{
		f_attack2[0] &= ~RSF0_BR_FIRE;
		f_attack2[1] &= ~(RSF1_BA_FIRE | RSF1_BO_FIRE);
	}

	if ((f_attack2[0] & RSF0_BR_FIRE) || (f_attack2[1] & (RSF1_BA_FIRE | RSF1_BO_FIRE)))
	{
		if (smart & (SM_OPP_FIRE) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_FIRE] = DR<3>(min_dam[RFA_BR_FIRE]);
			median_dam[RFA_BR_FIRE] = DR<3>(median_dam[RFA_BR_FIRE]);
			max_dam[RFA_BR_FIRE] = DR<3>(max_dam[RFA_BR_FIRE]);
			min_dam[RFA_BA_FIRE] = DR<3>(min_dam[RFA_BA_FIRE]);
			median_dam[RFA_BA_FIRE] = DR<3>(median_dam[RFA_BA_FIRE]);
			max_dam[RFA_BA_FIRE] = DR<3>(max_dam[RFA_BA_FIRE]);
			min_dam[RFA_BO_FIRE] = DR<3>(min_dam[RFA_BO_FIRE]);
			median_dam[RFA_BO_FIRE] = DR<3>(median_dam[RFA_BO_FIRE]);
			max_dam[RFA_BO_FIRE] = DR<3>(max_dam[RFA_BO_FIRE]);
		};
		if (smart & (SM_RES_FIRE) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_FIRE] = DR<3>(min_dam[RFA_BR_FIRE]);
			median_dam[RFA_BR_FIRE] = DR<3>(median_dam[RFA_BR_FIRE]);
			max_dam[RFA_BR_FIRE] = DR<3>(max_dam[RFA_BR_FIRE]);
			min_dam[RFA_BA_FIRE] = DR<3>(min_dam[RFA_BA_FIRE]);
			median_dam[RFA_BA_FIRE] = DR<3>(median_dam[RFA_BA_FIRE]);
			max_dam[RFA_BA_FIRE] = DR<3>(max_dam[RFA_BA_FIRE]);
			min_dam[RFA_BO_FIRE] = DR<3>(min_dam[RFA_BO_FIRE]);
			median_dam[RFA_BO_FIRE] = DR<3>(median_dam[RFA_BO_FIRE]);
			max_dam[RFA_BO_FIRE] = DR<3>(max_dam[RFA_BO_FIRE]);
		};
	};

	if ((smart & (SM_IMM_COLD)) && int_outof(r_ptr, 100))
	{
		f_attack2[0] &= ~RSF0_BR_COLD;
		f_attack2[1] &= ~(RSF1_BA_COLD | RSF1_BO_COLD | RSF1_BO_ICEE);
	};

	if ((f_attack2[0] & RSF0_BR_COLD) || (f_attack2[1] & (RSF1_BA_COLD | RSF1_BO_COLD | RSF1_BO_ICEE)))
	{
		if (smart & (SM_OPP_COLD) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_COLD] = DR<3>(min_dam[RFA_BR_COLD]);
			median_dam[RFA_BR_COLD] = DR<3>(median_dam[RFA_BR_COLD]);
			max_dam[RFA_BR_COLD] = DR<3>(max_dam[RFA_BR_COLD]);
			min_dam[RFA_BA_COLD] = DR<3>(min_dam[RFA_BA_COLD]);
			median_dam[RFA_BA_COLD] = DR<3>(median_dam[RFA_BA_COLD]);
			max_dam[RFA_BA_COLD] = DR<3>(max_dam[RFA_BA_COLD]);
			min_dam[RFA_BO_COLD] = DR<3>(min_dam[RFA_BO_COLD]);
			median_dam[RFA_BO_COLD] = DR<3>(median_dam[RFA_BO_COLD]);
			max_dam[RFA_BO_COLD] = DR<3>(max_dam[RFA_BO_COLD]);
			min_dam[RFA_BO_ICEE] = DR<3>(min_dam[RFA_BO_ICEE]);
			median_dam[RFA_BO_ICEE] = DR<3>(median_dam[RFA_BO_ICEE]);
			max_dam[RFA_BO_ICEE] = DR<3>(max_dam[RFA_BO_ICEE]);
		};
		if (smart & (SM_RES_COLD) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_COLD] = DR<3>(min_dam[RFA_BR_COLD]);
			median_dam[RFA_BR_COLD] = DR<3>(median_dam[RFA_BR_COLD]);
			max_dam[RFA_BR_COLD] = DR<3>(max_dam[RFA_BR_COLD]);
			min_dam[RFA_BA_COLD] = DR<3>(min_dam[RFA_BA_COLD]);
			median_dam[RFA_BA_COLD] = DR<3>(median_dam[RFA_BA_COLD]);
			max_dam[RFA_BA_COLD] = DR<3>(max_dam[RFA_BA_COLD]);
			min_dam[RFA_BO_COLD] = DR<3>(min_dam[RFA_BO_COLD]);
			median_dam[RFA_BO_COLD] = DR<3>(median_dam[RFA_BO_COLD]);
			max_dam[RFA_BO_COLD] = DR<3>(max_dam[RFA_BO_COLD]);
			min_dam[RFA_BO_ICEE] = DR<3>(min_dam[RFA_BO_ICEE]);
			median_dam[RFA_BO_ICEE] = DR<3>(median_dam[RFA_BO_ICEE]);
			max_dam[RFA_BO_ICEE] = DR<3>(max_dam[RFA_BO_ICEE]);
		};
	};

	if ((f_attack2[0] & RSF0_BR_POIS) || (f_attack2[1] & RSF1_BA_POIS))
	{
		if (smart & (SM_OPP_POIS) && int_outof(r_ptr, 100))
		{
			if (0==max_dam[RFA_BR_POIS])
			{
				f_attack2[0] &= ~(RSF0_BR_POIS);
			}
			else
			{
				min_dam[RFA_BR_POIS] = DR<3>(min_dam[RFA_BR_POIS]);
				median_dam[RFA_BR_POIS] = DR<3>(median_dam[RFA_BR_POIS]);
				max_dam[RFA_BR_POIS] = DR<3>(max_dam[RFA_BR_POIS]);
			}

			min_dam[RFA_BA_POIS] = DR<3>(min_dam[RFA_BA_POIS]);
			median_dam[RFA_BA_POIS] = DR<3>(median_dam[RFA_BA_POIS]);
			max_dam[RFA_BA_POIS] = DR<3>(max_dam[RFA_BA_POIS]);
		};
		if (smart & (SM_RES_POIS) && int_outof(r_ptr, 100))
		{
			if (0==max_dam[RFA_BR_POIS])
			{
				f_attack2[0] &= ~RSF0_BR_POIS;
			}
			else
			{
				min_dam[RFA_BR_POIS] = DR<3>(min_dam[RFA_BR_POIS]);
				median_dam[RFA_BR_POIS] = DR<3>(median_dam[RFA_BR_POIS]);
				max_dam[RFA_BR_POIS] = DR<3>(max_dam[RFA_BR_POIS]);
			}

			min_dam[RFA_BA_POIS] = DR<3>(min_dam[RFA_BA_POIS]);
			median_dam[RFA_BA_POIS] = DR<3>(median_dam[RFA_BA_POIS]);
			max_dam[RFA_BA_POIS] = DR<3>(max_dam[RFA_BA_POIS]);
		};
	};

	if (smart & (SM_RES_LITE))
	{
		if ((f_attack2[0] & RSF0_BR_LITE) && int_outof(r_ptr, 100))
		{
			min_dam[RFA_BR_LITE] /= 3;
			median_dam[RFA_BR_LITE] = 4 * median_dam[RFA_BR_LITE] / 10;	/* XXX */
			max_dam[RFA_BR_LITE] = 4 * max_dam[RFA_BR_LITE] / 7;
			if (0==max_dam[RFA_BR_LITE]) f_attack2[0] &= ~RSF0_BR_LITE;
		}
	}

	if (smart & (SM_RES_DARK))
	{
		if (((f_attack2[0] & RSF0_BR_DARK) || (f_attack2[1] & RSF1_BA_DARK)) && int_outof(r_ptr, 100))
		{
			if (f_attack2[0] & RSF0_BR_DARK)
			{
				min_dam[RFA_BR_DARK] /= 3;
				median_dam[RFA_BR_DARK] = 4 * median_dam[RFA_BR_DARK] / 10;	/* XXX */
				max_dam[RFA_BR_DARK] = 4 * max_dam[RFA_BR_DARK] / 7;
			}

			if (f_attack2[1] & RSF1_BA_DARK)
			{
				min_dam[RFA_BA_DARK] /= 3;
				median_dam[RFA_BA_DARK] = 4 * median_dam[RFA_BA_DARK] / 10;	/* XXX */
				max_dam[RFA_BA_DARK] = 4 * max_dam[RFA_BA_DARK] / 7;
			}
			if (0==max_dam[RFA_BR_DARK]) f_attack2[0] &= ~RSF0_BR_DARK;
			if (0==max_dam[RFA_BA_DARK]) f_attack2[1] &= ~RSF1_BA_DARK;
		}
	}

	/* note that resist blindness also makes 0hp light/dark attacks moot */
	if (smart & (SM_RES_BLIND) && ((f_attack2[0] & (RSF0_BR_LITE | RSF0_BR_DARK)) || (f_attack2[1] & RSF1_BA_DARK)) && int_outof(r_ptr, 100))
	{
		if (0==max_dam[RFA_BR_LITE]) f_attack2[0] &= ~RSF0_BR_LITE;
		if (0==max_dam[RFA_BR_DARK]) f_attack2[0] &= ~RSF0_BR_DARK;
		if (0==max_dam[RFA_BA_DARK]) f_attack2[1] &= ~RSF1_BA_DARK;
	}

	if (smart & (SM_RES_CONFU) && (f_attack2[0] & RSF0_BR_CONF) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_CONF] = 5 * min_dam[RFA_BR_CONF] / 12;
		median_dam[RFA_BR_CONF] = 5 * median_dam[RFA_BR_CONF] / 10;	/* XXX */
		max_dam[RFA_BR_CONF] = 5 * max_dam[RFA_BR_CONF] / 7;
		if (0==max_dam[RFA_BR_CONF]) f_attack2[0] &= ~RSF0_BR_CONF;
	}

	if (smart & (SM_RES_SOUND) && (f_attack2[0] & RSF0_BR_SOUN) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_SOUN] = 5 * min_dam[RFA_BR_SOUN] / 12;
		median_dam[RFA_BR_SOUN] = 5 * median_dam[RFA_BR_SOUN] / 10;	/* XXX */
		max_dam[RFA_BR_SOUN] = 5 * max_dam[RFA_BR_SOUN] / 7;
		if (0==max_dam[RFA_BR_SOUN]) f_attack2[0] &= ~RSF0_BR_SOUN;
	}

	if (smart & (SM_RES_SHARD) && (f_attack2[0] & RSF0_BR_SHAR) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_SHAR] /= 2;
		median_dam[RFA_BR_SHAR] = 6 * median_dam[RFA_BR_SHAR] / 10;	/* XXX */
		max_dam[RFA_BR_SHAR] = 6 * max_dam[RFA_BR_SHAR] / 7;
		if (0==max_dam[RFA_BR_SHAR]) f_attack2[0] &= ~RSF0_BR_SHAR;
	}

	if (smart & (SM_RES_NEXUS) && (f_attack2[0] & RSF0_BR_NEXU) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_NEXU] /= 2;
		median_dam[RFA_BR_NEXU] = 6 * median_dam[RFA_BR_NEXU] / 10;	/* XXX */
		max_dam[RFA_BR_NEXU] = 6 * max_dam[RFA_BR_NEXU] / 7;
		if (0==max_dam[RFA_BR_NEXU]) f_attack2[0] &= ~RSF0_BR_NEXU;
	}

	if (smart & (SM_RES_NETHR) && ((f_attack2[0] & RSF0_BR_NETH) || (f_attack2[1] & (RSF1_BA_NETH | RSF1_BO_NETH))) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_NETH] /= 2;
		median_dam[RFA_BR_NETH] = 6 * median_dam[RFA_BR_NETH] / 10;	/* XXX */
		max_dam[RFA_BR_NETH] = 6 * max_dam[RFA_BR_NETH] / 7;
		if (0==max_dam[RFA_BR_NETH]) f_attack2[0] &= ~RSF0_BR_NETH;
		min_dam[RFA_BA_NETH] /= 2;
		median_dam[RFA_BA_NETH] = 6 * median_dam[RFA_BA_NETH] / 10;	/* XXX */
		max_dam[RFA_BA_NETH] = 6 * max_dam[RFA_BA_NETH] / 7;
		min_dam[RFA_BO_NETH] /= 2;
		median_dam[RFA_BO_NETH] = 6 * median_dam[RFA_BO_NETH] / 10;	/* XXX */
		max_dam[RFA_BO_NETH] = 6 * max_dam[RFA_BO_NETH] / 7;
	}

	if (smart & (SM_RES_CHAOS) && (f_attack2[0] & RSF0_BR_CHAO) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_CHAO] /= 2;
		median_dam[RFA_BR_CHAO] = 6 * median_dam[RFA_BR_CHAO] / 10;	/* XXX */
		max_dam[RFA_BR_CHAO] = 6 * max_dam[RFA_BR_CHAO] / 7;
		if (0==max_dam[RFA_BR_CHAO]) f_attack2[0] &= ~RSF0_BR_CHAO;
	}

	if (smart & (SM_RES_DISEN) && (f_attack2[0] & RSF0_BR_DISE) && int_outof(r_ptr, 100))
	{
		min_dam[RFA_BR_DISE] /= 2;
		median_dam[RFA_BR_DISE] = 6 * median_dam[RFA_BR_DISE] / 10;	/* XXX */
		max_dam[RFA_BR_DISE] = 6 * max_dam[RFA_BR_DISE] / 7;
		if (0==max_dam[RFA_BR_DISE]) f_attack2[0] &= ~RSF0_BR_DISE;
	}

	/* damage filters */
	/* acid, fire, cold, electricity are always pure damage */
	/* poison, light, dark, confusion, sound, shard, nexus, nether, chaos, disenchant, water, force are iffy as to whether they're pure damage */
	/* breath weapons are flat-damage, others are randomized somewhat */

	{	/* C-ish blocking brace */
	bool for_the_win = FALSE;
	for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; ++i)
		if ((int)min_dam[i]>apparent_player_health)
		{
			for_the_win = TRUE;
			break;
		}
	if (for_the_win)
	{
		for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; ++i)
			if ((int)min_dam[i]<=apparent_player_health)
				RESET_FLAG(f_attack2,i);
	}
	else
	{
		for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; ++i)
			if ((int)max_dam[i]>apparent_player_health)
			{
				for_the_win = TRUE;
				break;
			}
		if (for_the_win)
		{
			for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; ++i)
				if ((int)max_dam[i]<=apparent_player_health)
					RESET_FLAG(f_attack2,i);
		}
	}
	}	/* end C-ish blocking brace */

/* A has a noxious qualitiative effect that prevents lower damage from being dominated */
#define _TERSE_COMPARE_SPELL_DAM(A,B)	\
	if (min_dam[RFA_##A]>max_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##B);	\
	else if (median_dam[RFA_##A]>median_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##B)

#define _COMPARE_SPELL_DAM(A,B)	\
	if 		(min_dam[RFA_##A]>max_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##B);	\
	else if (max_dam[RFA_##A]<min_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##A);	\
	else if (median_dam[RFA_##A]>median_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##B)	\
	else if (median_dam[RFA_##A]<median_dam[RFA_##B])	\
		RESET_FLAG(f_attack2,RFA_##A)

	if (f_attack2[1] & RSF1_MISSILE)
	{	/* compare median damage against cause_1...cause_4 */
#define _MISSILE_VS(A)	\
		if (f_attack2[1] & RSF1_##A)	\
		{	\
			if ((long)median_dam[RFA_MISSILE]*100<(long)median_dam[RFA_##A]*p_ptr->skills[SKILL_SAVE])	\
				RESET_FLAG(f_attack2,RFA_MISSILE);	\
			else	\
				RESET_FLAG(f_attack2,RFA_##A);	\
		}

		/* if cheating, use player's save in arbitrating magic missile vs cause wounds */
		if (OPTION(adult_smart_cheat))
		{
			_MISSILE_VS(CAUSE_1);
			_MISSILE_VS(CAUSE_2);
			_MISSILE_VS(CAUSE_3);
			_MISSILE_VS(CAUSE_4);
		}

		/* following can be pure damage */
#if 0
		if ((f_attack2[1] & RSF1_BA_POIS) && (smart & (SM_OPP_POIS | SM_RES_POIS))) _MISSILE_VS(BA_POIS);
		if ((f_attack2[1] & RSF1_BA_NETH) && (smart & SM_RES_NETHR)) _MISSILE_VS(BA_NETH);
		if ((f_attack2[1] & RSF1_BA_WATE) && (smart & SM_RES_SOUND) && (smart & SM_RES_CONFU)) _MISSILE_VS(BA_WATE);
		if (f_attack2[1] & RSF1_BA_MANA) _MISSILE_VS(BA_MANA);
		if ((f_attack2[1] & RSF1_BA_DARK) && (smart & (SM_RES_BLIND | SM_RES_DARK))) _MISSILE_VS(BA_DARK);
		if ((f_attack2[1] & RSF1_BO_NETH) && (smart & SM_RES_NETHR)) _MISSILE_VS(BO_NETH);
		if ((f_attack2[1] & RSF1_BO_WATE) && (smart & SM_RES_SOUND) && (smart & SM_RES_CONFU)) _MISSILE_VS(BO_WATE);
		if (f_attack2[1] & RSF1_BO_MANA) _MISSILE_VS(BO_MANA);
		if ((f_attack2[1] & RSF1_BO_PLAS) && )
		if ((f_attack2[1] & RSF1_BO_ICEE) && (smart & SM_RES_SOUND) && (smart & SM_RES_SHARD)) _MISSILE_VS(BO_ICEE);
#endif

#undef _MISSILE_VS
	}

/* What we actually have to handle
S:BLIND | CONF | MISSILE | DARKNESS | BA_POIS

S:BO_FIRE | SLOW | HOLD | CAUSE_3 | MISSILE


S:ARROW_1 | MISSILE (eh...as-is, MISSILE dominates ARROW_1; won't be true after rewrite)

S:HEAL | SLOW | TRAPS | BO_COLD | BA_POIS

S:CONF | SCARE
S:BR_LITE | BR_DARK

S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS

S:S_ANIMAL
S:HASTE | BLINK | BLIND | HOLD | SLOW | BO_FIRE | BO_ELEC

S:BLINK | CAUSE_2 | BO_COLD | BA_POIS

S:HEAL | BLIND | CONF | CAUSE_2 | MIND_BLAST

S:BLIND | CONF | SCARE | CAUSE_2 | BO_COLD

S:HEAL | SCARE | BO_ACID | BA_ACID

S:HASTE | TPORT | TELE_TO | BLIND | CONF
S:BO_FIRE | BO_COLD | BO_ELEC
S:S_MONSTER

S:BLIND | HOLD | SCARE | MIND_BLAST | BRAIN_SMASH | FORGET

S:BLIND | CONF | SCARE | CAUSE_2 | FORGET
S:BO_ACID | BO_FIRE | BO_COLD | BO_ELEC
S:S_MONSTER

S:TPORT | HOLD | SCARE | CAUSE_2 | TRAPS | BO_FIRE
S:S_MONSTER

S:HEAL | BLIND | SLOW | CONF | SCARE
S:CAUSE_3 | CAUSE_4 | BR_DARK | BR_POIS
S:TRAPS | S_KIN

S:1_IN_8
S:BOULDER | BLINK | TELE_TO | CONF | SCARE | BO_ELEC | BA_ELEC

S:SLOW | CONF | SCARE
S:BR_DISE | BR_CHAO

S:SLOW | CONF | SCARE
S:BR_SOUN | BR_SHAR

S:SLOW | CONF | SCARE
S:BR_SOUN | BR_SHAR | BR_DISE | BR_CHAO

S:SLOW | CONF | SCARE
S:BR_LITE | BR_DARK

S:HEAL | HASTE | BLIND | SCARE | MIND_BLAST | BO_FIRE
S:S_MONSTERS

S:BLINK | TELE_TO | TELE_AWAY | BLIND | HOLD | SLOW | SCARE
S:CAUSE_3 | DRAIN_MANA | BRAIN_SMASH

S:TELE_TO | HOLD | CONF | SCARE | CAUSE_3 | MIND_BLAST | FORGET
S:DARKNESS | BO_NETH

S:BO_ACID | BA_ACID

S:BO_FIRE | BA_FIRE
S:S_DEMON

S:BO_ICEE | BA_COLD

S:HASTE | TPORT | TELE_TO | BLIND | HOLD | SCARE | CAUSE_3
S:BO_NETH
S:S_UNDEAD

S:BO_PLAS | BA_FIRE

S:BLIND | SCARE | CAUSE_3 | BO_NETH
S:S_MONSTERS

S:HEAL | TRAPS | BO_FIRE | BO_COLD | BO_ELEC | BO_ICEE

S:BLIND | SLOW | CONF | SCARE | DRAIN_MANA | MIND_BLAST
S:FORGET | DARKNESS | BO_ACID | BO_FIRE | BO_COLD | BO_ELEC

S:HOLD | SCARE | CAUSE_3 | BO_NETH

S:HEAL | HASTE | TELE_AWAY | CONF | BO_MANA | BO_PLAS
S:S_MONSTERS | S_ANGEL

S:BLIND | SCARE | CAUSE_3 | MIND_BLAST | DARKNESS | BO_NETH

S:BO_ICEE | BO_WATE | BA_COLD | BA_WATE

S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_SOUN | BR_CONF
S:BR_SHAR | BR_GRAV | BR_NEXU

S:SCARE | BA_POIS
S:BR_POIS

S:BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | DRAIN_MANA
S:BRAIN_SMASH | DARKNESS | BO_NETH

S:SCARE | BO_FIRE
S:BR_FIRE

S:HEAL | CAUSE_3 | CAUSE_4 | HASTE | SCARE | BLIND | S_ANGEL

S:BLINK | TELE_TO | BLIND | CONF | CAUSE_3 | TRAPS
S:BO_ACID | BA_FIRE | BA_COLD
S:S_MONSTER | S_UNDEAD | S_DRAGON

S:HOLD | SCARE | CAUSE_3 | BO_FIRE | BO_PLAS | BA_ACID
S:S_HYDRA | S_KIN

S:BLIND | HOLD | SCARE | CAUSE_3 | FORGET
S:BO_ACID | BO_FIRE | BO_COLD | BO_NETH
S:S_MONSTER

S:HEAL | BLINK | TELE_TO | BLIND | CONF | CAUSE_3 | DARKNESS
S:BO_ACID | BA_FIRE | BA_COLD
S:S_MONSTER | S_UNDEAD | S_DEMON

S:BLINK | TELE_TO | BLIND | HOLD | CONF | SCARE | CAUSE_3 | CAUSE_4
S:DRAIN_MANA | BRAIN_SMASH
S:S_UNDEAD

S:BLIND | HOLD | SCARE | CAUSE_3 | DARKNESS
S:BO_FIRE | BO_COLD | BO_NETH
S:S_MONSTERS

S:CAUSE_3 | BO_WATE | BO_MANA

S:TELE_TO | BLIND | SCARE | CAUSE_2 | CAUSE_4 | BO_MANA
S:S_ANGEL

S:BLIND | HOLD | SCARE | CAUSE_3 | BO_FIRE | BO_NETH | BA_FIRE
S:S_MONSTER

S:SLOW | CONF | CAUSE_4 | DRAIN_MANA | MIND_BLAST | FORGET
S:BO_MANA | BO_NETH | BRAIN_SMASH | BA_FIRE | BA_COLD | BO_ACID
S:S_UNDEAD

S:BLIND | CONF
S:BR_LITE | BR_DARK | BR_CONF

S:BLIND | HOLD | SCARE | CAUSE_3
S:BA_FIRE | BA_NETH | BA_COLD | BA_ELEC | BA_ACID
S:S_UNDEAD

S:BO_ACID | BA_ACID

S:BO_ELEC | BA_COLD | BA_ELEC

S:SCARE | BO_FIRE | BO_PLAS | BA_FIRE
S:BR_FIRE

S:HEAL | BLIND | HOLD | CAUSE_3 | CAUSE_4
S:S_MONSTER | S_UNDEAD

S:TELE_LEVEL | BLIND | HOLD | CONF | CAUSE_4 | DRAIN_MANA | BO_NETH
S:S_UNDEAD

S:BLIND | SLOW | CONF | ARROW_3
S:BR_POIS

S:CONF | CAUSE_3
S:BR_COLD

S:BLIND | HOLD | SCARE | CAUSE_3 | BA_FIRE | BA_NETH
S:S_MONSTERS | S_UNDEAD | S_HOUND

S:CONF | CAUSE_3
S:BR_FIRE

S:CONF | SCARE
S:BR_COLD | BR_NETH

S:HOLD | SCARE
S:BR_FIRE | BR_NEXU

S:CONF | CAUSE_3
S:BR_FIRE

S:BLIND | CONF | SCARE
S:BR_FIRE | BR_POIS
S:S_HI_DRAGON

S:BLIND | SCARE | CAUSE_4 | BRAIN_SMASH
S:BO_MANA | BO_NETH | BA_NETH | S_UNDEAD

S:SLOW | ARROW_4 | BO_MANA | BO_PLAS | BA_ELEC
S:BR_WALL

S:BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | MIND_BLAST
S:BO_COLD | BA_COLD | BA_NETH
S:S_UNDEAD

S:TELE_LEVEL | BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | BO_MANA
S:BA_FIRE | BA_COLD | BA_NETH | S_HI_UNDEAD

S:BO_FIRE | BO_PLAS | BA_FIRE
S:BR_FIRE | BR_LITE | BR_PLAS | S_KIN

S:BLIND | SCARE | BRAIN_SMASH
S:BO_MANA | BO_NETH | BA_NETH
S:BR_NETH | S_UNDEAD

S:TELE_AWAY | BLIND | CONF | SCARE | CAUSE_3 | FORGET | DARKNESS
S:BO_FIRE | BO_ACID | BO_COLD | BO_ELEC

S:BO_MANA | BO_NETH | BA_NETH | BRAIN_SMASH
S:S_UNDEAD

S:SLOW | CAUSE_4 | MIND_BLAST | BRAIN_SMASH | TRAPS | BO_PLAS
S:BO_NETH | BA_WATE
S:S_UNDEAD

S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | BR_LITE
S:BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | BR_NETH
S:BR_WALL | BR_INER | BR_TIME | BR_GRAV | BR_PLAS | BR_NEXU

S:SCARE
S:BO_FIRE | BO_PLAS | BA_FIRE | BA_POIS
S:BR_FIRE | BR_POIS | S_HYDRA | S_KIN

S:BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | DRAIN_MANA
S:BRAIN_SMASH | BA_NETH | S_KIN | S_HI_UNDEAD

S:HEAL | BLIND | CONF | SCARE | CAUSE_3 | CAUSE_4 | BRAIN_SMASH
S:FORGET | S_MONSTERS | S_ANIMAL

S:TELE_TO | BLIND
S:BO_FIRE | BO_MANA | BO_PLAS | BA_FIRE
S:BR_FIRE | BR_PLAS
S:S_ANGEL

S:TELE_TO | BLIND
S:BO_MANA | BO_NETH | BA_NETH
S:BR_NETH | S_ANGEL

S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS
S:S_HI_DRAGON | S_KIN

S:BLIND | SCARE | BRAIN_SMASH
S:BO_MANA | BO_NETH | BA_NETH
S:S_UNDEAD

S:HEAL | HASTE | TPORT | TELE_AWAY | BLIND | MIND_BLAST | SCARE
S:CAUSE_4 | FORGET | TRAPS
S:BO_ICEE | BA_ACID | BA_FIRE | BA_COLD | BA_WATE
S:S_UNDEAD | S_DEMON | S_HI_DRAGON | S_MONSTERS | S_ANIMAL

S:BLIND | HOLD | CONF | DRAIN_MANA | BA_NETH
S:S_UNDEAD | TELE_TO | TPORT | BLINK | BRAIN_SMASH

S:CAUSE_4
S:BR_CHAO

S:BLIND | CONF | SCARE
S:BR_CHAO | BR_DISE
S:S_DRAGON | S_HI_DRAGON

S:BLIND | CONF | SCARE
S:BR_SOUN | BR_SHAR
S:S_DRAGON | S_HI_DRAGON

S:BLIND | CONF | SCARE
S:BR_SOUN | BR_CHAO | BR_SHAR | BR_DISE
S:S_DRAGON | S_HI_DRAGON

S:BLIND | HOLD | CONF
S:BA_DARK | BA_NETH
S:S_WRAITH | S_HI_UNDEAD | S_KIN

S:BR_POIS | BR_ELEC | BR_ACID | BR_FIRE | BR_COLD
S:CONF | SCARE | BLIND
S:S_DRAGON | S_HI_DRAGON

S:TELE_TO | BLIND | HOLD | CONF | CAUSE_3 | CAUSE_4 | DRAIN_MANA
S:BRAIN_SMASH | BA_MANA | BA_NETH
S:S_UNDEAD

S:BLINK | TELE_TO | BLIND | HOLD | SCARE | CAUSE_4
S:BRAIN_SMASH | TRAPS | BA_MANA
S:BO_MANA | BA_NETH | S_KIN | S_MONSTERS | S_UNDEAD

S:BLIND | CONF | FORGET | SCARE | DRAIN_MANA | BRAIN_SMASH
S:BA_DARK | BO_MANA | BA_NETH | BA_ACID | BA_FIRE | BA_COLD
S:S_KIN

S:HEAL | BLIND | SLOW | CONF | SCARE | DARKNESS | BA_DARK
S:BR_POIS | BR_DARK | S_KIN

S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME
S:BR_GRAV | BR_PLAS | BR_NEXU

S:TELE_TO | HOLD | CAUSE_3 | TRAPS
S:BO_PLAS | BA_DARK | BA_MANA | BA_FIRE | BA_WATE | BA_NETH
S:S_HI_DEMON | S_HI_UNDEAD

S:TELE_AWAY | BLIND | HOLD | SCARE | CAUSE_3
S:BO_MANA | BA_NETH | BRAIN_SMASH
S:S_MONSTERS | S_WRAITH | S_HI_UNDEAD | S_HI_DRAGON | S_HI_DEMON | S_KIN

S:MIND_BLAST | BO_ELEC | BO_MANA | BA_ELEC | S_HI_DEMON

S:TELE_TO | SLOW | SCARE | CAUSE_4 | BRAIN_SMASH
S:BO_ICEE | BO_MANA | BA_WATE | BA_NETH
S:S_HI_UNDEAD

S:BR_FIRE | BR_COLD | BR_DISE

S:BLIND | CONF | SCARE
S:BR_FIRE | BR_PLAS
S:S_HI_DEMON | S_UNDEAD

S:TPORT | BLIND | SCARE | CAUSE_4 | BRAIN_SMASH
S:BA_MANA | BO_MANA | BA_FIRE | BA_NETH
S:S_MONSTERS | S_HI_DEMON | S_HI_UNDEAD | S_KIN

S:BR_COLD | BR_SHAR | BR_SOUN | BR_LITE

S:BR_DARK | BR_POIS
S:BR_FIRE | BR_NETH
S:S_HOUND

S:BLIND | CONF | SCARE
S:BR_FIRE | BR_PLAS
S:S_HI_DEMON | S_HI_UNDEAD

S:TPORT | TELE_LEVEL | BLIND | SCARE | CAUSE_4
S:BO_ICEE | BO_MANA | BO_PLAS | BRAIN_SMASH | FORGET
S:BA_MANA | BA_FIRE | BA_WATE | BA_NETH | BA_DARK
S:S_MONSTERS | S_HI_DEMON | S_HI_UNDEAD | S_HI_DRAGON
S:S_WRAITH | S_UNIQUE

S:BA_MANA | BO_MANA | BA_NETH | BRAIN_SMASH
S:S_MONSTERS | S_UNIQUE | S_WRAITH | S_HI_UNDEAD | S_HI_DRAGON | S_HI_DEMON

S:BLIND | CONF | SCARE | CAUSE_2 | BA_POIS
S:S_MONSTER

S:BLINK | TELE_TO | BLIND | HOLD | CONF | SCARE | CAUSE_3 | CAUSE_4
S:DRAIN_MANA | BRAIN_SMASH | S_UNDEAD | S_DEMON | FORGET
S:TPORT | HEAL

S:BLINK | TELE_TO | SCARE | HOLD | CAUSE_4
S:DRAIN_MANA | BRAIN_SMASH | FORGET
S:TPORT | HEAL | S_HI_UNDEAD | S_DEMON | BA_NETH

S:BR_NETH | BR_DARK | BR_POIS

S:BR_POIS | BR_ACID | S_DEMON | BLIND | CONF | ARROW_4

S:BA_ELEC | BA_COLD | BO_ICEE | SCARE | S_DEMON

S:BR_COLD | BR_SHAR | BA_COLD | BO_ICEE | S_HI_DEMON
S:SLOW | SCARE | HOLD

S:S_HI_DEMON | S_HI_DRAGON | BR_FIRE | BR_POIS | BR_CHAO
S:SCARE | BA_FIRE | CAUSE_4

S:CONF | SCARE
S:BR_FIRE | BR_PLAS | BO_PLAS | BA_FIRE
S:S_UNDEAD | S_DEMON | S_HI_DEMON

S:ARROW_2 | MISSILE | BO_COLD | BO_ELEC | BLINK

S:HEAL | CAUSE_2 | SLOW | SCARE | BLIND

S:S_ANIMAL
S:ARROW_2 | ARROW_3 | ARROW_4 | MISSILE | BO_FIRE | BO_ELEC | BA_COLD
S:HASTE | BLINK

S:BOULDER | BO_WATE | BA_WATE | BO_ICEE | BA_ACID

S:BOULDER | BR_FIRE | BR_COLD | BR_SHAR | BR_SOUN
S:S_MONSTERS | S_KIN | TELE_TO | HEAL

S:CAUSE_2 | TELE_AWAY | BA_COLD | BO_ELEC | HOLD | DRAIN_MANA

S:BLIND | CONF | FORGET | SCARE | DRAIN_MANA | BRAIN_SMASH
S:BA_DARK | BO_MANA | BA_ACID | BA_FIRE | BA_COLD | BO_NETH
S:S_KIN

S:HEAL | BLINK | TELE_TO | BLIND | CONF | CAUSE_4 | DARKNESS
S:BA_NETH | BA_ELEC | BA_ACID | BA_FIRE | BA_COLD | BO_MANA
S:S_MONSTERS | S_UNDEAD | S_DRAGON | S_DEMON

S:HOLD | SCARE | CAUSE_3 | CAUSE_4 | DRAIN_MANA
S:BRAIN_SMASH | DARKNESS | BO_NETH | S_UNDEAD

S:BR_POIS | BR_DARK | BR_NEXU

S:BO_FIRE | SLOW | HOLD | CAUSE_3 | MISSILE

S:SLOW | HOLD | DRAIN_MANA | MIND_BLAST | HEAL
S:BA_FIRE | BO_FIRE | CAUSE_3 | S_SPIDER | S_KIN

S:1_IN_5 | MISSILE | CAUSE_1 | HEAL | CONF

S:SCARE | BR_POIS | BR_DARK

S:TELE_TO | CAUSE_4 | DRAIN_MANA
S:BRAIN_SMASH | BA_NETH | S_UNDEAD

S:BO_PLAS | BA_FIRE | BR_FIRE | BA_ELEC | S_DEMON | TELE_TO

S:HEAL | HASTE | BLIND | SCARE | S_UNDEAD
S:BA_POIS | BO_NETH | BA_COLD | DRAIN_MANA
S:MIND_BLAST | CAUSE_3 | DARKNESS | FORGET
 */

#undef _COMPARE_SPELL_DAM
#undef _TERSE_COMPARE_SPELL_DAM


	/* on-off non-damaging attacks */
	if (smart & (SM_RES_CONFU))
	{
		if ((f_attack2[1] & RSF1_CONF) && int_outof(r_ptr, 100)) f_attack2[1] &= ~RSF1_CONF;
	}

	if (smart & (SM_RES_NEXUS))
	{
		if ((f_attack2[2] & RSF2_TELE_LEVEL) && int_outof(r_ptr, 100)) f_attack2[2] &= ~RSF2_TELE_LEVEL;
	}

	if (smart & (SM_RES_FEAR))
	{
		if ((f_attack2[1] & RSF1_SCARE) && int_outof(r_ptr, 100)) f_attack2[1] &= ~RSF1_SCARE;
	}

	if (smart & (SM_RES_BLIND))
	{
		if ((f_attack2[1] & RSF1_BLIND) && int_outof(r_ptr, 100)) f_attack2[1] &= ~RSF1_BLIND;
	}

	if (smart & (SM_IMM_FREE))
	{
		if ((f_attack2[1] & (RSF1_HOLD | RSF1_SLOW)) && int_outof(r_ptr, 100)) f_attack2[1] &= ~(RSF1_HOLD | RSF1_SLOW);
	}

	if (smart & (SM_IMM_MANA))
	{
		if ((f_attack2[1] & RSF1_DRAIN_MANA) && int_outof(r_ptr, 100)) f_attack2[1] &= ~RSF1_DRAIN_MANA;
	}

	/* XXX XXX XXX No spells left? */
	/* if (!f_attack2[0] && !f_attack2[1] && !f_attack2[2]) ... */

	/* return the data */
	C_COPY(f_attack2, f_attack, RACE_FLAG_SPELL_STRICT_UB);
}


/*
 * Determine if there is a space near the selected spot in which
 * a summoned creature can appear
 */
static bool summon_possible(coord g)
{
	coord_scan t;

	/* Start at the location, and check 2 grids in each dir */
	for (t.y = g.y - 2; t.y <= g.y + 2; t.y++)
	{
		for (t.x = g.x - 2; t.x <= g.x + 2; t.x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(t.y, t.x)) continue;

			/* Only check a circular area */
			if (distance(g.y, g.x, t.y, t.x) > 2) continue;

			/* Hack: no summon on glyph of warding */
			if (cave_feat[t.y][t.x] == FEAT_GLYPH) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(t.y, t.x) && los(g, t))
			{
				return (TRUE);
			}
		}
	}

	return FALSE;
}



/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 *
 * Then we should perhaps instead supply a flag to "projectable()".  XXX XXX
 */
static bool clean_shot(coord g1, coord g2)
{
	coord grid_g[MAX(DUNGEON_HGT,DUNGEON_WID)];
	int grid_n = project_path(grid_g, MAX_RANGE, g1, g2, true, &wall_mon_stop);	/* Check the projection path */

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* May not end in an unrequested grid */
	if (grid_g[grid_n-1]!=g2) return (FALSE);

	/* May not end in a wall grid */
	if (!cave_floor_bold(g2.y, g2.x)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}


/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx_ok, int typ, int dam_hp)
{
	/* Target the player with a bolt attack */
	(void)project(m_idx_ok, 0, p_ptr->loc, dam_hp, typ, PROJECT_STOP | PROJECT_KILL);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int m_idx_ok, int typ, int dam_hp)
{
	monster_type* const m_ptr = &mon_list[m_idx_ok];
	monster_race* const r_ptr = m_ptr->race();
	int rad = (r_ptr->flags[1] & RF1_POWERFUL) ? 3 : 2;	/* Determine the radius of the blast */

	/* Target the player with a ball attack */
	(void)project(m_idx_ok, rad, p_ptr->loc, dam_hp, typ, PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL);
}


/*
 * Offsets for the spell indices
 */
#define BASE2_LOG_HACK_FRAGMENT(A,B) ((((A)>>(B)) & 1)*(B))

/** Unfortunately, this macro only works for an isolated bitflag 
 * (exactly one bit set, all others reset).  It also only works for at most 
 * 32 bits. 
 */
#define BASE2_LOG_HACK(A)	\
	(BASE2_LOG_HACK_FRAGMENT(A,31)+BASE2_LOG_HACK_FRAGMENT(A,30)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,29)+BASE2_LOG_HACK_FRAGMENT(A,28)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,27)+BASE2_LOG_HACK_FRAGMENT(A,26)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,25)+BASE2_LOG_HACK_FRAGMENT(A,24)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,23)+BASE2_LOG_HACK_FRAGMENT(A,22)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,21)+BASE2_LOG_HACK_FRAGMENT(A,20)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,19)+BASE2_LOG_HACK_FRAGMENT(A,18)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,17)+BASE2_LOG_HACK_FRAGMENT(A,16)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,15)+BASE2_LOG_HACK_FRAGMENT(A,14)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,13)+BASE2_LOG_HACK_FRAGMENT(A,12)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,11)+BASE2_LOG_HACK_FRAGMENT(A,10)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,9)+BASE2_LOG_HACK_FRAGMENT(A,8)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,7)+BASE2_LOG_HACK_FRAGMENT(A,6)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,5)+BASE2_LOG_HACK_FRAGMENT(A,4)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,3)+BASE2_LOG_HACK_FRAGMENT(A,2)+ \
	 BASE2_LOG_HACK_FRAGMENT(A,1))

#define SPELL_ORIGIN 1
#define MIN_NOMINATE_SPELL (SPELL_ORIGIN+32)

/** converts spell flag into an index value for the case statement */
#define SPELL(A,B) SPELL_ORIGIN+(A)*32+BASE2_LOG_HACK(B)

/*
 * Have a monster choose a spell to cast.
 *
 * Note that the monster's spell list has already had "useless" spells
 * (bolts that won't hit the player, summons without room, etc.) removed.
 * Perhaps that should be done by this function.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(const m_idx_type m_idx, u32b *f_attack)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();

	u32b f_mask[RACE_FLAG_SPELL_STRICT_UB] = {0L, 0L, 0L};

	int num = 0;
	byte spells[96];

	int i;
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	bool has_escape, has_attack, has_summon, has_tactic;
	bool has_annoy, has_haste, has_heal;


	/* Smart monsters restrict their spell choices. */
	if (OPTION(adult_smart_monsters) && !(r_ptr->flags[1] & RF1_STUPID))
	{
		/* What have we got? */
		has_escape = ((f_attack[0] & RSF0_ESCAPE_MASK) ||
		              (f_attack[1] & RSF1_ESCAPE_MASK) ||
		              (f_attack[2] & RSF2_ESCAPE_MASK));
		has_attack = ((f_attack[0] & RSF0_ATTACK_MASK) ||
		              (f_attack[1] & RSF1_ATTACK_MASK) ||
		              (f_attack[2] & RSF2_ATTACK_MASK));
		has_summon = ((f_attack[0] & RSF0_SUMMON_MASK) ||
		              (f_attack[1] & RSF1_SUMMON_MASK) ||
		              (f_attack[2] & RSF2_SUMMON_MASK));
		has_tactic = ((f_attack[0] & RSF0_TACTIC_MASK) ||
		              (f_attack[1] & RSF1_TACTIC_MASK) ||
		              (f_attack[2] & RSF2_TACTIC_MASK));
		has_annoy = ((f_attack[0] & RSF0_ANNOY_MASK) ||
		             (f_attack[1] & RSF1_ANNOY_MASK) ||
		             (f_attack[2] & RSF2_ANNOY_MASK));
		has_haste = ((f_attack[0] & RSF0_HASTE_MASK) ||
		             (f_attack[1] & RSF1_HASTE_MASK) ||
		             (f_attack[2] & RSF2_HASTE_MASK));
		has_heal = ((f_attack[0] & RSF0_HEAL_MASK) ||
		            (f_attack[1] & RSF1_HEAL_MASK) ||
		            (f_attack[2] & RSF2_HEAL_MASK));

		/*** Try to pick an appropriate spell type ***/

		/* Hurt badly or afraid, attempt to flee */
		if (has_escape && ((m_ptr->chp < m_ptr->mhp / 4) || m_ptr->monfear))
		{
			/* Choose escape spell */
			f_mask[0] = RSF0_ESCAPE_MASK;
			f_mask[1] = RSF1_ESCAPE_MASK;
			f_mask[2] = RSF2_ESCAPE_MASK;
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && m_ptr->chp < m_ptr->mhp / 4)
		{
			/* Choose heal spell */
			f_mask[0] = RSF0_HEAL_MASK;
			f_mask[1] = RSF1_HEAL_MASK;
			f_mask[2] = RSF2_HEAL_MASK;
		}

		/* Player is close and we have attack spells, blink away */
		else if (has_tactic && (distance(py, px, m_ptr->loc.y, m_ptr->loc.x) < 4) &&
		         has_attack && (rand_int(100) < 75))
		{
			/* Choose tactical spell */
			f_mask[0] = RSF0_TACTIC_MASK;
			f_mask[1] = RSF1_TACTIC_MASK;
			f_mask[2] = RSF2_TACTIC_MASK;
		}

		/* We're hurt (not badly), try to heal */
		else if (has_heal && (m_ptr->chp < m_ptr->mhp * 3 / 4) &&
		         (rand_int(100) < 60))
		{
			/* Choose heal spell */
			f_mask[0] = RSF0_HEAL_MASK;
			f_mask[1] = RSF1_HEAL_MASK;
			f_mask[2] = RSF2_HEAL_MASK;
		}

		/* Summon if possible (sometimes) */
		else if (has_summon && (rand_int(100) < 50))
		{
			/* Choose summon spell */
			f_mask[0] = RSF0_SUMMON_MASK;
			f_mask[1] = RSF1_SUMMON_MASK;
			f_mask[2] = RSF2_SUMMON_MASK;
		}

		/* Attack spell (most of the time) */
		else if (has_attack && (rand_int(100) < 85))
		{
			/* Choose attack spell */
			f_mask[0] = RSF0_ATTACK_MASK;
			f_mask[1] = RSF1_ATTACK_MASK;
			f_mask[2] = RSF2_ATTACK_MASK;
		}

		/* Try another tactical spell (sometimes) */
		else if (has_tactic && (rand_int(100) < 50))
		{
			/* Choose tactic spell */
			f_mask[0] = RSF0_TACTIC_MASK;
			f_mask[1] = RSF1_TACTIC_MASK;
			f_mask[2] = RSF2_TACTIC_MASK;
		}

		/* Haste self if we aren't already somewhat hasted (rarely) */
		else if (has_haste && (rand_int(100) < (20 + r_ptr->speed - m_ptr->speed)))
		{
			/* Choose haste spell */
			f_mask[0] = RSF0_HASTE_MASK;
			f_mask[1] = RSF1_HASTE_MASK;
			f_mask[2] = RSF2_HASTE_MASK;
		}

		/* Annoy player (most of the time) */
		else if (has_annoy && (rand_int(100) < 85))
		{
			/* Choose annoyance spell */
			f_mask[0] = RSF0_ANNOY_MASK;
			f_mask[1] = RSF1_ANNOY_MASK;
			f_mask[2] = RSF2_ANNOY_MASK;
		}

		/* Else choose no spell (The masks default to this.) */

		/* Keep only the interesting spells */
		for (i = 0; i < RACE_FLAG_SPELL_STRICT_UB; i++)
			f_attack[i] &= f_mask[i];

		/* Anything left? */
		if (!(f_attack[0] || f_attack[1] || f_attack[2])) return (0);
	}

	/* Extract all spells */
	for (i = 0; i < 32 * RACE_FLAG_SPELL_STRICT_UB; i++)
	{
		if (TEST_FLAG(f_attack, i)) spells[num++] = i + SPELL_ORIGIN;
	}

	/* Paranoia */
	if (num == 0) return 0;

	/* Pick at random */
	return (spells[rand_int(num)]);
}

/* spell implementations for struct lookup */
static void mspell_SHRIEK(int m_idx_ok)
{
	aggravate_monsters(m_idx_ok);
}

static void mspell_ARROW_1(int m_idx_ok)
{
	const monster_type& m = mon_list[m_idx_ok];
	if (missile_test_hit(40+m.race()->level, p_ptr->total_ac(), true, distance(m.loc.y, m.loc.x, p_ptr->loc.y, p_ptr->loc.x), p_ptr->loc))
	{
		msg_print("The arrow misses.");
		return;
	};
	bolt(m_idx_ok, GF_ARROW, NdS(1, 6));
}

static void mspell_ARROW_2(int m_idx_ok)
{
	const monster_type& m = mon_list[m_idx_ok];
	if (missile_test_hit(40+m.race()->level, p_ptr->total_ac(), true, distance(m.loc.y, m.loc.x, p_ptr->loc.y, p_ptr->loc.x), p_ptr->loc))
	{
		msg_print("The arrow misses.");
		return;
	};
	bolt(m_idx_ok, GF_ARROW, NdS(3, 6));
}

static void mspell_ARROW_3(int m_idx_ok)
{
	const monster_type& m = mon_list[m_idx_ok];
	if (missile_test_hit(40+m.race()->level, p_ptr->total_ac(), true, distance(m.loc.y, m.loc.x, p_ptr->loc.y, p_ptr->loc.x), p_ptr->loc))
	{
		msg_print("The missile misses.");
		return;
	};
	bolt(m_idx_ok, GF_ARROW, NdS(5, 6));
}

static void mspell_ARROW_4(int m_idx_ok)
{
	const monster_type& m = mon_list[m_idx_ok];
	if (missile_test_hit(40+m.race()->level, p_ptr->total_ac(), true, distance(m.loc.y, m.loc.x, p_ptr->loc.y, p_ptr->loc.x), p_ptr->loc))
	{
		msg_print("The missle misses.");
		return;
	};
	bolt(m_idx_ok, GF_ARROW, NdS(7, 6));
}

static void mspell_BOULDER(int m_idx_ok)
{
	const monster_type& m = mon_list[m_idx_ok];
	if (missile_test_hit(40+m.race()->level, p_ptr->total_ac(), true, distance(m.loc.y, m.loc.x, p_ptr->loc.y, p_ptr->loc.x), p_ptr->loc))
	{
		msg_print("The boulder misses.");
		return;
	};
	bolt(m_idx_ok, GF_ARROW, NdS(1 + mon_list[m_idx_ok].race()->level / 7, 12));
}

static void mspell_BR_ACID(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_ACID, MIN(dam, 1600));
}

static void mspell_BR_ELEC(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_ELEC, MIN(dam, 1600));
}

static void mspell_BR_FIRE(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_FIRE, MIN(dam, 1600));
}

static void mspell_BR_COLD(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_COLD, MIN(dam, 1600));
}

static void mspell_BR_POIS(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_POIS, MIN(dam, 800));
}

static void mspell_BR_NETH(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_NETHER, MIN(dam, 550));
}

static void mspell_BR_LITE(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_LITE, MIN(dam, 400));
}

static void mspell_BR_DARK(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_DARK, MIN(dam, 400));
}

static void mspell_BR_CONF(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_CONFUSION, MIN(dam, 400));
}

static void mspell_BR_SOUND(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_SOUND, MIN(dam, 500));
}

static void mspell_BR_CHAOS(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_CHAOS, MIN(dam, 500));
}

static void mspell_BR_DISE(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_DISENCHANT, MIN(dam, 500));
}

static void mspell_BR_NEXU(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_NEXUS, MIN(dam, 400));
}

static void mspell_BR_TIME(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_TIME, MIN(dam, 150));
}

static void mspell_BR_INER(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_INERTIA, MIN(dam, 200));
}

static void mspell_BR_GRAV(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_GRAVITY, MIN(dam, 200));
}

static void mspell_BR_SHAR(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_SHARD, MIN(dam, 500));
}

static void mspell_BR_PLAS(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/3;
	breath(m_idx_ok, GF_PLASMA, MIN(dam, 150));
}

static void mspell_BR_WALL(int m_idx_ok)
{
	const int dam = mon_list[m_idx_ok].chp/6;
	breath(m_idx_ok, GF_FORCE, MIN(dam, 200));
}

static void mspell_BA_ACID(int m_idx_ok)
{
	breath(m_idx_ok, GF_ACID, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 3) + 15);
}

static void mspell_BA_ELEC(int m_idx_ok)
{
	breath(m_idx_ok, GF_ELEC, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 3 / 2) + 8);
}

static void mspell_BA_FIRE(int m_idx_ok)
{
	breath(m_idx_ok, GF_FIRE, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 7 / 2) + 10);
}

static void mspell_BA_COLD(int m_idx_ok)
{
	breath(m_idx_ok, GF_COLD, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 3 / 2) + 10);
}

static void mspell_BA_POIS(int m_idx_ok)
{
	breath(m_idx_ok, GF_POIS, NdS(12, 2));
}

static void mspell_BA_NETH(int m_idx_ok)
{
	breath(m_idx_ok, GF_NETHER, (50 + NdS(10, 10) + MIN(mon_list[m_idx_ok].race()->level,1)));
}

static void mspell_BA_WATE(int m_idx_ok)
{
	msg_print("You are engulfed in a whirlpool.");
	breath(m_idx_ok, GF_WATER, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 5 / 2) + 50);
}

static void mspell_BA_MANA(int m_idx_ok)
{
	breath(m_idx_ok, GF_MANA, (MIN(mon_list[m_idx_ok].race()->level,1) * 5) + NdS(10, 10));
}

static void mspell_BA_DARK(int m_idx_ok)
{
	breath(m_idx_ok, GF_DARK, (MIN(mon_list[m_idx_ok].race()->level,1) * 5) + NdS(10, 10));
}

static void mspell_DRAIN_MANA(int m_idx_ok)
{
	monster_type& m = mon_list[m_idx_ok];
	if (p_ptr->csp)
	{
		int r1 = (randint(MIN(mon_list[m_idx_ok].race()->level,1)) / 2) + 1;	/* Attack power */

		/* Full drain */
		if (r1 >= p_ptr->csp)
		{
			r1 = p_ptr->csp;
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}

		/* Partial drain */
		else
		{
			p_ptr->csp -= r1;
		}

		/* Redraw mana */
		p_ptr->redraw |= (PR_MANA);

		/* Heal the monster */
		if (m.chp < m.mhp)
		{
			/* Heal */
			if ((m.mhp - m.chp) <= (6 * r1))
			{
				m.chp = m.mhp;
			}
			else
			{
				m.chp += (6 * r1);
			};

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx_ok) p_ptr->redraw |= (PR_HEALTH);

			/* Special message */
			if (!p_ptr->timed[TMD_BLIND] && m.ml)
			{
				char m_name[80];

				/* Get the monster name (or "it") */
				monster_desc(m_name, sizeof(m_name), &m, 0x00);

				msg_format("%^s appears healthier.", m_name);
			}
		}
	}
	else
	{
		msg_print("Or would have, if you had any magical energy to drain.");
	}
	update_smart_learn(m_idx_ok, DRS_MANA);
}

static void mspell_MIND_BLAST(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		msg_print("Your mind is blasted by psionic energy.");
		if (!p_ptr->resist_confu) p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
		take_hit(NdS(8, 8), ddesc);
	}
}

static void mspell_BRAIN_SMASH(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		msg_print("Your mind is blasted by psionic energy.");
		take_hit(NdS(12, 15), ddesc);
		if (!p_ptr->resist_blind) p_ptr->inc_timed<TMD_BLIND>(8 + rand_int(8));
		if (!p_ptr->resist_confu) p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
		if (!p_ptr->free_act) p_ptr->inc_timed<TMD_PARALYZED>(rand_int(4) + 4);
		p_ptr->inc_timed<TMD_SLOW>(rand_int(4) + 4);
	}
}

static void mspell_CAUSE_1(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		take_hit(NdS(3, 8), ddesc);
	}
}

static void mspell_CAUSE_2(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		take_hit(NdS(8, 8), ddesc);
	}
}

static void mspell_CAUSE_3(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		take_hit(NdS(10, 15), ddesc);
	}
}

static void mspell_CAUSE_4(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		char ddesc[80];
		/* Get the "died from" name */
		monster_desc(ddesc, sizeof(ddesc), mon_list + m_idx_ok, MDESC_SHOW | MDESC_IND2);

		take_hit(NdS(15, 15), ddesc);
		p_ptr->inc_timed<TMD_CUT>(NdS(10, 10));
	}
}

static void mspell_BO_ACID(int m_idx_ok)
{
	bolt(m_idx_ok, GF_ACID, NdS(7, 8) + (MIN(mon_list[m_idx_ok].race()->level,1) / 3));
}

static void mspell_BO_ELEC(int m_idx_ok)
{
	bolt(m_idx_ok, GF_ELEC, NdS(4, 8) + (MIN(mon_list[m_idx_ok].race()->level,1) / 3));
}

static void mspell_BO_FIRE(int m_idx_ok)
{
	bolt(m_idx_ok, GF_FIRE, NdS(9, 8) + (MIN(mon_list[m_idx_ok].race()->level,1) / 3));
}

static void mspell_BO_COLD(int m_idx_ok)
{
	bolt(m_idx_ok, GF_COLD, NdS(6, 8) + (MIN(mon_list[m_idx_ok].race()->level,1) / 3));
}

static void mspell_BO_NETH(int m_idx_ok)
{
	bolt(m_idx_ok, GF_NETHER, 30 + NdS(5, 5) + (MIN(mon_list[m_idx_ok].race()->level,1) * 3) / 2);
}

static void mspell_BO_WATE(int m_idx_ok)
{
	bolt(m_idx_ok, GF_WATER, NdS(10, 10) + (MIN(mon_list[m_idx_ok].race()->level,1)));
}

static void mspell_BO_MANA(int m_idx_ok)
{
	bolt(m_idx_ok, GF_MANA, randint(MIN(mon_list[m_idx_ok].race()->level,1) * 7 / 2) + 50);
}

static void mspell_BO_PLAS(int m_idx_ok)
{
	bolt(m_idx_ok, GF_PLASMA, 10 + NdS(8, 7) + MIN(mon_list[m_idx_ok].race()->level,1));
}

static void mspell_BO_ICEE(int m_idx_ok)
{
	bolt(m_idx_ok, GF_ICE, NdS(6, 6) + MIN(mon_list[m_idx_ok].race()->level,1));
}

static void mspell_MISSILE(int m_idx_ok)
{
	bolt(m_idx_ok, GF_MISSILE, NdS(2, 6) + (MIN(mon_list[m_idx_ok].race()->level,1) / 3));
}

static void mspell_SCARE(int m_idx_ok)
{
	if (p_ptr->resist_fear)
	{
		msg_print("You refuse to be frightened.");
	}
	else if (p_ptr->std_save())
	{
		msg_print("You refuse to be frightened.");
	}
	else
	{
		(void)p_ptr->inc_timed<TMD_AFRAID>(rand_int(4) + 4);
	}
	update_smart_learn(m_idx_ok, DRS_RES_FEAR);
}

static void mspell_BLIND(int m_idx_ok)
{
	if (p_ptr->resist_blind)
	{
		msg_print("You are unaffected!");
	}
	else if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		(void)p_ptr->set_timed<TMD_BLIND>(12 + rand_int(4));
	}
	update_smart_learn(m_idx_ok, DRS_RES_BLIND);
}

static void mspell_CONF(int m_idx_ok)
{
	if (p_ptr->resist_confu)
	{
		msg_print("You disbelieve the feeble spell.");
	}
	else if (p_ptr->std_save())
	{
		msg_print("You disbelieve the feeble spell.");
	}
	else
	{
		(void)p_ptr->inc_timed<TMD_CONFUSED>(rand_int(4) + 4);
	}
	update_smart_learn(m_idx_ok, DRS_RES_CONFU);
}

static void mspell_SLOW(int m_idx_ok)
{
	if (p_ptr->free_act)
	{
		msg_print("You are unaffected!");
	}
	else if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		(void)p_ptr->inc_timed<TMD_SLOW>(rand_int(4) + 4);
	}
	update_smart_learn(m_idx_ok, DRS_FREE);
}

static void mspell_HOLD(int m_idx_ok)
{
	if (p_ptr->free_act)
	{
			msg_print("You are unaffected!");
	}
	else if (p_ptr->std_save())
	{
		msg_format("You resist the effects!");
	}
	else
	{
		(void)p_ptr->inc_timed<TMD_PARALYZED>(rand_int(4) + 4);
	}
	update_smart_learn(m_idx_ok, DRS_FREE);
}

static void mspell_HASTE(int m_idx_ok)
{
	monster_type& m = mon_list[m_idx_ok];
	const monster_race* const r_ptr = m.race();
	char m_name[80];

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), &m, 0x00);

	/* Allow quick speed increases to base+10 */
	if (m.speed < r_ptr->speed + 10)
	{
		msg_format("%^s starts moving faster.", m_name);
		m.speed += 10;
	}

	/* Allow small speed increases to base+20 */
	else if (m.speed < r_ptr->speed + 20)
	{
		msg_format("%^s starts moving faster.", m_name);
		m.speed += 2;
	}
}

static void mspell_HEAL(int m_idx_ok)
{
	monster_type& m = mon_list[m_idx_ok];
	const int heal_power = MIN(mon_list[m_idx_ok].race()->level,1)*6;
	char m_name[80];

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), &m, 0x00);

	/* Fully healed */
	if ((m.mhp - m.chp) <= heal_power)
	{
		/* Fully healed */
		m.chp = m.mhp;

		msg_format((!p_ptr->timed[TMD_BLIND] && m.ml)	? "%^s looks REALLY healthy!"
							: "%^s sounds REALLY healthy!", m_name);
	}

	/* Partially healed */
	else
	{
		m.chp += heal_power;
		
		msg_format((!p_ptr->timed[TMD_BLIND] && m.ml)	? "%^s looks healthier."
							: "%^s sounds healthier.", m_name);
	}

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx_ok) p_ptr->redraw |= (PR_HEALTH);

	/* Cancel fear */
	if (m.monfear)
	{
		char m_poss[80];	

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, sizeof(m_poss), &m, MDESC_PRO2 | MDESC_POSS);

		m.monfear = 0;
		msg_format("%^s recovers %s courage.", m_name, m_poss);
	}
}

static void mspell_BLINK(int m_idx_ok)
{
	teleport_away(m_idx_ok, 10);
}

static void mspell_TELE_AWAY(int m_idx_ok)
{
	teleport_away(m_idx_ok, 10);
}

static void mspell_TPORT(int m_idx_ok)
{
	teleport_away(m_idx_ok, MAX_SIGHT * 2 + 5);
}

static void mspell_TELE_TO(int m_idx_ok)
{
	teleport_player_to(mon_list[m_idx_ok].loc);
}

static void mspell_TELE_LEVEL(int m_idx_ok)
{
	if (p_ptr->resist_nexus)
	{
		msg_print("You are unaffected!");
	}
	else if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else
	{
		teleport_player_level();
	}
	update_smart_learn(m_idx_ok, DRS_RES_NEXUS);
}

static void mspell_DARKNESS(int m_idx_ok)
{
	unlite_area(0, 3);
}

static void mspell_TRAPS(int m_idx_ok)
{
	trap_creation();
}

static void mspell_FORGET(int m_idx_ok)
{
	if (p_ptr->std_save())
	{
		msg_print("You resist the effects!");
	}
	else if (lose_all_info())
	{
		msg_print("Your memories fade away.");
	}
}

static void mspell_S_KIN(int m_idx_ok)
{
	int count = 0;
	{
	const monster_type& m = mon_list[m_idx_ok];
	const monster_race* const r_ptr = m.race();
	const int rlev = MIN(r_ptr->level,1);
	int k = 6;
	char m_poss[80];	
	char m_name[80];

	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), &m, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), &m, MDESC_PRO2 | MDESC_POSS);

	if (!p_ptr->timed[TMD_BLIND]) msg_format("%^s magically summons %s %s.",
 						m_name, m_poss,
		                ((r_ptr->flags[0] & RF0_UNIQUE) ? "minions" : "kin"));

	/* Hack -- Set the letter of the monsters to summon */
	summon_kin_type = r_ptr->d_char;

	do	count += summon_specific(m.loc, rlev, SUMMON_KIN);
	while(0 < --k);
	}
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_HI_DEMON(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_HI_DEMON);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many evil things appear nearby.");
}

static void mspell_S_MONSTER(int m_idx_ok)
{
	const int rlev = MIN(mon_list[m_idx_ok].race()->level,1);
	int count =  summon_specific(mon_list[m_idx_ok].loc, rlev, 0);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear something appear nearby.");
}

static void mspell_S_MONSTERS(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, 0);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_ANIMAL(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_ANIMAL);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_SPIDER(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 6;
	do	count += summon_specific(m.loc, rlev, SUMMON_SPIDER);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_HOUND(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 6;
	do	count += summon_specific(m.loc, rlev, SUMMON_HOUND);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_HYDRA(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 6;
	do	count += summon_specific(m.loc, rlev, SUMMON_HYDRA);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many things appear nearby.");
}

static void mspell_S_ANGEL(int m_idx_ok)
{
	const int rlev = MIN(mon_list[m_idx_ok].race()->level,1);
	int count = summon_specific(mon_list[m_idx_ok].loc, rlev, SUMMON_ANGEL);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear something appear nearby.");
}

static void mspell_S_DEMON(int m_idx_ok)
{
	const int rlev = MIN(mon_list[m_idx_ok].race()->level,1);
	int count = summon_specific(mon_list[m_idx_ok].loc, rlev, SUMMON_DEMON);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear something appear nearby.");
}

static void mspell_S_UNDEAD(int m_idx_ok)
{
	const int rlev = MIN(mon_list[m_idx_ok].race()->level,1);
	int count = summon_specific(mon_list[m_idx_ok].loc, rlev, SUMMON_UNDEAD);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear something appear nearby.");
}

static void mspell_S_DRAGON(int m_idx_ok)
{
	const int rlev = MIN(mon_list[m_idx_ok].race()->level,1);
	int count = summon_specific(mon_list[m_idx_ok].loc, rlev, SUMMON_DRAGON);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear something appear nearby.");
}

static void mspell_S_HI_UNDEAD(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_HI_UNDEAD);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many creepy things appear nearby.");
}

static void mspell_S_HI_DRAGON(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_HI_DRAGON);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many powerful things appear nearby.");
}

static void mspell_S_WRAITH(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_WRAITH);
	while(0 < --k);

	k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_HI_UNDEAD);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many creepy things appear nearby.");
}

static void mspell_S_UNIQUE(int m_idx_ok)
{
	int count = 0;
	const monster_type& m = mon_list[m_idx_ok];
	const int rlev = MIN(m.race()->level,1);
	int k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_UNIQUE);
	while(0 < --k);

	k = 8;
	do	count += summon_specific(m.loc, rlev, SUMMON_HI_UNDEAD);
	while(0 < --k);
	if (p_ptr->timed[TMD_BLIND] && count) msg_print("You hear many powerful things appear nearby.");
}

typedef void detailed_attack(int);
struct m_spell_config
{
	const char* const blind_msg;
	const char* const unseen_msg;
	const char* const seen_msg;

	detailed_attack* const m_spell_config;
	const byte sound_idx;
};

#define MSPELL_MACRO_NULL {NULL, NULL, NULL, NULL, 0}
#define UNIFORM_MSG(A)	A, A, A
#define BLIND_MSG(A,B)	A, B, B
#define SEEN_MSG(A,B)	A, A, B

#define MSPELL_SHRIEK		{UNIFORM_MSG("%^s makes a high pitched shriek."), mspell_SHRIEK , MSG_SHRIEK}
#define MSPELL_ARROW_1		{BLIND_MSG("%^s makes a strange noise.", "%^s fires an arrow."), mspell_ARROW_1, 0}
#define MSPELL_ARROW_2		{BLIND_MSG("%^s makes a strange noise.", "%^s fires an arrow!"), mspell_ARROW_2, 0}
#define MSPELL_ARROW_3		{BLIND_MSG("%^s makes a strange noise.", "%^s fires a missile."), mspell_ARROW_3, 0}
#define MSPELL_ARROW_4		{BLIND_MSG("%^s makes a strange noise.", "%^s fires a missile!"), mspell_ARROW_4, 0}
#define MSPELL_BR_ACID		{BLIND_MSG("%^s breathes.", "%^s breathes acid."), mspell_BR_ACID, MSG_BR_ACID}
#define MSPELL_BR_ELEC		{BLIND_MSG("%^s breathes.", "%^s breathes lightning."), mspell_BR_ELEC, MSG_BR_ELEC}
#define MSPELL_BR_FIRE		{BLIND_MSG("%^s breathes.", "%^s breathes fire."), mspell_BR_FIRE, MSG_BR_FIRE}
#define MSPELL_BR_COLD		{BLIND_MSG("%^s breathes.", "%^s breathes frost."), mspell_BR_COLD, MSG_BR_FROST}
#define MSPELL_BR_POIS		{BLIND_MSG("%^s breathes.", "%^s breathes gas."), mspell_BR_POIS, MSG_BR_GAS}
#define MSPELL_BR_NETH		{BLIND_MSG("%^s breathes.", "%^s breathes nether."), mspell_BR_NETH, MSG_BR_NETHER}
#define MSPELL_BR_LITE		{BLIND_MSG("%^s breathes.", "%^s breathes light."), mspell_BR_LITE, MSG_BR_LIGHT}
#define MSPELL_BR_DARK		{BLIND_MSG("%^s breathes.", "%^s breathes darkness."), mspell_BR_DARK, MSG_BR_DARK}
#define MSPELL_BR_CONF		{BLIND_MSG("%^s breathes.", "%^s breathes confusion."), mspell_BR_CONF, MSG_BR_CONF}
#define MSPELL_BR_SOUND		{BLIND_MSG("%^s breathes.", "%^s breathes sound."), mspell_BR_SOUND, MSG_BR_SOUND}
#define MSPELL_BR_CHAOS		{BLIND_MSG("%^s breathes.", "%^s breathes chaos."), mspell_BR_CHAOS, MSG_BR_CHAOS}
#define MSPELL_BR_DISE		{BLIND_MSG("%^s breathes.", "%^s breathes disenchantment."), mspell_BR_DISE, MSG_BR_DISENCHANT}
#define MSPELL_BR_NEXU		{BLIND_MSG("%^s breathes.", "%^s breathes nexus."), mspell_BR_NEXU, MSG_BR_NEXUS}
#define MSPELL_BR_TIME		{BLIND_MSG("%^s breathes.", "%^s breathes time."), mspell_BR_TIME, MSG_BR_TIME}
#define MSPELL_BR_INER		{BLIND_MSG("%^s breathes.", "%^s breathes inertia."), mspell_BR_INER, MSG_BR_INERTIA}
#define MSPELL_BR_GRAV		{BLIND_MSG("%^s breathes.", "%^s breathes gravity."), mspell_BR_GRAV, MSG_BR_GRAVITY}
#define MSPELL_BR_SHAR		{BLIND_MSG("%^s breathes.", "%^s breathes shards."), mspell_BR_SHAR, MSG_BR_SHARDS}
#define MSPELL_BR_PLAS		{BLIND_MSG("%^s breathes.", "%^s breathes plasma."), mspell_BR_PLAS, MSG_BR_PLASMA}
#define MSPELL_BR_WALL		{BLIND_MSG("%^s breathes.", "%^s breathes force."), mspell_BR_WALL, MSG_BR_FORCE}
#define MSPELL_BOULDER		{BLIND_MSG("%^s grunts with exertion.", "%^s hurls a boulder at you!"), mspell_BOULDER, 0}

#define MSPELL_BA_ACID		{BLIND_MSG("%^s mumbles.", "%^s casts an acid ball."), mspell_BA_ACID, 0}
#define MSPELL_BA_ELEC		{BLIND_MSG("%^s mumbles.", "%^s casts a lightning ball."), mspell_BA_ELEC, 0}
#define MSPELL_BA_FIRE		{BLIND_MSG("%^s mumbles.", "%^s casts a fire ball."), mspell_BA_FIRE, 0}
#define MSPELL_BA_COLD		{BLIND_MSG("%^s mumbles.", "%^s casts a frost ball."), mspell_BA_COLD, 0}
#define MSPELL_BA_POIS		{BLIND_MSG("%^s mumbles.", "%^s casts a stinking cloud."), mspell_BA_POIS, 0}
#define MSPELL_BA_NETH		{BLIND_MSG("%^s mumbles.", "%^s casts a nether ball."), mspell_BA_NETH, 0}
#define MSPELL_BA_WATE		{BLIND_MSG("%^s mumbles.", "%^s gestures fluidly."), mspell_BA_WATE, 0}
#define MSPELL_BA_MANA		{BLIND_MSG("%^s mumbles powerfully.", "%^s invokes a mana storm."), mspell_BA_MANA, 0}
#define MSPELL_BA_DARK		{BLIND_MSG("%^s mumbles powerfully.", "%^s invokes a darkness storm."), mspell_BA_DARK, 0}
#define MSPELL_DRAIN_MANA	{UNIFORM_MSG("%^s draws psychic energy from you!"), mspell_DRAIN_MANA, 0}
#define MSPELL_MIND_BLAST	{SEEN_MSG("You feel something focusing on your mind.", "%^s gazes deep into your eyes."), mspell_MIND_BLAST, 0}
#define MSPELL_BRAIN_SMASH	{SEEN_MSG("You feel something focusing on your mind.", "%^s looks deep into your eyes."), mspell_BRAIN_SMASH, 0}
#define MSPELL_CAUSE_1		{BLIND_MSG("%^s mumbles.", "%^s points at you and curses."), mspell_CAUSE_1, 0}
#define MSPELL_CAUSE_2		{BLIND_MSG("%^s mumbles.", "%^s points at you and curses horribly."), mspell_CAUSE_2, 0}
#define MSPELL_CAUSE_3		{BLIND_MSG("%^s mumbles.", "%^s points at you, incanting terribly!"), mspell_CAUSE_3, 0}
#define MSPELL_CAUSE_4		{BLIND_MSG("%^s mumbles.", "%^s points at you, screaming the word DIE!"), mspell_CAUSE_4, 0}
#define MSPELL_BO_ACID		{BLIND_MSG("%^s mumbles.", "%^s casts a acid bolt."), mspell_BO_ACID, 0}
#define MSPELL_BO_ELEC		{BLIND_MSG("%^s mumbles.", "%^s casts a lightning bolt."), mspell_BO_ELEC, 0}
#define MSPELL_BO_FIRE		{BLIND_MSG("%^s mumbles.", "%^s casts a fire bolt."), mspell_BO_FIRE, 0}
#define MSPELL_BO_COLD		{BLIND_MSG("%^s mumbles.", "%^s casts a frost bolt."), mspell_BO_COLD, 0}
#define MSPELL_BO_NETH		{BLIND_MSG("%^s mumbles.", "%^s casts a nether bolt."), mspell_BO_NETH, 0}
#define MSPELL_BO_WATE		{BLIND_MSG("%^s mumbles.", "%^s casts a water bolt."), mspell_BO_WATE, 0}
#define MSPELL_BO_MANA		{BLIND_MSG("%^s mumbles.", "%^s casts a mana bolt."), mspell_BO_MANA, 0}
#define MSPELL_BO_PLAS		{BLIND_MSG("%^s mumbles.", "%^s casts a plasma bolt."), mspell_BO_PLAS, 0}
#define MSPELL_BO_ICEE		{BLIND_MSG("%^s mumbles.", "%^s casts an ice bolt."), mspell_BO_ICEE, 0}
#define MSPELL_MISSILE		{BLIND_MSG("%^s mumbles.", "%^s casts a magic missile."), mspell_MISSILE, 0}
#define MSPELL_SCARE		{BLIND_MSG("%^s mumbles, and you hear scary noises.", "%^s casts a fearful illusion."), mspell_SCARE, MSG_CAST_FEAR}
#define MSPELL_BLIND		{BLIND_MSG("%^s mumbles.", "%^s casts a spell, burning your eyes!"), mspell_BLIND, 0}
#define MSPELL_CONF			{BLIND_MSG("%^s mumbles, and you hear puzzling noises.", "%^s creates a mesmerising illusion."), mspell_CONF, 0}
#define MSPELL_SLOW			{UNIFORM_MSG("%^s drains power from your muscles!"), mspell_SLOW, 0}
#define MSPELL_HOLD			{BLIND_MSG("%^s mumbles.", "%^s stares deep into your eyes!"), mspell_HOLD, 0}

#define MSPELL_HASTE		{BLIND_MSG("%^s mumbles.", "%^s concentrates on %s body."), mspell_HASTE, 0}
#define MSPELL_HEAL			{BLIND_MSG("%^s mumbles.", "%^s concentrates on %s wounds."), mspell_HEAL, 0}
#define MSPELL_BLINK		{UNIFORM_MSG("%^s blinks away."), mspell_BLINK, 0}
#define MSPELL_TPORT		{UNIFORM_MSG("%^s teleports away."), mspell_TPORT, 0}
#define MSPELL_TELE_TO		{UNIFORM_MSG("%^s commands you to return."), mspell_TELE_TO, 0}
#define MSPELL_TELE_AWAY	{UNIFORM_MSG("%^s teleports you away."), mspell_TELE_AWAY, 0}
#define MSPELL_TELE_LEVEL	{BLIND_MSG("%^s mumbles strangely.", "%^s gestures at your feet."), mspell_TELE_LEVEL, 0}
#define MSPELL_DARKNESS		{BLIND_MSG("%^s mumbles.", "%^s gestures in shadow."), mspell_DARKNESS, 0}
#define MSPELL_TRAPS		{BLIND_MSG("%^s mumbles, and then cackles evilly.", "%^s casts a spell and cackles evilly."), mspell_TRAPS, MSG_CREATE_TRAP}
#define MSPELL_FORGET		{UNIFORM_MSG("%^s tries to blank your mind."), mspell_FORGET, 0}
#define MSPELL_S_KIN		{BLIND_MSG("%^s mumbles.", NULL), mspell_S_KIN, MSG_SUM_MONSTER}
#define MSPELL_S_HI_DEMON	{BLIND_MSG("%^s mumbles.", "%^s magically summons greater demons!"), mspell_S_HI_DEMON, MSG_SUM_HI_DEMON}
#define MSPELL_S_MONSTER	{BLIND_MSG("%^s mumbles.", "%^s magically summons help!"), mspell_S_MONSTER, MSG_SUM_MONSTER}
#define MSPELL_S_MONSTERS	{BLIND_MSG("%^s mumbles.", "%^s magically summons monsters!"), mspell_S_MONSTERS, MSG_SUM_MONSTER}
#define MSPELL_S_ANIMAL		{BLIND_MSG("%^s mumbles.", "%^s magically summons animals."), mspell_S_ANIMAL, MSG_SUM_ANIMAL}
#define MSPELL_S_SPIDER		{BLIND_MSG("%^s mumbles.", "%^s magically summons spiders."), mspell_S_SPIDER, MSG_SUM_SPIDER}
#define MSPELL_S_HOUND		{BLIND_MSG("%^s mumbles.", "%^s magically summons hounds."), mspell_S_HOUND, MSG_SUM_HOUND}
#define MSPELL_S_HYDRA		{BLIND_MSG("%^s mumbles.", "%^s magically summons hydras."), mspell_S_HYDRA, MSG_SUM_HYDRA}
#define MSPELL_S_ANGEL		{BLIND_MSG("%^s mumbles.", "%^s magically summons an angel!"), mspell_S_ANGEL, MSG_SUM_ANGEL}
#define MSPELL_S_DEMON		{BLIND_MSG("%^s mumbles.", "%^s magically summons a hellish adversary!"), mspell_S_DEMON, MSG_SUM_DEMON}
#define MSPELL_S_UNDEAD		{BLIND_MSG("%^s mumbles.", "%^s magically summons an undead adversary!"), mspell_S_UNDEAD, MSG_SUM_UNDEAD}
#define MSPELL_S_DRAGON		{BLIND_MSG("%^s mumbles.", "%^s magically summons a dragon!"), mspell_S_DRAGON, MSG_SUM_DRAGON}
#define MSPELL_S_HI_UNDEAD	{BLIND_MSG("%^s mumbles.", "%^s magically summons greater undead!"), mspell_S_HI_UNDEAD, MSG_SUM_HI_UNDEAD}
#define MSPELL_S_HI_DRAGON	{BLIND_MSG("%^s mumbles.", "%^s magically summons ancient dragons!"), mspell_S_HI_DRAGON, MSG_SUM_HI_DRAGON}
#define MSPELL_S_WRAITH		{BLIND_MSG("%^s mumbles.", "%^s magically summons mighty undead opponents!"), mspell_S_WRAITH, MSG_SUM_WRAITH}
#define MSPELL_S_UNIQUE		{BLIND_MSG("%^s mumbles.", "%^s magically summons special opponents!"), mspell_S_UNIQUE, MSG_SUM_UNIQUE}

#define MSPELL_DEF(A) MSPELL_DEF2(ANG_CONCAT(RFA_,A))
#define MSPELL_DEF2(A) ANG_CONCAT(MSPELL_,A)

/* actual array */
m_spell_config magic_attack_list[]	=	{
	MSPELL_DEF(0),
	MSPELL_DEF(1),
	MSPELL_DEF(2),
	MSPELL_DEF(3),
	MSPELL_DEF(4),
	MSPELL_DEF(5),
	MSPELL_DEF(6),
	MSPELL_DEF(7),
	MSPELL_DEF(8),
	MSPELL_DEF(9),
	MSPELL_DEF(10),
	MSPELL_DEF(11),
	MSPELL_DEF(12),
	MSPELL_DEF(13),
	MSPELL_DEF(14),
	MSPELL_DEF(15),
	MSPELL_DEF(16),
	MSPELL_DEF(17),
	MSPELL_DEF(18),
	MSPELL_DEF(19),
	MSPELL_DEF(20),
	MSPELL_DEF(21),
	MSPELL_DEF(22),
	MSPELL_DEF(23),
	MSPELL_DEF(24),
	MSPELL_DEF(25),
	MSPELL_DEF(26),
	MSPELL_DEF(27),
	MSPELL_DEF(28),
	MSPELL_DEF(29),
	MSPELL_DEF(30),
	MSPELL_DEF(31),
	MSPELL_DEF(32),
	MSPELL_DEF(33),
	MSPELL_DEF(34),
	MSPELL_DEF(35),
	MSPELL_DEF(36),
	MSPELL_DEF(37),
	MSPELL_DEF(38),
	MSPELL_DEF(39),
	MSPELL_DEF(40),
	MSPELL_DEF(41),
	MSPELL_DEF(42),
	MSPELL_DEF(43),
	MSPELL_DEF(44),
	MSPELL_DEF(45),
	MSPELL_DEF(46),
	MSPELL_DEF(47),
	MSPELL_DEF(48),
	MSPELL_DEF(49),
	MSPELL_DEF(50),
	MSPELL_DEF(51),
	MSPELL_DEF(52),
	MSPELL_DEF(53),
	MSPELL_DEF(54),
	MSPELL_DEF(55),
	MSPELL_DEF(56),
	MSPELL_DEF(57),
	MSPELL_DEF(58),
	MSPELL_DEF(59),
	MSPELL_DEF(60),
	MSPELL_DEF(61),
	MSPELL_DEF(62),
	MSPELL_DEF(63),
	MSPELL_DEF(64),
	MSPELL_DEF(65),
	MSPELL_DEF(66),
	MSPELL_DEF(67),
	MSPELL_DEF(68),
	MSPELL_DEF(69),
	MSPELL_DEF(70),
	MSPELL_DEF(71),
	MSPELL_DEF(72),
	MSPELL_DEF(73),
	MSPELL_DEF(74),
	MSPELL_DEF(75),
	MSPELL_DEF(76),
	MSPELL_DEF(77),
	MSPELL_DEF(78),
	MSPELL_DEF(79),
	MSPELL_DEF(80),
	MSPELL_DEF(81),
	MSPELL_DEF(82),
	MSPELL_DEF(83),
	MSPELL_DEF(84),
	MSPELL_DEF(85),
	MSPELL_DEF(86),
	MSPELL_DEF(87),
	MSPELL_DEF(88),
	MSPELL_DEF(89),
	MSPELL_DEF(90),
	MSPELL_DEF(91),
	MSPELL_DEF(92),
	MSPELL_DEF(93),
	MSPELL_DEF(94),
	MSPELL_DEF(95)
};

#undef MSPELL_MACRO_NULL
#undef UNIFORM_MSG
#undef BLIND_MSG
#undef SEEN_MSG

#undef MSPELL_SHRIEK
#undef MSPELL_ARROW_1
#undef MSPELL_ARROW_2
#undef MSPELL_ARROW_3
#undef MSPELL_ARROW_4
#undef MSPELL_BR_ACID
#undef MSPELL_BR_ELEC
#undef MSPELL_BR_FIRE
#undef MSPELL_BR_COLD
#undef MSPELL_BR_POIS
#undef MSPELL_BR_NETH
#undef MSPELL_BR_LITE
#undef MSPELL_BR_DARK
#undef MSPELL_BR_CONF
#undef MSPELL_BR_SOUN
#undef MSPELL_BR_CHAO
#undef MSPELL_BR_DISE
#undef MSPELL_BR_NEXU
#undef MSPELL_BR_TIME
#undef MSPELL_BR_INER
#undef MSPELL_BR_GRAV
#undef MSPELL_BR_SHAR
#undef MSPELL_BR_PLAS
#undef MSPELL_BR_WALL
#undef MSPELL_BOULDER

#undef MSPELL_BA_ACID
#undef MSPELL_BA_ELEC
#undef MSPELL_BA_FIRE
#undef MSPELL_BA_COLD
#undef MSPELL_BA_POIS
#undef MSPELL_BA_NETH
#undef MSPELL_BA_WATE
#undef MSPELL_BA_MANA
#undef MSPELL_BA_DARK
#undef MSPELL_DRAIN_MANA
#undef MSPELL_MIND_BLAST
#undef MSPELL_BRAIN_SMASH
#undef MSPELL_CAUSE_1
#undef MSPELL_CAUSE_2
#undef MSPELL_CAUSE_3
#undef MSPELL_CAUSE_4
#undef MSPELL_BO_ACID
#undef MSPELL_BO_ELEC
#undef MSPELL_BO_FIRE
#undef MSPELL_BO_COLD
#undef MSPELL_BO_NETH
#undef MSPELL_BO_WATE
#undef MSPELL_BO_MANA
#undef MSPELL_BO_PLAS
#undef MSPELL_BO_ICEE
#undef MSPELL_MISSILE
#undef MSPELL_SCARE
#undef MSPELL_BLIND
#undef MSPELL_CONF
#undef MSPELL_SLOW
#undef MSPELL_HOLD

#undef MSPELL_HASTE
#undef MSPELL_HEAL
#undef MSPELL_BLINK
#undef MSPELL_TPORT
#undef MSPELL_TELE_TO
#undef MSPELL_TELE_AWAY
#undef MSPELL_TELE_LEVEL
#undef MSPELL_DARKNESS
#undef MSPELL_TRAPS
#undef MSPELL_FORGET
#undef MSPELL_S_KIN
#undef MSPELL_S_HI_DEMON
#undef MSPELL_S_MONSTER
#undef MSPELL_S_MONSTERS
#undef MSPELL_S_ANIMAL
#undef MSPELL_S_SPIDER
#undef MSPELL_S_HOUND
#undef MSPELL_S_HYDRA
#undef MSPELL_S_ANGEL
#undef MSPELL_S_DEMON
#undef MSPELL_S_UNDEAD
#undef MSPELL_S_DRAGON
#undef MSPELL_S_HI_UNDEAD
#undef MSPELL_S_HI_DRAGON
#undef MSPELL_S_WRAITH
#undef MSPELL_S_UNIQUE

/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 *
 * Note the special "MFLAG_NICE" flag, which prevents a monster from using
 * any spell attacks until the player has had a single chance to move.
 */
static bool make_attack_spell(const m_idx_type m_idx)
{
	int thrown_spell, rlev;

	u32b f_attack[RACE_FLAG_SPELL_STRICT_UB];

	range_spec damage_spec[32 * RACE_FLAG_SPELL_STRICT_UB];

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	const bool blind = p_ptr->timed[TMD_BLIND] ;	/* Extract the blind-ness */
	const bool seen = (!blind && m_ptr->ml);	/* Extract the "see-able-ness" */

	/* Cannot cast spells when confused */
	if (m_ptr->confused) return (FALSE);

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	{
	/* Hack -- Extract the spell probability */
	const int chance = (r_ptr->freq_innate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);


	/* Only do spells occasionally */
	if (rand_int(100) >= chance) return (FALSE);
	}

	/* Hack -- require projectable player */
	/* Check range */
	if (m_ptr->cdis > MAX_RANGE) return (FALSE);

	/* Check path */
	if (!projectable(m_ptr->loc, p_ptr->loc)) return (FALSE);


	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* monster awareness of player state */
	if (r_ptr->flags[1] & RF1_STUPID)
	{
		m_ptr->smart = 0L;	/* paranoia */
	}
	else
	{
		if (OPTION(adult_smart_learn))
		{
			/* Hack -- Occasionally forget player status */
			/* remember to replace this with more message-based approach later */
			if (m_ptr->smart && one_in_(100)) m_ptr->smart = 0L;
		}

		/* Cheat if requested */
		if (OPTION(adult_smart_cheat))
		{
			/* Know weirdness */
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
			if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);

			/* Know immunities */
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);

			/* Know oppositions */
			if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
			if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
			if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
			if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
			if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);

			/* Know resistances */
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);

			/* XXX being blind is blindness resistance XXX */
			if (p_ptr->timed[TMD_BLIND]) m_ptr->smart |= (SM_RES_BLIND);
		}
	}

	/* Extract the racial spell flags */
	C_COPY(f_attack, r_ptr->spell_flags, RACE_FLAG_SPELL_STRICT_UB);
	init_spell_damage(f_attack, damage_spec, rlev, m_ptr->chp, m_ptr->smart);

	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags[1] & RF1_SMART) &&
	    (m_ptr->chp < m_ptr->mhp / 10) &&
	    one_in_(2))
	{
		/* Require intelligent spells */
		f_attack[0] &= RSF0_INT_MASK;
		f_attack[1] &= RSF1_INT_MASK;
		f_attack[2] &= RSF2_INT_MASK;

		/* No spells left */
		if (!f_attack[0] && !f_attack[1] && !f_attack[2]) return (FALSE);
	}

	/* Check whether summons and bolts are worth it. */
	if (!(r_ptr->flags[1] & RF1_STUPID))
	{
		/* Check for a clean bolt shot */
		if (((f_attack[0] & RSF0_BOLT_MASK) ||
			(f_attack[1] & RSF1_BOLT_MASK) ||
			(f_attack[2] & RSF2_BOLT_MASK)) &&
			!clean_shot(m_ptr->loc, p_ptr->loc))
		{
			/* Remove spells that will only hurt friends */
			f_attack[0] &= ~RSF0_BOLT_MASK;
			f_attack[1] &= ~RSF1_BOLT_MASK;
			f_attack[2] &= ~RSF2_BOLT_MASK;
		}

		/* Check for a possible summon */
		if (!(summon_possible(m_ptr->loc)))
		{
			/* Remove summoning spells */
			f_attack[0] &= ~RSF0_SUMMON_MASK;
			f_attack[1] &= ~RSF1_SUMMON_MASK;
			f_attack[2] &= ~RSF2_SUMMON_MASK;
		}

		/* No spells left */
		if (!f_attack[0] && !f_attack[1] && !f_attack[2]) return (FALSE);
	}

	/* Remove the "ineffective" spells */
	remove_bad_spells(m_idx, f_attack, damage_spec);

	/* No spells left */
	if (!f_attack[0] && !f_attack[1] && !f_attack[2]) return (FALSE);

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);


	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, f_attack);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);
	/* XXX handle this as a bootstrap test XXX */
	assert((SPELL_ORIGIN<=thrown_spell) && (SPELL_ORIGIN + 32 * RACE_FLAG_SPELL_STRICT_UB > thrown_spell) && (NULL != magic_attack_list[thrown_spell-SPELL_ORIGIN].m_spell_config));

	/* Check for spell failure (innate attacks never fail) */
	if (thrown_spell >= SPELL_ORIGIN + 32)
	{
		/* Calculate spell failure rate */
		int failrate = 25 - (rlev + 3) / 4;

		/* Hack -- Stupid monsters will never fail (for jellies and such) */
		if (!OPTION(adult_smart_monsters) || r_ptr->flags[1] & RF1_STUPID) failrate = 0;

		if ((0 >= failrate) || (rand_int(100) < failrate))
		{
			msg_format("%^s tries to cast a spell, but fails.", m_name);
			return (TRUE);
		}
	}

	/* Cast the spell. */
	disturb(1, 0);
	if (magic_attack_list[thrown_spell-SPELL_ORIGIN].sound_idx) sound(magic_attack_list[thrown_spell-SPELL_ORIGIN].sound_idx);
	if (blind)
	{
		if (NULL!=magic_attack_list[thrown_spell-SPELL_ORIGIN].blind_msg)
		{
			if (strstr(magic_attack_list[thrown_spell-SPELL_ORIGIN].blind_msg, "%^s "))
			{
				msg_format(magic_attack_list[thrown_spell-SPELL_ORIGIN].blind_msg, m_name);
			}
			else
			{
				msg_print(magic_attack_list[thrown_spell-SPELL_ORIGIN].blind_msg);
			}
		}
	}
	else if (!seen)
	{
		if (NULL!=magic_attack_list[thrown_spell-SPELL_ORIGIN].unseen_msg)
		{
			if (strstr(magic_attack_list[thrown_spell-SPELL_ORIGIN].unseen_msg, "%^s "))
			{
				msg_format(magic_attack_list[thrown_spell-SPELL_ORIGIN].unseen_msg, m_name);
			}
			else
			{
				msg_print(magic_attack_list[thrown_spell-SPELL_ORIGIN].unseen_msg);
			}
		}
	}
	else
	{
		if (NULL!=magic_attack_list[thrown_spell-SPELL_ORIGIN].seen_msg)
		{
			if (strstr(magic_attack_list[thrown_spell-SPELL_ORIGIN].seen_msg, "%^s "))
			{
				msg_format(magic_attack_list[thrown_spell-SPELL_ORIGIN].seen_msg, m_name);
			}
			else
			{
				msg_print(magic_attack_list[thrown_spell-SPELL_ORIGIN].blind_msg);
			}
		}
	};
	(magic_attack_list[thrown_spell-SPELL_ORIGIN].m_spell_config)(m_idx);


	/* Remember what the monster did to us */
	if (seen)
	{
		SET_FLAG(l_ptr->flags, thrown_spell);
		/* Innate spell */
		if (thrown_spell < SPELL_ORIGIN+32)
		{
			if (l_ptr->cast_innate < UCHAR_MAX) l_ptr->cast_innate++;
		}

		/* Bolt, Ball, or Special spell */
		else
		{
			if (l_ptr->cast_spell < UCHAR_MAX) l_ptr->cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_S16B)) l_ptr->deaths++;


	/* A spell was cast */
	return (TRUE);
}

#if 0
/* 
 * initializes an estimate of how much "attack power" can be brought to bear on a grid.
 */
static void mon_attack_power(coord g,int& dam_min, int& dam_median, int& dam_max)
{
	/* base initialization */
	dam_min = 0;
	dam_median = 0;
	dam_max = 0;

	/*! \todo melee check */
	/*! \todo spells/innate check */
}
#endif

/*
 * Visibility estimator.  Ignores detection spells.
 * Weird mind monsters are presumed to brazenly assume indetectability.
 * Synchronize against: update_mon
 */
static bool player_would_see_monster(const m_idx_type m_idx,coord p_loc, coord m_loc)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();

	/* Compute distance */
	int	d = distance(p_loc.y,p_loc.x,m_loc.y,m_loc.x);

	/* Restrict distance */
	if (d > 255) d = 255;

	/* Nearby */
	if (d <= MAX_SIGHT)
	{
		/* Basic telepathy */
		if (p_ptr->telepathy)
		{
			/* Normal mind, allow telepathy */
			if (!(r_ptr->flags[1] & (RF1_EMPTY_MIND | RF1_WEIRD_MIND))) return TRUE;
		}

		/* Normal line of sight, and not blind */
		if (los(p_loc,m_loc) && !p_ptr->timed[TMD_BLIND])
		{
			/* Use "infravision" */
			if (d <= p_ptr->see_infra)
			{
				if (!(r_ptr->flags[1] & RF1_COLD_BLOOD)) return TRUE;
			}

			else if ((d <= p_ptr->cur_lite) || (cave_info[m_loc.y][m_loc.x] & CAVE_GLOW)) return TRUE;
		}
	}
	return FALSE;
}

static bool square_momentarily_unseen_by_player(const m_idx_type m_idx,coord m_loc,coord p_loc,int player_moves)
{
	if (player_would_see_monster(m_idx,p_loc,m_loc)) return FALSE;
	if (0<player_moves)
	{
		int i = 0;
		do	{	/* not that efficient at 2<=player_moves, and could be rewritten in terms of flow.c */
			coord test(p_loc);
			test += dd_coord_ddd[i];
			if (!in_bounds_fully(test.y,test.x)) continue;	/* irrational caution */
			if (!cave_empty_bold(test.y,test.x)) continue;
			if (!square_momentarily_unseen_by_player(m_idx,m_loc,test,player_moves-1)) return FALSE;
			}
		while(KEYPAD_DIR_MAX > ++i);
	};
	return TRUE;
}

static bool player_could_attack_monster(coord p_loc, coord m_loc)
{
	if (0 < p_ptr->timed[TMD_PARALYZED]) return FALSE;	/* paralyzed is no attack */
	if (100 <= p_ptr->timed[TMD_STUN]) return FALSE;	/* knocked out is no attack */

	const int d = distance(p_loc.y,p_loc.x,m_loc.y,m_loc.x);	/* Compute distance */
	if (1>=d) return TRUE;										/* melee is attackable */

	/* otherwise, walls provide complete immunity even to LOS spells */
	if (cave_info[m_loc.y][m_loc.x] & (CAVE_WALL)) return FALSE;

	/* XXX if player has an LoS effect, should count as attack-capable; might need cheat-AI XXX */
	/* XXX if player has a ball effect, should count as attack-capable; might need cheat-AI XXX */
	/* XXX if player has reasonable throwing weapons, should count as attack-capable; might need cheat-AI XXX */

	/* simple check for missile launcher; synchronize against do_cmd_fire */
	if (p_ptr->inventory[INVEN_BOW].obj_id.tval && d>=(10 + 5*p_ptr->ammo_mult) && projectable(p_loc,m_loc)) return TRUE;	

	/* no attacks */
	return FALSE;
}

/*
 *
 */
static bool square_momentarily_safe_from_player(coord m_loc,coord p_loc,int player_moves)
{
	if (player_could_attack_monster(p_loc,m_loc)) return FALSE;
	if (0<player_moves)
	{
		int i = 0;
		do	{	/* not that efficient at 2<=player_moves, and could be rewritten in terms of flow.c */
			coord test(p_loc);
			test += dd_coord_ddd[i];
			if (!in_bounds_fully(test.y,test.x)) continue;	/* irrational caution */
			if (!cave_empty_bold(test.y,test.x)) continue;
			if (!square_momentarily_safe_from_player(m_loc,test,player_moves-1)) return FALSE;
			}
		while(KEYPAD_DIR_MAX > ++i);
	};
	return TRUE;
}

#if 0
/*
 *  Returns whether a given monster is useless.  Here to make it easy to synchronize against mon_will_run.
 */
bool
monster_type::mon_is_useless(void)
{
	monster_race* const r_ptr = race();

	/*! \bug anything that can kill the player in one attack is useful (alternately, terrifying melee attack) */
	/*! \bug anything that can swamp the player is useful */

	/* generically wants to close */
	{	/* C-ish blocking brace */
	u16b p_lev = p_ptr->lev;							/* Examine player power (level) */
	u16b m_lev = r_ptr->level + 8 + 25;	/* Examine monster power (level plus morale) */

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	{	/* C-ish blocking brace */
	/* Examine player health */
//	u16b p_chp = p_ptr->chp;
	u16b p_mhp = p_ptr->mhp;

	/* Examine monster health */
//	u16b m_chp = chp;
	u32b m_mhp = mhp;

	/* Prepare to optimize the calculation */
//	u16b p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
//	u32b m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */
	u16b p_val = (p_lev * p_mhp) + (p_mhp << 2);	/* div p_mhp */
	u32b m_val = (m_lev * m_mhp) + (m_mhp << 2);	/* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp <= m_val * p_mhp) return (TRUE);
	}	/* end blocking brace */
	}	/* end blocking brace */
	return FALSE;
}

/*
 *  Returns whether a given monster race is useless.  Here to make it easy to synchronize against mon_will_run.
 */
bool
monster_race::race_is_useless(void)
{
	/*! \bug anything that can kill the player in one attack is useful (alternately, terrifying melee attack) */
	/*! \bug anything that can swamp the player is useful */

	/* generically wants to close */
	{	/* C-ish blocking brace */
	u16b p_lev = p_ptr->lev;							/* Examine player power (level) */
	u16b m_lev = level + 8 + 25;	/* Examine monster power (level plus morale) */

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	{	/* C-ish blocking brace */
	/* Examine player health */
//	u16b p_chp = p_ptr->chp;
	u16b p_mhp = p_ptr->mhp;

	/* Examine monster health */
//	u16b m_chp = chp;
	u32b m_mhp = h.maxroll();

	/* Prepare to optimize the calculation */
//	u16b p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
//	u32b m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */
	u16b p_val = (p_lev * p_mhp) + (p_mhp << 2);	/* div p_mhp */
	u32b m_val = (m_lev * m_mhp) + (m_mhp << 2);	/* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp <= m_val * p_mhp) return (TRUE);
	}	/* end blocking brace */
	}	/* end blocking brace */
	return FALSE;
}
#endif

/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static bool mon_will_run(const m_idx_type m_idx)
{
	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

	/* KBB: do not run if the player can't attack you anyway */
	/* assumes player gets 1 move; not so good against fast players */
	{
	int mon_moves;
	int player_moves;
	m_ptr->move_ratio(player_moves, mon_moves, *p_ptr, 0, 1);	/* pessimize */
	if (square_momentarily_safe_from_player(m_ptr->loc,p_ptr->loc,player_moves)) return FALSE;
	}

#if 0
	{
	int m_min, m_median, m_max;
	int pmelee_min, pmelee_median, pmelee_max;
	int pmissile_min, pmissile_median, pmissile_max;
	int mhp = m_ptr->chp;
	int php = p_ptr->chp;

	/* actually should be target-aware */
	m_ptr->melee_analyze(m_min,m_median,m_max,p_ptr->loc);
	p_ptr->melee_analyze(m_ptr,pmelee_min,pmelee_median,pmelee_max);
	p_ptr->missile_analyze(m_ptr,pmissile_min,pmissile_median,pmissile_max);

	if ((1 == m_ptr->cdis) && (m_max >= p_ptr->chp)) return FALSE;
	}
#else
	if (1 == m_ptr->cdis)
	{	/* KBB: check for melee instakill chance; do not run if we have it */
		int m_min, m_median, m_max;
		m_ptr->melee_analyze(m_min,m_median,m_max,p_ptr->loc);
		if (m_max > p_ptr->apparent_health()) return FALSE;
	}

	/* KBB: do not run if the player is swamped */
	{
	long total_min = 0;
	long total_median = 0;
	long total_max = 0;
	int next_to_player = 0;
	int m_idx2;
	for(m_idx2 = 1; m_idx2 < mon_max ; ++m_idx2)
	{
		int m_min, m_median, m_max;

		if (!mon_list[m_idx2].r_idx) continue;	/* no monster in slot */
		if (mon_list[m_idx2].csleep) continue;	/* sleeping monsters don't attack */

		if (1==mon_list[m_idx2].cdis) ++next_to_player;

		m_ptr->melee_analyze(m_min,m_median,m_max,p_ptr->loc);
		total_min += m_min;
		total_median += m_median;
		total_max += m_max;
	}
	/* XXX overly simple player swamping estimate XXX */
	if (total_median*next_to_player*(next_to_player-1) > 2*p_ptr->apparent_health()) return FALSE;
	}
#endif

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

	{	/* C-ish blocking brace */
	u16b p_lev = p_ptr->lev;							/* Examine player power (level) */
	u16b m_lev = r_ptr->level + (m_idx & 0x08) + 25;	/* Examine monster power (level plus morale) */

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	{	/* C-ish blocking brace */
	/* Examine player health */
	u16b p_chp = p_ptr->chp;
	u16b p_mhp = p_ptr->mhp;

	/* Examine monster health */
	u16b m_chp = m_ptr->chp;
	u16b m_mhp = m_ptr->mhp;

	/* Prepare to optimize the calculation */
	u16b p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
	u16b m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);
	}	/* end blocking brace */
	}	/* end blocking brace */

	/* Assume no terror */
	return (FALSE);
}

/*
 * Hack -- compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(const monster_type *m_ptr, const monster_type *n_ptr)
{
	u32b mexp1 = m_ptr->race()->mexp;
	u32b mexp2 = n_ptr->race()->mexp;

	/* Compare */
	if (mexp1 < mexp2) return (-1);
	if (mexp1 > mexp2) return (1);

	/* Assume equal */
	return (0);
}

static bool nodoor_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	/* Skip locations in a wall */
	if (!cave_floor_bold(g.y, g.x)) return FALSE;

	/* can't handle doors */
	if (cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL) ||
	     (cave_feat[g.y][g.x] == FEAT_SECRET))
		return FALSE;

	return TRUE;
}

static bool normal_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	/* Skip locations in a wall */
	if (!cave_floor_bold(g.y, g.x)) return FALSE;

	return TRUE;
}

static bool wallpasser_move_legal(coord g)
{
	/* Skip illegal locations */
	if (!in_bounds_fully(g.y, g.x)) return FALSE;

	return TRUE;
}

static int find_target_dir(const m_idx_type m_idx,coord g)
{
	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags[0] & RF0_NEVER_MOVE) return 0;

	{	/* C-style blocking brace */
	int dir_stack[KEYPAD_DIR_MAX];
	int dis_stack[KEYPAD_DIR_MAX];
	bool bool_stack[KEYPAD_DIR_MAX];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) ? wallpasser_move_legal
									: ((r_ptr->flags[1] & (RF1_OPEN_DOOR | RF1_BASH_DOOR)) ? normal_move_legal : nodoor_move_legal)),
						NULL,&g);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags[1] & RF1_KILL_BODY)) continue;	/* oops */
				{	/* C-ish blocking brace */
				if (compare_monsters(m_ptr, m_ptr_from_m_idx(cave_m_idx[n.y][n.x])) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* stupid monsters do not move at all intelligently */
	if (!(r_ptr->flags[1] & RF1_STUPID))
	{
		/* cover: deny player attacks */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_could_attack_monster(p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* concealment: deny player visibility */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_would_see_monster(m_idx,p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);	
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* maximize distance from player when closing in */
		i = dir_stack_UB;
		do	{
			--i;
			dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
			while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
			{
				if (dis_stack[i]>dis_stack[i+1])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* final tie-breaker: prefer orthogonal directions (diagonals are 150 energy cost in Zaiband) */
		/* relying on above being order-preserving to work */
		if (KEYPAD_CARDINAL_DIR_MAX<=dir_stack[dir_stack_UB-1])
		{
			i = dir_stack_UB-1;
			do 	if (KEYPAD_CARDINAL_DIR_MAX>dir_stack[--i])
					{
					dir_stack_UB = i+1;
					break;
					}
			while(0<i);
		}
	}

	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are never allowed to "flow",
 * since they should move directly towards the player.
 *
 * Prefer "non-diagonal" directions, but twiddle them a little
 * to angle slightly towards the player's actual location.
 *
 * Allow very perceptive monsters to track old "spoor" left by
 * previous locations occupied by the player.  This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly.  I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static int get_move_dir_aux(const m_idx_type m_idx)
{
	/* Monster flowing disabled */
	if (!OPTION(adult_flow_by_sound)) return 0;

	{	/* C-style blocking braces */
	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();
	
	int i;
	int best_i = 0;
	int when = 0;
	int cost = 999;

	/* Monster can go through rocks */
	if (r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) return 0;

	/* Monster never moves */
	if (r_ptr->flags[0] & RF0_NEVER_MOVE) return 0;

	/* Monster can find player anyway */
	i = find_target_dir(m_idx,p_ptr->loc);
	if (i) return i;

	/* The player is not currently near the monster grid */
	if (cave_when[m_ptr->loc.y][m_ptr->loc.x] < cave_when[p_ptr->loc.y][p_ptr->loc.x])
	{
		/* The player has never been near the monster grid */
		if (cave_when[m_ptr->loc.y][m_ptr->loc.x] == 0) return 0;

		/* The monster is not allowed to track the player */
		if (!OPTION(adult_flow_by_smell)) return 0;
	}

	/* Monster is too far away to notice the player */
	if (cave_cost[m_ptr->loc.y][m_ptr->loc.x] > MONSTER_FLOW_DEPTH) return 0;
	if (cave_cost[m_ptr->loc.y][m_ptr->loc.x] > r_ptr->aaf) return 0;

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x)) return 0;

	/* Check nearby grids, diagonals first */
	for (i = KEYPAD_DIR_MAX-1; i >= 0; i--)
	{
		/* Get the location */
		coord tmp(m_ptr->loc);
		tmp += dd_coord_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[tmp.y][tmp.x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[tmp.y][tmp.x] < when) continue;

		/* Ignore distant locations */
		if (cave_cost[tmp.y][tmp.x] > cost) continue;

		/* Save the cost and time */
		when = cave_when[tmp.y][tmp.x];
		cost = cave_cost[tmp.y][tmp.x];
		best_i = i;
	}

	/* No flowed move */
	if (!when) return 0;

	/* Success */
	return ddd[best_i];
	}	/* end blocking brace */
}

static bool player_no_los(coord g)
{
	return !player_has_los_bold(g.y,g.x);
}

static int find_safety_dir(const m_idx_type m_idx)
{
	monster_type* m_ptr = &mon_list[m_idx];
	monster_race* r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags[0] & RF0_NEVER_MOVE) return 0;

	{	/* C-style blocking brace */
	int dir_stack[KEYPAD_DIR_MAX];
	int dis_stack[KEYPAD_DIR_MAX];
	bool bool_stack[KEYPAD_DIR_MAX];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) ? wallpasser_move_legal
									: ((r_ptr->flags[1] & (RF1_OPEN_DOOR | RF1_BASH_DOOR)) ? normal_move_legal : nodoor_move_legal)),
						player_no_los,NULL);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags[1] & RF1_KILL_BODY)) continue;	/* oops */
				{	/* C-ish blocking brace */
				if (compare_monsters(m_ptr, m_ptr_from_m_idx(cave_m_idx[n.y][n.x])) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* maximize distance from player when fleeing */
	i = dir_stack_UB;
	do	{
		--i;
		dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
		while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
		{
			if (dis_stack[i]>dis_stack[i+1])
				dir_stack_UB = i+1;
			else
			{
				if (i+2<dir_stack_UB)
				{
					memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
				}
				--dir_stack_UB;
			}
		}
		}
	while(0<i);
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* stupid monsters do not move at all intelligently */
	if (!(r_ptr->flags[1] & RF1_STUPID))
	{
		/* cover: deny player attacks */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_could_attack_monster(p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* concealment: deny player visibility */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_would_see_monster(m_idx,p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);	
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* final tie-breaker: prefer orthogonal directions (diagonals are 150 energy cost in Zaiband) */
		/* relying on above being order-preserving to work */
		if (KEYPAD_CARDINAL_DIR_MAX<=dir_stack[dir_stack_UB-1])
		{
			i = dir_stack_UB-1;
			do 	if (KEYPAD_CARDINAL_DIR_MAX>dir_stack[--i])
					{
					dir_stack_UB = i+1;
					break;
					}
			while(0<i);
		}
	}
	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

static int find_hiding_dir(const m_idx_type m_idx)
{
	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();

	/* Monster never moves */
	if (r_ptr->flags[0] & RF0_NEVER_MOVE) return 0;

	{	/* C-style blocking brace */
	int dir_stack[KEYPAD_DIR_MAX];
	int dis_stack[KEYPAD_DIR_MAX];
	bool bool_stack[KEYPAD_DIR_MAX];
	size_t dir_stack_UB = 0;
	size_t i;

	byte access_map[(2*10+1)*(2*10+1)];

	flow_from(access_map,10,m_ptr->loc,((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) ? wallpasser_move_legal
									: ((r_ptr->flags[1] & (RF1_OPEN_DOOR | RF1_BASH_DOOR)) ? normal_move_legal : nodoor_move_legal)),
						player_no_los,NULL);

	for(i = 0; i<KEYPAD_DIR_MAX; ++i)
	{
		if (0!=access_map[distance_square_idx(dd_coord_ddd[i],10)])
		{	/* check for ability to move through other monsters here */
			coord n = m_ptr->loc;
			n += dd_coord_ddd[i];
			if (cave_m_idx[n.y][n.x] > 0)
			{
				if (!(r_ptr->flags[1] & RF1_KILL_BODY)) continue;	/* oops */
				{	/* C-ish blocking brace */
				if (compare_monsters(m_ptr, m_ptr_from_m_idx(cave_m_idx[n.y][n.x])) <= 0) continue;	/* no-go */				
				}	/* end blocking brace */
			}
			dir_stack[dir_stack_UB++] = i;
		}
	}

	/* if did not find safety, fail now */
	if (0 == dir_stack_UB) return 0;
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* minimize distance from player when hiding */
	i = dir_stack_UB;
	do	{
		--i;
		dis_stack[i] = distance(m_ptr->loc.y+ddy[dir_stack[i]],m_ptr->loc.x+ddx[dir_stack[i]],p_ptr->loc.y,p_ptr->loc.x);
		while(i+1<dir_stack_UB && dis_stack[i]!=dis_stack[i+1])
		{
			if (dis_stack[i]<dis_stack[i+1])
				dir_stack_UB = i+1;
			else
			{
				if (i+2<dir_stack_UB)
				{
					memmove(dis_stack+i,dis_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
				}
				--dir_stack_UB;
			}
		}
		}
	while(0<i);
	if (1 == dir_stack_UB) return ddd[dir_stack[0]];

	/* stupid monsters do not move at all intelligently */
	if (!(r_ptr->flags[1] & RF1_STUPID))
	{
		/* cover: deny player attacks */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_could_attack_monster(p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* concealment: deny player visibility */
		i = dir_stack_UB;
		do	{
			--i;
			bool_stack[i] = player_would_see_monster(m_idx,p_ptr->loc,m_ptr->loc+dd_coord_ddd[i]);
			while(i+1<dir_stack_UB && bool_stack[i]!=bool_stack[i+1])
			{
				if (!bool_stack[i])
					dir_stack_UB = i+1;
				else
				{
					if (i+2<dir_stack_UB)
					{
						memmove(bool_stack+i,bool_stack+i+1,(dir_stack_UB-(i+2))*sizeof(bool));
						memmove(dir_stack+i,dir_stack+i+1,(dir_stack_UB-(i+2))*sizeof(int));
					}
					--dir_stack_UB;
				}
			}
			}
		while(0<i);	
		if (1 == dir_stack_UB) return ddd[dir_stack[0]];

		/* final tie-breaker: prefer orthogonal directions (diagonals are 150 energy cost in Zaiband) */
		/* relying on above being order-preserving to work */
		if (KEYPAD_CARDINAL_DIR_MAX<=dir_stack[dir_stack_UB-1])
		{
			i = dir_stack_UB-1;
			do 	if (KEYPAD_CARDINAL_DIR_MAX>dir_stack[--i])
					{
					dir_stack_UB = i+1;
					break;
					}
			while(0<i);
		}
	}

	return ddd[dir_stack[rand_int(dir_stack_UB)]];
	}	/* end blocking brace */
}

#if 0
/*
 * Hack -- Precompute a bunch of calls to distance() in find_safety() and
 * find_hiding().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be reduced by using char.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */


static const int d_off_y_0[] =
{ 0 };

static const int d_off_x_0[] =
{ 0 };


static const int d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static const int d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static const int d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static const int d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static const int d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static const int d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };


static const int d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static const int d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };


static const int d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static const int d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };


static const int d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static const int d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };


static const int d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static const int d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };


static const int d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static const int d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };


static const int d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static const int d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };


static const int *dist_offsets_y[10] =
{
	d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
	d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static const int *dist_offsets_x[10] =
{
	d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
	d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};
#endif

/* XXX need a better formalism XXX */
/* template function on RHS? */
static bool check_for_glyph(coord g)
{
	return cave_feat[g.y][g.x] == FEAT_GLYPH;
}

static bool check_for_door(coord g)
{
	return cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL) ||
		         (cave_feat[g.y][g.x] == FEAT_SECRET);
}

/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(const m_idx_type m_idx, int mm[5])
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	monster_type* const m_ptr = &mon_list[m_idx];
	monster_race* const r_ptr = m_ptr->race();

	bool done = FALSE;

	int flow_dir = get_move_dir_aux(m_idx);

	if (!flow_dir)
	{
		flow_dir = 5;
		if (m_ptr->loc.x!=p_ptr->loc.x)
		{
			flow_dir += (m_ptr->loc.x<p_ptr->loc.x) ? 1 : -1;
		}
		if (m_ptr->loc.y!=p_ptr->loc.y)
		{
			flow_dir += (m_ptr->loc.y<p_ptr->loc.y) ? -3 : 3;
		}
	}


	/* Normal animal packs try to get the player out of corridors. */
	/* KBB: also count arbitrary floor grids (AI problem reported in some variant) */
	if (OPTION(adult_smart_packs) &&
	    (r_ptr->flags[0] & RF0_FRIENDS) && (r_ptr->flags[2] & RF2_ANIMAL) &&
	    !((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL))))
	{
		int i, room = 0, floor_count = 0;

		/* Count room grids next to player */
		for (i = 0; i < 8; i++)
		{
			coord test(p_ptr->loc);
			test += dd_coord_ddd[i];
			/* Check for room grid */
			if (cave_info[test.y][test.x] & (CAVE_ROOM)) ++room;
			if (cave_floor_bold(test.y, test.x)) ++floor_count;
		}

		/* Not in a room and strong player */
		if (((room < 8) && (floor_count < 8)) && (p_ptr->chp > p_ptr->mhp / 2))
		{
			/* Find hiding place */
			int hide_dir = find_hiding_dir(m_idx);
			if (hide_dir)
			{
				flow_dir = hide_dir;
				done = TRUE;
			}
		}
	}

	/* Apply fear */
	if (!done && mon_will_run(m_idx))
	{
		/* Try to find safe place */
		int flee_dir = (OPTION(adult_smart_monsters)) ? find_safety_dir(m_idx) : 0;
		if (flee_dir)
		{
			flow_dir = flee_dir;
		}
		else
		{
			flow_dir = 10-flow_dir;	/* inverts keypad direction */
		};

		done = TRUE;
	}

	/* Monster groups try to surround the player */
	if (!done && OPTION(adult_smart_packs) && (r_ptr->flags[0] & RF0_FRIENDS))
	{
		/* 
		 * Zaiband: if monster #1 is 1-away from player, and there is 
		 * a monster #2 2-away from player that is 1-away from #1 that has 
		 * no open square 1-away to move to then monster #1 should move into an empty-square
         */
		if (1==m_ptr->cdis)
		{
			int i;
			int near_escapes = 0;
			int near_monsters = 0;
			byte near_escape_bearing[4];		/* some sort of static stack needed here */
			byte near_monster_bearing[5];

			/* determine if pack AI should activate */
			for (i = 0; i < KEYPAD_DIR_MAX; i++)
			{
				coord tmp(m_ptr->loc);
				tmp += dd_coord_ddd[i];

				if (0 > cave_m_idx[tmp.y][tmp.x]) continue;	/* player here */
				else if (0 < cave_m_idx[tmp.y][tmp.x])
				{	/** \bug only monsters that this monster doesn't have a double-move on should count */
					const monster_type* const n_ptr = m_ptr_from_m_idx(cave_m_idx[tmp.y][tmp.x]);
					if (	2==n_ptr->cdis
						/* no-move monsters don't count */
						&& 	!(n_ptr->race()->flags[0] & RF0_NEVER_MOVE))
					{
						bool has_path = FALSE;
						int j;
						for (j = 0; j < KEYPAD_DIR_MAX; j++)
						{
							coord tmp2(tmp);
							tmp2 += dd_coord_ddd[j];
							if (0 < cave_m_idx[tmp2.y][tmp2.x]) continue;
							if (1<distance(tmp2.y, tmp2.x, py, px)) continue;
							if (cave_floor_bold(tmp2.y,tmp2.x) || (n_ptr->race()->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)))
							{
								has_path = TRUE;
								break;
							};
						};
						if (!has_path) near_monster_bearing[near_monsters++] = i;
					}
				}
				else if (	(cave_floor_bold(tmp.y,tmp.x) || (r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)))
						  && 1==distance(tmp.y,tmp.x,py,px))
				{
					near_escape_bearing[near_escapes++] = i;
				}
			}
			if (near_escapes && near_monsters)
			{
				flow_dir = (1==near_escapes) ? near_escape_bearing[0] : near_escape_bearing[rand_int(near_escapes)];
			}
		}
	}

	if (!flow_dir) return FALSE;

	mm[0] = flow_dir;
	if (one_in_(2))
	{
		mm[1] = cycle[chome[mm[0]]+1];
		mm[2] = cycle[chome[mm[0]]-1];
		mm[3] = cycle[chome[mm[0]]+2];
		mm[4] = cycle[chome[mm[0]]-2];
	}
	else
	{
		mm[1] = cycle[chome[mm[0]]-1];
		mm[2] = cycle[chome[mm[0]]+1];
		mm[3] = cycle[chome[mm[0]]-2];
		mm[4] = cycle[chome[mm[0]]+2];
	}

	{
	/* put illegal squares last */
	int goodUB = bad_squares_last(mm, ((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) ? wallpasser_move_legal
									: (r_ptr->flags[1] & (RF1_OPEN_DOOR | RF1_BASH_DOOR)) ? normal_move_legal : nodoor_move_legal),5,m_ptr->loc);

	if (2<=goodUB)
	{
		/* doors are expensive to open, try to bypass them.  They aren't as bad as glyphs of warding. */
		bad_squares_last(mm, check_for_door, goodUB, m_ptr->loc);
	
		/* try glyphs of warding last */
		bad_squares_last(mm, check_for_glyph, goodUB, m_ptr->loc);
	}
	}

	/* Want to move */
	return (TRUE);
}



/*
 * ZaiBand: Meta-code for enhanced jellies/molds
 * typedef for selective_destroy_ok must interoperate 
 * with set_acid_destroy in spells1.c
 */

typedef bool selective_destroy_helper(object_type *o_ptr, monster_type *m_ptr);

static bool eat_food_floor_ok(const object_type *o_ptr)
{
	if (   o_ptr->obj_id.tval == TV_FOOD
		&& o_ptr->obj_id.sval >= SV_FOOD_CURE_POISON)
		return (TRUE);
	return (FALSE);
}

/* XXX would like template function here XXX */
static bool non_money_ok(const object_type *o_ptr)
{
	return TV_GOLD != o_ptr->obj_id.tval;
}

static bool eat_lite_ok(const object_type *o_ptr)
{
	if (   o_ptr->obj_id.tval == TV_LITE
		&& 0<o_ptr->pval
		&& !o_ptr->is_artifact())
		return (TRUE);
	return (FALSE);
}

static bool eat_food_floor_helper(object_type *o_ptr, monster_type *m_ptr)
{
	if 		(o_ptr->obj_id.sval == SV_FOOD_CURE_PARANOIA && m_ptr->monfear)
		{	/* cure fear */
		m_ptr->monfear = 1;		/* XXX -- don't want to clone code */
		}
	else if (o_ptr->obj_id.sval == SV_FOOD_CURE_CONFUSION && m_ptr->confused)
		{	/* cure confusion */
		m_ptr->confused = 1;	/* XXX -- don't want to clone code */
		}
	return TRUE;
}

static bool take_item_helper(object_type *o_ptr, monster_type *m_ptr)
{
	object_type tmp = *o_ptr;
	monster_carry(m_ptr - mon_list, &tmp);			/* Carry the object */
	return TRUE;
}

static bool eat_lite_helper(object_type *o_ptr, monster_type *m_ptr)
{
	(void)(m_ptr);		/* unused parameter */
	o_ptr->pval = 0;	/* no light left! */
	return FALSE;
}

static bool dont_destroy_this(object_type *o_ptr, monster_type *m_ptr)
{
	if (o_ptr->is_artifact()) return TRUE;	/* artifacts are indestructible */

	if (NULL!=m_ptr)	/* monster is considered relevant to this test */
		{
		u32b f[OBJECT_FLAG_STRICT_UB];
		monster_race *r_ptr = m_ptr->race();
		object_flags(o_ptr, f); /* extract some flags */

		if (0L!=r_ptr->flags[2] && 0L!=f[0])
			{	/* React to objects that hurt the monster */
			u32b flg2 = 0L;

			if (f[0] & (TR1_SLAY_DRAGON | TR1_KILL_DRAGON)) flg2 |= RF2_DRAGON;
			if (f[0] & (TR1_SLAY_TROLL)) flg2 |= RF2_TROLL;
			if (f[0] & (TR1_SLAY_GIANT)) flg2 |= RF2_GIANT;
			if (f[0] & (TR1_SLAY_ORC)) flg2 |= RF2_ORC;
			if (f[0] & (TR1_SLAY_DEMON | TR1_KILL_DEMON)) flg2 |= RF2_DEMON;
			if (f[0] & (TR1_SLAY_UNDEAD | TR1_KILL_UNDEAD)) flg2 |= RF2_UNDEAD;
			if (f[0] & (TR1_SLAY_ANIMAL)) flg2 |= RF2_ANIMAL;
			if (f[0] & (TR1_SLAY_EVIL)) flg2 |= RF2_EVIL;

			if (r_ptr->flags[2] & flg2) return TRUE;
			}
		}
	return FALSE;
}

/* indestructible message: e.g., "%^s tries to eat %s, but fails." */
/* destructible message: e.g., "%^s eats %s." */
static bool destroy_items_selectively(s16b this_o_idx, monster_type *m_ptr, bool has_los,
	const char* const indestructible_message, const char* const destructible_message, 
	o_ptr_test* test_func, selective_destroy_helper* test_helper)
{
	char m_name[80];
	bool did_something = (FALSE);
	if (!this_o_idx) return (FALSE);

	monster_desc(m_name, sizeof(m_name), m_ptr, MDESC_IND1);	/* Get the monster name */

	do	{
		char o_name[80];
		object_type *o_ptr = &o_list[this_o_idx];
		s16b next_o_idx = o_ptr->next_o_idx;		/* Get the next object */

		if (NULL==test_func || test_func(o_ptr))
		{	/* approve item as target for selective destruction */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);	/* Get the object name */

			/* The object cannot be picked up by the monster */
			if (dont_destroy_this(o_ptr,m_ptr))
			{	/* Describe observable situations */
				if (m_ptr->ml && has_los && NULL!=indestructible_message)
				{	/* Dump a message */
					msg_format(indestructible_message,m_name, o_name);
					did_something = (TRUE);
				}
			}

			/* Eat the item */
			else
			{	/* Describe observable situations */
				if (has_los)
				{	/* Dump a message */
					msg_format(destructible_message,m_name, o_name);
					did_something = (TRUE);
				}

				/* item may affect monster */
				if (NULL==test_helper || test_helper(o_ptr,m_ptr))
					{
					delete_object_idx(this_o_idx);	/* delete the object */
					}
			}
		}

		this_o_idx = next_o_idx;
		}	
	while(this_o_idx);
	return did_something;
}

void
monster_type::wake_up()
{
	/* reality check */
	assert(0<csleep);

	csleep = 0;	/* Reset sleep counter */
	energy = 0;	/* Start with no energy */

	/* Notice the "waking up" */
	if (ml)
	{
		char m_name[80];

		/* Get the monster name */
		monster_desc(m_name, sizeof(m_name), this, 0);

		/* Dump a message */
		msg_format("%^s wakes up.", m_name);

		/* Hack -- Update the health bar */
		if (p_ptr->health_who == (this-mon_list)) p_ptr->redraw |= (PR_HEALTH);

		/* Hack -- Count the wakings */
		if (lore()->wake < UCHAR_MAX) lore()->wake++;
	}
}

void
monster_type::disturb(int d)
{
	/* reality check */
	assert(0<csleep);

	/* minimum disturbance level */
	if (0>=d) d = 1;

	/* Still asleep */
	if (csleep > d)
	{
		/* Monster wakes up "a little bit" */
		csleep -= d;

		/* Notice the "not waking up" */
		if (ml)
		{
			/* Hack -- Count the ignores */
			if (lore()->ignore < UCHAR_MAX) lore()->ignore++;
		}
	}

	/* Just woke up */
	else
	{
		wake_up();
	}
}

/*
 * Process a monster
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * In addition, monsters which *cannot* open or bash down a door
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(const m_idx_type m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = m_ptr->race();
	monster_lore *l_ptr = m_ptr->lore();

	int i;

	int mm[5];

	coord o,n;

	u32b learn_RF1 = 0;	/* learn flags */

	bool stagger = FALSE;
	bool do_turn = FALSE;
	bool do_move = FALSE;
	bool do_view = FALSE;

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		/* Aggravation */
		if (p_ptr->aggravate)
		{
			m_ptr->wake_up();
		}

		/* Zaiband: Player is in line-of-sight */
		else if ((m_ptr->cdis <= r_ptr->aaf) && los(m_ptr->loc,p_ptr->loc))
		{
			/* Wake up faster near the player */
			/* cf. noise code in dungeon.c; Intensity 1, distance is cached */
			m_ptr->disturb(100/m_ptr->cdis);
		}

		/* as the monster is either asleep or groggy, don't do anything more this turn */
		return;
	}


	/* Get the origin */
	o = m_ptr->loc;

	/* Attempt to "multiply" if able and allowed */
	if ((r_ptr->flags[1] & RF1_MULTIPLY) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = o.y - 1; y <= o.y + 1; y++)
		{
			for (x = o.x - 1; x <= o.x + 1; x++)
			{
				/* Count monsters */
				if (cave_m_idx[y][x] > 0) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || one_in_(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->flags[1] |= RF1_MULTIPLY;

					sound(MSG_MULTIPLY);	/* Make a sound */
				}

				return;	/* Multiplying takes energy */
			}
		}
	}


	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;


	/* Confused: Stagger */
	if (m_ptr->confused)
	{
		/* Stagger */
		stagger = TRUE;
	}

	/* Random movement */
	else if (r_ptr->flags[0] & (RF0_RAND_50 | RF0_RAND_25))
	{
		/* Random movement (25%) */
		if (!(r_ptr->flags[0] & RF0_RAND_50))
		{
			/* Random */
			if (rand_int(100) < 25)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->flags[0] |= RF0_RAND_25;

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (50%) */
		else if (!(r_ptr->flags[0] & RF0_RAND_25))
		{
			/* Random */
			if (rand_int(100) < 50)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->flags[0] |= RF0_RAND_50;

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (75%) */
		else
		{
			/* Random */
			if (rand_int(100) < 75)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->flags[0] |= (RF0_RAND_50 | RF0_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm)) return;
	}

/* Zaiband modification target.  This is where immobile jellies and molds destroy things.
**	Silver jelly: ...:EAT_LITE:...; NEVER_MOVE; STUPID => reduces torches and lanterns to 0
turns of light
**	Blue jelly: ...:COLD:...; NEVER_MOVE; STUPID => destroys cold-hating items
**	Green jelly: ...:ACID:...; NEVER_MOVE; STUPID => destroys acid-hating items
**	Red mold: ...:FIRE:...; NEVER_MOVE; STUPID => destroys fire-hating items
**	Rot jelly: ...:EAT_FOOD:...; NEVER_MOVE; STUPID => destroys food, if not organic items
**	Shimmering mold: ...:ELEC:...; NEVER_MOVE; STUPID => destroys electricity-hating items
Range of effect: Objects in square are automatically blasted.
NOTE: Vulnerable items must be moved away in initial generation.
 */
	if ((r_ptr->flags[0] & RF0_NEVER_MOVE) && (r_ptr->flags[1] & RF1_STUPID))
		{
		if (monster_has_attack(r_ptr,RBE_EAT_FOOD))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									player_has_los_bold(o.y, o.x), NULL,
									"%^s spoils %s.",
									o_ptr_is<TV_FOOD>,NULL);
			}
		if (monster_has_attack(r_ptr,RBE_EAT_LITE))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									  player_has_los_bold(o.y, o.x), NULL,
									  "%^s unfuels %s.",
									  eat_lite_ok, eat_lite_helper);
			}
		if (monster_has_attack(r_ptr,RBE_ACID))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									player_has_los_bold(o.y, o.x), NULL,
									"%^s dissolves %s.",
									set_acid_destroy, NULL);
			}
		if (monster_has_attack(r_ptr,RBE_ELEC))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									player_has_los_bold(o.y, o.x), NULL,
									"%^s causes %s to explode.",
									set_elec_destroy, NULL);
			}
		if (monster_has_attack(r_ptr,RBE_FIRE))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									player_has_los_bold(o.y, o.x), NULL,
									"%^s incinerates %s.",
									set_fire_destroy, NULL);
			}
		if (monster_has_attack(r_ptr,RBE_COLD))
			{
			destroy_items_selectively(cave_o_idx[o.y][o.x], m_ptr,
									player_has_los_bold(o.y, o.x), NULL,
									"%^s shatters %s.",
									set_cold_destroy, NULL);
			}
		}

	/* wake up sleeping allies ... */
	if (!(r_ptr->flags[1] & RF1_STUPID) && los(o,p_ptr->loc))
	{
		for (i = mon_max - 1; i >= 1; i--)
		{
			/* Get the monster */
			const monster_type* const n_ptr = &mon_list[i];
			if (!n_ptr->r_idx) continue;	/* ignore dead monsters */
			if (!n_ptr->csleep) continue;	/* ally is awake (this catches self) */

			{
			const int range = distance(o.y, o.x, n_ptr->loc.y, n_ptr->loc.x);
			if (range>r_ptr->aaf) 			continue;	/* can't notice the ally */
			if (range>n_ptr->race()->aaf)	continue;	/* ally can't notice me */
			}

			if (los(o,n_ptr->loc))
			{	/* see the ally */
				apply_noise(o,1);	/* XXX do something about intensity later */
									/* XXX do something about a message later; CZ should bark, others should yell/shout */
				break;
			}
		}
	}

	/* Process moves */
	for (i = 0; i < 5; i++)
	{
		/* Get the direction (or stagger) */
		int d = (stagger ? ddd[rand_int(KEYPAD_DIR_MAX)] : mm[i]);

		/* Get the destination */
		coord n(o + dd_coord[d]);

		/* check legality */
		if (!(((r_ptr->flags[1] & (RF1_PASS_WALL | RF1_KILL_WALL)) ? wallpasser_move_legal
									: (r_ptr->flags[1] & (RF1_OPEN_DOOR | RF1_BASH_DOOR)) ? normal_move_legal : nodoor_move_legal)(n))) continue;

		/* Permanent wall (irrational caution) */
		if (FEAT_PERM_EXTRA <= cave_feat[n.y][n.x]) continue;

		do_move = TRUE;		/* Assume we can move */

		/* KBB: check whether move is suicidal, if not staggering */
		/* do not veto attacking the player */
		if (!stagger && (cave_m_idx[n.y][n.x] >= 0))
		{
			int mon_moves = 0;
			int player_moves = 0;
			m_ptr->move_ratio(player_moves, mon_moves, *p_ptr, 0, 1);	/* pessimize */

			/* unseen monster does not want to be both seen and attackable after moving */
			if (!m_ptr->ml)
			{	
				if (   !square_momentarily_unseen_by_player(m_idx,n,p_ptr->loc,player_moves)
					&& square_momentarily_unseen_by_player(m_idx,m_ptr->loc,p_ptr->loc,player_moves)
					&& !square_momentarily_safe_from_player(n,p_ptr->loc,player_moves)
					&& square_momentarily_safe_from_player(m_ptr->loc,p_ptr->loc,player_moves))
				{	/* oops */
				do_move = FALSE;
				}
			}

			/* seen, not-attackable monster does not want to be attackable after moving */
			else
			{
				if (   !square_momentarily_safe_from_player(n,p_ptr->loc,player_moves)
					&& square_momentarily_safe_from_player(m_ptr->loc,p_ptr->loc,player_moves))
				{	/* oops */
				do_move = FALSE;
				}
			}
		};

		/* Open floor is ok, need to check other terrain */
		if (do_move && !cave_floor_bold(n.y, n.x))
		{
			/* Well, be more careful about checking */
			do_move = FALSE;

			/* Monster moves through walls (and doors) */
			if (r_ptr->flags[1] & RF1_PASS_WALL)
			{
				/* Pass through walls/doors/rubble */
				do_move = TRUE;
	
				/* Monster went through a wall */
				learn_RF1 |= RF1_PASS_WALL;
			}

			/* Monster destroys walls (and doors) */
			else if (r_ptr->flags[1] & RF1_KILL_WALL)
			{
				/* Eat through walls/doors/rubble */
				do_move = TRUE;

				/* Monster destroyed a wall */
				learn_RF1 |= RF1_KILL_WALL;

				/* Forget the wall */
				cave_info[n.y][n.x] &= ~(CAVE_MARK);

				/* Notice */
				cave_set_feat(n.y, n.x, FEAT_FLOOR);

				/* Note changes to viewable region */
				if (player_has_los_bold(n.y, n.x)) do_view = TRUE;
			}

			/* Handle doors and secret doors */
			else if (cave_feat_in_range(n.y,n.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL) ||
			         (cave_feat[n.y][n.x] == FEAT_SECRET))
			{
				bool may_bash = TRUE;

				/* Take a turn */
				do_turn = TRUE;

				/* Creature can open doors. */
				if (r_ptr->flags[1] & RF1_OPEN_DOOR)
				{
					/* Closed doors and secret doors */
					if ((cave_feat[n.y][n.x] == FEAT_DOOR_HEAD) ||
					    (cave_feat[n.y][n.x] == FEAT_SECRET))
					{
						/* The door is open */
						learn_RF1 |= RF1_OPEN_DOOR;

						/* Do not bash the door */
						may_bash = FALSE;
					}

					/* Locked doors (not jammed) */
					else if (cave_feat[n.y][n.x] < FEAT_DOOR_HEAD + 0x08)
					{	/* Door power */
						const int k = ((cave_feat[n.y][n.x] - FEAT_DOOR_HEAD) & 0x07);

						may_bash = FALSE;	/* Do not bash the door */

						/* Try to unlock it XXX XXX XXX */
						if (((m_ptr->chp / 10) > k+1) && rand_int(m_ptr->chp / 10) > k)
						{
							/* Unlock the door */
							cave_set_feat(n.y, n.x, FEAT_DOOR_HEAD + 0x00);
						}
					}
				}

				/* Stuck doors -- attempt to bash them down if allowed */
				if (may_bash && (r_ptr->flags[1] & RF1_BASH_DOOR))
				{	/* weak monsters can't bash doors */
					if (20 <= m_ptr->chp)
					{
						/* Door power */
						const int k = ((cave_feat[n.y][n.x] - FEAT_DOOR_HEAD) & 0x07);

						/* Attempt to Bash XXX XXX XXX */
						if (((m_ptr->chp / 10) > k+1) && rand_int(m_ptr->chp / 10) > k)
						{
							msg_print("You hear a door burst open!");
							disturb(0, 0);			/* Disturb */
							apply_noise(n, 10);		/* Bashing open doors is noisy */
							learn_RF1 |= RF1_BASH_DOOR;
							do_move = TRUE;			/* Hack -- fall into doorway */
						}
						else
						{
							msg_print("You hear a door strain, but hold.");
							disturb(0, 0);			/* Disturb */
							apply_noise(n, 2);		/* Failing to bash open doors is noisy */
						}
					}
					else
					{
						/* Zaiband: Don't use that turn after all */
						do_turn = FALSE;
					}
				}


				/* Deal with doors in the way */
				if (learn_RF1 & (RF1_BASH_DOOR | RF1_OPEN_DOOR))
				{
					/* Break down the door */
					if ((learn_RF1 & RF1_BASH_DOOR) && one_in_(2))
					{
						cave_set_feat(n.y, n.x, FEAT_BROKEN);
					}

					/* Open the door */
					else
					{
						cave_set_feat(n.y, n.x, FEAT_OPEN);
					}

					/* Handle viewable doors */
					if (player_has_los_bold(n.y, n.x)) do_view = TRUE;
				}
			}
		}

		/* Hack -- check for Glyph of Warding */
		if (do_move && (cave_feat[n.y][n.x] == FEAT_GLYPH))
		{
			do_move = FALSE;	/* Assume no move allowed */

			/* Break the ward */
			if (randint(BREAK_GLYPH) < r_ptr->level)
			{
				/* Describe observable breakage */
				if (cave_info[n.y][n.x] & (CAVE_MARK))
				{
					msg_print("The rune of protection is broken!");
				}

				cave_info[n.y][n.x] &= ~(CAVE_MARK);	/* Forget the rune */
				cave_set_feat(n.y, n.x, FEAT_FLOOR);	/* Break the rune */
				do_move = TRUE;							/* Allow movement */
			}
			else
			{
				do_turn = TRUE;	/* Zaiband: failing to break the glyph uses a turn */
			}
		}

		if (do_move && (cave_m_idx[n.y][n.x] < 0))
		{
			do_move = FALSE;	/* Do not move */

			/* Some monsters never attack */
			if (r_ptr->flags[0] & RF0_NEVER_BLOW)
			{
				/* memorize lack of attacks */
				if (m_ptr->ml) l_ptr->flags[0] |= RF0_NEVER_BLOW;
			}

			/* The player is in the way.  Attack him. */
			else
			{
				make_attack_normal(m_idx);	/* Do the attack */
				do_turn = TRUE;				/* Took a turn */
			}
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags[0] & RF0_NEVER_MOVE))
		{
			/* memorize lack of moving */
			if (m_ptr->ml) l_ptr->flags[0] |= RF0_NEVER_MOVE;

			do_move = FALSE;	/* Do not move */

			/* Do physical ranged attacks */
			do_turn = make_attack_ranged_physical(m_idx);
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[n.y][n.x] > 0))
		{
			const monster_type* const n_ptr = m_ptr_from_m_idx(cave_m_idx[n.y][n.x]);

			do_move = FALSE;	/* Assume no movement */

			/* Kill weaker monsters */
			if ((r_ptr->flags[1] & RF1_KILL_BODY) &&
			    (compare_monsters(m_ptr, n_ptr) > 0))
			{
				do_move = TRUE;		/* Allow movement */

				/* Monster ate another monster */
				learn_RF1 |= RF1_KILL_BODY;

				/* Message XXX XXX XXX */

				delete_monster(n);	/* Kill the monster */
			}
		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[n.y][n.x] > 0))
		{
			const monster_type* const n_ptr = m_ptr_from_m_idx(cave_m_idx[n.y][n.x]);

			do_move = FALSE;	/* Assume no movement */

			/* Push past weaker monsters (unless leaving a wall) */
			if ((r_ptr->flags[1] & RF1_MOVE_BODY) &&
			    (compare_monsters(m_ptr, n_ptr) > 0) &&
			    (cave_floor_bold(m_ptr->loc.y, m_ptr->loc.x)))
			{
				do_move = TRUE;	/* Allow movement */

				/* Monster pushed past another monster */
				learn_RF1 |= RF1_MOVE_BODY;

				/* XXX XXX XXX Message */
			}
		}


		/* Creature has been allowed move */
		if (do_move)
		{
			do_turn = TRUE;	/* Take a turn */

			/* Zaiband: if a diagonal move, use 150 energy rather than 100 energy */
			if (0!=dd_coord[d].x && 0!=dd_coord[d].y) m_ptr->energy -= 50;

			monster_swap(o, n);	/* Move the monster */

			/* Possible disturb */
			if (m_ptr->ml && (OPTION(disturb_move) || 
							((m_ptr->mflag & (MFLAG_VIEW)) && OPTION(disturb_near))))
			{
				disturb(0, 0);	/* Disturb */
			}


			/* Start ZaiBand modification */
			/* If something attacks to eat food, it should eat food from the floor! */
			/* this condition must reflect the attack type's existence */
			if (monster_has_attack(r_ptr,RBE_EAT_FOOD))
				{	/* Scan all objects in the grid */
				(void)destroy_items_selectively(cave_o_idx[n.y][n.x], m_ptr,
										player_has_los_bold(n.y, n.x),
										"%^s tries to eat %s, but fails.",
										"%^s eats %s.",
										eat_food_floor_ok, eat_food_floor_helper);
				}
			/* End ZaiBand modification */

			/* ZaiBand: no change to code effect, but restructured to take advantage 
			 * of meta-code.  [well...you *must* see it done before you know it.]
			 * META: there should be a reality check prohibiting both RF1_KILL_ITEM 
			 * and RF1_TAKE_ITEM in the same r_ptr->flags[1]
			 */
			if 		(r_ptr->flags[1] & RF1_KILL_ITEM)
				{
				if (destroy_items_selectively(cave_o_idx[n.y][n.x], m_ptr,
										player_has_los_bold(n.y, n.x), NULL,
										"%^s crushes %s.", non_money_ok,
										NULL))
					learn_RF1 |= RF1_KILL_ITEM;
				}
			else if (r_ptr->flags[1] & RF1_TAKE_ITEM)
				{
				if (destroy_items_selectively(cave_o_idx[n.y][n.x], m_ptr,
										player_has_los_bold(n.y, n.x),
										"%^s tries to pick up %s, but fails.",
										"%^s picks up %s.",
										non_money_ok, take_item_helper))
					learn_RF1 |= RF1_TAKE_ITEM;
				}
			/* End ZaiBand modification */
		}

		if (do_turn) break;	/* Stop when done */
	}


	/* If we haven't done anything, try casting a spell again */
	if (OPTION(adult_smart_monsters) && !do_turn && !do_move)
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
	}

	/* Notice changes in view */
	if (do_view)
	{
		/* Update the visuals, fully update the flow */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FORGET_FLOW | PU_UPDATE_FLOW);
	}


	/* Learn things from observable monster */
	if (m_ptr->ml) l_ptr->flags[1] |= learn_RF1;

	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
	{
		m_ptr->monfear = 0;	/* No longer afraid */

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}
}


static bool monster_can_flow(const monster_type* const m_ptr)
{
	/* Hack -- Monsters can "smell" the player from far away */
	if (OPTION(adult_flow_by_sound))
	{
		/* Monster location */
		int fy = m_ptr->loc.y;
		int fx = m_ptr->loc.x;

		/* Check the flow (normal aaf is about 20) */
		if ((cave_when[fy][fx] == cave_when[p_ptr->loc.y][p_ptr->loc.x]) &&
		    (cave_cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
		    (cave_cost[fy][fx] < m_ptr->race()->aaf))
		{
			return TRUE;
		}
	}

	return FALSE;
}




/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.  This flag
 * is optimized via the "repair_mflag_nice" flag.
 */
void process_monsters(byte minimum_energy)
{
	if (1==mon_max || p_ptr->leaving) return;

	m_idx_type i(mon_max - 1);

	/* Process the monsters (backwards); handle leaving */
	while(i>=1 && !p_ptr->leaving)
	{
		/* Get the monster */
		monster_type* const m_ptr = &mon_list[i];

		if (	m_ptr->r_idx						/* ignore "dead" monsters */
			&&	m_ptr->energy >= minimum_energy)	/* need enough energy to move */
		/* Heal monster? XXX XXX XXX */
		{
			const monster_race* const r_ptr = m_ptr->race();	/* Get the race */

			/*
			 * Process the monster if the monster either:
			 * - can "sense" the player
			 * - is hurt
			 * - can "see" the player (checked backwards)
			 * - is already awake and has either full gaze or clairvoyant gaze [ZaiBand]
			 * - can "smell" the player from far away (flow)
			 */
			if ((m_ptr->cdis <= r_ptr->aaf) ||
			    (m_ptr->chp < m_ptr->mhp) ||
			    player_has_los_bold(m_ptr->loc.y, m_ptr->loc.x) ||
				(!m_ptr->csleep && (monster_has_attack(r_ptr,RBM_CLAIRVOYANT_GAZE) || monster_has_attack(r_ptr,RBM_RANGED_GAZE)) && MAX_RANGE<distance(m_ptr->loc.y,m_ptr->loc.x,p_ptr->loc.y,p_ptr->loc.x)) ||
			    monster_can_flow(m_ptr))
			{
				process_monster(i);	/* Process the monster */
			}

			m_ptr->energy -= 100;	/* Use up "some" energy */
		}	/* end blocking brace */
		if (1==i) break;
		--i;
	}
}
