/* File: monster1.c */

/*
 * File: monster1.c
 * Purpose: Monster description code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "melee.h"
#include "option.h"
#include "raceflag.h"


/*
 * Pronoun arrays, by gender.
 */
static const char* const wd_he[3] =
{ "it", "he", "she" };
static const char* const wd_his[3] =
{ "its", "his", "her" };


/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c, s, p)    (((c) == 1) ? (s) : (p))

int race_gender_index(const monster_race& r)
{
	/* Extract a gender (if applicable) */
	if (r.flags[0] & RF0_FEMALE) return 2;
	if (r.flags[0] & RF0_MALE) return 1;
	return 0;
}

static void output_list(const char* const list[], int num, byte attr)
{
	int i;
	const char *conjunction = "and ";

	if (num < 0)
	{
		num = -num;
		conjunction = "or ";
	}

	for (i = 0; i < num; i++)
	{
        if (i)
		{
			if (num > 2)
				text_out(", ");
			else
				text_out(" ");

			if (i == num - 1)
				text_out(conjunction);
		}

		text_out_c(attr, list[i]);
	}
}


static void output_desc_list(int msex, const char* const intro, const char* const list[], int n, byte attr)
{
	if (n != 0)
	{
		/* Output intro */
		text_out(format("%^s %s ", wd_he[msex], intro));

		/* Output list */
		output_list(list, n, attr);

		/* Output end */
		text_out(".  ");
	}
}






/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(const monster_race& r, const monster_lore& lore)
{
	s32b level = r.level;
	s32b kills = lore.tkills;

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r.flags[0] & RF0_UNIQUE)) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Zaiband: Cf. monster_critical(),make_attack_normal()/melee1.c  
 * 20d1 attacks and stronger are always supercharged criticals, 
 * other than RBE_HURT.  This is scary.
 *
 * Any attack that can theoretically kill the player in one hit is scary.
 */
static bool scary_damage(const monster_blow& blow)
{
	if ((byte)RBE_HURT==blow.effect)
	{
		if (armor_damage_reduction(blow.d.maxroll(),p_ptr->total_ac()) > p_ptr->chp)
			return TRUE;	/* one-blow kills are scary */
	}
	else
	{
		if (	(byte)(1)==blow.d.sides && 20<=blow.d.dice)
			return TRUE;	/* Guaranteed supercharged criticals: scary. */
		if (blow.d.maxroll() > p_ptr->chp)
			return TRUE;	/* one-blow kills are scary */
	}

	return FALSE;
}

/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(const monster_race& r, const monster_lore& lore, int i)
{
	s32b level = r.level;
	s32b a = lore.blows[i];
	s32b d = r.blow[i].d.maxroll();

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/*
	 * Zaiband: Cf. monster_critical(),make_attack_normal()/melee1.c  
	 * 20d1 attacks and stronger are always supercharged criticals, 
	 * other than RBE_HURT.
	 */
	if (	(byte)RBE_HURT!=r.blow[i].effect && (byte)(1)==r.blow[i].d.sides
		&& 	20<=d)
		return TRUE;	/* Guaranteed supercharged criticals: obvious. */

	/* Skip non-uniques */
	if (!(r.flags[0] & RF0_UNIQUE)) return (FALSE);

	/* Unique monsters are twice as easy. */
	if ((4 + level) * a > (80/2) * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}


static void describe_monster_desc(const monster_race& r)
{
	char buf[2048];

	/* Simple method */
	my_strcpy(buf, r.text(), sizeof(buf));

	/* Dump it */
	text_out(buf);
	text_out("\n");
}


static void describe_monster_spells(const monster_race& r, const monster_lore& lore)
{
	int msex = race_gender_index(r);
	bool breath = FALSE;
	bool magic = FALSE;
	int vn;
	const char* vp[64];


	/* Collect innate attacks */
	vn = 0;
	if (lore.spell_flags[0] & RSF0_SHRIEK)  vp[vn++] = "shriek for help";
	if (lore.spell_flags[0] & RSF0_ARROW_1) vp[vn++] = "fire an arrow";
	if (lore.spell_flags[0] & RSF0_ARROW_2) vp[vn++] = "fire arrows";
	if (lore.spell_flags[0] & RSF0_ARROW_3) vp[vn++] = "fire a missile";
	if (lore.spell_flags[0] & RSF0_ARROW_4) vp[vn++] = "fire missiles";
	if (lore.spell_flags[0] & RSF0_BOULDER) vp[vn++] = "throw boulders";

	/* Describe innate attacks */
	output_desc_list(msex, "may", vp, -vn, TERM_WHITE);

	/* Collect breaths */
	vn = 0;
	if (lore.spell_flags[0] & RSF0_BR_ACID)		vp[vn++] = "acid";
	if (lore.spell_flags[0] & RSF0_BR_ELEC)		vp[vn++] = "lightning";
	if (lore.spell_flags[0] & RSF0_BR_FIRE)		vp[vn++] = "fire";
	if (lore.spell_flags[0] & RSF0_BR_COLD)		vp[vn++] = "frost";
	if (lore.spell_flags[0] & RSF0_BR_POIS)		vp[vn++] = "poison";
	if (lore.spell_flags[0] & RSF0_BR_NETH)		vp[vn++] = "nether";
	if (lore.spell_flags[0] & RSF0_BR_LITE)		vp[vn++] = "light";
	if (lore.spell_flags[0] & RSF0_BR_DARK)		vp[vn++] = "darkness";
	if (lore.spell_flags[0] & RSF0_BR_CONF)		vp[vn++] = "confusion";
	if (lore.spell_flags[0] & RSF0_BR_SOUN)		vp[vn++] = "sound";
	if (lore.spell_flags[0] & RSF0_BR_CHAO)		vp[vn++] = "chaos";
	if (lore.spell_flags[0] & RSF0_BR_DISE)		vp[vn++] = "disenchantment";
	if (lore.spell_flags[0] & RSF0_BR_NEXU)		vp[vn++] = "nexus";
	if (lore.spell_flags[0] & RSF0_BR_TIME)		vp[vn++] = "time";
	if (lore.spell_flags[0] & RSF0_BR_INER)		vp[vn++] = "inertia";
	if (lore.spell_flags[0] & RSF0_BR_GRAV)		vp[vn++] = "gravity";
	if (lore.spell_flags[0] & RSF0_BR_SHAR)		vp[vn++] = "shards";
	if (lore.spell_flags[0] & RSF0_BR_PLAS)		vp[vn++] = "plasma";
	if (lore.spell_flags[0] & RSF0_BR_WALL)		vp[vn++] = "force";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Display */
		text_out("%^s may ", wd_he[msex]);
		text_out_c(TERM_L_RED, "breathe ");
		output_list(vp, -vn, TERM_WHITE);
	}


	/* Collect spells */
	vn = 0;
	if (lore.spell_flags[1] & RSF1_BA_ACID)     vp[vn++] = "produce acid balls";
	if (lore.spell_flags[1] & RSF1_BA_ELEC)     vp[vn++] = "produce lightning balls";
	if (lore.spell_flags[1] & RSF1_BA_FIRE)     vp[vn++] = "produce fire balls";
	if (lore.spell_flags[1] & RSF1_BA_COLD)     vp[vn++] = "produce frost balls";
	if (lore.spell_flags[1] & RSF1_BA_POIS)     vp[vn++] = "produce poison balls";
	if (lore.spell_flags[1] & RSF1_BA_NETH)     vp[vn++] = "produce nether balls";
	if (lore.spell_flags[1] & RSF1_BA_WATE)     vp[vn++] = "produce water balls";
	if (lore.spell_flags[1] & RSF1_BA_MANA)     vp[vn++] = "invoke mana storms";
	if (lore.spell_flags[1] & RSF1_BA_DARK)     vp[vn++] = "invoke darkness storms";
	if (lore.spell_flags[1] & RSF1_DRAIN_MANA)  vp[vn++] = "drain mana";
	if (lore.spell_flags[1] & RSF1_MIND_BLAST)  vp[vn++] = "cause mind blasting";
	if (lore.spell_flags[1] & RSF1_BRAIN_SMASH) vp[vn++] = "cause brain smashing";
	if (lore.spell_flags[1] & RSF1_CAUSE_1)     vp[vn++] = "cause light wounds";
	if (lore.spell_flags[1] & RSF1_CAUSE_2)     vp[vn++] = "cause serious wounds";
	if (lore.spell_flags[1] & RSF1_CAUSE_3)     vp[vn++] = "cause critical wounds";
	if (lore.spell_flags[1] & RSF1_CAUSE_4)     vp[vn++] = "cause mortal wounds";
	if (lore.spell_flags[1] & RSF1_BO_ACID)     vp[vn++] = "produce acid bolts";
	if (lore.spell_flags[1] & RSF1_BO_ELEC)     vp[vn++] = "produce lightning bolts";
	if (lore.spell_flags[1] & RSF1_BO_FIRE)     vp[vn++] = "produce fire bolts";
	if (lore.spell_flags[1] & RSF1_BO_COLD)     vp[vn++] = "produce frost bolts";
/*	if (lore.spell_flags[1] & RSF1_BO_POIS)     vp[vn++] = "produce poison bolts"; */
	if (lore.spell_flags[1] & RSF1_BO_NETH)     vp[vn++] = "produce nether bolts";
	if (lore.spell_flags[1] & RSF1_BO_WATE)     vp[vn++] = "produce water bolts";
	if (lore.spell_flags[1] & RSF1_BO_MANA)     vp[vn++] = "produce mana bolts";
	if (lore.spell_flags[1] & RSF1_BO_PLAS)     vp[vn++] = "produce plasma bolts";
	if (lore.spell_flags[1] & RSF1_BO_ICEE)     vp[vn++] = "produce ice bolts";
	if (lore.spell_flags[1] & RSF1_MISSILE)     vp[vn++] = "produce magic missiles";
	if (lore.spell_flags[1] & RSF1_SCARE)       vp[vn++] = "terrify";
	if (lore.spell_flags[1] & RSF1_BLIND)       vp[vn++] = "blind";
	if (lore.spell_flags[1] & RSF1_CONF)        vp[vn++] = "confuse";
	if (lore.spell_flags[1] & RSF1_SLOW)        vp[vn++] = "slow";
	if (lore.spell_flags[1] & RSF1_HOLD)        vp[vn++] = "paralyze";
	if (lore.spell_flags[2] & RSF2_HASTE)       vp[vn++] = "haste-self";
	if (lore.spell_flags[2] & RSF2_HEAL)        vp[vn++] = "heal-self";
	if (lore.spell_flags[2] & RSF2_BLINK)       vp[vn++] = "blink-self";
	if (lore.spell_flags[2] & RSF2_TPORT)       vp[vn++] = "teleport-self";
	if (lore.spell_flags[2] & RSF2_TELE_TO)     vp[vn++] = "teleport to";
	if (lore.spell_flags[2] & RSF2_TELE_AWAY)   vp[vn++] = "teleport away";
	if (lore.spell_flags[2] & RSF2_TELE_LEVEL)  vp[vn++] = "teleport level";
	if (lore.spell_flags[2] & RSF2_DARKNESS)    vp[vn++] = "create darkness";
	if (lore.spell_flags[2] & RSF2_TRAPS)       vp[vn++] = "create traps";
	if (lore.spell_flags[2] & RSF2_FORGET)      vp[vn++] = "cause amnesia";
	if (lore.spell_flags[2] & RSF2_S_KIN)       vp[vn++] = "summon similar monsters";
	if (lore.spell_flags[2] & RSF2_S_MONSTER)   vp[vn++] = "summon a monster";
	if (lore.spell_flags[2] & RSF2_S_MONSTERS)  vp[vn++] = "summon monsters";
	if (lore.spell_flags[2] & RSF2_S_ANIMAL)    vp[vn++] = "summon animals";
	if (lore.spell_flags[2] & RSF2_S_SPIDER)    vp[vn++] = "summon spiders";
	if (lore.spell_flags[2] & RSF2_S_HOUND)     vp[vn++] = "summon hounds";
	if (lore.spell_flags[2] & RSF2_S_HYDRA)     vp[vn++] = "summon hydras";
	if (lore.spell_flags[2] & RSF2_S_ANGEL)     vp[vn++] = "summon an angel";
	if (lore.spell_flags[2] & RSF2_S_DEMON)     vp[vn++] = "summon a demon";
	if (lore.spell_flags[2] & RSF2_S_UNDEAD)    vp[vn++] = "summon an undead";
	if (lore.spell_flags[2] & RSF2_S_DRAGON)    vp[vn++] = "summon a dragon";
	if (lore.spell_flags[2] & RSF2_S_HI_UNDEAD) vp[vn++] = "summon Greater Undead";
	if (lore.spell_flags[2] & RSF2_S_HI_DRAGON) vp[vn++] = "summon Ancient Dragons";
	if (lore.spell_flags[2] & RSF2_S_HI_DEMON)  vp[vn++] = "summon Greater Demons";
	if (lore.spell_flags[2] & RSF2_S_WRAITH)    vp[vn++] = "summon Ring Wraiths";
	if (lore.spell_flags[2] & RSF2_S_UNIQUE)    vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
			text_out(", and is also ");
		else
			text_out("%^s is ", wd_he[msex]);

		/* Verb Phrase */
		text_out_c(TERM_L_RED, "magical");
		text_out(", casting spells");

		/* Adverb */
		if (lore.flags[1] & RF1_SMART) text_out(" intelligently");

		/* List */
		text_out(" which ");
		output_list(vp, -vn, TERM_WHITE);
	}


	/* End the sentence about innate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		int m = lore.cast_innate + lore.cast_spell;

		/* Average frequency */
		int n = (r.freq_innate + r.freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			text_out("; ");
			text_out_c(TERM_L_GREEN, "1");
			text_out(" time in ");
			text_out_c(TERM_L_GREEN, "%d", 100 / n);
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			text_out("; about ");
			text_out_c(TERM_L_GREEN, "1");
			text_out(" time in ");
			text_out_c(TERM_L_GREEN, "%d", 100 / n);
		}

		/* End this sentence */
		text_out(".  ");
	}
}


static void describe_monster_drop(const monster_race& r, const monster_lore& lore)
{
	/* Drops gold and/or items */
	if (lore.drop_gold || lore.drop_item)
	{	
		int msex = race_gender_index(r);
		int n = MAX(lore.drop_gold, lore.drop_item);	/* Count maximum drop */
		const char* p;
		bool sin = FALSE;

		/* Intro */
		text_out(format("%^s may carry", wd_he[msex]));

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
		if (lore.flags[0] & RF0_DROP_GREAT)
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (lore.flags[0] & RF0_DROP_GOOD)
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
		if (lore.drop_item)
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
		if (lore.drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) text_out("n");

			/* Dump "treasure(s)" */
			if (p) text_out(p);
			text_out(" treasure");
			if (n != 1) text_out("s");
		}

		/* End this sentence */
		text_out(".  ");
	}
}


static void describe_monster_attack(const monster_race& r, const monster_lore& lore)
{
	int n = 0;
	int m, i;

	int msex = race_gender_index(r);
	
	/* Count the number of "known" attacks */
	for (m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		/* Skip non-attacks */
		if (!r.blow[m].method) continue;

		/* Count known attacks */
		if (lore.blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (i = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		if (!r.blow[m].method) continue;	/* Skip non-attacks */
		if (!lore.blows[m]) continue;		/* Skip unknown attacks */

		dice_sides d = r.blow[m].d;		/* Extract the attack info */
		const char* p = "do something weird"; 	/* Hack -- force a method in case something of invalid */
		const char* q = NULL;					/* Default effect */

		/* Get the method */
		switch (r.blow[m].method)
		{
			case RBM_HIT:	p = "hit"; break;
			case RBM_TOUCH:	p = "touch"; break;
			case RBM_PUNCH:	p = "punch"; break;
			case RBM_KICK:	p = "kick"; break;
			case RBM_CLAW:	p = "claw"; break;
			case RBM_BITE:	p = "bite"; break;
			case RBM_STING:	p = "sting"; break;
			case RBM_BUTT:	p = "butt"; break;
			case RBM_CRUSH:	p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			case RBM_CRAWL:	p = "crawl on you"; break;
			case RBM_DROOL:	p = "drool on you"; break;
			case RBM_SPIT:	p = "spit"; break;
			case RBM_GAZE:	p = "gaze"; break;
			case RBM_WAIL:	p = "wail"; break;
			case RBM_SPORE:	p = "release spores"; break;
			case RBM_BEG:	p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
			case RBM_MOAN:	p = "moan"; break;
			/* ZaiBand extended monster attacks */
			case RBM_PRESBYOPIC_GAZE: p = "gaze at moderate ranges"; break;
			case RBM_HYPEROPIC_GAZE: p = "gaze when far away"; break;
			case RBM_RANGED_GAZE: p = "gaze anywhere it can see"; break;
			case RBM_CLAIRVOYANT_GAZE: 	p = "gaze (disregarding mere physical obstructions)"; break;
		}


		/* Get the effect */
		switch (r.blow[m].effect)
		{
			case RBE_HURT:      q = "attack"; break;
			case RBE_POISON:    q = "poison"; break;
			case RBE_UN_BONUS:  q = "disenchant"; break;
			case RBE_UN_POWER:  q = "drain charges"; break;
			case RBE_EAT_GOLD:  q = "steal gold"; break;
			case RBE_EAT_ITEM:  q = "steal items"; break;
			case RBE_EAT_FOOD:  q = "eat your food"; break;
			case RBE_EAT_LITE:  q = "absorb light"; break;
			case RBE_ACID:      q = "shoot acid"; break;
			case RBE_ELEC:      q = "electrify"; break;
			case RBE_FIRE:      q = "burn"; break;
			case RBE_COLD:      q = "freeze"; break;
			case RBE_BLIND:     q = "blind"; break;
			case RBE_CONFUSE:   q = "confuse"; break;
			case RBE_TERRIFY:   q = "terrify"; break;
			case RBE_PARALYZE:  q = "paralyze"; break;
			case RBE_LOSE_STR:  q = "reduce strength"; break;
			case RBE_LOSE_INT:  q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:  q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:  q = "reduce dexterity"; break;
			case RBE_LOSE_CON:  q = "reduce constitution"; break;
			case RBE_LOSE_CHR:  q = "reduce charisma"; break;
			case RBE_LOSE_ALL:  q = "reduce all stats"; break;
			case RBE_SHATTER:   q = "shatter"; break;
			case RBE_EXP_10:    q = "lower experience"; break;
			case RBE_EXP_20:    q = "lower experience"; break;
			case RBE_EXP_40:    q = "lower experience"; break;
			case RBE_EXP_80:    q = "lower experience"; break;
			case RBE_HALLU:     q = "cause hallucinations"; break;
		}


		/* Introduce the attack description */
		if (!i)
		{
			text_out(format("%^s can ", wd_he[msex]));
		}
		else if (i < n-1)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}

		assert(p);

		/* Describe the method */
		text_out(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			text_out(" to ");
			text_out_c(TERM_L_RED, q);

			/* Describe damage (if known) */
			if (d.dice && d.sides && know_damage(r, lore, m))
			{
				/* Display the damage */
				text_out((scary_damage(r.blow[m])) ? " with frightening damage" : " with damage");
				text_out(format(" %dd%d", (int)d.dice, (int)d.sides));
			}
		}


		/* Count the attacks as printed */
		++i;
	}

	/* Finish sentence above */
	if (i)
	{
		text_out(".  ");
	}

	/* Notice lack of attacks */
	else if (lore.flags[0] & RF0_NEVER_BLOW)
	{
		text_out(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		text_out(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}
}


static void describe_monster_abilities(const monster_race& r, const monster_lore& lore)
{
	int vn;
	const char* vp[64];

	int msex = race_gender_index(r);
	bool prev = FALSE;


	/* Collect special abilities. */
	vn = 0;
	if (lore.flags[1] & RF1_OPEN_DOOR) vp[vn++] = "open doors";
	if (lore.flags[1] & RF1_BASH_DOOR) vp[vn++] = "bash down doors";
	if (lore.flags[1] & RF1_PASS_WALL) vp[vn++] = "pass through walls";
	if (lore.flags[1] & RF1_KILL_WALL) vp[vn++] = "bore through walls";
	if (lore.flags[1] & RF1_MOVE_BODY) vp[vn++] = "push past weaker monsters";
	if (lore.flags[1] & RF1_KILL_BODY) vp[vn++] = "destroy weaker monsters";
	if (lore.flags[1] & RF1_TAKE_ITEM) vp[vn++] = "pick up objects";
	if (lore.flags[1] & RF1_KILL_ITEM) vp[vn++] = "destroy objects";

	/* Describe special abilities. */
	output_desc_list(msex, "can", vp, vn, TERM_WHITE);


	/* Describe detection traits */
	vn = 0;
	if (lore.flags[1] & RF1_INVISIBLE)  vp[vn++] = "invisible";
	if (lore.flags[1] & RF1_COLD_BLOOD) vp[vn++] = "cold blooded";
	if (lore.flags[1] & RF1_EMPTY_MIND) vp[vn++] = "not detected by telepathy";
	if (lore.flags[1] & RF1_WEIRD_MIND) vp[vn++] = "rarely detected by telepathy";

	output_desc_list(msex, "is", vp, vn, TERM_WHITE);


	/* Describe special things */
	if (lore.flags[1] & RF1_MULTIPLY)
		text_out("%^s breeds explosively.  ", wd_he[msex]);
	if (lore.flags[1] & RF1_REGENERATE)
		text_out("%^s regenerates quickly.  ", wd_he[msex]);


	/* Collect susceptibilities */
	vn = 0;
	if (lore.flags[2] & RF2_HURT_ROCK) vp[vn++] = "rock remover";
	if (lore.flags[2] & RF2_HURT_LITE) vp[vn++] = "bright light";
	if (lore.flags[2] & RF2_HURT_FIRE) vp[vn++] = "fire";
	if (lore.flags[2] & RF2_HURT_COLD) vp[vn++] = "cold";

	if (vn)
	{
		/* Output connecting text */
		text_out("%^s is hurt by ", wd_he[msex]);
		output_list(vp, vn, TERM_YELLOW);
		prev = TRUE;
	}

	/* Collect immunities and resistances */
	vn = 0;
	if (lore.flags[2] & RF2_IM_ACID)   vp[vn++] = "acid";
	if (lore.flags[2] & RF2_IM_ELEC)   vp[vn++] = "lightning";
	if (lore.flags[2] & RF2_IM_FIRE)   vp[vn++] = "fire";
	if (lore.flags[2] & RF2_IM_COLD)   vp[vn++] = "cold";
	if (lore.flags[2] & RF2_IM_POIS)   vp[vn++] = "poison";
	if (lore.flags[2] & RF2_IM_WATER)  vp[vn++] = "water";
	if (lore.flags[2] & RF2_RES_NETH)  vp[vn++] = "nether";
	if (lore.flags[2] & RF2_RES_PLAS)  vp[vn++] = "plasma";
	if (lore.flags[2] & RF2_RES_NEXUS) vp[vn++] = "nexus";
	if (lore.flags[2] & RF2_RES_DISE)  vp[vn++] = "disenchantment";

	if (vn)
	{
		/* Output connecting text */
		if (prev)
			text_out(", but resists ");
		else
			text_out("%^s resists ", wd_he[msex]);

		/* Write the text */
		output_list(vp, vn, TERM_ORANGE);
		prev = TRUE;
	}

	/* Collect non-effects */
	vn = 0;
	if (lore.flags[2] & RF2_NO_STUN) vp[vn++] = "stunned";
	if (lore.flags[2] & RF2_NO_FEAR) vp[vn++] = "frightened";
	if (lore.flags[2] & RF2_NO_CONF) vp[vn++] = "confused";
	if (lore.flags[2] & RF2_NO_SLEEP) vp[vn++] = "slept";

	if (vn)
	{
		/* Output connecting text */
		if (prev)
			text_out(", and cannot be ");
		else
			text_out("%^s cannot be ", wd_he[msex]);

		output_list(vp, -vn, TERM_ORANGE);
		prev = TRUE;
	}


	/* Full stop. */
	if (prev) text_out(".  ");


	/* Do we know how aware it is? */
	if ((((int)lore.wake * (int)lore.wake) > r.sleep) ||
	    (lore.ignore == UCHAR_MAX) ||
	    ((r.sleep == 0) && (lore.tkills >= 10)))
	{
		const char* act;

		if (r.sleep > 200)     act = "prefers to ignore";
		else if (r.sleep > 95) act = "pays very little attention to";
		else if (r.sleep > 75) act = "pays little attention to";
		else if (r.sleep > 45) act = "tends to overlook";
		else if (r.sleep > 25) act = "takes quite a while to see";
		else if (r.sleep > 10) act = "takes a while to see";
		else if (r.sleep > 5)  act = "is fairly observant of";
		else if (r.sleep > 3)  act = "is observant of";
		else if (r.sleep > 1)  act = "is very observant of";
		else if (r.sleep > 0)  act = "is vigilant for";
		else                        act = "is ever vigilant for";

		text_out("%^s %s intruders, which %s may notice from ", wd_he[msex], act, wd_he[msex]);
		text_out_c(TERM_L_GREEN, "%d", 10 * r.aaf);
		text_out(" feet.  ");
	}

	/* Describe escorts */
	if ((lore.flags[0] & (RF0_ESCORT | RF0_ESCORTS)))
	{
		text_out(format("%^s usually appears with escorts.  ", wd_he[msex]));
	}

	/* Describe friends */
	else if ((lore.flags[0] & (RF0_FRIEND | RF0_FRIENDS)))
	{
		text_out(format("%^s usually appears in groups.  ", wd_he[msex]));
	}
}


static void describe_monster_kills(const monster_race& r, const monster_lore& lore)
{
	int msex = race_gender_index(r);
	bool out = TRUE;


	/* Treat uniques differently */
	if (lore.flags[0] & RF0_UNIQUE)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r.max_num == 0);

		/* We've been killed... */
		if (lore.deaths)
		{
			/* Killed ancestors */
			text_out(format("%^s has slain %d of your ancestors",
			            wd_he[msex], lore.deaths));

			/* But we've also killed it */
			if (dead)
			{
				text_out(", but you have taken revenge!  ");
			}

			/* Unavenged (ever) */
			else
			{
				text_out(format(", who %s unavenged.  ",
				            plural(lore.deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			text_out("You have slain this foe.  ");
		}
		else
		{
			/* Alive and never killed us */
			out = FALSE;
		}
	}

	/* Not unique, but killed us */
	else if (lore.deaths)
	{
		/* Dead ancestors */
		text_out(format("%d of your ancestors %s been killed by this creature, ",
		            lore.deaths, plural(lore.deaths, "has", "have")));

		/* Some kills this life */
		if (lore.pkills)
		{
			text_out(format("and you have exterminated at least %d of the creatures.  ",
			            lore.pkills));
		}

		/* Some kills past lives */
		else if (lore.tkills)
		{
			text_out(format("and %s have exterminated at least %d of the creatures.  ",
			            "your ancestors", lore.tkills));
		}

		/* No kills */
		else
		{
			text_out_c(TERM_RED, format("and %s is not ever known to have been defeated.  ",
			            wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (lore.pkills)
		{
			text_out(format("You have killed at least %d of these creatures.  ",
			            lore.pkills));
		}

		/* Killed some last life */
		else if (lore.tkills)
		{
			text_out(format("Your ancestors have killed at least %d of these creatures.  ",
			            lore.tkills));
		}

		/* Killed none */
		else
		{
			text_out("No battles to the death are recalled.  ");
		}
	}

	/* Separate */
	if (out) text_out("\n");
}


static void describe_monster_toughness(const monster_race& r, const monster_lore& lore)
{
	/* Describe monster "toughness" */
	if (know_armour(r, lore))
	{
		int msex = race_gender_index(r);

		/* Armor */
		text_out(format("%^s has an armor rating of %d", wd_he[msex], r.ac));

		/* Maximized hitpoints */
		if (lore.flags[0] & RF0_FORCE_MAXHP)
		{
			text_out(format(" and a life rating of %d.  ", r.h.maxroll()));
		}

		/* Variable hitpoints */
		else
		{
			text_out(format(" and a life rating of %dd%d.  ",
			            (int)(r.h.dice), (int)(r.h.sides)));
		}
	}
}


static void describe_monster_exp(const monster_race& r, const monster_lore& lore)
{
	/* Describe experience */
	const char* p = "th";	/* English ordinal suffix */
	const char* q = "";		/* leading vowels in numbers */

	/* calculate the integer exp part (abuse later) */
	int i = (long)r.mexp * r.level / p_ptr->lev;

	/* calculate the fractional exp part scaled by 100, */
	/* must use long arithmetic to avoid overflow (abuse later) */
	int j = ((((long)r.mexp * r.level % p_ptr->lev) *
		  (long)1000 / p_ptr->lev + 5) / 10);

	/* Introduction */
	text_out((lore.flags[0] & RF0_UNIQUE) ? "Killing" : "A kill of");
	text_out(" this creature");


	/* Mention the experience */
	text_out(format(" is worth %ld.%02ld point%s",
			        (long)i, (long)j,
			        (((i == 1) && (j == 0)) ? "" : "s")));

	/* Take account of annoying English */
	i = p_ptr->lev % 10;
	if ((p_ptr->lev / 10) == 1) /* nothing */;
	else if (i == 1) p = "st";
	else if (i == 2) p = "nd";
	else if (i == 3) p = "rd";

	/* Take account of "leading vowels" in numbers */
	i = p_ptr->lev;
	if ((i == 8) || (i == 11) || (i == 18)) q = "n";

	/* Mention the dependance on the player's level */
	text_out(format(" for a%s %lu%s level character.  ", q, (long)i, p));
}


static void describe_monster_movement(const monster_race& r, const monster_lore& lore)
{
	bool old = FALSE;


	text_out("This");

	if (lore.flags[2] & RF2_ANIMAL) text_out_c(TERM_L_BLUE, " natural");
	if (lore.flags[2] & RF2_EVIL) text_out_c(TERM_L_BLUE, " evil");
	if (lore.flags[2] & RF2_UNDEAD) text_out_c(TERM_L_BLUE, " undead");

	if (lore.flags[2] & RF2_DRAGON) text_out_c(TERM_L_BLUE, " dragon");
	else if (lore.flags[2] & RF2_DEMON) text_out_c(TERM_L_BLUE, " demon");
	else if (lore.flags[2] & RF2_GIANT) text_out_c(TERM_L_BLUE, " giant");
	else if (lore.flags[2] & RF2_TROLL) text_out_c(TERM_L_BLUE, " troll");
	else if (lore.flags[2] & RF2_ORC) text_out_c(TERM_L_BLUE, " orc");
	else text_out(" creature");

	/* Describe location */
	if (r.level == 0)
	{
		text_out_c(TERM_SLATE, " lives in the town");
		old = TRUE;
	}
	else
	{
		text_out_c(TERM_SLATE, (lore.flags[0] & RF0_FORCE_DEPTH) ? " is found "
																 : " is normally found ");
		
		if (OPTION(depth_in_feet))
		{
			text_out_c(TERM_SLATE, format("at depths of %d feet",
			                            r.level * 50));
		}
		else
		{
			text_out_c(TERM_SLATE, format("on dungeon level %d",
			                            r.level));
		}
		old = TRUE;
	}

	if (old) text_out(", and");

	text_out(" moves");

	/* Random-ness */
	if (lore.flags[0] & (RF0_RAND_50 | RF0_RAND_25))
	{
		/* Adverb */
		if ((lore.flags[0] & RF0_RAND_50) && (lore.flags[0] & RF0_RAND_25))
		{
			text_out(" extremely");
		}
		else if (lore.flags[0] & RF0_RAND_50)
		{
			text_out(" somewhat");
		}
		else if (lore.flags[0] & RF0_RAND_25)
		{
			text_out(" a bit");
		}

		/* Adjective */
		text_out(" erratically");

		/* Hack -- Occasional conjunction */
		if (r.speed != 110) text_out(", and");
	}

	/* Speed */
	if (r.speed > 110)
	{
		if (r.speed > 130) text_out_c(TERM_GREEN, " incredibly");
		else if (r.speed > 120) text_out_c(TERM_GREEN, " very");
		text_out_c(TERM_GREEN, " quickly");
	}
	else if (r.speed < 110)
	{
		if (r.speed < 90) text_out_c(TERM_GREEN, " incredibly");
		else if (r.speed < 100) text_out_c(TERM_GREEN, " very");
		text_out_c(TERM_GREEN, " slowly");
	}
	else
	{
		text_out_c(TERM_GREEN, " at normal speed");
	}

	/* The code above includes "attack speed" */
	if (lore.flags[0] & RF0_NEVER_MOVE)
	{
		text_out(", but does not deign to chase intruders");
	}

	/* End this sentence */
	text_out(".  ");
}



/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(const monster_race& r, monster_lore& lore)
{
	int i;


	/* Hack -- Maximal kills */
	lore.tkills = MAX_S16B;

	/* Hack -- Maximal info */
	lore.wake = lore.ignore = UCHAR_MAX;

	/* Observe "maximal" attacks */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Examine "actual" blows */
		if (r.blow[i].effect || r.blow[i].method)
		{
			/* Hack -- maximal observations */
			lore.blows[i] = UCHAR_MAX;
		}
	}

	/* Hack -- maximal drops */
	lore.drop_gold = lore.drop_item =
	(((r.flags[0] & RF0_DROP_4D2) ? 8 : 0) +
	 ((r.flags[0] & RF0_DROP_3D2) ? 6 : 0) +
	 ((r.flags[0] & RF0_DROP_2D2) ? 4 : 0) +
	 ((r.flags[0] & RF0_DROP_1D2) ? 2 : 0) +
	 ((r.flags[0] & RF0_DROP_90)  ? 1 : 0) +
	 ((r.flags[0] & RF0_DROP_60)  ? 1 : 0));

	/* Hack -- but only "valid" drops */
	if (r.flags[0] & RF0_ONLY_GOLD) lore.drop_item = 0;
	if (r.flags[0] & RF0_ONLY_ITEM) lore.drop_gold = 0;

	/* Hack -- observe many spells */
	lore.cast_innate = UCHAR_MAX;
	lore.cast_spell = UCHAR_MAX;

	/* Hack -- know all the flags */
	C_COPY(lore.flags, r.flags, RACE_FLAG_STRICT_UB);
	C_COPY(lore.spell_flags, r.spell_flags, RACE_FLAG_SPELL_STRICT_UB);
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
void describe_monster(int r_idx, bool spoilers)
{
	/* Get the race, and a copy of the lore */
	const monster_race *r_ptr = &monster_type::r_info[r_idx];
	monster_lore lore = monster_type::l_list[r_idx];

	/* Assume some "obvious" flags */
	lore.flags[0] |= (r_ptr->flags[0] & RF0_OBVIOUS_MASK);
	lore.spell_flags[0] |= (r_ptr->spell_flags[0] & RSF0_OBVIOUS_MASK);

	/* Killing a monster reveals some properties */
	if (lore.tkills)
	{
		/* Know "race" flags */
		lore.flags[2] |= (r_ptr->flags[2] & RF2_RACE_MASK);

		/* Know "forced" flags */
		lore.flags[0] |= (r_ptr->flags[0] & (RF0_FORCE_DEPTH | RF0_FORCE_MAXHP));
	}

	/* Cheat -- know everything */
	if (OPTION(cheat_know) || spoilers) cheat_monster_lore(*r_ptr, lore);

	/* Show kills of monster vs. player(s) */
	if (!spoilers && OPTION(show_details))
		describe_monster_kills(*r_ptr, lore);

	/* Monster description */
	if (spoilers || OPTION(show_details))
		describe_monster_desc(*r_ptr);

	/* Describe the movement and level of the monster */
	describe_monster_movement(*r_ptr, lore);

	/* Describe experience */
	if (!spoilers) describe_monster_exp(*r_ptr, lore);

	/* Describe spells and innate attacks */
	describe_monster_spells(*r_ptr, lore);

	/* Describe monster "toughness" */
	if (!spoilers) describe_monster_toughness(*r_ptr, lore);

	/* Describe the abilities of the monster */
	describe_monster_abilities(*r_ptr, lore);

	/* Describe the monster drop */
	describe_monster_drop(*r_ptr, lore);

	/* Describe the known attacks */
	describe_monster_attack(*r_ptr, lore);

	/* Notice "Quest" monsters */
	if (lore.flags[0] & RF0_QUESTOR)
	{
		text_out("You feel an intense desire to kill this monster...  ");
	}

	/* All done */
	text_out("\n");
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
	monster_race *r_ptr = &monster_type::r_info[r_idx];

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
	if (!(r_ptr->flags[0] & RF0_UNIQUE))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, r_ptr->name());

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
	Term_addstr(-1, TERM_WHITE, "'):");
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

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

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

	/* Describe monster */
	roff_top(r_idx);
}
