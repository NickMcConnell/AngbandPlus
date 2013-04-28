/* File: monster1.c */

/*
 * Recall a monster on screen.  Display nearby monsters, get closest monster
 * in LOS.  Process player ghosts.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3]  = {"it",  "he",  "she"};
static cptr wd_his[3] = {"its", "his", "her"};
static cptr wd_him[3] = {"it",  "him", "her"};

/*
 * Descriptions of monster rarity.
 */
static char *wd_rarity(byte rarity, bool unique)
{
	static char rarity_desc[40];

	if (unique)
	{
		if      (rarity == 1) strcpy(rarity_desc, "very often encountered");
		else if (rarity == 2) strcpy(rarity_desc, "often encountered");
		else if (rarity == 3) strcpy(rarity_desc, "fairly often encountered");
		else if (rarity == 4) strcpy(rarity_desc, "infrequently encountered");
		else if (rarity <= 6) strcpy(rarity_desc, "seldom encountered");
		else if (rarity <= 9) strcpy(rarity_desc, "very seldom encountered");
		else                  strcpy(rarity_desc, "almost never encountered");
	}
	else
	{
		if      (rarity == 1) strcpy(rarity_desc, "very common");
		else if (rarity == 2) strcpy(rarity_desc, "common");
		else if (rarity <= 4) strcpy(rarity_desc, "not very common");
		else if (rarity <= 6) strcpy(rarity_desc, "fairly rare");
		else if (rarity <= 9) strcpy(rarity_desc, "rare");
		else                  strcpy(rarity_desc, "extremely rare");
	}

	return (rarity_desc);
}

/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c,s,p) \
(((c) == 1) ? (s) : (p))



/*
 * Determine if the armor class of the monster is known.
 *
 * You need to kill about 25 low-level or 4 high-level monsters to
 * know their armor class.
 */
bool know_armor(int r_idx, const monster_lore *l_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];

	int level = 20 + r_ptr->level;

	int kills = l_ptr->tkills;

	/* Uniques are easier to know */
	int diff = (r_ptr->flags1 & (RF1_UNIQUE)) ? 250 : 500;

	/* Those skilled in nature lore learn more quickly */
	diff -= MIN(0, get_skill(S_NATURE, 0, 150) - r_ptr->level);

	/* Test for knowledge */
	if (kills > div_round(diff, level)) return (TRUE);

	/* We don't know AC yet */
	return (FALSE);
}


/*
 * Determine if the starting mana of the monster is known.
 *
 * You need to kill about 25 low-level or 4 high-level monsters to
 * know their mana.
 */
bool know_mana(int r_idx, const monster_lore *l_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];

	int level = 20 + r_ptr->level;

	int kills = l_ptr->tkills;

	/* Uniques are easier to know */
	int diff = (r_ptr->flags1 & (RF1_UNIQUE)) ? 250 : 500;

	/* Those skilled in wizardry learn more quickly */
	diff -= MIN(0, get_skill(S_WIZARDRY, 0, 150) - r_ptr->level);

	/* Test for knowledge */
	if (kills > div_round(diff, level)) return (TRUE);

	/* We don't know mana yet */
	return (FALSE);
}


/*
 * Determine if the damage of a monster's melee attack is known.
 *
 * You need to suffer approximately 15-20 blows to know their damage,
 * less if you are skilled in Nature Lore.
 */
static bool know_damage(int r_idx, const monster_lore *l_ptr, int i)
{
	monster_race *r_ptr = &r_info[r_idx];

	int level = 20 + r_ptr->level;

	int hits = l_ptr->blows[i];

	int diff = 20 + (r_ptr->blow[i].d_dice * r_ptr->blow[i].d_side / 4);

	/* Nature lore gives faster knowledge */
	int skill_div = MIN(30, 10 + (get_skill(S_NATURE, 0, 300) / level));

	/* Normal monsters - standard difficulty */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) diff = diff * 10 / skill_div;

	/* Uniques are easier to learn about */
	else                                 diff = diff * 5 / skill_div;

	/* Chance to learn increases with depth and blows suffered */
	if ((level) * hits > 40 * diff) return (TRUE);

	/* We don't know damage yet */
	return (FALSE);
}


/*
 * Output monster description
 */
static void describe_monster_desc(int r_idx)
{
	const monster_race *r_ptr = &r_info[r_idx];
	char buf[2048];

	/* Simple method */
	(void)my_strcpy(buf, r_text + r_ptr->text, sizeof(buf));

	/* Hack -- player ghosts have extra text */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		/* Extract a gender (if applicable) */
		int msex = 0;
		if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
		else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;

		/* Add some text */
		strcat(buf, format("  You feel you know %s, and %s knows you.  This can only mean trouble.", wd_him[msex], wd_he[msex]));
	}

	/* Dump it */
	text_out_c(TERM_L_BLUE, buf);
	text_out("  ");

	/* Notice "Quest" monsters */
	if (r_ptr->flags1 & (RF1_QUESTOR))
	{
		text_out_c(TERM_PURPLE, "You feel an intense desire to kill this monster...  ");
	}

	/* New paragraph. */
	text_out("\n");

	/* Space if we're showing extra lines */
	if (Term->rows > 25) text_out("\n");
}



/*
 * Output monster spell descriptions
 */
static void describe_monster_spells(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n;
	int msex = 0;
	bool innate = FALSE;
	bool breath = FALSE;
	bool magic = FALSE;
	int vn;
	cptr vp[96];

	int spower;


	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Get spell power */
	spower = r_ptr->spell_power;


	/* Collect innate attacks */
	vn = 0;
	if (l_ptr->flags4 & (RF4_SHRIEK))		vp[vn++] = "shriek for help";
	if (l_ptr->flags4 & (RF4_LASH))
	{
		if (l_ptr->flags3 & (RF3_ANIMAL) || (r_ptr->blow[0].effect == RBE_ACID))
			vp[vn++] = "spit at you from a distance";
		else
			vp[vn++] = "lash you if nearby";
	}
	if (l_ptr->flags4 & (RF4_BOULDER))
	{
		if (spower < 10) vp[vn++] = "throw rocks";
		else vp[vn++] = "throw boulders";
	}
	if (l_ptr->flags4 & (RF4_SHOT))
	{
		if (spower < 5) vp[vn++] = "sling pebbles";
		else if (spower < 15) vp[vn++] = "sling leaden pellets";
		else vp[vn++] = "sling seeker shots";
	}
	if (l_ptr->flags4 & (RF4_ARROW))
	{
		if (spower < 8) vp[vn++] = "shoot little arrows";
		else if (spower < 15) vp[vn++] = "shoot arrows";
		else vp[vn++] = "shoot seeker arrows";
	}
	if (l_ptr->flags4 & (RF4_BOLT))
	{
		if (spower < 8) vp[vn++] = "fire bolts";
		else if (spower < 15) vp[vn++] = "fire crossbow quarrels";
		else vp[vn++] = "fire seeker bolts";
	}
	if (l_ptr->flags4 & (RF4_MISSL))
	{
		if (spower < 8) vp[vn++] = "fire little missiles";
		else if (spower < 15) vp[vn++] = "fire missiles";
		else vp[vn++] = "fire heavy missiles";
	}
	if (l_ptr->flags4 & (RF4_PMISSL))
	{
		if (l_ptr->flags2 & (RF2_MORGUL_MAGIC)) vp[vn++] = "hurl black darts";
		else vp[vn++] = "whip poisoned darts";
	}

	/* Describe innate attacks */
	if (vn)
	{
		/* Note innate */
		innate = TRUE;

		/* Intro */
		text_out(format("%^s may ", wd_he[msex]));
		if (l_ptr->flags2 & (RF2_ARCHER)) text_out("frequently ");

		/* List the innate attacks */
		for (n = 0; n < vn; n++)
		{
			/* Conjunction text between descriptions */
			if (n != 0)
			{
				if      (n < vn-1) text_out(", ");
				else if (n != 1)   text_out(", or ");
				else               text_out(" or ");
			}

			/* Describe */
			text_out(vp[n]);
		}
	}

	/* Collect breaths */
	vn = 0;
	if (l_ptr->flags4 & (RF4_BRTH_ACID))       vp[vn++] = "acid";
	if (l_ptr->flags4 & (RF4_BRTH_ELEC))       vp[vn++] = "lightning";
	if (l_ptr->flags4 & (RF4_BRTH_FIRE))       vp[vn++] = "fire";
	if (l_ptr->flags4 & (RF4_BRTH_COLD))       vp[vn++] = "frost";
	if (l_ptr->flags4 & (RF4_BRTH_POIS))       vp[vn++] = "poison";
	if (l_ptr->flags4 & (RF4_BRTH_PLAS))       vp[vn++] = "plasma";

	if (l_ptr->flags4 & (RF4_BRTH_LITE))       vp[vn++] = "light";
	if (l_ptr->flags4 & (RF4_BRTH_DARK))
	{
		if (l_ptr->flags2 & (RF2_MORGUL_MAGIC)) vp[vn++] = "Night";
		else vp[vn++] = "darkness";
	}
	if (l_ptr->flags4 & (RF4_BRTH_CONFU))      vp[vn++] = "confusion";
	if (l_ptr->flags4 & (RF4_BRTH_SOUND))      vp[vn++] = "sound";
	if (l_ptr->flags4 & (RF4_BRTH_SHARD))      vp[vn++] = "shards";
	if (l_ptr->flags4 & (RF4_BRTH_INER))       vp[vn++] = "inertia";
	if (l_ptr->flags4 & (RF4_BRTH_GRAV))       vp[vn++] = "gravity";
	if (l_ptr->flags4 & (RF4_BRTH_WIND))       vp[vn++] = "winds";
	if (l_ptr->flags4 & (RF4_BRTH_FORCE))      vp[vn++] = "force";

	if (l_ptr->flags4 & (RF4_BRTH_NEXUS))      vp[vn++] = "nexus";
	if (l_ptr->flags4 & (RF4_BRTH_NETHR))      vp[vn++] = "nether";
	if (l_ptr->flags4 & (RF4_BRTH_CHAOS))      vp[vn++] = "chaos";
	if (l_ptr->flags4 & (RF4_BRTH_DISEN))      vp[vn++] = "disenchantment";
	if (l_ptr->flags4 & (RF4_BRTH_TIME))       vp[vn++] = "time";
	if (l_ptr->flags4 & (RF4_BRTH_MANA))       vp[vn++] = "mana";

	if (l_ptr->flags4 & (RF4_XXX1))            vp[vn++] = "something";
	if (l_ptr->flags4 & (RF4_XXX2))            vp[vn++] = "something";
	if (l_ptr->flags4 & (RF4_XXX3))            vp[vn++] = "something";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		if (innate) text_out(", and breathe ");
		else        text_out(format("%^s may breathe ", wd_he[msex]));

		/* List the breaths */
		for (n = 0; n < vn; n++)
		{
			/* Conjunction text between descriptions */
			if (n != 0)
			{
				if      (n < vn-1) text_out(", ");
				else if (n != 1)   text_out(", or ");
				else               text_out(" or ");
			}

			/* Describe */
			text_out(vp[n]);
		}

		/* Note powerful breaths */
		if (l_ptr->flags2 & (RF2_POWERFUL)) text_out(" powerfully");
	}


	/* Collect spells */
	vn = 0;
	if (l_ptr->flags5 & (RF5_BALL_ACID))
	{
		if (spower < 70) vp[vn++] = "produce acid balls";
		else vp[vn++] = "produce acid storms";
	}
	if (l_ptr->flags5 & (RF5_BALL_ELEC))
	{
		if (spower < 70) vp[vn++] = "produce lightning balls";
		else vp[vn++] = "produce lightning storms";
	}
	if (l_ptr->flags5 & (RF5_BALL_FIRE))
	{
		if (l_ptr->flags2 & (RF2_UDUN_MAGIC))
		{
			if (spower < 70) vp[vn++] = "produce balls of hellfire";
			else if (spower < 110) vp[vn++] = "invoke storms of Udun-fire";
			else vp[vn++] = "call upon the fires of Udun";
		}
		else
		{
			if (spower < 70) vp[vn++] = "produce fire balls";
			else vp[vn++] = "produce fire storms";
		}
	}
	if (l_ptr->flags5 & (RF5_BALL_COLD))
	{
		if (l_ptr->flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 70) vp[vn++] = "produce spheres of deadly cold";
			else vp[vn++] = "invoke storms of deadly cold";
		}
		else
		{
			if (spower < 70) vp[vn++] = "produce frost balls";
			else vp[vn++] = "produce frost storms";
		}
	}
	if (l_ptr->flags5 & (RF5_BALL_POIS))
	{
		if (l_ptr->flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 15) vp[vn++] = "produce clouds of venom";
			else if (spower < 70) vp[vn++] = "produce venomous balls";
			else vp[vn++] = "raise storms of venom";
		}
		else
		{
			if (spower < 15) vp[vn++] = "produce stinking clouds";
			else if (spower < 70) vp[vn++] = "produce poison balls";
			else vp[vn++] = "produce storms of poison";
		}
	}
	if (l_ptr->flags5 & (RF5_BALL_LITE))
	{
		if (spower < 15) vp[vn++] = "produce spheres of light";
		else if (spower < 70) vp[vn++] = "produce explosions of light";
		else vp[vn++] = "invoke starbursts";
	}
	if (l_ptr->flags5 & (RF5_BALL_DARK))
	{
		if (l_ptr->flags2 & (RF2_MORGUL_MAGIC))
		{
			if (spower < 70) vp[vn++] = "produce spheres of Night";
			else vp[vn++] = "conjure up storms of Night";
		}
		else
		{
			if (spower < 70) vp[vn++] = "produce balls of darkness";
			else vp[vn++] = "produce storms of darkness";
		}
	}
	if (l_ptr->flags5 & (RF5_BALL_CONFU))
	{
		if (spower < 70) vp[vn++] = "produce confusion balls";
		else vp[vn++] = "produce storms of confusion";
	}
	if (l_ptr->flags5 & (RF5_BALL_SOUND))
	{
		if (spower < 15) vp[vn++] = "produce explosions of sound";
		else if (spower < 70) vp[vn++] = "produce thunderclaps";
		else vp[vn++] = "unleash storms of sound";
	}
	if (l_ptr->flags5 & (RF5_BALL_SHARD))
	{
		if (spower < 15) vp[vn++] = "produce blasts of shards";
		else if (spower < 70) vp[vn++] = "produce whirlwinds of shards";
		else vp[vn++] = "call up shardstorms";
	}
	if (l_ptr->flags5 & (RF5_BALL_WIND))
	{
		if (spower < 15) vp[vn++] = "produce blasts of wind";
		else if (spower < 80) vp[vn++] = "produce whirlwinds";
		else vp[vn++] = "call up cyclones";
	}
	if (l_ptr->flags5 & (RF5_BALL_STORM))
	{
		if (spower < 15) vp[vn++] = "produce little storms";
		else if (spower < 75) vp[vn++] = "produce whirlpools";
		else vp[vn++] = "call up raging storms";
	}
	if (l_ptr->flags5 & (RF5_BALL_NETHR))
	{
		if (spower < 25) vp[vn++] = "produce nether orbs";
		else if (spower < 75) vp[vn++] = "produce nether balls";
		else vp[vn++] = "invoke nether storms";
	}
	if (l_ptr->flags5 & (RF5_BALL_CHAOS))
	{
		if (spower < 20) vp[vn++] = "produce spheres of chaos";
		else if (spower < 80) vp[vn++] = "produce explosions of chaos";
		else vp[vn++] = "call up maelstroms of raw chaos";
	}
	if (l_ptr->flags5 & (RF5_BALL_MANA))
	{
		if (spower < 20) vp[vn++] = "produce manabursts";
		else if (spower < 80) vp[vn++] = "produce balls of mana";
		else vp[vn++] = "invoke mana storms";
	}
	if (l_ptr->flags5 & (RF5_BOLT_ACID))    vp[vn++] = "produce acid bolts";
	if (l_ptr->flags5 & (RF5_BOLT_ELEC))    vp[vn++] = "produce lightning bolts";
	if (l_ptr->flags5 & (RF5_BOLT_FIRE))    vp[vn++] = "produce fire bolts";
	if (l_ptr->flags5 & (RF5_BOLT_COLD))    vp[vn++] = "produce frost bolts";
	if (l_ptr->flags5 & (RF5_BOLT_POIS))    vp[vn++] = "produce poison bolts";
	if (l_ptr->flags5 & (RF5_BOLT_PLAS))    vp[vn++] = "produce plasma bolts";
	if (l_ptr->flags5 & (RF5_BOLT_ICE))     vp[vn++] = "produce ice bolts";
	if (l_ptr->flags5 & (RF5_BOLT_WATER))   vp[vn++] = "produce water bolts";
	if (l_ptr->flags5 & (RF5_BOLT_NETHR))   vp[vn++] = "produce nether bolts";
	if (l_ptr->flags5 & (RF5_BOLT_MANA))
	{
		if      (spower <  6) vp[vn++] = "produce magic missiles";
		else if (spower < 40) vp[vn++] = "produce magic bolts";
		else                  vp[vn++] = "produce mana bolts";
	}
	if (l_ptr->flags5 & (RF5_BEAM_ELEC))
	{
		if      (spower < 12) vp[vn++] = "shoot sparks of lightning";
		else if (spower < 40) vp[vn++] = "shoot beams of lightning";
		else                  vp[vn++] = "shoot thunderbolts of lightning";
	}
	if (l_ptr->flags5 & (RF5_BEAM_ICE))
	{
		if      (spower <  7) vp[vn++] = "cast streams of ice";
		else if (spower < 40) vp[vn++] = "cast spears of ice";
		else                  vp[vn++] = "cast lances of ice";
	}
	if (l_ptr->flags5 & (RF5_BEAM_NETHR))
	{
		if (spower < 40) vp[vn++] = "cast beams of nether";
		else if (spower < 90) vp[vn++] = "hurl lances of nether";
		else vp[vn++] = "shoot rays of death";
	}
	if (l_ptr->flags5 & (RF5_ARC__HFIRE))
	{
		if (l_ptr->flags2 & (RF2_UDUN_MAGIC))
		{
			if (spower < 50) vp[vn++] = "produce columns of hellfire";
			else vp[vn++] = "envelop you in hellfire";
		}
		else
		{
			if (spower < 50) vp[vn++] = "produce columns of fire";
			else vp[vn++] = "raise storms of fire";
		}
	}

	if (l_ptr->flags6 & (RF6_HELLDARK))
	{
		if (spower < 40) vp[vn++] = "conjure up hellish darkness";
		else vp[vn++] = "invoke the wrath of Hell";
	}

	if (l_ptr->flags6 & (RF6_HOLY_SMITE))
	{
		if (spower < 40) vp[vn++] = "call down holy lances";
		else vp[vn++] = "invoke Divine wrath";
	}



	if (l_ptr->flags5 & (RF5_ARC__FORCE))
	{
		if (spower < 40) vp[vn++] = "thrust you away";
		else if (spower < 100) vp[vn++] = "hurl you away";
		else vp[vn++] = "snatch you up, and throw you away";
	}
	if (l_ptr->flags6 & (RF6_HASTE))        vp[vn++] = "haste-self";
	if (l_ptr->flags6 & (RF6_ADD_MANA))     vp[vn++] = "restore mana";
	if (l_ptr->flags6 & (RF6_HEAL))         vp[vn++] = "heal-self";
	if (l_ptr->flags6 & (RF6_CURE))         vp[vn++] = "cure what ails it";
	if (l_ptr->flags6 & (RF6_BLINK))        vp[vn++] = "blink-self";
	if (l_ptr->flags6 & (RF6_TPORT))        vp[vn++] = "teleport-self";
	if (l_ptr->flags6 & (RF6_TELE_SELF_TO)) vp[vn++] = "blink towards you";
	if (l_ptr->flags6 & (RF6_TELE_TO))      vp[vn++] = "teleport to";
	if (l_ptr->flags6 & (RF6_TELE_AWAY))    vp[vn++] = "teleport away";
	if (l_ptr->flags6 & (RF6_TELE_LEVEL))   vp[vn++] = "teleport level";
	if (l_ptr->flags6 & (RF6_DARKNESS))     vp[vn++] = "create darkness";
	if (l_ptr->flags6 & (RF6_DARKNESS))     vp[vn++] = "curse you and your equipment";
	if (l_ptr->flags6 & (RF6_TRAPS))        vp[vn++] = "create traps";
	if (l_ptr->flags6 & (RF6_FORGET))       vp[vn++] = "cause amnesia";
	if (l_ptr->flags6 & (RF6_DRAIN_MANA))   vp[vn++] = "drain mana";
	if (l_ptr->flags6 & (RF6_MIND_BLAST))   vp[vn++] = "cause mind blasting";
	if (l_ptr->flags6 & (RF6_WOUND))
	{
		if      (spower <  7)                vp[vn++] = "cause light wounds";
		else if (spower < 15)                vp[vn++] = "cause medium wounds";
		else if (spower < 30)                vp[vn++] = "cause serious wounds";
		else if (spower < 50)                vp[vn++] = "cause critical wounds";
		else                                 vp[vn++] = "cause mortal wounds";
	}
	if (l_ptr->flags6 & (RF6_HUNGER))       vp[vn++] = "cause starvation";
	if (l_ptr->flags6 & (RF6_SCARE))        vp[vn++] = "terrify";
	if (l_ptr->flags6 & (RF6_BLIND))        vp[vn++] = "blind";
	if (l_ptr->flags6 & (RF6_CONF))         vp[vn++] = "confuse";
	if (l_ptr->flags6 & (RF6_SLOW))         vp[vn++] = "slow";
	if (l_ptr->flags6 & (RF6_HOLD))         vp[vn++] = "paralyze";

	/* Save current spell index. */
	m = vn;

	/* Summons are described somewhat differently. */
	if (l_ptr->flags7)
	{
		/* Summons */
		if (l_ptr->flags7 & (RF7_S_KIN))
		{
			if (l_ptr->flags1 & (RF1_UNIQUE))
			{
				if (r_ptr->flags1 & (RF1_FEMALE)) vp[vn++] = "her minions";
				else if (r_ptr->flags1 & (RF1_MALE)) vp[vn++] = "his minions";
				else vp[vn++] = "its minions";
			}
			else
			{
				vp[vn++] = "similar monsters";
			}
		}
		if (l_ptr->flags7 & (RF7_S_MONSTER))       vp[vn++] = "a monster";
		if (l_ptr->flags7 & (RF7_S_MONSTERS))      vp[vn++] = "monsters";
		if (l_ptr->flags7 & (RF7_S_BEETLE))        vp[vn++] = "a beetle";
		if (l_ptr->flags7 & (RF7_S_ANT))           vp[vn++] = "ants";
		if (l_ptr->flags7 & (RF7_S_SPIDER))        vp[vn++] = "spiders";
		if (l_ptr->flags7 & (RF7_S_HOUND))         vp[vn++] = "hounds";
		if (l_ptr->flags7 & (RF7_S_ANIMAL))        vp[vn++] = "natural creatures";
		if (l_ptr->flags7 & (RF7_S_THIEF))         vp[vn++] = "thieves";
		if (l_ptr->flags7 & (RF7_S_BERTBILLTOM))   vp[vn++] = "his friends";
		if (l_ptr->flags7 & (RF7_S_ORC))           vp[vn++] = "his rider";
		if (l_ptr->flags7 & (RF7_S_ANGEL))         vp[vn++] = "an angel";
		if (l_ptr->flags7 & (RF7_S_DRAGON))        vp[vn++] = "a dragon";
		if (l_ptr->flags7 & (RF7_S_HI_DRAGON))     vp[vn++] = "Ancient Dragons";
		if (l_ptr->flags7 & (RF7_S_DEMON))         vp[vn++] = "a demon";
		if (l_ptr->flags7 & (RF7_S_HI_DEMON))      vp[vn++] = "Greater Demons";
		if (l_ptr->flags7 & (RF7_S_UNDEAD))        vp[vn++] = "an undead";
		if (l_ptr->flags7 & (RF7_S_HI_UNDEAD))     vp[vn++] = "Greater Undead";
		if (l_ptr->flags7 & (RF7_S_WRAITH))        vp[vn++] = "the Ringwraiths";
		if (l_ptr->flags7 & (RF7_S_UNIQUE))        vp[vn++] = "Unique Monsters";
	}

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if ((breath) || (innate))
		{
			text_out(", and is also");
		}
		else
		{
			text_out(format("%^s is", wd_he[msex]));
		}


		/* Verb Phrase */
		text_out(" magical, casting");

		/* Describe magic */
		if ((l_ptr->flags2 & (RF2_UDUN_MAGIC)) &&
		    (l_ptr->flags2 & (RF2_MORGUL_MAGIC)))
			text_out_c(TERM_PURPLE, " perilous spells of Morgul and of Udun");
		else if (l_ptr->flags2 & (RF2_MORGUL_MAGIC))
			text_out_c(TERM_PURPLE, " Morgul-spells");
		else if (l_ptr->flags2 & (RF2_UDUN_MAGIC))
			text_out_c(TERM_L_RED, " spells of Udun");
		else
			text_out(" spells");

		/* Adverb */
		if (l_ptr->flags2 & (RF2_SMART))
		{
			/* Suppress text if monster has both Udun and Morgul-magic. */
			if (!((l_ptr->flags2 & (RF2_UDUN_MAGIC)) &&
				(l_ptr->flags2 & (RF2_MORGUL_MAGIC))))
			{
				text_out(" intelligently");
			}
		}

		/* Normal spells */
		for (n = 0; n < m; n++)
		{
			if (n == 0)       text_out(" which ");
			else if (n < m-1) text_out(", ");
			else if (n != 1)  text_out(", or ");
			else              text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* Summons */
		for (n = m; n < vn; n++)
		{
			if (n == 0) text_out(" which summon ");
			else if (n == m) text_out(", or summon ");
			else if (n < vn-1) text_out(", ");
			else if (n == m+1) text_out(" or ");
			else text_out(", or ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* End the sentence about innate/other spells */
	if (innate || breath || magic)
	{
		/* Total casting */
		m = l_ptr->ranged;

		/* Average frequency */
		n = r_ptr->freq_ranged;

		/* Describe the spell frequency */
		if (m > 50)
		{
			text_out(format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			text_out(format("; about 1 time in %d", 100 / n));
		}

		/* Describe monster mana */
		if (r_ptr->mana && know_mana(r_idx, l_ptr))
		{
			/* Mana */
			text_out(format(" with a mana rating of %d", r_ptr->mana));

		}

		/* End this sentence */
		text_out(".  ");
	}
}


/*
 * Output monster drop description
 */
static void describe_monster_drop(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	bool add_an_n = FALSE;

	int n;

	cptr p;

	int msex = 0;

	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Drops gold and/or items */
	if (l_ptr->drop_gold || l_ptr->drop_item)
	{
		/* Intro */
		text_out(format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			text_out(" a");
			add_an_n = TRUE;
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
		if (l_ptr->flags1 & (RF1_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (l_ptr->flags1 & (RF1_DROP_GOOD))
		{
			p = " good";
			add_an_n = FALSE;
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (l_ptr->drop_item)
		{
			/* Handle singular "an" */
			if ((add_an_n) && (!(l_ptr->flags1 & (RF1_DROP_CHEST))))
				text_out("n");
			add_an_n = FALSE;

			/* Dump "object(s)" */
			if (p) text_out(p);
			if (l_ptr->flags1 & (RF1_DROP_CHEST)) text_out(" chest");
			else text_out(" object");
			if (n != 1) text_out("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->drop_gold)
		{
			/* Cancel prefix */
			if (!p) add_an_n = FALSE;

			/* Handle singular "an" */
			if (add_an_n) text_out("n");
			add_an_n = FALSE;

			/* Dump "treasure(s)" */
			if (p) text_out(p);
			text_out(" treasure");
			if (n != 1) text_out("s");
		}

		/* End this sentence */
		text_out(".  ");
	}
}


/*
 * Output monster melee attacks description
 */
static void describe_monster_attack(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n, r;
	cptr p, q;

	int msex = 0;

	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if (!l_ptr->blows[m]) continue;


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
			case RBM_HIT:           p = "hit"; break;
			case RBM_TOUCH:         p = "touch"; break;
			case RBM_PUNCH:         p = "punch"; break;
			case RBM_KICK:          p = "kick"; break;
			case RBM_CLAW:          p = "claw"; break;
			case RBM_BITE:          p = "bite"; break;
			case RBM_PECK:          p = "peck"; break;
			case RBM_STING:         p = "sting"; break;
			case RBM_XXX1:          break;
			case RBM_BUTT:          p = "butt"; break;
			case RBM_CRUSH:         p = "crush"; break;
			case RBM_ENGULF:        p = "engulf"; break;
			case RBM_CRAWL:         p = "crawl on you"; break;
			case RBM_DROOL:         p = "drool on you"; break;
			case RBM_SPIT:          p = "spit"; break;
			case RBM_SLIME:         p = "slime"; break;
			case RBM_GAZE:          p = "gaze"; break;
			case RBM_WAIL:          p = "wail"; break;
			case RBM_SPORE:         p = "release spores"; break;
			case RBM_XXX4:          break;
			case RBM_BEG:           p = "beg"; break;
			case RBM_INSULT:        p = "insult"; break;
			case RBM_SNEER:         p = "sneer"; break;
			case RBM_XXX5:          break;
		}


		/* Default effect */
		q = NULL;

		/* Get the effect */
		switch (effect)
		{
			case RBE_HURT:          q = "attack"; break;
			case RBE_WOUND:         q = "wound"; break;
			case RBE_BATTER:        q = "stun"; break;
			case RBE_SHATTER:       q = "shatter"; break;

			case RBE_UN_BONUS:      q = "disenchant"; break;
			case RBE_UN_POWER:      q = "drain charges"; break;
			case RBE_LOSE_MANA:     q = "drain mana"; break;
			case RBE_EAT_GOLD:      q = "steal gold"; break;
			case RBE_EAT_ITEM:      q = "steal items"; break;
			case RBE_EAT_FOOD:      q = "eat your food"; break;
			case RBE_EAT_LITE:      q = "absorb light"; break;
			case RBE_HUNGER:        q = "cause hunger"; break;

			case RBE_POISON:        q = "poison"; break;
			case RBE_ACID:          q = "shoot acid"; break;
			case RBE_ELEC:          q = "electrocute"; break;
			case RBE_FIRE:          q = "burn"; break;
			case RBE_COLD:          q = "freeze"; break;

			case RBE_BLIND:         q = "blind"; break;
			case RBE_CONFUSE:       q = "confuse"; break;
			case RBE_TERRIFY:       q = "terrify"; break;
			case RBE_PARALYZE:      q = "paralyze"; break;
			case RBE_HALLU:         q = "induce hallucinations"; break;
			case RBE_DISEASE:       q = "cause disease"; break;

			case RBE_LOSE_STR:      q = "reduce strength"; break;
			case RBE_LOSE_INT:      q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:      q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:      q = "reduce dexterity"; break;
			case RBE_LOSE_CON:      q = "reduce constitution"; break;
			case RBE_LOSE_CHR:      q = "reduce charisma"; break;
			case RBE_LOSE_LUC:      q = "reduce luck"; break;
			case RBE_LOSE_ALL:      q = "reduce all stats"; break;

			case RBE_EXP_10:        q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:        q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:        q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:        q = "lower experience (by 80d6+)"; break;
		}


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
			if (d1 && d2 && know_damage(r_idx, l_ptr, m))
			{
				/* Display the damage */
				text_out(" with damage");
				text_out(format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		/* Does it miss? */
		if (l_ptr->flags2 & (RF2_NOMISS))
		{
			text_out(format(", and %s never misses.  ", wd_he[msex]));
		}
		else
		{
			text_out(".  ");
		}
	}

	/* Notice lack of attacks */
	else if (l_ptr->flags1 & (RF1_NEVER_BLOW))
	{
		text_out(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		text_out(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}
}



/*
 * Output monster abilities description
 */
static void describe_monster_abilities(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int n;

	int vn;
	cptr vp[64];

	int msex = 0;
	bool flag_resist = FALSE;

	char name[DESC_LEN];

	/* Get monster name */
	(void)my_strcpy(name, r_name + r_ptr->name, sizeof(name));

	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Collect special abilities. */
	vn = 0;
	if (l_ptr->flags2 & (RF2_OPEN_DOOR)) vp[vn++] = "open doors";
	if (l_ptr->flags2 & (RF2_BASH_DOOR)) vp[vn++] = "bash down doors";
	if (l_ptr->flags2 & (RF2_PASS_WALL)) vp[vn++] = "pass through walls";
	if (l_ptr->flags2 & (RF2_KILL_WALL)) vp[vn++] = "bore through walls";
	if (l_ptr->flags2 & (RF2_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
	if (l_ptr->flags2 & (RF2_TAKE_ITEM)) vp[vn++] = "pick up objects";
	if (l_ptr->flags2 & (RF2_KILL_ITEM)) vp[vn++] = "destroy objects";


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
			else if (n == 1) text_out(" and ");
			else text_out(", and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}

	/* Describe special abilities. */
	if (l_ptr->flags2 & (RF2_INVISIBLE))
	{
		text_out(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_COLD_BLOOD))
	{
		text_out(format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_EMPTY_MIND))
	{
		text_out(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_WEIRD_MIND))
	{
		text_out(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_MULTIPLY))
	{
		text_out(format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_REGENERATE))
	{
		text_out(format("%^s regenerates quickly.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & (RF2_CLOUD_SURROUND))
	{
		int typ = 0, dam = 0, rad = 0;

		/* Get type of cloud */
		cloud_surround(r_idx, &typ, &dam, &rad);

		/* We emit something */
		if (typ)
		{
			text_out(format("%^s is surrounded by ", wd_he[msex]));

			/* Describe cloud */
			if (typ == GF_FIRE)           text_out("fire");
			else if (typ == GF_COLD)      text_out("frost");
			else if (typ == GF_ELEC)      text_out("lightning");
			else if (typ == GF_ACID)      text_out("acidic smoke");
			else if (typ == GF_POIS)      text_out("noxious gases");
			else if (typ == GF_SOUND)     text_out("a cacophony of sound");
			else if (typ == GF_SPORE)     text_out("spores");
			else if (typ == GF_DARK)      text_out("darkness");
			else if (typ == GF_DARK_WEAK) text_out("darkness");
			else                          text_out("powerful forces");
			text_out(".  ");
		}
	}

	/* Collect susceptibilities */
	vn = 0;
	if (l_ptr->flags3 & (RF3_HURT_ROCK)) vp[vn++] = "rock remover";
	if (l_ptr->flags3 & (RF3_HURT_LITE)) vp[vn++] = "bright light";
	if (l_ptr->flags3 & (RF3_HURT_FIRE)) vp[vn++] = "fire";
	if (l_ptr->flags3 & (RF3_HURT_COLD)) vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" is badly hurt by ");
			else if (n < vn-1) text_out(", ");
			else if (n == 1) text_out(" and ");
			else text_out(", and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect immunities */
	vn = 0;

	if (l_ptr->flags3 & (RF3_IM_ACID)) vp[vn++] = "acid";
	if (l_ptr->flags3 & (RF3_IM_ELEC)) vp[vn++] = "lightning";
	if (l_ptr->flags3 & (RF3_IM_FIRE)) vp[vn++] = "fire";
	if (l_ptr->flags3 & (RF3_IM_COLD)) vp[vn++] = "cold";
	if (l_ptr->flags3 & (RF3_IM_POIS)) vp[vn++] = "poison";
	if (l_ptr->flags3 & (RF3_RES_BLUNT)) vp[vn++] = "blunt weapons";
	if (l_ptr->flags3 & (RF3_RES_EDGED)) vp[vn++] = "edged weapons";
	if (l_ptr->flags2 & (RF2_PASS_WALL)) vp[vn++] = "barehanded attacks";

	/* Describe immunities */
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
			else if (n == 1) text_out(" and ");
			else text_out(", and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");

		/* Note that some resistances are known */
		flag_resist = TRUE;
	}


	/* Collect high resistances */
	vn = 0;
	if ((l_ptr->flags4 & (RF4_BRTH_LITE)) || (r_ptr->d_char == 'A'))
		vp[vn++] = "light";
	if ((l_ptr->flags4 & (RF4_BRTH_DARK)) ||
	    (l_ptr->flags2 & (RF2_MORGUL_MAGIC)) ||
	    ((l_ptr->flags3 & (RF3_ORC)) && (flag_resist)))
	{
		vp[vn++] = "darkness";
	}

	if (l_ptr->flags4 & (RF4_BRTH_CONFU)) vp[vn++] = "confusion";
	if (l_ptr->flags4 & (RF4_BRTH_SOUND)) vp[vn++] = "sound";
	if (l_ptr->flags4 & (RF4_BRTH_SHARD)) vp[vn++] = "shards";
	if (l_ptr->flags4 & (RF4_BRTH_INER)) vp[vn++] = "inertia";
	if (l_ptr->flags4 & (RF4_BRTH_GRAV)) vp[vn++] = "gravity";
	if (l_ptr->flags4 & (RF4_BRTH_FORCE)) vp[vn++] = "force";
	if ((l_ptr->flags4 & (RF4_BRTH_WIND)) || (prefix(name, "Air")))
		vp[vn++] = "winds";


	if ((l_ptr->flags3 & (RF3_RES_WATER)) || (prefix(name, "Water")))
		vp[vn++] = "water";

	if ((l_ptr->flags4 & (RF4_BRTH_PLAS)) || (l_ptr->flags3 & (RF3_RES_PLAS)) ||
	    ((vn) && ((l_ptr->flags3 & (RF3_IM_ELEC)) || (l_ptr->flags3 & (RF3_IM_FIRE)))))
	{
		vp[vn++] = "plasma";
	}

	if (((l_ptr->flags3 & (RF3_ORC)) && (flag_resist)) ||
	    (l_ptr->flags3 & (RF3_RES_NETHR)) ||
	    (l_ptr->flags4 & (RF4_BRTH_NETHR))) vp[vn++] = "nether";

	if ((l_ptr->flags3 & (RF3_RES_NEXUS)) ||
	    (l_ptr->flags4 & (RF4_BRTH_NEXUS))) vp[vn++] = "nexus";

	if ((l_ptr->flags4 & (RF4_BRTH_CHAOS)) ||
	    (l_ptr->flags3 & (RF3_RES_CHAOS))) vp[vn++] = "chaos";

	if ((l_ptr->flags3 & (RF3_RES_DISEN)) || (l_ptr->flags4 & (RF4_BRTH_DISEN)))
		vp[vn++] = "disenchantment";
	if (l_ptr->flags3 & (RF3_RES_TPORT)) vp[vn++] = "teleportation";


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
			else if (n == 1) text_out(" and ");
			else text_out(", and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if ((l_ptr->flags3 & (RF3_NO_STUN)) ||
	    (l_ptr->flags4 & (RF4_BRTH_SOUND)) ||
	    (l_ptr->flags4 & (RF4_BRTH_FORCE)))
	{
		vp[vn++] = "stunned";
	}
	if (l_ptr->flags3 & (RF3_NO_FEAR))
		vp[vn++] = "frightened";
	if ((l_ptr->flags3 & (RF3_NO_CONF)) ||
	    (l_ptr->flags4 & (RF4_BRTH_CONFU)) ||
	    (l_ptr->flags4 & (RF4_BRTH_CHAOS)))
	{
		vp[vn++] = "confused";
	}
	if (l_ptr->flags3 & (RF3_NO_SLEEP))
		vp[vn++] = "slept";
	if (l_ptr->flags3 & (RF3_IM_BLUNT))
		vp[vn++] = "much harmed by blunt weapons";
	if (l_ptr->flags3 & (RF3_IM_EDGED))
		vp[vn++] = "much harmed by edged weapons";

	/* Describe non-effects */
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
			else if (n == 1) text_out(" or ");
			else text_out(", or ");

			/* Dump */
			text_out(vp[n]);
		}

		/* Dodges blows - add to other text */
		if (l_ptr->flags2 & (RF2_EVASIVE))
			text_out(format(", and %s often dodges attacks", wd_he[msex]));

		/* End */
		text_out(".  ");
	}

	/* Dodges blows - insert as a sentence */
	else if (l_ptr->flags2 & (RF2_EVASIVE))
		text_out(format("%^s often dodges attacks.  ", wd_he[msex]));


	/* Do we know how aware it is? */
	if ((((int)l_ptr->wake * (int)l_ptr->wake) > r_ptr->sleep) ||
	    (l_ptr->ignore == MAX_UCHAR) ||
	    ((r_ptr->sleep == 0) && (l_ptr->tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
			act = "is nearly oblivious of";
		}
		else if (r_ptr->sleep > 140)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 115)
		{
			act = "pays very little attention to";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays little attention to";
		}
		else if (r_ptr->sleep > 70)
		{
			act = "tends to overlook";
		}
		else if (r_ptr->sleep > 45)
		{
			act = "takes quite a while to see";
		}
		else if (r_ptr->sleep > 30)
		{
			act = "takes a while to see";
		}
		else if (r_ptr->sleep > 20)
		{
			act = "will soon notice";
		}
		else if (r_ptr->sleep > 14)
		{
			act = "is fairly observant of";
		}
		else if (r_ptr->sleep > 8)
		{
			act = "is observant of";
		}
		else if (r_ptr->sleep > 4)
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

		text_out(format("%^s %s intruders, which %s may notice from %d %s.  ",
		     wd_he[msex], act, wd_he[msex],
		     ((use_metric) ? 3 : 10) * r_ptr->aaf,
		     ((use_metric) ? "meters" : "feet")));
	}

	/* Describe escorts */
	if ((l_ptr->flags1 & (RF1_ESCORT)) || (l_ptr->flags1 & (RF1_ESCORTS)))
	{
		text_out(format("%^s usually appears with %sescorts.  ",
			    wd_he[msex],
			    ((l_ptr->flags1 & (RF1_ESCORTS)) ? "lots of " : "")));
	}

	/* Describe friends */
	else if ((l_ptr->flags1 & (RF1_FRIEND)) || (l_ptr->flags1 & (RF1_FRIENDS)))
	{
		text_out(format("%^s usually appears in %sgroups.  ",
			    wd_he[msex],
			    ((l_ptr->flags1 & (RF1_FRIENDS)) ? "large " : "small ")));
	}
}




/*
 * Output monster kills description
 */
static void describe_monster_kills(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;

	bool out = TRUE;


	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Treat uniques differently */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->deaths)
		{
			/* Killed ancestors */
			text_out(format("%^s has slain %d of your ancestors",
				    wd_he[msex], l_ptr->deaths));

			/* But we've also killed it */
			if (dead)
			{
				text_out(", but you have taken revenge!  ");
			}

			/* Unavenged (ever) */
			else
			{
				text_out(format(", who %s unavenged.  ",
					    plural(l_ptr->deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			text_out("You have slain this foe.  ");
		}

	}

	/* Not unique, but killed us */
	else if (l_ptr->deaths)
	{
		/* Dead ancestors */
		text_out(format("%d of your ancestors %s been killed by this creature, ",
			l_ptr->deaths, plural(l_ptr->deaths, "has", "have")));

		/* Some kills this life */
		if (l_ptr->pkills)
		{
			text_out(format("and you have exterminated at least %d of the creatures.  ",
				l_ptr->pkills));
		}

		/* Some kills past lives */
		else if (l_ptr->tkills)
		{
			text_out(format("and %s have exterminated at least %d of the creatures.  ",
				"your ancestors", l_ptr->tkills));
		}

		/* No kills */
		else
		{
			text_out("and it is not ever known to have been defeated.  ");
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->pkills)
		{
			text_out(format("You have killed at least %d of these creatures.  ",
				l_ptr->pkills));
		}

		/* Killed some last life */
		else if (l_ptr->tkills)
		{
			text_out(format("Your ancestors have killed at least %d of these creatures.  ",
				l_ptr->tkills));
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

/*
 * Output monster hitpoints description
 */
static void describe_monster_toughness(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;


	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	/* Describe monster "toughness" */
	if (know_armor(r_idx, l_ptr))
	{
		/* Armor */
		text_out(format("%^s has an armor rating of %d",
		            wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (l_ptr->flags1 & (RF1_FIXED_HPS))
		{
			text_out(format(" and a life rating of %d.  ",
			            r_ptr->hitpoints));
		}

		/* Variable hitpoints */
		else
		{
			text_out(format(" and an approximate life rating of %d.  ",
			            r_ptr->hitpoints));
		}
	}
}

/*
 * Output monster experience description
 */
static void describe_monster_exp(int r_idx, const monster_lore *l_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Describe experience if known */
	if (l_ptr->tkills)
	{
		/* Introduction */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			text_out("Killing");
		}
		else
		{
			text_out("A kill of");
		}
		text_out(" this creature");


		/* Calculate and describe the exp value of this monster */
		if (TRUE)
		{
			long i, j;

			/* Calculate integer experience */
			i = monster_exp(r_ptr);

			/* Calculate fractional experience */
			j = (monster_exp_frac(r_ptr) + 5) / 10;

			/* Describe the experience */
			text_out(format(" is worth %ld.%02ld point%s",
			    (long)i, (long)j,
			    (((i == 1) && (j == 0)) ? "" : "s")));

			/* Mention the dependence on the player's skill */
			text_out(" for a character of your skill.  ");
		}
	}
}

/*
 * Output monster movement description
 */
static void describe_monster_movement(int r_idx, const monster_lore *l_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool old = FALSE;

	bool unique = (r_ptr->flags1 & (RF1_UNIQUE));

	int msex = 0;

	/* Extract a gender (if applicable) */
	if      (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE))   msex = 1;


	text_out("This");

	if (l_ptr->flags3 & (RF3_ANIMAL)) text_out_c(TERM_L_BLUE, " natural");
	if (l_ptr->flags3 & (RF3_EVIL)) text_out_c(TERM_L_BLUE, " evil");
	if (l_ptr->flags3 & (RF3_UNDEAD)) text_out_c(TERM_L_BLUE, " undead");

	if (l_ptr->flags3 & (RF3_DRAGON)) text_out_c(TERM_L_BLUE, " dragon");
	else if (l_ptr->flags3 & (RF3_DEMON)) text_out_c(TERM_L_BLUE, " demon");
	else if (l_ptr->flags3 & (RF3_GIANT)) text_out_c(TERM_L_BLUE, " giant");
	else if (l_ptr->flags3 & (RF3_TROLL)) text_out_c(TERM_L_BLUE, " troll");
	else if (l_ptr->flags3 & (RF3_ORC)) text_out_c(TERM_L_BLUE, " orc");
	else text_out(" creature");

	/* Describe location */
	if (r_ptr->level == 0)
	{
		text_out(" lives in the town");
		old = TRUE;
	}
	else if ((l_ptr->tkills) || (l_ptr->flags & (LORE_KNOWN_DEPTH)))
	{
		char depth_desc[DESC_LEN];

		if ((depth_in_feet) && (use_metric))
		{
			strcpy(depth_desc, format("at depths of %d meters", r_ptr->level * 15));
		}
		else if (depth_in_feet)
		{
			strcpy(depth_desc, format("at depths of %d feet", r_ptr->level * 50));
		}
		else
		{
			strcpy(depth_desc, format("on dungeon level %d", r_ptr->level));
		}


		/* Build the description of rarity and location. */
		if (r_ptr->flags1 & (RF1_QUESTOR))
		{
			/* Questor monsters are fixed-depth, and always appear. */
			text_out(format(" is always found %s", depth_desc));
		}

		else if (r_ptr->flags1 & (RF1_FORCE_DEPTH))
		{
			text_out(format(" is %s, never found above %s",
				wd_rarity(r_ptr->rarity, unique),
				depth_desc));
		}
		else
		{
			text_out(format(" is %s, normally found %s",
				wd_rarity(r_ptr->rarity, unique),
				depth_desc));
		}

		old = TRUE;
	}

	/* Movement */
	if (old)
	{
		text_out(", and ");
	}
	else
	{
		text_out(" ");
		old = TRUE;
	}

	/* Monsters that never move can strike */
	if (l_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Nastier monster */
		if ((!(l_ptr->flags1 & (RF1_NEVER_BLOW))) && (r_ptr->speed > 110))
		{
			text_out("strikes");
		}
		else
		{
			text_out("acts");
		}
	}
	else
	{
		text_out("moves");

		/* Random-ness -- only for monsters that can move */
		if ((l_ptr->flags1 & (RF1_RAND_50)) || (l_ptr->flags1 & (RF1_RAND_25)))
		{
			/* Adverb */
			if ((l_ptr->flags1 & (RF1_RAND_50)) && (l_ptr->flags1 & (RF1_RAND_25)))
			{
				text_out(" extremely");
			}
			else if (l_ptr->flags1 & (RF1_RAND_50))
			{
				text_out(" somewhat");
			}
			else if (l_ptr->flags1 & (RF1_RAND_25))
			{
				text_out(" a bit");
			}

			/* Adjective */
			text_out(" erratically");

			/* Hack -- Occasional conjunction */
			if (r_ptr->speed != 110) text_out(", and");
		}
	}

	/* Speed */
	if (r_ptr->flags2 & (RF2_SAMESPD))
	{
		if (r_ptr->speed > 110)
		{
			if (r_ptr->speed > 120) text_out(" much");
			text_out(" faster than you");
		}
		else if (r_ptr->speed < 110)
		{
			if (r_ptr->speed < 100) text_out(" much");
			text_out(" slower than you");
		}
		else
		{
			text_out(" as fast as you");
		}
	}
	else if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) text_out(" incredibly");
		else if (r_ptr->speed > 120) text_out(" very");
		else if (r_ptr->speed < 116) text_out(" fairly");
		text_out(" quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) text_out(" extremely");
		else if (r_ptr->speed < 100) text_out(" very");
		text_out(" slowly");
	}
	else
	{
		text_out(" at normal speed");
	}

	/* Special comments about creatures that don't move. */
	if (l_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Describe */
		text_out(", but does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		text_out(".  ");
	}
}

/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(int r_idx, monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int i;


	/* Hack -- Maximal kills */
	l_ptr->tkills = MAX_SHORT;

	/* Hack -- Maximal info */
	l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

	/* Observe "maximal" attacks */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
		{
			/* Hack -- maximal observations */
			l_ptr->blows[i] = MAX_UCHAR;
		}
	}

	/* Hack -- maximal drops */
	l_ptr->drop_gold = l_ptr->drop_item =
	(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
	 ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
	 ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
	 ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
	 ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
	 ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

	/* Hack -- but only "valid" drops */
	if (r_ptr->flags1 & (RF1_ONLY_GOLD)) l_ptr->drop_item = 0;
	if (r_ptr->flags1 & (RF1_ONLY_ITEM)) l_ptr->drop_gold = 0;

	/* Hack -- observe many spells */
	l_ptr->ranged = MAX_UCHAR;

	/* Hack -- know all the flags */
	l_ptr->flags1 = r_ptr->flags1;
	l_ptr->flags2 = r_ptr->flags2;
	l_ptr->flags3 = r_ptr->flags3;
	l_ptr->flags4 = r_ptr->flags4;
	l_ptr->flags5 = r_ptr->flags5;
	l_ptr->flags6 = r_ptr->flags6;
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
	monster_lore lore;

	/* Get the race and lore */
	const monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];


	/* Hack -- create a copy of the monster-memory */
	COPY(&lore, l_ptr, monster_lore);

	/* Assume some "obvious" flags */
	lore.flags1 |= (r_ptr->flags1 & RF1_OBVIOUS_MASK);


	/* Killing a monster reveals some properties */
	if (lore.tkills)
	{
		/* Know "race" flags */
		lore.flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);

		/* Know "forced" flags */
		lore.flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FIXED_HPS));
	}

	/* Cheat -- know everything */
	if (cheat_know || spoilers) cheat_monster_lore(r_idx, &lore);

	/* Player ghosts are always fully known */
	else if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		cheat_monster_lore(r_idx, &lore);

	/* Monster description */
	describe_monster_desc(r_idx);

	/* Show kills of monster vs. player(s) */
	if (!spoilers) describe_monster_kills(r_idx, &lore);

	/* Describe the movement and level of the monster */
	describe_monster_movement(r_idx, &lore);

	/* Describe experience */
	if (!spoilers) describe_monster_exp(r_idx, &lore);

	/* Describe spells and innate attacks */
	describe_monster_spells(r_idx, &lore);

	/* Describe monster "toughness" */
	if (!spoilers) describe_monster_toughness(r_idx, &lore);

	/* Describe the abilities of the monster */
	describe_monster_abilities(r_idx, &lore);

	/* Describe the monster drop */
	describe_monster_drop(r_idx, &lore);

	/* Describe the known attacks */
	describe_monster_attack(r_idx, &lore);

	/* All done */
	text_out("\n");
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx, int row)
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


	/* Clear the top line  XXX XXX */
	clear_row(row);

	/* Reset the cursor */
	(void)Term_gotoxy(0, row);

	/* Special treatment for player ghosts. */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		(void)Term_addstr(-1, TERM_WHITE, ghost_name);
		(void)Term_addstr(-1, TERM_WHITE, ", the ");
	}

	/* A title (use "The" for non-uniques) */
	else if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		(void)Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the racial name. */
	(void)Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	(void)Term_addstr(-1, TERM_WHITE, " ('");
	(void)Term_addch(a1, c1);
	(void)Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "adjusted" attr/char info */
	(void)Term_addstr(-1, TERM_WHITE, "/('");
	(void)Term_addch(a2, c2);
	(void)Term_addstr(-1, TERM_WHITE, "'):");
}


/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	clear_space(1, (Term->cols - 80) / 2, 80);

	/* Center on screen */
	text_out_indent = (Term->cols - 80) / 2;
	text_out_wrap = text_out_indent + 80;
	text_border_left = 1;

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

	/* Describe monster */
	roff_top(r_idx, 0);

	text_border_left = 0;
}


/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx)
{
	/* Erase the window */
	(void)Term_clear();

	/* Begin recall */
	(void)Term_gotoxy(0, 1);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE);

	/* Describe monster */
	roff_top(r_idx, 0);
}



/*
 * Display visible monsters in a window.  Optionally, also list nearby objects
 */
void display_m_list(int y, int x, bool also_list_objects)
{
	int w, h, col_wid;
	int i;
	int attr = TERM_WHITE;

	char buf[DESC_LEN];

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	u16b *race_count = NULL;
	int total_count = 0;


	/* Get size of window */
	(void)Term_get_size(&w, &h);

	/* Paranoia -- refuse to accept too small a window */
	if ((h < 2) || (w < 3)) return;

	/* Erase the window */
	if (!y && !x) (void)Term_clear();

	/* Hallucination */
	if ((p_ptr->image) && (!x) && (!y))
	{
		c_prt(TERM_L_PURPLE, "You can't believe what you are seeing!  It's like a dream!", 0, 0);
		(void)Term_fresh();
		return;
	}


	/* Calculate number of columns */
	i = ((w+20) / 40);

	/* Calculate column width */
	col_wid = w / i;


	/* Allocate the array */
	C_MAKE(race_count, z_info->r_max, u16b);

	/* Scan the monster list */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Skip monsters that are not fully visible */
		if (!mon_fully_visible(&m_list[i])) continue;

		/* Bump the count for this race */
		race_count[m_ptr->r_idx]++;

		/* Bump the total count */
		total_count++;
	}


	/* Note no visible monsters */
	if (!total_count)
	{
		c_prt(TERM_SLATE, "You see no monsters.", y, x);
		goto end_of_function;
	}

	/* Message */
	prt(format("You can see %d monster%s:",
		total_count, (total_count > 1 ? "s" : "")), y, x);


	/* Display the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		/* No monsters of this race are visible */
		if (!race_count[i]) continue;

		/* Get monster */
		r_ptr = &r_info[i];
		l_ptr = &l_list[i];

		/* Monster has killed one or more of our ancestors) */
		if (l_ptr->deaths)
		{
			/* Unique versus non-unique */
			if (r_ptr->flags1 & RF1_UNIQUE) attr = TERM_RED;
			else                            attr = TERM_L_RED;
		}

		/* First sighting by this character  XXX */
		else if (l_ptr->sights <= 1)
		{
			/* Unique versus non-unique */
			if (r_ptr->flags1 & RF1_UNIQUE) attr = TERM_L_PURPLE;
			else                            attr = TERM_L_BLUE;
		}

		/* Normal monster (no reason to suspect dangerous) */
		else
		{
			/* Unique versus non-unique */
			if (r_ptr->flags1 & RF1_UNIQUE) attr = TERM_L_PURPLE;
			else                            attr = TERM_WHITE;
		}

		/* Monsters are listed in columns, roughly 40 characters wide */
		if (++y >= h)
		{
			x += col_wid;
			y = 1;
		}
		if (x >= w - (col_wid / 2)) break;


		/* Build the monster name and number visible */
		if (race_count[i] == 1)
		{
			(void)strnfmt(buf, sizeof(buf), r_name + r_ptr->name);
		}
		else
		{
			(void)strnfmt(buf, sizeof(buf), "%s (x%d)", r_name + r_ptr->name, race_count[i]);
		}
		/* Truncate to fit column */
		buf[col_wid-3] = '\0';

		/* Display the pict */
		(void)Term_putch(x, y, r_ptr->x_attr, r_ptr->x_char);

		/* Display the name */
		c_prt(attr, buf, y, x + 2);
	}


	/* End of function */
	end_of_function:

	/* Also list objects, if requested and possible */
	if (also_list_objects)
	{
		/* Skip some space.  Prefer to advance to next column. */
		if ((x == 0) && (w > 3 * col_wid / 2))
		{
			x = col_wid;
			y = 0;
		}
		else if (y + 6 >= h)
		{
			x += col_wid;
			y = 0;
		}
		else y += 4;

		/* If we have (a fair amount of) space, print out nearest objects */
		if (x < w - (col_wid / 2)) display_nearby_objects(y, x, FALSE);
	}

	/* Free the race counters */
	if (race_count) FREE(race_count);
}



/*
 * Give the short form of a monster's name, without titles.
 */
void short_m_name(char *name)
{
	char *s;

	/* Scan through the copied name */
	for (s = name; *s; s++)
	{
		/* Next character is a ',' -- stop now */
		if (*s == ',')
		{
			break;
		}

		/* Next character is a space -- look ahead */
		else if (*s == ' ')
		{
			/* Stop on "of" or "the" */
			if (*(s+1) && (*(s+1) == 'o') && *(s+2) && (*(s+2) == 'f'))
			{
				break;
			}
			else if (*(s+1) && (*(s+1) == 't') && *(s+2) && (*(s+2) == 'h') &&
			         *(s+3) && (*(s+3) == 'e'))
			{
				break;
			}
		}
	}

	/* Truncate the copy */
	*s = '\0';
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
		FREE(monster_dist);
		FREE(monster_index);

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
	FREE(monster_dist);
	FREE(monster_index);
}




/*
 * Add various player ghost attributes depending on race.  -LM-
 */
static void process_ghost_race(int ghost_race, int r_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];
	byte n;

	switch (ghost_race)
	{
		/* Human */
		case 0:
		{
			/* No differences */
			break;
		}

		/* Elf */
		case 1:
		{
			if (r_ptr->freq_ranged) r_ptr->freq_ranged += 5;
			r_ptr->aaf += 4;
			r_ptr->hitpoints = 4 * r_ptr->hitpoints / 5;
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			    r_ptr->flags3 &= ~(RF3_HURT_LITE);
			break;
		}

		/* Hobbit */
		case 2:
		{
			r_ptr->ac += 2 + (r_ptr->ac / 4);
			r_ptr->hitpoints = 4 * r_ptr->hitpoints / 5;

			break;
		}

		/* Gnome */
		case 3:
		{
			if (r_ptr->freq_ranged < 15) r_ptr->freq_ranged = 15;
			if (r_ptr->mana < 15) r_ptr->mana = 15;
			r_ptr->flags6 |= (RF6_BLINK);
			r_ptr->flags3 |= (RF3_NO_SLEEP);
			r_ptr->hitpoints = 5 * r_ptr->hitpoints / 6;
			break;
		}

		/* Dwarf */
		case 4:
		{
			r_ptr->hitpoints = 6 * r_ptr->hitpoints / 5;
			break;
		}

		/* Half-Orc */
		case 5:
		{
			r_ptr->flags3 |= (RF3_ORC);
			break;
		}

		/* Half-Troll */
		case 6:
		{
			if (!r_ptr->freq_ranged) r_ptr->freq_ranged = 5;
			if (r_ptr->freq_ranged > 5)
				r_ptr->freq_ranged = 2 * r_ptr->freq_ranged / 3;

			r_ptr->flags4 |= (RF4_BOULDER);
			r_ptr->flags3 |= (RF3_TROLL);
			r_ptr->flags3 |= (TR3_REGEN);

			r_ptr->hitpoints = 4 * r_ptr->hitpoints / 3;
			r_ptr->aaf -= 2;

			r_ptr->ac += r_ptr->level / 10 + 10;

			m_ptr->mspeed -= 2;

			for (n = 0; n < MONSTER_BLOW_MAX; n++)
			{
				r_ptr->blow[n].d_side = 4 * r_ptr->blow[n].d_side / 3;
			}

			break;
		}

		/* Dunadan */
		case 7:
		{
			r_ptr->ac += r_ptr->level / 10 + 5;

			for (n = 0; n < MONSTER_BLOW_MAX; n++)
			{
				if ((n == 1) || (n == 3))
					r_ptr->blow[n].d_side = 6 * r_ptr->blow[n].d_side / 5;
			}
			break;
		}

		/* High-Elf */
		case 8:
		{
			r_ptr->ac += r_ptr->level / 10 + 2;

			if (r_ptr->freq_ranged) r_ptr->freq_ranged += 8;
			r_ptr->aaf += 5;
			if (r_ptr->flags3 & (RF3_HURT_LITE))
				r_ptr->flags3 &= ~(RF3_HURT_LITE);
			break;
		}

		/* Dark Elf */
		case 9:
		{
			if (r_ptr->freq_ranged) r_ptr->freq_ranged += 5;
			r_ptr->hitpoints = 4 * r_ptr->hitpoints / 5;
			break;
		}

		/* Giant */
		case 10:
		{
			r_ptr->flags3 |= (RF3_GIANT);
			r_ptr->flags3 |= (RF3_IM_FIRE);
			r_ptr->flags3 |= (RF3_IM_COLD);

			r_ptr->hitpoints = 4 * r_ptr->hitpoints / 3;
			r_ptr->aaf -= 2;
			if (r_ptr->freq_ranged > 5) r_ptr->freq_ranged -= 5;

			for (n = 0; n < MONSTER_BLOW_MAX; n++)
			{
				r_ptr->blow[n].d_side = 5 * r_ptr->blow[n].d_side / 4;
			}
			break;
		}
	}
}

/*
 * Add various player ghost attributes depending on realm and skill specialties.  -LM-
 */
static void process_ghost_realm(int ghost_realm, int ghost_specialty,
	int r_idx, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[r_idx];
	int dun_level = r_ptr->level;
	byte n;

	/*
	 * Note the care taken to make sure that all monsters that get spells
	 * can also cast them, since not all racial templates include spells.
	 */
	switch (ghost_realm)
	{
		/* Sorcery */
		case 1:
		{
			if (r_ptr->freq_ranged == 0) r_ptr->freq_ranged = 10;
			else r_ptr->freq_ranged += 10;

			if (r_ptr->flags4 & (RF4_SHOT)) r_ptr->flags4 &= ~(RF4_SHOT);
			if (r_ptr->flags4 & (RF4_ARROW)) r_ptr->flags4 &= ~(RF4_ARROW);
			if (r_ptr->flags4 & (RF4_BOLT)) r_ptr->flags4 &= ~(RF4_BOLT);

			r_ptr->flags5 |= (RF5_BOLT_MANA);

			if (dun_level > 11) r_ptr->flags5 |= (RF5_BALL_POIS);
			if ((dun_level > 13) && (dun_level < 25))
				r_ptr->flags5 |= (RF5_BOLT_ELEC);
			if ((dun_level > 16) && (dun_level < 35))
				r_ptr->flags5 |= (RF5_BEAM_ICE);
			if (dun_level > 24) r_ptr->flags5 |= (RF5_BALL_ELEC);
			if (dun_level > 34) r_ptr->flags5 |= (RF5_BALL_COLD);
			if (dun_level > 44) r_ptr->flags5 |= (RF5_BALL_ACID);
			if (dun_level > 64) r_ptr->flags5 |= (RF5_BALL_MANA);

			if (dun_level > 19) r_ptr->flags6 |= (RF6_SLOW);
			if (dun_level > 39) r_ptr->flags6 |= (RF6_HOLD);

			if (dun_level > 19) r_ptr->flags6 |= (RF6_HASTE);
			if (m_ptr->mspeed > 130) m_ptr->mspeed = 130;

			r_ptr->flags6 |= (RF6_BLINK);
			r_ptr->flags6 |= (RF6_BLIND);
			if (dun_level > 26) r_ptr->flags6 |= (RF6_TPORT);
			if (dun_level > 55) r_ptr->flags6 |= (RF6_TELE_AWAY);

			if (ghost_specialty == SPECIALTY_P_WIZARD)
			{
				r_ptr->mana = 3 * r_ptr->mana / 2;
				r_ptr->spell_power = 6 * r_ptr->spell_power / 5;
				r_ptr->hitpoints = 2 * r_ptr->hitpoints / 3;
				r_ptr->freq_ranged += 15;

				for (n = 0; n < MONSTER_BLOW_MAX; n++)
				{
					r_ptr->blow[n].d_side = 2 * r_ptr->blow[n].d_side / 3;
				}
			}
			else
			{
				r_ptr->hitpoints = 3 * r_ptr->hitpoints / 4;
			}

			break;
		}
		/* Piety */
		case 2:
		{
			if (r_ptr->freq_ranged == 0) r_ptr->freq_ranged = 10;
			else r_ptr->freq_ranged += 5;

			r_ptr->flags6 |= (RF6_WOUND);
			if (dun_level > 34) r_ptr->flags5 |= (RF5_BALL_LITE);
			if (dun_level > 64) r_ptr->flags5 |= (RF5_BEAM_NETHR);

			if (dun_level > 29) r_ptr->flags6 |= (RF6_BLINK);

			r_ptr->flags6 |= (RF6_SCARE);
			if (dun_level > 44) r_ptr->flags6 |= (RF6_HOLD);
			if (dun_level > 54) r_ptr->flags6 |= (RF6_FORGET);

			r_ptr->flags6 |= (RF6_HEAL);
			if (dun_level > 24) r_ptr->flags6 |= (RF6_CURE);

			if (dun_level > 29) r_ptr->flags7 |= (RF7_S_MONSTER);
			if (dun_level > 49) r_ptr->flags7 |= (RF7_S_MONSTERS);

			if (ghost_specialty == SPECIALTY_P_PRIEST)
			{
				r_ptr->mana = 3 * r_ptr->mana / 2;
				r_ptr->spell_power = 6 * r_ptr->spell_power / 5;
				r_ptr->hitpoints = 2 * r_ptr->hitpoints / 3;
				r_ptr->freq_ranged += 15;

				for (n = 0; n < MONSTER_BLOW_MAX; n++)
				{
					r_ptr->blow[n].d_side = 2 * r_ptr->blow[n].d_side / 3;
				}
			}
			else
			{
				r_ptr->hitpoints = 3 * r_ptr->hitpoints / 4;
			}

			break;
		}
		/* Nature magic */
		case 3:
		{
			if (r_ptr->freq_ranged == 0) r_ptr->freq_ranged = 10;
			else r_ptr->freq_ranged += 5;

			if (dun_level > 29) r_ptr->flags5 |= (RF5_BALL_ELEC);
			else r_ptr->flags5 |= (RF5_BEAM_ELEC);

			if (dun_level > 34) r_ptr->flags5 |= (RF5_BALL_COLD);
			else if (dun_level > 9) r_ptr->flags5 |= (RF5_BOLT_COLD);

			if (dun_level > 39) r_ptr->flags5 |= (RF5_BALL_FIRE);
			else if (dun_level > 14) r_ptr->flags5 |= (RF5_BOLT_FIRE);

			if (dun_level > 39) r_ptr->flags5 |= (RF5_BALL_ACID);
			else if (dun_level > 19) r_ptr->flags5 |= (RF5_BOLT_ACID);

			if (dun_level > 44) r_ptr->flags5 |= (RF5_BALL_POIS);
			else if (dun_level > 24) r_ptr->flags5 |= (RF5_BOLT_POIS);

			if (dun_level > 29) r_ptr->flags5 |= (RF5_BOLT_PLAS);

			if (dun_level > 49) r_ptr->flags5 |= (RF5_BALL_STORM);
			else if (dun_level > 34) r_ptr->flags5 |= (RF5_BOLT_WATER);

			if (dun_level > 54) r_ptr->flags5 |= (RF5_BALL_SOUND);
			if (dun_level > 54) r_ptr->flags5 |= (RF5_BALL_SHARD);
			if (dun_level > 59) r_ptr->flags5 |= (RF5_ARC__FORCE);

			if (dun_level > 24) r_ptr->flags6 |= (RF6_CURE);
			if (dun_level > 54) r_ptr->flags6 |= (RF6_HEAL);

			if (dun_level < 40) r_ptr->flags7 |= (RF7_S_ANT);
			if (dun_level < 40) r_ptr->flags7 |= (RF7_S_SPIDER);
			if (dun_level > 20) r_ptr->flags7 |= (RF7_S_ANIMAL);
			if (dun_level > 20) r_ptr->flags7 |= (RF7_S_HOUND);

			if (ghost_specialty == SPECIALTY_P_DRUID)
			{
				r_ptr->mana = 3 * r_ptr->mana / 2;
				r_ptr->spell_power = 6 * r_ptr->spell_power / 5;
				r_ptr->hitpoints = 2 * r_ptr->hitpoints / 3;
				r_ptr->freq_ranged += 15;

				for (n = 0; n < MONSTER_BLOW_MAX; n++)
				{
					r_ptr->blow[n].d_side = 2 * r_ptr->blow[n].d_side / 3;
				}
			}
			else
			{
				r_ptr->hitpoints = 3 * r_ptr->hitpoints / 4;
			}

			break;
		}
		/* Necromancy */
		case 4:
		{
			if (r_ptr->freq_ranged == 0) r_ptr->freq_ranged = 10;
			else r_ptr->freq_ranged += 10;

			if (dun_level > 49) r_ptr->flags2 |= (RF2_MORGUL_MAGIC);

			r_ptr->flags5 |= (RF5_BOLT_MANA);
			r_ptr->flags6 |= (RF6_WOUND);
			r_ptr->flags6 |= (RF6_DARKNESS);

			if (dun_level > 4) r_ptr->flags5 |= (RF5_BALL_POIS);
			if (dun_level > 14) r_ptr->flags5 |= (RF5_BALL_DARK);
			if (dun_level > 44) r_ptr->flags5 |= (RF5_BALL_NETHR);
			if (dun_level > 54) r_ptr->flags5 |= (RF5_BALL_MANA);
			if (dun_level > 54) r_ptr->flags5 |= (RF5_ARC__HFIRE);

			if (dun_level > 9) r_ptr->flags6 |= (RF6_BLINK);
			if (dun_level > 9) r_ptr->flags6 |= (RF6_BLIND);
			if (dun_level > 9) r_ptr->flags6 |= (RF6_SCARE);
			if (dun_level > 19) r_ptr->flags6 |= (RF6_CONF);
			if (dun_level > 29) r_ptr->flags6 |= (RF6_TPORT);
			if (dun_level > 39) r_ptr->flags6 |= (RF6_HOLD);

			if (dun_level > 49) r_ptr->flags6 |= (RF6_MIND_BLAST);

			if (dun_level > 19) r_ptr->flags7 |= (RF7_S_DEMON);
			if (dun_level > 69) r_ptr->flags7 |= (RF7_S_HI_DEMON);
			if (dun_level > 29) r_ptr->flags7 |= (RF7_S_UNDEAD);
			if (dun_level > 79) r_ptr->flags7 |= (RF7_S_HI_UNDEAD);

			if (ghost_specialty == SPECIALTY_P_NECRO)
			{
				r_ptr->mana = 3 * r_ptr->mana / 2;
				r_ptr->spell_power = 6 * r_ptr->spell_power / 5;
				r_ptr->hitpoints = 2 * r_ptr->hitpoints / 3;
				r_ptr->freq_ranged += 15;

				for (n = 0; n < MONSTER_BLOW_MAX; n++)
				{
					r_ptr->blow[n].d_side = 2 * r_ptr->blow[n].d_side / 3;
				}
			}
			else
			{
				r_ptr->hitpoints = 3 * r_ptr->hitpoints / 4;
			}

			break;
		}
	}


	/* Handle skill specialties */
	switch (ghost_specialty)
	{
		/* Various kinds of burglars */
		case SPECIALTY_HAND_BURGLAR:
		case SPECIALTY_FIGHT_BURGLAR:
		case SPECIALTY_PURE_BURGLAR:
		case SPECIALTY_MAGE_BURGLAR:
		case SPECIALTY_PRIEST_BURGLAR:
		case SPECIALTY_DRUID_BURGLAR:
		case SPECIALTY_NECRO_BURGLAR:
		{
			if (r_ptr->freq_ranged == 0) r_ptr->freq_ranged = 10;

			r_ptr->hitpoints = 5 * r_ptr->hitpoints / 6;

			r_ptr->flags6 |= (RF6_DARKNESS);
			if (dun_level > 26) r_ptr->flags6 |= (RF6_TRAPS);
			if (dun_level > 44) r_ptr->flags6 |= (RF6_TELE_TO);

			if (dun_level > 49) r_ptr->flags2 |= (RF2_EMPTY_MIND);
			else if (dun_level > 24)
				r_ptr->flags2 |= (RF2_WEIRD_MIND);

			if (dun_level > 39) r_ptr->flags2 |= (RF2_INVISIBLE);

			if (dun_level > 44) r_ptr->flags7 |= (RF7_S_THIEF);

			if (r_ptr->blow[0].effect == RBE_HURT)
			{
				r_ptr->blow[0].effect = RBE_EAT_GOLD;
			}
			if (r_ptr->blow[1].effect == RBE_HURT)
			{
				r_ptr->blow[1].effect = RBE_EAT_ITEM;
			}

			if (r_ptr->mana < 25) r_ptr->mana = 25;

			break;
		}

		/* Physical missiles */
		case SPECIALTY_SLING:
		{
			if (dun_level > 39) r_ptr->flags2 |= (RF2_ARCHER);
			else r_ptr->freq_ranged += 25;
			r_ptr->flags4 |= (RF4_SHOT);
			r_ptr->mana -= (r_ptr->mana / 3);
			if (ghost_realm) r_ptr->spell_power -= (r_ptr->spell_power / 3);

			break;
		}
		case SPECIALTY_BOW:
		case SPECIALTY_MISSILE_MAGIC:
		{
			if (dun_level > 39) r_ptr->flags2 |= (RF2_ARCHER);
			else r_ptr->freq_ranged += 25;
			r_ptr->flags4 |= (RF4_ARROW);
			r_ptr->mana -= (r_ptr->mana / 3);
			if (ghost_realm) r_ptr->spell_power -= (r_ptr->spell_power / 3);

			break;
		}
		case SPECIALTY_CROSSBOW:
		{
			if (dun_level > 39) r_ptr->flags2 |= (RF2_ARCHER);
			else r_ptr->freq_ranged += 25;
			r_ptr->flags4 |= (RF4_BOLT);
			r_ptr->mana -= (r_ptr->mana / 3);
			if (ghost_realm) r_ptr->spell_power -= (r_ptr->spell_power / 3);

			break;
		}
		case SPECIALTY_THROWING:
		{
			if (dun_level > 39) r_ptr->flags2 |= (RF2_ARCHER);
			else r_ptr->freq_ranged += 25;
			r_ptr->flags4 |= (RF4_MISSL);
			r_ptr->mana -= (r_ptr->mana / 3);
			if (ghost_realm) r_ptr->spell_power -= (r_ptr->spell_power / 3);

			break;
		}

		/* Melee combat */
		case SPECIALTY_SWORDS:
		case SPECIALTY_POLEARMS:
		case SPECIALTY_HAFTED:
		case SPECIALTY_WRESTLING:
		case SPECIALTY_KARATE:
		case SPECIALTY_FIGHTER:
		{
			r_ptr->freq_ranged /= 2;

			r_ptr->spell_power = 2 * r_ptr->spell_power / 3;

			r_ptr->hitpoints = 3 * r_ptr->hitpoints / 2;
			r_ptr->ac += r_ptr->level / 10 + 5;

			if (r_ptr->level >= 10)
			{
				for (n = 0; n < MONSTER_BLOW_MAX; n++)
				{
					r_ptr->blow[n].d_side = 3 * r_ptr->blow[n].d_side / 2;
					break;
				}
			}

			break;
		}
	}


	/* Hack -- clean up illogical combinations of flags */
	if ((r_ptr->flags3 & (RF3_IM_FIRE)) &&
	    (r_ptr->flags3 & (RF3_HURT_FIRE)))
	{
		r_ptr->flags3 &= ~(RF3_IM_FIRE | RF3_HURT_FIRE);
	}
	if ((r_ptr->flags3 & (RF3_IM_COLD)) &&
	    (r_ptr->flags3 & (RF3_HURT_COLD)))
	{
		r_ptr->flags3 &= ~(RF3_IM_COLD | RF3_HURT_COLD);
	}
}

/*
 * Once a monster with the flag "PLAYER_GHOST" is generated, it needs
 * to have a little color added, if it hasn't been prepared before.
 * This function uses a bones file to get a name, give the ghost a
 * gender, and add flags depending on the race and magic realm of the
 * slain adventurer.  -LM-
 */
bool prepare_ghost(int r_idx, monster_type *m_ptr, bool from_savefile)
{
	int ghost_sex, ghost_race, ghost_realm, ghost_specialty = 0;
	byte attempt, i, backup_file_selector;

	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	FILE *fp;
	bool err = FALSE;
	char path[1024];


	/* Paranoia. */
	if (!(r_ptr->flags2 & (RF2_PLAYER_GHOST))) return (TRUE);

	/* Hack -- If the ghost has a sex, then it must already have been prepared. */
	if ((r_ptr->flags1 & RF1_MALE) || (r_ptr->flags1 & RF1_FEMALE))
	{
		/* Hack - Ensure that ghosts are always "seen". */
		l_ptr->sights = 1;

		/* Nothing more to do. */
		return (TRUE);
	}

	/* Hack -- No easy player ghosts, unless the ghost is from a savefile. */
	if ((r_ptr->level < p_ptr->depth - 10) && (from_savefile == FALSE))
		return (FALSE);

	/* Store the index of the base race. */
	r_ghost = r_idx;


	/*
	 * Choose a bones file.  Use the variable bones_selector if it has any
	 * information in it (this allows saved ghosts to reacquire all special
	 * features), then use the current depth, and finally pick at random.
	 */
	for (attempt = 0; attempt < 200; ++attempt)
	{
		/* Prepare a path, and store the file number for future reference. */
		if (attempt == 0)
		{
			if (bones_selector)
			{
				(void)strnfmt(path, sizeof(path), "%s/bone.%03d", ANGBAND_DIR_BONE, bones_selector);
			}
			else
			{
				(void)strnfmt(path, sizeof(path), "%s/bone.%03d", ANGBAND_DIR_BONE, p_ptr->depth);
				bones_selector = (byte)p_ptr->depth;
			}
		}
		else
		{
			backup_file_selector = randint(100);
			(void)strnfmt(path, sizeof(path), "%s/bone.%03d", ANGBAND_DIR_BONE, backup_file_selector);
			bones_selector = backup_file_selector;
		}

		/* Attempt to open the bones file. */
		fp = my_fopen(path, "r");

		/* Success. */
		if (fp) break;
	}

	/* No bones file found, so no ghost is made. */
	if (!fp) return (FALSE);


	/* Read the file. */
	fp = my_fopen(path, "r");

	/* Read five entries */
	err = (fscanf(fp, "%[^\n]\n%d\n%d\n%d\n%d", ghost_name,
		&ghost_sex, &ghost_race, &ghost_realm, &ghost_specialty) != 5);

	/* If that didn't work, try reading the first four */
	if (err)
	{
		err = (fscanf(fp, "%[^\n]\n%d\n%d\n%d", ghost_name,
			&ghost_sex, &ghost_race, &ghost_realm) != 4);
	}

	/* Close the file */
	(void)my_fclose(fp);

	/* Hack -- broken file */
	if (err)
	{
		bones_selector = 0;
		return (FALSE);
	}

	/*** Process the ghost name and store it in a global variable. ***/

	/* XXX XXX XXX Find the first comma, or end of string */
	for (i = 0; (i < 16) && (ghost_name[i]) && (ghost_name[i] != ','); i++);

	/* Terminate the name */
	ghost_name[i] = '\0';

	/* Force a name */
	if (!ghost_name[0]) strcpy(ghost_name, "Nobody");

	/* Capitalize the name */
	if (my_islower(ghost_name[0])) ghost_name[0] = my_toupper(ghost_name[0]);


	/*** Process sex. ***/

	/* Sanity check. */
	if (ghost_sex >= MAX_SEXES) ghost_sex = rand_int(MAX_SEXES);

	/* And use that number to toggle on either the male or the female flag. */
	if (ghost_sex == 0) r_ptr->flags1 |= (RF1_FEMALE);
	if (ghost_sex == 1) r_ptr->flags1 |= (RF1_MALE);


	/*** Process race. ***/

	/* Sanity check. */
	if (ghost_race >= MAX_RACES) ghost_race = rand_int(MAX_RACES);

	/* And use the ghost race to gain some flags. */
	process_ghost_race(ghost_race, r_idx, m_ptr);


	/*** Process realm. ***/

	/* Sanity check. */
	if (ghost_realm >= 5) ghost_realm = rand_int(5);

	/* And use the ghost realm to gain some flags. */
	process_ghost_realm(ghost_realm, ghost_specialty, r_idx, m_ptr);

	/* Hack - a little extra help for the deepest ghosts */
	if (p_ptr->depth > 75) r_ptr->spell_power += 3 * (p_ptr->depth - 75) / 2;


	/* Hack -- increase the level feeling */
	level_rating += 10;

	/* A ghost makes the level special */
	good_item_flag = TRUE;

	/*
	 * Hack - Player ghosts are "seen" whenever generated, to conform with
	 * previous practice.
	 */
	l_ptr->sights = 1;

	/* Success */
	return (TRUE);
}


