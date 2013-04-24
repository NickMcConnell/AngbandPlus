/* File: tables.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




/*
 * Global array for looping through the "keypad directions".
 */
const s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/*
 * Global arrays for converting "keypad direction" into "offsets".
 */
const s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

const s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]".
 */
const s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

const s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };


/*
 * Circular keypad direction array
 */
const s16b cdd[8] =
{ 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Global arrays for optimizing "ddx[cdd[i]]" and "ddy[cdd[i]]"
 */
const s16b ddx_cdd[8] =
{ 0, 1, 1, 1, 0, -1, -1, -1 };

const s16b ddy_cdd[8] =
{ 1, 1, 0, -1, -1, -1, 0, 1 };

/*
 * Global arrays for optimizing "ddx[cdd[i]]" and "ddy[cdd[i]]"
 */
const s16b ddx_cdddouble[8] =
{ 0, 2, 2, 2, 0, -2, -2, -2 };

const s16b ddy_cdddouble[8] =
{ 2, 2, 0, -2, -2, -2, 0, 2 };

/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
const char hexsym[16] =
{
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
}; 


/*
 * This table is used to help calculate the number of blows the player can
 * make in a single round of attacks (one player turn) with a normal weapon.
 *
 * This number ranges from a single blow/round for weak players to up to six
 * blows/round for powerful warriors.
 *
 * Note that certain artifacts and ego-items give "bonus" blows/round.
 *
 * Currently none of the below is true for steam. 
 *
 * First, from the player class, we extract some values:
 *
 *    Warrior --> num = 6; mul = 5; div = MAX(30, weapon_weight);
 *    Mage    --> num = 4; mul = 2; div = MAX(40, weapon_weight);
 *    Priest  --> num = 5; mul = 3; div = MAX(35, weapon_weight);
 *    Rogue   --> num = 5; mul = 3; div = MAX(30, weapon_weight);
 *    Ranger  --> num = 5; mul = 4; div = MAX(35, weapon_weight);
 *    Paladin --> num = 5; mul = 4; div = MAX(30, weapon_weight);
 *
 * To get "P", we look up the relevant "adj_str_blow[]" (see above),
 * multiply it by "mul", and then divide it by "div", rounding down.
 *
 * To get "D", we look up the relevant "adj_dex_blow[]" (see above),
 * note especially column 6 (DEX 18/101) and 11 (DEX 18/150).
 *
 * The player gets "blows_table[P][D]" blows/round, as shown below,
 * up to a maximum of "num" blows/round, plus any "bonus" blows/round.
 */
const byte blows_table[12][12] =
{
	/* P/D */
	/* 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11+ */

	/* 0  */
	{  1,   1,   1,   1,   1,   1,   2,   2,   2,   2,   2,   3 },

	/* 1  */
	{  1,   1,   1,   1,   2,   2,   3,   3,   3,   4,   4,   4 },

	/* 2  */
	{  1,   1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5 },

	/* 3  */
	{  1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5,   5 },

	/* 4  */
	{  1,   2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5 },

	/* 5  */
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 6  */
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 7  */
	{  2,   3,   3,   4,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 8  */
	{  3,   3,   3,   4,   4,   4,   5,   5,   5,   5,   6,   6 },

	/* 9  */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   6,   6 },

	/* 10 */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   6,   6,   6 },

	/* 11+ */
	{  3,   3,   4,   4,   4,   4,   5,   5,   6,   6,   6,   7 },
};


/*
 * This table allows quick conversion from "speed" to "energy"
 * The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
 * Note that table access is *much* quicker than computation.
 *
 * Note that the table has been changed at high speeds.  From
 * "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
 * at speeds above "Fast (+30)", one approaches an asymptotic
 * effective limit of 50 energy per turn.  This means that it
 * is relatively easy to reach "Fast (+30)" and get about 40
 * energy per turn, but then speed becomes very "expensive",
 * and you must get all the way to "Fast (+50)" to reach the
 * point of getting 45 energy per turn.  After that point,
 * furthur increases in speed are more or less pointless,
 * except to balance out heavy inventory.
 *
 * Note that currently the fastest monster is "Fast (+30)".
 *
 * It should be possible to lower the energy threshhold from
 * 100 units to 50 units, though this may interact badly with
 * the (compiled out) small random energy boost code.  It may
 * also tend to cause more "clumping" at high speeds.
 */
const byte extract_energy[200] =
{
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* S-50 */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	/* S-40 */     2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
	/* S-30 */     2,  2,  2,  2,  2,  2,  2,  3,  3,  3,
	/* S-20 */     3,  3,  3,  3,  3,  4,  4,  4,  4,  4,
	/* S-10 */     5,  5,  5,  5,  6,  6,  7,  7,  8,  9,
	/* Norm */    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
	/* F+10 */    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
	/* F+20 */    30, 31, 32, 33, 34, 35, 36, 36, 37, 37,
	/* F+30 */    38, 38, 39, 39, 40, 40, 40, 41, 41, 41,
	/* F+40 */    42, 42, 42, 43, 43, 43, 44, 44, 44, 44,
	/* F+50 */    45, 45, 45, 45, 45, 46, 46, 46, 46, 46,
	/* F+60 */    47, 47, 47, 47, 47, 48, 48, 48, 48, 48,
	/* F+70 */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
	/* Fast */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
};


/*
 * Base experience levels, may be adjusted up for race and/or class
 */
const s32b player_exp[PY_MAX_LEVEL] =
{				/* difference */
	15,			/* XX */
	38,			/* XX */
	75,			/* XX */
	135,		/* XX */
	210,		/* XX */
	300,		/* XX */
	405,		/* XX */
	525,		/* XX */
	675,		/* XXX */
	900,		/* XXX */
	1200,		/* XXX */
	1500,		/* XXX */
	2100,		/* XXX */
	2700,		/* XXX */
	3450,		/* XXX */
	4200,		/* XXX */
	5100,		/* XXX */
	6000,		/* XXX */
	7050,		/* XXX */
	8100,		/* XXX */
	10200,		/* XXX */
	12600,
	15300,
	18750,
	26250L,
	37500L,
	52500L,
	75000L,
	112500L,
	150000L,
	225000L,
	300000L,
	412500L,
	525000L,
	675000L,
	825000L,
	1050000L,
	1275000L,
	1500000L,
	1875000L,
	2250000L,
	2700000L,
	3150000L,
	3600000L,
	4050000L,
	4500000L,
	5250000L,
	6000000L,
	8000000L,
	10000000L
};


/*
 * Player Sexes
 *
 *	Title,
 *	Winner
 */
const player_sex sex_info[MAX_SEXES] =
{
	{
		"Female",
		"Queen"
	},

	{
		"Male",
		"King"
	}
};


spell_book books[SV_BOOK_MAX] =
{
	{
		/* Liber Al Vel Legis (sval 0) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_AIM_OF_THE_WILL, "Aim of the Will", 3, 1, 7 },
			{ POW_SENSE_WILL, "Sense Will", 4, 2, 6 },
			{ POW_ASTRAL_GATE, "Astral Gate", 8, 5, 12 },
			{ POW_INNER_RAD, "Inner Radiance", 9, 3, 7 },
			{ POW_DEMONIC_WILL, "Demonic Will", 11, 10, 2 },
			{ POW_LO_OBJECT, "Object/Metal Detection", 13, 6, 11 },
			{ POW_LO_TRAPS, "Safe Passage of the Will", 15, 6, 11 },
			{ POW_PLAGUE_WILL, "Plaguing the Will", 20, 7, 16 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* The Zohar (Book of Splendor) (sval 1) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_SUPPRESSION_WILL, "Supression of the Will", 15, 3, 10 },
			{ POW_IDENTIFY, "Knowledge of Secular Objects", 20, 15, 35 },
			{ POW_RECHARGE, "Transfering Will to Objects", 23, 30, 25 },
			{ POW_FROST_BOLT, "Spectacle of Ice", 25, 9, 17},
			{ POW_FIRE_BOLT, "Spectacle of Fire", 25, 9, 17},
			{ POW_HASTE, "Focus of the Will", 26, 10, 35 },
			{ POW_TELEPORT_OTHER_I, "Movement of the Will of Others", 30, 18, 40 },
			{ POW_FIREBALL, "Sphere of Flame", 32, 18, 20 },
			{ POW_GENOCIDE, "Solitude of the Will", 45, 40, 60 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Sepher Yetzirah (Book of Formation) (sval 2) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_ACID_BOLT, "Spectacle of Acid", 26, 11, 20 },
			{ POW_LIGHTNING_BOLT, "Spectacle of Lightning", 26, 11, 20 },
			{ POW_CLOUD_KILL, "Purge the Toxins of the Will", 33, 23, 35 },
			{ POW_ICE_STORM, "Storm of Ice", 36, 22, 34 },
			{ POW_FIRE_STORM, "Storm of Fire", 38, 22, 34 },
			{ POW_METEOR_STORM, "Swarm of Meteors", 43, 30, 45 },
			{ POW_ELEMENTAL_BALL, "Sphere of the elements", 47, 40, 45 },
			{ POW_SOUL_STORM, "Soulstorm", 50, 50, 40 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* De Vermis Mysteriis (Mysteries of the Worm) (sval 3) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_WORD_OF_RECALL_I, "Path of the Worm", 30, 10, 14 },
			{ POW_RECHARGE_II, "Power of the Worm", 44, 20, 35 },
			{ POW_EARTHQUAKE_I, "Force of the Worm", 46, 28, 30 },
			{ POW_MIND_OF_WORM, "Mind of the Worm", 51, 19, 50 },
			{ POW_MASS_GENOCIDE, "Death of the Worm", 53, 65, 60 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Cthaat Aquadingen (sval 4) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_SUMMON_DEMON, "Hellspawn", 10, 30, 60},
			{ POW_VOORISH_SIGN, "Voorish Sign", 12, 15, 15 },/* + magic skill */
			{ POW_BYAKHEE_WINDS, "Winds of Byakhee", 16, 12, 30 }, /*wind cone */
			{ POW_FAUGN_BARRIER, "Barrier of Chaugnar Faugn", 20, 18, 40 }, /*fear wall*/
			{ POW_BANISH_DEMON, "Expatriate Demon", 55, 20, 55 },	/* dispel */
			{ POW_SUMMON_DEMON_II, "Helllord", 65, 100, 40 },
			{ POW_LAMP_ALHAZRED, "Lamp of Alhazred", 70, 120, 14 }, /* halucination enlightnment */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Othuum Omnicia (sval 5) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_CONTACT_YITHIAN, "Contact Yithian", 10, 2, 10 }, /* detect magic */
			{ POW_INCANT_MI_GO, "Incantation of Mi-go", 18, 8, 18 }, /* (drain) wheel (short beams in all directions) */
			{ POW_CONSUME_FLESH, "Consume Flesh", 25, 1, 55 }, /* hp to sp */
			{ POW_MELT_STONE, "Melt Stone", 30, 3, 28 }, /* wall to mud */
			{ POW_CHIME_TEZCH, "Chime of Tezchaptl", 45, 16, 35 }, /* walls of force radiating out */
			{ POW_MIRROR_ATEP, "Mirror of Tarkhun Atep", 55, 40, 40 }, /* invisible */
			{ POW_GATE, "Gate", 60, 7, 10 }, /* dimension door */
			{ POW_EFFIGY_HATE, "Effigy of Hate", 70, 120, 35 }, /* large killwallsphere /then graviton sphere */
			{ POW_CONTACT_NYAR, "Contact Nyarlathotep", 75, 150, 50 }, /* *IDENTIFY* */
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Revelations of Glaaki (sval 6) */
		SBF_MAGIC,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_DEMON_COURAGE, "Demonic Courage", 9, 4, 12 }, 
			{ POW_DEMON_FURY, "Demonic Fury", 18, 8, 16 },
			{ POW_DEMONIC_VIGOR, "Infernal Health", 28, 10, 20 },
			{ POW_DEMON_SHIELD, "Demonic Shield", 32, 20, 25 },
			{ POW_STYGIAN_WAR, "Stygian Warfare", 40, 40, 30 }, /* combat bonus */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 7) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },/*Moving hell circle of death */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 8) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 9) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Treatise on the Resurrection (sval 10) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_CHANT, "Holy Protection Chant", 1, 1, 10 },
			{ POW_SANCTUARY, "Protection of the Lord", 3, 2, 15 },
			{ POW_HEALING_I, "Resting of the Body", 5, 3, 20 },
			{ POW_PROTECTION_FROM_EVIL, "Protection from Evil", 9, 3, 20 },
			{ POW_HEALING_II, "Healing of the Body", 11, 3, 22 },
			{ POW_REMOVE_CURSE, "Remove Foul Curses", 13, 8, 22 },
			{ POW_TURN_UNDEAD, "Purity of the living", 15, 4, 22 },
			{ POW_HEALING_III, "Purification of the Body", 18, 5, 23 },
			{ POW_DESTROY_MACHINE, "Reckoning of Metal", 20, 2, 11 },
			{ POW_HOLY_BOLT, "Holy Bolt", 20, 2, 9 }
			
		}
	},
	{
		/* Odes of Solomon (sval 11) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_PORTAL, "Ballad of the Holy Gate", 6, 3, 15 },
			{ POW_SENSE_INVISIBLE, "Hymn: Pierce the Spectral Barrier", 11, 4, 28 },
			{ POW_SENSE_SURROUNDINGS, "Hymn: Secular Surroundings", 12, 18, 65 },
			{ POW_SATISFY_HUNGER, "Last Supper", 13, 5, 30 },
			{ POW_PRAYER, "Holy Prayer", 14, 8, 33 },
			{ POW_DISPEL_EVIL, "Prayer to Dispel Evil", 18, 15, 33 },
			{ POW_IDENTIFY_II, "Lore", 22, 25, 85 },
			{ POW_HEALING_IV, "Healing", 40, 17, 70 },
			{ POW_HOLY_WORD, "Holy Word", 50, 15, 70 },
			{ POW_RESTORATION, "Healing of the Flesh", 65, 30, 80 }
			
			
		}
	},
	{
		/* The Pnakotic Manuscripts (sval 12) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_DISPEL_CURSE, "Holy Cleansing of Objects", 25, 30, 75 },
			{ POW_BANISHMENT, "Word of Banishment", 28, 35, 45 },
			{ POW_RECHARGE_III, "Holy Infusion of Power", 31, 50, 90 },
			{ POW_DISPEL_EVIL_II, "Exorcism of Evil", 34, 40, 55 },
			{ POW_WORD_OF_RECALL_II, "Godly Passage", 36, 80, 80 },
			{ POW_RESISTANCE, "Holy Resistance", 60, 80, 70 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Khorda Avesta (sval 13) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_HEAL_CUTS, "Ashem Vohu", 5, 2, 10 },
			{ POW_REMOVE_FEAR_II, "Padyab-Kusti", 8, 3, 13 },
			{ POW_MINOR_RESISTANCE, "Ahunwar", 10, 8, 22 },
			{ POW_MUSCLE_BUFF, "Srosh Baj", 20, 35, 33 },
			{ POW_SHATTER, "Doa Tan-Dorostri", 25, 8, 21 },
			{ POW_NO_TELEPORT, "Khwarshed Niyayesh", 30, 10, 35 },
			{ POW_VIGOR_BUFF, "Hoshbam", 35, 45, 44 },
			{ POW_STONE_MELD, "Vispa Humata", 55, 50, 60 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Trimorphic Protennoia (Nag Hammadi) (sval 14) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_FREE_ACT, "Spirit of Freedom", 3, 2, 12 },
			{ POW_DIVINE_FAVOR, "Divine Favor", 12, 18, 33 },
			{ POW_FLAMING_WRATH, "Flaming Wrath", 20, 12, 35 },
			{ POW_ANTI_MAGIC, "Anti-magic Shell", 40, 50, 60 },
			{ POW_HOLY_STRIKE, "Holy Strike", 45, 20, 36},
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* The Corpus Hermeticum (sval 15) */
		SBF_PRAYER,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_DANCING_LIGHTS, "Poemandres, the Shepherd of Men", 8, 3, 23 },/* dancing lights */
			{ POW_SACRED_SERMON, "The Sacred Sermon", 15, 30, 95 },
			{ POW_MANIFEST_GOD, "Though Unmanifest God Is Most Manifest", 40, 60, 45 },
			{ POW_THOUGHT_SENSE, "On Thought and Sense", 55, 12, 28 },
			{ POW_KEY, "The Key", 75, 120, 55 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 16) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 17) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Utility bandolier (sval 18) */
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_CALL_LIGHT, "Gaslight", 1, 1, 10 },
			{ POW_REMOVE_FEAR, "Harmonic Fortitude Restorer", 2, 2, 15 },
			{ POW_SPRING_BLADE, "Spring Blade", 3, 2, 10 },
			{ POW_HEALING_VI, "Physiological Robustness Aid", 4, 3, 15 },
			{ POW_DETECT_DOOR_STAIR, "Egress & Portal Locator", 8, 5, 20 },
			{ POW_DETECT_TRAPS, "Physical Danger Warning System", 10, 2, 20 },
			{ POW_SLOW_POISON, "Toxin Inhibitor", 7, 8, 30 },
			{ POW_NOURISHMENT, "Nourishment Support System", 18, 6, 35 },
			{ POW_OBJECT_ANALYSIS, "Object Analysis", 32, 30, 40 },
			{ POW_FETCH, "Object Grappling Hook", 40, 20, 20 }
			
		}
	},
	{
		/* Detective's Kit  (sval 19) */
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_DETECT_HOSTILE, "Hostile Lifeform Detection", 2, 1, 10 },
			{ POW_DETECT_TRAPS_DOORS, "Danger and Escape Route Locator", 8, 2, 12 },
			{ POW_TREASURE_DETECTION, "Valuable Mineral Detector", 6, 2, 14 },
			{ POW_DETECT_ENCHANTMENT, "Pseudonatural Harmonizer", 17, 3, 16 },
			{ POW_DETECTION, "Sherlock Holmes Analytic System", 26, 10, 18 },
			{ POW_PERCEPTION, "Object Property Analyzer", 20, 12, 20 },
			{ POW_PROBEING, "Biological Scanner", 28, 15, 22 },
			{ POW_CLARIVOYANCE, "Autopneumatic Etheric Data Collector", 35, 20, 24 },
			{ POW_IDENTIFY_III, "Complete Object Analysis", 40, 40, 18 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Clockwork Chassis (sval 20)*/
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_SPEAR_OF_LIGHT, "Photic Beam", 2, 4, 20 },
			{ POW_ETHERIC_JUMP, "Etheric Jump", 8, 5, 30 },
			{ POW_DEFENSIVE_ARRAY, "Defensive Array", 15, 10, 40 },
			{ POW_NEUTRALIZE_POISON, "Toxin Elimination System", 11, 15, 50 },
			{ POW_GUNS, "Forward Cannons", 16, 27, 45 },
			{ POW_HEALING_VII, "Advanced Physiological Aid", 20, 12, 45 },
			{ POW_TURN_STONE_TO_MUD, "Pneumatic Tunneling Device", 22, 15, 50 },
			{ POW_EARTHQUAKE_II, "Sonic Unharmonizer", 30, 25, 35 },
			{ POW_MISSILE, "Shoulder Rocket", 35, 40, 40 },
			{ POW_EMP, "Electro-magnetic Pulse", 40, 40, 10 }
			
		}
	},
	{
		/* Clockwork carbine (sval 21) */
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_LEAD_SLUGS, "Fire Lead Slugs", 3, 5, 35 },
			{ POW_LIGHTNING_RAY, "Fire Lightning Ray", 8, 6, 40 },
			{ POW_FROST_RAY, "Fire Frost Beam", 12, 8, 40 },
			{ POW_HEAT_RAY, "Fire Flamethrower", 15, 8, 42 },
			{ POW_GRAVITY_RAY, "Fire Graviton Ray", 25, 9, 44 },
			{ POW_TELEPORT_OTHER_II, "Fire Dimensional Ray", 30, 10, 46 },
			/* need some cool powers here. :-/ */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Velocipede (sval 22) */
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_BLINK, "Etheric Step", 5, 10, 20 },
			{ POW_TELEPORT_SELF, "Etheric Travel", 10, 15, 25 },
			{ POW_TELEPORT_OTHER_III, "Etheric Strike", 15, 20, 30 },
			{ POW_TELEPORT_LEVEL, "Etheric Flight", 18, 22, 32 },
			{ POW_WORD_OF_RECALL_III, "Etheric Return", 20, 30, 40 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* The Analytic Engine (sval 23) */
		SBF_DEVICE,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ POW_HEALING_VIII, "Superior Physiological Aid", 16, 30, 40 },
			{ POW_BIOLOGICAL_ENHANCE, "Biological Enhancment Routine", 18, 35, 55 },
			{ POW_POLYMORPH_OTHER, "Specimen Mutating Utility", 20, 20, 30 },
			{ POW_RECHARGE_IV, "Object Power Restorer", 22, 75, 55 },
			{ POW_DOOR_CREATION, "Egress Synthesizer", 15, 30, 40 },
			{ POW_STAIR_CREATION, "Passage Synthesizer", 17, 30, 40 },
			{ POW_BIOLOGICAL_ENHANCE_II, "Improved Biological Enhancement", 30, 80, 65 },
			{ POW_HEALING_IX, "Total Physiological Aid", 35, 80, 77 },
			{ POW_ALTER_REALITY, "Emergency Environment Alteration", 40, 100, 40 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 24) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 25) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 26) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	},
	{
		/* Book name (sval 27) */
		0,
		{	/* Index, Spell name, level of skill, mana, failure */
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 },
			{ 0, NULL, 99, 0, 0 }
			
		}
	}	
};


/*
 * Steamware Tables
 */
 
steamware wares[MAX_STEAMWARE_PARTS] =
{
	{
		SW_EYES,
		{
			{ "Alpha eyes",	5000,		800,	2000},
			{ "Beta eyes", 	24000,		1200,	7000},
			{ "Gamma eyes",	60000,		2000,	19000},
			{ "Delta eyes",	120000,		8000,	39000},
			{ "Research Completed",	0,	0,	0}			
		}
	},
	{
		SW_WIRED_REFLEX,
		{
			{"Alpha wired reflexes",	40000,		8000, 	5000},
			{"Beta wired reflexes", 	120000,		24000, 	12000},
			{"Gamma wired reflexes",	240000,		80000,	22000},
			{"Delta wired reflexes",	1600000,	300000,	50000},
			{ "Research Completed",	0,	0,	0}			
		}
	},
	{
		SW_DERMAL_PLATE,
		{
			{"Alpha dermal plating",	8000,	1200, 	3000},
			{"Beta dermal plating", 	16000,	4000, 	9000},
			{"Gamma dermal plating",	32000,	12000,	20000},
			{"Delta dermal plating",	64000,	24000,	45000},
			{ "Research Completed",	0,	0,	0}			
		}
	},
	{
		SW_FURNACE_CORE,
		{
			{"Alpha core furnace",	50000,		20000, 	10000},
			{"Beta core furnace", 	100000,		40000, 	20000},
			{"Gamma core furnace",	500000,		80000,	40000},
			{"Delta core furnace",	1000000,	400000,	60000},
			{ "Research Completed",	0,	0,	0}			
		}
	},
	{
		SW_SPURS,
		{
			{"Alpha spurs",	12000,		1000, 	40000},
			{"Beta spurs", 	10000,		2000, 	10000},
			{"Gamma spurs",	20000,		4000,	29000},
			{"Delta spurs",	40000,		8000,	39000},
			{ "Research Completed",	0,	0,	0}			
		}
	}
};
 
/*
 * Resistance names 
 */

cptr resist_names[RS_MAX] =
{
	"fire",
	"earth",
	"air",
	"water",
	"electricty",
	"ice",
	"acid",
	"poison",
	"time",
	"ether",
	"sound",
	"nether",
	"light",
	"dark",
	"mental",
	"force",
	"spirit"
};

/*
 * Resistance names 
 */
cptr resist_names_short[RS_MAX] =
{
	"Fire ",
	"Erth ",
	"Air  ",
	"Water",
	"Elec ",
	"Ice  ",
	"Acid ",
	"Poisn",
	"Time ",
	"Ether",
	"Sound",
	"Nethr",
	"Light",
	"Dark ",
	"Mind ",
	"Force",
	"Sprt "
};

/*
 * Resistance maximums
 * Normal resistance, cap with temporary resist
 */
res_cap resist_caps[RS_MAX] =
{
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88},
	{88, 88}
};

/*
 * Each chest has a certain set of traps, determined by pval
 * Each chest has a "pval" from 1 to the chest level (max 55)
 * If the "pval" is negative then the trap has been disarmed
 * The "pval" of a chest determines the quality of its treasure
 * Note that disarming a trap on a chest also removes the lock.
 */
const byte chest_traps[100] =
{
	0,					/* 0 == empty */
	(CHEST_POISON),
	(CHEST_LOSE_MUS),
	(CHEST_LOSE_VIG),
	(CHEST_LOSE_MUS),
	(CHEST_LOSE_VIG),			/* 5 == best small wooden */
	0,
	(CHEST_POISON),
	(CHEST_POISON),
	(CHEST_LOSE_MUS),
	(CHEST_LOSE_VIG),
	(CHEST_POISON),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_SUMMON),			/* 15 == best large wooden */
	0,
	(CHEST_LOSE_MUS),
	(CHEST_LOSE_VIG),
	(CHEST_PARALYZE),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_SUMMON),
	(CHEST_PARALYZE),
	(CHEST_LOSE_MUS),
	(CHEST_LOSE_VIG),
	(CHEST_EXPLODE),			/* 25 == best small iron */
	0,
	(CHEST_POISON | CHEST_LOSE_MUS),
	(CHEST_POISON | CHEST_LOSE_VIG),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_PARALYZE),
	(CHEST_POISON | CHEST_SUMMON),
	(CHEST_SUMMON),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),	/* 35 == best large iron */
	0,
	(CHEST_SUMMON),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_POISON | CHEST_PARALYZE),	/* 45 == best small steel */
	0,
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_MUS),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_VIG),
	(CHEST_POISON | CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_POISON | CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_MUS | CHEST_LOSE_VIG),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_POISON | CHEST_PARALYZE),	/* 55 == best large steel */
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	0,
	0, /* 65 */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
};


/*
 * Hack -- the "basic" color names (see "TERM_xxx")
 */
cptr color_names[16] =
{
	"Dark",
	"White",
	"Slate",
	"Orange",
	"Red",
	"Green",
	"Blue",
	"Umber",
	"Light Dark",
	"Light Slate",
	"Violet",
	"Yellow",
	"Light Red",
	"Light Green",
	"Light Blue",
	"Light Umber",
};


/* New stat names. 
 *
 * Please take note that the new stat names are reorganized,
 * all the physical stats are up front, and all the mental
 * stats are at the end 
 */ 
cptr stat_names[A_MAX] =
{
	"MUS: ", "AGI: ", "VIG: ", "SCH: ", "EGO: ", "CHR: "
};

/*
 * Abbreviations of damaged stats
 */
/* NEW damaged stats. Note the new order */ 
 cptr stat_names_reduced[A_MAX] =
{
	"Mus: ", "Agi: ", "Vig: ", "Sch: ", "Ego: ", "Chr: "
};


/*
 * Certain "screens" always use the main screen, including News, Birth,
 * Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
 *
 * Later, special flags may allow sub-windows to "steal" stuff from the
 * main window, including File dump (help), File dump (artifacts, uniques),
 * Character screen, Small scale map, Previous Messages, Store screen, etc.
 */
cptr window_flag_desc[32] =
{
	"Display inven/equip",
	"Display equip/inven",
	"Display player (basic)",
	"Display player (extra)",
	NULL,
	NULL,
	"Display messages",
	"Display overhead view",
	"Display monster recall",
	"Display object recall",
	NULL,
	"Display snap-shot",
	"Display visible monsters",
	NULL,
	"Display borg messages",
	"Display borg status",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};


/*
 * Options -- textual names (where defined)
 */
cptr option_text[OPT_MAX] =
{
	"rogue_like_commands",		/* OPT_rogue_like_commands */
	"quick_messages",			/* OPT_quick_messages */
	"use_old_target",			/* OPT_use_old_target */
	"floor_query_flag",			/* OPT_floor_query_flag */
	"carry_query_flag",			/* OPT_carry_query_flag */
	"always_pickup",			/* OPT_always_pickup */
	NULL,			/* old always_repeat */
	"depth_in_feet",			/* OPT_depth_in_feet */
	NULL,		/* old stack_force_notes */
	NULL,		/* old stack_force_costs */
	"show_labels",				/* OPT_show_labels */
	"show_weights",				/* OPT_show_weights */
	"show_choices",				/* OPT_show_choices */
	"show_details",				/* OPT_show_details */
	"ring_bell",				/* OPT_ring_bell */
	"show_flavors",				/* OPT_flavors */
	"run_ignore_stairs",		/* OPT_run_ignore_stairs */
	"run_ignore_doors",			/* OPT_run_ignore_doors */
	"run_cut_corners",			/* OPT_run_cut_corners */
	"run_use_corners",			/* OPT_run_use_corners */
	"disturb_move",				/* OPT_disturb_move */
	"disturb_near",				/* OPT_disturb_near */
	"disturb_panel",			/* OPT_disturb_panel */
	"disturb_state",			/* OPT_disturb_state */
	"disturb_minor",			/* OPT_disturb_minor */
	"disturb_other",			/* OPT_disturb_other */
	"alert_hitpoint",			/* OPT_alert_hitpoint */
	"alert_failure",			/* OPT_alert_failure */
	"verify_destroy",			/* OPT_verify_destroy */
	"verify_special",			/* OPT_verify_special */
	"allow_quantity",			/* OPT_allow_quantity */
	"spell_book_select",		/* OPT_spell_book_select */
	"auto_haggle",				/* OPT_auto_haggle */
	NULL,				/* OPT_auto_scum */
	"menu_allowed",						/* OPT_menu_allowed */
	NULL,						/* xxx testing_carry */
	"expand_look",				/* OPT_expand_look */
	"expand_list",				/* OPT_expand_list */
	"view_perma_grids",			/* OPT_view_perma_grids */
	"view_torch_grids",			/* OPT_view_torch_grids */
	"dungeon_align",			/* OPT_dungeon_align */
	"dungeon_stair",			/* OPT_dungeon_stair */
	NULL,			/* xxx */
	NULL,			/* xxx */
	NULL,						/* xxx track_follow */
	NULL,						/* xxx track_target */
	NULL,				/* xxx */
	"smart_cheat",				/* OPT_smart_cheat */
	"view_reduce_lite",			/* OPT_view_reduce_lite */
	"hidden_player",			/* OPT_hidden_player */
	"avoid_abort",				/* OPT_avoid_abort */
	"avoid_other",				/* OPT_avoid_other */
	"flush_failure",			/* OPT_flush_failure */
	"flush_disturb",			/* OPT_flush_disturb */
	NULL,						/* xxx flush_command */
	"fresh_before",				/* OPT_fresh_before */
	"fresh_after",				/* OPT_fresh_after */
	NULL,						/* xxx fresh_message */
	"compress_savefile",		/* OPT_compress_savefile */
	"hilite_player",			/* OPT_hilite_player */
	"view_yellow_lite",			/* OPT_view_yellow_lite */
	"view_bright_lite",			/* OPT_view_bright_lite */
	"view_granite_lite",		/* OPT_view_granite_lite */
	"view_special_lite",		/* OPT_view_special_lite */
	"easy_open",				/* OPT_easy_open */
	"easy_alter",				/* OPT_easy_alter */
	"easy_floor",				/* OPT_easy_floor */
	"show_piles",				/* OPT_show_piles */
	"center_player",			/* OPT_center_player */
	"run_avoid_center",			/* OPT_run_avoid_center */
	"scroll_target",			/* OPT_scroll_target */
	"auto_more",				/* OPT_auto_more */
	NULL,			/* xxx */
	NULL,				/* xxx */
	"exp_need",					/* OPT_exp_need */
	"verify_leave_quests",		/* OPT_verify_leave_quest */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	"birth_preserve",			/* OPT_birth_preserve */
	"birth_ironman",			/* OPT_birth_ironman */
	"birth_no_stores",			/* OPT_birth_no_stores */
	"birth_no_artifacts",		/* OPT_birth_no_artifacts */
	NULL,			/* OPT_birth_rand_artifacts */
	NULL,		/* OPT_birth_point_based */
	NULL,		/* OPT_birth_auto_roller */
	NULL,			/* OPT_birth_maximize */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	"cheat_peek",				/* OPT_cheat_peek */
	"cheat_hear",				/* OPT_cheat_hear */
	"cheat_room",				/* OPT_cheat_room */
	"cheat_xtra",				/* OPT_cheat_xtra */
	"cheat_know",				/* OPT_cheat_know */
	"cheat_live",				/* OPT_cheat_live */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	"adult_preserve",			/* OPT_adult_preserve */
	"adult_ironman",			/* OPT_adult_ironman */
	"adult_no_stores",			/* OPT_adult_no_stores */
	"adult_no_artifacts",		/* OPT_adult_no_artifacts */
	"adult_rand_artifacts",		/* OPT_adult_rand_artifacts */
	NULL,						/* OLD OPT_adult_point_based */
	NULL,						/* OLD OPT_adult_auto_roller */
	NULL,						/* OLD OPT_adult_maximize */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	"score_peek",				/* OPT_score_peek */
	"score_hear",				/* OPT_score_hear */
	"score_room",				/* OPT_score_room */
	"score_xtra",				/* OPT_score_xtra */
	"score_know",				/* OPT_score_know */
	"score_live",				/* OPT_score_live */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL,						/* xxx */
	NULL						/* xxx */
};


/*
 * Options -- descriptions (where defined)
 */
cptr option_desc[OPT_MAX] =
{
	"Rogue-like commands",						/* OPT_rogue_like_commands */
	"Activate quick messages",					/* OPT_quick_messages */
	"Use old target by default",				/* OPT_use_old_target */
	"Prompt for floor item selection",			/* OPT_floor_query_flag */
	"Prompt before picking things up",			/* OPT_carry_query_flag */
	"Pick things up by default",				/* OPT_always_pickup */
	NULL,					/* old always_repeat */
	"Show dungeon level in feet",				/* OPT_depth_in_feet */
	NULL,			/* old stack_force_notes */
	NULL,			/* old stack_force_costs */
	"Show labels in equipment listings",		/* OPT_show_labels */
	"Show weights in all object listings",		/* OPT_show_weights */
	"Show choices in inven/equip windows",		/* OPT_show_choices */
	"Show details in monster descriptions",		/* OPT_show_details */
	"Audible bell (on errors, etc)",			/* OPT_ring_bell */
	"Show flavors in object descriptions",		/* OPT_show_flacors */
	"When running, ignore stairs",				/* OPT_run_ignore_stairs */
	"When running, ignore doors",				/* OPT_run_ignore_doors */
	"When running, cut corners",				/* OPT_run_cut_corners */
	"When running, use corners",				/* OPT_run_use_corners */
	"Disturb whenever any monster moves",		/* OPT_disturb_move */
	"Disturb whenever viewable monster moves",	/* OPT_disturb_near */
	"Disturb whenever map panel changes",		/* OPT_disturb_panel */
	"Disturb whenever player state changes",	/* OPT_disturb_state */
	"Disturb whenever boring things happen",	/* OPT_disturb_minor */
	"Disturb whenever various things happen",	/* OPT_disturb_other */
	"Alert user to critical hitpoints",			/* OPT_alert_hitpoint */
	"Alert user to various failures",			/* OPT_alert_failure */
	"Verify destruction of objects",			/* OPT_verify_destroy */
	"Verify use of special commands",			/* OPT_verify_special */
	"Allow quantity specification",				/* OPT_allow_quantity */
	"Toggle inven/equip spellbook select",		/* OPT_spell_book_select */
	"Auto-haggle in stores",					/* OPT_auto_haggle */
	NULL,				/* xxx auto_scum */
	"Allow the menu (enter key)",										/* OPT_menu_allowed */
	NULL,										/* xxx testing_carry */
	"Expand the power of the look command",		/* OPT_expand_look */
	"Expand the power of the list commands",	/* OPT_expand_list */
	"Map remembers all perma-lit grids",		/* OPT_view_perma_grids */
	"Map remembers all torch-lit grids",		/* OPT_view_torch_grids */
	"Generate dungeons with aligned rooms",		/* OPT_dungeon_align */
	"Generate dungeons with connected stairs",	/* OPT_dungeon_stair */
	NULL,	/* xxx */
	NULL,	/* xxx */
	NULL,										/* xxx track_follow */
	NULL,										/* xxx track_target */
	NULL,		/* xxx */
	"Monsters exploit players weaknesses",		/* OPT_smart_cheat */
	"Reduce lite-radius when running",			/* OPT_view_reduce_lite */
	"Hide player symbol when running",			/* OPT_hidden_player */
	"Avoid checking for user abort",			/* OPT_avoid_abort */
	"Avoid processing special colors",			/* OPT_avoid_other */
	"Flush input on various failures",			/* OPT_flush_failure */
	"Flush input whenever disturbed",			/* OPT_flush_disturb */
	NULL,										/* xxx */
	"Flush output before every command",		/* OPT_fresh_before */
	"Flush output after various things",		/* OPT_fresh_after */
	NULL,										/* xxx */
	"Compress messages in savefiles",			/* OPT_compress_savefile */
	"Hilite the player with the cursor",		/* OPT_hilite_player */
	"Use special colors for torch lite",		/* OPT_view_yellow_lite */
	"Use special colors for field of view",		/* OPT_view_bright_lite */
	"Use special colors for wall grids",		/* OPT_view_granite_lite */
	"Use special colors for floor grids",		/* OPT_view_special_lite */
	"Open/Disarm/Close without direction",		/* OPT_easy_open */
	"Open/Disarm doors/traps on movement",		/* OPT_easy_alter */
	"Display floor stacks in a list",   		/* OPT_easy_floor */
	"Show stacks using special attr/char",		/* OPT_show_piles */
	"Center map continuously (very slow)",		/* OPT_center_player */
	"Avoid centering while running",			/* OPT_run_avoid_center */
	"Scroll map while targetting",				/* OPT_scroll_target */
	"Automatically clear '-more-' prompts",		/* OPT_auto_more */
	NULL,		/* xxx */
	NULL,	/* xxx */
	"Display experience needed for next level",	/* OPT_exp_need */
	"Verify before descending from quest level",/* OPT_verify_leave_quest */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	"Birth: Preserve artifacts when leaving level",	/* OPT_birth_preserve */
	"Birth: Restrict the use of stairs/recall",	/* OPT_birth_ironman */
	"Birth: Restrict the use of stores/home",	/* OPT_birth_no_stores */
	"Birth: Restrict creation of artifacts",	/* OPT_birth_no_artifacts */
	NULL, /* OPT_birth_rand_artifacts */
	NULL,	/* OPT_birth_point_based */
	NULL,	/* OPT_birth_auto_roller */
	NULL,	/* OPT_birth_maximize */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	"Cheat: Peek into object creation",			/* OPT_cheat_peek */
	"Cheat: Peek into monster creation",		/* OPT_cheat_hear */
	"Cheat: Peek into dungeon creation",		/* OPT_cheat_room */
	"Cheat: Peek into something else",			/* OPT_cheat_xtra */
	"Cheat: Know complete monster info",		/* OPT_cheat_know */
	"Cheat: Allow player to avoid death",		/* OPT_cheat_live */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	"Adult: Preserve artifacts when leaving level",	/* OPT_adult_preserve */
	"Adult: Restrict the use of stairs/recall",	/* OPT_adult_ironman */
	"Adult: Restrict the use of stores/home",	/* OPT_adult_no_stores */
	"Adult: Restrict creation of artifacts",	/* OPT_adult_no_artifacts */
	"Adult: Randomize some of the artifacts (beta)",	/* OPT_adult_rand_artifacts */
	NULL,	/* OPT_adult_point_based */
	NULL,	/* OPT_adult_auto_roller */
	NULL,	/* OPT_adult_maximize */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	"Score: Peek into object creation",			/* OPT_score_peek */
	"Score: Peek into monster creation",		/* OPT_score_hear */
	"Score: Peek into dungeon creation",		/* OPT_score_room */
	"Score: Peek into something else",			/* OPT_score_xtra */
	"Score: Know complete monster info",		/* OPT_score_know */
	"Score: Allow player to avoid death",		/* OPT_score_live */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL,										/* xxx */
	NULL										/* xxx */
};


/*
 * Options -- normal values
 */
const bool option_norm[OPT_MAX] =
{
	FALSE,		/* OPT_rogue_like_commands */
	TRUE,		/* OPT_quick_messages */
	FALSE,		/* OPT_use_old_target */
	FALSE,		/* OPT_floor_query_flag */
	TRUE,		/* OPT_carry_query_flag */
	TRUE,		/* OPT_always_pickup */
	FALSE,		/* xxx old always_repeat */
	FALSE,		/* OPT_depth_in_feet */
	FALSE,		/* xxx old stack_force_notes */
	FALSE,		/* OPT_stack_force_costs */
	TRUE,		/* OPT_show_labels */
	TRUE,		/* OPT_show_weights */
	TRUE,		/* OPT_show_choices */
	TRUE,		/* OPT_show_details */
	TRUE,		/* OPT_ring_bell */
	TRUE,		/* OPT_show_flavors */
	TRUE,		/* OPT_run_ignore_stairs */
	TRUE,		/* OPT_run_ignore_doors */
	TRUE,		/* OPT_run_cut_corners */
	TRUE,		/* OPT_run_use_corners */
	TRUE,		/* OPT_disturb_move */
	TRUE,		/* OPT_disturb_near */
	TRUE,		/* OPT_disturb_panel */
	TRUE,		/* OPT_disturb_state */
	TRUE,		/* OPT_disturb_minor */
	TRUE,		/* OPT_disturb_other */
	TRUE,		/* OPT_alert_hitpoint */
	FALSE,		/* OPT_alert_failure */
	TRUE,		/* OPT_verify_destroy */
	FALSE,		/* OPT_verify_special */
	TRUE,		/* OPT_allow_quantity */
	TRUE,		/* OPT_spell_book_select */
	TRUE,		/* OPT_auto_haggle */
	FALSE,		/* xxx auto_scum */
	TRUE,		/* OPT_menu_allowed */
	FALSE,		/* xxx */
	TRUE,		/* OPT_expand_look */
	TRUE,		/* OPT_expand_list */
	TRUE,		/* OPT_view_perma_grids */
	TRUE,		/* OPT_view_torch_grids */
	TRUE,		/* OPT_dungeon_align */
	TRUE,		/* OPT_dungeon_stair */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx track_follow */
	FALSE,		/* xxx track_target */
	FALSE,		/* xxx */
	TRUE,		/* OPT_smart_cheat */
	TRUE,		/* OPT_view_reduce_lite */
	FALSE,		/* OPT_hidden_player */
	FALSE,		/* OPT_avoid_abort */
	FALSE,		/* OPT_avoid_other */
	TRUE,		/* OPT_flush_failure */
	FALSE,		/* OPT_flush_disturb */
	FALSE,		/* xxx */
	TRUE,		/* OPT_fresh_before */
	FALSE,		/* OPT_fresh_after */
	FALSE,		/* xxx */
	TRUE,		/* OPT_compress_savefile */
	TRUE,		/* OPT_hilite_player */
	TRUE,		/* OPT_view_yellow_lite */
	TRUE,		/* OPT_view_bright_lite */
	TRUE,		/* OPT_view_granite_lite */
	TRUE,		/* OPT_view_special_lite */
	TRUE,		/* OPT_easy_open */
	TRUE,		/* OPT_easy_alter */
	FALSE,		/* OPT_easy_floor */
	FALSE,		/* OPT_show_piles */
	FALSE,		/* OPT_center_player */
	FALSE,		/* OPT_run_avoid_center */
	FALSE,		/* OPT_scroll_target */
	FALSE,		/* OPT_auto_more */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	TRUE,		/* OPT_exp_need */
	TRUE,		/* OPT_verify_leave_quest */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	TRUE,		/* OPT_birth_preserve */
	FALSE,		/* OPT_birth_ironman */
	FALSE,		/* OPT_birth_no_stores */
	FALSE,		/* OPT_birth_no_artifacts */
	FALSE,		/* OPT_birth_rand_artifacts */
	FALSE,		/* OPT_birth_point_based */
	FALSE,		/* OPT_birth_auto_roller */
	FALSE,		/* OPT_birth_maximize */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* OPT_cheat_peek */
	FALSE,		/* OPT_cheat_hear */
	FALSE,		/* OPT_cheat_room */
	FALSE,		/* OPT_cheat_xtra */
	FALSE,		/* OPT_cheat_know */
	FALSE,		/* OPT_cheat_live */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	TRUE,		/* OPT_adult_preserve */
	FALSE,		/* OPT_adult_ironman */
	FALSE,		/* OPT_adult_no_stores */
	FALSE,		/* OPT_adult_no_artifacts */
	FALSE,		/* OPT_adult_rand_artifacts */
	FALSE,		/* OPT_adult_point_based */
	FALSE,		/* OPT_adult_auto_roller */
	FALSE,		/* OPT_adult_maximize */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* OPT_score_peek */
	FALSE,		/* OPT_score_hear */
	FALSE,		/* OPT_score_room */
	FALSE,		/* OPT_score_xtra */
	FALSE,		/* OPT_score_know */
	FALSE,		/* OPT_score_live */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE,		/* xxx */
	FALSE		/* xxx */
};


/*
 * Option screen interface
 *
 * Note the special significance given to the constant "255".
 */
const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** User-Interface ***/

	{
		OPT_rogue_like_commands,
		OPT_quick_messages,
		OPT_use_old_target,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_always_pickup,
		255,
		OPT_depth_in_feet,
		OPT_exp_need,
		OPT_verify_leave_quest,
		255,
		255,
		OPT_show_labels,
		OPT_show_weights,
		OPT_show_choices,
		OPT_show_details,
		OPT_show_flavors,
		OPT_ring_bell,
		OPT_menu_allowed,
		255
	},

	/*** Disturbance ***/

	{
		OPT_run_ignore_stairs,
		OPT_run_ignore_doors,
		OPT_run_cut_corners,
		OPT_run_use_corners,
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_panel,
		OPT_disturb_state,
		OPT_disturb_minor,
		OPT_disturb_other,
		OPT_alert_hitpoint,
		OPT_alert_failure,
		OPT_verify_destroy,
		OPT_verify_special,
		OPT_allow_quantity,
		OPT_spell_book_select,
		OPT_auto_more,
		255,
		255,
		255
	},

	/*** Game-Play ***/

	{
		OPT_auto_haggle,
		255,
		OPT_expand_look,
		OPT_expand_list,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		OPT_dungeon_align,
		OPT_dungeon_stair,
		255,
		255,
		255,
		255,
		255,
		OPT_smart_cheat,
		OPT_easy_open,
		OPT_easy_alter,
		OPT_easy_floor,
		OPT_show_piles,
		255,
		255
	},

	/*** Efficiency ***/

	{
		OPT_view_reduce_lite,
		OPT_hidden_player,
		OPT_avoid_abort,
		OPT_avoid_other,
		OPT_flush_failure,
		OPT_flush_disturb,
		OPT_fresh_before,
		OPT_fresh_after,
		OPT_compress_savefile,
		OPT_hilite_player,
		OPT_view_yellow_lite,
		OPT_view_bright_lite,
		OPT_view_granite_lite,
		OPT_view_special_lite,
 		OPT_center_player,
 		OPT_run_avoid_center,
		OPT_scroll_target,
		255,
		255,
		255,
	},

	/*** Birth ***/

	{
		OPT_birth_preserve,
		OPT_birth_ironman,
		OPT_birth_no_stores,
		OPT_birth_no_artifacts,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255
	},

	/*** Cheat ***/

	{
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_xtra,
		OPT_cheat_know,
		OPT_cheat_live,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255
	}
};


cptr inscrip_text[MAX_INSCRIP] =
{
	NULL,
	"shattered",
	"worthless",
	"cursed",
	"broken",
	"average",
	"good",
	"excellent",
	"special",
	"uncursed",
	"indestructible",
	"weird",
	"twisted"
};



byte mana_cost_RF4[32]=
{
	1,			/* RF4_SHIEIK */
	0,			/* RF4_LASH */
	0,			/* RF4_ARROW */
	0,			/* RF4_GUN */
	0,			/* RF4_RIFLE */
	0,			/* RF4_SHOTGUN */
	0,			/* RF4_ROCKET */
	0,			/* RF4_MISSILE */
	0,			/* RF4_BR_FIRE */
	0,			/* RF4_BR_EARTH */
	0,			/* RF4_BR_AIR */
	0,			/* RF4_BR_WATER */
	0,			/* RF4_BR_ELEC */
	0,			/* RF4_BR_ICE */
	0,			/* RF4_BR_ACID */
	0,			/* RF4_BR_POISON */
	0,			/* RF4_BR_TIME */
	0,			/* RF4_BR_ETHER */
	0,			/* RF4_BR_SOUND */
	0,			/* RF4_BR_NETHER */
	0,			/* RF4_BR_GRAVITY */
	0,			/* RF4_BR_RAD */
	0,			/* RF4_BR_LIGHT */
	0,			/* RF4_BR_DARK */
	0,			/* RF4_CLOUD_RAD */
	0,			/* RF4_CLOUD_POISON */
	0,			/* RF4_XXX3 */
	0,			/* RF4_XXX4 */
	0,			/* RF4_XXX5 */
	0,			/* RF4_XXX6 */
	0,			/* RF4_FOG_NETHER */
	0			/* RF4_FOG_POISON */
};

byte mana_cost_RF5[32]=
{
	4,			/* RF5_BA_FIRE */
	4,			/* RF5_BA_EARTH */
	4,			/* RF5_BA_AIR */
	4,			/* RF5_BA_WATER */
	5,			/* RF5_BA_ELEC */
	5, 			/* RF5_BA_ICE */
	5, 			/* RF5_BA_ACID */
	5, 			/* RF5_BA_POISON */
	6, 			/* RF5_BA_TIME */
	6, 			/* RF5_BA_ETHER */
	6, 			/* RF5_BA_SOUND */
	6, 			/* RF5_BA_NETHER */
	7, 			/* RF5_BA_GRAVITY */
	7, 			/* RF5_BA_EMP */
	7, 			/* RF5_BA_RAD */
	0, 			/* RF5_XXX1 */
	4, 			/* RF5_BO_FIRE */
	4, 			/* RF5_BO_EARTH */
	4, 			/* RF5_BO_AIR */
	4, 			/* RF5_BO_WATER */
	4, 			/* RF5_BO_ELEC */
	4, 			/* RF5_BO_ICE */
	4, 			/* RF5_BO_ACID */
	4, 			/* RF5_BO_POISON */
	5, 			/* RF5_BO_TIME */
	5, 			/* RF5_BO_ETHER */
	5, 			/* RF5_BO_SOUND */
	5, 			/* RF5_BO_NETHER */
	5, 			/* RF5_BO_GRAVITY */
	3, 			/* RF5_BO_DARK */
	0, 			/* RF5_XXX3 */
	0  			/* RF5_XXX4 */
};

byte mana_cost_RF6[32]=
{
	6, 			/* RF6_HASTE */
	3, 			/* RF6_CURE */
	5, 			/* RF6_HEAL */
	0, 			/* RF6_ADD_MANA */
	1, 			/* RF6_BLINK */
	6, 			/* RF6_TPORT */
	0, 			/* RF6_XXX1 */
	0, 			/* RF6_XXX2 */
	4, 			/* RF6_TELE_TO */
	8, 			/* RF6_TELE_AWAY */
	8, 			/* RF6_TELE_LEVEL */
	4, 			/* RF6_TELE_SELF_TO */
	1, 			/* RF6_DARKNESS */
	2, 			/* RF6_TRAPS */
	6, 			/* RF6_FORGET */
	1, 			/* RF6_FEAR */
	4, 			/* RF6_PSI */
	6, 			/* RF6_DOMINATION */
	1, 			/* RF6_STUN */
	3, 			/* RF6_TK */
	5, 			/* RF6_FORCE */
	1, 			/* RF6_CONFUSION */
	4, 			/* RF6_SPIRIT */
	6, 			/* RF6_ECTOPLASM */
	3, 			/* RF6_BLIND */
	5, 			/* RF6_SLOW */
	6, 			/* RF6_HOLD */
	0, 			/* RF6_DRAIN_MANA */
	0, 			/* RF6_CHARGE */
	0, 			/* RF6_XXX4 */
	0, 			/* RF6_XXX5 */
	60  		/* RF6_MIRROR_IMAGE */
};

byte mana_cost_RF7[32]=
{
	3,			/* RF7_BE_FIRE */
	3, 			/* RF7_BE_ELEC */
	3, 			/* RF7_BE_WATER */
	0,			/* RF7_XXX1 */
	0,			/* RF7_XXX1 */
	0,			/* RF7_XXX3 */
	0, 			/* RF7_XXX4 */
	0, 			/* RF7_XXX5 */
	0,	 		/* RF7_XXX1 */
	0,			/* RF7_XXX1 */
	0,			/* RF7_XXX1 */
	0,			/* RF7_XXX1 */
	0, 			/* RF7_XXX6 */
	0, 			/* RF7_XXX7 */
	4,			/* RF7_S_CUTTENCLIP */
	13, 		/* RF7_S_BEASTMEN */
	10, 		/* RF7_S_PLANTS */
	12,			/* RF7_S_KIN */
	16,			/* RF7_S_HI_DEMON */
	8, 			/* RF7_S_MONSTER */
	15,			/* RF7_S_MONSTERS */
	15,			/* RF7_S_AUTOMATA */
	12,			/* RF7_S_SPIDER */
	14,			/* RF7_S_HOUND */
	15,			/* RF7_S_MONKEY */
	10,			/* RF7_S_ALIEN */
	10,			/* RF7_S_DEMON */
	12,			/* RF7_S_UNDEAD */
	15,			/* RF7_S_ELEMENTAL */
	22,			/* RF7_S_HI_UNDEAD */
	22,			/* RF7_S_HI_ELEMENTAL */
	22 			/* RF7_S_UNIQUE */
};

/*
 * d_base:     base desirability for AI.
 * d_summ:     desriability for AI per monster level
 *                  times 0-3 based on number of clear spaces
 * d_hurt:     desirability for AI per monster spell power
 *                  times 0-3 based on damage taken
 * d_mana:     desirability for AI per monster spell power
 *                  times 0-2 based on mana shortage
 * d_esc:      desirability for AI per monster level
 *                  times 0-3 based on fear, and damage taken
 * d_tact:     desirability for AI per monster level, modified
 *                  times 0-3 based on proximity, min_range, and best_range
 * d_res:      category of 'resistability' checked by monster AI
 *                 for purposes of desirability.
 * d_range:    % of spell desirability retained for each step past 'range'
 */

byte spell_desire_RF4[32][8] =
{
/*  d_base	  d_hurt  d_esc	     d_res				    */
/*	     d_summ	  d_mana   d_tact	       d_range		    */
	{ 30,  0,   0,   5,	0,   0,	   0	  ,  100}, /* RF4_SHRIEK	*/
	{ 40,  0,   0,   5,	0,   0,	   0	  ,    0}, /* RF4_LASH		*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_ARROW		*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_GUN		*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_RIFLE		*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_SHOTGUN	*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_ROCKET	*/
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,   20}, /* RF4_MISSILE	*/
	{ 65,  0,   0,   5,	0,   0, LRN_FIRE  ,   90}, /* RF4_BR_FIRE	*/
	{ 65,  0,   0,   5,	0,   0, LRN_EARTH ,   90}, /* RF4_BR_EARTH	*/
	{ 65,  0,   0,   5,	0,   0, LRN_AIR   ,   90}, /* RF4_BR_AIR	*/
	{ 65,  0,   0,   5,	0,   0, LRN_WATER ,   90}, /* RF4_BR_WATER	*/
	{ 65,  0,   0,   5,	0,   0, LRN_ELEC  ,   90}, /* RF4_BR_ELEC	*/
	{ 65,  0,   0,   5,	0,   0, LRN_ICE   ,   90}, /* RF4_BR_ICE	*/
	{ 65,  0,   0,   5,	0,   0, LRN_ACID  ,   90}, /* RF4_BR_ACID	*/
	{ 65,  0,   0,   5,	0,   0, LRN_POISON,   90}, /* RF4_BR_POISON	*/
	{ 65,  0,   0,   5,	0,   0, LRN_TIME  ,   90}, /* RF4_BR_TIME	*/
	{ 65,  0,   0,   5,	0,   0, LRN_ETHER ,   90}, /* RF4_BR_ETHER	*/
	{ 65,  0,   0,   5,	0,   0, LRN_SOUND ,   90}, /* RF4_BR_SOUND	*/
	{ 65,  0,   0,   5,	0,   0,	LRN_NETHER,   90}, /* RF4_BR_NETHER	*/
	{ 50,  0,   0,   5,	0,   0,    0      ,   90}, /* RF4_BR_GRAVITY*/
	{ 50,  0,   0,   5,	0,   0,    0      ,   90}, /* RF4_BR_RAD	*/
	{ 65,  0,   0,   5,	0,   0, LRN_LIGHT ,   90}, /* RF4_BR_LIGHT	*/
	{ 65,  0,   0,   5,	0,   0, LRN_DARK  ,   90}, /* RF4_BR_DARK	*/
	{ 65,  0,   0,   5,	0,   0,    0      ,   90}, /* RF4_CLOUD_RAD	*/
	{ 65,  0,   0,   5,	0,   0,    0      ,   90}, /* RF4_CLOUD_POISON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX3		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX4		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX5		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX6		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_FOG_NETHER		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}  /* RF4_FOG_POISON		*/
};

byte spell_desire_RF5[32][8] =
{
/*  d_base	  d_hurt   d_esc      d_res				    */
/*	     d_summ	  d_mana  d_tact	        d_range		    */
	{ 50,  0,   0,   0,	0,   0, LRN_FIRE  ,  100}, /* RF5_BA_FIRE	*/
	{ 50,  0,   0,   0,	0,   0, LRN_EARTH ,  100}, /* RF5_BA_EARTH	*/
	{ 50,  0,   0,   0,	0,   0, LRN_AIR   ,  100}, /* RF5_BA_AIR	*/
	{ 50,  0,   0,   0,	0,   0, LRN_WATER ,  100}, /* RF5_BA_WATER	*/
	{ 40,  0,   0,   0,	0,   0, LRN_ELEC  ,  100}, /* RF5_BA_ELEC	*/
	{ 40,  0,   0,   0,	0,   0, LRN_ICE   ,  100}, /* RF5_BA_ICE	*/
	{ 40,  0,   0,   0,	0,   0, LRN_ACID  ,  100}, /* RF5_BA_ACID	*/
	{ 40,  0,   0,   0,	0,   0, LRN_POISON,  100}, /* RF5_BA_POISON	*/
	{ 30,  0,   0,   0,	0,   0, LRN_TIME  ,  100}, /* RF5_BA_TIME	*/
	{ 30,  0,   0,   0,	0,   0, LRN_ETHER ,  100}, /* RF5_BA_ETHER	*/
	{ 30,  0,   0,   0,	0,   0, LRN_SOUND ,  100}, /* RF5_BA_SOUND	*/
	{ 30,  0,   0,   0,	0,   0, LRN_NETHER,  100}, /* RF5_BA_NETHER	*/
	{ 20,  0,   0,   0,	0,   0,    0      ,  100}, /* RF5_BA_GRAVITY*/
	{ 20,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_BA_EMP	*/
	{ 20,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_BA_RAD	*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_XXX1		*/
	{ 50,  0,   0,   0,	0,   0, LRN_FIRE  ,  100}, /* RF5_BO_FIRE	*/
	{ 50,  0,   0,   0,	0,   0, LRN_EARTH ,  100}, /* RF5_BO_EARTH	*/
	{ 50,  0,   0,   0,	0,   0, LRN_AIR   ,  100}, /* RF5_BO_AIR	*/
	{ 50,  0,   0,   0,	0,   0, LRN_WATER ,  100}, /* RF5_BO_WATER	*/
	{ 45,  0,   0,   0,	0,   0, LRN_ELEC  ,  100}, /* RF5_BO_ELEC	*/
	{ 45,  0,   0,   0,	0,   0, LRN_ICE   ,  100}, /* RF5_BO_ICE	*/
	{ 45,  0,   0,   0,	0,   0, LRN_ACID  ,  100}, /* RF5_BO_ACID	*/
	{ 45,  0,   0,   0,	0,   0, LRN_POISON,  100}, /* RF5_BO_POISON	*/
	{ 35,  0,   0,   0,	0,   0, LRN_TIME  ,  100}, /* RF5_BO_TIME	*/
	{ 35,  0,   0,   0,	0,   0,	LRN_ETHER ,  100}, /* RF5_BO_ETHER	*/
	{ 35,  0,   0,   0,	0,   0,	LRN_SOUND ,  100}, /* RF5_BO_SOUND	*/
	{ 35,  0,   0,   0,	0,   0, LRN_NETHER,  100}, /* RF5_BO_NETHER */
	{ 25,  0,   0,   0,	0,   0,    0      ,  100}, /* RF5_BO_GRAVITY*/
	{ 45,  0,   0,   0,	0,   0,    0      ,  100}, /* RF5_BO_DARK	*/
	{ 0,   0,   0,   0,	0,   0,    0      ,  100}, /* RF5_XXX3		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}  /* RF5_XXX4 		*/
};

byte spell_desire_RF6[32][8] =
{
/*  d_base	  d_hurt  d_esc	     d_res				    */
/*	     d_summ	   d_mana  d_tact	          d_range		    */
	{ 50,  0,   0,   0,	0,   0,	   0	  ,  	100}, /* RF6_HASTE			*/
	{ 50,  0,   0,   0, 0,   0,	   0	  ,  	100}, /* RF6_CURE			*/
	{ 10,  0,   20,  0,	0,   0,	   0	  ,  	100}, /* RF6_HEAL			*/
	{ 15,  0,   0,   25,0,   0,	   0	  ,  	100}, /* RF6_ADD_MANA		*/
	{ 27,  0,   0,   0,	10,  15,   0	  ,  	100}, /* RF6_BLINK			*/
	{ 10,  0,   0,   0,	20,  10,   0	  ,  	100}, /* RF6_TPORT			*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  	100}, /* RF6_XXX1			*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  	100}, /* RF6_XXX2			*/
	{ 30,  0,   0,   0,	0,   10,   0	  ,  	100}, /* RF6_TELE_TO		*/
	{ 10,  0,   0,   0,	20,  10,   0	  ,  	100}, /* RF6_TELE_AWAY		*/
	{ 10,  0,   0,   0,	20,  10,   0      ,	    100}, /* RF6_TELE_LEVEL		*/
	{ 30,  0,   0,   0,	0,   0,	   0	  ,  	100}, /* RF6_TELE_SELF_TO	*/
	{ 20,  0,   0,   0,	5,   0,	   0	  ,  	100}, /* RF6_DARKNESS		*/
	{ 25,  0,   0,   0,	5,   0,	   0	  ,  	100}, /* RF6_TRAPS			*/
	{ 25,  0,   0,   0,	5,   0, LRN_SAVE  ,  	100}, /* RF6_FORGET			*/
	{ 25,  0,   0,   0, 0,   0, LRN_FEAR  ,     100}, /* RF6_FEAR			*/
	{ 15,  0,   0,   0,	0,   0,	LRN_PSI   ,  	100}, /* RF6_PSI			*/
	{ 10,  0,   0,   0,	0,   0,	LRN_PSI   ,  	100}, /* RF6_DOMINATION		*/
	{ 25,  0,   0,   0,	0,   0, LRN_TK    ,  	100}, /* RF6_STUN			*/
	{ 15,  0,   0,   0,	0,   0, LRN_TK    ,  	100}, /* RF6_TK				*/
	{ 10,  0,   0,   0,	0,   0, LRN_TK    ,  	100}, /* RF6_FORCE			*/
	{ 25,  0,   0,   0,	0,   0,	LRN_CONFU ,  	100}, /* RF6_CONFUSION		*/
	{ 15,  0,   0,   0,	0,   0,	LRN_SPIRIT,  	100}, /* RF6_SPIRIT			*/
	{ 10,  0,   0,   0,	0,   0,	LRN_SPIRIT,  	100}, /* RF6_ECTOPLASM		*/
	{ 30,  0,   0,   0,	0,   0,	LRN_BLIND ,  	100}, /* RF6_BLIND			*/
	{ 30,  0,   0,   0,	0,   0,	LRN_SAVE  ,  	100}, /* RF6_SLOW			*/
	{ 40,  0,   0,   0,	0,   0,	LRN_FREE_SAVE,  100}, /* RF6_HOLD			*/
	{ 25,  0,  15,   0,	0,   0, LRN_MANA  ,     100}, /* RF6_DRAIN_MANA		*/
	{ 25,   0,   0,   0,	0,   10,    0      ,	    0}, /* RF6_CHARGE			*/
	{ 0,   0,   0,   0,	0,   0,    0      ,	    100}, /* RF6_XXX4			*/
	{ 0,   0,   0,   0,	0,   0,    0      ,	    100}, /* RF6_XXX5			*/
	{ 20,  5,  18,   0,	0,   0,    0      ,	    100}  /* RF6_MIRROR_IMAGE	*/
};

byte spell_desire_RF7[32][8] =
{
/*     d_base	  d_hurt    d_esc	 d_res				    */
/*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 50,  0,   0,   0,	0,   0,	LRN_FIRE  ,  80}, /* RF7_BE_FIRE		*/
	{ 50,  0,   0,   0,	0,   0,	LRN_ELEC  ,  80}, /* RF7_BE_ELEC		*/
	{ 50,  0,   0,   0,	0,   0,	LRN_WATER ,  80}, /* RF7_BE_WATER		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X4		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X5		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X6		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X7		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X8		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X9		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X10		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X11		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X12		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X13		*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7X14		*/
	{ 0,   30,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_CUTTENCLIP		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_BEASTMEN	*/
	{ 0,   10,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_PLANTS		*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_KIN			*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DEMON	*/
	{ 0,   14,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_MONSTER		*/
	{ 0,   14,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_MONSTERS	*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_AUTOMATA	*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_SPIDER		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HOUND		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_MONKEY		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_ALIEN		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DEMON		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_UNDEAD		*/
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_ELEMENTAL	*/
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_UNDEAD	*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_ELEMENTAL*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}  /* RF7_S_UNIQUE  	*/
};

/*
 * Optimal Ranges for various spells.
 * 6 is optimal for Breath Weapons, Beams, and Arcs.
 * 3 is optimal for Lash/Spit.
 * 0 indicates no range limitation for other spells.
 *
 * This range is considered a preference if d_range in spell_desire is > 0.
 * It is a hard limit if d_range = 0.
 */
byte spell_range_RF4[32] =
{
	0,3,0,0,0,0,0,0,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0
};

byte spell_range_RF5[32] =
{
	6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,0,6,6,6,6,6,6,6,6,6,6,6,6,6,0,0,0
};

byte spell_range_RF6[32] =
{
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0
};

byte spell_range_RF7[32] =
{
	6,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};
