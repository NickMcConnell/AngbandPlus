/* File: nmagic1.c */

/* 
 * Purpose: Code for the new magic system. 
 * By Benjamin Mann, although large chunks are copied from elsewhere. 
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef USE_NEW_MAGIC
/* 
 * Calculate the potential level from the field and a constant
 */
static int L(int num, int action, int target)
{
	return p_ptr->magic_fields[action][target] ? 
		num/p_ptr->magic_fields[action][target] : 9999;
}

/*
 * Determine the pure "spell skill" spell levels
 */
static int calculate_spell_level_aux(int spell)
{
	switch (spell)
	{
	case 1 : /* Detect Invisibility */
		return MIN(MIN(MIN(MAX(L(25,MAG_CREATE,MAG_LIFE),L(15,MAG_DIVINE,MAG_LIFE)),L(10,MAG_DIVINE,MAG_MIND)),
		       MIN(L(20,MAG_DIVINE,MAG_NATURE),L(10,MAG_DIVINE,MAG_BODY))),
		       L(30,MAG_DIVINE,MAG_STUFF));
	case 2 : /* Detect Creatures */
		return L(5,MAG_DIVINE,MAG_NATURE);
	case 3 : /* Detect Undead and Demons */
		return L(5,MAG_DIVINE,MAG_STUFF);
	case 4 : /* Detect Evil */
		return MIN(L(5,MAG_CREATE,MAG_LIFE),L(2,MAG_DIVINE,MAG_LIFE));
	case 5 : /* Detect Monsters */
		return MIN(L(3,MAG_DIVINE,MAG_BODY),MIN(
			L(6,MAG_DIVINE,MAG_MIND),L(12,MAG_DIVINE,MAG_LIFE)));
	case 6 : /* Detect Traps, Doors and Stairs */
		return MIN(MIN(MAX(L(27,MAG_CREATE,MAG_LIFE),L(19,MAG_DIVINE,MAG_LIFE)),L(3,MAG_DIVINE,MAG_MATTER)),
			L(10,MAG_DIVINE,MAG_NATURE));
	case 7 : /* Detect Valuables */
		return L(9,MAG_DIVINE,MAG_MATTER);
	case 8 : /* Detect Enchantment */
		return L(30,MAG_DIVINE,MAG_MAGIC);
	case 9 : /* Detect Artifacts */
		return L(260,MAG_DIVINE,MAG_MAGIC);
	case 10 : /* Greater Detection */
		return MIN(L(60,MAG_DIVINE,MAG_REAL),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_REAL] > 0 ? 30 : 9999));
	case 11 : /* Magic Mapping */
		return MIN(L(27,MAG_DIVINE,MAG_MATTER),L(40,MAG_DIVINE,MAG_NATURE));
	case 12 : /* Awareness */
		return MIN(L(63,MAG_DIVINE,MAG_REAL),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_REAL] > 0 ? 32 : 9999));
	case 13 : /* Absolute Awareness */
		return MAX(L(340,MAG_DIVINE,MAG_MAGIC),L(150,MAG_DIVINE,MAG_REAL));
	case 14 : /* Clairvoyance */
		return MIN(L(66,MAG_DIVINE,MAG_REAL),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_REAL] > 0 ? 33 : 9999));
	case 15 : /* Call Sunlight */
		return MIN(L(190,MAG_CREATE,MAG_NATURE),L(168,MAG_CREATE,MAG_ENERGY));
	case 16 : /* Telepathy */
		return L(42,MAG_DIVINE,MAG_MIND);
	case 17 : /* Psychometry */
		return MIN(MAX(L(40,MAG_CREATE,MAG_LIFE),L(25,MAG_DIVINE,MAG_LIFE)),
			MIN(L(14,MAG_DIVINE,MAG_MAGIC),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_MAGIC] > 0 ? 8 : 9999)));
	case 18 : /* Identify */
		return MIN(MAX(L(94,MAG_CREATE,MAG_LIFE),L(67,MAG_DIVINE,MAG_LIFE)),
			MIN(L(30,MAG_DIVINE,MAG_MAGIC),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_MAGIC] > 0 ? 17 : 9999)));
	case 19 : /* General Identify */
		return L(55,MAG_DIVINE,MAG_MAGIC);
	case 20 : /* Area Identify */
		return MIN(MAX(L(234,MAG_CREATE,MAG_LIFE),L(168,MAG_DIVINE,MAG_LIFE)),
			MIN(L(75,MAG_DIVINE,MAG_MAGIC),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_MAGIC] > 0 ? 38 : 9999)));
	case 21 : /* Cave Identify */
		return L(400,MAG_DIVINE,MAG_MAGIC);
	case 22 : /* Greater Identify */
		return MIN(MAX(L(280,MAG_CREATE,MAG_LIFE),L(200,MAG_DIVINE,MAG_LIFE)),
			MIN(L(90,MAG_DIVINE,MAG_MAGIC),
			(p_ptr->magic_fields[MAG_DIVINE][MAG_MAGIC] > 0 ? 45 : 9999)));
	case 23 : /* Complete Item Knowledge */
		return L(120,MAG_DIVINE,MAG_MAGIC);
	case 24 : /* Greater Area Identify */
		return L(140,MAG_DIVINE,MAG_MAGIC);
	case 25 : /* Absolute Identify */
		return L(425,MAG_DIVINE,MAG_MAGIC);
	case 26 : /* Self Knowledge */
		return MIN(MIN(L(60,MAG_DIVINE,MAG_MIND),L(60,MAG_DIVINE,MAG_BODY)),
			MIN(L(100,MAG_DIVINE,MAG_LIFE),L(100,MAG_DIVINE,MAG_NATURE)));
	case 27 : /* Absolute Knowledge */
		return MAX(MAX(L(450,MAG_DIVINE,MAG_MAGIC),L(200,MAG_DIVINE,MAG_REAL)),
				MIN(L(200,MAG_DIVINE,MAG_MIND),L(200,MAG_DIVINE,MAG_BODY)));
	case 28 : /* Cure Light Wounds */
		return MIN(MIN(MAX(L(10,MAG_CREATE,MAG_LIFE),L(5,MAG_CHANGE,MAG_LIFE)),L(10,MAG_CREATE,MAG_NATURE)),
		       MIN(L(10,MAG_CHANGE,MAG_BODY),L(5,MAG_CHANGE,MAG_NATURE)));
	case 29 : /* Cure Normal Wounds */
		return MIN(MIN(MAX(L(40,MAG_CREATE,MAG_LIFE),L(25,MAG_CHANGE,MAG_LIFE)),L(90,MAG_CREATE,MAG_NATURE)),
		       MIN(L(50,MAG_CHANGE,MAG_BODY),L(25,MAG_CHANGE,MAG_NATURE)));
	case 30 : /* Cure Serious Wounds */
		return MIN(MIN(MAX(L(65,MAG_CREATE,MAG_LIFE),L(45,MAG_CHANGE,MAG_LIFE)),L(150,MAG_CREATE,MAG_NATURE)),
		       MIN(L(85,MAG_CHANGE,MAG_BODY),L(43,MAG_CHANGE,MAG_NATURE)));
	case 31 : /* Cure Critical Wounds */
		return MIN(MIN(MAX(L(105,MAG_CREATE,MAG_LIFE),L(75,MAG_CHANGE,MAG_LIFE)),L(250,MAG_CREATE,MAG_NATURE)),
		       MIN(L(135,MAG_CHANGE,MAG_BODY),L(63,MAG_CHANGE,MAG_NATURE)));
	case 32 : /* Healing */
		return MIN(MIN(MAX(L(140,MAG_CREATE,MAG_LIFE),L(100,MAG_CHANGE,MAG_LIFE)),L(340,MAG_CREATE,MAG_NATURE)),
		       MIN(L(180,MAG_CHANGE,MAG_BODY),L(80,MAG_CHANGE,MAG_NATURE)));
	case 33 : /* Greater Healing */
		return MIN(MIN(MAX(L(280,MAG_CREATE,MAG_LIFE),L(200,MAG_CHANGE,MAG_LIFE)),L(700,MAG_CREATE,MAG_NATURE)),
		       MIN(L(360,MAG_CHANGE,MAG_BODY),L(170,MAG_CHANGE,MAG_NATURE)));
	case 34 : /* Restore Life */
		return MIN(MIN(MAX(L(200,MAG_CREATE,MAG_LIFE),L(140,MAG_CHANGE,MAG_LIFE)),L(1000,MAG_CREATE,MAG_NATURE)),
		       MIN(L(225,MAG_CHANGE,MAG_MIND),L(250,MAG_CHANGE,MAG_NATURE)));
	case 35 : /* Restoration */
		return MIN(MIN(MAX(L(250,MAG_CREATE,MAG_LIFE),L(175,MAG_CHANGE,MAG_LIFE)),L(1250,MAG_CREATE,MAG_NATURE)),
		       MIN(L(320,MAG_CHANGE,MAG_BODY),L(310,MAG_CHANGE,MAG_NATURE)));
	case 36 : /* Neutralise Poison */
		return MIN(MIN(MAX(L(65,MAG_CREATE,MAG_LIFE),L(45,MAG_CHANGE,MAG_LIFE)),L(120,MAG_CHANGE,MAG_BODY)),
				MIN(L(30,MAG_CHANGE,MAG_NATURE),L(60,MAG_DESTROY,MAG_NATURE)));
	case 37 : /* Cure Wounds and Poison */
		return L(35,MAG_CHANGE,MAG_NATURE);
	case 38 : /* Remove Fear */
		return MIN(MAX(L(25,MAG_CREATE,MAG_LIFE),L(15,MAG_CHANGE,MAG_LIFE)),L(12,MAG_CONTROL,MAG_MIND));
	case 39 : /* Remove Curse */
		return MIN(MAX(L(50,MAG_CREATE,MAG_LIFE),L(35,MAG_CHANGE,MAG_LIFE)),L(66,MAG_DESTROY,MAG_MAGIC));
	case 40 : /* Dispel Curse */
		return MAX(L(115,MAG_CREATE,MAG_LIFE),L(80,MAG_CHANGE,MAG_LIFE));
	case 41 : /* Bless */
		return MIN(MAX(L(10,MAG_CREATE,MAG_LIFE),L(5,MAG_CONTROL,MAG_LIFE)),L(15,MAG_CHANGE,MAG_BODY));
	case 42 : /* Prayer */
		return MAX(L(110,MAG_CREATE,MAG_LIFE),L(75,MAG_CONTROL,MAG_LIFE));
	case 43 : /* Heroism */
		return MIN(MAX(L(40,MAG_CREATE,MAG_LIFE),L(25,MAG_CONTROL,MAG_LIFE)),
				MIN(L(50,MAG_CHANGE,MAG_MIND),L(25,MAG_CONTROL,MAG_MIND)));
	case 44 : /* Haste */
		return MIN(L(66,MAG_CHANGE,MAG_BODY),L(132,MAG_CREATE,MAG_ENERGY));
	case 45 : /* Berserk Strength */
		return MIN(L(50,MAG_CONTROL,MAG_MIND),L(100,MAG_CONTROL,MAG_BODY));
	case 46 : /* Battle Frenzy */
		return MIN(L(150,MAG_CONTROL,MAG_MIND),L(300,MAG_CONTROL,MAG_BODY));
	case 47 : /* Protection from Corrosion */
		return MIN(L(200,MAG_CHANGE,MAG_NATURE),L(105,MAG_CHANGE,MAG_MATTER));
	case 48 : /* Resist Cold */
		return MIN(L(30,MAG_CONTROL,MAG_BODY),L(24,MAG_CREATE,MAG_ENERGY));
	case 49 : /* Resist Fire */
		return MIN(L(30,MAG_CONTROL,MAG_BODY),L(24,MAG_DESTROY,MAG_ENERGY));
	case 50 : /* Resist Lightning */
		return MIN(L(30,MAG_CONTROL,MAG_BODY),L(24,MAG_DESTROY,MAG_ENERGY));
	case 51 : /* Resist Acid */
		return MIN(L(30,MAG_CONTROL,MAG_BODY),L(18,MAG_CONTROL,MAG_MATTER));
	case 52 : /* Resist Poison */
		return MIN(L(55,MAG_CONTROL,MAG_BODY),L(35,MAG_CONTROL,MAG_MATTER));
	case 53 : /* Resist Environment */
		return L(30,MAG_CONTROL,MAG_NATURE);
	case 54 : /* Resistance */
		return MIN(L(90,MAG_CONTROL,MAG_NATURE),L(100,MAG_CONTROL,MAG_BODY));
	case 55 : /* Stoneskin */
		return MIN(L(60,MAG_CHANGE,MAG_NATURE),L(50,MAG_CHANGE,MAG_BODY));
	case 56 : /* Globe of Invulnerability */
		return MIN(MAX(L(350,MAG_CREATE,MAG_LIFE),L(250,MAG_CONTROL,MAG_LIFE)),L(180,MAG_CONTROL,MAG_BODY));
	case 57 : /* Wraithform */
		return L(280,MAG_CHANGE,MAG_MATTER);
	case 58 : /* Light */
		return MIN(MIN(L(25,MAG_CREATE,MAG_LIFE),L(60,MAG_CREATE,MAG_MAGIC)),
				MIN(L(12,MAG_CREATE,MAG_ENERGY),L(10,MAG_CREATE,MAG_NATURE)));
	case 59 : /* Spear of Light */
		return MIN(L(60,MAG_CREATE,MAG_NATURE),L(48,MAG_CREATE,MAG_ENERGY));
	case 60 : /* Create Food */
		return MIN(MIN(L(50,MAG_CREATE,MAG_LIFE),L(10,MAG_CREATE,MAG_NATURE)),
				MIN(L(60,MAG_CREATE,MAG_BODY),L(40,MAG_CHANGE,MAG_BODY)));
	case 61 : /* See Invisible */
		return MIN(MIN(MIN(MAX(L(75,MAG_CREATE,MAG_LIFE),L(50,MAG_DIVINE,MAG_LIFE)),L(30,MAG_DIVINE,MAG_MIND)),
		       MIN(L(60,MAG_DIVINE,MAG_NATURE),L(30,MAG_DIVINE,MAG_BODY))),
		       MIN(L(30,MAG_DIVINE,MAG_LIFE),L(90,MAG_DIVINE,MAG_STUFF)));
	case 62 : /* Glyph of Warding */
		return MIN(L(235,MAG_CREATE,MAG_LIFE),L(168,MAG_CREATE,MAG_STUFF));
	case 63 : /* Greater Warding */
		return MIN(L(310,MAG_CREATE,MAG_LIFE),L(225,MAG_CREATE,MAG_STUFF));
	case 64 : /* Explosive Rune */
		return L(150,MAG_CREATE,MAG_ENERGY);
	case 65 : /* Touch of Confusion */
		return L(45,MAG_DESTROY,MAG_STUFF);
	case 66 : /* Confuse Monster */
		return MIN(L(12,MAG_CHANGE,MAG_MIND),L(90,MAG_DESTROY,MAG_STUFF));
	case 67 : /* Sleep */
		return MIN(L(18,MAG_CHANGE,MAG_MIND),L(34,MAG_CHANGE,MAG_NATURE));
	case 68 : /* Mass Sleep */
		return MIN(L(39,MAG_CHANGE,MAG_MIND),L(70,MAG_CHANGE,MAG_NATURE));
	case 69 : /* Slow Monster */
		return L(33,MAG_CHANGE,MAG_BODY);
	case 70 : /* Entangle */
		return L(80,MAG_CREATE,MAG_NATURE);
	case 71 : /* Scare Monster */
		return L(25,MAG_CHANGE,MAG_MIND);
	case 72 : /* Terror */
		return L(90,MAG_CHANGE,MAG_MIND);
	case 73 : /* Horrify */
		return L(45,MAG_CHANGE,MAG_MIND);
	case 74 : /* Hold Monster */
		return L(30,MAG_CONTROL,MAG_BODY);
	case 75 : /* Protection from Evil */
		return MAX(L(80,MAG_CREATE,MAG_LIFE),L(55,MAG_CONTROL,MAG_LIFE));
	case 76 : /* Mind Blast */
		return L(15,MAG_DESTROY,MAG_MIND);
	case 77 : /* Magic Missile */
		return 1;
	case 78 : /* Mana Bolt */
		return MIN(L(360,MAG_DESTROY,MAG_STUFF),L(240,MAG_CREATE,MAG_STUFF));
	case 79 : /* Mana Storm */
		return MIN(L(405,MAG_DESTROY,MAG_STUFF),L(270,MAG_CREATE,MAG_STUFF));
	case 80 : /* Hellfire */
		return L(210,MAG_DESTROY,MAG_LIFE);
	case 81 : /* Malediction */
		return L(10,MAG_DESTROY,MAG_LIFE);
	case 82 : /* Orb of Entropy */
		return L(60,MAG_DESTROY,MAG_LIFE);
	case 83 : /* Orb of Draining */
		return L(73,MAG_CREATE,MAG_LIFE);
	case 84 : /* Electrical Charge */
		return MIN(L(7,MAG_CREATE,MAG_ENERGY),L(10,MAG_DESTROY,MAG_STUFF));
	case 85 : /* Lightning Bolt */
		return MIN(L(40,MAG_CREATE,MAG_NATURE),L(36,MAG_CREATE,MAG_ENERGY));
	case 86 : /* Chain Lightning */
		return MIN(L(150,MAG_DESTROY,MAG_STUFF),L(90,MAG_CREATE,MAG_ENERGY));
	case 87 : /* Ball Lightning */
		return MIN(L(115,MAG_CREATE,MAG_NATURE),L(90,MAG_CREATE,MAG_ENERGY));
	case 88 : /* Lightning Storm */
		return MIN(L(190,MAG_CREATE,MAG_NATURE),L(198,MAG_CREATE,MAG_ENERGY));
	case 89 : /* Electrocution */
		return L(228,MAG_CREATE,MAG_ENERGY);
	case 90 : /* Absolute Lightning */
		return L(270,MAG_CREATE,MAG_ENERGY);
	case 91 : /* Freeze */
		return L(8,MAG_DESTROY,MAG_ENERGY);
	case 92 : /* Frost Bolt */
		return MIN(L(50,MAG_CREATE,MAG_NATURE),L(48,MAG_DESTROY,MAG_ENERGY));
	case 93 : /* Cold Ball */
		return L(114,MAG_DESTROY,MAG_ENERGY);
	case 94 : /* Ice Storm */
		return MIN(L(170,MAG_CREATE,MAG_NATURE),L(180,MAG_DESTROY,MAG_ENERGY));
	case 95 : /* Absolute Zero */
		return L(282,MAG_DESTROY,MAG_ENERGY);
	case 96 : /* Shoot Acid */
		return L(8,MAG_CREATE,MAG_MATTER);
	case 97 : /* Acid Bolt */
		return L(66,MAG_CREATE,MAG_MATTER);
	case 98 : /* Acid Ball */
		return L(126,MAG_CREATE,MAG_MATTER);
	case 99 : /* Mass Corrosion */
		return L(198,MAG_CREATE,MAG_MATTER);
	case 100 : /* Absolute Acid */
		return L(294,MAG_CREATE,MAG_MATTER);
	case 101 : /* Flame */
		return MIN(L(12,MAG_CREATE,MAG_ENERGY),L(18,MAG_DESTROY,MAG_STUFF));
	case 102 : /* Fire Bolt */
		return MIN(L(128,MAG_DESTROY,MAG_STUFF),L(78,MAG_CREATE,MAG_ENERGY));
	case 103 : /* Fireball */
		return MIN(L(270,MAG_DESTROY,MAG_STUFF),L(150,MAG_CREATE,MAG_ENERGY));
	case 104 : /* Firestorm */
		return L(234,MAG_CREATE,MAG_ENERGY);
	case 105 : /* Sunfire */
		return L(300,MAG_CREATE,MAG_ENERGY);
	case 106 : /* Flame Strike */
		return MIN(L(333,MAG_DESTROY,MAG_STUFF),L(222,MAG_CREATE,MAG_ENERGY));
	case 107 : /* Meteor Swarm */
		return MIN(L(315,MAG_DESTROY,MAG_STUFF),L(210,MAG_CREATE,MAG_ENERGY));
	case 108 : /* Stinking Cloud */
		return L(18,MAG_CREATE,MAG_MATTER);
	case 109 : /* Cloudkill */
		return L(150,MAG_CREATE,MAG_MATTER);
	case 110 : /* Absolute Poison */
		return L(246,MAG_CREATE,MAG_MATTER);
	case 111 : /* Whirlpool */
		return MIN(L(170,MAG_CREATE,MAG_NATURE),
				MAX(L(144,MAG_CREATE,MAG_MATTER),L(144,MAG_CREATE,MAG_ENERGY)));
	case 112 : /* Magic Rocket */
		return MIN(L(360,MAG_DESTROY,MAG_STUFF),
				MAX(L(160,MAG_CREATE,MAG_MATTER),L(160,MAG_CREATE,MAG_ENERGY)));
	case 113 : /* Sonic Boom */
		return MIN(L(225,MAG_DESTROY,MAG_STUFF),
				MIN(L(126,MAG_CREATE,MAG_ENERGY),L(170,MAG_CONTROL,MAG_MATTER)));
	case 114 : /* Chaos Bolt */
		return MIN(L(200,MAG_DESTROY,MAG_STUFF),L(200,MAG_DESTROY,MAG_REAL));
	case 115 : /* Chaos Spread */
		return L(360,MAG_DESTROY,MAG_STUFF);
	case 116 : /* Breathe Chaos */
		return L(432,MAG_DESTROY,MAG_STUFF);
	case 117 : /* Call Chaos */
		return L(369,MAG_DESTROY,MAG_STUFF);
	case 118 : /* Fist of Force */
		return MIN(L(135,MAG_DESTROY,MAG_STUFF),L(70,MAG_CONTROL,MAG_MATTER));
	case 119 : /* Disintegration */
		return MIN(L(207,MAG_DESTROY,MAG_STUFF),L(150,MAG_DESTROY,MAG_MATTER));
	case 120 : /* Beam of Gravity */
		return MIN(L(207,MAG_DESTROY,MAG_STUFF),L(100,MAG_CONTROL,MAG_MATTER));
	case 121 : /* Gravitic Wave */
		return L(60,MAG_CONTROL,MAG_MATTER);
	case 122 : /* Doom Bolt */
		return MIN(L(252,MAG_DESTROY,MAG_STUFF),L(138,MAG_CREATE,MAG_STUFF));
	case 123 : /* Darkness Bolt */
		return L(42,MAG_DESTROY,MAG_ENERGY);
	case 124 : /* Darkness Storm */
		return L(240,MAG_DESTROY,MAG_ENERGY);
	case 125 : /* Nether Bolt */
		return L(72,MAG_CREATE,MAG_STUFF);
	case 126 : /* Nether Ball */
		return L(144,MAG_CREATE,MAG_STUFF);
	case 127 : /* Project Void */
		return L(60,MAG_DESTROY,MAG_REAL);
	case 128 : /* Call the Void */
		return L(258,MAG_DESTROY,MAG_REAL);
	case 129 : /* Breathe Power */
		return L(210,MAG_CREATE,MAG_ENERGY);
	case 130 : /* Power */
		return MIN(L(441,MAG_DESTROY,MAG_STUFF),L(294,MAG_CREATE,MAG_STUFF));
	case 131 : /* Death Ray */
		return L(100,MAG_DESTROY,MAG_LIFE);
	case 132 : /* Wonder */
		return L(171,MAG_DESTROY,MAG_STUFF);
	case 133 : /* Invoke Spirits */
		return L(50,MAG_CONTROL,MAG_STUFF);
	case 134 : /* Vampiric Drain */
		return L(138,MAG_CONTROL,MAG_LIFE);
	case 135 : /* Drain Life */
		return L(264,MAG_CONTROL,MAG_LIFE);
	case 136 : /* Exorcism */
		return L(105,MAG_CREATE,MAG_LIFE);
	case 137 : /* Dispel Undead and Demons */
		return L(120,MAG_CREATE,MAG_LIFE);
	case 138 : /* Dispel Evil */
		return L(175,MAG_CREATE,MAG_LIFE);
	case 139 : /* Word of Death */
		return L(165,MAG_DESTROY,MAG_LIFE);
	case 140 : /* Evocation */
		return L(185,MAG_DESTROY,MAG_LIFE);
	case 141 : /* Purge */
		return L(154,MAG_CHANGE,MAG_REAL);
	case 142 : /* Mass Purge */
		return L(160,MAG_CHANGE,MAG_REAL);
	case 143 : /* Omnicide */
		return /*MIN(L(225,MAG_DESTROY,MAG_LIFE),L(225,MAG_DESTROY,MAG_BODY))*/9999;
	case 144 : /* Polymorph Other */
		return MIN(L(99,MAG_DESTROY,MAG_STUFF),L(44,MAG_CHANGE,MAG_BODY));
	case 145 : /* Charm Animal */
		return L(25,MAG_CONTROL,MAG_NATURE);
	case 146 : /* Enslave the Undead */
		return L(50,MAG_CONTROL,MAG_STUFF);
	case 147 : /* Charm Monster */
		return MIN(MAX(L(80,MAG_CREATE,MAG_LIFE),L(55,MAG_CONTROL,MAG_LIFE)),
				MIN(L(30,MAG_CONTROL,MAG_MIND),L(60,MAG_CHANGE,MAG_MIND)));
	case 148 : /* Animal Friendship */
		return L(165,MAG_CONTROL,MAG_NATURE);
	case 149 : /* Day of the Dove */
		return MIN(MAX(L(170,MAG_CREATE,MAG_LIFE),L(120,MAG_CONTROL,MAG_LIFE)),
				MIN(L(80,MAG_CONTROL,MAG_MIND),L(160,MAG_CHANGE,MAG_MIND)));
	case 150 : /* Summon Monster */
		return MAX(MAX(MIN(L(210,MAG_CONTROL,MAG_MIND),MAX(L(135,MAG_CONTROL,MAG_MIND),
							L(210,MAG_CONTROL,MAG_LIFE))),L(90,MAG_CHANGE,MAG_BODY)),
			L(90,MAG_DIVINE,MAG_MIND));
	case 151 : /* Phantasmal Servant */
		return MAX(MIN(L(196,MAG_CONTROL,MAG_MIND),MAX(L(126,MAG_CONTROL,MAG_MIND),
						L(196,MAG_CONTROL,MAG_LIFE))),L(84,MAG_CREATE,MAG_STUFF));
	case 152 : /* Conjure Elemental */
		return MAX(MIN(L(232,MAG_CONTROL,MAG_MIND),MAX(L(150,MAG_CONTROL,MAG_MIND),
						L(232,MAG_CONTROL,MAG_LIFE))),L(99,MAG_CREATE,MAG_MAGIC));
	case 153 : /* Summon Spiders */
		return MAX(MAX(MIN(L(168,MAG_CONTROL,MAG_MIND),MAX(L(108,MAG_CONTROL,MAG_MIND),
							L(168,MAG_CONTROL,MAG_LIFE))),L(72,MAG_CHANGE,MAG_BODY)),
			L(72,MAG_DIVINE,MAG_MIND));
	case 154 : /* Summon Hydrae */
		return MAX(MAX(MIN(L(182,MAG_CONTROL,MAG_MIND),MAX(L(117,MAG_CONTROL,MAG_MIND),
							L(182,MAG_CONTROL,MAG_LIFE))),L(78,MAG_CHANGE,MAG_BODY)),
			L(78,MAG_DIVINE,MAG_MIND));
	case 155 : /* Summon Hounds */
		return MAX(MAX(MIN(L(210,MAG_CONTROL,MAG_MIND),MAX(L(135,MAG_CONTROL,MAG_MIND),
							L(210,MAG_CONTROL,MAG_LIFE))),L(90,MAG_CHANGE,MAG_BODY)),
			L(90,MAG_DIVINE,MAG_MIND));
	case 156 : /* Summon Animal */
		return MAX(MAX(MIN(L(168,MAG_CONTROL,MAG_MIND),MAX(L(108,MAG_CONTROL,MAG_MIND),
							L(168,MAG_CONTROL,MAG_LIFE))),L(72,MAG_CHANGE,MAG_BODY)),
			L(72,MAG_DIVINE,MAG_MIND));
	case 157 : /* Summon Animals */
		return MAX(L(105,MAG_CHANGE,MAG_NATURE),L(105,MAG_CONTROL,MAG_NATURE));
	case 158 : /* Summon Demon */
		return MIN(L(423,MAG_DESTROY,MAG_STUFF),L(235,MAG_CONTROL,MAG_STUFF));
	case 159 : /* Summon Greater Demon */
		return MAX(MAX(MIN(L(329,MAG_CONTROL,MAG_MIND),MAX(L(212,MAG_CONTROL,MAG_MIND),
							L(329,MAG_CONTROL,MAG_LIFE))),L(141,MAG_CHANGE,MAG_BODY)),
			L(141,MAG_DIVINE,MAG_STUFF));
	case 160 : /* Summon Undead */
		return MAX(MAX(MIN(L(252,MAG_CONTROL,MAG_MIND),MAX(L(162,MAG_CONTROL,MAG_MIND),
							L(252,MAG_CONTROL,MAG_LIFE))),L(108,MAG_CHANGE,MAG_BODY)),
			L(108,MAG_DIVINE,MAG_STUFF));
	case 161 : /* Summon Greater Undead */
		return MAX(MAX(MIN(L(343,MAG_CONTROL,MAG_MIND),MAX(L(220,MAG_CONTROL,MAG_MIND),
							L(343,MAG_CONTROL,MAG_LIFE))),L(147,MAG_CHANGE,MAG_BODY)),
			L(147,MAG_DIVINE,MAG_STUFF));
	case 162 : /* Summon Dragon */
		return MAX(MAX(MIN(L(273,MAG_CONTROL,MAG_MIND),MAX(L(176,MAG_CONTROL,MAG_MIND),
							L(273,MAG_CONTROL,MAG_LIFE))),L(117,MAG_CHANGE,MAG_BODY)),
			L(117,MAG_DIVINE,MAG_MIND));
	case 163 : /* Summon Ancient Dragon */
		return MAX(MAX(MIN(L(336,MAG_CONTROL,MAG_MIND),MAX(L(200,MAG_CONTROL,MAG_MIND),
							L(336,MAG_CONTROL,MAG_LIFE))),L(144,MAG_CHANGE,MAG_BODY)),
			L(144,MAG_DIVINE,MAG_MIND));
	case 164 : /* Summon Cyberdemon */
		return MAX(MAX(MIN(L(315,MAG_CONTROL,MAG_MIND),MAX(L(200,MAG_CONTROL,MAG_MIND),
							L(315,MAG_CONTROL,MAG_LIFE))),L(135,MAG_CHANGE,MAG_BODY)),
			L(135,MAG_DIVINE,MAG_MATTER));
	case 165 : /* Strange Summoning */
		return L(60,MAG_CREATE,MAG_BODY);
	case 166 : /* Mass Summoning */
		return MAX(MAX(MIN(L(294,MAG_CONTROL,MAG_MIND),MAX(L(189,MAG_CONTROL,MAG_MIND),
							L(294,MAG_CONTROL,MAG_LIFE))),L(126,MAG_CHANGE,MAG_BODY)),
			L(126,MAG_DIVINE,MAG_MIND));
	case 167 : /* Raise the Dead */
		return L(125,MAG_CHANGE,MAG_LIFE);
	case 168 : /* Banishment */
		return L(210,MAG_CHANGE,MAG_BODY);
	case 169 : /* Banish Evil */
		return MAX(L(175,MAG_CREATE,MAG_LIFE),L(125,MAG_CHANGE,MAG_LIFE));
	case 170 : /* Holy Word */
		return MAX(L(275,MAG_CREATE,MAG_LIFE),L(195,MAG_CHANGE,MAG_LIFE));
	case 171 : /* Divine Intervention */
		return MAX(L(300,MAG_CREATE,MAG_LIFE),L(210,MAG_CHANGE,MAG_LIFE));
	case 172 : /* Nature's Wrath */
		return MAX(L(200,MAG_CONTROL,MAG_NATURE),L(200,MAG_CHANGE,MAG_NATURE));
	case 173 : /* Recharging */
		return MIN(L(160,MAG_DESTROY,MAG_STUFF),L(23,MAG_CREATE,MAG_MAGIC));
	case 174 : /* Bless Weapon */
		return MAX(L(210,MAG_CREATE,MAG_LIFE),L(150,MAG_CHANGE,MAG_LIFE));
	case 175 : /* Enchant Weapon */
		return L(120,MAG_CREATE,MAG_MAGIC);
	case 176 : /* Enchant Armour */
		return L(120,MAG_CREATE,MAG_MAGIC);
	case 177 : /* Elemental Brand */
		return MIN(L(195,MAG_CREATE,MAG_NATURE),L(100,MAG_CREATE,MAG_MAGIC));
	case 178 : /* Poison Branding */
		return L(150,MAG_CREATE,MAG_MAGIC);
	case 179 : /* Chaos Branding */
		return L(405,MAG_DESTROY,MAG_STUFF);
	case 180 : /* Vampiric Branding */
		return L(165,MAG_CREATE,MAG_MAGIC);
	case 181 : /* Trump Branding */
		return L(175,MAG_CREATE,MAG_MAGIC);
	case 182 : /* Teleport Away */
		return L(54,MAG_CHANGE,MAG_BODY);
	case 183 : /* Phase Door */
		return L(3,MAG_CHANGE,MAG_BODY);
	case 184 : /* Teleport */
		return L(15,MAG_CHANGE,MAG_BODY);
	case 185 : /* Teleport Level */
		return L(60,MAG_CHANGE,MAG_BODY);
	case 186 : /* Dimension Door */
		return L(36,MAG_CHANGE,MAG_BODY);
	case 187 : /* Word of Recall */
		return L(75,MAG_CHANGE,MAG_BODY);
	case 188 : /* Reset Recall */
		return L(18,MAG_CHANGE,MAG_MAGIC);
	case 189 : /* Free Path */
		return MIN(L(36,MAG_DESTROY,MAG_STUFF),L(6,MAG_DESTROY,MAG_MATTER));
	case 190 : /* Destroy Barriers */
		return MIN(L(36,MAG_DESTROY,MAG_STUFF),L(6,MAG_DESTROY,MAG_MATTER));
	case 191 : /* Telekinesis */
		return L(75,MAG_CONTROL,MAG_MATTER);
	case 192 : /* Alchemy */
		return L(126,MAG_CHANGE,MAG_MATTER);
	case 193 : /* Stone to Mud */
		return MIN(L(35,MAG_CHANGE,MAG_NATURE),L(20,MAG_CHANGE,MAG_MATTER));
	case 194 : /* Door Building */
		return L(30,MAG_CREATE,MAG_MATTER);
	case 195 : /* Stair Building */
		return L(36,MAG_CREATE,MAG_MATTER);
	case 196 : /* Wall of Stone */
		return MIN(L(160,MAG_CREATE,MAG_NATURE),L(85,MAG_CREATE,MAG_MATTER));
	case 197 : /* Create Rock */
		return MIN(L(190,MAG_CREATE,MAG_NATURE),L(100,MAG_CREATE,MAG_MATTER));
	case 198 : /* Earthquake */
		return MIN(MIN(L(138,MAG_CREATE,MAG_ENERGY),L(96,MAG_CONTROL,MAG_MATTER)),
				L(115,MAG_CREATE,MAG_NATURE));
	case 199 : /* Word of Destruction */
		return MIN(L(324,MAG_DESTROY,MAG_STUFF),L(180,MAG_DESTROY,MAG_MATTER));
	case 200 : /* Whirlwind Attack */
		return MIN(L(120,MAG_CONTROL,MAG_NATURE),L(80,MAG_CONTROL,MAG_BODY));
	case 201 : /* Alter Reality */
		return MIN(L(270,MAG_DESTROY,MAG_STUFF),L(150,MAG_CHANGE,MAG_REAL));
	case 202 : /* Polymorph Self */
		return MIN(L(378,MAG_DESTROY,MAG_STUFF),L(210,MAG_CHANGE,MAG_BODY));
	case 203 : /* Lock */
		return L(3,MAG_CHANGE,MAG_MATTER);
	case 204 : /* Phlogiston */
		return L(15,MAG_CREATE,MAG_ENERGY);
	default:
		msg_print("Warning - illegal spell called");
		return 9999;
	}
	/* Should never get here */
	/*return 9999;*/
}

/* 
 * Decides whether a particular god is willing to grant a particular spell
 */
static bool spell_god_ok(int spell, int god)
{
	switch (spell)
	{
	case 44 : /* Haste */
	case 45 : /* Berserk Strength */
	case 46 : /* Battle Frenzy */
	case 55 : /* Stoneskin */
	case 57 : /* Wraithform */
	case 72 : /* Terror */
	case 73 : /* Horrify */
	case 81 : /* Malediction */
	case 82 : /* Orb of Entropy */
	case 108 : /* Stinking Cloud */
	case 123 : /* Darkness Bolt */
	case 124 : /* Darkness Storm */
	case 125 : /* Nether Bolt */
	case 126 : /* Nether Ball */
	case 131 : /* Death Ray */
	case 133 : /* Invoke Spirits */
	case 134 : /* Vampiric Drain */
	case 135 : /* Drain Life */
	case 139 : /* Word of Death */
	case 140 : /* Evocation */
	case 144 : /* Polymorph Other */
	case 146 : /* Enslave the Undead */
	case 167 : /* Raise the Dead */
	case 178 : /* Poison Branding */
	case 179 : /* Chaos Branding */
	case 180 : /* Vampiric Branding */
	case 181 : /* Trump Branding */
	case 202 : /* Polymorph Self */
		return FALSE;
	}
	return TRUE;

}

/* 
 * Calculate the level at which a spell can be cast.
 * Depends mostly on the player's proficiency with the relevant spell areas.
 * Return something >50 if it can't be cast.
 */
int calculate_spell_level(int spell) 
{
	int level;
	
	/* Priests have certain restrictions */
	if (p_ptr->pclass == CLASS_PRIEST || p_ptr->pclass == CLASS_PALADIN)
	{
		/* Does the god disallow it? */
		if (!spell_god_ok(spell, p_ptr->chaos_patron)) return 20000;

		/* Is it mage-type? */
		if (spell_stats[spell][6] == 2) return 20000;
	}

	/* Get the base level */
	level = calculate_spell_level_aux(spell);

	/* Some results are bad */
	if (level < 0) return 20000;
	if (!level) level = 1;

	/* Mages are bad at priest-type spells */
	if (!spell_stats[spell][6] && !(p_ptr->pclass == CLASS_PRIEST || p_ptr->pclass == CLASS_PALADIN))
	{
		level = MAX(level + 1, (level * 4) / 3);
	}

	/* Adjust by first spell level */
	level += (mp_ptr->spell_first - 1);
	
	/* If we're good at it, we won't be THAT good at it */
	if (level < spell_stats[spell][7])
	{
		level = MIN(((5 * spell_stats[spell][7]) + level) / 6, spell_stats[spell][7] - 1);
	}

	/* There may be a minimum level */
	return MAX(spell_stats[spell][5], level);
}

/*
 * Calculates the amount of mana needed to cast a spell
 */
int calculate_mana(int spell)
{
	int level = calculate_spell_level(spell);
	if (level < spell_stats[spell][7])
	{
		int lowcall = calculate_spell_level_aux(spell);
		if (!lowcall) lowcall = 1;
		return ((spell_stats[spell][7] + (4 * lowcall)) * spell_stats[spell][0]) / 500;
	}
	else return (level * spell_stats[spell][0]) / 100;
}
	
/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 * If ever is true, we return whether we can ever cast it.
 */
bool spell_okay_new(int spell, bool known, bool ever)
{
	int level = calculate_spell_level(spell);

	/* Spell may at some point be cast? */
	if (ever) return (level > 50 ? FALSE : TRUE);

	/* Spell is illegal */
	if (level > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_forgotten[spell / 32] & (1L << (spell % 32)))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_learned[spell / 32] & (1L << (spell % 32)))
	{
		/* Okay to cast, not to study */
		return (known);
	}

	/* Have we found it yet? */
	if (!(p_ptr->spell_found[spell / 32] & (1L << (spell % 32)))) return FALSE;

	/* Okay to study, not to cast */
	return (!known);
}



#define SPELL_SELECTION_ROW	8

/* 
 * Try to delete something in a list of choices
 */
static bool delete_list_hook(int i)
{
	/* Only sufficiently small positions give a deleteable spell list */
	if (i < spell_list_num - 1)
	{
		spell_list_wipe((s16b) i + 1);
		return TRUE;
	}
	else
	{
		/* The user is being very silly, so let's punish him */
		bell();
		return FALSE;
	}
}

/* Number of entries which are lists of spells, rather than individual ones */
static int num_lists = 0;

/*
 * Display additional information about spells.
 * Returns an index indicating how intrinsically good we are at it.
 * 0 indicates not a spell.
 * 1 is good, 2 is normal, 3-5 are progressively bad.
 */
int spell_ability_rating(int spell)
{
	/* Paranoia - is it a spell? */
	if (spell <= 0 || spell >= MAX_SPELLS_CURRENT) return 0;
	/* Level at which we can cast */
	int level = calculate_spell_level(spell);

	/* Normal level to cast it */
	int normal = spell_stats[spell][7];

	if (level == normal) 		return 2;
	if (level <  normal) 		return 1;
	if (level <= normal * 7 / 4)	return 3;
	if (level <= normal * 3)	return 4;
					return 5;
}

/*
 * Display additional information about spells.
 */
static void spell_list_hook(int position, cptr c_str)
{
	s16b 	i;
	int 	spell;
	int 	num;
	char 	buf[128];
	char 	anotherbuf[30];
	byte 	colour;
	cptr	skill_desc;
	

	/* Delete the detritus of any previous hook */
	Term_erase(0, SPELL_SELECTION_ROW-3, Term->wid);
	Term_erase(0, SPELL_SELECTION_ROW-2, Term->wid);
	for (i = 0; i < MAX_SPELLS_IN_BOOK + 2; ++i)
	{
		Term_erase(50, SPELL_SELECTION_ROW+6+i, Term->wid-50);
	}
	
	/* Is it a spell? */
	if (position >= num_lists)
	{
		/* 
		 * Extract the proper class index from the string. 
		 */

		/* We may need to remove brackets */
		strcpy(buf, c_str);
		if (buf[0] == '{') buf[strlen(buf) - 1] = '\0';
			
		for (spell = 1; spell < MAX_SPELLS_CURRENT; ++spell)
		{
			if (!strcmp((buf[0] == '{' ? buf+1 : buf), new_spell_name[spell])) break;
		}

		/* Did we get something */
		if (spell == MAX_SPELLS_CURRENT) return;

		/* Choose a colour */
		switch (spell_ability_rating(spell))
		{
			case 0: colour = TERM_WHITE; 	skill_desc="";		break;
			case 1: colour = TERM_L_BLUE; 	skill_desc="excellent";	break;
			case 2: colour = TERM_L_GREEN; 	skill_desc="good";	break;
			case 3: colour = TERM_YELLOW; 	skill_desc="fair";	break;
			case 4: colour = TERM_ORANGE; 	skill_desc="poor";	break;
			case 5:	colour = TERM_L_RED; 	skill_desc="very poor";	break;
			/* Paranoia */
			default: colour = TERM_WHITE; 	skill_desc="error";	break;
		}
		
		/* Display the details */
		sprintf(buf, "Affinity: %s", skill_desc);
		Term_putstr(0, SPELL_SELECTION_ROW-3, -1, colour, buf);
		i = strlen(buf);
		spell_info_new(anotherbuf, spell);
		sprintf(buf, ", Level: %d, Mana: %d, Fail: %d%%, %s", calculate_spell_level(spell),
				calculate_mana(spell), spell_chance_new(spell), anotherbuf);
		Term_putstr(i, SPELL_SELECTION_ROW-3, -1, TERM_WHITE, buf);
		Term_putstr(0, SPELL_SELECTION_ROW-2, -1, TERM_WHITE, new_spell_desc[spell]);
		return;
	}
	else
	/* It's a list, so list them */
	{
		cptr string;
		
		/* If it's a fixed list, we've already got it */
		if (position < spell_list_num - 1)
		{
			i = position + 1;
			string = spell_list_string(-i);
		}
		/* If it's from a book, we need to find it */
		else
		{
			i = inventory[A2I(c_str[15])].spell_list;
			string = quarky_str(i);
		}
	
		/* Check how many spells we have */
		num = string ? strlen(string) / 2 : 0;

		/* Is it empty? */
		if (!num)
		{
			msg_print("Error! Empty spell list listed.");
			return;
		}
	
		/* Sanity check */
		if (num < 0 || num > MAX_SPELLS_IN_BOOK)
		{
			msg_print("Bad number of spells in spell list");
			return;
		}
	
		/* Print spells */
		for (spell = 0; spell < num; ++spell)
		{
			Term_putstr(50, SPELL_SELECTION_ROW+6+spell, -1, TERM_WHITE,
					new_spell_name[spell_number(string[spell * 2],string[(spell * 2) + 1])]);
		}
		if (position < spell_list_num - 1)
		{
			Term_putstr(50, SPELL_SELECTION_ROW+6+num+1, -1, TERM_WHITE, 
					"'-' to delete list");
		}
		return;
	}		
}

/*
 * Choose a spell from the list of all spells,
 * pruned to only contain acceptable choices.
 *
 * known should be TRUE if we only want to include spells
 * which are currently castable, FALSE otherwise
 *
 * lists is true if we want to be able to choose spell lists
 * as well as individual spells
 */
int get_spell_from_grand_list(int *sn, cptr prompt, bool known, bool lists, 
		bool *is_list, int do_with_written)
{
	int 		spell_nums[MAX_SPELLS_CURRENT];
	cptr 		spell_name[MAX_SPELLS_CURRENT];
	cptr 		string;
	object_type	*o_ptr;
	int 		num;
	int 		num_fixed_lists;
	char		buf[80];
	bool		written_spells[MAX_SPELLS_CURRENT];
	
	int 		result;
	
	int 		i, spell;

	/* We may want to repeat */
	result = -2;
	while (result == -2)
	{
		/* We may want to exclude written spells */
		if (do_with_written)
		{
			/* Assume spells are not written */
			for (i=1; i<MAX_SPELLS_CURRENT; ++i)
				written_spells[i] = 0;

			/* Scan the inventory for spellbooks */
			for (i = 0; i < INVEN_PACK; i++)
			{
				o_ptr = &inventory[i];
		
				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Need a spellbook with spells */
				if (o_ptr->tval == TV_SPELLBOOK && o_ptr->spell_list)
				{
					/* Extract the spells */
					string = quarky_str(o_ptr->spell_list);

					/* Check how many spells we have */
					num = string ? strlen(string) / 2 : 0;
	
					/* Sanity check */
					if (num <= 0 || num > MAX_SPELLS_IN_BOOK)
					{
						msg_print("Bad number of spells in spell book");
						return FALSE;
					}
	
					/* Get the spells */
					for (spell = 0; spell < num; ++spell)
					{
						written_spells[spell_number(string[spell * 2],
								string[(spell * 2) + 1])] = 1;
					}
				}
			}
		}
		
		/* No possibilities yet */
		num = 0;
		num_fixed_lists = 0;
		num_lists = 0;
		
		if (lists)
		{
			/* Add all available permanent spell lists */
			for (i=1; i<spell_list_num; ++i)
			{
				spell_name[num] = spell_list_name[i];
				spell_nums[num] = -i;
				++num;
			}
	
			/* Record position */
			num_fixed_lists = num;
			
			/* Add any spell lists in currently held books */
			for (i = 0; i < INVEN_PACK; i++)
			{
				o_ptr = &inventory[i];
		
				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				if (o_ptr->tval == TV_SPELLBOOK && o_ptr->spell_list)
				{
					/* Build a descriptive string */
					if (!o_ptr->inscription)
						sprintf(buf, "Spells in book %c)", I2A(i));
					else
						snprintf(buf, 80, "Spells in book %c) {%s}", I2A(i), 
								quark_str(o_ptr->inscription));

					/* Assign it */
					spell_name[num] = string_make(buf);
					spell_nums[num] = o_ptr->spell_list;
					++num;
				}
			}

			/* That's the number of lists */
			num_lists = num;
		}

		/* Extract all the possible spells */
		for (i=1; i<MAX_SPELLS_CURRENT; ++i)
		{
			/* is it viable? */
			if (spell_okay_new(i, known, FALSE) &&
					(do_with_written != 2 || written_spells[i] == 0))
			{
				/* Add it to the list */
				if (do_with_written != 1 || written_spells[i] == 0)
				{
					spell_name[num] = new_spell_name[i];
				}
				else
				{
					snprintf(buf, 80, "{%s}", new_spell_name[i]);
					spell_name[num] = string_make(buf);
				}
				spell_nums[num] = i;
				++num;
			}
		}

		/* If we have no spells, we can't get a choice */
		if (!num) return FALSE;

		/* Prepare */
		screen_save();
		Term_clear();

		/* Build a prompt */
		sprintf(buf, "(RETURN=select,ESC=exit) %s which spell? ", prompt);

		/* Display */
		Term_putstr(0, 1, 70, TERM_WHITE, buf);
	
		/* Display */
		Term_putstr(0, 2, 70, TERM_WHITE, 
				"Type the first few letters of a spell to search, or use the arrow keys");
		Term_putstr(0, 3, 70, TERM_WHITE, 
				"to scroll. Press <ENTER> to select the highlighted spell.");
	
		/* Ask for a spell */
		result = get_player_sort_choice_long(num_lists, spell_name, num, 0, 50, 
					SPELL_SELECTION_ROW, NULL, FALSE, 
					spell_list_hook, delete_list_hook);

		/* Housekeeping */
		for (i=num_fixed_lists; i<num_lists; ++i)
		{
			string_free(spell_name[i]);
		}

		if (do_with_written == 1)
		{
			for (i=num_lists; i<num; ++i)
				if (written_spells[spell_nums[i]])
					string_free(spell_name[i]);
		}

		screen_load();
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->redraw |= (PR_EQUIPPY);
		p_ptr->window |= (PW_INVEN);
		handle_stuff();
	}
	
	/* Did the user cancel? */
	if (result == -1) return FALSE;

	/* List? */
	if (lists)
	{
		*is_list = (result < num_lists ? TRUE : FALSE);
	}
	
	/* Save the result */
	*sn = spell_nums[result];

	/* More housekeeping */
	num_lists = 0;

	/* Success */
	return TRUE;
}

/*
 * Extra information on a spell, based on spell_info()
 *
 * We can use up to 14 characters of the buffer 'p'
 */
void spell_info_new(char *p, int spell)
{
	/* Default */
	strcpy(p, "");
		
	int plev = p_ptr->lev;
	
	
	/* See below */
	/* int orb = (plev / ((p_ptr->pclass == CLASS_PRIEST ||
	                    p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4));
				(is generating warnings at the moment) */

	/* Analyze the spell */
	switch (spell)
	{
	case 14 : /* Clairvoyance */
		strcpy (p, " dur 25+d30"); break;
	case 15 : /* Call Sunlight */
		strcpy (p, " dam 75"); break;
	case 16 : /* Telepathy */
		strcpy (p, " dur 25+d30"); break;
	case 28 : /* Cure Light Wounds */
		strcpy (p, " heal 2d10"); break;
	case 29 : /* Cure Normal Wounds */
		strcpy (p, " heal 4d10"); break;
	case 30 : /* Cure Serious Wounds */
		strcpy (p, " heal 8d10"); break;
	case 31 : /* Cure Critical Wounds */
		strcpy (p, " heal 20d10"); break;
	case 32 : /* Healing */
		strcpy (p, " heal 300"); break;
	case 33 : /* Greater Healing */
		strcpy (p, " heal 2000"); break;
	case 41 : /* Bless */
		strcpy (p, " dur 12+d12 turns"); break;
	case 42 : /* Prayer */
		strcpy (p, " dur 48+d48"); break;
	case 43 : /* Heroism */
		strcpy (p, " dur 25+d25"); break;
	case 44 : /* Haste */
		sprintf(p, " dur %d+d%d", plev, plev + 20); break;
	case 45 : /* Berserk Strength */
		strcpy (p, " dur 25+d25"); break;
	case 46 : /* Battle Frenzy */
		strcpy (p, " max dur 50"); break;
	case 47 : /* Protection from Corrosion */
	case 48 : /* Resist Cold */
	case 49 : /* Resist Fire */
	case 50 : /* Resist Lightning */
	case 51 : /* Resist Acid */
	case 52 : /* Resist Poison */
	case 53 : /* Resist Environment */
	case 54 : /* Resistance */
		strcpy (p, " dur 20+d20"); break;
	case 55 : /* Stoneskin */
		strcpy (p, " dur 20+d30"); break;
	case 56 : /* Globe of Invulnerability */
	case 57 : /* Wraithform */
		strcpy (p, " dur 8+d8"); break;
	case 58 : /* Light */
		sprintf(p, " dam %d", 10 + (plev / 2)); break;
	case 59 : /* Spear of Light */
		strcpy (p, " dam 6d8"); break;
	case 61 : /* See Invisible */
		strcpy (p, " dur 24+d24"); break;
	case 64 : /* Explosive Rune */
		sprintf(p, " dam 7d7+%d", plev / 2); break;
	case 75 : /* Protection from Evil */
		sprintf(p, " dur d25+%d", 3 * plev); break;
	case 76 : /* Mind Blast */
		sprintf(p, " dam %dd3", 3 + ((plev - 1) / 5)); break;
	case 77 : /* Magic Missile */
		sprintf(p, " dam %dd3", 3 + ((plev - 1) / 5)); break;
	case 78 : /* Mana Bolt */
		sprintf(p, " dam 75+d%d", (350 + (plev * 3))); break;
	case 79 : /* Mana Storm */
		sprintf(p, " dam %d", 300 + (plev * 2)); break;
	case 80 : /* Hellfire */
		strcpy (p, " dam 666"); break;
	case 81 : /* Malediction */
		sprintf(p, " dam %dd3", 3 + ((plev - 1) / 5)); break;
	case 82 : /* Orb of Entropy */
		sprintf(p, " dam 3d6+%d", plev +
    				(plev / (((p_ptr->pclass == CLASS_MAGE) ||
    				(p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))); break;
	case 83 : /* Orb of Draining */
		sprintf(p, " dam 3d6+%d", plev + (plev / 
					(p_ptr->pclass == CLASS_PRIEST ? 2 : 4))); break;
	case 84 : /* Electrical Charge */
		sprintf(p, " dam %dd4", 3 + ((plev - 1) / 5)); break;
	case 85 : /* Lightning Bolt */
		sprintf(p, " dam %dd8", (3 + ((plev - 5) / 4))); break;
	case 86 : /* Chain Lightning */
		sprintf(p, " dam %dd8", (5 + (plev / 10))); break;
	case 87 : /* Ball Lightning */
		sprintf(p, " dam %d", 15 + plev); break;
	case 88 : /* Lightning Storm */
		sprintf(p, " dam %d", 70 + plev); break;
	case 89 : /* Electrocution */
		sprintf(p, " dam %d", 125 + (2 * plev)); break;
	case 90 : /* Absolute Lightning */
		sprintf(p, " dam %d", 250 + (4 * plev)); break;
	case 91 : /* Freeze */
		sprintf(p, " dam %dd4", 3 + ((plev - 1) / 5)); break;
	case 92 : /* Frost Bolt */
		sprintf(p, " dam %dd8", (5 + ((plev - 5) / 4))); break;
	case 93 : /* Cold Ball */
		sprintf(p, " dam %d", 30 + plev); break;
	case 94 : /* Ice Storm */
		sprintf(p, " dam %d", 70 + plev); break;
	case 95 : /* Absolute Zero */
		sprintf(p, " dam %d", 300 + (6 * plev)); break;
	case 96 : /* Shoot Acid */
		sprintf(p, " dam %dd4", 3 + ((plev - 1) / 5)); break;
	case 97 : /* Acid Bolt */
		sprintf(p, " dam %dd8", (6 + ((plev - 5) / 4))); break;
	case 98 : /* Acid Ball */
		sprintf(p, " dam %d", 40 + plev); break;
	case 99 : /* Mass Corrosion */
		sprintf(p, " dam %d", 90 + plev); break;
	case 100 : /* Absolute Acid */
		sprintf(p, " dam %d", 400 + (7 * plev)); break;
	case 101 : /* Flame */
		sprintf(p, " dam %dd4", 3 + ((plev + 2) / 4)); break;
	case 102 : /* Fire Bolt */
		sprintf(p, " dam %dd8", (8 + ((plev - 5) / 4))); break;
	case 103 : /* Fireball */
		sprintf(p, " dam %d", 55 + plev); break;
	case 104 : /* Firestorm */
		sprintf(p, " dam %d", 150 + (2 * plev)); break;
	case 105 : /* Sunfire */
		sprintf(p, " dam %d", 450 + (9 * plev)); break;
	case 106 : /* Flame Strike */
		sprintf(p, " dam %d", 75 + plev); break;
	case 107 : /* Meteor Swarm */
		sprintf(p, " dam %d each", (3 * plev) / 2); break;
	case 108 : /* Stinking Cloud */
		sprintf(p, " dam %d", 8 + (plev / 2)); break;
	case 109 : /* Cloudkill */
		sprintf(p, " dam %d", 40 + plev); break;
	case 110 : /* Absolute Poison */
		sprintf(p, " dam %d", 150 + (3 * plev)); break;
	case 111 : /* Whirlpool */
		sprintf(p, " dam %d", 100 + plev); break;
	case 112 : /* Magic Rocket */
		sprintf(p, " dam %d", 120 + (2 * plev)); break;
	case 113 : /* Sonic Boom */
		sprintf(p, " dam %d", 45 + plev); break;
	case 114 : /* Chaos Bolt */
		sprintf(p, " dam %dd8", (10 + ((plev - 5) / 4))); break;
	case 115 : /* Chaos Spread */
		sprintf(p, " dam %d", 66 + plev); break;
	case 116 : /* Breathe Chaos */
		sprintf(p, " dam %d", p_ptr->chp); break;
	case 117 : /* Call Chaos */
		strcpy (p, " dam 75 / 150"); break;
	case 118 : /* Fist of Force */
		sprintf(p, " dam %dd8", (8 + ((plev - 5) / 4))); break;
	case 119 : /* Disintegration */
		sprintf(p, " dam %d", 80 + plev); break;
	case 120 : /* Beam of Gravity */
		sprintf(p, " dam %dd8", (9 + (plev / 10))); break;
	case 121 : /* Gravitic Wave */
		sprintf(p, " dam %d", (25 + (plev / 2)) / 2); break;
	case 122 : /* Doom Bolt */
		sprintf(p, " dam %dd8", (11 + ((plev - 5) / 4))); break;
	case 123 : /* Darkness Bolt */
		sprintf(p, " dam %dd8", (4 + ((plev - 5) / 4))); break;
	case 124 : /* Darkness Storm */
		sprintf(p, " dam %d", 150 + (2 * plev)); break;
	case 125 : /* Nether Bolt */
		sprintf(p, " dam %dd8", (7 + ((plev - 5) / 4))); break;
	case 126 : /* Nether Ball */
		sprintf(p, " dam %d", 50 + plev); break;
	case 127 : /* Project Void */
		sprintf(p, " dam %dd8", (5 + ((plev - 5) / 4))); break;
	case 128 : /* Call the Void */
		sprintf(p, " dam %d", 250 + (2 * plev)); break;
	case 129 : /* Breathe Power */
		strcpy (p, " random"); break;
	case 130 : /* Power */
		strcpy (p, " dam 3 * 175"); break;
	case 132 : /* Wonder */
	case 133 : /* Invoke Spirits */
		strcpy (p, " random"); break;
	case 134 : /* Vampiric Drain */
		sprintf(p, " dm %d* 5+d15", 2 + (plev / 15)); break;
	case 135 : /* Drain Life */
		strcpy (p, " dam 3*100"); break;
	case 136 : /* Exorcism */
		sprintf(p, " dam %d+%d", plev, plev); break;
	case 137 : /* Dispel Undead and Demons */
		sprintf(p, " dam %d+%d", 3 * plev, 3 * plev); break;
	case 138 : /* Dispel Evil */
		sprintf(p, " dam %d", 4 * plev); break;
	case 139 : /* Word of Death */
		sprintf(p, " dam %d", plev * 3); break;
	case 140 : /* Evocation */
		sprintf(p, " dam %d", plev * 4); break;
	case 170 : /* Holy Word */
		sprintf(p, " d %d/h 1000", 4 * plev); break;
	case 171 : /* Divine Intervention */
		sprintf(p, " h300/d%d+388", plev * 4); break;
	case 172 : /* Nature's Wrath */
		sprintf(p, " dam %d+%d", 4 * plev, 100 + plev); break;
	case 183 : /* Phase Door */
		strcpy (p, " range 10"); break;
	case 184 : /* Teleport */
		sprintf(p, " range %d", plev * 5); break;
	case 186 : /* Dimension Door */
		sprintf(p, " range %d", plev + 2); break;
	case 187 : /* Word of Recall */
		strcpy (p, " delay 15+d21"); break;
	case 191 : /* Telekinesis */
		sprintf(p, " max wgt %d", plev * 15 / 10); break;
	case 198 : /* Earthquake */
		strcpy (p, " rad 10"); break;
	}
}

/*
 * Calculate the percentage chance of a spell failing.
 * Based on spell_chance()
 */
s16b spell_chance_new(int spell) 
{
	int chance, minfail, level;

	level = calculate_spell_level(spell);
	
	/* Extract the base spell failure rate */
	chance = spell_stats[spell][1];
	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - level);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

	/* We may be good at the spell */
	if (level < spell_stats[spell][7]) chance -= 
		((20 * (spell_stats[spell][7] - calculate_spell_level_aux(spell)))
		 / spell_stats[spell][7]);
	
	/* Not enough mana to cast */
	if (calculate_mana(spell) > p_ptr->csp)
	{
		chance += 5 * (calculate_mana(spell) - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

	/*
	 * Non mage/priest characters never get too good
	 * (added high mage, mindcrafter)
	 */
	if ((p_ptr->pclass != CLASS_PRIEST) &&
	    (p_ptr->pclass != CLASS_MAGE) &&
	    (p_ptr->pclass != CLASS_MINDCRAFTER))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Hack -- Priest prayer penalty for doing bad things */
	if ((p_ptr->pclass == CLASS_PRIEST) && p_ptr->icky_wield) chance += 25;

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Permanently acquire the potential use of a spell
 * from a spell scroll. Destroy the scroll.
 */
bool do_cmd_record_magic() 
{ 
	int item;

	int i,j;

	object_type *o_ptr;

	cptr q, s;

	int spell;
	
	/* Restrict choices to spell scrolls */
	item_tester_tval = TV_SPELL_SCROLL;

	/* Get an item */
	q = "Record from which spell scroll? ";
	s = "You have no spell scrolls.";
	if (!get_item(&item, q, s, USE_INVEN)) return FALSE;

	/* Get the item */
	o_ptr = &inventory[item];

	/* Extract the spell */
	spell = o_ptr->pval;

	/* Check whether we can ever cst the spell */
	if (!spell_okay_new(spell, FALSE, TRUE))
	{
		msg_print("You don't understand the spell.");
		return FALSE;
	}

	/* Calculate where the spell flag is */
	i = spell / 32;
	j = 1L << (spell % 32);
	
	/* Check whether we've already got it */
	if (p_ptr->spell_found[i] & j)
	{
		msg_print("You already have the spell recorded.");
		return FALSE;
	}

	/* Okay, we're doing it now. Record the spell */
	p_ptr->spell_found[i] |= j;

	/* Lose the scroll */
	inven_item_increase(item, -1);
	inven_item_optimize(item);

	/* Report */
	msg_format("You record the spell: %s", new_spell_name[spell]);
	
	p_ptr->redraw |= (PR_EQUIPPY);

	/* Success */
	return TRUE;
}


static void create_spell_list_aux(object_type *o_ptr, s16b list_index, s16b original_list)
{
	/* Skip dead objects */
	if (!o_ptr->k_idx) return;

	/* Skip non-spellbooks */
	if (o_ptr->tval != TV_SPELLBOOK) return;

	/* Is the spellbook affected? */
	if (o_ptr->spell_list == original_list)
	{
		o_ptr->spell_list = list_index;
	}
}

/*
 * Create a "permanent spell list" for easy access
 */
bool do_cmd_create_spell_list()
{
	int 		item;
	s16b		list_index;

	object_type 	*o_ptr;
	s16b		original_list;

	int 		i,j,k;
	cptr 		q, s;


	/* Choose the book to get a spell list from */
	item_tester_tval = TV_SPELLBOOK;

	/* Get an item */
	q = "Use the list in which book? ";
	s = "You have no spellbooks.";
	if (!get_item(&item, q, s, USE_INVEN)) return FALSE;

	/* Get the item */
	o_ptr = &inventory[item];

	/* Extract the list */
	original_list = o_ptr->spell_list;
	
	/* Check that it has a spell list */
	if (!original_list)
	{
		msg_print("That book has no spells in it!");
		return FALSE;
	}

	/* Check that it's not already a spell list */
	if (original_list < 0)
	{
		msg_format("This book already uses the spell list: %s",
				spell_list_name[-original_list]);
		return FALSE;
	}

	/* Add the spell list */
	list_index = spell_list_add(quark_str(original_list));

	/* Did we succeed? */
	if (!list_index) return FALSE;
	
	/* Any spellbooks with this spell list now use it */
	for (i = 1; i < town_count; i++)
	for (j = 0; j < town[i].numstores; j++)
	for (k = 0; k < town[i].store[j].stock_num; k++)
		create_spell_list_aux(&(town[i].store[j].stock[k]), list_index, original_list);
	for (i = 0; i < INVEN_PACK; i++)
		create_spell_list_aux(&inventory[i], list_index, original_list);
	for (i = 1; i < o_max; i++)
		create_spell_list_aux(&o_list[i], list_index, original_list);

	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->redraw |= (PR_EQUIPPY);
	p_ptr->window |= (PW_INVEN);
	handle_stuff();
	
	/* Success */
	return TRUE;
}

static u16b save_spell_list(cptr string, u16b position)
{
	int i;
	
	/* Look for a spell list */
	for (i = 1; i < spell_list_num; i++)
	{
		/* Check for equality */
		if (streq(spell_list_str[i], string)) return (-i);
	}

	/* OK, we need a quark */
	return quark_create_single(string, position);
}

/*
 * Write a spell into a spellbook
 */
bool do_cmd_write_magic()
{
	int     	spell = 0;
	int 		item;
	bool 		is_list = FALSE;

	object_type 	*o_ptr;
	s16b		quantity;
	char 		string[(MAX_SPELLS_IN_BOOK * 2) + 1];
	int		num;

	int 		i;
	cptr 		q, s;


	/* Firstly we get the book to write in. Restrict choices to spellbooks */
	item_tester_tval = TV_SPELLBOOK;

	/* Get an item */
	q = "Write in which book? ";
	s = "You have no spellbooks.";
	if (!get_item(&item, q, s, USE_INVEN)) return FALSE;

	/* Get the item */
	o_ptr = &inventory[item];

	/* Extract the current spell list */
	if (o_ptr->spell_list) 
	{
		strncpy(string, quarky_str(o_ptr->spell_list), (MAX_SPELLS_IN_BOOK * 2) + 1);
	}
	else
	{
		string[0] = '\0';
	}

	/* Check how many spells we have */
	num = string ? strlen(string) / 2 : 0;

	/* Sanity check */
	if (num < 0 || num > MAX_SPELLS_IN_BOOK)
	{
		msg_print("Bad number of spells in spell book.");
		return FALSE;
	}

	/* Is it full? */
	if (num == MAX_SPELLS_IN_BOOK)
	{
		msg_print("The spellbook is full.");
		return FALSE;
	}
	
	/* Now we get the spell to write */
	if (!get_spell_from_grand_list(&spell, "write", TRUE, 
				num ? FALSE : TRUE, &is_list, 1)) 
	{
		msg_print("Could not get a spell to write");
		return FALSE;
	}
		
	if (is_list)
	{
		/* Sanity check */
		if (strlen(quarky_str((u16b) spell)) > (MAX_SPELLS_IN_BOOK * 2))
		{
			msg_print("Warning! Spell list too long!");
			return FALSE;
		}

		/* Copy string */
		strcpy(string, quarky_str((u16b) spell));
	}
	else
	{
		/* Does it already have the spell in? */
		for (i=0; i<num; ++i)
		{
			if (spell_number(string[i * 2],string[(i * 2) + 1]) == spell)
			{
				msg_print("The spellbook already contains that spell!");
				return FALSE;
			}
		}
	
		/* Construct the new string to hold the spell */
		string[num * 2] = spell_char_1(spell);
		string[(num * 2) + 1] = spell_char_2(spell);
		string[(num * 2) + 2] = '\0';
	}

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();
	
	/* Are we writing to a stack? */
	if (o_ptr->number > 1)
	{
		/* Find out how many to write to */
		quantity = get_quantity(format("Write in how many books? (1-%d): ",
					o_ptr->number), o_ptr->number);

		if (!quantity) return FALSE;
		
		if (quantity < o_ptr->number)
		{
			/* Unstack all but one book */
			object_type forge;
			object_type *q_ptr;

			/* Get local object */
			q_ptr = &forge;

			/* Obtain a local object */
			object_copy(q_ptr, o_ptr);

			/* Modify quantity */
			q_ptr->number = o_ptr->number - quantity;

			o_ptr->number = quantity;
	
			/* Save the new list */
			o_ptr->spell_list = save_spell_list(string, o_ptr->spell_list);

			/* Unstack */
			p_ptr->total_weight -= q_ptr->weight;
			item = inven_carry(q_ptr);

			/* Message */
			msg_print("You unstack your book.");
		}
		else
		{
			/* Write to all */
			o_ptr->spell_list = save_spell_list(string, o_ptr->spell_list);
		}
	}
	else
	{	
		/* Save it */
		o_ptr->spell_list = save_spell_list(string, o_ptr->spell_list);
	}

	/* Report */
	if (is_list) msg_print("You write a list of spells.");
	else msg_format("You write the spell: %s.", new_spell_name[spell]);

	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->redraw |= (PR_EQUIPPY);
	p_ptr->window |= (PW_INVEN);
	handle_stuff();

	/* Success */
	return TRUE;
}

/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 *
 * Based on calc_spells()
 */
void calc_spells_new(void)
{
	int			i, j, k, levels;
	int			num_allowed, num_known;

	/* Save the current number of spells to learn */
	s16b old_spells = p_ptr->new_spells;


	/* Hack -- must be literate */
	if (!mp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;


	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 50);


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 1; j < MAX_SPELLS_CURRENT; j++)
	{
		/* Count known spells */
		if (p_ptr->spell_learned[j / 32] & (1L << (j % 32)))
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;


	/* Forget spells which are too hard */
	for (i = MAX_SPELLS_CURRENT - 1; i > 0; i--)
	{
		/* Efficiency -- all done */
		/* Probably not efficient any more */
		/* if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break; */

		/* Access the spell */
		j = p_ptr->spells[i];

		/* Skip non-spells */
		if (!j || j >= MAX_SPELLS_CURRENT) continue;

		/* Skip spells we are allowed to know */
		if (calculate_spell_level(j) <= p_ptr->lev) continue;

		/* Is it known? */
		if (p_ptr->spell_learned[j / 32] & (1L << (j % 32)))
		{
			/* Mark as forgotten - no longer known */
			p_ptr->spell_forgotten[j / 32] |= (1L << (j % 32));
			p_ptr->spell_learned[j / 32] &= ~(1L << (j % 32));

			/* Message */
			msg_format("You have forgotten the spell of %s.",
			new_spell_name[j]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = MAX_SPELLS_CURRENT - 1; i > 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
		/* Probably not efficient any more */
		/* if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break; */

		/* Get the (i+1)th spell learned */
		j = p_ptr->spells[i];

		/* Skip unknown spells */
		if (!j || j >= MAX_SPELLS_CURRENT) continue;

		/* Forget it (if learned) */
		if (p_ptr->spell_learned[j / 32] & (1L << (j % 32)))
		{
			/* Mark as forgotten - no longer known */
			p_ptr->spell_forgotten[j / 32] |= (1L << (j % 32));
			p_ptr->spell_learned[j / 32] &= ~(1L << (j % 32));

			/* Message */
			msg_format("You have forgotten the spell of %s.",
			           new_spell_name[j]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 1; i < MAX_SPELLS_CURRENT; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Efficiency -- all done */
		/* Probably not efficient any more */
		/* if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2) break; */

		/* Get the next spell we learned */
		j = p_ptr->spells[i];

		/* Skip unknown spells */
		if (!j || j >= MAX_SPELLS_CURRENT) break;

		/* Skip spells we cannot remember */
		if (calculate_spell_level(j) > p_ptr->lev) continue;

		/* First set of spells */
		if (p_ptr->spell_forgotten[j / 32] & (1L << (j % 32)))
		{
			/* No longer forgotten - known once more */
			p_ptr->spell_forgotten[j / 32] &= ~(1L << (j % 32));
			p_ptr->spell_learned[j / 32] |= (1L << (j % 32));

			/* Message */
			msg_format("You have remembered the spell of %s.",
			           new_spell_name[j]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 1; j < MAX_SPELLS_CURRENT; j++)
	{
		/* Skip spells we cannot remember */
		if (calculate_spell_level(j) > p_ptr->lev) continue;

		/* Skip spells we already know */
		if (p_ptr->spell_learned[j / 32] & (1L << (j % 32)))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more spell%s.",
			           p_ptr->new_spells,
			           (p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}

#endif /* USE_NEW_MAGIC */

