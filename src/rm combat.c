/* File: combat.c */

/* Purpose: alternate combat routines */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* Table Definitions */
#define TABLE_AL 0
#define TABLE_SL 1
#define TABLE_CL 2
#define TABLE_RSL 3
#define TABLE_RCL 4

/* Critical Types */
#define CRIT_P 0
#define CRIT_S 1
#define CRIT_K 2
#define CRIT_G 3
#define CRIT_ST 4
#define CRIT_SW 5
#define CRIT_E 6
#define CRIT_I 7
#define CRIT_H 8
#define CRIT_C 9
#define CRIT_U 10
#define CRIT_T 11

/* One Handed Blades */
#define CHART_DAGGGER 0
#define CHART_FALCHION 1
#define CHART_HAND_AXE 2
#define CHART_MAIN GAUCHE 3
#define CHART_SCIMITAR 4
#define CHART_RAPIER 5
#define CHART_BROADSWORD 6
#define CHART_SHORT_SWORD 7

/* One Handed Misc */
#define CHART_BARE_FIST 8
#define CHART_CLUB 9
#define CHART_WAR_HAMMER 10
#define CHART_MACE 11
#define CHART_MORNING_STAR 12
#define CHART_WHIP 13

/* Missile */
#define CHART_BOLA 14
#define CHART_COMP_BOW 15
#define CHART_HVY_XBOW 16
#define CHART_LT_XBOW 17
#define CHART_LONG_BOW 18
#define CHART_SHORT_BOW 19
#define CHART_SLING 20

/* Two Handed */
#define CHART_BATTLE_AXE 21
#define CHART_FLAIL 22
#define CHART_WAR_MATTOCK 23
#define CHART_QUARTERSTAFF 24
#define CHART_2_HANDED_SWORD 25

/* Pole Arms */
#define CHART_JAVELIN 26
#define CHART_LANCE 27
#define CHART_POLE_ARM 28
#define CHART_SPEAR 29

/* Natural */
#define CHART_BEAK_PINCER 30
#define CHART_BITE 31
#define CHART_CLAW 32
#define CHART_GRAPPLE 33
#define CHART_HORN 34
#define CHART_BASH 35
#define CHART_STING 36
#define CHART_TINY 37
#define CHART_TRAMPLE 38
#define CHART_FALL_CRUSH 39

/* Martial Arts */
#define CHART_MA_STRIKES 40
#define CHART_MA_SWEEPS 41

/* Spells */
#define CHART_SHOCK_BOLT 42
#define CHART_WATER_BOLT 43
#define CHART_ICE_BOLT 44
#define CHART_FIRE_BOLT 45
#define CHART_LIGHTNING_BOLT 46
#define CHART_COLD_BALL 47
#define CHART_FIRE_BALL 48


/*
 * Armour DB Mods reduce the value of an attack based on how
 * much protection the specific armour type gives against the
 * table that the attack is rolled on.
 */
s16b armour_db_mod[5][20] = 
{
	/* TABLE_AL */
	{
		15,5,19,23,0,
		3,7,12,5,13,
		18,22,15,25,30,
		35,26,36,45,50
	},
	/* TABLE_SL */
	{
		2,0,12,16,11,
		16,21,19,12,19,
		24,29,13,16,23,
		24,12,15,23,28
	},
	/* TABLE_CL */
	{
		0,4,16,25,11,
		15,22,24,22,30,
		37,44,23,31,37,
		38,29,36,41,48
	},
	/* TABLE_RSL */
	{
		13,3,18,25,18,
		17,17,15,22,27,
		28,25,20,15,10,
		7,17,12,5,0
	},
	/* TABLE_RCL */
	{
		38,25,33,29,29,
		23,15,10,21,18,
		12,8,17,14,7,
		4,11,8,4,0
	}
};

/*
 * Armour_thresh values indicate the thresholds needed for
 * an attack to hit an opponent with that armour category
 * and to cause extra 'critical' damage against the opponent.
 */
s16b armour_thresh[5][6] =
{
	/* Skin (AT 1-4)*/
	{
		61,71,76,82,93,111
	},
	/* Soft Leather (AT 5-8)*/
	{
		52,72,84,99,112,131
	},
	/* Rigid Leather (AT 9-12)*/
	{
		52,72,83,98,114,126
	},
	/* Chain (AT 13-16) */
	{
		26,75,89,100,110,119
	},
	/* Plate (AT 17-20) */
	{
		5,75,89,97,104,109
	}
};

typedef struct attack_type attack_type;

struct attack_type
{
	s16b fumble_range; /* Roll this or less to fumble the attack */
	s16b table; /* The basic type of the attack */
	s16b crit1; /* The first choice of critical */
	s16b crit2; /* The second choice of critical */
	s16b ob_mod[5]; /* The bonus for this weapon against this armour category */
	double bhf[5]; /* The amount of damage per point of attack roll over the threshold */
};

attack_type attack_table[49]=
{
	/* Dagger */
	{
		1,TABLE_AL,CRIT_P,CRIT_S,
		{ -10,-12,-20,-20,-20},
		{ 4.7,8.2,8.5,10.7,22.9}
	},
	/* Falchion */
	{
		5,TABLE_AL,CRIT_S,CRIT_K,
		{ 0,-2,4,4,4},
		{ 2.3,3.3,3.3,4.4,6.4}
	},
	/* Hand Axe */
	{
		4,TABLE_AL,CRIT_S,CRIT_K,
		{ -5,-5,3,5,5},
		{ 3.1,4.5,4.2,5.5,8.5}
	},
	/* Main Gauche */
	{
		2,TABLE_AL,CRIT_P,CRIT_S,
		{ -8,-10,-15,-15,-15},
		{ 4.4,7.1,7.3,9.1,16.7}
	},
	/* Scimitar */
	{
		4,TABLE_AL,CRIT_S,CRIT_K,
		{ -5,-5,-5,-5,0},
		{ 2.5,3.7,3.9,7.7,13.2}
	},
	/* Rapier */
	{
		4,TABLE_AL,CRIT_P,CRIT_S,
		{ 10,5,-4,0,-18},
		{ 5.3,7.5,9.1,10.4,23.7}
	},
	/* Broadsword */
	{
		3,TABLE_AL,CRIT_S,CRIT_K,
		{ 0,0,0,0,0},
		{ 2.8,4.1,4.4,6.0,10.3}
	},
	/* Short Sword */
	{
		2,TABLE_AL,CRIT_S,CRIT_P,
		{ 5,0,-5,-10,-10},
		{ 3.8,5.4,6.4,8.7,14.9}
	},
	/* Bare Fist */
	{
		1,TABLE_AL,CRIT_K,CRIT_K,
		{ -25,-35,-35,-35,-31},
		{ 11.4,15.9,25.0,29.0,31.5}
	},
	/* Club */
	{
		4,TABLE_AL,CRIT_K,CRIT_K,
		{ -15,-15,-10,-10,-5},
		{ 3.9,5.7,4.9,5.7,10.3}
	},
	/* War Hammer */
	{
		4,TABLE_AL,CRIT_K,CRIT_P,
		{ -5,0,0,10,10},
		{ 3.2,4.7,4.2,5.0,7.7}
	},
	/* Mace */
	{
		2,TABLE_AL,CRIT_K,CRIT_K,
		{ -5,-5,0,5,5},
		{ 3.5,5.0,4.9,5.3,8.0}
	},
	/* Morning Star */
	{
		8,TABLE_AL,CRIT_K,CRIT_P,
		{ 0,0,5,10,10},
		{ 2.5,3.5,3.9,4.6,6.6}
	},
	/* Whip */
	{
		6,TABLE_AL,CRIT_K,CRIT_G,
		{ -9,-10,-15,-25,-25},
		{ 3.9,8.8,10.1,12.2,32.0}
	},
	/* Bola */
	{
		7,TABLE_AL,CRIT_K,CRIT_G,
		{ -10,-10,-10,-10,-6},
		{ 5.1,8.0,4.1,5.7,10.1}
	},
	/* Composite Bow */
	{
		4,TABLE_AL,CRIT_P,CRIT_P,
		{ -9,1,-2,10,5},
		{ 2.9,3.4,3.0,3.5,6.4}
	},
	/* Hvy Crossbow */
	{
		5,TABLE_AL,CRIT_P,CRIT_P,
		{ -2,6,4,15,10},
		{ 2.3,3.1,2.6,3.5,5.8}
	},
	/* Lght Crossbow */
	{
		5,TABLE_AL,CRIT_P,CRIT_P,
		{ -7,-3,-2,3,0},
		{ 2.8,3.4,3.1,3.7,7.1}
	},
	/* Long Bow */
	{
		5,TABLE_AL,CRIT_P,CRIT_P,
		{ -3,3,1,13,8},
		{ 2.6,3.2,2.9,3.3,5.8}
	},
	/* Short Bow */
	{
		4,TABLE_AL,CRIT_P,CRIT_P,
		{ -12,-4,-11,0,0},
		{ 3.1,4.3,3.6,4.0,9.9}
	},
	/* Sling */
	{
		6,TABLE_AL,CRIT_K,CRIT_K,
		{ -12,-15,-6,-5,0},
		{ 2.5,3.9,3.4,4.3,6.7}
	},
	/* Battle Axe */
	{
		5,TABLE_AL,CRIT_K,CRIT_S,
		{ 0,3,6,13,13},
		{1.5,2.3,2.0,2.6,4.6}
	},
	/* Flail */
	{
		8,TABLE_AL,CRIT_K,CRIT_S,
		{ 4,6,10,13,13},
		{ 1.8,2.4,2.6,3.3,4.5}
	},
	/* War Mattock */
	{
		6,TABLE_AL,CRIT_K,CRIT_K,
		{ 0,3,6,15,18},
		{ 1.7,2.2,2.2,2.7,3.6}
	},
	/* Quarterstaff */
	{
		3,TABLE_AL,CRIT_K,CRIT_K,
		{ -15,-12,-12,-12,-15},
		{ 2.8,3.7,4.5,6.1,9.2}
	},
	/* 2-Handed Sword */
	{
		5,TABLE_AL,CRIT_K,CRIT_S,
		{ 5,6,8,10,10},
		{ 1.6,2.1,2.0,2.8,4.7}
	},
	/* Javelin */
	{
		4,TABLE_AL,CRIT_P,CRIT_P,
		{ -6,-6,-6,-7,-10},
		{ 3.3,4.3,3.7,5.2,11.4}
	},
	/* Lance */
	{
		7,TABLE_AL,CRIT_P,CRIT_K,
		{ 0,4,5,17,20},
		{ 1.4,1.9,1.8,2.1,2.8}
	},
	/* Pole Arm */
	{
		7,TABLE_AL,CRIT_P,CRIT_K,
		{ -2,-2,-1,0,0},
		{ 1.8,2.5,2.5,3.3,5.2}
	},
	/* Spear */
	{
		5,TABLE_AL,CRIT_P,CRIT_S,
		{ -4,-5,0,3,-4},
		{ 2.8,4.0,3.6,4.7,10.5}
	},
	/* Beak/Pincer */
	{
		2,TABLE_CL,CRIT_S,CRIT_K,
		{ 3,5,-7,9,30},
		{ 3.4,3.3,4.3,4.5,4.6}
	},
	/* Bite */
	{
		2,TABLE_CL,CRIT_S,CRIT_P,
		{ 17,5,0,15,23},
		{ 2.5,2.6,2.7,3.0,3.2}
	},
	/* Claw */
	{
		2,TABLE_CL,CRIT_S,CRIT_P,
		{ 20,1,-3,11,22},
		{ 4.7,5.0,5.3,5.4,5.9}
	},
	/* Grapple */
	{
		2,TABLE_RCL,CRIT_G,CRIT_G,
		{ -6,-5,11,30,45},
		{ 5.0,6.4,9.2,8.0,12.4}
	},
	/* Horn */
	{
		2,TABLE_CL,CRIT_P,CRIT_U,
		{ 19,2,3,13,22},
		{ 2.9,3.1,3.2,3.4,3.5}
	},
	/* Bash */
	{
		2,TABLE_RCL,CRIT_K,CRIT_U,
		{ -4,-8,7,25,43},
		{ 5.7,5.8,6.3,5.9,6.8}
	},
	/* Sting */
	{
		2,TABLE_CL,CRIT_P,CRIT_P,
		{ 21,-5,-2,11,21},
		{ 9.3,10.4,9.7,10.8,11.5}
	},
	/* Tiny */
	{
		2,TABLE_CL,CRIT_T,CRIT_T,
		{ 26,4,0,14,25},
		{ 8.3,9.6,10.4,10.9,10.3}
	},
	/* Trample */
	{
		2,TABLE_CL,CRIT_K,CRIT_K,
		{ 37,18,9,28,34},
		{ 3.1,3.0,4.2,4.7,4.6}
	},
	/* Fall/Crush */
	{
		2,TABLE_CL,CRIT_K,CRIT_K,
		{ 45,35,23,45,43},
		{ 2.4,2.8,3.1,3.0,3.6}
	},
	/* M/A Strikes */
	{
		2,TABLE_CL,CRIT_ST,CRIT_ST,
		{ 24,17,2,24,22},
		{ 4.7,5.1,5.8,5.4,6.3}
	},
	/* M/A Sweeps */
	{
		2,TABLE_RCL,CRIT_SW,CRIT_SW,
		{ -10,1,13,34,55},
		{ 5.7,7.6,11.3,10.3,15.8}
	},
	/* Shock Bolt */
	{
		2,TABLE_RSL,CRIT_E,CRIT_E,
		{ 9,-11,-1,28,46},
		{ 7.0,8.6,10.2,8.6,10.2}
	},
	/* Water Bolt */
	{
		2,TABLE_SL,CRIT_I,CRIT_I,
		{ 6,-5,-3,16,28},
		{ 3.7,4.9,7.6,7.6,10.5}
	},
	/* Ice Bolt */
	{
		2,TABLE_RSL,CRIT_I,CRIT_C,
		{ 21,15,18,34,53},
		{ 2.6,3.5,6.0,6.1,8.5}
	},
	/* Fire Bolt */
	{
		2,TABLE_SL,CRIT_H,CRIT_I,
		{ 38,10,15,36,51},
		{ 3.3,4.2,6.2,5.8,7.7}
	},
	/* Lightning Bolt */
	{
		2,TABLE_RSL,CRIT_E,CRIT_I,
		{ 30,21,18,50,66},
		{ 3.0,4.0,5.2,4.8,5.6}
	},
	/* Cold Ball */
	{
		2,TABLE_SL,CRIT_C,CRIT_C,
		{ 62,39,48,64,76},
		{ 7.0,8.4,9.8,9.1,10.9}
	},
	/* Fire Ball */
	{
		2,TABLE_SL,CRIT_H,CRIT_H,
		{ 68,53,61,72,84},
		{ 5.6,6.0,7.4,8.2,9.5}
	}
};

/*
 * Calculate an attack
 * skill = bonus to the attack roll
 * arm_type = armour type (1-20) of the victim
 * at_type = attack type
 * at_size = size of attack (0-3)
 * dam = return value of damage done (-1 means a fumble)
 * crit_type = If passed, it is returned. If -1 passed, a type is selected
 * crit_level = returned critical level (0-6, 0 = no critical)
 * return value = true if hit, false if missed
 */
bool make_attack(s16b skill,s16b arm_type,s16b at_type, s16b at_size,
						s16b* dam,s16b* crit_type,s16b* crit_level)
{
	s16b roll;
	double base_dam;
	s16b at_table = attack_table[at_type].table;
	s16b arm_cat = (arm_type-1)/4;
	
	/* First reset the variables */
	*dam=0;
	*crit_level=0;

	/* Make the attack roll */
	roll=(s16b)rand_range(1,100);

	/* Test for a fumble */
	if (roll <= attack_table[at_type].fumble_range)
	{
		*dam=-1; /* Signal a fumble */
		return (FALSE); /* Return a miss */
	}

	/* We didn't fumble, so add the skill and limit the result */
	roll += skill;

	switch (at_size)
	{
	case 0:
		if (roll > 105) roll = 105;
		break;
	case 1:
		if (roll > 120) roll = 120;
		break;
	case 2:
		if (roll > 135) roll = 135;
		break;
	case 3:
		if (roll > 150) roll = 150;
		break;
	default:
		msg_format("Illegal attack size (%d)",at_size);
		return (FALSE);
	}

	/* Now add the modifier for the weapon against this armour type */
	roll += attack_table[at_type].ob_mod[arm_cat];

	/* Subtract the modifier for the armour type against the attack type */
	roll -= armour_db_mod[at_table][arm_type-1];

	/* Now test for a hit */
	if (roll < armour_thresh[arm_cat][0])
	{
		/* We missed */
		return (FALSE);
	}

	/* Work out the damage */
	base_dam = (double)(roll-armour_thresh[arm_cat][0]);
	*dam = (s16b)(base_dam/attack_table[at_type].bhf[arm_cat]);

	/* Always a minimal amount */
	if (*dam == 0) *dam = 1;

	/* Work out the crit level */
	if (roll >= armour_thresh[arm_cat][5])
	{
		*crit_level = 5;
	}
	else if (roll >= armour_thresh[arm_cat][4])
	{
		*crit_level = 4;
	}
	else if (roll >= armour_thresh[arm_cat][3])
	{
		*crit_level = 3;
	}
	else if (roll >= armour_thresh[arm_cat][2])
	{
		*crit_level = 2;
	}
	else if (roll >= armour_thresh[arm_cat][1])
	{
		*crit_level = 1;
	}
	else
	{
		*crit_level = 0;
	}

	if (*crit_type < 0)
	{
		*crit_type = attack_table[at_type].crit1;
		if (rand_range(1,2) == 2)
		{
			*crit_type = attack_table[at_type].crit2;
		}
	}
	return (TRUE);
}