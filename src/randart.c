
/*
 * Copyright (c) 2005 Jeff Greene, Diego Gonzalez
 *
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

#include "init.h"



#define MAX_TRIES 200
#define BUFLEN 1024

#define sign(x)	((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))

/* Total number of different slay types used */

#define SLAY_MAX 0x00010000L

/*
 * Average damage for good ego ammo of various types, used for balance
 * The current values assume normal (non-seeker) ammo enchanted to +9
 */
#define AVG_SLING_AMMO_DAMAGE 11
#define AVG_BOW_AMMO_DAMAGE 12
#define AVG_XBOW_AMMO_DAMAGE 12

/* Inhibiting factors for large bonus values */
#define INHIBIT_STRONG  15
#define INHIBIT_WEAK 7

#define ART_FLAGS_BAD 	(TR3_TELEPORT | TR3_DRAIN_EXP | TR3_IMPACT | TR3_AGGRAVATE | \
						 TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE)

/*
 * Numerical index values for the different types
 * These are to make the code more readable.
 * Note aggravate is handled separately.
 */
#define CAT_STATS		0
#define CAT_SPEED		1
#define CAT_SLAYS		2
#define CAT_BRANDS		3
#define CAT_RESISTS		4	/*includes immunities*/
#define CAT_ABILITIES	5	/*non-pval*/
#define CAT_TUNNEL		6
#define CAT_IMPACT		7
#define CAT_WEAP_XTRA	8
#define CAT_BOW_XTRA	9
#define CAT_STEALTH		10
#define CAT_VISION		11	/*infravision, searching*/
#define CAT_COMBAT		12
#define CAT_TO_AC		13
#define CAT_TO_BASE		14 /*add to base damage dice, sides, or armor base*/
#define CAT_WEIGH_LESS	15
#define CAT_LIGHT		16
#define CAT_NATIVE		17
#define CAT_MAX			18

/*The different types of artifacts*/
#define ART_TYPE_WEAPON			0
#define ART_TYPE_SHOVEL			1
#define ART_TYPE_BOW			2
#define ART_TYPE_SPECIAL		3	/*Rings, amulets, LIGHT sources*/
#define ART_TYPE_ARMOR			4
#define ART_TYPE_DRAG_ARMOR		5
#define ART_TYPE_CLOAK			6
#define ART_TYPE_SHIELD			7
#define ART_TYPE_HELM			8
#define ART_TYPE_CROWN			9
#define ART_TYPE_GLOVES			10
#define ART_TYPE_BOOTS			11
#define ART_TYPE_MAX 			12

/*
 * Table of frequency of each ability for each type.
 * Notice the total values in each row are not consitstent.
 * The total is added up in art_fequ_total, and
 * randart characteristics are selected based on a random number of that total.
 * The numbers are best changed in a spreadsheet, since changing one number affects
 * the percentages for each type of attribute in that item.
 */



/*
	#0  CAT_STATS
	#1  CAT_SPEED
	#2  CAT_SLAYS
	#3  CAT_BRANDS
	#4  CAT_RESISTS
	#5  CAT_ABILITIES
	#6  CAT_TUNNEL
	#7  CAT_IMPACT
	#8  CAT_WEAP_XTRA
	#9  CAT_BOW_XTRA
	#10  CAT_STEALTH
	#11 CAT_VISION
	#12 CAT_COMBAT
	#13 CAT_TO_AC
	#14 CAT_TO_BASE
	#15 CAT_WEIGH_LESS
	#16 CAT_LIGHT
*/

/*
 * Table of frequency of each ability for each type
 * The appropriate slot from this table is "downloaded" into art_freq table
 * for use when creating each randart.
 */
static const byte table_type_freq[ART_TYPE_MAX][CAT_MAX] =
{
		/*#0, #1,#2, #3, #4, #5, #6, #7, #8, #9,#10,#11,#12 #13#14 #15,#16 #17		*/
			{25, 2, 60,  7, 30, 12,  5,  2, 2,  0,  8,  1,120, 10, 18, 10, 20,	0}, /*  ART_TYPE_WEAPON   */
			{16, 1,	20,  3, 20, 10, 80, 10, 1,  0,  8,  1, 40, 10, 10, 10, 10,	0}, /*  ART_TYPE_SHOVEL   */
			{20, 1,	 0,  0, 10, 10,  0,  0, 0, 40,  8,  1, 80,  0,  0,  6,  0,	0}, /*  ART_TYPE_BOW   	*/
			{45, 3,	 0,  0, 30, 30,  0,  0, 0,  0, 10,  2, 10, 10,  0,  0, 14,	5}, /*  ART_TYPE_SPECIAL  */
			{30, 1,	 0,  0, 40, 14,  0,  0, 0,  0, 10,  1,  1, 50, 10, 20,  4,	0}, /*  ART_TYPE_ARMOR   	*/
			{30, 2,	 0,  0, 30, 20,  0,  0, 0,  0, 10,  1,  3, 50, 20, 20, 20,	0}, /*  ART_TYPE_DRAG_ARMOR   */
			{20, 2,	 0,  0, 30, 14,  0,  0, 0,  0, 20,  1,  3, 35, 10, 10,  4,	0}, /*  ART_TYPE_CLOAK 	*/
			{15, 1,  0,  0, 30, 10,  0,  0, 0,  0,  6,  1,  1, 50, 10, 10,  8,	0}, /*  ART_TYPE_SHIELD   */
			{35, 1,  0,  0, 20, 14,  0,  0, 0,  0, 10, 10,  1, 30, 10, 10, 14,	0}, /*  ART_TYPE_HELM   	*/
			{25, 1,  0,  0, 10, 24,  0,  0, 0,  0, 10, 10,  5, 25, 10, 10, 20,	0}, /*  ART_TYPE_CROWN  	*/
			{15, 1,  0,  0, 16, 10,  0,  0, 0,  0,  6,  1, 20, 40, 10, 10,  4,	0}, /*  ART_TYPE_GLOVES   */
			{16, 5,  0,  0, 16, 10,  0,  0, 0,  0, 30,  1,  5, 30, 10, 10,  4,	10}  /*  ART_TYPE_BOOTS   	*/
};


/*used to hold the frequencies for the above table for the current randart*/
static u16b art_freq[CAT_MAX];

/*
 *This list is sliightly different than the artifact type list above.
 *
 *The different types of artifact themes (Rings, amulets, LIGHT sources)
 */
#define ART_THEME_EDGED			0
#define ART_THEME_POLEARM		1
#define ART_THEME_HAFTED		2
#define ART_THEME_SHOVEL		3
#define ART_THEME_BOW			4
#define ART_THEME_ARMOR			5
#define ART_THEME_DRAG_ARMOR	6
#define ART_THEME_CLOAK			7
#define ART_THEME_SHIELD		8
#define ART_THEME_HELM			9
#define ART_THEME_GLOVES		10
#define ART_THEME_BOOTS			11
#define ART_THEME_MAX 			12

#define COL_THEME_FREQ  		0	/*frequency of generating an artifact outside of randart set*/
#define COL_THEME_MIN			1  	/*Minimum number of artifacts to use have in an artifact set*/
#define COL_THEME_DROP_TYPE		2  	/*Drop type to use when initializing the tables*/
#define COL_MAX					3

/*
 * Note the COL_THEME_FREQ must be less than
 * this number for the frequency table to work right
 */
#define MIN_ENFORCEMENT			50

static const byte theme_type[ART_THEME_MAX][COL_MAX] =
{
   /*#0, #1, #2, */
	{15,10,  DROP_TYPE_EDGED}, 		/*  ART_THEME_EDGED   	*/
	{12, 8,	 DROP_TYPE_POLEARM}, 	/*  ART_THEME_POLEARM	*/
	{12, 8,	 DROP_TYPE_HAFTED}, 	/*  ART_THEME_HAFTED   	*/
	{1,  1,	 DROP_TYPE_DIGGING}, 	/*  ART_THEME_SHOVEL  	*/
	{6,  5,	 DROP_TYPE_BOW}, 		/*  ART_THEME_BOW   	*/
	{13, 9,	 DROP_TYPE_ARMOR}, 		/*  ART_THEME_ARMOR 	*/
	{0,  0,  DROP_TYPE_DRAGON_ARMOR}, /* Currently handled by Armor, but this is needed elsewhere*/
	{7,	 5,	 DROP_TYPE_CLOAK}, 		/*  ART_THEME_CLOAK		*/
	{5,  4,  DROP_TYPE_SHIELD}, 	/*  ART_THEME_SHIELD   	*/
	{10, 7,  DROP_TYPE_HEADGEAR}, 	/*  ART_THEME_HELM   	*/
	{8,  6,  DROP_TYPE_GLOVES}, 	/*  ART_THEME_GLOVES   	*/
	{4,  3,  DROP_TYPE_BOOTS}  		/*  ART_THEME_BOOTS   	*/
};

static int art_theme_freq[ART_THEME_MAX];



#define NORMAL_FREQUENCY	10

/*
 * Make some stats more frequent for certain types.  A "0' just means normal frequency
 * The appropriate slot from this table is "downloaded" into art_freq table
 * for use when creating each randart.
 */
static const byte table_stat_freq[ART_TYPE_MAX][A_MAX] =
{
   /*STR INT WIS DEX CON CHR*/
	{  5,  0,  0,  0,  0,  0}, /*  ART_TYPE_WEAPON  */
	{  5,  0,  0,  0,  0,  0}, /*  ART_TYPE_SHOVEL  */
	{  0,  0,  0,  0,  0,  0}, /*  ART_TYPE_BOW   	*/
	{  0,  0,  0,  0,  0,  0}, /*  ART_TYPE_SPECIAL */
	{  0,  0,  0,  0,  5,  0}, /*  ART_TYPE_ARMOR   	*/
	{  0,  0,  0,  0,  0,  0}, /*  ART_TYPE_DRAG_ARMOR   */
	{  0,  0,  0,  0,  0,  0}, /*  ART_TYPE_CLOAK 	*/
	{  5,  0,  0,  0,  5,  0}, /*  ART_TYPE_SHIELD  */
	{  0,  5,  5,  0,  0,  5}, /*  ART_TYPE_HELM   	*/
	{  0, 10, 10,  0,  0, 10}, /*  ART_TYPE_CROWN  	*/
	{  0,  0,  0, 10,  5,  0}, /*  ART_TYPE_GLOVES  */
	{  0,  0,  0,  5,  0,  0}  /*  ART_TYPE_BOOTS   */
};

/*Current randart only - Used to keep weightings for each stat*/
static byte art_stat_freq[A_MAX];



/*
 * Frequencies of "higher" resists and immunities are determined by artifact
 * depth rather than by artifact type
 */

/*TR2_RESISTANCE is the basic 4 elements and is already defined in defines.h*/
#define TR2_LOW_RESIST (TR2_RES_FEAR | TR2_RES_POIS | TR2_RES_BLIND | \
					TR2_RES_CONFU | TR2_RES_NEXUS)
#define TR2_MED_RESIST (TR2_RES_LIGHT | TR2_RES_DARK | TR2_RES_SOUND | TR2_RES_SHARD)
#define TR2_HIGH_RESIST (TR2_RES_NETHR | TR2_RES_CHAOS | TR2_RES_DISEN)
/* TR2_IMMUNE_ALL covers immunities and is already defined in defines.h*/

/*
	#0  TR3_SLOW_DIGEST
	#1  TR3_FEATHER
	#2  TR3_LIGHT
	#3  TR3_REGEN
	#4  TR3_TELEPATHY
	#5  TR3_SEE_INVIS
	#6  TR3_FREE_ACT
	#7  TR3_HOLD_LIFE

 * Table of frequency adjustments of each ability for each type
 * 10 is normal frequency, -10 means no chance
 */
static const int table_ability_freq[ART_TYPE_MAX][OBJECT_XTRA_SIZE_POWER] =
{
   /*#0, #1, #2, #3, #4, #5, #6, #7*/
	{ 8,  7,  10, 10,  3, 10, 10,  5}, /*  ART_TYPE_WEAPON   */
	{ 8,  7,  10, 10,  3, 10, 10,  5}, /*  ART_TYPE_SHOVEL   */
	{ 8,  7,   0, 10,  3, 10, 10,  5}, /*  ART_TYPE_BOW   	*/
	{ 8,  5,   0, 10,  3, 10, 10,  7}, /*  ART_TYPE_SPECIAL  */
	{ 8,  7,   0, 10,  3, 10, 10,  5}, /*  ART_TYPE_ARMOR   	*/
	{ 8,  7,   0, 10,  5, 10, 10,  5}, /*  ART_TYPE_DRAG_ARMOR   */
	{ 8,  7,   0, 10,  3, 10, 10,  5}, /*  ART_TYPE_CLOAK 	*/
	{ 8,  7,   0, 10,  3, 10,  8,  5}, /*  ART_TYPE_SHIELD   */
	{ 8,  7,   0, 10,  5, 13,  8,  5}, /*  ART_TYPE_HELM   	*/
	{ 8,  7,   0, 10,  5, 13,  8,  5}, /*  ART_TYPE_CROWN  	*/
	{ 8,  7,   0, 10,  3, 10, 12,  5}, /*  ART_TYPE_GLOVES   */
	{ 8, 20,   0, 10,  3, 10,  8,  5}, /*  ART_TYPE_BOOTS   	*/
};

/*used to keep frequencies for each ability*/
static byte art_abil_freq[OBJECT_XTRA_SIZE_POWER];

#define NUM_FAVORED_SLAY_PAIRS 4

static const u32b favored_slay_pairs[NUM_FAVORED_SLAY_PAIRS][2] =
{
	{TR1_SLAY_UNDEAD, TR1_SLAY_DEMON},
	{TR1_SLAY_DEMON, TR1_SLAY_UNDEAD},
	{TR1_SLAY_ORC, TR1_SLAY_TROLL},
	{TR1_SLAY_TROLL, TR1_SLAY_ORC},
};

#define NUM_FAVORED_RESIST_PAIRS 3

static const u32b favored_resist_pairs[NUM_FAVORED_RESIST_PAIRS][2] =
{
	{TR2_RES_LIGHT, TR2_RES_DARK},
	{TR2_RES_DARK, TR2_RES_LIGHT},
	{TR2_RES_CHAOS, TR2_RES_CONFU},
};


/*
 * Boost ratings for combinations of ability bonuses
 * We go up to +24 here - anything higher is inhibited
 */
static s16b ability_power[25] =
	{0, 0, 0, 0, 0, 1, 2, 3, 4,
	6, 8, 10, 12, 15, 18, 21, 24, 28, 32,
	37, 42, 48, 55, 65, 75};

/*
 * Mean start and increment values for to_hit, to_dam and AC.  Update these
 * if the algorithm changes.  They are used in frequency generation.
 */

#define MEAN_HIT_INCREMENT  3
#define MEAN_DAM_INCREMENT  3
#define MEAN_HIT_STARTVAL  8
#define MEAN_DAM_STARTVAL  8
#define MEAN_AC_STARTVAL  15
#define MEAN_AC_INCREMENT  5

/*
 * Cache the results of lookup_kind(), which is expensive and would
 * otherwise be called much too often.
 */
static s16b *kinds;

/*
 * Store the original artifact power ratings
 */
static s32b *base_power;

/*
 * Store the original base item levels
 */
static byte *base_item_level;

/*
 * Store the original base item rarities
 */
static byte *base_item_rarity;

/*
 * Store the original artifact rarities
 */
static byte *base_art_rarity;

/* Store the current artifact k_idx */

static s16b cur_art_k_idx;


/*
 * Use W. Sheldon Simms' random name generator.  Generate a random word using
 * the probability tables we built at game startup.  Relies on the ASCII character
 * set.  Relies on European vowels (a, e, i, o, u).  The generated name should
 * be copied/used before calling this function again.
 */
static char *make_word(byte min_length, byte max_length)
{
	static char word_buf[90];
	int r, totalfreq;
	int tries, lnum, vow;
	int c_prev, c_cur, c_next;
	char *cp;

startover:
	vow = 0;
	lnum = 0;
	tries = 0;
	cp = word_buf;
	c_prev = c_cur = S_WORD;

	while (1)
	{
	    getletter:
		c_next = 0;
		r = rand_int(n_info->ltotal[c_prev][c_cur]);
		totalfreq = n_info->lprobs[c_prev][c_cur][c_next];

		/*find the letter*/
		while (totalfreq <= r)
		{
			c_next++;
			totalfreq += n_info->lprobs[c_prev][c_cur][c_next];
		}

		if (c_next == E_WORD)
		{
			if ((lnum < min_length) || vow == 0)
			{
				tries++;
				if (tries < 10) goto getletter;
				goto startover;
			}
			*cp = '\0';
			break;
		}

		if (lnum >= max_length) goto startover;

		*cp = I2A(c_next);

		if (my_is_vowel(*cp)) vow++;

		cp++;
		lnum++;
		c_prev = c_cur;
		c_cur = c_next;
	}

	word_buf[0] = toupper((unsigned char)word_buf[0]);

	return (word_buf);
}


void make_random_name(char *random_name, byte min, byte max)
{

	/*get the randomly generated word*/
	my_strcpy(random_name, make_word(min, max), max);

	return;
}


/*
 * Go through the attack types for this monster.
 * We look for the maximum possible maximum damage that this
 * monster can inflict in 10 game turns.  For melee
 * attacks we use the average damage assuming all attacks hit.
 * Spells are handled on a case by case basis.
 * For breaths we assume the monster has maximum HP.  In general
 * we assume all low  Special spells like
 * summoning that don't cause damage are  assigned a
 * 'fake' damage rating depending on player level.
 */

static long eval_max_dam(int r_idx)
{
	int i, x;
	u32b dam = 1;
	u32b hp;
	u32b melee_dam, atk_dam, spell_dam;
	byte rlev;
	monster_race *r_ptr;
	u32b flag, breath_mask, attack_mask, ball_mask, beam_mask;
	u32b flag_counter;

	r_ptr = &r_info[r_idx];

	/*clear the counters*/
	melee_dam = atk_dam = spell_dam = 0;

	/* Evaluate average HP for this monster */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) hp = r_ptr->hdice * r_ptr->hside;
	else hp = r_ptr->hdice * (r_ptr->hside + 1) / 2;

	/* Extract the monster level, force 1 for town monsters */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	for (x = 0; x < 4; x++)
	{

		/*Get the flags 4 monster flags and masks*/
		switch (x)
		{
			case 0:
			{
		 		flag = r_ptr->flags4;
				attack_mask = RF4_ATTACK_MASK;
				breath_mask = RF4_BREATH_MASK;
				ball_mask 	= RF4_BALL_MASK;
				beam_mask 	= RF4_BEAM_MASK;
				break;
			}
			case 1:
			{
		 		flag = r_ptr->flags5;
				attack_mask = RF5_ATTACK_MASK;
				breath_mask = RF5_BREATH_MASK;
				ball_mask 	= RF5_BALL_MASK;
				beam_mask 	= RF5_BEAM_MASK;
				break;
			}
			case 2:
			{
		 		flag = r_ptr->flags6;
				attack_mask = RF6_ATTACK_MASK;
				breath_mask = RF6_BREATH_MASK;
				ball_mask 	= RF6_BALL_MASK;
				beam_mask 	= RF6_BEAM_MASK;
				break;
			}
			case 3:
			default:
			{
		 		flag = r_ptr->flags7;
				attack_mask = RF7_ATTACK_MASK;
				breath_mask = RF7_BREATH_MASK;
				ball_mask 	= RF7_BALL_MASK;
				beam_mask 	= RF7_BEAM_MASK;
				break;
			}
		}

		/*no spells here, continue*/
		if (!flag) continue;

		flag_counter = 0x00000001;

		/* using 32 assumes a u32b flag size*/
		for (i = 0; i < 32; i++)
		{
			u16b this_dam = 0;

			/* First make sure monster has the flag*/
			if (flag & flag_counter)
			{
				bool powerful = (r_ptr->flags2 & (RF2_POWERFUL) ? TRUE : FALSE);

				/*Is it a breath? Should only be flag 4*/
				if (breath_mask & flag_counter)
				{
					int which_gf = 0;
					int mult = 1;
					int div_by = 1;

					/*hack - all breaths are in flag 4*/

					if (flag_counter == RF4_BRTH_ACID) 		which_gf = GF_ACID;
					else if (flag_counter == RF4_BRTH_ELEC) which_gf = GF_ELEC;
					else if (flag_counter == RF4_BRTH_FIRE) which_gf = GF_FIRE;
					else if (flag_counter == RF4_BRTH_COLD) which_gf = GF_COLD;
					else if (flag_counter == RF4_BRTH_POIS)
					{
						which_gf = GF_POIS;
						mult = 10;
						div_by = 9;
					}
					else if (flag_counter == RF4_BRTH_PLAS)
					{
						which_gf = GF_PLASMA;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_LIGHT)
					{
						which_gf = GF_LIGHT;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_DARK)
					{
						which_gf = GF_DARK;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_CONFU)
					{
						which_gf = GF_CONFUSION;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_SOUND)
					{
						which_gf = GF_SOUND;
						mult = 6;
						div_by = 5;
					}
					else if (flag_counter == RF4_BRTH_SHARD)
					{
						which_gf = GF_SHARD;
						mult = 8;
						div_by = 7;
					}
					else if (flag_counter == RF4_BRTH_INER)
					{
						which_gf = GF_INERTIA;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_GRAV)
					{
						which_gf = GF_GRAVITY;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_FORCE)
					{
						which_gf = GF_FORCE;
						mult = 6;
						div_by = 5;
					}
					else if (flag_counter == RF4_BRTH_NEXUS)
					{
						which_gf = GF_NEXUS;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_NETHR)
					{
						which_gf = GF_NETHER;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_CHAOS)
					{
						which_gf = GF_CHAOS;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_DISEN)
					{
						which_gf = GF_DISENCHANT;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_TIME)
					{
						which_gf = GF_TIME;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_MANA) which_gf = GF_MANA;

					if (which_gf)
					{
						this_dam = get_breath_dam(hp, which_gf, powerful);

						/* handle elemental breaths*/
						switch (which_gf)
					    {
							case GF_ACID:
							case GF_FIRE:
							case GF_COLD:
							case GF_ELEC:
							case GF_POIS:
							{
								/* Lets just pretend the player has the right base resist*/
								this_dam /= 3;

								break;
							}

							default: break;
						}

						this_dam = (this_dam * mult) / div_by;

						/*slight bonus for cloud_surround*/
						if (r_ptr->flags2 & RF2_CLOUD_SURROUND) this_dam = this_dam * 11 / 10;
					}
				}

				/*Is it a ball spell? Should only be flag 5*/
				else if (ball_mask & flag_counter)
				{
					int which_gf = 0;

					if (flag_counter == RF5_BALL_ACID) 		which_gf = GF_ACID;
					else if (flag_counter == RF5_BALL_ELEC) which_gf = GF_ELEC;
					else if (flag_counter == RF5_BALL_FIRE) which_gf = GF_FIRE;
					else if (flag_counter == RF5_BALL_COLD) which_gf = GF_COLD;
					else if (flag_counter == RF5_BALL_POIS)	which_gf = GF_POIS;
					else if (flag_counter == RF5_BALL_LIGHT)which_gf = GF_LIGHT;
					else if (flag_counter == RF5_BALL_DARK) which_gf = GF_DARK;
					else if (flag_counter == RF5_BALL_CONFU)which_gf = GF_CONFUSION;
					else if (flag_counter == RF5_BALL_SOUND)which_gf = GF_SOUND;
					else if (flag_counter == RF5_BALL_SHARD)which_gf = GF_SHARD;
					else if (flag_counter == RF5_BALL_METEOR)which_gf = GF_METEOR;
					else if (flag_counter == RF5_BALL_STORM) which_gf = GF_WATER;
					else if (flag_counter == RF5_BALL_NETHR)which_gf = GF_NETHER;
					else if (flag_counter == RF5_BALL_CHAOS)which_gf = GF_CHAOS;
					else if (flag_counter == RF5_BALL_MANA) which_gf = GF_MANA;
					else if (flag_counter == RF5_BALL_WATER)which_gf = GF_WATER;

					if (which_gf)
					{
						int attack = 96 + (x * 32) + i;

						this_dam = get_ball_beam_dam(-1, r_ptr, attack, which_gf, powerful);

						/* handle elemental breaths*/
						switch (which_gf)
						{
							case GF_ACID:
							case GF_FIRE:
							case GF_COLD:
							case GF_ELEC:
							case GF_POIS:
							{
								/* Lets just pretend the player has the right base resist*/
								this_dam /= 3;
								break;
							}

							default: break;
						}

						/*slight bonus for cloud_surround*/
						if (r_ptr->flags2 & RF2_CLOUD_SURROUND) this_dam = this_dam * 11 / 10;
					}
				}

				/*Is it a beam spell? Should only be flag 5*/
				else if (beam_mask & flag_counter)
				{
					int which_gf = 0;

					if (flag_counter == RF5_BEAM_ELEC) 		which_gf = GF_ELEC;
					else if (flag_counter == RF5_BEAM_ICE) 	which_gf = GF_ICE;
					else if (flag_counter == RF5_BEAM_NETHR)which_gf = GF_NETHER;
					else if (flag_counter == RF5_BEAM_LAVA)	which_gf = GF_LAVA;

					if (which_gf)
					{
						int attack = 96 + (x * 32) + i;

						this_dam = get_ball_beam_dam(-1, r_ptr, attack, which_gf, powerful);
					}

					/*slight bonus for cloud_surround*/
					if (r_ptr->flags2 & RF2_CLOUD_SURROUND) this_dam = this_dam * 11 / 10;
				}


				/*Is it an arrow, bolt, or beam?*/
				else if (attack_mask & flag_counter)
				{
					switch (x)
					{
						case 0:
						{
							this_dam = r_ptr->spell_power * spell_info_RF4[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF4[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 1:
						{
							this_dam = r_ptr->spell_power * spell_info_RF5[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF5[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 2:
						{
							this_dam = r_ptr->spell_power * spell_info_RF6[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF6[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 3:
						{
							this_dam = r_ptr->spell_power * spell_info_RF7[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF7[i][COL_SPELL_DAM_DIV]);
							break;
						}
					}
				}

				else switch (x)
				{
					/*Misc flag4 flags*/
					case 0:
					{
						if (flag_counter == RF4_SHRIEK) this_dam = rlev / 2;
						break;
					}

					case 1:
					{
						/*Right now all flag5 are attack mask spells*/
						break;
					}

					case 2:
					{
						/*Misc flag6 flags*/
						if (flag_counter == RF6_ADD_MANA) this_dam = MAX(r_ptr->mana, 30);
						else if (flag_counter == RF6_BLINK) this_dam = rlev / 3;
						else if (flag_counter == RF6_TELE_SELF_TO) this_dam = rlev * 2;
						else if (flag_counter == RF6_TELE_TO) this_dam = rlev;
						else if (flag_counter == RF6_DARKNESS) this_dam = rlev;
						else if (flag_counter == RF6_TRAPS) this_dam = rlev;
						else if (flag_counter == RF6_DRAIN_MANA) this_dam = rlev * 2;
						else if (flag_counter == RF6_HUNGER) this_dam = rlev;
						else if (flag_counter == RF6_SCARE) this_dam = rlev;
						else if (flag_counter == RF6_BLIND) this_dam = rlev;
						else if (flag_counter == RF6_CONF) this_dam = rlev;
						else if (flag_counter == RF6_SLOW) this_dam = rlev;
						else if (flag_counter == RF6_HOLD) this_dam = 25;
						break;
					}
					/*All flag7 flags*/
					case 3:
					{
						/*Right now all flag7 are summon spells*/
						/* All summons are assigned arbitrary values according to their levels*/
						if 		(flag_counter == RF7_S_KIN) 	this_dam = rlev * 2;
						else if (flag_counter == RF7_S_MONSTER)	this_dam = rlev * 2 / 5;
						else if (flag_counter == RF7_S_MONSTERS)this_dam = rlev * 4 / 5;
						else if (flag_counter == RF7_S_ANT)		this_dam = rlev / 5;
						else if (flag_counter == RF7_S_SPIDER)	this_dam = rlev / 5;
						else if (flag_counter == RF7_S_HOUND)	this_dam = rlev;
						else if (flag_counter == RF7_S_ANIMAL)	this_dam = rlev / 2;
						else if (flag_counter == RF7_S_HYDRA)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_THIEF)	this_dam = rlev / 2;
						else if (flag_counter == RF7_S_BERTBILLTOM)	this_dam = rlev * 3 / 4;
						else if (flag_counter == RF7_S_AINU)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_DRAGON)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_DRAGON) this_dam = rlev * 4;
						else if (flag_counter == RF7_S_DEMON)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_DEMON)this_dam = rlev * 3;
						else if (flag_counter == RF7_S_UNDEAD)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_UNDEAD)this_dam = rlev * 4;
						else if (flag_counter == RF7_S_WRAITH)	this_dam = rlev * 9 / 2;
						else if (flag_counter == RF7_S_UNIQUE)	this_dam = rlev * 3;
						else if (flag_counter == RF7_S_HI_UNIQUE)	this_dam = rlev * 5;
						break;
					}
				}

			}

			if (this_dam > spell_dam) spell_dam = this_dam;

			/*shift one bit*/
			flag_counter = flag_counter << 1;
		}
	}

	/* Only do if it has attacks */
	if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
	{
		for (i = 0; i < 4; i++)
		{
			/* Extract the attack infomation */
			int effect = r_ptr->blow[i].effect;
			int method = r_ptr->blow[i].method;
			int d_dice = r_ptr->blow[i].d_dice;
			int d_side = r_ptr->blow[i].d_side;

			/* Hack -- no more attacks */
			if (!method) continue;

			/* Assume average damage*/
			atk_dam = d_dice * (d_side + 1) / 2;

			switch (method)
			{
				/*possible stun*/
				case RBM_HIT:
				{
					if ((effect == RBE_WOUND) || (effect == RBE_BATTER))
					{
						atk_dam *= 5;
						atk_dam /= 4;
					}
					break;
				}
				/*stun definitely most dangerous*/
				case RBM_PUNCH:
				case RBM_KICK:
				case RBM_BUTT:
				case RBM_CRUSH:
				{
					atk_dam *= 4;
					atk_dam /= 3;
					break;
				}
				/*cut*/
				case RBM_CLAW:
				case RBM_BITE:
				case RBM_PECK:
				case RBM_BREATHE:
				{
					atk_dam *= 7;
					atk_dam /= 5;
					break;
				}
				default: break;
			}

			switch (effect)
			{
				/*other bad effects - minor*/
				case RBE_EAT_GOLD:
				case RBE_EAT_ITEM:
				case RBE_EAT_FOOD:
				case RBE_HUNGER:
				case RBE_EAT_LIGHT:
				case RBE_TERRIFY:
				{
					atk_dam *= 11;
					atk_dam /= 10;
					break;
				}
				/*other bad effects - major*/
				case RBE_UN_BONUS:
				case RBE_UN_POWER:
				case RBE_LOSE_MANA:
				case RBE_POISON:
				case RBE_ACID:
				case RBE_ELEC:
				case RBE_FIRE:
				case RBE_COLD:
				case RBE_BLIND:
				case RBE_CONFUSE:
				case RBE_PARALYZE:
				case RBE_DISEASE:
				case RBE_LOSE_STR:
				case RBE_LOSE_INT:
				case RBE_LOSE_WIS:
				case RBE_LOSE_DEX:
				case RBE_LOSE_CON:
				case RBE_LOSE_CHR:
				case RBE_LOSE_ALL:
				case RBE_EXP_10:
				case RBE_EXP_20:
				case RBE_EXP_40:
				case RBE_EXP_80:
				case RBE_HALLU:
				{
					atk_dam *= 5;
					atk_dam /= 4;
					break;
				}
				/*Earthquakes*/
				case RBE_SHATTER:
				{
					atk_dam *= 7;
					atk_dam /= 6;
					break;
				}
				/*nothing special*/
				default: break;
			}

			/*keep a running total*/
			melee_dam += atk_dam;
		}

		/*Reduce damamge potential for monsters that move randomly*/
		if ((r_ptr->flags1 & (RF1_RAND_25)) || (r_ptr->flags1 & (RF1_RAND_50)))
		{
			int reduce = 100;

			if (r_ptr->flags1 & (RF1_RAND_25)) reduce -= 25;
			if (r_ptr->flags1 & (RF1_RAND_50)) reduce -= 50;

			/*even moving randomly one in 8 times will hit the player*/
			reduce += (100 - reduce) / 8;

			/* adjust the melee damage*/
			melee_dam = (melee_dam * reduce) / 100;
		}

		/*monsters who can't move aren't nearly as much of a combat threat*/
		if (r_ptr->flags1 & (RF1_NEVER_MOVE))
		{
			melee_dam /= 4;
		}

		/*but keep a minimum*/
		if (melee_dam < 1) melee_dam = 1;
	}

	/*
	 * Get the max damage attack
	 */

	if (dam < spell_dam) dam = spell_dam;
	if (dam < melee_dam) dam = melee_dam;

	/*
	 * Adjust for speed.  Monster at speed 120 will do double damage,
	 * monster at speed 100 will do half, etc.  Bonus for monsters who can haste self.
	 */
	if (game_mode == GAME_NPPMORIA) dam = calc_energy_gain(r_ptr->r_speed + (r_ptr->flags6 & (RF6_HASTE) ? 1 : 0)) / 10;
	else dam = (dam * extract_energy_nppangband[r_ptr->r_speed + ((r_ptr->flags6 & (RF6_HASTE)) ? 5 : 0)]) / 10;

	/*but deep in a minimum*/
	if (dam < 1) dam  = 1;

	/* We're done */
	return (dam);
}

/*adjust a monsters hit points for how easily the monster is damaged*/
static u32b mon_hp_adjust(int r_idx)
{
	u32b hp;
	int slay_reduce = 0;

	monster_race *r_ptr = &r_info[r_idx];

	/*Get the monster base hitpoints*/
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) hp = r_ptr->hdice * r_ptr->hside;
	else hp = r_ptr->hdice * (r_ptr->hside + 1) / 2;

	/*just assume healers have more staying power*/
	if (r_ptr->flags6 & RF6_HEAL) hp = (hp * 6) / 5;
	else if (r_ptr->flags6 & RF6_CURE) hp = (hp * 15) / 14;

	/*monsters that can teleport are a hassle, and can easily run away*/
	if 	((r_ptr->flags6 & RF6_TPORT) ||
		 (r_ptr->flags6 & RF6_TELE_AWAY)||
		 (r_ptr->flags6 & RF6_TELE_LEVEL)) hp = (hp * 6) / 5;

	/*
	 * Base slays
	 */
	if (r_ptr->flags3 & RF3_ANIMAL)		slay_reduce++;
	if (r_ptr->flags3 & RF3_DEMON) 		slay_reduce++;
	if (r_ptr->flags3 & RF3_ORC) 		slay_reduce++;
	if (r_ptr->flags3 & RF3_TROLL) 		slay_reduce++;
	if (r_ptr->flags3 & RF3_GIANT)		slay_reduce++;
	if (r_ptr->flags3 & RF3_DRAGON) 	slay_reduce++;

	/*
	 * These two aren't very powerful, but the slays are so frequent and cover so much.
	 */
	if ((r_ptr->flags3 & RF3_EVIL) ||
		(r_ptr->flags3 & RF3_UNDEAD))	slay_reduce +=3;

	/* Do the resists to those who have all resists*/
	if (r_ptr->flags3 & RF3_IM_ELEM)
	{
		/*count resists*/
		byte counter = 0;

		if (r_ptr->flags3 & RF3_IM_ACID)	counter ++;
		if (r_ptr->flags3 & RF3_IM_FIRE) 	counter ++;
		if (r_ptr->flags3 & RF3_IM_COLD)	counter ++;
		if (r_ptr->flags3 & RF3_IM_ELEC)	counter ++;
		if (r_ptr->flags3 & RF3_IM_POIS)	counter ++;

		if (counter == 1) slay_reduce ++;
		else if (counter == 2) slay_reduce += 2;
		else if (counter == 3) slay_reduce += 3;
		else if (counter == 4) slay_reduce += 5;
		else if (counter == 5) slay_reduce += 8;
	}


	/*Not very powerful, but so common and easy to use*/
	if (r_ptr->flags3 & RF3_HURT_LIGHT)	slay_reduce += 2;
	if (r_ptr->flags3 & RF3_HURT_ROCK)	slay_reduce += 2;

	/*cut hitpoint value up to half, depending on suceptability to slays*/
	if (slay_reduce > 15) slay_reduce = 50;
	else slay_reduce = (30 - slay_reduce);

	hp = (hp * slay_reduce) / 30;

	/*boundry control*/
	if (hp < 1) hp = 1;

	/*regeneration means slightly more hp*/
	if (r_ptr->flags2 & RF2_REGENERATE) {hp *= 10; hp /= 9;}

	if (r_ptr->flags2 & RF2_EVASIVE) 	{hp *= 3; hp /= 2;}

	if (r_ptr->flags2 & RF2_INVISIBLE)
	{
			hp = (hp * (r_ptr->level + 2)) / r_ptr->level;
	}

	/*slight increase for no_charm*/
	if (r_ptr->flags3 & RF3_NO_CHARM) {hp *= 10; hp /= 9;}

	/*more boundry control*/
	if (hp < 1) hp = 1;

	return (hp);

}

/*
 * Initialize the data structures for the monster power ratings
 * ToDo: Add handling and return codes for error conditions if any.
 */

static bool init_mon_power(void)
{
	int i, j;

	s16b mon_count[MAX_DEPTH_ALL][CREATURE_TYPE_MAX];
	u32b mon_power_total[MAX_DEPTH_ALL][CREATURE_TYPE_MAX];
	monster_race *r_ptr;

	/*first clear the tables*/
	for (i = 0; i < MAX_DEPTH_ALL; i++)
	{

		for (j = 0; j < CREATURE_TYPE_MAX; j++)
		{
			mon_count[i][j] = 0;
			mon_power_total[i][j] = 0;
			mon_power_ave[i][j] = 0;
		}

	}

	/*
	 * Go through r_info and evaluate power ratings.
	 * ASSUMPTION: The monsters in r_info are in ascending order by level.
	 */
	for (i = 1; i < z_info->r_max; i++)
	{
		u32b hp;
		u32b dam;

		byte creature_type;

		r_ptr = &r_info[i];

		/*Hack - skip player ghost templates*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/*Hack - skip player ghost templates*/
		if (r_ptr->flags1 & (RF1_UNIQUE))	creature_type = CREATURE_UNIQUE;
		else creature_type = CREATURE_NON_UNIQUE;

		/*Hack - 0 power for town monsters*/
		if (!r_ptr->level) r_ptr->mon_power = hp = dam = 0;

		else
		{

			hp = mon_hp_adjust(i);

			/* Maximum damage this monster can do in 10 game turns*/
			dam = eval_max_dam(i);

			/* Define the power rating */
			r_ptr->mon_power = hp * dam;


		}

#ifdef ALLOW_DATA_DUMP

		/*record the hp and damage score*/
		r_ptr->mon_eval_hp	= hp;
		r_ptr->mon_eval_dam = dam;

#endif /*ALLOW_DATA_DUMP*/

		/*
		 * Slight adjustment for group monsters.
		 * Escorts are not evaluated because they tend to
		 * be much weaker than friends.
		 */
		if (r_ptr->flags1 & RF1_FRIEND) r_ptr->mon_power = (r_ptr->mon_power * 11) / 10;
		else if (r_ptr->flags1 & RF1_FRIENDS) r_ptr->mon_power = (r_ptr->mon_power * 8) / 7;

		mon_count[r_ptr->level][creature_type] ++;
		mon_power_total[r_ptr->level][creature_type] += r_ptr->mon_power;

	}

	/*populate the mon_power-ave table*/
	for (i = 0; i < MAX_DEPTH_ALL; i++)
	{
		for (j = 0; j < CREATURE_TYPE_MAX; j++)
		{

			/*leave the level as 0*/
			if (mon_count[i][j] == 0) continue;

			/*get the average power rater*/
			mon_power_ave[i][j] = mon_power_total[i][j] / mon_count[i][j];
		}

	}

	/*
	 * Now smooth it out because some levels aren't consistent, mainly due to
	 * there not being enough monsters at the deeper levels
     */
	for (i = 0; i < MAX_DEPTH_ALL; i++)
	{

		byte min_level = 1;

		for (j = 0; j < CREATURE_TYPE_MAX; j++)
		{

			/*empty levels*/
			if (mon_power_ave[i][j] == 0)
			{
				/*paranoia, so we don't crash on the next line*/
				if (i <= min_level) continue;

				/*use the previous level*/
				mon_power_ave[i][j] = mon_power_ave[i - 1][j];

				continue;
			}

		}

	}

#ifdef ALLOW_DATA_DUMP

	write_mon_power();

#endif /*ALLOW_DATA_DUMP*/

	/* Now we have all the ratings */
	return (TRUE);
}


#ifdef USE_ART_THEME

/* Return ART_THEME slot.
 * IMPORTANT: Assumes the function can_be_artifact would return true.
 * Not intended for special artifacts!!!
 * Not currently used, but too useful to delete
 */
static byte get_art_theme(const artifact_type *a_ptr)
{
	switch (a_ptr->tval)
	{
		case TV_HARD_ARMOR: /*fall through*/
		case TV_SOFT_ARMOR: return (ART_THEME_ARMOR);
		case TV_DRAG_ARMOR:	/*fall through*/
		case TV_DRAG_SHIELD:return (ART_THEME_DRAG_ARMOR);
		case TV_SHIELD:		return (ART_THEME_SHIELD);
		case TV_CLOAK:		return (ART_THEME_CLOAK);
		case TV_BOOTS:		return (ART_THEME_BOOTS);
		case TV_GLOVES:		return (ART_THEME_GLOVES);
		case TV_HELM:		/*fall through*/
		case TV_CROWN:		return (ART_THEME_HELM);
		case TV_BOW:		return (ART_THEME_BOW);
		case TV_SWORD:		return (ART_THEME_EDGED);
		case TV_HAFTED:		return (ART_THEME_HAFTED);
		case TV_POLEARM:	return (ART_THEME_POLEARM);
		case TV_DIGGING:	return (ART_THEME_SHOVEL);

		/*notice returning this would crash the game if it was looked
		 * up in a table, but I have to return something. -JG
		 */
		default:			return (ART_THEME_MAX);
	}


}

#endif /*USE_ART_THEME*/

/*
 * Calculate the rating for calculating a weapon base damage potential
 */
static int weapon_damage_calc(const artifact_type *a_ptr)
{

	int slay_adjust = 0;
	int damage_calc = (a_ptr->dd * (a_ptr->ds + 1) / 2);

	/*count up the number of slays and brands*/

	if (a_ptr->a_flags1 & TR1_SLAY_ANIMAL) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_EVIL) slay_adjust += 3;
	if (a_ptr->a_flags1 & TR1_SLAY_UNDEAD) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_DEMON) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_ORC) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_TROLL) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_GIANT) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_SLAY_DRAGON) slay_adjust += 2;
	if (a_ptr->a_flags1 & TR1_KILL_DRAGON) slay_adjust += 4;
	if (a_ptr->a_flags1 & TR1_KILL_DEMON) slay_adjust += 4;
	if (a_ptr->a_flags1 & TR1_KILL_UNDEAD) slay_adjust += 4;

	if (a_ptr->a_flags1 & TR1_BRAND_POIS) slay_adjust += 3;
	if (a_ptr->a_flags1 & TR1_BRAND_ACID) slay_adjust += 4;
	if (a_ptr->a_flags1 & TR1_BRAND_ELEC) slay_adjust += 4;
	if (a_ptr->a_flags1 & TR1_BRAND_FIRE) slay_adjust += 4;
	if (a_ptr->a_flags1 & TR1_BRAND_COLD) slay_adjust += 4;

	/*increse the weapon damage rater based on the number and power of slays*/
	damage_calc += damage_calc * (slay_adjust) / 10;

	/*multiply this figure for the extra blows*/
	if (a_ptr->a_flags1 & TR1_BLOWS) damage_calc *= a_ptr->pval;

	return (damage_calc);
}

/*
 * Calculate the multiplier we'll get with a given bow type.
 */
static int bow_multiplier(int sval)
{
	switch (sval)
	{
		case SV_SLING:
		case SV_SHORT_BOW:
			return (2);
		case SV_LONG_BOW:
		case SV_LIGHT_XBOW:
			return (3);
		case SV_HEAVY_XBOW:
			return (4);
		default:
			msg_format("Illegal bow sval %d", sval);
	}

	return (0);
}


/*
 * Evaluate the artifact's overall power level.
 * Must be sure there is an artifact created before calling this function
 */
s32b artifact_power(int a_idx)
{
	const artifact_type *a_ptr = &a_info[a_idx];
	s32b p = 0;

	object_kind *k_ptr = &k_info[cur_art_k_idx];

	int extra_stat_bonus = 0;

	/* Evaluate certain abilities based on type of object. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			/*
			 * Damage multiplier for bows should be weighted less than that
			 * for melee weapons, since players typically get fewer shots
			 * than hits (note, however, that the multipliers are applied
			 * afterwards in the bow calculation, not before as for melee
			 * weapons, which tends to bring these numbers back into line).
			 */

			if (a_ptr->to_d < 9)
			{
				/* Could enchant this up - just use to_d value of 9 */
				p += 9;
			}
			else
			{
				p += (a_ptr->to_d);
			}

			if (a_ptr->to_d > 15) p += (a_ptr->to_d - 15) * 2;
			if (a_ptr->to_d > 25) p += (a_ptr->to_d - 25) * 2;

			p += sign(a_ptr->to_h) * (ABS(a_ptr->to_h) / 3);

			if (a_ptr->to_h > 15) p += (a_ptr->to_h - 15) * 2;
			if (a_ptr->to_h > 25) p += (a_ptr->to_h - 25) * 2;
			/*
			 * Add the average damage of fully enchanted (good) ammo for this
			 * weapon.  Could make this dynamic based on k_info if desired.
			 */

			if (a_ptr->sval == SV_SLING)
			{
				p += AVG_SLING_AMMO_DAMAGE;
			}
			else if (a_ptr->sval == SV_SHORT_BOW ||
				a_ptr->sval == SV_LONG_BOW)
			{
				p += AVG_BOW_AMMO_DAMAGE;
			}
			else if (a_ptr->sval == SV_LIGHT_XBOW ||
				a_ptr->sval == SV_HEAVY_XBOW)
			{
				p += AVG_XBOW_AMMO_DAMAGE;
			}

			mult = bow_multiplier(a_ptr->sval);

			if (a_ptr->a_flags1 & TR1_MIGHT)
			{
				if (a_ptr->pval > 3 || a_ptr->pval < 0)
				{
					p += 20000;	/* inhibit */
					return (p);
				}
				else
				{
					mult += a_ptr->pval;
				}
			}
			p *= mult;

			if (a_ptr->a_flags1 & TR1_SHOTS)
			{
				/*
				 * Extra shots are calculated differently for bows than for
				 * slings or crossbows.
				 */
				if (a_ptr->pval > 3 || a_ptr->pval < 0)
				{
					p += 20000;	/* inhibit */
					return (p);
				}
				else if (a_ptr->pval > 0)
				{
					p = (p * (1 + a_ptr->pval));
				}

			}
			p += sign(a_ptr->to_h) * (ABS(a_ptr->to_h) / 3);

			/* Shots + might is incredibly powerful*/
			if ((a_ptr->a_flags1 & TR1_MIGHT) &&
				(a_ptr->a_flags1 & TR1_SHOTS))
			{
				p = p * 3 / 2;
			}

			/*
			 * Correction to match ratings to melee damage ratings.
			 * We multiply all missile weapons by 1.5 in order to compare damage.
			 * (CR 11/20/01 - changed this to 1.25).
			 * Melee weapons assume 5 attacks per turn, so we must also divide
			 * by 5 to get equal ratings.
			 */

			if (a_ptr->sval == SV_SHORT_BOW ||
				a_ptr->sval == SV_LONG_BOW)
			{
				p = sign(p) * (ABS(p) / 4);
			}
			else
			{
				p = sign(p) * (ABS(p) / 4);
			}

			if (a_ptr->weight < k_ptr->weight) p++;

			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/*calculate the weapon damage (factoring in slays and brands)*/
			p += weapon_damage_calc(a_ptr);

			if (a_ptr->to_d < 9)
			{
				/* This could be enchanted up, so just assume to_d of +9 */
				p += 9;
			}
			else
			{
				p += a_ptr->to_d;
			}

			if (a_ptr->to_d > 15) p += (a_ptr->to_d - 15) * 2;
			if (a_ptr->to_d > 25) p += (a_ptr->to_d - 25) * 2;

			p += sign(a_ptr->to_h) * (ABS(a_ptr->to_h) / 3);

			if (a_ptr->to_h > 15) p += (a_ptr->to_h - 15) * 2;
			if (a_ptr->to_h > 25) p += (a_ptr->to_h - 25) * 2;

			/*bonus to damage for well balanced and throwing items*/
			if (a_ptr->a_flags3 & TR3_THROWING)
			{
				if (a_ptr->a_flags3 & TR3_PERFECT_BALANCE) p += a_ptr->dd * (a_ptr->ds + 1) / 2;
				else p += a_ptr->dd * (a_ptr->ds + 1) / 4;
			}

			if (a_ptr->weight < k_ptr->weight) p++;

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		{
			p += sign(a_ptr->ac) * ((ABS(a_ptr->ac) * 2) / 3);

			p += sign(a_ptr->to_h) * ((ABS(a_ptr->to_h) * 2) / 3);

			p += a_ptr->to_d * 3;

			if (a_ptr->weight < k_ptr->weight) p++;

			break;
		}
		case TV_LIGHT:
		{
			p += 5;

			p += sign(a_ptr->to_h) * ((ABS(a_ptr->to_h) * 2) / 3);

			p += a_ptr->to_d * 3;

			break;
		}
		case TV_RING:
		case TV_AMULET:
		{

			p += sign(a_ptr->to_h) * ((ABS(a_ptr->to_h) * 2) / 3);

			p += a_ptr->to_d * 3;

			break;
		}
	}

	/* Other abilities are evaluated independent of the object type. */
	p += sign(a_ptr->to_a) * (ABS(a_ptr->to_a) / 2);

	if (a_ptr->to_a > 15)
	{
		p += (a_ptr->to_a - 15);
	}
	if (a_ptr->to_a > 25)
	{
		p += (a_ptr->to_a - 25);
	}


	if (a_ptr->pval > 0)
	{
		if (a_ptr->a_flags1 & TR1_TUNNEL) p += 1;

		if (a_ptr->a_flags1 & TR1_STR)
		{
			p += 3 * a_ptr->pval;
		}
		if (a_ptr->a_flags1 & TR1_INT)
		{
			p += 2 * a_ptr->pval;
		}
		if (a_ptr->a_flags1 & TR1_WIS)
		{
			p += 2 * a_ptr->pval;
		}
		if (a_ptr->a_flags1 & TR1_DEX)
		{
			p += 3 * a_ptr->pval;
		}
		if (a_ptr->a_flags1 & TR1_CON)
		{
			p += 4 * a_ptr->pval;
		}
		if (a_ptr->a_flags1 & TR1_STEALTH)
		{
			p += a_ptr->pval;
		}
		/* For now add very small amount for searching */
		if (a_ptr->a_flags1 & TR1_SEARCH)
		{
			p += a_ptr->pval / 6;
		}
		/* Add extra power term if there are a lot of ability bonuses */
		if (a_ptr->pval > 0)
		{
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_STR) ? a_ptr->pval: 0);
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_INT) ? 3 * a_ptr->pval / 4: 0);
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_WIS) ? 3 * a_ptr->pval / 4: 0);
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_DEX) ? a_ptr->pval: 0);
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_CON) ? a_ptr->pval: 0);
			extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_STEALTH) ? 3 * a_ptr->pval / 4: 0);

			if (a_ptr->tval == TV_BOW)
			{
				extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_MIGHT) ? 5 * a_ptr->pval / 2: 0);
				extra_stat_bonus += ( (a_ptr->a_flags1 & TR1_SHOTS) ? 3 * a_ptr->pval: 0);
			}

			if (extra_stat_bonus > 24)
			{
				/* Inhibit */
				p += 20000;
				return (p);
			}
			else
			{
				p += ability_power[extra_stat_bonus];
			}
		}

	}
	else if (a_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (a_ptr->a_flags1 & TR1_STR) p += 4 * a_ptr->pval;
		if (a_ptr->a_flags1 & TR1_INT) p += 2 * a_ptr->pval;
		if (a_ptr->a_flags1 & TR1_WIS) p += 2 * a_ptr->pval;
		if (a_ptr->a_flags1 & TR1_DEX) p += 3 * a_ptr->pval;
		if (a_ptr->a_flags1 & TR1_CON) p += 4 * a_ptr->pval;
		if (a_ptr->a_flags1 & TR1_STEALTH) p += a_ptr->pval;
	}
	if (a_ptr->a_flags1 & TR1_CHR)
	{
		p += a_ptr->pval;
	}
	if (a_ptr->a_flags1 & TR1_INFRA)
	{
		p += a_ptr->pval;
	}
	if (a_ptr->a_flags1 & TR1_SPEED)
	{
		/*high bonus for high speed bonus*/
		if (a_ptr->pval > 0)	p += (a_ptr->pval * 10);
		/*extra bonus for very high speeds*/
		if (a_ptr->pval > 5)	p += (a_ptr->pval - 5) * (a_ptr->pval - 5);

		/*linear subtration for negative bonus*/
		if (a_ptr->pval < 0) p += a_ptr->pval;
	}


	/*Do the sustains*/
	if (a_ptr->a_flags2 & TR2_SUST_STATS)
	{

		byte sustains = 0;

		if (a_ptr->a_flags2 & TR2_SUST_STR) {p += 5;  sustains++;}
		if (a_ptr->a_flags2 & TR2_SUST_INT) {p += 2;  sustains++;}
		if (a_ptr->a_flags2 & TR2_SUST_WIS) {p += 2;  sustains++;}
		if (a_ptr->a_flags2 & TR2_SUST_DEX) {p += 4;  sustains++;}
		if (a_ptr->a_flags2 & TR2_SUST_CON) {p += 3;  sustains++;}
		if (a_ptr->a_flags2 & TR2_SUST_CHR) {p += 1;  sustains++;}

		if (sustains > 4) p += 2;
		if (sustains > 5) p += 2;
	}

	/*Immunities*/
	if (a_ptr->a_flags2 & TR2_IMMUNE_ALL)
	{
		byte immunities = 0;

		if (a_ptr->a_flags2 & TR2_IM_ACID){ p += 17; immunities++;}
		if (a_ptr->a_flags2 & TR2_IM_ELEC){ p += 14; immunities++;}
		if (a_ptr->a_flags2 & TR2_IM_FIRE){ p += 22; immunities++;}
		if (a_ptr->a_flags2 & TR2_IM_COLD){ p += 17; immunities++;}
		if (a_ptr->a_flags2 & TR2_IM_POIS){ p += 12; immunities++;}

		if (immunities > 1)	p += 15;
		if (immunities > 2)	p += 15;
		if (immunities > 3) p += 15;
		/* inhibit */
		if (immunities > 4)
		{
			p += 20000;
			return (p);
		}
	}

	/*Abilities*/
	if (a_ptr->a_flags3 & TR3_ABILITIES_MASK)
	{
		byte abilities = 0;

		if (a_ptr->a_flags3 & TR3_SLOW_DIGEST) 	{p += 1;	abilities++;}
		if (a_ptr->a_flags3 & TR3_FEATHER) 		{p += 1;	abilities++;}
		if (a_ptr->a_flags3 & TR3_LIGHT) 		{p += 3;	abilities++;}
		if (a_ptr->a_flags3 & TR3_REGEN) 		{p += 4;	abilities++;}
		if (a_ptr->a_flags3 & TR3_TELEPATHY) 	{p += 15;	abilities++;}
		if (a_ptr->a_flags3 & TR3_SEE_INVIS) 	{p += 5;	abilities++;}
		if (a_ptr->a_flags3 & TR3_FREE_ACT) 	{p += 7;	abilities++;}
		if (a_ptr->a_flags3 & TR3_HOLD_LIFE) 	{p += 6;	abilities++;}

		if (abilities > 5) p += 5;
		if (abilities > 6) p += 5;
		if (abilities > 7) p += 5;
	}

	if (a_ptr->a_flags3 & TR3_BLESSED) p += 1;
	if (a_ptr->a_flags3 & TR3_SLOW_DIGEST) p += 1;

	/*Low resists*/
	if (a_ptr->a_flags2 & TR2_RESISTANCE)
	{
		byte resists = 0;

		if (a_ptr->a_flags2 & TR2_RES_ACID) {p += 2;  resists++;}
		if (a_ptr->a_flags2 & TR2_RES_ELEC) {p += 3;  resists++;}
		if (a_ptr->a_flags2 & TR2_RES_FIRE) {p += 3;  resists++;}
		if (a_ptr->a_flags2 & TR2_RES_COLD) {p += 3;  resists++;}

		if (resists == 4) p += 10;
	}

	/*High resists*/
	if (a_ptr->a_flags2 & TR2_RESISTANCES_MASK)
	{
		byte resists = 0;

		if (a_ptr->a_flags2 & TR2_RES_POIS)	{p += 14;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_LIGHT)	{p += 8;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_DARK)	{p += 8;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_BLIND){p += 8;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_CONFU){p += 12;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_SOUND){p += 3;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_SHARD){p += 4;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_NEXUS){p += 7;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_NETHR){p += 10;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_CHAOS){p += 10;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_DISEN){p += 10;	resists++;}
		if (a_ptr->a_flags2 & TR2_RES_LIGHT)	{p += 8;	resists++;}

		if (resists > 8) p += 3;
		if (resists > 9) p += 3;
		if (resists > 10) p += 3;
		if (resists > 11) p += 3;
	}

	/* Artifact makes player native to terrains*/
	if (a_ptr->a_native & TN1_NATIVE_LAVA) p += 25;
	if (a_ptr->a_native & TN1_NATIVE_ICE)  p += 10;
	if (a_ptr->a_native & TN1_NATIVE_OIL)  p += 15;
	if (a_ptr->a_native & TN1_NATIVE_FIRE)  p += 10;
	if (a_ptr->a_native & TN1_NATIVE_SAND)  p += 10;
	if (a_ptr->a_native & TN1_NATIVE_FOREST)  p += 10;
	if (a_ptr->a_native & TN1_NATIVE_WATER)  p += 15;
	if (a_ptr->a_native & TN1_NATIVE_ACID)  p += 10;
	if (a_ptr->a_native & TN1_NATIVE_MUD)  p += 10;


	return (p);
}

/*
 * Store the original artifact power ratings as a baseline
 */
static void store_base_power (void)
{
	int i;
	artifact_type *a_ptr;
	s16b k_idx;


	for(i = 0; i < z_info->art_norm_max; i++)
	{
		/*First store the base power of each item*/
		base_power[i] = artifact_power(i);
	}

	for(i = 0; i < z_info->art_norm_max; i++)
	{
		int y;
		bool found_rarity = FALSE;
		alloc_entry *table = alloc_kind_table;

		/* Kinds array was populated in the above step in artifact_power */
		k_idx = kinds[i];
		a_ptr = &a_info[i];

		/* Process probabilities */
		for (y = 0; y < alloc_kind_size; y++)
		{
			if (k_idx != table[y].index) continue;

			/*The table is sorted by depth, just use the lowest one*/
			base_item_level[i] = table[y].level;

			/*The rarity tables are divided by 100 in the prob_table*/
			base_item_rarity[i] = 100 / table[y].prob2;

			/*Paranoia*/
			if (base_item_rarity[i] < 1) base_item_rarity[i] = 1;

			found_rarity = TRUE;

			break;

		}

		/* Whoops!  Just make something up*/
		if (!found_rarity)
		{
			/*The table is sorted by depth, just use the lowest one*/
			base_item_level[i] = 1;
			base_item_rarity[i] = 1;
		}

		base_art_rarity[i] = a_ptr->a_rarity;
	}

}



/*
 * We've just added an ability which uses the pval bonus.  Make sure it's
 * not zero.  If it's currently negative, leave it negative (heh heh).
 */
static void do_pval(artifact_type *a_ptr)
{
	int factor = 1;
	/* Track whether we have blows, might or shots on this item */
	if (a_ptr->a_flags1 & TR1_BLOWS) factor++;
	if (a_ptr->a_flags1 & TR1_MIGHT) factor++;
	if (a_ptr->a_flags1 & TR1_SHOTS) factor++;

	/* Blows, might, shots handled separately */
	if (factor > 1)
	{
		a_ptr->pval = (s16b)(1 + rand_int(2));
		/* Give it a shot at +3 */
		if (one_in_(INHIBIT_STRONG)) a_ptr->pval = 3;
	}

	else if (a_ptr->pval == 0)
	{
		a_ptr->pval = (s16b)(1 + rand_int(2));
	}
	else if (a_ptr->pval < 0)
	{
		if (one_in_(2))
		{
			a_ptr->pval--;
		}
	}
	/*put reasonable limits on stat increases*/
	else if (a_ptr->a_flags1 & TR1_ALL_STATS)
	{

		if (a_ptr->pval > 6) a_ptr->pval = 6;
		else if (a_ptr->pval < 6)
		{
			if (one_in_(MAX(a_ptr->pval * factor, 1))) a_ptr->pval++;
		}

		/* = 6 or 7*/
		if (one_in_(INHIBIT_STRONG)) a_ptr->pval ++;
	}
	else if (one_in_(MAX(a_ptr->pval * factor, 1)))
	{
		/*
		 * CR: made this a bit rarer and diminishing with higher pval -
		 * also rarer if item has blows/might/shots already
		 */
		a_ptr->pval++;
	}
}


static void remove_contradictory(artifact_type *a_ptr)
{
	if (a_ptr->a_flags3 & TR3_AGGRAVATE) a_ptr->a_flags1 &= ~(TR1_STEALTH);
	if (a_ptr->a_flags2 & TR2_IM_ACID) a_ptr->a_flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->a_flags2 & TR2_IM_ELEC) a_ptr->a_flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->a_flags2 & TR2_IM_FIRE) a_ptr->a_flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->a_flags2 & TR2_IM_COLD) a_ptr->a_flags2 &= ~(TR2_RES_COLD);

	if (a_ptr->pval < 0)
	{
		if (a_ptr->a_flags1 & TR1_STR) a_ptr->a_flags2 &= ~(TR2_SUST_STR);
		if (a_ptr->a_flags1 & TR1_INT) a_ptr->a_flags2 &= ~(TR2_SUST_INT);
		if (a_ptr->a_flags1 & TR1_WIS) a_ptr->a_flags2 &= ~(TR2_SUST_WIS);
		if (a_ptr->a_flags1 & TR1_DEX) a_ptr->a_flags2 &= ~(TR2_SUST_DEX);
		if (a_ptr->a_flags1 & TR1_CON) a_ptr->a_flags2 &= ~(TR2_SUST_CON);
		if (a_ptr->a_flags1 & TR1_CHR) a_ptr->a_flags2 &= ~(TR2_SUST_CHR);
		a_ptr->a_flags1 &= ~(TR1_BLOWS);
	}

	if (a_ptr->a_flags3 & TR3_LIGHT_CURSE) a_ptr->a_flags3 &= ~(TR3_BLESSED);
	if (a_ptr->a_flags1 & TR1_KILL_DRAGON) a_ptr->a_flags1 &= ~(TR1_SLAY_DRAGON);
	if (a_ptr->a_flags1 & TR1_KILL_DEMON) a_ptr->a_flags1 &= ~(TR1_SLAY_DEMON);
	if (a_ptr->a_flags1 & TR1_KILL_UNDEAD) a_ptr->a_flags1 &= ~(TR1_SLAY_UNDEAD);
	if (a_ptr->a_flags3 & TR3_DRAIN_EXP) a_ptr->a_flags3 &= ~(TR3_HOLD_LIFE);

}

/*
 * Adjust the parsed frequencies for any peculiarities of the
 * algorithm.  For example, if stat bonuses and sustains are
 * being added in a correlated fashion, it will tend to push
 * the frequencies up for both of them.  In this method we
 * compensate for cases like this by applying corrective
 * scaling.
 */

/*
 * Choose a random ability using weights based on the given ability frequency
 * table.  The function returns false if no ability can be added.
 */
static bool add_ability(artifact_type *a_ptr)
{
	int abil_selector, abil_counter, counter, abil_freq_total;
	u32b flag;

	/*find out the current frequency total*/
	abil_freq_total = 0;

	flag = OBJECT_XTRA_BASE_POWER;

	for (abil_counter = 0; abil_counter < OBJECT_XTRA_SIZE_POWER; abil_counter++)
	{
		/*we already have this one added*/
		if (a_ptr->a_flags3 & flag)
		{
			/*Don't try to add it again*/
			art_abil_freq[abil_counter] = 0;
		}
		else abil_freq_total += art_abil_freq[abil_counter];

		/*shift the bit to check for the next ability*/
		flag = flag << 1;
	}

	/*We don't have anything else to add*/
	if (abil_freq_total == 0) return FALSE;

	/* Generate a random number between 1 and current ability total */
	abil_selector = randint(abil_freq_total);

	flag = OBJECT_XTRA_BASE_POWER;

	/* Find the entry in the table that this number represents. */
	counter = 0;
	for (abil_counter = 0; abil_counter < OBJECT_XTRA_SIZE_POWER; abil_counter++)
	{
		counter += art_abil_freq[abil_counter];

		/*we found the choice, stop and return the category*/
		if (counter >= abil_selector) break;

		/*shift the bit to check for the next ability*/
		flag = flag << 1;
	}

	/*We have the flag to add*/
	a_ptr->a_flags3 |= flag;

	return (TRUE);
}


/*
 * Sustain a sustain.  Try hard to add one that is positive.
 */

static bool add_sustain(artifact_type *a_ptr)
{
	int stat_selector, stat_counter, counter, stat_freq_total;
	u32b sust_flag, stat_flag;

	static byte art_sust_freq[A_MAX];

	/*find out the current frequency total*/
	stat_freq_total = 0;

	sust_flag = OBJECT_XTRA_BASE_SUSTAIN;
	stat_flag = OBJECT_XTRA_BASE_STAT_ADD;

	for (stat_counter = 0; stat_counter < A_MAX; stat_counter++)
	{
		/*we already have this one added*/
		if (a_ptr->a_flags2 & sust_flag)
		{
			/*Don't try to add it again*/
			art_sust_freq[stat_counter] = 0;
		}
		else
		{
			stat_freq_total += art_stat_freq[stat_counter];
			art_sust_freq[stat_counter] = art_stat_freq[stat_counter];

			/*Hack - add in a heavy bias for positive stats that aren't sustained yet*/
			if ((a_ptr->a_flags1 & stat_flag) && (a_ptr->pval > 0))
			{
				stat_freq_total += 100;
				art_sust_freq[stat_counter] += 100;
			}

		}

		/*shift the bit to check for the next stat*/
		sust_flag = sust_flag << 1;
		stat_flag = stat_flag << 1;
	}

	/*We don't have any stat to sustain*/
	if (stat_freq_total == 0) return FALSE;

	/* Generate a random number between 1 and current stat total */
	stat_selector = randint(stat_freq_total);

	sust_flag = OBJECT_XTRA_BASE_STAT_ADD;

	/* Find the entry in the table that this number represents. */
	counter = 0;
	for (stat_counter = 0; stat_counter < A_MAX; stat_counter++)
	{
		counter += art_sust_freq[stat_counter];

		/*we found the choice, stop and return the category*/
		if (counter >= stat_selector) break;

		/*shift the bit to check for the next stat*/
		sust_flag = sust_flag << 1;
	}

	/*We have the flag to add*/
	a_ptr->a_flags2 |= sust_flag;

	return (TRUE);
}

static bool add_stat(artifact_type *a_ptr)
{

	int stat_selector, stat_counter, counter, stat_freq_total;
	u32b flag_stat_add, flag_sustain;

	/*find out the current frequency total*/
	stat_freq_total = 0;

	flag_stat_add = OBJECT_XTRA_BASE_STAT_ADD;

	for (stat_counter = 0; stat_counter < A_MAX; stat_counter++)
	{
		/*we already have this one added*/
		if (a_ptr->a_flags1 & flag_stat_add)
		{
			art_stat_freq[stat_counter] = 0;
		}
		else stat_freq_total += art_stat_freq[stat_counter];

		/*shift the bit to check for the next stat*/
		flag_stat_add = flag_stat_add << 1;
	}

	/*We don't have any stat to add*/
	if (stat_freq_total == 0) return (FALSE);

	/* Generate a random number between 1 and current stat total */
	stat_selector = randint(stat_freq_total);

	flag_stat_add = OBJECT_XTRA_BASE_STAT_ADD;
	flag_sustain = OBJECT_XTRA_BASE_SUSTAIN;

	/* Find the entry in the table that this number represents. */
	counter = 0;
	for (stat_counter = 0; stat_counter < A_MAX; stat_counter++)
	{
		counter += art_stat_freq[stat_counter];

		/*we found the choice, stop and return the category*/
		if (counter >= stat_selector) break;

		/*shift the bit to check for the next stat*/
		flag_stat_add = flag_stat_add << 1;
		flag_sustain = flag_sustain << 1;
	}

	/*We have the flag to add*/
	a_ptr->a_flags1 |= flag_stat_add;

	/*50% of the time, add the sustain as well*/
	if (one_in_(2))
	{

		/*We don't have this one.  Add it*/
		a_ptr->a_flags2 |= flag_sustain;
	}

	/*re-do the pval*/
	do_pval(a_ptr);

	return (TRUE);

}

/*
 * Add a resist, with all applicable resists having an equal chance.
 * This function could really be used to add anything to flags2
 * so long as each one has an equal chance
 */
static bool add_one_resist(artifact_type *a_ptr, u32b avail_flags)
{
	/*Get their flag 2*/
	u32b has_flag_mask = 0L;
	byte i, counter;
	u32b flag_holder = 0x00000001;
	byte number_of_flags = 0;

	has_flag_mask |= a_ptr->a_flags2;

	/*Limit this to only the relevant flags*/
	has_flag_mask &= avail_flags;

	/*first count all the flags*/
	for (i = 0; i < 32; i++)
	{
		/*the flag is part of teh mask, and the artifact doesn't already have it*/
		if ((avail_flags & flag_holder) &&
			(!(has_flag_mask & flag_holder))) number_of_flags++;

		/*shift to the next bit*/
		flag_holder = flag_holder << 1;
	}

	/*no available flags*/
	if (number_of_flags == 0) return (FALSE);

	/*select a flag*/
	counter = randint(number_of_flags);

	/*re-set some things*/
	number_of_flags = 0;
	flag_holder = 0x00000001;

	/*first count all the flags*/
	for (i = 0; i < 32; i++)
	{
		if ((avail_flags & flag_holder) &&
			(!(has_flag_mask & flag_holder))) number_of_flags++;

		/*We found the flag - stop*/
		if (number_of_flags == counter) break;

		/*shift to the next bit*/
		flag_holder = flag_holder << 1;
	}

	/*add the flag and return*/
	a_ptr->a_flags2 |= flag_holder;

	/*try to add some of the complimentary pairs of resists*/
	for (counter = 0; counter < NUM_FAVORED_RESIST_PAIRS; counter ++)
	{
		if ((flag_holder == favored_resist_pairs[counter][0]) && (one_in_(2)))
		{
			a_ptr->a_flags2 |= (favored_resist_pairs[counter][1]);
			break;
		}
	}

	return (TRUE);
}



static bool add_brand(artifact_type *a_ptr)
{
	/* Hack - if all brands are added already, exit to avoid infinite loop */
	if ((a_ptr->a_flags1 & TR1_BRAND_ACID) && (a_ptr->a_flags1 & TR1_BRAND_ELEC) &&
		(a_ptr->a_flags1 & TR1_BRAND_COLD) && (a_ptr->a_flags1 & TR1_BRAND_FIRE) &&
		(a_ptr->a_flags1 & TR1_BRAND_POIS))  return (FALSE);

	/* Make sure we add one that hasn't been added yet */
	while (TRUE)
	{
		u32b brand_flag = OBJECT_XTRA_BASE_BRAND;

		int r = rand_int(OBJECT_XTRA_SIZE_BRAND);

		/*use bit operations to get to the right stat flag*/
		brand_flag = brand_flag << r;

		/*We already have this one*/
		if(a_ptr->a_flags1 & brand_flag) continue;

		/*We don't have this one.  Add it*/
		a_ptr->a_flags1 |= brand_flag;

		/* 50% of the time, add the corresponding resist. */
		if (one_in_(2))
		{
			u32b res_flag = OBJECT_XTRA_BASE_LOW_RESIST;
			res_flag = res_flag << r;
			a_ptr->a_flags2 |= res_flag;
		}

		/*Get out of the loop*/
		break;
	}



	return (TRUE);

}

/*
 * Add a slay or kill, return false if the artifact has all of them are all full.
  */

#define SLAYS_AND_KILLS (OBJECT_XTRA_SIZE_SLAY + OBJECT_XTRA_SIZE_KILL)

static bool add_slay(artifact_type *a_ptr)
{
	byte art_slay_freq[SLAYS_AND_KILLS];

	int slay_selector, slay_counter, counter, slay_freq_total;
	u32b flag_slay_add;

	/*find out the current frequency total*/
	slay_freq_total = 0;

	flag_slay_add = OBJECT_XTRA_BASE_SLAY;

	/*first check the slays*/
	for (slay_counter = 0; slay_counter < SLAYS_AND_KILLS; slay_counter++)
	{

		/*hack - don't add a slay when we already have a more powerful flag*/
		if ((a_ptr->a_flags1 & TR1_KILL_UNDEAD) &&
			(flag_slay_add == TR1_SLAY_UNDEAD)) art_slay_freq[slay_counter] = 0;
		else if ((a_ptr->a_flags1 & TR1_KILL_DEMON) &&
			(flag_slay_add == TR1_SLAY_DEMON)) art_slay_freq[slay_counter] = 0;
		else if ((a_ptr->a_flags1 & TR1_KILL_DRAGON) &&
			(flag_slay_add == TR1_SLAY_DRAGON)) art_slay_freq[slay_counter] = 0;

		/*We already have this one*/
		else if (a_ptr->a_flags1 & flag_slay_add)	art_slay_freq[slay_counter] = 0;

		/*We don't have this one*/
		else
		{
			if (slay_counter < OBJECT_XTRA_SIZE_SLAY)
				art_slay_freq[slay_counter] = NORMAL_FREQUENCY * 2;
			else art_slay_freq[slay_counter] = NORMAL_FREQUENCY / 2;
		}

		slay_freq_total += art_slay_freq[slay_counter];

		/*shift the bit to check for the next stat*/
		flag_slay_add = flag_slay_add << 1;
	}

	/*We don't have any stat to add*/
	if (slay_freq_total == 0) return (FALSE);

	/* Generate a random number between 1 and current stat total */
	slay_selector = randint(slay_freq_total);

	flag_slay_add = OBJECT_XTRA_BASE_SLAY;

	/* Find the entry in the table that this number represents. */
	counter = 0;
	for (slay_counter = 0; slay_counter < SLAYS_AND_KILLS; slay_counter++)
	{
		counter += art_stat_freq[slay_counter];

		/*we found the choice, stop and return the category*/
		if (counter >= slay_selector) break;

		/*shift the bit to check for the next stat*/
		flag_slay_add = flag_slay_add << 1;
	}

	/*We have the flag to add*/
	a_ptr->a_flags1 |= flag_slay_add;

	/*try to add some of the complimentary pairs of slays*/
	for (counter = 0; counter < NUM_FAVORED_SLAY_PAIRS; counter ++)
	{
		if ((flag_slay_add == favored_slay_pairs[counter][0]) && (one_in_(2)))
		{
			a_ptr->a_flags1 |= (favored_slay_pairs[counter][1]);
			break;
		}
	}

	return (TRUE);

}

static void add_to_hit(artifact_type *a_ptr, int fixed, int random)
{

	switch (a_ptr->tval)
	{
		/*Is it a weapon?*/
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* just not default*/
			break;
		}
		default:
	   	{
			/*Inhibit too high*/
			if ((a_ptr->to_h > 7) && (!one_in_(INHIBIT_WEAK))) return;

			if (a_ptr->to_h < 3) a_ptr->to_h += damroll(2,2);
			else a_ptr->to_h++;
			a_ptr->to_d = a_ptr->to_h;
			return;
		}
	}

	/*if cursed, make it worse*/
	if (a_ptr->to_h < 0)
	{
		a_ptr->to_h--;
		return;
	}

	/* Inhibit above certain threshholds */
	if (a_ptr->to_h > 25)
	{
		/* Strongly inhibit */
		if (one_in_(INHIBIT_STRONG)) a_ptr->to_h ++;
		return;
	}
	else if (a_ptr->to_h > 15)
	{
		/* Weakly inhibit */
		if (one_in_(INHIBIT_WEAK))	a_ptr->to_h +=randint(2);
		return;
	}
	else if (a_ptr->to_h > 5)
	{
		/*
		 * less of a bonus with greater to-hit, or for non-weapons
		 */
		random /= 2;
	}

	a_ptr->to_h += (fixed + rand_int(random));
	if (a_ptr->to_h > 0) a_ptr->a_flags3 |= TR3_SHOW_MODS;
}

static void add_to_dam(artifact_type *a_ptr, int fixed, int random)
{

	/*Handle non-weapons differently*/
	switch (a_ptr->tval)
	{
		/*Is it a weapon?*/
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* just not default*/
			break;
		}
		default:
	   	{
			/*Inhibit too high*/
			if ((a_ptr->to_d > 7) && (!one_in_(INHIBIT_WEAK))) return;


			if (a_ptr->to_d < 3) a_ptr->to_d += damroll(2,2);
			else a_ptr->to_d++;
			a_ptr->to_h = a_ptr->to_d;
			return;
		}
	}

	/*if cursed, make it worse*/
	if (a_ptr->to_d < 0)
	{
		a_ptr->to_d--;
		return;
	}

	/* Inhibit above certain threshholds */
	if (a_ptr->to_d > 25)
	{
		/* Strongly inhibit */
		if (one_in_(INHIBIT_STRONG)) a_ptr->to_d ++;
		return;
	}
	else if (a_ptr->to_d > 15)
	{
		/* Weakly inhibit */
		if (one_in_(INHIBIT_WEAK)) a_ptr->to_d += randint(2);
		return;
	}
	else if (a_ptr->to_d > 5)
	{
		/*
		 * less of a bonus with greater to-hit, or for non-weapons
		 */
		random /= 2;
	}
	a_ptr->to_d += (fixed + rand_int(random));
	if (a_ptr->to_d > 0) a_ptr->a_flags3 |= TR3_SHOW_MODS;
}

static void add_to_ac(artifact_type *a_ptr, int fixed, int random)
{
	/* Inhibit above certain threshholds */
	if (a_ptr->to_a > 20)
	{
		/* Strongly inhibit */
		if (one_in_(INHIBIT_STRONG)) a_ptr->to_a ++;
		return;

	}
	else if (a_ptr->to_h > 10)
	{
		/* Weakly inhibit */
		if (one_in_(INHIBIT_WEAK))	a_ptr->to_a += randint(2);
		return;
	}
	else if (a_ptr->to_h > 7)
	{
		/*
		 * less of a bonus with greater to-ac
		 */
		random /= 2;
	}
	a_ptr->to_a += (s16b)(fixed + rand_int(random));
}



static bool add_one_native(artifact_type *a_ptr)
{
	bool native[NUM_NATIVE];

	int native_selector, native_counter, counter, native_freq_total;
	u32b flag_native_add;

	/* Find out the current frequency total*/
	native_freq_total = 0;

	flag_native_add = P_NATIVE_LAVA;

	/* First check the natives*/
	for (native_counter = 0; native_counter < NUM_NATIVE; native_counter++)
	{
		native[native_counter] = FALSE;

		/* hack - don't add a native when we already have one*/
		if (!(a_ptr->a_native && (flag_native_add)))
		{
			native[native_counter] = TRUE;
			native_freq_total++;
		}

		/* Shift the bit to check for the next stat*/
		flag_native_add = flag_native_add << 1;
	}

	/* We don't have any native to add*/
	if (native_freq_total == 0) return (FALSE);

	/* Generate a random number between 1 and current stat total */
	native_selector = randint(native_freq_total);

	flag_native_add = OBJECT_XTRA_BASE_NATIVE;

	counter = 0;

	for (native_counter = 0; native_counter < NUM_NATIVE; native_counter++)
	{

		/* hack - don't add a slay when we already have one*/
		if (native[native_counter])
		{
			counter++;
		}

		/* We found the choice, stop and return the category*/
		if (counter >= native_selector) break;

		/* Shift the bit to check for the next stat*/
		flag_native_add = flag_native_add << 1;
	}

	/* We have the flag to add*/
	a_ptr->a_native |= flag_native_add;

	return (TRUE);

}



/*prepare a basic-non-magic artifact template based on the base object*/
static void	artifact_prep(s16b k_idx, int a_idx)
{

	object_kind *k_ptr = &k_info[k_idx];
	artifact_type *a_ptr = &a_info[a_idx];

	a_ptr->tval = k_ptr->tval;
	a_ptr->sval = k_ptr->sval;
	a_ptr->pval = k_ptr->pval;
	a_ptr->to_h = k_ptr->to_h;
	a_ptr->to_d = k_ptr->to_d;
	a_ptr->to_a = k_ptr->to_a;
	a_ptr->ac = k_ptr->ac;
	a_ptr->dd = k_ptr->dd;
	a_ptr->ds = k_ptr->ds;
	a_ptr->weight = k_ptr->weight;
	a_ptr->a_flags1 = k_ptr->k_flags1;
	a_ptr->a_flags2 = k_ptr->k_flags2;
	a_ptr->a_flags3 = k_ptr->k_flags3;

	/* Artifacts ignore everything */
	a_ptr->a_flags3 |= TR3_IGNORE_MASK;


	/* Assign basic stats to the artifact based on its artifact level. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			a_ptr->to_h += (s16b)(a_ptr->a_level / 10 + rand_int(4) +
			                      rand_int(4));
			a_ptr->to_d += (s16b)(a_ptr->a_level / 10 + rand_int(4));
			a_ptr->to_d += (s16b)(rand_int((a_ptr->dd * a_ptr->ds) / 2 + 1));
			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			a_ptr->to_a += (s16b)(a_ptr->a_level / 10 + a_ptr->ac / 3 +
			                      rand_int(8));

			if (a_ptr->to_a < 10)
				a_ptr->to_a += (s16b)(2 + rand_int(4) + rand_int(4));

			break;

		}
		/*Hack dragon armor & shields require special handling*/
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		{
			ego_item_type *e_ptr;
			int i, j, e_idx;

			alloc_entry *table = alloc_ego_table;

			long total, value;

			/* Reset total */
			total = 0L;

			/*Use an abbreviated version of ego-item where
			  object level doesn't matter so we always succeed*/

			/* Process probabilities */
			for (i = 0; i < alloc_ego_size; i++)
			{
				/* Default */
				table[i].prob3 = 0;

				/* Get the index */
				e_idx = table[i].index;

				/* Get the actual kind */
				e_ptr = &e_info[e_idx];

				/* Test if this is a legal ego-item type for this object */
				for (j = 0; j < EGO_TVALS_MAX; j++)
				{
					/* Require identical base type */
					if (a_ptr->tval == e_ptr->tval[j])
					{
						/* Require sval in bounds, lower */
						if (a_ptr->sval >= e_ptr->min_sval[j])
						{
							/* Require sval in bounds, upper */
							if (a_ptr->sval <= e_ptr->max_sval[j])
							{
								/* Accept */
								table[i].prob3 = table[i].prob2;
							}
						}
					}
				}

				/* Total */
				total += table[i].prob3;
			}

			/* Pick an ego-item */
			value = rand_int(total);

			/* Find the object */
			for (i = 0; i < alloc_ego_size; i++)
			{
				/* Found the entry */
				if (value < table[i].prob3) break;

				/* Decrement */
				value = value - table[i].prob3;
			}

			/*point to it*/
			e_ptr = &e_info[table[i].index];

			/*Apply the ego-item flags to the artifact*/
			a_ptr->a_flags1 |= e_ptr->flags1;
			a_ptr->a_flags2 |= e_ptr->flags2;
			a_ptr->a_flags3 |= e_ptr->flags3;

			break;

		}

		/*break for the switch a_ptr->tval*/
		default: break;
	}
}

/*
 * Build a suitable frequency table for this item, based on the object type.
 * This must be called before any randart can be made.
 *
 * To do: alter probabilities for possible race and class themes
 */
static void build_freq_table(artifact_type *a_ptr)
{
	int i;

	byte art_type;

	switch (a_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			art_type = ART_TYPE_WEAPON;
			break;
		}
		case TV_DIGGING:
		{
			art_type = ART_TYPE_SHOVEL;
			break;
		}
		case TV_BOW:
		{
			art_type = ART_TYPE_BOW;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		case TV_LIGHT:
		{
			art_type = ART_TYPE_SPECIAL;
			break;
		}
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			art_type = ART_TYPE_ARMOR;
			break;
		}
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		{
			art_type = ART_TYPE_DRAG_ARMOR;
			break;
		}
		case TV_CLOAK:
		{
			art_type = ART_TYPE_CLOAK;
			break;
		}
		case TV_SHIELD:
		{
			art_type = ART_TYPE_SHIELD;
			break;
		}
		case TV_HELM:
		{
			art_type = ART_TYPE_HELM;
			break;
		}
		case TV_CROWN:
		{
			art_type = ART_TYPE_CROWN;
			break;
		}
		case TV_BOOTS:
		{
			art_type = ART_TYPE_BOOTS;
			break;
		}
		case TV_GLOVES:
		{
			art_type = ART_TYPE_GLOVES;
			break;
		}
		/*
		 * Notice the default will cause the game to crash since at this point
		 * there is no turning back when creating an adult random artifact game. -JG
		 */
		 default: return;
	}

	/* Load the frequencies*/
	for (i = 0; i < CAT_MAX; i++)
	{
		art_freq[i] = table_type_freq[art_type][i];
	}

	/*load the stat frequency table*/
	for (i = 0; i < A_MAX; i++)
	{
		art_stat_freq[i] = NORMAL_FREQUENCY + table_stat_freq[art_type][i];
	}

	/*Load the abilities frequency table*/
	for (i = 0; i < OBJECT_XTRA_SIZE_POWER; i++)
	{
		art_abil_freq[i] = table_ability_freq[art_type][i];
	}

	/*High resists and immunities are determined by artifact depth, not a frequency table*/

	/*Get the current k_idx*/
	cur_art_k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

}

/*
 * Try very hard to increase weighting to succeed in creating minimum values.
 */
static void adjust_art_freq_table(void)
{
	byte i;

	int art_min_total = 0;

	for (i = 0; i < ART_THEME_MAX; i++)
	{

		art_theme_freq[i] += (theme_type[i][COL_THEME_MIN]) * MIN_ENFORCEMENT;

		/*keep track of the total to make sure we aren't attempting the impossible*/
		art_min_total += theme_type[i][COL_THEME_MIN];

	}

	/*
	 * If necessary, reduce minimums to make sure the total minimum artifact is
	 * less than 80% of the regular artifact set.
	 */
	while (art_min_total > ((z_info->art_norm_max - z_info->art_spec_max) * 8 / 10))
	{

		for (i = 0; i < ART_THEME_MAX; i++)
		{
			if (art_theme_freq[i] > MIN_ENFORCEMENT)
			{
				art_theme_freq[i] -= MIN_ENFORCEMENT;
			}

			art_min_total--;
		}
	}

	return;
}


/*
 * Build the frequency tables
 */
static void build_art_freq_table(void)
{
	byte i;

	for (i = 0; i < ART_THEME_MAX; i++)
	{
		art_theme_freq[i] = theme_type[i][COL_THEME_FREQ];
	}

	return;
}

/*
 * Pick a category of weapon randomly.
 */
static byte get_theme(void)
{
	byte theme;

	int counter, theme_selector, theme_freq_total;

	/*find out the current frequency total*/
	theme_freq_total = 0;

	for (theme = 0; theme < ART_THEME_MAX; theme++)
	{
		theme_freq_total += art_theme_freq[theme];
	}

	/* Generate a random number between 1 and current frequency total */
	theme_selector = randint(theme_freq_total);

	/* Find the entry in the table that this number represents. */

	counter = 0;
	theme = 0;
	for (theme = 0; theme < ART_THEME_MAX; theme++)
	{
		counter += art_theme_freq[theme];

		/*we found the choice, stop and return the category*/
		if (counter >= theme_selector) break;

	}

	/*
	 * This should only happen when the
	 * adult_rand_artifacts option is true
	 */
	if (art_theme_freq[theme] >= MIN_ENFORCEMENT) art_theme_freq[theme] -= MIN_ENFORCEMENT;

	/*return the appropriate drop type*/
	return (theme_type[theme][COL_THEME_DROP_TYPE]);

}

/*
 * Randomly select a base item type (tval,sval).  Assign the various fields
 * corresponding to that choice.
 */
static void choose_item(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	object_kind *k_ptr;
	s16b k_idx;
	byte theme;

	byte target_level;

    if (a_idx < z_info->art_norm_max)
	{
		target_level = (base_item_level[a_idx] +
						(rand_int(MAX_DEPTH - base_item_level[a_idx])));
	}
	else target_level = object_level + 5;

	/*
	 * Look up the original artifact's base object kind to get level and
	 * rarity information to supplement the artifact level/rarity.  As a
	 * degenerate case consider Bladeturner.
	 */
	/*If replacing standard art set, replace the rarity*/
	if (a_idx < z_info->art_norm_max)
	{
		int y;

		alloc_entry *table = alloc_kind_table;

		k_idx = kinds[a_idx];

		/* Process probabilities */
		for (y = 0; y < alloc_kind_size; y++)
		{
			if (k_idx != table[y].index) continue;

			/*The rarity tables are divided by 100 in the prob_table*/
			a_ptr->a_rarity += (100 / table[y].prob2);

			break;
		}
	}

	theme = get_theme();

	/*prepare the object generation level for a specific theme*/
	if(!prep_object_theme(theme)) return;

	k_idx = 0;

	/*get the object number*/
	while (!k_idx) k_idx = get_obj_num(target_level);

	/* Clear restriction */
	get_obj_num_hook = NULL;

	/* Un-do the object theme */
	get_obj_num_prep();

	k_ptr = &k_info[k_idx];

	/*prepare a basic-non-magic artifact template based on the base object*/
	artifact_prep(k_idx, a_idx);

	a_ptr->a_flags1 |= k_ptr->k_flags1;
	a_ptr->a_flags2 |= k_ptr->k_flags2;
	a_ptr->a_flags3 |= k_ptr->k_flags3;

}

/*
 * Choose a random feature using weights based on the given cumulative frequency
 * table.  A pointer to the frequency array (which must be of size ART_IDX_TOTAL)
 * is passed as a parameter.  The function returns a number representing the
 * index of the ability chosen.
 */
static int choose_power_type (void)
{
	int cat_selector, cat_counter, counter, art_freq_total;

	/*find out the current frequency total*/
	art_freq_total = 0;

	for (cat_counter = 0; cat_counter < CAT_MAX; cat_counter++)
	{
		art_freq_total += art_freq[cat_counter];
	}

	/* Generate a random number between 1 and current frequency total */
	cat_selector = randint(art_freq_total);

	/* Find the entry in the table that this number represents. */
	counter = 0;
	for (cat_counter = 0; cat_counter < CAT_MAX; cat_counter++)
	{
		counter += art_freq[cat_counter];

		/*we found the choice, stop and return the category*/
		if (counter >= cat_selector) break;
	}

	return (cat_counter);
}

/*
 * Add an ability given by the index choice.  This is mostly just a long case
 * statement.
 *
 * Note that this method is totally general and imposes no restrictions on
 * appropriate item type for a given ability.  This is assumed to have
 * been done already.
 */

static void add_feature_aux(artifact_type *a_ptr, int choice)
{

	switch(choice)
	{
		case CAT_STATS:
		{
			/*add a stat, or if all stats are taken and sustained, don't try again*/
			byte choice = randint(((a_ptr->a_level > 30) ? 30 : a_ptr->a_level));

			/*2/3 of the time, try to add a stat, except at low levels*/
			if (choice <=10)
			{
				if (!add_sustain(a_ptr))
				{
					if (!add_stat(a_ptr))	art_freq[CAT_STATS] = 0;
				}

			}
			else
			{
				if (!add_stat(a_ptr))
				{
					if (!add_sustain(a_ptr)) art_freq[CAT_STATS] = 0;
				}

			}
			break;
		}
		case CAT_SPEED:
		{
			/*
			 * Note there is a reason to add this again.
			 * Do PVAL might increase the speed.
			 */
			if ((one_in_(2)) || (a_ptr->a_flags1 & TR1_SPEED))
			{
				a_ptr->a_flags1 |= TR1_SPEED;
				do_pval(a_ptr);
			}
			break;
		}
		case CAT_SLAYS:
		{
			/*add a slay, or if all slays are taken, don't try again*/
			if (!add_slay(a_ptr))  art_freq[CAT_SLAYS] = 0;
			break;
		}
		case CAT_BRANDS:
		{
			/*add a brand, or if all brands are taken, don't try again*/
			if (!add_brand(a_ptr)) art_freq[CAT_BRANDS] = 0;
			break;
		}
		case CAT_RESISTS:
		{
			byte choice;

			/*resists are added by depth*/
			byte highest = ((a_ptr->a_level > 52) ? 52 : a_ptr->a_level);

			/*occasionally increase the level*/
			while (one_in_(10)) highest += 3;

			/*make the selection*/
			choice = randint(highest);

			/*add resists, the power of which depends on artifact depth*/
			if (choice <= 18)
			{
				/*exit if we added a base resist*/
				if (add_one_resist(a_ptr, TR2_RESISTANCE)) break;
			}
			/*add resists, the power of which depends on artifact depth*/
			if (choice <= 30)
			{
				/*exit if we added a low resist*/
				if (add_one_resist(a_ptr, TR2_LOW_RESIST)) break;
			}
			if (choice <= 40)
			{
				/*exit if we added a medium resist*/
				if (add_one_resist(a_ptr, TR2_MED_RESIST)) break;
			}
			if (choice <= 50)
			{
				/*exit if we added a low resist*/
				if (add_one_resist(a_ptr, TR2_HIGH_RESIST)) break;
			}
			/*add an immunity if all else has failed or the number is high enough*/
			if (choice > 50) (void)add_one_resist(a_ptr, TR2_IMMUNE_ALL);

			break;
		}
		case CAT_ABILITIES:
		{
			if (!add_ability(a_ptr)) art_freq[CAT_ABILITIES] = 0;
			break;
		}
		case CAT_TUNNEL:
		{
			object_kind *k_ptr = &k_info[cur_art_k_idx];

			/*
			 * Don't try again if we already have this one, PVAL is negative,
			 * or if it too light
			 */
			if ((a_ptr->a_flags1 & TR1_TUNNEL) || (a_ptr->pval < 0) ||
				(k_ptr->weight < 50))
			{

				art_freq[CAT_TUNNEL] = 0;
				break;
			}

			switch (a_ptr->tval)
			{
				case TV_HAFTED:
				case TV_DIGGING:
				case TV_POLEARM:
				case TV_SWORD:
				{
					a_ptr->a_flags1 |= TR1_TUNNEL;
					do_pval(a_ptr);
					break;
				}
				default: art_freq[CAT_TUNNEL] = 0;
			}
			break;
		}
		case CAT_IMPACT:
		{
			object_kind *k_ptr = &k_info[cur_art_k_idx];

			/*Only try this one once*/
			art_freq[CAT_IMPACT] = 0;

			/*light objects shouldn't be allowed to impact*/
			if (k_ptr->weight < 120) break;

			/*heavier objects only do this some of the time*/
			if (randint(1000) < (k_ptr->weight - 120))	a_ptr->a_flags3 |= TR3_IMPACT;

			break;
		}
		case CAT_WEAP_XTRA:
		{
			/*WE already have this one.  Don't check again*/
			if ((a_ptr->a_flags1 & TR1_BLOWS) || (a_ptr->pval < 0))
			{
				art_freq[CAT_WEAP_XTRA] = 0;
				break;
			}
			if (one_in_(2))
			{
				a_ptr->a_flags1 |= TR1_BLOWS;
				do_pval(a_ptr);
			}
			break;
		}
		case CAT_BOW_XTRA:
		{
			/*All full, or negative pval - Prevent this from being called again*/
			if (((a_ptr->a_flags1 & TR1_SHOTS) && (a_ptr->a_flags1 & TR1_MIGHT))
				 || (a_ptr->pval < 0))
			{
				art_freq[CAT_BOW_XTRA] = 0;
				break;
			}

			/*
			 * If there is only one of the two, add the other one.
			 * Note this might make the artifact too powerful, so it may be
			 * cancelled out
			 */
			if ((a_ptr->a_flags1 & TR1_SHOTS) || (a_ptr->a_flags1 & TR1_MIGHT))
			{
				a_ptr->a_flags1 |= (TR1_SHOTS | TR1_MIGHT);

			}

			/*We don't have either, 50% chance of adding either one*/
			else if one_in_(2)
			{
				a_ptr->a_flags1 |= (TR1_SHOTS);
			}
			else
			{
				a_ptr->a_flags1 |= (TR1_MIGHT);
			}

			/*(Prevent this from being called twice too often*/
			art_freq[CAT_BOW_XTRA] /= 3;

			do_pval(a_ptr);
			break;
		}
		case CAT_STEALTH:
		{
			/*Not necessary to call this again*/
			if ((a_ptr->a_flags1 & TR1_STEALTH) || (a_ptr->a_flags3 & TR3_AGGRAVATE))
			{
				art_freq[CAT_STEALTH] = 0;
				break;
			}
			a_ptr->a_flags1 |= TR1_STEALTH;
			do_pval(a_ptr);
			break;
		}
		case CAT_VISION:
		{
			/*All full - Prevent this from being called again*/
			if (((a_ptr->a_flags1 & TR1_INFRA) && (a_ptr->a_flags1 & TR1_SEARCH)) ||
				(a_ptr->pval < 0))
			{
				art_freq[CAT_VISION] = 0;
				break;
			}

			/*
			 * If there is only one of the two, add the other one.
			 */
			if ((a_ptr->a_flags1 & TR1_INFRA) || (a_ptr->a_flags1 & TR1_SEARCH))
			{
				a_ptr->a_flags1 |= (TR1_INFRA | TR1_SEARCH);

			}

			/*50% chance of adding either one*/
			else if one_in_(2)
			{
				a_ptr->a_flags1 |= (TR1_INFRA);
			}
			else
			{
				a_ptr->a_flags1 |= (TR1_SEARCH);
			}
			do_pval(a_ptr);
			break;
		}
		case CAT_COMBAT:
		{
			/*50-50% chance of adding to-hit or to-damage*/
			if (one_in_(2))	add_to_hit(a_ptr, 1, 2 * MEAN_DAM_INCREMENT);
			else add_to_dam(a_ptr, 1, 2 * MEAN_DAM_INCREMENT);
			break;
		}
		case CAT_TO_AC:
		{
			add_to_ac(a_ptr, 1, 2 * MEAN_DAM_INCREMENT);
			break;
		}
		/*add base AC, base to-hit, or base-to-dam to artifact*/
		case CAT_TO_BASE:
		{
			switch (a_ptr->tval)
			{
		    	case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				case TV_DIGGING:
				{
					/* Hack -- Super-charge the damage dice */
					while (one_in_(15))  a_ptr->dd++;

					/* Hack -- Limit the damage dice to max of 9*/
					if (a_ptr->dd > 9) a_ptr->dd = 9;

					/* Hack -- Super-charge the damage sides */
					while (one_in_(15))  a_ptr->ds++;

					/* Hack -- Limit the damage sides to max of 9*/
					if (a_ptr->ds > 9) a_ptr->ds = 9;
					break;
				}
				/*add to armor*/
				case TV_HARD_ARMOR:
				case TV_SOFT_ARMOR:
				case TV_DRAG_ARMOR:
				case TV_DRAG_SHIELD:
				case TV_CLOAK:
				case TV_SHIELD:
				case TV_HELM:
				case TV_CROWN:
				case TV_BOOTS:
				case TV_GLOVES:
				{
					int super = 0;

					/* Hack -- Super-charge the armor class */
					while ((a_ptr->ac > 0) &&
					       (one_in_(15)))
					{
						/*extra bonus at higher values*/
						super += (1 + (a_ptr->ac / 10));
					}

					/* Hack -- Limit the ac supercharge to max of 9*/
					if (super > 9) super = 9;

					/* Supercharge the ac. */
					a_ptr->ac += super;

					break;
				}
				/* we are correcting reaching this by mistake*/
				default: art_freq[CAT_TO_BASE] = 0;
			}
			break;
		}
		case CAT_WEIGH_LESS:
		{

			/*reduce the weight for all but items less than 2 pounds*/
			if (a_ptr->weight < 20) art_freq[CAT_WEIGH_LESS] = 0;
			else   a_ptr->weight = (a_ptr->weight * 9) / 10;

			/*hack - we also limit how often we do this, and only once for less than 5 lbs*/
			if ((a_ptr->weight < 50) || (one_in_(2))) art_freq[CAT_TO_BASE] = 0;
			break;
		}
		case CAT_LIGHT:
		{
			/*
			 * Hack - Don't add this to LIGHT sources
			 * or make sure we don't return again if we already had it.
			 */
			if (((a_ptr->tval == TV_LIGHT) || (a_ptr->a_flags3 & TR3_LIGHT)) ||
				(a_ptr->pval < 0))
			{
				art_freq[CAT_LIGHT] = 0;
				break;
			}

			a_ptr->a_flags3 |= TR3_LIGHT;
			break;
		}
		case CAT_NATIVE:
		{
			add_one_native(a_ptr);
			break;
		}

	}
}

/*
 * Randomly select an extra ability to be added to the artifact in question.
 * XXX - This function is way too large.
 */
static void add_feature(artifact_type *a_ptr)
{
	int r;

	/* Choose a random ability using the frequency table previously defined*/
	r = choose_power_type();

	/* Add the appropriate ability */
	add_feature_aux(a_ptr, r);

	/* Now remove contradictory or redundant powers. */
	remove_contradictory(a_ptr);

	/* Adding WIS to sharp weapons always blesses them */
	if ((a_ptr->a_flags1 & TR1_WIS) && (a_ptr->pval > 0) &&
		(a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM))
	{
		a_ptr->a_flags3 |= TR3_BLESSED;
	}
}



/*
 * Try to supercharge this item by running through the list of the supercharge
 * abilities and attempting to add each in turn.  An artifact only gets one
 * chance at each of these up front (if applicable).
 */
static void try_supercharge(artifact_type *a_ptr, int final_power)
{
	bool did_supercharge = FALSE;

	/* Huge damage dice - melee weapon only */
	if (a_ptr->tval == TV_DIGGING || a_ptr->tval == TV_HAFTED ||
		a_ptr->tval == TV_POLEARM || a_ptr->tval == TV_SWORD)
	{
		if (rand_int(a_ptr->a_level) < (final_power / 10))
		{
			if (one_in_(2))
			{
				a_ptr->dd += 3 + rand_int(4);
				if (a_ptr->dd > 9) a_ptr->dd = 9;
			}
			else
			{
				a_ptr->ds += 3 + rand_int(4);
				if (a_ptr->ds > 9) a_ptr->ds = 9;
			}

			did_supercharge = TRUE;

		}
	}

	/* Bows - +3 might or +3 shots */
	if (a_ptr->tval == TV_BOW)
	{
		if (rand_int(a_ptr->a_level) < (final_power / 10))
		{
			a_ptr->a_flags1 |= TR1_SHOTS;
			a_ptr->pval = 3;
		}
		else if (rand_int(a_ptr->a_level) < (final_power / 10))
		{
			a_ptr->a_flags1 |= TR1_MIGHT;
			a_ptr->pval = 3;
		}

		did_supercharge = TRUE;
	}

	/* Big speed bonus - any item (except bows, because we can't have +10- shots/might) */
	else if (rand_int(a_ptr->a_level) < (final_power / 20))
	{
		a_ptr->a_flags1 |= TR1_SPEED;
		a_ptr->pval = 6 + rand_int(4);

		/*boots love speed.  Make sure stats don't mess it up*/
		if (a_ptr->tval == TV_BOOTS)
		{
			a_ptr->pval += damroll (3,2);
		}

		did_supercharge = TRUE;
	}
	/* Aggravation */
	if (did_supercharge)
	{
		switch (a_ptr->tval)
    	{
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
		    case TV_POLEARM:
			case TV_SWORD:
			{
					if (rand_int (150) < (final_power / 8))
				{
					a_ptr->a_flags3 |= TR3_AGGRAVATE;
				}
				break;
			}

			default:
			{
				if (rand_int (150) < (final_power / 8))
				{
					a_ptr->a_flags3 |= TR3_AGGRAVATE;
				}
				break;
			}
		}
	}
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void do_curse(artifact_type *a_ptr)
{
	if (one_in_(3))
		a_ptr->a_flags3 |= TR3_AGGRAVATE;
	if (one_in_(5))
		a_ptr->a_flags3 |= TR3_DRAIN_EXP;
	if (one_in_(7))
		a_ptr->a_flags3 |= TR3_TELEPORT;

	if ((a_ptr->pval > 0) && (one_in_(2)))
		a_ptr->pval = -a_ptr->pval;
	if ((a_ptr->to_a > 0) && (one_in_(2)))
		a_ptr->to_a = -a_ptr->to_a;
	if ((a_ptr->to_h > 0) && (one_in_(2)))
		a_ptr->to_h = -a_ptr->to_h;
	if ((a_ptr->to_d > 0) && (one_in_(4)))
		a_ptr->to_d = -a_ptr->to_d;

	if (a_ptr->a_flags3 & TR3_LIGHT_CURSE)
	{
		if (one_in_(2)) a_ptr->a_flags3 |= TR3_HEAVY_CURSE;
		return;
	}

	a_ptr->a_flags3 |= TR3_LIGHT_CURSE;

	if (one_in_(4))	a_ptr->a_flags3 |= TR3_HEAVY_CURSE;
}



/*
 * Note the three special cases (One Ring, Grond, Morgoth).
 */
static void scramble_artifact(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	artifact_type artifact_type_body;
	artifact_type *a_old = &artifact_type_body;
	object_kind *k_ptr;
	u32b activates = a_ptr->a_flags3 & TR3_ACTIVATE;
	s32b power;
	int tries;
	s16b k_idx;
	byte rarity_old, base_rarity_old;
	s16b rarity_new;
	s32b ap;
	bool curse_me = FALSE;
	u32b flags_bad = 0L;
	char buf[MAX_LEN_ART_NAME];

	/* Special cases -- don't randomize these! */
	if (strstr(a_ptr->name, "One Ring")) return;
	if (strstr(a_ptr->name, "Grond")) return;
	if (strstr(a_ptr->name, "of Morgoth")) return;

	/* Skip unused artifacts, too! */
	if (a_ptr->tval == 0) return;

	/*if there are bad flags, add them in later*/
	if (a_ptr->a_flags3 & ART_FLAGS_BAD)
	{
		flags_bad = a_ptr->a_flags3;
		flags_bad &=  ART_FLAGS_BAD;
	}

	/*randomize the name*/
	make_random_name(buf, 5, 11);

	if (!one_in_(3))
	{

		my_strcpy(a_ptr->name, format("'%^s'", buf), MAX_LEN_ART_NAME);

	}
	else
	{
	    my_strcpy(a_ptr->name, format("of %^s", buf), MAX_LEN_ART_NAME);
	}

	/* Evaluate the original artifact to determine the power level. */
	power = base_power[a_idx];

	k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

	/*paranoia - don't attempt to scramble invalid artifacts*/
	if (!k_idx) return;

	/* If it has a restricted ability then don't randomize it. */
	if (power > 10000)
	{
		return;
	}

	if (power < 0) curse_me = TRUE;

	if (a_idx >= z_info->art_spec_max)
	{
		/*
		 * Normal artifact - choose a random base item type.
		 */
		int y;
		int new_object_rarity = 0;
		alloc_entry *table = alloc_kind_table;

		/* Capture the rarity of the original base item and artifact */
		base_rarity_old = base_item_rarity[a_idx];
		rarity_old = base_art_rarity[a_idx];

		/*actually get the item*/
		choose_item(a_idx);

		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);
		k_ptr = &k_info[k_idx];

		/*
		 * Calculate the proper rarity based on the new type.  We attempt
		 * to preserve the 'effective rarity' which is equal to the
		 * artifact rarity multiplied by the base item rarity.
		 */

		/* Process probabilities */
		for (y = 0; y < alloc_kind_size; y++)
		{
			if (k_idx != table[y].index) continue;

			/*The rarity tables are divided by 100 in the prob_table*/
			new_object_rarity = MAX((100 / table[y].prob2), 1);

			break;
		}

		/* Whoops!  Just make something up*/
		if (!new_object_rarity) new_object_rarity = 1;

		/*The table is sorted by depth, just use the lowest one*/
		rarity_new = ((s16b) rarity_old * (s16b)base_rarity_old ) /
						new_object_rarity;

		if (rarity_new > 255) rarity_new = 255;

		/* Got an item - set the new rarity */
		a_ptr->a_rarity = (byte) rarity_new;

	}
	else
	{
		/* Special artifact (light source, ring, or
		   amulet).  Clear the following fields; leave
		   the rest alone. */
		a_ptr->pval = 0;
		a_ptr->to_h = a_ptr->to_d = a_ptr->to_a = 0;
		a_ptr->a_flags1 = a_ptr->a_flags2 = 0;

		/* Artifacts ignore everything */
		a_ptr->a_flags3 = (TR3_IGNORE_MASK);

		k_ptr = &k_info[k_idx];

		ap = artifact_power (a_idx);

		/*make sure there is some room to add something*/
		if ((ap * 10 / 9) > power) power = (ap * 10 / 9);

	}

	/* Got a base item. */
	/* Generate the cumulative frequency table for this item type */
	build_freq_table(a_ptr);

	/*add in the throwing flag, and possibly perfect_balance*/
	if (k_ptr->k_flags3 & (TR3_THROWING))
	{
		a_ptr->a_flags3 |= TR3_THROWING;
		if (one_in_(2)) a_ptr->a_flags3 |= TR3_PERFECT_BALANCE;
	}

	/* Copy artifact info temporarily. */
	*a_old = *a_ptr;

	/* Give this artifact a shot at being supercharged */
	try_supercharge(a_ptr, power);
	ap = artifact_power(a_idx);
	if (ap > (power * 21) / 20 + 1)
	{
		/* too powerful -- put it back */
		*a_ptr = *a_old;
	}

	/* First draft: add two abilities, then curse it three times. */
	if (curse_me)
	{
		int count;
		/* Copy artifact info temporarily. */
		*a_old = *a_ptr;

		for (count = 0; count < MAX_TRIES; count ++)
		{
			add_feature(a_ptr);
			add_feature(a_ptr);
			do_curse(a_ptr);
			do_curse(a_ptr);
			do_curse(a_ptr);
			remove_contradictory(a_ptr);
			ap = artifact_power(a_idx);

			/* Accept if it doesn't have any inhibited abilities */
			if (ap < 10000) break;
			/* Otherwise go back and try again */
			else
			{
				*a_ptr = *a_old;
			}

		}

		/* Cursed items never have any resale value */
		a_ptr->cost = 0;
	}

	else
	{

		/*
		 * Select a random set of abilities which roughly matches the
		 * original's in terms of overall power/usefulness.
		 */
		for (tries = 0; tries < MAX_TRIES; tries++)
		{

			/* Copy artifact info temporarily. */
			*a_old = *a_ptr;
			add_feature(a_ptr);
			remove_contradictory(a_ptr);
			ap = artifact_power(a_idx);

			/* CR 11/14/01 - pushed both limits up by about 5% */
			if (ap >= (power * 22 / 20))
			{
				/* too powerful -- put it back */
				*a_ptr = *a_old;

				continue;
			}

			else if (ap >= (power * 19) / 20)	/* just right */
			{
				break;
			}

		}		/* end of power selection */

		/* Set the cost proportional to the power level */
		a_ptr->cost = ap * 1000L;
	}

	if (a_ptr->cost < 0) a_ptr->cost = 0;

	/* Restore some flags */
	if (activates) a_ptr->a_flags3 |= TR3_ACTIVATE;
	if (a_idx < z_info->art_norm_max) a_ptr->a_flags3 |= TR3_INSTA_ART;

	/*add back in any bad flags*/
	a_ptr->a_flags3 |= flags_bad;

	/*
	 * Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	 * too lazy to find out which ones need it and which ones don't.
	 */
	if (a_ptr->pval)
		a_ptr->a_flags3 |= TR3_HIDE_TYPE;

}


/*
 * Return nonzero if there is an acceptable minimum set of random artifacts.
 * With the overloaded weighting of randart themes until the minimum is
 * set, it is highly unlikely this routine will fail. -JG
 */
static bool artifacts_acceptable(void)
{
	byte i;

	for (i = 0; i < ART_THEME_MAX; i++)
	{
		if (art_theme_freq[i] >= MIN_ENFORCEMENT) return (FALSE);
	}

	/*we have a good set*/
	return (TRUE);
}

static errr scramble(void)
{
	/*Prevent making come unacceptable things for artifacts such as arrows*/
	object_generation_mode = OB_GEN_MODE_RANDART;

	while (TRUE)
	{

		int a_idx;

		/* Generate all the artifacts. */
		for (a_idx = 1; a_idx < z_info->art_norm_max; a_idx++)
		{
			scramble_artifact(a_idx);
		}

		if (artifacts_acceptable()) break;

	}

	/*Re-Set things*/
	object_generation_mode = OB_GEN_MODE_NORMAL;

	/* Success */
	return (0);
}


static errr do_randart_aux(bool full)
{
	errr result;


	if (full)
	{
		/*note: we are failing here*/

		/* Randomize the artifacts */
		if ((result = scramble()) != 0) return (result);
	}

	/* Success */
	return (0);
}

/*build the names table at the beginning on the game*/
void build_randart_tables(void)
{

	/* Allocate the "kinds" array */
	kinds = C_ZNEW(z_info->art_norm_max, s16b);

	/* Initialize the monster power ratings */
	(void)init_mon_power();

	/*build the frequency tables*/
	build_art_freq_table();

}

/*free the randart tables at the end of the game*/
void free_randart_tables(void)
{

	FREE(kinds);
}

/*
 * Randomize the artifacts
 *
 * The full flag toggles between just randomizing the names and
 * complete randomization of the artifacts.
 */
errr do_randart(u32b randart_seed, bool full)
{
	errr err;

	/* Prepare to use the Angband "simple" RNG. */
	Rand_value = randart_seed;
	Rand_quick = TRUE;

	/* Only do all the following if full randomization requested */
	if (full)
	{

		/* Allocate the various "original powers" arrays */
		base_power = C_ZNEW(z_info->art_norm_max, s32b);
		base_item_level = C_ZNEW(z_info->art_norm_max, byte);
		base_item_rarity = C_ZNEW(z_info->art_norm_max, byte);
		base_art_rarity = C_ZNEW(z_info->art_norm_max, byte);

		/* Store the original power ratings */
		store_base_power();

		/*adjust the randart frequencies to enforce minimum values*/
		adjust_art_freq_table();

	}

	/* Generate the random artifact (names) */
	err = do_randart_aux(full);

	/* Only do all the following if full randomization requested */
	if (full)
	{

		/* Free the "original powers" arrays */
		FREE(base_power);
		FREE(base_item_level);
		FREE(base_item_rarity);
		FREE(base_art_rarity);
	}

	/* When done, resume use of the Angband "complex" RNG. */
	Rand_quick = FALSE;

	return (err);
}


/*
 * Attempt to create an artifact, returns false if there is no available slot.
 * This function assumes that an object suitable for an artifact is there, or if
 * it is blank it will make one.
 */
bool make_one_randart(object_type *o_ptr, int art_power, bool tailored)
{
	char tmp[MAX_LEN_ART_NAME];
	int i, tries;
	int a_idx = 0;
	s32b ap;
	object_kind *k_ptr;
	artifact_type *a_ptr;
	u32b f1, f2, f3;
	artifact_type artifact_type_body;
	artifact_type *a_old = &artifact_type_body;;

	/*find an artifact slot, return false if none are available*/
	for (i = z_info->art_norm_max; i < z_info->art_max; i++)
	{
		a_ptr = &a_info[i];

		/* This slot is already being used */
		if ((a_ptr->tval + a_ptr->sval) > 0) continue;

		/*this one slot is reserved for quest artifacts*/
		if (i == QUEST_ART_SLOT) continue;

		/*found a space to create an artifact*/
		a_idx = i;

		break;
	}

	if (!a_idx) return (FALSE);

	/*point to the artifact*/
	a_ptr = &a_info[a_idx];

	/* Clear the artifact record */
	(void)WIPE(a_ptr, artifact_type);

	/*point to the object type*/
	k_ptr = &k_info[o_ptr->k_idx];

	/*paranoia - make sure we have a suitable object_type*/
	if (can_be_randart(o_ptr))
	{
		/*prepare a basic, non-magic artifact template based on the object kind*/
		artifact_prep(o_ptr->k_idx, a_idx);

		/*
	 	 * bad hack - because we didn't determine object type based on artifact
		 * power, make sure we have room to add power.
	 	 */
		ap = artifact_power(a_idx);
		if (ap > art_power) art_power = ap * 6 / 5;

	}

	/*blank template - we need an object*/
	else if (!o_ptr->k_idx)
	{

		s16b k_idx = 0;
		byte old_mode = object_generation_mode;

		byte theme;

		/*Prevent making some unacceptable things for artifacts such as arrows*/
		object_generation_mode = OB_GEN_MODE_RANDART;

		/*prepare the object template*/
		object_wipe(o_ptr);

		/*get the obejct number test it for appropriate power*/
		while (TRUE)
	   	{
			theme = get_theme();

			/*hack - we con't need any more artifact digging tools*/
			if (theme != DROP_TYPE_DIGGING) break;
		}

		/*prepare the object generation level for a specific theme*/
		prep_object_theme(theme);

		k_idx = 0;

		while (!k_idx) k_idx = get_obj_num(MIN(object_level + 20, MAX_DEPTH - 1));

		/* Clear restriction */
		get_obj_num_hook = NULL;

		/* Un-do the object theme */
		get_obj_num_prep();

		/* Clear the artifact record */
		(void)WIPE(a_ptr, artifact_type);

		/*prepare a basic, non-magic artifact template based on the object kind*/
		artifact_prep(k_idx, a_idx);

		ap = artifact_power(a_idx);

		/*make sure there is room to add stuff*/
		if (ap > art_power) art_power = ap * 6 / 5;

		/*Re-Set the mode*/
		object_generation_mode = old_mode;

		/*point to the object type (in case it is needed again*/
		k_ptr = &k_info[o_ptr->k_idx];
	}

	/*All others fail, but this shouldn't happen*/
	else return(FALSE);

	/*mark the artifact*/
	o_ptr->art_num = a_idx;

	/*add in the throwing flag, and possibly perfect_balance*/
	if (k_ptr->k_flags3 & (TR3_THROWING))
	{
		a_ptr->a_flags3 |= TR3_THROWING;
		if (one_in_(2)) a_ptr->a_flags3 |= TR3_PERFECT_BALANCE;
	}

	/*Start artifact naming with a blank name*/
	tmp[0] = '\0';

	/*possibly allow character to name the artifact*/
	if (tailored)
	{
		char ask_first[40];
		strnfmt(ask_first, sizeof(ask_first), "Name your artifact? ");
		if (get_check(ask_first))
		{
			char buf[MAX_LEN_ART_NAME];

			/*start with a blank name*/
			buf[0] = '\0';

			if (get_string("Enter a name for your artifact: ", buf, sizeof(buf)))
			{

				/*The additional check is because players sometimes hit return accidentally*/
				if (strlen(buf) > 0) my_strcpy(tmp, format("'%^s'", buf), MAX_LEN_ART_NAME);
			}
		}
	}
	/*If none selected, make one at random*/
	if ((!tailored) || (tmp[0] == '\0'))
	{
		char buf[MAX_LEN_ART_NAME];

		/*randomize the name*/
		make_random_name(buf, 5, 11);

		/*Capitalize the name*/
		buf[0] = toupper((unsigned char)buf[0]);

		if (!one_in_(3))
		{

			my_strcpy(tmp, format("'%^s'", buf), MAX_LEN_ART_NAME);

		}
		else
		{
	    	my_strcpy(tmp, format("of %^s", buf), MAX_LEN_ART_NAME);
		}
	}

	/*copy the name*/
	my_strcpy(a_ptr->name, format("%s", tmp), MAX_LEN_ART_NAME);

	/* Generate the cumulative frequency table for this item type */
	build_freq_table(a_ptr);

	/*save any base object template flags before artifact creation*/
	f1 = k_ptr->k_flags1;
	f2 = k_ptr->k_flags2;
	f3 = k_ptr->k_flags3;

	/*the artifacts are always going to be created on demand, no need for rarity*/
	a_ptr->a_rarity = MIN(art_power, 255);

	/*The level affects the power of the object*/
	a_ptr->a_level = object_level;

	/* Copy artifact info temporarily. */
	*a_old = *a_ptr;

	/* Give this artifact a shot at being supercharged */
	try_supercharge(a_ptr, art_power);
	ap = artifact_power(a_idx);
	if (ap >= (art_power * 21 / 20)+ 1)
	{
		/* too powerful -- put it back */
		*a_ptr = *a_old;
	}

	/* Restore some flags */
	a_ptr->a_flags1 |= f1;
	a_ptr->a_flags2 |= f2;
	a_ptr->a_flags3 |= f3;

	/*
	 * Select a random set of abilities which roughly matches the
	 * original's in terms of overall power/usefulness.
	 */
	for (tries = 0; tries < MAX_TRIES; tries++)
	{
		/* Copy artifact info temporarily. */
		*a_old = *a_ptr;

		add_feature(a_ptr);
		ap = artifact_power(a_idx);
		remove_contradictory(a_ptr);

		if (ap >= (art_power * 21 / 20) + 1)
		{
			/* too powerful -- put it back */
			*a_ptr = *a_old;

			continue;
		}

		else if (ap >= (art_power * 19) / 20 + 1)	/* just right */
		{
			break;
		}
	}

	/*Make sure certain classes can use the artifact they get without penalty*/
	if (tailored)
	{
		switch (a_ptr->tval)
		{
			/*hack - automatically bless weapons for priests*/
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (cp_ptr->flags & CF_BLESS_WEAPON) a_ptr->a_flags3 |= TR3_BLESSED;
				break;
			}
			/*make sure mage spellcasters get free action or dex boost*/
			case TV_GLOVES:
			{
				if (cp_ptr->flags & CF_CUMBER_GLOVE)
				{
					/* Limit to legal glove types */
					if (!((a_ptr->a_flags3 & (TR3_FREE_ACT)) ||
							  (a_ptr->a_flags1 & (TR1_DEX))))
					{
						a_ptr->a_flags3 |= TR3_FREE_ACT;
					}
				}
				break;
			}
			default: break;
		}
	}

	ap = artifact_power(a_idx);

	a_ptr->cost = ap * 1000L;

	if (a_ptr->cost < 0) a_ptr->cost = 0;

	/*
	 * Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	 * too lazy to find out which ones need it and which ones don't.
	 */
	if (a_ptr->pval)
		a_ptr->a_flags3 |= TR3_HIDE_TYPE;

	/* Hack -- Mark the artifact as "created" */
	a_ptr->a_cur_num = 1;
	a_ptr->a_max_num = 1;

	/*turn the object into an artifact*/
	object_into_artifact(o_ptr, a_ptr);

	/* Set the good item flag */
	good_item_flag = TRUE;

	return (TRUE);
}

/*
 * Create a quest artifact
 */
void make_quest_artifact(int lev)
{
	int a_idx = QUEST_ART_SLOT;
	artifact_type *a_ptr = &a_info[a_idx];
	s16b k_idx = 0;
	char buf[MAX_LEN_ART_NAME];

	byte old_mode = object_generation_mode;

	/* Clear the artifact record */
	artifact_wipe(QUEST_ART_SLOT, TRUE);

	/*Prevent making some unacceptable things for artifacts such as arrows*/
	object_generation_mode = OB_GEN_MODE_RANDART;

	while (!k_idx)
	{
		byte theme = get_theme();

		/*hack - we aren't ready for randart amulets and rings yet*/
		if (theme == DROP_TYPE_DIGGING) continue;

		/*prepare the object generation level for a specific theme*/
		prep_object_theme(theme);

		k_idx = get_obj_num(MIN(object_level + 15, MAX_DEPTH - 1));

		/* Clear restriction */
		get_obj_num_hook = NULL;

		/* Un-do the object theme */
		get_obj_num_prep();

	}

	/*Re-Set the mode*/
	object_generation_mode = old_mode;

	/*Not sure if this is needed, but you never know*/
	a_ptr->a_rarity = 1;
	a_ptr->a_level = lev;
	a_ptr->cost = 0;

	/*Hack - this may prevent from being accidentally created*/
	a_ptr->a_cur_num = 1;
	a_ptr->a_max_num = 1;

	/*prepare a basic, non-magic artifact template based on the object kind*/
	artifact_prep(k_idx, a_idx);

	/*randomize the name*/
	make_random_name(buf, 5, 11);

	/*Capitalize the name*/
	buf[0] = toupper((unsigned char)buf[0]);

	if (!one_in_(3))
	{
		my_strcpy(a_ptr->name, format("'%^s'", buf), MAX_LEN_ART_NAME);
	}
	else
	{
	    my_strcpy(a_ptr->name, format("of %^s", buf), MAX_LEN_ART_NAME);
	}

	return;
}


/*
 * Attempt to create an artifact, returns false if there is no available slot.
 * This function assumes that an object suitable for an artifact is there, or if
 * it is blank it will make one.
 */
void create_quest_artifact(object_type *o_ptr)
{
	int a_idx = QUEST_ART_SLOT;
	artifact_type *a_ptr = &a_info[a_idx];

	o_ptr->k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

	/*make the base item*/
	object_prep(o_ptr, o_ptr->k_idx);

	/* Hack -- Mark the artifact as "created" */
	a_ptr->a_cur_num = 1;
	a_ptr->a_max_num = 1;

	/*mark the artifact*/
	o_ptr->art_num = a_idx;

	/*make the object into an artifact*/
	object_into_artifact(o_ptr, a_ptr);

	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known, and as a quest item */
	o_ptr->ident |= (IDENT_MENTAL | IDENT_QUEST);

	/* Set the good item flag */
	good_item_flag = TRUE;

	return;
}



/*
 * Wipe an artifact clean.
 * Check first to make sure it is a randart.
 */
void artifact_wipe(int a_idx, bool quest_art_wipe)
{
	artifact_type *a_ptr = &a_info[a_idx];

	/*Hack - don't wipe any standard artifacts*/
	if (a_idx < z_info->art_norm_max) return;

	/*We wipe these only when the quest fails/succeeds*/
	if ((a_idx == QUEST_ART_SLOT) && (!quest_art_wipe)) return;

	/* Wipe the structure */
	(void)WIPE(a_ptr, artifact_type);

	/*terminate the string*/
	a_ptr->name[0] = '\0';
}

/*
 * Return true if an artifact can turn into a randart.
 * Note rings, amulets, and LIGHT sources are currently not supported.
 * This is because there is no clear base object type for additional "special" artifacts,
 * plus object flavor would have to be handled for rings and amulets.
 * Someday I would like to resolve this. -JG
 */
bool can_be_randart(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr-> k_idx];

	switch (k_ptr->tval)
	{
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
		default: return (FALSE);
	}
}
