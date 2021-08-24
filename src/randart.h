#ifndef RANDART_H
#define RANDART_H

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


#endif // RANDART_H
