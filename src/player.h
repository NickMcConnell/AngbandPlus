/*
 * Player constants
 */
#define PY_MAX_EXP	99999999L	/* Maximum exp */
#define PY_MAX_GOLD	999999999L	/* Maximum gold */
#define PY_MAX_LEVEL	50			/* Maximum level */

/*
 * Player "food" crucial values
 */
#define PY_FOOD_MAX			15000   /* Food value (Bloated) */
#define PY_FOOD_FULL	10000   /* Food value (Normal) */
#define PY_FOOD_ALERT	2000	/* Food value (Hungry) */
#define PY_FOOD_WEAK	1000	/* Food value (Weak) */
#define PY_FOOD_FAINT	500			/* Food value (Fainting) */
#define PY_FOOD_STARVE	100			/* Food value (Starving) */

/*
 * Player regeneration constants
 */
#define PY_REGEN_NORMAL		197			/* Regen factor*2^16 when full */
#define PY_REGEN_WEAK		98			/* Regen factor*2^16 when weak */
#define PY_REGEN_FAINT		33			/* Regen factor*2^16 when fainting */
#define PY_REGEN_HPBASE		1442	/* Min amount hp regen*2^16 */
#define PY_REGEN_MNBASE		524			/* Min amount mana regen*2^16 */

/*
 * Maximum number of "normal" pack slots, and the index of the "overflow"
 * slot, which can hold an item, but only temporarily, since it causes the
 * pack to "overflow", dropping the "last" item onto the ground.	Since this
 * value is used as an actual slot, it must be less than "INVEN_WIELD" (below).
 * Note that "INVEN_PACK" is probably hard-coded by its use in savefiles, and
 * by the fact that the screen can only show 23 items plus a one-line prompt.
 */
#define INVEN_PACK			23

/*
 * Indexes used for various "equipment" slots (hard-coded by savefiles, etc).
 */
#define INVEN_WIELD			24
#define INVEN_BOW	25
#define INVEN_LEFT	26
#define INVEN_RIGHT	27
#define INVEN_NECK	28
#define INVEN_LITE	29
#define INVEN_BODY	30
#define INVEN_OUTER	31
#define INVEN_ARM	32
#define INVEN_HEAD	33
#define INVEN_HANDS	34
#define INVEN_FEET	35

/*
 * Total number of inventory slots (hard-coded).
 */
#define INVEN_TOTAL	36


/*
 * A "stack" of items is limited to less than 100 items (hard-coded).
 */
#define MAX_STACK_SIZE				100


/*
 * Indexes of the various "stats" (hard-coded by savefiles, etc).
 */
#define A_STR	 0
#define A_INT	 1
#define A_WIS	 2
#define A_DEX	 3
#define A_CON	 4
#define A_CHR	 5

/*
 * Player sex constants (hard-coded by save-files, arrays, etc)
 */
#define SEX_FEMALE			0
#define SEX_MALE				1

/*
 * Player race constants (hard-coded by save-files, arrays, etc)
 */
#define RACE_HUMAN			0
#define RACE_HALF_ELF	1
#define RACE_ELF				2
#define RACE_HOBBIT			3
#define RACE_GNOME			4
#define RACE_DWARF			5
#define RACE_HALF_ORC	6
#define RACE_HALF_TROLL 7
#define RACE_AMBERITE	8
#define RACE_HIGH_ELF	9
#define RACE_BARBARIAN 10
#define RACE_HALF_OGRE 11
#define RACE_HALF_GIANT	12
#define RACE_HALF_TITAN	13
#define RACE_CYCLOPS		14
#define RACE_YEEK		15
#define RACE_KLACKON		16
#define RACE_KOBOLD		17
#define RACE_NIBELUNG	18
#define RACE_DRACONIAN	20
#define RACE_MIND_FLAYER	21
#define RACE_IMP			22
#define RACE_GOLEM		23
#define RACE_SKELETON	24
#define RACE_ZOMBIE		25
#define RACE_VAMPIRE		26
#define RACE_SPECTRE		27
#define RACE_SPRITE		28
#define RACE_BEASTMAN	29
#define RACE_FIEND		30


/*
 * Player class constants (hard-coded by save-files, arrays, etc)
 */
#define CLASS_WARRIOR	0
#define CLASS_MAGE			1
#define CLASS_PRIEST	2
#define CLASS_ROGUE			3
#define CLASS_RANGER	4
#define CLASS_PALADIN	5
#define CLASS_WARRIOR_MAGE 6
#define CLASS_CHAOS_WARRIOR 7
#define CLASS_MONK 8
#define CLASS_MINDCRAFTER 9
#define CLASS_HIGH_MAGE 10


/*
 * Player realm constants (hard-coded by save-files, arrays, etc)
 */
#define REALM_LIFE			1
#define REALM_SORCERY		2
#define REALM_NATURE			3
#define REALM_CHAOS			4
#define REALM_DEATH			5
#define REALM_TRUMP			6
#define REALM_ARCANE			7

