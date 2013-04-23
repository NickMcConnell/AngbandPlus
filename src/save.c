#define SAVE_C
/* File: save.c */

/* Purpose: interact with savefiles */

#include "angband.h"
#include "loadsave.h"


/*
 * Don't use hard-coded values for the version, so that the game can create
 * a save file as if it was an older version if desired.
 */
/*
 * Savefile version
 */
byte sf_major;			/* Savefile's "version_major" */
byte sf_minor;			/* Savefile's "version_minor" */
byte sf_patch;			/* Savefile's "version_patch" */
byte sf_extra;			/* Savefile's "version_extra" */

u16b sf_flags;			/* Savefile's "version flags" */

/*
 * Savefile information
 */
u32b sf_xtra;			/* Operating system info */
u32b sf_when;			/* Time when savefile created */
u16b sf_lives;			/* Number of past "lives" with this file */
u16b sf_saves;			/* Number of "saves" during this life */

/*
 * The table of version flags which alter the object table.
 * This is currently a flag, but may need to change to an index as the
 * save file version grows.
 */
static u32b object_version[] =
{
#ifdef SF_K_INFO_1
	SF_K_INFO_1,
#else
	0,
#endif
	0
};

/*
 * The table of object indices for the above indices in the same order.
 */
static s16b object_table[][2] =
{
	{OBJ_MAX_DISTRO,	525},
	{OBJ_NOTHING,	0},
	{OBJ_FOOD_BLINDNESS,	1},
	{OBJ_FOOD_PARANOIA,	2},
	{OBJ_FOOD_CONFUSION,	3},
	{OBJ_FOOD_HALLUCINATION,	4},
	{OBJ_FOOD_CURE_POISON,	5},
	{OBJ_FOOD_CURE_BLINDNESS,	6},
	{OBJ_FOOD_CURE_PARANOIA,	7},
	{OBJ_FOOD_CURE_CONFUSION,	8},
	{OBJ_FOOD_DEC_STR,	9},
	{OBJ_FOOD_UNHEALTH,	10},
	{OBJ_FOOD_RES_CON,	11},
	{OBJ_FOOD_RESTORING,	12},
	{OBJ_FOOD_DEC_INT,	13},
	{OBJ_FOOD_DEC_WIS,	14},
	{OBJ_FOOD_POISON,	15},
	{OBJ_FOOD_SICKNESS,	16},
	{OBJ_FOOD_PARALYSIS,	17},
	{OBJ_FOOD_RES_STR,	18},
	{OBJ_FOOD_DISEASE,	19},
	{OBJ_FOOD_CURE_SERIOUS,	20},
	{OBJ_RATION_OF_FOOD,	21},
	{OBJ_HARD_BISCUIT,	22},
	{OBJ_STRIP_OF_VENISON,	23},
	{OBJ_SLIME_MOLD,	24},
	{OBJ_PIECE_OF_ELVISH_WAYBREAD,	25},
	{OBJ_PINT_OF_FINE_ALE,	26},
	{OBJ_BOTTLE_OF_FINE_WINE,	27},
	{OBJ_UNKNOWN,	28},
	{OBJ_BROKEN_DAGGER,	30},
	{OBJ_BASTARD_SWORD,	31},
	{OBJ_SCIMITAR,	32},
	{OBJ_TULWAR,	33},
	{OBJ_BROAD_SWORD,	34},
	{OBJ_SHORT_SWORD,	35},
	{OBJ_BLADE_OF_CHAOS,	36},
	{OBJ_TWO_HANDED_SWORD,	37},
	{OBJ_MAIN_GAUCHE,	38},
	{OBJ_CUTLASS,	39},
	{OBJ_EXECUTIONERS_SWORD,	40},
	{OBJ_KATANA,	41},
	{OBJ_LONG_SWORD,	42},
	{OBJ_DAGGER,	43},
	{OBJ_RAPIER,	44},
	{OBJ_SABRE,	45},
	{OBJ_SMALL_SWORD,	46},
	{OBJ_BROKEN_SWORD,	47},
	{OBJ_BALL_AND_CHAIN,	48},
	{OBJ_WHIP,	49},
	{OBJ_FLAIL,	50},
	{OBJ_TWO_HANDED_FLAIL,	51},
	{OBJ_MORNING_STAR,	52},
	{OBJ_MACE,	53},
	{OBJ_QUARTERSTAFF,	54},
	{OBJ_WAR_HAMMER,	55},
	{OBJ_LEAD_FILLED_MACE,	56},
	{OBJ_MACE_OF_DISRUPTION,	57},
	{OBJ_LUCERNE_HAMMER,	58},
	{OBJ_BEAKED_AXE,	59},
	{OBJ_GLAIVE,	60},
	{OBJ_HALBERD,	61},
	{OBJ_AWL_PIKE,	62},
	{OBJ_PIKE,	63},
	{OBJ_SPEAR,	64},
	{OBJ_TRIDENT,	65},
	{OBJ_LANCE,	66},
	{OBJ_GREAT_AXE,	67},
	{OBJ_BATTLE_AXE,	68},
	{OBJ_LOCHABER_AXE,	69},
	{OBJ_BROAD_AXE,	70},
	{OBJ_SCYTHE,	71},
	{OBJ_SCYTHE_OF_SLICING,	72},
	{OBJ_SHORT_BOW,	73},
	{OBJ_LONG_BOW,	74},
	{OBJ_LIGHT_CROSSBOW,	75},
	{OBJ_HEAVY_CROSSBOW,	76},
	{OBJ_SLING,	77},
	{OBJ_ARROW,	78},
	{OBJ_SEEKER_ARROW,	79},
	{OBJ_BOLT,	80},
	{OBJ_SEEKER_BOLT,	81},
	{OBJ_ROUNDED_PEBBLE,	82},
	{OBJ_IRON_SHOT,	83},
	{OBJ_SHOVEL,	84},
	{OBJ_GNOMISH_SHOVEL,	85},
	{OBJ_DWARVEN_SHOVEL,	86},
	{OBJ_PICK,	87},
	{OBJ_ORCISH_PICK,	88},
	{OBJ_DWARVEN_PICK,	89},
	{OBJ_ELVEN_CLOAK,	90},
	{OBJ_SOFT_LEATHER_BOOTS,	91},
	{OBJ_HARD_LEATHER_BOOTS,	92},
	{OBJ_METAL_SHOD_BOOTS,	93},
	{OBJ_HARD_LEATHER_CAP,	94},
	{OBJ_METAL_CAP,	95},
	{OBJ_IRON_HELM,	96},
	{OBJ_STEEL_HELM,	97},
	{OBJ_IRON_CROWN,	98},
	{OBJ_GOLDEN_CROWN,	99},
	{OBJ_JEWEL_ENCRUSTED_CROWN,	100},
	{OBJ_ROBE,	101},
	{OBJ_FILTHY_RAG,	102},
	{OBJ_SOFT_LEATHER_ARMOUR,	103},
	{OBJ_SOFT_STUDDED_LEATHER,	104},
	{OBJ_HARD_LEATHER_ARMOUR,	105},
	{OBJ_HARD_STUDDED_LEATHER,	106},
	{OBJ_LEATHER_SCALE_MAIL,	107},
	{OBJ_METAL_SCALE_MAIL,	108},
	{OBJ_CHAIN_MAIL,	109},
	{OBJ_RUSTY_CHAIN_MAIL,	110},
	{OBJ_AUGMENTED_CHAIN_MAIL,	111},
	{OBJ_BAR_CHAIN_MAIL,	112},
	{OBJ_METAL_BRIGANDINE_ARMOUR,	113},
	{OBJ_PARTIAL_PLATE_ARMOUR,	114},
	{OBJ_METAL_LAMELLAR_ARMOUR,	115},
	{OBJ_FULL_PLATE_ARMOUR,	116},
	{OBJ_RIBBED_PLATE_ARMOUR,	117},
	{OBJ_ADAMANTITE_PLATE_MAIL,	118},
	{OBJ_MITHRIL_PLATE_MAIL,	119},
	{OBJ_MITHRIL_CHAIN_MAIL,	120},
	{OBJ_DOUBLE_CHAIN_MAIL,	121},
	{OBJ_SHIELD_OF_DEFLECTION,	122},
	{OBJ_CLOAK,	123},
	{OBJ_SHADOW_CLOAK,	124},
	{OBJ_LEATHER_GLOVES,	125},
	{OBJ_GAUNTLETS,	126},
	{OBJ_CESTI,	127},
	{OBJ_SMALL_LEATHER_SHIELD,	128},
	{OBJ_LARGE_LEATHER_SHIELD,	129},
	{OBJ_SMALL_METAL_SHIELD,	130},
	{OBJ_LARGE_METAL_SHIELD,	131},
	{OBJ_RING_INC_STR,	132},
	{OBJ_RING_INC_DEX,	133},
	{OBJ_RING_INC_CON,	134},
	{OBJ_RING_INC_INT,	135},
	{OBJ_RING_SPEED,	136},
	{OBJ_RING_SEARCHING,	137},
	{OBJ_RING_TELEPORTATION,	138},
	{OBJ_RING_SLOW_DIGESTION,	139},
	{OBJ_RING_RES_FIRE,	140},
	{OBJ_RING_RES_COLD,	141},
	{OBJ_RING_LEVITATION,	142},
	{OBJ_RING_RES_POISON,	143},
	{OBJ_RING_FREE_ACTION,	144},
	{OBJ_RING_DEC_STR,	145},
	{OBJ_RING_FIRE,	146},
	{OBJ_RING_ACID,	147},
	{OBJ_RING_ICE,	148},
	{OBJ_RING_WOE,	149},
	{OBJ_RING_DEC_INT,	150},
	{OBJ_RING_DAMAGE,	151},
	{OBJ_RING_ACCURACY,	152},
	{OBJ_RING_PROTECTION,	153},
	{OBJ_RING_AGGRAVATE_MONSTER,	154},
	{OBJ_RING_SEE_INVIS,	155},
	{OBJ_RING_SUSTAIN_STR,	156},
	{OBJ_RING_SUSTAIN_INT,	157},
	{OBJ_RING_SUSTAIN_WIS,	158},
	{OBJ_RING_SUSTAIN_CON,	159},
	{OBJ_RING_SUSTAIN_DEX,	160},
	{OBJ_RING_SUSTAIN_CHR,	161},
	{OBJ_RING_SLAYING,	162},
	{OBJ_AMULET_BRILLIANCE,	163},
	{OBJ_AMULET_INC_CHR,	164},
	{OBJ_AMULET_SEARCHING,	165},
	{OBJ_AMULET_TELEPORTATION,	166},
	{OBJ_AMULET_SLOW_DIGESTION,	167},
	{OBJ_AMULET_RES_ACID,	168},
	{OBJ_AMULET_ADORNMENT,	169},
	{OBJ_AMULET_THE_MAGI,	171},
	{OBJ_AMULET_DOOM,	172},
	{OBJ_SCROLL_ENCHANT_WEAPON_TO_HIT,	173},
	{OBJ_SCROLL_ENCHANT_WEAPON_TO_DAM,	174},
	{OBJ_SCROLL_ENCHANT_ARMOUR,	175},
	{OBJ_SCROLL_IDENTIFY,	176},
	{OBJ_SCROLL_STAR_IDENTIFY,	177},
	{OBJ_SCROLL_RUMOUR,	178},
	{OBJ_SCROLL_CHAOS,	179},
	{OBJ_SCROLL_REMOVE_CURSE,	180},
	{OBJ_SCROLL_LIGHT,	181},
	{OBJ_SCROLL_FIRE,	182},
	{OBJ_SCROLL_ICE,	183},
	{OBJ_SCROLL_SUMMON_MONSTER,	184},
	{OBJ_SCROLL_PHASE_DOOR,	185},
	{OBJ_SCROLL_TELEPORTATION,	186},
	{OBJ_SCROLL_TELEPORT_LEVEL,	187},
	{OBJ_SCROLL_MONSTER_CONFUSION,	188},
	{OBJ_SCROLL_MAGIC_MAPPING,	189},
	{OBJ_SCROLL_RUNE_OF_PROTECTION,	190},
	{OBJ_SCROLL_STAR_REMOVE_CURSE,	191},
	{OBJ_SCROLL_TREASURE_DETECTION,	192},
	{OBJ_SCROLL_OBJECT_DETECTION,	193},
	{OBJ_SCROLL_TRAP_DETECTION,	194},
	{OBJ_SCROLL_DOOR_STAIR_LOCATION,	197},
	{OBJ_SCROLL_ACQUIREMENT,	198},
	{OBJ_SCROLL_STAR_ACQUIREMENT,	199},
	{OBJ_SCROLL_MASS_GENOCIDE,	200},
	{OBJ_SCROLL_DETECT_INVIS,	201},
	{OBJ_SCROLL_AGGRAVATE_MONSTER,	202},
	{OBJ_SCROLL_TRAP_CREATION,	203},
	{OBJ_SCROLL_TRAP_DOOR_DESTRUCTION,	204},
	{OBJ_SCROLL_ARTIFACT_CREATION,	205},
	{OBJ_SCROLL_RECHARGING,	206},
	{OBJ_SCROLL_GENOCIDE,	207},
	{OBJ_SCROLL_DARKNESS,	208},
	{OBJ_SCROLL_PROTECTION_FROM_EVIL,	209},
	{OBJ_SCROLL_SATISFY_HUNGER,	210},
	{OBJ_SCROLL_DISPEL_UNDEAD,	211},
	{OBJ_SCROLL_STAR_ENCHANT_WEAPON,	212},
	{OBJ_SCROLL_CURSE_WEAPON,	213},
	{OBJ_SCROLL_STAR_ENCHANT_ARMOUR,	214},
	{OBJ_SCROLL_CURSE_ARMOUR,	215},
	{OBJ_SCROLL_SUMMON_UNDEAD,	216},
	{OBJ_SCROLL_BLESSING,	217},
	{OBJ_SCROLL_HOLY_CHANT,	218},
	{OBJ_SCROLL_HOLY_PRAYER,	219},
	{OBJ_SCROLL_WORD_OF_RECALL,	220},
	{OBJ_SCROLL_STAR_DESTRUCTION,	221},
	{OBJ_POTION_SLIME_MOLD_JUICE,	222},
	{OBJ_POTION_APPLE_JUICE,	223},
	{OBJ_POTION_WATER,	224},
	{OBJ_POTION_INC_STR,	225},
	{OBJ_POTION_DEC_STR,	226},
	{OBJ_POTION_RES_STR,	227},
	{OBJ_POTION_INC_INT,	228},
	{OBJ_POTION_DEC_INT,	229},
	{OBJ_POTION_RES_INT,	230},
	{OBJ_POTION_INC_WIS,	231},
	{OBJ_POTION_DEC_WIS,	232},
	{OBJ_POTION_RES_WIS,	233},
	{OBJ_POTION_INC_CHR,	234},
	{OBJ_POTION_DEC_CHR,	235},
	{OBJ_POTION_RES_CHR,	236},
	{OBJ_POTION_CURING,	237},
	{OBJ_POTION_INVULNERABILITY,	238},
	{OBJ_POTION_NEW_LIFE,	239},
	{OBJ_POTION_CURE_SERIOUS,	240},
	{OBJ_POTION_CURE_CRITICAL,	241},
	{OBJ_POTION_HEALING,	242},
	{OBJ_POTION_INC_CON,	243},
	{OBJ_POTION_EXPERIENCE,	244},
	{OBJ_POTION_SLEEP,	245},
	{OBJ_POTION_BLINDNESS,	246},
	{OBJ_POTION_BOOZE,	247},
	{OBJ_POTION_POISON,	248},
	{OBJ_POTION_SPEED,	249},
	{OBJ_POTION_SLOWNESS,	250},
	{OBJ_POTION_INC_DEX,	251},
	{OBJ_POTION_RES_DEX,	252},
	{OBJ_POTION_RES_CON,	253},
	{OBJ_POTION_LOSE_MEMORIES,	254},
	{OBJ_POTION_SALT_WATER,	255},
	{OBJ_POTION_ENLIGHTENMENT,	256},
	{OBJ_POTION_HEROISM,	257},
	{OBJ_POTION_BERSERK_STR,	258},
	{OBJ_POTION_BOLDNESS,	259},
	{OBJ_POTION_RES_LIFE_LEVELS,	260},
	{OBJ_POTION_RES_HEAT,	261},
	{OBJ_POTION_RES_COLD,	262},
	{OBJ_POTION_DETECT_INVIS,	263},
	{OBJ_POTION_SLOW_POISON,	264},
	{OBJ_POTION_NEUTRALIZE_POISON,	265},
	{OBJ_POTION_RES_MANA,	266},
	{OBJ_POTION_INFRA_VISION,	267},
	{OBJ_POTION_RESISTANCE,	268},
	{OBJ_WAND_LIGHT,	269},
	{OBJ_WAND_TAME_MONSTER,	270},
	{OBJ_WAND_COLD_BOLT,	271},
	{OBJ_WAND_FIRE_BOLT,	272},
	{OBJ_WAND_STONE_TO_MUD,	273},
	{OBJ_WAND_POLYMORPH,	274},
	{OBJ_WAND_HEAL_MONSTER,	275},
	{OBJ_WAND_HASTE_MONSTER,	276},
	{OBJ_WAND_SLOW_MONSTER,	277},
	{OBJ_WAND_CONFUSE_MONSTER,	278},
	{OBJ_WAND_SLEEP_MONSTER,	279},
	{OBJ_WAND_DRAIN_LIFE,	280},
	{OBJ_WAND_TRAP_DOOR_DESTRUCTION,	281},
	{OBJ_WAND_MAGIC_MISSILE,	282},
	{OBJ_WAND_CLONE_MONSTER,	283},
	{OBJ_WAND_SCARE_MONSTER,	284},
	{OBJ_WAND_TELEPORT_OTHER,	285},
	{OBJ_WAND_DISARMING,	286},
	{OBJ_WAND_ELEC_BALL,	287},
	{OBJ_WAND_COLD_BALL,	288},
	{OBJ_WAND_FIRE_BALL,	289},
	{OBJ_WAND_STINKING_CLOUD,	290},
	{OBJ_WAND_ACID_BALL,	291},
	{OBJ_WAND_WONDER,	292},
	{OBJ_WAND_ACID_BOLT,	294},
	{OBJ_WAND_DRAGON_FIRE,	295},
	{OBJ_WAND_DRAGON_COLD,	296},
	{OBJ_WAND_DRAGON_BREATH,	297},
	{OBJ_WAND_ANNIHILATION,	298},
	{OBJ_WAND_SHARD_BALL,	299},
	{OBJ_STAFF_TRAP_LOCATION,	300},
	{OBJ_STAFF_TREASURE_LOCATION,	301},
	{OBJ_STAFF_OBJECT_LOCATION,	302},
	{OBJ_STAFF_TELEPORTATION,	303},
	{OBJ_STAFF_EARTHQUAKES,	304},
	{OBJ_STAFF_SUMMONING,	305},
	{OBJ_STAFF_LIGHT,	306},
	{OBJ_STAFF_STAR_DESTRUCTION,	307},
	{OBJ_STAFF_STARLIGHT,	308},
	{OBJ_STAFF_HASTE_MONSTERS,	309},
	{OBJ_STAFF_SLOW_MONSTERS,	310},
	{OBJ_STAFF_SLEEP_MONSTERS,	311},
	{OBJ_STAFF_CURE_LIGHT,	312},
	{OBJ_STAFF_DETECT_INVIS,	313},
	{OBJ_STAFF_SPEED,	314},
	{OBJ_STAFF_SLOWNESS,	315},
	{OBJ_STAFF_DOOR_STAIR_LOCATION,	316},
	{OBJ_STAFF_REMOVE_CURSE,	317},
	{OBJ_STAFF_DETECT_EVIL,	318},
	{OBJ_STAFF_CURING,	319},
	{OBJ_STAFF_DISPEL_EVIL,	320},
	{OBJ_STAFF_PROBING,	321},
	{OBJ_STAFF_DARKNESS,	322},
	{OBJ_STAFF_GENOCIDE,	323},
	{OBJ_STAFF_POWER,	324},
	{OBJ_STAFF_THE_MAGI,	325},
	{OBJ_STAFF_PERCEPTION,	326},
	{OBJ_STAFF_HOLINESS,	327},
	{OBJ_STAFF_ENLIGHTENMENT,	328},
	{OBJ_STAFF_HEALING,	329},
	{OBJ_SORCERY_BEGINNERS_HANDBOOK,	334},
	{OBJ_SORCERY_MASTER_SORCERERS_HANDBOOK,	335},
	{OBJ_SORCERY_RLYEH_TEXT,	336},
	{OBJ_SORCERY_UNAUSPRECHLICHEN_KULTEN,	337},
	{OBJ_SMALL_WOODEN_CHEST,	338},
	{OBJ_LARGE_WOODEN_CHEST,	339},
	{OBJ_SMALL_IRON_CHEST,	340},
	{OBJ_LARGE_IRON_CHEST,	341},
	{OBJ_SMALL_STEEL_CHEST,	342},
	{OBJ_LARGE_STEEL_CHEST,	343},
	{OBJ_RUINED_CHEST,	344},
	{OBJ_IRON_SPIKE,	345},
	{OBJ_WOODEN_TORCH,	346},
	{OBJ_BRASS_LANTERN,	347},
	{OBJ_FLASK_OF_OIL,	348},
	{OBJ_EMPTY_BOTTLE,	349},
	{OBJ_ROD_HAVOC,	350},
	{OBJ_ROD_DOOR_STAIR_LOCATION,	351},
	{OBJ_ROD_TRAP_LOCATION,	352},
	{OBJ_ROD_PROBING,	353},
	{OBJ_ROD_RECALL,	354},
	{OBJ_ROD_ILLUMINATION,	355},
	{OBJ_ROD_LIGHT,	356},
	{OBJ_ROD_ELEC_BOLT,	357},
	{OBJ_ROD_COLD_BOLT,	358},
	{OBJ_ROD_FIRE_BOLT,	359},
	{OBJ_ROD_POLYMORPH,	360},
	{OBJ_ROD_SLOW_MONSTER,	361},
	{OBJ_ROD_SLEEP_MONSTER,	362},
	{OBJ_ROD_DRAIN_LIFE,	363},
	{OBJ_ROD_TELEPORT_OTHER,	364},
	{OBJ_ROD_DISARMING,	365},
	{OBJ_ROD_ELEC_BALL,	366},
	{OBJ_ROD_COLD_BALL,	367},
	{OBJ_ROD_FIRE_BALL,	368},
	{OBJ_ROD_ACID_BALL,	369},
	{OBJ_ROD_ACID_BOLT,	370},
	{OBJ_ROD_ENLIGHTENMENT,	371},
	{OBJ_ROD_PERCEPTION,	372},
	{OBJ_ROD_CURING,	373},
	{OBJ_ROD_HEALING,	374},
	{OBJ_ROD_DETECTION,	375},
	{OBJ_ROD_RESTORATION,	376},
	{OBJ_ROD_SPEED,	377},
	{OBJ_LUMP_OF_SULPHUR,	378},
	{OBJ_HEMLOCK_TWIG,	379},
	{OBJ_SILVER_UNICORN_HORN,	380},
	{OBJ_CRYSTAL,	381},
	{OBJ_FLY_AGARIC_TOADSTOOL,	382},
	{OBJ_CLOVE_OF_GARLIC,	383},
	{OBJ_GEODE,	384},
	{OBJ_THAUMATURGY_SIGN_OF_CHAOS,	385},
	{OBJ_THAUMATURGY_CHAOS_MASTERY,	386},
	{OBJ_THAUMATURGY_THE_KING_IN_YELLOW,	387},
	{OBJ_THAUMATURGY_REVELATIONS_OF_GLAAKI,	388},
	{OBJ_SHARD_OF_POTTERY,	389},
	{OBJ_BROKEN_STICK,	390},
	{OBJ_BROKEN_SKULL,	391},
	{OBJ_BROKEN_BONE,	392},
	{OBJ_CANINE_SKELETON,	393},
	{OBJ_RODENT_SKELETON,	394},
	{OBJ_HUMAN_SKELETON,	395},
	{OBJ_DWARF_SKELETON,	396},
	{OBJ_ELF_SKELETON,	397},
	{OBJ_GNOME_SKELETON,	398},
	{OBJ_DSM_BLACK,	400},
	{OBJ_DSM_BLUE,	401},
	{OBJ_DSM_WHITE,	402},
	{OBJ_DSM_RED,	403},
	{OBJ_DSM_GREEN,	404},
	{OBJ_DSM_MULTI_HUED,	405},
	{OBJ_DSM_PSEUDO,	406},
	{OBJ_DSM_LAW,	407},
	{OBJ_DSM_BRONZE,	408},
	{OBJ_DSM_GOLD,	409},
	{OBJ_DSM_CHAOS,	410},
	{OBJ_DSM_BALANCE,	411},
	{OBJ_DSM_POWER,	412},
	{OBJ_DRAGON_HELM,	413},
	{OBJ_DRAGON_SHIELD,	414},
	{OBJ_POTION_IOCAINE,	415},
	{OBJ_POTION_RUINATION,	416},
	{OBJ_POTION_DETONATIONS,	417},
	{OBJ_POTION_AUGMENTATION,	418},
	{OBJ_POTION_STAR_HEALING,	419},
	{OBJ_POTION_LIFE,	420},
	{OBJ_POTION_SELF_KNOWLEDGE,	421},
	{OBJ_POTION_STAR_ENLIGHTENMENT,	422},
	{OBJ_RING_RES_FEAR,	425},
	{OBJ_RING_RES_LIGHT_AND_DARKNESS,	426},
	{OBJ_RING_RES_NETHER,	427},
	{OBJ_RING_RES_NEXUS,	428},
	{OBJ_RING_RES_SOUND,	429},
	{OBJ_RING_RES_CONFUSION,	430},
	{OBJ_RING_RES_SHARD,	431},
	{OBJ_RING_RES_DISENCHANTMENT,	432},
	{OBJ_RING_RES_CHAOS,	433},
	{OBJ_RING_RES_BLINDNESS,	434},
	{OBJ_RING_LORDLY_PROTECTION,	435},
	{OBJ_RING_EXTRA_ATTACKS,	436},
	{OBJ_POTION_CURE_LIGHT,	437},
	{OBJ_POTION_DEC_DEX,	438},
	{OBJ_POTION_DEC_CON,	439},
	{OBJ_COPPER,	480},
	{OBJ_COPPER_2,	481},
	{OBJ_COPPER_3,	482},
	{OBJ_SILVER,	483},
	{OBJ_SILVER_2,	484},
	{OBJ_SILVER_3,	485},
	{OBJ_GARNETS,	486},
	{OBJ_GARNETS_2,	487},
	{OBJ_GOLD,	488},
	{OBJ_GOLD_2,	489},
	{OBJ_GOLD_3,	490},
	{OBJ_OPALS,	491},
	{OBJ_SAPPHIRES,	492},
	{OBJ_RUBIES,	493},
	{OBJ_DIAMONDS,	494},
	{OBJ_EMERALDS,	495},
	{OBJ_MITHRIL,	496},
	{OBJ_ADAMANTITE,	497},
	{OBJ_MIGHTY_HAMMER_OF_WORLDS,	498},
	{OBJ_LEAD_CROWN_OF_THE_UNIVERSE,	499},
	{OBJ_STAR_ESSENCE_OF_POLARIS,	500},
	{OBJ_STAR_ESSENCE_OF_XOTH,	501},
	{OBJ_GEMSTONE_TRAPEZODEDRON,	502},
	{OBJ_AMULET_LOBON,	503},
	{OBJ_AMULET_ABD_ALHAZRED,	504},
	{OBJ_NECKLACE_OF_THE_DWARVES,	505},
	{OBJ_RING_MAGIC,	506},
	{OBJ_RING_BAST,	507},
	{OBJ_RING_ELEMENTAL_POWER_FIRE,	508},
	{OBJ_RING_ELEMENTAL_POWER_ICE,	509},
	{OBJ_RING_ELEMENTAL_POWER_STORM,	510},
	{OBJ_RING_NYARLATHOTEP,	511},
	{OBJ_CONJURATION_MINOR_CONJURINGS,	512},
	{OBJ_CONJURATION_CONJURING_MASTERY,	513},
	{OBJ_CONJURATION_BOOK_OF_EIBON,	514},
	{OBJ_CONJURATION_LIBER_IVONIS,	515},
	{OBJ_NECROMANCY_BLACK_PRAYERS,	516},
	{OBJ_NECROMANCY_BLACK_MASS,	517},
	{OBJ_NECROMANCY_NECRONOMICON,	518},
	{OBJ_NECROMANCY_KITAB_AL_AZIF,	519},
	{OBJ_AMULET_REFLECTION,	520},
	{OBJ_AMULET_ANTI_MAGIC,	521},
	{OBJ_AMULET_ANTI_TELEPORTATION,	522},
	{OBJ_AMULET_RESISTANCE,	523},
	{OBJ_RUNESWORD_STORMBRINGER,	524},
	{OBJ_NO_TEA,	525},
};

#ifdef FUTURE_SAVEFILES

/*
 * XXX XXX XXX Ignore this for now...
 *
 * The basic format of Angband 2.8.0 (and later) savefiles is simple.
 *
 * The savefile itself is a "header" (4 bytes) plus a series of "blocks",
 * plus, perhaps, some form of "footer" at the end.
 *
 * The "header" contains information about the "version" of the savefile.
 * Conveniently, pre-2.8.0 savefiles also use a 4 byte header, though the
 * interpretation of the "sf_extra" byte is very different.  Unfortunately,
 * savefiles from Angband 2.5.X reverse the sf_major and sf_minor fields,
 * and must be handled specially, until we decide to start ignoring them.
 *
 * Each "block" is a "type" (2 bytes), plus a "size" (2 bytes), plus "data",
 * plus a "check" (2 bytes), plus a "stamp" (2 bytes).  The format of the
 * "check" and "stamp" bytes is still being contemplated, but it would be
 * nice for one to be a simple byte-checksum, and the other to be a complex
 * additive checksum of some kind.  Both should be zero if the block is empty.
 *
 * Standard types:
 *   TYPE_BIRTH --> creation info
 *   TYPE_OPTIONS --> option settings
 *   TYPE_MESSAGES --> message recall
 *   TYPE_PLAYER --> player information
 *   TYPE_SPELLS --> spell information
 *   TYPE_INVEN --> player inven/equip
 *   TYPE_STORES --> store information
 *   TYPE_RACES --> monster race data
 *   TYPE_KINDS --> object kind data
 *   TYPE_UNIQUES --> unique info
 *   TYPE_ARTIFACTS --> artifact info
 *   TYPE_QUESTS --> quest info
 *
 * Dungeon information:
 *   TYPE_DUNGEON --> dungeon info
 *   TYPE_FEATURES --> dungeon features
 *   TYPE_OBJECTS --> dungeon objects
 *   TYPE_MONSTERS --> dungeon monsters
 *
 * Conversions:
 *   Break old "races" into normals/uniques
 *   Extract info about the "unique" monsters
 *
 * Question:
 *   Should there be a single "block" for info about all the stores, or one
 *   "block" for each store?  Or one "block", which contains "sub-blocks" of
 *   some kind?  Should we dump every "sub-block", or just the "useful" ones?
 *
 * Question:
 *   Should the normals/uniques be broken for 2.8.0, or should 2.8.0 simply
 *   be a "fixed point" into which older savefiles are converted, and then
 *   future versions could ignore older savefiles, and the "conversions"
 *   would be much simpler.
 */


/*
 * XXX XXX XXX
 */
#define TYPE_OPTIONS 17362


/*
 * Hack -- current savefile
 */
static int data_fd = -1;


/*
 * Hack -- current block type
 */
static u16b data_type;

/*
 * Hack -- current block size
 */
static u16b data_size;

/*
 * Hack -- pointer to the data buffer
 */
static byte *data_head;

/*
 * Hack -- pointer into the data buffer
 */
static byte *data_next;

/*
 * Hack -- write the current "block" to the savefile
 */
static errr wr_block(void)
{
	errr err;

	byte fake[4];

	/* Save the type and size */
	fake[0] = (byte)(data_type);
	fake[1] = (byte)(data_type >> 8);
	fake[2] = (byte)(data_size);
	fake[3] = (byte)(data_size >> 8);

	/* Dump the head */
	err = fd_write(data_fd, (char*)&fake, 4);

	/* Dump the actual data */
	err = fd_write(data_fd, (char*)data_head, data_size);

	/* XXX XXX XXX */
	fake[0] = 0;
	fake[1] = 0;
	fake[2] = 0;
	fake[3] = 0;

	/* Dump the tail */
	err = fd_write(data_fd, (char*)&fake, 4);

	/* Hack -- reset */
	data_next = data_head;

	/* Wipe the data block */
	C_WIPE(data_head, 65535, byte);

	/* Success */
	return (0);
}



/*
 * Hack -- add data to the current block
 */
static void put_byte(byte v)
{
	*data_next++ = v;
}

/*
 * Hack -- add data to the current block
 */
static void put_char(char v)
{
	put_byte((byte)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u16b(u16b v)
{
	*data_next++ = (byte)(v);
	*data_next++ = (byte)(v >> 8);
}

/*
 * Hack -- add data to the current block
 */
static void put_s16b(s16b v)
{
	put_u16b((u16b)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_u32b(u32b v)
{
	*data_next++ = (byte)(v);
	*data_next++ = (byte)(v >> 8);
	*data_next++ = (byte)(v >> 16);
	*data_next++ = (byte)(v >> 24);
}

/*
 * Hack -- add data to the current block
 */
static void put_s32b(s32b v)
{
	put_u32b((u32b)(v));
}

/*
 * Hack -- add data to the current block
 */
static void put_string(char *str)
{
	while ((*data_next++ = *str++) != '\0');
}


/*
 * Hack -- read the next "block" from the savefile
 */
static errr rd_block(void)
{
	errr err;

	byte fake[4];

	/* Read the head data */
	err = fd_read(data_fd, (char*)&fake, 4);

	/* Extract the type and size */
	data_type = (fake[0] | ((u16b)fake[1] << 8));
	data_size = (fake[2] | ((u16b)fake[3] << 8));

	/* Wipe the data block */
	C_WIPE(data_head, 65535, byte);

	/* Read the actual data */
	err = fd_read(data_fd, (char*)data_head, data_size);

	/* Read the tail data */
	err = fd_read(data_fd, (char*)&fake, 4);

	/* XXX XXX XXX Verify */

	/* Hack -- reset */
	data_next = data_head;

	/* Success */
	return (0);
}


/*
 * Hack -- get data from the current block
 */
static void get_byte(byte *ip)
{
	byte d1;
	d1 = (*data_next++);
	(*ip) = (d1);
}

/*
 * Hack -- get data from the current block
 */
static void get_char(char *ip)
{
	get_byte((byte*)ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u16b(u16b *ip)
{
	u16b d0, d1;
	d0 = (*data_next++);
	d1 = (*data_next++);
	(*ip) = (d0 | (d1 << 8));
}

/*
 * Hack -- get data from the current block
 */
static void get_s16b(s16b *ip)
{
	get_u16b((u16b*)ip);
}

/*
 * Hack -- get data from the current block
 */
static void get_u32b(u32b *ip)
{
	u32b d0, d1, d2, d3;
	d0 = (*data_next++);
	d1 = (*data_next++);
	d2 = (*data_next++);
	d3 = (*data_next++);
	(*ip) = (d0 | (d1 << 8) | (d2 << 16) | (d3 << 24));
}

/*
 * Hack -- get data from the current block
 */
static void get_s32b(s32b *ip)
{
	get_u32b((u32b*)ip);
}



/*
 * Read a savefile for Angband 2.8.0
 */
static errr rd_savefile(void)
{
	bool    done = FALSE;

	byte    fake[4];


	/* Open the savefile */
	data_fd = fd_open(savefile, O_RDONLY);

	/* No file */
	if (data_fd < 0) return (1);

	/* Strip the first four bytes (see below) */
	if (fd_read(data_fd, (char*)(fake), 4)) return (1);


	/* Make array XXX XXX XXX */
	C_MAKE(data_head, 65535, byte);

	/* Hack -- reset */
	data_next = data_head;


	/* Read blocks */
	while (!done)
	{
		/* Read the block */
		if (rd_block()) break;

		/* Analyze the type */
		switch (data_type)
		{
			/* Done XXX XXX XXX */
			case 0:
			{
				done = TRUE;
				break;
			}

			/* Grab the options */
			case TYPE_OPTIONS:
			{
				if (get_options()) err = -1;
				break;
			}
		}

		/* XXX XXX XXX verify "data_next" */
		if (data_next - data_head > data_size) break;
	}


	/* XXX XXX XXX Check for errors */


	/* Kill array XXX XXX XXX */
	KILL(data_head);


	/* Success */
	return (0);
}


#endif /* FUTURE_SAVEFILES */




/*
 * Some "local" parameters, used to help write savefiles
 */

static FILE     *fff;           /* Current save "file" */

static byte     xor_byte;       /* Simple encryption */

static u32b     v_stamp = 0L;   /* A simple "checksum" on the actual values */
static u32b     x_stamp = 0L;   /* A simple "checksum" on the encoded bytes */



/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void)putc((int)xor_byte, fff);

	/* Maintain the checksum info */
	v_stamp += v;
	x_stamp += xor_byte;
}

static void wr_byte(byte v)
{
	sf_put(v);
}

static void wr_u16b(u16b v)
{
	sf_put((byte)(v & 0xFF));
	sf_put((byte)((v >> 8) & 0xFF));
}

static void wr_s16b(s16b v)
{
	wr_u16b((u16b)v);
}

static void wr_u32b(u32b v)
{
	sf_put((byte)(v & 0xFF)); 
	sf_put((byte)((v >> 8) & 0xFF));
	sf_put((byte)((v >> 16) & 0xFF));
	sf_put((byte)((v >> 24) & 0xFF));
}

static void wr_s32b(s32b v)
{
	wr_u32b((u32b)v);
}

static void wr_string(cptr str)
{
	while (*str)
	{
		wr_byte(*str);
		str++;
	}
	wr_byte(*str);
}


/*
 * These functions write info in larger logical records
 */


/*
 * Write an "item" record
 */
static void wr_item(object_type *o_ptr)
{
	u16b k_idx = convert_k_idx(o_ptr->k_idx, sf_flags_now, sf_flags);
	wr_s16b(k_idx);

	/* Location */
	wr_byte(o_ptr->iy);
	wr_byte(o_ptr->ix);

	wr_byte(o_ptr->tval);
	wr_byte(0); /* Was sval, but sval is unused in all versions. */
	wr_s16b(o_ptr->pval);

	wr_byte(o_ptr->discount);
	wr_byte(o_ptr->number);
	wr_s16b(o_ptr->weight);

	wr_byte(o_ptr->name1);
	wr_byte(o_ptr->name2);
	wr_s16b(o_ptr->timeout);

	wr_s16b(o_ptr->to_h);
	wr_s16b(o_ptr->to_d);
	wr_s16b(o_ptr->to_a);
	wr_s16b(o_ptr->ac);
	wr_byte(o_ptr->dd);
	wr_byte(o_ptr->ds);

#ifdef SF_16_IDENT
	if (!has_flag(SF_16_IDENT))
	{
		byte temp = (o_ptr->ident & 0xFE) | (0x01 * ((o_ptr->ident & IDENT_SENSE) == IDENT_SENSE));
		wr_byte(temp);
	}
	else
	{
		wr_u16b(o_ptr->ident);
	}
#else
	wr_byte(o_ptr->ident);
#endif

	wr_byte(o_ptr->marked);

    wr_u32b(o_ptr->flags1);
    wr_u32b(o_ptr->flags2);
    wr_u32b(o_ptr->flags3);

	/* Held by monster index */
	wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	wr_byte(o_ptr->xtra1);
	wr_byte(o_ptr->xtra2);

	/* Save the inscription (if any) */
	wr_string(quark_str(o_ptr->note));
	
	/* Save the randart name, if any. */
	wr_string(quark_str(o_ptr->art_name));
}


/*
 * Write a "monster" record
 */
static void wr_monster(monster_type *m_ptr)
{
	wr_s16b(m_ptr->r_idx);
	wr_byte(m_ptr->fy);
	wr_byte(m_ptr->fx);
	wr_byte(m_ptr->generation);
	wr_s16b(m_ptr->hp);
	wr_s16b(m_ptr->maxhp);
	wr_s16b(m_ptr->csleep);
	wr_byte(m_ptr->mspeed);
	wr_s16b(m_ptr->energy);
	wr_byte(m_ptr->stunned);
	wr_byte(m_ptr->confused);
	wr_byte(m_ptr->monfear);
    wr_u32b(m_ptr->smart);
	wr_byte(0);
}


/*
 * Write a "lore" record
 */
static void wr_lore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Count sights/deaths/kills */
	wr_s16b(r_ptr->r_sights);
	wr_s16b(r_ptr->r_deaths);
	wr_s16b(r_ptr->r_pkills);
	wr_s16b(r_ptr->r_tkills);

	/* Count wakes and ignores */
	wr_byte(r_ptr->r_wake);
	wr_byte(r_ptr->r_ignore);

	/* Extra stuff */
#if 0
	wr_byte(r_ptr->r_xtra1);
	wr_byte(r_ptr->r_xtra2);
#else
	wr_u16b(0);
#endif

	/* Count drops */
	wr_byte(r_ptr->r_drop_gold);
	wr_byte(r_ptr->r_drop_item);

	/* Count spells */
	wr_byte(r_ptr->r_cast_inate);
	wr_byte(r_ptr->r_cast_spell);

	/* Count blows of each type */
	wr_byte(r_ptr->r_blows[0]);
	wr_byte(r_ptr->r_blows[1]);
	wr_byte(r_ptr->r_blows[2]);
	wr_byte(r_ptr->r_blows[3]);

	/* Memorize flags */
	wr_u32b(r_ptr->r_flags1);
	wr_u32b(r_ptr->r_flags2);
	wr_u32b(r_ptr->r_flags3);
	wr_u32b(r_ptr->r_flags4);
	wr_u32b(r_ptr->r_flags5);
	wr_u32b(r_ptr->r_flags6);


	/* Monster limit per level */
	wr_byte(r_ptr->max_num);

	/* Later (?) */
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
}

#ifdef SF_DEATHEVENTTEXT

/*
 * Write a "death event" record
 */
static void wr_death(void)
{
	uint i;
	u16b tmp16u = UNREAD_VALUE;
		for (i = 0; i < MAX_DEATH_EVENTS; i++)
		{
			death_event_type *d_ptr = &death_event[i];
			if (!d_ptr->r_idx) break;
			if (i%15 == 0)
			{
				if (i) wr_u16b(tmp16u);
				tmp16u = 0;
			}
			if (d_ptr->flags & EF_KNOWN)
			{
				tmp16u |= 1<<(i%15);
			}
		}
		/* Terminate by setting the 16th bit. */
		tmp16u |= 1<<15;
		wr_u16b(tmp16u);
}

#endif

/*
 * Write an "xtra" record
 */
static void wr_xtra(s16b k_idx)
{
	byte tmp8u = 0;
	object_kind *k_ptr;

	k_idx = convert_k_idx(k_idx, sf_flags_now, sf_flags);
	k_ptr = &k_info[k_idx];

	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;

	wr_byte(tmp8u);
}


/*
 * Write a "store" record
 */
static void wr_store(store_type *st_ptr)
{
	int j;
	
	/* Save the type */
	wr_byte(st_ptr->type);

	/* Save the position */
	wr_byte(st_ptr->x);
	wr_byte(st_ptr->y);
	/* Save the "open" counter */
	wr_u32b(st_ptr->store_open);

	/* Save the "insults" */
	wr_s16b(st_ptr->insult_cur);

	/* Save the current owner */
	wr_byte(st_ptr->bought);
	wr_byte(st_ptr->owner);

	/* Save the stock size */
	wr_byte((byte)(st_ptr->stock_num));

	/* Save the "haggle" info */
	wr_s16b(st_ptr->good_buy);
	wr_s16b(st_ptr->bad_buy);

	/* Save the stock */
	for (j = 0; j < st_ptr->stock_num; j++)
	{
		/* Save each item in stock */
		wr_item(&st_ptr->stock[j]);
	}
}


/*
 * Write RNG state
 */
static errr wr_randomizer(void)
{
	int i;

	/* Zero */
	wr_u16b(0);
	
	/* Place */
	wr_u16b(Rand_place);
	
	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		wr_u32b(Rand_state[i]);
	}
	
	/* Success */
	return (0);
}


/*
 * Write the "options"
 */
static void wr_options(void)
{
	int i;

	u16b c;


	/*** Oops ***/

	/* Oops */
	for (i = 0; i < 4; i++) wr_u32b(0L);


	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte((byte)(delay_factor));

	/* Write "hitpoint_warn" */
	wr_byte((byte)(hitpoint_warn));


	/*** Cheating options ***/

	c = 0;

	if (cheat_wzrd) c |= 0x0002;

	if (cheat_peek) c |= 0x0100;
	if (cheat_hear) c |= 0x0200;
	if (cheat_room) c |= 0x0400;
	if (cheat_xtra) c |= 0x0800;
	if (cheat_live) c |= 0x2000;
	if (cheat_skll) c |= 0x4000;

	wr_u16b(c);

    /* Autosave info */
    wr_byte(autosave_l);
#ifdef SF_Q_SAVE
	if (has_flag(SF_Q_SAVE) && autosave_q) autosave_t |= 2;
#endif
    wr_byte(autosave_t);
    wr_s16b(autosave_freq);

	/*** Extract options ***/

	/* Analyze the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_set;
		int ob = option_info[i].o_bit;

		/* Process real entries */
		if (option_info[i].o_var)
		{
			/* Set */
			if (*option_info[i].o_var)
			{
				/* Set */
				option_flag[os] |= (1L << ob);
			}
			
			/* Clear */
			else
			{
				/* Clear */
				option_flag[os] &= ~(1L << ob);
			}
		}
	}


	/*** Normal options ***/

	/* Dump the flags */
	for (i = 0; i < 8; i++) wr_u32b(option_flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(option_mask[i]);


	/*** Window options ***/

#ifdef SF_3D_WINPRI
	/* Dump the flags */
	for (i = 0; i < 8; i++)
	{
		if (has_flag(SF_3D_WINPRI))
		{
			for (c = 0; c < 32; c++)
			{
				byte pri = (windows[i].pri[c])%16;
				pri += 16*((windows[i].rep[c])%16);
				wr_byte(pri);
			}
		}
		else
		{
			/* The current display should be as good as any. */
			wr_u32b(1<<(windows[i].current));
		}
	}

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(windows[i].mask);
#else
	/* Dump the flags */
	for (i = 0; i < 8; i++) wr_u32b(window_flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(window_mask[i]);
#endif
}


/*
 * Hack -- Write the "ghost" info
 */
static void wr_ghost(void)
{
	int i;

	monster_race *r_ptr = &r_info[MAX_R_IDX-1];


	/* Name */
	wr_string(r_name + r_ptr->name);

	/* Visuals */
	wr_byte(r_ptr->d_char);
	wr_byte(r_ptr->d_attr);

	/* Level/Rarity */
	wr_byte(r_ptr->level);
	wr_byte(r_ptr->rarity);
	wr_byte(r_ptr->cur_num);
	wr_byte(r_ptr->max_num);

	/* Misc info */
	wr_byte(r_ptr->hdice);
	wr_byte(r_ptr->hside);
	wr_s16b(r_ptr->ac);
	wr_s16b(r_ptr->sleep);
	wr_byte(r_ptr->aaf);
	wr_byte(r_ptr->speed);

	/* Experience */
	wr_s32b(r_ptr->mexp);

	/* Extra */
#if 0
	wr_s16b(r_ptr->extra);
#else
	wr_s16b(0);
#endif

	/* Frequency */
	wr_byte(r_ptr->freq_inate);
	wr_byte(r_ptr->freq_spell);
	wr_byte(r_ptr->num_blows);

	/* Flags */
	wr_u32b(r_ptr->flags1);
	wr_u32b(r_ptr->flags2);
	wr_u32b(r_ptr->flags3);
	wr_u32b(r_ptr->flags4);
	wr_u32b(r_ptr->flags5);
	wr_u32b(r_ptr->flags6);

	/* Attacks */
	for (i = 0; i < 4; i++)
	{
		wr_byte(r_ptr->blow[i].method);
		wr_byte(r_ptr->blow[i].effect);
		wr_byte(r_ptr->blow[i].d_dice);
		wr_byte(r_ptr->blow[i].d_side);
	}
}


/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
	int i,j;

	wr_string(player_name);

	wr_string(died_from);

	for (i = 0; i < 4; i++)
	{
		wr_string(history[i]);
	}

	/* Race/Template/Gender/Spells */
	wr_byte(p_ptr->prace);
	wr_byte(p_ptr->ptemplate);
	wr_byte(p_ptr->psex);
	wr_byte(0);     /* oops */

	wr_byte(p_ptr->hitdie);
    wr_u16b(p_ptr->expfact);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);
	wr_s16b(p_ptr->birthday);
	wr_s16b(p_ptr->startdate);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < 6; ++i) wr_s16b(p_ptr->stat_max[i]);
	for (i = 0; i < 6; ++i) wr_s16b(p_ptr->stat_cur[i]);

	/* Ignore the transient stats */
	for (i = 0; i < 12; ++i) wr_s16b(0);

	wr_u32b(p_ptr->au);

	wr_u32b(p_ptr->exp);
	wr_u16b(p_ptr->exp_frac);

#ifdef SF_SAVE_MAX_SKILLS

	if (has_flag(SF_SAVE_MAX_SKILLS))
	{
		j = MAX_SKILLS;
		wr_byte(j);
	}
	else

#endif /* SF_SAVE_MAX_SKILLS */

	/* Without SAVE_MAX_SKILLS, there are assumed to be 27 skills. */
	{
		j = 27;
	}
	
	for (i=0; i<j; i++)
	{
		wr_byte(skill_set[i].value);
		wr_byte(skill_set[i].max_value);
#ifdef SF_SKILL_BASE
		if (has_flag(SF_SKILL_BASE))
		{
			wr_byte(skill_set[i].base);
			wr_byte(skill_set[i].ceiling);
		}
#endif
		wr_u16b(skill_set[i].exp_to_raise);
		wr_u16b(skill_set[i].experience);
	}

	wr_s16b(p_ptr->mhp);
	wr_s16b(p_ptr->chp);
	wr_u16b(p_ptr->chp_frac);

	wr_s16b(p_ptr->msp);
	wr_s16b(p_ptr->csp);
	wr_u16b(p_ptr->csp_frac);

	wr_s16b(p_ptr->mchi);
	wr_s16b(p_ptr->cchi);
	wr_u16b(p_ptr->chi_frac);

	/* Max Player and Dungeon Levels */
	for(i=0;i<MAX_CAVES;i++)
	{
		wr_s16b(p_ptr->max_dlv[i]);
	}

	/* More info */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(p_ptr->sc);
	wr_s16b(0);     /* oops */

	wr_s16b(0);             /* old "rest" */
	wr_s16b(p_ptr->blind);
	wr_s16b(p_ptr->paralyzed);
	wr_s16b(p_ptr->confused);
	wr_s16b(p_ptr->food);
	wr_s16b(0);     /* old "food_digested" */
	wr_s16b(0);     /* old "protection" */
	wr_s16b(p_ptr->energy);
	wr_s16b(p_ptr->fast);
	wr_s16b(p_ptr->slow);
	wr_s16b(p_ptr->afraid);
	wr_s16b(p_ptr->cut);
	wr_s16b(p_ptr->stun);
	wr_s16b(p_ptr->poisoned);
	wr_s16b(p_ptr->image);
	wr_s16b(p_ptr->protevil);
	wr_s16b(p_ptr->invuln);
	wr_s16b(p_ptr->hero);
	wr_s16b(p_ptr->shero);
	wr_s16b(p_ptr->shield);
	wr_s16b(p_ptr->blessed);
	wr_s16b(p_ptr->tim_invis);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->see_infra);
	wr_s16b(p_ptr->tim_infra);
	wr_s16b(p_ptr->oppose_fire);
	wr_s16b(p_ptr->oppose_cold);
	wr_s16b(p_ptr->oppose_acid);
	wr_s16b(p_ptr->oppose_elec);
	wr_s16b(p_ptr->oppose_pois);
    wr_s16b(p_ptr->tim_esp);
    wr_s16b(p_ptr->wraith_form);
    wr_s16b(p_ptr->resist_magic);
    wr_s16b(p_ptr->tim_xtra1);
    wr_s16b(p_ptr->tim_xtra2);
    wr_s16b(p_ptr->tim_xtra3);
    wr_s16b(p_ptr->tim_xtra4);
    wr_s16b(p_ptr->tim_xtra5);
    wr_s16b(p_ptr->tim_xtra6);
    wr_s16b(p_ptr->tim_xtra7);
    wr_s16b(p_ptr->tim_xtra8);

    wr_s16b(p_ptr->chaos_patron);
    wr_u32b(p_ptr->muta1);
    wr_u32b(p_ptr->muta2);
    wr_u32b(p_ptr->muta3);

	wr_byte(p_ptr->confusing);
	for (i=0;i<MAX_TOWNS;i++) wr_byte(p_ptr->house[i]);
	wr_byte(p_ptr->ritual);
	wr_byte(p_ptr->sneaking);
	wr_byte(0);

	/* Future use */
	for (i = 0; i < 12; i++) wr_u32b(0L);

	/* Ignore some flags */
	wr_u32b(0L);    /* oops */
	wr_u32b(0L);    /* oops */
	wr_u32b(0L);    /* oops */


	/* Write the "object seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_wild);
	for(i=0;i<12;i++)
	{
		for(j=0;j<12;j++)
		{
			wr_u32b(wild_grid[i][j].seed);
			wr_byte(wild_grid[i][j].dungeon);
			wr_byte(wild_grid[i][j].road_map);
		}
	}


	/* Special stuff */
	wr_u16b(panic_save);
	wr_u16b(total_winner);
	wr_u16b(noscore);


	/* Write death */
	wr_byte(death);

	/* Write feeling */
	wr_byte((byte)(feeling));

	/* Turn of last "feeling" */
	wr_s32b(old_turn);

	/* Current turn */
	wr_s32b(turn);

#ifdef SF_CURSE
	if (has_flag(SF_CURSE))
		wr_s32b(curse_turn);
#endif
}



/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
	int i, y, x;

	byte tmp8u;
	u16b tmp16u;

	byte count;
	u16b prev_char;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(dun_level);
	wr_u16b(dun_offset);
	wr_u16b(dun_bias);
	wr_byte(cur_town);
	wr_byte(cur_dungeon);
	wr_byte(recall_dungeon);
	wr_byte(came_from);
	wr_u16b(num_repro);
	wr_u16b(py);
	wr_u16b(px);
	wr_u16b(wildx);
	wr_u16b(wildy);
	wr_u16b(cur_hgt);
	wr_u16b(cur_wid);
	wr_u16b(max_panel_rows);
	wr_u16b(max_panel_cols);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract the cave flags */
			tmp16u = c_ptr->info;
#ifdef SF_16_CAVE_FLAG
			if (!has_flag(SF_16_CAVE_FLAG)) tmp16u &= 0x00FF;
#endif
			
			/* If the run is broken, or too full, flush it */
			if ((tmp16u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
#ifdef SF_16_CAVE_FLAG
				if (has_flag(SF_16_CAVE_FLAG))
					wr_u16b(prev_char);
				else
#endif
				wr_byte((byte)prev_char);

				prev_char = tmp16u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
#ifdef SF_16_CAVE_FLAG
		if (has_flag(SF_16_CAVE_FLAG))
			wr_u16b(prev_char);
		else
#endif
		wr_byte((byte)prev_char);
	}
func_false();


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp8u = c_ptr->feat;
			
			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				wr_byte((byte)prev_char);
				prev_char = tmp8u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
		wr_byte((byte)prev_char);
	}




	/* Compact the objects */
	compact_objects(0);
	/* Compact the monsters */
	compact_monsters(0);

	/*** Dump objects ***/

	/* Total objects */
	wr_u16b(o_max);

	/* Dump the objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Dump it */
		wr_item(o_ptr);
	}


	/*** Dump the monsters ***/


	/* Total monsters */
	wr_u16b(m_max);

	/* Dump the monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		
		/* Dump it */
		wr_monster(m_ptr);
	}
}



/*
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
	int        i;

	u32b              now;

	byte            tmp8u;
	u16b            tmp16u;


	/* Guess at the current time */
	now = time((time_t *)0);


	/* Note the operating system */
	sf_xtra = 0L;

	/* Note when the file was saved */
	sf_when = now;

	/* Note the number of saves */
	sf_saves++;


	/*** Actually write the file ***/

    /* Dump the file header */
	xor_byte = 0;
	    wr_byte(sf_major);
	xor_byte = 0;
	    wr_byte(sf_minor);
	xor_byte = 0;
	wr_byte(sf_patch);
	xor_byte = 0;

	tmp8u = (byte)(rand_int(256));
	wr_byte(tmp8u);


	/* Reset the checksum */
	v_stamp = 0L;
	x_stamp = 0L;


	/* Operating system */
	wr_u32b(sf_xtra);


	/* Time file last saved */
	wr_u32b(sf_when);

	/* Number of past lives */
	wr_u16b(sf_lives);

	/* Number of times saved */
	wr_u16b(sf_saves);


	/* Space */
	wr_u32b(0L);
	wr_u32b(0L);


	/* Write the RNG state */
	wr_randomizer();


	/* Write the boolean "options" */
	wr_options();


	/* Dump the number of "messages" */
	tmp16u = message_num();
	if (compress_savefile && (tmp16u > 40)) tmp16u = 40;
	wr_u16b(tmp16u);

	/* Dump the messages (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str((short)i));
	}


	/* Dump the monster lore */
	tmp16u = MAX_R_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_lore(i);

#ifdef SF_DEATHEVENTTEXT
	/* Dump the death event lore */
	if (has_flag(SF_DEATHEVENTTEXT)) wr_death();
#endif

	/* Dump the object memory */
	tmp16u = MAX_K_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_xtra(i);


	/* Hack -- Dump the quests */
	tmp16u = MAX_Q_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_byte((byte)(q_list[i].level));
		wr_s16b((short)(q_list[i].r_idx));
		wr_byte((byte)(q_list[i].dungeon));
		wr_byte((byte)(q_list[i].cur_num));
		wr_byte((byte)(q_list[i].max_num));
#ifdef SF_QUEST_UNKNOWN
		if (has_flag(SF_QUEST_UNKNOWN)) wr_byte((byte)(q_list[i].cur_num_known));
#endif
	}

	/* Hack -- Dump the artifacts */
	tmp16u = MAX_A_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->cur_num);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
	}



	/* Write the "extra" information */
	wr_extra();


	/* Dump the "player hp" entries */
	tmp16u = 100;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_s16b(player_hp[i]);
	}


	/* Write spell data */
	for (i=0;i<MAX_SCHOOL;i++)
	{
		wr_u32b(spell_learned[i]);
		wr_u32b(spell_worked[i]);
		wr_u32b(spell_forgotten[i]);
	}

	/* Dump the ordered spells */
	for (i = 0; i < 128; i++)
	{
		wr_byte(spell_order[i]);
	}

		/* Dump spirit info */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		wr_u16b(spirits[i].pact);
		wr_u32b(spirits[i].annoyance);
		wr_string(spirits[i].name);
	}


	/* Write the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Dump index */
		wr_u16b((short)i);

		/* Dump object */
		wr_item(o_ptr);
	}

	/* Add a sentinel */
	wr_u16b(0xFFFF);


	/* Note the stores */
	tmp16u = MAX_STORES_TOTAL;
	wr_u16b(tmp16u);

	/* Dump the stores */
	for (i = 0; i < tmp16u; i++) wr_store(&store[i]);


	/* Player is not dead, write the dungeon */
	if (!death)
	{
		/* Dump the dungeon */
		wr_dungeon();

		/* Dump the ghost */
		wr_ghost();
	}


	/* Write the "value check-sum" */
	wr_u32b(v_stamp);

	/* Write the "encoded checksum" */
	wr_u32b(x_stamp);


	/* Error in save */
	if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;

	/* Successful save */
	return TRUE;
}


/*
 * Medium level player saver
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
 */
static bool save_player_aux(char *name)
{
	bool    ok = FALSE;

	int             fd = -1;

	int             mode = 0644;


	/* No file yet */
	fff = NULL;


	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);


	/* Create the savefile */
	fd = fd_make(name, mode);

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		(void)fd_close(fd);

		/* Open the savefile */
		fff = my_fopen(name, "wb");

		/* Successful open */
		if (fff)
		{
			/* Write the savefile */
			if (wr_savefile_new()) ok = TRUE;

			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}

		/* Remove "broken" files */
		if (!ok) (void)fd_kill(name);
	}


	/* Failure */
	if (!ok) return (FALSE);

	/* Successful save */
	character_saved = TRUE;

	/* Success */
	return (TRUE);
}

/*
 * Define the flags for the current version globally.
 */
const u16b sf_flags_now = 0x00000000
#ifdef SF_SKILL_BASE
	| SF_SKILL_BASE
#endif
#ifdef SF_16_IDENT
	| SF_16_IDENT
#endif
#ifdef SF_CURSE
	| SF_CURSE
#endif
#ifdef SF_Q_SAVE
	| SF_Q_SAVE
#endif
#ifdef SF_DEATHEVENTTEXT
	| SF_DEATHEVENTTEXT
#endif
#ifdef SF_QUEST_UNKNOWN
	| SF_QUEST_UNKNOWN
#endif
#ifdef SF_3D_WINPRI
	| SF_3D_WINPRI
#endif
#ifdef SF_16_CAVE_FLAG
	| SF_16_CAVE_FLAG
#endif
#ifdef SF_SAVE_MAX_SKILLS
	| SF_SAVE_MAX_SKILLS
#endif
#ifdef SF_K_INFO_1
	| SF_K_INFO_1
#endif
	;

/*
 * Determine the current version number based on compile-time options.
 */
static void current_version(u16b *flags, byte *major, byte *minor, byte *patch)
{
	if (flags) (*flags) = sf_flags_now;
	if (major) (*major) = SFM_SPECIAL;
	if (minor) (*minor) = (*flags)/256;
	if (patch) (*patch) = (*flags)%256;
}

/*
 * Convert an object table from one version to another.
 */
s16b convert_k_idx(s16b k_idx, u32b from_v, u32b to_v)
{
	/* Assume the oldest version in the absence of relevant flags. */
	uint i, from, to, max = N_ELEMENTS(object_version)-1;
	int max_distro;

	/* Find the newest version flag which is included in each of old and new. */
	for (i = 0, from = to = max; i < max; i++)
	{
		if (from_v & object_version[i]) from = i;
		if (to_v & object_version[i]) to = i;
	}

	/* No change. */
	if (from == to) return k_idx;

	/* Return the new version of the given object. */
	for (i = 0; i < N_ELEMENTS(object_table); i++)
	{
		if (object_table[i][from] != k_idx) continue;
		return object_table[i][to];
	}

	/*
	 * Maintain the user area's integrity.
	 * OBJ_MAX_DISTRO must be a term in the above table.
	 */
	max_distro = convert_k_idx(OBJ_MAX_DISTRO, sf_flags_now, from_v);
	if (k_idx > max_distro)
	{
		return k_idx - max_distro +
			convert_k_idx(OBJ_MAX_DISTRO, sf_flags_now, to_v);
	}
	/* 
	 * As anything can happen to strange indices in the system area, convert
	 * them to bad objects in order for them to be safely removed later.
	 */
	else
	{
		return -1;
	}
}

/*
 * Attempt to save the player in a savefile
 *
 * This routine is only capable of creating a save file for the current version
 * and for 4.1.0, but the rest of the file should cope with any desired save
 * file.
 */
bool save_player(bool as_4_1_0)
{

	int             result = FALSE;

	char    safe[1024];

	/* Find the current version. */
	current_version(&sf_flags, &sf_major, &sf_minor, &sf_patch);

#ifdef SF_SKILL_BASE
	/* If a 4.1.0 savefile is required, provide one. */
	if (as_4_1_0)
	{
		sf_flags = SF_SKILL_BASE;
		sf_major = 4;
		sf_minor = 1;
		sf_patch = 0;
	}
#endif

#ifdef SET_UID

# ifdef SECURE

	/* Get "games" permissions */
	beGames();

# endif

#endif


	/* New savefile */
	strcpy(safe, savefile);
	strcat(safe, ".new");

#ifdef VM
	/* Hack -- support "flat directory" usage on VM/ESA */
	strcpy(safe, savefile);
	strcat(safe, "n");
#endif /* VM */

	/* Remove it */
	fd_kill(safe);

	/* Attempt to save the player */
	if (save_player_aux(safe))
	{
		char temp[1024];

		/* Old savefile */
		strcpy(temp, savefile);
		strcat(temp, ".old");

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		strcpy(temp, savefile);
		strcat(temp, "o");
#endif /* VM */

		/* Remove it */
		fd_kill(temp);

		/* Preserve old savefile */
		fd_move(savefile, temp);

		/* Activate new savefile */
		fd_move(safe, savefile);

		/* Remove preserved savefile */
		fd_kill(temp);

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

		/* Lock on savefile */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock file */
		fd_kill(temp);

#endif

		/* Success */
		result = TRUE;
	}


#ifdef SET_UID

# ifdef SECURE

	/* Drop "games" permissions */
	bePlayer();

# endif

#endif


	/* Return the result */
	return (result);
}



/*
 * Attempt to Load a "savefile"
 *
 * Version 2.7.0 introduced a slightly different "savefile" format from
 * older versions, requiring a completely different parsing method.
 *
 * Note that savefiles from 2.7.0 - 2.7.2 are completely obsolete.
 *
 * Pre-2.8.0 savefiles lose some data, see "load2.c" for info.
 *
 * Pre-2.7.0 savefiles lose a lot of things, see "load1.c" for info.
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
	int             fd = -1;

	errr    err = 0;

	byte    vvv[4];

#ifdef VERIFY_TIMESTAMP
	struct stat     statbuf;
#endif

	cptr    what = "generic";


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	death = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(VM)

	/* XXX XXX XXX Fix this */

	/* Verify the existance of the savefile */
	if (access(savefile, 0) < 0)
	{
		/* Give a message */
		msg_print("Savefile does not exist.");
		msg_print(NULL);

		/* Allow this */
		return (TRUE);
	}

#endif


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (!err)
	{
		FILE *fkk;

		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Check for lock */
		fkk = my_fopen(temp, "r");

		/* Oops, lock exists */
		if (fkk)
		{
			/* Close the file */
			my_fclose(fkk);

			/* Message */
			msg_print("Savefile is currently in use.");
			msg_print(NULL);

			/* Oops */
			return (FALSE);
		}

		/* Create a lock file */
		fkk = my_fopen(temp, "w");

		/* Dump a line of info */
		fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

		/* Close the lock file */
		my_fclose(fkk);
	}

#endif


	/* Okay */
	if (!err)
	{
		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{

#ifdef VERIFY_TIMESTAMP
		/* Get the timestamp */
		(void)fstat(fd, &statbuf);
#endif

		/* Read the first four bytes */
		if (fd_read(fd, (char*)(vvv), 4)) err = -1;

		/* What */
		if (err) what = "Cannot read savefile";

		/* Close the file */
		(void)fd_close(fd);
	}

	/* Process file */
	if (!err)
	{

		/* Extract version */
        sf_major = vvv[0];
        sf_minor = vvv[1];
        sf_patch = vvv[2];
		sf_extra = vvv[3];

		/* Clear screen */
		Term_clear();

		/* Attempt to load */
		err = rd_savefile_new();
		/* Message (below) */
		if (err) what = "Cannot parse savefile";
	}

	/* Paranoia */
	if (!err)
	{
		/* Invalid turn */
		if (!turn) err = -1;

		/* Message (below) */
		if (err) what = "Broken savefile";
	}

#ifdef VERIFY_TIMESTAMP
	/* Verify timestamp */
	if (!err)
	{
		/* Hack -- Verify the timestamp */
		if (sf_when > (statbuf.st_ctime + 100) ||
		    sf_when < (statbuf.st_ctime - 100))
		{
			/* Message */
			what = "Invalid timestamp";

			/* Oops */
			err = -1;
		}
	}
#endif


	/* Okay */
	if (!err)
	{
		u16b cur_flags;
		byte cur[3];
		current_version(&cur_flags, cur, cur+1, cur+2);
		/* Give a conversion warning */
        if ((cur[0] != sf_major) ||
            (cur[1] != sf_minor) ||
            (cur[2] != sf_patch))
		{
			/* Message */
            msg_format("Converted a %d.%d.%d savefile.",
                       sf_major, sf_minor, sf_patch);
			msg_print(NULL);
		}

		/* Player is dead */
		if (death)
		{
			/* Player is no longer "dead" */
			death = FALSE;

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = old_turn = 0;

			/* Done */
			return (TRUE);
		}

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			(void)strcpy(died_from, "(alive and well)");
		}

		/* Success */
		return (TRUE);
	}


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (TRUE)
	{
		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock */
		fd_kill(temp);
	}

#endif


	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
		   what, sf_major, sf_minor, sf_patch);
	msg_print(NULL);

	/* Oops */
	return (FALSE);
}


