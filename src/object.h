
#ifndef INCLUDED_OBJECT_H
#define INCLUDED_OBJECT_H

/* File: object.h */

/*
 * Maximum amount of starting equipment
 */
#define MAX_START_ITEMS	4


/*
 * Number of tval/min-sval/max-sval slots per ego_item
 */
#define EGO_TVALS_MAX 3



/*
 * There is a 1/20 (5%) chance of inflating the requested object_level
 * during the creation of an object (see "get_obj_num()" in "object.c").
 * Lower values yield better objects more often.
 */
#define GREAT_OBJ	20

/*
 * There is a 1/20 (5%) chance that ego-items with an inflated base-level are
 * generated when an object is turned into an ego-item (see make_ego_item()
 * in object2.c). As above, lower values yield better ego-items more often.
 */
#define GREAT_EGO	20


/*
 * Refueling constants
 */
#define FUEL_TORCH	5000	/* Maximum amount of fuel in a torch */
#define FUEL_LAMP	15000   /* Maximum amount of fuel in a lantern */
#define DEFAULT_TORCH	FUEL_TORCH  /* Default amount of fuel in a torch */
#define DEFAULT_LAMP   (FUEL_LAMP / 2)  /* Default amount of fuel in a lantern */


/*** General index values ***/

/*used as values for the "object_generation_mode variable*/

#define OB_GEN_MODE_NORMAL		0
#define OB_GEN_MODE_GEN_ST		1
#define OB_GEN_MODE_ARMOURY		2
#define OB_GEN_MODE_WEAPONSMITH	3
#define OB_GEN_MODE_TEMPLE		4
#define OB_GEN_MODE_ALCHEMY		5
#define OB_GEN_MODE_MAGIC_SHOP	6
#define OB_GEN_MODE_BLACK_MARK  7
#define OB_GEN_MODE_BOOKSHOP	8
#define OB_GEN_MODE_CHEST		11
#define OB_GEN_MODE_QUEST		12
#define OB_GEN_STORE_HEAD		1
#define OB_GEN_STORE_TAIL		8
#define OB_GEN_MODE_RANDART		13


#define CHEST_LEVEL			130
#define QUEST_LEVEL			131



/*** Important artifact indexes (see "lib/edit/artifact.txt") ***/

#define ART_MORGOTH			138
#define ART_GROND			139

/*
 * Maximum length of artifact names
 */
#define MAX_LEN_ART_NAME 30

#define S_WORD 26
#define E_WORD S_WORD

/*themed drops*/
#define DROP_TYPE_UNTHEMED				0
#define DROP_TYPE_GOLD					1
#define DROP_TYPE_POTION				2
#define DROP_TYPE_ROD_WAND_STAFF		3
#define DROP_TYPE_SCROLL				4
#define DROP_TYPE_SHIELD				5
#define DROP_TYPE_WEAPON				6
#define DROP_TYPE_ARMOR					7
#define DROP_TYPE_BOOTS					8
#define DROP_TYPE_BOW					9
#define DROP_TYPE_CLOAK					10
#define DROP_TYPE_GLOVES				11
#define DROP_TYPE_HAFTED				12
#define DROP_TYPE_HEADGEAR				13
#define DROP_TYPE_JEWELRY				14
#define DROP_TYPE_DRAGON_ARMOR			15
#define DROP_TYPE_CHEST					16
#define DROP_TYPE_DUNGEON_MAGIC_BOOK	17
#define DROP_TYPE_DUNGEON_PRAYER_BOOK	18
#define DROP_TYPE_DUNGEON_DRUID_BOOK	19
#define DROP_TYPE_EDGED					20
#define DROP_TYPE_POLEARM				21
#define DROP_TYPE_DIGGING				22
#define DROP_TYPE_MORIA_ITEMS			23
#define DROP_TYPE_MORIA_WEAPONS			24
#define DROP_TYPE_MORIA_ARMOR_BODY		25
#define DROP_TYPE_MORIA_ARMOR_OTHER		26


/*** Object "tval" and "sval" codes ***/


/*
 * The values for the "tval" field of various objects.
 *
 * This value is the primary means by which items are sorted in the
 * player inventory, followed by "sval" and "cost".
 *
 * Note that a "BOW" with tval = 19 and sval S = 10*N+P takes a missile
 * weapon with tval = 16+N, and does (xP) damage when so combined.  This
 * fact is not actually used in the source, but it kind of interesting.
 *
 * Note that as of 2.7.8, the "item flags" apply to all items, though
 * only armor and weapons and a few other items use any of these flags.
 */

#define TV_SKELETON      1	/* Skeletons ('s') */
#define TV_BOTTLE		 2	/* Empty bottles ('!') */
#define TV_JUNK          3	/* Sticks, Pottery, etc ('~') */
#define TV_SPIKE         5	/* Spikes ('~') */
#define TV_CHEST         7	/* Chests ('~') */
#define TV_SHOT			16	/* Ammo for slings */
#define TV_ARROW        17	/* Ammo for bows */
#define TV_BOLT         18	/* Ammo for x-bows */
#define TV_BOW          19	/* Slings/Bows/Xbows */
#define TV_DIGGING      20	/* Shovels/Picks */
#define TV_HAFTED       21	/* Priest Weapons */
#define TV_POLEARM      22	/* Axes and Pikes */
#define TV_SWORD        23	/* Edged Weapons */
#define TV_BOOTS        30	/* Boots */
#define TV_GLOVES       31	/* Gloves */
#define TV_HELM         32	/* Helms */
#define TV_CROWN        33	/* Crowns */
#define TV_SHIELD       34	/* Shields */
#define TV_CLOAK        35	/* Cloaks */
#define TV_SOFT_ARMOR   36	/* Soft Armor */
#define TV_HARD_ARMOR   37	/* Hard Armor */
#define TV_DRAG_ARMOR	38	/* Dragon Scale Mail */
#define TV_LIGHT         39	/* Lights (including Specials) */
#define TV_AMULET       40	/* Amulets (including Specials) */
#define TV_DRAG_SHIELD	41	/* Dragon Scale Shield*/
#define TV_RING         45	/* Rings (including Specials) */
#define TV_STAFF        55
#define TV_WAND         65
#define TV_ROD          66
#define TV_SCROLL       70
#define TV_PARCHMENT    71  /* Scrap of a scroll collected by the adventurer's guild */
#define TV_POTION       75
#define TV_FLASK        77
#define TV_FOOD         80
#define TV_MAGIC_BOOK   90
#define TV_PRAYER_BOOK  91
#define TV_DRUID_BOOK	92
#define TV_GOLD         100	/* Gold can only be picked up by players */

/* The "sval" codes for TV_JUNK */
#define SV_SKELETON_RAT				1
#define SV_SKELETON_CENTIPEDE		2
#define SV_EMPTY_BOTTLE				4
#define SV_POTTERY_SHARDS			5
#define SV_SKELETON_HUMAN			7
#define SV_SKELETON_DWARF			8
#define SV_SKELETON_ELF				9
#define SV_SKELETON_GNOME			10
#define SV_BROKEN_TEETH				11
#define SV_BROKEN_BONE				12
#define SV_BROKEN_STICK				13



/* The "sval" codes for TV_SHOT/TV_ARROW/TV_BOLT */
#define SV_AMMO_LIGHT		0	/* pebbles */
#define SV_AMMO_NORMAL		1	/* shots, arrows, bolts */
#define SV_AMMO_HEAVY		2	/* seeker arrows and bolts */


/* The "sval" codes for TV_BOW (note information in "sval") */
#define SV_SLING			2	/* (x2) */
#define SV_SHORT_BOW		12	/* (x2) */
#define SV_LONG_BOW			13	/* (x3) */
#define SV_COMPOSITE_BOW	14	/* (x3) */
#define SV_LIGHT_XBOW		23	/* (x3) */
#define SV_HEAVY_XBOW		24	/* (x4) */

/* The "sval" codes for TV_DIGGING */
#define SV_SHOVEL			1
#define SV_PICK_DWARVEN		2
#define SV_PICK				4
#define SV_MATTOCK			7

/* The "sval" values for TV_HAFTED */
#define SV_CAT_NINE_TAILS		1	/* 1d3 */
#define SV_WHIP					2	/* 1d6 */
#define SV_QUARTERSTAFF			3	/* 1d9 */
#define SV_CLUB_WOODEN			4	/* 1d9 */
#define SV_MACE					5	/* 2d4 */
#define SV_BALL_AND_CHAIN		6	/* 2d4 */
#define SV_WAR_HAMMER			8	/* 3d3 */
#define SV_LUCERN_HAMMER		10	/* 2d5 */
#define SV_THROWING_HAMMER		11	/* 2d5 */
#define SV_MORNING_STAR			12	/* 2d6 */
#define SV_FLAIL				13	/* 2d6 */
#define SV_FLAIL_TWO_HANDED		14	/* 3d6 */
#define SV_LEAD_FILLED_MACE		15	/* 3d4 */
#define SV_MACE_OF_DISRUPTION	20	/* 5d8 */
#define SV_GROND				50	/* 3d4 */

/* The "sval" values for TV_POLEARM */
#define SV_SPEAR				2	/* 1d6 */
#define SV_JAVELIN				3	/* 1d6 */
#define SV_AWL_PIKE				4	/* 1d8 */
#define SV_TRIDENT				5	/* 1d9 */
#define SV_LANCE				6	/* 1d9 */
#define SV_PIKE					8	/* 2d5 */
#define SV_LUCERNE_HAMMER		9	/* 2d5 */
#define SV_BEAKED_AXE			10	/* 2d6 */
#define SV_BROAD_AXE			11	/* 2d6 */
#define SV_THROWING_AXE			12	/* 2d4 */
#define SV_GLAIVE				13	/* 2d6 */
#define SV_HALBERD				15	/* 3d4 */
#define SV_FAUCHARD				16	/* 3d4 */
#define SV_SCYTHE				17	/* 5d3 */
#define SV_BATTLE_AXE			22	/* 2d8 */
#define SV_BATTLE_AXE_EUROPEAN	23	/* 3d4 */
#define SV_GREAT_AXE			25	/* 4d4 */
#define SV_LOCHABER_AXE			28	/* 3d8 */
#define SV_SCYTHE_OF_SLICING	30	/* 8d4 */

/* The "sval" codes for TV_SWORD */
#define SV_BROKEN_DAGGER		1	/* 1d1 */
#define SV_BROKEN_SWORD			2	/* 1d2 */
#define SV_DAGGER_STILLETO		3	/* 1d4 */
#define SV_DAGGER				4	/* 1d4 */
#define SV_MAIN_GAUCHE			5	/* 1d5 */
#define SV_DAGGER_MISERICORDE	6	/* 1d4 */
#define SV_RAPIER				7	/* 1d6 */
#define SV_SMALL_SWORD			8	/* 1d6 */
#define SV_SHORT_SWORD			10	/* 1d7 */
#define SV_SABRE				11	/* 1d7 */
#define SV_CUTLASS				12	/* 1d7 */
#define SV_SWORD_THRUSTING		13	/* 1d6 */
#define SV_FOIL					14	/* 1d5 */
#define SV_SWORD_THRUSTING_BASELARD	15	/* 1d7 */
#define SV_BROAD_SWORD			16	/* 2d5 */
#define SV_LONG_SWORD			17	/* 2d5 */
#define SV_SCIMITAR				18	/* 2d5 */
#define SV_KATANA				20	/* 3d4 */
#define SV_BASTARD_SWORD		21	/* 3d4 */
#define SV_BACKSWORD			24  /* 1d9 */
#define SV_TWO_HANDED_SWORD		25	/* 3d6 */
#define SV_TWO_HANDED_SWORD_ESPADON		26	/* 3d6 */
#define SV_TWO_HANDED_SWORD_FLAMBERGE		27	/* 3d6 */
#define SV_EXECUTIONERS_SWORD	28	/* 4d5 */
#define SV_TWO_HANDED_SWORD_NO_DACHI		29	/* 4d4 */
#define SV_BLADE_OF_CHAOS		30	/* 6d5 */
#define SV_TWO_HANDED_SWORD_ZWEIHANDER		31	/* 4d6 */

/* The "sval" codes for TV_SHIELD */
#define SV_SMALL_LEATHER_SHIELD		2
#define SV_SMALL_METAL_SHIELD		3
#define SV_LARGE_LEATHER_SHIELD		4
#define SV_LARGE_METAL_SHIELD		5
#define SV_MEDIUM_LEATHER_SHIELD	6
#define SV_MEDIUM_METAL_SHIELD		7
#define SV_SHIELD_OF_DEFLECTION		10

/* The "sval" codes for TV_HELM */
#define SV_SOFT_LEATHER_CAP		1
#define SV_HARD_LEATHER_CAP		2
#define SV_METAL_CAP			3
#define SV_IRON_HELM			5
#define SV_STEEL_HELM			6
#define SV_SILVER_CROWN			9
#define SV_IRON_CROWN			10
#define SV_GOLDEN_CROWN			11
#define SV_JEWELED_CROWN		12
#define SV_MORGOTH				50

/* The "sval" codes for TV_BOOTS */
#define SV_PAIR_OF_SOFT_LEATHER_SHOES	1
#define SV_PAIR_OF_SOFT_LEATHER_BOOTS	2
#define SV_PAIR_OF_HARD_LEATHER_BOOTS	4
#define SV_PAIR_OF_METAL_SHOD_BOOTS		6

/* The "sval" codes for TV_CLOAK */
#define SV_CLOAK					1
#define SV_SHADOW_CLOAK				6

/* The "sval" codes for TV_GLOVES */
#define SV_SET_OF_LEATHER_GLOVES	1
#define SV_SET_OF_GAUNTLETS			2
#define SV_SET_OF_CESTI				5

/* The "sval" codes for TV_SOFT_ARMOR */
#define SV_FILTHY_RAG				1
#define SV_ROBE						2
#define SV_SOFT_LEATHER_ARMOR		4
#define SV_SOFT_STUDDED_LEATHER		5
#define SV_HARD_LEATHER_ARMOR		6
#define SV_HARD_STUDDED_LEATHER		7
#define SV_LEATHER_SCALE_MAIL		11
#define SV_LEATHER_RING_MAIL_HARD	12
#define SV_LEATHER_RING_MAIL_SOFT	13
#define SV_WOVEN_CORD_ARMOR			14

/* The "sval" codes for TV_HARD_ARMOR */
#define SV_RUSTY_CHAIN_MAIL			1	/* 14- */
#define SV_METAL_SCALE_MAIL			3	/* 13 */
#define SV_CHAIN_MAIL				4	/* 14 */
#define SV_AUGMENTED_CHAIN_MAIL		6	/* 16 */
#define SV_DOUBLE_CHAIN_MAIL		7	/* 16 */
#define SV_METAL_BRIGANDINE_ARMOUR	9	/* 19 */
#define SV_PARTIAL_PLATE_ARMOUR		12	/* 22 */
#define SV_METAL_LAMELLAR_ARMOUR	13	/* 23 */
#define SV_FULL_PLATE_ARMOUR		15	/* 25 */
#define SV_RIBBED_PLATE_ARMOUR		18	/* 28 */
#define SV_MITHRIL_CHAIN_MAIL		20	/* 28+ */
#define SV_BAR_CHAIN_MAIL			21	/* 28+ */
#define SV_LAMINATED_ARMOR			22	/* 28+ */
#define SV_MITHRIL_PLATE_MAIL		25	/* 35+ */
#define SV_ADAMANTITE_PLATE_MAIL	30	/* 40+ */

/* The "sval" codes for TV_DRAG_ARMOR and SHIELDS*/
#define SV_DRAG_YOUNG		1
#define SV_DRAG_MEDIUM		2
#define SV_DRAG_ANCIENT		3
#define SV_DRAG_WYRM		4


/* The "ego" codes for TV_DRAG_ARMOR and TV_DRAG_SHIELD*/
#define EGO_DRAGON_POWER		18
#define EGO_DRAGON_MULTIHUED	19
#define EGO_DRAGON_BALANCE		20
#define EGO_DRAGON_LAW			21
#define EGO_DRAGON_CHAOS		22
#define EGO_DRAGON_GREEN		23
#define EGO_DRAGON_PSEUDO		24
#define EGO_DRAGON_BLACK		25
#define EGO_DRAGON_BLUE			26
#define EGO_DRAGON_WHITE		27
#define EGO_DRAGON_RED			28
#define EGO_DRAGON_BRONZE		29
#define EGO_DRAGON_GOLD			30





/* The sval codes for TV_LIGHT */
#define SV_LIGHT_TORCH		0
#define SV_LIGHT_LANTERN		1
#define SV_LIGHT_GALADRIEL	4
#define SV_LIGHT_ELENDIL		5
#define SV_LIGHT_THRAIN		6
#define SV_LIGHT_PALANTIR	7

/* The "sval" codes for TV_AMULET */
#define SV_AMULET_DOOM			0
#define SV_AMULET_TELEPORT		1
#define SV_AMULET_ADORNMENT		2
#define SV_AMULET_SLOW_DIGEST	3
#define SV_AMULET_RESIST_ACID	4
#define SV_AMULET_SEARCHING		5
#define SV_AMULET_WISDOM		6
#define SV_AMULET_CHARISMA		7
#define SV_AMULET_THE_MAGI		8
#define SV_AMULET_SUSTENANCE	9
#define SV_AMULET_CARLAMMAS		10
#define SV_AMULET_INGWE			11
#define SV_AMULET_DWARVES		12
#define SV_AMULET_ESP			13
#define SV_AMULET_RESIST		14
#define SV_AMULET_REGEN			15
#define SV_AMULET_ELESSAR		16
#define SV_AMULET_EVENSTAR		17
#define SV_AMULET_DEVOTION		18
#define SV_AMULET_WEAPONMASTERY	19
#define SV_AMULET_TRICKERY		20
#define SV_AMULET_INFRAVISION		21
#define SV_AMULET_RESIST_LIGHTNING  22
#define SV_AMULET_WOE				23


/* The sval codes for TV_RING */
#define SV_RING_WOE				0
#define SV_RING_AGGRAVATION		1
#define SV_RING_WEAKNESS		2
#define SV_RING_STUPIDITY		3
#define SV_RING_TELEPORTATION	4
/* xxx */

#define SV_RING_SLOW_DIGESTION	6
#define SV_RING_FEATHER_FALL	7
#define SV_RING_RESIST_FIRE		8
#define SV_RING_RESIST_COLD		9
#define SV_RING_SUSTAIN_STR		10
#define SV_RING_SUSTAIN_INT		11
#define SV_RING_SUSTAIN_WIS		12
#define SV_RING_SUSTAIN_DEX		13
#define SV_RING_SUSTAIN_CON		14
#define SV_RING_SUSTAIN_CHR		15
#define SV_RING_PROTECTION		16
#define SV_RING_ACID			17
#define SV_RING_FLAMES			18
#define SV_RING_ICE				19
#define SV_RING_RESIST_POIS		20
#define SV_RING_FREE_ACTION		21
#define SV_RING_SEE_INVIS		22
#define SV_RING_SEARCHING		23
#define SV_RING_STR				24
#define SV_RING_INT				25
#define SV_RING_DEX				26
#define SV_RING_CON				27
#define SV_RING_ACCURACY		28
#define SV_RING_DAMAGE			29
#define SV_RING_SLAYING			30
#define SV_RING_SPEED			31
#define SV_RING_BARAHIR			32
#define SV_RING_TULKAS			33
#define SV_RING_NARYA			34
#define SV_RING_NENYA			35
#define SV_RING_VILYA			36
#define SV_RING_POWER			37
#define SV_RING_LIGHTNING		38
#define SV_RING_RESIST_NETHER	39
#define SV_RING_LORD_PROT_ACID	40
#define SV_RING_LORD_PROT_FIRE	41
#define SV_RING_LORD_PROT_COLD	42
#define SV_RING_ADORNMENT		43


/* The "sval" codes for TV_STAFF */
#define SV_STAFF_DARKNESS		0
#define SV_STAFF_SLOWNESS		1
#define SV_STAFF_HASTE_MONSTERS	2
#define SV_STAFF_SUMMONING		3
#define SV_STAFF_TELEPORTATION	4
#define SV_STAFF_IDENTIFY		5

#define SV_STAFF_STARLIGHT		7
#define SV_STAFF_LIGHT			8
#define SV_STAFF_MAPPING		9
#define SV_STAFF_DETECT_GOLD	10
#define SV_STAFF_DETECT_ITEM	11
#define SV_STAFF_DETECT_TRAP	12
#define SV_STAFF_DETECT_DOOR	13
#define SV_STAFF_DETECT_INVIS	14
#define SV_STAFF_DETECT_EVIL	15
#define SV_STAFF_CURE_LIGHT		16
#define SV_STAFF_CURING			17
#define SV_STAFF_HEALING		18
#define SV_STAFF_THE_MAGI		19
#define SV_STAFF_SLEEP_MONSTERS	20
#define SV_STAFF_SLOW_MONSTERS	21
#define SV_STAFF_SPEED			22
#define SV_STAFF_PROBING		23
#define SV_STAFF_DISPEL_EVIL	24
#define SV_STAFF_POWER			25
#define SV_STAFF_HOLINESS		26
#define SV_STAFF_BANISHMENT		27
#define SV_STAFF_EARTHQUAKES	28
#define SV_STAFF_DESTRUCTION	29
#define SV_STAFF_MASS_IDENTIFY	30
#define SV_STAFF_MASS_POLYMORPH	31
#define SV_STAFF_REMOVE_CURSE	32


/* The "sval" codes for TV_WAND */
#define SV_WAND_HEAL_MONSTER	0
#define SV_WAND_HASTE_MONSTER	1
#define SV_WAND_CLONE_MONSTER	2
#define SV_WAND_TELEPORT_AWAY	3
#define SV_WAND_DISARMING		4
#define SV_WAND_TRAP_DOOR_DEST	5
#define SV_WAND_STONE_TO_MUD	6
#define SV_WAND_LIGHT			7
#define SV_WAND_SLEEP_MONSTER	8
#define SV_WAND_SLOW_MONSTER	9
#define SV_WAND_CONFUSE_MONSTER	10
#define SV_WAND_FEAR_MONSTER	11
#define SV_WAND_DRAIN_LIFE		12
#define SV_WAND_POLYMORPH		13
#define SV_WAND_STINKING_CLOUD	14
#define SV_WAND_MAGIC_MISSILE	15
#define SV_WAND_ACID_BOLT		16
#define SV_WAND_ELEC_BOLT		17
#define SV_WAND_FIRE_BOLT		18
#define SV_WAND_COLD_BOLT		19
#define SV_WAND_ACID_BALL		20
#define SV_WAND_ELEC_BALL		21
#define SV_WAND_FIRE_BALL		22
#define SV_WAND_COLD_BALL		23
#define SV_WAND_WONDER			24
#define SV_WAND_ANNIHILATION	25
#define SV_WAND_DRAGON_FIRE		26
#define SV_WAND_DRAGON_COLD		27
#define SV_WAND_DRAGON_BREATH	28
#define SV_WAND_WALL_BUILDING	29



/* The "sval" codes for TV_ROD */
#define SV_ROD_DETECT_TRAP		0
#define SV_ROD_DETECT_DOOR		1
#define SV_ROD_IDENTIFY			2
#define SV_ROD_RECALL			3
#define SV_ROD_ILLUMINATION		4
#define SV_ROD_MAPPING			5
#define SV_ROD_DETECTION		6
#define SV_ROD_PROBING			7
#define SV_ROD_CURING			8
#define SV_ROD_HEALING			9
#define SV_ROD_RESTORATION		10
#define SV_ROD_SPEED			11
#define SV_ROD_STONE_TO_MUD		12
#define SV_ROD_TELEPORT_AWAY	13
#define SV_ROD_DISARMING		14
#define SV_ROD_LIGHT			15
#define SV_ROD_SLEEP_MONSTER	16
#define SV_ROD_SLOW_MONSTER		17
#define SV_ROD_DRAIN_LIFE		18
#define SV_ROD_POLYMORPH		19
#define SV_ROD_ACID_BOLT		20
#define SV_ROD_ELEC_BOLT		21
#define SV_ROD_FIRE_BOLT		22
#define SV_ROD_COLD_BOLT		23
#define SV_ROD_ACID_BALL		24
#define SV_ROD_ELEC_BALL		25
#define SV_ROD_FIRE_BALL		26
#define SV_ROD_COLD_BALL		27
#define SV_ROD_STAR_IDENTIFY	28
#define SV_ROD_MASS_IDENTIFY	29


/* The "sval" codes for TV_SCROLL */

#define SV_SCROLL_DARKNESS				0
#define SV_SCROLL_AGGRAVATE_MONSTER		1
#define SV_SCROLL_CURSE_ARMOR			2
#define SV_SCROLL_CURSE_WEAPON			3
#define SV_SCROLL_SUMMON_MONSTER		4
#define SV_SCROLL_SUMMON_UNDEAD			5
#define SV_SCROLL_SUMMON_UNIQUE			6
#define SV_SCROLL_TRAP_CREATION			7
#define SV_SCROLL_PHASE_DOOR			8
#define SV_SCROLL_TELEPORT				9
#define SV_SCROLL_TELEPORT_LEVEL		10
#define SV_SCROLL_WORD_OF_RECALL		11
#define SV_SCROLL_IDENTIFY				12
#define SV_SCROLL_STAR_IDENTIFY			13
#define SV_SCROLL_REMOVE_CURSE			14
#define SV_SCROLL_STAR_REMOVE_CURSE		15
#define SV_SCROLL_ENCHANT_ARMOR			16
#define SV_SCROLL_ENCHANT_WEAPON_TO_HIT	17
#define SV_SCROLL_ENCHANT_WEAPON_TO_DAM	18
/* xxx enchant missile? */
#define SV_SCROLL_STAR_ENCHANT_ARMOR	20
#define SV_SCROLL_STAR_ENCHANT_WEAPON	21
#define SV_SCROLL_RECHARGING			22
#define SV_SCROLL_STAR_RECHARGING		23
#define SV_SCROLL_LIGHT					24
#define SV_SCROLL_MAPPING				25
#define SV_SCROLL_DETECT_GOLD			26
#define SV_SCROLL_DETECT_ITEM			27
#define SV_SCROLL_DETECT_TRAP			28
#define SV_SCROLL_DETECT_DOOR			29
#define SV_SCROLL_DETECT_INVIS			30
/* xxx (detect evil?) */
#define SV_SCROLL_SATISFY_HUNGER		32
#define SV_SCROLL_BLESSING				33
#define SV_SCROLL_HOLY_CHANT			34
#define SV_SCROLL_HOLY_PRAYER			35
#define SV_SCROLL_MONSTER_CONFUSION		36
#define SV_SCROLL_PROTECTION_FROM_EVIL	37
#define SV_SCROLL_RUNE_OF_PROTECTION	38
#define SV_SCROLL_TRAP_DOOR_DESTRUCTION	39
#define SV_SCROLL_CREATE_MONSTER_TRAP	40
#define SV_SCROLL_STAR_DESTRUCTION		41
#define SV_SCROLL_DISPEL_UNDEAD			42
#define SV_SCROLL_CREATE_RANDART		43
#define SV_SCROLL_BANISHMENT			44
#define SV_SCROLL_MASS_BANISHMENT		45
#define SV_SCROLL_ACQUIREMENT			46
#define SV_SCROLL_STAR_ACQUIREMENT		47
#define SV_SCROLL_CREATE_FOOD			48
#define SV_SCROLL_CREATE_DOORS			49
#define SV_SCROLL_SLEEP_MONSTER			50

#define SV_PARCHMENT_FRAGMENT			1

/* The "sval" codes for TV_POTION */
#define SV_POTION_WATER				0
#define SV_POTION_APPLE_JUICE		1
#define SV_POTION_SLIME_MOLD		2
/* xxx (fixed color) */
#define SV_POTION_SLOWNESS			4
#define SV_POTION_SALT_WATER		5
#define SV_POTION_POISON			6
#define SV_POTION_BLINDNESS			7
/* xxx */
#define SV_POTION_CONFUSION			9
/* xxx */
#define SV_POTION_SLEEP				11
/* xxx */
#define SV_POTION_LOSE_MEMORIES		13
#define SV_POTION_DRAIN_MANA		14
#define SV_POTION_RUINATION			15
#define SV_POTION_DEC_STR			16
#define SV_POTION_DEC_INT			17
#define SV_POTION_DEC_WIS			18
#define SV_POTION_DEC_DEX			19
#define SV_POTION_DEC_CON			20
#define SV_POTION_DEC_CHR			21
#define SV_POTION_DETONATIONS		22
#define SV_POTION_DEATH				23
#define SV_POTION_INFRAVISION		24
#define SV_POTION_DETECT_INVIS		25
#define SV_POTION_SLOW_POISON		26
#define SV_POTION_CURE_POISON		27
#define SV_POTION_BOLDNESS			28
#define SV_POTION_SPEED				29
#define SV_POTION_RESIST_HEAT		30
#define SV_POTION_RESIST_COLD		31
#define SV_POTION_HEROISM			32
#define SV_POTION_BERSERK_STRENGTH	33
#define SV_POTION_CURE_LIGHT		34
#define SV_POTION_CURE_SERIOUS		35
#define SV_POTION_CURE_CRITICAL		36
#define SV_POTION_HEALING			37
#define SV_POTION_STAR_HEALING		38
#define SV_POTION_LIFE				39
#define SV_POTION_RESTORE_MANA		40
#define SV_POTION_RESTORE_EXP		41
#define SV_POTION_RES_STR			42
#define SV_POTION_RES_INT			43
#define SV_POTION_RES_WIS			44
#define SV_POTION_RES_DEX			45
#define SV_POTION_RES_CON			46
#define SV_POTION_RES_CHR			47
#define SV_POTION_INC_STR			48
#define SV_POTION_INC_INT			49
#define SV_POTION_INC_WIS			50
#define SV_POTION_INC_DEX			51
#define SV_POTION_INC_CON			52
#define SV_POTION_INC_CHR			53
#define SV_POTION_INVULNERABILITY	54
#define SV_POTION_AUGMENTATION			55
#define SV_POTION_ENLIGHTENMENT			56
#define SV_POTION_STAR_ENLIGHTENMENT	57
#define SV_POTION_SELF_KNOWLEDGE		58
#define SV_POTION_EXPERIENCE			59
#define SV_POTION_RESIST_ACID			60
#define SV_POTION_RESIST_ELECTRICITY	61
#define SV_POTION_RESIST_POISON			62
#define SV_POTION_RESISTANCE			63


/* The "sval" codes for TV_FOOD */
#define SV_FOOD_POISON			0
#define SV_FOOD_BLINDNESS		1
#define SV_FOOD_PARANOIA		2
#define SV_FOOD_CONFUSION		3
#define SV_FOOD_HALLUCINATION	4
#define SV_FOOD_PARALYSIS		5
#define SV_FOOD_WEAKNESS		6
#define SV_FOOD_SICKNESS		7
#define SV_FOOD_STUPIDITY		8
#define SV_FOOD_NAIVETY			9
#define SV_FOOD_UNHEALTH		10
#define SV_FOOD_DISEASE			11
#define SV_FOOD_CURE_POISON		12
#define SV_FOOD_CURE_BLINDNESS	13
#define SV_FOOD_CURE_PARANOIA	14
#define SV_FOOD_CURE_CONFUSION	15
#define SV_FOOD_CURE_SERIOUS	16
#define SV_FOOD_RESTORE_STR		17
#define SV_FOOD_RESTORE_CON		18
#define SV_FOOD_RESTORING		19
#define SV_FOOD_FIRST_AID		20
#define SV_FOOD_MINOR_CURES		21
#define SV_FOOD_LIGHT_CURES		22
#define SV_FOOD_RESTORATION		23
#define SV_FOOD_MAJOR_CURES		24
#define SV_FOOD_RATION			35
#define SV_FOOD_SLIME_MOLD		36
#define SV_FOOD_WAYBREAD		37
#define SV_FOOD_BISCUIT			38
#define SV_FOOD_BEEF_JERKY		39
#define SV_FOOD_FINE_ALE		40
#define SV_FOOD_FINE_WINE		41
#define SV_FOOD_FINE_MUSH		42

/*gold, incomplete list defined primarily for the mimics*/

#define SV_GOLD_COPPER			3
#define SV_GOLD_SILVER			6
#define SV_GOLD_GARNET			8
#define SV_GOLD_GOLD			11
#define SV_GOLD_OPALS			12
#define SV_GOLD_SAPPHIRES		13
#define SV_GOLD_RUBIES			14
#define SV_GOLD_DIAMOND			15
#define SV_GOLD_EMERALD			16
#define SV_GOLD_MITHRIL			17
#define SV_GOLD_ADAMANTITE		18


/*
 * Special "sval" limit -- first "normal" food
 */
#define SV_FOOD_MIN_FOOD	32

/*
 * Special "sval" limit -- first "aimed" rod
 */
#define SV_ROD_MIN_DIRECTION	12

/*
 * Special "sval" limit -- first "large" chest
 */
#define SV_CHEST_MIN_SMALL	1

/*
 * Special "sval" limit -- first "large" chest
 */
#define SV_CHEST_MIN_LARGE	5

/*
 * Special "sval" for jeweled chest
 */
#define SV_CHEST_JEWELED_LARGE	8

/*
 * Special "sval" limit -- first "good" magic/prayer book
 */
#define SV_BOOK_MIN_GOOD	4


/*
 * Special "k_info" hard coded values -
 */
#define MAX_GOLD_NPPANGBAND		18	/* Number of "gold" entries */
#define MAX_GOLD_NPPMORIA		17	/* Number of "gold" entries */


/*Squelch Modes for k_info->squelch*/


#define SQUELCH_NEVER   			0 /*allow pickup, but defer to OPT_always_pickup*/
#define NO_SQUELCH_NEVER_PICKUP		1 /*never pickup, override OPT_always_pickup*/
#define NO_SQUELCH_ALWAYS_PICKUP 	2/*always pickup, override pickup and floor query options*/
#define SQUELCH_ALWAYS  			3 /*destroy when player walks over*/
#define SQUELCH_OPT_MAX				4


/*
 * These are the various levels of quality squelching supported by the game.
 * Less concisely:
 * 0 ---> No squelching
 * 1 ---> Squelch cursed items
 * 2 ---> Squelch average and worse items
 * 3 ---> Squelch good and worse items
 * 4 ---> squelch all but artifacts
 * 5 ---> squelch open chests
 */

#define SQUELCH_NONE     		0
#define SQUELCH_CURSED   		1
#define SQUELCH_AVERAGE  		2
#define SQUELCH_GOOD_STRONG     3
#define SQUELCH_GOOD_WEAK		4
#define SQUELCH_ALL      		5
#define SQUELCH_MAX				6

/*number of bytes used in squelch sub-quality array*/
#define SQUELCH_BYTES    24




/*
 * Special "sval" value -- unknown "sval"
 */
#define SV_UNKNOWN			255

/*
 * Bit flags for the "get_item" function
 */
#define USE_EQUIP		0x01	/* Allow equip items */
#define USE_INVEN		0x02	/* Allow inven items */
#define USE_FLOOR		0x04	/* Allow floor items */
#define USE_QUIVER		0x08	/* Allow quiver items, forbid classic equipment */
#define NOUN_VERB		0x10	/* We are choosing the item first, then the command */
#define IS_HARMLESS   	0x20	/* Ignore generic warning inscriptions */
#define QUIVER_FIRST 	0x40	/* Show item prices in item lists */
#define SHOW_FAIL     	0x80 	/* Show device failure in item lists */




/*** Object flags ***/


/*
 * Chest trap flags (see "tables.c")
 */
#define CHEST_LOSE_STR	0x01
#define CHEST_LOSE_CON	0x02
#define CHEST_POISON	0x04
#define CHEST_PARALYZE	0x08
#define CHEST_EXPLODE	0x10
#define CHEST_SUMMON	0x20


/*
 * Special object flags
 */
#define IDENT_SENSE     		0x00000001	/* Item has been "sensed" */
#define IDENT_CONFIRMED_USE		0x00000002	/* Weapon is confirmed to be used (for swap weapons, attacking with bow, shovel, etc )*/
#define IDENT_EMPTY     		0x00000004	/* Item charges are known */
#define IDENT_KNOWN     		0x00000008	/* Item abilities are known */
#define IDENT_STORE     		0x00000010	/* Item is in the inventory of a store */
#define IDENT_MENTAL    		0x00000020	/* Item information is known */
#define IDENT_CURSED    		0x00000040	/* Item is temporarily cursed */
#define IDENT_BROKEN    		0x00000080	/* Item is permanently worthless */
#define IDENT_QUEST     		0x00000100	/* Item is a quest object (quest artifact or mimicing quest monster */
#define IDENT_PERFECT_BALANCE   0x00000200	/* Item has perfect balance */
#define IDENT_HIDE_CARRY		0x00000400	/* Don't reveal the object is being carried by a creature*/
#define IDENT_EFFECT 			0x00000800	/* Know item activation/effect */
#define IDENT_FIRED			    0x00001000	/* Has been used as a missile */
#define IDENT_INDESTRUCT	    0x00002000	/* Tried to destroy it and failed */
#define IDENT_NAME			    0x00004000	/* Know the name of ego or artifact if there is one */
#define IDENT_NOTART		    0x00008000	/* Item is known not to be an artifact */
#define IDENT_QUIVER		    0x00010000	/* Ammo goes in quiver */
#define IDENT_UNUSED_XXX2XXXX   0x00020000	/* Unused */
#define IDENT_UNUSED_XXX4XXXX   0x00040000	/* Unused */
#define IDENT_UNUSED_XXX8XXXX   0x00080000	/* Unused */
#define IDENT_UNUSED_XX1XXXXX   0x00100000	/* Unused */
#define IDENT_UNUSED_XX2XXXXX   0x00200000	/* Unused */
#define IDENT_UNUSED_XX4XXXXX   0x00400000	/* Unused */
#define IDENT_UNUSED_XX8XXXXX   0x00800000	/* Unused */
#define IDENT_UNUSED_X1XXXXXX   0x01000000	/* Unused */
#define IDENT_UNUSED_X2XXXXXX   0x02000000	/* Unused */
#define IDENT_UNUSED_X4XXXXXX   0x04000000	/* Unused */
#define IDENT_UNUSED_X8XXXXXX   0x08000000	/* Unused */
#define IDENT_UNUSED_1XXXXXXX   0x10000000	/* Unused */
#define IDENT_UNUSED_2XXXXXXX   0x20000000	/* Unused */
#define IDENT_UNUSED_4XXXXXXX   0x40000000	/* Unused */
#define IDENT_UNUSED_8XXXXXXX   0x80000000	/* Unused */





/*
 * Number of special inscriptions, plus one.
 */
#define MAX_INSCRIP			12




/*
 * As of 2.7.8, the "object flags" are valid for all objects, and as
 * of 2.7.9, these flags are not actually stored with the object, but
 * rather in the object_kind, ego_item, and artifact structures.
 *
 * Note that "flags1" contains all flags dependant on "pval" (including
 * stat bonuses, but NOT stat sustainers), plus all "extra attack damage"
 * flags (SLAY_XXX and BRAND_XXX).
 *
 * Note that "flags2" contains all "resistances" (including "sustain" flags,
 * immunity flags, and resistance flags).  Note that "free action" and "hold
 * life" are no longer considered to be "immunities".
 *
 * Note that "flags3" contains everything else (including eight good flags,
 * seven unused flags, four bad flags, four damage ignoring flags, six weird
 * flags, and three cursed flags).

 * Kills need to come right after slays for the randart code to work right
 */

#define TR1_STR             0x00000001L /* STR += "pval" */
#define TR1_INT             0x00000002L /* INT += "pval" */
#define TR1_WIS             0x00000004L /* WIS += "pval" */
#define TR1_DEX             0x00000008L /* DEX += "pval" */
#define TR1_CON             0x00000010L /* CON += "pval" */
#define TR1_CHR             0x00000020L /* CHR += "pval" */
#define TR1_TR1XXX1		    0x00000040L /* (reserved) */
#define TR1_TR1XXX2         0x00000080L /* (reserved) */
#define TR1_STEALTH         0x00000100L /* Stealth += "pval" */
#define TR1_SEARCH          0x00000200L /* Search += "pval" */
#define TR1_INFRA           0x00000400L /* Infra += "pval" */
#define TR1_TUNNEL          0x00000800L /* Tunnel += "pval" */
#define TR1_SPEED           0x00001000L /* Speed += "pval" */
#define TR1_BLOWS           0x00002000L /* Blows += "pval" */
#define TR1_SHOTS           0x00004000L /* Shots += "pval" */
#define TR1_MIGHT           0x00008000L /* Might += "pval" */
#define TR1_SLAY_ANIMAL     0x00010000L /* Weapon slays animals */
#define TR1_SLAY_EVIL       0x00020000L /* Weapon slays evil */
#define TR1_SLAY_UNDEAD     0x00040000L /* Weapon slays undead */
#define TR1_SLAY_DEMON      0x00080000L /* Weapon slays demon */
#define TR1_SLAY_ORC        0x00100000L /* Weapon slays orc */
#define TR1_SLAY_TROLL      0x00200000L /* Weapon slays troll */
#define TR1_SLAY_GIANT      0x00400000L /* Weapon slays giant */
#define TR1_SLAY_DRAGON     0x00800000L /* Weapon slays dragon */
#define TR1_KILL_DRAGON     0x01000000L /* Weapon kills dragon */
#define TR1_KILL_DEMON      0x02000000L /* Weapon kills demon */
#define TR1_KILL_UNDEAD     0x04000000L /* Weapon "kills" undead */
#define TR1_BRAND_ACID      0x08000000L /* Weapon has poison brand */
#define TR1_BRAND_ELEC      0x10000000L /* Weapon has acid brand */
#define TR1_BRAND_FIRE      0x20000000L /* Weapon has elec brand */
#define TR1_BRAND_COLD      0x40000000L /* Weapon has fire brand */
#define TR1_BRAND_POIS      0x80000000L /* Weapon has cold brand */


/*TR1 Uber-Flags*/
#define TR1_ALL_STATS	(TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR)


#define TR2_SUST_STR        0x00000001L /* Sustain STR */
#define TR2_SUST_INT        0x00000002L /* Sustain INT */
#define TR2_SUST_WIS        0x00000004L /* Sustain WIS */
#define TR2_SUST_DEX        0x00000008L /* Sustain DEX */
#define TR2_SUST_CON        0x00000010L /* Sustain CON */
#define TR2_SUST_CHR        0x00000020L /* Sustain CHR */
#define TR2_TR2XXX1         0x00000040L /* (reserved) */
#define TR2_TR2XXX2         0x00000080L /* (reserved) */
#define TR2_TR2XXX3         0x00000100L /* (reserved) */
#define TR2_TR2XXX4         0x00000200L /* (reserved) */
#define TR2_TR2XXX5         0x00000400L /* (reserved) */
#define TR2_IM_ACID         0x00000800L /* Immunity to acid */
#define TR2_IM_ELEC         0x00001000L /* Immunity to elec */
#define TR2_IM_FIRE         0x00002000L /* Immunity to fire */
#define TR2_IM_COLD         0x00004000L /* Immunity to cold */
#define TR2_IM_POIS         0x00008000L /* Immunity to poison */
#define TR2_RES_ACID        0x00010000L /* Resist acid */
#define TR2_RES_ELEC        0x00020000L /* Resist elec */
#define TR2_RES_FIRE        0x00040000L /* Resist fire */
#define TR2_RES_COLD        0x00080000L /* Resist cold */
#define TR2_RES_POIS        0x00100000L /* Resist poison */
#define TR2_RES_FEAR        0x00200000L /* Resist fear */
#define TR2_RES_LIGHT       0x00400000L /* Resist light */
#define TR2_RES_DARK        0x00800000L /* Resist dark */
#define TR2_RES_BLIND       0x01000000L /* Resist blind */
#define TR2_RES_CONFU       0x02000000L /* Resist confusion */
#define TR2_RES_SOUND       0x04000000L /* Resist sound */
#define TR2_RES_SHARD       0x08000000L /* Resist shards */
#define TR2_RES_NEXUS       0x10000000L /* Resist nexus */
#define TR2_RES_NETHR       0x20000000L /* Resist nether */
#define TR2_RES_CHAOS       0x40000000L /* Resist chaos */
#define TR2_RES_DISEN       0x80000000L /* Resist disenchant */

/*TR2 Uber-Flags*/
#define TR2_SUST_STATS	(TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS | TR2_SUST_DEX | \
						 TR2_SUST_CON | TR2_SUST_CHR)
#define TR2_RESISTANCE (TR2_RES_ACID | TR2_RES_ELEC | TR2_RES_FIRE | TR2_RES_COLD)
#define TR2_IMMUNE_ALL (TR2_IM_ACID | TR2_IM_ELEC | TR2_IM_FIRE | TR2_IM_COLD | TR2_IM_POIS)



#define TR3_SLOW_DIGEST     0x00000001L /* Slow digest */
#define TR3_FEATHER         0x00000002L /* Feather Falling */
#define TR3_LIGHT            0x00000004L /* Perma-Light */
#define TR3_REGEN           0x00000008L /* Regeneration */
#define TR3_TELEPATHY       0x00000010L /* Telepathy */
#define TR3_SEE_INVIS       0x00000020L /* See Invis */
#define TR3_FREE_ACT        0x00000040L /* Free action */
#define TR3_HOLD_LIFE       0x00000080L /* Hold life */
#define TR3_NEVER_PICKUP    0x00000100L /* monsters can't pickup*/
#define TR3_IRONMAN_ONLY    0x00000200L	/* Ironman object */
#define TR3_STORE_ONLY      0x00000400L /* Do not generate object in a dungeon */
#define TR3_TR3XXX4         0x00000800L
#define TR3_IMPACT          0x00001000L /* Earthquake blows */
#define TR3_TELEPORT        0x00002000L /* Random teleportation */
#define TR3_AGGRAVATE       0x00004000L /* Aggravate monsters */
#define TR3_DRAIN_EXP       0x00008000L /* Experience drain */
#define TR3_IGNORE_ACID     0x00010000L /* Item ignores Acid Damage */
#define TR3_IGNORE_ELEC     0x00020000L /* Item ignores Elec Damage */
#define TR3_IGNORE_FIRE     0x00040000L /* Item ignores Fire Damage */
#define TR3_IGNORE_COLD     0x00080000L /* Item ignores Cold Damage */
#define TR3_THROWING        0x00100000L /* Can be thrown for additional damage */
#define TR3_PERFECT_BALANCE	0x00200000L /* item with perfect balance  */
#define TR3_BLESSED         0x00400000L /* Item has been blessed */
#define TR3_ACTIVATE        0x00800000L /* Item can be activated */
#define TR3_INSTA_ART       0x01000000L /* Item makes an artifact */
#define TR3_EASY_KNOW       0x02000000L /* Item is known if aware */
#define TR3_HIDE_TYPE       0x04000000L /* Item hides description */
#define TR3_SHOW_MODS       0x08000000L /* Item shows Tohit/Todam */
#define TR3_TR3XXX7         0x10000000L /* (reserved) */
#define TR3_LIGHT_CURSE     0x20000000L /* Item has Light Curse */
#define TR3_HEAVY_CURSE     0x40000000L /* Item has Heavy Curse */
#define TR3_PERMA_CURSE     0x80000000L /* Item has Perma Curse */

/*TR3 Uber-Flags*/
#define TR3_IGNORE_ALL (TR3_IGNORE_ACID | TR3_IGNORE_ELEC | TR3_IGNORE_FIRE | TR3_IGNORE_COLD)

/* Native flags - extracted from feature flags */
#define TN1_NATIVE_LAVA 	ELEMENT_LAVA
#define TN1_NATIVE_ICE  	ELEMENT_ICE
#define TN1_NATIVE_OIL   	ELEMENT_OIL
#define TN1_NATIVE_FIRE   	ELEMENT_FIRE
#define TN1_NATIVE_SAND 	ELEMENT_SAND
#define TN1_NATIVE_FOREST	ELEMENT_FOREST
#define TN1_NATIVE_WATER 	ELEMENT_WATER
#define TN1_NATIVE_ACID   	ELEMENT_ACID
#define TN1_NATIVE_MUD 		ELEMENT_MUD
#define TN1_NATIVE_UNUSED 	0x0200




/*
 * Hack -- flag set 1 -- mask for "pval-dependant" flags.
 * Note that all "pval" dependant flags must be in "flags1".
 */
#define TR1_PVAL_MASK \
	(TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | \
	 TR1_CON | TR1_CHR | TR1_TR1XXX1 | TR1_TR1XXX2 | \
	 TR1_STEALTH | TR1_SEARCH | TR1_INFRA | TR1_TUNNEL | \
	 TR1_SPEED | TR1_BLOWS | TR1_SHOTS | TR1_MIGHT)

/*
 * Flag set 3 -- mask for "ignore element" flags.
 */
#define TR3_IGNORE_MASK \
	(TR3_IGNORE_ACID | TR3_IGNORE_ELEC | TR3_IGNORE_FIRE | \
	 TR3_IGNORE_COLD )

/*
 * Stat sustain flags
 */
#define TR1_STAT_MOD_MASK \
	(0L)

#define TR2_STAT_MOD_MASK \
	(TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS | TR2_SUST_DEX | \
		  TR2_SUST_CON | TR2_SUST_CHR)

#define TR3_STAT_MOD_MASK \
	(0L)

#define TN1_STAT_MOD_MASK \
		(0L)

/*
 * Stat add flags
 */
#define TR1_STAT_SUST_MASK \
	(TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR )

#define TR2_STAT_SUST_MASK \
	(0L)

#define TR3_STAT_SUST_MASK \
	(0L)

#define TN1_STAT_SUST_MASK \
		(0L)


/*
 * Immunity flags
 */
#define TR1_IMMUNITIES_MASK \
	(0L)

#define TR2_IMMUNITIES_MASK \
	(TR2_IM_ACID | TR2_IM_ELEC | TR2_IM_FIRE | TR2_IM_COLD | TR2_IM_POIS)

#define TR3_IMMUNITIES_MASK \
	(0L)

#define TN1_IMMUNITIES_MASK \
		(0L)


/*
 * Low Resist flags
 */
#define TR1_LOW_RESIST_MASK \
	(0L)

#define TR2_LOW_RESIST_MASK \
	(TR2_RES_ACID | TR2_RES_ELEC | TR2_RES_FIRE | TR2_RES_COLD)

#define TR3_LOW_RESIST_MASK \
	(0L)

#define TN1_LOW_RESIST_MASK \
		(0L)

/*
 * Medium Resist flags
 */
#define TR1_MED_RESIST_MASK \
	(0L)

#define TR2_MED_RESIST_MASK \
	(TR2_RES_FEAR | TR2_RES_LIGHT | TR2_RES_DARK | TR2_RES_BLIND | TR2_RES_CONFU | \
		TR2_RES_SOUND | TR2_RES_SHARD | TR2_RES_NEXUS)

#define TR3_MED_RESIST_MASK \
	(0L)

#define TN1_MED_RESIST_MASK \
		(0L)

/*
 * High Resist flags
 */
#define TR1_HIGH_RESIST_MASK \
	(0L)

#define TR2_HIGH_RESIST_MASK \
	(TR2_RES_NETHR |  TR2_RES_CHAOS | TR2_RES_DISEN)

#define TR3_HIGH_RESIST_MASK \
	(0L)

#define TN1_HIGH_RESIST_MASK \
		(0L)

/*
 * All Resist flags
 */
#define TR1_RESISTANCES_MASK \
	(TR1_LOW_RESIST_MASK |  TR1_MED_RESIST_MASK | TR1_HIGH_RESIST_MASK)

#define TR2_RESISTANCES_MASK \
	(TR2_LOW_RESIST_MASK |  TR2_MED_RESIST_MASK | TR2_HIGH_RESIST_MASK)

#define TR3_RESISTANCES_MASK \
	(TR3_LOW_RESIST_MASK |  TR3_MED_RESIST_MASK | TR3_HIGH_RESIST_MASK)

#define TN1_RESISTANCES_MASK \
		(TN1_LOW_RESIST_MASK |  TN1_MED_RESIST_MASK | TN1_HIGH_RESIST_MASK)

/*low level abilities*/

#define TR1_LOW_ABILITIES_MASK \
	(0L)

#define TR2_LOW_ABILITIES_MASK \
	(0L)

#define TR3_LOW_ABILITIES_MASK \
	(TR3_SLOW_DIGEST | TR3_FEATHER | TR3_LIGHT | TR3_REGEN | TR3_SEE_INVIS | \
	 TR3_SEE_INVIS | TR3_FREE_ACT)

#define TN1_LOW_ABILITIES_MASK \
		(0L)

/*high level abilities*/

#define TR1_HIGH_ABILITIES_MASK \
	(0L)

#define TR2_HIGH_ABILITIES_MASK \
	(0L)

#define TR3_HIGH_ABILITIES_MASK \
	(TR3_TELEPATHY | TR3_HOLD_LIFE)

#define TN1_HIGH_ABILITIES_MASK \
		(0L)


/*all abilities*/

#define TR1_ABILITIES_MASK \
	(TR1_LOW_ABILITIES_MASK | TR1_HIGH_ABILITIES_MASK)

#define TR2_ABILITIES_MASK \
	(TR2_LOW_ABILITIES_MASK | TR2_HIGH_ABILITIES_MASK)

#define TR3_ABILITIES_MASK \
	(TR3_LOW_ABILITIES_MASK | TR3_HIGH_ABILITIES_MASK)

#define TN1_ABILITIES_MASK \
		(TN1_LOW_ABILITIES_MASK | TR3_HIGH_ABILITIES_MASK)


/*Slay weapon types*/

#define TR1_SLAY_MASK \
	(TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD | TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL | \
         TR1_SLAY_GIANT | TR1_SLAY_DRAGON)

#define TR2_SLAY_MASK \
	(0L)

#define TR3_SLAY_MASK \
	(0L)

#define TN1_SLAY_MASK \
		(0L)

/*Kill weapon types*/

#define TR1_KILL_MASK \
	(TR1_KILL_DRAGON | TR1_KILL_DEMON | TR1_KILL_UNDEAD)

#define TR2_KILL_MASK \
	(0L)

#define TR3_KILL_MASK \
	(0L)

#define TN1_KILL_MASK \
		(0L)

/* Elemental Brand weapon types*/

#define TR1_BRAND_MASK \
	(TR1_BRAND_POIS | TR1_BRAND_ACID | TR1_BRAND_ELEC | TR1_BRAND_FIRE | TR1_BRAND_COLD)

#define TR2_BRAND_MASK \
	(0L)

#define TR3_BRAND_MASK \
	(0L)

#define TN1_BRAND_MASK \
		(0L)

/* All weapon Multipliars*/

#define TR1_ALL_WEAPON_EGO_MASK \
	(TR1_SLAY_MASK | TR1_KILL_MASK | TR1_BRAND_MASK)

#define TR2_ALL_WEAPON_EGO_MASK \
	(TR2_SLAY_MASK | TR2_KILL_MASK | TR2_BRAND_MASK)

#define TR3_ALL_WEAPON_EGO_MASK \
	(TR3_SLAY_MASK | TR3_KILL_MASK | TR3_BRAND_MASK)

#define TN1_ALL_WEAPON_EGO_MASK \
		(TN1_SLAY_MASK | TN1_KILL_MASK | TN1_BRAND_MASK)

/* Native to Element types*/

#define TR1_NATIVE_MASK \
	(0L)

#define TR2_NATIVE_MASK \
	(0L)

#define TR3_NATIVE_MASK \
	(0L)

#define TN1_NATIVE_MASK \
		(TERRAIN_MASK)


/*
 * Hack -- special "xtra" object flag info (type)
 */
#define OBJECT_XTRA_STAT_SUSTAIN	1
#define OBJECT_XTRA_TYPE_HIGH_RESIST	2
#define OBJECT_XTRA_TYPE_POWER		3
#define OBJECT_XTRA_TYPE_IMMUNITY	4
#define OBJECT_XTRA_TYPE_STAT_ADD	5
#define OBJECT_XTRA_TYPE_SLAY		6 /*Weapons Only*/
#define OBJECT_XTRA_TYPE_KILL		7 /*Weapons Only*/
#define OBJECT_XTRA_TYPE_BRAND		8 /*Weapons Only*/
#define OBJECT_XTRA_TYPE_LOW_RESIST	9 /*Weapons Only*/
#define OBJECT_XTRA_TYPE_NATIVE		10 /*Footwear Only*/

/*
 * Hack -- special "xtra" object flag info (base flag value)
 */
#define OBJECT_XTRA_BASE_SUSTAIN	TR2_SUST_STR
#define OBJECT_XTRA_BASE_HIGH_RESIST		TR2_RES_POIS
#define OBJECT_XTRA_BASE_POWER		TR3_SLOW_DIGEST
#define OBJECT_XTRA_BASE_IMMUNITY	TR2_IM_ACID
#define OBJECT_XTRA_BASE_STAT_ADD	TR1_STR
#define OBJECT_XTRA_BASE_SLAY		TR1_SLAY_ANIMAL
#define OBJECT_XTRA_BASE_KILL		TR1_KILL_DRAGON
#define OBJECT_XTRA_BASE_BRAND		TR1_BRAND_ACID
#define OBJECT_XTRA_BASE_LOW_RESIST		TR2_RES_ACID
#define OBJECT_XTRA_BASE_NATIVE		TN1_NATIVE_LAVA

/*
 * Hack -- special "xtra" object flag info (number of flags)
 */
#define OBJECT_XTRA_SIZE_SUSTAIN	6
#define OBJECT_XTRA_SIZE_HIGH_RESIST	12
#define OBJECT_XTRA_SIZE_POWER		8
#define OBJECT_XTRA_SIZE_IMMUNITY	5
#define OBJECT_XTRA_SIZE_STAT_ADD	6
#define OBJECT_XTRA_SIZE_SLAY		8
#define OBJECT_XTRA_SIZE_KILL		3
#define OBJECT_XTRA_SIZE_BRAND		5
#define OBJECT_XTRA_SIZE_LOW_RESIST	4
#define OBJECT_XTRA_SIZE_NATIVE		NUM_NATIVE

/*Chance of adding additional flags after the first one*/
#define EXTRA_FLAG_CHANCE			20

#define EGO_AMMO_FLAME		99
#define EGO_AMMO_FROST		100
#define EGO_AMMO_VENOM		97

#define EGO_BRAND_ELEMENTS		41
#define BRAND_OFFSET_FLAME		2
#define BRAND_OFFSET_FROST		3
#define BRAND_OFFSET_VENOM		4

#define EGO_FIREPROOF	141

#define EGO_SHATTERED	143
#define EGO_BLASTED		144



/*
 * Determine if a given inventory item is "aware"
 */
#define object_aware_p(T) \
	(k_info[(T)->k_idx].aware)

/*
 * Determine if a given inventory item is "tried"
 */
#define object_tried_p(T) \
	(k_info[(T)->k_idx].tried)


/*
 * Determine if a given inventory item is "known"
 * Test One -- Check for special "known" tag
 * Test Two -- Check for "Easy Know" + "Aware"
 */
#define object_known_p(T) \
	(((T)->ident & (IDENT_KNOWN)) || \
	 ((k_info[(T)->k_idx].k_flags3 & (TR3_EASY_KNOW)) && \
	  k_info[(T)->k_idx].aware))

 /*
  * Determine if the attr and char should consider the item's flavor
  *
  * Identified scrolls should use their own tile.
  */
#define use_flavor_glyph(T) \
	((k_info[(T)->k_idx].flavor) && \
	 !((k_info[(T)->k_idx].tval == TV_SCROLL) && object_aware_p(T)))


/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_attr(T) \
	(use_flavor_glyph(T) ? \
	 (flavor_info[k_info[(T)->k_idx].flavor].x_attr) : \
	 (k_info[(T)->k_idx].x_attr))

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_attr_default(T) \
       ((k_info[(T)->k_idx].flavor) ? \
        (flavor_info[k_info[(T)->k_idx].flavor].d_attr) : \
        (k_info[(T)->k_idx].d_attr))


/*
 * Return the "attr" for a k_idx.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_type_attr(T) \
	((k_info[T].flavor) ? \
	 (flavor_info[k_info[T].flavor].x_attr) : \
	 (k_info[T].x_attr))

/*
 * Return the "attr" for a k_idx.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_type_attr_default(T) \
       ((k_info[T].flavor) ? \
        (flavor_info[k_info[T].flavor].d_attr) : \
        (k_info[T].d_attr))


		/*
 * Return the "attr" for a given item kind.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_attr(K) \
	(use_flavor_glyph(K) ? \
	 (flavor_info[k_info[(K)].flavor].x_attr) : \
	 (k_info[(K)].x_attr))

/*
 * Return the "char" for a given item kind.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_char(K) \
	(use_flavor_glyph(K) ? \
	 (flavor_info[k_info[(K)].flavor].x_char) : \
	 (k_info[(K)].x_char))


/*
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_char(T) \
	(use_flavor_glyph(T) ? \
	 (flavor_info[k_info[(T)->k_idx].flavor].x_char) : \
	 (k_info[(T)->k_idx].x_char))

/*
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_char_default(T) \
	((k_info[(T)->k_idx].flavor) ? \
	 (flavor_info[k_info[(T)->k_idx].flavor].d_char) : \
	 (k_info[(T)->k_idx].d_char))

/*
 * Return the "char" for a k_idx.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_type_char(T) \
	((k_info[T].flavor) ? \
	 (flavor_info[k_info[T].flavor].x_char) : \
	 (k_info[T].x_char))

/*
 * Return the "char" for a k_idx.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_type_char_default(T) \
       ((k_info[T].flavor) ? \
        (flavor_info[k_info[T].flavor].d_char) : \
        (k_info[T].d_char))

/*
 * Find out if we have to show the prompts for auto(un)inscribe object kinds
 * based on an instance object inscription.
 * We show them if:
 *    The player enabled this feature
 *    The object is aware (to not reveal flavors) and
 *    The object isn't an special artifact template and
 *    The object isn't an identified artifact and
 *    The object isn't ammo.
 */
#define ACCESS_AUTOINSCRIPTIONS(o_ptr) \
(expand_inscribe && \
object_aware_p(o_ptr) && \
!(k_info[(o_ptr)->k_idx].k_flags3 & TR3_INSTA_ART) && \
!(artifact_p(o_ptr) && object_known_p(o_ptr)) && \
!ammo_p(o_ptr))



 /*
 * Rings and Amulets
 */
#define object_is_jewelry(T) \
	(((T)->tval == TV_RING) || ((T)->tval == TV_AMULET))

/* Returns TRUE if T is a torch or a lantern */
#define fuelable_lite_p(T) (((T)->tval == TV_LIGHT) && (!artifact_p(T)))

/*
 * Artifacts use the "art_num" field
 */
#define artifact_p(T) \
	((T)->art_num ? TRUE : FALSE)

#define artifact_known(T) \
	(artifact_p(T) && ( object_known_p(T) || \
            			(T)->discount == INSCRIP_TERRIBLE || \
            			(T)->discount == INSCRIP_INDESTRUCTIBLE || \
            			(T)->discount == INSCRIP_SPECIAL ))

/*
 * Ego-Items use the "ego_name" field
 */
#define ego_item_p(T) \
	((T)->ego_num ? TRUE : FALSE)


/*
 * Broken items.
 */
#define broken_p(T) \
	((T)->ident & (IDENT_BROKEN))

/*
 * Cursed items.
 */
#define cursed_p(T) \
	((T)->ident & (IDENT_CURSED))

/*
 * Ammo.
 */
#define ammo_p(T) \
	(((T)->tval == TV_BOLT) || ((T)->tval == TV_ARROW) || \
	((T)->tval == TV_SHOT))


/* Returns TRUE if T is a torch or a lantern */
#define fuelable_light_p(T) (((T)->tval == TV_LIGHT) && (!artifact_p(T)))




/* Object origins */
#define ORIGIN_NONE             0
#define ORIGIN_BIRTH            1
#define ORIGIN_STORE            2
#define ORIGIN_FLOOR            3
#define ORIGIN_DROP_UNKNOWN     4
#define ORIGIN_DROP_KNOWN       5
#define ORIGIN_REWARD           6
#define ORIGIN_ACQUIRE          7
#define ORIGIN_MORGOTH          8
#define ORIGIN_CHEAT            9
#define ORIGIN_MIXED            10
#define ORIGIN_CHEST            11
#define ORIGIN_MAGIC			12

/*
 * Return TRUE if the given artifact is enabled to use the "easy mental" feature
 */
#define ARTIFACT_EASY_MENTAL(O_PTR) \
        (artifact_p(O_PTR) && \
        !adult_rand_artifacts && \
        ((O_PTR)->art_num < z_info->art_norm_max))

/*** Variables ***/

/*
 * Max sizes of the following arrays.
 */
#define MAX_TITLES     55       /* Used with scrolls (min 48) */


/** The titles of scrolls, ordered by sval. */
extern char scroll_adj[MAX_TITLES][16];

/*
 * Objects in the quiver are stored in groups. Each group has its own set of tags ranging from 0 to 9.
 * The order of the groups is determined by the value of these constants
 */


/*
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */

/*** Constants ***/

/**
 * Modes for object_desc().
 */
#define		ODESC_BASE   0x00   	/*!< Only describe the base name */
#define		ODESC_COMBAT 0x01   	/*!< Also show combat bonuses */
#define		ODESC_EXTRA  0x02   	/*!< Show charges/inscriptions/pvals */
#define		ODESC_STORE  0x04   	/*!< This is an in-store description */
#define		ODESC_PLURAL 0x08   	/*!< Always pluralise */
#define		ODESC_SINGULAR    0x10    /*!< Always singular */
#define		ODESC_SPOIL  0x20    /*!< Display regardless of player knowledge */
#define		ODESC_PREFIX 0x40   	/* */

#define		ODESC_FULL   (ODESC_COMBAT | ODESC_EXTRA)  /*!< Show entire description */


/**
 * Modes for item lists in "show_inven()"  "show_equip()" and "show_floor()"
 */
#define		OLIST_NONE   0x00   /* No options */
#define   	OLIST_WINDOW 0x01   /* Display list in a sub-term (left-align) */
#define   	OLIST_QUIVER 0x02   /* Display quiver lines */
#define   	OLIST_GOLD   0x04   /* Include gold in the list */
#define		OLIST_WEIGHT 0x08   /* Show item weight */
#define		OLIST_PRICE  0x10   /* Show item price */
#define		OLIST_FAIL   0x20    /* Show device failure */



/**
 * Modes for object_info()
 */
#define	OINFO_NONE   = 0x00 /* No options */
#define	OINFO_TERSE  = 0x00 /* Keep descriptions brief, e.g. for dumps */
#define	OINFO_SUBJ   = 0x02 /* Describe object from the character's POV */
#define	OINFO_FULL   = 0x04 /* Treat object as if fully IDd */
#define	OINFO_DUMMY  = 0x08 /* Object does not exist (e.g. knowledge menu) */



/**
 * Pseudo-ID markers.
 */
typedef enum
{
	INSCRIP_NULL = 100,
	INSCRIP_TERRIBLE   =    101,
	INSCRIP_WORTHLESS  =    102,
	INSCRIP_CURSED     =    103,
	INSCRIP_BROKEN     =    104,
	INSCRIP_AVERAGE    =    105,
	INSCRIP_GOOD_STRONG=    106,
	INSCRIP_GOOD_WEAK  =	107,
	INSCRIP_EXCELLENT  =    108,
	INSCRIP_SPECIAL    =    109,
	INSCRIP_UNCURSED   =    110,
	INSCRIP_INDESTRUCTIBLE =111,

	INSCRIP_MAX                  /*!< Maximum number of pseudo-ID markers */
} obj_pseudo_t;

/*
 * Objects in the quiver are stored in groups. Each group has its own set of tags ranging from 0 to 9.
 * The order of the groups is determined by the value of these constants
 */
enum
{
	QUIVER_GROUP_BOLTS = 0,
	QUIVER_GROUP_ARROWS,
	QUIVER_GROUP_SHOTS,
	QUIVER_GROUP_THROWING_WEAPONS,
	MAX_QUIVER_GROUPS
};


#endif /* INCLUDED_OBJECT_H */
