/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* object.h: objects-related definitions */

#ifndef OBJECT_H_INCLUDED
#define OBJECT_H_INCLUDED

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
 * There is an 1/3 (33%) chance that an artifact will be turned into a randart
 * instead...
 */
#define MAKE_RANDART	3

/*
 * Refueling constants
 */
#define FUEL_TORCH	5000	/* Maximum amount of fuel in a torch */
#define FUEL_LAMP	15000   /* Maximum amount of fuel in a lantern */

/*
 * Maximum amount of starting equipment
 */
#define MAX_START_ITEMS	4

/*
 * Number of tval/min-sval/max-sval slots per ego_item
 */
#define EGO_TVALS_MAX 3

/*
 * Important artifact indexes (see "lib/edit/artifact.txt") 
 */

#define ART_POWER			13
#define ART_MORGOTH			34
#define ART_GROND			111

/*
 * Hack -- first "normal" artifact in the artifact list.  All of
 * the artifacts with indexes from 1 to 15 are "special" (lights,
 * rings, amulets), and the ones from 16 to 127 are "normal".
 */
#define ART_MIN_NORMAL		16

/**** Ego-Item indexes (see "lib/edit/ego_item.txt") ****/

enum
{
	/* Nothing */
	/* xxx */

	/* Body Armor */
        EGO_RESIST_ACID = 4,
        EGO_RESIST_ELEC,
        EGO_RESIST_FIRE,
        EGO_RESIST_COLD,
        EGO_RESISTANCE,
        EGO_ELVENKIND,
        EGO_ARMR_VULN,
        EGO_PERMANENCE,
        EGO_ARMR_DWARVEN,
	/* xxx */

	/* Shields */
        EGO_ENDURE_ACID = 16,
        EGO_ENDURE_ELEC,
        EGO_ENDURE_FIRE,
        EGO_ENDURE_COLD,
        EGO_ENDURANCE,
        EGO_SHIELD_ELVENKIND,
        EGO_SHIELD_PRESERVATION,
        EGO_SHIELD_VULN,

	/* Crowns and Helms */
        EGO_INTELLIGENCE,
        EGO_WISDOM,
        EGO_BEAUTY,
        EGO_MAGI,
        EGO_MIGHT,
        EGO_LORDLINESS,
        EGO_SEEING,
        EGO_INFRAVISION,
        EGO_LITE,
        EGO_TELEPATHY,
        EGO_REGENERATION,
        EGO_TELEPORTATION,
        EGO_STUPIDITY,
        EGO_NAIVETY,
        EGO_UGLINESS,
        EGO_SICKLINESS,

	/* Cloaks */
        EGO_PROTECTION,
        EGO_STEALTH,
        EGO_AMAN,
        EGO_CLOAK_MAGI,
        EGO_ENVELOPING,
        EGO_VULNERABILITY,
        EGO_IRRITATION,
	/* xxx */

	/* Gloves */
        EGO_FREE_ACTION = 48,
        EGO_SLAYING,
        EGO_AGILITY,
        EGO_POWER,
        EGO_GLOVES_THIEVERY,
        EGO_GAUNTLETS_COMBAT,
        EGO_WEAKNESS,
        EGO_CLUMSINESS,

	/* Boots */
        EGO_SLOW_DESCENT,
        EGO_QUIET,
        EGO_MOTION,
        EGO_SPEED,
        EGO_STABILITY,
        EGO_NOISE,
        EGO_SLOWNESS,
        EGO_ANNOYANCE,

	/* Weapons */
        EGO_HA,
        EGO_DF,
        EGO_BLESS_BLADE,
        EGO_GONDOLIN,
        EGO_WEST,
        EGO_ATTACKS,
        EGO_FURY,
	/* xxx */
        EGO_BRAND_ACID = 72,
        EGO_BRAND_ELEC,
        EGO_BRAND_FIRE,
        EGO_BRAND_COLD,
        EGO_BRAND_POIS,
	/* xxx */
        EGO_SLAY_ANIMAL = 80,
        EGO_SLAY_EVIL,
        EGO_SLAY_UNDEAD,
        EGO_SLAY_DEMON,
        EGO_SLAY_ORC,
        EGO_SLAY_TROLL,
        EGO_SLAY_GIANT,
        EGO_SLAY_DRAGON,
        EGO_KILL_ANIMAL,
        EGO_KILL_EVIL,
        EGO_KILL_UNDEAD,
        EGO_KILL_DEMON,
        EGO_KILL_ORC,
        EGO_KILL_TROLL,
        EGO_KILL_GIANT,
        EGO_KILL_DRAGON,
	/* xxx */
        EGO_DIGGING = 100,
        EGO_DIGGER_EARTHQUAKE,
        EGO_MORGUL,
	/* xxx */

	/* Bows */
        EGO_ACCURACY = 104,
        EGO_VELOCITY,
        EGO_BOW_LORIEN,
        EGO_CROSSBOW_HARAD,
        EGO_EXTRA_MIGHT,
        EGO_EXTRA_SHOTS,
        EGO_SLING_BUCKLAND,
        EGO_NAZGUL,

	/* Ammo */
        EGO_HURT_ANIMAL,
        EGO_HURT_EVIL,
        EGO_HURT_UNDEAD,
        EGO_HURT_DEMON,
        EGO_HURT_ORC,
        EGO_HURT_TROLL,
        EGO_HURT_GIANT,
        EGO_HURT_DRAGON,
        EGO_AMMO_HOLY,
        EGO_AMMO_VENOM,
        EGO_FLAME,
        EGO_FROST,
        EGO_WOUNDING,
        EGO_BACKBITING,

	/* Broken items */
        EGO_SHATTERED,
        EGO_BLASTED
};

/* Themed drops */
enum
{
        DROP_TYPE_UNTHEMED,
        DROP_TYPE_GOLD,
        DROP_TYPE_POTION,
        DROP_TYPE_ROD_WAND_STAFF,
        DROP_TYPE_SCROLL,
        DROP_TYPE_SHIELD,
        DROP_TYPE_WEAPON,
        DROP_TYPE_ARMOR,
        DROP_TYPE_BOOTS,
        DROP_TYPE_BOW,
        DROP_TYPE_CLOAK,
        DROP_TYPE_GLOVES,
        DROP_TYPE_HAFTED,
        DROP_TYPE_HEADGEAR,
        DROP_TYPE_JEWELRY,
        DROP_TYPE_DRAGON_ARMOR,
        DROP_TYPE_CHEST,
        DROP_TYPE_DUNGEON_MAGIC_BOOK,
        DROP_TYPE_DUNGEON_PRAYER_BOOK
};

/**** Object "tval" and "sval" codes ****/

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
#define TV_BOTTLE	 2	/* Empty bottles ('!') */
#define TV_JUNK          3	/* Sticks, Pottery, etc ('~') */
#define TV_SPIKE         5	/* Spikes ('~') */
#define TV_CHEST         7	/* Chests ('~') */
#define TV_SHOT		16	/* Ammo for slings */
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
#define TV_LITE         39	/* Lites (including Specials) */
#define TV_AMULET       40	/* Amulets (including Specials) */
#define TV_RING         45	/* Rings (including Specials) */
#define TV_STAFF        55
#define TV_WAND         65
#define TV_ROD          66
#define TV_SCROLL       70
#define TV_POTION       75
#define TV_FLASK        77
#define TV_FOOD         80
#define TV_CORPSE	85	/* Corpse for possessor */
#define TV_STATUE	86	/* Statue */
#define TV_MAGIC_BOOK   90
#define TV_PRAYER_BOOK  91
#define TV_MONSTER_BOOK	99	/* Placeholder */
#define TV_GOLD         100	/* Gold can only be picked up by players */

/* The (important) "sval" codes for TV_JUNK */
#define SV_BOULDER		10

/* The "sval" codes for TV_SHOT/TV_ARROW/TV_BOLT */
#define SV_AMMO_LIGHT		0	/* pebbles */
#define SV_AMMO_NORMAL		1	/* shots, arrows, bolts */
#define SV_AMMO_HEAVY		2	/* seeker arrows and bolts */
#define SV_AMMO_SILVER		3	/* silver arrows and bolts */

/* The "sval" codes for TV_BOW (note information in "sval") */
#define SV_SLING		2	/* (x2) */
#define SV_SHORT_BOW		12	/* (x2) */
#define SV_LONG_BOW		13	/* (x3) */
#define SV_LIGHT_XBOW		23	/* (x3) */
#define SV_HEAVY_XBOW		24	/* (x4) */

/* The "sval" codes for TV_DIGGING */
#define SV_SHOVEL		1
#define SV_GNOMISH_SHOVEL	2
#define SV_DWARVEN_SHOVEL	3
#define SV_PICK			4
#define SV_ORCISH_PICK		5
#define SV_DWARVEN_PICK		6
#define SV_MATTOCK		7

/* The "sval" values for TV_HAFTED */
#define SV_WHIP			2	/* 1d6 */
#define SV_QUARTERSTAFF		3	/* 1d9 */
#define SV_MACE			5	/* 2d4 */
#define SV_BALL_AND_CHAIN	6	/* 2d4 */
#define SV_WAR_HAMMER		8	/* 3d3 */
#define SV_LUCERN_HAMMER	10	/* 2d5 */
#define SV_THROWING_HAMMER	11	/* 2d5 */
#define SV_MORNING_STAR		12	/* 2d6 */
#define SV_FLAIL		13	/* 2d6 */
#define SV_LEAD_FILLED_MACE	15	/* 3d4 */
#define SV_TWO_HANDED_FLAIL	18	/* 3d6 */
#define SV_MACE_OF_DISRUPTION	20	/* 5d8 */
#define SV_GROND		50	/* 3d4 */

/* The "sval" values for TV_POLEARM */
#define SV_POISONED_DART	1	/* 4d3 */
#define SV_SPEAR		2	/* 1d6 */
#define SV_AWL_PIKE		4	/* 1d8 */
#define SV_TRIDENT		5	/* 1d9 */
#define SV_PIKE			8	/* 2d5 */
#define SV_BEAKED_AXE		10	/* 2d6 */
#define SV_BROAD_AXE		11	/* 2d6 */
#define SV_THROWING_AXE		12	/* 2d4 */
#define SV_GLAIVE		13	/* 2d6 */
#define SV_HALBERD		15	/* 3d4 */
#define SV_SCYTHE		17	/* 5d3 */
#define SV_LANCE		20	/* 2d8 */
#define SV_BATTLE_AXE		22	/* 2d8 */
#define SV_GREAT_AXE		25	/* 4d4 */
#define SV_LOCHABER_AXE		28	/* 3d8 */
#define SV_SCYTHE_OF_SLICING	30	/* 8d4 */

/* The "sval" codes for TV_SWORD */
#define SV_BROKEN_DAGGER	1	/* 1d1 */
#define SV_BROKEN_SWORD		2	/* 1d2 */
#define SV_DAGGER		4	/* 1d4 */
#define SV_MAIN_GAUCHE		5	/* 1d5 */
#define SV_RAPIER		7	/* 1d6 */
#define SV_SMALL_SWORD		8	/* 1d6 */
#define SV_SHORT_SWORD		10	/* 1d7 */
#define SV_SABRE		11	/* 1d7 */
#define SV_CUTLASS		12	/* 1d7 */
#define SV_TULWAR		15	/* 2d4 */
#define SV_BROAD_SWORD		16	/* 2d5 */
#define SV_LONG_SWORD		17	/* 2d5 */
#define SV_SCIMITAR		18	/* 2d5 */
#define SV_KATANA		20	/* 3d4 */
#define SV_BASTARD_SWORD	21	/* 3d4 */
#define SV_TWO_HANDED_SWORD	25	/* 3d6 */
#define SV_EXECUTIONERS_SWORD	28	/* 4d5 */
#define SV_BLADE_OF_CHAOS	30	/* 6d5 */
#define SV_SWORD_OF_BALROG	31	/* 7d5 */

/* The "sval" codes for TV_SHIELD */
#define SV_SMALL_LEATHER_SHIELD	2
#define SV_SMALL_METAL_SHIELD	3
#define SV_LARGE_LEATHER_SHIELD	4
#define SV_LARGE_METAL_SHIELD	5
#define SV_SHIELD_OF_DEFLECTION	10

/* The "sval" codes for TV_HELM */
#define SV_HARD_LEATHER_CAP	2
#define SV_METAL_CAP		3
#define SV_IRON_HELM		5
#define SV_STEEL_HELM		6
#define SV_IRON_CROWN		10
#define SV_GOLDEN_CROWN		11
#define SV_JEWELED_CROWN	12
#define SV_MORGOTH		50

/* The "sval" codes for TV_BOOTS */
#define SV_PAIR_OF_SOFT_LEATHER_BOOTS	2
#define SV_PAIR_OF_HARD_LEATHER_BOOTS	3
#define SV_PAIR_OF_METAL_SHOD_BOOTS	6

/* The "sval" codes for TV_CLOAK */
#define SV_CLOAK		1
#define SV_SHADOW_CLOAK		6

/* The "sval" codes for TV_GLOVES */
#define SV_SET_OF_LEATHER_GLOVES	1
#define SV_SET_OF_GAUNTLETS		2
#define SV_SET_OF_CESTI			5

/* The "sval" codes for TV_SOFT_ARMOR */
#define SV_FILTHY_RAG			1
#define SV_ROBE				2
#define SV_SOFT_LEATHER_ARMOR		4
#define SV_SOFT_STUDDED_LEATHER		5
#define SV_HARD_LEATHER_ARMOR		6
#define SV_HARD_STUDDED_LEATHER		7
#define SV_LEATHER_SCALE_MAIL		11

/* The "sval" codes for TV_HARD_ARMOR */
#define SV_RUSTY_CHAIN_MAIL		1	/* 14- */
#define SV_METAL_SCALE_MAIL		3	/* 13 */
#define SV_CHAIN_MAIL			4	/* 14 */
#define SV_AUGMENTED_CHAIN_MAIL		6	/* 16 */
#define SV_DOUBLE_CHAIN_MAIL		7	/* 16 */
#define SV_BAR_CHAIN_MAIL		8	/* 18 */
#define SV_METAL_BRIGANDINE_ARMOUR	9	/* 19 */
#define SV_PARTIAL_PLATE_ARMOUR		12	/* 22 */
#define SV_METAL_LAMELLAR_ARMOUR	13	/* 23 */
#define SV_FULL_PLATE_ARMOUR		15	/* 25 */
#define SV_RIBBED_PLATE_ARMOUR		18	/* 28 */
#define SV_MITHRIL_CHAIN_MAIL		20	/* 28+ */
#define SV_MITHRIL_PLATE_MAIL		25	/* 35+ */
#define SV_ADAMANTITE_PLATE_MAIL	30	/* 40+ */

/* The "sval" codes for TV_DRAG_ARMOR */
#define SV_DRAGON_BLACK		1
#define SV_DRAGON_BLUE		2
#define SV_DRAGON_WHITE		3
#define SV_DRAGON_RED		4
#define SV_DRAGON_GREEN		5
#define SV_DRAGON_MULTIHUED	6
#define SV_DRAGON_SHINING	10
#define SV_DRAGON_LAW		12
#define SV_DRAGON_BRONZE	14
#define SV_DRAGON_GOLD		16
#define SV_DRAGON_CHAOS		18
#define SV_DRAGON_SHADOW	19
#define SV_DRAGON_BALANCE	20
#define SV_DRAGON_POWER		30

/* The sval codes for TV_LITE */
#define SV_LITE_TORCH		0
#define SV_LITE_LANTERN		1
#define SV_LITE_GALADRIEL	4  /* Art.: Phial of Galadriel */
#define SV_LITE_ELENDIL		5  /* Art.: Star of Elendil */
#define SV_LITE_THRAIN		6  /* Art.: Arkenstone of Thrain */ 
#define SV_LITE_PALANTIR	7  /* Art.: Palantir of Westernesse */

/* The "sval" codes for TV_AMULET */
#define SV_AMULET_DOOM		0
#define SV_AMULET_TELEPORT	1
#define SV_AMULET_ADORNMENT	2
#define SV_AMULET_SLOW_DIGEST	3
#define SV_AMULET_RESIST_ACID	4
#define SV_AMULET_SEARCHING	5
#define SV_AMULET_WISDOM	6
#define SV_AMULET_CHARISMA	7
#define SV_AMULET_THE_MAGI	8
#define SV_AMULET_SUSTENANCE	9
#define SV_AMULET_CARLAMMAS	10 /* Art.: Amulet of Carlammas */
#define SV_AMULET_INGWE		11 /* Art.: Amulet of Ingwe */
#define SV_AMULET_DWARVES	12 /* Art.: Necklace of the Dwarves */
#define SV_AMULET_ESP		13
#define SV_AMULET_RESIST	14
#define SV_AMULET_REGEN		15
#define SV_AMULET_ELESSAR	16 /* Art.: Elfstone of Elessar */
#define SV_AMULET_EVENSTAR	17 /* Art.: Jewel 'Evenstar' */
#define SV_AMULET_DEVOTION	18
#define SV_AMULET_WEAPONMASTERY	19
#define SV_AMULET_TRICKERY	20
#define SV_AMULET_INFRAVISION	21
#define SV_AMULET_RESIST_LIGHTNING  22


/* The sval codes for TV_RING */
#define SV_RING_WOE		0
#define SV_RING_AGGRAVATION	1
#define SV_RING_WEAKNESS	2
#define SV_RING_STUPIDITY	3
#define SV_RING_TELEPORTATION	4
#define SV_RING_INVISIBILITY	5
#define SV_RING_SLOW_DIGESTION	6
#define SV_RING_FEATHER_FALL	7
#define SV_RING_RESIST_FIRE	8
#define SV_RING_RESIST_COLD	9
#define SV_RING_SUSTAIN_STR	10
#define SV_RING_SUSTAIN_INT	11
#define SV_RING_SUSTAIN_WIS	12
#define SV_RING_SUSTAIN_DEX	13
#define SV_RING_SUSTAIN_CON	14
#define SV_RING_SUSTAIN_CHR	15
#define SV_RING_PROTECTION	16
#define SV_RING_ACID		17
#define SV_RING_FLAMES		18
#define SV_RING_ICE		19
#define SV_RING_RESIST_POIS	20
#define SV_RING_FREE_ACTION	21
#define SV_RING_SEE_INVIS	22
#define SV_RING_SEARCHING	23
#define SV_RING_STR		24
#define SV_RING_INT		25
#define SV_RING_DEX		26
#define SV_RING_CON		27
#define SV_RING_ACCURACY	28
#define SV_RING_DAMAGE		29
#define SV_RING_SLAYING		30
#define SV_RING_SPEED		31
#define SV_RING_BARAHIR		32 /* Art.: Ring of Barahir */
#define SV_RING_TULKAS		33 /* Art.: Ring of Tulkas */
#define SV_RING_NARYA		34 /* Art.: Ring of Power 'Narya' */
#define SV_RING_NENYA		35 /* Art.: Ring of Power 'Nenya' */
#define SV_RING_VILYA		36 /* Art.: Ring of Power 'Vilya' */
#define SV_RING_POWER		37 /* Art.: RoP 'The One Ring' */
#define SV_RING_LIGHTNING	38


/* The "sval" codes for TV_STAFF */
#define SV_STAFF_DARKNESS	0
#define SV_STAFF_SLOWNESS	1
#define SV_STAFF_HASTE_MONSTERS	2
#define SV_STAFF_SUMMONING	3
#define SV_STAFF_TELEPORTATION	4
#define SV_STAFF_IDENTIFY	5
#define SV_STAFF_REMOVE_CURSE	6
#define SV_STAFF_STARLITE	7
#define SV_STAFF_LITE		8
#define SV_STAFF_MAPPING	9
#define SV_STAFF_DETECT_GOLD	10
#define SV_STAFF_DETECT_ITEM	11
#define SV_STAFF_DETECT_TRAP	12
#define SV_STAFF_DETECT_DOOR	13
#define SV_STAFF_DETECT_INVIS	14
#define SV_STAFF_DETECT_EVIL	15
#define SV_STAFF_CURE_LIGHT	16
#define SV_STAFF_CURING		17
#define SV_STAFF_HEALING	18
#define SV_STAFF_THE_MAGI	19
#define SV_STAFF_SLEEP_MONSTERS	20
#define SV_STAFF_SLOW_MONSTERS	21
#define SV_STAFF_SPEED		22
#define SV_STAFF_PROBING	23
#define SV_STAFF_DISPEL_EVIL	24
#define SV_STAFF_POWER		25
#define SV_STAFF_HOLINESS	26
#define SV_STAFF_BANISHMENT	27
#define SV_STAFF_EARTHQUAKES	28
#define SV_STAFF_DESTRUCTION	29
#define SV_STAFF_SUMMON_PET	30


/* The "sval" codes for TV_WAND */
#define SV_WAND_HEAL_MONSTER	0
#define SV_WAND_HASTE_MONSTER	1
#define SV_WAND_CLONE_MONSTER	2
#define SV_WAND_TELEPORT_AWAY	3
#define SV_WAND_DISARMING	4		
#define SV_WAND_TRAP_DOOR_DEST	5
#define SV_WAND_STONE_TO_MUD	6
#define SV_WAND_LITE		7
#define SV_WAND_SLEEP_MONSTER	8
#define SV_WAND_SLOW_MONSTER	9
#define SV_WAND_CONFUSE_MONSTER	10
#define SV_WAND_FEAR_MONSTER	11
#define SV_WAND_DRAIN_LIFE	12
#define SV_WAND_POLYMORPH	13
#define SV_WAND_STINKING_CLOUD	14
#define SV_WAND_MAGIC_MISSILE	15
#define SV_WAND_ACID_BOLT	16
#define SV_WAND_ELEC_BOLT	17
#define SV_WAND_FIRE_BOLT	18
#define SV_WAND_COLD_BOLT	19
#define SV_WAND_ACID_BALL	20
#define SV_WAND_ELEC_BALL	21
#define SV_WAND_FIRE_BALL	22
#define SV_WAND_COLD_BALL	23
#define SV_WAND_CHARM		24
#define SV_WAND_WONDER		25
#define SV_WAND_ANNIHILATION	26
#define SV_WAND_DRAGON_FIRE	27
#define SV_WAND_DRAGON_COLD	28
#define SV_WAND_DRAGON_BREATH	29
#define SV_WANT_BLIND_MONSTER	30

/* The "sval" codes for TV_ROD */
#define SV_ROD_DETECT_TRAP	0
#define SV_ROD_DETECT_DOOR	1
#define SV_ROD_IDENTIFY		2
#define SV_ROD_RECALL		3
#define SV_ROD_ILLUMINATION	4
#define SV_ROD_MAPPING		5
#define SV_ROD_DETECTION	6
#define SV_ROD_PROBING		7
#define SV_ROD_CURING		8
#define SV_ROD_HEALING		9
#define SV_ROD_RESTORATION	10
#define SV_ROD_SPEED		11
/* xxx (aimed) */
#define SV_ROD_TELEPORT_AWAY	13
#define SV_ROD_DISARMING	14
#define SV_ROD_LITE		15
#define SV_ROD_SLEEP_MONSTER	16
#define SV_ROD_SLOW_MONSTER	17
#define SV_ROD_DRAIN_LIFE	18
#define SV_ROD_POLYMORPH	19
#define SV_ROD_ACID_BOLT	20
#define SV_ROD_ELEC_BOLT	21
#define SV_ROD_FIRE_BOLT	22
#define SV_ROD_COLD_BOLT	23
#define SV_ROD_ACID_BALL	24
#define SV_ROD_ELEC_BALL	25
#define SV_ROD_FIRE_BALL	26
#define SV_ROD_COLD_BALL	27


/* The "sval" codes for TV_SCROLL */

#define SV_SCROLL_DARKNESS		0
#define SV_SCROLL_AGGRAVATE_MONSTER	1
#define SV_SCROLL_CURSE_ARMOR		2
#define SV_SCROLL_CURSE_WEAPON		3
#define SV_SCROLL_SUMMON_MONSTER	4
#define SV_SCROLL_SUMMON_UNDEAD		5
#define SV_SCROLL_SUMMON_PET		6
#define SV_SCROLL_TRAP_CREATION		7
#define SV_SCROLL_PHASE_DOOR		8
#define SV_SCROLL_TELEPORT		9
#define SV_SCROLL_TELEPORT_LEVEL	10
#define SV_SCROLL_WORD_OF_RECALL	11
#define SV_SCROLL_IDENTIFY		12
#define SV_SCROLL_STAR_IDENTIFY		13
#define SV_SCROLL_REMOVE_CURSE		14
#define SV_SCROLL_STAR_REMOVE_CURSE	15
#define SV_SCROLL_ENCHANT_ARMOR		16
#define SV_SCROLL_ENCHANT_WEAPON_TO_HIT	17
#define SV_SCROLL_ENCHANT_WEAPON_TO_DAM	18
/* xxx enchant missile? */
#define SV_SCROLL_STAR_ENCHANT_ARMOR	20
#define SV_SCROLL_STAR_ENCHANT_WEAPON	21
#define SV_SCROLL_RECHARGING		22
/* xxx */
#define SV_SCROLL_LIGHT			24
#define SV_SCROLL_MAPPING		25
#define SV_SCROLL_DETECT_GOLD		26
#define SV_SCROLL_DETECT_ITEM		27
#define SV_SCROLL_DETECT_TRAP		28
#define SV_SCROLL_DETECT_DOOR		29
#define SV_SCROLL_DETECT_INVIS		30
/* xxx (detect evil?) */
#define SV_SCROLL_SATISFY_HUNGER	32
#define SV_SCROLL_BLESSING		33
#define SV_SCROLL_HOLY_CHANT		34
#define SV_SCROLL_HOLY_PRAYER		35
#define SV_SCROLL_MONSTER_CONFUSION	36
#define SV_SCROLL_PROTECTION_FROM_EVIL	37
#define SV_SCROLL_RUNE_OF_PROTECTION	38
#define SV_SCROLL_TRAP_DOOR_DESTRUCTION	39
#define SV_SCROLL_CREATE_MONSTER_TRAP	40
#define SV_SCROLL_STAR_DESTRUCTION	41
#define SV_SCROLL_DISPEL_UNDEAD		42
/* xxx */
#define SV_SCROLL_BANISHMENT		44
#define SV_SCROLL_MASS_BANISHMENT	45
#define SV_SCROLL_ACQUIREMENT		46
#define SV_SCROLL_STAR_ACQUIREMENT	47

/* The "sval" codes for TV_POTION */
#define SV_POTION_WATER			0
#define SV_POTION_APPLE_JUICE		1
#define SV_POTION_SLIME_MOLD		2
/* xxx (fixed color) */
#define SV_POTION_SLOWNESS		4
#define SV_POTION_SALT_WATER		5
#define SV_POTION_POISON		6
#define SV_POTION_BLINDNESS		7
/* xxx */
#define SV_POTION_CONFUSION		9
/* xxx */
#define SV_POTION_SLEEP			11
/* xxx */
#define SV_POTION_LOSE_MEMORIES		13
#define SV_POTION_DRAIN_MANA		14
#define SV_POTION_RUINATION		15
#define SV_POTION_DEC_STR		16
#define SV_POTION_DEC_INT		17
#define SV_POTION_DEC_WIS		18
#define SV_POTION_DEC_DEX		19
#define SV_POTION_DEC_CON		20
#define SV_POTION_DEC_CHR		21
#define SV_POTION_DETONATIONS		22
#define SV_POTION_DEATH			23
#define SV_POTION_INFRAVISION		24
#define SV_POTION_DETECT_INVIS		25
#define SV_POTION_SLOW_POISON		26
#define SV_POTION_CURE_POISON		27
#define SV_POTION_BOLDNESS		28
#define SV_POTION_SPEED			29
#define SV_POTION_RESIST_HEAT		30
#define SV_POTION_RESIST_COLD		31
#define SV_POTION_HEROISM		32
#define SV_POTION_BERSERK_STRENGTH	33
#define SV_POTION_CURE_LIGHT		34
#define SV_POTION_CURE_SERIOUS		35
#define SV_POTION_CURE_CRITICAL		36
#define SV_POTION_HEALING		37
#define SV_POTION_STAR_HEALING		38
#define SV_POTION_LIFE			39
#define SV_POTION_RESTORE_MANA		40
#define SV_POTION_RESTORE_EXP		41
#define SV_POTION_RES_STR		42
#define SV_POTION_RES_INT		43
#define SV_POTION_RES_WIS		44
#define SV_POTION_RES_DEX		45
#define SV_POTION_RES_CON		46
#define SV_POTION_RES_CHR		47
#define SV_POTION_INC_STR		48
#define SV_POTION_INC_INT		49
#define SV_POTION_INC_WIS		50
#define SV_POTION_INC_DEX		51
#define SV_POTION_INC_CON		52
#define SV_POTION_INC_CHR		53
/* xxx */
#define SV_POTION_AUGMENTATION		55
#define SV_POTION_ENLIGHTENMENT		56
#define SV_POTION_STAR_ENLIGHTENMENT	57
#define SV_POTION_SELF_KNOWLEDGE	58
#define SV_POTION_EXPERIENCE		59
#define SV_POTION_RESIST_ACID		60
#define SV_POTION_RESIST_ELECTRICITY	61
#define SV_POTION_RESIST_POISON		62
#define SV_POTION_RESISTANCE		63
#define SV_POTION_INVISIBILITY		64

/* The "sval" codes for TV_FOOD */
#define SV_FOOD_POISON		0
#define SV_FOOD_BLINDNESS	1
#define SV_FOOD_PARANOIA	2
#define SV_FOOD_CONFUSION	3
#define SV_FOOD_HALLUCINATION	4
#define SV_FOOD_PARALYSIS	5
#define SV_FOOD_WEAKNESS	6
#define SV_FOOD_SICKNESS	7
#define SV_FOOD_STUPIDITY	8
#define SV_FOOD_NAIVETY		9
#define SV_FOOD_UNHEALTH	10
#define SV_FOOD_DISEASE		11
#define SV_FOOD_CURE_POISON	12
#define SV_FOOD_CURE_BLINDNESS	13
#define SV_FOOD_CURE_PARANOIA	14
#define SV_FOOD_CURE_CONFUSION	15
#define SV_FOOD_CURE_SERIOUS	16
#define SV_FOOD_RESTORE_STR	17
#define SV_FOOD_RESTORE_CON	18
#define SV_FOOD_RESTORING	19
/* many missing mushrooms */
#define SV_FOOD_BISCUIT		32
#define SV_FOOD_JERKY		33
#define SV_FOOD_RATION		35
#define SV_FOOD_SLIME_MOLD	36
#define SV_FOOD_WAYBREAD	37
#define SV_FOOD_PINT_OF_ALE	38
#define SV_FOOD_PINT_OF_WINE	39


/*gold, incomplete list defined primarily for the mimics*/

#define SV_GOLD_COPPER		1
#define SV_GOLD_SILVER		4
#define SV_GOLD_GOLD		9
#define SV_GOLD_MITHRIL		17
#define SV_GOLD_ADAMANTITE	18


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
#define MAX_GOLD		18	/* Number of "gold" entries */


/*
 * These are the various levels of squelching supported by the game.
 */
enum
{
        SQUELCH_NONE,		/* Nothing */
        SQUELCH_CURSED,		/* Cursed items */
        SQUELCH_AVERAGE,	/* Average and worse */
        SQUELCH_GOOD,		/* Good and worse */
        SQUELCH_ALL,		/* All but artifacts */
        SQUELCH_OPENED_CHESTS	/* Open chests */
};

/*others are defines in squelch.c, static int do_qual_squelch,
 *but this one is used in chest opening. JG*/
#define CHEST_INDEX 	19

/*number of bytes used in squelch sub-quality array*/
#define SQUELCH_BYTES    24


/*
 * Special "sval" value -- unknown "sval"
 */
#define SV_UNKNOWN			255

/*
 * Special object flags
 */
#define IDENT_SENSE     	0x00000001	/* Item has been "sensed" */
#define IDENT_FIXED     	0x00000002	/* Item has been "haggled" */
#define IDENT_EMPTY     	0x00000004	/* Item charges are known */
#define IDENT_KNOWN     	0x00000008	/* Item abilities are known */
#define IDENT_STORE     	0x00000010	/* Item is in the inventory of a store */
#define IDENT_MENTAL    	0x00000020	/* Item information is known */
#define IDENT_CURSED    	0x00000040	/* Item is temporarily cursed */
#define IDENT_BROKEN    	0x00000080	/* Item is permanently worthless */
#define IDENT_QUEST     	0x00000100	/* Item is a quest item */
#define IDENT_PERFECT_BALANCE   0x00000200	/* Item has perfect balance */

/*
 * The special inscriptions.
 */
enum
{
        INSCRIP_NULL = 100,
        INSCRIP_TERRIBLE,
        INSCRIP_WORTHLESS,
        INSCRIP_CURSED,
        INSCRIP_BROKEN,		/* Unused? */
        INSCRIP_AVERAGE,
        INSCRIP_GOOD,
        INSCRIP_EXCELLENT,
        INSCRIP_SPECIAL,
        INSCRIP_UNCURSED,	/* Unused? */
        INSCRIP_INDESTRUCTIBLE
};

/*
 * Number of special inscriptions, plus one.
 */
#define MAX_INSCRIP			11


/**** Object flags ****/

/*
 * As of Vanilla 2.7.8, the "object flags" are valid for all objects, and as
 * of 2.7.9, these flags are not actually stored with the object, but
 * rather in the object_kind, ego_item, and artifact structures.
 * And as of PosBand 0.2.0 (or earlier?) they're back in object_type
 * again, to allow randarts and other things like new DSMs.
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
 *
 * And note that "flags4" contains some flags which are supposed to be
 * purely racial. This field is introduced in PosBand.
 */

#define TR1_STR             0x00000001L /* STR += "pval" */
#define TR1_INT             0x00000002L /* INT += "pval" */
#define TR1_WIS             0x00000004L /* WIS += "pval" */
#define TR1_DEX             0x00000008L /* DEX += "pval" */
#define TR1_CON             0x00000010L /* CON += "pval" */
#define TR1_CHR             0x00000020L /* CHR += "pval" */
#define TR1_TR1XXX1	    0x00000040L /* (reserved) */
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
#define TR1_BRAND_POIS      0x08000000L /* Weapon has poison brand */
#define TR1_BRAND_ACID      0x10000000L /* Weapon has acid brand */
#define TR1_BRAND_ELEC      0x20000000L /* Weapon has elec brand */
#define TR1_BRAND_FIRE      0x40000000L /* Weapon has fire brand */
#define TR1_BRAND_COLD      0x80000000L /* Weapon has cold brand */

/* TR1 Uber-Flags */
#define TR1_ALL_STATS	(TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR)

/*
 * Hack -- flag set 1 -- mask for "pval-dependant" flags.
 * Note that all "pval" dependant flags must be in "flags1".
 */
#define TR1_PVAL_MASK \
	(TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | \
	 TR1_CON | TR1_CHR | TR1_TR1XXX1 | TR1_TR1XXX2 | \
	 TR1_STEALTH | TR1_SEARCH | TR1_INFRA | TR1_TUNNEL | \
	 TR1_SPEED | TR1_BLOWS | TR1_SHOTS | TR1_MIGHT)



#define TR2_SUST_STR        0x00000001L /* Sustain STR */
#define TR2_SUST_INT        0x00000002L /* Sustain INT */
#define TR2_SUST_WIS        0x00000004L /* Sustain WIS */
#define TR2_SUST_DEX        0x00000008L /* Sustain DEX */
#define TR2_SUST_CON        0x00000010L /* Sustain CON */
#define TR2_SUST_CHR        0x00000020L /* Sustain CHR */
#define TR2_TR2XXX1         0x00000040L /* (reserved, formerly TR2_COLD_TOUCH -> TR4) */
#define TR2_TR2XXX2         0x00000080L /* (reserved) */
#define TR2_TR2XXX3         0x00000100L /* (reserved) */
#define TR2_TR2XXX4         0x00000200L /* (reserved, formerly TR2_PASS_WALL -> TR4) */
#define TR2_TR2XXX5         0x00000400L /* (reserved, formerly TR2_UNDEAD_RACE -> TR4) */
#define TR2_IM_POIS	    0x00000800L /* Immunity to pois */
#define TR2_IM_ACID         0x00001000L /* Immunity to acid */
#define TR2_IM_ELEC         0x00002000L /* Immunity to elec */
#define TR2_IM_FIRE         0x00004000L /* Immunity to fire */
#define TR2_IM_COLD         0x00008000L /* Immunity to cold */
#define TR2_RES_ACID        0x00010000L /* Resist acid */
#define TR2_RES_ELEC        0x00020000L /* Resist elec */
#define TR2_RES_FIRE        0x00040000L /* Resist fire */
#define TR2_RES_COLD        0x00080000L /* Resist cold */
#define TR2_RES_POIS        0x00100000L /* Resist poison */
#define TR2_RES_FEAR        0x00200000L /* Resist fear */
#define TR2_RES_LITE        0x00400000L /* Resist lite */
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

#define TR3_SLOW_DIGEST     0x00000001L /* Slow digest */
#define TR3_FEATHER         0x00000002L /* Feather Falling */
#define TR3_LITE            0x00000004L /* Perma-Lite */
#define TR3_REGEN           0x00000008L /* Regeneration */
#define TR3_TELEPATHY       0x00000010L /* Telepathy */
#define TR3_SEE_INVIS       0x00000020L /* See Invis */
#define TR3_FREE_ACT        0x00000040L /* Free action */
#define TR3_HOLD_LIFE       0x00000080L /* Hold life */
#define TR3_NEVER_PICKUP    0x00000100L /* Monsters can't pickup*/
#define TR3_INVISIBILITY    0x00000200L /* Grants invisibility */
#define TR3_TR3XXX3         0x00000400L
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
#define TR3_PERFECT_BALANCE 0x00200000L /* item with perfect balance  */
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

/* TR3 Uber-Flags */
#define TR3_IGNORE_ALL (TR3_IGNORE_ACID | TR3_IGNORE_ELEC | TR3_IGNORE_FIRE | TR3_IGNORE_COLD)

/*
 * Flag set 3 -- mask for "ignore element" flags.
 */
#define TR3_IGNORE_MASK TR3_IGNORE_ALL

/* TR4 - special flags, added for PosBand */
/* Note! Most of them are designed for races. ALIGN_XXX *must not*
 * be set for any objects (or some weird things could happen).
 * Others are possible, but can greatly disrupt balance. Okay, okay,
 * it is already disrupted. ;-)
 */

#define TR4_ACID_TOUCH		0x00000001L /* Acidic Touch */
#define TR4_ELEC_TOUCH		0x00000002L /* Shocking Touch */
#define TR4_FIRE_TOUCH		0x00000004L /* Burning Touch */
#define TR4_COLD_TOUCH		0x00000008L /* Freezing Touch */
#define TR4_PASS_WALL		0x00000010L /* Immaterial */
#define TR4_UNDEAD_RACE		0x00000020L /* Undead race */
#define TR4_TR4XXX1		0x00000040L /* xxx demonic race? */
#define TR4_TR4XXX2		0x00000080L /* xxx draconic race? */
#define TR4_ALIGN_CHAOS		0x00000100L /* Chaotic powers */
#define TR4_ALIGN_LAW		0x00000200L /* Lawful powers */
#define TR4_TR4XXX3		0x00000400L /* xxx more alignments possible */
#define TR4_TR4XXX4		0x00000800L
#define TR4_TR4XXX5		0x00001000L
#define TR4_TR4XXX6		0x00002000L
#define TR4_TR4XXX7		0x00004000L
#define TR4_TR4XXX8		0x00008000L
#define TR4_GIANT_WIELD		0x00010000L /* Needs huge weapon (>=18lbs) */
#define TR4_GIANT_WEAR		0x00020000L /* Needs huge armor (at least plate mail) */
#define TR4_THROW_BOULDER	0x00040000L /* Affinity with boulders */
#define TR4_POIS_BLOOD		0x00080000L /* Poisonous blood (?) */
#define TR4_EAT_WALL		0x00100000L /* Eats walls */
#define TR4_SUPER_REGEN		0x00200000L /* Super-regeneration */
#define TR4_EAT_STATUE		0x00400000L /* Eats statues (like basilisks) */
#define TR4_AURA_FEAR		0x00800000L /* Aure of fear */
#define TR4_TR4XXX17		0x01000000L
#define TR4_TR4XXX18		0x02000000L
#define TR4_TR4XXX19		0x04000000L
#define TR4_TR4XXX20		0x08000000L
#define TR4_TR4XXX21		0x10000000L
#define TR4_TR4XXX22		0x20000000L
#define TR4_TR4XXX23		0x40000000L
#define TR4_NO_INNATE		0x80000000L /* No innate attacks */


/*
 * Hack -- special "xtra" object flag info (type)
 */
enum
{
        OBJECT_XTRA_TYPE_SUSTAIN = 1,
        OBJECT_XTRA_TYPE_RESIST,
        OBJECT_XTRA_TYPE_POWER
};

/*
 * Hack -- special "xtra" object flag info (what flag set)
 */
#define OBJECT_XTRA_WHAT_SUSTAIN	2
#define OBJECT_XTRA_WHAT_RESIST		2
#define OBJECT_XTRA_WHAT_POWER		3

/*
 * Hack -- special "xtra" object flag info (base flag value)
 */
#define OBJECT_XTRA_BASE_SUSTAIN	TR2_SUST_STR
#define OBJECT_XTRA_BASE_RESIST		TR2_RES_POIS
#define OBJECT_XTRA_BASE_POWER		TR3_SLOW_DIGEST

/*
 * Hack -- special "xtra" object flag info (number of flags)
 */
#define OBJECT_XTRA_SIZE_SUSTAIN	6
#define OBJECT_XTRA_SIZE_RESIST		12
#define OBJECT_XTRA_SIZE_POWER		8

/*
 * Chest trap flags (see "tables.c")
 */
#define CHEST_LOSE_STR	0x01
#define CHEST_LOSE_CON	0x02
#define CHEST_POISON	0x04
#define CHEST_PARALYZE	0x08
#define CHEST_EXPLODE	0x10
#define CHEST_SUMMON	0x20


/**** Useful macros ****/

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
	 ((k_info[(T)->k_idx].flags3 & (TR3_EASY_KNOW)) && \
	  k_info[(T)->k_idx].aware))

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_attr(T) \
	((k_info[(T)->k_idx].flavor) ? \
	 (flavor_info[k_info[(T)->k_idx].flavor].x_attr) : \
	 (k_info[(T)->k_idx].x_attr))

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
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_char(T) \
	((k_info[(T)->k_idx].flavor) ? \
	 (flavor_info[k_info[(T)->k_idx].flavor].x_char) : \
	 (k_info[(T)->k_idx].x_char))

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
 * Artifacts use the "name1" field
 */
#define artifact_p(T) \
	(((T)->name1 || (T)->name3 || (T)->rart_name) ? TRUE : FALSE)

/*
 * Ego-Items use the "name2" field
 */
#define ego_item_p(T) \
	((T)->name2 ? TRUE : FALSE)

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
 * Artifact activations
 */
enum
{
    	/* Normal and special artifacts */
    
        ACT_ILLUMINATION,
        ACT_MAGIC_MAP,
        ACT_CLAIRVOYANCE,
        ACT_PROT_EVIL,
        ACT_DISP_EVIL,
        ACT_HEAL1,
        ACT_HEAL2,
        ACT_CURE_WOUNDS,
        ACT_HASTE1,
        ACT_HASTE2,
        ACT_FIRE1,
        ACT_FIRE2,
        ACT_FIRE3,
        ACT_FROST1,
        ACT_FROST2,
        ACT_FROST3,
        ACT_FROST4,
        ACT_FROST5,
        ACT_ACID1,
        ACT_RECHARGE1,
        ACT_SLEEP,
        ACT_LIGHTNING_BOLT,
        ACT_ELEC2,
        ACT_BANISHMENT,
        ACT_MASS_BANISHMENT,
        ACT_IDENTIFY,
        ACT_DRAIN_LIFE1,
        ACT_DRAIN_LIFE2,
        ACT_BIZZARE,
        ACT_STAR_BALL,
        ACT_RAGE_BLESS_RESIST,
        ACT_PHASE,
        ACT_TRAP_DOOR_DEST,
        ACT_DETECT,
        ACT_RESIST,
        ACT_TELEPORT,
        ACT_RESTORE_LIFE,
        ACT_MISSILE,
        ACT_ARROW,
        ACT_REM_FEAR_POIS,
        ACT_STINKING_CLOUD,
        ACT_STONE_TO_MUD,
        ACT_TELE_AWAY,
        ACT_WOR,
        ACT_CONFUSE,
        ACT_PROBE,
        ACT_FIREBRAND,
        ACT_STARLIGHT,
        ACT_MANA_BOLT,
        ACT_BERSERKER,
	
	/* Unique artifacts */
	
        ACT_BURST_CHAOS,
	ACT_ELEM_AIR,
        ACT_ELEM_FIRE,
        ACT_ELEM_WATER,
        ACT_NETHER_STORM,
        ACT_SUMM_NAZGUL,
        ACT_SUMM_PET,
        ACT_SUMM_TOWNSMEN,
        ACT_SUMM_VALA,
        ACT_ULTIMATE_DOMINATION,
        ACT_VECNA,

        ACT_MAX
};

/**** Object types ****/

/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */
struct object_kind
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte tval;			/* Object type */
	byte sval;			/* Object sub type */

	s16b pval;			/* Object extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage dice/sides */

	s32b weight;		/* Weight */

	s32b cost;			/* Object "base cost" */

	u32b flags1;		/* Flags, set 1 */
	u32b flags2;		/* Flags, set 2 */
	u32b flags3;		/* Flags, set 3 */
	u32b flags4;		/* Flags, set 4 (new) */

	byte locale[4];		/* Allocation level(s) */
	byte chance[4];		/* Allocation chance(s) */

	byte level;			/* Level */
	byte extra;			/* Something */


	byte d_attr;		/* Default object attribute */
	char d_char;		/* Default object character */


	byte x_attr;		/* Desired object attribute */
	char x_char;		/* Desired object character */


	u16b flavor;		/* Special object flavor (or zero) */


	bool aware;			/* The player is "aware" of the item's effects */

	bool tried;			/* The player has "tried" one of the items */

	bool squelch;		/* Squelch item if known            */

	bool everseen;		/* Used to despoilify squelch menus */
};


/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
struct artifact_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte tval;			/* Artifact type */
	byte sval;			/* Artifact sub type */

	s16b pval;			/* Artifact extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage when hits */

	s16b weight;		/* Weight */

	s32b cost;			/* Artifact "cost" */

	u32b flags1;		/* Artifact Flags, set 1 */
	u32b flags2;		/* Artifact Flags, set 2 */
	u32b flags3;		/* Artifact Flags, set 3 */
	u32b flags4;		/* Artifact Flags, set 4 (new) */

	byte level;			/* Artifact level */
	byte rarity;		/* Artifact rarity */

	byte cur_num;		/* Number created (0 or 1) */
	byte max_num;		/* Unused (should be "1") */

	byte activation;	/* Activation to use */
	u16b time;			/* Activation time */
	u16b randtime;		/* Activation time dice */
};


/*
 * Information about "ego-items".
 */
struct ego_item_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	s32b cost;			/* Ego-item "cost" */

	u32b flags1;		/* Ego-Item Flags, set 1 */
	u32b flags2;		/* Ego-Item Flags, set 2 */
	u32b flags3;		/* Ego-Item Flags, set 3 */
	u32b flags4;		/* Ego-Item Flags, set 4 (new) */

	byte level;			/* Minimum level */
	byte rarity;		/* Object rarity */
	byte rating;		/* Level rating boost */

	byte tval[EGO_TVALS_MAX]; /* Legal tval */
	byte min_sval[EGO_TVALS_MAX];	/* Minimum legal sval */
	byte max_sval[EGO_TVALS_MAX];	/* Maximum legal sval */

	byte max_to_h;		/* Maximum to-hit bonus */
	byte max_to_d;		/* Maximum to-dam bonus */
	byte max_to_a;		/* Maximum to-ac bonus */
	byte max_pval;		/* Maximum pval */

	byte xtra;			/* Extra sustain/resist/power */
};

/*
 * Object information, for a specific object.
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Note that "object" records are "copied" on a fairly regular basis,
 * and care must be taken when handling such objects.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 *
 * Each cave grid points to one (or zero) objects via the "o_idx"
 * field (above).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a "stack" of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "hold_o_idx"
 * field (below).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a pile of objects held by the monster.
 *
 * The "held_m_idx" field is used to indicate which monster, if any,
 * is holding the object.  Objects being held have "ix=0" and "iy=0".
 */
struct object_type
{
	s16b k_idx;			/* Kind index (zero if "dead") */

	byte iy;			/* Y-position on map, or zero */
	byte ix;			/* X-position on map, or zero */

	byte tval;			/* Item type (from kind) */
	byte sval;			/* Item sub-type (from kind) */

	s16b pval;			/* Item extra-parameter */

	byte discount;		/* Discount (if any) */

	byte number;		/* Number of items */

	s32b weight;		/* Item weight */

	byte name1;			/* Artifact type, if any */
	byte name2;			/* Ego-Item type, if any */
	s16b name3;			/* Unique artifact type, if any */

	byte xtra1;			/* Extra info type */
	byte xtra2;			/* Extra info index */

	s16b to_h;			/* Plusses to hit */
	s16b to_d;			/* Plusses to damage */
	s16b to_a;			/* Plusses to AC */

	s16b ac;			/* Normal AC */

	byte dd, ds;		/* Damage dice/sides */

	byte useof;		/* Mega-Hack -- use these flags instead of k_idx */
	u32b flags1;		/* Flags, set 1 */
	u32b flags2;		/* Flags, set 2 */
	u32b flags3;		/* Flags, set 3 */
	u32b flags4;		/* Flags, set 4 (new) */

	s16b timeout;		/* Timeout Counter */

	u32b ident;			/* Special flags (was byte) */

	byte marked;		/* Object is marked */

	u16b note;			/* Inscription index */

	s16b next_o_idx;	/* Next object in stack (if any) */

	s16b held_m_idx;	/* Monster holding us (if any) */
	
	u16b rart_name;		/* Randart name (quark) */
	u16b rart_desc;		/* Randart description (quark) */
};

struct flavor_type
{
	u32b text;      /* Text (offset) */

	byte tval;      /* Associated object type */
	byte sval;      /* Associated object sub-type */

	byte d_attr;    /* Default flavor attribute */
	char d_char;    /* Default flavor character */

	byte x_attr;    /* Desired flavor attribute */
	char x_char;    /* Desired flavor character */
};


/**** Object variables ****/

extern const byte chest_traps[64];
extern cptr inscrip_text[MAX_INSCRIP];

extern s16b o_max;
extern s16b o_cnt;
extern s16b (*cave_o_idx)[MAX_DUNGEON_WID];
extern object_type *o_list;
extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern artifact_type *a_info;
extern char *a_name;
extern char *a_text;
extern artifact_type *u_info;
extern char *u_name;
extern char *u_text;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern flavor_type *flavor_info;
extern char *flavor_name;
extern char *flavor_text;


/**** Object functions ****/

/* obj-info.c */
bool object_info_out(const object_type *o_ptr);
void object_info_screen(const object_type *o_ptr);

/* object.c */
void flavor_init(void);
void reset_visuals(bool prefs);
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4);
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4);
void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
void mimic_desc_object(char *buf, size_t max, s16b mimic_k_idx);
void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
void describe_item_activation(const object_type *o_ptr);
void identify_random_gen(const object_type *o_ptr);
char index_to_label(int i);
s16b label_to_inven(int c);
s16b label_to_equip(int c);
s16b wield_slot(const object_type *o_ptr);
cptr mention_use(int i);
cptr describe_use(int i);
bool item_tester_okay(const object_type *o_ptr);
int scan_floor(int *items, int size, int y, int x, int mode);
void display_inven(void);
void display_equip(void);
void show_inven(void);
void show_equip(void);
void show_floor(const int *floor_list, int floor_num);
void toggle_inven_equip(void);
bool get_item(int *cp, cptr pmt, cptr str, int mode);
void excise_object_idx(int o_idx);
void delete_object_idx(int o_idx);
void delete_object(int y, int x);
void compact_objects(int size);
void wipe_o_list(void);
s16b o_pop(void);
object_type* get_first_object(int y, int x);
object_type* get_next_object(const object_type *o_ptr);
errr get_obj_num_prep(void);
s16b get_obj_num(int level);
void object_known(object_type *o_ptr);
void object_aware(object_type *o_ptr);
void object_tried(object_type *o_ptr);
bool is_blessed(const object_type *o_ptr);
s32b object_value(const object_type *o_ptr);
void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
void reduce_charges(object_type *o_ptr, int amt);
bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
void object_absorb(object_type *o_ptr, const object_type *j_ptr);
s16b lookup_kind(int tval, int sval);
void object_wipe(object_type *o_ptr);
void object_copy(object_type *o_ptr, const object_type *j_ptr);
void object_prep(object_type *o_ptr, int k_idx);
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great);
bool make_object(object_type *j_ptr, bool good, bool great, int objecttype);
bool make_gold(object_type *j_ptr);
s16b floor_carry(int y, int x, object_type *j_ptr);
void drop_near(object_type *j_ptr, int chance, int y, int x);
void acquirement(int y1, int x1, int num, bool great);
void place_object(int y, int x, bool good, bool great, int droptype);
void place_quest_chest(int y, int x, bool good, bool great);
void place_gold(int y, int x);
void pick_trap(int y, int x);
void place_trap(int y, int x);
void place_secret_door(int y, int x);
void place_closed_door(int y, int x);
void place_random_door(int y, int x);
void inven_item_charges(int item);
void inven_item_describe(int item);
void inven_item_increase(int item, int num);
void inven_item_optimize(int item);
void floor_item_charges(int item);
void floor_item_describe(int item);
void floor_item_increase(int item, int num);
void floor_item_optimize(int item);
bool inven_carry_okay(const object_type *o_ptr);
s16b inven_carry(object_type *o_ptr);
s16b inven_takeoff(int item, int amt);
void inven_drop(int item, int amt);
void combine_pack(void);
void reorder_pack(void);
void steal_object_from_monster(int y, int x);
void create_quest_item(int ny, int nx);
int value_check_aux1(const object_type *o_ptr);
bool wearable_p(const object_type *o_ptr);

/* use-obj.c */
bool use_object(object_type *o_ptr, bool *ident);
void do_cmd_eat_food(void);
void do_cmd_quaff_potion(void);
void do_cmd_read_scroll(void);
void do_cmd_use_staff(void);
void do_cmd_aim_wand(void);
void do_cmd_zap_rod(void);
void do_cmd_activate(void);

/* squlech.c */
extern byte squelch_level[SQUELCH_BYTES];
extern byte auto_destroy;
extern bool squelch_corpses;
void do_cmd_squelch(void);
int squelch_itemp(object_type *, byte, int);
int do_squelch_item(int, int, object_type *);
void rearrange_stack(int, int);
void do_squelch_pile(int, int);

/* randart.c */
errr do_randart(u32b randart_seed, bool full);
s32b artifact_power(int a_idx);
extern bool preserve_base_type_hack;
bool scramble_artifact_random(int oa_idx, object_type *o_ptr);
void randart_init(void);
void randart_free(void);

#endif /* OBJECT_H_INCLUDED */
