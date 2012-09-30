/* File: wild.h */

/* Purpose: Wilderness + Quest generation header file */

/*
 * Copyright (c) 1989, 2003 James E. Wilson, Robert A. Koeneke,
 *                          Robert Ruehlmann, Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/* Include low-level grid stuff */
#include "grid.h"

/* 1/4 of the wilderness is sea */
#define	SEA_FRACTION	4

/* Number of lakes to try and make */
#define LAKE_NUM		4

/* Constant^2 that determines number of rivers */
#define RIVER_NUM		4

/* Maximum distance a road can connect */
#define ROAD_DIST		30

/* Minimum fractional distance a road can approach a non-connecting town */
#define ROAD_MIN		3

/* Minimum separation between towns */
#define MIN_DIST_TOWN	10

/* Minimum separation between quests */
#define MIN_DIST_QUEST	10

/* Minimum separation between dungeons */
#define MIN_DIST_DUNGEON	8

/* Number of wilderness places */
#define NUM_TOWNS		20
#define NUM_DUNGEON		20

/* Dodgy replacement for SCREEN_WID and SCREEN_HGT */

/* This will be removed later. */
#define TOWN_WID	66
#define TOWN_HGT	22

#define V_TOWN_BLOCK_WID	\
	(((TOWN_WID / WILD_BLOCK_SIZE) + 1) * WILD_BLOCK_SIZE)
#define V_TOWN_BLOCK_HGT	\
	(((TOWN_HGT / WILD_BLOCK_SIZE) + 1) * WILD_BLOCK_SIZE)

/* Starting town has pre-defined stores */
#define START_STORE_NUM     7

/* Town monster types */
#define TOWN_MONST_VILLAGER		1
#define TOWN_MONST_ELVES		2
#define TOWN_MONST_DWARF		3
#define TOWN_MONST_LIZARD		4
#define TOWN_MONST_MONST		5
#define TOWN_MONST_ABANDONED	6


/* Road constants used to define with of the path */
#define ROAD_LEVEL		(WILD_BLOCK_SIZE * 150)
#define TRACK_LEVEL		(WILD_BLOCK_SIZE * 140)
#define ROAD_BORDER		(WILD_BLOCK_SIZE * 120)
#define GROUND_LEVEL	(WILD_BLOCK_SIZE * 100)

/* Decision tree constants */

/* Lower two bits describe cut */
#define DT_HGT		0x01
#define DT_POP		0x02
#define DT_LAW		0x03

/* These two bits describe the direction to branch */
#define DT_LEFT		0x04
#define DT_RIGHT	0x08

/* Town size cutoffs for names */
#define T_SIZE_SMALL	(128 + 30)
#define T_SIZE_TOWN		(128 + 50)
#define T_SIZE_CITY		(128 + 70)
#define T_SIZE_CASTLE	(128 +	100)

/* Town building constants */
#define CITY_OUTSIDE	0
#define CITY_WALL		1
#define CITY_INSIDE		2


/* Quest status */
#define QUEST_STATUS_UNTAKEN		0
#define QUEST_STATUS_TAKEN			1
#define QUEST_STATUS_COMPLETED		2
#define QUEST_STATUS_FINISHED		3

/* Quest creation flags */
#define Q_GEN_PICKY		0x01
#define Q_GEN_OCEAN		0x02

/* Quest flags */
#define QUEST_FLAG_ACTIVE		0x01	/* Quest triggers have effect */
#define QUEST_FLAG_TIME			0x02	/* Quest has timeout */
#define QUEST_FLAG_ITEM			0x04	/* Player has art. quest item */
#define QUEST_FLAG_DUMMY		0x08
#define QUEST_FLAG_KNOWN		0x10	/* Player knows about this quest */

/* Helper defines for random quests */
#define QUEST_CAMP_MON		5	/* One in five squares has a monster */
#define QUEST_CAMP_OBJ		5	/* One in five squares has an object */
#define QUEST_CAMP_SCATTER	10	/* Non-camp sqaures have stuff */

/* Dungeon flags */
#define DF_NONE			0x00
#define DF_ROAD			0x01
#define DF_TRACK		0x02


/* Building types */
#define BT_GENERAL		0
#define BT_STORE		1
#define BT_BUILD		2

/* Some useful macros */
#define build_is_store(X) \
	(wild_build[X].type == BT_STORE)

#define build_is_general(X) \
	(wild_build[X].type == BT_GENERAL)

#define build_is_build(X) \
	(wild_build[X].type == BT_BUILD)

/* Wilderness building info type */
typedef struct wild_building_type wild_building_type;

struct wild_building_type
{
	u16b gen;	/* Created */
	u16b field;	/* Field type, if applicable */

	byte type;	/* Type of building */

	/* Suggested location in parameter space */
	byte pop;
	byte magic;
	byte law;

	u16b rarity;	/* Rarity of store */
};

/* Quest generation helper */
typedef struct quest_aux_type quest_aux_type;

struct quest_aux_type
{
	bool (*hook_func) (int r_idx);
	int level;
	int chance;
	cptr name;
};

/* wild1.c */
extern wild_building_type wild_build[MAX_CITY_BUILD];
extern bool init_places(int xx, int yy);
extern void clear_temp_block(void);
extern void set_temp_corner_val(u16b val);
extern void set_temp_mid(u16b val);
extern void frac_block(void);

/* wild2.c */
extern void draw_city(place_type *pl_ptr);
extern void draw_dungeon(place_type *pl_ptr);
extern void van_town_gen(place_type *pl_ptr);
extern void init_vanilla_town(void);

/* quest.c */
extern void pick_wild_quest(int *xsize, int *ysize, byte *flags);
extern bool quest_blank(int x, int y, int xsize, int ysize, int place_num,
						byte flags);
extern bool create_quest(int x, int y, int place_num);
extern void draw_quest(place_type *pl_ptr);
