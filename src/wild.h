/* File: wild1.h */

/* Purpose: Wilderness generation header file */

/*
 * Copyright (c) 1989, 1999 James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


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
#define TOWN_MIN_DIST	16


/* Dodgy replacement for SCREEN_WID and SCREEN_HGT */

/* This will be removed later. */
#define TOWN_WID	66
#define TOWN_HGT	22

/* Starting town has pre-defined stores */
#define START_STORE_NUM		6


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
	u16b	gen;	/* Created */
	u16b	field;	/* Field type, if applicable */
	
	byte	type;	/* Type of building */
	
	/* Suggested location in parameter space */
	byte	pop;
	byte	magic;
	byte	law;
	
	u16b	rarity;	/* Rarity of store */
};


/* Externs */

extern int wild_stairs_x;
extern int wild_stairs_y;
extern wild_building_type	wild_build[MAX_CITY_BUILD];
extern byte build_x[WILD_BLOCK_SIZE * WILD_BLOCK_SIZE];
extern byte build_y[WILD_BLOCK_SIZE * WILD_BLOCK_SIZE];

extern byte fill_town_driver(void);
extern void clear_temp_block(void);
extern void set_temp_corner_val(u16b val);
extern void set_temp_mid(u16b val);
extern void frac_block(void);
extern void draw_city(u16b town_num);
extern void van_town_gen(u16b town_num);
extern void init_wild_cache(void);
