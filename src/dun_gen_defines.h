#ifndef DUN_GEN_DEFINES
#define DUN_GEN_DEFINES

#endif // DUN_GEN_DEFINES

/*
 * Dungeon generation values
 */


#define DUN_ROOMS	50
#define DUN_UNUSUAL_MORIA	300	/* Level/chance of unusual room */
#define DUN_UNUSUAL	180	/* Level/chance of unusual room */
#define DUN_DEST	35	/* 1/chance of having a destroyed level */
#define DUN_FRACTAL	25	/* 1/chance of having a fractal level */
#define SMALL_LEVEL	10	/* 1/chance of smaller size */
#define THEMED_LEVEL_CHANCE	75	/* 1/chance of being a themed level */
#define WILDERNESS_LEVEL_CHANCE	75 /* 1/chance of being a pseudo-wilderness level */
#define LABYRINTH_LEVEL_CHANCE	75 /* 1/chance of being a labrynth level */
#define GREATER_VAULT_LEVEL_CHANCE	150 /* 1/chance of being a greater vault level */

#define DUN_MAX_LAKES	3	/* Maximum number of lakes/rivers */
#define DUN_FEAT_RNG	2	/* Width of lake */

/* Maximum size of a fractal map */
#define MAX_FRACTAL_SIZE 65

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND	10	/* Chance of random direction */
#define DUN_TUN_CHG	30	/* Chance of changing direction */
#define DUN_TUN_CON	15	/* Chance of extra tunneling */
#define DUN_TUN_PEN	25	/* Chance of doors at room entrances */
#define DUN_TUN_JCT	90	/* Chance of doors at tunnel junctions */

/*
 * Dungeon streamer generation values
 */
#define DUN_STR_DEN	5	/* Density of streamers */
#define DUN_STR_RNG	2	/* Width of streamers */
#define DUN_STR_MAG	3	/* Number of magma streamers */
#define DUN_STR_MC	90	/* 1/chance of treasure per magma */
#define DUN_STR_QUA	2	/* Number of quartz streamers */
#define DUN_STR_QC	40	/* 1/chance of treasure per quartz */
#define DUN_STR_SAN	2	/* Number of sandstone streamers */
#define DUN_STR_SLV	40	/* Deepest level sandstone occurs instead of magma */
#define DUN_STR_GOL	20	/* 1/chance of rich mineral vein */
#define DUN_STR_GC	2	/* 1/chance of treasure per rich mineral vein */
#define DUN_STR_CRA	8	/* 1/chance of cracks through dungeon */
#define DUN_STR_CC	0	/* 1/chance of treasure per crack */

/*
 * Dungeon treausre allocation values
 */
#define DUN_AMT_ROOM	9	/* Amount of objects for rooms */
#define DUN_AMT_ITEM	3	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD	3	/* Amount of treasure for rooms/corridors */

#define DUN_AMT_ROOM_MORIA	7	/* Amount of objects for rooms */
#define DUN_AMT_ITEM_MORIA	2	/* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD_MORIA	2	/* Amount of treasure for rooms/corridors */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR		1	/* Hallway */
#define ALLOC_SET_ROOM		2	/* Room */
#define ALLOC_SET_BOTH		3	/* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE	1	/* Rubble */
#define ALLOC_TYP_TRAP		3	/* Trap */
#define ALLOC_TYP_GOLD		4	/* Gold */
#define ALLOC_TYP_OBJECT	5	/* Object */
#define ALLOC_TYP_CHEST		6	/* Object */
#define ALLOC_TYP_PARCHMENT 7	/* Special parchment, for collection quests */


/*
 * Maximum numbers of rooms along each axis (currently 6x18)
 */

#define MAX_ROOMS_ROW	(MAX_DUNGEON_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL	(MAX_DUNGEON_WID / BLOCK_WID)

/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX	110
#define DOOR_MAX	200
#define WALL_MAX	500
#define TUNN_MAX	900


/*
 * These flags control the construction of starburst rooms and lakes
 */
#define STAR_BURST_ROOM		0x00000001	/* Mark grids with CAVE_ROOM */
#define STAR_BURST_LIGHT	0x00000002	/* Mark grids with CAVE_GLOW */
#define STAR_BURST_CLOVER	0x00000004	/* Allow cloverleaf rooms */
#define STAR_BURST_RAW_FLOOR	0x00000008	/* Floor overwrites dungeon */
#define STAR_BURST_RAW_EDGE	0x00000010	/* Edge overwrites dungeon */


/*
 * Maximal number of room types
 */
#define ROOM_MAX	15
#define ROOM_MIN	2

/*
 * Maximum distance between rooms
 */
#define MAX_RANGE_TO_ROOM 15

/* Available fractal map types */
enum
{
    FRACTAL_TYPE_17x33 = 0,
    FRACTAL_TYPE_33x65,
    FRACTAL_TYPE_9x9,
    FRACTAL_TYPE_17x17,
    FRACTAL_TYPE_33x33,
    MAX_FRACTAL_TYPES
};

/* These ones are the valid values for the map grids */
enum
{
    FRACTAL_NONE = 0,	/* Used only at construction time */
    FRACTAL_WALL,       /* Wall grid */
    FRACTAL_EDGE,       /* Wall grid adjacent to floor (outer wall) */
    FRACTAL_FLOOR,      /* Floor grid */
    FRACTAL_POOL_1,     /* Pool grid */
    FRACTAL_POOL_2,     /* Pool grid */
    FRACTAL_POOL_3,     /* Pool grid */
};

#define SIZE_FRACTAL_REPOSITORY  5

/* Verify that a point is inside a fractal */
#define IN_FRACTAL(template,y,x) \
    (((y) >= 0) && ((y) < (template)->size) && \
    ((x) >= 0) && ((x) < (template)->size))

#define CUR_NUM_THEME_LEVEL_FLAGS       8
#define CUR_NUM_THEME_LEVEL_FEATURES    11
#define CUR_NUM_ELEMENTAL_TRANSFORMATIONS 15

/*
 * The center of a fog formation must be close to water/ice. This is the
 * radius of the area we check for water/ice grids.
 */
#define FOG_SRC_RAD 7

/* Transformation types */
#define TRANSFORM_WALL		1
#define TRANSFORM_REGION	2

/* Available size for lakes (in blocks) */
enum
{
    LAKE_DATA_2x2,
    LAKE_DATA_2x3,
    LAKE_DATA_3x3,
    LAKE_DATA_3x4,
    LAKE_DATA_4x4,
    LAKE_DATA_4x5,
    MAX_LAKE_DATA
};

/* Note the height and width must be an odd number */
#define LABYRINTH_HGT 41
#define LABYRINTH_WID 81
#define LABYRINTH_AREA (LABYRINTH_WID * LABYRINTH_HGT)
