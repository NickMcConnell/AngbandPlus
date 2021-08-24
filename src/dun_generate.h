#ifndef DUN_GENERATE_H
#define DUN_GENERATE_H

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


bool allow_uniques;


/*
 * Maximal number of room types
 */
#define ROOM_MAX	15
#define ROOM_MIN	2

/*
 * Maximum distance between rooms
 */
#define MAX_RANGE_TO_ROOM 15




/*
 * Room type information
 */

typedef struct room_data room_data;

struct room_data
{
    /* Required size in blocks */
    s16b dy1, dy2, dx1, dx2;

    /* Hack -- minimum level */
    s16b level;
};


/*
 * Structure to hold all "dungeon generation" data
 */

typedef struct dun_data dun_data;

struct dun_data
{
    /* Array of centers of rooms */
    int cent_n;
    coord cent[CENT_MAX];

    /* Array of possible door locations */
    int door_n;
    coord door[DOOR_MAX];

    /* Array of wall piercing locations */
    int wall_n;
    coord wall[WALL_MAX];

    /* Array of tunnel grids */
    int tunn_n;
    coord tunn[TUNN_MAX];

    /* Number of blocks along each axis */
    int row_rooms;
    int col_rooms;

    /* Array of which blocks are used */
    bool room_map[MAX_ROOMS_ROW][MAX_ROOMS_COL];

    /* Hack -- there is a pit/nest on this level */
    bool crowded;
};


/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;


/*
 * Array of room types (assumes 11x11 blocks)
 */
static const room_data room[ROOM_MAX] =
{
    { 0, 0, 0, 0, 0 },		/* 0 = Nothing */
    { 0, 0, -1, 1, 1 },		/* 1 = Simple (33x11) */
    { 0, 0, -1, 1, 1 },		/* 2 = Overlapping (33x11) */
    { 0, 0, -1, 1, 3 },		/* 3 = Crossed (33x11) */
    { 0, 0, -1, 1, 3 },		/* 4 = Large (33x11) */
    { 0, 0, -1, 1, 5 },		/* 5 = Monster nest (33x11) */
    { 0, 0, -1, 1, 5 },		/* 6 = Monster pit (33x11) */
    { 0, 1, -1, 1, 5 },		/* 7 = Lesser vault (33x22) */
    { -1, 2, -2, 3, 10 },	/* 8 = Greater vault (66x44) */
    { 0, 1, -1, 1, 0 },		/* 9 = Quest vault (44x22) */
    { 0, 1, -1, 1, 0},		/* 10 = Starburst (33x22) */
    { -1, 2, -2, 3, 0},		/* 11 = Great Starburst (66x44) */

    /*
     * Hack - if the dimensions for starburst rooms or great starburst
     * rooms change, these lines in build_type_starburst need changing:
     if (giant_room)
     {
        dy = 19;
        dx = 30;
     }
     33x22
     else
     {
        dy = 9;
        dx = 14;
     }

     -JG
     */

    {0, 1, -1, 1, 0},		/* 12 = Small Fractal (33x22) */
    {-1, 1, -1, 1, 0},		/* 13 = Medium Fractal (33x33) */
    {-1, 1, -2, 3, 0}		/* 14 = Big Fractal (66x33) */
};


/*
 * This table holds aditional flags for themed levels
 * These flags are used to forbid the generation of certain features BEFORE
 * placing any lakes or pools.
 * Example: you can assign LF1_LAVA to a red dragon level to avoid the
 * generation of ice, acid, oil, etc.
 * See "build_themed_level_nature"
 */
static struct
{
    byte theme;
    u32b flags;		/* A combination of the LF1_* flags */
} themed_level_flags[] =
{
    {LEV_THEME_DEMON_MINOR,	LF1_FIRE},
    {LEV_THEME_DEMON_ALL,	LF1_FIRE},
    {LEV_THEME_DEMON_MAJOR,	LF1_LAVA | LF1_FIRE},
    {LEV_THEME_DRAGON_FIRE,	LF1_FIRE},
    {LEV_THEME_DRAGON_ACID,	LF1_ACID},
    {LEV_THEME_DRAGON_ELEC,	LF1_WATER},
    {LEV_THEME_DRAGON_COLD,	LF1_ICE},
    {LEV_THEME_UNDEAD,		LF1_ICE},
    /* Add entries for more themed levels if needed */
};

/*
 * This table holds the default features used in *almost* all lakes and pools
 * of a themed level.
 * See "pick_proper_feature"
 */
static struct
{
    byte theme;
    u16b feature;
} themed_level_features[] =
{
    {LEV_THEME_DEMON_MINOR,	FEAT_FIRE},
    {LEV_THEME_DEMON_ALL,	FEAT_FIRE},
    {LEV_THEME_DEMON_MAJOR,	FEAT_FLOOR_LAVA},
    {LEV_THEME_DEMON_MAJOR,	FEAT_FIRE},
    {LEV_THEME_DRAGON_FIRE,	FEAT_FIRE},
    {LEV_THEME_DRAGON_ACID,	FEAT_FLOOR_ACID},
    {LEV_THEME_DRAGON_ELEC,	FEAT_FLOOR_WATER},
    {LEV_THEME_DRAGON_COLD,	FEAT_FLOOR_ICE},
    {LEV_THEME_TROLL,		FEAT_FOREST_SOIL},
    {LEV_THEME_OGRE,		FEAT_FOREST_SOIL},
    {LEV_THEME_OGRE,		FEAT_THICKET},
    /* Add entries for more themed levels if needed */
};

/*
 * A feature pair (wall + floor) that contains also a chance value
 * and a radius
 */
typedef struct
{
    u32b level_flag;
    u16b wall;
    u16b floor;
    u16b chance;
    byte rad;
} feature_selector_item_type;

/*
 * An array of feature pairs. It's used to pick random pairs based
 * on their chance values
 */
typedef struct
{
    feature_selector_item_type items[30];
    byte size;
    u16b total_chance;	/* The sum of the chances of each pair */
} feature_selector_type;

/*
 * Table of "magic" values used to ponder the size of the dungeon
 * 1 means "tiny": 2x2, 2x3, 2x4, 3x2, 3x3, 4x2
 * 2 means "small rectangle": 2x5, 2x6, 3x4, 4x3, 5x2, 6x2
 * 3 means "medium rectangle": 3x5, 3x6, 4x4, 5x3, 6x3
 * 4 means "medium": 4x5, 4x6, 5x4, 6x4
 * 5 means "large": 5x5, 5x6, 6x5
 * 6 means "largest": 6x6
 */
static byte dungeon_size_tab[7][7] =
{
    /*	0	1	2	3	4	5	6	*/
    {	0,	0,	0,	0,	0,	0,	0	},	/* 0 */
    {	0,	0,	0,	0,	0,	0,	0	},	/* 1 */
    {	0,	0,	1,	1,	1,	2,	2	},	/* 2 */
    {	0,	0,	1,	1,	2,	3,	3	},	/* 3 */
    {	0,	0,	1,	2,	3,	4,	4	},	/* 4 */
    {	0,	0,	2,	3,	4,	5,	5	},	/* 5 */
    {	0,	0,	2,	3,	4,	5,	6	},	/* 6 */
};

static bool room_build(int by0, int bx0, int typ);

/*
 * A circular queue of map locations.
 * Check out defines.h and util.c for usage
 */
typedef struct
{
    /* Maximum number of grids in the queue */
    size_t max_size;
    /* Grid data */
    coord *data;
    /* Head and tail of the queue */
    size_t head, tail;
} grid_queue_type;

/* Utilitary macros for the grid queue type */

/* These two return the coordinates of the grid at the front of the queue */
/* THE QUEUE MUST CONTAIN AT LEAST ONE ELEMENT */
#define GRID_QUEUE_Y(Q) ((Q)->data[(Q)->head].y)
#define GRID_QUEUE_X(Q) ((Q)->data[(Q)->head].x)

/* Returns TRUE if the given queue is empty */
#define GRID_QUEUE_EMPTY(Q) ((Q)->head == (Q)->tail)

/* Returns TRUE if the given queue is full */
#define GRID_QUEUE_FULL(Q) ((((Q)->tail + 1) % (Q)->max_size) == (Q)->head)

#endif // DUN_GENERATE_H
