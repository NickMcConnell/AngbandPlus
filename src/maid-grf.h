/* File: maid-grf.h */

/* Purpose: Common header file for graphics ports */

/*
 * Copyright (c) 2002 Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/*
 * List of callback types
 */
#define CALL_MAP_INFO		1
#define CALL_MAP_ERASE		2
#define CALL_PLAYER_MOVE	3
#define CALL_OBJECT_LIST	4

#define CALL_MAX			5

/*
 * Callback type (Ignoring parameters)
 */
typedef void (*callback_type) (void);

/*
 * Linked list for callbacks
 */
typedef struct callback_list callback_list;

struct callback_list
{
	callback_type func;	/* Function to call */
	callback_list *next;	/* Next callback in list */
	vptr data;	/* Data for callbacks so can use same function multiple times */
};

extern void set_callback(callback_type call_func, int number, vptr data);
extern void del_callback(int number, vptr data);


/*
 * Number of blocks in the overhead map cache.
 * 
 * This number must be bigger than the number of blocks
 * required to remember the biggest dungeon level.
 */
#define MAP_CACHE	(WILD_VIEW * WILD_VIEW * 2)


/*
 * Constants used to pass information to users
 * of the overhead map hooks.
 */
#define MAP_SEEN	0x01		/* GRID_SEEN equivalent */
#define MAP_GLOW	0x02		/* CAVE_GLOW equivalent */
#define MAP_LITE	0x04		/* GRID_LITE equivalent */
#define MAP_ONCE    0x08		/* This block has ever been seen */

/*
 * Constants used to describe the state of monsters
 * to users of the overhead map
 */
#define MONST_ASLEEP	0x01
#define MONST_FRIEND	0x02
#define MONST_PET		0x04
#define MONST_CONFUSED	0x08
#define MONST_FEAR		0x10
#define MONST_STUN		0x20
#define MONST_INVULN	0x40


/*
 * Make an iterator, so we can scan the map quickly
 */
#define MAP_ITT_START(M) \
	do { \
		int _map_count;\
		\
		for (_map_count = 0; _map_count < MAP_CACHE; _map_count++) \
		{ \
			int _map_i, _map_j; \
			\
			if (map_cache_x[_map_count] == -1) continue; \
			\
			if (map_grid[map_cache_y[_map_count]][map_cache_x[_map_count]] == -1)\
				 continue; \
			\
			for (_map_i = 0; _map_i < WILD_BLOCK_SIZE; _map_i++) \
			{ \
				for (_map_j = 0; _map_j < WILD_BLOCK_SIZE; _map_j++) \
				{ \
					(M) = &map_cache[_map_count][_map_j][_map_i];


#define MAP_ITT_END \
				} \
			} \
		} \
	} while (0)

/* Macro to extract location during iteration */
#define MAP_GET_LOC(X, Y)\
	(((X) = _map_i + map_cache_x[_map_count] * WILD_BLOCK_SIZE), \
	 ((Y) = _map_j + map_cache_y[_map_count] * WILD_BLOCK_SIZE))

/*
 * Map data structure
 */
typedef struct term_map term_map;

struct term_map
{
	/* Information about what is at the grid */
	s16b object;
	s16b monster;
	s16b field;
	byte terrain;
	char unknown;

	/* Location */
	u16b x;
	u16b y;

	/* Player-known flags */
	byte flags;

	/* Monster flags */
	byte m_flags;

	/* Rough measure of monster hp */
	byte m_hp;
	
	/* Priority for overhead mini map */
	byte priority;
	
	/* Map information about square */
	byte a;
	char c;
	byte ta;
	char tc;
};

typedef struct map_block map_block;

struct map_block
{
	/* Save what it looks like */
	byte a;
	char c;
	
	byte ta;
	char tc;

	/* Save the cave info itself - used by the borg */
#ifdef TERM_CAVE_MAP
	s16b object;
	s16b monster;
	s16b field;
	byte terrain;

	/* unknown mimics or flavoured objects */
	char unknown;

	/* Monster flags */
	byte m_flags;

	/* Monster hp (scaled) */
	byte m_hp;
#endif /* TERM_CAVE_MAP */

	/* Borg-specific stuff */
#ifdef ALLOW_BORG
	u16b fear;	/* fear value */

	u16b kill;	/* Entry into "kill" list */
	u16b take;	/* Entry into "take" list */
	u16b trap;	/* MT - Entry into "trap" list */
	u16b m_effect;  /* MT - Magic Effect, like glyphs */
	
	byte feat;	/* Terrain feature */
	byte info;	/* info flags */

	byte flow;	/* "flow" data */
	byte cost;	/* "cost" data */

	byte detect;	/* Detection flags */
	byte xtra;	/* search count */
#endif /* ALLOW_BORG */

	/* We need to save the flags to get the refcounting right. */
	byte flags;
	
	/* Priority of tile (For overhead map display) */
	byte priority;
};

typedef map_block *map_blk_ptr;
typedef map_block **map_blk_ptr_ptr;

typedef void (*map_info_hook_type) (map_block *mb_ptr, const term_map *map, vptr data);
typedef void (*map_erase_hook_type) (vptr data);
typedef void (*player_move_hook_type) (int x, int y, vptr data);


/*
 * This is used by the Borg and by ports that would
 * like to access lists of objects
 */
#ifdef TERM_USE_LIST

/*
 * Object List data structure
 */
typedef struct term_list term_list;

struct term_list
{
	cptr o_name;	/* Name */
	cptr xtra_name;	/* Extra Name (Artifacts and ego items) */

	u32b kn_flags[4];	/* Known Flags */

	s32b cost;	/* Object "base cost" */

	s16b k_idx;	/* Kind index (zero if "unknown") */
	s16b weight;	/* Item weight */

	s16b to_h;	/* Bonus to hit */
	s16b to_d;	/* Bonus to dam */
	s16b to_a;	/* Bonus to ac */
	s16b ac;	/* Armor class */
	byte dd;	/* Damage dice */
	byte ds;	/* Damage sides */

	byte number;	/* Number of items */
	byte info;	/* Special flags */

	s16b timeout;	/* Timeout Counter */

	s16b pval;	/* Particular value */

	byte tval;	/* Item type (from kind) */
};


typedef struct list_item list_item;

struct list_item
{
	cptr o_name;	/* Name */
	cptr xtra_name;	/* Extra Name (Artifacts and ego items) */

	u32b kn_flags[4];	/* Known Flags */

	s32b cost;	/* Object "base cost" */

	s16b k_idx;	/* Kind index (zero if "dead") */
	s16b weight;	/* Item weight */

	s16b to_h;	/* Bonus to hit */
	s16b to_d;	/* Bonus to dam */
	s16b to_a;	/* Bonus to ac */
	s16b ac;	/* Armor class */
	byte dd;	/* Damage dice */
	byte ds;	/* Damage sides */

	byte number;	/* Number of items */
	byte info;	/* Special flags */

	s16b timeout;	/* Timeout Counter */

	s16b pval;	/* Particular value */
	byte tval;	/* Item type (from kind) */

	/* Save the glyph to use */
#ifdef TERM_OBJ_GLYPH
	u16b feature_code;
#endif /* TERM_OBJ_GLYPH */

#ifdef ALLOW_BORG
	byte treat_as;	/* Treat item as if it is in a different list */
#endif /* ALLOW_BORG */
};

typedef void (*list_notice_hook_type) (byte, vptr data);


#endif /* TERM_USE_LIST */

/* Extern Variables */
extern byte gamma_table[256];
extern map_blk_ptr_ptr *map_cache;
extern s16b *map_cache_refcount;
extern int *map_cache_x;
extern int *map_cache_y;
extern int **map_grid;

#ifdef TERM_USE_LIST
extern list_item *equipment;
extern int equip_num;
extern list_item *inventory;
extern int inven_num;
extern list_item *cur_list;
extern int cur_num;
#endif /* TERM_USE_LIST */

/* Extern Functions */
extern void build_gamma_table(int gamma);
extern cptr get_default_font(int term_num);
extern bool pick_graphics(int graphics, int *xsize, int *ysize, char *filename);
extern bool is_bigtiled(int x, int y);
extern void toggle_bigtile(void);
extern void map_get_player(int *x, int *y);
extern bool map_in_bounds(int x, int y);
extern map_block *map_loc(int dx, int dy);
