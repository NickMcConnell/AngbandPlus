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

/* cave.h: level generation, dungeon */

#ifndef CAVE_H_INCLUDED
#define CAVE_H_INCLUDED

/**** Dungeon/screen sizes ****/

/*
 * Number of grids in each block (vertically)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_HGT	11

/*
 * Number of grids in each block (horizontally)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_WID	11

/*
 * Number of grids in each panel (vertically)
 * Must be a multiple of BLOCK_HGT
 */
#define PANEL_HGT	11

/* Since dungeon generation uses PANEL wid to generate dungeons,
 * the width there can't be dependent on bigtile mode.
 */
#define PANEL_WID_FIXED	33

/*
 * Number of grids in each panel (horizontally)
 * Must be a multiple of BLOCK_WID
 */
#define PANEL_WID	(use_bigtile ? 16 : PANEL_WID_FIXED)

/*
 * Number of grids in each screen (vertically)
 * Must be a multiple of PANEL_HGT (at least 2x)
 */
#define SCREEN_HGT	(Term->hgt - ROW_MAP - 1)

/*
 * Number of grids in each screen (horizontally)
 * Must be a multiple of PANEL_WID (at least 2x)
 */
#define SCREEN_WID	((Term->wid - COL_MAP - 1) / (use_bigtile ? 2 : 1))

/*
 * Number of grids in each dungeon (horizontally)
 * Must be a multiple of SCREEN_HGT
 * Must be less or equal to 256
 */
#define MAX_DUNGEON_HGT		66

/*
 * Number of grids in each dungeon (vertically)
 * Must be a multiple of SCREEN_WID
 * Must be less or equal to 256
 */
#define MAX_DUNGEON_WID		198

/*
 * Number of rooms to attempt to generate in dungeon.
 */
#define DUN_ROOMS		50

/*
 * Maximum dungeon level.  The player can never reach this level
 * in the dungeon, and this value is used for various calculations
 * involving object and monster creation.  It must be at least 100.
 * Setting it below 128 may prevent the creation of some objects.
 */
#define MAX_DEPTH	128

/*
 * Maximum size of the "view" array (see "cave.c")
 * Note that the "view radius" will NEVER exceed 20, and even if the "view"
 * was octagonal, we would never require more than 1520 entries in the array.
 */
#define VIEW_MAX 1536

/*
 * Maximum size of the "temp" array (see "cave.c")
 * Note that we must be as large as "VIEW_MAX" for proper functioning
 * of the "update_view()" function, and we must also be as large as the
 * largest illuminatable room, but no room is larger than 800 grids.  We
 * must also be large enough to allow "good enough" use as a circular queue,
 * to calculate monster flow, but note that the flow code is "paranoid".
 */
#define TEMP_MAX 1536

/*
 * Maximum distance from the character to store flow (noise) information
 */
#define NOISE_STRENGTH   45

/*
 * Character turns it takes for smell to totally dissipate
 */
#define SMELL_STRENGTH   50

/*
 * There is a 1/160 chance per round of creating a new monster
 */
#define MAX_M_ALLOC_CHANCE	160

/*
 * Normal levels get at least 14 monsters
 */
#define MIN_M_ALLOC_LEVEL	14

/*
 * The town starts out with 4 residents during the day
 */
#define MIN_M_ALLOC_TD		4

/*
 * The town starts out with 8 residents during the night
 */
#define MIN_M_ALLOC_TN		8

/*
 * A "stack" of items is limited to less than 100 items (hard-coded).
 */
#define MAX_STACK_SIZE			100

/*
 * Maximum number of objects allowed in a single dungeon grid.
 *
 * The main-screen has a minimum size of 24 rows, so we can always
 * display 23 objects + 1 header line.
 */
#define MAX_FLOOR_STACK			23

/*** Feature Indexes (see "lib/edit/feature.txt") ***/

/* Nothing */
#define FEAT_NONE		0x00	/* Should not be used */

/* Various */
#define FEAT_FLOOR		0x01
#define FEAT_INVIS		0x02	/* Not-yet-discovered trap */
#define FEAT_GLYPH		0x03	/* Glyph of warding */
#define FEAT_OPEN		0x04	/* Open door */
#define FEAT_BROKEN		0x05	/* Broken door */

/* Stairs moved to make way for adventurers guild */
#define FEAT_LESS		0x50	/* Stair (up) */
#define FEAT_MORE		0x51	/* Stair (down) */

#define FEAT_LESS_SHAFT		0x52	/* Shaft (up) */
#define FEAT_MORE_SHAFT		0x53	/* Shaft (down) */

/* Stair Head/Tail */
#define FEAT_STAIR_HEAD		0x50
#define FEAT_STAIR_TAIL		0x53

/* Shops */
#define FEAT_SHOP_HEAD		0x06
#define FEAT_SHOP_TAIL		0x0E

/* Traps */
#define FEAT_TRAP_HEAD		0x0F
#define FEAT_TRAP_TAIL		0x1F

/* Doors */
#define FEAT_DOOR_HEAD		0x20
#define FEAT_DOOR_TAIL		0x2F

/* Walls */
#define FEAT_WALL_HEAD		0x30
#define FEAT_WALL_TAIL		0x3F

/* Extra */
#define FEAT_SECRET		0x30	/* Secret door */
#define FEAT_RUBBLE		0x31	/* Rubble */

/* Seams */
#define FEAT_MAGMA		0x32	/* Magma */
#define FEAT_QUARTZ		0x33	/* Quartz */
#define FEAT_MAGMA_H		0x34	/* Unused? */
#define FEAT_QUARTZ_H		0x35	/* Unused? */
#define FEAT_MAGMA_K		0x36	/* Magma with treasure */
#define FEAT_QUARTZ_K		0x37	/* Quartz with treasure */

/* Walls (look similar to player, but are generated differently) */
#define FEAT_WALL_EXTRA		0x38
#define FEAT_WALL_INNER		0x39
#define FEAT_WALL_OUTER		0x3A
#define FEAT_WALL_SOLID		0x3B

/* Permanent walls (look similar to player, but are generated differently) */
#define FEAT_PERM_EXTRA		0x3C
#define FEAT_PERM_INNER		0x3D
#define FEAT_PERM_OUTER		0x3E
#define FEAT_PERM_SOLID		0x3F

/* Specials trap that only effects monsters.  Created only by rogues. */
#define FEAT_MTRAP_HEAD		0x40
#define FEAT_MTRAP_TAIL		0x4F
#define FEAT_MTRAP_BASE		0x40 /* Level 1 */
#define FEAT_MTRAP_STURDY	0x41 /* Level 1 */
#define FEAT_MTRAP_SLOWING	0x42 /* Level 6 */
#define FEAT_MTRAP_CONFUSION	0x43 /* Level 12 */
#define FEAT_MTRAP_POISON	0x44 /* Level 18 */
#define FEAT_MTRAP_DRAIN_LIFE	0x45 /* Level 24 */
#define FEAT_MTRAP_ELEC		0x46 /* Level 30 */
#define FEAT_MTRAP_EXPLOSIVE	0x47 /* Level 36 */
#define FEAT_MTRAP_PORTAL	0x48 /* Level 42 */
#define FEAT_MTRAP_DISPEL_M	0x49 /* Level 48 */

/* Misc */
#define FEAT_WEB		0x54	/* Spider's web */

/* New terrain */
/* There are two forms for each element, and also grass/trees */
#define FEAT_WATER		0x60
#define FEAT_ICE		0x61
#define FEAT_FIRE		0x62
#define FEAT_LAVA		0x63
#define FEAT_SAND		0x64
#define FEAT_ROCKS		0x65
#define FEAT_FOG		0x66
#define FEAT_ABYSS		0x67
#define FEAT_GRASS		0x68
#define FEAT_TREE		0x69

/*
 * Bit flags for the "target_set" function
 *
 *	KILL: Target monsters
 *	LOOK: Describe grid fully
 *	XTRA: Currently unused flag
 *	GRID: Select from all grids
 */
#define TARGET_KILL		0x01
#define TARGET_LOOK		0x02
#define TARGET_XTRA		0x04
#define TARGET_GRID		0x08

/*** Cave flags ***/

/*
 * Special cave grid flags
 */
#define CAVE_MARK		0x0001 	/* memorized feature */
#define CAVE_GLOW		0x0002 	/* self-illuminating */
#define CAVE_ICKY		0x0004 	/* part of a vault */
#define CAVE_ROOM		0x0008 	/* part of a room */
#define CAVE_SEEN		0x0010 	/* seen flag */
#define CAVE_VIEW		0x0020 	/* view flag */
#define CAVE_TEMP		0x0040 	/* temp flag */
#define CAVE_WALL		0x0080 	/* wall flag */
#define CAVE_FIRE       	0x0100  /* is in line of fire */

/*** Macros ***/

/*
 * Convert an "attr"/"char" pair into a "pict" (P)
 */
#define PICT(A,C) \
	((((u16b)(A)) << 8) | ((byte)(C)))

/*
 * Convert a "pict" (P) into an "attr" (A)
 */
#define PICT_A(P) \
	((byte)((P) >> 8))

/*
 * Convert a "pict" (P) into an "char" (C)
 */
#define PICT_C(P) \
	((char)((byte)(P)))

/*
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y,X) \
	(256 * (Y) + (X))

/*
 * Convert a "grid" (G) into a "location" (Y)
 */
#define GRID_Y(G) \
	((int)((G) / 256U))

/*
 * Convert a "grid" (G) into a "location" (X)
 */
#define GRID_X(G) \
	((int)((G) % 256U))

/*
 * Determines if a map location is "meaningful"
 */
#define in_bounds(Y,X) \
	(((unsigned)(Y) < (unsigned)(p_ptr->cur_map_hgt)) && \
	 ((unsigned)(X) < (unsigned)(p_ptr->cur_map_wid)))

/*
 * Determines if a map location is fully inside the outer walls
 * This is more than twice as expensive as "in_bounds()", but
 * often we need to exclude the outer walls from calculations.
 */
#define in_bounds_fully(Y,X) \
	(((Y) > 0) && ((Y) < p_ptr->cur_map_hgt-1) && \
	 ((X) > 0) && ((X) < p_ptr->cur_map_wid-1))

/*
 * Determines if a map location is currently "on screen"
 * Note that "panel_contains(Y,X)" always implies "in_bounds(Y,X)".
 * Pre-storing this into a cave_info flag would be nice.  XXX XXX
 */
#define panel_contains(Y,X) \
	(((unsigned)((Y) - p_ptr->wy) < (unsigned)(SCREEN_HGT)) && \
	 ((unsigned)((X) - p_ptr->wx) < (unsigned)(SCREEN_WID)))

/*
 * Determine if a "legal" grid is a "floor" grid
 *
 * Line 1 -- forbid doors, rubble, seams, walls
 *
 * Note the use of the new "CAVE_WALL" flag.
 */
#define cave_floor_bold(Y,X) \
	(!(cave_info[Y][X] & (CAVE_WALL)))

/*
 * Determine if a "legal" grid is a "clean" floor grid
 *
 * Line 1 -- forbid non-floors
 * Line 2 -- forbid normal objects
 */
#define cave_clean_bold(Y,X) \
	((cave_feat[Y][X] == FEAT_FLOOR || cave_feat[Y][X] >= FEAT_WATER) && \
	 (cave_o_idx[Y][X] == 0))

/*
 * Determine if a "legal" grid is an "empty" floor grid
 *
 * Line 1 -- forbid doors, rubble, seams, walls
 * Line 2 -- forbid player/monsters
 */
#define cave_empty_bold(Y,X) \
	(cave_floor_bold(Y,X) && \
	 (cave_m_idx[Y][X] == 0))

/*
 * Determine if a "legal" grid is an "naked" floor grid
 *
 * Line 1 -- forbid non-floors
 * Line 2 -- forbid normal objects
 * Line 3 -- forbid player/monsters
 */
#define cave_naked_bold(Y,X) \
	((cave_feat[Y][X] == FEAT_FLOOR || cave_feat[Y][X] >= FEAT_WATER) && \
	 (cave_o_idx[Y][X] == 0) && \
	 (cave_m_idx[Y][X] == 0))

/*
 * Determine if a "legal" grid is a "shop" grid
 */
#define cave_shop_bold(Y,X) \
	 ((cave_feat[Y][X] >= FEAT_SHOP_HEAD) && \
	  (cave_feat[Y][X] <= FEAT_SHOP_TAIL))

/*
 * Determine if a "legal" grid is a "shop" grid
 */
#define cave_trap_bold(Y,X) \
	 ((cave_feat[Y][X] >= FEAT_TRAP_HEAD) && \
	  (cave_feat[Y][X] <= FEAT_TRAP_TAIL))

/*
 * Determine if a "legal" grid is a "monster_trap" grid
 */
#define cave_mon_trap_bold(Y,X) \
	 ((cave_feat[Y][X] >= FEAT_MTRAP_HEAD) && \
	  (cave_feat[Y][X] <= FEAT_MTRAP_TAIL))

/*
 * Determine if a "legal" grid is a "wall" grid
 */
#define cave_wall_bold(Y,X) \
	 ((cave_feat[Y][X] >= FEAT_WALL_HEAD) && \
	  (cave_feat[Y][X] <= FEAT_WALL_TAIL))

/*
 * Determine if a "legal" grid is an up stairs.
 */
#define cave_up_stairs(Y,X) \
   ((cave_feat[Y][X] == FEAT_LESS_SHAFT) || \
    (cave_feat[Y][X] == FEAT_LESS))

/*
 * Determine if a "legal" grid is a down stairs.
 */
#define cave_down_stairs(Y,X) \
   ((cave_feat[Y][X] == FEAT_MORE_SHAFT) || \
    (cave_feat[Y][X] == FEAT_MORE))

/*
 * Determine if a "legal" grid is a "stair" grid
 */
#define cave_stair_bold(Y,X) \
	 ((cave_feat[Y][X] >= FEAT_STAIR_HEAD) && \
	  (cave_feat[Y][X] <= FEAT_STAIR_TAIL))

/*
 * Determine if a "legal" grid is "permanent"
 *
 * Line 1 -- perma-walls
 * Line 2-3 -- stairs
 * Line 4-5 -- shop doors
 */
#define cave_perma_bold(Y,X) \
	(((cave_feat[Y][X] >= FEAT_PERM_EXTRA) && \
	  (cave_feat[Y][X] <= FEAT_PERM_SOLID)) || \
	  (cave_stair_bold(y,x)) || \
	  (cave_shop_bold(Y,X)))

/*
 * Determine if a "legal" grid is a closed door but not secret.
 * Open or broken doors don't count.
 */
#define cave_known_door(Y,X) \
    ((cave_feat[Y][X] >= FEAT_DOOR_HEAD) && \
     (cave_feat[Y][X] <= FEAT_DOOR_TAIL))

/*
 * Determine if a "legal" grid is a closed door.
 * Open or broken doors don't count.
 */
#define cave_closed_door(Y,X) \
   (((cave_feat[Y][X] >= FEAT_DOOR_HEAD) && \
     (cave_feat[Y][X] <= FEAT_DOOR_TAIL)) || \
     (cave_feat[Y][X] == FEAT_SECRET))

/*
 * Determine if a "legal" grid is within "los" of the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_has_los_bold(Y,X) \
	((cave_info[Y][X] & (CAVE_VIEW)) != 0)

/*
 * Determine if a "legal" grid can be "seen" by the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_can_see_bold(Y,X) \
	((cave_info[Y][X] & (CAVE_SEEN)) != 0)

/*
 * Determine if a "legal" grid is within "line of fire" of the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_can_fire_bold(Y,X) \
	(cave_info[Y][X] & (CAVE_FIRE))

#define MAX_VAULT_REQ		5	/* Max requirements for vault creation */
#define MAX_VAULT_COD		35	/* Max encondings for vault creation */


/**** Types ****/

/*
 * An array of MAX_DUNGEON_WID byte's
 */
typedef byte byte_wid[MAX_DUNGEON_WID];

/*
 * An array of MAX_DUNGEON_WID s16b's
 */
typedef s16b s16b_wid[MAX_DUNGEON_WID];

    
/*
 * Information about terrain "features"
 */
struct feature_type
{
	u32b name;		/* Name (offset) */
	u32b text;		/* Text (offset) */

	byte mimic;		/* Feature to mimic */

	byte d_attr;		/* Default feature attribute */
	char d_char;		/* Default feature character */

	byte x_attr;		/* Desired feature attribute */
	char x_char;		/* Desired feature character */
};

/*
 * "Requirements of vault generation"
 */
struct vault_req_type
{
	byte type;
	s16b info;
};

/*
 * "Encoding of vault generation chars"
 */
struct vault_cod_type
{
	char code;
	s16b race;
	s16b terr;
};

/*
 * Information about "vault generation"
 */
struct vault_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte typ;			/* Vault type */

	byte rat;			/* Vault rating */

	byte hgt;			/* Vault height */
	byte wid;			/* Vault width */
	
	vault_req_type req[MAX_VAULT_REQ];	/* Requirements */
	vault_cod_type cod[MAX_VAULT_COD];	/* Encoding */
};

/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
struct alloc_entry
{
	s16b index;		/* The actual index */

	byte level;		/* Base dungeon level */
	byte prob1;		/* Probability, pass 1 */
	byte prob2;		/* Probability, pass 2 */
	byte prob3;		/* Probability, pass 3 */

	u16b total;		/* Unused for now */
};


/**** Variables ****/

/* Direction tables */
extern const s16b ddd[9];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];

extern int view_n;
extern u16b *view_g;
extern int temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern u16b (*cave_info)[256];
extern byte (*cave_feat)[MAX_DUNGEON_WID];
extern s16b (*cave_m_idx)[MAX_DUNGEON_WID];
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;

extern s16b add_wakeup_chance;
extern s16b total_wakeup_chance;
extern byte (*cave_cost)[MAX_DUNGEON_WID];
extern byte (*cave_when)[MAX_DUNGEON_WID];

extern feature_type *f_info;
extern char *f_name;
extern char *f_text;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;


/**** Functions ****/

/* cave.c */
int distance(int y1, int x1, int y2, int x2);
bool los(int y1, int x1, int y2, int x2);
bool no_lite(void);
bool cave_valid_bold(int y, int x);
bool feat_supports_lighting(int feat);
void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
void map_info_default(int y, int x, byte *ap, char *cp);
void move_cursor_relative(int y, int x);
void print_rel(char c, byte a, int y, int x);
void note_spot(int y, int x);
void lite_spot(int y, int x);
void prt_map(void);
void display_map(int *cy, int *cx);
void do_cmd_view_map(void);
errr vinfo_init(void);
void forget_view(void);
void update_view(void);
void update_noise(bool full);
void update_smell(void);
void map_area(void);
void wiz_lite(void);
void wiz_dark(void);
void town_illuminate(bool daytime);
void cave_set_feat(int y, int x, int feat);
int  project_path(u16b *gp, int range, int y1, int x1, int *y2, int *x2, u32b flg);
byte projectable(int y1, int x1, int y2, int x2, u32b flg);
void scatter(int *yp, int *xp, int y, int x, int d, int m);
void health_track(int m_idx);
void monster_race_track(int r_idx);
void object_kind_track(int k_idx);
void disturb(int stop_search, int unused_flag);

/* game.c */
void play_game(bool new_game);

/* generate.c */
extern bool pet_generate_hack;
void generate_cave(void);

/* target.c */
bool tgt_pt(int *x, int *y);
bool modify_panel(int wy, int wx);
bool adjust_panel(int y, int x);
bool change_panel(int dir);
void verify_panel(void);
int motion_dir(int y1, int x1, int y2, int x2);
int target_dir(char ch);
bool target_able(int m_idx);
bool target_okay(void);
void target_set_monster(int m_idx);
void target_set_location(int y, int x);
bool target_set_interactive(int mode);
bool get_aim_dir(int *dp);
bool get_rep_dir(int *dp);
bool confuse_dir(int *dp);

#endif /* CAVE_H_INCLUDED */
