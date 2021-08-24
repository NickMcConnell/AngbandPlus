/*
 * File: wilderness.h
 * Purpose: Wilderness interface
 */

#ifndef WILDERNESS_H
#define WILDERNESS_H

/*
 * Different types of terrain, used for the wilderness.
 */
#define WILD_UNDEFINED      0
#define WILD_SHORE          1
#define WILD_GRASS          2
#define WILD_WOOD           3
#define WILD_SWAMP          4
#define WILD_WASTE          5
#define WILD_MOUNTAIN       6
#define WILD_VOLCANO        7
#define WILD_CLONE          8
#define WILD_TOWN           9
#define WILD_MUDPIT         10
#define WILD_SCORCHED       11

/* Different buildings */
#define WILD_LOG_CABIN      0
#define WILD_TOWN_HOME      1
#define WILD_ARENA          2

/* Types of crops */
#define WILD_CROP_POTATO    0
#define WILD_CROP_CABBAGE   1
#define WILD_CROP_CARROT    2
#define WILD_CROP_BEET      3
#define WILD_CROP_MUSHROOM  4
#define WILD_CROP_SQUASH    5
#define WILD_CROP_CORN      6

/* Used for wilderness generation */
#define DIR_NORTH   0
#define DIR_EAST    1
#define DIR_SOUTH   2
#define DIR_WEST    3

extern u32b seed_wild;

/* Information about "Arena" (special building for pvp) */
typedef struct
{
    byte x_1;
    byte y_1;
    byte x_2;
    byte y_2;
    s32b depth;
    int player1;
    int player2;
} arena_type;

/*
 * Total number of arenas
 */
#define MAX_ARENAS  10

extern arena_type arenas[MAX_ARENAS];
extern u16b num_arenas;

enum wild_gen
{
    WILD_NONE = 0,  /* Not generated */
    WILD_GENERATED, /* Generated without dwellings */
    WILD_FURNISHED, /* Generated with furnished dwellings */
    WILD_DESERTED   /* Generated with empty dwellings */
};

/*
 * Adding this structure so we can have different creatures generated
 * in different types of wilderness...
 */
typedef struct
{
    int world_x;                    /* The world coordinates (transient) */
    int world_y;
    int type;                       /* What kind of terrain we are in (transient) */
    int monst_lev;                  /* Monster level (transient) */
    enum wild_gen generated;        /* Level is generated */
} wilderness_type;

extern wilderness_type *wild_info;

/*
 * Wilderness macros
 */
#define wild_radius(idx) \
    (abs(wild_info[idx].world_x) + abs(wild_info[idx].world_y))

#define monster_level(DEPTH) \
    ((DEPTH >= 0)? DEPTH: wild_info[DEPTH].monst_lev)

#define object_level(DEPTH) \
    ((DEPTH >= 0)? DEPTH: wild_radius(DEPTH))

#define town_area(DEPTH) \
    ((DEPTH <= 0)? (wild_radius(DEPTH) <= 3): false)

extern int world_index(int world_x, int world_y);
extern void init_wild_info(void);
extern int determine_wilderness_type(int depth);
extern void wild_add_monster(struct player *p, struct chunk *c);
extern void wild_cat_depth(int depth, char *buf, int len);
extern bool wild_is_explored(struct player *p, int idx);
extern void wild_set_explored(struct player *p, int idx);
extern void wild_add_crop(struct chunk *c, int x, int y, int type);
extern void wild_deserted_message(struct player *p);

extern struct chunk *wilderness_gen(struct player *p);

#endif /* WILDERNESS_H */
