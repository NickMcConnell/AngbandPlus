#ifndef DUN_GEN_STRUCTURES
#define DUN_GEN_STRUCTURES

#include "src/dun_gen_defines.h"


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
 * A fractal map is a matrix (MAX_FRACTAL_SIZE * MAX_FRACTAL_SIZE) of
 * small numbers. Each number in the map should be replaced with a dungeon
 * feature. This way we can use fractal maps to perform different actions like
 * building rooms, placing pools, etc.
 *
 * We are going to store this matrix in dynamic memory so we have to define
 * a couple of new types:
 */

/*
 * A row of the fractal map
 */
typedef byte fractal_map_wid[MAX_FRACTAL_SIZE];

/*
 * FRACTAL MAP. An array of rows. It can be used as a two-dimensional array.
 */
typedef fractal_map_wid *fractal_map;

/*
 * VERY IMPORTANT: The map must be a square. Its size must be
 * "power of 2 plus 1". Valid values are 3, 5, 9, 17, 33, 65, 129, 257, etc.
 * The maximum supported size is 65.
 *
 * Aditional comments about the construction of the map:
 * Only nine grids of this square are processed at the beginning.
 * These grids are the four corners (who MUST be previously set to meaningful
 * values) and five inner grids.
 * The inner grids are the middle points of each side plus the grid
 * at the center.
 * These nine grids can be viewed as a "3x3" square.
 * Example:
 *
 * a*b
 * ***
 * c*d
 *
 * The algorithm supplies the values for the five inner grids by
 * randomly chosing one *adjacent* corner. Example:
 *
 * aab
 * cbd
 * ccd
 *
 * The next step is to consider this "3x3" square as a part of a larger
 * square:
 *
 * a*a*b
 * *****
 * c*b*d
 * *****
 * c*c*d
 *
 * Now we have four "3x3" squares. The process is repeated for each one of
 * them. The process is stopped when the initial square can't be "magnified"
 * any longer.
 */




typedef struct fractal_template fractal_template;

/* Initialization function for templates */
typedef void (*fractal_init_func)(fractal_map map, fractal_template *t_ptr);

/*
 * A fractal template is used to set the basic shape of the fractal.
 *
 * Templates must provide values for at least the four corners of the map.
 * See the examples.
 */
struct fractal_template
{
    /* The type of the fractal map (one of FRACTAL_TYPE_*) */
    byte type;

    /* The maximum size of the fractal map (3, 5, 9, 17, 33 or 65) */
    int size;

    /* The initialization function for this template */
    fractal_init_func init_func;
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

typedef struct
{
    byte theme;
    u32b flags;		/* A combination of the LF1_* flags */
} flags_themed_levels;

typedef struct
{
    byte theme;
    u16b feature;
} level_feature_themes;

typedef struct
{
    int hgt;
    int wid;
} fractal_dim_struct;

/*
 * A table that holds information of elemental features and the proper
 * transformations that must be triggered if they are present in the dungeon
 */
typedef struct
{
    u32b level_flag;/* The LF1_* flag that must be present in the current level */
    byte type;	/* The transformation type (one of the TRANSFORM_* constants) */
    u16b wall;
    u16b floor;
    u16b chance;
    byte rad;
} elemental_transformation_info;

#endif // DUN_GEN_STRUCTURES

