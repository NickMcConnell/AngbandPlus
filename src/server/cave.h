/*
 * File: cave.h
 * Purpose: Matters relating to the current dungeon level
 */

#ifndef CAVE_H
#define CAVE_H

/*
 * Terrain flags
 */
enum
{
    #define TF(a,b) TF_##a,
    #include "list-terrain-flags.h"
    #undef TF
    TF_MAX
};

#define TF_SIZE                 FLAG_SIZE(TF_MAX)

#define tf_has(f, flag) flag_has_dbg(f, TF_SIZE, flag, #f, #flag)

/* Number of entries in presets.prf */
#define MAX_PRESETS 6

/* Number of class entries in xtra.prf */
#define MAX_XTRA_CLASSES 16

/* Number of race entries in xtra.prf */
#define MAX_XTRA_RACES 15

/* Number of basic grids per panel, vertically and horizontally */
#define PANEL_SIZE 11

/* Maximum number of objects on the level (198x66) */
#define MAX_OBJECTS 13068

/*
 * Information about terrain features.
 *
 * At the moment this isn't very much, but eventually a primitive flag-based
 * information system will be used here.
 */
struct feature
{
    char *name;             /* Name */
    char *desc;
    int fidx;               /* Index */
    struct feature *next;
    byte mimic;             /* Feature to mimic */
    byte priority;          /* Display priority */
    byte shopnum;           /* Which shop does it take you to? */
    byte dig;               /* How hard is it to dig through? */
    bitflag flags[TF_SIZE]; /* Terrain flags */
    byte d_attr;            /* Default feature attribute */
    char d_char;            /* Default feature character */
};

extern struct feature *f_info;

struct grid_data
{
    s16b m_idx;                     /* Monster index */
    int f_idx;                      /* Feature index */
    struct object *first_obj;       /* The first item on the grid */
    struct trap *trap;              /* Trap */
    bool multiple_objects;          /* Multiple objects in the grid */
    bool unseen_object;             /* Is there an unaware object there? */
    bool unseen_money;              /* Is there some unaware money there? */
    enum grid_light_level lighting; /* Light level */
    bool in_view;                   /* Grid can be seen */
    bool is_player;                 /* Grid contains the player */
    bool hallucinate;               /* Hallucinatory grid */
    bool trapborder;                /* Trap detection boundary */
};

struct square
{
    byte feat;
    bitflag *info;
    s16b mon;
    struct object *obj;
    struct trap *trap;
};

struct chunk
{
    s16b depth;
    u32b obj_rating;
    u32b mon_rating;
    bool good_item;
    int height;
    int width;
    int *feat_count;

    struct square **squares;

    struct monster *monsters;
    u16b mon_max;
    u16b mon_cnt;

    /* PWMAngband */
    s16b num_clones;
    bool scan_monsters;
    hturn generated;
    bool *o_gen;

    byte level_up_y;
    byte level_up_x;
    byte level_down_y;
    byte level_down_x;
    byte level_rand_y;
    byte level_rand_x;
};

/*
 * square_predicate is a function pointer which tests a given square to
 * see if the predicate in question is true.
 */
typedef bool (*square_predicate)(struct chunk *c, int y, int x);

/*** Feature Indexes (see "lib/gamedata/terrain.txt") ***/

/* Nothing */
extern int FEAT_NONE;

/* Various */
extern int FEAT_FLOOR;
extern int FEAT_CLOSED;
extern int FEAT_OPEN;
extern int FEAT_BROKEN;
extern int FEAT_LESS;
extern int FEAT_MORE;

/* Secret door */
extern int FEAT_SECRET;

/* Rubble */
extern int FEAT_RUBBLE;
extern int FEAT_PASS_RUBBLE;

/* Mineral seams */
extern int FEAT_MAGMA;
extern int FEAT_QUARTZ;
extern int FEAT_MAGMA_K;
extern int FEAT_QUARTZ_K;

/* Walls */
extern int FEAT_GRANITE;
extern int FEAT_PERM;
extern int FEAT_LAVA_FLOW;

/* Special trap detect features - should be replaced with square flags */
extern int FEAT_DTRAP_FLOOR;
extern int FEAT_DTRAP_WALL;

/* MAngband-specific terrain elements */
extern int FEAT_PERM_STATIC;
extern int FEAT_PERM_HOUSE;
extern int FEAT_PERM_FAKE;
extern int FEAT_PERM_ARENA;

extern int FEAT_WATER;
extern int FEAT_MUD;
extern int FEAT_DRAWBRIDGE;
extern int FEAT_FOUNTAIN;
extern int FEAT_FNT_DRIED;
extern int FEAT_LOOSE_DIRT;
extern int FEAT_DIRT;
extern int FEAT_FLOOR_SAFE;
extern int FEAT_LAVA;
extern int FEAT_STREET;
extern int FEAT_FLOOR_PIT;

extern int FEAT_GRASS;
extern int FEAT_CROP;
extern int FEAT_TREE;
extern int FEAT_EVIL_TREE;
extern int FEAT_MOUNTAIN;
extern int FEAT_LOGS;
extern int FEAT_SWAMP;
extern int FEAT_TOWN;

extern int FEAT_PERM_CLEAR;

/* Special "home doors" */
extern int FEAT_HOME_OPEN;
extern int FEAT_HOME_CLOSED;

extern s16b ddd[9];
extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];

/* cave.c */
extern void set_terrain(void);
extern struct chunk *cave_new(int height, int width);
extern void cave_free(struct chunk *c);
extern void scatter(struct chunk *c, int *yp, int *xp, int y, int x, int d, bool need_los);
extern struct monster *cave_monster(struct chunk *c, int idx);
extern int cave_monster_max(struct chunk *c);
extern int cave_monster_count(struct chunk *c);
extern int count_feats(struct player *p, struct chunk *c, int *y, int *x,
    bool (*test)(struct chunk *c, int y, int x), bool under);
extern void update_visuals(int depth);
extern void note_viewable_changes(int depth, int y, int x, bool forget);
extern void fully_update_flow(int depth);
extern void display_fullmap(struct player *p);
extern void update_cursor(struct actor *who);
extern void update_health(struct actor *who);
extern void (*master_move_hook)(struct player *p, char *args);
extern void master_build(struct player *p, char* parms);
extern void fill_dirt(struct chunk *c, int y1, int x1, int y2, int x2);
extern void add_crop(struct chunk *c, int y1, int x1, int y2, int x2, int orientation);
extern byte add_building(struct chunk *c, int y1, int x1, int y2, int x2, int type);
extern void add_moat(struct chunk *c, int y1, int x1, int y2, int x2, int drawbridge_y[3],
    int drawbridge_x[3]);

extern cave_view_type player_presets[MAX_PRESETS][MAX_XTRA_CLASSES][MAX_XTRA_RACES][MAX_SEXES];
extern cave_view_type player_numbers[MAX_PRESETS][8];

/* cave-map.c */
extern void map_info(struct player *p, struct chunk *c, unsigned y, unsigned x,
    struct grid_data *g);
extern void square_note_spot_aux(struct player *p, struct chunk *c, int y, int x);
extern void square_note_spot(struct chunk *c, int y, int x);
extern void square_light_spot_aux(struct player *p, struct chunk *cv, int y, int x);
extern void square_light_spot(struct chunk *c, int y, int x);
extern void light_room(struct player *p, struct chunk *c, int y1, int x1, bool light);
extern void wiz_light(struct player *p, struct chunk *c, bool full);
extern void wiz_dark(struct player *p);
extern void cave_illuminate(struct player *p, struct chunk *c, bool daytime);
extern void cave_forget_flow(struct player *p, struct chunk *c);
extern void cave_update_flow(struct player *p, struct chunk *c);

/* cave-square.c */
extern bool feat_is_magma(int feat);
extern bool feat_is_quartz(int feat);
extern bool feat_is_treasure(int feat);
extern bool feat_is_wall(int feat);
extern bool feat_is_monster_walkable(int feat);
extern bool feat_is_shop(int feat);
extern bool feat_is_passable(int feat);
extern bool feat_is_projectable(int feat);
extern bool feat_is_bright(int feat);
extern bool feat_issafefloor(int feat);
extern bool feat_isterrain(int feat);
extern bool feat_isprefixed(int feat);
extern int feat_order_special(int feat);
extern int feat_pseudo(char d_char);
extern byte feat_terrain(int type);
extern bool feat_ishomedoor(int feat);
extern bool feat_isperm(int feat);
extern bool square_isfloor(struct chunk *c, int y, int x);
extern bool square_issafefloor(struct chunk *c, int y, int x);
extern bool square_ispitfloor(struct chunk *c, int y, int x);
extern bool square_isotherfloor(struct chunk *c, int y, int x);
extern bool square_isanyfloor(struct chunk *c, int y, int x);
extern bool square_isrock(struct chunk *c, int y, int x);
extern bool square_isperm(struct chunk *c, int y, int x);
extern bool square_isborder(struct chunk *c, int y, int x);
extern bool square_ispermarena(struct chunk *c, int y, int x);
extern bool square_ispermhouse(struct chunk *c, int y, int x);
extern bool square_ispermstatic(struct chunk *c, int y, int x);
extern bool square_ispermfake(struct chunk *c, int y, int x);
extern bool square_ismagma(struct chunk *c, int y, int x);
extern bool square_isquartz(struct chunk *c, int y, int x);
extern bool square_ismineral(struct chunk *c, int y, int x);
extern bool square_hasgoldvein(struct chunk *c, int y, int x);
extern bool square_isrubble(struct chunk *c, int y, int x);
extern bool square_issecretdoor(struct chunk *c, int y, int x);
extern bool square_isopendoor(struct chunk *c, int y, int x);
extern bool square_home_isopendoor(struct chunk *c, int y, int x);
extern bool square_iscloseddoor(struct chunk *c, int y, int x);
extern bool square_basic_iscloseddoor(struct chunk *c, int y, int x);
extern bool square_home_iscloseddoor(struct chunk *c, int y, int x);
extern bool square_isbrokendoor(struct chunk *c, int y, int x);
extern bool square_isdoor(struct chunk *c, int y, int x);
extern bool square_isstairs(struct chunk *c, int y, int x);
extern bool square_isupstairs(struct chunk *c, int y, int x);
extern bool square_isdownstairs(struct chunk *c, int y, int x);
extern bool square_isshop(struct chunk *c, int y, int x);
extern bool square_noticeable(struct chunk *c, int y, int x);
extern bool square_isplayer(struct chunk *c, int y, int x);
extern bool square_isknown(struct player *p, int y, int x);
extern bool square_isnotknown(struct player *p, struct chunk *c, int y, int x);
extern bool square_ismark(struct player *p, int y, int x);
extern bool square_istree(struct chunk *c, int y, int x);
extern bool square_isstrongtree(struct chunk *c, int y, int x);
extern bool square_iswitheredtree(struct chunk *c, int y, int x);
extern bool square_isdirt(struct chunk *c, int y, int x);
extern bool square_isgrass(struct chunk *c, int y, int x);
extern bool square_iscrop(struct chunk *c, int y, int x);
extern bool square_iswater(struct chunk *c, int y, int x);
extern bool square_islava(struct chunk *c, int y, int x);
extern bool square_ismountain(struct chunk *c, int y, int x);
extern bool square_isdryfountain(struct chunk *c, int y, int x);
extern bool square_isfountain(struct chunk *c, int y, int x);
extern bool square_isglow(struct chunk *c, int y, int x);
extern bool square_isvault(struct chunk *c, int y, int x);
extern bool square_isroom(struct chunk *c, int y, int x);
extern bool square_isseen(struct player *p, int y, int x);
extern bool square_isview(struct player *p, int y, int x);
extern bool square_wasseen(struct chunk *c, int y, int x);
extern bool square_isdtrap(struct player *p, int y, int x);
extern bool square_isfeel(struct chunk *c, int y, int x);
extern bool square_ispfeel(struct player *p, int y, int x);
extern bool square_isdedge(struct player *p, int y, int x);
extern bool square_istrap(struct chunk *c, int y, int x);
extern bool square_iswall_inner(struct chunk *c, int y, int x);
extern bool square_iswall_outer(struct chunk *c, int y, int x);
extern bool square_iswall_solid(struct chunk *c, int y, int x);
extern bool square_ismon_restrict(struct chunk *c, int y, int x);
extern bool square_isno_teleport(struct chunk *c, int y, int x);
extern bool square_isno_map(struct chunk *c, int y, int x);
extern bool square_isno_esp(struct chunk *c, int y, int x);
extern bool square_isproject(struct chunk *c, int y, int x);
extern bool square_isopen(struct chunk *c, int y, int x);
extern bool square_isempty(struct chunk *c, int y, int x);
extern bool square_isemptyfloor(struct chunk *c, int y, int x);
extern bool square_canputitem(struct chunk *c, int y, int x);
extern bool square_isdiggable(struct chunk *c, int y, int x);
extern bool square_seemsdiggable(struct chunk *c, int y, int x);
extern bool square_is_monster_walkable(struct chunk *c, int y, int x);
extern bool square_ispassable(struct chunk *c, int y, int x);
extern bool square_isprojectable(struct chunk *c, int y, int x);
extern bool square_iswall(struct chunk *c, int y, int x);
extern bool square_isstrongwall(struct chunk *c, int y, int x);
extern bool square_isbright(struct chunk *c, int y, int x);
extern bool square_iswarded(struct chunk *c, int y, int x);
extern bool square_canward(struct chunk *c, int y, int x);
extern bool square_seemslikewall(struct chunk *c, int y, int x);
extern bool square_isinteresting(struct chunk *c, int y, int x);
extern bool square_islockeddoor(struct chunk *c, int y, int x);
extern bool square_isplayertrap(struct chunk *c, int y, int x);
extern bool square_isvisibletrap(struct chunk *c, int y, int x);
extern bool square_issecrettrap(struct chunk *c, int y, int x);
extern bool square_isknowntrap(struct chunk *c, int y, int x);
extern bool square_changeable(struct chunk *c, int y, int x);
extern bool square_dtrap_edge(struct player *p, struct chunk *c, int y, int x);
extern bool square_in_bounds(struct chunk *c, int y, int x);
extern bool square_in_bounds_fully(struct chunk *c, int y, int x);
extern struct feature *square_feat(struct chunk *c, int y, int x);
extern struct monster *square_monster(struct chunk *c, int y, int x);
extern struct object *square_object(struct chunk *c, int y, int x);
extern struct trap *square_trap(struct chunk *c, int y, int x);
extern bool square_holds_object(struct chunk *c, int y, int x, struct object *obj);
extern void square_excise_object(struct chunk *c, int y, int x, struct object *obj);
extern void square_excise_pile(struct chunk *c, int y, int x);
extern void square_set_feat(struct chunk *c, int y, int x, int feat);
extern void square_add_trap(struct chunk *c, int y, int x);
extern void square_add_ward(struct chunk *c, int y, int x);
extern void square_add_stairs(struct chunk *c, int y, int x, int depth);
extern void square_open_door(struct chunk *c, int y, int x);
extern void square_open_homedoor(struct chunk *c, int y, int x);
extern void square_close_door(struct chunk *c, int y, int x);
extern void square_smash_door(struct chunk *c, int y, int x);
extern void square_unlock_door(struct chunk *c, int y, int x);
extern void square_destroy_door(struct chunk *c, int y, int x);
extern void square_destroy_trap(struct player *p, struct chunk *c, int y, int x);
extern void square_tunnel_wall(struct chunk *c, int y, int x);
extern void square_destroy_wall(struct chunk *c, int y, int x);
extern void square_destroy(struct chunk *c, int y, int x);
extern void square_earthquake(struct chunk *c, int y, int x);
extern void square_remove_ward(struct chunk *c, int y, int x);
extern void square_upgrade_mineral(struct chunk *c, int y, int x);
extern void square_destroy_rubble(struct chunk *c, int y, int x);
extern int square_shopnum(struct chunk *c, int y, int x);
extern int square_digging(struct chunk *c, int y, int x);
extern int square_apparent_feat(struct player *p, struct chunk *c, int y, int x);
extern const char *square_apparent_name(struct player *p, struct chunk *c, int y, int x);
extern void square_memorize(struct player *p, struct chunk *c, int y, int x);
extern void square_forget(struct player *p, int y, int x);
extern void square_mark(struct player *p, int y, int x);
extern void square_unmark(struct player *p, int y, int x);
extern void square_unglow(struct chunk *c, int y, int x);
extern bool square_isnormal(struct chunk *c, int y, int x);
extern void square_destroy_tree(struct chunk *c, int y, int x);
extern void square_burn_tree(struct chunk *c, int y, int x);
extern void square_burn_grass(struct chunk *c, int y, int x);
extern void square_colorize_door(struct chunk *c, int y, int x, int power);
extern void square_build_permhouse(struct chunk *c, int y, int x);
extern void square_dry_fountain(struct chunk *c, int y, int x);
extern void square_clear_feat(struct chunk *c, int y, int x);
extern void square_add_wall(struct chunk *c, int y, int x);
extern void square_add_tree(struct chunk *c, int y, int x);
extern void square_add_dirt(struct chunk *c, int y, int x);
extern void square_add_grass(struct chunk *c, int y, int x);
extern void square_add_safe(struct chunk *c, int y, int x);
extern bool square_isplot(struct chunk *c, int y, int x);
extern void square_actor(struct chunk *c, int y, int x, struct actor *who);
extern int square_known_feat(struct player *p, struct chunk *c, int y, int x);
extern void square_illuminate(struct player *p, struct chunk *c, int y, int x, bool daytime);
extern struct trap *square_top_trap(struct chunk *c, int y, int x);
extern void square_memorize_trap(struct player *p, struct chunk *c, int y, int x);
extern struct trap *square_known_trap(struct player *p, struct chunk *c, int y, int x);
extern void square_forget_trap(struct player *p, int y, int x);

/* cave-view.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(struct chunk *c, int y1, int x1, int y2, int x2);
extern void forget_view(struct player *p, struct chunk *c);
extern void update_view(struct player *p, struct chunk *c);
extern bool no_light(struct player *p);

#endif /* CAVE_H */
