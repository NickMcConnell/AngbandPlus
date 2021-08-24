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
    #define TF(a, b) TF_##a,
    #include "list-terrain-flags.h"
    #undef TF
    TF_MAX
};

#define TF_SIZE                 FLAG_SIZE(TF_MAX)

#define tf_has(f, flag) flag_has_dbg(f, TF_SIZE, flag, #f, #flag)

/* Number of basic grids per panel, vertically and horizontally */
#define PANEL_SIZE 11

/* Maximum number of objects on the level (198x66) */
#define MAX_OBJECTS 13068

struct preset
{
    cave_view_type **player_presets[MAX_SEXES];
    cave_view_type player_numbers[8];
    cave_view_type player_bubbles[8];
    struct preset *next;
    int cidx, ridx;
};

/*
 * Information about terrain features.
 *
 * At the moment this isn't very much, but eventually a primitive flag-based
 * information system will be used here.
 */
struct feature
{
    char *name;             /* Name */
    char *shortdesc;
    char *desc;
    unsigned int fidx;          /* Index */
    struct feature *next;
    char *mimic;                /* Name of feature to mimic */
    byte priority;              /* Display priority */
    byte shopnum;               /* Which shop does it take you to? */
    byte dig;                   /* How hard is it to dig through? */
    bitflag flags[TF_SIZE];     /* Terrain flags */
    byte d_attr;                /* Default feature attribute */
    char d_char;                /* Default feature character */
    char *hurt_msg;             /* Message on being hurt by feature */
    char *died_flavor;          /* Flavored message on dying to feature */
    char *die_msg;              /* Message on dying to feature */
    char *confused_msg;         /* Message on confused monster moving into feature */
    char *look_prefix;          /* Prefix for name in look result */
    char *look_in_preposition;  /* Preposition in look result when on the terrain */
    int resist_flag;            /* Monster resist flag for entering feature */
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
};

struct square
{
    u16b feat;
    bitflag *info;
    int light;
    s16b mon;
    struct object *obj;
    struct trap *trap;
};

struct connector
{
    struct loc up;
    struct loc down;
    struct loc rand;
};

struct chunk
{
    struct worldpos wpos;
    u32b obj_rating;
    u32b mon_rating;
    bool good_item;
    int height;
    int width;
    int *feat_count;

    struct square **squares;
    struct loc decoy;

    struct monster *monsters;
    u16b mon_max;
    u16b mon_cnt;
    int num_repro;

    struct monster_group **monster_groups;

    struct connector *join;

    /* PWMAngband */
    bool scan_monsters;
    hturn generated;
    bool *o_gen;

    bool light_level;
    bool gen_hack;
};

/*
 * square_predicate is a function pointer which tests a given square to
 * see if the predicate in question is true.
 */
typedef bool (*square_predicate)(struct chunk *c, struct loc *grid);

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
extern int FEAT_LAVA;

/* MAngband-specific terrain elements */
extern int FEAT_PERM_STATIC;
extern int FEAT_PERM_HOUSE;
extern int FEAT_PERM_ARENA;

extern int FEAT_WATER;
extern int FEAT_MUD;
extern int FEAT_DRAWBRIDGE;
extern int FEAT_FOUNTAIN;
extern int FEAT_FNT_DRIED;
extern int FEAT_LOOSE_DIRT;
extern int FEAT_DIRT;
extern int FEAT_FLOOR_SAFE;
extern int FEAT_LAVA_STREAM;
extern int FEAT_STREET;
extern int FEAT_FLOOR_PIT;

extern int FEAT_GRASS;
extern int FEAT_CROP;
extern int FEAT_TREE;
extern int FEAT_EVIL_TREE;
extern int FEAT_MOUNTAIN;
extern int FEAT_LOGS;
extern int FEAT_SAND;
extern int FEAT_SANDWALL;
extern int FEAT_ICE;
extern int FEAT_ICEWALL;
extern int FEAT_SWAMP;
extern int FEAT_TOWN;
extern int FEAT_DEEP_WATER;
extern int FEAT_HILL;
extern int FEAT_SHORE;

extern int FEAT_PERM_CLEAR;

/* Special "home doors" */
extern int FEAT_HOME_OPEN;
extern int FEAT_HOME_CLOSED;

extern int FEAT_WEB;
extern int FEAT_TRAINING;

extern s16b ddd[9];
extern struct loc ddgrid[10];
extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];
extern struct loc ddgrid_ddd[9];
extern const int *dist_offsets_y[10];
extern const int *dist_offsets_x[10];
extern const byte side_dirs[20][8];

enum
{
    DIR_NONE = 0,
    DIR_SW,
    DIR_S,
    DIR_SE,
    DIR_W,
    DIR_SPOT,
    DIR_E,
    DIR_NW,
    DIR_N,
    DIR_NE
};

/* cave.c */
extern int motion_dir(struct loc *start, struct loc *finish);
extern void next_grid(struct loc *next, struct loc *grid, int dir);
extern int lookup_feat(const char *name);
extern void set_terrain(void);
extern struct chunk *cave_new(int height, int width);
extern void cave_free(struct chunk *c);
extern bool scatter(struct chunk *c, struct loc *place, struct loc *grid, int d, bool need_los);
extern struct monster *cave_monster(struct chunk *c, int idx);
extern int cave_monster_max(struct chunk *c);
extern int cave_monster_count(struct chunk *c);
extern int count_feats(struct player *p, struct chunk *c, struct loc *grid,
    bool (*test)(struct chunk *c, struct loc *grid), bool under);
extern struct loc *cave_find_decoy(struct chunk *c);
extern void update_visuals(struct worldpos *wpos);
extern void note_viewable_changes(struct worldpos *wpos, struct loc *grid);
extern void fully_update_flow(struct worldpos *wpos);
extern void display_fullmap(struct player *p);
extern void update_cursor(struct source *who);
extern void update_health(struct source *who);
extern void (*master_move_hook)(struct player *p, char *args);
extern void master_build(struct player *p, char *parms);
extern void fill_dirt(struct chunk *c, struct loc *grid1, struct loc *grid2);
extern void add_crop(struct chunk *c, struct loc *grid1, struct loc *grid2, int orientation);
extern int add_building(struct chunk *c, struct loc *grid1, struct loc *grid2, int type);
extern void add_moat(struct chunk *c, struct loc *grid1, struct loc *grid2, struct loc drawbridge[3]);

extern struct preset *presets;
extern int presets_count;

/* cave-map.c */
extern void map_info(struct player *p, struct chunk *c, struct loc *grid, struct grid_data *g);
extern void square_note_spot_aux(struct player *p, struct chunk *c, struct loc *grid);
extern void square_note_spot(struct chunk *c, struct loc *grid);
extern void square_light_spot_aux(struct player *p, struct chunk *cv, struct loc *grid);
extern void square_light_spot(struct chunk *c, struct loc *grid);
extern void light_room(struct player *p, struct chunk *c, struct loc *grid, bool light);
extern void wiz_light(struct player *p, struct chunk *c, bool full);
extern void wiz_dark(struct player *p, struct chunk *c, bool full);
extern void cave_illuminate(struct player *p, struct chunk *c, bool daytime);
extern void square_forget_all(struct chunk *c, struct loc *grid);
extern void square_forget_pile_all(struct chunk *c, struct loc *grid);

/* cave-square.c */
extern bool feat_is_magma(int feat);
extern bool feat_is_quartz(int feat);
extern bool feat_is_treasure(int feat);
extern bool feat_is_wall(int feat);
extern bool feat_is_floor(int feat);
extern bool feat_is_trap_holding(int feat);
extern bool feat_is_object_holding(int feat);
extern bool feat_is_monster_walkable(int feat);
extern bool feat_is_shop(int feat);
extern bool feat_is_passable(int feat);
extern bool feat_is_projectable(int feat);
extern bool feat_is_torch(int feat);
extern bool feat_is_bright(int feat);
extern bool feat_is_fiery(int feat);
extern bool feat_is_no_flow(int feat);
extern bool feat_is_no_scent(int feat);
extern bool feat_is_smooth(int feat);
extern bool feat_issafefloor(int feat);
extern bool feat_isterrain(int feat);
extern int feat_order_special(int feat);
extern int feat_pseudo(char d_char);
extern bool feat_ishomedoor(int feat);
extern bool feat_isperm(int feat);
extern bool feat_ismetamap(int feat);
extern bool square_isfloor(struct chunk *c, struct loc *grid);
extern bool square_issafefloor(struct chunk *c, struct loc *grid);
extern bool square_ispitfloor(struct chunk *c, struct loc *grid);
extern bool square_isotherfloor(struct chunk *c, struct loc *grid);
extern bool square_isanyfloor(struct chunk *c, struct loc *grid);
extern bool square_istrappable(struct chunk *c, struct loc *grid);
extern bool square_isobjectholding(struct chunk *c, struct loc *grid);
extern bool square_isrock(struct chunk *c, struct loc *grid);
extern bool square_isperm(struct chunk *c, struct loc *grid);
extern bool square_isunpassable(struct chunk *c, struct loc *grid);
extern bool square_isborder(struct chunk *c, struct loc *grid);
extern bool square_ispermborder(struct chunk *c, struct loc *grid);
extern bool square_ispermarena(struct chunk *c, struct loc *grid);
extern bool square_ispermhouse(struct chunk *c, struct loc *grid);
extern bool square_ispermstatic(struct chunk *c, struct loc *grid);
extern bool square_ispermfake(struct chunk *c, struct loc *grid);
extern bool square_ismagma(struct chunk *c, struct loc *grid);
extern bool square_isquartz(struct chunk *c, struct loc *grid);
extern bool square_ismineral_other(struct chunk *c, struct loc *grid);
extern bool square_ismineral(struct chunk *c, struct loc *grid);
extern bool square_hasgoldvein(struct chunk *c, struct loc *grid);
extern bool square_hasgoldvein_p(struct player *p, struct loc *grid);
extern bool square_isrubble(struct chunk *c, struct loc *grid);
extern bool square_issecretdoor(struct chunk *c, struct loc *grid);
extern bool square_isopendoor(struct chunk *c, struct loc *grid);
extern bool square_home_isopendoor(struct chunk *c, struct loc *grid);
extern bool square_iscloseddoor(struct chunk *c, struct loc *grid);
extern bool square_basic_iscloseddoor(struct chunk *c, struct loc *grid);
extern bool square_home_iscloseddoor(struct chunk *c, struct loc *grid);
extern bool square_isbrokendoor(struct chunk *c, struct loc *grid);
extern bool square_isdoor(struct chunk *c, struct loc *grid);
extern bool square_isdoor_p(struct player *p, struct loc *grid);
extern bool square_isstairs(struct chunk *c, struct loc *grid);
extern bool square_isstairs_p(struct player *p, struct loc *grid);
extern bool square_isupstairs(struct chunk *c, struct loc *grid);
extern bool square_isdownstairs(struct chunk *c, struct loc *grid);
extern bool square_isshop(struct chunk *c, struct loc *grid);
extern bool square_noticeable(struct chunk *c, struct loc *grid);
extern bool square_isplayer(struct chunk *c, struct loc *grid);
extern bool square_isoccupied(struct chunk *c, struct loc *grid);
extern bool square_isknown(struct player *p, struct loc *grid);
extern bool square_isnotknown(struct player *p, struct chunk *c, struct loc *grid);
extern bool square_ismark(struct player *p, struct loc *grid);
extern bool square_istree(struct chunk *c, struct loc *grid);
extern bool square_isstrongtree(struct chunk *c, struct loc *grid);
extern bool square_iswitheredtree(struct chunk *c, struct loc *grid);
extern bool square_isdirt(struct chunk *c, struct loc *grid);
extern bool square_isgrass(struct chunk *c, struct loc *grid);
extern bool square_iscrop(struct chunk *c, struct loc *grid);
extern bool square_iswater(struct chunk *c, struct loc *grid);
extern bool square_islava(struct chunk *c, struct loc *grid);
extern bool square_isnether(struct chunk *c, struct loc *grid);
extern bool square_ismountain(struct chunk *c, struct loc *grid);
extern bool square_isdryfountain(struct chunk *c, struct loc *grid);
extern bool square_isfountain(struct chunk *c, struct loc *grid);
extern bool square_iswebbed(struct chunk *c, struct loc *grid);
extern bool square_isglow(struct chunk *c, struct loc *grid);
extern bool square_isvault(struct chunk *c, struct loc *grid);
extern bool square_notrash(struct chunk *c, struct loc *grid);
extern bool square_isroom(struct chunk *c, struct loc *grid);
extern bool square_isseen(struct player *p, struct loc *grid);
extern bool square_isview(struct player *p, struct loc *grid);
extern bool square_wasseen(struct chunk *c, struct loc *grid);
extern bool square_isdtrap(struct player *p, struct loc *grid);
extern bool square_isfeel(struct chunk *c, struct loc *grid);
extern bool square_ispfeel(struct player *p, struct loc *grid);
extern bool square_istrap(struct chunk *c, struct loc *grid);
extern bool square_iswall_inner(struct chunk *c, struct loc *grid);
extern bool square_iswall_outer(struct chunk *c, struct loc *grid);
extern bool square_iswall_solid(struct chunk *c, struct loc *grid);
extern bool square_ismon_restrict(struct chunk *c, struct loc *grid);
extern bool square_isno_teleport(struct chunk *c, struct loc *grid);
extern bool square_isno_map(struct chunk *c, struct loc *grid);
extern bool square_isno_esp(struct chunk *c, struct loc *grid);
extern bool square_isproject(struct chunk *c, struct loc *grid);
extern bool square_isno_stairs(struct chunk *c, struct loc *grid);
extern bool square_isopen(struct chunk *c, struct loc *grid);
extern bool square_isempty(struct chunk *c, struct loc *grid);
extern bool square_isemptywater(struct chunk *c, struct loc *grid);
extern bool square_istraining(struct chunk *c, struct loc *grid);
extern bool square_isemptyfloor(struct chunk *c, struct loc *grid);
extern bool square_canputitem(struct chunk *c, struct loc *grid);
extern bool square_isdiggable(struct chunk *c, struct loc *grid);
extern bool square_seemsdiggable(struct chunk *c, struct loc *grid);
extern bool square_iswebbable(struct chunk *c, struct loc *grid);
extern bool square_is_monster_walkable(struct chunk *c, struct loc *grid);
extern bool square_ispassable(struct chunk *c, struct loc *grid);
extern bool square_isprojectable(struct chunk *c, struct loc *grid);
extern bool square_iswall(struct chunk *c, struct loc *grid);
extern bool square_isstrongwall(struct chunk *c, struct loc *grid);
extern bool square_isbright(struct chunk *c, struct loc *grid);
extern bool square_isfiery(struct chunk *c, struct loc *grid);
extern bool square_islit(struct chunk *c, struct loc *grid);
extern bool square_isdamaging(struct chunk *c, struct loc *grid);
extern bool square_isnoflow(struct chunk *c, struct loc *grid);
extern bool square_isnoscent(struct chunk *c, struct loc *grid);
extern bool square_iswarded(struct chunk *c, struct loc *grid);
extern bool square_isdecoyed(struct chunk *c, struct loc *grid);
extern bool square_seemslikewall(struct chunk *c, struct loc *grid);
extern bool square_isinteresting(struct chunk *c, struct loc *grid);
extern bool square_islockeddoor(struct chunk *c, struct loc *grid);
extern bool square_isplayertrap(struct chunk *c, struct loc *grid);
extern bool square_isvisibletrap(struct chunk *c, struct loc *grid);
extern bool square_issecrettrap(struct chunk *c, struct loc *grid);
extern bool square_isdisabledtrap(struct chunk *c, struct loc *grid);
extern bool square_isdisarmabletrap(struct chunk *c, struct loc *grid);
extern bool square_dtrap_edge(struct player *p, struct chunk *c, struct loc *grid);
extern bool square_changeable(struct chunk *c, struct loc *grid);
extern bool square_in_bounds(struct chunk *c, struct loc *grid);
extern bool square_in_bounds_fully(struct chunk *c, struct loc *grid);
extern bool square_isbelievedwall(struct player *p, struct chunk *c, struct loc *grid);
extern struct square *square(struct chunk *c, struct loc *grid);
extern struct player_square *square_p(struct player *p, struct loc *grid);
extern struct feature *square_feat(struct chunk *c, struct loc *grid);
extern int square_light(struct chunk *c, struct loc *grid);
extern struct monster *square_monster(struct chunk *c, struct loc *grid);
extern struct object *square_object(struct chunk *c, struct loc *grid);
extern struct trap *square_trap(struct chunk *c, struct loc *grid);
extern bool square_holds_object(struct chunk *c, struct loc *grid, struct object *obj);
extern void square_excise_object(struct chunk *c, struct loc *grid, struct object *obj);
extern void square_excise_pile(struct chunk *c, struct loc *grid);
extern void square_delete_object(struct chunk *c, struct loc *grid, struct object *obj,
    bool do_note, bool do_light);
extern void square_sense_pile(struct player *p, struct chunk *c, struct loc *grid);
extern void square_know_pile(struct player *p, struct chunk *c, struct loc *grid);
extern void square_forget_pile(struct player *p, struct loc *grid);
extern struct object *square_known_pile(struct player *p, struct chunk *c, struct loc *grid);
extern int square_num_walls_adjacent(struct chunk *c, struct loc *grid);
extern void square_set_feat(struct chunk *c, struct loc *grid, int feat);
extern void square_set_mon(struct chunk *c, struct loc *grid, int midx);
extern void square_set_obj(struct chunk *c, struct loc *grid, struct object *obj);
extern void square_set_trap(struct chunk *c, struct loc *grid, struct trap *trap);
extern void square_add_trap(struct chunk *c, struct loc *grid);
extern void square_add_glyph(struct chunk *c, struct loc *grid, int type);
extern void square_add_web(struct chunk *c, struct loc *grid);
extern void square_add_stairs(struct chunk *c, struct loc *grid, int feat_stairs);
extern void square_open_door(struct chunk *c, struct loc *grid);
extern void square_open_homedoor(struct chunk *c, struct loc *grid);
extern void square_close_door(struct chunk *c, struct loc *grid);
extern void square_smash_door(struct chunk *c, struct loc *grid);
extern void square_unlock_door(struct chunk *c, struct loc *grid);
extern void square_set_floor(struct chunk *c, struct loc *grid, int feat);
extern void square_destroy_door(struct chunk *c, struct loc *grid);
extern void square_destroy_trap(struct chunk *c, struct loc *grid);
extern void square_disable_trap(struct player *p, struct chunk *c, struct loc *grid);
extern void square_destroy_decoy(struct player *p, struct chunk *c, struct loc *grid);
extern void square_tunnel_wall(struct chunk *c, struct loc *grid);
extern void square_destroy_wall(struct chunk *c, struct loc *grid);
extern void square_smash_wall(struct chunk *c, struct loc *grid);
extern void square_destroy(struct chunk *c, struct loc *grid);
extern void square_earthquake(struct chunk *c, struct loc *grid);
extern void square_upgrade_mineral(struct chunk *c, struct loc *grid);
extern void square_destroy_rubble(struct chunk *c, struct loc *grid);
extern int square_shopnum(struct chunk *c, struct loc *grid);
extern int square_digging(struct chunk *c, struct loc *grid);
extern int square_apparent_feat(struct player *p, struct chunk *c, struct loc *grid);
extern const char *square_apparent_name(struct player *p, struct chunk *c, struct loc *grid);
extern const char *square_apparent_look_prefix(struct player *p, struct chunk *c, struct loc *grid);
extern const char *square_apparent_look_in_preposition(struct player *p, struct chunk *c,
    struct loc *grid);
extern void square_memorize(struct player *p, struct chunk *c, struct loc *grid);
extern void square_forget(struct player *p, struct loc *grid);
extern void square_mark(struct player *p, struct loc *grid);
extern void square_unmark(struct player *p, struct loc *grid);
extern void square_unglow(struct chunk *c, struct loc *grid);
extern bool square_isnormal(struct chunk *c, struct loc *grid);
extern void square_destroy_tree(struct chunk *c, struct loc *grid);
extern void square_burn_tree(struct chunk *c, struct loc *grid);
extern void square_burn_grass(struct chunk *c, struct loc *grid);
extern void square_colorize_door(struct chunk *c, struct loc *grid, int power);
extern void square_build_permhouse(struct chunk *c, struct loc *grid);
extern void square_dry_fountain(struct chunk *c, struct loc *grid);
extern void square_clear_feat(struct chunk *c, struct loc *grid);
extern void square_add_wall(struct chunk *c, struct loc *grid);
extern void square_add_tree(struct chunk *c, struct loc *grid);
extern void square_add_dirt(struct chunk *c, struct loc *grid);
extern void square_add_grass(struct chunk *c, struct loc *grid);
extern void square_add_safe(struct chunk *c, struct loc *grid);
extern bool square_isplot(struct chunk *c, struct loc *grid);
extern void square_actor(struct chunk *c, struct loc *grid, struct source *who);
extern int square_known_feat(struct player *p, struct chunk *c, struct loc *grid);
extern void square_illuminate(struct player *p, struct chunk *c, struct loc *grid, bool daytime,
    bool light);
extern struct trap *square_top_trap(struct chunk *c, struct loc *grid);
extern void square_memorize_trap(struct player *p, struct chunk *c, struct loc *grid);
extern struct trap *square_known_trap(struct player *p, struct chunk *c, struct loc *grid);
extern void square_forget_trap(struct player *p, struct loc *grid);
extern void square_init_join_up(struct chunk *c);
extern void square_set_join_up(struct chunk *c, struct loc *grid);
extern void square_init_join_down(struct chunk *c);
extern void square_set_join_down(struct chunk *c, struct loc *grid);
extern void square_init_join_rand(struct chunk *c);
extern void square_set_join_rand(struct chunk *c, struct loc *grid);
extern void square_set_upstairs(struct chunk *c, struct loc *grid);
extern void square_set_downstairs(struct chunk *c, struct loc *grid, int feat);
extern void square_set_rubble(struct chunk *c, struct loc *grid, int feat);

/* cave-view.c */
extern int distance(struct loc *grid1, struct loc *grid2);
extern bool los(struct chunk *c, struct loc *grid1, struct loc *grid2);
extern void update_view(struct player *p, struct chunk *c);
extern bool no_light(struct player *p);

#endif /* CAVE_H */
