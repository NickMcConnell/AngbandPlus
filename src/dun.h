#ifndef INCLUDED_DUN_H
#define INCLUDED_DUN_H

#include "point_map.h"
#include "dun_tim.h"

extern bool dun_feat_init(void); /* XXX init_angband */

/************************************************************************
 * Grid
 ************************************************************************/
typedef void (*dun_grid_f)(point_t pos, dun_grid_ptr grid);
typedef bool (*dun_grid_p)(point_t pos, dun_grid_ptr grid);
typedef int  (*dun_grid_weight_f)(point_t pos, dun_grid_ptr grid);

/************************************************************************
 * GridEx
 ************************************************************************/
typedef struct dun_grid_ex_s dun_grid_ex_t, *dun_grid_ex_ptr;
struct dun_grid_ex_s
{
    dun_grid_ptr grid;
    mon_ptr      mon;
    obj_ptr      obj;
    bool         plr;
    point_t      pos;
};

/************************************************************************
 * Dungeon Type (Angband, Icky Caves, Isengard, etc)
 ************************************************************************/
enum { D_NONE = 0,
       D_WORLD,
       D_SURFACE, 
       D_QUEST,
       D_AMBER, 

       /* The Desolation of Smaug */
       D_STRONGHOLD,
       D_ORC_CAVE,
       D_FOREST,
       D_CAMELOT,
       D_ICKY_CAVE,
       D_LONELY_MOUNTAIN,

       /* The Betrayal of Saruman */
       D_MORIA,
       D_CASTLE,
       D_MOUNTAIN,
       D_ISENGARD,

       /* The Dark Tower */
       D_DARK_TOWER,
       D_MINAS_MORGUL,
       D_MOUNT_DOOM,
       D_ANGBAND,

       /* For Random Wilderness */
       D_OLYMPUS,
       D_DRAGONS_LAIR,
       D_RANDOM_FOREST,
       D_RANDOM_MOUNTAIN,
       D_RANDOM_VOLCANO,
       D_RANDOM_SEA,
       D_WIZARDS_TOWER,
       D_MONASTERY,
       D_GRAVEYARD,
       D_SANCTUARY, /* of the Knight's Templar */
       D_PANDEMONIUM, /* Demons and Mephistopheles */
       D_NUMENOR,
       D_RLYEH,
       D_DARK_CASTLE, /* Vampires and Thuringwethil|Vlad */
       D_DARK_CAVE, /* Dark Elves and Malekith */
};

typedef void (*dun_place_f)(dun_ptr dun, point_t pos);
struct dun_type_flags_s
{
    u32b info; /* DF_* flags for general info and restrictions; copied|shared with dun_t.flags */
    u32b gen;  /* DF_GEN_* flags are used during level generation */
    u16b plr;  /* DF_PLR_* flags track plr actions and are saved */
};
typedef struct dun_type_flags_s dun_type_flags_t;

struct dun_type_s
{
    u16b    id;
    cptr    name;
    cptr    desc;
    rect_t  (*size_f)(dun_type_ptr me);
    void    (*enter_f)(dun_type_ptr me);
    void    (*init_f)(dun_type_ptr me);
    void    (*pre_gen_f)(dun_type_ptr me, dun_gen_ptr gen);
    void    (*post_gen_f)(dun_type_ptr me, dun_gen_ptr gen);
    void    (*change_dun_f)(dun_type_ptr me, dun_ptr dun);
    void    (*kill_mon_f)(dun_type_ptr me, mon_ptr mon);
    int     (*mon_alloc_f)(dun_type_ptr me, mon_race_ptr race, int prob);
    void    (*exit_f)(dun_type_ptr me);
    void    (*save_f)(dun_type_ptr me, savefile_ptr file);
    void    (*load_f)(dun_type_ptr me, savefile_ptr file);

    dun_type_flags_t flags;

    s16b    min_dun_lvl;
    s16b    max_dun_lvl;
    sym_t   final_guardian;

    void    (*place_outer_wall)(dun_ptr dun, point_t pos);
    void    (*place_inner_wall)(dun_ptr dun, point_t pos);
    void    (*place_wall)(dun_ptr dun, point_t pos);
    void    (*place_floor)(dun_ptr dun, point_t pos);
    void    (*place_stream1)(dun_ptr dun, point_t pos);
    void    (*place_stream2)(dun_ptr dun, point_t pos);

    vec_ptr mon_alloc_tbl;
    vec_ptr obj_alloc_tbl;

    point_t world_pos;   /* grid in D_WORLD for dungeon entrance */
    s16b    plr_max_lvl; /* plr recall depth */
    byte    last_recall; /* counter to force FF_RECALL rooms */
    byte    last_shop;   /* counter to force ROOM_SHOP rooms */
};
/* info and restrictions (dun_type->flags.info) */
#define DF_RANDOM           0x00000001  /* wilderness encounters only */
#define DF_KNOWN            0x00000002   /* player always knows this dungeon location on world map */
#define DF_NO_STATS         0x00000004   /* skip during statistics runs (wizard mode) */

/* note the following are shared with dun->flags and are copied over during generation,
 * so they cannot overlap with DF_* defined below. The reason for this is that "special levels"
 * might want restrictions normally not enforced by dun->type. cf, DF_GC et. al. below */
#define DF_NO_MAGIC         0x00010000
#define DF_NO_MELEE         0x00020000
#define DF_NO_GENOCIDE      0x00040000
#define DF_NO_DESTRUCT      0x00080000
#define DF_NO_QUAKE         0x00100000
#define DF_NO_SUMMON        0x00200000   /* XXX not implemented yet */
#define DF_NO_TELEPORT      0x00400000   /* XXX not implemented yet */
#define DF_NO_REGEN         0x00800000   /* XXX not implemented yet */
#define DF_NO_LIGHT         0x01000000
#define DF_RESTRICT_MASK    0xFFFF0000

/* Generation Flags (dun_type->flags.gen) */
#define DF_GEN_RIVER_WATER      0x00000001
#define DF_GEN_RIVER_LAVA       0x00000002
#define DF_GEN_LAKE_WATER       0x00000004
#define DF_GEN_LAKE_LAVA        0x00000008
#define DF_GEN_LAKE_RUBBLE      0x00000010
#define DF_GEN_LAKE_TREE        0x00000020
#define DF_GEN_NO_DOORS         0x00000040
#define DF_GEN_CURTAIN          0x00000080
#define DF_GEN_CAVE             0x00000100
#define DF_GEN_CAVERN           0x00000200
#define DF_GEN_ARENA            0x00000400
#define DF_GEN_MAZE             0x00000800
#define DF_GEN_NO_VAULT         0x00001000
#define DF_GEN_DESTROY          0x00002000
#define DF_GEN_NO_CAVE          0x00004000
#define DF_GEN_TOWER            0x00008000

#define DF_GEN_LAKE_MASK (DF_GEN_LAKE_WATER | DF_GEN_LAKE_LAVA | DF_GEN_LAKE_RUBBLE | DF_GEN_LAKE_TREE)

/* player flags (dun_type->flags.plr) */
#define DF_PLR_ENTERED         0x0001 /* plr has entered this dungeon */
#define DF_PLR_COMPLETED       0x0002 /* plr has conquered this dungeon (completely in case of multiple guardians) */
#define DF_PLR_FAILED          0x0004 /* plr has failed this dungeon and may not return */
#define DF_PLR_SECRET          0x0008 /* plr needs to do something to reveal this dungeon */

extern int          dun_types_parse(cptr name);
extern dun_type_ptr dun_types_lookup(int id);
extern dun_type_ptr dun_types_choose(cptr prompt, bool wizard);
extern dun_type_ptr dun_types_random(int level);
extern void         dun_types_reset_world(void);
extern void         dun_types_save(savefile_ptr file);
extern void         dun_types_load(savefile_ptr file);
extern vec_ptr      plr_dun_types(void);
extern int          plr_max_dun_lvl(void);
extern vec_ptr      world_dun_types(void);

/************************************************************************
 * Stairs
 ************************************************************************/
typedef struct dun_stairs_s dun_stairs_t, *dun_stairs_ptr;
struct dun_stairs_s
{
    dun_stairs_ptr next;
    point_t pos_here;    /* location on this level */
    point_t pos_there;   /* location of return stairs on that level, once generated */
    s16b    dun_id;      /* 0 means not yet generated. always check for a gc'd level on non-zero */
    s16b    dun_type_id; /* same as dun_t.dun_type_id unless on surface */
    s16b    dun_lvl;     /* probably dun_t.dun_lvl + 1 or 2 unless I do something evil */
    dun_flow_ptr flow;   /* cf _flow_stairs in mon_ai.c */
};

/************************************************************************
 * Page
 *
 * Every dungeon level maintains a list of pages for grid storage. Most
 * levels inside of a dungeon will only have a single page for the entire
 * level, where page->rect is the same as dun->rect. The wilderness
 * surface will maintain pages for adjacent D_WORLD tiles, scrolling
 * as the player moves about.
 * Note that pages are maintained in MRU order by dun_grid_at. So, any
 * traversal of the page list must first DF_PAGELOCK the dun_t to prevent
 * reordering during iteration. In general, clients should *not* be doing
 * this (cf dun_iter_grids, _gen_surface, dun_world_move_plr).
 ************************************************************************/
typedef struct dun_page_s dun_page_t, *dun_page_ptr;
struct dun_page_s
{
    rect_t       rect;
    u32b         flags;
    dun_grid_ptr grids;
    dun_page_ptr next;   /* private! */
    dun_page_ptr prev;   /* private! */
};
#define PF_MARK 0x80000000

extern dun_page_ptr dun_page_alloc(rect_t rect); /* for D_SURFACE */

/************************************************************************
 * Dungeon Level
 *
 * The dungeon level (dun_t) manages terrain (grids), objects and monsters
 * for a single game level. Levels are connected via stairs and the dun_mgr
 * handles generation, garbage collection and processing as the game
 * progresses. There are no external files created for floors while playing.
 * Instead, old levels will be purged when they become sufficiently distant
 * from the player's current level. New levels will be generated on the
 * fly whenever the player approaches close to the stairs leading to that
 * level. Monsters will be processed on close-by levels, so beware!
 *
 * Every level occupies a rectangular region for position-based access.
 * Do not assume that this region begins at (0, 0) ... On D_SURFACE, it
 * will not.
 *
 * Memory management is forgiving. When you delete a monster, memory is
 * moved to the "graveyard" so continued pointer access will not crash
 * (but is still icky). Objects similarly move to the "junkpile". Both
 * are cleared at the start of the next game turn.
 ************************************************************************/
struct dun_s
{
    u16b           id;         /* unique id (opaque) */
    dun_type_ptr   type;
    s16b           dun_lvl;
    s16b           difficulty; /* monster and object generation; usually = loc.lvl */
    s16b           quest_id;   /* D_QUEST */
    u32b           turn;
    rect_t         rect;       /* rect of level ... pages may chunk this rect */
    dun_page_ptr   page;       /* list of grid chunks */
    int_map_ptr    mon;        /* all monsters in this dun_t ... */
    vec_ptr        graveyard;  /* ... except the recently slain */
    int_map_ptr    obj;        /* all objects in this dun_t ... */
    vec_ptr        junkpile;   /* ... except the recently discarded */
    point_map_ptr  mon_pos;    /* all monsters by position: dun_move_mon */
    point_map_ptr  obj_pos;    /* all floor objects by position (omits mon carried objects) */
    dun_stairs_ptr stairs;     /* not very many stairs per level (3-5 or so) */
    vec_ptr        mon_alloc_tbl; /* optional vec<mon_race_ptr> */
    vec_ptr        obj_alloc_tbl; /* optional vec<obj_kind_ptr> */
    point_t        flow_pos;
    dun_flow_ptr   flow;
    u32b           flags;
    s16b           breed_ct, breed_kill_ct;
    byte           feeling;
    s16b           feeling_delay;
    s16b           ambient_light;
    s16b           plr_dis; /* distance from plr to connecting stairs if plr not on level; 0 otherwise */
    town_ptr       town;    /* DF_SHOP */
    dun_tim_ptr    timers;
    point_map_ptr  flows;   /* map<pos, dun_flow_ptr> */
};
#define DF_GENERATED        0x0001
#define DF_LOCKED           0x0002
#define DF_UPDATE_FLOW      0x0004
#define DF_MARK             0x0008
#define DF_VISITED          0x0010
#define DF_RECALL           0x0020  /* level has a ROOM_RECALL room; update stuff when "visited" */
#define DF_SHOP             0x0040  /* level has a ROOM_SHOP room; update stuff when "visited" */
#define DF_AUTOSAVE         0x0080
#define DF_TRAVEL           0x0100  /* level has ROOM_TRAVEL (cf. _pre_gen_rooms) */
#define DF_PAGELOCK         0x0200
#define DF_GC               0x0400
/* XXX 0xFFFF0000 is reserved for "restrictions" copied from dun->type->flags.info
 * cf dun_gen_aux. In the future, special levels may impose restrictions not normally
 * enforced by dun->type->flags.info. Client code checks dun->flags for restrictions,
 * not dun->type->flags.info in order to handle both cases XXX */

/* Allocating, Freeing and Generating new dungeon levels. These are generally
 * for the dun_mgr only. */
extern dun_ptr      dun_alloc_aux(int id);
extern dun_ptr      dun_alloc(int id, rect_t rect);
extern void         dun_free_page(dun_ptr dun, dun_page_ptr page); /* for D_SURFACE */
extern void         dun_clear(dun_ptr dun);
extern void         dun_free(dun_ptr dun);

/* Dungeon Generation: All managed by dun_mgr at the appropriate time.
 * Generally, a level is generated in response to stair proximity during
 * dun_mgr_process. The initial surface generation occurs during plr_birth.
 * Finally, some wizard jump commands allow the player to jump to a
 * specific level in a specific dungeon (cf nexus effects and trump tower) */
extern dun_ptr      dun_gen_connected(dun_ptr dun, dun_stairs_ptr stairs);
extern point_t      dun_world_pos(point_t surface_pos);
extern dun_ptr      dun_gen_surface(point_t world_pos);
extern dun_ptr      dun_gen_quest(quest_ptr quest);
extern void         dun_regen_town(dun_ptr dun); /* reinit_wilderness for quests */
extern void         dun_regen_surface(dun_ptr dun); /* testing wilderness fractals */
extern dun_ptr      dun_gen_wizard(int dun_type_id, int dun_lvl);
extern point_t      dun_random_plr_pos(dun_ptr dun);
extern point_t      dun_random_mon_pos(dun_ptr dun, mon_race_ptr race);

/* dun->rect gives the region of this level. The boundary points
 * are permanent walls and are not legal grids for normal gameplay.
 * The interior points are safe to use. For example, you may safely
 * iterate adjacent grids on an interior point, though some of these
 * might be boundary points. On the surface, dun->rect moves with the
 * player, keeping plr->pos well in the interior region. Boundary
 * grids are not permanent in this case, but are out of bounds for 
 * monsters, objects and players. */
extern bool          dun_pos_valid(dun_ptr dun, point_t pos);
extern bool          dun_pos_boundary(dun_ptr dun, point_t pos);
extern bool          dun_pos_interior(dun_ptr dun, point_t pos);
extern rect_t        dun_clip_rect(dun_ptr dun, rect_t rect); /* interior */

/* Grid  Access */
extern dun_cell_ptr  dun_cell_at(dun_ptr dun, point_t pos);
extern dun_grid_ptr  dun_grid_at(dun_ptr dun, point_t pos);
extern dun_grid_ex_t dun_grid_ex_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_drop_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_mon_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_plr_at(dun_ptr dun, point_t pos);
extern bool          dun_stop_disintegration_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_los_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_project_at(dun_ptr dun, point_t pos);

extern bool          dun_clean_at(dun_ptr dun, point_t pos);
extern bool          dun_naked_at(dun_ptr dun, point_t pos);

extern bool          dun_allow_trap_at(dun_ptr dun, point_t pos);

extern point_t       dun_scatter(dun_ptr dun, point_t pos, int spread);
extern point_t       dun_scatter_aux(dun_ptr dun, point_t pos, int spread, bool (*filter)(dun_ptr dun, point_t pos));

/* Iterate grids in a dungeon, performing an action for each grid. Make sure you
 * understand the distinction between all grids, interior grids and boundary grids
 * for a given rectangular region. */
extern void          dun_iter_grids(dun_ptr dun, dun_grid_f f);
extern void          dun_iter_boundary(dun_ptr dun, dun_grid_f f);
extern void          dun_iter_interior(dun_ptr dun, dun_grid_f f);
extern void          dun_iter_adjacent(dun_ptr dun, point_t pos, dun_grid_f f); /* interior only */
extern void          dun_iter_rect(dun_ptr dun, rect_t rect, dun_grid_f f);     /* rect is clipped to interior of this level */
extern void          dun_iter_rect_interior(dun_ptr dun, rect_t rect, dun_grid_f f); /* rect is asserted inside interior of this level */
extern void          dun_iter_rect_boundary(dun_ptr dun, rect_t rect, dun_grid_f f); /* rect is asserted inside interior of this level */

/* Filter the entire dungeon for interesting positions. The filter takes a position argument
 * to allow for more interesting queries that include object and/or monster info. Looking for
 * all the grids for player placement, as an example, requires grids w/o monsters in addition
 * to normal terrain restrictions. */
extern point_vec_ptr dun_filter_grids(dun_ptr dun, dun_grid_p p);
extern point_t       dun_find_grid(dun_ptr dun, dun_grid_p p);

/* Randomly choose an interior position based upon a weight function. Check the result against
 * dun_pos_interior to make sure the selection worked (Expensive) */
extern point_t       dun_random_grid(dun_ptr dun, dun_grid_weight_f weight);
extern point_t       dun_random_grid_in_rect(dun_ptr dun, rect_t rect, dun_grid_weight_f weight);
extern dun_stairs_ptr dun_random_stairs(dun_ptr dun);

/* Monster Management */
extern mon_ptr       dun_alloc_mon(dun_ptr dun);
extern mon_ptr       dun_mon(dun_ptr dun, u32b id);
extern mon_ptr       dun_mon_ex(dun_ptr dun, u32b id); /* recursive search (depth first) */
extern vec_ptr       dun_adjacent_mon(dun_ptr dun, point_t pos);
extern vec_ptr       dun_mon_filter(dun_ptr dun, mon_p p);
extern mon_ptr       dun_mon_at(dun_ptr dun, point_t pos);
extern void          dun_attach_mon(dun_ptr dun, mon_ptr mon);
extern void          dun_place_mon(dun_ptr dun, mon_ptr mon, point_t pos);
extern void          dun_move_mon(dun_ptr dun, mon_ptr mon, point_t pos);
extern mon_ptr       dun_detach_mon(dun_ptr dun, u32b id);
extern void          dun_delete_mon(dun_ptr dun, u32b id);
extern void          dun_iter_mon(dun_ptr dun, void (*f)(int id, mon_ptr mon));
extern vec_ptr       dun_filter_mon(dun_ptr dun, mon_p p);

/* Object Management (cf dun_obj.c) */
extern obj_ptr       dun_obj(dun_ptr dun, int id);
extern obj_ptr       dun_obj_at(dun_ptr dun, point_t pos); /* might be a pile ... use obj_t.next */
extern vec_ptr       dun_pile_at(dun_ptr dun, point_t pos); /* cf project_o */
extern void          dun_place_obj(dun_ptr dun, obj_ptr obj, point_t pos);
extern void          dun_drop_obj(dun_ptr dun, obj_ptr obj, point_t pos);
extern bool          dun_drop_near(dun_ptr dun, obj_ptr obj, point_t pos);
extern bool          dun_drop_break_near(dun_ptr dun, obj_ptr obj, point_t pos, int break_pct);
extern void          dun_mon_drop_carried_obj(dun_ptr dun, mon_ptr mon);
extern bool          dun_mon_pickup(dun_ptr dun, mon_ptr mon);
extern bool          dun_mon_destroy(dun_ptr dun, mon_ptr mon);
extern void          dun_mon_steal_plr(dun_ptr dun, mon_ptr mon, obj_ptr obj); /* from player, so obj copied into dun */
extern bool          dun_fetch_obj(dun_ptr dun, point_t from, point_t to);
extern obj_ptr       dun_detach_obj(dun_ptr dun, int id);
extern void          dun_delete_obj(dun_ptr dun, int id);
extern void          dun_destroy_obj_at(dun_ptr dun, point_t pos);  /* destruction, earthquake, etc */
extern bool          dun_can_destroy_obj_at(dun_ptr dun, point_t pos);
extern bool          dun_can_destroy(dun_ptr dun, point_t pos);
extern void          dun_iter_obj(dun_ptr dun, void (*f)(int id, obj_ptr obj));
extern void          dun_iter_floor_obj(dun_ptr dun, void (*f)(point_t pos, obj_ptr pile));
extern vec_ptr       dun_filter_obj(dun_ptr dun, obj_p p); /* floor objects only */
extern bool          dun_obj_integrity(dun_ptr dun);
extern void          dun_obj_panic(dun_ptr dun); /* recover a corrupted savefile */

/* Player Management */
extern void          dun_move_plr(dun_ptr dun, point_t pos);
extern void          dun_world_move_plr(dun_ptr dun, point_t pos);
extern bool          dun_plr_at(dun_ptr dun, point_t pos);
extern void          dun_update_flow(dun_ptr dun);
extern void          dun_update_mon_flow(dun_ptr dun); /* PU_MON_FLOW from, e.g., Stone to Mud */
extern dun_flow_ptr  dun_find_flow_at(dun_ptr dun, point_t pos); /* create a flow at pos */
extern void          dun_forget_flow_at(dun_ptr dun, point_t pos);

/* Stairs and Changing Levels (cf dun_mgr_recall_plr for Word of Recall) */
extern void          dun_quest_stairs(dun_ptr dun, point_t pos, int lvl); /* "A magical staircase appears ..." */
extern void          dun_quest_travel(dun_ptr dun, point_t pos);
extern void          dun_add_stairs(dun_ptr dun, dun_stairs_ptr info);
extern bool          dun_take_stairs_plr(dun_ptr dun);
extern bool          dun_take_stairs_mon(dun_ptr dun, mon_ptr mon);
extern void          dun_trap_door_plr(dun_ptr dun);
extern void          dun_trap_door_mon(dun_ptr dun, mon_ptr mon);
extern void          dun_teleport_level_plr(dun_ptr dun);
extern void          dun_teleport_level_mon(dun_ptr dun, mon_ptr mon);
extern bool          dun_create_stairs(dun_ptr dun, bool down_only);

/* Game Processing */
extern void          dun_process(dun_ptr dun);

extern void          dun_forget_flow(dun_ptr dun);
extern void          dun_note_pos(dun_ptr dun, point_t pos);
extern void          dun_draw_pos(dun_ptr dun, point_t pos);

/* Savefile Support */
extern void          dun_load(dun_ptr dun, savefile_ptr file);
extern void          dun_save(dun_ptr dun, savefile_ptr file);

/* Debugging */
extern void          dun_dump(dun_ptr dun, cptr file, int format);

/************************************************************************
 * Flow (dun_util.c)
 ************************************************************************/
#define DUN_FLOW_MAX  250
#define DUN_FLOW_NULL 255
struct dun_flow_s
{
    dun_ptr       dun;
    point_t       pos;
/*  private: */
    int           radius;
    int           flow_max;
    dun_grid_p    filter;
    rect_t        rect;
    byte         *map;    
    bool          lazy;  /* only calc if needed (only about 10% in one test run!) */
};


extern dun_flow_ptr dun_flow_calc(dun_ptr dun, point_t pos, int radius, dun_grid_p filter);
extern void         dun_flow_free(dun_flow_ptr flow);
extern void         dun_flow_recalc(dun_flow_ptr flow, point_t pos);
extern int          dun_flow_at(dun_flow_ptr flow, point_t pos); /* out of bounds return DUN_FLOW_NULL */
extern int          dun_flow_at_plr(dun_flow_ptr flow);
extern int          dun_flow_at_mon(dun_flow_ptr flow, mon_ptr mon);


/************************************************************************
 * Manager (Singleton)
 ************************************************************************/
#define _MRU_LEN 15
typedef struct dun_mgr_prof_s dun_mgr_prof_t, *dun_mgr_prof_ptr;
struct dun_mgr_s
{
    u16b        next_dun_id;
    u16b        next_obj_id;
    u16b        next_pack_id;
    u32b        next_mon_id;
    u32b        turn;         /* overflows after about 40,000 game days */
    int_map_ptr dungeons;
    u16b        mru[_MRU_LEN];
    int         mru_head, mru_tail;
    dun_ptr     world;
    dun_frac_ptr world_frac;
    dun_ptr     surface;
    u32b        world_seed;
    int_map_ptr packs;        /* monster pack info here allows pack monsters to change level */
    u16b        plr_pack_id;
    dun_mgr_prof_ptr prof;    /* optional */
};
extern dun_mgr_ptr  dun_mgr(void);
extern int          dun_mgr_next_mon_id(void);
extern int          dun_mgr_next_obj_id(void);
extern int          dun_mgr_next_dun_id(void);
extern int          dun_mgr_next_pack_id(void);
extern dun_ptr      dun_mgr_alloc_dun(rect_t rect);
extern void         dun_mgr_delete_surface(void);
extern dun_ptr      dun_mgr_dun(int id);
extern mon_pack_ptr dun_mgr_alloc_pack(void);
extern mon_pack_ptr dun_mgr_pack(int id);
extern void         dun_mgr_free_pack(mon_pack_ptr pack);
extern mon_ptr      dun_mgr_relocate_unique(int race_id, dun_ptr dun, point_t pos); /* quests */
extern int          dun_mgr_count_mon_race(int race_id);

extern bool         dun_mgr_recall_plr(void); /* Word of Recall spell or FF_RECALL */
extern void         dun_mgr_travel_plr(void); /* FF_TRAVEL */
extern void         dun_mgr_wizard_jump(int dun_type_id, int dun_lvl);
extern bool         dun_mgr_teleport_town(u32b flags);

extern void         dun_mgr_process(void);
extern void         dun_mgr_gc(bool force);         /* garbage collect distant levels */
extern void         dun_mgr_doc(doc_ptr doc);
extern void         dun_mgr_plr_change_dun(dun_ptr dun, point_t pos);
extern void         dun_mgr_load(savefile_ptr file);
extern void         dun_mgr_save(savefile_ptr file);

extern void         dun_mgr_display(rect_t map_rect);

extern mon_pack_ptr plr_pack(void);  /* pets */

struct dun_mgr_prof_s
{
    z_timer_t run_timer;
    z_timer_t view_timer;
    u32b      view_count;
    z_timer_t light_timer;
    z_timer_t flow_timer;
    z_timer_t gen_timer;
    z_timer_t redraw_timer;
    time_t    wall_time;
};
/************************************************************************
 * Worlds
 ************************************************************************/
enum { W_NONE = 0,
       W_SMAUG,
       W_SARUMAN,
       W_SAURON,
       W_AMBER,
};
struct dun_world_s
{
    u16b    id;
    u16b    flags;
    cptr    name;
    cptr    desc;
    cptr    file;        /* map file to generate D_WORLD dun_t (The World Map) */
    void    (*init_f)(dun_world_ptr me);
    void    (*pre_gen_f)(dun_world_ptr me, dun_gen_ptr dun);
    void    (*post_gen_f)(dun_world_ptr me, dun_gen_ptr dun);
    void    (*change_dun_f)(dun_world_ptr me, dun_ptr dun);
    void    (*kill_mon_f)(dun_world_ptr me, mon_ptr mon);
    int     (*mon_alloc_f)(dun_world_ptr me, mon_race_ptr race, int prob);
    void    (*surface_feat_f)(dun_world_ptr me, point_t pos, dun_grid_ptr grid);
    void    (*save_f)(dun_world_ptr me, savefile_ptr file);
    void    (*load_f)(dun_world_ptr me, savefile_ptr file);

    u16b    final_dungeon;
    u16b    final_guardian;
    u16b    next_world_id;  /* slay the final_guardian to go to the next_world_id */
    u16b    plr_flags;
    byte    encounter_chance;  /* permil chance of wilderness encounters (ROOM_WILDERNESS) */

    vec_ptr mon_alloc_tbl;
    vec_ptr obj_alloc_tbl;
};

#define WF_RIVER_LAVA 0x0001

#define WFP_ENTERED   0x0001
#define WFP_COMPLETED 0x0002
#define WFP_FAILED    0x0004

extern int           dun_world_town_id(void);
extern void          dun_world_map_ui(void);
extern void          dun_world_reseed(u32b world_seed);
extern void          dun_world_dump_frac(dun_ptr dun); /* XXX debug */
extern dun_ptr       dun_world_gen_map(dun_world_ptr world);
extern void          dun_gen_world_wizard(void);

extern dun_world_ptr dun_worlds_current(void);
extern void          dun_worlds_birth(void);
extern int           dun_worlds_parse(cptr name);
extern dun_world_ptr dun_worlds_lookup(int id);
extern void          dun_worlds_save(savefile_ptr file);
extern void          dun_worlds_load(savefile_ptr file);

extern void          dun_worlds_wizard(int id);

/************************************************************************
 * Hacks
 ************************************************************************/
/* The "cave" pointer tries to always be the level containing the player with the
 * following exceptions:
 * [1] Generating a new level temporarily sets "cave" to the new dun_ptr so the
 *     existing generation code can function.
 * [2] Processing monsters likewise fudges the "cave" to the level being processed.
 * But outside of a game loop (e.g., during shutdown), you can generally rely on
 * a non-null "cave" that is the current player's level. Code will eventually need
 * to know about the dun_ptr and handle plr presence or absence gracefully.
 *
 * See also cave.c for some transitional helpers.
 * XXX I'd like to remove cave some day ... Use plr_dun() and mon_dun() instead.
 * We only ever display the plr_dun. XXX */
extern dun_ptr cave;

#endif
