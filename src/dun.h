#ifndef INCLUDED_DUN_H
#define INCLUDED_DUN_H

#include "point_map.h"

/************************************************************************
 * Grid
 ************************************************************************/
/* use existing cave_type for now */
typedef struct cave_type dun_grid_t, *dun_grid_ptr;

/* use existing feature_type for now (perhaps forever!) */
typedef struct feature_type dun_feat_t, *dun_feat_ptr;

typedef void (*dun_grid_f)(point_t pos, dun_grid_ptr grid);
typedef bool (*dun_grid_p)(point_t pos, dun_grid_ptr grid);

extern bool dun_grid_allow_drop(dun_grid_ptr grid);
extern dun_feat_ptr dun_grid_feat(dun_grid_ptr grid);
extern dun_feat_ptr dun_grid_feat_mimic(dun_grid_ptr grid);

/************************************************************************
 * GridEx
 ************************************************************************/
typedef struct dun_grid_ex_s dun_grid_ex_t, *dun_grid_ex_ptr;
struct dun_grid_ex_s
{
    dun_grid_ptr grid;
    dun_feat_ptr feat;
    dun_feat_ptr feat_mimic;
    mon_ptr      mon;
    obj_ptr      obj;
    point_t      pos;
};

/************************************************************************
 * Dungeon Type (Angband, Icky Caves, Isengard, etc)
 ************************************************************************/
typedef struct dun_gen_s dun_gen_t, *dun_gen_ptr;

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

       /* XXX */
       D_OLYMPUS };

struct dun_type_s
{
    u16b    id;
    cptr    name;
    cptr    desc;
    cptr    parse;
    rect_t  (*size_f)(dun_type_ptr me);
    void    (*enter_f)(dun_type_ptr me);
    void    (*pre_gen_f)(dun_type_ptr me, dun_gen_ptr gen);
    void    (*post_gen_f)(dun_type_ptr me, dun_gen_ptr gen);
    void    (*change_dun_f)(dun_type_ptr me, dun_ptr dun);
    void    (*kill_mon_f)(dun_type_ptr me, mon_ptr mon);
    int     (*mon_alloc_f)(dun_type_ptr me, mon_race_ptr race, int prob);
    void    (*exit_f)(dun_type_ptr me);
    void    (*save_f)(dun_type_ptr me, savefile_ptr file);
    void    (*load_f)(dun_type_ptr me, savefile_ptr file);
    u32b    flags;       /* DF_* flags are unsaved. */

    s16b    min_dun_lvl;
    s16b    max_dun_lvl;
    s16b    final_guardian;

    s16b    feat_wall_outer;
    s16b    feat_wall_inner;
    s16b    feat_wall_solid;
    s16b    stream1, stream2;
    s16b    floor_type[100], fill_type[100];

    vec_ptr mon_alloc_tbl;
    vec_ptr obj_alloc_tbl;

    point_t world_pos;   /* grid in D_WORLD for dungeon entrance */
    s16b    plr_max_lvl; /* plr recall depth */
    byte    last_recall; /* counter to force FF_RECALL rooms */
    byte    last_shop;   /* counter to force ROOM_SHOP rooms */
    u16b    plr_flags;   /* DFP_* flags are saved */
};
/* Generation Flags */
#define DF_RIVER_WATER      0x00000001
#define DF_RIVER_LAVA       0x00000002
#define DF_LAKE_WATER       0x00000004
#define DF_LAKE_LAVA        0x00000008
#define DF_LAKE_RUBBLE      0x00000010
#define DF_LAKE_TREE        0x00000020
#define DF_NO_DOORS         0x00000040
#define DF_CURTAIN          0x00000080
#define DF_CAVE             0x00000100
#define DF_CAVERN           0x00000200
#define DF_ARENA            0x00000400
#define DF_MAZE             0x00000800
#define DF_NO_VAULT         0x00001000
#define DF_DESTROY          0x00002000
#define DF_NO_CAVE          0x00004000
#define DF_TOWER            0x00008000
#define DF_GEN_MASK         0x0000FFFF
#define DF_LAKE_MASK (DF_LAKE_WATER | DF_LAKE_LAVA | DF_LAKE_RUBBLE | DF_LAKE_TREE)

/* restrictions (shared with dun_t.flags) */
#define DF_NO_MAGIC         0x00100000
#define DF_NO_MELEE         0x00200000
#define DF_NO_GENOCIDE      0x00400000
#define DF_NO_DESTRUCT      0x00800000
#define DF_NO_QUAKE         0x01000000
#define DF_NO_SUMMON        0x02000000   /* XXX not implemented yet */
#define DF_NO_TELEPORT      0x04000000   /* XXX not implemented yet */
#define DF_NO_REGEN         0x08000000   /* XXX not implemented yet */
#define DF_RESTRICT_MASK    0x0FF00000

/* other */
#define DF_RANDOM           0x10000000

/* player flags */
#define DFP_ENTERED         0x0001 /* plr has entered this dungeon */
#define DFP_COMPLETED       0x0002 /* plr has conquered this dungeon (completely in case of multiple guardians) */
#define DFP_FAILED          0x0004 /* plr has failed this dungeon and may not return */
#define DFP_SECRET          0x0008 /* plr needs to do something to reveal this dungeon */

extern int          dun_types_parse(cptr name);
extern dun_type_ptr dun_types_lookup(int id);
extern dun_type_ptr dun_types_choose(cptr prompt, bool wizard);
extern dun_type_ptr dun_types_random(int level);
extern void         dun_types_reset_world(void);
extern void         dun_types_save(savefile_ptr file);
extern void         dun_types_load(savefile_ptr file);
extern vec_ptr      plr_dun_types(void);
extern vec_ptr      plr_towns(void);
extern int          plr_max_dun_lvl(void);
extern vec_ptr      world_dun_types(void);
extern vec_ptr      world_towns(void);

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
};

/************************************************************************
 * Page
 *
 * Every dungeon level maintains a list of pages for grid storage. Most
 * levels inside of a dungeon will only have a single page for the entire
 * level, where page->rect is the same as dun->rect. The wilderness
 * surface will maintain pages for adjacent D_WORLD tiles, scrolling
 * as the player moves about.
 ************************************************************************/
typedef struct dun_page_s dun_page_t, *dun_page_ptr;
struct dun_page_s
{
    rect_t       rect;
    u32b         flags;
    dun_grid_ptr grids;
    dun_page_ptr next;
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
typedef struct dun_flow_s dun_flow_t, *dun_flow_ptr;
struct dun_s
{
    u16b           dun_id;     /* unique id (opaque) */
    s16b           dun_type_id;
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
    s16b           plr_dis; /* distance from plr to connecting stairs if plr not on level; 0 otherwise */
    town_ptr       town;    /* DF_SHOP */
};
#define DF_GENERATED        0x00000001
#define DF_LOCKED           0x00000002
#define DF_UPDATE_FLOW      0x00000004
#define DF_MARK             0x00000008
#define DF_VISITED          0x00000010
#define DF_RECALL           0x00000020  /* level has a ROOM_RECALL room; update stuff when "visited" */
#define DF_SHOP             0x00000040  /* level has a ROOM_SHOP room; update stuff when "visited" */
#define DF_AUTOSAVE         0x00000080
#define DF_TRAVEL           0x00000100  /* level has ROOM_TRAVEL (cf. _pre_gen_rooms) */

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
 * player, keeping p_ptr->pos well in the interior region. Boundary
 * grids are not permanent in this case, but are out of bounds for 
 * monsters, objects and players. */
extern bool          dun_pos_valid(dun_ptr dun, point_t pos);
extern bool          dun_pos_boundary(dun_ptr dun, point_t pos);
extern bool          dun_pos_interior(dun_ptr dun, point_t pos);

/* Grid and Feature Access */
extern dun_grid_ptr  dun_grid_at(dun_ptr dun, point_t pos);
extern dun_grid_ex_t dun_grid_ex_at(dun_ptr dun, point_t pos);
extern dun_feat_ptr  dun_feat_at(dun_ptr dun, point_t pos);
extern dun_feat_ptr  dun_feat_mimic_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_drop_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_mon_at(dun_ptr dun, point_t pos);
extern bool          dun_stop_disintegration_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_los_at(dun_ptr dun, point_t pos);
extern bool          dun_allow_project_at(dun_ptr dun, point_t pos);

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
extern point_t       dun_random_grid(dun_ptr dun, int (*weight_f)(point_t pos, dun_grid_ptr grid));

/* XXX need to think how to handle low level primitives in a multi-dungeon scenario.
 * Hacking a global "cave" pointer and calling existing code is a last resort, but
 * may be required until old code can be migrated. */
extern bool          dun_los(dun_ptr dun, point_t p1, point_t p2);
extern bool          dun_projectable(dun_ptr dun, point_t p1, point_t p2);
extern point_vec_ptr dun_project_path(dun_ptr dun, int range, point_t p1, point_t p2, int flags);

/* Monster Management */
extern mon_ptr       dun_alloc_mon(dun_ptr dun);
extern mon_ptr       dun_mon(dun_ptr dun, int id);
extern vec_ptr       dun_adjacent_mon(dun_ptr dun, point_t pos);
extern vec_ptr       dun_mon_filter(dun_ptr dun, mon_p p);
extern mon_ptr       dun_mon_at(dun_ptr dun, point_t pos);
extern void          dun_attach_mon(dun_ptr dun, mon_ptr mon);
extern void          dun_place_mon(dun_ptr dun, mon_ptr mon, point_t pos);
extern void          dun_move_mon(dun_ptr dun, mon_ptr mon, point_t pos);
extern mon_ptr       dun_detach_mon(dun_ptr dun, int id);
extern void          dun_delete_mon(dun_ptr dun, int id);
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
extern void          dun_mon_pickup(dun_ptr dun, mon_ptr mon);
extern void          dun_mon_destroy(dun_ptr dun, mon_ptr mon);
extern void          dun_mon_steal_plr(dun_ptr dun, mon_ptr mon, obj_ptr obj); /* from player, so obj copied into dun */
extern bool          dun_fetch_obj(dun_ptr dun, point_t from, point_t to);
extern obj_ptr       dun_detach_obj(dun_ptr dun, int id);
extern void          dun_delete_obj(dun_ptr dun, int id);
extern void          dun_destroy_obj_at(dun_ptr dun, point_t pos);  /* destruction, earthquake, etc */
extern void          dun_iter_obj(dun_ptr dun, void (*f)(int id, obj_ptr obj));
extern void          dun_iter_floor_obj(dun_ptr dun, void (*f)(point_t pos, obj_ptr pile));
extern vec_ptr       dun_filter_obj(dun_ptr dun, obj_p p); /* floor objects only */

/* Player Management */
extern void          dun_move_plr(dun_ptr dun, point_t pos);
extern void          dun_world_move_plr(dun_ptr dun, point_t pos);
extern bool          dun_plr_at(dun_ptr dun, point_t pos);
extern void          dun_update_flow(dun_ptr dun);

/* Stairs and Changing Levels (cf dun_mgr_recall_plr for Word of Recall) */
extern void          dun_quest_stairs(dun_ptr dun, point_t pos, int lvl); /* "A magical staircase appears ..." */
extern void          dun_quest_travel(dun_ptr dun, point_t pos);
extern void          dun_add_stairs(dun_ptr dun, dun_stairs_ptr info);
extern bool          dun_take_stairs_plr(dun_ptr dun);
extern bool          dun_take_stairs_mon(dun_ptr dun, mon_ptr mon);
extern void          dun_trap_door_plr(dun_ptr dun);
extern void          dun_teleport_level_plr(dun_ptr dun);
extern void          dun_teleport_level_mon(dun_ptr dun, mon_ptr mon);
extern bool          dun_create_stairs(dun_ptr dun, bool down_only);

/* Game Processing */
extern void          dun_process(dun_ptr dun);

extern void          dun_forget_flow(dun_ptr dun);
extern void          dun_note_pos(dun_ptr dun, point_t pos);
extern void          dun_lite_pos(dun_ptr dun, point_t pos);

/* Savefile Support */
extern void          dun_load(dun_ptr dun, savefile_ptr file);
extern void          dun_save(dun_ptr dun, savefile_ptr file);

/************************************************************************
 * Flow (dun_util.c)
 ************************************************************************/
typedef struct flow_data_s flow_data_t, *flow_data_ptr;
struct flow_data_s { byte cost; byte dist; };

extern dun_flow_ptr dun_flow_calc(dun_ptr dun, point_t pos, int radius, dun_grid_p filter);
extern void         dun_flow_free(dun_flow_ptr flow);
extern void         dun_flow_recalc(dun_flow_ptr flow, point_t pos);
extern flow_data_t  dun_flow_at(dun_flow_ptr flow, point_t pos); /* out of bounds return 0 */


/************************************************************************
 * Manager (Singleton)
 ************************************************************************/
#define _MRU_LEN 7
typedef struct dun_mgr_s dun_mgr_t, *dun_mgr_ptr;
struct dun_mgr_s
{
    u16b        next_dun_id;
    u16b        next_mon_id;
    u16b        next_obj_id;
    u16b        next_pack_id;
    u32b        turn;         /* overflows after about 40,000 game days */
    int_map_ptr dungeons;
    u16b        mru[_MRU_LEN];
    int         mru_head, mru_tail;
    dun_ptr     world;
    dun_ptr     surface;
    u32b        world_seed;
    int_map_ptr packs;        /* monster pack info here allows pack monsters to change level */
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
extern bool         dun_mgr_teleport_town(void);

extern void         dun_mgr_process(void);
extern void         dun_mgr_gc(bool force);         /* garbage collect distant levels */
extern void         dun_mgr_doc(doc_ptr doc);
extern void         dun_mgr_plr_change_dun(dun_ptr dun, point_t pos);
extern void         dun_mgr_load(savefile_ptr file);
extern void         dun_mgr_save(savefile_ptr file);

extern void         dun_mgr_display(rect_t map_rect);

/************************************************************************
 * Worlds
 ************************************************************************/
enum { W_NONE = 0,
       W_SMAUG,
       W_SARUMAN,
       W_SAURON };
typedef struct dun_world_s dun_world_t, *dun_world_ptr;
struct dun_world_s
{
    u16b    id;
    u16b    flags;
    cptr    name;
    cptr    desc;
    cptr    file;        /* map file to generate D_WORLD dun_t (The World Map) */
    void    (*pre_gen_f)(dun_world_ptr me, dun_gen_ptr dun);
    void    (*post_gen_f)(dun_world_ptr me, dun_gen_ptr dun);
    void    (*change_dun_f)(dun_world_ptr me, dun_ptr dun);
    void    (*kill_mon_f)(dun_world_ptr me, mon_ptr mon);
    int     (*mon_alloc_f)(dun_world_ptr me, mon_race_ptr race, int prob);
    void    (*save_f)(dun_world_ptr me, savefile_ptr file);
    void    (*load_f)(dun_world_ptr me, savefile_ptr file);

    u16b    final_dungeon;
    u16b    final_guardian;
    u16b    next_world_id;  /* slay the final_guardian to go to the next_world_id */
    u16b    plr_flags;

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
