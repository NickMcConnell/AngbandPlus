#ifndef GLOBALS_H
#define GLOBALS_H

#include "src/structures.h"
#include "src/dun_classes.h"
#include "src/mon_classes.h"
#include "src/player_classes.h"
#include <QString>
#include <QFile>

//was externs.h


extern byte game_mode;

extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_xtra;
extern u32b seed_randart;
extern u32b seed_flavor;
extern u32b seed_town;
extern u32b seed_ghost;
extern s16b object_level;
extern s16b monster_level;
extern QChar summon_kin_type;
extern monster_type *summoner;
extern int which_keyset;
extern int use_graphics;

extern s16b o_max;
extern s16b o_cnt;
extern s16b mon_max;
extern s16b mon_cnt;
extern s16b x_max;
extern s16b x_cnt;
extern byte feeling;
extern bool do_feeling;
extern s16b rating;
extern u32b level_flag;
extern bool good_item_flag;

extern QString current_savefile;

extern s16b add_wakeup_chance;
extern s16b total_wakeup_chance;

//Various game arrays and lists
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *mon_list;
extern monster_lore *l_list;
extern effect_type *x_list;
extern const player_sex *sp_ptr;
extern const player_race *rp_ptr;
extern player_class *cp_ptr;
extern player_magic *mp_ptr;
extern player_other *op_ptr;
extern player_type *p_ptr;
extern player_attribute_maximums *pam_ptr;
extern vault_type *v_info;
extern feature_type *f_info;
extern feature_lore *f_l_list;
extern object_kind *k_info;
extern ghost_template *t_info;
extern artifact_type *a_info;
extern artifact_lore *a_l_list;
extern ego_item_type *e_info;
extern monster_race *r_info;
extern player_race *p_info;
extern player_class *c_info;
extern hist_type *h_info;
extern owner_type *b_info;
extern byte *g_info;
extern flavor_type *flavor_info;
extern quest_type *q_info;
extern names_type *n_info;
extern store_type *store;
extern object_type *inventory;
extern QVector<alloc_entry_new> alloc_kind_table;
extern QVector<alloc_entry_new> alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern s16b alloc_feat_size;
extern alloc_entry *alloc_feat_table;

extern option_entry options[OPT_MAX];

extern s16b x_pop(void);

extern byte squelch_level[SQUELCH_BYTES];

extern quiver_group_type quiver_group[MAX_QUIVER_GROUPS];


extern bool item_tester_full;
extern byte item_tester_tval;
extern bool item_tester_swap;
extern bool (*item_tester_hook)(object_type*);
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern bool (*get_feat_num_hook)(int k_idx);

/* Sorting functions */
extern bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);
extern void (*ang_sort_swap)(void *u, void *v, int a, int b);
extern void ang_sort(void *u, void *v, int n);

// Monser race messages
extern QVector<monster_race_message> mon_msg;
extern QVector<monster_message_history> mon_message_hist;

//monster movement information
extern QVector<move_moment_type> mon_moment_info;

// Track which squares need to be drawn
extern QVector<coord> redraw_coords;

// Vector lists of dungeon grids
extern QVector<coord> view_grids;
extern QVector<coord> fire_grids;
extern QVector<coord> project_grids;
extern QVector<coord> room_grids;
extern QVector<coord> target_grids;

extern u32b mon_power_ave[MAX_DEPTH_ALL][CREATURE_TYPE_MAX];

extern QVector<dynamic_grid_type> dyna_grids;
extern u16b dyna_next;


extern dungeon_type dungeon_info[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
extern u16b cave_cost[MAX_FLOWS][MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
extern int cost_at_center[MAX_FLOWS];


extern u16b quest_indicator_timer;
extern byte quest_indicator_complete;


extern dungeon_capabilities_type *dun_cap;

//pre-defined colors - loaded at startup
extern QColor defined_colors[MAX_COLORS];
extern byte tval_to_attr[128];

extern byte num_trap_on_level;
extern s16b player_ghost_num;
extern s16b ghost_r_idx;
extern QString player_ghost_name;
extern QString g_vault_name;
extern u16b altered_inventory_counter;
extern bool allow_altered_inventory;
extern u32b dungeon_summon_mask_f7;


extern s16b coin_type;
extern byte object_generation_mode;
extern bool repair_mflag_mark;
extern bool repair_mflag_show;

extern const byte option_page_nppangband[OPT_PAGE_MAX][OPT_PAGE_PER];
extern const byte option_page_nppmoria[OPT_PAGE_MAX][OPT_PAGE_PER];


#endif // GLOBALS_H
