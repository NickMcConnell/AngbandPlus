/* File: tnb.h */

/* Purpose: AngbandTk header */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_TNB_H_
#define _INCLUDE_TNB_H_

#if !defined(PLATFORM_MAC) && !defined(PLATFORM_WIN) && !defined(PLATFORM_X11)
#error "you must define one of PLATFORM_MAC, PLATFORM_WIN or PLATFORM_X11"
#endif /* */

#if !defined(ANGBANDTK) && !defined(KANGBANDTK) && !defined(OANGBANDTK) && !defined(ZANGBANDTK)
#error "you must pass -DxANGBANDTK to the compiler"
#endif /* */

/*
 * Hack -- The Borg in AngbandTk requires that we export some stuff.
 * Other variants don't need to export symbols from the EXE.
 */
#ifndef ANG_EXTERN
#define ANG_EXTERN extern
#define ANG_STORAGE_CLASS
#endif /* not ANG_EXTERN */

/*
 * These macros mask some differences between different variants
 * to make the code cleaner.
 */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
#define cave_feat(y,x) cave_feat[y][x] /* ASSERT(inbounds(y,x)) ? */
#define cave_info(y,x) cave_info[y][x]
#define cave_m_idx(y,x) cave_m_idx[y][x]
#define cave_o_idx(y,x) cave_o_idx[y][x]
#define in_bounds_test in_bounds
#define in_bounds_fully_test in_bounds_fully
#define p_ptr_depth p_ptr->depth
#define p_ptr_max_depth p_ptr->max_depth
#define p_ptr_max_lev p_ptr->max_lev
#define op_ptr_base_name op_ptr->base_name
#define op_ptr_delay_factor op_ptr->delay_factor
#define op_ptr_hitpoint_warn op_ptr->hitpoint_warn
#define op_ptr_full_name op_ptr->full_name
#define p_ptr_py p_ptr->py
#define p_ptr_px p_ptr->px
#define p_ptr_running p_ptr->running
#define p_ptr_resting p_ptr->resting
#define p_ptr_command_arg p_ptr->command_arg
#define p_ptr_command_cmd p_ptr->command_cmd
#define p_ptr_command_rep p_ptr->command_rep
#define p_ptr_command_wrk p_ptr->command_wrk
#define p_ptr_target_who p_ptr->target_who
#define p_ptr_target_col p_ptr->target_col
#define p_ptr_target_row p_ptr->target_row
#define p_ptr_is_dead p_ptr->is_dead
#define p_ptr_died_from p_ptr->died_from
#define p_ptr_wizard p_ptr->wizard
#define p_ptr_total_winner p_ptr->total_winner
#define p_ptr_history p_ptr->history
#define p_ptr_player_hp p_ptr->player_hp
#define p_ptr_noscore p_ptr->noscore
#define p_ptr_energy_use p_ptr->energy_use
#define p_ptr_equip_cnt p_ptr->equip_cnt
#define p_ptr_point_based adult_point_based
#define p_ptr_auto_roller adult_auto_roller
#define p_ptr_maximize adult_maximize
#define p_ptr_preserve adult_preserve
#if defined(ANGBANDTK) || defined(KANGBANDTK)
#define mp_ptr_spell_type mp_ptr->spell_type
#define MAX_VALID_R_IDX (MAX_R_IDX - 1)
#endif /* */
#if defined(OANGBANDTK)
#define mp_ptr_spell_type (mp_ptr->spell_book - TV_MAGIC_BOOK)
#define easy_floor 1
#define MAX_SHAPE 11
#define MAX_VALID_R_IDX MAX_R_IDX
#endif /* */
#define set_user_inscription(o,q) o->note = q
#define get_user_inscription(o) o->note
#define player_is_here(y,x) \
	(cave_m_idx[y][x] < 0)
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK)
#define MAX_A_IDX z_info->a_max
#define MAX_F_IDX z_info->f_max
#define MAX_K_IDX z_info->k_max
#define MAX_M_IDX z_info->m_max
#define MAX_O_IDX z_info->o_max
#define MAX_R_IDX z_info->r_max
#define MAX_V_IDX z_info->v_max
#define MAX_P_IDX z_info->p_max
#endif /* ANGBANDTK, KANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
#define race_info p_info
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

#if defined(OANGBANDTK)
#define OPT_ADULT OPT_adult_start
#define OPT_BIRTH OPT_birth_start
#define OPT_CHEAT OPT_cheat_start
#define OPT_SCORE OPT_score_start
#endif

#if defined(ZANGBANDTK)
#define MAX_A_IDX max_a_idx
#define MAX_F_IDX max_f_idx
#define MAX_K_IDX max_k_idx
#define MAX_M_IDX max_m_idx
#define MAX_O_IDX max_o_idx
#define MAX_R_IDX max_r_idx
#define MAX_V_IDX max_v_idx
#define MAX_P_IDX MAX_RACES
#define MAX_QUESTS max_quests
#define DUNGEON_WID MAX_WID
#define DUNGEON_HGT MAX_HGT
#define cave_feat(y,x) cave[y][x].feat
#define cave_info(y,x) cave[y][x].info
#define cave_m_idx(y,x) cave[y][x].m_idx
#define cave_o_idx(y,x) cave[y][x].o_idx
#define in_bounds_test in_bounds2
#define in_bounds_fully_test in_bounds
#define p_ptr_depth dun_level
#define p_ptr_max_depth p_ptr->max_dlv
#define p_ptr_max_lev p_ptr->max_plv
#define op_ptr_base_name player_base
#define op_ptr_delay_factor delay_factor
#define op_ptr_hitpoint_warn hitpoint_warn
#define op_ptr_full_name player_name
#define p_ptr_py py
#define p_ptr_px px
#define p_ptr_running running
#define p_ptr_resting resting
#define p_ptr_command_arg command_arg
#define p_ptr_command_cmd command_cmd
#define p_ptr_command_rep command_rep
#define p_ptr_command_wrk command_wrk
#define p_ptr_target_who target_who
#define p_ptr_target_col target_col
#define p_ptr_target_row target_row
#define p_ptr_is_dead death
#define p_ptr_died_from died_from
#define p_ptr_wizard wizard
#define p_ptr_total_winner total_winner
#define p_ptr_history history
#define p_ptr_player_hp player_hp
#define p_ptr_noscore noscore
#define p_ptr_energy_use energy_use
#define p_ptr_equip_cnt equip_cnt
#define p_ptr_point_based point_based
#define p_ptr_auto_roller autoroller
#define p_ptr_maximize maximize_mode
#define p_ptr_preserve preserve_mode
#define Rand_normal randnor
#define MAX_VALID_R_IDX max_r_idx
#define set_user_inscription(o,q) o->inscription = q
#define get_user_inscription(o) o->inscription
#define player_is_here(y,x) \
	(((y) == py) && ((x) == px))
#endif /* ZANGBANDTK */

/*
 * Angband, KAngband and OAngband have monster_lore, while
 * ZAngband has monster_race. In addition, OAngband has
 * removed the "r_" prefix before field names.
 */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
#define LORE_NTH(n) l_list[n]
#define LORE_TYPE monster_lore
#define LF(f) r_##f
#endif
#if defined(OANGBANDTK)
#define LORE_NTH(n) l_list[n]
#define LORE_TYPE monster_lore
#define LF(f) f
#endif
#if defined(ZANGBANDTK)
#define LORE_NTH(n) r_info[n]
#define LORE_TYPE monster_race
#define LF(f) r_##f
#endif
#define DECLARE_LORE(v) LORE_TYPE *v
#define AT_LORE(n) &LORE_NTH(n)

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
#define ART_CURNUM(a) a->cur_num
#endif
#if defined(OANGBANDTK)
#define ART_CURNUM(a) a->creat_stat
#endif

#define monster_race_valid(r_idx) ((r_idx >= 0) && (r_idx < MAX_VALID_R_IDX))

#if defined(ANGBANDTK) || defined(OANGBANDTK)
#define monster_is_friend(m) 0
#endif /* ANGBANDTK, OANGBANDTK */
#if defined(KANGBANDTK)
#define monster_is_friend(m) ((m)->is_friendly || (m)->is_pet)
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
#define monster_is_friend(m) (is_friendly(m) || is_pet(m))
#endif /* ZANGBANDTK */

#ifndef PY_MAX_SPELLS
#define PY_MAX_SPELLS 64
#endif
#if !defined(ANGBANDTK) && !defined(KANGBANDTK)
#define message_flush() msg_print(NULL)
#endif

#include "dbwin.h"
#include "sound.h"

#define PU_MAP_INFO 0x04000000L	/* Update g_grid[] */

/* cmd4.c */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
#define CHEAT_MAX 6
#endif /* */
#if defined(ZANGBANDTK)
#define CHEAT_MAX 6
extern option_type cheat_info[];
#endif /* */

/* dungeon.c */
extern bool sense_chance(int *mage, int *warrior);

/* files.c */
#if defined(ZANGBANDTK)
extern void player_flags(u32b *f1, u32b *f2, u32b *f3);
#endif

/* init1.c */
extern char **r_info_flags[10];
extern int r_info_flags_max;
extern cptr k_info_flags1[], k_info_flags2[], k_info_flags3[];
extern cptr r_info_flags1[], r_info_flags2[], r_info_flags3[],
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	r_info_flags4[], r_info_flags5[], r_info_flags6[];
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
	r_info_flags4[], r_info_flags5[], r_info_flags6[], r_info_flags7[],
	r_info_flags8[], r_info_flags9[];

/* racial.c */
extern int racial_chance(s16b min_level, int use_stat, int difficulty);

/* spells3.c */
extern void spell_info(char *p, int spell, int realm);
#endif /* ZANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK)
extern void look_mon_desc(char *buf, int m_idx);
#endif /* */

#if defined(OANGBANDTK)
extern int *artifact_normal, *artifact_special;
extern int artifact_normal_cnt, artifact_special_cnt;
extern void init_artifacts(void);
#endif /* */

/* main-tnb.c */
extern bool g_initialized;
extern bool game_in_progress;
extern cptr ANGBAND_DIR_XTRA_HELP;
extern cptr ANGBAND_DIR_SOUND;
extern cptr ANGBAND_DIR_ROOT;
extern cptr ANGBAND_DIR_TK;
extern cptr ANGBAND_DIR_COMMON;
extern cptr ANGBAND_DIR_COMMON_TK;

/* canv-widget.c */
extern int CanvasWidget_Init(void);
extern void CanvasWidget_Idle(void);
extern void CanvasWidget_Setup(void);
extern void CanvasWidget_Unload(void);
extern void CanvasWidget_Exit(void);

/* icon1.c */

#define FLAVOR_AMULET 0
#define FLAVOR_MUSHROOM 1
#define FLAVOR_POTION 2
#define FLAVOR_RING 3
#define FLAVOR_ROD 4
#define FLAVOR_STAFF 5
#define FLAVOR_WAND 6
#define FLAVOR_MAX 7

extern void angtk_flavor_init(int *max, byte **attr);
extern void angtk_flavor_swap(int n, int a, int b);

/* Constants for g_feat_flag[] */
#define FEAT_FLAG_BORING 0x01 /* Feature is optionally memorized */
#define FEAT_FLAG_TOWN 0x02 /* Feature uses lighting in the town */

extern byte *g_feat_flag;

/* Constants for t_grid.dark */
#define GRID_LITE_TORCH 0
#define GRID_LITE_NORMAL 1
#define GRID_LITE_DARK 2

/* Constants for t_grid.xtra */
#define GRID_XTRA_LITE_0 0x0001 /* Light radius bit 1/2 (NOT USED) */
#define GRID_XTRA_LITE_1 0x0002 /* Light radius bit 2/2 (NOT USED) */
#define GRID_XTRA_PILLAR 0x0004 /* Grid is a pillar */
#define GRID_XTRA_ISVERT 0x0008 /* Door is vertical */
#define GRID_XTRA_WALL 0x0010 /* Is a wall or secret door */
#define GRID_XTRA_DOOR 0x0020 /* Is a door */

/*
 * Option: Use unique image/description for floor stack of items.
 */
#define ALLOW_PILE_IMAGE

#ifdef ALLOW_PILE_IMAGE
#define GRID_XTRA_PILE   0x8000 /* Pile of items */
#endif /* ALLOW_PILE_IMAGE */

/* Constants for t_grid.shape */
enum {
GRID_SHAPE_NOT,
GRID_SHAPE_SINGLE,
GRID_SHAPE_NS,
GRID_SHAPE_WE,
GRID_SHAPE_CORNER_NW,
GRID_SHAPE_CORNER_NE,
GRID_SHAPE_CORNER_SW,
GRID_SHAPE_CORNER_SE,
GRID_SHAPE_TRI_N,
GRID_SHAPE_TRI_S,
GRID_SHAPE_TRI_W,
GRID_SHAPE_TRI_E,
GRID_SHAPE_QUAD,
GRID_SHAPE_MAX
};

extern int wall_shape(int y, int x, bool force);
extern bool is_door(int y, int x);
extern bool is_wall(int y, int x);

/*
 * Memory of what is seen at a cave location for the entire cave.
 */
typedef struct t_grid {
	int dark; /* GRID_LITE_XXX flags */
	int f_idx; /* Feature */
	int o_idx; /* Object */
	int m_idx; /* Character/Monster */
	int xtra; /* GRID_XTRA_XXX flags */
	int shape; /* GRID_SHAPE_XXX enum */
} t_grid;
extern t_grid *g_grid[DUNGEON_HGT];

/* TRUE if g_grid[].xtra was initialized */
extern int g_grid_xtra_init;

extern bool g_daytime; /* Day or night */

/* Cave location -> t_grid */
extern void get_grid_info(int y, int x, t_grid *gridPtr);

extern void angtk_flicker(void);
extern void angtk_image_reset(void);
extern void angtk_cave_changed(void);
extern void angtk_feat_known(int y, int x);
extern void angtk_feat_changed(int y, int x);
extern void angtk_view_floor(int y, int x, int info, int torch);
extern void angtk_view_wall(int y, int x, int info, int torch);
extern void set_grid_assign(int y, int x);
extern bool door_vertical(int y, int x);
extern void free_icons(void);
extern void init_palette(void);
extern unsigned long Milliseconds(void);

/* widget.c */
extern void angtk_widget_lock(bool lock);
extern void angtk_effect_clear(int y, int x);
extern bool angtk_effect_spell(int y, int x, int typ, int bolt);
extern bool angtk_effect_ammo(int y, int x, object_type *o_ptr, int dir);
extern bool angtk_effect_object(int y, int x, object_type *o_ptr);
extern void angtk_invert_spot(int y, int x);
extern void angtk_detect_radius(int y, int x, int r);
extern void angtk_destroy_area(int arg);
extern void (*angtk_lite_spot)(int y, int x);
extern void angtk_lite_spot_real(int y, int x);
extern void angtk_wipe_spot(int y, int x);
extern void widget_invalidate_shape(int y, int x);
extern void angtk_idle(void);
extern void angtk_project_hint(int action, int rad, int y, int x, int flg);
extern void angtk_locate(int dir);

/* interp1.c */
extern cptr *keyword_gender;
extern cptr *keyword_race;
extern cptr *keyword_class;
#if defined(OANGBANDTK)
extern cptr keyword_shape[];
#endif /* */

/* Like object_attr, by k_info[] index */
#define kind_attr(k_idx) \
	((k_info[k_idx].flavor) ? \
	 (misc_to_attr[k_info[k_idx].flavor]) : \
	 (k_info[k_idx].x_attr))

extern char *g_attr_str;
extern cptr keyword_artifact_location[];
extern int debug_commands;
extern int exit_skip_save;
extern bool command_repeating;
extern bool mouse_running;
extern int g_cave_hgt, g_cave_wid;
extern void angtk_angband_initialized(void);
extern void angtk_character_generated(void);
extern void angtk_display_info_init(void);
extern void angtk_display_info_append(cptr s);
extern void angtk_display_info_done(cptr title);
extern void angtk_display_info(char *title, char **info, int count);
extern void angtk_eval(char *command, ...);
extern int angtk_eval_file(char *extFileName);
extern int angtk_generate(void);
extern void angtk_cave_generated(void);
extern void angtk_examine(int y, int x, char *out_val);
extern int angtk_find_artifact(int a_idx, object_type *o_ptr);
extern void angtk_health(char *buf);
extern int angtk_tval_string(cptr *str, int tval);
extern int angtk_tval_const(int *tval, cptr str);
extern int dump_object_info(char *varName, object_type *o_ptr, int index);
extern void angtk_init(void);
extern void angtk_exit(void);
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
extern bool player_test_feature(int y, int x, int mode);
#endif /* KANGBANDTK, ZANGBANDTK */
extern char *player_status(int status, int *value);

#if defined(ZANGBANDTK)
extern void blows_per_round(int *_blows, int *_muta_att);
extern void shots_per_round(int *shots, int *shots_frac);
#endif

#define TARGET_STATE_SET 0x0001 /* Target is set */
#define TARGET_STATE_VIS 0x0002 /* Target is visible */
#define TARGET_STATE_CHANGE 0x0004 /* Target changed */
extern int target_state;
extern bool target_vis;

#define PR_POSITION 0x20000000L /* p_ptr->redraw: player position changed */
#define PR_TARGET 0x40000000L /* p_ptr->redraw: target visibility changed */

/*
 * XXXXX Important!
 * If you add INKEY_XXX flags here, you must update the inkey_to_str[]
 * array in interp2.c, and keyword_inkey[] in bind.c, and the
 * KEYWORD_INKEY_XXX constants below.
 */
 
#define INKEY_CMD 1
#define INKEY_DIR 2
#define INKEY_DISTURB 3
#define INKEY_ITEM 4
#define INKEY_ITEM_STORE 5
#define INKEY_MORE 6
#define INKEY_SPELL 7
#define INKEY_TARGET 8
#if defined(KANGBANDTK)
#define INKEY_CMD_PET 9
#endif /* ZANGBANDTK */
#if defined(OANGBANDTK)
#define INKEY_ELE_ATTACK 9
#endif
#if defined(ZANGBANDTK)
#define INKEY_MINDCRAFT 9
#define INKEY_POWER 10
#define INKEY_CMD_PET 11
#endif /* ZANGBANDTK */

ANG_EXTERN int inkey_flags;
extern int inkey_book;
extern cptr inkey_options;

/* r_info.c */
extern long angtk_roff(int r_idx, char *buffer);

#if defined(KANGBANDTK) || defined(ZANGBANDTK)

typedef struct _buildingdata {
	bool inside;
	int building_loc;
	building_type *bldg;
} _buildingdata;

extern _buildingdata g_buildingdata;

#endif /* KANGBANDTK, ZANGBANDTK */

/* store.c */
typedef struct _storedata {
	bool shopping;
	int store_num;
	store_type *st_ptr;
	owner_type *ot_ptr;
} _storedata;
#if defined(__LCC__) && defined(BORG_BUILD)
extern _storedata *_imp__storedata;
#define storedata (*_imp__storedata)
#else
ANG_EXTERN _storedata storedata;
#endif
extern int dump_object_info_store(char *varName, object_type *o_ptr, int index);
extern bool store_will_buy(const object_type *o_ptr);

/* Event types */
#define EVENT_POSITION 1
#define EVENT_STAT 2
#define EVENT_TARGET 3
#define EVENT_STATUS 4
#define EVENT_INKEY 5
#define EVENT_CURSOR 6
#define EVENT_ASSIGN 7
#define EVENT_TERM 8
#define EVENT_CHOOSE 9
#define EVENT_TRACK 10
#define EVENT_PY 11
#define EVENT_SETTING 12
#define EVENT_DUNGEON 13
#define EVENT_KEYMAP 14
#define EVENT_ASK 15
#define EVENT_MOUSE_DISTURB 16

#define KEYWORD_INKEY_CMD 0
#define KEYWORD_INKEY_DIR 1
#define KEYWORD_INKEY_DISPLAY 2
#define KEYWORD_INKEY_ITEM 3
#define KEYWORD_INKEY_ITEM_STORE 4
#define KEYWORD_INKEY_MORE 5
#define KEYWORD_INKEY_SPELL 6
#define KEYWORD_INKEY_TARGET 7

enum {
KEYWORD_STATUS_CUT,
KEYWORD_STATUS_STUN,
KEYWORD_STATUS_HUNGER,
KEYWORD_STATUS_BLIND,
KEYWORD_STATUS_CONFUSED,
KEYWORD_STATUS_AFRAID,
KEYWORD_STATUS_POISONED,
KEYWORD_STATUS_STATE,
KEYWORD_STATUS_SPEED,
KEYWORD_STATUS_STUDY,
KEYWORD_STATUS_WINNER,
#if defined(OANGBANDTK)
KEYWORD_STATUS_SHAPE,
#endif /* */
KEYWORD_STATUS_MAX
};

#define KEYWORD_TARGET_SET 0
#define KEYWORD_TARGET_UNSET 1
#define KEYWORD_TARGET_VISIBILITY 2

#define KEYWORD_CURSOR_SHOW 0
#define KEYWORD_CURSOR_HIDE 1

#define KEYWORD_ASSIGN_MONSTER 0
#define KEYWORD_ASSIGN_OBJECT 1
#define KEYWORD_ASSIGN_CHARACTER 2
#define KEYWORD_ASSIGN_FEATURE 3
#define KEYWORD_ASSIGN_SHOPKEEPER 4
#define KEYWORD_ASSIGN_ARTIFACT 5

#define KEYWORD_TERM_FRESH 0
#define KEYWORD_TERM_INKEY 1

#define KEYWORD_CHOOSE_ITEM 0
#define KEYWORD_CHOOSE_SPELL 1
#if defined(KANGBANDTK)
#define KEYWORD_CHOOSE_CMD_PET 2
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
#define KEYWORD_CHOOSE_ELE_ATTACK 2
#endif
#if defined(ZANGBANDTK)
#define KEYWORD_CHOOSE_POWER 2
#define KEYWORD_CHOOSE_MINDCRAFT 3
#define KEYWORD_CHOOSE_CMD_PET 4
#endif /* ZANGBANDTK */

#define KEYWORD_TRACK_HEALTH 0
#define KEYWORD_TRACK_RACE 1
#define KEYWORD_TRACK_OBJECT 2
#define KEYWORD_TRACK_INVENTORY 3
#define KEYWORD_TRACK_EQUIPMENT 4
#define KEYWORD_TRACK_GRID 5
#define KEYWORD_TRACK_MESSAGE 6

#define KEYWORD_PY_HP 0
#define KEYWORD_PY_SP 1
#define KEYWORD_PY_FOOD 2
#define KEYWORD_PY_DEPTH 3
#define KEYWORD_PY_EXP 4
#define KEYWORD_PY_LEVEL 5
#define KEYWORD_PY_AC 6
#define KEYWORD_PY_NAME 7
#define KEYWORD_PY_TITLE 8
#define KEYWORD_PY_GOLD 9
#if defined(ZANGBANDTK)
#define KEYWORD_PY_RACE 10
#endif /* ZANGBANDTK */

#define KEYWORD_DUNGEON_ENTER 0
#define KEYWORD_DUNGEON_LEAVE 1
#define KEYWORD_DUNGEON_GEN 2

#define KEYWORD_ASK_QUANTITY 0
#define KEYWORD_ASK_YES_NO 1
#define KEYWORD_ASK_HAGGLE 2

/* bind.c */
extern cptr keyword_assign[];
extern cptr keyword_stat[];
extern cptr keyword_status[];
extern cptr keyword_target[];
extern cptr *keyword_setting;
extern void init_bindings(void);
extern void Bind_Ask(int detail, int min, int max, int show);
extern void Bind_Choose(int detail, int other, int show);
extern void Bind_Cursor(int detail, int y, int x);
extern void Bind_Generic(int eventType, int eventDetail);
extern void Bind_Keymap(int ch);
extern void Bind_Position(int who, int y1, int x1, int y2, int x2);
extern void Bind_Setting(int detail, int value);
extern void Bind_Status(int detail);
extern void Bind_Track(int detail, int who, int y, int x);
extern void Bind_Option(const char *name, int value);

/* birth-tnb.c */
extern void init_birth(void);

/* debug.c */
extern int wiz_see_monsters;
extern int wiz_auto_lite;
extern int wiz_free_moves;
extern void init_debug(void);

/* describe.c */
extern cptr keyword_slot[];
extern int strcpy_len(char *s1, const char *s2);
extern long angtk_describe_object(object_type *o_ptr, char *buf, bool in_store);
extern bool make_fake_artifact(object_type *o_ptr, int name1);
extern int SetArrayValueChar(char *varName, char *field, char value);
extern int SetArrayValueLong(char *varName, char *field, long value);
extern int SetArrayValueString(char *varName, char *field, char *value);

/* map.c */
extern void map_init(void);
extern void map_exit(void);
extern void map_symbol_set(int y, int x);
extern char *map_symbol_name(int symbolIndex);
extern int map_symbol_feature(int f_idx);

/* savefile.c */
extern errr angtk_savefile_info(char *fileName, char *varName);

/* setting.c */
extern void settings_init(void);
extern void settings_exit(void);

/* struct.c */
extern void struct_init(void);
extern void struct_exit(void);

#if 0
/* tkterm.c */
extern int TkTerm_Init(Tcl_Interp *interp);
#endif

/* town.c */
extern void vault_init(void);
extern void vault_setup(void);
extern void vault_unload(void);
extern void vault_exit(void);

/* treectrl.c */
extern int TreeCtrl_Init(void);
extern void TreeCtrl_Unload(void);
extern void TreeCtrl_Setup(void);
extern void TreeCtrl_Idle(void);
extern void TreeCtrl_Exit(void);

/* util-tnb.c */
#ifdef BORG_TK
ANG_EXTERN char angtk_prompt_text[256];
#endif /* not BORG_TK */
extern cptr keyword_term_color[];
extern byte g_prompt_attr;
extern void prompt_print(cptr str);
extern void prompt_erase(void);
extern void prompt_format(cptr fmt, ...);
extern void prompt_append(cptr str);
extern void prompt_open(cptr str);
extern void prompt_update(cptr str);
extern void any_more(cptr prompt);

extern int ExtToUtf_SetArrayValueString(char *varName, char *field, char *value);

extern void aim_dir_preserve(int dir);
extern void aim_dir_maintain(void);
extern void aim_dir_finish(void);


/* widget.c */
extern int widget_init(void);
extern void widget_exit(void);
extern void widget_setup(void);

/*
 * Option: Use a fast file buffer when writing/reading savefiles.
 */
#define ALLOW_SAVEFILE_BUFFER

/* Size of temp savefile buffer */
#define SAVEFILE_BUFFER_MAX (30L * 1024L)

/* Version number for savefile extension */
#define VERSION_MAJOR_TNB 1
#define VERSION_MINOR_TNB 0

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
# if defined(TNB_SQUELCH)
#  define VERSION_PATCH_TNB 1
# else
#  define VERSION_PATCH_TNB 0
# endif
#endif
#if defined(OANGBANDTK)
#define VERSION_PATCH_TNB 1
#endif

#if defined(ZANGBANDTK)

/*
 * Hack -- When using the Trump Tower to teleport to a level, ignore the
 * remember_recall option
 */
extern bool wor_trump_hack;

/* Constants for racial and mutation powers */
enum {

/* Racial power */
POWER_DWARF,
POWER_HOBBIT,
POWER_GNOME,
POWER_HALF_ORC,
POWER_HALF_TROLL,
POWER_AMBERITE_A,
POWER_AMBERITE_B,
POWER_BARBARIAN,
POWER_HALF_OGRE,
POWER_HALF_GIANT,
POWER_HALF_TITAN,
POWER_CYCLOPS,
POWER_YEEK,
POWER_KLACKON,
POWER_KOBOLD,
POWER_NIBELUNG,
POWER_DARK_ELF,
POWER_DRACONIAN,
POWER_MIND_FLAYER,
POWER_IMP,
POWER_GOLEM,
POWER_SKELETON,
POWER_ZOMBIE,
POWER_VAMPIRE,
POWER_SPECTRE,
POWER_SPRITE,

/* Mutation power */
POWER_SPIT_ACID,
POWER_BR_FIRE,
POWER_HYPN_GAZE,
POWER_TELEKINES,
POWER_VTELEPORT,
POWER_MIND_BLST,
POWER_RADIATION,
POWER_VAMPIRISM,
POWER_SMELL_MET,
POWER_SMELL_MON,
POWER_BLINK,
POWER_EAT_ROCK,
POWER_SWAP_POS,
POWER_SHRIEK,
POWER_ILLUMINE,
POWER_DET_CURSE,
POWER_BERSERK,
POWER_POLYMORPH,
POWER_MIDAS_TCH,
POWER_GROW_MOLD,
POWER_RESIST,
POWER_EARTHQUAKE,
POWER_EAT_MAGIC,
POWER_WEIGH_MAG,
POWER_STERILITY,
POWER_PANIC_HIT,
POWER_DAZZLE,
POWER_LASER_EYE,
POWER_RECALL,
POWER_BANISH,
POWER_COLD_TOUCH,
POWER_LAUNCHER,

/* Max number of power's */
MAX_POWER
};

extern cptr power_desc[MAX_POWER];
extern int get_powers(int *power);

#endif /* ZANGBANDTK */

#ifdef ALLOW_EASY_FLOOR
extern int floor_y, floor_x;
#endif /* ALLOW_EASY_FLOOR */

#define ALLOW_STATUS_EXTRA

#ifdef ALLOW_STATUS_EXTRA

enum {
	PR_BLESSED,
	PR_HERO,
	PR_SHERO,
	PR_OPPOSE_ACID,
	PR_OPPOSE_COLD,
	PR_OPPOSE_ELEC,
	PR_OPPOSE_FIRE,
	PR_OPPOSE_POIS,
	PR_PROTEVIL,
	PR_SHIELD,
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	PR_INVULN,
#endif /* */
	PR_FAST,
	PR_SLOW,
	PR_TIM_INFRA,
	PR_SEE_INVIS,
	PR_RECALL,
	PR_IMAGE,
#if defined(KANGBANDTK)
	PR_ANCHOR,
	PR_GHOST,
	PR_INVIS,
	PR_OPPOSE_CC,
	PR_OPPOSE_LD,
	PR_OPPOSE_SS,
	PR_OPPOSE_NETH,
	PR_OPPOSE_NEX,
	PR_LEVITATE,
	PR_SUS_STR,
	PR_SUS_INT,
	PR_SUS_WIS,
	PR_SUS_DEX,
	PR_SUS_CON,
	PR_SUS_CHR,
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
	PR_ELE_ATTACK,
	PR_ESP,
	PR_MAGICDEF,
	PR_STEALTH,
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
	PR_TIM_ESP,
	PR_WRAITH,
#endif /* ZANGBANDTK */
	PR_MAX
};

enum {
KEYWORD_STATUS_EXTRA = KEYWORD_STATUS_MAX
};

extern void redraw_init(void);
extern void redraw_flush(void);
extern void redraw_add(int pr);

#endif /* ALLOW_STATUS_EXTRA */

#endif /* _INCLUDE_TNB_H_ */
