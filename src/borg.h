/* File: borg.h */

/* Purpose: Header file for the Borg -BEN- */

#ifndef INCLUDED_BORG_H
#define INCLUDED_BORG_H

#include "angband.h"


#ifdef ALLOW_BORG


/*
 * This file is the primary header file for the Borg
 *
 * It defines constants, macros, types, variables, and functions available
 * to all of the "borg" source files. 
 *
 * Note that the standard "angband.h" header file is includes by this header
 * file, which makes many things available to the Borg, only some of which
 * should actually be used.
 */



/***** Available constants *****/


/*
 * Maximum size of the "view" array
 */
#define BORG_VIEW_MAX 1536


/*
 * Number of grids in the "temp" array
 */
#define BORG_TEMP_MAX 2048


/*
 * Number of grids in the "hack" array
 */
#define BORG_HACK_MAX 2048


/*
 * Number of grids in the "flow" array
 */
#define BORG_FLOW_MAX 2048


/*
 * Number of shops in the "borg_track_shop" array
 */
#define BORG_MAX_SHOP 8


/*
 * Number of stairs up in the "borg_track_less" array
 */
#define BORG_MAX_LESS 16


/*
 * Number of stairs up in the "borg_track_more" array
 */
#define BORG_MAX_MORE 16


/*
 * Number of takes in the "borg_takes" array
 */
#define BORG_MAX_TAKE 256


/*
 * Number of kills in the "borg_kills" array
 */
#define BORG_MAX_KILL 256


/*
 * Size of Keypress buffer
 */
#define BORG_KEY_SIZE 8192




/*
 * Flags for the "info" field of grids
 *
 * Some of these flags are very much like the ones used in "cave_type".
 *
 * Note that some of the flags below are not "perfect", in particular,
 * several of the flags should be treated as "best guesses", see below.
 *
 * The "CAVE_MARK" flag means that the grid has been "observed", though
 * the terrain feature may or may not be memorized.  Note the use of the
 * "FEAT_NONE", "FEAT_FLOOR", and "FEAT_INVIS" feature codes below.
 *
 * The "CAVE_GLOW" flag means that a grid is probably "perma-lit", but
 * sometimes it is really only "recently" perma-lit, but was then made
 * dark with a darkness attack, and it is now torch-lit or off-screen.
 *
 * The "CAVE_DARK" flag means that a grid is probably not "perma-lit",
 * but sometimes it is really only "recently" dark, but was then made
 * "lite" with a "call lite" spell, and it is now torch-lit or off-screen.
 *
 * The "CAVE_HACK" flag means that a grid is a "lit" floor grid, and
 * if it is not lit by the player's torch, then it is perma-lit.
 *
 * The "CAVE_SEEN" flag means that a grid is probably viewable by the
 * player, but this may not be true if the nearby "CAVE_VIEW" and/or
 * "CAVE_GLOW" flags are not correct, or the "lite radius" has changed
 * recently, or various other things are true.
 *
 * The "CAVE_VIEW" flag means that a grid is probably in line of sight
 * of the player, but this may not be true if some of the grids between
 * the player and the grid contain previously unseen walls/doors/etc.
 *
 * The "CAVE_TEMP" flag means that a grid has been added to the array
 * "borg_temp_x"/"borg_temp_y", though normally we ignore this flag.
 *
 * The "CAVE_WALL" flag means that the grid blocks line of sight.
 */


/*
 * Some new names for some useless cave flags XXX XXX XXX
 */

#define CAVE_DARK	CAVE_ICKY
#define CAVE_HACK	CAVE_ROOM


/*
 * Spell method values
 */

#define BORG_MAGIC_ICK		0	/* Spell is illegible */
#define BORG_MAGIC_NOP		1	/* Spell takes no arguments */
#define BORG_MAGIC_EXT		2	/* Spell has "detection" effects */
#define BORG_MAGIC_AIM		3	/* Spell requires a direction */
#define BORG_MAGIC_OBJ		4	/* Spell requires a pack object */
#define BORG_MAGIC_WHO		5	/* Spell requires a monster symbol */


/*
 * Spell status values
 */

#define BORG_MAGIC_ICKY		0	/* Spell is illegible */
#define BORG_MAGIC_LOST		1	/* Spell is forgotten */
#define BORG_MAGIC_HIGH		2	/* Spell is high level */
#define BORG_MAGIC_OKAY		3	/* Spell is learnable */
#define BORG_MAGIC_TEST		4	/* Spell is untried */
#define BORG_MAGIC_KNOW		5	/* Spell is known */


/*
 * Possible values of "borg_task"
 */
#define GOAL_KILL	1		/* Monsters */
#define GOAL_TAKE	2		/* Objects */
#define GOAL_MISC	3		/* Stores */
#define GOAL_DARK	4		/* Exploring */
#define GOAL_XTRA	5		/* Searching */
#define GOAL_BORE	6		/* Leaving */
#define GOAL_FLEE	7		/* Fleeing */


/*
 * Minimum "harmless" food
 */

#define SV_FOOD_MIN_OKAY	SV_FOOD_CURE_POISON


/*
 * Hack -- location of the "Lv Mana Fail" prompt
 */
#define ROW_SPELL	1
#define COL_SPELL	20+35



/***** Available macros *****/


/*
 * Determine "twice" the distance between two points
 * This results in "diagonals" being "correctly" ranged,
 * that is, a diagonal appears "furthur" than an adjacent.
 */
#define double_distance(Y1,X1,Y2,X2) \
	(distance(((int)(Y1))<<1,((int)(X1))<<1,((int)(Y2))<<1,((int)(X2))<<1))

/*
 * Determine if a grid is a floor grid
 */
#define borg_cave_floor_bold(Y,X) \
	(!(borg_cave_feat[Y][X] & 0x20))


/*
 * Determine if a grid is on the current panel
 */
#define borg_panel_contains(Y,X) \
    (((unsigned)((Y) - b_ptr->wy) < (unsigned)(SCREEN_HGT)) && \
     ((unsigned)((X) - b_ptr->wx) < (unsigned)(SCREEN_WID)))



/***** Available types *****/


/*
 * Grid information
 */

typedef struct auto_wank auto_wank;

struct auto_wank
{
	byte x;
	byte y;

	byte t_a;
	char t_c;

	bool is_take;
	bool is_kill;
};


/*
 * Object information
 */

typedef struct auto_take auto_take;

struct auto_take
{
	s16b	k_idx;		/* Kind index */

	byte	t_a;		/* Last attr */
	byte	t_c;		/* Last char */

	bool	known;		/* Verified kind */

	bool	seen;		/* Assigned motion */

	byte	x, y;		/* Location */

	s16b	when;		/* When last seen */
};


/*
 * Monster information
 */

typedef struct auto_kill auto_kill;

struct auto_kill
{
	s16b	r_idx;		/* Race index */

	byte	t_a;		/* Last attr */
	byte	t_c;		/* Last char */

	bool	known;		/* Verified race */
	bool	awake;		/* Probably awake */

	bool	seen;		/* Assigned motion */
	bool	used;		/* Assigned message */

	byte	x, y;		/* Location */

	byte	ox, oy;		/* Old location */

	byte	speed;		/* Estimated speed */
	byte	moves;		/* Estimates moves */

	s16b	power;		/* Estimated hit-points */
	s16b	other;		/* Estimated something */

	s16b	when;		/* When last seen */
};



/*
 * A structure holding information about an object.
 *
 * The "note" field should only be used during the analysis phase.
 *
 * The "iqty" is zero if the object is "missing"
 * The "kind" is zero if the object is "unaware" (or missing)
 * The "able" is zero if the object is "unknown" (or unaware or missing)
 *
 * Note that unaware items will have a "tval" but an invalid "sval".
 */

typedef struct auto_item auto_item;

struct auto_item
{
	char desc[80];	/* Actual Description */

	cptr note;		/* Inscription (or empty string) */

	bool able;		/* True if item is identified */

	bool average;	/* True if item is average */
	bool blessed;	/* True if item is good or better */
	bool special;	/* True if item is special or terrible */

	bool tried;		/* True if item is marked tried */
	bool empty;		/* True if item is marked empty */

	s16b kind;		/* Kind index */

	byte tval;		/* Item type */
	byte sval;		/* Item sub-type */
	s16b pval;		/* Item extra-info */

	byte discount;	/* Discount */

	byte iqty;		/* Number of items */

	s16b weight;	/* Predicted weight */

	byte name1;		/* Artifact index (if any) */
	byte name2;		/* Ego-item index (if any) */

	s16b timeout;	/* Timeout counter XXX XXX */

	s16b to_h;		/* Bonus to hit */
	s16b to_d;		/* Bonus to dam */
	s16b to_a;		/* Bonus to ac */
	s16b ac;		/* Armor class */
	byte dd;		/* Damage dice */
	byte ds;		/* Damage sides */

	bool do_star;	/* Use *identify*, step 1 */
	bool do_exam;	/* Use *identify*, step 2 */

	s32b cost;		/* Cost (in stores) */

	s32b value;		/* Value (estimated) */

	u32b flags1;	/* Extracted item flags (set 1) */
	u32b flags2;	/* Extracted item flags	(set 2) */
	u32b flags3;	/* Extracted item flags	(set 3) */
};


/*
 * A shop
 */

typedef struct auto_shop auto_shop;

struct auto_shop
{
	s16b when;		/* Time stamp */

	s16b xtra;		/* Something unused */

	s16b page;		/* Current page */
	s16b more;		/* Number of pages */

	auto_item ware[24];	/* Store contents */
};



/*
 * A spell/prayer in a book
 */

typedef struct auto_magic auto_magic;

struct auto_magic
{
	cptr name;		/* Textual name */

	byte status;	/* Status (see above) */

	byte method;	/* Method (see above) */

	byte rating;	/* Usefulness */

	byte level;		/* Required level */

	byte power;		/* Required power */

	byte cheat;		/* Actual "spell index" (or 99) */
};



/*
 * Extra player information
 */

typedef struct player_xtra player_xtra;

struct player_xtra {

	s16b gy;					/* Goal location */
	s16b gx;					/* Goal location */
	
	s16b old_depth;				/* Previous depth */

	s16b old_chp;				/* Previous hit points */
	s16b old_csp;				/* Previous spell points */

	s16b old_px;				/* Previous location */
	s16b old_py;				/* Previous location */

	s16b old_wx;				/* Previous panel */
	s16b old_wy;				/* Previous panel */

	s16b ammo_sides;			/* Ammo -- "sides" */
	s16b ammo_range;			/* Ammo -- "range" */

	s16b need_enchant_to_a;		/* Needed enchantment */
	s16b need_enchant_to_h;		/* Needed enchantment */
	s16b need_enchant_to_d;		/* Needed enchantment */
};


/*
 * String analysis information
 */

typedef struct player_anal player_anal;

struct player_anal {

	int single_num;		/* Number of "singles" */
	s16b *single_what;		/* Kind indexes for "singles" */
	cptr *single_text;		/* Textual prefixes for "singles" */

	int plural_num;		/* Number of "plurals" */
	s16b *plural_what;		/* Kind index for "plurals" */
	cptr *plural_text;		/* Textual prefixes for "plurals" */

	int artego_num;		/* Number of "artegos" */
	s16b *artego_what;		/* Indexes for "artegos" */
	cptr *artego_text;		/* Textual prefixes for "artegos" */

	int unique_num;		/* Number of uniques */
	s16b *unique_what;		/* Indexes of uniques */
	s16b *unique_size;		/* Length of uniques */
	cptr *unique_text;		/* Names of uniques */

	int normal_num;		/* Number of normals */
	s16b *normal_what;		/* Indexes of normals */
	s16b *normal_size;		/* Length of normals */
	cptr *normal_text;		/* Names of normals */
};



/*** Available variables ***/


/* borg1.c */
extern s16b borg_active;
extern bool borg_prompt;
extern char *borg_key_queue;
extern s16b borg_key_head;
extern s16b borg_key_tail;
extern FILE *borg_fff;
extern char borg_match_string[128];
extern bool borg_flag_save_level;
extern bool borg_flag_save_depth;
extern bool borg_rand_quick;
extern u32b borg_rand_value;
extern u32b borg_rand_local;
extern s16b borg_time;
extern s16b borg_when_began;
extern s16b borg_when_call_lite;
extern s16b borg_when_wizard_lite;
extern s16b borg_when_detect_traps;
extern s16b borg_when_detect_doors;
extern s16b borg_when_detect_walls;
extern s16b borg_task;
extern s16b borg_avoid;
extern s16b borg_boost;
extern bool borg_rising;
extern bool borg_leaving;
extern bool borg_fleeing;
extern bool borg_ignoring;
extern bool borg_recalling;
extern bool borg_completed;
extern bool borg_stair_less;
extern bool borg_stair_more;
extern s16b borg_goal_shop;
extern s16b borg_goal_ware;
extern s16b borg_goal_item;
extern s16b borg_exam_item;
extern char borg_exam_note[32];
extern bool borg_need_save;
extern sint amt_fuel;
extern sint amt_food;
extern sint amt_ident;
extern sint amt_recall;
extern sint amt_phase;
extern sint amt_escape;
extern sint amt_teleport;
extern sint amt_cure_critical;
extern sint amt_cure_serious;
extern sint amt_detect_trap;
extern sint amt_detect_door;
extern sint amt_missile;
extern sint amt_book[9];
extern sint amt_add_stat[6];
extern sint amt_fix_stat[6];
extern sint amt_fix_exp;
extern sint amt_enchant_to_a;
extern sint amt_enchant_to_d;
extern sint amt_enchant_to_h;
extern sint num_fuel;
extern sint num_food;
extern sint num_ident;
extern sint num_recall;
extern sint num_phase;
extern sint num_escape;
extern sint num_teleport;
extern sint num_cure_critical;
extern sint num_cure_serious;
extern sint num_missile;
extern sint num_book[9];
extern sint num_fix_stat[6];
extern sint num_fix_exp;
extern sint num_enchant_to_a;
extern sint num_enchant_to_d;
extern sint num_enchant_to_h;
extern int borg_base_depth;
extern int borg_base_level;
extern s32b borg_base_exp;
extern s32b borg_base_au;
extern int borg_base_pspeed;
extern int borg_base_ac;
extern int borg_base_chp;
extern int borg_base_mhp;
extern int borg_base_csp;
extern int borg_base_msp;
extern int borg_base_stat[6];
extern bool borg_stat_max[6];
extern int borg_base_book[9];
extern int borg_base_shop;
extern int borg_base_wgt;
extern bool borg_base_is_weak;
extern bool borg_base_is_hungry;
extern bool borg_base_is_full;
extern bool borg_base_is_gorged;
extern bool borg_base_is_blind;
extern bool borg_base_is_afraid;
extern bool borg_base_is_confused;
extern bool borg_base_is_poisoned;
extern bool borg_base_is_cut;
extern bool borg_base_is_stun;
extern bool borg_base_is_image;
extern bool borg_base_is_study;
extern bool borg_base_fix_lev;
extern bool borg_base_fix_exp;
extern bool borg_base_fix_stat[6];
extern int borg_feeling;
extern int borg_happy_depth;
extern int borg_happy_count;
extern s32b borg_base_power;
extern bool borg_failure;
extern bool borg_simulate;
extern bool borg_expected;
extern player_type *b_ptr;
extern player_xtra *xb_ptr;
extern player_race *rb_ptr;
extern player_class *cb_ptr;
extern player_magic *mb_ptr;
extern player_anal *ab_ptr;
extern s16b borg_msg_len;
extern s16b borg_msg_siz;
extern char *borg_msg_buf;
extern s16b borg_msg_num;
extern s16b borg_msg_max;
extern s16b *borg_msg_pos;
extern s16b *borg_msg_use;
extern bool borg_detect_wall[6][6];
extern bool borg_detect_trap[6][6];
extern bool borg_detect_door[6][6];
extern byte *borg_track_shop_x;
extern byte *borg_track_shop_y;
extern s16b borg_track_less_num;
extern byte *borg_track_less_x;
extern byte *borg_track_less_y;
extern s16b borg_track_more_num;
extern byte *borg_track_more_x;
extern byte *borg_track_more_y;
extern s16b borg_wank_num;
extern auto_wank *borg_wanks;
extern s16b borg_takes_cnt;
extern s16b borg_takes_nxt;
extern auto_take borg_takes[BORG_MAX_TAKE];
extern s16b borg_kills_cnt;
extern s16b borg_kills_nxt;
extern auto_kill borg_kills[BORG_MAX_KILL];
extern u16b borg_fear_region[6][18];
extern s16b *borg_race_count;
extern s16b *borg_race_death;
extern byte *borg_char_feat;
extern bool *borg_char_is_take;
extern bool *borg_char_is_kill;
extern sint borg_view_n;
extern u16b *borg_view_g;
extern sint borg_temp_n;
extern u16b *borg_temp_g;
extern byte *borg_temp_y;
extern byte *borg_temp_x;
extern sint borg_hack_n;
extern u16b *borg_hack_g;
extern byte *borg_hack_y;
extern byte *borg_hack_x;
extern sint borg_flow_head;
extern sint borg_flow_tail;
extern u16b *borg_flow_g;
extern byte *borg_flow_y;
extern byte *borg_flow_x;
extern byte (*borg_cave_info)[256];
extern byte (*borg_cave_feat)[256];
extern byte (*borg_cave_o_idx)[DUNGEON_WID];
extern byte (*borg_cave_m_idx)[DUNGEON_WID];
extern byte (*borg_cave_search)[DUNGEON_WID];
extern s16b (*borg_cave_danger)[DUNGEON_WID];
extern byte (*borg_flow_cost)[DUNGEON_WID];
extern byte (*borg_flow_work)[DUNGEON_WID];
extern bool borg_do_wipe_danger;
extern bool borg_do_update_view;
extern bool borg_do_inven;
extern bool borg_do_equip;
extern bool borg_do_panel;
extern bool borg_do_frame;
extern bool borg_do_spell;
extern byte borg_do_spell_aux;
extern bool borg_do_browse;
extern byte borg_do_browse_what;
extern byte borg_do_browse_more;
extern bool borg_do_crush_junk;
extern bool borg_do_crush_hole;
extern bool borg_do_crush_slow;
extern auto_item *borg_items;
extern auto_shop *borg_shops;
extern auto_item *borg_safe_items;
extern auto_shop *borg_safe_shops;
extern auto_magic borg_magics[9][9];
extern const term *borg_term_pointer;



/*** Available functions ***/


/* borg1.c */
extern errr borg_what_char(int x, int y, byte *a, char *c);
extern errr borg_what_text(int x, int y, int n, byte *a, char *s);
extern void borg_steal_inventory_weight(char *buf);
extern void borg_steal_inventory_desc(char *buf, int item);
extern void borg_steal_spell_info(char *buf, int book, int what);
extern void borg_steal_panel_info(char *buf);
extern void borg_info(cptr what);
extern void borg_note(cptr what);
extern errr borg_keypress(char k);
extern errr borg_keypresses(cptr str);
extern char borg_inkey(bool take);
extern void borg_flush(void);
extern void borg_oops(cptr what);


/* borg2.c */
extern bool borg_make_note(cptr what);
extern bool borg_change_name(cptr str);
extern bool borg_dump_character(cptr str);
extern bool borg_save_game(void);
extern void borg_update_frame(void);
extern bool borg_los(int y1, int x1, int y2, int x2);
extern sint borg_project_path(u16b *gp, int range, \
                              int y1, int x1, int y2, int x2, int flg);
extern bool borg_projectable(int y1, int x1, int y2, int x2);
extern errr borg_vinfo_init(void);
extern void borg_forget_view(void);
extern void borg_update_view(void);


/* borg3.c */
extern int borg_wield_slot(auto_item *item);
extern int borg_count(int tval, int sval);
extern int borg_slot(int tval, int sval);
extern void borg_item_analyze(auto_item *item, cptr desc);
extern void borg_send_item_index(int i);
extern void borg_send_inscribe_item(int i, cptr str);
extern void borg_send_uninscribe_item(int i);
extern bool borg_refuel_torch(void);
extern bool borg_refuel_lantern(void);
extern bool borg_eat_food(int sval);
extern bool borg_quaff_potion(int sval);
extern bool borg_read_scroll(int sval);
extern bool borg_zap_rod(int sval);
extern bool borg_aim_wand(int sval);
extern bool borg_use_staff(int sval);
extern bool borg_activate_dragon(int sval);
extern bool borg_activate_artifact(int name1);
extern bool borg_spell_legal(int book, int what);
extern bool borg_spell_okay(int book, int what);
extern bool borg_spell(int book, int what);
extern bool borg_prayer_legal(int book, int what);
extern bool borg_prayer_okay(int book, int what);
extern bool borg_prayer(int book, int what);
extern void borg_watch_equip(int which);
extern void borg_watch_inven(int which);
extern void borg_parse_equip(void);
extern void borg_parse_inven(void);
extern void borg_parse_spell(int book);
extern void borg_prepare_book(int book);


/* borg4.c */
extern void borg_notice(void);
extern void borg_notice_home(void);
extern s32b borg_power(void);
extern s32b borg_power_home(void);
extern int borg_danger_aux(int y, int x, int c, int i);
extern int borg_danger(int y, int x, int c);
extern bool borg_restock(void);
extern bool borg_prepared(int depth);


/* borg5.c */
extern void borg_forget_map(void);
extern void borg_update_map(void);
extern void borg_delete_kill(int i);
extern void borg_update(void);
extern void borg_react(cptr msg, cptr buf);


/* borg6.c */
extern bool borg_recall(void);
extern bool borg_caution(void);
extern bool borg_attack(void);
extern bool borg_recover(void);
extern bool borg_charge_kill(void);
extern bool borg_charge_take(void);
extern bool borg_twitchy(void);
extern bool borg_flow_old(int why);
extern bool borg_flow_stair_both(int why);
extern bool borg_flow_stair_less(int why);
extern bool borg_flow_stair_more(int why);
extern bool borg_flow_shop_visit(void);
extern bool borg_flow_shop_entry(int n);
extern bool borg_flow_kill(bool viewable);
extern bool borg_flow_take(bool viewable);
extern bool borg_flow_dark(bool nearby);
extern bool borg_flow_spastic(bool bored);


/* borg7.c */
extern bool borg_item_icky(auto_item *item);
extern bool borg_use_things(void);
extern bool borg_check_lite(void);
extern bool borg_enchanting(void);
extern bool borg_recharging(void);
extern bool borg_crush_junk(void);
extern bool borg_crush_hole(void);
extern bool borg_crush_slow(void);
extern bool borg_test_stuff(void);
extern bool borg_star_stuff(void);
extern bool borg_exam_stuff(void);
extern bool borg_takeoff_stuff(void);
extern bool borg_swap_rings(void);
extern bool borg_wear_rings(void);
extern bool borg_wear_stuff(void);
extern bool borg_best_stuff(void);
extern bool borg_study_magic(bool bored);
extern bool borg_play_magic(void);
extern bool borg_leave_level(bool bored);
extern bool borg_think_store(void);
extern bool borg_think_dungeon(void);


/* borg8.c */
extern void borg_think(void);
extern void borg_parse(cptr msg);


/* borg9.c */
extern void do_cmd_borg(void);


#endif /* ALLOW_BORG */

#endif /* INCLUDED_BORG_H */

