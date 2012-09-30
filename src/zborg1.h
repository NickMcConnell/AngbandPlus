
/* File: zborg1.h */
/* Purpose: Header file for "borg1.c" -BEN- */

#ifndef INCLUDED_BORG1_H
#define INCLUDED_BORG1_H

#include "angband.h"
#include "maid-grf.h"



#ifdef ALLOW_BORG


/*
 * This file provides support for "borg1.c".
 */



/*** Some constants ***/

/*
 * Possible values of "goal"
 */
#define GOAL_NONE   0			/* No goal */
#define GOAL_KILL   1			/* Monsters */
#define GOAL_TAKE   2			/* Objects */
#define GOAL_FLEE   3			/* Fleeing */
#define GOAL_SHOP   4			/* Stores */
#define GOAL_DARK   5			/* Exploring */
#define GOAL_XTRA   6			/* Searching */
#define GOAL_BORE   7			/* Leaving */
#define GOAL_TOWN	8			/* Town Special Grid */
#define GOAL_FEAT	9			/* Getting of painful feat */
#define GOAL_CAVE	10			/* Reach a dungeon */
#define GOAL_MAX	11

/*
 * Flags for the "info" field of grids
 *
 * "BORG_MAP_VIEW"  This is the equivalent of GRID_VIEW,
 * calculated with the best information available to the
 * borg.
 *
 * "BORG_MAP_ICKY"  These are grids not to flow over.
 *
 * "BORG_MAP_KNOW"  This marks grids already in the flow,
 * and already tested for 'ickyness'.  This is done for
 * efficiency.
 */
#define BORG_MAP_VIEW   0x01	/* in line of sight */
#define BORG_MAP_ICKY	0x02	/* grids to avoid */
#define BORG_MAP_KNOW	0x04	/* 'know' grids */



/* Flags used to mark detection */
#define BORG_DETECT_TRAP	0x01
#define BORG_DETECT_DOOR	0x02
#define BORG_DETECT_WALL	0x04
#define BORG_DETECT_EVIL	0x08


/*
 * Borg detection radius.
 *
 * This is smaller than the actual detection radius because
 * we don't want the borg to walk into undetected regions.
 */
#define BORG_MAX_DETECT		(MAX_DETECT - 2)

/*
 * Some assistance with the borg_attack and magic arrows
 */
#define GF_ARROW_FLAME		93
#define GF_ARROW_FROST		94
#define GF_ARROW_SHOCKING	95
#define GF_ARROW_ANIMAL		96
#define GF_ARROW_DRAGON		97
#define GF_ARROW_EVIL		98
#define GF_ARROW_EXPLOSION	99

#define GF_HOLY_WORD		100
#define GF_DISP_UNDEAD_DEMON 101	/* effect both */
#define GF_ELEMENTS			102	/* all elements could be cast */
#define GF_DEATHRAY			103

/* values for the successful_target global */
#define BORG_TARGET			-1
#define BORG_FRESH_TARGET	0
#define BORG_ARROW_TARGET	5


/*
 * Maximum size of the "view" array
 */
#define AUTO_VIEW_MAX 1536


/*
 * Number of grids in the "temp" array
 */
#define BORG_TEMP_MAX 1536


/*
 * Number of grids in the "borg_next" array
 */
#define BORG_NEXT_MAX 8


/*
 * Number of grids in the "flow" array
 */
#define BORG_FLOW_MAX 2000


/*
 * Size of Keypress buffer
 */
#define KEY_SIZE 8192

/* Maximum number of 'takes' */
#define BORG_TAKES_MAX	1024
#define BORG_KILLS_MAX	1024

/*
 * Object information
 */
typedef struct borg_take borg_take;

struct borg_take
{
	s16b k_idx;	/* Kind index */

	/* Location */
	s16b x;
	s16b y;

	char unknown;	/* Unknown type */
};

/*
 * Monster information
 */
typedef struct borg_kill borg_kill;

struct borg_kill
{
	s16b r_idx;	/* Race index */

	s16b power;	/* Estimated hit-points */
	s32b when;	/* When last seen */

	/* Location */
	s16b x;
	s16b y;

	bool ranged_attack;	/* can attack from a dx */

	byte m_flags;

	byte type;	/* Type of kill */
};


/*
 * A store
 */
typedef struct borg_shop borg_shop;

struct borg_shop
{
	/* Location */
	s16b x;
	s16b y;

	/* Town */
	int town_num;

	/* Time stamp */
	s32b when;
	bool visit;

	/* Is this shop useful? */
	char type;
	s16b b_count;
	s16b u_count;
};


/*
 * A dungeon
 */
typedef struct borg_dungeon borg_dungeon;

struct borg_dungeon
{
	/* Location */
	s16b x;
	s16b y;

	/* depth */
	s16b min_depth;
	s16b max_depth;

	bool bottom;
};

/*
 * A town
 */
typedef struct borg_town borg_town;

struct borg_town
{
	/* Location */
	s16b x;
	s16b y;

	/* name */
	char name[T_NAME_LEN];

	/* Was the borg here? */
	bool visit;
};

/* Max size for the wilderness */
#define BORG_MAX_WILD_SIZE	(max_wild * WILD_BLOCK_SIZE)

/* Maximal distance the borg can travel between dungeons */
#define BORG_MAX_DISTANCE	(BORG_MAX_WILD_SIZE * 3 / 2)

/* Small distance in the wilderness (when the borg is close enough) */
#define BORG_SMALL_DISTANCE	96

/*
 * Some variables
 */
extern bool borg_active;	/* Actually active */
extern bool borg_cancel;	/* Being cancelled */

extern bool borg_dont_react;
extern int successful_target;

/*
 * Borg-abilities
 */
typedef struct borg_ability borg_ability;

struct borg_ability
{
	s16b phase;
	s16b teleport;
	s16b teleport_level;
	s16b teleport_away;
	s16b escape;
	s16b fuel;

	s16b heal;
	s16b easy_heal;
	s16b id;
	s16b id_item;
	s16b star_id;
	s16b star_id_item;
	s16b berserk;
	s16b speed;

	s16b staff_magi;
	s16b staff_dest;
	s16b staff_cool;
	s16b missile;

	s16b cure_pois;
	s16b cure_blind;
	s16b cure_conf;

	s16b det_trap;
	s16b det_door;
	s16b det_evil;
	s16b magic_map;
	s16b lite;

	s16b recharge;
	s16b remove_curse;
	s16b remove_curse_item;
	s16b star_remove_curse;
	s16b star_remove_curse_item;
	s16b glyph;

	s16b ccw;
	s16b csw;
	s16b clw;
	s16b res_heat;
	s16b res_cold;
	s16b res_all;

	s16b death;
	s16b poison;
	s16b mana;
	s16b logrus;
	s16b genocide;
	s16b mass_genocide;
	s16b invulnerability;
	s16b artifact;
	s16b artify_item;
	s16b acquire;
	s16b mundane;

	s16b bolt;
	s16b ball;
};

/*
 * Borg status
 */
typedef struct borg_status borg_status;

struct borg_status
{
	/* Food status */
	bool weak;
	bool hungry;
	bool full;
	bool gorged;

	/* Various status */
	bool blind;
	bool afraid;
	bool confused;
	bool poisoned;
	bool cut;
	bool image;
	bool study;
	bool search;

	/* Stun */
	bool stun;
	bool heavy_stun;

	/* Draining */
	bool fixlvl;
	bool fixexp;
	bool fixstat[A_MAX];	/* Fix stats */
	
	/* Heavy stuff */
	bool hvy_weapon;

	/* Cursedness */
	bool cursed;
	bool heavy_curse;
};


/*
 * Borg-player information
 */
typedef struct borg_player borg_player;

struct borg_player
{
	/* Abilities */
	borg_ability able;

	/* Status */
	borg_status status;

	/* Sustains */
	bool sust[A_MAX];

	bool intmana;
	bool wismana;

	bool britelite;	/* Lite does not require fuel */
	byte cur_lite;	/* Current light radius */

	bool winner;	/* Have we killed the Serpent? */
	bool hour;		/* Time of day */

	/* Hitpoints */
	int chp;
	int mhp;
	int oldhp;

	/* Spellpoints */
	int csp;
	int msp;

	s16b speed;	/* Current speed */

	byte realm1;	/* First magic realm */
	byte realm2;	/* Second magic realm */

	s16b lev;	/* Cur level */
	s16b max_lev;	/* Max level */

	s16b depth;	/* Cur depth */
	s16b max_depth;	/* Max depth */

	/* Combined object flags */
	u32b flags[4];

	/* Mutation flags */
	u32b muta1;
	u32b muta2;
	u32b muta3;

	s16b food;	/* Power of food */
	s16b recall;	/* Power of recall */

	/* Combat stats */
	s16b ac;
	s16b to_h;
	s16b to_d;
	s16b w_to_d;
	s16b b_to_d;
	s16b b_max_dam;
	s16b blows;
	s16b mana_bonus;

	u32b value;	/* Cost of items we are carrying */

	s16b weight;	/* Weight of items we are carrying */
	s16b encumber;	/* Weight of encumberance */

	s16b see_infra;	/* Infravision range */

	s16b skill_dis;	/* Skill: Disarming */
	s16b skill_dev;	/* Skill: Magic Devices */
	s16b skill_sav;	/* Skill: Saving throw */
	s16b skill_stl;	/* Skill: Stealth factor */
	s16b skill_sns;	/* Skill: Sensing ability */
	s16b skill_fos;	/* Skill: Searching frequency */
	s16b skill_thn;	/* Skill: To hit (normal) */
	s16b skill_thb;	/* Skill: To hit (shooting) */
	s16b skill_tht;	/* Skill: To hit (throwing) */
	s16b skill_dig;	/* Skill: Digging */
};

extern borg_player *bp_ptr;


/*
 * Various silly flags
 */

extern bool borg_stop_king;		/* The borg stops when he wins */
extern bool borg_cheat_death;	/* Is there life after death? */
extern bool borg_flag_dump;		/* Save savefile at each death */
extern bool borg_flag_save;		/* Save savefile at each level */
extern bool borg_save;			/* do a save next time we get to press a key! */

/*
 * Use a simple internal random number generator
 */
extern u32b borg_rand_local;	/* Save personal setting */


/*
 * Hack -- time variables
 */

extern s32b borg_t;	/* Current "time" */
extern s32b borg_temp_fill_valid;	/* When were the monster arrays filled */
extern s32b need_see_inviso;	/* To tell me to cast it */
extern s32b borg_see_inv;
extern bool vault_on_level;	/* borg will search for a vault */
extern bool unique_on_level;
extern int  unique_r_idx;
extern bool scaryguy_on_level;

extern bool breeder_level;	/* Borg will shut doors */
extern s16b old_depth;
extern s16b borg_no_retreat;

/*
 * Hack -- Other time variables
 */

extern s32b when_call_lite;	/* When we last did call light */
extern s32b when_wizard_lite;	/* When we last did wizard light */

extern s32b when_detect_traps;	/* When we last detected traps */
extern s32b when_detect_doors;	/* When we last detected doors */
extern s32b when_detect_walls;	/* When we last detected walls */
extern s32b when_detect_evil;

extern bool my_need_alter;	/* incase of walls/doors */
extern bool my_no_alter;	/* incase of walls/doors */

/*
 * Some information
 */

extern s16b goal;	/* Flowing (goal type) */

extern bool goal_rising;	/* Currently returning to town */

extern bool goal_leaving;	/* Currently leaving the level */

extern bool goal_fleeing;	/* Currently fleeing the level */

extern bool goal_ignoring;	/* Currently ignoring monsters */

extern int goal_recalling;	/* Currently waiting for recall, guessing turns left */
extern bool goal_less;	/* return to, but dont use, the next up stairs */

extern s16b borg_times_twitch;	/* how often twitchy on this level */
extern s16b borg_escapes;	/* how often teleported on this level */

extern bool stair_less;	/* Use the next "up" staircase */
extern bool stair_more;	/* Use the next "down" staircase */

extern s32b borg_began;	/* When this level began */
extern s32b borg_time_town;	/* how long it has been since I was in town */

extern s16b avoidance;	/* Current danger thresh-hold */

extern bool borg_failure;	/* Notice failure */

extern bool borg_attacking;	/* Are we attacking a monster? */
extern bool borg_offsetting;	/* Are we attacking a monster? with offsett balls */

extern bool borg_completed;	/* Completed the level */
extern bool borg_needs_searching;	/* borg will search with each step */
extern bool borg_full_damage;	/* make danger = full possible damage. */

/* defence flags */
extern bool borg_prot_from_evil;
extern bool borg_speed;
extern bool borg_bless;
extern bool borg_hero;
extern bool borg_berserk;
extern bool my_oppose_fire;
extern bool my_oppose_cold;
extern bool my_oppose_acid;
extern bool my_oppose_pois;
extern bool my_oppose_elec;
extern s16b borg_goi;
extern s16b borg_wraith_form;
extern s16b borg_inviso;
extern bool borg_esp;
extern s16b borg_game_ratio;
extern bool borg_shield;
extern bool borg_on_glyph;	/* borg is standing on a glyph of warding */
extern bool borg_create_door;	/* borg is going to create doors */
extern bool borg_open_door_failed;
extern bool borg_close_door_failed;
extern bool borg_sleep_spell;
extern bool borg_sleep_spell_ii;
extern bool borg_slow_spell;
extern bool borg_confuse_spell;
extern bool borg_fear_mon_spell;


/* Which shop or dungeon to visit next */
extern s16b goal_town;
extern s16b goal_shop;
extern s16b goal_dungeon;
extern s16b goal_explore_x;
extern s16b goal_explore_y;

/* Current shop/dungeon index */
extern s16b town_num;
extern s16b shop_num;
extern s16b dungeon_num;

/* List of known shops and dungeons */
extern borg_town *borg_towns;
extern borg_shop *borg_shops;
extern borg_dungeon *borg_dungeons;

/* Number of allocated towns */
extern s16b borg_town_num;
extern s16b borg_town_size;

/* Number of allocated stores */
extern s16b borg_shop_num;
extern s16b borg_shop_size;

/* Number of allocated dungeons */
extern s16b borg_dungeon_num;
extern s16b borg_dungeon_size;

/*
 * Other variables
 */

extern int c_x;	/* Current location (X) */
extern int c_y;	/* Current location (Y) */

extern int g_x;	/* Goal location (X) */
extern int g_y;	/* Goal location (Y) */

extern s32b g_power;		/* Current power value */
extern s32b g_power_home;	/* Current power_home value */

extern int dim_door_y;		/* Safe landing zone for DDoor */
extern int dim_door_x;

extern int bad_obj_x[50];	/* Dropped cursed artifact at location (X) */
extern int bad_obj_y[50];	/* Dropped cursed artifact at location (Y) */
extern int bad_obj_n;

/*
 * Some estimated state variables
 */

extern s16b my_stat_max[6];	/* Current "maximal" stat values    */
extern s16b my_stat_cur[6];	/* Current "natural" stat values    */
extern s16b my_stat_ind[6];	/* Current "additions" to stat values   */
extern bool my_need_stat_check[6];	/* do I need to check my stats */

extern s16b my_stat_add[6];	/* aditions to stats */

extern s16b home_stat_add[6];

extern s16b weapon_swap_digger;

extern int my_ammo_tval;	/* Ammo -- "tval"   */
extern s16b my_ammo_power;	/* Average power   */
extern s16b my_ammo_range;	/* Shooting range   */


/*
 * Various "amounts" (for the player)
 */

extern s16b amt_food_scroll;
extern s16b amt_food_lowcal;
extern s16b amt_torch;
extern s16b amt_lantern;
extern s16b amt_flask;

extern s16b amt_slow_poison;
extern s16b amt_pot_curing;
extern s16b amt_star_heal;
extern s16b amt_life;
extern s16b amt_rod_heal;

extern s16b amt_book[8][4];	/* [realm][sval] */

extern s16b amt_add_stat[6];
extern s16b amt_fix_stat[7];

extern s16b amt_fix_exp;

extern s16b amt_enchant_to_a;
extern s16b amt_enchant_to_d;
extern s16b amt_enchant_to_h;
extern s16b amt_brand_weapon;	/* cubragol and bolts */
extern s16b amt_digger;


/*
 * Hack -- extra state variables
 */

extern int borg_feeling;	/* Current level "feeling" */




/*
 * State variables extracted from the screen
 */

extern s32b borg_gold;	/* Current gold */

extern int borg_stat[6];	/* Current stats */

extern int borg_book[8][4];	/* Current book slots, Realm,sval */



/*
 * Constant state variables
 */

extern int borg_race;	/* Current race */
extern int borg_class;	/* Current class */



/*
 * Constant state structures
 */

extern player_race *rb_ptr;	/* Player race info */
extern player_class *cb_ptr;	/* Player class info */
extern player_magic *pmb_ptr;	/* Player magic info */


/*
 * Number of turns to step for (zero means forever)
 */
extern u16b borg_step;	/* Step count (if any) */

/*
 * Status message search string
 */
extern char borg_match[128];	/* Search string */


/*
 * Log file
 */
extern FILE *borg_fff;	/* Log file */


/*
 * Track "stairs up"
 */
extern s16b track_less_num;
extern s16b track_less_size;
extern int *track_less_x;
extern int *track_less_y;


/*
 * Track "stairs down"
 */
extern s16b track_more_num;
extern s16b track_more_size;
extern int *track_more_x;
extern int *track_more_y;

/*
 * Track glyphs
 */
extern s16b track_glyph_num;
extern s16b track_glyph_size;
extern int *track_glyph_x;
extern int *track_glyph_y;

/*
 * Track steps
 */
extern s16b track_step_num;
extern s16b track_step_size;
extern int *track_step_x;
extern int *track_step_y;

/*
 * Track closed doors
 */
extern s16b track_door_num;
extern s16b track_door_size;
extern int *track_door_x;
extern int *track_door_y;

/*
 * The object list.  This list is used to "track" objects.
 */
extern s16b borg_takes_cnt;
extern s16b borg_takes_nxt;

extern borg_take *borg_takes;


/*
 * The monster list.  This list is used to "track" monsters.
 */
extern s16b borg_kills_cnt;
extern s16b borg_kills_nxt;

extern borg_kill *borg_kills;


/*
 * Maintain a set of grids (viewable grids)
 */

extern s16b borg_view_n;
extern s16b *borg_view_y;
extern s16b *borg_view_x;


/*
 * Maintain a set of grids (scanning arrays)
 */

/* For any monster within MAX_RANGE */
extern s16b borg_temp_n;
extern s16b *borg_temp_y;
extern s16b *borg_temp_x;

/* For the monsters immediately surrounding the borg */
extern s16b borg_next_n;
extern s16b *borg_next_y;
extern s16b *borg_next_x;

/* For the monsters that can be hit by a bolt */
extern s16b borg_bolt_n;
extern s16b *borg_bolt_y;
extern s16b *borg_bolt_x;

/* For the monsters that can be hit by a beam, basically any monster in LOS */
extern s16b borg_beam_n;
extern s16b *borg_beam_y;
extern s16b *borg_beam_x;

/* For the monsters that can be hit by a ball with radius > 1 */
extern s16b borg_ball_n;
extern s16b *borg_ball_y;
extern s16b *borg_ball_x;


/*
 * Maintain a set of grids (flow calculations)
 */
extern s16b borg_flow_n;
extern s16b *borg_flow_y;
extern s16b *borg_flow_x;


/*
 * Hack -- use "flow" array as a queue
 */
extern int flow_head;
extern int flow_tail;


/*
 * Strategy flags -- examine the world
 */
extern bool borg_do_frame;	/* Acquire "frame" info */

extern bool borg_do_spell;	/* Acquire "spell" info */


/*
 * Strategy flags -- run certain functions
 */

extern bool borg_do_destroy;

/* am I fighting a unique */
extern int borg_fighting_unique;
extern bool borg_fighting_evil_unique;
#define BORG_QUESTOR	100

/*** Some functions ***/

/*
 * Queue a keypress
 */
extern errr borg_keypress(char k);

/*
 * Queue several keypresses
 */
extern errr borg_keypresses(cptr str);

/*
 * Dequeue a keypress
 */
extern char borg_inkey(bool take);

/*
 * Flush the keypresses
 */
extern void borg_flush(void);


/*
 * Obtain some text from the screen (single character)
 */
extern errr borg_what_char(int x, int y, byte *a, char *c);

/*
 * Obtain some text from the screen (multiple characters)
 */
extern errr borg_what_text(int x, int y, int n, byte *a, char *s);
extern bool borg_term_text_comp(int x, int y, cptr what);


/*
 * Log a message, Search it, and Show/Memorize it in pieces
 */
extern void borg_note(cptr fmt, ...);

/*
 * Abort the Borg, noting the reason
 */
extern void borg_oops(cptr fmt, ...);

/*
 * Take a "memory note"
 */
extern bool borg_tell(cptr what);

/*
 * Change the player name
 */
extern bool borg_change_name(cptr str);

/*
 * Dump a character description
 */
extern bool borg_dump_character(cptr str);

/*
 * Save the game (but do not quit)
 */
extern bool borg_save_game(void);


/*
 * Initialize this file
 */
extern void borg_init_1(void);

#endif

#endif
