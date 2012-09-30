
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
#define GOAL_KILL   1			/* Monsters */
#define GOAL_TAKE   2			/* Objects */
#define GOAL_MISC   3			/* Stores */
#define GOAL_DARK   4			/* Exploring */
#define GOAL_XTRA   5			/* Searching */
#define GOAL_BORE   6			/* Leaving */
#define GOAL_FLEE   7			/* Fleeing */
#define GOAL_TOWN	8			/* Town Special Grid */


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

/* the Z randarts are considered #127 by the borg */
#define ART_RANDART  127

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
#define AUTO_TEMP_MAX 1536


/*
 * Number of grids in the "borg_hit" array
 */
#define AUTO_HIT_MAX 1536


/*
 * Number of grids in the "flow" array
 */
#define AUTO_FLOW_MAX 2000


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

	/* Time stamp */
	s32b when;

	/* Is this shop useful? */
	s16b b_count;
	s16b u_count;
};


/*
 * Some variables
 */
extern bool borg_active;	/* Actually active */
extern bool borg_cancel;	/* Being cancelled */

extern bool borg_stop_king;
extern bool borg_dont_react;
extern int successful_target;

extern bool borg_scums_uniques;

/*
 * Borg-abilities
 */
typedef struct borg_ability borg_ability;

struct borg_ability
{
	s16b phase;
	s16b teleport;
	s16b teleport_level;
	s16b escape;
	s16b fuel;

	s16b heal;
	s16b easy_heal;
	s16b id;
	s16b star_id;
	s16b berserk;
	s16b speed;

	s16b staff_magi;
	s16b staff_dest;
	s16b staff_cool;
	s16b missile;
	s16b curepois;

	s16b det_trap;
	s16b det_door;
	s16b det_evil;
	s16b magic_map;
	s16b lite;

	s16b recharge;
	s16b remove_curse;
	s16b star_remove_curse;
	s16b pfe;
	s16b glyph;
	s16b ccw;

	s16b csw;
	s16b res_heat;
	s16b res_cold;

	s16b death;
	s16b poison;
	s16b mana;
	s16b logrus;
	s16b genocide;
	s16b mass_genocide;
	s16b invulnerability;

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

extern bool borg_flag_save;	/* Save savefile at each level */

extern bool borg_flag_dump;	/* Save savefile at each death */

extern bool borg_save;	/* do a save next time we get to press a key! */

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


/*
 * Shop goals
 */

extern s16b goal_shop;	/* Next shop to visit */

/*
 * Hack -- current shop index
 */
extern s16b shop_num;

/* Current "shops" */
extern borg_shop *borg_shops;

/*
 * Number of allocated stores...
 */
extern s16b track_shop_num;
extern s16b track_shop_size;

/*
 * Other variables
 */

extern int c_x;	/* Current location (X) */
extern int c_y;	/* Current location (Y) */

extern int g_x;	/* Goal location (X) */
extern int g_y;	/* Goal location (Y) */

extern int dim_door_y;	/* Safe landing zone for DDoor */
extern int dim_door_x;

extern int bad_obj_x[50];	/* Dropped cursed artifact at location (X) */
extern int bad_obj_y[50];	/* Dropped cursed artifact at location (Y) */

/*
 * Some estimated state variables
 */

extern s16b my_stat_max[6];	/* Current "maximal" stat values    */
extern s16b my_stat_cur[6];	/* Current "natural" stat values    */
extern s16b my_stat_use[6];	/* Current "resulting" stat values  */
extern s16b my_stat_ind[6];	/* Current "additions" to stat values   */
extern bool my_need_stat_check[6];	/* do I need to check my stats */

extern s16b my_stat_add[6];	/* aditions to stats */

extern s16b home_stat_add[6];

extern bool borg_wearing_cursed;
extern bool borg_heavy_curse;

extern s16b weapon_swap_digger;

extern int my_ammo_tval;	/* Ammo -- "tval"   */
extern s16b my_ammo_power;	/* Average power   */
extern s16b my_ammo_range;	/* Shooting range   */

extern bool my_need_enchant_to_a;	/* Need some enchantment */
extern bool my_need_enchant_to_h;	/* Need some enchantment */
extern bool my_need_enchant_to_d;	/* Need some enchantment */


/*
 * Various "amounts" (for the player)
 */

extern s16b amt_food_scroll;
extern s16b amt_food_hical;
extern s16b amt_food_lowcal;
extern s16b amt_torch;
extern s16b amt_lantern;
extern s16b amt_flask;

extern s16b amt_slow_poison;
extern s16b amt_cure_confusion;
extern s16b amt_cure_blind;
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
extern s16b borg_view_y[AUTO_VIEW_MAX];
extern s16b borg_view_x[AUTO_VIEW_MAX];


/*
 * Maintain a set of grids (scanning arrays)
 */

/* For any monster within MAX_RANGE */
extern s16b borg_temp_n;
extern s16b borg_temp_y[AUTO_TEMP_MAX];
extern s16b borg_temp_x[AUTO_TEMP_MAX];

/* For the monsters immediately surrounding the borg */
extern s16b borg_next_n;
extern s16b borg_next_y[AUTO_HIT_MAX];
extern s16b borg_next_x[AUTO_HIT_MAX];

/* For the monsters that can be hit by a bolt */
extern s16b borg_bolt_n;
extern s16b borg_bolt_y[AUTO_TEMP_MAX];
extern s16b borg_bolt_x[AUTO_TEMP_MAX];

/* For the monsters that can be hit by a beam, basically any monster in LOS */
extern s16b borg_beam_n;
extern s16b borg_beam_y[AUTO_TEMP_MAX];
extern s16b borg_beam_x[AUTO_TEMP_MAX];

/* For the monsters that can be hit by a ball with radius > 1 */
extern s16b borg_ball_n;
extern s16b borg_ball_y[AUTO_TEMP_MAX];
extern s16b borg_ball_x[AUTO_TEMP_MAX];


/*
 * Maintain a set of grids (flow calculations)
 */
extern s16b borg_flow_n;
extern s16b borg_flow_y[AUTO_FLOW_MAX];
extern s16b borg_flow_x[AUTO_FLOW_MAX];


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

extern bool borg_do_crush_junk;

extern bool borg_do_crush_hole;

extern bool borg_do_crush_slow;

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


/*
 * Log a message, Search it, and Show/Memorize it in pieces
 */
extern void borg_note(cptr what);
extern void borg_note_fmt(cptr fmt, ...);

/*
 * Abort the Borg, noting the reason
 */
extern void borg_oops(cptr what);
extern void borg_oops_fmt(cptr fmt, ...);

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
