#ifndef PLAYER_CLASSES_H
#define PLAYER_CLASSES_H

#include <src/player.h>
#include <src/object_classes.h>
#include <QString>

class magic_type
{
public:

    byte slevel;		/* Required level (to learn) */
    byte smana;			/* Required mana (to cast) */
    byte sfail;			/* Minimum chance of failure */
    byte sexp;			/* Encoded experience bonus */
};


/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */
class player_magic
{
public:

    magic_type info[PY_MAX_SPELLS];	/* The available spells */
};



/*
 * Player sex info
 */
class player_sex
{
public:

    QString title;			/* Type of sex */

    QString winner;		/* Name of winner */
};


/*
 * Player racial info
 */
class player_race
{
public:
    player_race();
    void player_race_wipe();

    QString pr_name;			/* Name */
    QString pr_text;			/* Text */

    s16b r_adj[A_MAX];	/* Racial stat bonuses */

    s16b r_dis;			/* disarming */
    s16b r_dev;			/* magic devices */
    s16b r_sav;			/* saving throw */
    s16b r_stl;			/* stealth */
    s16b r_srh;			/* search ability */
    s16b r_fos;			/* search frequency */
    s16b r_thn;			/* combat (normal) */
    s16b r_thb;			/* combat (shooting) */

    byte r_mhp;			/* Race hit-dice modifier */
    byte r_exp;			/* Race experience factor */

    byte b_age;			/* base age */
    byte m_age;			/* mod age */

    byte m_b_ht;		/* base height (males) */
    byte m_m_ht;		/* mod height (males) */
    byte m_b_wt;		/* base weight (males) */
    byte m_m_wt;		/* mod weight (males) */

    byte f_b_ht;		/* base height (females) */
    byte f_m_ht;		/* mod height (females) */
    byte f_b_wt;		/* base weight (females) */
    byte f_m_wt;		/* mod weight (females) */

    byte infra;			/* Infra-vision	range */

    byte choice;		/* Legal class choices */

    s16b hist;			/* Starting history index */

    u32b pr_flags1;		/* Racial Flags, set 1 */
    u32b pr_flags2;		/* Racial Flags, set 2 */
    u32b pr_flags3;		/* Racial Flags, set 3 */
    u32b pr_native;		/* Player Native Flags, set 3 */


};


/*
 * Starting equipment entry
 */
class start_item
{
public:

    byte tval;	/* Item's tval */
    byte sval;	/* Item's sval */
    byte min;	/* Minimum starting amount */
    byte max;	/* Maximum starting amount */
};


/*
 * Player class info
 */
class player_class
{
public:
    player_class();
    void player_class_wipe();

    QString cl_name;			/* Name  */

    QString cl_title[PY_MAX_LEVEL];		/* Titles  */

    s16b c_adj[A_MAX];	/* Class stat modifier */

    s16b c_dis;			/* class disarming */
    s16b c_dev;			/* class magic devices */
    s16b c_sav;			/* class saving throws */
    s16b c_stl;			/* class stealth */
    s16b c_srh;			/* class searching ability */
    s16b c_fos;			/* class searching frequency */
    s16b c_thn;			/* class to hit (normal) */
    s16b c_thb;			/* class to hit (bows) */

    s16b x_dis;			/* extra disarming */
    s16b x_dev;			/* extra magic devices */
    s16b x_sav;			/* extra saving throws */
    s16b x_stl;			/* extra stealth */
    s16b x_srh;			/* extra searching ability */
    s16b x_fos;			/* extra searching frequency */
    s16b x_thn;			/* extra to hit (normal) */
    s16b x_thb;			/* extra to hit (bows) */

    s16b c_mhp;			/* Class hit-dice adjustment */
    s16b c_exp;			/* Class experience factor */

    u32b flags;			/* Class Flags */
    u32b c_native;		/* Class Native Flags*/

    u16b max_attacks;	/* Maximum possible attacks */
    u16b min_weight;	/* Minimum weapon weight for calculations */
    u16b att_multiply;	/* Multiplier for attack calculations */

    byte spell_book;	/* Tval of spell books (if any) */
    u16b spell_first;	/* Level of first spell */
    u16b spell_weight;	/* Weight that hurts spells */

    u32b sense_base;	/* Base pseudo-id value */
    u16b sense_div;		/* Pseudo-id divisor */

    start_item start_items[MAX_START_ITEMS];/* The starting inventory */

    player_magic spells; /* Magic spells */
    bool pseudo_id_heavy();

};


/*
 * Player background information
 */
class hist_type
{
public:

    QString h_text;			    /* Text (offset) */

    byte roll;			    /* Frequency of this entry */
    byte chart;			    /* Chart index */
    byte next;			    /* Next chart index */
    byte bonus;			    /* Social Class Bonus + 50 */
};



class player_state
{
public:

    s16b p_speed;		/* Current speed */

    s16b num_blow;		/* Number of blows */
    s16b num_fire;		/* Number of shots */

    byte ammo_mult;		/* Ammo multiplier */
    byte ammo_tval;		/* Ammo variety */

    s16b stat_equip[A_MAX];	/* Equipment stat bonuses */
    s16b stat_index[A_MAX];	/* Indexes for the lookup tables */
    s16b stat_loaded_cur[A_MAX];	/* Current "natural" stats with all adjustments */
    s16b stat_loaded_max[A_MAX];       /* Current "maximal" stats with all adjustments */

    s16b known_ac;		/* Known base ac */
    s16b ac;			/* Base ac */

    s16b known_to_a;	/* Known bonus to ac */
    s16b to_a;			/* Bonus to ac */

    s16b to_h;			/* Bonus to hit */
    s16b known_to_h;	/* Known bonus to hit */

    s16b to_d;			/* Bonus to dam */
    s16b known_to_d;	// Known bonus to dam

    s16b see_infra;		/* Infravision range */

    s16b skills[SKILL_MAX];	/* Skills */

    u32b noise;			/* Derived from stealth */

    s16b cur_light;		/* Radius of lite (if any) */

    u32b p_flags_native_with_temp;        // The native flags with temporary resists factored in (essentaial for code that must be efficient)
    u32b p_flags_native_no_temp;   // The native flags without temporary resists factored in (needed for character screen

    bool sustain_str;	/* Keep strength */
    bool sustain_int;	/* Keep intelligence */
    bool sustain_wis;	/* Keep wisdom */
    bool sustain_dex;	/* Keep dexterity */
    bool sustain_con;	/* Keep constitution */
    bool sustain_chr;	/* Keep charisma */

    bool immune_acid;	/* Immunity to acid */
    bool immune_elec;	/* Immunity to lightning */
    bool immune_fire;	/* Immunity to fire */
    bool immune_cold;	/* Immunity to cold */
    bool immune_pois;	/* immunity to poison*/

    bool resist_acid;	/* Resist acid */
    bool resist_elec;	/* Resist lightning */
    bool resist_fire;	/* Resist fire */
    bool resist_cold;	/* Resist cold */
    bool resist_pois;	/* Resist poison */

    bool resist_fear;	/* Resist fear */
    bool resist_light;	/* Resist light */
    bool resist_dark;	/* Resist darkness */
    bool resist_blind;	/* Resist blindness */
    bool resist_confu;	/* Resist confusion */
    bool resist_sound;	/* Resist sound */
    bool resist_shard;	/* Resist shards */
    bool resist_nexus;	/* Resist nexus */
    bool resist_nethr;	/* Resist nether */
    bool resist_chaos;	/* Resist chaos */
    bool resist_disen;	/* Resist disenchant */

    bool native_lava;
    bool native_ice;
    bool native_oil;
    bool native_fire;
    bool native_sand;
    bool native_forest;
    bool native_water;
    bool native_acid;
    bool native_mud;
    bool native_boiling_water;
    bool native_boiling_mud;

    bool slow_digest;	/* Slower digestion */
    bool ffall;			/* Feather falling */
    bool regenerate;	/* Regeneration */
    bool telepathy;		/* Telepathy */
    bool see_inv;		/* See invisible */
    bool see_inv_perm;   // Does the player have permanent see invisible, or is it from a temporary effect?
    bool free_act;		/* Free action */
    bool hold_life;		/* Hold life */
    bool afraid; 		/* Afraid */
    bool light;			/* Permanent light */

    bool impact;		/* Earthquake blows */
    bool aggravate;		/* Aggravate monsters */
    bool teleport;		/* Random teleporting */
    bool exp_drain;		/* Experience draining */

    bool bless_blade;	/* Blessed blade */

    bool cursed_quiver;	/* The quiver is cursed */

    bool cumber_armor;	/* Mana draining armor */
    bool cumber_glove;	/* Mana draining gloves */
    bool heavy_wield;	/* Heavy weapon */
    bool heavy_shoot;	/* Heavy shooter */
    bool icky_wield;	/* Icky weapon */

    // All variables above need to be re-set in player_set_wipe.
    void player_state_wipe();


};


/*
 * Some more player information
 *
 * This information is retained across player lives
 */
class player_other
{
public:

    QString full_name;		/* Full name */

    bool opt[OPT_MAX];		/* Options */

    byte hitpoint_warn;		/* Hitpoint warning (0 to 9) */

    byte delay_factor;		/* Delay factor (0 to 9) */

    // All variables above need to be re-set in player_set_wipe.
    void player_other_wipe();
};


/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This entire structure is wiped when a new character is born.
 *
 * This structure is more or less laid out so that the information
 * which must be saved in the savefile precedes all the information
 * which can be recomputed as needed.
 */
class player_type
{
public:

    byte py;			/* Player location */
    byte px;			/* Player location */

    byte flow_center_y;  /* Centerpoints of the last full flow rebuild. */
    byte flow_center_x;

    byte update_center_y; /* Centerpoints of the last partial flow rebuild. */
    byte update_center_x;

    byte psex;			/* Sex index */
    byte prace;			/* Race index */
    byte pclass;		/* Class index */

    QString tile_id;

    byte hitdie;		/* Hit dice (sides) */
    byte expfact;		/* Experience factor */

    s16b age;			/* Characters age */
    s16b ht;			/* Height */
    s16b wt;			/* Weight */
    s16b sc;			/* Social Class */

    u16b q_fame;		/* Fame - used for quests */
    u16b deferred_rewards; /* Quest reward points deferred for future use */

    s32b au;			/* Current Gold */

    u32b current_score; // Player's current score.

    s16b quest_depth;	/* Max depth */
    s16b max_depth;		/* Max depth */
    s16b depth;			/* Cur depth */
    s16b recall_depth;		/* recall depth*/

    s16b max_lev;		/* Max level */
    s16b lev;			/* Cur level */

    s32b max_exp;		/* Max experience */
    s32b exp;			/* Cur experience */
    u16b exp_frac;		/* Cur exp frac (times 2^16) */

    s16b mhp;			/* Max hit pts */
    s16b chp;			/* Cur hit pts */
    u16b chp_frac;		/* Cur hit frac (times 2^16) */

    s16b msp;			/* Max mana pts */
    s16b csp;			/* Cur mana pts */
    u16b csp_frac;		/* Cur mana frac (times 2^16) */

    s16b stat_base_cur[A_MAX];	/* Current "natural" stat values, before adjustments */
    s16b stat_base_max[A_MAX];	/* Current "maximal" stat values, before adjustments */
    s16b stat_quest_add[A_MAX];	/* Quest reward bonuses */

    s16b timed[TMD_MAX];	/* Timed effects */

    s16b word_recall;	/* Word of recall counter */

    s16b p_energy;		/* Current energy */

    s16b food;			/* Current nutrition */

    byte confusing;		/* Glowing hands */
    byte searching;		/* Currently searching */

    s16b base_wakeup_chance;	/* Base amount of character noise */

    byte spell_flags[PY_MAX_SPELLS]; /* Spell flags */

    byte spell_order[PY_MAX_SPELLS];	/* Spell order */

    s16b player_hp[PY_MAX_LEVEL];	/* HP Array */

    QString died_from;		/* Cause of death */
    QString history;	/* Initial history */

    u16b total_winner;		/* Total winner */
    u16b panic_save;		/* Panic save */

    bool terminated;        // Player game was deliberately ended.
    bool is_dead;			/* Player is dead */
    bool player_turn;      /* It is the player's turn to move */
    bool is_wizard;			/* Player is in wizard mode */


    /*** Temporary fields ***/

    bool playing;			/* True if player is playing */
    bool in_store;          // Player is inside a store
    bool message_append;    /* Messages should be appended onscreen */

    bool leaving_level;		/* True if player is leaving the current level*/

    bool autosave;          /* True if autosave is pending */

    s16b create_stair;		/* Create a staircase on next level */

    byte cur_map_hgt;		/* Current dungeon level hight */
    byte cur_map_wid;		/* Current dungeon level width */

    u32b total_weight;		/* Total weight being carried */
    s16b inven_cnt;			/* Number of items in inventory */
    s16b equip_cnt;			/* Number of items in equipment */
    s16b pack_size_reduce;	/*
                             * Number of inventory slots used by
                             * the quiver */
    u16b quiver_remainder;	/* "cached" quiver statistics*/
    u16b quiver_slots;

    s16b target_set;		/* Target flag */
    s16b target_who;		/* Target identity */
    s16b target_row;		/* Target location */
    s16b target_col;		/* Target location */

    s16b health_who;		/* Health bar trackee */

    s16b monster_race_idx;	/* Monster race trackee */

    s16b object_idx;    /* Object trackee */
    s16b object_kind_idx;	/* Object kind trackee */

    s16b feature_kind_idx;	/* Feature kind tracker*/

    bool running_withpathfind;      /* Are we using the pathfinder ? */

    s16b run_cur_dir;		/* Direction we are running */
    s16b run_old_dir;		/* Direction we came from */
    bool run_unused;		/* Unused (padding field) */
    bool run_open_area;		/* Looking for an open area */
    bool run_break_right;	/* Looking for a break (right) */
    bool run_break_left;	/* Looking for a break (left) */

    s16b command_current;	/* Gives identity of current command */
    cmd_arg player_args;    /* All information about the current player command */

    s16b command_previous;          // Remembers the previous command
    cmd_arg command_previous_args;  // Remembers the previous command args

    int current_hotkey;

    s16b command_see;	/**< See "cmd1.c" */

    s16b command_new;		/* Hack -- command chaining XXX XXX */

    s16b new_spells;		/* Number of spells available */

    u32b notice;		/* Special Updates (bit flags) */
    u32b update;		/* Pending Updates (bit flags) */
    u32b redraw;		/* Normal Redraws (bit flags) */
    u32b window;		/* Window Redraws (bit flags) */

    /* Generation fields (for quick start) */
    s32b au_birth;          /* Birth gold when option birth_money is false */
    s16b stat_birth[A_MAX]; /* Birth "natural" stat values */
    s16b ht_birth;          /* Birth Height */
    s16b wt_birth;          /* Birth Weight */
    s16b sc_birth;		/* Birth social class */

    /* Variable and calculatable player state */
    player_state	state;

    byte vulnerability;	/* Used to make animal packs charge and retreat */

    u16b next_quest;

    u16b cumulative_terrain_damage; /* How much damage we are taking from
                                        terrain (multiplied by 10) */

    s32b p_turn; /* Player turn */
    s32b game_turn; /* Player turn */

    u16b dungeon_type;	/* One of the DUNGEON_TYPE_* constants */

    void player_command_wipe();
    void player_previous_command_wipe();
    void player_previous_command_update(s16b command, cmd_arg args);
    void player_type_wipe();

    bool has_learned_spells();
    bool can_cast();
    bool can_study();
    bool chooses_spells();
    bool is_running();
    bool is_resting();
    bool should_stop_resting();
    void message_append_start();
    void message_append_stop();
    u16b cut_status();
    u16b stun_status();

};

class player_attribute_maximums
{
public:

   s16b max_skills[SKILL_MAX];	/* Skills */
   s16b max_p_speed;       // Max energy gain
   s16b max_wakeup_chance; //for stealth

    // This wipes the structure as well.
    void calculate_maximums();
};



#endif // PLAYER_CLASSES_H
