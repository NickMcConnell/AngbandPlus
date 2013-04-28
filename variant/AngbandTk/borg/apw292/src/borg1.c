/* File: borg1.c */
/* Purpose: Low level stuff for the Borg -BEN- */
#include "angband.h"


#ifdef ALLOW_BORG

#include "borg1.h"


/*
 * This file contains various low level variables and routines.
 */

/* Date of the last change */
char borg_engine_date[] = __DATE__;

/*
 * Borg information, ScreenSaver or continual play mode;
 */
int auto_respawn;
int auto_respawn_race;
int auto_respawn_class;
int auto_respawn_str;
int auto_respawn_int;
int auto_respawn_wis;
int auto_respawn_dex;
int auto_respawn_con;
int auto_respawn_chr;
int auto_dump_level;
bool auto_respawn_winners;

/* dynamic borg stuff */
bool borg_uses_swaps;
bool borg_uses_calcs = TRUE;
bool borg_worships_damage;
bool borg_worships_speed;
bool borg_worships_hp;
bool borg_worships_mana;
bool borg_worships_ac;
bool borg_worships_gold;
bool borg_plays_risky;
bool borg_slow_optimizehome;
bool borg_scums_uniques;
bool borg_engage_cloak;
int borg_chest_fail_tolerance;

/* HACK... this should really be a parm into borg_prepared */
/*         I am just being lazy */
bool borg_slow_return = FALSE;

req_item *borg_required_item[MAX_CLASS];
int n_req[MAX_CLASS];
power_item *borg_power_item[MAX_CLASS];
int n_pwr[MAX_CLASS];
int *borg_has;
int *borg_has_on;
int *borg_artifact;
int *borg_skill;
int size_class;
int size_depth;
int size_obj;
int *formula[1000];
cptr prefix_pref[] =
{
/* personal attributes */
    "_STR",
    "_INT",
    "_WIS",
    "_DEX",
    "_CON",
    "_CHR",
    "_CSTR",
    "_CINT",
    "_CWIS",
    "_CDEX",
    "_CCON",
    "_CCHR",
    "_SSTR",
    "_SINT",
    "_SWIS",
    "_SDEX",
    "_SCON",
    "_SCHR",
    "_LITE",
    "_HP",
    "_ADJHP",
    "_MMANA",
    "_ADJSP",
    "_SFAIL1",
    "_SFAIL2",
    "_CLEVEL",
    "_ESP",
    "_LITRAD",
    "_RECALL",
    "_FOOD",  /* 29 */
    "_SPEED",
    "_SDIG",
    "_FEATH",
    "_REG",
    "_SINV",
    "_INFRA",
    "_DIS",
    "_DEV",
    "_SAV",
    "_STL",
    "_SRCH",
    "_SERCHFREQ",
    "_THN",
    "_THB",
    "_THT",
    "_DIG",
    "_IFIRE",
    "_IACID",
    "_ICOLD",
    "_IELEC",
    "_RFIRE",
    "_RCOLD",
    "_RELEC",
    "_RACID",
    "_RPOIS",
    "_RFEAR",
    "_RLITE",
    "_RDARK",
    "_RBLIND",
    "_RCONF",
    "_RSND",
    "_RSHRD",
    "_RNXUS",
    "_RNTHR",
    "_RKAOS",
    "_RDIS",
    "_HLIFE",
    "_FRACT",
    "_SRFIRE", /* same as without S but includes swap */
    "_SRCOLD",
    "_SRELEC",
    "_SRACID",
    "_SRPOIS",
    "_SRFEAR",
    "_SRLITE",
    "_SRDARK",
    "_SRBLIND",
    "_SRCONF",
    "_SRSND",
    "_SRSHRD",
    "_SRNXUS",
    "_SRNTHR",
    "_SRKAOS",
    "_SRDIS",
    "_SHLIFE",
    "_SFRACT",

/* random extra variable */
    "_DEPTH",  /* current depth being tested */
    "_CDEPTH", /* borgs current depth */

/* some combat stuff */
    "_ARMOR",
    "_TOHIT",   /* base to hit, does not include weapon */
    "_TODAM",   /* base to damage, does not include weapon */
    "_WTOHIT",  /* weapon to hit */
    "_WTODAM",  /* weapon to damage */
    "_BTOHIT",  /* bow to hit */
    "_BTODAM",  /* bow to damage */
    "_BLOWS",
    "_SHOTS",
    "_WMAXDAM", /* max damage per round with weapon (normal blow) */
                /* Assumes you can enchant to +8 if you are level 25+ */
    "_WBASEDAM",/* max damage per round with weapon (normal blow) */
                /* Assumes you have no enchantment */
    "_BMAXDAM", /* max damage per round with bow (normal hit) */
                /* Assumes you can enchant to +8 if you are level 25+ */
    "_HEAVYWEPON",
    "_HEAVYBOW",

/* curses */
    "_CRSTELE",
    "_CRSAGRV",

/* weapon attributes */
    "_WSANIMAL",  /* WS = weapon slays */
    "_WSEVIL",
    "_WSUNDEAD",
    "_WSDEMON",
    "_WSORC",
    "_WSTROLL",
    "_WSGIANT",
    "_WSDRAGON",
    "_WKDRAGON",   /* WK = Weapon Kills */
    "_WIMPACT",
    "_WBACID",     /* WB = Weapon Branded With */
    "_WBELEC",
    "_WBFIRE",
    "_WBCOLD",

/* amounts */
    "_ATELEPORT",
    "_AESCAPE",
    "_FUEL",
    "_HEAL",
    "_EZHEAL",
    "_ID",
    "_ASPEED",
    "_ASTFMAGI",  /* Amount Staff Charges */
    "_ASTFDEST",
    "_AMISSLES",  /* only ones for your current bow count */
    "_ACUREPOIS",
    "_ADETTRAP",
    "_ADETDOOR",
    "_ADETEVIL",
    "_AMAGICMAP",
    "_ARECHARGE",
    "_APFE",      /* Protection from Evil */
    "_AGLYPH",    /* Rune Protection */
    "_ACCW",     /* CCW potions (just because we use it so often) */
    "_ACSW",     /* CSW potions (+ CLW if cut) */
    "_ARESHEAT", /* potions of res heat */
    "_ARESCOLD", /* pot of res cold */
    "_ATELEPORTLVL", /* scroll of teleport level */
    "_AXGOI",        /* Reliable GOI spell */
    "_AGOI",        /* GOI spell Legal*/
    "_AHWORD",      /* Holy Word prayer Legal*/

    NULL
};

/*
 * Some variables
 */

bool auto_active;       /* Actually active */
bool borg_resurrect = FALSE;    /* continous play mode */

bool auto_cancel;       /* Being cancelled */

char genocide_target;   /* identity of the poor unsuspecting soul */
int zap_slot;                  /* slot of a wand/staff---to avoid a game bug*/
bool borg_casted_glyph;        /* because we dont have a launch anymore */
int auto_stop_dlevel = -1;
int auto_stop_clevel = -1;
bool auto_stop_king = TRUE;
bool borg_dont_react = FALSE;
int successful_target = 0;
int sold_item_tval;
int sold_item_sval;
int sold_item_pval;
int sold_item_store;

/*
 * Hack -- optional cheating flags
 */

bool auto_cheat_equip;  /* Cheat for "equip mode" */

bool auto_cheat_inven;  /* Cheat for "inven mode" */

bool auto_cheat_spell;  /* Cheat for "browse mode" */

bool auto_cheat_panel;  /* Cheat for "panel mode" */


/*
 * Various silly flags
 */

bool auto_flag_save = TRUE;    /* Save savefile at each level */
bool auto_flag_dump = FALSE;    /* Save savefile at each death */
bool borg_save = FALSE;        /* do a save next level */
bool auto_borg_message = TRUE; /* apw list messages or not */
bool borg_graphics = FALSE;    /* rr9's graphics */
bool borg_confirm_target = FALSE; /* emergency spell use */

/*
 * Use a simple internal random number generator
 */

bool auto_rand_quick;       /* Save system setting */

u32b auto_rand_value;       /* Save system setting */

u32b auto_rand_local;       /* Save personal setting */


bool auto_do_star_id;


/*
 * Hack -- Time variables
 */

s16b c_t = 0L;          /* Current "time" */
s16b need_see_inviso = 0;    /* cast this when required */
s16b borg_see_inv = 0;
bool need_shift_panel = FALSE;    /* to spot offscreens */
s16b when_shift_panel = 0L;
s16b time_this_panel = 0L;   /* Current "time" on current panel*/
bool vault_on_level;         /* Borg will search for a vault */
bool unique_on_level;
bool scaryguy_on_level;     /* flee from certain guys */

bool breeder_level = FALSE;          /* Borg will shut door */
s16b old_depth = 128;
s16b borg_respawn = 0;
bool genocide_level_hounds = FALSE;       /* Used against Hounds */
s16b borg_hound_count = 0;
s16b borg_no_retreat= 0;

/*
 * Hack -- Other time variables
 */

s16b when_call_lite;        /* When we last did call light */
s16b when_wizard_lite;      /* When we last did wizard light */

s16b when_detect_traps;     /* When we last detected traps */
s16b when_detect_doors;     /* When we last detected doors */
s16b when_detect_walls;     /* When we last detected walls */
s16b when_detect_evil;      /* When we last detected monsters or evil */
s16b when_last_kill_mult = 0;   /* When a multiplier was last killed */

bool my_need_alter;        /* incase i hit a wall or door */
bool my_no_alter;          /*  */
bool my_need_redraw;        /* incase i hit a wall or door */
bool borg_attempting_refresh = FALSE;  /* for the goi spell */

/*
 * Some information
 */

s16b goal;          /* Goal type */

bool goal_rising;       /* Currently returning to town */

bool goal_leaving;      /* Currently leaving the level */

bool goal_fleeing;      /* Currently fleeing the level */

bool goal_ignoring;     /* Currently ignoring monsters */

bool goal_recalling;        /* Currently waiting for recall */

s16b borg_times_twitch; /* how often twitchy on this level */
s16b borg_escapes;      /* how often teleported on this level */

bool stair_less;        /* Use the next "up" staircase */
bool stair_more;        /* Use the next "down" staircase */

s32b auto_began;        /* When this level began */
s32b auto_time_town;    /* how long it has been since I was in town */

s16b avoidance = 0;     /* Current danger thresh-hold */

bool auto_failure;      /* Notice failure */

bool auto_simulate;     /* Simulation flag */
bool borg_attacking;        /* Simulation flag */
bool borg_offsetting;    /* offset ball attacks */

bool auto_completed;        /* Completed the level */
bool borg_on_upstairs;      /* used when leaving a level */
bool borg_on_dnstairs;      /* used when leaving a level */

/* defence flags */
bool borg_prot_from_evil;
bool borg_speed;
bool borg_bless;
bool borg_hero;
bool borg_berserk;
bool my_oppose_fire;
bool my_oppose_cold;
bool my_oppose_acid;
bool my_oppose_pois;
bool my_oppose_elec;
s16b borg_goi;
s16b borg_game_ratio;  /* the ratio of borg time to game time */

bool borg_shield;
bool borg_on_glyph;    /* borg is standing on a glyph of warding */
bool borg_create_door;    /* borg is going to create doors */
bool borg_sleep_spell;
bool borg_sleep_spell_ii;
bool borg_slow_spell;  /* borg is about to cast the spell */
bool borg_confuse_spell;
bool borg_fear_mon_spell;

/*
 * Current shopping information
 */

s16b goal_shop = -1;        /* Next shop to visit */
s16b goal_ware = -1;        /* Next item to buy there */
s16b goal_item = -1;        /* Next item to sell there */


/*
 * Location variables
 */

int w_x;            /* Current panel offset (X) */
int w_y;            /* Current panel offset (Y) */

int c_x;            /* Current location (X) */
int c_y;            /* Current location (Y) */

int g_x;            /* Goal location (X) */
int g_y;            /* Goal location (Y) */

/* BIG HACK! Assume only 10 cursed artifacts */
int bad_obj_x[50];  /* Dropped cursed artifact at location (X) */
int bad_obj_y[50];  /* Dropped cursed artifact at location (Y) */


/*
 * State variables extracted from the screen
 */

bool do_weak;       /* Currently Weak */
bool do_hungry;     /* Currently Hungry/Weak */

bool do_full;       /* Currently Full/Gorged */
bool do_gorged;     /* Currently Gorged */

bool do_blind;      /* Currently Blind */
bool do_afraid;     /* Currently Afraid */
bool do_confused;   /* Currently Confused */
bool do_poisoned;   /* Currently Poisoned */

bool do_cut;        /* Currently bleeding */
bool do_stun;       /* Currently stunned */
bool do_heavy_stun; /* Currently very stunned */

bool do_image;      /* May be hallucinating */
bool do_study;      /* May learn spells */

bool do_fix_lev;    /* Drained LEV */
bool do_fix_exp;    /* Drained EXP */

bool do_fix_stat[6];    /* Drained Stats */


/*
 * Some estimated state variables
 */

s16b my_stat_max[6];    /* Current "maximal" stat values */
s16b my_stat_cur[6];    /* Current "natural" stat values */
s16b my_stat_use[6];    /* Current "resulting" stat values */
s16b my_stat_ind[6];    /* Current "additions" to stat values */
bool my_need_stat_check[6];  /* do I need to check my stats? */

s16b my_stat_add[6];  /* additions to stats  This will allow upgrading of */
                      /* equiptment to allow a ring of int +4 to be traded */
                      /* for a ring of int +6 even if maximized to allow a */
                      /* later swap to be better. */

s16b home_stat_add[6];

int weapon_swap;    /* location of my swap weapon */
int armour_swap;    /* my swap of armour */

/* a 3 state boolean */
/*-1 = not cursed, no help needed for it */
/* 0 = light curse, needs light remove curse spell */
/* 1 = heavy curse, needs heavy remove curse spell */
int decurse_weapon_swap;  /* my swap is great, except its cursed */
int enchant_weapon_swap_to_h;  /* my swap is great, except its cursed */
int enchant_weapon_swap_to_d;  /* my swap is great, except its cursed */
int decurse_armour_swap;  /* my swap is great, except its cursed */
int enchant_armour_swap_to_a;  /* my swap is great, except its cursed */
bool borg_wearing_cursed;

s32b weapon_swap_value;
s32b armour_swap_value;

s16b weapon_swap_digger;
byte weapon_swap_slay_animal;
byte weapon_swap_slay_evil;
byte weapon_swap_slay_undead;
byte weapon_swap_slay_demon;
byte weapon_swap_slay_orc;
byte weapon_swap_slay_troll;
byte weapon_swap_slay_giant;
byte weapon_swap_slay_dragon;
byte weapon_swap_kill_dragon;
byte weapon_swap_impact;
byte weapon_swap_brand_acid;
byte weapon_swap_brand_elec;
byte weapon_swap_brand_fire;
byte weapon_swap_brand_cold;
byte weapon_swap_see_infra;
byte weapon_swap_slow_digest;
byte weapon_swap_aggravate;
byte weapon_swap_teleport;
byte weapon_swap_regenerate;
byte weapon_swap_telepathy;
byte weapon_swap_lite;
byte weapon_swap_see_invis;
byte weapon_swap_ffall;
byte weapon_swap_free_act;
byte weapon_swap_hold_life;
byte weapon_swap_immune_fire;
byte weapon_swap_immune_acid;
byte weapon_swap_immune_cold;
byte weapon_swap_immune_elec;
byte weapon_swap_resist_acid;
byte weapon_swap_resist_elec;
byte weapon_swap_resist_fire;
byte weapon_swap_resist_cold;
byte weapon_swap_resist_pois;
byte weapon_swap_resist_conf;
byte weapon_swap_resist_sound;
byte weapon_swap_resist_lite;
byte weapon_swap_resist_dark;
byte weapon_swap_resist_chaos;
byte weapon_swap_resist_disen;
byte weapon_swap_resist_shard;
byte weapon_swap_resist_nexus;
byte weapon_swap_resist_blind;
byte weapon_swap_resist_neth;
byte weapon_swap_resist_fear;
byte armour_swap_slay_animal;
byte armour_swap_slay_evil;
byte armour_swap_slay_undead;
byte armour_swap_slay_demon;
byte armour_swap_slay_orc;
byte armour_swap_slay_troll;
byte armour_swap_slay_giant;
byte armour_swap_slay_dragon;
byte armour_swap_kill_dragon;
byte armour_swap_impact;
byte armour_swap_brand_acid;
byte armour_swap_brand_elec;
byte armour_swap_brand_fire;
byte armour_swap_brand_cold;
byte armour_swap_see_infra;
byte armour_swap_slow_digest;
byte armour_swap_aggravate;
byte armour_swap_teleport;
byte armour_swap_regenerate;
byte armour_swap_telepathy;
byte armour_swap_lite;
byte armour_swap_see_invis;
byte armour_swap_ffall;
byte armour_swap_free_act;
byte armour_swap_hold_life;
byte armour_swap_immune_fire;
byte armour_swap_immune_acid;
byte armour_swap_immune_cold;
byte armour_swap_immune_elec;
byte armour_swap_resist_acid;
byte armour_swap_resist_elec;
byte armour_swap_resist_fire;
byte armour_swap_resist_cold;
byte armour_swap_resist_pois;
byte armour_swap_resist_conf;
byte armour_swap_resist_sound;
byte armour_swap_resist_lite;
byte armour_swap_resist_dark;
byte armour_swap_resist_chaos;
byte armour_swap_resist_disen;
byte armour_swap_resist_shard;
byte armour_swap_resist_nexus;
byte armour_swap_resist_blind;
byte armour_swap_resist_neth;
byte armour_swap_resist_fear;

byte my_ammo_tval;  /* Ammo -- "tval" */
byte my_ammo_sides; /* Ammo -- "sides" */
s16b my_ammo_power; /* Shooting multipler */
s16b my_ammo_range; /* Shooting range */

s16b my_need_enchant_to_a;  /* Need some enchantment */
s16b my_need_enchant_to_h;  /* Need some enchantment */
s16b my_need_enchant_to_d;  /* Need some enchantment */
s16b my_need_brand_weapon;  /* apw actually brand bolts */

/*
 * Hack -- basic "power"
 */

s32b my_power;


/*
 * Various "amounts" (for the player)
 */

s16b amt_phase;
s16b amt_food_hical;
s16b amt_food_lowcal;

s16b amt_ez_heal_star;
s16b amt_ez_heal_life;
s16b amt_rod_heal;      /* used by warriors and rogues mainly */
s16b amt_pot_heal;      /* make sure that real potions are carried to Morgy */
s16b amt_mana;
s16b amt_slow_poison;
s16b amt_cure_confusion;
s16b amt_cure_blind;

s16b amt_book[9];

s16b amt_add_stat[6];
s16b amt_fix_stat[7];  /* #7 is to fix all stats */
s16b amt_fix_exp;

s16b amt_cool_staff;   /* holiness - power staff */

s16b amt_enchant_to_a;
s16b amt_enchant_to_d;
s16b amt_enchant_to_h;
s16b amt_brand_weapon;  /* apw brand bolts */
s16b amt_enchant_weapon;
s16b amt_enchant_armor;
s16b amt_digger;

/*
 * Various "amounts" (for the home)
 */

s16b num_food;
s16b num_mold;
s16b num_ident;
s16b num_star_ident;
s16b num_recall;
s16b num_phase;
s16b num_escape;
s16b num_teleport;
s16b num_berserk;
s16b num_teleport_level;

s16b num_cure_critical;
s16b num_cure_serious;

s16b num_pot_rheat;
s16b num_pot_rcold;

s16b num_missile;

s16b num_book[9];

s16b num_fix_stat[7]; /* #7 is to fix all stats */

s16b num_fix_exp;
s16b num_mana;
s16b num_heal;
s16b num_heal_true;
s16b num_ez_heal;
s16b num_ez_heal_true;
s16b num_pfe;
s16b num_glyph;
s16b num_mass_genocide;
s16b num_speed;

s16b num_enchant_to_a;
s16b num_enchant_to_d;
s16b num_enchant_to_h;
s16b num_brand_weapon;  /*apw brand bolts */
s16b num_genocide;

s16b num_artifact;

s16b home_slot_free;
s16b home_damage;
s16b num_duplicate_items;
s16b num_slow_digest;
s16b num_regenerate;
s16b num_telepathy;
s16b num_lite;
s16b num_see_inv;
s16b num_invisible;   /* apw */

s16b num_ffall;
s16b num_free_act;
s16b num_hold_life;
s16b num_immune_acid;
s16b num_immune_elec;
s16b num_immune_fire;
s16b num_immune_cold;
s16b num_resist_acid;
s16b num_resist_elec;
s16b num_resist_fire;
s16b num_resist_cold;
s16b num_resist_pois;
s16b num_resist_conf;
s16b num_resist_sound;
s16b num_resist_lite;
s16b num_resist_dark;
s16b num_resist_chaos;
s16b num_resist_disen;
s16b num_resist_shard;
s16b num_resist_nexus;
s16b num_resist_blind;
s16b num_resist_neth;
s16b num_sustain_str;
s16b num_sustain_int;
s16b num_sustain_wis;
s16b num_sustain_dex;
s16b num_sustain_con;
s16b num_sustain_all;

s16b num_speed;
s16b num_edged_weapon;
s16b num_bad_gloves;
s16b num_weapons;
s16b num_bow;
s16b num_rings;
s16b num_neck;
s16b num_armor;
s16b num_cloaks;
s16b num_shields;
s16b num_hats;
s16b num_gloves;
s16b num_boots;

/*
 * Hack -- extra state variables
 */

int auto_feeling = 0;   /* Current level "feeling" */

int auto_max_level = 1; /* Maximum player level */
int auto_max_depth = 0; /* Maximum dungeon depth */


/*
 * Hack -- current shop index
 */

s16b shop_num = -1;     /* Current shop index */



/*
 * State variables extracted from the screen
 */

int auto_depth;     /* Current dungeon "level" */

bool borg_king;     /* We won!  Time to retire. */

s32b auto_exp;      /* Current experience */

s32b auto_gold;     /* Current gold */

int auto_speed;     /* Current speed */

int auto_ac;        /* Current ac */

int auto_chp;       /* Current hitpoints */
int auto_mhp;       /* Maximum hitpoints */

int auto_csp;       /* Current spell points */
int auto_msp;       /* Maximum spell points */

int auto_stat[6];   /* Current stat values */

int auto_book[9];   /* Current book slots */


/*
 * State variables extracted from the inventory/equipment
 */

int auto_cur_wgt;   /* Current weight */


/*
 * Constant state variables
 */

int auto_race;      /* Player race */
int auto_class;     /* Player class */


/*
 * Hack -- access the class/race records
 */

player_race *rb_ptr;    /* Player race info */
player_class *cb_ptr;   /* Player class info */

player_magic *mb_ptr;   /* Player magic info */



/*
 * Number of turns to step for (zero means forever)
 */
u16b auto_step = 0;     /* Step count (if any) */


/*
 * Status message search string
 */
char auto_match[128] = "plain gold ring";  /* Search string */


/*
 * Log file
 */
FILE *auto_fff = NULL;      /* Log file */


/*
 * Hack -- single character constants
 */

const char p1 = '(', p2 = ')';
const char c1 = '{', c2 = '}';
const char b1 = '[', b2 = ']';


/*
 * Hack -- the detection arrays
 */

bool auto_detect_wall[6][6];

bool auto_detect_trap[6][6];

bool auto_detect_door[6][6];

bool auto_detect_evil[6][6];

/*
 * Locate the store doors
 */

byte *track_shop_x;
byte *track_shop_y;


/*
 * Track "stairs up"
 */

s16b track_less_num;
s16b track_less_size;
byte *track_less_x;
byte *track_less_y;


/*
 * Track "stairs down"
 */

s16b track_more_num;
s16b track_more_size;
byte *track_more_x;
byte *track_more_y;

/*
 * Track glyphs
 */
s16b track_glyph_num;
s16b track_glyph_size;
byte *track_glyph_x;
byte *track_glyph_y;

/*
 * Track Steps
 */
s16b track_step_num;
s16b track_step_size;
byte *track_step_x;
byte *track_step_y;

/*
 * Track closed doors
 */
s16b track_door_num;
s16b track_door_size;
byte *track_door_x;
byte *track_door_y;

/*
 * The object list.  This list is used to "track" objects.
 */

s16b auto_takes_cnt;

s16b auto_takes_nxt;

auto_take *auto_takes;


/*
 * The monster list.  This list is used to "track" monsters.
 */

s16b auto_kills_cnt;
s16b auto_kills_summoner;    /* index of a summoner */
s16b auto_kills_nxt;

auto_kill *auto_kills;


/* a 3 state boolean */
/*-1 = not checked yet */
/* 0 = not ready */
/* 1 = ready */
int borg_ready_morgoth;


/*
 * Hack -- extra fear per "region"
 */
u16b auto_fear_region[6][18];


/*
 * Hack -- count racial appearances per level
 */
s16b *auto_race_count;


/*
 * Hack -- count racial kills (for uniques)
 */

s16b *auto_race_death;


/*
 * Classification of map symbols
 */

bool auto_is_take[256];     /* Symbol may be an object */

bool auto_is_kill[256];     /* Symbol may be a monster */


/*
 * The current map
 */

auto_grid *auto_grids[AUTO_MAX_Y];  /* The grids */


#ifdef BORG_ROOMS

/*
 * Some "map" related variables
 */

int auto_room_max = 0;      /* First totally free room */

auto_room *auto_rooms;      /* Current "room list" */

auto_room *auto_room_head;  /* &auto_rooms[0] */

auto_room *auto_room_tail;  /* &auto_rooms[AUTO_MAX_ROOMS-1] */

#endif


/*
 * Maintain a set of grids marked as "BORG_LITE"
 */

s16b auto_lite_n = 0;

byte auto_lite_x[AUTO_LITE_MAX];
byte auto_lite_y[AUTO_LITE_MAX];

/*
 * Maintain a set of grids marked as "BORG_GLOW"
 */

s16b auto_glow_n = 0;

byte auto_glow_x[AUTO_LITE_MAX];
byte auto_glow_y[AUTO_LITE_MAX];


/*
 * Maintain a set of grids marked as "BORG_VIEW"
 */

s16b auto_view_n = 0;

byte auto_view_x[AUTO_VIEW_MAX];
byte auto_view_y[AUTO_VIEW_MAX];


/*
 * Maintain a temporary set of grids
 */

s16b auto_temp_n = 0;

byte auto_temp_x[AUTO_TEMP_MAX];
byte auto_temp_y[AUTO_TEMP_MAX];

byte offset_x;
byte offset_y;


/*
 * Maintain a circular queue of grids
 */

s16b auto_flow_n = 0;

byte auto_flow_x[AUTO_FLOW_MAX];
byte auto_flow_y[AUTO_FLOW_MAX];


/*
 * Hack -- use "flow" array as a queue
 */

int flow_head = 0;
int flow_tail = 0;



/*
 * Some variables
 */

auto_data *auto_data_flow;  /* Current "flow" data */

auto_data *auto_data_cost;  /* Current "cost" data */

auto_data *auto_data_hard;  /* Constant "hard" data */

auto_data *auto_data_know;  /* Current "know" flags */

auto_data *auto_data_icky;  /* Current "icky" flags */



/*
 * Strategy flags -- recalculate things
 */

bool auto_danger_wipe = FALSE;  /* Recalculate danger */

bool auto_update_view = FALSE;  /* Recalculate view */

bool auto_update_lite = FALSE;  /* Recalculate lite */


/*
 * Strategy flags -- examine the world
 */

bool auto_do_inven = TRUE;  /* Acquire "inven" info */

bool auto_do_equip = TRUE;  /* Acquire "equip" info */

bool auto_do_panel = TRUE;  /* Acquire "panel" info */

bool auto_do_frame = TRUE;  /* Acquire "frame" info */

bool auto_do_spell = TRUE;  /* Acquire "spell" info */

byte auto_do_spell_aux = 0; /* Hack -- book for "auto_do_spell" */

bool auto_do_browse = 0;    /* Acquire "store" info */

byte auto_do_browse_what = 0;   /* Hack -- store for "auto_do_browse" */

byte auto_do_browse_more = 0;   /* Hack -- pages for "auto_do_browse" */


/*
 * Strategy flags -- run certain functions
 */

bool auto_do_crush_junk = FALSE;

bool auto_do_crush_hole = FALSE;

bool auto_do_crush_slow = FALSE;

/* am I fighting a unique? */
int borg_fighting_unique;
bool borg_fighting_evil_unique;

/* am I fighting a summoner? */
bool borg_fighting_summoner;

/*
 * Calculate "incremental motion". Used by project() and shoot().
 * Assumes that (*y,*x) lies on the path from (y1,x1) to (y2,x2).
 */
/* changing this to be more like project_path */
/* note that this is much slower but much more accurate */
void mmove2(int *py, int *px, int y1, int x1, int y2, int x2)
{
    int dy, dx;
    int sy, sx;
    int y, x;

    /* Scale factors */
    int full, half;

    /* Fractions */
    int frac;

    /* Slope */
    int m;

    /* Distance */
    int k = 0;

    /* Extract the distance travelled */
    /* Analyze "dy" */
    if (y2 < y1)
    {
        dy = (y1 - y2);
        sy = -1;
    }
    else
    {
        dy = (y2 - y1);
        sy = 1;
    }

    /* Analyze "dx" */
    if (x2 < x1)
    {
        dx = (x1 - x2);
        sx = -1;
    }
    else
    {
        dx = (x2 - x1);
        sx = 1;
    }

    /* Paranoia -- Hack -- no motion */
    if (!dy && !dx) return;

    /* Number of "units" in one "half" grid */
    half = (dy * dx);

    /* Number of "units" in one "full" grid */
    full = half << 1;

    /* First step is fixed */
    if (*px == x1 && *py == y1)
    {
        if (dy > dx)
        {
            *py+=sy;
            return;
        }
        else if (dx > dy)
        {
            *px+=sx;
            return;
        }
        else
        {
            *px+=sx;
            *py+=sy;
            return;
        }
    }

    /* Move mostly vertically */
    if (dy > dx)
    {
        k = dy;

        /* Start at tile edge */
        frac = dx * dx;

        /* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        y = y1 + sy;
        x = x1;

        /* Create the projection path */
        while (1)
        {
            if (x == *px && y == *py)
                k = 1;

            /* Slant */
            if (m)
            {
                /* Advance (X) part 1 */
                frac += m;

                /* Horizontal change */
                if (frac >= half)
                {
                    /* Advance (X) part 2 */
                    x += sx;

                    /* Advance (X) part 3 */
                    frac -= full;
                }
            }

            /* Advance (Y) */
            y += sy;

            /* Track distance */
            k--;

            if (!k)
            {
                *px = x;
                *py = y;
                return;
            }
        }
    }
    /* Move mostly horizontally */
    else if (dx > dy)
    {
        /* Start at tile edge */
        frac = dy * dy;

        /* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
        m = frac << 1;

        /* Start */
        y = y1;
        x = x1 + sx;
        k = dx;

        /* Create the projection path */
        while (1)
        {
            if (x == *px && y == *py)
                k = 1;

            /* Slant */
            if (m)
            {
                /* Advance (Y) part 1 */
                frac += m;

                /* Vertical change */
                if (frac >= half)
                {
                    /* Advance (Y) part 2 */
                    y += sy;

                    /* Advance (Y) part 3 */
                    frac -= full;
                }
            }

            /* Advance (X) */
            x += sx;

            /* Track distance */
            k--;

            if (!k)
            {
                *px = x;
                *py = y;
                return;
            }
        }
    }
    /* Diagonal */
    else
    {
        /* Start */
        k = dy;
        y = y1 + sy;
        x = x1 + sx;

        /* Create the projection path */
        while (1)
        {
            if (x == *px && y == *py)
                k=1;

            /* Advance (Y) */
            y += sy;

            /* Advance (X) */
            x += sx;

            /* Track distance */
            k--;

            if (!k)
            {
                *px = x;
                *py = y;
                return;
            }
        }
    }
}
#ifndef BORG_TK

/*
 * Query the "attr/char" at a given location on the screen
 * We return "zero" if the given location was legal
 *
 * XXX XXX XXX We assume the given location is legal
 */
errr borg_what_char(int x, int y, byte *a, char *c)
{
    /* Direct access XXX XXX XXX */
    (*a) = (Term->scr->a[y][x]);
    (*c) = (Term->scr->c[y][x]);

    /* Success */
    return (0);
}


/*
 * Query the "attr/chars" at a given location on the screen
 *
 * Note that "a" points to a single "attr", and "s" to an array
 * of "chars", into which the attribute and text at the given
 * location are stored.
 *
 * We will not grab more than "ABS(n)" characters for the string.
 * If "n" is "positive", we will grab exactly "n" chars, or fail.
 * If "n" is "negative", we will grab until the attribute changes.
 *
 * We automatically convert all "blanks" and "invisible text" into
 * spaces, and we ignore the attribute of such characters.
 *
 * We do not strip final spaces, so this function will very often
 * read characters all the way to the end of the line.
 *
 * We succeed only if a string of some form existed, and all of
 * the non-space characters in the string have the same attribute,
 * and the string was long enough.
 *
 * XXX XXX XXX We assume the given location is legal
 */
errr borg_what_text(int x, int y, int n, byte *a, char *s)
{
    int i;

    byte t_a;
    char t_c;

    byte *aa;
    char *cc;

    /* Current attribute */
    byte d_a = 0;

    /* Max length to scan for */
    int m = ABS(n);

    /* Hack -- Do not run off the screen */
    if (x + m > 80) m = 80 - x;

    /* Direct access XXX XXX XXX */
    aa = &(Term->scr->a[y][x]);
    cc = &(Term->scr->c[y][x]);

    /* Grab the string */
    for (i = 0; i < m; i++)
    {
        /* Access */
        t_a = *aa++;
        t_c = *cc++;

        /* Handle spaces */
        if ((t_c == ' ') || !t_a)
        {
            /* Save space */
            s[i] = ' ';
        }

        /* Handle real text */
        else
        {
            /* Attribute ready */
            if (d_a)
            {
                /* Verify the "attribute" (or stop) */
                if (t_a != d_a) break;
            }

            /* Acquire attribute */
            else
            {
                /* Save it */
                d_a = t_a;
            }

            /* Save char */
            s[i] = t_c;
        }
    }

    /* Terminate the string */
    s[i] = '\0';

    /* Save the attribute */
    (*a) = d_a;

    /* Too short */
    if ((n > 0) && (i != n)) return (1);

    /* Success */
    return (0);
}

#endif /* not BORG_TK */


/*
 * Log a message to a file
 */
void borg_info(cptr what)
{
    /* Dump a log file message */
    if (auto_fff) fprintf(auto_fff, "%s\n", what);
if (auto_fff) fflush(auto_fff); /* TNB */
}



/*
 * Memorize a message, Log it, Search it, and Display it in pieces
 */
void borg_note(cptr what)
{
/* apw show the notes only if auto_borg_message is set--speed up the borg */
if (auto_borg_message)
   {
    int j, n, i, k;

    int w, h, x, y;

#if 1 /* BUG -- TNB */
	char buf[1024];
	(void) strcpy(buf, what);
	what = buf;
#endif

#ifndef BORG_TK
    term *old = Term;
#endif

    /* Memorize it */
    message_add(what, MSG_GENERIC);


    /* Log the message */
    borg_info(what);


    /* Mega-Hack -- Check against the search string */
    if (auto_match[0] && strstr(what, auto_match))
    {
        /* Clean cancel */
        auto_cancel = TRUE;
    }

    /* Mega-Hack -- Check against the swap loops */
    if (strstr(what, "Best Combo"))
    {
        /* Tick the anti loop clock */
        time_this_panel += 10;
        borg_note(format("# Anti-loop variable tick (%d).", time_this_panel));
    }

#ifndef BORG_TK
    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        if (!angband_term[j]) continue;

        /* Check flag */
        if (!(op_ptr->window_flag[j] & PW_BORG_1)) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Access size */
        Term_get_size(&w, &h);

        /* Access cursor */
        Term_locate(&x, &y);

        /* Erase current line */
        Term_erase(0, y, 255);


        /* Total length */
        n = strlen(what);

        /* Too long */
        if (n > w - 2)
        {
            char buf[1024];

            /* Split */
            while (n > w - 2)
            {
                /* Default */
                k = w - 2;

                /* Find a split point */
                for (i = w / 2; i < w - 2; i++)
                {
                    /* Pre-emptive split point */
                    if (isspace(what[i])) k = i;
                }

                /* Copy over the split message */
                for (i = 0; i < k; i++)
                {
                    /* Copy */
                    buf[i] = what[i];
                }

                /* Indicate split */
                buf[i++] = '\\';

                /* Terminate */
                buf[i] = '\0';

                /* Show message */
                Term_addstr(-1, TERM_WHITE, buf);

                /* Advance (wrap) */
                if (++y >= h) y = 0;

                /* Erase next line */
                Term_erase(0, y, 255);

                /* Advance */
                what += k;

                /* Reduce */
                n -= k;
            }

            /* Show message tail */
            Term_addstr(-1, TERM_WHITE, what);

            /* Advance (wrap) */
            if (++y >= h) y = 0;

            /* Erase next line */
            Term_erase(0, y, 255);
        }

        /* Normal */
        else
        {
            /* Show message */
            Term_addstr(-1, TERM_WHITE, what);

            /* Advance (wrap) */
            if (++y >= h) y = 0;

            /* Erase next line */
            Term_erase(0, y, 255);
        }


        /* Flush output */
        Term_fresh();

        /* Use correct window */
        Term_activate(old);
    }
#endif /* not BORG_TK */
   }
 }




/*
 * Abort the Borg, noting the reason
 */
void borg_oops(cptr what)
{
    /* Stop processing */
    auto_active = FALSE;

    /* Give a warning */
    borg_note(format("# Aborting (%s).", what));

    /* Forget borg keys */
    borg_flush();
}

/*
 * A Queue of keypresses to be sent
 */
static char *auto_key_queue;
static s16b auto_key_head;
static s16b auto_key_tail;


/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypress(char k)
{
    /* Hack -- Refuse to enqueue "nul" */
    if (!k) return (-1);

    /* Hack -- note the keypress */
#if 1 /* BUG -- TNB */
	if (auto_fff) 
	{
		if (k == '\n')
			fprintf(auto_fff, "& Key <\\n>\n");
		else if (k == '\r')
			fprintf(auto_fff, "& Key <\\r>\n");
		else if (k == ESCAPE)
			fprintf(auto_fff, "& Key <ESC>\n");
		else
			fprintf(auto_fff, "& Key <%c>\n", k);
		fflush(auto_fff);
	}
#else
    if (auto_fff) borg_info(format("& Key <%c>", k));
#endif

    /* Store the char, advance the queue */
    auto_key_queue[auto_key_head++] = k;

    /* Circular queue, handle wrap */
    if (auto_key_head == KEY_SIZE) auto_key_head = 0;

    /* Hack -- Catch overflow (forget oldest) */
    if (auto_key_head == auto_key_tail) borg_oops("overflow");

    /* Hack -- Overflow may induce circular queue */
    if (auto_key_tail == KEY_SIZE) auto_key_tail = 0;

    /* Success */
    return (0);
}


/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypresses(cptr str)
{
    cptr s;

    /* Enqueue them */
    for (s = str; *s; s++) borg_keypress(*s);

    /* Success */
    return (0);
}


/*
 * Get the next Borg keypress
 */
char borg_inkey(bool take)
{
    int i;

    /* Nothing ready */
    if (auto_key_head == auto_key_tail)
        return (0);

    /* Extract the keypress */
    i = auto_key_queue[auto_key_tail];

    /* Do not advance */
    if (!take) return (i);

    /* Advance the queue */
    auto_key_tail++;

    /* Circular queue requires wrap-around */
    if (auto_key_tail == KEY_SIZE) auto_key_tail = 0;

    /* Return the key */
    return (i);
}



/*
 * Get the next Borg keypress
 */
void borg_flush(void)
{
    /* Simply forget old keys */
    auto_key_tail = auto_key_head;
}






/*
 * Hack -- take a note later
 */
bool borg_tell(cptr what)
{
    cptr s;

    /* Hack -- self note */
    borg_keypress(':');
    for (s = what; *s; s++) borg_keypress(*s);
    borg_keypress('\n');

    /* Success */
    return (TRUE);
}



/*
 * Attempt to change the borg's name
 */
bool borg_change_name(cptr str)
{
    cptr s;

    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Character description */
    borg_keypress('C');

    /* Change the name */
    borg_keypress('c');

    /* Enter the new name */
    for (s = str; *s; s++) borg_keypress(*s);

    /* End the name */
    borg_keypress('\r');

    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Success */
    return (TRUE);
}


/*
 * Attempt to dump a character description file
 */
bool borg_dump_character(cptr str)
{
    cptr s;

    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Character description */
    borg_keypress('C');

    /* Dump character file */
    borg_keypress('f');

    /* Enter the new name */
    for (s = str; *s; s++) borg_keypress(*s);

    /* End the file name */
    borg_keypress('\r');

    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Success */
    return (TRUE);
}




/*
 * Attempt to save the game
 */
bool borg_save_game(void)
{
    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Save the game */
    borg_keypress('^');
    borg_keypress('S');

    /* Cancel everything */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);

    /* Success */
    return (TRUE);
}




/*
 * Update the Borg based on the current "frame"
 *
 * Assumes the Borg is actually in the dungeon.
 */
void borg_update_frame(void)
{
    int i;

#ifdef BORG_TK

    /* Extract the current level */
    auto_depth = p_ptr->depth;


    /* XXX XXX XXX Title (ignore) */


    /* XXX XXX XXX Info (monster health) */


    /* Assume level is fine */
    do_fix_lev = FALSE;

    /* Note "Lev" vs "LEV" */
    if (p_ptr->lev < p_ptr->max_lev) do_fix_lev = TRUE;

    /* Extract "LEVEL xxxxxx" */
    borg_skill[BI_CLEVEL] = p_ptr->lev;

    /* Note "***WINNER***" */
    borg_king = p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL);

    /* Assume experience is fine */
    do_fix_exp = FALSE;

    /* Note "Exp" vs "EXP" and am I lower than level 50*/
    if ((p_ptr->exp < p_ptr->max_exp) &&
        (p_ptr->lev != 50)) do_fix_exp = TRUE;

    /* Extract "EXP xxxxxxxx" */
    auto_exp = p_ptr->exp;


    /* Extract "AU xxxxxxxxx" */
    auto_gold = p_ptr->au;


    /* Extract "Fast (+x)" or "Slow (-x)" */
    auto_speed = p_ptr->pspeed;

    /* Check my float for decrementing variables */
    if (auto_speed >110)
    {
        borg_game_ratio = 100000/(((auto_speed-110)*10)+100);
    }
    else
    {
        borg_game_ratio = 1000;
    }

    /* if hasting, it doesn't count as 'auto_speed'.  The speed */
    /* gained from hasting is counted seperately. */
    if (borg_speed)
        auto_speed -= 10;

    /* Extract "Cur AC xxxxx" */
    auto_ac = p_ptr->dis_ac + p_ptr->dis_to_a;

    /* Extract "Max HP xxxxx" */
    auto_mhp = p_ptr->mhp;

    /* Extract "Cur HP xxxxx" */
    auto_chp = p_ptr->chp;

    /* Extract "Max SP xxxxx" (or zero) */
    auto_msp = p_ptr->msp;

    /* Extract "Cur SP xxxxx" (or zero) */
    auto_csp = p_ptr->csp;

    /* Clear all the "state flags" */
    do_weak = do_hungry = do_full = do_gorged = FALSE;
    do_blind = do_confused = do_afraid = do_poisoned = FALSE;
    do_cut = do_stun = do_heavy_stun = do_image = do_study = FALSE;

    /* Check for "Weak" */
    if (p_ptr->food < PY_FOOD_WEAK) do_weak = do_hungry = TRUE;

    /* Check for "Hungry" */
    else if (p_ptr->food < PY_FOOD_ALERT) do_hungry = TRUE;

    /* Check for "Normal" */
    else if (p_ptr->food < PY_FOOD_FULL) /* Nothing */;

    /* Check for "Full" */
    else if (p_ptr->food < PY_FOOD_MAX) do_full = TRUE;

    /* Check for "Gorged" */
    else do_gorged = do_full = TRUE;

    /* Check for "Blind" */
    if (p_ptr->blind) do_blind = TRUE;

    /* Check for "Confused" */
    if (p_ptr->confused) do_confused = TRUE;

    /* Check for "Afraid" */
    if (p_ptr->afraid) do_afraid = TRUE;

    /* Check for "Poisoned" */
    if (p_ptr->poisoned) do_poisoned = TRUE;

    /* Check for any text */
    if (p_ptr->cut) do_cut = TRUE;

    /* Check for Stun */
    if (p_ptr->stun && (p_ptr->stun <= 50)) do_stun = TRUE;

    /* Check for Heavy Stun */
    if (p_ptr->stun > 50) do_heavy_stun = TRUE;

    /* XXX XXX XXX Parse "State" */

    /* Check for "Study" */
    if (p_ptr->new_spells) do_study = TRUE;

    /* Parse stats */
    for (i = 0; i < 6; i++)
    {
        do_fix_stat[i] = p_ptr->stat_cur[i] < p_ptr->stat_max[i];
        auto_stat[i] = p_ptr->stat_cur[i];
    }

#else /* not BORG_TK */


    byte t_a;

    char buf[160];


    /* Check for "   Town" or "  Lev 8" or " Lev 13" */
    if (0 == borg_what_text(COL_DEPTH, ROW_DEPTH, -7, &t_a, buf))
    {
        cptr s;

        /* Skip the non-digits */
        for (s = buf; *s && !isdigit(*s); s++) /* loop */;

        /* Extract the current level */
        auto_depth = atoi(s);
    }


    /* XXX XXX XXX Title (ignore) */


    /* XXX XXX XXX Info (monster health) */


    /* Assume level is fine */
    do_fix_lev = FALSE;

    /* Check for drained level */
    if (0 == borg_what_text(COL_LEVEL, ROW_LEVEL, -3, &t_a, buf))
    {
        /* Note "Lev" vs "LEV" */
        if (islower(buf[2])) do_fix_lev = TRUE;
    }

    /* Extract current level */
    if (0 == borg_what_text(COL_LEVEL + 6, ROW_LEVEL, -6, &t_a, buf))
    {
        /* Extract "LEVEL xxxxxx" */
        borg_skill[BI_CLEVEL] = atoi(buf);
    }

    /* Extract current level */
    if (0 == borg_what_text(COL_TITLE, ROW_TITLE, -12, &t_a, buf))
    {
        /* Note "Lev" vs "LEV" */
        borg_king = (buf[0] == '*' &&
            buf[1] == '*' &&
            buf[2] == '*' &&
            buf[3] == 'W' &&
            buf[4] == 'I' &&
            buf[5] == 'N' &&
            buf[6] == 'N' &&
            buf[7] == 'E' &&
            buf[8] == 'R' &&
            buf[9] == '*' &&
            buf[10] == '*' &&
            buf[11] == '*');
    }

    /* Assume experience is fine */
    do_fix_exp = FALSE;

    /* Check for drained experience */
    if (0 == borg_what_text(COL_EXP, ROW_EXP, -3, &t_a, buf))
    {
        /* Note "Exp" vs "EXP" */
        if (islower(buf[2])) do_fix_exp = TRUE;
    }

    /* Extract current experience */
    if (0 == borg_what_text(COL_EXP + 4, ROW_EXP, -8, &t_a, buf))
    {
        /* Extract "EXP xxxxxxxx" */
        auto_exp = atol(buf);
    }


    /* Extract current gold */
    if (0 == borg_what_text(COL_GOLD + 3, ROW_GOLD, -9, &t_a, buf))
    {
        /* Extract "AU xxxxxxxxx" */
        auto_gold = atol(buf);
    }


    /* Extract speed */
    if (0 == borg_what_text(COL_SPEED, ROW_SPEED, -14, &t_a, buf))
    {
        /* Extract "Fast (+x)" or "Slow (-x)" */
        auto_speed = 110 + atoi(buf + 6);

        /* Check my float for decrementing variables */
        if (auto_speed >110)
        {
            borg_game_ratio = 100000/(((auto_speed-110)*10)+100);
        }
        else
        {
            borg_game_ratio = 1000;
        }
        /* if hasting, it doesn't count as 'auto_speed'.  The speed */
        /* gained from hasting is counted seperately. */
        if (borg_speed)
            auto_speed -= 10;
    }

    /* Extract armor class */
    if (0 == borg_what_text(COL_AC + 7, ROW_AC, -5, &t_a, buf))
    {
        /* Extract "Cur AC xxxxx" */
        auto_ac = atoi(buf);
    }


    /* Extract maximum hitpoints */
    if (0 == borg_what_text(COL_MAXHP + 7, ROW_MAXHP, -5, &t_a, buf))
    {
        /* Extract "Max HP xxxxx" */
        auto_mhp = atoi(buf);
    }

    /* Extract current hitpoints */
    if (0 == borg_what_text(COL_CURHP + 7, ROW_CURHP, -5, &t_a, buf))
    {
        /* Extract "Cur HP xxxxx" */
        auto_chp = atoi(buf);
    }


    /* Extract maximum spell points */
    if (0 == borg_what_text(COL_MAXSP + 7, ROW_MAXSP, -5, &t_a, buf))
    {
        /* Extract "Max SP xxxxx" (or zero) */
        auto_msp = atoi(buf);
    }

    /* Extract current spell points */
    if (0 == borg_what_text(COL_CURSP + 7, ROW_CURSP, -5, &t_a, buf))
    {
        /* Extract "Cur SP xxxxx" (or zero) */
        auto_csp = atoi(buf);
    }



    /* Clear all the "state flags" */
    do_weak = do_hungry = do_full = do_gorged = FALSE;
    do_blind = do_confused = do_afraid = do_poisoned = FALSE;
    do_cut = do_stun = do_heavy_stun = do_image = do_study = FALSE;

    /* Check for hunger */
    if (0 == borg_what_text(COL_HUNGRY, ROW_HUNGRY, -1, &t_a, buf))
    {
        /* Check for "Hungry" */
        if (buf[0] == 'H') do_hungry = TRUE;

        /* Check for "Weak" */
        if (buf[0] == 'W') do_weak = do_hungry = TRUE;

        /* Check for "Full" */
        if (buf[0] == 'F') do_full = TRUE;

        /* Check for "Gorged" */
        if (buf[0] == 'G') do_gorged = do_full = TRUE;
    }

    /* Check for blind */
    if (0 == borg_what_text(COL_BLIND, ROW_BLIND, -1, &t_a, buf))
    {
        /* Check for "Blind" */
        if (buf[0] == 'B') do_blind = TRUE;
    }

    /* Check for confused */
    if (0 == borg_what_text(COL_CONFUSED, ROW_CONFUSED, -1, &t_a, buf))
    {
        /* Check for "Confused" */
        if (buf[0] == 'C') do_confused = TRUE;
    }

    /* Check for afraid */
    if (0 == borg_what_text(COL_AFRAID, ROW_AFRAID, -1, &t_a, buf))
    {
        /* Check for "Afraid" */
        if (buf[0] == 'A') do_afraid = TRUE;
    }

    /* Check for poisoned */
    if (0 == borg_what_text(COL_POISONED, ROW_POISONED, -1, &t_a, buf))
    {
        /* Check for "Poisoned" */
        if (buf[0] == 'P') do_poisoned = TRUE;
    }


    /* XXX XXX Check for cut */
    if (0 == borg_what_text(COL_CUT, ROW_CUT, -1, &t_a, buf))
    {
        /* Check for any text */
        if (isalpha(buf[0])) do_cut = TRUE;
    }

    /* XXX XXX Check for stun */
    if (0 == borg_what_text(COL_STUN, ROW_STUN, -1, &t_a, buf))
    {
        /* Check for Stun */
        if (buf[0] == 'S') do_stun = TRUE;

        /* Check for Heavy Stun */
        if (buf[0] == 'H') do_heavy_stun = TRUE;
    }


    /* XXX XXX XXX Parse "State" */


    /* Check for study */
    if (0 == borg_what_text(COL_STUDY, ROW_STUDY, -1, &t_a, buf))
    {
        /* Check for "Study" */
        if (buf[0] == 'S') do_study = TRUE;
    }


    /* Parse stats */
    for (i = 0; i < 6; i++)
    {
        /* Check "NNN   xxxxxx" */
        if (0 == borg_what_text(COL_STAT, ROW_STAT+i, -3, &t_a, buf))
        {
            /* Note "Nnn" vs "NNN" (in Linux a value > 256 is returned) */
            do_fix_stat[i] = (islower(buf[2]) != 0);
        }

        /* Check "NNN   xxxxxx" */
        if (0 == borg_what_text(COL_STAT+6, ROW_STAT+i, -6, &t_a, buf))
        {
            /* Parse "18/..." */
            if (buf[5] == '*') auto_stat[i] = 18 + 220;

            /* Parse "18/NNN" */
            else if (buf[2] == '/') auto_stat[i] = 18 + atoi(buf+3);

            /* Parse " 18/NN" */
            else if (buf[3] == '/') auto_stat[i] = 18 + atoi(buf+4);

            /* Parse "    NN" */
            else auto_stat[i] = atoi(buf+4);
        }
    }

#endif /* not BORG_TK */
}

int
borg_check_formula(int *formula)
{
    int     oper1;          /* operand #1 */
    int     oper2;          /* operand #2 */
    int     stack[256];     /* stack */
    int     *stackptr;      /* stack pointer */

    /* loop until we hit BFO_DONE */
    for (stackptr = stack; *formula; formula++)
    {
        if (stackptr < stack)
            return 0;
        switch (*formula)
        {
            /* Number */
            case BFO_NUMBER:
                *stackptr++ = *++formula;
                break;

            /* Variable */
            case BFO_VARIABLE:
                *stackptr++ = borg_has[*++formula];
                if ((*formula) > (z_info->k_max + z_info->k_max + z_info->a_max + BI_MAX))
                    return 0;
                break;

            /* Equal */
            case BFO_EQ:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 == oper2);
                break;

            /* Not Equal */
            case BFO_NEQ:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 != oper2);
                break;

            /* Less Than */
            case BFO_LT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 < oper2);
                break;

            /* Less Than Or Equal */
            case BFO_LTE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 <= oper2);
                break;

            /* Greater Than */
            case BFO_GT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 > oper2);
                break;

            /* Greater Than Or Equal */
            case BFO_GTE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 >= oper2);
                break;

            /* Logical And */
            case BFO_AND:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 && oper2);
                break;

            /* Logical Or */
            case BFO_OR:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 || oper2);
                break;

            /* Plus */
            case BFO_PLUS:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 + oper2);
                break;

            /* Minus */
            case BFO_MINUS:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 - oper2);
                break;

            /* Divide */
            case BFO_DIVIDE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 / (oper2 ? oper2 : 1));
                break;

            /* Multiply */
            case BFO_MULT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 * oper2);
                break;

            /* Logical Not */
            case BFO_NOT:
                oper1 = *--stackptr;
                *stackptr++ = (!oper1);
                break;

            default:
                return 0;
        }
    }

    if (stackptr != (stack+1))
        return 0;
    return 1;
}

cptr
borg_prt_formula(int *formula)
{
    static char FormulaStr[2000];
    char tmpStr[50];

    memset(FormulaStr, 0, sizeof(FormulaStr));
    /* loop until we hit BFO_DONE */
    for (; *formula; formula++)
    {
        switch (*formula)
        {
            /* Number */
            case BFO_NUMBER:

                sprintf(tmpStr, "%d ", *++formula);
                strcat(FormulaStr, tmpStr);
                break;

            /* Variable */
            case BFO_VARIABLE:
                strcat(FormulaStr, "'");
                strcat(FormulaStr, borg_prt_item(*++formula));
                strcat(FormulaStr, "'");
                strcat(FormulaStr, " ");
                break;

            /* Equal */
            case BFO_EQ:
                strcat(FormulaStr, "== ");
                break;

            /* Not Equal */
            case BFO_NEQ:
                strcat(FormulaStr, "!= ");
                break;

            /* Less Than */
            case BFO_LT:
                strcat(FormulaStr, "< ");
                break;

            /* Less Than Or Equal */
            case BFO_LTE:
                strcat(FormulaStr, "<= ");
                break;

            /* Greater Than */
            case BFO_GT:
                strcat(FormulaStr, "> ");
                break;

            /* Greater Than Or Equal */
            case BFO_GTE:
                strcat(FormulaStr, ">= ");
                break;

            /* Logical And */
            case BFO_AND:
                strcat(FormulaStr, "&& ");
                break;

            /* Logical Or */
            case BFO_OR:
                strcat(FormulaStr, "|| ");
                break;

            /* Plus */
            case BFO_PLUS:
                strcat(FormulaStr, "+ ");
                break;

            /* Minus */
            case BFO_MINUS:
                strcat(FormulaStr, "- ");
                break;

            /* Divide */
            case BFO_DIVIDE:
                strcat(FormulaStr, "/ ");
                break;

            /* Multiply */
            case BFO_MULT:
                strcat(FormulaStr, "* ");
                break;

            /* Logical Not */
            case BFO_NOT:
                strcat(FormulaStr, "! ");
                break;
        }
    }

    /* BFO_DONE */
    return FormulaStr;
}

int
borg_calc_formula(int *formula)
{
    int     oper1;          /* operand #1 */
    int     oper2;          /* operand #2 */
    int     stack[256];     /* stack */
    int     *stackptr;      /* stack pointer */


    if (!formula)
        return 0;

    *stack = 0;
    /* loop until we hit BFO_DONE */
    for (stackptr = stack; *formula; formula++)
    {
        switch (*formula)
        {
            /* Number */
            case BFO_NUMBER:
                *stackptr++ = *++formula;
                break;

            /* Variable */
            case BFO_VARIABLE:
                *stackptr++ = borg_has[*++formula];
                break;

            /* Equal */
            case BFO_EQ:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 == oper2);
                break;

            /* Not Equal */
            case BFO_NEQ:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 != oper2);
                break;

            /* Less Than */
            case BFO_LT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 < oper2);
                break;

            /* Less Than Or Equal */
            case BFO_LTE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 <= oper2);
                break;

            /* Greater Than */
            case BFO_GT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 > oper2);
                break;

            /* Greater Than Or Equal */
            case BFO_GTE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 >= oper2);
                break;

            /* Logical And */
            case BFO_AND:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 && oper2);
                break;

            /* Logical Or */
            case BFO_OR:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 || oper2);
                break;

            /* Plus */
            case BFO_PLUS:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 + oper2);
                break;

            /* Minus */
            case BFO_MINUS:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 - oper2);
                break;

            /* Divide */
            case BFO_DIVIDE:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 / (oper2 ? oper2 : 1));
                break;

            /* Multiply */
            case BFO_MULT:
                oper2 = *--stackptr;
                oper1 = *--stackptr;
                *stackptr++ = (oper1 * oper2);
                break;

            /* Logical Not */
            case BFO_NOT:
                oper1 = *--stackptr;
                *stackptr++ = (!oper1);
                break;
        }
    }

    /* BFO_DONE */
    return *--stackptr;
}


/*
 * Initialize this file
 */
void borg_init_1(void)
{
    int i, x, y;


    /* Allocate the "keypress queue" */
    C_MAKE(auto_key_queue, KEY_SIZE, char);


    /* Prapare a local random number seed */
if (!auto_rand_local)
    auto_rand_local = rand_int(0x10000000);



#ifdef BORG_ROOMS

    /*** Rooms ***/

    /* Make the array of rooms */
    C_MAKE(auto_rooms, AUTO_ROOMS, auto_room);

    /* Initialize the rooms */
    for (i = 0; i < AUTO_ROOMS; i++)
    {
        /* Save our own index */
        auto_rooms[i].self = i;

        /* Initialize the "free" list */
        auto_rooms[i].free = i + 1;
    }

    /* Save the head/tail of the room array */
    auto_room_head = &auto_rooms[0];
    auto_room_tail = &auto_rooms[AUTO_ROOMS-1];

    /* Prepare the "tail" of the free list */
    auto_room_tail->free = auto_room_tail->self;

    /* Reset the free list */
    auto_room_head->free = 1;

    /* Maximum room index */
    auto_room_max = 1;

#endif


    /*** Grids ***/

    /* Make each row of grids */
    for (y = 0; y < AUTO_MAX_Y; y++)
    {
        /* Make each row */
        C_MAKE(auto_grids[y], AUTO_MAX_X, auto_grid);
    }


    /*** Grid data ***/

    /* Allocate */
    MAKE(auto_data_flow, auto_data);

    /* Allocate */
    MAKE(auto_data_cost, auto_data);

    /* Allocate */
    MAKE(auto_data_hard, auto_data);

    /* Allocate */
    MAKE(auto_data_know, auto_data);

    /* Allocate */
    MAKE(auto_data_icky, auto_data);

    /* Prepare "auto_data_hard" */
    for (y = 0; y < AUTO_MAX_Y; y++)
    {
        for (x = 0; x < AUTO_MAX_X; x++)
        {
            /* Prepare "auto_data_hard" */
            auto_data_hard->data[y][x] = 255;
        }
    }


    /*** Very special "tracking" array ***/

    /* Track the shop locations */
    C_MAKE(track_shop_x, 9, byte);
    C_MAKE(track_shop_y, 9, byte);


    /*** Special "tracking" arrays ***/

    /* Track "up" stairs */
    track_less_num = 0;
    track_less_size = 16;
    C_MAKE(track_less_x, track_less_size, byte);
    C_MAKE(track_less_y, track_less_size, byte);

    /* Track "down" stairs */
    track_more_num = 0;
    track_more_size = 16;
    C_MAKE(track_more_x, track_more_size, byte);
    C_MAKE(track_more_y, track_more_size, byte);

    /* Track glyphs */
    track_glyph_num = 0;
    track_glyph_size = 256;
    C_MAKE(track_glyph_x, track_glyph_size, byte);
    C_MAKE(track_glyph_y, track_glyph_size, byte);

    /* Track Steps */
    track_step_num = 0;
    track_step_size = 256;
    C_MAKE(track_step_x, track_step_size, byte);
    C_MAKE(track_step_y, track_step_size, byte);

    /* Track closed doors */
    track_door_num = 0;
    track_door_size = 256;
    C_MAKE(track_door_x, track_door_size, byte);
    C_MAKE(track_door_y, track_door_size, byte);

    /*** Object tracking ***/

    /* No objects yet */
    auto_takes_cnt = 0;
    auto_takes_nxt = 1;

    /* Array of objects */
    C_MAKE(auto_takes, 256, auto_take);

    /* Scan the objects */
    for (i = 0; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip non-items */
        if (!k_ptr->name) continue;

        /* Notice this object */
        auto_is_take[(byte)(k_ptr->d_char)] = TRUE;
    }

    /*** Monster tracking ***/

    /* No monsters yet */
    auto_kills_cnt = 0;
    auto_kills_nxt = 1;

    /* Array of monsters */
    C_MAKE(auto_kills, 256, auto_kill);

    /* Scan the monsters */
    for (i = 1; i < z_info->r_max-1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip non-monsters */
        if (!r_ptr->name) continue;
#if 0
        /* Hack -- Skip "clear" monsters XXX XXX XXX */
        if (r_ptr->flags1 & RF1_CHAR_CLEAR) continue;

        /* Hack -- Skip "multi" monsters XXX XXX XXX */
        if (r_ptr->flags1 & RF1_CHAR_MULTI) continue;
#endif
        /* Notice this monster */
        auto_is_kill[(byte)(r_ptr->d_char)] = TRUE;
    }


    /*** Special counters ***/

    /* Count racial appearances */
    C_MAKE(auto_race_count, z_info->r_max, s16b);

    /* Count racial deaths */
    C_MAKE(auto_race_death, z_info->r_max, s16b);


    /*** XXX XXX XXX Hack -- Cheat ***/

    /* Hack -- Extract dead uniques */
    for (i = 1; i < z_info->r_max-1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip non-monsters */
        if (!r_ptr->name) continue;

        /* Skip non-uniques */
        if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

        /* Mega-Hack -- Access "dead unique" list */
        if (r_ptr->max_num == 0) auto_race_death[i] = 1;
    }

    /* Hack -- Access max depth */
    auto_max_depth = p_ptr->max_depth;

}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
