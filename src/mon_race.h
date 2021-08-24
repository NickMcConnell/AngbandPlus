#ifndef INCLUDED_MON_RACE_H
#define INCLUDED_MON_RACE_H

#include "c-vec.h"

/************************************************************************
 * Monster Race Type
 *
 * Note: Flags are still grouped by classification. We use RF_ style for
 * historical continuity (R meaning monster Race ... cf r_info et al).
 * In addition, to prevent coding errors, the 3rd letter indicates the
 * grouping. For example, allocation information is gathered in the 
 * type mon_alloc_s whose flags are denoted by RFA_*. And mobility info
 * is gathered in the type mon_move_s whose flags are denoted by RFM_*.
 ************************************************************************/

/* Allocation Info */
enum {
    RFA_UNIQUE =        0x000001,
    RFA_UNIQUE2 =       0x000002,
    RFA_GUARDIAN =      0x000004,
    RFA_FORCE_DEPTH =   0x000008,
    RFA_ESCORT =        0x000010,
    RFA_NO_QUEST =      0x000020,
    RFA_NO_SUMMON =     0x000040,
    RFA_WEB =           0x000080, /* spiders spawn with webs (and skeletons) */

    RFA_WILD_ONLY =     0x001000,
    RFA_WILD_TOWN =     0x002000,

    RFA_WILD_SHORE =    0x010000,
    RFA_WILD_OCEAN =    0x020000,
    RFA_WILD_WASTE =    0x040000,
    RFA_WILD_WOOD =     0x080000,
    RFA_WILD_VOLCANO =  0x100000,
    RFA_WILD_MOUNTAIN = 0x200000,
    RFA_WILD_GRASS =    0x400000,
    RFA_WILD_SWAMP =    0x800000, /* XXX not implemented? XXX */
    RFA_WILD_ALL =      RFA_WILD_WASTE | RFA_WILD_WOOD | RFA_WILD_GRASS, /* sic */
};
struct mon_alloc_s
{
    u32b flags;
    u16b world_id;     /* restrict mon_race to a specific world */
    u16b dun_type_id;  /* restrict mon_race to a specific dungeon */

    byte max_max_num;  /* 1: unique; 5: nazgul; 7: camelot knight, etc 0 implies no restriction on cur_num */
    byte max_num;      /* Maximum population allowed per level */
    s16b cur_num;      /* Monster population on current level */

    byte lvl;
    byte max_lvl;
    byte rarity;
};
typedef struct mon_alloc_s mon_alloc_t, *mon_alloc_ptr;

/* Display Info */
enum { /* These flags are mostly for ASCII mode (except SHAPECHANGER) ... cf map_info */
    RFD_CHAR_CLEAR =     0x0001, /* character 'absorbs' object or feature char at this location */
    RFD_ATTR_CLEAR =     0x0002, /* attribute 'absorbs' object or feature char at this location */
    RFD_ATTR_MULTI =     0x0004, /* attribute randomly flickers (e.g. randint0(16)) (cf ATTR_ANY) */
    RFD_ATTR_ANY =       0x0008, /* random attribute can be any of 16 colors (cf ATTR_MULTI) */
    RFD_ATTR_SEMIRAND =  0x0010, /* attribute is random, but fixed (e.g. mon->id % 16) */
    RFD_SHAPECHANGER =   0x0020, /* term_char flickers (e.g. vec_random(mon_alloc_tbl)) */
};
struct mon_display_s {
    u16b flags;
    char c;
};
typedef struct mon_display_s mon_display_t, *mon_display_ptr;

/* Mobility */
enum {
    RFM_NEVER =      0x000001,
    RFM_OPEN =       0x000002,
    RFM_BASH =       0x000004,
    RFM_PUSH =       0x000008,
    RFM_TRAMPLE =    0x000010,
    RFM_PICKUP =     0x000020,
    RFM_DESTROY =    0x000040,
    RFM_TUNNEL =     0x000080,
    RFM_PASSWALL =   0x000100,
    RFM_SWIM =       0x000200,
    RFM_FLY =        0x000400,
    RFM_CLIMB =      0x000800,
    RFM_TRUMP =      0x001000,
    RFM_QUICK =      0x002000,
    RFM_PASSWEB =    0x004000,
    RFM_CLEARWEB =   0x008000,
};
struct mon_move_s {
    u32b flags;
    s16b speed;
    s16b sleep;
    byte range;
    byte random;  /* XXX formerly RAND_25 | RAND_50 */
};
typedef struct mon_move_s mon_move_t, *mon_move_ptr;

/* alignment ranges from -255 to +255 ... cf your_alignment() for plr descriptive ranges
 * These values are for ALIGN() directives in r_info */
enum align_e {
    ALIGN_CHAOTIC =      -200,
    ALIGN_VERY_EVIL =    -150,
    ALIGN_EVIL =         -100,
    ALIGN_NEUTRAL_EVIL =  -25,
    ALIGN_NEUTRAL =         0,
    ALIGN_NEUTRAL_GOOD =   25,
    ALIGN_GOOD =          100,
    ALIGN_VERY_GOOD =     150,
    ALIGN_LAWFUL =        200
};

/* Monster Kind (e.g. Orc, Troll, Dragon ...). Alignment is not a 'kind',
 * nor is 'Unique'. cf mon_race->align and RFA_UNIQUE (mon_race->alloc.flags). */
enum mon_kind_e {
    RFK_ORC =        0x000001,
    RFK_TROLL =      0x000002,
    RFK_GIANT =      0x000004,
    RFK_DRAGON =     0x000008,
    RFK_DEMON =      0x000010,
    RFK_UNDEAD =     0x000020,
    RFK_ANIMAL =     0x000040,
    RFK_HUMAN =      0x000080,
    RFK_ELF =        0x000100,
    RFK_DARK_ELF =   0x000200,
    RFK_HOBBIT =     0x000400,
    RFK_DWARF =      0x000800,
    RFK_AMBERITE =   0x001000,
    RFK_THIEF =      0x002000,
    RFK_KNIGHT =     0x004000,
    RFK_OLYMPIAN =   0x008000,
    RFK_NONLIVING =  0x010000,
    RFK_AQUATIC =    0x020000,
    RFK_NAZGUL =     0x040000, /* XXX could probably be removed ... */
    RFK_HORROR =     0x080000,
};

/* abilities and attributes are general RF_ flags */
enum mon_ability_e { /* mon_race->abilities */
    RF_SPEAK =       0x0001,
    RF_REFLECT =     0x0002,
    RF_INVIS =       0x0004,
    RF_MULTIPLY =    0x0008,
    RF_REGEN =       0x0010,
    RF_REVENGE =     0x0020,
    RF_FEAR =        0x0040,
};
enum mon_attribute_e { /* mon_race->attributes */
    RF_MALE =        0x0001,
    RF_FEMALE =      0x0002,
    RF_SMART =       0x0004,
    RF_STUPID =      0x0008,
    RF_WEIRD_MIND =  0x0010,
    RF_EMPTY_MIND =  0x0020,
    RF_COLD_BLOOD =  0x0040,
    RF_FRIENDLY =    0x0080,
    RF_RIDING =      0x0100,
    RF_KILL_EXP =    0x0200,
    RF_IM_ILLUSION = 0x0400,
    RF_TEMPLATE =    0x20000000,
    RF_DEPRECATED =  0x40000000
};

/* mon_body for possessor */
struct skills_s
{
    s16b dis;            /* disarming */
    s16b dev;            /* magic devices */
    s16b sav;            /* saving throw */
    s16b stl;            /* stealth */
    s16b srh;            /* search ability */
    s16b fos;            /* search frequency */
    s16b thn;            /* combat (normal) */
    s16b thb;            /* combat (shooting) */
};
struct blows_calc_s
{
    s16b max;
    s16b wgt;
    s16b mul;
};

enum mon_body_e {
    RF_DROP_CORPSE =       0x00000001,
    RF_DROP_SKELETON =     0x00000002,
    RF_POS_GAIN_AC =       0x00000004,
    RF_POS_TELEPATHY =     0x00000008,
    RF_POS_SEE_INVIS =     0x00000010,
    RF_POS_HOLD_LIFE =     0x00000020,
    RF_POS_SUST_STR =      0x00000040,
    RF_POS_SUST_INT =      0x00000080,
    RF_POS_SUST_WIS =      0x00000100,
    RF_POS_SUST_DEX =      0x00000200,
    RF_POS_SUST_CON =      0x00000400,
    RF_POS_SUST_CHR =      0x00000800,
    RF_POS_BACKSTAB =      0x00001000,
    RF_POS_DISABLED =      0x00002000,
};
struct mon_body_s
{
    u32b     flags;
    s16b     stats[MAX_STATS];
    s16b     extra_stats[MAX_STATS];
    skills_t skills;
    skills_t extra_skills;
    s16b     life;
    s16b     infra;
    s16b     spell_stat;
    sym_t    body_id;
    s16b     class_id;
    s16b     speed;
    blows_calc_t blows_calc;
};
typedef struct mon_body_s mon_body_t, *mon_body_ptr;

struct mon_aura_s
{
    mon_aura_ptr next;
    s16b   gf;
    dice_t dam;
    byte   pct;
    u16b   lore;
};

/* evolution */
typedef struct {
    sym_t id;
    u32b  exp;
} mon_evolution_t;

/* lore */
enum mon_lore_e {
    RFL_PACT =      0x0001, /* Warlock */
    RFL_PROBE =     0x0002,
    RFL_EVOLUTION = 0x0004,
    RFL_POSSESSOR = 0x0008,
    RFL_ALIGN =     0x0010,
};
typedef struct { /* track kills by plr */
    u16b total;   /* from all games */
    u16b current; /* current game */
} lore_kills_t;
typedef struct { /* track monster actions observed by plr from all games */
    u32b total;  /* includes spells, moves and attacks (but not sleeping turns) */
    u32b spell;  /* spell/total gives accurate spell frequency (observed) */
    u16b wake;   /* counts monster wakings to reveal race->move.sleep */
    u16b sleep;  /* sleeping turns ... not part of total */
} lore_turns_t;
typedef struct { /* track maximal object drops by kind from all games */
    byte gold;   /* e.g. "drops up to 12 treasures" */
    byte obj;    /* e.g. "drops 1 or 2 items" */
} lore_drops_t;
struct mon_lore_s
{
    u32b flags;

    u32b move;
    u32b kind;       /* orc, troll, dragon, etc. */
    u32b abilities;  /* regenerate, invisible, multiply, etc. */
    u32b attributes; /* male, smart, riding, etc. */
    u32b resist;

    u16b sightings;
    u16b deaths;
    lore_kills_t kills;
    lore_turns_t turns;
    lore_drops_t drops;
};
typedef struct mon_lore_s mon_lore_t, *mon_lore_ptr;

struct mon_race_s
{
    sym_t id;
    sym_t base_id;
    mon_display_t display;

    cptr name;
    cptr text;

    mon_alloc_t alloc;
    mon_move_t move;

    dice_t hp;

    s16b ac;                  /* Armour Class: Always use mon_ac(mon) instead! */
    s16b align;
    s16b light;               /* SELF_LIGHT or SELF_DARK (<0) */
    s16b lantern;             /* HAS_LIGHT or HAS_DARK (<0) cf _MAX_MON_LIGHT */
    s16b weight;
    byte flagsx;              /* Temp Flags valid only for a single game. Written to savefile.
                                 For example, this unique is a questor. Or this
                                 unique is suppressed and won't appear in this game. */
    byte stolen_ct;           /* For uniques in this lifetime only. Prevents PickPocket scumming of excellent drop uniques */

    s32b mexp;                /* Exp value for kill */

    mon_spells_ptr spells;
    mon_drop_ptr drops;
    mon_rule_ptr friends;     /* place_monster: creates a mon_pack */
    mon_rule_ptr kin;         /* S_KIN: who answers my summons (e.g. p.Rolento calls {.grenade) */

    u32b kind;                /* orc, troll, dragon, etc. */
    u32b abilities;           /* regenerate, invisible, multiply, etc. */
    u32b attributes;          /* male, smart, riding, etc. */
    u32b resist;
    u32b immune;
    u32b vuln;

    vec_ptr blows;
    mon_aura_ptr auras;

    mon_evolution_t evolution;

    mon_body_t body;          /* For The Possessor */
    mon_lore_t lore;
    int prob;                 /* scratch variable for mon_alloc_choose_aux2 */
};

/************************************************************************
 * Global Monster Races (r_info)
 ************************************************************************/
extern mon_race_ptr mon_race_alloc(sym_t id);
extern mon_race_ptr mon_race_alloc_ex(sym_t id, mon_race_ptr base);
extern void         mon_race_free(mon_race_ptr race);

extern mon_race_ptr mon_race_lookup(sym_t id);
extern mon_race_ptr mon_race_parse(cptr token);
extern vec_ptr      mon_race_filter(mon_race_p filter);
extern void         mon_race_iter(mon_race_f f);

extern bool         mon_race_init(void); /* parse r_info ... init_angband */
extern void         mon_race_reset(void); /* reset lore et. al. ... player_wipe */
extern void         mon_race_save(savefile_ptr file);
extern void         mon_race_load(savefile_ptr file);

extern bool mon_race_is_(mon_race_ptr race, cptr which);
extern bool mon_race_is_one_(mon_race_ptr race, cptr which[]);
extern bool mon_race_is_d_char(char c); /* init1.c */


/************************************************************************
 * Monster Allocation
 ************************************************************************/
typedef int (*mon_alloc_weight_f)(mon_race_ptr race, int prob);

extern vec_ptr mon_alloc_tbl; /* vec<mon_race_ptr> */
extern void mon_alloc_init(void);
extern vec_ptr mon_alloc_current_tbl(void);
extern void mon_alloc_clear_filters(void);
extern void mon_alloc_push_filter(mon_race_p filter);
extern void mon_alloc_pop_filter(void);
extern void mon_alloc_push_weight(mon_alloc_weight_f weight);
extern void mon_alloc_pop_weight(void);
extern mon_race_ptr mon_alloc_choose(int level);
extern mon_race_ptr mon_alloc_choose_aux(int level, u32b options);
extern mon_race_ptr mon_alloc_choose_aux2(vec_ptr tbl, int level, int min_level, u32b options);

/* surface checks */
extern bool mon_alloc_town(mon_race_ptr race);
extern bool mon_alloc_ocean(mon_race_ptr race);
extern bool mon_alloc_shore(mon_race_ptr race);
extern bool mon_alloc_waste(mon_race_ptr race);
extern bool mon_alloc_grass(mon_race_ptr race);
extern bool mon_alloc_woods(mon_race_ptr race);
extern bool mon_alloc_volcano(mon_race_ptr race);
extern bool mon_alloc_mountain(mon_race_ptr race);
extern bool mon_alloc_surface(mon_race_ptr race);

/* dungeon checks */
extern bool mon_alloc_deep_water(mon_race_ptr race);
extern bool mon_alloc_shallow_water(mon_race_ptr race);
extern bool mon_alloc_lava(mon_race_ptr race);
extern bool mon_alloc_floor(mon_race_ptr race);
extern bool mon_alloc_dungeon(mon_race_ptr race);

extern mon_race_p mon_alloc_cell_p(dun_cell_ptr cell);


#endif
