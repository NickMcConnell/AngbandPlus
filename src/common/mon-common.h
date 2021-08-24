/*
 * File: mon-common.h
 * Purpose: Flags, structures and variables for monsters
 */

#ifndef INCLUDED_MONSTER_COMMON_H
#define INCLUDED_MONSTER_COMMON_H

/** Monster flags **/

/*
 * Special Monster Flags (all temporary)
 */
enum
{
    #define MFLAG(a, b) MFLAG_##a,
    #include "list-mon-temp-flags.h"
    #undef MFLAG
    MFLAG_MAX
};

#define MFLAG_SIZE FLAG_SIZE(MFLAG_MAX)

/*
 * Monster property and ability flags (race flags)
 */
enum
{
    #define RF(a, b, c) RF_##a,
    #include "list-mon-race-flags.h"
    #undef RF
    RF_MAX
};

#define RF_SIZE FLAG_SIZE(RF_MAX)

/*
 * Spell type bitflags
 */
enum
{
    RST_BOLT    = 0x0001,
    RST_BALL    = 0x0002,
    RST_BREATH  = 0x0004,
    RST_ATTACK  = 0x0008,   /* Direct (non-projectable) attacks */
    RST_ANNOY   = 0x0010,   /* Irritant spells, usually non-fatal */
    RST_HASTE   = 0x0020,   /* Relative speed advantage */
    RST_HEAL    = 0x0040,
    RST_TACTIC  = 0x0080,   /* Get a better position */
    RST_ESCAPE  = 0x0100,
    RST_SUMMON  = 0x0200,
    RST_INNATE  = 0x0400,
    RST_MISSILE = 0x0800,
    RST_HACK    = 0x1000    /* Hack -- projectable attacks that are not an element */
};

typedef enum
{
    RSV_SKILL  = 0x01,
    RSV_UNDEAD = 0x02
} mon_spell_save;

/*
 * Monster spell flag indices
 */
enum
{
    #define RSF(a, b, c) RSF_##a,
    #include "list-mon-spells.h"
    #undef RSF
    RSF_MAX
};

#define RSF_SIZE    FLAG_SIZE(RSF_MAX)

/*
 * Monster Timed Effects
 */
enum
{
    #define MON_TMD(a, b, c, d, e, f) MON_TMD_##a,
    #include "list-mon-timed.h"
    #undef MON_TMD

    MON_TMD_MAX
};

/** Structures **/

/*
 * Monster blows
 */
struct monster_blow
{
    struct monster_blow *next;

    byte method;        /* Method (RBM_*) */
    byte effect;        /* Effect (RBE_*) */
    random_value dice;  /* Damage Dice */
};

/*
 * Monster pain messages
 */
struct monster_pain
{
	const char *messages[7];
	int pain_idx;

	struct monster_pain *next;
};

/*
 * Base monster type
 */
struct monster_base
{
	struct monster_base *next;

	char *name;                     /* Name for recognition in code */
	char *text;                     /* In-game name */

	bitflag flags[RF_SIZE];         /* Flags */
	bitflag spell_flags[RSF_SIZE];  /* Spell flags */

	char d_char;                    /* Default monster character */

	struct monster_pain *pain;      /* Pain messages */
};

/*
 * Specified monster drops
 */
struct monster_drop
{
    struct monster_drop *next;
    struct object_kind *kind;
    struct artifact *artifact;
    unsigned int percent_chance;
    unsigned int min;
    unsigned int max;
};

/*
 * Monster friends (specific monster)
 */
struct monster_friends
{
    struct monster_friends *next;
    char *name;
    struct monster_race *race;
    unsigned int percent_chance;
    unsigned int number_dice;
    unsigned int number_side;
};

/*
 * Monster friends (general type)
 */
struct monster_friends_base
{
    struct monster_friends_base *next;
    struct monster_base *base;
    unsigned int percent_chance;
    unsigned int number_dice;
    unsigned int number_side;
};

/*
 * How monsters mimic
 */
struct monster_mimic
{
    struct monster_mimic *next;
    struct object_kind *kind;
};

/*
 * Monster "lore" information
 */
struct monster_lore
{
    byte spawned;                       /* Unique has spawned (global) */
    byte seen;                          /* Unique has been seen (global) */
    byte pseen;                         /* Race has been seen (player) */
    s16b pdeaths;                       /* Count deaths from this monster (player) */
    s16b tdeaths;                       /* Count all deaths from this monster (global) */
    s16b pkills;                        /* Count monsters killed in this life (player) */
    s16b tkills;                        /* Count monsters killed in all lives (global) */
    byte wake;                          /* Number of times woken up (player) */
    byte ignore;                        /* Number of times ignored (player) */
    byte cast_innate;                   /* Max number of innate spells seen (player) */
    byte cast_spell;                    /* Max number of other spells seen (player) */
    byte *blows;                        /* Number of times each blow type was seen (player) */
    bitflag flags[RF_SIZE];             /* Observed racial flags (player) */
    bitflag spell_flags[RSF_SIZE];      /* Observed racial spell flags (player) */

    /* Derived known fields, put here for simplicity */
    bool all_known;
    bool *blow_known;
    bool armour_known;
    bool drop_known;
    bool sleep_known;
    bool spell_freq_known;
};

/*
 * Monster "race" information, including racial memories
 *
 * Note that "d_attr" and "d_char" are used for MORE than "visual" stuff.
 */
struct monster_race
{
    struct monster_race *next;
    unsigned int ridx;                      /* Index */
    char *name;                             /* Name */
    char *text;                             /* Text */
    char *plural;                           /* Optional pluralized name */
    struct monster_base *base;
    int avg_hp;                             /* Average HP for this creature */
    int ac;                                 /* Armour Class */
    int sleep;                              /* Inactive counter (base) */
    int aaf;                                /* Area affect radius (1-100) */
    int speed;                              /* Speed (normally 110) */
    int mexp;                               /* Exp value for kill */
    u32b power;                             /* Monster power */
    u32b scaled_power;                      /* Monster power scaled by level */
    int freq_spell;                         /* Spell frequency */
    bitflag flags[RF_SIZE];                 /* Flags */
    bitflag spell_flags[RSF_SIZE];          /* Spell flags */
    struct monster_blow *blow;              /* Melee blows */
    int level;                              /* Level of creature */
    int rarity;                             /* Rarity of creature */
    byte d_attr;                            /* Default monster attribute */
    char d_char;                            /* Default monster character */
    s16b extra;                             /* Corpse weight */
    struct monster_lore lore;               /* Monster "lore" information */
    struct monster_drop *drops;
    struct monster_friends *friends;
    struct monster_friends_base *friends_base;
    struct monster_mimic *mimic_kinds;
};

/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 *
 * The "held_obj" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */
struct monster
{
    struct monster_race *race;
    int midx;
    byte fy;                            /* Y location on map */
    byte fx;                            /* X location on map */
    s16b hp;                            /* Current Hit points */
    s16b maxhp;                         /* Max Hit points */
    s16b m_timed[MON_TMD_MAX];          /* Timed monster status effects */
    byte mspeed;                        /* Monster "speed" */
    s32b energy;                        /* Monster "energy" */
    byte cdis;                          /* Current dis from player (transient) */
    bool unaware;                       /* Players don't know this is a monster */
    bool handled;                       /* Monster has been processed this turn */
    struct object *mimicked_obj;        /* Object this monster is mimicking */
    struct object *held_obj;            /* Object being held (if any) */
    byte attr;                          /* "attr" last used for drawing monster */
    struct player_state known_pstate;   /* Known player state */
    byte ty;                            /* Monster target (transient) */
    byte tx;
    byte min_range;                     /* Minimum combat range (transient) */

    /* MAngband */
    s16b depth;                         /* Level of the dungeon */
    struct player *closest_player;      /* The player closest to this monster (transient) */

    /* PWMAngband */
    s16b ac;                            /* Armour Class */
    struct monster_blow *blow;          /* Melee blows */
    s16b level;                         /* Level of creature */
    s16b master;                        /* The player controlling this monster */
    byte lifespan;                      /* Lifespan of controlled creature */
    byte resilient;                     /* Controlled creature is resilient */
    byte status;                        /* Monster status: hostile, guard, follower, attacker */
    byte clone;                         /* Monster is a clone */
    s16b mimicked_k_idx;                /* Object kind this monster is mimicking (random mimics) */
    byte origin;                        /* How this monster was created */
    byte feat;                          /* Terrain under monster (for feature mimics) */
};

/*
 * A stacked monster message entry
 */
typedef struct monster_race_message
{
    struct monster_race *race;  /* The race of the monster */
    byte mon_flags;             /* Flags */
    int  msg_code;              /* The coded message */
    byte mon_count;             /* How many monsters triggered this message */
    bool delay;                 /* Should this message be put off to the end */
    byte delay_tag;             /* To group delayed messages for better presentation */
} monster_race_message;

typedef struct monster_message_history
{
    struct monster *mon;    /* The monster */
    int message_code;       /* The coded message */
} monster_message_history;

/** Variables **/

extern struct monster_race *r_info;
extern struct monster_base *rb_info;

#endif /* INCLUDED_MONSTER_COMMON_H */
