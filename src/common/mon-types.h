/*
 * File: mon-types.h
 * Purpose: Global monster type declarations
 */

#ifndef INCLUDED_MONSTER_TYPES_H
#define INCLUDED_MONSTER_TYPES_H

/** Constants **/

/* Monster Timed Effects */
enum
{
    MON_TMD_SLEEP = 0,
    MON_TMD_STUN,
    MON_TMD_CONF,
    MON_TMD_FEAR,
    MON_TMD_SLOW,
    MON_TMD_FAST,
    MON_TMD_POIS,
    MON_TMD_CUT,
    MON_TMD_BLIND,
    MON_TMD_HOLD,

    MON_TMD_MAX
};

/** Structures **/

/*
 * Monster blow structure
 *
 *  - Method (RBM_*)
 *  - Effect (RBE_*)
 *  - Damage Dice
 *  - Damage Sides
 */
struct monster_blow
{
    byte method;
    byte effect;
    byte d_dice;
    byte d_side;
};

/*
 * Monster pain messages.
 */
typedef struct monster_pain
{
	const char *messages[7];
	int pain_idx;

	struct monster_pain *next;
} monster_pain;

/*
 * Information about "base" monster type.
 */
typedef struct monster_base
{
	struct monster_base *next;

	char *name;
	char *text;

	bitflag flags[RF_SIZE];         /* Flags */
	bitflag spell_flags[RSF_SIZE];  /* Spell flags */

	char d_char;                    /* Default monster character */

	monster_pain *pain;             /* Pain messages */
} monster_base;

/* Information about specified monster drops */
struct monster_drop
{
    struct monster_drop *next;
    struct object_kind *kind;
    struct artifact *artifact;
    unsigned int percent_chance;
    unsigned int min;
    unsigned int max;
};

struct monster_mimic
{
    struct monster_mimic *next;
    struct object_kind *kind;
};

/*  
 * Monster "lore" information
 *
 * Note that these fields are related to the "monster recall" and can
 * be scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc). XXX XXX XXX
 */
typedef struct
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
    byte drop_gold;                     /* Max number of gold dropped at once (player) */
    byte drop_item;                     /* Max number of item dropped at once (player) */
    byte cast_innate;                   /* Max number of innate spells seen (player) */
    byte cast_spell;                    /* Max number of other spells seen (player) */
    byte blows[MONSTER_BLOW_MAX];       /* Number of times each blow type was seen (player) */
    bitflag flags[RF_SIZE];             /* Observed racial flags (player) */
    bitflag spell_flags[RSF_SIZE];      /* Observed racial spell flags (player) */
} monster_lore;

/*
 * Monster "race" information, including racial memories
 *
 * Note that "d_attr" and "d_char" are used for MORE than "visual" stuff.
 *
 * Note that "x_attr" and "x_char" are used ONLY for "visual" stuff.
 */
typedef struct monster_race
{
    struct monster_race *next;
    unsigned int ridx;                      /* Index */
    char *name;                             /* Name */
    char *text;                             /* Text */
    struct monster_base *base;
    u16b avg_hp;                            /* Average HP for this creature */
    s16b ac;                                /* Armour Class */
    s16b sleep;                             /* Inactive counter (base) */
    byte aaf;                               /* Area affect radius (1-100) */
    byte speed;                             /* Speed (normally 110) */
    s32b mexp;                              /* Exp value for kill */
    u32b power;                             /* Monster power */
    long scaled_power;                      /* Monster power scaled by level */
    s16b highest_threat;                    /* Monster highest threat */
    long melee_dam;
    long spell_dam;
    long hp;
    byte freq_spell;                        /* Spell frequency */
    bitflag flags[RF_SIZE];                 /* Flags */
    bitflag spell_flags[RSF_SIZE];          /* Spell flags */
    struct monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */
    s16b level;                             /* Level of creature */
    byte rarity;                            /* Rarity of creature */
    byte d_attr;                            /* Default monster attribute */
    char d_char;                            /* Default monster character */
    byte x_attr;                            /* Desired monster attribute */
    char x_char;                            /* Desired monster character */
    s16b extra;                             /* Corpse weight */
    monster_lore lore;                      /* Monster "lore" information */
    struct monster_drop *drops;
    struct monster_mimic *mimic_kinds;
} monster_race;

/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */
typedef struct monster
{
    struct monster_race *race;
    s16b r_idx;                     /* Monster race index */
    int midx;
    byte fy;                        /* Y location on map */
    byte fx;                        /* X location on map */
    s16b hp;                        /* Current Hit points */
    s16b maxhp;                     /* Max Hit points */
    s16b m_timed[MON_TMD_MAX];      /* Timed monster status effects */
    byte mspeed;                    /* Monster "speed" */
    s32b energy;                    /* Monster "energy" */
    byte cdis;                      /* Current dis from player (transient) */
    bool unaware;                   /* Players don't know this is a monster */
    s16b mimicked_o_idx;            /* Object this monster is mimicking */
    s16b hold_o_idx;                /* Object being held (if any) */
    byte attr;                      /* "attr" last used for drawing monster */
    u32b smart;                     /* Field for "cfg_ai_learn" */
    bitflag known_pflags[OF_SIZE];  /* Known player flags */

    /* MAngband */
    s16b depth;                     /* Level of the dungeon */
    s16b closest_player;            /* The player closest to this monster (transient) */

    /* PWMAngband */
    s16b ac;                        /* Armour Class */
    struct monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */
    s16b level;                     /* Level of creature */
    s16b master;                    /* The player controlling this monster */
    byte lifespan;                  /* Lifespan of controlled creature */
    byte status;                    /* Monster status: hostile, guard, follower, attacker */
    byte clone;                     /* Monster is a clone */
    s16b mimicked_k_idx;            /* Object kind this monster is mimicking (random mimics) */
    byte origin;                    /* How this monster was created */
} monster_type;

/*
 * A stacked monster message entry
 */
typedef struct monster_race_message
{
    s16b mon_race;  /* The race of the monster */
    byte mon_flags; /* Flags: 0x01 means hidden monster, 0x02 means offscreen monster */
    int  msg_code;  /* The coded message */
    byte mon_count; /* How many monsters triggered this message */
    bool delay;     /* Should this message be put off to the end */
} monster_race_message;

typedef struct monster_message_history
{
    struct monster *mon;    /* The monster */
    int message_code;       /* The coded message */
} monster_message_history;

/*
 * Structure for monster spell types
 */
struct mon_spell
{
    u16b index;             /* Numerical index (RSF_FOO) */
    int type;               /* Type bitflag */
    const char *desc;       /* Verbal description */
    int cap;                /* Damage cap */
    int div;                /* Damage divisor (monhp / this) */
    int gf;                 /* Flag for projection type (GF_FOO) */
    int msgt;               /* Flag for message colouring */
    byte save;              /* Does this attack allow a saving throw? */
    int hit;                /* To-hit level for the attack */
    const char *verb;       /* Description of the attack */
    random_value base_dam;  /* Base damage for the attack */
    random_value rlev_dam;  /* Monster-level-dependent damage */
    const char *blind_verb; /* Description of the attack if unseen */
    const char *what;       /* Special message (for flavored death messages) */
};

/*
 * Structure for side effects of spell attacks
 */
struct spell_effect
{
    u16b index;         /* Numerical index (RSE_#) */
    u16b method;        /* What RSF_ attack has this effect */
    int gf;             /* What GF_ type has this effect */
    bool timed;         /* TRUE if timed, FALSE if permanent */
    int flag;           /* Effect flag */
    random_value base;  /* The base duration or impact */
    random_value dam;   /* Damage-dependent duration or impact */
    int chance;         /* Chance of this effect if >1 available */
    bool save;          /* Does this effect allow a saving throw? */
    int res_flag;       /* Resistance to this specific effect */
    random_value power; /* Power rating of effect */
};

#endif /* INCLUDED_MONSTER_TYPES_H */
