/*
 * File: obj-common.h
 * Purpose: Structures and functions for objects
 */

#ifndef INCLUDED_OBJECT_COMMON_H
#define INCLUDED_OBJECT_COMMON_H

/*** Constants ***/

/*
 * Spell types used by project(), needed for object resistances.
 */
enum
{
    #define ELEM(a, b, c, d) ELEM_##a,
    #include "list-elements.h"
    #undef ELEM
    ELEM_MAX
};

#define ELEM_BASE_MIN   ELEM_ACID
#define ELEM_BASE_MAX   ELEM_COLD
#define ELEM_HIGH_MIN   ELEM_POIS
#define ELEM_HIGH_MAX   ELEM_DISEN
#define ELEM_XHIGH_MAX  ELEM_MANA

/* The object flags */
enum
{
    #define OF(a, b) OF_##a,
    #include "list-object-flags.h"
    #undef OF
    OF_MAX
};

#define OF_SIZE                FLAG_SIZE(OF_MAX)

#define of_has(f, flag)        flag_has_dbg(f, OF_SIZE, flag, #f, #flag)
#define of_next(f, flag)       flag_next(f, OF_SIZE, flag)
#define of_count(f)            flag_count(f, OF_SIZE)
#define of_is_empty(f)         flag_is_empty(f, OF_SIZE)
#define of_is_full(f)          flag_is_full(f, OF_SIZE)
#define of_is_inter(f1, f2)    flag_is_inter(f1, f2, OF_SIZE)
#define of_is_subset(f1, f2)   flag_is_subset(f1, f2, OF_SIZE)
#define of_is_equal(f1, f2)    flag_is_equal(f1, f2, OF_SIZE)
#define of_on(f, flag)         flag_on_dbg(f, OF_SIZE, flag, #f, #flag)
#define of_off(f, flag)        flag_off(f, OF_SIZE, flag)
#define of_wipe(f)             flag_wipe(f, OF_SIZE)
#define of_setall(f)           flag_setall(f, OF_SIZE)
#define of_negate(f)           flag_negate(f, OF_SIZE)
#define of_copy(f1, f2)        flag_copy(f1, f2, OF_SIZE)
#define of_union(f1, f2)       flag_union(f1, f2, OF_SIZE)
#define of_inter(f1, f2)       flag_inter(f1, f2, OF_SIZE)
#define of_diff(f1, f2)        flag_diff(f1, f2, OF_SIZE)

#define of_has_unique(f, flag) \
    ((of_next(f, FLAG_START) == flag) && (of_next(f, flag + 1) == FLAG_END))

/* The object kind flags */
enum
{
    #define KF(a, b) KF_##a,
    #include "list-kind-flags.h"
    #undef KF
    KF_MAX
};

#define KF_SIZE                FLAG_SIZE(KF_MAX)

#define kf_has(f, flag)        flag_has_dbg(f, KF_SIZE, flag, #f, #flag)
#define kf_next(f, flag)       flag_next(f, KF_SIZE, flag)
#define kf_is_empty(f)         flag_is_empty(f, KF_SIZE)
#define kf_is_full(f)          flag_is_full(f, KF_SIZE)
#define kf_is_inter(f1, f2)    flag_is_inter(f1, f2, KF_SIZE)
#define kf_is_subset(f1, f2)   flag_is_subset(f1, f2, KF_SIZE)
#define kf_is_equal(f1, f2)    flag_is_equal(f1, f2, KF_SIZE)
#define kf_on(f, flag)         flag_on_dbg(f, KF_SIZE, flag, #f, #flag)
#define kf_off(f, flag)        flag_off(f, KF_SIZE, flag)
#define kf_wipe(f)             flag_wipe(f, KF_SIZE)
#define kf_setall(f)           flag_setall(f, KF_SIZE)
#define kf_negate(f)           flag_negate(f, KF_SIZE)
#define kf_copy(f1, f2)        flag_copy(f1, f2, KF_SIZE)
#define kf_union(f1, f2)       flag_union(f1, f2, KF_SIZE)
#define kf_inter(f1, f2)       flag_inter(f1, f2, KF_SIZE)
#define kf_diff(f1, f2)        flag_diff(f1, f2, KF_SIZE)

/* The object modifiers */
enum
{
    #define STAT(a) OBJ_MOD_##a,
    #include "list-stats.h"
    #undef STAT
    #define OBJ_MOD(a) OBJ_MOD_##a,
    #include "list-object-modifiers.h"
    #undef OBJ_MOD
    OBJ_MOD_MAX
};

/*
 * The different kinds of quality ignoring
 */
enum
{
    IGNORE_NONE,
    IGNORE_BAD,
    IGNORE_AVERAGE,
    IGNORE_GOOD,
    IGNORE_ALL,

    IGNORE_MAX
};

/*** Structures ***/

/*
 * Effect
 */
struct effect
{
    struct effect *next;
    u16b index;         /* The effect index */
    dice_t *dice;       /* Dice expression used in the effect */
    int subtype;        /* Projection type, timed effect type, etc. */
    int radius;         /* Radius of the effect (if it has one) */
    int other;          /* Extra parameter to be passed to the handler */
    int y;              /* Y coordinate or distance */
    int x;              /* X coordinate or distance */
    int flag;           /* Hack -- flag for mimic spells */
    char *self_msg;     /* Message for affected player */
    char *other_msg;    /* Message for other players */
};

/*
 * Object flavors
 */
struct flavor
{
    char *text;         /* Text */
    unsigned int fidx;  /* Index */
    struct flavor *next;
    byte tval;          /* Associated object type */
    byte sval;          /* Associated object sub-type */
    byte d_attr;        /* Default flavor attribute */
    char d_char;        /* Default flavor character */
};

/*
 * Brand type
 */
struct brand
{
    char *code;
    char *name;
    char *verb;
    int resist_flag;
    int multiplier;
    int power;
    char *active_verb;
    char *active_verb_plural;
    char *desc_adjective;
    struct brand *next;
};

/*
 * Slay type
 */
struct slay
{
    char *code;
    char *name;
    char *base;
    char *melee_verb;
    char *range_verb;
    int race_flag;
    int multiplier;
    int power;
    int esp_chance;
    int esp_flag;
    struct slay *next;
};

/*
 * Curse type
 */
struct curse
{
    struct curse *next;
    char *name;
    bool *poss;
    struct object *obj;
    char *conflict;
    bitflag conflict_flags[OF_SIZE];
    char *desc;
};

extern struct curse *curses;

enum
{
    EL_INFO_HATES = 0x01,
    EL_INFO_IGNORE = 0x02,
    EL_INFO_RANDOM = 0x04
};

/*
 * Element info type
 */
struct element_info
{
    s16b res_level;
    bitflag flags;
    byte lvl;
};

/*
 * Activation structure
 */
struct activation
{
    struct activation *next;
    char *name;
    unsigned int index;
    bool aim;
    int power;
    struct effect *effect;
    char *message;
    char *desc;
};

extern struct activation *activations;

/*
 * Information about object types, like rods, wands, etc.
 */
struct object_base
{
    char *name;
    int tval;
    struct object_base *next;
    int attr;
    bitflag flags[OF_SIZE];
    bitflag kind_flags[KF_SIZE];    /* Kind flags */
    struct element_info el_info[ELEM_MAX];
    int break_perc;
    int max_stack;
    int num_svals;
};

/*
 * Information about object kinds, including player knowledge.
 *
 * TODO: split out the user-changeable bits into a separate struct so this
 * one can be read-only.
 */
struct object_kind
{
    char *name;                     /* Name */
    char *text;                     /* Description */
    struct object_base *base;
    u32b kidx;                      /* Index */
    struct object_kind *next;
    byte tval;                      /* General object type (see TV_ macros) */
    byte sval;                      /* Object sub-type */
    random_value pval;              /* Item extra-parameter */
    random_value to_h;              /* Bonus to hit */
    random_value to_d;              /* Bonus to damage */
    random_value to_a;              /* Bonus to armor */
    int ac;                         /* Base armor */
    byte dd, ds;                    /* Damage dice/sides */
    int weight;                     /* Weight, in 1/10lbs */
    int cost;                       /* Object base cost */
    bitflag flags[OF_SIZE];         /* Flags (all) */
    bitflag kind_flags[KF_SIZE];    /* Kind flags */
    random_value modifiers[OBJ_MOD_MAX];
    struct element_info el_info[ELEM_MAX];
    bool *brands;
    bool *slays;
    int *curses;                    /* Array of curse powers */
    byte d_attr;                    /* Default object attribute */
    char d_char;                    /* Default object character */
    int alloc_prob;                 /* Allocation: commonness */
    int alloc_min;                  /* Highest normal dungeon level */
    int alloc_max;                  /* Lowest normal dungeon level */
    int level;                      /* Level */
    struct effect *effect;          /* Effect this item produces (effects.c) */
    struct activation *activation;  /* Activation */
    random_value time;              /* Recharge time (if appropriate) */
    random_value charge;            /* Number of charges (staves/wands) */
    int gen_mult_prob;              /* Probability of generating more than one */
    random_value stack_size;        /* Number to generate */
    struct flavor *flavor;          /* Special object flavor (or zero) */
};

extern struct object_kind *k_info;

/*
 * Information about artifacts.
 *
 * Note that "created" and "owned" are written to the savefile.
 */
struct artifact
{
    char *name;                     /* Name */
    char *text;                     /* Description */
    u32b aidx;                      /* Index */
    struct artifact *next;
    int tval;                       /* General artifact type (see TV_ macros) */
    int sval;                       /* Artifact sub-type */
    int to_h;                       /* Bonus to hit */
    int to_d;                       /* Bonus to damage */
    int to_a;                       /* Bonus to armor */
    int ac;                         /* Base armor */
    int dd, ds;                     /* Base damage dice/sides */
    int weight;                     /* Weight in 1/10lbs */
    bitflag flags[OF_SIZE];         /* Flags (all) */
    int modifiers[OBJ_MOD_MAX];
    struct element_info el_info[ELEM_MAX];
    bool *brands;
    bool *slays;
    int *curses;                    /* Array of curse powers */
    int level;                      /* Difficulty level for activation */
    int alloc_prob;                 /* Chance of being generated (i.e. rarity) */
    int alloc_min;                  /* Minimum depth (can appear earlier) */
    int alloc_max;                  /* Maximum depth (will NEVER appear deeper) */
    byte created;                   /* Artifact is created */
    byte owned;                     /* Artifact is owned */
    struct activation *activation;  /* Artifact activation */
    char *alt_msg;
    random_value time;              /* Recharge time (if appropriate) */
    int base_power;                 /* Artifact power (for randarts) */
};

/*
 * Structure for possible object kinds for an ego item
 */
struct poss_item
{
    u32b kidx;
    struct poss_item *next;
};

/*
 * Information about ego-items.
 */
struct ego_item
{
    char *name;                     /* Name */
    char *text;                     /* Description */
    u32b eidx;                      /* Index */
    struct ego_item *next;
    bitflag flags[OF_SIZE];         /* Flags (all) */
    bitflag kind_flags[KF_SIZE];    /* Kind flags */
    random_value modifiers[OBJ_MOD_MAX];
    int min_modifiers[OBJ_MOD_MAX];
    struct element_info el_info[ELEM_MAX];
    bool *brands;
    bool *slays;
    int *curses;                    /* Array of curse powers */
    int rating;                     /* Level rating boost */
    int alloc_prob;                 /* Chance of being generated (i.e. rarity) */
    int alloc_min;                  /* Minimum depth (can appear earlier) */
    int alloc_max;                  /* Maximum depth (will NEVER appear deeper) */
    struct poss_item *poss_items;
    random_value to_h;              /* Extra to-hit bonus */
    random_value to_d;              /* Extra to-dam bonus */
    random_value to_a;              /* Extra to-ac bonus */
    int min_to_h;                   /* Minimum to-hit value */
    int min_to_d;                   /* Minimum to-dam value */
    int min_to_a;                   /* Minimum to-ac value */
    struct activation *activation;  /* Activation */
    random_value time;              /* Recharge time (rods/activation) */
};

extern struct ego_item *e_info;

/*
 * State of activatable items (including rods)
 */
enum
{
    ACT_NONE,       /* Not activatable */
    ACT_TIMEOUT,    /* Charging */
    ACT_NORMAL      /* Activatable */
};

/*
 * Direction choice
 */
enum
{
    AIM_NONE,   /* None */
    AIM_RANDOM, /* Random */
    AIM_NORMAL  /* Normal */
};

/*
 * Hack -- extra information used by the client
 */
struct object_xtra
{
    byte attr;              /* Color */
    byte act;               /* Activation flag */
    byte aim;               /* Aiming flag */
    byte fuel;              /* Fuelable flag */
    byte fail;              /* Fail flag */
    s16b slot;              /* Slot flag */
    byte max;               /* Max amount */
    byte owned;             /* Owned amount */
    byte stuck;             /* Stuck flag */
    byte known;             /* Known flag */
    byte known_effect;      /* Known effect flag */
    byte sellable;          /* Sellable flag */
    byte carry;             /* Carry flag */
    byte quality_ignore;    /* Quality ignoring */
    byte ignored;           /* Ignored flag */
    s16b eidx;              /* Ego index (or -1 if no ego) */
    byte equipped;          /* Equipped flag */
    byte magic;             /* Magic flag */
    s16b bidx;              /* Book index */
    char name[NORMAL_WID];
    char name_terse[NORMAL_WID];
    char name_base[NORMAL_WID];
    char name_curse[NORMAL_WID];
    char name_power[NORMAL_WID];
};

/*
 * Flags for the obj->notice field
 */
enum
{
    OBJ_NOTICE_WORN = 0x01,
    OBJ_NOTICE_ASSESSED = 0x02,
    OBJ_NOTICE_IGNORE = 0x04
};

struct curse_data
{
    int power;
    int timeout;
    int to_a;
    int to_h;
    int to_d;
    int modifiers[OBJ_MOD_MAX];
};

/*
 * Object information, for a specific object.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Each cave grid points to one (or zero) objects via the "obj" field in
 * its "squares" struct.  Each object then points to one (or zero) objects
 * via the "next" field, and (aside from the first) back via its "prev"
 * field, forming a doubly linked list, which in game terms represents a
 * stack of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "held_obj"
 * field (see monster.h).  Each object then points to one (or zero) objects
 * and back to previous objects by its own "next" and "prev" fields,
 * forming a doubly linked list, which in game terms represents the
 * monster's inventory.
 *
 * The "held_m_idx" field is used to indicate which monster, if any,
 * is holding the object.  Objects being held have (0, 0) as a grid.
 *
 * Note that object records are not now copied, but allocated on object
 * creation and freed on object destruction.  These records are handed
 * around between player and monster inventories and the floor on a fairly
 * regular basis, and care must be taken when handling such objects.
 */
struct object
{
    struct object_kind *kind;           /* Kind of the object */
    struct ego_item *ego;               /* Ego item info of the object, if any */
    struct artifact *artifact;          /* Artifact info of the object, if any */

    struct object *prev;                /* Previous object in a pile */
    struct object *next;                /* Next object in a pile */
    struct object *known;               /* Known version of this object */

    s16b oidx;                          /* Item list index, if any */

    struct loc grid;                    /* Position on map, or (0, 0) */

    byte tval;                          /* Item type (from kind) */
    byte sval;                          /* Item sub-type (from kind) */

    s32b pval;                          /* Item extra-parameter */

    s16b weight;                        /* Item weight */

    byte dd;                            /* Number of damage dice */
    byte ds;                            /* Number of sides on each damage die */
    s16b ac;                            /* Normal AC */
    s16b to_a;                          /* Plusses to AC */
    s16b to_h;                          /* Plusses to hit */
    s16b to_d;                          /* Plusses to damage */

    bitflag flags[OF_SIZE];             /* Object flags */
    s32b modifiers[OBJ_MOD_MAX];        /* Object modifiers */
    struct element_info el_info[ELEM_MAX];  /* Object element info */
    bool *brands;                       /* Array of brands */
    bool *slays;                        /* Array of slays */
    struct curse_data *curses;          /* Array of curse powers and timeouts */

    struct effect *effect;              /* Effect this item produces (effects.c) */
    struct activation *activation;      /* Activation */
    random_value time;                  /* Recharge time (rods/activation) */
    s16b timeout;                       /* Timeout Counter */

    byte number;                        /* Number of items */
    bitflag notice;                     /* Attention paid to the object */

    s16b held_m_idx;                    /* Monster holding us (if any) */
    s16b mimicking_m_idx;               /* Monster mimicking us (if any) */

    byte origin;                        /* How this item was found */
    s16b origin_depth;                  /* What depth the item was found at */
    struct monster_race *origin_race;   /* Monster race that dropped it */

    quark_t note;                       /* Inscription index */

    /* MAngband & PWMAngband fields */

    struct worldpos wpos;               /* Position on the world map */
    s32b randart_seed;                  /* Randart seed, if any */
    s32b askprice;                      /* Item sale price (transient) */
    s32b creator;                       /* Item creator (if any) */
    s32b owner;                         /* Item owner (if any) */
    byte level_req;                     /* Level requirement */
    byte ignore_protect;                /* Bypass auto-ignore */
    byte ordered;                       /* Item has been ordered */
    struct object_xtra info_xtra;       /* Extra information used by the client */
    byte attr;                          /* "attr" last used for drawing object */
    s16b decay;                         /* Decay timeout for corpses */
    byte bypass_aware;                  /* Bypasses the "aware" flag */
};

typedef bool (*item_tester)(struct player *, const struct object *);

#endif /* INCLUDED_OBJECT_COMMON_H */
