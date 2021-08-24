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
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) ELEM_##a,
    #include "list-elements.h"
    #undef ELEM
    ELEM_MAX
};

#define ELEM_BASE_MIN   ELEM_ACID
#define ELEM_HIGH_MIN   ELEM_POIS
#define ELEM_HIGH_MAX   ELEM_DISEN

/* The object flags */
enum
{
    OF_NONE,
    #define STAT(a, b, c, d, e, f, g, h) OF_##c,
    #include "list-stats.h"
    #undef STAT
    #define OF(a, b, c, d, e) OF_##a,
    #include "list-object-flags.h"
    #undef OF
    OF_MAX
};

#define OF_SIZE                FLAG_SIZE(OF_MAX)

#define of_has(f, flag)        flag_has_dbg(f, OF_SIZE, flag, #f, #flag)
#define of_next(f, flag)       flag_next(f, OF_SIZE, flag)
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
#define of_comp_union(f1, f2)  flag_comp_union(f1, f2, OF_SIZE)
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
#define kf_comp_union(f1, f2)  flag_comp_union(f1, f2, KF_SIZE)
#define kf_inter(f1, f2)       flag_inter(f1, f2, KF_SIZE)
#define kf_diff(f1, f2)        flag_diff(f1, f2, KF_SIZE)

/* The object modifiers */
enum
{
    #define STAT(a, b, c, d, e, f, g, h) OBJ_MOD_##a,
    #include "list-stats.h"
    #undef STAT
    #define OBJ_MOD(a, b, c, d) OBJ_MOD_##a,
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
    IGNORE_WORTHLESS,
    IGNORE_AVERAGE,
    IGNORE_GOOD,
    IGNORE_EXCELLENT_NO_HI,
    IGNORE_EXCELLENT_NO_SPL,
    IGNORE_ALL,

    IGNORE_MAX
};

/*** Structures ***/

struct effect
{
    struct effect *next;
    u16b index;         /* The effect index */
    dice_t *dice;       /* Dice expression used in the effect */
    int params[3];      /* Extra parameters to be passed to the handler */
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

/* Brand type */
struct brand
{
    char *name;
    int element;
    int multiplier;
    int damage; /* Storage for damage during description */
    struct brand *next;
};

/* Slay type */
struct slay
{
    char *name;
    int race_flag;
    int multiplier;
    int damage; /* Storage for damage during description */
    struct slay *next;
};

enum
{
    EL_XXX = 0x01, /* TODO: remove for the next 1.1.12 version */
    EL_INFO_HATES = 0x02, /* TODO: renumber for the next 1.1.12 version */
    EL_INFO_IGNORE = 0x04, /* TODO: renumber for the next 1.1.12 version */
    EL_INFO_RANDOM = 0x08, /* TODO: renumber for the next 1.1.12 version */
    EL_YYY = 0x10  /* TODO: remove for the next 1.1.12 version */
};

/* Element info type */
struct element_info
{
    s16b res_level;
    bitflag flags;
};

/*
 * Activation structure
 */
struct activation
{
    struct activation *next;
    char *name;
    int index;
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
    struct brand *brands;
    struct slay *slays;
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
    struct brand *brands;
    struct slay *slays;
    int level;                      /* Difficulty level for activation */
    int alloc_prob;                 /* Chance of being generated (i.e. rarity) */
    int alloc_min;                  /* Minimum depth (can appear earlier) */
    int alloc_max;                  /* Maximum depth (will NEVER appear deeper) */
    byte created;                   /* Artifact is created */
    byte owned;                     /* Artifact is owned */
    struct activation *activation;  /* Artifact activation */
    char *alt_msg;
    random_value time;              /* Recharge time (if appropriate) */
};

/*
 * Structure for possible object kinds for an ego item
 */
struct ego_poss_item
{
    u32b kidx;
    struct ego_poss_item *next;
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
    struct brand *brands;
    struct slay *slays;
    int level;                      /* Minimum level */
    int rarity;                     /* Object rarity */
    int rating;                     /* Level rating boost */
    int alloc_prob;                 /* Chance of being generated (i.e. rarity) */
    int alloc_min;                  /* Minimum depth (can appear earlier) */
    int alloc_max;                  /* Maximum depth (will NEVER appear deeper) */
    struct ego_poss_item *poss_items;
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
    ACT_NONE,
    ACT_HACK,
    ACT_TIMEOUT,
    ACT_NORMAL,
    ACT_AIMED
};

/*
 * Hack -- extra information used by the client
 */
struct object_xtra
{
    byte attr;              /* Color */
    byte act;               /* Activation flag */
    byte fuel;              /* Fuelable flag */
    byte fail;              /* Fail flag */
    s16b slot;              /* Slot flag */
    byte max;               /* Max amount */
    byte owned;             /* Owned amount */
    byte cursed;            /* Cursed flag */
    byte known;             /* Known flag */
    byte sellable;          /* Sellable flag */
    byte carry;             /* Carry flag */
    byte quality_ignore;    /* Quality ignoring */
    byte ignored;           /* Ignored flag */
    byte ego_ignore;        /* Ego ignoring */
    s16b eidx;              /* Ego index */
    byte equipped;          /* Equipped flag */
    s16b bidx;              /* Book index */
    char name[NORMAL_WID];
    char name_terse[NORMAL_WID];
    char name_base[NORMAL_WID];
};

/*
 * Flags for the obj->notice field
 */
enum
{
    OBJ_NOTICE_WORN = 0x01,
    OBJ_NOTICE_SENSED = 0x02,
    OBJ_NOTICE_IGNORE = 0x04
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
 * is holding the object.  Objects being held have "ix = 0" and "iy = 0".
 *
 * Note that object records are not now copied, but allocated on object
 * creation and freed on object destruction.  These records are handed
 * around between player and monster inventories and the floor on a fairly
 * regular basis, and care must be taken when handling such objects.
 */
struct object
{
    struct object_kind *kind;
    struct ego_item *ego;
    struct artifact *artifact;

    struct object *prev;            /* Previous object in a pile */
    struct object *next;            /* Next object in a pile */
    struct object *known;           /* Known version of this object */

    s16b oidx;                      /* Item list index, if any */

    byte iy;                        /* Y-position on map, or zero */
    byte ix;                        /* X-position on map, or zero */
    byte tval;                      /* Item type (from kind) */
    byte sval;                      /* Item sub-type (from kind) */
    s32b pval;                      /* Item extra-parameter */
    s16b weight;                    /* Item weight */
    bitflag flags[OF_SIZE];         /* Flags (all) */
    s32b modifiers[OBJ_MOD_MAX];
    struct element_info el_info[ELEM_MAX];
    struct brand *brands;
    struct slay *slays;
    s16b ac;                        /* Normal AC */
    s16b to_a;                      /* Plusses to AC */
    s16b to_h;                      /* Plusses to hit */
    s16b to_d;                      /* Plusses to damage */
    byte dd, ds;                    /* Damage dice/sides */
    struct effect *effect;          /* Effect this item produces (effects.c) */
    struct activation *activation;  /* Activation */
    random_value time;              /* Recharge time (rods/activation) */
    s16b timeout;                   /* Timeout Counter */
    byte number;                    /* Number of items */
    bitflag notice;                 /* Attention paid to the object */
    s16b held_m_idx;                /* Monster holding us (if any) */
    s16b mimicking_m_idx;           /* Monster mimicking us (if any) */
    byte origin;                    /* How this item was found */
    s16b origin_depth;              /* What depth the item was found at */
    u16b origin_xtra;               /* Extra information about origin */
    quark_t note;                   /* Inscription index */
    s16b depth;                     /* Depth into the dungeon */
    s32b randart_seed;              /* Randart seed, if any */
    s32b askprice;                  /* Item sale price (transient) */
    s32b creator;                   /* Item creator (if any) */
    s32b owner;                     /* Item owner (if any) */
    byte allow_ignore;              /* Item can be ignored */
    byte ordered;                   /* Item has been ordered */
    struct object_xtra info_xtra;   /* Extra information used by the client */
    byte attr;                      /* "attr" last used for drawing object */
    s16b decay;                     /* Decay timeout for corpses */
    byte bypass_aware;              /* Bypasses the "aware" flag */
};

typedef bool (*item_tester)(struct player *, const struct object *);

#endif /* INCLUDED_OBJECT_COMMON_H */
