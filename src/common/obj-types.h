/*
 * File: obj-types.h
 * Purpose: Global object type declarations
 */

#ifndef INCLUDED_OBJECT_TYPES_H
#define INCLUDED_OBJECT_TYPES_H

#include "h-quark.h"

/*** Constants ***/

/* The object flags */
enum
{
    #define OF(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) OF_##a,
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

/*
 * Maximum number of pvals on objects
 *
 * Note: all pvals other than DEFAULT_PVAL are assumed to be associated with
 * flags, and any non-flag uses of pval (e.g. chest quality, gold quantity)
 * are assumed to use DEFAULT_PVAL.
 */
#define MAX_PVALS       3
#define DEFAULT_PVAL    0

/*** Structures ***/

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
    byte x_attr;        /* Desired flavor attribute */
    char x_char;        /* Desired flavor character */
};

/*
 * Information about object types, like rods, wands, etc.
 */
typedef struct object_base
{
    char *name;
    int tval;
    struct object_base *next;
    bitflag flags[OF_SIZE];
    int break_perc;
} object_base;

/*
 * Information about object kinds, including player knowledge.
 *
 * TODO: split out the user-changeable bits into a separate struct so this
 * one can be read-only.
 */
typedef struct object_kind
{
    char *name;                 /* Name */
    char *text;                 /* Description */
    object_base *base;
    u32b kidx;                  /* Index */
    struct object_kind *next;
    byte tval;                  /* General object type (see TV_ macros) */
    byte sval;                  /* Object sub-type (see SV_ macros) */
    random_value pval[MAX_PVALS];   /* Power for any flags which need it */
    byte num_pvals;                 /* Number of pvals in use on this item */
    random_value to_h;          /* Bonus to hit */
    random_value to_d;          /* Bonus to damage */
    random_value to_a;          /* Bonus to armor */
    s16b ac;                    /* Base armor */
    byte dd, ds;                /* Damage dice/sides */
    s16b weight;                /* Weight, in 1/10lbs */
    s32b cost;                  /* Object base cost */
    bitflag flags[OF_SIZE];                 /* Flags (all) */
    bitflag pval_flags[MAX_PVALS][OF_SIZE]; /* Flags (pval-dependent) */
    byte d_attr;                /* Default object attribute */
    char d_char;                /* Default object character */
    byte alloc_prob;            /* Allocation: commonness */
    byte alloc_min;             /* Highest normal dungeon level */
    byte alloc_max;             /* Lowest normal dungeon level */
    byte level;                 /* Level */
    u16b effect;                /* Effect this item produces (effects.c) */
    random_value time;          /* Recharge time (if appropriate) */
    random_value charge;        /* Number of charges (staves/wands) */
    byte gen_mult_prob;         /* Probability of generating more than one */
    random_value stack_size;    /* Number to generate */
    struct flavor *flavor;      /* Special object flavor (or zero) */

    /** Game-dependent **/
    byte x_attr;            /* Desired object attribute (set by user/pref file) */
    char x_char;            /* Desired object character (set by user/pref file) */

    struct spell *spells;
} object_kind;

/*
 * Information about artifacts.
 *
 * Note that "created" and "owned" are written to the savefile.
 */
typedef struct artifact
{
    char *name;             /* Name */
    char *text;             /* Description */
    u32b aidx;              /* Index */
    struct artifact *next;
    byte tval;              /* General artifact type (see TV_ macros) */
    byte sval;              /* Artifact sub-type (see SV_ macros) */
    s16b pval[MAX_PVALS];   /* Power for any flags which need it */
    byte num_pvals;         /* Number of pvals in use on this item */
    s16b to_h;              /* Bonus to hit */
    s16b to_d;              /* Bonus to damage */
    s16b to_a;              /* Bonus to armor */
    s16b ac;                /* Base armor */
    byte dd, ds;            /* Base damage dice/sides */
    s16b weight;            /* Weight in 1/10lbs */
    bitflag flags[OF_SIZE];                 /* Flags (all) */
    bitflag pval_flags[MAX_PVALS][OF_SIZE]; /* Flags (pval-dependent) */
    byte level;             /* Difficulty level for activation */
    byte alloc_prob;        /* Chance of being generated (i.e. rarity) */
    byte alloc_min;         /* Minimum depth (can appear earlier) */
    byte alloc_max;         /* Maximum depth (will NEVER appear deeper) */
    byte created;           /* Artifact is created */
    byte owned;             /* Artifact is owned */
    u16b effect;            /* Artifact activation (see effects.c) */
    char *effect_msg;       /* Effect message */
    random_value time;      /* Recharge time (if appropriate) */
} artifact_type;

/*
 * Information about "ego-items".
 */
typedef struct ego_item
{
    char *name;                     /* Name */
    char *text;                     /* Description */
    u32b eidx;                      /* Index */
    struct ego_item *next;
    bitflag flags[OF_SIZE];                 /* Flags (all) */
    bitflag pval_flags[MAX_PVALS][OF_SIZE]; /* Flags (pval-dependent) */
    byte level;                     /* Minimum level */
    byte rarity;                    /* Object rarity */
    byte rating;                    /* Level rating boost */
    byte alloc_prob;                /* Chance of being generated (i.e. rarity) */
    byte alloc_min;                 /* Minimum depth (can appear earlier) */
    byte alloc_max;                 /* Maximum depth (will NEVER appear deeper) */
    byte tval[EGO_TVALS_MAX];       /* Legal tval */
    byte min_sval[EGO_TVALS_MAX];   /* Minimum legal sval */
    byte max_sval[EGO_TVALS_MAX];   /* Maximum legal sval */
    random_value to_h;              /* Extra to-hit bonus */
    random_value to_d;              /* Extra to-dam bonus */
    random_value to_a;              /* Extra to-ac bonus */
    random_value pval[MAX_PVALS];   /* Extra pval bonuses */
    byte num_pvals;                 /* Number of pvals used */
    byte min_to_h;                  /* Minimum to-hit value */
    byte min_to_d;                  /* Minimum to-dam value */
    byte min_to_a;                  /* Minimum to-ac value */
    byte min_pval[MAX_PVALS];       /* Minimum pvals */
    byte xtra;                      /* Extra sustain/resist/power */
} ego_item_type;

/*
 * Hack -- Extra information used by the client
 */
struct object_xtra
{
    byte attr;  /* Color */
    byte act;   /* Activation flag */
    byte fuel;  /* Fuelable flag */
    byte fail;  /* Fail flag */
    s16b slot;  /* Slot flag */
    byte index; /* Inventory index */
    s16b o_idx; /* Object index */
    byte max;   /* Max amount */
    byte owned; /* Owned amount */
};

/*
 * Object information, for a specific object.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Note that "object" records are "copied" on a fairly regular basis,
 * and care must be taken when handling such objects.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 *
 * Each cave grid points to one (or zero) objects via the "o_idx"
 * field (above).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a "stack" of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "hold_o_idx"
 * field (below).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a pile of objects held by the monster.
 *
 * The "held_m_idx" field is used to indicate which monster, if any,
 * is holding the object.  Objects being held have "ix=0" and "iy=0".
 */
typedef struct object
{
    struct object_kind *kind;
    struct ego_item *ego;
    struct artifact *artifact;
    byte iy;                /* Y-position on map, or zero */
    byte ix;                /* X-position on map, or zero */
    byte tval;              /* Item type (from kind) */
    byte sval;              /* Item sub-type (from kind) */
    s32b pval[MAX_PVALS];   /* Item extra-parameters */
    byte num_pvals;         /* Number of pvals in use */
    s16b weight;            /* Item weight */
    bitflag flags[OF_SIZE];                 /* Flags (all) */
    bitflag known_flags[OF_SIZE];           /* Known flags (all) */
    bitflag pval_flags[MAX_PVALS][OF_SIZE]; /* Flags (pval-dependent) */
    u16b ident;             /* Special flags */
    s16b ac;                /* Normal AC */
    s16b to_a;              /* Plusses to AC */
    s16b to_h;              /* Plusses to hit */
    s16b to_d;              /* Plusses to damage */
    byte dd, ds;            /* Damage dice/sides */
    s16b timeout;           /* Timeout Counter */
    byte number;            /* Number of items */
    byte ignore;            /* Item is ignored */
    s16b next_o_idx;        /* Next object in stack (if any) */
    s16b held_m_idx;        /* Monster holding us (if any) */
    s16b mimicking_m_idx;   /* Monster mimicking us (if any) */
    byte origin;            /* How this item was found */
    s16b origin_depth;      /* What depth the item was found at */
    u16b origin_xtra;       /* Extra information about origin */
    quark_t note;           /* Inscription index */
    s16b depth;             /* Depth into the dungeon */
    s32b randart_seed;      /* Randart seed, if any */
    s32b askprice;          /* Item sale price (transient) */
    s32b creator;           /* Item creator (if any) */
    s32b owner;             /* Item owner (if any) */
    byte squelch;           /* Item is squelchable */
    byte ordered;           /* Item has been ordered */
    struct object_xtra info_xtra;  /* Extra information used by the client */
    u16b effect;            /* Effect this item produces (effects.c) */
    random_value time;      /* Recharge time (if appropriate) */
} object_type;

#endif /* INCLUDED_OBJECT_TYPES_H */
