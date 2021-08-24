/*
 * File: monster.h
 * Purpose: Structures and functions for monsters
 */

#ifndef MONSTER_MONSTER_H
#define MONSTER_MONSTER_H

/** Monster flags **/

/*
 * The monster flag types
 */
enum monster_flag_type
{
    RFT_NONE = 0,   /* placeholder flag */
    RFT_OBV,        /* an obvious property */
    RFT_DISP,       /* for display purposes */
    RFT_GEN,        /* related to generation */
    RFT_NOTE,       /* especially noteworthy for lore */
    RFT_BEHAV,      /* behaviour-related */
    RFT_DROP,       /* drop details */
    RFT_DET,        /* detection properties */
    RFT_ALTER,      /* environment shaping */
    RFT_RACE_N,     /* types of monster (noun) */
    RFT_RACE_A,     /* types of monster (adjective) */
    RFT_VULN,       /* vulnerabilities with no corresponding resistance */
    RFT_VULN_I,     /* vulnerabilities with a corresponding resistance */
    RFT_RES,        /* elemental resistances */
    RFT_PROT,       /* immunity from status effects */

    RFT_MAX
};

/*
 * Special Monster Flags (all temporary)
 */

#define mflag_has(f, flag)        flag_has_dbg(f, MFLAG_SIZE, flag, #f, #flag)
#define mflag_next(f, flag)       flag_next(f, MFLAG_SIZE, flag)
#define mflag_is_empty(f)         flag_is_empty(f, MFLAG_SIZE)
#define mflag_is_full(f)          flag_is_full(f, MFLAG_SIZE)
#define mflag_is_inter(f1, f2)    flag_is_inter(f1, f2, MFLAG_SIZE)
#define mflag_is_subset(f1, f2)   flag_is_subset(f1, f2, MFLAG_SIZE)
#define mflag_is_equal(f1, f2)    flag_is_equal(f1, f2, MFLAG_SIZE)
#define mflag_on(f, flag)         flag_on_dbg(f, MFLAG_SIZE, flag, #f, #flag)
#define mflag_off(f, flag)        flag_off(f, MFLAG_SIZE, flag)
#define mflag_wipe(f)             flag_wipe(f, MFLAG_SIZE)
#define mflag_setall(f)           flag_setall(f, MFLAG_SIZE)
#define mflag_negate(f)           flag_negate(f, MFLAG_SIZE)
#define mflag_copy(f1, f2)        flag_copy(f1, f2, MFLAG_SIZE)
#define mflag_union(f1, f2)       flag_union(f1, f2, MFLAG_SIZE)
#define mflag_inter(f1, f2)       flag_inter(f1, f2, MFLAG_SIZE)
#define mflag_diff(f1, f2)        flag_diff(f1, f2, MFLAG_SIZE)

/*
 * Monster property and ability flags (race flags)
 */

#define rf_has(f, flag)        flag_has_dbg(f, RF_SIZE, flag, #f, #flag)
#define rf_next(f, flag)       flag_next(f, RF_SIZE, flag)
#define rf_count(f)            flag_count(f, RF_SIZE)
#define rf_is_empty(f)         flag_is_empty(f, RF_SIZE)
#define rf_is_full(f)          flag_is_full(f, RF_SIZE)
#define rf_is_inter(f1, f2)    flag_is_inter(f1, f2, RF_SIZE)
#define rf_is_subset(f1, f2)   flag_is_subset(f1, f2, RF_SIZE)
#define rf_is_equal(f1, f2)    flag_is_equal(f1, f2, RF_SIZE)
#define rf_on(f, flag)         flag_on_dbg(f, RF_SIZE, flag, #f, #flag)
#define rf_off(f, flag)        flag_off(f, RF_SIZE, flag)
#define rf_wipe(f)             flag_wipe(f, RF_SIZE)
#define rf_setall(f)           flag_setall(f, RF_SIZE)
#define rf_negate(f)           flag_negate(f, RF_SIZE)
#define rf_copy(f1, f2)        flag_copy(f1, f2, RF_SIZE)
#define rf_union(f1, f2)       flag_union(f1, f2, RF_SIZE)
#define rf_inter(f1, f2)       flag_inter(f1, f2, RF_SIZE)
#define rf_diff(f1, f2)        flag_diff(f1, f2, RF_SIZE)

/*
 * Is that monster shimmering?
 */
#define monster_shimmer(R) \
    (rf_has((R)->flags, RF_ATTR_MULTI) || rf_has((R)->flags, RF_ATTR_FLICKER))

/*
 * The monster flag structure
 */
struct monster_flag
{
    u16b index;         /* the RF_ index */
    u16b type;          /* RFT_ category */
    const char *desc;   /* lore description */
};

/*
 * Monster spell types
 */
struct monster_spell
{
    struct monster_spell *next;
    u16b index;                 /* Numerical index (RSF_FOO) */
    int msgt;                   /* Flag for message colouring */
    char *message;              /* Description of the attack */
    char *blind_message;        /* Description of the attack if unseen */
    char *message_strong;       /* Description of the attack (strong version) */
    char *blind_message_strong; /* Attack description if unseen (strong)*/
    char *miss_message;         /* Description of a missed attack */
    char *save_message;         /* Message on passing saving throw, if any */
    char *lore_desc;            /* Description of the attack used in lore text */
    char *lore_desc_strong;     /* Attack description used in lore text (strong) */
    byte lore_attr;             /* Color of the attack used in lore text */
    byte lore_attr_resist;      /* Color used in lore text when resisted */
    byte lore_attr_immune;      /* Color used in lore text when resisted strongly */
    int hit;                    /* To-hit level for the attack */
    struct effect *effect;      /* Effect(s) of the spell */
};

/** Variables **/

extern struct monster_pain *pain_messages;
extern struct monster_spell *monster_spells;
extern const struct monster_race *ref_race;

#endif /* MONSTER_MONSTER_H */
