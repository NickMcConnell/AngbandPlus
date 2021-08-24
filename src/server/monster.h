/*
 * File: monster.h
 * Purpose: Structures and functions for monsters
 */

#ifndef MONSTER_MONSTER_H
#define MONSTER_MONSTER_H

/** Monster flags **/

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
#define mflag_comp_union(f1, f2)  flag_comp_union(f1, f2, MFLAG_SIZE)
#define mflag_inter(f1, f2)       flag_inter(f1, f2, MFLAG_SIZE)
#define mflag_diff(f1, f2)        flag_diff(f1, f2, MFLAG_SIZE)

/*
 * Monster property and ability flags (race flags)
 */

#define rf_has(f, flag)        flag_has_dbg(f, RF_SIZE, flag, #f, #flag)
#define rf_next(f, flag)       flag_next(f, RF_SIZE, flag)
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
#define rf_comp_union(f1, f2)  flag_comp_union(f1, f2, RF_SIZE)
#define rf_inter(f1, f2)       flag_inter(f1, f2, RF_SIZE)
#define rf_diff(f1, f2)        flag_diff(f1, f2, RF_SIZE)

/* Some flags are obvious */
#define RF_OBVIOUS_MASK \
    RF_UNIQUE, RF_QUESTOR, RF_MALE, RF_FEMALE, \
    RF_GROUP_AI

/* "race" flags */
#define RF_RACE_MASK \
    RF_ORC, RF_TROLL, RF_GIANT, RF_DRAGON, RF_DEMON, \
    RF_UNDEAD, RF_EVIL, RF_ANIMAL, RF_METAL, RF_NONLIVING

/* Drop flags to be revealed on first kill */
#define RF_DROP_MASK \
    RF_DROP_GOOD, RF_DROP_GREAT, RF_ONLY_ITEM, RF_ONLY_GOLD, \
    RF_DROP_20, RF_DROP_40, RF_DROP_60, \
    RF_DROP_4, RF_DROP_3, RF_DROP_2, RF_DROP_1

/*
 * Is that monster shimmering?
 */
#define monster_shimmer(R) \
    (rf_has((R)->flags, RF_ATTR_MULTI) || rf_has((R)->flags, RF_ATTR_FLICKER))

/*
 * Monster spell types
 */
struct monster_spell
{
    struct monster_spell *next;
    u16b index;             /* Numerical index (RSF_FOO) */
    int msgt;               /* Flag for message colouring */
    char *message;          /* Description of the attack */
    char *blind_message;    /* Description of the attack if unseen */
    char *miss_message;     /* Description of a missed attack */
    char *save_message;     /* Message on passing saving throw, if any */
    char *lore_desc;        /* Description of the attack used in lore text */
    int hit;                /* To-hit level for the attack */
    struct effect *effect;  /* Effect(s) of the spell */
    random_value power;     /* Relative power of the spell */
};

/** Variables **/

extern struct monster_pain *pain_messages;
extern struct monster_spell *monster_spells;
extern const struct monster_race *ref_race;

#endif /* MONSTER_MONSTER_H */
