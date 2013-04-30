/*
 * File: mon-spell.h
 * Purpose: Structures and functions for monster spells
 */

#ifndef MONSTER_SPELL_H
#define MONSTER_SPELL_H

/** Constants **/

/* List of spell effects */
enum
{
    #define RSE(a, b, c, d, e, f, g, h, i, j, k) \
        RSE_##a,
    #include "list-spell-effects.h"
    #undef RSE
    RSE_MAX
};

/* Flags for non-timed spell effects */
enum spell_effect_flag
{
    S_INV_DAM,
    S_TELEPORT,
    S_TELE_TO,
    S_TELE_LEV,
    S_TELE_SELF,
    S_DRAIN_LIFE,
    S_DRAIN_STAT,
    S_SWAP_STAT,
    S_DRAIN_ALL,
    S_DISEN,
    S_ANIMAL,
    S_SPIDER,
    S_HOUND,
    S_HYDRA,
    S_AINU,
    S_DEMON,
    S_UNDEAD,
    S_DRAGON,
    S_HI_DEMON,
    S_HI_UNDEAD,
    S_HI_DRAGON,
    S_WRAITH,
    S_UNIQUE,
    S_KIN,
    S_MONSTER,
    S_MONSTERS,
    S_DRAIN_MANA,
    S_HEAL,
    S_DARKEN,
    S_TRAPS,
    S_AGGRAVATE,

    /* PWMAngband */
    S_MAGE_PROJECT,
    S_PRIEST_PROJECT,
    S_SORC_PROJECT,
    S_SHAD_PROJECT,
    S_HUNT_PROJECT,
    S_PSI_PROJECT,
    S_DEATH_PROJECT,
    S_GHOST_PROJECT,
    S_MIMIC_PROJECT,
    S_ELEM_PROJECT,
    S_SUMMON_PROJECT,
    S_POLY_BAT,
    S_JELLY,
    S_GOLEM,
    S_VORTEX,

    S_MAX
};

/* Minimum flag which can fail */
#define MIN_NONINNATE_SPELL RSF_BA_ACID

/** Macros **/

#define rsf_has(f, flag)       flag_has_dbg(f, RSF_SIZE, flag, #f, #flag)
#define rsf_next(f, flag)      flag_next(f, RSF_SIZE, flag)
#define rsf_is_empty(f)        flag_is_empty(f, RSF_SIZE)
#define rsf_is_full(f)         flag_is_full(f, RSF_SIZE)
#define rsf_is_inter(f1, f2)   flag_is_inter(f1, f2, RSF_SIZE)
#define rsf_is_subset(f1, f2)  flag_is_subset(f1, f2, RSF_SIZE)
#define rsf_is_equal(f1, f2)   flag_is_equal(f1, f2, RSF_SIZE)
#define rsf_on(f, flag)        flag_on_dbg(f, RSF_SIZE, flag, #f, #flag)
#define rsf_off(f, flag)       flag_off(f, RSF_SIZE, flag)
#define rsf_wipe(f)            flag_wipe(f, RSF_SIZE)
#define rsf_setall(f)          flag_setall(f, RSF_SIZE)
#define rsf_negate(f)          flag_negate(f, RSF_SIZE)
#define rsf_copy(f1, f2)       flag_copy(f1, f2, RSF_SIZE)
#define rsf_union(f1, f2)      flag_union(f1, f2, RSF_SIZE)
#define rsf_comp_union(f1, f2) flag_comp_union(f1, f2, RSF_SIZE)
#define rsf_inter(f1, f2)      flag_inter(f1, f2, RSF_SIZE)
#define rsf_diff(f1, f2)       flag_diff(f1, f2, RSF_SIZE)

/** Functions **/

extern int summon_monster_aux(struct player *p, int y, int x, int flag, int rlev, int max, int chance);
extern void do_side_effects(struct player *p, int spell, int dam, int m_idx, bool seen, bool check);
extern void do_mon_spell(int Ind, int spell, int m_idx, bool seen);
extern bool test_spells(bitflag *f, enum mon_spell_type type);
extern void set_spells(bitflag *f, enum mon_spell_type type);
extern void unset_spells(int Ind, bitflag *spells, bitflag *flags, const monster_race *r_ptr);
extern int best_spell_power(const monster_race *r_ptr, int resist);
extern void do_mon_spell_MvM(int Ind, int target_m_idx, int spell, int m_idx, bool seen);
extern bool is_spell_summon(int spell);
extern int spell_effect(int spell);

#endif /* MONSTER_SPELL_H */
