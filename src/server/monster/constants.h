/*
 * File: constants.h
 * Purpose: Constants for monsters
 */

#ifndef INCLUDED_MONSTER_CONSTANTS_H
#define INCLUDED_MONSTER_CONSTANTS_H

/*
 * Maximum flow depth when using "MONSTER_FLOW"
 */
#define MONSTER_FLOW_DEPTH  32

/*
 * Monster blow methods
 */
enum
{
    #define RBM(x, y) RBM_##x,
    #include "list-blow-methods.h"
    #undef RBM
    RBM_MAX
};

/*
 * Monster blow effects
 */
enum
{
    #define RBE(x, y) RBE_##x,
    #include "list-blow-effects.h"
    #undef RBE
    RBE_MAX
};

/*
 * Hack: player immunity to mana draining cannot be represented by m_ptr->known_pflags
 */
#define SM_IMM_MANA     0x00000800

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
    RF_FRIEND, RF_FRIENDS, RF_ESCORT, RF_ESCORTS

/* "race" flags */
#define RF_RACE_MASK \
    RF_ORC, RF_TROLL, RF_GIANT, RF_DRAGON, RF_DEMON, \
    RF_UNDEAD, RF_EVIL, RF_ANIMAL, RF_METAL, RF_NONLIVING

/*
 * Some monster types are different
 */
#define monster_is_unusual(R) \
    flags_test((R)->flags, RF_SIZE, RF_DEMON, RF_UNDEAD, RF_STUPID, RF_NONLIVING, FLAG_END)

/*
 * Unliving monsters: undead, demons, elementals, golems, vortices
 */
#define monster_is_nonliving(R) \
    flags_test((R)->flags, RF_SIZE, RF_DEMON, RF_UNDEAD, RF_NONLIVING, FLAG_END)

/*** Obsolete ***/

#define NOT_USED                            /* to avoid confusion in spells1.c */

#define ARROWX_HIT                          50
#define ARROWX_DMG(level, dam_aspect)       damcalc(9, 6, dam_aspect)

#define ARROW1_HIT                          40
#define ARROW1_DMG(level, dam_aspect)       damcalc(1, 6, dam_aspect)

#define ARROW2_HIT                          40
#define ARROW2_DMG(level, dam_aspect)       damcalc(3, 6, dam_aspect)

#define ARROW3_HIT                          50
#define ARROW3_DMG(level, dam_aspect)       damcalc(5, 6, dam_aspect)

#define ARROW4_HIT                          50
#define ARROW4_DMG(level, dam_aspect)       damcalc(7, 6, dam_aspect)

#define BR_ACID_MAX                         1600
#define BR_ACID_DIVISOR                     3
#define RES_ACID_ADJ(dam, dam_aspect)       ((dam + 2) / 3)
#define DBLRES_ACID_ADJ(dam, dam_aspect)    ((dam + 8) / 9)
#define VULN_ACID_ADJ(dam, dam_aspect)      (dam * 2)

#define BR_ELEC_MAX                         1600
#define BR_ELEC_DIVISOR                     3
#define RES_ELEC_ADJ(dam, dam_aspect)       ((dam + 2) / 3)
#define DBLRES_ELEC_ADJ(dam, dam_aspect)    ((dam + 8) / 9)
#define VULN_ELEC_ADJ(dam, dam_aspect)      (dam * 2)

#define BR_FIRE_MAX                         1600
#define BR_FIRE_DIVISOR                     3
#define RES_FIRE_ADJ(dam, dam_aspect)       ((dam + 2) / 3)
#define DBLRES_FIRE_ADJ(dam, dam_aspect)    ((dam + 8) / 9)
#define VULN_FIRE_ADJ(dam, dam_aspect)      (dam * 2)

#define BR_COLD_MAX                         1600
#define BR_COLD_DIVISOR                     3
#define RES_COLD_ADJ(dam, dam_aspect)       ((dam + 2) / 3)
#define DBLRES_COLD_ADJ(dam, dam_aspect)    ((dam + 8) / 9)
#define VULN_COLD_ADJ(dam, dam_aspect)      (dam * 2)

#define BR_POIS_MAX                         800
#define BR_POIS_DIVISOR                     3
#define RES_POIS_ADJ(dam, dam_aspect)       ((dam + 2) / 3)

#define BR_NETH_MAX                         550
#define BR_NETH_DIVISOR                     6
#define RES_NETH_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_LIGHT_MAX                         400
#define BR_LIGHT_DIVISOR                     6
#define RES_LIGHT_ADJ(dam, dam_aspect)       (dam * 4 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_DARK_MAX                         400
#define BR_DARK_DIVISOR                     6
#define RES_DARK_ADJ(dam, dam_aspect)       (dam * 4 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_NEXU_MAX                         400
#define BR_NEXU_DIVISOR                     6
#define RES_NEXU_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_SOUN_MAX                         500
#define BR_SOUN_DIVISOR                     6
#define RES_SOUN_ADJ(dam, dam_aspect)       (dam * 5 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_CHAO_MAX                         500
#define BR_CHAO_DIVISOR                     6
#define RES_CHAO_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_DISE_MAX                         500
#define BR_DISE_DIVISOR                     6
#define RES_DISE_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_SHAR_MAX                         500
#define BR_SHAR_DIVISOR                     6
#define RES_SHAR_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_TIME_MAX                         150
#define BR_TIME_DIVISOR                     3
#define RES_TIME_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_INER_MAX                         200
#define BR_INER_DIVISOR                     6

#define BR_FORC_MAX                         200
#define BR_FORC_DIVISOR                     6

#define BR_GRAV_MAX                         200
#define BR_GRAV_DIVISOR                     3
#define RES_GRAV_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_PLAS_MAX                         150
#define BR_PLAS_DIVISOR                     6

#define BR_MANA_MAX                         250
#define BR_MANA_DIVISOR                     3
#define RES_MANA_ADJ(dam, dam_aspect)       (dam * 6 / (damcalc(1, 6, dam_aspect) + 6))

#define BR_WATE_MAX                         300
#define BR_WATE_DIVISOR                     3

#define BOULDER_HIT                         60
#define BOULDER_DMG(level, dam_aspect)      damcalc(1 + level / 7, 12, dam_aspect)

#define BA_ACID_DMG(level, dam_aspect)      (damcalc(1, level * 3, dam_aspect) + 15)
#define BA_ELEC_DMG(level, dam_aspect)      (damcalc(1, level * 3 / 2, dam_aspect) + 8)
#define BA_FIRE_DMG(level, dam_aspect)      (damcalc(1, level * 7 / 2, dam_aspect) + 10)
#define BA_COLD_DMG(level, dam_aspect)      (damcalc(1, level * 3 / 2, dam_aspect) + 10)
#define BA_POIS_DMG(level, dam_aspect)      damcalc(12, 2, dam_aspect)
#define BA_NETH_DMG(level, dam_aspect)      (damcalc(10, 10, dam_aspect) + level + 50)
#define BA_WATE_DMG(level, dam_aspect)      (damcalc(1, level * 5 / 2, dam_aspect) + 50)
#define BA_MANA_DMG(level, dam_aspect)      (damcalc(10, 10, dam_aspect) + level * 5)
#define BA_DARK_DMG(level, dam_aspect)      (damcalc(10, 10, dam_aspect) + level * 5)

#define MIND_BLAST_DMG(level, dam_aspect)   damcalc(8, 8, dam_aspect)
#define BRAIN_SMASH_DMG(level, dam_aspect)  damcalc(12, 15, dam_aspect)

#define CAUSE_1_DMG(level, dam_aspect)      damcalc(3, 8, dam_aspect)
#define CAUSE_2_DMG(level, dam_aspect)      damcalc(8, 8, dam_aspect)
#define CAUSE_3_DMG(level, dam_aspect)      damcalc(10, 15, dam_aspect)
#define CAUSE_4_DMG(level, dam_aspect)      damcalc(15, 15, dam_aspect)
#define CAUSE_4_CUT                         damroll(10, 10)

#define BO_ACID_DMG(level, dam_aspect)      (damcalc(7, 8, dam_aspect) + level / 3)
#define BO_ELEC_DMG(level, dam_aspect)      (damcalc(4, 8, dam_aspect) + level / 3)
#define BO_FIRE_DMG(level, dam_aspect)      (damcalc(9, 8, dam_aspect) + level / 3)
#define BO_COLD_DMG(level, dam_aspect)      (damcalc(6, 8, dam_aspect) + level / 3)
#define BO_NETH_DMG(level, dam_aspect)      (damcalc(5, 5, dam_aspect) + level * 3 / 2 + 30)
#define BO_WATE_DMG(level, dam_aspect)      (damcalc(10, 10, dam_aspect) + level)
#define BO_MANA_DMG(level, dam_aspect)      (damcalc(1, level * 7 / 2, dam_aspect) + 50)
#define BO_PLAS_DMG(level, dam_aspect)      (damcalc(8, 7, dam_aspect) + level + 10)
#define BO_ICEE_DMG(level, dam_aspect)      (damcalc(6, 6, dam_aspect) + level)
#define MISSILE_DMG(level, dam_aspect)      (damcalc(2, 6, dam_aspect) + level / 3)

#endif /* INCLUDED_MONSTER_CONSTANTS_H */
