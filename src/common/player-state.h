/*
 * File: player-state.h
 * Purpose: Player state
 */

#ifndef INCLUDED_PLAYER_STATE_H
#define INCLUDED_PLAYER_STATE_H

/*
 * Indexes of the various "stats" (hard-coded by savefiles, etc).
 */
enum
{
    #define STAT(a, b, c, d, e, f, g, h) STAT_##a,
    #include "list-stats.h"
    #undef STAT

    STAT_MAX
};

/*
 * Skill indexes
 */
enum
{
    SKILL_DISARM,           /* Skill: Disarming */
    SKILL_DEVICE,           /* Skill: Magic Devices */
    SKILL_SAVE,             /* Skill: Saving throw */
    SKILL_STEALTH,          /* Skill: Stealth factor */
    SKILL_SEARCH,           /* Skill: Searching ability */
    SKILL_SEARCH_FREQUENCY, /* Skill: Searching frequency */
    SKILL_TO_HIT_MELEE,     /* Skill: To hit (normal) */
    SKILL_TO_HIT_BOW,       /* Skill: To hit (shooting) */
    SKILL_TO_HIT_THROW,     /* Skill: To hit (throwing) */
    SKILL_DIGGING,          /* Skill: Digging */

    SKILL_MAX
};

/*
 * Player race and class flags
 */
enum
{
    #define PF(a, b, c) PF_##a,
    #include "list-player-flags.h"
    #undef PF
    PF__MAX
};

#define PF_SIZE                FLAG_SIZE(PF__MAX)

#define pf_has(f, flag)        flag_has_dbg(f, PF_SIZE, flag, #f, #flag)
#define pf_next(f, flag)       flag_next(f, PF_SIZE, flag)
#define pf_is_empty(f)         flag_is_empty(f, PF_SIZE)
#define pf_is_full(f)          flag_is_full(f, PF_SIZE)
#define pf_is_inter(f1, f2)    flag_is_inter(f1, f2, PF_SIZE)
#define pf_is_subset(f1, f2)   flag_is_subset(f1, f2, PF_SIZE)
#define pf_is_equal(f1, f2)    flag_is_equal(f1, f2, PF_SIZE)
#define pf_on(f, flag)         flag_on_dbg(f, PF_SIZE, flag, #f, #flag)
#define pf_off(f, flag)        flag_off(f, PF_SIZE, flag)
#define pf_wipe(f)             flag_wipe(f, PF_SIZE)
#define pf_setall(f)           flag_setall(f, PF_SIZE)
#define pf_negate(f)           flag_negate(f, PF_SIZE)
#define pf_copy(f1, f2)        flag_copy(f1, f2, PF_SIZE)
#define pf_union(f1, f2)       flag_union(f1, f2, PF_SIZE)
#define pf_comp_union(f1, f2)  flag_comp_union(f1, f2, PF_SIZE)
#define pf_inter(f1, f2)       flag_inter(f1, f2, PF_SIZE)
#define pf_diff(f1, f2)        flag_diff(f1, f2, PF_SIZE)

#define player_has(P, flag) \
    (pf_has((P)->race->pflags, (flag)) || pf_has((P)->clazz->pflags, (flag)))

/*
 * All the variable state that changes when you put on/take off equipment.
 * Player flags are not currently variable, but useful here so monsters can
 * learn them.
 */
struct player_state
{
    /*** Angband extracted fields ***/

    s16b speed;                 /* Current speed */
    s16b num_blows;             /* Number of blows x100 */
    s16b num_shots;             /* Number of shots */
    byte ammo_mult;             /* Ammo multiplier */
    byte ammo_tval;             /* Ammo variety */
    s16b stat_add[STAT_MAX];    /* Modifiers to stat values */
    s16b stat_ind[STAT_MAX];    /* Indexes into stat tables */
    s16b stat_use[STAT_MAX];    /* Current modified stats */
    s16b stat_top[STAT_MAX];    /* Maximal modified stats */
    s16b ac;                    /* Base ac */
    s16b to_a;                  /* Bonus to ac */
    s16b to_h;                  /* Bonus to hit */
    s16b to_d;                  /* Bonus to dam */
    s16b see_infra;             /* Infravision range */
    s16b cur_light;             /* Radius of light (if any) */
    s16b skills[SKILL_MAX];     /* Skills */
    int noise;                  /* Derived from stealth */
    bool heavy_wield;           /* Heavy weapon */
    bool heavy_shoot;           /* Heavy shooter */
    bool icky_wield;            /* Icky weapon */
    bool cumber_armor;          /* Mana draining armor */
    bool cumber_glove;          /* Mana draining gloves */
    bitflag flags[OF_SIZE];     /* Status flags from race and items */
    bitflag pflags[PF_SIZE];    /* Player intrinsic flags */
    struct element_info el_info[ELEM_MAX];  /* Resists from race and items */

    /*** PWMAngband extracted fields ***/

    bool cumber_shield;         /* Encumbering shield */
    s16b frac_blow;             /* Blow frac (%) */
};

#endif /* INCLUDED_PLAYER_STATE_H */
