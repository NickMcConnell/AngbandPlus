/*
 * File: player-state.h
 * Purpose: Player state
 */

#ifndef INCLUDED_PLAYER_STATE_H
#define INCLUDED_PLAYER_STATE_H

/*
 * Indexes of the player stats (hard-coded by savefiles).
 */
enum
{
    #define STAT(a) STAT_##a,
    #include "list-stats.h"
    #undef STAT

    STAT_MAX
};

/*
 * Skill indexes
 */
enum
{
    SKILL_DISARM_PHYS,      /* Disarming - physical */
    SKILL_DISARM_MAGIC,     /* Disarming - magical */
    SKILL_DEVICE,           /* Magic Devices */
    SKILL_SAVE,             /* Saving throw */
    SKILL_STEALTH,          /* Stealth factor */
    SKILL_SEARCH,           /* Searching ability */
    SKILL_TO_HIT_MELEE,     /* To hit (normal) */
    SKILL_TO_HIT_BOW,       /* To hit (shooting) */
    SKILL_TO_HIT_THROW,     /* To hit (throwing) */
    SKILL_DIGGING,          /* Digging */

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

    int stat_add[STAT_MAX];     /* Equipment stat bonuses */
    int stat_ind[STAT_MAX];     /* Indexes into stat tables */
    int stat_use[STAT_MAX];     /* Current modified stats */
    int stat_top[STAT_MAX];     /* Maximal modified stats */
    int skills[SKILL_MAX];      /* Skills */
    int speed;                  /* Current speed */
    int num_blows;              /* Number of blows x100 */
    int num_shots;              /* Number of shots */
    int ammo_mult;              /* Ammo multiplier */
    int ammo_tval;              /* Ammo variety */
    int ac;                     /* Base ac */
    int to_a;                   /* Bonus to ac */
    int to_h;                   /* Bonus to hit */
    int to_d;                   /* Bonus to dam */
    int see_infra;              /* Infravision range */
    int cur_light;              /* Radius of light (if any) */
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
    int frac_blow;              /* Blow frac (%) */
};

#endif /* INCLUDED_PLAYER_STATE_H */
