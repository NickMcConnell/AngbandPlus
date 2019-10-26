#ifndef INCLUDED_MON_H
#define INCLUDED_MON_H

#include "c-vec.h"

typedef struct dun_s dun_t, *dun_ptr; /* mon.h */
typedef struct dun_type_s dun_type_t, *dun_type_ptr; /* plr.h */

/* XXX Refactor monster_type, monster_race, etc ... */
typedef struct monster_type mon_t, *mon_ptr;
typedef struct monster_race mon_race_t, *mon_race_ptr;
typedef struct pack_info_s mon_pack_t, *mon_pack_ptr;

typedef bool (*mon_p)(mon_ptr mon);
typedef void (*mon_f)(mon_ptr mon);
typedef bool (*mon_race_p)(mon_race_ptr race);
typedef void (*mon_lore_f)(mon_ptr mon);


/************************************************************************
 * Monster Allocation
 ************************************************************************/
typedef int (*mon_alloc_weight_f)(mon_race_ptr race, int prob);

extern vec_ptr mon_alloc_tbl; /* vec<mon_race_ptr> */
extern void mon_alloc_init(void);
extern vec_ptr mon_alloc_current_tbl(void);
extern void mon_alloc_clear_filters(void);
extern void mon_alloc_push_filter(mon_race_p filter);
extern void mon_alloc_pop_filter(void);
extern void mon_alloc_push_weight(mon_alloc_weight_f weight);
extern void mon_alloc_pop_weight(void);
extern mon_race_ptr mon_alloc_choose(int level);
extern mon_race_ptr mon_alloc_choose_aux(int level, u32b options);
extern mon_race_ptr mon_alloc_choose_aux2(vec_ptr tbl, int level, int min_level, u32b options);

/* surface checks */
extern bool mon_alloc_town(mon_race_ptr race);
extern bool mon_alloc_ocean(mon_race_ptr race);
extern bool mon_alloc_shore(mon_race_ptr race);
extern bool mon_alloc_waste(mon_race_ptr race);
extern bool mon_alloc_grass(mon_race_ptr race);
extern bool mon_alloc_woods(mon_race_ptr race);
extern bool mon_alloc_volcano(mon_race_ptr race);
extern bool mon_alloc_mountain(mon_race_ptr race);
extern bool mon_alloc_surface(mon_race_ptr race);

/* dungeon checks */
extern bool mon_alloc_deep_water(mon_race_ptr race);
extern bool mon_alloc_shallow_water(mon_race_ptr race);
extern bool mon_alloc_lava(mon_race_ptr race);
extern bool mon_alloc_floor(mon_race_ptr race);
extern bool mon_alloc_dungeon(mon_race_ptr race);

extern mon_race_p mon_alloc_feat_p(int feat_id);

/************************************************************************
 * XXX
 ************************************************************************/
extern mon_ptr mon_alloc(void);
extern void    mon_free(mon_ptr mon);

extern dun_ptr mon_dun(mon_ptr mon);
extern int     mon_ac(mon_ptr mon);

extern bool mon_show_msg(mon_ptr mon);

extern mon_race_ptr mon_race(mon_ptr mon);
extern mon_race_ptr mon_race_lookup(int id);
extern mon_race_ptr mon_apparent_race(mon_ptr mon);
extern mon_race_ptr mon_true_race(mon_ptr mon);

extern void mon_anger(mon_ptr mon);
extern void mon_anger_spell(mon_ptr mon, int dam);
extern void mon_anger_shoot(mon_ptr mon, int dam);

extern int  mon_stun_amount(int dam);
extern bool mon_stun(mon_ptr mon, int amt);

/* saving throws */
extern bool mon_save_stun(int rlev, int dam);
extern bool mon_save_slow(int rlev, int dam);
extern bool mon_save_disenchant(int r_idx, int dam, int flags);
extern bool mon_save_poly(int rlev, int dam);
extern bool mon_save_smash(int rlev, int dam);
extern bool mon_save_psi(int rlev, int dam);
extern bool mon_save_time(int r_idx, int dam, int flags);
extern bool mon_save_p(int r_idx, int stat);
extern bool mon_save_m(int r_idx, int src_r_idx);
extern bool mon_save_aux(int r_idx, int power);
extern int  mon_save_r_level(int r_idx);

extern void mon_load(mon_ptr mon, savefile_ptr file);
extern void mon_save(mon_ptr mon, savefile_ptr file);

extern bool mon_can_attack(mon_ptr mon);

extern cptr mon_race_describe_singular(char c);  /* 'o'->"Orc", etc. */
extern cptr mon_race_describe_plural(char c);  /* 'o'->"Orcs", etc. */

/* Monster Info and Lore */
extern bool mon_is_dead(mon_ptr mon);
extern bool unique_is_dead(int id);
extern bool mon_is_deleted(mon_ptr mon);
extern bool mon_is_smart(mon_ptr mon, int sm);
extern bool mon_is_pet(mon_ptr mon);
extern bool mon_is_friendly(mon_ptr mon);
extern bool mon_is_cloned(mon_ptr mon);
extern bool mon_is_guardian(mon_ptr mon);

extern bool mon_is_animal(mon_ptr mon);
extern bool mon_race_is_animal(mon_race_ptr race);
extern void mon_lore_animal(mon_ptr mon);

extern bool mon_is_demon(mon_ptr mon);
extern bool mon_race_is_demon(mon_race_ptr race);
extern void mon_lore_demon(mon_ptr mon);

extern bool mon_is_dragon(mon_ptr mon);
extern bool mon_race_is_dragon(mon_race_ptr race);
extern void mon_lore_dragon(mon_ptr mon);

extern bool mon_is_evil(mon_ptr mon);
extern bool mon_race_is_evil(mon_race_ptr race);
extern void mon_lore_evil(mon_ptr mon);

extern bool mon_is_good(mon_ptr mon);
extern bool mon_race_is_good(mon_race_ptr race);
extern void mon_lore_good(mon_ptr mon);

extern bool mon_is_giant(mon_ptr mon);
extern bool mon_race_is_giant(mon_race_ptr race);
extern void mon_lore_giant(mon_ptr mon);

extern bool mon_is_human(mon_ptr mon);
extern bool mon_race_is_human(mon_race_ptr race);
extern void mon_lore_human(mon_ptr mon);

extern bool mon_is_living(mon_ptr mon);
extern bool mon_race_is_living(mon_race_ptr race);
extern void mon_lore_living(mon_ptr mon);

extern bool mon_is_orc(mon_ptr mon);
extern bool mon_race_is_orc(mon_race_ptr race);
extern void mon_lore_orc(mon_ptr mon);

extern bool mon_is_troll(mon_ptr mon);
extern bool mon_race_is_troll(mon_race_ptr race);
extern void mon_lore_troll(mon_ptr mon);

extern bool mon_is_undead(mon_ptr mon);
extern bool mon_race_is_undead(mon_race_ptr race);
extern void mon_lore_undead(mon_ptr mon);

extern bool mon_is_unique(mon_ptr mon);
extern bool mon_race_is_unique(mon_race_ptr race);

/* resistances */
extern bool mon_not_res_acid(mon_ptr mon);
extern bool mon_res_acid(mon_ptr mon);
extern bool mon_race_res_acid(mon_race_ptr race);
extern void mon_lore_res_acid(mon_ptr mon);

extern bool mon_im_acid(mon_ptr mon);
extern bool mon_race_im_acid(mon_race_ptr race);
extern void mon_lore_im_acid(mon_ptr mon);

extern bool mon_not_res_elec(mon_ptr mon);
extern bool mon_res_elec(mon_ptr mon);
extern bool mon_race_res_elec(mon_race_ptr race);
extern void mon_lore_res_elec(mon_ptr mon);

extern bool mon_im_elec(mon_ptr mon);
extern bool mon_race_im_elec(mon_race_ptr race);
extern void mon_lore_im_elec(mon_ptr mon);

extern bool mon_vuln_fire(mon_ptr mon);
extern bool mon_race_vuln_fire(mon_race_ptr race);
extern void mon_lore_vuln_fire(mon_ptr mon);

extern bool mon_not_res_fire(mon_ptr mon);
extern bool mon_res_fire(mon_ptr mon);
extern bool mon_race_res_fire(mon_race_ptr race);
extern void mon_lore_res_fire(mon_ptr mon);

extern bool mon_im_fire(mon_ptr mon);
extern bool mon_race_im_fire(mon_race_ptr race);
extern void mon_lore_im_fire(mon_ptr mon);

extern bool mon_vuln_cold(mon_ptr mon);
extern bool mon_race_vuln_cold(mon_race_ptr race);
extern void mon_lore_vuln_cold(mon_ptr mon);

extern bool mon_not_res_cold(mon_ptr mon);
extern bool mon_res_cold(mon_ptr mon);
extern bool mon_race_res_cold(mon_race_ptr race);
extern void mon_lore_res_cold(mon_ptr mon);

extern bool mon_im_cold(mon_ptr mon);
extern bool mon_race_im_cold(mon_race_ptr race);
extern void mon_lore_im_cold(mon_ptr mon);

extern bool mon_not_res_pois(mon_ptr mon);
extern bool mon_res_pois(mon_ptr mon);
extern bool mon_race_res_pois(mon_race_ptr race);
extern void mon_lore_res_pois(mon_ptr mon);

extern bool mon_im_pois(mon_ptr mon);
extern bool mon_race_im_pois(mon_race_ptr race);
extern void mon_lore_im_pois(mon_ptr mon);

extern bool mon_vuln_lite(mon_ptr mon);
extern bool mon_race_vuln_lite(mon_race_ptr race);
extern void mon_lore_vuln_lite(mon_ptr mon);

extern bool mon_not_res_lite(mon_ptr mon);
extern bool mon_res_lite(mon_ptr mon);
extern bool mon_race_res_lite(mon_race_ptr race);
extern void mon_lore_res_lite(mon_ptr mon);

extern bool mon_not_res_dark(mon_ptr mon);
extern bool mon_res_dark(mon_ptr mon);
extern bool mon_race_res_dark(mon_race_ptr race);
extern void mon_lore_res_dark(mon_ptr mon);

extern bool mon_not_res_plasma(mon_ptr mon);
extern bool mon_res_plasma(mon_ptr mon);
extern bool mon_race_res_plasma(mon_race_ptr race);
extern void mon_lore_res_plasma(mon_ptr mon);

/* Monster Drops */
typedef struct mon_drop_s mon_drop_t, *mon_drop_ptr;
struct mon_drop_s
{
    mon_drop_ptr next;
    byte pct;          /* odds of this drop rule applying (0 => 100%) */
    byte dd, ds, base; /* number of objects: XdY+Z. Or 1 if no dice given. */
    byte theme;        /* use a theme rather than an obj_drop_t rule */
    obj_drop_t drop;   /* info on how to make objects for this rule */
};
extern mon_drop_ptr mon_drop_alloc(void);
extern void         mon_drop_free(mon_drop_ptr drop);

extern vec_ptr mon_drop_make(mon_ptr mon);
extern errr    mon_drop_parse(char *buf, mon_race_ptr race, int options); /* parse a single O: line in r_info */
extern void    mon_drop_init(mon_ptr mon);
extern obj_ptr mon_pick_pocket(mon_ptr mon);

/************************************************************************* 
 * Monster Blows
 *   Monsters use blows to attack the player and each other. Set these
 *   in ../lib/edit/r_info.txt on the B: line. Browse the file to learn
 *   the syntax. Savefiles store the "lore" fields by ordinal position,
 *   so reordering blows and effects in r_info may mis-inform players
 *   about their experiences (but won't crash). Monster blows are stored
 *   in monster_race.blows and each monster can have as many as you want.
 *   Each mon_blow_t can also have as many blows as you like. So, you can
 *   totally maul the player if that is your goal!
 *
 *   Players use blows for innate attacks. For example, the possessor will
 *   copy the monster's blows allowing the player to use them in melee.
 *   Most monster mode races also dynamically build blows for the player 
 *   to use. And finally, a few attack mutations will grant weird innate
 *   attacks (horns, beak, trunk, tentacles, etc). All of these blows are
 *   stored in p_ptr->innate_blows in response to the PU_INNATE update
 *   flag. See calc_innate_attacks on race_t and class_t.
 *************************************************************************/

/* [1] Blows have multiple effects, each with its own dice and probability
 *     of occuring. The first effect on the blow is considered the "base"
 *     effect and can be boosted by criticals (player and monster) and
 *     slays (player). Usually, the base effect codes pure damage while
 *     later effects do special stuff. For example:
 *       B:HIT:HURT(5d5+10):STEAL_ITEM
 *       B:TOUCH:DAM(3d3):DISENCHANT
 *       B:SLASH:HURT(8d4+10):CUT(3d3, 25%) */
typedef struct {
    s16b   effect; /* either RBE_* or GF_* ... they don't overlap. */
    dice_t dice;   /* damage or the power of the effect */
    byte   pct;    /* probability of occuring: 0 => 100% (as does 100) */
    s16b   lore;   /* monster lore: number of times this effect experienced */
} mon_effect_t, *mon_effect_ptr;

/* [2] monster blow flags (MBF_) 
 *     Note that mon_blow_alloc(method) sets blow->flags with table set
 *     defaults (_mon_blow_info). You may then override them as necessary.
 *     For example, RBM_HIT has MBF_MASK_HAND by default, since usually
 *     we can assume a monster hits with its hand (e.g. Novice warrior).
 *     However, various mutations need to turn this off so the player can
 *     still use melee weapons. */
#define MBF_TOUCH      0x0001 /* blow touches so apply auras */
#define MBF_ALLOW_CRIT 0x0002 /* blow can land criticals (if base effect is RBE_HURT or _SHATTER) */
#define MBF_POSSESSOR  0x0004 /* for possessor proficiency */
#define MBF_MONK       0x0008 /* for martial arts XXX unused atm */
#define MBF_MASK_HAND  0x0010 /* player skips this blow when using a weapon (e.g. RBM_PUNCH) */
#define MBF_MASK_BLIND 0x0020 /* player skips this blow when blinded (e.g. RBM_GAZE) */

#define MBF_MONK_PUNCH (MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK | MBF_MASK_HAND)
#define MBF_MONK_KICK  (MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK)

/* [3] The monster blow (mon_blow_t)
 *     Use mon_blow_alloc(method) to create one and mon_blow_free to destroy one.
 *     Since we use dynamic storage, never declare one on the stack. */
typedef struct {
    cptr name;      /* player: display and skills */
    cptr msg;       /* player: custom hit message (e.g. "You strangle %s.") */
    byte method;    /* RBM_* code gives default messaging and behavior. */
    byte power;     /* added to skill. This is 3x a normal to hit bonus (cf BTH_PLUS_ADJ) */
    s16b blows;     /* fractional ... 125 is 1.25 blows per round (25% 2 blows; 75% 1 blow) */
    s16b lore;      /* monster lore: number of times this blow seen (hit or miss) */
    s16b flags;     /* MBF_* flags: default from _mon_blow_info table in mon.c */
    s16b weight;    /* criticals: if 0 see _mon_blow_weight(). Can be set in r_info. */
    byte allocated; /* effects: vec<mon_effect_t> */
    byte effect_ct; /* iterate effects[i] for 0 <= i < effect_ct */
    mon_effect_ptr effects; /* use mon_blow_push_effect() and mon_blow_pop_effect() to manage effects */
} mon_blow_t, *mon_blow_ptr;

/* find the first blow of a particular type (e.g. RBM_EXPLODE) */
extern mon_blow_ptr mon_blows_find(vec_ptr blows, int method);

extern mon_blow_ptr mon_blow_alloc(int method);
extern void         mon_blow_free(mon_blow_ptr blow);
extern mon_blow_ptr mon_blow_copy(mon_blow_ptr blow);
extern bool         mon_blow_allow_crit(mon_blow_ptr blow);
extern mon_effect_ptr mon_blow_push_effect(mon_blow_ptr blow, int effect, dice_t dice);
extern mon_effect_ptr mon_blow_push_effect_aux(mon_blow_ptr blow, mon_effect_ptr effect); /* r_info parser */
extern void         mon_blow_pop_effect(mon_blow_ptr blow); /* cf _deadly_bite_spell */
extern dice_t       mon_blow_base_dice(mon_blow_ptr blow);
extern int          mon_blow_base_effect(mon_blow_ptr blow);

/* [4] Finally, we have information describing the various RBM_* methods.
 *     These are used for default messages and behavior during combat.
 *     See _mon_blow_info for the table. */
typedef struct {
    int  id;          /* code:    RBM_GAZE */
    cptr name;        /* display: "Gaze" */
    cptr mon_hit_msg; /* monster: "%^s gazes at %s." */
    cptr plr_hit_msg; /* player:  "You gaze at %s." */
    cptr parse;       /* r_info:  "GAZE" */
    int  flags;       /* info:    RBM_MASK_BLIND */
} mon_blow_info_t, *mon_blow_info_ptr;

extern mon_blow_info_ptr mon_blow_info_lookup(int method);
extern mon_blow_info_ptr mon_blow_info_parse(cptr name); /* r_info parser */

extern mon_effect_ptr mon_auras_find(mon_race_ptr race, int effect);

#endif
