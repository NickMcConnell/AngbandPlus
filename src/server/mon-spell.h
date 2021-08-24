/*
 * File: mon-spell.h
 * Purpose: Structures and functions for monster spells
 */

#ifndef MONSTER_SPELL_H
#define MONSTER_SPELL_H

/** Macros **/

#define rsf_has(f, flag)       flag_has_dbg(f, RSF_SIZE, flag, #f, #flag)
#define rsf_next(f, flag)      flag_next(f, RSF_SIZE, flag)
#define rsf_count(f)           flag_count(f, RSF_SIZE)
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
#define rsf_inter(f1, f2)      flag_inter(f1, f2, RSF_SIZE)
#define rsf_diff(f1, f2)       flag_diff(f1, f2, RSF_SIZE)

/*
 * Breath attacks
 */
#define RSF_BREATH_MASK \
    RSF_BR_ACID, RSF_BR_ELEC, RSF_BR_FIRE, RSF_BR_COLD, \
    RSF_BR_POIS, RSF_BR_NETH, RSF_BR_LIGHT, RSF_BR_DARK, \
    RSF_BR_SOUN, RSF_BR_CHAO, RSF_BR_DISE, RSF_BR_NEXU, \
    RSF_BR_TIME, RSF_BR_INER, RSF_BR_GRAV, RSF_BR_SHAR, \
    RSF_BR_PLAS, RSF_BR_WALL, RSF_BR_MANA, RSF_BR_WATE

/** Functions **/

extern bool mon_spell_is_innate(int index);
extern const struct monster_spell *monster_spell_by_index(int index);
extern void do_mon_spell(struct player *p, struct chunk *c, int index, struct monster *mon,
    bool seen);
extern bool test_spells(bitflag *f, int types);
extern void set_breath(bitflag *f);
extern void ignore_spells(bitflag *f, int types);
extern void unset_spells(struct player *p, bitflag *spells, bitflag *flags, bitflag *pflags,
    struct element_info *el, const struct monster_race *race);
extern int breath_dam(int type, int hp);
extern void create_mon_spell_mask(bitflag *f, ...);
extern const char *mon_spell_lore_description(int index, const struct monster_race *race);
extern int mon_spell_lore_damage(int index, const struct monster_race *race, bool know_hp);
extern void init_spells(bitflag *f);
extern void do_mon_spell_MvM(struct player *p, struct chunk *c, struct monster *target_m_ptr,
    int index, struct monster *mon, bool seen);
extern bool is_spell_summon(int index);
extern int spell_effect(int index);
extern int breath_effect(struct player *p, bitflag mon_breath[RSF_SIZE]);

#endif /* MONSTER_SPELL_H */
