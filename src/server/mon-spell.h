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

/** Functions **/

extern bool mon_spell_is_innate(int index);
extern const struct monster_spell *monster_spell_by_index(int index);
extern void do_mon_spell(struct player *p, struct chunk *c, struct monster *target_mon, int index,
    struct monster *mon, bool seen);
extern bool test_spells(bitflag *f, int types);
extern void set_breath(bitflag *f);
extern void ignore_spells(bitflag *f, int types);
extern void unset_spells(struct player *p, bitflag *spells, bitflag *flags, bitflag *pflags,
    struct element_info *el, const struct monster *mon);
extern int breath_dam(int type, int hp);
extern void create_mon_spell_mask(bitflag *f, ...);
extern const char *mon_spell_lore_description(int index, const struct monster_race *race);
extern int mon_spell_lore_damage(int index, const struct monster_race *race, bool know_hp);
extern void init_spells(bitflag *f);
extern bool is_spell_summon(int index);
extern int spell_effect(int index);
extern int breath_effect(struct player *p, bitflag mon_breath[RSF_SIZE]);

#endif /* MONSTER_SPELL_H */
