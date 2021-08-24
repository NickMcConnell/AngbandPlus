/*
 * File: mon-lore.h
 * Purpose: Monster memory code.
 */

#ifndef MONSTER_LORE_H
#define MONSTER_LORE_H

extern int spell_color(struct player *p, int spell_index);
extern void lore_learn_spell_if_has(struct monster_lore *lore, const struct monster_race *race,
    int flag);
extern void lore_learn_spell_if_visible(struct monster_lore *lore, bool visible, int flag);
extern void lore_learn_flag_if_visible(struct monster_lore *lore, bool visible, int flag);
extern void lore_update(const struct monster_race *race, struct monster_lore *lore);
extern void lore_do_probe(struct player *p, struct monster *mon);
extern bool lore_is_fully_known(struct player *p, const struct monster_race *race);
extern void lore_treasure(struct player *p, struct monster *mon, int num_item, int num_gold);
extern void monster_flags_known(const struct monster_race *race, const struct monster_lore *lore,
    bitflag flags[RF_SIZE]);
extern void lore_append_kills(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, const bitflag known_flags[RF_SIZE]);
extern void lore_append_flavor(struct player *p, const struct monster_race *race);
extern void lore_append_movement(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_toughness(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_exp(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_drop(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_abilities(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_awareness(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_friends(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_spells(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void lore_append_attack(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE]);
extern void get_global_lore(struct player *p, const struct monster_race *race,
    struct monster_lore* lore);
extern struct monster_lore *get_lore(struct player *p, const struct monster_race *race);

#endif /* MONSTER_LORE_H */
