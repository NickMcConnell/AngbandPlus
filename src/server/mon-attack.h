/*
 * File: mon-attack.h
 * Purpose: Monster attacks
 */

#ifndef MONSTER_ATTACK_H
#define MONSTER_ATTACK_H

extern bool make_attack_spell(struct actor *who, struct chunk *c, struct monster *mon,
    int target_m_dis);
extern bool check_hit(struct actor *who, int power, int level);
extern int adjust_dam_armor(int damage, int ac);
extern bool make_attack_normal(struct monster *mon, struct actor *who);
extern int get_cut(random_value dice, int d_dam);
extern int get_stun(random_value dice, int d_dam);
extern int get_power(int effect);

#endif /* MONSTER_ATTACK_H */
