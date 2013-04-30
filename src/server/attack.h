/*
 * File: attack.h
 * Purpose: Attack interface
 */

#ifndef ATTACK_H
#define ATTACK_H

/* attack.c */
extern void py_attack(int Ind, int y, int x);
extern void un_power(struct player *p, int m_idx, bool* obvious);
extern void eat_gold(struct player *p, int m_idx);
extern void eat_item(struct player *p, int m_idx, bool* obvious, int* blinked);
extern void eat_fud(struct player *p, int p_idx, bool* obvious);
extern void eat_light(struct player *p, bool* obvious);
extern void drain_xp(struct player *p, int amt);
extern void drop_weapon(struct player *p, int damage);
extern int breakage_chance(const object_type *o_ptr, bool hit_target);
extern bool test_hit(int chance, int ac, int vis);
extern void apply_poly_brand(int r_idx, int m, bitflag f[OF_SIZE],
    bitflag known_f[OF_SIZE]);
extern void apply_bow_brand(int bow_brand_t, bitflag f[OF_SIZE],
    bitflag known_f[OF_SIZE]);

struct attack_result
{
    bool success;
    int dmg;
    u32b msg_type;
    const char *hit_verb;
    bool do_poison;
};

/*
 * ranged_attack is a function pointer, used to execute a kind of attack.
 *
 * This allows us to abstract details of throwing, shooting, etc. out while
 * keeping the core projectile tracking, monster cleanup, and display code
 * in common.
 */
typedef struct attack_result (*ranged_attack) (int Ind, object_type *o_ptr, int y, int x);

#endif /* ATTACK_H */
