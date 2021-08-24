/*
 * File: obj-slays.h
 * Purpose: Structures and functions for dealing with slays and brands
 */

#ifndef OBJECT_SLAYS_H
#define OBJECT_SLAYS_H

extern struct slay *slays;
extern struct brand *brands;

/*** Functions ***/

extern bool same_monsters_slain(int slay1, int slay2);
extern bool append_slay(bool **current, int index);
extern bool copy_slays(bool **dest, bool *source);
extern bool append_brand(bool **current, int index);
extern bool copy_brands(bool **dest, bool *source);
extern bool append_random_brand(bool **current, struct brand **brand);
extern bool append_random_slay(bool **current, struct slay **slay);
extern int brand_count(bool *local_brands);
extern int slay_count(bool *local_slays);
extern void improve_attack_modifier(struct player *p, struct object *obj, struct source *who,
    int *best_mult, struct side_effects *effects, char *verb, size_t len, bool range);
extern bool react_to_slay(struct object *obj, const struct monster *mon);
extern bool brands_are_equal(const struct object *obj1, const struct object *obj2);
extern bool slays_are_equal(const struct object *obj1, const struct object *obj2);
extern int get_brand(const char *name, int multiplier);
extern int get_poly_brand(struct monster_race *race, int method);
extern int get_bow_brand(struct bow_brand *brand);

#endif /* OBJECT_SLAYS_H */
