/*
 * File: obj-slays.h
 * Purpose: Structures and functions for dealing with slays and brands
 */

#ifndef OBJECT_SLAYS_H
#define OBJECT_SLAYS_H

/*** Functions ***/

extern void copy_slay(struct slay **dest, struct slay *source);
extern void copy_brand(struct brand **dest, struct brand *source);
extern void free_slay(struct slay *source);
extern void free_brand(struct brand *source);
extern bool append_fixed_brand(struct brand **dest, int element, int mult);
extern bool append_random_brand(struct brand **current);
extern bool append_fixed_slay(struct slay **dest, const char *name, int race_flag, int multiplier);
extern bool append_random_slay(struct artifact *art, bool melee);
extern int brand_count(struct brand *brands);
extern int slay_count(struct slay *slays);
extern struct brand *brand_collect(struct brand *b, const struct object *obj);
extern struct slay *slay_collect(struct slay *s, const struct object *obj);
extern void object_notice_brands(struct player *p, struct object *obj,
    const struct monster_race *race);
extern void object_notice_slays(struct player *p, struct object *obj,
    const struct monster_race *race);
extern void improve_attack_modifier(struct player *p, struct object *obj, struct actor *who,
    int *best_mult, bool *do_poison, char *verb, size_t len, bool range, bool real);
extern bool react_to_slay(struct object *obj, const struct monster *mon);
extern bool brands_are_equal(struct brand *brand1, struct brand *brand2);
extern bool slays_are_equal(struct slay *slay1, struct slay *slay2);
extern s32b check_slay_cache(const struct object *obj, bool known);
extern void fill_slay_cache(const struct object *obj, bool known, s32b value);
extern void create_slay_cache(void);
extern void free_slay_cache(void);
extern int get_poly_brand(struct monster_race *race, int method);
extern int get_bow_brand(struct bow_brand *brand);

#endif /* OBJECT_SLAYS_H */
