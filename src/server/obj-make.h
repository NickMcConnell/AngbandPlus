/*
 * File: obj-make.h
 * Purpose: Object generation functions
 */

#ifndef OBJECT_MAKE_H
#define OBJECT_MAKE_H

/*
 * Define a value for minima which will be ignored (a replacement for 0,
 * because 0 and some small negatives are valid values).
 */
#define NO_MINIMUM  255

extern struct object_kind *get_obj_num(int level, bool good, int tval);
extern void init_powers(const struct object *obj, int *power, int *resist);
extern void dec_power(const struct object *obj, int *power);
extern void dec_resist(const struct object *obj, int *resist);
extern void inc_power(const struct object *obj, int *power, int *resist);
extern void inc_resist(const struct object *obj, int *power, int *resist);
extern void do_fixed_powers(struct object *obj, int power, int resist);
extern void undo_fixed_powers(struct object *obj, int power, int resist);
extern void get_power_descs(int power, int resist, char *buf, int len);
extern void object_prep(struct player *p, struct object *obj, struct object_kind *k, int lev,
    aspect rand_aspect);
extern int apply_magic(struct player *p, struct chunk *c, struct object *obj, int lev,
    bool allow_artifacts, bool good, bool great, bool extra_roll);
extern struct object *make_object(struct player *p, struct chunk *c, int lev, bool good,
    bool great, bool extra_roll, s32b *value, int tval);
extern void acquirement(struct player *p, struct chunk *c, int num, quark_t quark);
extern struct object_kind *money_kind(const char *name, int value);
extern struct object *make_gold(struct player *p, int lev, char *coin_type);
extern void make_randart(struct player *p, struct chunk *c, struct object *obj,
    struct artifact *art, s32b randart_seed);
extern void copy_artifact_data(struct object *obj, const struct artifact *art);
extern bool make_fake_artifact(struct object **obj_address, const struct artifact *artifact);
extern bool create_randart_drop(struct player *p, struct chunk *c, struct object **obj_address,
    int a_idx, bool check);
extern bool kind_is_good(const struct object_kind *kind);
extern void ego_apply_magic(struct object *obj, int level);
extern void fuel_default(struct object *obj);
extern void create_randart(struct player *p, struct chunk *c);
extern void reroll_randart(struct player *p, struct chunk *c);

#endif /* OBJECT_MAKE_H */
