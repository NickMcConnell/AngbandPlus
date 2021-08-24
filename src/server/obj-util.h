/*
 * File: obj-util.h
 * Purpose: Object utilities
 */

#ifndef OBJECT_UTIL_H
#define OBJECT_UTIL_H

/* Maximum number of scroll titles generated */
#define MAX_TITLES 50

/*
 * Per-player artifact states
 */
#define ARTS_NOT_FOUND  0
#define ARTS_GENERATED  1
#define ARTS_FOUND      2
#define ARTS_ABANDONED  3
#define ARTS_SOLD       4
#define ARTS_CREATED    5

/*
 * Drop modes
 */
#define DROP_FADE   1
#define DROP_FORBID 2
#define DROP_SILENT 3

/*
 * Missiles
 */
#define magic_ammo_p(T) \
    (tval_is_ammo(T) && kf_has((T)->kind->kind_flags, KF_AMMO_MAGIC))

extern void flavor_init(void);
extern void object_flags(const struct object *obj, bitflag flags[OF_SIZE]);
extern void object_flags_known(const struct object *obj, bitflag flags[OF_SIZE], bool aware);
extern struct effect *object_effect(const struct object *obj);
extern bool is_unknown(const struct object *obj);
extern bool is_unknown_money(const struct object *obj);
extern int compare_items(struct player *p, const struct object *o1, const struct object *o2);
extern bool obj_can_fail(struct player *p, const struct object *o);
extern int get_use_device_chance(struct player *p, const struct object *obj);
extern void distribute_charges(struct object *source, struct object *dest, int amt);
extern int number_charging(const struct object *obj);
extern bool recharge_timeout(struct object *obj);
extern void get_object_info(struct player *p, struct object *obj, struct object_xtra *info_xtra);
extern int get_owner_id(const struct object *obj);
extern void set_artifact_info(struct player *p, const struct object *obj, byte info);
extern void object_absorb_origin(struct object *obj1, struct object *obj2);
extern bool kind_is_good_other(const struct object_kind *kind);
extern void set_origin(struct object *obj, byte origin, s16b origin_depth, u16b origin_xtra);
extern void shimmer_objects(struct player *p, struct chunk *c);
extern void process_objects(struct chunk *c);
extern bool is_owner(struct player *p, struct object *obj);
extern void object_own(struct player *p, struct object *obj);
extern void preserve_artifact_aux(const struct object *obj);
extern void preserve_artifact(const struct object *obj);
extern bool is_sense_machine(const struct object *obj);
extern bool use_object(struct player *p, struct object *obj, int amount, bool describe);
extern void redraw_floor(int depth, int y, int x);
extern bool object_marked_aware(struct player *p, const struct object *obj);
extern struct object *object_from_index(struct player *p, int item, bool prompt,
    bool check_ignore);

#endif /* OBJECT_UTIL_H */
