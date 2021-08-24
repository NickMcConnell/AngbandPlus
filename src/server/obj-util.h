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
#define DROP_CARRY  4

extern void flavor_init(void);
extern void object_flags_aux(const struct object *obj, bitflag flags[OF_SIZE]);
extern void object_flags(const struct object *obj, bitflag flags[OF_SIZE]);
extern void object_flags_known(const struct object *obj, bitflag flags[OF_SIZE], bool aware);
extern void object_modifiers(const struct object *obj, s32b modifiers[OBJ_MOD_MAX]);
extern void object_to_h(const struct object *obj, s16b *to_h);
extern void object_to_d(const struct object *obj, s16b *to_d);
extern void object_to_a(const struct object *obj, s16b *to_a);
extern void object_elements(const struct object *obj, struct element_info el_info[ELEM_MAX]);
extern struct effect *object_effect(const struct object *obj);
extern bool is_unknown(const struct object *obj);
extern bool is_unknown_money(const struct object *obj);
extern int compare_items(struct player *p, const struct object *o1, const struct object *o2);
extern bool obj_can_fail(struct player *p, const struct object *o);
extern int get_use_device_chance(struct player *p, const struct object *obj);
extern void distribute_charges(struct object *source, struct object *dest, int amt);
extern int number_charging(const struct object *obj);
extern bool recharge_timeout(struct object *obj);
extern bool obj_can_takeoff(const struct object *obj);
extern int obj_needs_aim(struct player *p, const struct object *obj);
extern void get_object_info(struct player *p, struct object *obj, byte equipped,
    struct object_xtra *info_xtra);
extern int get_owner_id(const struct object *obj);
extern void set_artifact_info(struct player *p, const struct object *obj, byte info);
extern bool kind_is_good_other(const struct object_kind *kind);
extern void set_origin(struct object *obj, byte origin, s16b origin_depth,
    struct monster_race *origin_race);
extern void shimmer_objects(struct player *p, struct chunk *c);
extern void process_objects(struct chunk *c);
extern bool is_owner(struct player *p, struct object *obj);
extern bool has_level_req(struct player *p, struct object *obj);
extern void object_audit(struct player *p, struct object *obj);
extern void object_own(struct player *p, struct object *obj);
extern void preserve_artifact_aux(const struct object *obj);
extern void preserve_artifact(const struct object *obj);
extern bool use_object(struct player *p, struct object *obj, int amount, bool describe);
extern void redraw_floor(struct worldpos *wpos, struct loc *grid, struct object *obj);
extern bool object_marked_aware(struct player *p, const struct object *obj);
extern struct object *object_from_index(struct player *p, int item, bool prompt,
    bool check_ignore);
extern struct ego_item *lookup_ego_item(const char *name, struct object_kind *kind);
extern struct artifact *lookup_artifact_name(const char *name);
extern void print_custom_message(struct player *p, struct object *obj, const char *string,
    int msg_type);

#endif /* OBJECT_UTIL_H */
