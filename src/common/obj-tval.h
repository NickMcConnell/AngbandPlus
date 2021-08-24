/*
 * File: obj-tval.h
 * Purpose: Wrapper functions for tvals.
 */

#ifndef OBJECT_TVAL_H
#define OBJECT_TVAL_H


/*
 * The values for the "tval" field of various objects.
 *
 * This value is the primary means by which items are sorted in the
 * player inventory, followed by "sval" and "cost".
 */
enum
{
    #define TV(a, b) TV_##a,
    #include "list-tvals.h"
    #undef TV
    TV_MAX
};

/*
 * Special "sval" value -- unknown "sval"
 */
#define SV_UNKNOWN  0

extern bool tval_is_staff(const struct object *obj);
extern bool tval_is_wand(const struct object *obj);
extern bool tval_is_rod(const struct object *obj);
extern bool tval_is_potion(const struct object *obj);
extern bool tval_is_scroll(const struct object *obj);
extern bool tval_is_scroll_k(const struct object_kind *kind);
extern bool tval_is_edible(const struct object *obj);
extern bool tval_is_food_k(const struct object_kind *kind);
extern bool tval_is_mushroom(const struct object *obj);
extern bool tval_is_light(const struct object *obj);
extern bool tval_is_light_k(const struct object_kind *kind);
extern bool tval_is_ring(const struct object *obj);
extern bool tval_is_ring_k(const struct object_kind *kind);
extern bool tval_is_amulet(const struct object *obj);
extern bool tval_is_chest(const struct object *obj);
extern bool tval_is_chest_k(const struct object_kind *kind);
extern bool tval_is_fuel(const struct object *obj);
extern bool tval_is_money(const struct object *obj);
extern bool tval_is_money_k(const struct object_kind *kind);
extern bool tval_can_have_nourishment(const struct object *obj);
extern bool tval_can_have_charges(const struct object *obj);
extern bool tval_can_have_timeout(const struct object *obj);
extern bool tval_is_body_armor(const struct object *obj);
extern bool tval_is_head_armor(const struct object *obj);
extern bool tval_is_ammo(const struct object *obj);
extern bool tval_is_launcher(const struct object *obj);
extern bool tval_is_useable(const struct object *obj);
extern bool tval_can_have_failure(const struct object *obj);
extern bool tval_is_jewelry(const struct object *obj);
extern bool tval_is_enchantable_weapon(const struct object *obj);
extern bool tval_is_weapon(const struct object *obj);
extern bool tval_is_armor(const struct object *obj);
extern bool tval_is_melee_weapon(const struct object *obj);
extern bool tval_has_variable_power(const struct object *obj);
extern bool tval_is_wearable(const struct object *obj);
extern bool tval_can_have_flavor(const struct object *obj);
extern bool tval_is_book_k(const struct object_kind *kind);
extern bool tval_is_book(const struct object *obj);
extern bool tval_is_zapper(const struct object *obj);
extern int tval_find_idx(const char *name);
extern const char *tval_find_name(int tval);
extern int tval_sval_count(const char *name);
extern int tval_sval_list(const char *name, int *list, int max_size);

extern bool tval_can_be_fired(const struct object *shooter, const struct object *ammo);
extern bool tval_is_bottle(const struct object *obj);
extern bool tval_is_arrow(const struct object *obj);
extern bool tval_is_bolt(const struct object *obj);
extern bool tval_is_deed(const struct object *obj);
extern bool tval_is_stone(const struct object *obj);
extern bool tval_is_corpse(const struct object *obj);
extern bool tval_is_skeleton(const struct object *obj);
extern bool tval_is_horn(const struct object *obj);
extern bool tval_is_digger(const struct object *obj);
extern bool tval_is_tool(const struct object *obj);
extern bool tval_is_mstaff(const struct object *obj);
extern bool tval_is_dark_sword(const struct object *obj);
extern bool tval_is_crop(const struct object *obj);
extern int tval_wielding_cut(const struct object *obj);

#endif /* OBJECT_TVAL_H */
