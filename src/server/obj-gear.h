/*
 * File: obj-gear.h
 * Purpose: Management of inventory, equipment and quiver
 */


#ifndef OBJECT_GEAR_H
#define OBJECT_GEAR_H

extern bool slot_type_is(struct player *p, int slot, int type);
extern bool object_is_carried(struct player *p, const struct object *obj);
extern s16b wield_slot(struct player *p, const struct object *obj);
extern bool minus_ac(struct player *p);
extern void gear_excise_object(struct player *p, struct object *obj);
extern struct object *gear_last_item(struct player *p);
extern void gear_insert_end(struct player *p, struct object *obj);
extern struct object *gear_object_for_use(struct player *p, struct object *obj, int num,
    bool message, bool *none_left);
extern int inven_carry_num(struct player *p, struct object *obj, bool stack);
extern bool inven_drop_okay(struct player *p, struct object *obj);
extern bool inven_carry_okay(struct player *p, struct object *obj);
extern void inven_item_charges(struct player *p, struct object *obj);
extern void inven_carry(struct player *p, struct object *obj, bool absorb, bool message);
extern void inven_wield(struct player *p, struct object *obj, int slot);
extern void inven_takeoff(struct player *p, struct object *obj);
extern bool inven_drop(struct player *p, struct object *obj, int amt, bool bypass_inscr);
extern void combine_pack(struct player *p);
extern void pack_overflow(struct player *p, struct chunk *c, struct object *obj);
extern bool item_tester_hook_wear(struct player *p, const struct object *obj);

#endif /* OBJECT_GEAR_H */
