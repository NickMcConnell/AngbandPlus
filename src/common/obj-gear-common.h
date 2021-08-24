/*
 * File: obj-gear-common.h
 * Purpose: Management of inventory, equipment and quiver
 */


#ifndef OBJECT_GEAR_COMMON_H
#define OBJECT_GEAR_COMMON_H

/*
 * Player equipment slot types
 */
enum
{
    #define EQUIP(a, b, c, d, e, f) EQUIP_##a,
    #include "list-equip-slots.h"
    #undef EQUIP
    EQUIP_MAX
};

extern int slot_by_name(struct player *p, const char *name);
extern struct object *slot_object(struct player *p, int slot);
extern struct object *equipped_item_by_slot_name(struct player *p, const char *name);
extern bool object_is_equipped(struct player_body body, const struct object *obj);
extern const char *equip_mention(struct player *p, int slot);
extern const char *equip_describe(struct player *p, int slot);
extern char gear_to_label(struct player *p, struct object *obj);
extern int equipped_item_slot(struct player_body body, const struct object *item);
extern bool obj_can_wear(struct player *p, const struct object *obj);

#endif /* OBJECT_GEAR_COMMON_H */
