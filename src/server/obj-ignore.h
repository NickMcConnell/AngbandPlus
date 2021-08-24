/*
 * File: obj-ignore.h
 * Purpose: Item ignoring
 */

#ifndef OBJ_IGNORE_H
#define OBJ_IGNORE_H

/*
 * Ignore flags
 */
#define IGNORE_ALLOW    1
#define IGNORE_PROTECT  2

extern bool object_is_ignored(struct player *p, const struct object *obj);
extern bool ignore_item_ok(struct player *p, const struct object *obj);
extern void ignore_drop(struct player *p);
extern byte ignore_level_of(struct player *p, const struct object *obj);

#endif /* OBJ_IGNORE_H */
