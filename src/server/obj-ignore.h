/*
 * File: obj-ignore.h
 * Purpose: Item ignoring
 */

#ifndef OBJ_IGNORE_H
#define OBJ_IGNORE_H

extern void assess_object(struct player *p, struct object *obj, bool tocarry);
extern bool object_is_ignored(struct player *p, const struct object *obj);
extern bool ignore_item_ok(struct player *p, const struct object *obj);
extern void ignore_drop(struct player *p);
extern byte ignore_level_of(struct player *p, const struct object *obj);
extern const char *get_autoinscription(struct player *p, struct object_kind *kind);
extern int apply_autoinscription(struct player *p, struct object *obj);
extern int remove_autoinscription(struct player *p, s16b kind);
extern int add_autoinscription(struct player *p, s16b kind, const char *inscription);

#endif /* OBJ_IGNORE_H */
