/*
 * File: obj-chest.h
 * Purpose: Encapsulation of chest-related functions
 */

#ifndef OBJECT_CHEST_H
#define OBJECT_CHEST_H

/*
 * Chest trap flags (see "obj-chest.c")
 */
#define CHEST_LOSE_STR      0x01
#define CHEST_LOSE_CON      0x02
#define CHEST_POISON        0x04
#define CHEST_PARALYZE      0x08
#define CHEST_EXPLODE       0x10
#define CHEST_SUMMON        0x20

/*
 * Chest check types
 */
enum chest_query
{
    CHEST_ANY,
    CHEST_OPENABLE,
    CHEST_TRAPPED
};

extern byte chest_trap_type(const struct object *obj);
extern bool is_trapped_chest(const struct object *obj);
extern bool is_locked_chest(const struct object *obj);
extern void unlock_chest(struct object *obj);
extern struct object *chest_check(struct player *p, struct chunk *c, int y, int x,
    enum chest_query check_type);
extern int count_chests(struct player *p, struct chunk *c, int *y, int *x,
    enum chest_query check_type);
extern bool do_cmd_open_chest(struct player *p, struct chunk *c, int y, int x,
    struct object *obj);
extern bool do_cmd_disarm_chest(struct player *p, struct chunk *c, int y, int x,
    struct object *obj);

#endif /* OBJECT_CHEST_H */
