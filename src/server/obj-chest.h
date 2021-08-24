/*
 * File: obj-chest.h
 * Purpose: Encapsulation of chest-related functions
 */

#ifndef OBJECT_CHEST_H
#define OBJECT_CHEST_H

/*
 * Chest check types
 */
enum chest_query
{
    CHEST_ANY,
    CHEST_OPENABLE,
    CHEST_TRAPPED
};

extern struct file_parser chest_trap_parser;

extern char *chest_trap_name(const struct object *obj);
extern bool is_trapped_chest(const struct object *obj);
extern bool is_locked_chest(const struct object *obj);
extern int pick_chest_traps(struct object *obj);
extern void unlock_chest(struct object *obj);
extern struct object *chest_check(struct player *p, struct chunk *c, struct loc *grid,
    enum chest_query check_type);
extern int count_chests(struct player *p, struct chunk *c, struct loc *grid,
    enum chest_query check_type);
extern bool do_cmd_open_chest(struct player *p, struct chunk *c, struct loc *grid,
    struct object *obj);
extern bool do_cmd_disarm_chest(struct player *p, struct chunk *c, struct loc *grid,
    struct object *obj);

#endif /* OBJECT_CHEST_H */
