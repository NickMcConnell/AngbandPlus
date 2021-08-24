/*
 * File: obj-pile.h
 * Purpose: Deal with piles of objects
 */

#ifndef OBJECT_PILE_H
#define OBJECT_PILE_H

/*
 * Modes for stacking by object_similar()
 */
typedef enum
{
    OSTACK_NONE    = 0x00,  /* No options (this does NOT mean no stacking) */
    OSTACK_STORE   = 0x01,  /* Store stacking */
    OSTACK_PACK    = 0x02,  /* Inventory and home */
    OSTACK_LIST    = 0x04,  /* Object list */
    OSTACK_MONSTER = 0x08,  /* Monster carrying objects */
    OSTACK_FLOOR   = 0x10,  /* Floor stacking */
    OSTACK_QUIVER  = 0x20   /* Quiver */
} object_stack_t;

/*
 * Modes for floor scanning by scan_floor()
 */
typedef enum
{
    OFLOOR_NONE    = 0x00,  /* No options */
    OFLOOR_TEST    = 0x01,  /* Verify item tester */
    OFLOOR_SENSE   = 0x02,  /* Sensed or known items only */
    OFLOOR_TOP     = 0x04,  /* Only the top item */
    OFLOOR_VISIBLE = 0x08   /* Visible items only */
} object_floor_t;

extern void pile_insert(struct object **pile, struct object *obj);
extern void pile_insert_end(struct object **pile, struct object *obj);
extern void pile_excise(struct object **pile, struct object *obj);
extern struct object *pile_last_item(struct object *pile);
extern bool pile_contains(const struct object *top, const struct object *obj);
extern struct object *object_new(void);
extern void object_free(struct object *obj);
extern void object_delete(struct object **obj_address);
extern void object_pile_free(struct object *obj);
extern bool object_stackable(struct player *p, const struct object *obj1,
    const struct object *obj2, object_stack_t mode);
extern bool object_similar(struct player *p, const struct object *obj1,
    const struct object *obj2, object_stack_t mode);
extern void object_origin_combine(struct object *obj1, struct object *obj2);
extern void object_absorb_partial(struct object *obj1, struct object *obj2);
extern void object_absorb(struct object *obj1, struct object *obj2);
extern void object_wipe(struct object *obj);
extern void object_copy(struct object *dest, const struct object *src);
extern void object_copy_amt(struct object *dest, struct object *src, int amt);
extern struct object *object_split(struct object *src, int amt);
extern struct object *floor_object_for_use(struct player *p, struct chunk *c,
    struct object *obj, int num, bool message, bool *none_left);
extern bool floor_carry(struct player *p, struct chunk *c, struct loc *grid, struct object *drop,
    bool *note);
extern bool floor_add(struct chunk *c, struct loc *grid, struct object *drop);
extern void drop_near(struct player *p, struct chunk *c, struct object **dropped, int chance,
    struct loc *grid, bool verbose, int mode);
extern void push_object(struct player *p, struct chunk *c, struct loc *grid);
extern int scan_floor(struct player *p, struct chunk *c, struct object **items, int max_size,
    object_floor_t mode, item_tester tester);
extern int scan_distant_floor(struct player *p, struct chunk *c, struct object **items,
    int max_size, struct loc *grid);
extern void player_know_floor(struct player *p, struct chunk *c);

#endif /* OBJECT_PILE_H */
