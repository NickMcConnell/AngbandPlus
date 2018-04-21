#ifndef INCLUDED_QUIVER_H
#define INCLUDED_QUIVER_H

#include "inv.h"

#define QUIVER_MAX 26

extern void    quiver_init(void);

extern void    quiver_display(doc_ptr doc, obj_p p, int flags);

/* Adding and removing: Quivers allow a large number of slots
 * (QUIVER_MAX) but restrict the number arrows, etc. The capacity 
 * of the quiver may change as the user finds new and better 
 * quivers in the dungeon. Note: We rely on the cooperation of
 * other code to ensure that the user has equipped a quiver. */
extern bool    quiver_likes(obj_ptr obj);
extern bool    quiver_tolerates(obj_ptr obj);
extern int     quiver_capacity(void);
extern void    quiver_carry(obj_ptr obj); /* combines quiver, then carries pack, then overflows */
extern void    quiver_remove(slot_t slot);
extern void    quiver_remove_all(void); /* player lost quiver due to shapeshifting ... */
extern void    quiver_drop(obj_ptr obj);

/* Accessing, Iterating, Searching */
extern obj_ptr quiver_obj(slot_t slot);
extern int     quiver_max(void); /* for (slot = 1; slot <= quiver_max(); slot++) ... */

extern inv_ptr quiver_filter(obj_p p);
extern void    quiver_for_each(obj_f f);
extern void    quiver_for_each_that(obj_f f, obj_p p);
extern slot_t  quiver_find_first(obj_p p);
extern slot_t  quiver_find_next(obj_p p, slot_t prev_match);
extern slot_t  quiver_find_art(int which);
extern slot_t  quiver_find_ego(int which);
extern slot_t  quiver_find_obj(int tval, int sval);
extern slot_t  quiver_random_slot(obj_p p);

/* The quiver will 'optimize' upon request, combining objects via
 * stacking and resorting. See PN_REORDER and PN_COMBINE, which
 * I've combined into a single method since it is unclear why 
 * they need to be separate. */
extern bool    quiver_optimize(void);
extern void    quiver_delayed_describe(void);

/* Properties of the Entire Inventory */
extern int     quiver_weight(obj_p p);
extern int     quiver_count(obj_p p);
extern int     quiver_count_slots(obj_p p);

/* Savefiles */
extern void    quiver_load(savefile_ptr file);
extern void    quiver_save(savefile_ptr file);
#endif
