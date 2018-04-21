#ifndef INCLUDED_PACK_H
#define INCLUDED_PACK_H

#include "inv.h"

#define PACK_MAX 26

extern void    pack_init(void);

/* User Interface and Display */
extern void    pack_ui(void);
extern void    pack_display(doc_ptr doc, obj_p p, int flags);

/* Adding and Removing Items */
extern bool pack_get_floor(void);
    extern void pack_get(obj_ptr obj);
        extern void pack_carry(obj_ptr obj);
            extern void pack_carry_aux(obj_ptr obj);
            extern void pack_remove(slot_t slot);
/* pack_get_floor gets all items on the floor, potentially prompting the
 *   user for a selection. This is used by the 'g'et command, as well
 *   as movement when XXXX is enabled.
 *
 * pack_get(obj) gets a single item. This helper function is called by
 *   pack_get_floor for each obj to add, as well as by the autopicker.
 *   Also, some spells, like Whip Fetch, directly move objects from the
 *   floor to the pack and use this function. obj will be released. This
 *   version must be used in preference to pack_carry whenever obj->loc.where
 *   is INV_EQUIP, INV_QUIVER, INV_FLOOR. This version properly handles
 *   checking for quest completion and then calls pack_carry(obj).
 *
 * pack_carry(obj) is the next layer down and actually adds obj, but it
 *   might go to the quiver instead. obj may stack (multiple times) or
 *   it may go to a new slot. Stop worrying about which slot gets it. Also,
 *   if the pack is full, obj gets pushed to the overflow stack which will
 *   be handled later by pack_overflow(). Don't worry about checking whether
 *   the pack is full or not: Just let overflow handle things. obj is not
 *   released!
 *   pack_carry(obj) is called directly during player birth and by several
 *   buildings to directly give the player objects (obj can be a stack
 *   variable here). Of course, pack_carry is the servant of pack_get(obj).
 *
 * pack_remove(obj) is a helper fn which you should never call. It is used
 *   by obj_release.
 */
extern void    pack_drop(obj_ptr obj);
extern void    pack_describe(obj_ptr obj);

/* Accessing, Iterating, Searching */
extern obj_ptr pack_obj(slot_t slot);
extern int     pack_max(void); /* for (slot = 1; slot <= pack_max(); slot++) ... */

extern inv_ptr pack_filter(obj_p p);
extern void    pack_for_each(obj_f f);
extern void    pack_for_each_that(obj_f f, obj_p p);
extern slot_t  pack_find_first(obj_p p);
extern slot_t  pack_find_next(obj_p p, slot_t prev_match);
extern slot_t  pack_find_art(int which);
extern slot_t  pack_find_ego(int which);
extern slot_t  pack_find_obj(int tval, int sval);
extern slot_t  pack_find_device(int effect);
extern slot_t  pack_random_slot(obj_p p);

/* Bonuses: A few rare items grant bonuses from the pack. */
extern void    pack_calc_bonuses(void);

/* Overflow
extern void    pack_push_overflow(obj_ptr obj);*/
extern bool    pack_overflow(void);
extern int     pack_overflow_count(void);

/* Optimize: Combine, Sort, Cleanup Garbage
 * Pack slots are stable until pack_optimize, making looping safe.
 * Normally, notice_stuff() should not be called in the middle of a loop,
 * but you can lock() the pack just to be safe. Be sure to unlock(). */
extern void    pack_lock(void);
extern bool    pack_optimize(void);
extern void    pack_delayed_describe(void);
extern void    pack_unlock(void);

/* Properties of the Entire Inventory */
extern int     pack_weight(obj_p p);
extern int     pack_count(obj_p p);
extern int     pack_count_slots(obj_p p);
extern bool    pack_is_full(void);

/* Savefiles */
extern void    pack_load(savefile_ptr file);
extern void    pack_save(savefile_ptr file);
#endif
