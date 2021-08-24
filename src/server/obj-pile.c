/*
 * File: obj-pile.c
 * Purpose: Deal with piles of objects
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2020 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "s-angband.h"


struct pile_integrity_info
{
    struct object *fail_pile;
    struct object *fail_object;
    bool fail_prev;
    bool fail_next;
    const char *fail_msg;
};


static void write_pile(ang_file *fff, void *data)
{
    struct pile_integrity_info *p = (struct pile_integrity_info *)data;
    struct object *current = p->fail_pile;

    file_putf(fff, "Pile integrity failure in %s\n\n", p->fail_msg);
    if (p->fail_object && p->fail_object->kind)
    {
        file_putf(fff, "Guilty object\n=============\n");
        file_putf(fff, "Name: %s\n", p->fail_object->kind->name);
        if (p->fail_prev)
        {
            file_putf(fff, "Previous: ");
            if (p->fail_object->prev && p->fail_object->prev->kind)
                file_putf(fff, "%s\n", p->fail_object->prev->kind->name);
            else
                file_putf(fff, "bad object\n");
        }
        if (p->fail_next)
        {
            file_putf(fff, "Next: ");
            if (p->fail_object->next && p->fail_object->next->kind)
                file_putf(fff, "%s\n", p->fail_object->next->kind->name);
            else
                file_putf(fff, "bad object\n");
        }
        file_putf(fff, "\n");
    }

    if (current)
    {
        file_putf(fff, "Guilty pile\n=============\n");
        while (current)
        {
            if (current->kind)
                file_putf(fff, "Name: %s\n", current->kind->name);
            else
                file_putf(fff, "bad object\n");
            current = current->next;
        }
    }
}


/*
 * Quit on getting an object pile error, writing a diagnosis file
 */
static void pile_integrity_fail(struct object *pile, struct object *obj, const char *msg)
{
    char path[MSG_LEN];
    struct pile_integrity_info data;

    /* Set the pile info to write out */
    data.fail_pile = pile;
    data.fail_object = obj;
    data.fail_prev = (obj->prev != NULL);
    data.fail_next = (obj->next != NULL);
    data.fail_msg = msg;

    /* Write to the user directory */
    path_build(path, sizeof(path), ANGBAND_DIR_USER, "pile_error.txt");

    if (text_lines_to_file(path, write_pile, (void *)&data))
        quit_fmt("Failed to create file %s.new", path);

    quit_fmt("Pile integrity failure, details written to %s", path);
}


/*
 * Check the integrity of a pile - make sure it's not circular and that each
 * entry in the chain has consistent next and prev pointers.
 */
static void pile_check_integrity(const char *op, struct object *pile, struct object *hilight)
{
    struct object *obj = pile;
    struct object *prev = NULL;

    /* Check prev<->next chain */
    while (obj)
    {
        if (obj->prev != prev) pile_integrity_fail(pile, obj, "pile_check_integrity (1)");
        prev = obj;
        obj = obj->next;
    }

    /* Check for circularity */
    for (obj = pile; obj; obj = obj->next)
    {
        struct object *check;

        for (check = obj->next; check; check = check->next)
        {
            if (check->next == obj) pile_integrity_fail(pile, check, "pile_check_integrity (2)");
        }
    }
}


/*
 * Insert 'obj' into the pile 'pile'.
 *
 * 'obj' must not already be in any other lists.
 */
void pile_insert(struct object **pile, struct object *obj)
{
    if (obj->prev || obj->next) pile_integrity_fail(NULL, obj, "pile_insert");

    if (*pile)
    {
        obj->next = *pile;
        (*pile)->prev = obj;
    }

    *pile = obj;

    pile_check_integrity("insert", *pile, obj);
}


/*
 * Insert 'obj' at the end of pile 'pile'.
 *
 * Unlike pile_insert(), obj can be the beginning of a new list of objects.
 */
void pile_insert_end(struct object **pile, struct object *obj)
{
    if (obj->prev) pile_integrity_fail(NULL, obj, "pile_insert_end");

    if (*pile)
    {
        struct object *end = pile_last_item(*pile);

        end->next = obj;
        obj->prev = end;
    }
    else
        *pile = obj;

    pile_check_integrity("insert_end", *pile, obj);
}


/*
 * Remove object 'obj' from pile 'pile'.
 */
void pile_excise(struct object **pile, struct object *obj)
{
    struct object *prev = obj->prev;
    struct object *next = obj->next;

    if (!pile_contains(*pile, obj)) pile_integrity_fail(*pile, obj, "pile_excise (1)");
    pile_check_integrity("excise [pre]", *pile, obj);

    /* Special case - excise top object */
    if (*pile == obj)
    {
        if (prev) pile_integrity_fail(*pile, obj, "pile_excise (2)");

        *pile = next;
    }

    /* Otherwise unlink from the previous */
    else
    {
        if (prev == NULL) pile_integrity_fail(*pile, obj, "pile_excise (3)");

        prev->next = next;
        obj->prev = NULL;
    }

    /* And then unlink from the next */
    if (next)
    {
        next->prev = prev;
        obj->next = NULL;
    }

    pile_check_integrity("excise [post]", *pile, NULL);
}


/*
 * Return the last item in pile 'pile'.
 */
struct object *pile_last_item(struct object *pile)
{
    struct object *obj = pile;

    pile_check_integrity("last_item", pile, NULL);

    /* No pile at all */
    if (!pile) return NULL;

    /* Run along the list, stopping just before the end */
    while (obj->next) obj = obj->next;

    return obj;
}


/*
 * Check if pile 'pile' contains object 'obj'.
 */
bool pile_contains(const struct object *top, const struct object *obj)
{
    const struct object *pile_obj = top;

    while (pile_obj)
    {
        if (obj == pile_obj) return true;
        pile_obj = pile_obj->next;
    }

    return false;
}


/*
 * Create a new object and return it
 */
struct object *object_new(void)
{
    return mem_zalloc(sizeof(struct object));
}


/*
 * Free up an object
 *
 * This doesn't affect any game state outside of the object itself
 */
void object_free(struct object *obj)
{
    mem_free(obj->slays);
    mem_free(obj->brands);
    mem_free(obj->curses);

    mem_free(obj);
}


/*
 * Delete an object and free its memory, and set its pointer to NULL
 */
void object_delete(struct object **obj_address)
{
    struct object *obj = *obj_address;
    struct object *prev = obj->prev;
    struct object *next = obj->next;

    /* Check any next and previous objects */
    if (next)
    {
        if (prev)
        {
            prev->next = next;
            next->prev = prev;
        }
        else
            next->prev = NULL;
    }
    else if (prev)
        prev->next = NULL;

    /* Free known object */
    if (obj->known) object_free(obj->known);

    object_free(obj);
    *obj_address = NULL;
}


/*
 * Free an entire object pile
 */
void object_pile_free(struct object *obj)
{
    struct object *current = obj, *next;

    while (current)
    {
        next = current->next;
        object_delete(&current);
        current = next;
    }
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow weapons/armor to stack, if "known".
 *
 * Missiles will combine if both stacks have the same "known" status.
 * This is done to make unidentified stacks of missiles useful.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, except rods, never stack (for various
 * reasons).
 */
bool object_stackable(struct player *p, const struct object *obj1, const struct object *obj2,
    object_stack_t mode)
{
    int i;

    /* Equipment items don't stack */
    if (p && object_is_equipped(p->body, obj1)) return false;
    if (p && object_is_equipped(p->body, obj2)) return false;

    /* If either item is unknown, do not stack */
    if ((mode & OSTACK_LIST) && object_marked_aware(p, obj1)) return false;
    if ((mode & OSTACK_LIST) && object_marked_aware(p, obj2)) return false;

    /* Hack -- requires same location (object list) */
    if ((mode & OSTACK_LIST) && !loc_eq(&((struct object *)obj1)->grid, &((struct object *)obj2)->grid))
        return false;

    /* Hack -- identical items cannot be stacked */
    if (obj1 == obj2) return false;

    /* Require identical object kinds */
    if (obj1->kind != obj2->kind) return false;

    /* Different flags don't stack */
    if (!of_is_equal(obj1->flags, obj2->flags)) return false;

    /* Different elements don't stack */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (obj1->el_info[i].res_level != obj2->el_info[i].res_level) return false;
    }

    /* Artifacts never stack */
    if (obj1->artifact || obj2->artifact) return false;

    /* Chests never stack */
    if (tval_is_chest(obj1)) return false;

    /* Food, potions, scrolls, staves, wands and rods all stack nicely */
    if (tval_is_edible(obj1) || tval_is_potion(obj1) || tval_is_scroll(obj1) ||
        tval_is_staff(obj1) || tval_is_wand(obj1) || tval_is_rod(obj1))
    {
        /* Assume okay */
    }

    /* Weapons, ammo, armour, jewelry, lights, tools */
    else if (tval_has_variable_power(obj1))
    {
        bool obj1_is_known = object_is_known(p, obj1);
        bool obj2_is_known = object_is_known(p, obj2);

        /* Require identical values */
        if (obj1->ac != obj2->ac) return false;
        if (obj1->dd != obj2->dd) return false;
        if (obj1->ds != obj2->ds) return false;

        /* Require identical bonuses */
        if (obj1->to_h != obj2->to_h) return false;
        if (obj1->to_d != obj2->to_d) return false;
        if (obj1->to_a != obj2->to_a) return false;

        /* Require all identical modifiers */
        for (i = 0; i < OBJ_MOD_MAX; i++)
        {
            if (obj1->modifiers[i] != obj2->modifiers[i]) return false;
        }

        /* Require identical ego-item types */
        if (obj1->ego != obj2->ego) return false;

        /* Require identical curses */
        if (!curses_are_equal(obj1, obj2)) return false;

        /* Hack -- require identical brands (for Elemental ego) */
        if (!brands_are_equal(obj1, obj2)) return false;

        /* Hack -- never stack recharging wearables */
        if ((obj1->timeout || obj2->timeout) && !tval_is_light(obj1))
            return false;

        /* Lights must have same amount of fuel */
        else if ((obj1->timeout != obj2->timeout) && tval_is_light(obj1))
            return false;

        /* Prevent unIDd items stacking with IDd items in the object list */
        if ((mode & OSTACK_LIST) && (obj1_is_known != obj2_is_known))
            return false;
    }

    /* Skeletons */
    else if (tval_is_skeleton(obj1))
    {
        /* Require identical monster type */
        if (obj1->pval != obj2->pval) return false;
    }

    /* Corpses */
    else if (tval_is_corpse(obj1))
    {
        /* Require identical monster type and timeout */
        if (obj1->pval != obj2->pval) return false;
        if (obj1->decay != obj2->decay) return false;
    }

    /* Anything else */
    else
    {
        /* Probably okay */
    }

    /* Require compatible inscriptions */
    if (obj1->note && obj2->note && (obj1->note != obj2->note)) return false;

    /* They must be similar enough */
    return true;
}


/*
 * Return whether each stack of objects can be merged into one stack.
 */
bool object_similar(struct player *p, const struct object *obj1, const struct object *obj2,
    object_stack_t mode)
{
    int total;

    /* Hack -- gold */
    if (tval_is_money(obj1))
    {
        total = obj1->pval + obj2->pval;

        /* Check against stacking limit */
        if (total > PY_MAX_GOLD) return false;
    }
    else
    {
        total = obj1->number + obj2->number;

        /* Check against stacking limit - except in stores which absorb anyway */
        if (!(mode & OSTACK_STORE) && (total > obj1->kind->base->max_stack)) return false;
    }

    return object_stackable(p, obj1, obj2, mode);
}


/*
 * Combine the origins of two objects
 */
void object_origin_combine(struct object *obj1, struct object *obj2)
{
    int act = 2;

    if ((obj1->origin == obj2->origin) && (obj1->origin_depth == obj2->origin_depth) &&
        (obj1->origin_race == obj2->origin_race)) return;

    if (obj1->origin_race && obj2->origin_race)
    {
        bool uniq1 = monster_is_unique(obj1->origin_race);
        bool uniq2 = monster_is_unique(obj2->origin_race);

        if (uniq1 && !uniq2) act = 0;
        else if (uniq2 && !uniq1) act = 1;
        else act = 2;
    }

    switch (act)
    {
        /* Overwrite with obj2 */
        case 1:
        {
            set_origin(obj1, obj2->origin, obj2->origin_depth, obj2->origin_race);
            break;
        }

        /* Set as "mixed" */
        case 2:
        {
            set_origin(obj1, ORIGIN_MIXED, 0, NULL);
            break;
        }
    }
}


static void object_absorb_known(struct object *known_obj1, struct object *known_obj2)
{
    int i;

    /* Merge all known flags */
    of_union(known_obj1->flags, known_obj2->flags);

    /* Merge all known modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (known_obj2->modifiers[i])
            known_obj1->modifiers[i] = 1;
    }

    /* Merge all known elemental properties */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (known_obj2->el_info[i].res_level)
            known_obj1->el_info[i].res_level = 1;
    }

    /* Merge all known brands and slays */
    copy_brands(&known_obj1->brands, known_obj2->brands);
    copy_slays(&known_obj1->slays, known_obj2->slays);

    for (i = 0; known_obj2->curses && (i < z_info->curse_max); i++)
        append_curse(known_obj1, known_obj2, i);

    /* Merge everything else */
    if (known_obj2->artifact) known_obj1->artifact = (struct artifact *)1;
    if (known_obj2->ego) known_obj1->ego = (struct ego_item *)1;
    known_obj1->notice |= known_obj2->notice;
    if (known_obj2->pval) known_obj1->pval = 1;
    if (known_obj2->dd) known_obj1->dd = 1;
    if (known_obj2->ds) known_obj1->ds = 1;
    if (known_obj2->ac) known_obj1->ac = 1;
    if (known_obj2->to_a) known_obj1->to_a = 1;
    if (known_obj2->to_h) known_obj1->to_h = 1;
    if (known_obj2->to_d) known_obj1->to_d = 1;
    if (known_obj2->effect) known_obj1->effect = (struct effect *)1;
}


/*
 * Allow one item to "absorb" another, assuming they are similar.
 *
 * The blending of the "note" field assumes that either (1) one has an
 * inscription and the other does not, or (2) neither has an inscription.
 * In both these cases, we can simply use the existing note, unless the
 * blending object has a note, in which case we use that note.
 *
 * These assumptions are enforced by the "object_similar()" code.
 */
static void object_absorb_merge(struct object *obj1, struct object *obj2)
{
    /* First object gains any extra knowledge from second */
    object_absorb_known(obj1->known, obj2->known);

    /* Merge inscriptions */
    if (obj2->note) obj1->note = obj2->note;

    /* Combine timeouts for rod stacking */
    if (tval_can_have_timeout(obj1)) obj1->timeout += obj2->timeout;

    /* Combine charges for wands and staves */
    if (tval_can_have_charges(obj1))
        obj1->pval += obj2->pval;

    /* Combine origin data as best we can */
    object_origin_combine(obj1, obj2);
}


/*
 * Merge a smaller stack into a larger stack, leaving two uneven stacks.
 */
void object_absorb_partial(struct object *obj1, struct object *obj2)
{
    int smallest = MIN(obj1->number, obj2->number);
    int largest = MAX(obj1->number, obj2->number);
    int difference = obj1->kind->base->max_stack - largest;

    obj1->number = largest + difference;
    obj2->number = smallest - difference;

    object_absorb_merge(obj1, obj2);
}


/*
 * Merge two stacks into one stack.
 */
void object_absorb(struct object *obj1, struct object *obj2)
{
    int total;

    /* Hack -- gold */
    if (tval_is_money(obj1))
    {
        total = obj1->pval + obj2->pval;

        /* Combine the pvals */
        obj1->pval = MIN(total, PY_MAX_GOLD);
    }
    else
    {
        total = obj1->number + obj2->number;

        /* Add together the item counts */
        obj1->number = MIN(total, obj1->kind->base->max_stack);
    }

    object_absorb_merge(obj1, obj2);
    object_delete(&obj2);
}


/*
 * Wipe an object clean.
 */
void object_wipe(struct object *obj)
{
    /* Wipe the structure */
    memset(obj, 0, sizeof(*obj));
}


/*
 * Prepare an object based on an existing object
 */
void object_copy(struct object *dest, const struct object *src)
{
    /* Copy the structure (this includes pointers) */
    memcpy(dest, src, sizeof(struct object));

    /* Reset pointers */
    dest->known = NULL;
    dest->brands = NULL;
    dest->slays = NULL;
    dest->curses = NULL;

    /* Copy pointers from the source object */
    if (src->known)
    {
        dest->known = object_new();
        object_copy(dest->known, src->known);
    }
    if (src->slays)
    {
        size_t array_size = z_info->slay_max * sizeof(bool);

        dest->slays = mem_zalloc(array_size);
        memcpy(dest->slays, src->slays, array_size);
    }
    if (src->brands)
    {
        size_t array_size = z_info->brand_max * sizeof(bool);

        dest->brands = mem_zalloc(array_size);
        memcpy(dest->brands, src->brands, array_size);
    }
    if (src->curses)
    {
        size_t array_size = z_info->curse_max * sizeof(struct curse_data);

        dest->curses = mem_zalloc(array_size);
        memcpy(dest->curses, src->curses, array_size);
    }

    /* Detach from any pile */
    dest->prev = NULL;
    dest->next = NULL;
}


/*
 * Prepare an object `dest` representing `amt` objects, based on an existing
 * object `src` representing at least `amt` objects.
 *
 * Takes care of the charge redistribution concerns of stacked items.
 */
void object_copy_amt(struct object *dest, struct object *src, int amt)
{
    int charge_time = randcalc(src->time, 0, AVERAGE), max_time;

    /* Get a copy of the object */
    object_copy(dest, src);

    /* Modify quantity */
    dest->number = amt;

    /*
     * If the item has charges/timeouts, set them to the correct level
     * too. We split off the same amount as distribute_charges.
     */
    if (tval_can_have_charges(src))
        dest->pval = src->pval * amt / src->number;

    if (tval_can_have_timeout(src))
    {
        max_time = charge_time * amt;

        if (src->timeout > max_time) dest->timeout = max_time;
        else dest->timeout = src->timeout;
    }
}


/*
 * Split off 'amt' items from 'src' and return.
 *
 * Where object_copy_amt() makes `amt` new objects, this function leaves the
 * total number unchanged; otherwise the two functions are similar.
 *
 * This function should only be used when amt < src->number
 */
struct object *object_split(struct object *src, int amt)
{
    struct object *dest = object_new();

    /* Get a copy of the object */
    object_copy(dest, src);

    /* Check legality */
    my_assert(src->number > amt);

    /* Distribute charges of wands, staves, or rods */
    distribute_charges(src, dest, amt);

    /* Modify quantity */
    dest->number = amt;
    src->number -= amt;

    return dest;
}


/*
 * Remove an amount of an object from the floor, returning a detached object
 * which can be used - it is assumed that the object is on the player grid.
 *
 * Optionally describe what remains.
 */
struct object *floor_object_for_use(struct player *p, struct chunk *c, struct object *obj,
    int num, bool message, bool *none_left)
{
    struct object *usable;
    char name[NORMAL_WID];
    struct loc grid;

    /* Save object info (if we use the entire stack) */
    loc_copy(&grid, &obj->grid);

    /* Bounds check */
    num = MIN(num, obj->number);

    /* Split off a usable object if necessary */
    if (obj->number > num)
    {
        usable = object_split(obj, num);

        /* Describe if necessary */
        if (message) object_desc(p, name, sizeof(name), obj, ODESC_PREFIX | ODESC_FULL);
    }

    /* We're using the entire stack */
    else
    {
        /* Describe if necessary */
        if (message)
        {
            /* Describe zero amount */
            obj->number = 0;
            object_desc(p, name, sizeof(name), obj, ODESC_PREFIX | ODESC_FULL);
            obj->number = num;
        }

        /* We're using the entire stack */
        usable = obj;
        square_excise_object(c, &usable->grid, usable);
        *none_left = true;

        /* Stop tracking item */
        if (tracked_object_is(p->upkeep, obj)) track_object(p->upkeep, NULL);
    }

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    redraw_floor(&p->wpos, &grid, NULL);

    /* Print a message if desired */
    if (message)
        msg(p, "You see %s.", name);

    return usable;
}


/*
 * Find and return the oldest object on the given grid marked as "ignore".
 */
static struct object *floor_get_oldest_ignored(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj, *ignore = NULL;

    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        if (p && ignore_item_ok(p, obj)) ignore = obj;
    }

    return ignore;
}


static int grid_to_index(struct chunk *c, struct loc *grid, struct object *obj)
{
    int oidx = obj->oidx;

    square_delete_object(c, grid, obj, false, false);

    /* Use this index */
    c->o_gen[0 - (oidx + 1)] = true;

    return oidx;
}


/*
 * Hack -- obtain an index for a floor object
 */
static int floor_to_index(struct chunk *c)
{
    int i;
    struct object *obj;
    struct loc begin, end;
    struct loc_iterator iter;

    /* Scan the list of generated indices */
    for (i = 0; i < MAX_OBJECTS; i++)
    {
        if (!c->o_gen[i])
        {
            /* Use this index */
            c->o_gen[i] = true;

            return 0 - (i + 1);
        }
    }

    /* List is full, try nuking something to make room */

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* First do crops and junk */
    do
    {
        for (obj = square_object(c, &iter.cur); obj; obj = obj->next)
        {
            /* Nuke crops */
            if (tval_is_crop(obj)) return grid_to_index(c, &iter.cur, obj);

            /* Nuke junk */
            if (tval_is_skeleton(obj) || tval_is_corpse(obj) || tval_is_bottle(obj))
                return grid_to_index(c, &iter.cur, obj);
        }
    }
    while (loc_iterator_next_strict(&iter));

    loc_init(&begin, 0, 0);
    loc_init(&end, c->width, c->height);
    loc_iterator_first(&iter, &begin, &end);

    /* Then do gold */
    do
    {
        /* Hack -- skip gold in houses */
        if ((c->wpos.depth == 0) && square_isvault(c, &iter.cur)) continue;

        for (obj = square_object(c, &iter.cur); obj; obj = obj->next)
        {
            /* Nuke gold */
            if (tval_is_money(obj)) return grid_to_index(c, &iter.cur, obj);
        }
    }
    while (loc_iterator_next_strict(&iter));

    return 0;
}


/*
 * Let the floor carry an object, deleting old ignored items if necessary.
 * The calling function must deal with the dropped object on failure.
 *
 * Optionally put the object at the top or bottom of the pile
 */
bool floor_carry(struct player *p, struct chunk *c, struct loc *grid, struct object *drop,
    bool *note)
{
    int n = 0;
    struct object *obj, *ignore = floor_get_oldest_ignored(p, c, grid);

    /* Fail if the square can't hold objects */
    if (!square_isobjectholding(c, grid)) return false;

    /* Scan objects in that grid for combination */
    for (obj = square_object(c, grid); obj; obj = obj->next)
    {
        /* Check for combination */
        if (object_similar(p, obj, drop, OSTACK_FLOOR))
        {
            /* Combine the items */
            object_absorb(obj, drop);

            /* Note the pile */
            if (square_isview(p, grid)) square_note_spot(c, grid);

            /* Don't mention if ignored */
            if (p && ignore_item_ok(p, obj)) *note = false;

            /* Result */
            return true;
        }

        /* Count objects */
        n++;
    }

    /* The stack is already too large */
    if (n >= z_info->floor_size)
    {
        /* Delete the oldest ignored object */
        if (ignore)
            square_delete_object(c, grid, ignore, false, false);
        else
            return false;
    }

    /* Hack -- set index */
    drop->oidx = floor_to_index(c);
    if (!drop->oidx) return false;

    /* Location */
    loc_copy(&drop->grid, grid);
    memcpy(&drop->wpos, &c->wpos, sizeof(struct worldpos));

    /* Forget monster */
    drop->held_m_idx = 0;

    /* Link to the first object in the pile */
    pile_insert(&square(c, grid)->obj, drop);

    /* Redraw */
    square_note_spot(c, grid);
    square_light_spot(c, grid);

    /* Don't mention if ignored */
    if (p && ignore_item_ok(p, drop)) *note = false;

    /* Result */
    return true;
}


/*
 * Add an object to the floor (from the savefile).
 *
 * This is a simplified version of floor_carry().
 */
bool floor_add(struct chunk *c, struct loc *grid, struct object *drop)
{
    /* Fail if the square can't hold objects */
    if (!square_in_bounds_fully(c, grid)) return false;
    if (!square_isobjectholding(c, grid)) return false;

    /* Hack -- set index */
    drop->oidx = floor_to_index(c);
    if (!drop->oidx) return false;

    /* Location */
    loc_copy(&drop->grid, grid);
    memcpy(&drop->wpos, &c->wpos, sizeof(struct worldpos));

    /* Forget monster */
    drop->held_m_idx = 0;

    /* Link to the last object in the pile */
    pile_insert_end(&square(c, grid)->obj, drop);

    /* Result */
    return true;
}


/*
 * Delete an object when the floor fails to carry it, and attempt to remove
 * it from the object list
 */
static void floor_carry_fail(struct player *p, struct object *drop, bool broke, bool preserve,
    int mode)
{
    char o_name[NORMAL_WID];
    char *verb = (broke? VERB_AGREEMENT(drop->number, "breaks", "break"):
        VERB_AGREEMENT(drop->number, "disappears", "disappear"));

    /* Carry object directly instead (if we can) */
    if (p && (mode == DROP_CARRY) && inven_carry_okay(p, drop) && weight_okay(p, drop))
    {
        assess_object(p, drop, true);
        inven_carry(p, drop, true, false);
        return;
    }

    if (p) object_desc(p, o_name, sizeof(o_name), drop, ODESC_BASE);
    if (p) msg(p, "The %s %s.", o_name, verb);

    /* Hack -- preserve artifacts */
    if (preserve && drop->artifact)
    {
        /* Only works when owner is ingame */
        if (!p) p = player_get(get_owner_id(drop));

        /* Preserve any artifact */
        preserve_artifact_aux(drop);
        if (p) history_lose_artifact(p, drop);
    }

    /* Delete completely */
    object_delete(&drop);
}


/*
 * Find a grid near the given one for an object to fall on
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed. Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 *
 * If prefer_pile is true, does not apply a penalty for putting different types
 * of items in the same grid.
 */
static bool drop_find_grid(struct player *p, struct chunk *c, struct object *drop, int mode,
    bool prefer_pile, struct loc *grid)
{
    int best_score = -1;
    struct loc best;
    int i, dy, dx;
    struct object *obj;

    loc_copy(&best, grid);

    /* Scan local grids */
    for (dy = -3; dy <= 3; dy++)
    {
        for (dx = -3; dx <= 3; dx++)
        {
            bool combine = false;
            int dist = (dy * dy) + (dx * dx);
            struct loc loc_try;
            int num_shown = 0;
            int num_ignored = 0;
            int score;
            struct monster *mon;
            int size;

            loc_init(&loc_try, grid->x + dx, grid->y + dy);

            /* Ignore */
            if ((dist > 10) || !square_in_bounds_fully(c, &loc_try) || !los(c, grid, &loc_try) ||
                !square_isanyfloor(c, &loc_try) || square_isplayertrap(c, &loc_try) ||
                square_trap_flag(c, &loc_try, TRF_GLYPH))
            {
                continue;
            }

            /* Hack -- not where a NEVER_MOVE + NO_DEATH monster stands */
            mon = square_monster(c, &loc_try);
            if (mon && rf_has(mon->race->flags, RF_NEVER_MOVE) &&
                rf_has(mon->race->flags, RF_NO_DEATH))
            {
                continue;
            }

            /* Analyse the grid for carrying the new object */
            for (obj = square_object(c, &loc_try); obj; obj = obj->next)
            {
                /* Check for possible combination */
                if (object_similar(p, obj, drop, OSTACK_FLOOR)) combine = true;

                /* Count objects */
                if (!(p && ignore_item_ok(p, obj))) num_shown++;
                else num_ignored++;
            }
            if (!combine) num_shown++;

            /* Hack: limit size of pile in houses */
            size = z_info->floor_size;
            if (location_in_house(&c->wpos, &loc_try)) size = cfg_house_floor_size;

            /* Disallow if the stack size is too big */
            if (((num_shown + num_ignored) > size) && !floor_get_oldest_ignored(p, c, &loc_try))
                continue;

            /* Score the location based on how close and how full the grid is */
            score = 1000 - (dist + (prefer_pile? 0: num_shown * 5));

            if ((score < best_score) || ((score == best_score) && one_in_(2))) continue;

            best_score = score;
            loc_copy(&best, &loc_try);
        }
    }

    /* Return if we have a score, otherwise fail or try harder for artifacts */
    if (best_score >= 0)
    {
        loc_copy(grid, &best);
        return true;
    }
    if (!drop->artifact)
    {
        floor_carry_fail(p, drop, false, false, mode);
        return false;
    }
    for (i = 0; i < 2000; i++)
    {
        /* Start bouncing from grid to grid, stopping if we find an empty one */
        if (i < 1000)
        {
            best.y = rand_spread(best.y, 1);
            best.x = rand_spread(best.x, 1);
        }

        /* Now go to purely random locations */
        else
        {
            best.y = randint0(c->height);
            best.x = randint0(c->width);
        }

        if (square_canputitem(c, &best))
        {
            loc_copy(grid, &best);
            return true;
        }
    }

    /* Fail */
    floor_carry_fail(p, drop, false, true, mode);
    return false;
}


/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "square_in_bounds_fully()".
 *
 * This function takes a parameter "chance". This is the percentage
 * chance that the item will "disappear" instead of drop. If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * This function will produce a description of a drop event under the player
 * when "verbose" is true.
 *
 * If "prefer_pile" is true, the penalty for putting different types of items
 * in the same square is not applied.
 */
void drop_near(struct player *p, struct chunk *c, struct object **dropped, int chance,
    struct loc *grid, bool verbose, int mode, bool prefer_pile)
{
    char o_name[NORMAL_WID];
    struct loc best;
    bool in_house = false, no_drop = false;
    struct player *q = NULL;
    bool dont_ignore = false;

    loc_copy(&best, grid);

    /* Describe object */
    if (p) object_desc(p, o_name, sizeof(o_name), *dropped, ODESC_BASE);

    /* Handle normal breakage */
    if (!(*dropped)->artifact && (chance > 0) && magik(chance))
    {
        floor_carry_fail(p, *dropped, true, false, DROP_FADE);
        return;
    }

    /* Find the best grid and drop the item, destroying if there's no space */
    if (!drop_find_grid(p, c, *dropped, mode, prefer_pile, &best)) return;

    /* Check houses */
    if (true_artifact_p(*dropped) || tval_can_have_timeout(*dropped) || tval_is_light(*dropped))
        in_house = location_in_house(&c->wpos, &best);

    /* Process true artifacts */
    if (true_artifact_p(*dropped))
    {
        /* True artifacts cannot be dropped in houses... */
        if (in_house)
        {
            /* ...except the Crown and Grond which are not "unique" artifacts */
            if (!kf_has((*dropped)->kind->kind_flags, KF_QUEST_ART))
                no_drop = true;
        }

        /* True artifacts cannot be dropped/thrown in the wilderness */
        else if (in_wild(&c->wpos)) no_drop = true;

        /* True artifacts cannot be dropped/thrown on special levels */
        else if (special_level(&c->wpos)) no_drop = true;

        if (no_drop)
        {
            switch (mode)
            {
                /* Make true artifact vanish */
                case DROP_FADE:
                case DROP_CARRY:
                {
                    if (p) msg(p, "The %s fades into the air!", o_name);

                    /* Preserve any true artifact */
                    preserve_artifact_aux(*dropped);
                    if (p) history_lose_artifact(p, *dropped);

                    object_delete(dropped);
                    break;
                }

                /* Since the object has already been excised, we carry it again */
                case DROP_FORBID:
                {
                    if (p) msg(p, "You cannot drop this here.");
                    if (p) inven_carry(p, *dropped, true, true);
                    break;
                }

                /* Since the object has already been excised, we silently carry it again */
                case DROP_SILENT:
                {
                    if (p) inven_carry(p, *dropped, true, false);
                    break;
                }
            }

            return;
        }
    }

    /* Recharge rods dropped in houses instantly */
    if (tval_can_have_timeout(*dropped) && in_house) (*dropped)->timeout = 0;

    /* Refuel lights dropped in houses to the standard amount */
    if (tval_is_light(*dropped) && in_house) fuel_default(*dropped);

    if (square(c, &best)->mon < 0)
    {
        q = player_get(0 - square(c, &best)->mon);

        /* Check the item still exists and isn't ignored */
        dont_ignore = (verbose && !ignore_item_ok(q, *dropped));
    }

    if (floor_carry(p, c, &best, *dropped, &dont_ignore))
    {
        /* Sound */
        if (p) sound(p, MSG_DROP);

        /* Message when an object falls under a player */
        if (dont_ignore) msg(q, "You feel something roll beneath your feet.");

        /* Redraw */
        if (q) player_know_floor(q, c);
    }
    else
        floor_carry_fail(p, *dropped, false, true, mode);
}


/*
 * This will push objects off a square.
 *
 * The methodology is to load all objects on the square into a queue. Replace
 * the previous square with a type that does not allow for objects. Drop the
 * objects. Last, put the square back to its original type.
 */
void push_object(struct player *p, struct chunk *c, struct loc *grid)
{
    int feat_old;
    struct object *obj;
    struct queue *queue = q_new(z_info->floor_size);
    bool rune = square_iswarded(c, grid), glyph = square_isdecoyed(c, grid);
    struct monster *mon = square_monster(c, grid);

    /* XXX */
    if (mon && mon->mimicked_obj) become_aware(p, c, mon);
    obj = square_object(c, grid);
    if (!obj) return;

    /* Save the original terrain feature */
    feat_old = square(c, grid)->feat;

    /* Push all objects on the square, stripped of pile info, into the queue */
    while (obj)
    {
        struct object *next = obj->next;

        q_push_ptr(queue, obj);

        /* Orphan the object */
        square_excise_object(c, grid, obj);

        /* Next object */
        obj = next;
    }

    /* Set feature to an open door */
    square_open_door(c, grid);

    /* Drop objects back onto the floor */
    while (q_len(queue) > 0)
    {
        /* Take object from the queue */
        obj = q_pop_ptr(queue);

        /* Drop the object */
        drop_near(p, c, &obj, 0, grid, false, DROP_FADE, false);
    }

    /* Reset cave feature and glyph if needed */
    square_set_feat(c, grid, feat_old);
    if (rune) square_add_glyph(c, grid, GLYPH_WARDING);
    else if (glyph) square_add_glyph(c, grid, GLYPH_DECOY);

    q_free(queue);
}


/*
 * Get a list of the objects at the player's location.
 *
 * Return the number of objects acquired.
 */
int scan_floor(struct player *p, struct chunk *c, struct object **items, int max_size,
    object_floor_t mode, item_tester tester)
{
    struct object *obj;
    int num = 0;
    bool unknown;

    /* Sanity */
    if (!square_in_bounds(c, &p->grid)) return 0;

    /* Sensed or known */
    if (mode & OFLOOR_SENSE) obj = square_known_pile(p, c, &p->grid);
    else obj = square_object(c, &p->grid);

    /* Skip empty squares */
    if (!obj) return 0;

    unknown = is_unknown(square_known_pile(p, c, &p->grid));

    /* Scan all objects in the grid */
    for ( ; obj; obj = obj->next)
    {
        /* Enforce limit */
        if (num >= max_size) break;

        /* Item tester */
        if ((mode & OFLOOR_TEST) && !object_test(p, tester, obj)) continue;

        /* Visible */
        if ((mode & OFLOOR_VISIBLE) && !unknown && ignore_item_ok(p, obj)) continue;

        /* Accept this item */
        items[num++] = obj;

        /* Only one */
        if (mode & OFLOOR_TOP) break;
    }

    return num;
}


/*
 * Get a list of the known objects at the given location.
 *
 * Return the number of objects acquired.
 */
int scan_distant_floor(struct player *p, struct chunk *c, struct object **items, int max_size,
    struct loc *grid)
{
    struct object *obj;
    int num = 0;

    /* Sanity */
    if (!square_in_bounds(c, grid)) return 0;

    /* Scan all objects in the grid */
    for (obj = square_known_pile(p, c, grid); obj; obj = obj->next)
    {
        /* Enforce limit */
        if (num >= max_size) break;

        /* Visible */
        if (!is_unknown(obj) && ignore_item_ok(p, obj)) continue;

        /* Accept this item */
        items[num++] = obj;
    }

    return num;
}


/*
 * Update the player's knowledge of the objects on walkover
 */
void player_know_floor(struct player *p, struct chunk *c)
{
    if (square(c, &p->grid)->obj)
    {
        struct object *obj;

        square_know_pile(p, c, &p->grid);

        /* Know every object, recognise artifacts */
        for (obj = square_object(c, &p->grid); obj; obj = obj->next)
        {
            if (!ignore_item_ok(p, obj)) assess_object(p, obj, false);
        }

        redraw_floor(&p->wpos, &p->grid, NULL);
    }
}

