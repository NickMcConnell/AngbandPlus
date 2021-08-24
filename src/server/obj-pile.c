/*
 * File: obj-pile.c
 * Purpose: Deal with piles of objects
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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
        my_assert(obj->prev == prev);
        prev = obj;
        obj = obj->next;
    }

    /* Check for circularity */
    for (obj = pile; obj; obj = obj->next)
    {
        struct object *check;

        for (check = obj->next; check; check = check->next) {my_assert(check->next != obj);}
    }
}


/*
 * Insert 'obj' into the pile 'pile'.
 *
 * 'obj' must not already be in any other lists.
 */
void pile_insert(struct object **pile, struct object *obj)
{
    my_assert(obj->prev == NULL);
    my_assert(obj->next == NULL);

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
    my_assert(obj->prev == NULL);

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

    my_assert(pile_contains(*pile, obj));
    pile_check_integrity("excise [pre]", *pile, obj);

    /* Special case - excise top object */
    if (*pile == obj)
    {
        my_assert(prev == NULL);

        *pile = next;
    }

    /* Otherwise unlink from the previous */
    else
    {
        my_assert(prev != NULL);

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
    if (obj->known)
    {
        free_brand(obj->known->brands);
        free_slay(obj->known->slays);
        mem_free(obj->known);
    }

    /* Free slays and brands */
    free_brand(obj->brands);
    free_slay(obj->slays);

    mem_free(obj);
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
    if ((mode & OSTACK_LIST) && ((obj1->iy != obj2->iy) || (obj1->ix != obj2->ix))) return false;

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

        /* Hack -- require identical brands (for EGO_ELEMENTAL) */
        if (!brands_are_equal(obj1->brands, obj2->brands)) return false;

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
    int total = obj1->number + obj2->number;

    /* Check against stacking limit - except in stores which absorb anyway */
    if (!(mode & OSTACK_STORE) && (total > z_info->stack_size)) return false;

    return object_stackable(p, obj1, obj2, mode);
}


static void object_absorb_known(struct object *known_obj1, struct object *known_obj2)
{
    int i;
    struct brand *b;
    struct slay *s;

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
    for (b = known_obj2->brands; b; b = b->next)
        append_fixed_brand(&known_obj1->brands, b->element, b->multiplier);
    for (s = known_obj2->slays; s; s = s->next)
        append_fixed_slay(&known_obj1->slays, s->name, s->race_flag, s->multiplier);

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

    /* Hack -- blend "origins" */
    object_absorb_origin(obj1, obj2);
}


/*
 * Merge a smaller stack into a larger stack, leaving two uneven stacks.
 */
void object_absorb_partial(struct object *obj1, struct object *obj2)
{
    int smallest = MIN(obj1->number, obj2->number);
    int largest = MAX(obj1->number, obj2->number);
    int difference = z_info->stack_size - largest;

    obj1->number = largest + difference;
    obj2->number = smallest - difference;

    object_absorb_merge(obj1, obj2);
}


/*
 * Merge two stacks into one stack.
 */
void object_absorb(struct object *obj1, struct object *obj2)
{
    int total = obj1->number + obj2->number;

    /* Add together the item counts */
    obj1->number = ((total <= z_info->stack_size)? total: z_info->stack_size);

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

    /* Copy pointers from the source object */
    if (src->known)
    {
        dest->known = object_new();
        object_copy(dest->known, src->known);
    }
    copy_brand(&dest->brands, src->brands);
    copy_slay(&dest->slays, src->slays);

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
    int depth, y, x;

    /* Save object info (if we use the entire stack) */
    depth = obj->depth;
    y = obj->iy;
    x = obj->ix;

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
        square_excise_object(c, usable->iy, usable->ix, usable);
        *none_left = true;

        /* Stop tracking item */
        if (tracked_object_is(p->upkeep, obj)) track_object(p->upkeep, NULL);
    }

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    redraw_floor(depth, y, x);

    /* Print a message if desired */
    if (message)
        msg(p, "You see %s.", name);

    return usable;
}


/*
 * Find and return the oldest object on the given grid marked as "ignore".
 */
static struct object *floor_get_oldest_ignored(struct player *p, struct chunk *c, int y, int x)
{
    struct object *obj, *ignore = NULL;

    for (obj = square_object(c, y, x); obj; obj = obj->next)
    {
        if (p && ignore_item_ok(p, obj)) ignore = obj;
    }

    return ignore;
}


/*
 * Hack -- obtain an index for a floor object
 */
static int floor_to_index(struct chunk *c)
{
    int i, y, x;
    struct object *obj;

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

    /* First do crops and junk */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            for (obj = square_object(c, y, x); obj; obj = obj->next)
            {
                /* Nuke crops */
                if (tval_is_crop(obj)) break;

                /* Nuke junk */
                if (tval_is_skeleton(obj) || tval_is_corpse(obj) || tval_is_bottle(obj)) break;
            }
        }
    }
    if (obj)
    {
        int oidx = obj->oidx;

        square_excise_object(c, y, x, obj);
        object_delete(&obj);

        /* Use this index */
        c->o_gen[0 - (oidx + 1)] = true;

        return oidx;
    }

    /* Then do gold */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            for (obj = square_object(c, y, x); obj; obj = obj->next)
            {
                /* Nuke gold */
                if (tval_is_money(obj))
                {
                    /* Hack -- skip gold in houses */
                    if ((c->depth <= 0) && square_isvault(c, obj->iy, obj->ix)) continue;

                    break;
                }
            }
        }
    }
    if (obj)
    {
        int oidx = obj->oidx;

        square_excise_object(c, y, x, obj);
        object_delete(&obj);

        /* Use this index */
        c->o_gen[0 - (oidx + 1)] = true;

        return oidx;
    }

    return 0;
}


/*
 * Let the floor carry an object, deleting old ignored items if necessary.
 * The calling function must deal with the dropped object on failure.
 *
 * Optionally put the object at the top or bottom of the pile
 */
bool floor_carry(struct player *p, struct chunk *c, int y, int x, struct object *drop, bool last)
{
    int n = 0;
    struct object *obj, *ignore = floor_get_oldest_ignored(p, c, y, x);

    /* Scan objects in that grid for combination */
    for (obj = square_object(c, y, x); obj; obj = obj->next)
    {
        /* Check for combination */
        if (object_similar(p, obj, drop, OSTACK_FLOOR))
        {
            /* Combine the items */
            object_absorb(obj, drop);

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
        {
            square_excise_object(c, y, x, ignore);
            object_delete(&ignore);
        }
        else
            return false;
    }

    /* Hack -- set index */
    drop->oidx = floor_to_index(c);
    if (!drop->oidx) return false;

    /* Location */
    drop->iy = y;
    drop->ix = x;
    drop->depth = c->depth;

    /* Forget monster */
    drop->held_m_idx = 0;

    /* Link to the first or last object in the pile */
    if (last)
        pile_insert_end(&c->squares[y][x].obj, drop);
    else
        pile_insert(&c->squares[y][x].obj, drop);

    /* Redraw */
    square_note_spot(c, y, x);
    square_light_spot(c, y, x);

    /* Result */
    return true;
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
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed. Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 *
 * Objects which fail to be carried by the floor are deleted.
 */
void drop_near(struct player *p, struct chunk *c, struct object *dropped, int chance, int y,
    int x, bool verbose, int mode)
{
    int i, k, n, d, s;
    int bs, bn;
    int by, bx;
    int dy, dx;
    int ty, tx;
    struct object *obj;
    char o_name[NORMAL_WID];
    bool flag = false;
    bool in_house = false, no_drop = false;
    struct player *q = NULL;
    bool ignorable = false;

    /* Describe object */
    if (p) object_desc(p, o_name, sizeof(o_name), dropped, ODESC_BASE);

    /* Handle normal "breakage" */
    if (!dropped->artifact && (chance > 0) && magik(chance))
    {
        /* Message */
        if (p) msg(p, "The %s %s.", o_name, VERB_AGREEMENT(dropped->number, "breaks", "break"));

        /* Failure */
        object_delete(&dropped);
        return;
    }

    /* Score */
    bs = -1;

    /* Picker */
    bn = 0;

    /* Default */
    by = y;
    bx = x;

    /* Scan local grids */
    for (dy = -3; dy <= 3; dy++)
    {
        for (dx = -3; dx <= 3; dx++)
        {
            bool comb = false;

            /* Calculate actual distance */
            d = (dy * dy) + (dx * dx);

            /* Ignore distant grids */
            if (d > 10) continue;

            /* Location */
            ty = y + dy;
            tx = x + dx;

            /* Skip illegal grids */
            if (!square_in_bounds_fully(c, ty, tx)) continue;

            /* Require line of sight */
            if (!los(c, y, x, ty, tx)) continue;

            /* Require floor space */
            if (!square_isanyfloor(c, ty, tx)) continue;

            /* Require no trap or rune */
            if (square_isplayertrap(c, ty, tx) || square_iswarded(c, ty, tx)) continue;

            /* No objects */
            k = 0;
            n = 0;

            /* Scan objects in that grid */
            for (obj = square_object(c, ty, tx); obj; obj = obj->next)
            {
                /* Check for possible combination */
                if (object_similar(p, obj, dropped, OSTACK_FLOOR)) comb = true;

                /* Count objects */
                if (!(p && ignore_item_ok(p, obj))) k++;
                else n++;
            }

            /* Add new object */
            if (!comb) k++;

            /* Paranoia? */
            if (((k + n) > z_info->floor_size) && !floor_get_oldest_ignored(p, c, ty, tx))
                continue;

            /* Calculate score */
            s = 1000 - (d + k * 5);

            /* Skip bad values */
            if (s < bs) continue;

            /* New best value */
            if (s > bs) bn = 0;

            /* Apply the randomizer to equivalent values */
            if ((++bn >= 2) && randint0(bn)) continue;

            /* Keep score */
            bs = s;

            /* Track it */
            by = ty;
            bx = tx;

            /* Okay */
            flag = true;
        }
    }

    /* Handle lack of space */
    if (!flag && !dropped->artifact)
    {
        /* Message */
        if (p)
            msg(p, "The %s %s.", o_name, VERB_AGREEMENT(dropped->number, "disappears", "disappear"));

        /* Failure */
        object_delete(&dropped);
        return;
    }

    /* Find a grid */
    for (i = 0; !flag; i++)
    {
        /* Bounce around */
        if (i < 1000)
        {
            ty = rand_spread(by, 1);
            tx = rand_spread(bx, 1);
        }

        /* Random location */
        else
        {
            ty = randint0(c->height);
            tx = randint0(c->width);
        }

        /* Require floor space */
        if (!square_canputitem(c, ty, tx)) continue;

        /* Bounce to that location */
        by = ty;
        bx = tx;

        /* Okay */
        flag = true;
    }

    /* Check houses */
    if (true_artifact_p(dropped) || tval_can_have_timeout(dropped) || tval_is_light(dropped))
        in_house = location_in_house(c->depth, by, bx);

    /* Process true artifacts */
    if (true_artifact_p(dropped))
    {
        /* True artifacts cannot be dropped in houses... */
        if (in_house)
        {
            /* ...except the Crown and Grond which are not "unique" artifacts */
            if (!kf_has(dropped->kind->kind_flags, KF_QUEST_ART))
                no_drop = true;
        }

        /* True artifacts cannot be dropped/thrown in the wilderness */
        else if (c->depth < 0) no_drop = true;

        /* True artifacts cannot be dropped/thrown on special levels */
        else if (special_level(c->depth)) no_drop = true;

        if (no_drop)
        {
            switch (mode)
            {
                /* Make true artifact vanish */
                case DROP_FADE:
                {
                    if (p) msg(p, "The %s fades into the air!", o_name);

                    /* Preserve any true artifact */
                    preserve_artifact_aux(dropped);
                    if (p) history_lose_artifact(p, dropped);

                    object_delete(&dropped);
                    break;
                }

                /* Since the object has already been excised, we carry it again */
                case DROP_FORBID:
                {
                    msg(p, "You cannot drop this here.");
                    inven_carry(p, dropped, true, true);
                    break;
                }

                /* Since the object has already been excised, we silently carry it again */
                case DROP_SILENT:
                {
                    inven_carry(p, dropped, true, false);
                    break;
                }
            }

            return;
        }
    }

    /* Recharge rods dropped in houses instantly */
    if (tval_can_have_timeout(dropped) && in_house) dropped->timeout = 0;

    /* Refuel lights dropped in houses to the standard amount */
    if (tval_is_light(dropped) && in_house) fuel_default(dropped);

    if (c->squares[by][bx].mon < 0)
    {
        q = player_get(0 - c->squares[by][bx].mon);
        ignorable = ignore_item_ok(q, dropped);
    }

    /* Give it to the floor */
    if (!floor_carry(p, c, by, bx, dropped, false))
    {
        /* Message */
        if (p)
            msg(p, "The %s %s.", o_name, VERB_AGREEMENT(dropped->number, "disappears", "disappear"));

        /* Hack -- preserve artifacts */
        if (dropped->artifact)
        {
            /* Only works when owner is ingame */
            if (!p) p = player_get(get_owner_id(dropped));

            /* Preserve any artifact */
            preserve_artifact_aux(dropped);
            if (p) history_lose_artifact(p, dropped);
        }

        /* Failure */
        object_delete(&dropped);
        return;
    }

    /* Sound */
    if (p) sound(p, MSG_DROP);

    /* Message when an object falls under a player */
    if (q)
    {
        /* Check the item still exists and isn't ignored */
        if (verbose && !ignorable)
            msg(q, "You feel something roll beneath your feet.");

        /* Redraw */
        q->upkeep->redraw |= PR_FLOOR;
    }
}


/*
 * This will push objects off a square.
 *
 * The methodology is to load all objects on the square into a queue. Replace
 * the previous square with a type that does not allow for objects. Drop the
 * objects. Last, put the square back to its original type.
 */
void push_object(struct player *p, struct chunk *c, int y, int x)
{
    /* Save the original terrain feature */
    int feat_old = c->squares[y][x].feat;

    struct object *obj = square_object(c, y, x);
    struct queue *queue = q_new(z_info->floor_size);
    bool glyph = square_iswarded(c, y, x);

    /* Push all objects on the square, stripped of pile info, into the queue */
    while (obj)
    {
        struct object *next = obj->next;

        q_push_ptr(queue, obj);

        /* Orphan the object */
        square_excise_object(c, y, x, obj);

        /* Next object */
        obj = next;
    }

    /* Set feature to an open door */
    square_open_door(c, y, x);

    /* Drop objects back onto the floor */
    while (q_len(queue) > 0)
    {
        /* Take object from the queue */
        obj = q_pop_ptr(queue);

        /* Drop the object */
        drop_near(p, c, obj, 0, y, x, false, DROP_FADE);
    }

    /* Reset cave feature and rune if needed */
    square_set_feat(c, y, x, feat_old);
    if (glyph) square_add_ward(c, y, x);

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
    int py = p->py;
    int px = p->px;
    int num = 0;
    bool unknown;

    /* Sanity */
    if (!square_in_bounds(c, py, px)) return 0;

    /* Sensed or known */
    if (mode & OFLOOR_SENSE) obj = floor_pile_known(p, c, py, px);
    else obj = square_object(c, py, px);

    /* Skip empty squares */
    if (!obj) return 0;

    unknown = is_unknown(floor_pile_known(p, c, py, px));

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
    int y, int x)
{
    struct object *obj;
    int num = 0;

    /* Sanity */
    if (!square_in_bounds(c, y, x)) return 0;

    /* Scan all objects in the grid */
    for (obj = floor_pile_known(p, c, y, x); obj; obj = obj->next)
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
 * Sense the existence of objects on a grid in the current level
 */
void floor_pile_sense(struct player *p, struct chunk *c, int y, int x)
{
    struct object *obj;

    if (p->depth != c->depth) return;

    /* Make new sensed objects where necessary */
    if (p->cave->squares[y][x].obj) return;

    /* Sense every item on this grid */
    for (obj = square_object(c, y, x); obj; obj = obj->next)
    {
        /* Make the new object */
        struct object *new_obj = object_new();

        /* Give it a fake kind */
        object_prep(p, new_obj, (tval_is_money(obj)? unknown_gold_kind: unknown_item_kind), 0,
            MINIMISE);

        /* Attach it to the current floor pile */
        new_obj->iy = y;
        new_obj->ix = x;
        new_obj->depth = c->depth;
        pile_insert_end(&p->cave->squares[y][x].obj, new_obj);
    }
}


static bool object_equals(const struct object *obj1, const struct object *obj2)
{
    struct object *test;

    /* Objects are strictly equal */
    if (obj1 == obj2) return true;

    /* Objects are strictly different */
    if (!(obj1 && obj2)) return false;

    /* Make a writable identical copy of the second object */
    test = object_new();
    memcpy(test, obj2, sizeof(struct object));

    /* Make prev and next strictly equal since they are irrelevant */
    test->prev = obj1->prev;
    test->next = obj1->next;

    /* Known part must be equal */
    if (!object_equals(obj1->known, test->known))
    {
        mem_free(test);
        return false;
    }

    /* Make known strictly equal since they are now irrelevant */
    test->known = obj1->known;

    /* Brands must be equal */
    if (!brands_are_equal(obj1->brands, test->brands))
    {
        mem_free(test);
        return false;
    }

    /* Make brands strictly equal since they are now irrelevant */
    test->brands = obj1->brands;

    /* Slays must be equal */
    if (!slays_are_equal(obj1->slays, test->slays))
    {
        mem_free(test);
        return false;
    }

    /* Make slays strictly equal since they are now irrelevant */
    test->slays = obj1->slays;

    /* Make attr strictly equal since they are irrelevant */
    test->attr = obj1->attr;

    /* All other fields must be equal */
    if (memcmp(obj1, test, sizeof(struct object)) != 0)
    {
        mem_free(test);
        return false;
    }

    /* Success */
    mem_free(test);
    return true;
}


static void floor_pile_update(struct player *p, struct chunk *c, int y, int x)
{
    struct object *obj;

    /* Know every item on this grid */
    for (obj = square_object(c, y, x); obj; obj = obj->next)
    {
        /* Make the new object */
        struct object *new_obj = object_new();

        object_copy(new_obj, obj);

        /* Attach it to the current floor pile */
        pile_insert_end(&p->cave->squares[y][x].obj, new_obj);
    }
}


/*
 * Update the player's knowledge of the objects on a grid in the current level
 */
void floor_pile_know(struct player *p, struct chunk *c, int y, int x)
{
    struct object *obj = square_object(c, y, x), *known_obj = p->cave->squares[y][x].obj;

    if (p->depth != c->depth) return;

    /* Object is not known: update knowledge */
    if (!known_obj)
    {
        floor_pile_update(p, c, y, x);
        return;
    }

    /* Object is absent: wipe knowledge */
    if (!obj)
    {
        floor_pile_forget(p, y, x);
        return;
    }

    /* Object is known: wipe and update knowledge if something changed */
    while (obj || known_obj)
    {
        if (!object_equals(obj, known_obj))
        {
            floor_pile_forget(p, y, x);
            floor_pile_update(p, c, y, x);
            return;
        }
        if (obj) obj = obj->next;
        if (known_obj) known_obj = known_obj->next;
    }
}


void floor_pile_forget(struct player *p, int y, int x)
{
    struct object *current = p->cave->squares[y][x].obj, *next;

    while (current)
    {
        next = current->next;

        /* Stop tracking item */
        if (tracked_object_is(p->upkeep, current)) track_object(p->upkeep, NULL);

        object_delete(&current);
        current = next;
    }
    p->cave->squares[y][x].obj = NULL;
}


struct object *floor_pile_known(struct player *p, struct chunk *c, int y, int x)
{
    /* Hack -- DM has full knowledge */
    if (p->dm_flags & DM_SEE_LEVEL) return square_object(c, y, x);

    return p->cave->squares[y][x].obj;
}
