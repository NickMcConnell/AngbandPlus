
/*
 * File: object_util.cpp
 * Purpose: Object list maintenance and other object utilities
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */
#include "src/npp.h"
#include "src/init.h"


/*
 * Convert an inventory index into a one character label.
 *
 * Note that the label does NOT distinguish inven/equip.
 */
QChar index_to_label(int i)
{
    /* Indexes for "inven" are easy */
    if (i < INVEN_WIELD) return (number_to_letter(i));

    /* Indexes for "equip" are offset */
    return (number_to_letter(i - INVEN_WIELD));
}


static int get_inscribed_ammo_slot(const object_type *o_ptr)
{
    if (!o_ptr->k_idx) return FALSE;
    if (o_ptr->inscription.isEmpty()) return 0;
    QString inscrip = o_ptr->inscription;

    while (inscrip.contains('@'))
    {
        /* Find the first '@' */
        int first_index = inscrip.indexOf('@');

        // clear the '@'
        inscrip.remove(first_index, 1);

        //Paranoia
        if (inscrip.length() < first_index) continue;

        QChar s = inscrip[first_index];

        /* Found "@n"? */
        if (s.isDigit())
        {
            /* Convert to number */
            return (QUIVER_START + s.digitValue());
        }

        if (s.isLetter())
        {
            int convert = letter_to_number(s);
            if ((convert >=0) && (convert < 10))
            {
                return (QUIVER_START + convert);
            }
        }
    }

    return (0);
}


/**
 * Used by wield_slot() to find an appropriate slot for ammo. See wield_slot()
 * for information on what this returns.
 */
s16b wield_slot_ammo(object_type *o_ptr)
{
    s16b i, open = 0;
    object_type *j_ptr;

    /* Never pick up mimics */
    if (o_ptr->is_mimic()) return (QUIVER_END);

    /* If the ammo is inscribed with a slot number, we'll try to put it in */
    /* that slot, if possible. */
    i = get_inscribed_ammo_slot(o_ptr);
    if (i && !inventory[i].k_idx) return i;

    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        j_ptr = &inventory[i];

        if (!j_ptr->k_idx)
        {
            /* Save the open slot if we haven't found one already */
            if (!open) open = i;
            continue;
        }

        if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

        /* If ammo is cursed we can't stack it */
        if (inventory[i].is_cursed()) continue;

        /* If they are stackable, we'll use this slot for sure */
        if (object_similar(&inventory[i], o_ptr)) return i;
    }

    /* If not absorbed, return an open slot (or QUIVER_START if no room) */
    return open ? open : QUIVER_END;
}


/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(object_type *o_ptr)
{
    /*Hack - don't allow quest items to be worn*/
    if(o_ptr->is_quest_object()) return (-1);

    /* Hack - Don't wield mimic objects */
    if (o_ptr->is_mimic()) return (-1);

    /* Slot for equipment */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        {

            /* See if the throwing weapon can go into the quiver first */
            if ((is_throwing_weapon(o_ptr)) && (o_ptr->use_verify[AUTO_WIELD_QUIVER]))
            {
                s16b x = (wield_slot_ammo(o_ptr));

                if (x != QUIVER_END) return (x);
            }

            return (INVEN_WIELD);
        }

        case TV_BOW:
        {
            if (birth_swap_weapons) return (INVEN_WIELD);
            else return (INVEN_BOW);
        }

        case TV_RING:
        {
            /* Use the right hand first */
            if (!inventory[INVEN_RIGHT].k_idx) return (INVEN_RIGHT);

            /* Use the left hand for swapping (by default) */
            return (INVEN_LEFT);
        }

        case TV_AMULET:
        {
            return (INVEN_NECK);
        }

        case TV_LIGHT:
        {
            return (INVEN_LIGHT);
        }

        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        {
            return (INVEN_BODY);
        }

        case TV_CLOAK:
        {
            return (INVEN_OUTER);
        }

        case TV_SHIELD:
        case TV_DRAG_SHIELD:
        {
            return (INVEN_ARM);
        }

        case TV_CROWN:
        case TV_HELM:
        {
            return (INVEN_HEAD);
        }

        case TV_GLOVES:
        {
            return (INVEN_HANDS);
        }

        case TV_BOOTS:
        {
            return (INVEN_FEET);
        }

        /* Ammo asks for first quiver slot */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            return (wield_slot_ammo(o_ptr));
        }
    }

    /* No slot available */
    return (-1);
}


/*
 * Returns whether item o_ptr will fit in slot 'slot'
 */
bool slot_can_wield_item(int slot, object_type *o_ptr)
{
    if (o_ptr->is_ring())
    {
        return (slot == INVEN_LEFT || slot == INVEN_RIGHT) ? TRUE : FALSE;
    }
    else if (o_ptr->is_ammo())
    {
        return (slot >= QUIVER_START && slot < QUIVER_END) ? TRUE : FALSE;
    }
    else if ((is_throwing_weapon(o_ptr)) && (o_ptr->use_verify[AUTO_WIELD_QUIVER]))
    {
        return (slot >= QUIVER_START && slot < QUIVER_END) ? TRUE : FALSE;
    }

    return (wield_slot(o_ptr) == slot) ? TRUE : FALSE;
}


/*
 * Return a string mentioning how a given item is carried
 */
QString mention_use(int slot)
{

    switch (slot)
    {

        /* Also INVEN_MAIN_WEAPON and INVEN_SWAP_WEAPON */
        case INVEN_WIELD:
        case INVEN_BOW:
        {
            object_type *o_ptr = &inventory[slot];
            if (birth_swap_weapons)
            {
                if (slot == INVEN_SWAP_WEAPON) return "holding";
            }

            if (o_ptr->is_bow())
            {
                if (p_ptr->state.heavy_shoot)	return "Just holding";
                else return "Shooting";
            }
            else
            {
                /* Weapons */
                if (p_ptr->state.heavy_wield) return "Just lifting";
                else return "Wielding";
            }
        }

        case INVEN_LEFT:  return "On left hand";
        case INVEN_RIGHT: return "On right hand";
        case INVEN_NECK:  return "Around neck";
        case INVEN_LIGHT: return "Light source";
        case INVEN_BODY:  return "On body";
        case INVEN_OUTER: return "About body";
        case INVEN_ARM:   return "On arm";
        case INVEN_HEAD:  return "On head";
        case INVEN_HANDS: return "On hands";
        case INVEN_FEET:  return "On feet";

        case QUIVER_START + 0: return "In quiver [0]";
        case QUIVER_START + 1: return "In quiver [1]";
        case QUIVER_START + 2: return "In quiver [2]";
        case QUIVER_START + 3: return "In quiver [3]";
        case QUIVER_START + 4: return "In quiver [4]";
        case QUIVER_START + 5: return "In quiver [5]";
        case QUIVER_START + 6: return "In quiver [6]";
        case QUIVER_START + 7: return "In quiver [7]";
        case QUIVER_START + 8: return "In quiver [8]";
        case QUIVER_START + 9: return "In quiver [9]";
    }

    return "In pack";
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
QString describe_use(int i)
{
    QString p;
    object_type *o_ptr;

    if ((birth_swap_weapons) && ((i == INVEN_MAIN_WEAPON) || (i == INVEN_SWAP_WEAPON)))
    {
        o_ptr = &inventory[i];
        if (o_ptr->is_bow())
        {
            if (p_ptr->state.heavy_shoot)	p = "just holding";
            else p = "shooting missiles with";
        }
        else
        {
            if (p_ptr->state.heavy_wield)	p = "just lifting";
            else p = "attacking monsters with";
        }

        /* Return the result */
        return p;
    }

    if (i >= QUIVER_START) p = "carrying in your quiver";

    else switch (i)
    {
        case INVEN_WIELD:	p = "attacking monsters with"; break;
        case INVEN_BOW:		p = "shooting missiles with"; break;
        case INVEN_LEFT:	p = "wearing on your left hand"; break;
        case INVEN_RIGHT:	p = "wearing on your right hand"; break;
        case INVEN_NECK:	p = "wearing around your neck"; break;
        case INVEN_LIGHT:	p = "using to light the way"; break;
        case INVEN_BODY:	p = "wearing on your body"; break;
        case INVEN_OUTER:	p = "wearing on your back"; break;
        case INVEN_ARM:		p = "wearing on your arm"; break;
        case INVEN_HEAD:	p = "wearing on your head"; break;
        case INVEN_HANDS:	p = "wearing on your hands"; break;
        case INVEN_FEET:	p = "wearing on your feet"; break;
        default:			p = "carrying in your pack"; break;
    }

    /* Hack -- Heavy weapon */
    if (i == INVEN_WIELD)
    {
        o_ptr = &inventory[i];
        if (p_ptr->state.heavy_wield) p = "just lifting";
    }

    /* Hack -- Heavy bow */
    if (i == INVEN_BOW)
    {
        o_ptr = &inventory[i];
        if (p_ptr->state.heavy_shoot)p = "just holding";
    }

    /* Return the result */
    return p;
}


static int quiver_wield(int item, object_type *o_ptr)
{
    int slot = 0;
    int num;
    QString o_name;

    object_type object_type_body;
    object_type *i_ptr;

    /* was: num = get_quantity(NULL, o_ptr->number);*/
    num = o_ptr->number;

    /* Cancel */
    if (num <= 0) return 0;

    /* Check free space */
    if (!quiver_carry_okay(o_ptr, num, item))
    {
        message(QString("Your quiver needs more space."));
        return 0;
    }

    /* Get local object */
    i_ptr = &object_type_body;

    /* Copy to the local object */
    i_ptr->object_copy(o_ptr);

    /* Modify quantity */
    i_ptr->number = num;

    /* No longer in use */
    i_ptr->obj_in_use = FALSE;

    /* Describe the result */
    o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

    /*Mark it to go in the quiver */
    o_ptr->use_verify[AUTO_WIELD_QUIVER] = TRUE;

    slot = wield_slot_ammo(o_ptr);

    /* Handle errors (paranoia) */
    if (slot == QUIVER_END)
    {
        /* No space. */
        message(QString("Your quiver is full."));
        return 0;
    }

    /* Decrease the item (from the pack) */
    if (item >= 0)
    {
        inven_item_increase(item, -num);
        inven_item_optimize(item);
    }

    /* Decrease the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -num);
        floor_item_optimize(0 - item);
    }

    /* Get the wield slot */
    o_ptr = &inventory[slot];

    /* Describe the result */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Take off existing item */
    if (o_ptr->k_idx)
    {
        /* Take off existing item */
        o_ptr->number += i_ptr->number;
    }

    /* Wear the new stuff */
    else o_ptr->object_copy(i_ptr);

    /* Describe the result */
    o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    sound(MSG_WIELD);
    message(QString("You have readied %1 (%2).") .arg(o_name) .arg(index_to_label(slot)));

    /* Cursed! */
    if (o_ptr->is_cursed())
    {
        /* Warn the player */
        sound(MSG_CURSED);
        message(QString("Oops! It feels deathly cold!"));

        /* Remove special inscription, if any */
        if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

        /* Sense the object if allowed */
        if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

        /* The object has been "sensed" */
        o_ptr->ident |= (IDENT_SENSE);
    }

    slot = sort_quiver(slot);

    /* Save quiver size */
    save_quiver_size();

    /* See if we have to overflow the pack */
    pack_overflow();

    /* Recalculate bonuses */
    p_ptr->notice |= (PN_SORT_QUIVER);
    p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
    if (item > 0) p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    else p_ptr->redraw |= (PR_WIN_EQUIPMENT | PR_WIN_OBJLIST);

    /* Reorder the quiver and return the perhaps modified slot */
    return (slot);
}


/*
 * Wield or wear a single item from the pack or floor.
 * Does not process the player energy.
 */
void wield_item(object_type *o_ptr, int item, int slot)
{
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;

    QString fmt;
    QString o_name;

    /* Describe the result */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Ammo goes in quiver slots, which have special rules. */
    if (IS_QUIVER_SLOT(slot))
    {
        /* Put the ammo in the quiver */
        slot = quiver_wield(item, o_ptr);

        // Succcess
        return;
    }

    /* Hack - Don't wield mimic objects */
    if (o_ptr->is_mimic()) return;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain local object */
    i_ptr->object_copy(o_ptr);

    /* Modify quantity */
    i_ptr->number = 1;

    /* No longer in use */
    i_ptr->obj_in_use = FALSE;

    /* Decrease the item (from the pack) */
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_optimize(item);
    }

    /* Decrease the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_optimize(0 - item);

    }

    /* Get the wield slot */
    o_ptr = &inventory[slot];

    /* Take off existing item */
    if (o_ptr->k_idx)
    {
        /* Take off existing item */
        (void)inven_takeoff(slot, 255);
    }

    /* Wear the new stuff */
    o_ptr->object_copy(i_ptr);

    /* Increment the equip counter by hand */
    p_ptr->equip_cnt++;

    /* Autoinscribe if the object was on the floor */
    if (item < 0) apply_autoinscription(o_ptr);

    /* Describe the result */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Where is the item now */
    if ((slot == INVEN_WIELD) || (slot == INVEN_BOW))
    {
        if (o_ptr->is_bow()) fmt = (QString("You are shooting with %1 (%2).") .arg(o_name) .arg(index_to_label(slot)));
        else fmt = (QString("You are wielding %1 (%2).") .arg(o_name) .arg(index_to_label(slot)));

    }
    else if (slot == INVEN_LIGHT) fmt = (QString("Your light source is %1 (%2).") .arg(o_name) .arg(index_to_label(slot)));

    else fmt = (QString("You are wearing %1 (%2).") .arg(o_name) .arg(index_to_label(slot)));

    /* Message */
    sound(MSG_WIELD);
    message(fmt);

    /* Cursed! */
    if (o_ptr->is_cursed())
    {
        /* Warn the player */
        sound(MSG_CURSED);
        message(QString("Oops! It feels deathly cold!"));

        /* Remove special inscription, if any */
        if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

        /* Sense the object if allowed */
        if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_CURSED;

        /* The object has been "sensed" */
        o_ptr->ident |= (IDENT_SENSE);
    }

    /* Save quiver size */
    save_quiver_size();

    /* See if we have to overflow the pack */
    pack_overflow();

    /* Recalculate bonuses, torch, mana */
    p_ptr->notice |= (PN_SORT_QUIVER);
    p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
    if (item > 0) p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    else p_ptr->redraw |= (PR_WIN_EQUIPMENT | PR_WIN_OBJLIST);
}





/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_type *o_ptr, int obj_num)
{
    /* Hack -- allow listing empty slots */
    if (item_tester_full) return (TRUE);

    /* Require an item */
    if (!o_ptr->k_idx) return (FALSE);

    /* Don't allow this choice for swap weapons */
    if ((item_tester_swap) && (obj_num == INVEN_SWAP_WEAPON)) return (FALSE);

    /* Hack -- ignore "gold" */
    if (o_ptr->tval == TV_GOLD) return (FALSE);

    /* Check the tval */
    if (item_tester_tval)
    {
        if (item_tester_tval != o_ptr->tval) return (FALSE);
    }

    /* Check the hook */
    if (item_tester_hook)
    {
        if (!(*item_tester_hook)(o_ptr)) return (FALSE);
    }

    /* Assume okay */
    return (TRUE);
}

/*
 * Get the indexes of objects at a given floor location.
 *
 * Return the number of object indexes acquired.
 *
 * Never acquire more than "size" object indexes, and never return a
 * number bigger than "size", even if more floor objects exist.
 *
 * Valid flags are any combination of the bits:
 *
 *   0x01 -- Verify item tester
 *   0x02 -- Marked items only
 */
int scan_floor(int *items, int size, int y, int x, int mode)
{
    int this_o_idx, next_o_idx;

    int num = 0;

    /* Sanity */
    if (!in_bounds(y, x)) return (0);

    /* Scan all objects in the grid */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Verify item tester */
        if ((mode & 0x01) && !item_tester_okay(o_ptr, this_o_idx)) continue;

        /* Marked items only */
        if ((mode & 0x02) && !o_ptr->marked) continue;

        /* Accept this item */
        items[num++] = this_o_idx;

        /* Enforce size limit */
        if (num >= size) break;
    }

    /* Result */
    return (num);
}


/*
 * Excise a dungeon object from any stacks
 */
void excise_object_idx(int o_idx)
{
    object_type *j_ptr;

    s16b this_o_idx, next_o_idx = 0;

    s16b prev_o_idx = 0;

    /* Object */
    j_ptr = &o_list[o_idx];

    /* Monster */
    if (j_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Monster */
        m_ptr = &mon_list[j_ptr->held_m_idx];

        /* Scan all objects in the grid */
        for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = &o_list[this_o_idx];

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Done */
            if (this_o_idx == o_idx)
            {
                /* No previous */
                if (prev_o_idx == 0)
                {
                    /* Remove from list */
                    m_ptr->hold_o_idx = next_o_idx;
                }

                /* Real previous */
                else
                {
                    object_type *i_ptr;

                    /* Previous object */
                    i_ptr = &o_list[prev_o_idx];

                    /* Remove from list */
                    i_ptr->next_o_idx = next_o_idx;
                }

                /* Forget next pointer */
                o_ptr->next_o_idx = 0;

                /* Done */
                break;
            }

            /* Save prev_o_idx */
            prev_o_idx = this_o_idx;
        }
    }

    /* Dungeon */
    else
    {
        int y = j_ptr->iy;
        int x = j_ptr->ix;

        /* Scan all objects in the grid */
        for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = &o_list[this_o_idx];

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Done */
            if (this_o_idx == o_idx)
            {
                /* No previous */
                if (prev_o_idx == 0)
                {
                    /* Remove from list */
                    dungeon_info[y][x].object_idx = next_o_idx;
                }

                /* Real previous */
                else
                {
                    object_type *i_ptr;

                    /* Previous object */
                    i_ptr = &o_list[prev_o_idx];

                    /* Remove from list */
                    i_ptr->next_o_idx = next_o_idx;
                }

                /* Forget next pointer */
                o_ptr->next_o_idx = 0;

                /* Done */
                break;
            }

            /* Save prev_o_idx */
            prev_o_idx = this_o_idx;
        }
    }
}


/*
 * Delete a dungeon object
 *
 * Handle "stacks" of objects correctly.
 */
void delete_object_idx(int o_idx)
{
    object_type *j_ptr;

    /* Excise */
    excise_object_idx(o_idx);

    /* Object */
    j_ptr = &o_list[o_idx];

    /* Dungeon floor */
    if (!(j_ptr->held_m_idx))
    {
        int y, x;

        /* Location */
        y = j_ptr->iy;
        x = j_ptr->ix;

        p_ptr->redraw |= (PR_WIN_OBJLIST);

        /* Visual update */
        light_spot(y, x);
    }

    /* Wipe the object */
    j_ptr->object_wipe();

    /* Count objects */
    o_cnt--;
}


/*
 * Deletes all objects at given location
 */
void delete_object(int y, int x)
{
    s16b this_o_idx, next_o_idx = 0;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Scan all objects in the grid */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Wipe the object */
        o_ptr->object_wipe();

        /* Count objects */
        o_cnt--;
    }

    /* Objects are gone */
    dungeon_info[y][x].object_idx = 0;

    p_ptr->redraw |= (PR_WIN_OBJLIST);

    /* Visual update */
    light_spot(y, x);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_objects_aux(int i1, int i2)
{
    int i;

    object_type *o_ptr;


    /* Do nothing */
    if (i1 == i2) return;


    /* Repair objects */
    for (i = 1; i < o_max; i++)
    {
        /* Get the object */
        o_ptr = &o_list[i];

        /* Skip "dead" objects */
        if (!o_ptr->k_idx) continue;

        /* Repair "next" pointers */
        if (o_ptr->next_o_idx == i1)
        {
            /* Repair */
            o_ptr->next_o_idx = i2;
        }
    }


    /* Get the object */
    o_ptr = &o_list[i1];


    /* Monster */
    if (o_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Get the monster */
        m_ptr = &mon_list[o_ptr->held_m_idx];

        /* Repair monster */
        if (m_ptr->hold_o_idx == i1)
        {
            /* Repair */
            m_ptr->hold_o_idx = i2;
        }
    }

    /* Dungeon */
    else
    {
        int y, x;

        /* Get location */
        y = o_ptr->iy;
        x = o_ptr->ix;

        /* Repair grid */
        if (dungeon_info[y][x].object_idx == i1)
        {
            /* Repair */
            dungeon_info[y][x].object_idx = i2;
        }
    }


    /* Hack -- move object */
    COPY(&o_list[i2], &o_list[i1], object_type);

    /* Hack -- wipe hole */
    o_ptr->object_wipe();
}


/*
 * Compact and Reorder the object list
 *
 * This function can be very dangerous, use with caution!
 *
 * When actually "compacting" objects, we base the saving throw on a
 * combination of object level, distance from player, and current
 * "desperation".
 *
 * After "compacting" (if needed), we "reorder" the objects into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int i, y, x, num, cnt;

    int cur_lev, cur_dis, chance;

    /* Compact */
    if (size)
    {
        /* Message */
        message(QString("Compacting objects..."));

        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);

    }

    /* First do squelchable objects */
    for (i = 1; (i < o_max) && (size); i++)
    {
        object_type *o_ptr = &(o_list[i]);

        /* Nuke squelched items */
        if (squelch_item_ok(o_ptr))
        {
            delete_object_idx(i);
            size--;
        }
    }

    /* Now try to combine objects and gold instead (backwards) */
    for (i = o_max - 1; i >= 2 && (size); i--)
    {
        object_type *o_ptr =&(o_list[i]);
        int j;

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        for (j = i - 1; j >= 1; j--)
        {
            object_type *j_ptr =&(o_list[j]);

            /* Skip dead objects */
            if (!j_ptr->k_idx) continue;

            if (object_similar(j_ptr, o_ptr))
            {
                /* Combine the items */
                object_absorb(j_ptr, o_ptr);

                /* Delete the object */
                delete_object_idx(i);
                size--;
                break;
            }
        }
    }

    /* Compact at least 'size' objects */
    for (num = 0, cnt = 1; num < size; cnt++)
    {
        /* Get more vicious each iteration */
        cur_lev = 5 * cnt;

        /* Get closer each iteration */
        cur_dis = 5 * (20 - cnt);

        /* Examine the objects */
        for (i = 1; i < o_max; i++)
        {
            object_type *o_ptr = &o_list[i];

            object_kind *k_ptr = &k_info[o_ptr->k_idx];

            /* Skip dead objects */
            if (!o_ptr->k_idx) continue;

            /* Hack -- High level objects start out "immune" */
            if ((k_ptr->k_level > cur_lev) && (k_ptr->squelch != SQUELCH_ALWAYS)) continue;

            /* Hack - don't compact mimics */
            if (o_ptr->is_mimic()) continue;

            /* Monster */
            if (o_ptr->held_m_idx)
            {
                monster_type *m_ptr;

                /* Get the monster */
                m_ptr = &mon_list[o_ptr->held_m_idx];

                /* Get the location */
                y = m_ptr->fy;
                x = m_ptr->fx;

                /* Monsters protect their objects */
                if ((rand_int(100) < 90) && (k_ptr->squelch != SQUELCH_ALWAYS)) continue;
            }

            /* Dungeon */
            else
            {
                /* Get the location */
                y = o_ptr->iy;
                x = o_ptr->ix;
            }

            /* Nearby objects start out "immune" */
            if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis) &&
                (k_ptr->squelch != SQUELCH_ALWAYS)) continue;

            /* Saving throw */
            chance = 90;

            /* Hack -- only compact artifacts in emergencies */
            if (o_ptr->is_artifact() && (cnt < 1000)) chance = 100;

            /* Apply the saving throw */
            if (rand_int(100) < chance) continue;

            /* Delete the object */
            delete_object_idx(i);

            /* Count it */
            num++;
        }
    }


    /* Excise dead objects (backwards!) */
    for (i = o_max - 1; i >= 1; i--)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip real objects */
        if (o_ptr->k_idx) continue;

        /* Move last object into open hole */
        compact_objects_aux(o_max - 1, i);

        /* Compress "o_max" */
        o_max--;
    }
}


/*
 * Delete all the items when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "cave_o_idx[y][x]" field for every grid,
 * and the "m_ptr->next_o_idx" field for every monster, since
 * we know we are clearing every object.  Technically, we only
 * clear those fields for grids/monsters containing objects,
 * and we clear it once for every such object.
 */
void wipe_o_list(void)
{
    int i;

    /* Delete the existing objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Mega-Hack -- preserve artifacts */
        if (!character_dungeon || birth_preserve)
        {
            /* Hack -- Preserve unknown artifacts */
            if (o_ptr->is_artifact() && !o_ptr->is_known())
            {
                /* Mega-Hack -- Preserve the artifact */
                a_info[o_ptr->art_num].a_cur_num = 0;
            }
        }

        /* Monster */
        if (o_ptr->held_m_idx)
        {
            monster_type *m_ptr;

            /* Monster */
            m_ptr = &mon_list[o_ptr->held_m_idx];

            /* Hack -- see above */
            m_ptr->hold_o_idx = 0;
        }

        /* Dungeon */
        else
        {
            /* Get the location */
            int y = o_ptr->iy;
            int x = o_ptr->ix;

            /* Hack -- see above */
            dungeon_info[y][x].object_idx = 0;
        }

        /*Wipe the randart if necessary*/
        if (o_ptr->art_num) artifact_wipe(o_ptr->art_num, FALSE);

        /* Wipe the object */
        o_ptr->object_wipe();
    }

    /* Reset "o_max" */
    o_max = 1;

    /* Reset "o_cnt" */
    o_cnt = 0;
}





/*
 * Get and return the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b o_pop(void)
{
    int i;


    /* Initial allocation */
    if (o_max < z_info->o_max)
    {
        /* Get next space */
        i = o_max;

        /* Expand object array */
        o_max++;

        /* Count objects */
        o_cnt++;

        /* Use this object */
        return (i);
    }


    /* Recycle dead objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[i];

        /* Skip live objects */
        if (o_ptr->k_idx) continue;

        /* Count objects */
        o_cnt++;

        /* Use this object */
        return (i);
    }

    /* Warn the player (except during dungeon creation) */
    if (character_dungeon) pop_up_message_box("Too many objects!");

    /* Oops */
    return (0);
}


/*
 * Returns the number of floor items on a certain grid
 */
int count_floor_items(int y, int x, bool pickup_only)
{
    s16b this_o_idx, next_o_idx = 0;
    int count = 0;
    object_type *o_ptr;

    /* Paranoia */
    if (!in_bounds_fully(y, x)) return (0);

    /* Nothing there */
    if (!dungeon_info[y][x].has_object()) return (0);

    /* Scan all objects in the grid */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Factor in whether the player wants to be prompted first */
        if (pickup_only)
        {
            if (k_info[o_ptr->k_idx].squelch == NO_SQUELCH_NEVER_PICKUP)
            {
                /* Only known items */
                if (o_ptr->is_known()) continue;
            }
        }

        count++;
    }
    return (count);
}


/*
 * Get the first object at a dungeon location
 * or NULL if there isn't one.
 */
object_type *get_first_object(int y, int x)
{
    s16b o_idx;

    /* Paranoia */
    if (!in_bounds_fully(y, x)) return (NULL);

    o_idx = dungeon_info[y][x].object_idx;

    if (o_idx) return (&o_list[o_idx]);

    /* No object */
    return (NULL);
}


/*
 * Get the next object in a stack or NULL if there isn't one.
 */
object_type *get_next_object(object_type *o_ptr)
{
    if (o_ptr->next_o_idx) return (&o_list[o_ptr->next_o_idx]);

    /* No more objects */
    return (NULL);
}


/*
 * Apply a "object restriction function" to the "object allocation table"
 */
int get_obj_num_prep(void)
{
    int i;

    /* Scan the allocation table */
    for (i = 0; i < alloc_kind_table.size(); i++)
    {
        /* Accept objects which pass the restriction, if any */
        if (!get_obj_num_hook || (*get_obj_num_hook)(alloc_kind_table[i].index))
        {
            /* Accept this object */
            alloc_kind_table[i].hook_probability = alloc_kind_table[i].base_probability;
        }

        /* Do not use this object */
        else
        {
            /* Decline this object */
            alloc_kind_table[i].hook_probability = 0;
        }
    }

    /* Success */
    return (0);
}


/*
 * Determine if a weapon is 'blessed'
 */
bool is_blessed(object_type *o_ptr)
{
    /* Is the object blessed? */
    if (o_ptr->obj_flags_3 & (TR3_BLESSED)) return (TRUE);
    return (FALSE);

}


/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(object_type *o_ptr)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Use template cost for aware objects */
    if (o_ptr->is_aware()) return (k_ptr->cost);

    /* Analyze the type */
    switch (o_ptr->tval)
    {
        /* Un-aware Food */
        case TV_FOOD: return (5L);

        /* Un-aware Potions */
        case TV_POTION: return (20L);

        /* Un-aware Scrolls */
        case TV_SCROLL: return (20L);

        /* Un-aware Staffs */
        case TV_STAFF: return (70L);

        /* Un-aware Wands */
        case TV_WAND: return (50L);

        /* Un-aware Rods */
        case TV_ROD: return (90L);

        /* Un-aware Rings */
        case TV_RING: return (45L);

        /* Un-aware Amulets */
        case TV_AMULET: return (45L);
    }

    /* Paranoia -- Oops */
    return (0L);
}


/*
 * Return the "real" price of a "known" item, not including discounts.
 *
 * Wand and staffs get cost for each charge.
 *
 * Armor is worth an extra 100 gold per bonus point to armor class.
 *
 * Weapons are worth an extra 100 gold per bonus point (AC,TH,TD).
 *
 * Missiles are only worth 5 gold per bonus point, since they
 * usually appear in groups of 20, and we want the player to get
 * the same amount of cash for any "equivalent" item.  Note that
 * missiles never have any of the "pval" flags, and in fact, they
 * only have a few of the available flags, primarily of the "slay"
 * and "brand" and "ignore" variety.
 *
 * Weapons with negative hit+damage bonuses are worthless.
 *
 * Every wearable item with a "pval" bonus is worth extra (see below).
 */
static s32b object_value_real(object_type *o_ptr)
{
    s32b value;

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Hack -- "worthless" items */
    if (!k_ptr->cost) return (0L);

    /* Base cost */
    value = k_ptr->cost;

    /* Artifact */
    if (o_ptr->art_num)
    {
        artifact_type *a_ptr = &a_info[o_ptr->art_num];

        /* Hack -- "worthless" artifacts */
        if (!a_ptr->cost) return (0L);

        /* Hack -- Use the artifact cost instead */
        value = a_ptr->cost;
    }

    /* Ego-Item */
    else if (o_ptr->ego_num)
    {
        ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

        /* Hack -- "worthless" ego-items */
        if (!e_ptr->cost) return (0L);

        /* Hack -- Reward the ego-item with a bonus */
        value += e_ptr->cost;
    }


    /* Analyze pval bonus */
    switch (o_ptr->tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_LIGHT:
        case TV_AMULET:
        case TV_RING:
        {
            /* Hack -- Negative "pval" is always bad */
            if (o_ptr->pval < 0) return (0L);

            /* No pval */
            if (!o_ptr->pval) break;

            /* Give credit for stat bonuses */
            if (o_ptr->obj_flags_1 & (TR1_STR)) value += (o_ptr->pval * 200L);
            if (o_ptr->obj_flags_1 & (TR1_INT)) value += (o_ptr->pval * 200L);
            if (o_ptr->obj_flags_1 & (TR1_WIS)) value += (o_ptr->pval * 200L);
            if (o_ptr->obj_flags_1 & (TR1_DEX)) value += (o_ptr->pval * 200L);
            if (o_ptr->obj_flags_1 & (TR1_CON)) value += (o_ptr->pval * 200L);
            if (o_ptr->obj_flags_1 & (TR1_CHR)) value += (o_ptr->pval * 200L);

            /* Give credit for stealth and searching */
            if (o_ptr->obj_flags_1 & (TR1_STEALTH)) value += (o_ptr->pval * 100L);
            if (o_ptr->obj_flags_1 & (TR1_SEARCH)) value += (o_ptr->pval * 100L);

            /* Give credit for infra-vision and tunneling */
            if (o_ptr->obj_flags_1 & (TR1_INFRA)) value += (o_ptr->pval * 50L);
            if (o_ptr->obj_flags_1 & (TR1_TUNNEL)) value += (o_ptr->pval * 50L);

            /* Give credit for perfect balance. */
            if (o_ptr->ident & IDENT_PERFECT_BALANCE) value += o_ptr->dd * 200L;

            /* Give credit for extra attacks */
            if (o_ptr->obj_flags_1 & (TR1_BLOWS)) value += (o_ptr->pval * 2000L);

            /* Give credit for speed bonus */
            if (o_ptr->obj_flags_1 & (TR1_SPEED)) value += (o_ptr->pval * 30000L);

            break;
        }
    }


    /* Analyze the item */
    switch (o_ptr->tval)
    {
        /* Wands/Staffs */
        case TV_WAND:
        case TV_STAFF:
        {
            /* Pay extra for charges, depending on standard number of
             * charges.  Handle new-style wands correctly.
             */
            value += ((value / 20) * (o_ptr->pval / o_ptr->number));

            /* Done */
            break;
        }

        /* Rings/Amulets */
        case TV_RING:
        case TV_AMULET:
        {
            /* Hack -- negative bonuses are bad */
            if (o_ptr->to_a < 0) return (0L);
            if (o_ptr->to_h < 0) return (0L);
            if (o_ptr->to_d < 0) return (0L);

            /* Give credit for bonuses */
            value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L);

            /* Done */
            break;
        }

        /* Armor */
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            /* Give credit for hit bonus */
            value += ((o_ptr->to_h - k_ptr->to_h) * 100L);

            /* Give credit for damage bonus */
            value += ((o_ptr->to_d - k_ptr->to_d) * 100L);

            /* Give credit for armor bonus */
            value += (o_ptr->to_a * 100L);

            /* Done */
            break;
        }

        /* Bows/Weapons */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_SWORD:
        case TV_POLEARM:
        {
            /* Hack -- negative hit/damage bonuses */
            if (o_ptr->to_h + o_ptr->to_d < 0) return (0L);

            /* Factor in the bonuses */
            value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L);

            /* Hack -- Factor in extra damage dice */
            if ((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds))
            {
                value += (o_ptr->dd - k_ptr->dd) * o_ptr->ds * 100L;
            }

            /* Done */
            break;
        }

        /* Ammo */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /* Hack -- negative hit/damage bonuses */
            if (o_ptr->to_h + o_ptr->to_d < 0) return (0L);

            /* Factor in the bonuses */
            value += ((o_ptr->to_h + o_ptr->to_d) * 5L);

            /* Hack -- Factor in extra damage dice */
            if ((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds))
            {
                value += (o_ptr->dd - k_ptr->dd) * o_ptr->ds * 5L;
            }

            /* Done */
            break;
        }
    }

    /*TO_DO - determine value of native items*/

    /* No negative value */
    if (value < 0) value = 0;

    /* Return the value */
    return (value);
}


/*
 * Return the price of an item including plusses (and charges).
 *
 * This function returns the "value" of the given item (qty one).
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 *
 * Note that discounted items stay discounted forever.
 */
s32b object_value(object_type *o_ptr)
{
    s32b value;

    /* Unknown items -- acquire a base value */
    if (o_ptr->is_known())
    {
        /* Broken items -- worthless */
        if (o_ptr->is_broken()) return (0L);

        /* Cursed items -- worthless */
        if (o_ptr->is_cursed()) return (0L);

        /* Real value (see above) */
        value = object_value_real(o_ptr);
    }

    /* Known items -- acquire the actual value */
    else
    {
        /* Hack -- Felt broken items */
        if ((o_ptr->ident & (IDENT_SENSE)) && o_ptr->is_broken()) return (0L);

        /* Hack -- Felt cursed items */
        if ((o_ptr->ident & (IDENT_SENSE)) && o_ptr->is_cursed()) return (0L);

        /* Base value (see above) */
        value = object_value_base(o_ptr);
    }


    /* Apply discount (if any) */
    if (o_ptr->discount > 0 && o_ptr->discount < INSCRIP_NULL)
    {
        value -= (value * o_ptr->discount / 100L);
    }


    /* Return the final value */
    return (value);
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.  They will unstack
 * (if necessary) when they are used.
 *
 * If permitted, we allow weapons/armor to stack, if fully "known".
 *
 * Missiles will combine if both stacks have the same "known" status.
 * This is done to make unidentified stacks of missiles useful.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, except rods, never stack (for various reasons).
 */
bool object_similar(object_type *o_ptr, object_type *j_ptr)
{
    /* Extract the flags */
    o_ptr->update_object_flags();
    j_ptr->update_object_flags();

    int total = o_ptr->number + j_ptr->number;

    /* Special case: gold */
    if ((o_ptr->tval == TV_GOLD) && (j_ptr->tval == TV_GOLD))
    {
        /* A gold object might have different object entries */
        QString o_name = k_info[o_ptr->k_idx].k_name;
        QString j_name = k_info[j_ptr->k_idx].k_name;

        /* Compare by name */
        if (o_name.compare(j_name))
        {
            /* pval is a s16b. Check bounds */
            if (((long)(o_ptr->pval) + (long)(j_ptr->pval)) <= SHRT_MAX) return (TRUE);

            /* Overflow. Don't stack gold */
            else return (FALSE);
        }
    }

    /* Require identical object types */
    if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

    /* Hack - mimic objects aren't similar*/
    if (o_ptr->is_mimic()) return (FALSE);
    if (j_ptr->is_mimic()) return (FALSE);

    /* Analyze the items */
    switch (o_ptr->tval)
    {
        /* Chests */
        case TV_CHEST:
        {
            /* Otherwise, never okay */
            return (FALSE);
        }

        /* Food and Potions and Scrolls */
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:
        case TV_PARCHMENT:
        {
            /* Assume okay */
            break;
        }

        /* Staves and Wands */
        case TV_STAFF:
        case TV_WAND:
        {
            /* Require either knowledge or known empty for both wands and staffs. */
            if ((!(o_ptr->ident & (IDENT_EMPTY)) &&
                            !o_ptr->is_known()) ||
                            (!(j_ptr->ident & (IDENT_EMPTY)) &&
                            !j_ptr->is_known())) return (FALSE);

            /* Wand/Staffs charges combine in NPPangband.  */

            /* Assume okay */
            break;

        }

        /* Rods */
        case TV_ROD:
        {

            /* Assume okay */
            break;
        }

        /* Weapons and Armor */
        /* Rings, Amulets, Lites and Books */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_RING:
        case TV_AMULET:
        case TV_LIGHT:
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            /* Require both items to be known */
            if (!o_ptr->is_known() || !j_ptr->is_known()) return (FALSE);

            /* Fall through */
        }

        /* Missiles */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Require identical knowledge of both items */
            if (o_ptr->is_known() != j_ptr->is_known()) return (FALSE);

            /* Require identical "bonuses" */
            if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
            if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
            if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

            /* Require identical "pval" code */
            if (o_ptr->pval != j_ptr->pval) return (FALSE);

            /* Require identical "artifact" names */
            if (o_ptr->art_num != j_ptr->art_num) return (FALSE);

            /* Require identical "ego-item" names */
            if (o_ptr->ego_num != j_ptr->ego_num) return (FALSE);

            /* Hack -- Never stack "powerful" items */
            if (o_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

            /* Mega-Hack -- Handle lites */
            if (o_ptr->is_fuelable_lite())
            {
                if (o_ptr->timeout != j_ptr->timeout) return (FALSE);
            }

            /* Hack -- Never stack recharging items */
            else if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

            /* Require identical "values" */
            if (o_ptr->ac != j_ptr->ac) return (FALSE);
            if (o_ptr->dd != j_ptr->dd) return (FALSE);
            if (o_ptr->ds != j_ptr->ds) return (FALSE);


            /*Allow well balanced items to stack only with other
             *well balanced items*/
            if ((o_ptr->ident & IDENT_PERFECT_BALANCE) !=
                (j_ptr->ident & IDENT_PERFECT_BALANCE)) return (FALSE);

            /* Probably okay */
            break;
        }

        /* Various */
        default:
        {
            /* Require knowledge */
            if (!o_ptr->is_known() || !j_ptr->is_known()) return (FALSE);

            /* Probably okay */
            break;
        }
    }

    /* Hack -- Require identical "cursed" and "broken" status */
    if (((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED))) ||
        ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))))
    {
        return (FALSE);
    }

    /* Hack -- Mimics never stack */
    if ((o_ptr->is_mimic()) || (j_ptr->is_mimic()))
    {
        return (FALSE);
    }

    /* Hack -- Require compatible inscriptions */
    if (o_ptr->inscription != j_ptr->inscription)
    {
        /* Normally require matching inscriptions */
        if (!stack_force_notes) return (FALSE);

        /* Never combine different inscriptions */
        if (!o_ptr->inscription.isEmpty()) return (FALSE);
    }


    /* Hack -- Require compatible "discount" fields */
    if (o_ptr->discount != j_ptr->discount)
    {

        /* Both are (different) special inscriptions */
        if ((o_ptr->discount >= INSCRIP_NULL) &&
            (j_ptr->discount >= INSCRIP_NULL))
        {
            /* Normally require matching inscriptions */
            return (FALSE);
        }

        /* One is a special inscription, one is a discount or nothing */
        else if ((o_ptr->discount >= INSCRIP_NULL) ||
                 (j_ptr->discount >= INSCRIP_NULL))
        {
            /* Normally require matching inscriptions */
            if (!stack_force_notes) return (FALSE);

            /* Hack -- Never merge a special inscription with a discount */
            if ((o_ptr->discount > 0) && (j_ptr->discount > 0)) return (0);
        }

        /* One is a discount, one is a (different) discount or nothing */
        else
        {
            /* Normally require matching discounts */
            if (!stack_force_costs) return (FALSE);
        }
    }

    /* Maximal "stacking" limit */
    if (total >= MAX_STACK_SIZE) return (FALSE);

    /* They match, so they must be similar */
    return (TRUE);
}


/*
 * Allow one item to "absorb" another, assuming they are similar.
 *
 * The blending of the "note" field assumes that either (1) one has an
 * inscription and the other does not, or (2) neither has an inscription.
 * In both these cases, we can simply use the existing note, unless the
 * blending object has a note, in which case we use that note.
 *
 * The blending of the "discount" field assumes that either (1) one is a
 * special inscription and one is nothing, or (2) one is a discount and
 * one is a smaller discount, or (3) one is a discount and one is nothing,
 * or (4) both are nothing.  In all of these cases, we can simply use the
 * "maximum" of the two "discount" fields.
 *
 * These assumptions are enforced by the "object_similar()" code.
 */
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    int total = o_ptr->number + j_ptr->number;

    /* Special case: Gold */
    if (o_ptr->tval == TV_GOLD)
    {
        /* Paranoia. Prevent overflow */
        long gold = (long)(o_ptr->pval) + (long)(j_ptr->pval);

        /* Check bounds */
        if (gold > SHRT_MAX) gold = SHRT_MAX;

        /* Set the new amount of gold */
        o_ptr->pval = (s16b)gold;

        /* Done */
        return;
    }

    /* Add together the item counts - should never happen*/
    o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

    /* Hack -- Blend "known" status */
    if (j_ptr->is_known()) o_ptr->mark_known(j_ptr->is_aware());

    /* Hack -- Blend store status */
    if (j_ptr->ident & (IDENT_STORE)) o_ptr->ident |= (IDENT_STORE);

    /* Hack -- Blend "mental" status */
    if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

    /* Hack -- Blend "notes" */
    if (!j_ptr->inscription.isEmpty()) o_ptr->inscription = j_ptr->inscription;

    /* Mega-Hack -- Blend "discounts" */
    if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;

    /* Hack -- if rods are stacking, re-calculate the
     * pvals (maximum timeouts) and current timeouts together.
     */
    if (o_ptr->tval == TV_ROD)
    {
        o_ptr->pval = total * k_ptr->pval;
        o_ptr->timeout += j_ptr->timeout;
    }

    /* Hack -- if wands or staffs are stacking, combine the charges. */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
    {
        o_ptr->pval += j_ptr->pval;
    }

    /* Combine the histories */
    stack_histories(o_ptr, j_ptr);
}



/*
 * Prepare an object `dst` representing `amt` objects,  based on an existing
 * object `src` representing at least `amt` objects.
 *
 * Note that the function distribute_charges will need to be called
 * for rods, wands, and staves if a stack of objects is being separated.
 */
void object_copy_amt(object_type *dst, object_type *src, int amt)
{

    /* Get a copy of the object */
    dst->object_copy(src);

    /* Modify quantity */
    dst->number = amt;

    /*
     * If the item has charges/timeouts, set them to the correct level
     * too. We split off the same amount as distribute_charges.
     */
    if (src->tval == TV_WAND || src->tval == TV_STAFF)
    {
        dst->pval = src->pval * amt / src->number;
    }

    if (src->tval == TV_ROD)
    {
        int max_time = dst->pval * amt;

        if (src->timeout > max_time)
            dst->timeout = max_time;
        else
            dst->timeout = src->timeout;
    }

}


/*
 * Let the floor carry an object, deleting old squelched items if necessary
 */
s16b floor_carry(int y, int x, object_type *j_ptr)
{
    int n = 0;

    s16b o_idx;

    s16b this_o_idx, next_o_idx = 0;


    /* Scan objects in that grid for combination */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for combination */
        if (object_similar(o_ptr, j_ptr) && ((o_ptr->number + j_ptr->number) < MAX_STACK_SIZE))
        {
            /* Combine the items */
            object_absorb(o_ptr, j_ptr);

            /* Result */
            return (this_o_idx);
        }

        /* Count objects */
        n++;
    }

    /* Option -- disallow stacking */
    if ((birth_no_stacking) && n) return (0);

    /* The stack is already too large */
    if (n > MAX_FLOOR_STACK) return (0);

    /* Make an object */
    o_idx = o_pop();

    /* Success */
    if (o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[o_idx];

        /* Structure Copy */
        o_ptr->object_copy(j_ptr);

        /* Location */
        o_ptr->iy = y;
        o_ptr->ix = x;

        /* Forget monster */
        o_ptr->held_m_idx = 0;

        /* Link the object to the pile */
        o_ptr->next_o_idx = dungeon_info[y][x].object_idx;

        /* Link the floor to the object */
        dungeon_info[y][x].object_idx = o_idx;

        /* Rearrange to reflect squelching */
        rearrange_stack(y, x);

        /* Notice */
        note_spot(y, x);

        /* Redraw */
        light_spot(y, x);


        /*If the player can see the drop, mark the lore*/
        if (player_can_see_bold(y,x))
        {
            /*Mark the feature lore*/
            feature_lore *f_l_ptr = &f_l_list[dungeon_info[y][x].feature_idx];
            f_l_ptr->f_l_flags1 |= (FF1_DROP);
        }
    }

    /* Result */
    return (o_idx);
}


/*
 * Find the coordinates of an object in to floor similar to j_ptr or an empty grid. We try to find a grid
 * close to the given point (oy, ox) inside a circle of radius max_dist
 * Return TRUE if we found the grid, in which case the point is updated.
 */
static bool find_similar_object_or_empty_grid(object_type *j_ptr, int *oy, int *ox, int max_dist)
{
    int dy, dx;
    /* Minimum distance to a similar object */
    int d_comb = max_dist * 2;
    int y_comb = 0, x_comb = 0;
    /* Minimum distance to an empty grid */
    int d_empty = max_dist * 2;
    int y_empty = 0, x_empty = 0;

    /* Scan a square of length = max_dist * 2 + 1 */
    for (dy = -max_dist; dy <= max_dist; dy++)
    {
        for (dx = -max_dist; dx <= max_dist; dx++)
        {
            int d;

            /* Get coordinates */
            int y = *oy + dy;
            int x = *ox + dx;

            /* Ignore annoying locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Get the distance to the center */
            d = distance(y, x, *oy, *ox);

            /*
             * Validate grid according to:
             * 	it's inside the circle
             * 	it's projectable
             * 	it's capable of contain objects
             * 	it's passable (for effects that disable movement)
             * 	it's free of traps
             * 	it doesn't contain a terrain feature dangerous for the object
             */
            if ((d <= max_dist) && generic_los(*oy, *ox, y, x, CAVE_PROJECT) &&
                cave_ff1_match(y, x, FF1_DROP) && cave_passable_bold(y, x))// &&
                //!cave_any_trap_bold(y, x) && !object_hates_location(y, x, j_ptr))
            {
                /* Get the first object index in that grid */
                int o_idx = dungeon_info[y][x].object_idx;

                /* The grid is empty */
                if (o_idx == 0)
                {
                    /* Update the minimum distance to an empty grid */
                    /* Randomize if the distances are equal */
                    if ((d < d_empty) || ((d == d_empty) && one_in_(2)))
                    {
                        /* Save the distance */
                        d_empty = d;
                        /* Save the coordinates */
                        y_empty = y;
                        x_empty = x;
                    }
                }

                /* Find a similar object in the stack */
                while (o_idx)
                {
                    /* Get the object */
                    object_type *o_ptr = &o_list[o_idx];

                    /* It's similar? */
                    if (object_similar(j_ptr, o_ptr))
                    {
                        /* Update the minimum distance to a similar object */
                        if (d < d_comb)
                        {
                            /* Save the distance */
                            d_comb = d;
                            /* Save the coordinates */
                            y_comb = y;
                            x_comb = x;
                        }

                        /* Done with this grid */
                        break;
                    }
                    /* Object is different */
                    else
                    {
                        /* Get the next object index */
                            o_idx = o_ptr->next_o_idx;
                    }
                }
            }
        }
    }

    /* A similar object is closer than an empty grid */
    if (d_comb < d_empty)
    {
        /* Return false if coordinates are zero */
        if ((!y_comb) && (!x_comb)) return (FALSE);

        /* Return the coordinates */
        *oy = y_comb;
        *ox = x_comb;


        return (TRUE);
    }

    /* An empty grid is closer than a similar object */
    if (d_empty < d_comb)
    {
        /* Return false if coordinates are zero */
        if ((!y_empty) && (!x_empty)) return (FALSE);

        /* Return the coordinates */
        *oy = y_empty;
        *ox = x_empty;
        return (TRUE);
    }

    /* The distances are equal. Check if we found something */
    if (d_comb < max_dist * 2)
    {
        /* Return the coordinates */
        *oy = y_comb;
        *ox = x_comb;
        return (TRUE);
    }

    /* Couldn't find a suitable grid */
    return (FALSE);
}


/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
 *
 * This function takes a parameter "chance".  This is the percentage
 * chance that the item will "disappear" instead of drop.  If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * Hack -- this function uses "chance" to determine if it should produce
 * some form of "description" of the drop event (under the player).
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed.  Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 *
 * Returns true if the object was dropped, false if it failed or was destroyed.
 */
bool drop_near(object_type *j_ptr, int chance, int y, int x)
{
    int i, k, d, s;

    int bs, bn;
    int by, bx;
    int dy, dx;
    int ty, tx;

    object_type *o_ptr;

    QString o_name;

    bool flag = FALSE;

    bool plural = FALSE;

    /* Extract plural */
    if (j_ptr->number != 1) plural = TRUE;

    /* Describe object */
    o_name = object_desc(j_ptr, ODESC_BASE);

    /* Handle normal "breakage" */
    if (!j_ptr->is_artifact() && (rand_int(100) < chance))
    {
        /* Message */
        if (character_dungeon)
        {
            QString msg;

            if (plural) msg = (QString("The %1 disappear.") .arg(o_name));
            else msg = (QString("The %1 disappears.") .arg(o_name));

            message(msg);
        }


        /* Debug */
        if (cheat_hear) message(QString("Breakage (breakage)."));

        p_ptr->redraw |= (PR_WIN_OBJLIST);

        /* Failure */
        return (FALSE);
    }

    /* Score */
    bs = -1;

    /* Picker */
    bn = 0;

    /* Default */
    by = y;
    bx = x;

    /* Special case: Gold. Find a suitable grid */
    if ((j_ptr->tval == TV_GOLD) && find_similar_object_or_empty_grid(j_ptr, &by, &bx, 5))
    {
        flag = TRUE;
    }
    /* Normal case */
    else
    {
        /* Scan local grids */
        for (dy = -3; dy <= 3; dy++)
        {
            /* Scan local grids */
            for (dx = -3; dx <= 3; dx++)
            {
                bool comb = FALSE;

                /* Calculate actual distance */
                d = (dy * dy) + (dx * dx);

                /* Ignore distant grids */
                if (d > 10) continue;

                /* Location */
                ty = y + dy;
                tx = x + dx;

                /* Skip illegal grids */
                if (!in_bounds_fully(ty, tx)) continue;

                /* Require line of sight */
                if (!los(y, x, ty, tx)) continue;

                /* Require a grid suitable for drops */
                if (!(cave_ff1_match(ty, tx, FF1_DROP) &&
                            cave_passable_bold(ty, tx))) continue;

                /* Ignore traps and glyphs */
                if (cave_any_trap_bold(ty, tx)) continue;

                // Ignore hidden objects
                if (cave_hidden_object_bold(ty, tx)) continue;

                /* Ignore dangerous locations */
                if (object_hates_location(ty, tx, j_ptr)) continue;

                /* No objects */
                k = 0;

                /* Scan objects in that grid */
                for (o_ptr = get_first_object(ty, tx); o_ptr; o_ptr = get_next_object(o_ptr))
                {
                    /* Check for possible combination */
                    if (object_similar(o_ptr, j_ptr)) comb = TRUE;

                    /* Count objects */
                    k++;
                }

                /* Add new object */
                if (!comb) k++;

                /* Option -- disallow stacking */
                if (birth_no_stacking && (k > 1)) continue;

                /* Paranoia */
                if (k > MAX_FLOOR_STACK) continue;

                /* Calculate score */
                s = 1000 - (d + k * 5);

                /* Skip bad values */
                if (s < bs) continue;

                /* New best value */
                if (s > bs) bn = 0;

                /* Apply the randomizer to equivalent values */
                if ((++bn >= 2) && (rand_int(bn) != 0)) continue;

                /* Keep score */
                bs = s;

                /* Track it */
                by = ty;
                bx = tx;

                /* Okay */
                flag = TRUE;
            }
        }
    }

    /* Handle lack of space */
    if (!flag && !j_ptr->is_artifact())
    {
        /* Message */
        if (character_dungeon)
        {
            QString msg;

            if (plural) msg = (QString("The %1 disappear.") .arg(o_name));
            else msg = (QString("The %1 disappears.") .arg(o_name));

            message(msg);
        }

        /* Debug */
        if (cheat_hear) message(QString("Breakage (no floor space)."));

        /* Failure */
        return (FALSE);
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

        /* Random locations */
        else
        {
            ty = rand_int(p_ptr->cur_map_hgt);
            tx = rand_int(p_ptr->cur_map_wid);
        }

        /* Require grid suitable for drops */
        if (!cave_clean_bold(ty, tx)) continue;

        /* Bounce to that location */
        by = ty;
        bx = tx;

        /* Okay */
        flag = TRUE;
    }

    /* Give it to the floor */
    if (!floor_carry(by, bx, j_ptr))
    {
        /* Message */
        if (character_dungeon)
        {
            QString msg;

            if (plural) msg = (QString("The %1 disappear.") .arg(o_name));
            else msg = (QString("The %1 disappears.") .arg(o_name));

            message(msg);
        }

        /* Debug */
        if (cheat_hear) message(QString("Breakage (too many objects)."));

        /* Hack -- Preserve artifacts */
        a_info[j_ptr->art_num].a_cur_num = 0;

        /* Failure */
        return (FALSE);
    }

    /* Mega-Hack -- no message if "dropped" by player */
    /* Message when an object falls under the player */
    if (chance && (dungeon_info[by][bx].monster_idx < 0))
    {
        /* Hack -- Only one message per turn */
        static long last_turn = -1;

        /* Compare current turn with the one we saved before */
        if (p_ptr->game_turn != last_turn)
        {
            /* Save the turn */
            last_turn = p_ptr->game_turn;

            /* Message */
            message(QString("You feel something roll beneath your feet."));
        }
    }

    p_ptr->redraw |= (PR_WIN_OBJLIST);

    return (TRUE);
}

/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
    object_type *o_ptr = &inventory[item];

    QString o_name;

    if (o_ptr->is_artifact() && o_ptr->is_known())
    {
        /* Get a description */
        o_name = object_desc(o_ptr, ODESC_FULL);

        /* Print a message */
        message(QString("You no longer have the %1 (%2).") .arg(o_name) .arg(index_to_label(item)));
    }
    else
    {
        /* Get a description */
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Print a message */
        message(QString("You have %1 (%2).") .arg(o_name) .arg(index_to_label(item)));
    }
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(int item, int num)
{
    object_type *o_ptr = &inventory[item];

    /* Apply */
    num += o_ptr->number;

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= o_ptr->number;

    /* Change the number and weight */
    if (num)
    {
        /* Add the number */
        o_ptr->number += num;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Recalculate mana XXX */
        p_ptr->update |= (PU_MANA);

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    }
}


/*
 * Returns in tag_num the numeric value of the inscription tag associated with
 * an object in the inventory.
 * Valid tags have the form "@n" or "@xn" where "n" is a number (0-9) and "x"
 * is cmd. If cmd is 0 then "x" can be anything.
 * Returns FALSE if the object doesn't have a valid tag.
 */
int get_tag_num(int o_idx, byte *tag_num)
{
    object_type *o_ptr = &inventory[o_idx];
    QString inscrip = o_ptr->inscription;
    /* Ignore empty objects */
    if (!o_ptr->k_idx) return FALSE;

    /* Ignore objects without notes */
    if (o_ptr->inscription.isEmpty()) return FALSE;

    while (inscrip.contains('@'))
    {
        /* Find the first '@' */
        int first_index = inscrip.indexOf('@');

        // clear the '@'
        inscrip.remove(first_index, 1);

        //Paranoia
        if (inscrip.length() < first_index) continue;

        QChar s = inscrip[first_index];

        /* Found "@n"? */
        if (s.isDigit())
        {
            /* Convert to number */
            *tag_num = s.digitValue();
            return TRUE;
        }

        if (s.isLetter())
        {
            int convert = letter_to_number(s);
            if ((convert >=0) && (convert < 10))
            {
                *tag_num = convert;
                return (TRUE);
            }
        }
    }

    return FALSE;
}


/*
 * Returns the number of quiver units an object will consume when it's stored in the quiver.
 * Every 99 quiver units we consume an inventory slot
 */
int quiver_space_per_unit(object_type *o_ptr)
{
    return (o_ptr->is_ammo() ? 1: 5);
}



/*
 * Save the size of the quiver.
 */
void save_quiver_size(void)
{
    int count, i;
    object_type *i_ptr;

    /*
     * Items in the quiver take up space which needs to be subtracted
     * from space available elsewhere.
     */
    count = 0;

    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        /* Get the item */
        i_ptr = &inventory[i];

        /* Ignore empty. */
        if (!i_ptr->k_idx) continue;

        /* Tally up missiles. */
        count += i_ptr->number * quiver_space_per_unit(i_ptr);
    }

    p_ptr->quiver_slots = ((count + (OBJ_MAX_STACK - 1)) / OBJ_MAX_STACK);
    p_ptr->quiver_remainder = count % OBJ_MAX_STACK;
}

/**
 * Compare ammunition from slots (0-9); used for sorting.
 *
 * \returns -1 if slot1 should come first, 1 if slot2 should come first, or 0.
 */
int compare_ammo(int slot1, int slot2)
{
    (void)slot1;
    (void)slot2;
    /* Right now there is no sorting criteria */
    return 0;
}


/*
 * Returns the quiver group associated to an object. Defaults to throwing weapons
 */
byte quiver_get_group(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
        case TV_BOLT: return (QUIVER_GROUP_BOLTS);
        case TV_ARROW: return (QUIVER_GROUP_ARROWS);
        case TV_SHOT: return (QUIVER_GROUP_SHOTS);
    }

    return (QUIVER_GROUP_THROWING_WEAPONS);
}


/*
 * Sort ammo in the quiver.  If requested, track the given slot and return
 * its final position.
 *
 * Items marked with inscriptions of the form "@ [f] [any digit]"
 * ("@f4", "@4", etc.) will be locked. It means that the ammo will be always
 * fired using that digit. Different locked ammo can share the same tag.
 * Unlocked ammo can be fired using its current pseudo-tag (shown in the
 * equipment window). In that case the pseudo-tag can change depending on
 * ammo consumption. -DG- (based on code from -LM- and -BR-)
 */
int sort_quiver(int slot)
{
    int i, j, k;

    s32b i_value;
    byte i_group;

    object_type *i_ptr;
    object_type *j_ptr;

    bool flag = FALSE;
    byte tag;

    /* This is used to sort locked and unlocked ammo */
    struct ammo_info_type {
        byte group;
        s16b idx;
        byte tag;
        s32b value;
    } ammo_info[QUIVER_SIZE];
    /* Position of the first locked slot */
    byte first_locked;
    /* Total size of the quiver (unlocked + locked ammo) */
    byte quiver_size;

    /*
     * This will hold the final ordering of ammo slots.
     * Note that only the indexes are stored
     */
    s16b sorted_ammo_idxs[QUIVER_SIZE];

    /*
     * Reorder the quiver.
     *
     * First, we sort the ammo *indexes* in two sets, locked and unlocked
     * ammo. We use the same table for both sets (ammo_info).
     * Unlocked ammo goes in the beginning of the table and locked ammo
     * later.
     * The sets are merged later to produce the final ordering.
     * We use "first_locked" to determine the bound between sets. -DG-
     */
    first_locked = quiver_size = 0;

    /* Traverse the quiver */
    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        /* Get the object */
        i_ptr = &inventory[i];

        /* Ignore empty objects */
        if (!i_ptr->k_idx) continue;

        /* Get the value of the object */
        i_value = object_value(i_ptr);

        /* Note that we store all throwing weapons in the same group */
        i_group = quiver_get_group(i_ptr);

        /* Get the real tag of the object, if any */
        if (get_tag_num(i, &tag))
        {
            /* Determine the portion of the table to be used */
            j = first_locked;
            k = quiver_size;
        }
        /*
         * If there isn't a tag we use a special value
         * to separate the sets.
         */
        else
        {
            tag = 10;
            /* Determine the portion of the table to be used */
            j = 0;
            k = first_locked;

            /* We know that all locked ammo will be displaced */
            ++first_locked;
        }

        /* Search for the right place in the table */
        for (; j < k; j++)
        {
            /* Get the other ammo */
            j_ptr = &inventory[ammo_info[j].idx];

            /* Objects sort by increasing group */
            if (i_group < ammo_info[j].group) break;
            if (i_group > ammo_info[j].group) continue;
            /*
             * Objects sort by increasing tag.
             * Note that all unlocked ammo have the same tag (10)
             * so this step is meaningless for them -DG-
             */
            if (tag < ammo_info[j].tag) break;
            if (tag > ammo_info[j].tag) continue;

            /* Objects sort by decreasing tval */
            if (i_ptr->tval > j_ptr->tval) break;
            if (i_ptr->tval < j_ptr->tval) continue;

            /* Non-aware items always come last */
            if (!i_ptr->is_aware()) continue;
            if (!j_ptr->is_aware()) break;

            /* Objects sort by increasing sval */
            if (i_ptr->sval < j_ptr->sval) break;
            if (i_ptr->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!i_ptr->is_known()) continue;
            if (!j_ptr->is_known()) break;

            /* Objects sort by decreasing value */
            if (i_value > ammo_info[j].value) break;
            if (i_value < ammo_info[j].value) continue;
        }

        /*
         * We found the place. Displace the other slot
         * indexes if necessary
         */
        for (k = quiver_size; k > j; k--)
        {
            COPY(&ammo_info[k], &ammo_info[k - 1],
                struct ammo_info_type);
        }

        /* Cache some data from the slot in the table */
        ammo_info[j].group = i_group;
        ammo_info[j].idx = i;
        ammo_info[j].tag = tag;

        /* We cache the value of the object too */
        ammo_info[j].value = i_value;

        /* Update the size of the quiver */
        ++quiver_size;
    }

    /*
     * Second, we merge the two sets to find the final ordering of the
     * ammo slots.
     * What this step really does is to place unlocked ammo in
     * the slots which aren't used by locked ammo. Again, we only work
     * with indexes in this step.
     */
    i = 0;
    j = first_locked;
    tag = k = 0;

    /* Compare ammo between the sets */
    while ((i < first_locked) && (j < quiver_size))
    {
        /* Groups are different. Add the smallest first */
        if (ammo_info[i].group > ammo_info[j].group)
        {
            sorted_ammo_idxs[k++] = ammo_info[j++].idx;
            /* Reset the tag */
            tag = 0;
        }

        /* Groups are different. Add the smallest first */
        else if (ammo_info[i].group < ammo_info[j].group)
        {
            sorted_ammo_idxs[k++] = ammo_info[i++].idx;
            /* Reset the tag */
            tag = 0;
        }

        /*
         * Same group, and the tag is unlocked. Add some unlocked
         * ammo first
         */
        else if (tag < ammo_info[j].tag)
        {
            sorted_ammo_idxs[k++] = ammo_info[i++].idx;
            /* Increment the pseudo-tag */
            ++tag;
        }
        /*
         * The tag is locked. Perhaps there are several locked
         * slots with the same tag too. Add the locked ammo first
         */
        else
        {
            /*
             * The tag is incremented only at the first occurrence
             * of that value
             */
            if (tag == ammo_info[j].tag)
            {
                ++tag;
            }
            /* But the ammo is always added */
            sorted_ammo_idxs[k++] = ammo_info[j++].idx;
        }
    }

    /* Add remaining unlocked ammo, if necessary */
    while (i < first_locked)
    {
        sorted_ammo_idxs[k++] = ammo_info[i++].idx;
    }

    /* Add remaining locked ammo, if necessary */
    while (j < quiver_size)
    {
        sorted_ammo_idxs[k++] = ammo_info[j++].idx;
    }

    /* Determine if we have to reorder the real ammo */
    for (k = 0; k < quiver_size; k++)
    {
        if (sorted_ammo_idxs[k] != (QUIVER_START + k))
        {
            flag = TRUE;

            break;
        }
    }

    /* Reorder the real quiver, if necessary */
    if (flag)
    {
        /* Temporary copy of the quiver */
        object_type quiver[QUIVER_SIZE];

        /*
         * Copy the real ammo into the temporary quiver using
         * the new order
         */
        for (i = 0; i < quiver_size; i++)
        {
            /* Get the original slot */
            k = sorted_ammo_idxs[i];

            /* Note the indexes */
            quiver[i].object_copy(&inventory[k]);

            /*
             * Hack - Adjust the temporary slot if necessary.
             * Note that the slot becomes temporarily negative
             * to avoid multiple matches (indexes are positive).
             */
            if (slot == k) slot = 0 - (QUIVER_START + i);
        }

        /*
         * Hack - The loop has ended and slot was changed only once
         * like we wanted. Now we make slot positive again. -DG-
         */
        if (slot < 0) slot = 0 - slot;

        /*
         * Now dump the temporary quiver (sorted) into the real quiver
         */
        for (i = 0; i < quiver_size; i++)
        {
            inventory[QUIVER_START + i].object_copy(&quiver[i]);
        }

        /* Clear unused slots */
        for (i = QUIVER_START + quiver_size; i < QUIVER_END; i++)
        {
            inventory[i].object_wipe();
        }

        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_EQUIPMENT);

        /* Message */
        if (!slot) message(QString("You reorganize your quiver."));
    }

    return (slot);
}


/*
 * Shifts ammo at or above the item slot towards the end of the quiver, making
 * room for a new piece of ammo.
 */
void open_quiver_slot(int slot)
{
    int i, pref;
    int dest = QUIVER_END - 1;

    /* This should only be used on ammunition */
    if (slot < QUIVER_START) return;

    /* Quiver is full */
    if (inventory[QUIVER_END - 1].k_idx) return;

    /* Find the first open quiver slot */
    while (inventory[dest].k_idx) dest++;

    /* Swap things with the space one higher (essentially moving the open space
     * towards our goal slot. */
    for (i = dest - 1; i >= slot; i--)
    {
        /* If we have an item with an inscribed location (and it's in */
        /* that location) then we won't move it. */
        pref = get_inscribed_ammo_slot(&inventory[i]);
        if (i != slot && pref && pref == i) continue;

        /* Copy the item up and wipe the old slot */
        COPY(&inventory[dest], &inventory[i], object_type);
        dest = i;
        inventory[dest].object_wipe();
    }
}


/*
 * Returns TRUE if the specified number of objects of type o_ptr->k_idx can be
 * held in the quiver.
 * Hack: if you are moving objects from the inventory to the quiver pass the
 * inventory slot occupied by the object in "item". This helps us to determine
 * if we have one free inventory slot more. You can pass -1 to ignore this feature.
 */
bool quiver_carry_okay(object_type *o_ptr, int num, int item)
{
    int i;
    int ammo_num = 0;
    int have;
    int need;

    /* Paranoia */
    if ((num <= 0) || (num > o_ptr->number)) num = o_ptr->number;

    /* Count ammo in the quiver */
    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        /* Get the object */
        object_type *i_ptr = &inventory[i];

        /* Ignore empty objects */
        if (!i_ptr->k_idx) continue;

        /* Increment the ammo count */
        ammo_num += (i_ptr->number * quiver_space_per_unit(i_ptr));
    }

    /* Add the requested amount of objects to be put in the quiver */
    ammo_num += (num * quiver_space_per_unit(o_ptr));

    /* We need as many free inventory as: */
    need = (ammo_num + (OBJ_MAX_STACK - 1)) / OBJ_MAX_STACK;

    /* Calculate the number of available inventory slots */
    have = INVEN_PACK - p_ptr->inven_cnt;

    /* If we are emptying an inventory slot we have one free slot more */
    if ((item >= 0) && (item < INVEN_PACK) && (num == o_ptr->number)) ++have;

    /* Compute the result */
    return (need <= have);
}


/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(int item)
{
    object_type *o_ptr = &inventory[item];
    int i, j, slot, limit;

    /* Save a possibly new quiver size */
    if (item >= QUIVER_START) save_quiver_size();

    /* Only optimize real items which are empty */
    if (!o_ptr->k_idx || o_ptr->number) return;

    /* Items in the pack are treated differently from other items */
    if (item < INVEN_WIELD)
    {
        p_ptr->update |= (PU_BONUS);
        p_ptr->redraw |= PR_WIN_INVENTORY;
        limit = INVEN_MAX_PACK;
    }

    /* Items in the quiver and equipped items are (mostly) treated similarly */
    else
    {
        p_ptr->equip_cnt--;
        p_ptr->redraw |= PR_WIN_EQUIPMENT;
        limit = item >= QUIVER_START ? QUIVER_END : 0;
    }

    /* If the item is equipped (but not in the quiver), there is no need to */
    /* slide other items. Bonuses and such will need to be recalculated */
    if (!limit)
    {
        /* Erase the empty slot */
        inventory[item].object_wipe();

        /* Recalculate stuff */
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_TORCH);
        p_ptr->update |= (PU_MANA);

        return;
    }

    /* Slide everything down */
    for (j = item, i = item + 1; i < limit; i++)
    {
        if (limit == QUIVER_END && inventory[i].k_idx)
        {
            /* If we have an item with an inscribed location (and it's in */
            /* that location) then we won't move it. */
            slot = get_inscribed_ammo_slot(&inventory[i]);
            if (slot && slot == i)
                continue;
        }
        COPY(&inventory[j], &inventory[i], object_type);
        j = i;
    }

    /* Reorder the quiver if necessary */
    if (item >= QUIVER_START) sort_quiver(0);

    /* Wipe the left-over object on the end */
    inventory[j].object_wipe();

    /* Inventory has changed, so disable repeat command */
    cmd_disable_repeat();
}


/*
 * Describe an item in the inventory.
 */
void floor_item_describe(int item)
{
    object_type *o_ptr = &o_list[item];

    QString o_name;

    /* Get a description */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Print a message */
    message(QString("You see %1.") .arg(o_name));
}


/*
 * Increase the "number" of an item on the floor
 */
void floor_item_increase(int item, int num)
{
    object_type *o_ptr = &o_list[item];

    /* Apply */
    num += o_ptr->number;

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= o_ptr->number;

    /* Change the number */
    o_ptr->number += num;
}


/*
 * Optimize an item on the floor (destroy "empty" items)
 */
void floor_item_optimize(int item)
{
    object_type *o_ptr = &o_list[item];

    /* Paranoia -- be sure it exists */
    if (!o_ptr->k_idx) return;

    /* Only optimize empty items */
    if (o_ptr->number) return;

    /* Delete the object */
    delete_object_idx(item);
}


/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(object_type *o_ptr)
{
    /* Empty slot? */
    if (p_ptr->inven_cnt < INVEN_MAX_PACK) return TRUE;

    /* Check if it can stack */
    if (inven_stack_okay(o_ptr, 0)) return TRUE;

    /* Nope */
    return FALSE;
}


/*
 * Check to see if an item is stackable in the quiver
 */
bool quiver_stack_okay(object_type *o_ptr)
{
    int j;

    for (j = QUIVER_START; j < QUIVER_END; j++)
    {
        object_type *j_ptr = &inventory[j];

        /* Skip equipped items and non-objects */
        if (!j_ptr->k_idx) continue;

        if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

        /* Return true if the two items can be combined */
        if (object_similar(j_ptr, o_ptr)) return (TRUE);
    }
    return (FALSE);
}


/*
 * Check to see if an item is stackable in the inventory
 */
bool inven_stack_okay(object_type *o_ptr, int set_limit)
{
    /* Similar slot? */
    int j;

    /* If our pack is full and we're adding too many missiles, there won't be
     * enough room in the quiver, so don't check it. */
    int limit;

    /* Stop at the specified point */
    if (set_limit) limit = set_limit;

    else if (!pack_is_full())
        /* The pack has more room */
        limit = ALL_INVEN_TOTAL;
    else if (p_ptr->quiver_remainder == 0)
        /* Quiver already maxed out */
        limit = INVEN_PACK;
    else if (p_ptr->quiver_remainder + o_ptr->number > OBJ_MAX_STACK)
        /* Too much new ammo */
        limit = INVEN_PACK;
    else
        limit = ALL_INVEN_TOTAL;

    for (j = 0; j < limit; j++)
    {
        object_type *j_ptr = &inventory[j];

        /* Skip equipped items and non-objects */
        if (j >= INVEN_PACK && j < QUIVER_START) continue;
        if (!j_ptr->k_idx) continue;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr)) return (TRUE);
    }
    return (FALSE);
}


/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", else,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined.  This may be tricky.  See "dungeon.c" for info.
 *
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
s16b quiver_carry(object_type *o_ptr)
{
    int i, j, k;
    int n = -1;

    object_type *j_ptr;

    /*paranoia, don't pick up "&nothings"*/
    if (!o_ptr->k_idx) return (-1);

    /* Hack - Don't pick up mimic objects */
    if (o_ptr->is_mimic()) return (-1);

    /* Must be ammo. */
    if (!o_ptr->is_ammo() && !is_throwing_weapon(o_ptr)) return (-1);

    /* Check for combining */
    for (j = QUIVER_START; j < QUIVER_END; j++)
    {
        j_ptr = &inventory[j];

        /* Skip non-objects */
        if (!j_ptr->k_idx) continue;

        /* Hack -- track last item */
        n = j;

        if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr))
        {
            /* Combine the items */
            object_absorb(j_ptr, o_ptr);

            /*Mark it to go in the quiver */
            o_ptr->use_verify[AUTO_WIELD_QUIVER] = TRUE;

            /* Recalculate bonuses */
            p_ptr->update |= (PU_BONUS);

            /* Combine and Reorder pack */
            p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

            /* Redraw stuff */
            p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

            /* Save quiver size */
            save_quiver_size();

            /* Success */
            return (j);
        }
    }

    /* Paranoia */
    if (p_ptr->inven_cnt > INVEN_MAX_PACK) return (-1);

    /* Find an empty slot */
    for (j = QUIVER_START; j < QUIVER_END; j++)
    {
        j_ptr = &inventory[j];

        /* Use it if found */
        if (!j_ptr->k_idx) break;
    }

    /* Use that slot */
    i = j;

    /* Apply an autoinscription */
    apply_autoinscription(o_ptr);

    /* Reorder the quiver */
    if (i < QUIVER_END)
    {
        s32b o_value, j_value;

        /* Get the "value" of the item */
        o_value = object_value(o_ptr);

        /* Scan every occupied slot */
        for (j = QUIVER_START; j < QUIVER_END; j++)
        {
            j_ptr = &inventory[j];

            /* Use empty slots */
            if (!j_ptr->k_idx) break;

            /* Objects sort by decreasing type */
            if (o_ptr->tval > j_ptr->tval) break;
            if (o_ptr->tval < j_ptr->tval) continue;

            /* Non-aware (flavored) items always come last */
            if (!o_ptr->is_aware()) continue;
            if (!j_ptr->is_aware()) break;

            /* Objects sort by increasing sval */
            if (o_ptr->sval < j_ptr->sval) break;
            if (o_ptr->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!o_ptr->is_known()) continue;
            if (!j_ptr->is_known()) break;

            /* Determine the "value" of the pack item */
            j_value = object_value(j_ptr);

            /* Objects sort by decreasing value */
            if (o_value > j_value) break;
            if (o_value < j_value) continue;
        }

        /* Use that slot */
        i = j;

        /* Slide objects */
        for (k = n; k >= i; k--)
        {
            /* Hack -- Slide the item */
            inventory[k+1].object_copy(&inventory[k]);
        }

        /* Wipe the empty slot */
        inventory[i].object_wipe();
    }

    o_ptr->has_been_seen();

    /* Copy the item */
    inventory[i].object_copy(o_ptr);

    /* Get the new object */
    j_ptr = &inventory[i];

    /* Forget stack */
    j_ptr->next_o_idx = 0;

    /* Forget monster */
    j_ptr->held_m_idx = 0;

    /* Forget location */
    j_ptr->iy = j_ptr->ix = 0;

    /* No longer marked */
    j_ptr->marked = FALSE;

    /*Mark it to go in the quiver */
    j_ptr->use_verify[AUTO_WIELD_QUIVER] = TRUE;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Combine and Reorder pack */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    /* Save quiver size */
    save_quiver_size();

    /* Return the slot */
    return (i);
}


/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", else,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined.  This may be tricky.  See "dungeon.c" for info.
 *
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
s16b inven_carry(object_type *o_ptr)
{
    int i, j, k;
    int n = -1;

    object_type *j_ptr;

    /* Never pick up mimics */
    if (o_ptr->is_mimic()) return (-1);

    /* Check for combining */
    for (j = 0; j < INVEN_PACK; j++)
    {
        j_ptr = &inventory[j];

        /* Skip non-objects */
        if (!j_ptr->k_idx) continue;

        /* Hack -- track last item */
        n = j;

        if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr))
        {
            /* Combine the items */
            object_absorb(j_ptr, o_ptr);

            /* Recalculate bonuses */
            p_ptr->update |= (PU_BONUS);

            /* Redraw stuff */
            p_ptr->redraw |= PR_WIN_INVENTORY;

            /* Save quiver size */
            save_quiver_size();

            /* Success */
            return (j);
        }
    }

    /* Paranoia */
    if (p_ptr->inven_cnt > INVEN_MAX_PACK)	return (-1);

    /* Find an empty slot */
    for (j = 0; j <= INVEN_MAX_PACK; j++)
    {
        j_ptr = &inventory[j];

        /* Use it if found */
        if (!j_ptr->k_idx) break;
    }

    /* Use that slot */
    i = j;

    /* Apply an autoinscription */
    apply_autoinscription(o_ptr);

    /* Reorder the pack */
    if (i < INVEN_MAX_PACK)
    {
        s32b o_value, j_value;

        /* Get the "value" of the item */
        o_value = object_value(o_ptr);

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_MAX_PACK; j++)
        {
            j_ptr = &inventory[j];

            /* Use empty slots */
            if (!j_ptr->k_idx) break;

            /* Hack -- readable books always come first */
            if ((o_ptr->tval == cp_ptr->spell_book) &&
                (j_ptr->tval != cp_ptr->spell_book)) break;
            if ((j_ptr->tval == cp_ptr->spell_book) &&
                (o_ptr->tval != cp_ptr->spell_book)) continue;

            /* Objects sort by decreasing type */
            if (o_ptr->tval > j_ptr->tval) break;
            if (o_ptr->tval < j_ptr->tval) continue;

            /* Non-aware (flavored) items always come last */
            if (!o_ptr->is_aware()) continue;
            if (!j_ptr->is_aware()) break;

            /* Objects sort by increasing sval */
            if (o_ptr->sval < j_ptr->sval) break;
            if (o_ptr->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!o_ptr->is_known()) continue;
            if (!j_ptr->is_known()) break;

            /* Lites sort by decreasing fuel */
            if (o_ptr->tval == TV_LIGHT)
            {
                if (o_ptr->timeout > j_ptr->timeout) break;
                if (o_ptr->timeout < j_ptr->timeout) continue;
            }

            /* Determine the "value" of the pack item */
            j_value = object_value(j_ptr);

            /* Objects sort by decreasing value */
            if (o_value > j_value) break;
            if (o_value < j_value) continue;
        }

        /* Use that slot */
        i = j;

        /* Slide objects */
        for (k = n; k >= i; k--)
        {
            /* Hack -- Slide the item */
            inventory[k+1].object_copy(&inventory[k]);
        }

        /* Wipe the empty slot */
        inventory[i].object_wipe();
    }

    o_ptr->has_been_seen();

    /* Copy the item */
    inventory[i].object_copy(o_ptr);

    /* Get the new object */
    j_ptr = &inventory[i];

    /* Forget stack */
    j_ptr->next_o_idx = 0;

    /* Forget monster */
    j_ptr->held_m_idx = 0;

    /* Forget location */
    j_ptr->iy = j_ptr->ix = 0;

    /* No longer marked */
    j_ptr->marked = FALSE;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Combine and Reorder pack */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    /* Save quiver size */
    save_quiver_size();

    /* Return the slot */
    return (i);
}


/*
 * Take off (some of) a non-cursed equipment item
 *
 * Note that only one item at a time can be wielded per slot.
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground.
 *
 * Return the inventory slot into which the item is placed.
 */
s16b inven_takeoff(int item, int amt)
{
    int slot;

    object_type *o_ptr;

    object_type *i_ptr;
    object_type object_type_body;

    QString act;

    QString o_name;

    /* Get the item to take off */
    o_ptr = &inventory[item];

    /* Paranoia */
    if (amt <= 0) return (-1);

    /* Verify */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain a local object */
    i_ptr->object_copy(o_ptr);

    /* Modify quantity */
    i_ptr->number = amt;

    /* No longer in use */
    i_ptr->obj_in_use = FALSE;

    /* Modify, Optimize */
    inven_item_increase(item, -amt);
    inven_item_optimize(item);

    /* Double-check the quiver size */
    save_quiver_size();

    /* Describe the object */
    o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Carry the object */
    slot = inven_carry(i_ptr);

    /* Remove the mark to auto-wield in quiver */
    o_ptr->use_verify[AUTO_WIELD_QUIVER] = FALSE;

    /* Special handling for the quiver */
    if ((item > QUIVER_START) && (item < QUIVER_END))
    {
        message(QString("You remove %1 (%2) from your quiver.") .arg(o_name) .arg(index_to_label(slot)));

        return(slot);
    }

    /* Took off weapon */
    if ((item == INVEN_WIELD) || (item == INVEN_BOW))
    {
        if (o_ptr->is_bow()) act = "You were shooting";
        else act = "You were wielding";
    }

    /* Took off light */
    else if (item == INVEN_LIGHT)
    {
        act = "You were holding";
    }

    /* Took off something */
    else
    {
        act = "You were wearing";
    }

    /* Message */
    message(QString("%1 %2 (%3).") .arg(act) .arg(o_name) .arg(index_to_label(slot)));

    /* Return slot */
    return (slot);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(int item, int amt)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    object_type *o_ptr;

    object_type *i_ptr;
    object_type object_type_body;

    QString o_name;

    /* Get the original object */
    o_ptr = &inventory[item];

    /* Error check */
    if (amt <= 0) return;

    /* Not too many */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Take off equipment */
    if (item >= INVEN_WIELD)
    {
        /* Take off first */
        item = inven_takeoff(item, amt);

        /* Get the original object */
        o_ptr = &inventory[item];

        if (!o_ptr->k_idx) return;

        /* Describe local object */
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
    }

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain local object */
    i_ptr->object_copy(o_ptr);

    /* Distribute charges of wands, staves, or rods */
    distribute_charges(o_ptr, i_ptr, amt);

    /* Modify quantity */
    i_ptr->number = amt;

    /* No longer in use */
    i_ptr->obj_in_use = FALSE;

    /* Describe local object */
    o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    message(QString("You drop %1 (%2).") .arg(o_name)  .arg(index_to_label(item)));

    /* Drop it near the player */
    drop_near(i_ptr, 0, py, px);

    /* Modify, Describe, Optimize */
    inven_item_increase(item, -amt);
    inven_item_describe(item);
    inven_item_optimize(item);

    pack_overflow();
}


/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(void)
{
    int i, j, k;

    object_type *o_ptr;
    object_type *j_ptr;

    bool flag = FALSE;

    /* Combine the pack (backwards) */
    for (i = INVEN_PACK; i > 0; i--)
    {
        /* Get the item */
        o_ptr = &inventory[i];

        /* Skip empty items */
        if (!o_ptr->k_idx) continue;

        /* Scan the items above that item */
        for (j = 0; j < i; j++)
        {
            /* Get the item */
            j_ptr = &inventory[j];

            /* Skip empty items */
            if (!j_ptr->k_idx) continue;

            if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

            /* Can we drop "o_ptr" onto "j_ptr"? */
            if (object_similar(j_ptr, o_ptr))
            {
                /* Take note */
                flag = TRUE;

                /* Add together the item counts */
                object_absorb(j_ptr, o_ptr);

                /* Slide everything down */
                for (k = i; k < INVEN_PACK; k++)
                {
                    /* Hack -- slide object */
                    COPY(&inventory[k], &inventory[k+1], object_type);
                }

                /* Hack -- wipe hole */
                inventory[k].object_wipe();

                /* Redraw stuff */
                p_ptr->update |= (PU_BONUS);
                p_ptr->redraw |= PR_WIN_INVENTORY;

                /* Done */
                break;
            }
        }
    }

    /* Message */
    if (flag) message(QString("You combine some items in your pack."));
}


/*
 * Combine ammo in the quiver.
 */
void combine_quiver(void)
{
    int i, j, k;

    object_type *i_ptr;
    object_type *j_ptr;

    bool flag = FALSE;

    /* Combine the quiver (backwards) */
    for (i = QUIVER_END - 1; i > QUIVER_START; i--)
    {
        /* Get the item */
        i_ptr = &inventory[i];

        /* Skip empty items */
        if (!i_ptr->k_idx) continue;

        /* Scan the items above that item */
        for (j = QUIVER_START; j < i; j++)
        {
            /* Get the item */
            j_ptr = &inventory[j];

            /* Skip empty items */
            if (!j_ptr->k_idx) continue;

            if ((i_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

            /* Can we drop "i_ptr" onto "j_ptr"? */
            if (object_similar(j_ptr, i_ptr))
            {
                /* Take note */
                flag = TRUE;

                /* Add together the item counts */
                object_absorb(j_ptr, i_ptr);

                /* Slide everything down */
                for (k = i; k < (QUIVER_END - 1); k++)
                {
                    /* Hack -- slide object */
                    COPY(&inventory[k], &inventory[k+1], object_type);
                }

                /* Hack -- wipe hole */
                inventory[k].object_wipe();

                /* Done */
                break;
            }
        }
    }

    if (flag)
    {
        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_EQUIPMENT);

        /* Message */
        message(QString("You combine your quiver."));
    }
}


/*
 * Reorder items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void reorder_pack(void)
{
    int i, j, k;

    s32b o_value;
    s32b j_value;

    object_type *o_ptr;
    object_type *j_ptr;

    object_type *i_ptr;
    object_type object_type_body;

    bool flag = FALSE;


    /* Re-order the pack (forwards) */
    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Mega-Hack -- allow "proper" over-flow */
        if ((i == INVEN_PACK) && (p_ptr->inven_cnt == INVEN_PACK)) break;

        /* Get the item */
        o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Get the "value" of the item */
        o_value = object_value(o_ptr);

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_PACK; j++)
        {
            /* Get the item already there */
            j_ptr = &inventory[j];

            /* Use empty slots */
            if (!j_ptr->k_idx) break;

            /* Hack -- readable books always come first */
            if ((o_ptr->tval == cp_ptr->spell_book) &&
                (j_ptr->tval != cp_ptr->spell_book)) break;
            if ((j_ptr->tval == cp_ptr->spell_book) &&
                (o_ptr->tval != cp_ptr->spell_book)) continue;

            /* Objects sort by decreasing type */
            if (o_ptr->tval > j_ptr->tval) break;
            if (o_ptr->tval < j_ptr->tval) continue;

            /* Non-aware (flavored) items always come last */
            if (!o_ptr->is_aware()) continue;
            if (!j_ptr->is_aware()) break;

            /* Objects sort by increasing sval */
            if (o_ptr->sval < j_ptr->sval) break;
            if (o_ptr->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!o_ptr->is_known()) continue;
            if (!j_ptr->is_known()) break;

            /* Lites sort by decreasing fuel */
            if (o_ptr->tval == TV_LIGHT)
            {
                if (o_ptr->timeout > j_ptr->timeout) break;
                if (o_ptr->timeout < j_ptr->timeout) continue;
            }

            /* Determine the "value" of the pack item */
            j_value = object_value(j_ptr);

            /* Objects sort by decreasing value */
            if (o_value > j_value) break;
            if (o_value < j_value) continue;
        }

        /* Never move down */
        if (j >= i) continue;

        /* Take note */
        flag = TRUE;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Save a copy of the moving item */
        i_ptr->object_copy(&inventory[i]);

        /* Slide the objects */
        for (k = i; k > j; k--)
        {
            /* Slide the item */
            inventory[k].object_copy(&inventory[k-1]);
        }

        /* Insert the moving item */
        inventory[j].object_copy(i_ptr);

        /* Redraw stuff */
        p_ptr->redraw |= PR_WIN_INVENTORY;
    }

    /* Message */
    if (flag) message(QString("You reorder some items in your pack."));
}


/*
 *Returns the number of times in 1000 that @ will FAIL
 * - thanks to Ed Graham for the formula
 */
int get_use_device_chance(object_type *o_ptr)
{
    int lev, skill, fail;

    /* these could be globals if desired, calculated rather than stated */
    int skill_min = 10;
    int skill_max = 141;
    int diff_min = 1;
    int diff_max = 100;

    /* Extract the item level, which is the difficulty rating */
    if (o_ptr->is_artifact()) lev = a_info[o_ptr->art_num].a_level;
    else lev = k_info[o_ptr->k_idx].k_level;

    /* Chance of failure */
    skill = p_ptr->state.skills[SKILL_DEVICE];

    fail = 100 * ((skill - lev) - (skill_max - diff_min))
        / ((lev - skill) - (diff_max - skill_min));

    /* Confusion makes things twice as hard */
    if (p_ptr->timed[TMD_CONFUSED]) fail *= 2;

    /* Limit range */
    if (fail > 950) fail = 950;
    if (fail < 10) fail = 10;

    return fail;
}


/*
 * Distribute charges of rods or wands.
 *
 * o_ptr = source item
 * i_ptr = target item, must be of the same type as o_ptr
 * amt	 = number of items that are transfered
 */
void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt)
{
    /*
     * Hack -- If rods, wands or staffs are dropped, the total maximum timeout or
     * charges need to be allocated between the two stacks.   If all the items

     * are being dropped, it makes for a neater message to leave the original
     * stack's pval alone. -LM-
     */
    if ((o_ptr->tval == TV_WAND) ||
        (o_ptr->tval == TV_ROD) ||
        (o_ptr->tval == TV_STAFF))
    {

        i_ptr->pval = o_ptr->pval * amt / o_ptr->number;

        if (amt < o_ptr->number) o_ptr->pval -= i_ptr->pval;

        /* Hack -- Rods also need to have their timeouts distributed.  The
         * dropped stack will accept all time remaining to charge up to its
         * maximum.
         */
        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
        {
            if (i_ptr->pval > o_ptr->timeout)
                i_ptr->timeout = o_ptr->timeout;
            else
                i_ptr->timeout = i_ptr->pval;

            if (amt < o_ptr->number)
                o_ptr->timeout -= i_ptr->timeout;
        }
    }
}


/*
 * Reduce the number of charges in a stack of objects
 */
void reduce_charges(object_type *o_ptr, int amt)
{
    /*
     * Hack -- If rods or wand are destroyed, the total maximum timeout or
     * charges of the stack needs to be reduced, unless all the items are
     * being destroyed. -LM-
     */
    if (((o_ptr->tval == TV_WAND) ||
         (o_ptr->tval == TV_ROD) ||
         (o_ptr->tval == TV_STAFF)) &&
        (amt < o_ptr->number))
    {
        o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
    }
}


/*** Object kind lookup functions ***/


/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
    int k;

    /* Look for it */
    for (k = 1; k < z_info->k_max; k++)
    {
        object_kind *k_ptr = &k_info[k];

        /* Found a match */
        if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return (k);
    }

    /* Oops */
    pop_up_message_box(QString("Lookup_kind could not find object (%1,%2)") .arg(tval)  .arg(sval));

    /* Oops */
    return (0);
}

/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_ego(s16b tval, s16b sval, QString ego_title)
{
    int e;

    ego_title = ego_title.toLower();

    /* Look for a matching ego-entry */
    for (e = 1; e < z_info->e_max; e++)
    {
        ego_item_type *e_ptr = &e_info[e];

        /* Test if this is a legal ego-item type for this object */
        for (int j = 0; j < EGO_TVALS_MAX; j++)
        {
            /* Require identical base type */
            if (tval != e_ptr->tval[j]) continue;

            // require matching sval range
            if (sval <  e_ptr->min_sval[j]) continue;
            if (sval >  e_ptr->max_sval[j]) continue;

            QString ego_name = e_ptr->e_name.toLower();

            // Make sure the name atches
            if (!ego_name.contains(ego_title)) continue;

            // We have a match
            return (e);
        }
    }

    /* Oops */
    pop_up_message_box(QString("No ego_item %1") .arg(ego_title));

    /* Oops */
    return (0);
}



/**
 * This draws the Object Recall subwindow when displaying a particular object
 * (e.g. a helmet in the backpack, or a scroll on the ground)
 */
void display_object_idx_recall(s16b item)
{
    object_type *o_ptr = object_from_item_idx(item);

    object_info_screen(o_ptr);
}


/*
 * An "item_tester_hook" for refilling lanterns and torches
 */
bool obj_can_refill(object_type *o_ptr)
{
    const object_type *j_ptr = &inventory[INVEN_LIGHT];

    if (j_ptr->sval == SV_LIGHT_LANTERN)
    {
        /* Flasks of oil are okay */
        if (o_ptr->tval == TV_FLASK) return (TRUE);
    }

    /* Non-empty lanterns are okay */
    if ((o_ptr->tval == TV_LIGHT) &&
        (o_ptr->sval == j_ptr->sval) &&
        (o_ptr->timeout > 0))
    {
        return (TRUE);
    }

    /* Assume not okay */
    return (FALSE);
}




bool obj_is_chest(object_type *o_ptr) {return o_ptr->is_chest();}



/**
 * Determine whether an object is a chest
 *
 * \param o_ptr is the object to check
 */
bool chest_requires_disarming(object_type *o_ptr)
{
    if (!o_ptr->is_chest()) return FALSE;

    /* We don't know if it is trapped or not */
    if (!o_ptr->is_known()) return FALSE;

    /* Already disarmed. */
    if (o_ptr->pval <= 0) return FALSE;

    if (!chest_traps[o_ptr->pval]) return FALSE;

    return (TRUE);
}



/*
 * Determine whether the ammo can be fired with the current launcher.
 * If message is true, provide an error message
 */
bool ammo_can_fire(object_type *o_ptr, int item)
{
    /* Get the "bow" (if any) */
    object_type *j_ptr = &inventory[INVEN_BOW];

    if (birth_swap_weapons)
    {
        j_ptr = &inventory[INVEN_MAIN_WEAPON];
        if (!j_ptr->is_bow() && (j_ptr->tval)) return(FALSE);
    }

    /* No ammo */
    if (!o_ptr->is_ammo()) return (FALSE);

    /* No launcher */
    if (!j_ptr->tval || !p_ptr->state.ammo_tval) return (FALSE);

    /* Not within reach */
    if (!item_is_available(item, NULL, (USE_INVEN | USE_FLOOR | USE_QUIVER))) return (FALSE);

    /* Cursed quiver */
    if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver && !o_ptr->is_cursed()) return (FALSE);

    /* Wrong ammo type */
    if (o_ptr->tval != p_ptr->state.ammo_tval) return (FALSE);

    /* Success! */
    return (TRUE);
}


/*
 * See if the player has ammo that can be used with the launcher in the quiver
 */
bool has_correct_ammo(void)
{
    int i;

    /* First search the quiver */
    for (i=QUIVER_START; i < QUIVER_END; i++)
    {
        object_type *o_ptr = & inventory [i];

        if (ammo_can_fire(o_ptr, i)) return (TRUE);
    }

    /* Next, search the inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = & inventory [i];

        if (ammo_can_fire(o_ptr, i)) return (TRUE);
    }

    return (FALSE);
}


/*
 * Determine if an object has charges
 */
bool obj_has_charges(object_type *o_ptr)
{
    if (o_ptr->tval != TV_WAND && o_ptr->tval != TV_STAFF) return FALSE;

    if (o_ptr->pval <= 0) return FALSE;

    return TRUE;
}




/*** Generic utility functions ***/


/*
 * Get an o_ptr from an item number
 */
object_type *object_from_item_idx(int item)
{
    if (item >= 0)
        return &inventory[item];
    else
        return &o_list[0 - item];
}

/*
 * Does the given object target a trap?
 */
bool obj_aim_trap(object_type *o_ptr)
{
    if(!o_ptr->is_aware()) return(FALSE);

    switch (o_ptr->tval)
    {
        case TV_WAND:
        {
            if (o_ptr->sval == SV_WAND_DISARMING) return (TRUE);
            if (o_ptr->sval == SV_WAND_TRAP_DOOR_DEST)return (TRUE);
            return (FALSE);
        }
        case TV_ROD:
        {
            if (o_ptr->sval == SV_ROD_DISARMING) return (TRUE);
            return (FALSE);
        }

        default: /*fall through*/break;
    }

    /*Oops*/
    return (FALSE);
}

bool obj_needs_aim(object_kind *k_ptr)
{
    switch (k_ptr->tval)
    {
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_WAND:
        {
            /* All wands and dragon armor need aiming. */
            return (TRUE);
        }
        case TV_ROD:
        {
            /*Some rod doesn't need targeting*/
            if (((k_ptr->sval >= SV_ROD_MIN_DIRECTION) &&
                 (k_ptr->sval != SV_ROD_STAR_IDENTIFY) &&
                 (k_ptr->sval != SV_ROD_MASS_IDENTIFY)) || !k_ptr->aware)return(TRUE);
            else return (FALSE);
        }
        case TV_RING:
        {
            switch (k_ptr->sval)
            {
                case SV_RING_ACID:
                case SV_RING_FLAMES:
                case SV_RING_ICE:
                case SV_RING_LIGHTNING:
                {
                    return (TRUE);
                }
                default: return(FALSE);
            }
            break; /* compiler happiness */
        }

        default: /*fall through*/break;
    }

    // Not needed
    return (FALSE);
}

bool obj_needs_aim(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];
    return (obj_needs_aim(k_ptr));
}


/*
 * Does the given object need to be aimed?
 */
bool obj_needs_aim(object_type *o_ptr)
{

    if (o_ptr->art_num)
    {
        artifact_type *a_ptr = &a_info[o_ptr->art_num];

        switch (a_ptr->activation)
        {
            case ACT_FIRE3:
            case ACT_FROST5:
            case ACT_ELEC2:
            case ACT_BIZZARE:
            case ACT_MISSILE:
            case ACT_FIRE1:
            case ACT_FROST1:
            case ACT_LIGHTNING_BOLT:
            case ACT_ACID1:
            case ACT_ARROW:
            case ACT_STINKING_CLOUD:
            case ACT_FROST2:
            case ACT_FROST3:
            case ACT_FROST4:
            case ACT_FIRE2:
            case ACT_DRAIN_LIFE2:
            case ACT_STONE_TO_MUD:
            case ACT_TELE_AWAY:
            case ACT_CONFUSE:
            case ACT_MANA_BOLT:
            case ACT_DRAIN_LIFE1:
            {
                return(TRUE);
            }
            default: return(FALSE);
        }
    }

    return (obj_needs_aim(o_ptr->k_idx));
}


/*
 * Determine if an object is activatable
 */
bool obj_is_activatable(object_type *o_ptr)
{
    /* Check for Activation */
    if (!(o_ptr->obj_flags_3 & TR3_ACTIVATE)) return (FALSE);

    return (TRUE);
}

/*
 * Determine if an object can be activatable (is charged)
 */
bool obj_can_activate(object_type *o_ptr)
{

    if (obj_is_activatable(o_ptr))
    {
        /* Check the recharge */
        if (!o_ptr->timeout) return TRUE;
    }

    return FALSE;

}


/*
 * Verify the "okayness" of a given item.
 *
 * The item can be negative to mean "item on floor".
 */
bool get_item_okay(int item)
{
    /* Verify the item */
    return (item_tester_okay(object_from_item_idx(item), item));
}


/*
 * Get a list of "valid" item indexes.
 *
 * Fills item_list[] with items that are "okay" as defined by the
 * current item_tester_hook, etc.  mode determines what combination of
 * inventory, equipment and player's floor location should be used
 * when drawing up the list.
 *
 * Returns the number of items placed into the list.
 *
 * Maximum space that can be used is [INVEN_TOTAL + MAX_FLOOR_STACK],
 * though practically speaking much smaller numbers are likely.
 */
int scan_items(int *item_list, size_t item_list_max, int mode)
{
    bool use_inven = ((mode & USE_INVEN) ? TRUE : FALSE);
    bool use_equip = ((mode & USE_EQUIP) ? TRUE : FALSE);
    bool use_floor = ((mode & USE_FLOOR) ? TRUE : FALSE);
    bool use_quiver = ((mode & USE_QUIVER) ? TRUE : FALSE);

    int floor_list[MAX_FLOOR_STACK];
    int floor_num;

    int i;
    size_t item_list_num = 0;

    if (use_inven)
    {
        for (i = 0; i < INVEN_PACK && item_list_num < item_list_max; i++)
        {
            if (get_item_okay(i))
                item_list[item_list_num++] = i;
        }
    }

    if (use_equip)
    {
        for (i = INVEN_WIELD; i < INVEN_TOTAL && item_list_num < item_list_max; i++)
        {
            if (get_item_okay(i))
                item_list[item_list_num++] = i;
        }
    }

    if (use_quiver)
    {
        for (i = QUIVER_START; i < QUIVER_END && item_list_num < item_list_max; i++)
        {
            if (get_item_okay(i))
                item_list[item_list_num++] = i;
        }
    }

    /* Scan all non-gold objects in the grid */
    if (use_floor)
    {
        floor_num = scan_floor(floor_list, N_ELEMENTS(floor_list), p_ptr->py, p_ptr->px, 0x03);

        for (i = 0; i < floor_num && item_list_num < item_list_max; i++)
        {
            if (get_item_okay(-floor_list[i]))
                item_list[item_list_num++] = -floor_list[i];
        }
    }

    /* Forget the item_tester_tval and item_tester_hook  restrictions */
    item_tester_tval = 0;
    item_tester_hook = NULL;
    item_tester_swap = FALSE;


    return item_list_num;
}


/*
 * Check if the given item is available for the player to use.
 *
 * 'mode' defines which areas we should look at, a la scan_items().
 */
bool item_is_available(int item, bool (*tester)(object_type *), int mode)
{
    int item_list[ALL_INVEN_TOTAL + MAX_FLOOR_STACK];
    int item_num;
    int i;

    item_tester_hook = tester;
    item_tester_tval = 0;
    item_tester_swap = FALSE;
    item_num = scan_items(item_list, N_ELEMENTS(item_list), mode);

    for (i = 0; i < item_num; i++)
    {
        if (item_list[i] == item)
            return TRUE;
    }

    return FALSE;
}


/*
 * Confirm that the player has "access" to an item to use it.
 *  "USE_STORE is intended only for book browsing
 */
bool object_kind_is_available(int k_idx, int mode)
{
    object_type *o_ptr;
    int i;
    bool use_inven = ((mode & USE_INVEN) ? TRUE : FALSE);
    bool use_equip = ((mode & USE_EQUIP) ? TRUE : FALSE);
    bool use_floor = ((mode & USE_FLOOR) ? TRUE : FALSE);
    bool use_quiver = ((mode & USE_QUIVER) ? TRUE : FALSE);
    bool use_store = ((mode & USE_STORE) ? TRUE : FALSE);

    // Paranoia
    if (!p_ptr->in_store || !dungeon_info[p_ptr->py][p_ptr->px].is_store()) use_store = FALSE;

    if (use_inven)
    {
        for (i = 0; i < INVEN_PACK; i++)
        {
            o_ptr = &inventory[i];
            if (o_ptr->k_idx == k_idx) return TRUE;
        }
    }

    if (use_equip)
    {
        for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
        {
            o_ptr = &inventory[i];
            if (o_ptr->k_idx == k_idx) return TRUE;
        }
    }

    if (use_quiver)
    {
        for (i = QUIVER_START; i < QUIVER_END; i++)
        {
            o_ptr = &inventory[i];
            if (o_ptr->k_idx == k_idx) return TRUE;
        }
    }

    /* Scan all non-gold objects in the grid */
    if (use_floor)
    {
        s16b this_o_idx, next_o_idx;

        for (this_o_idx = dungeon_info[p_ptr->py][p_ptr->px].object_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            /* Get the object */
            o_ptr = &o_list[this_o_idx];

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;

            if (o_ptr->k_idx == k_idx) return TRUE;
        }
    }

    if (use_store)
    {
        int feat = dungeon_info[p_ptr->py][p_ptr->px].feature_idx;
        int store_idx = f_info[feat].f_power;
        store_type *st_ptr = &store[store_idx];

        for (i = 0; i < st_ptr->stock_num; i++)
        {
            object_type *o_ptr = &st_ptr->stock[i];
            if (o_ptr->k_idx == k_idx) return TRUE;
        }

    }

    return (FALSE);
}


bool is_throwing_weapon(object_type *o_ptr)
{

    if (o_ptr->obj_flags_3 & (TR3_THROWING)) return (TRUE);

    return (FALSE);
}


/*
 * Returns whether the pack is holding the maximum number of items. The max
 * size is INVEN_MAX_PACK, which is a macro since quiver size affects slots
 * available.
 */
bool pack_is_full(void)
{
    return inventory[INVEN_MAX_PACK - 1].k_idx ? TRUE : FALSE;
}


/*
 * Returns whether the pack is holding the more than the maximum number of
 * items. The max size is INVEN_MAX_PACK, which is a macro since quiver size
 * affects slots available. If this is true, calling pack_overflow() will
 * trigger a pack overflow.
 */
bool pack_is_overfull(void)
{
    return inventory[INVEN_MAX_PACK].k_idx ? TRUE : FALSE;
}


/*
 * Overflow an item from the pack, if it is overfull.
 */
void pack_overflow(void)
{
    int item = INVEN_MAX_PACK;
    QString o_name;
    object_type *o_ptr;

    if (!pack_is_overfull()) return;

    /* Get the slot to be dropped */
    o_ptr = &inventory[item];

    /* Disturbing */
    disturb(TRUE, TRUE);

    /* Warning */
    message(QString("Your pack overflows!"));

    /* Describe */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    message(QString("You drop %1 (%2).") .arg(o_name)  .arg(index_to_label(item)));

    /* Drop it (carefully) near the player */
    drop_near(o_ptr, 0, p_ptr->py, p_ptr->px);

    /* Modify, Describe, Optimize */
    inven_item_increase(item, -255);
    inven_item_describe(item);
    inven_item_optimize(item);

    /* Notice stuff (if needed) */
    notice_stuff();

    /* Update stuff (if needed) */
    update_stuff();

    /* Redraw stuff (if needed) */
    redraw_stuff();

}



/*
 * Mark an object's history
 */
void object_history(object_type *o_ptr, byte origin, s16b r_idx)
{
    o_ptr->origin_nature = origin;

    /* Paranoia */
    o_ptr->origin_dlvl = 0;
    o_ptr->origin_r_idx = 0;
    o_ptr->origin_m_name.clear();

    switch (origin)
    {
        case ORIGIN_DROP_KNOWN:
        {
            monster_race *r_ptr = &r_info[r_idx];

            /* Race */
            o_ptr->origin_r_idx = r_idx;

            /* Special case: player ghosts */
            if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
            {
                /* Store the name */
                o_ptr->origin_m_name = player_ghost_name;
            }

            /* Fall through */
        }
        case ORIGIN_ACQUIRE:
        case ORIGIN_DROP_UNKNOWN:
        case ORIGIN_FLOOR:
        case ORIGIN_CHEST:
        {
            o_ptr->origin_dlvl = p_ptr->depth;
            break;
        }
    }
}


/* The possible actions */

#define KEEP_O		1
#define KEEP_J		2
#define MIX		3

/*
 * Try to stack item histories
 */
void stack_histories(object_type *o_ptr, const object_type *j_ptr)
{
    int action = MIX;

    /* Histories are identical anyway */
    if ((o_ptr->origin_nature == j_ptr->origin_nature) &&
         (o_ptr->origin_dlvl == j_ptr->origin_dlvl) &&
         (o_ptr->origin_r_idx == j_ptr->origin_r_idx) &&
         (o_ptr->origin_m_name == j_ptr->origin_m_name))

    {
        action = KEEP_O;
    }

    /* Do it */
    switch (action)
    {
        case (KEEP_O):
        {
            /* Nothing to do */
            break;
        }
        case (KEEP_J):
        {
            /* Copy over the second object's history */
            o_ptr->origin_nature = j_ptr->origin_nature;
            o_ptr->origin_dlvl = j_ptr->origin_dlvl;
            o_ptr->origin_r_idx = j_ptr->origin_r_idx;
            o_ptr->origin_m_name = j_ptr->origin_m_name;
            break;
        }
        case (MIX): default:
        {
            /* Lose object history */
            o_ptr->origin_nature = ORIGIN_MIXED;
            o_ptr->origin_dlvl = 0;
            o_ptr->origin_r_idx = 0;
            o_ptr->origin_m_name.clear();
            break;
        }
    }
}

/*
 * Can only put on wieldable items
 */
bool obj_can_wear(object_type *o_ptr)
{
    s16b x = wield_slot(o_ptr);

    return ((x >= INVEN_WIELD) && x < QUIVER_END);
}
