/*
 * File: obj-util.c
 * Purpose: Object list maintenance and other object utilities
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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


#include "../s-angband.h"
#include "../../common/randname.h"
#include "../../common/tvalsval.h"
#include "../effects.h"
#include "../history.h"
#include "../monster/mon-make.h"
#include "../netserver.h"
#include "inventory.h"
#include "pval.h"
#include "../party.h"
#include "../s-spells.h"
#include "../squelch.h"
#include "../z-queue.h"


static struct object *o_list;
static s16b *o_fast;


/*
 * Hold the titles of scrolls, 6 to 14 characters each, plus quotes
 */
static char scroll_adj[MAX_TITLES][18];


static void flavor_assign_fixed(void)
{
    int i;
    struct flavor *f;

    for (f = flavors; f; f = f->next)
    {
        /* Skip random flavors */
        if (f->sval == SV_UNKNOWN) continue;

        for (i = 0; i < z_info->k_max; i++)
        {
            struct object_kind *k = &k_info[i];

            /* Skip other objects */
            if ((k->tval == f->tval) && (k->sval == f->sval))
            {
                /* Store the flavor */
                k->flavor = f;
            }
        }
    }
}


static void flavor_assign_random(byte tval)
{
    int i;
    int flavor_count = 0;
    int choice;
    struct flavor *f;

    /* Count the random flavors for the given tval */
    for (f = flavors; f; f = f->next)
    {
        if ((f->tval == tval) && (f->sval == SV_UNKNOWN))
            flavor_count++;
    }

    for (i = 0; i < z_info->k_max; i++)
    {
        /* Skip other object types */
        /* Skip objects that already are flavored */
        if ((k_info[i].tval != tval) || k_info[i].flavor) continue;

        /* Hack - Ordinary food is "boring" */
        if ((tval == TV_FOOD) && (k_info[i].sval < SV_FOOD_MIN_SHROOM))
            continue;

        if (!flavor_count) quit_fmt("Not enough flavors for tval %d.", tval);

        /* Select a flavor */
        choice = randint0(flavor_count);

        /* Find and store the flavor */
        for (f = flavors; f; f = f->next)
        {
            /* Skip other tvals */
            /* Skip assigned svals */
            if ((f->tval != tval) || (f->sval != SV_UNKNOWN)) continue;

            if (choice == 0)
            {
                /* Store the flavor */
                k_info[i].flavor = f;

                /* Mark the flavor as used */
                f->sval = k_info[i].sval;

                /* Hack - Set the scroll name if it's a scroll */
                if (tval == TV_SCROLL)
                    f->text = string_make(scroll_adj[k_info[i].sval]);

                /* One less flavor to choose from */
                flavor_count--;

                break;
            }

            choice--;
        }
    }
}


/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 2 to 8 letters long, and that no scroll is finished
 * until it attempts to grow beyond 15 letters.  The first time this
 * can happen is when the current title has 6 letters and the new word
 * has 8 letters, which would result in a 6 letter scroll title.
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 */
void flavor_init(void)
{
    int i, j;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant flavors */
    Rand_value = seed_flavor;

    flavor_assign_fixed();

    flavor_assign_random(TV_RING);
    flavor_assign_random(TV_AMULET);
    flavor_assign_random(TV_STAFF);
    flavor_assign_random(TV_WAND);
    flavor_assign_random(TV_ROD);
    flavor_assign_random(TV_FOOD);
    flavor_assign_random(TV_POTION);

    /* Scrolls (random titles, always white) */
    for (i = 0; i < MAX_TITLES; i++)
    {
        char buf[26];
        char *end = buf + 1;
        int titlelen = 0;
        int wordlen;
        bool okay = TRUE;

        my_strcpy(buf, "\"", sizeof(buf));
        wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24, name_sections);
        while (titlelen + wordlen < (int)(sizeof(scroll_adj[0]) - 3))
        {
            end[wordlen] = ' ';
            titlelen += wordlen + 1;
            end += wordlen + 1;
            wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24 - titlelen, name_sections);
        }
        buf[titlelen] = '"';
        buf[titlelen + 1] = '\0';

        /* Check the scroll name hasn't already been generated */
        for (j = 0; j < i; j++)
        {
            if (streq(buf, scroll_adj[j]))
            {
                okay = FALSE;
                break;
            }
        }

        if (okay)
            my_strcpy(scroll_adj[i], buf, sizeof(scroll_adj[0]));
        else
        {
            /* Have another go at making a name */
            i--;
        }
    }

    flavor_assign_random(TV_SCROLL);

    /* Hack -- Use the "complex" RNG */
    Rand_quick = FALSE;
}


/*
 * Reset the "visual" lists
 *
 * This is useful for switching on/off the "use_graphics" flag.
 */
void reset_visuals(void)
{
    int i;
    struct flavor *f;

    /* Extract some info about terrain features */
    for (i = 0; i < z_info->f_max; i++)
    {
        int j;
        feature_type *f_ptr = &f_info[i];

        /* Assume we will use the underlying values */
        for (j = 0; j < FEAT_LIGHTING_MAX; j++)
        {
            f_ptr->x_attr[j] = f_ptr->d_attr;
            f_ptr->x_char[j] = f_ptr->d_char;
        }
    }

    /* Extract some info about objects */
    for (i = 0; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Assume we will use the underlying values */
        k_ptr->x_attr = k_ptr->d_attr;
        k_ptr->x_char = k_ptr->d_char;
    }

    /* Extract some info about monsters */
    for (i = 0; i < z_info->r_max; i++)
    {
        /* Extract the "underlying" attr */
        r_info[i].x_attr = r_info[i].d_attr;

        /* Extract the "underlying" char */
        r_info[i].x_char = r_info[i].d_char;
    }

    /* Extract default attr/char code for flavors */
    for (f = flavors; f; f = f->next)
    {
        f->x_attr = f->d_attr;
        f->x_char = f->d_char;
    }

    /* Extract attr/chars for inventory objects (by tval) */
    for (i = 0; i < (int)N_ELEMENTS(tval_to_attr); i++)
    {
        /* Default to white */
        tval_to_attr[i] = TERM_WHITE;
    }
}


/*
 * Hack -- Remove redundant bitflags
 */
static void remove_redundant_flags(bitflag flags[OF_SIZE])
{
    /*
     * ESP evil bypasses ESP undead/demon
     *
     * Note: although orcs/trolls/giants/dragons are evil, ESP evil cannot
     * bypass ESP orc/troll/giant/dragon because of the corresponding player
     * races (a player of the Half-Orc race can be detected by ESP orc, but
     * not by ESP evil)
     */
    if (of_has(flags, OF_ESP_EVIL))
    {
        of_off(flags, OF_ESP_UNDEAD);
        of_off(flags, OF_ESP_DEMON);
    }

    /* ESP all bypasses all other ESPs */
    if (of_has(flags, OF_ESP_ALL))
    {
        bitflag f2[OF_SIZE];

        create_mask(f2, FALSE, OFT_ESP, OFT_MAX);
        of_diff(flags, f2);
        of_on(flags, OF_ESP_ALL);
    }

    /* Immunity bypasses resistance */
    if (of_has(flags, OF_IM_FIRE)) of_off(flags, OF_RES_FIRE);
    if (of_has(flags, OF_IM_COLD)) of_off(flags, OF_RES_COLD);
    if (of_has(flags, OF_IM_ELEC)) of_off(flags, OF_RES_ELEC);
    if (of_has(flags, OF_IM_ACID)) of_off(flags, OF_RES_ACID);
}


/*
 * Obtain the flags for an item
 */
static void object_flags_aux(const object_type *o_ptr, bitflag flags[OF_SIZE],
    bool no_redundancy)
{
    of_wipe(flags);

    if (!o_ptr->kind) return;

    of_copy(flags, o_ptr->flags);

    /* Hack -- Remove redundant bitflags */
    if (no_redundancy) remove_redundant_flags(flags);
}


/*
 * Obtain the flags for an item
 */
void object_flags(const object_type *o_ptr, bitflag flags[OF_SIZE])
{
    object_flags_aux(o_ptr, flags, TRUE);
}


/*
 * Obtain the flags for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, bitflag flags[OF_SIZE], bool aware)
{
    object_flags_aux(o_ptr, flags, FALSE);
    if (!o_ptr->kind) return;

    of_inter(flags, o_ptr->known_flags);

    if (aware) of_union(flags, o_ptr->kind->flags);

    if (o_ptr->ego && easy_know(o_ptr, aware))
        of_union(flags, o_ptr->ego->flags);

    /* Hack -- Remove redundant bitflags */
    remove_redundant_flags(flags);

    /* Make sure all flags are present on the object */
    of_inter(flags, o_ptr->flags);
}


/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
s16b index_to_label(int i)
{
    /* Indexes for "inven" are easy */
    if (i < INVEN_WIELD) return (I2A(i));

    /* Indexes for "equip" are offset */
    return (I2A(i - INVEN_WIELD));
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
bool wearable_p(const object_type *o_ptr)
{
    /* Valid "tval" codes */
    switch (o_ptr->tval)
    {
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HORN:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LIGHT:
        case TV_AMULET:
        case TV_RING:
            return TRUE;
    }

    /* Nope */
    return FALSE;
}


static int get_inscribed_ammo_slot(const object_type *o_ptr)
{
    char *s;

    if (!o_ptr->note) return 0;
    s = strchr(quark_str(o_ptr->note), 'f');
    if (!s || (s[1] < '0') || (s[1] > '9')) return 0;

    return QUIVER_START + (s[1] - '0');
}


/*
 * Used by wield_slot() to find an appopriate slot for ammo. See wield_slot()
 * for information on what this returns.
 */
static s16b wield_slot_ammo(struct player *p, const object_type *o_ptr)
{
    s16b i, open = 0;

    /*
     * If the ammo is inscribed with a slot number, we'll try to put it in
     * that slot, if possible.
     */
    i = get_inscribed_ammo_slot(o_ptr);
    if (i && !p->inventory[i].kind) return i;

    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        if (!p->inventory[i].kind)
        {
            /* Save the open slot if we haven't found one already */
            if (!open) open = i;
            continue;
        }

        /* If ammo is cursed we can't stack it */
        if (cursed_p(p->inventory[i].flags)) continue;

        /* If they are stackable, we'll use this slot for sure */
        if (object_similar(&p->inventory[i], o_ptr, OSTACK_QUIVER)) return i;
    }

    /* If not absorbed, return an open slot (or QUIVER_START if no room) */
    return (open? open: QUIVER_START);
}


/*
 * Determine which equipment slot (if any) an item likes. The slot might (or
 * might not) be open, but it is a slot which the object could be equipped in.
 *
 * For items where multiple slots could work (e.g. ammo or rings), the function
 * will try to a return a stackable slot first (only for ammo), then an open
 * slot if possible, and finally a used (but valid) slot if necessary.
 */
s16b wield_slot(struct player *p, const object_type *o_ptr)
{
    /* Slot for equipment */
    switch (o_ptr->tval)
    {
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
            return INVEN_WIELD;
        case TV_BOW:
            return INVEN_BOW;
        case TV_RING:
            if (p) return (p->inventory[INVEN_RIGHT].kind? INVEN_LEFT: INVEN_RIGHT);
            return INVEN_LEFT;
        case TV_AMULET:
            return INVEN_NECK;
        case TV_LIGHT:
            return INVEN_LIGHT;
        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
            return INVEN_BODY;
        case TV_CLOAK:
            return INVEN_OUTER;
        case TV_SHIELD:
            return INVEN_ARM;
        case TV_CROWN:
        case TV_HELM:
            return INVEN_HEAD;
        case TV_GLOVES:
            return INVEN_HANDS;
        case TV_BOOTS:
            return INVEN_FEET;
        case TV_DIGGING:
        case TV_HORN:
            return INVEN_TOOL;
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
            if (p) return wield_slot_ammo(p, o_ptr);
            return QUIVER_START;
    }

    /* No slot available */
    return -1;
}


/*
 * Returns the slot item o_ptr will fit in (slot 'slot' is the default)
 */
int slot_can_wield_item(int Ind, int slot, const object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if ((o_ptr->tval == TV_RING) && ((slot == INVEN_LEFT) || (slot == INVEN_RIGHT)))
        return slot;

    if (obj_is_ammo(p_ptr, o_ptr) && ((slot >= QUIVER_START) && (slot < QUIVER_END)))
        return slot;

    return wield_slot(p_ptr, o_ptr);
}


/*
 * Return a string mentioning how a given item is carried
 */
const char *mention_use(int slot)
{
    /* Examine the location */
    switch (slot)
    {
        case INVEN_WIELD:  return "Wielding";
        case INVEN_BOW:    return "Shooting";
        case INVEN_LEFT:   return "On left hand";
        case INVEN_RIGHT:  return "On right hand";
        case INVEN_NECK:   return "Around neck";
        case INVEN_LIGHT:  return "Light source";
        case INVEN_BODY:   return "On body";
        case INVEN_OUTER:  return "About body";
        case INVEN_ARM:    return "On arm";
        case INVEN_HEAD:   return "On head";
        case INVEN_HANDS:  return "On hands";
        case INVEN_FEET:   return "On feet";
        case INVEN_TOOL:   return "Using";

        case QUIVER_START + 0: return "In quiver [f0]";
        case QUIVER_START + 1: return "In quiver [f1]";
        case QUIVER_START + 2: return "In quiver [f2]";
        case QUIVER_START + 3: return "In quiver [f3]";
        case QUIVER_START + 4: return "In quiver [f4]";
    }

    return "In pack";
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
const char *describe_use(struct player *p, int i)
{
    const char *pm;

    switch (i)
    {
        case INVEN_WIELD: pm = "attacking enemies with"; break;
        case INVEN_BOW:   pm = "shooting missiles with"; break;
        case INVEN_LEFT:  pm = "wearing on your left hand"; break;
        case INVEN_RIGHT: pm = "wearing on your right hand"; break;
        case INVEN_NECK:  pm = "wearing around your neck"; break;
        case INVEN_LIGHT: pm = "using to light the way"; break;
        case INVEN_BODY:  pm = "wearing on your body"; break;
        case INVEN_OUTER: pm = "wearing on your back"; break;
        case INVEN_ARM:   pm = "wearing on your arm"; break;
        case INVEN_HEAD:  pm = "wearing on your head"; break;
        case INVEN_HANDS: pm = "wearing on your hands"; break;
        case INVEN_FEET:  pm = "wearing on your feet"; break;
        case INVEN_TOOL:  pm = "using to dig"; break;
        default:          pm = "carrying in your pack"; break;
    }

    /* Hack -- Heavy weapon */
    if (i == INVEN_WIELD)
    {
        object_type *o_ptr;
        o_ptr = &p->inventory[i];
        if (adj_str_hold[p->state.stat_ind[A_STR]] < o_ptr->weight / 10)
            pm = "just lifting";
    }

    /* Hack -- Heavy bow */
    if (i == INVEN_BOW)
    {
        object_type *o_ptr;
        o_ptr = &p->inventory[i];
        if (adj_str_hold[p->state.stat_ind[A_STR]] < o_ptr->weight / 10)
            pm = "just holding";
    }

    /* Hack -- Missiles */
    if (i > INVEN_TOOL) pm = "carrying in your quiver";

    /* Return the result */
    return pm;
}


/*
 * Return true if the item is unknown (has yet to be seen by the player).
 */
static bool is_unknown(struct player *p, const object_type *o_ptr)
{
    grid_data gd;

    map_info(p, o_ptr->iy, o_ptr->ix, &gd);
    return gd.unseen_object;
}


/*
 * Get the indexes of objects at a given floor location.
 *
 * Return the number of object indexes acquired.
 *
 * Valid flags are any combination of the bits:
 *   0x01 -- Verify item tester
 *   0x02 -- Marked/visible items only
 */
int scan_floor(struct player *p, int *items, int max_size, int depth, int y, int x, int mode)
{
    s16b this_o_idx, next_o_idx;
    int num = 0;

    /* Sanity */
    if (!in_bounds(y, x)) return 0;

    /* Scan all objects in the grid */
    for (this_o_idx = cave_get(depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* XXX Hack -- Enforce limit */
        if (num >= max_size) break;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore all non-objects */
        if (!o_ptr->kind) continue;

        /* Item tester */
        if ((mode & 0x01) && !item_tester_okay(p, o_ptr)) continue;

        /* Marked */
        if (mode & 0x02)
        {
            if (!object_marked(p, this_o_idx)) continue;
            if (!is_unknown(p, o_ptr) && squelch_item_ok(p, o_ptr)) continue;
        }

        /* Accept this item */
        items[num++] = this_o_idx;
    }

    return num;
}


/*
 * Excise a dungeon object from any stacks
 */
static void excise_object_idx(int o_idx)
{
    object_type *j_ptr;
    s16b this_o_idx, next_o_idx = 0;
    s16b prev_o_idx = 0;

    /* Object */
    j_ptr = object_byid(o_idx);

    /* Paranoia */
    if (!cave_get(j_ptr->depth)) return;

    /* Monster */
    if (j_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Monster */
        m_ptr = cave_monster(cave_get(j_ptr->depth), j_ptr->held_m_idx);

        /* Scan all objects in the grid */
        for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = object_byid(this_o_idx);

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
                    i_ptr = object_byid(prev_o_idx);

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
        for (this_o_idx = cave_get(j_ptr->depth)->o_idx[y][x]; this_o_idx;
            this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = object_byid(this_o_idx);

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Done */
            if (this_o_idx == o_idx)
            {
                /* No previous */
                if (prev_o_idx == 0)
                {
                    /* Remove from list */
                    cave_get(j_ptr->depth)->o_idx[y][x] = next_o_idx;
                }

                /* Real previous */
                else
                {
                    object_type *i_ptr;

                    /* Previous object */
                    i_ptr = object_byid(prev_o_idx);

                    /* Remove from list */
                    i_ptr->next_o_idx = next_o_idx;
                }

                /* Forget next pointer */
                o_ptr->next_o_idx = 0;

                /* Clear visibility flags */
                clear_visibility(o_idx, j_ptr->depth, TRUE);

                /* Redraw */
                redraw_floor(j_ptr->depth, y, x);

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
    object_type *j_ptr = object_byid(o_idx);

    /* Excise (with efficiency) */
    if (j_ptr->held_m_idx >= 0) excise_object_idx(o_idx);

    /* Dungeon floor */
    if (!j_ptr->held_m_idx)
    {
        int y, x;

        /* Location */
        y = j_ptr->iy;
        x = j_ptr->ix;

        /* Visual update */
        if (cave_get(j_ptr->depth)) cave_light_spot(cave_get(j_ptr->depth), y, x);
    }

    /* Delete the mimicking monster if necessary */
    if (j_ptr->mimicking_m_idx)
    {
        monster_type *m_ptr;

        m_ptr = cave_monster(cave_get(j_ptr->depth), j_ptr->mimicking_m_idx);

        /* Clear the mimicry */
        m_ptr->mimicked_o_idx = 0;

        delete_monster_idx(cave_get(j_ptr->depth), j_ptr->mimicking_m_idx);
    }

    /* Wipe the object */
    object_wipe(j_ptr);
}


/*
 * Deletes all objects at given location
 */
void delete_object(int depth, int y, int x)
{
    s16b this_o_idx, next_o_idx = 0;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Paranoia -- make sure the level has been allocated */
    if (!cave_get(depth))
    {
        plog_fmt("Error : tried to delete object on unallocated level %d", depth);
        return;
    }

    /* Scan all objects in the grid */
    for (this_o_idx = cave_get(depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Preserve unseen artifacts */
        preserve_artifact(o_ptr);

        /* Clear visibility flags */
        clear_visibility(this_o_idx, depth, TRUE);

        /* Delete the mimicking monster if necessary */
        if (o_ptr->mimicking_m_idx)
        {
            monster_type *m_ptr;

            m_ptr = cave_monster(cave_get(depth), o_ptr->mimicking_m_idx);

            /* Clear the mimicry */
            m_ptr->mimicked_o_idx = 0;

            delete_monster_idx(cave_get(depth), o_ptr->mimicking_m_idx);
        }

        /* Wipe the object */
        object_wipe(o_ptr);
    }

    /* Objects are gone */
    cave_get(depth)->o_idx[y][x] = 0;

    /* Redraw */
    redraw_floor(depth, y, x);

    /* Visual update */
    cave_light_spot(cave_get(depth), y, x);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_objects_aux(int i1, int i2)
{
    int i, Ind;
    object_type *o_ptr;

    /* Do nothing */
    if (i1 == i2) return;

    /* Repair objects */
    for (i = 1; i < o_max; i++)
    {
        /* Get the object */
        o_ptr = object_byid(i);

        /* Skip "dead" objects */
        if (!o_ptr->kind) continue;

        /* Repair "next" pointers */
        if (o_ptr->next_o_idx == i1)
        {
            /* Repair */
            o_ptr->next_o_idx = i2;
        }
    }

    /* Get the object */
    o_ptr = object_byid(i1);

    /* Monster */
    if (cave_get(o_ptr->depth) && o_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Get the monster */
        m_ptr = cave_monster(cave_get(o_ptr->depth), o_ptr->held_m_idx);

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
        if (cave_get(o_ptr->depth) && (cave_get(o_ptr->depth)->o_idx[y][x] == i1))
        {
            /* Repair */
           cave_get(o_ptr->depth)->o_idx[y][x] = i2;
        }

        /* Mimic */
        if (o_ptr->mimicking_m_idx)
        {
            monster_type *m_ptr;

            /* Get the monster */
            m_ptr = cave_monster(cave_get(o_ptr->depth), o_ptr->mimicking_m_idx);

            /* Repair monster */
            if (m_ptr->mimicked_o_idx == i1)
            {
                /* Repair */
                m_ptr->mimicked_o_idx = i2;
            }
        }
    }

    /* Copy the visibility flags for each player */
    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);

        p_ptr->obj_marked[i2] = p_ptr->obj_marked[i1];

        /* Redraw */
        p_ptr->redraw |= (PR_FLOOR | PR_ITEMLIST);
    }

    /* Hack -- move object */
    object_copy(object_byid(i2), object_byid(i1));

    /* Hack -- wipe hole */
    object_wipe(o_ptr);
}


/*
 * Determines if an object is eligible for squelching for all players.
 */
static bool everyone_squelch_item_ok(object_type *o_ptr)
{
    int i;
    bool squelch = FALSE;

    /* Preserve items in houses! */
    if ((o_ptr->depth <= 0) && is_icky(o_ptr->depth, o_ptr->iy, o_ptr->ix))
        return FALSE;

    /* Check all players */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        /* Item is not squelchable for this player */
        if (!squelch_item_ok(player_get(i), o_ptr)) return FALSE;

        /* Item is squelchable for this player... */
        squelch = TRUE;
    }

    /* ...but we need at least one player */
    return squelch;
}


/*
 * Compact and reorder the object list
 *
 * This function can be very dangerous, use with caution!
 *
 * When compacting objects, we first destroy gold, on the basis that by the
 * time item compaction becomes an issue, the player really won't care.
 * We also nuke items marked as squelch.
 *
 * When compacting other objects, we base the saving throw on a combination of
 * object level, object value, and current "desperation".
 *
 * After compacting, we "reorder" the objects into a more compact order, and we
 * reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
    int i, y, x, cnt;
    int cur_lev, cur_val, chance;

    /* Reorder objects when not passed a size */
    if (!size)
    {
        /* Excise dead objects (backwards!) */
        for (i = o_max - 1; i >= 1; i--)
        {
            object_type *o_ptr = object_byid(i);

            /* Skip real objects */
            if (o_ptr->kind) continue;

            /* Move last object into open hole */
            compact_objects_aux(o_max - 1, i);

            /* Compress "o_max" */
            o_max--;
        }

        /* Reset "o_nxt" */
        o_nxt = o_max;

        /* Reset "o_top" */
        o_top = 0;

        /* Collect "live" objects */
        for (i = 1; i < o_max; i++)
        {
            /* Collect indexes */
            o_fast[o_top++] = i;
        }

        return;
    }

    /* Message */
    plog("Compacting objects...");

    /*** Try destroying objects ***/

    /* First do crops and junk */
    for (i = 1; (i < o_max) && (size); i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Nuke crops */
        if (o_ptr->tval == TV_CROP)
        {
            delete_object_idx(i);
            size--;
        }

        /* Nuke junk */
        if ((o_ptr->tval == TV_SKELETON) || (o_ptr->tval == TV_CORPSE) ||
            (o_ptr->tval == TV_BOTTLE))
        {
            delete_object_idx(i);
            size--;
        }
    }

    /* Then do gold and squelched items */
    for (i = 1; (i < o_max) && (size); i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Nuke gold or squelched items */
        if ((o_ptr->tval == TV_GOLD) || everyone_squelch_item_ok(o_ptr))
        {
            /* Hack -- Skip gold/items in houses */
            y = o_ptr->iy;
            x = o_ptr->ix;
            if ((o_ptr->depth <= 0) && is_icky(o_ptr->depth, y, x))
                continue;

            delete_object_idx(i);
            size--;
        }
    }

    /* Compact at least 'size' objects */
    for (cnt = 1; size; cnt++)
    {
        /* Get more vicious each iteration */
        cur_lev = 5 * cnt;

        /* Destroy more valuable items each iteration */
        cur_val = 500 * (cnt - 1);

        /* Examine the objects */
        for (i = 1; (i < o_max) && (size); i++)
        {
            object_type *o_ptr = object_byid(i);

            /* Skip dead objects */
            if (!o_ptr->kind) continue;

            /* Hack -- High level objects start out "immune" */
            if (o_ptr->kind->level > cur_lev) continue;

            /* Valuable objects start out "immune" */
            if (object_value(NULL, o_ptr, 1) > cur_val) continue;

            /* Saving throw */
            chance = 90;

            /* Monster */
            if (o_ptr->held_m_idx)
            {
                /* Monsters protect their objects */
            }

            /* Mimicked items */
            else if (o_ptr->mimicking_m_idx)
            {
                /* Get the location */
                y = o_ptr->iy;
                x = o_ptr->ix;

                /* Mimicked items try hard not to be compacted */
                if (randint0(100) < 90)
                    continue;
            }

            /* Dungeon */
            else
            {
                /* Get the location */
                y = o_ptr->iy;
                x = o_ptr->ix;

                /* Hack -- Only compact items in houses in emergencies */
                if ((o_ptr->depth <= 0) && is_icky(o_ptr->depth, y, x))
                {
                    /* Grant immunity except in emergencies */
                    if (cnt < 1000) chance = 100;
                }
            }

            /* Hack -- Only compact artifacts in emergencies */
            if (o_ptr->artifact && (cnt < 1000)) chance = 100;

            /* Apply the saving throw */
            if (magik(chance)) continue;

            /* Hack -- Preserve artifacts */
            preserve_artifact(o_ptr);

            /* Delete the object */
            delete_object_idx(i);
            size--;
        }
    }

    /* Reorder objects */
    compact_objects(0);
}


/*
 * Delete all the items when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "cave->o_idx[y][x]" field for every grid,
 * and the "m_ptr->hold_o_idx" field for every monster, since
 * we know we are clearing every object.  Technically, we only
 * clear those fields for grids/monsters containing objects,
 * and we clear it once for every such object.
 */
void wipe_o_list(struct cave *c)
{
    int i;

    /* Delete the existing objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip objects not on this depth */
        if (o_ptr->depth != c->depth) continue;

        /* Hack -- preserve artifacts */
        if (o_ptr->artifact)
        {
            /* Only works when owner is ingame */
            player_type *p_ptr = player_get(get_owner_id(o_ptr));

            /* Mark artifact as abandoned */
            set_artifact_info(p_ptr, o_ptr, ARTS_ABANDONED);

            /* Preserve any artifact */
            preserve_artifact_aux(o_ptr);
        }

        /* Monster */
        if (o_ptr->held_m_idx)
        {
            monster_type *m_ptr;

            /* Monster */
            m_ptr = cave_monster(c, o_ptr->held_m_idx);

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
            c->o_idx[y][x] = 0;

            /* Clear visibility flags */
            clear_visibility(c->o_idx[y][x], c->depth, FALSE);
        }

        /* Wipe the object */
        object_wipe(o_ptr);
    }

    /* Compact the object list */
    compact_objects(0);
}


/*
 * Preserve artifacts on the ground.
 *
 * Same as wipe_o_list(), but cave_get(depth) may not exist, and we don't need to
 * clear visibility flags.
 */
void preserve_artifacts(int depth)
{
    int i;

    /* Check there are no players on the level first */
    if (players_on_depth[depth]) return;

    /* Delete the existing objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip objects not on this depth */
        if (o_ptr->depth != depth) continue;

        /* Hack -- preserve artifacts */
        if (o_ptr->artifact)
        {
            /* Only works when owner is ingame */
            player_type *p_ptr = player_get(get_owner_id(o_ptr));

            /* Mark artifact as abandoned */
            set_artifact_info(p_ptr, o_ptr, ARTS_ABANDONED);

            /* Preserve any artifact */
            preserve_artifact_aux(o_ptr);
        }

        /* Monster */
        if (cave_get(depth) && o_ptr->held_m_idx)
        {
            monster_type *m_ptr;

            /* Monster */
            m_ptr = cave_monster(cave_get(depth), o_ptr->held_m_idx);

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
            if (cave_get(depth)) cave_get(depth)->o_idx[y][x] = 0;
        }

        /* Wipe the object */
        object_wipe(o_ptr);
    }

    /* Compact the object list */
    compact_objects(0);
}


/*
 * Acquires and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 *
 * Note that this function must maintain the special "o_fast"
 * array of pointers to "live" objects.
 */
s16b o_pop(void)
{
    int i, n, k;

    /* Initial allocation */
    if (o_max < z_info->o_max)
    {
        /* Get next space */
        i = o_max;

        /* Expand object array */
        o_max++;

        /* Update "o_fast" */
        o_fast[o_top++] = i;

        /* Use this object */
        return (i);
    }

    /* Check for some space */
    for (n = 1; n < z_info->o_max; n++)
    {
        /* Get next space */
        i = o_nxt;

        /* Advance (and wrap) the "next" pointer */
        if (++o_nxt >= z_info->o_max) o_nxt = 1;

        /* Skip objects in use */
        if (object_byid(i)->kind) continue;

        /* Verify space XXX XXX */
        if (o_top >= z_info->o_max) continue;

        /* Verify not allocated */
        for (k = 0; k < o_top; k++)
        {
            /* Hack -- Prevent errors */
            if (o_fast[k] == i) i = 0;
        }

        /* Oops XXX XXX */
        if (!i) continue;

        /* Update "o_fast" */
        o_fast[o_top++] = i;

        /* Use this object */
        return (i);
    }

    /* Warn the player */
    if (!ht_zero(&cave_get(0)->generated)) plog("Too many objects!");

    /* Oops */
    return (0);
}


/*
 * Get the first object at a dungeon location or NULL if there isn't one.
 */
object_type *get_first_object(int depth, int y, int x)
{
    s16b o_idx = cave_get(depth)->o_idx[y][x];

    if (o_idx) return object_byid(o_idx);

    /* No object */
    return NULL;
}


/*
 * Get the next object in a stack or NULL if there isn't one.
 */
object_type *get_next_object(const object_type *o_ptr)
{
    if (o_ptr->next_o_idx) return object_byid(o_ptr->next_o_idx);

    /* No more objects */
    return NULL;
}


/*
 * Return the "value" of an "unknown" non-wearable item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(struct player *p, const object_type *o_ptr)
{
    /* Use template cost for aware objects */
    if (object_flavor_is_aware(p, o_ptr)) return (o_ptr->kind->cost);

    /* Analyze the type */
    switch (o_ptr->tval)
    {
        case TV_FOOD: return 5;
        case TV_POTION:
        case TV_SCROLL: return 20;
        case TV_WAND: return 50;
        case TV_STAFF: return 70;
        case TV_ROD: return 90;
    }

    /* Paranoia (should never come here) */
    return 0;
}


/*
 * Return the "real" price of a "known" item
 *
 * Wands and staves get cost for each charge.
 *
 * Wearable items (weapons, launchers, jewelry, lights, armour, tools, ammo)
 * are priced according to their power rating. All ammo, and normal (non-ego)
 * torches are scaled down by AMMO_RESCALER to reflect their impermanence.
 *
 * PWMAngband: artifacts are always sellable
 */
s32b object_value_real(struct player *p, const object_type *o_ptr, int qty)
{
    s32b value, total_value;
    s32b power;
    int a = 1;
    int b = 5;
    s32b min_value = (o_ptr->artifact? 1L: 0L);

    /* Hack -- Worthless objects */
    if (o_ptr->ident & IDENT_WORTHLESS) return (min_value);

    if (wearable_p(o_ptr))
    {
        bool normal_ammo = (obj_is_ammo(p, o_ptr) && !o_ptr->artifact);
        bool normal_torch = ((o_ptr->tval == TV_LIGHT) &&
            (o_ptr->sval == SV_LIGHT_TORCH) && !o_ptr->ego);

        power = object_power(p, o_ptr);
        value = sign(power) * (a * power * power + b * power);

        if (normal_ammo || normal_torch)
        {
            value = value / AMMO_RESCALER;
            if (value < 1) value = 1;
        }

        /* PWMAngband -- Boost magic ammo */
        if (magic_ammo_p(o_ptr)) value += 100L;

        /* PWMAngband -- Boost artifact missiles */
        if (obj_is_ammo(p, o_ptr) && o_ptr->artifact) value += 10000L;

        /* PWMAngband -- Boost Rings of Polymorphing */
        if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POLYMORPHING))
        {
            int i;
            bitflag f[MAX_PVALS][OF_SIZE];

            /* Be careful: the flag only exists for known rings... */
            object_pval_flags(o_ptr, f);
            for (i = 0; i < o_ptr->num_pvals; i++)
            {
                if (of_has(f[i], OF_POLY_RACE))
                {
                    monster_race *r_ptr = &r_info[o_ptr->pval[i]];

                    value += r_ptr->power / 100;
                    break;
                }
            }
        }

        total_value = value * qty;

        if (total_value < min_value) total_value = min_value;

        return (total_value);
    }

    /* Hack -- "worthless" items */
    if (!o_ptr->kind->cost) return (min_value);

    /* Base cost */
    value = o_ptr->kind->cost;

    /* Calculate total value */
    total_value = value * qty;

    /* Wands/Staffs */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
    {
        int charges = o_ptr->pval[DEFAULT_PVAL] * qty;

        /* Calculate number of charges, rounded up */
        if (o_ptr->number)
        {
            charges = o_ptr->pval[DEFAULT_PVAL] * qty / o_ptr->number;
            if ((o_ptr->pval[DEFAULT_PVAL] * qty) % o_ptr->number != 0) charges++;
        }

        /* Pay extra for charges, depending on standard number of charges */
        total_value += value * charges / 20;
    }

    /* No negative value */
    if (total_value < min_value) total_value = min_value;

    /* Return the value */
    return (total_value);
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 */
s32b object_value(struct player *p, const object_type *o_ptr, int qty)
{
    s32b value;

    /* Gold */
    if (o_ptr->tval == TV_GOLD) return o_ptr->pval[DEFAULT_PVAL];

    /* Known items */
    if (object_is_known(p, o_ptr))
    {
        /* Known cursed items */
        if (cursed_p((bitflag *)o_ptr->flags)) return (0L);

        /* Real value */
        value = object_value_real(p, o_ptr, qty);
    }

    /* Unknown wearable items */
    else if (wearable_p(o_ptr))
    {
        object_type object_type_body;
        object_type *j_ptr = &object_type_body;

        /* Hack -- Felt cursed items */
        if (object_was_sensed(o_ptr) && cursed_p((bitflag *)o_ptr->flags)) return (0L);

        object_copy(j_ptr, o_ptr);

        /* Give j_ptr only the flags known to be in o_ptr */
        object_flags_known(o_ptr, j_ptr->flags, object_flavor_is_aware(p, o_ptr));
        object_pval_flags_known(o_ptr, j_ptr->pval_flags, object_flavor_is_aware(p, o_ptr));

        if (!object_attack_plusses_are_visible(p, o_ptr))
            j_ptr->to_h = j_ptr->to_d = 0;
        if (!object_defence_plusses_are_visible(p, o_ptr))
            j_ptr->to_a = 0;

        value = object_value_real(p, j_ptr, qty);
    }

    /* Base value */
    else
        value = object_value_base(p, o_ptr) * qty;

    /* Return the final value */
    return (value);
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
bool object_similar(const object_type *o_ptr, const object_type *j_ptr, object_stack_t mode)
{
    int i;
    int total = o_ptr->number + j_ptr->number;
    bitflag oflags[MAX_PVALS][OF_SIZE], jflags[MAX_PVALS][OF_SIZE];

    /* Check against stacking limit - except in stores which absorb anyway */
    if (!(mode & OSTACK_STORE) && (total >= MAX_STACK_SIZE)) return FALSE;

    /* Hack -- identical items cannot be stacked */
    if (o_ptr == j_ptr) return FALSE;

    /* Require identical object kinds */
    if (o_ptr->kind != j_ptr->kind) return FALSE;

    /* Different flags don't stack */
    if (!of_is_equal(o_ptr->flags, j_ptr->flags)) return FALSE;

    /* Artifacts never stack */
    if (o_ptr->artifact || j_ptr->artifact) return FALSE;

    /* Analyze the items */
    switch (o_ptr->tval)
    {
        /* Chests never stack */
        case TV_CHEST:
        {
            /* Never okay */
            return FALSE;
        }

        /* Food, potions, scrolls, staves, wands and rods all stack nicely */
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
        {
            /* Assume okay */
            break;
        }

        /* Weapons, ammo, armour, jewelry, lights, tools */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HORN:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_RING:
        case TV_AMULET:
        case TV_LIGHT:
        case TV_ROCK:
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Require identical values */
            if (o_ptr->ac != j_ptr->ac) return FALSE;
            if (o_ptr->dd != j_ptr->dd) return FALSE;
            if (o_ptr->ds != j_ptr->ds) return FALSE;

            /* Require identical bonuses */
            if (o_ptr->to_h != j_ptr->to_h) return FALSE;
            if (o_ptr->to_d != j_ptr->to_d) return FALSE;
            if (o_ptr->to_a != j_ptr->to_a) return FALSE;

            object_pval_flags(o_ptr, oflags);
            object_pval_flags(j_ptr, jflags);

            /* Require all identical pvals */
            for (i = 0; i < MAX_PVALS; i++)
            {
                if (o_ptr->pval[i] != j_ptr->pval[i]) return FALSE;
                if (!of_is_equal(oflags[i], jflags[i])) return FALSE;
            }

            /* Require identical ego-item types */
            if (o_ptr->ego != j_ptr->ego) return FALSE;

            /* Hack -- Never stack recharging wearables */
            if ((o_ptr->timeout || j_ptr->timeout) && (o_ptr->tval != TV_LIGHT))
                return FALSE;

            /* Lights must have same amount of fuel */
            else if ((o_ptr->timeout != j_ptr->timeout) && (o_ptr->tval == TV_LIGHT))
                return FALSE;

            /* Prevent unIDd items stacking in the object list */
            if ((mode & OSTACK_LIST) && !(o_ptr->ident & j_ptr->ident & IDENT_KNOWN))
                return FALSE;

            /* Probably okay */
            break;
        }

        /* Corpses */
        case TV_CORPSE:
        {
            /* Require identical monster type and timeout */
            if (o_ptr->pval[DEFAULT_PVAL] != j_ptr->pval[DEFAULT_PVAL]) return FALSE;
            if (o_ptr->pval[DEFAULT_PVAL + 1] != j_ptr->pval[DEFAULT_PVAL + 1]) return FALSE;

            /* Probably okay */
            break;
        }

        /* Anything else */
        default:
        {
            /* Probably okay */
            break;
        }
    }

    /* Require compatible inscriptions */
    if (o_ptr->note && j_ptr->note && (o_ptr->note != j_ptr->note)) return FALSE;

    /* They must be similar enough */
    return TRUE;
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
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    int total = o_ptr->number + j_ptr->number;

    /* Add together the item counts */
    o_ptr->number = ((total < MAX_STACK_SIZE)? total: (MAX_STACK_SIZE - 1));

    /* Blend all knowledge */
    o_ptr->ident |= (j_ptr->ident & ~IDENT_EMPTY);
    of_union(o_ptr->known_flags, j_ptr->known_flags);

    /* Merge inscriptions */
    if (j_ptr->note) o_ptr->note = j_ptr->note;

    /* Combine timeouts for rod stacking */
    if (o_ptr->tval == TV_ROD) o_ptr->timeout += j_ptr->timeout;

    /* Combine pvals for wands and staves */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
        o_ptr->pval[DEFAULT_PVAL] += j_ptr->pval[DEFAULT_PVAL];

    /* Hack -- blend "origins" */
    object_absorb_origin(o_ptr, j_ptr);
}


/*
 * Wipe an object clean.
 */
void object_wipe(object_type* o_ptr)
{
    /* Wipe the object */
    WIPE(o_ptr, object_type);
}


/*
 * Prepare an object based on an existing object
 */
void object_copy(object_type *o_ptr, const object_type *j_ptr)
{
    /* Copy the structure */
    COPY(o_ptr, j_ptr, object_type);
}


/*
 * Prepare an object `dst` representing `amt` objects,  based on an existing
 * object `src` representing at least `amt` objects.
 *
 * Takes care of the charge redistribution concerns of stacked items.
 */
void object_copy_amt(object_type *dst, object_type *src, int amt)
{
    int charge_time = randcalc(src->kind->time, 0, AVERAGE), max_time;

    /* Get a copy of the object */
    object_copy(dst, src);

    /* Modify quantity */
    dst->number = amt;

    /*
     * If the item has charges/timeouts, set them to the correct level
     * too. We split off the same amount as distribute_charges.
     */
    if ((src->tval == TV_WAND) || (src->tval == TV_STAFF))
        dst->pval[DEFAULT_PVAL] = src->pval[DEFAULT_PVAL] * amt / src->number;

    if (src->tval == TV_ROD)
    {
        max_time = charge_time * amt;

        if (src->timeout > max_time) dst->timeout = max_time;
        else dst->timeout = src->timeout;
    }
}


/*
 * Find and return the index to the oldest object on the given grid marked as "squelch".
 */
static s16b floor_get_idx_oldest_squelched(struct player *p, int depth, int y, int x)
{
    s16b squelch_idx = 0;
    s16b this_o_idx;
    object_type *o_ptr = NULL;

    for (this_o_idx = cave_get(depth)->o_idx[y][x]; this_o_idx;
        this_o_idx = o_ptr->next_o_idx)
    {
        o_ptr = object_byid(this_o_idx);

        if (p && squelch_item_ok(p, o_ptr)) squelch_idx = this_o_idx;
    }

    return squelch_idx;
}


static void under_feet(int Ind, object_type *o_ptr, bool verbose)
{
    player_type *p_ptr = player_get(Ind);

    if (verbose && !squelch_item_ok(p_ptr, o_ptr))
        msg(p_ptr, "You feel something roll beneath your feet.");

    /* Redraw */
    p_ptr->redraw |= PR_FLOOR;
}


/*
 * Let the floor carry an object, deleting old squelched items if necessary
 */
s16b floor_carry(struct player *p, struct cave *c, int y, int x, object_type *j_ptr, bool verbose)
{
    int n = 0;
    s16b o_idx;
    s16b this_o_idx, next_o_idx = 0;

    /* Scan objects in that grid for combination */
    for (this_o_idx = c->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for combination */
        if (object_similar(o_ptr, j_ptr, OSTACK_FLOOR))
        {
            /* Combine the items */
            object_absorb(o_ptr, j_ptr);

            if (c->m_idx[y][x] < 0)
                under_feet(0 - c->m_idx[y][x], o_ptr, verbose);

            /* Result */
            return (this_o_idx);
        }

        /* Count objects */
        n++;
    }

    /* The stack is already too large */
    if (n >= MAX_FLOOR_STACK)
    {
        /* Squelch the oldest squelched object */
        s16b squelch_idx = floor_get_idx_oldest_squelched(p, c->depth, y, x);

        if (squelch_idx)
            delete_object_idx(squelch_idx);
        else
            return 0;
    }

    /* Make an object */
    o_idx = o_pop();

    /* Success */
    if (o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(o_idx);

        /* Structure Copy */
        object_copy(o_ptr, j_ptr);

        /* Location */
        o_ptr->iy = y;
        o_ptr->ix = x;
        o_ptr->depth = c->depth;

        /* Forget monster */
        o_ptr->held_m_idx = 0;

        /* Link the object to the pile */
        o_ptr->next_o_idx = c->o_idx[y][x];

        /* Link the floor to the object */
        c->o_idx[y][x] = o_idx;

        /* Clear visibility flags */
        clear_visibility(c->o_idx[y][x], c->depth, FALSE);

        /* Notice */
        cave_note_spot(c, y, x);

        /* Redraw */
        cave_light_spot(c, y, x);

        if (c->m_idx[y][x] < 0)
            under_feet(0 - c->m_idx[y][x], o_ptr, verbose);
    }

    /* Result */
    return (o_idx);
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
 * This function will produce a description of a drop event under the player
 * when "verbose" is true.
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed.  Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 */
byte drop_near(struct player *p, struct cave *c, object_type *j_ptr, int chance, int y,
    int x, bool verbose)
{
    int i, k, n, d, s;
    int bs, bn;
    int by, bx;
    int dy, dx;
    int ty, tx;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    bool flag = FALSE;
    bool plural = FALSE;

    /* Paranoia */
    if (j_ptr == NULL) return DROP_NONE;

    /* Extract plural */
    if (j_ptr->number > 1) plural = TRUE;

    /* Describe object */
    if (p) object_desc(p, o_name, sizeof(o_name), j_ptr, ODESC_BASE);

    /* Handle normal "breakage" */
    if (!j_ptr->artifact && (chance > 0) && magik(chance))
    {
        /* Message */
        if (p) msg(p, "The %s break%s.", o_name, PLURAL(plural));

        /* Failure */
        return DROP_NONE;
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
            if (!los(c->depth, y, x, ty, tx)) continue;

            /* Require floor space */
            if (!cave_isanyfloor(c, ty, tx)) continue;

            /* No objects */
            k = 0;
            n = 0;

            /* Scan objects in that grid */
            for (o_ptr = get_first_object(c->depth, ty, tx); o_ptr; o_ptr = get_next_object(o_ptr))
            {
                /* Check for possible combination */
                if (object_similar(o_ptr, j_ptr, OSTACK_FLOOR)) comb = TRUE;

                /* Count objects */
                if (!(p && squelch_item_ok(p, o_ptr))) k++;
                else n++;
            }

            /* Add new object */
            if (!comb) k++;

            /* Paranoia? */
            if (((k + n) > MAX_FLOOR_STACK) && !floor_get_idx_oldest_squelched(p, c->depth, ty, tx))
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
            flag = TRUE;
        }
    }

    /* Handle lack of space */
    if (!flag && !j_ptr->artifact)
    {
        /* Message */
        if (p) msg(p, "The %s disappear%s.", o_name, PLURAL(plural));

        /* Failure */
        return DROP_NONE;
    }

    /* Artifact: find a useful grid */
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
            ty = randint0(DUNGEON_HGT);
            tx = randint0(DUNGEON_WID);
        }

        /* Require floor space */
        if (!cave_isanyfloor(c, ty, tx)) continue;

        /* Bounce to that location */
        by = ty;
        bx = tx;

        /* Require floor space */
        if (!cave_canputitem(c, by, bx))
        {
            /* Try to find another location with clean floor */
            if (i < 10000) continue;

            /* If none can be found, then simply use any valid grid */
            if (!cave_valid_bold(c->depth, by, bx)) break;
        }

        /* Okay */
        flag = TRUE;
    }

    /* Successful drop */
    if (flag)
    {
        bool in_house = FALSE;

        /* Check houses */
        if (true_artifact_p(j_ptr) || (j_ptr->tval == TV_ROD) || (j_ptr->tval == TV_LIGHT))
        {
            for (i = 0; i < num_houses; i++)
            {
                /* Check this one */
                if ((houses[i].depth == c->depth) &&
                    (houses[i].x_1 <= bx) && (bx <= houses[i].x_2) &&
                    (houses[i].y_1 <= by) && (by <= houses[i].y_2))
                {
                    in_house = TRUE;
                    break;
                }
            }
        }

        /* Process true artifacts */
        if (true_artifact_p(j_ptr))
        {
            /* True artifacts cannot be dropped in houses... */
            if (in_house)
            {
                /* ...except the Crown and Grond which are not "unique" artifacts */
                if ((j_ptr->artifact->aidx != ART_MORGOTH) &&
                    (j_ptr->artifact->aidx != ART_GROND))
                        return DROP_ERROR;
            }

            /* True artifacts cannot be dropped/thrown in the wilderness */
            else if (c->depth < 0) return DROP_ERROR;

            /* True artifacts cannot be dropped/thrown on special levels */
            else if (check_special_level(c->depth)) return DROP_ERROR;
        }

        /* Recharge rods dropped in houses instantly */
        if ((j_ptr->tval == TV_ROD) && in_house) j_ptr->timeout = 0;

        /* Refuel lights dropped in houses to the standard amount */
        if ((j_ptr->tval == TV_LIGHT) && in_house) fuel_default(j_ptr);

        /* Assume fails */
        flag = FALSE;

        /* Crush anything under us (for artifacts) */
        if (j_ptr->artifact) delete_object(c->depth, by, bx);

        /* Give it to the floor */
        if (floor_carry(p, c, by, bx, j_ptr, verbose))
        {
            /* Sound */
            if (p) sound(p, MSG_DROP);

            /* Success */
            flag = TRUE;
        }
    }

    /* Poor little object */
    if (!flag)
    {
        /* Message */
        if (p) msg(p, "The %s disappear%s.", o_name, PLURAL(plural));

        /* Hack -- Preserve artifacts */
        if (j_ptr->artifact)
        {
            /* Only works when owner is ingame */
            if (!p) p = player_get(get_owner_id(j_ptr));

            /* Preserve any artifact */
            preserve_artifact_aux(j_ptr);
            if (p) history_lose_artifact(p, j_ptr);
        }

        /* Failure */
        return DROP_NONE;
    }

    return DROP_OK;
}


/*
 * This will push objects off a square.
 *
 * The methodology is to load all objects on the square into a queue. Replace
 * the previous square with a type that does not allow for objects. Drop the
 * objects. Last, put the square back to its original type.
 */
void push_object(struct player *p, int y, int x)
{
    /* Save the original terrain feature */
    int feat_old = cave_get(p->depth)->feat[y][x];

    object_type *o_ptr;
    struct queue *queue = q_new(MAX_FLOOR_STACK);

    /* Push all objects on the square into the queue */
    for (o_ptr = get_first_object(p->depth, y, x); o_ptr; o_ptr = get_next_object(o_ptr))
        q_push_ptr(queue, o_ptr);

    /* Set feature to an open door */
    cave_set_feat(cave_get(p->depth), y, x, FEAT_OPEN);

    /* Drop objects back onto the floor */
    while (q_len(queue) > 0)
    {
        /* Take object from the queue */
        o_ptr = q_pop_ptr(queue);

        /* Drop the object */
        drop_near(p, cave_get(p->depth), o_ptr, 0, y, x, FALSE);
    }

    /* Delete original objects */
    delete_object(p->depth, y, x);

    /* Reset cave feature */
    cave_set_feat(cave_get(p->depth), y, x, feat_old);

    q_free(queue);
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(struct player *p, int z1, int y1, int x1, int num, quark_t quark)
{
    object_type *i_ptr;
    object_type object_type_body;

    /* Acquirement */
    while (num--)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        object_wipe(i_ptr);

        /* Make a good (or great) object (if possible) */
        if (!make_object(p, cave_get(z1), i_ptr, object_level(z1), TRUE, TRUE, NULL)) continue;
        set_origin(i_ptr, ORIGIN_ACQUIRE, z1, 0);
        if (quark > 0) i_ptr->note = quark;

        /* Drop the object */
        drop_near(p, cave_get(z1), i_ptr, 0, y1, x1, TRUE);
    }
}


/*
 * Describe the charges on an item in the inventory.
 */
void inven_item_charges(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *o_ptr = &p_ptr->inventory[item];

    /* Require staff/wand */
    if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

    /* Require known item */
    if (!object_is_known(p_ptr, o_ptr)) return;

    /* Print a message */
    msg(p_ptr, "You have %d charge%s remaining.", o_ptr->pval[DEFAULT_PVAL],
        PLURAL(o_ptr->pval[DEFAULT_PVAL]));
}


/*
 * Describe an item in the inventory. Note: only called when an item is
 * dropped, used, or otherwise deleted from the inventory
 */
void inven_item_describe(struct player *p, int item)
{
    object_type *o_ptr = &p->inventory[item];
    char o_name[NORMAL_WID];

    /* Get a description */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Print a message */
    msg(p, "You have %s.", o_name);
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(struct player *p, int item, int num)
{
    object_type *o_ptr = &p->inventory[item];

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

        /* Add the weight */
        p->total_weight += (num * o_ptr->weight);

        /* Recalculate bonuses */
        p->update |= (PU_BONUS);

        /* Recalculate mana XXX */
        p->update |= (PU_MANA);

        /* Combine the pack */
        p->notice |= (PN_COMBINE);

        /* Redraw */
        p->redraw |= (PR_INVEN | PR_EQUIP);
    }
}


/*
 * Save the size of the quiver.
 */
void save_quiver_size(struct player *p, bool report)
{
    int i, count = 0;
    int maxsize = MAX_STACK_SIZE - 1;

    for (i = QUIVER_START; i < QUIVER_END; i++)
        if (p->inventory[i].kind) count += p->inventory[i].number;

    p->quiver_size = count;
    p->quiver_slots = (count + maxsize - 1) / maxsize;
    p->quiver_remainder = count % maxsize;

    if (report)
        Send_quiver_size(p, p->quiver_size, p->quiver_slots, p->quiver_remainder);
}


/*
 * Compare ammunition from slots (0-4); used for sorting.
 *
 * Returns -1 if slot1 should come first, 1 if slot2 should come first, or 0.
 */
static int compare_ammo(int slot1, int slot2)
{
    /* Right now there is no sorting criteria */
    return 0;
}


/*
 * Swap ammunition between quiver slots (0-4).
 */
static void swap_quiver_slots(struct player *p, int slot1, int slot2)
{
    int i = slot1 + QUIVER_START;
    int j = slot2 + QUIVER_START;
    object_type o;

    object_copy(&o, &p->inventory[i]);
    object_copy(&p->inventory[i], &p->inventory[j]);
    object_copy(&p->inventory[j], &o);
}


/*
 * Sorts the quiver--ammunition inscribed with @fN prefers to end up in quiver
 * slot N.
 */
void sort_quiver(struct player *p)
{
    bool locked[QUIVER_SIZE] = {FALSE, FALSE, FALSE, FALSE, FALSE};
    int desired[QUIVER_SIZE] = {-1, -1, -1, -1, -1};
    int i, j, k;
    object_type *o_ptr;

    /*
     * Here we figure out which slots have inscribed ammo, and whether that
     * ammo is already in the slot it "wants" to be in or not.
     */
    for (i = 0; i < QUIVER_SIZE; i++)
    {
        j = QUIVER_START + i;
        o_ptr = &p->inventory[j];

        /* Skip this slot if it doesn't have ammo */
        if (!o_ptr->kind) continue;

        /* Figure out which slot this ammo prefers, if any */
        k = get_inscribed_ammo_slot(o_ptr);
        if (!k) continue;

        k -= QUIVER_START;
        if (k == i) locked[i] = TRUE;
        if (desired[k] < 0) desired[k] = i;
    }

    /*
     * For items which had a preference that was not fulfilled, we will swap
     * them into the slot as long as it isn't already locked.
     */
    for (i = 0; i < QUIVER_SIZE; i++)
    {
        if (locked[i] || (desired[i] < 0)) continue;

        /* Item in slot 'desired[i]' desires to be in slot 'i' */
        swap_quiver_slots(p, desired[i], i);
        locked[i] = TRUE;
    }

    /*
     * Now we need to compact ammo which isn't in a preferred slot towards the
     * "front" of the quiver
     */
    for (i = 0; i < QUIVER_SIZE; i++)
    {
        /* If the slot isn't empty, skip it */
        if (p->inventory[QUIVER_START + i].kind) continue;

        /* Start from the end and find an unlocked item to put here. */
        for (j = QUIVER_SIZE - 1; j > i; j--)
        {
            if (!p->inventory[QUIVER_START + j].kind || locked[j]) continue;
            swap_quiver_slots(p, i, j);
            break;
        }
    }

    /* Now we will sort all other ammo using a simple insertion sort */
    for (i = 0; i < QUIVER_SIZE; i++)
    {
        k = i;
        if (!locked[k])
        {
            for (j = i + 1; j < QUIVER_SIZE; j++)
            {
                if (!locked[j] && (compare_ammo(k, j) > 0))
                    swap_quiver_slots(p, j, k);
            }
        }
    }
}


/*
 * Shifts ammo at or above the item slot towards the end of the quiver, making
 * room for a new piece of ammo.
 */
void open_quiver_slot(int Ind, int slot)
{
    player_type *p_ptr = player_get(Ind);
    int i, pref;
    int dest = QUIVER_END - 1;

    /* This should only be used on ammunition */
    if (slot < QUIVER_START) return;

    /* Quiver is full */
    if (p_ptr->inventory[QUIVER_END - 1].kind) return;

    /* Find the first open quiver slot */
    while (p_ptr->inventory[dest].kind) dest++;

    /*
     * Swap things with the space one higher (essentially moving the open space
     * towards our goal slot.
     */
    for (i = dest - 1; i >= slot; i--)
    {
        /*
         * If we have an item with an inscribed location (and it's in
         * that location) then we won't move it.
         */
        pref = get_inscribed_ammo_slot(&p_ptr->inventory[i]);
        if (i != slot && pref && pref == i) continue;

        /* Copy the item up and wipe the old slot */
        COPY(&p_ptr->inventory[dest], &p_ptr->inventory[i], object_type);
        dest = i;
        object_wipe(&p_ptr->inventory[dest]);
    }
}


/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(struct player *p, int item)
{
    object_type *o_ptr = &p->inventory[item];
    int i, j, slot, limit;

    /* Save a possibly new quiver size */
    if (item >= QUIVER_START) save_quiver_size(p, TRUE);

    /* Only optimize real items which are empty */
    if (!o_ptr->kind || o_ptr->number) return;

    /* Items in the pack are treated differently from other items */
    if (item < INVEN_WIELD)
    {
        p->inven_cnt--;
        p->redraw |= (PR_INVEN | PR_SPELL | PR_STUDY);
        limit = INVEN_MAX_PACK(p);
    }

    /* Items in the quiver and equipped items are (mostly) treated similarly */
    else
    {
        p->redraw |= PR_EQUIP;
        limit = ((item >= QUIVER_START)? QUIVER_END: 0);
    }

    /*
     * If the item is equipped (but not in the quiver), there is no need to
     * slide other items. Bonuses and such will need to be recalculated
     */
    if (!limit)
    {
        /* Erase the empty slot */
        object_wipe(&p->inventory[item]);

        /* Recalculate stuff */
        p->update |= (PU_BONUS);
        p->update |= (PU_TORCH);
        p->update |= (PU_MANA);
    }

    /* Slide everything down */
    for (j = item, i = item + 1; i < limit; i++)
    {
        if ((limit == QUIVER_END) && p->inventory[i].kind)
        {
            /*
             * If we have an item with an inscribed location (and it's in
             * that location) then we won't move it.
             */
            slot = get_inscribed_ammo_slot(&p->inventory[i]);
            if (slot && (slot == i)) continue;
        }
        COPY(&p->inventory[j], &p->inventory[i], object_type);
        j = i;
    }

    /* Reorder the quiver if necessary */
    if (item >= QUIVER_START) sort_quiver(p);

    /* Wipe the left-over object on the end */
    object_wipe(&p->inventory[j]);
}


/*
 * Increase the "number" of an item on the floor
 */
void floor_item_increase(struct player *p, int item, int num)
{
    object_type *o_ptr = object_byid(item);

    /* Apply */
    num += o_ptr->number;

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= o_ptr->number;

    /* Change the number */
    o_ptr->number += num;

    /* Redraw */
    if (o_ptr->number) p->redraw |= (PR_FLOOR | PR_ITEMLIST);
}


/*
 * Optimize an item on the floor (destroy "empty" items)
 */
void floor_item_optimize(int item)
{
    object_type *o_ptr = object_byid(item);

    /* Paranoia -- be sure it exists */
    if (!o_ptr->kind) return;

    /* Only optimize empty items */
    if (o_ptr->number) return;

    /* Delete it */
    delete_object_idx(item);
}


/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(struct player *p, object_type *o_ptr)
{
    /* Empty slot? */
    if (p->inven_cnt < INVEN_MAX_PACK(p)) return TRUE;

    /* Check if it can stack */
    if (inven_stack_okay(p, o_ptr)) return TRUE;

    /* Nope */
    return FALSE;
}


/*
 * Returns whether the pack is holding the maximum number of items. The max
 * size is INVEN_MAX_PACK, which is a macro since quiver size affects slots
 * available.
 */
static bool pack_is_full(struct player *p)
{
    return (p->inventory[INVEN_MAX_PACK(p) - 1].kind? TRUE: FALSE);
}


/*
 * Check to see if an item is stackable in the inventory
 */
bool inven_stack_okay(struct player *p, object_type *o_ptr)
{
    /* Similar slot? */
    int j;

    /*
     * If our pack is full and we're adding too many missiles, there won't be
     * enough room in the quiver, so don't check it.
     */
    int limit;

    /* The pack has more room */
    if (!pack_is_full(p)) limit = ALL_INVEN_TOTAL;

    /* Quiver already maxed out */
    else if (p->quiver_remainder == 0) limit = INVEN_PACK;

    /* Too much new ammo */
    else if (p->quiver_remainder + o_ptr->number >= MAX_STACK_SIZE) limit = INVEN_PACK;

    /* The quiver has more room */
    else limit = ALL_INVEN_TOTAL;

    for (j = 0; j < limit; j++)
    {
        /* Get that item */
        object_type *j_ptr = &p->inventory[j];

        /* Skip equipped items and non-objects */
        if ((j >= INVEN_PACK) && (j < QUIVER_START)) continue;
        if (!j_ptr->kind) continue;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr, OSTACK_PACK)) return (TRUE);
    }

    return (FALSE);
}


/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", otherwise,
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
s16b inven_carry(struct player *p, struct object *o, bool report)
{
    int i, j, k;
    int n = -1;
    object_type *j_ptr;

    /* Object is now owned */
    object_own(p, o);

    /* Check for combining */
    for (j = 0; j < INVEN_PACK; j++)
    {
        j_ptr = &p->inventory[j];

        /* Skip empty items */
        if (!j_ptr->kind) continue;

        /* Hack -- Track last item */
        n = j;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o, OSTACK_PACK))
        {
            /* Combine the items */
            object_absorb(j_ptr, o);

            /* Increase the weight */
            p->total_weight += (o->number * o->weight);

            /* Recalculate bonuses */
            p->update |= (PU_BONUS);

            /* Redraw */
            p->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL | PR_STUDY);

            /* Save quiver size */
            save_quiver_size(p, report);

            /* Success */
            return (j);
        }
    }

    /* Paranoia */
    if (p->inven_cnt > INVEN_MAX_PACK(p)) return (-1);

    /* Find an empty slot */
    for (j = 0; j <= INVEN_MAX_PACK(p); j++)
    {
        j_ptr = &p->inventory[j];

        /* Use it if found */
        if (!j_ptr->kind) break;
    }

    /* Use that slot */
    i = j;

    /* Reorder the pack */
    if (i < INVEN_MAX_PACK(p))
    {
        s32b o_value, j_value;

        /* Get the "value" of the item */
        o_value = o->kind->cost;

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_MAX_PACK(p); j++)
        {
            j_ptr = &p->inventory[j];

            /* Use empty slots */
            if (!j_ptr->kind) break;

            /* Hack -- readable books always come first */
            if ((o->tval == p->clazz->spell_book) &&
                (j_ptr->tval != p->clazz->spell_book)) break;
            if ((j_ptr->tval == p->clazz->spell_book) &&
                (o->tval != p->clazz->spell_book)) continue;

            /* Objects sort by decreasing type */
            if (o->tval > j_ptr->tval) break;
            if (o->tval < j_ptr->tval) continue;

            /* Non-aware (flavored) items always come last */
            if (!object_flavor_is_aware(p, o)) continue;
            if (!object_flavor_is_aware(p, j_ptr)) break;

            /* Objects sort by increasing sval */
            if (o->sval < j_ptr->sval) break;
            if (o->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!object_is_known(p, o)) continue;
            if (!object_is_known(p, j_ptr)) break;

            /* Determine the "value" of the pack item */
            j_value = j_ptr->kind->cost;

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
            object_copy(&p->inventory[k + 1], &p->inventory[k]);
        }

        /* Wipe the empty slot */
        object_wipe(&p->inventory[i]);
    }

    /* Copy the item */
    object_copy(&p->inventory[i], o);

    /* Get the new object */
    j_ptr = &p->inventory[i];

    /* Forget stack */
    j_ptr->next_o_idx = 0;

    /* Forget monster */
    j_ptr->held_m_idx = 0;

    /* Forget location */
    j_ptr->iy = j_ptr->ix = j_ptr->depth = 0;

    /* Increase the weight */
    p->total_weight += (j_ptr->number * j_ptr->weight);

    /* Count the items */
    p->inven_cnt++;

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Combine and Reorder pack */
    p->notice |= (PN_COMBINE | PN_REORDER);

    /* Redraw */
    p->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL | PR_STUDY);

    /* Hobbits ID mushrooms on pickup, gnomes ID wands and staffs on pickup */
    if (!object_is_known(p, j_ptr))
    {
        if (player_has(p, PF_KNOW_MUSHROOM) && (j_ptr->tval == TV_FOOD))
        {
            do_ident_item(p, i, j_ptr);
            msg(p, "Mushrooms for breakfast!");
        }

        if (player_has(p, PF_KNOW_ZAPPER) &&
            ((j_ptr->tval == TV_WAND) || (j_ptr->tval == TV_STAFF)))
                do_ident_item(p, i, j_ptr);
    }

    /* Save quiver size */
    save_quiver_size(p, report);

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
s16b inven_takeoff(struct player *p, int item, int amt)
{
    int slot;
    object_type *o_ptr;
    object_type *i_ptr;
    object_type object_type_body;
    const char *act;
    char o_name[NORMAL_WID];

    /* Get the item to take off */
    o_ptr = &p->inventory[item];

    /* Paranoia */
    if (amt <= 0) return (-1);

    /* The inscription prevents it */
    __trapR(p, CGI(o_ptr, 't', FALSE), -1)

    /* Verify */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain a local object */
    object_copy(i_ptr, o_ptr);

    /* Modify quantity */
    i_ptr->number = amt;

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Took off weapon */
    if (item == INVEN_WIELD)
        act = "You were wielding";

    /* Took off bow/light/missiles */
    else if ((item == INVEN_BOW) || (item == INVEN_LIGHT) ||
        ((item >= QUIVER_START) && (item < QUIVER_END)))
            act = "You were holding";

    /* Took off tool */
    else if (item == INVEN_TOOL)
        act = "You were using";

    /* Took off something */
    else
        act = "You were wearing";

    /* Modify, Optimize */
    inven_item_increase(p, item, 0 - amt);
    inven_item_optimize(p, item);

    /* Carry the object */
    slot = inven_carry(p, i_ptr, TRUE);

    /* Message */
    msgt(p, MSG_WIELD, "%s %s (%c).", act, o_name, index_to_label(slot));

    p->notice |= PN_SQUELCH;

    /* Redraw */
    p->redraw |= (PR_EQUIP | PR_PLUSSES);

    /* Return slot */
    return (slot);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
bool inven_drop(struct player *p, int item, int amt, bool bypass_inscr)
{
    int py = p->py;
    int px = p->px;
    object_type *o_ptr;
    object_type *i_ptr;
    object_type object_type_body;
    char o_name[NORMAL_WID];
    byte drop_state;

    /* Get the original object */
    o_ptr = &p->inventory[item];

    /* Error check */
    if (amt <= 0) return TRUE;

    /* Not too many */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* The inscription prevents it */
    __trapR(p, CGI(o_ptr, 'd', FALSE) && !bypass_inscr, TRUE)

    /* Never drop true artifacts above their base depth except the Crown and Grond */
    if (!cfg_artifact_drop_shallow && true_artifact_p(o_ptr) &&
        (p->depth < o_ptr->artifact->level) &&
        (o_ptr->artifact->aidx != ART_MORGOTH) && (o_ptr->artifact->aidx != ART_GROND))
    {
        if (!bypass_inscr) msg(p, "You cannot drop this here.");
        return FALSE;
    }

    /* Never drop items in wrong house */
    if (!check_store_drop(p, o_ptr))
    {
        if (!bypass_inscr) msg(p, "You cannot drop this here.");
        return FALSE;
    }

    /* Take off equipment */
    if (item >= INVEN_WIELD)
    {
        /* Take off first */
        item = inven_takeoff(p, item, amt);

        /* Get the original object */
        o_ptr = &p->inventory[item];

        /* Redraw */
        p->redraw |= (PR_EQUIP);
    }

    /* Get local object */
    i_ptr = &object_type_body;

    /* Get a copy with the right "amt" */
    object_distribute_amt(i_ptr, o_ptr, amt);

    /* Drop it (carefully) near the player */
    drop_state = drop_near(p, cave_get(p->depth), i_ptr, 0, py, px, FALSE);
    if (drop_state == DROP_ERROR)
    {
        if (!bypass_inscr) msg(p, "You cannot drop this here.");
        return FALSE;
    }

    /* Message */
    if (drop_state == DROP_OK)
    {
        object_desc(p, o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);
        msg(p, "You drop %s (%c).", o_name, index_to_label(item));
    }

    /* Modify, Describe, Optimize */
    inven_item_increase(p, item, 0 - amt);
    inven_item_describe(p, item);
    inven_item_optimize(p, item);

    return TRUE;
}


/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(struct player *p)
{
    int i, j, k;
    object_type *o_ptr;
    object_type *j_ptr;
    bool flag = FALSE;

    /* Combine the pack (backwards) */
    for (i = INVEN_PACK; i > 0; i--)
    {
        bool slide = FALSE;

        /* Get the item */
        o_ptr = &p->inventory[i];

        /* Skip empty items */
        if (!o_ptr->kind) continue;

        /* Scan the items above that item */
        for (j = 0; j < i; j++)
        {
            /* Get the item */
            j_ptr = &p->inventory[j];

            /* Skip empty items */
            if (!j_ptr->kind) continue;

            /* Can we drop "o_ptr" onto "j_ptr"? */
            if (object_similar(j_ptr, o_ptr, OSTACK_PACK))
            {
                /* Take note */
                flag = slide = TRUE;

                /* Add together the item counts */
                object_absorb(j_ptr, o_ptr);

                break;
            }
        }

        /* Compact the inventory */
        if (slide)
        {
            /* One object is gone */
            p->inven_cnt--;

            /* Slide everything down */
            for (k = i; k < INVEN_PACK; k++)
            {
                /* Hack -- slide object */
                object_copy(&p->inventory[k], &p->inventory[k + 1]);
            }

            /* Hack -- wipe hole */
            object_wipe(&p->inventory[k]);

            /* Redraw */
            p->redraw |= (PR_INVEN | PR_SPELL | PR_STUDY);
        }
    }

    /* Message */
    if (flag) msg(p, "You combine some items in your pack.");

    /* Auto-id */
    if (check_state(p, OF_KNOWLEDGE)) identify_pack(p);
}


/*
 * Reorder items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void reorder_pack(struct player *p)
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
        /* Get the item */
        o_ptr = &p->inventory[i];

        /* Skip empty slots */
        if (!o_ptr->kind) continue;

        /* Get the "value" of the item */
        o_value = o_ptr->kind->cost;

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_PACK; j++)
        {
            /* Get the item already there */
            j_ptr = &p->inventory[j];

            /* Use empty slots */
            if (!j_ptr->kind) break;

            /* Hack -- readable books always come first */
            if ((o_ptr->tval == p->clazz->spell_book) &&
                (j_ptr->tval != p->clazz->spell_book)) break;
            if ((j_ptr->tval == p->clazz->spell_book) &&
                (o_ptr->tval != p->clazz->spell_book)) continue;

            /* Objects sort by decreasing type */
            if (o_ptr->tval > j_ptr->tval) break;
            if (o_ptr->tval < j_ptr->tval) continue;

            /* Non-aware (flavored) items always come last */
            if (!object_flavor_is_aware(p, o_ptr)) continue;
            if (!object_flavor_is_aware(p, j_ptr)) break;

            /* Objects sort by increasing sval */
            if (o_ptr->sval < j_ptr->sval) break;
            if (o_ptr->sval > j_ptr->sval) continue;

            /* Unidentified objects always come last */
            if (!object_is_known(p, o_ptr)) continue;
            if (!object_is_known(p, j_ptr)) break;

            /* Determine the "value" of the pack item */
            j_value = j_ptr->kind->cost;

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
        object_copy(i_ptr, &p->inventory[i]);

        /* Slide the objects */
        for (k = i; k > j; k--)
        {
            /* Slide the item */
            object_copy(&p->inventory[k], &p->inventory[k-1]);
        }

        /* Insert the moving item */
        object_copy(&p->inventory[j], i_ptr);

        /* Redraw */
        p->redraw |= (PR_INVEN);
    }

    /* Message */
    if (flag) msg(p, "You reorder some items in your pack.");
}


/*
 * Returns the number of times in 1000 that @ will FAIL
 */
int get_use_device_chance(struct player *p, const object_type *o_ptr)
{
    int lev, fail, numerator, denominator;
    int skill = p->state.skills[SKILL_DEVICE];
    int skill_min = 10;
    int skill_max = 141;
    int diff_min  = 1;
    int diff_max  = 100;

    /* Extract the item level, which is the difficulty rating */
    if (o_ptr->artifact)
    {
        artifact_type *a_ptr = get_artifact(o_ptr);

        lev = a_ptr->level;
    }
    else
        lev = o_ptr->kind->level;

    numerator   = (skill - lev) - (skill_max - diff_min);
    denominator = (lev - skill) - (diff_max - skill_min);

    /* Make sure that we don't divide by zero */
    if (denominator == 0) denominator = ((numerator > 0)? 1: -1);

    fail = (100 * numerator) / denominator;

    /* Ensure failure rate is between 1% and 75% */
    if (fail > 750) fail = 750;
    if (fail < 10) fail = 10;

    return fail;
}


/*
 * Distribute charges of rods, staves, or wands.
 *
 * o_ptr = source item
 * q_ptr = target item, must be of the same type as o_ptr
 * amt   = number of items that are transfered
 */
static void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt)
{
    int charge_time = randcalc(o_ptr->kind->time, 0, AVERAGE), max_time;

    /*
     * Hack -- If staves or wands are dropped, the total maximum
     * charges need to be allocated between the two stacks.
     * If all the items are being dropped, it makes for a neater message
     * to leave the original stack's pval alone.
     */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
    {
        q_ptr->pval[DEFAULT_PVAL] = o_ptr->pval[DEFAULT_PVAL] * amt / o_ptr->number;

        if (amt < o_ptr->number) o_ptr->pval[DEFAULT_PVAL] -= q_ptr->pval[DEFAULT_PVAL];
    }

    /*
     * Hack -- Rods also need to have their timeouts distributed.
     *
     * The dropped stack will accept all time remaining to charge up to
     * its maximum.
     */
    if (o_ptr->tval == TV_ROD)
    {
        max_time = charge_time * amt;

        if (o_ptr->timeout > max_time) q_ptr->timeout = max_time;
        else q_ptr->timeout = o_ptr->timeout;

        if (amt < o_ptr->number) o_ptr->timeout -= q_ptr->timeout;
    }
}


void reduce_charges(object_type *o_ptr, int amt)
{
    /*
     * Hack -- If rods or wands are destroyed, the total maximum timeout or
     * charges of the stack needs to be reduced, unless all the items are
     * being destroyed.
     */
    if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) &&
        (amt < o_ptr->number))
            o_ptr->pval[DEFAULT_PVAL] -= o_ptr->pval[DEFAULT_PVAL] * amt / o_ptr->number;
    if ((o_ptr->tval == TV_ROD) && (amt < o_ptr->number))
        o_ptr->timeout -= o_ptr->timeout * amt / o_ptr->number;
}


int number_charging(const object_type *o_ptr)
{
    int charge_time, num_charging;

    charge_time = randcalc(o_ptr->time, 0, AVERAGE);

    /* Item has no timeout */
    if (charge_time <= 0) return 0;

    /* No items are charging */
    if (o_ptr->timeout <= 0) return 0;

    /* Calculate number charging based on timeout */
    num_charging = (o_ptr->timeout + charge_time - 1) / charge_time;

    /* Number charging cannot exceed stack size */
    if (num_charging > o_ptr->number) num_charging = o_ptr->number;

    return num_charging;
}


bool recharge_timeout(object_type *o_ptr)
{
    int charging_before, charging_after;

    /* Find the number of charging items */
    charging_before = number_charging(o_ptr);

    /* Nothing to charge */
    if (charging_before == 0) return FALSE;

    /* Decrease the timeout */
    o_ptr->timeout -= MIN(charging_before, o_ptr->timeout);

    /* Find the new number of charging items */
    charging_after = number_charging(o_ptr);

    /* Return true if at least 1 item obtained a charge */
    if (charging_after < charging_before) return TRUE;

    return FALSE;
}


/*
 * Sort comparator for objects using only tval and sval
 * -1 if o1 should be first
 *  1 if o2 should be first
 *  0 if it doesn't matter
 */
static int compare_types(const object_type *o1, const object_type *o2)
{
    if (o1->tval == o2->tval) return CMP(o1->sval, o2->sval);
    return CMP(o1->tval, o2->tval);
}


/*
 * Sort comparator for objects
 * -1 if o1 should be first
 *  1 if o2 should be first
 *  0 if it doesn't matter
 *
 * The sort order is designed with the "list items" command in mind.
 */
static int compare_items(struct player *p, const object_type *o1, const object_type *o2)
{
    /* Unknown objects go at the end, order doesn't matter */
    if (is_unknown(p, o1) || is_unknown(p, o2))
    {
        if (!is_unknown(p, o1)) return -1;
        return 1;
    }

    /* Known artifacts will sort first */
    if (object_is_known_artifact(o1) && object_is_known_artifact(o2))
        return compare_types(o1, o2);
    if (object_is_known_artifact(o1)) return -1;
    if (object_is_known_artifact(o2)) return 1;

    /* Unknown objects will sort next */
    if (!object_flavor_is_aware(p, o1) && !object_flavor_is_aware(p, o2))
        return compare_types(o1, o2);
    if (!object_flavor_is_aware(p, o1)) return -1;
    if (!object_flavor_is_aware(p, o2)) return 1;

    /* If only one of them is worthless, the other comes first */
    if ((o1->kind->cost == 0) && (o2->kind->cost != 0)) return 1;
    if ((o1->kind->cost != 0) && (o2->kind->cost == 0)) return -1;

    /* Otherwise, just compare tvals and svals */
    /* NOTE: arguably there could be a better order than this */
    return compare_types(o1, o2);
}


/*
 * Display visible items, similar to display_monlist
 */
void display_itemlist(struct player *p, bool do_cmd)
{
    int mx, my;
    unsigned i, disp_count = 0;
    object_type *types[MAX_ITEMLIST + 1];
    int counts[MAX_ITEMLIST + 1];
    int dx[MAX_ITEMLIST + 1], dy[MAX_ITEMLIST + 1];
    unsigned counter = 0;
    byte attr;

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Hallucination is weird */
    if (p->timed[TMD_IMAGE])
    {
        /* Clear display and print note */
        text_out_c(p, TERM_ORANGE, "Your hallucinations are too wild to see things clearly.");

        /* Restore height and width of current dungeon level */
        text_out_done(p);

        /* Done */
        return;
    }

    /* Scan the object list */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);
        unsigned j;

        /* Skip "dead" objects */
        if (!o_ptr->kind) continue;

        /* Paranoia: skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Make sure it's on the same dungeon level */
        if (p->depth != o_ptr->depth) continue;

        /* Only visible objects */
        if (!object_marked(p, i)) continue;

        if (!is_unknown(p, o_ptr) && squelch_item_ok(p, o_ptr)) continue;
        if (o_ptr->tval == TV_GOLD) continue;

        my = o_ptr->iy;
        mx = o_ptr->ix;

        /* See if we've already seen a similar item; if so, just add to its count */
        for (j = 0; j < counter; j++)
        {
            /* Unknown items cannot be stacked */
            if (object_similar(o_ptr, types[j], OSTACK_LIST) && !is_unknown(p, o_ptr))
            {
                counts[j] += o_ptr->number;
                if ((my - p->py) * (my - p->py) + (mx - p->px) * (mx - p->px) <
                    dy[j] * dy[j] + dx[j] * dx[j])
                {
                    dy[j] = my - p->py;
                    dx[j] = mx - p->px;
                }
                break;
            }
        }

        /*
         * We saw a new item. So insert it at the end of the list and
         * then sort it forward using compare_items(). The types list
         * is always kept sorted.
         */
        if (j == counter)
        {
            types[counter] = o_ptr;
            counts[counter] = o_ptr->number;
            dy[counter] = my - p->py;
            dx[counter] = mx - p->px;

            while ((j > 0) && (compare_items(p, types[j - 1], types[j]) > 0))
            {
                object_type *tmp_o = types[j - 1];
                int tmpcount = counts[j - 1];
                int tmpdx = dx[j - 1];
                int tmpdy = dy[j - 1];

                types[j - 1] = types[j];
                types[j] = tmp_o;
                dx[j - 1] = dx[j];
                dx[j] = tmpdx;
                dy[j - 1] = dy[j];
                dy[j] = tmpdy;
                counts[j - 1] = counts[j];
                counts[j] = tmpcount;
                j--;
            }

            /* Paranoia */
            if (counter < MAX_ITEMLIST) counter++;
        }
    }

    /* Note no visible items */
    if (!counter)
    {
        /* Clear display and print note */
        text_out_c(p, TERM_SLATE, "You see no items.");

        /* Restore height and width of current dungeon level */
        text_out_done(p);

        /* Done */
        return;
    }

    /* Message */
    text_out(p, "You can see %d item%s:", counter, PLURAL(counter));
    text_out(p, "\n\n");

    for (i = 0; i < counter; i++)
    {
        /* o_name will hold the object_desc() name for the object. */
        /* o_desc will also need to put a (x4) behind it. */
        /* Can there be more than 999 stackable items on a level? */
        char o_name[NORMAL_WID];
        char o_desc[NORMAL_WID];
        object_type *o_ptr = types[i];

        /* Page wrap */
        if (!do_cmd)
        {
            disp_count++;
            if (disp_count == p->max_hgt - 4)
            {
                text_out(p, "... and %d more\n", counter - disp_count);
                break;
            }
        }

        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_FULL);
        if ((counts[i] > 1) && !is_unknown(p, o_ptr))
        {
            strnfmt(o_desc, sizeof(o_desc), " %s (x%d) %d %c, %d %c", o_name, counts[i],
                ((dy[i] > 0)? dy[i]: 0 - dy[i]), ((dy[i] > 0)? 'S': 'N'),
                ((dx[i] > 0)? dx[i]: 0 - dx[i]), ((dx[i] > 0)? 'E': 'W'));
        }
        else
        {
            strnfmt(o_desc, sizeof(o_desc), " %s %d %c, %d %c", o_name,
                ((dy[i] > 0)? dy[i]: 0 - dy[i]), ((dy[i] > 0)? 'S': 'N'),
                ((dx[i] > 0)? dx[i]: 0 - dx[i]), ((dx[i] > 0)? 'E': 'W'));
        }

        /* Unknown object */
        if (is_unknown(p, o_ptr))
            attr = TERM_RED;

        /* Known artifact */
        else if (object_is_known_artifact(o_ptr))
            attr = TERM_VIOLET;

        /* Unaware of kind */
        else if (!object_flavor_is_aware(p, o_ptr))
            attr = TERM_L_RED;

        /* Worthless */
        else if (o_ptr->kind->cost == 0)
            attr = TERM_SLATE;

        /* Default */
        else
            attr = TERM_WHITE;

        /* Display the pict */
        if (!is_unknown(p, o_ptr))
        {
            byte a;
            char c;

            if (p->tile_distorted)
            {
                a = o_ptr->kind->d_attr;
                c = o_ptr->kind->d_char;

                if (o_ptr->kind->flavor && !(p->obj_aware[o_ptr->kind->kidx] && a && c))
                {
                    a = o_ptr->kind->flavor->d_attr;
                    c = o_ptr->kind->flavor->d_char;
                }

                /* Multi-hued object */
                if (a == TERM_MULTI) a = TERM_VIOLET;

                text_out_c(p, a, "%c", c);
            }
            else
            {
                a = object_attr(p, o_ptr);
                c = object_char(p, o_ptr);

                /* Multi-hued object */
                if (a == TERM_MULTI) a = TERM_VIOLET;

                p->info[p->info_y][p->info_x].a = a;
                p->info[p->info_y][p->info_x].c = c;
                p->info_x++;
            }
        }
        else if (p->tile_distorted)
            text_out_c(p, k_info[6].d_attr, "%c", k_info[6].d_char);
        else
        {
            p->info[p->info_y][p->info_x].a = object_kind_attr(p, &k_info[6]);
            p->info[p->info_y][p->info_x].c = object_kind_char(p, &k_info[6]);
            p->info_x++;
        }

        /* Print and bump line counter */
        text_out_c(p, attr, o_desc);
        text_out(p, "\n");
    }

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}


/*** Generic utility functions ***/


/* Get an o_ptr from an item number */
object_type *object_from_item_idx(struct player *p, int item, int inv_start, bool prompt)
{
    s16b this_o_idx, next_o_idx;

    /* Get the item (from pack or equipment) */
    if (item >= inv_start) return &p->inventory[item];

    /* Restrict equipment */
    if (inv_start) return NULL;

    /* Get the item (on the floor) */
    for (this_o_idx = cave_get(p->depth)->o_idx[p->py][p->px]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Ignore all non-objects */
        if (!o_ptr->kind) continue;

        /* Ignore all hidden objects */
        if (!object_marked(p, this_o_idx) || squelch_item_ok(p, o_ptr)) continue;

        /* Check item (on the floor) */
        if (this_o_idx == 0 - item) return object_byid(this_o_idx);
    }

    /* Nothing */
    if (prompt) msg(p, "There's nothing on the floor.");
    return NULL;
}


/*
 * Returns whether the pack is holding the more than the maximum number of
 * items. The max size is INVEN_MAX_PACK, which is a macro since quiver size
 * affects slots available. If this is true, calling pack_overflow() will
 * trigger a pack overflow.
 */
static bool pack_is_overfull(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    return (p_ptr->inventory[INVEN_MAX_PACK(p_ptr)].kind? TRUE: FALSE);
}


/*
 * Overflow an item from the pack, if it is overfull.
 */
void pack_overflow(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int item = INVEN_MAX_PACK(p_ptr);
    char o_name[NORMAL_WID];
    object_type *o_ptr;
    int amt;
    byte drop_state;

    if (!pack_is_overfull(Ind)) return;

    /* Get the slot to be dropped */
    o_ptr = &p_ptr->inventory[item];
    amt = o_ptr->number;

    /* Disturbing */
    disturb(p_ptr, 0, 0);

    /* Warning */
    msg(p_ptr, "Your pack overflows!");

    /* Drop it (carefully) near the player */
    drop_state = drop_near(p_ptr, cave_get(p_ptr->depth), o_ptr, 0, p_ptr->py, p_ptr->px,
        FALSE);
    if (drop_state == DROP_ERROR)
    {
        /* Message */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        msg(p_ptr, "The %s fades into the air!", o_name);

        /* Preserve any true artifact */
        preserve_artifact_aux(o_ptr);
        history_lose_artifact(p_ptr, o_ptr);
    }

    /* Message */
    if (drop_state == DROP_OK)
    {
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
        msg(p_ptr, "You drop %s.", o_name);
    }

    /* Modify, Describe, Optimize */
    inven_item_increase(p_ptr, item, 0 - amt);
    inven_item_describe(p_ptr, item);
    inven_item_optimize(p_ptr, item);
}


/*** PWMAngband ***/


void get_object_info(struct player *p, object_type *o_ptr, byte *attr, byte *act, byte *fuel,
    byte *fail, int *slot)
{
    object_type *j_ptr;

    /* Get a color */
    *attr = p->tval_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

    /* Get the "activation" flag */
    *act = 0;
    if (o_ptr->tval == TV_ROD)
    {
        /* Get direction choice from the sval */
        if (is_rod_aimed(o_ptr)) *act = 1;

        /* Force direction choice for unknown rods */
        if (!object_flavor_is_aware(p, o_ptr)) *act = 1;
    }

    /* Get the "fuelable" flag */
    *fuel = 0;
    if ((o_ptr->tval == TV_LIGHT) && is_lamp(o_ptr))
    {
        bitflag f[OF_SIZE];

        /* Get flags */
        object_flags(o_ptr, f);

        /* Non-empty, non-everburning lamps are okay */
        if ((o_ptr->timeout > 0) && !of_has(f, OF_NO_FUEL)) *fuel = 1;
    }

    /* Get the "fail" flag */
    *fail = 255;
    if (object_effect_is_known(p, o_ptr) && !is_food(o_ptr) && (o_ptr->tval != TV_POTION) &&
        (o_ptr->tval != TV_SCROLL))
    {
        *fail = (9 + get_use_device_chance(p, o_ptr)) / 10;
    }

    /* Get the "slot" flag */
    *slot = wield_slot(p, o_ptr);
    if (*slot == -1) return;
    j_ptr = &p->inventory[*slot];

    /*
     * Usually if the slot is taken we'll just replace the item in the slot,
     * but in some cases we need to ask the user which slot they actually
     * want to replace
     */
    if (!j_ptr->kind || ((o_ptr->tval != TV_RING) &&
        (!obj_is_ammo(p, o_ptr) || object_similar(j_ptr, o_ptr, OSTACK_QUIVER))))
    {
        *slot = -1;
    }
}


int get_owner_id(const object_type *o_ptr)
{
    int ind;

    if (!o_ptr->owner) return 0;

    for (ind = 1; ind < NumPlayers + 1; ind++)
    {
        player_type *p_ptr = player_get(ind);

        if (p_ptr->id == o_ptr->owner) return ind;
    }

    return 0;
}


void set_artifact_info(struct player *p, const object_type *o_ptr, byte info)
{
    byte *pinfo;

    /* Paranoia */
    if (!p) return;

    /* Not an artifact */
    if (!o_ptr->artifact) return;

    /* True artifacts */
    if (true_artifact_p(o_ptr)) pinfo = p->art_info;
    else pinfo = p->randart_info;

    /* Add history entry */
    switch (info)
    {
        case ARTS_GENERATED:
            history_add_artifact(p, o_ptr, FALSE);
            break;
        case ARTS_FOUND:
            history_add_artifact(p, o_ptr, TRUE);
            break;
        case ARTS_ABANDONED:
        case ARTS_SOLD:
            history_lose_artifact(p, o_ptr);
            break;
    }

    /* Only once */
    if (pinfo[o_ptr->artifact->aidx] >= info) return;

    /* Register info */
    pinfo[o_ptr->artifact->aidx] = info;
}


void object_absorb_origin(object_type *o_ptr, object_type *j_ptr)
{
    /* Combine origin data as best we can */
    if ((o_ptr->origin != j_ptr->origin) ||
        (o_ptr->origin_depth != j_ptr->origin_depth) ||
        (o_ptr->origin_xtra != j_ptr->origin_xtra))
    {
        int act = 2;

        if (o_ptr->origin_xtra && j_ptr->origin_xtra)
        {
            monster_race *r_ptr = &r_info[o_ptr->origin_xtra];
            monster_race *s_ptr = &r_info[j_ptr->origin_xtra];

            bool r_uniq = rf_has(r_ptr->flags, RF_UNIQUE);
            bool s_uniq = rf_has(s_ptr->flags, RF_UNIQUE);

            if (r_uniq && !s_uniq) act = 0;
            else if (s_uniq && !r_uniq) act = 1;
            else act = 2;
        }

        switch (act)
        {
            /* Overwrite with j_ptr */
            case 1:
            {
                set_origin(o_ptr, j_ptr->origin, j_ptr->origin_depth,
                    j_ptr->origin_xtra);
                break;
            }

            /* Set as "mixed" */
            case 2:
            {
                set_origin(o_ptr, ORIGIN_MIXED, 0, 0);
                break;
            }
        }
    }
}


/* Index of the first high book by tval */
static byte book_min_good[] =
{
    4, 4, 4, 3, 1, 3, 3, 255, 2
};


/*
 * Other "kind" values for "good" templates.
 * Used by the auto-squelch system to avoid squelching "good" items at
 * "average" level.
 */
bool kind_is_good_other(const object_kind *kind)
{
    /* Analyze the item type */
    switch (kind->tval)
    {
        /* Armor */
        case TV_DRAG_ARMOR:
        {
            /* Dragon Scale Mails are good */
            return (TRUE);
        }

        /* Books */
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_SORCERY_BOOK:
        case TV_SHADOW_BOOK:
        case TV_HUNT_BOOK:
        case TV_PSI_BOOK:
        case TV_DEATH_BOOK:
        case TV_ELEM_BOOK:
        case TV_SUMMON_BOOK:
        {
            /* High level books are good */
            if (kind->sval >= book_min_good[kind->tval - TV_MAGIC_BOOK])
                return (TRUE);

            return (FALSE);
        }

        /* Rings */
        case TV_RING:
        {
            /* Rings of speed are good */
            if (kind->sval == SV_RING_SPEED) return (TRUE);

            return (FALSE);
        }

        /* Amulets */
        case TV_AMULET:
        {
            /* High level amulets are good (those with a level rating boost) */
            if (kind->sval == SV_AMULET_THE_MAGI) return (TRUE);
            if (kind->sval == SV_AMULET_DEVOTION) return (TRUE);
            if (kind->sval == SV_AMULET_WEAPONMASTERY) return (TRUE);
            if (kind->sval == SV_AMULET_TRICKERY) return (TRUE);

            return (FALSE);
        }
    }

    /* Assume not good */
    return (FALSE);
}


void set_origin(object_type *i_ptr, byte origin, s16b origin_depth, u16b origin_xtra)
{
    i_ptr->origin = origin;
    i_ptr->origin_depth = origin_depth;
    i_ptr->origin_xtra = origin_xtra;
}


/*
 * Hack -- process the objects
 */
void process_objects(void)
{
    int k, i, j;
    object_type *o_ptr;

    /* Every 10 game turns */
    if ((turn.turn % 10) != 5) return;

    /* Process objects */
    for (k = o_top - 1; k >= 0; k--)
    {
        /* Access the index */
        i = o_fast[k];

        /* Get the object */
        o_ptr = object_byid(i);

        /* Excise "dead" objects */
        if (!o_ptr->kind)
        {
            /* Excise the object */
            o_fast[k] = o_fast[--o_top];

            /* Skip */
            continue;
        }

        /* Shimmer multi-hued objects */
        if (object_shimmer(o_ptr) && cave_get(o_ptr->depth))
        {
            /* Check everyone */
            for (j = 1; j < NumPlayers + 1; j++)
            {
                player_type *q_ptr = player_get(j);

                /* If he's not here, skip him */
                if (q_ptr->depth != o_ptr->depth) continue;

                /* Actually light that spot for that player */
                if (allow_shimmer(q_ptr))
                    cave_light_spot_aux(q_ptr, cave_get(o_ptr->depth), o_ptr->iy, o_ptr->ix);
            }
        }

        /* Recharge rods on the ground */
        if ((o_ptr->tval == TV_ROD) && recharge_timeout(o_ptr))
            redraw_floor(o_ptr->depth, o_ptr->iy, o_ptr->ix);

        /* Corpses on the ground slowly decompose */
        if (o_ptr->tval == TV_CORPSE)
        {
            o_ptr->pval[DEFAULT_PVAL + 1]--;

            /* Notice changes */
            if (o_ptr->pval[DEFAULT_PVAL + 1] == o_ptr->timeout / 5)
                redraw_floor(o_ptr->depth, o_ptr->iy, o_ptr->ix);

            /* No more corpse... */
            else if (!o_ptr->pval[DEFAULT_PVAL + 1])
                delete_object_idx(i);
        }
    }
}


/*
 * Place dungeon objects
 */
void place_objects(int depth)
{
    int i;

    /* Read the dungeon items */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip objects on depths that aren't allocated */
        if (!cave_get(o_ptr->depth)) continue;

        /* Skip objects not on this depth */
        if (depth && (o_ptr->depth != depth)) continue;

        /* Dungeon floor */
        if (!o_ptr->held_m_idx)
        {
            int x = o_ptr->ix;
            int y = o_ptr->iy;

            /* ToDo: Verify coordinates */

            /* Skip already placed objects */
            if (cave_get(o_ptr->depth)->o_idx[y][x] == i) continue;

            /* Link the object to the pile */
            o_ptr->next_o_idx = cave_get(o_ptr->depth)->o_idx[y][x];

            /* Link the floor to the object */
            cave_get(o_ptr->depth)->o_idx[y][x] = i;
        }
    }
}


/*
 * Prepare an object `dst` representing `amt` objects,  based on an existing
 * object `src` representing at least `amt` objects.
 *
 * Takes care of the charge redistribution concerns of stacked items.
 */
void object_distribute_amt(object_type *dst, object_type *src, int amt)
{
    /* Get a copy of the object */
    object_copy(dst, src);

    /* Modify quantity */
    dst->number = amt;

    /* Distribute the charges of rods/wands/staves between the stacks */
    distribute_charges(src, dst, amt);
}


bool is_owner(struct player *p, object_type *o_ptr)
{
    /* Free object */
    if (!o_ptr->owner) return TRUE;

    /* No restriction */
    if (!OPT_P(p, birth_no_stores)) return TRUE;

    /* Must be the owner */
    return (o_ptr->owner == p->id);
}


void object_own(struct player *p, object_type *o_ptr)
{
    o_ptr->askprice = 0;

    /* Check ownership */
    if (!obj_own_p(p, o_ptr))
    {
        char o_name[NORMAL_WID];
        char buf[512];
        const char *owner_name;
        hash_entry *ptr;

        /* Owner name */
        ptr = lookup_player(o_ptr->owner);
        owner_name = ((ptr && ht_zero(&ptr->death_turn))? ptr->name: "(deceased)");

        /* Log transaction */
        strnfmt(buf, sizeof(buf), "TR %s-%ld | %s-%ld $ %ld", owner_name,
            (long)o_ptr->owner, p->name, (long)p->id, (long)object_value(p, o_ptr, 1));
        audit(buf);

        /* Object name */
        object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
        strnfmt(buf, sizeof(buf), "TR+%s", o_name);
        audit(buf);

        /* Hack -- potion of experience */
        if (o_ptr->kind->effect == EF_GAIN_EXP) o_ptr->askprice = 1;
    }

    /* Set ownership */
    o_ptr->owner = p->id;

    /* Artifact is now owned */
    if (true_artifact_p(o_ptr)) o_ptr->artifact->owned = 1;
}


static int get_creator_id(const object_type *o_ptr)
{
    int ind;

    if (!o_ptr->creator) return 0;

    for (ind = 1; ind < NumPlayers + 1; ind++)
    {
        player_type *p_ptr = player_get(ind);

        if (p_ptr->id == o_ptr->creator) return ind;
    }

    return 0;
}


void preserve_artifact_aux(const object_type *o_ptr)
{
    /* Not an artifact */
    if (!o_ptr->artifact) return;

    /* True artifacts */
    if (true_artifact_p(o_ptr))
    {
        if (o_ptr->artifact->created) o_ptr->artifact->created--;
        o_ptr->artifact->owned = 0;
    }

    /* Randarts */
    else
    {
        /* Only works when creator is ingame */
        int Ind = get_creator_id(o_ptr);

        if (Ind > 0) player_get(Ind)->randart_created[o_ptr->artifact->aidx] = 0;
    }
}


void preserve_artifact(const object_type *o_ptr)
{
    player_type *p_ptr;

    /* Not an artifact */
    if (!o_ptr->artifact) return;

    /* Only works when owner is ingame */
    p_ptr = player_get(get_owner_id(o_ptr));

    /* Preserve any artifact */
    preserve_artifact_aux(o_ptr);
    if (p_ptr) history_lose_artifact(p_ptr, o_ptr);
}


bool is_sense_machine(const object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HORN:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LIGHT: return TRUE;
    }

    return FALSE;
}


/*
 * Return an object's activation.
 */
bool object_activation(struct player *p, const object_type *o_ptr, bool known, int *effect)
{
    if (known || object_effect_is_known(p, o_ptr))
    {
        *effect = o_ptr->effect;
        return FALSE;
    }
    if (o_ptr->effect) return TRUE;
    return FALSE;
}


void item_decrease(struct player *p, int item, int amount, bool describe)
{
    /* Reduce and describe inventory */
    if (item >= 0)
    {
        inven_item_increase(p, item, 0 - amount);
        if (describe) inven_item_describe(p, item);
        inven_item_optimize(p, item);
    }

    /* Reduce and describe floor item */
    else
    {
        floor_item_increase(p, 0 - item, 0 - amount);
        floor_item_optimize(0 - item);
    }
}


/*
 * Redraw changes occured on floor items
 */
void redraw_floor(int depth, int y, int x)
{
    int i;

    /* Redraw changes for all players */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Same depth */
        if (p_ptr->depth != depth) continue;

        /* Redraw */
        p_ptr->redraw |= PR_ITEMLIST;

        /* Under a player */
        if ((p_ptr->py != y) || (p_ptr->px != x)) continue;

        /* Redraw */
        p_ptr->redraw |= PR_FLOOR;
    }
}


/*
 * Clear visibility flags
 */
void clear_visibility(s16b o_idx, int depth, bool check_depth)
{
    int i;

    /* Redraw changes for all players */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Same depth */
        if (check_depth && (p_ptr->depth != depth)) continue;

        /* This player cannot see it */
        p_ptr->obj_marked[o_idx] = MARK_UNAWARE;

        /* Redraw */
        if (!check_depth) p_ptr->redraw |= PR_ITEMLIST;
    }
}


struct object *object_byid(s16b oidx)
{
    my_assert(oidx >= 0);
    my_assert(oidx <= z_info->o_max);

    return &o_list[oidx];
}


void objects_init(void)
{
    o_list = C_ZNEW(z_info->o_max, struct object);
    o_fast = C_ZNEW(z_info->o_max, s16b);
}


void objects_destroy(void)
{
    mem_free(o_list);
    mem_free(o_fast);
}
