/*
 * File: house.c
 * Purpose: House code.
 *
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


static struct house_type *houses;
static size_t num_houses = 0;
static size_t alloc_houses = 0;
static size_t num_custom = 0;


/*
 * Maximum number of custom houses available.
 */
#define MAX_HOUSES  1024


/*
 * Initialize the house package
 */
void houses_init(void)
{
    alloc_houses = MAX_HOUSES;
    houses = mem_zalloc(alloc_houses * sizeof(struct house_type));
}


/*
 * De-initialize the house package
 */
void houses_free(void)
{
    mem_free(houses);
}


/*
 * Count the number of houses
 */
int houses_count(void)
{
    return (int)num_houses;
}


/*  
 * Determine if the player is inside the house
 */
bool house_inside(struct player *p, int house)
{
    /* Paranoia */
    if ((house < 0) || (house >= houses_count())) return false;

    /* Skip unallocated houses */
    if (!houses[house].state) return false;

    /* Skip houses not on this level */
    if (!wpos_eq(&houses[house].wpos, &p->wpos)) return false;

    /* Player is inside the house */
    return loc_between(&p->grid, &houses[house].grid_1, &houses[house].grid_2);
}


/*
 * Determine if the player owns the house
 */
bool house_owned_by(struct player *p, int house)
{
    /* Paranoia */
    if ((house < 0) || (house >= houses_count())) return false;

    return (houses[house].state && (houses[house].ownerid > 0) && (p->id == houses[house].ownerid));
}


/*
 * Count the number of owned houses
 */
int houses_owned(struct player *p)
{
    int house, count = 0;

    for (house = 0; house < houses_count(); house++)
    {
        /* Only normal or extended houses */
        if (house_owned_by(p, house) && (houses[house].state < HOUSE_CUSTOM)) count++;
    }

    return count;
}


/*
 * Return the index of a house given a coordinate grid
 */
int pick_house(struct worldpos *wpos, struct loc *grid)
{
    int i;

    /* Check each house */
    for (i = 0; i < houses_count(); i++)
    {
        /* Check this one */
        if (houses[i].state && loc_eq(&houses[i].door, grid) && wpos_eq(&houses[i].wpos, wpos))
        {
            /* Return */
            return i;
        }
    }

    /* Failure */
    return -1;
}


/*
 * Given coordinates return a house to which they belong.
 * Houses can be overlapping, so a single coordinate grid may match several
 * houses. The offset parameter allows searching for the next match.
 */
int find_house(struct player *p, struct loc *grid, int offset)
{
    int i;

    for (i = offset; i < houses_count(); i++)
    {
        struct loc prev, next;

        loc_init(&prev, houses[i].grid_1.x - 1, houses[i].grid_1.y - 1);
        loc_init(&next, houses[i].grid_2.x + 1, houses[i].grid_2.y + 1);

        /* Check the house position *including* the walls */
        if (houses[i].state && wpos_eq(&houses[i].wpos, &p->wpos) &&
            loc_between(grid, &prev, &next))
        {
            /* We found the house this section of wall belongs to */
            return i;
        }
    }
    return -1;
}


/*
 * Set house owner
 */
void set_house_owner(struct player *p, struct house_type *house)
{
    house->ownerid = p->id;
    my_strcpy(house->ownername, p->name, sizeof(house->ownername));
    house->color = COLOUR_WHITE;
}


/*
 * Get an empty house slot
 */
int house_add(bool custom)
{
    int house;

    /* Check maximum number of custom houses */
    if (custom && (num_custom == MAX_HOUSES)) return -1;

    /* Increment number of custom houses */
    if (custom) num_custom++;

    /* Get an empty house slot */
    for (house = 0; house < houses_count(); house++)
    {
        if (!houses[house].state) break;
    }

    /* Check maximum number of houses */
    if (house == houses_count())
    {
        /* Check number of allocated houses */
        if (num_houses == alloc_houses)
        {
            /* Extend the house array */
            alloc_houses += MAX_HOUSES;
            houses = mem_realloc(houses, alloc_houses * sizeof(struct house_type));
        }

        /* Increment number of houses */
        num_houses++;
    }

    return house;
}


/*
 * Set house
 */
void house_set(int slot, struct house_type *house)
{
    /* Paranoia */
    if ((slot < 0) || (slot >= houses_count())) return;

    memcpy(&houses[slot], house, sizeof(struct house_type));
}


/*
 * Get the sector as a "centered" panel
 */
static void loc_panel(struct player *p, struct loc *grid, struct loc *offset)
{
    int screen_hgt = p->screen_rows / p->tile_hgt;
    int screen_wid = p->screen_cols / p->tile_wid;

    int panel_wid = screen_wid / 2;
    int panel_hgt = screen_hgt / 2;

    /* Hack -- enforce illegal panel */
    loc_init(offset, z_info->dungeon_wid, z_info->dungeon_hgt);

    /* Scroll screen vertically when off-center */
    if (grid->y != offset->y + panel_hgt)
        offset->y = grid->y - panel_hgt;

    /* Scroll screen vertically when 3 grids from top/bottom edge */
    else if ((grid->y < offset->y + 3) || (grid->y >= offset->y + screen_hgt - 3))
        offset->y = grid->y - panel_hgt;

    /* Scroll screen horizontally when off-center */
    if (grid->x != offset->x + panel_wid)
        offset->x = grid->x - panel_wid;

    /* Scroll screen horizontally when 3 grids from left/right edge */
    else if ((grid->x < offset->x + 3) || (grid->x >= offset->x + screen_wid - 3))
        offset->x = grid->x - panel_wid;

    /* Verify offset->y, adjust if needed */
    if (offset->y > z_info->dungeon_hgt - screen_hgt) offset->y = z_info->dungeon_hgt - screen_hgt;
    if (offset->y < 0) offset->y = 0;

    /* Verify offset->x, adjust if needed */
    if (offset->x > z_info->dungeon_wid - screen_wid) offset->x = z_info->dungeon_wid - screen_wid;
    if (offset->x < 0) offset->x = 0;
}


/*
 * List owned houses in a file
 */
void house_list(struct player *p, ang_file *fff)
{
    int i, j = 0;
    char buf[160];
    char dpt[13];

    int panel_wid = PANEL_SIZE / p->tile_wid;
    int panel_hgt = PANEL_SIZE / p->tile_hgt;

    for (i = 0; i < houses_count(); i++)
    {
        const char *where = "at";
        struct loc grid;

        if (!house_owned_by(p, i)) continue;

        j++;

        dpt[0] = '\0';
        wild_cat_depth(&houses[i].wpos, dpt, sizeof(dpt));

        /* Get the sector of the door house as a "centered" panel */
        loc_panel(p, &houses[i].door, &grid);

        if (in_town(&houses[i].wpos)) where = "in";

        strnfmt(buf, sizeof(buf), "  %c) House %d %s %s, sector [%d,%d]\n", I2A(j - 1), j, where,
            dpt, (grid.y / panel_hgt), (grid.x / panel_wid));
        file_put(fff, buf);
    }
    if (!j) file_put(fff, "You do not own any house.\n");
}


/*
 * Determine if the level contains owned houses
 */
bool level_has_owned_houses(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        /* House on this level and owned? */
        if (houses[i].state && wpos_eq(&houses[i].wpos, wpos) && (houses[i].ownerid > 0))
            return true;
    }

    return false;
}


/*
 * Wipe custom houses on a level
 */
void wipe_custom_houses(struct worldpos *wpos)
{
    int house;

    for (house = 0; house < houses_count(); house++)
    {
        /* Skip houses not on this level */
        if (!wpos_eq(&houses[house].wpos, wpos)) continue;

        /* Wipe extended and custom houses */
        if (houses[house].state >= HOUSE_EXTENDED)
        {
            memset(&houses[house], 0, sizeof(struct house_type));
            num_custom--;
        }
    }
}


/*
 * Determine if the player has stored items in houses
 */
bool has_home_inventory(struct player *p)
{
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        struct loc_iterator iter;

        if (!house_owned_by(p, i)) continue;

        loc_iterator_first(&iter, &houses[i].grid_1, &houses[i].grid_2);

        do
        {
            if (square_object(chunk_get(&houses[i].wpos), &iter.cur)) return true;
        }
        while (loc_iterator_next(&iter));
    }

    return false;
}


/*
 * Dump content of owned houses in a file
 */
void house_dump(struct player *p, ang_file *fp)
{
    int i, j;
    char o_name[NORMAL_WID];

    /* Header */
    file_put(fp, "  [House List]\n\n");

    /* Dump all available items */
    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);
        struct loc_iterator iter;

        if (!house_owned_by(p, i)) continue;

        loc_iterator_first(&iter, &houses[i].grid_1, &houses[i].grid_2);

        if (i > 0) file_put(fp, "\n");
        j = 0;

        do
        {
            struct object *obj;

            for (obj = square_object(c, &iter.cur); obj; obj = obj->next)
            {
                /* Display groups of 26 objects */
                if (j == 26)
                {
                    file_put(fp, "\n");
                    j = 0;
                }

                object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
                file_putf(fp, "%c) %s\n", I2A(j), o_name);
                j++;
            }
        }
        while (loc_iterator_next(&iter));
    }

    /* Add an empty line */
    file_put(fp, "\n\n");
}


/*
 * Determine if the location is inside a house
 */
bool location_in_house(struct worldpos *wpos, struct loc *grid)
{
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        /* Check this one */
        if (houses[i].state && wpos_eq(&houses[i].wpos, wpos) &&
            loc_between(grid, &houses[i].grid_1, &houses[i].grid_2))
        {
            return true;
        }
    }

    return false;
}


/*
 * Get house
 */
struct house_type *house_get(int house)
{
    /* Paranoia */
    if ((house < 0) || (house >= houses_count())) return NULL;

    return &houses[house];
}


/*
 * Reset house
 */
void reset_house(int house)
{
    int i;
    struct chunk *c = chunk_get(&houses[house].wpos);
    struct loc_iterator iter;

    /* House is no longer owned */
    houses[house].ownername[0] = '\0';
    houses[house].ownerid = 0;
    houses[house].color = 0;
    houses[house].free = 0;

    /* Remove all players from the house */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (house_inside(p, house))
        {
            msg(p, "You have been expelled from the house.");
            do
            {
                struct source who_body;
                struct source *who = &who_body;

                source_player(who, get_player_index(get_connection(p->conn)), p);
                effect_simple(EF_TELEPORT, who, "10", 0, 0, 0, 0, 0, NULL);
            }
            while (house_inside(p, house));
        }
    }

    /* Close the door */
    square_colorize_door(c, &houses[house].door, 0);

    loc_iterator_first(&iter, &houses[house].grid_1, &houses[house].grid_2);

    /* Reset the house */
    do
    {
        /* Delete the objects */
        square_excise_pile(c, &iter.cur);
    }
    while (loc_iterator_next(&iter));
}


/*
 * Reset owned houses
 */
void reset_houses(struct player *p)
{
    int i;

    /* Clear his houses */
    for (i = 0; i < houses_count(); i++)
    {
        /* Is house owned? */
        if (house_owned_by(p, i))
        {
            /* House is no longer owned */
            reset_house(i);
        }
    }
}


/*
 * Know content of owned houses
 */
void know_houses(struct player *p)
{
    struct object *obj;
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);
        struct loc_iterator iter;

        if (!house_owned_by(p, i)) continue;

        loc_iterator_first(&iter, &houses[i].grid_1, &houses[i].grid_2);

        do
        {
            for (obj = square_object(c, &iter.cur); obj; obj = obj->next)
                object_notice_everything(p, obj);
        }
        while (loc_iterator_next(&iter));
    }
}


static byte door_color(int missile_attr)
{
    return get_color(missile_attr, ATTR_DOOR, 1);
}


/*
 * Colorize house door
 */
void colorize_door(struct player *p, struct object_kind *kind, struct chunk *c, struct loc *grid)
{
    int house, i;

    /* Find the color of the missile */
    byte m_attr = (kind->flavor? flavor_x_attr[kind->flavor->fidx]: kind_x_attr[kind->kidx]);

    /* Pick a house */
    if ((house = pick_house(&p->wpos, grid)) == -1) return;

    /* Must own the house */
    if (!house_owned_by(p, house)) return;

    /* Find suitable color */
    for (i = 1; i < BASIC_COLORS; i++)
    {
        if (feat_x_attr[FEAT_HOME_CLOSED + i][LIGHTING_LIT] != door_color(m_attr)) continue;

        /* Perform colorization */
        houses[house].color = i;
        square_colorize_door(c, grid, i);

        /* Done */
        break;
    }
}


/*
 * Return the name of a player owned store
 */
bool get_player_store_name(int num, char *name, int len)
{
    struct loc_iterator iter;
    const char *c;
    struct chunk *cv = chunk_get(&houses[num].wpos);

    /* Default title */
    my_strcpy(name, "Shop", len);

    loc_iterator_first(&iter, &houses[num].grid_1, &houses[num].grid_2);

    /* Scan house */
    do
    {
        struct object *obj;

        /* Scan all objects in the grid */
        for (obj = square_object(cv, &iter.cur); obj; obj = obj->next)
        {
            /* If there was an object, does it have a store name? */
            if (obj->note)
            {
                c = my_stristr(quark_str(obj->note), "store name");
                if (c)
                {
                    /* Get name */
                    c += 10; /* skip "store name" */
                    if (*c++ == ' ')
                    {
                        my_strcpy(name, c, len);
                        return true;
                    }
                }
            }
        }
    }
    while (loc_iterator_next(&iter));

    return false;
}


/*
 * Return the index of a house near a location.
 *
 * Returns -3 if invalid dimensions, -2 if not owned, -1 if not found, or the index if found.
 */
int house_near(struct player *p, struct loc *grid1, struct loc *grid2)
{
    int house;

    for (house = 0; house < houses_count(); house++)
    {
        /* Skip unallocated houses */
        if (!houses[house].state) continue;

        /* Skip houses not on this level */
        if (!wpos_eq(&houses[house].wpos, &p->wpos)) continue;

        /* Skip houses far away */
        if ((houses[house].grid_2.x + 2 < grid1->x) || (houses[house].grid_1.x - 2 > grid2->x) ||
            (houses[house].grid_2.y + 2 < grid1->y) || (houses[house].grid_1.y - 2 > grid2->y))
        {
            continue;
        }

        /* Check north and south */
        if ((grid1->y == houses[house].grid_2.y + 2) || (grid2->y == houses[house].grid_1.y - 2))
        {
            /* Do we own this house? */
            if (!house_owned_by(p, house)) return -2;

            /* Can we extend this house? */
            if ((grid1->x == houses[house].grid_1.x - 1) && (grid2->x == houses[house].grid_2.x + 1))
                return house;

            return -3;
        }

        /* Check east and west */
        if ((grid1->x == houses[house].grid_2.x + 2) || (grid2->x == houses[house].grid_1.x - 2))
        {
            /* Do we own this house? */
            if (!house_owned_by(p, house)) return -2;

            /* Can we extend this house? */
            if ((grid1->y == houses[house].grid_1.y - 1) && (grid2->y == houses[house].grid_2.y + 1))
                return house;

            return -3;
        }
    }

    return -1;
}


/*
 * Extend house
 */
bool house_extend(void)
{
    /* Check maximum number of custom houses */
    if (num_custom == MAX_HOUSES) return false;

    /* Increment number of custom houses */
    num_custom++;

    return true;
}


/*
 * Memorize the content of owned houses
 */
void memorize_houses(struct player *p)
{
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);
        struct loc_iterator iter;

        if (!house_owned_by(p, i)) continue;

        /* Only on the current level */
        if (!wpos_eq(&houses[i].wpos, &p->wpos)) continue;

        loc_iterator_first(&iter, &houses[i].grid_1, &houses[i].grid_2);

        do
        {
            square_know_pile(p, c, &iter.cur);
        }
        while (loc_iterator_next(&iter));
    }
}
