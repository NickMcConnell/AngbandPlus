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
    if (!COORDS_EQUAL(&houses[house].wpos, &p->wpos)) return false;

    /* Player is inside the house */
    return ((p->px >= houses[house].x_1) && (p->px <= houses[house].x_2) &&
        (p->py >= houses[house].y_1) && (p->py <= houses[house].y_2));
}


/*
 * Determine if the player owns the house
 */
bool house_owned_by(struct player *p, int house)
{
    /* Paranoia */
    if ((house < 0) || (house >= houses_count())) return false;

    return (houses[house].state && (houses[house].ownerid > 0) &&
        (p->id == houses[house].ownerid));
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
 * Return the index of a house given a coordinate pair
 */
int pick_house(struct worldpos *wpos, int y, int x)
{
    int i;

    /* Check each house */
    for (i = 0; i < houses_count(); i++)
    {
        /* Check this one */
        if (houses[i].state && (houses[i].door_x == x) && (houses[i].door_y == y) &&
            COORDS_EQUAL(&houses[i].wpos, wpos))
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
 * Houses can be overlapping, so a single coordinate pair may match several
 * houses. The offset parameter allows searching for the next match.
 */
int find_house(struct player *p, int x, int y, int offset)
{
    int i;

    for (i = offset; i < houses_count(); i++)
    {
        /* Check the house position *including* the walls */
        if (houses[i].state && COORDS_EQUAL(&houses[i].wpos, &p->wpos) &&
            (x >= houses[i].x_1 - 1) && (x <= houses[i].x_2 + 1) &&
            (y >= houses[i].y_1 - 1) && (y <= houses[i].y_2 + 1))
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
    house->color = PLAYER_STORE_BM;
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
 * Verify the current panel (relative to a location).
 */
static void verify_panel_loc(struct player *p, int py, int px, s16b *sy, s16b *sx)
{
    int screen_hgt, screen_wid;
    int panel_wid, panel_hgt;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    panel_wid = screen_wid / 2;
    panel_hgt = screen_hgt / 2;

    /* Scroll screen vertically when 3 grids from top/bottom edge */
    if ((py < *sy + 3) || (py >= *sy + screen_hgt - 3))
        *sy = py - panel_hgt;

    /* Scroll screen horizontally when 3 grids from left/right edge */
    if ((px < *sx + 3) || (px >= *sx + screen_wid - 3))
        *sx = px - panel_wid;

    /* Verify wy, adjust if needed */
    if (*sy > z_info->dungeon_hgt - screen_hgt) *sy = z_info->dungeon_hgt - screen_hgt;
    if (*sy < 0) *sy = 0;

    /* Verify wx, adjust if needed */
    if (*sx > z_info->dungeon_wid - screen_wid) *sx = z_info->dungeon_wid - screen_wid;
    if (*sx < 0) *sx = 0;
}


/*
 * List owned houses in a file
 */
void house_list(struct player *p, ang_file *fff)
{
    int i, j = 0;
    char buf[160];
    s16b sy, sx;
    char dpt[13];
    int panel_wid, panel_hgt;

    panel_wid = PANEL_SIZE / p->tile_wid;
    panel_hgt = PANEL_SIZE / p->tile_hgt;

    for (i = 0; i < houses_count(); i++)
    {
        const char *where = "at";

        if (!house_owned_by(p, i)) continue;

        j++;

        dpt[0] = '\0';
        wild_cat_depth(&houses[i].wpos, dpt, sizeof(dpt));

        verify_panel_loc(p, houses[i].y_1, houses[i].x_1, &sy, &sx);

        if (in_town(&houses[i].wpos)) where = "in";

        strnfmt(buf, sizeof(buf), "  %c) House %d %s %s, sector [%d,%d]\n", I2A(j - 1), j, where,
            dpt, (sy / panel_hgt), (sx / panel_wid));
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
        if (houses[i].state && COORDS_EQUAL(&houses[i].wpos, wpos) && (houses[i].ownerid > 0))
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
        if (!COORDS_EQUAL(&houses[house].wpos, wpos)) continue;

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
    int i, x, y;

    for (i = 0; i < houses_count(); i++)
    {
        if (!house_owned_by(p, i)) continue;

        for (y = houses[i].y_1; y <= houses[i].y_2; y++)
        {
            for (x = houses[i].x_1; x <= houses[i].x_2; x++)
            {
                if (square_object(chunk_get(&houses[i].wpos), y, x)) return true;
            }
        }
    }

    return false;
}


/*
 * Dump content of owned houses in a file
 */
void house_dump(struct player *p, ang_file *fp)
{
    int i, j, x, y;
    char o_name[NORMAL_WID];

    /* Header */
    file_put(fp, "  [House List]\n\n");

    /* Dump all available items */
    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);

        if (!house_owned_by(p, i)) continue;

        if (i > 0) file_put(fp, "\n");
        j = 0;
        for (y = houses[i].y_1; y <= houses[i].y_2; y++)
        {
            for (x = houses[i].x_1; x <= houses[i].x_2; x++)
            {
                struct object *obj;

                for (obj = square_object(c, y, x); obj; obj = obj->next)
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
        }
    }

    /* Add an empty line */
    file_put(fp, "\n\n");
}


/*
 * Determine if the location is inside a house
 */
bool location_in_house(struct worldpos *wpos, int y, int x)
{
    int i;

    for (i = 0; i < houses_count(); i++)
    {
        /* Check this one */
        if (houses[i].state && COORDS_EQUAL(&houses[i].wpos, wpos) &&
            (houses[i].x_1 <= x) && (x <= houses[i].x_2) &&
            (houses[i].y_1 <= y) && (y <= houses[i].y_2))
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
    int i, yy, xx;
    struct chunk *c = chunk_get(&houses[house].wpos);

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
                effect_simple(EF_TELEPORT, who, "10", 0, 0, 0, NULL);
            }
            while (house_inside(p, house));
        }
    }

    /* Close the door */
    square_colorize_door(c, houses[house].door_y, houses[house].door_x, 0);

    /* Reset the house */
    for (yy = houses[house].y_1; yy <= houses[house].y_2; yy++)
    {
        for (xx = houses[house].x_1; xx <= houses[house].x_2; xx++)
        {
            /* Delete the objects */
            square_excise_pile(c, yy, xx);
        }
    }
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
    int i, y, x;

    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);

        if (!house_owned_by(p, i)) continue;

        for (y = houses[i].y_1; y <= houses[i].y_2; y++)
            for (x = houses[i].x_1; x <= houses[i].x_2; x++)
                for (obj = square_object(c, y, x); obj; obj = obj->next)
                    object_notice_everything(p, obj);
    }
}


static byte door_color(int missile_attr)
{
    return get_color(missile_attr, ATTR_DOOR, 1);
}


/*
 * Colorize house door
 */
void colorize_door(struct player *p, struct object_kind *kind, struct chunk *c, int y, int x)
{
    int shopnum = PLAYER_STORE_GENERAL, house;

    /* Find the color of the missile */
    byte m_attr = (kind->flavor? flavor_x_attr[kind->flavor->fidx]: kind_x_attr[kind->kidx]);

    /* Find suitable color */
    while (shopnum < PLAYER_STORE_MAX)
    {
        if (feat_x_attr[FEAT_HOME_CLOSED + shopnum][LIGHTING_LIT] == door_color(m_attr))
        {
            char store_name[NORMAL_WID];

            /* Pick a house */
            if ((house = pick_house(&p->wpos, y, x)) == -1) break;

            /* Must own the house */
            if (!house_owned_by(p, house)) break;

            /* Check house contents for non custom stores */
            if (!get_player_store_name(house, store_name, sizeof(store_name)))
            {
                int hy, hx;
                bool invalid = false;

                /* Scan house */
                for (hy = houses[house].y_1; (hy <= houses[house].y_2 && !invalid); hy++)
                {
                    for (hx = houses[house].x_1; (hx <= houses[house].x_2 && !invalid); hx++)
                    {
                        struct object *obj;

                        /* Objects */
                        for (obj = c->squares[hy][hx].obj; obj && !invalid; obj = obj->next)
                            invalid = !check_store_drop_color(p, obj, shopnum);
                    }
                }
                if (invalid) break;
            }

            /* Perform colorization */
            houses[house].color = shopnum;
            square_colorize_door(c, y, x, shopnum);

            /* Done */
            break;
        }

        shopnum++;
    }
}


/*
 * Return the name of a player owned store
 */
bool get_player_store_name(int num, char *name, int len)
{
    int x, y;
    const char *c;
    struct chunk *cv = chunk_get(&houses[num].wpos);

    /* Default title */
    switch (houses[num].color)
    {
        case PLAYER_STORE_GENERAL: my_strcpy(name, "General Store", len); break;
        case PLAYER_STORE_ARMOURY: my_strcpy(name, "Armoury", len); break;
        case PLAYER_STORE_SMITH: my_strcpy(name, "Weapon Shop", len); break;
        case PLAYER_STORE_TEMPLE: my_strcpy(name, "Ecclesial Shop", len); break;
        case PLAYER_STORE_ALCHEMIST: my_strcpy(name, "Alchemy Shop", len); break;
        case PLAYER_STORE_MAGIC: my_strcpy(name, "Magic Shop", len); break;
        case PLAYER_STORE_LIBRARY: my_strcpy(name, "Bookseller", len); break;
        case PLAYER_STORE_BM: my_strcpy(name, "Black Market", len); break;
        case PLAYER_STORE_XBM: my_strcpy(name, "Expensive Black Market", len); break;
        case PLAYER_STORE_TAVERN: my_strcpy(name, "Tavern", len); break;
        case PLAYER_STORE_HOME: my_strcpy(name, "Home", len); break;
    }

    /* Scan house */
    for (y = houses[num].y_1; y <= houses[num].y_2; y++)
    {
        for (x = houses[num].x_1; x <= houses[num].x_2; x++)
        {
            struct object *obj;

            /* Scan all objects in the grid */
            for (obj = square_object(cv, y, x); obj; obj = obj->next)
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
    }
    return false;
}


/*
 * Return the index of a house near a location.
 *
 * Returns -3 if invalid dimensions, -2 if not owned, -1 if not found, or the index if found.
 */
int house_near(struct player *p, int x1, int y1, int x2, int y2)
{
    int house;

    for (house = 0; house < houses_count(); house++)
    {
        /* Skip unallocated houses */
        if (!houses[house].state) continue;

        /* Skip houses not on this level */
        if (!COORDS_EQUAL(&houses[house].wpos, &p->wpos)) continue;

        /* Skip houses far away */
        if ((houses[house].x_2 + 2 < x1) || (houses[house].x_1 - 2 > x2) ||
            (houses[house].y_2 + 2 < y1) || (houses[house].y_1 - 2 > y2))
        {
            continue;
        }

        /* Check north and south */
        if ((y1 == houses[house].y_2 + 2) || (y2 == houses[house].y_1 - 2))
        {
            /* Do we own this house? */
            if (!house_owned_by(p, house)) return -2;

            /* Can we extend this house? */
            if ((x1 == houses[house].x_1 - 1) && (x2 == houses[house].x_2 + 1)) return house;

            return -3;
        }

        /* Check east and west */
        if ((x1 == houses[house].x_2 + 2) || (x2 == houses[house].x_1 - 2))
        {
            /* Do we own this house? */
            if (!house_owned_by(p, house)) return -2;

            /* Can we extend this house? */
            if ((y1 == houses[house].y_1 - 1) && (y2 == houses[house].y_2 + 1)) return house;

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
    int i, y, x;

    for (i = 0; i < houses_count(); i++)
    {
        struct chunk *c = chunk_get(&houses[i].wpos);

        if (!house_owned_by(p, i)) continue;

        /* Only on the current level */
        if (!COORDS_EQUAL(&houses[i].wpos, &p->wpos)) continue;

        for (y = houses[i].y_1; y <= houses[i].y_2; y++)
            for (x = houses[i].x_1; x <= houses[i].x_2; x++)
                square_know_pile(p, c, y, x);
    }
}
