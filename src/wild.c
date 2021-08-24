/* File: wild.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Wilderness generation
 *
 * CTK: Added infinitely scrolling wilderness support (hackish).
 */

#include "angband.h"
#include "rooms.h"
#include <assert.h>

monster_hook_type wilderness_mon_hook = NULL;

/* One way to scum the wilderness was to use "stair scumming", continually
   entering and leaving wild_mode while waiting for desired monsters to get
   spawned adjacent to the player. For example, dragons in the mountains
   were a good, quick source of experience and loot for a mid level player.
   Let's prevent any encounters when the player leaves wild mode (excepting ambushes,
   of course). To get encounters, the player must seek them by traveling about. */
bool no_encounters_hack = FALSE;

/* Scratch buffer for wilderness terrain creation */
s16b *_cave[MAX_HGT];

/* Boundary grids of cave[][] must be permanent walls mimicking the correct features.
   We set this up after generation or scrolling. But prior to scrolling, we need
   to undo this hack since a boundary grid will (probably) scroll into the interior
   of the viewport. Failure to mark boundaries as permanent causes numerous catastrophic
   access violations as a lot (most) code never checks in_bounds() before accessing
   the cave ... sigh. */
static void _set_boundary(void)
{
    int i;
    /* Special boundary walls -- North */
    for (i = 0; i < MAX_WID; i++)
    {
        cave[0][i].mimic = cave[0][i].feat;
        cave[0][i].feat = feat_permanent;
    }

    /* Special boundary walls -- South */
    for (i = 0; i < MAX_WID; i++)
    {
        cave[MAX_HGT - 1][i].mimic = cave[MAX_HGT - 1][i].feat;
        cave[MAX_HGT - 1][i].feat = feat_permanent;
    }

    /* Special boundary walls -- West */
    for (i = 1; i < MAX_HGT - 1; i++)
    {
        cave[i][0].mimic = cave[i][0].feat;
        cave[i][0].feat = feat_permanent;
    }

    /* Special boundary walls -- East */
    for (i = 1; i < MAX_HGT - 1; i++)
    {
        cave[i][MAX_WID - 1].mimic = cave[i][MAX_WID - 1].feat;
        cave[i][MAX_WID - 1].feat = feat_permanent;
    }
}
static void _unset_boundary(void)
{
    int i;
    /* Special boundary walls -- North */
    for (i = 0; i < MAX_WID; i++)
    {
        cave[0][i].feat = cave[0][i].mimic;
        cave[0][i].mimic = 0;
    }

    /* Special boundary walls -- South */
    for (i = 0; i < MAX_WID; i++)
    {
        cave[MAX_HGT - 1][i].feat = cave[MAX_HGT - 1][i].mimic;
        cave[MAX_HGT - 1][i].mimic = 0;
    }

    /* Special boundary walls -- West */
    for (i = 1; i < MAX_HGT - 1; i++)
    {
        cave[i][0].feat = cave[i][0].mimic;
        cave[i][0].mimic = 0;
    }

    /* Special boundary walls -- East */
    for (i = 1; i < MAX_HGT - 1; i++)
    {
        cave[i][MAX_WID - 1].feat = cave[i][MAX_WID - 1].mimic;
        cave[i][MAX_WID - 1].mimic = 0;
    }
}
static bool _scroll_panel(int dx, int dy)
{
    int y, x;
    rect_t r = ui_map_rect();

    y = viewport_origin.y + dy;
    x = viewport_origin.x + dx;

    if (y > cur_hgt - 3*r.cy/4) y = cur_hgt - 3*r.cy/4;
    if (y < -r.cy/4) y = -r.cy/4;

    if (x > cur_wid - 3*r.cx/4) x = cur_wid - 3*r.cx/4;
    if (x < -r.cx/4) x = -r.cx/4;

    if (y != viewport_origin.y || x != viewport_origin.x)
    {
        viewport_origin.x = x;
        viewport_origin.y = y;

        p_ptr->update |= PU_MONSTERS;
        p_ptr->redraw |= PR_MAP;

    /*  Don't: handle_stuff(); as this clears CAVE_TEMP flags! */
        return TRUE;
    }
    return FALSE;
}

static void _apply_glow(bool all)
{
    int y,x;
    cave_type *c_ptr;
    feature_type *f_ptr;

    for (y = 0; y < MAX_HGT; y++)
    {
        for (x = 0; x < MAX_WID ; x++)
        {
            c_ptr = &cave[y][x];
            if (all || (c_ptr->info & CAVE_TEMP))
            {
                f_ptr = &f_info[get_feat_mimic(c_ptr)];
                if (is_daytime())
                {
                    if ( (c_ptr->info & CAVE_ROOM)
                      && !have_flag(f_ptr->flags, FF_WALL)
                      && !have_flag(f_ptr->flags, FF_DOOR) )
                    {
                        /* TODO */
                    }
                    else
                    {
                        c_ptr->info |= CAVE_GLOW | CAVE_AWARE;
                        if (view_perma_grids) c_ptr->info |= CAVE_MARK;
                    }
                }
                else
                {
                    if ( !is_mirror_grid(c_ptr)
                      && !have_flag(f_ptr->flags, FF_QUEST_ENTER)
                      && !have_flag(f_ptr->flags, FF_ENTRANCE) )
                    {
                        c_ptr->info &= ~CAVE_GLOW;

                        /* Darken "boring" features */
                        if (!have_flag(f_ptr->flags, FF_REMEMBER))
                            c_ptr->info &= ~CAVE_MARK;
                    }
                    else if (have_flag(f_ptr->flags, FF_ENTRANCE))
                    {
                        c_ptr->info |= CAVE_GLOW | CAVE_AWARE;
                        if (view_perma_grids) c_ptr->info |= CAVE_MARK;
                    }
                }
                if (!all)
                    c_ptr->info &= ~CAVE_TEMP;
            }
        }
    }
}

static bool _is_boundary(int x, int y)
{
    if (x == 0 || x == MAX_WID - 1)
        return TRUE;
    if (y == 0 || y == MAX_HGT - 1)
        return TRUE;
    return FALSE;
}

static void _scroll_grid(int src_x, int src_y, int dest_x, int dest_y)
{
    cave_type *src = &cave[src_y][src_x];
    s16b       this_o_idx, next_o_idx = 0;

    assert(!(src->info & CAVE_TEMP));

    if (in_bounds2(dest_y, dest_x))
    {
        cave_type *dest = &cave[dest_y][dest_x];

        if (dest->m_idx)
            delete_monster_idx(dest->m_idx);

        for (this_o_idx = dest->o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            next_o_idx = o_list[this_o_idx].next_o_idx;
            delete_object_idx(this_o_idx);
        }

        *dest = *src;
        WIPE(src, cave_type);
        src->info |= CAVE_TEMP;  /* Mark for _apply_glow */

        if (dest->m_idx)
        {
            m_list[dest->m_idx].fy = dest_y;
            m_list[dest->m_idx].fx = dest_x;
            if (_is_boundary(dest_x, dest_y))
                delete_monster_idx(dest->m_idx);
        }
        for (this_o_idx = dest->o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            next_o_idx = o_list[this_o_idx].next_o_idx;
            o_list[this_o_idx].loc.y = dest_y;
            o_list[this_o_idx].loc.x = dest_x;
            if (_is_boundary(dest_x, dest_y))
                delete_object_idx(this_o_idx);
        }
    }
    else
    {
        if (src->m_idx)
            delete_monster_idx(src->m_idx);
        for (this_o_idx = src->o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            next_o_idx = o_list[this_o_idx].next_o_idx;
            delete_object_idx(this_o_idx);
        }
        WIPE(src, cave_type);
        src->info |= CAVE_TEMP;  /* Mark for _apply_glow */
    }
}

static void _scroll_cave(int dx, int dy)
{
    int x, y, i;

#if 1
    if (p_ptr->wizard)
    {
        cmsg_format(TERM_VIOLET, "Scroll Cave (%d,%d)", dx, dy);
        msg_boundary();
    }
#endif

    if (dy == 0 && dx == 0)
        return;

    forget_view();
    forget_lite();
    forget_flow();

    if (dy <= 0 && dx <= 0)
    {
        for (y = 0; y < MAX_HGT; y++)
        {
            for (x = 0; x < MAX_WID; x++)
                _scroll_grid(x, y, x + dx, y + dy);
        }
    }
    else if (dy >= 0 && dx >= 0)
    {
        for (y = MAX_HGT - 1; y >= 0 ; y--)
        {
            for (x = MAX_WID - 1; x >= 0; x--)
                _scroll_grid(x, y, x + dx, y + dy);
        }
    }
    else if (dy > 0 && dx < 0)
    {
        for (y = MAX_HGT - 1; y >= 0 ; y--)
        {
            for (x = 0; x < MAX_WID; x++)
                _scroll_grid(x, y, x + dx, y + dy);
        }
    }
    else if (dy < 0 && dx > 0)
    {
        for (y = 0; y < MAX_HGT; y++)
        {
            for (x = MAX_WID - 1; x >= 0; x--)
                _scroll_grid(x, y, x + dx, y + dy);
        }
    }
    else
    {
        /* ooops! */
    }

    px += dx;
    py += dy;

    for (i = 0; i < max_pack_info_idx; i++)
    {
        pack_info_t *pack_info_ptr = &pack_info_list[i];
        if (pack_info_ptr->ai == AI_GUARD_POS)
        {
            pack_info_ptr->guard_x += dx;
            pack_info_ptr->guard_y += dy;
        }
    }

    if (center_player && (center_running || (!running && !travel.run)))
    {
        /* Note: This is jerky if the panel is too big, but
           that is not our fault! Rather, the auto-center option
           fails to actually center the player as they approach
           the boundary of the cave. Shrink your display window
           enough and you will gain a very smooth scrolling experience! */
        viewport_verify_aux(VIEWPORT_FORCE_CENTER);
    }
    else
        _scroll_panel(dx, dy);

    p_ptr->update |= PU_DISTANCE | PU_VIEW | PU_LITE | PU_FLOW;
    p_ptr->redraw |= PR_MAP;
    p_ptr->window |= PW_OVERHEAD | PW_DUNGEON;
}

static bool _in_bounds(int x, int y)
{
    return x > 0 && x < (max_wild_x - 1)
        && y > 0 && y < (max_wild_y - 1);
}

int wilderness_level(int x, int y)
{
    int total = 0;
    int ct = 0;
    int dx, dy;

    if (wilderness[y][x].entrance || wilderness[y][x].town)
        return MIN(wilderness[y][x].level, 60);

    /* Average adjacent wilderness tiles to smooth out difficulty transitions */
    for (dx = -1; dx <= 1; dx++)
    {
        for (dy = -1; dy <= 1; dy++)
        {
            int x2 = x + dx;
            int y2 = y + dy;
            if (!_in_bounds(x2, y2)) continue;
            if (wilderness[y2][x2].terrain != TERRAIN_EDGE)
            {
                total += wilderness[y2][x2].level;
                ct++;
            }
        }
    }
    assert(ct);
    return MIN(total / ct, 60);
}

static void _generate_cave(rect_t exclude);
static void _generate_area(int x, int y, int dx, int dy, rect_t exclude);
static void _generate_encounters(int x, int y, rect_t r, rect_t exclude);
bool wilderness_scroll_lock = FALSE;

void wilderness_move_player(int old_x, int old_y)
{
    int     old_qx = old_x / WILD_SCROLL_CX;
    int     old_qy = old_y / WILD_SCROLL_CY; /* q is for "quadrant" and is a misnomer ... */
    int     qx = px / WILD_SCROLL_CX;
    int     qy = py / WILD_SCROLL_CY;
    int     wild_qx = qx + p_ptr->wilderness_dx; /* Which "quadrant" of the current wilderness tile? */
    int     wild_qy = qy + p_ptr->wilderness_dy;
    int     dx = qx - old_qx;
    int     dy = qy - old_qy;
    rect_t  viewport;
    rect_t  valid;
    bool    do_disturb = FALSE;

    if (no_wilderness)
        return;

    /* There are several ways we could scroll:
       [1] Use (dx,dy) calculated above (i.e. on every "quadrant" change).
       [2] Only scroll when the user hits a boundary "quadrant".

       Let's try [2] but I left the code in for [1] for easy reversion.
       Note that [1] allows "back and forth" "scroll scumming" for
       wilderness encounters while [2] would require extensive movement,
       so is probably to be preferred. */
    dx = 0;
    dy = 0;
    if (qx == 2)
        dx = 1;
    if (qx == 0)
        dx = -1;
    if (qy == 2)
        dy = 1;
    if (qy == 0)
        dy = -1;

#if 1
    /* Because I am so easily confused :( */
    if (p_ptr->wizard)
    {
        rect_t r = ui_char_info_rect();
        int    row = r.y + r.cy - 11;
        int    col = r.x;

        c_put_str(TERM_WHITE, format("P:%3d/%3d", px, py), row++, col);
        c_put_str(TERM_WHITE, format("W:%3d/%3d", p_ptr->wilderness_x, p_ptr->wilderness_y), row++, col);
        c_put_str(TERM_WHITE, format("D:%3d/%3d", p_ptr->wilderness_dx, p_ptr->wilderness_dy), row++, col);
        c_put_str(TERM_WHITE, format("O:%3d/%3d", old_qx, old_qy), row++, col);
        c_put_str(TERM_WHITE, format("Q:%3d/%3d", qx, qy), row++, col);
        c_put_str(TERM_WHITE, format("S:%3d/%3d", dx, dy), row++, col);
        c_put_str(TERM_WHITE, format("L:%3d", wilderness_level(p_ptr->wilderness_x, p_ptr->wilderness_y)), row++, col);
        c_put_str(TERM_WHITE, format("T:%3d", p_ptr->town_num), row++, col);
    }
#endif

    if (!dx && !dy)
        return;

    /* Some code might not be prepared for _scroll_cave ... For example, rush attacks build a path,
       and then repeatedly move the player. */
    if (wilderness_scroll_lock)
    {
    #if 0
        msg_format("Skip Scroll (%d,%d)", dx, dy);
    #endif
        return;
    }

    no_encounters_hack = FALSE;
    _unset_boundary();
    _scroll_cave(-dx*WILD_SCROLL_CX, -dy*WILD_SCROLL_CY);

    p_ptr->wilderness_dx += dx;
    p_ptr->wilderness_dy += dy;

    /* Patch up player's wilderness coordinates. If they are standing on (X,Y)
       then make sure wilderness_x and y reflect this. */
    if (wild_qx >= 3)
    {
        p_ptr->wilderness_x++;
        p_ptr->wilderness_dx -= 3;
        if (disturb_panel) do_disturb = TRUE;
    }
    else if (wild_qx < 0)
    {
        p_ptr->wilderness_x--;
        p_ptr->wilderness_dx += 3;
        if (disturb_panel) do_disturb = TRUE;
    }
    if (wild_qy >= 3)
    {
        p_ptr->wilderness_y++;
        p_ptr->wilderness_dy -= 3;
        if (disturb_panel) do_disturb = TRUE;
    }
    else if (wild_qy < 0)
    {
        p_ptr->wilderness_y--;
        p_ptr->wilderness_dy += 3;
        if (disturb_panel) do_disturb = TRUE;
    }

    /* Scroll in new wilderness info with appropriate monsters! */
    viewport = rect(0, 0, MAX_WID, MAX_HGT);
    valid = rect_translate(viewport, -dx*WILD_SCROLL_CX, -dy*WILD_SCROLL_CY);
    valid = rect_intersect(viewport, valid);
    _generate_cave(valid);
    _set_boundary();

    /* Note: While it is true that disturb() will cancel traveling, travel_step()
       will undo the effects of any disturb() calls processed by move_player() (which
       includes us, btw). I have no idea why this is so, but it is (undocumented from Henband).
       And thus, we gosh darn better update the travel flow before disturb() sets
       travel.run to 0 (prior to travel_step restoring it after we return!)
       In other words: Here is a cryptic sequencing issue with global variables! Do not move this!! */
    if (travel.run)
        travel_wilderness_scroll(travel.x - dx*WILD_SCROLL_CX, travel.y - dy*WILD_SCROLL_CY);

    if (do_disturb) disturb(0, 0);
    p_ptr->redraw |= PR_BASIC; /* In case the user left/entered a town ... */
    handle_stuff();  /* Is this necessary?? */

#if 0
    c_put_str(TERM_WHITE, format("P:%3d/%3d", px, py), 26, 0);
    c_put_str(TERM_WHITE, format("W:%3d/%3d", p_ptr->wilderness_x, p_ptr->wilderness_y), 27, 0);
    c_put_str(TERM_WHITE, format("D:%3d/%3d", p_ptr->wilderness_dx, p_ptr->wilderness_dy), 28, 0);
    c_put_str(TERM_WHITE, format("L:%3d", wilderness_level(p_ptr->wilderness_x, p_ptr->wilderness_y)), 32, 0);
    c_put_str(TERM_WHITE, format("T:%3d", p_ptr->town_num), 33, 0);
#endif
}

static void _wipe_generate_cave_flags(rect_t r)
{
    int x, y;
    /* CAVE_INNER == CAVE_REDRAW. Unless you do the following,
       inner feature tiles ('#') will not redraw when in the player's view.
       cf wipe_generate_cave_flags(); but don't call that guy! */
    for (y = r.y; y < r.y + r.cy; y++)
    {
        for (x = r.x; x < r.x + r.cx; x++)
        {
            cave[y][x].info &= ~CAVE_MASK;
            /* TODO: Setting CAVE_UNSAFE reveals where the rooms are! */
            /*if (cave[y][x].info & CAVE_ROOM)
                cave[y][x].info |= CAVE_UNSAFE; */
        }
    }
}

static int _encounter_terrain_type(int x, int y)
{
    int result = wilderness[y][x].terrain;
    switch (result)
    {
    case TERRAIN_SHALLOW_LAVA:
        result = TERRAIN_DEEP_LAVA;
        break;
    case TERRAIN_SHALLOW_WATER:
        result = TERRAIN_DEEP_WATER;
        break;
    case TERRAIN_DIRT:
    case TERRAIN_DESERT:
        result = TERRAIN_GRASS;
        break;
    }
    return result;
}

static bool _build_room(room_ptr room, transform_ptr xform, rect_t r, rect_t exclude)
{
    int    qx_min = r.x/WILD_SCROLL_CX;
    int    qx_max = (r.x + r.cx - 1)/WILD_SCROLL_CX;
    int    qy_min = r.y/WILD_SCROLL_CY;
    int    qy_max = (r.y + r.cy - 1)/WILD_SCROLL_CY;
    int    qx = rand_range(qx_min, qx_max);
    int    qy = rand_range(qy_min, qy_max);
    rect_t qrect = rect(qx*WILD_SCROLL_CX, qy*WILD_SCROLL_CY, WILD_SCROLL_CX, WILD_SCROLL_CY);
        
    xform->dest = rect_translate(xform->dest, 
        qrect.x + 2 + randint0(qrect.cx - xform->dest.cx - 4),
        qrect.y + 1 + randint0(qrect.cy - xform->dest.cy - 2));

    if (!rect_contains(r, xform->dest)) return FALSE;
    if (!rect_contains(qrect, xform->dest)) return FALSE;

    /* Exclude if overlaps the "valid region" during a scroll op */
    if (rect_is_valid(exclude))
    {
        rect_t temp_rect = rect_intersect(exclude, xform->dest);
        if (rect_is_valid(temp_rect)) return FALSE;
    }
    /* Exclude if player is in the room during a non-scroll op (e.g. ambush)
        Note the player will always be inside the exclude rect during
        a scroll op. */
    else if (room->type == ROOM_WILDERNESS)
    {
        /* Player has not been placed yet, but will be placed at (oldpx, oldpy) shortly */
        /* N.B. Ambush encounters include player placement information */
        if (rect_contains_pt(xform->dest, p_ptr->oldpx, p_ptr->oldpy)) return FALSE;
    }

    build_room_template_aux(room, xform, NULL);
    _wipe_generate_cave_flags(r);

    if (is_daytime() && room->type == ROOM_WILDERNESS && disturb_minor && !travel.run)
    {
        msg_print("You've stumbled onto something interesting ...");
        disturb(0, 0);
    }
    if (room->type == ROOM_AMBUSH)
    {
        msg_print("Press <color:y>Space</color> to continue.");
        flush();
        for (;;)
        {
            char ch = inkey();
            if (ch == ' ') break;
        }
        msg_line_clear();
    }
    return TRUE;
}

static bool _generate_special_encounter(room_ptr room, rect_t r, rect_t exclude)
{
    int i;
    for (i = 0; i < 100; i++)
    {
        transform_ptr xform = transform_alloc_room(room, size(WILD_SCROLL_CX-4, WILD_SCROLL_CY-2));
        bool ok = _build_room(room, xform, r, exclude);
        transform_free(xform);
        if (ok) return TRUE;
    }
    return FALSE;
}

#define _WILD_ENCOUNTER_CHANCE 10

static void _generate_encounters(int x, int y, rect_t r, rect_t exclude)
{
    int    ct, prob, i, x2, y2, r_idx, j;
    rect_t invalid = {0};

    if (rect_is_valid(exclude))
        invalid = rect_intersect(r, exclude);

    if (r.cx < 10 || r.cy < 10)
        return;

    if (wilderness[y][x].terrain == TERRAIN_EDGE)
        return;

    wilderness_mon_hook = get_wilderness_monster_hook(x, y);
    get_mon_num_prep(wilderness_mon_hook, NULL);
    base_level = wilderness_level(x, y);
    monster_level = base_level;
    object_level = base_level;

    /* Special Encounter? */
    if ( !wilderness[y][x].town
      && !wilderness[y][x].road
      && !wilderness[y][x].entrance
      && !no_wilderness
      && !generate_encounter
      && !no_encounters_hack
      && one_in_(_WILD_ENCOUNTER_CHANCE))
    {
        room_ptr room = choose_room_template(ROOM_WILDERNESS, _encounter_terrain_type(x, y));
        if (room)
            _generate_special_encounter(room, r, exclude);
    }

    /* Scripted Ambush? */
    if ( !wilderness[y][x].town
      && generate_encounter
      && !no_wilderness
      && one_in_(5))
    {
        room_ptr room = choose_room_template(ROOM_AMBUSH, _encounter_terrain_type(x, y));
        if (room && _generate_special_encounter(room, r, exclude))
            generate_encounter = FALSE;
    }

    wilderness_mon_hook = NULL;

    /* Random Monsters */
    if (generate_encounter) /* Unscripted Ambush? */
        ct = 20;
    else if (no_encounters_hack)
        ct = 0;
    else if (!wilderness[y][x].road)
        ct = 10;
    else
        ct = 4;

    ct = ct * 100 * (rect_area(r) - rect_area(invalid)) / (MAX_HGT * MAX_WID);
    prob = ct % 100;
    ct /= 100;
    if (randint0(100) < prob)
        ct++;
    if (!ct)
        return;

    wilderness_mon_hook = get_wilderness_monster_hook(x, y);
    get_mon_num_prep(wilderness_mon_hook, NULL);
    base_level = wilderness_level(x, y);
    monster_level = base_level;
    object_level = base_level;

    for (i = 0; i < ct; i++)
    {
        for (j = 0; j < 1000; j++)
        {
            x2 = r.x + 5 + randint0(r.cx - 10);
            y2 = r.y + 5 + randint0(r.cy - 10);
            if (!rect_is_valid(exclude) || !rect_contains_pt(exclude, x2, y2))
            {
                int options = PM_ALLOW_GROUP;
                if (!generate_encounter && one_in_(3))
                    options |= PM_ALLOW_SLEEP;
                r_idx = get_mon_num(monster_level);
                if (r_idx)
                {
                    if (r_info[r_idx].level == 0) options |= PM_ALLOW_SLEEP;
                    place_monster_aux(0, y2, x2, r_idx, options);
                }
                break;
            }
        }
    }

    wilderness_mon_hook = NULL;
}

/* The current cave[][] is a 3x3 viewport on a very large wilderness map.
   Picture a "cursor" which you can slide about on the map and peer into
   the wilderness. Coordinates (wilderness_x and y) and offsets (wilderness_dx and dy)
   apply to this cursor.

   This routine will fill in the cave for both an initial level generation
   and a scroll operation (in which case valid indicates the portion of the cave[][] that
   is correctly filled in). */
void _generate_cave(rect_t exclude)
{
    rect_t viewport = rect(0, 0, MAX_WID, MAX_HGT);
    int x, y;

    p_ptr->town_num = 0;
    for (x = -1; x <= 1; x++)
    {
        for (y = -1; y <= 1; y++)
        {
            int     wild_x = p_ptr->wilderness_x + x;
            int     wild_y = p_ptr->wilderness_y + y;
            int     dx = (x*3 - p_ptr->wilderness_dx) * WILD_SCROLL_CX;
            int     dy = (y*3 - p_ptr->wilderness_dy) * WILD_SCROLL_CY;
            rect_t  tile = rect_translate(viewport, dx, dy);
            rect_t  r = rect_intersect(viewport, tile);

            /* Keep the town_num accurate ... */
            if (wilderness[wild_y][wild_x].town)
            {
                p_ptr->town_num = wilderness[wild_y][wild_x].town;
                town_on_visit(p_ptr->town_num);
            }

            /* ... before excluding this tile during scrolling.
             * We did just clear p_ptr->town_num at the top, after all! */
            if (!rect_is_valid(r)) continue;
            if (rect_is_valid(exclude) && rect_contains(exclude, r)) continue;

            _generate_area(wild_x, wild_y, dx, dy, exclude);
            _generate_encounters(wild_x, wild_y, r, exclude);
        }
    }
    _apply_glow(!rect_is_valid(exclude));
}

static void set_floor_and_wall_aux(s16b feat_type[100], feat_prob prob[DUNGEON_FEAT_PROB_NUM])
{
    int lim[DUNGEON_FEAT_PROB_NUM], cur = 0, i;

    lim[0] = prob[0].percent;
    for (i = 1; i < DUNGEON_FEAT_PROB_NUM; i++) lim[i] = lim[i - 1] + prob[i].percent;

    /* Paranoia */
    if (lim[DUNGEON_FEAT_PROB_NUM - 1] < 100) lim[DUNGEON_FEAT_PROB_NUM - 1] = 100;

    for (i = 0; i < 100; i++)
    {
        while (i == lim[cur]) cur++;
        feat_type[i] = prob[cur].feat;
    }
}

/*
 * Fill the arrays of floors and walls in the good proportions
 */
void set_floor_and_wall(byte type)
{
    static byte cur_type = 255;
    dungeon_info_type *d_ptr;

    /* Already filled */
    if (cur_type == type) return;

    cur_type = type;
    d_ptr = &d_info[type];

    set_floor_and_wall_aux(floor_type, d_ptr->floor);
    set_floor_and_wall_aux(fill_type, d_ptr->fill);

    feat_wall_outer = d_ptr->outer_wall;
    feat_wall_inner = d_ptr->inner_wall;
    feat_wall_solid = d_ptr->outer_wall;
}


/*
 * Helper for plasma generation.
 */
static void perturb_point_mid(int x1, int x2, int x3, int x4,
              int xmid, int ymid, int rough, int depth_max)
{
    /*
     * Average the four corners & perturb it a bit.
     * tmp is a random int +/- rough
     */
    int tmp2 = rough*2 + 1;
    int tmp = randint1(tmp2) - (rough + 1);

    int avg = ((x1 + x2 + x3 + x4) / 4) + tmp;

    /* Division always rounds down, so we round up again */
    if (((x1 + x2 + x3 + x4) % 4) > 1)
        avg++;

    /* Normalize */
    if (avg < 0) avg = 0;
    if (avg > depth_max) avg = depth_max;

    /* Set the new value. */
    _cave[ymid][xmid] = avg;
}


static void perturb_point_end(int x1, int x2, int x3,
              int xmid, int ymid, int rough, int depth_max)
{
    /*
     * Average the three corners & perturb it a bit.
     * tmp is a random int +/- rough
     */
    int tmp2 = rough * 2 + 1;
    int tmp = randint0(tmp2) - rough;

    int avg = ((x1 + x2 + x3) / 3) + tmp;

    /* Division always rounds down, so we round up again */
    if ((x1 + x2 + x3) % 3) avg++;

    /* Normalize */
    if (avg < 0) avg = 0;
    if (avg > depth_max) avg = depth_max;

    /* Set the new value. */
    _cave[ymid][xmid] = avg;
}


/*
 * A generic function to generate the plasma fractal.
 * Note that it uses ``cave_feat'' as temporary storage.
 * The values in ``cave_feat'' after this function
 * are NOT actual features; They are raw heights which
 * need to be converted to features.
 */
static void plasma_recursive(int x1, int y1, int x2, int y2,
                 int depth_max, int rough)
{
    /* Find middle */
    int xmid = (x2 - x1) / 2 + x1;
    int ymid = (y2 - y1) / 2 + y1;

    /* Are we done? */
    if (x1 + 1 == x2) return;

    perturb_point_mid(_cave[y1][x1], _cave[y2][x1], _cave[y1][x2],
        _cave[y2][x2], xmid, ymid, rough, depth_max);

    perturb_point_end(_cave[y1][x1], _cave[y1][x2], _cave[ymid][xmid],
        xmid, y1, rough, depth_max);

    perturb_point_end(_cave[y1][x2], _cave[y2][x2], _cave[ymid][xmid],
        x2, ymid, rough, depth_max);

    perturb_point_end(_cave[y2][x2], _cave[y2][x1], _cave[ymid][xmid],
        xmid, y2, rough, depth_max);

    perturb_point_end(_cave[y2][x1], _cave[y1][x1], _cave[ymid][xmid],
        x1, ymid, rough, depth_max);


    /* Recurse the four quadrants */
    plasma_recursive(x1, y1, xmid, ymid, depth_max, rough);
    plasma_recursive(xmid, y1, x2, ymid, depth_max, rough);
    plasma_recursive(x1, ymid, xmid, y2, depth_max, rough);
    plasma_recursive(xmid, ymid, x2, y2, depth_max, rough);
}


#define MAX_FEAT_IN_TERRAIN 18

/*
 * The default table in terrain level generation.
 */
static s16b terrain_table[MAX_WILDERNESS][MAX_FEAT_IN_TERRAIN];

static void generate_wilderness_area(int terrain, u32b seed)
{
    int x1, y1;
    int table_size = sizeof(terrain_table[0]) / sizeof(s16b);
    int roughness = 1; /* The roughness of the level. */

    /* The outer wall is easy */
    if (terrain == TERRAIN_EDGE)
    {
        /* Create level background */
        for (y1 = 0; y1 < MAX_HGT; y1++)
        {
            for (x1 = 0; x1 < MAX_WID; x1++)
            {
                _cave[y1][x1] = feat_permanent;
            }
        }

        /* We are done already */
        return;
    }


    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant town layout */
    Rand_value = seed;

    /* Create level background */
    for (y1 = 0; y1 < MAX_HGT; y1++)
    {
        for (x1 = 0; x1 < MAX_WID; x1++)
        {
            _cave[y1][x1] = table_size / 2;
        }
    }

    /* x1, y1, x2, y2, num_depths, roughness */
    plasma_recursive(0, 0, MAX_WID-1, MAX_HGT-1, table_size-1, roughness);

    for (y1 = 0; y1 < MAX_HGT; y1++)
    {
        for (x1 = 0; x1 < MAX_WID; x1++)
        {
            _cave[y1][x1] = terrain_table[terrain][_cave[y1][x1]];
        }
    }

    /* Use the complex RNG */
    Rand_quick = FALSE;
}


static void _generate_entrance(int x, int y, int dx, int dy)
{
    int dun_idx = wilderness[y][x].entrance;
    int y2, x2;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant town layout */
    Rand_value = wilderness[y][x].seed;

    y2 = rand_range(6, cur_hgt - 6) + dy;
    x2 = rand_range(6, cur_wid - 6) + dx;

    if (in_bounds(y2, x2))
    {
        cave[y2][x2].feat = feat_entrance;
        cave[y2][x2].special = dun_idx;

        if ( !(dungeon_flags[dun_idx] & DUNGEON_NO_GUARDIAN)
          && d_info[dun_idx].initial_guardian )
        {
            int i;
            bool skip = FALSE;

            /* Thanks to wilderness scrolling, we'll need to double check
               that we haven't already allocated the guardian! */
            for (i = 0; i < max_m_idx; i++)
            {
                if (!m_list[i].r_idx) continue;

                if ( (m_list[i].smart & (1U << SM_GUARDIAN))
                  && m_list[i].pack_idx
                  && pack_info_list[m_list[i].pack_idx].guard_idx == dun_idx )
                {
                    skip = TRUE;
                    break;
                }
            }

            if (!skip)
            {
                int dx = 0, dy = 0;
                int m_idx = 0;

                /* Don't place it on the stairs. If the player gets by the guardian, then
                   this is where they will be placed should they take the stairs back up
                   to the surface. */
                while (dx == 0 && dy == 0)
                {
                    dx = randint1(3) - 2;
                    dy = randint1(3) - 2;
                }

                m_idx = place_monster_one(0, y2 + dy, x2 + dx, d_info[dun_idx].initial_guardian, 0, 0);

                /* We'll use pack ai to guard this location (see _scroll_cave() above).
                   We'll use smart flags to mark the guardian, and hack the dungeon index into
                   the pack info so that we can correctly mark the dungeon upon killing the
                   guardian. See pack_on_slay_monster(). */
                if (m_idx)
                {
                    monster_type *m_ptr = &m_list[m_idx];
                    int           pack_idx = pack_info_pop();
                    pack_info_t  *pack_ptr = &pack_info_list[pack_idx];

                    m_ptr->pack_idx = pack_idx;
                    m_ptr->smart |= (1U << SM_GUARDIAN);

                    pack_ptr->count++;
                    pack_ptr->ai = AI_GUARD_POS;
                    pack_ptr->guard_x = x2;
                    pack_ptr->guard_y = y2;
                    pack_ptr->guard_idx = dun_idx; /* Hack: See pack_on_slay_monster() for more details. */
                }
            }
        }
    }
    /* Use the complex RNG */
    Rand_quick = FALSE;
}

/*
 * Load a town or generate a terrain level using "plasma" fractals.
 *
 * x and y are the coordinates of the area in the wilderness.
 * Border and corner are optimization flags to speed up the
 * generation of the fractal terrain.
 * If border is set then only the border of the terrain should
 * be generated (for initializing the border structure).
 * If corner is set then only the corners of the area are needed.
 */
static void _generate_area(int x, int y, int dx, int dy, rect_t exclude)
{
    int x1, y1;

    if (abs(dy) >= MAX_HGT) return;
    if (abs(dx) >= MAX_WID) return;

    {
        int terrain = wilderness[y][x].terrain;
        int dun_idx = wilderness[y][x].entrance;
        u32b seed = wilderness[y][x].seed;

        generate_wilderness_area(terrain, seed);

        if (wilderness[y][x].road && !wilderness[y][x].town)
        {
            _cave[MAX_HGT/2][MAX_WID/2] = feat_floor;

            if (wilderness[y-1][x].road)
            {
                /* North road */
                for (y1 = 0; y1 < MAX_HGT/2; y1++)
                {
                    x1 = MAX_WID/2;
                    _cave[y1][x1] = feat_floor;
                }
            }

            if (wilderness[y+1][x].road)
            {
                /* South road */
                for (y1 = MAX_HGT/2; y1 < MAX_HGT; y1++)
                {
                    x1 = MAX_WID/2;
                    _cave[y1][x1] = feat_floor;
                }
            }

            if (wilderness[y][x+1].road)
            {
                /* East road */
                for (x1 = MAX_WID/2; x1 < MAX_WID; x1++)
                {
                    y1 = MAX_HGT/2;
                    _cave[y1][x1] = feat_floor;
                }
            }

            if (wilderness[y][x-1].road)
            {
                /* West road */
                for (x1 = 0; x1 < MAX_WID/2; x1++)
                {
                    y1 = MAX_HGT/2;
                    _cave[y1][x1] = feat_floor;
                }
            }
        }

        /* Copy features from scratch buffer to true cave data, applying a delta for scrolling */
        for (y1 = 0; y1 < MAX_HGT; y1++)
        {
            for (x1 = 0; x1 < MAX_WID; x1++)
            {
                int y2 = y1 + dy;
                int x2 = x1 + dx;

                if (!in_bounds2(y2, x2)) continue;
                if (rect_is_valid(exclude) && rect_contains_pt(exclude, x2, y2)) continue;
                cave[y2][x2].feat = _cave[y1][x1];
            }
        }

        /* Create the town on top of default terrain */
        if (wilderness[y][x].town)
        {
            wild_scroll_t scroll = {{0}};
            room_ptr      town_map;

            /* Reset the buildings */
            init_buildings();

            /* Initialize the town */
            if (rect_is_valid(exclude))
            {
                scroll.flags |= INIT_SCROLL_WILDERNESS;
                scroll.exclude = exclude;
            }
            scroll.scroll = point(dx, dy);
            town_map = towns_get_map();
            if (town_map)
            {
                transform_ptr xform = transform_alloc(0, rect(0, 0, town_map->width, town_map->height));
                build_room_template_aux(town_map, xform, &scroll);
                room_free(town_map);
                transform_free(xform);
            }
        }


        /* Ah ... well, our _cave scratch buffer can't handle the stairs. */
        if ( dun_idx
         && !wilderness[y][x].town
         && (p_ptr->total_winner || !(d_info[dun_idx].flags1 & DF1_WINNER))
         && !(dungeon_flags[dun_idx] & DUNGEON_NO_ENTRANCE) )
        {
            _generate_entrance(x, y, dx, dy);
        }
    }
}

/*
 * Build the wilderness area outside of the town.
 */
void wilderness_gen(void)
{
    int           y, x;
    cave_type    *c_ptr;
    feature_type *f_ptr;

    /* Big town */
    cur_hgt = MAX_HGT;
    cur_wid = MAX_WID;

    /* Init the wilderness */
    process_dungeon_file("w_info.txt", 0);

    dun_level = 0;

    _generate_cave(rect_invalid());
    generate_encounter = FALSE;
    _set_boundary();

    /* When teleporting from town to town, look for the building that offers the
       teleport service to place the player */
    if (p_ptr->teleport_town)
    {
        for (y = 0; y < cur_hgt; y++)
        {
            for (x = 0; x < cur_wid; x++)
            {
                c_ptr = &cave[y][x];

                f_ptr = &f_info[c_ptr->feat];
                if (have_flag(f_ptr->flags, FF_BLDG))
                {
                    if ((f_ptr->subtype == 4) || ((p_ptr->town_num == TOWN_OUTPOST) && (f_ptr->subtype == 0)))
                    {
                        if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
                        p_ptr->oldpy = y;
                        p_ptr->oldpx = x;
                    }
                }
            }
        }
        p_ptr->teleport_town = FALSE;
    }
    /* When leaving the dungeon, look for the wilderness stairs to place the player */
    else if (p_ptr->leaving_dungeon && !(d_info[p_ptr->leaving_dungeon].flags1 & DF1_RANDOM))
    {
        bool loytyi = FALSE;
        for (y = 0; ((!loytyi) && (y < cur_hgt)); y++)
        {
            for (x = 0; ((!loytyi) && (x < cur_wid)); x++)
            {
                /* Get the cave grid */
                c_ptr = &cave[y][x];

                if (cave_have_flag_grid(c_ptr, FF_ENTRANCE))
                {
                    {
                        if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
                        p_ptr->oldpy = y;
                        p_ptr->oldpx = x;
                        loytyi = (c_ptr->special == p_ptr->leaving_dungeon);
                    }
                }
            }
        }
        p_ptr->teleport_town = FALSE;
    }

    player_place(p_ptr->oldpy, p_ptr->oldpx);

    /* Fill the arrays of floors and walls in the good proportions */
    set_floor_and_wall(0);

    /* Force scroll after wilderness travel since we are typically
       placed in a boundary "quadrant" */
    viewport_verify_aux(VIEWPORT_FORCE_CENTER);
    wilderness_move_player(p_ptr->oldpx, p_ptr->oldpy);
}


static s16b conv_terrain2feat[MAX_WILDERNESS];

/*
 * Build the wilderness area.
 * -DG-
 */
void wilderness_gen_small(void)
{
    int i, j;

    /* To prevent stupid things */
    for (i = 0; i < MAX_WID; i++)
    for (j = 0; j < MAX_HGT; j++)
    {
        cave[j][i].feat = feat_permanent;
    }

    /* Init the wilderness */
    process_dungeon_file("w_info.txt", 0);

    /* Fill the map */
    for (i = 0; i < max_wild_x; i++)
    for (j = 0; j < max_wild_y; j++)
    {
        if (wilderness[j][i].town && (wilderness[j][i].town != NO_TOWN))
        {
            cave[j][i].feat = feat_town;
            cave[j][i].special = wilderness[j][i].town;
        }
        else if (wilderness[j][i].road) cave[j][i].feat = feat_floor;
        else if (wilderness[j][i].entrance && (p_ptr->total_winner || !(d_info[wilderness[j][i].entrance].flags1 & DF1_WINNER)))
        {
            cave[j][i].feat = feat_entrance;
            cave[j][i].special = (byte)wilderness[j][i].entrance;
        }
        else cave[j][i].feat = conv_terrain2feat[wilderness[j][i].terrain];

        cave[j][i].info |= (CAVE_GLOW | CAVE_MARK | CAVE_AWARE);
    }

    cur_hgt = (s16b) max_wild_y;
    cur_wid = (s16b) max_wild_x;

    if (cur_hgt > MAX_HGT) cur_hgt = MAX_HGT;
    if (cur_wid > MAX_WID) cur_wid = MAX_WID;

    /* Place the player */
    px = p_ptr->wilderness_x;
    py = p_ptr->wilderness_y;

    p_ptr->town_num = 0;
}


typedef struct wilderness_grid wilderness_grid;

struct wilderness_grid
{
    int     terrain;    /* Terrain type */
    int     town;       /* Town number */
    s16b    level;      /* Level of the wilderness */
    byte    road;       /* Road */
    char    name[32];   /* Name of the town/wilderness */
};

static wilderness_grid w_letter[255];


/*
 * Parse a sub-file of the "extra info"
 */
static int _parse_y;
errr parse_line_wilderness(char *buf, int options)
{
    int i, num;
    char *zz[33];

    /* Paranoia */
    if (!(buf[0] == 'W')) return (PARSE_ERROR_GENERIC);

    switch (buf[2])
    {
        /* Process "W:F:<letter>:<terrain>:<town>:<road>:<name> */
    case 'J':
        return 0;
    case 'F':
    case 'E':
    {
        _parse_y = 0; /* hack: prepare to reparse the map */
        if ((num = tokenize(buf+4, 6, zz, 0)) > 1)
        {
            int index = zz[0][0];

            if (num > 1)
                w_letter[index].terrain = atoi(zz[1]);
            else
                w_letter[index].terrain = 0;

            if (num > 2)
                w_letter[index].level = atoi(zz[2]);
            else
                w_letter[index].level = 0;

            if (num > 3)
                w_letter[index].town = atoi(zz[3]);
            else
                w_letter[index].town = 0;

            if (num > 4)
                w_letter[index].road = atoi(zz[4]);
            else
                w_letter[index].road = 0;

            if (num > 5)
                strcpy(w_letter[index].name, zz[5]);
            else
                w_letter[index].name[0] = 0;
        }
        else
        {
                /* Failure */
            return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
        }

        break;
    }

    /* Process "W:D:<layout> */
    /* Layout of the wilderness */
    case 'D':
    {
        int   i;
        char *s = buf+4;
        int len = strlen(s);

        assert(0 <= _parse_y && _parse_y < max_wild_y);
        for (i = 0; i < len; i++)
        {
            int letter = s[i];
            assert(0 <= i && i < max_wild_x);
            wilderness[_parse_y][i].terrain = w_letter[letter].terrain;
            wilderness[_parse_y][i].level = w_letter[letter].level;
            wilderness[_parse_y][i].town = w_letter[letter].town;
            wilderness[_parse_y][i].road = w_letter[letter].road;
        }
        _parse_y++;
        break;
    }

    /* Process "W:P:<x>:<y> - starting position in the wilderness */
    case 'P':
    {
        if ((p_ptr->wilderness_x == 0) &&
            (p_ptr->wilderness_y == 0))
        {
            if (tokenize(buf+4, 2, zz, 0) == 2)
            {
                p_ptr->wilderness_y = atoi(zz[0]);
                p_ptr->wilderness_x = atoi(zz[1]);

                if ((p_ptr->wilderness_x < 1) ||
                    (p_ptr->wilderness_x > max_wild_x) ||
                    (p_ptr->wilderness_y < 1) ||
                    (p_ptr->wilderness_y > max_wild_y))
                {
                    return (PARSE_ERROR_OUT_OF_BOUNDS);
                }
            }
            else
            {
                return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
            }
        }

        break;
    }

    default:
        /* Failure */
        return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
    }

    for (i = 1; i < max_d_idx; i++)
    {
        if (!d_info[i].maxdepth) continue;
        if (d_info[i].flags1 & DF1_RANDOM) continue;

        wilderness[d_info[i].dy][d_info[i].dx].entrance = i;

        if (!wilderness[d_info[i].dy][d_info[i].dx].town && !(d_info[i].flags1 & DF1_WINNER))
            wilderness[d_info[i].dy][d_info[i].dx].level = d_info[i].mindepth;
    }

    /* Success */
    return (0);
}


/*
 * Generate the random seeds for the wilderness
 */
void seed_wilderness_aux(void)
{
    int x, y;
    Rand_quick = TRUE;
    Rand_value = wilderness_seed;
    for (x = 0; x < max_wild_x; x++)
    {
        for (y = 0; y < max_wild_y; y++)
        {
            wilderness[y][x].seed = randint0(0x10000000);
            wilderness[y][x].entrance = 0;
        }
    }
    Rand_quick = FALSE;
}
void seed_wilderness(void)
{
    wilderness_seed = randint0(0x10000000);
    seed_wilderness_aux();
}


/*
 * Pointer to wilderness_type
 */
typedef wilderness_type *wilderness_type_ptr;

/*
 * Initialize wilderness array
 */
errr init_wilderness(void)
{
    int i;

    /* Allocate the wilderness (two-dimension array) */
    C_MAKE(wilderness, max_wild_y, wilderness_type_ptr);
    C_MAKE(wilderness[0], max_wild_x * max_wild_y, wilderness_type);

    /* Init the other pointers */
    for (i = 1; i < max_wild_y; i++)
        wilderness[i] = wilderness[0] + i * max_wild_x;

    for (i = 0; i < MAX_HGT; i++)
        C_MAKE(_cave[i], MAX_WID, s16b);

    generate_encounter = FALSE;

    return 0;
}


static void init_terrain_table(int terrain, s16b feat_global, cptr fmt, ...)
{
    va_list vp;
    cptr    p;
    int     cur = 0;
    char    check = 'a';
    s16b    feat;
    int     num;

    /* Begin the varargs stuff */
    va_start(vp, fmt);

    /* Wilderness terrains on global map */
    conv_terrain2feat[terrain] = feat_global;

    /* Wilderness terrains on local map */
    for (p = fmt; *p; p++)
    {
        if (*p == check)
        {
            int lim;

            feat = (s16b)va_arg(vp, int);
            num = va_arg(vp, int);
            lim = cur + num;

            for (; (cur < lim) && (cur < MAX_FEAT_IN_TERRAIN); cur++)
                terrain_table[terrain][cur] = feat;
            if (cur >= MAX_FEAT_IN_TERRAIN) break;

            check++;
        }
        else /* Paranoia */
        {
            plog_fmt("Format error");
        }
    }

    /* Paranoia */
    if (cur < MAX_FEAT_IN_TERRAIN)
    {
        plog_fmt("Too few parameters");
    }

    /* End the varargs stuff */
    va_end(vp);
}


/*
 * Initialize arrays for wilderness terrains
 */
void init_wilderness_terrains(void)
{
    init_terrain_table(TERRAIN_EDGE, feat_permanent, "a",
        feat_permanent, MAX_FEAT_IN_TERRAIN);

    init_terrain_table(TERRAIN_TOWN, feat_town, "a",
        feat_floor, MAX_FEAT_IN_TERRAIN);

    init_terrain_table(TERRAIN_DEEP_WATER, feat_deep_water, "ab",
        feat_deep_water, 12,
        feat_shallow_water, MAX_FEAT_IN_TERRAIN - 12);

    init_terrain_table(TERRAIN_SHALLOW_WATER, feat_shallow_water, "abcde",
        feat_deep_water, 3,
        feat_shallow_water, 12,
        feat_floor, 1,
        feat_dirt, 1,
        feat_grass, MAX_FEAT_IN_TERRAIN - 17);

    init_terrain_table(TERRAIN_SWAMP, feat_swamp, "abcdef",
        feat_dirt, 2,
        feat_grass, 3,
        feat_tree, 1,
        feat_brake, 1,
        feat_shallow_water, 4,
        feat_swamp, MAX_FEAT_IN_TERRAIN - 11);

    init_terrain_table(TERRAIN_DIRT, feat_dirt, "abcdef",
        feat_floor, 3,
        feat_dirt, 10,
        feat_flower, 1,
        feat_brake, 1,
        feat_grass, 1,
        feat_tree, MAX_FEAT_IN_TERRAIN - 16);

    init_terrain_table(TERRAIN_GRASS, feat_grass, "abcdef",
        feat_floor, 2,
        feat_dirt, 2,
        feat_grass, 9,
        feat_flower, 1,
        feat_brake, 2,
        feat_tree, MAX_FEAT_IN_TERRAIN - 16);

    init_terrain_table(TERRAIN_TREES, feat_tree, "abcde",
        feat_floor, 2,
        feat_dirt, 1,
        feat_tree, 11,
        feat_brake, 2,
        feat_grass, MAX_FEAT_IN_TERRAIN - 16);

    init_terrain_table(TERRAIN_DESERT, feat_dirt, "abc",
        feat_floor, 2,
        feat_dirt, 13,
        feat_grass, MAX_FEAT_IN_TERRAIN - 15);

    init_terrain_table(TERRAIN_SHALLOW_LAVA, feat_shallow_lava, "abc",
        feat_shallow_lava, 14,
        feat_deep_lava, 3,
        feat_mountain, MAX_FEAT_IN_TERRAIN - 17);

    init_terrain_table(TERRAIN_DEEP_LAVA, feat_deep_lava, "abcd",
        feat_dirt, 3,
        feat_shallow_lava, 3,
        feat_deep_lava, 10,
        feat_mountain, MAX_FEAT_IN_TERRAIN - 16);

    init_terrain_table(TERRAIN_MOUNTAIN, feat_mountain, "abcdef",
        feat_floor, 1,
        feat_brake, 1,
        feat_grass, 3,
        feat_dirt, 3,
        feat_tree, 3,
        feat_mountain, MAX_FEAT_IN_TERRAIN - 11);
}


bool change_wild_mode(void)
{
    int i;
    bool have_pet = FALSE;

    /* It is in the middle of changing map */
    if (p_ptr->leaving) return FALSE;


    if (no_wilderness)
    {
        msg_print("No global map.");
        return FALSE;
    }

    if (p_ptr->wild_mode)
    {
        /* Save the location in the global map */
        if (py != p_ptr->wilderness_y || px != p_ptr->wilderness_x)
        {
            p_ptr->wilderness_x = px;
            p_ptr->wilderness_y = py;
            p_ptr->wilderness_dx = 0;
            p_ptr->wilderness_dy = 0;
        }

        /* Give first move to the player */
        p_ptr->energy_need = 0;

        /* Go back to the ordinary map */
        p_ptr->wild_mode = FALSE;

        /* Leaving */
        p_ptr->leaving = TRUE;

        no_encounters_hack = TRUE;

        /* Succeed */
        return TRUE;
    }

    for (i = 1; i < m_max; i++)
    {
        monster_type *m_ptr = &m_list[i];
        monster_race *r_ptr;

        if (!m_ptr->r_idx) continue;
        r_ptr = &r_info[m_ptr->r_idx];
        if (is_pet(m_ptr) && i != p_ptr->riding) have_pet = TRUE;
        if (MON_CSLEEP(m_ptr)) continue;
        if (m_ptr->cdis > MAX_SIGHT) continue;
        if ( !los(py, px, m_ptr->fy, m_ptr->fx) /* XXX For Hugo ;) */
          && m_ptr->cdis > MIN(MAX_SIGHT, r_ptr->aaf) )
        {
            continue;
        }
        if (!is_hostile(m_ptr)) continue;
        /* Monster Awareness of the player is a TODO concept, not yet correctly implemented.
           At the moment, only the Ring player race uses this and there is a slight bug as well!
        if (!is_aware(m_ptr)) continue;*/
        msg_print("You cannot enter the global map since there are some monsters nearby!");
        energy_use = 0;
        return FALSE;
    }

    if (have_pet)
    {
        cptr msg = "Do you leave your pets behind? ";

        if (!get_check_strict(msg, CHECK_OKAY_CANCEL))
        {
            energy_use = 0;
            return FALSE;
        }
    }

    /* HACK */
    energy_use = 1000;

    /* Remember the position */
    p_ptr->oldpx = px;
    p_ptr->oldpy = py;

    /* Cancel hex spelling */
    if (hex_spelling_any()) stop_hex_spell_all();

    /* Cancel any special action */
    set_action(ACTION_NONE);

    /* Go into the global map */
    p_ptr->wild_mode = TRUE;

    /* Leaving */
    p_ptr->leaving = TRUE;

    /* Succeed */
    return TRUE;
}

bool py_on_surface(void)
{
    return !dun_level && !p_ptr->inside_arena && !p_ptr->inside_battle && !quests_get_current();
}

bool py_in_town(void)
{
    return py_on_surface() && p_ptr->town_num;
}

bool py_in_dungeon(void)
{
    return dungeon_type != 0;
}

bool py_can_recall(void)
{
    if (p_ptr->inside_arena || ironman_downward)
        return FALSE;

    return TRUE;
}

