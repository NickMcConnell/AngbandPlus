#include "angband.h"
#include "dun.h"
#include "dun_gen.h"
#include "dun_util.h"
#include <assert.h>

#define _TILE_CX 128
#define _TILE_CY  64
#define _TILE_BX   7
#define _TILE_BY   6

static dun_u32b_ptr _seeds = NULL;
void dun_world_reseed(u32b world_seed)
{
    dun_mgr_ptr dm = dun_mgr();
    point_t min, max, p;

    assert(dm->world);
    assert(dm->world_seed == world_seed);

    if (_seeds) dun_u32b_free(_seeds);
    _seeds = dun_u32b_alloc(dm->world->rect);

    Rand_quick = TRUE;
    Rand_value = world_seed;

    min = rect_top_left(dm->world->rect);
    max = rect_bottom_right(dm->world->rect);
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
            dun_u32b_set(_seeds, p, randint0(0x10000000));
    }

    Rand_quick = FALSE;
}

static void _add_feat(vec_ptr v, int feat, int ct)
{
    int i;
    for (i = 0; i < ct; i++)
        vec_add_int(v, feat);
}

point_t dun_world_pos(point_t surface_pos)
{
    return point_create(surface_pos.x >> _TILE_BX, surface_pos.y >> _TILE_BY);
}
static point_t _surface_pos(point_t world_pos)
{
    return point_create(world_pos.x << _TILE_BX, world_pos.y << _TILE_BY);
}
static rect_t _tile_rect(point_t world_pos)
{
    point_t surface_pos = _surface_pos(world_pos);
    return rect_create(surface_pos.x, surface_pos.y, _TILE_CX, _TILE_CY);
}
static rect_t _viewport_rect(point_t world_pos)
{
    rect_t world_rect = rect_create_centered(world_pos, 1, 1);
    point_t pos = _surface_pos(rect_top_left(world_rect));
    return rect_create(pos.x, pos.y, 3*_TILE_CX, 3*_TILE_CY);
}
static dun_page_ptr _find_page(dun_ptr dun, point_t pos)
{
    dun_page_ptr page;
    for (page = dun->page; page; page = page->next)
    {
        if (rect_contains_point(page->rect, pos))
            return page;
    }
    return NULL;
}
static dun_grid_ptr dun_page_grid_at(dun_page_ptr page, point_t pos)
{
    int x = pos.x - page->rect.x;
    int y = pos.y - page->rect.y;
    int i = y * page->rect.cx + x;
    assert(rect_contains_point(page->rect, pos));
    return &page->grids[i];
}
static int _difficulty(dun_grid_ptr grid)
{
    if (grid->info & CAVE_TOWN) return towns_lookup(grid->special)->level;
    if (grid->info & CAVE_DUNGEON) return dun_types_lookup(grid->special)->min_dun_lvl;
    return grid->special;
}
static void _set_difficulty(dun_ptr surface)
{
    dun_ptr      world = dun_mgr()->world;
    dun_page_ptr page;
    int          ct = 0, total = 0;

    /* difficulty averages over active world grids */
    for (page = surface->page; page; page = page->next)
    {
        point_t      pos = rect_top_left(page->rect);
        point_t      world_pos = dun_world_pos(pos);
        dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
        int          difficulty = _difficulty(world_grid);

        total += difficulty;
        ct++;
    }
    surface->difficulty = total/ct;
    p_ptr->redraw |= PR_DEPTH;
}

/************************************************************************
 * D_SURFACE
 ************************************************************************/
#define _FEAT_COUNT 18
static int _initial_height(int feat)
{
    if (feat == feat_deep_water) return 9;
    else if (feat == feat_shallow_water) return 27;
    else if (feat == feat_swamp) return 45;
    else if (feat == feat_dirt) return 63;
    else if (feat == feat_grass) return 81;
    else if (feat == feat_tree) return 99;
    else if (feat == feat_mountain) return 117;
    else if (feat == feat_shallow_lava) return 135;
    else if (feat == feat_deep_lava) return 153;
    else return 117; /* world boundary is MOUNTAIN_WALL */
}
static vec_ptr _feat_map(void)
{
    static vec_ptr _v = NULL;
    if (!_v)
    {
        _v = vec_alloc(NULL);
        /* deep water */
        _add_feat(_v, feat_deep_water, 6);
        _add_feat(_v, feat_shallow_water, 6);
        _add_feat(_v, feat_deep_water, 6);

        /* shallow water */
        _add_feat(_v, feat_deep_water, 3);
        _add_feat(_v, feat_shallow_water, 10);
        _add_feat(_v, feat_swamp, 2);
        _add_feat(_v, feat_shallow_water, 3);

        /* swamp */
        _add_feat(_v, feat_shallow_water, 2);
        _add_feat(_v, feat_swamp, 2);
        _add_feat(_v, feat_shallow_water, 1);
        _add_feat(_v, feat_swamp, 2);
        _add_feat(_v, feat_shallow_water, 1);
        _add_feat(_v, feat_swamp, 5);
        _add_feat(_v, feat_grass, 1);
        _add_feat(_v, feat_tree, 1);
        _add_feat(_v, feat_brake, 1);
        _add_feat(_v, feat_grass, 2);

        /* dirt */
        _add_feat(_v, feat_dirt, 10);
        _add_feat(_v, feat_flower, 2);
        _add_feat(_v, feat_brake, 2);
        _add_feat(_v, feat_grass, 2);
        _add_feat(_v, feat_tree, 2);

        /* grass */
        _add_feat(_v, feat_dirt, 3);
        _add_feat(_v, feat_grass, 9);
        _add_feat(_v, feat_flower, 1);
        _add_feat(_v, feat_brake, 2);
        _add_feat(_v, feat_tree, 3);

        /* tree */
        _add_feat(_v, feat_tree, 5);
        _add_feat(_v, feat_brake, 1);
        _add_feat(_v, feat_tree, 5);
        _add_feat(_v, feat_grass, 1);
        _add_feat(_v, feat_tree, 6);

        /* mountain */
        _add_feat(_v, feat_mountain, 2);
        _add_feat(_v, feat_tree, 1);
        _add_feat(_v, feat_mountain, 2);
        _add_feat(_v, feat_tree, 1);
        _add_feat(_v, feat_mountain, 5);
        _add_feat(_v, feat_tree, 1);
        _add_feat(_v, feat_grass, 1);
        _add_feat(_v, feat_mountain, 5);

        /* shallow lava */
        _add_feat(_v, feat_mountain, 1);
        _add_feat(_v, feat_shallow_lava, 2);
        _add_feat(_v, feat_mountain, 2);
        _add_feat(_v, feat_shallow_lava, 3);
        _add_feat(_v, feat_mountain, 1);
        _add_feat(_v, feat_shallow_lava, 3);
        _add_feat(_v, feat_deep_lava, 1);
        _add_feat(_v, feat_shallow_lava, 4);

        /* deep lava */
        _add_feat(_v, feat_shallow_lava, 3);
        _add_feat(_v, feat_deep_lava, 6);
        _add_feat(_v, feat_shallow_lava, 3);
        _add_feat(_v, feat_deep_lava, 6);

        assert(vec_length(_v) <= 255);
    }
    return _v;
}
static void _road_aux(dun_page_ptr page, point_t start, point_t stop, int feat[3], int width, point_t perturb)
{
    int length = point_distance(start, stop);

    if (length > 4)
    {
        point_t mp = point_perturbed_midpoint_aux(start, stop, perturb);
        if (!rect_contains_point(page->rect, mp))
            mp = point_midpoint(start, stop);

        dun_page_grid_at(page, mp)->feat = feat[0];
        _road_aux(page, start, mp, feat, width, perturb);
        _road_aux(page, mp, stop, feat, width, perturb);
    }
    else
    {
        int l;
        for (l = 0; l < length; l++)
        {
            point_t p, d;
            int w = width + 1;
            p.x = start.x + l*(stop.x - start.x)/length;
            p.y = start.y + l*(stop.y - start.y)/length;

            for (d.y = -w; d.y <= w; d.y++)
            {
                for (d.x = -w; d.x <= w; d.x++)
                {
                    point_t p2 = point_add(p, d);
                    dun_grid_ptr g;
                    int dist;

                    if (!rect_contains_point(page->rect, p2)) continue;
                    g = dun_page_grid_at(page, p2);
                    if (g->feat == feat[0]) continue;

                    dist = point_distance(p2, p);
                    if (dist > rand_spread(width, 1)) continue;

                    if (dist == 0) g->feat = feat[0];
                    else if (dist <= width) g->feat = feat[1];
                    else if (g->feat != feat[1]) g->feat = feat[2];
                }
            }
        }
    }
}
static void _road(dun_page_ptr page, point_t start, point_t stop, int feat[3], int width)
{
    dun_grid_ptr g = dun_page_grid_at(page, start);
    g->feat = feat[0];
    g = dun_page_grid_at(page, stop);
    g->feat = feat[0];
    _road_aux(page, start, stop, feat, width, point_create(75, 25));
}
static void _gen_roads(dun_page_ptr page)
{
    dun_ptr      world = dun_mgr()->world;
    point_t      center = rect_center(page->rect);
    point_t      world_pos = dun_world_pos(center);
    dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
    point_t      adj_world_pos;
    int          feat[3];

    feat[0] = feat_road;
    feat[1] = feat_floor;
    feat[2] = feat_dirt;

    if (world_grid->feat == feat_mountain)
        feat[2] = feat_rubble;
    else if (world_grid->feat == feat_deep_water)
        feat[2] = feat_swamp;
    else if (world_grid->feat == feat_tree)
        feat[2] = feat_flower;

    /* north road */
    adj_world_pos = point_step(world_pos, 8);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_ROAD)
    {
        point_t stop = rect_top_center(page->rect);
        _road(page, center, stop, feat, 2);
    }
    /* south road */
    adj_world_pos = point_step(world_pos, 2);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_ROAD)
    {
        point_t stop = rect_bottom_center(page->rect);
        _road(page, center, stop, feat, 2);
    }
    /* east road */
    adj_world_pos = point_step(world_pos, 6);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_ROAD)
    {
        point_t stop = rect_right_center(page->rect);
        _road(page, center, stop, feat, 2);
    }
    /* weast road */
    adj_world_pos = point_step(world_pos, 4);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_ROAD)
    {
        point_t stop = rect_left_center(page->rect);
        _road(page, center, stop, feat, 2);
    }
}
static void _river(dun_page_ptr page, point_t start, point_t stop, int feat[3], int width)
{
    dun_grid_ptr g = dun_page_grid_at(page, start);
    g->feat = feat[0];
    g = dun_page_grid_at(page, stop);
    g->feat = feat[0];
    _road_aux(page, start, stop, feat, width, point_create(75, 50));
}
static void _gen_rivers(dun_page_ptr page)
{
    dun_ptr      world = dun_mgr()->world;
    point_t      center = rect_center(page->rect);
    point_t      world_pos = dun_world_pos(center);
    dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
    point_t      adj_world_pos;
    int          feat[3];
    int          width = 8;

    if (dun_worlds_current()->flags & WF_RIVER_LAVA)
    {
        feat[0] = feat_shallow_lava;
        feat[1] = feat_shallow_lava;
        feat[2] = feat_magma_vein;
    }
    else
    {
        feat[0] = feat_shallow_water;
        feat[1] = feat_shallow_water;
        feat[2] = feat_dirt;
        if (world_grid->feat == feat_deep_water || world_grid->feat == feat_shallow_water)
            feat[2] = feat_shallow_water;
        else if (world_grid->feat == feat_swamp)
            feat[2] = feat_swamp;
        else if (world_grid->feat == feat_tree)
            feat[2] = feat_brake;
    }
    /* north river */
    adj_world_pos = point_step(world_pos, 8);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_RIVER)
    {
        point_t stop = rect_top_center(page->rect);
        _river(page, center, stop, feat, width);
    }
    /* south river */
    adj_world_pos = point_step(world_pos, 2);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_RIVER)
    {
        point_t stop = rect_bottom_center(page->rect);
        _river(page, center, stop, feat, width);
    }
    /* east river */
    adj_world_pos = point_step(world_pos, 6);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_RIVER)
    {
        point_t stop = rect_right_center(page->rect);
        _river(page, center, stop, feat, width);
    }
    /* weast river */
    adj_world_pos = point_step(world_pos, 4);
    if (dun_grid_at(world, adj_world_pos)->info & CAVE_RIVER)
    {
        point_t stop = rect_left_center(page->rect);
        _river(page, center, stop, feat, width);
    }
}
static dun_stairs_ptr _alloc_stairs(void)
{
    dun_stairs_ptr stairs = malloc(sizeof(dun_stairs_t));
    memset(stairs, 0, sizeof(dun_stairs_t));
    return stairs;
}
static void _process_town_map(dun_ptr dun, dun_page_ptr page)
{
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect);
    point_t      p;
    dun_grid_ptr grid = page->grids;

    /* add stair info for QUEST_ENTER grids */
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_feat_ptr feat = dun_grid_feat(grid);
            if (have_flag(feat->flags, FF_QUEST_ENTER))
            {
                dun_stairs_ptr stairs = _alloc_stairs();
                stairs->pos_here = p;
                stairs->dun_type_id = D_QUEST;
                stairs->dun_lvl = grid->special; /* XXX */

                dun_add_stairs(dun, stairs);
            }
            grid++;
        }
    }
}
static void _gen_town(dun_ptr dun, dun_page_ptr page, int town_id)
{
    point_t  center = rect_center(page->rect);
    room_ptr town_map;

    assert(cave == dun); /* for build_room_template_aux */
    town_on_visit(town_id);
    town_map = towns_get_map(town_id);
    if (town_map)
    {
        transform_ptr xform = transform_alloc(0, rect_create(0, 0, town_map->width, town_map->height));
        xform->dest = rect_translate(xform->dest, page->rect.x, page->rect.y);
        xform->dest = rect_recenter(xform->dest, center);
        assert(rect_contains(page->rect, xform->dest));
        build_room_template_aux(town_map, xform);
        room_free(town_map);
        transform_free(xform);
    }

    _process_town_map(dun, page);
}
static void _gen_dungeon(dun_ptr dun, dun_page_ptr page, int dun_type_id)
{
    rect_t         r = rect_deflate(page->rect, 16, 8);
    point_t        p = rect_random_point(r);
    dun_grid_ptr   g = dun_page_grid_at(page, p);
    dun_stairs_ptr s = _alloc_stairs();
    dun_type_ptr   di = dun_types_lookup(dun_type_id);

    g->feat = feat_entrance;
    g->special = dun_type_id;

    s->pos_here = p;
    s->dun_type_id = dun_type_id;
    s->dun_lvl = di->min_dun_lvl;
    dun_add_stairs(dun, s);
}
static void _gen_quest(dun_ptr dun, dun_page_ptr page, int dun_type_id)
{
    /* XXX Normally, the player needs to sign up for quests at
     * a specific town. But we can also set quests on the world
     * map. Need to place stairs, generate the level, etc. But,
     * once the quest is complete, the stairs must be removed.
     * Rewards can be granted upon "quest completion" in quest.c
     * This is all TODO, of course! XXX */
}
static void _glow_page(dun_page_ptr page)
{
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect), p;
    dun_grid_ptr grid = page->grids;

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_feat_ptr feat = dun_grid_feat_mimic(grid);
            if (is_daytime())
            {
                if ( (grid->info & CAVE_ROOM)
                  && !have_flag(feat->flags, FF_WALL)
                  && !have_flag(feat->flags, FF_DOOR) )
                {
                    /* TODO */
                }
                else
                {
                    grid->info |= CAVE_AWARE | CAVE_GLOW;
                    if (view_perma_grids) grid->info |= CAVE_MARK;
                }
            }
            else
            {
                if ( !is_mirror_grid(grid)
                  && !have_flag(feat->flags, FF_QUEST_ENTER)
                  && !have_flag(feat->flags, FF_ENTRANCE) )
                {
                    grid->info &= ~CAVE_GLOW;
                    if (!have_flag(feat->flags, FF_REMEMBER))
                        grid->info &= ~CAVE_MARK;
                }
                else if (have_flag(feat->flags, FF_ENTRANCE))
                {
                    grid->info |= CAVE_GLOW | CAVE_AWARE;
                    if (view_perma_grids) grid->info |= CAVE_MARK;
                }
            }
            grid++;
        }
    }

}
static int _wild_perturb(dun_frac_ptr frac, int scale)
    { return rand_range(-frac->rough, frac->rough); }
static dun_frac_ptr _gen_frac(dun_page_ptr page)
{
    dun_ptr      world = dun_mgr()->world;
    vec_ptr      feat_map = _feat_map();
    dun_frac_ptr frac = dun_frac_alloc(page->rect, vec_length(feat_map) - 1);

    /* compute some basic points (4 corners, center, and 4 side mid-points) */
    point_t      tl = rect_top_left(page->rect);
    point_t      br = rect_bottom_right(page->rect);
    point_t      tr = point_create(br.x, tl.y);
    point_t      bl = point_create(tl.x, br.y);
    point_t      c = rect_center(page->rect);
    point_t      tc = point_create(c.x, tl.y);
    point_t      lc = point_create(tl.x, c.y);
    point_t      bc = point_create(c.x, bl.y);
    point_t      rc = point_create(tr.x, c.y);

    point_t      world_pos = dun_world_pos(c);
    int          heights[3][3];
    int          i, j, h;

    /* extract fractal height info for 3x3 world viewport centered on page */
    for (j = 0; j < 3; j++)
    {
        for (i = 0; i < 3; i++)
        {
            point_t      d = point_create(i - 1, j - 1);
            point_t      p = point_add(world_pos, d);
            dun_grid_ptr wg = dun_grid_at(world, p);

            heights[j][i] = _initial_height(wg->feat);
        }
    }

    /* seed the fractal algorithm using adjacent world tile terrain
     * all averages are unperturbed. */
    frac->rough = 1;
    frac->grid = 0;
    frac->perturb_f = _wild_perturb;

    /* top-right */
    h = (heights[0][1] + heights[0][2] + heights[1][1] + heights[1][2])/4;
    dun_frac_set(frac, tr, h);

    /* top-left */
    h = (heights[0][0] + heights[0][1] + heights[1][0] + heights[1][1])/4;
    dun_frac_set(frac, tl, h);

    /* bottom-left */
    h = (heights[1][0] + heights[1][1] + heights[2][0] + heights[2][1])/4;
    dun_frac_set(frac, bl, h);

    /* bottom-right */
    h = (heights[1][1] + heights[1][2] + heights[2][1] + heights[2][2])/4;
    dun_frac_set(frac, br, h);

    /* center */
    dun_frac_set(frac, c, heights[1][1]);

    /* top-midpoint */
    h = (heights[0][1] + heights[1][1] + dun_frac_get(frac, tl) + dun_frac_get(frac, tr))/4;
    dun_frac_set(frac, tc, h);

    /* left-midpoint */
    h = (heights[1][0] + heights[1][1] + dun_frac_get(frac, tl) + dun_frac_get(frac, bl))/4;
    dun_frac_set(frac, lc, h);

    /* bottom-midpoint */
    h = (heights[1][1] + heights[2][1] + dun_frac_get(frac, bl) + dun_frac_get(frac, br))/4;
    dun_frac_set(frac, bc, h);

    /* right-midpoint */
    h = (heights[1][1] + heights[1][2] + dun_frac_get(frac, tr) + dun_frac_get(frac, br))/4;
    dun_frac_set(frac, rc, h);

    /* recurse */
    dun_frac_calc_aux(frac, tc.x, tc.y, rc.x, rc.y); /* QI: Top Right */
    dun_frac_calc_aux(frac, tl.x, tl.y, c.x, c.y);   /* QII: Top Left */
    dun_frac_calc_aux(frac, lc.x, lc.y, bc.x, bc.y); /* QIII: Bottom Left */
    dun_frac_calc_aux(frac, c.x, c.y, br.x, br.y);   /* QIV: Bottom Right */

    return frac;
}
static int _encounter_terrain_type(int feat)
{
    if (feat == feat_shallow_water || feat == feat_deep_water)
        return TERRAIN_DEEP_WATER;
    if (feat == feat_swamp) return TERRAIN_SWAMP;
    if (feat == feat_grass) return TERRAIN_GRASS;
    if (feat == feat_tree) return TERRAIN_TREES;
    if (feat == feat_mountain) return TERRAIN_MOUNTAIN;
    if (feat == feat_shallow_lava || feat == feat_deep_lava)
        return TERRAIN_DEEP_LAVA;
    return TERRAIN_EDGE;
}
static void _wipe_gen_flags(dun_page_ptr page, rect_t r)
{
    point_t min = rect_top_right(r);
    point_t max = rect_bottom_left(r), p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_grid_ptr g = dun_page_grid_at(page, p);
            g->info &= ~CAVE_MASK;
        }
    }
}
static bool _notice(room_ptr room)
{
    /* Notice wilderness encounters, but only in the daytime.
     * Don't notice trivial encounters (require ROOM_NOTICE to be set) */
    if (room->type != ROOM_WILDERNESS) return FALSE;
    if (p_ptr->wizard) return TRUE; /* XXX Testing */
    if (!(room->flags & ROOM_NOTICE)) return FALSE;
    if (!is_daytime()) return FALSE;
    return TRUE;
}
static bool _gen_room_aux(dun_page_ptr page, room_ptr room, transform_ptr xform)
{
    rect_t pr = rect_deflate(page->rect, 2, 1);
    rect_t rr = rect_translate(xform->dest,
        pr.x + 2 + randint0(pr.cx - xform->dest.cx - 4),
        pr.y + 1 + randint0(pr.cy - xform->dest.cy - 2));

    if (!rect_contains(pr, rr)) return FALSE;

    xform->dest = rr;
    build_room_template_aux(room, xform);
    _wipe_gen_flags(page, rr);

    if (_notice(room))
    {
        if (p_ptr->wizard)
            msg_format("You have stumbled onto <color:o>%s</color>.", room->name);
        else
            msg_print("You have stumbled on to something interesting.");
        disturb(1, 0);
    }
    return TRUE;
}
static bool _gen_room(dun_page_ptr page, room_ptr room)
{
    int i;
    assert(room);
    for (i = 0; i < 100; i++)
    {
        transform_ptr xform = transform_alloc_room(room, size_create(_TILE_CX - 4, _TILE_CY - 2));
        bool          ok = _gen_room_aux(page, room, xform);
        transform_free(xform);
        if (ok) return TRUE;
    }
    return FALSE;
}
static void _gen_monsters(dun_ptr dun, dun_page_ptr page)
{
    int          i, j, ct = 0;
    rect_t       r = rect_interior(page->rect);
    point_t      world_pos = dun_world_pos(rect_center(r));
    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, world_pos);
    dun_world_ptr world_info = dun_worlds_lookup(p_ptr->world_id);
    int          lvl = _difficulty(world_grid);
    int          mode = PM_ALLOW_GROUP;
    bool         allow_encounter = !(world_grid->info & (CAVE_TOWN | CAVE_ROAD | CAVE_RIVER | CAVE_DUNGEON | CAVE_QUEST));

    if (!p_ptr->dun_id && !(world_grid->info & CAVE_TOWN)) /* plr_birth: give an easy start! */
        return;

    if (world_grid->info & CAVE_TOWN)
    {
        town_ptr town = towns_lookup(world_grid->special);
        if (town->populate_f)
        {
            town->populate_f(dun, page->rect);
            return;
        }
        if (town->mon_alloc_f) 
            mon_alloc_push_weight(town->mon_alloc_f);
        if (town->flags & TF_FRIENDLY)
            mode |= PM_FORCE_FRIENDLY;
        ct = rand_range(3, 10);
    }
    else if (world_grid->info & CAVE_ROAD)
    {
        if (one_in_(2))
            ct = randint0(randint1(4));
    }
    else
    {
        ct = randint0(randint1(7));
    }

    /* XXX before adding mon_alloc filters */
    if (allow_encounter && randint0(1000) < world_info->encounter_chance)
    {
        int which = _encounter_terrain_type(world_grid->feat);
        room_ptr room = choose_room_template(ROOM_WILDERNESS, which);
        assert(cave == dun);
        if (room && _gen_room(page, room))
            ct = (ct + 1)/2; /* fewer monsters to compensate */
    }

    if (world_grid->info & CAVE_TOWN)
        mon_alloc_push_filter(mon_alloc_town);
    else if (world_grid->feat == feat_deep_water)
        mon_alloc_push_filter(mon_alloc_ocean);
    else if (world_grid->feat == feat_shallow_water)
        mon_alloc_push_filter(mon_alloc_shore);
    else if (world_grid->feat == feat_swamp)
        mon_alloc_push_filter(mon_alloc_shore);
    else if (world_grid->feat == feat_tree)
        mon_alloc_push_filter(mon_alloc_woods);
    else if (world_grid->feat == feat_mountain)
        mon_alloc_push_filter(mon_alloc_mountain);
    else if (world_grid->feat == feat_grass)
        mon_alloc_push_filter(mon_alloc_grass);
    else if (world_grid->feat == feat_dirt)
        mon_alloc_push_filter(mon_alloc_waste);
    else if (world_grid->feat == feat_shallow_lava)
        mon_alloc_push_filter(mon_alloc_volcano);
    else if (world_grid->feat == feat_deep_lava)
        mon_alloc_push_filter(mon_alloc_volcano);
    else 
        mon_alloc_push_filter(mon_alloc_grass);
    for (i = 0; i < ct; i++)
    {
        point_t pos = {0};
        mon_race_ptr race = NULL;

        if ((world_grid->info & CAVE_TOWN) || one_in_(2))
            mode |= PM_ALLOW_SLEEP;
        else
            mode &= ~PM_ALLOW_SLEEP;

        for (j = 0; j < 1000; j++)
        {
            point_t p = rect_random_point(r);
            if (!dun_allow_mon_at(dun, p)) continue;
            pos = p;
            break;
        }
        if (!rect_contains_point(r, pos)) continue;
        /*if (i == 0 && (world_grid->info & CAVE_TOWN))
            race = mon_alloc_choose_aux2(mon_alloc_current_tbl(), lvl, 0, GMN_DEBUG);
        else*/
            race = mon_alloc_choose(lvl);
        if (!race) continue;
        place_monster_aux(0, pos, race->id, mode);
    }
    mon_alloc_pop_filter();
    if (world_grid->info & CAVE_TOWN)
    {
        town_ptr town = towns_lookup(world_grid->special);
        if (town->mon_alloc_f) mon_alloc_pop_weight();
    }
}
void dun_world_dump_frac(dun_ptr dun)
{
    FILE        *fp;
    char         buf[1024];
    dun_frac_ptr frac;
    dun_page_ptr page = _find_page(dun, p_ptr->pos);
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect), p;
    point_t      world_pos = dun_world_pos(min);

    assert(dun->dun_type_id == D_SURFACE);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "frac.txt");
    fp = my_fopen(buf, "w");
    if (!fp) return;

    fprintf(fp, "Wilderness Fractal (%d, %d, %d, %d) for World Pos (%d, %d)\n",
        min.x, min.y, max.x, max.y, world_pos.x, world_pos.y);

    Rand_quick = TRUE;
    Rand_value = dun_u32b_get(_seeds, world_pos);
    frac = _gen_frac(page); 
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
            fprintf(fp, "%3d ", dun_frac_get(frac, p));
        fprintf(fp, "\n");
    }
    my_fclose(fp);

    Rand_quick = FALSE;
    msg_format("Created fractal file %s.", buf);
}
static void _gen_page(dun_ptr dun, dun_page_ptr page, dun_grid_ptr world_grid)
{
    vec_ptr      feat_map = _feat_map();
    dun_frac_ptr frac;
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect), p;
    dun_grid_ptr grid = page->grids;

    if (dun_pos_boundary(dun_mgr()->world, dun_world_pos(min)))
    {
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                grid->feat = world_grid->feat;
                grid++;
            }
        }
        _glow_page(page);
        return;
    }

    Rand_quick = TRUE;
    Rand_value = dun_u32b_get(_seeds, dun_world_pos(min));

    /* terrain */
    frac = _gen_frac(page); 
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            byte h = dun_frac_get(frac, p);
            assert(h < vec_length(feat_map));
            grid->feat = vec_get_int(feat_map, h);
            grid++;
        }
    }
    dun_frac_free(frac);
    #if 0
    dun_page_grid_at(page, min)->feat = feat_pattern_1;
    dun_page_grid_at(page, point_create(max.x, min.y))->feat = feat_pattern_1;
    dun_page_grid_at(page, point_create(min.x, max.y))->feat = feat_pattern_1;
    dun_page_grid_at(page, max)->feat = feat_pattern_1;
    #endif
    
    /* roads, town, quests and dungeon entrances */
    if (world_grid->info & CAVE_ROAD)
        _gen_roads(page);
    if (world_grid->info & CAVE_RIVER)
        _gen_rivers(page);
    if (world_grid->info & CAVE_TOWN)
    {
        assert(!(world_grid->info & (CAVE_DUNGEON | CAVE_QUEST)));
        _gen_town(dun, page, world_grid->special);
    }
    if (world_grid->info & CAVE_DUNGEON)
    {
        dun_type_ptr type = dun_types_lookup(world_grid->special);
        assert(!(world_grid->info & (CAVE_TOWN | CAVE_QUEST)));
        if (!(type->plr_flags & (DFP_FAILED | DFP_SECRET)))
            _gen_dungeon(dun, page, world_grid->special);
    }
    if (world_grid->info & CAVE_QUEST)
    {
        assert(!(world_grid->info & (CAVE_TOWN | CAVE_DUNGEON)));
        _gen_quest(dun, page, world_grid->special);
    }

    Rand_quick = FALSE;
}
static bool _home(point_t pos, dun_grid_ptr grid)
{
    dun_feat_ptr feat = dun_grid_feat(grid);
    return have_flag(feat->flags, FF_STORE) && feat->subtype == SHOP_HOME;
}
dun_ptr _gen_surface(point_t world_pos, bool place)
{
    dun_ptr world = dun_mgr()->world;
    dun_ptr surface = dun_mgr_alloc_dun(rect_invalid());
    dun_ptr old_cave = cave;
    dun_page_ptr page;
    int i;

    surface->dun_type_id = D_SURFACE;
    surface->rect = _viewport_rect(world_pos);
    cave = surface; /* XXX required for build_room_template_aux (towns) */

    /* Pass 1: Generate terrain */
    for (i = 0; i < 9; i++)
    {
        point_t      p = point_step(world_pos, ddd[i]);
        dun_grid_ptr world_grid = dun_grid_at(world, p);
        rect_t       r = _tile_rect(p);

        page = dun_page_alloc(r);
        page->next = surface->page;
        surface->page = page;
        _gen_page(surface, page, world_grid); /* <=== Might generate a town */
        world_grid->info |= CAVE_MARK;
    }
    p_ptr->window |= PW_WORLD_MAP;

    /* Pass 2: Generate monsters in a separate pass to avoid page
     * faults with monster groups (scatter might leave the current
     * page to an unallocated page; cf place_monster_group). */
    for (page = surface->page; page; page = page->next)
    {
        _gen_monsters(surface, page);
        _glow_page(page);
    }
    
    cave = old_cave;
    surface->flags |= DF_GENERATED;
    surface->flags |= (dun_types_lookup(D_SURFACE)->flags & DF_RESTRICT_MASK);
    _set_difficulty(surface);

    if (place)
    {
        point_t pos = dun_find_grid(surface, _home);
        if (!dun_pos_interior(surface, pos))
            pos = rect_center(surface->rect);
        dun_mgr_plr_change_dun(surface, pos);
    }

    return surface;
}
void dun_regen_surface(dun_ptr dun)
{
    dun_ptr world = dun_mgr()->world;
    dun_page_ptr page;
    assert(dun->dun_type_id == D_SURFACE);
    for (page = dun->page; page; page = page->next)
    {
        point_t      world_pos = dun_world_pos(rect_center(page->rect));
        dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
        _gen_page(dun, page, world_grid);
    }
}
void dun_regen_town(dun_ptr dun)
{
    point_t      world_pos = dun_world_pos(p_ptr->pos);
    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, world_pos);
    dun_page_ptr page = _find_page(dun, p_ptr->pos);

    /* plr should be on surface level, inside a town */
    assert(dun->dun_type_id == D_SURFACE);
    assert(p_ptr->dun_id == dun->dun_id);
    assert(page);

    _gen_page(dun, page, world_grid); 
}
dun_ptr dun_gen_surface(point_t world_pos)
{
    /* XXX We enforce a single D_SURFACE level at a time. Care is
     * taken that the topmost level only generates a single up stairs.
     * Should the player find an alternative route to the surface, we
     * erase the previous surface level (e.g, L0a->L1a->L2->L1b->L0b
     * will erase L0a when we generate L0b. Pathing back to L1a will then
     * erase L0b and gen an L0c) cf dun_gen_connected */
    dun_mgr_delete_surface();
    dun_mgr()->surface = _gen_surface(world_pos, FALSE);
    return dun_mgr()->surface;
}
int dun_world_town_id(void)
{
    int     town_id = 0;
    dun_ptr dun = dun_mgr_dun(p_ptr->dun_id);

    if (dun && dun->dun_type_id == D_SURFACE)
    {
        dun_ptr      world = dun_mgr()->world;
        point_t      world_pos = dun_world_pos(p_ptr->pos);
        dun_grid_ptr world_grid = dun_grid_at(world, world_pos);

        if (world_grid->info & CAVE_TOWN)
            town_id = world_grid->special;
    }
    return town_id;
}
void dun_world_move_plr(dun_ptr dun, point_t pos)
{
    point_t world_pos = dun_world_pos(pos);
    rect_t  new_rect = _viewport_rect(world_pos);

    assert(dun->dun_type_id == D_SURFACE);
    assert(dun_pos_interior(dun, pos));
    assert(cave == dun); /* for place_monster */

    if (!rect_equals(new_rect, dun->rect))
    {
        dun_page_ptr page, next;
        dun_page_ptr keep = NULL;
        point_t      min = rect_top_left(new_rect);
        point_t      max = rect_bottom_right(new_rect);
        point_t      p;

        /* forget old pages outside of new_rect */
        for (page = dun->page; page; page = next)
        {
            next = page->next;
            if (!rect_contains(new_rect, page->rect))
                dun_free_page(dun, page);
            else
            {
                page->flags &= ~PF_MARK;
                page->next = keep;
                keep = page;
                /* XXX During nightime travel, we only "mark" the player's current grid. */
                if (rect_contains_point(page->rect, pos))
                {
                    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, dun_world_pos(pos));
                    world_grid->info |= CAVE_MARK;
                }
            }
        }
        dun->page = keep;
        dun->rect = new_rect;

        /* generate new pages by walking the 3x3 lattice of topleft page rects */
        for (p.y = min.y; p.y < max.y; p.y += _TILE_CY)
        {
            for (p.x = min.x; p.x < max.x; p.x += _TILE_CX)
            {
                if (!_find_page(dun, p)) /* missing */
                {
                    rect_t       r = rect_create(p.x, p.y, _TILE_CX, _TILE_CY);
                    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, dun_world_pos(p));

                    page = dun_page_alloc(r);
                    page->flags |= PF_MARK;
                    page->next = dun->page;
                    dun->page = page;
                    _gen_page(dun, page, world_grid);
                    if (is_daytime())
                        world_grid->info |= CAVE_MARK;
                    if (world_grid->info & CAVE_DUNGEON)
                    {
                        dun_type_ptr type = dun_types_lookup(world_grid->special);
                        if (!(type->plr_flags & DFP_ENTERED))
                        {
                            msg_print("You have stumbled on to something interesting.");
                            disturb(1, 0);
                        }
                    }
                }
            }
        }
        _set_difficulty(dun);

        /* allocate monsters for new pages, but only once we have completely allocated
         * storage for dun->rect or place_monster_group might page fault.  */
        for (page = dun->page; page; page = page->next)
        {
            if (page->flags & PF_MARK)
            {
                _gen_monsters(dun, page);
                _glow_page(page);
                page->flags &= ~PF_MARK;
            }
        }
        p_ptr->window |= PW_WORLD_MAP;
    }
}

/************************************************************************
 * D_WORLD
 ************************************************************************/
static bool _plr_start_pos(point_t pos, dun_grid_ptr grid) { return (grid->info & CAVE_MARK); }
static bool _plr_start_panic_pos(point_t pos, dun_grid_ptr grid) { return BOOL(grid->info & (CAVE_TOWN)); }
static point_t _start_pos(dun_ptr world)
{
    point_vec_ptr pts = dun_filter_grids(world, _plr_start_pos);
    point_t pos = {0};
    if (!point_vec_length(pts))
    {
        point_vec_free(pts);
        pts = dun_filter_grids(world, _plr_start_panic_pos);
    }

    if (point_vec_length(pts))
    {
        int i = randint0(point_vec_length(pts));
        pos = point_vec_get(pts, i);
    }
    point_vec_free(pts);
    return pos;
}

/* scan the world map for town and dungeon locations */
static void _process_world_map(dun_ptr world)
{
    point_t min = rect_top_left(world->rect);
    point_t max = rect_bottom_right(world->rect);
    point_t p;

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_grid_ptr grid = dun_grid_at(world, p);
            if (grid->info & CAVE_DUNGEON)
            {
                dun_type_ptr type = dun_types_lookup(grid->special);
                type->world_pos = p;
                if (type->flags & DF_KNOWN)
                    grid->info |= CAVE_MARK;
                if (type->init_f)
                    type->init_f(type);
            }
            else if (grid->info & CAVE_TOWN)
                towns_lookup(grid->special)->world_pos = p;
        }
    }
}

bool dun_mgr_teleport_town(void)
{
    town_ptr    town = towns_choose(p_ptr->wizard);
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr     old_surface = dm->surface;

    if (!quests_check_leave()) return FALSE;
    if (!town) return FALSE;
    if (!dun_pos_interior(dm->world, town->world_pos)) return FALSE;

    dm->surface = _gen_surface(town->world_pos, TRUE);
    if (old_surface)
        int_map_delete(dm->dungeons, old_surface->dun_id); /* paranoia */
    return TRUE;
}
static void _world_map_msg(void)
{
    msg_print("<color:R>World Map</color> [<color:keypress>dir</color>, "
              "<color:keypress>M</color>, <color:keypress>q</color> to quit]");
}
void dun_world_map_ui(void)
{
    dun_ptr old_cave = cave;
    point_t old_viewport_origin = viewport_origin;
    point_t world_pos = dun_world_pos(p_ptr->pos);
    point_t old_plr_pos = p_ptr->pos;
    rect_t  map_rect = ui_map_rect();
    bool    done = FALSE;

    Term_clear(); /* XXX erase do_cmd_view_map screen */
    /*msg_line_clear();*/
    _world_map_msg();

    /* Make the game think the plr is in D_WORLD */
    cave = dun_mgr()->world;
    p_ptr->pos = world_pos;
    viewport_verify_aux(p_ptr->pos, VIEWPORT_FORCE_CENTER);
    while (!done)
    {
        int     cmd;
        point_t ui_pos;

        prt_map();

        /* Wait for Keypress */
        ui_pos = cave_pt_to_ui_pt(world_pos);
        if (rect_contains_point(map_rect, ui_pos))
            Term_gotoxy(ui_pos.x, ui_pos.y);
        cmd = inkey_special(TRUE);
        switch (cmd)
        {
        case SKEY_DOWN: case '2':
            viewport_origin.y--;
            break;
        case SKEY_UP: case '8':
            viewport_origin.y++;
            break;
        case SKEY_RIGHT: case '6':
            viewport_origin.x--;
            break;
        case SKEY_LEFT: case '4':
            viewport_origin.x++;
            break;
        case 'M':
            msg_line_clear();
            do_cmd_view_map();
            _world_map_msg();
            break;
        case 'q':
        case ESCAPE:
        case '\r':
            done = TRUE;
            break;
        default:
            if (quick_messages)
                done = TRUE;
        }
    }
    msg_line_clear();

    /* Back to Normal Play */
    p_ptr->pos = old_plr_pos;
    cave = old_cave;
    viewport_origin = old_viewport_origin;
    /* XXX We are now invoked from do_cmd_view_map(): prt_map();*/
}


/************************************************************************
 * Worlds
 ************************************************************************/
dun_world_ptr dun_worlds_current(void)
{
    return dun_worlds_lookup(p_ptr->world_id);
}

typedef struct {
    int            id;
    cptr           parse;
    dun_world_ptr (*create_f)(void);
} _entry_t, *_entry_ptr;

static dun_world_ptr _smaug(void);
static dun_world_ptr _saruman(void);
static dun_world_ptr _sauron(void);
static dun_world_ptr _amber(void);

static _entry_t _tbl[] = {
    { W_SMAUG, "Smaug", _smaug },
    { W_SARUMAN, "Saruman", _saruman },
    { W_SAURON, "Sauron", _sauron },
    { W_AMBER, "Amber", _amber }, 
    { 0 }
};
static dun_world_ptr dun_world_alloc(int id, cptr name)
{
    dun_world_ptr world = malloc(sizeof(dun_world_t));
    memset(world, 0, sizeof(dun_world_t));
    world->id = id;
    world->name = name;
    world->desc = name;
    return world;
}
static void dun_world_free(dun_world_ptr world)
{
    if (!world) return;
    if (world->mon_alloc_tbl) vec_free(world->mon_alloc_tbl);
    if (world->obj_alloc_tbl) vec_free(world->obj_alloc_tbl);
    free(world);
}
int dun_worlds_parse(cptr name)
{
    int i;
    for (i = 0;; i++)
    {
        _entry_ptr e = &_tbl[i];
        if (!e->create_f) break;
        if (strcmp(e->parse, name) == 0) return e->id;
    }
    return W_NONE;
}
static int_map_ptr _worlds(void)
{
    static int_map_ptr _map = NULL;
    if (!_map)
        _map = int_map_alloc((int_map_free_f)dun_world_free);
    return _map;
}
dun_world_ptr dun_worlds_lookup(int id)
{
    dun_world_ptr world = int_map_find(_worlds(), id);
    if (!world)
    {
        int i;
        for (i = 0; !world; i++)
        {
            _entry_ptr e = &_tbl[i];
            if (e->id != id) continue;
            assert(e->create_f);
            if (!e->create_f) return NULL;
            world = e->create_f();
            assert(world->id == id);
            int_map_add(_worlds(), id, world);
        }
    }
    return world;
}
static void dun_world_load(dun_world_ptr world, savefile_ptr file)
{
    world->plr_flags   = savefile_read_u16b(file);
    if (world->load_f) world->load_f(world, file);
}
static void dun_world_save(dun_world_ptr world, savefile_ptr file)
{
    savefile_write_u16b(file, world->id);
    savefile_write_u16b(file, world->plr_flags);
    if (world->save_f) world->save_f(world, file);
}
void dun_worlds_save(savefile_ptr file)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_worlds());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_world_ptr world = int_map_iter_current(iter);
        dun_world_save(world, file);
    }
    savefile_write_u16b(file, W_NONE);
    int_map_iter_free(iter);
}

void dun_worlds_load(savefile_ptr file)
{
    int_map_ptr  worlds = _worlds();
    dun_world_ptr world;
    int_map_clear(worlds);
    for (;;)
    {
        int id = savefile_read_u16b(file);
        if (id == W_NONE) break;
        world = dun_worlds_lookup(id);
        assert(world);
        assert(world->id == id);
        dun_world_load(world, file);
    }
}

/************************************************************************
 * Changing Worlds
 ************************************************************************/
static void _dun_worlds_enter(int id)
{
    dun_mgr_ptr   dm = dun_mgr();
    dun_world_ptr world = dun_worlds_lookup(id);
    dun_ptr       world_map = dun_world_gen_map(world);
    point_t       start_pos;

    if (!world_map) return;

    if (dm->world) /* changing worlds */
    {
        forget_lite(); /* XXX */
        forget_view();
        clear_mon_lite();

        int_map_delete(dm->dungeons, dm->world->dun_id);
        if (dm->surface)
            int_map_delete(dm->dungeons, dm->surface->dun_id);
        dm->world = NULL;
        dm->surface = NULL;
    }
    towns_reset_world();
    dun_types_reset_world();

    p_ptr->world_id = id;
    world->plr_flags |= WFP_ENTERED;

    assert(!int_map_find(dm->dungeons, world_map->dun_id));
    int_map_add(dm->dungeons, world_map->dun_id, world_map);
    dm->world = world_map;
    dm->world_seed = randint0(0x10000000);
    dun_world_reseed(dm->world_seed);
    
    if (world->init_f)
        world->init_f(world);

    start_pos = _start_pos(world_map); /* XXX don't start at D_AMBER */
    _process_world_map(world_map);     /* XXX this CAVE_MARKs D_AMBER for convenience */

    dun_mgr()->surface = _gen_surface(start_pos, TRUE);
}
void dun_worlds_birth(void)
{
    _dun_worlds_enter(p_ptr->initial_world_id);
}
void dun_worlds_wizard(int id)
{
    _dun_worlds_enter(id);
}
void dun_mgr_travel_plr(void)
{
    dun_world_ptr w = dun_worlds_current();
    if (!w->next_world_id)
        msg_print("Nothing happens."); /* software bug */
    else
    {
        _dun_worlds_enter(w->next_world_id);
        assert(p_ptr->world_id == w->next_world_id);
        w = dun_worlds_current();
        msg_format("<color:B>You have entered <color:r>%s</color>.</color>", w->name);
        msg_boundary();
        msg_print(w->desc);
        msg_print(NULL);
    }
}

/************************************************************************
 * Helpers for Conquerable Worlds
 ************************************************************************/
static void _conquer(dun_world_ptr me)
{
    msg_format("<color:B>You have completed <color:r>%s</color>!</color>", me->name);
    virtue_add(VIRTUE_VALOUR, 10);
    do_inc_stat(A_STR);
    do_inc_stat(A_INT);
    do_inc_stat(A_WIS);
    do_inc_stat(A_DEX);
    do_inc_stat(A_CON);
    do_inc_stat(A_CHR);
    p_ptr->fame += 5 + randint1(5);
    me->plr_flags |= WFP_COMPLETED;

}
static void _kill_mon(dun_world_ptr me, mon_ptr mon)
{
    if (!(me->plr_flags & (WFP_COMPLETED | WFP_FAILED)) && mon->r_idx == me->final_guardian)
    {
        _conquer(me);
        if (me->next_world_id)
        {
            dun_ptr dun = mon_dun(mon);
            msg_print("<color:B>New trials await you! Come, the quest continues. Be steadfast!</color>");
            dun_quest_travel(dun, mon->pos);
        }
        else
        {
            p_ptr->total_winner = TRUE;
            p_ptr->fame += 50;
            if (p_ptr->pclass == CLASS_CHAOS_WARRIOR || mut_present(MUT_CHAOS_GIFT))
            {
                msg_format("The voice of %s booms out:", chaos_patrons[p_ptr->chaos_patron]);
                msg_print("'Thou art donst well, mortal!'");
            }
            cmsg_print(TERM_L_GREEN, "*** CONGRATULATIONS ***");
            msg_print("You have won the game!");
            msg_print("You may retire (commit suicide) when you are ready.");
        }
    }
}
static void _pre_gen(dun_world_ptr me, dun_gen_ptr gen)
{
    /* In case the player did not take the initial portal, they may
     * return to the sight of their glory to regain access to the 
     * next world. */
    if ( (me->plr_flags & WFP_COMPLETED) 
      && me->next_world_id
      && gen->dun->dun_type_id == me->final_dungeon
      && gen->dun->dun_lvl == gen->type->max_dun_lvl )
    {
        if (dun_gen_template_room(gen, ROOM_ROOM, ROOM_TRAVEL))
            gen->dun->flags |= DF_TRAVEL;
    }
}
/************************************************************************
 * Smaug
 ************************************************************************/
static dun_world_ptr _smaug(void)
{
    dun_world_ptr me = dun_world_alloc(W_SMAUG, "The Desolation of Smaug");
    me->desc = "And so your quest begins. Your first challenge is to rid the world of the "
                "great dragon, <color:keyword>Smaug</color>, who dwells most of the time in "
                "<color:keyword>The Lonely Mountain</color> towards the east (Unless he is "
                "out pillaging the surrounding countryside). If you manage to complete this "
                "quest, a way will be opened for you to travel to the next world to face "
                "greater challenges. Be steadfast and may the light shine on you!";
    me->file = "w_smaug.txt";
    me->final_dungeon = D_LONELY_MOUNTAIN;
    me->final_guardian = MON_SMAUG;
    me->next_world_id = W_SARUMAN;
    me->kill_mon_f = _kill_mon;
    me->pre_gen_f = _pre_gen;
    return me;
}
/************************************************************************
 * Saruman
 ************************************************************************/
static dun_world_ptr _saruman(void)
{
    dun_world_ptr me = dun_world_alloc(W_SARUMAN, "The Betrayal of Saruman");
    me->desc = "Your quest must continue. <color:keyword>Saruman</color>, the White Wizard, "
                "has joined forces with the powers of <color:keyword>Mordor</color>. He is "
                "gathering an army at the tower of <color:keyword>Isengard</color>, an army "
                "bred for a single purpose: to destroy the world of men! You must defeat this "
                "treacherous wizard or all hope is lost.";
    me->file = "w_saruman.txt";
    me->final_dungeon = D_ISENGARD;
    me->final_guardian = MON_SARUMAN;
    me->next_world_id = W_SAURON;
    me->kill_mon_f = _kill_mon;
    me->pre_gen_f = _pre_gen;
    return me;
}
/************************************************************************
 * Sauron
 ************************************************************************/
static void _sauron_kill_mon(dun_world_ptr me, mon_ptr mon)
{
    if (mon->r_idx == MON_SAURON)
    {
        msg_print("<color:v>Congratulations!</color> And now, your greatest challenge remains. "
                   "You must travel east to <color:keyword>The Pits of Angband</color> and "
                   "face the Great Enemy: <color:keyword>Morgoth, Lord of Darkness</color>!");
        dun_types_lookup(D_ANGBAND)->plr_flags &= ~DFP_SECRET;
    }
    else _kill_mon(me, mon);
}
static dun_world_ptr _sauron(void)
{
    dun_world_ptr me = dun_world_alloc(W_SAURON, "The Depths of Mordor");
    me->desc = "Your greatest challenges await you still. <color:keyword>Sauron</color> has "
                "regained corporeal form and is preparing to destroy all of Middle Earth. "
                "More than Orcs and Trolls are gathering for a final assault. You must "
                "travel to <color:keyword>Mordor</color>, to the very heart of the enemy, "
                "and face this foe. After defeating Sauron, you will gain access to the "
                "<color:keyword>Pits of Angband</color> where the Great Enemy, <color:keyword>"
                "Morgoth</color> must be slain. Good Luck!";
    me->file = "w_sauron.txt";
    me->final_dungeon = D_ANGBAND;
    me->final_guardian = MON_MORGOTH;
    me->kill_mon_f = _sauron_kill_mon;
    me->pre_gen_f = _pre_gen;
    return me;
}
/************************************************************************
 * Amber
 ************************************************************************/
static dun_world_ptr _amber(void)
{
    dun_world_ptr me = dun_world_alloc(W_AMBER, "The World of Amber");
    me->desc = "The forces of chaos have grown troublesome of late. You must "
               "slay the dreaded <color:keyword>Serpent of Chaos</color> in "
               "order to restore balance to the world. Good Luck!";
    me->final_dungeon = D_AMBER;
    me->final_guardian = MON_SERPENT;
    me->kill_mon_f = _kill_mon;
    me->pre_gen_f = _pre_gen;
    me->encounter_chance = 10;
    return me;
}
