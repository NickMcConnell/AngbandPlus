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
static void dun_page_iter(dun_page_ptr page, dun_grid_f f)
{
    dun_grid_ptr grid = page->grids;
    point_t pos;
    for (pos.y = page->rect.y; pos.y < page->rect.y + page->rect.cy; pos.y++)
    {
        for (pos.x = page->rect.x; pos.x < page->rect.x + page->rect.cx; pos.x++)
        {
            f(pos, grid);
            grid++;
        }
    }
}
static int _difficulty(dun_grid_ptr grid)
{
    if (grid->flags & CELL_TOWN) return towns_lookup(grid->parm2)->level;
    if (grid->flags & CELL_DUNGEON) return dun_types_lookup(grid->parm2)->min_dun_lvl;
    return grid->parm2;
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
    plr->redraw |= PR_DEPTH;
}

/************************************************************************
 * D_SURFACE
 ************************************************************************/
#define _FEAT_COUNT 18
enum { /* for fractals ... need a 1-dimensional height map */
    _FEAT_DEEP_WATER,
    _FEAT_SHALLOW_WATER,
    _FEAT_SWAMP,
    _FEAT_DIRT,
    _FEAT_GRASS,
    _FEAT_FLOWER,
    _FEAT_BRAKE,
    _FEAT_TREE,
    _FEAT_MOUNTAIN,
    _FEAT_SHALLOW_LAVA,
    _FEAT_DEEP_LAVA
};
static void _feat_make(int feat, dun_cell_ptr cell)
{
    switch (feat)
    {
    case _FEAT_DEEP_WATER:
        cell_make_deep_water(cell);
        break;
    case _FEAT_SHALLOW_WATER:
        cell_make_shallow_water(cell);
        break;
    case _FEAT_SWAMP:
        cell_make_swamp(cell);
        break;
    case _FEAT_DIRT:
        cell_make_dirt(cell);
        break;
    case _FEAT_GRASS:
        cell_make_grass(cell);
        break;
    case _FEAT_FLOWER:
        cell_make_flower(cell);
        break;
    case _FEAT_BRAKE:
        cell_make_brake(cell);
        break;
    case _FEAT_TREE:
        cell_make_tree(cell);
        break;
    case _FEAT_MOUNTAIN:
        cell_make_mountain(cell);
        break;
    case _FEAT_SHALLOW_LAVA:
        cell_make_shallow_lava(cell);
        break;
    case _FEAT_DEEP_LAVA:
        cell_make_deep_lava(cell);
        break;
    default:
        cell_make_floor(cell);
    }
}
static int _initial_height(dun_cell_ptr cell)
{
    if (water_is_deep(cell)) return 14;
    if (water_is_shallow(cell)) return 40;
    if (water_is_swamp(cell)) return 48;
    if (floor_is_dirt(cell)) return 52;
    if (floor_is_grass(cell)) return 61;
    if (cell_is_tree(cell)) return 83;
    if (wall_is_mountain(cell)) return 120;
    if (lava_is_shallow(cell)) return 144;
    if (lava_is_deep(cell)) return 148;
    return 140; /* world boundary is MOUNTAIN_WALL */
}
static vec_ptr _feat_map(void) /* XXX this must match _feat_map in dun_world_gen.c */
{
    static vec_ptr _v = NULL;
    if (!_v)
    {
        _v = vec_alloc(NULL);
        /* deep water 36 0-35 */
        _add_feat(_v, _FEAT_DEEP_WATER, 30);
        _add_feat(_v, _FEAT_SHALLOW_WATER, 6);

        /* shallow water 15 36-50 */
        _add_feat(_v, _FEAT_SHALLOW_WATER, 10);
        _add_feat(_v, _FEAT_SWAMP, 5);

        /* grass 15 51-65 */
        _add_feat(_v, _FEAT_DIRT, 3);
        _add_feat(_v, _FEAT_GRASS, 7);
        _add_feat(_v, _FEAT_FLOWER, 1);
        _add_feat(_v, _FEAT_BRAKE, 2);
        _add_feat(_v, _FEAT_TREE, 2);

        /* tree 35 66-100 */
        _add_feat(_v, _FEAT_TREE, 4);
        _add_feat(_v, _FEAT_GRASS, 1);
        _add_feat(_v, _FEAT_BRAKE, 1);
        _add_feat(_v, _FEAT_TREE, 4);
        _add_feat(_v, _FEAT_GRASS, 1);
        _add_feat(_v, _FEAT_BRAKE, 1);
        _add_feat(_v, _FEAT_TREE, 4);
        _add_feat(_v, _FEAT_GRASS, 1);
        _add_feat(_v, _FEAT_BRAKE, 1);
        _add_feat(_v, _FEAT_TREE, 4);
        _add_feat(_v, _FEAT_GRASS, 1);
        _add_feat(_v, _FEAT_BRAKE, 1);
        _add_feat(_v, _FEAT_GRASS, 1);
        _add_feat(_v, _FEAT_TREE, 4);
        _add_feat(_v, _FEAT_GRASS, 2);
        _add_feat(_v, _FEAT_TREE, 4);

        /* mountain 40 101-140 */
        _add_feat(_v, _FEAT_MOUNTAIN, 4);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 1);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 5);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 1);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 3);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 3);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 2);
        _add_feat(_v, _FEAT_TREE, 3);
        _add_feat(_v, _FEAT_GRASS, 2);
        _add_feat(_v, _FEAT_MOUNTAIN, 3);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 1);
        _add_feat(_v, _FEAT_TREE, 1);
        _add_feat(_v, _FEAT_MOUNTAIN, 4);

        /* shallow lava 7 */
        _add_feat(_v, _FEAT_SHALLOW_LAVA, 7);

        /* deep lava 3 */
        _add_feat(_v, _FEAT_DEEP_LAVA, 3);
        assert(vec_length(_v) <= 255);
    }
    return _v;
}
/* road|river driver (recursive) */
typedef void (*_road_f)(dun_cell_ptr cell, int d, int w);
static void _road_aux(dun_page_ptr page, point_t start, point_t stop, _road_f feat, int width, point_t perturb)
{
    int length = point_distance(start, stop);

    if (length > 4)
    {
        point_t mp = point_perturbed_midpoint_aux(start, stop, perturb);
        dun_grid_ptr g;
        if (!rect_contains_point(page->rect, mp))
            mp = point_midpoint(start, stop);

        g = dun_page_grid_at(page, mp);
        if (!(g->flags & CELL_TOWN))
            feat(g, 0, width);
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
                    if (g->flags & CELL_TOWN) continue; /* don't clobber towns! */

                    dist = point_distance(p2, p);
                    if (dist > rand_spread(width, 1)) continue;

                    feat(g, dist, width);
                }
            }
        }
    }
}
/* roads */
static cell_make_f _feat[3]; /* shared with rivers */
static void _road_cell(dun_cell_ptr cell, int d, int w)
{
    if (d == 0)
    {
        _feat[0](cell);
        cell->flags |= CELL_ROAD;
        return;
    }
    if (cell->flags & CELL_ROAD) return;
    if (d <= w)
    {
        _feat[1](cell);
        cell->flags |= CELL_ROAD;
    }
    else _feat[2](cell);
}
static void _road(dun_page_ptr page, point_t start, point_t stop, int width)
{
    dun_grid_ptr g = dun_page_grid_at(page, start);
    if (!(g->flags & CELL_TOWN)) 
    {
        _feat[0](g);
        g->flags |= CELL_ROAD;
    }
    g = dun_page_grid_at(page, stop);
    if (!(g->flags & CELL_TOWN))
    {
        _feat[0](g);
        g->flags |= CELL_ROAD;
    }
    _road_aux(page, start, stop, _road_cell, width, point_create(75, 25));
}
static void _gen_roads(dun_page_ptr page)
{
    dun_ptr      world = dun_mgr()->world;
    point_t      center = rect_center(page->rect);
    point_t      world_pos = dun_world_pos(center);
    dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
    point_t      adj_world_pos;

    _feat[0] = cell_make_road;
    _feat[1] = cell_make_floor;
    _feat[2] = cell_make_dirt;

    if (wall_is_mountain(world_grid))
        _feat[2] = cell_make_rubble;
    else if (water_is_deep(world_grid))
        _feat[2] = cell_make_swamp;
    else if (cell_is_tree(world_grid))
        _feat[2] = cell_make_flower;

    /* north road */
    adj_world_pos = point_step(world_pos, 8);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_ROAD)
    {
        point_t stop = rect_top_center(page->rect);
        _road(page, center, stop, 3);
    }
    /* south road */
    adj_world_pos = point_step(world_pos, 2);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_ROAD)
    {
        point_t stop = rect_bottom_center(page->rect);
        _road(page, center, stop, 3);
    }
    /* east road */
    adj_world_pos = point_step(world_pos, 6);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_ROAD)
    {
        point_t stop = rect_right_center(page->rect);
        _road(page, center, stop, 2);
    }
    /* weast road */
    adj_world_pos = point_step(world_pos, 4);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_ROAD)
    {
        point_t stop = rect_left_center(page->rect);
        _road(page, center, stop, 2);
    }
}
/* rivers */
static void _river_cell(dun_cell_ptr cell, int d, int w)
{
    if (d == 0)
    {
        _feat[0](cell);
        cell->flags |= CELL_RIVER;
        return;
    }
    if (cell->flags & CELL_RIVER) return;
    if (d <= w)
    {
        _feat[1](cell);
        cell->flags |= CELL_RIVER;
    }
    else _feat[2](cell);
}
static void _river(dun_page_ptr page, point_t start, point_t stop, int width)
{
    dun_grid_ptr g = dun_page_grid_at(page, start);
    if (!(g->flags & CELL_TOWN))
    {
        _feat[0](g);
        g->flags |= CELL_RIVER;
    }
    g = dun_page_grid_at(page, stop);
    if (!(g->flags & CELL_TOWN))
    {
        _feat[0](g);
        g->flags |= CELL_RIVER;
    }
    _road_aux(page, start, stop, _river_cell, width, point_create(75, 50));
}
static void _gen_rivers(dun_page_ptr page)
{
    dun_ptr      world = dun_mgr()->world;
    point_t      center = rect_center(page->rect);
    point_t      world_pos = dun_world_pos(center);
    dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
    point_t      adj_world_pos;
    int          width = 8;

    if (dun_worlds_current()->flags & WF_RIVER_LAVA)
    {
        _feat[0] = cell_make_shallow_lava;
        _feat[1] = cell_make_shallow_lava;
        _feat[2] = cell_make_magma;
    }
    else
    {
        _feat[0] = cell_make_shallow_water;
        _feat[1] = cell_make_shallow_water;
        _feat[2] = cell_make_dirt;
        if (cell_is_water(world_grid))
            _feat[2] = cell_make_shallow_water;
        else if (water_is_swamp(world_grid))
            _feat[2] = cell_make_swamp;
        else if (cell_is_tree(world_grid))
            _feat[2] = cell_make_brake;
    }
    /* north river */
    adj_world_pos = point_step(world_pos, 8);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_RIVER)
    {
        point_t stop = rect_top_center(page->rect);
        _river(page, center, stop, width + 2);
    }
    /* south river */
    adj_world_pos = point_step(world_pos, 2);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_RIVER)
    {
        point_t stop = rect_bottom_center(page->rect);
        _river(page, center, stop, width + 2);
    }
    /* east river */
    adj_world_pos = point_step(world_pos, 6);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_RIVER)
    {
        point_t stop = rect_right_center(page->rect);
        _river(page, center, stop, width - 2);
    }
    /* weast river */
    adj_world_pos = point_step(world_pos, 4);
    if (dun_grid_at(world, adj_world_pos)->flags & CELL_RIVER)
    {
        point_t stop = rect_left_center(page->rect);
        _river(page, center, stop, width - 2);
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
            if (stairs_enter_quest(grid))
            {
                dun_stairs_ptr stairs = _alloc_stairs();
                stairs->pos_here = p;
                stairs->dun_type_id = D_QUEST;
                stairs->dun_lvl = stairs_quest_id(grid); /* XXX */

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
        xform->dest = rect_translate_xy(xform->dest, page->rect.x, page->rect.y);
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

    cell_make_dungeon_entrance(g, dun_type_id);

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
    bool         light = is_daytime();

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            if (light)
            {
                if ( (grid->flags & CELL_ROOM)
                  && grid->type != FEAT_WALL
                  && grid->type != FEAT_DOOR )
                {
                    /* TODO */
                }
                else
                {
                    grid->flags |= CELL_AWARE;
                    if (view_perma_grids) grid->flags |= CELL_MAP;
                }
            }
            else
            {
                if (cell_is_boring(grid))
                    grid->flags &= ~CELL_MAP;

                if ( stairs_enter_quest(grid)
                  || stairs_enter_dungeon(grid) )
                {
                    grid->flags |= CELL_AWARE;
                    if (view_perma_grids) grid->flags |= CELL_MAP;
                }
            }
            grid++;
        }
    }

}
static int _wild_perturb(dun_frac_ptr frac, int scale)
    { return rand_range(-frac->rough, frac->rough); }
static int _feat_weight(dun_cell_ptr cell)
{
    if (water_is_deep(cell)) return 3;
    if (water_is_shallow(cell)) return 3;
    if (water_is_swamp(cell)) return 2;
    if (floor_is_dirt(cell)) return 2;
    if (floor_is_grass(cell)) return 2;
    if (cell_is_tree(cell)) return 2;
    if (wall_is_mountain(cell)) return 5;
    if (lava_is_shallow(cell)) return 5;
    if (lava_is_deep(cell)) return 5;
    return 2; /* world boundary is MOUNTAIN_WALL */
}
static int _smooth_pos(dun_frac_ptr frac, point_t pos)
{
    dun_mgr_ptr  dm = dun_mgr();
    int th = 16*(100 + dun_frac_get(frac, pos));
    int tw = 16;
    int i;
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        int h = 100 + dun_frac_get(frac, p);
        dun_grid_ptr wg = dun_grid_at(dm->world, p);
        int w = _feat_weight(wg);
        th += w*h;
        tw += w;
    }
    return th / tw - 100;
}
static void _smooth(dun_frac_ptr frac)
{
    dun_mgr_ptr  dm = dun_mgr();
    point_t      min = rect_top_left(frac->rect);
    point_t      max = rect_bottom_right(frac->rect);
    point_t      p;

    /* smooth (interior)  XXX need better algorithm XXX */
    for (p.y = min.y + 1; p.y < max.y; p.y++)
    {
        for (p.x = min.x + 1; p.x < max.x; p.x++)
        {
            dun_grid_ptr wg = dun_grid_at(dm->world, p);
            if (wg->type == FEAT_LAVA) continue;
            dun_frac_set(frac, p, _smooth_pos(frac, p));
        }
    }
}
static dun_frac_ptr _gen_world_frac(void)
{
    dun_mgr_ptr  dm = dun_mgr();
    dun_frac_ptr frac = dun_frac_alloc(dm->world->rect, vec_length(_feat_map()));
    point_t      min = rect_top_left(dm->world->rect);
    point_t      max = rect_bottom_right(dm->world->rect);
    point_t      p;
    int i;

    /* initialize */
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_grid_ptr wg = dun_grid_at(dm->world, p);
            dun_frac_set(frac, p, _initial_height(wg));
        }
    }

    /* smooth (interior)  XXX need better algorithm XXX */
    for (i = 0; i < 1; i++)
        _smooth(frac);

    return frac;
}
static dun_frac_ptr _gen_frac(dun_page_ptr page)
{
    dun_mgr_ptr  dm = dun_mgr();
    dun_ptr      world = dm->world;
    vec_ptr      feat_map = _feat_map();
    dun_frac_ptr frac = dun_frac_alloc(page->rect, vec_length(feat_map) - 1);

    /* compute some basic points (4 corners, center, and 4 side mid-points) */
    point_t      tl = rect_top_left(page->rect);
    point_t      br = rect_bottom_right(page->rect);
    point_t      tr = rect_top_right(page->rect);
    point_t      bl = rect_bottom_left(page->rect);
    point_t      c = rect_center(page->rect);
    point_t      tc = rect_top_center(page->rect);
    point_t      lc = rect_left_center(page->rect);
    point_t      bc = rect_bottom_center(page->rect);
    point_t      rc = rect_right_center(page->rect);

    point_t      world_pos = dun_world_pos(c);
    int          heights[3][3];
    int          i, j, h;

    /* XXX build a "fractal" for this hard-coded wilderness map */
    if (!dm->world_frac && plr->world_id != W_AMBER)
        dm->world_frac = _gen_world_frac();

    /* extract fractal height info for 3x3 world viewport centered on page
     * XXX use saved world_frac if available (cf dun_world_gen.c) */
    for (j = 0; j < 3; j++)
    {
        for (i = 0; i < 3; i++)
        {
            point_t      d = point_create(i - 1, j - 1);
            point_t      p = point_add(world_pos, d);
            dun_grid_ptr wg = dun_grid_at(world, p);

            if (dm->world_frac) h = dun_frac_get(dm->world_frac, p);
            else h = _initial_height(wg);

            heights[j][i] = h;
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
static bool _notice(room_ptr room)
{
    /* Notice wilderness encounters, but only in the daytime.
     * Don't notice trivial encounters (require ROOM_NOTICE to be set) */
    if (room->type != ROOM_WILDERNESS) return FALSE;
    if (plr->wizard) return TRUE; /* XXX Testing */
    if (!(room->flags & ROOM_NOTICE)) return FALSE;
    if (!is_daytime()) return FALSE;
    return TRUE;
}
static bool _gen_room_aux(dun_page_ptr page, room_ptr room, transform_ptr xform)
{
    rect_t pr = rect_deflate(page->rect, 2, 1);
    rect_t rr = rect_translate_xy(xform->dest,
        pr.x + 2 + randint0(pr.cx - xform->dest.cx - 4),
        pr.y + 1 + randint0(pr.cy - xform->dest.cy - 2));

    if (!rect_contains(pr, rr)) return FALSE;

    xform->dest = rr;
    build_room_template_aux(room, xform);

    if (_notice(room))
    {
        if (plr->wizard)
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
    int          lvl = _difficulty(world_grid);
    int          mode = PM_ALLOW_GROUP;

    #ifdef DEVELOPER
    if (0 && plr->wizard) return;
    #endif

    if (!plr->dun_id && !(world_grid->flags & CELL_TOWN)) /* plr_birth: give an easy start! */
        return;

    if (world_grid->flags & CELL_TOWN)
    {
        town_ptr town = towns_lookup(world_grid->parm2);
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
    else
    {
        if (world_grid->flags & CELL_ROAD)
        {
            if (_1d(200) <= lvl)
                ct = _1d(4);
        }
        else if (_1d(80) <= lvl)
        {
            ct = _2d(3);
        }
    }

    {
        dun_world_ptr world_info = dun_worlds_lookup(plr->world_id);
        bool          allow_encounter = !(world_grid->flags & (CELL_TOWN | CELL_ROAD | CELL_RIVER | CELL_DUNGEON | CELL_QUEST));
        /* XXX before adding mon_alloc filters */
        if (allow_encounter && randint0(1000) < world_info->encounter_chance)
        {
            int      which = sym_add(cell_name(world_grid));
            room_ptr room = choose_room_template(ROOM_WILDERNESS, which);
            assert(cave == dun);
            if (room && _gen_room(page, room))
                ct = (ct + 1)/2; /* fewer monsters to compensate */
        }
    }

    if (world_grid->flags & CELL_TOWN)
        mon_alloc_push_filter(mon_alloc_town);
    else if (water_is_deep(world_grid))
        mon_alloc_push_filter(mon_alloc_ocean);
    else if (water_is_shallow(world_grid))
        mon_alloc_push_filter(mon_alloc_shore);
    else if (water_is_swamp(world_grid))
        mon_alloc_push_filter(mon_alloc_shore);
    else if (floor_is_dirt(world_grid))
        mon_alloc_push_filter(mon_alloc_waste);
    else if (floor_is_grass(world_grid))
        mon_alloc_push_filter(mon_alloc_grass);
    else if (cell_is_tree(world_grid))
        mon_alloc_push_filter(mon_alloc_woods);
    else if (wall_is_mountain(world_grid))
        mon_alloc_push_filter(mon_alloc_mountain);
    else if (lava_is_shallow(world_grid))
        mon_alloc_push_filter(mon_alloc_volcano);
    else if (lava_is_deep(world_grid))
        mon_alloc_push_filter(mon_alloc_volcano);
    else 
        mon_alloc_push_filter(mon_alloc_grass);
    for (i = 0; i < ct; i++)
    {
        point_t pos = {0};
        mon_race_ptr race = NULL;

        if ((world_grid->flags & CELL_TOWN) || _1d(40) > lvl)
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
        /*if (i == 0 && (world_grid->flags & CELL_TOWN))
            race = mon_alloc_choose_aux2(mon_alloc_current_tbl(), lvl, 0, GMN_DEBUG);
        else*/
            race = mon_alloc_choose(lvl);
        if (!race) continue;
        place_monster_aux(who_create_null(), pos, race, mode);
    }
    mon_alloc_pop_filter();
    if (world_grid->flags & CELL_TOWN)
    {
        town_ptr town = towns_lookup(world_grid->parm2);
        if (town->mon_alloc_f) mon_alloc_pop_weight();
    }
}
void dun_world_dump_frac(dun_ptr dun)
{
    FILE        *fp;
    char         buf[1024];
    dun_frac_ptr frac;
    dun_page_ptr page = _find_page(dun, plr->pos);
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect), p;
    point_t      world_pos = dun_world_pos(min);

    assert(dun->type->id == D_SURFACE);

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
static void _remove_cave_mask(point_t pos, dun_grid_ptr grid)
{ 
    bool town = BOOL(grid->flags & CELL_TOWN);
    /*grid->info &= ~CAVE_MASK;*/
    if (town) grid->flags |= CELL_AWARE;
}
static void _gen_page(dun_ptr dun, dun_page_ptr page, dun_grid_ptr world_grid)
{
    vec_ptr      feat_map = _feat_map();
    dun_frac_ptr frac;
    point_t      min = rect_top_left(page->rect);
    point_t      max = rect_bottom_right(page->rect), p;
    dun_grid_ptr grid = page->grids;
    dun_world_ptr world_info = dun_worlds_lookup(plr->world_id);
    point_t      world_pos = dun_world_pos(min);

    if (dun_pos_boundary(dun_mgr()->world, dun_world_pos(min)))
    {
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                *grid = *world_grid;
                grid++;
            }
        }
        _glow_page(page);
        return;
    }

    Rand_quick = TRUE;
    Rand_value = dun_u32b_get(_seeds, world_pos);

    /* terrain */
    frac = _gen_frac(page); 
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            byte h = dun_frac_get(frac, p);
            assert(h < vec_length(feat_map));
            /* h -> _FEAT_FOO -> cell_make_foo */
            _feat_make(vec_get_int(feat_map, h), grid);
            if (world_info->surface_feat_f)
                world_info->surface_feat_f(world_info, p, grid);
            grid++;
        }
    }
    dun_frac_free(frac);
    
    /* roads, town, quests and dungeon entrances */
    if (world_grid->flags & CELL_TOWN)
    {
        assert(!(world_grid->flags & (CELL_DUNGEON | CELL_QUEST)));
        _gen_town(dun, page, world_grid->parm2);
        /* note: town grids will be marked with CELL_TOWN. road generation will
         * skip these grids */
    }
    if (world_grid->flags & CELL_ROAD)
        _gen_roads(page);
    if (world_grid->flags & CELL_RIVER)
        _gen_rivers(page);
    if (world_grid->flags & CELL_DUNGEON)
    {
        dun_type_ptr type = dun_types_lookup(world_grid->parm2);
        assert(!(world_grid->flags & (CELL_TOWN | CELL_QUEST)));
        if (!(type->flags.plr & (DF_PLR_FAILED | DF_PLR_SECRET)))
            _gen_dungeon(dun, page, world_grid->parm2);
    }
    if (world_grid->flags & CELL_QUEST)
    {
        assert(!(world_grid->flags & (CELL_TOWN | CELL_DUNGEON)));
        _gen_quest(dun, page, world_grid->parm2);
    }
    if (world_grid->flags & CELL_TOWN)
    {
        /* mark town as CELL_AWARE ... perhaps cleanup in future */
        dun_page_iter(page, _remove_cave_mask);
    }

    Rand_quick = FALSE;
}
static bool _home(point_t pos, dun_grid_ptr grid)
{
    return shop_is_home(grid);
}
dun_ptr _gen_surface(point_t world_pos, bool place)
{
    dun_ptr world = dun_mgr()->world;
    dun_ptr surface = dun_mgr_alloc_dun(rect_invalid());
    dun_ptr old_cave = cave;
    dun_page_ptr page;
    int i;

    surface->type = dun_types_lookup(D_SURFACE);
    surface->rect = _viewport_rect(world_pos);
    surface->flags |= DF_PAGELOCK; /* don't re-order pages since we iterate them */
    if (is_daytime())
        surface->ambient_light = 5;
    cave = surface; /* XXX required for build_room_template_aux (towns) */

    /* Pass 1: Generate terrain */
    for (i = 0; i < 9; i++)
    {
        point_t      p = point_step(world_pos, ddd[i]);
        dun_grid_ptr world_grid = dun_grid_at(world, p);
        rect_t       r = _tile_rect(p);

        page = dun_page_alloc(r);
        page->next = surface->page;
        if (surface->page)
            surface->page->prev = page;
        surface->page = page;
        _gen_page(surface, page, world_grid); /* <=== Might generate a town */
        world_grid->flags |= CELL_MAP;
    }
    plr->window |= PW_WORLD_MAP;

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
    surface->flags |= (dun_types_lookup(D_SURFACE)->flags.info & DF_RESTRICT_MASK);
    _set_difficulty(surface);

    if (place)
    {
        point_t pos = dun_find_grid(surface, _home);
        if (!dun_pos_interior(surface, pos))
            pos = rect_center(surface->rect);
        dun_mgr_plr_change_dun(surface, pos);
    }

    /* allow locality of reference optimization */
    surface->flags &= ~DF_PAGELOCK;

    return surface;
}
void dun_regen_surface(dun_ptr dun)
{
    dun_ptr world = dun_mgr()->world;
    dun_page_ptr page;
    assert(dun->type->id == D_SURFACE);
    dun->flags |= DF_PAGELOCK;
    for (page = dun->page; page; page = page->next)
    {
        point_t      world_pos = dun_world_pos(rect_center(page->rect));
        dun_grid_ptr world_grid = dun_grid_at(world, world_pos);
        _gen_page(dun, page, world_grid);
    }
    dun->flags &= ~DF_PAGELOCK;
}
void dun_regen_town(dun_ptr dun)
{
    point_t      world_pos = dun_world_pos(plr->pos);
    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, world_pos);
    dun_page_ptr page = _find_page(dun, plr->pos);

    /* plr should be on surface level, inside a town */
    assert(dun->type->id == D_SURFACE);
    assert(plr->dun_id == dun->id);
    assert(page);

    _gen_page(dun, page, world_grid); 
}
dun_ptr dun_gen_surface(point_t world_pos)
{
    dun_mgr_ptr dm = dun_mgr();
    bool prof = FALSE;
    if (dm->prof && dm->prof->gen_timer.paused) /* XXX detect dun_gen_connected */
    {
        z_timer_resume(&dm->prof->gen_timer);
        prof = TRUE;
    }
    /* XXX We enforce a single D_SURFACE level at a time. Care is
     * taken that the topmost level only generates a single up stairs.
     * Should the player find an alternative route to the surface, we
     * erase the previous surface level (e.g, L0a->L1a->L2->L1b->L0b
     * will erase L0a when we generate L0b. Pathing back to L1a will then
     * erase L0b and gen an L0c) cf dun_gen_connected */
    dun_mgr_delete_surface();
    dun_mgr()->surface = _gen_surface(world_pos, FALSE);
    if (dm->prof && prof)
        z_timer_pause(&dm->prof->gen_timer);
    return dun_mgr()->surface;
}
int dun_world_town_id(void)
{
    int     town_id = 0;
    dun_ptr dun = dun_mgr_dun(plr->dun_id);

    if (dun && dun->type->id == D_SURFACE)
    {
        dun_ptr      world = dun_mgr()->world;
        point_t      world_pos = dun_world_pos(plr->pos);
        dun_grid_ptr world_grid = dun_grid_at(world, world_pos);

        if (world_grid->flags & CELL_TOWN)
            town_id = world_grid->parm2;
    }
    return town_id;
}
void dun_world_move_plr(dun_ptr dun, point_t pos)
{
    dun_mgr_ptr dm = dun_mgr();
    point_t world_pos = dun_world_pos(pos);
    rect_t  new_rect = _viewport_rect(world_pos);

    assert(dun->type->id == D_SURFACE);
    assert(dun_pos_interior(dun, pos));
    assert(cave == dun); /* for place_monster */

    if (!rect_equals(new_rect, dun->rect))
    {
        dun_page_ptr page, next;
        dun_page_ptr keep = NULL;
        point_t      min = rect_top_left(new_rect);
        point_t      max = rect_bottom_right(new_rect);
        point_t      p;

        dun->flags |= DF_PAGELOCK;
        if (dm->prof)
            z_timer_resume(&dm->prof->gen_timer);

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
                if (keep)
                    keep->prev = page;
                keep = page;
                /* XXX During nightime travel, we only "mark" the player's current grid. */
                if (rect_contains_point(page->rect, pos))
                {
                    dun_grid_ptr world_grid = dun_grid_at(dun_mgr()->world, dun_world_pos(pos));
                    world_grid->flags |= CELL_MAP;
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
                    if (dun->page)
                        dun->page->prev = page;
                    dun->page = page;
                    _gen_page(dun, page, world_grid);
                    if (is_daytime())
                        world_grid->flags |= CELL_MAP;
                    if (world_grid->flags & CELL_DUNGEON)
                    {
                        dun_type_ptr type = dun_types_lookup(world_grid->parm2);
                        if (!(type->flags.plr & DF_PLR_ENTERED) && !(type->flags.plr & DF_PLR_SECRET))
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
        dun->flags &= ~DF_PAGELOCK;
        plr->window |= PW_WORLD_MAP;
        if (dm->prof)
            z_timer_pause(&dm->prof->gen_timer);
    }
}

/************************************************************************
 * D_WORLD
 ************************************************************************/
static bool _plr_start_pos(point_t pos, dun_grid_ptr grid) { return (grid->flags & CELL_MAP); }
static bool _plr_start_panic_pos(point_t pos, dun_grid_ptr grid) { return BOOL(grid->flags & CELL_TOWN); }
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
            if (grid->flags & CELL_DUNGEON)
            {
                dun_type_ptr type = dun_types_lookup(grid->parm2);
                type->world_pos = p;
                if (type->flags.info & DF_KNOWN)
                    grid->flags |= CELL_MAP;
                if (type->init_f)
                    type->init_f(type);
            }
            else if (grid->flags & CELL_TOWN)
                towns_lookup(grid->parm2)->world_pos = p;
        }
    }
}

extern void _mru_clear(dun_mgr_ptr dm); /* dun.c ... but not public! */
/* XXX There is an exploit with Teleport to Town followed by immediate Recall:
 * A dungeon guardian loses his entourage! This is due to dun_mgr_relocate_unique
 * which finds the unique on a garbage level, and restores him to the current level
 * (cf _alloc_guardian). We need to force a GC, carefully removing the mru so that
 * the old level is considered garbage. XXX */
bool dun_mgr_teleport_town(u32b flags)
{
    town_ptr    town = towns_choose(flags);
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr     old_surface = dm->surface;
    bool        gc = plr_dun()->type->id != D_SURFACE; /* detect spell vs _inn_tele_town */

    if (!quests_check_leave()) return FALSE;
    if (!town) return FALSE;
    if (!dun_pos_interior(dm->world, town->world_pos)) return FALSE;

    if (gc)
        _mru_clear(dm);
    dm->surface = _gen_surface(town->world_pos, TRUE);
    if (gc)
        dm->surface->flags |= DF_GC;
    if (old_surface)
        int_map_delete(dm->dungeons, old_surface->id); /* paranoia */
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
    point_t world_pos = dun_world_pos(plr->pos);
    point_t old_plr_pos = plr->pos;
    rect_t  map_rect = ui_map_rect();
    bool    done = FALSE;

    Term_clear(); /* XXX erase do_cmd_view_map screen */
    /*msg_line_clear();*/
    _world_map_msg();

    /* Make the game think the plr is in D_WORLD */
    cave = dun_mgr()->world;
    plr->pos = world_pos;
    viewport_verify_aux(plr->pos, VIEWPORT_FORCE_CENTER);
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
    plr->pos = old_plr_pos;
    cave = old_cave;
    viewport_origin = old_viewport_origin;
    /* XXX We are now invoked from do_cmd_view_map(): prt_map();*/
}


/************************************************************************
 * Worlds
 ************************************************************************/
dun_world_ptr dun_worlds_current(void)
{
    return dun_worlds_lookup(plr->world_id);
}

typedef struct {
    int            id;
    cptr           name; /* for flag-like parsing */
    cptr           parse; /* for more readable parsing */
    dun_world_ptr (*create_f)(void);
} _entry_t, *_entry_ptr;

static dun_world_ptr _smaug(void);
static dun_world_ptr _saruman(void);
static dun_world_ptr _sauron(void);
static dun_world_ptr _amber(void);

static _entry_t _tbl[] = {
    { W_SMAUG, "W_SMAUG", "Smaug", _smaug },
    { W_SARUMAN, "W_SARUMAN", "Saruman", _saruman },
    { W_SAURON, "W_SAURON", "Sauron", _sauron },
    { W_AMBER, "W_AMBER", "Amber", _amber }, 
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
        if (strcmp(e->name, name) == 0) return e->id;
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
        int_map_delete(dm->dungeons, dm->world->id);
        if (dm->surface)
            int_map_delete(dm->dungeons, dm->surface->id);
        dm->world = NULL;
        dm->surface = NULL;
    }
    towns_reset_world();
    dun_types_reset_world();
    plr->update = PU_UN_LIGHT | PU_UN_VIEW;

    plr->world_id = id;
    world->plr_flags |= WFP_ENTERED;

    assert(!int_map_find(dm->dungeons, world_map->id));
    int_map_add(dm->dungeons, world_map->id, world_map);
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
    _dun_worlds_enter(plr->initial_world_id);
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
        assert(plr->world_id == w->next_world_id);
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
    plr->fame += 5 + randint1(5);
    me->plr_flags |= WFP_COMPLETED;

}
static void _kill_mon(dun_world_ptr me, mon_ptr mon)
{
    if (!(me->plr_flags & (WFP_COMPLETED | WFP_FAILED)) && mon->race->id == me->final_guardian)
    {
        _conquer(me);
        if (me->next_world_id)
        {
            dun_ptr dun = mon->dun;
            msg_print("<color:B>New trials await you! Come, the quest continues. Be steadfast!</color>");
            dun_quest_travel(dun, mon->pos);
        }
        else
        {
            plr->total_winner = TRUE;
            plr->fame += 50;
            if (plr->pclass == CLASS_CHAOS_WARRIOR || mut_present(MUT_CHAOS_GIFT))
            {
                msg_format("The voice of %s booms out:", chaos_patrons[plr->chaos_patron]);
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
      && gen->dun->type->id == me->final_dungeon
      && gen->dun->dun_lvl == gen->dun->type->max_dun_lvl )
    {
        /* For DF_TOWER levels, there is about a 10% chance that the travel
         * room will get "clipped". Luck being what she is, this can happen
         * 2 or 3 times in a row! (cf dun_gen_reserve and _tower_out_of_bounds) */
        while (!dun_gen_template_room(gen, ROOM_ROOM, ROOM_TRAVEL))
        {
            if (plr->wizard)
                msg_print("<color:y>Failed to create Travel Portal!</color>");
        }
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
    me->final_guardian = mon_race_parse("D.Smaug")->id;
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
    me->final_guardian = mon_race_parse("p.Saruman")->id;
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
    if (mon_race_is_(mon->race, "p.Sauron"))
    {
        msg_print("<color:v>Congratulations!</color> And now, your greatest challenge remains. "
                   "You must travel east to <color:keyword>The Pits of Angband</color> and "
                   "face the Great Enemy: <color:keyword>Morgoth, Lord of Darkness</color>!");
        dun_types_lookup(D_ANGBAND)->flags.plr &= ~DF_PLR_SECRET;
    }
    else _kill_mon(me, mon);
}
void _mordor_feat(dun_world_ptr me, point_t pos, dun_cell_ptr cell)
{
    int mordor_x = 17 * _TILE_CX; /* XXX hacked from ../lib/edit/w_sauron.txt */
    
    if (pos.x > mordor_x)
    {
        int dx = pos.x - mordor_x;
        if (dx > 99 || randint0(100) < dx) /* no life in Mordor! (gradual transition) */
        {
            if (cell_is_tree(cell)) cell_make_dirt(cell);
            if (floor_is_flower(cell)) cell_make_dirt(cell);
            if (floor_is_grass(cell)) cell_make_swamp(cell);
        }
    }
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
    me->final_guardian = mon_race_parse("P.Morgoth")->id;
    me->kill_mon_f = _sauron_kill_mon;
    me->pre_gen_f = _pre_gen;
    me->surface_feat_f = _mordor_feat;
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
    me->final_guardian = mon_race_parse("J.Chaos")->id;
    me->kill_mon_f = _kill_mon;
    me->pre_gen_f = _pre_gen;
    me->encounter_chance = 50;
    return me;
}
