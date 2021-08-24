#include "angband.h"
#include "dun.h"
#include "dun_gen.h"
#include "dun_util.h"
#include <assert.h>

/************************************************************************
 * Read D_WORLD map from a file (hard-coded layout)
 * This currently re-uses room templates in a hackish way.
 ************************************************************************/
static room_ptr _room;
static errr _parse_room(char *line, int options)
{
    assert(_room);
    return parse_room_line(_room, line, options);
}
static dun_ptr _read_world_map(dun_world_ptr world)
{
    transform_ptr xform;
    dun_ptr       world_map = NULL;
    dun_ptr       old_cave = cave;

    assert(world);
    assert(world->file);

    _room = room_alloc("world");
    if (parse_edit_file(world->file, _parse_room, 0) != ERROR_SUCCESS)
    {
        room_free(_room);
        _room = NULL;
        quit("Unable to initialize World Map.");
        return NULL;
    }
    world_map = dun_alloc(dun_mgr_next_dun_id(), rect_create(0, 0, _room->width, _room->height));
    world_map->type = dun_types_lookup(D_WORLD);
    world_map->flags |= DF_LOCKED | DF_GENERATED;

    cave = world_map;
    xform = transform_alloc_room(_room, size_create(_room->width, _room->height));
    build_room_template_aux(_room, xform);
    transform_free(xform);
    cave = old_cave;

    room_free(_room);
    _room = NULL;

    return world_map;
}

/************************************************************************
 * Generate a random D_WORLD map (Experimental)
 ************************************************************************/
/* Fractally generated wilderness terrain. The "feat map" is used
 * to convert fractal "heights" into terrain features. */
static int _cutoff = 0; /* marks the mountain index in _feat_map() */
static int _scratch = 0;
enum { /* for fractals ... need a 1-dimensional height map */
    _FEAT_DEEP_WATER,
    _FEAT_SHALLOW_WATER,
    _FEAT_GRASS,
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
    case _FEAT_GRASS:
        cell_make_grass(cell);
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
        cell_make_grass(cell);
    }
}
static void _add_feat(vec_ptr v, int feat, int ct)
{
    int i;
    for (i = 0; i < ct; i++)
        vec_add_int(v, feat);
}
static vec_ptr _feat_map(void) /* XXX heights must match _feat_map in dun_world.c */
{
    static vec_ptr _v = NULL;
    if (!_v)
    {
        _v = vec_alloc(NULL);
        _add_feat(_v, _FEAT_DEEP_WATER, 36);
        _add_feat(_v, _FEAT_SHALLOW_WATER, 15);
        _add_feat(_v, _FEAT_GRASS, 15);
        _add_feat(_v, _FEAT_TREE, 35);
        _cutoff = vec_length(_v);
        _add_feat(_v, _FEAT_MOUNTAIN, 40);
        _add_feat(_v, _FEAT_SHALLOW_LAVA, 7);
        _add_feat(_v, _FEAT_DEEP_LAVA, 3);
        assert(vec_length(_v) <= 255);
    }
    return _v;
}
static bool _massage(dun_frac_ptr frac)
{
    /* limit the amount of mountainous terrain */
    int area = rect_area(frac->rect), pct;
    int floor = 0;
    point_t p;
    point_t min = rect_top_left(frac->rect);
    point_t max = rect_bottom_right(frac->rect);
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            int h = dun_frac_get(frac, p);
            if (h < frac->cutoff)
                floor++;
        }
    }
    pct = 100 * floor / area;
    return 70 <= pct && pct < 90; /* i.e. require 10-30% mountains ... cf _dungeons */
    /* XXX might want to limit ocean percentages ... I've seen some "water worlds"! */
}
static dun_frac_ptr _world_frac(point_t size, vec_ptr feat)
{
    int attempt = 100;
    int max_height = vec_length(feat) - 1;
    rect_t rect = rect_create(0, 0, size.x, size.y);
    dun_frac_ptr frac = dun_frac_alloc(rect, max_height);

    frac->rough = 2 + max_height * 9 / 100;
    frac->grid = 3 + max_height * 13 / 100;
    frac->cutoff = _cutoff;

    while (--attempt)
    {
        dun_frac_calc(frac);
        if (_massage(frac)) break;

        dun_frac_reset(frac);
    }
    if (!attempt)
    {
        dun_frac_free(frac);
        frac = NULL;
    }
    return frac;
}
/* A "tunneler" for created roads. This is currently rather poor. */
static int _dir(point_t pos, point_t stop)
{
    int dx = stop.x - pos.x;
    int dy = stop.y - pos.y;

    if (dx && dy) /* no diagonal movement */
    {
        if (one_in_(2)) dx = 0;
        else dy = 0;
    }
    if (dx > 0) return 6;
    if (dx < 0) return 4;
    if (dy > 0) return 2;
    if (dy < 0) return 8;

    assert(0);
    return 5;
}
static int _rand_dir(void) { return ddd[randint0(4)]; }
static int _change_dir(point_t pos, point_t stop, int rand_pct)
{
    if (randint0(100) < rand_pct)
        return _rand_dir();
    return _dir(pos, stop);
}
static bool _road(dun_ptr dun, point_t start, point_t stop)
{
    point_t      pos = start;
    int          dir = _dir(start, stop);
    point_t      next_pos;
    int          ct = 0;
    dun_grid_ptr grid;

    /* prevent towns being too close to one another */
    if (point_fast_distance(start, stop) < 5) return FALSE;

    dun_grid_at(dun, pos)->flags |= CELL_ROAD;
    while (!point_equals(pos, stop))
    {
        if (ct++ > 2000) return FALSE;
        if (randint0(100) < 70)
            dir = _change_dir(pos, stop, 10);
        next_pos = point_step(pos, dir);
        while (!dun_pos_interior(dun, next_pos))
        {
            dir = _change_dir(pos, stop, 50);
            next_pos = point_step(pos, dir);
        }
        grid = dun_grid_at(dun, next_pos);
        if (water_is_deep(grid) || water_is_shallow(grid))
            dir = _change_dir(pos, stop, 10);
        else if ((grid->flags & CELL_ROAD) && randint0(100) < 25)
            return FALSE;
        else
        {
            pos = next_pos;
            grid->flags |= CELL_ROAD;
        }
    }
    return TRUE;
}
static bool _river(dun_ptr dun, point_t start, point_t stop)
{
    point_t      pos = start;
    int          dir = _dir(start, stop);
    point_t      next_pos;
    int          ct = 0;
    dun_grid_ptr grid;

    dun_grid_at(dun, pos)->flags |= CELL_RIVER;
    while (!point_equals(pos, stop))
    {
        if (ct++ > 2000) return FALSE;
        if (randint0(100) < 70)
            dir = _change_dir(pos, stop, 0);
        next_pos = point_step(pos, dir);
        while (!dun_pos_interior(dun, next_pos))
        {
            dir = _change_dir(pos, stop, 0);
            next_pos = point_step(pos, dir);
        }
        grid = dun_grid_at(dun, next_pos);
        if (water_is_deep(grid))
        {
            grid->flags |= CELL_RIVER;
            return TRUE; /* XXX */
        }
        else if (grid->flags & CELL_TOWN)
            return FALSE;
        else if (grid->flags & CELL_RIVER)
            return FALSE;
        else
        {
            pos = next_pos;
            grid->flags |= CELL_RIVER;
        }
    }
    return TRUE;
}
/* Context information for the generation. */
typedef struct {
    dun_ptr dun;
    bool fail;
    point_vec_ptr towns, dungeons;
    point_t start_pos;
} _context_t, *_context_ptr;
static _context_ptr _context = NULL;
static _context_ptr _context_alloc(void)
{
    _context_ptr c = malloc(sizeof(_context_t));
    memset(c, 0, sizeof(_context_t));
    c->towns = point_vec_alloc();
    c->dungeons = point_vec_alloc();
    return c;
}
static void _context_free(_context_ptr c)
{
    point_vec_free(c->towns);
    point_vec_free(c->dungeons);
    free(c);
}

/* Reset the world and try again. This keeps terrain allocation but
 * resets towns, dungeons and roads. */
static void _reset_grid(point_t pos, dun_grid_ptr grid)
{
    grid->flags &= ~(CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_MAP);
    grid->parm2 = 0;
}
static void _context_reset(_context_ptr c)
{
    assert(c->dun);
    dun_iter_interior(c->dun, _reset_grid);
    point_vec_clear(c->towns);
    point_vec_clear(c->dungeons);
    c->start_pos = point_create(0, 0);
    c->fail = FALSE;
}

static void _terrain(point_t size)
{
    dun_mgr_ptr  dm = dun_mgr();
    vec_ptr      feat = _feat_map();
    dun_frac_ptr frac = _world_frac(size, feat);

    assert(!_context->dun);

    if (frac)
    {
        rect_t  rect = rect_create(0, 0, size.x, size.y);
        point_t min = rect_top_left(rect);
        point_t max = rect_bottom_right(rect);
        point_t pos;

        _context->dun = dun_alloc(dun_mgr_next_dun_id(), rect);
        _context->dun->type = dun_types_lookup(D_WORLD);
        _context->dun->flags |= DF_LOCKED | DF_GENERATED;

        for (pos.y = min.y; pos.y <= max.y; pos.y++)
        {
            for (pos.x = min.x; pos.x <= max.x; pos.x++)
            {
                byte         h = dun_frac_get(frac, pos);
                dun_grid_ptr grid = dun_grid_at(_context->dun, pos);

                /* boundary tiles must be PERMANENT */
                if (dun_pos_boundary(_context->dun, pos))
                    cell_make_mountain_wall(grid);
                else /* h -> _FEAT_FOO -> cell_make_foo */
                    _feat_make(vec_get_int(feat, h), grid);
            }
        }
        /* remember world fractal for better terrain generation (cf _gen_frac in dun_world.c) */
        if (dm->world_frac) dun_frac_free(dm->world_frac);
        dm->world_frac = frac;
    }
    else
    {
        _context->fail = TRUE;
    }
}
static int _town_w(point_t pos, dun_grid_ptr grid)
{
    int w, i;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON)) return 0;
    if (water_is_shallow(grid)) return 0;
    if (water_is_deep(grid)) return 0;

    /* towns like to be near water (for trading) but not near mountains (too many orcs!) */
    w = 100;
    assert(dun_pos_interior(_context->dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);

        if (water_is_shallow(g)) w += 10;
        else if (water_is_deep(g)) w += 10;
        else if (wall_is_mountain(g)) w -= 5;
        else if (lava_is_shallow(g)) w -= 10;
        else if (lava_is_deep(g)) w -= 20;
    }
    return MAX(0, w);
}
static int _town_grass_w(point_t pos, dun_grid_ptr grid)
{
    if (!floor_is_grass(grid)) return 0;
    return _town_w(pos, grid);
}
static int _town_forest_w(point_t pos, dun_grid_ptr grid)
{
    int i, ct = 0;
    if (!cell_is_tree(grid)) return 0;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON)) return 0;

    /* TOWN_ANGWIL should be buried deep in a forest ... */
    assert(dun_pos_interior(_context->dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        if (!cell_is_tree(g))
            ct++;
    }
    if (ct) return 1; /* ... most of the time (paranoia) */
    return 1000;
}
static int _town_mountain_w(point_t pos, dun_grid_ptr grid)
{
    int i, ct = 0;
    if (!wall_is_mountain(grid)) return 0;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON)) return 0;

    /* TOWN_ZUL should be buried deep in the mountains ... */
    assert(dun_pos_interior(_context->dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        if (g->type != FEAT_WALL && g->type != FEAT_LAVA)
            ct++;
    }
    if (ct) return 1; /* ... most of the time (paranoia) */
    return 1000;
}
static void _town(int which, dun_grid_weight_f weight)
{
    point_t      p = dun_random_grid(_context->dun, weight);
    dun_grid_ptr g;
    town_ptr     t = towns_lookup(which);

    if (!dun_pos_interior(_context->dun, p))
    {
        _context->fail = TRUE;
        return;
    }
    g = dun_grid_at(_context->dun, p);
    g->flags |= CELL_TOWN;
    g->parm2 = which;
    if (!(t->flags & TF_SECRET))
    {
        point_vec_add(_context->towns, p);
        if (!dun_pos_interior(_context->dun, _context->start_pos))
        {
            g->flags |= CELL_MAP;
            _context->start_pos = p;
        }
    }
}
static void _towns(void)
{
    _town(TOWN_OUTPOST, _town_grass_w);
    _town(TOWN_ANGWIL, _town_forest_w);
    _town(TOWN_TELMORA, _town_grass_w);
    _town(TOWN_ZUL, _town_mountain_w);
}
static void _road_check(point_t pos, dun_grid_ptr grid)
{
    if (grid->flags & CELL_ROAD)
    {
        int i, ct = 0;
        bool on = FALSE;
        /* Look for a "four square" revealed by a run of 3 or more consecutive
         * ROAD grids during circular traversal. Note that each 4 square will
         * be counted multiple times. */
        for (i = 0; i < 8; i++)
        {
            point_t p = point_step(pos, cdd[i]);
            dun_grid_ptr g = dun_grid_at(_context->dun, p);
            if (g->flags & CELL_ROAD)
            {
                if (!on)
                {
                    on = TRUE;
                    ct = 0;
                }
                ct++;
                if (ct > 2) break;
            }
            else if (on)
            {
                assert(ct < 3);
                on = FALSE;
                ct = 0;
            }
        }
        if (ct > 2) _scratch++;
    }
}
static void _roads(void)
{
    if (point_vec_length(_context->towns) > 1)
    {
        point_t pos, prev;
        int i;

        _sort_cluster(_context->towns, rect_center(_context->dun->rect));
        prev = point_vec_get(_context->towns, 0);
        for (i = 1; i < point_vec_length(_context->towns); i++)
        {
            pos = point_vec_get(_context->towns, i);
            if (!_road(_context->dun, prev, pos))
            {
                _context->fail = TRUE;
                break;
            }
            prev = pos;
        }

        _scratch = 0;
        dun_iter_interior(_context->dun, _road_check);
        if (_scratch / 2 > 3)
            _context->fail = TRUE;
    } 
}
static int _start_dun_w(point_t pos, dun_grid_ptr grid)
{
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;
    if (!floor_is_grass(grid) && !cell_is_tree(grid)) return 0;

    /* the start dungeon s/b adjacent to the start town so the player knows what to do */
    switch (point_fast_distance(pos, _context->start_pos))
    {
    case 1: return 100;
    case 2: return 1;
    }
    return 0;
}
static int _win_dun_w(point_t pos, dun_grid_ptr grid)
{
    int d, w;

    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;

    /* the win dungeon s/b reasonably close to town. we assume this dungeon is
     * running DL1 to DL100, so it should be easy to find. don't require levitation
     * for access. */
    if (!floor_is_grass(grid) && !cell_is_tree(grid)) return 0;

    d = point_fast_distance(pos, _context->start_pos);
    if (d < 3) w = 0;  /* not too close, though */
    else w = 100 - d*d/2;

    return MAX(0, w);
}
static int _closest_town(point_t pos)
{
    int min = 1000, i;
    for (i = 0; i < point_vec_length(_context->towns); i++)
    {
        point_t p = point_vec_get(_context->towns, i);
        int     d = point_fast_distance(p, pos);
        if (!i || d < min)
            min = d;
    }
    return min;
}
static int _shy_dun_w(point_t pos, dun_grid_ptr grid)
{
    int d, w;

    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;

    d = _closest_town(pos);
    if (d < 5) w = 0;
    else w = d + d*d/5;

    assert(w >= 0);
    return MIN(1000, w);
}
static int _dun_w(point_t pos, dun_grid_ptr grid)
{
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;
    if (_closest_town(pos) < 5) return 0;
    return 1;
}
static int _forest_w(point_t pos, dun_grid_ptr grid)
{
    if (!cell_is_tree(grid)) return 0;
    return _dun_w(pos, grid);
}
static int _shy_mountain_w(point_t pos, dun_grid_ptr grid)
{
    if (!wall_is_mountain(grid)) return 0;
    return _shy_dun_w(pos, grid);
}
static int _mountain_w(point_t pos, dun_grid_ptr grid)
{
    if (!wall_is_mountain(grid)) return 0;
    return _dun_w(pos, grid);
}
static int _inner_mountain_w(point_t pos, dun_grid_ptr grid)
{
    int i, ct = 0;
    if (!wall_is_mountain(grid)) return 0;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;

    assert(dun_pos_interior(_context->dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        if (g->type != FEAT_WALL && g->type != FEAT_LAVA)
            ct++;
    }
    if (ct) return 1; /* ... most of the time (paranoia) */
    return 1000;
}
static int _foothills_w(point_t pos, dun_grid_ptr grid)
{
    int ct = 0, i;
    if (!wall_is_mountain(grid)) return 0;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;

    /* This is for D_ORC_CAVE, a low level dungeon. We are looking for a mountain
     * entrance the won't be too difficult (should be a boundary point for a
     * mountainous region) */
    assert(dun_pos_interior(_context->dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        if (g->type != FEAT_WALL && g->type != FEAT_LAVA)
            ct++;
    }
    return ct*ct*ct;
}
static int _swamp_w(point_t pos, dun_grid_ptr grid)
{
    int i, ct = 0;
    if (!water_is_shallow(grid)) return 0;
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_RIVER)) return 0;
    if (_closest_town(pos) < 5) return 0;

    /* feat_swamp is not being used, so fake it, looking for the shores of a lake */
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        if (floor_is_grass(g) || cell_is_tree(g)) ct++;
    }
    return 1 + 10*ct*ct;
}
static int _shy_swamp_w(point_t pos, dun_grid_ptr grid)
{
    int w = _swamp_w(pos, grid);
    if (w)
    {
        int d = _closest_town(pos);
        if (d < 5) w = 0;
        else w = w * d * d / 10;
    }
    return MIN(1000, w);
}
static int _shy_dry_w(point_t pos, dun_grid_ptr grid)
{
    if (grid->type == FEAT_WATER) return 0;
    return _shy_dun_w(pos, grid);
}
static int _deep_water_w(point_t pos, dun_grid_ptr grid)
{
    if (!water_is_deep(grid)) return 0;
    return _dun_w(pos, grid);
}
/* The game is more interesting if the available dungeons are
 * randomized for each game. This also allows more variety without
 * overcrowding the wilderness. We'll divide the available dungeons
 * into tiers for better distribution (e.g. ensure at least 1 high
 * difficulty dungeon). */
typedef struct {
    int id;
    int tier;
    dun_grid_weight_f location;
    int run;
} _dun_tbl_t, *_dun_tbl_ptr;
static _dun_tbl_t _dun_tbl[] = {
    /* Tier 1: End DL32-35 */
    { D_ICKY_CAVE, 1, _swamp_w },
    { D_FOREST, 1, _forest_w },
    { D_CAMELOT, 1, _shy_dry_w },
    /* Tier 2: End DL40-45 */
    { D_MOUNTAIN, 2, _mountain_w },
    { D_DARK_CAVE, 2, _foothills_w },
    /* Tier 3: End DL50 or even DL60 */
    { D_WIZARDS_TOWER, 3, _shy_dry_w },
    { D_MONASTERY, 3, _shy_dry_w },
    { D_SANCTUARY, 3, _shy_dry_w },
    { D_CASTLE, 3, _shy_dry_w },
    { D_DARK_CASTLE, 3, _inner_mountain_w },
    /* Tier 4: End DL70 or more */
    { D_RLYEH, 4, _deep_water_w },
    { D_DRAGONS_LAIR, 4, _shy_mountain_w },
    { D_GRAVEYARD, 4, _shy_swamp_w },
    { D_NUMENOR, 4, _deep_water_w },
    { D_PANDEMONIUM, 4, _shy_mountain_w },
    { D_NONE }
};
static int _run = 0;
static void _dungeon(int which, dun_grid_weight_f weight)
{
    point_t      p;
    dun_grid_ptr g;

    if (_context->fail) return;
    p = dun_random_grid(_context->dun, weight);
    if (!dun_pos_interior(_context->dun, p))
    {
        _context->fail = TRUE;
        return;
    }
    g = dun_grid_at(_context->dun, p);
    g->flags |= CELL_DUNGEON;
    g->parm2 = which;
    point_vec_add(_context->dungeons, p);
}
static bool _random_dungeon(int tier)
{
    int i, tot = 0, n;
    for (i = 0; ; i++)
    {
        _dun_tbl_ptr entry = &_dun_tbl[i];
        if (entry->id == D_NONE) break;
        if (entry->tier != tier) continue;
        if (entry->run >= _run) continue;
        tot++;
    }
    if (!tot) return FALSE;
    n = randint0(tot);
    for (i = 0; ; i++)
    {
        _dun_tbl_ptr entry = &_dun_tbl[i];
        if (entry->id == D_NONE) break;
        if (entry->tier != tier) continue;
        if (entry->run >= _run) continue;
        if (--n < 0)
        {
            entry->run = _run; /* don't re-pick this dungeon */
            _dungeon(entry->id, entry->location);
            return TRUE;
        }
    }

    return FALSE;
}
static void _random_dungeons(int tier, int count)
{
    int i;
    for (i = 0; i < count; i++)
        _random_dungeon(tier);
}
static void _dungeons(void)
{
    _run++;

    /* fixed dungeons */
    _dungeon(D_STRONGHOLD, _start_dun_w);
    _dungeon(D_AMBER, _win_dun_w);
    _dungeon(D_ORC_CAVE, _foothills_w);

    /* randomized dungeons */
    _random_dungeons(1, _1d(2));
    _random_dungeons(2, 1);
    _random_dungeons(3, 2);
    _random_dungeons(4, 2);
}
static int _water_w(point_t pos, dun_grid_ptr grid)
{
    if (water_is_deep(grid)) return 1;
    return 0;
}
static void _river_check(point_t pos, dun_grid_ptr grid)
{
    if (grid->flags & CELL_RIVER)
    {
        int i, ct = 0;
        bool on = FALSE;
        /* Look for a "four square" revealed by a run of 3 or more consecutive
         * RIVER grids during circular traversal. Note that each 4 square will
         * be counted multiple times. */
        for (i = 0; i < 8; i++)
        {
            point_t p = point_step(pos, cdd[i]);
            dun_grid_ptr g = dun_grid_at(_context->dun, p);
            if (g->flags & CELL_RIVER)
            {
                if (!on)
                {
                    on = TRUE;
                    ct = 0;
                }
                ct++;
                if (ct > 2) break;
            }
            else if (on)
            {
                assert(ct < 3);
                on = FALSE;
                ct = 0;
            }
        }
        if (ct > 2) _scratch++;
    }
}
static void _rivers(void)
{
    int ct = _1d(3), i;
    for (i = 0; i < ct; i++)
    {
        point_t start = dun_random_grid(_context->dun, _shy_mountain_w);
        point_t stop = dun_random_grid(_context->dun, _water_w);
        if (dun_pos_interior(_context->dun, start) && dun_pos_interior(_context->dun, stop))
            _river(_context->dun, start, stop); /* XXX ignore failures */
    }
    _scratch = 0;
    dun_iter_interior(_context->dun, _river_check);
    if (_scratch / 2 > 0)
        _context->fail = TRUE;
}
static int _terrain_difficulty(dun_cell_ptr cell)
{
    if (water_is_deep(cell)) return 50;
    if (water_is_shallow(cell)) return 10; /* towns are common by the sea */
    if (floor_is_grass(cell)) return 5;
    if (cell_is_tree(cell)) return 20;
    if (wall_is_mountain(cell)) return 40;
    if (lava_is_shallow(cell)) return 60;
    if (lava_is_deep(cell)) return 70;
    if (wall_is_mountain_wall(cell)) return 20;

    return 0;
}
static void _grid_difficulty(point_t pos, dun_grid_ptr grid)
{
    int i, t = 0, w = 0;
    /* grid->parm2 means something else for towns and dungeons */
    if (grid->flags & (CELL_TOWN | CELL_DUNGEON)) return;

    /* weigh in nearby dungeons */
    for (i = 0; i < point_vec_length(_context->dungeons); i++)
    {
        point_t p = point_vec_get(_context->dungeons, i);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        dun_type_ptr dt;
        int d = point_fast_distance(pos, p);

        if (d > 9) continue;
        assert(g->flags & CELL_DUNGEON);
        dt = dun_types_lookup(g->parm2);
        /* closer dungeons get weighted more heavily */
        t += (100 - d*d) * dt->min_dun_lvl;
        w += 100 - d*d; 
    }

    /* weigh in nearby towns */
    for (i = 0; i < point_vec_length(_context->towns); i++)
    {
        point_t p = point_vec_get(_context->towns, i);
        dun_grid_ptr g = dun_grid_at(_context->dun, p);
        town_ptr town;
        int d = point_fast_distance(pos, p);

        if (d > 9) continue;
        assert(g->flags & CELL_TOWN);
        town = towns_lookup(g->parm2);
        /* closer towns get weighted more heavily */
        t += (100 - d*d) * town->level;
        w += 100 - d*d;
    }

    if (!w) /* XXX nothing close by */
    {
        grid->parm2 = _terrain_difficulty(grid);
    }
    else
    {
        /* weigh in current terrain (heavily) */
        t += 2 * w * _terrain_difficulty(grid);
        w += 2 * w;

        grid->parm2 = MAX(0, MIN(90, t / w));
    }

    /* roads are "safe" */
    if (grid->flags & CELL_ROAD)
        grid->parm2 = (grid->parm2 + 3)/4;
}
static void _difficulty(void)
{
    dun_iter_grids(_context->dun, _grid_difficulty);
}
static dun_ptr _random_world_map(point_t size)
{
    dun_ptr result = NULL;
    int try = 20;

    _context = _context_alloc();

    _terrain(size);
    while (--try && !_context->fail)
    {
        if (!_context->fail) _towns();
        if (!_context->fail) _roads();  /* roads connect the towns */
        if (!_context->fail) _rivers(); /* rivers wash out roads and avoid town squares */
        if (!_context->fail) _dungeons(); /* dungeons avoid towns, roads and rivers */
        if (!_context->fail) _difficulty();

        if (!_context->fail)
        {
            result = _context->dun;
            break;
        }
        _context_reset(_context);
    }
    if (!result && _context->dun)
        dun_free(_context->dun);
    _context_free(_context);
    return result;
}
/* XXX Tip on failures. Keep tries low in _random_world_map, but high in dun_world_gen_map.
 * It often happens that a bad terrain will make decent road/river generation impossible,
 * so, for example, getting a 1000+ run of failures before trying a new terrain is quite
 * common (and noticeably slow, of course :) */

/************************************************************************
 * Create the D_WORLD map
 ************************************************************************/
dun_ptr dun_world_gen_map(dun_world_ptr world)
{
    int try = 1000;
    assert(world);
    if (world->file)
    {
        dun_mgr_ptr dm = dun_mgr();
        if (dm->world_frac)
        {
            dun_frac_free(dm->world_frac);
            dm->world_frac = NULL;
        }
        return _read_world_map(world);
    }
    /* XXX consider adding a hook to dun_world_s. */
    while (try--)
    {
        dun_ptr dun = _random_world_map(size_create(50, 20));
        if (dun) return dun;
    }
    return NULL;
}
void dun_gen_world_wizard(void)
{
    #if FEAT_BUGZ
    rect_t map_rect = ui_map_rect();
    point_t size = size_create(map_rect.cx, map_rect.cy);
    point_t min = rect_top_left(map_rect);
    point_t center = rect_center(map_rect);
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr dun = _random_world_map(size);
    bool done = FALSE;

    while(!done && dun)
    {
        point_t p;
        int cmd;

        msg_line_clear();
        Term_clear_rect(map_rect);
        for (p.y = 0; p.y < size.y; p.y++)
        {
            for (p.x = 0; p.x < size.x; p.x++)
            {
                dun_feat_ptr feat = dun_feat_at(dun, p);
                point_t      uip = point_add(min, p);
                Term_putch(uip.x, uip.y, feat->x_attr[F_LIT_STANDARD], feat->x_char[F_LIT_STANDARD]);
            }
        }
        Term_gotoxy(center.x, center.y);
        cmd = inkey_special(FALSE);
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q' || cmd == '\r') break;
        switch (cmd)
        {
        case ESCAPE: case 'q': case '\r':
            done = TRUE;
            break;
        case 'r':
            int_map_delete(dm->dungeons, dun->id);
            dun = _random_world_map(size);
            break;
        }
    }
    plr->redraw |= PR_MAP;
    if (dun) int_map_delete(dm->dungeons, dun->id);
    #endif
}
