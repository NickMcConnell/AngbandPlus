#include "angband.h"

#include "dun.h"
#include "dun_gen.h"
#include "dun_util.h"
#include <assert.h>

static dun_ptr _generating;

static int _scale(int amt, int pct)
{
    int num = amt * pct / 10; /* scaled by 10 */
    int result = num / 10;
    if (randint0(10) < num%10) /* round up */
        result++;
    return result;
}
static rect_t _get_dun_rect(dun_type_ptr dun_type, int dun_lvl)
{
    if (dun_type->size_f)
        return dun_type->size_f(dun_type);
    if (one_in_(7))
    {
        int cx, cy;
        int aspect = rand_range(25, 50);
        if (one_in_(2) && randint0(100) < dun_lvl)
            cx = rand_range(240, 300);
        else
            cx = rand_range(80, 160);
        cy = cx * aspect / 100;
        return rect_create(0, 0, cx, cy);
    }
    return rect_create(0, 0, 200, 70);
}
/* helper to generate a normal level using the old generation code. */
static dun_ptr dun_gen_aux(int dun_type_id, int dun_lvl)
{
    dun_ptr dun;
    dun_type_ptr dun_type;

    assert(dun_type_id != D_SURFACE);
    assert(dun_type_id != D_QUEST);
    dun_type = dun_types_lookup(dun_type_id);

    /* allocate */
    dun = dun_mgr_alloc_dun(_get_dun_rect(dun_type, dun_lvl));
    dun->type = dun_type;
    dun->dun_lvl = dun_lvl;
    dun->difficulty = dun_lvl;
    dun->flags |= (dun_type->flags.info & DF_RESTRICT_MASK);
    _generating = dun; /* needed for stair and player placement */

    /* generate */
    {
        dun_ptr old_cave = cave;
        cave = dun;
        dun_gen(dun);
        cave = old_cave;
    }

    return dun; /* dun is not DF_GENERATED yet ... still needs stairs */
}
/* XXX we are generating stairs and placing the player *outside* of normal
 * generation code, so temp cave flags cannot be used (e.g. CAVE_FLOOR).
 * Also, avoid using the "cave" global pointer by accident. */
static int _stair_wall_ct;
static int _stair_pos(point_t pos, dun_grid_ptr grid)
{
    int wall_ct = 0, i;

    if (grid->type != FEAT_FLOOR) return 0;
    if (grid->flags & CELL_ROOM) return 0;
    if (dun_obj_at(_generating, pos)) return 0;
    if (dun_mon_at(_generating, pos)) return 0;

    for (i = 0; i < 4; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g2 = dun_grid_at(_generating, p);
        if (g2->type == FEAT_WALL) wall_ct++;
    }
    if (wall_ct < _stair_wall_ct) return 0;
    return 1;
}
static int _stair_pos_panic(point_t pos, dun_grid_ptr grid)
{
    if (!cell_allow_mon(grid, NULL)) return 0; /* old FF_PLACE */
    return 1;
}
static point_t _place_stairs_aux(dun_ptr dun, dun_place_f f, int wall_ct)
{
    point_t pos = {0};

    assert(_generating == dun);

    _stair_wall_ct = wall_ct;
    while (_stair_wall_ct >= 0)
    {
        pos = dun_random_grid(dun, _stair_pos);
        if (dun_pos_interior(dun, pos)) break;
        --_stair_wall_ct;
    }
    if (!dun_pos_interior(dun, pos))
    {
        mon_ptr mon;
        pos = dun_random_grid(dun, _stair_pos_panic);
        mon = dun_mon_at(dun, pos);
        if (mon) dun_delete_mon(dun, mon->id);
        dun_destroy_obj_at(dun, pos);
    }
    if (!dun_pos_interior(dun, pos)) return pos;
    f(dun, pos);
    return pos;
}
static dun_stairs_ptr _alloc_stairs(void)
{
    dun_stairs_ptr stairs = malloc(sizeof(dun_stairs_t));
    memset(stairs, 0, sizeof(dun_stairs_t));
    return stairs;
}
static void _place_stairs(dun_ptr dun, dun_place_f f, int ct, int wall_ct)
{
    int dun_type_id = dun->type->id;
    int dun_lvl = dun->dun_lvl;
    int i;

    assert(_generating == dun);

    if (f == dun_place_upstairs)
    {
        dun_lvl = dun->dun_lvl - 1;
        if (dun_lvl < dun->type->min_dun_lvl)
        {
            dun_type_id = D_SURFACE;
            dun_lvl = 0;
            ct = 1;
        }
    }
    else
    {
        assert(f == dun_place_downstairs);
        dun_lvl = dun->dun_lvl + 1;
        if (dun_lvl > dun->type->max_dun_lvl) return;
        if (dun_lvl == dun->type->max_dun_lvl) ct = 1;
    }

    for (i = 0; i < ct; i++)
    {
        point_t pos = _place_stairs_aux(dun, f, wall_ct);
        dun_stairs_ptr stairs;
        if (!dun_pos_interior(dun, pos)) continue;
        stairs = _alloc_stairs();
        stairs->pos_here = pos;
        stairs->dun_type_id = dun_type_id;
        stairs->dun_lvl = dun_lvl;
        dun_add_stairs(dun, stairs);
    }
}
static int _plr_pos(point_t pos, dun_grid_ptr grid)
{
    if (dun_mon_at(_generating, pos)) return 0;
    if (!cell_allow_plr(grid)) return 0;
    if (grid->flags & CELL_VAULT) return 0;

    if (floor_has_trap(grid)) return 0;
    switch (grid->type)
    {
    case FEAT_FLOOR: return 100;
    case FEAT_STAIRS:
    case FEAT_TREE:
    case FEAT_WATER:
    case FEAT_LAVA:
    case FEAT_CHASM: return 1;
    }

    return 0;
}
static int _plr_pos_panic(point_t pos, dun_grid_ptr grid)
{
    if (!cell_allow_plr(grid)) return 0;
    switch (grid->type)
    {
    case FEAT_FLOOR: return 100;
    case FEAT_STAIRS:
    case FEAT_TREE:
    case FEAT_WATER:
    case FEAT_LAVA:
    case FEAT_CHASM: return 1;
    }
    return 0;
}
point_t dun_random_plr_pos(dun_ptr dun)
{
    point_t pos;
    _generating = dun; /* XXX */
    pos = dun_random_grid(dun, _plr_pos);
    if (!dun_pos_interior(dun, pos))
    {
        mon_ptr mon;
        pos = dun_random_grid(dun, _plr_pos_panic);
        mon = dun_mon_at(dun, pos);
        if (mon) dun_delete_mon(dun, mon->id);
    }
    _generating = NULL;
    return pos;
}
static bool _mon_can_enter(dun_ptr dun, point_t pos, mon_race_ptr race)
{
    if (dun_plr_at(dun, pos)) return FALSE;
    if (dun_mon_at(dun, pos)) return FALSE;
    return cell_allow_mon_race(dun_grid_at(dun, pos), race);
}
point_t dun_random_mon_pos(dun_ptr dun, mon_race_ptr race)
{
    rect_t rect = rect_interior(dun->rect);
    int    i;

    for (i = 0; i < 1000; i++)
    {
        point_t p =  rect_random_point(rect);
        dun_grid_ptr grid = dun_grid_at(dun, p);

        if (!_mon_can_enter(dun, p, race)) continue;
        if (grid->flags & CELL_VAULT) continue;
        if (plr->dun_id == dun->id && plr_distance(p) < 10) continue;

        return p;
    }
    return point_create(-1, -1);
}
dun_ptr dun_gen_wizard(int dun_type_id, int dun_lvl)
{
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr dun;
    dun_type_ptr type;
    point_t pos;

    if (dm->prof)
        z_timer_resume(&dm->prof->gen_timer);

    /* generate*/
    dun = dun_gen_aux(dun_type_id, dun_lvl);
    type = dun_types_lookup(dun_type_id);

    /* place stairs */
    if (!quests_find_quest(dun_type_id, dun_lvl))
        _place_stairs(dun, dun_place_downstairs, 2, 3);
    if (dun_lvl == type->min_dun_lvl)
        _place_stairs(dun, dun_place_upstairs, 1, 3);
    else
        _place_stairs(dun, dun_place_upstairs, _1d(2), 3);
    dun->flags |= DF_GENERATED;
    _generating = NULL;
    if (dm->prof)
        z_timer_pause(&dm->prof->gen_timer);

    /* place player */
    pos = dun_random_plr_pos(dun);
    dun_mgr_plr_change_dun(dun, pos);
    return dun;
}
static void _place_perm(point_t pos, dun_grid_ptr grid)
{ 
    dun_place_mountain_wall(cave, pos);
}
dun_ptr dun_gen_quest(quest_ptr quest)
{
    dun_ptr dun, old_cave = cave;
    room_ptr room = quest_get_map(quest);
    /* XXX rotation */
    transform_ptr xform = transform_alloc_room(room, size_create(room->width, room->height));

    /* allocate */
    dun = dun_mgr_alloc_dun(xform->dest);
    dun->type = dun_types_lookup(D_QUEST);
    dun->dun_lvl = quest->level;
    dun->difficulty = quest->level;
    dun->quest_id = quest->id;

    /* generate: note that build_room_template_aux will place the plr */
    cave = dun;
    dun_iter_grids(dun, _place_perm);
    mon_alloc_push_filter(mon_alloc_dungeon);
    plr->new_pos = point_create(0, 0); /* quest maps *should* locate the player */
    build_room_template_aux(room, xform);
    mon_alloc_pop_filter();
    cave = old_cave;

    transform_free(xform);
    room_free(room);
    dun->flags |= (dun_types_lookup(D_QUEST)->flags.info & DF_RESTRICT_MASK);
    dun->flags |= DF_GENERATED;

    return dun;
}
static bool _quest_stairs(point_t pos, dun_grid_ptr grid) { return stairs_go_up(grid); }
static point_t _find_quest_stairs(dun_ptr dun) { return dun_find_grid(dun, _quest_stairs); }
dun_ptr dun_gen_connected(dun_ptr return_dun, dun_stairs_ptr return_stairs)
{
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr dun;

    if (dm->prof)
        z_timer_resume(&dm->prof->gen_timer);
    if (return_stairs->dun_type_id == D_SURFACE)
    {
        /* For returning to the surface, we disregard the recall return
         * pos (plr->old_pos). Instead, we need to find and use the 
         * world grid for this particular dungeon. */
        dun_type_ptr   type = dun_types_lookup(return_dun->type->id);
        point_t        world_pos = type->world_pos;
        dun_stairs_ptr stairs;

        dun = dun_gen_surface(world_pos);
        return_stairs->dun_id = dun->id;

        /* update existing surface stairs (cf _gen_dungeon in dun_world.c)
         * But consider that D_SURFACE has 9 world grids and there might
         * be mutliple entrance grids for the current world pos. */
        for (stairs = dun->stairs; stairs; stairs = stairs->next)
        {
            if (stairs->dun_type_id == return_dun->type->id)
                break;
        }
        assert(stairs);
        stairs->dun_lvl = return_dun->dun_lvl; /* in case of shafts (XXX) */
        stairs->dun_id = return_dun->id;
        stairs->pos_there = return_stairs->pos_here;
        return_stairs->pos_there = stairs->pos_here;
        dun->flow_pos = stairs->pos_here;
        dun->flags |= DF_UPDATE_FLOW;
        assert(dun->flags & DF_GENERATED);
    }
    else if (return_stairs->dun_type_id == D_QUEST)
    {
        /* quest levels are generated just-in-time by dun_take_stairs_plr */
        int            quest_id = return_stairs->dun_lvl; /* XXX _process_town_map */
        quest_ptr      quest = quests_get(quest_id);
        point_t        pos;
        dun_stairs_ptr stairs;

        dun = dun_gen_quest(quest);
        pos = _find_quest_stairs(dun);
        assert(dun_pos_interior(dun, pos));

        return_stairs->dun_id = dun->id;
        return_stairs->pos_there = pos;

        stairs = _alloc_stairs();
        stairs->pos_here = pos;
        stairs->pos_there = return_stairs->pos_here;
        stairs->dun_id = return_dun->id;
        stairs->dun_type_id = return_dun->type->id;
        stairs->dun_lvl = return_dun->dun_lvl;
        dun_add_stairs(dun, stairs);
        dun->flow_pos = stairs->pos_here;
        dun->flags |= DF_UPDATE_FLOW;
        assert(dun->flags & DF_GENERATED);
    }
    else
    {
        dun_place_f f = dun_place_downstairs;
        dun_stairs_ptr stairs;

        if (return_stairs->dun_lvl > return_dun->dun_lvl)
            f = dun_place_upstairs;

        dun = dun_gen_aux(return_stairs->dun_type_id, return_stairs->dun_lvl);
        return_stairs->dun_id = dun->id;

        /* allocate new connected stairs */
        return_stairs->pos_there = _place_stairs_aux(dun, f, 3);
        stairs = _alloc_stairs();
        stairs->pos_here = return_stairs->pos_there;
        stairs->pos_there = return_stairs->pos_here;
        stairs->dun_id = return_dun->id;
        stairs->dun_type_id = return_dun->type->id;
        stairs->dun_lvl = return_dun->dun_lvl;
        dun_add_stairs(dun, stairs);
        dun->flow_pos = stairs->pos_here;

        if (!(dun->type->flags.info & DF_RANDOM))
        {
            /* random down stairs */
            if (!quests_find_quest(dun->type->id, dun->dun_lvl))
                _place_stairs(dun, dun_place_downstairs, 2, 3);

            /* random up stairs */
            if (return_dun->type->id != D_SURFACE)
                _place_stairs(dun, dun_place_upstairs, 1, 3);
        }
        dun->flags |= DF_UPDATE_FLOW | DF_GENERATED;
        _generating = NULL;
    }
    if (dm->prof)
        z_timer_pause(&dm->prof->gen_timer);

    return dun;
}

/************************************************************************
 * Dungeon Generation
 ************************************************************************/
typedef void (*dun_gen_debug_f)(dun_gen_ptr gen, cptr msg);
#if 0
static FILE  *_debug_fp = NULL;
static bool _is_free(dun_gen_ptr gen, point_t pos)
{
    dun_space_ptr space;
    for (space = gen->space; space; space = space->next)
        if (rect_contains_point(space->rect, pos)) return TRUE;
    return FALSE;
}
static void dun_gen_debug_file(dun_gen_ptr gen, cptr msg)
{
    if (!_debug_fp)
    {
        char buf[1024];
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "gen.txt");
        _debug_fp = my_fopen(buf, "w");
    }
    fprintf(_debug_fp, "%s\n", msg);
    if (gen)
    {
        point_t min = rect_top_left(gen->dun->rect);
        point_t max = rect_bottom_right(gen->dun->rect);
        point_t p;
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                dun_grid_ptr grid = dun_grid_at(gen->dun, p);
                dun_feat_ptr feat = dun_grid_feat(grid);
                char         display = feat->d_char[F_LIT_STANDARD];

                if (grid->feat == feat_tree) display = 'T';
                if (display == '#')
                {
                    if (grid->info & CAVE_SOLID) display = 'S';
                    else if (grid->info & CAVE_OUTER) display = 'O';
                    else if (grid->info & CAVE_INNER) display = 'I';
                    else if (_is_free(gen, p)) display = 'F';
                }
                if (grid->info & CAVE_TEMP)
                    display = '&';
                fputc(display, _debug_fp); 
            }
            fputc('\n', _debug_fp);
        }
    }
    else
    {
        my_fclose(_debug_fp);
        _debug_fp = NULL;
    }
}
static dun_gen_debug_f _debug_f = dun_gen_debug_file;
#else
static dun_gen_debug_f _debug_f = NULL;
#endif
static dun_space_ptr dun_space_alloc(rect_t r)
{
    dun_space_ptr space = malloc(sizeof(dun_space_t));
    space->next = NULL;
    space->prev = NULL;
    space->rect = r;
    return space;
}
dun_gen_ptr dun_gen_alloc(dun_ptr dun)
{
    dun_gen_ptr gen = malloc(sizeof(dun_gen_t));
    memset(gen, 0, sizeof(dun_gen_t));

    gen->dun = dun;
    gen->scale_pct = 100 * rect_area(dun->rect) / (198*66); /* pct of traditional dungeon size */
    gen->rooms = point_vec_alloc();
    gen->floors = point_vec_alloc();
    gen->walls = point_vec_alloc();
    gen->doors = point_vec_alloc();
    gen->space = dun_space_alloc(rect_interior(dun->rect));

    /* tunneler parameters */
    gen->rand_dir_pct = rand_range(5, 20);
    gen->change_dir_pct = rand_range(20, 60);
    gen->quit_pct = rand_range(40, 60);
    gen->door_pierce_pct = rand_range(30, 70);
    gen->door_intersect_pct = rand_range(60, 90);

    return gen;
}
void dun_gen_free(dun_gen_ptr gen)
{
    dun_space_ptr next, space;
    if (!gen) return;

    for (space = gen->space; space; space = next)
    {
        next = space->next;
        free(space);
    }

    point_vec_free(gen->doors);
    point_vec_free(gen->walls);
    point_vec_free(gen->floors);
    point_vec_free(gen->rooms);

    free(gen);
}

/* we use a module level variable for the current generation to make
 * life easier for function hooks (like dun_iter_grids and the like). */
static dun_gen_ptr _dun_gen = NULL;

static void _solid_perm_wall(point_t pos, dun_grid_ptr grid);
static void _extra_wall(point_t pos, dun_grid_ptr grid);
static void _set_unsafe(point_t pos, dun_grid_ptr grid) { grid->flags |= CELL_UNSAFE; }
void dun_gen(dun_ptr dun)
{
    bool success = FALSE;
    while (!success)
    {
        assert(!_dun_gen);
        _dun_gen = dun_gen_alloc(dun);

        dun_clear(dun);
        dun_iter_grids(dun, _extra_wall);
        if (dun_gen_rooms(_dun_gen) && dun_gen_tunnels(_dun_gen))
        {
            dun_gen_streamers(_dun_gen);
            dun_iter_boundary(dun, _solid_perm_wall);

            dun_gen_monsters(_dun_gen);
            dun_gen_traps(_dun_gen);
            if (!(dun->type->flags.gen & DF_GEN_NO_CAVE))
                dun_gen_rubble(_dun_gen);
            dun_gen_objects(_dun_gen);
            if (dun->type->post_gen_f)
                dun->type->post_gen_f(dun->type, _dun_gen);

            /*dun_iter_grids(dun, _wipe_mask); XXX not needed yet */
            if (cave->type->id != D_SURFACE) dun_iter_grids(dun, _set_unsafe);
            success = TRUE;
        }
        else
        {
            if (dun->flags & DF_SHOP)
            {
                dun->flags &= ~DF_SHOP;
                town_free(dun->town);
                dun->town = NULL;
            }
            else
                dun->type->last_shop--; 
            if (dun->flags & DF_RECALL)
                dun->flags &= ~DF_RECALL;
            else
                dun->type->last_recall--; 
            msg_format("<color:r>Generation Restarted: <color:y>%s</color>.</color>", _dun_gen->error);
            if (_debug_f)
                _debug_f(_dun_gen, format("Generation Restarted: %s", _dun_gen->error));
        }
        dun_gen_free(_dun_gen);
        _dun_gen = NULL;
    }
    if (_debug_f)
        _debug_f(NULL, "Generation completed");
}
/************************************************************************
 * Space Allocation: New
 *
 * We use a free list of available rectangular regions. This is more prone
 * to fragmentation issues than the old 11x11 block based allocator, but
 * it handles arbitrarily sized dungeons and appears to work well in practice.
 * Rooms are allocated in order of priority, so you don't miss out on vaults.
 * Of course, ROOM_RECALL and ROOM_SHOP rooms are always generated first,
 * so you don't miss those either. The real space hogs are the cave rooms, 
 * but I prefer large caverns.
 ************************************************************************/
static rect_t _random_block(rect_t rect, point_t size)
{
    int blocks_x = rect.cx / size.x;
    int blocks_y = rect.cy / size.y;
    int which_x = randint0(blocks_x);
    int which_y = randint0(blocks_y);
    point_t v = point_create(which_x * size.x, which_y * size.y);
    point_t tl;
    rect_t block;

    assert(blocks_x >= 1);
    assert(blocks_y >= 1);
    if (blocks_x == 1) v.x += randint0(rect.cx - size.x);
    if (blocks_y == 1) v.y += randint0(rect.cy - size.y);
    tl = point_add(rect_top_left(rect), v);
    block = rect_create(tl.x, tl.y, size.x, size.y);
    assert(rect_contains(rect, block));
    return block;
}
/* Remove the largest possible free space rect from master minus block. It is
 * important to always do this optimally to reduce fragmentation issues. */
static bool _remove_largest(dun_gen_ptr gen, rect_ptr master, rect_t block)
{
    struct { rect_t child, master; } rects[4];
    int i, best = -1, best_area = -1;

    /* left block */
    rects[0].child = rect_create(master->x, master->y, block.x - master->x, master->cy);
    rects[0].master = rect_create(block.x, master->y, master->cx - rects[0].child.cx, master->cy);

    /* right block */
    rects[1].child = rect_create(block.x + block.cx, master->y, rect_right(*master) - (block.x + block.cx), master->cy);
    rects[1].master = rect_create(master->x, master->y, rects[1].child.x - master->x, master->cy);

    /* top block */
    rects[2].child = rect_create(master->x, master->y, master->cx, block.y - master->y);
    rects[2].master = rect_create(master->x, block.y, master->cx, master->cy - rects[2].child.cy);

    /* bottom block */
    rects[3].child = rect_create(master->x, block.y + block.cy, master->cx, rect_bottom(*master) - (block.y + block.cy));
    rects[3].master = rect_create(master->x, master->y, master->cx, rects[3].child.y - master->y);

    /* find the best */
    for (i = 0; i < 4; i++)
    {
        int area;
        if (!rect_is_valid(rects[i].child)) continue;

        assert(rect_is_valid(rects[i].master));
        assert(rect_contains(gen->dun->rect, rects[i].child));
        assert(rect_contains(gen->dun->rect, rects[i].master));
        assert(!rect_is_valid(rect_intersect(rects[i].master, rects[i].child)));
        assert(rect_contains(*master, rects[i].child));
        assert(rect_contains(*master, rects[i].master));

        area = rect_area(rects[i].child);
        if (area > best_area)
        {
            best = i;
            best_area = area;
        }
    }
    if (best < 0) return FALSE;
    if (rects[best].child.cx > 10 && rects[best].child.cy > 10)
    {
        /* link in the child */
        dun_space_ptr space = dun_space_alloc(rects[best].child);
        space->next = gen->space;
        if (gen->space) gen->space->prev = space;
        gen->space = space;

        /* shrink master for the next call */
        *master = rects[best].master;
        return TRUE;
    }
    return FALSE;
}
static void _reserve_aux(dun_gen_ptr gen, dun_space_ptr space, rect_t block)
{
    rect_t master = space->rect;

    assert(rect_contains(space->rect, block));

    /* unlink */
    if (!space->prev)
    {
        gen->space = space->next;
        if (space->next) space->next->prev = NULL;
    }
    else
    {
        space->prev->next = space->next;
        if (space->next) space->next->prev = space->prev;
    }
    free(space);

    /* link in leftovers */
    while (_remove_largest(gen, &master, block)) /* loop */;
}
static rect_t _reserve(dun_gen_ptr gen, dun_space_ptr space, point_t size)
{
    /* reserve a region */
    rect_t block = _random_block(space->rect, size);
    rect_t reserved = rect_create(0, 0, size.x, size.y);
    reserved = rect_recenter(reserved, rect_center(block));
    assert(rect_contains(space->rect, reserved));
    assert(rect_contains(block, reserved));

    /* update free space */
    _reserve_aux(gen, space, block);
    return reserved;
}
static bool _tower_out_of_bounds(dun_gen_ptr gen, point_t p);
rect_t dun_gen_reserve(dun_gen_ptr gen, point_t size)
{
    rect_t        reserved = rect_invalid();
    dun_space_ptr space;
    int           tot = 0, pick;

    assert(size.x > 0 && size.y > 0);
    /* pad a bit to prevent rooms touching one another */
    size.x += 4;
    size.y += 2;

    for (space = gen->space; space; space = space->next)
    {
        if (space->rect.cx < size.x) continue;
        if (space->rect.cy < size.y) continue;
        tot += rect_area(space->rect);
    }
    if (!tot) return reserved;
    pick = randint0(tot);
    for (space = gen->space; space; space = space->next)
    {
        if (space->rect.cx < size.x) continue;
        if (space->rect.cy < size.y) continue;
        pick -= rect_area(space->rect);
        if (pick < 0)
        {
            reserved = _reserve(gen, space, size); /* space has been deleted! */
            break;
        }
    }
    assert(rect_is_valid(reserved));
    assert(reserved.cx >= size.x);
    assert(reserved.cy >= size.y);
    assert(rect_contains(gen->dun->rect, reserved));
    if (_tower_out_of_bounds(gen, rect_center(reserved))) /* D_ISENGARD, D_DARK_TOWER enforce circular clipping */
        return rect_invalid();
    reserved = rect_deflate(reserved, 2, 1); /* undo the padding */
    point_vec_push(gen->rooms, rect_center(reserved));
    return reserved;
}
bool dun_gen_reserve_rect(dun_gen_ptr gen, rect_t rect)
{
    dun_space_ptr space;

    for (space = gen->space; space; space = space->next)
    {
        if (rect_contains(space->rect, rect))
        {
            _reserve_aux(gen, space, rect);
            return TRUE;
        }
    }
    return FALSE;
}

/************************************************************************
 * Walls and Floors
 * Inner and Outer Walls are room walls. 
 * Extra Walls are walls outside of any room.
 * Solid Walls cannot be pierced by the tunneler.
 ************************************************************************/
static dun_grid_ptr _grid_at(point_t pos) { return dun_grid_at(_dun_gen->dun, pos); }
static dun_type_ptr _type(void) { return _dun_gen->dun->type; }
static void _extra_wall(point_t pos, dun_grid_ptr grid)
{
    _type()->place_wall(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
    grid->flags |= CELL_EXTRA;
}
static void _outer_wall(point_t pos, dun_grid_ptr grid)
{
    _type()->place_outer_wall(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
    grid->flags |= CELL_OUTER | CELL_ROOM;
}
static void _lit_outer_wall(point_t pos, dun_grid_ptr grid)
{
    _outer_wall(pos, grid);
    grid->flags |= CELL_LIT;
}
static void _inner_wall(point_t pos, dun_grid_ptr grid)
{
    _type()->place_inner_wall(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
    grid->flags |= CELL_INNER | CELL_ROOM;
}
static void _inner_wall_at(point_t pos) { _inner_wall(pos, _grid_at(pos)); }
static void _inner_wall_at_xy(int x, int y) { _inner_wall_at(point_create(x, y)); }
static void _solid_noperm_wall(point_t pos, dun_grid_ptr grid)
{
    _type()->place_outer_wall(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
    grid->flags |= CELL_SOLID;
}
static void _solid_perm_wall(point_t pos, dun_grid_ptr grid)
{
    dun_place_mountain_wall(_dun_gen->dun, pos);
    assert(grid->flags & CELL_PERM);
    grid->flags |= CELL_SOLID;
}
static void _solid_wall(point_t pos, dun_grid_ptr grid)
{
    _type()->place_outer_wall(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
    grid->flags |= CELL_SOLID;
}
static void _solid_wall_at(point_t pos) { _solid_wall(pos, _grid_at(pos)); }
static void _solid_wall_at_xy(int x, int y) { _solid_wall_at(point_create(x, y)); }

static void _floor(point_t pos, dun_grid_ptr grid)
{
    _type()->place_floor(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
}
static void _room(point_t pos, dun_grid_ptr grid)
{
    _floor(pos, grid);
    grid->flags |= CELL_ROOM;
}
static void _lit_room(point_t pos, dun_grid_ptr grid)
{
    _room(pos, grid);
    grid->flags |= CELL_LIT;
}
/************************************************************************
 * Doors
 ************************************************************************/
static void _random_door(point_t pos, dun_grid_ptr grid)
{
    dun_place_random_door(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
}
/************************************************************************
 * Monsters, Traps and Objects
 ************************************************************************/
static int _lvl_amount(int lvl) { return MAX(2, MIN(10, lvl/3)); }
void dun_gen_monsters(dun_gen_ptr gen)
{
    /* Note: These are extra monsters, filling out NORMAL and FRACAVE rooms.
     * Most TEMPLATE rooms and all VAULT rooms will generate their own monsters
     * during room generation. */
    int base = 14; /* XXX add to gen->type */
    int xtra = _lvl_amount(gen->dun->difficulty);
    int num = MAX(5, _scale(base + _1d(xtra), gen->scale_pct));
    int i;
    assert(cave == gen->dun);
    for (i = 0; i < num; i++)
        alloc_monster(0, PM_ALLOW_SLEEP);
}
static point_t _random_drop_pos(dun_ptr dun, dun_grid_p filter)
{
    rect_t  rect = rect_interior(dun->rect);
    int attempt = 5000;
    while (--attempt)
    {
        point_t      pos = rect_random_point(rect);
        dun_grid_ptr grid = dun_grid_at(dun, pos);

        if (grid->type != FEAT_FLOOR) continue;
        if (!cell_allow_obj(grid)) continue;
        if (dun_obj_at(dun, pos)) continue;
        if (dun_mon_at(dun, pos)) continue;

        /* Don't rubble out shop entrances or wizard tiles. Seems to happen quite a bit! */
        if (grid->flags & CELL_PERM) continue;

        if (filter && !filter(pos, grid)) continue;
        return pos;
    }
    return point_create(0, 0);
}
static bool _room_p(point_t pos, dun_grid_ptr grid) { return BOOL(grid->flags & CELL_ROOM); }
static bool _not_room_p(point_t pos, dun_grid_ptr grid) { return !(grid->flags & CELL_ROOM); }
static void _trap(point_t pos, dun_grid_ptr grid)
{
    dun_place_trap(_dun_gen->dun, pos);
}
static void _rubble(point_t pos, dun_grid_ptr grid)
{
    dun_place_rubble(_dun_gen->dun, pos);
}
static void _object(point_t pos, dun_grid_ptr grid)
{
    obj_t forge = {0};
    int   lvl = cave->difficulty;
    /* Comment: Monsters drop objects at (ML + DL)/2. In practice,
       this means that your best drops are just laying on the ground,
       and this encourages recall scumming for end game resources such
       as wands of rockets. Note: Vaults are not affected and we want
       to encourage these! Room templates need some thought ... */
    if (lvl > 31)
    {
       int n = lvl - 30;
       lvl = 30 + n/2 + randint1(n/2);
    }
    if (!dun_allow_drop_at(_dun_gen->dun, pos)) return;
    if (!make_object(&forge, lvl, 0)) return;
    dun_place_obj(_dun_gen->dun, &forge, pos);
}
static void _gold(point_t pos, dun_grid_ptr grid)
{
    obj_t forge = {0};
    if (!dun_allow_drop_at(_dun_gen->dun, pos)) return;
    if (!make_gold(&forge, 0)) return; /* XXX uses cave */
    dun_place_obj(_dun_gen->dun, &forge, pos);
}
static void _food(point_t pos, dun_grid_ptr grid)
{
    obj_t forge = {0};
    int   k_idx;
    if (!dun_allow_drop_at(_dun_gen->dun, pos)) return;
    if (prace_is_(RACE_ENT))
        k_idx = lookup_kind(TV_POTION, SV_POTION_WATER);
    else
        k_idx = lookup_kind(TV_FOOD, SV_FOOD_RATION);
    object_prep(&forge, k_idx);
    obj_make_pile(&forge);
    dun_place_obj(_dun_gen->dun, &forge, pos);
}
static void _light(point_t pos, dun_grid_ptr grid)
{
    obj_t forge = {0};
    int   k_idx;
    if (!dun_allow_drop_at(_dun_gen->dun, pos)) return;
    if (one_in_(3))
        k_idx = lookup_kind(TV_FLASK, SV_FLASK_OIL);
    else
        k_idx = lookup_kind(TV_LIGHT, SV_LIGHT_LANTERN);
    object_prep(&forge, k_idx);
    apply_magic(&forge, _dun_gen->dun->difficulty, 0);
    obj_make_pile(&forge);
    dun_place_obj(_dun_gen->dun, &forge, pos);
}
static void _skeleton(point_t pos, dun_grid_ptr grid)
{
    obj_t forge = {0};
    int   k_idx = lookup_kind(TV_CORPSE, SV_SKELETON);
    if (!dun_allow_drop_at(_dun_gen->dun, pos)) return;
    object_prep(&forge, k_idx);
    apply_magic(&forge, _dun_gen->dun->difficulty, 0);
    if (!forge.race_id)
        return; /* e.g. Graveyard ... unable to reproduce why no race was chosen (a_m_aux_4) XXX */
    dun_place_obj(_dun_gen->dun, &forge, pos);
}
static void _gen_stuff(dun_gen_ptr gen, int num, dun_grid_p filter, dun_grid_f action)
{
    int i;
    num = _scale(num, gen->scale_pct);
    for (i = 0; i < num; i++)
    {
        point_t pos = _random_drop_pos(gen->dun, filter);
        if (!dun_pos_interior(gen->dun, pos)) break;
        action(pos, _grid_at(pos));
    }
}
static void _gen_misc(dun_gen_ptr gen, dun_grid_p filter, dun_grid_f action)
{
    int roll = _lvl_amount(gen->dun->difficulty);
    _gen_stuff(gen, _1d(roll), filter, action);
}
void dun_gen_traps(dun_gen_ptr gen) { _gen_misc(gen, NULL, _trap); }
void dun_gen_rubble(dun_gen_ptr gen) { _gen_misc(gen, _not_room_p, _rubble); }
static bool _mon_extra_drop_check(mon_ptr mon)
{
    mon_race_ptr race = mon->race;
    mon_drop_ptr drop;

    /* Look for a monster that might drop an object. It's rather weird
     * to have a Creeping Copper Coins drop a prize object! */
    if (!race->drops) return FALSE;
    for (drop = race->drops; drop; drop = drop->next)
    {
        if ((drop->drop.flags & OBJ_DROP_TYPE) && drop->drop.object == TV_GOLD) continue;
        if (drop->drop.flags & (OBJ_DROP_RANDOM | OBJ_DROP_TYPE)) return TRUE;
    }
    return FALSE;
}
static void _mon_give_extra_drop(u32b flag, int num)
{
    vec_ptr v;
    int_map_iter_ptr iter;

    num = _scale(num, _dun_gen->scale_pct);
    if (num <= 0) return;

    v = vec_alloc(NULL);
    for (iter = int_map_iter_alloc(_dun_gen->dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (!_mon_extra_drop_check(mon)) continue;
        vec_add(v, mon);
    }
    int_map_iter_free(iter);

    if (vec_length(v))
    {
        int i;
        /* XXX Give the drops, but we make no effort to avoid
         * picking the same monster more than once */
        for (i = 0; i < num; i++)
        {
            mon_ptr mon = vec_get(v, randint0(vec_length(v)));
            mon->mflag2 |= flag;
        }
    }
    vec_free(v);
}
void dun_gen_objects(dun_gen_ptr gen)
{
    _gen_stuff(gen, randnor(6, 3), _room_p, _object);
    _gen_stuff(gen, randnor(2, 3), _not_room_p, _object);
    _gen_stuff(gen, randnor(2, 3), NULL, _gold);

    if (one_in_(2))
        _gen_stuff(gen, 1, NULL, _food);

    if (gen->dun->dun_lvl <= 15 && one_in_(2))
        _gen_stuff(gen, 1, NULL, _light);

    if (cave->dun_lvl >= 10 && one_in_(20))
        _gen_stuff(gen, damroll(3, 5), NULL, _skeleton);

    _mon_give_extra_drop(MFLAG2_DROP_BASIC, 1);
    _mon_give_extra_drop(MFLAG2_DROP_UTILITY, randint0(4));
    if (gen->dun->dun_lvl > gen->dun->type->plr_max_lvl)
        _mon_give_extra_drop(MFLAG2_DROP_PRIZE, 1);
}
/************************************************************************
 * Room Builders
 ************************************************************************/
static void _closed_curtain(point_t pos, dun_grid_ptr grid)
{
    dun_place_closed_curtain(_dun_gen->dun, pos);
    grid->flags &= ~CELL_GEN_MASK;
}
bool dun_gen_normal_room(dun_gen_ptr gen)
{
    point_t size = size_create(5 + _1d(11) + _1d(11), 3 + _1d(4) + _1d(3));
    rect_t  rect = dun_gen_reserve(gen, size);
    dun_grid_f floor_f = _room, wall_f = _outer_wall;
    point_t min, max, p;
    bool curtain = (gen->dun->type->flags.gen & DF_GEN_CURTAIN) &&
        one_in_((gen->dun->type->flags.gen & DF_GEN_NO_CAVE) ? 48 : 512);

    if (gen->dun->dun_lvl <= _1d(25))
    {
        floor_f = _lit_room;
        wall_f = _lit_outer_wall;
    }

    if (!rect_is_valid(rect))
    {
        size = size_create(rand_range(8, 12), rand_range(5, 8)); 
        rect = dun_gen_reserve(gen, size);
        if (!rect_is_valid(rect)) return FALSE;
    }

    dun_iter_rect_boundary(gen->dun, rect, wall_f);
    dun_iter_rect_interior(gen->dun, rect, floor_f);

    /* Specialty rooms will work with the interior floor space */
    rect = rect_interior(rect);
    min = rect_top_left(rect);
    max = rect_bottom_right(rect);
    if (curtain && rect.cx > 2 && rect.cy > 2) /* curtained room */
        dun_iter_rect_boundary(gen->dun, rect, _closed_curtain);

    if (one_in_(20)) /* pillar room */
    {
        for (p.y = min.y; p.y <= max.y; p.y += 2)
            for (p.x = min.x; p.x <= max.x; p.x += 2)
                _inner_wall_at(p);
    }
    else if (one_in_(20)) /* four corner pillars */
    {
        if (min.x + 4 < max.x && min.y + 4 < max.y)
        {
            _inner_wall_at_xy(min.x + 1, min.y + 1);
            _inner_wall_at_xy(max.x - 1, min.y + 1);
            _inner_wall_at_xy(min.x + 1, max.y - 1);
            _inner_wall_at_xy(max.x - 1, max.y - 1);
        }
    }
    else if (one_in_(50)) /* ragged-edge room */
    {
        for (p.y = min.y + 2; p.y <= max.y - 2; p.y += 2)
        {
            _inner_wall_at_xy(min.x, p.y);
            _inner_wall_at_xy(max.x, p.y);
        }
        for (p.x = min.x + 2; p.x <= max.x - 2; p.x += 2)
        {
            _inner_wall_at_xy(p.x, min.y);
            _inner_wall_at_xy(p.x, max.y);
        }
    }
    else if (one_in_(50)) /* divided room */
    {
        point_t center = rect_center(rect);
        dun_grid_ptr grid;
        bool ok = FALSE;
        bool curtain2 = (gen->dun->type->flags.gen & DF_GEN_CURTAIN) &&
            one_in_((gen->dun->type->flags.gen & DF_GEN_NO_CAVE) ? 2 : 128);

        if (rect.cy % 2 && _1d(100) < 50) /* Horizontal wall */
        {
            p.y = center.y;
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                grid = _grid_at(p);
                if (curtain2) _closed_curtain(p, grid);
                else _inner_wall(p, grid);
            }
            /* Prevent edge of wall from being tunneled */
            _solid_wall_at_xy(min.x - 1, p.y);
            _solid_wall_at_xy(max.x + 1, p.y);
            ok = TRUE;
        }
        else if (rect.cx % 2) /* Vertical wall */
        {
            p.x = center.x;
            for (p.y = min.y; p.y <= max.y; p.y++)
            {
                grid = _grid_at(p);
                if (curtain2) _closed_curtain(p, grid);
                else _inner_wall(p, grid);
            }
            /* Prevent edge of wall from being tunneled */
            _solid_wall_at_xy(p.x, min.y - 1);
            _solid_wall_at_xy(p.x, max.y + 1);
            ok = TRUE;
        }
        if (ok)
        {
            grid = _grid_at(center);
            if (curtain2) _closed_curtain(p, grid);
            else _random_door(center, grid);
        }
    }
    return TRUE;
}
static rect_t _rect_create_tlbr(point_t tl, point_t br)
{
    return rect_create(tl.x, tl.y, br.x - tl.x + 1, br.y - tl.y + 1);
}
bool dun_gen_overlap_room(dun_gen_ptr gen)
{
    point_t size = size_create(25, 11);
    rect_t  rect = dun_gen_reserve(gen, size), r1f, r1, r2f, r2;
    point_t center, min1, max1, min2, max2;
    dun_grid_f floor_f = _room, wall_f = _outer_wall;

    if (gen->dun->dun_lvl <= _1d(25))
    {
        floor_f = _lit_room;
        wall_f = _lit_outer_wall;
    }

    if (!rect_is_valid(rect)) return FALSE;
    center = rect_center(rect);

    /* Determine floor space of room1 */
    min1.x = center.x - _1d(11);
    min1.y = center.y - _1d(4);
    max1.x = center.x + _1d(10);
    max1.y = center.y + _1d(3);
    r1f = _rect_create_tlbr(min1, max1); /* floor space */
    r1 = rect_inflate(r1f, 1, 1); /* includes walls */

    /* Determine floor space of room2 */
    min2.x = center.x - _1d(10);
    min2.y = center.y - _1d(3);
    max2.x = center.x + _1d(11);
    max2.y = center.y + _1d(4);
    r2f = _rect_create_tlbr(min2, max2);
    r2 = rect_inflate(r2f, 1, 1);

    /* full floors for both roooms */
    dun_iter_rect(gen->dun, r1, floor_f);
    dun_iter_rect(gen->dun, r2, floor_f);

    /* walls */
    dun_iter_rect_boundary(gen->dun, r1, wall_f);
    dun_iter_rect_boundary(gen->dun, r2, wall_f);

    /* replace floors */
    dun_iter_rect(gen->dun, r1f, floor_f);
    dun_iter_rect(gen->dun, r2f, floor_f);

    return TRUE;
}
/************************************************************************
 * Room Templates
 ************************************************************************/
bool dun_gen_template_room(dun_gen_ptr gen, int type, int subtype)
{
    room_ptr      room;
    transform_ptr xform;
    rect_t        rect;

    /* Pick a room template */
    room = choose_room_template(type, subtype);
    if (!room) return FALSE;

    /* pick type of transformation */
    if (room->flags & ROOM_NO_ROTATE)
        xform = transform_alloc(0, rect_create(0, 0, room->width, room->height));
    else
        xform = transform_alloc_room(room, size_create(gen->dun->rect.cx, gen->dun->rect.cy));

    /* Find and reserve some space in the dungeon. */
    rect = dun_gen_reserve(gen, size_create(xform->dest.cx, xform->dest.cy));
    if (!rect_is_valid(rect))
    {
        transform_free(xform);
        return FALSE;
    }

    /* Position the destination rect */
    xform->dest = rect_translate_xy(xform->dest, rect.x, rect.y);

    /* XXX */
    assert(cave == gen->dun);
    build_room_template_aux(room, xform);
    transform_free(xform);
    return TRUE;
}
/************************************************************************
 * Cave Rooms
 ************************************************************************/
static bool _massage(dun_frac_ptr frac)
{
    int area = rect_area(frac->rect);
    int floor = 0, i;
    point_t min = rect_top_left(frac->rect);
    point_t max = rect_bottom_right(frac->rect), p;
    point_t center = rect_center(frac->rect);
    point_queue_ptr q = point_queue_alloc();
    dun_ptr dun = _dun_gen ? _dun_gen->dun : cave; /* XXX dun_gen_cave_wizard */
    dun_bmp_ptr bmp = dun_bmp_alloc(dun, frac->rect);

    assert(dun_frac_get(frac, center) < frac->cutoff);

    point_queue_add(q, center);
    dun_bmp_set(bmp, center);
    while (point_queue_count(q) > 0)
    {
        p = point_queue_remove(q);
        ++floor;
        for (i = 0; i < 8; i++)
        {
            point_t p2 = point_step(p, ddd[i]);
            if (!rect_contains_point(frac->rect, p2)) continue;
            if (dun_bmp_test(bmp, p2)) continue;
            if (dun_frac_get(frac, p2) >= frac->cutoff) continue;
            point_queue_add(q, p2);
            dun_bmp_set(bmp, p2);
        }
    }
    point_queue_free(q);

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            if (!dun_bmp_test(bmp, p))
                dun_frac_set(frac, p, frac->max);
        }
    }
    dun_bmp_free(bmp);
    return 100 * floor / area > 35;
}
static dun_frac_ptr _cave_frac(rect_t rect, int cutoff_pct)
{
    int attempt = 100;
    int max_height = MIN(250, MAX(rect.cx, rect.cy));
    dun_frac_ptr frac = dun_frac_alloc(rect, max_height);
    point_t min = rect_top_left(rect);
    point_t max = rect_bottom_right(rect);
    point_t center = rect_center(rect);
    point_t p;

    /* these fractal algorithms require careful tuning and are rather fragile
     * wrt small parameter changes */
    frac->rough = 2 + max_height * 9 / 100;
    frac->grid = 3 + max_height * 13 / 100;
    frac->cutoff = max_height * cutoff_pct / 100;

    while (--attempt)
    {
        /* seed the fractal generation; setting the boundary above the cutoff ensures
         * we get an enclosed room. */
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            p.x = min.x; dun_frac_set(frac, p, frac->cutoff + _1d(max_height - frac->cutoff));
            p.x = max.x; dun_frac_set(frac, p, frac->cutoff + _1d(max_height - frac->cutoff));
        }
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            p.y = min.y; dun_frac_set(frac, p, frac->cutoff + _1d(max_height - frac->cutoff));
            p.y = max.y; dun_frac_set(frac, p, frac->cutoff + _1d(max_height - frac->cutoff));
        }
        /* seed the center as a floor so we know where to start looking for the room (_massage
         * and _cave_fill) */
        dun_frac_set(frac, center, 1);

        dun_frac_calc(frac);
        if (_massage(frac)) break;

        dun_frac_reset(frac);
    }
    if (!attempt)
    {
        dun_frac_free(frac);
        frac = NULL; /* fail this room gen */
    }
    return frac;
}
static void _fix_outer_wall(point_t pos, dun_grid_ptr grid)
{
    int i;
    bool fix = TRUE;
    /* we are looking for OUTER walls that should be INNER */
    for (i = 0; i < 8 && fix; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = _grid_at(p);
        if (g->flags & CELL_EXTRA)
            fix = FALSE;
        else if (g->type == FEAT_FLOOR && !(g->flags & CELL_ROOM))
            fix = FALSE;
    }
    if (fix)
    {
        grid->flags &= ~CELL_OUTER;
        grid->flags |= CELL_INNER;
    }
}
static void _fix_outer_walls(dun_frac_ptr frac)
{
    point_t min = rect_top_left(frac->rect);
    point_t max = rect_bottom_right(frac->rect), p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_grid_ptr g = _grid_at(p);
            if (g->flags & CELL_OUTER)
                _fix_outer_wall(p, g);
        }
    }
}
/* fill in a fractally generated cave. we use a hook function to support multiple
 * terrain types below the cutoff. Above the cutoff is always made into a wall without
 * using the hook. */
typedef void (*_cave_fill_f)(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid);
static void _cave_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid) { _room(pos, grid); }
static void _cave_fill(dun_frac_ptr frac, _cave_fill_f f, u32b flags)
{
    point_t         center = rect_center(frac->rect);
    point_queue_ptr q = point_queue_alloc();
    dun_bmp_ptr     bmp = dun_bmp_alloc(_dun_gen->dun, frac->rect);

    point_queue_add(q, center);
    dun_bmp_set(bmp, center);
    while (point_queue_count(q) > 0)
    {
        point_t      p = point_queue_remove(q);
        dun_grid_ptr g = _grid_at(p);
        int          i;

        f(frac, p, g);
        g->flags |= flags;

        for (i = 0; i < 8; i++)
        {
            point_t p2 = point_step(p, ddd[i]);
            if (!rect_contains_point(frac->rect, p2)) continue;
            if (dun_bmp_test(bmp, p2)) continue;

            dun_bmp_set(bmp, p2);
            if (dun_frac_get(frac, p2) >= frac->cutoff)
            {
                g = _grid_at(p2);
                _outer_wall(p2, g);
                g->flags |= flags;
                continue;
            }
            point_queue_add(q, p2);
        }
    }
    point_queue_free(q);
    dun_bmp_free(bmp);
    _fix_outer_walls(frac);
}
bool dun_gen_cave_room(dun_gen_ptr gen)
{
    point_t      size;
    rect_t       rect;
    u32b         flags;
    dun_frac_ptr frac;

    /* try for a big cave first */
    size = size_create(20 + 2*_1d(16), 12 + 2*_1d(12));
    rect = dun_gen_reserve(gen, size);
    if (!rect_is_valid(rect))
    {
        size = size_create(15 + 2*_1d(8), 10 + 2*_1d(6));
        rect = dun_gen_reserve(gen, size);
        if (!rect_is_valid(rect)) return FALSE;
    }

    /* build the fractal */
    frac = _cave_frac(rect, 60);
    if (!frac) return FALSE;

    /* build the room from the fractal */
    flags = CELL_ROOM;
    if (gen->dun->dun_lvl <= _1d(25))
        flags |= CELL_LIT;
    _cave_fill(frac, _cave_f, flags);

    dun_frac_free(frac);
    return TRUE;
}
static rect_t _wizard_rect(void)
{
    rect_t r = ui_map_rect();
    return r;
    return rect_create(r.x, r.y, r.cx/2, r.cy/2);
}
void dun_gen_cave_wizard(void)
{
    rect_t map_rect = _wizard_rect();
    point_t min = rect_top_left(map_rect);
    point_t max = rect_bottom_right(map_rect);
    point_t center = rect_center(map_rect);
    dun_frac_ptr frac = _cave_frac(map_rect, 60);
    bool done = FALSE;

    while(!done)
    {
        point_t p;
        int cmd;

        msg_line_clear();
        Term_clear_rect(map_rect);
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                int h = dun_frac_get(frac, p);
                term_char_t tc = h < frac->cutoff ? visual_get("DIRT", 0) : 
                                                    visual_get("MOUNTAIN", 0);
                Term_queue_term_char(p, tc);
            }
        }
        msg_format("Cutoff = %d [r,+,-,q]", frac->cutoff);
        Term_gotoxy(center.x, center.y);
        cmd = inkey_special(FALSE);
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q' || cmd == '\r') break;
        switch (cmd)
        {
        case ESCAPE: case 'q': case '\r':
            done = TRUE;
            break;
        case '+':
            frac->cutoff++;
            break;
        case '-':
            frac->cutoff--;
            break;
        case 'r':
            dun_frac_free(frac);
            frac = _cave_frac(map_rect, 60);
            break;
        }
    }
    plr->redraw |= PR_MAP;
    dun_frac_free(frac);
}
/************************************************************************
 * Table Driven Random Room Selection
 ************************************************************************/
typedef int (*dun_gen_weight_f)(dun_gen_ptr gen);
static bool _gen_template(dun_gen_ptr gen) { return dun_gen_template_room(gen, ROOM_ROOM, ROOM_NORMAL); }
static bool _gen_shop(dun_gen_ptr gen) { return dun_gen_template_room(gen, ROOM_ROOM, ROOM_SHOP); }
static bool _gen_recall(dun_gen_ptr gen)
{ 
    if (gen->dun->type->id == D_AMBER && gen->dun->dun_lvl < 99 && randint0(150) < gen->dun->difficulty)
        return dun_gen_template_room(gen, ROOM_ROOM, ROOM_PATTERN); /* PATTERN_EXIT can recall (pattern_effect) */
    return dun_gen_template_room(gen, ROOM_ROOM, ROOM_RECALL);
}
static bool _gen_lesser_vault(dun_gen_ptr gen) { return dun_gen_template_room(gen, ROOM_VAULT, VAULT_LESSER); }
static bool _gen_greater_vault(dun_gen_ptr gen) { return dun_gen_template_room(gen, ROOM_VAULT, VAULT_GREATER); }
static int _template_weight(dun_gen_ptr gen) { return MIN(500, 5*gen->dun->difficulty); } 
static int _lesser_vault_weight(dun_gen_ptr gen)
{
    int l = gen->dun->difficulty;
    if (gen->dun->type->flags.gen & DF_GEN_NO_VAULT) return 0;
    if (l < 40) return 0;
    return MIN(5, 1 + (l - 40)/10);
}
static int _greater_vault_weight(dun_gen_ptr gen)
{
    int l = gen->dun->difficulty;
    if (gen->dun->type->flags.gen & DF_GEN_NO_VAULT) return 0;
    if (l < 50) return 0;
    return MIN(5, 1 + (l - 50)/7);
}
static int _cave_weight(dun_gen_ptr gen)
{
    int p;
    if (gen->dun->type->flags.gen & DF_GEN_NO_CAVE) return 0;
    if (gen->scale_pct < 75) return 0;
    p = MIN(900, 200 + 10*gen->dun->difficulty);
    if (gen->dun->type->flags.gen & DF_GEN_CAVE)
        p += MAX(100, 700 - 7*gen->dun->difficulty);
    return p;
}
static int _normal_weight(dun_gen_ptr gen)
{ 
    if (gen->dun->type->flags.gen & DF_GEN_NO_CAVE) return 700;
    if (gen->dun->type->flags.gen & DF_GEN_CAVE) return 0;
    return MAX(100, 700 - 7*gen->dun->difficulty);
}
static int _overlap_weight(dun_gen_ptr gen)
{ 
    if (gen->dun->type->flags.gen & DF_GEN_NO_CAVE) return 300;
    return MAX(30, 300 - 3*gen->dun->difficulty); 
}
typedef struct {
    cptr             name;
    dun_gen_room_f   builder;
    dun_gen_weight_f weight;
    int              priority;
} _room_prob_t, *_room_prob_ptr;
static _room_prob_t _room_tbl[] = {
    { "Normal", dun_gen_normal_room, _normal_weight, 1 },
    { "Overlapped", dun_gen_overlap_room, _overlap_weight, 1 },
    { "Template", _gen_template, _template_weight, 2 },
    { "Cave", dun_gen_cave_room, _cave_weight, 2 },
    { "Lesser Vault", _gen_lesser_vault, _lesser_vault_weight, 4 },
    { "Greater Vault", _gen_greater_vault, _greater_vault_weight, 5 },
    { 0 }
};

static int _cmp_room_prob(_room_prob_ptr left, _room_prob_ptr right)
{
    if (left->priority < right->priority) return 1;
    if (left->priority > right->priority) return -1;
    return 0;
}
static _room_prob_ptr _random_room(dun_gen_ptr gen)
{
    int i, tot = 0, pick;
    for (i = 0; ; i++)
    {
        _room_prob_ptr p = &_room_tbl[i];
        if (!p->name) break;
        tot += p->weight(gen);
    }
    assert(tot);
    if (!tot) return NULL;
    pick = randint0(tot);
    for (i = 0; ; i++)
    {
        _room_prob_ptr p = &_room_tbl[i];
        if (!p->name) break;
        pick -= p->weight(gen);
        if (pick < 0) return p;
    }
    assert(0);
    return NULL;
}
static vec_ptr _random_rooms(dun_gen_ptr gen, int ct)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; i < ct; i++)
        vec_add(v, _random_room(gen));

    vec_sort(v, (vec_cmp_f)_cmp_room_prob);
    return v;
}

/************************************************************************
 * Rooms
 ************************************************************************/
static void _cavern_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid) { _floor(pos, grid); }
static void _lava_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        dun_place_deep_lava(_dun_gen->dun, pos);
    else if (h < 2*frac->cutoff/3)
        dun_place_shallow_lava(_dun_gen->dun, pos);
    else
        _floor(pos, grid);
}
static void _lava_vault_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        dun_place_shallow_lava(_dun_gen->dun, pos);
    else if (h < 2*frac->cutoff/3)
        dun_place_deep_lava(_dun_gen->dun, pos);
    else
        dun_place_shallow_lava(_dun_gen->dun, pos);
}
static void _water_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        dun_place_deep_water(_dun_gen->dun, pos);
    else if (h < 2*frac->cutoff/3)
        dun_place_shallow_water(_dun_gen->dun, pos);
    else
        _floor(pos, grid);
}
static void _water_vault_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        dun_place_shallow_water(_dun_gen->dun, pos);
    else if (h < 2*frac->cutoff/3)
        dun_place_deep_water(_dun_gen->dun, pos);
    else
        dun_place_shallow_water(_dun_gen->dun, pos);
}
static void _collapsed_cave_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        _floor(pos, grid);
    else if (h < 2*frac->cutoff/3)
        _floor(pos, grid);
    else
        _rubble(pos, grid);
}
static void _earth_vault_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        _rubble(pos, grid);
    else if (h < 2*frac->cutoff/3)
        _floor(pos, grid);
    else
        _rubble(pos, grid);
}
static void _tree_f(dun_frac_ptr frac, point_t pos, dun_grid_ptr grid)
{
    int h = dun_frac_get(frac, pos);
    if (h < frac->cutoff/3)
        dun_place_grass(_dun_gen->dun, pos);
    else if (h < 2*frac->cutoff/3)
        dun_place_tree(_dun_gen->dun, pos);
    else
        dun_place_grass(_dun_gen->dun, pos);
}
static bool _lake(rect_t rect, _cave_fill_f f, int cutoff_pct)
{
    dun_frac_ptr frac = _cave_frac(rect, cutoff_pct);
    if (frac)
    {
        _cave_fill(frac, f, 0);
        dun_frac_free(frac);
        return TRUE;
    }
    return FALSE;
}
static int _random_lake(dun_gen_ptr gen)
{
    struct { _cave_fill_f f; u32b mask; int lvl; int prob; } tbl[] = {
        {_water_f, DF_GEN_LAKE_WATER, 30, 2 },
        {_water_vault_f, DF_GEN_LAKE_WATER, 30, 1 },
        {_lava_f, DF_GEN_LAKE_LAVA, 50, 2},
        {_lava_vault_f, DF_GEN_LAKE_LAVA, 50, 1},
        {_collapsed_cave_f, DF_GEN_LAKE_RUBBLE, 20, 2},
        {_earth_vault_f, DF_GEN_LAKE_RUBBLE, 20, 1},
        {_tree_f, DF_GEN_LAKE_TREE, 1, 2},
        {0},
    };
    int i, tot = 0, pick;
    for (i = 0; ; i++)
    {
        if (!tbl[i].f) break;
        if (!(gen->dun->type->flags.gen & tbl[i].mask)) continue;
        if (gen->dun->difficulty < tbl[i].lvl) continue;
        tot += tbl[i].prob;
    }
    if (!tot) return 0;
    pick = randint0(tot);
    for (i = 0; ; i++)
    {
        if (!tbl[i].f) break;
        if (!(gen->dun->type->flags.gen & tbl[i].mask)) continue;
        if (gen->dun->difficulty < tbl[i].lvl) continue;
        pick -= tbl[i].prob;
        if (pick < 0 && _lake(rect_interior(gen->dun->rect), tbl[i].f, 60))
        {
            switch (tbl[i].mask)
            {
            case DF_GEN_LAKE_WATER: return FEAT_WATER; /* prevents rivers of lava later */
            case DF_GEN_LAKE_LAVA: return FEAT_LAVA;   /* prevents rivers of water later */
            case DF_GEN_LAKE_RUBBLE: return FEAT_WALL;
            case DF_GEN_LAKE_TREE: return FEAT_TREE;
            }
        }
    }
    return 0;
}
typedef void (*_river_f)(dun_ptr dun, point_t pos, dun_cell_ptr cell, int d, int w);
static void _river_lava(dun_ptr dun, point_t pos, dun_cell_ptr cell, int d, int w)
{
    if (d > w)
    {
        if (!lava_is_deep(cell))
            dun_place_shallow_lava(dun, pos);
    }
    else
        dun_place_deep_lava(dun, pos);
}
static void _river_water(dun_ptr dun, point_t pos, dun_cell_ptr cell, int d, int w)
{
    if (d > w)
    {
        if (!water_is_deep(cell))
            dun_place_shallow_water(dun, pos);
    }
    else
        dun_place_deep_water(dun, pos);
}
static void _river_chasm(dun_ptr dun, point_t pos, dun_cell_ptr cell, int d, int w)
{
    if (d > w)
    {
        if (!cell_is_chasm(cell))
            dun_place_rubble(dun, pos);
    }
    else
        dun_place_chasm(dun, pos);
}
static void _river_aux(dun_gen_ptr gen, point_t start, point_t stop, _river_f feat, int width)
{
    int length = point_distance(start, stop);

    if (length > 4)
    {
        point_t mp = point_perturbed_midpoint(start, stop);
        if (!dun_pos_interior(gen->dun, mp))
            mp = point_midpoint(start, stop);

        _river_aux(gen, start, mp, feat, width);
        _river_aux(gen, mp, stop, feat, width);
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

                    if (!dun_pos_interior(gen->dun, p2)) continue;
                    g = dun_grid_at(gen->dun, p2);
                    if (g->flags & CELL_PERM) continue;

                    dist = point_distance(p2, p);
                    if (dist > rand_spread(width, 1)) continue;
                    feat(gen->dun, p2, g, dist, width);
                }
            }
        }
    }
}
static point_t _rect_random_left(rect_t r)
{
    int y = rand_range(r.y, rect_bottom(r));
    return point_create(r.x, y);
}
static point_t _rect_random_right(rect_t r)
{
    int y = rand_range(r.y, rect_bottom(r));
    return point_create(rect_right(r), y);
}
static point_t _rect_random_top(rect_t r)
{
    int x = rand_range(r.x, rect_right(r));
    return point_create(x, r.y);
}
static point_t _rect_random_bottom(rect_t r)
{
    int x = rand_range(r.x, rect_right(r));
    return point_create(x, rect_bottom(r));
}
static void dun_gen_river(dun_gen_ptr gen, _river_f feat)
{
    rect_t  rect = rect_interior(gen->dun->rect);
    point_t start = {0};
    point_t stop = {0};
    int width = 2;

    if (one_in_(2))
    {
        start = _rect_random_top(rect);
        width = 3;
        switch (_1d(7))
        {
        case 1: stop = _rect_random_left(rect); break;
        case 2: stop = _rect_random_right(rect); break;
        default: stop = _rect_random_bottom(rect);
        }
    }
    else
    {
        start = _rect_random_left(rect);
        switch (_1d(7))
        {
        case 1: stop = _rect_random_top(rect); break;
        case 2: stop = _rect_random_bottom(rect); break;
        default: stop = _rect_random_right(rect);
        }
    }
    _river_aux(gen, start, stop, feat, width);
    gen->river = TRUE;
}
static void _pre_gen_hooks(dun_gen_ptr gen)
{
    dun_world_ptr world = dun_worlds_current();

    if (world->pre_gen_f)
        world->pre_gen_f(world, gen);

    if (gen->dun->type->pre_gen_f)
        gen->dun->type->pre_gen_f(gen->dun->type, gen);
}
void dun_gen_lava_vault(dun_gen_ptr gen)
{
    _lake(rect_interior(gen->dun->rect), _lava_vault_f, 80);
    gen->lake = FEAT_LAVA;
}
static void _pre_gen_rooms(dun_gen_ptr gen)
{
    rect_t rect = rect_interior(gen->dun->rect);

    _pre_gen_hooks(gen);

    /* special kinds of levels (arena, cavern, lake et. al.). These will overwrite
     * any existing rooms (e.g. ROOM_TRAVEL; cf _pre_gen in dun_world.c) */
    if (point_vec_length(gen->rooms) == 0)
    {
        if (gen->dun->dun_lvl > 30 && one_in_(36) && (gen->dun->type->flags.gen & DF_GEN_DESTROY))
        {
            gen->destroyed = TRUE;
            if (one_in_(2)) _lake(rect, _collapsed_cave_f, 60);
            else _lake(rect, _earth_vault_f, 60);
        }
        else if ((gen->dun->type->flags.gen & DF_GEN_ARENA) && one_in_(24))
        {
            gen->arena = TRUE;
            dun_iter_grids(gen->dun, _floor);
            dun_iter_boundary(gen->dun, _extra_wall);
        }
        else if (!gen->lake && one_in_(24) && (gen->dun->type->flags.gen & DF_GEN_LAKE_MASK))
        {
            gen->lake = _random_lake(gen);
        }
        else if (!gen->cavern && gen->dun->dun_lvl > 20 && (gen->dun->type->flags.gen & DF_GEN_CAVERN) && randint0(1000) < gen->dun->dun_lvl)
        {
            gen->cavern = _lake(rect, _cavern_f, 60);
        }
    }

    /* Special rooms for recall and shopping ... These are guaranteed
     * to happen every N (visited) levels. */
    if (gen->dun->dun_lvl >= 4)
    {
        if (gen->dun->type->last_recall >= 2 || randint0(400) < 200 + gen->dun->dun_lvl)
        {
            /* XXX FF_RECALL might get "clipped" on DF_TOWER levels */
            if (_gen_recall(gen)) gen->dun->flags |= DF_RECALL;
            else gen->dun->type->last_recall++;
        }
        else
            gen->dun->type->last_recall++;
    }
    if (gen->dun->dun_lvl >= 15)
    {
        if (gen->dun->type->last_shop >= 20 || one_in_(20))
        {
            gen->dun->town = town_alloc(TOWN_RANDOM, "Random");
            _gen_shop(gen);
            gen->dun->flags |= DF_SHOP;
        }
        else
            gen->dun->type->last_shop++;
    }
}
static int _tower_dist_squared(point_t p1, point_t p2)
{
    point_t p = point_abs(point_subtract(p1, p2));
    return p.x*p.x + 4*p.y*p.y;
}
static int _tower_max(dun_gen_ptr gen)
{
    if (!gen->tower_max)
    {
        rect_t  r = gen->dun->rect;
        point_t center = rect_center(r);
        gen->tower_max = MAX(_tower_dist_squared(rect_top_center(r), center),
                             _tower_dist_squared(rect_left_center(r), center));
    }
    return gen->tower_max;
}
static bool _tower_out_of_bounds(dun_gen_ptr gen, point_t p)
{
    if (gen->dun->type->flags.gen & DF_GEN_TOWER)
    {
        int d2 = _tower_dist_squared(rect_center(gen->dun->rect), p);
        if (d2 >= _tower_max(gen)) return TRUE;
    }
    return FALSE;
}
static void _post_gen_rooms(dun_gen_ptr gen)
{
    if (gen->dun->type->flags.gen & DF_GEN_TOWER) /* XXX */
    {
        rect_t  r = gen->dun->rect;
        point_t center = rect_center(r);
        point_t min = rect_top_left(r);
        point_t max = rect_bottom_right(r);
        point_t p;
        int     r2 = _tower_max(gen);

        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                int d2 = _tower_dist_squared(p, center);
                dun_grid_ptr g = _grid_at(p);
                if (d2 >= r2)
                {
                    dun_destroy_obj_at(gen->dun, p);
                    _solid_perm_wall(p, g);
                }
            }
        }
    }
    if (gen->destroyed)
    {
        int i;
        rect_t r = rect_deflate(gen->dun->rect, 5, 5);
        for (i = 0; i < _1d(5); i++)
        {
            point_t pos = rect_random_point(r);
            assert(cave == gen->dun);
            destroy_area(pos, 15, -1);
        }
    }
    if (!gen->river && one_in_(7) && _1d(gen->dun->dun_lvl) > 5)
    {
        _river_f feat = NULL;
        if (_1d(200) > gen->dun->dun_lvl && (gen->dun->type->flags.gen & DF_GEN_RIVER_WATER))
        {
            if (gen->lake != FEAT_LAVA)
                feat = _river_water;
        }
        else if (gen->dun->type->flags.gen & DF_GEN_RIVER_LAVA)
        {
            if (gen->lake != FEAT_WATER)
                feat = _river_lava;
        }
        /* XXX Experimental: DF_GEN_RIVER_CHASM */
        if (!gen->lake && one_in_(20) && (gen->dun->type->flags.gen & DF_GEN_CAVERN))
            feat = _river_chasm;

        if (feat)
            dun_gen_river(gen, feat);
    }
}
bool dun_gen_rooms(dun_gen_ptr gen)
{
    int     ct = MAX(5, _scale(rand_range(10, 25), gen->scale_pct)), i;
    vec_ptr rooms = _random_rooms(gen, ct);

    _pre_gen_rooms(gen);
    for (i = 0; i < vec_length(rooms); i++)
    {
        _room_prob_ptr p = vec_get(rooms, i);
        if (!p->builder(gen)) continue;
        if (_debug_f)
            _debug_f(gen, format("Room #%d: %s", i + 1, p->name));
    }
    if (point_vec_length(gen->rooms) < 3)
    {
        gen->error = "Unable to generate enough rooms.";
        vec_free(rooms);
        return FALSE;
    }
    _post_gen_rooms(gen);
    vec_free(rooms);
    return TRUE;
}

/************************************************************************
 * Streamers
 ************************************************************************/
static dun_place_f _streamer_f;
static void _river_streamer(dun_ptr dun, point_t pos, dun_cell_ptr cell, int d, int w)
{
    if (_1d(100) <= 70) return; /* XXX tune me: _river_aux will test cells multiple times */
    if (cell_is_water(cell) || cell_is_lava(cell)) return;
    if (_streamer_f != dun_place_brake) /* XXX _forest_aux */
    {
        /*if (!cell_is_wall(cell)) return; XXX in forest, 'wall' might be a tree ... or brake! */
        if (!(cell->flags & (CELL_EXTRA | CELL_INNER | CELL_OUTER | CELL_SOLID))) return;
        if (wall_has_secret_door(cell)) return;
    }
    _streamer_f(dun, pos);
}
static void _streamer(dun_gen_ptr gen, dun_place_f feat)
{
    _streamer_f = feat;
    dun_gen_river(gen, _river_streamer);
}
void dun_gen_streamers(dun_gen_ptr gen)
{
    if (gen->lake) return;
    if (gen->dun->type->place_stream1)
        _streamer(gen, gen->dun->type->place_stream1);
    if (gen->dun->type->place_stream2)
        _streamer(gen, gen->dun->type->place_stream2);
}

/************************************************************************
 * Tunneler
 ************************************************************************/
static int _rand_dir(void) { return ddd[randint0(4)]; }
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
static int _change_dir(dun_gen_ptr gen, point_t pos, point_t stop)
{
    if (randint0(100) < gen->rand_dir_pct)
        return _rand_dir();
    return _dir(pos, stop);
}
static void _pierce(dun_gen_ptr gen, point_t pos)
{
    int i;
    point_vec_push(gen->walls, pos);
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_grid_ptr g = _grid_at(p);
        if (g->flags & CELL_OUTER)
            _solid_noperm_wall(p, g);
    }
}
static bool _tunnel(dun_gen_ptr gen, point_t start, point_t stop)
{
    point_t      pos = start;
    int          dir = _dir(start, stop);
    point_t      next_pos;
    int          ct = 0;
    dun_grid_ptr grid;
    bool         door_flag = FALSE;

    while (!point_equals(pos, stop))
    {
        if (ct++ > 2000) return FALSE;
        if (randint0(100) < gen->change_dir_pct)
            dir = _change_dir(gen, pos, stop);
        next_pos = point_step(pos, dir);
        while (!dun_pos_interior(gen->dun, next_pos))
        {
            dir = _change_dir(gen, pos, stop);
            next_pos = point_step(pos, dir);
        }
        grid = _grid_at(next_pos);
        if (grid->flags & CELL_SOLID) /* work around SOLID obstacles */
            dir = _change_dir(gen, pos, stop);
        else if (grid->flags & CELL_OUTER) /* pierce room walls (coming and going) */
        {
            /* peeking ahead prevents piercing room corners */
            point_t p = point_step(next_pos, dir);
            dun_grid_ptr g = _grid_at(p);
            if (g->flags & (CELL_OUTER | CELL_SOLID)) continue;

            pos = next_pos;
            _pierce(gen, pos);
        }
        else if (grid->flags & CELL_ROOM) /* ignore room grids (including INNER walls) */
            pos = next_pos;
        else if (grid->flags & (CELL_EXTRA | CELL_INNER)) /* lay down tunnel floor grids */
        {
            pos = next_pos;
            point_vec_push(gen->floors, pos);
            door_flag = FALSE; /* Allow door in next grid */
        }
        else /* Handle corridor intersections or overlaps */
        {
            pos = next_pos;
            if (!door_flag)
            {
                point_vec_push(gen->doors, pos);
                door_flag = TRUE; /* prevent adjacent doors */
            }
            if (randint0(100) < gen->quit_pct && point_fast_distance(pos, start) > 15) /* quit */
                break;
        }
    }
    return TRUE;
}
/************************************************************************
 * Tunnel Driver
 *
 * For big dungeons, it is important to employ some sort of room clustering
 * algorithm rather than simply randomly stomping about. At the moment, there
 * is but a single clustering algorithm but more could be added:
 * [1] Cluster rooms by quadrant.
 * [2] Connect rooms in each cluster, sorting the room centers for a 
 *     circular cycle. Tunneler is allowed to quit if it likes.
 * [3] Connect the clusters in a circular path, picking the best room
 *     from each cluster during the join. Tunneler is not allowed to quit.
 ************************************************************************/
static int _adjacent_corridor_count(point_t pos)
{
    int i, ct = 0;
    for (i = 0; i < 4; i++)
    {
        point_t      p = point_step(pos, ddd[i]);
        dun_grid_ptr grid = _grid_at(p);

        if (grid->type == FEAT_WALL) continue;
        if (grid->type != FEAT_FLOOR) continue;
        if (grid->flags & CELL_ROOM) continue;
        ct++;
    }
    return ct;
}
static bool _is_wall_at(point_t pos)
{
    dun_grid_ptr grid = _grid_at(pos);
    return grid->type == FEAT_WALL;
}
static bool _possible_door_at(point_t pos)
{
    if (_adjacent_corridor_count(pos) < 2) return FALSE;
    if (_is_wall_at(point_step(pos, 8)) && _is_wall_at(point_step(pos, 2))) return TRUE;
    if (_is_wall_at(point_step(pos, 4)) && _is_wall_at(point_step(pos, 6))) return TRUE;
    return FALSE;
}
static void _try_door_at(point_t pos)
{
    dun_grid_ptr grid;
    if (!dun_pos_interior(_dun_gen->dun, pos)) return;
    if (_type()->flags.gen & DF_GEN_NO_DOORS) return;

    grid = _grid_at(pos);

    if (grid->type == FEAT_WALL) return;
    if (grid->flags & CELL_ROOM) return;
    if (!_possible_door_at(pos)) return;
    if (_1d(100) > _dun_gen->door_intersect_pct) return;

    _random_door(pos, grid);
}
static bool _connect(dun_gen_ptr gen, point_t start, point_t stop)
{
    int j;
    bool fail = FALSE;
    /* compute the tunnel */
    point_vec_clear(gen->floors);
    point_vec_clear(gen->walls);

    if (!_tunnel(gen, start, stop))
        fail = TRUE;

    /* build the tunnel: note that EXTRA walls are often chasms in certain dun_types.
     * dun_gen begins with dun_iter_grids(_extra_wall) ... */
    for (j = 0; j < point_vec_length(gen->floors); j++)
    {
        point_t      p = point_vec_get(gen->floors, j);
        dun_grid_ptr grid = _grid_at(p);

        if (grid->type == FEAT_WALL || grid->type == FEAT_TREE || grid->type == FEAT_CHASM)
            _floor(p, grid);
    }

    /* pierce outer walls of rooms */
    for (j = 0; j < point_vec_length(gen->walls); j++)
    {
        point_t      p = point_vec_get(gen->walls, j);
        dun_grid_ptr grid = _grid_at(p);

        _room(p, grid);

        if (!(gen->dun->type->flags.gen & DF_GEN_NO_DOORS) && randint0(100) < gen->door_pierce_pct)
            _random_door(p, grid);
    }
    if (_debug_f)
    {
        _grid_at(start)->flags |= CELL_TEMP;
        _grid_at(stop)->flags |= CELL_TEMP;
        _debug_f(gen, format("Tunnel %s from (%d, %d) to (%d, %d).", 
            fail ? "*FAILED*" : "worked", 
            start.x, start.y, stop.x, stop.y));
        _grid_at(start)->flags &= ~CELL_TEMP;
        _grid_at(stop)->flags &= ~CELL_TEMP;
        if (fail)
            cmsg_print(TERM_VIOLET, "TUNNEL ***FAILED***");
    }
    return !fail;
}
static void _junctions(dun_gen_ptr gen)
{
    int i;
    /* Place intersection doors */
    for (i = 0; i < point_vec_length(gen->doors); i++)
    {
        point_t p = point_vec_get(gen->doors, i);
        _try_door_at(point_step(p, 6));
        _try_door_at(point_step(p, 4));
        _try_door_at(point_step(p, 8));
        _try_door_at(point_step(p, 2));
    }
}
static bool _connect_rooms(dun_gen_ptr gen, point_vec_ptr rooms)
{
    int ct = point_vec_length(rooms), i, fail_ct = 0, old_quit_pct = gen->quit_pct;
    point_t start, stop;

    point_vec_clear(gen->doors);
    if (ct == 2) gen->quit_pct = 0;

    /* tunnel from one room to the next in a complete cycle */
    start = point_vec_get(rooms, ct - 1);
    for (i = 0; i < ct; i++)
    {
        stop = point_vec_get(rooms, i);

        /* XXX river might start on an exact room center! cf dun_gen_reserve and dun_gen_river */
        if (point_compare(start, stop) == 0) continue;

        if (!_connect(gen, start, stop))
            fail_ct++;
        if (fail_ct >= 2) 
        {
            gen->error = "Unable to tunnel";
            return FALSE;
        }
        start = stop;
        if (ct == 2)
        {
            gen->quit_pct = old_quit_pct;
            break;
        }
    }
    _junctions(gen);
    return TRUE;
}
static rect_t _qI(rect_t r)
{
    point_t tl = rect_top_center(r);
    point_t sz = point_subtract(rect_right_center(r), tl);
    rect_t  q = rect_create(tl.x, tl.y, sz.x, sz.y);
    assert(rect_contains(r, q));
    return q;
}
static rect_t _qII(rect_t r)
{
    point_t tl = rect_top_left(r);
    point_t sz = point_subtract(rect_center(r), tl);
    rect_t  q = rect_create(tl.x, tl.y, sz.x, sz.y);
    assert(rect_contains(r, q));
    return q;
}
static rect_t _qIII(rect_t r)
{
    point_t tl = rect_left_center(r);
    point_t sz = point_subtract(rect_bottom_center(r), tl);
    rect_t  q = rect_create(tl.x, tl.y, sz.x, sz.y + 1);
    assert(rect_contains(r, q));
    return q;
}
static rect_t _qIV(rect_t r)
{
    point_t tl = rect_center(r);
    point_t sz = point_subtract(rect_bottom_right(r), tl);
    rect_t  q = rect_create(tl.x, tl.y, sz.x + 1, sz.y + 1);
    assert(rect_contains(r, q));
    return q;
}
static point_t _closest(point_vec_ptr points, point_t pos)
{
    int i, best_dist = 999999;
    point_t best = point_create(-1, -1);

    for  (i = 0; i < point_vec_length(points); i++)
    {
        point_t p = point_vec_get(points, i);
        int     d = point_fast_distance(pos, p);
        if (d < best_dist)
        {
            best = p;
            best_dist = d;
        }
    }
    return best;
}
typedef struct { point_vec_ptr rooms; rect_t rect; } _cluster_t, *_cluster_ptr;
static _cluster_t _cluster(point_vec_ptr rooms, rect_t rect)
{
    _cluster_t cluster = {0};
    int i;

    cluster.rect = rect;
    for (i = 0; i < point_vec_length(rooms); i++)
    {
        point_t p = point_vec_get(rooms, i);
        if (rect_contains_point(rect, p))
        {
            if (!cluster.rooms)
                cluster.rooms = point_vec_alloc();
            point_vec_add(cluster.rooms, p);
        }
    }
    return cluster;
}
static int _quadrant(point_t p, point_t o)
{
    point_t v = point_subtract(p, o);
    if (v.x >= 0 && v.y >= 0) return 1; /* bottom right due to text mapping mode */
    if (v.x < 0 && v.y >= 0) return 2;
    if (v.x < 0 && v.y < 0) return 3;
    return 4;
}
static int _cmp_cluster(point_t left, point_t right, point_t origin)
{
    int ql = _quadrant(left, origin);
    int qr = _quadrant(right, origin);
    if (ql < qr) return -1;
    if (ql > qr) return 1;
    switch (ql) /* Draw a picture with an arrow in each quadrant for the */
    {           /* sort direction: should give a nice diamond shape! */
    case 1: /* arrown down and to the left */
        if (left.x < right.x) return 1;  /* decreasing x is left */
        if (left.x > right.x) return -1;
        if (left.y < right.y) return -1; /* increasing y is down */
        if (left.y > right.y) return 1;
        break;
    case 2: /* arrow up and to the left */
        if (left.x < right.x) return 1;
        if (left.x > right.x) return -1;
        if (left.y < right.y) return 1; /* decreasing y is up */
        if (left.y > right.y) return -1;
        break;
    case 3: /* arrow up and to the right */
        if (left.x < right.x) return -1;
        if (left.x > right.x) return 1;
        if (left.y < right.y) return 1;
        if (left.y > right.y) return -1;
        break;
    case 4: /* arrow down and to the right */
        if (left.x < right.x) return -1;
        if (left.x > right.x) return 1;
        if (left.y < right.y) return -1;
        if (left.y > right.y) return 1;
        break;
    }
    return 0;
}
void _sort_cluster(point_vec_ptr rooms, point_t origin)
{
    int i, j, ct = point_vec_length(rooms);
    for (i = 0; i < ct; i++) /* bubble sort: ct is really small (4 to 6 at most) */
    {
        for (j = ct-1; j > i; j--)
        {
            point_t p1 = point_vec_get(rooms, j);
            point_t p2 = point_vec_get(rooms, j-1);
            if (_cmp_cluster(p1, p2, origin) < 0)
                point_vec_swap(rooms, j, j-1);
        }
    }
}
static point_t _rect_closest(rect_t rect, point_t pos)
{
    point_vec_ptr points = point_vec_alloc();
    point_t result;

    point_vec_push(points, rect_top_left(rect));
    point_vec_push(points, rect_top_center(rect));
    point_vec_push(points, rect_top_right(rect));
    point_vec_push(points, rect_right_center(rect));
    point_vec_push(points, rect_bottom_right(rect));
    point_vec_push(points, rect_bottom_center(rect));
    point_vec_push(points, rect_bottom_left(rect));
    point_vec_push(points, rect_left_center(rect));

    result = _closest(points, pos);

    point_vec_free(points);
    return result;
}
static void _connect_clusters(dun_gen_ptr gen, _cluster_t cluster1, _cluster_t cluster2)
{
    point_t anchor = _rect_closest(cluster1.rect, rect_center(cluster2.rect));
    point_t start = _closest(cluster1.rooms, anchor);
    point_t stop = _closest(cluster2.rooms, start);

    if (dun_pos_interior(gen->dun, start) && dun_pos_interior(gen->dun, stop))
        _connect(gen, start, stop);
}
bool dun_gen_tunnels(dun_gen_ptr gen)
{
    _cluster_t clusters[4], cluster;
    int        i, ct = 0;
    bool       result = TRUE;

    if (point_vec_length(gen->rooms) < 2) return FALSE;

    /* compute quadrants and room clusters, but only 
     * remember populated quadrants. */
    cluster = _cluster(gen->rooms, _qI(gen->dun->rect));
    if (cluster.rooms) clusters[ct++] = cluster;

    cluster = _cluster(gen->rooms, _qII(gen->dun->rect));
    if (cluster.rooms) clusters[ct++] = cluster;

    cluster = _cluster(gen->rooms, _qIII(gen->dun->rect));
    if (cluster.rooms) clusters[ct++] = cluster;

    cluster = _cluster(gen->rooms, _qIV(gen->dun->rect));
    if (cluster.rooms) clusters[ct++] = cluster;

    /* connect the rooms in each cluster */
    for (i = 0; i < ct && result; i++)
    {
        _cluster_t c = clusters[i];
        if (point_vec_length(c.rooms) < 2) continue;
        _sort_cluster(c.rooms, rect_center(c.rect));
        if (!_connect_rooms(gen, c.rooms))
            result = FALSE;
    }

    /* connect the clusters to each other */
    if (result && ct > 1)
    {
        _cluster_t start, stop;
        gen->quit_pct = 0; /* no quitting this time! */
        point_vec_clear(gen->doors);
        start = clusters[ct - 1];
        for (i = 0; i < ct; i++)
        {
            stop = clusters[i];
            _connect_clusters(gen, start, stop);
            start = stop;
        }
        _junctions(gen);
    }

    for (i = 0; i < ct; i++)
        point_vec_free(clusters[i].rooms);

    return result;
}
