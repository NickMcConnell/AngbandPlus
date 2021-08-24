#include "angband.h"

#include "mon_ai.h"
#include <assert.h>

/* This is a refactor of melee2.c (process_monster, get_moves, etc.)
 *
 * For monster processing, see mon_process, which will successively:
 *    _try_stairs (attempt to leave current level either to chase plr, or run away)
 *    _try_spell (spellcasting)
 *    _move
 * XXX cf dun_process_monsters
 *
 * For monster movement, the core concepts are:
 * [1] Moving towards or away from a point. This does not use pathfinding,
 *     but, rather, simply tries an array of directions based on mon->pos
 *     and the goal. (melee2 reduced everything to this via get_moves and mm)
 *     XXX see _move_towards and _move_away.
 * [2] Pathfinding using a flow (dun_flow_t). Here we make local decisions
 *     based on the cost of grids adjacent to mon->pos.
 *     A flow represents a goal to be reached (flow->pos). This might be
 *     the plr but might be something else. Common flows are:
 *     [2.1] dun->flow: always leads to plr, though perhaps not directly.
 *           If plr is on a different lvl, then dun->flow->pos leads to 
 *           stairs instead.
 *     [2.2] mon->flow: allows hunting specific monsters (e.g. command
 *           your pets to hunt pet_t_m_idx). mon->flow->pos tracks mon->pos.
 *           XXX this might need rebalancing as it is very powerful!
 *     [2.3] pack->flow (pending): allows monster patrols. allows friendly
 *           monsters to hunt enemies
 *     XXX see _flow_towards and _flow_away.
 * Higher level AI will usually use either of these 2 strategies as appropriate.
 * Note that _try_move is the workhorse, attempting a single move to an adjacent
 * pos. Higher level AI is all about trying alternatives in the best order.
 * XXX see _move
 *
 */

static mon_ptr _current = NULL;

/************************************************************************
 * Pre-compute offset vectors for a given distance. Used for monster hiding.
 ************************************************************************/
static vec_ptr _offsets = NULL;
static void _precompute(vec_ptr vec, int max)
{
    int i;
    point_t d;
    point_t o = {0};
    for (i = 0; i <= max; i++)
        vec_add(vec, point_vec_alloc());
    for (d.y = -max; d.y <= max; d.y++)
    {
        for (d.x = -max; d.x <= max; d.x++)
        {
            int x = point_distance(d, o);
            if (x <= max)
            {
                point_vec_ptr pv = vec_get(vec, x);
                point_vec_push(pv, d);
            }
        }
    }
}
point_vec_ptr distance_offsets(int dis)
{
    assert(0 <= dis && dis <= MAX_PRECOMPUTE_DISTANCE);
    if (!_offsets)
    {
        _offsets = vec_alloc((vec_free_f)point_vec_free);
        _precompute(_offsets, MAX_PRECOMPUTE_DISTANCE);
    }
    return vec_get(_offsets, dis);
}

/************************************************************************
 * Monster Hiding and Safety:
 * Always check result for dun_pos_interior() to see if valid.
 ************************************************************************/
static point_t _point_invalid(void) { return point_create(-1, -1); }
static point_t _find_safety(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    int     r;
    int     best_d = 0;
    point_t best_p = {0};

    assert(cave == dun); /* XXX */
    assert(plr->dun_id == mon->dun->id);

    for (r = 1; r <= MAX_PRECOMPUTE_DISTANCE; r++)
    {
        point_vec_ptr pv = distance_offsets(r);
        int i;

        for (i = 0; i < point_vec_length(pv); i++)
        {
            point_t v = point_vec_get(pv, i);
            point_t p = point_add(mon->pos, v);
            dun_grid_ptr g;
            int d, d2;

            if (!dun_pos_interior(dun, p)) continue;
            g = dun_grid_at(dun, p);

            if (!cell_allow_mon(g, mon)) continue;

            d = dun_flow_at(dun->flow, p);
            d2 = dun_flow_at(dun->flow, mon->pos);

            if (d == DUN_FLOW_NULL) continue;
            if (d2 == DUN_FLOW_NULL) continue;
            if (d > d2 + 2*r) continue; /* skip if too far away */

            if (!point_project_aux(plr->pos, p, PROJECT_ILLUSION))
            {
                int d = plr_distance(p);

                if (d > best_d)
                {
                    best_p = p;
                    best_d = d;
                }
            }
        }

        if (best_d > 0)
            return best_p;
    }

    return _point_invalid();
}
/*
* Provide a location to flee to, but give the player a wide berth.
*
* A monster may wish to flee to a location that is behind the player,
* but instead of heading directly for it, the monster should "swerve"
* around the player so that he has a smaller chance of getting hit.
*
* Monster is hostile, and wants to flee to the safety of safe_pos.
* We'll return a better choice, if possible, so as to avoid the plr.
* We always return a valid pos.
*/
static point_t _adjust_safety(mon_ptr mon, point_t safe_pos)
{
    dun_ptr dun = mon->dun;
    int     i, best_s = -1; 
    point_t best_p = {0};

    assert(dun_pos_interior(dun, safe_pos));
    if (mon->dun->id != plr->dun_id) return safe_pos; /* no-need */

    for (i = 7; i >= 0; i--)
    {
        point_t p = point_step(mon->pos, ddd[i]);
        int d, s, f;

        if (!dun_pos_interior(dun, p)) continue;
        d = point_distance(p, safe_pos);
        f = dun_flow_at(dun->flow, p);
        if (f == DUN_FLOW_NULL) continue;

        /* get the highest score */
        s = 5000 / (d + 3) - 500 / (f + 1);
        if (s < 0) s = 0;
        if (s < best_s) continue;
        best_s = s;
        best_p = p;
    }
    if (best_s > 0)
        return best_p;
    return safe_pos; /* no change */
}
static point_t _find_hiding(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    int     r;

    assert(cave == dun); /* XXX */

    for (r = 1; r <= MAX_PRECOMPUTE_DISTANCE; r++)
    {
        point_vec_ptr pv = distance_offsets(r);
        int i, best_d = 999;
        point_t best_p = {0};

        for (i = 0; i < point_vec_length(pv); i++)
        {
            point_t v = point_vec_get(pv, i);
            point_t p = point_add(mon->pos, v);
            dun_grid_ptr g;

            if (!dun_pos_interior(dun, p)) continue;
            g = dun_grid_at(dun, p);

            if (dun_plr_at(dun, p)) continue;
            if (dun_mon_at(dun, p)) continue;
            if (!illusion_allow_mon(g, mon)) continue;

            /* Check for closest hidden, available grid */
            if ( !point_project_aux(plr->pos, p, PROJECT_ILLUSION)  /* hidden */
              && dun_project_aux(dun, mon->pos, p, PROJECT_STOP, DUN_PATH_MAX) ) /* nothing in the way along this path */
            {
                int d = point_distance(p, plr->pos);

                if (d < best_d && d >= 2)
                {
                    best_d = d;
                    best_p = p;
                }
            }
        }
        if (best_d < 999)
            return best_p;
    }
    return _point_invalid();
}

/************************************************************************
 * Handle multiple choices for a given move. Allow choosing best alternative.
 ************************************************************************/
#define _MAX_CHOICE 8 /* we are generally checking the 8 possible adjacent moves */
typedef struct {
    point_t pos;
    int     priority;
} _choice_t, *_choice_ptr;
static     _choice_t _choices[_MAX_CHOICE];
static int _choice_ct = 0;
typedef int (_choice_cmp_f)(_choice_ptr left, _choice_ptr right);
static void _clear_choices(void)
{
    _choice_ct = 0;
}
static void _push_choice(point_t pos, int priority)
{
    assert(_choice_ct < _MAX_CHOICE);
    _choices[_choice_ct].pos = pos;
    _choices[_choice_ct].priority = priority;
    ++_choice_ct;
}
static void _sort_choices(_choice_cmp_f f)
{
    int i, j;
    for (i = 0; i < _choice_ct; i++) /* bubble sort */
    {
        for (j = _choice_ct-1; j > i; j--)
        {
            _choice_ptr c1 = &_choices[j];
            _choice_ptr c2 = &_choices[j-1];
            if (f(c1, c2) < 0)
            {
                _choice_t tmp = *c1;
                _choices[j] = *c2;
                _choices[j-1] = tmp;
            }
        }
    }
}
static int _choice_cmp_desc(_choice_ptr left, _choice_ptr right)
{
    if (left->priority > right->priority) return -1;
    if (left->priority < right->priority) return 1;
    return 0;
}
static int _choice_cmp_asc(_choice_ptr left, _choice_ptr right)
{
    if (left->priority > right->priority) return 1;
    if (left->priority < right->priority) return -1;
    return 0;
}
/************************************************************************
 * Movement: Actually do the move! Note that moving might attack instead.
 *
 * _try_move attempts to move to an adjacent position and returns TRUE
 * to stop processing this monster. Higher level AI will _try_move various
 * alternatives until a successful one is found.
 ************************************************************************/
typedef struct {
    mon_ptr         mon;
    mon_race_ptr    race;
    dun_ptr         dun;
    dun_grid_ex_t   gx;
    point_t         from, to;

    bool            do_turn;
    bool            do_move;
    bool            do_view;

    bool            asc_nerf;
    bool            can_cross;
    bool            could_splash;
    bool            must_alter_to_move;
    bool            riding;

    bool            did_move_body;
} _move_context_t, *_move_context_ptr;
static void _init_move_context(_move_context_ptr ctx, mon_ptr mon, point_t pos)
{
    memset(ctx, 0, sizeof(_move_context_t));
    ctx->mon = mon;
    ctx->race = mon->race;
    ctx->dun = mon->dun;
    ctx->gx = dun_grid_ex_at(ctx->dun, pos);
    ctx->from = mon->pos;
    ctx->to = pos;
    ctx->can_cross = mon_can_cross_illusion(mon, pos);
    if (mon->id == plr->riding)
        ctx->riding = TRUE;
}

static void _break_glyph(_move_context_ptr ctx)
{
    if (!floor_has_glyph_of_warding(ctx->gx.grid)) return;

    ctx->do_move = FALSE;
    if (mon_race_never_blow(ctx->race) && ctx->gx.plr) return;

    if (!mon_is_pet(ctx->mon) && _1d(BREAK_GLYPH) < mon_lvl(ctx->mon))
    {
        if (ctx->gx.grid->flags & CELL_MAP)
            msg_print("The rune of protection is broken!");

        dun_remove_glyph(cave, ctx->gx.pos);
        ctx->do_move = TRUE;
        ctx->do_turn = TRUE;
    }
}
static bool _is_mon_trap(_move_context_ptr ctx)
{
    if (floor_has_plr_trap(ctx->gx.grid)) return TRUE;
    if (floor_has_explosive_rune(ctx->gx.grid)) return TRUE;
    return FALSE;
}
static void _disarm_plr_trap(_move_context_ptr ctx)
{
    if (!_is_mon_trap(ctx)) return;
    if (mon_is_pet(ctx->mon)) return;

    if (floor_has_explosive_rune(ctx->gx.grid))
    {
        if (_1d(BREAK_MON_TRAP * plr->lev / 50) < mon_lvl(ctx->mon))
        {
            if (ctx->gx.grid->flags & CELL_MAP)
                msg_print("Your trap is broken!");
            dun_remove_glyph(cave, ctx->gx.pos);
        }
    }
    else if (mon_disarm_plr_trap(ctx->mon, ctx->gx.grid))
    {
        if (ctx->gx.grid->flags & CELL_MAP)
            msg_print("Your trap is broken!");
        dun_remove_plr_trap(cave, ctx->gx.pos);
    }
}
static bool _stop(_move_context_ptr ctx)
{
    if (!ctx->do_move) return TRUE;
    if (!mon_is_valid(ctx->mon)) return TRUE;
    if (!point_equals(ctx->mon->pos, ctx->from)) return TRUE; /* e.g. hit teleport trap */
    return FALSE;
}
#if FEAT_BUGZ
static bool _asc_nerf(_move_context_ptr ctx)
{
    if (mon_has_summon_spell(ctx->mon) && plr->dun_id == ctx->dun->id)
    {
        assert(cave == ctx->dun);
        if ( plr->chp > plr->mhp * 4 / 5 /* If @ wounded, pursue! */
          && !plr_at(ctx->to)
          && plr_view(ctx->to) /* stepping into los of plr */
          && plr_project(ctx->to) 
          && !plr_project(ctx->from) )
        {
            int ct_open = 0;
            int ct_enemy = 0;
            point_t d;

            /* Inspect @'s surroundings */
            for (d.y = -2; d.y <= 2; d.y++)
            {
                for (d.x = -2; d.x <= 2; d.x++)
                {
                    point_t p = point_add(plr->pos, d);
                    mon_ptr tmon;

                    if (!dun_pos_interior(ctx->dun, p)) continue;
                    if (plr_distance(p) > 2) continue;
                    if (cave_have_flag_at(p, FF_PATTERN)) continue;
                    if (point_equals(p, ctx->to)) continue;
                    if (point_equals(p, ctx->from)) continue;

                    tmon = mon_at(p);
                    if (tmon && mon_is_hostile(tmon)) ct_enemy++;
                    if (cave_empty_at(p) && plr_project(p)) ct_open++; /* XXX */
                }
            }

            if (ct_enemy)
            {
                /* If @ is in battle, join the fray! */
            }
            else if (ct_open < 1 + randint1(4))
            {
                /* not enough summoning opportunities, so hold off unless angered */
                if (!ctx->mon->anger || !one_in_(3))
                {
                    /*asc_nerf = TRUE;
                    could_splash = mon_could_splash(mon, plr->pos);*/
                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}
#endif
static void _attack(_move_context_ptr ctx)
{
    if (ctx->gx.plr) /* handles riding as well (ie ctx->gx.mon == plr_riding_mon()) */
    {
        if (mon_race_never_blow(ctx->race))
        {
            ctx->do_move = FALSE;
        }
        else if (ctx->dun->flags & DF_NO_MELEE)
        {
            if (!mon_tim_find(ctx->mon, T_CONFUSED))
            {
                if (!mon_is_stupid(ctx->mon)) ctx->do_move = FALSE;
                else mon_lore_stupid(ctx->mon);
            }
        }

        /* friendly monsters should not waste turns considering the plr's pos */
        if (mon_is_pet(ctx->mon))
        {
            /* ... on the other hand, an over-eager puppy dancing around the plr
             * can be annoying ... Heel! */
            if (ctx->mon->target_id || who_mon(plr->pet_target))
                ctx->do_move = FALSE; /* i.e. find a way around master to squirrel */
        }
        else if (!mon_is_hostile(ctx->mon) && (!ctx->gx.mon || !are_enemies(ctx->mon, ctx->gx.mon)))
        {
            ctx->do_move = FALSE;
        }

        if (ctx->do_move && (plr->special_defense & DEFENSE_INVISIBLE))
        {
            if (one_in_(2))
                ctx->do_move = FALSE;
        }

        if (ctx->do_move)
        {
            if (mon_attack(ctx->mon, ctx->to)) /* XXX checks mon_is_hostile, handles riding etc */
            {
                if (mon_is_invisible(ctx->mon) && plr->see_inv && !ctx->mon->ml)
                    update_mon(ctx->mon, FALSE);
            }
            ctx->do_turn = TRUE;
            ctx->do_move = FALSE;
        }
    }
    else if (ctx->gx.mon)
    {
        mon_race_ptr race = ctx->gx.mon->race;
        ctx->do_move = FALSE;
        assert(ctx->gx.mon->id != plr->riding); /* now handled in plr case */
        /* Wake-up 'friends' */
        if ( ctx->mon->pack /* require a pack */
          && ctx->mon->pack == ctx->gx.mon->pack /* require same pack */
          && mon_tim_find(ctx->gx.mon, MT_SLEEP) )
        {
            mon_tim_delete(ctx->gx.mon, MT_SLEEP);
            if (mon_show_msg(ctx->mon))
            {
                char name1[MAX_NLEN_MON], name2[MAX_NLEN_MON];
                monster_desc(name1, ctx->mon, 0);
                monster_desc(name2, ctx->gx.mon, 0);
                msg_format("%^s wakes %s up.", name1, name2);
            }
            ctx->do_turn = TRUE;
        }
        /* Attack 'enemies' */
        else if ( ( mon_can_trample_mon(ctx->mon) 
                 && !mon_never_blow(ctx->mon)
                 && ctx->race->mexp * ctx->race->alloc.lvl > race->mexp * race->alloc.lvl
                 && ctx->can_cross )
               || are_enemies(ctx->mon, ctx->gx.mon)
               || mon_tim_find(ctx->mon, T_CONFUSED) )
        {
            if (!mon_never_blow(ctx->mon))
            {
                if (mon_can_trample_mon(ctx->mon))
                    mon_lore_trample_mon(ctx->mon);

                if (mon_attack(ctx->mon, ctx->to))
                {
                    ctx->do_turn = TRUE;
                }
                else if (ctx->dun->flags & DF_NO_MELEE)
                {
                    if (mon_is_stupid(ctx->mon))
                        mon_lore_stupid(ctx->mon);
                    ctx->do_turn = TRUE;
                }
            }
        }
        /* Push past weaker monsters ... */
        else if ( mon_race_can_push_mon(ctx->race)
               && !mon_race_never_move(ctx->race)
               && ctx->race->mexp > race->mexp
               && ctx->can_cross
               && mon_can_cross(ctx->gx.mon, ctx->from) ) /* ... unless leaving a wall, etc */
        {
            ctx->do_move = TRUE;
            ctx->did_move_body = TRUE;
            mon_tim_remove(ctx->gx.mon, MT_SLEEP);
        }
    }
}
static void _gothmog(_move_context_ptr ctx)
{
    if (ctx->race->alloc.lvl > 70 && mon_race_can_tunnel(ctx->race))
    {
        int dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t pos = point_step(ctx->to, ddd[dir]);
            dun_cell_ptr cell;

            if (!dun_pos_interior(ctx->dun, pos)) continue;

            cell = dun_cell_at(ctx->dun, pos);
            if (cell->flags & CELL_PERM) continue;

            if (mon_race_is_(ctx->mon->race, "U.Gothmog"))
            {
                if (!lava_is_deep(cell))
                    dun_place_shallow_lava(ctx->dun, pos); /* cf _lava_flags */
            }
            else
            {
                /* XXX Note that dun->type->place_floor will reset CELL_LIT, even
                 * if the existing tile is already a floor (cf CELL_CHANGE_MASK) */
                cell_affect(ctx->dun, pos, cell, GF_DISINTEGRATE, 0);
            }
        }
        if (mon_race_is_(ctx->mon->race, "U.Gothmog"))
        {
            dun_cell_ptr cell = dun_cell_at(ctx->dun, ctx->to);
            if (!(cell->flags & CELL_PERM)) /* paranoia */
                dun_place_deep_lava(ctx->dun, ctx->to);
        }
        plr->update |= PU_FLOW | PU_MON_FLOW;
    }
}
static void _ent(_move_context_ptr ctx)
{
    if ( mon_race_is_(ctx->mon->race, "#.Fangorn")
      || mon_race_is_(ctx->mon->race, "#.ent") )
    {
        int dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t pos = point_step(ctx->to, ddd[dir]);
            dun_cell_ptr cell;

            if (!dun_pos_interior(ctx->dun, pos)) continue;
            cell = dun_cell_at(ctx->dun, pos);

            if (!cell_is_floor(cell)) continue;

            if (floor_is_floor(cell) || floor_is_dirt(cell))
            {
                switch (randint1(5))
                {
                case 1: dun_place_flower(ctx->dun, pos); break;
                case 2: dun_place_brake(ctx->dun, pos); break;
                default: dun_place_grass(ctx->dun, pos);
                }
            }
        }
        if (cell_is_floor(dun_cell_at(ctx->dun, ctx->to)))
            dun_place_tree(ctx->dun, ctx->to);
    }
}
static void _spider(_move_context_ptr ctx)
{
    if ( mon_race_is_(ctx->mon->race, "S.Ungoliant")
      || mon_race_is_(ctx->mon->race, "S.Atlach-Nacha") )
    {
        int dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t pos = point_step(ctx->to, ddd[dir]);
            dun_cell_ptr cell;

            if (!dun_pos_interior(ctx->dun, pos)) continue;
            cell = dun_cell_at(ctx->dun, pos);

            if (!cell_is_floor(cell)) continue;
            if (!floor_has_web(cell) && _1d(150) <= mon_lvl(ctx->mon))
                dun_place_web(ctx->dun, pos);
        }
        if (cell_is_floor(dun_cell_at(ctx->dun, ctx->to)))
            dun_place_web(ctx->dun, ctx->to);
    }
}
static bool _diagonal(_move_context_ptr ctx)
{
    point_t v = point_subtract(ctx->to, ctx->from);
    return v.x && v.y;
}
static bool _actually_move(_move_context_ptr ctx)
{
    if (!ctx->riding)
    {
        dun_move_mon(ctx->dun, ctx->mon, ctx->to);
        cell_accept_mon(ctx->dun, ctx->to, ctx->gx.grid, ctx->mon);
        if (!mon_is_valid(ctx->mon)) return FALSE;

        /* XXX (Proposed): Diagonal movement costs 50% more energy */
        if (_diagonal(ctx))
            ctx->mon->energy_need += ENERGY_NEED() / 2;

        if (mon_move_quick(ctx->mon))
        {
            ctx->mon->energy_need -= ENERGY_NEED() / 2;
            mon_lore_move_quick(ctx->mon);
        }

        /* Some classes have techniques to apply whenever a monster moves too close */
        plr_hook_move_monster(ctx->mon);
        if (!mon_is_valid(ctx->mon)) return FALSE;
    }
    else
    {
        /* Moving the player will handle moving mon as well */
        if (!move_player_effect(ctx->to, MPE_DONT_PICKUP)) return FALSE;
    }
    return TRUE;
}
static void _disturb(_move_context_ptr ctx)
{
    mon_race_ptr race = ctx->mon->apparent_race;
    if (ctx->mon->ml &&
        (disturb_move ||
         (ctx->mon->cdis <= 2 && plr_view(ctx->mon->pos)) ||
         (disturb_near && plr_view(ctx->mon->pos)) ||
         (disturb_high && race->lore.kills.total && race->alloc.lvl >= plr->lev)))
    {
        if (town_no_disturb && plr_in_town() && ctx->race->alloc.lvl == 0)
        {
        }
        else if (mon_is_hostile(ctx->mon))
            disturb(0, 0);
    }
}
static void _try_move_aux(_move_context_ptr ctx)
{
    ctx->do_move = TRUE; /* early exit once do_move turns false */
    ctx->do_turn = FALSE;

    /* BLOCK: Attack if possible, handling Glyphs of Warding.
     * Note that stationary monsters can attack adjacent foes. */
    _break_glyph(ctx);
    if (_stop(ctx)) return; /* unable to break Glyph of Warding */

    _attack(ctx);
    if (_stop(ctx)) return; /* attacked, possibly dead as well */

    /* BLOCK: Handle monsters unable to move. This includes the player's
     * mount if he has a tight hold on the reins. */
    if (ctx->riding)
    {
        if (!plr->riding_ryoute && !mon_tim_find(ctx->mon, T_FEAR))
            ctx->do_move = FALSE;
    }
    if (mon_tim_find(ctx->mon, MT_BOUND))
    {
        ctx->do_move = FALSE;
        ctx->do_turn = TRUE;
    }
    if (mon_never_move(ctx->mon))
    {
        ctx->do_move = FALSE;
        ctx->do_turn = TRUE;
        mon_lore_never_move(ctx->mon);
        if (mon_is_invisible(ctx->mon) && plr->see_inv)
            update_mon(ctx->mon, FALSE);
    }
    if (_stop(ctx)) return; /* unable to move */

    /* BLOCK: Check for legal terrain, possibly altering (OPEN|BASH|DISARM) */
    if ( wall_has_secret_door(ctx->gx.grid)
      && !ctx->can_cross
      && !mon_is_stupid(ctx->mon)
      && (mon_can_bash_door(ctx->mon) || mon_can_open_door(ctx->mon)) )
    {
        dun_place_closed_door(ctx->dun, ctx->to);
        dun_note_pos(ctx->dun, ctx->to);
        dun_draw_pos(ctx->dun, ctx->to);
    }
    if (door_is_closed(ctx->gx.grid) && !ctx->can_cross)
    {
        switch (dun_open_mon(ctx->dun, ctx->to, ctx->mon))
        {
        case ACTION_SUCCESS:
            ctx->do_turn = TRUE;
            ctx->can_cross = mon_can_cross(ctx->mon, ctx->to);
            break;
        case ACTION_ABORT:
            ctx->do_move = FALSE;
            break;
        case ACTION_FAIL:
            ctx->do_move = FALSE;
            ctx->do_turn = TRUE;
            break;
        }
    }
    else _disarm_plr_trap(ctx);
    if (!ctx->can_cross)
        ctx->do_move = FALSE;
    if (_stop(ctx)) return; /* illegal move or failed|unable to OPEN|BASH door */

    /* BLOCK: Perform the move. Stop using _stop! */
    ctx->do_turn = TRUE;
    if (!_actually_move(ctx)) return; /* dead monster */
    if (!point_equals(ctx->mon->pos, ctx->to)) return; /* teleported by PLR_TRAP? */
          /* Anyway, we shouldn't pickup|destroy objects if mon no longer there. */

    /* BLOCK: After effects, including PICKUP|DESTROY objects and some
     * enhanced terrain alterations */
    _disturb(ctx);
    if (mon_can_destroy_obj(ctx->mon))
        dun_mon_destroy(ctx->dun, ctx->mon);
    else if (mon_can_pickup_obj(ctx->mon))
        dun_mon_pickup(ctx->dun, ctx->mon);
    _gothmog(ctx);
    _ent(ctx);
    _spider(ctx);
}
static bool _try_move(mon_ptr mon, point_t pos)
{
    _move_context_t ctx;

    assert(point_is_adjacent(mon->pos, pos)); /* did you mean _move_towards? */

    if (!mon_is_valid(mon)) return FALSE;
    if (!dun_pos_interior(mon->dun, pos)) return FALSE;
    if (point_equals(mon->pos, pos)) return FALSE; /* paranoia */

    /* prepare for battle */
    _init_move_context(&ctx, mon, pos);

    _try_move_aux(&ctx);

    /* the after math */
    if (ctx.did_move_body) mon_lore_push_mon(ctx.mon);
    if (ctx.do_view)
    {
        plr->update |= PU_FLOW;
        plr->window |= PW_OVERHEAD | PW_DUNGEON;
    }
    return ctx.do_turn;
}
/************************************************************************
 * Movement AI: Utilities
 ************************************************************************/
static bool _ignore_plr(mon_ptr mon)
{
    /* XXX Should AI consider the plr? Currently, the answer is
     * generally yes, unless a Ninja (or Burgler) is Hiding in Shadows.
     * But I can think of more reasons to apply this check, such
     * as delayed monster awareness of plr, or plr illusions. At any rate,
     * this routine gives an easy way to tell the AI to ignore the plr. */
    /* if (!(mon->mflag2 & MFLAG2_AWARE_PLR)) return TRUE; */
    if (mon->mflag & MFLAG_IGNORE_PLR) return TRUE;
    return FALSE;
}
static bool _drunken(mon_ptr mon)
{
    /* a "drunken" monster staggers about randomly */
    mon_race_ptr race;

    if (mon_race_is_(mon->race, "@.player")) return FALSE; /* XXX */
    if (mon_tim_find(mon, T_CONFUSED)) return TRUE;
    /* XXX MFLAG_IGNORE_PLR should not process here. Instead, they can counterattack
     * mon->target_pos, or _find_enemy() or flow to a pack goal (AI_WANDER or AI_HUNT).
     * _move_drunken is a last resort (cf _move_hostile) */

    race = mon->race;

    /* Erratic Movement */
    if (race->move.random && randint0(100) < race->move.random)
    {
        /* mon_lore ??? */
        return TRUE;
    }

    /* Can't reach player - find something else to hit */
    if (mon_race_never_move(race) && mon->cdis > 1) return TRUE;

    return FALSE;
}
static bool _pack_will_run(mon_ptr mon)
{
    if (!mon->pack) return FALSE;
    if (mon_is_unique(mon) && mon_is_pet(mon)) return FALSE;
    if (mon->pack->ai == AI_FEAR)
    {
        int odds = 500 - mon->cdis * mon->cdis;
        if (odds <= 1 || one_in_(odds))
            mon->pack->ai = AI_SEEK;
        else if (mon_dis(mon) > 5)
            return TRUE;
    }
    else if (mon->pack->ai == AI_MAINTAIN_DISTANCE && mon->dun->id == plr->dun_id)
    {
        if ( 1 < mon->cdis && mon->cdis <= mon->pack->distance
          && plr->chp >= plr->mhp * 4 / 5 ) /* If @ wounded, pursue! */
        {
            return TRUE;
        }
    }
    return FALSE;
}
static bool _will_run(mon_ptr mon)
{
    /*return plr->wizard;*/
    if (mon_tim_find(mon, T_BERSERK)) return FALSE; /* note T_BERSERK and T_FEAR should never happen */
    if (mon_race_is_(mon->race, "@.player")) return FALSE; /* XXX */
    if (mon_tim_find(mon, T_FEAR)) return TRUE;
    if (mon_will_run(mon)) return TRUE;
    if (_pack_will_run(mon)) return TRUE;
    return FALSE;
}
static bool _try_choices(mon_ptr mon)
{
    int i;
    for (i = 0; i < _choice_ct; i++)
    {
        point_t p = _choices[i].pos;
        if (_try_move(mon, p)) return TRUE;
    }
    return FALSE;
}
/************************************************************************
 * Movement AI: Think about where to move and try various alternatives.
 *
 * Level 1: Movement in a direction (_move_towards and _move_away). This
 * is the most naive strategy since it does not consider pathfinding. We
 * end up here when monsters gain a direct path to their target (mon_project).
 ************************************************************************/
static bool _move_towards(mon_ptr mon, point_t pos)
{
    int mm[8] = {0};
    int ct = point_step_dirs(mon->pos, pos, mm);
    int i;
    for (i = 0; i < ct; i++)
    {
        int d = mm[i];
        point_t p = point_step(mon->pos, d);
        if (_try_move(mon, p)) return TRUE;
    }
    return FALSE;
}
static bool _move_away(mon_ptr mon, point_t pos)
{
    int  mm[8] = {0};
    int  ct = point_step_dirs(pos, mon->pos, mm);
    bool avoid_plr = FALSE;
    int  i;

    /* The following tweak allows a hidden monster to remain hidden */
    if ( mon_is_hostile(mon)
      && (mon->mflag & MFLAG_WILL_RUN)
      && plr->dun_id == mon->dun->id
      && !plr_project_mon(mon)
      && !mon_tim_find(mon, T_BLIND)      /* requires knowledge of plr->pos */
      && !mon_tim_find(mon, T_CONFUSED) ) /* this is intelligent behaviour */
    {
        avoid_plr = TRUE;
    }

    for (i = 0; i < ct; i++)
    {
        int d = mm[i];
        point_t p = point_step(mon->pos, d);
        if (avoid_plr && plr_project(p)) continue;
        if (_try_move(mon, p)) return TRUE;
    }
    if (avoid_plr) return TRUE; /* our best option is to stay hidden */
    return FALSE;
}
static bool _move_drunken(mon_ptr mon)
{
    int i;
    for (i = 0; i < 4; i++)
    {
        int d = ddd[randint0(8)];
        point_t p = point_step(mon->pos, d);
        if (_try_move(mon, p)) return TRUE;
    }
    return FALSE;
}
static bool _move_blind(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    if (!dun_pos_interior(dun, mon->last_enemy_pos)) return _move_drunken(mon);
    if ((mon->mflag & MFLAG_WILL_RUN) && _move_away(mon, mon->last_enemy_pos)) return TRUE;
    else if (_move_towards(mon, mon->last_enemy_pos)) return TRUE;
    return _move_drunken(mon);
}
/************************************************************************
 * Movement AI: Think about where to move and try various alternatives.
 *
 * Level 2: Pathfinding using a flow (_flow_towards and _flow_away). Flows
 * enable pathfinding. Various kinds of flows include:
 *   [1] dun->flow: find the plr (perhaps via stairs)
 *   [2] mon->flow: hunt a prey
 *   [3] stairs->flow: flee the level
 * flow->pos is the "goal" of the flow. When a monster gains direct access
 * to the goal, we can revert to Level 1 movement ai: _move_towards(flow->pos).
 * Otherwise, we try alternatives in their best order using _try_choices.
 ************************************************************************/
static bool _is_closed_door(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    return door_is_closed(cell);
}
static bool _move_spellcaster(mon_ptr mon, dun_flow_ptr flow)
{
    /* Look for farthest projectable pos from target. Avoid cutting corners
     * into a waiting player, but back up and Breathe Death instead. This routine
     * handles intelligently stepping into view of target. This is basically
     * an alternative flow AI to _move_flow */
    if (!mon_project(mon, flow->pos)) /* skip if already castable */
    {
        dun_ptr      dun = mon->dun;
        bool         can_open_door = mon_can_open_door(mon) || mon_can_bash_door(mon); 
        bool         ignore_walls = mon_ignore_walls(mon);
        int          current_c = dun_flow_at(flow, mon->pos);
        int          i;

        if (current_c == DUN_FLOW_NULL) return FALSE; /* skip if no flow data */
        _clear_choices();
        for (i = 7; i >= 0; i--)
        {
            point_t p = point_step(mon->pos, ddd[i]);
            int     c;

            if (!dun_pos_interior(dun, p)) continue;
            if (point_equals(p, flow->pos)) return FALSE;  /* XXX should never happen */

            c = dun_flow_at(flow, p);
            if (!ignore_walls)
            {
                if (c == DUN_FLOW_NULL) continue;
                if (!can_open_door && _is_closed_door(dun, p)) continue;
            }
            else if (!c) c = 998; /* passwall */

            if (current_c < c) continue;
            if (!point_project_aux(p, flow->pos, PROJECT_ILLUSION)) continue;
            _push_choice(p, c);
        } 
        _sort_choices(_choice_cmp_desc); /* want highest cost projectable square */
        return _try_choices(mon);
    }
    return FALSE;
}
static bool _direct_path_aux(mon_ptr mon, line_gen_ptr gen)
{
    dun_ptr      dun = mon->dun;

    line_gen_first(gen); /* skip initial point */
    for (;;)
    {
        point_t pos = line_gen_next(gen);

        if (!dun_pos_interior(dun, pos)) break;

        if (point_equals(pos, gen->l.b))
            return TRUE;

        if (line_gen_distance(gen) >= 2*DUN_VIEW_MAX) break;
        if (!mon_can_cross_illusion(mon, pos))
            break;
    }
    return FALSE;
}
static bool _direct_path(mon_ptr mon, point_t pos)
{
    line_gen_t gen;
    int        d = point_fast_distance(mon->pos, pos);
    bool       b;

    if (d > 2*DUN_VIEW_MAX) return FALSE;
    if (d < 2) return TRUE;

    line_gen_create(&gen, line_create(mon->pos, pos));
    b = _direct_path_aux(mon, &gen);
    line_gen_destroy(&gen);
    return b;
}
static bool _mon_ignore_flow(mon_ptr mon, dun_flow_ptr flow)
{
    if (!mon_will_flow(mon)) return TRUE;
    /* this is a very common case */
    if (mon->dun->id == plr->dun_id && point_equals(plr->pos, flow->pos) && plr_view(mon->pos))
        return TRUE;
    /* we used to "_project" ... but consider forests on D_SURFACE */
    return _direct_path(mon, flow->pos);
}
static bool _flow_towards(mon_ptr mon, dun_flow_ptr flow)
{
    dun_ptr dun = mon->dun;
    bool    bash = mon_can_open_door(mon) || mon_can_bash_door(mon); 
    bool    move = mon_can_push_mon(mon) || mon_can_trample_mon(mon);
    bool    ignore_web = mon_ignore_webs(mon);
    bool    fear = BOOL(mon->mflag & MFLAG_WILL_RUN);
    int     current_c, i;

    if (mon->dun != flow->dun) return FALSE; /* paranoia */

    if (_mon_ignore_flow(mon, flow))
        return _move_towards(mon, flow->pos);

    current_c = dun_flow_at(flow, mon->pos);
    if (current_c == DUN_FLOW_NULL) return FALSE; /* no flow data (e.g. plr hiding in walls) */

    /* spellcaster stepping into LOS of player. we need to make sure that
     * flow is for the actual plr rather than a monster, position or stairs */
    if ( flow->dun->id == plr->dun_id  /* plr presence on lvl */
      && point_equals(flow->pos, plr->pos) /* plr is goal of flow */
      && mon_has_attack_spell(mon)           /* bold, hostile spellcaster */
      && mon_is_hostile(mon)
      && !fear )
    {
        if (_move_spellcaster(mon, flow))
            return TRUE;
    }

    _clear_choices();
    for (i = 0; i < 8; i++) /* diagonal moves cost more energy: favor cardinal directions */
    {
        point_t p = point_step(mon->pos, ddd[i]);
        int     c;

        if (!dun_pos_interior(dun, p)) continue;

        c = dun_flow_at(flow, p);
        if (c == DUN_FLOW_NULL) continue;
        if (!move && dun_mon_at(dun, p)) continue;

        if (c >= current_c) continue; /* want *strictly* lower cost */

        if (!bash || !ignore_web)
        {
            dun_grid_ptr g = dun_grid_at(dun, p);
            if (!bash && door_is_closed(g)) c += 3;
            if (!ignore_web && floor_has_web(g)) c += 5;
        }
        if (fear && plr->dun_id == mon->dun->id)
        {
            if (point_equals(plr->pos, p)) c += 50;
            else if (point_is_adjacent(plr->pos, p)) c += 10;
        }
        _push_choice(p, c);
    }
    _sort_choices(_choice_cmp_asc); /* want lowest cost */
    return _try_choices(mon);
}
static bool _flow_away(mon_ptr mon, dun_flow_ptr flow)
{
    dun_ptr      dun = mon->dun;
    bool         bash = mon_can_open_door(mon) || mon_can_bash_door(mon); 
    bool         move = mon_can_push_mon(mon) || mon_can_trample_mon(mon);
    bool         fear = BOOL(mon->mflag & MFLAG_WILL_RUN);
    bool         fear_plr = fear && plr->dun_id == mon->dun->id && point_equals(flow->pos, plr->pos);
    int          current_c, i;

    if (mon->dun != flow->dun) return FALSE; /* paranoia */

    if (!mon_will_flow(mon))
        return _move_away(mon, flow->pos);

    current_c = dun_flow_at(flow, mon->pos);
    if (current_c == DUN_FLOW_NULL) return FALSE; /* no flow data (e.g. plr hiding in walls) */
    
    _clear_choices();
    for (i = 0; i < 8; i++) /* diagonal moves cost more energy: favor cardinal directions */
    {
        point_t p = point_step(mon->pos, ddd[i]);
        int     c;

        if (!dun_pos_interior(dun, p)) continue;

        c = dun_flow_at(flow, p);
        if (c == DUN_FLOW_NULL) continue;
        if (!move && dun_mon_at(dun, p)) continue;

        if (!bash)
        {
            dun_grid_ptr g = dun_grid_at(dun, p);
            if (door_is_closed(g)) c += 3;
        }
        if (fear_plr && point_project_aux(plr->pos, p, PROJECT_ILLUSION)) continue; /* use _find_safety if no safe flow available */
        if (c < current_c) continue;  /* want highest cost */
        _push_choice(p, c);
    }
    _sort_choices(_choice_cmp_desc); /* want highest cost */
    return _try_choices(mon);
}
/************************************************************************
 * Movement AI: Target Acquisition
 ************************************************************************/
static vec_ptr _enemies(mon_ptr mon)
{
    vec_ptr            v = vec_alloc(NULL);
    dun_ptr            dun = mon->dun;
    mon_race_ptr       race = mon->race;
    point_map_iter_ptr iter;

    for (iter = point_map_iter_alloc(dun->mon_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        mon_ptr tgt_mon = point_map_iter_current(iter);
        
        if (tgt_mon == mon) continue;
        if (mon_is_pet(mon))
        {
            if (plr->pet_follow_distance < 0) /* Only fight away from player */
            {
                int rng = -plr->pet_follow_distance;
                if (tgt_mon->cdis <= rng) continue;
            }
            else if (mon->cdis < tgt_mon->cdis && tgt_mon->cdis > plr->pet_follow_distance)
                continue; /* No fighting away from player */
            if (race->move.range < tgt_mon->cdis) continue; /* stay within aaf of master */
        }
        if (point_fast_distance(mon->pos, tgt_mon->pos) > MAX_SIGHT) continue;
        if (!are_enemies(mon, tgt_mon)) continue;

        if (mon_ignore_walls(mon))
        {
            if (!dun_in_disintegration_range(dun, mon->pos, tgt_mon->pos))
                continue;
        }
        else if (!mon_project(mon, tgt_mon->pos)) continue;
        vec_add(v, tgt_mon);
    }
    point_map_iter_free(iter);
    return v;
}
static int _cmp_enemy(mon_ptr m1, mon_ptr m2)
{
    int d1, d2;
    assert(_current);
    /* hostiles prefer the plr's pets */
    if (mon_is_pet(m1) && !mon_is_pet(m2)) return -1;
    if (!mon_is_pet(m1) && mon_is_pet(m2)) return 1;
    /* XXX Alignment? */
    /* prefer closest */
    d1 = point_fast_distance(_current->pos, m1->pos);
    d2 = point_fast_distance(_current->pos, m2->pos);
    if (d1 < d2) return -1;
    if (d1 > d2) return 1;
    return 0;
}
static int _cmp_enemy_friendly(mon_ptr m1, mon_ptr m2)
{
    int d1, d2;
    assert(_current);
    /* friends prefer hostiles */
    if (mon_is_hostile(m1) && !mon_is_hostile(m2)) return -1;
    if (!mon_is_hostile(m1) && mon_is_hostile(m2)) return 1;
    /* XXX Alignment? */
    /* prefer closest */
    d1 = point_fast_distance(_current->pos, m1->pos);
    d2 = point_fast_distance(_current->pos, m2->pos);
    if (d1 < d2) return -1;
    if (d1 > d2) return 1;
    return 0;
}
static mon_ptr _closest_enemy(mon_ptr mon)
{
    mon_ptr foe = NULL;
    vec_ptr v;
    assert(_current == mon);
    v = _enemies(mon);
    if (vec_length(v))
    {
        if (mon_is_hostile(mon))
            vec_sort(v, (vec_cmp_f)_cmp_enemy);
        else
            vec_sort(v, (vec_cmp_f)_cmp_enemy_friendly);
        foe = vec_get(v, 0);
    }
    vec_free(v);
    return foe;
}
#if 0
static mon_ptr _random_enemy(mon_ptr mon)
{
    vec_ptr v = _enemies(mon);
    mon_ptr foe = vec_random(v);
    vec_free(v);
    return foe;
}
#endif
static mon_ptr _find_enemy(mon_ptr mon)
{
    mon_ptr foe = NULL;

    /* plr and steed fight as a team! */
    if (mon->id == plr->riding && who_is_mon(plr->riding_target))
        foe = who_mon(plr->riding_target);

    /* other pets prefer the pet target */
    else if (mon_is_pet(mon) && who_is_mon(plr->pet_target))
        foe = who_mon(plr->pet_target);

    if (foe && foe->dun == mon->dun)
        return foe;

    /* everything else gets closest foe ... this is for movement, not spellcasting */
    foe = _closest_enemy(mon);
    return foe;
}
static bool _pack_allow_target(mon_ptr mon)
{
    if (!mon->pack) return TRUE;
    if (mon->pack->ai == AI_GUARD_MON) return FALSE;
    if (mon->pack->ai == AI_GUARD_POS) return FALSE;
    return TRUE;
}
static bool _acquire_target(mon_ptr mon)
{
    mon_ptr foe = _find_enemy(mon);
    if (foe && _move_towards(mon, foe->pos))
    {
        if ( _pack_allow_target(mon)  /* some pack strategies should not lock on to a target */
          && !(mon_is_pet(mon) && mon_is_pet(foe)) ) /* pets can fight each other, but don't "lock on" */
        {
            mon->target_id = foe->id; /* lock on to target (next _move will prefer _move_target) */
        }
        return TRUE;
    }
    return FALSE;
}
/************************************************************************
 * Movement AI: Pack AI
 ************************************************************************/
static bool _lure(mon_ptr mon)
{
    dun_ptr      dun = mon->dun;
    int          i, open = 0, score;
    
    assert(dun->id == plr->dun_id);

    if (_ignore_plr(mon)) return FALSE;

    /* attempt to lure plr out into the open */
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(plr->pos, ddd[i]);
        dun_grid_ptr g;

        if (!dun_pos_interior(dun, p)) continue;
        g = dun_grid_at(dun, p);
        if (g->flags & CELL_ROOM) open++;
        else if (illusion_allow_mon(g, mon)) open++;
    }
    if (!mon->race->spells) open -= 2;

    score = 8 * (plr->chp + plr->csp)/(plr->mhp + plr->msp);
    if (open < score)
    {
        point_t pos = _find_hiding(mon);
        if (dun_pos_interior(dun, pos))
            return _move_towards(mon, pos);
    }
    /* XXX consider refactoring _surround code
     * return _surround(mon) */
    return FALSE;
}
static bool _move_pack(mon_ptr mon)
{
    dun_ptr dun = mon->dun;

    if (!mon->pack) return FALSE;
    if (mon->mflag & MFLAG_WILL_RUN) return FALSE;
    if (mon->dun->id != plr->dun_id)
    {
        /* Here is a weird case: A pack of Hounds of Tindalos (AI_SHOOT)
         * off level, sought the plr's stairs. When plr entered level, they
         * froze in place, but this place happened to be inside walls! They were
         * actually flowing to the stairs but got interrupted en route. They never
         * should have moved to begin with. Likewise, monsters using flows should
         * not leave the level (e.g. AI_PATROL, AI_HUNT, AI_WANDER). Nor should
         * monster's guarding a position. */
        switch (mon->pack->ai)
        {
        case AI_SEEK:
        case AI_LURE:
        case AI_MAINTAIN_DISTANCE:
            /* flow to stairs, enter plr's level, then resume ai */
            return FALSE; 
            /* XXX Note this is exploitable. Plr can take stairs and then wait
             * for monster to catch up using "whack a mole" tactics. XXX */
        }
    }

    if (mon->pack->ai == AI_HUNT)
    {
        mon_ptr prey = dun_mon(dun, mon->pack->prey_id);

        /* acquire new prey if current dead (or escaped level) */
        if (!mon_is_valid(prey))
            prey = mon_pack_hunt(mon->pack); /* might switch to AI_WANDER or AI_SEEK */

        /* acquire new prey if pathfinding won't work. this is a rare case: e.g. Gandalf
         * teleported his prey away in the Dragons' Lair, and it landed inside a walled
         * up chasm. */
        if (prey && prey->flow && dun_flow_at(prey->flow, mon->pos) == DUN_FLOW_NULL)
            prey = mon_pack_hunt(mon->pack);

        if (prey)
        {
            /* prey in view is best option */
            if (mon_project(mon, prey->pos)) return _move_towards(mon, prey->pos);

            /* plr in view always interrupts the hunt */
            if (mon_is_hostile(mon) && !_ignore_plr(mon))
            {
                if (mon_project_plr(mon) && _move_towards(mon, plr->pos)) return TRUE;
            }

            /* enemy in view? perhaps we should see how far away they are first.
             * perhaps we should compare cost of target to current cost using pet->flow? */
            if (one_in_(5) && _acquire_target(mon)) return TRUE;

            /* pathfind to prey */
            if (prey->flow && _flow_towards(mon, prey->flow)) return TRUE;

            return _move_towards(mon, prey->pos);
        }
    }
    if (mon->pack->ai == AI_WANDER)
    {
        if (!mon->pack->flow || point_equals(mon->pos, mon->pack->flow->pos)) /* pick a new goal */
        {
            mon_pack_wander(mon->pack);
            if (!mon->pack->flow) return FALSE; /* unable to pick a goal */
        }

        /* allow wanderer to get distracted en route to goal.
         * unlike hunters, wanderers are easily distracted. */
        if (mon_is_hostile(mon) && !_ignore_plr(mon))
        {
            if (mon_project_plr(mon) && _move_towards(mon, plr->pos)) return TRUE;
        }
        if (_acquire_target(mon)) return TRUE;

        /* continue towards goal */
        return _flow_towards(mon, mon->pack->flow);
    }

    if (!mon_is_hostile(mon)) return FALSE;
    if (mon->pack->ai == AI_LURE && !mon_ignore_walls(mon) && _lure(mon)) return TRUE; /* need to _surround when _lure not needed ... see below */
    if (mon->pack->ai == AI_SHOOT && mon_dis(mon) > 1) return TRUE; /* i.e. don't move! */
    if (mon->pack->ai == AI_GUARD_POS)
    {
        if (mon_dis(mon) > 3 && mon->anger < 10)
            return _move_towards(mon, mon->pack->pos);
    }
    if (mon->pack->ai == AI_GUARD_MON)
    {
        mon_ptr guard_mon = mon_pack_mon(mon->pack, mon->pack->guard_id);
        if (!mon_is_valid(guard_mon)) /* guard monster removed or slain */
            mon->pack->ai = AI_SEEK;
        else if (mon == guard_mon)
            return FALSE; /* hey, that's me! */
        else if (guard_mon->dun != mon->dun && guard_mon->dun->id == plr->dun_id)
            return _flow_towards(mon, dun->flow); /* follow charge off level */
        else if (mon_is_hostile(mon) && mon_dis(mon) <= 3 && !_ignore_plr(mon))
            return FALSE; /* go after @ */
        else
            return _move_towards(mon, guard_mon->pos); /* protect and defend! */
    }
    if (mon->pack->leader_id == mon->id || mon_pack_count(mon->pack) < 4) return FALSE; /* abandon further pack AI */
    if (_ignore_plr(mon)) return FALSE;

    /* surround the plr */
    if (mon->dun->id == plr->dun_id && mon->cdis < 3 && !mon_never_move(mon))
    {
        int i;
        for (i = 0; i < 8; i++)
        {
            int     j = (mon->id + i) & 7; /* semi-random direction */
            point_t p = point_step(plr->pos, ddd[j]);
            if (!dun_pos_interior(dun, p)) continue;
            if (point_equals(mon->pos, p) && _try_move(mon, plr->pos)) return TRUE; /* attack plr */
            if (mon_can_enter(mon, p) && _move_towards(mon, p)) return TRUE; /* move here ... but don't jump here :) */
        }
    }
    return FALSE;
}
/************************************************************************
 * Movement AI: Pet AI
 ************************************************************************/
static bool _move_pet(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    if (!mon_is_pet(mon)) return FALSE; /* paranoia */

    /* try to obey master and keep off */
    if ( mon->dun->id == plr->dun_id
      && plr->pet_follow_distance < 0
      && mon_dis(mon) <= -plr->pet_follow_distance )
    {
        if (_flow_away(mon, dun->flow)) return TRUE;
        if (_move_away(mon, plr->pos)) return TRUE;
    }

    if (!(mon->mflag & MFLAG_WILL_RUN))
    {
        /* try to obey master and hunt a specific prey. the goal here
         * is pathfinding, and this won't be required if master holds
         * the reins! */
        if (who_is_mon(plr->pet_target) && (mon->id != plr->riding || !plr->ryoute))
        {
            mon_ptr foe = who_mon(plr->pet_target);
            if (foe->dun == mon->dun)
            {
                if (mon_will_flow(mon) && foe->flow && _flow_towards(mon, foe->flow)) return TRUE;
                return _move_towards(mon, foe->pos);
            }
        }

        /* look for squirrels! */
        if (_acquire_target(mon)) return TRUE;
    }

    /* where did master go to? note that a frightened doggie always
     * heads for the safety of master (whimpering with tucked tail) */
    if (_flow_towards(mon, dun->flow)) return TRUE;
    if (_move_towards(mon, dun->flow_pos)) return TRUE; /* passwall Dread! */

    /* chase tail! */
    return _move_drunken(mon);
}
/************************************************************************
 * Movement AI: Run Away
 ************************************************************************/
static bool _flow_stairs(mon_ptr mon)
{
    dun_ptr        dun = mon->dun;
    dun_stairs_ptr stairs, best_stairs = NULL;
    int            best_d = 9999;
    bool           no_flow = !mon_will_flow(mon);

    /* some monsters are not allowed to leave the level */
    if (mon->pack)
    {
        if (mon->pack->ai == AI_WANDER || mon->pack->ai == AI_HUNT)
            return FALSE; /* cf _try_stairs */
    }

    /* pathfind to nearest staircase */
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        int d;
        /* avoid plr level. note, if we flee the plr level, we would immediately
         * return without this check */
        if (plr->dun_id == stairs->dun_id) continue;
        /* passwall monsters do not pathfind ... pick closest staircase and _move_towards(that) */
        if (no_flow)
        {
            d = point_fast_distance(mon->pos, stairs->pos_here);
            if (d > 30) continue;
            if (d > best_d) continue;
            best_d = d;
            best_stairs = stairs;
        }
        else
        {
            if (point_fast_distance(mon->pos, stairs->pos_here) > 22) continue; /* XXX */
            if (!stairs->flow)
                stairs->flow = dun_flow_calc(dun, stairs->pos_here, 15, NULL);
            d = dun_flow_at(stairs->flow, mon->pos);
            if (d == DUN_FLOW_NULL) continue; /* too distant or not reachable */
            assert(DUN_FLOW_NULL >= d); /* XXX otherwise, need to check null below */
            if (dun_flow_at_plr(stairs->flow) < d) continue; /* check if plr can cut us off */
            if (d < best_d)
            {
                best_stairs = stairs;
                best_d = d;
            }
        }
    }

    if (best_stairs)
    {
        if (no_flow) return _move_towards(mon, best_stairs->pos_here);
        assert(best_stairs->flow);
        if (_flow_towards(mon, best_stairs->flow)) return TRUE;
    }
    return FALSE;
}
static bool _run(mon_ptr mon)
{
    if (!(mon->mflag & MFLAG_WILL_RUN)) return FALSE;
    if (mon_is_hostile(mon))
    {
        dun_ptr dun = mon->dun;
        point_t pos;
        /* go for nearest stairs */
        if (_flow_stairs(mon)) return TRUE;
        /* flee current target */
        if (mon->target_id)
        {
            mon_ptr target = dun_mon(dun, mon->target_id);
            if (target && _move_away(mon, target->pos)) return TRUE;
        }
        else if (dun_pos_interior(dun, mon->target_pos))
        {
            if (_move_away(mon, mon->target_pos)) return TRUE;
        }
        /* flee the plr */
        if (!_ignore_plr(mon))
        {
            if (_flow_away(mon, dun->flow)) return TRUE;
            if (plr->dun_id == dun->id) /* plr presence induces ducking out of plr_project */
            {
                if (plr_project(mon->pos)) /* XXX _find_safety might do something dumb, like picking */
                {                          /* a point that the plr has cutoff access to */
                    pos = _find_safety(mon);
                    if (dun_pos_interior(dun, pos))
                    {
                        pos = _adjust_safety(mon, pos);
                        return _move_towards(mon, pos);
                    }
                }
                return _move_away(mon, plr->pos);
            }
            return _move_away(mon, dun->flow_pos); /* move away from plr stairs */
        }
    }
    else /* friendly or pet */
    {
        if (mon->target_id)
        {
            mon_ptr foe = dun_mon(mon->dun, mon->target_id);
            if (foe && _move_away(mon, foe->pos)) return TRUE;
        }
        /* Note: pets will be handled by _move_pet, where they _flow_towards master */
    }
    return FALSE;
}
/************************************************************************
 * Movement AI: Attack Target (mon vs mon AI)
 *
 * Note that monster targets are now sticky and are remembered across turns.
 * This gives more realistic monster battles, but we need to allow monsters
 * to give up targets and go back to chasing the plr.
 *
 * Outside of this module, monsters acquire targets in response to:
 *   [1] plr melee: plr_attack_end
 *   [2] plr archery: (code re-write pending ... currently do_cmd_fire_aux2) 
 *   [3] mon melee: mon_attack_end
 *   [4] plr or mon spellcasting: gf_affect_m (if GF_AFFECT_SPELL)
 *
 * Inside this module, monsters acquire targets if
 *   [1] They are not hostile: _move_friendly and _move_pet
 *   [2] If they are hostile, but the plr is not around: _move_hostile
 ************************************************************************/
static bool _plr_is_adjacent(mon_ptr mon)
{
    if (_ignore_plr(mon)) return FALSE;
    if (plr->dun_id != mon->dun->id) return FALSE;
    return point_is_adjacent(mon->pos, plr->pos);
}
static bool _move_target(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    if (mon->mflag & MFLAG_WILL_RUN) return FALSE;

    if (mon->target_id) /* vs. monster */
    {
        mon_ptr target = dun_mon(dun, mon->target_id);
        /* giveup target if dead or no longer an enemy (e.g. plr angered a friendly mon) ... */
        if (!target || !are_enemies(mon, target))
            mon_clear_target(mon);
        /* ... or too distant (perhaps target teleported). Keep a non-projectable
         * target if still close by (blink). */
        else if (!mon_project(mon, target->pos))
        {
            if (point_fast_distance(mon->pos, target->pos) > MAX_SIGHT)
                mon_clear_target(mon);
            else if (mon_is_pet(mon) /*&& pet_t_m_idx && mon->id != pet_t_m_idx*/)
                mon_clear_target(mon); /* stop chasing squirrels and obey master */
        }
        else if (mon_race_is_(target->race, "@.player"))
        {
            /* Don't give up target if we are fooled by a plr illusion.
             * Note: The next time the real plr hurts us, our target will reset, 
             * but a crafty plr might use the distraction to run away. */
            return _move_towards(mon, target->pos);
        }
        else /* target still valid and projectable */
        {
            /* don't leave off attacking plr to pursue a distant target. occasionally
             * attack adjacent plr if target is also adjacent. */
            if (mon_is_hostile(mon) && _plr_is_adjacent(mon))
            {
                if (!point_is_adjacent(mon->pos, target->pos) || one_in_(2))
                {
                    return FALSE;
                }
            }
            /* put aside petty quarrels and team up against a common enemy: the plr! */
            if (!_ignore_plr(mon) && !mon_tim_find(mon, MT_DISCORD) && mon_is_hostile(mon) && mon_is_hostile(target))
            {
                int cost;
                if (mon_project_plr(mon) && one_in_(2))
                {
                    mon_clear_target(mon);
                    mon_clear_target(target);
                    return FALSE;
                }
                cost = dun_flow_at(dun->flow, mon->pos);
                if (cost != DUN_FLOW_NULL && _1d(MAX_SIGHT) > cost) /* the lower the cost, the closer the plr */
                {
                    mon_clear_target(mon);
                    mon_clear_target(target);
                    return FALSE;
                }
            }
            /* keep after target */
            return _move_towards(mon, target->pos);
        }
    }
    else if (dun_pos_interior(dun, mon->target_pos))
    {
        bool moved = FALSE;

        /* XXX Support better diversions. For example, the Illusionist can place
         * a Klaxon at a given position. Whenever it sounds, nearby monsters are
         * woken up and pathfind to the target. */
        if (!point_equals(mon->pos, mon->target_pos))
        {
            if (!mon_project(mon, mon->target_pos) && mon_will_flow(mon))
            {
                dun_flow_ptr flow = dun_find_flow_at(dun, mon->target_pos);
                if (flow) moved = _flow_towards(mon, flow);
            }
            if (!moved) moved = _move_towards(mon, mon->target_pos);
        }

        /* XXX cf _preprocess where monsters are given a chance to notice the plr */

        /* monster is puzzled by a positional lure for a few turns ... */
        if (point_equals(mon->pos, mon->target_pos))
        {
            int odds = 4;
            if (mon_is_smart(mon)) odds /= 2;
            if (mon_is_stupid(mon)) odds *= 4;

            if (one_in_(odds)) /* figure it out */
            {
                if (dun_tim_remove_at(dun, mon->target_pos, DT_KLAXON))
                    msg_print("<color:R>Your Klaxon has been discovered!</color>");

                mon_clear_target(mon);
                if (mon_show_msg(mon))
                {
                    char name[MAX_NLEN_MON];
                    monster_desc(name, mon, 0);
                    msg_format("<color:R>%^s is no longer diverted!</color>", name);
                }
            }
        }
        return TRUE;
    }
    return FALSE;
}
/************************************************************************
 * Movement AI: Base Case separated for Hostile vs Friendly monsters
 *
 * This is the last chance to find a valid move. If we got here, then
 * we are not frightened or confused, lack a valid target, are not a pet
 * and lack intelligent monster pack behavior.
 *
 * Hostile monsters will go for the plr or attempt to acquire a target.
 * Friendly monster look for a target.
 ************************************************************************/
static bool _plr_project(dun_ptr dun, point_t pos) /* plr or stairs to plr */
{
    if (plr->dun_id == dun->id) return plr_project(pos);
    return point_project(pos, dun->flow_pos);
}
static bool _move_hostile(mon_ptr mon)
{
    dun_ptr dun = mon->dun;

    /* If plr is not LoS, look for a LoS enemy. For example, a friendly
     * unique may be attacking me, and we should settle things first. */
    if ((_ignore_plr(mon) || !mon_project_plr(mon)) && one_in_(5))
    {
        if (_acquire_target(mon)) return TRUE;
    }

    if (!_ignore_plr(mon))
    {
        /* Double check out of range processing. For example, monsters are battling far away,
         * but my target has blinked out of view. Keep my target, don't move, and wait until
         * my target reappears to continue the battle (cf AI_HUNT). XXX Only do this check
         * if mon has a target (direct, via mon->target, or pack based via mon->pack->prey).
         * On D_SURFACE, or if the plr is OFC_AGGRAVATE'ing, then range is temporarily increased
         * but mon is still after plr. cf _preprocess_mon XXX */
        bool target = mon_has_valid_target(mon);
        if (!target && mon->pack && mon->pack->ai == AI_HUNT)
            target = TRUE;
        if (target && mon_dis(mon) > mon_move_range(mon) && !_plr_project(dun, mon->pos))
            return TRUE;

        #if 1
        /* XXX On surface, try to lead a moving target. This will override normal flow
         * behaviour, but flows only really matter inside the town walls. */
        if ( mon->dun->id == plr->dun_id
          && mon->dun->type->id == D_SURFACE
       /* && running */
          && !plr_in_town()
          && point_is_adjacent(plr->pos, plr->last_pos) )
        {
            point_t v = point_subtract(plr->pos, plr->last_pos);
            line_t l = line_create(plr->pos, point_add(plr->pos, point_scale(v, DUN_VIEW_MAX)));
            int d = 0;
            
            if (line_is_valid(l))
                d = point_distance_to_line(mon->pos, l);

            if (d > 0)
            {
                point_t p = point_add(plr->pos, point_scale(v, d));
                if (_move_towards(mon, p)) return TRUE;
            }
        }
        #endif

        /* Now we flow or move towards plr */
        if (mon_will_flow(mon) && _flow_towards(mon, dun->flow)) return TRUE;
        if (_move_towards(mon, dun->flow_pos)) return TRUE;
    }

    /* out of options */
    return _move_drunken(mon);
}
static bool _move_friendly(mon_ptr mon)
{
    if (_acquire_target(mon)) return TRUE;

    /* out of options */
    return _move_drunken(mon);
}
/************************************************************************
 * Movement AI: Stuck in a Web (based on Vanilla)
 ************************************************************************/
static bool _stuck(mon_ptr mon)
{
    dun_cell_ptr cell = dun_cell_at(mon->dun, mon->pos);
    mon_aura_ptr aura;

    mon->mflag2 &= ~MFLAG2_WEB;
    if (!floor_has_web(cell)) return FALSE;
    
    /* spiders walk across webs effortlessly */
    if (mon_can_passweb(mon))
    {
        mon_lore_passweb(mon);
        return FALSE;
    }

    /* fire auras burn webs as monster moves (no charge) */
    aura = mon_auras_find(mon->race, GF_FIRE);
    if (aura)
    {
        floor_remove_web(cell);
        dun_note_pos(mon->dun, mon->pos);
        dun_draw_pos(mon->dun, mon->pos);
        mon_lore_aura(mon, aura);
        return FALSE;
    }

    /* disintegrate destroys webs (no charge) */
    if (mon_can_tunnel(mon)) /* XXX RFM_KILLWALL vs RFM_DIG? */
    {
        floor_remove_web(cell);
        dun_note_pos(mon->dun, mon->pos);
        dun_draw_pos(mon->dun, mon->pos);
        mon_lore_tunnel(mon);
        return FALSE;
    }

    /* big monsters thrash about, clearing the web (energy) */
    if (mon_can_clearweb(mon))
    {
        floor_remove_web(cell);
        dun_note_pos(mon->dun, mon->pos);
        dun_draw_pos(mon->dun, mon->pos);
        mon_lore_clearweb(mon);
        return TRUE; /* spend a turn */
    }

    /* chance of escape: big monsters are not held for long
     * XXX parm2 is the 'stickiness' of the web (cf _make_web) */
    if (!cell->parm2 || _1d(200 * cell->parm2) < mon->race->weight)
    {
        floor_remove_web(cell);
        dun_note_pos(mon->dun, mon->pos);
        dun_draw_pos(mon->dun, mon->pos);
        return TRUE; /* spend a turn */
    }
    else
    {
        assert(cell->parm2 > 0);
        cell->parm2--;
    }

    mon->mflag2 |= MFLAG2_WEB; /* cf _ai_stuck */
    return TRUE;
}
/************************************************************************
 * Movement AI: Putting it all together
 ************************************************************************/
static bool _move(mon_ptr mon)
{
    if (_will_run(mon)) mon->mflag |= MFLAG_WILL_RUN;
    if (mon->mana) mon->mana--;
    if (plr_view(mon->pos)) mon_lore_turn(mon);

    if (_stuck(mon)) return TRUE; /* XXX and helpless! */
    if (_drunken(mon)) return _move_drunken(mon);
    if (mon_tim_find(mon, T_BLIND)) return _move_blind(mon);
    if (_run(mon)) return TRUE;
    if (_move_target(mon)) return TRUE;

    if (mon_is_pet(mon)) return _move_pet(mon);

    if (_move_pack(mon)) return TRUE;
    if (mon_is_friendly(mon)) return _move_friendly(mon);
    return _move_hostile(mon);
}
/************************************************************************
 * Stairs
 ************************************************************************/
static bool _try_stairs(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    bool    fear = FALSE;

    if (!cell_is_stairs(dun_cell_at(dun, mon->pos))) return FALSE;
    if (mon_can_multiply(mon) && !mon_is_pet(mon)) return FALSE;

    if (!mon_is_pet(mon))
        fear = _will_run(mon);

    /* allow friendly uniques to AI_WANDER off level */
    if (mon->pack)
    {
        /* Wanderers only leave if this staircase is their goal.
         * Note AI_HUNT->AI_WANDER if no more prey. They are flagged as MFLAG2_HUNTER
         * in this case and should go from AI_WANDER->AI_HUNT once they have left this level. 
         * Note the case below to prohibit AI_HUNTers from bouncing right back.*/
        if (mon->pack->ai == AI_WANDER)
        {
            if (mon_pack_count(mon->pack) == 1 && mon->pack->flow && point_equals(mon->pack->flow->pos, mon->pos) )
            {
                if (dun_take_stairs_mon(dun, mon))
                {
                    if (mon->mflag2 & MFLAG2_HUNTER)
                        mon_pack_hunt(mon->pack); /* pick a prey on new level */
                    else
                        mon_pack_wander(mon->pack); /* pick a goal on new level */
                    return TRUE;
                }
            }
            return FALSE;
        }
        else if (mon->pack->ai == AI_HUNT)
            return FALSE;
        /* XXX Other ai strategies may leave, depending on the checks below.
         * XXX AI_GUARD_MON might need some checking here. */
    }
    /* bold monsters (and pets) follow player up/down stairs
     * Note: we only consider stairs leading to the plr's current level (this is at dun->flow_pos) */
    if (!fear && plr->dun_id != dun->id && point_equals(mon->pos, dun->flow_pos))
    {
        if (dun_take_stairs_mon(dun, mon))
            return TRUE;
    }
    /* frightened monsters flee player's level up/down stairs
     * XXX note on questors: this would be OK for dungeon guardians, but not for quest.c
     * cf quests_check_leave. plr won't be allowed to chase and questor may never return.  XXX */
    else if (fear && plr->dun_id == dun->id && !(mon->mflag2 & MFLAG2_QUESTOR))
    {
        if (dun_take_stairs_mon(dun, mon))
            return TRUE;
    }
    return FALSE;
}

/************************************************************************
 * Spells
 ************************************************************************/
static int _spell_freq(mon_ptr mon)
{
    mon_race_ptr race = mon->race;
    int          freq = 0;
    if (race->spells && !(mon->mflag2 & MFLAG2_ILLUSION))
    {
        freq = race->spells->freq;

        /* XXX We used to up the frequency for plr on a glyph of warding, 
         * but we don't know if the plr is the actual target of mon. Rethink XXX */

        /* Increase spell frequency for pack AI */
        if (mon->pack)
        {
            switch (mon->pack->ai)
            {
            case AI_SHOOT:
                freq = MAX(30, freq + 15);
                break;
            case AI_MAINTAIN_DISTANCE:
                freq += MIN(freq/2, 15);
                break;
            case AI_LURE:
            case AI_FEAR:
            case AI_GUARD_POS:
                freq += MIN(freq/2, 10);
                break;
            }
        }
        /* Angry monsters will eventually spell if they get too pissed off.
         * Monsters are angered by distance attacks (spell casters/archers) */
        freq += mon->anger;
        if (freq > 100) freq = 100;

        /* XXX Adapt spell frequency down if monster is stunned (EXPERIMENTAL)
         * Sure, stunning effects fail rates, but not on innate spells (breaths).
         * In fact, distance stunning gives no benefit against big breathers ...
         * Try a sprite mindcrafter and you'll see what I mean. */
        if (mon_tim_find(mon, T_STUN) && freq < 100)
        {
            int s = mon_tim_amount(mon, T_STUN);
            int p = MAX(0, 100 - s);
            freq = freq * p / 100;
            if (freq < 1) freq = 1;
        }
    }
    return freq;
}
static bool _counterspell(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    /* This duplicates _choose_target in mon_spell.c. In other words,
     * mon_spell_mon will re-aquire either mon->target or pack->prey.
     * We just need to know to call mon_spell_mon before mon_spell[_plr] */
    if (mon->target_id)
    {
        mon_ptr target = dun_mon(dun, mon->target_id);
        if (target && are_enemies(mon, target) && mon_project(mon, target->pos))
        {
            /* Monster targets are now sticky, and track the most recent
             * source of damage. Occasionally override if we can see plr.
             * Do not give up a "Player" target since monsters are utterly
             * convinced that this illusion is actually the player! */
            if (!_ignore_plr(mon) && !mon_race_is_(target->race, "@.player"))
            {
                if (mon_is_hostile(mon) && mon_project_plr(mon) && one_in_(2)) return FALSE;
            }
            return TRUE;
        }
    }
    if (mon->pack)
    {
        mon_ptr target = NULL;
        if (mon->pack->ai == AI_HUNT) target = dun_mon(dun, mon->pack->prey_id);
        if (target && mon_project(mon, target->pos))
            return TRUE;
    }
    return FALSE;
}
static bool _try_spell(mon_ptr mon)
{
    mon_race_ptr race = mon->race;
    int freq = _spell_freq(mon);
    if (!freq || randint1(100) > freq) return FALSE; /* move/attack instead */
    /* Block spells occasionally if the monster just cast.
     * Here, were are attempting to prevent long runs of consecutive casts for
     * melee characters (anger=0). Of course, Nodens should still spell every
     * turn! See ^A"F for analysis ... This is approach II. */
    if (!mon->anger && mon->mana > 0 && race->spells->freq <= 50 && !one_in_(1 + mon->mana))
        return FALSE; /* move/attack instead */
    /* Block spells for "Anti-Magic Ray" (Rage-Mage) and Anti-Magic monster spell (Possessor or mon_spell_mon) */
    if (mon->anti_magic_ct)
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);

        assert(race->spells);
        if (race->spells->freq == 100)
        {
            if (!mon_save_p(mon, A_STR)) /* XXX originally for Rage-Mage ... rethink */
            {
                if (mon_show_msg(mon))
                    msg_format("%^s tries to break your anti-magic ray but fails.", m_name);
                mon->anti_magic_ct--;
                return TRUE; /* lose a turn */
            }
            else
            {
                if (mon_show_msg(mon))
                    msg_format("%^s breaks your anti-magic ray.", m_name);
                mon->anti_magic_ct = 0;
                /* spells allowed on this turn */
            }
        }
        else
        {
            mon->anti_magic_ct--;
            return FALSE; /* move/attack instead */
        }
    }
    /* Try to cast a spell. We give preference to counterattacking a monster if possible. */
    if (_counterspell(mon))
    {
        if (mon_spell_cast_mon(mon, NULL)) return TRUE; /* XXX _choose_target will pick mon->target for counterattack */
        if (!_ignore_plr(mon) && mon_spell_cast(mon, NULL))
        {
            mon->anger = 0;
            mon->mana++;
            return TRUE;
        }
    }
    else
    {
        if (!_ignore_plr(mon) && mon_spell_cast(mon, NULL))
        {
            mon->anger = 0;
            mon->mana++;
            return TRUE;
        }
        if (mon_spell_cast_mon(mon, NULL)) return TRUE;
    }
    return FALSE; /* failed to pick a spell (or plr hiding in shadows) ... move/attack instead */
}

/************************************************************************
 * Breeding
 ************************************************************************/
static bool _try_multiply(mon_ptr mon)
{
    if (mon_can_multiply(mon) && plr->dun_id == mon->dun->id)
    {
        dun_ptr dun = mon->dun;
        if (dun->breed_ct >= MAX_REPRO) return FALSE;
        if (randint0(375) < virtue_current(VIRTUE_HARMONY)) return FALSE;
        if (plr_block_multiply(mon)) return FALSE;
        if (randint1(MAX_REPRO - dun->breed_ct) > randint1(5*dun->breed_kill_ct))
        {
            int ct = 0, dir;

            for (dir = 0; dir < 8; dir++)
            {
                point_t p = point_step(mon->pos, ddd[dir]);
                if (!dun_pos_interior(dun, p)) continue;
                if (dun_mon_at(dun, p)) ct++;
            }

            /* Hack -- multiply slower in crowded areas */
            if (ct < 4 && one_in_(8 + 8*ct))
            {
                bool allow = TRUE;
                assert(cave == dun); /* XXX multiply_monster, calculate_upkeep */
                if (mon_is_pet(mon))
                {
                    int upkeep = calculate_upkeep();
                    if (upkeep > 80)
                        allow = FALSE;
                    if (plr->pet_extra_flags & PF_NO_BREEDING)
                        allow = FALSE;
                }
                if (allow)
                {
                    mon_ptr baby = multiply_monster(mon, FALSE, mon_is_pet(mon) ? PM_FORCE_PET : 0);
                    if (baby && baby->ml)
                        mon_lore_can_multiply(mon);
                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}
/************************************************************************
 * Preprocessing
 ************************************************************************/
static void _get_angry(mon_ptr mon)
{
    /* XXX Pet's are never "aggravated" */
    if ((plr->cursed & OFC_AGGRAVATE) && mon_is_friendly(mon))
    {
        /* XXX If we don't do packs all at once, the pack will begin
         * in-fighting as we process members one at a time! Note: This
         * is pretty much the only time that angering a monster should
         * anger the entire pack ... or is it? Attack a member and the
         * rest of the pack does what: Side with plr? Turn on plr? Did
         * they even see the offense? XXX */
        if (mon->pack) mon_pack_anger(mon->pack);
        else anger_monster(mon);
    }
}
static void _scatter_mold(mon_ptr mon)
{
    if (!mon_race_is_(mon->race, "w.ohmu")) return;
    if (one_in_(3))
    {
        int  k;
        int  flags = PM_ALLOW_GROUP;
        if (mon_is_pet(mon)) flags |= PM_FORCE_PET;
        for (k = 0; k < 6; k++)
            summon_specific(who_create_mon(mon), mon->pos, mon->race->alloc.lvl, SUMMON_BIZARRE1, flags);
    }
}
static void _cybernoise(mon_ptr mon)
{
    if (!mon_race_is_(mon->apparent_race, "U.cyber")) return;
    if (!disturb_minor) return;
    if (mon->ml) return;
    if (mon_dis(mon) > MAX_SIGHT) return;

    if (one_in_(20))
    {
        msg_print("You hear heavy steps.");
        disturb(0, 0);
    }
}

static void _speak(mon_ptr mon)
{
    if (!mon_can_speak(mon)) return;
    if (mon->mflag & MFLAG_IGNORE_PLR) return;
    if (!is_aware(mon)) return;
    if (mon->id == plr->riding) return;
    if (!plr_view(mon->pos)) return;

    if (one_in_(8))
    {
        char m_name[80];
        char monmessage[1024];
        cptr filename;

        /* Acquire the monster name/poss */
        if (mon->ml)
            monster_desc(m_name, mon, 0);
        else
            strcpy(m_name, "It");

        /* Select the file for monster quotes */
        if (mon_tim_find(mon, T_FEAR))
            filename = "monfear.txt";
        else if (mon_is_friendly(mon))
            filename = "monfrien.txt";
        else
            filename = "monspeak.txt";

        if (get_rnd_line(filename, sym_str(mon->apparent_race->id), monmessage) == 0)
        {
            msg_format("<color:g>%^s</color> %s", m_name, monmessage);
            msg_boundary();
        }
    }
}

static bool _notice_plr(mon_ptr mon)
{
    if (!mon_project_plr(mon)) return FALSE;
    if (plr->cur_light <= 0) return one_in_(20 + plr->skills.stl);
    if (plr->skills.stl < 7) return TRUE;
    return one_in_(plr->skills.stl - 5); /* XXX A rogue s/b able to tip-toe up to a distracted monster. */
}

/* Preprocessing handles trumps, chameleons, disappearing pets, sleeping monsters, etc.
 * If preprocessing returns success, we keep going. Otherwise, something
 * went wrong - the monster might be dead or sleeping - in which case we stop
 * processing this monster. */
static bool _preprocess(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    mon_race_ptr race;

    if ((mon->mflag2 & MFLAG2_CHAMELEON) && one_in_(13) && !mon_tim_find(mon, MT_SLEEP))
        choose_new_monster(mon, FALSE, NULL);

    race = mon->race; /* after chameleon change */
    if (mon_is_trump(mon) && one_in_(2))
    {
        if (!mon_tim_find(mon, MT_SLEEP) && mon->id != plr->riding)
        {
            mon_lore_trump(mon);
            teleport_away(mon, 5, 0);
        }
    }

    if (mon->id == plr->riding)
    {
        if (plr->prace == RACE_MON_RING)
        {
            ring_process_m(mon);
        }
        else if (!mon_race_is_ridable(race))
        {
            if (rakuba(0, TRUE))
            {
                char m_name[80];
                monster_desc(m_name, mon, 0);
                msg_format("You have fallen from %s.", m_name);
            }
        }
    }

    /* Players hidden in shadow are almost imperceptible. -LM- */
    mon->mflag &= ~MFLAG_IGNORE_PLR; /* assume we are aware of the player */
    if (!mon_is_pet(mon))
    {
        if (plr->innocence) /* but he wouldn't hurt a fly, would he? */
            mon->mflag |= MFLAG_IGNORE_PLR;
        else if (plr->special_defense & NINJA_S_STEALTH)
        {
            int tmp = plr->lev*6 + (plr->skills.stl + 10)*4;
            if (plr->monlite) tmp /= 3;
            if (plr->cursed & OFC_AGGRAVATE) tmp /= 2;
            if (race->alloc.lvl > (plr->lev*plr->lev/20+10)) tmp /= 3;
            /* Low-level monsters will find it difficult to locate the player. */
            if (randint0(tmp) > (race->alloc.lvl+20)) mon->mflag |= MFLAG_IGNORE_PLR;

            /* Note: The aware flag will induce random monster movement since
               the player is hiding in the shadows. The is_aware() attribute
               means the monster is unaware that a mimic is not an object. In
               this case, movement should still be towards the object (player). */
        }
    }
    /* Invisibility works similarly! (Illusionist)
     * We ignore plr->cur_light in the interest of simplicity.
     * cf project which reveals the plr
     * cf mon_take_hit() which reveals the plr
     * cf _spell_cast_init() which fuzzes up targetting
     * cf _attack which fuzzes up melee */
    if (!(mon->mflag & MFLAG_IGNORE_PLR) && (plr->special_defense & DEFENSE_INVISIBLE) && !mon_is_pet(mon))
    {
        int pl = plr->lev*6 + (plr->skills.stl + 10)*4;
        int ml = race->alloc.lvl + 20;

        if (race->body.flags & RF_POS_SEE_INVIS) ml *= 2; /* XXX this is really common! */

        if (randint0(pl) > ml) mon->mflag |= MFLAG_IGNORE_PLR;
    }
    /* Illusionist Trickery. AI code is better if we check this here. Otherwise,
     * a fooled monster can still spell cast on the plr. */
    if (!(mon->mflag & MFLAG_IGNORE_PLR) && dun_pos_interior(dun, mon->target_pos))
    {
        if (mon_is_hostile(mon) && _notice_plr(mon))
        {
            mon_clear_target(mon);
            if (mon_show_msg(mon))
            {
                char name[MAX_NLEN_MON];
                monster_desc(name, mon, 0);
                msg_format("<color:R>%^s notices you!</color>", name);
            }
        }
        else mon->mflag |= MFLAG_IGNORE_PLR;
    }

    if (mon_tim_find(mon, MT_DISCORD) && !mon->target_id && !_plr_is_adjacent(mon))
    {
        mon_ptr foe = _find_enemy(mon);
        if (foe) mon->target_id = foe->id;
        /* XXX Note that are_enemies might be false after timer expiration and
         * that _move_target will detect this and reset the target. */
        /* XXX Note we still consider the plr ... if no "enemies" are about,
         * then we *will* attack the plr. Try adding T_CLOAK_INNOCENCE with
         * T_MASK_DISCORD for better results :D */
    }

    /* Chant of Friendship grants temporary friendship so long as the chant
     * is maintained and audible. */
    if ( mon_is_temp_friendly(mon)
      && (/* XXX !plr_view(mon->pos) ||*/ !plr_tim_find(T_BLESS_FRIENDSHIP))
      && one_in_(7))
    {
        if (!mon_is_unique(mon) && !mon_save_p(mon, A_CHR))
        {
            /* your chant made a deep impression on this monster ... a conversion! */
            remove_flag(mon->smart, SM_TEMP_FRIENDLY);
        }
        else /* recidivism */
        {
            if (mon_show_msg(mon))
            {
                char name[80];

                monster_desc(name, mon, 0);
                msg_format("%^s gets angry!", name);
            }
            set_hostile(mon);
        }
    }

    /* Chant of Obedience grants temporary obedience so long as the chant
     * is maintained and audible. Note: Mask of Command does a similar thing
     * for the Illusionist, so we need to check the source of obedience. */
    if ( mon_is_temp_pet(mon)
      && !plr_tim_find(T_MASK_CHARM) /* cf _mask_command_off */
      && (/* XXX !plr_view(mon->pos) || */ !plr_tim_find(T_BLESS_OBEDIENCE))
      && one_in_(7) )
    {
        if (!mon_is_unique(mon) && !mon_save_p(mon, A_CHR))
        {
            /* your chant made a deep impression on this monster ... a conversion! */
            remove_flag(mon->smart, SM_TEMP_PET);
        }
        else /* recidivism */
        {
            if (mon_show_msg(mon))
            {
                char name[80];

                monster_desc(name, mon, 0);
                msg_format("%^s gets angry!", name);
            }
            set_hostile(mon);
        }
    }
    /* XXX not sure about requiring LoS for chants ... could check range instead? */

    if (mon->parent_id && !dun_mon(dun, mon->parent_id))
    {
        /* Its parent have gone, it also goes away.  Hack: Only for pets.  */
        if (!mon_is_pet(mon))
        {
            mon_set_parent(mon, 0);
        }
        else
        {
            bool keep = FALSE;
            /* XXX See if master has taken nearby stairs (e.g. player's mount).
             * We'll head for the stairs to follow the player in this case. */
            if (mon->dun->id != plr->dun_id)
            {
                if (dun_mon(plr_dun(), mon->parent_id))
                    keep = TRUE;
            }
            if (!keep)
            {
                if (mon_show_msg(mon))
                {
                    char m_name[80];
                    monster_desc(m_name, mon, 0);
                    msg_format("%^s disappears!", m_name);
                }
                delete_monster(mon);
                return FALSE;
            }
        }
    }
    if (mon_race_is_(mon->race, "{.grenade"))
    {
        gf_affect_m(who_create_null(), mon, GF_MISSILE, mon->hp + 1, GF_AFFECT_QUIET);
        return FALSE;
    }

    if ((mon_is_pet(mon) || mon_is_friendly(mon)) && (mon_race_is_unique(race) || mon_race_is_nazgul(race)))
    {
        static int riding_pinch = 0;

        if (mon->hp < mon->maxhp/3)
        {
            char m_name[MAX_NLEN];
            monster_desc(m_name, mon, 0);

            if (mon->id == plr->riding && riding_pinch < 2)
            {
                msg_format("%^s seems to be in so much pain, and trying to escape from your restriction.", m_name);
                riding_pinch++;
                disturb(1, 0);
            }
            else
            {
                if (mon->id == plr->riding)
                {
                    msg_format("%^s succeeded to escape from your restriction!", m_name);
                    if (rakuba(-1, FALSE))
                        msg_print("You have fallen from riding pet.");
                }

                if (mon_show_msg(mon))
                {
                    if ( mon_can_speak(mon)
                      && !mon_race_is_(mon->race, "C.Grip")
                      && !mon_race_is_(mon->race, "C.Wolf")
                      && !mon_race_is_(mon->race, "C.Fang")
                      && plr_view(mon->pos) )
                    {
                        msg_format("%^s says 'It is the pinch! I will retreat'.", m_name);
                    }
                    msg_format("%^s read a scroll of teleport level.", m_name);
                    msg_format("%^s disappears.", m_name);
                }

                if (mon->id == plr->riding && rakuba(-1, FALSE))
                    msg_print("You have fallen from riding pet.");

                quests_on_kill_mon(mon);
                delete_monster(mon);
                return FALSE;
            }
        }
        else
        {
            /* Reset the counter */
            if (mon->id == plr->riding) riding_pinch = 0;
        }
    }

    /* Hack: Rings wake up potential ring bearers */
    if ( mon_tim_find(mon, MT_SLEEP)
      && plr->action == ACTION_GLITTER
      && mon_is_type(mon->race, SUMMON_RING_BEARER) )
    {
        mon_tim_remove(mon, MT_SLEEP);
    }

    /* Aggravation wakes up sleeping monsters. Otherwise, they miss a turn */
    if (mon_tim_find(mon, MT_SLEEP))
    {
        if (plr->cursed & OFC_AGGRAVATE)
            mon_tim_remove(mon, MT_SLEEP);
        else
            return FALSE;
    }

    if (mon->id == plr->riding)
        plr->update |= PU_BONUS; /* XXX Why? */

    _get_angry(mon);
    _scatter_mold(mon);
    _cybernoise(mon);
    _speak(mon);

    return TRUE;
}

/************************************************************************
 * Monster Processing Entry Point
 ************************************************************************/
mon_ptr mon_process_current(void) { return _current; }
u32b mon_process_current_id(void) { return _current ? _current->id : 0; }

void mon_process_aux(mon_ptr mon)
{
    assert(_current == mon);
    mon->mflag &= ~MFLAG_PROCESS_MASK;

    if (!_preprocess(mon)) return; /* monster is dead, removed, or still sleeping */
    if (_try_stairs(mon)) return;
    if (_try_multiply(mon)) return;
    if (_try_spell(mon)) return;
    if (!_move(mon))
    {   
        /* XXX get bold if out of options */
        if (mon_tim_find(mon, T_FEAR) && !_ignore_plr(mon))
        {
            mon_tim_delete(mon, T_FEAR);
            if (mon_show_msg(mon))
            {
                char m_name[80];
                monster_desc(m_name, mon, 0);
                msg_format("%^s turns to fight!", m_name);
            }
        }
    }
    /* XXX Leave MFLAG_PROCESS_MASK on till next mon_process. For example, the plr can
     * ambush an MFLAG_IGNORE_PLR monster. cf plr_check_hit */
    /* XXX cf process_the_world:
     * mon_process(dio)
     *  mon_spell_cast(dio)
     *   process_the_world()
     *    mon_process_aux(dio)(x3-4) XXX */
}
void mon_process(mon_ptr mon)
{
    assert(!_current); /* not re-entrant ... but cf process_the_world */
    _current = mon;
    mon_process_aux(mon);
    _current = NULL;
}
/************************************************************************
 * Monster Targetting
 *
 * This is for mon vs. mon battles. Hostile monsters always implicitly
 * target the plr, so there is no need to track him. However, a valid
 * mon->target will override pursuit of the plr, so direct plr damage
 * should clear the current target.
 *
 * Note: Target used to reset after every monster move. Now, it only
 * resets upon plr damage or monster teleportation. So, a position target
 * can now lure a monster away until something damages it.
 ************************************************************************/
point_t mon_fuzzy_pos(mon_ptr mon, point_t pos)
{
    int d = point_fast_distance(mon->pos, pos);
    int s;
    if (d < 2) return pos;
    s = 1 + (d-2)/3;
    assert(s > 0);
    return scatter(pos, s);
}
void mon_set_target(mon_ptr mon, point_t pos)
{
    if (!mon_is_valid(mon)) return;

    /* If the plr hurts mon, then forget the current target ... */
    if (dun_plr_at(mon->dun, pos))
        mon_clear_target(mon);
    /* ... otherwise, remember the last offender and seek revenge next turn */
    else
    {
        mon_ptr target = dun_mon_at(mon->dun, pos);
        if (target)
        {
            if (!are_enemies(mon, target)) return; /* ignore ... client code usually checks this, but not always */
            mon->target_id = target->id;
            mon->target_pos.x = 0;
            mon->target_pos.y = 0;
            if (mon->pack)
            {
                mon_pack_set_target(mon->pack, mon, target);
            }
        }
        else
        {
            mon->target_id = 0;
            mon->target_pos = pos;
        }
    }
    if (mon_tim_find(mon, T_BLIND))
        mon->last_enemy_pos = mon_fuzzy_pos(mon, pos);
    else
        mon->last_enemy_pos = pos;
}
void mon_clear_target(mon_ptr mon)
{
    mon->target_pos.x = 0;
    mon->target_pos.y = 0;
    mon->target_id = 0;
    /* cf teleport_away or dun_take_stairs_mon */
    mon->last_enemy_pos.x = 0;
    mon->last_enemy_pos.y = 0;
}
bool mon_has_valid_target(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    /* target is a known monster? */
    if (mon->target_id)
    {
        mon_ptr target = dun_mon(dun, mon->target_id);
        if (!target) /* dead or fled */
        {
            mon_clear_target(mon);
            return FALSE;
        }
        return TRUE;
    }
    /* target is a location (e.g. last broken mirror in a mirror-master trick shot) */
    return dun_pos_interior(dun, mon->target_pos);
}
