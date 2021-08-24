/* File: cave.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: low level dungeon routines -BEN- */


#include "angband.h"
#include "dun.h"

#include <assert.h>

/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@angband.org)
 */

static byte display_autopick;
static int match_autopick;
static object_type *autopick_obj;

/*
 * Distance between two points via Newton-Raphson technique
 */
int plr_distance(point_t p) { return point_distance(plr->pos, p); }
int point_distance(point_t p1, point_t p2)
{
    point_t v = point_subtract(p2, p1);
    point_t a = point_abs(v);
    int target = (a.y * a.y) + (a.x * a.x);
    int d = (a.y > a.x) ? (a.y + (a.x>>1)) : (a.x + (a.y>>1));
    int err;

    /* Simple case */
    if (!a.y || !a.x) return d;

    while (1)
    {
        /* Approximate error */
        err = (target - d * d) / (2 * d);

        /* No error - we are done */
        if (!err) break;

        /* Adjust distance */
        d += err;
    }

    return d;
}


bool point_los(point_t p1, point_t p2) { return dun_los(cave, p1, p2); }


/*
 * Can the player "see" the given grid in detail?
 *
 * He must have vision, illumination, and line of sight.
 *
 * Note that "CAVE_GLOW" makes little sense for a wall, since it would mean
 * that a wall is visible from any direction. That would be odd. Except
 * under wizard light, which might make sense. Thus, for walls, we require
 * not only that they be "CAVE_GLOW", but also, that they be adjacent to a
 * grid which is not only "CAVE_GLOW", but which is a non-wall, and which is
 * in line of sight of the player.
 *
 * XXX This check is handled during the plr_light calculation (dun_update_light) XXX
 */
bool plr_can_see(point_t pos)
{
    int light;

    if (plr->dun_id != cave->id) return FALSE;
    if (plr_tim_find(T_BLIND)) return FALSE;

    light = plr_light(pos);
    if (light > 0) return TRUE; /* light != 0 => plr_view(pos) */
    if (!plr_view(pos)) return FALSE;
    return plr_see_nocto(pos);
}

/*
 * Returns true if the player's grid is dark
 */
bool no_light(void)
{
    return (!plr_can_see(plr->pos));
}




/*
 * Hack -- Legal monster codes
 */
static char image_monster_hack[] = \
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/*
 * Hack -- Legal object codes
 */
static char image_object_hack[] = "?/|\\\"!$()_-=[]{},~";

/*
 * Mega-Hack -- Hallucinatory monster
 */
static term_char_t image_monster(void)
{
    term_char_t tc;
    if (use_graphics)
    {
        vec_ptr v = mon_alloc_current_tbl();
        mon_race_ptr r = vec_random(v);

        if (r) return mon_race_visual(r);
    }


    tc.c = (one_in_(25) ?
           image_object_hack[randint0(sizeof(image_object_hack) - 1)] :
           image_monster_hack[randint0(sizeof(image_monster_hack) - 1)]);

    tc.a = randint1(15);
    return tc;
}


/*
 * Mega-Hack -- Hallucinatory object
 */
static term_char_t image_object(void)
{
    term_char_t tc;
    if (use_graphics)
    {
        object_kind *k_ptr = &k_info[randint1(max_k_idx-1)];

        tc.c = k_ptr->x_char;
        tc.a = k_ptr->x_attr;
    }
    else
    {
        int n = sizeof(image_object_hack) - 1;

        tc.c = image_object_hack[randint0(n)];
        tc.a = randint1(15);
    }
    return tc;
}



/*
 * Hack -- Random hallucination
 */
static term_char_t image_random(void)
{
    if (randint0(100) < 75)
        return image_monster();
    return image_object();
}

/*
 * XXX removed stale|misleading comments
 * map_info is used to render map grids for display. We now use a map_char_t
 * which maintains a layered stack of display chars (term_char_t) as well
 * as lighting and priority information.
 * XXX
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them. Technically, the flag
 * "CHAR_MULTI" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.
 *
 * Note the effects of hallucination. Objects always appear as random
 * "objects", monsters as random "monsters", and normal grids occasionally
 * appear as random "monsters" or "objects", but note that these random
 * "monsters" and "objects" are really just "colored ascii symbols".
 *
 */
void map_info(point_t pos, map_char_ptr mc)
{
    dun_grid_ex_t gx = dun_grid_ex_at(cave, pos);
    obj_ptr obj;
    int lite = plr_light(pos);
    bool nocto = plr_see_nocto(pos);

    mc->count = 0;
    mc->light = lite;
    mc->priority = 1; /* UNSAFE < NOTHING < FLOOR < ... */
                      /*   0    <    1    <   2   < ... */
    /* optionally ignore daylight ... still requires view_light (cf _apply_lighting) */
    if (!view_daylight && cave->type->id == D_SURFACE && plr_view(pos))
        mc->light = lite - cave->ambient_light;
    /* optionally ignore CELL_LIT ... still requires view_light (cf _apply_lighting) */
    if (!view_gridlight && (gx.grid->flags & CELL_LIT) && plr_view(pos))
        mc->light--;
    /* experimental: if you are "viewing grid light", then don't flicker display
     * as plr_view changes. this is distracting ... simply draw all out of view
     * lit tiles at lite=1 */
    if (view_gridlight && (gx.grid->flags & CELL_LIT) && !plr_view(pos))
        mc->light++;
    if (view_gridlight && (gx.grid->flags & CELL_DARK) && !plr_view(pos))
        mc->light--;

    /* Hacks for dun_world_map_ui */
    if (cave->type->id == D_WORLD)
    {
        if (gx.grid->flags & CELL_MAP)
        {
            if (gx.grid->flags & CELL_ROAD)
            {
                map_char_push(mc, visual_get("FLOOR", 0));
                mc->priority = 20;
            }
            if (gx.grid->flags & CELL_TOWN)
            {
                map_char_push(mc, visual_get("TOWN", 0));
                mc->priority = 30;
            }
            if (gx.grid->flags & CELL_DUNGEON)
            {
                dun_type_ptr type = dun_types_lookup(gx.grid->parm2);
                if (!(type->flags.plr & (DF_PLR_FAILED | DF_PLR_SECRET)))
                {
                    if (!(type->flags.plr & DF_PLR_ENTERED))
                        map_char_push(mc, visual_get("QUEST_ENTRANCE", 0));
                    else
                        map_char_push(mc, visual_get("STAIRS_DOWN", 0));
                    mc->priority = 30;
                }
            }
            if (!mc->count)
            {
                cell_display(gx.grid, 0, mc);
                mc->priority = cell_priority(gx.grid);
            }
        }
        return;
    }

    /* Terrain */
    if (plr_tim_find(T_HALLUCINATE) && one_in_(256))
        map_char_push(mc, image_random());
    else if (cell_is_boring(gx.grid)) /* floors */
    {
        if ( !plr_tim_find(T_BLIND)
          && ((gx.grid->flags & CELL_MAP) || lite > 0 || nocto) )
        {
            cell_display(gx.grid, mc->light, mc);
            mc->priority = cell_priority(gx.grid);
        }
        else
        {
            /* Unsafe cave grid -- idea borrowed from Unangband */
            if (view_unsafe_grids && dun_pos_interior(cave, pos) && (gx.grid->flags & CELL_UNSAFE))
            {
                map_char_push(mc, visual_get("UNSAFE", 0));
                mc->priority = 0;
            }
        }
    }
    else /* map remembers non-floors */
    {
        if (gx.grid->flags & CELL_MAP)
        {
            cell_display(gx.grid, mc->light, mc);
            mc->priority = cell_priority(gx.grid);
        }
        else
        {
            /* Unsafe cave grid -- idea borrowed from Unangband */
            if (view_unsafe_grids && dun_pos_interior(cave, pos) && (gx.grid->flags & CELL_UNSAFE))
            {
                map_char_push(mc, visual_get("UNSAFE", 0));
                mc->priority = 0;
            }
        }
    }

    /* Objects: seek first 'wanted' object base on pickup preferences */
    for (obj = gx.obj; obj; obj = obj->next)
    {
        if (obj->marked & OM_FOUND)
        {
            if (display_autopick)
            {
                byte act;

                match_autopick = is_autopick(obj);
                if(match_autopick == -1)
                    continue;

                act = autopick_list[match_autopick].action;
                if ((act & DO_DISPLAY) && (act & display_autopick))
                    autopick_obj = obj;
                else
                {
                    match_autopick = -1;
                    continue;
                }
            }

            /* found a match */
            mc->priority = 20;

            if (plr_tim_find(T_HALLUCINATE)) map_char_push(mc, image_object());
            else map_char_push(mc, obj_display_char(obj));

            break;
        }
    }

    /* Handle monsters and/or plr */
    if (gx.plr)
    {
        map_char_push(mc, plr_get_display_char_attr());
        mc->priority = 31;
    }
    else if (gx.mon && display_autopick == 0 && gx.mon->ml)
    {
        monster_race *r_ptr = gx.mon->apparent_race;

        mc->priority = 30;

        /* Hallucination */
        if (plr_tim_find(T_HALLUCINATE))
        {
            /*
             * Monsters with both CHAR_CLEAR and ATTR_CLEAR
             * flags are always unseen.
             */
            if ((r_ptr->display.flags & (RFD_CHAR_CLEAR | RFD_ATTR_CLEAR)) == (RFD_CHAR_CLEAR | RFD_ATTR_CLEAR))
            {
                /* Do nothing */
            }
            else
            {
                /* Hallucinatory monster */
                map_char_push(mc, image_monster());
            }
        }
        else
        {
            term_char_t tc = mon_visual(gx.mon);

            if ((gx.mon->mflag2 & MFLAG2_ILLUSION) && mon_race_is_(gx.mon->race, "@.player"))
            {
                tc = plr_get_display_char_attr();
                map_char_push(mc, tc);
            }
            else if (gx.mon->mflag2 & MFLAG2_FUZZY)
            {
                map_char_push(mc, tc);
            }
            /* Pets: Optional hilite */
            else if ( (plr->pet_extra_flags & PF_HILITE)
                   && ( mon_is_pet(gx.mon)
                     || who_mon(plr->duelist_target) == gx.mon ) )
            {
                term_char_t hl = visual_get("HILITE", 0);
                if (!use_graphics)
                    tc.a = hl.a;
                else
                {
                    mc->count = 0; /* XXX cf Term_queue_map_char */
                    map_char_push(mc, hl);
                }
                map_char_push(mc, tc);
            }
            /* Normal monsters */
            else if (!(r_ptr->display.flags & (RFD_CHAR_CLEAR | RFD_SHAPECHANGER | RFD_ATTR_CLEAR
                                   | RFD_ATTR_MULTI | RFD_ATTR_SEMIRAND)))
            {
                map_char_push(mc, tc);
            }

            /*
             * Monsters with both CHAR_CLEAR and ATTR_CLEAR
             * flags are always unseen.
             */
            else if ((r_ptr->display.flags & (RFD_CHAR_CLEAR | RFD_ATTR_CLEAR)) == (RFD_CHAR_CLEAR | RFD_ATTR_CLEAR))
            {
            }

            else
            {
                term_char_t bg = map_char_top(mc);
                /***  Monster's attr  ***/
                if ((r_ptr->display.flags & RFD_ATTR_CLEAR) && (bg.a != TERM_DARK) && !use_graphics)
                {
                    tc.a = bg.a;
                }
                else if ((r_ptr->display.flags & RFD_ATTR_MULTI) && !use_graphics)
                {
                    /* Multi-hued attr */
                    if (r_ptr->display.flags & RFD_ATTR_ANY) tc.a = randint1(15);
                    else switch (randint1(7))
                    {
                    case 1: tc.a = TERM_RED;     break;
                    case 2: tc.a = TERM_L_RED;   break;
                    case 3: tc.a = TERM_WHITE;   break;
                    case 4: tc.a = TERM_L_GREEN; break;
                    case 5: tc.a = TERM_BLUE;    break;
                    case 6: tc.a = TERM_L_DARK;  break;
                    case 7: tc.a = TERM_GREEN;   break;
                    }
                }
                else if ((r_ptr->display.flags & RFD_ATTR_SEMIRAND) && !use_graphics)
                {
                    /* Use semi-random attr (usually mimics' colors vary) */
                    tc.a = gx.mon->id % 15 + 1;
                }
                else
                {
                }

                /***  Monster's char  ***/
                if ((r_ptr->display.flags & RFD_CHAR_CLEAR) && (bg.c != ' ') && !use_graphics)
                {
                    tc.c = bg.c;
                }
                else if (r_ptr->display.flags & RFD_SHAPECHANGER)
                {
                    tc = image_monster();
                }
                else
                {
                }
                map_char_push(mc, tc);
            }
        }
    }
}

static mon_race_ptr _plr_display_race(void)
{
    switch (plr->mimic_form)
    {
    case MIMIC_VAMPIRE:
        return mon_race_parse("V.master");
    case MIMIC_DEMON:
        return mon_race_parse("U.balrog");
    case MIMIC_DEMON_LORD:
        return mon_race_parse("U.Gothmog");
    case MIMIC_BAT:
        return mon_race_parse("b.vampire");
    case MIMIC_MIST:
        return mon_race_parse("#.vampiric mist");
    case MIMIC_WOLF:
        return mon_race_parse("C.wolf");
    }
    if (plr->prace == RACE_MON_RING && plr->riding)
        return plr_riding_mon()->race;

    if (plr->prace == RACE_DRACONIAN && mut_present(MUT_DRACONIAN_METAMORPHOSIS))
    {
        switch(plr->psubrace)
        {
        case DRACONIAN_RED: return mon_race_parse("D.red");
        case DRACONIAN_WHITE: return mon_race_parse("D.white");
        case DRACONIAN_BLUE: return mon_race_parse("D.blue");
        case DRACONIAN_BLACK: return mon_race_parse("D.black");
        case DRACONIAN_GREEN: return mon_race_parse("D.green");
        case DRACONIAN_BRONZE: return mon_race_parse("D.bronze");
        case DRACONIAN_GOLD: return mon_race_parse("D.gold");
        case DRACONIAN_SHADOW: return mon_race_parse("D.death");
        case DRACONIAN_CRYSTAL: return mon_race_parse("D.crystal");
        }
    }
    return plr_mon_race();
}
term_char_t plr_get_display_char_attr(void)
{
    mon_race_ptr race = mon_race_parse("@.player");

    if (display_race)
        race = _plr_display_race();

    assert(race); /* paranoia */
    if (!race) race = mon_race_parse("@.player"); /* defense */

    return mon_race_visual(race);
}

/*
 * Moves the cursor to a given MAP (y,x) location
 */
void move_cursor_relative(point_t pos)
{
    point_t ui = cave_pt_to_ui_pt(pos);
    Term_gotoxy(ui.x, ui.y);
}



/*
 * Place an attr/char pair at the given map coordinate, if legal.
 */
void print_rel(char c, byte a, int y, int x)
{
    if (cave_xy_is_visible(x, y))
    {
        point_t ui = cave_xy_to_ui_pt(x, y);
        if (!msg_line_contains(ui.y, ui.x))
            Term_queue_bigchar(ui.x, ui.y, a, c, 0, 0);
    }
}

/*
 * Memorize interesting viewable object/features in the given grid
 *
 * This function should only be called on "legal" grids.
 *
 * This function will memorize the object and/or feature in the given
 * grid, if they are (1) viewable and (2) interesting. Note that all
 * objects are interesting, all terrain features except floors (and
 * invisible traps) are interesting, and floors (and invisible traps)
 * are interesting sometimes (depending on various options involving
 * the illumination of floor grids).
 *
 * The automatic memorization of all objects and non-floor terrain
 * features as soon as they are displayed allows incredible amounts
 * of optimization in various places, especially "map_info()".
 *
 * Note that the memorization of objects is completely separate from
 * the memorization of terrain features, preventing annoying floor
 * memorization when a detected object is picked up from a dark floor,
 * and object memorization when an object is dropped into a floor grid
 * which is memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of
 * a grid (or the object in a grid) is called into question, such
 * as when an object is created in a grid, when a terrain feature
 * "changes" from "floor" to "non-floor", when any grid becomes
 * "illuminated" or "viewable", and when a "floor" grid becomes
 * "torch-lit".
 *
 * Note the relatively efficient use of this function by the various
 * "update_view()" and "update_lite()" calls, to allow objects and
 * terrain features to be memorized (and drawn) whenever they become
 * viewable or illuminated in any way, but not when they "maintain"
 * or "lose" their previous viewability or illumination.
 *
 * Note the butchered "internal" version of "player_can_see_bold()",
 * optimized primarily for the most common cases, that is, for the
 * non-marked floor grids.
 */
void note_pos(point_t pos)
{
    dun_grid_ptr cell = dun_grid_at(cave, pos);
    obj_ptr obj;

    if (!plr_can_see(pos)) return;

    /* Memorize objects */
    for (obj = dun_obj_at(cave, pos); obj; obj = obj->next)
    {
        obj->marked |= OM_FOUND;
        plr->window |= PW_OBJECT_LIST;
    }

    cell->flags |= CELL_AWARE;

    /* Hack -- memorize grids */
    if (!(cell->flags & CELL_MAP))
    {
        int lite = plr_light(pos);

        /* Memorize some "boring" grids */
        if (cell_is_boring(cell))
        {
            /* Option -- memorize all torch-lit floors */
            if (view_torch_grids && (lite > 0 || plr_see_nocto(pos)))
                cell->flags |= CELL_MAP;

            /* Option -- memorize all perma-lit floors */
            else if (view_perma_grids && ((cell->flags & CELL_LIT) && lite > 0))
                cell->flags |= CELL_MAP;
        }

        /* Memorize normal grids (REMEMBER | LOS) */
        else if (cell_los(cell))
            cell->flags |= CELL_MAP;

        /* Memorize torch-lit walls */
        else if (lite > 0)
            cell->flags |= CELL_MAP;

        /* Memorize walls seen by noctovision of Ninja */
        else if (plr_see_nocto(pos))
            cell->flags |= CELL_MAP;
    }
}

void display_dungeon(void)
{
    point_t p;
    for (p.x = plr->pos.x - Term->wid / 2 + 1; p.x <= plr->pos.x + Term->wid / 2; p.x++)
    {
        for (p.y = plr->pos.y - Term->hgt / 2 + 1; p.y <= plr->pos.y + Term->hgt / 2; p.y++)
        {
            point_t tp;
            tp.x = p.x - plr->pos.x + Term->wid / 2 - 1;
            tp.y = p.y - plr->pos.y + Term->hgt / 2 - 1;
            if (dun_pos_valid(cave, p))
            {
                map_char_t mc = {0};

                map_info(p, &mc);
                Term_queue_map_char(tp, &mc);
            }
            else
            {
                Term_putch(tp.x, tp.y, TERM_WHITE, ' ');
            }
        }
    }
}


/*
 * Redraw (on the screen) a given MAP location
 *
 * This function should only be called on "legal" grids
 */
void draw_pos(point_t pos)
{
    if (plr->dun_id != cave->id) return;
    if (cave_pt_is_visible(pos))
    {
        point_t ui = cave_pt_to_ui_pt(pos);

        if (msg_line_contains(ui.y, ui.x)) return;
        if (dun_pos_valid(cave, pos))
        {
            map_char_t mc = {0};
            map_info(pos, &mc);
            Term_queue_map_char(ui, &mc);
            plr->window |= (PW_OVERHEAD | PW_DUNGEON);
        }
    }
}

/*
 * Prints the map of the dungeon
 *
 * Note that, for efficiency, we contain an "optimized" version
 * of both "lite_spot()" and "print_rel()", and that we use the
 * "lite_spot()" function to display the player grid, if needed.
 */
void prt_map_aux(rect_t map_rect)
{
    point_t uip;
    int     v;
    rect_t  msg_rect = msg_line_rect();

    /* 'map_rect' should be a subset of ui_map_rect(), but msg_line_delayed_clear() is
     * actually passing the last msg_line_rect whose top line is out of bounds. Clip it. */
    map_rect = rect_intersect(map_rect, ui_map_rect());
    if (!rect_is_valid(map_rect)) return;

    /* Access the cursor state */
    Term_get_cursor(&v);

    /* Hide the cursor */
    Term_set_cursor(0);

    for (uip = rect_top_left(map_rect); uip.y < map_rect.y + map_rect.cy; uip.y++)
    {
        if (msg_line_contains(uip.y, -1))
        {
            int x = msg_rect.x + msg_rect.cx;
            Term_erase(x, uip.y, map_rect.cx - x);
        }
        else
            Term_erase(uip.x, uip.y, map_rect.cx);
    }

    for (uip.y = map_rect.y; uip.y < map_rect.y + map_rect.cy; uip.y++)
    {
        for (uip.x = map_rect.x; uip.x < map_rect.x + map_rect.cx; uip.x++)
        {
            point_t cp = ui_pt_to_cave_pt(uip);
            map_char_t mc = {0};

            if (msg_line_contains(uip.y, uip.x)) continue;
            if (!dun_pos_valid(cave, cp)) continue;

            map_info(cp, &mc);
            Term_queue_map_char(uip, &mc);
        }
    }

    /* Display player */
    draw_pos(plr->pos);

    /* Restore the cursor */
    Term_set_cursor(v);
}
void prt_map(void)
{
    prt_map_aux(ui_map_rect());
}

static void prt_line_pos(point_t pos, dun_bmp_ptr bmp, term_char_t bc)
{
    point_t ui = cave_pt_to_ui_pt(pos);

    if (dun_bmp_test(bmp, pos)) return;
    dun_bmp_set(bmp, pos);
    if (ui_pt_is_visible(ui) && !msg_line_contains(ui.y, ui.x))
    {
        map_char_t mc = {0};
        map_char_push(&mc, bc);
        Term_queue_map_char(ui, &mc);
    }
}
static void prt_line_aux(dun_line_gen_ptr gen, int range, dun_bmp_ptr bmp, term_char_t bc)
{
    prt_line_pos(dun_line_gen_first(gen), bmp, bc);
    for (;;)
    {
        point_t p = dun_line_gen_next(gen);
        if (!rect_contains_point(bmp->rect, p)) break;
        prt_line_pos(p, bmp, bc);
    }
}
static void prt_line(line_t line)
{
    int gf[5] = { GF_TIME, GF_DARK, GF_ARROW, GF_ACID, GF_COLD };
    int i;
    dun_line_gen_ptr gen;
    dun_bmp_ptr bmp;
    rect_t r;

    if (!line_is_valid(line)) return;

    plr->redraw |= (PR_MAP);
    redraw_stuff();

    r = rect_create_centered(plr->pos, DUN_VIEW_MAX, DUN_VIEW_MAX);
    r = rect_intersect(cave->rect, r);
    gen = dun_line_gen_alloc(line);
    bmp = dun_bmp_alloc(cave, r);
    for (i = 0; i < 5; i++)
    {
        term_char_t bc = blast_char(gf[i]);

        prt_line_aux(gen, DUN_VIEW_MAX, bmp, bc);
        if (!dun_line_gen_next_strategy(gen)) break;
    }
    dun_bmp_free(bmp);
    dun_line_gen_free(gen);
}

/*
 * print project path
 */
static void prt_path_aux(int y, int x, int xtra_flgs);
void prt_path(int y, int x, int xtra_flgs)
{
    if (plr->wizard)
    {
        line_t l;
        l.a = plr->pos;
        l.b.x = x;
        l.b.y = y;
        prt_line(l);
    }
    else prt_path_aux(y, x, xtra_flgs);
}
static void prt_path_aux(int y, int x, int xtra_flgs)
{
    int i;
    dun_path_ptr path = NULL;
    term_char_t bc = blast_char(GF_TIME);
    int range = project_length ? project_length : DUN_PATH_MAX;
    u32b flgs = PROJECT_THRU;

    flgs |= xtra_flgs;

    if (!display_path) return;
    if (-1 == project_length)
        return;

    /* Get projection path */
    path = dun_path_alloc_aux(cave, plr->pos, point_create(x, y), flgs, range);

    plr->redraw |= (PR_MAP);
    redraw_stuff();

    /* Draw path */
    for (i = 0; i < path->count; i++)
    {
        point_t pos = path->points[i];
        point_t ui = cave_pt_to_ui_pt(pos);

        if (ui_pt_is_visible(ui) && !msg_line_contains(ui.y, ui.x))
        {
            map_char_t mc = {0};
            map_info(pos, &mc);
            map_char_push(&mc, bc);
            if (use_graphics || mc.count == 1)
                Term_queue_map_char(ui, &mc);
            else /* ascii does not layer; re-color any intervening monsters; show target loc */
            {
                term_char_t fg = bc;
                term_char_t bg = mc.stack[mc.count - 2];
                if (pos.x == x && pos.y == y) /* target pos */
                    fg.a = TERM_L_DARK;
                else if (dun_mon_at(cave, pos))
                    fg.c = bg.c;
                Term_queue_term_char(ui, fg);
            }
        }

        /* Known Wall */
        if (!(flgs & PROJECT_DISI))
        {
            dun_grid_ptr grid = dun_grid_at(cave, pos);
            if ((grid->flags & CELL_MAP) && !cell_project(grid)) break;
        }
    }
    dun_path_free(path);
}


static cptr simplify_list[][2] =
{
    {"^Ring of ",   "="},
    {"^Amulet of ", "\""},
    {"^Scroll of ", "?"},
    {"^Scroll titled ", "?"},
    {"^Wand of "  , "-"},
    {"^Rod of "   , "-"},
    {"^Staff of " , "_"},
    {"^Potion of ", "!"},
    {" Spellbook ",""},
    {"^Book of ",   ""},
    {" Magic [",   "["},
    {" Book [",    "["},
    {" Arts [",    "["},
    {"^Set of ",    ""},
    {"^Pair of ",   ""},
    {NULL, NULL}
};

static bool _is_dice_boosted(object_type *o_ptr)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    if (o_ptr->dd != k_ptr->dd || o_ptr->ds != k_ptr->ds || o_ptr->mult != k_ptr->mult)
        return TRUE;
    return FALSE;
}

static void display_shortened_item_name(object_type *o_ptr, int y)
{
    char buf[MAX_NLEN + 20];
    char *c = buf;
    int len = 0;
    byte attr;

/*  object_desc(buf, o_ptr, (OD_NO_FLAVOR | OD_OMIT_PREFIX | OD_NAME_AND_DICE)); */
    if (obj_is_weapon(o_ptr) && _is_dice_boosted(o_ptr))
    {
        char tmp[MAX_NLEN];
        object_desc(tmp, o_ptr, (OD_NO_FLAVOR | OD_OMIT_PREFIX | OD_NAME_ONLY));
        sprintf(buf, "%dd%d %s", o_ptr->dd, o_ptr->ds, tmp);
    }
    else if (o_ptr->tval == TV_BOW && _is_dice_boosted(o_ptr))
    {
        char tmp[MAX_NLEN];
        object_desc(tmp, o_ptr, (OD_NO_FLAVOR | OD_OMIT_PREFIX | OD_NAME_ONLY));
        sprintf(buf, "x%d.%2.2d %s", o_ptr->mult / 100, o_ptr->mult % 100, tmp);
    }
    else
        object_desc(buf, o_ptr, (OD_NO_FLAVOR | OD_OMIT_PREFIX | OD_NAME_ONLY));
    attr = tv_color(o_ptr->tval);

    if (plr_tim_find(T_HALLUCINATE))
    {
        attr = TERM_WHITE;
        strcpy(buf, "something strange");
    }

    for (c = buf; *c; c++)
    {
        int i;
        for (i = 0; simplify_list[i][1]; i++)
        {
            cptr org_w = simplify_list[i][0];

            if (*org_w == '^')
            {
                if (c == buf)
                    org_w++;
                else
                    continue;
            }

            if (!strncmp(c, org_w, strlen(org_w)))
            {
                char *s = c;
                cptr tmp = simplify_list[i][1];
                while (*tmp)
                    *s++ = *tmp++;
                tmp = c + strlen(org_w);
                while (*tmp)
                    *s++ = *tmp++;
                *s = '\0';
            }
        }
    }

    c = buf;
    len = 0;
    while(*c)
    {
        {
            if(len + 1 > 12) break;
            c++;
            len++;
        }
    }
    *c='\0';
    Term_putstr(0, y, 12, attr, buf);
}

/*
 * Display a "small-scale" map of the dungeon in the active Term
 */
#define _ROW_MAP                  0
#define _COL_MAP                  12


void display_map(int *cy, int *cx)
{
    int i, j, x, y;

    byte ta;
    char tc;

    byte tp;

    byte **bigma;
    char **bigmc;
    byte **bigmp;

    byte **ma;
    char **mc;
    byte **mp;

    /* Save lighting effects */
    bool old_view_lite = view_light;

    int hgt, wid, yrat, xrat;

    int **match_autopick_yx;
    object_type ***object_autopick_yx;

    /* Get size */
    Term_get_size(&wid, &hgt);
    hgt -= 2;
    wid -= 14;
    if (use_bigtile) wid /= 2;

    yrat = (cave->rect.cy + hgt - 1) / hgt;
    xrat = (cave->rect.cx + wid - 1) / wid;

    /* Disable lighting effects */
    view_light = FALSE;

    /* Allocate the maps */
    C_MAKE(ma, (hgt + 2), byte_ptr);
    C_MAKE(mc, (hgt + 2), char_ptr);
    C_MAKE(mp, (hgt + 2), byte_ptr);
    C_MAKE(match_autopick_yx, (hgt + 2), sint_ptr);
    C_MAKE(object_autopick_yx, (hgt + 2), object_type **);

    /* Allocate and wipe each line map */
    for (y = 0; y < (hgt + 2); y++)
    {
        /* Allocate one row each array */
        C_MAKE(ma[y], (wid + 2), byte);
        C_MAKE(mc[y], (wid + 2), char);
        C_MAKE(mp[y], (wid + 2), byte);
        C_MAKE(match_autopick_yx[y], (wid + 2), int);
        C_MAKE(object_autopick_yx[y], (wid + 2), object_type *);

        for (x = 0; x < wid + 2; ++x)
        {
            match_autopick_yx[y][x] = -1;
            object_autopick_yx[y][x] = NULL;

            /* Nothing here */
            ma[y][x] = TERM_WHITE;
            mc[y][x] = ' ';

            /* No priority */
            mp[y][x] = 0;
        }
    }

    /* Allocate the maps */
    C_MAKE(bigma, (cave->rect.cy + 2), byte_ptr);
    C_MAKE(bigmc, (cave->rect.cy + 2), char_ptr);
    C_MAKE(bigmp, (cave->rect.cy + 2), byte_ptr);

    /* Allocate and wipe each line map */
    for (y = 0; y < (cave->rect.cy + 2); y++)
    {
        /* Allocate one row each array */
        C_MAKE(bigma[y], (cave->rect.cx + 2), byte);
        C_MAKE(bigmc[y], (cave->rect.cx + 2), char);
        C_MAKE(bigmp[y], (cave->rect.cx + 2), byte);

        for (x = 0; x < cave->rect.cx + 2; ++x)
        {
            /* Nothing here */
            bigma[y][x] = TERM_WHITE;
            bigmc[y][x] = ' ';

            /* No priority */
            bigmp[y][x] = 0;
        }
    }

    /* Fill in the map. Don't assume cave->rect anchors at the origin. */
    for (i = 0; i < cave->rect.cx; ++i)
    {
        for (j = 0; j < cave->rect.cy; ++j)
        {
            point_t pos = point_create(cave->rect.x + i, cave->rect.y + j);
            map_char_t mc = {0};
            term_char_t fg;

            /* Location */
            x = i / xrat + 1;
            y = j / yrat + 1;

            match_autopick=-1;
            autopick_obj=NULL;

            /* Extract the current attr/char at that map location */
            map_info(pos, &mc);
            fg = map_char_top(&mc);

            /* Extract the priority */
            tp = mc.priority;

            if(match_autopick!=-1
               && (match_autopick_yx[y][x] == -1
                   || match_autopick_yx[y][x] > match_autopick))
            {
                match_autopick_yx[y][x] = match_autopick;
                object_autopick_yx[y][x] = autopick_obj;
                tp = 0x7f;
            }

            /* Save the char, attr and priority */
            bigmc[j+1][i+1] = fg.c;
            bigma[j+1][i+1] = fg.a;
            bigmp[j+1][i+1] = tp;
        }
    }

    for (j = 0; j < cave->rect.cy; ++j)
    {
        for (i = 0; i < cave->rect.cx; ++i)
        {
            /* Location */
            x = i / xrat + 1;
            y = j / yrat + 1;

            tc = bigmc[j+1][i+1];
            ta = bigma[j+1][i+1];
            tp = bigmp[j+1][i+1];

            /* rare feature has more priority */
            if (mp[y][x] == tp)
            {
                int t;
                int cnt = 0;

                for (t = 0; t < 8; t++)
                {
                    if (tc == bigmc[j+1+ddy_cdd[t]][i+1+ddx_cdd[t]] &&
                        ta == bigma[j+1+ddy_cdd[t]][i+1+ddx_cdd[t]])
                        cnt++;
                }
                if (cnt <= 4)
                    tp++;
            }

            /* Save "best" */
            if (mp[y][x] < tp)
            {
                /* Save the char, attr and priority */
                mc[y][x] = tc;
                ma[y][x] = ta;
                mp[y][x] = tp;
            }
        }
    }


    /* Corners */
    x = wid + 1;
    y = hgt + 1;

    /* Draw the corners */
    mc[0][0] = mc[0][x] = mc[y][0] = mc[y][x] = '+';

    /* Draw the horizontal edges */
    for (x = 1; x <= wid; x++) mc[0][x] = mc[y][x] = '-';

    /* Draw the vertical edges */
    for (y = 1; y <= hgt; y++) mc[y][0] = mc[y][x] = '|';


    /* Display each map line in order */
    for (y = 0; y < hgt + 2; ++y)
    {
        /* Start a new line */
        Term_gotoxy(_COL_MAP, y);

        /* Display the line */
        for (x = 0; x < wid + 2; ++x)
        {
            ta = ma[y][x];
            tc = mc[y][x];

            /* Add the character */
            Term_add_bigch(ta, tc);
        }
    }


    for (y = 1; y < hgt + 1; ++y)
    {
      match_autopick = -1;
      for (x = 1; x <= wid; x++){
        if (match_autopick_yx[y][x] != -1 &&
        (match_autopick > match_autopick_yx[y][x] ||
         match_autopick == -1)){
          match_autopick = match_autopick_yx[y][x];
          autopick_obj = object_autopick_yx[y][x];
        }
      }

      /* Clear old display */
      Term_putstr(0, y, 12, 0, "            ");

      if (match_autopick != -1)
#if 1
          display_shortened_item_name(autopick_obj, y);
#else
      {
          char buf[13] = "\0";
          strncpy(buf,autopick_list[match_autopick].name,12);
          buf[12] = '\0';
          put_str(buf,y,0);
      }
#endif

    }

    /* Player location */
        (*cy) = plr->pos.y / yrat + 1 + _ROW_MAP;
    if (!use_bigtile)
        (*cx) = plr->pos.x / xrat + 1 + _COL_MAP;
    else
        (*cx) = (plr->pos.x / xrat + 1) * 2 + _COL_MAP;

    /* Restore lighting effects */
    view_light = old_view_lite;

    /* Free each line map */
    for (y = 0; y < (hgt + 2); y++)
    {
        /* Free one row each array */
        C_FREE(ma[y], (wid + 2), byte);
        C_FREE(mc[y], (wid + 2), char);
        C_FREE(mp[y], (wid + 2), byte);
        C_FREE(match_autopick_yx[y], (wid + 2), int);
        C_FREE(object_autopick_yx[y], (wid + 2), object_type **);
    }

    /* Free each line map */
    C_FREE(ma, (hgt + 2), byte_ptr);
    C_FREE(mc, (hgt + 2), char_ptr);
    C_FREE(mp, (hgt + 2), byte_ptr);
    C_FREE(match_autopick_yx, (hgt + 2), sint_ptr);
    C_FREE(object_autopick_yx, (hgt + 2), object_type **);

    /* Free each line map */
    for (y = 0; y < (cave->rect.cy + 2); y++)
    {
        /* Free one row each array */
        C_FREE(bigma[y], (cave->rect.cx + 2), byte);
        C_FREE(bigmc[y], (cave->rect.cx + 2), char);
        C_FREE(bigmp[y], (cave->rect.cx + 2), byte);
    }

    /* Free each line map */
    C_FREE(bigma, (cave->rect.cy + 2), byte_ptr);
    C_FREE(bigmc, (cave->rect.cy + 2), char_ptr);
    C_FREE(bigmp, (cave->rect.cy + 2), byte_ptr);
}


/*
 * Display a "small-scale" map of the dungeon for the player
 *
 * Currently, the "player" is displayed on the map. XXX XXX XXX
 */
void do_cmd_view_map(void)
{
    int cy, cx;


    /* Save the screen */
    screen_save();

    /* Note */
    prt("Please wait...", 0, 0);

    /* Flush */
    Term_fresh();

    /* Clear the screen */
    Term_clear();

    display_autopick = 0;

    /* Display the map */
    display_map(&cy, &cx);

    /* Wait for it */
    if(max_autopick)
    {
        display_autopick = ITEM_DISPLAY;

        while (1)
        {
            int i;
            byte flag = 0;

            int wid, hgt, row_message;

            Term_get_size(&wid, &hgt);
            row_message = hgt - 1;

            if (cave->type->id == D_SURFACE)
                put_str(" Hit M to Display the World Map.", row_message, 1);
            else
                put_str(" Hit M, N(for ~), K(for !), or D(same as M+N) to display auto-picker items.", row_message, 1);

            /* Hilite the player */
            move_cursor(cy, cx);

            i = inkey();

            if ('M' == i)
            {
                if (cave->type->id == D_SURFACE)
                    dun_world_map_ui();
                else
                    flag = (DO_AUTOPICK | DO_QUERY_AUTOPICK);
            }
            else if ('|' == i)
            {
                dun_dump(cave, "map.html", DOC_FORMAT_HTML);
                continue;
            }
            else if ('N' == i)
                flag = DONT_AUTOPICK;
            else if ('K' == i)
                flag = DO_AUTODESTROY;
            else if ('D' == i)
                flag = (DO_AUTOPICK | DO_QUERY_AUTOPICK | DONT_AUTOPICK);
            else
                break;

            Term_fresh();

            if (~display_autopick & flag)
                display_autopick |= flag;
            else
                display_autopick &= ~flag;
            /* Display the map */
            display_map(&cy, &cx);
        }

        display_autopick = 0;

    }
    else
    {
        put_str("Hit any key to continue", 23, 30);
        move_cursor(cy, cx);
        inkey();
    }

    /* Restore the screen */
    screen_load();
}

/*
 * Hack -- map the current panel (plus some) ala "magic mapping"
 */
static int _map_range;
static void _map_grid(point_t pos, dun_grid_ptr grid)
{
    int      i;

    if (plr_distance(pos) > _map_range) return;

    /* All non-walls are "checked" */
    if (grid->type != FEAT_WALL)
    {
        grid->flags |= CELL_AWARE;
        if (!cell_is_boring(grid))
            grid->flags |= CELL_MAP;

        /* Memorize known walls */
        for (i = 0; i < 8; i++)
        {
            point_t  p = point_step(pos, ddd[i]);
            dun_grid_ptr g = dun_grid_at(cave, p); 

            if (!cell_is_boring(g))
                g->flags |= CELL_MAP | CELL_AWARE;
        }
    }
}
void map_area(int range)
{
    rect_t r = rect_create_centered(plr->pos, range, range);
    rect_t cr = rect_deflate(cave->rect, 1, 1); /* interior only ... _map_grid does not bounds check! */
    r = rect_intersect(cr, r);
    _map_range = range;
    dun_iter_rect(cave, r, _map_grid);
    plr->redraw |= (PR_MAP);
    plr->window |= (PW_OVERHEAD | PW_DUNGEON);
}

/*
 * Light up the dungeon using "clairvoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", memorizes all grids as with magic mapping, and, under the
 * standard option settings (view_perma_grids but not view_torch_grids)
 * memorizes all floor grids too.
 *
 * Note that if "view_perma_grids" is not set, we do not memorize floor
 * grids, since this would defeat the purpose of "view_perma_grids", not
 * that anyone seems to play without this option.
 *
 * Note that if "view_torch_grids" is set, we do not memorize floor grids,
 * since this would prevent the use of "view_torch_grids" as a method to
 * keep track of what grids have been observed directly.
 */
static bool _wiz_lite; /* flag whether or not to illuminate grids */
static void _wiz_lite_obj(point_t pos, obj_ptr obj)
{
    obj->marked |= OM_FOUND;
}
static void _wiz_lite_grid(point_t pos, dun_grid_ptr grid)
{
    if (grid->type != FEAT_WALL)
    {
        int i;
        /* Scan all neighbors */
        for (i = 0; i < 9; i++)
        {
            point_t      adj_pos = point_step(pos, ddd[i]);
            dun_grid_ptr adj_grid = dun_grid_at(cave, adj_pos); /* XXX */

            adj_grid->flags |= CELL_AWARE;

            /* Perma-lite the grid (unless Ninja or Eye of Vecna) */
            if (_wiz_lite)
                adj_grid->flags |= CELL_LIT;

            /* Memorize normal features */
            if (!cell_is_boring(adj_grid))
                adj_grid->flags |= CELL_MAP;

            /* Perma-lit grids (newly and previously) */
            else if (adj_grid->flags & CELL_LIT)
            {
                /* Normally, memorize floors (see above) */
                if (view_perma_grids && !view_torch_grids)
                    adj_grid->flags |= CELL_MAP;
            }
        }
    }
}
static void _wiz_lite_surface(point_t pos, dun_grid_ptr grid)
{
    grid->flags |= CELL_MAP;
    if (_wiz_lite)
        grid->flags |= CELL_LIT;
}
static void _wiz_lite_aux(void)
{
    dun_iter_floor_obj(cave, _wiz_lite_obj);
    /* Optimize the surface: This assumes there are no "rooms". The
     * surface contains 9*8k cells, and _wiz_lite_grid enumerates the
     * 8 adjacent grids for each of these, giving over 500k cell accesses
     * (dun_grid_at). Look at dun_grid_at ... it's not slow, but it ain't
     * exactly fast. Now look at dun_iter_grids! */
    if (cave->type->id == D_SURFACE)
        dun_iter_grids(cave, _wiz_lite_surface);
    else
        dun_iter_interior(cave, _wiz_lite_grid);
    plr->update |= PU_MONSTERS | PU_LIGHT;
    plr->redraw |= PR_MAP;
    plr->window |= PW_OVERHEAD | PW_DUNGEON | PW_OBJECT_LIST;

}
void wiz_lite(void) /* e.g. !Enlightenment or Sorcery's Clairvoyance spell */
{
    _wiz_lite = cave->type->id != D_DARK_TOWER;
    _wiz_lite_aux();
}
void wiz_map(void) /* e.g. Eye of Vecna or Ninja's Detect Near power */
{
    _wiz_lite = FALSE;
    _wiz_lite_aux();
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
static void _wiz_dark_obj(point_t pos, obj_ptr obj)
{
    obj->marked &= (OM_TOUCHED | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED);
}
static void _wiz_dark_interior(point_t pos, dun_grid_ptr grid)
{
    grid->flags &= ~(CELL_MAP | CELL_DETECT);
    grid->flags |= CELL_UNSAFE;
}
static void _wiz_dark_border(point_t pos, dun_grid_ptr grid)
{
    grid->flags &= ~CELL_MAP;
}
void wiz_dark(void)
{
    dun_iter_interior(cave, _wiz_dark_interior);
    dun_iter_boundary(cave, _wiz_dark_border);
    dun_iter_floor_obj(cave, _wiz_dark_obj);

    plr->update |= PU_UN_VIEW | PU_UN_LIGHT;
    plr->update |= PU_VIEW | PU_LIGHT | PU_MON_LIGHT;
    plr->update |= PU_MONSTERS;
    plr->redraw |= PR_MAP;
    plr->window |= PW_OVERHEAD | PW_DUNGEON | PW_OBJECT_LIST;
}

/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming no monster gets in the way.
 *
 * This is slightly (but significantly) different from "los(y1,x1,y2,x2)".
 */
bool point_project_aux(point_t p1, point_t p2, u32b flags)
{
    int rng = project_length ? project_length : MAX_RANGE;
    return dun_project_aux(cave, p1, p2, flags, rng);
}
bool point_project(point_t p1, point_t p2)
{
    return point_project_aux(p1, p2, 0);
}
bool plr_project(point_t p)
{
    if (plr->dun_id != cave->id) return FALSE;
    return point_project(plr->pos, p); /* plr not fooled by CAVE_ILLUSION */
}
bool mon_project(mon_ptr mon, point_t p)
{
    if (mon->dun != cave) return FALSE;
    return point_project_aux(mon->pos, p, PROJECT_ILLUSION); /* monster fooled by CAVE_ILLUSION */
}
bool plr_project_mon(mon_ptr mon)
{
    if (plr->dun_id != mon->dun->id) return FALSE;
    return plr_project(mon->pos);
}
bool mon_project_plr(mon_ptr mon) /* note mon_project_plr != plr_project_mon (CAVE_ILLUSION) */
{
    if (plr->dun_id != mon->dun->id) return FALSE;
    return mon_project(mon, plr->pos);
}

/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location.
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 */
point_t scatter(point_t pos, int d)
{
    point_t p;
    assert(d >= 0);
    for (;;)
    {
        p = point_random_jump(pos, d);
        if (!dun_pos_interior(cave, p)) continue;
        if (d > 1 && point_distance(pos, p) > d) continue;
        if (point_project(pos, p)) break;
    }
    return p;
}

/*
 * Track a new monster
 */
void health_track(mon_ptr mon)
{
    if (mon)
    {
        if (mon->id == plr->riding) return; /* already tracked */
        plr->health_who = mon->id;
    }
    else
        plr->health_who = 0;
    plr->redraw |= PR_HEALTH_BARS;
}



/*
 * Hack -- track the given monster race
 */
void mon_track(mon_ptr mon)
{
    if (!(mon->mflag2 & MFLAG2_FUZZY))
        monster_race_track(mon->apparent_race);
}
void monster_race_track(mon_race_ptr race)
{
    plr->monster_race_idx = race->id;
    plr->window |= PW_MONSTER;
}

/*
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
    /* Save this monster ID */
    plr->object_kind_idx = k_idx;

    /* Window stuff */
    plr->window |= (PW_OBJECT);
}



/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * The second arg is currently unused, but could induce output flush.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(int stop_search, int unused_flag)
{
    /* Cancel auto-commands */
    /* command_new = 0; */

    /* Cancel repeated commands */
    if (command_rep)
    {
        /* Cancel */
        command_rep = 0;

        /* Redraw the state (later) */
        plr->redraw |= (PR_STATE);
    }

    /* Cancel Resting */
    if ( plr->action == ACTION_REST
      || plr->action == ACTION_GLITTER
      || (stop_search && plr->action == ACTION_SEARCH) )
    {
        /* Cancel */
        set_action(ACTION_NONE);
    }

    /* Cancel running */
    if (running)
    {
        /* Cancel */
        running = 0;

        /* Check for new panel if appropriate */
        if (center_player && !center_running) viewport_verify();

        /* Calculate torch radius */
        plr->update |= (PU_TORCH);

        /* Update monster flow */
        plr->update |= (PU_FLOW);
    }

    if (travel.run)
    {
        travel_cancel();

        /* Check for new panel if appropriate */
        if (center_player && !center_running) viewport_verify();

        /* Calculate torch radius */
        plr->update |= (PU_TORCH);
        plr->update |= (PU_FLOW);  /* cf dun_update_flow ... force a recalc */
    }

    /* Flush the input if requested */
    if (flush_disturb) flush();
}


