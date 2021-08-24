#include "angband.h"

#include <assert.h>

/************************************************************************
 * Throw Object
 ***********************************************************************/
static void    _animate(plr_throw_ptr context);
static bool    _hit_mon(plr_throw_ptr context, mon_ptr mon);
static bool    _hit_wall(plr_throw_ptr context);
static obj_ptr _get_obj(int type);
static bool    _get_target(plr_throw_ptr context);
static bool    _init_context(plr_throw_ptr context);
static void    _throw(plr_throw_ptr context);
static void    _return(plr_throw_ptr context);

bool plr_throw(plr_throw_ptr context)
{
    if (!_init_context(context)) return FALSE;
    _throw(context);
    _return(context);
    return TRUE;
}

static void _animate(plr_throw_ptr context)
{
    point_t p = context->path[context->path_pos];

    if (cave_pt_is_visible(p) && plr_can_see(p))
    {
        char c = object_char(context->obj);
        byte a = object_attr(context->obj);

        /* Draw, Hilite, Fresh, Pause, Erase */
        print_rel(c, a, p.y, p.x);
        move_cursor_relative(p);
        Term_fresh();
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
        draw_pos(p);
        Term_fresh();
    }
    else
    {
        /* Pause anyway, for consistancy */
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
    }
}

static obj_ptr _get_obj(int type)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Throw which item?";
    prompt.error = "You have nothing to throw.";
    if (type & THROW_BOOMERANG)
    {
        slot_t slot = equip_find_first(obj_is_weapon);
        if (slot && ((type & THROW_DISPLAY) || plr->weapon_ct == 1))
            return equip_obj(slot);
        prompt.where[0] = INV_EQUIP;
        prompt.filter = obj_is_weapon;
    }
    else
    {
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_EQUIP;
        prompt.where[2] = INV_FLOOR;
    }
    obj_prompt(&prompt);
    return prompt.obj;
}

static bool _get_target(plr_throw_ptr context)
{
    point_t tgt;
    if (context->dir == 5 && !target_okay())
        context->dir = 0;
    if (!context->dir)
    {
        project_length = context->range;
        if (!get_fire_dir(&context->dir)) return FALSE;
        project_length = 0;
    }
    if (context->dir == DIR_RANDOM) /* Shuriken Spreading (ninja.c) */
        tgt = point_random_jump(plr->pos, 50);
    else if (context->dir == DIR_TARGET && target_okay())
        tgt = who_pos(plr->target);
    else
        tgt = point_jump(plr->pos, context->dir, 99);

    if (point_equals(plr->pos, tgt)) return FALSE;

    assert(context->range <= MAX_SIGHT);
    context->path_ct = project_path(context->path,
        context->range, plr->pos, tgt, 0);
    context->path_pos = 0;
    if (!context->path_ct) return FALSE;
    return TRUE;
}

static bool _init_context(plr_throw_ptr context)
{
    /* object */
    if (!context->obj)
    {
        context->obj = _get_obj(context->type);
        if (!context->obj) return FALSE;
        if ( context->obj->loc.where == INV_EQUIP
          && obj_has_flag(context->obj, OF_NO_REMOVE) )
        {
            msg_print("You cannot throw that item!");
            return FALSE;
        }
        if ( context->obj->loc.where == INV_EQUIP
          && context->obj->tval == TV_QUIVER
          && quiver_count(NULL) )
        {
            msg_print("Your quiver still holds ammo. Remove all the ammo from your quiver first.");
            return FALSE;
        }
    }
    obj_flags(context->obj, context->flags);
    object_desc(context->obj_name, context->obj, OD_NAME_ONLY | OD_OMIT_PREFIX | OD_OMIT_INSCRIPTION);

    /* energy and take a turn */
    if (!(context->type & THROW_DISPLAY))
    {
        if (!context->energy)
        {
            context->energy = 100;
            if (plr->pclass == CLASS_ROGUE || plr->pclass == CLASS_NINJA)
                context->energy -= plr->lev;
        }
        energy_use = context->energy;
        if (!fear_allow_shoot())
        {
            msg_print("You are too scared!");
            return FALSE;
        }
        if (context->obj->loc.where == INV_EQUIP && !equip_can_takeoff(context->obj))
            return FALSE;
    }

    /* multiplier (required for range calc) */
    if (!context->mult)
        context->mult = 100;
    if (plr->mighty_throw)
        context->mult += 100;
    if (have_flag(context->flags, OF_THROWING))
        context->mult += 100;
    context->mult = context->mult * (100 + adj_str_td[plr->stat_ind[A_STR]] - 128) / 100;

    /* range */
    if (!context->range)
    {
        int mul, div, rng;

        mul = 10 + 2 * (context->mult - 100) / 100;
        div = context->obj->weight > 10 ? context->obj->weight : 10;
        if (have_flag(context->flags, OF_THROWING) || (context->type & THROW_BOOMERANG))
            div /= 2;

        rng = (adj_str_blow[plr->stat_ind[A_STR]] + 20) * mul / div;
        if (rng > mul) rng = mul;
        if (rng < 5) rng = 5;

        context->range = rng;
    }
    context->range = MIN(MAX_RANGE, context->range);

    /* target */
    if (!(context->type & THROW_DISPLAY))
    {
        if (!_get_target(context)) return FALSE;
    }

    /* boomerang? */
    context->come_back = FALSE;
    context->fail_catch = FALSE;
    if ( obj_is_specified_art(context->obj, "\\.Mjollnir")
      || obj_is_specified_art(context->obj, "\\.Aegis Fang") )
    {
        context->type |= THROW_BOOMERANG;
        context->back_chance += 20; /* upgrade the odds if already Boomerang */
    }
    if (context->type & THROW_BOOMERANG)
    {
        context->back_chance += adj_dex_th[plr->stat_ind[A_DEX]] - 128;
        if (!(context->type & THROW_DISPLAY))
        {
            int oops = 100;
            context->back_chance += randint1(30);
            if (context->back_chance > 30 && !one_in_(oops))
            {
                context->come_back = TRUE;
                if (plr_tim_find(T_BLIND) || plr_tim_find(T_HALLUCINATE) || plr_tim_find(T_CONFUSED) || one_in_(oops))
                    context->fail_catch = TRUE;
                else
                {
                    oops = 37;
                    if (plr_tim_find(T_STUN))
                        oops += 10;
                    if (context->back_chance <= oops)
                        context->fail_catch = TRUE;
                }
            }
        }
    }

    /* skill and some ninja hacks (hengband) */
    context->skill += plr->skill_tht + (plr->shooter_info.to_h + context->obj->to_h) * BTH_PLUS_ADJ;
    if (plr->pclass == CLASS_NINJA)
    {
        if ( context->obj->tval == TV_SPIKE
          || (context->obj->tval == TV_SWORD && have_flag(context->flags, OF_THROWING)) )
        {
            context->skill *= 2; /* wow! */
            context->to_d += ((plr->lev+30)*(plr->lev+30)-900)/55; /* +100 at CL50 */
        }
    }
    if (plr_tim_find(T_STUN))
        context->skill -= context->skill * MIN(100, plr_tim_amount(T_STUN)) / 150;

    return TRUE;
}

/* Randomize the multiplier a bit */
static int _tries(plr_throw_ptr context)
{
    int tries = context->mult/100;
    if (randint0(100) < context->mult%100) tries++;
    return MAX(1, tries);
}
static int _hits(plr_throw_ptr context, mon_ptr mon)
{
    int hits = 0;
    int tries = _tries(context);
    int skill = context->skill - (context->path_pos + 1);
    int ac = mon_ac(mon);
    int i;
    if (test_hit_fire(skill, ac, mon->ml))
    {
        hits++;
        for (i = 1; i < tries; i++)
        {
            if (test_hit_fire(skill, ac, mon->ml))
                hits++;
        }
    }
    return hits;
}
static bool _hit_mon(plr_throw_ptr context, mon_ptr mon)
{
    bool visible = mon->ml;
    int  skill = context->skill - (context->path_pos + 1);
    int  hits = _hits(context, mon);
    int  tdam = 0;

    if (hits)
    {
        bool fear = FALSE;
        bool ambush = mon_tim_find(mon, MT_SLEEP) && visible && plr->ambush;
        int  to_d = 0;
        slay_t slay = {0};

        if (!visible)
            msg_format("The %s finds a mark.", context->obj_name);
        else
        {
            char m_name[80];
            monster_desc(m_name, mon, 0);
            if (ambush)
                cmsg_format(TERM_RED, "The %s cruelly hits %s.", context->obj_name, m_name);
            else if (plr->wizard)
                msg_format("The %s hits %s (x%d).", context->obj_name, m_name, hits);
            else
                msg_format("The %s hits %s.", context->obj_name, m_name);
            if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
            health_track(mon);
        }

        /***** The Damage Calculation!!! *****/
        tdam = damroll(context->obj->dd + context->to_dd, context->obj->ds);
        if (have_flag(context->flags, OF_VORPAL) || have_flag(context->flags, OF_VORPAL2))
        {
            int  vorpal_chance = have_flag(context->flags, OF_VORPAL2) ? 2 : 4;
            if (one_in_(vorpal_chance * 3 / 2))
            {
                int mult = 2;
                char m_name[80];

                while (one_in_(vorpal_chance))
                    mult++;

                tdam *= mult;

                monster_desc(m_name, mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
                switch (mult)
                {
                case 2: msg_format("Your weapon <color:U>gouges</color> %s!", m_name); break;
                case 3: msg_format("Your weapon <color:y>maims</color> %s!!", m_name); break;
                case 4: msg_format("Your weapon <color:R>carves</color> %s!!!", m_name); break;
                case 5: msg_format("Your weapon <color:r>cleaves</color> %s!!!!", m_name); break;
                case 6: msg_format("Your weapon <color:v>smites</color> %s!!!!!", m_name); break;
                case 7: msg_format("Your weapon <color:v>eviscerates</color> %s!!!!!!", m_name); break;
                default: msg_format("Your weapon <color:v>shreds</color> %s!!!!!!!", m_name); break;
                }

                if (have_flag(context->flags, OF_VORPAL2))
                    obj_learn_slay(context->obj, OF_VORPAL2, "is <color:v>*Sharp*</color>");
                else
                    obj_learn_slay(context->obj, OF_VORPAL, "is <color:R>Sharp</color>");
            }
        }
        slay = crit_aux(CRIT_FREQ_ROLL, context->obj->weight, skill);
        if (slay.id)
        {
            tdam = tdam * slay.mul/100;
            to_d += slay.add;
            if (slay.msg) msg_print(slay.msg);
        }
        slay = slay_mon(context->flags, mon, context->obj);
        if (slay.id)
        {
            tdam = tdam * slay.mul / 100;
            to_d += slay.add;
        }
        tdam += to_d + context->obj->to_d;
        tdam = tdam * hits;
        if (ambush)
            tdam *= 2;
        tdam += context->to_d;

        if (context->mod_damage_f)
            tdam = context->mod_damage_f(context, tdam);

        if (plr_tim_find(T_STUN))
            tdam -= tdam * MIN(100, plr_tim_amount(T_STUN)) / 150;

        if (tdam < 0) tdam = 0;
        tdam = mon_damage_mod(mon, tdam, FALSE);
        context->dam = tdam;
        if (mon_take_hit(mon, tdam, &fear, extract_note_dies(real_r_ptr(mon))))
        {
            /* Dead monster */
        }
        else
        {
            if (have_flag(context->flags, OF_BRAND_VAMP))
            {
                char m_name[80];
                int  heal = MIN(30, damroll(3, tdam / 8));
                monster_desc(m_name, mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
                msg_format("Your weapon drains life from %s!", m_name);
                hp_player_aux(heal);
                obj_learn_slay(context->obj, OF_BRAND_VAMP, "is <color:D>Vampiric</color>");
            }
            message_pain(mon->id, tdam);
            if (tdam > 0)
            {
                anger_monster(mon);
                mon_set_target(mon, plr->pos);
            }

            if (tdam > 0 && mon->cdis > 1 && allow_ticked_off(mon->race))
            {
                if (!plr->stealthy_snipe)
                    mon_anger_shoot(mon, tdam);
            }

            if (fear && mon->ml)
            {
                char m_name[80];
                sound(SOUND_FLEE);
                monster_desc(m_name, mon, MD_PRON_VISIBLE);
                msg_format("%^s flees in terror!", m_name);
            }
            if (context->after_hit_f)
                context->after_hit_f(context, mon->id);
        }
        if (!(context->type & THROW_BOOMERANG))
            context->break_chance = breakage_chance(context->obj);
    }
    #if 0
    {
        rect_t r = ui_char_info_rect();
        int    a = TERM_WHITE;
        if (tdam > 1000) a = TERM_VIOLET;
        else if (tdam > 750) a = TERM_RED;
        else if (tdam > 500) a = TERM_L_RED;
        else if (tdam > 250) a = TERM_YELLOW;
        else if (tdam > 100) a = TERM_L_UMBER;
        c_put_str(a, format("Dam: %4d", tdam), r.y + r.cy - 3, r.x);
        /*cmsg_format(a, "You did %d total damage.", tdam);*/
    }
    #endif
    return TRUE;
}

static bool _hit_wall(plr_throw_ptr context)
{
    if (!(context->type & THROW_BOOMERANG))
        context->break_chance = breakage_chance(context->obj);
    return TRUE;
}

static void _throw(plr_throw_ptr context)
{
    assert(context->path_ct);
    for (context->path_pos = 0; ; context->path_pos++)
    {
        point_t p = context->path[context->path_pos];
        mon_ptr mon = dun_mon_at(cave, p);

        _animate(context);

        if (mon)
        {
            if (_hit_mon(context, mon)) break;
        }

        if (!cell_project(dun_cell_at(cave, p)))
        {
            if (_hit_wall(context)) break;
        }

        /* careful ... leave path_pos on the last processed square
         * so we can drop the object if needed */
        if (context->path_pos == context->path_ct - 1) break;
    }
    if (plr->pclass == CLASS_NINJA && context->obj->tval == TV_SPIKE)
        stats_on_use(context->obj, 1);
}

static void _return(plr_throw_ptr context)
{
    /* animation for the return */
    if (context->come_back)
    {
        for (; context->path_pos >= 0; context->path_pos--)
            _animate(context);
        msg_format("Your %s comes back to you.", context->obj_name);
        if (object_is_(context->obj, TV_POLEARM, SV_DEATH_SCYTHE) && (one_in_(2) || context->fail_catch))
            death_scythe_miss(context->obj, HAND_NONE, context->skill);
    }

    /* handle fumbling, dropping and catching */
    if (!context->come_back || context->fail_catch)
    {
        /* only message/warn if the user expects a return */
        if (context->type & THROW_BOOMERANG)
        {
            if (!context->come_back)
                msg_format("Your %s fails to return!", context->obj_name);

            if (context->fail_catch)
                cmsg_print(TERM_VIOLET, "But you can't catch!");

            if (context->obj->loc.where == INV_EQUIP)
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
        }
        if (!context->come_back)
        {
            point_t p = context->path[context->path_pos];
            /* Figurines transform into pets (enemies if cursed) */
            if (context->obj->tval == TV_FIGURINE)
            {
                if (!summon_named_creature(who_create_null(), p, mon_race_lookup(context->obj->race_id),
                                !obj_is_cursed(context->obj) ? PM_FORCE_PET : 0))
                    msg_print("The Figurine writhes and then shatters.");

                else if (obj_is_cursed(context->obj))
                    msg_print("You have a bad feeling about this.");

                context->obj->number--;
                obj_release(context->obj, OBJ_RELEASE_QUIET);
            }
            /* potions shatter with effects if they hit a wall or monster */
            else if (context->obj->tval == TV_POTION && randint0(100) < context->break_chance)
            {
                msg_format("The %s shatters!", context->obj_name);
                if (potion_smash_effect(who_create_null(), p, context->obj->k_idx))
                {
                    /* I think this needs fixing in project_m ... */
                    mon_ptr mon = dun_mon_at(cave, p);
                    if (mon && mon_is_friendly(mon) && !mon_tim_find(mon, T_INVULN))
                    {
                        char m_name[80];
                        monster_desc(m_name, mon, 0);
                        msg_format("%^s gets angry!", m_name);
                        set_hostile(mon);
                    }
                }
                context->obj->number--;
                obj_release(context->obj, OBJ_RELEASE_QUIET);
            }
            /* everything else drops (perhaps breaks) at the end of the path */
            else
                obj_drop_at(context->obj, 1, p.x, p.y, context->break_chance);
        }
        else
            obj_drop_at(context->obj, 1, plr->pos.x, plr->pos.y, context->break_chance);
        context->obj = NULL;
    }
    else
    {
        /* The thrown object returned and the player caught it.
         * This means it stays equipped (or, rarely, in the pack)
         * so no change is required. */
    }
}

/************************************************************************
 * Document Throw Weapon for Character Sheet
 ***********************************************************************/
static void _display_weapon_slay(int base_mul, slay_t slay, bool force, int throw_mul, int num_throw,
                                 int dd, int ds, int to_d, cptr name, int color, doc_ptr doc)
{
    int mul, dam;

    mul = slay.mul;
    if (force)
        mul = mul * 3/2 + 150;
    mul = mul * base_mul / 100;

    dam = mul * dd * (ds + 1)/200 + to_d;
    dam = throw_mul * dam / 100;
    if (plr_tim_find(T_STUN))
        dam -= dam * MIN(100, plr_tim_amount(T_STUN)) / 150;

    doc_printf(doc, "<color:%c> %-7.7s</color>", attr_to_attr_char(color), name);
    doc_printf(doc, ": %d/%d [%d.%02dx]\n",
                    dam, num_throw * dam / 100,
                    mul/100, mul%100);
}

static slay_t _slay(int mul)
{
    slay_t slay = {0};
    slay.id = OF_FAKE;
    slay.mul = mul;
    return slay;
}
void plr_throw_display(plr_throw_ptr context)
{
    doc_ptr doc = doc_alloc(80);
    plr_throw_doc(context, doc);
    if (doc_line_count(doc))
    {
        /* screen_load|save do nothing if already saved. I'll fix this someday, but
         * for now we need ugly hacks to prevent garbage when browsing spells.
         * these hacks only work if screen_depth is 1 and if clients know to redraw
         * their menus inside their menu loops */
        bool screen_hack = screen_is_saved();
        if (screen_hack) screen_load(); /* screen_depth back to 0 */
    
        screen_save(); /* <=== bug: this would not save our current state without the screen_hack */
        doc_display(doc, "Throw", 0);
        screen_load(); /* <=== bug: this would not erase our drawing without the screen_hack */

        if (screen_hack) screen_save(); /* screen_depth back to 1 with (hopefully) the original screen */
    }
    doc_free(doc);
}
void plr_throw_doc(plr_throw_ptr context, doc_ptr doc)
{
    int to_d = 0, i;
    int to_h = 0;
    int mult = 100;
    slay_t crit = {0}, slay;
    int crit_chance = 0;
    int num_throw = 100;
    bool force = FALSE;
    doc_ptr cols[2] = {0};
    vec_ptr v = NULL;

    _init_context(context);
    if (!context->obj) return;
    if (context->energy)
        num_throw = 10000 / context->energy;

    if (obj_is_known(context->obj))
    {
        to_d = context->obj->to_d;
        to_h = context->obj->to_h;
    }

    obj_flags_known(context->obj, context->flags);
    if (have_flag(context->flags, OF_BRAND_MANA))
    {
        int cost = 1 + context->obj->dd * context->obj->ds / 7;
        if (plr->csp >= cost)
            force = TRUE;
    }

    if (have_flag(context->flags, OF_VORPAL2))
        mult = mult * 5 / 3;  /* 1 + 1/3(1 + 1/2 + ...) = 1.667x */
    else if (have_flag(context->flags, OF_VORPAL))
        mult = mult * 11 / 9; /* 1 + 1/6(1 + 1/4 + ...) = 1.222x */

    crit_chance = crit_chance_aux(CRIT_FREQ_ROLL, context->obj->weight, context->skill);
    crit = crit_avg(CRIT_QUAL_ROLL, context->obj->weight);
    crit.mul = (1000 - crit_chance)*100/1000 + (crit_chance*crit.mul)/1000;
    crit.add = crit_chance*crit.add/1000;


    /* Display in 2 columns, side by side */
    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    /* Column #1 */
    object_desc(context->obj_name, context->obj, OD_COLOR_CODED | OD_NAME_AND_ENCHANT | OD_THROWING);
    doc_printf(cols[0], "<color:y> Throwing:</color> <indent><style:indent>%s</style></indent>\n", context->obj_name);

    doc_printf(cols[0], " %-7.7s: %d.%d lbs\n", "Weight", context->obj->weight/10, context->obj->weight%10);
    doc_printf(cols[0], " %-7.7s: %d + %d = %d\n", "To Hit", to_h, plr->shooter_info.dis_to_h, to_h + plr->shooter_info.dis_to_h);

    doc_printf(cols[0], " %-7.7s: %d\n", "Range", context->range);
    if (plr->wizard && 0)
        doc_printf(cols[0], " %-7.7s: %d (31 to return; 38 to catch)\n", "Back", context->back_chance);
    doc_printf(cols[0], " %-7.7s: %d%%\n", "Return", 99*(1000 - MAX(0, (30 - context->back_chance))*1000/30)/1000);
    doc_printf(cols[0], " %-7.7s: %d%%\n", "Catch", 99*(1000 - MAX(0, (37 - context->back_chance))*1000/30)/1000);
    doc_printf(cols[0], " %-7.7s: %d.%2.2dx\n", "Mult", context->mult/100, context->mult%100);
    doc_printf(cols[0], " %-7.7s: %d.%2.2d\n", "Throws", num_throw/100, num_throw%100);

    mult = mult * crit.mul / 100;
    to_d = to_d + crit.add / 100;

    doc_printf(cols[0], "<color:G> %-7.7s</color>\n", "Damage");

    if (crit.add)
    {
        doc_printf(cols[0], " %-7.7s: %d.%02dx + %d.%02d\n", "Crits",
                        crit.mul/100, crit.mul%100, crit.add/100, crit.add%100);
    }
    else
    {
        doc_printf(cols[0], " %-7.7s: %d.%02dx (%d.%d%%)\n", "Crits",
                        crit.mul/100, crit.mul%100, crit_chance / 10, crit_chance % 10);
    }

    _display_weapon_slay(mult, _slay(100), FALSE, context->mult, num_throw, context->obj->dd, context->obj->ds, to_d, "Normal", TERM_WHITE, cols[0]);
    if (force)
        _display_weapon_slay(mult, _slay(100), force, context->mult, num_throw, context->obj->dd, context->obj->ds, to_d, "Force", TERM_L_BLUE, cols[0]);

    v = of_lookup_slay();
    for (i = 0; i < vec_length(v); i++)
    {
        of_info_ptr info = vec_get(v, i);
        if (!have_flag(context->flags, info->id)) continue;
        slay = slay_lookup(info->id);
        if (!slay.id) continue;
        _display_weapon_slay(mult, slay, force, context->mult, num_throw, context->obj->dd, context->obj->ds, to_d, slay.name, TERM_YELLOW, cols[0]);
    }
    vec_free(v);
    v = of_lookup_brand();
    for (i = 0; i < vec_length(v); i++)
    {
        of_info_ptr info = vec_get(v, i);
        if (!have_flag(context->flags, info->id)) continue;
        slay = slay_lookup(info->id);
        if (!slay.id) continue;
        _display_weapon_slay(mult, slay, force, context->mult, num_throw, context->obj->dd, context->obj->ds, to_d, slay.name, TERM_RED, cols[0]);
    }
    vec_free(v);

    /* Column #2 */
    doc_insert(cols[1], "<color:G>Accuracy</color>\n");
    doc_insert(cols[1], " AC Hit\n");

    doc_printf(cols[1], "%3d %2d%%\n", 25, throw_hit_chance(to_h, 25, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 50, throw_hit_chance(to_h, 50, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 75, throw_hit_chance(to_h, 75, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 100, throw_hit_chance(to_h, 100, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 125, throw_hit_chance(to_h, 125, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 150, throw_hit_chance(to_h, 150, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 175, throw_hit_chance(to_h, 175, 10));
    doc_printf(cols[1], "%3d %2d%%\n", 200, throw_hit_chance(to_h, 200, 10));

    /* Assemble the result */
    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);
}

