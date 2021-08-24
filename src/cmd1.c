/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Movement commands (part 1) */

#include "angband.h"
#include "equip.h"
#include <assert.h>

void death_scythe_miss(object_type *o_ptr, int hand, int skill)
{
    int k;
    slay_t crit;
    int dd = o_ptr->dd;
    int ds = o_ptr->ds;
    int to_h = 0;
    int to_d = 0;

    /* Sound */
    sound(SOUND_HIT);

    /* Message */
    if (hand == HAND_NONE) /* this is a thrown  weapon */
        cmsg_print(TERM_VIOLET, "Your scythe viciously slashes you!");
    else
    {
        cmsg_print(TERM_VIOLET, "Your scythe returns to you!");
        dd += plr->attack_info[hand].to_dd;
        ds += plr->attack_info[hand].to_ds;
        to_h += plr->attack_info[hand].to_h;
        to_d += plr->attack_info[hand].to_d;
    }

    k = damroll(dd, ds);
    {
        int mult;
        switch (plr->mimic_form)
        {
        case MIMIC_NONE:
            switch (plr->prace)
            {
                case RACE_YEEK:
                case RACE_KLACKON:
                case RACE_HUMAN:
                case RACE_AMBERITE:
                case RACE_DUNADAN:
                case RACE_BARBARIAN:
                case RACE_BEASTMAN:
                case RACE_DEMIGOD:
                    mult = 25;break;
                case RACE_SNOTLING:
                case RACE_HALF_TROLL:
                case RACE_HALF_OGRE:
                case RACE_HALF_GIANT:
                case RACE_HALF_TITAN:
                case RACE_CYCLOPS:
                case RACE_IMP:
                case RACE_TENGU:
                case RACE_SKELETON:
                case RACE_ZOMBIE:
                case RACE_VAMPIRE:
                case RACE_MON_VAMPIRE:
                case RACE_SPECTRE:
                case RACE_BALROG:
                case RACE_DRACONIAN:
                case RACE_MON_LICH:
                case RACE_MON_DRAGON:
                    mult = 30;break;
                default:
                    mult = 10;break;
            }
            break;
        case MIMIC_DEMON:
        case MIMIC_DEMON_LORD:
        case MIMIC_VAMPIRE:
            mult = 30;break;
        default:
            mult = 10;break;
        }

        if (plr->align < 0 && mult < 20)
            mult = 20;
        if (!res_save_default(GF_ACID) && mult < 25)
            mult = 25;
        if (!res_save_default(GF_ELEC) && mult < 25)
            mult = 25;
        if (!res_save_default(GF_FIRE) && mult < 25)
            mult = 25;
        if (!res_save_default(GF_COLD) && mult < 25)
            mult = 25;
        if (!res_save_default(GF_POIS) && mult < 25)
            mult = 25;

        if (obj_has_flag(o_ptr, OF_BRAND_MANA) && (plr->csp > (plr->msp / 30)))
        {
            plr->csp -= (1+(plr->msp / 30));
            plr->redraw |= (PR_MANA);
            mult = mult * 3 / 2 + 15;
        }

        k *= mult;
        k /= 10;
    }

    crit = crit_aux(CRIT_FREQ_ROLL, o_ptr->weight, skill);
    if (crit.id)
    {
        k = k * crit.mul/100 + crit.add;
        if (crit.msg) msg_print(crit.msg);
    }

    if (one_in_(6))
    {
        int mult = 2;
        msg_format("Your weapon cuts deep into yourself!");
        /* Try to increase the damage */
        while (one_in_(4))
        {
            mult++;
        }

        k *= mult;
    }
    k += to_d + o_ptr->to_d;

    if (k < 0) k = 0;

    take_hit(DAMAGE_FORCE, k, "Death scythe");
}

/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
    int k;

    /* Never hit */
    if (chance <= 0) return (FALSE);

    /* Invisible monsters are harder to hit */
    if (!vis) chance = (chance + 1) / 2;

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- Instant miss or hit */
    if (k < 10) return (k < 5);

    /* Power competes against armor */
    if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

    /* Assume hit */
    return (TRUE);
}



/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_norm(int chance, int ac, int vis)
{
    int k;

    /* Wimpy attack never hits */
    if (chance <= 0) return (FALSE);

    /* Penalize invisible targets */
    if (!vis) chance = (chance + 1) / 2;

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- Instant miss or hit */
    if (k < 10) return (k < 5);

    /* Power must defeat armor */
    if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

    /* Assume hit */
    return (TRUE);
}

int search(void)
{
    int i, rc = ACTION_CONTINUE;
    for (i = 0; i < 9 && rc == ACTION_CONTINUE; i++) /* XXX need to search '5' for chests under foot */
    {
        point_t p = point_step(plr->pos, ddd[i]);
        if (dun_pos_interior(cave, p))
            rc = dun_search(cave, p, 0);
    }
    return rc;
}

static bool _knockback_seek(dun_line_gen_ptr gen, point_t pos)
{
    for (dun_line_gen_first(gen); !point_equals(gen->current_point, pos); dun_line_gen_next(gen))
    {
        if (!dun_pos_interior(cave, gen->current_point)) return FALSE;
        if (dun_stop_project(cave, gen->current_point, pos, 0)) return FALSE;
    }
    assert(point_equals(gen->current_point, pos));
    return TRUE;
}
/* advanced monster knockback. we cannot just allocate a longer dun_path
 * since we might exceed DUN_PATH_MAX */
static bool _knockback(mon_ptr mon, int dist)
{
    line_t line = line_create(plr->pos, mon->pos);
    dun_line_gen_ptr gen = dun_line_gen_alloc(line);
    bool ok = FALSE;

    /* find the path from plr to mon (respecting FF_PROJECT) */
    for (;;)
    {
        if (_knockback_seek(gen, mon->pos)) break;
        if (!dun_line_gen_next_strategy(gen)) break;
    }

    /* now push the monster back along this path */
    if (point_equals(gen->current_point, mon->pos))
    {
        int i;
        for (i = 0; i < dist; i++)
        {
            point_t p = dun_line_gen_next(gen);
            dun_cell_ptr cell = dun_cell_at(cave, p);
            if (!dun_allow_mon_at(cave, p)) break;
            if (!cell_allow_mon(cell, mon)) break;
            dun_move_mon(cave, mon, p);
            Term_xtra(TERM_XTRA_DELAY, delay_animation);
            Term_fresh();
        }
        ok = TRUE;
    }
    dun_line_gen_free(gen);
    return ok;
}
void do_monster_knockback(mon_ptr mon, int dist)
{
    if (point_equals(plr->pos, mon->pos)) return;
    _knockback(mon, dist);
}
bool random_opponent(int *y, int *x)
{
    int dirs[9];
    int ct = 0;
    int i;
    point_t p;

    for (i = 0; i < 8; i++)
    {
        p = point_step(plr->pos, ddd[i]);
        if (dun_mon_at(cave, p))
            dirs[ct++] = i;
    }

    if (ct)
    {
        i = randint0(ct);
        p = point_step(plr->pos, ddd[dirs[i]]);
        *y = p.y;
        *x = p.x;
        return TRUE;
    }
    return FALSE;
}

static bool _auto_detect_traps(void)
{
    slot_t slot;
    if (!auto_detect_traps) return FALSE;
    if (cave->type->id == D_SURFACE) return FALSE;

    /* Hooks go first. For example, the Necromancer prefers Undead Sight which throws
     * magic mapping into the bargain. They might also carry -Detection to locate monsters
     * and we should only used that as a last resort. */
    if (plr_hook_auto_detect()) return TRUE;
    /* XXX These s/b hooks */
    if (plr->pclass == CLASS_MAGIC_EATER && magic_eater_auto_detect_traps()) return TRUE;
    if ((prace_is_(RACE_DWARF) || prace_is_(RACE_NIBELUNG)) && plr->lev > 20)
    {
        cast_detect_doors_stairs_traps();
        /* hp charge is only 5 ... forget about it. */
        return TRUE;
    }

    slot = pack_find_obj(TV_SCROLL, SV_SCROLL_DETECT_TRAP);
    if (slot && !plr_tim_find(T_BLIND) && !(get_race()->flags & RACE_IS_ILLITERATE))
    {
        obj_ptr scroll = pack_obj(slot);
        if (obj_is_known(scroll))
        {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            stats_on_use(scroll, 1);
            scroll->number--;
            obj_release(scroll, 0);
            return TRUE;
        }
    }
    slot = pack_find_device(EFFECT_DETECT_TRAPS);
    if (slot)
    {
        obj_ptr device = pack_obj(slot);
        detect_traps(DETECT_RAD_DEFAULT, TRUE);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    slot = pack_find_device(EFFECT_DETECT_ALL);
    if (slot)
    {
        obj_ptr device = pack_obj(slot);
        detect_all(DETECT_RAD_DEFAULT);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    slot = quiver_find_device(EFFECT_DETECT_ALL);
    if (slot)
    {
        obj_ptr device = quiver_obj(slot);
        detect_all(DETECT_RAD_DEFAULT);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    return FALSE;
}

static bool _cell_allow_quickwalk(dun_cell_ptr cell)
{
    if (!cell_project(cell)) return FALSE;
    if (lava_is_deep(cell) || water_is_deep(cell))
        return plr->levitation;
    return TRUE;
}

/*
 * Move the player
 */
bool move_player_effect(point_t pos, u32b mpe_mode)
{
    dun_cell_ptr old_cell = dun_cell_at(cave, plr->pos);
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    bool old_dtrap = BOOL(old_cell->flags & CELL_DETECT);
    bool new_dtrap = BOOL(cell->flags & CELL_DETECT);

    /* leaving a trap detected zone: note we apply detection before moving. the distance
     * check is to handle teleportation. */
    if (!(mpe_mode & MPE_STAYING) && disturb_trap_detect && point_fast_distance(plr->pos, pos) < 2)
    {
        if (old_dtrap && !new_dtrap)
        {
            if (!_auto_detect_traps())
            {
                if (running || travel.run)
                {
                    disturb(0, 0);
                    energy_use = 0;
                    cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
                    return FALSE;
                }
            }
            else
            {
                old_dtrap = BOOL(old_cell->flags & CELL_DETECT);
                new_dtrap = BOOL(cell->flags & CELL_DETECT);
            }
        }
    }

    if (!(mpe_mode & MPE_STAYING))
    {
        point_t old_pos = plr->pos;
        mon_ptr old_mon = dun_mon_at(cave, old_pos);
        mon_ptr new_mon = dun_mon_at(cave, pos);
        bool diagonal = old_pos.y != pos.y && old_pos.x != pos.x;

        /* Move the player */
        dun_move_plr(cave, pos);

        /* XXX (Proposed): Diagonal movement costs 50% more energy */
        if (diagonal)
            energy_use += 50;

        /* Hack -- For moving monster or riding player's moving */
        if (!(mpe_mode & MPE_DONT_SWAP_MON))
        {
            if (old_mon) dun_move_mon(cave, old_mon, pos);
            else if (new_mon) dun_move_mon(cave, new_mon, old_pos);
        }

        dun_draw_pos(cave, old_pos);
        dun_draw_pos(cave, pos);
        viewport_verify();

        /* Check detection status */
        if (old_dtrap && !new_dtrap)
        {
            if (alert_trap_detect)
            {
                cmsg_print(TERM_VIOLET, "You leave a trap detected zone.");
                msg_print(NULL); /* Force a -more- prompt (unless auto_more is enabled!) */
            }
            /* XXX plr->redraw |= PR_STATUS; */
        }
        else if (!old_dtrap && new_dtrap)
        {
            if (alert_trap_detect)
                cmsg_print(TERM_L_BLUE, "You enter a trap detected zone.");
            /* XXX plr->redraw |= PR_STATUS; */
        }

        if (mpe_mode & MPE_FORGET_FLOW)
        {
            dun_forget_flow(cave);
            plr->update |= PU_UN_VIEW;
            plr->redraw |= PR_MAP;
        }
        plr->update |= PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_LIGHT;
        plr->window |= PW_MONSTER_LIST | PW_OBJECT_LIST;

        /* Position Targets are confusing. They should be dismissed when no longer valid.
         * Note: Originally, I had this check in target_okay(), which is, of course, called
         * fairly often and repeatedly. While this had the fortunate side effect of preventing
         * many 'trick shot' projection abuses, it also messed up 'disintegration' effects
         * (such as Breathe Disintegration or Beam of Disintegration). For these, the user
         * needs to target a non-projectable monster. As a compromise, we will continue to
         * dismiss such targets, but only once the player moves. */
        if (who_is_pos(plr->target))
        {
            point_t pos = who_pos(plr->target);
            if (!dun_pos_interior(cave, pos) || !plr_project(pos))
            {
                plr->target = who_create_null();
                plr->redraw |= PR_HEALTH_BARS;
            }
        }

        /* XXX 
        if (!view_unsafe_grids)
            plr->redraw |= PR_STATUS;
        */

        plr_hook_move_player();

        plr->window |= PW_OVERHEAD | PW_DUNGEON;

        /* Remove "unsafe" flag */
        if ( (!plr_tim_find(T_BLIND) && !no_light())
          || !floor_has_trap(cell) )
        {
            cell->flags &= ~CELL_UNSAFE;
        }

        /* Handle stuff XXX Why?! */
        if (mpe_mode & MPE_HANDLE_STUFF) handle_stuff();

        if (plr_tim_find(T_CLOAK_INVIS))
            set_invisible(TRUE);

        if (plr->action == ACTION_QUICK_WALK && !_cell_allow_quickwalk(cell))
        {
            msg_print("You cannot run in here.");
            set_action(ACTION_NONE);

            if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
            {
                msg_print("Your shadow stance is disrupted!");
                weaponmaster_set_toggle(TOGGLE_NONE);
            }
        }
    }

    if (mpe_mode & MPE_ENERGY_USE)
    {
        /* Spontaneous Searching */
        if ((plr->skills.fos >= 50) || (0 == randint0(50 - plr->skills.fos)))
            search();

        /* Continuous Searching */
        if (plr->action == ACTION_SEARCH)
            search();
    }

    /* Handle "objects" */
    if (!(mpe_mode & MPE_DONT_PICKUP))
    {
        if (mpe_mode & MPE_DO_PICKUP)
            pack_get_floor();
        else
        {
            cptr verb = no_light() ? "feel" : "see";
            char name[MAX_NLEN_OBJ];
            obj_ptr obj;
            autopick_get_floor();
            for (obj = dun_obj_at(cave, plr->pos); obj; obj = obj->next)
            {
                object_desc(name, obj, OD_COLOR_CODED);
                msg_format("You %s %s.", verb, name);
                disturb(0, 0);
            }
        }
    }

    /* XXX move these to cell_accept_plr ... */
    if (stairs_enter_quest(cell))
    {
        disturb(0, 0);
        energy_use = 0;
        command_new = SPECIAL_KEY_QUEST;
    }
    else if (!(mpe_mode & MPE_STAYING))
        cell_accept_plr(cave, pos, cell);
    else if (cell_is_portal(cell) || cell_is_bldg(cell))
        cell_accept_plr(cave, pos, cell);

    return dun_plr_at(cave, pos) && !plr->is_dead;
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid. Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir, bool do_pickup, bool break_trap)
{
    dun_cell_ptr old_cell = dun_cell_at(cave, plr->pos);
    point_t      pos = point_step(plr->pos, dir);
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    mon_ptr      mon = dun_mon_at(cave, pos);

    char m_name[80];

    bool p_can_enter = cell_allow_plr(cell);
    bool stormbringer = equip_find_art("|.Stormbringer");
    bool shadow_strike = FALSE;
    bool oktomove = TRUE;
    bool do_past = FALSE;

/* BLOCK: If the player is stuck in a web, they may lose a turn trying to escape. */
    if (floor_has_web(old_cell))
    {
        if ( plr->pass_web
          || plr->pass_wall)
        {
        }
        else if (plr->kill_wall || plr->sh_fire)
        {
            msg_print("The webs evaporate!");
            floor_remove_web(old_cell);
            dun_draw_pos(cave, plr->pos);
        }
        else if (plr->clear_web)
        {
            msg_print("You hack at the sticky webs.");
            floor_remove_web(old_cell);
            dun_draw_pos(cave, plr->pos);
            return; /* no move */
        }
        else if (!old_cell->parm2 || plr->skill_dig > randint0(30*old_cell->parm2))
        {
            msg_print("You hack at the sticky webs.");
            floor_remove_web(old_cell);
            dun_draw_pos(cave, plr->pos);
            return; /* no move */
        }
        else
        {
            msg_print("You are stuck!");
            old_cell->parm2--;
            return; /* no move */
        }
    }

/* BLOCK: Attack adjacent monsters; possibly decide to push past friendly monsters */
    if (mon && (mon->ml || p_can_enter))
    {
        /* Normally, the plr should not attack friendly monsters when moving */
        if ( !mon_is_hostile(mon)
          && !( !mon->ml 
             || plr_tim_find(T_CONFUSED)
             || plr_tim_find(T_HALLUCINATE)
             || plr_tim_find(T_STUN)
             || (mut_present(MUT_BERS_RAGE) && plr_tim_find(T_BERSERK)) )
          && pattern_legal_move(old_cell, cell)
          && p_can_enter )
        {
            mon_tim_delete(mon, MT_SLEEP);
            monster_desc(m_name, mon, 0); /* see "You push past %s" below */
            if (mon->ml)
            {
                if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
                health_track(mon);
            }

            /* displace? */
            if (stormbringer && randint1(1000) > 666)
            {
                plr_attack_normal(pos);
                oktomove = FALSE;
            }
            else if (cell_allow_mon(old_cell, mon))
            {
                do_past = TRUE;
            }
            else
            {
                msg_format("%^s is in your way!", m_name);
                energy_use = 0;
                oktomove = FALSE;
            }
            /* now continue on to 'movement' */
        }
        else
        {
            plr_attack_normal(pos);
            if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
                shadow_strike = TRUE; /* attack then swap positions */
            else
                oktomove = FALSE;
        }
    }

/* BLOCK: Check for legal movement. Formerly handled riding and possessors, 
 * but cell_allow_plr() now does this. There are some other riding checks
 * if your mount is paralyzed|stunned|sleeping ... */
    if (!oktomove)
    {
    }
    else if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
    {
        mon_race_ptr plr_race = plr_mon_race();
        if (mon_race_never_move(plr_race))
        {
            energy_use *= 3;
            /*msg_print("You can't move!");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);*/
        }
        else if (!p_can_enter && !cell_is_door(cell))
        {
            msg_print("Your current form cannot enter that location.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
    }
    else if (plr->prace == RACE_MON_RING && !plr->riding)
    {
        msg_print("You can't move! Try using your Glitter power to lure a ringbearer instead.");
        energy_use = 0;
        oktomove = FALSE;
        disturb(0, 0);
    }
    else if (plr->riding)
    {
        mon_ptr riding_m_ptr = plr_riding_mon();
        mon_race_ptr riding_r_ptr = riding_m_ptr->race;
        if (mon_race_never_move(riding_r_ptr))
        {
            msg_print("Your mount cannot move!");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (mon_tim_find(riding_m_ptr, MT_SLEEP))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is sleeping.", m_name);
            oktomove = FALSE;
            disturb(0,0);
        }
        else if (mon_tim_find(riding_m_ptr, T_PARALYZED))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is paralyzed.", m_name);
            oktomove = FALSE;
            disturb(0,0);
        }
        else if (mon_tim_find(riding_m_ptr, T_FEAR))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is too scared to control.", m_name);
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (plr->riding_ryoute) /* your mount is in control and decides where to move */
        {
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (!p_can_enter)
        {
            msg_print("Your current mount cannot enter that location.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }

        if (oktomove && randint0(50) < mon_tim_amount(riding_m_ptr, T_STUN))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("You cannot control stunned %s!",m_name);
            oktomove = FALSE;
            disturb(0, 0);
        }
    }
    /* XXX was features without MOVE but with CAN_FLY */
    else if (!p_can_enter && (cell_is_chasm(cell) || wall_is_mountain(cell)))
    {
        msg_print("You need levitation to move there!");
        oktomove = FALSE;
        if (!shadow_strike)
            energy_use = 0;
        running = 0;
    }

/* BLOCK: Disarm traps; Open Doors; Bump into Walls */
    if (!oktomove)
    {
        /* FYI: Either the player was blocked from movement -OR- the player attacked
         * because a monster was in the way. */
    }
#ifdef ALLOW_EASY_DISARM /* TNB */
    /* Disarm a visible trap */
    else if ((do_pickup != easy_disarm) && floor_has_known_trap(cell))
    {
        if (!plr_can_ignore_trap(cell))
        {
            if (dun_disarm(cave, pos, 0) == ACTION_CONTINUE)
                return; /* failed to disarm, but didn't trigger */
        }
    }

#endif /* ALLOW_EASY_DISARM -- TNB */

    else if (!p_can_enter)
    {
        oktomove = FALSE;
        disturb(0, 0);

        /* Notice things in the dark */
        if (!(cell->flags & CELL_MAP) && !plr_can_see(pos))
        {
            cptr desc = cell_desc(cell);
            msg_format("You feel %s %s blocking your way.",
                is_a_vowel(desc[0]) ? "an" : "a", desc);

            cell->flags |= (CELL_MAP | CELL_AWARE);
            dun_draw_pos(cave, pos);
        }

        /* Notice things */
        else
        {
            cptr desc = cell_desc(cell);

#ifdef ALLOW_EASY_OPEN
            /* Closed doors */
            if (easy_open && door_is_closed(cell))
            {
                int rc = dun_open(cave, pos, 0);
                if (rc == ACTION_CONTINUE && always_repeat)
                {
                    static int _repeat_count = 0;
                    if (door_is_locked(cell))
                    {
                        if (_repeat_count == 0)
                            _repeat_count = 99;
                        else
                            --_repeat_count;

                        if (_repeat_count)
                            command_rep = 1;
                    }
                    else
                        _repeat_count = 0;
                }
                return;
            }
#endif /* ALLOW_EASY_OPEN */

            msg_format("There is %s %s blocking your way.",
                is_a_vowel(desc[0]) ? "an" : "a", desc);

            /*
             * Well, it makes sense that you lose time bumping into
             * a wall _if_ you are confused, stunned or blind; but
             * typing mistakes should not cost you a turn...
             */
            if (!(plr_tim_find(T_CONFUSED) || plr_tim_find(T_STUN) || plr_tim_find(T_HALLUCINATE)))
            {
                if (!shadow_strike)
                    energy_use = 0;
            }
        }
    }

/* BLOCK: Handle walking the pattern */
    if (oktomove && !pattern_legal_move(old_cell, cell))
    {
        if (!(plr_tim_find(T_CONFUSED) || plr_tim_find(T_STUN) || plr_tim_find(T_HALLUCINATE)))
        {
            if (!shadow_strike)
                energy_use = 0;
        }

        /* To avoid a loop with running */
        disturb(0, 0);

        oktomove = FALSE;
    }

/* BLOCK: Actually move */
    if (oktomove)
    {
        u32b mpe_mode = MPE_ENERGY_USE;

        if (plr->warning)
        {
            if (!process_warning(pos))
            {
                if (!shadow_strike)
                    energy_use = 25;
                return;
            }
        }

        if (do_past)
            msg_format("You push past %s.", m_name);


#ifdef ALLOW_EASY_DISARM /* TNB */

        if (do_pickup != always_pickup) mpe_mode |= MPE_DO_PICKUP;

#else /* ALLOW_EASY_DISARM -- TNB */

        if (do_pickup) mpe_mode |= MPE_DO_PICKUP;

#endif /* ALLOW_EASY_DISARM -- TNB */

        if (break_trap) mpe_mode |= MPE_BREAK_TRAP;

        /* Move the player */
        move_player_effect(pos, mpe_mode);
    }
}


static bool ignore_avoid_run;
static bool _cell_avoid_run(dun_cell_ptr cell)
{
    if (cell->type == FEAT_TREE) return TRUE;
    if (wall_is_mountain(cell)) return TRUE;
    if (floor_has_web(cell)) return TRUE;
    return FALSE;
}
/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(point_t pos, int dir)
{
    point_t next = point_step(pos, dir);
    dun_cell_ptr cell;

    if (!dun_pos_valid(cave, next)) return FALSE;
    
    cell = dun_cell_at(cave, next);

    /* Must be known to the player */
    if (cell->flags & CELL_MAP)
    {
        if (!cell_allow_plr(cell))
            return cell->type != FEAT_DOOR;

        if (_cell_avoid_run(cell))
        {
            if (!ignore_avoid_run)
                return TRUE;
        }
        else if (cell->type == FEAT_WALL) /* passwall */
            return TRUE;
    }

    return FALSE;
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(point_t pos, int dir)
{
    point_t next = point_step(pos, dir);
    dun_cell_ptr cell;

    if (!dun_pos_valid(cave, next)) return FALSE;
    cell = dun_cell_at(cave, next);
    if (cell->flags & CELL_MAP) return FALSE;
    return plr_can_see(next);
}

/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea    You are in the open on at least one side.
 * find_breakleft   You have a wall on the left, and will stop if it opens
 * find_breakright  You have a wall on the right, and will stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * ##s
 * @x?
 * #.?
 *
 * If they are both seen to be closed, then it is seen that no benefit
 * is gained from moving straight. It is a known corner. To cut the
 * corner, go diagonally, otherwise go straight, but pretend you
 * stepped diagonally into that next location for a full view next
 * time. Conversely, if one of the ? squares is not seen to be closed,
 * then there is a potential choice. We check to see whether it is a
 * potential corner or an intersection/room entrance. If the square
 * two spaces straight ahead, and the space marked with 's' are both
 * unknown space, then it is a potential corner and enter if
 * find_examine is set, otherwise must stop because it is not a
 * corner. (find_examine option is removed and always is TRUE.)
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;

static bool find_road;

/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                 p
 */
static void run_init(int dir)
{
    point_t next;
    int i;
    bool deepleft, deepright, shortleft, shortright;

    /* Save the direction */
    find_current = dir;

    /* Assume running straight */
    find_prevdir = dir;

    plr->run_pos = plr->pos;
    next = point_step(plr->pos, dir);

    /* Hack: Stay on the road when running on surface */
    find_road = FALSE;
    if (cave->type->id == D_SURFACE && floor_is_road(dun_cell_at(cave, plr->pos))) 
    {
        if (floor_is_road(dun_cell_at(cave, next)))
        {
            find_road = TRUE;
            find_openarea = FALSE;
            find_breakright = find_breakleft = FALSE;
            return;
        }
    }

    /* Assume looking for open area */
    find_openarea = TRUE;

    /* Assume not looking for breaks */
    find_breakright = find_breakleft = FALSE;

    /* Assume no nearby walls */
    deepleft = deepright = FALSE;
    shortright = shortleft = FALSE;

    ignore_avoid_run = _cell_avoid_run(dun_cell_at(cave, next));

    /* Extract cycle index */
    i = chome[dir];

    /* Check for walls */
    if (see_wall(plr->pos, cycle[i+1]))
    {
        find_breakleft = TRUE;
        shortleft = TRUE;
    }
    else if (see_wall(next, cycle[i+1]))
    {
        find_breakleft = TRUE;
        deepleft = TRUE;
    }

    /* Check for walls */
    if (see_wall(plr->pos, cycle[i-1]))
    {
        find_breakright = TRUE;
        shortright = TRUE;
    }
    else if (see_wall(next, cycle[i-1]))
    {
        find_breakright = TRUE;
        deepright = TRUE;
    }

    /* Looking for a break */
    if (find_breakleft && find_breakright)
    {
        /* Not looking for open area */
        find_openarea = FALSE;

        /* Hack -- allow angled corridor entry */
        if (dir & 0x01)
        {
            if (deepleft && !deepright)
            {
                find_prevdir = cycle[i - 1];
            }
            else if (deepright && !deepleft)
            {
                find_prevdir = cycle[i + 1];
            }
        }

        /* Hack -- allow blunt corridor entry */
        else if (see_wall(next, cycle[i]))
        {
            if (shortleft && !shortright)
            {
                find_prevdir = cycle[i - 2];
            }
            else if (shortright && !shortleft)
            {
                find_prevdir = cycle[i + 2];
            }
        }
    }
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
    int         prev_dir, new_dir, check_dir = 0;
    point_t     next;
    int         i, max, inv;
    int         option = 0, option2 = 0;
    dun_cell_ptr cell;
    obj_ptr     obj;
    mon_ptr     mon;

    /* Where we came from */
    prev_dir = find_prevdir;

    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        next = point_step(plr->pos, new_dir);
        cell = dun_cell_at(cave, next);
        mon = dun_mon_at(cave, next);

        /* Visible monsters abort running */
        if (mon && !mon_is_pet(mon) && mon->ml) return TRUE;

        /* Visible objects abort running */
        for (obj = dun_obj_at(cave, next); obj; obj = obj->next)
            if (obj->marked & OM_FOUND) return TRUE;

        /* Assume unknown */
        inv = TRUE;

        /* Check memorized grids */
        if (cell->flags & CELL_MAP)
        {
            bool notice = cell_notice(cell);
            bool move = cell_allow_plr(cell) && !cell_is_wall(cell);

            /* optionally (or conditionally) ignore terrain */
            if (notice && move)
            {
                if (find_ignore_doors && door_is_open(cell))
                    notice = FALSE;

                else if (find_ignore_stairs && cell_is_stairs(cell))
                    notice = FALSE;

                else if (cell_is_lava(cell))
                {
                    if (res_pct(GF_FIRE) >= 100 || plr_tim_find(T_INVULN))
                        notice = FALSE;
                }
                else if (water_is_deep(cell))
                {
                    if ( plr->levitation
                      || plr->can_swim
                      || plr_total_weight() <= weight_limit() )
                    {
                        notice = FALSE;
                    }
                }
            }

            /* Interesting feature */
            if (notice) return TRUE;

            /* The grid is "visible" */
            inv = FALSE;
        }

        /* Analyze unknown grids and floors considering mimic */
        if (inv || !see_wall(next, 0))
        {
            /* Looking for open area */
            if (find_openarea)
            {
                /* Nothing */
            }
            /* stay on road */
            else if (find_road && !floor_is_road(cell))
            {
                /* Nothing */
            }

            /* The first new direction. */
            else if (!option)
            {
                option = new_dir;
            }

            /* Three new directions. Stop running. */
            else if (option2)
            {
                return (TRUE);
            }

            /* Two non-adjacent new directions. Stop running. */
            else if (option != cycle[chome[prev_dir] + i - 1])
            {
                return (TRUE);
            }

            /* Two new (adjacent) directions (case 1) */
            else if (new_dir & 0x01)
            {
                check_dir = cycle[chome[prev_dir] + i - 2];
                option2 = new_dir;
            }

            /* Two new (adjacent) directions (case 2) */
            else
            {
                check_dir = cycle[chome[prev_dir] + i + 1];
                option2 = option;
                option = new_dir;
            }
        }

        /* Obstacle, while looking for open area */
        else
        {
            if (find_openarea)
            {
                if (i < 0)
                {
                    /* Break to the right */
                    find_breakright = TRUE;
                }

                else if (i > 0)
                {
                    /* Break to the left */
                    find_breakleft = TRUE;
                }
            }
        }
    }

    /* Looking for open area */
    if (find_openarea)
    {
        /* Hack -- look again */
        for (i = -max; i < 0; i++)
        {
            /* Unknown grid or non-wall */
            if (!see_wall(plr->pos, cycle[chome[prev_dir] + i]))
            {
                /* Looking to break right */
                if (find_breakright)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break left */
                if (find_breakleft)
                {
                    return (TRUE);
                }
            }
        }

        /* Hack -- look again */
        for (i = max; i > 0; i--)
        {
            /* Unknown grid or non-wall */
            if (!see_wall(plr->pos, cycle[chome[prev_dir] + i]))
            {
                /* Looking to break left */
                if (find_breakleft)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break right */
                if (find_breakright)
                {
                    return (TRUE);
                }
            }
        }
    }

    /* Not looking for open area */
    else
    {
        /* No options */
        if (!option)
        {
            return (TRUE);
        }

        /* One option */
        else if (!option2)
        {
            /* Primary option */
            find_current = option;

            /* No other options */
            find_prevdir = option;
        }

        /* Two options, examining corners */
        else if (!find_cut)
        {
            /* Primary option */
            find_current = option;

            /* Hack -- allow curving */
            find_prevdir = option2;
        }

        /* Two options, pick one */
        else
        {
            /* Get next location */
            next = point_step(plr->pos, option);

            /* Don't see that it is closed off. */
            /* This could be a potential corner or an intersection. */
            if (!see_wall(next, option) ||
                !see_wall(next, check_dir))
            {
                /* Can not see anything ahead and in the direction we */
                /* are turning, assume that it is a potential corner. */
                if (see_nothing(next, option) &&
                    see_nothing(next, option2))
                {
                    find_current = option;
                    find_prevdir = option2;
                }

                /* STOP: we are next to an intersection or a room */
                else
                {
                    return (TRUE);
                }
            }

            /* This corner is seen to be enclosed; we cut the corner. */
            else if (find_cut)
            {
                find_current = option2;
                find_prevdir = option2;
            }

            /* This corner is seen to be enclosed, and we */
            /* deliberately go the long way. */
            else
            {
                find_current = option;
                find_prevdir = option2;
            }
        }
    }

    /* About to hit a known wall, stop */
    if (see_wall(plr->pos, find_current))
    {
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}



/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
    /* Start running */
    if (dir)
    {
        /* Ignore AVOID_RUN on a first step */
        ignore_avoid_run = TRUE;

        /* Hack -- do not start silly run */
        if (see_wall(plr->pos, dir))
        {
            /* Message */
            msg_print("You cannot run in that direction.");

            /* Disturb */
            disturb(0, 0);

            /* Done */
            return;
        }

        /* Initialize */
        run_init(dir);
    }

    /* Keep running */
    else
    {
        /* Update run */
        if (run_test())
        {
            /* Disturb */
            disturb(0, 0);

            /* Done */
            return;
        }
    }

    /* Decrease the run counter */
    if (--running <= 0) return;

    /* Take time */
    energy_use = 100;

    /* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM /* TNB */

    move_player(find_current, FALSE, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

    move_player(find_current, always_pickup, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */

    Term_xtra(TERM_XTRA_DELAY, delay_run);
    Term_fresh();

    if (dun_plr_at(cave, plr->run_pos))
    {
        plr->run_pos.x = 0;
        plr->run_pos.y = 0;
        disturb(0, 0);
    }
}


static int travel_cost(point_t pos)
{
    int cost;
    assert(travel.flow);
    cost = dun_flow_at(travel.flow, pos);
    if (cost == DUN_FLOW_NULL)
        cost = 9999;
    return cost;
}

static bool _travel_abort_direct(point_t pos)
{
    mon_ptr mon;
    int i;

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return TRUE;
    }
    if (disturb_trap_detect)
    {
        bool         old_dtrap = FALSE;
        bool         new_dtrap = FALSE;
        dun_grid_ptr grid = dun_grid_at(cave, pos);

        if (dun_grid_at(cave, plr->pos)->flags & CELL_DETECT)
            old_dtrap = TRUE;

        if (grid->flags & CELL_DETECT)
            new_dtrap = TRUE;

        if (old_dtrap && !new_dtrap && !_auto_detect_traps())
        {
            cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
            return TRUE;
        }
    }
    /* stop if hostile mon on target square */
    mon = dun_mon_at(cave, pos);
    if (mon && !mon_is_pet(mon) && mon->ml) return TRUE;

    /* stop if hostile mon adjacent to target square
     * it is annoying when auto_get_objects runs you into a Death mold! */
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        if (!dun_pos_interior(cave, p)) continue;
        mon = dun_mon_at(cave, p);
        if (mon && !mon_is_pet(mon) && mon->ml) return TRUE;
    }

    return FALSE;
}

static bool travel_abort(void)
{
    int prev_dir;
    int i, max;
    bool stop = TRUE;
    int current_cost = travel_cost(plr->pos);

    /* Where we came from */
    prev_dir = find_prevdir;

    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    for (i = 0; i < 8; i++)
    {
        point_t pos = point_step(plr->pos, ddd[i]);
        int     cost = travel_cost(pos);
        if (cost < current_cost)
            stop = FALSE;
    }

    if (stop) return TRUE;

    /* Cannot travel when blind */
    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return TRUE;
    }

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        int          dir = cycle[chome[prev_dir] + i];
        point_t      pos = point_step(plr->pos, dir);
        dun_grid_ptr grid;
        mon_ptr      mon;

        if (!dun_pos_interior(cave, pos)) continue;
        grid = dun_grid_at(cave, pos);

        if (disturb_trap_detect)
        {
            bool old_dtrap = FALSE;
            bool new_dtrap = FALSE;

            if (dun_grid_at(cave, plr->pos)->flags & CELL_DETECT)
                old_dtrap = TRUE;

            if (grid->flags & CELL_DETECT)
                new_dtrap = TRUE;

            if (old_dtrap && !new_dtrap && !_auto_detect_traps())
            {
                cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
                return TRUE;
            }
        }

        /* Visible monsters abort running */
        mon = dun_mon_at(cave, pos);
        if (mon && !mon_is_pet(mon) && mon->ml) return TRUE;
    }

    return FALSE;
}

void travel_step(void)
{
    int dir = 0;
    int old_run = travel.run;
    point_t next_pos = {0};
    dun_grid_ptr grid;

    assert(travel.path);
    if (point_vec_length(travel.path))
    {
        point_t pos;

        assert(0 <= travel.path_idx && travel.path_idx < point_vec_length(travel.path) - 1);
        pos = point_vec_get(travel.path, travel.path_idx);
        next_pos = point_vec_get(travel.path, ++travel.path_idx);

        if (!point_equals(pos, plr->pos) || _travel_abort_direct(next_pos))
        {
            disturb(0, 0);
            return;
        }
        dir = point_step_dir(plr->pos, next_pos); /* XXX */
    }
    else
    {
        int i;
        int dirs[8] = { 2, 4, 6, 8, 1, 7, 9, 3 };

        assert(travel.flow);

        find_prevdir = travel.dir;

        if (travel_abort())
        {
            if (travel.run == 255)
                msg_print("No route is found!");
            disturb(0, 0);
            return;
        }


        for (i = 0; i < 8; i++)
        {
            int     d = dirs[i];
            point_t pt = point_step(plr->pos, d);

            if (!dir || travel_cost(pt) < travel_cost(next_pos))
            {
                dir = d;
                next_pos = pt;
            }
        }
    }

    energy_use = 100;

    assert(dun_pos_interior(cave, next_pos));
    grid = dun_grid_at(cave, next_pos);

    /* Travelling is bumping into jammed doors and getting stuck */
    if (door_is_jammed(grid))
    {
        disturb(0, 0);
        return;
    }

    /* Closed door */
    else if (door_is_closed(grid))
    {
        if (!easy_open)
        {
            disturb(0, 0);
            return;
        }
    }
    /* Travelling is bumping into mountains and permanent walls and getting stuck */
    else if (!cell_allow_plr(grid) || cell_is_wall(grid) || cell_is_pattern(grid))
    {
        disturb(0, 0);
        return;
    }

    travel.dir = dir;
    move_player(dir, always_pickup, easy_disarm);
    if (!point_equals(plr->pos, next_pos)) /* open door; attack invisible monster; etc. */
        --travel.path_idx;
    Term_xtra(TERM_XTRA_DELAY, delay_run);
    Term_fresh();
    travel.run = old_run;

    if (point_equals(plr->pos, travel.pos))
        travel_end();
    else
        travel.run--;
}


