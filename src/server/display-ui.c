/*
 * File: display-ui.c
 * Purpose: Server-side game UI.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Antony Sidwell
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


/*** Sidebar display functions ***/


/*
 * Print character stat in given row, column
 */
static void prt_stat(struct player *p, int stat)
{
    Send_stat(p, stat, p->state.stat_top[stat], p->state.stat_use[stat],
        p->stat_max[stat], p->state.stat_add[stat], p->stat_cur[stat]);
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(struct player *p)
{
    const char *title = get_title(p);

    /* Ghost */
    if (p->ghost) title = "Ghost";

    Send_title(p, title);
}


/*
 * Prints level
 */
static void prt_lvl(struct player *p)
{
    Send_lvl(p, p->lev, p->max_lev);
}     


/*
 * Display the experience
 */
static void prt_exp(struct player *p)
{
    Send_exp(p, p->max_exp, p->exp, p->expfact);
}


/*
 * Prints current gold
 */
static void prt_gold(struct player *p)
{
    Send_gold(p, p->au);
}


/*
 * Equippy chars (ASCII representation of gear in equipment slot order)
 */
static void prt_equippy(struct player *p)
{
    int i;
    byte a;
    char c;
    struct object *obj;

    /* Dump equippy chars */
    for (i = 0; i < p->body.count; i++)
    {
        /* Object */
        obj = slot_object(p, i);

        if (obj)
        {
            /* Use ASCII symbol for distorted tiles */
            if (p->tile_distorted)
            {
                a = obj->kind->d_attr;
                c = obj->kind->d_char;

                if (obj->kind->flavor && !(p->obj_aware[obj->kind->kidx] && a && c))
                {
                    a = obj->kind->flavor->d_attr;
                    c = obj->kind->flavor->d_char;
                }
            }
            else
            {
                a = object_attr(p, obj);
                c = object_char(p, obj);
            }

            /* Multi-hued object */
            if (a == COLOUR_MULTI) a = COLOUR_VIOLET;
        }
        else
        {
            a = COLOUR_WHITE;
            c = ' ';
        }

        /* Dump proper character */
        p->hist_flags[0][i].a = a;
        p->hist_flags[0][i].c = c;
    }
}


/*
 * Prints current AC
 */
static void prt_ac(struct player *p)
{
    Send_ac(p, p->known_state.ac, p->known_state.to_a);
}


/*
 * Prints current hitpoints
 */
static void prt_hp(struct player *p)
{
    /* Hack -- redraw player, since the player's color now indicates approximate health. */
    if (OPT(p, hp_changes_color) && !p->use_graphics)
        square_light_spot(chunk_get(&p->wpos), p->py, p->px);

    Send_hp(p, p->mhp, p->chp);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(struct player *p)
{
    Send_sp(p, p->msp, p->csp);
}


/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targeting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.  When nothing
 * is being tracked, we clear the health bar.  If the monster being
 * tracked is not currently visible, a special health bar is shown.
 */
static void prt_health(struct player *p)
{
    bool is_unseen, is_dead, is_afraid, is_confused, is_stunned, is_asleep, is_held;
    bool is_poisoned, is_bleeding, is_blind;
    int pct;
    struct source *health_who = &p->upkeep->health_who;

    /* Not tracking */
    if (source_null(health_who))
    {
        /* Erase the health bar */
        Send_monster_health(p, 0, 0);
        return;
    }

    /* Tracking a player */
    if (health_who->player)
    {
        /* Extract various states */
        is_unseen = !player_is_visible(p, health_who->idx);
        is_dead = ((health_who->player->chp < 0)? true: false);
        is_afraid = (player_of_has(health_who->player, OF_AFRAID)? true: false);
        is_confused = (health_who->player->timed[TMD_CONFUSED]? true: false);
        is_stunned = (health_who->player->timed[TMD_PARALYZED]? true: false);
        is_asleep = (health_who->player->timed[TMD_PARALYZED]? true: false);
        is_held = (health_who->player->timed[TMD_PARALYZED]? true: false);
        is_poisoned = (health_who->player->timed[TMD_POISONED]? true: false);
        is_bleeding = (health_who->player->timed[TMD_CUT]? true: false);
        is_blind =  (health_who->player->timed[TMD_BLIND]? true: false);

        /* Extract the "percent" of health */
        pct = 100L * health_who->player->chp / health_who->player->mhp;
    }

    /* Tracking a monster */
    else
    {
        /* Extract various states */
        is_unseen = !monster_is_visible(p, health_who->idx);
        is_dead = ((health_who->monster->hp < 0)? true: false);
        is_afraid = (health_who->monster->m_timed[MON_TMD_FEAR]? true: false);
        is_confused = (health_who->monster->m_timed[MON_TMD_CONF]? true: false);
        is_stunned = (health_who->monster->m_timed[MON_TMD_STUN]? true: false);
        is_asleep = (health_who->monster->m_timed[MON_TMD_SLEEP]? true: false);
        is_held = (health_who->monster->m_timed[MON_TMD_HOLD]? true: false);
        is_poisoned = (health_who->monster->m_timed[MON_TMD_POIS]? true: false);
        is_bleeding = (health_who->monster->m_timed[MON_TMD_CUT]? true: false);
        is_blind = (health_who->monster->m_timed[MON_TMD_BLIND]? true: false);

        /* Extract the "percent" of health */
        pct = 100L * health_who->monster->hp / health_who->monster->maxhp;
    }

    /* Tracking an unseen, hallucinatory, or dead monster (or player) */
    if (is_unseen || p->timed[TMD_IMAGE] || is_dead)
    {
        /* Indicate that the monster (or player) health is "unknown" */
        Send_monster_health(p, 0, COLOUR_WHITE);
    }

    /* Tracking a visible monster (or player) */
    else
    {
        int len;

        /* Default to almost dead */
        byte attr = COLOUR_RED;

        /* Badly wounded */
        if (pct >= 10) attr = COLOUR_L_RED;

        /* Wounded */
        if (pct >= 25) attr = COLOUR_ORANGE;

        /* Somewhat wounded */
        if (pct >= 60) attr = COLOUR_YELLOW;

        /* Healthy */
        if (pct >= 100) attr = COLOUR_L_GREEN;

        /* Afraid */
        if (is_afraid) attr = COLOUR_VIOLET;

        /* Poisoned */
        if (is_poisoned) attr = COLOUR_GREEN;

        /* Bleeding */
        if (is_bleeding) attr = COLOUR_L_UMBER;

        /* Confused */
        if (is_confused) attr = COLOUR_UMBER;

        /* Stunned */
        if (is_stunned) attr = COLOUR_L_BLUE;

        /* Asleep */
        if (is_asleep) attr = COLOUR_BLUE;

        /* Held */
        if (is_held) attr = COLOUR_BLUE;

        /* Blind */
        if (is_blind) attr = COLOUR_L_DARK;

        /* Convert percent into "health" */
        len = ((pct < 10)? 1: ((pct < 90)? (pct / 10 + 1): 10));

        /* Send the health */
        Send_monster_health(p, len, attr);
    }
}


/*
 * Prints the speed of a character.
 */
static void prt_speed(struct player *p)
{
    s16b speed = get_speed(p);
    int mult = 10 * frame_energy(speed + 110) / frame_energy(110);

    Send_speed(p, speed, mult);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(struct player *p)
{
    if (p->wpos.depth > 0)
        strnfmt(p->depths, sizeof(p->depths), "%d' (L%d)", p->wpos.depth * 50, p->wpos.depth);
    else
    {
        struct location *town = get_town(&p->wpos);

        if (town)
            my_strcpy(p->depths, town->name, sizeof(p->depths));
        else
            strnfmt(p->depths, sizeof(p->depths), "W (%d, %d)", p->wpos.wx, p->wpos.wy);
    }

    Send_depth(p, p->wpos.depth, p->max_depth, p->depths);
}


/*** Status line display functions ***/


/*
 * Prints status of hunger
 */
static void prt_hunger(struct player *p)
{
    Send_food(p, p->food);
}


/*
 * Print all timed effects.
 */
static void prt_tmd(struct player *p)
{
    Send_status(p, p->timed);
}


static void prt_player_flag_info(struct player *p);


/*
 * Prints all status line indicators
 */
static void prt_status(struct player *p)
{
    int i;

    prt_hunger(p);
    prt_tmd(p);

    /* Hack -- timed flags display */
    prt_player_flag_info(p);
    for (i = 0; i < N_HISTORY_FLAGS; i++) Send_objflags(p, i);
}


/*
 * Prints all state effects
 */
static void prt_state(struct player *p)
{
    bool s, r, u;

    /* Stealth mode */
    s = (p->stealthy? true: false);

    /* Resting */
    r = player_is_resting(p);

    /* Unignoring */
    u = (p->unignoring? true: false);

    Send_state(p, s, r, u);

    /* Print recall/deep descent status */
    Send_recall(p, p->word_recall, p->deep_descent);
}


/*
 * Prints trap detection status
 */
static void prt_dtrap(struct player *p)
{
    Send_dtrap(p, get_dtrap(p));
}


/*
 * Print whether a character is studying or not.
 */
static void prt_study(struct player *p)
{
    /* Does the player have a book in the inventory or on the floor that has unlearned spells? */
    p->can_study_book = player_book_has_unlearned_spells(p);

    Send_study(p, p->upkeep->new_spells, p->can_study_book);
}


/*** Subwindow display functions ***/


/*
 * Display floor item in equipment sub-window
 */
static void prt_floor_item(struct player *p)
{
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(struct object *));
    int floor_num;
    struct chunk *c = chunk_get(&p->wpos);

    /* Paranoia */
    if (!c) return;

    floor_num = scan_floor(p, c, floor_list, floor_max, OFLOOR_SENSE | OFLOOR_VISIBLE, NULL);
    display_floor(p, c, floor_list, floor_num);
    mem_free(floor_list);
}


void dump_spells(struct player *p, struct object *obj)
{
    int j;
    spell_flags flags;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    char help[20];
    const char *comment = help;
    byte line_attr;
    char spell_name[31];

    /* Get the book */
    int bidx = object_to_book_index(p, obj);
    const struct class_book *book = object_to_book(p, obj);

    /* Requires a spellbook */
    if (!book) return;

    Send_book_info(p, bidx, book->realm->name);

    /* Dump the spells */
    for (j = 0; j < book->num_spells; j++)
    {
        const struct class_spell *spell = &book->spells[j];

        /* Get the spell index */
        int spell_index = spell->sidx;

        /* Skip illegible spells */
        if (spell->slevel >= 99)
        {
            flags.line_attr = COLOUR_L_DARK;
            flags.flag = RSF_NONE;
            flags.dir_attr = 0;
            flags.proj_attr = 0;

            my_strcpy(out_val, "(illegible)", sizeof(out_val));
            my_strcpy(out_desc, "", sizeof(out_desc));

            Send_spell_info(p, bidx, j, out_val, &flags);
            Send_spell_desc(p, bidx, j, out_desc);
            continue;
        }

        /* Get extra info */
        get_spell_info(p, spell_index, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = COLOUR_WHITE;

        /* Analyze the spell */
        if (p->spell_flags[spell_index] & PY_SPELL_FORGOTTEN)
        {
            comment = " forgotten";
            line_attr = COLOUR_YELLOW;
        }
        else if (!(p->spell_flags[spell_index] & PY_SPELL_LEARNED))
        {
            if (spell->slevel <= p->lev)
            {
                comment = " unknown";
                line_attr = COLOUR_L_BLUE;
            }
            else
            {
                comment = " difficult";
                line_attr = COLOUR_RED;
            }
        }
        else if (!(p->spell_flags[spell_index] & PY_SPELL_WORKED))
        {
            comment = " untried";
            line_attr = COLOUR_L_GREEN;
        }

        flags.line_attr = line_attr;
        flags.flag = RSF_NONE;
        flags.dir_attr = effect_aim(spell->effect);
        flags.proj_attr = spell->sproj;

        /* Dump the spell --(-- */
        if (streq(spell->realm->name, "elemental"))
        {
            strnfmt(spell_name, sizeof(spell_name), "%s (%d)", spell->name,
                p->spell_power[spell_index]);
        }
        else
            my_strcpy(spell_name, spell->name, sizeof(spell_name));
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", spell_name, spell->slevel,
            spell->smana, spell_chance(p, spell_index), comment);
        my_strcpy(out_desc, spell->text, sizeof(out_desc));

        Send_spell_info(p, bidx, j, out_val, &flags);
        Send_spell_desc(p, bidx, j, out_desc);
    }
}


/*
 * Hack -- fix spells
 */
static void fix_spell(struct player *p)
{
    spell_flags flags;
    int i;
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(struct object *));
    int floor_num;
    struct chunk *c = chunk_get(&p->wpos);

    /* Ghosts get a different set */
    if (p->ghost && !player_can_undead(p))
    {
        show_ghost_spells(p);
        return;
    }

    /* Shapechangers get a different set */
    if (player_has(p, PF_MONSTER_SPELLS))
    {
        show_mimic_spells(p);
        return;
    }

    /* Hack -- must be literate */
    if (!p->clazz->magic.total_spells) return;

    flags.line_attr = COLOUR_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Scan for appropriate books (in the pack) */
    for (i = 0; i < z_info->pack_size; i++)
    {
        struct object *obj = p->upkeep->inven[i];

        /* Requires an object */
        if (!obj) continue;

        dump_spells(p, obj);
    }

    /* Scan for appropriate books (on the floor) */
    floor_num = scan_floor(p, c, floor_list, floor_max, OFLOOR_SENSE | OFLOOR_VISIBLE, NULL);
    for (i = 0; i < floor_num; i++)
    {
        struct object *obj = floor_list[i];

        /* Requires an object */
        if (!obj) continue;

        dump_spells(p, obj);
    }
    mem_free(floor_list);
}


/*
 * Hack -- display mini-map view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_map(struct player *p)
{
    display_map(p, true);
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(struct player *p)
{
    struct actor_race *monster_race = &p->upkeep->monster_race;

    /* We need something */
    if (ACTOR_RACE_NULL(monster_race)) return;

    /* Describe it */
    if (monster_race->player) describe_player(p, monster_race->player);
    else describe_monster(p, monster_race->race);

    /* Notify player */
    notify_player_popup(p, "Monster Recall", NTERM_WIN_MONSTER, 0);
}


/*
 * Hack -- display monsters in sub-windows
 */
static void fix_monlist(struct player *p)
{
    /* Display visible monsters */
    monster_list_show_subwindow(p, p->max_hgt - 2, NORMAL_WID - 5);

    /* Notify player */
    notify_player_popup(p, "Monster List", NTERM_WIN_MONLIST, 0);
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(struct player *p)
{
    struct object *obj = p->upkeep->object;
    char o_name[NORMAL_WID];

    /* Cancel tracking */
    if (!obj)
    {
        p->special_file_type = SPECIAL_FILE_OTHER;
        text_out_init(p);
        text_out_done(p);
        notify_player_popup(p, "", NTERM_WIN_OBJECT, 0);
        return;
    }

    /* Ignore all hidden objects */
    if (!object_is_carried(p, obj) && ignore_item_ok(p, obj)) return;

    /* Get name */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Capitalize object name for header */
    my_strcap(o_name);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Dump info into player */
    object_info(p, obj, OINFO_NONE);

    /* Restore height and width of current dungeon level */
    text_out_done(p);

    /* Notify player */
    notify_player_popup(p, o_name, NTERM_WIN_OBJECT, 0);
}


/*
 * Hack -- display objects in sub-windows
 */
static void fix_objlist(struct player *p)
{
    /* Display visible objects */
    object_list_show_subwindow(p, p->max_hgt - 2, NORMAL_WID - 5);

    /* Notify player */
    notify_player_popup(p, "Object List", NTERM_WIN_OBJLIST, 0);
}


/*** Update flag handler functions ***/


/*
 * Obtain innate player mods
 */
static void player_mods(struct player *p, int mod, bool *res, bool *vul)
{
    switch (mod)
    {
        case OBJ_MOD_STEALTH:
        {
            /* Handle polymorphed players */
            if (p->poly_race)
            {
                if ((p->poly_race->weight > 0) && (p->poly_race->weight <= 100)) *res = true;
                else if (p->poly_race->weight > 150) *vul = true;
            }

            /* Monks get stealth bonus */
            if (player_has(p, PF_MARTIAL_ARTS) && !player_of_has(p, OF_AGGRAVATE))
                *res = true;

            break;
        }

        case OBJ_MOD_INFRA:
        {
            /* Ghost */
            if (p->ghost) *res = true;

            /* If the race has innate infravision, set the corresponding flag */
            if (p->race->infra > 0) *res = true;

            break;
        }

        case OBJ_MOD_TUNNEL:
        {
            /* Handle polymorphed players */
            if (p->poly_race && rf_has(p->poly_race->flags, RF_KILL_WALL)) *res = true;

            /* Elementalists get instant tunnel at level 50 */
            if (player_has(p, PF_ELEMENTAL_SPELLS) && (p->lev == 50)) *res = true;

            /* If the race has innate digging, set the corresponding flag */
            if (p->race->r_skills[SKILL_DIGGING] > 0) *res = true;

            break;
        }

        case OBJ_MOD_SPEED:
        {
            /* Ent: penalty to speed */
            if (player_has(p, PF_GIANT)) *vul = true;

            /* Unencumbered monks get speed bonus */
            if (monk_armor_ok(p) && (p->lev >= 10)) *res = true;

            /* Rogues get speed bonus */
            if (player_has(p, PF_SPEED_BONUS) && (p->lev >= 5)) *res = true;

            /* Handle polymorphed players */
            if (p->poly_race)
            {
                if (((p->poly_race->speed - 110) / 2) > 0) *res = true;
                else if (((p->poly_race->speed - 110) / 2) < 0) *vul = true;
            }

            break;
        }
    }
}


static bool is_dragon_immune(struct monster_race *race)
{
    struct dragon_breed *dn = get_dragon_form(race);

    if (dn) return (dn->immune? true: false);
    return false;
}


/*
 * Obtain the "elements" for the player as if he was an item
 */
void player_elements(struct player *p, struct element_info el_info[ELEM_MAX])
{
    /* Clear */
    memset(el_info, 0, ELEM_MAX * sizeof(struct element_info));

    /* Add racial flags */
    memcpy(el_info, p->race->el_info, ELEM_MAX * sizeof(struct element_info));

    /* Thunderlord */
    if (player_has(p, PF_THUNDERLORD))
    {
        if (p->lev >= 10) el_info[ELEM_FIRE].res_level = 1;
        if (p->lev >= 15) el_info[ELEM_COLD].res_level = 1;
        if (p->lev >= 20) el_info[ELEM_ACID].res_level = 1;
        if (p->lev >= 25) el_info[ELEM_ELEC].res_level = 1;
    }

    /* Elementalist */
    if (player_has(p, PF_ELEMENTAL_SPELLS))
    {
        if (p->lev >= 10) el_info[ELEM_FIRE].res_level = 1;
        if (p->lev >= 15) el_info[ELEM_COLD].res_level = 1;
        if (p->lev >= 20) el_info[ELEM_ACID].res_level = 1;
        if (p->lev >= 25) el_info[ELEM_ELEC].res_level = 1;
    }

    /* Ghost */
    if (p->ghost)
    {
        el_info[ELEM_NETHER].res_level = 1;

        /* PWMAngband */
        el_info[ELEM_DARK].res_level = 1;
        el_info[ELEM_POIS].res_level = 1;
        el_info[ELEM_COLD].res_level = 1;
        el_info[ELEM_SHARD].res_level = 1;
        el_info[ELEM_TIME].res_level = 1;
    }

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        bool dragon_immune = (player_has(p, PF_DRAGON) && is_dragon_immune(p->poly_race));

        if (rf_has(p->poly_race->flags, RF_IM_ACID))
            el_info[ELEM_ACID].res_level = (dragon_immune? 3: 1);
        else if (rsf_has(p->poly_race->spell_flags, RSF_BR_ACID))
            el_info[ELEM_ACID].res_level = 1;

        if (rf_has(p->poly_race->flags, RF_IM_ELEC))
            el_info[ELEM_ELEC].res_level = (dragon_immune? 3: 1);
        else if (rsf_has(p->poly_race->spell_flags, RSF_BR_ELEC))
            el_info[ELEM_ELEC].res_level = 1;

        if (rf_has(p->poly_race->flags, RF_IM_FIRE))
            el_info[ELEM_FIRE].res_level = (dragon_immune? 3: 1);
        else if (rsf_has(p->poly_race->spell_flags, RSF_BR_FIRE))
            el_info[ELEM_FIRE].res_level = 1;
        else if (rf_has(p->poly_race->flags, RF_HURT_FIRE))
            el_info[ELEM_FIRE].res_level = -1;

        if (rf_has(p->poly_race->flags, RF_IM_COLD))
            el_info[ELEM_COLD].res_level = (dragon_immune? 3: 1);
        else if (rsf_has(p->poly_race->spell_flags, RSF_BR_COLD))
            el_info[ELEM_COLD].res_level = 1;
        else if (rf_has(p->poly_race->flags, RF_HURT_COLD))
            el_info[ELEM_COLD].res_level = -1;

        if (rf_has(p->poly_race->flags, RF_IM_POIS) || rsf_has(p->poly_race->spell_flags, RSF_BR_POIS))
            el_info[ELEM_POIS].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_LIGHT)) el_info[ELEM_LIGHT].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_DARK)) el_info[ELEM_DARK].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_SOUN)) el_info[ELEM_SOUND].res_level = 1;
        if (rf_has(p->poly_race->flags, RF_HURT_ROCK) || rsf_has(p->poly_race->spell_flags, RSF_BR_SHAR))
            el_info[ELEM_SHARD].res_level = 1;
        if (rf_has(p->poly_race->flags, RF_IM_NEXUS) || rsf_has(p->poly_race->spell_flags, RSF_BR_NEXU))
            el_info[ELEM_NEXUS].res_level = 1;
        if (rf_has(p->poly_race->flags, RF_IM_NETHER) || rsf_has(p->poly_race->spell_flags, RSF_BR_NETH))
            el_info[ELEM_NETHER].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_CHAO)) el_info[ELEM_CHAOS].res_level = 1;
        if (rf_has(p->poly_race->flags, RF_IM_DISEN) || rsf_has(p->poly_race->spell_flags, RSF_BR_DISE))
            el_info[ELEM_DISEN].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_TIME)) el_info[ELEM_TIME].res_level = 1;
        if (rsf_has(p->poly_race->spell_flags, RSF_BR_MANA)) el_info[ELEM_MANA].res_level = 1;
    }
}


/*** Generic "deal with" functions ***/


/*
 * Redraw the cursor
 *
 * This function must simply calculate correct offset for each player
 * and update his cursor location
 */
static void cursor_redraw(struct player *p)
{
    bool is_unseen, is_dead;
    int x, y;
    struct source *cursor_who = &p->cursor_who;
    struct chunk *c = chunk_get(&p->wpos);

    /* Not tracking */
    if (source_null(cursor_who))
    {
        /* Reset the cursor */
        Send_cursor(p, 0, 0, 0);

        /* Remove old targeting path */
        if (p->path_drawn) load_path(p, p->path_n, p->path_g);

        return;
    }

    /* Tracking a player */
    if (cursor_who->player)
    {
        /* Extract various states */
        is_unseen = !player_is_visible(p, cursor_who->idx);
        is_dead = false;
        x = cursor_who->player->px;
        y = cursor_who->player->py;
    }

    /* Tracking a monster */
    else
    {
        /* Extract various states */
        is_unseen = !monster_is_visible(p, cursor_who->idx);
        is_dead = ((cursor_who->monster->hp < 0)? true: false);
        x = cursor_who->monster->fx;
        y = cursor_who->monster->fy;
    }

    /* Tracking an unseen or dead monster (or player) */
    if (is_unseen || is_dead)
    {
        /* Reset the cursor */
        Send_cursor(p, 0, 0, 0);

        /* Cancel tracking */
        cursor_track(p, NULL);

        /* Remove old targeting path */
        if (p->path_drawn) load_path(p, p->path_n, p->path_g);
    }

    /* Tracking a visible monster (or player) */
    else
    {
        char vis = 1;

        /* Hack -- is there something targetable above our position? */
        if (square_in_bounds_fully(c, y - 1, x) && target_accept(p, y - 1, x))
            vis = 2;

        Send_cursor(p, vis, x - p->offset_x, y - p->offset_y + 1);

        /* Redraw targeting path */
        if (p->path_drawn)
        {
            /* Remove old path */
            load_path(p, p->path_n, p->path_g);

            /* Redraw new targeting path */
            p->path_n = project_path(NULL, p->path_g, z_info->max_range, c, p->py, p->px,
                y, x, PROJECT_THRU);
            p->path_drawn = draw_path(p, p->path_n, p->path_g, p->py, p->px);
        }
    }
}


static void prt_player_sust_info(struct player *p)
{
    int i, stat;
    struct object *obj;
    bitflag f[OF_SIZE];
    byte a;
    char c;
    bool aware;

    /* Process equipment */
    for (i = 0; i < p->body.count; ++i)
    {
        s32b modifiers[OBJ_MOD_MAX];

        /* Get the object */
        obj = slot_object(p, i);

        /* Get the "known" flags */
        aware = (obj? object_flavor_is_aware(p, obj): false);
        object_flags_known(obj, f, aware);

        object_modifiers(obj, modifiers);

        /* Initialize color based on sign of modifier. */
        for (stat = 0; stat < STAT_MAX; stat++)
        {
            /* Default */
            a = COLOUR_SLATE;
            c = '.';

            /* Boosted or reduced */
            if (modifiers[stat])
            {
                /* Default */
                c = '*';

                /* Good */
                if (modifiers[stat] > 0)
                {
                    /* Good */
                    a = COLOUR_L_GREEN;

                    /* Label boost */
                    if (modifiers[stat] < 10)
                        c = I2D(modifiers[stat]);
                }

                /* Bad */
                if (modifiers[stat] < 0)
                {
                    /* Bad */
                    a = COLOUR_RED;

                    /* Label boost */
                    if (modifiers[stat] > -10)
                        c = I2D(0 - modifiers[stat]);
                }
            }

            /* Sustain */
            if (of_has(f, sustain_flag(stat)))
            {
                /* Dark green */
                a = COLOUR_GREEN;

                /* Convert '.' to 's' */
                if (c == '.') c = 's';
            }

            if ((c == '.') && obj && !object_flag_is_known(p, obj, sustain_flag(stat)))
                c = '?';

            /* Dump proper character */
            p->hist_flags[stat + 1][i].a = a;
            p->hist_flags[stat + 1][i].c = c;
        }
    }

    /* Player flags */
    player_flags(p, f);

    /* Check stats */
    for (stat = 0; stat < STAT_MAX; ++stat)
    {
        /* Default */
        a = COLOUR_SLATE;
        c = '.';

        /* Sustain */
        if (of_has(f, sustain_flag(stat)))
        {
            /* Dark green "s" */
            a = COLOUR_GREEN;
            c = 's';
        }

        /* Dump */
        p->hist_flags[stat + 1][p->body.count].a = a;
        p->hist_flags[stat + 1][p->body.count].c = c;
    }

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        int boost;

        /* Default */
        a = p->hist_flags[STAT_INT + 1][p->body.count].a;
        c = p->hist_flags[STAT_INT + 1][p->body.count].c;
        boost = 0;

        if (monster_is_stupid(p->poly_race)) boost -= 2;
        if (monster_is_smart(p->poly_race)) boost += 2;
        if (p->poly_race->freq_spell == 33) boost += 1;
        if (p->poly_race->freq_spell == 50) boost += 3;
        if (p->poly_race->freq_spell == 100) boost += 5;

        /* Boost */
        if (boost)
        {
            /* Default */
            c = '*';

            /* Good */
            if (boost)
            {
                /* Good */
                if (a == COLOUR_SLATE) a = COLOUR_L_GREEN;

                /* Label boost */
                if (boost < 10) c = I2D(boost);
            }

            /* Bad */
            if (boost < 0)
            {
                /* Bad */
                if (a == COLOUR_SLATE) a = COLOUR_RED;

                /* Label boost */
                if (boost > -10) c = I2D(-(boost));
            }
        }

        /* Dump */
        p->hist_flags[STAT_INT + 1][p->body.count].a = a;
        p->hist_flags[STAT_INT + 1][p->body.count].c = c;
    }
}


/*
 * List of resistances and abilities to display
 */
struct player_flag_record
{
    int mod;        /* Modifier */
    int flag;       /* Flag bit */
    int element;    /* Element */
    int tmd_flag;   /* Corresponding timed flag */
};


static const struct player_flag_record player_flag_table[(RES_PANELS + 1) * RES_ROWS] =
{
    {-1, -1, ELEM_ACID, TMD_OPP_ACID},
    {-1, -1, ELEM_ELEC, TMD_OPP_ELEC},
    {-1, -1, ELEM_FIRE, TMD_OPP_FIRE},
    {-1, -1, ELEM_COLD, TMD_OPP_COLD},
    {-1, -1, ELEM_POIS, TMD_OPP_POIS},
    {-1, -1, ELEM_LIGHT, -1},
    {-1, -1, ELEM_DARK, -1},
    {-1, -1, ELEM_SOUND, -1},
    {-1, -1, ELEM_SHARD, -1},

    {-1, -1, ELEM_NEXUS, -1},
    {-1, -1, ELEM_NETHER, -1},
    {-1, -1, ELEM_CHAOS, -1},
    {-1, -1, ELEM_DISEN, -1},
    {-1, OF_FEATHER, -1, -1},
    {-1, OF_PROT_FEAR, -1, TMD_BOLD},
    {-1, OF_PROT_BLIND, -1, -1},
    {-1, OF_PROT_CONF, -1, TMD_OPP_CONF},
    {-1, OF_PROT_STUN, -1, -1},

    {OBJ_MOD_LIGHT, -1, -1, -1},
    {-1, OF_REGEN, -1, -1},
    {-1, OF_ESP_ALL, -1, TMD_ESP},
    {-1, OF_SEE_INVIS, -1, TMD_SINVIS},
    {-1, OF_FREE_ACT, -1, -1},
    {-1, OF_HOLD_LIFE, -1, -1},
    {OBJ_MOD_STEALTH, -1, -1, -1},
    {OBJ_MOD_SEARCH, -1, -1, -1},
    {OBJ_MOD_INFRA, -1, -1, TMD_SINFRA},

    {OBJ_MOD_TUNNEL, -1, -1, -1},
    {OBJ_MOD_SPEED, -1, -1, TMD_FAST},
    {OBJ_MOD_BLOWS, -1, -1, -1},
    {OBJ_MOD_SHOTS, -1, -1, -1},
    {OBJ_MOD_MIGHT, -1, -1, -1},
    {-1, OF_SLOW_DIGEST, -1, -1},
    {-1, OF_IMPAIR_HP, -1, -1},
    {-1, OF_AFRAID, -1, TMD_AFRAID},
    {-1, OF_AGGRAVATE, -1, -1},

    {-1, OF_ESP_RADIUS, -1, -1},
    {-1, OF_ESP_EVIL, -1, -1},
    {-1, OF_ESP_ANIMAL, -1, -1},
    {-1, OF_ESP_UNDEAD, -1, -1},
    {-1, OF_ESP_DEMON, -1, -1},
    {-1, OF_ESP_ORC, -1, -1},
    {-1, OF_ESP_TROLL, -1, -1},
    {-1, OF_ESP_GIANT, -1, -1},
    {-1, OF_ESP_DRAGON, -1, -1}
};


static void prt_resistance_panel(struct player *p, int which, const struct player_flag_record *rec)
{
    size_t i;
    int j;
    int off = 1 + STAT_MAX + RES_ROWS * which;

    for (i = 0; i < RES_ROWS; i++)
    {
        /* Repeated extraction of flags is inefficient but more natural */
        for (j = 0; j <= p->body.count; j++)
        {
            bitflag f[OF_SIZE];
            byte attr = (COLOUR_WHITE | (j % 2) * 8); /* Alternating columns */
            char sym = '.';
            bool res = false, imm = false, vul = false, rune = false;
            bool timed = false;
            bool known = true;

            /* Object or player info? */
            if (j < p->body.count)
            {
                struct object *obj = slot_object(p, j);

                /* Wipe flagset */
                of_wipe(f);

                /* Get known properties */
                if (obj)
                {
                    bool aware = object_flavor_is_aware(p, obj);

                    object_flags_known(obj, f, aware);
                    if (rec[i].element != -1)
                        known = object_element_is_known(obj, rec[i].element, aware);
                    else if (rec[i].flag != -1)
                        known = object_flag_is_known(p, obj, rec[i].flag);
                }

                /* Get resistance, immunity and vulnerability info */
                if (rec[i].mod != -1)
                {
                    s32b modifiers[OBJ_MOD_MAX];

                    object_modifiers(obj, modifiers);

                    res = (modifiers[rec[i].mod] > 0);
                    vul = (modifiers[rec[i].mod] < 0);
                }
                else if (rec[i].flag != -1)
                    res = of_has(f, rec[i].flag);
                else if (rec[i].element != -1)
                {
                    struct element_info el_info[ELEM_MAX];

                    object_elements(obj, el_info);

                    imm = (known && (el_info[rec[i].element].res_level == 3));
                    res = (known && (el_info[rec[i].element].res_level == 1));
                    vul = (known && (el_info[rec[i].element].res_level == -1));
                }
            }
            else
            {
                player_flags(p, f);

                /* Timed flags only in the player column */
                if (rec[i].tmd_flag >= 0)
                {
                    timed = (p->timed[rec[i].tmd_flag]? true: false);

                    /* There has to be one special case... */
                    if ((rec[i].tmd_flag == TMD_AFRAID) && (p->timed[TMD_TERROR]))
                        timed = true;
                }

                /* Set which (if any) symbol and color are used */
                if (rec[i].mod != -1)
                    player_mods(p, rec[i].mod, &res, &vul);
                else if (rec[i].flag != -1)
                    res = of_has(f, rec[i].flag);
                else if (rec[i].element != -1)
                {
                    struct element_info el_info[ELEM_MAX];

                    player_elements(p, el_info);

                    imm = (el_info[rec[i].element].res_level == 3);
                    res = (el_info[rec[i].element].res_level == 1);
                    vul = (el_info[rec[i].element].res_level == -1);
                }
            }

            /* Check if the rune is known */
            if (rec[i].mod != -1)
                rune = (p->obj_k->modifiers[rec[i].mod] == 1);
            else if (rec[i].flag != -1)
                rune = of_has(p->obj_k->flags, rec[i].flag);
            else if (rec[i].element != -1)
                rune = (p->obj_k->el_info[rec[i].element].res_level == 1);

            /* Set the symbols and print them */
            if (imm) sym = '*';
            else if (res && !vul) sym = '+';
            else if (vul && !res) sym = '-';
            else if (timed) {sym = '!'; attr = COLOUR_L_GREEN;}
            else if (!known && !rune) sym = '?';

            /* Hack -- rune is known */
            if (rune) attr += BASIC_COLORS;

            p->hist_flags[off + i][j].a = attr;
            p->hist_flags[off + i][j].c = sym;
        }
    }
}


static void prt_player_flag_info(struct player *p)
{
    int i;

    for (i = 0; i <= RES_PANELS; i++)
        prt_resistance_panel(p, i, player_flag_table + i * RES_ROWS);
}


static void prt_flags(struct player *p)
{
    int i;

    prt_equippy(p);
    prt_player_sust_info(p);
    prt_player_flag_info(p);

    for (i = 0; i < N_HISTORY_FLAGS; i++) Send_objflags(p, i);
}


static void prt_plusses(struct player *p)
{
    int show_dd, show_ds;
    int show_mhit, show_mdam;
    int show_shit, show_sdam;

    get_plusses(p, &p->known_state, &show_dd, &show_ds, &show_mhit, &show_mdam, &show_shit,
        &show_sdam);

    Send_plusses(p, show_dd, show_ds, show_mhit, show_mdam, show_shit, show_sdam);
}


static void prt_misc(struct player *p)
{
    Send_char_info(p, p->race->ridx, p->clazz->cidx, p->psex);
}


static void prt_stats(struct player *p)
{
    int i;

    for (i = 0; i < STAT_MAX; i++) prt_stat(p, i);
}


static void prt_minimap(struct player *p)
{
    prt_map(p);
    if (p->window_flag & PW_MAP) fix_map(p);
}


static void prt_equip(struct player *p)
{
    display_equip(p);
    prt_flags(p);
    update_prevent_inscriptions(p);
}


static void prt_monster(struct player *p)
{
    if (p->window_flag & PW_MONSTER) fix_monster(p);
}


static void prt_monsterlist(struct player *p)
{
    /* Only if full refresh */
    if (!p->full_refresh) return;

    if (p->window_flag & PW_MONLIST) fix_monlist(p);
    p->upkeep->redraw &= ~(PR_MONLIST);
}


static void prt_object(struct player *p)
{
    if (p->window_flag & PW_OBJECT) fix_object(p);
}


static void prt_objectlist(struct player *p)
{
    /* Only if full refresh */
    if (!p->full_refresh) return;

    if (p->window_flag & PW_ITEMLIST) fix_objlist(p);
    p->upkeep->redraw &= ~(PR_ITEMLIST);
}


typedef struct
{
    u32b flag;
    void (*hook)(struct player *);
    bool remove_flag;
} flag_event_trigger;


/*
 * Events triggered by the various flags.
 */
static const flag_event_trigger redraw_events[] =
{
    {PR_MISC, prt_misc, true},
    {PR_TITLE, prt_title, true},
    {PR_LEV, prt_lvl, true},
    {PR_EXP, prt_exp, true},
    {PR_STATS, prt_stats, true},
    {PR_ARMOR, prt_ac, true},
    {PR_HP, prt_hp, true},
    {PR_MANA, prt_sp, true},
    {PR_GOLD, prt_gold, true},
    {PR_OBJECT, prt_object, true},
    {PR_ITEMLIST, prt_objectlist, false},
    {PR_HEALTH, prt_health, true},
    {PR_SPEED, prt_speed, true},
    {PR_STUDY, prt_study, true},
    {PR_DEPTH, prt_depth, true},
    {PR_STATUS, prt_status, true},
    {PR_DTRAP, prt_dtrap, true},
    {PR_STATE, prt_state, true},
    {PR_MAP, prt_minimap, true},
    {PR_INVEN, display_inven, true},
    {PR_EQUIP, prt_equip, true},
    {PR_MONSTER, prt_monster, true},
    {PR_MONLIST, prt_monsterlist, false},
    {PR_SPELL, fix_spell, true},
    {PR_PLUSSES, prt_plusses, true},
    {PR_CURSOR, cursor_redraw, true},
    {PR_FLOOR, prt_floor_item, true}
};


/*
 * Handle "p->upkeep->redraw"
 */
void redraw_stuff(struct player *p)
{
    size_t i;

    /* Nothing to do */
    if (!p->upkeep->redraw) return;

    /* Character is not ready yet, no screen updates */
    if (!p->alive) return;

    /* Hack -- while running, only update monster/object lists when panel changes */
    if (p->upkeep->running)
        p->full_refresh = p->upkeep->running_update;
    p->upkeep->running_update = false;

    /* For each listed flag, give an appropriate response */
    for (i = 0; i < N_ELEMENTS(redraw_events); i++)
    {
        const flag_event_trigger *hnd = &redraw_events[i];
        if (p->upkeep->redraw & hnd->flag)
        {
            if (hnd->remove_flag)
                p->upkeep->redraw &= ~(hnd->flag);
            hnd->hook(p);
        }
    }
}


/* Find player on arena "a", who is not player "p" */
static struct player *pick_arena_opponent(struct player *p, int a)
{
    int i;

    /* Count other players in this arena */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        if (player->arena_num == a)
        {
            /* Found someone */
            if (p != player) return player;
        }
    }

    /* No one found */
    return NULL;
}


static void msg_broadcast_arena(struct player *loser, struct player *winner, const char *msg)
{
    int i;

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Skip the specified players */
        if (player == loser) continue;
        if (player == winner) continue;

        /* Tell this one */
        msg_print(player, msg, MSG_BROADCAST_ARENA);
    }

    /* Send to console */
    console_print((char*)msg, 0);
}


/*
 * Increase players hit points to max and notice effects.
 */
void restore_hp(struct player *p)
{
    int old_num = get_player_num(p);

    /* Heal the player */
    p->chp = p->mhp;
    p->chp_frac = 0;

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Redraw */
    p->upkeep->redraw |= PR_HP;
}


/*
 * Increase players mana points to max and notice effects.
 */
void restore_sp(struct player *p)
{
    int old_num = get_player_num(p);

    /* Restore mana */
    p->csp = p->msp;
    p->csp_frac = 0;

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Redraw */
    p->upkeep->redraw |= PR_MANA;
}


/* Cleanup after PvP Fight in the arena */
static void evacuate_arena(struct player *p)
{
    char buf[100];
    struct player *q = pick_arena_opponent(p, p->arena_num);
    struct source who_body;
    struct source *who = &who_body;

    /* Paranoia */
    if (!q || (q == p)) return;

    /* Friendship */
    pvp_check(q, p, PVP_REMOVE, true, 0x00);
    pvp_check(p, q, PVP_REMOVE, true, 0x00);

    /* Messages */
    msg(p, "You lose consciousness.");
    msg(q, "You knock %s out.", p->name);

    /* Tell everyone about the outcome */
    strnfmt(buf, sizeof(buf), "%s was defeated by %s in the arena.", p->name, q->name);
    msg_broadcast_arena(p, q, buf);

    /* Remove potential bad effects */
    player_clear_timed(p, TMD_POISONED, true);
    player_clear_timed(q, TMD_POISONED, true);

    /* Give hit points and mana points back */
    restore_hp(p);
    restore_sp(p);
    restore_hp(q);
    restore_sp(q);

    /* Teleport */
    p->arena_num = -1;
    source_player(who, get_player_index(get_connection(p->conn)), p);
    effect_simple(EF_TELEPORT, who, "1", 0, 0, 0, NULL);
    q->arena_num = -1;
    source_player(who, get_player_index(get_connection(q->conn)), q);
    effect_simple(EF_TELEPORT, who, "1", 0, 0, 0, NULL);

    /* Messages */
    msg(p, "You recover outside the arena.");
    msg(q, "You gloriously leave the arena.");
}


struct cmp_val
{
    struct player *player;
    struct object *obj;
};


static int cmp_value(const void *a, const void *b)
{
    const struct cmp_val *pa = a;
    const struct cmp_val *pb = b;
    s32b va = 0, vb = 0;

    if (pa->obj->tval)
        va = (s32b)object_value(pa->player, pa->obj, 1);
    if (pb->obj->tval)
        vb = (s32b)object_value(pb->player, pb->obj, 1);

    if (va > vb) return -1;
    if (va < vb) return 1;
    return 0;
}


static void player_strip(struct player *p, bool perma_death)
{
    size_t i, count = 0;
    struct object *obj;
    struct cmp_val *gear;
    struct chunk *c = chunk_get(&p->wpos);

    /* Scan player's gear */
    for (obj = p->gear; obj; obj = obj->next) count++;
    gear = mem_zalloc(sizeof(*gear) * count);
    for (i = 0, obj = p->gear; obj; i++, obj = obj->next)
    {
        gear[i].player = p;
        gear[i].obj = obj;
    }

    /* Sort the player's gear according to value */
    sort(gear, count, sizeof(*gear), cmp_value);

    /* Starting with the most valuable, drop things one by one */
    for (i = 0; i < count; i++)
    {
        obj = gear[i].obj;

        /* If we will permanently die, drop nothing */
        if (perma_death)
        {
            gear_excise_object(p, obj);

            /* Preserve any artifact */
            preserve_artifact_aux(obj);
            if (obj->artifact) history_lose_artifact(p, obj);

            object_delete(&obj);

            continue;
        }

        /* Never drop deeds of property */
        if (tval_is_deed(obj)) continue;

        /* Excise the object and drop it */
        gear_excise_object(p, obj);
        drop_near(p, c, &obj, 0, p->py, p->px, false, DROP_FADE);
    }

    mem_free(gear);

    if (perma_death) return;

    /* Drop gold if player has any */
    if (p->alive && p->au)
    {
        /* Setup the object */
        obj = object_new();
        object_prep(p, obj, money_kind("gold", p->au), 0, MINIMISE);

        /* Set the amount */
        obj->pval = p->au;

        /* No more gold */
        p->au = 0;

        /* Drop it */
        drop_near(p, c, &obj, 0, p->py, p->px, false, DROP_FADE);
    }
}


static void player_funeral(struct player *p)
{
    /* Clear his houses */
    reset_houses(p);

    /* Remove him from his party */
    if (p->party)
    {
        /* He leaves */
        party_leave(p);
    }

    /* Kill him */
    if (!p->alive) p->is_dead = true;

    /* One less player here */
    leave_depth(p, chunk_get(&p->wpos));

    /* Remove him from the player name database */
    remove_player_name(p->name);

    /* Put him on the high score list */
    enter_score(p, &p->death_info.time);

    /* Perform a full redraw */
    message_flush(p);
    refresh_stuff(p);

    /* Send any remaining information over the network */
    Net_output_p(p);

    /* Get rid of him */
    p->upkeep->funeral = true;

    /* Display the winner crown */
    if (p->total_winner) Send_winner(p);

    /* Display the tombstone */
    else Send_death_cause(p);
}


void player_dump(struct player *p, bool server)
{
    char dumpname[42];

    /* Only record the original death */
    if (p->ghost == 1) return;

    /* Save the server-side character dump (used by the online ladder) */
    if (server)
    {
        strnfmt(dumpname, sizeof(dumpname), "%s-%s.txt", p->name, ht_show(&turn));
        p->ladder = true;
        if (dump_save(p, dumpname))
            plog("Character dump successful.");
        else
            plog("Character dump failed!");
    }

    /* Save a client-side character dump */
    strnfmt(dumpname, sizeof(dumpname), "%s.txt", p->name);
    p->ladder = false;
    if (dump_save(p, dumpname))
        plog("Character dump successful.");
    else
        plog("Character dump failed!");
}


/*
 * Know inventory and home items upon death
 */
void death_knowledge(struct player *p)
{
    struct object *obj;

    for (obj = p->gear; obj; obj = obj->next)
        object_notice_everything(p, obj);

    know_houses(p);

    history_unmask_unknown(p);

    /* Hack -- recalculate bonuses */
    p->upkeep->update |= (PU_BONUS);
    handle_stuff(p);
}


/*
 * Handle the death of a player and drop their stuff.
 *
 * changed so players remain in the team when killed
 * changed so when leader ghosts perish the team is disbanded
 */
void player_death(struct player *p)
{
    char buf[MSG_LEN];
    const char *prompt = get_title(p);
    bool perma_death, suicided = false;
    int i;
    u16b type = MSG_BROADCAST_DIED;
    bool no_ghost = (OPT(p, birth_no_ghost) || OPT(p, birth_fruit_bat) || cfg_no_ghost);
    struct source who_body;
    struct source *who = &who_body;

    /* Hack -- don't die in Arena! */
    if (p->alive && (p->arena_num != -1))
    {
        p->is_dead = false;
        evacuate_arena(p);
        return;
    }

    /* Hack -- note death */
    msgt(p, MSG_DEATH, (p->ghost? "Your incorporeal body fades away - FOREVER.": "You die."));
    message_flush(p);
    if (p->ghost != 1)
    {
        strnfmt(buf, sizeof(buf), "Was killed by %s", p->died_from);
        history_add(p, buf, HIST_PLAYER_DEATH);
    }

    /* Tell everyone he died */
    if (p->ghost == 1)
    {
        strnfmt(buf, sizeof(buf), "%s %s's ghost was destroyed by %s.", prompt, p->name,
            p->died_from);
    }
    else if (p->alive)
    {
        char brave[40];

        if ((OPT(p, birth_no_ghost) && !cfg_no_ghost) ||
            (OPT(p, birth_no_recall) && (cfg_diving_mode < 3)) ||
            (OPT(p, birth_force_descend) && (cfg_limit_stairs < 3)))
        {
            strnfmt(brave, sizeof(brave), "The%s%s%s",
                (OPT(p, birth_no_ghost) && !cfg_no_ghost)? " brave": "",
                (OPT(p, birth_no_recall) && (cfg_diving_mode < 3))? " hardcore": "",
                (OPT(p, birth_force_descend) && (cfg_limit_stairs < 3))? " diving": "");
        }
        else
            my_strcpy(brave, "The unfortunate", sizeof(brave));

        strnfmt(buf, sizeof(buf), "%s %s %s the level %i %s %s %s.", brave, prompt, p->name, p->lev,
            p->race->name, p->clazz->name, p->died_flavor);
    }
    else if (!strcmp(p->died_from, "divine wrath"))
        strnfmt(buf, sizeof(buf), "%s was killed by divine wrath.", p->name);
    else if (!p->total_winner)
    {
        strnfmt(buf, sizeof(buf), "%s was terminated.", p->name);
        suicided = true;
        type = MSG_BROADCAST_ENTER_LEAVE;
    }
    else
    {
        strnfmt(buf, sizeof(buf), "The unbeatable %s has retired to a warm, sunny climate.",
            p->name);
        type = MSG_BROADCAST_WON;
    }

    /* Tell the players - handle the secret_dungeon_master option */
    if (!(p->dm_flags & DM_SECRET_PRESENCE))
    {
        /* Don't broadcast level 1 suicides */
        if (!suicided || (p->lev > 1))
            msg_broadcast(p, buf, type);
    }

    /*
     * Handle permanent death:
     * - all characters have a chance to die permanently (based on number of past deaths)
     * - no ghost characters (except Necromancers that can turn into an undead being)
     * - Dragon characters
     * - ghosts
     * - suiciding characters
     */
    perma_death = (magik(p->lives) || (no_ghost && !player_can_undead(p)) ||
        player_has(p, PF_DRAGON) || p->ghost || !p->alive);

    /* Know inventory and home items upon permadeath */
    if (perma_death) death_knowledge(p);

    /* Death dump (except ghosts and retiring winners) */
    if ((p->ghost != 1) && !(p->total_winner && !p->alive))
        player_dump(p, p->alive);

    /*
     * Handle every item (including gold):
     * - delete them for all characters that will permanently die
     * - keep them for Necromancers that can turn into an undead being
     * - drop them on the floor for all other characters
     */
    if (perma_death || !player_can_undead(p)) player_strip(p, perma_death);

    /* Tell him */
    if ((p->ghost != 1) && p->alive)
        msgt(p, MSG_DIED, "You have been killed by %s.", p->died_from);

    /* Handle permanent death */
    if (perma_death)
    {
        player_funeral(p);

        /* Done */
        return;
    }

    /* Handle polymorphed players */
    if (p->poly_race) do_cmd_poly(p, NULL, false, true);

    /* Cancel current effects */
    for (i = 0; i < TMD_MAX; i++) player_clear_timed(p, i, true);

    /* Turn him into a ghost */
    set_ghost_flag(p, 1, true);

    /* Give him his hit points and mana points back */
    restore_hp(p);
    restore_sp(p);

    /* Teleport him */
    source_player(who, get_player_index(get_connection(p->conn)), p);
    effect_simple(EF_TELEPORT, who, "200", 0, 0, 0, NULL);

    /* Feed him (maybe he died from starvation) */
    player_set_food(p, PY_FOOD_MAX - 1);

    /* Remove the death flag */
    p->is_dead = false;

    /* Cancel any WOR spells */
    p->word_recall = 0;
    p->deep_descent = 0;

    /* Notice, update and redraw */
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->redraw |= (PR_STATE | PR_BASIC | PR_PLUSSES | PR_INVEN | PR_SPELL);
}

/*
 * Resurrect a player
 */
void resurrect_player(struct player *p, struct chunk *c)
{
    /* Hack -- the dungeon master cannot resurrect */
    if (is_dm_p(p)) return;

    /* Message */
    msg(p, "You feel life return to your body.");

    /* Treat the spells of Undead Form and Death as special cases */
    if (p->ghost == 2)
    {
        /* Reset ghost flag */
        set_ghost_flag(p, 0, true);

        /* Redraw */
        p->upkeep->redraw |= (PR_TITLE);
        square_light_spot(c, p->py, p->px);

        return;
    }

    /* Log event */
    history_add(p, "Resurrected", HIST_PLAYER_REVIVE);

    /* Reset ghost flag */
    set_ghost_flag(p, 0, true);

    /* Lose some experience */
    player_exp_lose(p, p->max_exp / 2, true);

    /* Lose some CON */
    if (magik(p->lives))
    {
        /* Sometimes decrease it permanently */
        msg(p, "The process leaves you extremely weak and tired!");
        player_stat_dec(p, STAT_CON, true);
    }
    else
    {
        /* Otherwise just drain it */
        msg(p, "The process leaves you somewhat exhausted...");
        player_stat_dec(p, STAT_CON, false);
    }

    /* Increment number of resurrections */
    p->lives++;

    /* Redraw */
    p->upkeep->redraw |= (PR_BASIC | PR_SPELL);
    square_light_spot(c, p->py, p->px);

    /* Update */
    p->upkeep->update |= (PU_BONUS);

    /* Reset the death info */
    memset(&p->death_info, 0, sizeof(p->death_info));
}


static bool panel_should_modify(struct player *p, int *wy, int *wx)
{
    int screen_hgt, screen_wid;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    /* Verify wy, adjust if needed */
    if (*wy > z_info->dungeon_hgt - screen_hgt) *wy = z_info->dungeon_hgt - screen_hgt;
    if (*wy < 0) *wy = 0;

    /* Verify wx, adjust if needed */
    if (*wx > z_info->dungeon_wid - screen_wid) *wx = z_info->dungeon_wid - screen_wid;
    if (*wx < 0) *wx = 0;

    /* Needs changes? */
    return ((p->offset_y != *wy) || (p->offset_x != *wx));
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return true if anything done.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(struct player *p, int wy, int wx)
{
    /* React to changes */
    if (panel_should_modify(p, &wy, &wx))
    {
        /* Save wy, wx */
        p->offset_y = wy;
        p->offset_x = wx;

        /* Update stuff */
        p->upkeep->update |= (PU_MONSTERS);

        /* Redraw map */
        p->upkeep->redraw |= (PR_MAP);

        /* Hack -- while running, only update object/monster lists when panel changes */
        if (p->upkeep->running) p->upkeep->running_update = true;
      
        /* Changed */
        return true;
    }

    /* No change */
    return false;
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return true if the panel was changed.
 */
bool change_panel(struct player *p, int dir)
{
    bool changed = false;
    int wx, wy;
    int screen_hgt, screen_wid;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return false;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    /* Shift by half a panel */
    wy = p->offset_y + ddy[dir] * screen_hgt / 2;
    wx = p->offset_x + ddx[dir] * screen_wid / 2;

    /* Use "modify_panel" */
    if (modify_panel(p, wy, wx)) changed = true;

    return (changed);
}


/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * The "center_player" option allows the current panel to always be centered
 * around the player, which is very expensive, and also has some interesting
 * gameplay ramifications.
 */
static void verify_panel_int(struct player *p, bool centered)
{
    int wy, wx;
    int panel_wid, panel_hgt;
    int py = p->py;
    int px = p->px;
    int screen_hgt, screen_wid;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    wy = p->offset_y;
    wx = p->offset_x;

    panel_wid = screen_wid / 2;
    panel_hgt = screen_hgt / 2;

    /* Scroll screen vertically when off-center */
    if (centered && !p->upkeep->running && (py != wy + panel_hgt))
        wy = py - panel_hgt;

    /* Scroll screen vertically when 3 grids from top/bottom edge */
    else if ((py < wy + 3) || (py >= wy + screen_hgt - 3))
        wy = py - panel_hgt;

    /* Scroll screen horizontally when off-center */
    if (centered && !p->upkeep->running && (px != wx + panel_wid))
        wx = px - panel_wid;

    /* Scroll screen horizontally when 3 grids from left/right edge */
    else if ((px < wx + 3) || (px >= wx + screen_wid - 3))
        wx = px - panel_wid;

    /* Scroll if needed */
    modify_panel(p, wy, wx);
}


void verify_panel(struct player *p)
{
    verify_panel_int(p, OPT(p, center_player));
}


/*
 * Center the current panel.
 */
void center_panel(struct player *p)
{
    verify_panel_int(p, true);
}


/*
 * This table provides for different game speeds at different dungeon depths. Shallower depths
 * are faster, allowing for easier navigation. Deeper depths are slow, hopefully making
 * deep combat less of a test of reflexes.
 *
 * PWMAngband:
 * Game speed has been reduced again below 4250' to give more reaction time to the player
 * during fights with fast monsters. Game speed now caps at 4950', allowing the player to get
 * the same turn duration against "Fast (+20)" monsters as you would get on the surface against
 * unhasted monsters. Game speed has been greatly reduced on quest levels (depths 100/124+)
 * to get the same turn duration against "Fast (+30)" questors.
 */
static u16b level_speeds[] =
{
     75,  90,  91,  92,  93,  94,  95,  96,  97,  98,   /* Town - 450' */
     99, 100, 101, 102, 103, 104, 105, 106, 107, 108,   /* 500' - 950' */
    109, 110, 111, 112, 113, 114, 115, 116, 117, 118,   /* 1000' - 1450' */
    119, 120, 121, 122, 123, 124, 125, 126, 127, 128,   /* 1500' - 1950' */
    129, 130, 131, 132, 133, 134, 135, 136, 137, 138,   /* 2000' - 2450' */
    139, 140, 141, 142, 143, 144, 145, 146, 147, 148,   /* 2500' - 2950' */
    150, 152, 154, 156, 158, 160, 162, 164, 166, 168,   /* 3000' - 3450' */
    170, 172, 174, 176, 178, 180, 182, 184, 186, 188,   /* 3500' - 3950' */
    190, 192, 194, 196, 198, 200, 202, 204, 206, 208,   /* 4000' - 4450' */
    210, 212, 214, 216, 218, 220, 222, 223, 224, 225,   /* 4500' - 4950' */
    285, 225, 225, 225, 225, 225, 225, 225, 225, 225,   /* 5000' - 5450' */
    225, 225, 225, 225, 225, 225, 225, 225, 225, 225,   /* 5500' - 5950' */
    225, 225, 225, 225, 285, 285, 285, 285              /* 6000' - 6350' */
};


/*
 * Hack -- since the framerate has been boosted by five times since version
 * 0.6.0 to make game movement more smooth, we return the old level speed
 * times five to keep the same movement rate.
 */
int move_energy(int depth)
{
    return level_speeds[depth] * 5 * z_info->move_energy;
}


/*
 * Determine the speed of a given players "time bubble" and return a percentage
 * scaling factor which should be applied to any amount of energy granted to
 * players/monsters within the bubble.
 *
 * We check this player and then any other players recursively, the time of the
 * slowest bubble below normal time overrules other adjoining bubbles. This is
 * to support the scenario where a long chain of players may be stood just within
 * each others range. Forming a time bubble chain. :)
 *
 * When calling this function pass slowest as zero, which acts as a flag
 * that this is the main call, not a recursive call.
 */
static int base_time_factor(struct player *p, struct chunk *c, int slowest)
{
    int i, timefactor, health;

    /* Paranoia */
    if (!p) return NORMAL_TIME;

    /* If this is the initial call, reset time bubble check for all players on the level */
    for (i = 1; !slowest && (i <= NumPlayers); i++)
    {
        struct player *q = player_get(i);

        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;
        q->bubble_checked = false;
    }

    /* Normal time scale */
    timefactor = NORMAL_TIME;

    /* What's our percentage health? */
    health = (p->chp * 100) / p->mhp;

    /* Don't allow time to slow asymptotically towards infinity */
    if (health < MIN_TIME_SCALE) health = MIN_TIME_SCALE;

    /* Scale depending on health if HP are low enough */
    if (health <= p->opts.hitpoint_warn * 10)
        timefactor = timefactor * health / 100;

    /* Resting speeds up time disregarding health time scaling */
    if (player_is_resting(p)) timefactor = MAX_TIME_SCALE;

    /*
     * If this is a check for another player give way to their time
     * bubble if we aren't doing anything important
     */
    if (slowest && (timefactor == NORMAL_TIME))
    {
        bool los = false;

        /* If nothing in LoS */
        for (i = 1; i < cave_monster_max(c); i++)
        {
            struct monster *mon = cave_monster(c, i);
            bool incapacitated = (mon->m_timed[MON_TMD_SLEEP] || mon->m_timed[MON_TMD_HOLD]);

            if (!mon->race) continue;

            /* Check this monster */
            if (monster_is_in_view(p, i) && !incapacitated)
            {
                los = true;
                break;
            }
        }

        /* We don't really care about our time */
        if (!los) timefactor = MAX_TIME_SCALE;
    }

    /* We have checked our time bubble */
    p->bubble_checked = true;

    /* Check all other players within our range */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);
        int dist;

        /* Skip him if he's on a different dungeon level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* Only check them if they haven't already been checked */
        if (q->bubble_checked) continue;

        /* How far away is he? */
        dist = distance(p->py, p->px, q->py, q->px);

        /* Skip him if he's too far away */
        if (dist > z_info->max_sight) continue;

        /* Find the slowest time bubble chain we are part of */
        slowest = base_time_factor(q, c, timefactor);

        /* Use the slowest time bubble */
        if (slowest < timefactor) timefactor = slowest;
    }

    return timefactor;
}


/*
 * Determine the speed of a given players "time bubble" when the player is in town.
 *
 * This is a simplified version of base_time_factor() without concern for monsters or health.
 */
static int base_time_factor_simple(struct player *p, int slowest)
{
    int i, timefactor;

    /* Paranoia */
    if (!p) return NORMAL_TIME;

    /* If this is the initial call, reset time bubble check for all players on the level */
    for (i = 1; !slowest && (i <= NumPlayers); i++)
    {
        struct player *q = player_get(i);

        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;
        q->bubble_checked = false;
    }

    /* Normal time scale */
    timefactor = NORMAL_TIME;

    /* Resting speeds up time */
    if (player_is_resting(p)) timefactor = MAX_TIME_SCALE;

    /*
     * If this is a check for another player give way to their time
     * bubble if we aren't doing anything important
     */
    if (slowest && (timefactor == NORMAL_TIME)) timefactor = MAX_TIME_SCALE;

    /* We have checked our time bubble */
    p->bubble_checked = true;

    /* Check all other players within our range */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);
        int dist;

        /* Skip him if he's on a different dungeon level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* Only check them if they haven't already been checked */
        if (q->bubble_checked) continue;

        /* How far away is he? */
        dist = distance(p->py, p->px, q->py, q->px);

        /* Skip him if he's too far away */
        if (dist > z_info->max_sight) continue;

        /* Find the slowest time bubble chain we are part of */
        slowest = base_time_factor_simple(q, timefactor);

        /* Use the slowest time bubble */
        if (slowest < timefactor) timefactor = slowest;
    }

    return timefactor;
}


/*
 * Determine the given players current time factor.
 */
int time_factor(struct player *p, struct chunk *c)
{
    /* Paranoia */
    if (!p) return NORMAL_TIME;

    /* Use basic time scaling in towns */
    if (in_town(&p->wpos)) return base_time_factor_simple(p, 0);

    /* Scale our time by our bubbles time factor */
    return base_time_factor(p, c, 0);
}


/*
 * Dungeon master commands
 */


static void unstatic_level(struct player *p)
{
    int num_on_depth = 0, i;

    /* Figure out how many players are currently on the level */
    for (i = 1; i <= NumPlayers; i++)
    {
        if (COORDS_EQUAL(&player_get(i)->wpos, &p->wpos)) num_on_depth++;
    }

    /*
     * Set the number of players on the level equal to the number of
     * currently connected players on the level.
     */
    chunk_set_player_count(&p->wpos, num_on_depth);
}


static void manual_design(struct player *p, struct chunk *c, bool new_level)
{
    int i;

    /* Not in towns or quest levels */
    if (in_town(&p->wpos)) return;
    if (is_quest(p->wpos.depth)) return;

    /* Level is already locked */
    if (chunk_inhibit_players(&p->wpos)) return;

    msg(p, "Entering manual design mode...");

    /* Unstatic the level */
    unstatic_level(p);

    /* Recall all other players currently on the level immediately */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        if (q == p) continue;
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* No-recall players are simply pushed up one level (should be safe) */
        if ((cfg_diving_mode == 3) || OPT(q, birth_no_recall))
        {
            struct worldpos wpos;

            COORDS_SET(&wpos, q->wpos.wy, q->wpos.wx, dungeon_get_next_level(q, q->wpos.depth, -1));
            msg(p, "You are pulled upwards.");
            dungeon_change_level(q, chunk_get(&q->wpos), &wpos, LEVEL_GHOST);
            continue;
        }

        /* If in the wilderness, go to starting town */
        if (in_wild(&q->wpos))
            memcpy(&q->recall_wpos, start_wpos(), sizeof(struct worldpos));

        q->word_recall = 0;
        q->deep_descent = 0;
        recall_player(q, c);
    }

    /* Lock the level */
    chunk_set_player_count(&p->wpos, INHIBIT_DEPTH);

    /* New level */
    if (new_level)
    {
        bool daytime;

        /* Deallocate the level */
        chunk_list_remove(c);
        cave_wipe(c);

        /* Make a new chunk */
        c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);
        memcpy(&c->wpos, &p->wpos, sizeof(struct worldpos));
        player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

        /* Dungeon */
        if (p->wpos.depth > 0)
        {
            daytime = true;

            /* Generate a new empty level */
            draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM, SQUARE_NONE);
            fill_rectangle(c, 1, 1, c->height - 2, c->width - 2, FEAT_FLOOR, SQUARE_NONE);
        }

        /* Wilderness */
        else
        {
            daytime = is_daytime();

            /* Generate a new basic wilderness level */
            wilderness_gen_basic_layout(c);
        }

        /* Apply illumination */
        player_cave_clear(p, true);
        cave_illuminate(p, c, daytime);

        chunk_list_add(c);
        ht_copy(&c->generated, &turn);

        c->squares[p->py][p->px].mon = 0 - get_player_index(get_connection(p->conn));

        square_set_join_up(c, 0, 0);
        square_set_join_down(c, 0, 0);
    }

    square_set_join_rand(c, 0, 0);
}


static void exit_design(struct player *p, struct chunk *c, bool town)
{
    /* Level is not locked */
    if (!chunk_inhibit_players(&p->wpos)) return;

    /* Dungeon */
    if (p->wpos.depth > 0)
    {
        int yy, xx;
        int tries = 10000;

        /* Check level_up and level_down */
        if (!c->join->up.y && !c->join->up.x)
        {
            msg(p, "There is no down staircase on this level!");
            return;
        }
        if (!c->join->down.y && !c->join->down.x)
        {
            msg(p, "There is no up staircase on this level!");
            return;
        }

        /* Set level_rand */
        while (tries > 0)
        {
            tries--;

            /* Pick a legal spot */
            yy = rand_range(1, c->height - 2);
            xx = rand_range(1, c->width - 2);

            /* Must be a "naked" floor grid */
            if (square_isempty(c, yy, xx)) break;
        }
        if (!tries)
        {
            msg(p, "There are no naked floor grids on this level!");
            return;
        }

        square_set_join_rand(c, yy, xx);
    }

    msg(p, "Exiting manual design mode...");

    /* Hack -- clear player count */
    chunk_set_player_count(&p->wpos, 0);

    /* Save manually-designed dungeon level to file */
    save_dungeon_special(&p->wpos, town);

    /* Unlock the level */
    chunk_set_player_count(&p->wpos, 1);
}


/*
 * Static or unstatic a level
 */
static void master_level(struct player *p, char *parms)
{
    struct chunk *c = chunk_get(&p->wpos);

    /* Paranoia -- make sure the player is on a valid level */
    if (!c) return;

    switch (parms[0])
    {
        /* Unstatic the level */
        case 'u':
        {
            unstatic_level(p);
            msg(p, "The level has been unstaticed.");
            break;
        }

        /* Static the level */
        case 's':
        {
            /* Increase the number of players on the DM level by one. */
            chunk_increase_player_count(&p->wpos);
            msg(p, "The level has been staticed.");
            break;
        }

        /* Enter manual design (new level) */
        case 'w':
        {
            manual_design(p, c, true);
            break;
        }

        /* Enter manual design (same level) */
        case 'm':
        {
            manual_design(p, c, false);
            break;
        }

        /* Exit manual design (create town) */
        case 't':
        {
            exit_design(p, c, true);
            break;
        }

        /* Exit manual design (create level) */
        case 'x':
        {
            exit_design(p, c, false);
            break;
        }

    }
}


static char master_specific_race_char = 'a';


static bool master_summon_specific_aux(struct monster_race *race)
{
    /* No uniques */
    if (monster_is_unique(race)) return false;

    /* If we look like what we are looking for */
    if (race->d_char == master_specific_race_char) return true;
    return false;
}


/*
 * Takes a monster name and returns its race, or NULL if no such monster was found.
 */
static struct monster_race *race_from_name(char *name)
{
    char monster[NORMAL_WID];
    char *str;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each monster race */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Skip non-entries */
        if (!r_info[i].name) continue;

        /* Clean up monster name */
        clean_name(monster, r_info[i].name);

        /* If cleaned name matches our search string, return it */
        if (!strcmp(monster, name)) return &r_info[i];
    }

    return NULL;
}


/*
 * Takes a (partial) monster name and returns its race, or NULL if no match was found.
 */
static struct monster_race *race_from_name_fuzzy(char *name)
{
    char monster[NORMAL_WID];
    char *str;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each monster race */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Skip non-entries */
        if (!r_info[i].name) continue;

        /* Clean up monster name */
        clean_name(monster, r_info[i].name);

        /* If cleaned name matches our search string, return it */
        if (strstr(monster, name)) return &r_info[i];
    }

    return NULL;
}


/*
 * Auxillary function to master_summon, determine the exact type of monster
 * to summon from a more general description.
 */
static struct monster_race *master_summon_aux_monster_type(struct chunk *c, char monster_type,
    char *monster_parms)
{
    /* Handle each category of monster types */
    switch (monster_type)
    {
        /* Specific monster specified */
        case 's':
        {
            struct monster_race *race;

            /* Summon monster given by race name */
            if (strlen(monster_parms) > 1)
            {
                /* Race prefixed by '#': summon from exact race name */
                if (monster_parms[0] == '#') return race_from_name(monster_parms + 1);

                /* Summon monster given by partial race name */
                return race_from_name_fuzzy(monster_parms);
            }
            
            /* Summon monster given by race symbol */
            master_specific_race_char = monster_parms[0];
            get_mon_num_prep(master_summon_specific_aux);
            race = get_mon_num(c, randint0(100) + 10, true);
            get_mon_num_prep(NULL);

            /* Return our monster */
            return race;
        }

        /* Specific depth specified */
        case 'd':
            return get_mon_num(c, monster_parms[0], true);
    }

    /* Failure */
    return 0;
}


/*
 * Monster summoning options.
 */
static void master_summon(struct player *p, char *parms)
{
    /* Type: by number, group, pet, summoning mode on, summoning mode off */
    static char type = 0;

    /* Number of monsters to summon */
    static char count = 0;

    /* Monster type: specific, by depth, banishment */
    static char monster_type = 0;

    /* Monster parameter: name or depth */
    static char monster_parms[NORMAL_WID];

    /* How many monsters to actually summon */
    unsigned char size = 0;

    /* Which monster to actually summon, from previous variables */
    struct monster_race *race = NULL;

    int i;
    struct chunk *c = chunk_get(&p->wpos);

    /* Extract arguments. If none are found, summon previous type (for master_move_hook). */
    if (parms)
    {
        type = parms[0];
        count = parms[1];
        monster_type = parms[2];

        /* Hack -- since monster_parms is a string, throw it on the end */
        my_strcpy(monster_parms, &parms[3], sizeof(monster_parms));
    }

    switch (type)
    {
        /* Summon x here */
        case 'x':
        {
            /* For each monster we are summoning */
            for (i = 0; i < count; i++)
            {
                /* Hack -- monster_type 'b' specifies mass banishment */
                if (monster_type == 'b')
                {
                    struct source who_body;
                    struct source *who = &who_body;

                    source_player(who, get_player_index(get_connection(p->conn)), p);
                    effect_simple(EF_MASS_BANISH, who, "0", 0, 0, 0, NULL);
                    break;
                }

                /* Figure out who to summon */
                race = master_summon_aux_monster_type(c, monster_type, monster_parms);

                /* Summon the monster, if we have a valid one */
                if (race) summon_specific_race(p, c, p->py, p->px, race, 1);
            }
            break;
        }

        /* Summon x at random locations */
        case 'X':
        {
            for (i = 0; i < count; i++)
            {
                /* Figure out who to summon */
                race = master_summon_aux_monster_type(c, monster_type, monster_parms);

                /* Summon the monster at a random location */
                if (race) summon_specific_race_somewhere(p, c, race, 1);
            }
            break;
        }

        /* Summon group of random size here */
        case 'g':
        {
            /* Figure out how many to summon */
            size = randint0(randint0(50)) + 2;

            /* Figure out who to summon */
            race = master_summon_aux_monster_type(c, monster_type, monster_parms);

            /* Summon the group here */
            if (race) summon_specific_race(p, c, p->py, p->px, race, size);

            break;
        }

        /* Summon group of random size at random location */
        case 'G':
        {
            /* Figure out how many to summon */
            size = randint0(randint0(50)) + 2;

            /* Figure out who to summon */
            race = master_summon_aux_monster_type(c, monster_type, monster_parms);

            /* Summon the group at a random location */
            if (race) summon_specific_race_somewhere(p, c, race, size);

            break;
        }

        /* Summon pet here */
        case 'P':
        {
            /* Figure out who to summon */
            race = master_summon_aux_monster_type(c, monster_type, monster_parms);

            /* Summon the monster, if we have a valid one */
            if (race) summon_specific_race_aux(p, c, p->py, p->px, race, 1, true);

            break;
        }

        /* Summon mode on (use with discretion... lets not be TOO mean ;-) )*/
        case 'T':
        {
            type = 'x';
            count = 1;

            master_move_hook = master_summon;
            break;
        }

        /* Summon mode off */
        case 'F':
        {
            master_move_hook = NULL;
            break;
        }
    }
}


static struct vault *get_vault_byfuzzyname(char *name)
{
    struct vault *v; 
    char buf[NORMAL_WID];
    char *str;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    for (v = vaults; v; v = v->next)
    {
        /* Clean up name */
        clean_name(buf, v->name);

        /* If cleaned name matches our search string, return it */
        if (strstr(buf, name)) return v;
    }

    return NULL;
}


static struct vault *get_vault_byname(char *name)
{
    struct vault *v;

    for (v = vaults; v; v = v->next)
    {
        if (streq(v->name, name)) return v;
    }

    return NULL;
}


#define GE_IDX  0
#define GE_NEXT 1
#define GE_PREV 2


static struct object_kind *item_kind(struct object_kind *k, int method)
{
    int i = k->kidx;

    if (method == GE_NEXT) i++;
    if (method == GE_PREV) i--;

    while ((i > 0) && (i < z_info->k_max))
    {
        struct object_kind *kind = &k_info[i];

        /* Only allocated items */
        if (kind->alloc_prob > 0) return kind;

        if (method == GE_IDX) break;
        if (method == GE_NEXT) i++;
        if (method == GE_PREV) i--;
    }

    return NULL;
}


static bool kind_match_char(struct object_kind *kind, char d_char)
{
    if (!d_char) return true;

    /* Hack -- use '*' for rods since '-' will always look for a wand */
    if (d_char == '*') return (kind->tval == TV_ROD);

    return (d_char == kind->d_char);
}


static struct object_kind *item_kind_fuzzy(char *name)
{
    char match[NORMAL_WID];
    char *str;
    int i;
    char d_char = '\0';

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* Check if a symbol has been passed (!speed, =strength...) */
    if ((*name < 'a') || (*name > 'z')) d_char = *name;

    /* For each item kind race */
    for (i = 0; i < z_info->k_max; i++)
    {
        struct object_kind *kind = &k_info[i];

        if (!kind->name) continue;

        /* Check default object character */
        if (!kind_match_char(kind, d_char)) continue;

        /* Clean up its name */
        clean_name(match, kind->name);

        /* If cleaned name matches our search string, return it */
        if (strstr(match, (d_char? (name + 1): name))) return kind;
    }

    return NULL;
}


/*
 * Check an ego item from e_info
 */
static bool check_ego(const struct object *obj, struct ego_item *ego)
{
    struct poss_item *poss;

    /* Test if this is a legal ego item type for this object */
    for (poss = ego->poss_items; poss; poss = poss->next)
    {
        if (poss->kidx == obj->kind->kidx) return true;
    }

    return false;
}


static struct ego_item *item_ego(const struct object *obj, struct ego_item *e, int method)
{
    int i = e->eidx;

    if (method == GE_NEXT) i++;
    if (method == GE_PREV) i--;

    while ((i >= 0) && (i < z_info->e_max))
    {
        struct ego_item *ego = &e_info[i];

        /* Only allowed egos */
        if (check_ego(obj, ego)) return ego;

        if (method == GE_IDX) break;
        if (method == GE_NEXT) i++;
        if (method == GE_PREV) i--;
    }

    return NULL;
}


static struct ego_item *item_ego_fuzzy(const struct object *obj, char *name)
{
    char match[NORMAL_WID];
    char *str;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each ego kind race */
    for (i = 0; i < z_info->e_max; i++)
    {
        struct ego_item *ego = &e_info[i];

        if (!ego->name) continue;

        /* Check ego first to skip egos of incorrect kind with same name */
        if (!check_ego(obj, ego)) continue;

        /* Clean up its name */
        clean_name(match, ego->name);

        /* If cleaned name matches our search string, return it */
        if (strstr(match, name)) return ego;
    }

    return NULL;
}


static bool tohit_varies(const struct object *obj)
{
    /* Normal magic ammo are always +0 +0 */
    if (tval_is_ammo(obj) && of_has(obj->flags, OF_AMMO_MAGIC)) return false;

    /* Mage weapons and dark swords are always +0 +0 */
    if (tval_is_mstaff(obj) || tval_is_dark_sword(obj)) return false;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj)) return true;

    /* Need a to-hit */
    if (!obj->to_h) return false;

    /* Base items with variable to-hit */
    if (randcalc_varies(obj->kind->to_h)) return true;

    /* Ego items with variable to-hit */
    if (obj->ego && randcalc_varies(obj->ego->to_h)) return true;

    return false;
}


static s16b tohit_min(const struct object *obj)
{
    int base;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj))
    {
        if (obj->ego) return obj->ego->min_to_h;
        return 0;
    }

    /* Base items with variable to-hit */
    base = randcalc(obj->kind->to_h, z_info->max_depth - 1, MINIMISE);
    if (randcalc_varies(obj->kind->to_h)) return base;

    /* Ego items with variable to-hit */
    return base + randcalc(obj->ego->to_h, z_info->max_depth - 1, MINIMISE);
}


static s16b tohit_max(const struct object *obj)
{
    int base;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj)) return 255;

    /* Base items with variable to-hit */
    base = randcalc(obj->kind->to_h, z_info->max_depth - 1, MAXIMISE);
    if (randcalc_varies(obj->kind->to_h)) return base;

    /* Ego items with variable to-hit */
    return base + randcalc(obj->ego->to_h, z_info->max_depth - 1, MAXIMISE);
}


static bool todam_varies(const struct object *obj)
{
    /* Normal magic ammo are always +0 +0 */
    if (tval_is_ammo(obj) && of_has(obj->flags, OF_AMMO_MAGIC)) return false;

    /* Mage weapons and dark swords are always +0 +0 */
    if (tval_is_mstaff(obj) || tval_is_dark_sword(obj)) return false;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj)) return true;

    /* Need a to-dam */
    if (!obj->to_d) return false;

    /* Base items with variable to-dam */
    if (randcalc_varies(obj->kind->to_d)) return true;

    /* Ego items with variable to-dam */
    if (obj->ego && randcalc_varies(obj->ego->to_d)) return true;

    return false;
}


static s16b todam_min(const struct object *obj)
{
    int base;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj))
    {
        if (obj->ego) return obj->ego->min_to_d;
        return 0;
    }

    /* Base items with variable to-dam */
    base = randcalc(obj->kind->to_d, z_info->max_depth - 1, MINIMISE);
    if (randcalc_varies(obj->kind->to_d)) return base;

    /* Ego items with variable to-dam */
    return base + randcalc(obj->ego->to_d, z_info->max_depth - 1, MINIMISE);
}


static s16b todam_max(const struct object *obj)
{
    int base;

    /* Missiles and weapons */
    if (tval_is_enchantable_weapon(obj)) return 255;

    /* Base items with variable to-dam */
    base = randcalc(obj->kind->to_d, z_info->max_depth - 1, MAXIMISE);
    if (randcalc_varies(obj->kind->to_d)) return base;

    /* Ego items with variable to-dam */
    return base + randcalc(obj->ego->to_d, z_info->max_depth - 1, MAXIMISE);
}


static bool toac_varies(const struct object *obj)
{
    /* Armor parts */
    if (tval_is_armor(obj)) return true;

    /* Need a to-ac */
    if (!obj->to_a) return false;

    /* Base items with variable to-ac */
    if (randcalc_varies(obj->kind->to_a)) return true;

    /* Ego items with variable to-ac */
    if (obj->ego && randcalc_varies(obj->ego->to_a)) return true;

    return false;
}


static s16b toac_min(const struct object *obj)
{
    int base;

    /* Armor parts */
    if (tval_is_armor(obj))
    {
        if (obj->ego) return obj->ego->min_to_a;
        return 0;
    }

    /* Base items with variable to-ac */
    base = randcalc(obj->kind->to_a, z_info->max_depth - 1, MINIMISE);
    if (randcalc_varies(obj->kind->to_a)) return base;

    /* Ego items with variable to-ac */
    return base + randcalc(obj->ego->to_a, z_info->max_depth - 1, MINIMISE);
}


static s16b toac_max(const struct object *obj)
{
    int base;

    /* Armor parts */
    if (tval_is_armor(obj)) return 255;

    /* Base items with variable to-ac */
    base = randcalc(obj->kind->to_a, z_info->max_depth - 1, MAXIMISE);
    if (randcalc_varies(obj->kind->to_a)) return base;

    /* Ego items with variable to-ac */
    return base + randcalc(obj->ego->to_a, z_info->max_depth - 1, MAXIMISE);
}


static int mod_first(struct object *obj)
{
    int i;

    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (obj->modifiers[i]) return i;
    }

    return -1;
}


static int mod_previous(struct object *obj, int mod)
{
    int i = mod - 1;

    while (i >= 0)
    {
        if (obj->modifiers[i]) return i;
        i--;
    }

    return -1;
}


static int mod_next(struct object *obj, int mod)
{
    int i = mod + 1;

    while (i < OBJ_MOD_MAX)
    {
        if (obj->modifiers[i]) return i;
        i++;
    }

    return -1;
}


static void pval_add(struct object *obj, int incr)
{
    int value;
    bool ok = false;

    /* Need a flagless pval */
    if (!obj->pval) return;

    /* Add value */
    value = obj->pval + incr;

    /* Analyze */
    if (tval_is_chest(obj))
        ok = ((value >= 1) && (value <= 59));
    else if (tval_can_have_charges(obj))
    {
        ok = ((value >= randcalc(obj->kind->charge, z_info->max_depth - 1, MINIMISE)) &&
            (value <= randcalc(obj->kind->charge, z_info->max_depth - 1, MAXIMISE)));
    }
    else if (tval_can_have_nourishment(obj) || tval_is_fuel(obj))
    {
        ok = ((value >= randcalc(obj->kind->pval, z_info->max_depth - 1, MINIMISE)) &&
            (value <= randcalc(obj->kind->pval, z_info->max_depth - 1, MAXIMISE)));
    }

    /* Enforce min/max bounds */
    if (ok) obj->pval = value;
}


static void mod_add(struct object *obj, int mod, int incr)
{
    int value;
    bool mod_varies = false;
    s16b mod_min, mod_max;

    /* Need a modifier */
    if (mod == -1) return;

    /* Make a "test" copy */
    value = obj->modifiers[mod];

    /* Hack -- check monster race for rings of polymorphing */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
    {
        struct monster_race *race;

        /* Skip uniques and monsters that can't be generated */
        do
        {
            /* Add value */
            value += incr;
            if ((value <= 0) || (value >= z_info->r_max)) break;

            race = &r_info[value];
        }
        while (!race->name || monster_is_unique(race) ||
            (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
            (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters));

        /* Enforce min/max bounds */
        if ((value > 0) && (value < z_info->r_max))
            obj->modifiers[mod] = value;

        return;
    }

    /* Base items with variable modifier */
    mod_varies = randcalc_varies(obj->kind->modifiers[mod]);
    if (mod_varies)
    {
        mod_min = randcalc(obj->kind->modifiers[mod], z_info->max_depth - 1, MINIMISE);
        if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Speed")))
            mod_max = 255;
        else
            mod_max = randcalc(obj->kind->modifiers[mod], z_info->max_depth - 1, MAXIMISE);
    }

    /* Ego items with variable modifier */
    if (obj->ego && !mod_varies)
    {
        mod_varies = randcalc_varies(obj->ego->modifiers[mod]);
        if (mod_varies)
        {
            mod_min = randcalc(obj->ego->modifiers[mod], z_info->max_depth - 1, MINIMISE);
            mod_max = randcalc(obj->ego->modifiers[mod], z_info->max_depth - 1, MAXIMISE);

            /* Add possible fixed modifier from base item */
            mod_min += randcalc(obj->kind->modifiers[mod], z_info->max_depth - 1, MINIMISE);
            mod_max += randcalc(obj->kind->modifiers[mod], z_info->max_depth - 1, MAXIMISE);
        }
    }

    /* Item must have a variable modifier */
    if (mod_varies)
    {
        /* Add value */
        value += incr;

        /* Enforce min/max bounds */
        if ((value >= mod_min) && (value <= mod_max))
            obj->modifiers[mod] = value;
    }

    /* Apply minima */
    if (obj->ego && obj->modifiers[mod] && (obj->ego->min_modifiers[mod] != NO_MINIMUM) &&
        (obj->modifiers[mod] < obj->ego->min_modifiers[mod]))
    {
        obj->modifiers[mod] = obj->ego->min_modifiers[mod];
    }
}


static const char *id_status(struct player *p, struct object *obj)
{
    if (object_is_known(p, obj)) return "{id}";
    return "{unid}";
}


/*
 * Shadow function of apply_magic().
 */
static void apply_base_magic(struct object *obj)
{
    bool great = false;
    int level = z_info->max_depth - 1;

    /* Normal magic ammo are always +0 +0 */
    if (tval_is_ammo(obj) && of_has(obj->flags, OF_AMMO_MAGIC)) return;

    /* Mage weapons and dark swords are always +0 +0 */
    if (tval_is_mstaff(obj) || tval_is_dark_sword(obj)) return;

    /* Roll for "great" */
    if (obj->ego) great = true;

    /* Apply magic */
    if (tval_is_enchantable_weapon(obj))
    {
        if (great)
        {
            obj->to_h += randint1(5) + m_bonus(5, level);
            obj->to_h += m_bonus(10, level);
            obj->to_d += randint1(5) + m_bonus(5, level);
            obj->to_d += m_bonus(10, level);
        }
    }
    else if (tval_is_armor(obj))
    {
        if (great)
        {
            obj->to_a += randint1(5) + m_bonus(5, level);
            obj->to_a += m_bonus(10, level);
        }

        /* Bad */
        if (obj->to_a < 0)
        {
            size_t i;

            /* Hack -- reverse base bonus */
            for (i = 0; i < OBJ_MOD_MAX; i++)
            {
                if (obj->modifiers[i] > 0) obj->modifiers[i] = 0 - obj->modifiers[i];
            }
        }
    }
    else if (tval_is_chest(obj))
    {
        /* Hack -- skip ruined chests */
        if (obj->kind->level > 0)
        {
            /* Hack -- pick a "difficulty" */
            obj->pval = randint1(obj->kind->level);

            /* Never exceed "difficulty" of 55 to 59 */
            if (obj->pval > 55)
                obj->pval = 55 + randint0(5);
        }
    }
}


static struct artifact *item_art_fuzzy(const struct object *obj, char *name)
{
    char match[NORMAL_WID];
    char *str;
    int i;
    char *t;
    struct object_kind *kind;

    /* Get the kind */
    t = strtok(name, "|");
    kind = item_kind_fuzzy(t);
    if (!kind) return NULL;

    /* Get the name */
    t = strtok(NULL, "|");

    /* Lowercase our search string */
    for (str = t; *str; str++) *str = tolower((unsigned char)*str);

    /* For each artifact */
    for (i = 0; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];

        if (!art->name) continue;
        if (art->created) continue;
        if (kind != lookup_kind(art->tval, art->sval)) continue;

        /* Clean up its name */
        clean_name(match, art->name);

        /* If cleaned name matches our search string, return it */
        if (strstr(match, t)) return art;
    }

    return NULL;
}


/*
 * Generate something
 */
static void master_generate(struct player *p, char *parms)
{
    static struct object *obj = NULL;
    static int power;
    static int resist;
    static int mod;
    struct chunk *c = chunk_get(&p->wpos);

    switch (parms[0])
    {
        /* Generate an item */
        case 'i':
        {
            /* Parse command */
            switch (parms[1])
            {
                /* Clear */
                case 'r':
                {
                    if (obj)
                        object_delete(&obj);
                    else
                        obj = object_new();
                    power = FLAG_END;
                    resist = -1;
                    mod = -1;

                    break;
                }

                /* Item */
                case 'k':
                {
                    struct object_kind *kind = NULL;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'n':
                        {
                            kind = item_kind_fuzzy(&parms[3]);
                            if (kind) kind = item_kind(kind, GE_IDX);
                            break;
                        }
                        case '+':
                        {
                            if (obj->kind) kind = item_kind(obj->kind, GE_NEXT);
                            break;
                        }
                        case '-':
                        {
                            if (obj->kind) kind = item_kind(obj->kind, GE_PREV);
                            break;
                        }
                    }

                    /* Obtain an item */
                    if (kind && (kind != obj->kind))
                    {
                        power = FLAG_END;
                        resist = -1;

                        /* Prepare the object */
                        object_prep(p, obj, kind, z_info->max_depth - 1, RANDOMISE);
                        apply_base_magic(obj);

                        /* Set extra powers */
                        init_powers(obj, &power, &resist);
                        mod = mod_first(obj);
                    }

                    break;
                }

                /* Ego */
                case 'e':
                {
                    struct ego_item *ego = NULL;

                    /* Object must be defined */
                    if (!obj->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'n':
                        {
                            ego = item_ego_fuzzy(obj, &parms[3]);
                            if (ego) ego = item_ego(obj, ego, GE_IDX);
                            break;
                        }
                        case '+':
                        {
                            if (obj->ego) ego = item_ego(obj, obj->ego, GE_NEXT);
                            break;
                        }
                        case '-':
                        {
                            if (obj->ego) ego = item_ego(obj, obj->ego, GE_PREV);
                            break;
                        }
                    }

                    /* Obtain an ego */
                    if (ego && (ego != obj->ego))
                    {
                        struct object_kind *kind = obj->kind;

                        power = FLAG_END;
                        resist = -1;

                        /* Prepare the object */
                        object_prep(p, obj, kind, z_info->max_depth - 1, RANDOMISE);
                        obj->ego = ego;
                        apply_base_magic(obj);

                        /* Set extra powers */
                        init_powers(obj, &power, &resist);
                        ego_apply_magic(obj, z_info->max_depth - 1);
                        mod = mod_first(obj);
                    }

                    break;
                }

                /* Decrement value */
                case 'M':
                {
                    /* Object must be defined */
                    if (!obj->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'h':
                        {
                            /* Item must have a variable to-hit value */
                            if (!tohit_varies(obj)) break;

                            /* Enforce min bound */
                            if (obj->to_h == tohit_min(obj)) break;

                            obj->to_h--; break;
                        }
                        case 'd':
                        {
                            /* Item must have a variable to-dam value */
                            if (!todam_varies(obj)) break;

                            /* Enforce min bound */
                            if (obj->to_d == todam_min(obj)) break;

                            obj->to_d--; break;
                        }
                        case 'a':
                        {
                            /* Item must have a variable to-ac value */
                            if (!toac_varies(obj)) break;

                            /* Enforce min bound */
                            if (obj->to_a == toac_min(obj)) break;

                            obj->to_a--; break;
                        }
                        case 'p':
                        {
                            pval_add(obj, -1);
                            break;
                        }
                        case 'n':
                        {
                            mod = mod_previous(obj, mod);
                            break;
                        }
                        case 'v':
                        {
                            mod_add(obj, mod, -1);
                            break;
                        }
                        case 'x':
                        {
                            dec_power(obj, &power);
                            break;
                        }
                        case 'y':
                        {
                            dec_resist(obj, &resist);
                            break;
                        }
                        case 'm':
                        {
                            /* Set weapon/missile extra dice */
                            if ((tval_is_melee_weapon(obj) || tval_is_ammo(obj)) &&
                                (obj->dd > obj->kind->dd))
                            {
                                obj->dd--;
                            }
                            break;
                        }
                        case 'i':
                        {
                            /* Set unidentified */
                            if (obj->known) object_free(obj->known);
                            object_set_base_known(p, obj);
                            break;
                        }
                    }

                    break;
                }

                /* Increment value */
                case 'I':
                {
                    /* Object must be defined */
                    if (!obj->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'h':
                        {
                            /* Item must have a variable to-hit value */
                            if (!tohit_varies(obj)) break;

                            /* Enforce max bound */
                            if (obj->to_h == tohit_max(obj)) break;

                            obj->to_h++; break;
                        }
                        case 'd':
                        {
                            /* Item must have a variable to-dam value */
                            if (!todam_varies(obj)) break;

                            /* Enforce max bound */
                            if (obj->to_d == todam_max(obj)) break;

                            obj->to_d++; break;
                        }
                        case 'a':
                        {
                            /* Item must have a variable to-ac value */
                            if (!toac_varies(obj)) break;

                            /* Enforce max bound */
                            if (obj->to_a == toac_max(obj)) break;

                            obj->to_a++; break;
                        }
                        case 'p':
                        {
                            pval_add(obj, 1);
                            break;
                        }
                        case 'n':
                        {
                            mod = mod_next(obj, mod);
                            break;
                        }
                        case 'v':
                        {
                            mod_add(obj, mod, 1);
                            break;
                        }
                        case 'x':
                        {
                            inc_power(obj, &power, &resist);
                            break;
                        }
                        case 'y':
                        {
                            inc_resist(obj, &power, &resist);
                            break;
                        }
                        case 'm':
                        {
                            /* Set weapon/missile extra dice */
                            if ((tval_is_melee_weapon(obj) || tval_is_ammo(obj)) &&
                                (obj->dd < 255))
                            {
                                obj->dd++;
                            }
                            break;
                        }
                        case 'i':
                        {
                            /* Set known */
                            object_notice_everything(p, obj);
                            break;
                        }
                    }

                    break;
                }

                /* Generate */
                case 'd':
                {
                    struct object *drop;

                    /* Object must be defined */
                    if (!obj->kind) break;

                    obj->number = parms[2];

                    /* Set origin */
                    set_origin(obj, ORIGIN_CHEAT, p->wpos.depth, NULL);

                    /* Bypass auto-ignore */
                    obj->ignore_protect = 1;

                    /* Initialize some stuff */
                    do_fixed_powers(obj, power, resist);

                    /* Generate */
                    drop = object_new();
                    object_copy(drop, obj);
                    if (object_has_standard_to_h(drop)) drop->known->to_h = 1;
                    if (object_flavor_is_aware(p, drop)) object_id_set_aware(drop);
                    drop_near(p, c, &drop, 0, p->py, p->px, true, DROP_FADE);

                    /* Reinitialize some stuff */
                    undo_fixed_powers(obj, power, resist);

                    break;
                }
            }

            if (!obj) break;

            /* Display result */
            if (obj->kind)
            {
                char o_name[NORMAL_WID];
                char buf[NORMAL_WID];

                /* Describe item */
                object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

                /* Obtain power descriptions */
                get_power_descs(power, resist, buf, sizeof(buf));

                if (obj->ego)
                {
                    Send_special_line(p, 20, 20, 19, COLOUR_WHITE,
                        format("%d. %s %s", obj->kind->kidx, o_name, id_status(p, obj)));
                    Send_special_line(p, 20, 20, 20, COLOUR_WHITE,
                        format("%d. %s [%s]", obj->ego->eidx, obj->ego->name, buf));
                }
                else
                {
                    Send_special_line(p, 20, 20, 19, COLOUR_WHITE,
                        format("%d. %s [%s] %s", obj->kind->kidx, o_name, buf,
                        id_status(p, obj)));
                    Send_special_line(p, 20, 20, 20, COLOUR_WHITE, " [No Ego]");
                }
            }
            else
            {
                Send_special_line(p, 20, 20, 19, COLOUR_WHITE, " [No Item]");
                Send_special_line(p, 20, 20, 20, COLOUR_WHITE, " [No Ego]");
            }

            break;
        }

        /* Generate a vault */
        case 'v':
        {
            struct vault *v = NULL;

            switch (parms[1])
            {
                case 'n':
                {
                    /* Name prefixed by '#': exact vault name */
                    if (parms[2] == '#')
                        v = get_vault_byname(&parms[3]);
                    else
                        v = get_vault_byfuzzyname(&parms[2]);
                }
            }

            if (!v || !v->wid) msg(p, "Vault not found.");

            /* Forbidden outside of the dungeon */
            else if (p->wpos.depth == 0) msg(p, "You cannot generate a vault here");

            /* Not on quest levels */
            else if (is_quest(p->wpos.depth)) msg(p, "You cannot generate a vault here");

            /* Build a vault with the DM at the top left corner */
            else if (!build_vault(p, c, p->py + v->hgt / 2, p->px + v->wid / 2, v, false))
                msg(p, "Vault cannot be generated at this location.");

            break;
        }

        /* Generate a random artifact */
        case 'r':
        {
            /* Parse command */
            switch (parms[1])
            {
                /* Generate a random artifact from scratch */
                case 'n':
                    create_randart(p, c);
                    break;

                /* Reroll a random artifact */
                case 'r':
                {
                    reroll_randart(p, c);
                    break;
                }
            }

            break;
        }

        /* Generate a true artifact */
        case 'a':
        {
            struct artifact *art = item_art_fuzzy(obj, &parms[1]);
            struct object_kind *kind;
            struct object *obj;

            /* Paranoia */
            if (!art) break;

            /* Acquire the "kind" index */
            kind = lookup_kind(art->tval, art->sval);
            if (!kind) break;

            /* Get object */
            obj = object_new();

            /* Create the artifact */
            object_prep(p, obj, kind, art->alloc_min, RANDOMISE);

            /* Save the artifact type */
            obj->artifact = art;

            /* Extract the fields */
            copy_artifact_data(obj, art);

            /* Mark that the artifact has been created. */
            obj->artifact->created++;

            /* Mark the artifact as "generated" */
            set_artifact_info(p, obj, ARTS_GENERATED);

            /* Mark as cheat, and where created */
            set_origin(obj, ORIGIN_CHEAT, p->wpos.depth, NULL);

            if (object_has_standard_to_h(obj)) obj->known->to_h = 1;
            if (object_flavor_is_aware(p, obj)) object_id_set_aware(obj);

            /* Drop the object from heaven */
            drop_near(p, c, &obj, 0, p->py, p->px, true, DROP_FADE);

            break;
        }
    }
}


static const char *dm_flags_str[17] =
{
    "Dungeon Master",
    "Presence Hidden",
    "Can Change Self",
    "Can Change Others",
    "Access to Build Menu",
    "Access to Level Menu",
    "Access to Summon Menu",
    "Access to Generate Menu",
    "Monster Friend",
    "*Invulnerable*",
    "Ghostly Hands",
    "Ghostly Body",
    "Never Disturbed",
    "See Level",
    "See Monsters",
    "See Players",
    "Landlord"
};


static bool check_permissions(struct player *p, struct player *who, bool silent)
{
    u32b access_flag = DM_CAN_MUTATE_SELF;

    /* No player */
    if (!who)
    {
        if (!silent) Send_special_line(p, 17, 17, 15, COLOUR_WHITE, " Error: no player");
        return false;
    }

    /* Check permissions */
    if (who != p) access_flag = DM_CAN_ASSIGN;
    if (!(p->dm_flags & access_flag))
    {
        if (!silent)
        {
            Send_special_line(p, 17, 17, 15, COLOUR_WHITE,
                ((who == p)? " Error: can't change self": " Error: can't change others"));
        }
        return false;
    }

    return true;
}


static void master_player(struct player *p, char *parms)
{
    static struct player *dm_ptr = NULL;
    static int dm_player_off = 0;
    char *desc;

    /* Assign self */
    if (parms[0] == ' ')
    {
        dm_ptr = p;
        return;
    }

    /* Assign other (by name) */
    if (parms[0] != '>')
    {
        int i;

        dm_ptr = NULL;
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *q = player_get(i);

            if (!my_strnicmp(q->name, parms, strlen(parms)))
            {
                dm_ptr = q;
                break;
            }
        }
        return;
    }

    /* Analyze the command */
    switch (parms[1])
    {
        /* Prev Sel */
        case 'p':
        {
            if (dm_player_off > 0) dm_player_off--;
            return;
        }

        /* Next Sel */
        case 'n':
        {
            if (dm_player_off < 16) dm_player_off++;
            return;
        }

        /* Change */
        case 'x':
        {
            u32b new_flag = (1L << dm_player_off);

            if (!check_permissions(p, dm_ptr, true)) return;

            /* Toggle 1 of DM flags */
            if (!(dm_ptr->dm_flags & new_flag))
                dm_ptr->dm_flags |= new_flag;
            else
                dm_ptr->dm_flags &= ~new_flag;

            /* Hack -- for *invulnerable* set "invuln" */
            if (new_flag == DM_INVULNERABLE)
            {
                if (dm_ptr->dm_flags & new_flag)
                {
                    dm_ptr->timed[TMD_INVULN] = -1;
                    dm_ptr->upkeep->update |= (PU_BONUS | PU_MONSTERS);
                    dm_ptr->upkeep->redraw |= (PR_MAP | PR_STATUS);
                    handle_stuff(dm_ptr);
                }
                else
                    player_clear_timed(dm_ptr, TMD_INVULN, true);
            }

            return;
        }

        /* Toggle Ghost */
        case 'g':
        {
            if (!check_permissions(p, dm_ptr, true)) return;

            set_ghost_flag(dm_ptr, (dm_ptr->ghost? 0: 1), true);
            dm_ptr->upkeep->redraw |= (PR_BASIC | PR_SPELL);
            dm_ptr->upkeep->update |= (PU_BONUS);
            return;
        }

        /* Toggle Wizard */
        case 'w':
        {
            if (!check_permissions(p, dm_ptr, true)) return;

            dm_ptr->noscore = (dm_ptr->noscore? 0: 1);
            return;
        }
    }

    /* Check permissions */
    if (!check_permissions(p, dm_ptr, false))
    {
        dm_ptr = NULL;
        return;
    }

    /* Cannot toggle ghost for Dragon players or in fruit bat mode */
    if (dm_ptr->ghost && (player_has(dm_ptr, PF_DRAGON) || OPT(dm_ptr, birth_fruit_bat)))
    {
        Send_special_line(p, 17, 17, 15, COLOUR_WHITE,
            " Error: can't toggle ghost for no-ghost players");
        set_ghost_flag(dm_ptr, 0, true);
        dm_ptr->upkeep->redraw |= (PR_BASIC | PR_SPELL);
        dm_ptr->upkeep->update |= (PU_BONUS);
        dm_ptr = NULL;
        return;
    }

    /* Display */
    desc = format("  Player: %s%s%s", dm_ptr->name, (dm_ptr->ghost? ", ghost": ""),
        (dm_ptr->noscore? ", wizard": ""));
    Send_special_line(p, 17, 17, 15, COLOUR_WHITE, desc);
    desc = format("    Flag: %s -- %s", dm_flags_str[dm_player_off],
        ((dm_ptr->dm_flags & (1L << dm_player_off))? "Yes": "No"));
    Send_special_line(p, 17, 17, 16, COLOUR_WHITE, desc);
}


static void master_visuals(struct player *p)
{
    int type;
    char chars[] = "*|/-\\";

    /* Let the player scroll through the info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Scan the PROJ_XXX types */
    for (type = 0; type < PROJ_MAX; type++)
    {
        size_t i;
        const char *proj_name = proj_idx_to_name(type);

        /* Hack -- special coloring */
        p->info[type][0].a = COLOUR_SPECIAL;
        for (i = 1; i < NORMAL_WID; i++) p->info[type][i].a = COLOUR_WHITE;

        /* Erase */
        for (i = 0; i < NORMAL_WID / 2; i++)
        {
            p->info[type][i * 2].c = COLOUR_WHITE;
            p->info[type][i * 2 + 1].c = ' ';
        }

        /* Name */
        for (i = 0; i < strlen(proj_name); i++) p->info[type][i * 2 + 1].c = proj_name[i];
        p->info[type][strlen(proj_name) * 2 + 1].c = ':';

        for (i = 0; i < BOLT_MAX; i++)
        {
            bool use_gfx = (p->use_graphics && !p->tile_distorted);
            byte a = (use_gfx? p->proj_attr[type][i]: proj_color(type));
            char c = (use_gfx? p->proj_char[type][i]: chars[i]);

            p->info[type][i * 2 + 32].c = a;
            p->info[type][i * 2 + 33].c = c;
        }
    }

    /* Last line */
    p->last_info_line = PROJ_MAX - 1;

    /* Let the client know to expect some info */
    Send_special_other(p, "PROJ_XXX types", 1, true);
}


static void master_order(struct player *p, char *parms)
{
    static int dm_order = 0;
    char *desc;
    char o_desc[NORMAL_WID];

    /* Paranoia */
    if (parms[0] != '>') return;

    /* Analyze the command */
    switch (parms[1])
    {
        /* Prev Sel */
        case 'p':
        {
            if (dm_order > 0) dm_order--;
            return;
        }

        /* Next Sel */
        case 'n':
        {
            if (dm_order < STORE_ORDERS - 1) dm_order++;
            return;
        }

        /* Cancel order */
        case 'c':
        {
            if (streq(store_orders[dm_order], "{ordered}")) store_cancel_order(dm_order);
            my_strcpy(store_orders[dm_order], "", sizeof(store_orders[0]));
            return;
        }
    }

    /* Display */
    if (STRZERO(store_orders[dm_order]))
        my_strcpy(o_desc, "(available)", sizeof(o_desc));
    else if (streq(store_orders[dm_order], "{ordered}"))
        store_get_order(dm_order, o_desc, sizeof(o_desc));
    else
        my_strcpy(o_desc, store_orders[dm_order], sizeof(o_desc));
    desc = format("  Order #%d: %s", 1 + dm_order, o_desc);
    Send_special_line(p, 17, 17, 15, COLOUR_WHITE, desc);
}


/*
 * This is a nice utility function; it determines if a (NULL-terminated)
 * string consists of only digits (starting with a non-zero digit).
 */
static s16b get_idx_from_name(char *s)
{
    char *endptr = NULL;
    long l = strtol(s, &endptr, 10);

    return ((*endptr == '\0')? (s16b)l: 0);
}


static void master_debug(struct player *p, char *parms)
{
    /* Analyze the command */
    switch (parms[0])
    {
        /* Perform an effect */
        case 'E':
        {
            char *t;
            int index = EF_MAX;
            char dice[NORMAL_WID];
            int p1 = 0, p2 = 0, p3 = 0;
            bool ident = false;
            static bool used = true;
            struct source who_body;
            struct source *who = &who_body;

            /* Get the name */
            t = strtok(&parms[1], "|");
            if (t)
            {
                /* See if an effect index was entered */
                index = get_idx_from_name(t);

                /* If not, find the effect with that name */
                if ((index <= EF_NONE) || (index >= EF_MAX))
                    index = effect_lookup(t);
            }
            if (index == EF_MAX)
            {
                msg(p, "No effect found.");
                break;
            }

            /* Get the dice */
            t = strtok(NULL, "|");
            if (t) my_strcpy(dice, t, sizeof(dice));
            else my_strcpy(dice, "0", sizeof(dice));

            /* Get the parameters */
            t = strtok(NULL, "|");
            if (t)
            {
                p1 = get_idx_from_name(t);

                /* See if an effect parameter was entered */
                if (p1 == 0) p1 = effect_param(index, t);
                if (p1 == -1) p1 = 0;
            }
            t = strtok(NULL, "|");
            if (t) p2 = get_idx_from_name(t);
            t = strtok(NULL, "|");
            if (t) p3 = get_idx_from_name(t);

            if (used) current_clear(p);
            source_player(who, get_player_index(get_connection(p->conn)), p);
            used = effect_simple(index, who, dice, p1, p2, p3, &ident);
            if (ident) msg(p, "Identified!");
            break;
        }

        /* Create a trap */
        case 'T':
        {
            struct chunk *c = chunk_get(&p->wpos);
            struct trap_kind *trap;

            if (!random_level(&p->wpos) || !square_isfloor(c, p->py, p->px))
            {
                msg(p, "You can't place a trap there!");
                break;
            }

            trap = lookup_trap(&parms[1]);
            if (trap) place_trap(c, p->py, p->px, trap->tidx, 0);
            else msg(p, "Trap not found.");

            break;
        }
    }
}


void do_cmd_master(struct player *p, s16b command, char *buf)
{
    switch (command)
    {
        case MASTER_LEVEL:
        {
            if (!(p->dm_flags & DM_LEVEL_CONTROL)) return;
            master_level(p, buf);
            break;
        }

        case MASTER_BUILD:
        {
            if (!(p->dm_flags & DM_CAN_BUILD)) return;
            master_build(p, buf);
            break;
        }

        case MASTER_SUMMON:
        {
            if (!(p->dm_flags & DM_CAN_SUMMON)) return;
            master_summon(p, buf);
            break;
        }

        case MASTER_GENERATE:
        {
            if (!(p->dm_flags & DM_CAN_GENERATE)) return;
            master_generate(p, buf);
            break;
        }

        case MASTER_PLAYER:
        {
            master_player(p, buf);
            break;
        }

        case MASTER_VISUALS:
        {
            if (!is_dm_p(p)) return;
            master_visuals(p);
            break;
        }

        case MASTER_ORDER:
        {
            if (!is_dm_p(p)) return;
            master_order(p, buf);
            break;
        }

        case MASTER_DEBUG:
        {
            if (!is_dm_p(p)) return;
            master_debug(p, buf);
            break;
        }
    }
}


/* Find arena by those coordinates */
int pick_arena(struct worldpos *wpos, int y, int x)
{
    int i;

    for (i = 0; i < num_arenas; i++)
    {
        if (!COORDS_EQUAL(&arenas[i].wpos, wpos)) continue;
        if (x < arenas[i].x_1 || x > arenas[i].x_2) continue;
        if (y < arenas[i].y_1 || y > arenas[i].y_2) continue;

        /* Found */
        return i;
    }

    /* Failure */
    return -1;
}


/* Find number of players on arena "a" */
static int count_arena_opponents(int a)
{
    int i, count = 0;

    /* Count players in this arena */
    for (i = 1; i <= NumPlayers; i++)
    {
        if (player_get(i)->arena_num == a)
        {
            /* Found someone */
            count++;
        }
    }

    return count;
}


/*
 * Access Arena (Player touches its wall in py,px)
 */
void access_arena(struct player *p, int py, int px)
{
    struct player *q;
    int tmp_count, a;

    /* No-PvP mode: can't access arenas */
    if (cfg_pvp_hostility == PVP_DISABLED)
    {
        msg(p, "There is a wall blocking your way.");
        return;
    }

    /* Ghosts can't access arenas */
    if (p->ghost)
    {
        msg(p, "There is a wall blocking your way.");
        return;
    }

    /* Player is in a party */
    if (p->party)
    {
        msg(p, "Please leave your party before entering this arena.");
        return;
    }

    /* Player tries to leave the arena */
    if (p->arena_num != -1)
    {
        /* Count players in this arena */
        tmp_count = count_arena_opponents(p->arena_num);

        /* If he is alone, leave */
        if (tmp_count == 1)
        {
            struct source who_body;
            struct source *who = &who_body;

            msg(p, "You leave the arena.");
            p->arena_num = -1;
            source_player(who, get_player_index(get_connection(p->conn)), p);
            effect_simple(EF_TELEPORT, who, "1", 0, 0, 0, NULL);
        }
        else
            msg(p, "There is a wall blocking your way.");
    }

    /* Player tries to enter the arena */
    else
    {
        a = pick_arena(&p->wpos, py, px);

        /* Count players in this arena */
        tmp_count = count_arena_opponents(a);

        /* If arena is not 'full' -- enter it */
        if (tmp_count < 2)
        {
            struct source who_body;
            struct source *who = &who_body;

            msg(p, "You enter an ancient fighting pit.");
            source_player(who, get_player_index(get_connection(p->conn)), p);
            effect_simple(EF_TELEPORT_TO, who, "0",
                arenas[a].y_1 + 1 + randint1(arenas[a].y_2 - arenas[a].y_1 - 2),
                arenas[a].x_1 + 1 + randint1(arenas[a].x_2 - arenas[a].x_1 - 2), 1, NULL);
            p->arena_num = a;

            /* Both players are ready! */
            if (tmp_count == 1)
            {
                q = pick_arena_opponent(p, a);

                /* Declare hostility */
                pvp_check(q, p, PVP_ADD, true, 0x00);
                pvp_check(p, q, PVP_ADD, true, 0x00);
            }
        }
        else
            msg(p, "Arena is currently occupied.");
    }
}


static void format_line(struct player *p, struct source *who, char *line, char *buf, int len)
{
    const char *poss = player_poss(p);
    const char *refl;
    const char *poss_t, *refl_t;
    char target_name[NORMAL_WID], ch;

    switch (p->psex)
    {
        case SEX_FEMALE: refl = "her"; break;
        case SEX_MALE: refl = "him"; break;
        default: refl = "it"; break;
    }

    /* Player */
    if (who->player)
    {
        poss_t = player_poss(who->player);

        switch (who->player->psex)
        {
            case SEX_FEMALE: refl_t = "her"; break;
            case SEX_MALE: refl_t = "him"; break;
            default: refl_t = "it"; break;
        }

        my_strcpy(target_name, who->player->name, sizeof(target_name));
    }

    /* Monster */
    else
    {
        if (rf_has(who->monster->race->flags, RF_FEMALE))
        {
            poss_t = "her";
            refl_t = "her";
        }
        else if (rf_has(who->monster->race->flags, RF_MALE))
        {
            poss_t = "his";
            refl_t = "him";
        }
        else
        {
            poss_t = "its";
            refl_t = "it";
        }

        monster_desc(p, target_name, sizeof(target_name), who->monster, MDESC_DEFAULT);
    }

    /* Initialize */
    buf[0] = '\0';

    /* Parse the line */
    while (*line)
    {
        /* Get current, advance */
        ch = *line++;

        /* Format specifier */
        if (ch == '$')
        {
            /* Get current, advance */
            ch = *line++;

            /* Player name */
            if (ch == 'n') my_strcat(buf, p->name, len);

            /* Possessive, genderized (player) */
            else if (ch == 's') my_strcat(buf, poss, len);

            /* Objective (or Reflexive), genderized (player) */
            else if (ch == 'm') my_strcat(buf, refl, len);

            /* Target name */
            else if (ch == 'N') my_strcat(buf, target_name, len);

            /* Possessive, genderized (target) */
            else if (ch == 'S') my_strcat(buf, poss_t, len);

            /* Objective (or Reflexive), genderized (target) */
            else if (ch == 'M') my_strcat(buf, refl_t, len);
        }

        /* Plain text */
        else my_strcat(buf, format("%c", ch), len);
    }
}


/* Perform a 'social' action */
void do_cmd_social(struct player *p, const char *buf, int dir)
{
    int k;
    bool found = false;
    struct social *s;
    char *t;
    char line[NORMAL_WID];
    char text[MSG_LEN];
    struct source self_body;
    struct source *self = &self_body;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);

    /* Ghosts don't socialize */
    if (p->ghost) return;

    /* Scan the socials */
    for (k = 0; k < z_info->soc_max; k++)
    {
        s = &soc_info[k];

        if (streq(s->name, buf))
        {
            found = true;
            break;
        }
    }

    /* Not a valid action */
    if (!found) return;

    /* Try to find a target */
    if (s->target && dir && VALID_DIR(dir))
    {
        int flg = PROJECT_STOP;
        int ty, tx;
        int path_n = 0;
        struct loc path_g[512];

        /* Only fire in direction 5 if we have a target */
        if ((dir == 5) && !target_okay(p)) return;

        /* Use the given direction */
        tx = p->px + 99 * ddx[dir];
        ty = p->py + 99 * ddy[dir];

        /* Hack -- use an actual "target" */
        if (dir == 5)
        {
            flg &= ~PROJECT_STOP;
            target_get(p, &tx, &ty);
        }

        /* Calculate the path */
        path_n = project_path(NULL, path_g, (s->max_dist? s->max_dist: z_info->max_range), c, p->py,
            p->px, ty, tx, flg);
        if (!path_n) return;

        /* Get target grid */
        tx = path_g[path_n - 1].x;
        ty = path_g[path_n - 1].y;

        /* Target found at a reasonable distance */
        if (c->squares[ty][tx].mon)
        {
            square_actor(c, ty, tx, who);

            /* Get social */
            my_strcpy(text, s->text, sizeof(text));

            /* Display player line */
            strtok(text, "#");
            strtok(NULL, "#");
            t = strtok(NULL, "#");
            format_line(p, who, t, line, sizeof(line));
            msg_print(p, line, MSG_SOCIAL);

            /* Display nearby players line */
            t = strtok(NULL, "#");
            format_line(p, who, t, line, sizeof(line));
            if (who->player)
                msg_print_complex_near(p, p, MSG_SOCIAL, line);
            else
                msg_print_complex_near(p, who->player, MSG_SOCIAL, line);

            /* Display target line */
            if (who->player)
            {
                t = strtok(NULL, "#");
                format_line(p, who, t, line, sizeof(line));
                msg_print(who->player, line, MSG_SOCIAL);
            }

            /* Done */
            return;
        }
    }

    /* Get social */
    my_strcpy(text, s->text, sizeof(text));

    /* Display player line */
    t = strtok(text, "#");
    msg_print(p, t, MSG_SOCIAL);

    /* Display nearby players line */
    t = strtok(NULL, "#");
    source_player(self, 0, p);
    format_line(p, self, t, line, sizeof(line));
    msg_print_complex_near(p, p, MSG_SOCIAL, line);
}               


/*
 * Display player information
 */
void describe_player(struct player *p, struct player *q)
{
    const char *pm, *pm2, *poss, *poss2;
    int i;

    switch (q->psex)
    {
        case SEX_FEMALE: pm = "She"; pm2 = "she"; poss = "Her"; poss2 = "her"; break;
        case SEX_MALE: pm = "He"; pm2 = "he"; poss = "His"; poss2 = "his"; break;
        default: pm = "It"; pm2 = "it"; poss = "Its"; poss2 = "its"; break;
    }

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Describe name */
    text_out(p, q->name);
    text_out(p, ", the ");
    text_out(p, "%s", q->clazz->title[(q->lev - 1) / 5]);
    text_out(p, ".\n\n");
    text_out(p, "%s is %s %s ", pm,
        (is_a_vowel(tolower(q->race->name[0]))? "an": "a"), q->race->name);
    text_out_c(p, q->clazz->attr, q->clazz->name);
    text_out(p, ".\n\n");

    /* The player's "history" in 3rd person */
    for (i = 0; i < N_HIST_LINES; i++)
    {
        char buf[N_HIST_WRAP], tmp[N_HIST_WRAP];

        /* Replace "your" with "his/her/its" */
        strrepall(buf, sizeof(buf), q->history[i], "Your", poss);
        strrepall(tmp, sizeof(tmp), buf, "your", poss2);

        /* Replace "you" with "he/she/it" */
        strrepall(buf, sizeof(buf), tmp, "You", pm);
        strrepall(tmp, sizeof(tmp), buf, "you", pm2);

        /* Replace "are/have" with "is/has" */
        strrepall(buf, sizeof(buf), tmp, "are", "is");
        strrepall(tmp, sizeof(tmp), buf, "have", "has");

        text_out(p, "%s\n", tmp);
    }

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}               


/*
 * Display trap information
 */
void describe_trap(struct player *p, struct trap *trap)
{
    char name[NORMAL_WID];

    /* Get name */
    my_strcpy(name, trap->kind->desc, sizeof(name));

    /* Capitalize trap name for header */
    my_strcap(name);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Describe */
    if (trap->kind->text)
        text_out(p, trap->kind->text);
    else
        text_out(p, "Nothing is known about this trap.");
    text_out(p, "\n\n");

    /* Restore height and width of current dungeon level */
    text_out_done(p);

    /* Notify player */
    notify_player_popup(p, name, NTERM_WIN_SPECIAL, NTERM_POP);
}               


/*
 * Display feature information
 */
void describe_feat(struct player *p, struct feature *feat)
{
    char name[NORMAL_WID];

    /* Get name */
    my_strcpy(name, feat->name, sizeof(name));

    /* Capitalize feature name for header */
    my_strcap(name);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Describe */
    if (feat->desc)
        text_out(p, feat->desc);
    else
        text_out(p, "Nothing is known about this feature.");
    text_out(p, "\n\n");

    /* Restore height and width of current dungeon level */
    text_out_done(p);

    /* Notify player */
    notify_player_popup(p, name, NTERM_WIN_SPECIAL, NTERM_POP);
}


/*
 * Animations
 */


/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x, y) to (nx, ny); if the distance is not
 * "one", we (may) return "*".
 */
void bolt_pict(struct player *p, int y, int x, int ny, int nx, int typ, byte *a, char *c)
{
    int motion;

    /* Convert co-ordinates into motion */
    if ((ny == y) && (nx == x))
        motion = BOLT_NO_MOTION;
    else if (nx == x)
        motion = ((ny < y)? BOLT_0: BOLT_180);
    else if ((ny - y) == (x - nx))
        motion = ((ny < y)? BOLT_45: BOLT_225);
    else if (ny == y)
        motion = ((nx > x)? BOLT_90: BOLT_270);
    else if ((ny - y) == (nx - x))
        motion = ((nx > x)? BOLT_135: BOLT_315);
    else
        motion = BOLT_NO_MOTION;

    /* Decide on output char */
    if (!p->use_graphics)
    {
        /* ASCII is simple */
        char chars[] = "*|/-\\|/-\\";

        *c = chars[motion];
        *a = proj_color(typ);
    }
    else
    {
        *a = p->proj_attr[typ][motion];
        *c = p->proj_char[typ][motion];
    }
}


/*
 * Draw an explosion
 */
void display_explosion(struct chunk *cv, struct explosion *data, const bool *drawing, bool arc)
{
    bool new_radius = false;
    bool drawn[MAX_PLAYERS];
    int i, y, x, j;
    int proj_type = data->proj_type;
    int num_grids = data->num_grids;
    const int *distance_to_grid = data->distance_to_grid;
    const struct loc *blast_grid = data->blast_grid;

    /* Assume the player has seen no blast grids */
    for (i = 0; i < MAX_PLAYERS; ++i) drawn[i] = false;

    /* Draw the blast from inside out */
    for (i = 0; i < num_grids; i++)
    {
        /* Hack -- don't draw over breather */
        if (arc && !distance_to_grid[i]) continue;

        /* Extract the location */
        y = blast_grid[i].y;
        x = blast_grid[i].x;

        /* Do visuals for all players that can see the blast */
        for (j = 1; j <= NumPlayers; j++)
        {
            struct player *p = player_get(j);

            /* Skip irrelevant players */
            if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
            if (p->timed[TMD_BLIND]) continue;
            if (!panel_contains(p, y, x)) continue;
            if (p->did_visuals) continue;

            /* Only do visuals if the player can see the blast */
            if (square_isview(p, y, x))
            {
                byte a;
                char c;

                drawn[j] = true;

                /* Obtain the explosion pict */
                bolt_pict(p, y, x, y, x, proj_type, &a, &c);

                /* Just display the pict, ignoring what was under it */
                draw_path_grid(p, y, x, a, c);
            }
        }

        /* Check for new radius, taking care not to overrun array */
        if (i == num_grids - 1)
            new_radius = true;
        else if (distance_to_grid[i + 1] > distance_to_grid[i])
            new_radius = true;

        /* We have all the grids at the current radius, so draw it */
        if (new_radius)
        {
            /* Flush all the grids at this radius for all players that can see the blast */
            for (j = 1; j <= NumPlayers; j++)
            {
                struct player *p = player_get(j);

                /* Skip irrelevant players */
                if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
                if (p->timed[TMD_BLIND]) continue;

                /* Delay to show this radius appearing */
                if (drawing[j] || drawn[j])
                    Send_flush(p, true, true);
                else
                    Send_flush(p, true, false);
            }

            new_radius = false;
        }
    }

    /* Erase and flush for all players that can "see" the blast */
    for (j = 1; j <= NumPlayers; j++)
    {
        struct player *p = player_get(j);

        /* Skip irrelevant players */
        if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
        if (p->timed[TMD_BLIND]) continue;

        /* Erase and flush */
        if (drawn[j])
        {
            /* Erase the explosion drawn above */
            for (i = 0; i < num_grids; i++)
            {
                /* Hack -- don't draw over breather */
                if (arc && !distance_to_grid[i]) continue;

                /* Extract the location */
                y = blast_grid[i].y;
                x = blast_grid[i].x;

                /* Skip irrelevant players */
                if (!panel_contains(p, y, x)) continue;

                /* Erase visible, valid grids */
                if (square_isview(p, y, x))
                    square_light_spot_aux(p, cv, y, x);
            }

            /* Flush the explosion */
            Send_flush(p, true, false);
        }
    }

    /* Hack -- count how many blasts we have seen */
    for (j = 1; j <= NumPlayers; j++)
    {
        struct player *p = player_get(j);

        /* Skip irrelevant players */
        if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
        if (p->timed[TMD_BLIND]) continue;

        /* Add one to the count */
        if (drawn[j]) p->did_visuals = true;
    }
}


/*
 * Draw a moving spell effect (bolt or beam)
 */
void display_bolt(struct chunk *cv, struct bolt *data, bool *drawing)
{
    int proj_type = data->proj_type;
    bool beam = data->beam;
    int oy = data->oy;
    int ox = data->ox;
    int y = data->y;
    int x = data->x;
    int j;

    /* Do visuals for all players that can "see" the bolt */
    for (j = 1; j <= NumPlayers; j++)
    {
        struct player *p = player_get(j);

        /* Skip irrelevant players */
        if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
        if (p->timed[TMD_BLIND]) continue;
        if (!panel_contains(p, y, x)) continue;
        if (p->did_visuals) continue;

        /* Only do visuals if the player can "see" the bolt */
        if (square_isview(p, y, x))
        {
            byte a;
            char c;

            /* Obtain the bolt pict */
            bolt_pict(p, oy, ox, y, x, proj_type, &a, &c);

            /* Visual effects */
            flush_path_grid(p, cv, y, x, a, c);

            /* Display "beam" grids */
            if (beam)
            {
                /* Obtain the explosion pict */
                bolt_pict(p, y, x, y, x, proj_type, &a, &c);

                /* Visual effects */
                draw_path_grid(p, y, x, a, c);
            }

            /* Activate delay */
            drawing[j] = true;
        }

        /* Delay for consistency */
        else if (drawing[j])
            Send_flush(p, false, true);
    }
}


/*
 * Draw a moving missile
 */
void display_missile(struct chunk *cv, struct missile *data)
{
    byte mattr = data->mattr;
    char mchar = data->mchar;
    int y = data->y;
    int x = data->x;
    int k;

    /* Display the missile for each player */
    for (k = 1; k <= NumPlayers; k++)
    {
        struct player *p = player_get(k);

        /* Skip irrelevant players */
        if (!COORDS_EQUAL(&p->wpos, &cv->wpos)) continue;
        if (!panel_contains(p, y, x)) continue;

        /* Only do visuals if the player can "see" the missile */
        if (square_isseen(p, y, x))
        {
            /* Draw, Highlight, Fresh, Pause, Erase */
            flush_path_grid(p, cv, y, x, mattr, mchar);
        }
        else
        {
            /* Delay anyway for consistency */
            Send_flush(p, false, true);
        }
    }
}


/*
 * Output a short message to the top line of the screen. Save message in the history log.
 */
static void display_message_aux(struct player *p, int type, const char *msg)
{
    bool log = true;
    bool add = false;
    bool dup = false;
    char multiplier[12];
    s16b ptr;

    /* Excludes all channels but #public from the log file */
    if (type > MSG_CHAT) log = false;

    if (msg)
    {
        /* We don't need to log *everything* */
        if (strchr("[", *msg)) log = false;

        /*
         * Log messages for each player, so we can dump last messages
         * in server-side character dumps
         */
        if (p && log)
        {
            add = true;

            /* Ensure we know where the last message is */
            ptr = p->msg_hist_ptr - 1;
            if (ptr < 0) ptr = MAX_MSG_HIST - 1;

            /* If this message is already in the buffer, count it as a dupe */
            if (!strcmp(p->msg_log[ptr], msg))
            {
                p->msg_hist_dupe++;

                /* And don't add another copy to the buffer */
                add = false;
                dup = true;
            }

            /* This message is the end of a series of dupes */
            else if (p->msg_hist_dupe > 0)
            {
                /* Add the dupe counter to the end of the last message */
                strnfmt(multiplier, sizeof(multiplier), " (x%d)", p->msg_hist_dupe + 1);
                my_strcat(p->msg_log[ptr], multiplier, sizeof(p->msg_log[0]));
                p->msg_hist_dupe = 0;
            }

            if (add)
            {
                /* Standard, unique (for the moment) message */
                my_strcpy(p->msg_log[p->msg_hist_ptr], msg, NORMAL_WID - 1);
                p->msg_hist_ptr++;
            }

            /* Maintain a circular buffer */
            if (p->msg_hist_ptr == MAX_MSG_HIST)
                p->msg_hist_ptr = 0;

            /* Log the message */
            plog_fmt("%s: %s", p->name, msg);
        }
        else if (log)
        {
            /* Log the message */
            plog(msg);
        }
    }

    /* Hack -- repeated message of the same type */
    if (dup && (type == p->msg_last_type))
    {
        /* Send a SPACE character instead */
        Send_message(p, " ", type);
        return;
    }

    /* Paranoia */
    if (!p) return;

    /* Last type sent */
    p->msg_last_type = type;

    /* Ahh, the beautiful simplicity of it... */
    Send_message(p, msg, type);
}


/*
 * Output a message to the top line of the screen.
 *
 * Break long messages into multiple pieces (40-72 chars).
 *
 * Allow multiple short messages to "share" the top line.
 *
 * Prompt the user to make sure he has a chance to read them.
 *
 * These messages are memorized for later reference (see above).
 *
 * We could do a "Term_fresh()" to provide "flicker" if needed.
 *
 * We must be very careful about using the "msg()" functions without
 * explicitly calling the special "msg(NULL)" function, since this may
 * result in the loss of information if the screen is cleared, or if anything
 * is displayed on the top line.
 *
 * Hack -- note that "msg(NULL)" will clear the top line even if no
 * messages are pending.
 */
void display_message(struct player *p, struct message *data)
{
    int n;
    char *t;
    char buf[MSG_LEN];
    int w;
    int type;
    const char *msg;

    if (!data) return;

    type = data->type;
    msg = data->msg;

    /* No message */
    if (!msg)
    {
        display_message_aux(p, type, msg);
        return;
    }

    /* Obtain the size */
    w = NORMAL_WID;

    /* Message Length */
    n = (msg? strlen(msg): 0);

    /* Copy it */
    my_strcpy(buf, msg, sizeof(buf));

    /* Analyze the buffer */
    t = buf;

    /* Split message */
    while (n > w - 1)
    {
        char oops;
        int check, split;

        /* Default split */
        split = w - 8;

        /* Find the rightmost split point */
        for (check = (w / 2); check < w - 8; check++)
        {
            if (t[check] == ' ') split = check;
        }

        /* Save the split character */
        oops = t[split];

        /* Split the message */
        t[split] = '\0';

        /* Display part of the message */
        display_message_aux(p, type, t);

        /* Restore the split character */
        t[split] = oops;

        /* Insert a space */
        t[--split] = ' ';

        /* Prepare to recurse on the rest of "buf" */
        t += split; n -= split;
    }

    /* Display the tail of the message */
    display_message_aux(p, type, t);
}
