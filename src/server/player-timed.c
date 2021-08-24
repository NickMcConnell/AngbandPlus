/*
 * File: player-timed.c
 * Purpose: Timed effects handling
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2007 Andi Sidwell
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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


int PY_FOOD_MAX;
int PY_FOOD_FULL;
int PY_FOOD_HUNGRY;
int PY_FOOD_WEAK;
int PY_FOOD_FAINT;
int PY_FOOD_STARVE;


/*
 * Parsing functions for player_timed.txt
 */


struct timed_effect_data timed_effects[] =
{
    #define TMD(a, b, c) {#a, b, c, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0},
    #include "../common/list-player-timed.h"
    #undef TMD
    {"MAX", 0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0}
};


int timed_name_to_idx(const char *name)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(timed_effects); i++)
    {
        if (!my_stricmp(name, timed_effects[i].name)) return i;
    }

    return -1;
}


/*
 * List of timed effect names
 */
static const char *list_timed_effect_names[] =
{
    #define TMD(a, b, c) #a,
    #include "../common/list-player-timed.h"
    #undef TMD
    "MAX"
};


static enum parser_error parse_player_timed_name(struct parser *p)
{
    const char *name = parser_getstr(p, "name");
    int index;
    struct timed_effect_data *t;

    if (grab_name("timed effect", name, list_timed_effect_names,
        N_ELEMENTS(list_timed_effect_names), &index))
    {
        return PARSE_ERROR_INVALID_SPELL_NAME;
    }

    t = &timed_effects[index];
    t->index = index;
    parser_setpriv(p, t);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_desc(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->desc = string_append(t->desc, parser_getstr(p, "desc"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_grade(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);
    struct timed_grade *current, *l;
    const char *color = parser_getsym(p, "color");
    int attr = 0;

    my_assert(t);
    if (strlen(color) > 1) attr = color_text_to_attr(color);
    else attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;

    /* Make a zero grade structure if there isn't one */
    current = t->grade;
    if (!current)
    {
        t->grade = mem_zalloc(sizeof(struct timed_grade));
        current = t->grade;
    }

    /* Move to the highest grade so far */
    while (current->next) current = current->next;

    /* Add the new one */
    l = mem_zalloc(sizeof(*l));
    current->next = l;
    l->grade = current->grade + 1;
    l->color = attr;
    l->max = parser_getint(p, "max");
    l->name = string_make(parser_getsym(p, "name"));
    l->up_msg = string_make(parser_getsym(p, "up_msg"));
    if (parser_hasval(p, "down_msg"))
        l->down_msg  = string_make(parser_getsym(p, "down_msg"));

    /* Set food constants and deal with percentages */
    if (streq(t->name, "FOOD"))
    {
        l->max *= z_info->food_value;
        if (streq(l->name, "Starving")) PY_FOOD_STARVE = l->max;
        else if (streq(l->name, "Faint")) PY_FOOD_FAINT = l->max;
        else if (streq(l->name, "Weak")) PY_FOOD_WEAK = l->max;
        else if (streq(l->name, "Hungry")) PY_FOOD_HUNGRY = l->max;
        else if (streq(l->name, "Fed")) PY_FOOD_FULL = l->max;
        else if (streq(l->name, "Full")) PY_FOOD_MAX = l->max;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_end_message(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->on_end = string_append(t->on_end, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_increase_message(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->on_increase = string_append(t->on_increase, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_decrease_message(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->on_decrease = string_append(t->on_decrease, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_nbegin_message(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->near_begin = string_append(t->near_begin, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_nend_message(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->near_end = string_append(t->near_end, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_player_timed_message_type(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);

    my_assert(t);
    t->msgt = message_lookup_by_name(parser_getsym(p, "type"));
    return ((t->msgt < 0)? PARSE_ERROR_INVALID_MESSAGE: PARSE_ERROR_NONE);
}


static enum parser_error parse_player_timed_fail(struct parser *p)
{
    struct timed_effect_data *t = parser_priv(p);
    const char *name;

    my_assert(t);
    t->fail_code = parser_getuint(p, "code");

    name = parser_getstr(p, "flag");
    if (t->fail_code == TMD_FAIL_FLAG_OBJECT)
    {
        int flag = lookup_flag(list_obj_flag_names, name);

        if (flag == FLAG_END) return PARSE_ERROR_INVALID_FLAG;
        t->fail = flag;
    }
    else if ((t->fail_code == TMD_FAIL_FLAG_RESIST) || (t->fail_code == TMD_FAIL_FLAG_VULN))
    {
        size_t i = 0;

        while (list_element_names[i] && !streq(list_element_names[i], name)) i++;

        if (i == ELEM_MAX) return PARSE_ERROR_INVALID_FLAG;
        t->fail = i;
    }
    else
        return PARSE_ERROR_INVALID_FLAG;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_player_timed(void)
{
    struct parser *p = parser_new();
    parser_setpriv(p, NULL);

    parser_reg(p, "name str name", parse_player_timed_name);
    parser_reg(p, "desc str desc", parse_player_timed_desc);
    parser_reg(p, "grade sym color int max sym name sym up_msg ?sym down_msg",
        parse_player_timed_grade);
    parser_reg(p, "on-end str text", parse_player_timed_end_message);
    parser_reg(p, "on-increase str text", parse_player_timed_increase_message);
    parser_reg(p, "on-decrease str text", parse_player_timed_decrease_message);
    parser_reg(p, "near-begin str text", parse_player_timed_nbegin_message);
    parser_reg(p, "near-end str text", parse_player_timed_nend_message);
    parser_reg(p, "msgt sym type", parse_player_timed_message_type);
    parser_reg(p, "fail uint code str flag", parse_player_timed_fail);
    return p;
}


static errr run_parse_player_timed(struct parser *p)
{
    return parse_file_quit_not_found(p, "player_timed");
}


static errr finish_parse_player_timed(struct parser *p)
{
    parser_destroy(p);
    return 0;
}


static void cleanup_player_timed(void)
{
    size_t i;

    for (i = 0; i < TMD_MAX; i++)
    {
        struct timed_effect_data *effect = &timed_effects[i];
        struct timed_grade *grade = effect->grade;

        while (grade)
        {
            struct timed_grade *next = grade->next;

            string_free(grade->name);
            string_free(grade->up_msg);
            string_free(grade->down_msg);
            mem_free(grade);
            grade = next;
        }

        string_free(effect->desc);
        string_free(effect->on_end);
        string_free(effect->on_increase);
        string_free(effect->on_decrease);
        string_free(effect->near_begin);
        string_free(effect->near_end);

        effect->desc = NULL;
        effect->on_end = NULL;
        effect->on_increase = NULL;
        effect->on_decrease = NULL;
        effect->near_begin = NULL;
        effect->near_end = NULL;
    }
}


struct file_parser player_timed_parser =
{
    "player timed effects",
    init_parse_player_timed,
    run_parse_player_timed,
    finish_parse_player_timed,
    cleanup_player_timed
};


/*
 * Utilities for more complex or anomolous effects
 */


/*
 * Swap stats at random to temporarily scramble the player's stats.
 */
static void player_scramble_stats(struct player *p)
{
    int max1, cur1, max2, cur2, i, j, swap;

    /* Fisher-Yates shuffling algorithm. */
    for (i = STAT_MAX - 1; i > 0; --i)
    {
        j = randint0(i);

        max1 = p->stat_max[i];
        cur1 = p->stat_cur[i];
        max2 = p->stat_max[j];
        cur2 = p->stat_cur[j];

        p->stat_max[i] = max2;
        p->stat_cur[i] = cur2;
        p->stat_max[j] = max1;
        p->stat_cur[j] = cur1;

        /* Record what we did */
        swap = p->stat_map[i];
        p->stat_map[i] = p->stat_map[j];
        p->stat_map[j] = swap;
    }
}


/*
 * Undo scrambled stats when effect runs out.
 */
static void player_fix_scramble(struct player *p)
{
    int i;

    /* Figure out what stats should be */
    int new_cur[STAT_MAX];
    int new_max[STAT_MAX];

    for (i = 0; i < STAT_MAX; ++i)
    {
        new_cur[p->stat_map[i]] = p->stat_cur[i];
        new_max[p->stat_map[i]] = p->stat_max[i];
    }

    /* Apply new stats and clear the stat_map */
    for (i = 0; i < STAT_MAX; ++i)
    {
        p->stat_cur[i] = new_cur[i];
        p->stat_max[i] = new_max[i];
        p->stat_map[i] = i;
    }
}


/*
 * Set "p->timed[TMD_BOWBRAND]", notice observable changes
 */
static bool set_bow_brand(struct player *p, int v)
{
    bool notice = false;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_BOWBRAND])
        {
            switch (p->brand.type)
            {
                case PROJ_ELEC:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow deep blue.");
                        msg(p, "Your missiles glow deep blue!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with lightning.");
                        msg(p, "Your missiles are covered with lightning!");
                    }
                    break;
                }
                case PROJ_COLD:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow bright white.");
                        msg(p, "Your missiles glow bright white!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with frost.");
                        msg(p, "Your missiles are covered with frost!");
                    }
                    break;
                }
                case PROJ_FIRE:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow deep red.");
                        msg(p, "Your missiles glow deep red!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with fire.");
                        msg(p, "Your missiles are covered with fire!");
                    }
                    break;
                }
                case PROJ_ACID:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow pitch black.");
                        msg(p, "Your missiles glow pitch black!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with acid.");
                        msg(p, "Your missiles are covered with acid!");
                    }
                    break;
                }
                case PROJ_MON_CONF:
                    msg_misc(p, "'s missiles glow many colors.");
                    msg(p, "Your missiles glow many colors!");
                    break;
                case PROJ_POIS:
                    msg_misc(p, "'s missiles are covered with venom.");
                    msg(p, "Your missiles are covered with venom!");
                    break;
                case PROJ_ARROW:
                    msg_misc(p, "'s missiles sharpen.");
                    msg(p, "Your missiles sharpen!");
                    break;
                case PROJ_SHARD:
                    msg_misc(p, "'s missiles become explosive.");
                    msg(p, "Your missiles become explosive!");
                    break;
                case PROJ_MISSILE:
                    msg_misc(p, "'s missiles glow with power.");
                    msg(p, "Your missiles glow with power!");
                    break;
                case PROJ_SOUND:
                    msg_misc(p, "'s missiles vibrate in a strange way.");
                    msg(p, "Your missiles vibrate in a strange way!");
                    break;
            }
            notice = true;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BOWBRAND])
        {
            msg_misc(p, "'s missiles seem normal again.");
            msg(p, "Your missiles seem normal again.");
            notice = true;
        }
    }

    /* Use the value */
    p->timed[TMD_BOWBRAND] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Disturb */
    disturb(p);

    /* Redraw the "brand" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set a timed event permanently.
 */
static bool player_set_timed_perma(struct player *p, int idx)
{
    struct timed_effect_data *effect;
    bool notify = true;
    struct timed_grade *current_grade;

    /* No change */
    if (p->timed[idx] == -1) return false;

    /* Don't mention some effects */
    if ((idx == TMD_OPP_ACID) && player_is_immune(p, ELEM_ACID)) notify = false;
    if ((idx == TMD_OPP_ELEC) && player_is_immune(p, ELEM_ELEC)) notify = false;
    if ((idx == TMD_OPP_FIRE) && player_is_immune(p, ELEM_FIRE)) notify = false;
    if ((idx == TMD_OPP_COLD) && player_is_immune(p, ELEM_COLD)) notify = false;

    /* Find the effect */
    effect = &timed_effects[idx];
    current_grade = effect->grade;
    while (current_grade->next) current_grade = current_grade->next;

    /* Turning on, always mention */
    if (p->timed[idx] == 0)
    {
        msg_misc(p, effect->near_begin);
        msgt(p, effect->msgt, current_grade->up_msg);
        notify = true;
    }

    /* Use the value */
    p->timed[idx] = -1;

    /* Nothing to notice */
    if (!notify) return false;

    /* Disturb */
    disturb(p);

    /* Reveal hidden players */
    if (p->k_idx) aware_player(p, p);

    /* Update the visuals, as appropriate. */
    p->upkeep->update |= effect->flag_update;
    p->upkeep->redraw |= effect->flag_redraw;

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_ADRENALINE]", notice observable changes
 * Note the interaction with biofeedback
 */
static bool set_adrenaline(struct player *p, int v)
{
    int old_aux = 0, new_aux = 0;
    bool notice = false;

    /* Limit duration (100 turns / 20 turns at 5th stage) */
    if (v > 100)
    {
        v = 100;

        /* Too much adrenaline causes damage */
        msg(p, "Your body can't handle that much adrenaline!");
        take_hit(p, damroll(2, v), "adrenaline poisoning", false,
            "had a heart attack due to too much adrenaline");
        notice = true;
    }

    /* Different stages */
    if (p->timed[TMD_ADRENALINE] > 0) old_aux = 1 + (p->timed[TMD_ADRENALINE] - 1) / 20;
    if (v > 0) new_aux = 1 + (v - 1) / 20;

    /* Increase stage */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Berserk strength effect */
            case 1:
            {
                msg_misc(p, "'s veins are flooded with adrenaline.");
                msg(p, "Adrenaline surges through your veins!");
                hp_player(p, 30);
                player_clear_timed(p, TMD_AFRAID, true);
                player_set_timed_perma(p, TMD_BOLD);
                player_set_timed_perma(p, TMD_SHERO);

                /* Adrenaline doesn't work well when biofeedback is activated */
                if (p->timed[TMD_BIOFEEDBACK])
                {
                    player_clear_timed(p, TMD_BIOFEEDBACK, true);
                    take_hit(p, damroll(2, v), "adrenaline poisoning", false,
                        "had a heart attack due to too much adrenaline");
                }
                break;
            }

            /* Increase Str/Dex/Con */
            case 2:
            {
                msg_misc(p, "feels powerful.");
                msg(p, "You feel powerful!");
                break;
            }

            /* Increase Str/Dex/Con + increase to-dam */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands glow red.");
                msg(p, "Your hands glow red!");
                break;
            }

            /* Increase Str/Dex/Con + increase attack speed */
            case 4:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands tingle.");
                msg(p, "Your hands tingle!");
                break;
            }

            /* Increase Str/Dex/Con + haste effect */
            case 5:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_FAST);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Decrease stage */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, "'s veins are drained of adrenaline.");
                msg(p, "The adrenaline drains out of your veins.");
                player_clear_timed(p, TMD_BOLD, true);
                player_clear_timed(p, TMD_SHERO, true);
                break;
            }

            /* Decrease Str/Dex/Con */
            case 1:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                break;
            }

            /* Decrease Str/Dex/Con + decrease to-dam */
            case 2:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                msg_misc(p, "'s hands stop glowing.");
                msg(p, "Your hands stop glowing.");
                break;
            }

            /* Decrease Str/Dex/Con + decrease attack speed */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands ache.");
                msg(p, "Your hands ache.");
                break;
            }

            /* Decrease Str/Dex/Con + lose haste effect */
            case 4:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_FAST, true);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_ADRENALINE] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p);

    /* Redraw the "adrenaline" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_BIOFEEDBACK]", notice observable changes
 * Note the interaction with adrenaline
 */
static bool set_biofeedback(struct player *p, int v)
{
    bool notice = false;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_BIOFEEDBACK])
        {
            msg_misc(p, "'s pulse slows down.");
            msg(p, "Your pulse slows down!");

            /* Biofeedback doesn't work well when adrenaline is activated */
            if (p->timed[TMD_ADRENALINE])
            {
                player_clear_timed(p, TMD_ADRENALINE, true);
                if (one_in_(8))
                {
                    msg(p, "You feel weak and tired!");
                    player_inc_timed(p, TMD_SLOW, randint0(4) + 4, true, false);
                    if (one_in_(5))
                        player_inc_timed(p, TMD_PARALYZED, randint0(4) + 4, true, false);
                    if (one_in_(3)) player_inc_timed(p, TMD_STUN, randint1(30), true, false);
                }
            }
            notice = true;
        }

        /* Biofeedback can't reach high values */
        if (v > 35 + p->lev)
        {
            msg(p, "You speed up your pulse to avoid fainting!");
            v = 35 + p->lev;
            notice = true;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BIOFEEDBACK])
        {
            msg_misc(p, "'s pulse speeds up.");
            msg(p, "You lose control of your blood flow.");
            notice = true;
        }

    }

    /* Use the value */
    p->timed[TMD_BIOFEEDBACK] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p);

    /* Redraw the "biofeedback" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_HARMONY]", notice observable changes
 */
static bool set_harmony(struct player *p, int v)
{
    int old_aux = 0, new_aux = 0;
    bool notice = false;

    /* Limit duration (100 turns / 20 turns at 5th stage) */
    if (v > 100) v = 100;

    /* Different stages */
    if (p->timed[TMD_HARMONY] > 0) old_aux = 1 + (p->timed[TMD_HARMONY] - 1) / 20;
    if (v > 0) new_aux = 1 + (v - 1) / 20;

    /* Increase stage */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Bless effect */
            case 1:
            {
                msg_misc(p, "feels attuned to nature.");
                msg(p, "You feel attuned to nature!");
                player_set_timed_perma(p, TMD_BLESSED);
                break;
            }

            /* Increase Str/Dex/Con */
            case 2:
            {
                msg_misc(p, "feels powerful.");
                msg(p, "You feel powerful!");
                break;
            }

            /* Increase Str/Dex/Con + shield effect */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_SHIELD);
                break;
            }

            /* Increase Str/Dex/Con + resistance effect */
            case 4:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_OPP_ACID);
                player_set_timed_perma(p, TMD_OPP_ELEC);
                player_set_timed_perma(p, TMD_OPP_FIRE);
                player_set_timed_perma(p, TMD_OPP_COLD);
                player_set_timed_perma(p, TMD_OPP_POIS);
                break;
            }

            /* Increase Str/Dex/Con + haste effect */
            case 5:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_FAST);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Decrease stage */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, "feels less attuned to nature.");
                msg(p, "You feel less attuned to nature.");
                player_clear_timed(p, TMD_BLESSED, true);
                break;
            }

            /* Decrease Str/Dex/Con */
            case 1:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                break;
            }

            /* Decrease Str/Dex/Con + lose shield effect */
            case 2:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_SHIELD, true);
                break;
            }

            /* Decrease Str/Dex/Con + lose resistance effect */
            case 3:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_OPP_ACID, true);
                player_clear_timed(p, TMD_OPP_ELEC, true);
                player_clear_timed(p, TMD_OPP_FIRE, true);
                player_clear_timed(p, TMD_OPP_COLD, true);
                player_clear_timed(p, TMD_OPP_POIS, true);
                break;
            }

            /* Decrease Str/Dex/Con + lose haste effect */
            case 4:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_FAST, true);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_HARMONY] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p);

    /* Redraw the status */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Return true if the player timed effect matches the given string
 */
bool player_timed_grade_eq(struct player *p, int idx, char *match)
{
    if (p->timed[idx])
    {
        struct timed_grade *grade = timed_effects[idx].grade;

        while (p->timed[idx] > grade->max) grade = grade->next;
        if (grade->name && streq(grade->name, match)) return true;
    }

    return false;
}


/*
 * Setting, increasing, decreasing and clearing timed effects
 */


/*
 * Hack: check if player has permanent protection from confusion (see calc_bonuses)
 */
static bool player_of_has_prot_conf(struct player *p)
{
    bitflag collect_f[OF_SIZE], f[OF_SIZE];
    int i;

    player_flags(p, collect_f);

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (!obj) continue;
        object_flags(obj, f);
        of_union(collect_f, f);
    }

    return of_has(collect_f, OF_PROT_CONF);
}


/*
 * Set a timed event.
 */
bool player_set_timed(struct player *p, int idx, int v, bool notify)
{
    struct timed_effect_data *effect;
    struct timed_grade *new_grade, *current_grade;
    struct object *weapon;
    bool result, no_disturb = false;
    int food_meter = 0;

    my_assert(idx >= 0);
    my_assert(idx < TMD_MAX);

    effect = &timed_effects[idx];
    new_grade = effect->grade;
    current_grade = effect->grade;
    weapon = equipped_item_by_slot_name(p, "weapon");

    /* Lower bound */
    v = MAX(v, (idx == TMD_FOOD)? 1: 0);

    /* No change */
    if (p->timed[idx] == v) return false;

    /* Find the grade we will be going to, and the current one */
    while (v > new_grade->max)
    {
        new_grade = new_grade->next;
        if (!new_grade->next) break;
    }
    while (p->timed[idx] > current_grade->max)
    {
        current_grade = current_grade->next;
        if (!current_grade->next) break;
    }

    /* Upper bound */
    v = MIN(v, new_grade->max);

    /* Hack -- call other functions, reveal hidden players if noticed */
    if ((idx == TMD_STUN) && (p->dm_flags & DM_INVULNERABLE))
    {
        /* Hack -- the DM can not be stunned */
        if (p->k_idx) aware_player(p, p);
        return true;
    }
    if ((idx == TMD_CUT) && p->ghost && (v > 0))
    {
        /* Ghosts cannot bleed */
        if (p->k_idx) aware_player(p, p);
        return true;
    }
    if (idx == TMD_BOWBRAND)
    {
        result = set_bow_brand(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    if (idx == TMD_ADRENALINE)
    {
        result = set_adrenaline(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    if (idx == TMD_BIOFEEDBACK)
    {
        result = set_biofeedback(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    if (idx == TMD_HARMONY)
    {
        result = set_harmony(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }

    /* Don't mention some effects */
    if ((idx == TMD_OPP_ACID) && player_is_immune(p, ELEM_ACID)) notify = false;
    if ((idx == TMD_OPP_ELEC) && player_is_immune(p, ELEM_ELEC)) notify = false;
    if ((idx == TMD_OPP_FIRE) && player_is_immune(p, ELEM_FIRE)) notify = false;
    if ((idx == TMD_OPP_COLD) && player_is_immune(p, ELEM_COLD)) notify = false;
    if ((idx == TMD_OPP_CONF) && player_of_has_prot_conf(p)) notify = false;

    /* Always mention going up a grade, otherwise on request */
    if (new_grade->grade > current_grade->grade)
    {
        msg_misc(p, effect->near_begin);
        print_custom_message(p, weapon, new_grade->up_msg, effect->msgt);
        notify = true;
    }
    else if ((new_grade->grade < current_grade->grade) && new_grade->down_msg)
    {
        msg_misc(p, effect->near_begin);
        print_custom_message(p, weapon, new_grade->down_msg, effect->msgt);
        notify = true;

        /* If the player is at full hit points and starving, destroy his connection */
        if ((idx == TMD_FOOD) && (v < PY_FOOD_FAINT) && (p->chp == p->mhp) && OPT(p, disturb_faint))
            p->starving = true;
    }
    else if (notify)
    {
        /* Finishing */
        if (v == 0)
        {
            msg_misc(p, effect->near_end);
            print_custom_message(p, weapon, effect->on_end, MSG_RECOVER);
        }

        /* Decrementing */
        else if ((p->timed[idx] > v) && effect->on_decrease)
            print_custom_message(p, weapon, effect->on_decrease, effect->msgt);

        /* Incrementing */
        else if ((v > p->timed[idx]) && effect->on_increase)
            print_custom_message(p, weapon, effect->on_increase, effect->msgt);
    }

    /* Handle stat swap */
    if (idx == TMD_SCRAMBLE)
    {
        if (p->timed[idx] == 0) player_scramble_stats(p);
        else if (v == 0) player_fix_scramble(p);
    }

    /* Hack -- food meter */
    if (idx == TMD_FOOD) food_meter = p->timed[idx] / 100;

    /* Use the value */
    p->timed[idx] = v;

    /* Hack -- food meter */
    if ((idx == TMD_FOOD) && (food_meter != p->timed[idx] / 100))
    {
        if (!notify) no_disturb = true;
        notify = true;
    }

    /* Sort out the sprint effect */
    if ((idx == TMD_SPRINT) && (v == 0)) player_inc_timed(p, TMD_SLOW, 100, true, false);

    if (notify)
    {
        /* Disturb */
        if (!no_disturb) disturb(p);

        /* Reveal hidden players */
        if (p->k_idx) aware_player(p, p);

        /* Update the visuals, as appropriate. */
        p->upkeep->update |= effect->flag_update;
        p->upkeep->redraw |= effect->flag_redraw;

        /* Handle stuff */
        handle_stuff(p);
    }

    return notify;
}


/*
 * Check whether a timed effect will affect the player
 */
bool player_inc_check(struct player *p, struct monster *mon, int idx, bool lore)
{
    struct timed_effect_data *effect = &timed_effects[idx];

    /* Check that @ can be affected by this effect */
    if (!effect->fail_code) return true;

    /* Determine whether an effect can be prevented by a flag */
    if (effect->fail_code == TMD_FAIL_FLAG_OBJECT)
    {
        /* Effect is inhibited by an object flag */
        if (!lore) equip_learn_flag(p, effect->fail);

        /* If the effect is from a monster action, extra stuff happens */
        if (mon && !lore) update_smart_learn(mon, p, effect->fail, 0, -1);
        if (player_of_has(p, effect->fail))
        {
            if (mon && !lore) msg(p, "You resist the effect!");
            return false;
        }
    }
    else if (effect->fail_code == TMD_FAIL_FLAG_RESIST)
    {
        /* Effect is inhibited by a resist -- always learn */
        if (!lore) equip_learn_element(p, effect->fail);
        if (p->state.el_info[effect->fail].res_level > 0) return false;
    }
    else if (effect->fail_code == TMD_FAIL_FLAG_VULN)
    {
        /* Effect is inhibited by a vulnerability -- don't learn if temporary resistance */
        if (p->state.el_info[effect->fail].res_level < 0)
        {
            if (!lore) equip_learn_element(p, effect->fail);
            return false;
        }
    }

    /* Special case */
    if ((idx == TMD_POISONED) && p->timed[TMD_OPP_POIS]) return false;

    return true;
}


/*
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is true.
 * Check for resistance to the effect if `check` is true.
 */
bool player_inc_timed_aux(struct player *p, struct monster *mon, int idx, int v, bool notify,
    bool check)
{
    my_assert(idx >= 0);
    my_assert(idx < TMD_MAX);

    if (!check || player_inc_check(p, mon, idx, false))
    {
        /* Paralysis should be non-cumulative */
        if ((idx == TMD_PARALYZED) && (p->timed[idx] > 0)) return false;

        /* Hack -- permanent effect */
        if (p->timed[idx] == -1) return false;

        /* Handle polymorphed players */
        if (p->poly_race && (idx == TMD_AMNESIA))
        {
            if (rf_has(p->poly_race->flags, RF_EMPTY_MIND)) v /= 2;
            if (rf_has(p->poly_race->flags, RF_WEIRD_MIND)) v = v * 3 / 4;
        }

        return player_set_timed(p, idx, p->timed[idx] + v, notify);
    }

    return false;
}


/*
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is true.
 * Check for resistance to the effect if `check` is true.
 */
bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check)
{
    return player_inc_timed_aux(p, NULL, idx, v, notify, check);
}


/*
 * Decrease the timed effect `idx` by `v`.  Mention this if `notify` is true.
 */
bool player_dec_timed(struct player *p, int idx, int v, bool notify)
{
    int new_value;

    my_assert(idx >= 0);
    my_assert(idx < TMD_MAX);
    new_value = p->timed[idx] - v;

    if (p->no_disturb_icky && (new_value > 0)) p->no_disturb_icky = false;

    /* Obey `notify` if not finishing; if finishing, always notify */
    if (new_value > 0) return player_set_timed(p, idx, new_value, notify);

    return player_set_timed(p, idx, new_value, true);
}


/*
 * Clear the timed effect `idx`.  Mention this if `notify` is true.
 */
bool player_clear_timed(struct player *p, int idx, bool notify)
{
    my_assert(idx >= 0);
    my_assert(idx < TMD_MAX);

    return player_set_timed(p, idx, 0, notify);
}
