/*
 * File: birth.c
 * Purpose: Character creation
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "../common/tvalsval.h"
#include "files.h"
#include "generate.h"
#include "history.h"
#include "monster/mon-msg.h"
#include "netserver.h"
#include "object/inventory.h"
#include "s-spells.h"
#include "wilderness.h"


/*
 * Maximum amount of starting gold
 */
#define STARTING_GOLD   600


/*
 * Current stats
 */
static s16b stat_use[A_MAX];


/* Basic sort algorithm */
static void sort_stats(s16b* stats, s16b* stat_order)
{
    int i, j;

    /* I use a bubble sort because I'm lazy at the moment */
    for (i = 0; i < A_MAX; i++)
    {
        for (j = 0; j < A_MAX - 1; j++)
        {
            if (stats[j] < stats[j + 1])
            {
                s16b stat, order;

                /* Swap stats */
                stat = stats[j];
                stats[j] = stats[j + 1];
                stats[j + 1] = stat;

                /* Swap stat order */
                if (stat_order)
                {
                    order = stat_order[j];
                    stat_order[j] = stat_order[j + 1];
                    stat_order[j + 1] = order;
                }
            }
        }
    }
}


/* Roll some stats */
static void roll_stats(s16b* stats)
{
    int i, j;
    int dice[18];

    /* Roll and verify some stats */
    while (TRUE)
    {
        /* Roll some dice */
        for (j = i = 0; i < 18; i++)
        {
            /* Roll the dice */
            dice[i] = randint1(3 + i % 3);

            /* Collect the maximum */
            j += dice[i];
        }

        /* Verify totals */
        if ((j > 42) && (j < 54)) break;
    }

    /* Roll the stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Extract 5 + 1d3 + 1d4 + 1d5 */
        j = 5 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

        /* Save that value */
        stats[i] = j;
    }
}


/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(struct player *p, s16b* stat_roll)
{
    int i, bonus;
    bool rand_roller = FALSE;
    s16b stats[A_MAX];

    /* Point-based roller */
    if (stat_roll[A_MAX] == BR_POINTBASED)
    {
        /* Check over the given stats */
        for (i = 0; i < A_MAX; i++)
        {
            /* Check data */
            if ((stat_roll[i] < 8) || (stat_roll[i] > 17))
            {
                /* Incorrect data: use random roller */
                rand_roller = TRUE;
                break;
            }
        }

        /* Point-based roller: use "stat_roll" directly */
        if (!rand_roller)
        {
            /* Stats are given by "stat_roll" directly */
            for (i = 0; i < A_MAX; i++) p->stat_max[i] = stat_roll[i];
        }

        /* Random roller: roll and accept whatever stats we get */
        else
        {
            /* Roll and verify some stats */
            roll_stats(stats);

            /* Accept whatever stats we get */
            for (i = 0; i < A_MAX; i++) p->stat_max[i] = stats[i];
        }
    }

    /* Standard roller */
    else
    {
        s16b stat_order[A_MAX], stat_limit[A_MAX], stat_ok[A_MAX];
        int j;

        /* Clear "stats" array */
        for (i = 0; i < A_MAX; i++) stats[i] = 0;

        /* Stat order is given by "stat_roll" directly */
        for (i = 0; i < A_MAX; i++) stat_order[i] = stat_roll[i];

        /*
         * Ensure a minimum value of 17 for the first stat, 16 for the second
         * stat and 15 for the third stat; other stats have the legal minimum
         * value of 8
         */
        stat_limit[0] = 17;
        stat_limit[1] = 16;
        stat_limit[2] = 15;
        stat_limit[3] = 8;
        stat_limit[4] = 8;
        stat_limit[5] = 8;

        /* Clear "stat_ok" array */
        for (i = 0; i < A_MAX; i++) stat_ok[i] = 0;

        /* Check over the given stat order */
        for (i = 0; i < A_MAX; i++)
        {
            /* Check data */
            if ((stat_order[i] < 0) || (stat_order[i] >= A_MAX))
            {
                /* Incorrect data: use random roller */
                rand_roller = TRUE;
                break;
            }

            /* Increment "stat_ok" */
            stat_ok[stat_order[i]]++;
        }

        /* Check for duplicated or missing entries */
        for (i = 0; i < A_MAX; i++)
        {
            /* Check "stat_ok" flag */
            if (stat_ok[i] != 1)
            {
                /* Incorrect order: use random roller */
                rand_roller = TRUE;
                break;
            }
        }

        /* Roll */
        while (TRUE)
        {
            bool accept = TRUE;

            /* Roll and verify some stats */
            roll_stats(stats);

            /* Random roller: accept whatever stats we get */
            if (rand_roller) break;

            /* Clear "stat_ok" array */
            for (i = 0; i < A_MAX; i++) stat_ok[i] = 0;

            /* Count acceptable stats */
            for (i = 0; i < A_MAX; i++)
            {
                /* Increment count of acceptable stats */
                for (j = 0; j < A_MAX; j++)
                {
                    /* This stat is okay */
                    if (stats[j] >= stat_limit[i]) stat_ok[i]++;
                }
            }

            /* Check acceptable stats */
            for (i = 0; i < A_MAX; i++)
            {
                /* This stat is not okay */
                if (stat_ok[i] <= i)
                {
                    accept = FALSE;
                    break;
                }
            }

            /* Break if "happy" */
            if (accept) break;
        }

        /* Standard roller: sort and put stats in the correct order */
        if (!rand_roller)
        {
            /* Sort the stats */
            sort_stats(stats, NULL);

            /* Put stats in the correct order */
            for (i = 0; i < A_MAX; i++) p->stat_max[stat_order[i]] = stats[i];
        }

        /* Random roller: accept whatever stats we get */
        else
        {
            /* Accept whatever stats we get */
            for (i = 0; i < A_MAX; i++) p->stat_max[i] = stats[i];
        }
    }

    /* Adjust the stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Obtain a "bonus" for "race" and "class" */
        bonus = p->race->r_adj[i] + p->clazz->c_adj[i];

        /* Start fully healed */
        p->stat_cur[i] = p->stat_max[i];

        /* Efficiency -- Apply the racial/class bonuses */
        stat_use[i] = modify_stat_value(p->stat_max[i], bonus);
    }

    /* Save birth stats */
    for (i = 0; i < A_MAX; i++) p->stat_birth[i] = p->stat_max[i];
}


static void roll_hp(struct player *p)
{
    int i, j, min_value, max_value;

    /* Minimum hitpoints at highest level */
    min_value = (PY_MAX_LEVEL * (p->hitdie - 1) * 3) / 8;
    min_value += PY_MAX_LEVEL;

    /* Maximum hitpoints at highest level */
    max_value = (PY_MAX_LEVEL * (p->hitdie - 1) * 5) / 8;
    max_value += PY_MAX_LEVEL;

    /* Roll out the hitpoints */
    while (TRUE)
    {
        /* Roll the hitpoint values */
        for (i = 1; i < PY_MAX_LEVEL; i++)
        {
            j = randint1(p->hitdie);
            p->player_hp[i] = p->player_hp[i - 1] + j;
        }

        /* XXX Could also require acceptable "mid-level" hitpoints */

        /* Require "valid" hitpoints at highest level */
        if (p->player_hp[PY_MAX_LEVEL - 1] < min_value) continue;
        if (p->player_hp[PY_MAX_LEVEL - 1] > max_value) continue;

        /* Acceptable */
        break;
    }
}


/*
 * Calculate the bonuses and hitpoints. Don't send messages to the client.
 */
static void get_bonuses(struct player *p)
{
    /* Calculate the bonuses and hitpoints */
    p->update |= (PU_BONUS | PU_HP);

    /* Update stuff */
    update_stuff(p);

    /* Fully healed */
    p->chp = p->mhp;

    /* Fully rested */
    p->csp = p->msp;
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(struct player *p)
{
    int roll, social_class;
    struct history_entry *entry;
    int i, n;
    char *s, *t, *desc;
    char buf[240];
    struct history_chart *chart;

    /* Clear the previous history strings */
    for (i = 0; i < N_HIST_LINES; i++) p->history[i][0] = '\0';

    /* Clear the history text */
    buf[0] = '\0';

    /* Set pointers */
    t = &buf[0];
    desc = &p->descrip[0];

    /* Initial social class */
    social_class = randint1(4);

    /* Starting place */
    chart = p->race->history;

    /* Process the history */
    while (chart)
    {
        /* Roll for nobility */
        roll = randint1(100);

        /* Get the proper entry in the table */
        for (entry = chart->entries; entry; entry = entry->next)
        {
            if (roll <= entry->roll) break;
        }
        my_assert(entry);

        /* Get the textual history */
        my_strcat(buf, entry->text, sizeof(buf));
        for (s = entry->text; *s; s++)
        {
#define g_strcat(P, T) \
    n = strlen((T)); strncpy((P), (T), n); (P) += n;
#define P_CASE(C, F, T) \
    case (C): \
    { \
        g_strcat(t, (F)) \
        g_strcat(desc, (T)) \
        break; \
    }
#define G_CASE(C, F, TM, TF, TN) \
    case (C): \
    { \
        g_strcat(t, (F)) \
        if (p->psex == SEX_FEMALE) {g_strcat(desc, (TF))} \
        else if (p->psex == SEX_MALE) {g_strcat(desc, (TM))} \
        else {g_strcat(desc, (TN))} \
        break; \
    }
            switch (*s)
            {
                case '$':
                case '~':
                    s++;
                    switch (*s)
                    {
                        G_CASE('u', "You", "He", "She", "It")
                        G_CASE('r', "Your", "His", "Her", "Its")
                        P_CASE('a', "are", "is")
                        P_CASE('h', "have", "has")
                        default: continue;
                    }
                    break;
                default:
                    *t++ = *desc++ = *s;
            }
        }
        *t = *desc = '\0';

        /* Add in the social class */
        social_class += entry->bonus - 50;

        /* Enter the next chart */
        chart = entry->succ;
    }

    /* Verify social class */
    if (social_class > 75) social_class = 75;
    else if (social_class < 1) social_class = 1;

    /* Save the social class */
    p->sc = social_class;

    /* Skip leading spaces */
    for (s = buf; *s == ' '; s++) /* loop */;

    /* Get apparent length */
    n = strlen(s);

    /* Kill trailing spaces */
    while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

    /* Start at first line */
    i = 0;

    /* Collect the history */
    while (TRUE)
    {
        /* Extract remaining length */
        n = strlen(s);

        /* All done */
        if (n < N_HIST_WRAP)
        {
            /* Save one line of history */
            my_strcpy(p->history[i++], s, sizeof(p->history[0]));

            /* All done */
            break;
        }

        /* Find a reasonable break-point */
        for (n = N_HIST_WRAP; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

        /* Save next location */
        t = s + n;

        /* Wipe trailing spaces */
        while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

        /* Save one line of history */
        my_strcpy(p->history[i++], s, sizeof(p->history[0]));

        /* Start next line */
        for (s = t; *s == ' '; s++) /* loop */;
    }
}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw(struct player *p)
{
    /* Calculate the age */
    p->age = p->race->b_age + randint1(p->race->m_age);

    /* Calculate the height/weight for males */
    if (p->psex == SEX_MALE)
    {
        p->ht = Rand_normal(p->race->m_b_ht, p->race->m_m_ht);
        p->wt = Rand_normal(p->race->m_b_wt, p->race->m_m_wt);
    }

    /* Calculate the height/weight for females/neutrals */
    else
    {
        p->ht = Rand_normal(p->race->f_b_ht, p->race->f_m_ht);
        p->wt = Rand_normal(p->race->f_b_wt, p->race->f_m_wt);
    }
}


/*
 * Get the player's starting money
 */
static void get_money(struct player *p)
{
    p->au = STARTING_GOLD;
    if (cfg_no_recall) p->au *= 2;
}


/*
 * Clear all the global "character" data
 */
static void player_init(struct player *p, int conn)
{
    int i;

    /* Free player structure */
    player_free(p);

    /* Wipe the player */
    WIPE(p, struct player);

    /* Allocate memory for his inventory */
    p->inventory = C_ZNEW(ALL_INVEN_TOTAL, object_type);

    /* Allocate memory for his lore array */
    p->lore = C_ZNEW(z_info->r_max, monster_lore);

    /* Allocate memory for his artifact array */
    p->art_info = C_ZNEW(z_info->a_max, byte);

    /* Allocate memory for his randart arrays */
    p->randart_info = C_ZNEW(z_info->a_max + 9, byte);
    p->randart_created = C_ZNEW(z_info->a_max + 9, byte);

    /* Allocate memory for his dungeon flags array */
    p->obj_aware = C_ZNEW(z_info->k_max, bool);
    p->obj_tried = C_ZNEW(z_info->k_max, bool);
    p->kind_everseen = C_ZNEW(z_info->k_max, byte);
    p->ego_everseen = C_ZNEW(z_info->e_max, byte);

    /* Allocate memory for his visuals */
    p->f_attr = C_ZNEW(z_info->f_max, byte_lit);
    p->f_char = C_ZNEW(z_info->f_max, char_lit);
    p->k_attr = C_ZNEW(z_info->k_max, byte);
    p->k_char = C_ZNEW(z_info->k_max, char);
    p->d_attr = C_ZNEW(z_info->k_max, byte);
    p->d_char = C_ZNEW(z_info->k_max, char);
    p->r_attr = C_ZNEW(z_info->r_max, byte);
    p->r_char = C_ZNEW(z_info->r_max, char);

    /* Allocate memory for his object and monster lists */
    p->obj_marked = C_ZNEW(z_info->o_max, byte);
    p->mon_vis = C_ZNEW(z_info->m_max, bool);
    p->mon_los = C_ZNEW(z_info->m_max, bool);
    p->mon_det = C_ZNEW(z_info->m_max, byte);
    p->mon_hurt = C_ZNEW(z_info->m_max, bool);

    /* Hack -- initialize history */
    history_init(p);

    /* Allocate memory for his current cave grid info */
    p->cave = ZNEW(struct player_cave);
    p->cave->info = C_ZNEW(DUNGEON_HGT, byte_256);
    p->cave->cost = C_ZNEW(DUNGEON_HGT, byte_wid);
    p->cave->when = C_ZNEW(DUNGEON_HGT, byte_wid);

    /* Array of grids */
    p->temp_g = C_ZNEW(TEMP_MAX, u16b);

    /* Analyze every object */
    for (i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip "empty" objects */
        if (!k_ptr->name) continue;

        /* No flavor yields aware */
        if (!k_ptr->flavor) p->obj_aware[i] = TRUE;
    }

    /* Always start with a well fed player */
    p->food = PY_FOOD_FULL - 1;

    /* None of the spells have been learned yet */
    for (i = 0; i < PY_MAX_SPELLS; i++) p->spell_order[i] = 99;

    /* Assume no feeling */
    p->feeling = -1;

    /* Hack -- Assume the player has an initial knowledge of the area close to town */
    for (i = 0; i < 13; i++) wild_set_explored(p, i);

    /* Copy channels pointer */
    p->on_channel = Conn_get_console_channels(conn);

    /* Clear old channels */
    for (i = 0; i < MAX_CHANNELS; i++)
        p->on_channel[i] = 0;

    /* Listen on the default chat channel */
    p->on_channel[0] |= UCM_EAR;

    /* Copy his connection info */
    p->conn = conn;

    /* XXX default race/class */
    p->race = player_id2race(0);
    p->clazz = player_id2class(0);

    /* Array of stacked monster messages */
    p->mon_msg = C_ZNEW(MAX_STORED_MON_MSG, monster_race_message);
    p->mon_message_hist = C_ZNEW(MAX_STORED_MON_CODES, monster_message_history);
}


/*
 * Try to wield everything wieldable in the inventory.
 */
static void wield_all(struct player *p)
{
    object_type *o_ptr;
    object_type *i_ptr;
    object_type object_type_body;
    int slot;
    int item;
    int num;
    bool is_ammo;

    /* Scan through the slots backwards */
    for (item = INVEN_PACK - 1; item >= 0; item--)
    {
        o_ptr = &p->inventory[item];
        is_ammo = obj_is_ammo(p, o_ptr);

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Make sure we can wield it */
        slot = wield_slot(p, o_ptr);
        if (slot < INVEN_WIELD) continue;
        i_ptr = &p->inventory[slot];
        if (i_ptr->kind && (!is_ammo || (is_ammo && !object_similar(o_ptr, i_ptr, OSTACK_PACK))))
            continue;

        /* Dragons and Monks cannot use weapons */
        if ((player_has(p, PF_DRAGON) || player_has(p, PF_MARTIAL_ARTS)) &&
            ((slot == INVEN_WIELD) || (slot == INVEN_BOW)))
        {
            continue;
        }

        /* Figure out how much of the item we'll be wielding */
        num = (is_ammo? o_ptr->number: 1);

        /* Get local object */
        i_ptr = &object_type_body;
        object_copy(i_ptr, o_ptr);

        /* Modify quantity */
        i_ptr->number = num;

        /* Decrease the item (from the pack) */
        inven_item_increase(p, item, 0 - num);
        inven_item_optimize(p, item);

        /* Get the wield slot */
        o_ptr = &p->inventory[slot];

        /* Wear the new stuff */
        object_copy(o_ptr, i_ptr);

        /* Increase the weight */
        p->total_weight += i_ptr->weight * i_ptr->number;
    }

    save_quiver_size(p, FALSE);
}


static void player_outfit_aux(struct player *p, struct object_kind *k, byte number, s32b pval)
{
    object_type object_type_body;
    struct object *i_ptr = &object_type_body;

    /* Prepare the item */
    object_prep(i_ptr, k, 0, MINIMISE);
    if (number) i_ptr->number = number;
    if (pval) i_ptr->pval[DEFAULT_PVAL] = pval;

    /* Set origin */
    set_origin(i_ptr, ORIGIN_BIRTH, 0, 0);

    /* Object is known */
    object_notice_everything(p, i_ptr, FALSE);

    /* Bypass auto-squelch */
    i_ptr->squelch = SQUELCH_PROTECT;

    inven_carry(p, i_ptr, FALSE);
    p->kind_everseen[k->kidx] = 1;

    /* Deduct the cost of the item from starting cash */
    p->au -= object_value(p, i_ptr, i_ptr->number);
}


/*
 * Init players with some belongings
 *
 * Having an item identifies it and makes the player "aware" of its purpose.
 */
static void player_outfit(struct player *p)
{
    const struct start_item *si;

    /* Give the player starting equipment */
    for (si = p->clazz->start_items; si; si = si->next)
        player_outfit_aux(p, si->kind, (byte)rand_range(si->min, si->max), 0);

    /* Sanity check */
    if (p->au < 0) p->au = 0;
}


/* Book limits by class: 0 = index of the first high book, 1 = number of books */
static byte book_limits[2][MAX_CLASS] =
{
    {0, 4, 4, 3, 4, 4, 4, 0, 1, 0, 3, 0, 2, 3, 0},
    {0, 9, 9, 6, 9, 9, 8, 0, 3, 0, 6, 5, 4, 6, 0}
};


/*
 * Hard-coded items to give DM at his birth.
 */
static byte dm_player_outfit[][4] =
{
    /* tval, sval, number, pval */
    {TV_POTION, SV_POTION_AUGMENTATION, MAX_STACK_SIZE - 1, 0},
    {TV_POTION, SV_POTION_EXPERIENCE, MAX_STACK_SIZE - 1, 0},
    {TV_RING, SV_RING_SPEED, 2, 30},
    {0, 0, 0, 0}
};


/*
 * Init the DM with some belongings
 */
static void player_outfit_dm(struct player *p)
{
    int i;

    /* Initialize the DM with special powers */
    if (is_dm_p(p))
    {
        p->exp = p->max_exp = 50000000;
        p->lev = p->max_lev = PY_MAX_LEVEL;
        if (p->dm_flags & DM_INVULNERABLE)
        {
            p->timed[TMD_INVULN] = -1;
            p->update |= (PU_MONSTERS);
            p->redraw |= (PR_MAP | PR_STATUS);
        }
        set_ghost_flag(p, 1, FALSE);
        p->noscore = 1;
        get_bonuses(p);
    }

    /*
     * Give the DM some interesting stuff
     * In debug mode, everyone gets all that stuff for testing purposes
     */
#ifndef _DEBUG
    if (!is_dm_p(p)) return;
#endif

    /* All deep books */
    for (i = book_limits[0][p->clazz->cidx]; i < book_limits[1][p->clazz->cidx]; i++)
        player_outfit_aux(p, lookup_kind(p->clazz->spell_book, i), 1, 0);

    /* Other useful stuff */
    for (i = 0; dm_player_outfit[i][0]; i++)
    {
        player_outfit_aux(p, lookup_kind(dm_player_outfit[i][0], dm_player_outfit[i][1]),
            dm_player_outfit[i][2], dm_player_outfit[i][3]);
    }

    /* A ton of gold */
    p->au = 50000000;
}


/*
 * This fleshes out a full player based on the choices currently made,
 * and so is called whenever things like race or class are chosen.
 */
static void player_generate(struct player *p, byte psex, const struct player_race *r,
    const struct player_class *c)
{
    p->psex = psex;
    p->clazz = c;
    p->race = r;

    /* Restrict choices for Dragon race */
    if (pf_has(p->race->pflags, PF_DRAGON))
    {
        /* No Dragon DMs, Shapechangers and Necromancers */
        if (is_dm_p(p) || pf_has(p->clazz->pflags, PF_MONSTER_SPELLS) ||
            pf_has(p->clazz->pflags, PF_UNDEAD_POWERS))
        {
            p->race = player_id2race(0);
        }
    }

    p->sex = &sex_info[p->psex];

    /* Level 1 */
    p->max_lev = p->lev = 1;

    /* Experience factor */
    p->expfact = p->race->r_exp + p->clazz->c_exp;

    /* Hitdice */
    p->hitdie = p->race->r_mhp + p->clazz->c_mhp;

    /* Initial hitpoints */
    p->mhp = p->hitdie;

    /* Pre-calculate level 1 hitdice */
    p->player_hp[0] = p->hitdie;
}


static int count_players(struct player *p)
{
    int i, count = 0;

    /* Count players on this depth */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Skip this player */
        if (p_ptr == p) continue;

        /* Count */
        if (p_ptr->depth == p->depth) count++;
    }

    return count;
}


/*
 * Clear the flags for each cave grid
 */
void clear_cave_info(struct player *p, bool full)
{
    int y, x;

    /* Assume no feeling */
    if (full) p->feeling = -1;

    /* Reset number of feeling squares */
    if (full) p->cave->feeling_squares = 0;

    /* Clear flags and flow information. */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Erase flags */
            if (full)
                p->cave->info[y][x] = 0;
            else
                p->cave->info[y][x] &= ~(CAVE_MARK | CAVE_SEEN | CAVE_VIEW | CAVE_DTRAP | CAVE_DEDGE);

            /* Erase flow */
            if (full)
            {
                p->cave->cost[y][x] = 0;
                p->cave->when[y][x] = 0;
            }
        }
    }
}


static void player_setup(struct player *p, int id, u32b account)
{
    int wild_idx = world_index(p->world_x, p->world_y);
    bool reposition = FALSE;
    int i, k, y, x, d;
    hturn death_turn;

    /* Paranoia: catch bad player coordinates */

    /* Invalid depth */
    if ((p->depth < 0 - MAX_WILD) || (p->depth >= MAX_DEPTH)) reposition = TRUE;

    /* Invalid wilderness coordinates */
    else if ((wild_idx < 0 - MAX_WILD) || (wild_idx >= MAX_DEPTH) ||
        ((p->depth < 0) && (p->depth != wild_idx)) || ((p->depth >= 0) && (wild_idx != 0)))
    {
        reposition = TRUE;

        /* Unstatic the old level, in case depth was valid */
        players_on_depth[p->depth] = count_players(p);
    }

    /* Default location if just starting */
    else if (!p->depth && !p->py && !p->px) reposition = TRUE;

    /* Don't allow placement inside an arena */
    else if (pick_arena(p->depth, p->py, p->px) != -1)
    {
        reposition = TRUE;

        /* Unstatic the old level */
        players_on_depth[p->depth] = count_players(p);
    }

    /* Hack -- DM redesigning the level */
    else if (players_on_depth[p->depth] == INHIBIT_DEPTH) reposition = TRUE;

    /*
     * Don't allow placement inside a house if someone is shopping or
     * if we don't own it (anti-exploit)
     */
    else
    {
        player_type *q_ptr;

        for (i = 0; i < num_houses; i++)
        {
            /* Are we inside this house? */
            if (house_inside(p, i))
            {
                /* If we don't own it, get out of it */
                if (!house_owned_by(p, i))
                {
                    reposition = TRUE;

                    /* Unstatic the old level */
                    players_on_depth[p->depth] = count_players(p);

                    break;
                }

                /* Is anyone shopping in it? */
                for (k = 1; k <= NumPlayers; k++)
                {
                    q_ptr = player_get(k);
                    if (q_ptr && (p != q_ptr))
                    {
                        /* Someone in here? */
                        if ((q_ptr->player_store_num == i) && (q_ptr->store_num == STORE_PLAYER))
                        {
                            reposition = TRUE;

                            /* Unstatic the old level */
                            players_on_depth[p->depth] = count_players(p);

                            break;
                        }
                    }
                }

                break;
            }
        }
    }

    /* Reset */
    p->arena_num = -1;

    /* If we need to reposition the player, do it */
    if (reposition)
    {
        /* Put us in town */
        p->depth = 0;
        p->world_x = 0;
        p->world_y = 0;

        /* Clear the flags for each cave grid */
        clear_cave_info(p, TRUE);
    }

    /* Make sure the server doesn't think the player is in a store */
    p->store_num = -1;

    /* Rebuild the level if necessary */
    if (!cave_get(p->depth))
    {
        /* Allocate space for it */
        alloc_dungeon_level(p->depth);

        /* Generate a dungeon level there */
        p->feeling = cave_generate(cave_get(p->depth), p);

        /* He is now on the level, so add him to the player_on_depth list */
        players_on_depth[p->depth]++;

        /* Paranoia: update the player's wilderness map */
        if (p->depth < 0) wild_set_explored(p, 0 - p->depth);
    }

    /* Apply illumination */
    else
    {
        /*
         * Make sure he's supposed to be here -- If not, then the level has
         * been unstaticed and so he should forget his memory of the old level.
         */
        if (ht_cmp(&cave_get(p->depth)->generated, &p->quit_turn) > 0)
        {
            /* Clear the flags for each cave grid */
            clear_cave_info(p, TRUE);

            /* He is now on the level, so add him to the player_on_depth list */
            players_on_depth[p->depth]++;
        }

        /* Hack -- Night time in wilderness */
        if ((p->depth < 0) && !is_daytime()) clear_cave_info(p, FALSE);

        cave_illuminate(p, cave_get(p->depth), (is_daytime()? TRUE: FALSE));
    }

    /* Player gets to go first */
    p->energy = level_speed(p->depth);

    /* If we need to reposition the player, do it */
    if (reposition)
    {
        /* Put us in the tavern */
        p->py = level_down_y[0];
        p->px = level_down_x[0];
    }

    /* Be sure the player is in bounds */
    if (p->px < 1) p->px = 1;
    if (p->py < 1) p->py = 1;
    if (p->px >= DUNGEON_WID) p->px = DUNGEON_WID - 1;
    if (p->py >= DUNGEON_HGT) p->py = DUNGEON_HGT - 1;

    /* Pick a location */
    /* Players should NEVER be placed on top of other stuff */
    /* Simply move the player away until a proper location is found */
    /* If no location can be found (VERY unlikely), then simply use the initial location */
    for (i = 0; i < 3000; i++)
    {
        /* Increase distance (try 10 times for each step) */
        d = (i + 9) / 10;

        /* Pick a location (skip LOS test) */
        scatter(p->depth, &y, &x, p->py, p->px, d, TRUE);

        /* Require an "empty" floor grid */
        if (cave_empty_bold(p->depth, y, x))
        {
            /* Set the player's location */
            p->py = y;
            p->px = x;

            break;
        }
    }

    /* Hack -- Set previous player location */
    p->old_py = p->py;
    p->old_px = p->px;

    /* Add the player */
    cave_get(p->depth)->m_idx[p->py][p->px] = 0 - id;

    /* Redraw */
    cave_light_spot(cave_get(p->depth), p->py, p->px);

    /*
     * Delete him from the player name database
     *
     * This is useful for fault tolerance, as it is possible to have
     * two entries for one player name, if the server crashes hideously
     * or the machine has a power outage or something.
     * This is also useful when the savefile has been manually deleted.
     */
    delete_player_name(p->name);

    /* Add him to the player name database */
    ht_reset(&death_turn);
    add_player_name(p->id, account, p->name, &death_turn);
    plog_fmt("Player Name is [%s], id is %d", p->name, (int)p->id);

    /* Set his "current activities" variables */
    current_clear(p);
    p->current_house = p->current_selling = -1;
    p->offset_y_old = p->offset_x_old = -1;

    /* Make sure his party still exists */
    if (p->party && parties[p->party].num == 0)
    {
        /* Reset to neutral */
        p->party = 0;
    }

    /* Tell the server to redraw the player's display */
    p->redraw |= (PR_MAP | PR_EXTRA | PR_BASIC | PR_PLUSSES);

    /* Update his view, light, bonuses, and torch radius */
    p->update |= (PU_UPDATE_VIEW | PU_TORCH | PU_BONUS | PU_DISTANCE);

    /* Redraw */
    p->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL);

    /* This guy is alive now */
    p->alive = TRUE;
}


static void player_admin(struct player *p)
{
    /* Hack -- Set Dungeon Master flags */
#ifdef _DEBUG
    p->dm_flags |= (DM___MENU | DM_CAN_MUTATE_SELF);
#endif

    if (cfg_dungeon_master && !strcmp(p->name, cfg_dungeon_master))
    {
        /* All DM powers! */
        p->dm_flags = 0xFFFFFFFF;
        if (!cfg_secret_dungeon_master) p->dm_flags ^= DM_SECRET_PRESENCE;
    }
}


/*
 * Handle quick-start creation
 */
static void quickstart_roll(player_type *p_ptr, bool character_existed,
    byte *pridx, byte *pcidx, byte *psex, s16b *stat_roll)
{
    /* A character existed in the savefile */
    if (character_existed)
    {
        int i;

        /* Use previous info */
        *pridx = p_ptr->race->ridx;
        *pcidx = p_ptr->clazz->cidx;
        *psex = p_ptr->psex;

        /* Use point-based roller with previous birth stats */
        for (i = 0; i < A_MAX; i++) stat_roll[i] = p_ptr->stat_birth[i];
        stat_roll[A_MAX] = BR_POINTBASED;
    }

    /* New character */
    else
    {
        /* Roll a male half-troll warrior */
        *pridx = 7;
        *pcidx = 0;
        *psex = 1;

        /* Use standard roller with STR CON DEX CHR WIS INT as stat order */
        stat_roll[0] = A_STR;
        stat_roll[1] = A_CON;
        stat_roll[2] = A_DEX;
        stat_roll[3] = A_CHR;
        stat_roll[4] = A_WIS;
        stat_roll[5] = A_INT;
        stat_roll[A_MAX] = BR_NORMAL;
    }
}


/*
 * Handle dynastic quick start creation
 *
 * Returns 1 if quick start is possible, 0 if quick start is not possible, -1 if an error occurs.
 */
static int quickstart_ok(struct player *p, const char *name, int conn)
{
    char previous[NORMAL_WID];

    /* Get last incarnation */
    my_strcpy(previous, name, sizeof(previous));
    if (!get_incarnation(-1, previous, sizeof(previous))) return 0;

    /* Clear old information */
    player_init(p, conn);

    /* Copy his name */
    my_strcpy(p->name, previous, sizeof(p->name));

    /* Verify his name and create a savefile name */
    if (!process_player_name(p, TRUE)) return -1;

    /* Try to load the savefile */
    p->is_dead = TRUE;
    if (!(p->savefile[0] && file_exists(p->savefile))) return 0;
    if (!load_player(p)) return -1;

    /* Still alive */
    if (!p->is_dead) return 0;

    /* Success */
    return 1;
}


/*
 * Create a character.  Then wait for a moment.
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
struct player *player_birth(int id, u32b account, const char *name, const char *pass, int conn,
    byte ridx, byte cidx, byte psex, s16b* stat_roll)
{
    struct player *p;
    int i;
    bool character_existed = FALSE;

    /* Do some consistency checks */
    if (ridx >= player_rmax()) ridx = 0;
    if (cidx >= player_cmax()) cidx = 0;
    if (psex >= MAX_SEXES) psex = SEX_FEMALE;

    /* Allocate player and set pointer */
    p = ZNEW(player_type);
    player_set(id, p);

    /* Handle dynastic quick start */
    if (stat_roll[A_MAX] == BR_QDYNA)
    {
        int ret = quickstart_ok(p, name, conn);

        if (ret == -1)
        {
            player_free(p);
            mem_free(p);
            player_set(id, NULL);
            return NULL;
        }
        quickstart_roll(p, (ret? TRUE: FALSE), &ridx, &cidx, &psex, stat_roll);
    }

    /* Clear old information */
    player_init(p, conn);

    /* Copy his name */
    my_strcpy(p->name, name, sizeof(p->name));
    my_strcpy(p->pass, pass, sizeof(p->pass));

    /* DM powers? */
    player_admin(p);

    /* Verify his name and create a savefile name */
    if (!process_player_name(p, TRUE))
    {
        player_free(p);
        mem_free(p);
        player_set(id, NULL);
        return NULL;
    }

    /*** Try to load the savefile ***/

    p->is_dead = TRUE;

    if (p->savefile[0] && file_exists(p->savefile))
    {
        if (!load_player(p))
        {
            player_free(p);
            mem_free(p);
            player_set(id, NULL);
            return NULL;
        }

        /* Player is dead */
        if (p->is_dead)
        {
            /* A character existed in this savefile. */
            character_existed = TRUE;
        }

        /* Still alive */
        else if (p->chp >= 0)
        {
            /* Reset cause of death */
            my_strcpy(p->died_from, "(alive and well)", sizeof(p->died_from));
        }
    }

    /* No living character loaded */
    if (p->is_dead)
    {
        /* Make new player */
        p->is_dead = FALSE;

        /* Handle quick start */
        if (stat_roll[A_MAX] == BR_QUICK)
            quickstart_roll(p, character_existed, &ridx, &cidx, &psex, stat_roll);

        /* Hack -- Rewipe the player info if load failed */
        player_init(p, conn);

        /* Copy his name and connection info */
        my_strcpy(p->name, name, sizeof(p->name));
        my_strcpy(p->pass, pass, sizeof(p->pass));

        /* Reprocess his name */
        if (!process_player_name(p, TRUE))
        {
            player_free(p);
            mem_free(p);
            player_set(id, NULL);
            return NULL;
        }

        /* DM powers? */
        player_admin(p);

        /* Set his ID */
        p->id = player_id++;

        /* Actually Generate */
        player_generate(p, psex, player_id2race(ridx), player_id2class(cidx));

        /* Get a new character */
        get_stats(p, stat_roll);

        /* Update stats with bonuses, etc. */
        get_bonuses(p);

        /* Roll for age/height/weight */
        get_ahw(p);

        /* Roll for social class */
        get_history(p);

        roll_hp(p);

        /* Add new starting message */
        history_add_unique(p, "Began the quest to destroy Morgoth", HISTORY_PLAYER_BIRTH);

        /* Give the player some money */
        get_money(p);

        /* Outfit the player, if they can sell the stuff */
        if (!OPT_P(p, birth_no_selling)) player_outfit(p);
        player_outfit_dm(p);

        /* Now try wielding everything */
        wield_all(p);

        /* Dragon */
        if (player_has(p, PF_DRAGON))
        {
            poly_dragon(p, FALSE);
            get_bonuses(p);
        }

        /* Set his location, panel, etc. */
        player_setup(p, id, account);

        /* Give the DM full knowledge */
        if (is_dm_p(p))
        {
            /* Every item in the game */
            for (i = 0; i < z_info->k_max; i++)
            {
                p->obj_aware[i] = TRUE;
                p->kind_everseen[i] = 1;
            }
            for (i = 1; i < z_info->e_max; i++) p->ego_everseen[i] = 1;

            /* Every monster in the game */
            for (i = 1; i < z_info->r_max; i++)
            {
                p->lore[i].pseen = 1;
                p->lore[i].pkills = MAX_DEPTH;
            }
        }

        /* Success */
        return p;
    }

    /* Loading succeeded */
    player_setup(p, id, account);
    return p;
}


/*
 * We are starting a "brand new" server.
 * This function is only called if the server state savefile could not be loaded.
 */
void server_birth(void)
{
    /* Set party zero's name to "Neutral" */
    my_strcpy(parties[0].name, "Neutral", sizeof(parties[0].name));

    /* First player's ID should be 1 */
    player_id = 1;
}


/*  
 * Check if the given connection type is valid.
 */
u16b connection_type_ok(u16b conntype)
{
    if (conntype == CONNTYPE_PLAYER) return CONNTYPE_PLAYER;
    if (conntype == 8202 || conntype == 8205) return CONNTYPE_CONSOLE;
    return CONNTYPE_ERROR;
}


/*
 * Free player structure
 */
void player_free(struct player *p)
{
    if (!p) return;

    mem_free(p->inventory);
    mem_free(p->lore);
    mem_free(p->art_info);
    mem_free(p->randart_info);
    mem_free(p->randart_created);
    mem_free(p->obj_aware);
    mem_free(p->obj_tried);
    mem_free(p->kind_everseen);
    mem_free(p->ego_everseen);
    mem_free(p->f_attr);
    mem_free(p->f_char);
    mem_free(p->k_attr);
    mem_free(p->k_char);
    mem_free(p->d_attr);
    mem_free(p->d_char);
    mem_free(p->r_attr);
    mem_free(p->r_char);
    mem_free(p->obj_marked);
    mem_free(p->mon_vis);
    mem_free(p->mon_los);
    mem_free(p->mon_det);
    mem_free(p->mon_hurt);

    history_clear(p);

    /* Free the cave */
    if (p->cave)
    {
        mem_free(p->cave->info);
        mem_free(p->cave->cost);
        mem_free(p->cave->when);
        mem_free(p->cave);
    }

    /* Free the temp array */
    mem_free(p->temp_g);

    /* Free the stacked monster messages */
    mem_free(p->mon_msg);
    mem_free(p->mon_message_hist);
}
