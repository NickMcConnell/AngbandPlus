/*
 * File: player-birth.c
 * Purpose: Character creation
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


/* Current player ID */
s32b player_id;


/* Basic sort algorithm */
static void sort_stats(s16b* stats, s16b* stat_order)
{
    int i, j;

    /* I use a bubble sort because I'm lazy at the moment */
    for (i = 0; i < STAT_MAX; i++)
    {
        for (j = 0; j < STAT_MAX - 1; j++)
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
    int dice[3 * STAT_MAX];

    /* Roll and verify some stats */
    while (true)
    {
        /* Roll some dice */
        for (j = i = 0; i < 3 * STAT_MAX; i++)
        {
            /* Roll the dice */
            dice[i] = randint1(3 + i % 3);

            /* Collect the maximum */
            j += dice[i];
        }

        /* Verify totals */
        if ((j > 7 * STAT_MAX) && (j < 9 * STAT_MAX)) break;
    }

    /* Roll the stats */
    for (i = 0; i < STAT_MAX; i++)
    {
        /* Extract 5 + 1d3 + 1d4 + 1d5 */
        j = 5 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

        /* Save that value */
        stats[i] = j;
    }
}


/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[9] = {0, 1, 2, 3, 4, 5, 6, 8, 12};


/* Pool of available points */
#define MAX_BIRTH_POINTS 20


/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(struct player *p, s16b* stat_roll)
{
    int i;
    bool rand_roller = false;
    s16b stats[STAT_MAX];

    /* Point-based roller */
    if (stat_roll[STAT_MAX] == BR_POINTBASED)
    {
        int cost = 0;

        /* Check over the given stats */
        for (i = 0; i < STAT_MAX; i++)
        {
            /* Check data */
            if ((stat_roll[i] < 10) || (stat_roll[i] > 18))
            {
                /* Incorrect data: use random roller */
                rand_roller = true;
                break;
            }

            /* Total cost */
            cost += birth_stat_costs[stat_roll[i] - 10];
        }

        /* Incorrect data: use random roller */
        if (cost > MAX_BIRTH_POINTS) rand_roller = true;

        /* Point-based roller: use "stat_roll" directly */
        if (!rand_roller)
        {
            /* Stats are given by "stat_roll" directly */
            for (i = 0; i < STAT_MAX; i++) p->stat_max[i] = stat_roll[i];
        }

        /* Random roller: roll and accept whatever stats we get */
        else
        {
            /* Roll and verify some stats */
            roll_stats(stats);

            /* Accept whatever stats we get */
            for (i = 0; i < STAT_MAX; i++) p->stat_max[i] = stats[i];
        }
    }

    /* Standard roller */
    else
    {
        s16b stat_order[STAT_MAX], stat_limit[STAT_MAX], stat_ok[STAT_MAX];
        int j;

        /* Clear "stats" array */
        for (i = 0; i < STAT_MAX; i++) stats[i] = 0;

        /* Stat order is given by "stat_roll" directly */
        for (i = 0; i < STAT_MAX; i++) stat_order[i] = stat_roll[i];

        /*
         * Ensure a minimum value of 17 for the first stat, 15 for the second
         * stat and 12 for the third stat; other stats have the legal minimum
         * value of 8
         */
        stat_limit[0] = 17;
        stat_limit[1] = 15;
        stat_limit[2] = 12;
        stat_limit[3] = 8;
        stat_limit[4] = 8;

        /* Clear "stat_ok" array */
        for (i = 0; i < STAT_MAX; i++) stat_ok[i] = 0;

        /* Check over the given stat order */
        for (i = 0; i < STAT_MAX; i++)
        {
            /* Check data */
            if ((stat_order[i] < 0) || (stat_order[i] >= STAT_MAX))
            {
                /* Incorrect data: use random roller */
                rand_roller = true;
                break;
            }

            /* Increment "stat_ok" */
            stat_ok[stat_order[i]]++;
        }

        /* Check for duplicated or missing entries */
        for (i = 0; i < STAT_MAX; i++)
        {
            /* Check "stat_ok" flag */
            if (stat_ok[i] != 1)
            {
                /* Incorrect order: use random roller */
                rand_roller = true;
                break;
            }
        }

        /* Roll */
        while (true)
        {
            bool accept = true;

            /* Roll and verify some stats */
            roll_stats(stats);

            /* Random roller: accept whatever stats we get */
            if (rand_roller) break;

            /* Clear "stat_ok" array */
            for (i = 0; i < STAT_MAX; i++) stat_ok[i] = 0;

            /* Count acceptable stats */
            for (i = 0; i < STAT_MAX; i++)
            {
                /* Increment count of acceptable stats */
                for (j = 0; j < STAT_MAX; j++)
                {
                    /* This stat is okay */
                    if (stats[j] >= stat_limit[i]) stat_ok[i]++;
                }
            }

            /* Check acceptable stats */
            for (i = 0; i < STAT_MAX; i++)
            {
                /* This stat is not okay */
                if (stat_ok[i] <= i)
                {
                    accept = false;
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
            for (i = 0; i < STAT_MAX; i++) p->stat_max[stat_order[i]] = stats[i];
        }

        /* Random roller: accept whatever stats we get */
        else
        {
            /* Accept whatever stats we get */
            for (i = 0; i < STAT_MAX; i++) p->stat_max[i] = stats[i];
        }
    }

    /* Save the stats */
    for (i = 0; i < STAT_MAX; i++)
    {
        /* Start fully healed */
        p->stat_cur[i] = p->stat_max[i];

        /* Start with unscrambled stats */
        p->stat_map[i] = i;

        /* Save birth stats */
        p->stat_birth[i] = p->stat_max[i];
    }
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
    while (true)
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
    p->upkeep->update |= (PU_BONUS);

    /* Update stuff */
    update_stuff(p, chunk_get(&p->wpos));

    /* Fully healed */
    p->chp = p->mhp;

    /* Fully rested */
    p->csp = p->msp;
}


#define g_strcat(P, T) \
    n = strlen((T)); strncpy((P), (T), n); (P) += n


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(struct player *p)
{
    struct history_entry *entry;
    int i, n;
    char *s, *t;
    char buf[240];
    struct history_chart *chart;

    /* Clear the previous history strings */
    for (i = 0; i < N_HIST_LINES; i++) p->history[i][0] = '\0';

    /* Clear the history text */
    buf[0] = '\0';

    /* Set pointer */
    t = &buf[0];

    /* Starting place */
    chart = p->race->history;

    /* Process the history */
    while (chart)
    {
        /* Roll for nobility */
        int roll = randint1(100);

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
            switch (*s)
            {
                case '$':
                case '~':
                    s++;
                    switch (*s)
                    {
                        case 'u': {g_strcat(t, "You"); break;}
                        case 'r': {g_strcat(t, "Your"); break;}
                        case 'a': {g_strcat(t, "are"); break;}
                        case 'h': {g_strcat(t, "have"); break;}
                        default: continue;
                    }
                    break;
                default:
                    *t++ = *s;
            }
        }
        *t = '\0';

        /* Enter the next chart */
        chart = entry->succ;
    }

    /* Skip leading spaces */
    for (s = buf; *s == ' '; s++) /* loop */;

    /* Get apparent length */
    n = strlen(s);

    /* Kill trailing spaces */
    while ((n > 0) && (s[n - 1] == ' ')) s[--n] = '\0';

    /* Start at first line */
    i = 0;

    /* Collect the history */
    while (true)
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
        for (n = N_HIST_WRAP - 1; ((n > 0) && (s[n - 1] != ' ')); n--) /* loop */;

        /* Save next location */
        t = s + n;

        /* Save one line of history */
        my_strcpy(p->history[i++], s, n + 1);

        /* Start next line */
        s = t;
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

    /* Calculate the height/weight for females */
    else if (p->psex == SEX_FEMALE)
    {
        p->ht = Rand_normal(p->race->f_b_ht, p->race->f_m_ht);
        p->wt = Rand_normal(p->race->f_b_wt, p->race->f_m_wt);
    }

    /* For neither, go inbetween */
    else
    {
        p->ht = Rand_normal((p->race->f_b_ht + p->race->m_b_ht) / 2,
            (p->race->f_m_ht + p->race->m_m_ht) / 2);
        p->wt = Rand_normal((p->race->f_b_wt + p->race->m_b_wt) / 2,
            (p->race->f_m_wt + p->race->m_m_wt) / 2);
    }
}


/*
 * Get the player's starting money
 */
static void get_money(struct player *p, bool no_recall)
{
    p->au = z_info->start_gold;

    /* Give double starting gold to no_recall characters */
    if ((cfg_diving_mode == 2) || no_recall) p->au *= 2;
}


/*
 * Try to wield everything wieldable in the inventory.
 */
static void wield_all(struct player *p)
{
    struct object *obj, *new_pile = NULL;
    int slot;

    /* Scan through the slots */
    for (obj = p->gear; obj; obj = obj->next)
    {
        struct object *obj_temp;

        /* Make sure we can wield it */
        if (!item_tester_hook_wear(p, obj)) continue;
        slot = wield_slot(p, obj);
        obj_temp = slot_object(p, slot);
        if (obj_temp) continue;

        /* Split if necessary */
        if (obj->number > 1)
        {
            /* All but one go to the new object */
            struct object *obj_new = object_split(obj, obj->number - 1);

            /* Add to the pile of new objects to carry */
            pile_insert(&new_pile, obj_new);
        }

        /* Wear the new stuff */
        obj->oidx = z_info->pack_size + slot;
        p->body.slots[slot].obj = obj;

        /* Increment the equip counter by hand */
        p->upkeep->equip_cnt++;
    }

    /* Now add the unwielded split objects to the gear */
    if (new_pile)
        pile_insert_end(&p->gear, new_pile);
}


static void player_outfit_aux(struct player *p, struct object_kind *k, byte number)
{
    struct object *obj = object_new();

    /* Prepare the item */
    object_prep(p, obj, k, 0, MINIMISE);
    if (number) obj->number = number;

    /* Hack -- ring of speed (for DM) */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Speed")))
        obj->modifiers[OBJ_MOD_SPEED] = 30;

    /* Set origin */
    set_origin(obj, ORIGIN_BIRTH, 0, NULL);

    /* Object is known */
    object_notice_everything_aux(p, obj, false, false);

    /* Bypass auto-ignore */
    obj->ignore_protect = 1;

    /* Deduct the cost of the item from starting cash */
    p->au -= (s32b)object_value(p, obj, obj->number);

    /* Carry the item */
    inven_carry(p, obj, true, false);
    p->kind_everseen[k->kidx] = 1;
}


/*
 * Init players with some belongings
 *
 * Having an item identifies it and makes the player "aware" of its purpose.
 */
static void player_outfit(struct player *p, bool start_kit, bool no_recall)
{
    int i;
    const struct start_item *si;

    /* Player learns innate runes */
    player_learn_innate(p);

    /* Give the player obvious object knowledge */
    p->obj_k->dd = 1;
    p->obj_k->ds = 1;
    p->obj_k->ac = 1;
    p->obj_k->to_a = 1;
    p->obj_k->to_h = 1;
    p->obj_k->to_d = 1;
    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if (prop->subtype == OFT_LIGHT) of_on(p->obj_k->flags, i);
        if (prop->subtype == OFT_DIG) of_on(p->obj_k->flags, i);
    }

    /* Give the player starting equipment */
    for (si = p->clazz->start_items; si; si = si->next)
    {
        int num = rand_range(si->min, si->max);

        /* Without start_kit, only start with 1 food and 1 light */
        if (!start_kit)
        {
            if (!tval_is_food_k(si->kind) && !tval_is_light_k(si->kind)) continue;
            num = 1;
        }

        /* Don't give unnecessary starting equipment to no_recall characters */
        if (((cfg_diving_mode == 2) || no_recall) && !si->flag) continue;

        player_outfit_aux(p, si->kind, (byte)num);
    }

    /* Sanity check */
    if (p->au < 0) p->au = 0;

    if (cfg_diving_mode || no_recall || is_dm_p(p)) return;

    /* Give the player a deed of property */
    player_outfit_aux(p, lookup_kind_by_name(TV_DEED, "Deed of Property"), 1);
}


/*
 * Init the DM with some belongings
 */
static void player_outfit_dm(struct player *p)
{
    int i;
    struct object_kind *kind;

    /* Initialize the DM with special powers */
    if (is_dm_p(p))
    {
        p->exp = p->max_exp = 50000000;
        p->lev = p->max_lev = PY_MAX_LEVEL;
        if (p->dm_flags & DM_INVULNERABLE)
        {
            p->timed[TMD_INVULN] = -1;
            p->upkeep->update |= (PU_MONSTERS);
            p->upkeep->redraw |= (PR_MAP | PR_STATUS);
        }
        set_ghost_flag(p, 1, false);
        p->noscore = 1;
        get_bonuses(p);
    }

    /*
     * Give the DM some interesting stuff
     * In debug mode, everyone gets all that stuff for testing purposes
     */
#ifndef DEBUG_MODE
    if (!is_dm_p(p)) return;
#endif

    /* All books */
    if (p->clazz->magic.spell_realm && p->clazz->magic.spell_realm->book_noun)
    {
        for (i = 0; i < p->clazz->magic.num_books; i++)
        {
            struct class_book *book = &p->clazz->magic.books[i];

            player_outfit_aux(p, lookup_kind(book->tval, book->sval), 1);
        }
    }

    /* Other useful stuff */
    kind = lookup_kind_by_name(TV_POTION, "Augmentation");
    player_outfit_aux(p, kind, kind->base->max_stack);
    kind = lookup_kind_by_name(TV_POTION, "Experience");
    player_outfit_aux(p, kind, kind->base->max_stack);
    player_outfit_aux(p, lookup_kind_by_name(TV_RING, "Speed"), 2);

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

    /* Initialize the spells */
    player_spells_init(p);

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

    /* Count players on this level */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Skip this player */
        if (player == p) continue;

        /* Count */
        if (COORDS_EQUAL(&player->wpos, &p->wpos)) count++;
    }

    return count;
}


static bool depth_is_valid(struct wild_type *w_ptr, int depth)
{
    if (depth == 0) return true;
    if ((depth >= w_ptr->min_depth) && (depth < w_ptr->max_depth)) return true;
    return false;
}


static void player_setup(struct player *p, int id, u32b account, bool no_recall)
{
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);
    bool reposition = false, push_up = false;
    int i, k, y, x, d;
    hturn death_turn;
    struct chunk *c;

    /* Paranoia: catch bad player coordinates */

    /* Invalid wilderness coordinates */
    if (!w_ptr) reposition = true;

    /* Invalid depth */
    else if (!depth_is_valid(w_ptr, p->wpos.depth)) reposition = true;

    /* Default location if just starting */
    else if (COORDS_NULL(&p->wpos) && !p->py && !p->px) reposition = true;

    /* Don't allow placement inside an arena */
    else if (pick_arena(&p->wpos, p->py, p->px) != -1)
    {
        reposition = true;

        /* Unstatic the old level */
        chunk_set_player_count(&p->wpos, count_players(p));
    }

    /* Hack -- DM redesigning the level */
    else if (chunk_inhibit_players(&p->wpos))
    {
        reposition = true;

        /* No-recall players are simply pushed up one level (should be safe) */
        if ((cfg_diving_mode == 2) || no_recall) push_up = true;
    }

    /*
     * Don't allow placement inside a house if someone is shopping or
     * if we don't own it (anti-exploit)
     */
    else
    {
        struct player *q;

        for (i = 0; i < houses_count(); i++)
        {
            /* Are we inside this house? */
            if (!house_inside(p, i)) continue;

            /* If we don't own it, get out of it */
            if (!house_owned_by(p, i))
            {
                reposition = true;

                /* Unstatic the old level */
                chunk_set_player_count(&p->wpos, count_players(p));

                break;
            }

            /* Is anyone shopping in it? */
            for (k = 1; k <= NumPlayers; k++)
            {
                q = player_get(k);
                if (q && (p != q))
                {
                    /* Someone in here? */
                    if ((q->player_store_num == i) && (q->store_num == STORE_PLAYER))
                    {
                        reposition = true;

                        /* Unstatic the old level */
                        chunk_set_player_count(&p->wpos, count_players(p));

                        break;
                    }
                }
            }

            break;
        }
    }

    /* Reset */
    p->arena_num = -1;

    /* If we need to reposition the player, do it */
    if (reposition)
    {
        /* Hack -- DM redesigning the level (no_recall players) */
        if (push_up) p->wpos.depth = dungeon_get_next_level(p, p->wpos.depth, -1);

        /* Put us in base town */
        else if (cfg_diving_mode || no_recall)
            memcpy(&p->wpos, base_wpos(), sizeof(struct worldpos));

        /* Put us in starting town */
        else
            memcpy(&p->wpos, start_wpos(), sizeof(struct worldpos));
    }

    /* Make sure the server doesn't think the player is in a store */
    p->store_num = -1;

    c = chunk_get(&p->wpos);

    /* Rebuild the level if necessary */
    if (!c)
    {
        /* Generate a dungeon level there */
        c = prepare_next_level(p, &p->wpos);

        /* Player is now on the level */
        chunk_increase_player_count(&p->wpos);

        wild_deserted_message(p);

        /* Paranoia: update the player's wilderness map */
        if (p->wpos.depth == 0) wild_set_explored(p, &p->wpos);
    }

    /* Apply illumination */
    else
    {
        bool done = false;

        /* If we need to reposition the player, do it */
        if (reposition)
        {
            /* Clear the flags for each cave grid (cave dimensions may have changed) */
            player_cave_new(p, c->height, c->width);
            player_cave_clear(p, true);
            done = true;
        }

        /*
         * Make sure he's supposed to be here -- if not, then the level has
         * been unstaticed and so he should forget his memory of the old level.
         */
        if (ht_cmp(&c->generated, &p->quit_turn) > 0)
        {
            /* Clear the flags for each cave grid (cave dimensions may have changed) */
            player_cave_new(p, c->height, c->width);
            player_cave_clear(p, true);
            done = true;

            /* Player is now on the level */
            chunk_increase_player_count(&p->wpos);
        }

        /* Hack -- night time in wilderness */
        if (in_wild(&p->wpos) && !is_daytime())
        {
            player_cave_clear(p, false);
            done = true;
        }

        /* Memorize the content of owned houses */
        if (!done) memorize_houses(p);

        /* Illuminate */
        cave_illuminate(p, c, is_daytime());
    }

    /* Player gets to go first */
    set_energy(p, &p->wpos);

    /* If we need to reposition the player, do it */
    if (reposition)
    {
        /* Put us in the tavern */
        p->py = c->join->down.y;
        p->px = c->join->down.x;
    }

    /* Be sure the player is in bounds */
    if (!square_in_bounds_fully(c, p->py, p->px))
    {
        p->px = MIN(MAX(p->px, 1), c->width - 2);
        p->py = MIN(MAX(p->py, 1), c->height - 2);
    }

    /* Pick a location */
    /* Players should NEVER be placed on top of other stuff */
    /* Simply move the player away until a proper location is found */
    /* If no location can be found (VERY unlikely), then simply use the initial location */
    for (i = 0; i < 3000; i++)
    {
        /* Increase distance (try 10 times for each step) */
        d = (i + 9) / 10;

        /* Pick a location (skip LOS test) */
        if (!scatter(c, &y, &x, p->py, p->px, d, false)) continue;

        /* Require an "empty" floor grid */
        if (square_isemptyfloor(c, y, x))
        {
            /* Set the player's location */
            p->py = y;
            p->px = x;

            break;
        }
    }

    /* Hack -- set previous player location */
    p->old_py = p->py;
    p->old_px = p->px;

    /* Add the player */
    c->squares[p->py][p->px].mon = 0 - id;

    /* Redraw */
    square_light_spot(c, p->py, p->px);

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

    /* Update and redraw stuff (all of these are probably not needed...) */

    /* Update stuff */
    p->upkeep->update |= (PU_BONUS);

    /* Fully update the visuals (and monster distances) */
    p->upkeep->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

    /* Redraw dungeon */
    p->upkeep->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

    /* Redraw "statusy" things */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_MONSTER | PR_MONLIST | PR_ITEMLIST);

    /* MAngband */
    p->upkeep->redraw |= (PR_SPELL | PR_PLUSSES);

    /* This guy is alive now */
    p->alive = true;
}


static void player_admin(struct player *p)
{
    /* Hack -- set Dungeon Master flags */
#ifdef DEBUG_MODE
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
static void quickstart_roll(struct player *p, bool character_existed, byte *pridx, byte *pcidx,
    byte *psex, bool *old_history, s16b *stat_roll)
{
    /* A character existed in the savefile */
    if (character_existed)
    {
        int i;

        /* Use previous info */
        *pridx = p->race->ridx;
        *pcidx = p->clazz->cidx;
        *psex = p->psex;
        *old_history = true;

        /* Use point-based roller with previous birth stats */
        for (i = 0; i < STAT_MAX; i++) stat_roll[i] = p->stat_birth[i];
        stat_roll[STAT_MAX] = BR_POINTBASED;
    }

    /* New character */
    else
    {
        /* Roll a male half-troll warrior */
        *pridx = 7;
        *pcidx = 0;
        *psex = 1;
        *old_history = false;

        /* Use standard roller with STR CON DEX WIS INT as stat order */
        stat_roll[0] = STAT_STR;
        stat_roll[1] = STAT_CON;
        stat_roll[2] = STAT_DEX;
        stat_roll[3] = STAT_WIS;
        stat_roll[4] = STAT_INT;
        stat_roll[STAT_MAX] = BR_NORMAL;
    }
}


/*
 * Set the savefile name.
 */
bool savefile_set_name(struct player *p, char *savefile, const char *fname)
{
    char path[128];

    player_safe_name(path, sizeof(path), fname);

    /* Error */
    if (strlen(path) > MAX_NAME_LEN)
    {
        if (p) Destroy_connection(p->conn, "Your name is too long!");
        return false;
    }

    /* Build the filename */
    path_build(savefile, MSG_LEN, ANGBAND_DIR_SAVE, path);

    return true;
}


/*
 * Handle dynastic quick start creation
 *
 * Returns 1 if quick start is possible, 0 if quick start is not possible, -1 if an error occurs.
 */
static int quickstart_ok(struct player *p, const char *name, int conn, bool no_recall)
{
    char previous[NORMAL_WID];

    /* Get last incarnation */
    my_strcpy(previous, name, sizeof(previous));
    if (!get_previous_incarnation(previous, sizeof(previous))) return 0;

    /* Clear old information */
    init_player(p, conn, false, no_recall);

    /* Copy his name */
    my_strcpy(p->name, previous, sizeof(p->name));

    /* Verify his name and create a savefile name */
    if (!savefile_set_name(p, p->savefile, p->name)) return -1;

    /* Try to load the savefile */
    p->is_dead = true;
    if (!(p->savefile[0] && file_exists(p->savefile)))
    {
        /* Last incarnation: if "Foo I" doesn't exist, try "Foo" */
        my_strcpy(p->name, strip_suffix(previous), sizeof(p->name));
        if (!savefile_set_name(p, p->savefile, p->name)) return -1;
        if (!(p->savefile[0] && file_exists(p->savefile))) return 0;
    }
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
    byte ridx, byte cidx, byte psex, s16b* stat_roll, bool start_kit, bool no_recall)
{
    struct player *p;
    int i;
    bool character_existed = false;
    bool old_history = false;

    /* Do some consistency checks */
    if (ridx >= player_rmax()) ridx = 0;
    if (cidx >= player_cmax()) cidx = 0;
    if (psex >= MAX_SEXES) psex = SEX_FEMALE;

    /* Allocate player and set pointer */
    p = mem_zalloc(sizeof(struct player));
    player_set(id, p);

    /* Handle dynastic quick start */
    if (stat_roll[STAT_MAX] == BR_QDYNA)
    {
        int ret = quickstart_ok(p, name, conn, no_recall);

        if (ret == -1)
        {
            cleanup_player(p);
            mem_free(p);
            player_set(id, NULL);
            return NULL;
        }
        quickstart_roll(p, (ret? true: false), &ridx, &cidx, &psex, &old_history, stat_roll);
    }

    /* Clear old information */
    init_player(p, conn, old_history, no_recall);

    /* Copy his name */
    my_strcpy(p->name, name, sizeof(p->name));
    my_strcpy(p->pass, pass, sizeof(p->pass));

    /* DM powers? */
    player_admin(p);

    /* Verify his name and create a savefile name */
    if (!savefile_set_name(p, p->savefile, p->name))
    {
        cleanup_player(p);
        mem_free(p);
        player_set(id, NULL);
        return NULL;
    }

    /*** Try to load the savefile ***/

    p->is_dead = true;

    /* Try loading */
    if (file_exists(p->savefile))
    {
        if (!load_player(p))
        {
            cleanup_player(p);
            mem_free(p);
            player_set(id, NULL);
            return NULL;
        }

        /* Player is dead */
        if (p->is_dead)
        {
            /* A character existed in this savefile. */
            character_existed = true;
        }
    }

    /* No living character loaded */
    if (p->is_dead)
    {
        /* Make new player */
        p->is_dead = false;

        /* Handle quick start */
        if (stat_roll[STAT_MAX] == BR_QUICK)
            quickstart_roll(p, character_existed, &ridx, &cidx, &psex, &old_history, stat_roll);

        /* Hack -- rewipe the player info if load failed */
        init_player(p, conn, old_history, no_recall);

        /* Copy his name and connection info */
        my_strcpy(p->name, name, sizeof(p->name));
        my_strcpy(p->pass, pass, sizeof(p->pass));

        /* Reprocess his name */
        if (!savefile_set_name(p, p->savefile, p->name))
        {
            cleanup_player(p);
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
        if (!old_history) get_history(p);

        roll_hp(p);

        /* Embody */
        player_embody(p);

        /* Give the player some money */
        get_money(p, no_recall);

        /* Outfit the player, if they can sell the stuff */
        player_outfit(p, start_kit, no_recall);
        player_outfit_dm(p);

        /* Now try wielding everything */
        wield_all(p);

        /* Dragon */
        if (player_has(p, PF_DRAGON))
        {
            poly_dragon(p, false);
            get_bonuses(p);
        }

        /* Set his location, panel, etc. */
        player_setup(p, id, account, no_recall);

        /* Add new starting message */
        history_add_unique(p, "Began the quest to destroy Morgoth", HIST_PLAYER_BIRTH);

        /* Give the DM full knowledge */
        if (is_dm_p(p))
        {
            /* Every item in the game */
            for (i = 0; i < z_info->k_max; i++) p->kind_everseen[i] = 1;
            for (i = 0; i < z_info->e_max; i++) p->ego_everseen[i] = 1;

            /* Every monster in the game */
            for (i = 1; i < z_info->r_max; i++)
            {
                p->lore[i].pseen = 1;
                p->lore[i].pkills = z_info->max_depth;
                lore_update(&r_info[i], &p->lore[i]);
            }

            /* Every rune in the game */
            player_learn_everything(p);
        }

        /* Success */
        return p;
    }

    /* Loading succeeded */
    player_setup(p, id, account, no_recall);
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

    /* Seed for flavors */
    seed_flavor = randint0(0x10000000);
    flavor_init();

    /* Seed for wilderness layout */
    seed_wild = randint0(0x10000000);

    /* Hack -- enter the world */
    ht_reset(&turn);
    ht_add(&turn, 1);

    /* First player's ID should be 1 */
    player_id = 1;

    /* Initialize the stores */
    store_reset();
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
