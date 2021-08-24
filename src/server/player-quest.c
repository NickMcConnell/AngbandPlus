/*
 * File: player-quest.c
 * Purpose: All quest-related code
 *
 * Copyright (c) 2013 Angband developers
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


/*
 * Array of quests
 */
struct quest *quests;


/*
 * Parsing functions for quest.txt
 */
static enum parser_error parse_quest_name(struct parser *p)
{
    const char *name = parser_getstr(p, "name");
    struct quest *h = parser_priv(p);
    struct quest *q = mem_zalloc(sizeof(*q));

    q->next = h;
    parser_setpriv(p, q);
    q->name = string_make(name);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_quest_level(struct parser *p)
{
    struct quest *q = parser_priv(p);

    my_assert(q);
    q->level = parser_getuint(p, "level");
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_quest_race(struct parser *p)
{
    struct quest *q = parser_priv(p);
    const char *name = parser_getstr(p, "race");

    my_assert(q);
    q->race = lookup_monster(name);
    if (!q->race) return PARSE_ERROR_INVALID_MONSTER;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_quest_number(struct parser *p)
{
    struct quest *q = parser_priv(p);

    my_assert(q);
    q->max_num = parser_getuint(p, "number");
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_quest(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_quest_name);
    parser_reg(p, "level uint level", parse_quest_level);
    parser_reg(p, "race str race", parse_quest_race);
    parser_reg(p, "number uint number", parse_quest_number);

    return p;
}


static errr run_parse_quest(struct parser *p)
{
    return parse_file_quit_not_found(p, "quest");
}


static errr finish_parse_quest(struct parser *p)
{
    struct quest *quest, *next = NULL;
    int count;

    /* Scan the list for the max id */
    z_info->quest_max = 0;
    quest = parser_priv(p);
    while (quest)
    {
        z_info->quest_max++;
        quest = quest->next;
    }

    /* Allocate the direct access list and copy the data to it */
    quests = mem_zalloc(z_info->quest_max * sizeof(*quest));
    count = z_info->quest_max - 1;
    for (quest = parser_priv(p); quest; quest = next, count--)
    {
        memcpy(&quests[count], quest, sizeof(*quest));
        quests[count].index = count;
        next = quest->next;
        if (count < z_info->quest_max - 1) quests[count].next = &quests[count + 1];
        else quests[count].next = NULL;
        mem_free(quest);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_quest(void)
{
    int idx;

    /* Paranoia */
    if (!quests) return;

    for (idx = 0; idx < z_info->quest_max; idx++)
        string_free(quests[idx].name);
    mem_free(quests);
}


struct file_parser quests_parser =
{
    "quest",
    init_parse_quest,
    run_parse_quest,
    finish_parse_quest,
    cleanup_quest
};


/*
 * Check if the given level is a quest level.
 */
bool is_quest(int level)
{
    int i;

    for (i = 0; i < z_info->quest_max; i++)
    {
        if (quests[i].level == level) return true;
    }

    return false;
}


/*
 * Destroys the area around the winning player.
 * This is necessary to put the player, which will lose all true artifacts (and probably end up
 * half-naked), in safety.
 */
static void crumble_angband(struct player *p, struct chunk *c, struct loc *grid)
{
    int k, j;
    int notice[MAX_PLAYERS];
    int count = 0;
    struct loc begin, end;
    struct loc_iterator iter;

    loc_init(&begin, p->grid.x - 50, p->grid.y - 50);
    loc_init(&end, p->grid.x + 50, p->grid.y + 50);
    loc_iterator_first(&iter, &begin, &end);

    /* Huge area of effect */
    do
    {
        /* Skip illegal grids */
        if (!square_in_bounds_fully(c, &iter.cur)) continue;

        /* Extract the distance */
        k = distance(&p->grid, &iter.cur);

        /* Stay in the circle of death */
        if (k > 50) continue;

        /* Lose room and vault */
        sqinfo_off(square(c, &iter.cur)->info, SQUARE_ROOM);
        sqinfo_off(square(c, &iter.cur)->info, SQUARE_VAULT);
        sqinfo_off(square(c, &iter.cur)->info, SQUARE_NO_TELEPORT);
        if (square_ispitfloor(c, &iter.cur)) square_clear_feat(c, &iter.cur);

        /* Lose light */
        square_unglow(c, &iter.cur);
        square_forget_all(c, &iter.cur);
        square_light_spot(c, &iter.cur);

        /* Hack -- notice player */
        if (square(c, &iter.cur)->mon < 0)
        {
            /* Notice the player later */
            notice[count] = 0 - square(c, &iter.cur)->mon;
            count++;

            /* Do not hurt this grid */
            continue;
        }

        /* Hack -- skip the epicenter */
        if (player_is_at(p, &iter.cur)) continue;

        /* Hack -- skip Morgoth (he will be removed later) */
        if (loc_eq(&iter.cur, grid)) continue;

        /* Delete the monster (if any) */
        delete_monster(c, &iter.cur);
        if (square_ispitfloor(c, &iter.cur)) square_clear_feat(c, &iter.cur);

        /* Don't remove stairs */
        if (square_isstairs(c, &iter.cur)) continue;

        /* Destroy any grid that isn't a permanent wall */
        if (!square_isunpassable(c, &iter.cur))
        {
            /* Delete objects */
            square_forget_pile_all(c, &iter.cur);
            square_excise_pile(c, &iter.cur);
            square_destroy(c, &iter.cur);
        }
    }
    while (loc_iterator_next(&iter));

    /* Hack -- update players */
    for (j = 0; j < count; j++)
    {
        struct player *player = player_get(notice[j]);

        /* Message */
        msg(player, "The ground shakes violently as the fortress of Angband starts to crumble down...");

        /* Fully update the visuals */
        player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Redraw */
        player->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }
}


/*
 * Check if this (now dead) monster is a quest monster, and act appropriately
 */
int quest_check(struct player *p, struct chunk *c, const struct monster *m)
{
    int winners = 0;
    int i;

    /* Hack -- killing Morgoth marks some players as "winners" */
    if (m->race->base != lookup_monster_base("Morgoth")) return -1;

    /* A bad day for evil... */
    crumble_angband(p, c, &((struct monster *)m)->grid);

    /* Total winners */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);
        bool party_win = (party_share_with(p, p->party, player) && (player->lev >= 40) &&
            mflag_has(player->mflag[m->midx], MFLAG_HURT));

        /* Make the killer a total winner */
        /* Make high level party members on the same level total winners... */
        /* ... only if they participated in the fight! */
        /* Skip players that are already total winners */
        if (((player == p) || party_win) && !player->total_winner)
        {
            struct object *obj = player->gear, *next;

            /* Total winner */
            player->total_winner = 1;

            /* Redraw the "title" */
            player->upkeep->redraw |= (PR_TITLE);

            /* Congratulations */
            msg(player, "*** CONGRATULATIONS ***");
            msg(player, "You have won the game!");
            msg(player, "You may retire when you are ready.");

            /* Know inventory and home items upon victory */
            death_knowledge(player);

            /* Winner dump */
            my_strcpy(player->death_info.died_from, "winner", sizeof(player->death_info.died_from));
            player_dump(player, true);

            /* Set his retire_timer if necessary */
            if (cfg_retire_timer >= 0) player->retire_timer = cfg_retire_timer;

            /* Winners don't keep their true artifacts */
            while (obj)
            {
                next = obj->next;

                if (true_artifact_p(obj))
                {
                    char o_name[NORMAL_WID];

                    /* Message */
                    object_desc(player, o_name, sizeof(o_name), obj, ODESC_BASE);
                    msg(player, "The %s fades into the air!", o_name);

                    /* Preserve any true artifact */
                    preserve_artifact_aux(obj);
                    history_lose_artifact(player, obj);

                    /* Decrease the item, optimize. */
                    gear_excise_object(player, obj);
                    object_delete(&obj);
                }

                obj = next;
            }

            /* Notice */
            player->upkeep->notice |= (PN_COMBINE);

            /* Redraw */
            player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

            /* Hack -- instantly retire any new winners if necessary */
            if (cfg_retire_timer == 0) do_cmd_suicide(player);

            /* If not, generate the rewards for that player */
            else winners++;
        }
    }

    return winners;
}


/*
 * Check if a level is an active "quest" level
 */
bool is_quest_active(struct player *p, int level)
{
    int i;

    for (i = 0; i < z_info->quest_max; i++)
    {
        struct monster_lore *lore = get_lore(p, quests[i].race);

        /* Check depth and kill */
        if ((quests[i].level == level) && !lore->pkills) return true;
    }

    return false;
}


/*
 * Get a quest
 */
void start_quest(struct player *p)
{
    struct quest *quest = &p->quest;
    struct monster_race *race;
    int min = z_info->max_depth - 1, max = 1, min_depth, max_depth;
    int max_num, kills = 0;
    u16b i;

    /* Quest already taken? */
    if (quest->race)
    {
        msg(p, "You still need to kill %d of the %s race!", quest->max_num - quest->cur_num,
            quest->race->name);
        return;
    }

    /* Compute min depth and max depth for monsters that can be generated */
    for (i = 1; i < z_info->r_max; i++)
    {
        race = &r_info[i];
        if (monster_is_unique(race) || (race->level < 1) || (race->level > z_info->max_depth - 1) ||
            (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
            (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)) continue;
        if (race->level < min) min = race->level;
        if (race->level > max) max = race->level;
    }

    /* Get a quest - try to roughly match player max depth */
    min_depth = MIN(max, MAX(min, p->max_depth - 5));
    max_depth = MIN(max, MAX(min, p->max_depth + 5));

    /* Skip uniques and monsters that can't be generated */
    do
    {
        race = &r_info[randint1(z_info->r_max - 1)];
    }
    while ((race->level < min_depth) || (race->level > max_depth) || monster_is_unique(race) ||
        (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
        (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters));

    /* Monster race */
    quest->race = race;

    /* Set a base required number */
    max_num = 2 + randint1(5);

    /* Increase it for common monsters (based on player kills) */
    for (i = 1; i < z_info->r_max; i++)
        kills += r_info[i].lore.tkills;
    if (race->lore.tkills > (kills + z_info->r_max - 2) / (z_info->r_max - 1))
        max_num = max_num + 2 + randint1(5);

    /* Number required */
    quest->max_num = max_num;

    /* Set the timer */
    quest->timer = 10000;

    msg(p, "Find and kill %d of the %s race!", quest->max_num, race->name);
}


void process_quest(struct player *p)
{
    struct quest *quest = &p->quest;

    if (quest->timer > 0)
    {
        quest->timer--;
        if (quest->timer == 0)
        {
            /* Failed quest */
            msg(p, "You have failed your quest.");
            quest->race = NULL;
            quest->cur_num = 0;
            quest->max_num = 0;
        }
    }
}


void end_quest(struct player *p, struct chunk *c, const struct monster *m)
{
    struct quest *quest = &p->quest;

    if (quest->race && (quest->race == m->race))
    {
        quest->cur_num++;

        /* Note completed quests */
        if (quest->cur_num == quest->max_num)
        {
            quark_t quark_quest;
            char buf[MSG_LEN];

            msg(p, "You have just completed your current quest.");
            strnfmt(buf, sizeof(buf), "%s has won the %s quest!", p->name, m->race->name);
            msg_broadcast(p, buf, MSG_BROADCAST_QUEST);

            /* Quest is completed, clear it */
            quest->race = NULL;
            quest->cur_num = 0;
            quest->max_num = 0;

            /* Generate the reward */
            quark_quest = quark_add(format("%s quest", m->race->name));
            acquirement(p, c, 1, quark_quest);
        }
        else
            msg(p, "%d more to go!", quest->max_num - quest->cur_num);
    }
}
