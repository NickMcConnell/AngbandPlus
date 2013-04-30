/*
 * File: xtra2.c
 * Purpose: Targeting, sorting, panel update
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
#include "cmds.h"
#include "files.h"
#include "generate.h"
#include "history.h"
#include "monster/mon-make.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/pval.h"
#include "s-spells.h"
#include "target.h"
#include "wilderness.h"


/* Find player on arena "a", who is not player "Ind" */
static int pick_arena_opponent(int Ind, int a)
{
    int i;

    /* Count other players in this arena */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        if (player_get(i)->arena_num == a)
        {
            /* Found someone */
            if (Ind != i) return i;
        }
    }

    /* No one found */
    return -1;
}


static void msg_broadcast_arena(int loser, int winner, const char *msg)
{
    int i;

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Skip the specified players */
        if (i == loser) continue;
        if (i == winner) continue;

        /* Tell this one */
        msg_print_aux(player_get(i), msg, MSG_CHAT);
    }

    /* Send to console */
    console_print((char*)msg, 0);
}


/* Cleanup after PvP Fight in the arena */
static void evacuate_arena(int Ind)
{
    char buf[100];
    player_type *p_ptr = player_get(Ind), *q_ptr;
    int tmp_id = pick_arena_opponent(Ind, p_ptr->arena_num);

    /* Paranoia */
    if ((tmp_id == -1) || (tmp_id == Ind)) return;

    q_ptr = player_get(tmp_id);

    /* Friendship */
    pvp_check(q_ptr, p_ptr, PVP_REMOVE, TRUE, FEAT_NONE);
    pvp_check(p_ptr, q_ptr, PVP_REMOVE, TRUE, FEAT_NONE);

    /* Messages */
    msg(p_ptr, "You lose consciousness.");
    msg(q_ptr, "You knock %s out.", p_ptr->name);

    /* Tell everyone about the outcome */
    strnfmt(buf, sizeof(buf), "%s was defeated by %s in the arena.", p_ptr->name, q_ptr->name);
    msg_broadcast_arena(Ind, tmp_id, buf);

    /* Remove potential bad effects */
    player_clear_timed(p_ptr, TMD_POISONED, TRUE);
    player_clear_timed(q_ptr, TMD_POISONED, TRUE);

    /* Give hit points and mana points back */
    restore_hp(p_ptr);
    restore_sp(p_ptr);
    restore_hp(q_ptr);
    restore_sp(q_ptr);

    /* Teleport */
    p_ptr->arena_num = -1;
    teleport_player(p_ptr, 1);
    q_ptr->arena_num = -1;
    teleport_player(q_ptr, 1);

    /* Messages */
    msg(p_ptr, "You recover outside the arena.");
    msg(q_ptr, "You gloriously leave the arena.");
}


struct cmp_val
{
    struct player *p;
    int pos;
};


static int cmp_value(const void *a, const void *b)
{
    const struct cmp_val *pa = a;
    const struct cmp_val *pb = b;
    s32b va = 0, vb = 0;

    if (pa->p->inventory[pa->pos].tval)
        va = object_value(pa->p, &pa->p->inventory[pa->pos], 1);
    if (pb->p->inventory[pb->pos].tval)
        vb = object_value(pb->p, &pb->p->inventory[pb->pos], 1);

    if (va > vb) return -1;
    if (va < vb) return 1;
    return 0;
}


static void player_strip(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    size_t i;
    struct cmp_val *inven = mem_zalloc(sizeof(*inven) * ALL_INVEN_TOTAL);

    /* Drop gold if player has any */
    if (p_ptr->alive && p_ptr->au)
    {
        /* Put the player's gold in the overflow slot */
        object_prep(&p_ptr->inventory[INVEN_PACK], lookup_kind(TV_GOLD, SV_GOLD),
            0, MINIMISE);

        /* Drop no more than 32000 gold */
        if (p_ptr->au > 32000) p_ptr->au = 32000;

        /* Set the amount */
        p_ptr->inventory[INVEN_PACK].pval[DEFAULT_PVAL] = p_ptr->au;

        /* No more gold */
        p_ptr->au = 0;
    }

    /* Scan player's inventory */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        inven[i].p = p_ptr;
        inven[i].pos = i;
    }

    /* Sort the player's inventory according to value */
    sort(inven, ALL_INVEN_TOTAL, sizeof(*inven), cmp_value);

    /* Starting with the most valuable, drop things one by one */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &p_ptr->inventory[inven[i].pos];
        int amt = o_ptr->number;
        byte drop_state;

        /* Make sure we have an object */
        if (!o_ptr->kind) continue;

        /* If we committed suicide, drop nothing */
        if (!p_ptr->alive)
        {
            /* Preserve any artifact */
            preserve_artifact_aux(o_ptr);
            if (o_ptr->artifact) history_lose_artifact(p_ptr, o_ptr);

            continue;
        }

        /* Drop this one */
        drop_state = drop_near(p_ptr, cave_get(p_ptr->depth), o_ptr, 0, p_ptr->py,
            p_ptr->px, FALSE);

        /* Preserve any artifact */
        if (drop_state == DROP_ERROR)
        {
            preserve_artifact_aux(o_ptr);
            history_lose_artifact(p_ptr, o_ptr);
        }

        /* Erase the empty slot */
        object_wipe(o_ptr);
    }

    mem_free(inven);

    /* Save quiver size */
    save_quiver_size(p_ptr, TRUE);
}


static void player_funeral(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Clear his houses */
    for (i = 0; i < num_houses; i++)
    {
        /* Is house owned? */
        if (house_owned_by(p_ptr, i))
        {
            /* House is no longer owned */
            reset_house(i, houses[i].depth, houses[i].door_y, houses[i].door_x);
        }
    }

    /* Remove him from his party */
    if (p_ptr->party)
    {
        /* He leaves */
        party_leave(Ind);
    }

    /* Kill him */
    if (!p_ptr->alive) p_ptr->is_dead = TRUE;

    /* One less player here */
    leave_depth(p_ptr);

    /* Remove him from the player name database */
    remove_player_name(p_ptr->name);

    /* Put him on the high score list */
    enter_score(Ind, &p_ptr->death_info.time);

    /* Perform a full redraw */
    message_flush(p_ptr);
    refresh_stuff(Ind);

    /* Send any remaining information over the network */
    Net_output_p(Ind);

    /* Get rid of him */
    p_ptr->leaving = TRUE;

    /* Display the winner crown */
    if (p_ptr->total_winner) Send_winner(p_ptr);

    /* Display the tombstone */
    else Send_death_cause(p_ptr);
}


void player_dump(int Ind)
{
    char dumpname[42];

    /* Save the server-side character dump (used by the online ladder) */
    strnfmt(dumpname, sizeof(dumpname), "%s-%s.txt", player_get(Ind)->name,
        ht_show(&turn));
    file_character_server(Ind, ANGBAND_DIR_USER, dumpname, TRUE);

    /* Save a client-side character dump */
    strnfmt(dumpname, sizeof(dumpname), "%s.txt", player_get(Ind)->name);
    file_character_server(Ind, ANGBAND_DIR_APEX, dumpname, FALSE);
}


/*
 * Handle the death of a player and drop their stuff.
 *
 * changed so players remain in the team when killed
 * changed so when leader ghosts perish the team is disbanded
 */
void player_death(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    char buf[MSG_LEN];
    const char *p = get_title(p_ptr);
    bool death_spell = (!strcmp(p_ptr->died_from, "the spell of Undead Form") ||
        !strcmp(p_ptr->died_from, "the Death spell"));
    bool perma_death;
    int i;
    const char *brave;

    /* Hack -- Don't die in Arena! */
    if (p_ptr->arena_num != -1)
    {
        p_ptr->is_dead = FALSE;
        evacuate_arena(Ind);
        return;
    }

    /* Hack -- Note death */
    if (!p_ptr->ghost)
    {
        msgt(p_ptr, MSG_DEATH, "You die.");
        strnfmt(buf, sizeof(buf), "Was killed by %s", p_ptr->died_from);
        history_add(p_ptr, buf, HISTORY_PLAYER_DEATH, NULL);
    }
    else msgt(p_ptr, MSG_DEATH, "Your incorporeal body fades away - FOREVER.");
    message_flush(p_ptr);

    /* Tell everyone he died */
    if (p_ptr->ghost)
    {
        strnfmt(buf, sizeof(buf), "%s %s's ghost was destroyed by %s.", p, p_ptr->name,
            p_ptr->died_from);
    }
    else if (p_ptr->alive)
    {
        if (OPT_P(p_ptr, birth_no_ghost))
        {
            if (OPT_P(p_ptr, birth_ironman)) brave = "The iron";
            else brave = "The brave";
        }
        else if (OPT_P(p_ptr, birth_ironman)) brave = "The hardcore";
        else brave = "The unfortunate";
        strnfmt(buf, sizeof(buf), "%s %s %s %s.", brave, p, p_ptr->name, p_ptr->died_flavor);
    }
    else if (!strcmp(p_ptr->died_from, "divine wrath"))
        strnfmt(buf, sizeof(buf), "%s was killed by divine wrath.", p_ptr->name);
    else if (!p_ptr->total_winner)
        strnfmt(buf, sizeof(buf), "%s committed suicide.", p_ptr->name);
    else
        strnfmt(buf, sizeof(buf),
            "The unbeatable %s has retired to a warm, sunny climate.", p_ptr->name);

    /* Tell the players - handle the secret_dungeon_master option */
    if (!(p_ptr->dm_flags & DM_SECRET_PRESENCE))
    {
        /* Don't broadcast level 1 suicides */
        if (!strstr(buf, "suicide") || (p_ptr->lev > 1))
            msg_broadcast(p_ptr, buf);
    }

    /* Characters get a character dump on the server */
    if (!p_ptr->ghost && p_ptr->alive)
    {
        /* Character dump here, before we start dropping items */
        player_dump(Ind);
    }

    /*
     * Handle permanent death:
     * - all characters have a chance to die permanently
     *   (except when using the spells of Undead Form or Death)
     * - no ghost characters (except Necromancers that can use Undead Form)
     * - Dragon characters
     * - ghosts
     * - suiciding characters
     */
    perma_death = ((magik(p_ptr->lives) && !death_spell) ||
        ((OPT_P(p_ptr, birth_no_ghost) || OPT_P(p_ptr, birth_fruit_bat) || cfg_no_ghost) &&
            !player_can_undead(p_ptr)) ||
        player_has(p_ptr, PF_DRAGON) ||
        p_ptr->ghost ||
        !p_ptr->alive);

    /*
     * Drop every item (including gold) for:
     * - all characters that will permanently die
     * - all other characters except Necromancers that can use Undead Form
     * Note that items from suiciding characters never drop, they vanish instead...
     */
    if (perma_death || !player_can_undead(p_ptr)) player_strip(Ind);

    /* Tell him */
    if (!p_ptr->ghost && p_ptr->alive)
        msg(p_ptr, "You have been killed by %s.", p_ptr->died_from);

    /* Handle permanent death */
    if (perma_death)
    {
        player_funeral(Ind);

        /* Done */
        return;
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx) do_cmd_poly(p_ptr, 0, FALSE, TRUE);

    /* Cancel current effects */
    for (i = 0; i < TMD_MAX; i++) player_clear_timed(p_ptr, i, TRUE);

    /* Turn him into a ghost */
    /* Treat the spells of Undead Form and Death as a special cases */
    set_ghost_flag(p_ptr, (death_spell? 2: 1), TRUE);

    /* Give him his hit points and mana points back */
    restore_hp(p_ptr);
    restore_sp(p_ptr);

    /* Teleport him */
    teleport_player_aux(p_ptr, 200, TRUE);

    /* He is carrying nothing */
    if (!player_can_undead(p_ptr)) p_ptr->total_weight = 0;

    /* Feed him (maybe he died from starvation) */
    player_set_food(p_ptr, PY_FOOD_MAX - 1);

    /* Remove the death flag */
    p_ptr->is_dead = FALSE;

    /* Cancel any WOR spells */
    p_ptr->word_recall = 0;
    p_ptr->deep_descent = 0;

    /* Redraw the state (later) */
    p_ptr->redraw |= (PR_STATE);

    /* He is carrying nothing */
    if (!player_can_undead(p_ptr)) p_ptr->inven_cnt = 0;

    /* Update */
    p_ptr->update |= (PU_BONUS | PU_MANA | PU_TORCH);

    /* Redraw */
    p_ptr->redraw |= (PR_BASIC);

    /* Notice */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);

    /* Redraw */
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_SPELL);
}

/*
 * Resurrect a player
 */
void resurrect_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Hack -- The dungeon master cannot resurrect */
    if (is_dm_p(p_ptr)) return;

    /* Log event */
    history_add(p_ptr, "Resurrected", HISTORY_PLAYER_REVIVE, NULL);

    /* Message */
    msg(p_ptr, "You feel life return to your body.");

    /* Treat the spells of Undead Form and Death as special cases */
    if (p_ptr->ghost == 2)
    {
        /* Reset ghost flag */
        set_ghost_flag(p_ptr, 0, TRUE);

        /* Redraw */
        p_ptr->redraw |= (PR_TITLE);
        cave_light_spot(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

        return;
    }

    /* Reset ghost flag */
    set_ghost_flag(p_ptr, 0, TRUE);

    /* Lose some experience */
    player_exp_lose(p_ptr, p_ptr->max_exp / 2, TRUE);

    /* Lose some CON */
    if (magik(p_ptr->lives))
    {
        /* Sometimes decrease it permanently */
        msg(p_ptr, "The process leaves you extremely weak and tired!");
        player_stat_dec(p_ptr, A_CON, TRUE);
    }
    else
    {
        /* Otherwise just drain it */
        msg(p_ptr, "The process leaves you somewhat exhausted...");
        player_stat_dec(p_ptr, A_CON, FALSE);
    }

    /* Increment number of resurrections */
    p_ptr->lives++;

    /* Redraw */
    p_ptr->redraw |= (PR_BASIC | PR_SPELL);
    cave_light_spot(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

    /* Update */
    p_ptr->update |= (PU_BONUS);

    /* Reset the death info */
    WIPE(&p_ptr->death_info, struct player_death_info);
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(struct player *p, int wy, int wx)
{
    int screen_hgt, screen_wid;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    /* Verify wy, adjust if needed */
    if (wy > DUNGEON_HGT - screen_hgt) wy = DUNGEON_HGT - screen_hgt;
    if (wy < 0) wy = 0;

    /* Verify wx, adjust if needed */
    if (wx > DUNGEON_WID - screen_wid) wx = DUNGEON_WID - screen_wid;
    if (wx < 0) wx = 0;

    /* React to changes */
    if ((p->offset_y != wy) || (p->offset_x != wx))
    {
        /* Save wy, wx */
        p->offset_y = wy;
        p->offset_x = wx;

        /* Update stuff */
        p->update |= (PU_MONSTERS);

        /* Redraw map */
        p->redraw |= (PR_MAP);
      
        /* Changed */
        return (TRUE);
    }

    /* No change */
    return (FALSE);
}


/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
bool adjust_panel(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    bool changed = FALSE;
    int wx, wy;
    int screen_hgt, screen_wid;

    screen_hgt = p_ptr->screen_rows / p_ptr->tile_hgt;
    screen_wid = p_ptr->screen_cols / p_ptr->tile_wid;

    wy = p_ptr->offset_y;
    wx = p_ptr->offset_x;

    /* Adjust as needed */
    while (y >= wy + screen_hgt) wy += screen_hgt / 2;
    while (y < wy) wy -= screen_hgt / 2;

    /* Adjust as needed */
    while (x >= wx + screen_wid) wx += screen_wid / 2;
    while (x < wx) wx -= screen_wid / 2;

    /* Use "modify_panel" */
    if (modify_panel(p_ptr, wy, wx)) changed = TRUE;

    return (changed);
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(struct player *p, int dir)
{
    bool changed = FALSE;
    int wx, wy;
    int screen_hgt, screen_wid;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    screen_hgt = p->screen_rows / p->tile_hgt;
    screen_wid = p->screen_cols / p->tile_wid;

    /* Shift by half a panel */
    wy = p->offset_y + ddy[dir] * screen_hgt / 2;
    wx = p->offset_x + ddx[dir] * screen_wid / 2;

    /* Use "modify_panel" */
    if (modify_panel(p, wy, wx)) changed = TRUE;

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
    if (centered && !p->running && (py != wy + panel_hgt))
        wy = py - panel_hgt;

    /* Scroll screen vertically when 3 grids from top/bottom edge */
    else if ((py < wy + 3) || (py >= wy + screen_hgt - 3))
        wy = py - panel_hgt;

    /* Scroll screen horizontally when off-center */
    if (centered && !p->running && (px != wx + panel_wid))
        wx = px - panel_wid;

    /* Scroll screen horizontally when 3 grids from left/right edge */
    else if ((px < wx + 3) || (px >= wx + screen_wid - 3))
        wx = px - panel_wid;

    /* Scroll if needed */
    modify_panel(p, wy, wx);
}


void verify_panel(struct player *p)
{
    verify_panel_int(p, OPT_P(p, center_player));
}


/*
 * Center the current panel.
 */
void center_panel(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    verify_panel_int(p_ptr, TRUE);
}


/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
int motion_dir(int y1, int x1, int y2, int x2)
{
    /* No movement required */
    if ((y1 == y2) && (x1 == x2)) return (5);

    /* South or North */
    if (x1 == x2) return ((y1 < y2) ? 2 : 8);

    /* East or West */
    if (y1 == y2) return ((x1 < x2) ? 6 : 4);

    /* South-east or South-west */
    if (y1 < y2) return ((x1 < x2) ? 3 : 1);

    /* North-east or North-west */
    if (y1 > y2) return ((x1 < x2) ? 9 : 7);

    /* Paranoia */
    return (5);
}


/*
 * Selects the recall depth.
 * Setting negative levels is now legal, assuming that the player has explored
 * the respective wilderness level.
 */
static bool set_recall_depth(struct player *p, quark_t note)
{
    int recall_depth = 0;
    unsigned char* inscription = (unsigned char*)quark_str(note);

    /* Default to the players maximum depth */
    p->recall_depth = p->max_depth;

    /* Check for a valid inscription */
    if (inscription == NULL) return FALSE;

    /* Scan the inscription for @R */
    while (*inscription != '\0')
    {
        if (*inscription == '@')
        {
            inscription++;

            if (*inscription == 'R')
            {
                /* A valid @R has been located */
                inscription++;

                /* Convert the inscription into a level index */
                recall_depth = atoi(inscription);

                /* Help avoid typos */
                if (recall_depth % 50)
                {
                    p->recall_depth = 0;
                    return FALSE;
                }
                recall_depth /= 50;
            }
        }
        inscription++;
    }

    /* Do some bounds checking/sanity checks */
    if ((recall_depth > p->max_depth) || (!recall_depth)) recall_depth = p->max_depth;

    /* If a wilderness level, verify that the player has visited here before */
    if (recall_depth < 0)
    {
        /* If the player has not visited here, set the recall depth to the town */
        if (!wild_is_explored(p, 0 - recall_depth)) recall_depth = 1;
    }

    p->recall_depth = recall_depth;
    return TRUE;
}


/* Recall routine */
void set_recall(struct player *p, quark_t note, bool fast)
{
    if ((cfg_no_recall || OPT_P(p, birth_ironman)) && !p->total_winner)
        msg(p, "The air around you becomes charged... but only for a moment.");
    else if (!p->word_recall)
    {
        set_recall_depth(p, note);
        if (fast)
            p->word_recall = (s16b)randint1(PY_MAX_LEVEL + 6 - p->lev) + (PY_MAX_LEVEL - p->lev);
        else
            p->word_recall = (s16b)randint1(20) + 14;
        msg(p, "The air around you becomes charged...");
        msg_misc(p, " is surrounded by a charged aura...");

        /* Redraw the state (later) */
        p->redraw |= (PR_STATE);
    }
    else
    {
        p->word_recall = 0;
        msg(p, "A tension leaves the air around you...");
        msg_misc(p, "'s charged aura disappears...");

        /* Redraw the state (later) */
        p->redraw |= (PR_STATE);
    }
}

/* Try to resurrect someone */
bool do_scroll_life(struct player *p)
{
    int x, y;

    for (y = -1; y <= 1; y++)
    {
        for (x = -1; x <= 1; x++)
        {
            int m_idx;

            if ((x == 0) && (y == 0)) continue;

            m_idx = cave_get(p->depth)->m_idx[p->py + y][p->px + x];

            if ((m_idx < 0) && (cave_floor_bold(p->depth, p->py + y, p->px + x)))
            {
                player_type *q_ptr = player_get(0 - m_idx);

                if (q_ptr->ghost && !player_can_undead(q_ptr))
                {
                    resurrect_player(0 - m_idx);
                    return TRUE;
                }
            }
        }
    }

    /* We did not resurrect anyone */
    return FALSE;
}


/*
 * Hack -- Since the framerate has been boosted by five times since version
 * 0.6.0 to make game movement more smooth, we return the old level speed
 * times five to keep the same movement rate.
 */
int level_speed(int Ind)
{
    if (Ind <= 0) return level_speeds[0] * 5;
    else return level_speeds[Ind] * 5;
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
int base_time_factor(struct player *p, int slowest)
{
    player_type *q_ptr;
    int i, dist, health, timefactor;
    bool los;

    /* If this is the initial call, reset all players time bubble check */
    if (!slowest)
    {
        for (i = 1; i < NumPlayers + 1; i++)
        {
            q_ptr = player_get(i);
            if (q_ptr) q_ptr->bubble_checked = FALSE;
        }
    }

    /* Normal time scale */
    timefactor = NORMAL_TIME;

    /* What's our percentage health? */
    health = (p->chp * 100) / p->mhp;

    /* Don't allow time to slow asymptotically towards infinity */
    if (health < MIN_TIME_SCALE) health = MIN_TIME_SCALE;

    /* Scale depending on health if HP are low enough */
    if (health <= p->other.hitpoint_warn * 10)
        timefactor = timefactor * ((double)health / 100);

    /* Resting speeds up time disregarding health time scaling */
    if (p->resting) timefactor = MAX_TIME_SCALE;

    /*
     * If this is a check for another player give way to their time
     * bubble if we aren't doing anything important
     */
    if (slowest && (timefactor == NORMAL_TIME))
    {
        /* If nothing in LoS */
        los = FALSE;
        for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
        {
            /* Check this monster */
            if ((p->mon_los[i] &&
                !cave_monster(cave_get(p->depth), i)->m_timed[MON_TMD_SLEEP]))
            {
                los = TRUE;
                break;
            }
        }
        if (!los)
        {
            /* We don't really care about our time */
            timefactor = MAX_TIME_SCALE;
        }
    }

    /* We have checked our time bubble */
    p->bubble_checked = TRUE;

    /* Check all other players within our range */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        q_ptr = player_get(i);

        /* Only check them if they haven't already been checked */
        if (q_ptr && !q_ptr->bubble_checked)
        {
            /* Skip him if he's on a different dungeon level */
            if (q_ptr->depth != p->depth) continue;

            /* How far away is he? */
            dist = distance(p->py, p->px, q_ptr->py, q_ptr->px);

            /* Skip him if he's too far away */
            if (dist > MAX_SIGHT) continue;

            /* Find the slowest time bubble chain we are part of */
            slowest = base_time_factor(q_ptr, timefactor);

            /* Use the slowest time bubble */
            if (slowest < timefactor) timefactor = slowest;
        }
    }

    return timefactor;
}


/*
 * Determine the given players current time factor.
 */
int time_factor(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int timefactor, scale;

    /* Normal time scale, 100% */
    scale = NORMAL_TIME;

    /* Paranoia! */
    if (!p_ptr) return scale;

    /* Forget all about time scaling in town */
    if (!p_ptr->depth) return scale;

    /* Running speeds up time */
    if (p_ptr->running) scale = RUNNING_FACTOR;

    /* Determine our time scaling factor */
    timefactor = base_time_factor(p_ptr, 0);

    /* Scale our time by our bubbles time factor */
    scale = scale * ((double)timefactor / 100);

    return scale;
}


/*
 * Dungeon master commands
 */


static void unstatic_level(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int num_on_depth = 0, i;

    /* Figure out how many players are currently on the level */
    for (i = 1; i <= NumPlayers; i++)
    {
        if (player_get(i)->depth == p_ptr->depth) num_on_depth++;
    }

    /*
     * Set the number of players on the level equal to the number of
     * currently connected players on the level.
     */
    players_on_depth[p_ptr->depth] = num_on_depth;
}


static void manual_design(int Ind, bool new_level)
{
    player_type *p_ptr = player_get(Ind);
    int i, y, x;

    /* Forbidden outside of the dungeon (for now) */
    if (p_ptr->depth < 0) return;

    /* Not in town or quest levels */
    if (!p_ptr->depth || is_quest(p_ptr->depth)) return;

    /* Level is already locked */
    if (players_on_depth[p_ptr->depth] == INHIBIT_DEPTH) return;

    msg(p_ptr, "Entering manual design mode...");

    /* Unstatic the level */
    unstatic_level(Ind);

    /* Recall all other players currently on the level immediately */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *q_ptr = player_get(i);

        if (i == Ind) continue;
        if (q_ptr->depth != p_ptr->depth) continue;

        q_ptr->word_recall = 0;
        recall_player(i);
    }

    /* Lock the level */
    players_on_depth[p_ptr->depth] = INHIBIT_DEPTH;

    /* New level */
    if (new_level)
    {
        /* Wipe the level completely */
        for (y = 1; y < DUNGEON_HGT - 1; y++)
        {
            for (x = 1; x < DUNGEON_WID - 1; x++)
            {
                /* Lose info */
                cave_get(p_ptr->depth)->info[y][x] = 0;
                p_ptr->cave->info[y][x] = 0;

                /* Delete monsters */
                delete_monster(p_ptr->depth, y, x);

                /* Turn into basic floor */
                cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_FLOOR);

                /* Delete objects */
                delete_object(p_ptr->depth, y, x);
            }
        }
        level_up_y[p_ptr->depth] = 0;
        level_up_x[p_ptr->depth] = 0;
        level_down_y[p_ptr->depth] = 0;
        level_down_x[p_ptr->depth] = 0;

        /* Enlighten the level */
        wiz_light(p_ptr, TRUE);
    }

    level_rand_y[p_ptr->depth] = 0;
    level_rand_x[p_ptr->depth] = 0;
}


/*
 * Static or unstatic a level
 */
static void master_level(int Ind, char* parms)
{
    player_type *p_ptr = player_get(Ind);

    /* Paranoia -- make sure the player is on a valid level */
    if (!cave_get(p_ptr->depth)) return;

    switch (parms[0])
    {
        /* Unstatic the level */
        case 'u':
        {
            unstatic_level(Ind);
            msg(p_ptr, "The level has been unstaticed.");
            break;
        }

        /* Static the level */
        case 's':
        {
            /* Increase the number of players on the dungeon masters level by one. */
            players_on_depth[p_ptr->depth]++;
            msg(p_ptr, "The level has been staticed.");
            break;
        }

        /* Enter manual design (new level) */
        case 'w':
        {
            manual_design(Ind, TRUE);
            break;
        }

        /* Enter manual design */
        case 'm':
        {
            manual_design(Ind, FALSE);
            break;
        }

        /* Exit manual design */
        case 'x':
        {
            int yy, xx;

            /* Level is not locked */
            if (players_on_depth[p_ptr->depth] != INHIBIT_DEPTH) break;

            /* Check level_up and level_down */
            if (!level_up_y[p_ptr->depth] && !level_up_x[p_ptr->depth])
                msg(p_ptr, "There is no down staircase on this level!");
            else if (!level_down_y[p_ptr->depth] && !level_down_x[p_ptr->depth])
                msg(p_ptr, "There is no up staircase on this level!");
            else
            {
                int tries = 10000;

                /* Set level_rand */
                while (tries > 0)
                {
                    tries--;

                    /* Pick a legal spot */
                    yy = rand_range(1, DUNGEON_HGT - 2);
                    xx = rand_range(1, DUNGEON_WID - 2);

                    /* Must be a "naked" floor grid */
                    if (cave_naked_bold(p_ptr->depth, yy, xx)) break;
                }

                /* Check level_rand */
                if (!tries)
                    msg(p_ptr, "There are no naked floor grids on this level!");
                else
                {
                    msg(p_ptr, "Exiting manual design mode...");

                    level_rand_y[p_ptr->depth] = yy;
                    level_rand_x[p_ptr->depth] = xx;

                    /* Hack -- clear players_on_depth */
                    players_on_depth[p_ptr->depth] = 0;

                    /* Save manually-designed dungeon level to file */
                    save_dungeon_special(p_ptr->depth);

                    /* Unlock the level */
                    players_on_depth[p_ptr->depth] = 1;
                }
            }

            break;
        }
    }
}


static void place_feature(int Ind, byte cur_feat)
{
    player_type *p_ptr = player_get(Ind);

    /* Can only place a staircase once */
    if ((cur_feat == FEAT_LESS) &&
        (level_down_y[p_ptr->depth] || level_down_x[p_ptr->depth]))
    {
        msg(p_ptr, "There is already an up staircase on this level!");
        return;
    }
    if ((cur_feat == FEAT_MORE) &&
        (level_up_y[p_ptr->depth] || level_up_x[p_ptr->depth]))
    {
        msg(p_ptr, "There is already a down staircase on this level!");
        return;
    }

    /* Remove a staircase */
    if (cave_isupstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px))
    {
        level_down_y[p_ptr->depth] = 0;
        level_down_x[p_ptr->depth] = 0;
    }
    if (cave_isdownstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px))
    {
        level_up_y[p_ptr->depth] = 0;
        level_up_x[p_ptr->depth] = 0;
    }

    /* Set feature */
    cave_set_feat(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px, cur_feat);

    /* Place a staircase */
    if (cur_feat == FEAT_LESS)
    {
        level_down_y[p_ptr->depth] = p_ptr->py;
        level_down_x[p_ptr->depth] = p_ptr->px;
    }
    if (cur_feat == FEAT_MORE)
    {
        level_up_y[p_ptr->depth] = p_ptr->py;
        level_up_x[p_ptr->depth] = p_ptr->px;
    }
}


static void get_rectangle(int depth, int y0, int x0, int *ymax, int *xmax)
{
    int y, x;

    /* Find the width of the rectangle to fill */
    for (x = x0; x < *xmax; x++)
    {
        /* Require a "clean" floor grid */
        if (!cave_clean_bold(depth, y0, x))
        {
            if (x < *xmax) *xmax = x;
            break;
        }
    }

    /* Find the height of the rectangle to fill */
    for (y = y0; y < *ymax; y++)
    {
        /* Require a "clean" floor grid */
        if (!cave_clean_bold(depth, y, x0))
        {
            if (y < *ymax) *ymax = y;
            break;
        }
    }
}


/*
 * Build walls and such
 */
static void master_build(int Ind, char* parms)
{
    player_type *p_ptr = player_get(Ind);
    static byte cur_feat = FEAT_FLOOR;

    /* Paranoia -- make sure the player is on a valid level */
    if (!cave_get(p_ptr->depth)) return;

    /* Place a feature at the player's location */
    if (!parms)
    {
        place_feature(Ind, cur_feat);
        return;
    }

    switch (parms[0])
    {
        /* Set Feature */
        case 'i':
        {
            byte feat = (byte)parms[1];

            /* Unauthorized features */
            if ((feat == FEAT_SWAMP) || (feat == FEAT_TOWN)) break;

            cur_feat = feat;
            break;
        }

        /* Place Feature */
        case 'f':
        {
            place_feature(Ind, cur_feat);
            break;
        }

        /* Draw Line */
        case 'l':
        {
            int dir = (int)parms[1];

            /* No lines of staircases */
            if ((cur_feat == FEAT_LESS) || (cur_feat == FEAT_MORE)) break;

            /* No lines of shops */
            if ((cur_feat >= FEAT_SHOP_HEAD) && (cur_feat <= FEAT_SHOP_TAIL)) break;

            /* No lines of house doors */
            if ((cur_feat >= FEAT_HOME_OPEN) && (cur_feat <= FEAT_HOME_TAIL)) break;

            /* Draw a line if we have a valid direction */
            if (dir && (dir != 5) && VALID_DIR(dir))
            {
                s16b x = p_ptr->px, y = p_ptr->py;

                /* Require a "clean" floor grid */
                while (cave_clean_bold(p_ptr->depth, y, x))
                {
                    /* Set feature */
                    cave_set_feat(cave_get(p_ptr->depth), y, x, cur_feat);

                    /* Update the visuals */
                    update_visuals(p_ptr->depth);

                    /* Use the given direction */
                    x += ddx[dir];
                    y += ddy[dir];
                }
            }

            break;
        }

        /* Fill Rectangle */
        case 'r':
        {
            int y = p_ptr->py, x = p_ptr->px, y_max = DUNGEON_HGT - 1, x_max = DUNGEON_WID - 1;

            /* No rectangles of staircases */
            if ((cur_feat == FEAT_LESS) || (cur_feat == FEAT_MORE)) break;

            /* No rectangles of shops */
            if ((cur_feat >= FEAT_SHOP_HEAD) && (cur_feat <= FEAT_SHOP_TAIL)) break;

            /* No rectangles of house doors */
            if ((cur_feat >= FEAT_HOME_OPEN) && (cur_feat <= FEAT_HOME_TAIL)) break;

            /* Find the width and height of the rectangle to fill */
            while ((y < y_max) && (x < x_max))
            {
                get_rectangle(p_ptr->depth, y, x, &y_max, &x_max);
                y++; x++;
            }

            /* Fill rectangle */
            for (y = p_ptr->py; y < y_max; y++)
            {
                for (x = p_ptr->px; x < x_max; x++)
                {
                    /* Set feature */
                    cave_set_feat(cave_get(p_ptr->depth), y, x, cur_feat);

                    /* Update the visuals */
                    update_visuals(p_ptr->depth);
                }
            }

            break;
        }

        /* Build mode on */
        case 'm':
        {
            master_move_hook = master_build;
            break;
        }

        /* Build mode off */
        case 'x':
        {
            master_move_hook = NULL;
            break;
        }
    }
}


static char master_specific_race_char = 'a';


static bool master_summon_specific_aux(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* No uniques */
    if (rf_has(r_ptr->flags, RF_UNIQUE)) return FALSE;

    /* If we look like what we are looking for */
    if (r_ptr->d_char == master_specific_race_char) return TRUE;
    return FALSE;
}


/*
 * Auxillary function to master_summon, determine the exact type of monster
 * to summon from a more general description.
 */
static u16b master_summon_aux_monster_type(char monster_type, char* monster_parms)
{
    int tmp, depth;

    /* Handle each category of monster types */
    switch (monster_type)
    {
        /* Specific monster specified */
        case 's':
        {
            /* Summon monster given by race name */
            if (strlen(monster_parms) > 1)
            {
                /* Race prefixed by '#': summon from exact race name */
                if (monster_parms[0] == '#')
                    return race_index(monster_parms + 1);

                /* Summon monster given by partial race name */
                return race_index_fuzzy(monster_parms);
            }
            
            /* Summon monster given by race symbol */
            master_specific_race_char = monster_parms[0];
            get_mon_num_hook = master_summon_specific_aux;
            get_mon_num_prep();
            depth = randint0(100) + 10;
            tmp = get_mon_num(depth, monster_level(depth));

            /* Restore monster generator */
            get_mon_num_hook = NULL;
            get_mon_num_prep();

            /* Return our monster */
            return tmp;
        }

        /* Specific depth specified */
        case 'd':
            return get_mon_num(monster_parms[0], monster_level(monster_parms[0]));
    }

    /* Failure */
    return 0;
}


/*
 * Monster summoning options. More documentation on this later.
 */
static void master_summon(int Ind, char* parms)
{
    int c;
    player_type *p_ptr = player_get(Ind);

    /* What type of monster we are -- specific, random orc, etc */
    static char monster_type = 0;

    static char monster_parms[NORMAL_WID];

    /* What kind to summon -- x right here, group at random location, etc */
    static char summon_type = 0;

    /* Arguments to previous byte */
    static char summon_parms = 0;

    /* Which monster to actually summon, from previous variables */
    static u16b r_idx = 0;

    /* How many monsters to actually summon */
    unsigned char size = 0;

    /* Extract arguments.  If none are found, summon previous type. */
    if (parms)
    {
        /* The first character specifies the type of monster */
        summon_type = parms[0];
        summon_parms = parms[1];
        monster_type = parms[2];

        /* Hack -- since monster_parms is a string, throw it on the end */
        my_strcpy(monster_parms, &parms[3], sizeof(monster_parms));
    }

    switch (summon_type)
    {
        /* Summon x here */
        case 'x':
        {
            /* For each monster we are summoning */
            for (c = 0; c < summon_parms; c++)
            {
                /* Hack -- monster_type '0' specifies mass banishment */
                if (monster_type == '0')
                {
                    mass_banishment(p_ptr);
                    break;
                }

                /* Figure out who to summon */
                r_idx = master_summon_aux_monster_type(monster_type, monster_parms);

                /* Summon the monster, if we have a valid one */
                if (r_idx)
                    summon_specific_race(Ind, p_ptr->depth, p_ptr->py, p_ptr->px, r_idx, 1);
            }
            break;
        }

        /* Summon x at random locations */
        case 'X':
        {
            for (c = 0; c < summon_parms; c++)
            {
                /* Figure out who to summon */
                r_idx = master_summon_aux_monster_type(monster_type, monster_parms);

                /* Summon the monster at a random location */
                if (r_idx)
                    summon_specific_race_somewhere(Ind, p_ptr->depth, r_idx, 1);
            }
            break;
        }

        /* Summon group of random size here */
        case 'g':
        {
            /* Figure out how many to summon */
            size = randint0(randint0(50)) + 2;

            /* Figure out who to summon */
            r_idx = master_summon_aux_monster_type(monster_type, monster_parms);

            /* Summon the group here */
            summon_specific_race(Ind, p_ptr->depth, p_ptr->py, p_ptr->px, r_idx, size);

            break;
        }

        /* Summon group of random size at random location */
        case 'G':
        {
            /* Figure out how many to summon */
            size = randint0(randint0(50)) + 2;

            /* Figure out who to summon */
            r_idx = master_summon_aux_monster_type(monster_type, monster_parms);

            /* Someone the group at a random location */
            summon_specific_race_somewhere(Ind, p_ptr->depth, r_idx, size);

            break;
        }

        /* Summon mode on (use with discretion... lets not be TOO mean ;-) )*/
        case 'T':
        {
            summon_type = 'x';
            summon_parms = 1;

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
    char* str;
    char* dst;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    for (v = vaults; v; v = v->next)
    {
        /* Clean up name */
        dst = buf;
        for (str = v->name; *str; str++)
        {
            if (isalpha(*str) || (*str == 32)) *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

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


static struct vault *get_vault_byidx(int vidx)
{
    struct vault *v;

    for (v = vaults; v; v = v->next)
    {
        if (v->vidx == vidx) return v;
    }

    return NULL;
}


#define GE_IDX  0
#define GE_NEXT 1
#define GE_PREV 2


static object_kind *item_kind(object_kind *k, int method)
{
    int i = k->kidx;

    if (method == GE_NEXT) i++;
    if (method == GE_PREV) i--;

    while ((i > 0) && (i < z_info->k_max))
    {
        object_kind *kind = &k_info[i];

        /* Only allocated items */
        if (kind->alloc_prob > 0) return kind;

        if (method == GE_IDX) break;
        if (method == GE_NEXT) i++;
        if (method == GE_PREV) i--;
    }

    return NULL;
}


static object_kind *item_kind_fuzzy(char *name)
{
    char match[NORMAL_WID];
    char* str;
    char* dst;
    int i;
    char d_char = '\0';

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* Check if a symbol has been passed (!speed, =strength...) */
    if ((*name < 'a') || (*name > 'z')) d_char = *name;

    /* For each item kind race */
    for (i = 1; i < z_info->k_max; i++)
    {
        object_kind *kind = &k_info[i];

        if (!kind->name) continue;

        /* Check default object character */
        if (d_char && (d_char != kind->d_char)) continue;

        /* Clean up its name */
        dst = match;
        for (str = kind->name; *str; str++)
        {
            if (isalpha(*str) || (*str == ' ') || (*str == '*'))
                *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

        /* If cleaned name matches our search string, return it */
        if (strstr(match, (d_char? (name + 1): name))) return kind;
    }

    return NULL;
}


/*
 * Check an ego item from e_info
 */
static bool check_ego(const object_type *o_ptr, ego_item_type *ego)
{
    int idxtval;

    /* XXX Ignore cursed items for now */
    if (cursed_p(ego->flags)) return FALSE;

    /* Check possible ego tval values */
    for (idxtval = 0; TRUE; idxtval++)
    {
        if (idxtval == 4) return FALSE;
        if (o_ptr->tval == ego->tval[idxtval]) break;
    }

    /* Check sval range */
    if ((o_ptr->sval < ego->min_sval[idxtval]) || (o_ptr->sval > ego->max_sval[idxtval]))
        return FALSE;

    /* We have a winner */
    return TRUE;
}


static ego_item_type *item_ego(const object_type *o_ptr, ego_item_type *e, int method)
{
    int i = e->eidx;

    if (method == GE_NEXT) i++;
    if (method == GE_PREV) i--;

    while ((i > 0) && (i < z_info->e_max))
    {
        ego_item_type *ego = &e_info[i];

        /* Only allowed egos */
        if (check_ego(o_ptr, ego)) return ego;

        if (method == GE_IDX) break;
        if (method == GE_NEXT) i++;
        if (method == GE_PREV) i--;
    }

    return NULL;
}


static ego_item_type *item_ego_fuzzy(const object_type *o_ptr, char *name)
{
    char match[NORMAL_WID];
    char* str;
    char* dst;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each ego kind race */
    for (i = 1; i < z_info->e_max; i++)
    {
        ego_item_type *ego = &e_info[i];

        if (!ego->name) continue;

        /* Check ego first to skip egos of incorrect kind with same name */
        if (!check_ego(o_ptr, ego)) continue;

        /* Clean up its name */
        dst = match;
        for (str = ego->name; *str; str++)
        {
            if (isalpha(*str) || (*str == ' ') || (*str == '*'))
                *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

        /* If cleaned name matches our search string, return it */
        if (strstr(match, name)) return ego;
    }

    return NULL;
}


static bool tohit_varies(const object_type *o_ptr)
{
    /* Magic ammo are always +0 +0 */
    if (magic_ammo_p(o_ptr)) return FALSE;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr)) return TRUE;

    /* Need a to-hit */
    if (!o_ptr->to_h) return FALSE;

    /* Base items with variable to-hit */
    if (randcalc_varies(o_ptr->kind->to_h)) return TRUE;

    /* Ego items with variable to-hit */
    if (o_ptr->ego && randcalc_varies(o_ptr->ego->to_h)) return TRUE;

    return FALSE;
}


static s16b tohit_min(const object_type *o_ptr)
{
    int base;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr))
    {
        if (o_ptr->ego) return o_ptr->ego->min_to_h;
        return 0;
    }

    /* Base items with variable to-hit */
    base = randcalc(o_ptr->kind->to_h, MAX_DEPTH - 1, MINIMISE);
    if (randcalc_varies(o_ptr->kind->to_h)) return base;

    /* Ego items with variable to-hit */
    return base + randcalc(o_ptr->ego->to_h, MAX_DEPTH - 1, MINIMISE);
}


static s16b tohit_max(const object_type *o_ptr)
{
    int base;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr)) return 255;

    /* Base items with variable to-hit */
    base = randcalc(o_ptr->kind->to_h, MAX_DEPTH - 1, MAXIMISE);
    if (randcalc_varies(o_ptr->kind->to_h)) return base;

    /* Ego items with variable to-hit */
    return base + randcalc(o_ptr->ego->to_h, MAX_DEPTH - 1, MAXIMISE);
}


static bool todam_varies(const object_type *o_ptr)
{
    /* Magic ammo are always +0 +0 */
    if (magic_ammo_p(o_ptr)) return FALSE;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr)) return TRUE;

    /* Need a to-dam */
    if (!o_ptr->to_d) return FALSE;

    /* Base items with variable to-dam */
    if (randcalc_varies(o_ptr->kind->to_d)) return TRUE;

    /* Ego items with variable to-dam */
    if (o_ptr->ego && randcalc_varies(o_ptr->ego->to_d)) return TRUE;

    return FALSE;
}


static s16b todam_min(const object_type *o_ptr)
{
    int base;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr))
    {
        if (o_ptr->ego) return o_ptr->ego->min_to_d;
        return 0;
    }

    /* Base items with variable to-dam */
    base = randcalc(o_ptr->kind->to_d, MAX_DEPTH - 1, MINIMISE);
    if (randcalc_varies(o_ptr->kind->to_d)) return base;

    /* Ego items with variable to-dam */
    return base + randcalc(o_ptr->ego->to_d, MAX_DEPTH - 1, MINIMISE);
}


static s16b todam_max(const object_type *o_ptr)
{
    int base;

    /* Missiles and weapons */
    if (wieldable_p(o_ptr)) return 255;

    /* Base items with variable to-dam */
    base = randcalc(o_ptr->kind->to_d, MAX_DEPTH - 1, MAXIMISE);
    if (randcalc_varies(o_ptr->kind->to_d)) return base;

    /* Ego items with variable to-dam */
    return base + randcalc(o_ptr->ego->to_d, MAX_DEPTH - 1, MAXIMISE);
}


static bool toac_varies(const object_type *o_ptr)
{
    /* Armor parts */
    if (armor_p(o_ptr)) return TRUE;

    /* Need a to-ac */
    if (!o_ptr->to_a) return FALSE;

    /* Base items with variable to-ac */
    if (randcalc_varies(o_ptr->kind->to_a)) return TRUE;

    /* Ego items with variable to-ac */
    if (o_ptr->ego && randcalc_varies(o_ptr->ego->to_a)) return TRUE;

    return FALSE;
}


static s16b toac_min(const object_type *o_ptr)
{
    int base;

    /* Armor parts */
    if (armor_p(o_ptr))
    {
        if (o_ptr->ego) return o_ptr->ego->min_to_a;
        return 0;
    }

    /* Base items with variable to-ac */
    base = randcalc(o_ptr->kind->to_a, MAX_DEPTH - 1, MINIMISE);
    if (randcalc_varies(o_ptr->kind->to_a)) return base;

    /* Ego items with variable to-ac */
    return base + randcalc(o_ptr->ego->to_a, MAX_DEPTH - 1, MINIMISE);
}


static s16b toac_max(const object_type *o_ptr)
{
    int base;

    /* Armor parts */
    if (armor_p(o_ptr)) return 255;

    /* Base items with variable to-ac */
    base = randcalc(o_ptr->kind->to_a, MAX_DEPTH - 1, MAXIMISE);
    if (randcalc_varies(o_ptr->kind->to_a)) return base;

    /* Ego items with variable to-ac */
    return base + randcalc(o_ptr->ego->to_a, MAX_DEPTH - 1, MAXIMISE);
}


static int pval_add(object_type *o_ptr, int pval, int value)
{
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;

    /* Need a pval */
    if (!o_ptr->pval[pval]) return pval;

    /* Make a "test" copy */
    object_copy(i_ptr, o_ptr);

    /* Hack -- Check monster race for rings of Polymorphing */
    if ((i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_POLYMORPHING))
    {
        monster_race *r_ptr;

        /* Skip uniques and monsters that can't be generated */
        do
        {
            /* Add value */
            i_ptr->pval[pval] += value;
            if ((i_ptr->pval[pval] <= 0) || (i_ptr->pval[pval] >= z_info->r_max)) break;

            r_ptr = &r_info[i_ptr->pval[pval]];
        }
        while (!r_ptr->name || rf_has(r_ptr->flags, RF_UNIQUE) ||
            (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
            (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters));

        /* Enforce min/max bounds */
        if ((i_ptr->pval[pval] > 0) && (i_ptr->pval[pval] < z_info->r_max))
            object_copy(o_ptr, i_ptr);

        return pval;
    }

    /* Analyze */
    switch (i_ptr->tval)
    {
        case TV_CHEST:
        {
            /* Add value */
            i_ptr->pval[pval] += value;

            /* Enforce min/max bounds */
            if ((i_ptr->pval[pval] >= 1) && (i_ptr->pval[pval] <= 59))
                object_copy(o_ptr, i_ptr);

            return pval;
        }
        case TV_STAFF:
        case TV_WAND:
        {
            /* Add value */
            i_ptr->pval[pval] += value;

            /* Enforce min/max bounds */
            if ((i_ptr->pval[pval] >= randcalc(i_ptr->kind->charge, MAX_DEPTH - 1, MINIMISE)) &&
                (i_ptr->pval[pval] <= randcalc(i_ptr->kind->charge, MAX_DEPTH - 1, MAXIMISE)))
            {
                object_copy(o_ptr, i_ptr);
            }

            return pval;
        }
        default:
        {
            bitflag flags[OF_SIZE];
            int flag;

            /* Get a copy of pval flags for the current pval */
            of_copy(flags, i_ptr->pval_flags[pval]);

            /* Iterate on all flags affecting current pval */
            for (flag = of_next(flags, FLAG_START); flag != FLAG_END;
                flag = of_next(flags, flag + 1))
            {
                int i;
                bool pval_varies = FALSE;
                s16b pval_min, pval_max;

                /* Base items with variable pval */
                for (i = 0; i < i_ptr->kind->num_pvals; i++)
                {
                    if (of_has(i_ptr->kind->pval_flags[i], flag))
                    {
                        pval_varies = randcalc_varies(i_ptr->kind->pval[i]);
                        if (pval_varies)
                        {
                            pval_min = randcalc(i_ptr->kind->pval[i], MAX_DEPTH - 1, MINIMISE);
                            if ((i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_SPEED))
                                pval_max = 255;
                            else
                                pval_max = randcalc(i_ptr->kind->pval[i], MAX_DEPTH - 1, MAXIMISE);
                        }
                    }
                }

                /* Ego items with variable pval */
                for (i = 0; !pval_varies && i_ptr->ego && (i < i_ptr->ego->num_pvals); i++)
                {
                    if (of_has(i_ptr->ego->pval_flags[i], flag))
                    {
                        pval_varies = randcalc_varies(i_ptr->ego->pval[i]);
                        if (pval_varies)
                        {
                            pval_min = randcalc(i_ptr->ego->pval[i], MAX_DEPTH - 1, MINIMISE);
                            pval_max = randcalc(i_ptr->ego->pval[i], MAX_DEPTH - 1, MAXIMISE);

                            /* Add possible fixed pval from base item */
                            if (of_has(i_ptr->kind->pval_flags[i], flag))
                            {
                                pval_min += randcalc(i_ptr->kind->pval[i], MAX_DEPTH - 1, MINIMISE);
                                pval_max += randcalc(i_ptr->kind->pval[i], MAX_DEPTH - 1, MAXIMISE);
                            }
                        }
                    }
                }

                /* Item must have a variable pval */
                if (pval_varies)
                {
                    int new_pval = 0;

                    /* Add value */
                    object_add_pval(i_ptr, value, flag);

                    /* Get new pval (if shifted) */
                    if (of_has(i_ptr->flags, flag))
                        new_pval = i_ptr->pval[which_pval(i_ptr, flag)];

                    /* Enforce min/max bounds */
                    if ((new_pval >= pval_min) && (new_pval <= pval_max))
                        object_copy(o_ptr, i_ptr);
                }

                /* Reinitialize the "test" copy */
                object_copy(i_ptr, o_ptr);
            }

            /* Apply minima */
            if (o_ptr->ego) ego_min_pvals(o_ptr);

            if (pval >= o_ptr->num_pvals) return o_ptr->num_pvals - 1;
            return pval;
        }
    }
}


static const char *id_status(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if (object_is_known(p_ptr, o_ptr)) return "{id}";
    return "{unid}";
}


/*
 * Shadow function of apply_magic().
 */
static void apply_base_magic(object_type *o_ptr)
{
    bool great = FALSE;
    int level = MAX_DEPTH - 1;

    /* Magic ammo are always +0 +0 */
    if (magic_ammo_p(o_ptr)) return;

    /* Roll for "great" */
    if (o_ptr->ego) great = TRUE;

    /* Apply magic */
    switch (o_ptr->tval)
    {
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOW:
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            if (great)
            {
                o_ptr->to_h += randint1(5) + m_bonus(5, level);
                o_ptr->to_h += m_bonus(10, level);
                o_ptr->to_d += randint1(5) + m_bonus(5, level);
                o_ptr->to_d += m_bonus(10, level);
            }

            break;
        }

        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_HELM:
        case TV_CROWN:
        case TV_CLOAK:
        case TV_GLOVES:
        case TV_BOOTS:
        {
            if (great)
            {
                o_ptr->to_a += randint1(5) + m_bonus(5, level);
                o_ptr->to_a += m_bonus(10, level);
            }

            /* Bad */
            if (o_ptr->to_a < 0)
            {
                size_t i;

                /* Hack -- Reverse base bonus */
                for (i = 0; i < o_ptr->num_pvals; i++)
                {
                    if (o_ptr->pval[i] > 0) o_ptr->pval[i] = 0 - o_ptr->pval[i];
                }
            }

            break;
        }

        case TV_CHEST:
        {
            /* Hack -- skip ruined chests */
            if (o_ptr->kind->level <= 0) break;

            /* Hack -- pick a "difficulty" */
            o_ptr->pval[DEFAULT_PVAL] = randint1(o_ptr->kind->level);

            /* Never exceed "difficulty" of 55 to 59 */
            if (o_ptr->pval[DEFAULT_PVAL] > 55)
                o_ptr->pval[DEFAULT_PVAL] = 55 + randint0(5);

            break;
        }
    }
}


/*
 * Generate something
 */
static void master_generate(int Ind, char* parms)
{
    player_type *p_ptr = player_get(Ind);
    static object_type dm_obj;
    object_type *o_ptr = &dm_obj;
    static int xtra1;
    static int xtra2;
    static int pval;

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
                    object_wipe(o_ptr);
                    xtra1 = FLAG_END;
                    xtra2 = FLAG_END;
                    pval = DEFAULT_PVAL;

                    break;
                }

                /* Item */
                case 'k':
                {
                    object_kind *kind = NULL;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case '#':
                        {
                            int k0 = 0;

                            if (parms[3])
                            {
                                k0 += (byte)parms[3];
                                if (parms[4])
                                {
                                    k0 += (byte)parms[4];
                                    if (parms[5]) k0 += (byte)parms[5];
                                }
                            }
                            kind = item_kind(&k_info[k0], GE_IDX);
                            break;
                        }
                        case 'n':
                        {
                            kind = item_kind_fuzzy(&parms[3]);
                            if (kind) kind = item_kind(kind, GE_IDX);
                            break;
                        }
                        case '+':
                        {
                            if (o_ptr->kind) kind = item_kind(o_ptr->kind, GE_NEXT);
                            break;
                        }
                        case '-':
                        {
                            if (o_ptr->kind) kind = item_kind(o_ptr->kind, GE_PREV);
                            break;
                        }
                    }

                    /* Obtain an item */
                    if (kind && (kind != o_ptr->kind))
                    {
                        xtra1 = FLAG_END;
                        xtra2 = FLAG_END;
                        pval = DEFAULT_PVAL;

                        /* Prepare the object */
                        object_prep(o_ptr, kind, MAX_DEPTH - 1, RANDOMISE);
                        object_flavor_aware(p_ptr, o_ptr);
                        apply_base_magic(o_ptr);

                        /* Set extra powers */
                        init_powers(o_ptr, &xtra1, &xtra2);
                    }

                    break;
                }

                /* Ego */
                case 'e':
                {
                    ego_item_type *ego = NULL;

                    /* Object must be defined */
                    if (!o_ptr->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case '#':
                        {
                            ego = item_ego(o_ptr, &e_info[(byte)parms[3]], GE_IDX);
                            break;
                        }
                        case 'n':
                        {
                            ego = item_ego_fuzzy(o_ptr, &parms[3]);
                            if (ego) ego = item_ego(o_ptr, ego, GE_IDX);
                            break;
                        }
                        case '+':
                        {
                            if (o_ptr->ego) ego = item_ego(o_ptr, o_ptr->ego, GE_NEXT);
                            break;
                        }
                        case '-':
                        {
                            if (o_ptr->ego) ego = item_ego(o_ptr, o_ptr->ego, GE_PREV);
                            break;
                        }
                    }

                    /* Obtain an ego */
                    if (ego && (ego != o_ptr->ego))
                    {
                        object_kind *kind = o_ptr->kind;

                        xtra1 = FLAG_END;
                        xtra2 = FLAG_END;
                        pval = DEFAULT_PVAL;

                        /* Prepare the object */
                        object_prep(o_ptr, kind, MAX_DEPTH - 1, RANDOMISE);
                        o_ptr->ego = ego;
                        apply_base_magic(o_ptr);

                        /* Set extra powers */
                        init_powers(o_ptr, &xtra1, &xtra2);
                        ego_apply_magic(o_ptr, MAX_DEPTH - 1);
                    }

                    break;
                }

                /* Decrement value */
                case 'M':
                {
                    /* Object must be defined */
                    if (!o_ptr->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'h':
                        {
                            /* Item must have a variable to-hit value */
                            if (!tohit_varies(o_ptr)) break;

                            /* Enforce min bound */
                            if (o_ptr->to_h == tohit_min(o_ptr)) break;

                            o_ptr->to_h--; break;
                        }
                        case 'd':
                        {
                            /* Item must have a variable to-dam value */
                            if (!todam_varies(o_ptr)) break;

                            /* Enforce min bound */
                            if (o_ptr->to_d == todam_min(o_ptr)) break;

                            o_ptr->to_d--; break;
                        }
                        case 'a':
                        {
                            /* Item must have a variable to-ac value */
                            if (!toac_varies(o_ptr)) break;

                            /* Enforce min bound */
                            if (o_ptr->to_a == toac_min(o_ptr)) break;

                            o_ptr->to_a--; break;
                        }
                        case 'x':
                        {
                            dec_power1(o_ptr, &xtra1);
                            break;
                        }
                        case 'y':
                        {
                            dec_power2(o_ptr, &xtra2);
                            break;
                        }
                        case 'n':
                        {
                            /* Item must have a variable pval */
                            if (!o_ptr->num_pvals) break;

                            /* Min bound */
                            if (pval == DEFAULT_PVAL) break;

                            pval--; break;
                        }
                        case 'p':
                        {
                            pval = pval_add(o_ptr, pval, -1);
                            break;
                        }
                        case 'm':
                        {
                            /* Set weapon/missile extra dice */
                            if ((melee_p(o_ptr) || obj_is_ammo(p_ptr, o_ptr)) &&
                                (o_ptr->dd > o_ptr->kind->dd))
                            {
                                o_ptr->dd--;
                            }
                            break;
                        }
                        case 'i':
                        {
                            /* Set unidentified */
                            o_ptr->ident = 0;
                            of_wipe(o_ptr->known_flags);
                            break;
                        }
                    }

                    break;
                }

                /* Increment value */
                case 'I':
                {
                    /* Object must be defined */
                    if (!o_ptr->kind) break;

                    /* Parse subcommand */
                    switch (parms[2])
                    {
                        case 'h':
                        {
                            /* Item must have a variable to-hit value */
                            if (!tohit_varies(o_ptr)) break;

                            /* Enforce max bound */
                            if (o_ptr->to_h == tohit_max(o_ptr)) break;

                            o_ptr->to_h++; break;
                        }
                        case 'd':
                        {
                            /* Item must have a variable to-dam value */
                            if (!todam_varies(o_ptr)) break;

                            /* Enforce max bound */
                            if (o_ptr->to_d == todam_max(o_ptr)) break;

                            o_ptr->to_d++; break;
                        }
                        case 'a':
                        {
                            /* Item must have a variable to-ac value */
                            if (!toac_varies(o_ptr)) break;

                            /* Enforce max bound */
                            if (o_ptr->to_a == toac_max(o_ptr)) break;

                            o_ptr->to_a++; break;
                        }
                        case 'x':
                        {
                            inc_power1(o_ptr, &xtra1);
                            break;
                        }
                        case 'y':
                        {
                            inc_power2(o_ptr, &xtra2);
                            break;
                        }
                        case 'n':
                        {
                            /* Item must have a variable pval */
                            if (!o_ptr->num_pvals) break;

                            /* Max bound */
                            if (pval == o_ptr->num_pvals - 1) break;

                            pval++; break;
                        }
                        case 'p':
                        {
                            pval = pval_add(o_ptr, pval, 1);
                            break;
                        }
                        case 'm':
                        {
                            /* Set weapon/missile extra dice */
                            if ((melee_p(o_ptr) || obj_is_ammo(p_ptr, o_ptr)) && (o_ptr->dd < 255))
                                o_ptr->dd++;
                            break;
                        }
                        case 'i':
                        {
                            /* Set identified */
                            object_notice_everything(p_ptr, o_ptr, FALSE);
                            break;
                        }
                    }

                    break;
                }

                /* Generate */
                case 'd':
                {
                    /* Object must be defined */
                    if (!o_ptr->kind) break;

                    o_ptr->number = parms[2];

                    /* Set origin */
                    set_origin(o_ptr, ORIGIN_CHEAT, 0, 0);

                    /* Bypass auto-squelch */
                    o_ptr->squelch = SQUELCH_PROTECT;

                    /* Generate */
                    do_fixed_powers(o_ptr, xtra1, xtra2);
                    drop_near(p_ptr, cave_get(p_ptr->depth), o_ptr, 0, p_ptr->py,
                        p_ptr->px, TRUE);

                    /* Reinitialize some stuff */
                    undo_fixed_powers(o_ptr, xtra1, xtra2);

                    break;
                }
            }

            /* Display result */
            if (o_ptr->kind)
            {
                char o_name[NORMAL_WID];
                char buf[NORMAL_WID];

                /* Describe item */
                object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

                /* Obtain extra modifiers */
                get_extra_mods(xtra1, xtra2, buf, sizeof(buf));

                if (o_ptr->ego)
                {
                    Send_special_line(Ind, 19, 19, 18, TERM_WHITE,
                        format("%d. %s %s", o_ptr->kind->kidx, o_name,
                        id_status(Ind, o_ptr)));
                    Send_special_line(Ind, 19, 19, 19, TERM_WHITE,
                        format("%d. %s [%s]", o_ptr->ego->eidx,
                        o_ptr->ego->name, buf));
                }
                else
                {
                    Send_special_line(Ind, 19, 19, 18, TERM_WHITE,
                        format("%d. %s [%s] %s", o_ptr->kind->kidx, o_name, buf,
                        id_status(Ind, o_ptr)));
                    Send_special_line(Ind, 19, 19, 19, TERM_WHITE, " [No Ego]");
                }
            }
            else
            {
                Send_special_line(Ind, 19, 19, 18, TERM_WHITE, " [No Item]");
                Send_special_line(Ind, 19, 19, 19, TERM_WHITE, " [No Ego]");
            }

            break;
        }

        /* Generate a vault */
        case 'v':
        {
            struct vault *v_ptr = NULL;

            switch (parms[1])
            {
                case '#':
                    v_ptr = get_vault_byidx(parms[2]);
                    break;
                case 'n':
                {
                    /* Name prefixed by '#': exact vault name */
                    if (parms[2] == '#')
                        v_ptr = get_vault_byname(&parms[3]);
                    else
                        v_ptr = get_vault_byfuzzyname(&parms[2]);
                }
            }

            if (!v_ptr || !v_ptr->wid) return;

            /* Forbidden outside of the dungeon (for now) */
            if (p_ptr->depth < 0) return;

            /* Not in town or quest levels */
            if (!p_ptr->depth || is_quest(p_ptr->depth)) return;

            /* Build a vault with the DM at the top left corner */
            build_vault(p_ptr, cave_get(p_ptr->depth), p_ptr->py + v_ptr->hgt / 2,
                p_ptr->px + v_ptr->wid / 2, v_ptr->hgt, v_ptr->wid, v_ptr->text);

            break;
        }
    }
}


/*  
 * Determine if the player is inside the house
 */
bool house_inside(struct player *p, int house)
{
    if ((house >= 0) && (house < num_houses) && (houses[house].depth == p->depth) &&
        (p->px >= houses[house].x_1) && (p->px <= houses[house].x_2) &&
        (p->py >= houses[house].y_1) && (p->py <= houses[house].y_2))
    {
        return TRUE;
    }
    return FALSE;
}


/*  
 * Determine if the player owns the house
 */
bool house_owned_by(struct player *p, int house)
{
    return ((houses[house].ownerid > 0) && (p->id == houses[house].ownerid));
}


void reset_house(int house, int depth, int y, int x)
{
    int i, yy, xx;

    /* House is no longer owned */
    houses[house].ownername[0] = '\0';
    houses[house].ownerid = 0;
    houses[house].color = 0;

    /* Remove all players from the house */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        if (house_inside(p_ptr, house))
        {
            msg(p_ptr, "You have been expelled from the house.");
            do
            {
                teleport_player(p_ptr, 10);
            }
            while (house_inside(p_ptr, house));
        }
    }

    /* Reset the house the hard way on unallocated levels */
    if (!cave_get(depth))
    {
        /* Scan all objects */
        for (i = 1; i < o_max; i++)
        {
            object_type *o_ptr = object_byid(i);

            /* Skip dead objects */
            if (!o_ptr->kind) continue;

            /* Skip objects not on this depth */
            if (o_ptr->depth != depth) continue;

            /* Skip objects not inside the house */
            if ((o_ptr->iy < houses[house].y_1) || (o_ptr->iy > houses[house].y_2) ||
                (o_ptr->ix < houses[house].x_1) || (o_ptr->ix > houses[house].x_2))
            {
                continue;
            }

            /* Hack -- Preserve artifacts */
            preserve_artifact(o_ptr);

            /* Wipe the object */
            object_wipe(o_ptr);
        }

        return;
    }

    /* Close the door */
    cave_set_feat(cave_get(depth), y, x, FEAT_HOME_HEAD);

    /* Reset the house */
    for (yy = houses[house].y_1; yy <= houses[house].y_2; yy++)
    {
        for (xx = houses[house].x_1; xx <= houses[house].x_2; xx++)
        {
            /* Delete the objects */
            delete_object(depth, yy, xx);
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


static bool check_permissions(int Ind, int who, bool silent)
{
    player_type *p_ptr = player_get(Ind);
    u32b access_flag = DM_CAN_MUTATE_SELF;

    /* No player */
    if (!who)
    {
        if (!silent) Send_special_line(Ind, 17, 17, 15, TERM_WHITE, " Error: no player");
        return FALSE;
    }

    /* Check permissions */
    if (who != Ind) access_flag = DM_CAN_ASSIGN;
    if (!(p_ptr->dm_flags & access_flag))
    {
        if (!silent)
        {
            Send_special_line(Ind, 17, 17, 15, TERM_WHITE,
                ((who == Ind)? " Error: can't change self": " Error: can't change others"));
        }
        return FALSE;
    }

    return TRUE;
}


static void master_player(int Ind, char* parms)
{
    static int dm_player = 0;
    static int dm_player_off = 0;
    player_type *q_ptr;
    char *desc;

    /* Assign self */
    if (parms[0] == ' ')
    {
        dm_player = Ind;
        return;
    }

    /* Assign other (by name) */
    if (parms[0] != '>')
    {
        int i;

        dm_player = 0;
        for (i = 1; i <= NumPlayers; i++)
        {
            q_ptr = player_get(i);

            if (!strncasecmp(q_ptr->name, parms, strlen(parms)))
            {
                dm_player = i;
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

            if (!check_permissions(Ind, dm_player, TRUE)) return;
            q_ptr = player_get(dm_player);

            /* Toggle 1 of DM flags */
            if (!(q_ptr->dm_flags & new_flag))
                q_ptr->dm_flags |= new_flag;
            else
                q_ptr->dm_flags &= ~new_flag;

            /* Hack -- For *invulnerable* set "invuln" */
            if (new_flag == DM_INVULNERABLE)
            {
                if (q_ptr->dm_flags & new_flag)
                {
                    q_ptr->timed[TMD_INVULN] = -1;
                    q_ptr->update |= (PU_BONUS | PU_MONSTERS);
                    q_ptr->redraw |= (PR_MAP | PR_STATUS);
                    handle_stuff(q_ptr);
                }
                else
                    player_clear_timed(q_ptr, TMD_INVULN, TRUE);
            }

            return;
        }

        /* Toggle Ghost */
        case 'g':
        {
            if (!check_permissions(Ind, dm_player, TRUE)) return;
            q_ptr = player_get(dm_player);

            set_ghost_flag(q_ptr, (q_ptr->ghost? 0: 1), TRUE);
            q_ptr->redraw |= (PR_BASIC | PR_SPELL);
            q_ptr->update |= (PU_BONUS);
            return;
        }

        /* Toggle Wizard */
        case 'w':
        {
            if (!check_permissions(Ind, dm_player, TRUE)) return;
            q_ptr = player_get(dm_player);

            q_ptr->noscore = (q_ptr->noscore? 0: 1);
            return;
        }
    }

    /* Check permissions */
    if (!check_permissions(Ind, dm_player, FALSE))
    {
        dm_player = 0;
        return;
    }

    q_ptr = player_get(dm_player);

    /* Cannot toggle ghost for Dragon players or in fruit bat mode */
    if (q_ptr->ghost &&
        (player_has(player_get(dm_player), PF_DRAGON) || OPT_P(q_ptr, birth_fruit_bat)))
    {
        Send_special_line(Ind, 17, 17, 15, TERM_WHITE,
            " Error: can't toggle ghost for no-ghost players");
        set_ghost_flag(q_ptr, 0, TRUE);
        q_ptr->redraw |= (PR_BASIC | PR_SPELL);
        q_ptr->update |= (PU_BONUS);
        dm_player = 0;
        return;
    }

    /* Display */
    desc = format("  Player: %s%s%s", q_ptr->name, (q_ptr->ghost? ", ghost": ""),
        (q_ptr->noscore? ", wizard": ""));
    Send_special_line(Ind, 17, 17, 15, TERM_WHITE, desc);
    desc = format("    Flag: %s -- %s", dm_flags_str[dm_player_off],
        ((q_ptr->dm_flags & (1L << dm_player_off))? "Yes": "No"));
    Send_special_line(Ind, 17, 17, 16, TERM_WHITE, desc);
}


static void master_visuals(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int type;
    char chars[] = "*|/-\\";

    /* Check permissions */
    if (!is_dm_p(p_ptr)) return;

    /* Let the player scroll through the info */
    p_ptr->special_file_type = SPECIAL_FILE_OTHER;

    /* Scan the GF_XXX types */
    for (type = 0; type < GF_MAX; type++)
    {
        size_t i;
        const char *gf_name = gf_idx_to_name(type);

        /* Hack -- Special coloring */
        p_ptr->info[type][0].a = TERM_SPECIAL;
        for (i = 1; i < NORMAL_WID; i++) p_ptr->info[type][i].a = TERM_WHITE;

        /* Erase */
        for (i = 0; i < NORMAL_WID / 2; i++)
        {
            p_ptr->info[type][i * 2].c = TERM_WHITE;
            p_ptr->info[type][i * 2 + 1].c = ' ';
        }

        /* Name */
        for (i = 0; i < strlen(gf_name); i++) p_ptr->info[type][i * 2 + 1].c = gf_name[i];
        p_ptr->info[type][strlen(gf_name) * 2 + 1].c = ':';

        for (i = 0; i < BOLT_MAX; i++)
        {
            bool use_gfx = (p_ptr->use_graphics && !p_ptr->tile_distorted);
            byte a = (use_gfx? p_ptr->gf_attr[type][i]: spell_color(type));
            char c = (use_gfx? p_ptr->gf_char[type][i]: chars[i]);

            p_ptr->info[type][i * 2 + 32].c = a;
            p_ptr->info[type][i * 2 + 33].c = c;
        }
    }

    /* Last line */
    p_ptr->last_info_line = GF_MAX - 1;

    /* Let the client know to expect some info */
    Send_special_other(p_ptr, "GF_XXX types", 1, TRUE);
}


static void master_order(int Ind, char* parms)
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
            if (dm_order < STORE_MIN_KEEP - 1) dm_order++;
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
    Send_special_line(Ind, 17, 17, 15, TERM_WHITE, desc);
}


void do_cmd_master(int Ind, s16b command, char* buf)
{
    player_type *p_ptr = player_get(Ind);

    switch (command)
    {
        case MASTER_LEVEL:
        {
            if (!(p_ptr->dm_flags & DM_LEVEL_CONTROL)) return;
            master_level(Ind, buf);
            break;
        }

        case MASTER_BUILD:
        {
            if (!(p_ptr->dm_flags & DM_CAN_BUILD)) return;
            master_build(Ind, buf);
            break;
        }

        case MASTER_SUMMON:
        {
            if (!(p_ptr->dm_flags & DM_CAN_SUMMON)) return;
            master_summon(Ind, buf);
            break;
        }

        case MASTER_GENERATE:
        {
            if (!(p_ptr->dm_flags & DM_CAN_GENERATE)) return;
            master_generate(Ind, buf);
            break;
        }

        case MASTER_PLAYER:
        {
            master_player(Ind, buf);
            break;
        }

        case MASTER_VISUALS:
        {
            master_visuals(Ind);
            break;
        }

        case MASTER_ORDER:
        {
            master_order(Ind, buf);
            break;
        }
    }
}


/* Find arena by those coordinates */
int pick_arena(int depth, int y, int x)
{
    int i;

    for (i = 0; i < num_arenas; i++)
    {
        if (arenas[i].depth != depth) continue;
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
    for (i = 1; i < NumPlayers + 1; i++)
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
void access_arena(int Ind, int py, int px)
{
    player_type *p_ptr = player_get(Ind), *q_ptr;
    int tmp_count, a, tmp_id;

    /* No-PvP mode: can't access arenas */
    if (cfg_pvp_hostility == PVP_DISABLED)
    {
        msg(p_ptr, "There is a wall blocking your way.");
        return;
    }

    /* Ghosts can't access arenas */
    if (p_ptr->ghost)
    {
        msg(p_ptr, "There is a wall blocking your way.");
        return;
    }

    /* Player is in a party */
    if (p_ptr->party)
    {
        msg(p_ptr, "Please leave your party before entering this arena.");
        return;
    }

    /* Player tries to leave the arena */
    if (p_ptr->arena_num != -1)
    {
        /* Count players in this arena */
        tmp_count = count_arena_opponents(p_ptr->arena_num);

        /* If he is alone, leave */
        if (tmp_count == 1)
        {
            msg(p_ptr, "You leave the arena.");
            p_ptr->arena_num = -1;
            teleport_player(p_ptr, 1);
        }
        else
            msg(p_ptr, "There is a wall blocking your way.");
    }

    /* Player tries to enter the arena */
    else
    {
        a = pick_arena(p_ptr->depth, py, px);

        /* Count players in this arena */
        tmp_count = count_arena_opponents(a);

        /* If arena is not 'full' -- Enter it */
        if (tmp_count < 2)
        {
            msg(p_ptr, "You enter an ancient fighting pit.");
            teleport_player_to(p_ptr,
                arenas[a].y_1 + 1 + randint1(arenas[a].y_2 - arenas[a].y_1 - 2),
                arenas[a].x_1 + 1 + randint1(arenas[a].x_2 - arenas[a].x_1 - 2));
            p_ptr->arena_num = a;

            /* Both players are ready! */
            if (tmp_count == 1)
            {
                tmp_id = pick_arena_opponent(Ind, a);
                q_ptr = player_get(tmp_id);

                /* Declare hostility */
                pvp_check(q_ptr, p_ptr, PVP_ADD, TRUE, FEAT_NONE);
                pvp_check(p_ptr, q_ptr, PVP_ADD, TRUE, FEAT_NONE);
            }
        }
        else
            msg(p_ptr, "Arena is currently occupied.");
    }
}


static void format_line(int Ind, int target, char *line, char* buf, int len)
{
    player_type *p_ptr = player_get(Ind);
    const char *poss = player_poss(p_ptr);
    const char *refl;
    const char *poss_t, *refl_t;
    char target_name[NORMAL_WID], ch;

    switch (p_ptr->psex)
    {
        case SEX_FEMALE: refl = "her"; break;
        case SEX_MALE: refl = "him"; break;
        default: refl = "it"; break;
    }

    /* Player */
    if (target > 0)
    {
        player_type *t_ptr = player_get(target);

        poss_t = player_poss(t_ptr);

        switch (t_ptr->psex)
        {
            case SEX_FEMALE: refl_t = "her"; break;
            case SEX_MALE: refl_t = "him"; break;
            default: refl_t = "it"; break;
        }

        my_strcpy(target_name, t_ptr->name, sizeof(target_name));
    }

    /* Monster */
    else
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), 0 - target);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        if (rf_has(r_ptr->flags, RF_FEMALE))
        {
            poss_t = "her";
            refl_t = "her";
        }
        else if (rf_has(r_ptr->flags, RF_MALE))
        {
            poss_t = "his";
            refl_t = "him";
        }
        else
        {
            poss_t = "its";
            refl_t = "it";
        }

        monster_desc(p_ptr, target_name, sizeof(target_name), m_ptr, 0);
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
            if (ch == 'n') my_strcat(buf, p_ptr->name, len);

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
void do_cmd_social(int Ind, const char *buf, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int k;
    bool found = FALSE;
    social_type *s_ptr;
    char *t;
    char line[NORMAL_WID];
    char text[MSG_LEN];

    /* Ghosts don't socialize */
    if (p_ptr->ghost) return;

    /* Scan the socials */
    for (k = 0; k < z_info->soc_max; k++)
    {
        s_ptr = &soc_info[k];

        if (streq(s_ptr->name, buf))
        {
            found = TRUE;
            break;
        }
    }

    /* Not a valid action */
    if (!found) return;

    /* Try to find a target */
    if (s_ptr->target && dir && VALID_DIR(dir))
    {
        int flg = PROJECT_STOP;
        s16b ty, tx;
        int path_n = 0;
        u16b path_g[512];
        int m_idx;

        /* Only fire in direction 5 if we have a target */
        if ((dir == 5) && !target_okay(p_ptr)) return;

        /* Use the given direction */
        tx = p_ptr->px + 99 * ddx[dir];
        ty = p_ptr->py + 99 * ddy[dir];

        /* Hack -- Use an actual "target" */
        if (dir == 5)
        {
            flg &= ~PROJECT_STOP;
            target_get(p_ptr, &tx, &ty);
        }

        /* Calculate the path */
        path_n = project_path(path_g, (s_ptr->max_dist? s_ptr->max_dist: MAX_RANGE), p_ptr->depth,
            p_ptr->py, p_ptr->px, ty, tx, flg);
        if (!path_n) return;

        /* Get target grid */
        tx = GRID_X(path_g[path_n - 1]);
        ty = GRID_Y(path_g[path_n - 1]);
        m_idx = cave_get(p_ptr->depth)->m_idx[ty][tx];

        /* Target found at a reasonable distance */
        if (m_idx)
        {
            /* Get social */
            my_strcpy(text, s_ptr->text, sizeof(text));

            /* Display player line */
            strtok(text, "#");
            strtok(NULL, "#");
            t = strtok(NULL, "#");
            format_line(Ind, 0 - m_idx, t, line, sizeof(line));
            msg_print_aux(p_ptr, line, MSG_SOCIAL);

            /* Display nearby players line */
            t = strtok(NULL, "#");
            format_line(Ind, 0 - m_idx, t, line, sizeof(line));
            if (m_idx < 0)
                msg_print_complex_near(p_ptr, p_ptr, MSG_SOCIAL, line);
            else
                msg_print_complex_near(p_ptr, player_get(0 - m_idx), MSG_SOCIAL, line);

            /* Display target line */
            if (m_idx < 0)
            {
                t = strtok(NULL, "#");
                format_line(Ind, 0 - m_idx, t, line, sizeof(line));
                msg_print_aux(player_get(0 - m_idx), line, MSG_SOCIAL);
            }

            /* Done */
            return;
        }
    }

    /* Get social */
    my_strcpy(text, s_ptr->text, sizeof(text));

    /* Display player line */
    t = strtok(text, "#");
    msg_print_aux(p_ptr, t, MSG_SOCIAL);

    /* Display nearby players line */
    t = strtok(NULL, "#");
    format_line(Ind, Ind, t, line, sizeof(line));
    msg_print_complex_near(p_ptr, p_ptr, MSG_SOCIAL, line);
}               


/*
 * Display player information
 */
void describe_player(struct player *p, int who)
{
    player_type *q_ptr = player_get(who);
    char *s;
    char buf[240];
    const char *pm;

    switch (q_ptr->psex)
    {
        case SEX_FEMALE: pm = "She"; break;
        case SEX_MALE: pm = "He"; break;
        default: pm = "It"; break;
    }

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Describe name */
    text_out(p, q_ptr->name);
    text_out(p, ", the ");
    text_out(p, "%s", q_ptr->clazz->title[(q_ptr->lev - 1) / 5]);
    text_out(p, ".\n\n");
    text_out(p, "%s is %s %s ", pm,
        (is_a_vowel(tolower(q_ptr->race->name[0]))? "an": "a"), q_ptr->race->name);
    text_out_c(p, q_ptr->clazz->attr, q_ptr->clazz->name);
    text_out(p, ".\n\n");

    /* Describe History */
    strnfmt(buf, sizeof(buf), q_ptr->descrip);
    s = strtok(buf, " \n");
    while (s)
    {
        text_out(p, s);
        text_out(p, " ");
        s = strtok(NULL, " \n");
    }
    text_out(p, "\n");

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}
