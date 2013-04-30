/*
 * File: dungeon.c
 * Purpose: The game core bits, shared across platforms
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
#include "attack.h"
#include "cmds.h"
#include "files.h"
#include "generate.h"
#include "init.h"
#include "monster/mon-lore.h"
#include "monster/mon-make.h"
#include "monster/mon-spell.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "s-spells.h"
#include "target.h"
#include "wilderness.h"


/*
 * Change dungeon level - e.g. by going up stairs or with WoR
 */
void dungeon_change_level(struct player *p, int new_depth, byte new_level_method)
{
    /* Paranoia */
    if (!cave_get(p->depth))
    {
        Destroy_connection(p->conn, "Leaving an unallocated level, please report this bug!");
        return;
    }

    /* Paranoia: exit manual design */
    if (players_on_depth[p->depth] == INHIBIT_DEPTH)
        players_on_depth[p->depth] = 1;

    /* Remove the player */
    cave_get(p->depth)->m_idx[p->py][p->px] = 0;

    /* Redraw */
    cave_light_spot(cave_get(p->depth), p->py, p->px);

    /* Forget the view */
    forget_view(p);

    /* One less player here */
    leave_depth(p);

    /* Adjust player energy */
    p->energy = p->energy * level_speed(new_depth) / level_speed(p->depth);

    /* Paranoia */
    if (p->energy < 0) p->energy = 0;
    if (p->energy > level_speed(new_depth))
        p->energy = level_speed(new_depth);

    /* Set depth */
    p->depth = new_depth;

    /* One more player here */
    players_on_depth[new_depth]++;

    /* Generate a new level (later) */
    p->new_level_flag = TRUE;
    p->new_level_method = new_level_method;
    p->redraw |= (PR_DTRAP);
}


int find_player(s32b id)
{
    int i;

    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        if (p_ptr->id == id) return i;
    }

    /* Assume none */
    return 0;
}


/*
 * Regenerate hit points
 */
static void regenhp(int Ind, int percent)
{
    player_type *p_ptr = player_get(Ind);
    s32b new_chp, new_chp_frac;
    int old_chp;
    int old_num = get_player_num(p_ptr);

    /* Save the old hitpoints */
    old_chp = p_ptr->chp;

    /* Extract the new hitpoints */
    new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
    p_ptr->chp += (s16b)(new_chp >> 16);   /* div 65536 */

    /* Check for overflow */
    if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
    new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;    /* mod 65536 */
    if (new_chp_frac >= 0x10000L)
    {
        p_ptr->chp_frac = (u16b)(new_chp_frac - 0x10000L);
        p_ptr->chp++;
    }
    else
        p_ptr->chp_frac = (u16b)new_chp_frac;

    /* Fully healed */
    if (p_ptr->chp >= p_ptr->mhp)
    {
        p_ptr->chp = p_ptr->mhp;
        p_ptr->chp_frac = 0;
    }

    /* Notice changes */
    if (old_chp != p_ptr->chp)
    {
        /* Hack -- Redraw picture */
        redraw_picture(p_ptr, old_num);

        /* Redraw */
        p_ptr->redraw |= (PR_HP);
        wieldeds_notice_flag(p_ptr, OF_REGEN);
        wieldeds_notice_flag(p_ptr, OF_IMPAIR_HP);
    }
}


/*
 * Regenerate mana points
 */
static void regenmana(int Ind, int percent)
{
    player_type *p_ptr = player_get(Ind);
    s32b new_mana, new_mana_frac;
    int old_csp;
    int old_num = get_player_num(p_ptr);

    old_csp = p_ptr->csp;
    new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
    p_ptr->csp += (s16b)(new_mana >> 16);	/* div 65536 */

    /* Check for overflow */
    if ((p_ptr->csp < 0) && (old_csp > 0)) p_ptr->csp = MAX_SHORT;
    new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;  /* mod 65536 */
    if (new_mana_frac >= 0x10000L)
    {
        p_ptr->csp_frac = (u16b)(new_mana_frac - 0x10000L);
        p_ptr->csp++;
    }
    else
        p_ptr->csp_frac = (u16b)new_mana_frac;

    /* Must set frac to zero even if equal */
    if (p_ptr->csp >= p_ptr->msp)
    {
        p_ptr->csp = p_ptr->msp;
        p_ptr->csp_frac = 0;
    }

    /* Redraw mana */
    if (old_csp != p_ptr->csp)
    {
        /* Hack -- Redraw picture */
        redraw_picture(p_ptr, old_num);

        /* Redraw */
        p_ptr->redraw |= (PR_MANA);
        wieldeds_notice_flag(p_ptr, OF_REGEN);
        wieldeds_notice_flag(p_ptr, OF_IMPAIR_MANA);
    }
}


/*
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 *
 * Note that since this is done in real time, monsters will regenerate
 * faster in game time the deeper they are in the dungeon.
 */
static void regen_monsters(struct cave *c)
{
    int i, frac;
    double timed_frac;

    /* Regenerate everyone */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        /* Check the i'th monster */
        monster_type *m_ptr = cave_monster(c, i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip "unconscious" monsters */
        if (m_ptr->hp == 0) continue;

        /* Skip poisoned and bleeding monsters! */
        if (m_ptr->m_timed[MON_TMD_POIS] || m_ptr->m_timed[MON_TMD_CUT]) continue;

        /* Allow regeneration (if needed) */
        if (m_ptr->hp < m_ptr->maxhp)
        {
            /* Base regeneration: 1% of max hps */
            timed_frac = (double)m_ptr->maxhp / 100;

            /* If we are within a players time bubble, apply the time factor */
            if (m_ptr->closest_player)
                timed_frac *= ((double)time_factor(m_ptr->closest_player) / 100);

            /* Minimal regeneration rate: ensure 1 hp */
            frac = (int)timed_frac;
            if (!frac) frac = 1;

            /* Some monsters regenerate quickly */
            if (rf_has(r_ptr->flags, RF_REGENERATE)) frac *= 2;

            /* Regenerate */
            m_ptr->hp += frac;

            /* Do not over-regenerate */
            if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

            /* Update health bars */
            update_health(i);
        }
    }
}


static void play_ambient_sound(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Wilderness sound */
    if (p_ptr->depth < 0)
    {
        switch (wild_info[p_ptr->depth].type)
        {
            case WILD_SHORE: sound(p_ptr, MSG_WILD_SHORE); break;
            case WILD_GRASS: sound(p_ptr, MSG_WILD_GRASS); break;
            case WILD_WOOD: sound(p_ptr, MSG_WILD_WOOD); break;
            case WILD_SWAMP: sound(p_ptr, MSG_WILD_SWAMP); break;
            case WILD_WASTE: sound(p_ptr, MSG_WILD_WASTE); break;
            case WILD_MOUNTAIN: sound(p_ptr, MSG_WILD_MOUNTAIN); break;
            case WILD_VOLCANO: sound(p_ptr, MSG_WILD_VOLCANO); break;
        }
    }

    /* Town sound */
    else if (!p_ptr->depth)
    {
        /* Hack - is it daytime or nighttime? */
        if (is_daytime())
        {
            /* It's day. */
            sound(p_ptr, MSG_AMBIENT_DAY);
        }
        else
        {
            /* It's night. */
            sound(p_ptr, MSG_AMBIENT_NITE);
        }
    }

    /* Dungeon level 1-20 */
    else if (p_ptr->depth <= 20)
        sound(p_ptr, MSG_AMBIENT_DNG1);

    /* Dungeon level 21-40 */
    else if (p_ptr->depth <= 40)
        sound(p_ptr, MSG_AMBIENT_DNG2);

    /* Dungeon level 41-60 */
    else if (p_ptr->depth <= 60)
        sound(p_ptr, MSG_AMBIENT_DNG3);

    /* Dungeon level 61-80 */
    else if (p_ptr->depth <= 80)
        sound(p_ptr, MSG_AMBIENT_DNG4);

    /* Dungeon level 81-98 */
    else if (p_ptr->depth <= 98)
        sound(p_ptr, MSG_AMBIENT_DNG5);

    /* Dungeon level 99 */
    else if (p_ptr->depth == 99)
        sound(p_ptr, MSG_AMBIENT_SAURON);

    /* Dungeon level 100 */
    else if (p_ptr->depth == 100)
        sound(p_ptr, MSG_AMBIENT_MORGOTH);

    /* Dungeon level 101-125 */
    else if (p_ptr->depth <= 125)
        sound(p_ptr, MSG_AMBIENT_DNG6);

    /* Dungeon level 126 */
    else if (p_ptr->depth == 126)
        sound(p_ptr, MSG_AMBIENT_SENYA);

    /* Dungeon level 127 */
    else
        sound(p_ptr, MSG_AMBIENT_XAKAZE);
}


/*
 * Handle certain things once every 50 game turns
 */
static void process_world(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;

    /* Every 50 game turns */
    if (turn.turn % 50) return;

    /*** Check the Time ***/

    /* Play an ambient sound at regular intervals. */
    if (!(turn.turn % ((10L * TOWN_DAWN) / 4))) play_ambient_sound(Ind);

    /*** Handle the "town" (stores and sunshine) ***/

    /* While in town or wilderness */
    if (p_ptr->depth <= 0)
    {
        /* Hack -- Daybreak/Nighfall */
        if (!(turn.turn % (10L * TOWN_DUSK)))
        {
            bool dawn;

            /* Check for dawn */
            dawn = (!(turn.turn % (10L * TOWN_DAWN)));

            /* Day breaks */
            if (dawn) msg(p_ptr, "The sun has risen.");

            /* Night falls */
            else msg(p_ptr, "The sun has fallen.");

            /* Clear the flags for each cave grid */
            clear_cave_info(p_ptr, FALSE);

            /* Illuminate */
            cave_illuminate(p_ptr, cave_get(p_ptr->depth), dawn);

            /* Hack -- Regenerate crops */
            for (y = 0; y < DUNGEON_HGT; y++)
            {
                for (x = 0; x < DUNGEON_WID; x++)
                {
                    /* Regenerate crops */
                    if ((cave_get(p_ptr->depth)->feat[y][x] == FEAT_CROP) &&
                        !cave_get(p_ptr->depth)->o_idx[y][x] &&
                        !cave_get(p_ptr->depth)->m_idx[y][x] && dawn && one_in_(16))
                    {
                        /* Add crop to that location */
                        wild_add_crop(p_ptr->depth, x, y, randint0(7));
                    }
                }
            }
        }
    }

    /*** Process the monsters ***/

    /* Hack -- DM redesigning the level */
    if (players_on_depth[p_ptr->depth] == INHIBIT_DEPTH) return;

    /*
     * Note : since monsters are added at a constant rate in real time,
     * this corresponds in game time to them appearing at faster rates
     * deeper in the dungeon.
     */

    /* Check for creature generation */
    /* Hack -- Increase respawn rate on no_recall servers */
    if (one_in_(cfg_no_recall? MAX_M_ALLOC_CHANCE / 2: MAX_M_ALLOC_CHANCE))
    {
        /* Make a new monster */
        if (p_ptr->depth > 0)
            pick_and_place_distant_monster(p_ptr, cave_get(p_ptr->depth), MAX_SIGHT + 5, 0);
        else if (!p_ptr->depth)
            pick_and_place_distant_monster(p_ptr, cave_get(p_ptr->depth), MAX_SIGHT + 5, MON_SLEEP);
        else
            wild_add_monster(p_ptr, cave_get(p_ptr->depth));
    }
}


/*
 * Check for nearby players/monsters and attack the current target.
 */
static void auto_retaliate(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i, n = 0, target_x[8], target_y[8];
    int tx, ty, m_idx;
    bool found = FALSE;
    struct cave *c = cave_get(p_ptr->depth);

    /* Hack -- Shoppers don't auto-retaliate */
    if (in_store(p_ptr)) return;

    /* The dungeon master does not auto-retaliate */
    if (p_ptr->dm_flags & DM_MONSTER_FRIEND) return;

    /* Not while confused */
    if (p_ptr->timed[TMD_CONFUSED]) return;

    /* Check preventive inscription '^O' */
    if (CPI(p_ptr, 'O')) return;

    /* Try to find valid targets around us */
    for (i = 0; i < 8; i++)
    {
        bool hostile, visible, mimicking;

        /* Current location */
        tx = p_ptr->px + ddx_ddd[i];
        ty = p_ptr->py + ddy_ddd[i];
        m_idx = c->m_idx[ty][tx];

        /* Paranoia */
        if (!in_bounds_fully(ty, tx)) continue;

        /* Nobody here */
        if (!m_idx) continue;

        /* Target info */
        if (m_idx < 0)
        {
            hostile = pvp_check(p_ptr, player_get(0 - m_idx), PVP_CHECK_BOTH, TRUE, c->feat[ty][tx]);
            visible = p_ptr->play_vis[0 - m_idx];
            mimicking = (player_get(0 - m_idx)->k_idx != 0);
        }
        else
        {
            hostile = pvm_check(Ind, m_idx);
            visible = p_ptr->mon_vis[m_idx];
            mimicking = is_mimicking(cave_monster(c, m_idx));
        }

        /* If hostile and visible, it's a fair target (except unaware mimics) */
        if (hostile && visible && !mimicking)
        {
            target_x[n] = tx;
            target_y[n] = ty;
            n++;
        }
    }

    /* No valid target around */
    if (!n) return;

    /* If there's a current target, attack it (always) */
    if (p_ptr->health_who)
    {
        for (i = 0; i < n; i++)
        {
            /* Current location */
            tx = target_x[i];
            ty = target_y[i];
            m_idx = c->m_idx[ty][tx];

            /* Not the current target */
            if (p_ptr->health_who != m_idx) continue;

            /* Current target found */
            found = TRUE;
            break;
        }
    }

    /* If there's at least one valid target around, attack one (active auto-retaliator only) */
    if (OPT_P(p_ptr, active_auto_retaliator) && !found)
    {
        /* Choose randomly */
        i = randint0(n);
        tx = target_x[i];
        ty = target_y[i];
        m_idx = c->m_idx[ty][tx];
        found = TRUE;
    }

    /* No current target */
    if (!found) return;

    /* Handle player fear */
    if (check_state(p_ptr, OF_AFRAID))
    {
        char target_name[NORMAL_WID];

        if (m_idx > 0)
            monster_desc(p_ptr, target_name, sizeof(target_name), cave_monster(c, m_idx), 0);
        else
            my_strcpy(target_name, player_get(0 - m_idx)->name, sizeof(target_name));

        /* Message (only once per player turn) */
        if (!p_ptr->is_afraid)
            msgt(p_ptr, MSG_AFRAID, "You are too afraid to attack %s!", target_name);
        p_ptr->is_afraid = TRUE;

        /* Done */
        return;
    }

    /* Attack the current target */
    py_attack(Ind, ty, tx);

    /* Take a turn */
    use_energy(Ind);
}


/*
 * Helper for process_world -- Decrement p_ptr->timed[] fields.
 */
static void decrease_timeouts(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int adjust = (adj_con_fix[p_ptr->state.stat_ind[A_CON]] + 1);
    int i;

    /* Decrement all effects that can be done simply */
    for (i = 0; i < TMD_MAX; i++)
    {
        int decr = 1;
        if (!p_ptr->timed[i]) continue;

        switch (i)
        {
            case TMD_CUT:
            {
                /* Hack -- Check for truly "mortal" wound */
                decr = ((p_ptr->timed[i] > 1000)? 0: adjust);

                /* Biofeedback always helps */
                if (p_ptr->timed[TMD_BIOFEEDBACK]) decr += decr + 10;
                break;
            }

            case TMD_POISONED:
            case TMD_STUN:
            {
                decr = adjust;
                break;
            }

            case TMD_WRAITH:
            {
                /* Hack -- Must be in bounds */
                if (!players_on_depth[p_ptr->depth] || !in_bounds_fully(p_ptr->py, p_ptr->px))
                    decr = 0;
                break;
            }
        }

        /* Hack -- Make -1 permanent */
        if (p_ptr->timed[i] == -1) decr = 0;

        /* Decrement the effect */
        if (decr > 0) player_dec_timed(p_ptr, i, decr, FALSE);
    }
}


/*
 * Hack -- Helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void player_track_monster(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    monster_lore lore;
    bool changed = FALSE;

    /* Tracking a monster */
    if (p_ptr->monster_race_idx <= 0) return;

    /* Get the lores (player + global) */
    get_global_lore(p_ptr, p_ptr->monster_race_idx, &lore);

    /* Check for change of any kind */
    if (lore.spawned != p_ptr->current_lore.spawned)
    {
        p_ptr->current_lore.spawned = lore.spawned;
        changed = TRUE;
    }
    if (lore.seen != p_ptr->current_lore.seen)
    {
        p_ptr->current_lore.seen = lore.seen;
        changed = TRUE;
    }
    if (lore.pseen != p_ptr->current_lore.pseen)
    {
        p_ptr->current_lore.pseen = lore.pseen;
        changed = TRUE;
    }
    if (lore.pdeaths != p_ptr->current_lore.pdeaths)
    {
        p_ptr->current_lore.pdeaths = lore.pdeaths;
        changed = TRUE;
    }
    if (lore.tdeaths != p_ptr->current_lore.tdeaths)
    {
        p_ptr->current_lore.tdeaths = lore.tdeaths;
        changed = TRUE;
    }
    if (lore.pkills != p_ptr->current_lore.pkills)
    {
        p_ptr->current_lore.pkills = lore.pkills;
        changed = TRUE;
    }
    if (lore.tkills != p_ptr->current_lore.tkills)
    {
        p_ptr->current_lore.tkills = lore.tkills;
        changed = TRUE;
    }
    if (lore.wake != p_ptr->current_lore.wake)
    {
        p_ptr->current_lore.wake = lore.wake;
        changed = TRUE;
    }
    if (lore.ignore != p_ptr->current_lore.ignore)
    {
        p_ptr->current_lore.ignore = lore.ignore;
        changed = TRUE;
    }
    if (lore.drop_gold != p_ptr->current_lore.drop_gold)
    {
        p_ptr->current_lore.drop_gold = lore.drop_gold;
        changed = TRUE;
    }
    if (lore.drop_item != p_ptr->current_lore.drop_item)
    {
        p_ptr->current_lore.drop_item = lore.drop_item;
        changed = TRUE;
    }
    if (lore.cast_innate != p_ptr->current_lore.cast_innate)
    {
        p_ptr->current_lore.cast_innate = lore.cast_innate;
        changed = TRUE;
    }
    if (lore.cast_spell != p_ptr->current_lore.cast_spell)
    {
        p_ptr->current_lore.cast_spell = lore.cast_spell;
        changed = TRUE;
    }
    if (0 != memcmp(p_ptr->current_lore.blows, lore.blows, sizeof(byte) * MONSTER_BLOW_MAX))
    {
        memmove(p_ptr->current_lore.blows, lore.blows, sizeof(byte) * MONSTER_BLOW_MAX);
        changed = TRUE;
    }
    if (!rf_is_equal(p_ptr->current_lore.flags, lore.flags))
    {
        rf_copy(p_ptr->current_lore.flags, lore.flags);
        changed = TRUE;
    }
    if (!rsf_is_equal(p_ptr->current_lore.spell_flags, lore.spell_flags))
    {
        rsf_copy(p_ptr->current_lore.spell_flags, lore.spell_flags);
        changed = TRUE;
    }

    /* Redraw */
    if (changed) p_ptr->redraw |= (PR_MONSTER);
}


/*
 * Let the player know when an object is recharged.
 * Also inform player when first item of a stack has recharged.
 */
static void recharged_notice(int Ind, const object_type *o_ptr, bool all)
{
    player_type *p_ptr = player_get(Ind);
    char o_name[NORMAL_WID];

    if (!OPT_P(p_ptr, notify_recharge)) return;

    /* Describe (briefly) */
    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

    /* Disturb the player */
    disturb(p_ptr, 0, 0);

    /* Notify the player */
    if (o_ptr->number > 1)
    {
        if (all) msg(p_ptr, "Your %s have recharged.", o_name);
        else msg(p_ptr, "One of your %s has recharged.", o_name);
    }

    /* Artifacts */
    else if (o_ptr->artifact)
        msg(p_ptr, "The %s has recharged.", o_name);

    /* Single, non-artifact items */
    else msg(p_ptr, "Your %s has recharged.", o_name);
}


/*
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory. Decompose carried corpses slowly.
 */
static void recharge_objects(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;
    bool charged = FALSE, discharged_stack;
    object_type *o_ptr;

    /*** Recharge equipment ***/
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        /* Get the object */
        o_ptr = &p_ptr->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Recharge activatable objects */
        if (recharge_timeout(o_ptr))
        {
            charged = TRUE;

            /* Message if an item recharged */
            recharged_notice(Ind, o_ptr, TRUE);
        }
    }

    /* Notice changes */
    if (charged)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_EQUIP);
    }

    /* Check if we are shopping (fixes stacking exploits) */
    if (in_store(p_ptr)) return;

    charged = FALSE;

    /*** Recharge the inventory ***/
    for (i = INVEN_PACK - 1; i >= 0; i--)
    {
        int temp = 0;

        o_ptr = &p_ptr->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        discharged_stack = (number_charging(o_ptr) == o_ptr->number)? TRUE: FALSE;

        /* Recharge rods, and update if any rods are recharged */
        if ((o_ptr->tval == TV_ROD) && recharge_timeout(o_ptr))
        {
            charged = TRUE;

            /* Entire stack is recharged */
            if (!o_ptr->timeout) recharged_notice(Ind, o_ptr, TRUE);

            /* Previously exhausted stack has acquired a charge */
            else if (discharged_stack) recharged_notice(Ind, o_ptr, FALSE);
        }

        /* Handle corpse decay */
        if (o_ptr->tval == TV_CORPSE)
        {
            char o_name[NORMAL_WID];
            object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

            /* Corpses slowly decompose */
            o_ptr->pval[DEFAULT_PVAL + 1]--;

            /* Notice changes */
            if (o_ptr->pval[DEFAULT_PVAL + 1] == o_ptr->timeout / 5)
            {
                charged = TRUE;

                /* Rotten corpse */
                if (o_ptr->number > 1)
                    msg(p_ptr, "Your %s slowly rot away.", o_name);
                else
                    msg(p_ptr, "Your %s slowly rots away.", o_name);
            }
            else if (!o_ptr->pval[DEFAULT_PVAL + 1])
            {
                charged = TRUE;

                /* No more corpse... */
                if (o_ptr->number > 1)
                    msg(p_ptr, "Your %s rot away.", o_name);
                else
                    msg(p_ptr, "Your %s rots away.", o_name);
                inven_item_increase(p_ptr, i, 0 - o_ptr->number);
                inven_item_optimize(p_ptr, i);
            }
        }
    }

    /* Notice changes */
    if (charged)
    {
        /* Combine pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Redraw */
        p_ptr->redraw |= (PR_INVEN);
    }
}


/*
 * Handles "global" things on the server
 */
static void process_various(void)
{
    player_type *p_ptr;
    int i, j;

    /* Purge the player database occasionally */
    if (!(turn.turn % (cfg_fps * 60 * 60 * SERVER_PURGE)))
        purge_player_names();

    /* Save the server state occasionally */
    if (!(turn.turn % (cfg_fps * 60 * SERVER_SAVE)))
    {
        save_server_info();

        /* Save each player */
        for (i = 1; i <= NumPlayers; i++)
        {
            p_ptr = player_get(i);
            if (p_ptr->leaving) continue;

            /* Save this player */
            save_player(i);
        }
    }

    /* Handle certain things once a minute */
    if (!(turn.turn % (cfg_fps * 60)))
    {
        /* Update the player retirement timers */
        for (i = 1; i <= NumPlayers; i++)
        {
            p_ptr = player_get(i);
            if (p_ptr->leaving) continue;

            /* If our retirement timer is set */
            if (p_ptr->retire_timer > 0)
            {
                /* Decrement our retire timer */
                p_ptr->retire_timer--;

                /* If the timer runs out, forcibly retire this character */
                if (!p_ptr->retire_timer) do_cmd_suicide(i);
            }
        }

        /* If the level unstaticer is not disabled */
        if (cfg_level_unstatic_chance >= 0)
        {
            /* For each dungeon level */
            for (i = 1; i < MAX_DEPTH; i++)
            {
                /* If this depth is static */
                if (players_on_depth[i])
                {
                    int num_on_depth = 0;

                    /* Count the number of players actually in game on this depth */
                    for (j = 1; j < NumPlayers + 1; j++)
                    {
                        p_ptr = player_get(j);
                        if (p_ptr->leaving) continue;
                        if (p_ptr->depth == i) num_on_depth++;
                    }

                    /* If this level is static and no one is actually on it */
                    if (!num_on_depth)
                    {
                        /* The chance is one in (base_chance * depth) / 250 feet */
                        int chance = (cfg_level_unstatic_chance * (i + 5) / 5) - 1;

                        /* Full unstaticer */
                        if (!cfg_level_unstatic_chance) chance = 0;

                        /* Random chance of the level unstaticing */
                        if (one_in_(chance))
                        {
                            /* Unstatic the level */
                            players_on_depth[i] = 0;
                        }
                    }
                }
            }
        }
    }

    /* Grow trees very occasionally */
    if (!(turn.turn % (10L * GROW_TREE)) && ((trees_in_town < cfg_max_trees) || (cfg_max_trees == -1)))
    {
        /* Find a suitable location */
        for (i = 1; i < 1000; i++)
        {
            /* Pick a location */
            int y = rand_range(1, DUNGEON_HGT - 1);
            int x = rand_range(1, DUNGEON_WID - 1);

            /* Only allow "dirt" */
            if (cave_get(0)->feat[y][x] != FEAT_DIRT) continue;

            /* Never grow on top of objects or monsters */
            if (cave_get(0)->m_idx[y][x]) continue;
            if (cave_get(0)->o_idx[y][x]) continue;

            /* Grow a tree here */
            cave_set_feat(cave_get(0), y, x, FEAT_TREE);
            trees_in_town++;

            /* Done */
            break;
        }
    }

    /* Update the stores */
    if (!(turn.turn % (10L * STORE_TURNS)))
    {
        int n;

        /* Maintain each shop */
        for (n = 0; n < MAX_STORES; n++)
        {
            /* Maintain */
            store_maint(&stores[n], FALSE);
        }

        /* Sometimes, shuffle the shopkeepers */
        if (one_in_(STORE_SHUFFLE))
        {
            /* Shuffle a random shop (except tavern and player store) */
            store_shuffle(&stores[randint0(MAX_STORES - 2)], FALSE);
        }
    }

    /* Hack -- Prevent wilderness monster "buildup" */
    for (i = 1; i <= MAX_WILD; i++)
    {
        if (cave_get(0 - i) && !players_on_depth[0 - i] && !(turn.turn % (10L * TOWN_DUSK)) &&
            !(turn.turn % (10L * TOWN_DAWN)))
        {
            wipe_mon_list(cave_get(0 - i));
        }
    }
}


/*
 * Digest some food
 */
static void digest_food(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Ghosts don't need food */
    if (p_ptr->ghost) return;

    /* Don't use food in the town or on special levels */
    if (forbid_special(p_ptr->depth)) return;

    /* Don't use food near town (to avoid starving in one's own house) */
    if (town_area(p_ptr->depth)) return;

    /* Digest normally */
    if (p_ptr->food < PY_FOOD_MAX)
    {
        /* Basic digestion rate based on speed */
        i = get_energy(p_ptr->state.speed) * 2;

        /* Some effects require more food */
        if (p_ptr->timed[TMD_ADRENALINE]) i *= 5;
        if (p_ptr->timed[TMD_HARMONY]) i *= 5;
        if (p_ptr->timed[TMD_BIOFEEDBACK]) i *= 2;

        /* Regeneration takes more food */
        if (check_state(p_ptr, OF_REGEN)) i += 30;
        if (p_ptr->timed[TMD_REGEN]) i += 30;

        /* Slow digestion takes less food */
        if (check_state(p_ptr, OF_SLOW_DIGEST)) i -= 10;

        /* Digest some food */
        player_set_food(p_ptr, p_ptr->food - i);
    }

    /* Digest quickly when gorged */
    else
    {
        /* Digest a lot of food */
        player_set_food(p_ptr, p_ptr->food - 100);
    }

    /* Getting Faint */
    if (p_ptr->food < PY_FOOD_FAINT)
    {
        /* Faint occasionally */
        if (!p_ptr->timed[TMD_PARALYZED] && magik(10))
        {
            /* Message */
            msg(p_ptr, "You faint from the lack of food.");
            disturb(p_ptr, 1, 0);

            /* Hack -- Faint (bypass free action) */
            player_inc_timed(p_ptr, TMD_PARALYZED, 1 + randint0(5), TRUE, FALSE);
        }
    }

    /* Starve to death (slowly) */
    if (p_ptr->food < PY_FOOD_STARVE)
    {
        /* Calculate damage */
        i = (PY_FOOD_STARVE - p_ptr->food) / 10;

        /* Take damage */
        my_strcpy(p_ptr->died_flavor,
            "starved to death", sizeof(p_ptr->died_flavor));
        take_hit(p_ptr, i, "starvation", FALSE);
    }
}


/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 *
 * Notice the annoying code to handle "monster memory" changes,
 * which allows us to avoid having to update the window flags
 * every time we change any internal monster memory field, and
 * also reduces the number of times that the recall window must
 * be redrawn.
 *
 * Note that the code to check for user abort during repeated commands
 * and running and resting can be disabled entirely with an option, and
 * even if not disabled, it will only check during every 128th game turn
 * while resting, for efficiency.
 */
static void process_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i, time, timefactor;
    int regen_amount;
    object_type *o_ptr;

    /* Try to execute any commands on the command queue. */
    /* NB: process_pending may have deleted the connection! */
    if (process_pending_commands(p_ptr->conn)) return;

    /* Handle "leaving" */
    if (p_ptr->new_level_flag) return;

    /* If we are are in a slow time condition, give visual warning */
    timefactor = base_time_factor(p_ptr, 0);
    if (timefactor < NORMAL_TIME)
        cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

    /* Handle running */
    if (has_energy(Ind) && p_ptr->running) run_step(Ind);

    /* Handle searching */
    if (p_ptr->search_request)
    {
        p_ptr->search_request--;

        /* Only take a turn if attempted */
        if (has_energy(Ind) && search(Ind, TRUE)) use_energy(Ind);
    }

    /* Check for auto-retaliate */
    if (has_energy(Ind)) auto_retaliate(Ind);

    /* Handle "leaving" */
    if (p_ptr->new_level_flag) return;

    /* Notice stuff */
    notice_stuff(p_ptr);

    /* Hack -- Pack Overflow */
    pack_overflow(Ind);

    /* Process things such as regeneration, poison, cuts, etc. */

    /* Determine basic frequency of regen in game turns */
    time = level_speed(p_ptr->depth) / 1000;

    /* Scale frequency by players local time bubble */
    time = time / ((double)timefactor / 100);

    /* Use food, 10 times slower than other regen effects */
    if (!(turn.turn % (time * 10))) digest_food(Ind);

    if (!(turn.turn % time))
    {
        /* Hack -- Fade monster Detect over time */
        for (i = 1; i < cave_monster_max(cave_get(p_ptr->depth)); i++)
        {
            if (p_ptr->mon_det[i])
            {
                if (--p_ptr->mon_det[i] == 0) update_mon(p_ptr->depth, i, FALSE);
            }
        }

        /* Hack -- Fade player Detect over time */
        for (i = 1; i <= NumPlayers; i++)
        {
            if (p_ptr->play_det[i])
            {
                if (--p_ptr->play_det[i] == 0) update_player(i);
            }
        }

        /* Hack -- Semi-constant hallucination (but not in stores) */
        if (p_ptr->timed[TMD_IMAGE] && !in_store(p_ptr)) p_ptr->redraw |= (PR_MAP);

        /*** Damage over Time ***/

        /* Take damage from Undead Form */
        if (player_undead(p_ptr))
        {
            /* Take damage */
            take_hit(p_ptr, 1, "fading", FALSE);
        }

        /* Take damage from poison */
        if (p_ptr->timed[TMD_POISONED])
        {
            /* Take damage */
            my_strcpy(p_ptr->died_flavor,
                "died of blood poisoning", sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, 1, "poison", FALSE);
        }

        /* Take damage from cuts */
        if (p_ptr->timed[TMD_CUT])
        {
            /* Mortal wound or Deep Gash */
            if (p_ptr->timed[TMD_CUT] > 200) i = 3;

            /* Severe cut */
            else if (p_ptr->timed[TMD_CUT] > 100) i = 2;

            /* Other cuts */
            else i = 1;

            /* Take damage */
            my_strcpy(p_ptr->died_flavor, "bled to death", sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, i, "a fatal wound", FALSE);
        }

        /* Take damage from drowning */
        if ((cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px] == FEAT_WATER) &&
            !player_passwall(p_ptr) && !can_swim(p_ptr))
        {
            msg(p_ptr, "You are drowning!");
            my_strcpy(p_ptr->died_flavor, "drowned", sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, p_ptr->mhp / 100 + randint1(3), "drowning", FALSE);
        }

        /* Take damage from fire */
        if ((cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px] == FEAT_LAVA) &&
            !player_passwall(p_ptr))
        {
            int damage = p_ptr->mhp / 100 + randint1(3);

            msg(p_ptr, "You are hit by fire!");
            my_strcpy(p_ptr->died_flavor, "took a bath in molten lava", sizeof(p_ptr->died_flavor));
            damage = adjust_dam(p_ptr, GF_FIRE, damage, RANDOMISE,
                check_for_resist(p_ptr, GF_FIRE, TRUE));
            if (damage)
            {
                if (!take_hit(p_ptr, damage, "molten lava", FALSE))
                    inven_damage(p_ptr, GF_FIRE, MIN(damage * 5, 300));
            }
        }

        /*** Check the Food, and Regenerate ***/

        /** Regenerate HP **/

        /* Default regeneration */
        if (p_ptr->food >= PY_FOOD_WEAK) regen_amount = PY_REGEN_NORMAL;
        else if (p_ptr->food < PY_FOOD_STARVE) regen_amount = 0;
        else if (p_ptr->food < PY_FOOD_FAINT) regen_amount = PY_REGEN_FAINT;
        else regen_amount = PY_REGEN_WEAK;

        /* Various things speed up regeneration */
        if (check_state(p_ptr, OF_REGEN)) regen_amount *= 2;
        if (p_ptr->resting || p_ptr->searching) regen_amount *= 2;
        if (p_ptr->timed[TMD_REGEN]) regen_amount *= 3;

        /* Some things slow it down */
        if (check_state(p_ptr, OF_IMPAIR_HP)) regen_amount /= 2;

        /* Various things interfere with physical healing */
        if (p_ptr->timed[TMD_PARALYZED]) regen_amount = 0;
        if (p_ptr->timed[TMD_POISONED]) regen_amount = 0;
        if (p_ptr->timed[TMD_STUN]) regen_amount = 0;
        if (p_ptr->timed[TMD_CUT]) regen_amount = 0;
        if (player_undead(p_ptr)) regen_amount = 0;

        /* But Biofeedback always helps */
        if (p_ptr->timed[TMD_BIOFEEDBACK])
            regen_amount += randint1(0x400) + regen_amount;

        /* Regenerate Hit Points if needed */
        if (p_ptr->chp < p_ptr->mhp) regenhp(Ind, regen_amount);

        /** Regenerate SP **/

        /* Default regeneration */
        regen_amount = PY_REGEN_NORMAL;

        /* Various things speed up regeneration */
        if (check_state(p_ptr, OF_REGEN)) regen_amount *= 2;
        if (p_ptr->resting || p_ptr->searching) regen_amount *= 2;

        /* Some things slow it down */
        if (check_state(p_ptr, OF_IMPAIR_MANA)) regen_amount /= 2;

        /* Regenerate mana */
        if (p_ptr->csp < p_ptr->msp) regenmana(Ind, regen_amount);

        /*** Check for interrupts ***/

        /* Complete resting */
        if (p_ptr->resting < 0)
        {
            /* Basic resting */
            if (p_ptr->resting == REST_ALL_POINTS)
            {
                /* Stop resting */
                if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp == p_ptr->msp))
                    disturb(p_ptr, 0, 0);
            }

            /* Complete resting */
            else if (p_ptr->resting == REST_COMPLETE)
            {
                /* Stop resting */
                if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp == p_ptr->msp) &&
                    !p_ptr->timed[TMD_BLIND] && !p_ptr->timed[TMD_CONFUSED] &&
                    !p_ptr->timed[TMD_POISONED] && !p_ptr->timed[TMD_AFRAID] &&
                    !p_ptr->timed[TMD_TERROR] &&
                    !p_ptr->timed[TMD_STUN] && !p_ptr->timed[TMD_CUT] &&
                    !p_ptr->timed[TMD_SLOW] && !p_ptr->timed[TMD_PARALYZED] &&
                    !p_ptr->timed[TMD_IMAGE] && !p_ptr->word_recall)
                {
                    disturb(p_ptr, 0, 0);
                }
            }

            /* Rest until HP or SP are filled */
            else if (p_ptr->resting == REST_SOME_POINTS)
            {
                /* Stop resting */
                if ((p_ptr->chp == p_ptr->mhp) || (p_ptr->csp == p_ptr->msp))
                    disturb(p_ptr, 0, 0);
            }
        }

        /* Dwarves detect treasure */
        if (player_has(p_ptr, PF_SEE_ORE))
        {
            /* Only if they are in good shape */
            if (!p_ptr->timed[TMD_IMAGE] && !p_ptr->timed[TMD_CONFUSED] &&
                !p_ptr->timed[TMD_AMNESIA] && !p_ptr->timed[TMD_STUN] &&
                !p_ptr->timed[TMD_PARALYZED] && !p_ptr->timed[TMD_TERROR] &&
                !p_ptr->timed[TMD_AFRAID])
            {
                detect_close_buried_treasure(p_ptr);
            }
        }

        /*** Timeout Various Things ***/

        decrease_timeouts(Ind);

        /* Timed rest */
        if (p_ptr->resting > 0)
        {
            /* Reduce rest count */
            p_ptr->resting--;

            /* Redraw the state */
            if (p_ptr->resting == 0) p_ptr->redraw |= (PR_STATE);
        }

        /* Quest */
        if (p_ptr->quest.timer > 0)
        {
            p_ptr->quest.timer--;
            if (p_ptr->quest.timer == 0)
            {
                /* Failed quest */
                msg(p_ptr, "You have failed your quest.");
                p_ptr->quest.r_idx = 0;
                p_ptr->quest.cur_num = 0;
                p_ptr->quest.max_num = 0;
            }
        }

        /*** Process Light ***/

        /* Check for light being wielded */
        o_ptr = &p_ptr->inventory[INVEN_LIGHT];

        /* Burn some fuel in the current light */
        if (o_ptr->tval == TV_LIGHT)
        {
            bitflag f[OF_SIZE];
            bool burn_fuel = TRUE;

            /* Get the object flags */
            object_flags(o_ptr, f);

            /* Turn off the wanton burning of light during the day outside of the dungeon */
            if ((p_ptr->depth <= 0) && is_daytime())
                burn_fuel = FALSE;

            /* If the light has the NO_FUEL flag, well... */
            if (of_has(f, OF_NO_FUEL))
                burn_fuel = FALSE;

            /* Use some fuel (except on artifacts, or during the day) */
            if (burn_fuel && (o_ptr->timeout > 0))
            {
                /* Decrease life-span */
                o_ptr->timeout--;

                /* Hack -- Notice interesting fuel steps */
                if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
                {
                    /* Redraw */
                    p_ptr->redraw |= (PR_EQUIP);
                }

                /* Hack -- Special treatment when blind */
                if (p_ptr->timed[TMD_BLIND])
                {
                    /* Hack -- save some light for later */
                    if (o_ptr->timeout == 0) o_ptr->timeout++;
                }

                /* The light is now out */
                else if (o_ptr->timeout == 0)
                {
                    disturb(p_ptr, 0, 0);
                    msg(p_ptr, "Your light has gone out!");

                    /* If it's a torch, now is the time to delete it */
                    if (o_ptr->sval == SV_LIGHT_TORCH)
                    {
                        inven_item_increase(p_ptr, INVEN_LIGHT, -1);
                        inven_item_optimize(p_ptr, INVEN_LIGHT);
                    }
                }

                /* The light is getting dim */
                else if ((o_ptr->timeout < 50) && (!(o_ptr->timeout % 20)))
                {
                    disturb(p_ptr, 0, 0);
                    msg(p_ptr, "Your light is growing faint.");
                }
            }
        }

        /* Calculate torch radius */
        p_ptr->update |= (PU_TORCH);

        /*** Process Inventory ***/

        /* Handle experience draining */
        if (check_state(p_ptr, OF_DRAIN_EXP))
        {
            if (magik(10) && (p_ptr->exp > 0))
            {
                s32b d = damroll(10, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

                player_exp_lose(p_ptr, d / 10, FALSE);
            }

            wieldeds_notice_flag(p_ptr, OF_DRAIN_EXP);
        }

        /* Recharge activatable objects and rods */
        recharge_objects(Ind);

        /* Feel the inventory */
        sense_inventory(Ind);

        /*** Involuntary Movement ***/

        /* Random teleportation */
        if (check_state(p_ptr, OF_TELEPORT) && one_in_(100))
        {
            wieldeds_notice_flag(p_ptr, OF_TELEPORT);
            teleport_player(p_ptr, 40);
            disturb(p_ptr, 0, 0);
        }

        /* Delayed Word-of-Recall */
        if (p_ptr->word_recall)
        {
            /* Count down towards recall */
            p_ptr->word_recall--;

            /* Activate the recall */
            if (!p_ptr->word_recall)
            {
                /* Hack -- No recall if in a shop, or under the influence of space/time anchor */
                if (in_store(p_ptr) || check_st_anchor(p_ptr->depth, p_ptr->py, p_ptr->px))
                    p_ptr->word_recall++;
                else
                {
                    if (recall_player(Ind)) return;

                    /* Failure */
                    p_ptr->word_recall++;
                }
            }
        }

        /* Delayed Deep Descent */
        if (p_ptr->deep_descent)
        {
            /* Count down towards recall */
            p_ptr->deep_descent--;

            /* Activate the recall */
            if (!p_ptr->deep_descent)
            {
                /* Hack -- Not if in a shop, or under the influence of space/time anchor */
                if (in_store(p_ptr) || check_st_anchor(p_ptr->depth, p_ptr->py, p_ptr->px))
                    p_ptr->deep_descent++;
                else
                {
                    if (deep_descent(p_ptr, TRUE)) return;

                    /* Failure */
                    p_ptr->deep_descent++;
                }
            }
        }
    }

    /* Only when needed, every five game turns */
    if (!(turn.turn % 5))
    {
        /* Handle polymorphed players */
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        /* Flicker self if multi-hued */
        if (monster_shimmer(r_ptr) && allow_shimmer(p_ptr))
            cave_light_spot_aux(p_ptr, cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

        /* Flicker multi-hued players, party leaders and elementalists */
        if (p_ptr->shimmer)
        {
            int j;

            /* Check everyone */
            for (j = 1; j < NumPlayers + 1; j++)
            {
                player_type *q_ptr = player_get(j);

                /* If he's not here, skip him */
                if (q_ptr->depth != p_ptr->depth) continue;

                /* Flicker multi-hued players */
                if (monster_shimmer(r_ptr) && allow_shimmer(q_ptr))
                    cave_light_spot_aux(q_ptr, cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

                /* Flicker party leaders */
                if (is_party_owner(q_ptr, p_ptr) && OPT_P(q_ptr, highlight_leader))
                    cave_light_spot_aux(q_ptr, cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

                /* Flicker elementalists */
                if (player_has(p_ptr, PF_ELEMENTAL_SPELLS) && allow_shimmer(q_ptr))
                    cave_light_spot_aux(q_ptr, cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);
            }
        }
    }

    /* Track monster */
    player_track_monster(Ind);

    /* Refresh stuff */
    refresh_stuff(Ind);
}


static void place_player(int Ind, int starty, int startx)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, d, j;

    /* Try to find an empty space */
    for (j = 0; j < 1500; ++j)
    {
        /* Increasing distance */
        d = (j + 149) / 150;

        /* Pick a location (skip LOS test) */
        scatter(p_ptr->depth, &y, &x, starty, startx, d, TRUE);

        /* Must have an "empty" grid */
        if (!cave_empty_bold(p_ptr->depth, y, x)) continue;

        /* Not allowed to go onto a icky location (house) */
        if ((p_ptr->depth <= 0) && is_icky(p_ptr->depth, y, x)) continue;

        /* Place the player */
        p_ptr->old_py = p_ptr->py = y;
        p_ptr->old_px = p_ptr->px = x;

        return;
    }

    /* Try to find an occupied space */
    for (j = 0; j < 1500; ++j)
    {
        /* Increasing distance */
        d = (j + 149) / 150;

        /* Pick a location (skip LOS test) */
        scatter(p_ptr->depth, &y, &x, starty, startx, d, TRUE);

        /* Must have a "floor" grid (forbid players only) */
        if (!cave_floor_bold(p_ptr->depth, y, x) || (cave_get(p_ptr->depth)->m_idx[y][x] < 0))
            continue;

        /* Not allowed to go onto a icky location (house) */
        if ((p_ptr->depth <= 0) && is_icky(p_ptr->depth, y, x)) continue;

        /* Remove any monster at that location */
        delete_monster(p_ptr->depth, y, x);

        /* Place the player */
        p_ptr->old_py = p_ptr->py = y;
        p_ptr->old_px = p_ptr->px = x;

        return;
    }
}


static void remove_hounds(struct player *p)
{
    int j, d;
    struct cave *c = cave_get(p->depth);

    /* Remove nearby hounds */
    for (j = 1; j < cave_monster_max(c); j++)
    {
        monster_type *m_ptr = cave_monster(c, j);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip Unique Monsters */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Skip monsters other than hounds */
        if (r_ptr->base != lookup_monster_base("zephyr hound")) continue;

        /* Skip distant monsters */
        d = distance(p->py, p->px, m_ptr->fy, m_ptr->fx);
        if (d > MAX_SIGHT) continue;

        /* Delete the monster */
        delete_monster_idx(c, j);
    }
}


static void generate_new_level(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int startx, starty;
    bool new_level = FALSE;

    /* Paranoia */
    if (!players_on_depth[p_ptr->depth]) return;

    /* Play ambient sound on change of level. */
    play_ambient_sound(Ind);

    /* Check "maximum depth" to make sure it's still correct */
    if (p_ptr->depth > p_ptr->max_depth) p_ptr->max_depth = p_ptr->depth;

    /* Make sure the server doesn't think the player is in a store */
    p_ptr->store_num = -1;

    /* Somebody has entered an ungenerated level */
    if (!cave_get(p_ptr->depth))
    {
        new_level = TRUE;

        /* Allocate space for it */
        alloc_dungeon_level(p_ptr->depth);

        /* Generate a dungeon level there */
        p_ptr->feeling = cave_generate(cave_get(p_ptr->depth), p_ptr);
    }

    /* Apply illumination */
    else
    {
        /* Clear the flags for each cave grid */
        clear_cave_info(p_ptr, TRUE);

        cave_illuminate(p_ptr, cave_get(p_ptr->depth), (is_daytime()? TRUE: FALSE));
    }

    /* Give a level feeling to this player */
    if (random_level(p_ptr->depth)) display_feeling(p_ptr, FALSE);

    /* Player gets to go first */
    if (p_ptr->new_level_method != LEVEL_GHOST)
        p_ptr->energy = level_speed(p_ptr->depth);

    /* Hack -- Enforce illegal panel */
    p_ptr->offset_y = DUNGEON_HGT;
    p_ptr->offset_x = DUNGEON_WID;

    /* Determine starting location */
    switch (p_ptr->new_level_method)
    {
        /* Climbed down */
        case LEVEL_DOWN:
        {
            starty = level_down_y[p_ptr->depth];
            startx = level_down_x[p_ptr->depth];

            /* Never get pushed from stairs when entering a new level */
            if (new_level) delete_monster(p_ptr->depth, starty, startx);
            break;
        }

        /* Climbed up */
        case LEVEL_UP:
        {
            starty = level_up_y[p_ptr->depth];
            startx = level_up_x[p_ptr->depth];

            /* Never get pushed from stairs when entering a new level */
            if (new_level) delete_monster(p_ptr->depth, starty, startx);
            break;
        }

        /* Teleported level */
        case LEVEL_RAND:
            starty = level_rand_y[p_ptr->depth];
            startx = level_rand_x[p_ptr->depth];
            break;

        /* Used ghostly travel */
        case LEVEL_GHOST:
            starty = p_ptr->py;
            startx = p_ptr->px;
            break;

        /* Over the river and through the woods */
        case LEVEL_OUTSIDE:
            starty = p_ptr->py;
            startx = p_ptr->px;
            break;

        /*
         * This is used instead of extending the level_rand_y/x
         * into the negative direction to prevent us from
         * allocating so many starting locations.  Although this does
         * not make players teleport to simmilar locations, this
         * could be achieved by seeding the RNG with the depth.
         */
        case LEVEL_OUTSIDE_RAND:
        {
            /* Make sure we aren't in an "icky" location */
            do
            {
                starty = randint0(DUNGEON_HGT - 3) + 1;
                startx = randint0(DUNGEON_WID - 3) + 1;
            }
            while (is_icky(p_ptr->depth, starty, startx) ||
                !cave_floor_bold(p_ptr->depth, starty, startx));
            break;
        }
    }

    /* Place the player */
    place_player(Ind, starty, startx);

    /* Add the player */
    cave_get(p_ptr->depth)->m_idx[p_ptr->py][p_ptr->px] = 0 - Ind;

    /* Redraw */
    cave_light_spot(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px);

    /* Prevent hound insta-death */
    if (new_level) remove_hounds(p_ptr);

    /* Choose panel */
    verify_panel(p_ptr);

    /* Redraw */
    p_ptr->redraw |= (PR_MAP | PR_DEPTH | PR_FLOOR | PR_MONSTER | PR_MONLIST | PR_ITEMLIST);

    /* Fully update the visuals (and monster distances) */
    forget_view(p_ptr);
    update_view(p_ptr);
    update_monsters(p_ptr->depth, TRUE);
    update_players();

    /* Fully update the flow */
    cave_forget_flow(p_ptr);
    cave_update_flow(p_ptr, cave_get(p_ptr->depth));

    /* Clear the flag */
    p_ptr->new_level_flag = FALSE;

    /* Cancel the target */
    target_set_monster(Ind, 0);

    /* Cancel tracking */
    cursor_track(Ind, 0);

    /* Cancel the health bar */
    health_track(p_ptr, 0);
}


static void process_death()
{
    int i;

    /* Check for death */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Check for death (postpone death if no level) */
        if (p_ptr->is_dead && !p_ptr->new_level_flag && !p_ptr->leaving)
        {
            /* Kill him */
            player_death(i);
        }
    }
}


static void energize_monsters(struct cave *c)
{
    int i;

    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        int mspeed;
        monster_type *m_ptr = cave_monster(c, i);
        int energy;

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip "unconscious" monsters */
        if (m_ptr->hp == 0) continue;

        /* Calculate the net speed */
        mspeed = m_ptr->mspeed;
        if (m_ptr->m_timed[MON_TMD_FAST])
            mspeed += 10;
        if (m_ptr->m_timed[MON_TMD_SLOW])
            mspeed -= 10;

        /* Obtain the energy boost */
        energy = extract_energy[mspeed];

        /* If we are within a players time bubble, scale our energy */
        if (m_ptr->closest_player)
            energy *= ((double)time_factor(m_ptr->closest_player) / 100);

        /* Give this monster some energy */
        m_ptr->energy += energy;

        /* Make sure we don't store up too much energy */
        if (m_ptr->energy > level_speed(m_ptr->depth))
            m_ptr->energy = level_speed(m_ptr->depth);
    }
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 *
 * This is called every 1/FPS seconds.  Usually FPS is about
 * 50, so that a normal unhasted unburdened character gets 1 player turn per
 * second.  Note that we process every player and the monsters, then quit.
 * The "scheduling" code (see sched.c) is the REAL main loop, which handles
 * various inputs and timings.
 */
void dungeon(void)
{
    int i;

    /* Hack -- Reset current sound */
    for (i = 1; i < NumPlayers + 1; i++) player_get(i)->current_sound = -1;

    /*
     * The number of game turns per player turn can be calculated as the energy
     * required to act at the current depth (energy per player turn) divided by
     * the energy given per game turn given the current player speed.
     */

    /* Hack -- Reset "afraid" status every player turn */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);
        int player_turn = level_speed(p_ptr->depth) / extract_energy[p_ptr->state.speed];

        /* Reset "afraid" status */
        if (!(turn.turn % player_turn)) p_ptr->is_afraid = FALSE;
    }

    /* Hack -- Reset projection indicator every player turn */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /*
         * For projection indicator, we will consider the shortest number of game turns
         * possible -- the one obtained at max speed.
         */
        int player_turn = level_speed(p_ptr->depth) / extract_energy[199];

        /* Reset projection indicator */
        if (!(turn.turn % player_turn)) p_ptr->did_visuals = FALSE;
    }

    /* Hack -- Compact the monster list occasionally */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        struct cave *c = cave_get(i);

        if (c && (cave_monster_count(c) + 32 > z_info->m_max))
            compact_monsters(c, 64);
    }

    /* Hack -- Compress the monster list occasionally */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        struct cave *c = cave_get(i);

        if (c && (cave_monster_count(c) + 32 < cave_monster_max(c)))
            compact_monsters(c, 0);
    }

    /* Hack -- Compact the object list occasionally */
    if (o_top + 32 > z_info->o_max) compact_objects(64);

    /* Hack -- Compress the object list occasionally */
    if (o_top + 32 < o_max) compact_objects(0);

    /* Handle any network stuff */
    Net_input();

    /* Process the players */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Process that player */
        if (!p_ptr->new_level_flag && !p_ptr->leaving) process_player(i);
    }

    /* Check for death */
    process_death();

    /* Process the monsters */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i)) process_monsters(cave_get(i));
    }

    /* Check for death */
    process_death();

    /* Process all of the objects */
    process_objects();

    /* Process the world */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Process the world of that player */
        if (!p_ptr->new_level_flag && !p_ptr->leaving) process_world(i);
    }

    /* Process everything else */
    process_various();

    /* Hack -- Regenerate the monsters every hundred game turns */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i) && !(turn.turn % 100)) regen_monsters(cave_get(i));
    }

    /*** Apply energy ***/

    /* Give energy to all players */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);
        int energy;

        if (p_ptr->leaving) continue;

        /* Player is idle */
        p_ptr->is_idle = has_energy(i);

        /* How much energy should we get? */
        energy = extract_energy[p_ptr->state.speed];

        /* Scale depending upon our time bubble */
        p_ptr->bubble_speed = time_factor(i);
        energy = energy * ((double)p_ptr->bubble_speed / 100);

        /* In town, give everyone a RoS when they are running */
        if (!p_ptr->depth && p_ptr->running)
            energy = energy * ((double)RUNNING_FACTOR / 100);

        /* Give the player some energy */
        p_ptr->energy += energy;

        /* Make sure they don't have too much */
        if (p_ptr->energy > level_speed(p_ptr->depth))
            p_ptr->energy = level_speed(p_ptr->depth);

        /* Check "resting" status */
        if (p_ptr->resting)
        {
            /* No energy available while resting */
            /* This prevents us from instantly waking up */
            p_ptr->energy = 0;
        }

        /* Handle paralysis here */
        if (p_ptr->timed[TMD_PARALYZED] || p_ptr->timed[TMD_STUN] >= 100)
            p_ptr->energy = 0;
    }

    /* Give energy to all monsters */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i)) energize_monsters(cave_get(i));
    }

    /* Count game turns */
    ht_add(&turn, 1);
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        if (p_ptr->leaving) continue;

        /* Increment the game turn counter */
        ht_add(&p_ptr->game_turn, 1);

        /* Increment the player turn counter */
        if (has_energy(i) || p_ptr->resting) ht_add(&p_ptr->player_turn, 1);

        /* Increment the active player turn counter */
        if (has_energy(i) && !p_ptr->is_idle) ht_add(&p_ptr->active_turn, 1);

        /* Inform the client every second */
        if (!(turn.turn % cfg_fps))
        {
            Send_turn(i, ht_div(&p_ptr->game_turn, cfg_fps), ht_div(&p_ptr->player_turn, cfg_fps),
                ht_div(&p_ptr->active_turn, cfg_fps));
        }
    }

    /* Refresh everybody's displays */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Full refresh (includes monster/object lists) */
        p_ptr->full_refresh = TRUE;

        /* Refresh */
        refresh_stuff(i);

        /* Normal refresh (without monster/object lists) */
        p_ptr->full_refresh = FALSE;
    }

    /* Send any information over the network */
    Net_output();

    /* Get rid of dead players */
    for (i = NumPlayers; i > 0; i--)
    {
        player_type *p_ptr = player_get(i);
        char buf[MSG_LEN];

        if (!p_ptr->leaving) continue;

        /* Format string */
        if (!p_ptr->alive)
        {
            if (!strcmp(p_ptr->died_from, "divine wrath"))
                my_strcpy(buf, "Killed by divine wrath", sizeof(buf));
            else if (!p_ptr->total_winner)
                my_strcpy(buf, "Committed suicide", sizeof(buf));
            else
                my_strcpy(buf, "Retired", sizeof(buf));
        }
        else if (p_ptr->ghost)
            strnfmt(buf, sizeof(buf), "Destroyed by %s", p_ptr->died_from);
        else
            strnfmt(buf, sizeof(buf), "Killed by %s", p_ptr->died_from);

        /* Get rid of him */
        Destroy_connection(p_ptr->conn, buf);
    }

    /* Kick out starving players */
    for (i = NumPlayers; i > 0; i--)
    {
        player_type *p_ptr = player_get(i);

        if (!p_ptr->starving) continue;

        /* Kick him */
        Destroy_connection(p_ptr->conn, "Starving to death!");
    }

    /* Deallocate any unused levels */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        /* Everybody has left a level that is still generated */
        if (cave_get(i) && !players_on_depth[i])
        {
            /* Destroy the level (except town and special levels) */
            if (!forbid_special(i)) dealloc_dungeon_level(i);
        }
    }

    /* Make a new level */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        if (player_get(i)->new_level_flag) generate_new_level(i);
    }
}


static void init_global()
{
    int i, j;

    /*** Prepare global arrays for local features ***/

    f_char_s = C_ZNEW(z_info->f_max, char_lit);
    f_attr_s = C_ZNEW(z_info->f_max, byte_lit);
    for (i = 0; i < z_info->f_max; i++)
    {
        for (j = 0; j < FEAT_LIGHTING_MAX; j++)
        {
            f_char_s[i][j] = f_info[i].x_char[j];
            f_attr_s[i][j] = f_info[i].x_attr[j];
        }
    }

    r_char_s = C_ZNEW(z_info->r_max, char);
    r_attr_s = C_ZNEW(z_info->r_max, byte);
    for (i = 0; i < z_info->r_max; i++)
    {
        r_char_s[i] = r_info[i].x_char;
        r_attr_s[i] = r_info[i].x_attr;
    }
}


/*
 * Actually play a game
 */
void play_game(void)
{
    bool new_game = FALSE;

    server_generated = FALSE;

    /* Flash a message */
    plog("Please wait...");

    /* Attempt to load the server state information */
    if (!load_server_info())
    {
        /* Oops */
        quit("Broken server savefile");
    }

    /* Nothing loaded */
    if (!server_state_loaded)
    {
        /* Make server state info */
        new_game = TRUE;

        /* Create a new dungeon */
        ht_reset(&cave_get(0)->generated);
    }

    /* Init the RNG */
    if (Rand_quick)
    {
        u32b seed;

        /* Basic seed */
        seed = (time(NULL));

        /* Use the complex RNG */
        Rand_quick = FALSE;

        /* Seed the "complex" RNG */
        Rand_state_init(seed);
    }

    /* Roll new town */
    if (new_game)
    {
        /* Ignore the dungeon */
        ht_reset(&cave_get(0)->generated);

        /* Seed for flavors */
        seed_flavor = randint0(0x10000000);

        /* Seed for town layout */
        seed_town = randint0(0x10000000);

        /* Initialize server state information */
        server_birth();

        /* Hack -- enter the world */
        ht_reset(&turn);
        ht_add(&turn, 1);

        /* Initialize the stores */
        store_reset();
    }

    /* Flavor the objects */
    flavor_init();

    plog("Object flavors initialized...");

    /* Reset the visual mappings */
    reset_visuals();

    /* Load requested "pref" file */
    if (cfg_load_pref_file)
    {
        plog_fmt("Loading pref file: %s", cfg_load_pref_file);
        process_pref_file(cfg_load_pref_file, FALSE);
    }

    /* Local features are now fully defined, let's prepare global arrays */
    init_global();

    /* Make a town if necessary */
    if (ht_zero(&cave_get(0)->generated))
    {
        /* Actually generate the town */
        cave_generate(cave_get(0), NULL);
    }

    /* Place all dungeon objects */
    place_objects(0);

    /* Server initialization is now "complete" */
    server_generated = TRUE;

    /* Set up the contact socket, so we can allow players to connect */
    setup_contact_socket();

    /* Set up the network server */
    if (Setup_net_server() == -1)
        quit("Couldn't set up net server");

    /* Set up the main loop */
    install_timer_tick(dungeon, cfg_fps);

    /* Loop forever */
    sched();

    /* This should never, ever happen */
    plog("FATAL ERROR sched() returned!");

    /* Close stuff */
    close_game();

    /* Quit */
    quit(NULL);
}


void shutdown_server(void)
{
    int i;

    plog("Shutting down.");

    /* Stop the main loop */
    remove_timer_tick();

    /* Kick every player out and save his game */
    while (NumPlayers > 0)
    {
        /* Note that we always save the first player */
        player_type *p_ptr = player_get(1);

        /* Indicate cause */
        my_strcpy(p_ptr->died_from, "server shutdown", sizeof(p_ptr->died_from));

        /* Try to save */
        if (!save_player(1)) Destroy_connection(p_ptr->conn, "Server shutdown (save failed)");

        /* Successful save */
        Destroy_connection(p_ptr->conn, "Server shutdown (save succeeded)");
    }

    /* Preserve artifacts on the ground */
    for (i = 1; i < MAX_DEPTH; i++) preserve_artifacts(i);

    /* Save the server state */
    if (!save_server_info()) quit("Server state save failed!");

    /* Tell the metaserver that we're gone */
    Report_to_meta(META_DIE);

    quit("Server state saved");
}


/*
 * Check if the given depth is special static level, i.e. a hand designed level.
 */
bool check_special_level(s16b special_depth)
{
    int i;

    for (i = 0; i < MAX_SPECIAL_LEVELS; i++)
    {
        if (special_depth == special_levels[i]) return (TRUE);
    }

    return (FALSE);
}


/*
 * Forbid in the town or on special levels.
 */
bool forbid_special(s16b special_depth)
{
    return (!special_depth || check_special_level(special_depth));
}


/*
 * Returns whether "depth" corresponds to a randomly generated level.
 */
bool random_level(s16b depth)
{
    return ((depth > 0) && !check_special_level(depth));
}


/*
 * Recall a player.
 */
bool recall_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int new_depth = p_ptr->recall_depth, new_world_x = p_ptr->world_x, new_world_y = p_ptr->world_y;
    byte new_level_method;
    const char *msg_self, *msg_others;

    /* From dungeon to town */
    if (p_ptr->depth > 0)
    {
        /* Messages */
        msg_self = "You feel yourself yanked upwards!";
        msg_others = " is yanked upwards!";

        /* New location */
        new_depth = 0;
        new_level_method = LEVEL_RAND;
    }

    /* From wilderness to town */
    else if (p_ptr->depth < 0)
    {
        /* Messages */
        msg_self = "You feel yourself yanked sideways!";
        msg_others = " is yanked sideways!";

        /* New location */
        new_depth = 0;
        new_world_x = 0;
        new_world_y = 0;
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* From town to wilderness */
    else if (p_ptr->recall_depth < 0)
    {
        /* Messages */
        msg_self = "You feel yourself yanked sideways!";
        msg_others = " is yanked sideways!";

        /* New location */
        new_world_x = wild_info[new_depth].world_x;
        new_world_y = wild_info[new_depth].world_y;
        new_level_method = LEVEL_OUTSIDE_RAND;
    }

    /* From town to dungeon */
    else
    {
        /* Messages */
        msg_self = "You feel yourself yanked downwards!";
        msg_others = " is yanked downwards!";

        /* New location */
        new_level_method = LEVEL_RAND;
    }

    /* Hack -- DM redesigning the level */
    if (players_on_depth[new_depth] == INHIBIT_DEPTH) return FALSE;

    /* Disturbing! */
    disturb(p_ptr, 0, 0);

    /* Messages */
    msgt(p_ptr, MSG_TPLEVEL, msg_self);
    msg_misc(p_ptr, msg_others);

    /* Change location */
    dungeon_change_level(p_ptr, new_depth, new_level_method);

    /* Hack -- Replace the player */
    p_ptr->world_x = new_world_x;
    p_ptr->world_y = new_world_y;
    p_ptr->arena_num = -1;

    /* Redraw the state (later) */
    p_ptr->redraw |= (PR_STATE);

    return TRUE;
}
