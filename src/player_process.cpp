
/* File: player_process.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <src/cmds.h>
#include <QCoreApplication>

/*
 * Process the player terrain damage
 * This function can kill the player, so all calls to this function should be able to handle this.
 */
void process_player_terrain_damage(void)
{
    /* No damage to take terrain */
    if (p_ptr->cumulative_terrain_damage > 0)
    {
        /*
         * IMPORTANT: we divide cumulative damage by 10
         * to get a value nearly equal to "dam_non_native" at
         * normal speed (1 player turn every 10 game turns)
         */
        int dam = p_ptr->cumulative_terrain_damage / 10;

        QString name,  kb_str;

        /* Get the feature */
        int feat = dungeon_info[p_ptr->py][p_ptr->px].feature_idx;

        /* Uncomment this if you want a damage cap */
        /*dam = MIN(dam, f_info[feat].dam_non_native);*/

        /* Get the feature name */
        name = feature_desc(feat, TRUE, TRUE);

        /* Format the killer string */
        kb_str = "standing in ";
        kb_str.append(name);

        /* Take the hit */
        take_terrain_hit(dam, feat, kb_str);

        /* Reset terrain damage */
        p_ptr->cumulative_terrain_damage = 0;

    }
}

/*
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
    int i;
    bool changed = FALSE;

    static int old_monster_race_idx = 0;

    static u32b	old_flags1 = 0L;
    static u32b	old_flags2 = 0L;
    static u32b	old_flags3 = 0L;
    static u32b	old_flags4 = 0L;
    static u32b	old_flags5 = 0L;
    static u32b	old_flags6 = 0L;
    static u32b	old_flags7 = 0L;

    static byte old_blows[MONSTER_BLOW_MAX];

    static byte	old_ranged = 0;


    /* Tracking a monster */
    if (p_ptr->monster_race_idx)
    {
        /* Get the monster lore */
        monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

        for (i = 0; i < MONSTER_BLOW_MAX; i++)
        {
            if (old_blows[i] != l_ptr->blows[i])
            {
                changed = TRUE;
                break;
            }
        }

        /* Check for change of any kind */
        if (changed ||
            (old_monster_race_idx != p_ptr->monster_race_idx) ||
            (old_flags1 != l_ptr->r_l_flags1) ||
            (old_flags2 != l_ptr->r_l_flags2) ||
            (old_flags3 != l_ptr->r_l_flags3) ||
            (old_flags4 != l_ptr->r_l_flags4) ||
            (old_flags5 != l_ptr->r_l_flags5) ||
            (old_flags6 != l_ptr->r_l_flags6) ||
            (old_flags7 != l_ptr->r_l_flags7) ||
            (old_ranged != l_ptr->ranged))

        {
            /* Memorize old race */
            old_monster_race_idx = p_ptr->monster_race_idx;

            /* Memorize flags */
            old_flags1 = l_ptr->r_l_flags1;
            old_flags2 = l_ptr->r_l_flags2;
            old_flags3 = l_ptr->r_l_flags3;
            old_flags4 = l_ptr->r_l_flags4;
            old_flags5 = l_ptr->r_l_flags5;
            old_flags6 = l_ptr->r_l_flags6;
            old_flags7 = l_ptr->r_l_flags7;

            /* Memorize blows */
            for (i = 0; i < MONSTER_BLOW_MAX; i++)
                old_blows[i] = l_ptr->blows[i];

            /* Memorize castings */
            old_ranged = l_ptr->ranged;

            /* Window stuff */
            p_ptr->redraw |= (PR_WIN_MON_RECALL);

            /* Window stuff */
            handle_stuff();
        }
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
 */
void process_player(void)
{
    /* One more player turn */
    p_ptr->p_turn++;

    /* Take damage from terrain */
    process_player_terrain_damage();

    /* Dead player? */
    if (p_ptr->is_dead) return;

    /* Check for "player abort" */
    if (p_ptr->is_running() ||
        p_ptr->command_current || p_ptr->is_resting())
    {
        // Allow the check_disturb method of MainWindow to be executed
        QCoreApplication::processEvents();
    }

    /* Notice stuff (if needed) */
    notice_stuff();

    /* Update stuff (if needed) */
    update_stuff();

    /* Redraw stuff (if needed) */
    redraw_stuff();

    /* Hack -- Pack Overflow */
    pack_overflow();

    /* Paralyzed or Knocked Out */
    if ((p_ptr->timed[TMD_PARALYZED]) ||
        (p_ptr->stun_status() == STUN_KNOCKED_OUT))
    {
         /* Take a turn */
         p_ptr->p_energy -= BASE_ENERGY_MOVE;
    }

    /* Normal command */
    else
    {
        /* Check monster recall */
        process_player_aux();

        py_pickup_gold();
    }

    p_ptr->redraw |= (PR_TURNCOUNT);

    /*** Clean up ***/

    /* hack - check for secret squares */
    if (dungeon_info[p_ptr->py][p_ptr->px].cave_info & (CAVE_SPECIAL))
    {
        /* increase chance of altered inventory for around 100 turns*/
        altered_inventory_counter += 1;

        /*unmark the square*/
        dungeon_info[p_ptr->py][p_ptr->px].cave_info &= ~(CAVE_SPECIAL);
    }

    /* Check for greater vault squares */
    if ((dungeon_info[p_ptr->py][p_ptr->px].cave_info & (CAVE_G_VAULT)) && !g_vault_name.isEmpty())
    {
        message(QString("You have entered the %1") .arg(g_vault_name));

        QString note = (QString("Entered the %1.") .arg(g_vault_name));

        write_note(note, p_ptr->depth);

        g_vault_name.clear();
    }

    /* Some quests aren't finished by killing monsters */
    if (guild_quest_active())
    {
        quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

        /* See if the greater vault quest just finished, if so, complete it */
        if (q_ptr->q_type == QUEST_GREATER_VAULT)
        {
            process_greater_vault_quest();
        }
    }

    /* Get base noise increase -- less when resting */
    if (p_ptr->is_resting()) total_wakeup_chance += p_ptr->base_wakeup_chance / 2;
    else total_wakeup_chance += p_ptr->base_wakeup_chance;

    /* Increase noise if necessary */
    if (add_wakeup_chance)
    {
        total_wakeup_chance += add_wakeup_chance;
        add_wakeup_chance = 0;
    }

    /* Limit on noise (100% effective in disturbing monsters) */
    if (total_wakeup_chance > 10000)
    {
        total_wakeup_chance = 10000;
    }

    /* Update noise flow information */
    if ((p_ptr->py != p_ptr->update_center_y) || (p_ptr->px != p_ptr->update_center_x))
    {
        update_flows(FALSE);
    }

    /*
     * Reset character vulnerability if appropriate.  Will be calculated
     * by the first member of an animal pack that has a use for it.
     */
    if (p_ptr->vulnerability == 100)
    {
        /* Reset vulnerability only if the character is fully healed */
        if (p_ptr->chp >= p_ptr->mhp) p_ptr->vulnerability = 0;
    }
    else
    {
        p_ptr->vulnerability = 0;
    }


    if (guild_quest_active())
    {
        if (quest_slot_timed(GUILD_QUEST_SLOT))	p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    // Finally, update certain onscreen information before it becomes the player turn
    p_ptr->player_turn = TRUE;

    /* Notice stuff (if needed) */
    notice_stuff();

    /* Update stuff (if needed) */
    update_stuff();

    /* Redraw stuff (if needed) */
    redraw_stuff();
}
