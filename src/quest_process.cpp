
/* File: quest_process.cpp */

/*
 * Copyright (c) 1997 Eytan Zweig, Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"

/*
 * Display the current quest (if any)
 */
void do_cmd_quest(void)
{
    /* Check if you're on a quest */
    if (guild_quest_level() > 0)
    {
        /* Completed quest */
        if (guild_quest_complete())
        {
            message(QString("Collect your reward at the guild!"));
        }
        else
        {
            QString q_out = describe_quest(guild_quest_level());

            message(q_out);
        }
    }
    /* No quest at all */
    else message(QString("You are not currently undertaking a quest."));
}

/*
 * Helper function for process_guild_quests
 * Count the number of quest monsters hiding as mimic objects
 * Simply return zero if inapplicable
 */
static int count_quest_monsters(const quest_type *q_ptr)
{
    int cur_quest_monsters = q_ptr->q_num_killed;

    int j;

    /* Count the quest monsters */
    for (j = 1; j < mon_max; j++)
    {
        monster_type *m_ptr = &mon_list[j];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Count Quest monsters */
        if (m_ptr->mflag & (MFLAG_QUEST)) cur_quest_monsters++;
    }

    /* Count and return the mimics */
    for (j = 1; j < o_max; j++)
    {
        object_type *o_ptr = &o_list[j];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Only work with the mimic objects */
        if (!o_ptr->is_mimic()) continue;

        /* Mimic is waiting to turn into a quest monster */
        if (o_ptr->is_quest_object() && o_ptr->is_mimic()) cur_quest_monsters++;
    }

    return (cur_quest_monsters);
}


/*
 * Helper function for process_arena_level and add arena object.
 * Determine which object to give to the player.
 */
static s16b get_arena_obj_num(void)
{
    s16b k_idx;
    s16b level = randint0(p_ptr->depth) + p_ptr->depth / 5;
    byte tval_type = randint1(100);

    /* 40% chance of a potion */
    if (tval_type < 35)
    {
        if (level <= 20)		k_idx = lookup_kind(TV_POTION, SV_POTION_CURE_CRITICAL);
        else if (level <= 40)	k_idx = lookup_kind(TV_POTION, SV_POTION_SPEED);
        else if (level <= 70)	k_idx = lookup_kind(TV_POTION, SV_POTION_HEALING);
        else if (level <= 85)	k_idx = lookup_kind(TV_POTION, SV_POTION_STAR_HEALING);
        else 					k_idx = lookup_kind(TV_POTION, SV_POTION_LIFE);
    }
    /* 20% chance of a scroll */
    else if (tval_type <= 50)
    {
        if (level <= 10)		k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_PHASE_DOOR);
        else if (level <= 20)	k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_HOLY_PRAYER);
        else if (level <= 40)	k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_RECHARGING);
        else                    k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL);
    }
    /* 25% chance of a staff/wand */
    else if (tval_type <= 80)
    {
        if (level <= 20)		k_idx = lookup_kind(TV_WAND, SV_WAND_TELEPORT_AWAY);
        else if (level <= 40)	k_idx = lookup_kind(TV_WAND, SV_WAND_ANNIHILATION);
        else if (level <= 60)	k_idx = lookup_kind(TV_STAFF, SV_STAFF_HEALING);
        else if (level <= 85)	k_idx = lookup_kind(TV_STAFF, SV_STAFF_HOLINESS);
        else 					k_idx = lookup_kind(TV_STAFF, SV_STAFF_SPEED);
    }
    /* 20% chance of a custom item for that specific class */
    else
    {
        /* spellcasters want mana */
        if (cp_ptr->flags & (CF_ZERO_FAIL))
        {
            if (level < 65) k_idx = lookup_kind(TV_POTION, SV_POTION_RESTORE_MANA);
            else            k_idx = lookup_kind(TV_POTION, SV_STAFF_THE_MAGI);

        }

        /*
         * Most others get ammo, but first check to confirm
         * they are wielding something in the ammo slot.
         * If not, give them a potion of healing.
         */
        else if (!p_ptr->state.ammo_tval)
        {
            k_idx = lookup_kind(TV_POTION, SV_POTION_HEALING);
        }
        /* Others get ammo */
        else
        {
            byte sval;
            byte tval = p_ptr->state.ammo_tval;
            if (level <= 30)		sval = SV_AMMO_LIGHT;
            else if (level <= 75)	sval = SV_AMMO_NORMAL;
            else 					sval = SV_AMMO_HEAVY;

            /* Hack - the svals for shots are slightly different than bows and bolts */
            if ((tval == TV_ARROW || tval == TV_BOLT)) sval++;

            k_idx = lookup_kind(tval, sval);
        }
    }

    return (k_idx);
}

/*
 * Determine if a monster is suitable for the arena
 */
static bool monster_arena_labyrinth_okay(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Decline unique monsters for the arena */
    if (r_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

    /* No breeders */
    if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

    /* no mimics */
    if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) return (FALSE);

    /* no stationary */
    if (r_ptr->flags1 & (RF1_NEVER_MOVE)) return (FALSE);

    // Group monsters are too weak individually
    if (r_ptr->flags1 & (RF1_FRIEND | RF1_FRIENDS | RF1_ESCORT | RF1_ESCORTS)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for process_arena_level.
 * Add monsters to the recently added parts of the dungeon.
 * If new spaces have been cleared, this function will first try to put them there.
 */
static bool add_arena_monster(bool new_squares, byte stage, s32b cur_quest_monsters)
{
    u16b empty_squares_y[ARENA_LEVEL_AREA];
    u16b empty_squares_x[ARENA_LEVEL_AREA];
    byte empty_squares = 0;
    byte y, x;
    s16b r_idx;
    byte slot;
    monster_type *m_ptr;
    s16b mon_lev = p_ptr->depth;

    /*
     * Start with add floor spaces where appropriate.
     * See where the new squares are
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Ignore the outer walls locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Look for an exact match to the stage */
            if ((arena_level_map[y][x] != stage) && (new_squares)) continue;

            /* New, and open square */
            if (cave_naked_bold(y, x))
            {
                slot = empty_squares;

                empty_squares_y[slot] = y;
                empty_squares_x[slot] = x;
                empty_squares++;
            }
        }
    }

    /* Paranoia - shouldn't happen */
    if (!empty_squares) return (FALSE);

    /* Find a spot in the array */
    slot = randint0(empty_squares);
    y = empty_squares_y[slot];
    x = empty_squares_x[slot];

    /* Prepare allocation table */
    get_mon_num_hook = monster_arena_labyrinth_okay;
    get_mon_num_prep();

    if (((cur_quest_monsters % 5) == 2) ||
        ((cur_quest_monsters % 5) == 4))
    {
        mon_lev = (mon_lev * 3) / 4;
    }

    /* Pick a monster, using the given level */
    r_idx = get_mon_num(mon_lev, y, x, 0L);

    /* One out of 5 monsters, make sure they are a little harder */
    if ((cur_quest_monsters % 5) == 3)
    {
        s16b r2_idx = get_mon_num(mon_lev, y, x, 0L);
        monster_race *r_ptr = &r_info[r_idx];
        monster_race *r2_ptr = &r_info[r2_idx];

        /* If we found a tougher monster, use it */
        if (r_ptr->mon_power < r2_ptr->mon_power)
        {
            r_idx = r2_idx;
            r_ptr = &r_info[r_idx];
        }

        /* Try twice */
        r2_idx = get_mon_num(mon_lev, y, x, 0L);
        r2_ptr = &r_info[r2_idx];
        if (r_ptr->mon_power < r2_ptr->mon_power)
        {
            r_idx = r2_idx;
        }
    }

    /* Remove restriction */
    get_mon_num_hook = NULL;
    get_mon_num_prep();

    if (!place_monster_aux(y, x, r_idx, (MPLACE_GROUP | MPLACE_OVERRIDE))) return (FALSE);

    /* Mark it as a questor */
    if (!dungeon_info[y][x].monster_idx)	return (FALSE); /* Paranoia - should never happen */
    m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
    m_ptr->mflag |= (MFLAG_QUEST);
    disturb(FALSE, TRUE);
    return (TRUE);
}


/*
 * Helper function for process_arena_level.
 * Add objects to the recently added parts of the dungeon.
 * This function assumes new squares have just been cleared.
 */
static void add_arena_object(byte stage)
{
    u16b empty_squares_y[ARENA_LEVEL_AREA];
    u16b empty_squares_x[ARENA_LEVEL_AREA];
    byte empty_squares = 0;
    byte slot;
    byte y, x;
    s16b k_idx;
    object_type *i_ptr;
    object_type object_type_body;

    /*
     * Start with add floor spaces where appropriate.
     * See where the new squares are
     */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Ignore the outer walls locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Look for an exact match to the stage */
            if (arena_level_map[y][x] != stage) continue;

            /* New, and open square */
            if (cave_naked_bold(y, x))
            {
                empty_squares_y[empty_squares] = y;
                empty_squares_x[empty_squares] = x;
                empty_squares++;
            }
        }
    }

    /* Paranoia - shouldn't happen */
    if (!empty_squares) return;

    /* Get local object */
    i_ptr = &object_type_body;
    i_ptr->object_wipe();

    /* Pick a square at random */
    slot = randint0(empty_squares);
    y = empty_squares_y[slot];
    x = empty_squares_x[slot];

    k_idx = get_arena_obj_num();

    /* Prepare the object */
    object_prep(i_ptr, k_idx);

    apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

    object_quantities(i_ptr);

    /* But don't give out too much the ammo */
    if (i_ptr->is_ammo()) i_ptr->number /= 2;

    /* Identify it */
    i_ptr->mark_known(TRUE);

    /* Put it on the floor */
    floor_carry(y, x, i_ptr);
}


/*
 * This function assumes it is called every 10 game turns during an arena level.
 */
void process_arena_quest(void)
{
    int i;
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    s32b turns_lapsed = p_ptr->game_turn - q_info->turn_counter;
    bool new_squares = FALSE;

    /* Each monster phase is 5 game turns at normal speed */
    s32b current_mon_phase = turns_lapsed / ARENA_STAGE_MON + 1;

    /* Each quest phase is 20 game turns at normal speed */
    s32b current_lev_phase = turns_lapsed / ARENA_STAGE_LEV;
    s32b prev_lev_phase = (turns_lapsed - 10) / ARENA_STAGE_LEV;

    int cur_quest_monsters = count_quest_monsters(q_ptr);

    /* Boundary Control */
    if (current_mon_phase > q_ptr->q_max_num) current_mon_phase = q_ptr->q_max_num;
    if (prev_lev_phase < 0) prev_lev_phase = 0;

    /* First check if there is anything to do */
    if ((cur_quest_monsters >= current_mon_phase) &&
        ((current_lev_phase == prev_lev_phase) || (current_lev_phase >= ARENA_MAX_STAGES)))
    {
        return;
    }

    /* Open up more of the dungeon  */
    if (current_lev_phase > prev_lev_phase)
    {
        /* There are only 10 level phases */
        if (current_lev_phase < ARENA_MAX_STAGES)
        {
            byte new_objects = MIN((randint1(2)), current_lev_phase);

            /* Open up more of the arena */
            update_arena_level(current_lev_phase);

            /* We are expanding the level */
            new_squares = TRUE;

            /* Add new more objects */
            for (i = 0; i < new_objects; i++)
            {
                add_arena_object(current_lev_phase);
            }
        }
    }

    while (current_mon_phase > cur_quest_monsters)
    {
        /* Add monsters, unless there is no room or we have enough. */
        if (!add_arena_monster(new_squares, current_lev_phase, cur_quest_monsters))
        {
            /* No room for new monsters? */
            if (!new_squares)break;

            /* All the new squares are full, try other blank squares */
            new_squares  = FALSE;
        }
        else cur_quest_monsters++;
    }
}


/*
 * Helper function for process_labrynth_level.
 * Count objects on the level, including the dungeon and player backpack.
 * If
 */
static int count_labrynth_objects(void)
{
    int i;

    /* First, count in the backpack */
    int item_count = quest_item_count();

    /* Count the object list (includes those held by monsters */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Count the quest objects */
        if (o_ptr->is_quest_object())
        {
            item_count += o_ptr->number;
        }
    }

    return (item_count);
}


/*
 * Helper function for process_labyrnth_level.
 * Add a monster to the dungeon.
 * If requested, make them hold the quest parchment.
 * Also if requested, add a helpful item to the dungeon floor
 */
static bool add_labyrinth_monster_object(bool add_object, bool add_parchment)
{
    QVector<coord> empty_squares;
    empty_squares.clear();
    u16b slot;
    byte y, x;
    s16b r_idx;
    monster_type *m_ptr;
    s16b mon_lev = p_ptr->depth + 1;
    object_type *i_ptr;
    object_type object_type_body;
    int k_idx;

    i_ptr = &object_type_body;

    /*
     * Start with add floor spaces where appropriate. */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Ignore the outer walls locations */
            if (!in_bounds_fully(y, x)) continue;

            /*
             * We want them at least 10 squares away, based on the labyrinth.
             * Assumes energy to pass is 100.
             */
            if ((cave_cost[FLOW_PASS_DOORS][y][x] - cost_at_center[FLOW_PASS_DOORS]) < 1000) continue;

            /* New, and open square */
            if (cave_naked_bold(y, x))
            {
                empty_squares.append(make_coords(y, x));
            }
        }
    }

    /* Paranoia - shouldn't happen */
    if (empty_squares.size() < 2) return (FALSE);

    /* Find a spot in the array */
    slot = randint0(empty_squares.size());
    y = empty_squares[slot].y;
    x = empty_squares[slot].x;

    /* Prepare allocation table */
    get_mon_num_hook = monster_arena_labyrinth_okay;
    get_mon_num_prep();

    /* Pick a monster, using the given level */
    r_idx = get_mon_num(mon_lev, y, x, 0L);

    /* Remove restriction */
    get_mon_num_hook = NULL;
    get_mon_num_prep();

    if (!place_monster_aux(y, x, r_idx, (MPLACE_GROUP | MPLACE_OVERRIDE))) return (FALSE);

    /* Mark it as a questor */
    if (!dungeon_info[y][x].monster_idx)	return (FALSE); /* Paranoia - should never happen */
    m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
    m_ptr->mflag |= (MFLAG_QUEST);

    /* Now hand the monster an object if requested */
    if (add_parchment)
    {
        /* Make a piece of parchment */
        k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
        i_ptr->object_wipe();
        object_prep(i_ptr, k_idx);
        apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

        /*Don't let the player see what the object it, and make it a quest item*/
        i_ptr->ident |= (IDENT_HIDE_CARRY | IDENT_QUEST);

        /* Identify it */
        i_ptr->mark_known(TRUE);

        (void)monster_carry(dungeon_info[y][x].monster_idx, i_ptr);
    }

    if (add_object)
    {
        /* Now select another square */
        empty_squares.removeAt(slot);

        slot = randint0(empty_squares.size());
        y = empty_squares[slot].y;
        x = empty_squares[slot].x;

        k_idx = get_arena_obj_num();

        /* Prepare the object */
        i_ptr->object_wipe();
        object_prep(i_ptr, k_idx);

        apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);

        object_quantities(i_ptr);

        /* But don't give out too much the ammo */
        if (i_ptr->is_ammo()) i_ptr->number /= 2;

        /* Identify it */
        i_ptr->mark_known(TRUE);

        /* Put it on the floor */
        floor_carry(y, x, i_ptr);
    }

    return (TRUE);
}


/*
 * This function assumes it is called every 10 game turns during a labyrinth level.
 */
void process_labyrinth_quest(void)
{
    int i;
    s32b turns_lapsed = p_ptr->game_turn - q_info->turn_counter;

    /* Each wave is 20 game turns at normal speed */
    s32b current_lab_wave = turns_lapsed / LABYRINTH_STAGE_LEN;
    s32b prev_lab_wave = (turns_lapsed - 10) / LABYRINTH_STAGE_LEN;

    /* First check if the quest is complete */
    int num_objects = count_labrynth_objects();

    /* There are enough objects on the level */
    if (num_objects >= LABYRINTH_COLLECT) return;

    /* Only add monsters/objects every 200 turns */
    if (current_lab_wave == prev_lab_wave) return;

    /*
     * Every 200 turns, add another 4 monsters to the labyrinth
     * One of them will hold the object.
     * Also, put one helpful object on the floor.
      */
    if (current_lab_wave > prev_lab_wave)
    {
        int x = 4;
        bool obj_added = FALSE;
        bool parchment_added = FALSE;

        /* No more than 5 objects added, one every other wave */
        if (current_lab_wave > 10) obj_added = TRUE;
        else if (current_lab_wave % 2) obj_added = TRUE;

        for (i = 0; i < 4; i++)
        {
            bool add_obj = FALSE;
            bool add_parchment = FALSE;

            if (!obj_added)
            {
                if (one_in_(x))
                {
                    obj_added = add_obj = TRUE;
                }
            }

            if (!parchment_added)
            {
                if (one_in_(x))
                {
                    add_parchment = parchment_added = TRUE;
                }
            }

            add_labyrinth_monster_object(add_obj, add_parchment);
            x--;
        }
    }
}


/*
 * Check the time remaining on the quest.
 */
void process_greater_vault_quest(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    int i;

    /* Update the turn count */
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    if (quest_time_remaining() >= 1)
    {
        /* Not much time left.  Warn the player */
        if (quest_player_turns_remaining() <= 5) do_cmd_quest();

        /*
         * Go through every monster, and return if we find a single questor,
         */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];

            /* Ignore non-existent monsters */
            if (!m_ptr->r_idx) continue;

            /*
             * If we find a single quest monster, return.
             * If not, the quest is completed.
             */
            if (m_ptr->mflag & (MFLAG_QUEST)) return;
        }
    }

    /* If the player did not enter the vault, they fail the quest. */
    if (!g_vault_name.isEmpty())
    {
        quest_fail();
        g_vault_name.clear();

        return;
    }

    /* Handle completion of the quest */
    message(QString("You have completed your quest - collect your reward at the guild!"));

    /* Turn on quest indicator */
    quest_indicator_timer = 50;
    quest_indicator_complete = TRUE;

    /* Redraw the status */
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    /* Mark the quest as finished, write the note */
    quest_finished(q_ptr);
    write_quest_note(TRUE);

    /*
     * Clear out all the remaining monsters.
     * First preserve any artifacts they held.
     */
    for (i = mon_max - 1; i >= 1; i--)
    {
        /* Get this monster */
        monster_type *m_ptr = &mon_list[i];

        /* Skip real monsters */
        if (!m_ptr->r_idx) continue;

        /* Destroy anything they were holding */
        if (m_ptr->hold_o_idx)
        {
            s16b this_o_idx, next_o_idx = 0;

            /* Delete objects */
            for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
            {
                object_type *o_ptr;

                /* Get the object */
                o_ptr = &o_list[this_o_idx];

                /* Hack -- Preserve unknown artifacts */
                if (o_ptr->is_artifact() && !o_ptr->is_known())
                {
                    /* Mega-Hack -- Preserve the artifact */
                    a_info[o_ptr->art_num].a_cur_num = 0;
                }

                /* Get the next object */
                next_o_idx = o_ptr->next_o_idx;

                /* Hack -- efficiency */
                o_ptr->held_m_idx = 0;

                /* Delete the object */
                delete_object_idx(this_o_idx);
            }
        }

        /* Now delete the monster */
        delete_monster_idx(i);

        /* Update monster list window */
        p_ptr->redraw |= PR_WIN_MONLIST;
    }

}


/*
 * Clear a square of any objects, effects and monsters that may be there.
 */
static void clear_square(int y, int x, bool do_wall, u16b feat)
{
    /*
     * Transform the square.
     * First destroy any objects, effects and monsters that may be there
     */
    if (dungeon_info[y][x].monster_idx)
    {
        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        if (!is_monster_native_aux(feat, r_ptr->r_native))
        {
            bool old_seen = m_ptr->ml;
            bool new_seen = FALSE;

            if (!teleport_away(dungeon_info[y][x].monster_idx, 1)) delete_monster(y, x);
            else new_seen = m_ptr->ml;

            /* Print a message if the player sees it */
            if (old_seen)
            {
                /* Get "the monster" or "it" */
                QString m_name = monster_desc(m_ptr, 0);

                if (new_seen) message(QString("%1 blinks.") .arg(capitalize_first(m_name)));
                else message(QString("%1 disappears.") .arg(capitalize_first(m_name)));
            }
        }
    }

    if (dungeon_info[y][x].object_idx)
    {
        s16b this_o_idx, next_o_idx = 0;

        /* Delete objects - first preserve any unidentified objects */
        for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = &o_list[this_o_idx];

            /* Hack -- Preserve unknown artifacts */
            if (o_ptr->is_artifact() && !o_ptr->is_known())
            {
                /* Mega-Hack -- Preserve the artifact */
                a_info[o_ptr->art_num].a_cur_num = 0;
            }

            /* Get the next object */
            next_o_idx = o_ptr->next_o_idx;


            /* Hack - don't destroy the object if it can exist in the new feature */
            if (!do_wall)
            {
                if (!object_hates_feature(feat, o_ptr)) continue;
            }

            delete_object(y, x);
        }
    }

    /* Clear any effects */
    if (dungeon_info[y][x].effect_idx) delete_effects(y, x);
}


/*
 * Alter terrain during wilderness quest
 */
void process_wilderness_quest(void)
{
    int y, x;
    int ice_or_mud = 0;

    /* Every 100 game turns */
    if (p_ptr->game_turn % 100) return;

    /* First, figure out if we are on an ice level or mud level */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            if (dungeon_info[y][x].feature_idx == FEAT_WALL_CRACKED_OVER_BOILING_WATER)
            {
                ice_or_mud++;
                continue;
            }
            else if (dungeon_info[y][x].feature_idx == FEAT_WALL_CRACKED_OVER_BOILING_MUD)
            {
                ice_or_mud--;
                continue;
            }
        }

        /* No need to re-examine the whole level */
        if (ice_or_mud > 25) break;
        else if (ice_or_mud < -25) break;
    }

    /* Now allow the boiling mud-boiling water to spread */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            int i, result;
            bool permanent = FALSE;
            u16b chance = 0;
            bool do_wall = FALSE;

            /* Get the feature */
            u16b feat = dungeon_info[y][x].feature_idx;
            u16b new_feat;

            /* Already fully transformed */
            if (feat == FEAT_WALL_CRACKED_OVER_BOILING_WATER) 		continue;
            else if (feat == FEAT_WALL_CRACKED_OVER_BOILING_MUD)	continue;

            /* Don't do squares close to the player */
            if ((((y - p_ptr->py) * (y - p_ptr->py)) + ((x - p_ptr->px) * (x - p_ptr->px))) <= 100) continue;

            /* Don't alter stairs or permanent walls */
            if (cave_ff1_match(y, x, FF1_PERMANENT))
            {
                if (cave_ff1_match(y, x, (FF1_WALL | FF1_STAIRS))) continue;
            }

            if ((feat == FEAT_FLOOR_WATER_BOILING) || (feat == FEAT_FLOOR_MUD_BOILING))
            {
                do_wall = TRUE;
                chance += 25 + p_ptr->depth / 5;
            }

            /*
             * Check the squares around, come up with a chance
             * for them to convert to a wall or floor.
             */
            for (i = 0; i < 8; i++)
            {

                int yy = y + ddy_ddd[i];
                int xx = x + ddx_ddd[i];
                u16b feat2 = dungeon_info[yy][xx].feature_idx;
                if (!in_bounds_fully(yy, xx)) continue;

                /* Don't fill in around stairs */
                if (cave_ff1_match(yy,xx, FF1_STAIRS))
                {
                    permanent = TRUE;
                    break;
                }

                if ((feat2 == FEAT_WALL_CRACKED_OVER_BOILING_WATER) || (feat2 == FEAT_WALL_CRACKED_OVER_BOILING_MUD)) chance += 25 + p_ptr->depth / 5;
                else if ((feat2 == FEAT_FLOOR_WATER_BOILING) || (feat2 == FEAT_FLOOR_MUD_BOILING)) chance += 15 + p_ptr->depth / 5;
            }

            /* Leave space around the stairs */
            if (permanent) continue;

            result = randint1(1000);

            /* Small chance of something starting in the middle of nowhere */
            if ((result >= 999) && (!chance))
            {
                if (one_in_(5))
                {
                    result = 0;
                    chance = 1;
                }
            }

            /* Not this time */
            if (result > chance) continue;

            /* Determine what the new terrain should be */
            /* Ice level */
            if (ice_or_mud > 0)
            {
                if (do_wall)	new_feat =  FEAT_WALL_CRACKED_OVER_BOILING_WATER;
                else			new_feat =  FEAT_FLOOR_WATER_BOILING;
            }
            /* Boiling Mud */
            else
            {
                if (do_wall)	new_feat =  FEAT_WALL_CRACKED_OVER_BOILING_MUD;
                else 			new_feat =  FEAT_FLOOR_MUD_BOILING;
            }

            /* Clear off the square of objects, effects, and monsters */
            clear_square(y, x, do_wall, new_feat);

            /* Convert the feature */
            cave_set_feat(y, x, new_feat);

            light_spot(y, x);
        }
    }
}


/*
 * Check for quest failure or missing monsters.
 */
void process_guild_quests(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    int cur_quest_monsters = count_quest_monsters(q_ptr);
    int remaining = cur_quest_monsters - q_ptr->q_num_killed;
    int i, y, x;
    int best_r_idx = 0;
    int r_idx;
    int attempts_left = 10000;

    /* No need to process vault quests or fixed quests */
    if (quest_fixed(q_ptr)) return;
    if (q_ptr->q_type == QUEST_VAULT) return;
    if (quest_timed(q_ptr)) return;

    /* We have enough monsters, we are done */
    if (remaining >= (q_ptr->q_max_num - q_ptr->q_num_killed)) return;

    /* Find a legal, distant, unoccupied, space */
    while (attempts_left)
    {
        --attempts_left;

        /* Pick a location */
        y = rand_int(p_ptr->cur_map_hgt);
        x = rand_int(p_ptr->cur_map_wid);

        /* Require a grid that all monsters can exist in. */
        if (!cave_empty_bold(y, x)) continue;

        /* Accept far away grids */
        if (distance(y, x, p_ptr->py, p_ptr->px) >  MAX_SIGHT) break;
    }

    /* Couldn't find a spot */
    if (!attempts_left) return;

    /* Find the right type of monster to put on the level, and right degree of difficulty */
    /* Very simple for quests for a specific monster */
    if (quest_single_r_idx(q_ptr))
    {
        best_r_idx = q_ptr->mon_idx;
    }

    /* Get the theme, if needed */
    if (quest_themed(q_ptr))
    {
        if (q_ptr->q_type == QUEST_THEMED_LEVEL)
        {
            monster_level = p_ptr->depth + THEMED_LEVEL_QUEST_BOOST;
        }

        else /* QUEST_PIT and QUEST_NEST */
        {
            monster_level = p_ptr->depth + PIT_NEST_QUEST_BOOST;
        }

        get_mon_hook(q_ptr->q_theme);
    }

    /* Prepare allocation table */
    get_mon_num_prep();

    if (!best_r_idx)
    {
        monster_race *r_ptr;
        monster_race *r2_ptr = &r_info[best_r_idx];

        /* Quests where the monster is specified (monster quests, unique quests*/
        if (q_ptr->mon_idx) best_r_idx = q_ptr->mon_idx;

        /* 10 chances to get the strongest monster possible */
        else for (i = 0; i < 10; i++)
        {
            r_idx = get_mon_num(monster_level, y, x, 0L);

            if (!best_r_idx)
            {
                best_r_idx = r_idx;
                r2_ptr = &r_info[best_r_idx];
                continue;
            }

            r_ptr = &r_info[r_idx];

            /* Don't use a unique as a replacement */
            if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

            /* Weaker monster.  Don't use it */
            if (r_ptr->mon_power < r2_ptr->mon_power) continue;

            best_r_idx = r_idx;
            r2_ptr = &r_info[best_r_idx];

        }
    }

    if (place_monster_aux(y, x, best_r_idx, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_OVERRIDE)))
    {
        /* Scan the monster list */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr = &mon_list[i];

            /* Ignore dead monsters */
            if (!m_ptr->r_idx) continue;

            /* Make sure we have the right monster race */
            if (m_ptr->r_idx != best_r_idx) continue;

            /*mark it as a quest monster*/
            m_ptr->mflag |= (MFLAG_QUEST);
        }
    }

    /* Reset everything */
    monster_level = p_ptr->depth;
    get_mon_num_hook = NULL;
    get_mon_num_prep();
}
