
/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/cmds.h"
#include "tilebag.h"
#include <QTime>



/*
 * Change dungeon level.
 * Aside from setting the player depth at the beginning of the game,
 * this should be the only place where a player depth is actually
 * changed.
 */
void dungeon_change_level(int dlev)
{
    /* Handle lost greater vaults */
    if (!g_vault_name.isEmpty())
    {
        QString note = "Left the level without entering the ";
        note.append(g_vault_name);

        write_note(note, p_ptr->depth);

        g_vault_name.clear();
    }

    /* New depth */
    p_ptr->depth = dlev;

    /* Leaving */
    p_ptr->leaving_level = TRUE;

    /* Save the game when we arrive on the new level. */
    p_ptr->autosave = TRUE;
    p_ptr->update |= (PU_PLAYER_SCORE);
    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_MAP);
}


/*
 * Remove the ironman ego_items of the probability tables.
 */
static void remove_ironman_ego_items(void)
{
    s16b i;

    alloc_entry *table = alloc_ego_table;

    /* Go through "normal" ego-item types */
    for (i = 0; i < alloc_ego_size; i++)
    {
        ego_item_type *e_ptr = &e_info[table[i].index];

        /*
         * Mega-hack - Allow fireproof books if store services
         * are disabled
         */
        if ((table[i].index == EGO_FIREPROOF) &&
            birth_no_store_services) continue;

        /* Ignore ironman ego-item types */
        if (e_ptr->e_flags3 & TR3_IRONMAN_ONLY)
        {
            /*No chance to be created normally*/
            table[i].prob1 = 0;
            table[i].prob2 = 0;
            table[i].prob3 = 0;
        }
    }
}


/*
 * Remove the ironman items of the probability tables.
 */
static void remove_ironman_items(void)
{
    s16b i;

    alloc_entry *table = alloc_kind_table;

    /* Go through "normal" object types */
    for (i = 0; i < alloc_kind_size; i++)
    {
        object_kind *k_ptr = &k_info[table[i].index];

        /* Ignore ironman object types */
        if (k_ptr->k_flags3 & TR3_IRONMAN_ONLY)
        {
            /*No chance to be generated normally*/
            table[i].prob1 = 0;
            table[i].prob2 = 0;
            table[i].prob3 = 0;

            /*
             * Hack - don't let the player cast the spells from
             * an ironman_only book.  Note this can be a quest item, so it can be tried.
             */
            if ((k_ptr->tval == cp_ptr->spell_book) && (k_ptr->tried == FALSE))
            {
                byte j;

                /* Extract spells */
                for (j = 0; j < SPELLS_PER_BOOK; j++)
                {

                    s16b spell = get_spell_from_list(k_ptr->sval, j);

                    /*skip blank spell slots*/
                    if (spell == -1) continue;

                    /* Don't count Ironman Spells. */
                    p_ptr->spell_flags[spell] |= PY_SPELL_IRONMAN;

                }
            }
        }
    }
}


void launch_game()
{
    load_memory_scores();

    /* Flavor the objects */
    flavor_init();

    init_tile_bags();

    /* Remove ironman ego-items if needed */
    if (!birth_ironman && !birth_no_stores)
    {
        remove_ironman_items();
        remove_ironman_ego_items();
    }

    // Restore entity generation level
    monster_level = p_ptr->depth;

    object_level = p_ptr->depth;

    /* Generate a dungeon level if needed */
    if (!character_dungeon) generate_cave();

    // Load tile specifications
    extract_tiles();

    /* Character is now "complete" */
    character_generated = TRUE;

    /* Start with normal object generation mode */
    object_generation_mode = OB_GEN_MODE_NORMAL;

    /* Start with the item_tester_swap variable as false */
    item_tester_swap = FALSE;

    /* Start playing */
    p_ptr->playing = TRUE;

    /* Save not required yet. */
    p_ptr->autosave = FALSE;

    /* Hack -- Enforce "delayed death" */
    if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_PLAYER_SCORE);
    p_ptr->update |= (PU_TORCH | PU_UPDATE_VIEW | PU_FORGET_VIEW  | PU_DISTANCE);

    handle_stuff();

    ui_update_sidebar_all();
    ui_update_statusbar();
    ui_update_titlebar();
    ui_update_messages();
    ui_update_hotkey_toolbar();
}
