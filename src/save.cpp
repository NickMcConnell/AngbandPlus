
/* File: save.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, Jeff Greene, Diego Gonzalez and ohers
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/loadsave.h"
#include "src/store.h"

static QFile save_file;
static QDataStream out(&save_file);


/*
 * These functions place information into a savefile a byte at a time
 */

/*
 * These functions place information into a savefile a byte at a time
 */


static void wr_byte(byte v)
{
    out << v;
}

static void wr_u16b(u16b v)
{
    out << v;
}

static void wr_s16b(s16b v)
{
    out << v;
}

static void wr_u32b(u32b v)
{
    out << v;
}

static void wr_s32b(s32b v)
{
    out << v;
}

static void wr_string(QString str)
{
    out << str;
}


/*
 * These functions write info in larger logical records
 */


/*
 * Write an "item" record
 */
static void wr_item(const object_type *o_ptr)
{
    wr_s16b(o_ptr->k_idx);

    /* Location */
    wr_byte(o_ptr->iy);
    wr_byte(o_ptr->ix);

    wr_byte(o_ptr->tval);
    wr_byte(o_ptr->sval);
    wr_s16b(o_ptr->pval);

    wr_byte(o_ptr->discount);

    wr_byte(o_ptr->number);
    wr_s16b(o_ptr->weight);

    wr_byte(o_ptr->art_num);
    wr_byte(o_ptr->ego_num);

    wr_s16b(o_ptr->timeout);

    wr_s16b(o_ptr->to_h);
    wr_s16b(o_ptr->to_d);
    wr_s16b(o_ptr->to_a);
    wr_s16b(o_ptr->ac);
    wr_byte(o_ptr->dd);
    wr_byte(o_ptr->ds);

    wr_u32b(o_ptr->ident);

    wr_byte(o_ptr->marked);

    wr_s16b(o_ptr->mimic_r_idx);

    /* Old flags */
    wr_u32b(0L);
    wr_u32b(0L);
    wr_u32b(0L);

    /* Held by monster index */
    wr_s16b(o_ptr->held_m_idx);

    /* Extra information */
    wr_byte(o_ptr->xtra1);
    wr_u32b(o_ptr->xtra2);

    /* Save the inscription (if any) */
    wr_string(o_ptr->inscription);

    // Write the object kind verify bool array
    for (int i = 0; i < VERIFY_MAX; i++)
    {
        wr_byte(o_ptr->use_verify[i]);
    }

    /* Object history */
    wr_byte(o_ptr->origin_nature);
    wr_s16b(o_ptr->origin_dlvl);
    wr_s16b(o_ptr->origin_r_idx);
    wr_string(o_ptr->origin_m_name);


}



/*
 * Special monster flags that get saved in the savefile
 */
#define SAVE_MON_FLAGS (MFLAG_STERILE | MFLAG_ACTV | MFLAG_TOWN | MFLAG_WARY | \
                        MFLAG_SLOWER | MFLAG_FASTER | MFLAG_ALWAYS_CAST | \
                        MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD | MFLAG_BONUS_ITEM | \
                        MFLAG_HIT_BY_RANGED | MFLAG_HIT_BY_MELEE | MFLAG_QUEST | MFLAG_DESPERATE | \
                        MFLAG_NEED_PASS_WALL_FLOW | MFLAG_QUEST_SUMMON | MFLAG_JUST_SCARED)


/*
 * Write a "monster" record
 */
static void wr_monster(const monster_type *m_ptr)
{
    u32b tmp32u;
    int i;

    wr_s16b(m_ptr->r_idx);

    wr_byte(m_ptr->fy);
    wr_byte(m_ptr->fx);
    wr_s16b(m_ptr->hp);
    wr_s16b(m_ptr->maxhp);
    wr_byte(m_ptr->m_speed);
    wr_s16b(m_ptr->m_energy);

    /* Find the number of monster timed effects */
    wr_byte(MON_TMD_MAX);

    /* Write all the monster timed effects, in a loop */
    for (i = 0; i < MON_TMD_MAX; i++)	wr_s16b(m_ptr->m_timed[i]);

    /*save the temporary flags*/
    tmp32u = m_ptr->mflag & (SAVE_MON_FLAGS);
    wr_u32b(tmp32u);
    wr_u32b(m_ptr->smart);
    wr_byte(m_ptr->target_y);
    wr_byte(m_ptr->target_x);
    wr_byte(m_ptr->mana);
    wr_byte(0);
}

/*
 * Write an "item" record
 */
static void wr_effect(const effect_type *x_ptr)
{

    wr_byte(x_ptr->x_type);
    wr_u16b(x_ptr->x_f_idx);

    wr_byte(x_ptr->x_cur_y);
    wr_byte(x_ptr->x_cur_x);


    wr_byte(x_ptr->x_countdown);
    wr_byte(x_ptr->x_repeats);

    wr_u16b(x_ptr->x_power);

    wr_s16b(x_ptr->x_source);

    wr_u16b(x_ptr->x_flags);

    wr_s16b(x_ptr->x_r_idx);
}

static void wr_hotkey(int hotkey)
{
    single_hotkey *shk_ptr = &player_hotkeys[hotkey];

    wr_string(shk_ptr->hotkey_name);
    wr_string(shk_ptr->hotkey_button_name);
    wr_s16b((s16b)shk_ptr->hotkey_button);
    wr_u16b((u16b)shk_ptr->hotkey_steps.size());
    for (u16b i = 0; i < shk_ptr->hotkey_steps.size(); i++)
    {
        wr_byte(shk_ptr->hotkey_steps[i].step_commmand);

        cmd_arg args = shk_ptr->hotkey_steps[i].step_args;
        wr_string(args.string1);
        wr_string(args.string2);
        wr_s16b((s16b)args.choice);
        wr_s16b((s16b)args.item);
        wr_s16b((s16b)args.number);
        wr_s16b((s16b)args.direction);
        wr_s16b((s16b)args.slot);
        wr_s16b((s16b)args.repeats);
        wr_s16b((s16b)args.k_idx);
        if (args.verify) wr_byte(1);
        else wr_byte(0);
    }
}


/*
 * Write a "lore" record
 */
static void wr_monster_lore(int r_idx)
{
    monster_lore *l_ptr = &l_list[r_idx];

    /* Count sights/deaths/kills */
    wr_s16b(l_ptr->sights);
    wr_s16b(l_ptr->deaths);
    wr_s16b(l_ptr->pkills);
    wr_s16b(l_ptr->tkills);

    /* Count wakes and ignores */
    wr_byte(l_ptr->wake);
    wr_byte(l_ptr->ignore);

    /* Extra stuff */
    wr_byte(l_ptr->xtra1);
    wr_byte(l_ptr->xtra2);

    /* Count drops */
    wr_byte(l_ptr->drop_gold);
    wr_byte(l_ptr->drop_item);

    /* Count spells */
    wr_byte(l_ptr->ranged);

    /* Count blows of each type */
    for (int i = 0; i < MONSTER_BLOW_MAX; i++) wr_byte(l_ptr->blows[i]);

    /* Memorize flags */
    wr_u32b(l_ptr->r_l_flags1);
    wr_u32b(l_ptr->r_l_flags2);
    wr_u32b(l_ptr->r_l_flags3);
    wr_u32b(l_ptr->r_l_flags4);
    wr_u32b(l_ptr->r_l_flags5);
    wr_u32b(l_ptr->r_l_flags6);
    wr_u32b(l_ptr->r_l_flags7);
    wr_u32b(l_ptr->r_l_native);

    /* Later (?) */
    wr_byte(0);
    wr_byte(0);
    wr_byte(0);
}


/*
 * Write an "xtra" record
 */
static void wr_object_kind_info(int k_idx)
{
    byte tmp8u = 0;

    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->aware) tmp8u |= 0x01;
    if (k_ptr->tried) tmp8u |= 0x02;
    wr_byte(tmp8u);

    /*write the squelch settings*/
    tmp8u = k_ptr->squelch;
    wr_byte(tmp8u);

    // Write the object kind verify bool array
    for (int i = 0; i < VERIFY_MAX; i++)
    {
        tmp8u = k_ptr->use_verify[i];
        wr_byte(tmp8u);
    }

    wr_string(k_ptr->autoinscribe);

}


/*
 * Write a "store" record
 */
static void wr_store(const store_type *st_ptr)
{
    int j;

    /* Unused */
    wr_u32b(0L);

    /* Save the "insults" */
    wr_s16b(0);

    /* Save the current owner */
    wr_byte(st_ptr->owner);

    /* Save the stock size */
    wr_byte(st_ptr->stock_num);

    /* Save the "haggle" info */
    wr_s16b(0);
    wr_s16b(0);

    /* Save the stock */
    for (j = 0; j < st_ptr->stock_num; j++)
    {
        /* Save each item in stock */
        wr_item(&st_ptr->stock[j]);
    }
}

/*
 * Write an "artifact lore" record
 */
static void wr_artifact_lore(int a_idx)
{
    byte tmp8u = 0;

    /* We know about this artifact */
    if (a_l_list[a_idx].was_fully_identified) tmp8u = 1;

    /* Write the flags */
    wr_byte(tmp8u);

    /* For future use */
    wr_byte(0);
    wr_byte(0);
    wr_byte(0);
}


/*
 * Write a "terrain lore" record
 */
static void wr_feature_lore(int f_idx)
{
    int i;

    /* Get the feature */
    feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];
    byte tmp8u = 0;

    /* Save the "everseen flag" */
    if (f_ptr->f_everseen) tmp8u |= 0x01;

    wr_byte(tmp8u);

    /* Write the terrain_lore memory*/
    wr_byte(f_l_ptr->f_l_sights);

    /*Write the lore flags*/
    wr_u32b(f_l_ptr->f_l_flags1);
    wr_u32b(f_l_ptr->f_l_flags2);
    wr_u32b(f_l_ptr->f_l_flags3);

    wr_byte(f_l_ptr->f_l_defaults);

    /*record the max amount of feat states*/
    wr_byte(MAX_FEAT_STATES);

    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        wr_byte(f_l_ptr->f_l_state[i]);
    }

    wr_byte(f_l_ptr->f_l_power);

    wr_byte(f_l_ptr->f_l_dam_non_native);
    wr_byte(f_l_ptr->f_l_native_moves);
    wr_byte(f_l_ptr->f_l_non_native_moves);
    wr_byte(f_l_ptr->f_l_native_to_hit_adj);
    wr_byte(f_l_ptr->f_l_non_native_to_hit_adj);
    wr_byte(f_l_ptr->f_l_stealth_adj);

}


/*
 * Write RNG state
 */
static int wr_randomizer(void)
{
    int i;

    /* Zero */
    wr_u16b(0);

    /* Place */
    wr_u16b(Rand_place);

    /* State */
    for (i = 0; i < RAND_DEG; i++)
    {
        wr_u32b(Rand_state[i]);
    }

    /* Success */
    return (0);
}


/*
 * Write the "options"
 */
static void wr_options(void)
{
    int i;

    u32b flag[8];
    u32b mask[8];

    /*** Oops ***/

    /* Oops */
    for (i = 0; i < 4; i++) wr_u32b(0L);


    /*** Special Options ***/

    /* Write "delay_factor" */
    wr_byte(op_ptr->delay_factor);

    /* Write "hitpoint_warn" */
    wr_byte(op_ptr->hitpoint_warn);

    wr_u16b(0);	/* oops */


    /*** Normal options ***/

    /* Reset */
    for (i = 0; i < 8; i++)
    {
        flag[i] = 0L;
        mask[i] = 0L;
    }

    /* Analyze the options */
    for (i = 0; i < OPT_MAX; i++)
    {
        int os = i / 32;
        int ob = i % 32;

        /* Process real entries */
        if (options[i].name.length())
        {
            /* Set flag */
            if (op_ptr->opt[i])
            {
                /* Set */
                flag[os] |= (1L << ob);
            }

            /* Set mask */
            mask[os] |= (1L << ob);
        }
    }

    /* Dump the flags */
    for (i = 0; i < 8; i++) wr_u32b(flag[i]);

    /* Dump the masks */
    for (i = 0; i < 8; i++) wr_u32b(mask[i]);

    // Old ANGBAND_TERM_MAX window flags info
    for (i = 0; i < 64; i++) wr_byte(0);
}


/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
    int i;

    wr_string(op_ptr->full_name);

    wr_string(p_ptr->died_from);

    wr_string(p_ptr->history);

    /* Race/Class/Gender/Spells */
    wr_byte(p_ptr->prace);
    wr_byte(p_ptr->pclass);
    wr_byte(p_ptr->psex);
    wr_byte(0);	/* oops */

    wr_byte(p_ptr->hitdie);
    wr_byte(p_ptr->expfact);

    wr_s16b(p_ptr->age);
    wr_s16b(p_ptr->ht);
    wr_s16b(p_ptr->wt);

    /* Dump the stats (maximum and current) */
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_base_max[i]);
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_base_cur[i]);
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_birth[i]);
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_quest_add[i]);

    wr_s16b(p_ptr->ht_birth);
    wr_s16b(p_ptr->wt_birth);
    wr_s16b(p_ptr->sc_birth);
    wr_s32b(p_ptr->au_birth);

    /* Ignore the transient stats */
    for (i = 0; i < 12; ++i) wr_s16b(0);

    wr_u16b(p_ptr->q_fame);
    wr_u16b(p_ptr->deferred_rewards);

    wr_u32b(p_ptr->au);

    wr_u32b(p_ptr->max_exp);
    wr_u32b(p_ptr->exp);
    wr_u16b(p_ptr->exp_frac);
    wr_s16b(p_ptr->lev);

    wr_s16b(p_ptr->mhp);
    wr_s16b(p_ptr->chp);
    wr_u16b(p_ptr->chp_frac);

    wr_s16b(p_ptr->msp);
    wr_s16b(p_ptr->csp);
    wr_u16b(p_ptr->csp_frac);

    /* Max Player and Dungeon Levels */
    wr_s16b(p_ptr->max_lev);
    wr_s16b(p_ptr->max_depth);
    wr_s16b(p_ptr->recall_depth);
    wr_s16b(p_ptr->quest_depth);

    /* More info */
    wr_s16b(0);	/* oops */
    wr_s16b(0);	/* oops */
    wr_s16b(0);	/* oops */
    wr_s16b(p_ptr->sc);
    wr_s16b(0);	/* oops */

    wr_s16b(0);		/* old "rest" */
    wr_s16b(p_ptr->food);

    wr_s16b(0);	/* old "food_digested" */
    wr_s16b(0);	/* old "protection" */
    wr_s16b(p_ptr->p_energy);
    wr_s16b(p_ptr->word_recall);
    wr_s16b(p_ptr->state.see_infra);

    wr_byte(p_ptr->confusing);
    wr_byte(0);	/* oops */
    wr_byte(p_ptr->searching);
    wr_byte(0);	/* oops */
    wr_byte(0);	/* oops */
    wr_byte(0);

    /* Find the number of timed effects */
    wr_byte(TMD_MAX);

    /* Write all the effects, in a loop */
    for (i = 0; i < TMD_MAX; i++) wr_s16b(p_ptr->timed[i]);

    /* 4gai use */
    wr_s16b(p_ptr->base_wakeup_chance);
    wr_s16b(total_wakeup_chance);

    /* Save item-quality squelch sub-menu */
    for (i = 0; i < SQUELCH_BYTES; i++) wr_byte(squelch_level[i]);

    /* Store the name of the current greater vault */
    wr_string(g_vault_name);

    /* Save the current number of ego-item types */
    wr_u16b(z_info->e_max);

    /* Save ego-item squelch settings */
    for (i = 0; i < z_info->e_max; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        if (e_ptr->squelch) wr_byte(1);
        else wr_byte(0);
    }

    /* Store the bones file selector, if the player is not dead. -LM- */
    wr_s16b(player_ghost_num);

    /* Store the number of thefts on the level. -LM- */
    wr_byte(0);

    /* Store number of monster traps on this level. -LM- */
    wr_byte(num_trap_on_level);

    /* Future use */
    for (i = 0; i < 13; i++) wr_byte(0);

    /* Save the current dungeon summon mask */
    wr_u32b (dungeon_summon_mask_f7);

    /* Random artifact seed */
    wr_u32b(seed_randart);


    /* Ignore some flags */
    wr_u32b(0L);	/* oops */
    wr_u32b(0L);	/* oops */
    wr_u32b(0L);	/* oops */


    /* Write the "special seeds" */
    wr_u32b(seed_flavor);
    wr_u32b(seed_town);
    wr_u32b(seed_ghost);

    /* Special stuff */
    wr_u16b(p_ptr->panic_save);
    wr_u16b(p_ptr->total_winner);

    // Record wizard mode;
    if (p_ptr->is_wizard) wr_byte(TRUE);
    else wr_byte(FALSE);

    wr_byte(p_ptr->terminated);

    /* Write death */
    wr_byte(p_ptr->is_dead);

    /* Write feeling */
    wr_byte(feeling);

    /* Turn of last "feeling" */
    wr_byte(do_feeling);

    /* Current turn */
    wr_s32b(p_ptr->game_turn);

    /*Current Player Turn*/
    wr_s32b(p_ptr->p_turn);

    /* Turn counter for the quest indicator */
    {
        /* Get the timer value. Clear the last bit */
        u16b tmp16u = quest_indicator_timer & ~(QUEST_INDICATOR_COMPLETE_BIT);

        /* Turn on the last bit if the quest was completed */
        if (quest_indicator_complete) tmp16u |= (QUEST_INDICATOR_COMPLETE_BIT);

        /* Write timer + complete bit */
        wr_u16b(tmp16u);
    }
}


/*
 * Dump the random artifacts
 */
static void wr_randarts(void)
{
    int i, begin;

    if (birth_rand_artifacts) begin = 0;
    else begin = z_info->art_norm_max;

    wr_u16b(begin);
    wr_u16b(z_info->art_max);
    wr_u16b(z_info->art_norm_max);


    for (i = begin; i < z_info->art_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        wr_string(a_ptr->a_name);

        wr_byte(a_ptr->tval);
        wr_byte(a_ptr->sval);
        wr_s16b(a_ptr->pval);

        wr_s16b(a_ptr->to_h);
        wr_s16b(a_ptr->to_d);
        wr_s16b(a_ptr->to_a);
        wr_s16b(a_ptr->ac);

        wr_byte(a_ptr->dd);
        wr_byte(a_ptr->ds);

        wr_s16b(a_ptr->weight);

        wr_s32b(a_ptr->cost);

        wr_u32b(a_ptr->a_flags1);
        wr_u32b(a_ptr->a_flags2);
        wr_u32b(a_ptr->a_flags3);
        wr_u32b(a_ptr->a_native);

        wr_byte(a_ptr->a_level);
        wr_byte(a_ptr->a_rarity);

        wr_byte(a_ptr->activation);
        wr_u16b(a_ptr->time);
        wr_u16b(a_ptr->randtime);
    }

}

/*
 * Write the notes into the savefile. Every savefile has at least NOTES_MARK.
 */
static void wr_notes(void)
{
    //record the size of the array
    u16b tmp16u = notes_log.size();
    wr_u16b(tmp16u);

    /* Write the notes */
    for (int i = 0; i <tmp16u; i++)
    {
        wr_byte(notes_log[i].player_level);
        wr_s16b(notes_log[i].dun_depth);
        wr_s32b(notes_log[i].game_turn);
        wr_string(notes_log[i].recorded_note);
    }
}



/*
 * The cave grid flags that get saved in the savefile
 */
#define IMPORTANT_FLAGS (CAVE_MARK | CAVE_GLOW | CAVE_ICKY | CAVE_DTRAP | \
                         CAVE_ROOM | CAVE_SPECIAL | CAVE_G_VAULT | CAVE_EXPLORED)


/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
    int i, y, x;

    byte tmp8u;

    byte count;
    byte prev_char;


    /*** Basic info ***/

    /* Dungeon specific info follows */
    wr_u16b(p_ptr->depth);
    wr_u16b(p_ptr->dungeon_type);
    wr_u16b(p_ptr->py);
    wr_u16b(p_ptr->px);
    wr_byte(p_ptr->cur_map_hgt);
    wr_byte(p_ptr->cur_map_wid);
    wr_u16b(altered_inventory_counter);
    wr_u16b(0);


    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Extract the important cave_info flags */
            tmp8u = (dungeon_info[y][x].cave_info & (IMPORTANT_FLAGS));

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == UCHAR_MAX))
            {
                wr_byte((byte)count);
                wr_byte((byte)prev_char);
                prev_char = tmp8u;
                count = 1;
            }

            /* Continue the run */
            else
            {
                count++;
            }
        }
    }

    /* Flush the data (if any) */
    if (count)
    {
        wr_byte((byte)count);
        wr_byte((byte)prev_char);
    }


    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
    for (y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (x = 0; x < p_ptr->cur_map_wid; x++)
        {
            /* Extract a byte */
            tmp8u = dungeon_info[y][x].feature_idx;

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == UCHAR_MAX))
            {
                wr_byte((byte)count);
                wr_byte((byte)prev_char);
                prev_char = tmp8u;
                count = 1;
            }

            /* Continue the run */
            else
            {
                count++;
            }
        }
    }

    /* Flush the data (if any) */
    if (count)
    {
        wr_byte((byte)count);
        wr_byte((byte)prev_char);
    }


    /*** Compact ***/

    /* Compact the objects */
    compact_objects(0);

    /* Compact the monsters */
    compact_monsters(0);


    /*** Dump objects ***/

    /* Total objects */
    wr_u16b(o_max);

    /* Dump the objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Dump it */
        wr_item(o_ptr);
    }


    /*** Dump the monsters ***/

    wr_u16b(z_info->r_max);

    /* Write Monster race info */
    for (i = 0; i < z_info->r_max; i++) wr_byte(r_info[i].max_num);

    /* Total monsters */
    wr_u16b(mon_max);

    /* Dump the monsters */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];

        /* Dump it */
        wr_monster(m_ptr);
    }

    /*** Dump the effects ***/

    /* Total Effects */
    wr_u16b(x_max);

    /* Dump the Effects */
    for (i = 1; i < x_max; i++)
    {
        effect_type *x_ptr = &x_list[i];

        /* Dump it */
        wr_effect(x_ptr);
    }

    for (i = 0; i < NUM_HOTKEYS; i++)
    {
        wr_hotkey(i);
    }
}



/*
 * Actually write a save-file
 */
static bool wr_savefile(void)
{
    int i;

    u16b tmp16u;

    // Open the current file       
    save_file.setFileName(current_savefile);
    save_file.open(QIODevice::WriteOnly);

    // Ensure the data is read and written consistently
    out.setVersion(QDataStream::Qt_5_2);

    /*** Actually write the file ***/

    /* Dump the file header */
    wr_byte(VERSION_MAJOR);
    wr_byte(VERSION_MINOR);
    wr_byte(VERSION_PATCH);
    wr_byte(VERSION_EXTRA);
    wr_byte(game_mode);

    /* Operating system */
    wr_u32b(sf_xtra);

    /* Time file last saved */
    wr_u32b(sf_when);

    /* Number of past lives */
    wr_u16b(sf_lives);

    /* Number of times saved */
    wr_u16b(sf_saves);

    /* Space */
    wr_u32b(0L);
    wr_u32b(0L);

    /* Write the RNG state */
    wr_randomizer();

    /* Write the boolean "options" */
    wr_options();

    /* Dump the number of "messages" */
    tmp16u = message_list.size();
    if (tmp16u > 160) tmp16u = 160;
    wr_u16b(tmp16u);

    /* Dump the messages (newest first!) */
    for (i = 0; i <tmp16u; i++)
    {        
        wr_string(message_list[i].message);
        wr_byte(message_list[i].msg_color.red());
        wr_byte(message_list[i].msg_color.green());
        wr_byte(message_list[i].msg_color.blue());
        wr_u16b(message_list[i].repeats);
        wr_s32b(message_list[i].message_turn);
        wr_byte(message_list[i].append);
    }

    /* Dump the object memory */
    tmp16u = z_info->k_max;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) wr_object_kind_info(i);

    /* Hack -- Dump the quests */
    tmp16u = z_info->q_max;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        quest_type *q_ptr = &q_info[i];

        wr_byte(q_ptr->q_type);

        /* Only limited info for permanent quests.  The rest is detailed in quest.txt */
        if (q_ptr->q_type == QUEST_PERMANENT)
        {
            wr_byte(q_ptr->q_flags);
            wr_s16b(q_ptr->q_num_killed);
            continue;
        }

        wr_u16b(q_ptr->q_reward);
        wr_u16b(q_ptr->q_fame_inc);
        wr_byte(q_ptr->base_level);
        wr_byte(q_ptr->q_theme);
        wr_s16b(q_ptr->mon_idx);
        wr_s32b(q_ptr->turn_counter);
        wr_s16b(q_ptr->q_num_killed);
        wr_s16b(q_ptr->q_max_num);
        wr_byte(q_ptr->q_flags);
    }

    /* Hack -- Dump the artifacts */
    tmp16u = z_info->art_max;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        wr_byte(a_ptr->a_cur_num);
        wr_byte(0);
        wr_byte(0);
        wr_byte(0);
    }


    /* Write the "extra" information */
    wr_extra();

    /* Dump the "player hp" entries */
    tmp16u = z_info->max_level;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        wr_s16b(p_ptr->player_hp[i]);
    }

    /* Write spell data */
    wr_u16b(PY_MAX_SPELLS);

    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        wr_byte(p_ptr->spell_flags[i]);
    }

    /* Dump the ordered spells */
    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        wr_byte(p_ptr->spell_order[i]);
    }

    /*Write the randarts*/
    wr_randarts();

    /*Copy the notes file into the savefile*/
    wr_notes();

    /* Write the inventory */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Dump index */
        wr_u16b((u16b)i);

        /* Dump object */
        wr_item(o_ptr);
    }

    /* Add a sentinel */
    wr_u16b(0xFFFF);

    /* Note the stores */
    tmp16u = MAX_STORES;
    wr_u16b(tmp16u);

    /* Dump the stores */
    for (i = 0; i < tmp16u; i++) wr_store(&store[i]);

    /* Player is not dead, write the dungeon */
    if (!p_ptr->is_dead)
    {
        /* Dump the dungeon */
        wr_dungeon();
    }

    save_file.close();

    /* Successful save */
    return TRUE;
}


/*
 * Write the player scores
 */
static bool wr_scores(void)
{
    QString scores_filename = QString("scores.npp");

    if (game_mode == GAME_NPPANGBAND) scores_filename.prepend("nppangband_");
    else if (game_mode == GAME_NPPMORIA) scores_filename.prepend("nppmoria_");
    else return (FALSE);

    scores_filename.prepend(QString("%1/") .arg(npp_dir_bone.path()));

    int i;

    u16b tmp16u;

    // Open the current file
    save_file.setFileName(scores_filename);
    save_file.open(QIODevice::WriteOnly);

    // Ensure the data is read and written consistently
    out.setVersion(QDataStream::Qt_5_2);

    /*** Actually write the file ***/

    /* Dump the file header */
    wr_byte(VERSION_MAJOR);
    wr_byte(VERSION_MINOR);
    wr_byte(VERSION_PATCH);
    wr_byte(VERSION_EXTRA);
    wr_byte(game_mode);

    /* Dump the number of "scores" */
    tmp16u = player_scores_list.size();
    if (tmp16u > 30) tmp16u = 30;
    wr_u16b(tmp16u);

    /* Dump the scores (newest first!) */
    for (i = 0; i <tmp16u; i++)
    {
        wr_string(player_scores_list[i].version);
        wr_u32b(player_scores_list[i].score);
        wr_s32b(player_scores_list[i].turns);
        wr_string(player_scores_list[i].date_time);
        wr_string(player_scores_list[i].p_name);
        wr_string(player_scores_list[i].p_sex);
        wr_string(player_scores_list[i].p_race);
        wr_string(player_scores_list[i].p_class);
        wr_s16b(player_scores_list[i].cur_level);
        wr_s16b(player_scores_list[i].cur_depth);
        wr_s32b(player_scores_list[i].cur_exp);
        wr_s16b(player_scores_list[i].max_level);
        wr_s16b(player_scores_list[i].max_depth);
        wr_s32b(player_scores_list[i].max_exp);
        wr_u16b(player_scores_list[i].fame);
        wr_string(player_scores_list[i].death_how);
    }

    save_file.close();

    /* Successful save */
    return TRUE;
}

/*
 * Write the player memory
 */
static bool wr_memory(void)
{
    u16b tmp16u;
    int i;

    // Don't save wizard memory
    if (p_ptr->is_wizard) return TRUE;

    QString memory_filename = QString("memory.npp");

    if (game_mode == GAME_NPPANGBAND) memory_filename.prepend("nppangband_");
    else if (game_mode == GAME_NPPMORIA) memory_filename.prepend("nppmoria_");
    else return (FALSE);

    memory_filename.prepend(QString("%1/") .arg(npp_dir_bone.path()));

    // Open the current file
    save_file.setFileName(memory_filename);
    save_file.open(QIODevice::WriteOnly);

    // Ensure the data is read and written consistently
    out.setVersion(QDataStream::Qt_5_2);

    /*** Actually write the file ***/

    /* Dump the file header */
    wr_byte(VERSION_MAJOR);
    wr_byte(VERSION_MINOR);
    wr_byte(VERSION_PATCH);
    wr_byte(VERSION_EXTRA);
    wr_byte(game_mode);

    /* Dump the monster lore */
    tmp16u = z_info->r_max;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) wr_monster_lore(i);

    /* Dump the current number of terrain features */
    tmp16u = z_info->f_max;
    wr_u16b(tmp16u);

    /* Dump terrain lore */
    for (i = 0; i < tmp16u; i++) wr_feature_lore(i);

    /* Dump the current number of artifacts (normal + special) */
    tmp16u = z_info->art_norm_max;
    wr_u16b(tmp16u);

    /* Dump artifact lore */
    for (i = 0; i < tmp16u; i++) wr_artifact_lore(i);

    /* Dump the object everseen */
    tmp16u = z_info->k_max;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        object_kind *k_ptr = &k_info[i];

        if (k_ptr->everseen) wr_byte(1);
        else wr_byte(0);
    }

    /* Save the current number of ego-item types */
    wr_u16b(z_info->e_max);

    /* Save ego-item squelch settings */
    for (i = 0; i < z_info->e_max; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        if (e_ptr->everseen) wr_byte(1);
        else wr_byte(0);
    }

    save_file.close();

    /* Successful save */
    return TRUE;
}


/*
 * Attempt to save the player in a savefile
 */
bool save_player(void)
{
    save_player_ghost_file();

    /* Attempt to save the player */
    if (wr_savefile())
    {
        /* Hack -- Pretend the character was loaded */
        character_loaded = TRUE;

        if (!wr_scores()) return (FALSE);
        if (!wr_memory()) return (FALSE);

        /* Success */
        return (TRUE);
    }

    /* Oops */
    return (FALSE);
}


void do_hotkey_export(QString file_name)
{
    // Open the current file
    save_file.setFileName(file_name);
    save_file.open(QIODevice::WriteOnly);

    // Ensure the data is read and written consistently
    out.setVersion(QDataStream::Qt_5_2);

    /*** Actually write the file ***/

    /* Dump the game mode */
    wr_byte(game_mode);

    for (int i = 0; i < NUM_HOTKEYS; i++)
    {
        wr_hotkey(i);
    }

    save_file.close();
}
