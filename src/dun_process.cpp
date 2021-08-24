/* File: was dungeon.c */

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
#include "src/store.h"
#include "src/player_command.h"
#include "src/hotkeys.h"




/*
 * Sense the inventory
 */
static void sense_inventory(void)
{
    int i;

    int plev = p_ptr->lev;

    bool heavy = (cp_ptr->pseudo_id_heavy());

    int feel;

    object_type *o_ptr;

    QString o_name;

    int base = cp_ptr->sense_base;
    int divider;


    /*** Check for "sensing" ***/

    /* No sensing when confused */
    if (p_ptr->timed[TMD_CONFUSED]) return;

    if (cp_ptr->flags & CF_PSEUDO_ID_IMPROV)
    {
        divider = (plev * plev) + cp_ptr->sense_div;
    }
    else
    {
        divider = plev + cp_ptr->sense_div;
    }

    if (!one_in_(base/divider)) return;


    /*** Sense everything ***/

    /* Check everything */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        int squelch = SQUELCH_NO;

        o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Sensing the swap weapon is kind of cheating */
        if (birth_swap_weapons && (i == INVEN_SWAP_WEAPON))	continue;

        /* Skip non-sense checks */
        if (!o_ptr->can_be_pseudo_ided()) continue;

        /* It already has a discount or special inscription */
        if ((o_ptr->discount > 0) &&
            (o_ptr->discount != INSCRIP_INDESTRUCTIBLE)) continue;

        /* It has already been sensed, do not sense it again */
        if (o_ptr->ident & (IDENT_SENSE)) continue;

        /* It is known, no information needed */
        if (o_ptr->is_known()) continue;

        /* 80% failure on inventory items */
        if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

        /* Indestructible objects are either excellent or terrible */
        if (o_ptr->discount == INSCRIP_INDESTRUCTIBLE)
            heavy = TRUE;

        /* Check for a feeling */
        feel = (heavy ? o_ptr->pseudo_heavy() : o_ptr->pseudo_light());

        /* Skip non-feelings */
        if (!feel) continue;

        /* Squelch it? */
        if (i < INVEN_WIELD)
        {
            squelch = squelch_itemp(o_ptr, feel, FALSE);
        }

        /* Stop searching */
        disturb(TRUE, FALSE);

        /* Get an object description */
        o_name = object_desc(o_ptr, ODESC_FULL);

        /* Message (equipment) */

        sound(MSG_PSEUDOID);
        if (i >= INVEN_WIELD)
        {
            message(QString("You feel the %1 (%2) you are %3 %4 %5.") .arg(o_name)
                       .arg(index_to_label(i)) .arg(describe_use(i))
                       .arg((o_ptr->number == 1) ? "is" : "are") .arg(inscrip_text[feel - INSCRIP_NULL]));
        }

        /* Message (inventory) */
        else
        {
            message(QString("You feel the %1 (%2) in your pack %3 %4%5.") .arg(o_name)
                    .arg(index_to_label(i))
                    .arg((o_ptr->number == 1) ? "is" : "are") .arg(inscrip_text[feel - INSCRIP_NULL])
                    .arg(squelch_to_label(squelch)));
        }

        /* Sense the object */
        o_ptr->discount = feel;

        if (feel == INSCRIP_AVERAGE)
        {
            /* Identify it */
            o_ptr->mark_known(TRUE);
        }

        else
        {
            /* The object has been "sensed" */
            o_ptr->ident |= (IDENT_SENSE);
        }

        /* Squelch it if necessary */
        do_squelch_item(squelch, i, o_ptr);

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    }
}



/*
 * Regenerate hit points
 */
static void regenhp(int percent)
{
    s32b new_chp, new_chp_frac;
    int old_chp;

    /* Save the old hitpoints */
    old_chp = p_ptr->chp;

    /* Extract the new hitpoints */
    new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
    p_ptr->chp += (s16b)(new_chp >> 16);	/* div 65536 */

    /* check for overflow */
    if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = SHRT_MAX;
    new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
    if (new_chp_frac >= 0x10000L)
    {
        p_ptr->chp_frac = (u16b)(new_chp_frac - 0x10000L);
        p_ptr->chp++;
    }
    else
    {
        p_ptr->chp_frac = (u16b)new_chp_frac;
    }

    /* Fully healed */
    if (p_ptr->chp >= p_ptr->mhp)
    {
        p_ptr->chp = p_ptr->mhp;
        p_ptr->chp_frac = 0;
    }

    /* Notice changes */
    if (old_chp != p_ptr->chp)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }
}


/*
 * Regenerate mana points
 */
static void regenmana(int percent)
{
    s32b new_mana, new_mana_frac;
    int old_csp;

    old_csp = p_ptr->csp;
    new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
    p_ptr->csp += (s16b)(new_mana >> 16);	/* div 65536 */
    /* check for overflow */
    if ((p_ptr->csp < 0) && (old_csp > 0))
    {
        p_ptr->csp = SHRT_MAX;
    }
    new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
    if (new_mana_frac >= 0x10000L)
    {
        p_ptr->csp_frac = (u16b)(new_mana_frac - 0x10000L);
        p_ptr->csp++;
    }
    else
    {
        p_ptr->csp_frac = (u16b)new_mana_frac;
    }

    /* Must set frac to zero even if equal */
    if (p_ptr->csp >= p_ptr->msp)
    {
        p_ptr->csp = p_ptr->msp;
        p_ptr->csp_frac = 0;
    }

    /* Redraw mana */
    if (old_csp != p_ptr->csp)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }
}

/*
 * Give the monsters terrain damage (once per 10 game turns)
 */
static void monster_terrain_damage(void)
{
    int i;

    /* Regenerate everyone */
    for (i = 1; i < mon_max; i++)
    {
        /* Check the i'th monster */
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        /* Get the feature */
        u16b feat = dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx;
        feature_type *f_ptr = &f_info[feat];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Monsters in non-native terrain take damage, and isn't flying */
        if ((f_ptr->dam_non_native > 0) &&
            !is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr) &&
            !MONSTER_CAN_FLY(m_ptr, feat))
        {
            int gf_type = 0;

            u16b tmd_flag = (MON_TMD_FLG_NOTIFY);

            if (m_ptr->ml) tmd_flag |= MON_TMD_FLG_SEEN;

            /* Wake it up */
            mon_clear_timed(i, MON_TMD_SLEEP, tmd_flag);

            /*If we saw this, count this in the lore*/
            if (m_ptr->ml)
            {
                feature_lore *f_l_ptr = &f_l_list[feat];

                /*Count the number of times this damage has been felt*/
                if (f_l_ptr->f_l_dam_non_native < UCHAR_MAX) f_l_ptr->f_l_dam_non_native++;
            }

            get_spell_type_from_feature(feat, &gf_type);

            /* Hack - quest monsters shouldn't take damage from terrain */
            if (m_ptr->mflag & (MFLAG_QUEST))
            {
                teleport_away(i, 2);
                continue;
            }

            /*Take damage*/
            (void)project_m(SOURCE_OTHER, m_ptr->fy, m_ptr->fx, f_ptr->dam_non_native, gf_type, 0L);

            /* Hack - if the monster isn't visible or in line-of-sight, move it to safety */
            if ((!m_ptr->ml) && (!m_ptr->project)) teleport_away(i, 2);
        }
    }
}


/*
 * Regenerate the monsters (once per 100 game turns)
 */
static void regen_monsters(void)
{
    int i, frac;

    int smooth = (p_ptr->game_turn / 100) % 100;

    /* Regenerate everyone */
    for (i = 1; i < mon_max; i++)
    {
        /* Check the i'th monster */
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /*
         * Hack -- in order to avoid a monster of 200 hitpoints having twice
         * the regeneration of one with 199, and because we shouldn't randomize
         * things (since we don't randomize character regeneration), we use
         * current turn to smooth things out.
         */

        /* Regenerate mana, if needed */
        if (m_ptr->mana != r_ptr->mana)
        {
            frac = (r_ptr->mana + smooth) / 100;

            /* Minimal regeneration rate */
            if (!frac) frac = 1;

            /* Regenerate */
            m_ptr->mana += frac;

            /* Do not over-regenerate */
            if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

            /* Fully healed -> flag minimum range for recalculation */
            if (m_ptr->mana == r_ptr->mana) m_ptr->min_range = 0;
        }

        /* Allow hp regeneration, if needed. */
        if (m_ptr->hp != m_ptr->maxhp)
        {
            frac = (m_ptr->maxhp + smooth) / 100;

            /* Some monsters regenerate quickly */
            if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;

            /* Minimal regeneration rate */
            if (!frac) frac = 1;

            /* Regenerate */
            m_ptr->hp += frac;

            /* Do not over-regenerate */
            if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

            /* Fully healed -> flag minimum range for recalculation */
            if (m_ptr->hp == m_ptr->maxhp) m_ptr->min_range = 0;
        }

    }
}

/*
 * "all" must be FALSE when only one object of a stack is recharged.
 */
static void recharged_notice(object_type *o_ptr, bool all)
{
    QString o_name;

    QString s;

    /* No notify necessary */
    if (!o_ptr->use_verify[RECHARGE_NOTIFY]) return;

    /* Describe (briefly) */
    o_name = object_desc(o_ptr, ODESC_BASE);

    /* Disturb the player */
    disturb(TRUE, FALSE);

    /* Notify the player */
    if (o_ptr->number > 1)
    {
        if (all)
        {
           message(QString("Your %1 are recharged.") .arg(o_name));
        }
        else
        {
           message(QString("One of your %1 has recharged.") .arg(o_name));
        }
    }

    /*artifacts*/
    else if (o_ptr->art_num)
    {
       message(QString("The %1 has recharged.") .arg(o_name));
    }

    /*single, non-artifact items*/
    else message(QString("Your %1 has recharged.") .arg(o_name));
}

static void process_mimics(void)
{
    s16b i;
    int dist;
    int obj_y, obj_x;
    int chance;

    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];
        monster_race *r_ptr;

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Only work with the mimic objects */
        if (!o_ptr->is_mimic()) continue;

        r_ptr = &r_info[o_ptr->mimic_r_idx];

        /* Determine object location */
        /* Held by a monster */
        if (o_ptr->held_m_idx)
        {
            monster_type *m_ptr;

            /* Get the monster */
            m_ptr = &mon_list[o_ptr->held_m_idx];

            /* Get the location */
            obj_y = m_ptr->fy;
            obj_x = m_ptr->fx;
        }
        /* On the ground */
        else
        {
            obj_y = o_ptr->iy;
            obj_x = o_ptr->ix;
        }

        /*
         * If the mimic can't cast, wait until the player is right next to it to come out of hiding
         * Hack - make an exception for creeping coins (so pits/nests are still dangerous)
         *
         */
        if ((!r_ptr->freq_ranged) && (o_ptr->tval != TV_GOLD))
        {
            if ((ABS(obj_y - p_ptr->py) <= 1)  && (ABS(obj_x - p_ptr->px) <= 1))
            {
                reveal_mimic(i, o_ptr->marked);
            }
            continue;
        }

        /* get the distance to player */
        dist = distance(obj_y, obj_x, p_ptr->py, p_ptr->px);

        /* Must be in line of fire from the player */
        if (!player_can_fire_bold(obj_y, obj_x)) continue;

        /* paranoia */
        if (dist > MAX_SIGHT) continue;

        /* Chance to be revealed gets bigger as the player gets closer */
        chance = (MAX_SIGHT - dist) * (MAX_SIGHT/4)  + 10;

        /* Reveal the mimic if test is passed*/
        if (randint0(MAX_SIGHT * 5) < chance)
        {
            reveal_mimic(i, o_ptr->marked);
        }
    }
}


/*
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory and on the ground.
 */
static void recharge_objects(void)
{
    int i;
    int j = 0;

    object_type *o_ptr;
    object_kind *k_ptr;

    /*** Recharge equipment ***/
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        /* Get the object */
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip the swap weapon */
        if ((birth_swap_weapons) && (i == INVEN_SWAP_WEAPON)) continue;

        /* Recharge activatable objects */
        if (o_ptr->timeout > 0 && !o_ptr->is_fuelable_lite())
        {
            /* Recharge */
            o_ptr->timeout--;

            /* Notice changes */
            if (!(o_ptr->timeout))
            {
                /* Update window */
                j++;

                /* Message if item is recharged, if inscribed !! */
                if (!(o_ptr->timeout)) recharged_notice(o_ptr, TRUE);
            }
        }
    }

    /* Notice changes */
    if (j)
    {
        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_EQUIPMENT);
    }

    /* Recharge rods */
    for (j = 0, i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];
        k_ptr = &k_info[o_ptr->k_idx];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Examine all charging rods or stacks of charging rods. */
        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
        {
            /* Determine how many rods are charging. */
            s16b temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

            if (temp > o_ptr->number) temp = o_ptr->number;

            /* Decrease timeout by that number. */
            o_ptr->timeout -= temp;

            /* Boundary control. */
            if (o_ptr->timeout < 0) o_ptr->timeout = 0;

            /* Update if any rods are recharged */
            if (temp > (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval)
            {
                /* Update window */
                j++;

            /* Message if whole stack is recharged, if inscribed !! */
            if (!(o_ptr->timeout)) recharged_notice(o_ptr, TRUE);

            /* Message if first rod in the stack is recharged, if inscribed !! */
            else if (temp == o_ptr->number) recharged_notice(o_ptr, FALSE);

            }

        }
    }

    /* Notice changes */
    if (j)
    {
        /* Combine pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY);
    }

    /*** Recharge the ground ***/
    for (j = 0, i = 1; i < o_max; i++)
    {
        /* Get the object */
        o_ptr = &o_list[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Examine all charging rods or stacks of charging rods. */
        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
        {
            s16b temp;

            k_ptr = &k_info[o_ptr->k_idx];

            /* Determine how many rods are charging. */
            temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

            if (temp > o_ptr->number) temp = o_ptr->number;

            /* Decrease timeout by that number. */
            o_ptr->timeout -= temp;

            /* Boundary control. */
            if (o_ptr->timeout < 0) o_ptr->timeout = 0;

            j++;

        }
    }

    /* Notice changes */
    if (j) p_ptr->redraw |= (PR_WIN_OBJLIST);

    /*re-charge rods and artifacts in the home*/
    for (i = 0; i < MAX_INVENTORY_HOME	; ++i)
    {
        store_type *st_ptr = &store[STORE_HOME];

        /* Object */
        o_ptr = &st_ptr->stock[i];
        k_ptr = &k_info[o_ptr->k_idx];

        /* Skip empty objects */
        if (!o_ptr->k_idx) continue;

        /* Examine all charging rods or stacks of charging rods. */
        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
        {
            /* Determine how many rods are charging. */
            s16b temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

            if (temp > o_ptr->number) temp = o_ptr->number;

            /* Decrease timeout by that number. */
            o_ptr->timeout -= temp;

            /* Boundary control. */
            if (o_ptr->timeout < 0) o_ptr->timeout = 0;

        }
        else if ((o_ptr->art_num) && (o_ptr->timeout))
        {
            /* Decrease timeout by that number. */
            o_ptr->timeout--;

            /* Boundary control, paranoia. */
            if (o_ptr->timeout < 0) o_ptr->timeout = 0;
        }
    }
}


static void play_ambient_sound(void)
{
    /* Town sound */
    if (p_ptr->depth == 0)
    {
        /* Hack - is it daytime or nighttime? */
        if (p_ptr->game_turn % (10L * TOWN_DAWN) < TOWN_DAWN / 2)
        {
            /* It's day. */
            sound(MSG_AMBIENT_DAY);
        }
        else
        {
            /* It's night. */
            sound(MSG_AMBIENT_NITE);
        }

    }

    /* Dungeon level 1-20 */
    else if (p_ptr->depth <= 20)
    {
        sound(MSG_AMBIENT_DNG1);
    }

    /* Dungeon level 21-40 */
    else if (p_ptr->depth <= 40)
    {
        sound(MSG_AMBIENT_DNG2);
    }

    /* Dungeon level 41-60 */
    else if (p_ptr->depth <= 60)
    {
        sound(MSG_AMBIENT_DNG3);
    }

    /* Dungeon level 61-80 */
    else if (p_ptr->depth <= 80)
    {
        sound(MSG_AMBIENT_DNG4);
    }

    /* Dungeon level 80- */
    else
    {
        sound(MSG_AMBIENT_DNG5);
    }
}


/*
 * This function randomly extinguished fires near the player location
 */
static void put_out_fires(void)
{
    u16b feat;
    int y1, y2;
    int x1, x2;

    /* Debug message */
    if (cheat_room)
    {
        color_message(QString("Putting out fires."), MSG_NOTICE);
        disturb(TRUE, TRUE);
    }

    /* Get the bottom-right corner of a rectangle centered on the player */
    y2 = MIN(p_ptr->cur_map_hgt - 2, p_ptr->py + MAX_SIGHT);
    x2 = MIN(p_ptr->cur_map_wid - 2, p_ptr->px + MAX_SIGHT);

    /* Traverse the rectangle */
    for (y1 = MAX(1, p_ptr->py - MAX_SIGHT); y1 <= y2; y1++)
    {
        for (x1 = MAX(1, p_ptr->px - MAX_SIGHT); x1 <= x2; x1++)
        {
            /* Get the feature */
            feat = dungeon_info[y1][x1].feature_idx;

            /* Must be in the line of fire (to avoid abuses) */
            if (!player_can_fire_bold(y1, x1)) continue;

            /* Must be a fire */
            if (!feat_ff3_match(feat, ELEMENT_FIRE)) continue;

            /* Must be sensitive to cold  */
            if (!feat_ff2_match(feat, FF2_HURT_COLD)) continue;

            /* Get the new feature */
            feat = feat_state(feat, FS_HURT_COLD);

            /* The fire is burning oil, ignore */
            if (feat_ff3_match(feat, ELEMENT_OIL)) continue;

            /* Randomness */
            if (!one_in_(20)) continue;

            /* Extinguish the fire */
            cave_set_feat(y1, x1, feat);
        }
    }

    /* Rescan the element flags of the level */
    update_level_flag();
}


/*
 * Helper for process_world -- decrement p_ptr->timed[] fields.
 */
static void decrease_timeouts(void)
{
    int adjust = (adj_con_fix[p_ptr->state.stat_index[A_CON]] + 1);
    int i;

    /* Decrement all effects that can be done simply */
    for (i = 0; i < TMD_MAX; i++)
    {
        int decr = 1;
        if (!p_ptr->timed[i])
            continue;

        switch (i)
        {
            case TMD_CUT:
            {
                /* Hack -- check for truly "mortal" wound */
                decr = (p_ptr->timed[i] > 1000) ? 0 : adjust;
                break;
            }

            case TMD_POISONED:
            case TMD_STUN:
            {
                decr = adjust;
                break;
            }
        }
        /* Decrement the effect */
        dec_timed(i, decr, FALSE);
    }

    return;
}

/*
 * Checks if multi-color monsters onscreen.
 */
void do_animation(void)
{
    if (!character_dungeon) return;

    // No animation with graphics
    if (ui_using_tiles()) return;

    for (int i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        if (!m_ptr->r_idx) continue;
        if (!m_ptr->ml) continue;
        if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

        dungeon_info[m_ptr->fy][m_ptr->fx].monster_color = m_ptr->m_color = add_preset_color(multi_hued_color(r_ptr));

        ui_redraw_grid(m_ptr->fy, m_ptr->fx);
    }

    if (use_graphics == GRAPHICS_PSEUDO) return;

    // Shimmer effects
    for (int i = 1; i < x_max; i++)
    {
        effect_type *x_ptr = &x_list[i];

        if (!x_ptr->x_type) continue;

        /* Ignore invisible effects */
        if (x_ptr->x_flags & (EF1_HIDDEN)) continue;

        /* Only certain effects are allowed */
        if ((x_ptr->x_type == EFFECT_TRAP_SMART) ||
            (x_ptr->x_type == EFFECT_GLACIER) ||
            (x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
            (x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
        {
            /* Redraw */
            light_spot(x_ptr->x_cur_y, x_ptr->x_cur_x);
        }
    }
}


/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
    int i;

    int regen_amount;

    object_type *o_ptr;


    /* We decrease noise slightly every game turn */
    total_wakeup_chance -= 400;

    /* But the character always makes some noise */
    if (total_wakeup_chance < p_ptr->base_wakeup_chance)
        total_wakeup_chance = p_ptr->base_wakeup_chance;

    /* Every 10 game turns */
    if (p_ptr->game_turn % 10) return;

    /*** Update quests ***/
    if (guild_quest_active())
    {
        quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

        /* Check for failure */
        if (guild_quest_level() != p_ptr->depth)
        {
            if (one_in_(20))
            {
                if (!(p_ptr->game_turn % QUEST_TURNS))
                {
                    if (quest_might_fail_now())
                    {
                        quest_fail();
                    }
                }
            }
        }
        /* We are on the quest level */
        else
        {
            if (q_ptr->q_type == QUEST_ARENA_LEVEL) 		process_arena_quest();
            else if (q_ptr->q_type == QUEST_LABYRINTH) 		process_labyrinth_quest();
            else if (q_ptr->q_type == QUEST_WILDERNESS)		process_wilderness_quest();
            else if (q_ptr->q_type == QUEST_GREATER_VAULT)	process_greater_vault_quest();
            else if (!(p_ptr->game_turn % QUEST_TURNS)) process_guild_quests();
        }
    }

    /* Play an ambient sound at regular intervals. */
    if (!(p_ptr->game_turn % ((10L * TOWN_DAWN) / 4)))
    {
        play_ambient_sound();
    }

    /*** Handle the "town" (stores and sunshine) ***/

    /* While in town */
    if (!p_ptr->depth)
    {
        /* Hack -- Daybreak/Nighfall in town */
        if (!(p_ptr->game_turn % ((10L * TOWN_DAWN) / 2)))
        {
            bool dawn;

            /* Check for dawn */
            dawn = (!(p_ptr->game_turn % (10L * TOWN_DAWN)));

            /* Day breaks */
            if (dawn)
            {
                /* Message */
                message(QString("The sun has risen."));
            }

            /* Night falls */
            else
            {
                /* Message */
                message(QString("The sun has fallen."));
            }

            /* Illuminate */
            town_illuminate(dawn);
        }
    }

    /* While in the dungeon */
    else
    {
        /*** Update the Stores ***/

        /* Update each store once a day (while in dungeon) */
        if (!(p_ptr->game_turn % (10L * STORE_TURNS)))
        {

            int n;

            /* Message */
            if (cheat_xtra)
            {
                message(QString("Updating Shops..."));
            }

            /* Maintain each shop (except home and guild) */
            for (n = 0; n < MAX_STORES; n++)
            {
                /* Skip the home */
                if (n == STORE_HOME) continue;
                if (n == STORE_GUILD)  continue;

                /* Maintain */
                store_maint(n);
            }

            /* Sometimes, shuffle the shop-keepers */
            if (one_in_(STORE_SHUFFLE))
            {

                /* Message */
                if (cheat_xtra)
                {
                    message(QString("Shuffling a Shopkeeper..."));
                }

                /* Pick a random shop (except home and guild) */
                while (1)
                {
                    n = rand_int(MAX_STORES);
                    if ((n != STORE_HOME) && (n != STORE_GUILD)) break;
                }

                /* Shuffle it */
                store_shuffle(n);
            }

            /* Message */
            if (cheat_xtra)
            {
                message(QString("Done."));
            }
        }
    }

    /*** Process the monsters ***/

    /* Check for creature generation */
    if (one_in_(MAX_M_ALLOC_CHANCE))
    {
        /*
         * Make a new monster where it is allowed
         */
        if ((*dun_cap->allow_level_repopulation)())
        {
            (void)alloc_monster(MAX_SIGHT + 5, (MPLACE_SLEEP | MPLACE_GROUP | MPLACE_NO_MIMIC | MPLACE_NO_GHOST));
        }
    }

    /* Occasionally have the ghost give a challenge */
    if (!(p_ptr->game_turn % 2500))
    {
        ghost_challenge(FALSE);
    }

    /* Put out fire if necessary */
    if ((level_flag & (LF1_FIRE)) && !(p_ptr->game_turn % 1000)) put_out_fires();

    /* Hack -- Check for terrain damage */
    monster_terrain_damage();

    /* Hack -- Check for creature regeneration */
    if (!(p_ptr->game_turn % 100)) regen_monsters();

    /* Process effects */
    process_effects();

    /* Process dynamic dungeon grids */
    process_dynamic_terrain();

    /* Show stacked monster messages */
    notice_stuff();

    /*** Damage over Time ***/

    /* Take damage from poison */
    if (p_ptr->timed[TMD_POISONED])
    {
        /* Take damage */
        if(!(p_ptr->state.immune_pois))take_hit(1, "poison");
    }

    /* Take damage from cuts */
    if (p_ptr->timed[TMD_CUT])
    {
        /* Mortal wound or Deep Gash */
        if (p_ptr->timed[TMD_CUT] > CUT_DEEP_GASH)
        {
            i = 3;
        }

        /* Severe cut */
        else if (p_ptr->timed[TMD_CUT] > CUT_SEVERE)
        {
            i = 2;
        }

        /* Other cuts */
        else
        {
            i = 1;
        }

        /* Take damage */
        take_hit(i, "a fatal wound");
    }

    /* Don't bother with the rest if the player is dead. */
    if (p_ptr->is_dead) return;

    /*** Check the Food, and Regenerate ***/

    /* Digest normally */
    if (p_ptr->food < PY_FOOD_MAX)
    {
        /* Every 100 game turns */
        if (!(p_ptr->game_turn % 100))
        {
            /* Basic digestion rate based on speed */
            i = calc_energy_gain(p_ptr->state.p_speed) * 2;

            /* Regeneration takes more food */
            if (p_ptr->state.regenerate) i += 30;

            /* Slow digestion takes less food */
            if (p_ptr->state.slow_digest) i -= 10;

            /* Minimal digestion */
            if (i < 1) i = 1;

            /* Digest some food */
            (void)set_food(p_ptr->food - i);
        }
    }

    /* Digest quickly when gorged */
    else
    {
        /* Digest a lot of food */
        (void)set_food(p_ptr->food - 100);
    }

    /* Starve to death (slowly) */
    if (p_ptr->food < PY_FOOD_STARVE)
    {
        /* Calculate damage */
        i = (PY_FOOD_STARVE - p_ptr->food) / 10;

        /* Take damage */
        take_hit(i, "starvation");
    }

    /* Default regeneration */
    regen_amount = PY_REGEN_NORMAL;

    /* Getting Weak */
    if (p_ptr->food < PY_FOOD_WEAK)
    {
        /* Lower regeneration */
        if (p_ptr->food < PY_FOOD_STARVE)
        {
            regen_amount = 0;
        }
        else if (p_ptr->food < PY_FOOD_FAINT)
        {
            regen_amount = PY_REGEN_FAINT;
        }
        else
        {
            regen_amount = PY_REGEN_WEAK;
        }

        /* Getting Faint */
        if (p_ptr->food < PY_FOOD_FAINT)
        {
            /* Faint occasionally */
            if (!p_ptr->timed[TMD_PARALYZED] && (rand_int(100) < 10))
            {
                /* Message */
                message(QString("You faint from the lack of food."));
                disturb(TRUE, TRUE);

                /* Hack -- faint (bypass free action) */
                (void)inc_timed(TMD_PARALYZED, 1 + rand_int(5), TRUE);
            }
        }
    }

    /* Regeneration ability */
    if (p_ptr->state.regenerate)
    {
        regen_amount = regen_amount * 2;
    }

    /* Searching or Resting */
    if (p_ptr->searching || p_ptr->is_resting())
    {
        regen_amount = regen_amount * 2;
    }

    /* Regenerate the mana */
    if (p_ptr->csp < p_ptr->msp)
    {
        regenmana(regen_amount);
    }

    /* Various things interfere with healing */
    if (p_ptr->timed[TMD_PARALYZED]) regen_amount = 0;
    if ((p_ptr->timed[TMD_POISONED]) && (!(p_ptr->state.immune_pois))) regen_amount = 0;
    if (p_ptr->timed[TMD_STUN]) regen_amount = 0;
    if (p_ptr->timed[TMD_CUT]) regen_amount = 0;

    /* Regenerate Hit Points if needed */
    if (p_ptr->chp < p_ptr->mhp)
    {
        regenhp(regen_amount);
    }

    /*** Timeout Various Things ***/

    decrease_timeouts();

    /* Warn about flying */
    if (p_ptr->timed[TMD_FLYING])
    {
        if ((p_ptr->timed[TMD_FLYING] <= 3) && (p_ptr->timed[TMD_FLYING] > 0) &&
            (!p_ptr->state.ffall || ((f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].dam_non_native > 0) &&
             !is_player_native(p_ptr->py, p_ptr->px))))
        {
            color_message(QString("You are about to stop flying."), MSG_LOSING_FLYING);

            disturb(FALSE, TRUE);

        }
    }

    /*Temporary Native Flags*/

    /* Native to Lava */
    if (p_ptr->timed[TMD_NAT_LAVA])
    {
        if ((p_ptr->timed[TMD_NAT_LAVA]) && (p_ptr->timed[TMD_NAT_LAVA] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_LAVA))
            {
                color_message(QString("You are about to lose nativity to lava."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);

            }
        }

    }

    /* Native to Oil */
    if (p_ptr->timed[TMD_NAT_OIL])
    {
        if ((p_ptr->timed[TMD_NAT_OIL]) && (p_ptr->timed[TMD_NAT_OIL] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_OIL))
            {
                color_message(QString("You are about to lose nativity to oil."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);

            }
        }
    }

    /* Native to Sand */
    if (p_ptr->timed[TMD_NAT_SAND])
    {
        if ((p_ptr->timed[TMD_NAT_SAND]) && (p_ptr->timed[TMD_NAT_SAND] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_SAND))
            {
                color_message(QString("You are about to lose nativity to sand."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);
            }
        }
    }

    /* Native to Forest */
    if (p_ptr->timed[TMD_NAT_TREE])
    {
        if ((p_ptr->timed[TMD_NAT_TREE]) && (p_ptr->timed[TMD_NAT_TREE] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_FOREST))
            {
                color_message(QString("You are about to lose nativity to forest."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);
            }
        }
    }

    /* Native to Water */
    if (p_ptr->timed[TMD_NAT_WATER])
    {
        if ((p_ptr->timed[TMD_NAT_WATER]) && (p_ptr->timed[TMD_NAT_WATER] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_WATER))
            {
                color_message(QString("You are about to lose nativity to water."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);
            }
        }
    }

    /* Native to Mud */
    if (p_ptr->timed[TMD_NAT_MUD])
    {
        if ((p_ptr->timed[TMD_NAT_MUD]) && (p_ptr->timed[TMD_NAT_MUD] < 5))
        {
            if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_MUD))
            {
                color_message(QString("You are about to lose nativity to mud."), MSG_LOSING_NATIVITY);

                disturb(FALSE, TRUE);
            }
        }
    }

    /* Animate trees if necessary */
    if (p_ptr->timed[TMD_CALL_HOURNS] > 0) call_huorns();

    /*** Process Light ***/

    /* Check for light being wielded */
    o_ptr = &inventory[INVEN_LIGHT];

    /* Burn some fuel in the current lite */
    if (o_ptr->tval == TV_LIGHT)
    {
        /* Hack -- Use some fuel (except on artifacts) */
        if (!o_ptr->is_artifact() && (o_ptr->timeout > 0))
        {
            /* Decrease life-span */
            o_ptr->timeout--;

            /* Hack -- notice interesting fuel steps */
            if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
            {
                /* Redraw stuff */
                p_ptr->redraw |= (PR_WIN_EQUIPMENT);
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
                disturb(FALSE, TRUE);
                message(QString("Your light has gone out!"));
            }

            /* The light is getting dim */
            else if ((o_ptr->timeout < 100) && (!(o_ptr->timeout % 10)))
            {
                disturb(FALSE, FALSE);
                message(QString("Your light is growing faint."));
            }
        }
    }


    /* Calculate torch radius */
    p_ptr->update |= (PU_TORCH);

    /*** Process Inventory ***/

    /* Handle experience draining */
    if (p_ptr->state.exp_drain)
    {
        if ((rand_int(100) < 10) && (p_ptr->exp > 0))
        {
            p_ptr->exp--;
            p_ptr->max_exp--;
            check_experience();
        }
    }

    /* Process mimic objects */
    process_mimics();

    /* Recharge activatable objects and rods */
    recharge_objects();

    /* Feel the inventory */
    sense_inventory();


    /*** Process Objects ***/

    /*** Involuntary Movement ***/

    /* Mega-Hack -- Random teleportation XXX XXX XXX */
    if ((p_ptr->state.teleport) && (rand_int(100) < 1))
    {
        /* Teleport player */
        teleport_player(40, FALSE);
    }

    /* Delayed Word-of-Recall */
    if (p_ptr->word_recall)
    {
        /* Count down towards recall */
        p_ptr->word_recall--;

        /* Activate the recall */
        if (!p_ptr->word_recall)
        {
            /* Disturbing! */
            disturb(TRUE, TRUE);

            /* Sound */
            sound(MSG_TPLEVEL);

            /* Determine the level */
            if (p_ptr->depth)
            {
                message(QString("You feel yourself yanked upwards!"));

                /* Go to the town. */
                dungeon_change_level(0);
            }
            else
            {
                /* New depth */
                int new_depth = p_ptr->recall_depth;
                if (new_depth < 1) new_depth = 1;

                message(QString("You feel yourself yanked downwards!"));

                dungeon_change_level(new_depth);
            }
        }
    }

    /* Delayed level feelings */
    if ((p_ptr->depth) && (!p_ptr->leaving_level) && (!do_feeling) && (!(p_ptr->game_turn % 100)))
    {
        int chance;

        /*players notice arena levels almost instantly */
        if (p_ptr->dungeon_type== DUNGEON_TYPE_ARENA) chance = 2;

        /*players notice wilderness and labyrinth levels almost as quickly */
        else if (p_ptr->dungeon_type== DUNGEON_TYPE_WILDERNESS) chance = 10;
        else if (p_ptr->dungeon_type== DUNGEON_TYPE_LABYRINTH) chance = 10;
        else if (p_ptr->dungeon_type== DUNGEON_TYPE_GREATER_VAULT) chance = 10;

        /* Players notice themed levels quickly as well */
        else if (p_ptr->dungeon_type >= DUNGEON_TYPE_THEMED_LEVEL) chance = 20;

        else chance = 40;

        /* After sufficient time, can learn about the level */
        if ((rand_int(chance) < p_ptr->state.skills[SKILL_SEARCH_CHANCE]) &&
            (rand_int(chance) < p_ptr->state.skills[SKILL_SEARCH_CHANCE]))
        {
            /* Now have a feeling */
            do_feeling = TRUE;

            /* Announce feeling */
            do_cmd_feeling();

            /* Update the level indicator */
            p_ptr->redraw |= (PR_SIDEBAR_PL);

            /* Disturb */
            disturb(FALSE, FALSE);
        }
    }

    /* Score gets adjusted every 100000 turns */
    if (!(p_ptr->game_turn % 100000)) p_ptr->update |= (PU_PLAYER_SCORE);

    /* Notice stuff */
    notice_stuff();
    redraw_stuff();
}

void change_player_level(void)
{
    /* Play ambient sound on change of level. */
    play_ambient_sound();

    /* Notice stuff */
    notice_stuff();

    /* Update stuff */
    update_stuff();

    /* Redraw stuff */
    redraw_stuff();

    /* Cancel the target */
    target_set_monster(0, FALSE);

    /* Cancel the health bar */
    health_track(0);

    /* Forget the view */
    forget_view();

    /* Handle "quit and save" */
    if (!p_ptr->playing && !p_ptr->is_dead) return;

    /* Erase the old cave */
    reset_dungeon_info();
    wipe_o_list();
    wipe_mon_list();
    wipe_x_list();
    count_feat_everseen();

    /* Reset player ghost info */
    player_ghost_num = -1;
    ghost_r_idx = 0;
    player_ghost_name.clear();

    /* Delete any pending monster message */
    mon_msg.clear();
    mon_message_hist.clear();

    /* Check for quest_failure */
    if (guild_quest_active())
    {
        if (quest_fail_immediately()) quest_fail();

        else if (quest_might_fail_now())
        {
            /* Have a chance to fail if the quest is in progress */
            if (one_in_(10)) quest_fail();
        }
    }

    /* Accidental Death */
    if (p_ptr->playing && p_ptr->is_dead)
    {
        /* Mega-Hack -- Allow player to cheat death */
        if ((p_ptr->is_wizard || cheat_live) && !get_check("Die? "))
        {
            if (cheat_live) p_ptr->is_wizard = TRUE;

            /* Mark social class, reset age, if needed */
            if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

            /* Increase age */
            p_ptr->age++;

            /* Message */
            message(QString("You invoke wizard mode and cheat death."));

            /* Cheat death */
            p_ptr->is_dead = FALSE;

            /* Restore hit points */
            p_ptr->chp = p_ptr->mhp;
            p_ptr->chp_frac = 0;

            /* Restore spell points */
            p_ptr->csp = p_ptr->msp;
            p_ptr->csp_frac = 0;

            /* Hack -- Healing */
            (void)(clear_timed(TMD_BLIND, TRUE));
            (void)clear_timed(TMD_CONFUSED, TRUE);
            (void)clear_timed(TMD_POISONED, TRUE);
            (void)clear_timed(TMD_AFRAID, TRUE);
            (void)clear_timed(TMD_PARALYZED, TRUE);
            (void)clear_timed(TMD_IMAGE, TRUE);
            (void)set_stun(0);
            (void)set_cut(0);

            /* Hack -- Prevent starvation */
            (void)set_food(PY_FOOD_MAX - 1);

            /* Hack -- cancel recall */
            if (p_ptr->word_recall)
            {
                /* Message */
                message(QString("A tension leaves the air around you..."));

                /* Hack -- Prevent recall */
                p_ptr->word_recall = 0;
            }

            /* Note cause of death XXX XXX XXX */
            p_ptr->died_from = QString("Cheating death");

            /* New depth */
            dungeon_change_level(0);

        }
    }

    /* Handle "death" */
    if (p_ptr->is_dead) return;

    /* Reset the monster generation level */
    monster_level = p_ptr->depth;

    /* Reset the object generation level */
    object_level = p_ptr->depth;

    /* Make a new level */
    generate_cave();

    int i;    

    /* Not leaving */
    p_ptr->leaving_level = FALSE;

    /* Reset the "command" vars */
    p_ptr->player_command_wipe();

    /* Cancel the target */
    target_set_monster(0, FALSE);

    /* Cancel the health bar */
    health_track(0);

    /* Reset repair flags */
    repair_mflag_show = TRUE;
    repair_mflag_mark = TRUE;

    /* Reset terrain damage */
    p_ptr->cumulative_terrain_damage = 0;

    /* Disturb */
    disturb(TRUE, TRUE);

    /* Track maximum player level */
    if (p_ptr->max_lev < p_ptr->lev)
    {
        p_ptr->max_lev = p_ptr->lev;
    }

    /* Track maximum dungeon level */
    if (p_ptr->max_depth < p_ptr->depth)
    {
        p_ptr->max_depth = p_ptr->depth;
        p_ptr->update |= (PU_PLAYER_SCORE);
    }

    /* Track maximum quest level */
    if ((p_ptr->max_depth > 1) &&
        (p_ptr->max_depth > p_ptr->quest_depth))
    {
        p_ptr->quest_depth = p_ptr->max_depth;
    }

    /* Track recall dungeon level */
    if (p_ptr->recall_depth < p_ptr->depth)
    {
        p_ptr->recall_depth = p_ptr->depth;
    }

    /* If autosave is pending, do it now. */
    if (p_ptr->autosave)
    {
        save_player();
        p_ptr->autosave = FALSE;
    }

    /* No stairs down from fixed or guardian quests */
    if (no_down_stairs(p_ptr->depth))
    {
        if ((p_ptr->create_stair == FEAT_STAIRS_DOWN) ||
            (p_ptr->create_stair == FEAT_SHAFT_DOWN))
             p_ptr->create_stair = FALSE;
    }

    /* No stairs from town or if not allowed */
    if (!p_ptr->depth)
    {
        p_ptr->create_stair = FALSE;
    }

    /* Make a staircase */
    if (p_ptr->create_stair)
    {
        /* Place a staircase */
        if (cave_valid_bold(p_ptr->py, p_ptr->px))
        {
            /* XXX XXX XXX */
            delete_object(p_ptr->py, p_ptr->px);

            cave_set_feat(p_ptr->py, p_ptr->px, p_ptr->create_stair);

            /* Mark the stairs as known */
            dungeon_info[p_ptr->py][p_ptr->px].mark_square();
        }

        /* Cancel the stair request */
        p_ptr->create_stair = FALSE;
    }

    character_xtra = TRUE;

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Calculate torch radius */
    p_ptr->update |= (PU_TORCH);

    /* RE-do the flow */
    p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    /* Update stuff */
    update_stuff();

    /* Fully update the visuals (and monster distances) */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

    /* Update stuff */
    update_stuff();

    character_xtra = FALSE;

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_PANEL);

    /* Combine / Reorder the pack */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    p_ptr->message_append_stop();

    /* Check quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        quest_type *q_ptr = &q_info[i];

        /* Already complete */
        if (is_quest_complete(i)) continue;

        /* No quest */
        if (!q_ptr->base_level) continue;

        /* Check for quest */
        if (q_ptr->base_level == p_ptr->depth)
        {
            q_info[i].q_flags |= (QFLAG_STARTED);

            p_ptr->redraw |= (PR_SIDEBAR_PL);
            break;
        }
    }

    /* Notice stuff */
    notice_stuff();

    /* Redraw stuff */
    ui_redraw_all();

    ui_center(p_ptr->py, p_ptr->px);

    /* Handle delayed death */
    if (p_ptr->is_dead) return;

    /* Announce (or repeat) the feeling */
    if (p_ptr->depth && (do_feeling)) do_cmd_feeling();

    /* Announce a player ghost challenge. -LM- */
    ghost_challenge(TRUE);
}


// Process game turns until it is the player's turn to move again, or the player is dead.
static void process_game_turns(void)
{

    /* Main loop */
    while (TRUE)
    {
        /* Count game turns */
        p_ptr->game_turn++;

        /* Hack -- Compact the monster list occasionally */
        if (mon_cnt + 32 > z_info->m_max) compact_monsters(64);

        /* Hack -- Compress the monster list occasionally */
        if (mon_cnt + 32 < mon_max) compact_monsters(0);

        /* Hack -- Compact the object list occasionally */
        if (o_cnt + 32 > z_info->o_max) compact_objects(64);

        /* Hack -- Compress the object list occasionally */
        else if (o_cnt + 32 < o_max) compact_objects(0);

        /* Do any necessary animations */
        do_animation();

        /* Update terrain damage every game turn */
        if ((!is_player_native(p_ptr->py, p_ptr->px)) && (!p_ptr->timed[TMD_FLYING]))
        {
            p_ptr->cumulative_terrain_damage += f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].dam_non_native;
        }

        /*** Process player & monsters, break when it is the player's turn to move ***/
        process_entities();

        /* Update stuff */
        update_stuff();

        /* Redraw stuff */
        redraw_stuff();

        /* Handle reasons to break the loop */
        if (p_ptr->is_dead)  return;
        if (p_ptr->leaving_level) change_player_level();

        /* Process the world */
        process_world();

        /* Update stuff */
        update_stuff();

        /* Redraw stuff */
        redraw_stuff();

        if (p_ptr->is_dead) return;
        if (p_ptr->leaving_level) change_player_level();

        /* Handle reasons to break the loop */
        if (p_ptr->player_turn &&
            !(p_ptr->timed[TMD_PARALYZED] ||
             (p_ptr->stun_status() == STUN_KNOCKED_OUT)))
        {
                return;
        }
    }
}

static void redraw_hallucination()
{
    QRect vis = visible_dungeon();

    for (int i = 0; i < vis.height(); i++)
    {
        for (int j = 0; j < vis.width(); j++)
        {
            int y = vis.y() + i;
            int x = vis.x() + j;
            int m_idx = dungeon_info[y][x].monster_idx;
            if (m_idx > 0 && mon_list[m_idx].ml)
            {
                light_spot(y, x);
                continue;
            }
            int o_idx = dungeon_info[y][x].object_idx;
            while (o_idx)
            {
                if (o_list[o_idx].marked)
                {
                    light_spot(y, x);
                    break;
                }
                o_idx = o_list[o_idx].next_o_idx;
            }
        }
    }
}

static int depth_counter = 0;
/*
 * the actual process player energy function.
 * Separated so that repeated commands and macros can
 * easily call this command multiple times in a loop.
 */
void process_player_energy_aux(byte energy_used)
{

    if (depth_counter > 0)
    {
        pop_up_message_box("process_player_energy is calling itself indirectly");
        return;
    }

    depth_counter = 1;

    int i;

    p_ptr->p_energy -= energy_used;

    /* Hack -- constant hallucination */
    if (p_ptr->timed[TMD_IMAGE])
    {
        redraw_hallucination();
    }

    /* Hack -- Redraw depth if the temporary quest notification ends */
    if ((quest_indicator_timer > 0) && (--quest_indicator_timer == 0))
    {
        quest_indicator_complete = FALSE;
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    /* Shimmer monsters if needed */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr;
        monster_race *r_ptr;

        /* Get the monster */
        m_ptr = &mon_list[i];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        // Visible monsters only
        if (!m_ptr->ml) continue;

        /* Get the monster race */
        r_ptr = &r_info[m_ptr->r_idx];

        /* Skip non-multi-hued monsters */
        if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

        /* Redraw regardless */
        light_spot(m_ptr->fy, m_ptr->fx);
    }

    /* Traverse effect array */
    for (i = 1; i < x_max; i++)
    {
        effect_type *x_ptr = &x_list[i];

        if (!x_ptr->x_type) continue;

        /* Ignore invisible effects */
        if (x_ptr->x_flags & (EF1_HIDDEN)) continue;

        /* Only certain effects are allowed */
        if ((x_ptr->x_type == EFFECT_TRAP_SMART) ||
            (x_ptr->x_type == EFFECT_GLACIER) ||
            (x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
            (x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
        {
            /* Redraw */
            light_spot(x_ptr->x_cur_y, x_ptr->x_cur_x);
        }
    }

    /* Redraw visual indicator of temporary element brand */
    if (p_ptr->timed[TMD_SLAY_ELEM]) p_ptr->redraw |= (PR_STATUSBAR);

    /* Repair "mark" flags */
    if (repair_mflag_mark)
    {
        /* Reset the flag */
        repair_mflag_mark = FALSE;

        /* Process the monsters */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr;

            /* Get the monster */
            m_ptr = &mon_list[i];

            /* Skip dead monsters */
            /* if (!m_ptr->r_idx) continue; */

            /* Repair "mark" flag */
            if (m_ptr->mflag & (MFLAG_MARK))
            {
                /* Skip "show" monsters */
                if (m_ptr->mflag & (MFLAG_SHOW))
                {
                    /* Repair "mark" flag */
                    repair_mflag_mark = TRUE;

                    /* Skip */
                    continue;
                }

                /* Forget flag */
                m_ptr->mflag &= ~(MFLAG_MARK);

                /* Update the monster */
                update_mon(i, FALSE);

                /* Hack -- Force redraw of hidden monsters */
                if ((m_ptr->mflag & (MFLAG_HIDE)) && m_ptr->ml)
                {
                    /* Redraw */
                    light_spot(m_ptr->fy, m_ptr->fx);
                }
            }
        }
    }

    /* Repair "show" flags */
    if (repair_mflag_show)
    {
        /* Reset the flag */
        repair_mflag_show = FALSE;

        /* Process the monsters */
        for (i = 1; i < mon_max; i++)
        {
            monster_type *m_ptr;

            /* Get the monster */
            m_ptr = &mon_list[i];

            /* Skip dead monsters */
            /* if (!m_ptr->r_idx) continue; */

            /* Clear "show" flag */
            m_ptr->mflag &= ~(MFLAG_SHOW);
        }
    }

    // process game turns until it is the player's turn again, or the player is dead;
    process_game_turns();

    depth_counter = 0;

    if (p_ptr->is_dead)
    {
        player_death_close_game();
    }
}

/*
 * This function should be called after every command that uses player energy.
 * This will be the main 'loop' for repeated commands, so this function
 * assumes it is the final line of code in the function that calls it.
 */
void process_player_energy(byte energy_used)
{
    p_ptr->message_append_stop();

    process_player_energy_aux(energy_used);

    //Handle repeated commands
    if (!p_ptr->command_current) return;

    command_type *command_ptr = &command_info[p_ptr->command_current];

    /*
     * If resting, check if we should stop.
     */
    if (p_ptr->is_resting())
    {
        if (p_ptr->should_stop_resting())
        {
            disturb(FALSE, FALSE);
            p_ptr->redraw |= (PR_SIDEBAR_PL);
            return;
        }
    }

    // Check if we are done with the command
    if (command_ptr->repeated_command_completed())
    {
        p_ptr->player_command_wipe();
    }

    else
    {
        // Run the command, reduce repeat command count
        if (p_ptr->player_args.repeats) p_ptr->player_args.repeats--;
        command_ptr->command_function(p_ptr->player_args);
    }

    /* Redraw the state */
    p_ptr->redraw |= (PR_STATUSBAR);
}
