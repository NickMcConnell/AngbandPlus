/* File: was melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"

/*
 * Determine if there is a space near the the selected spot in which
 * a summoned creature can appear
 */
static int summon_possible(int y1, int x1)
{
    int y, x;
    int num_clear=0;

    /* Start at the location, and check 2 grids in each dir */
    for (y = y1 - 2; y <= y1 + 2; y++)
    {
        for (x = x1 - 2; x <= x1 + 2; x++)
        {
            /* Ignore illegal locations */
            if (!in_bounds(y, x)) continue;

            /* Only check a circular area */
            if (distance(y1, x1, y, x) > 2) continue;

            /* Hack: no summon on glyph of warding */
            if (f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_GLYPH)) continue;

            /* Require empty floor grid in line of sight */
            if (cave_empty_bold(y, x) && los(y1, x1, y, x))
            {
                num_clear++;
            }
        }
    }

    return (num_clear);
}


/*
 * Fully update monster's knowledge of the player.
 * Used by player ghost (and all monsters with smart_cheat).
 */
static void update_smart_cheat(int m_idx)
{
    monster_type *m_ptr = &mon_list[m_idx];

    /* Know weirdness */
    if (p_ptr->state.free_act) m_ptr->smart |= (SM_IMM_FREE);
    if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
    if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
    if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);

    /* Know immunities */
    if (p_ptr->state.immune_acid) m_ptr->smart |= (SM_IMM_ACID);
    if (p_ptr->state.immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
    if (p_ptr->state.immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
    if (p_ptr->state.immune_cold) m_ptr->smart |= (SM_IMM_COLD);
    if (p_ptr->state.immune_pois) m_ptr->smart |= (SM_IMM_POIS);

    /* Know oppositions */
    if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
    if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
    if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
    if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
    if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);

    /* Know resistances */
    if (p_ptr->state.resist_acid) m_ptr->smart |= (SM_RES_ACID);
    if (p_ptr->state.resist_elec) m_ptr->smart |= (SM_RES_ELEC);
    if (p_ptr->state.resist_fire) m_ptr->smart |= (SM_RES_FIRE);
    if (p_ptr->state.resist_cold) m_ptr->smart |= (SM_RES_COLD);
    if (p_ptr->state.resist_pois) m_ptr->smart |= (SM_RES_POIS);
    if (p_ptr->state.resist_fear) m_ptr->smart |= (SM_RES_FEAR);
    if (p_ptr->state.resist_light) m_ptr->smart |= (SM_RES_LIGHT);
    if (p_ptr->state.resist_dark) m_ptr->smart |= (SM_RES_DARK);
    if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
    if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
    if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
    if (p_ptr->state.resist_shard) m_ptr->smart |= (SM_RES_SHARD);
    if (p_ptr->state.resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
    if (p_ptr->state.resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
    if (p_ptr->state.resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
    if (p_ptr->state.resist_disen) m_ptr->smart |= (SM_RES_DISEN);

    return;
}

/*
 * Used to determine the player's known level of resistance to a
 * particular spell.
 *
 * The LRN_xxx constant determines what type of resistance is
 * applicable.  The monster's SM_xxx flags, as well as player
 * conditions, are referred to as needed.
 * -BR-
 */
static int find_resist(int m_idx, int spell_lrn)
{
    monster_type *m_ptr = &mon_list[m_idx];

    int a;
    u32b smart;

    /* Nothing Known */
    if (!m_ptr->smart) return (0);

    /* get smart flags */
    smart = m_ptr->smart;

    /* Which spell */
    switch (spell_lrn)
    {
        /* Spells 'resisted' by AC, Dex, etc.
         * Currently no assessment is made */
        case LRN_ARCH:
        {
            return (0);
        }
        /* As above, but poisonous. */
        case LRN_PARCH:
        {
            if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (10);
            else return (0);
        }
        /* Acid Spells */
        case LRN_ACID:
        {
            if (smart & (SM_IMM_ACID)) return (100);
            else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID))) return (70);
            else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID))) return (40);
            else return (0);
        }
        /* Lightning Spells */
        case LRN_ELEC:
        {
            if (smart & (SM_IMM_ELEC)) return (100);
            else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) return (70);
            else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) return (40);
            else return (0);
        }
        /* Fire Spells */
        case LRN_FIRE:
        {
            if (smart & (SM_IMM_FIRE)) return (100);
            else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE))) return (70);
            else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE))) return (40);
            else return (0);
        }
        /* Cold Spells */
        case LRN_COLD:
        {
            if (smart & (SM_IMM_COLD)) return (100);
            else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) return (70);
            else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) return (40);
            else return (0);
        }
        /* Ice Spells */
        case LRN_ICE:
        {
            if (smart & (SM_IMM_COLD)) a=90;
            else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) a = 60;
            else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) a = 30;
            else a = 0;
            if (smart & (SM_RES_SOUND)) a += 5;
            if (smart & (SM_RES_SHARD)) a += 5;
            return (a);
        }
        /* Poison Spells */
        case LRN_POIS:
        {
            if (smart & (SM_IMM_POIS)) return (100);
            else if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS))) return (80);
            else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (55);
            else return (0);
        }
        /* Plasma Spells */
        case LRN_PLAS:
        {
            if (smart & (SM_RES_SHARD)) return (15);
            else return (0);
        }
        /* Light Spells */
        case LRN_LIGHT:
        {
            if (smart & (SM_RES_LIGHT)) return (30);
            else return (0);
        }
        /* Darkness Spells */
        case LRN_DARK:
        {
            if (smart & (SM_RES_DARK)) return (30);
            else return (0);
        }
        /* Confusion Spells, damage dealing */
        case LRN_CONFU:
        {
            if (smart & (SM_RES_CONFU)) return (30);
            else return (0);
        }
        /* Sound Spells */
        case LRN_SOUND:
        {
            a=0;
            if (smart & (SM_RES_SOUND)) a += 30;
            if (smart & (SM_RES_CONFU)) a += 10;
            if (smart & (SM_PERF_SAVE)) a += 10;
            else if (smart & (SM_GOOD_SAVE)) a += 5;
            return (a);
        }
        /* Irresistible, but sound prevents stun */
        case LRN_SOUND2:
        {
            if (smart & (SM_RES_SOUND)) return (5);
            else return (0);
        }
        /* Shards Spells */
        case LRN_SHARD:
        {
            if (smart & (SM_RES_SHARD)) return (30);
            else return (0);
        }
        /* Nexus Spells */
        case LRN_NEXUS:
        {
            if (smart & (SM_RES_NEXUS)) return (30);
            else return (0);
        }
        /* Nether Spells */
        case LRN_NETHR:
        {
            if (smart & (SM_RES_NETHR)) return (30);
            else return (0);
        }
        /* Nether Spells */
        case LRN_LAVA:
        {
            if (smart & (SM_NAT_LAVA)) return (70);
            else return (0);
        }
        /* Chaos Spells */
        case LRN_CHAOS:
        {
            a = 0;
            if (smart & (SM_RES_CHAOS)) return(30);
            if (smart & (SM_RES_NETHR))  a += 10;
            if (smart & (SM_RES_CONFU))  a += 10;
            return (a);
        }
        /* Disenchantment Spells */
        case LRN_DISEN:
        {
            if (smart & (SM_RES_DISEN)) return (30);
            else return (0);
        }
        /* Storm Spells */
        case LRN_STORM:
        {
            a=0;
            if (smart & (SM_IMM_ELEC)) a += 15;
            else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) a += 10;
            else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) a += 5;
            if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)) ||(smart & (SM_IMM_COLD))) a += 5;
            if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)) ||(smart & (SM_IMM_ACID))) a += 5;
            if (smart & (SM_RES_CONFU)) a += 10;
            return (a);
        }
        /* Water Spells */
        case LRN_WATER:
        {
            a=0;
            if (smart & (SM_RES_CONFU)) a += 10;
            if (smart & (SM_RES_SOUND)) a += 5;
            return (a);
        }
        /* Spells that attack player mana */
        case LRN_MANA:
        {
            if (smart & (SM_IMM_MANA)) return (100);
            else return (0);
        }
        /* Spells Requiring Save or Resist Nexus */
        case LRN_NEXUS_SAVE:
        {
            if (smart & (SM_RES_NEXUS)) return (100);
            else if (smart & (SM_PERF_SAVE)) return (100);
            else if (smart & (SM_GOOD_SAVE)) return (30);
            else return (0);
        }
        /* Spells Requiring Save or Resist Fear */
        case LRN_FEAR_SAVE:
        {
            a = 0;
            if (smart & (SM_RES_FEAR)) a = 100;
            else if (smart & (SM_PERF_SAVE)) a = 100;
            else
            {
                if (smart & (SM_GOOD_SAVE)) a += 30;
                if (p_ptr->timed[TMD_AFRAID]) a += 50;
            }
            return (a);
        }
        /* Spells Requiring Save or Resist Blindness */
        case LRN_BLIND_SAVE:
        {
            a = 0;
            if (smart & (SM_RES_BLIND)) a = 100;
            else if (smart & (SM_PERF_SAVE)) a = 100;
            else
            {
                if (smart & (SM_GOOD_SAVE)) a += 30;
                if (p_ptr->timed[TMD_BLIND]) a += 50;
            }
            return (a);
        }
        /* Spells Requiring Save or Resist Confusion */
        case LRN_CONFU_SAVE:
        {
            a = 0;
            if (smart & (SM_RES_CONFU)) a = 100;
            else if (smart & (SM_PERF_SAVE)) a = 100;
            else
            {
                if (smart & (SM_GOOD_SAVE)) a += 30;
                if (p_ptr->timed[TMD_CONFUSED]) a += 50;
            }
            return (a);
        }
        /* Spells Requiring Save or Free Action */
        case LRN_FREE_SAVE:
        {
            a = 0;
            if (smart & (SM_IMM_FREE)) a = 100;
            else if (smart & (SM_PERF_SAVE)) a = 100;
            else if (p_ptr->timed[TMD_PARALYZED]) a = 80;
            else
            {
                if (smart & (SM_GOOD_SAVE)) a += 30;
                if (p_ptr->timed[TMD_SLOW]) a += 50;
            }
            return (a);
        }

        /* Spells Requiring Save  */
        case LRN_SAVE:
        {
            if (smart & (SM_PERF_SAVE)) return (100);
            else if (smart & (SM_GOOD_SAVE)) return (30);
            else return (0);
        }

        /* Spells Requiring Darkness/Save */
        case LRN_DARK_SAVE:
        {
            a = 0;

            if (smart & (SM_RES_DARK)) a += 25;

            if (smart & (SM_PERF_SAVE)) a += 25;
            else if (smart & (SM_GOOD_SAVE)) a += 15;
            return (a);
        }

        /* Anything else */
        default:
        {
            return (0);
        }
    }
}

/*
 * Used to exclude spells which are too expensive for the
 * monster to cast.  Excludes all spells that cost more than the
 * current available mana.
 *
 * Smart monsters may also exclude spells that use a lot of mana,
 * even if they have enough.
 *
 * -BR-
 */
static void remove_expensive_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p)
{
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int i, max_cost;

    u32b f4 = (*f4p);
    u32b f5 = (*f5p);
    u32b f6 = (*f6p);
    u32b f7 = (*f7p);

    /* Determine maximum amount of mana to be spent */
    /* Smart monsters will usually not blow all their mana on one spell.
     */
    if (r_ptr->flags2 & (RF2_SMART))
        max_cost = (m_ptr->mana * (rand_range(4, 6))) / 6;

    /* Otherwise spend up to the full current mana */
    else max_cost = m_ptr->mana;

    /* check innate spells for mana available */
    for (i = 0; i < 32; i++)
    {
        if (spell_info_RF4[i][COL_SPELL_MANA_COST] > max_cost) f4 &= ~(0x00000001 << i);
    }

    /* check normal spells for mana available */
    for (i = 0; i < 32; i++)
    {
        if (spell_info_RF5[i][COL_SPELL_MANA_COST] > max_cost) f5 &= ~(0x00000001 << i);
    }

    /* check other spells for mana available */
    for (i = 0; i < 32; i++)
    {
        if (spell_info_RF6[i][COL_SPELL_MANA_COST] > max_cost) f6 &= ~(0x00000001 << i);
    }

    /* check other spells for mana available */
    for (i = 0; i < 32; i++)
    {
        if (spell_info_RF7[i][COL_SPELL_MANA_COST] > max_cost) f7 &= ~(0x00000001 << i);
    }

    /* Modify the spell list. */
    (*f4p) = f4;
    (*f5p) = f5;
    (*f6p) = f6;
    (*f7p) = f7;

}


/*
 * Intelligent monsters use this function to filter away spells
 * which have no benefit.
 */
static void remove_useless_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool require_los)
{
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    u32b f4 = (*f4p);
    u32b f5 = (*f5p);
    u32b f6 = (*f6p);
    u32b f7 = (*f7p);

    /* Don't regain mana if full */
    if (m_ptr->mana >= r_ptr->mana) f6 &= ~(RF6_ADD_MANA);

    /* Don't heal if full */
    if (m_ptr->hp >= m_ptr->maxhp) f6 &= ~(RF6_HEAL);

    /* Don't Haste if Hasted */
    if (m_ptr->m_timed[MON_TMD_FAST] > 10) f6 &= ~(RF6_HASTE);

    /* Don't cure if not needed */
    if (!((m_ptr->m_timed[MON_TMD_STUN]) ||(m_ptr->m_timed[MON_TMD_FEAR]) ||
          (m_ptr->m_timed[MON_TMD_SLOW])))	f6 &= ~(RF6_CURE);

    /* Don't jump in already close, or don't want to be close */
    if (!(m_ptr->cdis > m_ptr->best_range) && require_los)
        f6 &= ~(RF6_TELE_SELF_TO);

    if (m_ptr->min_range > 5) f6 &= ~(RF6_TELE_SELF_TO);

    /* Rarely teleport to if too far or close */
    if ((m_ptr->cdis == 1) && (!one_in_(3))) f6 &= ~(RF6_TELE_TO);

    /* Modify the spell list. */
    (*f4p) = f4;
    (*f5p) = f5;
    (*f6p) = f6;
    (*f7p) = f7;
}



/*
 * Count the number of castable spells.
 *
 * If exactly 1 spell is available cast it.  If more than more is
 * available, and the random bit is set, pick one.
 *
 * Used as a short cut in 'choose_attack_spell' to circumvent AI
 * when there is only 1 choice. (random=FALSE)
 *
 * Also used in 'choose_attack_spell' to circumvent AI when
 * casting randomly (random=TRUE), as with dumb monsters.
 */
static int choose_attack_spell_fast(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool do_random)
{
    int i, num=0;
    byte spells[128];

    u32b f4 = (*f4p);
    u32b f5 = (*f5p);
    u32b f6 = (*f6p);
    u32b f7 = (*f7p);

    /* Extract the "innate" spells */
    for (i = 0; i < 32; i++)
    {
        if (f4 & (1L << i)) spells[num++] = i + 32 * 3;
    }

    /* Extract the "attack" spells */
    for (i = 0; i < 32; i++)
    {
        if (f5 & (1L << i)) spells[num++] = i + 32 * 4;
    }

    /* Extract the "miscellaneous" spells */
    for (i = 0; i < 32; i++)
    {
        if (f6 & (1L << i)) spells[num++] = i + 32 * 5;
    }

    /* Extract the "summon" spells */
    for (i = 0; i < 32; i++)
    {
        if (f7 & (1L << i)) spells[num++] = i + 32 * 6;
    }

    /* Paranoia */
    if (num == 0) return (0);

    /* Go quick if possible */
    if (num == 1)
    {
        /* Hack - Don't cast if known to be immune, unless
         * casting randomly anyway.  */
        if (!(do_random))
        {
            if (spells[0] < 128)
            {
                if (find_resist(m_idx, spell_desire_RF4[spells[0]-96][D_RES]) == 100) return (0);
            }
            else if (spells[0] < 160)
            {
                if (find_resist(m_idx, spell_desire_RF5[spells[0]-128][D_RES]) == 100) return (0);
            }
            else if (spells[0] < 192)
            {
                if (find_resist(m_idx, spell_desire_RF6[spells[0]-160][D_RES]) == 100) return (0);
            }
            else
            {
                if (find_resist(m_idx, spell_desire_RF7[spells[0]-192][D_RES]) == 100) return (0);
            }
        }

        /* Otherwise cast the one spell */
        else return (spells[0]);
    }

    /*
     * If we aren't allowed to choose at random
     * and we have multiple spells left, give up on quick
     * selection
     */
    if (!(do_random)) return (0);

    /* Pick at random */
    return (spells[rand_int(num)]);
}

/*
 * Have a monster choose a spell.
 *
 * Monster at m_idx uses this function to select a legal attack spell.
 * Spell casting AI is based here.
 *
 * First the code will try to save time by seeing if
 * choose_attack_spell_fast is helpful.  Otherwise, various AI
 * parameters are used to calculate a 'desirability' for each spell.
 * There is some randomness.  The most desirable spell is cast.
 *
 * archery_only can be used to restrict us to arrow/boulder type attacks.
 *
 * Returns the spell number, of '0' if no spell is selected.
 *
 *-BR-
 */
int choose_ranged_attack(int m_idx, int *tar_y, int *tar_x)
{
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int fy = m_ptr->fy;
    int fx = m_ptr->fx;

    byte *spell_desire;

    u32b f4, f5, f6, f7;

    byte spell_range;

    bool do_random = FALSE;

    bool require_los = TRUE;
    bool monster_blocking = FALSE;
    bool is_breath = FALSE;

    int i;
    int breath_hp, breath_maxhp, path, spaces;

    int want_hps=0, want_escape=0, want_mana=0, want_summon=0;
    int want_tactic=0, cur_range=0;

    int best_spell=0, best_spell_rating=0;
    int cur_spell_rating;

    /* Get the monster name (or "it") */
    QString m_name = monster_desc(m_ptr, 0x00);

    /* Extract the racial spell flags */
    f4 = r_ptr->flags4;
    f5 = r_ptr->flags5;
    f6 = r_ptr->flags6;
    f7 = r_ptr->flags7;

    /*default: target the player*/
    *tar_y = p_ptr->py;
    *tar_x = p_ptr->px;

    /*hack - some spells are pointless or unfair on arena levels*/
    if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA)
    {
        f6 &= ~(RF6_TELE_LEVEL | RF6_TELE_AWAY);
        f7 &= ~(dungeon_summon_mask_f7);
    }
    /*hack - some spells are pointless or unfair on labyrinth levels*/
    else if (p_ptr->dungeon_type == DUNGEON_TYPE_LABYRINTH)
    {
        f6 &= ~(RF6_TELE_LEVEL);
        f7 &= ~(dungeon_summon_mask_f7);
    }
    /*hack - some spells are unfair on themed levels */
    else if (p_ptr->dungeon_type == DUNGEON_TYPE_THEMED_LEVEL)
    {
        f6 &= ~(RF6_TELE_TO | RF6_TELE_LEVEL | RF6_TELE_AWAY);
        f7 &= ~(dungeon_summon_mask_f7);
    }
    /*hack - some spells are unfair on wilderness levels */
    else if (p_ptr->dungeon_type == DUNGEON_TYPE_WILDERNESS)
    {
        f6 &= ~(RF6_TELE_LEVEL);
        f7 &= ~(dungeon_summon_mask_f7);
    }
    /*hack - some spells are unfair on greater_vault levels*/
    else if (p_ptr->dungeon_type >= DUNGEON_TYPE_GREATER_VAULT)
    {
        f6 &= ~(RF6_TELE_LEVEL);
        f7 &= ~(dungeon_summon_mask_f7);
    }


    /* Check what kinds of spells can hit player */
    path = projectable(fy, fx, p_ptr->py, p_ptr->px, PROJECT_CHCK);

    /* do we have the player in sight at all? */
    if (path == PROJECT_NO)
    {
        bool clear_ball_spell = TRUE;

        /* Note if LOS is blocked by a monster instead of a wall */
        if (projectable(fy, fx, p_ptr->py, p_ptr->px, PROJECT_NONE))
        {
            clear_ball_spell = FALSE;
        monster_blocking = TRUE;
        }

        /*are we in range (and not stupid), and have access to ball spells?*/
        else if ((m_ptr->cdis < MAX_RANGE) && (!(r_ptr->flags2 & (RF2_STUPID))) &&
             ((r_ptr->flags4 & (RF4_BALL_MASK)) ||
              (r_ptr->flags5 & (RF5_BALL_MASK)) ||
              (r_ptr->flags6 & (RF6_BALL_MASK)) ||
              (r_ptr->flags7 & (RF7_BALL_MASK))))
        {

            int alt_y, alt_x, alt_path, best_y, best_x, best_path;

            /*start with no alternate shot*/
            best_y =  best_x = best_path  = 0;

            /* Check for impassable terrain */
            for (i = 0; i < 8; i++)
            {
                alt_y = p_ptr->py + ddy_ddd[i];
                alt_x = p_ptr->px + ddx_ddd[i];

                alt_path = projectable(m_ptr->fy, m_ptr->fx, alt_y, alt_x, PROJECT_CHCK);

                if (alt_path == PROJECT_NO) continue;

                if (alt_path == PROJECT_NOT_CLEAR)
                {
                    /*we already have a NOT_CLEAR path*/
                    if ((best_path == PROJECT_NOT_CLEAR) && (one_in_(2))) continue;
                }

                /*
                 * PROJECT_CLEAR, or monster has an
                 * empty square or a square with a safe monster
                 *  to lob a ball spell at player
                 */
                best_y = alt_y;
                best_x = alt_x;
                best_path = alt_path;
                /*we want to keep ball spells*/
                clear_ball_spell = FALSE;

                if (best_path == PROJECT_CLEAR) break;
            }

            if (best_y + best_x > 0)
            {
                /*default: target the player*/
                *tar_y = best_y;
                *tar_x = best_x;
            }
        }

        /* Don't allow breathing if player is not in a projectable path */
        if (!monster_blocking)
        {
            if (game_mode != GAME_NPPMORIA)
            {
                f4 &= ~(RF4_BREATH_MASK);
                f5 &= ~(RF5_BREATH_MASK);
                f6 &= ~(RF6_BREATH_MASK);
                f7 &= ~(RF7_BREATH_MASK);
            }
            require_los = FALSE;
        }

        /*We don't have a reason to try a ball spell*/
        if (clear_ball_spell)
        {
            f4 &= ~(RF4_BALL_MASK);
            f5 &= ~(RF5_BALL_MASK);
            f6 &= ~(RF6_BALL_MASK);
            f7 &= ~(RF7_BALL_MASK);
        }
    }

    /* Remove spells the 'no-brainers'*/
    /* Spells that require LOS */
    if ((!require_los) || (m_ptr->cdis > MAX_RANGE))
    {
        /*(ball spells would have been filtered out above if not usable*/
        f4 &= (RF4_NO_PLAYER_MASK | RF4_BALL_MASK);
        f5 &= (RF5_NO_PLAYER_MASK | RF5_BALL_MASK);
        f6 &= (RF6_NO_PLAYER_MASK | RF6_BALL_MASK);
        f7 &= (RF7_NO_PLAYER_MASK | RF7_BALL_MASK);
    }

    /*remove bolts and archery shots*/
    else if ((path == PROJECT_NOT_CLEAR) || (monster_blocking))
    {
        f4 &= ~(RF4_BOLT_MASK);
        f4 &= ~(RF4_ARCHERY_MASK);
        f5 &= ~(RF5_BOLT_MASK);
        f5 &= ~(RF5_ARCHERY_MASK);
        f6 &= ~(RF6_BOLT_MASK);
        f6 &= ~(RF6_ARCHERY_MASK);
        f7 &= ~(RF7_BOLT_MASK);
        f7 &= ~(RF7_ARCHERY_MASK);
    }

    /*
     * Flat out 75% chance of not casting if the player is not in sight
     * In addition, most spells don't work without a player around
     */
    if ((path == PROJECT_NO) && (!monster_blocking))
    {
        if (!one_in_(4)) return (0);
    }

    /* No spells left */
    if (!f4 && !f5 && !f6 && !f7) return (0);

    /* Spells we can not afford */
    remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

    /* Don't lash if too far or close */
    if ((m_ptr->cdis > 3) || (m_ptr->cdis < 2)) f4 &= ~(RF4_LASH);

    /* No spells left */
    if (!f4 && !f5 && !f6 && !f7) return (0);

    /* Stupid monsters choose at random. */
    if (r_ptr->flags2 & (RF2_STUPID)) return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));

    /* Remove spells that have no benefit
     * Does not include the effects of player resists/immunities */
    remove_useless_spells(m_idx, &f4, &f5, &f6, &f7, require_los);

    /* No spells left */
    if (!f4 && !f5 && !f6 && !f7) return (0);

    /* Sometimes non-dumb monsters cast randomly (though from the
     * restricted list)
     */
    if ((r_ptr->flags2 & (RF2_SMART)) && (one_in_(10))) do_random = TRUE;
    if ((!(r_ptr->flags2 & (RF2_SMART))) && (one_in_(5))) do_random = TRUE;

    /* Try 'fast' selection first.
     * If there is only one spell, choose that spell.
     * If there are multiple spells, choose one randomly if the 'random' flag is set.
     * Otherwise fail, and let the AI choose.
     */
    best_spell = choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, do_random);
    if (best_spell) return (best_spell);

    /* If we get this far, we are using the full-up AI.  Calculate
       some parameters. */

    /* Figure out if we are hurt */
    if (m_ptr->hp < m_ptr->maxhp/8) want_hps += 5;
    else if (m_ptr->hp < m_ptr->maxhp/5) want_hps += 3;
    else if (m_ptr->hp < m_ptr->maxhp/4) want_hps += 2;
    else if (m_ptr->hp < m_ptr->maxhp/2) want_hps++;
    else if (m_ptr->hp == m_ptr->maxhp) f6 &= ~(RF6_HEAL);

    /* Figure out if we want mana */
    if (m_ptr->mana < r_ptr->mana/4) want_mana +=2;
    else if (m_ptr->mana < r_ptr->mana/2) want_mana++;
    else if (m_ptr->mana == m_ptr->mana) f6 &= ~(RF6_ADD_MANA);

    /* Figure out if we want to scram */
    if (want_hps) want_escape = want_hps - 1;
    if (m_ptr->min_range == FLEE_RANGE) want_escape++;

    /* Desire to keep minimum distance */
    if (m_ptr->cdis < m_ptr->min_range)
        want_tactic += (m_ptr->min_range - m_ptr->cdis + 1) / 2;
    if (want_tactic > 3) want_tactic=3;

    /* Check terrain for purposes of summoning spells */
    spaces = summon_possible(m_ptr->fy, m_ptr->fx);
    if (spaces > 10) want_summon=3;
    else if (spaces > 3) want_summon=2;
    else if (spaces > 0) want_summon=1;
    else /*no spaces to summon*/
    {
        f4 &= ~(RF4_SUMMON_MASK);
        f5 &= ~(RF5_SUMMON_MASK);
        f6 &= ~(RF6_SUMMON_MASK);
        f7 &= ~(RF7_SUMMON_MASK);

    }

    /* Check if no spells left */
    if (!f4 && !f5 && !f6 && !f7) return (0);

    /* Find monster properties; Add an offset so that things are OK near zero */
    breath_hp = (m_ptr->hp > 2000 ? m_ptr->hp : 2000);
    breath_maxhp = (m_ptr->maxhp > 2000 ? m_ptr->maxhp : 2000);

    /* Cheat if requested, or if a player ghost. */
    if ((smart_cheat) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
    {
        update_smart_cheat(m_idx);
    }

    /* The conditionals are written for speed rather than readability
     * They should probably stay that way. */
    for (i = 0; i < 128; i++)
    {
        /* Do we even have this spell? */
        if (i < 32)
        {
            if (!(f4 &(1L <<  i    ))) continue;
            spell_desire=&spell_desire_RF4[i][0];
            spell_range = spell_info_RF4[i][COL_SPELL_BEST_RANGE];
            if (RF4_BREATH_MASK &(1L << (i   ))) is_breath=TRUE;
            else is_breath=FALSE;
        }
        else if (i < 64)
        {
            if (!(f5 &(1L << (i-32)))) continue;
            spell_desire=&spell_desire_RF5[i-32][0];
            spell_range=spell_info_RF5[i-32][COL_SPELL_BEST_RANGE];
            if (RF5_BREATH_MASK &(1L << (i-32))) is_breath=TRUE;
            else is_breath=FALSE;
        }
        else if (i < 96)
        {
            if (!(f6 &(1L << (i-64)))) continue;
            spell_desire=&spell_desire_RF6[i-64][0];
            spell_range=spell_info_RF6[i-64][COL_SPELL_BEST_RANGE];
            if (RF6_BREATH_MASK &(1L << (i-64))) is_breath=TRUE;
            else is_breath=FALSE;
        }
        else
        {
            if (!(f7 &(1L << (i-96)))) continue;
            spell_desire=&spell_desire_RF7[i-96][0];
            spell_range=spell_info_RF7[i-96][COL_SPELL_BEST_RANGE];
            if (RF7_BREATH_MASK &(1L << (i-96))) is_breath=TRUE;
            else is_breath=FALSE;
        }

        /* Base Desirability*/
        cur_spell_rating = spell_desire[D_BASE];

        /* modified for breath weapons */
        if (is_breath) cur_spell_rating = (cur_spell_rating * breath_hp) / breath_maxhp;

        /* Bonus if want summon and this spell is helpful */
        if (spell_desire[D_SUMM] && want_summon) cur_spell_rating +=
                              want_summon * spell_desire[D_SUMM];

        /* Bonus if wounded and this spell is helpful */
        if (spell_desire[D_HURT] && want_hps) cur_spell_rating +=
                            want_hps * spell_desire[D_HURT];

        /* Bonus if low on mana and this spell is helpful */
        if (spell_desire[D_MANA] && want_mana) cur_spell_rating +=
                             want_mana * spell_desire[D_MANA];

        /* Bonus if want to flee and this spell is helpful */
        if (spell_desire[D_ESC] && want_escape) cur_spell_rating +=
                              want_escape * spell_desire[D_ESC];

        /* Bonus if want a tactical move and this spell is helpful */
        if (spell_desire[D_TACT] && want_tactic) cur_spell_rating +=
                               want_tactic * spell_desire[D_TACT];

        /* Penalty if this spell is resisted */
        if (spell_desire[D_RES])
              cur_spell_rating = (cur_spell_rating * (100 - find_resist(m_idx, spell_desire[D_RES])))/100;

        /* Penalty for range if attack drops off in power */
        if (spell_range)
        {
            cur_range = m_ptr->cdis;
            while (cur_range-- > spell_range)
                cur_spell_rating = (cur_spell_rating * spell_desire[D_RANGE])/100;
        }

        /* Random factor; less random for smart monsters */
        if (r_ptr->flags2 & (RF2_SMART)) cur_spell_rating *= 16 + rand_int(100);
        else cur_spell_rating *= 12 + rand_int(50);

        /* Deflate for testing purposes */
        cur_spell_rating /= 20;

        /* Is this the best spell yet?, or alternate between equal spells*/
        if ((cur_spell_rating > best_spell_rating) ||
            ((cur_spell_rating == best_spell_rating) && one_in_(2)))
        {
            best_spell_rating = cur_spell_rating;
            best_spell = i + 96;
        }
    }

    if (cheat_know)
    {
        message(QString("Spell rating: %1.") .arg(best_spell_rating));
    }

    /* Return Best Spell */
    return (best_spell);
}



/*
 * Monster attempts to make a ranged (non-melee) attack.
 *
 * Determine if monster can attack at range, then see if it will.  Use
 * the helper function "choose_attack_spell()" to pick a physical ranged
 * attack, magic spell, or summon.  Execute the attack chosen.  Process
 * its effects, and update character knowledge of the monster.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 */
bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px)
{

    int i, k, rlev, spower, rad, manacost;
    int failrate;

    int m_idx = dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    QString m_name;
    QString m_poss;

    QString ddesc;

    /* Summon count */
    int count = 0;

    /* Summon level */
    int summon_lev;

    /* Is the player blind? */
    bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

    /* Can the player see the monster casting the spell? */
    bool seen = (!blind && m_ptr->ml);

    /* Player is vulnerable */
    bool in_range = player_can_fire_bold(m_ptr->fy, m_ptr->fx);

    bool powerful;

    u16b tmd_flag = (MON_TMD_FLG_NOTIFY);

    if (m_ptr->ml) tmd_flag |= MON_TMD_FLG_SEEN;

    if (r_ptr->flags2 & (RF2_POWERFUL)) powerful = TRUE;
    else powerful = FALSE;

    /* Determine mana cost */
    if (attack >= 224) return (FALSE);
    else if (attack >= 192) manacost = spell_info_RF7[attack-192][COL_SPELL_MANA_COST];
    else if (attack >= 160) manacost = spell_info_RF6[attack-160][COL_SPELL_MANA_COST];
    else if (attack >= 128) manacost = spell_info_RF5[attack-128][COL_SPELL_MANA_COST];
    else if (attack >=  96) manacost = spell_info_RF4[attack- 96][COL_SPELL_MANA_COST];
    else return (FALSE);

    /* Spend mana */
    m_ptr->mana -= manacost;

    /* Redraw (later) if needed */
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

    /*Monsters marked as aggressive don't stay that way permanently.  */
    if (m_ptr->mflag & (MFLAG_AGGRESSIVE))
    {
        /* Sometimes remove it */
        if (rand_int(MAX_DEPTH) > r_ptr->level) m_ptr->mflag &= ~(MFLAG_AGGRESSIVE);
    }

    /*** Get some info. ***/

    /* Extract the monster level.  Must be at least 1. */
    rlev = MAX(1, r_ptr->level);

    /* Extract the monster's spell power.  Must be at least 1. */
    spower = MAX(1, r_ptr->spell_power);

    /* Get the monster name (or "it") */
    m_name = monster_desc(m_ptr, 0x00);

    /* Get the monster possessive ("his"/"her"/"its") */
    m_poss = monster_desc(m_ptr, 0x22);

    /* Hack -- Get the "died from" name */
    ddesc = monster_desc(m_ptr, 0x88);

    /* Get the summon level */
    if (r_ptr->d_char == 'Q') summon_lev = r_ptr->level + 3;
    else                      summon_lev = r_ptr->level - 1;

    /* Calculate spell failure rate */
    failrate = 25 - (rlev + 3) / 4;

    /*stunned monsters always have a chance to fail*/
    if ((m_ptr->m_timed[MON_TMD_STUN]) && (failrate < 10)) failrate = 10;

    /* Hack -- Stupid monsters will never fail (for jellies and such) */
    if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

    /* Check for spell failure (breath/shot attacks never fail) */
    if ((attack >= 128) && (rand_int(100) < failrate))
    {
        /* Message */
        message(QString("%1 tries to cast a spell, but fails.") .arg(capitalize_first(m_name)));

        return (TRUE);
    }

    /*Monster has cast a spell*/
    m_ptr->mflag &= ~(MFLAG_ALWAYS_CAST);

    /* Hack - Remember the summoner */
    summoner = m_ptr;

    /*** Execute the ranged attack chosen. ***/
    switch (attack)
    {
        /* RF4_SHRIEK */
        case 96+0:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SHRIEK);
            if (r_ptr->flags2 & (RF2_SMART))
                message(QString("%1 shouts for help.") .arg(capitalize_first(m_name)));
            else
                message(QString("%1 makes a high pitched shriek.") .arg(capitalize_first(m_name)));
            aggravate_monsters(m_idx);
            break;
        }

        /* RF4_LASH  (used for whips, but also for spitting) */
        case 96+1:
        {
            /* Attack type and descriptions. */
            int typ;
            QString desc;
            QString add_of;
            bool do_blind = FALSE;

            int i;

            /* Get damage and effect of first melee blow. */
            int effect = r_ptr->blow[0].effect;
            int damage = damroll(r_ptr->blow[0].d_dice,
                r_ptr->blow[0].d_side);

            /* Add some more damage for other melee blows. */
            for (i = 1; i < MONSTER_BLOW_MAX; i++)
            {
                damage +=
                        damroll(r_ptr->blow[i].d_dice, r_ptr->blow[i].d_side) / 2;
            }

            /* Stop if no damage possible */
            if (!damage) break;

            /* Determine projection type, using effect. */
            switch (effect)
            {
                /* Pure damage with no extras */
                case RBE_HURT:
                {
                    typ = GF_ARROW;
                    desc = "";
                    add_of = "";
                    break;
                }
                case RBE_ACID:
                {
                    /* Some of attack is pure damage, and so
                     * resists should not be allowed to reduce
                     * damage as much as they do normally.
                     * Damage will be reduced later.
                     */
                    if (p_ptr->state.resist_acid) damage *= 2;
                    if (p_ptr->timed[TMD_OPP_ACID]) damage *= 2;

                    typ = GF_ACID;
                    desc = " acid";
                    add_of = " of";
                    break;
                }
                case RBE_ELEC:
                {
                    if (p_ptr->state.resist_elec) damage *= 2;
                    if (p_ptr->timed[TMD_OPP_ELEC]) damage *= 2;

                    typ = GF_ELEC;
                    desc = " lightning";
                    add_of = " of";
                    break;
                }
                case RBE_FIRE:
                {
                    if (p_ptr->state.resist_fire) damage *= 2;
                    if (p_ptr->timed[TMD_OPP_FIRE]) damage *= 2;

                    typ = GF_FIRE;
                    desc = " fire";
                    add_of = " of";
                    break;
                }
                case RBE_COLD:
                {
                    if (p_ptr->state.resist_cold) damage *= 2;
                    if (p_ptr->timed[TMD_OPP_COLD]) damage *= 2;

                    typ = GF_COLD;
                    desc = " frost";
                    add_of = " of";
                    break;
                }
                case RBE_POISON:
                {
                    if (p_ptr->state.resist_pois) damage *= 2;
                    if (p_ptr->timed[TMD_OPP_POIS]) damage *= 2;

                    typ = GF_POIS;
                    desc = " venom";
                    add_of = " of";
                    break;
                }
                case RBE_BLIND:
                {
                    /*hack - some cobras spit poison to blind*/
                    if (r_ptr->flags3 & (RF3_ANIMAL))
                    {
                        typ = GF_POIS;
                        desc = " venom";
                        add_of = " of";
                        do_blind = TRUE;
                    }
                    /*all other monsters*/
                    else
                    {
                        typ = GF_DARK;
                        desc = " blackness";
                        add_of = " of";
                    }
                    break;
                }
                case RBE_CONFUSE:
                case RBE_PARALYZE:
                {
                    typ = GF_CONFUSION;
                    desc = " confusion";
                    add_of = " of";
                    break;
                }

                case RBE_UN_BONUS:
                case RBE_UN_POWER:
                {
                    typ = GF_DISENCHANT;
                    desc = " unmagic";
                    add_of = " of";
                    break;
                }
                case RBE_LOSE_STR:
                case RBE_LOSE_DEX:
                case RBE_LOSE_CON:
                case RBE_LOSE_INT:
                case RBE_LOSE_WIS:
                case RBE_LOSE_CHR:
                case RBE_LOSE_ALL:
                {
                    typ = GF_TIME;
                    desc = " ruination";
                    add_of = " of";
                    break;
                }
                case RBE_EXP_10:
                case RBE_EXP_20:
                case RBE_EXP_40:
                case RBE_EXP_80:
                {
                    typ = GF_NETHER;
                    desc = " withering";
                    add_of = " of";
                    break;
                }
                default:
                {
                    typ = GF_ARROW;
                    desc = "";
                    add_of = "";
                    break;
                }
            }
            /* XXX -- Animals spit.   Acid-users spit. */
            if ((r_ptr->flags3 & (RF3_ANIMAL)) || (typ == GF_ACID))
            {
                if (blind) message(QString("You hear a soft sound."));
                else if (do_blind)
                {
                    message(QString("%1 spits%2 straight into your eyes.") .arg(capitalize_first(m_name)) .arg(desc));
                }
                else message(QString("%1 spits%2 at you.") .arg(capitalize_first(m_name)) .arg(desc));
            }
            /* All other creatures use a whip. */
            else
            {
                if (blind) message(QString("You hear a crack."));
                else message(QString("%1 lashes at you with a whip%2%3.") .arg(capitalize_first(m_name)) .arg(add_of) .arg(desc));
            }

            /* Crack the whip, or spit - range 3 */
            mon_beam(m_idx, typ, damage, 3);

            if (do_blind)
            {
                /* Increase blindness */
                if (!p_ptr->state.resist_blind)
                {
                    (void)inc_timed(TMD_BLIND, 10 + randint(rlev), TRUE);
                }
            }

            break;
        }
        /* RF4_BOULDER */
        case 96+2:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("You hear something grunt with exertion."));
            else if (spower < 8) message(QString("%1 hurls a rock at you.") .arg(capitalize_first(m_name)));
            else message(QString("%1 hurls a boulder at you.") .arg(capitalize_first(m_name)));
            mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_ROCK);
            break;
        }

        /* RF4_SHOT */
        case 96+3:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("You hear something whirl towards you."));
            else if (spower < 4) message(QString("%1 slings a pebble at you.") .arg(capitalize_first(m_name)));
            else if (spower < 10) message(QString("%1 slings a leaden pellet at you.") .arg(capitalize_first(m_name)));
            else message(QString("%1 slings a seeker shot at you.") .arg(capitalize_first(m_name)));

            mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_SHOT);
            break;
        }

        /* RF4_ARROW */
        case 96+4:
        {
            disturb(TRUE, TRUE);
            if (spower < 4)
            {
                if (blind) message(QString("You hear a soft twang."));
                else message(QString("%1 fires a small arrow.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 10)
            {
                if (blind) message(QString("You hear a twang."));
                else message(QString("%1 fires an arrow.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("You hear a loud thwang."));
                else message(QString("%1 fires a seeker arrow.") .arg(capitalize_first(m_name)));
            }

            mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_AMMO);
            break;
        }

        /* RF4_BOLT */
        case 96+5:
        {
            disturb(TRUE, TRUE);
            if (spower < 4)
            {
                if (blind) message(QString("You hear a soft twung."));
                else message(QString("%1 fires a little bolt.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 10)
            {
                if (blind) message(QString("You hear a twung."));
                else message(QString("%1 fires a crossbow bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("You hear a loud thwung."));
                else message(QString("%1 fires a seeker bolt.") .arg(capitalize_first(m_name)));
            }

            mon_bolt(m_idx, GF_ARROW, get_dam(r_ptr, attack), PROJECT_AMMO);
            break;
        }

        /* RF4_MISSL */
        case 96+6:
        {
            disturb(TRUE, TRUE);
            if (spower < 4)
            {
                if (blind) message(QString("You hear something small coming at you."));
                else message(QString("%1 fires a little missile.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 10)
            {
                if (blind) message(QString("You hear something coming at you.."));
                else message(QString("%1 fires a missile.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("You hear something powerful coming at you.."));
                else message(QString("%1 fires a heavy missile.") .arg(capitalize_first(m_name)));
            }

            mon_bolt(m_idx, GF_MISSILE, get_dam(r_ptr, attack), PROJECT_AMMO);
            break;
        }

        /* RF4_PMISSL */
        case 96+7:
        {
            disturb(TRUE, TRUE);

            if (blind) message(QString("You hear a soft 'fftt' sound."));
            else message(QString("%1 whips a poisoned dart at you.") .arg(capitalize_first(m_name)));
            mon_bolt_no_effect(m_idx, GF_POIS, get_dam(r_ptr, attack));
            break;
        }

        /* RF4_BRTH_ACID */
        case 96+8:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_ACID);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes acid.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_ACID, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            /*
             * Breaths are 40-degree arcs for POWERFUL monsters,
             * 20 degrees for others.
             */
            mon_arc(m_idx, GF_ACID, TRUE, get_breath_dam(m_ptr->hp, GF_ACID, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_ELEC */
        case 96+9:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_ELEC);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes lightning.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_ELEC, (m_ptr->hp / 4), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_ELEC, TRUE, get_breath_dam(m_ptr->hp, GF_ELEC, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_FIRE */
        case 96+10:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_FIRE);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes fire.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_FIRE, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_FIRE, TRUE, get_breath_dam(m_ptr->hp, GF_FIRE, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_COLD */
        case 96+11:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_FROST);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes frost.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_COLD, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_COLD, TRUE, get_breath_dam(m_ptr->hp, GF_COLD, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_POIS */
        case 96+12:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_GAS);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes gas.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_POIS, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_POIS, TRUE, get_breath_dam(m_ptr->hp, GF_POIS, powerful),
                    0, (powerful ? 50 : 30));
            break;
        }

        /* RF4_BRTH_PLAS */
        case 96+13:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_PLASMA);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes plasma.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_PLASMA, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_PLASMA, TRUE, get_breath_dam(m_ptr->hp, GF_PLASMA, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_LIGHT */
        case 96+14:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_LIGHT);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes light.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_LIGHT, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_LIGHT, TRUE, get_breath_dam(m_ptr->hp, GF_LIGHT, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_DARK */
        case 96+15:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_DARK);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            message(QString("%1 breathes darkness.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_DARK, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_DARK, TRUE, get_breath_dam(m_ptr->hp, GF_DARK, powerful),
                     0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_CONFU */
        case 96+16:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_CONF);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes confusion.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_CONFUSION, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_CONFUSION, TRUE, get_breath_dam(m_ptr->hp, GF_CONFUSION, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_SOUND */
        case 96+17:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_SOUND);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes sound.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_SOUND, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_SOUND, TRUE, get_breath_dam(m_ptr->hp, GF_SOUND, powerful),
                   0, (powerful ? 50 : 30));
            break;
        }

        /* RF4_BRTH_SHARD */
        case 96+18:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_SHARDS);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes shards.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_SHARD, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_SHARD, TRUE, get_breath_dam(m_ptr->hp, GF_SHARD, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_INER */
        case 96+19:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_INERTIA);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes inertia.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_INERTIA_NPP, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_INERTIA_NPP, TRUE, get_breath_dam(m_ptr->hp, GF_INERTIA_NPP, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_GRAV */
        case 96+20:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_GRAVITY);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes gravity.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_GRAVITY, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_GRAVITY, TRUE, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* Unused */
        case 96+21:
        {
            break;
        }

        /* RF4_BRTH_FORCE */
        case 96+22:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_FORCE);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes force.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_FORCE, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_FORCE, TRUE, get_breath_dam(m_ptr->hp, GF_FORCE, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_NEXUS */
        case 96+23:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_NEXUS);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes nexus.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_NEXUS, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_NEXUS, TRUE, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }
        /* RF4_BRTH_NETHR */
        case 96+24:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_NETHER);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes nether.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_NETHER, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_NETHER, TRUE, get_breath_dam(m_ptr->hp, GF_NETHER, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_CHAOS */
        case 96+25:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_CHAOS);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes chaos.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_CHAOS, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_CHAOS, TRUE, get_breath_dam(m_ptr->hp, GF_CHAOS, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_DISE */
        case 96+26:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_DISENCHANT);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes disenchantment.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_DISENCHANT, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_DISENCHANT, TRUE, get_breath_dam(m_ptr->hp, GF_DISENCHANT, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_TIME */
        case 96+27:
        {
            disturb(TRUE, TRUE);
            sound(MSG_BR_TIME);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes time.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_TIME, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_TIME, TRUE, get_breath_dam(m_ptr->hp, GF_TIME, powerful),
                   0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_BRTH_MANA */
        case 96+28:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
            else message(QString("%1 breathes raw mana.") .arg(capitalize_first(m_name)));

            /* Handle Moria breath spells the old fashioned way */
            if (game_mode == GAME_NPPMORIA)
            {
                mon_ball(m_idx, GF_TIME, (m_ptr->hp / 3), 2, py, px);
                break;
            }

            mon_arc(m_idx, GF_MANA, TRUE, get_breath_dam(m_ptr->hp, GF_MANA, powerful),
                    0, (powerful ? 40 : 20));
            break;
        }

        /* RF4_XXX6 */
        case 96+29:
        {
            break;
        }

        /* RF4_XXX7 */
        case 96+30:
        {
            break;
        }

        /* RF4_XXX8 */
        case 96+31:
        {
            break;
        }

        /* RF5_BALL_ACID */
        case 128+0:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_ACID))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of acid.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of acid.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a small acid ball.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an acid ball.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of acid.") .arg(capitalize_first(m_name)));
                if (spower < 80) rad = 3;
                else rad = 4;
            }
            mon_ball(m_idx, GF_ACID, get_ball_beam_dam(m_idx, r_ptr, attack, GF_ACID, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_ELEC */
        case 128+1:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_ELEC))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of electricity.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of electricity.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a small ball of electricity.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a ball of electricity.") .arg(capitalize_first(m_name)));
            }

            /* Electricity is the most variable of all attacks at high level. */
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));

                if (rand_int(3) != 0)
                {
                message(QString("%1 invokes a storm of electricity.") .arg(capitalize_first(m_name)));
                    if (spower < 80) rad = 3;
                    else rad = 4;
                    spower = 3 * spower / 4;
                }
                else
                {
                    message(QString("%1 calls a massive stroke of lightning down upon you!") .arg(capitalize_first(m_name)));
                    rad = 0;
                    spower = 3 * spower / 2;
                }
            }
            mon_ball(m_idx, GF_ELEC, get_ball_beam_dam(m_idx, r_ptr, attack, GF_ELEC, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_FIRE */
        case 128+2:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_FIRE))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of flames.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of flames.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a ball of fire.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a ball of fire.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 80)
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a firestorm.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            else
            {
                if (blind) message(QString("%1 intones in rising wrath.") .arg(capitalize_first(m_name)));
                else message(QString("%1 conjures up a maelstrom of fire!") .arg(capitalize_first(m_name)));
                rad = 4;
            }
            mon_ball(m_idx, GF_FIRE, get_ball_beam_dam(m_idx, r_ptr, attack, GF_FIRE, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_COLD */
        case 128+3:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_COLD))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of frost.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of frost.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a small frost ball.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a frost ball.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of frost.") .arg(capitalize_first(m_name)));
                if (spower < 80) rad = 3;
                else rad = 4;
            }
            mon_ball(m_idx, GF_COLD, get_ball_beam_dam(m_idx, r_ptr, attack, GF_COLD, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_POIS */
        case 128+4:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_POIS))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of poison.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of poison.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a stinking cloud.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a venomous cloud.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of poison.") .arg(capitalize_first(m_name)));
                if (spower < 80) rad = 4;
                else rad = 5;
            }
            mon_ball(m_idx, GF_POIS, get_ball_beam_dam(m_idx, r_ptr, attack, GF_POIS, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_LIGHT */
        case 128+5:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_LIGHT))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of light.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes a brilliant ball of light.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a sphere of light.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes an explosion of light.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a powerful explosion of light.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_LIGHT, get_ball_beam_dam(m_idx, r_ptr, attack, GF_LIGHT, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_DARK */
        case 128+6:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_DARK))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of darkness.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of darkness.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 20)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a ball of darkness.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 70)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a storm of darkness.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a powerful darkness storm.") .arg(capitalize_first(m_name)));
                if (spower < 110) rad = 3;
                else rad = 4;
            }
            mon_ball(m_idx, GF_DARK, get_ball_beam_dam(m_idx, r_ptr, attack, GF_DARK, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_CONFU */
        case 128+7:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_CONFU))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of confusion.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an massive ball of confusion.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a ball of confusion.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a storm of confusion.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a powerful storm of confusion.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_CONFUSION, get_ball_beam_dam(m_idx, r_ptr, attack, GF_CONFUSION, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_SOUND */
        case 128+8:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_SOUND))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of noise.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an ear-splitting ball of noise.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 calls up a blast of sound.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a thunderclap.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 unleashes a cacophony of sound.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_SOUND, get_ball_beam_dam(m_idx, r_ptr, attack, GF_SOUND, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_SHARD */
        case 128+9:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_SHARD))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of shards.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of shards.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 calls up up a blast of shards.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 50)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 calls up a whirlwind of shards.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of shards!") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_SHARD, get_ball_beam_dam(m_idx, r_ptr, attack, GF_SHARD, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_METEOR */
        case 128+10:
        {
            disturb(TRUE, TRUE);
            if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 produces a meteor shower.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 produces a meteor storm.") .arg(capitalize_first(m_name)));
                rad = 2;
            }
            else
            {
                if (blind) message(QString("%1 murmurs strongly.") .arg(capitalize_first(m_name)));
                else message(QString("%1 produces a violent meteor storm.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_METEOR, get_ball_beam_dam(m_idx, r_ptr, attack, GF_METEOR, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_STORM */
        case 128+11:
        {
            disturb(TRUE, TRUE);

            if (spower < 22)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 gestures fluidly.") .arg(capitalize_first(m_name)));
                message(QString("You are surrounded by a little storm."));
                rad = 2;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 gestures fluidly.") .arg(capitalize_first(m_name)));
                message(QString("You are engulfed in a whirlpool."));
                rad = 3;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 gestures fluidly.") .arg(capitalize_first(m_name)));
                message(QString("You are lost in a raging tempest of wind and water!"));
                rad = 5;
            }
            mon_ball(m_idx, GF_WATER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_WATER, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_NETHR */
        case 128+12:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_NETHR))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a nether ball.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous nether ball.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 22)
            {
                if (blind) message(QString("%1 whispers nastily.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an orb of nether.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs a deadly word.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a nether ball.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 intones with deadly menace.") .arg(capitalize_first(m_name)));
                else message(QString("%1 calls up a storm of nether magics.") .arg(capitalize_first(m_name)));
            rad = 3;
            }
            mon_ball(m_idx, GF_NETHER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_NETHER, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_CHAOS */
        case 128+13:
        {
            disturb(TRUE, TRUE);

            rad = 2;

            /* Special handling for breathers as opposed to casters */
            if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
            {
                if (spower < 40)
                {
                    if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                    else message(QString("%1 breathes a ball of chaos.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    if (blind) message(QString("%1 breathes forcefully.") .arg(capitalize_first(m_name)));
                    message(QString("%1 breathes an enormous ball of chaos.") .arg(capitalize_first(m_name)));
                    rad = 3;
                }
            }
            else if (spower < 13)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a sphere of chaos.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an exlosion of raw chaos.") .arg(capitalize_first(m_name)));
                rad = 2;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of chaos.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_CHAOS, get_ball_beam_dam(m_idx, r_ptr, attack, GF_CHAOS, powerful), rad, py, px);
            break;
        }

        /* RF5_BALL_MANA */
        case 128+14:
        {
            disturb(TRUE, TRUE);
            if (spower < 25)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a mana burst.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 50)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a mana ball.") .arg(capitalize_first(m_name)));
                rad = 2;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of mana.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_MANA, get_ball_beam_dam(m_idx, r_ptr, attack, GF_MANA, powerful), rad, py, px);

            break;
        }

        /* RF5_BALL_WATER */
        case 128+15:
        {
            disturb(TRUE, TRUE);
            if (spower < 15)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a small water ball.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 40)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a water ball.") .arg(capitalize_first(m_name)));
                rad = 2;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 invokes a storm of water.") .arg(capitalize_first(m_name)));
                if (spower < 120) rad = 3;
                else rad = 4;
            }
            mon_ball(m_idx, GF_WATER, get_ball_beam_dam(m_idx, r_ptr, attack, GF_WATER, powerful), rad, py, px);
            break;
        }

        /* RF5_BOLT_ACID */
        case 128+16:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an acid bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of acid.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_ACID, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_ELEC */
        case 128+17:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of electricity.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of lightning.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_ELEC, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_FIRE */
        case 128+18:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a fire bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 throws a fiery sphere at you.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_FIRE, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_COLD */
        case 128+19:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a frost bolt.") .arg(capitalize_first(m_name)));
            }
            else
        {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a frost bolt.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_COLD, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_POIS */
        case 128+20:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a poison bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of venom.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_POIS, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_PLAS */
        case 128+21:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a plasma bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of plasma.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_PLASMA, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_ICE */
        case 128+22:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an ice bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a bolt of ice.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_ICE, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_WATER */
        case 128+23:
        {
            disturb(TRUE, TRUE);
            if (spower < 50)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a water bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a water bolt.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_WATER, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_NETHR */
        case 128+24:
        {
            disturb(TRUE, TRUE);
            if (spower < 40)
            {
                if (blind) message(QString("%1 whispers nastily.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a nether bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs a deadly word.") .arg(capitalize_first(m_name)));
                else message(QString("%1 hurls a black bolt of nether at you.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_NETHER, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_MANA */
        case 128+25:
        {
            disturb(TRUE, TRUE);
            if ((spower < 5) || (spower <= rlev / 10))
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a magic missile.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a mana bolt.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_MANA, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BOLT_GRAV */
        case 128+26:
        {
            disturb(TRUE, TRUE);
            if ((spower < 5) || (spower <= rlev / 10))
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 fires a gravity bolt.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a powerful bolt of gravity.") .arg(capitalize_first(m_name)));
            }
            mon_bolt(m_idx, GF_GRAVITY, get_dam(r_ptr, attack), 0L);
            break;
        }

        /* RF5_BEAM_ELEC */
        case 128+27:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("You feel a crackling in the air."));

            /* Special handling for breathers as opposed to casters */
            else if (r_ptr->flags4 & (RF4_BRTH_ELEC))
            {
                message(QString("%1 breathes a lightning bolt.") .arg(capitalize_first(m_name)));
            }
            else message(QString("%1 shoots a spark of lightning at you.") .arg(capitalize_first(m_name)));

            mon_beam(m_idx, GF_ELEC, get_ball_beam_dam(-1, r_ptr, attack, GF_ELEC, powerful), 10);
            break;
        }

        /* RF5_BEAM_ICE */
        case 128+28:
        {
            disturb(TRUE, TRUE);
            if (r_ptr->flags4 & (RF4_BRTH_COLD))
            {
                if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                else message(QString("%1 breathes an icy spear") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an icy lance.") .arg(capitalize_first(m_name)));
            }
            mon_beam(m_idx, GF_ICE, get_ball_beam_dam(-1, r_ptr, attack, GF_ICE, powerful), 12);
            break;
        }

        /* RF5_BEAM_NETHR */
        case 128+29:
        {
            disturb(TRUE, TRUE);
            if (r_ptr->flags4 & (RF4_BRTH_NETHR))
            {
                if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                else message(QString("%1 breathes a beam of nether") .arg(capitalize_first(m_name)));
            }
            else if (spower < 25)
            {
                if (blind) message(QString("%1 whispers nastily.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a beam of nether.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 50)
            {
                if (blind) message(QString("%1 murmurs a deadly word.") .arg(capitalize_first(m_name)));
                else message(QString("%1 hurls a nether lance.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 intones with deadly menace.") .arg(capitalize_first(m_name)));
                else message(QString("%1 unleashes a ray of death.") .arg(capitalize_first(m_name)));
            }
            mon_beam(m_idx, GF_NETHER, get_ball_beam_dam(-1, r_ptr, attack, GF_NETHER, powerful), 10);
            break;
        }

        /* RF5_BEAM_LAVA */
        case 128+30:
        {
            disturb(TRUE, TRUE);
            /* SLightly different message for breathers */
            if (r_ptr->flags4 & (RF4_BRTH_ALL))
            {
                if (blind) message(QString("%1 breathes.") .arg(capitalize_first(m_name)));
                else message(QString("%1 breathes a stream of fiery lava.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 25)
            {
                if (blind) message(QString("%1 begins murmuring.") .arg(capitalize_first(m_name)));
                else message(QString("%1 shoots a beam of molten magma.") .arg(capitalize_first(m_name)));
            }
            else if (spower < 50)
            {
                if (blind) message(QString("%1 mubles something.") .arg(capitalize_first(m_name)));
                else message(QString("%1 shoots a jet of lava.") .arg(capitalize_first(m_name)));
            }
            else
            {
                if (blind) message(QString("%1 mubles something.") .arg(capitalize_first(m_name)));
                else message(QString("%1 shoots a searing jet of lava.") .arg(capitalize_first(m_name)));
            }
            mon_beam(m_idx, GF_LAVA, get_ball_beam_dam(-1, r_ptr, attack, GF_LAVA, powerful), 10);
            break;
        }

        /* RF5_HOLY_ORB */
        case 128+31:
        {
            disturb(TRUE, TRUE);
            if (spower < 40)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts an orb of draining.") .arg(capitalize_first(m_name)));
                rad = 1;
            }
            else if (spower < 90)
            {
                if (blind) message(QString("%1 murmurs deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a powerful orb of draining.") .arg(capitalize_first(m_name)));
                rad = 2;
            }
            else
            {
                if (blind) message(QString("%1 chants powerfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 casts a large orb of holy might.") .arg(capitalize_first(m_name)));
                rad = 3;
            }
            mon_ball(m_idx, GF_HOLY_ORB, get_ball_beam_dam(-1, r_ptr, attack, GF_HOLY_ORB, powerful), rad, py, px);

            break;
        }


        /* RF6_HASTE */
        case 160+0:
        {
            if (in_range)
            {
                disturb(TRUE, TRUE);

                if (!seen)
                {
                    message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    message(QString("%1 concentrates on %2 body.") .arg(capitalize_first(m_name)) .arg(m_poss));
                }
            }

            /*add to the haste counter*/
            mon_inc_timed(m_idx, MON_TMD_FAST, r_ptr->level + rand_int(r_ptr->level), MON_TMD_FLG_NOTIFY);

            break;
        }

        /* RF6_ADD_MANA */
        case 160+1:
        {
            if (in_range)
            {
                disturb(TRUE, TRUE);

                if (!seen)
                {
                    message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    message(QString("%1 gathers %2 power.") .arg(capitalize_first(m_name)) .arg(m_poss));
                }
            }

            /* Increase current mana.  Do not exceed maximum. */
            m_ptr->mana += (spower / 15) + 1;
            if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

            break;
        }

        /* RF6_HEAL */
        case 160+2:
        {
            int gain, cost;

            if (in_range)
            {
                disturb(TRUE, TRUE);

                /* Message */
                if (!seen)
                {
                    message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    message(QString("%1 concentrates on %2 wounds.") .arg(capitalize_first(m_name)) .arg(m_poss));
                }
            }

            /* We regain lost hitpoints (up to spower * 3) */
            gain = MIN(m_ptr->maxhp - m_ptr->hp, spower * 3);

            /* We do not gain more than mana * 15 HPs at a time */
            gain = MIN(gain, m_ptr->mana * 15);

            /* Regain some hitpoints */
            m_ptr->hp += gain;

            /* Lose some mana (high-level monsters are more efficient) */
            cost = 1 + gain / (5 + 2 * r_ptr->level / 5);

            /* Reduce mana (do not go negetive) */
            m_ptr->mana -= MIN(cost, m_ptr->mana);

            /* Fully healed */
            if (m_ptr->hp >= m_ptr->maxhp)
            {
                /* Fully healed */
                m_ptr->hp = m_ptr->maxhp;

                /* Message */
                if (in_range)
                {
                    if (seen) message(QString("%1 looks very healthy!") .arg(capitalize_first(m_name)));
                    else      message(QString("%1 sounds very healthy!") .arg(capitalize_first(m_name)));
                }
            }

            /* Partially healed */
            else
            {
                /* Message */
                if (in_range)
                {
                    if (seen) message(QString("%1 looks healthier.") .arg(capitalize_first(m_name)));
                    else      message(QString("%1 sounds healthier.") .arg(capitalize_first(m_name)));
                }
            }


            /* Redraw (later) if needed */
            if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

            /* Cancel fear */
            if (m_ptr->m_timed[MON_TMD_FEAR])
            {
                /* Cancel fear */
                mon_clear_timed(m_idx, MON_TMD_FEAR , tmd_flag);
            }

            /* Recalculate combat range later */
            m_ptr->min_range = 0;

            break;
        }

        /* RF6_CURE */
        case 160+3:
        {
            if (seen) message(QString("%1 concentrates on %2 ailments.") .arg(capitalize_first(m_name)) .arg(m_poss));

            /* Cancel stunning */
            if (m_ptr->m_timed[MON_TMD_STUN])
            {
                /* Cancel stunning */
                mon_clear_timed(m_idx, MON_TMD_STUN , tmd_flag);

            }

            /* Cancel fear */
            if (m_ptr->m_timed[MON_TMD_FEAR])
            {
                /* Cancel fear */
                mon_clear_timed(m_idx, MON_TMD_FEAR , tmd_flag);
            }

            /* Cancel slowing */
            if (m_ptr->m_timed[MON_TMD_SLOW])
            {
                /* Cancel fear */
                mon_clear_timed(m_idx, MON_TMD_SLOW , tmd_flag);
            }

            /* Redraw (later) if needed */
            if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

            break;
        }

        /* RF6_BLINK */
        case 160+4:
        {
            if (teleport_away(m_idx, 10))
            {

                if (seen) disturb(TRUE, TRUE);

                /*
                 * If it comes into view from around a corner (unlikely)
                 * give a message and learn about the casting
                 */
                if (!seen && m_ptr->ml)
                {
                    message(QString("%1 blinks into view.") .arg(capitalize_first(ddesc)));
                    disturb(TRUE, TRUE);
                    seen = TRUE;
                }

                /* Normal message */
                else
                {
                    if (seen) message(QString("%1 blinks away.") .arg(capitalize_first(m_name)));
                }
            }
            break;
        }

        /* RF6_TPORT */
        case 160+5:
        {
            if (teleport_away(m_idx, MAX_SIGHT * 2 + 5))
            {

                if (seen)
                {
                    disturb(TRUE, TRUE);
                    message(QString("%1 teleports away.") .arg(capitalize_first(m_name)));
                }

                /*
                 * if it comes into view from around a corner (VERY unlikely)
                 * give a message and learn about the casting
                 */
                else if (m_ptr->ml)
                {
                    message(QString("%1 teleports.") .arg(capitalize_first(ddesc)));
                    disturb(TRUE, TRUE);
                    seen = TRUE;
                }
            }

            break;
        }

        /* RF6_XXX3 */
        case 160+6:
        {
            break;
        }

        /* RF6_TELE_SELF_TO */
        case 160+7:
        {
            int old_cdis = m_ptr->cdis;

            if (seen) disturb(TRUE, TRUE);

            /* Move monster near player (also updates "m_ptr->ml"). */
            teleport_towards(m_ptr->fy, m_ptr->fx, py, px);

            /* Use up a little bit more energy than a standard move */
            m_ptr->m_energy -= BASE_ENERGY_MOVE / 2;

            /* Monster is now visible, but wasn't before. */
            if ((!seen) && (m_ptr->ml))
            {
                disturb(TRUE, TRUE);
                /* Message */
                message(QString("%1 suddenly appears.") .arg(capitalize_first(ddesc)));
            }

            /* Monster was visible before, but isn't now. */
            else if ((seen) && (!m_ptr->ml))
            {
                /* Message */
                message(QString("%1 blinks away.") .arg(capitalize_first(m_name)));
            }

            /* Monster is visible both before and after. */
            else if ((seen) && (m_ptr->ml))
            {
                if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) < (old_cdis - 1))
                {
                    message(QString("%1 blinks toward you.") .arg(capitalize_first(m_name)));
                }
                else
                {
                    message(QString("%1 blinks.") .arg(capitalize_first(m_name)));
                }
            }

            /* Have we seen them at any point?  If so, we will learn about the spell. */
            if (m_ptr->ml) seen = TRUE;

            break;
        }

        /* RF6_TELE_TO */
        case 160+8:
        {
            disturb(TRUE, TRUE);
            message(QString("%1 commands you to return.") .arg(capitalize_first(m_name)));
            teleport_player_to(m_ptr->fy, m_ptr->fx);

            /* Use up a little bit more energy than a standard move */
            m_ptr->m_energy -= BASE_ENERGY_MOVE * 3 / 4;
            break;
        }

        /* RF6_TELE_AWAY */
        case 160+9:
        {
            disturb(TRUE, TRUE);
            message(QString("%1 teleports you away.") .arg(capitalize_first(m_name)));
            teleport_player(100, FALSE);
            break;
        }

        /* RF6_TELE_LEVEL */
        case 160+10:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles strangely.") .arg(capitalize_first(m_name)));
            else message(QString("%1 gestures at your feet.") .arg(capitalize_first(m_name)));

            if (p_ptr->state.resist_nexus)
            {
                message(QString("You are unaffected!"));
            }
            else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You resist the effects!"));
            }
            else
            {
                (void)teleport_player_level(m_idx);
            }
            break;
        }

        /* RF6_XXX5 */
        case 160+11:
        {
            break;
        }

        /* RF6_DARKNESS */
        case 160+12:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 gestures in shadow.") .arg(capitalize_first(m_name)));
            (void)unlight_area(0, 3);
            break;
        }

        /* RF6_TRAPS */
        case 160+13:
        {
            disturb(TRUE, TRUE);
            sound(MSG_CREATE_TRAP);
            if (blind) message(QString("%1 mumbles, and then cackles evilly.") .arg(capitalize_first(m_name)));
            else message(QString("%1 casts a spell and cackles evilly.") .arg(capitalize_first(m_name)));
            (void)trap_creation(SOURCE_OTHER);
            break;
        }

        /* Unused - Was Amnesia attack */
        case 160+14:
        {
            break;
        }

        /* RF6_DRAIN_MANA */
        case 160+15:
        {
            if (p_ptr->csp)
            {
                int r1;

                /* Disturb if legal */
                disturb(TRUE, TRUE);

                /* Basic message */
                message(QString("%1 draws psychic energy from you!") .arg(capitalize_first(m_name)));

                /* Attack power */
                r1 = (randint(spower) / 20) + 1;

                /* Full drain */
                if (r1 >= p_ptr->csp)
                {
                    r1 = p_ptr->csp;
                    p_ptr->csp = 0;
                    p_ptr->csp_frac = 0;
                }

                /* Partial drain */
                else
                {
                    p_ptr->csp -= r1;
                }

                /* Redraw mana */
                p_ptr->redraw |= (PR_SIDEBAR_MON);

                /* Replenish monster mana */
                if (m_ptr->mana < r_ptr->mana)
                {
                    if ( r1 > r_ptr->mana - m_ptr->mana)
                    {
                         r1 -= r_ptr->mana - m_ptr->mana;
                         m_ptr->mana = r_ptr->mana;
                    }
                    else
                    {
                         m_ptr->mana += r1;
                         r1 = 0;
                    }

                    /* Redraw (later) if needed */
                    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

                }

                /* Heal the monster with remaining energy */
                if ((m_ptr->hp < m_ptr->maxhp) && (r1))
                {
                    /* Heal */
                    m_ptr->hp += (30 * (r1 + 1));
                    if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

                    /* Redraw (later) if needed */
                    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

                    /* Special message */
                    if (seen)
                    {
                        message(QString("%1 appears healthier.") .arg(capitalize_first(m_name)));
                    }
                }
            }
            break;
        }

        /* RF6_XXX6 */
        case 160+16:
        {
            break;
        }

        /* RF6_XXX7 */
        case 160+17:
        {
            break;
        }

        /* RF6_MIND_BLAST */
        case 160+18:
        {
            disturb(TRUE, TRUE);
            if (!seen)
            {
                message(QString("You feel something focusing on your mind."));
            }
            else
            {
                message(QString("%1 gazes deep into your eyes.") .arg(capitalize_first(m_name)));
            }

            if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You resist the effects!"));
            }
            else
            {
                message(QString("Your mind is blasted by psionic energy."));
                if (allow_player_confusion())
                {
                    (void)inc_timed(TMD_CONFUSED, rand_int(4) + 4, TRUE);
                }

                take_hit(get_dam(r_ptr, attack), ddesc);
            }
            break;
        }

        /* RF6_BRAIN_SMASH */
        case 160+19:
        {
            disturb(TRUE, TRUE);
            if (!seen)
            {
                message(QString("You feel something focusing on your mind."));

            }
            else
            {
                message(QString("%1 looks deep into your eyes.") .arg(capitalize_first(m_name)));
            }
            if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You resist the effects!"));
            }
            else
            {
                message(QString("Your mind is blasted by psionic energy."));
                take_hit(get_dam(r_ptr, attack), ddesc);
                if (!p_ptr->state.resist_blind)
                {
                    (void)inc_timed(TMD_BLIND, 8 + rand_int(8), TRUE);
                }
                if (allow_player_confusion())
                {
                    (void)inc_timed(TMD_CONFUSED, rand_int(4) + 4, TRUE);
                }
                if (!p_ptr->state.free_act)
                {
                    (void)inc_timed(TMD_PARALYZED, rand_int(4) + 4, TRUE);
                }
                (void)inc_timed(TMD_SLOW, rand_int(4) + 4, TRUE);
            }
            break;
        }

        /* RF6_WOUND */
        case 160+20:
        {
            disturb(TRUE, TRUE);

            if (spower < 4)
            {
                if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 points at you and curses.") .arg(capitalize_first(m_name)));
                k = 1;
            }
            else if (spower < 10)
            {
                if (blind) message(QString("%1 mumbles deeply.") .arg(capitalize_first(m_name)));
                else message(QString("%1 points at you and curses horribly.") .arg(capitalize_first(m_name)));
                k = 2;
            }
            else if (spower < 20)
            {
                if (blind) message(QString("%1 murmurs loudly.") .arg(capitalize_first(m_name)));
                else message(QString("%1 points at you, incanting terribly.") .arg(capitalize_first(m_name)));
                k = 3;
            }
            else if (spower < 35)
            {
                if (blind) message(QString("%1 cries out wrathfully.") .arg(capitalize_first(m_name)));
                else message(QString("%1 points at you, screaming words of peril!") .arg(capitalize_first(m_name)));
                k = 4;
            }
            else
            {
                if (blind) message(QString("%1 screams the word 'DIE!'") .arg(capitalize_first(m_name)));
                else message(QString("%1 points at you, screaming the word DIE!") .arg(capitalize_first(m_name)));
                k = 5;
            }

            if (rand_int(rlev / 2 + 70) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You resist the effects%1") .arg(spower < 30 ?  '.' : '!'));
            }
            else
            {
                /*
                 * Inflict damage. Note this spell has a hack
                 * than handles damage differently in get_dam.
                 */
                take_hit(get_dam(r_ptr, attack), ddesc);

                /* Cut the player depending on strength of spell. */
                if (k == 1) (void)set_cut(p_ptr->timed[TMD_CUT] + 8 + damroll(2, 4));
                if (k == 2) (void)set_cut(p_ptr->timed[TMD_CUT] + 23 + damroll(3, 8));
                if (k == 3) (void)set_cut(p_ptr->timed[TMD_CUT] + 46 + damroll(4, 12));
                if (k == 4) (void)set_cut(p_ptr->timed[TMD_CUT] + 95 + damroll(8, 15));
                if (k == 5) (void)set_cut(1200);
            }
            break;
        }

        /* RF6_XXX6 */
        case 160+21:
        {
            break;
        }

        /* RF6_XXX7 */
        case 160+22:
        {
            break;
        }

        /* RF6_XXX8 */
        case 160+23:
        {
            break;
        }

        /* RF6_XXX9 */
        case 160+24:
        {
            break;
        }

        /* RF6_HUNGER */
        case 160+25:
        {
            if (blind) message(QString("You are commanded to feel hungry."));
            else message(QString("%1 gestures at you, and commands that you feel hungry.") .arg(capitalize_first(m_name)));

            if (rand_int(rlev / 2 + 70) > p_ptr->state.skills[SKILL_SAVE])
            {
                /* Reduce food abruptly.  */
                (void)set_food(p_ptr->food - (p_ptr->food/4));

            }

            else message(QString("You resist the effects!"));

            break;
        }

        /* RF6_XX11 */
        case 160+26:
        {
            break;
        }

        /* RF6_SCARE */
        case 160+27:
        {
            disturb(TRUE, TRUE);
            sound(MSG_CAST_FEAR);
            if (blind) message(QString("%1 mumbles, and you hear scary noises.") .arg(capitalize_first(m_name)));
            else message(QString("%1 casts a fearful illusion.") .arg(capitalize_first(m_name)));
            if (p_ptr->state.resist_fear)
            {
                message(QString("You refuse to be frightened."));
            }
            else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You refuse to be frightened."));
            }
            else
            {
                i = div_round(r_ptr->level, 10);
                (void)inc_timed(TMD_AFRAID, i + rand_range(3, 6), TRUE);
            }
            break;
        }

        /* RF6_BLIND */
        case 160+28:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));

            /* Must not already be blind */
            else if (!p_ptr->timed[TMD_BLIND])
            {
                message(QString("%1 casts a spell, burning your eyes!") .arg(capitalize_first(m_name)));
                if (p_ptr->state.resist_blind)
                {
                    message(QString("You are unaffected!"));
                }
                else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
                {
                    message(QString("You blink, and your vision clears."));
                }
                else
                {
                    i = div_round(r_ptr->level, 4);
                    (void)inc_timed(TMD_BLIND, i + rand_range(5, 10), TRUE);
                }
            }

            break;
        }

        /* RF6_CONF */
        case 160+29:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles, and you hear puzzling noises.") .arg(capitalize_first(m_name)));
            else message(QString("%1 creates a mesmerising illusion.") .arg(capitalize_first(m_name)));
            if (!allow_player_confusion())
            {
                message(QString("You disbelieve the feeble spell."));
            }
            else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You disbelieve the feeble spell."));
            }
            else
            {
                i = div_round(r_ptr->level, 8);
                (void)inc_timed(TMD_CONFUSED, i + rand_range(4, 8), TRUE);
            }
            break;
        }

        /* RF6_SLOW */
        case 160+30:
        {
            disturb(TRUE, TRUE);
            message(QString("%1 drains power from your muscles!") .arg(capitalize_first(m_name)));
            if (p_ptr->state.free_act)
            {
                message(QString("You are unaffected!"));
            }
            else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                message(QString("You resist the effects!"));
            }
            else
            {
                i = div_round(r_ptr->level, 25);
                (void)inc_timed(TMD_SLOW, i + rand_range(5, 10), TRUE);
            }
            break;
        }

        /* RF6_HOLD */
        case 160+31:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
                else message(QString("%1 stares deep into your eyes!") .arg(capitalize_first(m_name)));
            if (p_ptr->state.free_act)
            {
                if (!p_ptr->timed[TMD_PARALYZED]) message(QString("You are unaffected!"));
            }
            else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
            {
                if (!p_ptr->timed[TMD_PARALYZED]) message(QString("You stare back unafraid!"));
            }

            /* Must not already be paralyzed */
            else if (!p_ptr->timed[TMD_PARALYZED])
            {
                (void)inc_timed(TMD_PARALYZED, rand_range(4, 8), TRUE);
            }
            break;
        }

        /* RF7_S_KIN */
        case 192 + 0:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons %2 %3.") .arg(capitalize_first(m_name)) .arg(m_poss)
                         .arg((r_ptr->flags1 & (RF1_UNIQUE)) ? "minions" : "kin"));

            /* Hack -- Set the letter of the monsters to summon */
            summon_kin_type = r_ptr->d_char;
            for (k = 0; k < (r_ptr->level > 40 ? 4 : 3); k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_KIN, 0L);
            }

            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_KIN);

            if (blind && count)
            {
                message(QString("You hear many things appear nearby."));
            }
            break;
        }

        /* RF7_XXX */
        case 192 + 1:  break;
        case 192 + 2:  break;


        /* RF7_S_MONSTER */
        case 192 + 3:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_MONSTER);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons help!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 1; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, 0L);
            }

            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_MONSTER);

            if (blind && count) message(QString("You hear something appear nearby."));
            break;
        }

        /* RF7_S_MONSTERS */
        case 192 + 4:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_MONSTER);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons monsters!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_MONSTERS);

            if (blind && count) message(QString("You hear many things appear nearby."));
            break;
        }

        /* RF7_XXX */
        case 192 + 5:  break;
        case 192 + 6:  break;
        case 192 + 7:  break;

        /* RF7_S_ANT */
        case 192 + 8:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons ants.") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_ANT, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_ANT);

            if (blind && count) message(QString("You hear chittering and skittering."));
            break;
        }

        /* RF7_S_SPIDER */
        case 192 + 9:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_SPIDER);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons spiders.") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_SPIDER, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_SPIDER);

            if (blind && count) message(QString("You hear many things appear nearby."));
            break;
        }

        /* RF7_S_HOUND */
        case 192 + 10:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_HOUND);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons hounds.") .arg(capitalize_first(m_name)));
            for (k = 0; k < 2; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_HOUND, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_HOUND);

            if (blind && count) message(QString("You hear snarling."));
            break;
        }

        /* RF7_S_ANIMAL */
        case 192 + 11:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_ANIMAL);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons natural creatures.") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_ANIMAL, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_ANIMAL);

            if (blind && count) message(QString("You hear many things appear nearby."));
            break;
        }

        /* RF7_S_HYDRA */
        case 192 + 12:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_HYDRA);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons hydras.") .arg(capitalize_first(m_name)));
            for (k = 0; k < 3; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HYDRA, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_HYDRA);

            if (blind && count)
            {
                message(QString("You hear hissing and slithering."));
            }
            break;
        }

        /* RF7_XXX */
        case 192 + 13:	break;

        /* RF7_S_THIEF */
        case 192 + 14:
        {
            disturb(TRUE, TRUE);
            if (blind) message(QString("%1 whistles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 whistles up a den of thieves!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_THIEF, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_THIEF);

            if (blind && count) message(QString("You hear footsteps and the whetting of knives."));
            break;
        }

        /* Summon Bert, Bill, and Tom */
        /* No messages unless sucessful */
        /* RF7_S_BERTBILLTOM */
        case 192 + 15:
        {
            for (k = 0; k < 2; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev + 5, SUMMON_BERTBILLTOM, 0L);
            }

            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_BERTBILLTOM);

            /* No messages unless successful */
            if (count)
            {
                if (blind) message(QString("Heavy footsteps approach!"));
                else       message(QString("%1 calls up his friends!") .arg(capitalize_first(m_name)));
            }
            break;
        }

        /* RF7_XXX */
        case 192 + 16:	break;

        /* RF6_S_AINU */
        case 192 + 17:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_AINU);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons a maia!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 1; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_AINU, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_AINU);

            if (blind && count) message(QString("Majestic songs of the Ainur fill the dungeon!"));
            break;
        }

        case 192 + 18:	break;
        case 192 + 19:	break;

        /* RF7_S_DRAGON */
        case 192 + 20:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_DRAGON);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons a dragon!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 1; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_DRAGON, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_DRAGON);

            if (blind && count) message(QString("You feel something breathing on you..."));
            break;
        }

        /* RF7_S_HI_DRAGON */
        case 192 + 21:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_HI_DRAGON);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons ancient dragons!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_HI_DRAGON, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_DRAGON);

            if (blind && count)
            {
                message(QString("You feel the breath of great wyrms."));
            }
            break;
        }

        /* RF7_XXX */
        case 192 + 22:	break;
        case 192 + 23:	break;

        /* RF7_S_DEMON */
        case 192 + 24:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_DEMON);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            {
                if (!(blind)) message(QString("%1 magically summons a hellish adversary!") .arg(capitalize_first(m_name)));
                for (k = 0; k < 1; k++)
                {
                    count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_DEMON, 0L);
                }
                /* No suitable matches, don't try this spell again */
                if (!count) dungeon_summon_mask_f7 |= (RF7_S_DEMON);

                if (blind && count) message(QString("You smell fire and brimstone."));
            }
            break;
        }

        /* RF7_S_HI_DEMON */
        case 192 + 25:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_HI_DEMON);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons greater demons!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_HI_DEMON, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_DEMON);

            if (blind && count)
            {
                message(QString("You hear many evil things appear nearby."));
            }
            break;
        }

        /* RF7_XXX */
        case 192 + 26:	break;

        /* Summon Uniques */
        /* RF7_S_UNIQUE */
        case 192 + 27:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_UNIQUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));

            for (k = 0; k < 3
            ; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_UNIQUE, 0L);
            }
            if (count)
            {
                if (blind) message(QString("You've got a bad feeling about this..."));
                else       message(QString("%1 magically summons mighty opponents!") .arg(capitalize_first(m_name)));
            }
            else
            {
                /* No suitable matches, don't try this spell again */
                dungeon_summon_mask_f7 |= (RF7_S_UNIQUE);

                if (!blind)
                    message(QString("%1 gestures imperiously ... and looks puzzled for a moment.") .arg(capitalize_first(m_name)));
            }
            break;
        }

        /* Summon Uniques */
        /* RF7_S_HI_UNIQUE */
        case 192 + 28:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_UNIQUE);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));

            for (k = 0; k < 3
            ; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_HI_UNIQUE, 0L);
            }
            if (count)
            {
                if (blind) message(QString("You've got a bad feeling about this..."));
                else       message(QString("%1 magically summons legendary opponents!") .arg(capitalize_first(m_name)));
            }
            else
            {
                /* No suitable matches, don't try this spell again */
                dungeon_summon_mask_f7 |= (RF7_S_HI_UNIQUE);

                if (!blind)
                    message(QString("%1 gestures imperiously ... and looks puzzled for a moment.") .arg(capitalize_first(m_name)));
            }
            break;
        }

        /* RF7_S_UNDEAD */
        case 192 + 29:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_UNDEAD);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons an undead adversary!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 1; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_UNDEAD, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_UNDEAD);

            if (blind && count) message(QString("You hear something creepy appear nearby."));
            break;
        }

        /* RF7_S_HI_UNDEAD */
        case 192 + 30:
        {
            disturb(TRUE, TRUE);
            sound(MSG_SUM_HI_UNDEAD);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons greater undead!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 4; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_HI_UNDEAD, 0L);
            }

            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_HI_UNDEAD);

            if (blind && count)
            {
                message(QString("You hear many creepy things appear nearby."));
            }
            break;
        }


        /* Summon the Ringwraiths */
        /* RF7_S_WRAITH */
        case 192 + 31:
        {
            int old_count;
            disturb(TRUE, TRUE);
            sound(MSG_SUM_WRAITH);
            if (blind) message(QString("%1 mumbles.") .arg(capitalize_first(m_name)));
            else message(QString("%1 magically summons mighty undead opponents!") .arg(capitalize_first(m_name)));
            for (k = 0; k < 6; k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                    summon_lev, SUMMON_WRAITH, 0L);
            }
            /* No suitable matches, don't try this spell again */
            if (!count) dungeon_summon_mask_f7 |= (RF7_S_WRAITH);
            old_count = count;
            for (k = 0; (k < 6) && (count < 6); k++)
            {
                count += summon_specific(m_ptr->fy, m_ptr->fx,
                             summon_lev, SUMMON_HI_UNDEAD, 0L);
            }
            if (count == old_count)
            {
                /* No suitable matches, don't try this spell again */
                dungeon_summon_mask_f7 |= (RF7_S_HI_UNDEAD);
            }
            if (blind && count)
            {
                message(QString("You hear many creepy things appear nearby."));
            }
            break;
        }



        /* Paranoia */
        default:
        {
            message(QString("A monster tried to cast a spell that has not yet been defined."));
        }
    }

    /* Hack - Forget the summoner */
    summoner = NULL;

    /* Learn Player Resists */
    if (attack < 128)
    {
          update_smart_learn(m_idx, spell_desire_RF4[attack-96][D_RES]);
    }
    else if (attack < 160)
    {
          update_smart_learn(m_idx, spell_desire_RF5[attack-128][D_RES]);
    }
    else if (attack < 192)
    {
          update_smart_learn(m_idx, spell_desire_RF6[attack-160][D_RES]);
    }
    else if (attack < 224)
    {
          update_smart_learn(m_idx, spell_desire_RF7[attack-192][D_RES]);
    }

    /* Mark minimum desired range for recalculation */
    m_ptr->min_range = 0;

    /* Remember what the monster did to us */
    if (seen)
    {
        /* Innate spell */
        if (attack < 32*4)
        {
        l_ptr->r_l_flags4 |= (1L << (attack - 32*3));
        if (l_ptr->ranged < UCHAR_MAX) l_ptr->ranged++;
        }

        /* Bolt or Ball */
        else if (attack < 32*5)
        {
        l_ptr->r_l_flags5 |= (1L << (attack - 32*4));
        if (l_ptr->ranged < UCHAR_MAX) l_ptr->ranged++;
        }

        /* Special spell */
        else if (attack < 32*6)
        {
        l_ptr->r_l_flags6 |= (1L << (attack - 32*5));
        if (l_ptr->ranged < UCHAR_MAX) l_ptr->ranged++;
        }

        /* Summon spell */
        else if (attack < 32*7)
        {
        l_ptr->r_l_flags7 |= (1L << (attack - 32*6));
        if (l_ptr->ranged < UCHAR_MAX) l_ptr->ranged++;
        }

    }

    if (seen && p_ptr->is_wizard)
        message(QString("%1 has %2 mana remaining.") .arg(capitalize_first(m_name)) .arg(m_ptr->mana));

    /* Always take note of monsters that kill you */
    if (p_ptr->is_dead && (l_ptr->deaths < SHRT_MAX))
    {
        l_ptr->deaths++;
    }

    /* A spell was cast */
    return (TRUE);
}


/*
 * Can the monster exist in this grid?
 *
 * Because this function is designed for use in monster placement and
 * generation as well as movement, it cannot accept monster-specific
 * data, but must rely solely on racial information.
 */
bool cave_exist_mon(const monster_race *r_ptr, int y, int x, bool occupied_ok, bool damage_ok, bool can_dig)
{
    feature_type *f_ptr;

    /* Check Bounds */
    if (!in_bounds(y, x)) return (FALSE);

    /* Check location */
    f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /* The grid is already occupied. */
    if (dungeon_info[y][x].has_monster())
    {
        if (!occupied_ok) return (FALSE);
    }

    /* Glyphs -- must break first */
    if (cave_player_glyph_bold(y, x)) return (FALSE);

    /* Permanent walls are never OK */
    if (_feat_ff1_match(f_ptr, FF1_MOVE | FF1_PERMANENT) ==
        (FF1_PERMANENT)) return (FALSE);

    /*** Check passability of various features. ***/

    /* Feature is a wall */
    if (!cave_passable_bold(y, x))
    {
        /* Monster isn't allowed to enter */
        if (!can_dig) return (FALSE);

        /* Handle creatures who can go through walls */
        if ((r_ptr->flags2 & (RF2_KILL_WALL)) ||
            (r_ptr->flags2 & (RF2_PASS_WALL)))
        {
            /* Monster is not going there by choice */
            if (damage_ok) return (TRUE);

            /* Check to see if monster wants to go there */
            if (cave_no_dam_for_mon(y, x, r_ptr)) return (TRUE);

            else return (FALSE);

        }
        else return (FALSE);
    }

    /* Monster is not going there by choice */
    if (damage_ok) return (TRUE);

    /* Check to see if monster wants to go there */
    if (cave_no_dam_for_mon(y, x, r_ptr)) return (TRUE);

    /* Flying monsters can pass through dangerous terrain */
    if (r_ptr->flags3 & (RF3_FLYING)) return (TRUE);

    /*Monster will be damaged going there*/
    return (FALSE);

}
