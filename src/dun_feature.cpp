/* File: feature.c */

/*
 * Copyright (c) 2006 Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "src/npp.h"


/*
 * Obtains the name of a feature.
 * Set add_prefix to TRUE if you want the name prefixed by an article.
 * Set get_mimic to TRUE if you want the fake name of the feature, if any.
 */
QString feature_desc(u16b feat, bool add_prefix, bool get_mimic)
{
    QString prefix;
    QString name;
    QString full_name;

    /* Get mimic feature, if requested */
    if (get_mimic) feat = f_info[feat].f_mimic;

    /* Paranoia */
    if (f_info[feat].f_name.isEmpty())
    {
        full_name = "unknown feature";

        return (full_name);
    }

    /* Get the name */
    name = f_info[feat].f_name;

    /* Found special mark -- No prefix */
    if (name[0] == '~')
    {
        // get rid of prefix.
        name.remove(0,1);
    }
    /* Want prefix */
    else if (add_prefix)
    {
        /* Check the first letter of the name for the right article */
        if (begins_with_vowel(name)) prefix = "an ";

        else prefix = "a ";

        /* Ignore prefix if there is one already */
        if (name.contains("the ")) prefix.clear();

        /* Hack -- Handle shops */
        if (feat_ff1_match(feat, FF1_SHOP))
        {
            prefix = "the entrance to the ";
        }
    }

    /* Hack -- Append the prefix quickly */
    full_name.clear();

    // start with the prefix
    full_name = prefix;

    /* Hack -- Add the name */
    full_name.append(name);

    return (full_name);
}


/*
 * Provide adjustments to combat chances based on the player position and
 * nativity.
 *
 * If being_attacked is TRUE, we assume that chance is a monster's chance to
 * hit the player.
 * If being_attacked is FALSE, we assume that chance is the player's chance to
 * hit a monster.
 */
int feat_adjust_combat_for_player(int chance, bool being_attacked)
{
    feature_type *f_ptr = &f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx];
    feature_lore *f_l_ptr = &f_l_list[dungeon_info[p_ptr->py][p_ptr->px].feature_idx];

    int bonus;

    /*No adjustments when the player is flying. */
    if (p_ptr->timed[TMD_FLYING]) return chance;

    /* Native player adjustment to combat */
    if (is_player_native(p_ptr->py, p_ptr->px))
    {
        /* Raw bonus to hit */
        bonus = f_ptr->native_to_hit_adj - 100;

        /*Mark the feature lore if the player understands*/
        if ((player_can_observe()) && (f_l_ptr->f_l_native_to_hit_adj < UCHAR_MAX))
        {
            f_l_ptr->f_l_native_to_hit_adj++;
        }
    }

    /*Non_native player adjustment to combat*/
    else
    {
        /* Raw bonus to hit */
        bonus = f_ptr->non_native_to_hit_adj - 100;

        /*Mark the feature lore if the player understands*/
        if ((player_can_observe()) && (f_l_ptr->f_l_non_native_to_hit_adj < UCHAR_MAX))
        {
            f_l_ptr->f_l_non_native_to_hit_adj++;
        }
    }

    /*
     * Invert the bonus if the player is being attacked (we are adjusting
     * monster's chance to hit)
     */
    if (being_attacked) bonus = -bonus;

    /* Adjust chance to hit */
    chance = (chance * (100 + bonus)) / 100;

    return (chance);
}


/*
 * Provide adjustments to combat chances based on the player position and
 * nativity.
 *
 * If being_attacked is TRUE, we assume that chance is the player's chance to
 * hit the monster.
 * If being_attacked is FALSE, we assume that chance is the monster's chance to
 * hit the player
 */
int feat_adjust_combat_for_monster(const monster_type *m_ptr, int chance,
    bool being_attacked)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    feature_type *f_ptr = &f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx];
    feature_lore *f_l_ptr = &f_l_list[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx];

    int bonus;

    /* Flying monsters get no change */
    if (m_ptr->mflag & (MFLAG_FLYING))
    {
        return (chance);
    }
    /* Monster_native adjustment to combat */
    else if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
    {
        /* Raw bonus to hit */
        bonus = f_ptr->native_to_hit_adj - 100;

        /* Mark the monster lore */
        if((m_ptr->ml) && player_can_observe())
        {
            u32b native = f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx].f_flags3;
            native &= r_ptr->r_native;
            l_ptr->r_l_native |= native;
        }

        /* Mark the feature lore if the player understands */
        if ((player_can_observe()) && (f_l_ptr->f_l_native_to_hit_adj < UCHAR_MAX))
        {
            f_l_ptr->f_l_native_to_hit_adj++;
        }
    }
    /* Non_native monster adjustment to combat */
    else
    {
        /* Raw bonus to hit */
        bonus = f_ptr->non_native_to_hit_adj - 100;

        /*Mark the feature lore if the player understands*/
        if ((player_can_observe()) && (f_l_ptr->f_l_non_native_to_hit_adj < UCHAR_MAX))
        {
            f_l_ptr->f_l_non_native_to_hit_adj++;
        }
    }

    /*
     * Invert the bonus if the monster is being attacked (we are adjusting
     * player's chance to hit)
     */
    if (being_attacked) bonus = -bonus;

    /* Adjust chance to hit */
    chance = (chance * (100 + bonus)) / 100;

    return (chance);
}


/*
 * Find a secret at the specified location and change it according to
 * the state.
 */
void find_secret(int y, int x)
{
    feature_type *f_ptr;
    feature_lore *f_l_ptr;

    /* Get feature */
    f_ptr = &f_info[dungeon_info[y][x].feature_idx];
    f_l_ptr = &f_l_list[dungeon_info[y][x].feature_idx];

    if (f_l_ptr->f_l_sights < UCHAR_MAX) f_l_ptr->f_l_sights++;

    if (player_has_los_bold(y, x) && (!f_ptr->f_text.isEmpty()))
    {
        /* You have found something */
        message(f_ptr->f_text);
    }

    /* Change the location */
    cave_alter_feat(y, x, FS_SECRET);

    /* Disturb */
    disturb(FALSE, FALSE);
}




/*
 * Take a feature, determine what that feature becomes
 * through applying the given action.
 */
u16b feat_state(u16b feat, int action)
{
    int i;

    /* Permanent stuff never gets changed */
    if (feat_ff1_match(feat, FF1_PERMANENT)) return (feat);

    /* Get the new feature */
    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        /* Found the action */
        if (f_info[feat].state[i].fs_action == action)
        {
            /* Return the new state */
            return (f_info[feat].state[i].fs_result);
        }
    }

    /* Try with the default action */
    return (f_info[feat].defaults);
}

/*
 * Determine the power of the given action. If the action isn't found we
 * return the value of the f_power field.
 */
u16b feat_state_power(u16b feat, int action)
{
    int i;

    /* Search the action  */
    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        /* Found the action */
        if (f_info[feat].state[i].fs_action == action)
        {
            /* Return the transition power */
            return (f_info[feat].state[i].fs_power);
        }
    }

    /* Try with the default power */
    return (f_info[feat].f_power);
}


/*
 * Determine the power of the given action.
 * Returns 0 if the feature doesn't have an explicit K: directive
 * for the given action
 */
u16b feat_state_explicit_power(u16b feat, int action)
{
    int i;

    /* Search the action  */
    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        /* Found the action */
        if (f_info[feat].state[i].fs_action == action)
        {
            /* Return the transition power */
            return (f_info[feat].state[i].fs_power);
        }
    }

    /* Not found */
    return (0);
}


/*
 * Fire a bolt at the player
 * Stop if we hit a monster, but this function should only fire with a clear shot.
 * Affect monsters and the player
 */
static void other_beam_or_bolt(int y, int x, int typ, int dam, bool beam)
{

    u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY | PROJECT_EFCT| PROJECT_ITEM;

    if (beam) flg |= PROJECT_BEAM;

    /* Target the player with a bolt attack */
    (void)project(SOURCE_EFFECT, 0, y, x, p_ptr->py, p_ptr->px, dam, typ, flg, 0 , 0);
}

/*
 * Fire an orb at the player from a trap/effect
 * Affect monsters and the player
 */
static bool other_orb_or_ball(int y, int x, int typ, int rad, int dam, bool orb)
{
    int source_diameter = (orb ? (10 + rad * 10) : 0);

    /* Add the ball bitflags */
    u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_WALL | PROJECT_PLAY;

    /* Limit radius to nine (up to 256 grids affected) */
    if (rad > 9) rad = 9;

    /* Cast a ball */
    return (project(SOURCE_EFFECT, rad, y, x, p_ptr->py, p_ptr->px, dam, typ, flg,   0, source_diameter));
}


/*
 * Handle a smart trap firing at the player.
 * Note that the variables y and x are only be used for a real trap.
 */
u16b fire_trap_smart(int f_idx, int y, int x, byte mode, QString* desc)
{
    desc->clear();

    /*Careful, in MODE-DESCRIBE, this will be undefined*/
    effect_type *x_ptr = &x_list[dungeon_info[y][x].effect_idx];

    /* Get feature */
    feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];

    /*paranoia*/
    if (!_feat_ff2_match(f_ptr, FF2_TRAP_SMART)) return (FALSE);

    /* Reveal the trap.  */
    if (mode == MODE_ACTION)
    {
        bool did_message = FALSE;

        QString feat_name = feature_desc(f_idx, FALSE, TRUE);

        /* Reveal it*/
        if (x_ptr->x_flags & (EF1_HIDDEN))
        {

            message(QString("A %1 appears!") .arg(feat_name));

            /*Make sure the next message makes sense*/
            did_message = TRUE;

            /*No longer hidden*/
            x_ptr->x_flags &= ~(EF1_HIDDEN);

            /* Memorize */
            dungeon_info[y][x].mark_square();

            /*Light it up*/
            light_spot(y, x);
        }

        /* We have seen this feature */
        f_ptr->f_everseen = TRUE;

        /* Print the message*/
        if (did_message) message(QString("It %1") .arg(f_ptr->f_text));
        else message(QString("The %1 %2") .arg(feat_name) .arg(f_ptr->f_text));

        /*Count in the feature lore the number of times set off*/
        if (f_l_ptr->f_l_power < UCHAR_MAX)
        {
            f_l_ptr->f_l_power++;
        }

        /* Disturb the player */
        disturb(TRUE, TRUE);
    }

    /* Don't describe if not set off*/
    if ((mode == MODE_DESCRIBE) && (!f_l_ptr->f_l_power) && !p_ptr->is_wizard)
    {
        desc->append("  The effects of this trap are unknown.");
        return(FALSE);
    }

    /* Analyze XXX XXX XXX */
    switch (f_ptr->f_power)
    {
        /*Bolts*/
        case 1:  /*Magic Missle Trap*/
        case 3:  /*Arrow Trap*/
        {

            if (mode == MODE_DESCRIBE)
            {
                desc->append(("  If there is a clear line of sight to the player, this rune fires"));
                if (f_ptr->f_power == 1) desc->append(" a magic missle");
                else if (f_ptr->f_power == 3) desc->append("  a magical arrow");
                desc->append(" at the player");
                if (f_l_ptr->f_l_power > 20)
                {
                    desc->append(QString(" as often as every %1d%2 turns.") .arg(f_ptr->x_timeout_set) .arg(f_ptr->x_timeout_rand));
                }
                return(TRUE);
            }
            if (mode == MODE_ACTION)
            {

                other_beam_or_bolt(y, x, f_ptr->x_gf_type, x_ptr->x_power, FALSE);
                return (TRUE);
            }
            else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BOLT);
            break;
        }
        case 6:  /*Lightning Trap*/
        {

            if (mode == MODE_DESCRIBE)
            {
                desc->append("  When the player is in line of sight, this rune fires");
                if (f_ptr->f_power == 6) desc->append(" a bolt of lightning");
                desc->append(" at the player");
                if (f_l_ptr->f_l_power > 20)
                {
                    desc->append(QString(" as often as every %1d%2 turns.") .arg(f_ptr->x_timeout_set) .arg(f_ptr->x_timeout_rand));
                }
                return(TRUE);

            }
            if (mode == MODE_ACTION)
            {

                other_beam_or_bolt(y, x, f_ptr->x_gf_type, x_ptr->x_power, TRUE);
                return (TRUE);
            }
            else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BEAM);
            break;
        }
        /*Orbs*/
        case 2:   /*Holy Orb Trap*/
        {

            if (mode == MODE_DESCRIBE)
            {
                desc->append("  When the player is in line of sight, this rune fires");
                if (f_ptr->f_power == 2) desc->append(" a holy orb");
                desc->append(" at the player");
                if (f_l_ptr->f_l_power > 20)
                {
                    desc->append(QString(" as often as every %1d%2 turns.") .arg(f_ptr->x_timeout_set) .arg(f_ptr->x_timeout_rand));
                }
                return(TRUE);

            }
            if (mode == MODE_ACTION)
            {
                other_orb_or_ball(y, x, f_ptr->x_gf_type, 2, x_ptr->x_power, TRUE);
                return (TRUE);
            }
            else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BALL);
            break;
        }
        /*Ball spells*/
        case 4:   /* poison ball trap*/
        case 5:   /* fire ball trap*/
        case 7:   /* frost ball trap*/
        case 8:   /* acid ball trap*/
        case 9:   /* nether ball trap*/
        case 10:  /* nexus ball trap*/
        case 13:  /* Shards Trap*/
        case 17:  /* Time Trap*/
        case 20:  /* Meteor Trap*/
        case 21:  /* Plasma Trap*/
        case 22:  /* Disenchantment Trap*/
        case 23:  /* Static Trap */
        {

            if (mode == MODE_DESCRIBE)
            {
                desc->append("  When the player is in line of sight, this rune fires");

                if (f_ptr->f_power == 4) desc->append(" a poison ball");
                else if (f_ptr->f_power == 5) desc->append(" a fire ball");
                else if (f_ptr->f_power == 7) desc->append(" a frost ball");
                else if (f_ptr->f_power == 8) desc->append(" an acid ball");
                else if (f_ptr->f_power == 9) desc->append(" a nether ball");
                else if (f_ptr->f_power == 10) desc->append(" a nexus ball");
                else if (f_ptr->f_power == 13) desc->append(" a ball of shards");
                else if (f_ptr->f_power == 17) desc->append(" a time ball");
                else if (f_ptr->f_power == 20) desc->append(" a meteor");
                else if (f_ptr->f_power == 21) desc->append(" a ball of plasma");
                else if (f_ptr->f_power == 22) desc->append(" a ball of disenchantment");
                else if (f_ptr->f_power == 23) desc->append(" a ball of static");
                desc->append(" at the player");

                if (f_l_ptr->f_l_power > 20)
                {
                    desc->append(QString(" as often as every %1d%2 turns.") .arg(f_ptr->x_timeout_set) .arg(f_ptr->x_timeout_rand));
                }
                return(TRUE);
            }
            if (mode == MODE_ACTION)
            {
                other_orb_or_ball(y, x, f_ptr->x_gf_type, 2, x_ptr->x_power, FALSE);

                return (TRUE);
            }
            else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BALL);
            break;
        }
        /* LOS */
        case 11: 	/*Confusion trap*/
        case 12:	/*Sound trap*/
        case 14:   /* Gravity Trap*/
        case 15:   /*Aggravate Monsters Trap*/
        case 16:   /* Inertia Trap*/
        case 18:   /* Light Trap*/
        case 19:   /* Darnkess Trap*/
        {

            if (mode == MODE_DESCRIBE)
            {
                desc->append("  When the player is in line of sight, this rune");

                if (f_ptr->f_power == 11) desc->append(" attempts to confuse every being in sight");
                else if (f_ptr->f_power == 12) desc->append(" surrounds the area with a deafening blast");
                else if (f_ptr->f_power == 14) desc->append(" hits the area with a blast of gravity");
                else if (f_ptr->f_power == 15) desc->append(" aggravates all creatures within line of sight");
                else if (f_ptr->f_power == 16) desc->append(" hits the area with a blast of inertia");
                else if (f_ptr->f_power == 18) desc->append(" releases a blinding light in the surrounding area");
                else if (f_ptr->f_power == 19) desc->append(" covers the surrounding area with darkness");

                if (f_l_ptr->f_l_power > 20)
                {
                    desc->append(QString(" as often as every %1d%2 turns.") .arg(f_ptr->x_timeout_set) .arg(f_ptr->x_timeout_rand));
                }
                return(TRUE);
            }
            if (mode == MODE_ACTION)
            {
                bool hurt_player = TRUE;

                /* Special handling of light trap */
                if (f_ptr->f_power == 18)
                {
                    light_room(p_ptr->py, p_ptr->px);

                }


                /* Special handling of darkness trap */
                else if (f_ptr->f_power == 19)
                {
                    unlight_room(p_ptr->py, p_ptr->px);

                }

                /*Special handling of confusion, it isn't supposed to damage the player*/
                else if (f_ptr->f_power == 11)
                {
                    hurt_player = FALSE;

                    if (allow_player_confusion())
                    {
                        (void)inc_timed(TMD_CONFUSED, rand_int(4) + 4 + x_ptr->x_power / 10, TRUE);
                    }
                }

                /* Mass aggravation does nothing to the player*/
                else if (f_ptr->f_power == 15)
                {
                    hurt_player = FALSE;
                }

                /*Affect the player for all others*/
                if (hurt_player) project_p(SOURCE_TRAP, p_ptr->py, p_ptr->px, x_ptr->x_power, f_ptr->x_gf_type, NULL);

                /* affect all monsters within LOS*/
                (void)project_los(y, x, x_ptr->x_power, f_ptr->x_gf_type);
                return (TRUE);
            }
            else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_LOS);
            break;
        }


        /*Oops!*/
        default:
            message(QString("unknown trap type"));

            break;
    }

    return(TRUE);
}


/*
 * Handle player hitting a real trap.
 * Note that the variables y and x can only be used for a real trap.
 */
QString hit_trap(int f_idx, int y, int x, byte mode)
{
    int dice,sides, reps, dam, i, num;

    QString name = "a trap";
    QString desc;
    desc.clear();

    /* Get feature */
    const feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];

    /*paranoia*/
    if (!_feat_ff2_match(f_ptr, FF2_TRAP_PASSIVE)) return(desc);

    if (mode == MODE_ACTION)
    {

        /* Get the trap effect */
        effect_type *x_ptr = &x_list[dungeon_info[y][x].effect_idx];

        if (p_ptr->timed[TMD_FLYING])
        {
            // Get the feature name */
            QString feat_name = feature_desc(f_idx, FALSE, TRUE);

            message(QString("You float over the %1.") .arg(feat_name));

            /* Disturb the player */
            disturb(FALSE, FALSE);

            /*We are done here*/
            return (desc);
        }


        /* Make it visible if necessary. Note paranoia check */
        if ((x_ptr->x_f_idx == f_idx) &&
            (x_ptr->x_flags & (EF1_HIDDEN)))
        {
            /* Now visible */
            x_ptr->x_flags &= ~(EF1_HIDDEN);

            /* Memorize */
            note_spot(y, x);

            /* Redraw */
            light_spot(y, x);
        }

        /*Count in the feature lore the number of times set off*/
        if (f_l_ptr->f_l_power < UCHAR_MAX)
        {
            f_l_ptr->f_l_power++;

            /* Disturb the player */
            disturb(FALSE, TRUE);
        }
    }

    /* Don't describe if not set off*/
    if ((mode == MODE_DESCRIBE) && (!f_l_ptr->f_l_power) && !p_ptr->is_wizard)
    {
        desc = ("  The effects of this trap are unknown.");
        return (desc);
    }

    /* Analyze XXX XXX XXX */
    switch (f_ptr->f_power)
    {
        case 0:
        {
            int dice2 = 2;
            int sides2 = 3;
            dice = 2;
            sides = 6;
            reps = 5;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This pit will cause you %1d%2 damage.") .arg(dice) .arg(sides));
                desc.append(QString("  Daggers will cut you up to %1 times for %2d%3 damage")  .arg(reps) .arg(dice2) .arg(sides2));
                desc.append(" each as you fall.");
                return (desc);
            }

            if (mode == MODE_ACTION)
            {
                if (p_ptr->state.ffall)
                {
                    message(QString("You float gently to the floor of the pit."));
                    message(QString("You carefully avoid setting off the daggers."));
                }

                else
                {
                    /* activate the ordinary daggers. */
                    message(QString("Daggers pierce you everywhere!"));

                    /* Base damage */
                    dam = damroll(dice, sides);

                    message(QString("You are impaled!"));

                    for (i = 0; i < randint(reps); i++)
                    {
                        dam += damroll(dice2, sides2);
                    }

                    (void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));

                    /* Take the damage */
                    take_hit(dam, name);
                }
            }
            break;
        }

        case 1:
        {
            dice = 2;
            sides = 6;

            if (game_mode == GAME_NPPMORIA) sides = 8;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap door will cause you %1d%2 damage when you fall through to the" ).arg(dice) .arg(sides));
                desc.append(" next level of the dungeon.");
                return (desc);
            }

            if (mode == MODE_ACTION)
            {

                message(QString("You fall through a trap door!"));
                if (p_ptr->state.ffall)
                {
                    message(QString("You float gently down to the next level."));
                }
                else
                {
                    dam = damroll(dice, sides);
                    take_hit(dam, name);
                }

                /* New depth */
                dungeon_change_level(p_ptr->depth + 1);
            }

            break;

        }

        case 2:
        {
            dice = 2;
            sides = 6;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This pit will cause you %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                message(QString("You fall into a pit!"));
                if (p_ptr->state.ffall)
                {
                    message(QString("You float gently to the bottom of the pit."));
                }
                else
                {
                    dam = damroll(dice, sides);
                    take_hit(dam, name);
                }
            }
            break;
        }

        case 3:
        {

            dice = 2;
            sides = 6;


            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This spiked pit will cause you %1d%2 damage when you fall into it.") .arg(dice) .arg(sides));
                desc.append(QString("  50 percent the time, The spikes will cut you up to (%1d%2) * 2 turns.") .arg(dice) .arg(sides));
                return (desc);
            }

            if (mode == MODE_ACTION)
            {

                message(QString("You fall into a spiked pit!"));

                if (p_ptr->state.ffall)
                {
                    message(QString("You float gently to the floor of the pit."));
                    message(QString("You carefully avoid touching the spikes."));
                }

                else
                {
                    /* Base damage */
                    dam = damroll(dice, sides);

                    /* Extra spike damage */
                    if (rand_int(100) < 50)
                    {
                        message(QString("You are impaled!"));

                        dam = dam * 2;
                        (void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));
                    }

                    /* Take the damage */
                    take_hit(dam, name);
                }
            }
            break;
        }

        case 4:
        {
            int percentage = 50;

            dice = 2;
            sides = 6;


            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This poison spiked pit will cause you %1d%1 damage when you fall into it.") .arg(dice) .arg(sides));
                desc.append (QString("  %1 percent of the time, the spikes will cut you up to (%2d%3) * 2 turns") .arg(percentage) .arg(dice) .arg(sides));
                desc.append (QString("  as well as poison you up to (%1d%2) * 4 turns.") .arg(dice) .arg(sides));
                return (desc);
            }

            if (mode == MODE_ACTION)
            {

                message(QString("You fall into a poison spiked pit!"));

                if (p_ptr->state.ffall)
                {
                    message(QString("You float gently to the floor of the pit."));
                    message(QString("You carefully avoid touching the poison spikes."));
                }

                else
                {
                    /* Base damage */
                    dam = damroll(dice, sides);

                    /* Extra spike damage */
                    if (rand_int(100) < percentage)
                    {
                        message(QString("You are impaled on poisonous spikes!"));

                        dam = dam * 2;
                        (void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));

                        if (p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois)
                        {
                            message(QString("The poison does not affect you!"));
                        }
                        else
                        {
                            dam = dam * 2;
                            (void)inc_timed(TMD_POISONED, randint(dam), TRUE);
                        }
                    }

                    /* Take the damage */
                    take_hit(dam, name);
                }
            }

            break;
        }

        case 5:
        {
            int sum_base = 2;
            int sum_plus = 3;


            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap will attempt to summon between %1 and %2 creatures.") .arg(sum_base + 1) .arg(sum_base + sum_plus));
                return (desc);
            }

            if (mode == MODE_ACTION)
            {

                message(QString("You are enveloped in a cloud of smoke!"));
                dungeon_info[y][x].cave_info &= ~(CAVE_MARK);

                /* Destroy the trap */
                delete_effect_idx(dungeon_info[y][x].effect_idx);

                /* Forget the trap */
                dungeon_info[y][x].cave_info &= ~(CAVE_MARK);

                num = sum_base + randint(sum_plus);
                for (i = 0; i < num; i++)
                {
                   (void)summon_specific(y, x, p_ptr->depth, 0, MPLACE_OVERRIDE);
                }
            }
            break;
        }

        case 6:
        {
            int dist = 100;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap will teleport you up to %1 squares away.") .arg(dist));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                message(QString("You hit a teleport trap!"));
                teleport_player(dist, FALSE);
            }
            break;
        }

        case 7:
        {
            dice = 4;
            sides = 6;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This fire trap will envelope you in flames for %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {
                message(QString("You are enveloped in flames!"));
                dam = damroll(dice, sides);
               fire_dam(dam, "a fire trap");
            }
            break;
        }

        case 8:
        {
            dice = 4;
            sides = 6;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This acid trap will splash you with acid for %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {
                message(QString("You are splashed with acid!"));
                dam = damroll(dice, sides);
                acid_dam(dam, "an acid trap");
            }
            break;
        }

        case 9:
        {
            int duration = 20;
            dice = 1;
            sides = 4;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap fires a small dart that can cause you to be slowed for %1 + %2d turns,") .arg(duration) .arg(duration));
                desc.append (QString(" and cause %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {
                if (check_hit(125))
                {
                    message(QString("A small dart hits you!"));
                    dam = damroll(dice, sides);

                    take_hit(dam, name);

                    (void)inc_timed(TMD_SLOW, rand_int(duration) + duration, TRUE);
                }
                else
                {
                    message(QString("A small dart barely misses you."));
                }
            }
            break;
        }

        case 10:
        {
            dice = 1;
            sides = 4;

            if (mode == MODE_DESCRIBE)
            {
                desc = ("  This trap fires a small dart that can drain your strength,");
                desc.append (QString(" and cause %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                if (check_hit(125))
                {
                    message(QString("A small dart hits you!"));
                    dam = damroll(dice, sides);

                    take_hit(dam, name);

                    (void)do_dec_stat(A_STR);
                }
                else
                {
                    message(QString("A small dart barely misses you."));
                }
            }
            break;
        }

        case 11:
        {
            dice = 1;
            sides = 4;

            if (mode == MODE_DESCRIBE)
            {
                desc = ("  This trap fires a small dart that can drain your dexterity,");
                desc.append (QString(" and cause %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                if (check_hit(125))
                {
                    message(QString("A small dart hits you!"));
                    dam = damroll(1, 4);

                    take_hit(dam, name);

                    (void)do_dec_stat(A_DEX);
                }
                else
                {
                    message(QString("A small dart barely misses you."));
                }
            }
            break;
        }

        case 12:
        {
            dice = 1;
            sides = 4;

            if (mode == MODE_DESCRIBE)
            {
                desc = ("  This trap fires a small dart that can drain your constitution,");
                desc.append (QString(" and cause %1d%2 damage.") .arg(dice) .arg(sides));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                if (check_hit(125))
                {
                    message(QString("A small dart hits you!"));
                    dam = damroll(1, 4);

                    take_hit(dam, name);

                    (void)do_dec_stat(A_CON);
                }
                else
                {
                    message(QString("A small dart barely misses you."));
                }
            }
            break;
        }

        case 13:
        {
            int base = 25;
            int rand_base = 50;

            if (game_mode == GAME_NPPMORIA) base = 50;

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap releases black gas that can blind you for %1 + %2d turns.") .arg(base) .arg(rand_base));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                message(QString("You are surrounded by a black gas!"));
                if (!p_ptr->state.resist_blind)
                {
                    (void)inc_timed(TMD_BLIND,rand_int(rand_base) + base, TRUE);
                }
            }
            break;
        }

        case 14:
        {
            int base = 10;
            int rand_base = 20;

            if (game_mode == GAME_NPPMORIA)
            {
                base = 15;
                rand_base = 15;
            }


            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap releases gas that can confuse you for %1 + %2d turns.") .arg(base) .arg(rand_base));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {

                message(QString("You are surrounded by a gas of scintillating colors!"));
                if (allow_player_confusion())
                {
                    (void)inc_timed(TMD_CONFUSED, rand_int(rand_base) + base, TRUE);
                }
            }
            break;
        }

        case 15:
        {
            int base = 10;
            int rand_base = 30;

            if (game_mode == GAME_NPPMORIA)
            {
                base = 2;
                rand_base = 10;
            }

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap releases green gas that can poison you for %1 + %2d turns.") .arg(base) .arg(rand_base));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {


                message(QString("You are surrounded by a pungent green gas!"));
                if (!p_ptr->state.resist_pois && !p_ptr->timed[TMD_OPP_POIS] && !p_ptr->state.immune_pois)
                {
                    (void)inc_timed(TMD_POISONED, rand_int(rand_base) + base, TRUE);
                }
            }
            break;
        }

        case 16:
        {
            int base = 5;
            int rand_base = 20;

            if (game_mode == GAME_NPPMORIA)
            {
                base = 4;
                rand_base = 10;
            }

            if (mode == MODE_DESCRIBE)
            {
                desc = (QString("  This trap releases a white mist that can paralyze you for %1 + %2d turns.") .arg(base) .arg(rand_base));
                return (desc);
            }
            if (mode == MODE_ACTION)
            {
                message(QString("You are surrounded by a strange white mist!"));
                if (!p_ptr->state.free_act)
                {
                    (void)inc_timed(TMD_PARALYZED, rand_int(rand_base) + base, TRUE);
                }
            }
            break;
        }

        /*Oops!*/
        default:
            message(QString("unknown trap type"));

            break;
    }

    if (mode == MODE_ACTION) disturb(FALSE, TRUE);

    return (desc);
}


/*
 * Let an feature appear near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
 *
 */
void feat_near(int feat, int y, int x)
{
    int d, s;

    int bs, bn;
    int by, bx;
    int dy, dx;
    int ty, tx;


    bool flag = FALSE;

    /* Score */
    bs = -1;

    /* Picker */
    bn = 0;

    /* Default */
    by = y;
    bx = x;

    /* Scan local grids */
    for (dy = -3; dy <= 3; dy++)
    {
        /* Scan local grids */
        for (dx = -3; dx <= 3; dx++)
        {
            /* Calculate actual distance */
            d = (dy * dy) + (dx * dx);

            /* Ignore distant grids */
            if (d > 10) continue;

            /* Location */
            ty = y + dy;
            tx = x + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(ty, tx)) continue;

            /* Require line of sight */
            if (!los(y, x, ty, tx)) continue;

            /* Prevent overwriting permanents */
            if (f_info[dungeon_info[ty][tx].feature_idx].f_flags1 & (FF1_PERMANENT)) continue;

            /* Don't like non-floor space */
            if (!(f_info[dungeon_info[ty][tx].feature_idx].f_flags1 & (FF1_FLOOR))) continue;

            /* Don't like objects */
            if (dungeon_info[ty][tx].has_object()) continue;

            /* Calculate score */
            s = 1000 - (d - dungeon_info[ty][tx].feature_idx);

            /* Skip bad values */
            if (s < bs) continue;

            /* New best value */
            if (s > bs) bn = 0;

            /* Apply the randomizer to equivalent values */
            if ((++bn >= 2) && (rand_int(bn) != 0)) continue;

            /* Keep score */
            bs = s;

            /* Track it */
            by = ty;
            bx = tx;

            /* Okay */
            flag = TRUE;
        }
    }

    /* Give it to the floor */
    if (flag) cave_set_feat(by, bx, feat);
}


/*
 * Count which terrain has been seen on this level.
 * Clear the everseen flag for the next level.
 *
 */
void count_feat_everseen(void)
{
    int i;

    /* Delete the existing objects */
    for (i = 1; i < z_info->f_max; i++)
    {
        feature_type *f_ptr = &f_info[i];
        feature_lore *f_l_ptr = &f_l_list[i];

        /* Skip features not seen */
        if (!f_ptr->f_everseen) continue;

        /*Count the number of levels this character has been seen*/
        if (f_l_ptr->f_l_sights < UCHAR_MAX) f_l_ptr->f_l_sights ++;

        /*Clear it for the next level*/
        f_ptr->f_everseen = FALSE;
    }
}


/*
 * Helper function to find a smart trap
 */
static bool vault_trap_smart(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-traps */
    if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

    /* Decline invisible traps */
    if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

    /* Decline passive traps */
    if (_feat_ff2_match(f_ptr, FF2_TRAP_PASSIVE)) return (FALSE);

    /* Decline traps set by player */
    if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function to find a passive trap
 */
static bool vault_trap_passive(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-traps */
    if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

    /* Decline invisible traps */
    if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

    /* Decline passive traps */
    if (_feat_ff2_match(f_ptr, FF2_TRAP_SMART)) return (FALSE);

    /* Decline traps set by player */
    if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function to find any dungeon trap
 */
static bool vault_trap_all(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-traps */
    if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

    /* Decline invisible traps */
    if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

    /* Decline traps set by player */
    if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Helper function for "jammed doors"
 */
static bool vault_jammed_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Jammed doors */
    if (f_ptr->f_flags3 & (FF3_DOOR_JAMMED)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for "secret doors"
 */
static bool vault_secret_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Secret doors */
    if (f_ptr->f_flags1 & (FF1_SECRET)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for "closed doors"
 */
static bool vault_closed_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Closed doors */
    if (f_ptr->f_flags3 & (FF3_DOOR_CLOSED)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for "opened doors"
 */
static bool vault_open_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Open doors */
    if (f_ptr->f_flags3 & (FF3_DOOR_OPEN)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for "broken doors"
 */
static bool vault_broken_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Broken doors */
    if (f_ptr->f_flags3 & (FF3_DOOR_BROKEN)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for "locked doors"
 */
bool vault_locked_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

    /* Locked doors */
    if (f_ptr->f_flags3 & (FF3_DOOR_LOCKED)) return (TRUE);

    /* Decline everything else */
    return (FALSE);
}


/*
 * Helper function for boring "closed doors"
 */
static bool vault_boring_closed_door(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];

    /* Decline non-door */
    if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (FALSE);

    /* Decline non-closed doors */
    if (!_feat_ff3_match(f_ptr, FF3_DOOR_CLOSED)) return (FALSE);

    /* Decline interesting closed doors */
    if (_feat_ff3_match(f_ptr, FF3_DOOR_LOCKED | FF3_DOOR_JAMMED))
    {
        return (FALSE);
    }

    /* Accept */
    return (TRUE);
}


/*
 * Apply a "feature restriction function" to the "feature allocation table"
 */
int get_feat_num_prep(void)
{
    int i;

    /* Get the entry */
    alloc_entry *table = alloc_feat_table;

    /* Scan the allocation table */
    for (i = 0; i < alloc_feat_size; i++)
    {
        /* Accept objects which pass the restriction, if any */
        if (!get_feat_num_hook || (*get_feat_num_hook)(table[i].index))
        {
            /* Accept this object */
            table[i].prob2 = table[i].prob1;
        }

        /* Do not use this object */
        else
        {
            /* Decline this object */
            table[i].prob2 = 0;
        }
    }

    /* Success */
    return (0);
}


/*
 * Choose an feature type that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "feature allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" feature, in
 * a relatively efficient manner.
 *
 * It is (slightly) more likely to acquire a feature of the given level
 * than one of a lower level.  This is done by choosing several features
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no features are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 * Happening all the time for trapped doors.
 */
s16b get_feat_num(int level)
{
    int i, j, p;

    int f_idx;

    long value, total;

    alloc_entry *table = alloc_feat_table;

    /* Save the level */
    int old_level = level;

    /* Boost level */
    if (level > 0)
    {
        /* Occasional "boost" */
        if (one_in_(40))
        {

            /* 10-20 levels boost */
            /*level += (10 + rand_int(11));*/

            /* 25-40 levels boost */
            level += (25 + rand_int(16));
        }
    }

    /* Reset total */
    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_feat_size; i++)
    {

        /* Default */
        table[i].prob3 = 0;

        /* Features are not sorted by depth */
        if (table[i].level > level) break;

        /* Get the index */
        f_idx = table[i].index;

        /* Limit the power of OOD features */
         if ((f_info[f_idx].f_level > (old_level + 10)) &&
             (!feat_ff2_match(f_idx, FF2_LAKE | FF2_RIVER) ||
            (f_info[f_idx].dam_non_native > (p_ptr->mhp / 2)))) continue;

        /*
         * Mega-Hack -- No summoning traps in themed levels,
         * labyrinth levels, wilderness levels or arena levels
         */
        if (feat_ff2_match(f_idx, FF2_TRAP_PASSIVE) &&
            (f_info[f_idx].f_power == 5) && (*dun_cap->limited_level_summoning)())
        {
            continue;
        }

        /* Hack -- no up stairs on certain levels */
        if (feat_ff1_match(f_idx, FF1_LESS))
        {
            /* No up stairs in the town */
            if (!p_ptr->depth) continue;

            /* No up shafts in level 1 */
            if ((p_ptr->depth < 2) && feat_ff2_match(f_idx, FF2_SHAFT)) continue;
        }

        /* Hack -- no chasm/trap doors/down stairs on certain levels */
        if (feat_ff1_match(f_idx, FF1_MORE))
        {
            /* Cache the quest type at the current depth */
            int what_quest_type = quest_check(p_ptr->depth);

            /* Verify if the feature is a down shaft */
            bool is_shaft = (feat_ff2_match(f_idx, FF2_SHAFT) != 0);

            /* Check max depth */
            if (p_ptr->depth >= (MAX_DEPTH - (is_shaft ? 2: 1))) continue;

            /* Stairs have special rules */
            if (feat_ff1_match(f_idx, FF1_STAIRS))
            {
                if (no_down_stairs(p_ptr->depth)) continue;

                /* Shafts shouldn't pierce quest levels */
               if (is_shaft && quest_check(p_ptr->depth + 1)) continue;
            }
            /* Trap doors don't get along with quests */
            else if (what_quest_type) continue;
        }

        /* Accept */
        table[i].prob3 = table[i].prob2;

        /* Total */
        total += table[i].prob3;
    }

    /* No legal features */
    if (total <= 0) return (0);


    /* Pick a feature */
    value = rand_int(total);

    /* Find the feature */
    for (i = 0; i < alloc_feat_size; i++)
    {
        /* Found the entry */
        if (value < table[i].prob3) break;

        /* Decrement */
        value = value - table[i].prob3;
    }

    /* Power boost */
    p = rand_int(100);

    /* Try for a "better" object once (50%) or twice (10%) */
    if (p < 60)
    {
        /* Save old */
        j = i;

        /* Pick a object */
        value = rand_int(total);

        /* Find the monster */
        for (i = 0; i < alloc_feat_size; i++)
        {
            /* Found the entry */
            if (value < table[i].prob3) break;

            /* Decrement */
            value = value - table[i].prob3;
        }

        /* Keep the "best" one */
        if (table[i].level < table[j].level) i = j;
    }

    /* Try for a "better" object twice (10%) */
    if (p < 10)
    {
        /* Save old */
        j = i;

        /* Pick a object */
        value = rand_int(total);

        /* Find the object */
        for (i = 0; i < alloc_feat_size; i++)
        {
            /* Found the entry */
            if (value < table[i].prob3) break;

            /* Decrement */
            value = value - table[i].prob3;
        }

        /* Keep the "best" one */
        if (table[i].level < table[j].level) i = j;
    }

    // hack
    if (!table[i].index) return(FEAT_FLOOR);

    /* Result */
    return (table[i].index);
}


/*
 * Hack -- pick and create a trap.  Allow specification of smart or dumb traps.
 *
 * Any call to this function should check to limit traps, such as the check for "trap doors" on quest levels.
 */
u16b pick_trap(byte mode)
{
    u16b feat = 0;

    /* No smart traps in Moria */
    if (game_mode == GAME_NPPMORIA) mode = EFFECT_TRAP_DUMB;

    /* Set hook*/

    if (mode == EFFECT_TRAP_SMART) get_feat_num_hook = vault_trap_smart;
    else if (mode == EFFECT_TRAP_DUMB) get_feat_num_hook = vault_trap_passive;
    else get_feat_num_hook = vault_trap_all;

    get_feat_num_prep();

    /* Pick a trap */
    while (1)
    {
        /* Pick the trap */
        feat = get_feat_num(p_ptr->depth);

        /*Special handling of trap doors*/
        if (feat == FEAT_TRAP_DOOR)
        {
            /* HACK - no trap doors on quest levels  */
            if (quest_check(p_ptr->depth)) continue;

            /* Hack -- no trap doors on the deepest level */
            if (p_ptr->depth >= MAX_DEPTH-1) continue;
        }

        /*Hack - no summoning traps on themed levels*/
        if ((feat == FEAT_TRAP_SUMMON) && (feeling >= LEV_THEME_HEAD)) continue;

        /* Done */
        break;
    }

    /* Clear hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    return (feat);

}


u16b get_secret_door_num(void)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_secret_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    return (feat);
}


/*
 * Place a secret door at the given location
 */
void place_secret_door(int y, int x)
{
    u16b feat = get_secret_door_num();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_closed_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_closed_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of boring closed door at the given location.
 */
void place_boring_closed_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_boring_closed_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_open_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_open_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_broken_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_broken_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_locked_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_locked_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of jammed door at the given location.
 */
void place_jammed_door(int y, int x)
{
    u16b feat;

    /* Set the hook */
    get_feat_num_hook = vault_jammed_door;

    get_feat_num_prep();

    /* Get the door */
    feat = get_feat_num(p_ptr->depth);

    /* Clear the hook */
    get_feat_num_hook = NULL;

    get_feat_num_prep();

    /* More paranoia */
    if (!feat) return;

    /* Set the door */
    cave_set_feat(y, x, feat);
}


/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
    int tmp;

    /* Choose an object */
    tmp = rand_int(1000);

    /* Open doors (300/1000) */
    if (tmp < 300)
    {
        /* Create open door */
        place_open_door(y, x);
    }

    /* Broken doors (100/1000) */
    else if (tmp < 400)
    {
        /* Create broken door */
        place_broken_door(y, x);
    }

    /* Secret doors (200/1000) */
    else if (tmp < 600)
    {
        /* Create secret door */
        place_secret_door(y, x);
    }

    /* Closed, locked, or stuck doors (400/1000) */
    else
    {
        /* Create closed door */
        place_closed_door(y, x);
    }
}


void lore_do_probe_feature(int f_idx)
{
    feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];

    int i;

    i = randint (3);

    /*learn 1 out of three terrain flags.....*/
    switch (i)
    {
        case 1:
        {
            f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
            break;
        }
        case 2:
        {
            f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
            break;
        }

        default:
        {
            f_l_ptr->f_l_flags3 = f_ptr->f_flags3;
            break;
        }
    }

    /* Hack -- Maximal sightings 50% of the time*/
    if (one_in_(2)) f_l_ptr->f_l_sights = UCHAR_MAX;

    /* Observe all transitions another 50% of the time*/
    else
    {
        for (i = 0; i < MAX_FEAT_STATES; i++)
        {
            /*There isn't an action here*/
            if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

            /* Hack -- we have seen this transition */
            f_l_ptr->f_l_state[i] = UCHAR_MAX;
        }

        /* Hack -- Maximal info */
        f_l_ptr->f_l_defaults = UCHAR_MAX;
    }

    /*Other 33% of the time, learn all combat, movement and stealth*/
    i = randint (3);

    /*learn either movement, damage to non-native, or stealth.....*/
    switch (i)
    {
        case 1:
        {
            f_l_ptr->f_l_dam_non_native = UCHAR_MAX;
            break;
        }
        case 2:
        {
            f_l_ptr->f_l_native_moves = UCHAR_MAX;
            f_l_ptr->f_l_non_native_moves = UCHAR_MAX;
            f_l_ptr->f_l_stealth_adj = UCHAR_MAX;
            break;
        }
        default:
        {
            f_l_ptr->f_l_native_to_hit_adj = UCHAR_MAX;
            f_l_ptr->f_l_non_native_to_hit_adj = UCHAR_MAX;
            break;
        }

    }
}


/*
 * Learn everything about a feature (by cheating)
 */
void cheat_feature_lore(int f_idx, feature_lore *f_l_ptr)
{
    const feature_type *f_ptr = &f_info[f_idx];

    int i;

    /* Hack -- Maximal sightings */
    f_l_ptr->f_l_sights = UCHAR_MAX;

    /* Hack -- Maximal info */
    f_l_ptr->f_l_defaults = UCHAR_MAX;

    /* Observe "maximal" attacks */
    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        /*There isn't an action here*/
        if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

        /* Hack -- we have seen this transition */
        f_l_ptr->f_l_state[i] = UCHAR_MAX;
    }

    /* Hack -- maximal uses of power */
    f_l_ptr->f_l_power = UCHAR_MAX;

    /*Hack -- Have seen all changes to movement, stealth, and combat*/
    f_l_ptr->f_l_dam_non_native = UCHAR_MAX;
    f_l_ptr->f_l_native_moves = UCHAR_MAX;
    f_l_ptr->f_l_non_native_moves = UCHAR_MAX;
    f_l_ptr->f_l_native_to_hit_adj = UCHAR_MAX;
    f_l_ptr->f_l_non_native_to_hit_adj = UCHAR_MAX;
    f_l_ptr->f_l_stealth_adj = UCHAR_MAX;

    /* Hack -- know all the flags */
    f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
    f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
    f_l_ptr->f_l_flags3 = f_ptr->f_flags3;

}

/*
 * Returns a pointer to a dyna_g entry given its coordinates.
 * Returns NULL if the entry was not found.
 */
dynamic_grid_type *get_dynamic_terrain(byte y, byte x)
{
    int i;

    /* Search in dyna_g */
    for (i = 0; i < dyna_next; i++)
    {
        /* Get the grid info */
        dynamic_grid_type *g_ptr = &dyna_grids[i];

        /* Found the coordinates */
        if ((g_ptr->y == y) && (g_ptr->x == x)) return (g_ptr);
    }

    /* Failure */
    return (NULL);
}


/*
 * Some dynamic features use a timer. Returns the value of that timer.
 * Every tick is equal to one call to process_dynamic_terrain.
 * Returns 0 if the feature isn't timed.
 */
static byte calculate_turn_count(u16b feat)
{
    /* Growing trees */
    if ((feat == FEAT_FOREST_SOIL_DYNAMIC) || (feat == FEAT_GRASS_DYNAMIC))
    {
        return (25 + rand_int(30));
    }

    /* Geysers */
    if (feat == FEAT_WALL_WATER_BOILING_GEYSER)
    {
        return (20 + rand_int(50));
    }

    /* Sniper flowers */
    if (feat == FEAT_PUTRID_FLOWER)
    {
        return (15 + rand_int(20));
    }

    /* Silent watchers */
    if (feat == FEAT_SILENT_WATCHER)
    {
        return (5 + rand_int(10));
    }

    /* Default */
    return (0);
}


/*
 * Add a new grid to the dyna_grids array given its coordinates.
 */
void add_dynamic_terrain(byte y, byte x)
{
    dynamic_grid_type this_dynamic_grid;

    /* Fill in the grid info */
    this_dynamic_grid.y = y;
    this_dynamic_grid.x = x;
    this_dynamic_grid.new_grid = TRUE;
    this_dynamic_grid.counter = calculate_turn_count(dungeon_info[y][x].feature_idx);

    dyna_grids.append(this_dynamic_grid);
}


/*
 * Remove a grid from the dyna_grids array given its coordinates
 */
void remove_dynamic_terrain(byte y, byte x)
{
    for (int i = dyna_grids.size()-1; i >= 0; i--)
    {
        dynamic_grid_type *g_ptr = &dyna_grids[i];

        if (g_ptr->y != y) continue;
        if (g_ptr->x != x) continue;

        // Remove this grid, adjust the counter so an entry is not skipped
        dyna_grids.removeAt(i);
    }
}


/*
 * Apply the effect of *one* dynamic feature.
 * We mark the terrain lore if the dynamic terrain change is observed.
 * This function should be consistent with describe_terrain_dynamic in the feature description.
 */
static void process_dynamic_terrain_aux(dynamic_grid_type *g_ptr)
{
    /* Get coordinates */
    int y = g_ptr->y;
    int x = g_ptr->x;

    /* Get the feature */
    u16b feat = dungeon_info[y][x].feature_idx;
    u16b feat2;

    /*We need to remember is the player saw this.*/
    feature_lore *f_l_ptr = &f_l_list[feat];

    /* Dynamic fire eventually burns its adjacent grids */
    if (feat_ff3_match(feat, FF3_FIRE))
    {
        int d;
        int dam = f_info[feat].dam_non_native;
        bool can_burn = FALSE;
        bool can_smoke = FALSE;

        if (!one_in_(3)) return;

        /* Scan adjacent grids */
        for (d = 0; d < 8; d++)
        {
            /* Get the coordinates */
            int yy = y + ddy_ddd[d];
            int xx = x + ddx_ddd[d];

            /* Ignore annoying locations*/
            if (!in_bounds(yy, xx)) continue;

            /* Get the feature */
            feat2 = dungeon_info[yy][xx].feature_idx;

            /* Feature can burn */
            if (feat_ff2_match(feat2, FF2_HURT_FIRE))
            {
                u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

                /* A grid can be affected. Keep the fire alive */
                can_burn = TRUE;

                /* Prevent scumming */
                if (!player_can_fire_bold(yy, xx)) continue;

                /* Oil burns faster */
                if (feat_ff3_match(feat2, FF3_OIL))
                {
                    /* But not always */
                    if (!one_in_(5)) continue;
                }
                else
                {
                    /* Don't want to burn forests too fast */
                    if (!one_in_(35)) continue;
                }

                /* Burn/melt the feature */
                project(SOURCE_OTHER, 0, y, x, yy, xx, dam, GF_FIRE, flg, 0, 0);

                /*Mark the lore if the player observed this*/
                if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
            }
            /* We can create smoke */
            else if ((dungeon_info[yy][xx].cave_info & (CAVE_MOVE | CAVE_LOS)) == (CAVE_MOVE | CAVE_LOS))
            {
                /* Remember this fact */
                can_smoke = TRUE;

                /* But not always make smoke */
                if (!one_in_(15)) continue;

                /* Create smoke */
                set_effect_lingering_cloud(FEAT_EFFECT_SMOKE, yy, xx, 100, SOURCE_OTHER, 0);

                /*Mark the lore if the player observed this*/
                if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
            }
        }

        /* Smokers are removed sometimes to speed things up */
        if (!can_burn && (!can_smoke || one_in_(2))) remove_dynamic_terrain(y, x);

        /* Done */
        return;
    }

    /* Boiling water geysers can soak the dungeon with boiling water */
    if (feat == FEAT_WALL_WATER_BOILING_GEYSER)
    {
        u32b flg = PROJECT_BOOM | PROJECT_ITEM | PROJECT_GRID |
            PROJECT_KILL | PROJECT_PLAY;

        int dam = f_info[feat].dam_non_native;

        /* Set the timer again */
        g_ptr->counter = calculate_turn_count(feat);

        /* Check line of fire */
        if (!player_can_fire_bold(y, x)) return;

        /* Show a message */
        if (player_can_see_bold(y, x))
        {
            message(QString("The geyser explodes in a burst of boiling water!"));

            /*Mark the lore*/
            f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
        }

        /* Splash! */
        project(SOURCE_OTHER, 2 + rand_int(2), y, x, y, x, dam, GF_BWATER, flg, 0, 0);

        /* Done */
        return;
    }

    /*
     * A very bad imitation of the Fangorn forest.
     */
    if ((feat == FEAT_FOREST_SOIL_DYNAMIC) || (feat == FEAT_GRASS_DYNAMIC))
    {
        bool skip = FALSE;

        /* Ignore locations out of line of fire */
        if (!player_can_fire_bold(y, x)) skip = TRUE;

        /* Ignore locations occupied by monsters/players */
        else if (dungeon_info[y][x].monster_idx) skip = TRUE;

        /* Ignore locations occupied by objects */
        else if (dungeon_info[y][x].object_idx) skip = TRUE;

        /* Don't grow too fast */
        else if (!one_in_(50)) skip = TRUE;

        /* Cancel tree generation */
        if (skip)
        {
            /* Set the timer again */
            g_ptr->counter = calculate_turn_count(feat);

            /* Done */
            return;
        }

        /* Message */
        if (player_can_see_bold(y, x))
        {
            message(QString("The forest seems to be alive."));

            /*Mark the lore*/
            f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);

            disturb(FALSE, FALSE);
        }

        /* Create a new tree */
        /*
         * This can be done safely because the branches are passable
         * and can hold objects, so any entity adjacent to the trunk
         * remains untouched.
         */
        cave_set_feat(y, x, FEAT_TREE);

        /* Done */
        return;
    }

    /* Animate waves */
    if (feat_ff3_match(feat, FF3_WATER))
    {
        int d = -1, k;
        int yy, xx;
        bool kill = FALSE;
        int freq = 10000;

        /* Crests don't live too much time */
        if (f_info[feat].f_name.contains("crest of a wave"))
        {
            kill = TRUE;
        }
        /* Other waves are often inactive */
        else
        {
            k = rand_int(200);

            if (k >= 10) return;
        }

        /* Pick a direction for the waves */
        k = p_ptr->game_turn % freq;

        /* To the right */
        if (k < (freq / 2))
        {
                int directions[] = {3, 6, 9};
            d = directions[rand_int(3)];
        }

        /* To the left */
        else
        {
            int directions[] = {1, 4, 7};
            d = directions[rand_int(3)];
        }

        /* Transform an adjacent grid if possible */
        if (d != -1)
        {
            /* Get coordinates */
            yy = y + ddy[d];
            xx = x + ddx[d];

            /* The grid must be affected by water  */
            if (in_bounds(yy, xx) && cave_ff2_match(yy, xx, FF2_HURT_WATER))
            {
                /* Transform */
                cave_alter_feat(yy, xx, FS_HURT_WATER);

                /* Remove the wave must of the time */
                if (!kill && !one_in_(10)) kill = TRUE;
            }
        }

        /* A wave disappears eventually */
        if (kill && feat_ff2_match(feat, FF2_HURT_FIRE))
        {
            /* Get the next feature to avoid messages */
            feat = feat_state(feat, FS_HURT_FIRE);

            /*
             * Turn the wave into plain water, or a crest into a
             * wave
             */
            cave_set_feat(y, x, feat);
        }

        /* Done */
        return;
    }

    /* Sniper flowers */
    if (feat == FEAT_PUTRID_FLOWER)
    {
        u32b flg = PROJECT_PLAY | PROJECT_KILL | PROJECT_STOP;

        int dam;
        byte gf_type;

        /* Set the timer again */
        g_ptr->counter = calculate_turn_count(feat);

        /* Check line of fire and los */
        if (!player_can_fire_bold(y, x) || !player_has_los_bold(y, x)) return;

        /* Select damage type */
        gf_type = (one_in_(10) ? GF_POIS: GF_ARROW);

        /* Show a message */
        if (player_can_see_bold(y, x))
        {
            if (gf_type == GF_ARROW)
            {
                message(QString("The putrid flower aims at you with spikes!"));
            }
            else if (gf_type == GF_POIS)
            {
                message(QString("The putrid flower spits poison in your face!"));
            }

            /*Mark the lore*/
            f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
        }

        /* Message for when the player can't see */
        else
        {
            if (gf_type == GF_ARROW)
            {
                message(QString("It aims at you with spikes!"));
            }
            else if (gf_type == GF_POIS)
            {
                message(QString("It spits poison in your face!"));
            }
        }

        /* Calculate damage */
        dam = 2 * p_ptr->depth / 3;

        if (dam < 1) dam = 1;

        /* Fire a bolt to the player  */
       project(SOURCE_OTHER, 0, y, x, p_ptr->py, p_ptr->px, dam, gf_type, flg, 0, 0);

        /* Done */
        return;
    }

    /* Silent watchers */
    if (feat == FEAT_SILENT_WATCHER)
    {
        /* Set the timer again */
        g_ptr->counter = calculate_turn_count(feat);

        /* Player must be near to the silent watcher */
        if (!player_has_los_bold(y, x) || (distance(y, x, p_ptr->py, p_ptr->px) > 4)) return;

        /* Message */
        message(QString("The silent watcher howls in madness!"));

        if (player_can_see_bold(y, x))
        {
            /*Mark the lore*/
            f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
        }

        /* Stop resting/running */
        disturb(FALSE, TRUE);

        /* Call monsters */
        aggravate_monsters(SOURCE_OTHER);

        /* Done */
        return;
    }

    /* Dynamic lava eventually burns its adjacent grids */
    if (feat_ff3_match(feat, TERRAIN_MASK) == (ELEMENT_LAVA))
    {
        int d;
        int dam = f_info[feat].dam_non_native;
        bool can_burn = FALSE;

        /* Flavor */
        if (!one_in_(2)) return;

        /* Scan adjacent grids */
        for (d = 0; d < 8; d++)
        {
            /* Get the coordinates */
            int yy = y + ddy_ddd[d];
            int xx = x + ddx_ddd[d];

            /* Ignore annoying locations*/
            if (!in_bounds(yy, xx)) continue;

            /* Get the feature */
            feat2 = dungeon_info[yy][xx].feature_idx;

            /* Feature can burn */
            if (feat_ff2_match(feat2, FF2_HURT_FIRE))
            {
                u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

                /* A grid can be affected. Keep the lava alive */
                can_burn = TRUE;

                /* Prevent scumming */
                if (!player_can_fire_bold(yy, xx)) continue;

                /* Flavor */
                if (!one_in_(4)) continue;

                /* Burn/melt the feature */
                project(SOURCE_OTHER, 0, y, x, yy, xx, dam, GF_FIRE, flg, 0, 0);

                /* Mark the lore if the player observed this */
                if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
            }
        }

        /* Remove the dynamic mark if the dungeon cannot be affected, to speed things up */
        if (!can_burn) remove_dynamic_terrain(y, x);

        /* Done */
        return;
    }
}


/*
 * Traverse the dynamic features stored in dyna_grids and apply their effects.
 */
void process_dynamic_terrain(void)
{
    /* Process the stored dynamic features */
    for (int i = 0; i < dyna_grids.size(); i++)
    {
        /* Get the grid info */
        dynamic_grid_type *g_ptr = &dyna_grids[i];

        /*
         * IMPORTANT: ignore any new dynamic feature created by another
         * dynamic feature in a previous iteration.
         * These new features will be processed in the next call to
         * this function.
         */
        if (g_ptr->new_grid)
        {
            g_ptr->new_grid = FALSE;
            continue;
        }

        /* Active timed terrain */
        /*
         * Note: the effect of a timed feature is always applied,
         * even when the player isn't in line of fire to track the
         * value of the counter properly.
         */
        if (g_ptr->counter > 0)
        {
            /* Decrement the counter */
            --g_ptr->counter;

            /* Do we need to apply the effect? */
            if (g_ptr->counter) continue;
        }
        /* Regular terrain (or inactive timed terrain) */
        /*
         * IMPORTANT: ignore any dynamic feature out of line of fire.
         * Reasons:
         * 1. Avoid possible dynamic-terrain-scum tactics.
         * Example: throwing a fire ball into an oil lake followed by
         * teleportation, etc.
         * 2. Efficiency (only the grids near to the player are
         * processed).
         */
        else if (!player_can_fire_bold(g_ptr->y, g_ptr->x)) continue;

        /* Actually, apply the effect */
        process_dynamic_terrain_aux(g_ptr);
    }
}

/*
 * Return a random index of the given array based on the weights contained in it
 * Each index can have a different weight and bigger values are more probable
 * to be picked
 * Return -1 on error
 */
static int pick_random_item(int chance_values[], int max)
{
    int total_chance = 0;
    int rand_chance;
    int i;

    /* Paranoia */
    if (max < 1) return (-1);

    /* Get the sum of the chances */
    for (i = 0; i < max; i++)
    {
        /* Paranoia */
        if (chance_values[i] < 0) chance_values[i] = 0;

        /* Update the total */
        total_chance += chance_values[i];
    }

    /* Paranoia */
    if (total_chance == 0) return (0);

    /* Get a random chance value */
    rand_chance = rand_int(total_chance);

    /* Reset the counter */
    total_chance = 0;

    /* Get the respective index of that chance */
    for (i = 0; i < max; i++)
    {
        /* Update the total chance again */
        total_chance += chance_values[i];

        /* The random chance is contained in this entry */
        if (rand_chance < total_chance) break;
    }

    /* Paranoia */
    if (i >= max) i = max - 1;

    /* Return the index */
    return (i);
}


#define MAX_RACES 10

/*
 * Randomly select and return a monster race. The race is guaranteed to be one
 * of the most powerful races in the current level.
 * Return 0 if an error occurs
 */
s16b select_powerful_race(void)
{
    int i, j, n = 0;
    s16b r_idx;
    monster_race *r_ptr;
    /* The most powerful races in the level. Note dummy entry at the end */
    s16b races[MAX_RACES + 1];
    /* The weight of each one of these races */
    int rarities[MAX_RACES];
    bool *marked;

    /* Player cannot read clearly */
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE])
    {
        /* Pick a random monster race */
        while (TRUE)
        {
            /* Do it */
            r_idx = randint(z_info->r_max - 1);

            /* Get the race */
            r_ptr = &r_info[r_idx];

            /* Ignore empty races */
            if (!r_ptr->r_speed) continue;

            /* Ignore player ghosts (no name) */
            if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

            /* Found one */
            break;
        }

        /* Done */
        return (r_idx);
    }

    /* No monsters */
    if (mon_cnt == 0) return (0);

    marked = C_ZNEW(z_info->r_max, bool);

    /* Find the most powerful monsters */
    for (i = 0; i < mon_max; i++)
    {
        /* Get the race */
        r_idx = mon_list[i].r_idx;

        /* Ignore dead monsters */
        if (!r_idx) continue;

        /* Find if the race was already stored */
        if (marked[r_idx]) continue;

        marked[r_idx] = TRUE;

        r_ptr = &r_info[r_idx];

        /*
         * Store the race ordering by monster power
         * Note the use of dummy entry at the end to discard weaker races
         */
        for (j = n; j > 0; j--)
        {
            /* Get the race */
            monster_race *r2_ptr = &r_info[races[j-1]];

            /* Monsters are ordered by monster power */
            if (r_ptr->mon_power <= r2_ptr->mon_power) break;

            /* Move the race (perhaps discarding it) */
            races[j] = races[j-1];
        }

        /* Store the race (maybe in the dummy entry, thus discarding it) */
        races[j] = r_idx;

        /* Check bounds and increment the race count if possible */
        if (n < MAX_RACES) ++n;
    }

    FREE_ARRAY(marked);

    /* Paranoia */
    if (n == 0) return (0);

    /* Determine the weight of each race */
    for (i = 0; i < n; i++)
    {
        /* Get the race */
        monster_race *r_ptr = &r_info[races[i]];

        /* Uniques are more "pickable" */
        if (r_ptr->flags1 & (RF1_UNIQUE)) rarities[i] = 5;

        /* Powerful monsters */
        else if (r_ptr->mon_power > mon_power_ave[p_ptr->depth][CREATURE_NON_UNIQUE]) rarities[i] = 2;

        /* Normal monsters */
        else rarities[i] = 1;
    }

    /* Pick a monster race */
    r_idx = races[pick_random_item(rarities, n)];

    /* Done */
    return (r_idx);
}


/*
 * Format a inscription that spoils the presence of a monster of the given race in the level
 * and put it on the given buffer. max is the size of the buffer
 */
QString format_monster_inscription(s16b r_idx)
{
    int i;
    monster_type *m_ptr;
    monster_type m_body;
    monster_race *r_ptr;
    QString inscr;
    QString name;

    /* Empty race or no monster found */
    if (r_idx < 1)
    {
        inscr = ("It doesn't say anything useful.");

        return (inscr);
    }

    /* Find a monster of that race */
    for (i = 0; i < mon_max; i++)
    {
        /* Get the monster */
        m_ptr = &mon_list[i];

        /* Found it? */
        if (m_ptr->r_idx == r_idx) break;
    }

    /* Not found */
    if (i >= mon_max)
    {
        /* Make a fake monster */
        m_ptr = &m_body;

        /* Clear the monster */
        m_ptr->monster_wipe();

        /* Hack -- Set the race */
        m_ptr->r_idx = r_idx;

        /* Hack -- Always visible */
        m_ptr->ml = TRUE;

        /* Hack -- Set some dummy location */
        m_ptr->fy = p_ptr->py;
        m_ptr->fx = p_ptr->px;
    }

    /* Get the race */
    r_ptr = &r_info[r_idx];

    /* Sleeping monsters */
    if (m_ptr->m_timed[MON_TMD_SLEEP])
    {
        /* Get the monster name */
        name = monster_desc(m_ptr, 0x180);

        /* Format */
        if (one_in_(2)) inscr = (QString("Sssshhhh! %1 is asleep!") .arg(name));

        else inscr = (QString("Hurry! before %1 wakes up!") .arg(name));

    }
    /* Special message for dragons */
    else if ((r_ptr->flags3 & (RF3_DRAGON)) && (r_ptr->d_char == 'D') && one_in_(2))
    {
        /* Get the monster name */
        name = monster_desc(m_ptr, 0x180);

        /* Format */
        inscr = (QString("It says: beware of the claws of %1!") .arg(name));
    }
    /* Special message for demons */
    else if ((r_ptr->flags3 & (RF3_DEMON)) && (r_ptr->d_char == 'U') && one_in_(2))
    {
        /* Get the monster name */
        name = monster_desc(m_ptr, 0x188);

        /* Format */
        inscr = (QString("It says: %1 came from hell to hunt you!") .arg(name));
    }
    /* Normal case (1) */
    else if (one_in_(3))
    {
        /* Get the monster name */
        name = name = monster_desc(m_ptr, 0x180);

        /* Format */
        inscr = (QString("Its says: %1 will attack you without mercy!") .arg(name));
    }
    /* Normal case (2) */
    else if (one_in_(2))
    {
        /* Get the monster name */
        name = monster_desc(m_ptr, 0x188);

        /* Format */
        inscr = (QString("It says: %2 guards this dungeon!") .arg(name));
    }
    /* Normal case (3) */
    else
    {
        /* Get the monster name */
        name = monster_desc(m_ptr, 0x188);

        /* Format */
        inscr = (QString("It says: You will be face to face with %3!") .arg(name));
    }

    return (inscr);
}


/*
 * Display a message spoiling the presence of the most powerful monsters in the current level
 * x_idx is the index of the effect that triggered this action
 * IMPORTANT: the given effect can be deleted by this function
 */
void decipher_strange_inscription(int x_idx)
{
    /* Get the effect */
    effect_type *x_ptr = &x_list[x_idx];
    QString name;

    /* Initial message */
    if (!x_ptr->x_r_idx)
    {
        message(QString("You try to decipher the inscription."));
    }
    else
    {
        message(QString("You try to decipher another part of the inscription."));
    }

    /* Hurt the player in rare occasions */
    if ((p_ptr->depth > 10) && one_in_(75))
    {
        /* Get the feature under the effect */
        u16b feat = dungeon_info[x_ptr->x_cur_y][x_ptr->x_cur_x].feature_idx;
        /* Hurt the player for 20% of his/her max HP */
        int dam = p_ptr->mhp / 5;
        int gf_type;

        if (one_in_(2))
        {
            /* Message */
            color_message(QString("It was a chaotic spell!"), TERM_WHITE);

            /* Set type */
            gf_type = GF_CHAOS;
        }
        else
        {
            /* Message */
            color_message(QString("It was a deadly spell!"), TERM_WHITE);

            /* Set type */
            gf_type = GF_NETHER;
        }

        /* Paranoia */
        if (dam < 1) dam = 1;

        /* Get the feature name */
        name = feature_desc(feat, TRUE, TRUE);

        /* Hurt the player */
        project_p(SOURCE_EFFECT, p_ptr->py, p_ptr->px, dam, gf_type, (QString("a spell inscribed on %1") .arg(name)));

        /* Remove effect */
        delete_effect_idx(x_idx);

        /* Done */
        return;
    }

    /* Pick a race */
    x_ptr->x_r_idx = select_powerful_race();

    /* We are printing thrash  */
    if ((p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) && one_in_(2)) message(QString("You don't trust your eyes."));

    /* Format the message */
    name = format_monster_inscription(x_ptr->x_r_idx);

    /* Hack -- Remember that this effect tried to show something without success */
    if (!x_ptr->x_r_idx) x_ptr->x_r_idx = -1;

    /* Show it */
    color_message(name, TERM_WHITE);

    /* Destroy the effect sometimes */
    if ((!p_ptr->is_wizard) && one_in_(3))
    {
        /* Get the effect name */
        name = feature_desc(x_ptr->x_f_idx, FALSE, TRUE);

        /* Message */
        message(QString("The %1 falls apart!") .arg(name));

        /* Remove effect */
        delete_effect_idx(x_idx);
    }
}


/*
 * Hurt the player/aggravate monsters
 * This function can kill the player
 */
void hit_silent_watcher(int y, int x)
{
    u32b flg = PROJECT_PLAY | PROJECT_KILL | PROJECT_STOP;

    /* Calculate damage */
    int dam = (300 * MAX(1, p_ptr->depth)) / 100;

    /* Message */
    color_message(QString("The silent watcher screams and curses at you!"), TERM_WHITE);

    /* Aggravate */
    aggravate_monsters(SOURCE_OTHER);

    /* Fire a bolt to the player  */
    project(SOURCE_OTHER, 0, y, x, p_ptr->py, p_ptr->px, dam, GF_NETHER, flg, 0, 0);

}


/*
 * Certain walls have special behavior when they are touched. This function
 * execute such actions (when do_action is TRUE) or check the presence of such
 * walls (when do_action is FALSE)
 * Return TRUE if an interesting wall was detected in the given location
 */
bool hit_wall(int y, int x, bool do_action)
{
    u16b feat = dungeon_info[y][x].feature_idx;
    u16b dam = f_info[feat].dam_non_native;
    QString name;

    /*We need to remember is the player saw this.*/
    feature_lore *f_l_ptr = &f_l_list[feat];

    /* Feature is dangerous for the player */
    if ((dam > 0) && !is_player_native(y, x))
    {
        QString kb_str;
        int gf_type;

        /* Check player immunity to the feature */
        (void)get_spell_type_from_feature(feat, &gf_type);
        if (is_player_immune(gf_type)) return (FALSE);

        /* Done */
        if (!do_action) return (TRUE);

        /* Get the name */
        name = feature_desc(feat, TRUE, TRUE);

        /* Format the killer string */
        kb_str = (QString("touching %1") .arg(name));

        /* Take the hit */
        take_terrain_hit(dam, feat, kb_str);

        return (TRUE);
    }

    /* Touching silent watchers is dangerous */
    if (feat == FEAT_SILENT_WATCHER)
    {
        /* Done */
        if (!do_action) return (TRUE);

        if (player_can_see_bold(y, x))
        {
            /*Mark the lore*/
            f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
        }

        /* Hurt the player */
        hit_silent_watcher(y, x);

        return (TRUE);
    }

    /* Teleport player */
    if (feat == FEAT_ETHEREAL_WALL)
    {
        /* Done */
        if (!do_action) return (TRUE);

        /* Hurt the player sometimes */
        if (one_in_(10))
        {
            /* Set flags */
            u32b flg = PROJECT_PLAY | PROJECT_GRID | PROJECT_KILL |
                PROJECT_ITEM | PROJECT_BOOM | PROJECT_WALL;

            /* Calculate damage (not centered on player) */
            int dam = (800 * MAX(1, p_ptr->depth)) / 100;

            /* Message */
            color_message(QString("The wall explodes in a burst of light!"), TERM_WHITE);

            /* Remove the wall */
            cave_alter_feat(y, x, FS_TUNNEL);

            /* Burst of light */
            project(SOURCE_OTHER, 2, y, x, y, x, dam, GF_LIGHT, flg, 0, 0);
        }
        /* It works most of the time */
        else
        {
            /* Message */
            message(QString("A blast of bright light teleports you away!"));

            /* Blind the player */
            if (!p_ptr->state.resist_blind && !p_ptr->state.resist_light)
            {
                /* Become blind */
                (void)inc_timed(TMD_BLIND, 10 + randint(10), TRUE);
            }

            /* Teleport */
            teleport_player(100 + rand_int(p_ptr->depth), FALSE);
        }

        return (TRUE);
    }

    /* Process effects */
    if (dungeon_info[y][x].has_effect())
    {
        /* Get the first effect index */
        s16b x_idx = dungeon_info[y][x].effect_idx;

        /* Traverse the effects */
        while (x_idx)
        {
            /* Get the effect */
            effect_type *x_ptr = &x_list[x_idx];

            /* Prepare the next effect */
            x_idx = x_ptr->next_x_idx;

            /* Found an inscription */
            if (x_ptr->x_type == EFFECT_INSCRIPTION)
            {
                /* Done */
                if (!do_action) return (TRUE);

                /* Can't see  */
                if (p_ptr->timed[TMD_BLIND] || no_light())
                {
                    /* Message */
                    message(QString("You can not see!"));
                }
                else
                {
                    /* Reveal monsters */
                    decipher_strange_inscription((int)(x_ptr - x_list));
                }

                /* Done */
                return (TRUE);
            }
        }
    }

    return (FALSE);
}


/*
 * Clear level_flag and then rescan the current level searching for elemental features.
 * Update level_flag for each element found.
 */
void update_level_flag(void)
{
    int y, x;

    /* Reset the global flags */
    level_flag = 0;

    /* Debug */
    if (cheat_room)
    {
        message(QString("Updating level flags."));
        disturb(FALSE, FALSE);
    }

    /* Scan the dungeon */
    for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
    {
        for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
        {
            /* Cache the feature */
            u16b feat = dungeon_info[y][x].feature_idx;

            /* Is it an elemental feature? */
            if (feat_ff3_match(feat, TERRAIN_MASK))
            {
                /* Update the global flags */
                level_flag |= get_level_flag(feat);
            }
        }
    }
}

/*
 * Return ONLY ONE of the LF1_* flags that represents the given feature.
 * Return 0 if there isn't none.
 */
u32b get_level_flag(u16b feat)
{
    /* Get the elemental flags */
    u32b element_flags = feat_ff3_match(feat, TERRAIN_MASK);

    /* Analyze the type of the flags */
    switch (element_flags)
    {
        /* Special case. Boiling mud (hybrid element) */
        case ELEMENT_BMUD:
        {
            /* This flag actually doesn't match any of the ELEMENT_* flags */
            return (LF1_BMUD);
        }
        /* Special case. Boiling water (hybrid element) */
        case ELEMENT_BWATER:
        {
            /* This flag actually doesn't match any of the ELEMENT_* flags */
            return (LF1_BWATER);
        }
        /* Just don't do anything for other flags */
        default:
        {
            return (element_flags);
        }

    }
}

/*
 * Return ALL the LF1_* flags that represents the nativity settings of the
 * given monster race.
 * Return 0 if there isn't none.
 */
u32b get_level_flag_from_race(monster_race *r_ptr)
{
    /* Get the native flags */
    u32b element_flags = (r_ptr->r_native & (TERRAIN_MASK));

    /* Special case. Boiling mud (hybrid element) */
    if ((element_flags & ELEMENT_BMUD) == (ELEMENT_BMUD))
    {
        /* Just add the pseudo flag */
        /* Note that LF1_LAVA and LF1_MUD still are in the flags */
        element_flags |= LF1_BMUD;
    }

    /* Special case. Boiling mud (hybrid element) */
    if ((element_flags & ELEMENT_BWATER) == (ELEMENT_BWATER))
    {
        /* Just add the pseudo flag */
        /* Note that LF1_LAVA and LF1_WATER still are in the flags */
        element_flags |= LF1_BWATER;
    }

    /* Done */
    return (element_flags);
}



/*
 * Paste the name of the element given in flag into buf.
 * max is the maximum size of buf.
 * flag must contain ONLY ONE of the LF1_* flags.
 */
QString describe_one_level_flag(u32b flag)
{
    /* Default name */
    QString name = "unknown";

    /* Analyze the flag */
    switch (flag)
    {
        case LF1_FIRE: name = "fire"; break;
        case LF1_ACID: name = "acid"; break;
        case LF1_WATER: name = "water"; break;
        case LF1_MUD: name = "mud"; break;
        case LF1_LAVA: name = "lava"; break;
        case LF1_ICE: name = "ice"; break;
        case LF1_FOREST: name = "forest"; break;
        case LF1_OIL: name = "oil"; break;
        case LF1_SAND: name = "sand"; break;
        case LF1_BWATER: name = "boiling water"; break;
        case LF1_BMUD: name = "boiling mud"; break;
    }

    /* Copy the name */
    return (name);
}


/*
 * Show several messages describing all the LF1_* flags contained in the
 * given set of flags
 */
void debug_all_level_flags(u32b all_flags)
{
    int i;
    u32b flag = 1;

    /* Parse the bits of the given flags */
    for (i = 0; i < 32; i++, flag <<= 1)
    {
        /* The current flags is present in the set? */
        if (all_flags & flag)
        {
            /* Get the name */
            QString buf = describe_one_level_flag(flag);

            /* Message */
            message(QString("The %1 flag is present.") .arg(buf));
        }
    }
}

