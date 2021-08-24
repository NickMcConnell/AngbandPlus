
/* File: was melee2.c */

/*
 * Copyright (c) 2001 Leon Marrick & Bahman Rabii, Ben Harrison,
 * James E. Wilson, Robert A. Koeneke, Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"



/*
 * Terrified monsters will turn to fight if they are slower than the
 * character, and closer to him than this distance.
 */
#define TURN_RANGE      3

/* Build a structure to hold movement data */
typedef struct move_data move_data;
struct move_data
{
    int move_chance;
    bool move_bash;
};


/*
 * Calculate minimum and desired combat ranges.  -BR-
 */
void find_range(monster_type *m_ptr)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u16b m_chp, m_mhp;
    u32b p_val, m_val;

    /* All "afraid" monsters will run away */
    if ((m_ptr->m_timed[MON_TMD_FEAR]) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0))
    {
        m_ptr->min_range = FLEE_RANGE;
    }

    /* Some monsters run when low on mana */
    else if ((r_ptr->flags2 & (RF2_LOW_MANA_RUN)) &&
        (m_ptr->mana < r_ptr->mana / 6)) m_ptr->min_range = FLEE_RANGE;

    /* Hack -- townsmen go about their business */
    else if (m_ptr->mflag & (MFLAG_TOWN)) m_ptr->min_range = 1;

    /*stupid monsters always charge*/
    else if (r_ptr->flags2 & (RF2_STUPID)) m_ptr->min_range = 1;

    /* Breeders cannot be terrified */
    else if (r_ptr->flags2 & (RF2_MULTIPLY)) m_ptr->min_range = 1;

    /* Allow monster terror */
    else
    {
        /* Minimum distance - stay at least this far if possible */
        m_ptr->min_range = 1;

        /* Examine player power  */
        p_lev = p_ptr->lev;

        /* Examine monster power (level plus morale) */
        m_lev = r_ptr->level +
            (dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx % 10) + 25;

        /* Optimize extreme cases below */
        if (m_lev < p_lev + 4) m_ptr->min_range = FLEE_RANGE;
        else if (m_lev + 3 < p_lev)
        {
            /* Examine player health */
            p_chp = p_ptr->chp;
            p_mhp = p_ptr->mhp;

            /* Examine monster health */
            m_chp = m_ptr->hp;
            m_mhp = m_ptr->maxhp;

            /* Prepare to optimize the calculation */
            p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
            m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */

            /* Strong players scare strong monsters */
            if (p_val * m_mhp > m_val * p_mhp)
                m_ptr->min_range = FLEE_RANGE;
        }
    }

    /* Handle range greater than FLEE Range */
    if (m_ptr->min_range > FLEE_RANGE) m_ptr->min_range = FLEE_RANGE;

    /* Nearby monsters that cannot run away will stand and fight */
    if ((m_ptr->cdis < TURN_RANGE) && (m_ptr->m_speed < p_ptr->state.p_speed))
        m_ptr->min_range = 1;

    /* Now find preferred range */
    m_ptr->best_range = m_ptr->min_range;

    if (r_ptr->freq_ranged > 24)
    {
        /* Heavy spell casters will sit back and cast */
        if (m_ptr->mana > r_ptr->mana / 5) m_ptr->best_range = 6;

        /* Creatures that don't move never like to get too close */
        else if (r_ptr->flags1 & (RF1_NEVER_MOVE)) m_ptr->best_range = 6;

        /* Spellcasters that don't strike never like to get too close */
        else if (r_ptr->flags1 & (RF1_NEVER_BLOW)) m_ptr->best_range = 8;

        /*Monsters who have had unfair attacks happen to them charge or cast */
        else if (m_ptr->mflag & (MFLAG_ATTACKED_BAD))
        {
            m_ptr->min_range = 1;
        }

        /* Breathers like point blank range */
        else if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
             (r_ptr->flags5 & (RF5_BREATH_MASK)) ||
             (r_ptr->flags6 & (RF6_BREATH_MASK)) ||
             (r_ptr->flags7 & (RF7_BREATH_MASK))) &&
            (m_ptr->best_range < 6) &&
            (m_ptr->hp > m_ptr->maxhp / 2))
        {
            m_ptr->best_range = 7;
        }
    }

}

static void find_best_flow(monster_type *m_ptr)
{
    int lowest_cost = BASE_FLOW_MAX;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int y = m_ptr->fy;
    int x = m_ptr->fx;

    bool allow_doors = TRUE;

    /* Remember if the monster cannot handle doors */
    if (MONSTER_HATES_DOORS(r_ptr)) allow_doors = FALSE;

    /*Can the monster pass through walls?*/
    if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL))
    {
        /*Is there a flow here?*/
        if (cave_cost[FLOW_PASS_WALLS][y][x])
        {
            /*
             * Is it the best route?
             */
            if (cave_cost[FLOW_PASS_WALLS][y][x] <= lowest_cost)
            {

                lowest_cost = cave_cost[FLOW_PASS_WALLS][y][x];

                /*Mark which flow we are using */
                m_ptr->using_flow = FLOW_PASS_WALLS;

            }
        }

        /*Why didn't we use this flow?*/
        if(m_ptr->using_flow != FLOW_PASS_WALLS)
        {
            /*The flow is not active*/
            if (cost_at_center[FLOW_PASS_WALLS] == 0)
            {
                /* Full update of the flows, which should activate the FLOW_PASS_WALLS */
                p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
            }
        }
    }

    /*Can the monster fly?*/
    if (r_ptr->flags3 & (RF3_FLYING))
    {
        int flow;

        /* Pick the proper version of the FLYING flow */
        if (allow_doors) flow = FLOW_FLYING;
        else flow = FLOW_FLYING_NO_DOORS;

        /*Is there a flow here?*/
        if (cave_cost[flow][y][x])
        {
            /*
             * Is it the best route?
             */
            if (cave_cost[flow][y][x] <= lowest_cost)
            {
                lowest_cost = cave_cost[flow][y][x];

                /*Mark which flow we are using */
                m_ptr->using_flow = flow;

            }
        }

        /* Why wouldn't a flying creature use this flow, unless they are using the FLOW_PASS_WALLS flow. */
        if((m_ptr->using_flow != flow) && (m_ptr->using_flow != FLOW_PASS_WALLS))
        {
            /*The flow is not active*/
            if (cost_at_center[flow] == 0)
            {
                /* Full update of the flows, which should activate the FLOW_FLYING */
                p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
            }
        }

    }

    /*
     * Can the monster pass doors?
     */
    if (r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR))
    {
        /*Is the best flow*/
        if (cave_cost[FLOW_PASS_DOORS][y][x])
        {
            if (cave_cost[FLOW_PASS_DOORS][y][x] < lowest_cost)
            {

                lowest_cost = cave_cost[FLOW_PASS_DOORS][y][x];

                /*Mark which flow we are using */
                m_ptr->using_flow = FLOW_PASS_DOORS;
            }
        }
    }

    /*FLOW_NO_DOORS will never be quicker than FLOW_PASS_DOORS.*/
    else if (cave_cost[FLOW_NO_DOORS][y][x])
    {
        /*Is it the best route?*/
        if (cave_cost[FLOW_NO_DOORS][y][x] <= lowest_cost)
        {
            lowest_cost = cave_cost[FLOW_NO_DOORS][y][x];

            /*Mark which flow we are using */
            m_ptr->using_flow = FLOW_NO_DOORS;
        }
    }

    /*Is the monster native to any terrains?*/
    if (r_ptr->r_native & (TERRAIN_MASK))
    {
        int j;

        u32b which_elem_flow = 1;

        int base, tail;

        /* Pick the proper versions of the elemental flows */
        if (allow_doors)
        {
            base = ELEM_FLOW_BASE;
            tail = ELEM_FLOW_TAIL;
        }
        /* Doorless flows */
        else
        {
            base = ELEM_FLOW_BASE_NO_DOORS;
            tail = ELEM_FLOW_TAIL_NO_DOORS;
        }

        for (j = base; j <= tail; j++, which_elem_flow <<= 1)
        {
            /*Is the monster native to this terrain*/
            if (r_ptr->r_native & which_elem_flow)
            {
                /*Is there a flow here?*/
                if (cave_cost[j][y][x])
                {
                    /*Is it the best route?*/
                    if (cave_cost[j][y][x] < lowest_cost)
                    {
                        /*Mark which flow we are using */
                        m_ptr->using_flow = j;

                        lowest_cost = cave_cost[j][y][x];
                    }
                }
            }
        }
    }

    /*Monster is flying*/
    if ((m_ptr->using_flow == FLOW_FLYING) ||
        (m_ptr->using_flow == FLOW_FLYING_NO_DOORS)) m_ptr->mflag |= (MFLAG_FLYING);

    /* Flying monster is standing on dangerous terrain. Make it fly */
    else if ((r_ptr->flags3 & (RF3_FLYING)) &&
                !cave_no_dam_for_mon(y, x, r_ptr)) m_ptr->mflag |= (MFLAG_FLYING);

    /*Not flying*/
    else m_ptr->mflag &= ~(MFLAG_FLYING);
}

/*
 * Check the effect of the Rogue's monster trap.  Certain traps may be avoided by
 * certain monsters.  Traps may be disarmed by smart monsters.
 * If the trap works, the effects vary depending on trap type. -BR-
 *
 * "death" tells the calling function if the monster is killed by the trap.
 *
 * Note that the variables y and x can only be used for a real trap.
 * In MODE_DESCRIBE - monster type will be undefined
 */
QString apply_monster_trap(int f_idx, int y, int x, byte mode)
{
    QString output;
    output.clear();

    monster_type *m_ptr = NULL;
    monster_race *r_ptr = NULL;
    monster_lore *l_ptr = NULL;
    feature_lore *f_l_ptr = &f_l_list[f_idx];

    int dis_chance = 0, trap_skill = 0;

    /* Assume monster not frightened by trap */
    bool fear = FALSE;

    /*extra damage if monster fails to disarm trap*/
    bool fail_disarm = FALSE;

    /* Assume the trap works */
    bool trap_hit = TRUE;

    /* Assume trap is not destroyed */
    bool trap_destroyed = FALSE;

    QString m_name;

    int m_idx = 0;

    /* Sanity check */
    if (!cave_monster_trap_bold(y,x) && (mode == MODE_ACTION)) return (output);

    if (mode == MODE_ACTION)
    {

        m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        r_ptr = &r_info[m_ptr->r_idx];
        l_ptr = &l_list[m_ptr->r_idx];
        m_idx = m_ptr->get_mon_idx();
    }

    /*Count in the feature lore the number of times set off*/
    if ((mode == MODE_ACTION) && (f_l_ptr->f_l_power < UCHAR_MAX))
    {
        f_l_ptr->f_l_power++;
    }

    /*Skip all this unless we are setting off a trap*/
    if (mode == MODE_ACTION)
    {

        /* Get "the monster" or "it" */
        m_name = monster_desc(m_ptr, 0);

        /* Evasive monsters can usually avoid traps entirely. */
        if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!one_in_(3)))
        {
            if (m_ptr->ml)
            {
                /* Message */
                message(QString("%1 dodges your trap.") .arg(capitalize_first(m_name)));

                /* Note that monster is evasive */
                l_ptr->r_l_flags2 |= (RF2_EVASIVE);
            }

            return (output);
        }

        /* Lightning traps affect all but ghosts */
        if (f_idx == FEAT_MTRAP_LIGHTNING)
        {
            if (r_ptr->flags2 & (RF2_PASS_WALL))
            {
                if (m_ptr->ml) message(QString("%1 passes through your trap.") .arg(capitalize_first(m_name)));

                trap_hit = FALSE;
            }
        }

        /* Other traps seldom affect ghosts. */
        else if ((r_ptr->flags2 & (RF2_PASS_WALL)) &&
              (rand_int(4) != 1) && (mode == MODE_ACTION))
        {
            if (m_ptr->ml) message(QString("%1 passes through your trap.") .arg(capitalize_first(m_name)));
            trap_hit = FALSE;
        }

        /* Find the monsters base skill at disarming */
        dis_chance = 40 + (2 * r_ptr->level);

        /*Hack - these traps are harder to disarm*/
        if (f_idx == FEAT_MTRAP_PORTAL) dis_chance /= 2;

        /*wary or smart creatures much more likely to disarm the trap*/
        if (r_ptr->flags2 & (RF2_SMART)) dis_chance *= 2;
        if (m_ptr->mflag & (MFLAG_WARY)) dis_chance *= 2;

        trap_skill = (p_ptr->state.skills[SKILL_DISARM] + p_ptr->lev - 15) / 2;
    }

    /*In decribe mode, we aren't hitting any trap*/
    if (mode == MODE_DESCRIBE) trap_hit = FALSE;

    /* Monsters may attempts to disarm traps which would affect them,
     * but occasionally set it off
     */
    if (trap_hit)
    {
        if ((r_ptr->flags2 & (RF2_SMART)) && (randint(dis_chance) > trap_skill))
        {
            /*accidentally blows it up*/
            if (one_in_(dis_chance / 15))
            {
                if (m_ptr->ml)
                {
                    message(QString("%1 tries to disarm your trap, but sets it off!") .arg(capitalize_first(m_name)));
                }

                /* worked */
                trap_hit = TRUE;

                /*monster will get extra damage because trap was right in their face.*/
                fail_disarm = TRUE;

            }

            /*succeeds in disarming*/
            else
            {
                if (m_ptr->ml)
                {
                    message(QString("%1 finds your trap and disarms it.") .arg(capitalize_first(m_name)));
                    }

                /* Trap is gone */
                trap_destroyed = TRUE;

                /* Didn't work */
                trap_hit = FALSE;

            }
        }

        /* Monsters can be wary of traps */
        else if ((m_ptr->mflag & (MFLAG_WARY)) || (r_ptr->flags2 & (RF2_SMART)))
        {
            /* Check for avoidance */
            if (randint(dis_chance) > (trap_skill))
            {
                if (m_ptr->ml)
                {
                    message(QString("%1 avoids your trap.") .arg(capitalize_first(m_name)));
                }

                /* Didn't work */
                trap_hit = FALSE;
            }
        }
    }

    /* I thought traps only affected players!  Unfair! */
    if ((trap_hit) || (mode == MODE_DESCRIBE))
    {
        /* Assume a default death */
        QString note_dies = " dies.";

        int n, trap_power = 0;

        int sturdy_break = 9;
        int reg_break = 3;

        if (trap_hit)
        {
            QString contains("Evg");

            /* Some monsters get "destroyed" */
            if ((r_ptr->flags3 & (RF3_DEMON)) ||
                (r_ptr->flags3 & (RF3_UNDEAD)) ||
                (r_ptr->flags2 & (RF2_STUPID)) ||
                (contains.contains(r_ptr->d_char)))
            {
                /* Special note at death */
                note_dies = " is destroyed.";
            }

            /* Players sees the monster, butfailed to disarm got it's own message */
            if ((m_ptr->ml) && (!(fail_disarm))) message(QString("%1 sets off your cunning trap!") .arg(capitalize_first(m_name)));

            /* Not seen but in line of sight */
            else if ((player_has_los_bold(y, x)) && (!(fail_disarm)))
                message(QString("Something sets off your cunning trap!"));

            /* Monster is not seen or in LOS */
            else
            {
                /* HACK - no message for non-damaging traps */
                if ((!(f_idx == FEAT_MTRAP_CONFUSION)) && (!(f_idx == FEAT_MTRAP_SLOWING)) &&
                    (!(f_idx == FEAT_MTRAP_PORTAL)))
                {
                    message(QString("You hear anguished yells in the distance."));
                }
            }

            /* Explosion traps are always destroyed. */
            if (f_idx == FEAT_MTRAP_EXPLOSIVE)
            {
                trap_destroyed = TRUE;
            }

            /* Some traps are rarely destroyed */
            else if (f_idx == FEAT_MTRAP_STURDY)
            {
                if (one_in_(sturdy_break)) trap_destroyed = TRUE;
            }

            /* Most traps are destroyed 1 time in 3 */
            else if (one_in_(reg_break)) trap_destroyed = TRUE;

            /* Find the 'power' of the trap effect */
            n = p_ptr->lev + ((p_ptr->lev * p_ptr->lev)/ 12);
            trap_power = 3 + randint(n) + n;

            /*the monster who fails to disarm gets a full blast at point-blank range*/
            if (fail_disarm) trap_power *=3;

            /* Monsters can be wary of traps */
            else if (m_ptr->mflag & (MFLAG_WARY)) trap_power /= 3;

            /* Trap 'critical' based on disarming skill (if not wary) */
            else if (randint(trap_skill) > (randint(dis_chance + (r_ptr->level * 2))))
            {
                trap_power += trap_power / 3;
            }
        }

        /* Affect the monster, or describe the trap. */
        switch (f_idx)
        {
            /* Sturdy trap gives 33% of damage (normal traps give half) */
            case FEAT_MTRAP_STURDY:
            {
                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will cause a moderate amount of damage any creature who walks into it."));
                    output.append(QString("  This trap is %1 times more sturdy than all other monster traps.") .arg(sturdy_break / reg_break));
                    break;
                }

                if (mode == MODE_ACTION)
                {

                    mon_take_hit(dungeon_info[y][x].monster_idx, (trap_power / 3), &fear, note_dies, SOURCE_PLAYER, TRUE);

                }
                break;
            }

            /* Confusion trap */
            case  FEAT_MTRAP_CONFUSION:
            {
                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will attempt to confuse any creature who walks into it."));
                    break;
                }

                if (mode == MODE_ACTION)
                {
                    int tmp = rand_int((3 * trap_power) / 2) - r_ptr->level - 10;

                    if (tmp < 0)
                    {
                        if (m_ptr->ml) message(QString("%1 is unaffected!") .arg(capitalize_first(m_name)));
                    }
                    else mon_inc_timed(m_idx, MON_TMD_CONF, 4 + tmp, MON_TMD_FLG_NOTIFY);
                }

                break;
            }

            /* Slow the monster */
            case FEAT_MTRAP_SLOWING:
            {

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will attempt to slow any creature who walks into it."));
                    break;
                }

                if (mode == MODE_ACTION)
                {
                    int tmp = rand_int((3 * trap_power) / 2) - r_ptr->level - 10;

                    if (tmp < 0)
                    {
                        if (m_ptr->ml) message(QString("%1 is unaffected!") .arg(capitalize_first(m_name)));
                    }

                    /* set or add to slow counter */
                    else mon_inc_timed(m_idx, MON_TMD_SLOW, 4 + tmp, MON_TMD_FLG_NOTIFY);
                }

                break;
            }

            /* Slow the monster */
            case FEAT_MTRAP_DRAIN_LIFE:
            {
                byte rad = 3;

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will set off an explosion that will damage"));
                    output.append(QString(" any living creature within %1 squares that is in line of sight of the blast.") .arg(rad));
                    break;
                }

                if (mode == MODE_ACTION)
                {


                    /*ball of drain life*/
                    (void)explosion(SOURCE_PLAYER, rad, y, x, (3 * trap_power) / 4, GF_LIFE_DRAIN, PROJECT_KILL);

                    break;
                }
            }

            case FEAT_MTRAP_POISON_GAS:
            {
                int rad = 3;

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will set off an explosion of poison gas that will affect"));
                    output.append(QString(" any creature within %1 squares that is in line of sight of the blast.") .arg(rad));
                    break;
                }

                if (mode == MODE_ACTION)
                {


                    /*ball of poison*/
                    (void)explosion(SOURCE_PLAYER, rad, y, x, (4 * trap_power) / 3, GF_POIS, (PROJECT_KILL | PROJECT_PLAY));

                }

                break;
            }

            case FEAT_MTRAP_LIGHTNING:
            {
                int rad = 3;

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will set off an explosion of electricity that will affect"));
                    output.append(QString(" any creature within %1 squares that is in line of sight of the blast.") .arg(rad));
                    break;
                }

                if (mode == MODE_ACTION)
                {

                    /*ball of electricity*/

                    (void)explosion(SOURCE_PLAYER, rad, y, x, (7 * trap_power) /8 , GF_ELEC, PROJECT_KILL);
                }

                break;
            }

            case FEAT_MTRAP_EXPLOSIVE:
            {
                int rad = 3;

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will set off an explosion of plamsa, followed by an"));
                    output.append(QString(" explosion of shards, that will affect "));
                    output.append(QString(" any creature within %1 squares that is in line of sight of the blast.") .arg(rad));
                    break;
                }

                if (mode == MODE_ACTION)
                {

                    /*explosion of fire*/
                    (void)explosion(SOURCE_PLAYER, 3, y, x, trap_power, GF_PLASMA, PROJECT_KILL);

                    /*followed by shards*/
                    (void)explosion(SOURCE_PLAYER, 3, y, x, trap_power, GF_SHARD, PROJECT_KILL);

                }

                break;
            }

            case FEAT_MTRAP_PORTAL:
            {
                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will teleport any creature who walks into it."));
                    break;
                }

                if (mode == MODE_ACTION)
                {
                    /*teleport the monster*/
                    if (teleport_away(dungeon_info[y][x].monster_idx, 5 + (trap_power / 10)))
                    {
                        /*give message if in LOS*/
                        if (m_ptr->ml) message(QString("%1 is teleported.") .arg(capitalize_first(m_name)));
                    }
                }

                break;
            }

            /* Dispel Monsters Trap */
            case FEAT_MTRAP_DISPEL_MONSTERS:
            {

                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will damage any creature within"));
                    output.append(QString(" line of sight when the trap is set off."));
                    break;
                }

                if (mode == MODE_ACTION)
                {

                    /*100% - 200% damage of trap power to all creatures within LOS of trap*/
                    int dam = (trap_power + randint(trap_power) + randint(trap_power / 2));

                    /* Damage the target monster */
                    (void)project_los(y, x, dam, GF_DISP_ALL);
                }

                break;
            }

            /* Default to the basic trap - half damage */
            default:
            {
                if (mode == MODE_DESCRIBE)
                {
                    output.append(QString("  This monster trap will damage any creature who walks into it."));
                    break;
                }

                if (mode == MODE_ACTION)
                {

                    (void)(mon_take_hit(dungeon_info[y][x].monster_idx, trap_power / 2, &fear,
                                note_dies, SOURCE_PLAYER, TRUE));
                }

                break;
            }
        }

        if (mode == MODE_DESCRIBE)
        {
            /* HACK - no message for non-damaging traps */
            if ((!(f_idx == FEAT_MTRAP_CONFUSION)) && (!(f_idx == FEAT_MTRAP_SLOWING)) &&
                    (!(f_idx == FEAT_MTRAP_PORTAL)))
            {
                output.append(QString("  The amount of damage caused by this trap increases as the player gains levels."));
            }
            return (output);
        }

        /*make the monsters who saw wary*/
        else (void)project_los(y, x, 0, GF_MAKE_WARY);

    }

    if (trap_destroyed)
    {
        message(QString("The trap has been destroyed."));

        /* Destroy the trap */
        delete_effect_idx(dungeon_info[y][x].effect_idx);

        /* Redraw the spot */
        light_spot(y, x);

        /*one less trap on level*/
        num_trap_on_level--;

        /* Stop resting */
        disturb(FALSE, TRUE);
    }

    /* Return */
    return(output);
}

/*
 * Given a central direction at position [dir #][0], return a series
 * of directions radiating out on both sides from the central direction
 * all the way back to its rear.
 *
 * Side directions come in pairs; for example, directions '1' and '3'
 * flank direction '2'.  The code should know which side to consider
 * first.  If the left, it must add 10 to the central direction to
 * access the second part of the table.
 */
static byte side_dirs[20][8] =
{
    { 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias right */
    { 1, 4, 2, 7, 3, 8, 6, 9 },
    { 2, 1, 3, 4, 6, 7, 9, 8 },
    { 3, 2, 6, 1, 9, 4, 8, 7 },
    { 4, 7, 1, 8, 2, 9, 3, 6 },
    { 5, 5, 5, 5, 5, 5, 5, 5 },
    { 6, 3, 9, 2, 8, 1, 7, 4 },
    { 7, 8, 4, 9, 1, 6, 2, 3 },
    { 8, 9, 7, 6, 4, 3, 1, 2 },
    { 9, 6, 8, 3, 7, 2, 4, 1 },

    { 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias left */
    { 1, 2, 4, 3, 7, 6, 8, 9 },
    { 2, 3, 1, 6, 4, 9, 7, 8 },
    { 3, 6, 2, 9, 1, 8, 4, 7 },
    { 4, 1, 7, 2, 8, 3, 9, 6 },
    { 5, 5, 5, 5, 5, 5, 5, 5 },
    { 6, 9, 3, 8, 2, 7, 1, 4 },
    { 7, 4, 8, 1, 9, 2, 6, 3 },
    { 8, 7, 9, 4, 6, 1, 3, 2 },
    { 9, 8, 6, 7, 3, 4, 2, 1 }
};


/*
 * Can the monster enter this grid?  How easy is it for them to do so?
 *
 * The code that uses this function sometimes assumes that it will never
 * return a value greater than 100.
 *
 * The usage of exp to determine whether one monster can kill another is
 * a kludge.  Maybe use HPs, plus a big bonus for acidic monsters
 * against monsters that don't like acid.
 *
 * The usage of exp to determine whether one monster can push past
 * another is also a tad iffy, but ensures that black orcs can always
 * push past other black orcs.
 */
int cave_passable_mon(monster_type *m_ptr, int y, int x, bool *bash)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Assume nothing in the grid other than the terrain hinders movement */
    int move_chance = 100;

    u16b feat;

    bool is_native;

    int unlock_chance = 0;
    int bash_chance = 0;

    /* Check Bounds */
    if (!in_bounds(y, x)) return (0);

    /* Check nativity */
    is_native = is_monster_native(y, x, r_ptr);

    /* Check location */
    feat = dungeon_info[y][x].feature_idx;

    /*
     * Don't move through permanent walls.
     * Passable permanent features are allowed (stairs)
     */
    if (feat_ff1_match(feat, FF1_PERMANENT | FF1_MOVE) == (FF1_PERMANENT))
    {
        return (0);
    }

    /* The grid is occupied by the player. */
    if (dungeon_info[y][x].monster_idx < 0)
    {
        /* Monster has no melee blows - character's grid is off-limits. */
        if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (0);

        /* Any monster with melee blows can attack the character. */
        else move_chance = 100;
    }

    /* The grid is occupied by a monster. */
    else if ((dungeon_info[y][x].monster_idx > 0) && (is_native))
    {
        monster_type *n_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *nr_ptr = &r_info[n_ptr->r_idx];

        /* Move weaker monsters */
        if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
            (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
            (r_ptr->mexp > nr_ptr->mexp))
        {
            move_chance = 100;
        }

        /* Push past hidden monsters */
        else if ((m_ptr->mflag & (MFLAG_HIDE)) ||
            (n_ptr->mflag & (MFLAG_HIDE)))
        {
            move_chance = 80;
        }

        /* Push past weaker or similar monsters */
        else if (r_ptr->mexp >= nr_ptr->mexp)
        {
            /* It's easier to push past weaker monsters */
            if (r_ptr->mexp == nr_ptr->mexp) move_chance = 40;
            else move_chance = 80;
        }

        /* Cannot do anything to clear away the other monster */
        else return (0);
    }

    /* Glyphs */
    else if (cave_player_glyph_bold(y, x))
    {
        int glyph_chance = 100 * r_ptr->level / BREAK_GLYPH;

        /* Glyphs are hard to break */
        if (move_chance > glyph_chance)
        {
             move_chance = glyph_chance;

        }
    }

    /* Monster is flying*/
    else if (MONSTER_CAN_FLY(m_ptr, feat))
    {
        move_chance = 100;
    }

    /* Is the monster native to that square */
    else if (!is_native)
    {
        /* confused monsters or stunned monsters don't know any better */
        if (m_ptr->m_timed[MON_TMD_CONF]) move_chance = 100;
        else if ((m_ptr->m_timed[MON_TMD_STUN]) && (one_in_(10))) move_chance = 100;

        /* Will monster be significantly damaged by going there? */
        else if ((f_info[feat].dam_non_native) > (m_ptr->hp / 15)) return (0);

        /*desirability of the move is based on energy to go in square*/
        else move_chance = 10000 / f_info[feat].non_native_energy_move;

        /*Never more than 100*/
        if (move_chance > 100) move_chance = 100;

    }

    /*Monster is native - movement based on 100 energy average*/
    else
    {
        move_chance = 10000 / f_info[feat].native_energy_move;

        /*Never more than 100*/
        if (move_chance > 100) move_chance = 100;
    }

    /*no further analysis for monsters who pass or kill walls*/
    if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
        (r_ptr->flags2 & (RF2_KILL_WALL)))
    {
        return (move_chance);
    }


    /*** Check passability of various features. ***/

    /* Feature is passable */
    if (cave_passable_bold(y, x))
    {
        /*
         * Any monster can handle floors, except glyphs,
         * which are handled above
         */
        return (move_chance);
    }

    /* An effect is disabling movement */
    else if (feat_ff1_match(feat, FF1_MOVE))
    {
        /* Reject grid */
        return (0);
    }

    /* Feature is a wall but the monster can fly over it */
    else if (MONSTER_CAN_FLY(m_ptr, feat))
    {
        return (move_chance);
    }

    /* Glyphs */
    if (cave_player_glyph_bold(y, x))
    {
        /* Glyphs are hard to break */
        return (move_chance);
    }

    /* Monster can open doors and the door isn't jammed */
    if (feat_ff1_match(feat, FF1_SECRET | FF1_DOOR) ==
            (FF1_SECRET | FF1_DOOR))
    {
        /* Discover the secret (temporarily) */
        feat = feat_state(feat, FS_SECRET);
    }

    /* Monster can open doors */
    if ((r_ptr->flags2 & (RF2_OPEN_DOOR)) &&
        feat_ff1_match(feat, FF1_CAN_OPEN) &&
        !feat_ff3_match(feat, FF3_DOOR_JAMMED))
    {
        int open_power = feat_state_power(feat, FS_OPEN);

        /* Secret doors and easily opened stuff */
        if (open_power == 0)
        {
            /*
             * Note:  This section will have to be rewritten if
             * secret doors can be jammed or locked as well.
             */

            /*
             * It usually takes two turns to open a door
             * and move into the doorway.
             */
            return (MIN(50, move_chance));
        }

        /*
         * Locked doors (not jammed).  Monsters know how hard
         * doors in their neighborhood are to unlock.
         */
        else
        {
            int lock_power, ability;
            QString contains = "ph";

            /* Door power (from 35 to 245) */
            lock_power = 35 * open_power;

            /* Calculate unlocking ability (usu. 11 to 200) */
            ability = r_ptr->level + 10;
            if (r_ptr->flags2 & (RF2_SMART)) ability *= 2;
            if (contains.contains(r_ptr->d_char))
                ability = 3 * ability / 2;

            /*
             * Chance varies from 5% to over 100%.  XXX XXX --
             * we ignore the fact that it takes extra time to
             * open the door and walk into the entranceway.
             */
            unlock_chance = (MAX(5, (100 * ability / lock_power)));
        }
    }

    /* Monster can bash doors */
    if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&
        feat_ff1_match(feat, FF1_CAN_BASH))
    {
        int door_power, bashing_power;

        /* Door power (from 60 to 420) */
        /*
         * XXX - just because a door is difficult to unlock
         * shouldn't mean that it's hard to bash.  Until the
         * character door bashing code is changed, however,
         * we'll stick with this.
         */
        door_power = 60 + 60 * feat_state_power(feat, FS_BASH);

        /*
         * Calculate bashing ability (usu. 21 to 300).  Note:
         * This formula assumes Oangband-style HPs.
         */
        bashing_power = 20 + r_ptr->level + m_ptr->hp / 15;

        if ((r_ptr->flags3 & (RF3_GIANT)) || (r_ptr->flags3 & (RF3_TROLL)))
            bashing_power = 3 * bashing_power / 2;

        /*
         * Chance varies from 2% to over 100%.  Note that
         * monsters "fall" into the entranceway in the same
         * turn that they bash the door down.
         */
        bash_chance = (MAX(2, (100 * bashing_power / door_power)));
    }

    /*
     * A monster cannot both bash and unlock a door in the same
     * turn.  It needs to pick one of the two methods to use.
     */
    if (unlock_chance > bash_chance) *bash = FALSE;
    else *bash = TRUE;

    return MIN(move_chance, (MAX(unlock_chance, bash_chance)));

}



/*Returns true if the monster is holding a quest artifact, false if they aren't.*/
static bool holding_quest_artifact(const monster_type *m_ptr)
{
    s16b this_o_idx, next_o_idx = 0;

    /* Search items being carried */
    for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /*Monster is holding a quest item*/
        if(o_ptr->is_quest_object() && o_ptr->art_num) return (TRUE);
    }

    /*didn't find one*/
    return (FALSE);
}

/*
 * Primary function for monsters that want to advance toward the character.
 * Assumes that the monster isn't frightened.
 *
 * Ghosts and rock-eaters do not use flow information, because they
 * can - in general - move directly towards the character.  We could make
 * them look for a grid at their preferred range, but the character
 * would then be able to avoid them better (it might also be a little
 * hard on those poor warriors...).
 *
 * Other monsters will use target information, then their ears, then their
 * noses (if they can), and advance blindly if nothing else works.
 *
 * When flowing, monsters prefer non-diagonal directions.  Monsters also
 * prefer squares that don't have monsters in them.
 *
 */
static void get_move_advance(monster_type *m_ptr, int *ty, int *tx)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int i;

    byte y, x, y1, x1;

    byte flow_used;

    bool wants_shot = FALSE;

    /* Start with the player's current location*/
    int lowest_cost = BASE_FLOW_MAX;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Monster location */
    y1 = m_ptr->fy;
    x1 = m_ptr->fx;

    /*
     * Monster can go through rocks should always try to head for the player, unless they
     * are obstructed.
     */
    if ((((r_ptr->flags2 & (RF2_PASS_WALL)) ||
        (r_ptr->flags2 & (RF2_KILL_WALL)))) &&
        (!(m_ptr->mflag & (MFLAG_NEED_PASS_WALL_FLOW))))
    {
        bool dummy;
        bool passable = FALSE;

        u32b distance_squared_now = (GET_SQUARE((py - y1)) + GET_SQUARE((px - x1)));

        /* Check for impassable terrain */
        for (i = 0; i < 8; i++)
        {
            u32b distance_squared_new;

            y = m_ptr->fy + ddy_ddd[i];
            x = m_ptr->fx + ddx_ddd[i];

            distance_squared_new =  (GET_SQUARE((py - y)) + GET_SQUARE((px - x)));

            /*This is further away from the player*/
            if (distance_squared_new > distance_squared_now) continue;

            if (cave_passable_mon(m_ptr, y, x, &dummy) >= 75)
            {
                passable = TRUE;
                break;
            }
        }

        /* Usually head straight for character */
        if (passable)
        {
            *ty = py;
            *tx = px;
            return;

        }

        else
        {
            /*Do we need to activate or update this flow?*/
            if ((ABS(py - p_ptr->flow_center_y) > 1) || (ABS(px - p_ptr->flow_center_x) > 1))
            {
                /* Full update of the flows, which should activate the FLOW_PASS_WALLS */
                p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
            }

            /*This monster needs to advance by flow from now on*/
            m_ptr->mflag |= (MFLAG_NEED_PASS_WALL_FLOW);
        }
    }

    /* Use target information if available */
    if ((m_ptr->target_y) && (m_ptr->target_x))
    {

        *ty = m_ptr->target_y;
        *tx = m_ptr->target_x;
        return;
    }


    /* Find the best and quickest route for the creature */


    if (m_ptr->using_flow == NEED_FLOW)
    {
        find_best_flow(m_ptr);
    }

    /*
     * Sometimes there is no good flow, because the player is standing in lava
     * Just try to head towards the player.
     */
    if (m_ptr->using_flow == NEED_FLOW)
    {

        *ty = p_ptr->py;
        *tx = p_ptr->px;
        return;
    }


    /*For efficiency, get the flow now*/
    flow_used = m_ptr->using_flow;

    /*Do we prefer a clear path to the player*/
    if ((r_ptr->flags4 & (RF4_BOLT_MASK)) ||
        (r_ptr->flags4 & (RF4_ARCHERY_MASK)) ||
        (r_ptr->flags5 & (RF5_BOLT_MASK)) ||
        (r_ptr->flags5 & (RF5_ARCHERY_MASK)) ||
        (r_ptr->flags6 & (RF6_BOLT_MASK)) ||
        (r_ptr->flags6 & (RF6_ARCHERY_MASK)) ||
        (r_ptr->flags7 & (RF7_BOLT_MASK)) ||
        (r_ptr->flags7 & (RF7_ARCHERY_MASK)))
    {
        /*Is monster wounded?*/
        if ((m_ptr->hp > (m_ptr->maxhp / 2)) || (m_ptr->hp > 750))
        {
                wants_shot = TRUE;
        }
    }


    /*We don't want to move further away from the player if we don't have to*/
    if (cave_cost[flow_used][m_ptr->fy][m_ptr->fx])
    {
        lowest_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx] + 1;

        *ty = m_ptr->fy;
        *tx = m_ptr->fx;
    }

    /*
     * Find the best square....  Check nearby grids, diagonals last.
     */
    for (i = 0; i < 8; i++)
    {

        /* Get the location */
        y = y1 + ddy_ddd[i];
        x = x1 + ddx_ddd[i];

        /* Check Bounds */
        if (!in_bounds(y, x)) continue;

        /*Is there a flow here?*/
        if (cave_cost[flow_used][y][x])
        {
            int this_cost = cave_cost[flow_used][y][x];

            /*
             * Is there a monster here?  If so, encourange a different path
             * to the player.  Try to surround the player quickly.
             * Strongest monsters get the quickest path.
             */
            if (dungeon_info[y][x].monster_idx > 0)
            {
                monster_type *m2_ptr = &mon_list[dungeon_info[y][x].monster_idx];
                monster_race *r2_ptr = &r_info[m_ptr->r_idx];

                /*
                 * Add a premium for advancing to this square depending on
                 * the strength of the monsters.
                 * Note: Avoid a divide by zero bug if the monster has zero hp.
                 */
                u32b cost_add = (200 * (m2_ptr->hp + 1)) / (m_ptr->hp + 1);

                /*
                 * Cap it at a 199 energy penalty, so the monster doesn't prefer a step
                 * backwards during normal movement
                 */
                if (cost_add > 199) cost_add = 199;

                /* Death molds can be permanent barriers */
                if (r2_ptr->flags1 & (RF1_NEVER_MOVE)) cost_add /= 2;


                /*Apply the penalty*/
                this_cost += cost_add;
            }
            /*avoid glyphs if possible*/
            else if (cave_player_glyph_bold(y, x))
            {
                if (is_monster_native(y, x, r_ptr))
                {
                    this_cost += (f_info[dungeon_info[y][x].feature_idx].native_energy_move);
                }
                else this_cost += (f_info[dungeon_info[y][x].feature_idx].non_native_energy_move);
            }
            /*Is it the best route?*/
            if (this_cost < lowest_cost)
            {
                /*Save the new closest distance*/
                lowest_cost = this_cost;

                /* Save the location */
                *ty = y;
                *tx = x;
                continue;
            }

            /*
             * Go in a straight path if energy is equal
             */
            else if (this_cost == lowest_cost)
            {

                bool better_spot = FALSE;

                /*
                 * Prefer the new spot if the distance squared is less than the current
                 * distance (see Pythagorean's theory for an explanation of the math)
                 * while it is extremely slow to get the square root of a number
                 * without using math.c (which uses floating point math), we can simply
                 * compare the squared numbers to find out which route is more direct to
                 * the player, thus making the monsters prefer the straight route
                 * vs diaganols.  Since distance in Angband is measured in a square pattern
                 * outward rather than a circular pattern, the monster could wobble
                 * back and forth diagonally while advancing towards the player.
                 * We can make the monster advance in a straighter path based on the
                 * comparing the squares of the distance (A2 + B2) and not bother finding the square root.
                 */

                u32b distance_squared_best = (GET_SQUARE((py - *ty)) + GET_SQUARE((px - *tx)));
                u32b distance_squared_new =  (GET_SQUARE((py - y)) + GET_SQUARE((px - x)));

                /* Have the player angle for a clear shot at the player if the monster wants it. */
                if (wants_shot)
                {
                    int best_path = projectable(*ty, *tx, py, px, PROJECT_CHCK);
                    int new_path = projectable(y, x, py, px, PROJECT_CHCK);

                    /*
                     * This is a clearer path, or if equally clear sometimes pick one
                     * The way the #defines are ordered, the higher the result, the clearer the shot
                     */
                    if (new_path > best_path)
                    {
                        /*Save the best square*/
                        lowest_cost = this_cost;

                        /* Save the location */
                        *ty = y;
                        *tx = x;

                        continue;
                    }

                    /*
                     * If equally clear almost always pick the further route because that will
                     * most likely help the monster find a clearer path on the next turn.
                     * The one_in_two check will help the creature wobble a bit to try to find an open shot.
                     */
                    if ((new_path == best_path) && (best_path < PROJECT_CLEAR))
                    {
                        if ((distance_squared_new > distance_squared_best) || one_in_(2))

                        /*Save the best square*/
                        lowest_cost = this_cost;

                        /* Save the location */
                        *ty = y;
                        *tx = x;

                        continue;
                    }
                }

                /*Don't "wobble" towards player*/
                if (distance_squared_new > distance_squared_best) continue;

                /*50-50 shot, for pillar dancers*/
                if (distance_squared_new == distance_squared_best)
                {
                    if (one_in_(2)) better_spot = TRUE;
                    else continue;
                }

                /*
                 * At this point, distance_squared_new is always lower than distance_squared_cur.
                 * No need to check due to previous if statements eliminating the other possibilities.
                 * The square under consideration is slightly closer,
                 * but we want to gradually make diagonal moves towards the player.
                 */
                if (!better_spot)
                {
                    int new_y_diff = ABS(p_ptr->py - y);
                    int new_x_diff = ABS(p_ptr->px - x);

                    int shorter_axis_diff = 1;

                    /*Base it on the shorter axis*/
                    if (new_y_diff > new_x_diff) shorter_axis_diff = new_y_diff - new_x_diff;
                    else if (new_x_diff > new_y_diff) shorter_axis_diff = new_x_diff - new_y_diff;
                    else shorter_axis_diff = 1; /*Hack - straight diagonal path to the player, always take it*/

                    if (one_in_(shorter_axis_diff)) better_spot = TRUE;

                }

                if (better_spot)
                {
                    /*Save the best square*/
                    lowest_cost = this_cost;

                    /* Save the location */
                    *ty = y;
                    *tx = x;
                     continue;
                }
            }
        }
        /*No flow here, continue*/
    }
}



/*
 * "Do not be seen."
 *
 * Monsters in LOS that want to retreat are primarily interested in
 * finding a nearby place that the character can't see into.
 * Search for such a place with the lowest cost to get to up to 15
 * grids away.
 *
 * Look outward from the monster's current position in a square-
 * shaped search pattern.  Calculate the approximate cost in monster
 * turns to get to each passable grid, using a crude route finder.  Penal-
 * ize grids close to or approaching the character.  Ignore hiding places
 * with no safe exit.  Once a passable grid is found that the character
 * can't see, the code will continue to search a little while longer,
 * depending on how pricey the first option seemed to be.
 *
 * If the search is successful, the monster will target that grid,
 * and (barring various special cases) run for it until it gets there.
 *
 * We use a limited waypoint system (see function "get_route_to_target()"
 * to reduce the likelihood that monsters will get stuck at a wall between
 * them and their target (which is kinda embarrassing...).
 *
 * This function does not yield perfect results; it is known to fail
 * in cases where the previous code worked just fine.  The reason why
 * it is used is because its failures are less common and (usually)
 * less embarrassing than was the case before.  In particular, it makes
 * monsters great at not being seen.
 *
 * This function is fairly expensive.  Call it only when necessary.
 */
static bool find_safety(monster_type *m_ptr, int *ty, int *tx)
{
    int i, j, d;

    /* Scanning range for hiding place search. */
    byte scan_range = 15;

    /*
     * Allocate and initialize a table of movement costs.
     * Both axis must be (2 * scan_range + 1).
     */
    u16b safe_cost[31][31];

    int y, x, yy, xx;

    int countdown = scan_range;

    int least_cost = BASE_FLOW_MAX;
    int least_cost_y = 0;
    int least_cost_x = 0;
    int chance, cost, parent_cost;
    bool dummy;

    /* Factors for converting table to actual dungeon grids */
    int conv_y, conv_x;

    for (i = 0; i < 31; i++)
    {
        for (j = 0; j < 31; j++)
        {
            safe_cost[i][j] = 0;
        }
    }

    conv_y = scan_range - m_ptr->fy;
    conv_x = scan_range - m_ptr->fx;

    /* Mark the origin */
    safe_cost[scan_range][scan_range] = 1;

    /* If the character's grid is in range, mark it as being off-limits */
    if ((ABS(m_ptr->fy - p_ptr->py) <= scan_range) &&
        (ABS(m_ptr->fx - p_ptr->px) <= scan_range))
    {
        safe_cost[p_ptr->py + conv_y][p_ptr->px + conv_x] = BASE_FLOW_MAX;
    }

    /* Work outward from the monster's current position */
    for (d = 0; d < scan_range; d++)
    {
        for (y = scan_range - d; y <= scan_range + d; y++)
        {
            for (x = scan_range - d; x <= scan_range + d;)
            {
                int x_tmp;

                /*
                 * Scan all grids of top and bottom rows, just
                 * outline other rows.
                 */
                if ((y != scan_range - d) && (y != scan_range + d))
                {
                    if (x == scan_range + d) x_tmp = 999;
                    else x_tmp = scan_range + d;
                }
                else x_tmp = x + 1;

                /* Grid and adjacent grids must be legal */
                if (!in_bounds_fully(y - conv_y, x - conv_x))
                {
                    x = x_tmp;
                    continue;
                }

                /* Grid is inaccessible (or at least very difficult to enter) */
                if ((safe_cost[y][x] == 0) || (safe_cost[y][x] >= 10000))
                {
                    x = x_tmp;
                    continue;
                }

                /* Get the accumulated cost to enter this grid */
                parent_cost = safe_cost[y][x];

                /* Scan all adjacent grids */
                for (i = 0; i < 8; i++)
                {
                    bool can_hide = FALSE;

                    yy = y + ddy_ddd[i];
                    xx = x + ddx_ddd[i];

                    /* check bounds */
                    if ((yy < 0) || (yy > 30) || (xx < 0) || (xx > 30)) continue;

                    /*
                     * Handle grids with empty cost and passable grids
                     * with costs we have a chance of beating.
                     */
                    if ((safe_cost[yy][xx] == 0) ||
                          ((safe_cost[yy][xx] > parent_cost + 1) &&
                           (safe_cost[yy][xx] < 100)))
                    {
                        /* The monster can hide here - best case scenario. */
                        if ((m_ptr->mflag & (MFLAG_HIDE)) &&
                            (cave_ff2_match(yy - conv_y, xx - conv_x, FF2_COVERED)))
                        {
                            can_hide = TRUE;

                            chance = 200;
                        }

                        /* Get the cost to enter this grid */
                        else chance = cave_passable_mon(m_ptr, yy - conv_y,
                                 xx - conv_x, &dummy);

                        /* Impassable */
                        if (!chance)
                        {
                            /* Cannot enter this grid */
                            safe_cost[yy][xx] = 100;
                            continue;
                        }

                        /* Calculate approximate cost (in monster turns) */
                        cost = 100 / chance;

                        /* Next to character */
                        if (distance(yy - conv_y, xx - conv_x,
                            p_ptr->py, p_ptr->px) <= 1)
                        {
                            /* Don't want to maneuver next to the character */
                            cost += 3;
                        }

                        /* Mark this grid with a cost value */
                        safe_cost[yy][xx] = parent_cost + cost;

                        /* Character can't see this grid */
                        if (!player_can_see_bold(yy - conv_y, xx - conv_x))
                        {
                            int this_cost = safe_cost[yy][xx];

                            /* Penalize grids that approach character */
                            if (ABS(p_ptr->py - (yy - conv_y)) <
                                ABS(m_ptr->fy - (yy - conv_y)))
                            {
                                 this_cost *= 2;
                            }
                            if (ABS(p_ptr->px - (xx - conv_x)) <
                                ABS(m_ptr->fx - (xx - conv_x)))
                            {
                                 this_cost *= 2;
                            }

                            /* Accept lower-cost, sometimes accept same-cost options */
                            if ((least_cost > this_cost) ||
                                (least_cost == this_cost && one_in_(2)) || can_hide)
                            {
                                bool has_escape = FALSE;

                                /* Scan all adjacent grids for escape routes */
                                for (j = 0; j < 8; j++)
                                {
                                    /* Calculate real adjacent grids */
                                    int yyy = yy - conv_y + ddy_ddd[i];
                                    int xxx = xx - conv_x + ddx_ddd[i];

                                    /* Check bounds */
                                    if (!in_bounds(yyy, xxx)) continue;

                                    /* Look for any passable grid that isn't in LOS */
                                    if (((!player_can_see_bold(yyy, xxx)) &&
                                        (cave_passable_mon(m_ptr, yyy, xxx, &dummy))) ||
                                        ((m_ptr->mflag & (MFLAG_HIDE)) &&
                                        (cave_ff2_match(yyy, xxx, FF2_COVERED))))
                                    {
                                        /* Not a one-grid cul-de-sac */
                                        has_escape = TRUE;
                                        break;
                                    }
                                }

                                /* Ignore cul-de-sacs */
                                if (has_escape == FALSE) continue;

                                least_cost = this_cost;
                                least_cost_y = yy;
                                least_cost_x = xx;

                                /*
                                 * Look hard for alternative hiding places if
                                 * this one seems pricey.
                                 */
                                countdown = 1 + least_cost - d;
                            }
                        }
                    }
                }

                /* Adjust x as instructed */
                x = x_tmp;
            }
        }

        /*
         * We found a good place a while ago, and haven't done better
         * since, so we're probably done.
         */
        if (countdown-- == 0) break;
    }

    /* We found a place that can be reached in reasonable time */
    if (least_cost < 50)
    {
        /* Convert to actual dungeon grid. */
        y = least_cost_y - conv_y;
        x = least_cost_x - conv_x;

        /* Move towards the hiding place */
        *ty = y;
        *tx = x;

        /* Target the hiding place */
        m_ptr->target_y = y;
        m_ptr->target_x = x;

        return (TRUE);
    }


    /* No good place found */
    return (FALSE);
}


/*
 * Helper function for monsters that want to retreat from the character.
 * Used for any monster that is terrified, frightened, is looking for a
 * temporary hiding spot, or just wants to open up some space between it
 * and the character.
 *
 * If the monster is well away from danger, let it relax.
 * If the monster's current target is not in LOS, use it (+).
 * If the monster is not in LOS, and cannot pass through walls, try to
 * use flow (noise) information.
 * If the monster is in LOS, even if it can pass through walls,
 * search for a hiding place (helper function "find_safety()"):
 * search for a hiding place (helper function "find_safety()").
 * If no hiding place is found, and there seems no way out, go down
 * fighting.
 *
 * If none of the above solves the problem, run away blindly.
 *
 * (+) There is one exception to the automatic usage of a target.  If the
 * target is only out of LOS because of "knight's move" rules (distance
 * along one axis is 2, and along the other, 1), then the monster will try
 * to find another adjacent grid that is out of sight.  What all this boils
 * down to is that monsters can now run around corners properly!
 *
 * Return TRUE if the monster did actually want to do anything.
 */
static bool get_move_retreat(monster_type *m_ptr, int *ty, int *tx)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int i;
    int y, x;

    bool done = FALSE;
    bool dummy;

    /* If the monster is well away from danger, let it relax. */
    if (m_ptr->cdis >= FLEE_RANGE)
    {
        return (FALSE);
    }

    /*Monster is hiding, relax*/
    if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

    /* Monster has a target */
    if ((m_ptr->target_y) && (m_ptr->target_x))
    {
        /* It's out of LOS; keep using it, except in "knight's move" cases */
        if (!player_has_los_bold(m_ptr->target_y, m_ptr->target_x))
        {
            /* Get axis distance from character to current target */
            int dist_y = ABS(p_ptr->py - m_ptr->target_y);
            int dist_x = ABS(p_ptr->px - m_ptr->target_x);

            /* It's only out of LOS because of "knight's move" rules */
            if (((dist_y == 2) && (dist_x == 1)) ||
                ((dist_y == 1) && (dist_x == 2)))
            {
                /*
                 * If there is another grid adjacent to the monster that
                 * the character cannot see into, and it isn't any harder
                 * to enter, use it instead.  Prefer diagonals.
                 */
                for (i = 7; i >= 0; i--)
                {
                    y = m_ptr->fy + ddy_ddd[i];
                    x = m_ptr->fx + ddx_ddd[i];

                    /* Check Bounds */
                    if (!in_bounds(y, x)) continue;

                    if (player_has_los_bold(y, x)) continue;

                    if ((y == m_ptr->target_y) && (x == m_ptr->target_x)) continue;

                    if (cave_passable_mon(m_ptr, m_ptr->target_y, m_ptr->target_x, &dummy) >
                        cave_passable_mon(m_ptr, y, x, &dummy)) continue;

                    m_ptr->target_y = y;
                    m_ptr->target_x = x;
                    break;
                }
            }

            /* Move towards the target */
            *ty = m_ptr->target_y;
            *tx = m_ptr->target_x;
            return (TRUE);
        }

        /* It's in LOS; cancel it. */
        else
        {
            m_ptr->target_y = 0;
            m_ptr->target_x = 0;
        }
    }

    /*
     * The  monster has no flow, and no target (as checked above)
     * First off, the game will crash if using flow == NEEDS flow and the game enters one of the "else" statements below
     * so this line of "if" and "else" statements must account for this.
     * In general, the monster will only need this if the player is standing in a dangerous terrain.
     */
    else if (m_ptr->using_flow == NEED_FLOW)
    {
        if (find_safety(m_ptr, ty, tx) == TRUE) return (TRUE);
    }

    /* The monster is not in LOS, but thinks it's still too close. */
    else if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
    {
        /* Monster cannot pass through walls */
        if (!((r_ptr->flags2 & (RF2_PASS_WALL)) ||
              (r_ptr->flags2 & (RF2_KILL_WALL))))
        {
            byte flow_used = get_monster_flow(m_ptr);

            /* Run away from noise */
            if (cave_cost[flow_used][m_ptr->fy][m_ptr->fx])
            {
                int start_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx];

                /* Look at adjacent grids, diagonals first */
                for (i = 7; i >= 0; i--)
                {
                    y = m_ptr->fy + ddy_ddd[i];
                    x = m_ptr->fx + ddx_ddd[i];

                    /* Check bounds */
                    if (!in_bounds(y, x)) continue;

                    /* Accept the first non-visible grid with a higher cost */
                    if (cave_cost[flow_used][y][x] > start_cost)
                    {
                        if (!player_has_los_bold(y, x))
                        {
                            *ty = y;  *tx = x;
                            done = TRUE;
                            break;
                        }
                    }
                }

                /* Return if successful */
                if (done) return (TRUE);
            }
        }

        /* No flow info, or don't need it -- see bottom of function */
    }

    /* The monster is in line of sight. */
    else
    {
        byte flow_used = get_monster_flow(m_ptr);

        int prev_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx];
        int start = rand_int(8);

        /* Look for adjacent hiding places */
        for (i = start; i < 8 + start; i++)
        {
            y = m_ptr->fy + ddy_ddd[i % 8];
            x = m_ptr->fx + ddx_ddd[i % 8];

            /* Check Bounds */
            if (!in_bounds(y, x)) continue;

            /* No grids in LOS */
            if (player_has_los_bold(y, x)) continue;

            /* Grid must be pretty easy to enter */
            if (cave_passable_mon(m_ptr, y, x, &dummy) < 50) continue;

            /* Accept any grid that doesn't have a lower flow (noise) cost. */
            if (cave_cost[flow_used][y][x] >= prev_cost)
            {
                *ty = y;
                *tx = x;
                prev_cost = cave_cost[flow_used][y][x];

                /* Success */
                return (TRUE);
            }
        }

        /* Find a nearby grid not in LOS of the character. */
        if (find_safety(m_ptr, ty, tx) == TRUE) return (TRUE);

        /*
         * No safe place found.  If monster is in LOS and close,
         * it will turn to fight.
         */
        if ((player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
            ((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0) &&
            ((m_ptr->cdis < TURN_RANGE) || (m_ptr->m_speed < p_ptr->state.p_speed)))
        {
            if (m_ptr->m_timed[MON_TMD_FEAR])
            {

                QString m_name;;

                m_ptr->mflag |= (MFLAG_DESPERATE);

                /* Forget target */
                m_ptr->target_y = 0;    m_ptr->target_x = 0;

                /* Charge!  XXX XXX */
                m_ptr->min_range = 1;  m_ptr->best_range = 1;

                /* Get the monster name */
                m_name = monster_desc(m_ptr, 0);

                message(QString("%1 fights on desperately!") .arg(capitalize_first(m_name)));
            }

            /* Charge! */
            *ty = p_ptr->py;
            *tx = p_ptr->px;
            return (TRUE);
        }
    }

    /* Move directly away from character. */
    *ty = -(p_ptr->py - m_ptr->fy);
    *tx = -(p_ptr->px - m_ptr->fx);

    /* We want to run away */
    return (TRUE);
}

static void calc_vulnerability(void)
{
    byte i;
    int y, x;

    /*already calculated*/
    if (p_ptr->vulnerability) return;

    /* Attack disabled or aggravating player -EB- */
    if (p_ptr->timed[TMD_BLIND] || p_ptr->timed[TMD_IMAGE] || p_ptr->timed[TMD_CONFUSED] ||
         p_ptr->timed[TMD_AFRAID] || p_ptr->timed[TMD_PARALYZED] || p_ptr->state.aggravate)
    {
        p_ptr->vulnerability = 10;
    }

    /*always more vulnerable if in a room,
     * and this removes the "monsters run if
     * player is in corner of a room" bug
     */
    if (dungeon_info[p_ptr->py][p_ptr->px].cave_info & (CAVE_ROOM))
    {
        p_ptr->vulnerability += 8;
    }

    /*
     * For use in non-rooms (corridors)
     * Count passable grids next to player
     */
    else for (i = 0; i < 8; i++)
    {
        y = p_ptr->py + ddy_ddd[i];
        x = p_ptr->px + ddx_ddd[i];

        /* Check Bounds */
        if (!in_bounds(y, x)) continue;

        /* Count floor grids (generic passable) */
        if (cave_passable_bold(y, x))
        {
            p_ptr->vulnerability++;
        }
    }

    /*
     * Take character weakness into account (this
     * always adds at least one)
     */
    p_ptr->vulnerability += (p_ptr->mhp / MAX(1, p_ptr->chp));

    /* Is seriously weakened -- go berserk */
    if ((p_ptr->chp < 2 * p_ptr->mhp / 5) ||
    /*or, very low on mana for spellcasters*/
        (p_ptr->csp < 2 * p_ptr->msp / 25))
    {
        p_ptr->vulnerability = 100;
    }
    /* Sometimes go berserk at random  XXX
     *
     */
    else if (p_ptr->vulnerability > 4)
    {

        bool berserk = FALSE;

        if (p_ptr->vulnerability >= 15)
        {
            if (randint(25) < p_ptr->vulnerability) berserk = TRUE;
        }
        else
        {
            int chance = 15 - p_ptr->vulnerability;
            if one_in_(chance) berserk = TRUE;
        }

        if (berserk) p_ptr->vulnerability = 100;
    }
}


/*
 * Choose the probable best direction for a monster to move in.  This
 * is done by choosing a target grid and then finding the direction that
 * best approaches it.
 *
 * Monsters that cannot move always attack if possible.
 * Frightened monsters retreat.
 * Monsters adjacent to the character attack if possible.
 *
 * Monster packs lure the character into open ground and then leap
 * upon him.  Monster groups try to surround the character.  -KJ-
 *
 * Monsters not in LOS always advance (this avoids player frustration).
 * Monsters in LOS will advance to the character, up to their standard
 * combat range, to a grid that allows them to target the character, or
 * just stay still if they are happy where they are, depending on the
 * tactical situation and the monster's preferred and minimum combat
 * ranges.
 * NOTE:  Here is an area that would benefit from more development work.
 *
 * Non-trivial movement calculations are performed by the helper
 * functions "get_move_advance" and "get_move_retreat", which keeps
 * this function relatively simple.
 *
 * The variable "must_use_target" is used for monsters that can't
 * currently perceive the character, but have a known target to move
 * towards.  With a bit more work, this will lead to semi-realistic
 * "hunting" behavior.
 *
 * Return FALSE if monster doesn't want to move or can't.
 */
bool get_move(monster_type *m_ptr, int *ty, int *tx, bool *fear, bool must_use_target)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    int py = p_ptr->py;
    int px = p_ptr->px;

    /* Assume no movement */
    *ty = m_ptr->fy;
    *tx = m_ptr->fx;

    /*
     * Monsters that cannot move will attack the character if he is
     * adjacent.  Otherwise, they cannot move unless they are
     * caught in non-native terrain that is hurting them.
     */
    if (r_ptr->flags1 & (RF1_NEVER_MOVE))
    {
        /* Hack -- memorize lack of moves after a while. */
        if (!(l_ptr->r_l_flags1 & (RF1_NEVER_MOVE)))
        {
            if ((m_ptr->ml) && (one_in_(20)))
            {
                l_ptr->r_l_flags1 |= (RF1_NEVER_MOVE);
            }
        }

        /* Is character in range? */
        if (m_ptr->cdis <= 1)
        {

            /* Monster can't melee either (pathetic little creature) */
            if (r_ptr->flags1 & (RF1_NEVER_BLOW))
            {
                /* Hack -- memorize lack of attacks after a while */
                if (!(l_ptr->r_l_flags1 & (RF1_NEVER_BLOW)))
                {
                    if ((m_ptr->ml) && (one_in_(10)))
                    {
                        l_ptr->r_l_flags1 |= (RF1_NEVER_BLOW);
                    }
                }
            }

            /* Can attack */
            else
            {
                /* Kill. */
                *fear = FALSE;
                *ty = py;
                *tx = px;
                return (TRUE);
            }
        }

        /*Are we taking damage where we are?*/
        if (!cave_no_dam_for_mon(m_ptr->fy, m_ptr->fx, r_ptr) &&
                       !MONSTER_CAN_FLY(m_ptr, dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx))
        {
            /*Move towards the player*/
            *fear = FALSE;
            get_move_advance(m_ptr, ty, tx);
            return (TRUE);
        }

        /* If we can't hit anything, do not move */
        else return (FALSE);
    }

    /*
     * Monster is only allowed to use targeting information.
     */
    if (must_use_target)
    {
        *ty = m_ptr->target_y;
        *tx = m_ptr->target_x;
        return (TRUE);
    }

    /*Stop using the flow if we don't need it*/
    if (m_ptr->mflag & (MFLAG_NEED_PASS_WALL_FLOW))
    {
        /*Must have clear line of sight*/
        if (player_can_fire_bold(m_ptr->fy, m_ptr->fx))
        {
            /*Paranoia - The flow must be is active*/
            if (cost_at_center[FLOW_PASS_WALLS] > 0)
            {
                /*Check if we have a straight and for the most part unobstructed path to the player*/
                if (m_ptr->cdis <= ((cave_cost[FLOW_PASS_WALLS][m_ptr->fy][m_ptr->fx] -
                             cost_at_center[FLOW_PASS_WALLS]) / BASE_ENERGY_MOVE))
                {
                    /*We might not need the flow anymore*/
                    m_ptr->mflag &= ~(MFLAG_NEED_PASS_WALL_FLOW);
                }
            }
        }
    }

    /*** Handle monster fear -- only for monsters that can move ***/

    /* Is the monster scared? */
    if ((m_ptr->min_range >= FLEE_RANGE) || (m_ptr->m_timed[MON_TMD_FEAR])) *fear = TRUE;
    else *fear = FALSE;

    /* Monster is frightened or terrified. */
    if ((*fear) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0))
    {
        /* The character is too close to avoid, and faster than we are */
        if ((!m_ptr->m_timed[MON_TMD_FEAR]) && (m_ptr->cdis < TURN_RANGE) &&
             (p_ptr->state.p_speed > m_ptr->m_speed))
        {
            /* Recalculate range */
            find_range(m_ptr);

            /* Note changes in monster attitude */
            if (m_ptr->min_range < m_ptr->cdis)
            {
                /* Cancel fear */
                *fear = FALSE;

                /* No message -- too annoying */
                m_ptr->mflag |= (MFLAG_DESPERATE);

                /* Advance, ... */
                get_move_advance(m_ptr, ty, tx);

                return (TRUE);
            }
        }

        /* The monster is within 25 grids of the character */
        else if (m_ptr->cdis < FLEE_RANGE)
        {
            /* Find and move towards a hidey-hole */
            get_move_retreat(m_ptr, ty, tx);

            return (TRUE);
        }

        /* Monster is well away from danger */
        else
        {
            /* No need to move */
            return (FALSE);
        }
    }

    /* If the character is adjacent, attack or back off.  */
    if ((!*fear) && (m_ptr->cdis <= 1))
    {
        /* Monsters that cannot attack back off. */
        if (r_ptr->flags1 & (RF1_NEVER_BLOW))
        {
            /* Hack -- memorize lack of attacks after a while */
            if (!(l_ptr->r_l_flags1 & (RF1_NEVER_BLOW)))
            {
                if ((m_ptr->ml) && (one_in_(10)))
                {
                    l_ptr->r_l_flags1 |= (RF1_NEVER_BLOW);
                }
            }

            /* Back away */
            *fear = TRUE;
        }

        else
        {
            /* All other monsters attack. */
            get_move_advance(m_ptr, ty, tx);
            return (TRUE);
        }
    }

    /* Animal packs try to lure the character into the open. */
    if ((!*fear) && ((r_ptr->flags1 & (RF1_FRIENDS)) || (r_ptr->flags1 & (RF1_FRIEND))) &&
        (r_ptr->flags3 & (RF3_ANIMAL)) &&
        (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL))))
    {

        /* Animal has to be willing to melee */
        if (m_ptr->min_range == 1)
        {
            /*
             * Make sure player vurnerability is up to date
             */
            calc_vulnerability();

            /*
             * RE_DO - figure out how vulnerable player is in this territory
             * player is more vulnerable in non-native territory
             */
            if (!is_player_native(p_ptr->py,p_ptr->px))
            {
                p_ptr->vulnerability = p_ptr->vulnerability * 3 / 2;
            }

            /*
             * Character is insufficiently vulnerable
             * and monster hasn't been hit by an attack that sets off alarms
             */
            if ((p_ptr->vulnerability <= 4) && (!(m_ptr->mflag & (MFLAG_ATTACKED_BAD))))
            {
                /* If we're in sight, find a hiding place */
                if (dungeon_info[m_ptr->fy][m_ptr->fx].cave_info & (CAVE_FIRE | CAVE_SEEN))
                {
                    /* Find a safe spot to lurk in */
                    if (get_move_retreat(m_ptr, ty, tx))
                    {
                        *fear = TRUE;
                    }
                    else
                    {
                        /* Cancel fear */
                        *fear = FALSE;

                        /* No message -- too annoying */

                        /* Advance, ... */
                        get_move_advance(m_ptr, ty, tx);
                    }
                }

                /* Otherwise, we advance cautiously */
                else
                {
                    /* Advance, ... */
                    get_move_advance(m_ptr, ty, tx);

                    /* ... but make sure we stay hidden. */
                    if (m_ptr->cdis > 1) *fear = TRUE;
                }

                /* done */
                return (TRUE);
            }
        }
    }

    /* Monster can go through rocks - head straight for character */
    if ((!*fear) && ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
                     (r_ptr->flags2 & (RF2_KILL_WALL))))
    {
        /* Advance, ... */
        get_move_advance(m_ptr, ty, tx);
    }


    /* No special moves made -- use standard movement */

    /* Not frightened */
    if (!*fear)
    {
        /*
         * The monster is advancing.
         */
        get_move_advance(m_ptr, ty, tx);
    }

    /* Monster is frightened */
    else
    {
        /* Back away -- try to be smart about it */
        get_move_retreat(m_ptr, ty, tx);
    }

    /* We do not want to move */
    if ((*ty == m_ptr->fy) && (*tx == m_ptr->fx)) return (FALSE);

    /* We want to move */
    return (TRUE);
}




/*
 * A simple method to help fleeing monsters who are having trouble getting
 * to their target.  It's very stupid, but works fairly well in the
 * situations it is called upon to resolve.  XXX XXX
 *
 * If this function claims success, ty and tx must be set to a grid
 * adjacent to the monster.
 *
 * Return TRUE if this function actually did any good.
 */
static bool get_route_to_target(monster_type *m_ptr, int *ty, int *tx)
{
    int i, j;
    int y, x, yy, xx;
    int tar_y, tar_x, dist_y, dist_x;

    bool dummy;
    bool below = FALSE;
    bool right = FALSE;

    tar_y = 0;
    tar_x = 0;

    /* Is the target further away vertically or horizontally? */
    dist_y = ABS(m_ptr->target_y - m_ptr->fy);
    dist_x = ABS(m_ptr->target_x - m_ptr->fx);

    /* Target is further away vertically than horizontally */
    if (dist_y > dist_x)
    {
        /* Find out if the target is below the monster */
        if (m_ptr->target_y - m_ptr->fy > 0) below = TRUE;

        /* Search adjacent grids */
        for (i = 0; i < 8; i++)
        {
            y = m_ptr->fy + ddy_ddd[i];
            x = m_ptr->fx + ddx_ddd[i];

            /* Check bounds */
            if (!in_bounds_fully(y, x)) continue;

            /* Grid is not passable */
            if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

            /* Grid will take me further away */
            if ((( below) && (y < m_ptr->fy)) ||
                ((!below) && (y > m_ptr->fy)))
            {
                continue;
            }

            /* Grid will not take me closer or further */
            else if (y == m_ptr->fy)
            {
                /* See if it leads to better things */
                for (j = 0; j < 8; j++)
                {
                    yy = y + ddy_ddd[j];
                    xx = x + ddx_ddd[j];

                    /* Grid does lead to better things */
                    if ((( below) && (yy > m_ptr->fy)) ||
                        ((!below) && (yy < m_ptr->fy)))
                    {
                        /* But it is not passable */
                        if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

                        /*
                         * Accept (original) grid, but don't immediately claim
                         * success
                         */
                        tar_y = y;
                        tar_x = x;
                    }
                }
            }

            /* Grid will take me closer */
            else
            {
                /* Don't look this gift horse in the mouth. */
                *ty = y;
                *tx = x;
                return (TRUE);
            }
        }
    }

    /* Target is further away horizontally than vertically */
    else if (dist_x > dist_y)
    {
        /* Find out if the target is right of the monster */
        if (m_ptr->target_x - m_ptr->fx > 0) right = TRUE;

        /* Search adjacent grids */
        for (i = 0; i < 8; i++)
        {
            y = m_ptr->fy + ddy_ddd[i];
            x = m_ptr->fx + ddx_ddd[i];

            /* Check bounds */
            if (!in_bounds_fully(y, x)) continue;

            /* Grid is not passable */
            if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

            /* Grid will take me further away */
            if ((( right) && (x < m_ptr->fx)) ||
                ((!right) && (x > m_ptr->fx)))
            {
                continue;
            }

            /* Grid will not take me closer or further */
            else if (x == m_ptr->fx)
            {
                /* See if it leads to better things */
                for (j = 0; j < 8; j++)
                {
                    yy = y + ddy_ddd[j];
                    xx = x + ddx_ddd[j];

                    /* Grid does lead to better things */
                    if ((( right) && (xx > m_ptr->fx)) ||
                        ((!right) && (xx < m_ptr->fx)))
                    {
                        /* But it is not passable */
                        if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

                        /* Accept (original) grid, but don't immediately claim success */
                        tar_y = y;
                        tar_x = x;
                    }
                }
            }

            /* Grid will take me closer */
            else
            {
                /* Don't look this gift horse in the mouth. */
                *ty = y;
                *tx = x;
                return (TRUE);
            }
        }
    }

    /* Target is the same distance away along both axes. */
    else
    {
        /* XXX XXX - code something later to fill this hole. */
        return (FALSE);
    }

    /* If we found a solution, claim success */
    if ((tar_y) && (tar_x))
    {
        *ty = tar_y;
        *tx = tar_x;
        return (TRUE);
    }

    /* No luck */
    return (FALSE);
}


/*
 * Confused monsters bang into walls and doors, and wander into lava or
 * water.  This function assumes that the monster does not belong in this
 * grid, and therefore should suffer for trying to enter it.
 */
static void make_confused_move(monster_type *m_ptr, int y, int x)
{
    QString m_name;

    bool seen = FALSE;

    bool confused = m_ptr->m_timed[MON_TMD_CONF];

    /* Check Bounds (fully) */
    if (!in_bounds_fully(y, x)) return;

    /* Check visibility */
    if ((m_ptr->ml) && (dungeon_info[y][x].cave_info & (CAVE_SEEN))) seen = TRUE;

    /* Get the monster name/poss */
    m_name = monster_desc(m_ptr, 0);

    /* Monster is frightened */
    if (seen)
    {
        message(QString("%1 panics!")  .arg(capitalize_first(m_name)));
    }

    /* Feature can't be passed */
    if (!cave_passable_bold(y, x))
    {
        /* Feature is a (known) door */
        if ((f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_DOOR)) &&
            (!(f_info[dungeon_info[y][x].feature_idx].f_flags1 & (FF1_SECRET))))
        {
            if (seen && confused)
                message(QString("%1 bangs into a door.") .arg(capitalize_first(m_name)));
        }

        /* Otherwise, we assume that the feature is a "wall".  XXX  */
        else
        {
            if (seen && confused)
                message(QString("%1 bashes into a wall.") .arg(capitalize_first(m_name)));
        }

        /* Sometimes stun the monster, but only lightly */
        if (one_in_(3))
        {
            mon_inc_timed(m_ptr->get_mon_idx(), MON_TMD_STUN, 3, MON_TMD_FLG_NOTIFY);
        }
    }

    /* Feature is not a wall */
    else
    {
        /* No changes */
    }
}


/*
 * Given a target grid, calculate the grid the monster will actually
 * attempt to move into.
 *
 * The simplest case is when the target grid is adjacent to us and
 * able to be entered easily.  Usually, however, one or both of these
 * conditions don't hold, and we must pick an initial direction, than
 * look at several directions to find that most likely to be the best
 * choice.  If so, the monster needs to know the order in which to try
 * other directions on either side.  If there is no good logical reason
 * to prioritize one side over the other, the monster will act on the
 * "spur of the moment", using current turn as a randomizer.
 *
 * The monster then attempts to move into the grid.  If it fails, this
 * function returns FALSE and the monster ends its turn.
 *
 * The variable "fear" is used to invoke any special rules for monsters
 * wanting to retreat rather than advance.  For example, such monsters
 * will not leave an non-viewable grid for a viewable one and will try
 * to avoid the character.
 *
 * The variable "bash" remembers whether a monster had to bash a door
 * or not.  This has to be remembered because the choice to bash is
 * made in a different function than the actual bash move.  XXX XXX  If
 * the number of such variables becomes greater, a structure to hold them
 * would look better than passing them around from function to function.
 */
bool make_move(monster_type *m_ptr, int *ty, int *tx, bool fear, bool *bash)
{
    int i, j;

    /* Start direction, current direction */
    int dir0, dir;

    /* Deltas, absolute axis distances from monster to target grid */
    int dy, ay, dx, ax;

    /* Existing monster location, proposed new location */
    int oy, ox, ny, nx;

    bool avoid = FALSE;
    bool passable = FALSE;
    bool look_again = FALSE;

    int chance;

    /* Remember where monster is */
    oy = m_ptr->fy;
    ox = m_ptr->fx;

    /* Get the change in position needed to get to the target */
    dy = *ty - oy;
    dx = *tx - ox;

    /* Monster is frightened, but is obliged to fight. */
    if ((fear) && (dungeon_info[*ty][*tx].monster_idx < 0))
    {
        /* Message if seen */
        if ((m_ptr->ml) && (m_ptr->m_timed[MON_TMD_FEAR]) &&
                  ((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0))
        {
            QString m_name;;

            m_ptr->mflag |= MFLAG_DESPERATE;

            /* Get the monster name */
            m_name = monster_desc(m_ptr, 0);

            /* Dump a message */
            message(QString("%1 fights on desperately!")  .arg(capitalize_first(m_name)));
        }

        /* Turn and fight */
        fear = FALSE;

        /* Forget target */
        m_ptr->target_y = 0;    m_ptr->target_x = 0;

        /* Charge!  XXX XXX */
        m_ptr->min_range = 1;  m_ptr->best_range = 1;

        /* can't return yet; monster might not be adjacent to player */
    }

    /* Is the target grid adjacent to the current monster's position? */
    if ((!fear) && (dy >= -1) && (dy <= 1) && (dx >= -1) && (dx <= 1))
    {
        /* If it is, try the shortcut of simply moving into the grid */

        /* Get the probability of entering this grid */
        chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

        /* Grid must be pretty easy to enter, or monster must be confused */
        if ((m_ptr->m_timed[MON_TMD_CONF]) || (chance >= 50))
        {
            /*
             * Amusing messages and effects for confused monsters trying
             * to enter terrain forbidden to them.
             */
            if ((m_ptr->m_timed[MON_TMD_CONF]) && (chance <= 25))
            {
                /* Sometimes hurt the poor little critter */
                if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

                /* Do not actually move */
                if (!chance) return (FALSE);
            }

            /* We can enter this grid */
            if ((chance >= 100) || (chance > rand_int(100)))
            {
                return (TRUE);
            }

            /* Failure to enter grid.  Cancel move */
            else
            {
                return (FALSE);
            }
        }
    }

    /* Calculate vertical and horizontal distances */
    ay = ABS(dy);
    ax = ABS(dx);

    /* We mostly want to move vertically */
    if (ay > (ax * 2))
    {
        /* Choose between directions '8' and '2' */
        if (dy < 0)
        {
            /* We're heading up */
            dir0 = 8;
            if ((dx < 0) || (dx == 0 && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
        else
        {
            /* We're heading down */
            dir0 = 2;
            if ((dx > 0) || (dx == 0 && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
    }

    /* We mostly want to move horizontally */
    else if (ax > (ay * 2))
    {
        /* Choose between directions '4' and '6' */
        if (dx < 0)
        {
            /* We're heading left */
            dir0 = 4;
            if ((dy > 0) || (dy == 0 && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
        else
        {
            /* We're heading right */
            dir0 = 6;
            if ((dy < 0) || (dy == 0 && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
    }

    /* We want to move up and sideways */
    else if (dy < 0)
    {
        /* Choose between directions '7' and '9' */
        if (dx < 0)
        {
            /* We're heading up and left */
            dir0 = 7;
            if ((ay < ax) || (ay == ax && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
        else
        {
            /* We're heading up and right */
            dir0 = 9;
            if ((ay > ax) || (ay == ax && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
    }

    /* We want to move down and sideways */
    else
    {
        /* Choose between directions '1' and '3' */
        if (dx < 0)
        {
            /* We're heading down and left */
            dir0 = 1;
            if ((ay > ax) || (ay == ax && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
        else
        {
            /* We're heading down and right */
            dir0 = 3;
            if ((ay < ax) || (ay == ax && (p_ptr->game_turn % 2 == 0))) dir0 += 10;
        }
    }


    /*
     * Now that we have an initial direction, we must determine which
     * grid to actually move into.
     */
    if (TRUE)
    {

        move_data moves_data[8];


        /*
         * Scan each of the eight possible directions, in the order of
         * priority given by the table "side_dirs", choosing the one that
         * looks like it will get the monster to the character - or away
         * from him - most effectively.
         */
        for (i = 0; i <= 8; i++)
        {
            /* Out of options */
            if (i == 8) break;

            /* Get the actual direction */
            dir = side_dirs[dir0][i];

            /* Get the grid in our chosen direction */
            ny = oy + ddy[dir];
            nx = ox + ddx[dir];

            /* Check Bounds */
            if (!in_bounds(ny, nx)) continue;

            /* Store this grid's movement data. */
            moves_data[i].move_chance =
                cave_passable_mon(m_ptr, ny, nx, bash);
            moves_data[i].move_bash = *bash;


            /* Confused monsters must choose the first grid */
            if (m_ptr->m_timed[MON_TMD_CONF]) break;

            /* If this grid is totally impassable, skip it */
            if (moves_data[i].move_chance == 0) continue;

            /* Frightened monsters work hard not to be seen. */
            if (fear)
            {
                /* Monster is having trouble navigating to its target. */
                if ((m_ptr->target_y) && (m_ptr->target_x) && (i >= 2) &&
                    (distance(m_ptr->fy, m_ptr->fx, m_ptr->target_y, m_ptr->target_x) > 1))
                {
                    /* Look for an adjacent grid leading to the target */
                    if (get_route_to_target(m_ptr, ty, tx))
                    {
                        /* Calculate the chance to enter the grid */
                        chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

                        /* Try to move into the grid */
                        if (randint(100) > chance)
                        {
                            /* Can't move here */
                            continue;
                        }

                        /* Can move */
                        return (TRUE);
                    }

                    /* No good route found */
                    else if (i >= 3)
                    {
                        /*
                         * We can't get to our hiding place.  We're in line of fire.
                         * The only thing left to do is go down fighting.  XXX XXX
                         */
                         if ((m_ptr->ml) && (player_can_fire_bold(oy, ox)) &&
                                 ((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0))
                         {
                            QString m_name;;

                            m_ptr->mflag |= (MFLAG_DESPERATE);

                            /* Forget target */
                            m_ptr->target_y = 0;    m_ptr->target_x = 0;

                            /* Charge!  XXX XXX */
                            m_ptr->min_range = 1;  m_ptr->best_range = 1;

                            /* Get the monster name */
                            m_name = monster_desc(m_ptr, 0);

                            /* Dump a message if they weren't just scared */
                            if (!(m_ptr->mflag & (MFLAG_JUST_SCARED)))
                                message(QString("%1 turns to fight!")  .arg(capitalize_first(m_name)));

                            /* Hack -- lose some time  XXX XXX */
                            return (FALSE);
                        }
                    }
                }

                /* Attacking the character as a first choice? */
                if ((i == 0) && (ny == p_ptr->py) && (nx == p_ptr->px))
                {
                    /* Need to rethink some plans XXX XXX XXX */
                    m_ptr->target_y = 0;
                    m_ptr->target_x = 0;
                }

                /* Monster is visible */
                if (m_ptr->ml)
                {
                    /* And is in LOS */
                    if (player_has_los_bold(oy, ox))
                    {
                        /* Accept any easily passable grid out of LOS */
                        if ((!player_has_los_bold(ny, nx)) &&
                            (moves_data[i].move_chance > 40))
                        {
                            break;
                        }
                    }
                    else
                    {
                        /* Do not enter a grid in LOS */
                        if (player_has_los_bold(ny, nx))
                        {
                            moves_data[i].move_chance = 0;
                            continue;
                        }
                    }
                }

                /* Monster can't be seen, and is not in a "seen" grid. */
                if ((!m_ptr->ml) && (!player_can_see_bold(oy, ox)))
                {
                    /* Do not enter a "seen" grid */
                    if (player_can_see_bold(ny, nx))
                    {
                        moves_data[i].move_chance = 0;
                        continue;
                    }
                }
            }

            /* XXX XXX -- Sometimes attempt to break glyphs. */
            if (cave_player_glyph_bold(ny, nx) && !fear && one_in_(5))
            {
                break;
            }

            /* Initial direction is almost certainly the best one */
            if ((i == 0) && (moves_data[i].move_chance >= 80))
            {
                /*
                 * If backing away and close, try not to walk next
                 * to the character, or get stuck fighting him.
                 */
                if ((fear) && (m_ptr->cdis <= 2) &&
                    (distance(p_ptr->py, p_ptr->px, ny, nx) <= 1))
                {
                    avoid = TRUE;
                }

                else break;
            }

            /* Either of the first two side directions looks good */
            else if (((i == 1) || (i == 2)) &&
                     (moves_data[i].move_chance >= 50))
            {
                /* Accept the central direction if at least as good */
                if ((moves_data[0].move_chance >=
                     moves_data[i].move_chance))
                {
                    if (avoid)
                    {
                        /* Frightened monsters try to avoid the character */
                        if (distance(p_ptr->py, p_ptr->px, ny, nx) == 0)
                        {
                            i = 0;
                        }
                    }
                    else
                    {
                        i = 0;
                    }
                }

                /* Accept this direction */
                break;
            }

            /* This is the first passable direction */
            if (!passable)
            {
                /* Note passable */
                passable = TRUE;

                /* All the best directions are blocked. */
                if (i >= 3)
                {
                    /* Settle for "good enough" */
                    break;
                }
            }

            /* We haven't made a decision yet; look again. */
            if (i == 7) look_again = TRUE;
        }


        /* We've exhausted all the easy answers. */
        if (look_again)
        {
            /* There are no passable directions. */
            if (!passable)
            {
                return (FALSE);
            }

            /* We can move. */
            for (j = 0; j < 8; j++)
            {
                /* Accept the first option, however poor.  XXX */
                if (moves_data[j].move_chance)
                {
                    i = j;
                    break;
                }
            }
        }

        /* If no direction was acceptable, end turn */
        if (i >= 8)
        {
            return (FALSE);
        }

        /* Get movement information (again) */
        dir = side_dirs[dir0][i];
        *bash = moves_data[i].move_bash;

        /* No good moves, so we just sit still and wait. */
        if ((dir == 5) || (dir == 0))
        {
            return (FALSE);
        }

        /* Get grid to move into */
        *ty = oy + ddy[dir];
        *tx = ox + ddx[dir];

        /*
         * Amusing messages and effects for confused monsters trying
         * to enter terrain forbidden to them.
         */
        if ((m_ptr->m_timed[MON_TMD_CONF]) && (moves_data[i].move_chance <= 25))
        {
            /* Sometimes hurt the poor little critter */
            if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

            /* Do not actually move */
            if (!moves_data[i].move_chance) return (FALSE);
        }

        /* Try to move in the chosen direction.  If we fail, end turn. */
        if ((moves_data[i].move_chance < 100) &&
            (randint(100) > moves_data[i].move_chance))
        {
            return (FALSE);
        }
    }

    /* We can move. */
    return (TRUE);
}

/*
 * If one monster moves into another monster's grid, they will
 * normally swap places.  If the second monster cannot exist in the
 * grid the first monster left, this can't happen.  In such cases,
 * the first monster tries to push the second out of the way.
 */
static bool push_aside(monster_type *m_ptr, monster_type *n_ptr)
{
    /* Get racial information about the second monster */
    monster_race *nr_ptr = &r_info[n_ptr->r_idx];

    int y, x, i;
    int dir = 0;

    /*
     * Translate the difference between the locations of the two
     * monsters into a direction of travel.
     */
    for (i = 0; i < 10; i++)
    {
        /* Require correct difference along the y-axis */
        if ((n_ptr->fy - m_ptr->fy) != ddy[i]) continue;

        /* Require correct difference along the x-axis */
        if ((n_ptr->fx - m_ptr->fx) != ddx[i]) continue;

        /* Found the direction */
        dir = i;
        break;
    }

    /* Favor either the left or right side on the "spur of the moment". */
    if (one_in_(2)) dir += 10;

    /* Check all directions radiating out from the initial direction. */
    for (i = 0; i < 7; i++)
    {
        int side_dir = side_dirs[dir][i];

        y = n_ptr->fy + ddy[side_dir];
        x = n_ptr->fx + ddx[side_dir];

        /* Illegal grid */
        if (!in_bounds_fully(y, x)) continue;

        /* Grid is not occupied, and the 2nd monster can exist in it. */
        if (cave_exist_mon(nr_ptr, y, x, FALSE, TRUE, TRUE))
        {
            /* Push the 2nd monster into the empty grid. */
            monster_swap(n_ptr->fy, n_ptr->fx, y, x);
            return (TRUE);
        }
    }

    /* We didn't find any empty, legal grids */
    return (FALSE);
}

/*
 * Process a monster's move.
 *
 * All the plotting and planning has been done, and all this function
 * has to do is move the monster into the chosen grid.
 *
 * This may involve attacking the character, breaking a glyph of
 * warding, bashing down a door, etc..  Once in the grid, monsters may
 * stumble into monster traps, hit a scent trail, pick up or destroy
 * objects, and so forth.
 *
 * A monster's move may disturb the character, depending on which
 * disturbance options are set.
 */
s16b process_move(monster_type *m_ptr, int ty, int tx, bool bash)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    u16b feat;

    /* Existing monster location, proposed new location */
    int oy, ox, ny, nx;

    /* Default move, default lack of view */
    bool do_move = TRUE;
    bool do_view = FALSE;

    /* Assume nothing */
    bool did_open_door = FALSE;
    bool did_bash_door = FALSE;
    bool did_take_item = FALSE;
    bool did_kill_item = FALSE;
    bool did_kill_body = FALSE;
    bool did_pass_wall = FALSE;
    bool did_kill_wall = FALSE;


    /* Remember where monster is */
    oy = m_ptr->fy;
    ox = m_ptr->fx;

    /* Get the destination */
    ny = ty;
    nx = tx;

    /* Check Bounds */
    if (!in_bounds(ny, nx)) return(BASE_ENERGY_MOVE);

    /* The monster is hidden in terrain, trying to attack the player.*/
    if (do_move && (m_ptr->mflag & (MFLAG_HIDE)) && (dungeon_info[ny][nx].monster_idx < 0))
    {
        monster_unhide(m_ptr);
    }

    /* The grid is occupied by the player. */
    if (dungeon_info[ny][nx].monster_idx < 0)
    {
        /* Attack if possible */
        if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
        {
            /* Player is standing on a glyph of warding */
            if (cave_player_glyph_bold(ny, nx))
            {
                /* Break glyphs of warding before attacking the player if possible */
                if (randint(BREAK_GLYPH) < r_ptr->level)
                {
                    /* Describe observable breakage */
                    if (dungeon_info[ny][nx].cave_info & (CAVE_MARK))
                    {
                        message(QString("The rune of protection is broken!"));
                    }
                    /* Destroy the rune */
                    delete_effect_idx(dungeon_info[ny][nx].effect_idx);
                }
            }

            (void)make_attack_normal(m_ptr);
        }

        /* End move */
        do_move = FALSE;
    }

    /* Can still move */
    if (do_move)
    {

        /* Get the feature in the grid that the monster is trying to enter. */
        feat = dungeon_info[ny][nx].feature_idx;

        /*
         * Monster doesn't want to hide in the feature.
         * Discover secret doors
         */
        if (feat_ff1_match(feat, FF1_SECRET | FF1_DOOR) == (FF1_SECRET | FF1_DOOR))
        {
            /* Discover secret */
            cave_alter_feat(ny, nx, FS_SECRET);

            /* Rescan the feature */
            feat = dungeon_info[ny][nx].feature_idx;

            /* Update visuals */
            if (player_can_see_bold(ny, nx)) do_view = TRUE;
        }

        /* Entering a wall */
        if (!cave_ff1_match(ny, nx, FF1_MOVE))
        {
            /* Monster passes through walls (and doors) */
            if (r_ptr->flags2 & (RF2_PASS_WALL))
            {
                /* Monster went through a wall */
                did_pass_wall = TRUE;
            }

            /* Monster flies over suitable terrain */
            else if (MONSTER_CAN_FLY(m_ptr, feat))
            {
                /* Blank test */
            }

            /* Monster destroys walls (and doors) */
            else if (r_ptr->flags2 & (RF2_KILL_WALL))
            {
                /* Noise distance depends on monster "dangerousness"  XXX */
                int noise_dist = 3 + MIN(8, m_ptr->hp / p_ptr->lev);

                /* Forget the wall */
                dungeon_info[ny][nx].cave_info &= ~(CAVE_MARK);

                /* Note that the monster killed the wall */
                if (player_can_see_bold(ny, nx))
                {
                    do_view = TRUE;

                    did_kill_wall = TRUE;

                    /* Stop everything */
                    disturb(FALSE, FALSE);
                }

                /* Output warning messages if the racket gets too loud */
                else if (m_ptr->cdis <= noise_dist)
                {
                    /* Grid is currently a door */
                    if (dungeon_info[ny][nx].is_closed_door())
                    {
                        message(QString("You hear a door being smashed open."));
                    }

                    /* Grid is anything else */
                    else
                    {
                        message(QString("You hear grinding noises."));
                    }

                    /* Stop everything if necessary */
                    disturb(FALSE, FALSE);
                }

                if (feat_ff1_match(feat, FF1_CAN_BASH))
                {
                    /* Bash through the feature */
                    /* Example: closed doors, trees */
                    cave_alter_feat(ny, nx, FS_BASH);
                }
                else
                {
                    /* Tunnel trough the walls */
                    cave_alter_feat(ny, nx, FS_TUNNEL);
                }
            }

            /* Monster bashes the door down */
            else if (bash && feat_ff1_match(feat, FF1_CAN_BASH))
            {
                /* Note that the monster bashed the feature (if visible) */
                if ((player_can_see_bold(ny, nx)) || (m_ptr->ml))
                {
                    do_view = TRUE;

                    did_bash_door = TRUE;

                    /* Stop everything */
                    disturb(FALSE, FALSE);
                }

                /* Character is not too far away */
                else if (m_ptr->cdis <= MAX_SIGHT)
                {
                    /* Grid is currently a door */
                    if (dungeon_info[ny][nx].is_closed_door())
                    {
                        message(QString("You hear a door burst open!"));
                    }
                    /* Grid is anything else */
                    else
                    {
                        message(QString("You hear a very loud noise."));
                    }

                    /* Stop everything if necessary */
                    disturb(FALSE, FALSE);
                }

                /* Just open the door sometimes */
                if (feat_ff1_match(feat, FF1_CAN_OPEN) && one_in_(2))
                {
                    cave_alter_feat(ny, nx, FS_OPEN);
                }
                /* Or break through the feature */
                else
                {
                    cave_alter_feat(ny, nx, FS_BASH);
                }
            }

            else if (feat_ff1_match(feat, FF1_CAN_OPEN))
            {
                /* Locked doors */
                if (feat_ff3_match(feat, FF3_DOOR_LOCKED))
                {
                    do_move = FALSE;
                }
                /* Ordinary doors */
                else if (feat_ff3_match(feat, FF3_DOOR_CLOSED))
                {
                    if (one_in_(5)) do_move = FALSE;
                }

                /* Handle doors in sight */
                if (player_can_see_bold(ny, nx))
                {
                    /* Do not disturb automatically */
                    do_view = TRUE;
                }
                /* Sometimes monsters are too noisy */
                else if ((m_ptr->cdis <= MAX_SIGHT) && one_in_(3))
                {

                    /* Get the feature name */
                    QString name = feature_desc(feat, TRUE, TRUE);

                    /* Show a message */
                    message(QString("You hear %1 being opened.") .arg(name));

                    /* Stop everything if necessary */
                    disturb(FALSE, FALSE);
                }

                /* Unlock the door */
                cave_alter_feat(ny, nx, FS_OPEN);

            }

            /* Paranoia -- Ignore all features not added to this code */
            else return (BASE_ENERGY_MOVE);
        }

        /* An effect is forbidding movement */
        else if (!cave_passable_bold(ny, nx))
        {
            /* Ghosts can pass */
            if (r_ptr->flags2 & (RF2_PASS_WALL))
            {
                did_pass_wall = TRUE;
            }

            /* Diggers can pass. Remove the effect */
            else if (r_ptr->flags2 & (RF2_KILL_WALL))
            {
                /* Get the index of the first effect */
                s16b x_idx = dungeon_info[ny][nx].effect_idx;

                /* Traverse the effect list of the grid */
                while (x_idx)
                {
                    /* Get the effect */
                    effect_type *x_ptr = &x_list[x_idx];

                    /* Get the associated feature */
                    u16b x_feat = x_ptr->x_f_idx;

                    /* Point to the next effect */
                    x_idx = x_ptr->next_x_idx;

                    /* Ignore effects that don't affect movement */
                    if (!x_feat || feat_ff1_match(x_feat, FF1_MOVE)) continue;

                    /* Remove the effect */
                    delete_effect_idx((s16b)(x_ptr - x_list));

                    /* Notify the player */
                    if (player_has_los_bold(ny, nx))
                    {

                        /* Get the name of the effect */
                        QString name = feature_desc(x_feat, FALSE, TRUE);

                        /* Show a message */
                        message(QString("The %1 was destroyed!") .arg(name));
                    }
                }

                did_kill_wall = TRUE;
            }

            /* Monster can't pass */
            else return (BASE_ENERGY_MOVE);
        }

        /* Glyphs */
        else if (cave_player_glyph_bold(ny, nx))
        {
            /* Describe observable breakage */
            if (dungeon_info[ny][nx].cave_info & (CAVE_MARK))
            {
                message(QString("The rune of protection is broken!"));
            }

            /* Destroy the rune */
            delete_effect_idx(dungeon_info[ny][nx].effect_idx);
        }

        /* Monsters tunnel through impassable terrain */
        else if ((r_ptr->flags2 & (RF2_KILL_WALL)) &&
                 (f_info[feat].f_flags1 & (FF1_CAN_TUNNEL)))
        {
            /* Tunnel through the wall */
            cave_alter_feat(ny, nx, FS_TUNNEL);

            /* Did kill wall */
            did_kill_wall = TRUE;
        }
    }

    /* Monster is allowed to move */
    if (do_move)
    {
        /* The grid is occupied by a monster. */
        if (dungeon_info[ny][nx].monster_idx > 0)
        {
            monster_type *n_ptr = &mon_list[dungeon_info[ny][nx].monster_idx];
            monster_race *nr_ptr = &r_info[n_ptr->r_idx];

            /* XXX - Kill (much) weaker monsters */
            if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
                (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
                (!holding_quest_artifact(n_ptr)) &&
                (!(n_ptr->mflag & (MFLAG_QUEST))) &&
                (r_ptr->mexp > nr_ptr->mexp * 2))
            {
                /* Note that the monster killed another monster (if visible) */
                did_kill_body = TRUE;

                /* Kill the monster */
                delete_monster(ny, nx);
            }

            /* Swap with or push aside the other monster */
            else
            {
                /* The other monster cannot switch places */
                if (!cave_exist_mon(nr_ptr, m_ptr->fy, m_ptr->fx, TRUE, TRUE, TRUE))
                {
                    /* Try to push it aside */
                    if (!push_aside(m_ptr, n_ptr))
                    {
                        /* Cancel move on failure */
                        do_move = FALSE;
                    }
                }
            }
        }
    }


    /* Monster can (still) move */
    if (do_move)
    {
        /* Move the monster */
        monster_swap(oy, ox, ny, nx);

        /*Mark the movement in the terrain lore, if the player is in a state to do so*/
        if (player_can_observe() && (m_ptr->ml) && (!(m_ptr->mflag & (MFLAG_FLYING))))
        {

            feature_lore *f_l_ptr = &f_l_list[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx];

            /*Check if the monster is native*/
            if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
            {
                /*Mark the lore*/
                if (f_l_ptr->f_l_native_moves < UCHAR_MAX) f_l_ptr->f_l_native_moves ++;
            }
            else
            {
                /*Mark the lore*/
                if (f_l_ptr->f_l_non_native_moves < UCHAR_MAX) f_l_ptr->f_l_non_native_moves ++;
            }
        }

        /* Cancel target when reached */
        if ((m_ptr->target_y == ny) && (m_ptr->target_x == nx))
        {
            m_ptr->target_y = 0;
            m_ptr->target_x = 0;
        }

        /* Check for monster trap */
        if cave_monster_trap_bold(ny,nx)
        {
            /* Apply trap */
            (void)apply_monster_trap(x_list[dungeon_info[ny][nx].effect_idx].x_f_idx, ny, nx, MODE_ACTION);

            /* Return if dead */
            if (!(m_ptr->r_idx)) return(BASE_ENERGY_MOVE);
        }

        /*Did a new monster get pushed into the old space?*/
        if (dungeon_info[oy][ox].monster_idx > 0)
        {
            /*Is there a trap there?*/
            if cave_monster_trap_bold(oy,ox)
            {
                /* Apply trap */
                (void)apply_monster_trap(x_list[dungeon_info[ny][nx].effect_idx].x_f_idx, oy, ox, MODE_ACTION);
            }
        }

        /* If he carries a light, update lights */
        if (r_ptr->flags2 & (RF2_HAS_LIGHT)) do_view = TRUE;



        /* Monster is visible and not cloaked */
        if (m_ptr->ml)
        {

            /* Player will always be disturbed if monster moves adjacent */
            if (m_ptr->cdis == 1) disturb(TRUE, TRUE);

            /* Hack -- ignore townspeople if strong enough  -clefs- */
            else if ((m_ptr->mflag & (MFLAG_TOWN)) && (p_ptr->lev >= 10))
            {
                /* Ignore */
            }

            /* Option -- be disturbed by all other monster movement */
            else if (disturb_move) disturb(TRUE, TRUE);

            /* Option -- be disturbed by monster movement in LOS */
            else if ((m_ptr->ml) && (disturb_near))
            {

                if ((m_ptr->project) ||
                        (((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL))) &&
                                (m_ptr->cdis < 3)))
                {
                    disturb(TRUE, TRUE);
                }
            }
        }

        /* Take or kill objects on the floor */
        if (((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
             (r_ptr->flags2 & (RF2_KILL_ITEM))) &&
            (f_info[dungeon_info[ny][nx].feature_idx].f_flags1 & (FF1_DROP)))
        {
            u32b flg3 = 0L;

            QString m_name;
            QString o_name;

            s16b this_o_idx, next_o_idx = 0;

            /* Scan all objects in the grid */
            for (this_o_idx = dungeon_info[ny][nx].object_idx; this_o_idx;
                 this_o_idx = next_o_idx)
            {
                object_type *o_ptr;

                /* Get the object */
                o_ptr = &o_list[this_o_idx];

                /* Get the next object */
                next_o_idx = o_ptr->next_o_idx;


                /* Skip gold */
                if (o_ptr->tval == TV_GOLD)
                {
                    continue;
                }

                /* React to objects that hurt the monster */
                if (o_ptr->obj_flags_1 & (TR1_SLAY_DRAGON))  flg3 |= (RF3_DRAGON);
                if (o_ptr->obj_flags_1 & (TR1_KILL_DRAGON))  flg3 |= (RF3_DRAGON);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_TROLL))   flg3 |= (RF3_TROLL);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_GIANT))   flg3 |= (RF3_GIANT);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_ORC))     flg3 |= (RF3_ORC);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_DEMON))   flg3 |= (RF3_DEMON);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_UNDEAD))  flg3 |= (RF3_UNDEAD);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_ANIMAL))  flg3 |= (RF3_ANIMAL);
                if (o_ptr->obj_flags_1 & (TR1_SLAY_EVIL))    flg3 |= (RF3_EVIL);

                /* The object (or quest related mimic) cannot be picked up by the monster */
                if (o_ptr->is_artifact() || (r_ptr->flags3 & flg3) ||
                   (o_ptr->obj_flags_3 & (TR3_NEVER_PICKUP)))
                {
                    /* Only give a message for "take_item" */
                    if (r_ptr->flags2 & (RF2_TAKE_ITEM))
                    {
                        /* Take note */
                        did_take_item = TRUE;

                        /* Describe observable situations */
                        if (m_ptr->ml && player_has_los_bold(ny, nx))
                        {
                            /* Get the object name */
                            o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

                            /* Get the monster name */
                            m_name = monster_desc(m_ptr, 0x04);

                            /* Dump a message */
                            message(QString("%1 tries to pick up %2, but fails.") .arg(capitalize_first(m_name))
                                    .arg(o_name));
                        }
                    }
                }

                /* Pick up the item */
                else if (r_ptr->flags2 & (RF2_TAKE_ITEM))
                {
                    object_type *i_ptr;
                    object_type object_type_body;

                    /* Take note */
                    did_take_item = TRUE;

                    /* Describe observable situations */
                    if (player_has_los_bold(ny, nx) && m_ptr->ml)
                    {
                        /* Get the object name */
                        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

                        /* Get the monster name */
                        m_name = monster_desc(m_ptr, 0x04);

                        /* Dump a message */
                        message(QString("%1 picks up %2.") .arg(capitalize_first(m_name)) .arg(o_name));
                    }

                    /* Get local object */
                    i_ptr = &object_type_body;

                    /* Obtain local object */
                    i_ptr->object_copy(o_ptr);

                    /* Delete the object */
                    delete_object_idx(this_o_idx);

                    /* Carry the object */
                    (void)monster_carry(dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx, i_ptr);
                }

                /* Destroy the item */
                else
                {
                    /* Take note */
                    did_kill_item = TRUE;

                    /* Describe observable situations */
                    if (player_has_los_bold(ny, nx))
                    {
                        /* Get the object name */
                        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

                        /* Get the monster name */
                        m_name = monster_desc(m_ptr, 0x04);

                        /* Dump a message */
                        color_message(QString("%1 crushes %2.") .arg(capitalize_first(m_name)) .arg(o_name), TERM_RED);
                    }

                    /* Delete the object */
                    delete_object_idx(this_o_idx);
                }
            }
        }
    }             /* End of monster's move */



    /* Notice changes in view */
    if (do_view)
    {
        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }

    /* Learn things from observable monster */
    if ((m_ptr->ml) && player_can_observe())
    {
        const feature_type *f_ptr = &f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx];
        feature_lore *f_l_ptr = &f_l_list[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx];

        /*Mark the monster lore*/
        if (do_move)
        {
            u32b native = f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx].f_flags3;
            native &= r_ptr->r_native;
            l_ptr->r_l_native |= native;
        }

        /* Monster is flying*/
        if (m_ptr->mflag & (MFLAG_FLYING))
        {
            /* Monster and terrain lore*/
            l_ptr->r_l_flags3 |= (RF3_FLYING);
            if ((f_ptr->f_flags2 & (FF2_CAN_FLY)) && (dungeon_info[m_ptr->fy][m_ptr->fx].cave_info & (CAVE_SEEN)))
            {
                f_l_ptr->f_l_flags2 |= FF2_CAN_FLY;
            }
        }

        /* Monster opened a door */
        if (did_open_door) l_ptr->r_l_flags2 |= (RF2_OPEN_DOOR);

        /* Monster bashed a door */
        if (did_bash_door) l_ptr->r_l_flags2 |= (RF2_BASH_DOOR);

        /* Monster tried to pick something up */
        if (did_take_item) l_ptr->r_l_flags2 |= (RF2_TAKE_ITEM);

        /* Monster tried to crush something */
        if (did_kill_item) l_ptr->r_l_flags2 |= (RF2_KILL_ITEM);

        /* Monster ate another monster */
        if (did_kill_body) l_ptr->r_l_flags2 |= (RF2_KILL_BODY);

        /* Monster passed through a wall */
        if (did_pass_wall) l_ptr->r_l_flags2 |= (RF2_PASS_WALL);

        /* Monster destroyed a wall */
        if (did_kill_wall) l_ptr->r_l_flags2 |= (RF2_KILL_WALL);
    }

    /*Monster is flying*/
    if (m_ptr->mflag & (MFLAG_FLYING)) return (BASE_ENERGY_MOVE);

    if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
    {
        return(f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx].native_energy_move);
    }
    else return(f_info[dungeon_info[m_ptr->fy][m_ptr->fx].feature_idx].non_native_energy_move);
}
