/* File: was melee2.c */

/*
 * Copyright (c) 2001 Leon Marrick & Bahman Rabii, Ben Harrison,
 * James E. Wilson, Robert A. Koeneke, Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/store.h"


/* Helpers for the cloud_surround function */

#define MAX_CLOUD_CHOICES 22

struct breath_gf
{
    u32b flag;
    int gf_value;
};

/*
 * Events triggered by the various flags.
 */
static const struct breath_gf breath_to_gf[] =
{
    { RF4_BRTH_ACID,  GF_ACID },
    { RF4_BRTH_ELEC,  GF_ELEC },
    { RF4_BRTH_FIRE,  GF_FIRE },
    { RF4_BRTH_COLD,  GF_COLD },
    { RF4_BRTH_POIS,  GF_POIS },
    { RF4_BRTH_LIGHT, GF_LIGHT },
    { RF4_BRTH_DARK,  GF_DARK },
    { RF4_BRTH_CONFU, GF_CONFUSION },
    { RF4_BRTH_SHARD, GF_SHARD },
    { RF4_BRTH_INER,  GF_INERTIA_NPP },
    { RF4_BRTH_GRAV,  GF_GRAVITY },
    { RF4_BRTH_NEXUS, GF_NEXUS },
    { RF4_BRTH_NETHR, GF_NETHER },
    { RF4_BRTH_CHAOS, GF_CHAOS },
    { RF4_BRTH_DISEN, GF_DISENCHANT },
    { RF4_BRTH_TIME,  GF_TIME },
};

/*
 * Some monsters are surrounded by gas, terrible heat, loud noises, spores,
 * etc.  Process any such affects.
 *
 * The Nazgul surround themselves with darkness, so they all have IS_LIT.
 */
void cloud_surround(int r_idx, int *typ, int *dam, int *rad)
{
    u32b i;
    monster_race *r_ptr = &r_info[r_idx];
    int choices[MAX_CLOUD_CHOICES];
    int count = 0;

    *typ = 0;
    *dam = div_round(r_ptr->level, 4);
    *rad = 1 + r_ptr->level / 30;

    /* Unique monsters have especially strong effects */
    if (r_ptr->flags1 & (RF1_UNIQUE))
    {
        *rad += 1;
        *dam *= 2;
    }

    /*** Determine the kind of cloud we're supposed to be giving off ***/

    /* If breaths and attrs match, the choice is clear. */
    if (r_ptr->flags4 & RF4_BRTH_ALL)
    {
        /* For each listed flag, add it to the possible effects */
        for (i = 0; i < N_ELEMENTS(breath_to_gf); i++)
        {
            const struct breath_gf *brth = &breath_to_gf[i];

            /* If they have the breath, add it to the list */
            if (r_ptr->flags4 & brth->flag)
            {
                choices[count++] = brth->gf_value;
            }

        }
    }

    /* Molds release spores */
    if (r_ptr->d_char == 'm')
    {
        choices[count++] = GF_SPORE;
    }

    /* The Nazgul darken everything nearby, and possibly blind the player  */
    if (r_ptr->d_char == 'W')
    {
        choices[count++] = GF_DARK;
    }
    /* So do silver jellies*/
    if (r_ptr->d_char == 'j')
    {
        choices[count++] = GF_DARK_WEAK;
    }

    if (count)
    {
        i = randint0(count);
        *typ = choices[i];
    }
}




/*
 * Terrified monsters will turn to fight if they are slower than the
 * character, and closer to him than this distance.
 */

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


/*alert others in pack about something using the m_flag, and wake them up*/
static void tell_allies(int y, int x, u32b flag)
{
    monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int i;

    /* Scan all other monsters */
    for (i = mon_max - 1; i >= 1; i--)
    {

        /* Access the monster */
        monster_type *n_ptr = &mon_list[i];
        monster_race *nr_ptr = &r_info[n_ptr->r_idx];

        /* Access the monster */
        n_ptr = &mon_list[i];
        nr_ptr = &r_info[n_ptr->r_idx];

        /* Ignore dead monsters */
        if (!n_ptr->r_idx) continue;

        /* Ignore monsters with the wrong symbol */
        if (r_ptr->d_char != nr_ptr->d_char) continue;

        /* Ignore monsters not in LOS */
        if (!los(y, x, n_ptr->fy, n_ptr->fx)) continue;

        /* Activate all other monsters and communicate to them */
        n_ptr->mflag |= (MFLAG_ACTV | flag | MFLAG_DESPERATE);

    }
}


/*
 * Get a target for a monster using the special "townsman" AI.
 */
static void get_town_target(monster_type *m_ptr)
{
    int i;
    int y, x;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Clear target */
    m_ptr->target_y = 0;
    m_ptr->target_x = 0;

    /* Hack -- Usually choose a random store, but not cats and dogs */
    if ((r_ptr->d_char != 'f') && (r_ptr->d_char != 'C') && (!one_in_(3)))
    {
        /* shop base - note that shop power is the shop -1*/
        i = rand_int(MAX_STORES);

        /* Try to find the store XXX XXX */
        for (y =1; y < p_ptr->cur_map_hgt - 1; y++)
        {
            for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
            {
                if (dungeon_info[y][x].is_store())
                {

                    /* Is our store */
                    if (f_info[dungeon_info[y][x].feature_idx].f_power == i)
                    {
                        m_ptr->target_y = y;
                        m_ptr->target_x = x;
                        break;
                    }
                }
            }

            /* Store found */
            if (m_ptr->target_y) return;
        }
    }

    /* No store chosen */
    if (!m_ptr->target_y)
    {
        for (i = 0;; i++)
        {
            /* Pick a grid on the edge of the map (simple test) */
            if (i < 100)
            {
                if (one_in_(2))
                {
                    /* Pick a random location along the N/S walls */
                    y = rand_range(1, p_ptr->cur_map_hgt - 1);

                    if (one_in_(2)) x = p_ptr->cur_map_wid - 2 ;
                    else            x = 2 ;

                }
                else
                {
                    /* Pick a random location along the E/W walls */
                    x = rand_range(1, p_ptr->cur_map_wid -1);

                    if (one_in_(2)) y = p_ptr->cur_map_hgt -2 ;
                    else            y = 2 ;
                }
            }
            else
            {
                y = rand_range(1, p_ptr->cur_map_hgt - 2);
                x = rand_range(1, p_ptr->cur_map_wid - 2);
            }

            /* Require "empty" floor grids */
            if (cave_empty_bold(y, x))
            {
                m_ptr->target_y = y;
                m_ptr->target_x = x;
                break;
            }
        }
    }
}

/*
 * Monster takes its turn.
 */
static s16b process_monster(monster_type *m_ptr)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    int i, k, y, x;
    int ty, tx;
    int chance = 0;
    int choice = 0;
    int dir;

    QString m_name;;

    bool fear = FALSE;

    bool bash = FALSE;

    /* Assume the monster doesn't have a target */
    bool must_use_target = FALSE;

    /* Will the monster move randomly? */
    bool random_move = FALSE;

    /* Calculate the monster's preferred combat range when needed */
    if (m_ptr->min_range == 0) find_range(m_ptr);

    /* Monster is in active mode. */
    if (m_ptr->mflag & (MFLAG_ACTV))
    {
        /*
         * Character is outside of scanning range and well outside
         * of sighting range.  Monster does not have a target.
         */
        if ((m_ptr->cdis >= FLEE_RANGE) && (m_ptr->cdis > r_ptr->aaf) &&
            (!m_ptr->target_y) && (!m_ptr->target_x))
        {

            /*Monster is no longer active*/
            m_ptr->mflag &= ~(MFLAG_ACTV | MFLAG_NEED_PASS_WALL_FLOW );
        }
    }


    /* Monster is in passive mode. */
    else
    {
        /* Character is inside scanning range */
        if (m_ptr->cdis <= r_ptr->aaf) m_ptr->mflag |= (MFLAG_ACTV);

        /* Monster has a target */
        else if ((m_ptr->target_y) && (m_ptr->target_x)) m_ptr->mflag |= (MFLAG_ACTV);
    }

    /*
     * Special handling if the first turn a monster has after
     * being attacked by the player, but the player is out of sight
     */
    if (m_ptr->mflag & (MFLAG_HIT_BY_RANGED))
    {
        /*Monster will be very upset if it can't see the player*/
        if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
        {
            m_ptr->mflag |= (MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);

            /*if part of a pack, let them know*/
            if ((r_ptr->flags1 & (RF1_FRIENDS)) ||
                (r_ptr->flags1 & (RF1_FRIEND)) ||
                (r_ptr->flags1 & (RF1_ESCORT)) ||
                (r_ptr->flags1 & (RF1_ESCORTS)))
            {
                tell_allies(m_ptr->fy, m_ptr->fx, MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);
            }

            /*Monsters with ranged attacks will try to cast a spell*/
            if (r_ptr->freq_ranged) m_ptr->mflag |= (MFLAG_ALWAYS_CAST);

            /*Tweak the monster speed*/
            m_ptr->mflag &= ~(MFLAG_SLOWER | MFLAG_FASTER);
            if (one_in_(3))	m_ptr->mflag |= (MFLAG_SLOWER);
            else if (one_in_(2)) m_ptr->mflag |= (MFLAG_FASTER);
            calc_monster_speed(m_ptr->fy, m_ptr->fx);
        }

        /*clear the flag*/
        m_ptr->mflag &= ~(MFLAG_HIT_BY_RANGED);

    }

    /*This if the first turn a monster has after being attacked by the player*/
    if (m_ptr->mflag & (MFLAG_HIT_BY_MELEE))
    {
        /*
         * Monster will be very upset if it isn't next to the
         * player (pillar dance, hack-n-back, etc)
         */
        if (m_ptr->cdis > 1)
        {
            m_ptr->mflag |= (MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);

            /*if part of a pack, let them know*/
            if ((r_ptr->flags1 & (RF1_FRIENDS)) ||
                (r_ptr->flags1 & (RF1_FRIEND)) ||
                (r_ptr->flags1 & (RF1_ESCORT)) ||
                (r_ptr->flags1 & (RF1_ESCORTS)))
            {
                tell_allies(m_ptr->fy, m_ptr->fx, MFLAG_AGGRESSIVE);
            }

            /*Monsters with ranged attacks will try to cast a spell*/
            if (r_ptr->freq_ranged) m_ptr->mflag |= (MFLAG_ALWAYS_CAST);

            /*Tweak the monster speed*/
            m_ptr->mflag &= ~(MFLAG_SLOWER | MFLAG_FASTER);
            if (one_in_(3))	m_ptr->mflag |= (MFLAG_SLOWER);
            else if (one_in_(2)) m_ptr->mflag |= (MFLAG_FASTER);
            calc_monster_speed(m_ptr->fy, m_ptr->fx);
        }

        /*clear the flag*/
        m_ptr->mflag &= ~(MFLAG_HIT_BY_MELEE);

    }

    /* A monster in passive mode will end its turn at this point. */
    if (!(m_ptr->mflag & (MFLAG_ACTV))) return (BASE_ENERGY_MOVE);

    /*Paranoia - Find the best flow for a monster*/
    if (m_ptr->using_flow == NEED_FLOW) find_best_flow(m_ptr);

    /* Hack -- Always redraw the current target monster health bar */
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

    /* Attempt to multiply if able to and allowed */
    if ((r_ptr->flags2 & (RF2_MULTIPLY)) &&
        (!m_ptr->m_timed[MON_TMD_CONF]) && (!m_ptr->m_timed[MON_TMD_FEAR]) &&
        (mon_cnt < z_info->m_max - 50) && (!(m_ptr->mflag & (MFLAG_STERILE))))
    {
        /* Count the adjacent monsters */
        for (k = 0, y = m_ptr->fy - 1; y <= m_ptr->fy + 1; y++)
        {
            for (x = m_ptr->fx - 1; x <= m_ptr->fx + 1; x++)
            {
                /* Check Bounds */
                if (!in_bounds(y, x)) continue;

                /* Count monsters */
                if (dungeon_info[y][x].monster_idx > 0) k++;
            }
        }

        /* Hack -- multiply slower in crowded areas */
        if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
        {
            /* Try to multiply */
            if (multiply_monster(dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx, FALSE))
            {
                /* Make a sound */
                sound(MSG_MULTIPLY);

                /* Take note if visible */
                if (m_ptr->ml)
                {
                    l_ptr->r_l_flags2 |= (RF2_MULTIPLY);
                }

                /* Multiplying takes energy */
                return (BASE_ENERGY_MOVE);
            }
        }
    }

    /* Make sure messages such as wakes up, etc. come before attack messages */
    flush_monster_messages();

    /*** Ranged attacks ***/

    /*Monster can cast spells*/
    if (r_ptr->freq_ranged)
    {
        int tar_y, tar_x;

        /* Extract the ranged attack probability. */
        chance = r_ptr->freq_ranged;

        /*certain conditions always cause a monster to always cast*/
        if (m_ptr->mflag & (MFLAG_ALWAYS_CAST)) chance = 100;

        /* Heavy spell casters will sit back and cast */
        else if (r_ptr->freq_ranged > 20)
        {
            if ((m_ptr->best_range >= m_ptr->cdis +2) &&
                ((m_ptr->best_range / 2) <= m_ptr->cdis))
            {

                /* Heavy spell casters will sit back and cast */
                if (m_ptr->mana > r_ptr->mana / 5)
                {
                    chance += (100 - chance) / 5;
                }

                /* Creatures that don't move never like to get too close */
                if (r_ptr->flags1 & (RF1_NEVER_MOVE)) chance += (100 - chance) / 3;

                /* Spellcasters that don't strike never like to get too close */
                else if (r_ptr->flags1 & (RF1_NEVER_BLOW)) chance += (100 - chance) / 3;

                /*Monsters who have had dangerous attacks happen to them are more extreme*/
                else if (m_ptr->mflag & (MFLAG_ATTACKED_BAD))
                {
                    chance += (100 - chance) / 3;
                }
            }

            /* Breathers like point blank range */
            else if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
             (r_ptr->flags5 & (RF5_BREATH_MASK)) ||
             (r_ptr->flags6 & (RF6_BREATH_MASK)) ||
             (r_ptr->flags7 & (RF7_BREATH_MASK))) &&
             (m_ptr->cdis < 6) &&
             (m_ptr->hp > m_ptr->maxhp / 2))
            {
                chance += (100 - chance) / 10;
            }

        }

        /*Monsters marked as aggressive or Desperatedo the same*/
        else if (m_ptr->mflag & (MFLAG_AGGRESSIVE | MFLAG_DESPERATE))
        {
            chance += ((100 - chance) / 10);
        }

        /* Cannot use ranged attacks when confused. */
        if (m_ptr->m_timed[MON_TMD_CONF]) chance = 0;

        /* Stunned monsters use ranged attacks half as often. */
        else if ((chance) && (m_ptr->m_timed[MON_TMD_STUN])) chance /= 2;

        /* Hidden creatures love ranged attacks */
        else if (m_ptr->mflag & (MFLAG_HIDE)) chance += ((100 - chance) / 3);

        /*
         * Monster does not have a path toward the player.  Cast much more often.
         */
        if ((m_ptr->using_flow == NEED_FLOW) && !(m_ptr->m_timed[MON_TMD_STUN]) && !(m_ptr->m_timed[MON_TMD_CONF]))
        {
            chance += ((100 - chance) * 3 / 4);
        }

        m_name = monster_desc(m_ptr, 0);

        /* Monster can use ranged attacks */
        if (rand_int(100) < chance)
        {
            /* Pick a ranged attack */
            choice = choose_ranged_attack(dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx, &tar_y, &tar_x);
        }

        /* Selected a ranged attack? */
        if (choice != 0)
        {
            /* The monster is hidden */
            if (m_ptr->mflag & (MFLAG_HIDE)) monster_unhide(m_ptr);

            /* Execute said attack */
            make_attack_ranged(m_ptr, choice, tar_y, tar_x);

            /* End turn */
            return(BASE_ENERGY_MOVE);
        }
    }

    /*** Movement ***/

    /* Assume no movement */
    ty = 0;
    tx = 0;

    /*
     * Innate semi-random movement.  Monsters adjacent to the character
     * can always strike accurately at him (monster isn't confused).
     */
    if ((r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25)) && (m_ptr->cdis > 1))
    {
        chance = 0;

        /* RAND_25 and RAND_50 are cumulative */
        if (r_ptr->flags1 & (RF1_RAND_25))
        {
            chance += 25;
            if (m_ptr->ml) l_ptr->r_l_flags1 |= (RF1_RAND_25);
        }
        if (r_ptr->flags1 & (RF1_RAND_50))
        {
            chance += 50;
            if (m_ptr->ml) l_ptr->r_l_flags1 |= (RF1_RAND_50);
        }

        /* Chance of moving randomly */
        if (rand_int(100) < chance) random_move = TRUE;
    }

    /* Monster isnt' moving randomly, isnt' running away
     * doesn't hear or smell the character
     */
    if (!random_move)
    {
        /*
         * First, monsters who can't cast, are aggressive, and
         * are not afraid just want to charge
         */
        if (!m_ptr->m_timed[MON_TMD_FEAR])
        {

            if (m_ptr->mflag & (MFLAG_AGGRESSIVE | MFLAG_DESPERATE) && (!r_ptr->freq_ranged))
            {
                m_ptr->target_y = 0;
                m_ptr->target_x = 0;
            }

            /*Monster can see the player*/
            if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
            {
                m_ptr->target_y = 0;
                m_ptr->target_x = 0;
            }
        }

        /*
         * Monster has a known target, but not only if the target is because
         * there is not a good flow
         */
        if ((m_ptr->target_y) && (m_ptr->target_x) && !(m_ptr->using_flow == NEED_FLOW)) must_use_target = TRUE;
    }

    /* Monster is using the special "townsman" AI */
    else if (m_ptr->mflag & (MFLAG_TOWN))
    {
        /* Always have somewhere to go */
        if ((!m_ptr->target_y) || (!m_ptr->target_x) ||
            (dungeon_info[m_ptr->fy][m_ptr->fx].is_store()))
        {
            /* Get a new target */
            get_town_target(m_ptr);
        }

        else if ((m_ptr->target_y == m_ptr->fy) &&
                 (m_ptr->target_x == m_ptr->fx))
        {
            /* go to a new target */
            get_town_target(m_ptr);
        }

        /* Not interested in the character */
        must_use_target = TRUE;
    }

    /*** Find a target to move to ***/

    /* Monster is genuinely confused */
    if ((m_ptr->m_timed[MON_TMD_CONF]) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))))
    {
        /* Choose any direction except five and zero */
        dir = rand_int(8);

        /* Monster can try to wander into /anything/... */
        ty = m_ptr->fy + ddy_ddd[dir];
        tx = m_ptr->fx + ddx_ddd[dir];
    }

    /* Monster isn't confused, just moving semi-randomly */
    else if (random_move)
    {

        int start = rand_int(8);
        bool dummy;

        /* Is the monster scared? */
        if ((!(r_ptr->flags1 & (RF1_NEVER_MOVE))) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0) &&
            ((m_ptr->min_range >= FLEE_RANGE) ||
             (m_ptr->m_timed[MON_TMD_FEAR])))
        {
            fear = TRUE;
        }

        /* Look at adjacent grids, starting at random. */
        for (i = start; i < 8 + start; i++)
        {
            y = m_ptr->fy + ddy_ddd[i % 8];
            x = m_ptr->fx + ddx_ddd[i % 8];

            /* Accept first passable grid. */
            if (cave_passable_mon(m_ptr, y, x, &dummy) != 0)
            {
                ty = y;
                tx = x;
                break;
            }
        }

        /* No passable grids found */
        if ((ty == 0) && (tx == 0)) return (BASE_ENERGY_MOVE);

        /* Cannot move, target grid does not contain the character */
        if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) &&
            (dungeon_info[ty][tx].monster_idx >= 0))
        {
            /* Cannot move */
            return(BASE_ENERGY_MOVE);
        }
    }

    /* Normal movement */
    else
    {
        /* Choose a pair of target grids, or cancel the move. */
        if (!get_move(m_ptr, &ty, &tx, &fear, must_use_target))
            return(BASE_ENERGY_MOVE);

    }

    /* Calculate the actual move.  Cancel move on failure to enter grid. */
    if (!make_move(m_ptr, &ty, &tx, fear, &bash)) return (BASE_ENERGY_MOVE);


    /* Change terrain, move the monster, handle secondary effects, end turn. */
    return (process_move(m_ptr, ty, tx, bash));

}


/*
 * Monster regeneration of recovery from all temporary
 * conditions.
 *
 * This function is called a lot, and is therefore fairly expensive.
 */
static void recover_monster(monster_type *m_ptr)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    int m_idx = dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx;
    bool visible = FALSE;

    /* Visible monsters must be both seen and noticed */
    if (m_ptr->ml)
    {
        visible = TRUE;
    }

    /* Handle any area-effects of the monster - only if active */
    if ((r_ptr->flags2 & (RF2_CLOUD_SURROUND)) &&
        (m_ptr->mflag & (MFLAG_ACTV)))
    {
        /* Assume no affect */
        bool affect = FALSE;

        int typ, dam, rad;

        rad = (r_ptr->flags2 & (RF2_POWERFUL) ? 2 : 1);

        /*just a minimal of damage*/
        dam = MAX(1, m_ptr->hp / 100);

        /* The Nazgul sometimes drain light */
        if (r_ptr->d_char == 'W')
        {
            if (one_in_(2)) affect = TRUE;
        }

        /* Silver jellies drain light only if their grid is lit */
        else if ((r_ptr->d_char == 'j') &&
                 (dungeon_info[m_ptr->fy][m_ptr->fx].cave_info & (CAVE_GLOW)))
        {
            affect = TRUE;
        }

        /* Other monsters are more likely to emit a cloud when they are closer, but
         * must always be line of sight */
        else if ((one_in_(m_ptr->cdis)) &&
                 (player_can_fire_bold(m_ptr->fy, m_ptr->fx))) affect = TRUE;

        /* Affect surroundings if appropriate */
        if (affect)
        {
            /* Get information */
            cloud_surround(m_ptr->r_idx, &typ, &dam, &rad);

            /* Learn about monster (before visibility changes) */
            if ((m_ptr->ml) && (r_ptr->flags2 & (RF2_CLOUD_SURROUND)))
            {
                l_ptr->r_l_flags2 |= (RF2_CLOUD_SURROUND);
            }

            /* Release of cloud (can affect visibility) */
            if (typ) mon_cloud(dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx, typ,
                               dam, rad);
        }
    }

    /* Monster is sleeping, but character is within detection range */
    if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (m_ptr->cdis <= r_ptr->aaf))
    {
        /* Aggravated by the player */
        if (p_ptr->state.aggravate)
        {
            /* Reset sleep counter */
            mon_clear_timed(m_idx, MON_TMD_SLEEP , MON_TMD_FLG_NOMESSAGE);

            /* Notice the "waking up" */
            if (visible)
            {
                QString m_name;;

                /* Acquire the monster name */
                m_name = monster_desc(m_ptr, 0);

                add_monster_message(m_name, m_idx, MON_MSG_WAKES_UP);
            }
        }

        /* Standard noise */
        else
        {
            int divisor, d;

            /* Monsters are disturbed less if far away */
            divisor = 300 + m_ptr->cdis * 200;

            /* Monsters are disturbed more if in LOS */
            if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) divisor /= 2;

            /* Get disturbance (additional noise counts double) */
            d = div_round(total_wakeup_chance + add_wakeup_chance, divisor);

            /* Still asleep */

            if (m_ptr->m_timed[MON_TMD_SLEEP] > d)
            {
                /* Monster's sleep is disturbed */
                mon_dec_timed(m_idx, MON_TMD_SLEEP, d , MON_TMD_FLG_NOMESSAGE);

                /* Notice the "not waking up" */
                if (visible)
                {

                    /* Hack -- Count the ignores */
                    if (l_ptr->ignore < UCHAR_MAX)
                    {
                        l_ptr->ignore++;
                    }

                    /* We are making a substantial amount of extra noise */
                    if (add_wakeup_chance >= 1000)
                    {
                        QString m_name;

                        /* Acquire the monster name */
                        m_name = monster_desc(m_ptr, 0);

                        /* Warning */
                        add_monster_message(m_name, m_idx, MON_MSG_STIRS);
                    }
                }
            }

            /* Just woke up */
            else
            {
                /* Monster's sleep is disturbed */
                mon_clear_timed(m_idx, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);

                /* Notice the "waking up" */
                if (visible)
                {
                    QString m_name;;

                    /* Acquire the monster name */
                    m_name = monster_desc(m_ptr, 0);

                    add_monster_message(m_name, m_idx, MON_MSG_WAKES_UP);

                    /* Hack -- Count the wakings */
                    if (l_ptr->wake < UCHAR_MAX)
                    {
                        l_ptr->wake++;
                    }
                }
            }
        }
    }

    /* Recover from stuns */
    if (m_ptr->m_timed[MON_TMD_STUN])
    {
        if (m_ptr->m_timed[MON_TMD_STUN] == 1)
        {
            mon_clear_timed(m_idx, MON_TMD_STUN, MON_TMD_FLG_NOTIFY);
        }

        else mon_dec_timed(m_idx, MON_TMD_STUN, 1 , MON_TMD_FLG_NOMESSAGE);

    }

    /* Recover from confusion */
    if (m_ptr->m_timed[MON_TMD_CONF])
    {
        if (m_ptr->m_timed[MON_TMD_CONF] == 1)
        {
            mon_clear_timed(m_idx, MON_TMD_CONF, MON_TMD_FLG_NOTIFY);
        }

        else mon_dec_timed(m_idx, MON_TMD_CONF, 1 , MON_TMD_FLG_NOMESSAGE);

    }

    /* Recover courage */
    if (m_ptr->m_timed[MON_TMD_FEAR] > 0)
    {
        if (m_ptr->m_timed[MON_TMD_FEAR] == 1)
        {
            mon_clear_timed(m_idx, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY);
            /*re-calculate minimum range */
            find_range(m_ptr);
        }

        else mon_dec_timed(m_idx, MON_TMD_FEAR, 1 , MON_TMD_FLG_NOMESSAGE);
    }

    /*
     * Handle haste counter
     */
    if (m_ptr->m_timed[MON_TMD_FAST])
    {
        if (m_ptr->m_timed[MON_TMD_FAST] == 1)
        {
            mon_clear_timed(m_idx, MON_TMD_FAST, MON_TMD_FLG_NOTIFY);
        }

        else mon_dec_timed(m_idx, MON_TMD_FAST, 1 , MON_TMD_FLG_NOMESSAGE);

    }

    /*
     * Handle slow counter
     */
    if (m_ptr->m_timed[MON_TMD_SLOW])
    {
        if (m_ptr->m_timed[MON_TMD_SLOW] == 1)
        {
            mon_clear_timed(m_idx, MON_TMD_SLOW, MON_TMD_FLG_NOTIFY);
        }

        else mon_dec_timed(m_idx, MON_TMD_SLOW, 1 , MON_TMD_FLG_NOMESSAGE);
    }

    /* Hack -- Update the health and mana bar (always) */
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);


}


/*
  * Sorting hook - vector of movement moments.
  * Sort by moment in decreasing order
  */
static bool sort_mon_moment_info(move_moment_type m1, move_moment_type m2)
{
    if (m1.moment >= m2.moment) return TRUE;
    return (FALSE);
}

/*
  * Process monsters, the character, and other entities.  -LM-
  *
  * Give the character energy.  If the character has >= 100 energy,
  * store character index for later movement.
  *
  * Every ten game turns, allow monsters to recover from temporary con-
  * ditions.  Every 100 game turns, regenerate monsters.  Give energy to
  * each monster, store monster index of all monster with >= energy for
  * later movement.
  *
  * All entities that move this turn are sorted by "movement moment",
  * the exact instant within the course of a game turn in which the
  * entity has exactly 100 energy, and may move.  Lower movement moments
  * take priority.
  */
void process_entities(void)
{
    int i;
    int energy_per_turn, old_energy, moment;
    int idx;

    monster_type *m_ptr;

    /* Clear the moment vector */
    mon_moment_info.clear();

    p_ptr->player_turn  = FALSE;

    /* Give the character some energy (unless leaving) */
    if (!p_ptr->leaving_level)
    {
        byte energy_gain = calc_energy_gain(p_ptr->state.p_speed);

        /* Give character energy */
        p_ptr->p_energy += energy_gain;

        /* Can the character move? */
        if (p_ptr->p_energy >= ENERGY_TO_MOVE)
        {
            /* Determine how much energy the character gets per turn */
            energy_per_turn = energy_gain;

            /* Note how much energy the character had last turn */
            old_energy = p_ptr->p_energy - energy_per_turn;

            /* Calculate movement moment - Hugo Kornelis - */
            moment = 100 * (ENERGY_TO_MOVE - old_energy) / (energy_per_turn);

            /* Insert character into movement table */
            move_moment_type this_mon_move;
            this_mon_move.m_idx = -1;
            this_mon_move.moment = moment;
            mon_moment_info.append(this_mon_move);
        }
    }

    /* Process the monsters */
    for (i = 1; i < mon_max; i++)
    {
        /* Get the monster */
        m_ptr = &mon_list[i];

        /* Ignore dead monsters */
        if (!m_ptr->r_idx) continue;

        energy_per_turn = calc_energy_gain(m_ptr->m_speed);

        /* Give this monster some energy */
        m_ptr->m_energy += energy_per_turn;

        /* Ignore monsters with less than 100 energy */
        if (m_ptr->m_energy < ENERGY_TO_MOVE) continue;

        /* Handle temporary monster attributes and regeneration */
        p_ptr->message_append_start();
        recover_monster(m_ptr);
        p_ptr->message_append_stop();

        /* Insert monster into the movement moment table */
        move_moment_type this_mon_move;
        this_mon_move.m_idx = i;
        this_mon_move.moment = moment;
        /* Calculate movement moment - Hugo Kornelis - */
        old_energy = m_ptr->m_energy - energy_per_turn;
        moment = 100 * (ENERGY_TO_MOVE - old_energy) / (energy_per_turn);
        this_mon_move.moment = moment;
        mon_moment_info.append(this_mon_move);
    }

    /* Sort the movement table by decreasing movement moment*/
    qSort(mon_moment_info.begin(), mon_moment_info.end(), sort_mon_moment_info);

    /* Process monsters and the character, in order of priority */
    for (i = 0; i < mon_moment_info.size(); i++)
    {
        /* Get next entity index*/
        idx = mon_moment_info[i].m_idx;

        /* This is a monster */
        if (idx > 0)
        {
            /* Character is dead or leaving the current level */
            if (p_ptr->leaving_level) continue;

            /* Get the monster race */
            m_ptr = &mon_list[idx];

            /* Paranoia - Ignore dead monsters */
            if (!m_ptr->r_idx) continue;

            /*sleeping monsters don't get a move*/
            if (m_ptr->m_timed[MON_TMD_SLEEP])
            {
                /*Burn some energy and continue*/
                if (m_ptr->m_energy >= ENERGY_TO_MOVE) m_ptr->m_energy -= BASE_ENERGY_MOVE;

                continue;
            }

            /* Require that monster still have at least 100 energy */
            if (m_ptr->m_energy >= ENERGY_TO_MOVE)
            {
                /* Monster takes a turn */
                p_ptr->message_append_start();
                m_ptr->m_energy -= process_monster(m_ptr);
                p_ptr->message_append_stop();
            }

        }

        /* This is the character */
        else if (idx < 0)
        {
            /* Let the character take a turn */
            process_player();
            return;
        }
    }
}

