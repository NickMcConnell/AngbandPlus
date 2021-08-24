/*
 * File: game-world.c
 * Purpose: Game core management of the game world
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


bool server_generated;      /* The server exists */
bool server_state_loaded;   /* The server state was loaded from a savefile */
u32b seed_flavor;           /* Hack -- consistent object colors */
hturn turn;                 /* Current game turn */


/*
 * This table allows quick conversion from "speed" to "energy"
 * The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
 * Note that table access is *much* quicker than computation.
 *
 * Note that the table has been changed at high speeds.  From
 * "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
 * at speeds above "Fast (+30)", one approaches an asymptotic
 * effective limit of 50 energy per turn.  This means that it
 * is relatively easy to reach "Fast (+30)" and get about 40
 * energy per turn, but then speed becomes very "expensive",
 * and you must get all the way to "Fast (+50)" to reach the
 * point of getting 45 energy per turn.  After that point,
 * further increases in speed are more or less pointless,
 * except to balance out heavy inventory.
 *
 * Note that currently the fastest monster is "Fast (+30)".
 */
static const byte extract_energy[200] =
{
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* S-50 */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    /* S-40 */     2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    /* S-30 */     2,  2,  2,  2,  2,  2,  2,  3,  3,  3,
    /* S-20 */     3,  3,  3,  3,  3,  4,  4,  4,  4,  4,
    /* S-10 */     5,  5,  5,  5,  6,  6,  7,  7,  8,  9,
    /* Norm */    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    /* F+10 */    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    /* F+20 */    30, 31, 32, 33, 34, 35, 36, 36, 37, 37,
    /* F+30 */    38, 38, 39, 39, 40, 40, 40, 41, 41, 41,
    /* F+40 */    42, 42, 42, 43, 43, 43, 44, 44, 44, 44,
    /* F+50 */    45, 45, 45, 45, 45, 46, 46, 46, 46, 46,
    /* F+60 */    47, 47, 47, 47, 47, 48, 48, 48, 48, 48,
    /* F+70 */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    /* Fast */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49
};


/*
 * Say whether it's daytime or not
 */
bool is_daytime(void)
{
    return ((turn.turn % (10L * z_info->day_length)) < (u32b)((10L * z_info->day_length) / 2));
}


/*
 * The amount of energy gained in a turn by a player or monster
 */
int turn_energy(int speed)
{
    return extract_energy[MIN(speed, N_ELEMENTS(extract_energy) - 1)] * z_info->move_energy / 100;
}


/*
 * The amount of energy gained in a frame by a player or monster
 *
 * PWMAngband: the energy is multiplied by 100 to ensure that slow players will always gain at
 * least 1 energy (energy = energy * bubble speed factor / 100) when locked in a slow time bubble.
 */
int frame_energy(int speed)
{
    return extract_energy[MIN(speed, N_ELEMENTS(extract_energy) - 1)] * 100;
}


/*
 * Let the player know when an object is recharged.
 * Also inform player when first item of a stack has recharged.
 */
static void recharged_notice(struct player *p, const struct object *obj, bool all)
{
    char o_name[NORMAL_WID];

    if (!OPT(p, notify_recharge)) return;

    /* Describe (briefly) */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Disturb the player */
    disturb(p, 0);

    /* Notify the player */
    if (obj->number > 1)
    {
        if (all) msg(p, "Your %s have recharged.", o_name);
        else msg(p, "One of your %s has recharged.", o_name);
    }

    /* Artifacts */
    else if (obj->artifact)
        msg(p, "The %s has recharged.", o_name);

    /* Single, non-artifact items */
    else msg(p, "Your %s has recharged.", o_name);
}


/*
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory. Decompose carried corpses slowly.
 */
static void recharge_objects(struct player *p)
{
    bool discharged_stack;
    struct object *obj = p->gear, *next;

    /* Recharge carried gear */
    while (obj)
    {
        next = obj->next;

        /* Recharge equipment */
        if (object_is_equipped(p->body, obj))
        {
            /* Recharge activatable objects */
            if (recharge_timeout(obj))
            {
                /* Message if an item recharged */
                recharged_notice(p, obj, true);

                /* Redraw */
                p->upkeep->redraw |= (PR_EQUIP);
            }
        }

        /* Recharge the inventory */
        /* Check if we are shopping (fixes stacking exploits) */
        else if (!in_store(p))
        {
            discharged_stack = (number_charging(obj) == obj->number)? true: false;

            /* Recharge rods, and update if any rods are recharged */
            if (tval_can_have_timeout(obj) && recharge_timeout(obj))
            {
                /* Entire stack is recharged */
                if (!obj->timeout) recharged_notice(p, obj, true);

                /* Previously exhausted stack has acquired a charge */
                else if (discharged_stack) recharged_notice(p, obj, false);

                /* Combine pack */
                p->upkeep->notice |= (PN_COMBINE);

                /* Redraw */
                p->upkeep->redraw |= (PR_INVEN);
            }

            /* Handle corpse decay */
            if (tval_is_corpse(obj))
            {
                char o_name[NORMAL_WID];
                object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

                /* Corpses slowly decompose */
                obj->decay--;

                /* Notice changes */
                if (obj->decay == obj->timeout / 5)
                {
                    /* Rotten corpse */
                    if (obj->number > 1)
                        msg(p, "Your %s slowly rot away.", o_name);
                    else
                        msg(p, "Your %s slowly rots away.", o_name);

                    /* Combine pack */
                    p->upkeep->notice |= (PN_COMBINE);

                    /* Redraw */
                    p->upkeep->redraw |= (PR_INVEN);
                }
                else if (!obj->decay)
                {
                    /* No more corpse... */
                    if (obj->number > 1)
                        msg(p, "Your %s rot away.", o_name);
                    else
                        msg(p, "Your %s rots away.", o_name);
                    gear_excise_object(p, obj);
                    object_delete(&obj);

                    /* Combine pack */
                    p->upkeep->notice |= (PN_COMBINE);

                    /* Redraw */
                    p->upkeep->redraw |= (PR_INVEN);
                }
            }
        }

        obj = next;
    }
}


/*
 * Play an ambient sound dependent on dungeon level, and day or night in towns
 */
static void play_ambient_sound(struct player *p)
{
    if (p->wpos.depth == 0)
    {
        if (in_town(&p->wpos))
        {
            if (is_daytime())
                sound(p, MSG_AMBIENT_DAY);
            else
                sound(p, MSG_AMBIENT_NITE);
        }
        else
            sound(p, wf_info[get_wt_info_at(p->wpos.wy, p->wpos.wx)->type].sound_idx);
    }
    else if (p->wpos.depth <= 20)
        sound(p, MSG_AMBIENT_DNG1);
    else if (p->wpos.depth <= 40)
        sound(p, MSG_AMBIENT_DNG2);
    else if (p->wpos.depth <= 60)
        sound(p, MSG_AMBIENT_DNG3);
    else if (p->wpos.depth <= 80)
        sound(p, MSG_AMBIENT_DNG4);
    else if (p->wpos.depth <= 98)
        sound(p, MSG_AMBIENT_DNG5);
    else if (p->wpos.depth == 99)
        sound(p, MSG_AMBIENT_SAURON);
    else if (p->wpos.depth == 100)
        sound(p, MSG_AMBIENT_MORGOTH);
    else if (p->wpos.depth <= 124)
        sound(p, MSG_AMBIENT_DNG6);
    else if (p->wpos.depth == 125)
        sound(p, MSG_AMBIENT_SENYA);
    else if (p->wpos.depth == 126)
        sound(p, MSG_AMBIENT_XAKAZE);
    else
        sound(p, MSG_AMBIENT_MELKOR);
}


/*
 * Helper for process_player -- decrement p->timed[] and curse effect fields.
 */
static void decrease_timeouts(struct player *p, struct chunk *c)
{
    int adjust = (adj_con_fix[p->state.stat_ind[STAT_CON]] + 1);
    int i;

    /* Most timed effects decrement by 1 */
    for (i = 0; i < TMD_MAX; i++)
    {
        int decr = 1;
        if (!p->timed[i]) continue;

        /* Special cases */
        switch (i)
        {
            case TMD_CUT:
            {
                /* Hack -- check for truly "mortal" wound */
                decr = ((p->timed[i] > TMD_CUT_DEEP)? 0: adjust);

                /* Biofeedback always helps */
                if (p->timed[TMD_BIOFEEDBACK]) decr += decr + 10;
                break;
            }

            case TMD_POISONED:
            case TMD_STUN:
            {
                decr = adjust;
                break;
            }

            case TMD_WRAITHFORM:
            {
                /* Hack -- must be in bounds */
                if (!chunk_has_players(&c->wpos) || !square_in_bounds_fully(c, p->py, p->px))
                    decr = 0;
                break;
            }
        }

        /* Hack -- make -1 permanent */
        if (p->timed[i] == -1) decr = 0;

        /* Decrement the effect */
        if (decr > 0) player_dec_timed(p, i, decr, false);
    }

    /* Curse effects always decrement by 1 */
    for (i = 0; i < p->body.count; i++)
    {
        struct curse_data *curse = NULL;
        int j;

        if (p->body.slots[i].obj == NULL) continue;
        curse = p->body.slots[i].obj->curses;
        for (j = 0; curse && (j < z_info->curse_max); j++)
        {
            if (curse[j].power == 0) continue;
            if (curse[j].timeout == 0) continue;
            curse[j].timeout--;
            if (!curse[j].timeout)
            {
                do_curse_effect(p, j);
                curse[j].timeout = randcalc(curses[j].obj->time, 0, RANDOMISE);
            }
        }
    }
}


/*
 * Handle things that need updating (mostly once every 10 game turns)
 */
static void process_world(struct player *p, struct chunk *c)
{
    int y, x;

    if (!p)
    {
        /* Compact the monster list if we're approaching the limit */
        if (cave_monster_count(c) + 32 > z_info->level_monster_max)
            compact_monsters(c, 64);

        /* Too many holes in the monster list - compress */
        if (cave_monster_count(c) + 32 < cave_monster_max(c))
            compact_monsters(c, 0);

        /* Decrease trap timeouts */
        for (y = 0; y < c->height; y++)
        {
            for (x = 0; x < c->width; x++)
            {
                struct trap *trap = c->squares[y][x].trap;

                while (trap)
                {
                    if (trap->timeout)
                    {
                        trap->timeout--;
                        if (!trap->timeout) square_light_spot(c, y, x);
                    }
                    trap = trap->next;
                }
            }
        }

        return;
    }

    /*** Check the Time ***/

    /* Play an ambient sound at regular intervals. */
    if (!(turn.turn % ((10L * z_info->day_length) / 4))) play_ambient_sound(p);

    /* Daybreak/Nighfall in towns or wilderness */
    if ((p->wpos.depth == 0) && !(turn.turn % ((10L * z_info->day_length) / 2)))
    {
        bool dawn;

        /* Check for dawn */
        dawn = (!(turn.turn % (10L * z_info->day_length)));

        /* Day breaks */
        if (dawn) msg(p, "The sun has risen.");

        /* Night falls */
        else msg(p, "The sun has fallen.");

        /* Clear the flags for each cave grid */
        player_cave_clear(p, false);

        /* Illuminate */
        cave_illuminate(p, c, dawn);

        /* Hack -- regenerate crops */
        for (y = 0; y < c->height; y++)
        {
            for (x = 0; x < c->width; x++)
            {
                /* Regenerate crops */
                if (square_iscrop(c, y, x) && !square_object(c, y, x) && !c->squares[y][x].mon &&
                    dawn && one_in_(16))
                {
                    /* Add crop to that location */
                    wild_add_crop(c, x, y, randint0(7));
                }
            }
        }
    }

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(&p->wpos)) return;

    /* Every ten turns */
    if (turn.turn % 10) return;

    /*
     * Note : since monsters are added at a constant rate in real time,
     * this corresponds in game time to them appearing at faster rates
     * deeper in the dungeon.
     */

    /* Check for creature generation */
    /* Hack -- increase respawn rate on no_recall servers */
    if (one_in_((cfg_diving_mode == 3)? z_info->alloc_monster_chance / 4:
        z_info->alloc_monster_chance))
    {
        if (in_wild(&p->wpos))
        {
            /* Respawn residents in the wilderness outside of town areas */
            if (!town_area(&p->wpos)) wild_add_monster(p, c);
        }
        else
            pick_and_place_distant_monster(p, c, z_info->max_sight + 5, MON_ASLEEP);
    }
}


/*
 * Check for nearby players/monsters and attack the current target.
 */
static void auto_retaliate(struct player *p, struct chunk *c)
{
    int i, n = 0, target_x[8], target_y[8];
    int tx, ty;
    bool found = false;
    struct source *health_who = &p->upkeep->health_who;
    struct source who_body;
    struct source *who = &who_body;

    /* Hack -- shoppers don't auto-retaliate */
    if (in_store(p)) return;

    /* The dungeon master does not auto-retaliate */
    if (p->dm_flags & DM_MONSTER_FRIEND) return;

    /* Not while confused or afraid */
    if (p->timed[TMD_CONFUSED] || player_of_has(p, OF_AFRAID)) return;

    /* Check preventive inscription '^O' */
    if (check_prevent_inscription(p, INSCRIPTION_RETALIATE)) return;

    /* Try to find valid targets around us */
    for (i = 0; i < 8; i++)
    {
        bool hostile, visible, mimicking;

        /* Current location */
        tx = p->px + ddx_ddd[i];
        ty = p->py + ddy_ddd[i];

        /* Paranoia */
        if (!square_in_bounds_fully(c, ty, tx)) continue;

        /* Nobody here */
        if (!c->squares[ty][tx].mon) continue;

        square_actor(c, ty, tx, who);

        /* Target info */
        if (who->player)
        {
            hostile = pvp_check(p, who->player, PVP_CHECK_BOTH, true, c->squares[ty][tx].feat);
            visible = player_is_visible(p, who->idx);
            mimicking = (who->player->k_idx != 0);
        }
        else
        {
            hostile = pvm_check(p, who->monster);
            visible = monster_is_visible(p, who->idx);
            mimicking = monster_is_camouflaged(who->monster);
        }

        /* If hostile and visible, it's a fair target (except hidden mimics) */
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
    if (!source_null(health_who))
    {
        for (i = 0; i < n; i++)
        {
            /* Current location */
            tx = target_x[i];
            ty = target_y[i];

            /* Not the current target */
            square_actor(c, ty, tx, who);
            if (!source_equal(health_who, who)) continue;

            /* Current target found */
            found = true;
            break;
        }
    }

    /* If there's at least one valid target around, attack one (active auto-retaliator only) */
    if (OPT(p, active_auto_retaliator) && !found)
    {
        /* Choose randomly */
        i = randint0(n);
        tx = target_x[i];
        ty = target_y[i];
        square_actor(c, ty, tx, who);
        found = true;
    }

    /* No current target */
    if (!found) return;

    /* Attack the current target */
    py_attack(p, c, ty, tx);

    /* Take a turn */
    use_energy(p);
}


/*
 * Digest some food
 */
static void digest_food(struct player *p)
{
    /* Ghosts don't need food */
    if (p->ghost) return;

    /* Don't use food in towns */
    if (forbid_town(&p->wpos)) return;

    /* Don't use food near towns (to avoid starving in one's own house) */
    if (town_area(&p->wpos)) return;

    /* Digest some food */
    player_set_food(p, p->food - player_digest(p));
}


/*
 * Every turn, the character makes enough noise that nearby monsters can use
 * it to home in.
 *
 * This function actually just computes distance from the player; this is
 * used in combination with the player's stealth value to determine what
 * monsters can hear. We mark the player's grid with 0, then fill in the noise
 * field of every grid that the player can reach with that "noise"
 * (actually distance) plus the number of steps needed to reach that grid,
 * so higher values mean further from the player.
 *
 * Monsters use this information by moving to adjacent grids with lower noise
 * values, thereby homing in on the player even though twisty tunnels and
 * mazes. Monsters have a hearing value, which is the largest sound value
 * they can detect.
 */
static void make_noise(struct player *p)
{
    int next_y = p->py;
    int next_x = p->px;
    int y, x, d;
    int noise = 0;
    struct queue *queue = q_new(p->cave->height * p->cave->width);
    struct chunk *c = chunk_get(&p->wpos);

    /* Set all the grids to silence */
    for (y = 1; y < p->cave->height - 1; y++)
        for (x = 1; x < p->cave->width - 1; x++)
            p->cave->noise.grids[y][x] = 0;

    /* Player makes noise */
    p->cave->noise.grids[next_y][next_x] = noise;
    q_push_int(queue, yx_to_i(next_y, next_x, p->cave->width));
    noise++;

    /* Propagate noise */
    while (q_len(queue) > 0)
    {
        /* Get the next grid */
        i_to_yx(q_pop_int(queue), p->cave->width, &next_y, &next_x);

        /* If we've reached the current noise level, put it back and step */
        if (p->cave->noise.grids[next_y][next_x] == noise)
        {
            q_push_int(queue, yx_to_i(next_y, next_x, p->cave->width));
            noise++;
            continue;
        }

        /* Assign noise to the children and enqueue them */
        for (d = 0; d < 8; d++)
        {
            /* Child location */
            y = next_y + ddy_ddd[d];
            x = next_x + ddx_ddd[d];
            if (!player_square_in_bounds(p, y, x)) continue;

            /* Ignore features that don't transmit sound */
            if (square_isnoflow(c, y, x)) continue;

            /* Skip grids that already have noise */
            if (p->cave->noise.grids[y][x] != 0) continue;

            /* Skip the player grid */
            if ((y == p->py) && (x == p->px)) continue;

            /* Save the noise */
            p->cave->noise.grids[y][x] = noise;

            /* Enqueue that entry */
            q_push_int(queue, yx_to_i(y, x, p->cave->width));
        }
    }

    q_free(queue);
}


/*
 * Characters leave scent trails for perceptive monsters to track.
 *
 * Scent is rather more limited than sound. Many creatures cannot use
 * it at all, it doesn't extend very far outwards from the character's
 * current position, and monsters can use it to home in the character,
 * but not to run away.
 *
 * Scent is valued according to age. When a character takes his turn,
 * scent is aged by one, and new scent is laid down. Monsters have a smell
 * value which indicates the oldest scent they can detect. Grids where the
 * player has never been will have scent 0. The player's grid will also have
 * scent 0, but this is OK as no monster will ever be smelling it.
 */
static void update_scent(struct player *p)
{
    int y, x;
    int scent_strength[5][5] =
    {
        {2, 2, 2, 2, 2},
        {2, 1, 1, 1, 2},
        {2, 1, 0, 1, 2},
        {2, 1, 1, 1, 2},
        {2, 2, 2, 2, 2},
    };
    struct chunk *c = chunk_get(&p->wpos);

    /* Update scent for all grids */
    for (y = 1; y < p->cave->height - 1; y++)
    {
        for (x = 1; x < p->cave->width - 1; x++)
        {
            if (p->cave->scent.grids[y][x] > 0)
                p->cave->scent.grids[y][x]++;
        }
    }

    /* Lay down new scent around the player */
    for (y = 0; y < 5; y++)
    {
        for (x = 0; x < 5; x++)
        {
            int scent_y = y + p->py - 2;
            int scent_x = x + p->px - 2;
            int new_scent = scent_strength[y][x];
            int d;
            bool add_scent = false;

            /* Ignore invalid or non-scent-carrying grids */
            if (!player_square_in_bounds(p, scent_y, scent_x)) continue;
            if (square_isnoscent(c, scent_y, scent_x)) continue;

            /* Check scent is spreading on floors, not going through walls */
            for (d = 0; d < 8; d++)
            {
                int adj_y = scent_y + ddy_ddd[d];
                int adj_x = scent_x + ddx_ddd[d];

                if (!player_square_in_bounds(p, adj_y, adj_x)) continue;

                /* Player grid is always valid */
                if ((x == 2) && (y == 2)) add_scent = true;

                /* Adjacent to a closer grid, so valid */
                if (p->cave->scent.grids[adj_y][adj_x] == new_scent - 1) add_scent = true;
            }

            /* Not valid */
            if (!add_scent) continue;

            /* Mark the scent */
            p->cave->scent.grids[scent_y][scent_x] = new_scent;
        }
    }
}


/*
 * Handle things that need updating once every 10 "scaled" game turns
 */
static void process_player_world(struct player *p, struct chunk *c)
{
    int i, time;

    /* Hack -- fade monster detect over time */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        if (p->mon_det[i])
        {
            p->mon_det[i]--;
            if (!p->mon_det[i]) update_mon(cave_monster(c, i), c, false);
        }
    }

    /* Hack -- fade player detect over time */
    for (i = 1; i <= NumPlayers; i++)
    {
        if (p->play_det[i])
        {
            p->play_det[i]--;
            if (!p->play_det[i]) update_player(player_get(i));
        }
    }

    /* Hack -- semi-constant hallucination (but not in stores) */
    if (p->timed[TMD_IMAGE] && !in_store(p)) p->upkeep->redraw |= (PR_MAP);

    /*** Damage over Time ***/

    /* Take damage from permanent wraithform while inside walls */
    if ((p->timed[TMD_WRAITHFORM] == -1) && !square_ispassable(c, p->py, p->px))
        take_hit(p, 1, "hypoxia", false, "was entombed into solid terrain");

    /* Take damage from Undead Form */
    if (player_undead(p))
        take_hit(p, 1, "fading", false, "faded away");

    /* Take damage from poison */
    if (p->timed[TMD_POISONED])
        take_hit(p, 1, "poison", false, "died of blood poisoning");

    /* Take damage from cuts */
    if (p->timed[TMD_CUT])
    {
        /* Mortal wound or Deep Gash */
        if (p->timed[TMD_CUT] > TMD_CUT_SEVERE) i = 3;

        /* Severe cut */
        else if (p->timed[TMD_CUT] > TMD_CUT_NASTY) i = 2;

        /* Other cuts */
        else i = 1;

        /* Take damage */
        take_hit(p, i, "a fatal wound", false, "bled to death");
    }

    /* Player can be damaged by terrain */
    player_take_terrain_damage(p, c, p->py, p->px);

    /* Effects of Black Breath */
    if (p->timed[TMD_BLACKBREATH])
    {
        if (one_in_(2))
        {
            msg(p, "The Black Breath sickens you.");
            player_stat_dec(p, STAT_CON, false);
        }
        if (one_in_(2))
        {
            msg(p, "The Black Breath saps your strength.");
            player_stat_dec(p, STAT_STR, false);
        }
        if (one_in_(2))
        {
            int drain = 100 + (p->exp / 100) * z_info->life_drain_percent;

            msg(p, "The Black Breath dims your life force.");
            player_exp_lose(p, drain, false);
        }
    }

    /*** Check the Food, and Regenerate ***/

    /* Every 100 "scaled" turns */
    time = move_energy(p->wpos.depth) / time_factor(p, c);

    /* Digest normally */
    if (!(turn.turn % time)) digest_food(p);

    /* Getting Faint */
    if (p->food < PY_FOOD_FAINT)
    {
        /* Faint occasionally */
        if (!p->timed[TMD_PARALYZED] && magik(10))
        {
            /* Message */
            msg(p, "You faint from the lack of food.");
            disturb(p, 1);

            /* Hack -- faint (bypass free action) */
            player_inc_timed(p, TMD_PARALYZED, 1 + randint0(5), true, false);
        }
    }

    /* Starve to death (slowly) */
    if (p->food < PY_FOOD_STARVE)
    {
        /* Calculate damage */
        i = (PY_FOOD_STARVE - p->food) / 10;

        /* Take damage */
        take_hit(p, i, "starvation", false, "starved to death");
    }

    /* Regenerate Hit Points if needed */
    if (p->chp < p->mhp) player_regen_hp(p, c);

    /* Regenerate mana if needed */
    if (p->csp < p->msp) player_regen_mana(p);

    /* Check for interrupts */
    player_resting_complete_special(p);

    /* Dwarves detect treasure */
    if (player_has(p, PF_SEE_ORE))
    {
        /* Only if they are in good shape */
        if (!p->timed[TMD_IMAGE] && !p->timed[TMD_CONFUSED] &&
            !p->timed[TMD_AMNESIA] && !p->timed[TMD_STUN] &&
            !p->timed[TMD_PARALYZED] && !p->timed[TMD_TERROR] &&
            !p->timed[TMD_AFRAID])
        {
            struct source who_body;
            struct source *who = &who_body;

            source_player(who, get_player_index(get_connection(p->conn)), p);
            effect_simple(EF_DETECT_GOLD, who, "3d3", 0, 0, 0, NULL);
        }
    }

    /* Timeout various things */
    decrease_timeouts(p, c);

    /* Quest */
    process_quest(p);

    /* Process light */
    player_update_light(p);

    /* Update noise and scent */
    make_noise(p);
    update_scent(p);

    /*** Process Inventory ***/

    /* Handle experience draining */
    if (player_of_has(p, OF_DRAIN_EXP))
    {
        if (magik(10) && (p->exp > 0))
        {
            s32b d = damroll(10, 6) + (p->exp / 100) * z_info->life_drain_percent;

            player_exp_lose(p, d / 10, false);
        }

        equip_learn_flag(p, OF_DRAIN_EXP);
    }

    /* Recharge activatable objects and rods */
    recharge_objects(p);

    /* Notice things after time */
    if (!(turn.turn % 100))
        equip_learn_after_time(p);

    /*** Involuntary Movement ***/

    /* Delayed Word-of-Recall */
    if (p->word_recall)
    {
        /* Count down towards recall */
        p->word_recall--;

        /* Activate the recall */
        if (!p->word_recall)
        {
            /* Hack -- no recall if in a shop, or under the influence of space/time anchor */
            if (in_store(p) || check_st_anchor(&p->wpos, p->py, p->px))
                p->word_recall++;

            /* Hack -- no recall if waiting for confirmation */
            else if (p->current_value == ITEM_PENDING)
                p->word_recall++;

            /* Activate the recall */
            else
                recall_player(p, c);
        }
    }

    /* Delayed Deep Descent */
    if (p->deep_descent)
    {
        /* Count down towards descent */
        p->deep_descent--;

        /* Activate the descent */
        if (!p->deep_descent)
        {
            /* Hack -- not if in a shop, or under the influence of space/time anchor */
            if (in_store(p) || check_st_anchor(&p->wpos, p->py, p->px))
                p->deep_descent++;
            else
            {
                struct source who_body;
                struct source *who = &who_body;

                source_player(who, get_player_index(get_connection(p->conn)), p);
                if (effect_simple(EF_DEEP_DESCENT, who, "0", 0, 1, 0, NULL)) return;

                /* Failure */
                p->deep_descent++;
            }
        }
    }
}


/*
 * Housekeeping after the processing of a player command
 */
static void process_player_cleanup(struct player *p)
{
    int timefactor, time;
    struct chunk *c = chunk_get(&p->wpos);
    int i;
    bool debug_mode = false;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        if (q->upkeep->new_level_method)
        {
            debug_mode = true;
            break;
        }
    }

    if (debug_mode) plog_fmt("BEGIN process_player_cleanup(%s)", p->name);

    /* If we are in a slow time condition, give visual warning */
    timefactor = time_factor(p, c);
    if (timefactor < NORMAL_TIME)
        square_light_spot_aux(p, c, p->py, p->px);

    /* Check for auto-retaliate */
    if (has_energy(p)) auto_retaliate(p, c);

    /* Notice stuff */
    notice_stuff(p);

    /* Hack -- pack overflow */
    pack_overflow(p, c, NULL);

    /* Determine basic frequency of regen in game turns, then scale by players local time bubble */
    time = move_energy(p->wpos.depth) / (10 * timefactor);

    if (debug_mode) plog("Process the world of that player every ten scaled turns");

    /* Process the world of that player every ten "scaled" turns */
    if (!(turn.turn % time)) process_player_world(p, c);

    if (debug_mode) plog("Flicker multi-hued players, party leaders and elementalists");

    /* Only when needed, every five game turns */
    if (!(turn.turn % 5))
    {
        /* Flicker self if multi-hued */
        if (p->poly_race && monster_shimmer(p->poly_race) && allow_shimmer(p))
            square_light_spot_aux(p, c, p->py, p->px);

        /* Flicker multi-hued players, party leaders and elementalists */
        if (p->shimmer)
        {
            int j;

            /* Check everyone */
            for (j = 1; j <= NumPlayers; j++)
            {
                struct player *q = player_get(j);

                /* Ignore the player that we're updating */
                if (q == p) continue;

                /* If he's not here, skip him */
                if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

                /* If he's not here YET, also skip him */
                if (q->upkeep->new_level_method) continue;

                /* Flicker multi-hued players */
                if (p->poly_race && monster_shimmer(p->poly_race) && allow_shimmer(q))
                    square_light_spot_aux(q, c, p->py, p->px);

                /* Flicker party leaders */
                if (is_party_owner(q, p) && OPT(q, highlight_leader))
                    square_light_spot_aux(q, c, p->py, p->px);

                /* Flicker elementalists */
                if (player_has(p, PF_ELEMENTAL_SPELLS) && allow_shimmer(q))
                    square_light_spot_aux(q, c, p->py, p->px);
            }
        }
    }

    if (debug_mode) plog("Check monster recall");

    /* Check monster recall */
    if (p->upkeep->monster_race.race)
    {
        struct monster_lore lore;
        byte *blows, *current_blows;
        bool *blow_known, *current_blow_known;

        /* Get the lores (player + global) */
        get_global_lore(p, p->upkeep->monster_race.race, &lore);

        /* Check for change in blows */
        if (0 != memcmp(p->current_lore.blows, lore.blows, z_info->mon_blows_max * sizeof(byte)))
        {
            memmove(p->current_lore.blows, lore.blows, z_info->mon_blows_max * sizeof(byte));
            p->upkeep->redraw |= (PR_MONSTER);
        }

        /* Check for change in known blows */
        if (0 != memcmp(p->current_lore.blow_known, lore.blow_known,
            z_info->mon_blows_max * sizeof(bool)))
        {
            memmove(p->current_lore.blow_known, lore.blow_known,
                z_info->mon_blows_max * sizeof(bool));
            p->upkeep->redraw |= (PR_MONSTER);
        }

        /* Hack -- save blow states, nullify */
        blows = lore.blows;
        current_blows = p->current_lore.blows;
        blow_known = lore.blow_known;
        current_blow_known = p->current_lore.blow_known;
        lore.blows = NULL;
        p->current_lore.blows = NULL;
        lore.blow_known = NULL;
        p->current_lore.blow_known = NULL;

        /* Check for change of any kind (except blow states) */
        if (0 != memcmp(&p->current_lore, &lore, sizeof(struct monster_lore)))
        {
            memmove(&p->current_lore, &lore, sizeof(struct monster_lore));
            p->upkeep->redraw |= (PR_MONSTER);
        }

        /* Hack -- restore blow states */
        lore.blows = blows;
        p->current_lore.blows = current_blows;
        lore.blow_known = blow_known;
        p->current_lore.blow_known = current_blow_known;

        mem_free(lore.blows);
        mem_free(lore.blow_known);
    }

    /* Refresh stuff */
    refresh_stuff(p);

    if (debug_mode) plog_fmt("END process_player_cleanup(%s)", p->name);
}


/*
 * Process player commands from the command queue, finishing when there is a
 * command using energy (any regular game command), or we run out of commands
 * and need another from the user, or the character changes level or dies, or
 * the game is stopped.
 */
static void process_player(struct player *p)
{
    /* Try to execute any commands on the command queue. */
    /* NB: process_pending_commands may have deleted the connection! */
    if (process_pending_commands(p->conn)) return;

    if (!p->upkeep->new_level_method && !p->upkeep->funeral)
        process_player_cleanup(p);
}


/*
 * Housekeeping on arriving on a new level
 */
static void on_new_level(void)
{
    int i;

    /* Hack -- reset current sound */
    for (i = 1; i <= NumPlayers; i++) player_get(i)->current_sound = -1;

    /*
     * The number of game turns per player turn can be calculated as the energy
     * required to act at the current depth (energy per player turn) divided by
     * the energy given per game turn given the current player speed.
     */

    /* Hack -- reset projection indicator every player turn */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /*
         * For projection indicator, we will consider the shortest number of game turns
         * possible -- the one obtained at max speed.
         */
        int player_turn = move_energy(p->wpos.depth) / frame_energy(199);

        /* Reset projection indicator */
        if (!(turn.turn % player_turn)) p->did_visuals = false;
    }
}


/*
 * Housekeeping on leaving a level
 */
static void on_leave_level(void)
{
    int i, x, y;

    /* Deallocate any unused levels */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            /* Caves */
            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                if (!w_ptr->chunk_list[i]) continue;

                /* Don't deallocate special levels */
                if (level_keep_allocated(w_ptr->chunk_list[i])) continue;

                /* Hack -- deallocate custom houses */
                wipe_custom_houses(&w_ptr->chunk_list[i]->wpos);

                /* Deallocate the level */
                cave_wipe(w_ptr->chunk_list[i]);
                w_ptr->chunk_list[i] = NULL;
            }
        }
    }
}


/*
 * Handles "global" things on the server
 */
static void process_various(void)
{
    /* Purge the player database occasionally */
    if (!(turn.turn % (cfg_fps * 60 * 60 * SERVER_PURGE)))
        purge_player_names();

    /* Save the server state occasionally */
    if (!(turn.turn % (cfg_fps * 60 * SERVER_SAVE)))
    {
        int i;

        /* Save server state */
        save_server_info();

        /* Save each player */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *p = player_get(i);

            /* Save this player */
            if (!p->upkeep->funeral) save_player(p);
        }
    }

    /* Handle certain things once a minute */
    if (!(turn.turn % (cfg_fps * 60)))
    {
        int i;

        /* Update the player retirement timers */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *p = player_get(i);

            /* If our retirement timer is set */
            if (!p->upkeep->funeral && (p->retire_timer > 0))
            {
                /* Decrement our retire timer */
                p->retire_timer--;

                /* If the timer runs out, forcibly retire this character */
                if (!p->retire_timer) do_cmd_suicide(p);
            }
        }

        /* If the level unstaticer is not disabled */
        if (cfg_level_unstatic_chance >= 0)
        {
            int depth, x, y;

            /* For each dungeon level */
            for (y = radius_wild; y >= 0 - radius_wild; y--)
            {
                for (x = 0 - radius_wild; x <= radius_wild; x++)
                {
                    struct wild_type *w_ptr = get_wt_info_at(y, x);

                    for (depth = w_ptr->min_depth; depth < w_ptr->max_depth; depth++)
                    {
                        int num_on_depth = 0;
                        struct worldpos wpos;

                        /* Random chance of the level unstaticing */
                        /* The chance is one in (base_chance * depth) / 250 feet */
                        int chance = (cfg_level_unstatic_chance * (depth + 5) / 5) - 1;

                        /* Full unstaticer */
                        if (!cfg_level_unstatic_chance) chance = 0;

                        COORDS_SET(&wpos, y, x, depth);

                        /* If this depth is static */
                        if (!chunk_has_players(&wpos)) continue;

                        /* Count the number of players actually in game on this level */
                        for (i = 1; i <= NumPlayers; i++)
                        {
                            struct player *p = player_get(i);

                            if (!p->upkeep->funeral && COORDS_EQUAL(&p->wpos, &wpos))
                                num_on_depth++;
                        }

                        /* Unstatic the level if this level is static and no one is actually on it */
                        if (!num_on_depth && one_in_(chance))
                            chunk_set_player_count(&wpos, 0);
                    }
                }
            }
        }
    }

    /* Grow trees very occasionally */
    if (!(turn.turn % (10L * GROW_TREE)) &&
        ((trees_in_town < cfg_max_trees) || (cfg_max_trees == -1)))
    {
        int i;
        struct chunk *c = chunk_get(start_wpos());

        /* Find a suitable location */
        for (i = 1; c && (i < 1000); i++)
        {
            /* Pick a location */
            int y = rand_range(1, c->height - 2);
            int x = rand_range(1, c->width - 2);

            /* Only allow "dirt" */
            if (!square_isdirt(c, y, x)) continue;

            /* Never grow on top of objects or monsters/players */
            if (c->squares[y][x].mon) continue;
            if (c->squares[y][x].obj) continue;

            /* Grow a tree here */
            square_add_tree(c, y, x);
            trees_in_town++;

            /* Done */
            break;
        }
    }

    /* Update the stores */
    store_update();

    /* Hack -- prevent wilderness monster "buildup" */
    if (!(turn.turn % ((10L * z_info->day_length) / 2)) && !(turn.turn % (10L * z_info->day_length)))
    {
        int y, x;

        for (y = radius_wild; y >= 0 - radius_wild; y--)
        {
            for (x = 0 - radius_wild; x <= radius_wild; x++)
            {
                struct wild_type *w_ptr = get_wt_info_at(y, x);
                struct chunk *c = w_ptr->chunk_list[0];
                int m_idx;

                if (in_town(&w_ptr->wpos)) continue;

                /* Must exist and not contain players */
                if (!c || chunk_has_players(&c->wpos)) continue;

                /* Mimic stuff */
                for (m_idx = cave_monster_max(c) - 1; m_idx >= 1; m_idx--)
                {
                    struct monster *mon = cave_monster(c, m_idx);
                    struct object *obj = mon->mimicked_obj;

                    /* Delete mimicked objects */
                    if (obj)
                    {
                        square_excise_object(c, mon->fy, mon->fx, obj);
                        object_delete(&obj);
                    }

                    /* Delete mimicked features */
                    if (mon->race->base == lookup_monster_base("feature mimic"))
                        square_set_feat(c, mon->fy, mon->fx, mon->feat);
                }

                /* Wipe the monster list */
                wipe_mon_list(c);
            }
        }
    }
}


static void process_death(void)
{
    int i;

    /* Check for death */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Check for death (postpone death if no level) */
        if (p->is_dead && !p->upkeep->new_level_method && !p->upkeep->funeral)
        {
            /* Kill him */
            player_death(p);
        }
    }
}


static void remove_hounds(struct player *p, struct chunk *c)
{
    int j, d;

    /* Remove nearby hounds */
    for (j = 1; j < cave_monster_max(c); j++)
    {
        struct monster *mon = cave_monster(c, j);

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Hack -- skip unique monsters */
        if (monster_is_unique(mon->race)) continue;

        /* Skip monsters other than hounds */
        if (mon->race->base != lookup_monster_base("zephyr hound")) continue;

        /* Skip distant monsters */
        d = distance(p->py, p->px, mon->fy, mon->fx);
        if (d > z_info->max_sight) continue;

        /* Delete the monster */
        delete_monster_idx(c, j);
    }
}


static void place_player(struct player *p, struct chunk *c, int starty, int startx)
{
    int y, x, d, j;

    /* Try to find an empty space */
    for (j = 0; j < 1500; ++j)
    {
        /* Increasing distance */
        d = (j + 149) / 150;

        /* Pick a location (skip LOS test) */
        if (!scatter(c, &y, &x, starty, startx, d, false)) continue;

        /* Must have an "empty" grid */
        if (!square_isemptyfloor(c, y, x)) continue;

        /* Not allowed to go onto a icky location (house) */
        if ((p->wpos.depth == 0) && square_isvault(c, y, x)) continue;

        /* Place the player */
        p->old_py = p->py = y;
        p->old_px = p->px = x;

        return;
    }

    /* Try to find an occupied space */
    for (j = 0; j < 1500; ++j)
    {
        /* Increasing distance */
        d = (j + 149) / 150;

        /* Pick a location (skip LOS test) */
        if (!scatter(c, &y, &x, starty, startx, d, false)) continue;

        /* Must have a "floor" grid (forbid players only) */
        if (!square_ispassable(c, y, x) || (c->squares[y][x].mon < 0))
            continue;

        /* Not allowed to go onto a icky location (house) */
        if ((p->wpos.depth == 0) && square_isvault(c, y, x)) continue;

        /* Remove any monster at that location */
        delete_monster(c, y, x);

        /* Place the player */
        p->old_py = p->py = y;
        p->old_px = p->px = x;

        return;
    }

    /* Paranoia: place the player in bounds */
    p->old_py = p->py = starty;
    p->old_px = p->px = startx;
}


static void generate_new_level(struct player *p)
{
    int startx, starty, id;
    bool new_level = false;
    struct chunk *c;

    id = get_player_index(get_connection(p->conn));
    c = chunk_get(&p->wpos);

    /* Paranoia */
    if (!chunk_has_players(&p->wpos)) return;

    /* Play ambient sound on change of level. */
    play_ambient_sound(p);

    /* Check "maximum depth" to make sure it's still correct */
    if (p->wpos.depth > p->max_depth) p->max_depth = p->wpos.depth;

    /* Make sure the server doesn't think the player is in a store */
    p->store_num = -1;

    /* Somebody has entered an ungenerated level */
    if (!c)
    {
        new_level = true;

        /* Generate a dungeon level there */
        c = prepare_next_level(p, &p->wpos);

        wild_deserted_message(p);
    }

    /* Apply illumination */
    else
    {
        /* Clear the flags for each cave grid (cave dimensions may have changed) */
        player_cave_new(p, c->height, c->width);
        player_cave_clear(p, true);

        /* Illuminate */
        cave_illuminate(p, c, is_daytime());

        /* Ensure fixed encounters on special levels (wilderness) */
        if (special_level(&c->wpos) && (cfg_diving_mode < 2))
        {
            int i, y, x;

            for (i = 1; i < z_info->r_max; i++)
            {
                struct monster_race *race = &r_info[i];

                /* The monster must be an unseen fixed encounter of this depth. */
                if (race->lore.spawned) continue;
                if (!rf_has(race->flags, RF_PWMANG_FIXED)) continue;
                if (race->level != c->wpos.depth) continue;

                /* Pick a location and place the monster */
                find_empty(c, &y, &x);
                place_new_monster(p, c, y, x, race, MON_ASLEEP | MON_GROUP, ORIGIN_DROP);
            }
        }
    }

    /* Give a level feeling to this player */
    p->obj_feeling = -1;
    p->mon_feeling = -1;
    if (random_level(&p->wpos)) display_feeling(p, false);
    p->upkeep->redraw |= (PR_STATE);

    /* Player gets to go first */
    if (p->upkeep->new_level_method != LEVEL_GHOST) set_energy(p, &p->wpos);

    /* Hack -- enforce illegal panel */
    p->offset_y = z_info->dungeon_hgt;
    p->offset_x = z_info->dungeon_wid;

    /* Determine starting location */
    switch (p->upkeep->new_level_method)
    {
        /* Climbed down */
        case LEVEL_DOWN:
        {
            starty = c->join->down.y;
            startx = c->join->down.x;

            /* Never get pushed from stairs when entering a new level */
            if (new_level) delete_monster(c, starty, startx);
            break;
        }

        /* Climbed up */
        case LEVEL_UP:
        {
            starty = c->join->up.y;
            startx = c->join->up.x;

            /* Never get pushed from stairs when entering a new level */
            if (new_level) delete_monster(c, starty, startx);
            break;
        }

        /* Teleported level */
        case LEVEL_RAND:
            starty = c->join->rand.y;
            startx = c->join->rand.x;
            break;

        /* Used ghostly travel, stay in bounds */
        case LEVEL_GHOST:
            starty = MIN(MAX(p->py, 1), c->height - 2);
            startx = MIN(MAX(p->px, 1), c->width - 2);
            break;

        /* Over the river and through the woods */
        case LEVEL_OUTSIDE:
            starty = p->py;
            startx = p->px;
            break;

        /*
         * This is used instead of extending the level_rand_y/x
         * into the negative direction to prevent us from
         * allocating so many starting locations. Although this does
         * not make players teleport to similar locations, this
         * could be achieved by seeding the RNG with the depth.
         */
        case LEVEL_OUTSIDE_RAND:
        {
            /* Make sure we aren't in an "icky" location */
            do
            {
                starty = rand_range(1, c->height - 2);
                startx = rand_range(1, c->width - 2);
            }
            while (square_isvault(c, starty, startx) || !square_ispassable(c, starty, startx));
            break;
        }
    }

    /* Place the player */
    place_player(p, c, starty, startx);

    /* Add the player */
    c->squares[p->py][p->px].mon = 0 - id;

    /* Redraw */
    square_light_spot(c, p->py, p->px);

    /* Prevent hound insta-death */
    if (new_level) remove_hounds(p, c);

    /* Choose panel */
    verify_panel(p);

    /* Redraw */
    p->upkeep->redraw |= (PR_MAP | PR_DEPTH | PR_FLOOR | PR_MONSTER | PR_OBJECT | PR_MONLIST |
        PR_ITEMLIST);

    /* Fully update the visuals (and monster distances) */
    update_view(p, c);
    update_monsters(c, true);
    update_players();

    /* Clear the flag */
    p->upkeep->new_level_method = 0;

    /* Cancel the target */
    target_set_monster(p, NULL);

    /* Cancel tracking */
    cursor_track(p, NULL);

    /* Cancel the health bar */
    health_track(p->upkeep, NULL);

    /* Calculate torch radius */
    p->upkeep->update |= (PU_BONUS);

    /* Detect secret doors and traps */
    search(p, c);
}


/*
 * Give the player some energy.
 */
static void energize_player(struct player *p)
{
    int energy;
    struct chunk *c = chunk_get(&p->wpos);

    /* Player is idle */
    p->is_idle = has_energy(p);

    /* How much energy should we get? */
    energy = frame_energy(p->state.speed);

    /* Scale depending upon our time bubble */
    energy = energy * time_factor(p, c) / 100;

    /* Running speeds up time */
    if (p->upkeep->running) energy = energy * RUNNING_FACTOR / 100;

    /* Hack -- record that amount for player turn calculation */
    p->charge += energy;

    /* Make sure they don't have too much */
    if (p->energy < move_energy(p->wpos.depth))
    {
        /* Give the player some energy */
        p->energy += energy;
    }

    /* Paralyzed or Knocked Out player gets no turn */
    if (p->timed[TMD_PARALYZED] || p->timed[TMD_STUN] >= 100)
        do_cmd_sleep(p);
}


/*
 * Give monsters some energy.
 */
static void energize_monsters(struct chunk *c)
{
    int i;
    int mspeed, energy;

    /* Process the monsters (backwards) */
    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        struct monster *mon;

        /* Get a 'live' monster */
        mon = cave_monster(c, i);
        if (!mon->race) continue;

        /* Skip "unconscious" monsters */
        if (mon->hp == 0) continue;

        /* Calculate the net speed */
        mspeed = mon->mspeed;
        if (mon->m_timed[MON_TMD_FAST])
            mspeed += 10;
        if (mon->m_timed[MON_TMD_SLOW])
            mspeed -= 2;

        /* Obtain the energy boost */
        energy = frame_energy(mspeed);

        /* If we are within a player's time bubble, scale our energy */
        if (mon->closest_player)
        {
            energy = energy * time_factor(mon->closest_player, c) / 100;

            /* Speed up time if the player is running, except in town */
            if (!in_town(&c->wpos) && mon->closest_player->upkeep->running)
                energy = energy * RUNNING_FACTOR / 100;
        }

        /* Make sure we don't store up too much energy */
        if (mon->energy < move_energy(mon->wpos.depth))
        {
            /* Give this monster some energy */
            mon->energy += energy;
        }
    }
}


/*
 * Pre-turn game loop.
 */
static void pre_turn_game_loop(void)
{
    int i, x, y;

    on_new_level();

    /* Handle any network stuff */
    Net_input();

    /* Process monsters with even more energy first */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c) process_monsters(c, true);
            }
        }
    }

    /* Check for death */
    process_death();
}


/*
 * Post-turn game loop.
 */
static void post_turn_game_loop(void)
{
    int i, x, y;

    /* Check for death */
    process_death();

    /* Process the rest of the monsters */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c)
                {
                    process_monsters(c, false);

                    /* Mark all monsters as ready to act when they have the energy */
                    reset_monsters(c);
                }
            }
        }
    }

    /* Check for death */
    process_death();

    /* Process the objects */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c) process_objects(c);
            }
        }
    }

    /* Process the world */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                /* Process the world every ten turns */
                if (c && !(turn.turn % 10)) process_world(NULL, c);
            }
        }
    }

    /* Process the world */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Process the world of that player */
        if (!p->upkeep->new_level_method && !p->upkeep->funeral)
            process_world(p, chunk_get(&p->wpos));
    }

    /* Process everything else */
    process_various();

    /* Give energy to all players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Give the player some energy */
        if (!p->upkeep->new_level_method && !p->upkeep->funeral) energize_player(p);
    }

    /* Give energy to all monsters */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c) energize_monsters(c);
            }
        }
    }

    /* Count game turns */
    ht_add(&turn, 1);
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (p->upkeep->funeral) continue;

        /* Increment the game turn counter */
        ht_add(&p->game_turn, 1);

        /* Increment the player turn counter */
        if (p->charge >= move_energy(p->wpos.depth))
        {
            p->charge -= move_energy(p->wpos.depth);
            ht_add(&p->player_turn, 1);
        }

        /* Increment the active player turn counter */
        if (p->has_energy && !p->is_idle) ht_add(&p->active_turn, 1);

        /* Player has energy */
        p->has_energy = has_energy(p);

        /* Inform the client every second */
        if (!(turn.turn % cfg_fps))
        {
            Send_turn(p, ht_div(&p->game_turn, cfg_fps), ht_div(&p->player_turn, 1),
                ht_div(&p->active_turn, 1));
        }
    }

    /* Refresh everybody's displays */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = true;

        /* Refresh */
        refresh_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = false;
    }

    /* Send any information over the network */
    Net_output();

    /* Get rid of dead players */
    for (i = NumPlayers; i > 0; i--)
    {
        struct player *p = player_get(i);
        char buf[MSG_LEN];
        connection_t *connp = get_connection(p->conn);

        if (!p->upkeep->funeral) continue;
        if (connp->state == CONN_QUIT) continue;

        /* Format string */
        if (!p->alive)
        {
            if (!strcmp(p->died_from, "divine wrath"))
                my_strcpy(buf, "Killed by divine wrath", sizeof(buf));
            else if (!p->total_winner)
                my_strcpy(buf, "Terminated", sizeof(buf));
            else
                my_strcpy(buf, "Retired", sizeof(buf));
        }
        else if (p->ghost)
            strnfmt(buf, sizeof(buf), "Destroyed by %s", p->died_from);
        else
            strnfmt(buf, sizeof(buf), "Killed by %s", p->died_from);

        /* Get rid of him */
        /*Destroy_connection(p->conn, buf);*/
        connp->quit_msg = string_make(buf);
        Conn_set_state(connp, CONN_QUIT, 1);
    }

    /* Kick out starving players */
    for (i = NumPlayers; i > 0; i--)
    {
        struct player *p = player_get(i);

        if (!p->starving) continue;

        /* Kick him */
        Destroy_connection(p->conn, "Starving to death!");
    }

    on_leave_level();

    /* Make a new level if requested */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (p->upkeep->new_level_method) generate_new_level(p);
    }
}


/*
 * Shimmer multi-hued things every ten game turns.
 *
 * Called in turn-based mode when the player is idle.
 */
static void process_player_shimmer(struct player *p)
{
    struct chunk *c = chunk_get(&p->wpos);
    static byte loop = 0;
    int i;

    /* Every 10 game turns */
    loop++;
    if (loop < 10) return;
    loop = 0;

    /* Flicker self if multi-hued */
    if (p->poly_race && monster_shimmer(p->poly_race))
        square_light_spot_aux(p, c, p->py, p->px);

    /* Shimmer multi-hued objects */
    shimmer_objects(p, c);

    /* Efficiency */
    if (!c->scan_monsters) return;

    /* Shimmer multi-hued monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);

        /* Light that spot */
        if (mon->race && monster_shimmer(mon->race))
            square_light_spot_aux(p, c, mon->fy, mon->fx);
    }
}


/*
 * HIGHLY EXPERIMENTAL: turn-based mode (for single player games)
 *
 * Process player commands from the command queue, finishing when there is a
 * command using energy (any regular game command), or we run out of commands
 * and need another from the user, or the character changes level or dies, or
 * the game is stopped.
 */
static void process_player_turn_based(struct player *p)
{
    /* Try to execute any commands on the command queue. */
    /* NB: process_pending_commands may have deleted the connection! */
    if (process_pending_commands(p->conn)) return;

    /* Shimmer multi-hued things if idle */
    if (allow_shimmer(p) && has_energy(p)) process_player_shimmer(p);

    /* Process the player until they use some energy */
    if (has_energy(p)) return;

    if (!p->upkeep->new_level_method && !p->upkeep->funeral)
        process_player_cleanup(p);
}


/*
 * The main game loop.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 *
 * This is called every frame (1/FPS seconds).
 * A character will get frame_energy(speed) energy every frame and will be able to act once
 * move_energy(depth) energy has been accumulated. With a FPS of 75, a normal unhasted unburdened
 * character gets 1 player turn at 0' every (75 * 5 * 100) / (10 * 100 * 75) = 0.5 second.
 * Energy required will be doubled at 3000' and tripled at 4950', which means that a character
 * will need to be "Fast (+10)" and "Fast (+20)" at these depths to act every 0.5 second.
 *
 * Note that we process every player and the monsters, then quit. The "scheduling" code
 * (see sched.c) is the REAL main loop, which handles various inputs and timings.
 */
void run_game_loop(void)
{
    int i;

    /* HIGHLY EXPERIMENTAL: turn-based mode (for single player games) */
    if (TURN_BASED && process_turn_based())
    {
        struct player *p = player_get(1);

        /* Execute pre-turn processing if the player is not idle */
        if (!p->is_idle) pre_turn_game_loop();

        /* Process the player */
        if (!p->upkeep->new_level_method && !p->upkeep->funeral)
            process_player_turn_based(p);

        /* Execute post-turn processing if the player used some energy */
        if (!has_energy(p)) post_turn_game_loop();

        /* Player is idle: refresh and send info to client (for commands that don't use energy) */
        else
        {
            p->is_idle = true;

            /* Full refresh (includes monster/object lists) */
            p->full_refresh = true;

            /* Refresh */
            refresh_stuff(p);

            /* Normal refresh (without monster/object lists) */
            p->full_refresh = false;

            /* Send any information over the network */
            Net_output_p(p);
        }

        return;
    }

    /* Execute pre-turn processing */
    pre_turn_game_loop();

    /* Process the players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Process that player */
        if (!p->upkeep->new_level_method && !p->upkeep->funeral) process_player(p);
    }

    /* Execute post-turn processing */
    post_turn_game_loop();
}


/*
 * Change a player into a King!
 */
void kingly(struct player *p)
{
    /* Fake death */
    my_strcpy(p->death_info.died_from, "winner", sizeof(p->death_info.died_from));

    /* Restore the experience */
    p->exp = p->max_exp;

    /* Restore the level */
    p->lev = p->max_lev;

    /* Hack -- player gets an XP bonus for beating the game */
    p->exp = p->max_exp += 10000000L;

    /* Hack -- ensure we are retired */
    p->retire_timer = 0;
}


bool level_keep_allocated(struct chunk *c)
{
    /* Don't deallocate levels which contain players */
    if (chunk_has_players(&c->wpos)) return true;

    /* Don't deallocate special levels */
    if (special_level(&c->wpos)) return true;

    /* Hack -- don't deallocate levels which contain owned houses */
    return level_has_owned_houses(&c->wpos);
}


/*
 * Save the game
 *
 * PWMAngband: this should never be called, normal save is handled by shutdown_server() and
 * abnormal save is handled by exit_game_panic().
 */
static void save_game(struct player *p)
{
    /* Disturb the player */
    disturb(p, 1);

    /* Clear messages */
    message_flush(p);

    /* Handle stuff */
    handle_stuff(p);

    /* Message */
    msg(p, "Saving game...");

    /* The player is not dead */
    my_strcpy(p->died_from, "(saved)", sizeof(p->died_from));

    /* Save the player */
    if (save_player(p))
        msg(p, "Saving game... done.");

    /* Save failed (oops) */
    else
        msg(p, "Saving game... failed!");

    /* Note that the player is not dead */
    my_strcpy(p->died_from, "(alive and well)", sizeof(p->died_from));
}


/*
 * Preserve artifacts on the ground.
 */
static void preserve_artifacts(void)
{
    int i, wx, wy;

    for (wy = radius_wild; wy >= 0 - radius_wild; wy--)
    {
        for (wx = 0 - radius_wild; wx <= radius_wild; wx++)
        {
            struct wild_type *w_ptr = get_wt_info_at(wy, wx);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];
                struct object *obj;
                int y, x;

                /* Don't deallocate special levels */
                if (!c || level_keep_allocated(c)) continue;

                for (y = 0; y < c->height; y++)
                {
                    for (x = 0; x < c->width; x++)
                    {
                        for (obj = square_object(c, y, x); obj; obj = obj->next)
                        {
                            /* Hack -- preserve artifacts */
                            if (obj->artifact)
                            {
                                /* Only works when owner is ingame */
                                struct player *p = player_get(get_owner_id(obj));

                                /* Mark artifact as abandoned */
                                set_artifact_info(p, obj, ARTS_ABANDONED);

                                /* Preserve any artifact */
                                preserve_artifact_aux(obj);
                            }
                        }
                    }
                }
            }
        }
    }
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * PWMAngband: this should never be called, normal save is handled by shutdown_server() and
 * abnormal save is handled by exit_game_panic().
 */
static void close_game(void)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Handle stuff */
        handle_stuff(p);

        /* Flush the messages */
        message_flush(p);

        /* Handle death */
        if (p->is_dead)
        {
            /* Handle retirement */
            if (p->total_winner) kingly(p);

            /* Save memories */
            if (!save_player(p)) msg(p, "death save failed!");

            /* Handle score, show Top scores */
            enter_score(p, &p->death_info.time);
        }

        /* Still alive */
        else
        {
            /* Save the game */
            save_game(p);
        }
    }

    /* Preserve artifacts on the ground */
    preserve_artifacts();

    /* Try to save the server information */
    save_server_info();
}


/*
 * Actually play a game.
 */
void play_game(void)
{
    /* Start server initialization */
    server_generated = false;

    /* Flash a message */
    plog("Please wait...");

    /* Attempt to load the server state information */
    if (!load_server_info())
        quit("Broken server savefile");

    /* Initialize server state information */
    if (!server_state_loaded) server_birth();

    plog("Object flavors initialized...");

    /* Initialize visual prefs */
    textui_prefs_init();

    /* Initialize graphics info and basic user pref data */
    if (cfg_load_pref_file)
    {
        plog_fmt("Loading pref file: %s", cfg_load_pref_file);
        process_pref_file(cfg_load_pref_file, false);
    }

    /* Server initialization is now "complete" */
    server_generated = true;

    /* Set up the contact socket, so we can allow players to connect */
    setup_contact_socket();

    /* Set up the network server */
    if (Setup_net_server() == -1)
        quit("Couldn't set up net server");

    /* Set up the main loop */
    install_timer_tick(run_game_loop, cfg_fps);

    /* Loop forever */
    sched();

    /* This should never, ever happen */
    plog("FATAL ERROR sched() returned!");

    /* Close stuff */
    close_game();

    /* Quit */
    quit(NULL);
}


/*
 * Shut down the server
 *
 * This function is called when the server is closed, or from the PWMAngband console.
 *
 * In here we try to save everybody's game, as well as save the server state.
 */
void shutdown_server(void)
{
    plog("Shutting down.");

    /* Stop the main loop */
    remove_timer_tick();

    /* Kick every player out and save his game */
    while (NumPlayers > 0)
    {
        /* Note that we always save the first player */
        struct player *p = player_get(1);

        /* Indicate cause */
        my_strcpy(p->died_from, "server shutdown", sizeof(p->died_from));

        /* Try to save */
        if (!save_player(p)) Destroy_connection(p->conn, "Server shutdown (save failed)");

        /* Successful save */
        Destroy_connection(p->conn, "Server shutdown (save succeeded)");
    }

    /* Preserve artifacts on the ground */
    preserve_artifacts();

    /* Try to save the server information */
    if (!save_server_info()) plog("Server state save failed!");

    /* Successful save of server info */
    else plog("Server state save succeeded!");

    /* Tell the metaserver that we're gone */
    Report_to_meta(META_DIE);

    /* Quit normally */
    quit(NULL);
}


/*
 * Handle a fatal crash.
 *
 * Here we try to save every player's state, and the state of the server
 * in general. Note that we must be extremely careful not to have a crash
 * in this function, or some things may not get saved. Also, this function
 * may get called because some data structures are not in a "correct" state.
 * For this reason many paranoia checks are done to prevent bad pointer
 * dereferences.
 *
 * Note that this function would not be needed at all if there were no bugs.
 */
void exit_game_panic(void)
{
    int i = 1;

    /* If nothing important has happened, just return */
    if (!server_generated) return;

    plog("Shutting down (panic save).");

    /* Kick every player out and save his game */
    while (NumPlayers > (i - 1))
    {
        struct player *p = player_get(i);

        /* Don't dereference bad pointers */
        if (!p)
        {
            /* Skip to next player */
            i++;

            continue;
        }

        /* Hack -- turn off some things */
        disturb(p, 1);

        /* Hack -- delay death */
        if (p->chp < 0) p->is_dead = false;

        /* Indicate panic save */
        my_strcpy(p->died_from, "(panic save)", sizeof(p->died_from));

        /* Hack -- unstatic if the DM left while manually designing a dungeon level */
        if (chunk_inhibit_players(&p->wpos)) chunk_set_player_count(&p->wpos, 0);

        /*
         * Try to save the player, don't worry if this fails because there
         * is nothing we can do now anyway
         */
        save_player(p);
        i++;
    }

    /* Preserve artifacts on the ground */
    preserve_artifacts();

    /* Try to save the server information */
    if (!save_server_info()) plog("Server panic info save failed!");

    /* Successful panic save of server info */
    else plog("Server panic info save succeeded!");

    /* Don't re-enter */
    server_generated = false;

    /* Free resources */
    cleanup_angband();
}


/*
 * Windows specific replacement for signal handling
 */
static LPTOP_LEVEL_EXCEPTION_FILTER old_handler;


/*
 * Callback to be called by Windows when our term closes, the user
 * logs off, the system is shutdown, etc.
 */
static BOOL ctrl_handler(DWORD fdwCtrlType)
{
    /* Save everything and quit the game */
    shutdown_server();
    return TRUE;
}


/*
 * Global unhandled exception handler
 *
 * If the server crashes under Windows, this is where we end up
 */
static LONG WINAPI UnhandledExceptionFilter(struct _EXCEPTION_POINTERS* ExceptionInfo)
{
    /*
     * We don't report to the meta server in this case, the meta
     * server will detect that we've gone anyway
     */

    /*
     * Call the previous exception handler, which we are assuming
     * is the MinGW exception handler which should have been implicitly
     * setup when we loaded the exchndl.dll library.
     */
    if (old_handler != NULL) old_handler(ExceptionInfo);

    /* Record the crash */
    if (server_generated)
    {
        plog_fmt("Exception 0x%lX at 0x%lX", ExceptionInfo->ExceptionRecord->ExceptionCode,
            (unsigned long)ExceptionInfo->ExceptionRecord->ExceptionAddress);
    }

    /* Save everything and quit the game */
    exit_game_panic();

    /* We don't expect to ever get here... but for what it's worth... */
    return (EXCEPTION_EXECUTE_HANDLER);
}


void setup_exit_handler(void)
{
    /* Trap CTRL+C, Logoff, Shutdown, etc */
    if (SetConsoleCtrlHandler((PHANDLER_ROUTINE)ctrl_handler, true))
        plog("Initialised exit save handler.");
    else
        plog("ERROR: Could not set panic save handler!");

    /* Trap unhandled exceptions, i.e. server crashes */
    old_handler = SetUnhandledExceptionFilter(UnhandledExceptionFilter);
}