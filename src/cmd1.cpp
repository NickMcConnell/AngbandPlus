// File: cmd1.cpp
// Purpose: Movement commands (part 1)

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit(int chance, int ac, int vis)
{
    int k;

    // Percentile dice 
    k = rand_int(100);

    // Hack -- Instant miss or hit
    if (k < 10) return (k < 5);

    // Wimpy attack never hits
    if (chance <= 0) return FALSE;

    // Penalize invisible targets
    if (!vis) chance = (chance + 1) / 2;

    // Power must defeat armor
    if (rand_int(chance) < (ac * 3 / 4)) return FALSE;

    // Assume hit
    return TRUE;
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
s16b critical_shot(int weight, int plus, int dam)
{
    int i, k;

    /* Extract "shot" power */
    i = weight + (p_ptr->get_to_h() + plus)*4 + p_ptr->GetLev()*2;

    /* Critical hit */
    if (randint(5000) <= i) {
        k = weight + randint(500);

        if (k < 500) {
            msg_print("It was a good shot!");
            dam = 2 * dam + 5;
        }
        else if (k < 1000) {
            msg_print("It was a great shot!");
            dam = 2 * dam + 10;
        }
        else {
            msg_print("It was a superb shot!");
            dam = 3 * dam + 15;
        }
    }

    return (dam);
}



/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
s16b critical_norm(int weight, int plus, int dam, CMonster *m_ptr)
{
    int i, k;

    // Extract "blow" power
    i = weight + (p_ptr->get_to_h() + plus)*5 + p_ptr->GetLev()*3;

    /* Chance */
    if (((p_ptr->GetClass() == CLASS_ROGUE) && (m_ptr->get_csleep() > 0)) ||
        (randint(5000) <= i))
    {
        k = weight + randint(650);

        if (k < 400) {
            msg_print("It was a good hit!");
            dam = 2 * dam + 5;
        }
        else if (k < 700) {
            msg_print("It was a great hit!");
            dam = 2 * dam + 10;
        }
        else if (k < 900) {
            msg_print("It was a superb hit!");
            dam = 3 * dam + 15;
        }
        else if (k < 1300) {
            msg_print("It was a *GREAT* hit!");
            dam = 3 * dam + 20;
        }
        else {
            msg_print("It was a *SUPERB* hit!");
            dam = ((7 * dam) / 2) + 25;
        }
    }

    return (dam);
}



/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
s16b tot_dam_aux(CItem *i_ptr, int tdam, CMonster *m_ptr)
{
    int mult = 1;
    CMonsterRace *r_ptr = m_ptr->get_r_ptr();
    u32b f1, f2, f3;

    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);

    /* Some "weapons" and "ammo" do extra damage */
    switch (i_ptr->GetTval()) {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:

            /* Slay Animal */
            if ((f1 & TR1_SLAY_ANIMAL) && (r_ptr->flags3 & RF3_ANIMAL)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_ANIMAL;

                if (mult < 2) mult = 2;
            }

            /* Slay Evil */
            if ((f1 & TR1_SLAY_EVIL) && (r_ptr->flags3 & RF3_EVIL)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_EVIL;

                if (mult < 2) mult = 2;
            }

            /* Slay Undead */
            if ((f1 & TR1_SLAY_UNDEAD) && (r_ptr->flags3 & RF3_UNDEAD)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_UNDEAD;

                if (mult < 3) mult = 3;
            }

            /* Slay Demon */
            if ((f1 & TR1_SLAY_DEMON) && (r_ptr->flags3 & RF3_DEMON)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_DEMON;

                if (mult < 3) mult = 3;
            }

            /* Slay Orc */
            if ((f1 & TR1_SLAY_ORC) && (r_ptr->flags3 & RF3_ORC)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_ORC;

                if (mult < 3) mult = 3;
            }

            /* Slay Troll */
            if ((f1 & TR1_SLAY_TROLL) && (r_ptr->flags3 & RF3_TROLL)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_TROLL;

                if (mult < 3) mult = 3;
            }

            /* Slay Giant */
            if ((f1 & TR1_SLAY_GIANT) && (r_ptr->flags3 & RF3_GIANT)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_GIANT;

                if (mult < 3) mult = 3;
            }

            /* Slay Dragon  */
            if ((f1 & TR1_SLAY_DRAGON) && (r_ptr->flags3 & RF3_DRAGON)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_DRAGON;

                if (mult < 3) mult = 3;
            }

            /* Execute Dragon */
            if ((f1 & TR1_KILL_DRAGON) && (r_ptr->flags3 & RF3_DRAGON)) {
                if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_DRAGON;

                if (mult < 5) mult = 5;
            }


            /* Brand (Acid) */
            if (f1 & TR1_BRAND_ACID) {
                /* Notice immunity */
                if (r_ptr->flags3 & RF3_IM_ACID) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_IM_ACID;
                }

                /* Otherwise, take the damage */
                else {
                    if (mult < 3) mult = 3;
                }
            }

            /* Brand (Elec) */
            if (f1 & TR1_BRAND_ELEC) {
                /* Notice immunity */
                if (r_ptr->flags3 & RF3_IM_ELEC) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_IM_ELEC;
                }

                /* Otherwise, take the damage */
                else {
                    if (mult < 3) mult = 3;
                }
            }

            /* Brand (Fire) */
            if (f1 & TR1_BRAND_FIRE) {
                /* Notice vulnerability */
                if (r_ptr->flags3 & RF3_HURT_FIRE) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_HURT_FIRE;
                    if (mult < 5) mult = 5;
                }

                /* Notice immunity */
                if (r_ptr->flags3 & RF3_IM_FIRE) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_IM_FIRE;
                }

                /* Otherwise, take the damage */
                else {
                    if (mult < 3) mult = 3;
                }
            }

            /* Brand (Poison */
            if (f1 & TR1_BRAND_POIS) {
                /* Notice immunity */
                if (r_ptr->flags3 & RF3_IM_POIS) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_IM_POIS;
                }

                /* Otherwise, take the damage */
                else {
                    if (mult < 3) mult = 3;
                }
            }

            /* Brand (Cold) */
            if (f1 & TR1_BRAND_COLD) {
                /* Notice vulnerability */
                if (r_ptr->flags3 & RF3_HURT_COLD) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_HURT_COLD;
                    if (mult < 5) mult = 5;
                }

                /* Notice immunity */
                if (r_ptr->flags3 & RF3_IM_COLD) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_IM_COLD;
                }

                /* Otherwise, take the damage */
                else {
                    if (mult < 3) mult = 3;
                }
            }
    }


    /* Return the total damage */
    return (tdam * mult);
}


/*
 * Searches for hidden things.                  -RAK-   
 */
void search(void)
{
    int y, x, chance;
    CGrid *g_ptr;
    CItem *i_ptr;


    /* Start with base search ability */
    chance = p_ptr->GetSkill(SKILL_SRH);

    /* Penalize various conditions */
    if (p_ptr->GetBlind() || p_ptr->no_lite()) chance = chance / 10;
    if (p_ptr->GetConfused()) chance = chance / 10;

    /* Search the nearby grids, which are always in bounds */
    for (x = p_ptr->GetX() - 1; x <= p_ptr->GetX() + 1; x++) {
        for (y = p_ptr->GetY() - 1; y <= p_ptr->GetY() + 1; y++) {
            /* Sometimes, notice things */
            if (percent(chance)) {
                /* Access the grid */
                g_ptr = &cave[y][x];

                /* Access the object */
                i_ptr = g_ptr->i_ptr;

                /* Invisible trap */
                if (g_ptr->get_feat() == CF_TRAP_INVIS) {
                    /* Pick a trap */
                    pick_trap(y, x);

                    /* Message */
                    msg_print("You have found a trap.");
                }

                /* Secret door */
                else if (g_ptr->get_feat() == CF_DOOR_SECRET) {
                    /* Message */
                    msg_print("You have found a secret door.");

                    /* Pick a door XXX XXX XXX */
                    g_ptr->set_feat(CF_DOOR_CLOSED);

                    /* Notice */
                    note_spot(y, x);
                }

                /* Search chests */
                else if (i_ptr && (i_ptr->GetTval() == TV_CHEST)) {
                    /* Examine chests for traps */
                    if (!i_ptr->isKnown() && (chest_traps[i_ptr->GetPval()])) {

                        /* Message */
                        msg_print("You have discovered a trap on the chest!");

                        /* Know the trap */
                        i_ptr->MakeKnown();
                    }
                }
            }
        }
    }
}




/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
void carry(void)
{
    CGrid *g_ptr = p_ptr->get_g_ptr();
    CItem *i_ptr = g_ptr->i_ptr, *j_ptr, *next_i_ptr;
    char i_name[80];


    // Loop through all objects in this pile
    while (i_ptr) {
        // Get the next object
        next_i_ptr = i_ptr->next_i_ptr;

        // Describe the object
        i_ptr->object_desc(i_name, TRUE, 3);

        // Pick up gold
        if (i_ptr->GetTval() == TV_GOLD) {
            // Message
            msg_format("You have found %ld gold pieces worth of %s.",
                (long)i_ptr->GetPval(), i_name);

            // Collect the gold
            p_ptr->SetGold(p_ptr->GetGold() + i_ptr->GetPval());

            // Delete gold
            delete_object(i_ptr);
        }

        // Pick it up
        else {
            // The pack is too full
            if (!inven_carry_okay(i_ptr)) {
                msg_format("You have no room for %s.", i_name);
            }

            // Pick up the item
            else {
                int slot;

                // Carry the item
                slot = inven_carry(i_ptr);

                // Get the item again
                j_ptr = &inventory[slot];

                // Describe the object
                j_ptr->object_desc(i_name, TRUE, 3);

                // Message 
                msg_format("You have %s (%c).", i_name, index_to_label(slot));

                // Delete original
                delete_object(i_ptr);
            }
        }

        i_ptr = next_i_ptr;
    }
}





/*
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
    int i, num, dam;
    CGrid *g_ptr;
    char *name = "a trap";


    /* Get the cave grid */
    g_ptr = p_ptr->get_g_ptr();

    /* Examine the trap sub-val */
    switch (g_ptr->get_feat()) {
        case CF_TRAP_TRAP_DOOR:
            msg_print("You fell through a trap door!");
            if (p_ptr->get_ffall()) {
                msg_print("You float gently down to the next level.");
            }
            else {
                dam = damroll(2, 8);
                p_ptr->take_hit(dam, name);
            }
            new_level_flag = TRUE;
            dun_level++;
            break;

        case CF_TRAP_OPEN_PIT:
            msg_print("You fell into a pit!");
            if (p_ptr->get_ffall()) {
                msg_print("You float gently to the bottom of the pit.");
            }
            else {
                dam = damroll(2, 6);
                p_ptr->take_hit(dam, name);
            }
            break;

        case CF_TRAP_SPIKED_PIT:
            msg_print("You fall into a spiked pit!");

            if (p_ptr->get_ffall()) {
                msg_print("You float gently to the floor of the pit.");
                msg_print("You carefully avoid touching the spikes.");
            }

            else {
                // Base damage
                dam = damroll(2, 6);

                // Extra spike damage
                if (percent(50)) {
                    msg_print("You are impaled!");

                    dam = dam * 2;
                    p_ptr->mod_cut(p_ptr->GetCut() + randint(dam));
                }

                // Take the damage
                p_ptr->take_hit(dam, name);
            }
            break;

        case CF_TRAP_POISON_PIT:
            msg_print("You fall into a spiked pit!");

            if (p_ptr->get_ffall()) {
                msg_print("You float gently to the floor of the pit.");
                msg_print("You carefully avoid touching the spikes.");
            }

            else {
                /* Base damage */
                dam = damroll(2, 6);

                /* Extra spike damage */
                if (percent(50)) {
                    msg_print("You are impaled on poisonous spikes!");

                    dam = dam * 2;
                    p_ptr->mod_cut(p_ptr->GetCut() + randint(dam));

                    if (p_ptr->get_resists(RESIST_POIS) || p_ptr->GetOpposePois()) {
                        msg_print("The poison does not affect you!");
                    }

                    else {
                        dam = dam * 2;
                        p_ptr->mod_poisoned(p_ptr->GetPoisoned() + randint(dam));
                    }
                }

                /* Take the damage */
                p_ptr->take_hit(dam, name);
            }

            break;

        case CF_TRAP_SUMMON:
            msg_print("You are enveloped in a cloud of smoke!");
            g_ptr->set_feat(CF_FLOOR);
            g_ptr->flags &= ~MAP_KNOW;
            note_spot(p_ptr->GetY(), p_ptr->GetX());
            num = 2 + randint(3);
            for (i = 0; i < num; i++) {
                summon_specific(p_ptr->GetY(), p_ptr->GetX(), dun_level, 0);
            }
            break;

        case CF_TRAP_TELEPORT:
            msg_print("You hit a teleport trap!");
            teleport_player(100);
            break;

        case CF_TRAP_FIRE:
            msg_print("You are enveloped in flames!");
            dam = damroll(4, 6);
            fire_dam(dam, "a fire trap");
            break;

        case CF_TRAP_ACID:
            msg_print("You are splashed with acid!");
            dam = damroll(4, 6);
            acid_dam(dam, "an acid trap");
            break;

        case CF_TRAP_DART_SLOW:
            if (test_hit(125, p_ptr->GetTotalAC(), TRUE)) {
                msg_print("A small dart hits you!");
                dam = damroll(1, 4);
                p_ptr->take_hit(dam, name);
                p_ptr->mod_slow(p_ptr->GetSlow() + rand_int(20) + 20);
            }
            else {
                msg_print("A small dart barely misses you.");
            }
            break;

        case CF_TRAP_DART_STR:
            if (test_hit(125, p_ptr->GetTotalAC(), TRUE)) {
                msg_print("A small dart hits you!");
                dam = damroll(1,4);
                p_ptr->take_hit(dam, name);
                do_dec_stat(STAT_STR);
            }
            else {
                msg_print("A small dart barely misses you.");
            }
            break;

        case CF_TRAP_DART_DEX:
            if (test_hit(125, p_ptr->GetTotalAC(), TRUE)) {
                msg_print("A small dart hits you!");
                dam = damroll(1, 4);
                p_ptr->take_hit(dam, name);
                do_dec_stat(STAT_DEX);
            }
            else {
                msg_print("A small dart barely misses you.");
            }
            break;

        case CF_TRAP_DART_CON:
            if (test_hit(125, p_ptr->GetTotalAC(), TRUE)) {
                msg_print("A small dart hits you!");
                dam = damroll(1,4);
                p_ptr->take_hit(dam, name);
                do_dec_stat(STAT_CON);
            }
            else {
                msg_print("A small dart barely misses you.");
            }
            break;

        case CF_TRAP_GAS_BLIND:
            msg_print("A black gas surrounds you!");
            if (!p_ptr->get_resists(RESIST_BLIND)) {
                p_ptr->mod_blind(p_ptr->GetBlind() + rand_int(50) + 25);
            }
            break;

        case CF_TRAP_GAS_CONFUSE:
            msg_print("A gas of scintillating colors surrounds you!");
            if (!p_ptr->get_resists(RESIST_CONF)) {
                p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(20) + 10);
            }
            break;

        case CF_TRAP_GAS_POISON:
            msg_print("A pungent green gas surrounds you!");
            if (!p_ptr->get_resists(RESIST_POIS) && !p_ptr->GetOpposePois()) {
                p_ptr->mod_poisoned(p_ptr->GetPoisoned() + rand_int(20) + 10);
            }
            break;

        case CF_TRAP_GAS_SLEEP:
            msg_print("A strange white mist surrounds you!");
            if (!p_ptr->get_free_act()) {
                p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + rand_int(10) + 5);
            }
            break;
    }
}



/*
 * Player attacks a (poor, defenseless) creature        -RAK-   
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void CPlayer::attack(CMonster *m_ptr)
{
    int num = 0, k, bonus, chance;
    CMonsterRace *r_ptr = m_ptr->get_r_ptr();
    CItem *i_ptr;
    char m_name[80];
    bool fear = FALSE;
    bool do_quake = FALSE;


    // Set last action
    p_ptr->action = 10;


    // Extract monster name (or "it")
    m_ptr->get_desc(m_name, 0);


    // Handle player fear
    if (GetAfraid()) {
        msg_format("You are too afraid to attack %s!", m_name);
        return;
    }

    // Handle shadowform
    if (GetShadowform()) {
        msg_format("You are too insubstantial to attack %s!", m_name);
        return;
    }


    // Access the weapon
    i_ptr = &inventory[INVEN_WIELD];

    // Calculate the "attack quality"
    bonus = get_to_h() + i_ptr->GetToH();
    chance = GetSkill(SKILL_THN) + (bonus * BTH_PLUS_ADJ);


    // Attack once for each legal blow
    while (num++ < 1) {
    //while (num++ < get_num_blow()) {
        /* Test for hit */
        if (test_hit(chance, r_ptr->ac, m_ptr->is_visible())) {
            //# Hit

            // Message
            msg_format("You hit %s.", m_name);

            // Hack -- bare hands do one damage
            k = 1;

            // Handle normal weapon
            if (i_ptr->exists()) {
                k = i_ptr->GetDamRoll();
                k = tot_dam_aux(i_ptr, k, m_ptr);
                if (get_impact() && (k > 50)) do_quake = TRUE;
                k = critical_norm(i_ptr->GetWeight(), i_ptr->GetToH(), k, m_ptr);
                k += i_ptr->GetToD();
            }

            // Apply the player damage bonuses
            k += get_to_d();

            // No negative damage
            if (k < 0) k = 0;

            // Damage, check for fear and death
            if (m_ptr->mon_take_hit(k, &fear, NULL)) break;

            /* Disturb monster */
            m_ptr->set_csleep(0);

            /* Confusion attack */
            if (GetConfusing()) {
                /* Cancel glowing hands */
                SetConfusing(FALSE);

                /* Message */
                msg_print("Your hands stop glowing.");

                /* Confuse the monster */
                if (r_ptr->flags3 & RF3_NO_CONF) {
                    if (m_ptr->is_visible()) r_ptr->r_flags3 |= RF3_NO_CONF;
                    msg_format("%^s is unaffected.", m_name);
                }
                else if (percent(r_ptr->level)) {
                    msg_format("%^s is unaffected.", m_name);
                }
                else {
                    msg_format("%^s appears confused.", m_name);
                    m_ptr->set_confused(m_ptr->get_confused() +
                        10 + rand_int(p_ptr->GetLev()) / 5);
                }
            }
        }

        /* Player misses */
        else {
            //# Miss

            /* Message */
            msg_format("You miss %s.", m_name);
        }
    }


    /* Hack -- delay fear messages */
    if (fear && m_ptr->is_visible()) {
        //# Flee

        /* Message */
        msg_format("%^s flees in terror!", m_name);
    }


    /* Mega-Hack -- apply earthquake brand */
    if (do_quake) earthquake(GetY(), GetX(), 10);
}





/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir)
{
    int x, y;
    CGrid *g_ptr;

    // Set last move
    p_ptr->last_move = dir;


    /* Find the result of moving */
    x = p_ptr->GetX() + dx[dir];
    y = p_ptr->GetY() + dy[dir];

    /* Examine the destination */
    g_ptr = &cave[y][x];


    // Hack -- attack monsters
    if (g_ptr->m_ptr) {
        // Attack
        p_ptr->DrainEnergy(100 / p_ptr->get_num_blow());
        p_ptr->attack(g_ptr->m_ptr);
        return;
    }

    // Normal movement if not a wall
    if (floor_grid_bold(y, x)) {
        // Take away some energy
        p_ptr->DrainEnergy(100);

        // Move the player
        p_ptr->SetLocation(x, y);

        // Set the action
        p_ptr->action = dir;

        // Update stuff
        p_ptr->set_update(p_ptr->get_update() | PU_DISTANCE | PU_VIEW | PU_LITE | PU_FLOW);


        // Handle "store doors"
        if (g_ptr->is_store_door()) {
            force_enter_store = TRUE;
        }

        /* Discover invisible traps */
        else if (g_ptr->get_feat() == CF_TRAP_INVIS) {
            /* Message */
            msg_print("You found a trap!");

            /* Pick a trap */
            pick_trap(p_ptr->GetY(), p_ptr->GetX());

            /* Hit the trap */
            hit_trap();
        }

        /* Set off an visible trap */
        else if (g_ptr->is_visible_trap()) {
            /* Hit the trap */
            hit_trap();
        }
    }
}



/*
 * Simple command to walk in some direction
 */
bool do_cmd_walk(int dir)
{
    // Check for energy
    if (p_ptr->isBusy()) return FALSE;

    // Search
    move_player(dir);

    // Did something
    return TRUE;
}