// File: cmd2.cpp
// Purpose: Movement commands (part 2)

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Simple command to "search" for one turn
 */
bool do_cmd_search(void)
{
    // Check for energy
    if (p_ptr->isBusy()) return FALSE;

    /* Take a turn */
    p_ptr->DrainEnergy(100);

    /* Search */
    search();
    
    // Did something
    return TRUE;
}


/*
 * Allocates objects upon opening a chest    -BEN-
 * Disperse treasures from the chest "i_ptr", centered at (x,y).
 */
static void chest_death(int y, int x, CItem *i_ptr)
{
    int i, d, ny, nx;
    int number, small;


    // Must be a chest
    if (i_ptr->GetTval() != TV_CHEST) return;

    // Determine if the chest is small
    small = (i_ptr->GetSval() < SV_CHEST_MIN_LARGE);

    // Determine how many items to drop
    number = (i_ptr->GetSval() % SV_CHEST_MIN_LARGE);

    // Generate some treasure
    if (i_ptr->GetPval() && (number > 0)) {
        /* Drop some objects (non-chests) */
        for ( ; number > 0; --number) {
            /* Try 20 times per item */
            for (i = 0; i < 20; ++i) {
                /* Pick a distance */
                d = ((i + 15) / 15);

                /* Pick a location */
                scatter(&ny, &nx, y, x, d, 0);

                /* Must be a clean floor grid */
                if (!clean_stackable_grid_bold(ny, nx)) continue;

                /* Opening a chest */
                opening_chest = TRUE;

                /* Small chests sometimes drop gold */
                if (small && percent(50)) {
                    place_gold(nx, ny, dun_level + 5);
                }

                /* Otherwise drop an item */
                else {
                    place_object(ny, nx, FALSE, FALSE, dun_level + 5);
                }

                /* No longer opening a chest */
                opening_chest = FALSE;

                /* Notice it */
                note_spot(ny, nx);

                /* Under the player */
                if ((nx == p_ptr->GetX()) && (ny == p_ptr->GetY())) {
                    msg_print("You feel something roll beneath your feet.");
                }

                /* Successful placement */
                break;
            }
        }
    }

    /* Empty */
    i_ptr->SetPval(0);

    /* Known */
    i_ptr->MakeKnown();
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, CItem *i_ptr)
{
    int  i, trap;


    /* Only analyze chests */
    if (i_ptr->GetTval() != TV_CHEST) return;

    /* Ignore disarmed chests */
    if (i_ptr->GetPval() <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[i_ptr->GetPval()];

    /* Lose strength */
    if (trap & CHEST_LOSE_STR) {
        msg_print("A small needle has pricked you!");
        p_ptr->take_hit(damroll(1, 4), "a poison needle");
        do_dec_stat(STAT_STR);
    }

    /* Lose constitution */
    if (trap & CHEST_LOSE_CON) {
        msg_print("A small needle has pricked you!");
        p_ptr->take_hit(damroll(1, 4), "a poison needle");
        do_dec_stat(STAT_CON);
    }

    /* Poison */
    if (trap & CHEST_POISON) {
        msg_print("A puff of green gas surrounds you!");
        if (!p_ptr->get_resists(RESIST_POIS) && !p_ptr->GetOpposePois()) {
            p_ptr->mod_poisoned(p_ptr->GetPoisoned() + 10 + randint(20));
        }
    }

    /* Paralyze */
    if (trap & CHEST_PARALYZE) {
        msg_print("A puff of yellow gas surrounds you!");
        if (!p_ptr->get_free_act()) {
            p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + 10 + randint(20));
        }
    }

    /* Summon monsters */
    if (trap & CHEST_SUMMON) {
        int num = 2 + randint(3);
        msg_print("You are enveloped in a cloud of smoke!");
        for (i = 0; i < num; i++) {
            summon_specific(y, x, dun_level, 0);
        }
    }

    /* Explode */
    if (trap & CHEST_EXPLODE) {
        msg_print("There is a sudden explosion!");
        msg_print("Everything inside the chest is destroyed!");
        i_ptr->SetPval(0);
        p_ptr->take_hit(damroll(5, 8), "an exploding chest");
    }
}



/*
 * Open a closed chest.
 */
static void do_open(int dir)
{
    int x, y, i, j, flag;
    CGrid *g_ptr;
    CItem *i_ptr;


    // Check for energy
    if (p_ptr->isBusy()) return;

    // Get requested location
    x = p_ptr->GetX() + dx[dir];
    y = p_ptr->GetY() + dy[dir];

    // Get requested grid
    g_ptr = &cave[y][x];

    // Get the object
    i_ptr = g_ptr->i_ptr;

    // Nothing useful
    if (!i_ptr || (i_ptr->GetTval() != TV_CHEST)) {
        msg_print("You see nothing there to open.");
    }

    /* Open a closed chest. */
    else if (i_ptr && (i_ptr->GetTval() == TV_CHEST)) {
        /* Take a turn */
        p_ptr->DrainEnergy(100);

        /* Assume opened successfully */
        flag = TRUE;

        /* Attempt to unlock it */
        if (i_ptr->GetPval() > 0) {
                  /* Assume locked, and thus not open */
            flag = FALSE;

            /* Get the "disarm" factor */
            i = p_ptr->GetSkill(SKILL_DIS);

            /* Penalize some conditions */
            if (p_ptr->GetBlind() || p_ptr->no_lite()) i = i / 10;
            if (p_ptr->GetConfused()) i = i / 10;

            /* Extract the difficulty */
            j = i - i_ptr->GetPval();

            /* Always have a small chance of success */
            if (j < 2) j = 2;

            /* Success -- May still have traps */
            if (percent(j)) {
                msg_print("You have picked the lock.");
                p_ptr->gain_exp(1);
                flag = TRUE;
            }

            /* Failure -- Keep trying */
            else {
                // We may continue repeating
                msg_print("You failed to pick the lock.");
            }
        }

        // Allowed to open
        if (flag) {
            // Apply chest traps, if any
            chest_trap(y, x, i_ptr);

            // Let the chest drop items
            chest_death(y, x, i_ptr);
        }
    }
}



/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
static bool twall(int y, int x)
{
    CGrid *g_ptr = &cave[y][x];

    // Paranoia -- Require a wall or door or some such
    if (floor_grid_bold(y, x)) return FALSE;

    // Remove the feature
    g_ptr->set_feat(CF_FLOOR);

    // Forget the "field mark"
    g_ptr->flags &= ~MAP_KNOW;

    // Notice
    note_spot(y, x);

    // Update some things
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

    // Result
    return TRUE;
}



/*
 * Disarms a trap, or chest     -RAK-
 */
static void do_disarm(int dir)
{
    int x, y, i, j, power;
    CGrid *g_ptr;
    CItem *i_ptr;


    // Need energy
    if (p_ptr->isBusy()) return;

    // Get location 
    x = p_ptr->GetX() + dx[dir];
    y = p_ptr->GetY() + dy[dir];

    // Get grid and contents
    g_ptr = &cave[y][x];

    // Access the item
    i_ptr = g_ptr->i_ptr;

    /* Nothing useful */
    if ((!i_ptr || i_ptr->GetTval() != TV_CHEST) && !g_ptr->is_visible_trap()) {
        msg_print("You see nothing there to disarm.");
    }

    // Normal disarm
    else if (i_ptr && (i_ptr->GetTval() == TV_CHEST)) {
        // Take energy
        p_ptr->DrainEnergy(100);

        // Get the "disarm" factor
        i = p_ptr->GetSkill(SKILL_DIS);

        // Penalize some conditions
        if (p_ptr->GetBlind() || p_ptr->no_lite()) i = i / 10;
        if (p_ptr->GetConfused()) i = i / 10;

        // Extract the difficulty 
        j = i - i_ptr->GetPval();

        // Always have a small chance of success
        if (j < 2) j = 2;

        // Must find the trap first.
        if (!i_ptr->isKnown()) {
            msg_print("I don't see any traps.");
        }

        // Already disarmed/unlocked
        else if (i_ptr->GetPval() <= 0) {
            msg_print("The chest is not trapped.");
        }

        // No traps to find.
        else if (!chest_traps[i_ptr->GetPval()]) {
            msg_print("The chest is not trapped.");
        }

        // Success (get a lot of experience) 
        else if (rand_int(100) < j) {
            msg_print("You have disarmed the chest.");
            p_ptr->gain_exp(i_ptr->GetPval());
            i_ptr->SetPval(-i_ptr->GetPval());
        }

        /* Failure -- Keep trying */
        else if ((i > 5) && (randint(i) > 5)) {
            /* We may keep trying */
            msg_print("You failed to disarm the chest.");
        }

        /* Failure -- Set off the trap */
        else {
            msg_print("You set off a trap!");
            chest_trap(y, x, i_ptr);
        }
    }

    // Disarm a trap
    else {
        // Get the "disarm" factor
        i = p_ptr->GetSkill(SKILL_DIS);

        // Penalize some conditions
        if (p_ptr->GetBlind() || p_ptr->no_lite()) i = i / 10;
        if (p_ptr->GetConfused()) i = i / 10;

        // XXX XXX XXX Variable power?

        // Extract trap "power"
        power = 5;

        // Extract the difficulty 
        j = i - power;

        // Always have a small chance of success
        if (j < 2) j = 2;

        // Don't succeed too much
        if (j > 100) j = 100;

        // Make sure i is at least 5
        if (i < 5) i = 5;

        // Success
        if (percent(j)) {
            /* Message */
            msg_print("You have disarmed the trap.");

            /* Reward */
            p_ptr->gain_exp(power);

            /* Remove the trap */
            g_ptr->set_feat(CF_FLOOR);

            /* Forget the "field mark" */
            g_ptr->flags &= ~MAP_KNOW;

            /* Notice */
            note_spot(y, x);

            /* Move the player onto the trap grid */
            move_player(dir);
        }

        // Failure -- Keep trying
        else if (randint(i) > 5) {
            // Take energy
            p_ptr->DrainEnergy(100);

            /* Message */
            msg_print("You failed to disarm the trap.");
        }

        /* Failure -- Set off the trap */
        else {
            /* Message */
            msg_print("You set off the trap!");

            /* Move the player onto the trap */
            move_player(dir);
        }
    }
}


/*
 * Alter in a direction
 */
void do_alter(int dir)
{
    int x, y, i, j;
    bool gold=FALSE;
    CGrid *g_ptr;

    // Acquire location
    x = p_ptr->GetX() + dx[dir];
    y = p_ptr->GetY() + dy[dir];

    // Get grid
    g_ptr = &cave[y][x];

    // If not known, do nothing
    if (!(g_ptr->flags & MAP_KNOW)) return;

    // Need energy
    if (p_ptr->isBusy()) return;


    // Set last move
    p_ptr->last_move = dir;

    // Attack monsters
    if (g_ptr->m_ptr) {
        p_ptr->DrainEnergy(100 / p_ptr->get_num_blow());
        p_ptr->attack(g_ptr->m_ptr);
        return;
    }


    // Switch on the feature in the grid
    switch (g_ptr->get_feat()) {   
        case CF_NOTHING:
        case CF_FLOOR:
        case CF_TRAP_INVIS:
        case CF_GLYPH:
        case CF_SHOP_GENERAL:
        case CF_SHOP_ARMORER:
        case CF_SHOP_WEAPON:
        case CF_SHOP_TEMPLE:
        case CF_SHOP_ALCHEMIST:
        case CF_SHOP_MAGIC:
        case CF_SHOP_BLACK:
        case CF_SHOP_HOME:
            // Do nothing
            break;

        case CF_DOOR_BROKEN:
            msg_print("The door appears to be broken.");
            return;

        case CF_DOOR_OPEN:
            // Take a turn 
            p_ptr->DrainEnergy(100);

            // Close the door
            g_ptr->set_feat(CF_DOOR_CLOSED);

            // Notice
            note_spot(y, x);

            // Update some things
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);
            return;

        case CF_DOOR_CLOSED:
            // Use energy
            p_ptr->DrainEnergy(100);

            // Open the door
            g_ptr->set_feat(CF_DOOR_OPEN);

            // Notice
            note_spot(y, x);

            // Update some things
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);
            return;

        case CF_DOOR_LOCKED_1:
        case CF_DOOR_LOCKED_2:
        case CF_DOOR_LOCKED_3:
        case CF_DOOR_LOCKED_4:
        case CF_DOOR_LOCKED_5:
        case CF_DOOR_LOCKED_6:
        case CF_DOOR_LOCKED_7:
            // Take a turn
            p_ptr->DrainEnergy(100);

            // Disarm factor
            i = p_ptr->GetSkill(SKILL_DIS);

            // Penalize some conditions
            if (p_ptr->GetBlind() || p_ptr->no_lite()) i = i / 10;
            if (p_ptr->GetConfused()) i = i / 10;

            // Modify by difficulty
            j = i - g_ptr->get_door_lock_strength()*4;

            // Always have a small chance of success
            if (j < 2) j = 2;

            // Success
            if (percent(j)) {
                // Message
                msg_print("You have picked the lock.");

                // Experience
                p_ptr->gain_exp(1);

                // Open the door
                g_ptr->set_feat(CF_DOOR_OPEN);

                // Notice
                note_spot(y, x);

                // Update some things
                p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);
            }

            // Failure
            else {
                msg_print("You failed to pick the lock.");
            }
            return;

        // Go up
        case CF_STAIR_UP_NE:
        case CF_STAIR_UP_NW:
        case CF_STAIR_UP_SW:
        case CF_STAIR_UP_SE:
            // Energy
            p_ptr->DrainEnergy(100);

            // Success
            msg_print("You enter a maze of up staircases.");

            // Go up the stairs
            dun_level--;
            new_level_flag = TRUE;

            // Create a way back
            create_down_stair = TRUE;

            return;

        // Go down
        case CF_STAIR_DOWN_NE:
        case CF_STAIR_DOWN_NW:
        case CF_STAIR_DOWN_SW:
        case CF_STAIR_DOWN_SE:
            // Drain energy
            p_ptr->DrainEnergy(100);

            // Success 
            msg_print("You enter a maze of down staircases.");

            // Go down the stairs
            dun_level++;
            new_level_flag = TRUE;

            // Create a way back
            create_up_stair = TRUE;

            return;

        case CF_TRAP_TRAP_DOOR:
        case CF_TRAP_OPEN_PIT:
        case CF_TRAP_SPIKED_PIT:
        case CF_TRAP_POISON_PIT:
        case CF_TRAP_SUMMON:
        case CF_TRAP_TELEPORT:
        case CF_TRAP_FIRE:
        case CF_TRAP_ACID:
        case CF_TRAP_DART_SLOW:
        case CF_TRAP_DART_STR:
        case CF_TRAP_DART_DEX:
        case CF_TRAP_DART_CON:
        case CF_TRAP_GAS_BLIND:
        case CF_TRAP_GAS_CONFUSE:
        case CF_TRAP_GAS_POISON:
        case CF_TRAP_GAS_SLEEP:
            // Disarm
            do_disarm(dir);
            return;

        case CF_DOOR_SECRET:
            // Give a message in vain, take a turn
            msg_print("You tunnel into the granite wall.");
            p_ptr->DrainEnergy(100);
            return;

        case CF_RUBBLE:
            // Take a turn
            p_ptr->DrainEnergy(100);

            // Update some things
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW |
                PU_MONSTERS);

            // Remove the rubble
            if ((p_ptr->GetSkill(SKILL_DIG) > rand_int(200)) && twall(y,x)) {
                // Message
                msg_print("You have removed the rubble.");

                // Hack -- place an object
                if (percent(10)) {
                    place_object(y, x, FALSE, FALSE, dun_level);
                    if (p_ptr->can_see_bold(y, x)) {
                        msg_print("You have found something!");
                    }
                }

                /* Notice */
                note_spot(y, x);
            }

            else {
                // Message, keep digging
                msg_print("You dig in the rubble.");
            }
            return;

        case CF_GRANITE_BASIC:
        case CF_GRANITE_INNER:
        case CF_GRANITE_OUTER:
        case CF_GRANITE_SOLID:
            // Take a turn
            p_ptr->DrainEnergy(100);

            // Update some things
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW |
                PU_MONSTERS);

            /* Tunnel */
            if ((p_ptr->GetSkill(SKILL_DIG) > 40 + rand_int(1600)) && twall(y, x)) {
                msg_print("You have finished the tunnel.");
            }

            /* Keep trying */
            else {
                /* We may continue tunelling */
                msg_print("You tunnel into the granite wall.");
            }
            return;

        case CF_PERMANENT_BASIC:
        case CF_PERMANENT_INNER:
        case CF_PERMANENT_OUTER:
        case CF_PERMANENT_SOLID:
            // Give a message in vain, take a turn
            msg_print("This seems to be permanent rock.");
            p_ptr->DrainEnergy(100);
            return;

        case CF_MAGMA_H: 
        case CF_MAGMA_K:
	    p_ptr->DrainEnergy(100);
	    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | 
			    PU_FLOW | PU_MONSTERS); 
	    if ((p_ptr->GetSkill(SKILL_DIG) > 10 + rand_int(400)) && twall(y, x)) 
            {
		     place_gold(x, y, dun_level + 5);
		     msg_print("You have found something!");
	    }
            else
		    msg_print("You tunnel into the vein.");
	    return ;
        case CF_QUARTZ_H: gold = TRUE;	    
	case CF_QUARTZ_K: gold = TRUE;
	    
	    p_ptr->DrainEnergy(100);
	    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | 
			    PU_FLOW | PU_MONSTERS); 
            if ((p_ptr->GetSkill(SKILL_DIG) > 20 + rand_int(800)) && twall(y, x)) 
            {
		     place_gold(x, y, dun_level + 10);
		     msg_print("You have found something!");
	    }
            else
		    msg_print("You tunnel into the vein.");
	    return ; 
        case CF_MAGMA:

	    p_ptr->DrainEnergy(100);
	    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | 
			    PU_FLOW | PU_MONSTERS); 
            if ((p_ptr->GetSkill(SKILL_DIG) > 10 + rand_int(400)) && twall(y, x)) 
			     msg_print("You have finished the tunnel.");

	    else
		    msg_print("You tunnel into the vein.");
	    return;
	    
        case CF_QUARTZ:
            	   
	    p_ptr->DrainEnergy(100);
	    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | 
			    PU_FLOW | PU_MONSTERS); 
            if ((p_ptr->GetSkill(SKILL_DIG) > 20 + rand_int(800)) && twall(y, x)) 
			     msg_print("You have finished the tunnel.");

	    else
		    msg_print("You tunnel into the vein.");
	    return;
    }
    

    // Chests
    if (g_ptr->i_ptr) {
        CItem *i_ptr = g_ptr->i_ptr;

        // Is it a chest?
        if (i_ptr->GetTval() == TV_CHEST) {
            if (!i_ptr->isKnown() || (i_ptr->GetPval() <= 0)) {
                do_open(dir);
            }
            else {
                do_disarm(dir);
            }
        }
    }
}



/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Items are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_fire(int tx, int ty)
{
    int item;
    int tdam, thits, tmul;
    int bonus, chance;
    CItem throw_obj, *i_ptr, *j_ptr;
    double vx, vy, vel, n;


    // Get the "bow" (if any)
    j_ptr = &inventory[INVEN_BOW];

    // Require a launcher
    if (!j_ptr->GetTval()) {
        msg_print("You have nothing to fire with.");
        return;
    }


    // Get the missile
    item = INVEN_ARROW;
    i_ptr = &inventory[item];

    // Check for the proper tval
    if (p_ptr->get_tval_ammo() != i_ptr->GetTval()) {
        msg_print("You cannot fire your ammunition with your current bow.");
        return;
    }

    // Need energy
    if (p_ptr->isBusy()) return;


    /* Create a "local missile object" */
    throw_obj = *i_ptr;
    throw_obj.SetNumber(1);

    /* Reduce and describe inventory */
    inven_item_increase(item, -1);
    inven_item_describe(item);
    inven_item_optimize(item);

    /* Use the missile object */
    i_ptr = &throw_obj;


    /* Use the proper number of shots */
    thits = p_ptr->get_num_fire();

    /* Base damage from thrown object plus launcher bonus */
    tdam = i_ptr->GetDamRoll() + i_ptr->GetToD() + j_ptr->GetToD();

    /* Actually "fire" the object */
    bonus = p_ptr->get_to_h() + i_ptr->GetToH() + j_ptr->GetToH();
    chance = p_ptr->GetSkill(SKILL_THB) + bonus*BTH_PLUS_ADJ;

    /* Assume a base multiplier */
    tmul = 1;

    /* Analyze the launcher */
    switch (j_ptr->GetSval()) {
        /* Sling and ammo */
        case SV_SLING:
            tmul = 2;
            break;

        /* Short Bow and Arrow */
        case SV_SHORT_BOW:
            tmul = 2;
            break;

        /* Long Bow and Arrow */
        case SV_LONG_BOW:
            tmul = 3;
            break;

        /* Light Crossbow and Bolt */
        case SV_LIGHT_XBOW:
            tmul = 3;
            break;

        /* Heavy Crossbow and Bolt */
        case SV_HEAVY_XBOW:
            tmul = 4;
            break;
    }

    /* Get extra "power" from "extra might" */
    if (p_ptr->get_xtra_might()) tmul++;

    // Boost the damage
    tdam *= tmul;

    // Velocity of projectile
    vel = (double)(10 + 5*tmul) / 59.0;


    // Take a (partial) turn
    p_ptr->DrainEnergy(100 / thits);


    // Calculate velocities in x and y directions
    vx = tx-p_ptr->GetX();
    vy = ty-p_ptr->GetY();
    n = sqrt(vx*vx+vy*vy);
    if (n < 1E-5) {
        vx = vy = 0;
    }
    else {
        vel /= n;
        vx *= vel; vy *= vel;
    }

    // Create a projectile
    CArrow *arrow = new CArrow(p_ptr->GetX(), p_ptr->GetY(), 1.75, vx, vy, 0.0, p_ptr,
        i_ptr, chance, tdam);
    add_projectile(arrow);

#if 0
    /* Chance of breakage */
    j = breakage_chance(i_ptr);

    /* Double the chance if we hit a monster */
    if (hit_body) j = j * 2;

    /* Paranoia -- maximum breakage chance */
    if (j > 100) j = 100;

    /* Drop (or break) near that location */
    drop_near(i_ptr, j, y, x);
#endif
}


/*
 * Prepare to launch missile(s) with right mouse button
 */
void do_cmd_fire(void)
{
    int item;
    CItem *i_ptr, *j_ptr;


    // Get the "bow" (if any)
    j_ptr = &inventory[INVEN_BOW];

    // Require a launcher
    if (!j_ptr->GetTval()) {
        msg_print("You have nothing to fire with.");
        return;
    }


    // Get the missile
    item = INVEN_ARROW;
    i_ptr = &inventory[item];

    // Check for the proper tval
    if (p_ptr->get_tval_ammo() != i_ptr->GetTval()) {
        msg_print("You cannot fire your ammunition with your current bow.");
        return;
    }


    // Set up to fire
    current_spell_type = 2;
}
