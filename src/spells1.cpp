// File: spells1.c
// Purpose: Spell code (part 1)

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"





/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * XXX XXX XXX This function may be very expensive...
 */
s16b poly_r_idx(int r_idx)
{
    CMonsterRace *r_ptr = &r_info[r_idx];

    int i, r, lev1, lev2;

    /* Hack -- Uniques never polymorph */
    if (r_ptr->flags1 & RF1_UNIQUE) return (r_idx);

    /* Pick a (possibly new) non-unique race */
    for (i = 0; i < 1000; i++) {
        /* Allowable range of "levels" for resulting monster */
        lev1 = r_ptr->level - ((randint(20)/randint(9))+1);
        lev2 = r_ptr->level + ((randint(20)/randint(9))+1);

        // Pick a new race from an increased depth
        r = get_mon_num((dun_level + r_ptr->level) / 2 + 5);

        /* Handle failure */
        if (!r) continue;

        /* Extract that monster */
        r_ptr = &r_info[r];

        /* Skip uniques */
        if (r_ptr->flags1 & RF1_UNIQUE) continue;

        /* Accept valid monsters */
        if ((r_ptr->level < lev1) && (r_ptr->level > lev2)) continue;

        /* Use that index */
        r_idx = r;

        /* Done */
        break;
    }

    /* Use the original */
    return r_idx;
}


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
void teleport_away(CMonster *m_ptr, int dis)
{
    int nx, ny, ox, oy, d, i, min;
    bool look = TRUE;


    /* Save the old location */
    m_ptr->GetLocation(&ox, &oy);

    // Default new location: old location
    m_ptr->GetLocation(&nx, &ny);

    /* Minimum distance */
    min = dis / 2;

    // Look until done
    do {
        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++) {
            /* Pick a (possibly illegal) location */
            while (1) {
                nx = rand_spread(ox, dis);
                ny = rand_spread(oy, dis);
                d = distance(ox, oy, nx, ny);
                if ((d >= min) && (d <= dis)) break;
            }

            // Ignore illegal locations
            if (!in_bounds(ny, nx)) continue;

            /* Require "empty" floor space */
            if (!empty_grid_bold(ny, nx)) continue;

            /* Hack -- no teleport onto glyph of warding */
            if (cave[ny][nx].get_feat() == CF_GLYPH) continue;

            /* No teleporting into vaults and such */
            /* if (cave[ny][nx].flags & MAP_ICKY) continue; */

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    } while (look);

    /* Update the new location */
    cave[ny][nx].m_ptr = m_ptr;

    /* Update the old location */
    cave[oy][ox].m_ptr = NULL;

    /* Move the monster */
    m_ptr->SetLocation(nx, ny);

    /* Update the monster (new location) */
    m_ptr->update();
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 */
void teleport_player(int dis)
{
    int d, i, min, ox, oy, x, y;
    
    p_ptr->GetLocation(&x, &y);

    bool look = TRUE;

    /* Minimum distance */
    min = dis / 2;

    /* Look until done */
    while (look) {
        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++) {
            /* Pick a (possibly illegal) location */
            while (1) {
                x = rand_spread(p_ptr->GetX(), dis);
                y = rand_spread(p_ptr->GetY(), dis);
                d = distance(p_ptr->GetX(), p_ptr->GetY(), x, y);
                if ((d >= min) && (d <= dis)) break;
            }

            /* Ignore illegal locations */
            if (!in_bounds(y, x)) continue;

            /* Require "naked" floor space */
            if (!naked_grid_bold(y, x)) continue;

            /* No teleporting into vaults and such */
            if (cave[y][x].flags & MAP_ICKY) continue;

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    }

    /* Save the old location */
    p_ptr->GetLocation(&ox, &oy);

    /* Move the player */
    p_ptr->SetLocation(x, y);

    /* Update stuff */
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW);

    /* Update the monsters */
    p_ptr->set_update(p_ptr->get_update() | PU_DISTANCE);

    /* Update stuff XXX XXX XXX */
    update_stuff();
}



/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx)
{
    int y, x, oy, ox, dis = 0, ctr = 0;


    // Find a usable location
    while (1) {
        // Pick a nearby legal location
        while (1) {
            y = rand_spread(ny, dis);
            x = rand_spread(nx, dis);
            if (in_bounds(y, x)) break;
        }

        /* Accept "naked" floor grids */
        if (naked_grid_bold(y, x)) break;

        /* Occasionally advance the distance */
        if (++ctr > (4 * dis * dis + 4 * dis + 1)) {
            ctr = 0;
            dis++;
        }
    }

    // Save the old location
    ox = p_ptr->GetX();
    oy = p_ptr->GetY();

    // Move the player
    p_ptr->SetLocation(x, y);

    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW);

    // Update the monsters
    p_ptr->set_update(p_ptr->get_update() | PU_DISTANCE);

    // Update stuff XXX XXX XXX
    update_stuff();
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
    if (!dun_level) {
        msg_print("You sink through the floor.");
        dun_level++;
        new_level_flag = TRUE;
    }
    else if (is_quest(dun_level) || (dun_level >= MAX_DEPTH-1)) {
        msg_print("You rise up through the ceiling.");
        dun_level--;
        new_level_flag = TRUE;
    }
    else if (percent(50)) {
        msg_print("You rise up through the ceiling.");
        dun_level--;
        new_level_flag = TRUE;
    }
    else {
        msg_print("You sink through the floor.");
        dun_level++;
        new_level_flag = TRUE;
    }
}






/*
 * Checks for Shadowform and then calls take_hit_internal().
 */
void CPlayer::take_hit(int damage, char *hit_from)
{
    /* Paranoia */
    if (death) return;

    /* Hack -- Apply shadowform */
    if (GetShadowform() && (damage < 9000)) damage = (damage + 1) / 2;

    /* Call other function */
    take_hit_internal(damage, hit_from);    
}





/*
 * Decreases players hit points and sets death flag if necessary.  Does
 * not permit Shadowform to have an effect.
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */
void CPlayer::take_hit_internal(int damage, char *hit_from)
{
    int old_chp = GetCHP();
    int warning = (GetMHP() * hitpoint_warn / 10);


    // Paranoia
    if (death) return;


    /* Hurt the player */
    SetCHP(GetCHP() - damage);

    /* Dead player */
    if (GetCHP() < 0) {
        //# Death

        // Hack -- Note death
        mini_message_box("Death", "You have died.");

        // Note cause of death
        strcpy(died_from, hit_from);

        // No longer a winner
        total_winner = FALSE;

        // Note death
        death = TRUE;

        /* Dead */
        return;
    }

    /* Hack -- hitpoint warning */
    if (warning && (GetCHP() <= warning)) {
        /* Hack -- bell on first notice */
        if (alert_hitpoint && (old_chp > warning)) bell();

        /* Message */
        msg_print("*** LOW HITPOINT WARNING! ***");
        turn_based = TRUE;
    }
}





/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind.  Also sling ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool CItem::hatesAcid(void)
{
    // Analyze the type
    switch (GetTval()) {
        // Wearable items
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:

        // Staffs/Scrolls are wood/paper
        case TV_STAFF:
        case TV_SCROLL:

        // Ouch (?)
        case TV_CHEST:

        // Junk is useless
        case TV_SKELETON:
        case TV_BOTTLE:

            return TRUE;
    }

    return FALSE;
}


/*
 * Does a given object (usually) hate electricity?
 */
bool CItem::hatesElec(void)
{
    switch (GetTval()) {
        case TV_RING:
        case TV_WAND:
            return TRUE;
    }

    return FALSE;
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
bool CItem::hatesFire(void)
{
    // Analyze the type
    switch (GetTval()) {
        // Wearable
        case TV_LITE:
        case TV_ARROW:
        case TV_BOW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:

        // Books
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:

        // Chests
        case TV_CHEST:

        // Staffs/Scrolls burn
        case TV_STAFF:
        case TV_SCROLL:
            return TRUE;
    }

    return FALSE;
}


/*
 * Does a given object (usually) hate cold?
 */
bool CItem::hatesCold(void)
{
    switch (GetTval()) {
        case TV_POTION:
        case TV_FLASK:
        case TV_BOTTLE:
            return TRUE;
    }
    return FALSE;
}









/*
 * Melt something
 */
static int set_acid_destroy(CItem *i_ptr)
{
    u32b f1, f2, f3;
    if (!i_ptr->hatesAcid()) return FALSE;
    i_ptr->GetFlags(&f1, &f2, &f3);
    if (f3 & TR3_IGNORE_ACID) return FALSE;
    return TRUE;
}


/*
 * Electrical damage
 */
static int set_elec_destroy(CItem *i_ptr)
{
    u32b f1, f2, f3;
    if (!i_ptr->hatesElec()) return FALSE;
    i_ptr->GetFlags(&f1, &f2, &f3);
    if (f3 & TR3_IGNORE_ELEC) return FALSE;
    return TRUE;
}


/*
 * Burn something
 */
static int set_fire_destroy(CItem *i_ptr)
{
    u32b f1, f2, f3;
    if (!i_ptr->hatesFire()) return (FALSE);
    i_ptr->GetFlags(&f1, &f2, &f3);
    if (f3 & TR3_IGNORE_FIRE) return (FALSE);
    return (TRUE);
}


/*
 * Freeze things
 */
static int set_cold_destroy(CItem *i_ptr)
{
    u32b f1, f2, f3;
    if (!i_ptr->hatesCold()) return (FALSE);
    i_ptr->GetFlags(&f1, &f2, &f3);
    if (f3 & TR3_IGNORE_COLD) return (FALSE);
    return TRUE;
}




/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(CItem *);

/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 * Destruction taken from "melee.c" code for "stealing".
 * Returns number of items destroyed.
 */
static int inven_damage(inven_func typ, int perc)
{
    int         i, j, k, amt;

    CItem *i_ptr;

    char        i_name[80];


    /* Count the casualties */
    k = 0;

    /* Scan through the slots backwards */
    for (i = 0; i < INVEN_PACK; i++) {
        // Get the item in that slot
        i_ptr = &inventory[i];

        // Hack -- for now, skip artifacts
        if (i_ptr->isArtifact()) continue;

        /* Give this item slot a shot at death */
        if ((*typ)(i_ptr)) {
            /* Count the casualties */
            for (amt = j = 0; j < i_ptr->GetNumber(); ++j) {
                if (percent(perc)) amt++;
            }

            /* Some casualities */
            if (amt) {
                /* Get a description */
                i_ptr->object_desc(i_name, FALSE, 3);

                /* Message */
                msg_format("%sour %s (%c) %s destroyed!",
                    (i_ptr->isPlural() ?
                    ((amt == i_ptr->GetNumber()) ? "All of y" :
                    (amt > 1 ? "Some of y" : "One of y")) : "Y"),
                    i_name, index_to_label(i),
                    ((amt > 1) ? "were" : "was"));

                /* Destroy "amt" items */
                inven_item_increase(i,-amt);
                inven_item_optimize(i);

                /* Count the casualties */
                k += amt;
            }
        }
    }

    /* Return the casualty count */
    return (k);
}




/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
    CItem *i_ptr = NULL;

    u32b f1, f2, f3;

    char i_name[80];

    int slot = 0;


    /* Pick a (possibly empty) inventory slot */
    switch (randint(6)) {
        case 1: slot = INVEN_BODY; break;
        case 2: slot = INVEN_ARM; break;
        case 3: slot = INVEN_OUTER; break;
        case 4: slot = INVEN_HANDS; break;
        case 5: slot = INVEN_HEAD; break;
        case 6: slot = INVEN_FEET; break;
    }
    i_ptr = &inventory[slot];

    /* Nothing to damage */
    if (!i_ptr->exists()) return FALSE;

    /* Describe */
    i_ptr->object_desc(i_name, FALSE, 0);

    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);

    /* Item resists */
    if (f3 & TR3_IGNORE_ACID) {
        msg_format("Your %s is unaffected!", i_name);
        return TRUE;
    }

    /* Is it destroyed? */
    if (i_ptr->GetAC() + i_ptr->GetToA() <= 0) {
        /* Message */
        msg_format("Your %s is destroyed!", i_name);

        /* Destroy the item */
        inven_item_increase(slot, -1);
        inven_item_optimize(slot);
    }
    else {
        /* Message */
        msg_format("Your %s is damaged!", i_name);

        /* Damage the item */
        i_ptr->SetToA(i_ptr->GetToA() - 1);
    }

    /* Calculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Item was damaged */
    return TRUE;
}


/*
 * Hurt the player with Acid
 */
void acid_dam(int dam, char *kb_str)
{
    int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

    /* Total Immunity */
    if (p_ptr->get_immunes(IMMUNE_ACID) || (dam <= 0)) return;

    /* Resist the damage */
    if (p_ptr->get_resists(RESIST_ACID)) dam = (dam + 2) / 3;
    if (p_ptr->GetOpposeAcid()) dam = (dam + 2) / 3;

    /* If any armor gets hit, defend the player */
    if (minus_ac()) dam = (dam + 1) / 2;

    /* Take damage */
    p_ptr->take_hit(dam, kb_str);

    /* Inventory damage */
    inven_damage(set_acid_destroy, inv);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, char *kb_str)
{
    int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

    /* Total immunity */
    if (p_ptr->get_immunes(IMMUNE_ELEC) || (dam <= 0)) return;

    /* Resist the damage */
    if (p_ptr->get_resists(RESIST_ELEC)) dam = (dam + 2) / 3;
    if (p_ptr->GetOpposeElec()) dam = (dam + 2) / 3;

    /* Take damage */
    p_ptr->take_hit(dam, kb_str);

    /* Inventory damage */
    inven_damage(set_elec_destroy, inv);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, char *kb_str)
{
    int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

    /* Totally immune */
    if (p_ptr->get_immunes(IMMUNE_FIRE) || (dam <= 0)) return;

    /* Resist the damage */
    if (p_ptr->get_resists(RESIST_FIRE)) dam = (dam + 2) / 3;
    if (p_ptr->GetOpposeFire()) dam = (dam + 2) / 3;

    /* Take damage */
    p_ptr->take_hit(dam, kb_str);

    /* Inventory damage */
    inven_damage(set_fire_destroy, inv);
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, char *kb_str)
{
    int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

    /* Total immunity */
    if (p_ptr->get_immunes(IMMUNE_COLD) || (dam <= 0)) return;

    /* Resist the damage */
    if (p_ptr->get_resists(RESIST_COLD)) dam = (dam + 2) / 3;
    if (p_ptr->GetOpposeCold()) dam = (dam + 2) / 3;

    /* Take damage */
    p_ptr->take_hit(dam, kb_str);

    /* Inventory damage */
    inven_damage(set_cold_destroy, inv);
}





/*
 * Increases a stat by one randomized level             -RAK-   
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
bool inc_stat(int stat)
{
    int value, gain;

    /* Then augment the current/max stat */
    value = p_ptr->GetStatCur(stat);

    /* Cannot go above 18/100 */
    if (value < 18+100) {
        /* Gain one (sometimes two) points */
        if (value < 18) {       
            gain = (percent(75) ? 1 : 2);
            value += gain;
        }

        /* Gain 1/6 to 1/3 of distance to 18/100 */
        else if (value < 18+98) {
            /* Approximate gain value */
            gain = (((18+100) - value) / 2 + 3) / 2;

            /* Paranoia */
            if (gain < 1) gain = 1;

            /* Apply the bonus */
            value += randint(gain) + gain / 2;

            /* Maximal value */
            if (value > 18+99) value = 18 + 99;
        }

        /* Gain one point at a time */
        else {
            value++;
        }

        /* Save the new value */
        p_ptr->SetStatCur(stat, value);

        /* Bring up the maximum too */
        if (value > p_ptr->GetStatMax(stat)) {
            p_ptr->SetStatMax(stat, value);
        }

        /* Recalculate bonuses */
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

        /* Success */
        return (TRUE);
    }

    /* Nothing to gain */
    return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults.  -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, int permanent)
{
    int cur, max, loss, same, res = FALSE;


    /* Acquire current value */
    cur = p_ptr->GetStatCur(stat);
    max = p_ptr->GetStatMax(stat);

    /* Note when the values are identical */
    same = (cur == max);

    /* Damage "current" value */
    if (cur > 3) {
        /* Handle "low" values */
        if (cur <= 18) {
            if (amount > 90) cur--;
            if (amount > 50) cur--;
            if (amount > 20) cur--;
            cur--;
        }

        /* Handle "high" values */
        else {
            /* Hack -- Decrement by a random amount between one-quarter
             * and one-half of the stat bonus times the percentage, with a
             * minimum damage of half the percentage. -CWS
             */
            loss = (((cur-18) / 2 + 1) / 2 + 1);

            /* Paranoia */
            if (loss < 1) loss = 1;

            /* Randomize the loss */
            loss = ((randint(loss) + loss) * amount) / 100;

            /* Maximal loss */
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            cur = cur - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (cur < 18) cur = (amount <= 20) ? 18 : 17;
        }

        /* Prevent illegal values */
        if (cur < 3) cur = 3;

        /* Something happened */
        if (cur != p_ptr->GetStatCur(stat)) res = TRUE;
    }

    /* Damage "max" value */
    if (permanent && (max > 3)) {
        /* Handle "low" values */
        if (max <= 18) {
            if (amount > 90) max--;
            if (amount > 50) max--;
            if (amount > 20) max--;
            max--;
        }

        /* Handle "high" values */
        else {
            /* Hack -- Decrement by a random amount between one-quarter
             * and one-half of the stat bonus times the percentage, with a
             * minimum damage of half the percentage. -CWS
             */
            loss = (((max-18) / 2 + 1) / 2 + 1);
            loss = ((randint(loss) + loss) * amount) / 100;
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            max = max - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (max < 18) max = (amount <= 20) ? 18 : 17;
        }

        /* Hack -- keep it clean */
        if (same || (max < cur)) max = cur;

        /* Something happened */
        if (max != p_ptr->GetStatMax(stat)) res = TRUE;
    }

    /* Apply changes */
    if (res) {
        /* Actually set the stat to its new value. */
        p_ptr->SetStatCur(stat, cur);
        p_ptr->SetStatMax(stat, max);

        /* Recalculate bonuses */
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);
    }

    /* Done */
    return res;
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
    /* Restore if needed */
    if (p_ptr->GetStatCur(stat) != p_ptr->GetStatMax(stat)) {
        /* Restore */
        p_ptr->SetStatCur(stat, p_ptr->GetStatMax(stat));

        /* Recalculate bonuses */
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

        /* Success */
        return TRUE;
    }

    /* Nothing to restore */
    return FALSE;
}




/*
 * Apply disenchantment to the player's stuff
 *
 * XXX XXX XXX This function is also called from the "melee" code
 *
 * The "mode" is currently unused.
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(int mode)
{
    int                 t = 0;

    CItem         *i_ptr;

    char                i_name[80];


    /* Unused */
    mode = mode;


    /* Pick a random slot */
    switch (randint(8)) {
         case 1: t = INVEN_WIELD; break;
         case 2: t = INVEN_BOW; break;
         case 3: t = INVEN_BODY; break;
         case 4: t = INVEN_OUTER; break;
         case 5: t = INVEN_ARM; break;
         case 6: t = INVEN_HEAD; break;
         case 7: t = INVEN_HANDS; break;
         case 8: t = INVEN_FEET; break;
    }

    /* Get the item */
    i_ptr = &inventory[t];

    /* No item, nothing happens */
    if (!i_ptr->exists()) return FALSE;


    /* Nothing to disenchant */
    if ((i_ptr->GetToH() <= 0) && (i_ptr->GetToD() <= 0) &&
        (i_ptr->GetToA() <= 0)) {
        /* Nothing to notice */
        return FALSE;
    }


    /* Describe the object */
    i_ptr->object_desc(i_name, FALSE, 0);


    /* Artifacts have 60% chance to resist */
    if (i_ptr->isArtifact() && percent(60)) {
        /* Message */
        msg_format("Your %s (%c) resist%s disenchantment!",
                   i_name, index_to_label(t),
                   ((i_ptr->GetNumber() != 1) ? "" : "s"));

        /* Notice */
        return (TRUE);
    }


    /* Disenchant tohit */
    if (i_ptr->GetToH() > 0) i_ptr->SetToH(i_ptr->GetToH() - 1);
    if ((i_ptr->GetToH() > 5) && percent(20)) {
        i_ptr->SetToH(i_ptr->GetToH() - 1);
    }

    /* Disenchant todam */
    if (i_ptr->GetToD() > 0) i_ptr->SetToD(i_ptr->GetToD() - 1);
    if ((i_ptr->GetToD() > 5) && percent(20)) {
        i_ptr->SetToD(i_ptr->GetToD() - 1);
    }

    /* Disenchant toac */
    if (i_ptr->GetToA() > 0) i_ptr->SetToA(i_ptr->GetToA() - 1);
    if ((i_ptr->GetToA() > 5) && percent(20)) {
        i_ptr->SetToA(i_ptr->GetToA() - 1);
    }

    /* Message */
    msg_format("Your %s (%c) %s disenchanted!",
               i_name, index_to_label(t),
               ((i_ptr->GetNumber() != 1) ? "were" : "was"));

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Notice */
    return (TRUE);
}


/*
 * Apply Nexus
 */
static void apply_nexus(CLiving *l_ptr)
{
    int max1, cur1, max2, cur2, ii, jj;

    switch (randint(7)) {
        case 1: case 2: case 3:
            teleport_player(200);
            break;

        case 4: case 5:
            teleport_player_to(l_ptr->GetY(), l_ptr->GetX());
            break;

        case 6:
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
                break;
            }

            /* Teleport Level */
            teleport_player_level();
            break;

        case 7:
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
                break;
            }
            msg_print("Your body starts to scramble...");

            /* Pick a pair of stats */
            ii = rand_int(6);
            do { jj = rand_int(6); } while (ii == jj);

            max1 = p_ptr->GetStatMax(ii);
            cur1 = p_ptr->GetStatCur(ii);
            max2 = p_ptr->GetStatMax(jj);
            cur2 = p_ptr->GetStatCur(jj);

            p_ptr->SetStatMax(ii, max2);
            p_ptr->SetStatCur(ii, cur2);
            p_ptr->SetStatMax(jj, max1);
            p_ptr->SetStatCur(jj, cur1);

            p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

            break;
    }
}





/*
 * Hack -- track "affected" monsters
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;



/*
 * We are called from "project()" to "damage" terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 */
static bool project_f(CLiving *who, int r, int y, int x, int dam, int typ)
{
    CGrid *g_ptr = &cave[y][x];
    bool obvious = FALSE;
    int div;


    /* Extract radius */
    div = r + 1;

    /* Decrease damage */
    dam = dam / div;


    /* Analyze the type */
    switch (typ)
    {
        /* XXX XXX XXX should destroy doors */
        case GF_ACID:
        case GF_FIRE:
        case GF_PLASMA:
        case GF_METEOR:
        case GF_MANA:
            break;

        /* Ignore most effects */
        case GF_ELEC:
        case GF_COLD:
        case GF_ICE:
        case GF_SHARDS:
        case GF_FORCE:
        case GF_SOUND:
        case GF_HOLY_ORB:
            break;

        /* Destroy Traps (and Locks) */
        case GF_KILL_TRAP:
            // Destroy invisible traps
            if ((g_ptr->get_feat() == CF_TRAP_INVIS) || g_ptr->is_visible_trap()) {
                /* Hack -- special message */
                if (p_ptr->can_see_bold(y, x)) {
                    msg_print("There is a bright flash of light!");
                    obvious = TRUE;
                }

                /* Destroy the trap */
                g_ptr->set_feat(CF_FLOOR);

                /* Notice */
                note_spot(y, x);
            }

            // Locked doors are found and unlocked
            else if (g_ptr->is_locked_door()) {
                /* Notice */
                if (p_ptr->can_see_bold(y, x)) {
                    msg_print("Click!");
                    obvious = TRUE;
                }

                /* Unlock the door */
                g_ptr->set_feat(CF_DOOR_CLOSED);

                /* Notice */
                note_spot(y, x);
            }

            break;

        // Destroy Doors (and traps)
        case GF_KILL_DOOR:
            // Destroy traps
            if ((g_ptr->get_feat() == CF_TRAP_INVIS) || g_ptr->is_visible_trap() ||
                g_ptr->is_door()) {
                // Hack -- special message
                if (p_ptr->can_see_bold(y, x)) {
                    msg_print("There is a bright flash of light!");
                    obvious = TRUE;
                }

                // Destroy the feature
                g_ptr->set_feat(CF_FLOOR);

                // Forget the wall
                g_ptr->flags &= ~MAP_KNOW;

                // Notice
                note_spot(y, x);

                // Update some things
                p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);
            }

            break;


        // Destroy walls (and doors)
        case GF_KILL_WALL:      
            // Non-walls (etc)
            if (floor_grid_bold(y, x)) break;

            // Permanent walls
            if (g_ptr->is_permawall()) break;

            // Granite
            if (g_ptr->is_wall()) {
                // Message
                if (g_ptr->flags & MAP_KNOW) {
                    msg_print("The wall turns into mud!");
                    obvious = TRUE;
                }

                // Destroy the wall
                g_ptr->set_feat(CF_FLOOR);
            }

            /* Rubble */
            else if (g_ptr->get_feat() == CF_RUBBLE) {
                /* Message */
                if (g_ptr->flags & MAP_KNOW) {
                    msg_print("The rubble turns into mud!");
                    obvious = TRUE;
                }

                /* Destroy the rubble */
                g_ptr->set_feat(CF_FLOOR);

                /* Hack -- place an object */
                if (percent(10)) {
                    /* Found something */
                    if (p_ptr->can_see_bold(y, x)) {
                        msg_print("There was something buried in the rubble!");
                        obvious = TRUE;
                    }

                    /* Place gold */
                    place_object(y, x, FALSE, FALSE, dun_level);
                }
            }

            // Destroy doors
            else if (g_ptr->is_door()) {
                /* Hack -- special message */
                if (g_ptr->flags & MAP_KNOW) {
                    msg_print("The door turns into mud!");
                    obvious = TRUE;
                }

                /* Destroy the feature */
                g_ptr->set_feat(CF_FLOOR);
            }

            /* Forget the wall */
            g_ptr->flags &= ~MAP_KNOW;

            /* Notice */
            note_spot(y, x);

            /* Update some things */
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

            break;

        /* Make doors */
        case GF_MAKE_DOOR:
            // Require a "naked" floor grid
            if (!naked_grid_bold(y, x)) break;

            // Create a closed door
            g_ptr->set_feat(CF_DOOR_CLOSED);

            // Notice
            note_spot(y, x);

            // Observe
            if (g_ptr->flags & MAP_KNOW) obvious = TRUE;

            // Update some things
            p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);

            break;

        /* Make traps */
        case GF_MAKE_TRAP:

            /* Require a "naked" floor grid */
            if (!naked_grid_bold(y, x)) break;

            /* Place a trap */
            place_trap(y, x);

            /* Notice */
            note_spot(y, x);

            break;

        /* Lite up the grid */
        case GF_LITE_WEAK:
        case GF_LITE:

            /* Turn on the light */
            g_ptr->flags |= MAP_GLOW;

            /* Notice */
            note_spot(y, x);

            /* Observe */
            if (p_ptr->can_see_bold(y,x)) obvious = TRUE;

            /* Mega-Hack -- Update the monster in the affected grid */
            /* This allows "spear of light" (etc) to work "correctly" */
            if (g_ptr->m_ptr) g_ptr->m_ptr->update();

            break;

        /* Darken the grid */
        case GF_DARK_WEAK:
        case GF_DARK:

            /* Notice */
            if (p_ptr->can_see_bold(y,x)) obvious = TRUE;

            /* Turn off the light. */
            g_ptr->flags &= ~MAP_GLOW;

            /* Mega-Hack -- Update the monster in the affected grid */
            /* This allows "spear of light" (etc) to work "correctly" */
            if (g_ptr->m_ptr) g_ptr->m_ptr->update();

            /* All done */
            break;
    }

    /* Return "Anything seen?" */
    return (obvious);
}



/*
 * We are called from "project()" to "damage" objects
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Perhaps we should only SOMETIMES damage things on the ground.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_i(CLiving *who, int r, int y, int x, int dam, int typ)
{
    CGrid *g_ptr = &cave[y][x];
    CItem *i_ptr = g_ptr->i_ptr;

    bool obvious = FALSE;
    bool is_art = FALSE;
    bool ignore = FALSE;
    bool plural = FALSE;
    bool do_kill = FALSE;

    char note_kill [500];

    u32b f1, f2, f3;

    char i_name[80];

    int div;

    strcpy(note_kill,"");
    /* Nothing here */
    if (!g_ptr->i_ptr) return FALSE;


    /* Extract radius */
    div = r + 1;

    /* Adjust damage */
    dam = dam / div;


    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);

    /* Get the "plural"-ness */
    if (i_ptr->isPlural()) plural = TRUE;

    /* Check for artifact */
    if (i_ptr->isArtifact()) is_art = TRUE;

    /* Analyze the type */
    switch (typ) {
        /* Acid -- Lots of things */
        case GF_ACID:
            if (i_ptr->hatesAcid()) {
                do_kill = TRUE;
                strcpy(note_kill , (plural ? " melt!" : " melts!"));
                if (f3 & TR3_IGNORE_ACID) ignore = TRUE;
            }
            break;

        /* Elec -- Rings and Wands */
        case GF_ELEC:
            if (i_ptr->hatesElec()) {
                do_kill = TRUE;
                strcpy(note_kill , (plural ? " is destroyed!" : " is destroyed!"));
                if (f3 & TR3_IGNORE_ELEC) ignore = TRUE;
            }
            break;

        /* Fire -- Flammable objects */
        case GF_FIRE:
            if (i_ptr->hatesFire()) {
                do_kill = TRUE;
                strcpy(note_kill , (plural ? " burn up!" : " burns up!"));
                if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            break;

        /* Cold -- potions and flasks */
        case GF_COLD:
            if (i_ptr->hatesCold()) {
                strcpy(note_kill , (plural ? " shatter!" : " shatters!"));
                do_kill = TRUE;
                if (f3 & TR3_IGNORE_COLD) ignore = TRUE;
            }
            break;

        /* Fire + Elec */
        case GF_PLASMA:
            if (i_ptr->hatesFire()) {
                do_kill = TRUE;
                strcpy(note_kill , (plural ? " burn up!" : " burns up!"));
                if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            if (i_ptr->hatesElec()) {
                ignore = FALSE;
                do_kill = TRUE;
                strcpy(note_kill, (plural ? " is destroyed!" : " is destroyed!"));
                if (f3 & TR3_IGNORE_ELEC) ignore = TRUE;
            }
            break;

        /* Fire + Cold */
        case GF_METEOR:
            if (i_ptr->hatesFire()) {
                do_kill = TRUE;
                strcpy(note_kill , (plural ? " burn up!" : " burns up!"));
                if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            if (i_ptr->hatesCold()) {
                ignore = FALSE;
                do_kill = TRUE;
                strcpy(note_kill, (plural ? " shatter!" : " shatters!"));
                if (f3 & TR3_IGNORE_COLD) ignore = TRUE;
            }
            break;

        /* Hack -- break potions and such */            
        case GF_ICE:
        case GF_SHARDS:
        case GF_FORCE:
        case GF_SOUND:
            if (i_ptr->hatesCold()) {
                strcpy(note_kill , (plural ? " shatter!" : " shatters!"));
                do_kill = TRUE;
            }
            break;

        /* Mana -- destroys everything */
        case GF_MANA:
            do_kill = TRUE;
            strcpy(note_kill , (plural ? " is destroyed!" : " is destroyed!"));

        /* Holy Orb -- destroys cursed non-artifacts */
        case GF_HOLY_ORB:
            if (i_ptr->isCursed()) {
                do_kill = TRUE;
                strcpy(note_kill, (plural ? " is destroyed!" : " is destroyed!"));
            }
            break;

        /* Unlock chests */
        case GF_KILL_TRAP:
        case GF_KILL_DOOR:      

            /* Chests are noticed only if trapped or locked */
            if (i_ptr->GetTval() == TV_CHEST) {
                /* Disarm/Unlock traps */
                if (i_ptr->GetPval() > 0) {
                    /* Disarm or Unlock */
                    i_ptr->SetPval(-i_ptr->GetPval());

                    /* Identify */
                    i_ptr->MakeKnown();

                    /* Notice */
                    if (i_ptr->GetMarked()) {
                        msg_print("Click!");
                        obvious = TRUE;
                    }
                }
            }

            break;
    }


    /* Attempt to destroy the object */
    if (do_kill) {
        /* Effect "observed" */
        if (i_ptr->GetMarked()) {
            obvious = TRUE;
            i_ptr->object_desc(i_name, FALSE, 0);
        }

        /* Artifacts, and other objects, get to resist */
        if (is_art || ignore) {
            /* Observe the resist */
            if (i_ptr->GetMarked()) {
                msg_format("The %s %s unaffected!",
                           i_name, (plural ? "are" : "is"));
            }
        }

        /* Kill it */
        else {
            /* Describe if needed */
            if (i_ptr->GetMarked() && note_kill) {
                msg_format("The %s%s", i_name, note_kill);
            }

            /* Delete the object */
            delete_object(i_ptr);
        }
    }

    /* Return "Anything seen?" */
    return (obvious);
}



/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a "radius" (see below),
 * which is used to decrease the power of explosions with distance, and a
 * location, via integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 * Note that "polymorph" is dangerous, since a failure in "place_monster()"'
 * may result in a dereference of an invalid pointer.  XXX XXX XXX
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
static bool project_m(CLiving *who, int r, int y, int x, int dam, int typ)
{
    int i, div;

    CGrid *g_ptr = &cave[y][x];

    CMonster *m_ptr = g_ptr->m_ptr;
    CMonsterRace *r_ptr;

    char *name;

    /* Is the monster "seen"? */
    bool seen;

    /* Were the "effects" obvious (if seen)? */
    bool obvious = FALSE;


    /* Polymorph setting (true or false) */
    int do_poly = FALSE;

    /* Teleport setting (max distance) */
    int do_dist = 0;

    /* Confusion setting (amount to confuse) */
    int do_conf = 0;

    /* Stunning setting (amount to stun) */
    int do_stun = 0;

    /* Sleep amount (amount to sleep) */
    int do_sleep = 0;

    /* Fear amount (amount to fear) */
    int do_fear = 0;


    /* Hold the monster name */
    char m_name[80];

    /* Assume no note */
    char *note = NULL;

    /* Assume a default death */
    char *note_dies = " dies.";

    /* Nobody here */
    if (!m_ptr) return FALSE;

    // Is the monster "seen"?
    seen = m_ptr->is_visible();

    // Get race and name
    r_ptr = m_ptr->get_r_ptr();
    name = r_name + r_ptr->name;

    /* Never affect projector */
    if (m_ptr == who) return FALSE;


    /* Extract radius */
    div = r + 1;

    /* Decrease damage */
    dam = dam / div;


    /* Mega-Hack */
    project_m_n++;
    project_m_x = x;
    project_m_y = y;


    /* Get the monster name (BEFORE polymorphing) */
    m_ptr->get_desc(m_name, 0);



    /* Some monsters get "destroyed" */
    if (r_ptr->isNonLiving() || (r_ptr->flags2 & RF2_STUPID)) {
        /* Special note at death */
        note_dies = " is destroyed.";
    }


    /* Analyze the damage type */
    switch (typ) {
        /* Magic Missile -- pure damage */
        case GF_MISSILE:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_RES_MAGIC) {
                note = " is unaffected!";
                dam = 0;
                if (seen) r_ptr->r_flags3 |= RF3_RES_MAGIC;
            }
            break;

        /* Acid */
        case GF_ACID:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_IM_ACID) {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_ACID;
            }
            break;

        /* Electricity */
        case GF_ELEC:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_IM_ELEC) {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_ELEC;
            }
            break;

        /* Fire damage */
        case GF_FIRE:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_HURT_FIRE) {
                // XXX no note
                dam *= 2;
                if (seen) r_ptr->r_flags3 |= RF3_HURT_FIRE;
            }
            if (r_ptr->flags3 & RF3_IM_FIRE) {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_FIRE;
            }
            break;

        /* Cold */
        case GF_COLD:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_HURT_COLD) {
                // XXX no note
                dam *= 2;
                if (seen) r_ptr->r_flags3 |= RF3_HURT_COLD;
            }
            if (r_ptr->flags3 & RF3_IM_COLD) {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_COLD;
            }
            break;

        /* Poison */
        case GF_POIS:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_IM_POIS)
            {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_POIS;
            }
            break;

        /* Holy Orb -- hurts evil, undead */
        case GF_HOLY_ORB:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_UNDEAD) {
                dam *= 3;
                note = " is hit very hard.";
                if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;
            }
            else if (r_ptr->flags3 & RF3_EVIL) {
                dam *= 2;
                note = " is hit hard.";
                if (seen) r_ptr->r_flags3 |= RF3_EVIL;
            }
            break;

        /* Arrow -- XXX no defense */
        case GF_ARROW:
            if (seen) obvious = TRUE;
            break;

        /* Plasma -- XXX perhaps check ELEC or FIRE */
        case GF_PLASMA:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_RES_PLASMA) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
                if (seen) r_ptr->r_flags3 |= RF3_RES_PLASMA;
            }
            break;

        /* Nether -- see above */
        case GF_NETHER:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_UNDEAD) {
                note = " is immune.";
                dam = 0;
                if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;
            }
            else if (r_ptr->flags4 & RF4_BR_NETH) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            else if (r_ptr->flags3 & RF3_EVIL) {
                dam /= 2;
                note = " resists somewhat.";
                if (seen) r_ptr->r_flags3 |= RF3_EVIL;
            }
            break;

        /* Water (acid) damage -- Water spirits/elementals are immune */
        case GF_WATER:
            if (seen) obvious = TRUE;
            if (r_ptr->flags3 & RF3_RES_WATER) {
                note = " is immune.";
                dam = 0;
                if (seen) r_ptr->r_flags3 |= RF3_RES_WATER;
            }
            break;

        /* Chaos -- Chaos breathers resist */
        case GF_CHAOS:
            if (seen) obvious = TRUE;
            do_conf = (5 + randint(11)) / div;
            if (r_ptr->flags4 & RF4_BR_CHAO) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            else if (percent(60)) {
                if (rand_int(3000) > r_ptr->level * r_ptr->level) {
                    do_poly = TRUE;
                }
            }
            break;

        /* Shards -- Shard breathers resist */
        case GF_SHARDS:
            if (seen) obvious = TRUE;
            if (r_ptr->flags4 & RF4_BR_SHAR) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Sound -- Sound breathers resist */
        case GF_SOUND:
            if (seen) obvious = TRUE;
            do_stun = (10 + randint(15)) / div;
            if (r_ptr->flags4 & RF4_BR_SOUN) {
                note = " resists.";
                dam *= 2; dam /= (randint(6)+6);
            }
            break;

        /* Confusion */
        case GF_CONFUSION:
            if (seen) obvious = TRUE;
            do_conf = (10 + randint(15)) / div;
            if (r_ptr->flags4 & RF4_BR_CONF) {
                note = " resists.";
                dam *= 2; dam /= (randint(6)+6);
            }
            else if (r_ptr->flags3 & RF3_NO_CONF) {
                note = " resists somewhat.";
                dam /= 2;
            }
            break;

        /* Disenchantment -- Breathers and Disenchanters resist */
        case GF_DISENCHANT:
            if (seen) obvious = TRUE;
            if ((r_ptr->flags4 & RF4_BR_DISE) ||
                prefix(name, "disen"))
            {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Nexus -- Breathers and Existers resist */
        case GF_NEXUS:
            if (seen) obvious = TRUE;
            if ((r_ptr->flags4 & RF4_BR_NEXU) || prefix(name, "nexus")) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Force */
        case GF_FORCE:
            if (seen) obvious = TRUE;
            do_stun = randint(15) / div;
            if (r_ptr->flags4 & RF4_BR_WALL) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Inertia -- breathers resist */
        case GF_INERTIA:
            if (seen) obvious = TRUE;
            if (r_ptr->flags4 & RF4_BR_INER) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Time -- breathers resist */
        case GF_TIME:
            if (seen) obvious = TRUE;
            if (r_ptr->flags4 & RF4_BR_TIME) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
            }
            break;

        /* Gravity -- breathers resist */
        case GF_GRAVITY:
            if (seen) obvious = TRUE;
            do_dist = 10;
            if (r_ptr->flags4 & RF4_BR_GRAV) {
                note = " resists.";
                dam *= 3; dam /= (randint(6)+6);
                do_dist = 0;
            }
            break;

        /* Pure damage */
        case GF_MANA:
            if (seen) obvious = TRUE;
            break;

        /* Meteor -- powerful magic missile */
        case GF_METEOR:
            if (seen) obvious = TRUE;
            break;

        /* Ice -- Cold + Cuts + Stun */
        case GF_ICE:
            if (seen) obvious = TRUE;
            do_stun = randint(15) / div;
            if (r_ptr->flags3 & RF3_IM_COLD) {
                note = " resists a lot.";
                dam /= 9;
                if (seen) r_ptr->r_flags3 |= RF3_IM_COLD;
            }
            break;


        /* Drain Life */
        case GF_OLD_DRAIN:
            if (seen) obvious = TRUE;
            if (r_ptr->isNonLiving()) {
                if (seen) {
                    if (r_ptr->flags3 & RF3_UNDEAD) r_ptr->r_flags3 |= RF3_UNDEAD;
                    if (r_ptr->flags3 & RF3_DEMON) r_ptr->r_flags3 |= RF3_DEMON;
                }

                note = " is unaffected!";
                obvious = FALSE;
                dam = 0;
            }

            break;

        /* Polymorph monster (Use "dam" as "power") */  
        case GF_OLD_POLY:

            if (seen) obvious = TRUE;

            /* Attempt to polymorph (see below) */
            do_poly = TRUE;

            /* Powerful monsters can resist */
            if ((r_ptr->flags1 & RF1_UNIQUE) || 
                (r_ptr->flags3 & RF3_RES_MAGIC) ||
                monster_saves(r_ptr->level, dam-10)) {
                if (r_ptr->flags3 & RF3_RES_MAGIC) {
                    if (seen) r_ptr->r_flags3 |= RF3_RES_MAGIC;
                }

                note = " is unaffected!";
                do_poly = FALSE;
                obvious = FALSE;
            }

            /* No "real" damage */
            dam = 0;    

            break;


        /* Clone monsters (Ignore "dam") */
        case GF_OLD_CLONE:

            if (seen) obvious = TRUE;

            /* Heal fully */
            m_ptr->SetCHP(m_ptr->GetMHP());

            /* Speed up */
            if (m_ptr->get_fast() < 30) {
                m_ptr->set_fast(30);
            }

            /* Attempt to clone. */
            if (multiply_monster(m_ptr)) {
                note = " spawns!";
            }

            /* No "real" damage */
            dam = 0;    

            break;


        /* Heal Monster (use "dam" as amount of healing) */
        case GF_OLD_HEAL:

            if (seen) obvious = TRUE;

            /* Wake up */
            m_ptr->set_csleep(0);

            /* Heal */
            m_ptr->SetCHP(m_ptr->GetCHP() + dam);

            /* No overflow */
            m_ptr->correct_hp_overflows();

            /* Message */
            note = " looks healthier.";

            /* No "real" damage */
            dam = 0;
            break;


        /* Speed Monster (Ignore "dam") */
        case GF_OLD_SPEED:

            if (seen) obvious = TRUE;

            /* Speed up */
            m_ptr->set_fast(m_ptr->get_fast() + 10 + randint(10));
            if (m_ptr->get_fast() > 200) m_ptr->set_fast(200);
            note = " starts moving faster.";

            /* No "real" damage */
            dam = 0;
            break;


        /* Slow Monster (Use "dam" as "power") */
        case GF_OLD_SLOW:

            if (seen) obvious = TRUE;

            /* Powerful monsters can resist */
            if ((r_ptr->flags1 & RF1_UNIQUE) || 
                (r_ptr->flags3 & RF3_RES_MAGIC) ||
                monster_saves(r_ptr->level, dam)) {
                if (r_ptr->flags3 & RF3_RES_MAGIC) {
                    if (seen) r_ptr->r_flags3 |= RF3_RES_MAGIC;
                }

                note = " is unaffected!";
                obvious = FALSE;
            }

            /* Normal monsters slow down */
            else {
                m_ptr->set_slow(m_ptr->get_slow() + 10 + randint(10));
                if (m_ptr->get_slow() > 200) m_ptr->set_slow(200);
                note = " starts moving slower.";
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Sleep (Use "dam" as "power") */
        case GF_OLD_SLEEP:

            if (seen) obvious = TRUE;

            /* Attempt a saving throw */
            if ((r_ptr->flags1 & RF1_UNIQUE) || 
                (r_ptr->flags3 & RF3_NO_SLEEP) || 
                (r_ptr->flags3 & RF3_RES_MAGIC) ||
                monster_saves(r_ptr->level, dam)) {
                /* Memorize a flag */
                if (r_ptr->flags3 & RF3_NO_SLEEP) {
                    if (seen) r_ptr->r_flags3 |= RF3_NO_SLEEP;
                }
                if (r_ptr->flags3 & RF3_RES_MAGIC) {
                    if (seen) r_ptr->r_flags3 |= RF3_RES_MAGIC;
                }

                /* No obvious effect */
                note = " is unaffected!";
                obvious = FALSE;
            }
            else {
                /* Go to sleep (much) later */
                note = " falls asleep!";
                do_sleep = 500;
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Confusion (Use "dam" as "power") */
        case GF_OLD_CONF:

            if (seen) obvious = TRUE;

            /* Get confused later */
            do_conf = damroll(3, (dam / 2)) + 1;

            /* Attempt a saving throw */
            if ((r_ptr->flags1 & RF1_UNIQUE) || 
                (r_ptr->flags3 & RF3_NO_CONF) || 
                (r_ptr->flags3 & RF3_RES_MAGIC) ||
                monster_saves(r_ptr->level, dam)) {
                /* Memorize a flag */
                if (r_ptr->flags3 & RF3_NO_CONF) {
                    if (seen) r_ptr->r_flags3 |= RF3_NO_CONF;
                }
                if (r_ptr->flags3 & RF3_RES_MAGIC) {
                    if (seen) r_ptr->r_flags3 |= RF3_RES_MAGIC;
                }

                /* Resist */
                do_conf = 0;

                /* No obvious effect */
                note = " is unaffected!";
                obvious = FALSE;
            }

            /* No "real" damage */
            dam = 0;
            break;



        /* Lite, but only hurts susceptible creatures */
        case GF_LITE_WEAK:

            /* Hurt by light */
            if (r_ptr->flags3 & RF3_HURT_LITE)
            {
                /* Obvious effect */
                if (seen) obvious = TRUE;

                /* Memorize the effects */
                if (seen) r_ptr->r_flags3 |= RF3_HURT_LITE;

                /* Special effect */
                note = " cringes from the light!";
                note_dies = " shrivels away in the light!";
            }

            /* Normally no damage */
            else
            {
                /* No damage */
                dam = 0;
            }

            break;



        /* Lite -- opposite of Dark */
        case GF_LITE:
            if (seen) obvious = TRUE;
            if (r_ptr->flags4 & RF4_BR_LITE)
            {
                note = " resists.";
                dam *= 2; dam /= (randint(6)+6);
            }
            else if (r_ptr->flags3 & RF3_HURT_LITE)
            {
                if (seen) r_ptr->r_flags3 |= RF3_HURT_LITE;
                note = " cringes from the light!";
                note_dies = " shrivels away in the light!";
                dam *= 2;
            }
            break;


        /* Dark -- opposite of Lite */
        case GF_DARK:
            if (seen) obvious = TRUE;
            if (r_ptr->flags4 & RF4_BR_DARK)
            {
                note = " resists.";
                dam *= 2; dam /= (randint(6)+6);
            }
            break;


        /* Stone to Mud */
        case GF_KILL_WALL:

            /* Hurt by rock remover */
            if (r_ptr->flags3 & RF3_HURT_ROCK)
            {
                /* Notice effect */
                if (seen) obvious = TRUE;

                /* Memorize the effects */
                if (seen) r_ptr->r_flags3 |= RF3_HURT_ROCK;

                /* Cute little message */
                note = " loses some skin!";
                note_dies = " dissolves!";
            }

            /* Usually, ignore the effects */
            else
            {
                /* No damage */
                dam = 0;
            }

            break;


        /* Teleport undead (Use "dam" as "power") */
        case GF_AWAY_UNDEAD:

            /* Only affect undead */
            if (r_ptr->flags3 & RF3_UNDEAD)
            {
                if (seen) obvious = TRUE;
                if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;
                do_dist = dam;
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Teleport evil (Use "dam" as "power") */
        case GF_AWAY_EVIL:

            /* Only affect undead */
            if (r_ptr->flags3 & RF3_EVIL)
            {
                if (seen) obvious = TRUE;
                if (seen) r_ptr->r_flags3 |= RF3_EVIL;
                do_dist = dam;
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Teleport monster (Use "dam" as "power") */
        case GF_AWAY_ALL:

            /* Obvious */
            if (seen) obvious = TRUE;

            /* Prepare to teleport */           
            do_dist = dam;

            /* No "real" damage */
            dam = 0;
            break;


        /* Turn undead (Use "dam" as "power") */
        case GF_TURN_UNDEAD:

            /* Only affect undead */
            if (r_ptr->flags3 & RF3_UNDEAD) {
                /* Learn about type */
                if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;

                /* Obvious */
                if (seen) obvious = TRUE;

                /* Apply some fear */
                do_fear = damroll(3, (dam / 2)) + 1;

                /* Attempt a saving throw */
                if (monster_saves(r_ptr->level, dam)) {
                    /* No obvious effect */
                    note = " is unaffected!";
                    obvious = FALSE;
                    do_fear = 0;
                }
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Turn evil (Use "dam" as "power") */
        case GF_TURN_EVIL:

            /* Only affect evil */
            if (r_ptr->flags3 & RF3_EVIL) {
                /* Learn about type */
                if (seen) r_ptr->r_flags3 |= RF3_EVIL;

                /* Obvious */
                if (seen) obvious = TRUE;

                /* Apply some fear */
                do_fear = damroll(3, (dam / 2)) + 1;

                /* Attempt a saving throw */
                if (monster_saves(r_ptr->level, dam)) {
                    /* No obvious effect */
                    note = " is unaffected!";
                    obvious = FALSE;
                    do_fear = 0;
                }
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Turn monster (Use "dam" as "power") */
        case GF_TURN_ALL:

            /* Obvious */
            if (seen) obvious = TRUE;

            /* Apply some fear */
            do_fear = damroll(3, (dam / 2)) + 1;

            /* Attempt a saving throw */
            if ((r_ptr->flags1 & RF1_UNIQUE) ||
                (r_ptr->flags3 & RF3_NO_FEAR) ||
                monster_saves(r_ptr->level, dam)) {
                /* No obvious effect */
                note = " is unaffected!";
                obvious = FALSE;
                do_fear = 0;
            }

            /* No "real" damage */
            dam = 0;
            break;


        /* Dispel undead */
        case GF_DISP_UNDEAD:

            /* Only affect undead */
            if (r_ptr->flags3 & RF3_UNDEAD) {
                /* Learn about type */
                if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;

                /* Obvious */
                if (seen) obvious = TRUE;

                /* Message */
                note = " shudders.";
                note_dies = " dissolves!";
            }

            /* Ignore other monsters */
            else {
                /* No damage */
                dam = 0;
            }

            break;


        /* Dispel evil */
        case GF_DISP_EVIL:

            /* Only affect evil */
            if (r_ptr->flags3 & RF3_EVIL)
            {
                /* Learn about type */
                if (seen) r_ptr->r_flags3 |= RF3_EVIL;

                /* Obvious */
                if (seen) obvious = TRUE;

                /* Message */
                note = " shudders.";
                note_dies = " dissolves!";
            }

            /* Ignore other monsters */
            else
            {
                /* No damage */
                dam = 0;
            }

            break;


        /* Dispel monster */
        case GF_DISP_ALL:

            /* Obvious */
            if (seen) obvious = TRUE;

            /* Message */
            note = " shudders.";
            note_dies = " dissolves!";

            break;
            

        /* Default */
        default:

            /* No damage */
            dam = 0;

            break;
    }



    /* "Unique" monsters cannot be polymorphed */
    if (r_ptr->flags1 & RF1_UNIQUE) do_poly = FALSE;


    /* "Unique" monsters can only be "killed" by the player */
    if (r_ptr->flags1 & RF1_UNIQUE) {
        /* Uniques may only be killed by the player */
        if ((who->GetTypeID() != TYPEID_CPLAYER) && (dam > m_ptr->GetCHP())) {
            dam = m_ptr->GetCHP();
        }
    }


    /* Check for death */
    if (dam > m_ptr->GetCHP()) {
        /* Extract method of death */
        note = note_dies;
    }

    /* Mega-Hack -- Handle "polymorph" */
    else if (do_poly) {
        /* Default -- assume no polymorph */
        note = " is unaffected!";

        /* Pick a "new" monster race */
        i = poly_r_idx(m_ptr->get_r_idx());

        /* Handle polymorh */
        if (i != m_ptr->get_r_idx()) {
            /* Obvious */
            if (seen) obvious = TRUE;

            /* Monster polymorphs */
            note = " changes!";

            /* Turn off the damage */
            dam = 0;

            /* "Kill" the "old" monster */
            delete_monster(m_ptr);

            /* Create a new monster (no groups, no sleep) */
            (void)place_monster_aux(y, x, i, 0);

            /* XXX XXX XXX Hack -- Assume success */

            /* Hack -- Get new monster */
            m_ptr = g_ptr->m_ptr;

            /* Hack -- Get new race */
            r_ptr = m_ptr->get_r_ptr();
        }
    }

    /* Handle "teleport" */
    else if (do_dist) {
        /* Obvious */
        if (seen) obvious = TRUE;

        /* Message */
        note = " disappears!";

        /* Teleport */
        teleport_away(m_ptr, do_dist);

        /* Hack -- get new location */
        x = m_ptr->GetX();
        y = m_ptr->GetY();

        /* Hack -- get new grid */
        g_ptr = &cave[y][x];
    }

    /* Sound and Impact breathers never stun */
    else if (do_stun &&
             !(r_ptr->flags4 & RF4_BR_SOUN) &&
             !(r_ptr->flags4 & RF4_BR_WALL))
    {
        /* Obvious */
        if (seen) obvious = TRUE;

        /* Get confused */
        if (m_ptr->get_stun()) {
            note = " is more dazed.";
            i = m_ptr->get_stun() + (do_stun / 2);
        }
        else {
            note = " is dazed.";
            i = do_stun;
        }

        /* Apply stun */
        m_ptr->set_stun((i < 200) ? i : 200);
    }

    /* Confusion and Chaos breathers (and sleepers) never confuse */
    else if (do_conf &&
            !(r_ptr->flags3 & RF3_NO_CONF) &&
            !(r_ptr->flags4 & RF4_BR_CONF) &&
            !(r_ptr->flags4 & RF4_BR_CHAO))
    {
        /* Obvious */
        if (seen) obvious = TRUE;

        /* Already partially confused */
        if (m_ptr->get_confused()) {
            note = " looks more confused.";
            i = m_ptr->get_confused() + (do_conf / 2);
        }

        /* Was not confused */
        else {
            note = " looks confused.";
            i = do_conf;
        }

        /* Apply confusion */
        m_ptr->set_confused((i < 200) ? i : 200);
    }


    /* Fear */
    if (do_fear) {
        /* Increase fear */
        i = m_ptr->get_afraid() + do_fear;

        /* Set fear */
        m_ptr->set_afraid((i < 200) ? i : 200);
    }


    /* If another monster did the damage, hurt the monster by hand */
    if (who->GetTypeID() != TYPEID_CPLAYER) {
        /* Wake the monster up */
        m_ptr->set_csleep(0);

        /* Hurt the monster */
        m_ptr->SetCHP(m_ptr->GetCHP() - dam);

        /* Dead monster */
        if (m_ptr->GetCHP() < 0) {
            /* Generate treasure, etc */
            m_ptr->monster_death();

            /* Delete the monster */
            delete_monster(m_ptr);

            /* Give detailed messages if destroyed */
            if (note) msg_format("%^s%s", m_name, note);
        }

        /* Damaged monster */
        else {
            /* Give detailed messages if visible or destroyed */
            if (note && seen) msg_format("%^s%s", m_name, note);

            /* Hack -- Pain message */
            else if (dam > 0) m_ptr->message_pain(dam);

            /* Hack -- handle sleep */
            if (do_sleep) m_ptr->set_csleep(do_sleep);
        }
    }

    /* If the player did it, give him experience, check fear */
    else {
        bool fear = FALSE;

        /* Hurt the monster, check for fear and death */
        if (m_ptr->mon_take_hit(dam, &fear, note_dies)) {
            /* Dead monster */
        }

        /* Damaged monster */
        else {
            /* Give detailed messages if visible or destroyed */
            if (note && seen) msg_format("%^s%s", m_name, note);

            /* Hack -- Pain message */
            else if (dam > 0) m_ptr->message_pain(dam);

            /* Take note */
            if ((fear || do_fear) && m_ptr->is_visible()) {
                //# Flee

                /* Message */
                msg_format("%^s flees in terror!", m_name);
            }

            /* Hack -- handle sleep */
            if (do_sleep) m_ptr->set_csleep(do_sleep);
        }
    }


    /* Update the monster XXX XXX XXX */
    m_ptr->update();


    /* Return "Anything seen?" */
    return (obvious);
}






/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 */
static bool project_p(CLiving *who, int r, int y, int x, int dam, int typ)
{
    int k = 0, div;

    /* Hack -- assume obvious */
    bool obvious = TRUE;

    /* Player blind-ness */
    bool blind = (p_ptr->GetBlind() ? TRUE : FALSE);

    /* Player needs a "description" (he is blind) */
    bool fuzzy = FALSE;

    /* Monster name (for attacks) */
    char m_name[80];

    /* Monster name (for damage) */
    char killer[80];

    /* Hack -- messages */
    char *act = NULL;


    // Player is not here
    if (!p_ptr->is_at(x, y)) return FALSE;

    // Player cannot hurt himself
    if (p_ptr == who) return FALSE;


    /* Extract radius */
    div = r + 1;

    /* Decrease damage */
    dam = dam / div;


    /* Hack -- always do at least one point of damage */
    if (dam <= 0) dam = 1;

    /* Hack -- Never do excessive damage */
    if (dam > 1600) dam = 1600;


    /* If the player is blind, be more descriptive */
    if (blind) fuzzy = TRUE;


    // Get the source's apparent name
    who->get_desc(m_name, 0);

    // Get the source's real name
    who->get_desc(killer, 0x88);


    /* Analyze the damage */
    switch (typ) {
        /* Standard damage -- hurts inventory too */
        case GF_ACID:
            if (fuzzy) msg_print("You are hit by acid!");
            acid_dam(dam, killer);
            break;

        /* Standard damage -- hurts inventory too */
        case GF_FIRE:
            if (fuzzy) msg_print("You are hit by fire!");
            fire_dam(dam, killer);
            break;

        /* Standard damage -- hurts inventory too */
        case GF_COLD:
            if (fuzzy) msg_print("You are hit by cold!");
            cold_dam(dam, killer);
            break;

        /* Standard damage -- hurts inventory too */
        case GF_ELEC:
            if (fuzzy) msg_print("You are hit by lightning!");
            elec_dam(dam, killer);
            break;

        /* Standard damage -- also poisons player */
        case GF_POIS:
            if (fuzzy) msg_print("You are hit by poison!");
            if (p_ptr->get_resists(RESIST_POIS)) dam = (dam + 2) / 3;
            if (p_ptr->GetOpposePois()) dam = (dam + 2) / 3;
            p_ptr->take_hit(dam, killer);
            if (!p_ptr->get_resists(RESIST_POIS) && !p_ptr->GetOpposePois()) {
                p_ptr->mod_poisoned(p_ptr->GetPoisoned() + rand_int(dam) + 10);
            }
            break;

        /* Standard damage */
        case GF_MISSILE:
            if (fuzzy) msg_print("You are hit by something!");
            p_ptr->take_hit(dam, killer);
            break;

        /* Holy Orb -- Player only takes partial damage */
        case GF_HOLY_ORB:
            if (fuzzy) msg_print("You are hit by something!");
            dam /= 2;
            p_ptr->take_hit(dam, killer);
            break;

        /* Arrow -- XXX no dodging */
        case GF_ARROW:
            if (fuzzy) msg_print("You are hit by something sharp!");
            p_ptr->take_hit(dam, killer);
            break;

        /* Plasma -- XXX No resist */
        case GF_PLASMA:
            if (fuzzy) msg_print("You are hit by something!");
            p_ptr->take_hit(dam, killer);
            if (!p_ptr->get_resists(RESIST_SOUND) && !p_ptr->GetShadowform()) {
                int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
                p_ptr->mod_stun(p_ptr->GetStun() + k);
            }
            break;

        /* Nether -- drain experience */
        case GF_NETHER:
            if (fuzzy) msg_print("You are hit by something strange!");
            if (p_ptr->get_resists(RESIST_NETH)) {
                dam *= 6; dam /= (randint(6) + 6);
            }
            else {
                if (p_ptr->get_hold_life() && percent(75)) {
                    msg_print("You keep hold of your life force!");
                }
                else if (p_ptr->get_hold_life()) {
                    msg_print("You feel your life slipping away!");
                    p_ptr->lose_exp(200 + (p_ptr->GetExp()/1000) * MON_DRAIN_LIFE);
                }
                else {
                    msg_print("You feel your life draining away!");
                    p_ptr->lose_exp(200 + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE);
                }
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Water -- stun/confuse */
        case GF_WATER:
            if (fuzzy) msg_print("You are hit by something!");
            if (!p_ptr->get_resists(RESIST_SOUND) && !p_ptr->GetShadowform()) {
                p_ptr->mod_stun(p_ptr->GetStun() + randint(40));
            }
            if (!p_ptr->get_resists(RESIST_CONF)) {
                p_ptr->mod_confused(p_ptr->GetConfused() + randint(5) + 5);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Chaos -- many effects */
        case GF_CHAOS:
            if (fuzzy) msg_print("You are hit by something strange!");
            if (p_ptr->get_resists(RESIST_CHAOS)) {
                dam *= 6; dam /= (randint(6) + 6);
            }
            if (!p_ptr->get_resists(RESIST_CONF)) {
                p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(20) + 10);
            }
            if (!p_ptr->get_resists(RESIST_NETH) && !p_ptr->get_resists(RESIST_CHAOS)) {
                if (p_ptr->get_hold_life() && percent(75)) {
                    msg_print("You keep hold of your life force!");
                }
                else if (p_ptr->get_hold_life()) {
                    msg_print("You feel your life slipping away!");
                    p_ptr->lose_exp(500 + (p_ptr->GetExp()/1000) * MON_DRAIN_LIFE);
                }
                else {
                    msg_print("You feel your life draining away!");
                    p_ptr->lose_exp(5000 + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE);
                }
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Shards -- mostly cutting */
        case GF_SHARDS:
            if (fuzzy) msg_print("You are hit by something sharp!");
            if (p_ptr->get_resists(RESIST_SHARD)) {
                dam *= 6; dam /= (randint(6) + 6);
            }
            else if (!p_ptr->GetShadowform()) {
                p_ptr->mod_cut(p_ptr->GetCut() + dam);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Sound -- mostly stunning */
        case GF_SOUND:
            if (fuzzy) msg_print("You are hit by something!");
            if (p_ptr->get_resists(RESIST_SOUND)) {
                dam *= 5; dam /= (randint(6) + 6);
            }
            else if (!p_ptr->GetShadowform()) {
                int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
                p_ptr->mod_stun(p_ptr->GetStun() + k);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Pure confusion */
        case GF_CONFUSION:
            if (fuzzy) msg_print("You are hit by something!");
            if (p_ptr->get_resists(RESIST_CONF)) {
                dam *= 5; dam /= (randint(6) + 6);
            }
            if (!p_ptr->get_resists(RESIST_CONF)) {
                p_ptr->mod_confused(p_ptr->GetConfused() + randint(20) + 10);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Disenchantment -- see above */
        case GF_DISENCHANT:
            if (fuzzy) msg_print("You are hit by something strange!");
            if (p_ptr->get_resists(RESIST_DISEN)) {
                dam *= 6; dam /= (randint(6) + 6);
            }
            else {
                apply_disenchant(0);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Nexus -- see above */
        case GF_NEXUS:
            if (fuzzy) msg_print("You are hit by something strange!");
            if (p_ptr->get_resists(RESIST_NEXUS)) {
                dam *= 6; dam /= (randint(6) + 6);
            }
            else {
                apply_nexus(who);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Force -- mostly stun */
        case GF_FORCE:
            if (fuzzy) msg_print("You are hit by something!");
            if (!p_ptr->get_resists(RESIST_SOUND) && !p_ptr->GetShadowform()) {
                p_ptr->mod_stun(p_ptr->GetStun() + randint(20));
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Inertia -- slowness */
        case GF_INERTIA:
            if (fuzzy) msg_print("You are hit by something strange!");
            p_ptr->mod_slow(p_ptr->GetSlow() + rand_int(4) + 4);
            p_ptr->take_hit(dam, killer);
            break;

        /* Lite -- blinding */
        case GF_LITE:
            if (fuzzy) msg_print("You are hit by something!");
            if (p_ptr->get_resists(RESIST_LITE)) {
                dam *= 4; dam /= (randint(6) + 6);
            }
            else if (!blind && !p_ptr->get_resists(RESIST_BLIND)) {
                p_ptr->mod_blind(p_ptr->GetBlind() + randint(5) + 2);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Dark -- blinding */
        case GF_DARK:
            if (fuzzy) msg_print("You are hit by something!");
            if (p_ptr->get_resists(RESIST_DARK)) {
                dam *= 4; dam /= (randint(6) + 6);
            }
            else if (!blind && !p_ptr->get_resists(RESIST_BLIND)) {
                p_ptr->mod_blind(p_ptr->GetBlind() + randint(5) + 2);
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Time -- bolt fewer effects XXX */
        case GF_TIME:
            if (fuzzy) msg_print("You are hit by something strange!");
            switch (randint(10)) {
                case 1: case 2: case 3: case 4: case 5:
                    msg_print("You feel life has clocked back.");
                    p_ptr->lose_exp(100 + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE);
                    break;

                case 6: case 7: case 8: case 9:

                    switch (randint(6)) {
                        case 1: k = STAT_STR; act = "strong"; break;
                        case 2: k = STAT_INT; act = "bright"; break;
                        case 3: k = STAT_WIS; act = "wise"; break;
                        case 4: k = STAT_DEX; act = "agile"; break;
                        case 5: k = STAT_CON; act = "hale"; break;
                        case 6: k = STAT_CHR; act = "beautiful"; break;
                    }

                    msg_format("You're not as %s as you used to be...", act);

                    p_ptr->SetStatCur(k, (p_ptr->GetStatCur(k) * 3) / 4);
                    if (p_ptr->GetStatCur(k) < 3) p_ptr->SetStatCur(k, 3);
                    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);
                    break;

                case 10:
                    msg_print("You're not as powerful as you used to be...");

                    for (k = 0; k < 6; k++) {
                        p_ptr->SetStatCur(k, (p_ptr->GetStatCur(k) * 3) / 4);
                        if (p_ptr->GetStatCur(k) < 3) p_ptr->SetStatCur(k, 3);
                    }
                    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);
                    break;
            }
            p_ptr->take_hit(dam, killer);
            break;

        /* Gravity -- slowness plus teleport */
        case GF_GRAVITY:
            if (fuzzy) msg_print("You are hit by something strange!");
            msg_print("Gravity warps around you.");
            teleport_player(5);
            p_ptr->mod_slow(p_ptr->GetSlow() + rand_int(4) + 4);
            p_ptr->take_hit(dam, killer);
            break;

        /* Pure damage */
        case GF_MANA:
            if (fuzzy) msg_print("You are hit by something!");
            p_ptr->take_hit(dam, killer);
            break;

        /* Pure damage */
        case GF_METEOR:
            if (fuzzy) msg_print("You are hit by something!");
            p_ptr->take_hit(dam, killer);
            break;

        /* Ice -- cold plus stun plus cuts */
        case GF_ICE:
            if (fuzzy) msg_print("You are hit by something sharp!");
            cold_dam(dam, killer);
            if (!p_ptr->get_resists(RESIST_SHARD) && !p_ptr->GetShadowform()) {
                p_ptr->mod_cut(p_ptr->GetCut() + damroll(5, 8));
            }
            if (!p_ptr->get_resists(RESIST_SOUND) && !p_ptr->GetShadowform()) {
                p_ptr->mod_stun(p_ptr->GetStun() + randint(15));
            }
            break;


        /* Default */
        default:

            /* No damage */
            dam = 0;

            break;
    }


    /* Return "Anything seen?" */
    return (obvious);
}









/*
 * Generic "beam"/"bolt"/"ball" projection routine.  -BEN-
 *
 * Input:
 *   who: Index of "source" monster (or "zero" for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   y,x: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
 * Allows a monster (or player) to project a beam/bolt/ball of a given kind
 * towards a given location (optionally passing over the heads of interposing
 * monsters), and have it do a given amount of damage to the monsters (and
 * optionally objects) within the given radius of the final location.
 *
 * A "bolt" travels from source to target and affects only the target grid.
 * A "beam" travels from source to target, affecting all grids passed through.
 * A "ball" travels from source to the target, exploding at the target, and
 *   affecting everything within the given radius of the target location.
 *
 * Traditionally, a "bolt" does not affect anything on the ground, and does
 * not pass over the heads of interposing monsters, much like a traditional
 * missile, and will "stop" abruptly at the "target" even if no monster is
 * positioned there, while a "ball", on the other hand, passes over the heads
 * of monsters between the source and target, and affects everything except
 * the source monster which lies within the final radius, while a "beam"
 * affects every monster between the source and target, except for the casting
 * monster (or player), and rarely affects things on the ground.
 *
 * Two special flags allow us to use this function in special ways, the
 * "PROJECT_HIDE" flag allows us to perform "invisible" projections, while
 * the "PROJECT_JUMP" flag allows us to affect a specific grid, without
 * actually projecting from the source monster (or player).
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * "radius" of standard ball attacks to nine units (diameter nineteen).
 *
 * One can project in a given "direction" by combining PROJECT_THRU with small
 * offsets to the initial location (see "line_spell()"), or by calculating
 * "virtual targets" far away from the player.
 *
 * One can also use PROJECT_THRU to send a beam/bolt along an angled path,
 * continuing until it actually hits somethings (useful for "stone to mud").
 *
 * Bolts and Beams explode INSIDE walls, so that they can destroy doors.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters
 * on both sides of a wall.  Some bug reports indicate that this is still
 * happening in 2.7.8 for Windows, though it appears to be impossible.
 *
 * We "pre-calculate" the blast area only in part for efficiency.
 * More importantly, this lets us do "explosions" from the "inside" out.
 * This results in a more logical distribution of "blast" treasure.
 * It also produces a better (in my opinion) animation of the explosion.
 * It could be (but is not) used to have the treasure dropped by monsters
 * in the middle of the explosion fall "outwards", and then be damaged by
 * the blast as it spreads outwards towards the treasure drop location.
 *
 * Walls and doors are included in the blast area, so that they can be
 * "burned" or "melted" in later versions.
 *
 * This algorithm is intended to maximize simplicity, not necessarily
 * efficiency, since this function is not a bottleneck in the code.
 *
 * We apply the blast effect from ground zero outwards, in several passes,
 * first affecting features, then objects, then monsters, then the player.
 * This allows walls to be removed before checking the object or monster
 * in the wall, and protects objects which are dropped by monsters killed
 * in the blast, and allows the player to see all affects before he is
 * killed or teleported away.  The semantics of this method are open to
 * various interpretations, but they seem to work well in practice.
 *
 * We process the blast area from ground-zero outwards to allow for better
 * distribution of treasure dropped by monsters, and because it provides a
 * pleasing visual effect at low cost.
 *
 * Note that the damage done by "ball" explosions decreases with distance.
 * This decrease is rapid, grids at radius "dist" take "1/dist" damage.
 *
 * Notice the "napalm" effect of "beam" weapons.  First they "project" to
 * the target, and then the damage "flows" along this beam of destruction.
 * The damage at every grid is the same as at the "center" of a "ball"
 * explosion, since the "beam" grids are treated as if they ARE at the
 * center of a "ball" explosion.
 *
 * Currently, specifying "beam" plus "ball" means that locations which are
 * covered by the initial "beam", and also covered by the final "ball", except
 * for the final grid (the epicenter of the ball), will be "hit twice", once
 * by the initial beam, and once by the exploding ball.  For the grid right
 * next to the epicenter, this results in 150% damage being done.  The center
 * does not have this problem, for the same reason the final grid in a "beam"
 * plus "bolt" does not -- it is explicitly removed.  Simply removing "beam"
 * grids which are covered by the "ball" will NOT work, as then they will
 * receive LESS damage than they should.  Do not combine "beam" with "ball".
 *
 * The array "gy[],gx[]" with current size "grids" is used to hold the
 * collected locations of all grids in the "blast area" plus "beam path".
 *
 * Note the rather complex usage of the "gm[]" array.  First, gm[0] is always
 * zero.  Second, for N>1, gm[N] is always the index (in gy[],gx[]) of the
 * first blast grid (see above) with radius "N" from the blast center.  Note
 * that only the first gm[1] grids in the blast area thus take full damage.
 * Also, note that gm[rad+1] is always equal to "grids", which is the total
 * number of blast grids.
 *
 * Note that once the projection is complete, (y2,x2) holds the final location
 * of bolts/beams, and the "epicenter" of balls.
 *
 * Note also that "rad" specifies the "inclusive" radius of projection blast,
 * so that a "rad" of "one" actually covers 5 or 9 grids, depending on the
 * implementation of the "distance" function.  Also, a bolt can be properly
 * viewed as a "ball" with a "rad" of "zero".
 *
 * Note that if no "target" is reached before the beam/bolt/ball travels the
 * maximum distance allowed (MAX_RANGE), no "blast" will be induced.  This
 * may be relevant even for bolts, since they have a "1x1" mini-blast.
 *
 * Some people have requested an "auto-explode ball attacks at max range"
 * option, which should probably be handled by this function.  XXX XXX XXX
 *
 * Note that for consistency, we "pretend" that the bolt actually takes "time"
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 */
bool project(CLiving *who, int rad, int y, int x, int dam, int typ, int flg)
{
    int i, t;
    int y1, x1, y2, x2;
    int y0, x0, y9, x9;
    int dist;

    /* Affected location(s) */
    CGrid *g_ptr;

    /* Assume the player sees nothing */
    bool notice = FALSE;

    /* Assume the player has seen no blast grids */
    bool drawn = FALSE;

    /* Is the player blind? */
    bool blind = (p_ptr->GetBlind() ? TRUE : FALSE);

    /* Number of grids in the "blast area" (including the "beam" path) */
    int grids = 0;

    /* Coordinates of the affected grids */
    byte gx[256], gy[256];

    /* Encoded "radius" info (see above) */
    byte gm[16];


    /* Location of player */
    x0 = p_ptr->GetX();
    y0 = p_ptr->GetY();


    /* Hack -- Jump to target */
    if (flg & PROJECT_JUMP) {
        x1 = x;
        y1 = y;
    }

    // Start at source entity
    else {
        x1 = who->GetX();
        y1 = who->GetY();
    }


    /* Default "destination" */
    y2 = y; x2 = x;


    /* Hack -- verify stuff */
    if (flg & PROJECT_THRU) {
        if ((x1 == x2) && (y1 == y2)) {
            flg &= ~PROJECT_THRU;
        }
    }


    /* Hack -- Assume there will be no blast (max radius 16) */
    for (dist = 0; dist < 16; dist++) gm[dist] = 0;


    // Hack -- Update stuff
    update_stuff();


    // Start at the source
    x = x9 = x1;
    y = y9 = y1;
    dist = 0;

    /* Project until done */
    for (;;) {
        /* Gather beam grids */
        if (flg & PROJECT_BEAM) {
            gy[grids] = y;
            gx[grids] = x;
            grids++;
        }

        /* XXX XXX Hack -- Display "beam" grids */
        //: Redo
//        if (!blind && !(flg & PROJECT_HIDE) &&
//            dist && (flg & PROJECT_BEAM) &&
//            panel_contains(y, x) && player_has_los_bold(y, x))
//        {
//            /* Hack -- Visual effect -- "explode" the grids */
//            print_rel('*', spell_color(typ), y, x);
//        }

        /* Check the grid */
        g_ptr = &cave[y][x];

        /* Never pass through walls */
        if (dist && !floor_grid_bold(y, x)) break;

        /* Check for arrival at "final target" (if desired) */
        if (!(flg & PROJECT_THRU) && (x == x2) && (y == y2)) break;

        /* If allowed, and we have moved at all, stop when we hit anybody */
        if (g_ptr->m_ptr && dist && (flg & PROJECT_STOP)) break;


        /* Calculate the new location */
        y9 = y;
        x9 = x;
        mmove2(&y9, &x9, y1, x1, y2, x2);

        /* Hack -- Balls explode BEFORE reaching walls or doors */
        if (!floor_grid_bold(y9, x9) && (rad > 0)) break;

        /* Keep track of the distance traveled */
        dist++;

        /* Nothing can travel furthur than the maximal distance */
        if (dist > MAX_RANGE) break;

        /* Only do visual effects (and delay) if requested */
        if (!blind && !(flg & PROJECT_HIDE)) {
            //: Display the bolt at x9, y9, and delay
        }


        /* Save the new location */
        y = y9;
        x = x9;
    }


    /* Save the "blast epicenter" */
    y2 = y;
    x2 = x;

    /* Start the "explosion" */
    gm[0] = 0;

    /* Hack -- make sure beams get to "explode" */
    gm[1] = grids;

    /* If we found a "target", explode there */
    if (dist <= MAX_RANGE) {
        /* Mega-Hack -- remove the final "beam" grid */
        if ((flg & PROJECT_BEAM) && (grids > 0)) grids--;

        /* Determine the blast area, work from the inside out */
        for (dist = 0; dist <= rad; dist++) {
            /* Scan the maximal blast area of radius "dist" */
            for (y = y2 - dist; y <= y2 + dist; y++) {
                for (x = x2 - dist; x <= x2 + dist; x++) {
                    /* Ignore "illegal" locations */
                    if (!in_bounds2(y, x)) continue;

                    /* Enforce a "circular" explosion */
                    if (distance(x2, y2, x, y) != dist) continue;

                    /* Ball explosions are stopped by walls */
                    if (!los(y2, x2, y, x)) continue;

                    /* Save this grid */
                    gy[grids] = y;
                    gx[grids] = x;
                    grids++;
                }
            }

            /* Encode some more "radius" info */
            gm[dist+1] = grids;
        }
    }


    /* Speed -- ignore "non-explosions" */
    if (!grids) return (FALSE);


    /* Display the "blast area" */
    if (!blind && !(flg & PROJECT_HIDE)) {
        /* Then do the "blast", from inside out */
        for (t = 0; t <= rad; t++) {
            /* Dump everything with this radius */
            for (i = gm[t]; i < gm[t+1]; i++) {
                /* Extract the location */
                y = gy[i];
                x = gx[i];

                /* The player can see it */
                //: XXX
//                if (player_has_los_bold(y, x) && panel_contains(y, x)) {
//                    drawn = TRUE;
//                    print_rel('*', spell_color(typ), y, x);
//                }
            }

            /* Flush each "radius" seperately */
            screen_refresh();

            /* Delay (efficiently) */
//            if (visual || drawn) delay(10 * delay_spd);
        }

        /* Flush the erasing */
        if (drawn) {
            /* Erase the explosion drawn above */
            for (i = 0; i < grids; i++) {
                /* Extract the location */
                y = gy[i];
                x = gx[i];

                /* Erase if needed */
                //: XXX
//                if (player_has_los_bold(y, x) && panel_contains(y, x)) {
//                    lite_spot(y, x);
//                }
            }

            /* Flush the explosion */
            screen_refresh();
        }
    }


    /* Check features */
    if (flg & PROJECT_GRID) {
        /* Start with "dist" of zero */
        dist = 0;

        /* Now hurt the cave grids (and objects) from the inside out */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist+1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the feature */
            if (project_f(who, dist, y, x, dam, typ)) notice = TRUE;
        }
    }


    /* Check objects */
    if (flg & PROJECT_ITEM) {
        /* Start with "dist" of zero */
        dist = 0;

        /* Now hurt the cave grids (and objects) from the inside out */
        for (i = 0; i < grids; i++)
        {
            /* Hack -- Notice new "dist" values */
            if (gm[dist+1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the object */
            if (project_i(who, dist, y, x, dam, typ)) notice = TRUE;
        }
    }


    /* Check monsters */
    if (flg & PROJECT_KILL) {
        /* Start with "dist" of zero */
        dist = 0;

        /* Mega-Hack */
        project_m_n = 0;
        project_m_x = 0;
        project_m_y = 0;

        /* Now hurt the monsters, from inside out */
        for (i = 0; i < grids; i++) {
            /* Hack -- Notice new "dist" values */
            if (gm[dist+1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Walls protect monsters */
            if (!floor_grid_bold(y,x)) continue;

            /* Affect the monster */
            if (project_m(who, dist, y, x, dam, typ)) notice = TRUE;
        }
    }


    /* Check player */
    if (flg & PROJECT_KILL) {
        /* Start with "dist" of zero */
        dist = 0;

        /* Now see if the player gets hurt */
        for (i = 0; i < grids; i++) {
            /* Hack -- Notice new "dist" values */
            if (gm[dist+1] == i) dist++;

            /* Get the grid location */
            y = gy[i];
            x = gx[i];

            /* Affect the player */
            if (project_p(who, dist, y, x, dam, typ)) notice = TRUE;
        }
    }


    /* Return "something was noticed" */
    return (notice);
}

