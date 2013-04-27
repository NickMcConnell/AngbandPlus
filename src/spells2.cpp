// File: spells2.c
// Purpose: Spell code (part 2)

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"




/*
 * Increase players hit points, notice effects
 */
bool CPlayer::heal_up(int num)
{
    if (p_ptr->GetCHP() < p_ptr->GetMHP()) {
        int pct;

        p_ptr->SetCHP(p_ptr->GetCHP() + num);
        p_ptr->correct_hp_overflows();

        pct = num * 100 / p_ptr->GetMHP();

        if (pct < 10) {
            msg_print("You feel a little better.");
        }
        else if (pct < 30) {
            msg_print("You feel somewhat better.");
        }
        else if (pct < 70) {
            msg_print("You feel quite a bit better.");
        }
        else {
            msg_print("You feel a whole lot better.");
        }

        return TRUE;
    }

    return FALSE;
}



/*
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
    CGrid *g_ptr = p_ptr->get_g_ptr();

    // Require clean space
    if (!clean_grid_bold(p_ptr->GetY(), p_ptr->GetX())) return;

    /* Create a glyph of warding */
    g_ptr->set_feat(CF_GLYPH);
}




/*
 * Array of stat "descriptions"
 */
static char *desc_stat_pos[] = {
    "strong",
    "smart",
    "wise",
    "dextrous",
    "healthy",
    "cute"
};


/*
 * Array of stat "descriptions"
 */
static char *desc_stat_neg[] = {
    "weak",
    "stupid",
    "naive",
    "clumsy",
    "sickly",
    "ugly"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
    /* Sustain */
    if (p_ptr->get_sustains(stat)) {
        /* Message */
        msg_format("You feel %s for a moment, but the feeling passes.",
            desc_stat_neg[stat]);

        /* Notice effect */
        return TRUE;
    }

    /* Attempt to reduce the stat */
    if (dec_stat(stat, 10, FALSE)) {
        /* Message */
        msg_format("You feel very %s.", desc_stat_neg[stat]);

        /* Notice effect */
        return TRUE;
    }

    /* Nothing obvious */
    return FALSE;
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
    /* Attempt to increase */
    if (res_stat(stat)) {
        /* Message */
        msg_format("You feel less %s.", desc_stat_neg[stat]);

        /* Notice */
        return TRUE;
    }

    /* Nothing obvious */
    return FALSE;
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
    bool res;

    /* Restore strength */
    res = res_stat(stat);

    /* Attempt to increase */
    if (inc_stat(stat)) {
        /* Message */
        msg_format("Wow!  You feel very %s!", desc_stat_pos[stat]);

        /* Notice */
        return TRUE;
    }

    /* Restoration worked */
    if (res) {
        /* Message */
        msg_format("You feel less %s.", desc_stat_neg[stat]);

        /* Notice */
        return TRUE;
    }

    /* Nothing obvious */
    return FALSE;
}



/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
    int i;
    CItem *i_ptr;

    /* Simply identify and know every item */
    for (i = 0; i < INVEN_TOTAL; i++) {
        i_ptr = &inventory[i];
        if (i_ptr->exists()) {
            i_ptr->object_aware();
            i_ptr->MakeKnown();
        }
    }
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] = {
    0, 10, 50, 100, 200,
    300, 400, 500, 700, 950,
    990, 992, 995, 997, 999,
    1000
};

static int enchant_table_dam[20] = {
    0, 10, 50, 100, 100,
    100, 200, 300, 400, 500,
    600, 700, 700, 700, 800,
    800, 850, 900, 950, 975,
};


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
    int         i, cnt = 0;

    /* Attempt to uncurse items being worn */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
        u32b f1, f2, f3;

        CItem *i_ptr = &inventory[i];

        // Uncursed already
        if (!i_ptr->isCursed()) continue;

        // Extract the flags
        i_ptr->GetFlags(&f1, &f2, &f3);

        // Heavily Cursed Items need a special spell
        if (!all && (f3 & TR3_HEAVY_CURSE)) continue;

        // Perma-Cursed Items can NEVER be uncursed
        if (f3 & TR3_PERMA_CURSE) continue;

        // Uncurse it
        i_ptr->ClearIdentFlag(ID_CURSED);

        // Hack -- Assume felt
        i_ptr->SetIdentFlag(ID_SENSE);

        // Take note
        i_ptr->SetNote("uncursed");

        // Recalculate the bonuses
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

        /* Count the uncursings */
        cnt++;
    }

    /* Return "something uncursed" */
    return cnt;
}


/*
 * Remove most curses
 */
bool remove_curse()
{
    return remove_curse_aux(FALSE);
}

/*
 * Remove all curses
 */
bool remove_all_curse()
{
    return remove_curse_aux(TRUE);
}



/*
 * Restores any drained experience
 */
bool CPlayer::restore_level()
{
    /* Restore experience */
    if (GetExp() < GetMaxExp()) {
        /* Message */
        msg_print("You feel your life energies returning.");

        /* Restore the experience */
        SetExp(GetMaxExp());

        /* Check the experience */
        check_experience();

        /* Did something */
        return TRUE;
    }

    /* No effect */
    return FALSE;
}


/*
 * self-knowledge... idea from nethack.  Useful for determining powers and
 * resistences of items.  It saves the screen, clears it, then starts listing
 * attributes, a screenful at a time.  (There are a LOT of attributes to
 * list.  It will probably take 2 or 3 screens for a powerful character who is
 * using several artifacts...) -CFT
 *
 * It is now a lot more efficient. -BEN-
 *
 * See also "identify_fully()".
 *
 * XXX XXX XXX Use the "show_file()" method, perhaps.
 */
void self_knowledge()
{
    int i = 0, j, k;
    u32b f1 = 0L, f2 = 0L, f3 = 0L;
    CItem *i_ptr;
    char *info[128];


    /* Acquire item flags from equipment */
    for (k = INVEN_WIELD; k < INVEN_TOTAL; k++) {
        u32b t1, t2, t3;

        i_ptr = &inventory[k];

        /* Skip empty items */
        if (!i_ptr->exists()) continue;

        /* Extract the flags */
        i_ptr->GetFlags(&t1, &t2, &t3);

        /* Extract flags */
        f1 |= t1;
        f2 |= t2;
        f3 |= t3;
    }


    if (p_ptr->GetBlind()) {
        info[i++] = "You cannot see.";
    }
    if (p_ptr->GetConfused()) {
        info[i++] = "You are confused.";
    }
    if (p_ptr->GetAfraid()) {
        info[i++] = "You are terrified.";
    }
    if (p_ptr->GetCut()) {
        info[i++] = "You are bleeding.";
    }
    if (p_ptr->GetStun()) {
        info[i++] = "You are stunned.";
    }
    if (p_ptr->GetPoisoned()) {
        info[i++] = "You are poisoned.";
    }

    if (p_ptr->get_aggravate()) {
        info[i++] = "You aggravate monsters.";
    }
    if (p_ptr->get_teleport()) {
        info[i++] = "Your position is very uncertain.";
    }

    if (p_ptr->GetBlessed()) {
        info[i++] = "You feel rightous.";
    }
    if (p_ptr->GetHero()) {
        info[i++] = "You feel heroic.";
    }
    if (p_ptr->GetSHero()) {
        info[i++] = "You are in a battle rage.";
    }
    if (p_ptr->GetProtevil()) {
        info[i++] = "You are protected from evil.";
    }
    if (p_ptr->GetShield()) {
        info[i++] = "You are protected by a mystic shield.";
    }
    if (p_ptr->GetShadowform()) {
        info[i++] = "You exist as merely a shade to your foes.";
    }
    if (p_ptr->GetConfusing()) {
        info[i++] = "Your hands are glowing dull red.";
    }
    if (p_ptr->GetNewSpells()) {
        info[i++] = "You can learn some more spells.";
    }
    if (p_ptr->GetWordRecall()) {
        info[i++] = "You will soon be recalled.";
    }
    if (p_ptr->get_see_infra()) {
        info[i++] = "Your eyes are sensitive to infrared light.";
    }
    if (p_ptr->get_see_inv()) {
        info[i++] = "You can see invisible creatures.";
    }
    if (p_ptr->get_ffall()) {
        info[i++] = "You land gently.";
    }
    if (p_ptr->get_free_act()) {
        info[i++] = "You have free action.";
    }
    if (p_ptr->get_regenerate()) {
        info[i++] = "You regenerate quickly.";
    }
    if (p_ptr->get_slow_digest()) {
        info[i++] = "Your appetite is small.";
    }
    if (p_ptr->get_telepathy()) {
        info[i++] = "You have ESP.";
    }
    if (p_ptr->get_hold_life()) {
        info[i++] = "You have a firm hold on your life force.";
    }
    if (p_ptr->get_lite()) {
        info[i++] = "You are carrying a permanent light.";
    }

    if (p_ptr->get_immunes(IMMUNE_ACID)) {
        info[i++] = "You are completely immune to acid.";
    }
    else if (p_ptr->get_resists(RESIST_ACID) && p_ptr->GetOpposeAcid()) {
        info[i++] = "You resist acid exceptionally well.";
    }
    else if (p_ptr->get_resists(RESIST_ACID) || p_ptr->GetOpposeAcid()) {
        info[i++] = "You are resistant to acid.";
    }

    if (p_ptr->get_immunes(IMMUNE_ELEC)) {
        info[i++] = "You are completely immune to lightning.";
    }
    else if (p_ptr->get_resists(RESIST_ELEC) && p_ptr->GetOpposeElec()) {
        info[i++] = "You resist lightning exceptionally well.";
    }
    else if (p_ptr->get_resists(RESIST_ELEC) || p_ptr->GetOpposeElec()) {
        info[i++] = "You are resistant to lightning.";
    }

    if (p_ptr->get_immunes(IMMUNE_FIRE)) {
        info[i++] = "You are completely immune to fire.";
    }
    else if (p_ptr->get_resists(RESIST_FIRE) && p_ptr->GetOpposeFire()) {
        info[i++] = "You resist fire exceptionally well.";
    }
    else if (p_ptr->get_resists(RESIST_FIRE) || p_ptr->GetOpposeFire()) {
        info[i++] = "You are resistant to fire.";
    }

    if (p_ptr->get_immunes(IMMUNE_COLD)) {
        info[i++] = "You are completely immune to cold.";
    }
    else if (p_ptr->get_resists(RESIST_COLD) && p_ptr->GetOpposeCold()) {
        info[i++] = "You resist cold exceptionally well.";
    }
    else if (p_ptr->get_resists(RESIST_COLD) || p_ptr->GetOpposeCold()) {
        info[i++] = "You are resistant to cold.";
    }

    if (p_ptr->get_resists(RESIST_POIS) && p_ptr->GetOpposePois()) {
        info[i++] = "You resist poison exceptionally well.";
    }
    else if (p_ptr->get_resists(RESIST_POIS) || p_ptr->GetOpposePois()) {
        info[i++] = "You are resistant to poison.";
    }

    if (p_ptr->get_resists(RESIST_LITE)) {
        info[i++] = "You are resistant to bright light.";
    }
    if (p_ptr->get_resists(RESIST_DARK)) {
        info[i++] = "You are resistant to darkness.";
    }
    if (p_ptr->get_resists(RESIST_CONF)) {
        info[i++] = "You are resistant to confusion.";
    }
    if (p_ptr->get_resists(RESIST_SOUND)) {
        info[i++] = "You are resistant to sonic attacks.";
    }
    if (p_ptr->get_resists(RESIST_DISEN)) {
        info[i++] = "You are resistant to disenchantment.";
    }
    if (p_ptr->get_resists(RESIST_CHAOS)) {
        info[i++] = "You are resistant to chaos.";
    }
    if (p_ptr->get_resists(RESIST_SHARD)) {
        info[i++] = "You are resistant to blasts of shards.";
    }
    if (p_ptr->get_resists(RESIST_NEXUS)) {
        info[i++] = "You are resistant to nexus attacks.";
    }
    if (p_ptr->get_resists(RESIST_NETH)) {
        info[i++] = "You are resistant to nether forces.";
    }
    if (p_ptr->get_resists(RESIST_FEAR)) {
        info[i++] = "You are completely fearless.";
    }
    if (p_ptr->get_resists(RESIST_BLIND)) {
        info[i++] = "Your eyes are resistant to blindness.";
    }

    if (p_ptr->get_sustains(STAT_STR)) {
        info[i++] = "Your strength is sustained.";
    }
    if (p_ptr->get_sustains(STAT_INT)) {
        info[i++] = "Your intelligence is sustained.";
    }
    if (p_ptr->get_sustains(STAT_WIS)) {
        info[i++] = "Your wisdom is sustained.";
    }
    if (p_ptr->get_sustains(STAT_CON)) {
        info[i++] = "Your constitution is sustained.";
    }
    if (p_ptr->get_sustains(STAT_DEX)) {
        info[i++] = "Your dexterity is sustained.";
    }
    if (p_ptr->get_sustains(STAT_CHR)) {
        info[i++] = "Your charisma is sustained.";
    }

    if (f1 & TR1_STR) {
        info[i++] = "Your strength is affected by your equipment.";
    }
    if (f1 & TR1_INT) {
        info[i++] = "Your intelligence is affected by your equipment.";
    }
    if (f1 & TR1_WIS) {
        info[i++] = "Your wisdom is affected by your equipment.";
    }
    if (f1 & TR1_DEX) {
        info[i++] = "Your dexterity is affected by your equipment.";
    }
    if (f1 & TR1_CON) {
        info[i++] = "Your constitution is affected by your equipment.";
    }
    if (f1 & TR1_CHR) {
        info[i++] = "Your charisma is affected by your equipment.";
    }

    if (f1 & TR1_STEALTH) {
        info[i++] = "Your stealth is affected by your equipment.";
    }
    if (f1 & TR1_SEARCH) {
        info[i++] = "Your searching ability is affected by your equipment.";
    }
    if (f1 & TR1_INFRA) {
        info[i++] = "Your infravision is affected by your equipment.";
    }
    if (f1 & TR1_TUNNEL) {
        info[i++] = "Your digging ability is affected by your equipment.";
    }
    if (f1 & TR1_SPEED) {
        info[i++] = "Your speed is affected by your equipment.";
    }
    if (f1 & TR1_BLOWS) {
        info[i++] = "Your attack speed is affected by your equipment.";
    }


    /* Access the current weapon */
    i_ptr = &inventory[INVEN_WIELD];

    /* Analyze the weapon */
    if (i_ptr->exists()) {
        /* Indicate Blessing */
        if (f3 & TR3_BLESSED) {
            info[i++] = "Your weapon has been blessed by the gods.";
        }

        /* Hack */
        if (f1 & TR1_IMPACT) {
            info[i++] = "The impact of your weapon can cause earthquakes.";
        }

        /* Special "Attack Bonuses" */
        if (f1 & TR1_BRAND_ACID) {
            info[i++] = "Your weapon melts your foes.";
        }
        if (f1 & TR1_BRAND_ELEC) {
            info[i++] = "Your weapon shocks your foes.";
        }
        if (f1 & TR1_BRAND_FIRE) {
            info[i++] = "Your weapon burns your foes.";
        }
        if (f1 & TR1_BRAND_COLD) {
            info[i++] = "Your weapon freezes your foes.";
        }
        if (f1 & TR1_BRAND_POIS) {
            info[i++] = "Your weapon poisons your foes.";
        }

        /* Special "slay" flags */
        if (f1 & TR1_SLAY_ANIMAL) {
            info[i++] = "Your weapon strikes at animals with extra force.";
        }
        if (f1 & TR1_SLAY_EVIL) {
            info[i++] = "Your weapon strikes at evil with extra force.";
        }
        if (f1 & TR1_SLAY_UNDEAD) {
            info[i++] = "Your weapon strikes at undead with holy wrath.";
        }
        if (f1 & TR1_SLAY_DEMON) {
            info[i++] = "Your weapon strikes at demons with holy wrath.";
        }
        if (f1 & TR1_SLAY_ORC) {
            info[i++] = "Your weapon is especially deadly against orcs.";
        }
        if (f1 & TR1_SLAY_TROLL) {
            info[i++] = "Your weapon is especially deadly against trolls.";
        }
        if (f1 & TR1_SLAY_GIANT) {
            info[i++] = "Your weapon is especially deadly against giants.";
        }
        if (f1 & TR1_SLAY_DRAGON) {
            info[i++] = "Your weapon is especially deadly against dragons.";
        }

        /* Special "kill" flags */
        if (f1 & TR1_KILL_DRAGON) {
            info[i++] = "Your weapon is a great bane of dragons.";
        }
    }


    /* Save the screen */
    byte *screen = save_screen();

    /* Erase the screen */
    for (k = 1; k < 24; k++) box(13*8, k*16, 639, k*16+15, COLOR_BLACK);

    /* Label the information */
    put_string(20*8, 1*16, "Your Attributes:", COLOR_WHITE);

    /* We will print on top of the map (column 13) */
    for (k = 2, j = 0; j < i; j++) {
        /* Show the info */
        put_string(15*8, (k++)*16, info[j], COLOR_WHITE);

        /* Every 20 entries (lines 2 to 21), start over */
        if ((k == 22) && (j+1 < i)) {
            put_string(15*8, k*16, "-- more --", COLOR_WHITE);
            screen_refresh();
            wait_for_key();
            for ( ; k > 2; k--) box(15*8, k*16, 639, k*16+15, COLOR_BLACK);
        }
    }

    /* Pause */
    put_string(13*8, k*16, "[Press any key to continue]", COLOR_WHITE);
    screen_refresh();
    wait_for_key();

    /* Restore the screen */
    restore_screen(screen);
    delete[] screen;
}






/*
 * Forget everything
 */
bool lose_all_info(void)
{
    // Forget info about objects
    for (int i = 0; i < INVEN_TOTAL; i++) {
        CItem *i_ptr = &inventory[i];

        /* Skip non-items */
        if (!i_ptr->exists()) continue;

        /* Allow "protection" by the MENTAL flag */
        if (i_ptr->TestIdentFlag(ID_MENTAL)) continue;

        /* Remove "default inscriptions" */
        if (i_ptr->GetNote() && i_ptr->TestIdentFlag(ID_SENSE)) {
            /* Access the inscription */
            char *q = i_ptr->GetNote();

            /* Hack -- Remove auto-inscriptions */
            if ((streq(q, "cursed")) ||
                (streq(q, "broken")) ||
                (streq(q, "good")) ||
                (streq(q, "average")) ||
                (streq(q, "excellent")) ||
                (streq(q, "worthless")) ||
                (streq(q, "special")) ||
                (streq(q, "terrible")))
            {
                /* Forget the inscription */
                i_ptr->SetNote(NULL);
            }
        }

        /* Hack -- Clear the "empty" flag */
        i_ptr->ClearIdentFlag(ID_EMPTY);

        /* Hack -- Clear the "known" flag */
        i_ptr->ClearIdentFlag(ID_KNOWN);

        /* Hack -- Clear the "felt" flag */
        i_ptr->ClearIdentFlag(ID_SENSE);
    }

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    /* Mega-Hack -- Forget the map */
    forget_map();

    /* It worked */
    return TRUE;
}


/*
 * Detect nearby treasure             -RAK-   
 *
 * We do not yet create any "hidden gold" features XXX XXX XXX
 */
bool detect_treasure(void)
{
    bool detect = FALSE;
    CItem *i_ptr;

    /* Scan the detection radius */
    for (int x = p_ptr->GetX() - MAX_DETECT; x <= p_ptr->GetX() + MAX_DETECT; x++) {
        for (int y = p_ptr->GetY() - MAX_DETECT; y <= p_ptr->GetY() + MAX_DETECT; y++) {
            if (!in_bounds(y, x)) continue;
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > MAX_DETECT) continue;

            // Loop through all objects
            i_ptr = cave[y][x].i_ptr;
            while (i_ptr) {
                // Notice gold
                if (i_ptr->GetTval() == TV_GOLD) {
                    // Notice new items
                    if (!i_ptr->GetMarked()) {
                        /* Detect */
                        detect = TRUE;

                        /* Hack -- memorize the item */
                        i_ptr->SetMarked(TRUE);
                    }
                }

                // Get next
                i_ptr = i_ptr->next_i_ptr;
            }
        }
    }

    return detect;
}



/*
 * Detect magic items.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 */
bool detect_magic()
{
    bool detect = FALSE;
    CItem *i_ptr;

    /* Scan the detection radius */
    for (int x = p_ptr->GetX() - MAX_DETECT; x <= p_ptr->GetX() + MAX_DETECT; x++) {
        for (int y = p_ptr->GetY() - MAX_DETECT; y <= p_ptr->GetY() + MAX_DETECT; y++) {
            if (!in_bounds(y, x)) continue;
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > MAX_DETECT) continue;

            // Access the object
            i_ptr = cave[y][x].i_ptr;

            // For each object...
            while (i_ptr) {
                int tv = i_ptr->GetTval();

                /* Artifacts, misc magic items, or enchanted wearables */
                if (i_ptr->isArtifact() || i_ptr->isEgoItem() ||
                    (tv == TV_AMULET) || (tv == TV_RING) ||
                    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
                    (tv == TV_SCROLL) || (tv == TV_POTION) ||
                    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) ||
                    ((i_ptr->GetToA() > 0) ||
                    (i_ptr->GetToH() + i_ptr->GetToD() > 0)))
                {
                    /* Note new items */
                    if (!i_ptr->GetMarked()) {
                        /* Detect */
                        detect = TRUE;

                        /* Memorize the item */
                        i_ptr->SetMarked(TRUE);
                    }
                }

                // Get next
                i_ptr = i_ptr->next_i_ptr;
            }
        }
    }

    /* Return result */
    return detect;
}





/*
 * Locates and displays all nearby invisible creatures -RAK-
 */
bool detect_invisible()
{
    bool flag = FALSE;
    CMonsterRace *r_ptr;

    // Go through every tile
    for (int x = 0; x < cur_wid; x++) {
        for (int y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

            /* Skip visible monsters */
            if (m_ptr->is_visible()) continue;

            // Skip visible monsters (in another sense)
            if (!(r_ptr->flags2 & RF2_INVISIBLE)) continue;

            /* Detect all invisible monsters */
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) <= MAX_DETECT) {
                /* Take note that they are invisible */
                r_ptr->r_flags2 |= RF2_INVISIBLE;

                /* Detect the monster */
                if (m_ptr->detect < 200) m_ptr->detect += 10;
                flag = TRUE;
            }
        }
    }

    /* Result */
    return flag;
}



/*
 * Display nearby evil creatures              -RAK-
 */
bool detect_evil(void)
{
    bool flag = FALSE;
    CMonsterRace *r_ptr;

    // Go through every tile
    for (int x = 0; x < cur_wid; x++) {
        for (int y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

            /* Skip visible monsters */
            if (m_ptr->is_visible()) continue;

            // Skip non-evil monsters
            if (!(r_ptr->flags2 & RF3_EVIL)) continue;

            /* Detect evil monsters */
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) <= MAX_DETECT) {
                /* Detect the monster */
                if (m_ptr->detect < 200) m_ptr->detect += 10;
                flag = TRUE;
            }
        }
    }

    /* Result */
    return flag;
}



/*
 * Display all nearby non-invisible monsters
 */
bool detect_monsters(void)
{
    bool flag = FALSE;
    CMonsterRace *r_ptr;

    // Go through every tile
    for (int x = 0; x < cur_wid; x++) {
        for (int y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

            // Skip visible monsters
            if (m_ptr->is_visible()) continue;

            // Skip invisible monsters (in another sense)
            if (r_ptr->flags2 & RF2_INVISIBLE) continue;

            /* Detect all non-invisible monsters */
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) <= MAX_DETECT) {
                /* Detect the monster */
                if (m_ptr->detect < 200) m_ptr->detect += 10;
                flag = TRUE;
            }
        }
    }

    /* Result */
    return flag;
}


/*
 * Detect everything
 */
bool detection(void)
{
    bool detect = FALSE;

    // Detect the easy things
    if (detect_treasure()) detect = TRUE;
    if (detect_object()) detect = TRUE;
    if (detect_trap()) detect = TRUE;
    if (detect_sdoor()) detect = TRUE;

    // Go through every tile
    for (int x = 0; x < cur_wid; x++) {
        for (int y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Skip visible monsters
            if (m_ptr->is_visible()) continue;

            /* Detect all monsters */
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) <= MAX_DETECT) {
                /* Detect the monster */
                if (m_ptr->detect < 200) m_ptr->detect += 10;
                detect = TRUE;
            }
        }
    }

    /* Result */
    return detect;
}


/*
 * Detect nearby objects              -RAK-   
 */
bool detect_object(void)
{
    bool detect = FALSE;
    CGrid *g_ptr;
    CItem *i_ptr;

    /* Scan the detect radius */
    for (int x = p_ptr->GetX() - MAX_DETECT; x <= p_ptr->GetX() + MAX_DETECT; x++) {
        for (int y = p_ptr->GetY() - MAX_DETECT; y <= p_ptr->GetY() + MAX_DETECT; y++) {
            if (!in_bounds(y, x)) continue;
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > MAX_DETECT) continue;

            g_ptr = &cave[y][x];

            i_ptr = g_ptr->i_ptr;

            // Loop through all objects
            while (i_ptr) {
                /* Note new non-gold objects */
                if (!i_ptr->GetMarked() && (i_ptr->GetTval() != TV_GOLD)) {
                    /* Detect */
                    detect = TRUE;

                    /* Hack -- memorize it */
                    i_ptr->SetMarked(TRUE);
                }

                // Get next object
                i_ptr = i_ptr->next_i_ptr;
            }
        }
    }

    return detect;
}


/*
 * Locates and displays nearby traps
 */
bool detect_trap(void)
{
    bool detect = FALSE;
    CGrid *g_ptr;

    /* Scan the detect radius */
    for (int x = p_ptr->GetX() - MAX_DETECT; x <= p_ptr->GetX() + MAX_DETECT; x++) {
        for (int y = p_ptr->GetY() - MAX_DETECT; y <= p_ptr->GetY() + MAX_DETECT; y++) {
            if (!in_bounds(y, x)) continue;
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > MAX_DETECT) continue;

            // Access the grid
            g_ptr = &cave[y][x];

            // Detect invisible traps
            if (g_ptr->get_feat() == CF_TRAP_INVIS) {
                // Pick a trap
                pick_trap(y, x);

                // Hack -- memorize it
                g_ptr->flags |= MAP_KNOW;

                // Obvious
                detect = TRUE;
            }
        }
    }

    return detect;
}



/*
 * Locates and displays all nearby stairs and secret doors -RAK-      
 */
bool detect_sdoor()
{
    bool detect = FALSE;
    CGrid *g_ptr;

    /* Scan the detect radius */
    for (int x = p_ptr->GetX() - MAX_DETECT; x <= p_ptr->GetX() + MAX_DETECT; x++) {
        for (int y = p_ptr->GetY() - MAX_DETECT; y <= p_ptr->GetY() + MAX_DETECT; y++) {
            if (!in_bounds(y, x)) continue;
            if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > MAX_DETECT) continue;

            // Access the grid
            g_ptr = &cave[y][x];

            /* Hack -- detect secret doors */
            if (g_ptr->get_feat() == CF_DOOR_SECRET) {
                /* Find the door */
                g_ptr->set_feat(CF_DOOR_CLOSED);

                /* Memorize the door */
                g_ptr->flags |= MAP_KNOW;

                /* Obvious */
                detect = TRUE;
            }

            /* Ignore known grids */
            if (g_ptr->flags & MAP_KNOW) continue;

            /* Hack -- detect stairs */
            //: Add a specific detector
            if ((g_ptr->get_feat() == CF_STAIR_DOWN_NE) ||
                (g_ptr->get_feat() == CF_STAIR_DOWN_NW) ||
                (g_ptr->get_feat() == CF_STAIR_DOWN_SW) ||
                (g_ptr->get_feat() == CF_STAIR_DOWN_SE) ||
                (g_ptr->get_feat() == CF_STAIR_UP_NE) ||
                (g_ptr->get_feat() == CF_STAIR_UP_NW) ||
                (g_ptr->get_feat() == CF_STAIR_UP_SW) ||
                (g_ptr->get_feat() == CF_STAIR_UP_SE))
            {
                /* Memorize the stairs */
                g_ptr->flags |= MAP_KNOW;

                /* Obvious */
                detect = TRUE;
            }
        }
    }

    return (detect);
}


/*
 * Create stairs at the player location
 */
void stair_creation()
{
//: Stair creation
    msg_print("The Stair Creation spell is disabled.");
#if 0
    CGrid *g_ptr = p_ptr->get_g_ptr();

    /* XXX XXX XXX */
    if (!valid_grid(p_ptr->GetY(), p_ptr->GetX())) {
        msg_print("The object resists the spell.");
        return;
    }

    /* Hack -- Delete old contents */
    delete_objects(p_ptr->GetY(), p_ptr->GetX());

    /* Create a staircase */
    if (!dun_level) {
        g_ptr->set_feat(CF_STAIRS_DOWN);
    }
    else if (is_quest(dun_level) || (dun_level >= MAX_DEPTH-1)) {
        g_ptr->set_feat(CF_STAIRS_UP);
    }
    else if (percent(50)) {
        g_ptr->set_feat(CF_STAIRS_UP);
    }
    else {
        g_ptr->set_feat(CF_STAIRS_DOWN);
    }

    /* Notice */
    note_spot(p_ptr->GetY(), p_ptr->GetX());
#endif
}




/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(CItem *i_ptr)
{
    switch (i_ptr->GetTval()) {
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
        case TV_BOW:
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
            return TRUE;
    }

    return FALSE;
}


/*
 * Hook to specify "armor"
 */
static bool item_tester_hook_armor(CItem *i_ptr)
{
    switch (i_ptr->GetTval()) {
        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_BOOTS:
        case TV_GLOVES:
            return TRUE;
    }

    return FALSE;
}


/*
 * Enchants a plus onto an item.                        -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(CItem *i_ptr, int n, int eflag)
{
    int i, chance, prob;
    bool res = FALSE;
    bool a = i_ptr->isArtifact();
    u32b f1, f2, f3;

    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);


    /* Large piles resist enchantment */
    prob = i_ptr->GetNumber() * 100;

    /* Missiles are easy to enchant */
    if ((i_ptr->GetTval() == TV_BOLT) ||
        (i_ptr->GetTval() == TV_ARROW) ||
        (i_ptr->GetTval() == TV_SHOT))
    {
        prob = prob / 20;
    }

    /* Try "n" times */
    for (i = 0; i < n; i++) {
        /* Hack -- Roll for pile resistance */
        if (rand_int(prob) >= 100) continue;

        /* Enchant to hit */
        if (eflag & ENCH_TOHIT) {
            if (i_ptr->GetToH() < 0) chance = 0;
            else if (i_ptr->GetToH() > 15) chance = 1000;
            else chance = enchant_table[i_ptr->GetToH()];

            if ((randint(1000) > chance) && (!a || percent(50))) {
                i_ptr->SetToH(i_ptr->GetToH() + 1);
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (i_ptr->isCursed() &&
                    (!(f3 & TR3_PERMA_CURSE)) &&
                    (i_ptr->GetToH() >= 0) && percent(25))
                {
                    msg_print("The curse is broken!");
                    i_ptr->ClearIdentFlag(ID_CURSED);
                    i_ptr->SetIdentFlag(ID_SENSE);
                    i_ptr->SetNote("uncursed");
                }
            }
        }

        /* Enchant to damage */
        if (eflag & ENCH_TODAM) {
            if (i_ptr->GetToD() < 0) chance = 0;
            else if (i_ptr->GetToD() > 19) chance = 1000;
            else {
                if (i_ptr->GetTval() == TV_BOW) {
                    chance = enchant_table[i_ptr->GetToD()];
                }
                else {
                    /* Greg Wooledge -- generally limit +to-dam to weapons'
                       natural damage limitation.  E.g., a tulwar (2d4) can go
                       to +8.  Note the effect upon missiles.... */
                    chance = enchant_table_dam[i_ptr->GetToD()];
                    if ((i_ptr->GetDD() * i_ptr->GetDS() <=
                        i_ptr->GetToD()) &&
                        (chance < 995)) chance = 995;
                }
            }

            if ((randint(1000) > chance) && (!a || percent(50))) {
                i_ptr->SetToD(i_ptr->GetToD() + 1);
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (i_ptr->isCursed() &&
                    (!(f3 & TR3_PERMA_CURSE)) &&
                    (i_ptr->GetToD() >= 0) && percent(25))
                {
                    msg_print("The curse is broken!");
                    i_ptr->ClearIdentFlag(ID_CURSED);
                    i_ptr->SetIdentFlag(ID_SENSE);
                    i_ptr->SetNote("uncursed");
                }
            }
        }

        /* Enchant to armor class */
        if (eflag & ENCH_TOAC)
        {
            if (i_ptr->GetToA() < 0) chance = 0;
            else if (i_ptr->GetToA() > 15) chance = 1000;
            else chance = enchant_table[i_ptr->GetToA()];

            if ((randint(1000) > chance) && (!a || percent(50))) {
                i_ptr->SetToA(i_ptr->GetToA() + 1);
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (i_ptr->isCursed() &&
                    (!(f3 & TR3_PERMA_CURSE)) &&
                    (i_ptr->GetToA() >= 0) && percent(25))
                {
                    msg_print("The curse is broken!");
                    i_ptr->ClearIdentFlag(ID_CURSED);
                    i_ptr->SetIdentFlag(ID_SENSE);
                    i_ptr->SetNote("uncursed");
                }
            }
        }
    }

    /* Failure */
    if (!res) return FALSE;

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    /* Success */
    return TRUE;
}


/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armor, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
    int item;
    bool okay = FALSE;
    CItem *i_ptr;
    char i_name[80];


    /* Assume enchant weapon */
    item_tester_hook = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (num_ac) item_tester_hook = item_tester_hook_armor;

    /* Get an item (from equip or inven or floor) */
    if (!get_item(&item, "Enchant which item? ", GI_EQUIP | GI_INVEN | GI_FLOOR)) {
        if (item == -2) msg_print("You have nothing to enchant.");
        return (FALSE);
    }

    /* Get the item (in the pack) */
    i_ptr = gi_i_ptr;


    /* Description */
    i_ptr->object_desc(i_name, FALSE, 0);

    /* Describe */
    msg_format("%s %s glow%s brightly!",
        ((item >= 0) ? "Your" : "The"), i_name,
        i_ptr->isPlural() ? "" : "s");

    /* Enchant */
    if (enchant(i_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
    if (enchant(i_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
    if (enchant(i_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

    /* Failure */
    if (!okay) {
        /* Message */
        msg_print("The enchantment failed.");
    }

    /* Something happened */
    return TRUE;
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell()
{
    int item;
    CItem *i_ptr;
    char i_name[80];


    /* Get an item (from equip or inven or floor) */
    if (!get_item(&item, "Identify which item? ", GI_EQUIP | GI_INVEN | GI_FLOOR)) {
        if (item == -2) msg_print("You have nothing to identify.");
        return (FALSE);
    }

    /* Get the item (in the pack) */
    i_ptr = gi_i_ptr;


    /* Identify it fully */
    i_ptr->object_aware();
    i_ptr->MakeKnown();

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    /* Description */
    i_ptr->object_desc(i_name, TRUE, 3);

    /* Describe */
    if (item >= INVEN_WIELD) {
        msg_format("%^s: %s (%c).",
                   describe_use(item), i_name, index_to_label(item));
    }
    else if (item >= 0) {
        msg_format("In your pack: %s (%c).", i_name, index_to_label(item));
    }
    else {
        msg_format("On the ground: %s.", i_name);
    }

    /* Something happened */
    return TRUE;
}



/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully()
{
    int item;
    CItem *i_ptr;
    char i_name[80];


    /* Get an item (from equip or inven or floor) */
    if (!get_item(&item, "*Identify* which item? ", GI_EQUIP | GI_INVEN | GI_FLOOR)) {
        if (item == -2) msg_print("You have nothing to *identify*.");
        return (FALSE);
    }

    /* Get the item (in the pack) */
    i_ptr = gi_i_ptr;


    /* Identify it fully */
    i_ptr->object_aware();
    i_ptr->MakeKnown();

    /* Mark the item as fully known */
    i_ptr->SetIdentFlag(ID_MENTAL);

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    /* update stuff */
    update_stuff();

    /* Description */
    i_ptr->object_desc(i_name, TRUE, 3);

    /* Describe */
    if (item >= INVEN_WIELD) {
        msg_format("%^s: %s (%c).",
                   describe_use(item), i_name, index_to_label(item));
    }
    else if (item >= 0) {
        msg_format("In your pack: %s (%c).", i_name, index_to_label(item));
    }
    else {
        msg_format("On the ground: %s.", i_name);
    }

    /* Describe it fully */
    identify_fully_aux(i_ptr);

    /* Success */
    return TRUE;
}


/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(CItem *i_ptr)
{
    /* Recharge staffs */
    if (i_ptr->GetTval() == TV_STAFF) return TRUE;

    /* Recharge wands */
    if (i_ptr->GetTval() == TV_WAND) return TRUE;

    /* Hack -- Recharge rods */
    if (i_ptr->GetTval() == TV_ROD) return TRUE;

    /* Nope */
    return FALSE;
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 * Mage -- Recharge I --> recharge(5)
 * Mage -- Recharge II --> recharge(40)
 * Mage -- Recharge III --> recharge(100)
 *
 * Priest -- Recharge --> recharge(15)
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(int num)
{
    int i, t, item, lev;

    CItem *i_ptr;


    /* Only accept legal items */
    item_tester_hook = item_tester_hook_recharge;

    /* Get an item from inven or floor */
    if (!get_item(&item, "Recharge which item? ", GI_INVEN | GI_FLOOR)) {
        if (item == -2) msg_print("You have nothing to recharge.");
        return (FALSE);
    }

    /* Get the item (in the pack) */
    i_ptr = gi_i_ptr;


    /* Extract the object "level" */
    lev = i_ptr->GetObjLevel();

    /* Recharge a rod */
    if (i_ptr->GetTval() == TV_ROD) {

        /* Extract a recharge power */
        i = (100 - lev + num) / 5;

        /* Paranoia -- prevent crashes */
        if (i < 1) i = 1;

        /* Back-fire */
        if (one_in(i)) {
            /* Hack -- backfire */
            msg_print("The recharge backfires, draining the rod further!");

            /* Hack -- decharge the rod */
            if (i_ptr->GetPval() < 10000) {
                i_ptr->SetPval((i_ptr->GetPval() + 100) * 2);
            }
        }

        /* Recharge */
        else {
            /* Rechange amount */
            t = num * damroll(2, 4);

            /* Recharge by that amount */
            if (i_ptr->GetPval() > t) {
                i_ptr->SetPval(i_ptr->GetPval() - t);
            }

            /* Fully recharged */
            else {
                i_ptr->SetPval(0);
            }
        }
    }

    /* Recharge wand/staff */
    else {
        /* Recharge power */
        i = (num + 100 - lev - (10 * i_ptr->GetPval())) / 15;

        /* Paranoia -- prevent crashes */
        if (i < 1) i = 1;

        /* Back-fire XXX XXX XXX */
        if (one_in(i)) {
            /* Dangerous Hack -- Destroy the item */
            msg_print("There is a bright flash of light.");

            /* Reduce and describe inventory */
            if (item >= 0) {
                inven_item_increase(item, -999);
                inven_item_describe(item);
                inven_item_optimize(item);
            }

            /* Reduce and describe floor item */
            else {
                floor_item_increase(i_ptr, -999);
                floor_item_describe(i_ptr);
                floor_item_optimize(i_ptr);
            }
        }

        /* Recharge */
        else {

            /* Extract a "power" */
            t = (num / (lev + 2)) + 1;

            /* Recharge based on the power */
            if (t > 0) i_ptr->SetPval(i_ptr->GetPval() + 2 + randint(t));

            /* Hack -- we no longer "know" the item */
            i_ptr->ClearIdentFlag(ID_KNOWN);

            /* Hack -- we no longer think the item is empty */
            i_ptr->ClearIdentFlag(ID_EMPTY);
        }
    }

    /* Combine / Reorder the pack (later) */
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    /* Something was done */
    return (TRUE);
}


/*
 * Apply a "project()" directly to all viewable monsters
 */
static bool project_hack(int typ, int dam)
{
    int x, y;
    int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
    bool obvious = FALSE;
    CMonster *m_ptr;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

                 /* Require line of sight */
            if (!player_has_los_bold(y, x)) continue;

            /* Jump directly to the target monster */
            if (project(p_ptr, 0, y, x, dam, typ, flg)) obvious = TRUE;
        }
    }

    /* Result */
    return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
    return project_hack(GF_OLD_SPEED, p_ptr->GetLev());
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
    return project_hack(GF_OLD_SLOW, p_ptr->GetLev());
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
    return project_hack(GF_OLD_SLEEP, p_ptr->GetLev());
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
    return project_hack(GF_AWAY_EVIL, dist);
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
    return project_hack(GF_TURN_UNDEAD, p_ptr->GetLev());
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
    return project_hack(GF_DISP_UNDEAD, dam);
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
    return project_hack(GF_DISP_EVIL, dam);
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
    return project_hack(GF_DISP_ALL, dam);
}





/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(CMonster *who)
{
    int x, y;
    bool sleep = FALSE;
    bool speed = FALSE;
    CMonster *m_ptr;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            /* Skip aggravating monster (or player) */
            if (m_ptr == who) continue;

            /* Wake up nearby sleeping monsters */
            if (m_ptr->get_cdis() < MAX_SIGHT * 2) {
                /* Wake up */
                if (m_ptr->get_csleep()) {
                    /* Wake up */
                    m_ptr->set_csleep(0);
                    sleep = TRUE;
                }
            }

            // Speed up monsters in line of sight
            if (player_has_los_bold(m_ptr->GetY(), m_ptr->GetX())) {
                /* If not already hasted 10 turns, make hasted 10 turns */
                if (m_ptr->get_fast() < 10) {
                    m_ptr->set_fast(10);
                    speed = TRUE;
                }
            }
        }
    }

    /* Messages */
    if (speed) msg_print("You feel a sudden stirring nearby!");
    else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool genocide(void)
{
    int x, y;
    int type;
    bool result = FALSE;
    CMonster *m_ptr;
    CMonsterRace *r_ptr;

    //: XXX Select a monster type
    type = TYPE_TOWN;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

            /* Hack -- Skip Unique Monsters */
            if (r_ptr->flags1 & RF1_UNIQUE) continue;

            // Skip monsters with the incorrect type
            if (r_ptr->type != type) continue;

            /* Skip magic-resistant monsters */
            if (r_ptr->flags3 & RF3_RES_MAGIC) continue;

            /* Delete the monster */
            delete_monster(m_ptr);

            /* Take damage */
            p_ptr->take_hit_internal(randint(4), "the strain of casting Genocide");

            /* Take note */
            result = TRUE;
        }
    }

    return result;
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(void)
{
    int x, y;
    bool result = FALSE;
    CMonster *m_ptr;
    CMonsterRace *r_ptr;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

                   /* Hack -- Skip unique monsters */
            if (r_ptr->flags1 & RF1_UNIQUE) continue;

            /* Skip magic-resistant monsters */
            if (r_ptr->flags3 & RF3_RES_MAGIC) continue;

            /* Skip distant monsters */
            if (m_ptr->get_cdis() > MAX_SIGHT) continue;

            /* Delete the monster */
            delete_monster(m_ptr);

            /* Take damage */
            p_ptr->take_hit_internal(randint(3), "the strain of casting Mass Genocide");

            /* Note effect */
            result = TRUE;
        }
    }

    return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
    int x, y;
    bool probe = FALSE;
    CMonster *m_ptr;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            /* Require line of sight */
            if (!player_has_los_bold(m_ptr->GetY(), m_ptr->GetX())) continue;

            /* Probe visible monsters */
            if (m_ptr->is_visible()) {
                char m_name[80];

                /* Get "the monster" or "something" */
                m_ptr->get_desc(m_name, 0x04);

                /* Describe the monster */
                msg_format("%^s has %d hit points.", m_name, m_ptr->GetCHP());

                // Learn all of the non-spell, non-treasure flags
                m_ptr->lore_do_probe();

                // Probe worked
                probe = TRUE;
            }
        }
    }

    /* Result */
    return probe;
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full)
{
    int y, x, k;
    CGrid *g_ptr;
    bool flag = FALSE;

    /* XXX XXX */
    full = full ? full : 0;

    /* Big area of affect */
    for (y = (y1 - r); y <= (y1 + r); y++) {
        for (x = (x1 - r); x <= (x1 + r); x++) {
            /* Skip illegal grids */
            if (!in_bounds(y, x)) continue;

            /* Extract the distance */
            k = distance(x1, y1, x, y);

            /* Stay in the circle of death */
            if (k > r) continue;

            /* Access the grid */
            g_ptr = &cave[y][x];

            /* Lose room and vault */
            g_ptr->flags &= ~(MAP_ROOM | MAP_ICKY);

            /* Lose light and knowledge */
            g_ptr->flags &= ~(MAP_KNOW | MAP_GLOW);

            /* Hack -- Notice player affect */
            if (p_ptr->is_at(x, y)) {
                /* Hurt the player later */
                flag = TRUE;

                /* Do not hurt this grid */
                continue;
            }

            /* Hack -- Skip the epicenter */
            if ((y == y1) && (x == x1)) continue;

            /* Delete the monster (if any) */
            delete_monster(y, x);
                
            /* Destroy "valid" grids */
            if (valid_grid(y, x)) {

                /* Delete the objects (if any) */
                delete_objects(y, x);

                /* Granite */
                if (percent(50)) {
                    /* Clear previous contents, add granite wall */
                    g_ptr->set_feat(CF_GRANITE_BASIC);
                }

                /* Floor */
                else {
                    /* Clear previous contents, add floor */
                    g_ptr->set_feat(CF_FLOOR);
                }
            }
        }
    }


    /* Hack -- Affect player */
    if (flag) {
        /* Message */
        msg_print("There is a searing blast of light!");

        /* Blind the player */
        if (!p_ptr->get_resists(RESIST_BLIND) && !p_ptr->get_resists(RESIST_LITE)) {
            /* Become blind */
            p_ptr->mod_blind(p_ptr->GetBlind() + 10 + randint(10));
        }
    }


    // Mega-Hack -- Forget the view and lite 
    p_ptr->set_update(p_ptr->get_update() | PU_UN_VIEW | PU_UN_LITE);

    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW);

    // Update the monsters
    p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that thus the player and monsters (except eaters of walls and
 * passers through walls) will never occupy the same grid as a wall.
 * Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
 * for a single turn, unless that monster can pass_walls or kill_walls.
 * This has allowed massive simplification of the "monster" code.
 */
void earthquake(int cy, int cx, int r)
{
    int i, t, x, y, xx, yy, dx, dy, ox, oy;
    int damage = 0;
    int sn = 0, sy = 0, sx = 0;
    bool hurt = FALSE;
    CGrid *g_ptr;
    bool map[32][32];


    /* Paranoia -- Enforce maximum range */
    if (r > 12) r = 12;

    /* Clear the "maximal blast" area */
    for (y = 0; y < 32; y++) {
        for (x = 0; x < 32; x++) {
            map[y][x] = FALSE;
        }
    }

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++) {
        for (dx = -r; dx <= r; dx++) {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cx, cy, xx, yy) > r) continue;

            /* Access the grid */
            g_ptr = &cave[yy][xx];

            /* Lose room and vault */
            g_ptr->flags &= ~(MAP_ROOM | MAP_ICKY);

            /* Lose light and knowledge */
            g_ptr->flags &= ~(MAP_GLOW | MAP_KNOW);

            /* Skip the epicenter */
            if (!dx && !dy) continue;

            /* Skip most grids */
            if (rand_int(100) < 85) continue;

            /* Damage this grid */
            map[16+yy-cy][16+xx-cx] = TRUE;

            /* Hack -- Take note of player damage */
            if (p_ptr->is_at(xx, yy)) hurt = TRUE;
        }
    }

    /* First, affect the player (if necessary) */
    if (hurt) {
        /* Check around the player */
        for (i = 0; i < 8; i++) {
            /* Access the location */
            y = p_ptr->GetY() + ddy[i];
            x = p_ptr->GetX() + ddx[i];

            /* Skip non-empty grids */
            if (!empty_grid_bold(y, x)) continue;

            /* Important -- Skip "quake" grids */
            if (map[16+y-cy][16+x-cx]) continue;

            /* Count "safe" grids */
            sn++;

            /* Randomize choice */
            if (rand_int(sn) > 0) continue;

            /* Save the safe location */
            sy = y; sx = x;
        }

        /* Random message */
        switch (randint(3)) {
            case 1:
                msg_print("The cave ceiling collapses!");
                break;
            case 2:
                msg_print("The cave floor twists in an unnatural way!");
                break;
            default:
                msg_print("The cave quakes!  You are pummeled with debris!");
                break;
        }

        /* Hurt the player a lot */
        if (!sn) {
            /* Message and damage */
            msg_print("You are severely crushed!");
            damage = 300;
        }

        /* Destroy the grid, and push the player to safety */
        else {
            /* Calculate results */
            switch (randint(3)) {
                case 1:
                    msg_print("You nimbly dodge the blast!");
                    damage = 0;
                    break;
                case 2:
                    msg_print("You are bashed by rubble!");
                    damage = damroll(10, 4);
                    p_ptr->mod_stun(p_ptr->GetStun() + randint(50));
                    break;
                case 3:
                    msg_print("You are crushed between the floor and ceiling!");
                    damage = damroll(10, 4);
                    p_ptr->mod_stun(p_ptr->GetStun() + randint(50));
                    break;
            }

            /* Save the old location */
            ox = p_ptr->GetX();
            oy = p_ptr->GetY();

            /* Move the player to the safe location */
            p_ptr->SetLocation(sx, sy);
        }

        /* Important -- no wall on player */
        map[16+p_ptr->GetY()-cy][16+p_ptr->GetX()-cx] = FALSE;

        /* Take some damage */
        if (damage) p_ptr->take_hit(damage, "an earthquake");
    }


    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++) {
        for (dx = -r; dx <= r; dx++) {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Access the grid */
            g_ptr = &cave[yy][xx];

            /* Process monsters */
            if (g_ptr->m_ptr) {
                CMonster *m_ptr = g_ptr->m_ptr;
                CMonsterRace *r_ptr = m_ptr->get_r_ptr();

                /* Most monsters cannot co-exist with rock */
                if (!(r_ptr->flags2 & RF2_KILL_WALL) &&
                    !(r_ptr->flags2 & RF2_PASS_WALL))
                {
                    char m_name[80];

                    /* Assume not safe */
                    sn = 0;

                    /* Monster can move to escape the wall */
                    if (!(r_ptr->flags1 & RF1_NEVER_MOVE)) {
                        /* Look for safety */
                        for (i = 0; i < 8; i++) {
                            /* Access the grid */
                            x = xx + ddx[i];
                            y = yy + ddy[i];

                            /* Skip non-empty grids */
                            if (!empty_grid_bold(y, x)) continue;

                            /* Hack -- no safety on glyph of warding */
                            if (cave[y][x].get_feat() == CF_GLYPH) continue;

                            /* Important -- Skip "quake" grids */
                            if (map[16+y-cy][16+x-cx]) continue;

                            /* Count "safe" grids */
                            sn++;

                            /* Randomize choice */
                            if (rand_int(sn) > 0) continue;

                            /* Save the safe grid */
                            sy = y; sx = x;
                        }
                    }

                    /* Describe the monster */
                    m_ptr->get_desc(m_name, 0);

                    /* Scream in pain */
                    msg_format("%^s wails out in pain!", m_name);

                    /* Take damage from the quake */
                    damage = (sn ? damroll(4, 8) : 200);

                    /* Monster is certainly awake */
                    m_ptr->set_csleep(0);

                    /* Apply damage directly */
                    m_ptr->SetCHP(m_ptr->GetCHP() - damage);

                    /* Delete (not kill) "dead" monsters */
                    if (m_ptr->GetCHP() < 0) {
                        /* Message */
                        msg_format("%^s is embedded in the rock!", m_name);

                        /* Delete the monster */
                        delete_monster(yy, xx);

                        /* No longer safe */
                        sn = 0;
                    }

                    /* Hack -- Escape from the rock */
                    if (sn) {
                        /* Update the new location */
                        cave[sy][sx].m_ptr = m_ptr;

                        /* Update the old location */
                        cave[yy][xx].m_ptr = NULL;

                        /* Move the monster */
                        m_ptr->SetLocation(sx, sy);

                        /* Update the monster (new location) */
                        m_ptr->update();
                    }
                }
            }
        }
    }


    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++) {
        for (dx = -r; dx <= r; dx++) {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Access the cave grid */
            g_ptr = &cave[yy][xx];

            /* Paranoia -- never affect player */
            if (p_ptr->is_at(xx, yy)) continue;

            /* Destroy location (if valid) */
            if (valid_grid(yy, xx)) {
                bool floor = floor_grid_bold(yy, xx);

                /* Delete any objects that are still there */
                delete_objects(yy, xx);

                /* Wall (or floor) type */
                t = (floor ? rand_int(100) : 200);

                /* Granite */
                if (t < 100) {
                    /* Clear previous contents, add granite wall */
                    g_ptr->set_feat(CF_GRANITE_BASIC);
                }

                /* Floor */
                else {
                    /* Clear previous contents, add floor */
                    g_ptr->set_feat(CF_FLOOR);
                }
            }
        }
    }


    // Mega-Hack -- Forget the view and lite
    p_ptr->set_update(p_ptr->get_update() | PU_UN_VIEW | PU_UN_LITE);

    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW);

    // Update the monsters 
    p_ptr->set_update(p_ptr->get_update() | PU_DISTANCE);
}


/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
    int i;

    /* Clear them all */
    for (i = 0; i < temp_n; i++) {
        int y = temp_y[i];
        int x = temp_x[i];

        CGrid *g_ptr = &cave[y][x];

        /* No longer in the array */
        g_ptr->flags &= ~MAP_TEMP;

        /* Update only non-MAP_GLOW grids */
        /* if (g_ptr->flags & MAP_GLOW) continue; */

        /* Perma-Lite */
        g_ptr->flags |= MAP_GLOW;

        /* Process affected monsters */
        if (g_ptr->m_ptr) {
            CMonster *m_ptr = g_ptr->m_ptr;
            CMonsterRace *r_ptr = m_ptr->get_r_ptr();

            // Update the monster
            m_ptr->update();

            /* Sometimes monsters wake up */
            if (m_ptr->get_csleep() &&
                (((r_ptr->flags2 & RF2_STUPID) && percent(10)) ||
                 percent(25) ||
                 (r_ptr->flags2 & RF2_SMART)))
            {
                /* Wake up! */
                m_ptr->set_csleep(0);

                /* Notice the "waking up" */
                if (m_ptr->is_visible()) {
                    char m_name[80];

                    /* Acquire the monster name */
                    m_ptr->get_desc(m_name, 0);

                    /* Dump a message */
                    msg_format("%^s wakes up.", m_name);
                }
            }
        }

        /* Note */
        note_spot(y, x);
    }

    /* None left */
    temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *                                                   
 * This routine is used (only) by "unlite_room()"
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
    int i;

    /* Clear them all */
    for (i = 0; i < temp_n; i++) {
        int y = temp_y[i];
        int x = temp_x[i];

        CGrid *g_ptr = &cave[y][x];

        /* No longer in the array */
        g_ptr->flags &= ~MAP_TEMP;

        /* Darken the grid */
        g_ptr->flags &= ~MAP_GLOW;

        /* Process affected monsters */
        if (g_ptr->m_ptr) {
            /* Update the monster */
            g_ptr->m_ptr->update();
        }
    }

    /* None left */
    temp_n = 0;
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
    CGrid *g_ptr = &cave[y][x];

    /* Avoid infinite recursion */
    if (g_ptr->flags & MAP_TEMP) return;

    /* Do not "leave" the current room */
    if (!(g_ptr->flags & MAP_ROOM)) return;

    /* Paranoia -- verify space */
    if (temp_n == TEMP_MAX) return;

    /* Mark the grid as "seen" */
    g_ptr->flags |= MAP_TEMP;

    /* Add it to the "seen" set */
    temp_y[temp_n] = y;
    temp_x[temp_n] = x;
    temp_n++;
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_room_aux(y1, x1);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < temp_n; i++) {
        x = temp_x[i], y = temp_y[i];

        /* Walls get lit, but stop light */
        if (!floor_grid_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_room_aux(y + 1, x);
        cave_temp_room_aux(y - 1, x);
        cave_temp_room_aux(y, x + 1);
        cave_temp_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_room_aux(y + 1, x + 1);
        cave_temp_room_aux(y - 1, x - 1);
        cave_temp_room_aux(y - 1, x + 1);
        cave_temp_room_aux(y + 1, x - 1);
    }

    /* Now, lite them all up at once */
    cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_room_aux(y1, x1);

    /* Spread, breadth first */
    for (i = 0; i < temp_n; i++)
    {
        x = temp_x[i], y = temp_y[i];

        /* Walls get dark, but stop darkness */
        if (!floor_grid_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_room_aux(y + 1, x);
        cave_temp_room_aux(y - 1, x);
        cave_temp_room_aux(y, x + 1);
        cave_temp_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_room_aux(y + 1, x + 1);
        cave_temp_room_aux(y - 1, x - 1);
        cave_temp_room_aux(y - 1, x + 1);
        cave_temp_room_aux(y + 1, x - 1);
    }

    /* Now, darken them all at once */
    cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
    int flg = PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!p_ptr->GetBlind()) {
        msg_print("You are surrounded by a white light.");
    }

    /* Hook into the "project()" function */
    project(p_ptr, rad, p_ptr->GetY(), p_ptr->GetX(), dam, GF_LITE_WEAK, flg);

    /* Lite up the room */
    lite_room(p_ptr->GetY(), p_ptr->GetX());

    /* Assume seen */
    return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
    int flg = PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!p_ptr->GetBlind()) {
        msg_print("Darkness surrounds you.");
    }

    /* Hook into the "project()" function */
    project(p_ptr, rad, p_ptr->GetY(), p_ptr->GetX(), dam, GF_DARK_WEAK, flg);

    /* Lite up the room */
    unlite_room(p_ptr->GetY(), p_ptr->GetX());

    /* Assume seen */
    return TRUE;
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int tx, int ty, int dam, int rad)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

    /* Analyze the "dir" and the "target".  Hurt items on floor. */
    return project(p_ptr, rad, ty, tx, dam, typ, flg);
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int tx, int ty, int dam, int flg)
{
    // Pass through the target if needed 
    flg |= PROJECT_THRU;

    // Analyze the "dir" and the "target", do NOT explode
    return project(p_ptr, 0, ty, tx, dam, typ, flg);
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int tx, int ty, int dam)
{
    return project_hook(typ, tx, ty, dam, PROJECT_STOP | PROJECT_KILL);
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int tx, int ty, int dam)
{
    return project_hook(typ, tx, ty, dam, PROJECT_BEAM | PROJECT_KILL);
}

/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int tx, int ty, int dam)
{
    if (percent(prob)) {
        return fire_beam(typ, tx, ty, dam);
    }
    else {
        return fire_bolt(typ, tx, ty, dam);
    }
}


/*
 * Some of the old functions
 */

bool lite_line(int tx, int ty)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
    return project_hook(GF_LITE_WEAK, tx, ty, damroll(6, 8), flg);
}

bool drain_life(int tx, int ty, int dam)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_DRAIN, tx, ty, dam, flg);
}

bool wall_to_mud(int tx, int ty)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    return project_hook(GF_KILL_WALL, tx, ty, 50, flg);
}

bool destroy_door(int tx, int ty)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return project_hook(GF_KILL_DOOR, tx, ty, 0, flg);
}

bool disarm_trap(int tx, int ty)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return project_hook(GF_KILL_TRAP, tx, ty, 0, flg);
}

bool heal_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_HEAL, tx, ty, damroll(4, 6), flg);
}

bool speed_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_OLD_SPEED, tx, ty, p_ptr->GetLev(), flg));
}

bool slow_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_SLOW, tx, ty, p_ptr->GetLev(), flg);
}

bool sleep_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_SLEEP, tx, ty, p_ptr->GetLev(), flg);
}

bool confuse_monster(int tx, int ty, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_CONF, tx, ty, plev, flg);
}

bool poly_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_POLY, tx, ty, p_ptr->GetLev(), flg);
}

bool clone_monster(int tx, int ty)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_OLD_CLONE, tx, ty, 0, flg);
}

bool fear_monster(int tx, int ty, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return project_hook(GF_TURN_ALL, tx, ty, plev, flg);
}

bool teleport_monster(int tx, int ty)
{
    int flg = PROJECT_BEAM | PROJECT_KILL;
    return project_hook(GF_AWAY_ALL, tx, ty, MAX_SIGHT * 5, flg);
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation()
{
//: Fix door creation
    msg_print("The Door Creation spell is disabled.");
    return FALSE;
/*
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return project(p_ptr, 1, p_ptr->GetY(), p_ptr->GetX(), 0, GF_MAKE_DOOR, flg);
*/
}

bool trap_creation()
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return project(p_ptr, 1, p_ptr->GetY(), p_ptr->GetX(), 0, GF_MAKE_TRAP, flg);
}

bool destroy_doors_touch()
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return project(p_ptr, 1, p_ptr->GetY(), p_ptr->GetX(), 0, GF_KILL_DOOR, flg);
}

bool sleep_monsters_touch(void)
{
    int flg = PROJECT_KILL | PROJECT_HIDE;
    return project(p_ptr, 1, p_ptr->GetY(), p_ptr->GetX(), p_ptr->GetLev(),
        GF_OLD_SLEEP, flg);
}



/*
 * Brand the current weapon
 */
void brand_weapon(void)
{
    CItem *i_ptr;

    i_ptr = &inventory[INVEN_WIELD];

    /* you can never modify artifacts / ego-items */
    /* you can never modify broken / cursed items */
    if (i_ptr->exists() &&
        !i_ptr->isArtifact() && !i_ptr->isEgoItem() &&
        !i_ptr->isCursed() && !i_ptr->isBroken())
    {
        char *act = NULL;
        char i_name[80];

        if (percent(50)) {
            act = "is covered in a fiery shield";
            i_ptr->SetName2(EGO_BRAND_FIRE);
        }

        else {
            act = "glows deep, icy blue";
            i_ptr->SetName2(EGO_BRAND_COLD);
        }

        i_ptr->object_desc(i_name, FALSE, 0);

        msg_format("Your %s %s!", i_name, act);

        enchant(i_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
    }

    else {
        msg_print("The Branding failed.");
    }
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
    CItem *j_ptr;
    char i_name[80];


    // Curse the body armor
    j_ptr = &inventory[INVEN_BODY];

    // Nothing to curse
    if (!j_ptr->exists()) return FALSE;


    // Describe
    j_ptr->object_desc(i_name, FALSE, 3);

    // Attempt a saving throw for artifacts
    if (j_ptr->isArtifact() && percent(50)) {
        // Cool
        msg_format("A %s aura tries to %s, but your %s resists the effects!",
            "terrible black", "surround your armor", i_name);
    }

    /* not artifact or failed save... */
    else {
        /* Oops */
        msg_format("A terrible black aura blasts your %s!", i_name);

        /* Blast the armor */
        j_ptr->SetName1(0);
        j_ptr->SetName2(EGO_BLASTED);
        j_ptr->SetToA(-damroll(2, 5));
        j_ptr->SetToH(0);
        j_ptr->SetToD(0);
        j_ptr->SetAC(0);
        j_ptr->SetDD(0);
        j_ptr->SetDS(0);

        /* Curse and break it */
        j_ptr->SetIdentFlag(ID_CURSED);
        j_ptr->SetIdentFlag(ID_BROKEN);

        // Recalculate bonuses and spell points
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_MANA);
    }

    return TRUE;
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
    CItem *j_ptr;

    char i_name[80];


    /* Curse the weapon */
    j_ptr = &inventory[INVEN_WIELD];

    /* Nothing to curse */
    if (!j_ptr->exists()) return FALSE;


    /* Describe */
    j_ptr->object_desc(i_name, FALSE, 3);

    /* Attempt a saving throw */
    if (j_ptr->isArtifact() && percent(50)) {
        /* Cool */
        msg_format("A %s tries to %s, but your %s resists the effects!",
                   "terrible black aura", "surround your weapon", i_name);
    }

    /* not artifact or failed save... */
    else {
        /* Oops */
        msg_format("A terrible black aura blasts your %s!", i_name);

        /* Shatter the weapon */
        j_ptr->SetName1(0);
        j_ptr->SetName2(EGO_SHATTERED);
        j_ptr->SetToH(-damroll(2, 5));
        j_ptr->SetToD(-damroll(2, 5));
        j_ptr->SetToA(0);
        j_ptr->SetAC(0);
        j_ptr->SetDD(0);
        j_ptr->SetDS(0);

        // Curse and break it
        j_ptr->SetIdentFlag(ID_CURSED);
        j_ptr->SetIdentFlag(ID_BROKEN);

        // Recalculate bonuses and spell points
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_MANA);
    }

    /* Notice */
    return TRUE;
}

