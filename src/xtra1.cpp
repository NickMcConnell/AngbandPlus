// File: xtra1.cpp
// Purpose: misc code

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
    // Above 18
    if (val > 18) {
        int bonus = (val - 18);

        if (bonus >= 220) {
            sprintf(out_val, "18/%3s", "***");
        }
        else if (bonus >= 100) {
            sprintf(out_val, "18/%03d", bonus);
        }
        else {
            sprintf(out_val, " 18/%02d", bonus);
        }
    }

    // From 3 to 18
    else {
        sprintf(out_val, "    %2d", val);
    }
}


/*
 * Converts stat num into a left justified string
 */
void cnv_stat_left(int val, char *out_val)
{
    // Above 18
    if (val > 18) {
        int bonus = (val - 18);

        if (bonus >= 220) {
            strcpy(out_val, "18/***");
        }
        else if (bonus >= 100) {
            sprintf(out_val, "18/%d", bonus);
        }
        else {
            sprintf(out_val, "18/%02d", bonus);
        }
    }

    // From 3 to 18
    else {
        sprintf(out_val, "%d", val);
    }
}



/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
    int i;

    // Reward
    if (amount > 0) {
        // Apply each point
        for (i = 0; i < amount; i++) {
            // One point at a time
            if (value < 18) value++;

            // Ten "points" at a time
            else value += 10;
        }
    }

    // Penalty
    else if (amount < 0) {
        // Apply each point
        for (i = 0; i < (0 - amount); i++) {
            // Ten points at a time
            if (value >= 18+10) value -= 10;

            // Hack -- prevent weirdness
            else if (value > 18) value = 18;

            // One point at a time
            else if (value > 3) value--;
        }
    }

    // Return new value
    return (value);
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
    int i, j, k, levels;
    int num_allowed, num_known;

    magic_type *s_ptr;

    char *p = (mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer";


    /* Hack -- must be literate */
    if (!mp_ptr->spell_book) return;

    /* Hack -- wait for creation */
    if (!character_generated) return;

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;


    /* Determine the number of spells allowed */
    levels = p_ptr->GetLev() - mp_ptr->spell_first + 1;

    /* Hack -- no negative spells */
    if (levels < 0) levels = 0;

    /* Extract total allowed spells */
    num_allowed = adj_mag_study[p_ptr->GetStatInd(mp_ptr->spell_stat)] *
        levels / 2;

    /* Assume none known */
    num_known = 0;

    /* Count the number of spells we know */
    for (j = 0; j < 64; j++) {
        /* Count known spells */
        if (spell_learned[j]) num_known++;
    }

    /* See how many spells we must forget or may learn */
    p_ptr->SetNewSpells(num_allowed - num_known);



    /* Forget spells which are too hard */
    for (i = 63; i >= 0; i--) {
        /* Access the spell */
        j = spell_order[i];

        /* Skip non-spells */
        if (j >= 99) continue;

        /* Get the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we are allowed to know */
        if (s_ptr->slevel <= p_ptr->GetLev()) continue;

        /* Is it known? */
        if (spell_learned[j]) {
            /* Mark as forgotten */
            spell_forgotten[j] = TRUE;

            /* No longer known */
            spell_learned[j] = FALSE;

            /* Message */
            msg_format("You have forgotten the %s of %s.", p,
                spell_names[mp_ptr->spell_type][j]);

            /* One more can be learned */
            p_ptr->SetNewSpells(p_ptr->GetNewSpells() + 1);
        }
    }


    /* Forget spells if we know too many spells */
    for (i = 63; i >= 0; i--) {
        /* Stop when possible */
        if (p_ptr->GetNewSpells() >= 0) break;

        /* Get the (i+1)th spell learned */
        j = spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) continue;

        /* Forget it (if learned) */
        if (spell_learned[j]) {
            /* Mark as forgotten */
            spell_forgotten[j] = TRUE;

            // No longer known
            spell_learned[j] = FALSE;

            // Message
            msg_format("You have forgotten the %s of %s.", p,
                spell_names[mp_ptr->spell_type][j]);

            // One more can be learned
            p_ptr->SetNewSpells(p_ptr->GetNewSpells() + 1);
        }
    }


    /* Check for spells to remember */
    for (i = 0; i < 64; i++) {
        /* None left to remember */
        if (p_ptr->GetNewSpells() <= 0) break;

        /* Get the next spell we learned */
        j = spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) break;

        /* Access the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p_ptr->GetLev()) continue;

        /* First set of spells */
        if (spell_forgotten[j]) {
            /* No longer forgotten */
            spell_forgotten[j] = FALSE;

            /* Known once more */
            spell_learned[j] = TRUE;

            /* Message */
            msg_format("You have remembered the %s of %s.",
                p, spell_names[mp_ptr->spell_type][j]);

            /* One less can be learned */
            p_ptr->SetNewSpells(p_ptr->GetNewSpells() - 1);
        }
    }


    /* Assume no spells available */
    k = 0;

    /* Count spells that can be learned */
    for (j = 0; j < 64; j++) {
        /* Access the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p_ptr->GetLev()) continue;

        /* Skip spells we already know */
        if (spell_learned[j]) continue;

        /* Count it */
        k++;
    }

    /* Cannot learn more spells than exist */
    if (p_ptr->GetNewSpells() > k) p_ptr->SetNewSpells(k);

    /* Learn new spells */
    if (p_ptr->GetNewSpells() && !p_ptr->GetOldSpells()) {
        /* Message */
        msg_format("You can learn some new %ss now.", p);
    }

    /* Save the new_spells value */
    p_ptr->SetOldSpells(p_ptr->GetNewSpells());
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_sp(void)
{
    int new_mana, levels, cur_wgt, max_wgt;
    CItem *i_ptr;
    bool old_cumber_glove = p_ptr->get_cumber_glove();
    bool old_cumber_armor = p_ptr->get_cumber_armor();


    /* Hack -- Must be literate */
    if (!mp_ptr->spell_book) return;


    /* Extract "effective" player level */
    levels = (p_ptr->GetLev() - mp_ptr->spell_first) + 1;

    /* Hack -- no negative mana */
    if (levels < 0) levels = 0;

    /* Extract total mana */
    new_mana = adj_mag_sp[p_ptr->GetStatInd(mp_ptr->spell_stat)]*levels/2;

    /* Hack -- usually add one mana */
    if (new_mana) new_mana++;


    /* Only mages are affected */
    if (mp_ptr->spell_book == TV_MAGIC_BOOK) {
        u32b f1, f2, f3;

        /* Assume player is not encumbered by gloves */
        p_ptr->set_cumber_glove(FALSE);

        /* Get the gloves */
        i_ptr = &inventory[INVEN_HANDS];

        /* Examine the gloves */
        i_ptr->GetFlags(&f1, &f2, &f3);

        /* Normal gloves hurt mage-type spells */
        if (i_ptr->exists() && !(f2 & TR2_FREE_ACT) &&
            !((f1 & TR1_DEX) && (i_ptr->GetPval() > 0)))
        {
            /* Encumbered */
            p_ptr->set_cumber_glove(TRUE);
        }
    }


    /* Assume player not encumbered by armor */
    p_ptr->set_cumber_armor(FALSE);

    /* Weigh the armor */
    cur_wgt = 0;
    cur_wgt += inventory[INVEN_BODY].GetWeight();
    cur_wgt += inventory[INVEN_HEAD].GetWeight();
    cur_wgt += inventory[INVEN_ARM].GetWeight();
    cur_wgt += inventory[INVEN_OUTER].GetWeight();
    cur_wgt += inventory[INVEN_HANDS].GetWeight();
    cur_wgt += inventory[INVEN_FEET].GetWeight();

    /* Determine the weight allowance */
    max_wgt = mp_ptr->spell_weight;

    /* Heavy armor penalizes mana */
    if (((cur_wgt - max_wgt) / 10) > 0) {
        /* Encumbered */
        p_ptr->set_cumber_armor(TRUE);

        /* Reduce mana */
        new_mana -= ((cur_wgt - max_wgt) / 10);
    }


    /* Mana can never be negative */
    if (new_mana < 0) new_mana = 0;


    /* Maximum mana has changed */
    if (p_ptr->GetMSP() != new_mana) {
        /* Save new mana */
        p_ptr->SetMSP(new_mana);

        /* Check for SP overflow */
        if (p_ptr->GetCSP() > p_ptr->GetMSP()) {
            p_ptr->SetCSP(p_ptr->GetMSP());
            p_ptr->SetCSPFrac(0);
        }
    }


    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Take note when encumbrance by gloves changes */
    if (old_cumber_glove != p_ptr->get_cumber_glove()) {
        /* Message */
        if (p_ptr->get_cumber_glove()) {
            msg_print("Your covered hands feel unsuitable for spellcasting.");
        }
        else {
            msg_print("Your hands feel more suitable for spellcasting.");
        }
    }


    /* Take note when encumbrance by armor changes */
    if (old_cumber_armor != p_ptr->get_cumber_armor()) {
        /* Message */
        if (p_ptr->get_cumber_armor()) {
            msg_print("The weight of your armor encumbers your movement.");
        }
        else {
            msg_print("You feel able to move more freely.");
        }
    }
}



/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints()
{
    int bonus, mhp;

    /* Un-inflate "half-hitpoint bonus per level" value */
    bonus = ((int)(adj_con_mhp[p_ptr->GetStatInd(STAT_CON)]) - 128);

    /* Calculate hitpoints */
    mhp = p_ptr->player_hp[p_ptr->GetLev()-1] + (bonus * p_ptr->GetLev() / 2);

    /* Always have at least one hitpoint per level */
    if (mhp < p_ptr->GetLev() + 1) mhp = p_ptr->GetLev() + 1;

    /* Factor in the hero / superhero settings */
    if (p_ptr->GetHero()) mhp += 10;
    if (p_ptr->GetSHero()) mhp += 30;

    /* New maximum hitpoints */
    if (mhp != p_ptr->GetMHP()) {
        /* Save the new max-hitpoints */
        p_ptr->SetMHP(mhp);

        /* Check for HP overflow */
        p_ptr->correct_hp_overflows();
    }
}



/*
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
    CItem *i_ptr = &inventory[INVEN_LITE];
    int old_lite = p_ptr->get_cur_lite();

    /* Assume no light */
    p_ptr->set_cur_lite(0);

    /* Examine actual lites */
    if (i_ptr->GetTval() == TV_LITE) {
        /* Torches (with fuel) provide some lite */
        if ((i_ptr->GetSval() == SV_LITE_TORCH) && (i_ptr->GetPval() > 0)) {
            p_ptr->set_cur_lite(1);
        }

        /* Lanterns (with fuel) provide more lite */
        if ((i_ptr->GetSval() == SV_LITE_LANTERN) && (i_ptr->GetPval() > 0)) {
            p_ptr->set_cur_lite(2);
        }

        /* Artifact Lites provide permanent, bright, lite */
        if (i_ptr->isArtifact()) p_ptr->set_cur_lite(3);
    }

    // Increase by various other permalight objects
    p_ptr->set_cur_lite(p_ptr->get_cur_lite() + p_ptr->get_lite());

    // Notice changes in the "lite radius"
    if (old_lite != p_ptr->get_cur_lite()) {
        // Update the lite and all monsters
        p_ptr->set_update(p_ptr->get_update() | PU_LITE | PU_MONSTERS);
    }
}



/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
    int i;

    /* Weight limit based only on strength */
    i = adj_str_wgt[p_ptr->GetStatInd(STAT_STR)] * 100;

    /* Return the result */
    return i;
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_sp() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
static void calc_bonuses(void)
{
    int i, j, hold;
    int old_telepathy, old_see_inv;
    int extra_blows, extra_shots;
    bool old_heavy_wield = p_ptr->get_heavy_wield();
    bool old_heavy_shoot = p_ptr->get_heavy_shoot();
    bool old_icky_wield = p_ptr->get_icky_wield();
    CItem *i_ptr;
    u32b f1, f2, f3;


    /* Save the old vision stuff */
    old_telepathy = p_ptr->get_telepathy();
    old_see_inv = p_ptr->get_see_inv();


    /* Clear extra blows/shots */
    extra_blows = extra_shots = 0;

    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) p_ptr->set_stat_add(i, 0);


    // Clear the Displayed/Real attack/defense values
    p_ptr->set_dis_to_h(0);
    p_ptr->set_to_h(0);
    p_ptr->set_dis_to_d(0);
    p_ptr->set_to_d(0);
    p_ptr->set_dis_to_a(0);
    p_ptr->set_to_a(0);
    p_ptr->set_dis_ac(0);
    p_ptr->set_ac(0);


    /* Clear all the flags */
    p_ptr->set_aggravate(FALSE);
    p_ptr->set_teleport(FALSE);
    p_ptr->set_exp_drain(FALSE);
    p_ptr->set_bless_blade(FALSE);
    p_ptr->set_xtra_might(FALSE);
    p_ptr->set_impact(FALSE);
    p_ptr->set_see_inv(FALSE);
    p_ptr->set_free_act(FALSE);
    p_ptr->set_slow_digest(FALSE);
    p_ptr->set_regenerate(FALSE);
    p_ptr->set_ffall(FALSE);
    p_ptr->set_hold_life(FALSE);
    p_ptr->set_telepathy(FALSE);
    p_ptr->set_lite(0);
    for (i = 0; i < MAX_IMMUNE; i++) p_ptr->set_immunes(i, FALSE);
    for (i = 0; i < MAX_RESIST; i++) p_ptr->set_resists(i, FALSE);
    for (i = 0; i < 6; i++) p_ptr->set_sustains(i, FALSE);



    /* Base infravision (purely racial) */
    p_ptr->set_see_infra(rp_ptr->infra);


    // Set all skills to base levels
    p_ptr->SetSkill(SKILL_DIS, rp_ptr->r_dis + cp_ptr->c_dis);
    p_ptr->SetSkill(SKILL_DEV, rp_ptr->r_dev + cp_ptr->c_dev);
    p_ptr->SetSkill(SKILL_SAV, rp_ptr->r_sav + cp_ptr->c_sav);
    p_ptr->SetSkill(SKILL_STL, rp_ptr->r_stl + cp_ptr->c_stl);
    p_ptr->SetSkill(SKILL_SRH, rp_ptr->r_srh + cp_ptr->c_srh);
    p_ptr->SetSkill(SKILL_FOS, rp_ptr->r_fos + cp_ptr->c_fos);
    p_ptr->SetSkill(SKILL_THN, rp_ptr->r_thn + cp_ptr->c_thn);
    p_ptr->SetSkill(SKILL_THB, rp_ptr->r_thb + cp_ptr->c_thb);
    p_ptr->SetSkill(SKILL_THT, rp_ptr->r_thb + cp_ptr->c_thb);
    p_ptr->SetSkill(SKILL_DIG, 0);


    // Race-specific abilities
    switch (p_ptr->GetRace()) {
        case RACE_ELF: p_ptr->set_resists(RESIST_LITE, TRUE); break;

        case RACE_HOBBIT: p_ptr->set_sustains(STAT_DEX, TRUE); break;

        case RACE_GNOME: p_ptr->set_free_act(TRUE); break;

        case RACE_DWARF: p_ptr->set_resists(RESIST_BLIND, TRUE); break;

        case RACE_HALF_ORC: p_ptr->set_resists(RESIST_DARK, TRUE); break;

        case RACE_HALF_TROLL: p_ptr->set_sustains(STAT_STR, TRUE); break;

        case RACE_DUNADAN: p_ptr->set_sustains(STAT_CON, TRUE); break;

        case RACE_HIGH_ELF:
            p_ptr->set_resists(RESIST_LITE, TRUE);
            p_ptr->set_see_inv(TRUE);
            break;
    }


    /* Start with "normal" speed */
    p_ptr->set_speed(110);

    // Start with a single blow and shot per turn
    p_ptr->set_num_blow(1);
    p_ptr->set_num_fire(1);

    /* Reset the "xtra" tval */
    p_ptr->set_tval_xtra(0);

    /* Reset the "ammo" tval */
    p_ptr->set_tval_ammo(0);


    /* Apply the racial modifiers */
    for (i = 0; i < 6; i++) {
        /* Modify the stats for "race" */
        p_ptr->set_stat_add(i,
            p_ptr->GetStatAdd(i) + rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
    }


    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
        i_ptr = &inventory[i];

        // Skip missing items
        if (!i_ptr->exists()) continue;

        // Extract the item flags
        i_ptr->GetFlags(&f1, &f2, &f3);

        // Affect stats
        if (f1 & TR1_STR) p_ptr->set_stat_add(STAT_STR,
            p_ptr->GetStatAdd(STAT_STR) + i_ptr->GetPval());
        if (f1 & TR1_INT) p_ptr->set_stat_add(STAT_INT,
            p_ptr->GetStatAdd(STAT_INT) + i_ptr->GetPval());
        if (f1 & TR1_WIS) p_ptr->set_stat_add(STAT_WIS,
            p_ptr->GetStatAdd(STAT_WIS) + i_ptr->GetPval());
        if (f1 & TR1_DEX) p_ptr->set_stat_add(STAT_DEX,
            p_ptr->GetStatAdd(STAT_DEX) + i_ptr->GetPval());
        if (f1 & TR1_CON) p_ptr->set_stat_add(STAT_CON,
            p_ptr->GetStatAdd(STAT_CON) + i_ptr->GetPval());
        if (f1 & TR1_CHR) p_ptr->set_stat_add(STAT_CHR,
            p_ptr->GetStatAdd(STAT_CHR) + i_ptr->GetPval());

        // Affect stealth
        if (f1 & TR1_STEALTH) {
            p_ptr->SetSkill(SKILL_STL, p_ptr->GetSkill(SKILL_STL) + i_ptr->GetPval());
        }

        /* Affect searching ability (factor of five) */
        if (f1 & TR1_SEARCH) {
            p_ptr->SetSkill(SKILL_SRH, p_ptr->GetSkill(SKILL_SRH) + i_ptr->GetPval()*5);
        }

        /* Affect searching frequency (factor of five) */
        if (f1 & TR1_SEARCH) {
            p_ptr->SetSkill(SKILL_FOS, p_ptr->GetSkill(SKILL_FOS) + i_ptr->GetPval()*5);
        }

        /* Affect infravision */
        if (f1 & TR1_INFRA) {
            p_ptr->set_see_infra(p_ptr->get_see_infra() + i_ptr->GetPval());
        }

        /* Affect digging (factor of 20) */
        if (f1 & TR1_TUNNEL) {
            p_ptr->SetSkill(SKILL_DIG, p_ptr->GetSkill(SKILL_DIG) + i_ptr->GetPval()*20);
        }

        /* Affect speed */
        if (f1 & TR1_SPEED) p_ptr->set_speed(p_ptr->get_speed() + i_ptr->GetPval());

        /* Affect blows */
        if (f1 & TR1_BLOWS) extra_blows += i_ptr->GetPval();

        /* Hack -- cause earthquakes */
        if (f1 & TR1_IMPACT) p_ptr->set_impact(TRUE);

        /* Boost shots */
        if (f3 & TR3_XTRA_SHOTS) extra_shots++;

        /* Various flags */
        if (f3 & TR3_AGGRAVATE) p_ptr->set_aggravate(TRUE);
        if (f3 & TR3_TELEPORT) p_ptr->set_teleport(TRUE);
        if (f3 & TR3_DRAIN_EXP) p_ptr->set_exp_drain(TRUE);
        if (f3 & TR3_BLESSED) p_ptr->set_bless_blade(TRUE);
        if (f3 & TR3_XTRA_MIGHT) p_ptr->set_xtra_might(TRUE);
        if (f3 & TR3_SLOW_DIGEST) p_ptr->set_slow_digest(TRUE);
        if (f3 & TR3_REGEN) p_ptr->set_regenerate(TRUE);
        if (f3 & TR3_TELEPATHY) p_ptr->set_telepathy(TRUE);
        if (f3 & TR3_LITE) p_ptr->set_lite(p_ptr->get_lite() + 1);
        if (f3 & TR3_SEE_INVIS) p_ptr->set_see_inv(TRUE);
        if (f3 & TR3_FEATHER) p_ptr->set_ffall(TRUE);
        if (f2 & TR2_FREE_ACT) p_ptr->set_free_act(TRUE);
        if (f2 & TR2_HOLD_LIFE) p_ptr->set_hold_life(TRUE);

        /* Immunity flags */
        if (f2 & TR2_IM_FIRE) p_ptr->set_immunes(IMMUNE_FIRE, TRUE);
        if (f2 & TR2_IM_ACID) p_ptr->set_immunes(IMMUNE_ACID, TRUE);
        if (f2 & TR2_IM_COLD) p_ptr->set_immunes(IMMUNE_COLD, TRUE);
        if (f2 & TR2_IM_ELEC) p_ptr->set_immunes(IMMUNE_ELEC, TRUE);

        /* Resistance flags */
        if (f2 & TR2_RES_ACID) p_ptr->set_resists(RESIST_ACID, TRUE);
        if (f2 & TR2_RES_ELEC) p_ptr->set_resists(RESIST_ELEC, TRUE);
        if (f2 & TR2_RES_FIRE) p_ptr->set_resists(RESIST_FIRE, TRUE);
        if (f2 & TR2_RES_COLD) p_ptr->set_resists(RESIST_COLD, TRUE);
        if (f2 & TR2_RES_POIS) p_ptr->set_resists(RESIST_POIS, TRUE);
        if (f2 & TR2_RES_CONF) p_ptr->set_resists(RESIST_CONF, TRUE);
        if (f2 & TR2_RES_SOUND) p_ptr->set_resists(RESIST_SOUND, TRUE);
        if (f2 & TR2_RES_LITE) p_ptr->set_resists(RESIST_LITE, TRUE);
        if (f2 & TR2_RES_DARK) p_ptr->set_resists(RESIST_DARK, TRUE);
        if (f2 & TR2_RES_CHAOS) p_ptr->set_resists(RESIST_CHAOS, TRUE);
        if (f2 & TR2_RES_DISEN) p_ptr->set_resists(RESIST_DISEN, TRUE);
        if (f2 & TR2_RES_SHARDS) p_ptr->set_resists(RESIST_SHARD, TRUE);
        if (f2 & TR2_RES_NEXUS) p_ptr->set_resists(RESIST_NEXUS, TRUE);
        if (f2 & TR2_RES_BLIND) p_ptr->set_resists(RESIST_BLIND, TRUE);
        if (f2 & TR2_RES_NETHER) p_ptr->set_resists(RESIST_NETH, TRUE);

        /* Sustain flags */
        if (f2 & TR2_SUST_STR) p_ptr->set_sustains(STAT_STR, TRUE);
        if (f2 & TR2_SUST_INT) p_ptr->set_sustains(STAT_INT, TRUE);
        if (f2 & TR2_SUST_WIS) p_ptr->set_sustains(STAT_WIS, TRUE);
        if (f2 & TR2_SUST_DEX) p_ptr->set_sustains(STAT_DEX, TRUE);
        if (f2 & TR2_SUST_CON) p_ptr->set_sustains(STAT_CON, TRUE);
        if (f2 & TR2_SUST_CHR) p_ptr->set_sustains(STAT_CHR, TRUE);

        /* Modify the base armor class */
        p_ptr->set_ac(p_ptr->get_ac() + i_ptr->GetAC());

        /* The base armor class is always known */
        p_ptr->set_dis_ac(p_ptr->get_dis_ac() + i_ptr->GetAC());

        /* Apply the bonuses to armor class */
        p_ptr->set_to_a(p_ptr->get_to_a() + i_ptr->GetToA());

        /* Apply the mental bonuses to armor class, if known */
        if (i_ptr->isKnown()) {
            p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() + i_ptr->GetToA());
        }

        /* Hack -- do not apply "weapon" bonuses */
        if (i == INVEN_WIELD) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (i == INVEN_BOW) continue;

        /* Apply the bonuses to hit/damage */
        p_ptr->set_to_h(p_ptr->get_to_h() + i_ptr->GetToH());
        p_ptr->set_to_d(p_ptr->get_to_d() + i_ptr->GetToD());

        /* Apply the mental bonuses tp hit/damage, if known */
        if (i_ptr->isKnown()) {
            p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + i_ptr->GetToH());
            p_ptr->set_dis_to_d(p_ptr->get_dis_to_d() + i_ptr->GetToD());
        }
    }


    /* Calculate stats */
    for (i = 0; i < 6; i++) {
        int top, use, ind;


        /* Extract the new "stat_use" value for the stat */
        top = modify_stat_value(p_ptr->GetStatMax(i), p_ptr->GetStatAdd(i));

        /* Notice changes */
        if (p_ptr->GetStatTop(i) != top) {
            /* Save the new value */
            p_ptr->set_stat_top(i, top);
        }


        // Extract the new "stat_use" value for the stat
        use = modify_stat_value(p_ptr->GetStatCur(i), p_ptr->GetStatAdd(i));

        /* Notice changes */
        if (p_ptr->GetStatUse(i) != use) {
            /* Save the new value */
            p_ptr->set_stat_use(i, use);
        }


        /* Values: 3, 4, ..., 17 */
        if (use <= 18) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (37);

        /* Notice changes */
        if (p_ptr->GetStatInd(i) != ind) {
            // Save the new index
            p_ptr->set_stat_ind(i, ind);

            // Change in CON affects Hitpoints
            if (i == STAT_CON) {
                p_ptr->set_update(p_ptr->get_update() | PU_HP);
            }

            // Change in INT may affect Mana/Spells
            else if (i == STAT_INT) {
                if (mp_ptr->spell_stat == STAT_INT) {
                    p_ptr->set_update(p_ptr->get_update() | PU_MANA | PU_SPELLS);
                }
            }

            // Change in WIS may affect Mana/Spells
            else if (i == STAT_WIS) {
                if (mp_ptr->spell_stat == STAT_WIS) {
                    p_ptr->set_update(p_ptr->get_update() | PU_MANA | PU_SPELLS);
                }
            }
        }
    }


    /* Apply temporary "stun" */
    if (p_ptr->GetStun() > 50) {
        p_ptr->set_to_h(p_ptr->get_to_h() - 20);
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() - 20);
        p_ptr->set_to_d(p_ptr->get_to_d() - 20);
        p_ptr->set_dis_to_d(p_ptr->get_dis_to_d() - 20);
    }
    else if (p_ptr->GetStun()) {
        p_ptr->set_to_h(p_ptr->get_to_h() - 5);
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() - 5);
        p_ptr->set_to_d(p_ptr->get_to_d() - 5);
        p_ptr->set_dis_to_d(p_ptr->get_dis_to_d() - 5);
    }


    // Shadowform
    if (p_ptr->GetShadowform()) {
        p_ptr->set_to_a(p_ptr->get_to_a() + 100);
        p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() + 100);
    }

    // Temporary blessing
    if (p_ptr->GetBlessed()) {
        p_ptr->set_to_a(p_ptr->get_to_a() + 5);
        p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() + 5);
        p_ptr->set_to_h(p_ptr->get_to_h() + 10);
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + 10);
    }

    /* Temprory shield */
    if (p_ptr->GetShield()) {
        p_ptr->set_to_a(p_ptr->get_to_a() + 50);
        p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() + 50);
    }

    /* Temporary "Hero" */
    if (p_ptr->GetHero()) {
        p_ptr->set_to_h(p_ptr->get_to_h() + 12);
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + 12);
    }

    // Temporary "Berserk"
    if (p_ptr->GetSHero()) {
        p_ptr->set_to_h(p_ptr->get_to_h() + 24);
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + 24);
        p_ptr->set_to_a(p_ptr->get_to_a() - 10);
        p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() - 10);
    }

    // Temporary "fast"
    if (p_ptr->GetFast()) {
        p_ptr->set_speed(p_ptr->get_speed() + 10);
    }

    // Temporary "slow"
    if (p_ptr->GetSlow()) {
        p_ptr->set_speed(p_ptr->get_speed() - 10);
    }

    // Temporary see invisible
    if (p_ptr->GetTimInvis()) {
        p_ptr->set_see_inv(TRUE);
    }

    // Temporary infravision boost
    if (p_ptr->GetTimInfra()) {
        p_ptr->set_see_infra(p_ptr->get_see_infra() + 1);
    }


    // Hack -- Res Chaos -> Res Conf
    if (p_ptr->get_resists(RESIST_CHAOS)) {
        p_ptr->set_resists(RESIST_CONF, TRUE);
    }

    // Hack -- Hero/Shero -> Res fear
    if (p_ptr->GetHero() || p_ptr->GetSHero()) {
        p_ptr->set_resists(RESIST_FEAR, TRUE);
    }


    // Hack -- Telepathy Change
    if (p_ptr->get_telepathy() != old_telepathy) {
        p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
    }

    // Hack -- See Invis Change
    if (p_ptr->get_see_inv() != old_see_inv) {
        p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
    }


    // Extract the current weight (in tenth pounds)
    j = p_ptr->GetTotalWeight();

    // Extract the "weight limit" (in tenth pounds)
    i = weight_limit();

    // XXX XXX XXX Apply "encumbrance" from weight
    if (j > i/2) p_ptr->set_speed(p_ptr->get_speed() - ((j - i/2) / (i/10)));

    // Bloating slows the player down
    if (p_ptr->GetFood() >= PY_FOOD_MAX) p_ptr->set_speed(p_ptr->get_speed() - 10);


    // Actual Modifier Bonuses (Un-inflate stat bonuses)
    p_ptr->set_to_a(p_ptr->get_to_a() +
        ((int)(adj_dex_ta[p_ptr->GetStatInd(STAT_DEX)]) - 128));
    p_ptr->set_to_d(p_ptr->get_to_d() +
        ((int)(adj_str_td[p_ptr->GetStatInd(STAT_STR)]) - 128));
    p_ptr->set_to_h(p_ptr->get_to_h() +
        ((int)(adj_dex_th[p_ptr->GetStatInd(STAT_DEX)]) - 128) +
        ((int)(adj_str_th[p_ptr->GetStatInd(STAT_STR)]) - 128));

    /* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
    p_ptr->set_dis_to_a(p_ptr->get_dis_to_a() +
        ((int)(adj_dex_ta[p_ptr->GetStatInd(STAT_DEX)]) - 128));
    p_ptr->set_dis_to_d(p_ptr->get_dis_to_d() +
        ((int)(adj_str_td[p_ptr->GetStatInd(STAT_STR)]) - 128));
    p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() +
        ((int)(adj_dex_th[p_ptr->GetStatInd(STAT_DEX)]) - 128) +
        ((int)(adj_str_th[p_ptr->GetStatInd(STAT_STR)]) - 128));



    // Obtain the "hold" value
    hold = adj_str_hold[p_ptr->GetStatInd(STAT_STR)];


    // Examine the "current bow"
    i_ptr = &inventory[INVEN_BOW];


    // Assume not heavy
    p_ptr->set_heavy_shoot(FALSE);

    // It is hard to hold a heavy bow
    if (hold < i_ptr->GetWeight() / 10) {
        // Hard to wield a heavy bow
        p_ptr->set_to_h(p_ptr->get_to_h() + 2 * (hold - i_ptr->GetWeight() / 10));
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + 2 * (hold - i_ptr->GetWeight() / 10));

        // Heavy Bow
        p_ptr->set_heavy_shoot(TRUE);
    }


    // Compute "extra shots" if needed
    if (i_ptr->exists()) {
        /* Take note of required "tval" for missiles */
        switch (i_ptr->GetSval()) {
            case SV_SLING:
                p_ptr->set_tval_ammo(TV_SHOT);
                break;

            case SV_SHORT_BOW:
            case SV_LONG_BOW:
                p_ptr->set_tval_ammo(TV_ARROW);
                break;

            case SV_LIGHT_XBOW:
            case SV_HEAVY_XBOW:
                p_ptr->set_tval_ammo(TV_BOLT);
                break;
        }

        // No extra shots if you have a heavy bow
        if (!p_ptr->get_heavy_shoot()) {
            // Hack -- Reward High Level Rangers using Bows
            if ((p_ptr->GetClass() == CLASS_RANGER) && (p_ptr->get_tval_ammo() == TV_ARROW)) {
                // Extra shot at level 20
                if (p_ptr->GetLev() >= 20) p_ptr->set_num_fire(p_ptr->get_num_fire() + 1);

                // Extra shot at level 40
                if (p_ptr->GetLev() >= 40) p_ptr->set_num_fire(p_ptr->get_num_fire() + 1);
            }

            // Add in the "bonus shots"
            p_ptr->set_num_fire(p_ptr->get_num_fire() + extra_shots);

            // Require at least one shot
            if (p_ptr->get_num_fire() < 1) p_ptr->set_num_fire(1);
        }
    }



    // Examine the "main weapon"
    i_ptr = &inventory[INVEN_WIELD];


    // Assume not heavy
    p_ptr->set_heavy_wield(FALSE);

    // It is hard to hold a heavy weapon
    if (hold < i_ptr->GetWeight() / 10) {
        // Hard to wield a heavy weapon
        p_ptr->set_to_h(p_ptr->get_to_h() + 2*(hold - i_ptr->GetWeight() / 10));
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() + 2*(hold - i_ptr->GetWeight() / 10));

        // Heavy weapon
        p_ptr->set_heavy_wield(TRUE);
    }


    // Normal weapons
    if (i_ptr->exists() && !p_ptr->get_heavy_wield()) {
        int str_index, dex_index;

        int num = 0, wgt = 0, mul = 0, div = 0;

        // Analyze the class
        switch (p_ptr->GetClass()) {
            // Warrior
            case CLASS_WARRIOR: num = 6; wgt = 30; mul = 5; break;

            // Mage
            case CLASS_MAGE:    num = 4; wgt = 40; mul = 2; break;

            // Priest
            case CLASS_PRIEST:  num = 5; wgt = 35; mul = 3; break;

            // Rogue
            case CLASS_ROGUE:   num = 5; wgt = 30; mul = 3; break;

            // Ranger
            case CLASS_RANGER:  num = 5; wgt = 35; mul = 4; break;

            // Paladin
            case CLASS_PALADIN: num = 5; wgt = 30; mul = 4; break;
        }

        // Enforce a minimum "weight" (tenth pounds)
        div = (i_ptr->GetWeight() < wgt) ? wgt : i_ptr->GetWeight();

        // Access the strength vs weight
        str_index = adj_str_blow[p_ptr->GetStatInd(STAT_STR)] * mul / div;

        /* Maximal value */
        if (str_index > 11) str_index = 11;

        /* Index by dexterity */
        dex_index = adj_dex_blow[p_ptr->GetStatInd(STAT_DEX)];

        /* Maximal value */
        if (dex_index > 11) dex_index = 11;

        /* Use the blows table */
        p_ptr->set_num_blow(blows_table[str_index][dex_index]);

        /* Maximal value */
        if (p_ptr->get_num_blow() > num) p_ptr->set_num_blow(num);

        /* Add in the "bonus blows" */
        p_ptr->set_num_blow(p_ptr->get_num_blow() + extra_blows);

        /* Require at least one blow */
        if (p_ptr->get_num_blow() < 1) p_ptr->set_num_blow(1);

        /* Boost digging skill by weapon weight */
        p_ptr->SetSkill(SKILL_DIG, p_ptr->GetSkill(SKILL_DIG) + i_ptr->GetWeight() / 10);
    }


    /* Assume okay */
    p_ptr->set_icky_wield(FALSE);

    /* Priest weapon penalty for non-blessed edged weapons */
    if ((p_ptr->GetClass() == CLASS_PRIEST) && !p_ptr->get_bless_blade() &&
        ((i_ptr->GetTval() == TV_SWORD) || (i_ptr->GetTval() == TV_POLEARM)))
    {
        /* Reduce the real bonuses */
        p_ptr->set_to_h(p_ptr->get_to_h() - 2);
        p_ptr->set_to_d(p_ptr->get_to_d() - 2);

        /* Reduce the mental bonuses */
        p_ptr->set_dis_to_h(p_ptr->get_dis_to_h() - 2);
        p_ptr->set_dis_to_d(p_ptr->get_dis_to_d() - 2);

        // Icky weapon
        p_ptr->set_icky_wield(TRUE);
    }


    // Affect Skill -- stealth (bonus one)
    p_ptr->AddSkill(SKILL_STL, 1);

    // Affect Skill -- disarming (DEX and INT)
    p_ptr->SetSkill(SKILL_DIS, p_ptr->GetSkill(SKILL_DIS) +
        adj_dex_dis[p_ptr->GetStatInd(STAT_DEX)] +
        adj_int_dis[p_ptr->GetStatInd(STAT_INT)]);

    // Affect Skill -- magic devices (INT)
    p_ptr->AddSkill(SKILL_DEV, adj_int_dev[p_ptr->GetStatInd(STAT_INT)]);

    // Affect Skill -- saving throw (WIS)
    p_ptr->AddSkill(SKILL_SAV, adj_wis_sav[p_ptr->GetStatInd(STAT_WIS)]);

    // Affect Skill -- digging (STR)
    p_ptr->AddSkill(SKILL_DIG, adj_str_dig[p_ptr->GetStatInd(STAT_STR)]);


    // Affect skills by level and class
    p_ptr->AddSkill(SKILL_DIS, cp_ptr->x_dis * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_DEV, cp_ptr->x_dev * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_SAV, cp_ptr->x_sav * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_STL, cp_ptr->x_stl * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_SRH, cp_ptr->x_srh * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_FOS, cp_ptr->x_fos * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_THN, cp_ptr->x_thn * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_THB, cp_ptr->x_thb * p_ptr->GetLev() / 10);
    p_ptr->AddSkill(SKILL_THT, cp_ptr->x_thb * p_ptr->GetLev() / 10);


    // Limit Skill -- stealth from 0 to 30
    if (p_ptr->GetSkill(SKILL_STL) > 30) p_ptr->SetSkill(SKILL_STL, 30);
    if (p_ptr->GetSkill(SKILL_STL) < 0) p_ptr->SetSkill(SKILL_STL, 0);

    // Limit Skill -- digging from 1 up
    if (p_ptr->GetSkill(SKILL_DIG) < 1) p_ptr->SetSkill(SKILL_DIG, 1);


    // Hack -- handle "xtra" mode
    if (character_xtra) return;

    // Take note when "heavy bow" changes
    if (old_heavy_shoot != p_ptr->get_heavy_shoot()) {
        /* Message */
        if (p_ptr->get_heavy_shoot()) {
            msg_print("You have trouble wielding such a heavy bow.");
        }
        else if (inventory[INVEN_BOW].exists()) {
            msg_print("You have no trouble wielding your bow.");
        }
        else {
            msg_print("You feel relieved to put down your heavy bow.");
        }
    }


    /* Take note when "heavy weapon" changes */
    if (old_heavy_wield != p_ptr->get_heavy_wield()) {
        /* Message */
        if (p_ptr->get_heavy_wield()) {
            msg_print("You have trouble wielding such a heavy weapon.");
        }
        else if (inventory[INVEN_WIELD].exists()) {
            msg_print("You have no trouble wielding your weapon.");
        }
        else {
            msg_print("You feel relieved to put down your heavy weapon.");
        }
    }


    /* Take note when "illegal weapon" changes */
    if (old_icky_wield != p_ptr->get_icky_wield()) {
        /* Message */
        if (p_ptr->get_icky_wield()) {
            msg_print("You do not feel comfortable with your weapon.");
        }
        else if (inventory[INVEN_WIELD].exists()) {
            msg_print("You feel comfortable with your weapon.");
        }
        else {
            msg_print("You feel more comfortable after removing your weapon.");
        }
    }
}



/*
 * Handle notice flags
 */
void notice_stuff(void)
{
    // Need to notice stuff?
    if (!p_ptr->get_notice()) return;

    // Combine the pack
    if (p_ptr->get_notice() & PN_COMBINE) p_ptr->combine_pack();

    // Reorder the pack
    if (p_ptr->get_notice() & PN_REORDER) p_ptr->reorder_pack();

    // Clear notice flag
    p_ptr->set_notice(0);
}


/*
 * Handle update flags
 */
void update_stuff(void)
{
    /* Update stuff */
    if (!p_ptr->get_update()) return;


    if (p_ptr->get_update() & PU_BONUS) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_BONUS);
        calc_bonuses();
    }

    if (p_ptr->get_update() & PU_TORCH) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_TORCH);
        calc_torch();
    }

    if (p_ptr->get_update() & PU_HP) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_HP);
        calc_hitpoints();
    }

    if (p_ptr->get_update() & PU_MANA) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_MANA);
        calc_sp();
    }

    if (p_ptr->get_update() & PU_SPELLS) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_SPELLS);
        calc_spells();
    }


    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;


    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;


    if (p_ptr->get_update() & PU_UN_LITE) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_UN_LITE);
        forget_lite();
    }

    if (p_ptr->get_update() & PU_UN_VIEW) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_UN_VIEW);
        forget_view();
    }


    if (p_ptr->get_update() & PU_VIEW) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_VIEW);
        update_view();
    }

    if (p_ptr->get_update() & PU_LITE) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_LITE);
        update_lite();
    }


    if (p_ptr->get_update() & PU_DISTANCE) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_DISTANCE);
        p_ptr->set_update(p_ptr->get_update() & ~PU_MONSTERS);
        update_monsters();
    }

    if (p_ptr->get_update() & PU_MONSTERS) {
        p_ptr->set_update(p_ptr->get_update() & ~PU_MONSTERS);
        update_monsters();
    }
}