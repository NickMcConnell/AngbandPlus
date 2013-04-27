// File: melee1.c
// Purpose: Monster melee attacks

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam)
{
    int max = 0;
    int total = dice * sides;

    /* Must do at least 95% of perfect */
    if (dam < total * 19 / 20) return 0;

    /* Weak blows rarely work */
    if ((dam < 20) && !percent(dam)) return 0;

    /* Perfect damage */
    if (dam == total) max++;

    /* Super-charge */
    if (dam >= 20) {
        while (percent(2)) max++;
    }

    /* Critical damage */
    if (dam > 45) return (6 + max);
    if (dam > 33) return (5 + max);
    if (dam > 25) return (4 + max);
    if (dam > 18) return (3 + max);
    if (dam > 11) return (2 + max);
    return (1 + max);
}





/*
 * Hack -- possible "insult" messages
 */
static char *desc_insult[] = {
    "insults you!",
    "insults your mother!",
    "gives you the finger!",
    "humiliates you!",
    "defiles you!",
    "dances around you!",
    "makes obscene gestures!",
    "moons you!"
};



/*
 * Hack -- possible "insult" messages
 */
static char *desc_moan[] = {
    "seems sad about something.",
    "asks if you have seen his dogs.",
    "tells you to get off his land.",
    "mumbles something about mushrooms."
};


/*
 * Attack the player via physical attacks.
 */
bool CMonster::make_attack_normal(void)
{
    CMonsterRace *r_ptr = get_r_ptr();
    int ap_cnt, i, k, tmp, ac, rlev;
    int do_cut, do_stun;
    s32b gold;
    CItem *i_ptr;
    char i_name[80], m_name[80], ddesc[80];
    bool blinked;


    /* Not allowed to attack */
    if (r_ptr->flags1 & RF1_NEVER_BLOW) return FALSE;


    // Total armor
    ac = p_ptr->GetTotalAC();

    // Extract the effective monster level
    rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


    /* Get the monster name (or "it") */
    get_desc(m_name, 0);

    /* Get the "died from" information (i.e. "a kobold") */
    get_desc(ddesc, 0x88);


    /* Assume no blink */
    blinked = FALSE;

    /* Scan through all four blows */
    for (ap_cnt = 0; ap_cnt < 4; ap_cnt++) {
        bool visible = FALSE;
        bool obvious = FALSE;

        int power = 0;
        int damage = 0;

        char *act = NULL;

        /* Extract the attack infomation */
        int effect = r_ptr->blow[ap_cnt].effect;
        int method = r_ptr->blow[ap_cnt].method;
        int d_dice = r_ptr->blow[ap_cnt].d_dice;
        int d_side = r_ptr->blow[ap_cnt].d_side;


        /* Hack -- no more attacks */
        if (!method) break;


        /* Stop if player is dead or gone */
        if (!alive || death || new_level_flag) break;


        /* Extract visibility (before blink) */
        if (is_visible()) visible = TRUE;



        /* Extract the attack "power" */
        switch (effect) {
            case RBE_HURT:      power = 60; break;
            case RBE_POISON:    power =  5; break;
            case RBE_UN_BONUS:  power = 20; break;
            case RBE_UN_POWER:  power = 15; break;
            case RBE_EAT_GOLD:  power =  5; break;
            case RBE_EAT_ITEM:  power =  5; break;
            case RBE_EAT_FOOD:  power =  5; break;
            case RBE_EAT_LITE:  power =  5; break;
            case RBE_ACID:      power =  0; break;
            case RBE_ELEC:      power = 10; break;
            case RBE_FIRE:      power = 10; break;
            case RBE_COLD:      power = 10; break;
            case RBE_BLIND:     power =  2; break;
            case RBE_CONFUSE:   power = 10; break;
            case RBE_TERRIFY:   power = 10; break;
            case RBE_PARALYZE:  power =  2; break;
            case RBE_LOSE_STR:  power =  0; break;
            case RBE_LOSE_DEX:  power =  0; break;
            case RBE_LOSE_CON:  power =  0; break;
            case RBE_LOSE_INT:  power =  0; break;
            case RBE_LOSE_WIS:  power =  0; break;
            case RBE_LOSE_CHR:  power =  0; break;
            case RBE_LOSE_ALL:  power =  2; break;
            case RBE_SHATTER:   power = 60; break;
            case RBE_EXP_10:    power =  5; break;
            case RBE_EXP_20:    power =  5; break;
            case RBE_EXP_40:    power =  5; break;
            case RBE_EXP_80:    power =  5; break;
        }

        /* Monster is stunned */
        if (get_stun()) power -= 50;


        /* Monster hits player */
        if (!effect || test_hit(power + rlev*3, ac, TRUE)) {
            /* Hack -- Apply "protection from evil" */
            if ((p_ptr->GetProtevil() > 0) &&
                (r_ptr->flags3 & RF3_EVIL) &&
                (p_ptr->GetLev() >= rlev) &&
                percent(50 + p_ptr->GetLev()))
            {
                /* Remember the Evil-ness */
                if (is_visible()) r_ptr->r_flags3 |= RF3_EVIL;

                /* Message */
                msg_format("%^s is repelled.", m_name);

                /* Hack -- Next attack */
                continue;
            }


            /* Assume no cut or stun */
            do_cut = do_stun = 0;

            /* Describe the attack method */
            switch (method) {
                case RBM_HIT:
                    act = "hits you.";
                    do_cut = do_stun = 1;
                    break;

                case RBM_TOUCH:
                    act = "touches you.";
                    break;

                case RBM_PUNCH:
                    act = "punches you.";
                    do_stun = 1;
                    break;

                case RBM_KICK:
                    act = "kicks you.";
                    do_stun = 1;
                    break;

                case RBM_CLAW:
                    act = "claws you.";
                    do_cut = 1;
                    break;

                case RBM_BITE:
                    act = "bites you.";
                    do_cut = 1;
                    break;

                case RBM_STING:
                    act = "stings you.";
                    break;

                case RBM_XXX1:
                    act = "XXX1's you.";
                    break;

                case RBM_BUTT:
                    act = "butts you.";
                    do_stun = 1;
                    break;

                case RBM_CRUSH:
                    act = "crushes you.";
                    do_stun = 1;
                    break;

                case RBM_ENGULF:
                    act = "engulfs you.";
                    break;

                case RBM_XXX2:
                    act = "XXX2's you.";
                    break;

                case RBM_CRAWL:
                    act = "crawls on you.";
                    break;

                case RBM_DROOL:
                    act = "drools on you.";
                    break;

                case RBM_SPIT:
                    act = "spits on you.";
                    break;

                case RBM_XXX3:
                    act = "XXX3's on you.";
                    break;

                case RBM_GAZE:
                    act = "gazes at you.";
                    break;

                case RBM_WAIL:
                    act = "wails at you.";
                    break;

                case RBM_SPORE:
                    act = "releases spores at you.";
                    break;

                case RBM_XXX4:
                    act = "projects XXX4's at you.";
                    break;

                case RBM_BEG:
                    act = "begs you for money.";
                    break;

                case RBM_INSULT:
                    act = desc_insult[rand_int(8)];
                    break;

                case RBM_MOAN:
                    act = desc_moan[rand_int(4)];
                    break;

                case RBM_XXX5:
                    act = "XXX5's you.";
                    break;
            }

            /* Message */
            if (act) msg_format("%^s %s", m_name, act);


            /* Hack -- assume all attacks are obvious */
            obvious = TRUE;

            /* Roll out the damage */
            damage = damroll(d_dice, d_side);

            /* Monster is stunned */
            if (get_stun()) damage -= (damage / 10);

            /* Apply appropriate damage */
            switch (effect) {
                case 0:

                    /* Hack -- Assume obvious */
                    obvious = TRUE;

                    /* Hack -- No damage */
                    damage = 0;

                    break;

                case RBE_HURT:

                    /* Obvious */
                    obvious = TRUE;

                    /* Hack -- Player armor reduces total damage */
                    // XXX ummm, either apply this nowhere or more places -- MJC
                    damage -= (damage * ((ac < 150) ? ac : 150) / 250);

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    break;

                case RBE_POISON:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Take "poison" effect */
                    if (!(p_ptr->get_resists(RESIST_POIS) || p_ptr->GetOpposePois())) {
                        if (p_ptr->mod_poisoned(p_ptr->GetPoisoned() + randint(rlev) + 5)) {
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_UN_BONUS:

                    // Take some damage
                    p_ptr->take_hit(damage, ddesc);

                    // Allow complete resist
                    if (!p_ptr->get_resists(RESIST_DISEN)) {
                        // Apply disenchantment
                        if (apply_disenchant(0)) obvious = TRUE;
                    }

                    break;

                case RBE_UN_POWER:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Find an item */
                    for (k = 0; k < 10; k++) {
                        /* Pick an item */
                        i = rand_int(INVEN_PACK);

                        /* Obtain the item */
                        i_ptr = &inventory[i];

                        /* Drain charged wands/staffs */
                        if (((i_ptr->GetTval() == TV_STAFF) ||
                             (i_ptr->GetTval() == TV_WAND)) &&
                            (i_ptr->GetPval()))
                        {
                            // Message
                            msg_print("Energy drains from your pack!");

                            // Obvious
                            obvious = TRUE;

                            // Uncharge
                            i_ptr->SetPval(0);

                            // Combine / Reorder the pack
                            p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

                            // Heal
                            SetCHP(GetCHP() +
                                rlev * i_ptr->GetPval() * i_ptr->GetNumber());
                            correct_hp_overflows();

                            // Done
                            break;
                        }
                    }

                    break;

                case RBE_EAT_GOLD:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Obvious */
                    obvious = TRUE;

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p_ptr->GetParalyzed() &&
                        percent(adj_dex_safe[p_ptr->GetStatInd(STAT_DEX)] +
                                p_ptr->GetLev()))
                    {
                        /* Saving throw message */
                        msg_print("You quickly protect your money pouch!");

                        /* Occasional blink anyway */
                        if (rand_int(3)) blinked = TRUE;
                    }

                    // Eat gold
                    else {
                        gold = (p_ptr->GetGold() + randint(p_ptr->GetGold())) / 15;
                        if (gold < 2) gold = 2;
                        if (gold > p_ptr->GetGold()) gold = p_ptr->GetGold();
                        p_ptr->SetGold(p_ptr->GetGold() - gold);
                        if (gold <= 0) {
                            msg_print("Nothing was stolen.");
                        }
                        else if (p_ptr->GetGold()) {
                            msg_print("Your purse feels lighter.");
                            msg_format("%ld coins were stolen!", (long)gold);
                        }
                        else {
                            msg_print("Your purse feels lighter.");
                            msg_print("All of your coins were stolen!");
                        }
                        blinked = TRUE;
                    }

                    break;

                case RBE_EAT_ITEM:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p_ptr->GetParalyzed() &&
                        percent(adj_dex_safe[p_ptr->GetStatInd(STAT_DEX)] +
                                p_ptr->GetLev()))
                    {
                        // Saving throw message
                        msg_print("You grab hold of your backpack!");

                        /* Occasional "blink" anyway */
                        blinked = TRUE;

                        /* Obvious */
                        obvious = TRUE;

                        /* Done */
                        break;
                    }

                    /* Find an item */
                    for (k = 0; k < 10; k++) {
                        /* Pick an item */
                        i = rand_int(INVEN_PACK);

                        /* Obtain the item */
                        i_ptr = &inventory[i];

                        /* Accept real items */
                        if (!i_ptr->exists()) continue;

                        /* Don't steal artifacts  -CFT */
                        if (i_ptr->isArtifact()) continue;

                        /* Get a description */
                        i_ptr->object_desc(i_name, FALSE, 3);

                        /* Message */
                        msg_format("%sour %s (%c) was stolen!",
                            i_ptr->isPlural() ? "One of y" : "Y",
                            i_name, index_to_label(i));

                        /* Steal the items */
                        inven_item_increase(i, -1);
                        inven_item_optimize(i);

                        /* Obvious */
                        obvious = TRUE;

                        /* Blink away */
                        blinked = TRUE;

                        /* Done */
                        break;
                    }

                    break;

                case RBE_EAT_FOOD:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Steal some food */
                    for (k = 0; k < 10; k++) {
                        /* Pick an item from the pack */
                        i = rand_int(INVEN_PACK);

                        /* Get the item */
                        i_ptr = &inventory[i];

                        /* Accept real items */
                        if (!i_ptr->exists()) continue;

                        /* Only eat food */
                        if (i_ptr->GetTval() != TV_FOOD) continue;

                        /* Get a description */
                        i_ptr->object_desc(i_name, FALSE, 0);

                        /* Message */
                        msg_format("%sour %s (%c) was eaten!",
                            i_ptr->isPlural() ? "One of y" : "Y",
                            i_name, index_to_label(i));

                        /* Steal the items */
                        inven_item_increase(i,-1);
                        inven_item_optimize(i);

                        /* Obvious */
                        obvious = TRUE;

                        /* Done */
                        break;
                    }

                    break;

                case RBE_EAT_LITE:

                    /* Take some damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Access the lite */
                    i_ptr = &inventory[INVEN_LITE];

                    /* Drain fuel */
                    if ((i_ptr->GetPval() > 0) && !i_ptr->isArtifact()) {
                        /* Reduce fuel */
                        i_ptr->SetPval(i_ptr->GetPval() - 250 - randint(250));
                        if (i_ptr->GetPval() < 1) i_ptr->SetPval(1);

                        /* Notice */
                        if (!p_ptr->GetBlind()) {
                            msg_print("Your light dims.");
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_ACID:

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg_print("You are covered in acid!");

                    /* Special damage */
                    acid_dam(damage, ddesc);

                    break;

                case RBE_ELEC:

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg_print("You are struck by electricity!");

                    /* Special damage */
                    elec_dam(damage, ddesc);

                    break;

                case RBE_FIRE:

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg_print("You are enveloped in flames!");

                    /* Special damage */
                    fire_dam(damage, ddesc);

                    break;

                case RBE_COLD:

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg_print("You are covered with frost!");

                    /* Special damage */
                    cold_dam(damage, ddesc);

                    break;

                case RBE_BLIND:

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Increase "blind" */
                    if (!p_ptr->get_resists(RESIST_BLIND)) {
                        if (p_ptr->mod_blind(p_ptr->GetBlind() + 10 + randint(rlev))) {
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_CONFUSE:

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Increase "confused" */
                    if (!p_ptr->get_resists(RESIST_CONF)) {
                        if (p_ptr->mod_confused(p_ptr->GetConfused() + 3 + randint(rlev))) {
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_TERRIFY:

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Increase "afraid" */
                    if (p_ptr->get_resists(RESIST_FEAR)) {
                        msg_print("You stand your ground!");
                        obvious = TRUE;
                    }
                    else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                        msg_print("You stand your ground!");
                        obvious = TRUE;
                    }
                    else {
                        if (p_ptr->mod_afraid(p_ptr->GetAfraid() + 3 + randint(rlev))) {
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_PARALYZE:

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Increase "paralyzed" */
                    if (p_ptr->get_free_act()) {
                        msg_print("You are unaffected!");
                        obvious = TRUE;
                    }
                    else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                        msg_print("You resist the effects!");
                        obvious = TRUE;
                    }
                    else {
                        if (p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + 3 + randint(rlev))) {
                            obvious = TRUE;
                        }
                    }

                    break;

                case RBE_LOSE_STR:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_STR)) obvious = TRUE;

                    break;

                case RBE_LOSE_INT:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_INT)) obvious = TRUE;

                    break;

                case RBE_LOSE_WIS:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_WIS)) obvious = TRUE;

                    break;

                case RBE_LOSE_DEX:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_DEX)) obvious = TRUE;

                    break;

                case RBE_LOSE_CON:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_CON)) obvious = TRUE;

                    break;

                case RBE_LOSE_CHR:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stat) */
                    if (do_dec_stat(STAT_CHR)) obvious = TRUE;

                    break;

                case RBE_LOSE_ALL:

                    /* Damage (physical) */
                    p_ptr->take_hit(damage, ddesc);

                    /* Damage (stats) */
                    if (do_dec_stat(STAT_STR)) obvious = TRUE;
                    if (do_dec_stat(STAT_DEX)) obvious = TRUE;
                    if (do_dec_stat(STAT_CON)) obvious = TRUE;
                    if (do_dec_stat(STAT_INT)) obvious = TRUE;
                    if (do_dec_stat(STAT_WIS)) obvious = TRUE;
                    if (do_dec_stat(STAT_CHR)) obvious = TRUE;

                    break;

                case RBE_SHATTER:

                    /* Obvious */
                    obvious = TRUE;

                    /* Hack -- Reduce damage based on the player armor class */
                    damage -= (damage * ((ac < 150) ? ac : 150) / 250);

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    /* Radius 8 earthquake centered at the monster */
                    if (damage > 23) {
                        earthquake(GetY(), GetX(), 8);
                    }

                    break;

                case RBE_EXP_10:

                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    if (p_ptr->get_hold_life() && percent(95)) {
                        msg_print("You keep hold of your life force!");
                    }
                    else {
                        s32b d = damroll(10,6) + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE;
                        if (p_ptr->get_hold_life()) {
                            msg_print("You feel your life slipping away!");
                            p_ptr->lose_exp(d/10);
                        }
                        else {
                            msg_print("You feel your life draining away!");
                            p_ptr->lose_exp(d);
                        }
                    }
                    break;

                case RBE_EXP_20:

                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    if (p_ptr->get_hold_life() && percent(90)) {
                        msg_print("You keep hold of your life force!");
                    }
                    else {
                        s32b d = damroll(20,6) + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE;
                        if (p_ptr->get_hold_life()) {
                            msg_print("You feel your life slipping away!");
                            p_ptr->lose_exp(d/10);
                        }
                        else {
                            msg_print("You feel your life draining away!");
                            p_ptr->lose_exp(d);
                        }
                    }
                    break;

                case RBE_EXP_40:

                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    if (p_ptr->get_hold_life() && percent(75)) {
                        msg_print("You keep hold of your life force!");
                    }
                    else {
                        s32b d = damroll(40,6) + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE;
                        if (p_ptr->get_hold_life()) {
                            msg_print("You feel your life slipping away!");
                            p_ptr->lose_exp(d/10);
                        }
                        else {
                            msg_print("You feel your life draining away!");
                            p_ptr->lose_exp(d);
                        }
                    }
                    break;

                case RBE_EXP_80:

                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    p_ptr->take_hit(damage, ddesc);

                    if (p_ptr->get_hold_life() && percent(50)) {
                        msg_print("You keep hold of your life force!");
                    }
                    else {
                        s32b d = damroll(80,6) + (p_ptr->GetExp()/100) * MON_DRAIN_LIFE;
                        if (p_ptr->get_hold_life()) {
                            msg_print("You feel your life slipping away!");
                            p_ptr->lose_exp(d/10);
                        }
                        else {
                            msg_print("You feel your life draining away!");
                            p_ptr->lose_exp(d);
                        }
                    }
                    break;
            }


            /* Hack -- only one of cut or stun */
            if (do_cut && do_stun) {
                /* Cancel cut */
                if (percent(50)) {
                    do_cut = 0;
                }

                /* Cancel stun */
                else {
                    do_stun = 0;
                }
            }

            /* Handle cut */
            if (do_cut) {
                int k = 0;

                /* Critical hit (zero if non-critical) */
                tmp = monster_critical(d_dice, d_side, damage);

                /* Roll for damage */
                switch (tmp)
                {
                    case 0: k = 0; break;
                    case 1: k = randint(5); break;
                    case 2: k = randint(5) + 5; break;
                    case 3: k = randint(20) + 20; break;
                    case 4: k = randint(50) + 50; break;
                    case 5: k = randint(100) + 100; break;
                    case 6: k = 300; break;
                    default: k = 500; break;
                }

                /* Apply the cut */
                if (k) p_ptr->mod_cut(p_ptr->GetCut() + k);
            }

            /* Handle stun */
            if (do_stun)
            {
                int k = 0;

                /* Critical hit (zero if non-critical) */
                tmp = monster_critical(d_dice, d_side, damage);

                /* Roll for damage */
                switch (tmp)
                {
                    case 0: k = 0; break;
                    case 1: k = randint(5); break;
                    case 2: k = randint(10) + 10; break;
                    case 3: k = randint(20) + 20; break;
                    case 4: k = randint(30) + 30; break;
                    case 5: k = randint(40) + 40; break;
                    case 6: k = randint(60) + 60; break;
                    default: k = randint(80) + 80; break;
                }

                /* Apply the stun */
                if (k) p_ptr->mod_stun(p_ptr->GetStun() + k);
            }
        }

        /* Monster missed player */
        else {
            /* Analyze failed attacks */
            switch (method) {
                case RBM_HIT:
                case RBM_TOUCH:
                case RBM_PUNCH:
                case RBM_KICK:
                case RBM_CLAW:
                case RBM_BITE:
                case RBM_STING:
                case RBM_XXX1:
                case RBM_BUTT:
                case RBM_CRUSH:
                case RBM_ENGULF:
                case RBM_XXX2:

                    /* Visible monsters */
                    if (is_visible()) {
                        /* Message */
                        msg_format("%^s misses you.", m_name);
                    }

                    break;
            }
        }


        /* Analyze "visible" monsters only */
        if (visible) {
            /* Count "obvious" attacks (and ones that cause damage) */
            if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10)) {
                /* Count attacks of this type */
                if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR) {
                    r_ptr->r_blows[ap_cnt]++;
                }
            }
        }
    }


    /* Blink away */
    if (blinked) {
        msg_print("There is a puff of smoke!");
        teleport_away(this, MAX_SIGHT * 2 + 5);
    }


    /* Always notice cause of death */
    if (death && (r_ptr->r_deaths < MAX_SHORT)) r_ptr->r_deaths++;


    /* Assume we attacked */
    return TRUE;
}