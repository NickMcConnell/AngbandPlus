/* File: was melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Leon Marrick, Bahman Rabbi, Diego Gonzalez, Jeff Greene
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"

#define MAX_DESC_INSULT   8

/*
 * Hack -- possible "insult" messages
 */
static QString desc_insult[MAX_DESC_INSULT] =
{
    "insults you!",
    "insults your mother!",
    "gives you the finger!",
    "humiliates you!",
    "defiles you!",
    "dances around you!",
    "makes obscene gestures!",
    "moons you!!!"
};


/*
 * Critical blows by monsters can inflict cuts and stuns.
 */
static int monster_critical(int dice, int sides, int dam, int effect)
{
    int max = 0;
    int bonus;
    int total = dice * sides;

    /* Special case -- wounding/battering attack */
    if ((effect == RBE_WOUND) || (effect == RBE_BATTER))
    {
        /* Must do at least 70% of perfect */
        if (dam < total * 7 / 10) return (0);
        max = 1;
    }

    /* Standard attack */
    else
    {
        /* Weak blows rarely work */
        if ((rand_int(20) >= dam) || (!one_in_(3))) return (0);

        /* Must do at least 90% of perfect */
        if (dam < total * 9 / 10) return (0);
    }

    /* Perfect damage */
    if (dam == total) max++;

    /* Get bonus to critical damage (never greater than 6) */
    bonus = MIN(6, div_round(dam, 8));

    /* Critical damage  (never greater than 6 + max) */
    return (randint(bonus) + max);
}



/*
 * Adjust damage for character armour.
 *
 * Use proper rounding.  -LM-
 */
static void ac_dam(int *dam, int ac)
{
    /* The effect of a point of damage decreases as the total rises */
    int d = 120 + ABS(ac);

    /* Adjust damage (whole) */
    *dam -= ((*dam) * ac) / d;

    /* Adjust damage (fractional) */
    if ((((*dam) * ABS(ac)) % d) > rand_int(d))
    {
        *dam -= ((ac > 0) ? 1 : -1);
    }
}

/*
 * Determine if a monster attack against the player succeeds.
 */
static bool check_hit_player(int power, int level, int m_idx)
{
    int ac, chance;

    monster_type *m_ptr = &mon_list[m_idx];

    /* Calculate the "attack quality".  Stunned monsters are hindered. */
    chance = (power + ((m_ptr->m_timed[MON_TMD_STUN]) ? level * 2 : level * 3));

    /*Adjust for player terrain*/
    chance = feat_adjust_combat_for_player(chance, TRUE);

    /*Adjust for monster terrain*/
    chance = feat_adjust_combat_for_monster(m_ptr, chance, FALSE);

    if (m_ptr->mflag & (MFLAG_DESPERATE)) chance += 15;

    /* Total armor */
    ac = p_ptr->state.ac + p_ptr->state.to_a;

    /* Check if the player was hit */
    return test_hit(chance, ac, TRUE);
}

/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(monster_type *m_ptr)
{
    int m_idx = dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    int ap_cnt;

    int i, k, tmp, ac, rlev;
    s16b heal;
    int do_cut, do_stun;
    int blows;

    bool alive = TRUE;

    s32b gold;

    object_type *o_ptr;

    QString o_name;

    QString m_name;

    QString ddesc;

    bool blinked;

    int sound_msg;

    bool player_on_glyph = FALSE;

    /* Not allowed to attack */
    if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

    /* Remember if the player is standing on a glyph of warding */
    if (cave_player_glyph_bold(p_ptr->py, p_ptr->px)) player_on_glyph = TRUE;

    /* Total armor */
    ac = p_ptr->state.ac + p_ptr->state.to_a;

    /* Extract the effective monster level */
    rlev = MAX(r_ptr->level, 1);

    /* Get the monster name (or "it") */
    m_name = monster_desc(m_ptr, 0);

    /* Get the "died from" information (i.e. "a white worm mass") */
    ddesc = monster_desc(m_ptr, 0x88);

    /* Assume no blink */
    blinked = FALSE;

    /* Calculate the number of blows this monster gets */
    for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
    {
        if (!r_ptr->blow[ap_cnt].method) break;
    }
    blows = ap_cnt;

    /* Scan through all the blows */
    for (ap_cnt = 0; ap_cnt < blows; ap_cnt++)
    {
        bool visible = FALSE;
        bool obvious = FALSE;
        bool no_effect = FALSE;
        bool do_break = FALSE;

        int power = 0;
        int dam = 0;

        QString act = NULL;
        QString msg;

        /* Extract the attack information */
        int effect = r_ptr->blow[ap_cnt].effect;
        int method = r_ptr->blow[ap_cnt].method;
        int d_dice = r_ptr->blow[ap_cnt].d_dice;
        int d_side = r_ptr->blow[ap_cnt].d_side;

        /* Hack -- no more attacks */
        if (!method) break;

        /* Handle "leaving" */
        if (p_ptr->leaving_level) break;

        /* Extract visibility (before blink) */
        if (m_ptr->ml) visible = TRUE;

        /* Assume no cut, stun, or touch */
        do_cut = do_stun = 0;

        /* Assume no sound */
        sound_msg = MSG_GENERIC;

        /* Extract visibility from carrying lite */
        if (r_ptr->flags2 & RF2_HAS_LIGHT) visible = TRUE;

        /* Extract the attack "power".  Elemental attacks upgraded. */
        switch (effect)
        {
            case RBE_HURT:       power = 60;  break;
            case RBE_WOUND:      power = 60;  break;
            case RBE_BATTER:     power = 60;  break;
            case RBE_SHATTER:    power = 60;  break;
            case RBE_UN_BONUS:   power = 20;  break;
            case RBE_UN_POWER:   power = 15;  break;
            case RBE_LOSE_MANA:  power = 25;  break;
            case RBE_EAT_GOLD:   power =  5;  break;
            case RBE_EAT_ITEM:   power =  5;  break;
            case RBE_EAT_FOOD:   power =  5;  break;
            case RBE_EAT_LIGHT:   power =  5;  break;
            case RBE_HUNGER:     power = 15;  break;
            case RBE_POISON:     power = 25;  break;
            case RBE_ACID:       power = 10;  break;
            case RBE_ELEC:       power = 10;  break;
            case RBE_FIRE:       power = 10;  break;
            case RBE_COLD:       power = 10;  break;
            case RBE_BLIND:      power =  5;  break;
            case RBE_CONFUSE:    power = 10;  break;
            case RBE_TERRIFY:    power = 10;  break;
            case RBE_PARALYZE:   power =  5;  break;
            case RBE_HALLU:      power = 10;  break;
            case RBE_DISEASE:    power = 10;  break;
            case RBE_LOSE_STR:   power =  0;  break;
            case RBE_LOSE_DEX:   power =  0;  break;
            case RBE_LOSE_CON:   power =  0;  break;
            case RBE_LOSE_INT:   power =  0;  break;
            case RBE_LOSE_WIS:   power =  0;  break;
            case RBE_LOSE_CHR:   power =  0;  break;
            case RBE_LOSE_ALL:   power =  2;  break;
            case RBE_EXP_10:     power =  5;  break;
            case RBE_EXP_20:     power =  5;  break;
            case RBE_EXP_40:     power =  5;  break;
            case RBE_EXP_80:     power =  5;  break;
        }

        /* Roll out the damage */
        dam = damroll(d_dice, d_side);

        /* Glyphs of warding halve melee damage */
        if (player_on_glyph)
        {
            dam /= 2;

            /* No damage, ignore blow */
            if (!dam) continue;
        }

        /* Describe the attack method, apply special hit chance mods. */
        switch (method)
        {
            case RBM_HIT:
            {

                /* Handle special effect types */
                if (effect == RBE_WOUND)
                {
                    if      (dam >= 30) act = "gouges you";
                    else if (dam >= 20) act = "slashes you";
                    else if (dam >= 5)  act = "cuts you";
                    else                act = "scratches you";
                }
                else if (effect == RBE_BATTER)
                {
                    if      (dam >= 30) act = "bludgeons you";
                    else if (dam >= 20) act = "batters you";
                    else if (dam >= 5)  act = "bashes you";
                    else                act = "hits you";
                }
                else
                {
                    act = "hits you";
                }
                do_cut = do_stun = 1;
                sound_msg = MSG_MON_HIT;
                break;
            }
            case RBM_TOUCH:
            {
                act = "touches you";
                sound_msg = MSG_MON_TOUCH;
                break;
            }

            case RBM_PUNCH:
            {
                act = "punches you";
                do_stun = 1;
                sound_msg = MSG_MON_PUNCH;
                break;
            }

            case RBM_KICK:
            {
                act = "kicks you";
                do_stun = 1;
                sound_msg = MSG_MON_KICK;
                break;
            }
            case RBM_CLAW:
            {
                if      (dam >= 25) act = "slashes you";
                else if (dam >=  5) act = "claws you";
                else                act = "scratches you";
                do_cut = 1;
                sound_msg = MSG_MON_CLAW;
                break;
            }
            case RBM_BITE:
            {
                if (dam >= 5) act = "bites you";
                else          act = "nips you";
                do_cut = 1;
                sound_msg = MSG_MON_BITE;
                break;
            }
            case RBM_PECK:
            {
                act = "pecks you";
                do_cut = 1;
                sound_msg = MSG_MON_BITE;
                break;
            }
            case RBM_BREATHE:
            {
                act = "breathes on you";
                do_cut = 1;
                sound_msg = MSG_BR_ACID;
                break;
            }
            case RBM_STING:
            {
                act = "stings you";
                sound_msg = MSG_MON_STING;
                break;
            }
            case RBM_BUTT:
            {
                if (dam >= rand_range(10, 20)) act = "tramples you";
                else                           act = "butts you";
                do_stun = 1;
                sound_msg = MSG_MON_BUTT;
                break;
            }
            case RBM_CRUSH:
            {
                if (dam >= 10) act = "crushes you";
                else           act = "squeezes you";
                do_stun = 1;
                sound_msg = MSG_MON_CRUSH;
                break;
            }
            case RBM_ENGULF:
            {
                if (dam >= randint(50)) act = "envelops you";
                else                    act = "engulfs you";
                sound_msg = MSG_MON_ENGULF;
                break;
            }
            case RBM_CRAWL:
            {
                act = "crawls on you";
                sound_msg = MSG_MON_CRAWL;
                break;
            }
            case RBM_DROOL:
            {
                act = "drools on you";
                sound_msg = MSG_MON_DROOL;
                break;
            }
            case RBM_SPIT:
            {
                act = "spits on you";
                sound_msg = MSG_MON_SPIT;
                break;
            }
            case RBM_SLIME:
            {
                act = "You've been slimed!";
                sound_msg = MSG_MON_SPIT;
                break;
            }
            case RBM_GAZE:
            {
                if      (dam >= rand_range(20, 30))
                    act = "glares at you terribly";
                else if (dam >= rand_range(5, 30))
                    act = "gazes upon you";
                else act = "gazes at you";
                sound_msg = MSG_MON_GAZE;
                break;
            }
            case RBM_WAIL:
            {
                act = "makes a horrible wail";
                sound_msg = MSG_MON_WAIL;
                break;
            }
            case RBM_SPORE:
            {
                act = "releases a cloud of spores";
                sound_msg = MSG_MON_SPORE;
                break;
            }
            case RBM_TRAMPLE:
            {
                act = "tramples all over you!";
                sound_msg = MSG_MON_CRUSH;
                break;
            }
            case RBM_XXX5:
            case RBM_XXX6:
            {
                act = "projects XXX's at you";
                break;
            }
            case RBM_BEG:
            {
                act = "begs you for money";
                sound_msg = MSG_MON_BEG;
                break;
            }
            case RBM_INSULT:
            {
                act = desc_insult[rand_int(MAX_DESC_INSULT)];
                sound_msg = MSG_MON_INSULT;
                break;
            }
        }

        /* No effect */
        if (no_effect) continue;

        /* Monster hits player */
        if (!effect || check_hit_player(power, rlev, m_idx))
        {
            /* Always disturbing */
            disturb(TRUE, TRUE);

            /* Hack -- Apply "protection from evil" */
            if ((p_ptr->timed[TMD_PROTEVIL] > 0) &&
                (r_ptr->flags3 & (RF3_EVIL)) &&
                (p_ptr->lev >= rlev) &&
                ((rand_int(100) + p_ptr->lev) > 50))
            {

                /* Remember the Evil-ness */
                if (m_ptr->ml)
                {
                    l_ptr->r_l_flags3 |= (RF3_EVIL);
                }

                /* Message */
                message(QString("%1 is repelled.") .arg(capitalize_first(m_name)));

                /* Hack -- Next attack */
                continue;
            }

            /* Message - special handling for sliming and insult attacks */
            if (!act.isEmpty())
            {

                if (method == RBM_SLIME) msg = (QString("%1") .arg(act));
                else if (method == RBM_INSULT)
                {
                    msg = (QString("%1 %2").arg(capitalize_first(m_name)).arg(act));
                }
                else
                {
                    if (dam > p_ptr->chp / 3)
                        msg = (QString("%1 %2!") .arg(capitalize_first(m_name)) .arg(act));
                    else
                        msg = (QString("%1 %2.") .arg(capitalize_first(m_name)) .arg(act));
                }

                /* Message */
                sound(sound_msg);
                message(msg);
            }

            /* Hack -- assume all attacks are obvious */
            obvious = TRUE;

            /* Apply appropriate damage */
            switch (effect)
            {

                /* No effect */
                case 0:
                {
                    /* Hack -- Assume obvious */
                    obvious = TRUE;

                    /* Hack -- No damage */
                    dam = 0;

                    break;
                }

                /* Ordinary hit */
                case RBE_HURT:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    break;
                }

                /* Hit with increased chance to wound */
                case RBE_WOUND:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Usually don't stun */
                    if ((do_stun) && (!one_in_(5))) do_stun = FALSE;

                    /* Always give a chance to inflict cuts */
                    do_cut = TRUE;

                    break;
                }

                /* Hit with increased chance to stun */
                case RBE_BATTER:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Usually don't cut */
                    if ((do_cut) && (!one_in_(5))) do_cut = FALSE;

                    /* Always give a chance to inflict stuns */
                    do_stun = TRUE;

                    break;
                }

                /* Hit to cause earthquakes */
                case RBE_SHATTER:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Radius 6 earthquake centered on the monster */
                    if (dam > rand_int(60))
                    {
                        earthquake(m_ptr->fy, m_ptr->fx, 6, TRUE);

                        /*check if the monster & player are still next to each other*/
                        if (distance(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx) > 1)
                            do_break = TRUE;

                    }

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    break;
                }

                /* Hit to disenchant */
                case RBE_UN_BONUS:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage, except in Moria */
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Allow complete resist */
                    if (!p_ptr->state.resist_disen)
                    {
                        /* Apply disenchantment */
                        if (apply_disenchant(0)) obvious = TRUE;
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_DISEN);

                    break;
                }

                /* Hit to reduce charges of magical items */
                case RBE_UN_POWER:
                {
                    /* Take damage, but not in Moria */
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Find an item */
                    for (k = 0; k < 20; k++)
                    {

                        /* Blindly hunt ten times for an item. */
                        i = rand_int(INVEN_PACK);

                        /* Obtain the item */
                        o_ptr = &inventory[i];

                        /* use "tmp" to decide if a item can
                         * be uncharged.  By default, assume it
                         * can't.
                         */
                        tmp = 0;

                        /* Skip non-objects */
                        if (!o_ptr->k_idx) continue;

                        /* Drain charged wands/staffs */
                        if (item_tester_hook_recharge(o_ptr))

                        {
                            /* case of charged wands/staffs. */
                            if (((o_ptr->tval == TV_STAFF) ||
                                (o_ptr->tval == TV_WAND)) &&
                                (o_ptr->pval)) tmp = 1;

                            /* case of (at least partially) charged rods. */
                            else if ((o_ptr->tval == TV_ROD) &&
                                (o_ptr->timeout < o_ptr->pval)) tmp = 1;

                            if (tmp)
                            {

                                heal = drain_charges(o_ptr, rlev);

                                /* Message */
                                message(QString("Energy drains from your pack!"));

                                /* Obvious */
                                obvious = TRUE;

                                /* Message */
                                if ((m_ptr->hp < m_ptr->maxhp) && (heal))
                                {
                                    if (m_ptr->ml) message(QString("%1 looks healthier.") .arg(capitalize_first(m_name)));
                                    else message(QString("%1 sounds healthier.") .arg(capitalize_first(m_name)));
                                }

                                /*heal is greater than monster wounds, restore mana too*/
                                if (heal > (m_ptr->maxhp - m_ptr->hp))
                                {

                                    /*leave some left over for mana*/
                                    heal -= (m_ptr->maxhp - m_ptr->hp);

                                    /*fully heal the monster*/
                                    m_ptr->hp = m_ptr->maxhp;

                                    /*mana is more powerful than HP*/
                                    heal /= 10;

                                    /* if heal was less than 10, make it 1*/
                                    if (heal < 1) heal = 1;

                                    /*give message if anything left over*/
                                    if (m_ptr->mana < r_ptr->mana)
                                    {
                                        if (m_ptr->ml) message(QString("%1 looks refreshed.") .arg(capitalize_first(m_name)));
                                        else message(QString("%1 sounds refreshed.") .arg(capitalize_first(m_name)));
                                    }

                                    /*add mana*/
                                    m_ptr->mana += heal;

                                    if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;
                                }

                                /* Simple Heal */
                                else m_ptr->hp += heal;

                                /* Redraw (later) if needed */
                                if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

                                /* Combine / Reorder the pack */
                                p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

                                /* Redraw stuff */
                                p_ptr->redraw |= (PR_WIN_INVENTORY);

                                /* not more than one inventory
                                 * slot effected. */
                                break;
                            }
                        }
                    }

                    break;
                }

                /* Hit to reduce mana */
                case RBE_LOSE_MANA:
                {
                    int drain;

                    QString msg_tmp = msg;

                    /* Obvious */
                    obvious = TRUE;

                    /* Damage (mana) */
                    if (p_ptr->csp)
                    {
                        /* Drain depends on maximum mana */
                        drain = 2 + rand_int(p_ptr->msp / 10);

                        /* Drain the mana */
                        if (drain > p_ptr->csp)
                        {
                        p_ptr->csp = 0;
                            p_ptr->csp_frac = 0;

                            msg_tmp.append(QString("  Your mana is gone!"));
                        }
                        else
                        {
                            p_ptr->csp -= drain;
                            msg_tmp.append(QString("  Your mana drains away."));
                        }

                        /* Redraw mana */
                        p_ptr->redraw |= (PR_SIDEBAR_MON);

                    }

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Damage (physical) */
                    take_hit(dam, ddesc);


                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_MANA);
                    break;
                }

                /* Hit to steal gold */

                case RBE_EAT_GOLD:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage , but not in Moria*/
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Confused monsters cannot steal successfully. */
                    if (m_ptr->m_timed[MON_TMD_CONF]) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p_ptr->timed[TMD_PARALYZED] &&
                        (rand_int(100) < (adj_dex_safe[p_ptr->state.stat_index[A_DEX]] +
                                          (p_ptr->lev / 10))))
                    {
                        /* Saving throw message */
                        message(QString("You quickly protect your money pouch!"));

                        /* Occasional blink anyway */
                        if (one_in_(3)) blinked = TRUE;
                    }

                    /* Eat gold */
                    else
                    {

                        /* The more gold you have, the smaller a fraction of it is stolen. */
                        int div1 = 8 + div_round(p_ptr->au, 4000L);
                        int div2 = div1 * 2;

                        /* Steal some gold (at least two gold pieces) */
                        gold = rand_range(p_ptr->au / div2, p_ptr->au / div1) +
                               rand_range(2, 25);

                        /*sometimes a player doesn't even have two coins*/
                        if (p_ptr->au < gold) gold = p_ptr->au;

                        /* Reduce character gold */
                        p_ptr->au -= gold;

                        /* Messages */
                        if (gold == 0)
                        {
                            message(QString("Nothing was stolen."));
                        }
                        else if (gold == 1)
                        {
                            message(QString("Your purse feels lighter."));
                            message(QString("Your coin was stolen!"));
                        }
                        else if (p_ptr->au)
                        {
                            message(QString("Your purse feels lighter."));
                            message(QString("%1 coins were stolen!") .arg(gold));
                        }
                        else
                        {
                            message(QString("Your purse feels lighter."));
                            message(QString("All of your coins were stolen!"));
                        }

                        /* While we have gold, put it in objects */
                        while (gold > 0)
                        {
                            int amt;

                            /* Create a new temporary object */
                            object_type o;
                            o.object_wipe();
                            object_prep(&o, lookup_kind(TV_GOLD, SV_GOLD_GOLD));

                            /* Amount of gold to put in this object */
                            amt = gold > SHRT_MAX ? SHRT_MAX : gold;
                            o.pval = amt;
                            gold -= amt;

                            /* Give the gold to the monster */
                            monster_carry(m_idx, &o);
                        }


                        /* Redraw gold and update player score */
                        p_ptr->update |= (PU_PLAYER_SCORE);
                        p_ptr->redraw |= (PR_SIDEBAR_MON);

                        /* Blink away */
                        blinked = TRUE;
                    }

                    break;
                }

                /* Hit to steal objects from the pack */
                case RBE_EAT_ITEM:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage, but not in Moria*/
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p_ptr->timed[TMD_PARALYZED] &&
                        (rand_int(100) < (adj_dex_safe[p_ptr->state.stat_index[A_DEX]] +
                                          (p_ptr->lev / 10))))
                    {
                        /* Saving throw message */
                        message(QString("You grab hold of your backpack!"));


                        /* Occasional "blink" anyway */
                        blinked = TRUE;

                        /* Obvious */
                        obvious = TRUE;

                        /* Done */
                        break;
                    }

                    /* Blindly scrabble in the backpack ten times */
                    for (k = 0; k < 10; k++)
                    {
                        object_type *i_ptr;
                        object_type object_type_body;

                        /* Pick an item */
                        i = rand_int(INVEN_PACK);

                        /* Obtain the item */
                        o_ptr = &inventory[i];

                        /* Skip non-objects */
                        if (!o_ptr->k_idx) continue;

                        /* Skip artifacts */
                        if (o_ptr->is_artifact()) continue;

                        /* Get a description */
                        o_name = object_desc(o_ptr, ODESC_FULL);

                        /* Message */
                        message(QString("%1our %2 (%3) was stolen!") .arg((o_ptr->number > 1) ? "One of y" : "Y")
                                   .arg(o_name) .arg(index_to_label(i)));

                        /* Get local object */
                        i_ptr = &object_type_body;

                        /* Obtain local object */
                        i_ptr->object_copy(o_ptr);

                        /* One item is stolen at a time. */
                        i_ptr->number = 1;

                        /* Hack -- If a rod or wand, allocate total
                         * maximum timeouts or charges between those
                         * stolen and those missed. -LM-
                         */
                        distribute_charges(o_ptr, i_ptr, 1);

                        /* Carry the object */
                        (void)monster_carry(m_idx, i_ptr);

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
                }

                /* Hit to eat food */
                case RBE_EAT_FOOD:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage, but not in Moria */
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Steal some food */
                    for (k = 0; k < 6; k++)
                    {
                        /* Pick an item from the pack */
                        i = rand_int(INVEN_PACK);

                        /* Get the item */
                        o_ptr = &inventory[i];

                        /* Skip non-objects */
                        if (!o_ptr->k_idx) continue;

                        /* Skip non-food objects */
                        if (o_ptr->tval != TV_FOOD) continue;

                        /* Get a description */
                        o_name = object_desc(o_ptr, ODESC_FULL);

                        if ((method != RBM_SLIME) ||
                            (o_ptr->sval < SV_FOOD_MIN_FOOD))
                        {
                            message(QString("%1 %2 (%3) was eaten!") .arg((o_ptr->number > 1) ? "One of your" : "Your")
                                            .arg(o_name) .arg(index_to_label(i)));
                        }

                        /* Special message for Green Glutton Ghosts */
                        else
                        {
                            message(QString("It got at your rations!"));
                        }

                        /* Steal the items */
                        inven_item_increase(i, -1);
                        inven_item_optimize(i);

                        /* Obvious */
                        obvious = TRUE;

                        /* Done */
                        break;
                    }

                    break;
                }

                /* Hit to reduce nutrition */
                case RBE_HUNGER:
                {
                    int resist = 2;

                    obvious = TRUE;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* We're not dead yet */
                    if (!p_ptr->is_dead)
                    {
                        /* Allow resistance */
                        if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE]) resist++;
                        if (p_ptr->state.slow_digest) resist += 2;

                        /* Message -- only if appropriate */
                        if ((resist > 2) &&
                            (p_ptr->food > PY_FOOD_ALERT))
                        {
                            message(QString("You resist the effects!"));
                        }
                        else
                        {
                            message(QString("You feel hungry..."));
                        }

                        /* Reduce food counter, but not too much. */
                        set_food(p_ptr->food -
                            MIN(500 + p_ptr->food / 5, p_ptr->food / resist));

                    }

                    break;
                }

                /* Hit to drain light */
                case RBE_EAT_LIGHT:
                {

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage, but not in Moria */
                    if (game_mode != GAME_NPPMORIA) take_hit(dam, ddesc);

                    /* Get the light source */
                    o_ptr = &inventory[INVEN_LIGHT];

                    /* Drain fuel */
                    if ((o_ptr->timeout > 0) && !o_ptr->is_artifact())
                    {
                        /* Reduce fuel */
                        o_ptr->timeout -= rand_range(250, 500);
                        if (o_ptr->timeout < 1) o_ptr->timeout = 1;

                        /* Notice */
                        if (!p_ptr->timed[TMD_BLIND])
                        {
                            message(QString("Your light dims."));
                            obvious = TRUE;
                        }

                        /* Redraw stuff */
                        p_ptr->redraw |= (PR_WIN_EQUIPMENT);
                    }

                    break;
                }


                /* Hit to poison */
                case RBE_POISON:
                {
                        /* Take damage */
                    take_hit(dam, ddesc);

                    /* Take "poison" effect */
                    if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
                    {
                        if (inc_timed(TMD_POISONED, randint(rlev) + 5, TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_POIS);

                    break;

                }

                /* Hit to inflict acid damage */
                case RBE_ACID:
                {
                    /* Some of attack is pure damage, and so
                     * resists should not be allowed to reduce
                     * damage as much as they do normally.
                     * Damage will be reduced later.
                     */
                    int dam2 = dam;

                    if (p_ptr->state.resist_acid) dam2 *= 2;
                    if (p_ptr->timed[TMD_OPP_ACID]) dam2 *= 2;

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    message(QString("You are covered in acid!"));

                    /* Special damage */
                    acid_dam(dam2, ddesc);

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_ACID);

                    break;
                }

                /* Hit to electrocute */
                case RBE_ELEC:
                {
                    /* Some of attack is pure damage, and so
                     * resists should not be allowed to reduce
                     * damage as much as they do normally.
                     * Damage will be reduced later.
                     */
                    int dam2 = dam;

                    /* Obvious */
                    obvious = TRUE;

                    if (p_ptr->state.resist_elec) dam2 *= 2;
                    if (p_ptr->timed[TMD_OPP_ELEC]) dam2 *= 2;

                    /* Message */
                    message(QString("You are struck by electricity!"));

                    /* Take damage (special) */
                    elec_dam(dam2, ddesc);

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_ELEC);

                    break;
                }

                /* Hit to burn */
                case RBE_FIRE:
                {
                    /* Some of attack is pure damage, and so
                     * resists should not be allowed to reduce
                     * damage as much as they do normally.
                     * Damage will be reduced later.
                     */
                    int dam2 = dam;

                    if (p_ptr->state.resist_fire) dam2 *= 2;
                    if (p_ptr->timed[TMD_OPP_FIRE]) dam2 *= 2;

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    message(QString("You are enveloped in flames!"));

                    /* Take damage (special) */
                    fire_dam(dam2, ddesc);

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_FIRE);

                    break;
                }

                case RBE_COLD:
                {
                    /* Some of attack is pure damage, and so
                     * resists should not be allowed to reduce
                     * damage as much as they do normally.
                     * Damage will be reduced later.
                     */
                    int dam2 = dam;

                    if (p_ptr->state.resist_cold) dam2 *= 2;
                    if (p_ptr->timed[TMD_OPP_COLD]) dam2 *= 2;

                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    message(QString("You are covered with frost!"));

                    /* Take damage (special) */
                    cold_dam(dam2, ddesc);

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_COLD);

                    break;
                }

                case RBE_BLIND:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Increase blindness */
                    if (!p_ptr->state.resist_blind)
                    {
                        if (inc_timed(TMD_BLIND, 10 + randint(rlev), TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_BLIND);

                    break;
                }

                /* Hit to confuse */
                case RBE_CONFUSE:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Increase "confused" */
                    if (allow_player_confusion())
                    {
                        if (inc_timed(TMD_CONFUSED, 3 + randint(rlev), TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_CONFU);

                    break;
                }

                /* Hit to frighten */
                case RBE_TERRIFY:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Increase "afraid" */
                    if (p_ptr->state.resist_fear)
                    {
                        message(QString("You stand your ground!"));
                        obvious = TRUE;
                    }
                    else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
                    {
                        message(QString("You stand your ground!"));
                        obvious = TRUE;
                    }
                    else
                    {
                        if (inc_timed(TMD_AFRAID, 3 + randint(rlev), TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_FEAR_SAVE);

                    break;
                }

                /* Hit to paralyze (never cumulative) */
                case RBE_PARALYZE:
                {
                    /* Hack -- Prevent perma-paralysis via damage */
                    if (p_ptr->timed[TMD_PARALYZED] && (dam < 1)) dam = 1;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Increase "paralyzed" */
                    if (p_ptr->state.free_act)
                    {
                        message(QString("You are unaffected!"));
                        obvious = TRUE;
                    }
                    else if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
                    {
                        message(QString("You resist the effects!"));
                        obvious = TRUE;
                    }
                    else
                    {
                        if (inc_timed(TMD_PARALYZED, 3 + randint(rlev), TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_FREE_SAVE);

                    break;
                }

                /* Hit to cause disease */
                case RBE_DISEASE:
                {
                    int do_disease = dam;

                    /* Player armour reduces raw damage */
                    ac_dam(&dam, ac);

                    /* Take (adjusted) damage */
                    take_hit(dam, ddesc);

                    /* Inflict disease (unaffected by armour) */
                    disease(&do_disease);

                    break;
                }

                case RBE_LOSE_STR:
                case RBE_LOSE_INT:
                case RBE_LOSE_WIS:
                case RBE_LOSE_DEX:
                case RBE_LOSE_CON:
                case RBE_LOSE_CHR:
                case RBE_LOSE_ALL:
                {
                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Reduce strength */
                    if ((effect == RBE_LOSE_STR) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_STR)) obvious = TRUE;
                    }

                    /* Reduce intelligence */
                    if ((effect == RBE_LOSE_INT) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_INT)) obvious = TRUE;
                    }

                    /* Reduce wisdom */
                    if ((effect == RBE_LOSE_WIS) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_WIS)) obvious = TRUE;
                    }

                    /* Reduce dexterity */
                    if ((effect == RBE_LOSE_DEX) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_DEX)) obvious = TRUE;
                    }

                    /* Reduce constitution */
                    if ((effect == RBE_LOSE_CON) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_CON)) obvious = TRUE;
                    }

                    /* Reduce constitution */
                    if ((effect == RBE_LOSE_CHR) || (effect == RBE_LOSE_ALL))
                    {
                        if (do_dec_stat(A_CHR)) obvious = TRUE;
                    }



                    break;
                }

                /* Hit to reduce skills */
                case RBE_EXP_10:
                case RBE_EXP_20:
                case RBE_EXP_40:
                case RBE_EXP_80:
                {
                    /* Assume draining */
                    bool drain = TRUE;

                    /* Obvious */
                    obvious = TRUE;

                    /* Player armour reduces total damage */
                    ac_dam(&dam, ac);

                    /* Take damage */
                    // No damage from this attack in Moria
                    if (game_mode != GAME_NPPMORIA)take_hit(dam, ddesc);

                    /* Hold life usually prevents life draining */
                    if (p_ptr->state.hold_life)
                    {
                        int save_chance = 0;

                        /*get the saving percentage*/
                        if (effect == RBE_EXP_10) save_chance = 95;
                        else if (effect == RBE_EXP_20) save_chance = 90;
                        else if (effect == RBE_EXP_40) save_chance = 75;
                        else if (effect == RBE_EXP_80) save_chance = 50;

                        if (rand_int(100) < save_chance)
                        {
                            message(QString("You keep hold of your life force!"));
                            drain = FALSE;
                        }
                    }

                    /* Drain life */
                    if (drain)
                    {
                        int d = 0;

                        // Moria experience drain is based on damage dice
                        if (game_mode == GAME_NPPMORIA) d = dam;

                        /*go through the 4 strengths of drain_life*/

                        else if (effect == RBE_EXP_10)
                            d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;

                        else if (effect == RBE_EXP_20)
                            d = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

                        else if (effect == RBE_EXP_40)
                            d = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

                        else if (effect == RBE_EXP_80)
                            d = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

                        /*give the player a message*/
                        if (p_ptr->state.hold_life)
                        {
                            message(QString("You feel your life slipping away!"));
                            lose_exp(d/10);
                        }
                        else
                        {
                            message(QString("You feel your life draining away!"));
                            lose_exp(d);
                        }
                    }
                    break;
                }


                case RBE_HALLU:
                {
                    /* Take damage */
                    take_hit(dam, ddesc);

                    /* Increase "image" */
                    if (!p_ptr->state.resist_chaos)
                    {
                        if (inc_timed(TMD_IMAGE, 3 + randint(rlev / 2), TRUE))
                        {
                            obvious = TRUE;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_idx, LRN_CHAOS);

                    break;
                }
            }

            /* Handle character death */
            if (p_ptr->is_dead && (l_ptr->deaths < SHRT_MAX))
            {
                l_ptr->deaths++;

                /* Leave immediately */
                return (TRUE);
            }

            /* Hack -- only one of cut or stun */
            if (do_cut && do_stun)
            {
                /* Cancel cut */
                if (one_in_(2))
                {
                    do_cut = 0;
                }

                /* Cancel stun */
                else
                {
                    do_stun = 0;
                }
            }

            /* Handle cut */
            if (do_cut)
            {

                /* Critical hit (zero if non-critical) */
                tmp = monster_critical(d_dice, d_side, dam, effect);

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
                if (k) (void)set_cut(p_ptr->timed[TMD_CUT] + k);
            }

            /* Handle stun */
            if (do_stun)
            {

                /* Critical hit (zero if non-critical) */
                tmp = monster_critical(d_dice, d_side, dam, effect);

                /* Roll for damage */
                switch (tmp)
                {
                    case 0:  k = 0; break;
                    case 1:  k = randint(5); break;
                    case 2:  k = rand_range( 8, 16); break;
                    case 3:  k = rand_range(15, 30); break;
                    case 4:  k = rand_range(25, 50); break;
                    case 5:  k = rand_range(35, 70); break;
                    case 6:  k = rand_range(45, 90); break;
                    default: k = 100; break;

                }

                /* Apply the stun */
                if (k) (void)set_stun(p_ptr->timed[TMD_STUN] + k);
            }
        }

        /* Monster missed player */
        else
        {
            /* Analyze failed attacks */
            switch (method)
            {
                case RBM_HIT:
                case RBM_TOUCH:
                case RBM_PUNCH:
                case RBM_KICK:
                case RBM_CLAW:
                case RBM_BITE:
                case RBM_PECK:
                case RBM_STING:
                case RBM_BREATHE:
                case RBM_BUTT:
                case RBM_CRUSH:


                /* Visible monsters */
                if (m_ptr->ml)
                {
                    /* Disturbing */
                    disturb(TRUE, FALSE);

                    /* Message */
                    message(QString("%1 misses you.")  .arg(capitalize_first(m_name)));
                }

                break;
            }
        }


        /* Analyze "visible" monsters only */
        if (visible)
        {
            /* Count "obvious" attacks (and ones that cause damage) */
            if (obvious || dam || (l_ptr->blows[ap_cnt] > 10))
            {
                /* Count attacks of this type */
                if (l_ptr->blows[ap_cnt] < UCHAR_MAX)
                {
                    l_ptr->blows[ap_cnt]++;
                }
            }
        }

        /*hack - stop attacks if monster and player are no longer next to each other*/
        if (do_break) break;

    }


    /* Blink away */
    if ((blinked) && (alive))
    {

        if (teleport_away(m_idx, MAX_SIGHT * 2 + 5))
        {
            message(QString("There is a puff of smoke!"));
        }
    }

    /* Assume we attacked */
    return (TRUE);
}


