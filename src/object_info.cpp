
/* File: obj-info.c */

/*
 * Copyright (c) 2002 Andrew Sidwell, Robert Ruehlmann
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/store.h"
#include <src/cmds.h>
#include <QMessageBox>



static QString output_list(QVector<QString> list, bool use_or)
{
    QString output;

    output.clear();

    QString conjunction = "and ";
    if (use_or) conjunction = "or ";

    for (int i = 0; i < list.size(); i++)
    {
        if (i != 0)
        {
            output.append((i == 1 && i == list.size() - 1) ? " " : ", ");
            if (i == list.size() - 1)	output.append(conjunction);

        }
        output.append(list.at(i));
    }
    return (output);
}


QString output_desc_list(QString intro, QVector<QString> list, bool use_or, bool end_punctuation)
{
    QString output;

    output.clear();

    if (list.size())
    {
        /* Output intro */
        output = (intro);

        /* Output list */
        output.append(output_list(list, use_or));

        /* Output end */
        if (end_punctuation) output.append(".  ");
    }

    return (output);
}


/*
 * Describe stat modifications.
 */
static QString describe_stats(object_type *o_ptr, u32b f1)
{
    QString output;

    output.clear();

    QVector<QString> descs;
    descs.clear();
    int pval = (o_ptr->pval > 0 ? o_ptr->pval : -o_ptr->pval);

    /* Abort if the pval is zero */
    if (!pval) return (output);

    /* Collect stat bonuses */
    if (f1 & (TR1_STR)) descs.append(color_string(stat_names_full[A_STR], pval > 0 ? TERM_GREEN : TERM_RED));
    if (f1 & (TR1_INT)) descs.append(color_string(stat_names_full[A_INT], pval > 0 ? TERM_GREEN : TERM_RED));
    if (f1 & (TR1_WIS)) descs.append(color_string(stat_names_full[A_WIS], pval > 0 ? TERM_GREEN : TERM_RED));
    if (f1 & (TR1_DEX)) descs.append(color_string(stat_names_full[A_DEX], pval > 0 ? TERM_GREEN : TERM_RED));
    if (f1 & (TR1_CON)) descs.append(color_string(stat_names_full[A_CON], pval > 0 ? TERM_GREEN : TERM_RED));
    if (f1 & (TR1_CHR)) descs.append(color_string(stat_names_full[A_CHR], pval > 0 ? TERM_GREEN : TERM_RED));

    /* Skip */
    if (!descs.size()) return (output);

    /* Shorten to "all stats", if appropriate. */
    if (descs.size() == A_MAX)
    {
        if (pval > 0) output.append(color_string("It increases all your stats", TERM_GREEN));
        else output.append(color_string("It decreases all your stats", TERM_RED));
    }
    else
    {
        if (pval > 0) output.append("It increases your ");
        else output.append("It decreases your ");

        /* Output list */
        output.append(output_list(descs, FALSE));
    }

    /* Output end */
    output.append(QString(" by %1.  ") .arg(pval));

    return (output);
}


/*
 * Describe "secondary bonuses" of an item.
 */
static QString describe_secondary(object_type *o_ptr, u32b f1)
{
    QString output;

    output.clear();

    QVector<QString> descs;
    descs.clear();
    int pval = (o_ptr->pval > 0 ? o_ptr->pval : -o_ptr->pval);

    /* Collect */
    if (f1 & (TR1_STEALTH)) descs.append("stealth");
    if (f1 & (TR1_SEARCH))  descs.append("searching");
    if (f1 & (TR1_INFRA))   descs.append("infravision");
    if (f1 & (TR1_TUNNEL))  descs.append("tunneling");
    if (f1 & (TR1_SPEED))   descs.append("speed");
    if (f1 & (TR1_BLOWS))   descs.append("attack speed");
    if (f1 & (TR1_SHOTS))   descs.append("shooting speed");
    if (f1 & (TR1_MIGHT))   descs.append("shooting power");

    /* Skip */
    if (!descs.size()) return (output);

    /* Start */
    output.append(QString("It %1 your ") .arg(o_ptr->pval > 0 ? "increases" : "decreases"));

    int attr = (pval > 0 ? TERM_GREEN : TERM_RED);

    for (int i = 0; i < descs.size(); i++)
    {
        descs[i] = color_string(descs[i], attr);
    }

    /* Output list */
    output.append(output_list(descs, FALSE));

    /* Output end */
    output.append(QString(" by %1.  ") .arg(pval));

    /* We found something */
    return (output);
}

/*
 * Describe the special slays and executes of an item.
 */
static QString describe_slay(object_type *o_ptr, u32b f1)
{
    QString output;
    output.clear();

    QVector<QString> slays;
    QVector<QString> execs;
    slays.clear();
    execs.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect brands */
    if (f1 & (TR1_SLAY_ANIMAL)) slays.append("animals");
    if (f1 & (TR1_SLAY_ORC))    slays.append("orcs");
    if (f1 & (TR1_SLAY_TROLL))  slays.append("trolls");
    if (f1 & (TR1_SLAY_GIANT))  slays.append("giants");

    /* Dragon slay/execute */
    if (f1 & TR1_KILL_DRAGON)
        execs.append("dragons");
    else if (f1 & TR1_SLAY_DRAGON)
        slays.append("dragons");

    /* Demon slay/execute */
    if (f1 & TR1_KILL_DEMON)
        execs.append("demons");
    else if (f1 & TR1_SLAY_DEMON)
        slays.append("demons");

    /* Undead slay/execute */
    if (f1 & TR1_KILL_UNDEAD)
        execs.append("undead");
    else if (f1 & TR1_SLAY_UNDEAD)
        slays.append("undead");

    if (f1 & (TR1_SLAY_EVIL)) slays.append("all evil creatures");

    /* Describe */
    if (slays.size())
    {
        for (int i = 0; i < slays.size(); i++)  slays[i] = color_string(slays[i], TERM_ORANGE_PEEL);

        /* Output intro */
        output.append("It slays ");

        /* Output list */
        output.append(output_list(slays, FALSE));

        /* Output end (if needed) */
        if (!execs.size()) output.append(".  ");
    }

    if (execs.size())
    {
        for (int i = 0; i < execs.size(); i++)  execs[i] = color_string(execs[i], TERM_BLUE);

        /* Output intro */
        if (slays.size()) output.append(", and it is especially deadly against ");
        else output.append("It is especially deadly against ");

        /* Output list */
        output.append(output_list(execs, FALSE));

        /* Output end */
        output.append(".  ");
    }

    /* We are done here */
    return (output);
}


/*
 * Describe elemental brands.
 */
static QString describe_brand(object_type *o_ptr, u32b f1)
{
    QString output;
    output.clear();

    QVector<QString> descs;
    descs.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect brands */
    if (f1 & (TR1_BRAND_ACID)) descs.append("acid");
    if (f1 & (TR1_BRAND_ELEC)) descs.append("electricity");
    if (f1 & (TR1_BRAND_FIRE)) descs.append("fire");
    if (f1 & (TR1_BRAND_COLD)) descs.append("frost");
    if (f1 & (TR1_BRAND_POIS)) descs.append("poison");

    if (!descs.size()) return (output);

    for (int i = 0; i < descs.size(); i++)  descs[i] = color_string(descs[i], TERM_GREEN);

    /* Describe brands */
    output.append(output_desc_list("It is branded with ", descs, FALSE, TRUE));

    /* We are done here */
    return (output);
}


/*
 * Describe immunities granted by an object.
 */
static QString describe_immune(object_type *o_ptr, u32b f2)
{
    QString output;
    output.clear();

    QVector<QString> descs;
    descs.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect immunities */
    if (f2 & (TR2_IM_ACID)) descs.append("acid");
    if (f2 & (TR2_IM_ELEC)) descs.append("lightning");
    if (f2 & (TR2_IM_FIRE)) descs.append("fire");
    if (f2 & (TR2_IM_COLD)) descs.append("cold");
    if (f2 & (TR2_IM_POIS)) descs.append("poison");

    if (!descs.size()) return (output);

    for (int i = 0; i < descs.size(); i++)  descs[i] = color_string(descs[i], TERM_BLUE);

    /* Describe immunities */
    output.append(output_desc_list("It provides immunity to ", descs, FALSE, TRUE));

    /* We are done here */
    return (output);
}

/*
 * Describe resistances granted by an object.
 */
static QString describe_resist(const object_type *o_ptr, u32b f2, u32b f3)
{
    QString output;
    output.clear();

    QVector<QString> vp;
    vp.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect resistances */
    if ((f2 & (TR2_RES_ACID)) && !(f2 & (TR2_IM_ACID)))
        vp.append("acid");
    if ((f2 & (TR2_RES_ELEC)) && !(f2 & (TR2_IM_ELEC)))
        vp.append("lightning");
    if ((f2 & (TR2_RES_FIRE)) && !(f2 & (TR2_IM_FIRE)))
        vp.append("fire");
    if ((f2 & (TR2_RES_COLD)) && !(f2 & (TR2_IM_COLD)))
        vp.append("cold");
    if ((f2 & (TR2_RES_POIS)) && !(f2 & (TR2_IM_POIS)))
        vp.append("poison");

    if (f2 & (TR2_RES_FEAR))  vp.append("fear");
    if (f2 & (TR2_RES_LIGHT))  vp.append("light");
    if (f2 & (TR2_RES_DARK))  vp.append("dark");
    if (f2 & (TR2_RES_BLIND)) vp.append("blindness");
    if (f2 & (TR2_RES_CONFU)) vp.append("confusion");
    if (f2 & (TR2_RES_SOUND)) vp.append("sound");
    if (f2 & (TR2_RES_SHARD)) vp.append("shards");
    if (f2 & (TR2_RES_NEXUS)) vp.append("nexus");
    if (f2 & (TR2_RES_NETHR)) vp.append("nether");
    if (f2 & (TR2_RES_CHAOS)) vp.append("chaos");
    if (f2 & (TR2_RES_DISEN)) vp.append("disenchantment");
    if (f3 & (TR3_HOLD_LIFE)) vp.append("life draining");

    // Nothing to report
    if (!vp.size()) return (output);

    for (int i = 0; i < vp.size(); i++)  vp[i] = color_string(vp[i], TERM_PURPLE);

    /* Describe resistances */
    output.append(output_desc_list("It provides resistance to ", vp, FALSE, TRUE));

    /* We are done here */
    return (output);
}



/*
 * Describe details of a weapon
 */
static QString describe_weapon(object_type *o_ptr, u32b f1, bool extra_info, bool is_real)
{
    QString output;
    output.clear();

    u16b i;
    int old_blows, new_blows, old_str, old_dex;
    int str_plus = 0;
    int dex_plus = 0;
    int str_done = -1;
    int dd, ds, plus, crit_hit_percent, average;
    u32b reported_brands = 0L;
    QChar plus_minus = '+';
    u16b counter;

    /* The player's hypothetical state, were they to wield this item */
    player_state object_state;
    object_type object_inven[ALL_INVEN_TOTAL];
    object_state.player_state_wipe();

    /* First check if we need this function */
    if (!o_ptr->is_weapon()) return (output);

    /* No descriptions of quest items */
    if (o_ptr->is_quest_object()) return (output);

    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_inven[i].object_copy(&inventory[i]);
    }

    /* Now replace the first slot with the weapon */
    object_inven[INVEN_WIELD] = *o_ptr;

    /* Get the player state */
    calc_bonuses(object_inven, & object_state, TRUE);

    if (is_real)
    {
        /*print out the number of attacks*/
        if (object_state.num_blow == 1) output.append("   It gives you one attack per turn.  ");
        else output.append(QString("   It gives you %1 attacks per turn.  ") .arg(object_state.num_blow));

        if (object_state.heavy_wield)
        {
            output.append("<br>");
            output.append(color_string("   You have trouble wielding such a heavy weapon.<br>", TERM_RED));
        }

        /* Message */
        if (object_state.icky_wield)
        {
            output.append("<br>");
            output.append(color_string("   You do not feel comfortable with this weapon.<br>", TERM_RED));
        }
    }

    if (!extra_info) return (output);

    if (o_ptr->is_known())
    {
        dd = o_ptr->dd;
        ds = o_ptr->ds;
    }
    else
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];
        dd = k_ptr->dd;
        ds = k_ptr->ds;
    }
    plus = object_state.known_to_d + (o_ptr->is_known() ? o_ptr->to_d : 0);
    crit_hit_percent = critical_hit_chance(o_ptr, object_state, TRUE) / (CRIT_HIT_CHANCE / 100);
    average = ((ds + 1) / 2) * dd + plus;

    output.append("<br>");

    if (plus < 0) plus = 0;

    /* Now calculate and print out average damage */
    output.append(QString("   This weapon does %1d%2%3%4 damage (%5 avg) per attack, with a critical hit chance of %6 percent.<br>")
                          .arg(dd)  .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(crit_hit_percent));

    output.append("<br>");

    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(slays_info_nppmoria);
    else counter = N_ELEMENTS(slays_info_nppangband);

    /* Go through each brand and specify the applicable brand multiplier */
    for (i = 0; i < counter; i++)
    {
        const slays_structure *si;
        if (game_mode == GAME_NPPMORIA) si = &slays_info_nppmoria[i];
        else si = &slays_info_nppangband[i];

        if (f1 & (si->slay_flag))
        {
            int new_dd = dd * si->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   This weapon does %1d%2%3%4 damage (%5 avg) against %6 each hit.<br>")
                          .arg(new_dd)  .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
        }
    }

    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(brands_info_nppmoria);
    else counter = N_ELEMENTS(brands_info_nppangband);

    /* Use the hackish slays info to find succeptibilities in Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        for (i = 0; i < counter; i++)
        {
            const slays_structure *si = &brands_info_nppmoria[i];

            /* See if any of the weapons's slays flag matches the monster race flags */
            if (f1 & (si->slay_flag))
            {
                int new_dd = dd * si->multiplier;

                average = ((ds + 1) / 2) * new_dd+ plus;

                output.append(QString("   This weapon does %1d%2%3%4 damage (%5 avg) against creatures who %6 each hit.<br>")
                              .arg(new_dd)  .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
            }
        }
    }

    /* Go through each brand and specify the applicable brand multiplier */
    else for (i = 0; i < counter; i++)
    {
        const brands_structure *bi = &brands_info_nppangband[i];

        /* We already checked this one, there are multiple entries for each flag */
        if (reported_brands & (bi->brand_flag)) continue;

        /* Remember this one so we don't repeat it */
        reported_brands |= (bi->brand_flag);

        if (f1 & (bi->brand_flag))
        {
            int new_dd = dd * bi->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   This weapon does %1d%2%3%4 damage (%5 avg) against creatures who do not %6.<br>")
                          .arg(new_dd)  .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(bi->brand_resist));
        }
    }

    /* Check for increased damage due to monster susceptibility */
    if (game_mode != GAME_NPPMORIA) for (i = 0; i < N_ELEMENTS(mon_suscept); i++)
    {
        const mon_susceptibility_struct *ms = &mon_suscept[i];
        if (f1 & (ms->brand_flag))
        {
            average = ((ds + 1) / 2) * dd;

            output.append(QString("   This weapon does an additional %1d%2 (%3 avg) damage against creatures who are susceptible to %4.<br>")
                          .arg(dd)  .arg(ds) .arg(average)  .arg(ms->brand_susceptibility));
        }
    }

    /* Describe how quickly the player can get additional attacks */
    old_blows = object_state.num_blow;

    /* Record current strength and dex */
    if (game_mode == GAME_NPPMORIA)
    {
        old_str = object_state.stat_loaded_cur[A_STR];
        old_dex = object_state.stat_loaded_cur[A_DEX];
    }
    else
    {
        old_str = object_state.stat_index[A_STR];
        old_dex = object_state.stat_index[A_DEX];
    }

    /* Then we check for extra "real" blows */
    for (dex_plus = 0; dex_plus < 8; dex_plus++)
    {
        for (str_plus = 0; str_plus < 8; str_plus++)
        {
            if (game_mode == GAME_NPPMORIA)
            {
                object_state.stat_loaded_cur[A_STR] = modify_stat_value(old_str, str_plus);
                object_state.stat_loaded_cur[A_DEX] = modify_stat_value(old_dex, dex_plus);
            }
            else
            {
                object_state.stat_index[A_STR] = old_str + str_plus;
                object_state.stat_index[A_DEX] = old_dex + dex_plus;
            }

            new_blows = calc_blows(o_ptr, &object_state);

            /* Calc blows doesn't factor in extra attacks */
            if (f1 & (TR1_BLOWS)) new_blows += o_ptr->pval;

            /*
             * Test to make sure that this extra blow is a
             * new str/dex combination, not a repeat
             */
            if ((new_blows > old_blows) &&
                ((str_plus < str_done) || (str_done == -1)))
            {
                output.append(QString("   With +%1 str and +%2 dex you would get %3 attacks per turn.<br>")
                        .arg(str_plus) .arg(dex_plus) .arg(new_blows));
                str_done = str_plus;
                break;
            }
        }
    }

    return (output);
}

/*
 * Describe details of a weapon
 */
static QString describe_bow_slot(object_type *o_ptr, u32b f3, bool extra_info)
{
    QString output;
    output.clear();

    int dd, ds, plus, crit_hit_percent, mult, average;
    object_type object_type_body;
    object_type *j_ptr = &object_type_body;
    QString j_name;
    QChar plus_minus = '+';

    /* default: SV_SHORT_BOW or SV_LONG_BOW	*/
    QString launcher = "bow";

    /* The player's hypothetical state, were they to wield this item */
    player_state object_state;
    object_type object_inven[ALL_INVEN_TOTAL];
    object_state.player_state_wipe();

    /* First check if we need this function */
    if (!o_ptr->is_bow()) return (output);
    if (o_ptr->is_quest_object()) return (output);

    /* No descriptions of quest items */

    /* Make sure we are calling the launcher by the right name */
    if (o_ptr->sval == SV_SLING) launcher = "sling";
    else if ((o_ptr->sval == SV_LIGHT_XBOW) ||
             (o_ptr->sval == SV_HEAVY_XBOW)) launcher = "crossbow";

    for (int i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_inven[i].object_copy(&inventory[i]);
    }

    /* Now replace the bow slot with the one being examined */
    if (birth_swap_weapons) object_inven[INVEN_MAIN_WEAPON] = *o_ptr;
    else object_inven[INVEN_BOW] = *o_ptr;

    /* Get the player state */
    calc_bonuses(object_inven, &object_state, TRUE);

    /* Assume a standard piece of ammunition for reporting purposes. */
    object_prep(j_ptr, lookup_kind(object_state.ammo_tval, SV_AMMO_NORMAL));
    j_ptr->mark_known(FALSE);

    j_name = object_desc(j_ptr, ODESC_COMBAT | ODESC_PLURAL);

    /*print out the number of attacks*/
    if (object_state.num_fire > 1)
    {
        if (object_state.num_fire == 2) output.append(QString("<br>   You can fire this %1 twice as quickly as an ordinary %2.<br>") .arg(launcher) .arg(launcher));

        else output.append(QString("<br>   You can fire this %1 %2 times more quickly than an ordinary %3.<br>") .arg(launcher) .arg(object_state.num_fire) .arg(launcher));
    }

    if (object_state.heavy_shoot)
    {
        output.append(color_string(QString("   <br>   You have trouble aiming such a heavy %1.<br>") .arg(launcher), TERM_RED));
    }

    if (!extra_info) return (output);

    mult = object_state.ammo_mult;
    dd = j_ptr->dd;
    ds = j_ptr->ds;
    plus = (o_ptr->is_known() ? o_ptr->to_d : 0) + (j_ptr->is_known() ? j_ptr->to_d : 0);

    /* Check for extra damage with a sling for a rogue */
    mult += rogue_shot(j_ptr, &plus, object_state);
    mult += brigand_shot(j_ptr, 0L, FALSE, object_state);

    dd *= mult;
    plus *= mult;
    crit_hit_percent = critical_shot_chance(o_ptr, object_state, FALSE, TRUE, f3) / (CRIT_HIT_CHANCE / 100);

    output.append("<br>");

    if (plus < 0) plus = 0;

    average = ((ds + 1) / 2) * dd + plus;

    /* Now calculate and print out average damage */
    output.append(QString("   Firing %1 from this %2 does %3d%4%5%6 damage (%7 avg), with a critical hit chance of %8 percent.<br>")
                   .arg(j_name) .arg(launcher) .arg(dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(crit_hit_percent));

    output.append("<br>");

    return (output);
}

/*
 * Describe details of a weapon
 */
static QString describe_ammo(object_type *o_ptr, u32b f1, u32b f3, bool extra_info)
{
    QString output;
    output.clear();

    u16b i;
    int dd, ds, plus, crit_hit_percent, mult, average;
    object_type object_type_body;
    object_type *j_ptr = &object_type_body;
    QString j_name;
    u32b reported_brands = 0L;
    QChar plus_minus = '+';
    u16b counter;

    /* The player's hypothetical state, were they to wield this item */
    player_state object_state;
    object_type object_inven[ALL_INVEN_TOTAL];
    object_state.player_state_wipe();

    /* First check if we need this function */
    if (!o_ptr->is_ammo()) return (output);

    /* No descriptions of quest items */
    if (o_ptr->is_quest_object()) return (output);

    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_inven[i].object_copy(&inventory[i]);
    }

    /* Now replace the bow slot with the one being examined */
    if (birth_swap_weapons) j_ptr = &object_inven[INVEN_MAIN_WEAPON];
    else j_ptr = &object_inven[INVEN_BOW];

    /* Get the player state */
    calc_bonuses(object_inven, &object_state, TRUE);

    /* Make sure we are working with the right launcher type */
    if (o_ptr->tval != object_state.ammo_tval)
    {
        /* Make a dummy launcher for ammo evaluation purposes */
        byte sval = SV_LONG_BOW;
        if (o_ptr->tval == TV_SHOT) sval = SV_SLING;
        else if (o_ptr->tval == TV_BOLT) sval = SV_LIGHT_XBOW;

        j_ptr->object_wipe();

        /* Assume a standard piece of ammunition for reporting purposes. */
        object_prep(j_ptr, lookup_kind(TV_BOW, sval));
        j_ptr->mark_known(FALSE);

        /* Re-do the player state */
        calc_bonuses(object_inven, &object_state, TRUE);
    }

    j_name = object_desc(j_ptr, ODESC_PREFIX | ODESC_COMBAT);

    if (!extra_info) return (output);

    output.append("<br>");

    mult = object_state.ammo_mult;

    if (o_ptr->is_known())
    {
        dd = o_ptr->dd;
        ds = o_ptr->ds;
    }
    else
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];
        dd = k_ptr->dd;
        ds = k_ptr->ds;
    }
    plus = (o_ptr->is_known() ? o_ptr->to_d : 0) + (j_ptr->is_known() ? j_ptr->to_d : 0);

    /* Check for extra damage with a sling for a rogue */
    mult += rogue_shot(o_ptr, &plus, object_state);
    mult += brigand_shot(o_ptr, 0L, FALSE, object_state);

    dd *= mult;
    plus *= mult;
    crit_hit_percent = critical_shot_chance(o_ptr, object_state, FALSE, TRUE, f3) / (CRIT_HIT_CHANCE / 100);

    output.append("<br>");

    if (plus < 0) plus = 0;

    average = ((ds + 1) / 2) * dd + plus;

    /* Now calculate and print out average damage */
    output.append(QString("   Firing this ammunition from %1 does %2d%3%4%5 damage (%6 avg), with a critical hit chance of %7 percent.<br>")
                  .arg(j_name) .arg(dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(crit_hit_percent));

    output.append("<br>");

    counter = N_ELEMENTS(slays_info_nppangband);
    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(slays_info_nppmoria);

    /* Go through each brand and specify the applicable brand multiplier */
    for (i = 0; i < counter; i++)
    {
        const slays_structure *si;
        if (game_mode == GAME_NPPMORIA) si = &slays_info_nppmoria[i];
        else si = &slays_info_nppangband[i];

        if (f1 & (si->slay_flag))
        {
            int new_dd = dd * si->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   This ammunition does %1d%2%3%4 damage (%5 avg) against %6 each hit.<br>")
                          .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
        }
    }

    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(brands_info_nppmoria);
    else counter = N_ELEMENTS(brands_info_nppangband);

    /* Use the hackish slays info to find succeptibilities in Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        for (i = 0; i < counter; i++)
        {
            const slays_structure *si = &brands_info_nppmoria[i];

            /* See if any of the weapons's slays flag matches the monster race flags */
            if (f1 & (si->slay_flag))
            {
                int new_dd = dd * si->multiplier;

                average = ((ds + 1) / 2) * new_dd + plus;

                output.append(QString("   This ammunition does %1d%2%3%4 damage (%5 avg) against creatures who %6 each hit.<br>")
                              .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
            }
        }
    }


    /* Go through each brand and specify the applicable brand multiplier */
    else for (i = 0; i < counter; i++)
    {
        const brands_structure *bi = &brands_info_nppangband[i];

        /* We already checked this one, there are multiple entries for each flag */
        if (reported_brands & (bi->brand_flag)) continue;

        /* Remember this one so we don't repeat it */
        reported_brands |= (bi->brand_flag);

        if (f1 & (bi->brand_flag))
        {
            int new_dd = dd * bi->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   This ammunition does %1d%2%3%4 damage (%5 avg) against creatures who do not %6.<br>")
                          .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(bi->brand_resist));
        }
    }

    /* Check for increased damage due to monster susceptibility */
    if (game_mode != GAME_NPPMORIA) for (i = 0; i < N_ELEMENTS(mon_suscept); i++)
    {
        const mon_susceptibility_struct *ms = &mon_suscept[i];
        if (f1 & (ms->brand_flag))
        {
            average = ((ds + 1) / 2) * dd;

            output.append(QString("   This ammunition does an additional %1d%2 damage (%3 avg) against creatures who are susceptible to %4.<br>")
                          .arg(dd) .arg(ds) .arg(average) .arg(ms->brand_susceptibility));
        }
    }

    return (output);
}

/*
 * Describe details of a weapon
 */
static QString describe_throwing_weapon(object_type *o_ptr, u32b f1, u32b f3, bool extra_info)
{
    QString output;
    output.clear();

    u16b i;
    int dd, ds, plus, crit_hit_percent, mult, average;
    u32b reported_brands = 0L;
    char plus_minus = '+';
    u16b counter;

    /* The player's hypothetical state, were they to throw this item */
    player_state object_state;
    object_type object_inven[ALL_INVEN_TOTAL];
    object_state.player_state_wipe();

    /* First check if we need this function */
    if (!is_throwing_weapon(o_ptr)) return (output);

    /* No descriptions of quest items */
    if (o_ptr->is_quest_object()) return (output);

    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_inven[i].object_copy(&inventory[i]);
    }

    /* Get the player state */
    calc_bonuses(object_inven, &object_state, TRUE);

    //output.append("<br>");

    if (!extra_info) return (output);

    if (o_ptr->is_known())
    {
        dd = o_ptr->dd;
        ds = o_ptr->ds;
    }
    else
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];
        dd = k_ptr->dd;
        ds = k_ptr->ds;
    }
    plus = (o_ptr->is_known() ? o_ptr->to_d : 0) + p_ptr->state.known_to_d; ;

    /* Apply the throwing weapon bonus. */
    mult = weapon_throw_adjust(o_ptr, f3, &plus, TRUE);

    dd *= mult;

    crit_hit_percent = critical_shot_chance(o_ptr, object_state, TRUE, TRUE, f3) / (CRIT_HIT_CHANCE / 100);

    //output.append("<br>");

    if (plus < 0)
    {
        plus_minus = '-';
        plus *= -1;
    }

    average = ((ds + 1) / 2) * dd + plus;

    /* Now calculate and print out average damage */
    output.append(QString("   Throwing this weapon does %1d%2%3%4 damage (%5 avg), with a critical hit chance of %6 percent.<br>")
                    .arg(dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(crit_hit_percent));

    //output.append("<br>");

    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(slays_info_nppmoria);
    else counter = N_ELEMENTS(slays_info_nppangband);


    /* Go through each brand and specify the applicable brand multiplier */
    for (i = 0; i < counter; i++)
    {
        const slays_structure *si;
        if (game_mode == GAME_NPPMORIA) si = &slays_info_nppmoria[i];
        else si = &slays_info_nppangband[i];

        if (f1 & (si->slay_flag))
        {
            int new_dd = dd * si->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   Throwing this weapon does %1d%2%3%4 damage (%5 avg) against %6 each hit.<br>")
                          .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
        }
    }

    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(brands_info_nppmoria);
    else counter = N_ELEMENTS(brands_info_nppangband);

    /* Use the hackish slays info to find succeptibilities in Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        for (i = 0; i < counter; i++)
        {
            const slays_structure *si = &brands_info_nppmoria[i];

            /* See if any of the weapons's slays flag matches the monster race flags */
            if (f1 & (si->slay_flag))
            {
                int new_dd = dd * si->multiplier;

                average = ((ds + 1) / 2) * new_dd + plus;

                output.append(QString("   Throwing this weapon does %1d%2%3%4 damage (%5 avg) against creatures who %6 each hit.<br>")
                              .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(si->slay_race));
            }
        }
    }


    /* Go through each brand and specify the applicable brand multiplier */
    else for (i = 0; i < counter; i++)
    {
        const brands_structure *bi = &brands_info_nppangband[i];

        /* We already checked this one, there are multiple entries for each flag */
        if (reported_brands & (bi->brand_flag)) continue;

        /* Remember this one so we don't repeat it */
        reported_brands |= (bi->brand_flag);

        if (f1 & (bi->brand_flag))
        {
            int new_dd = dd * bi->multiplier;

            average = ((ds + 1) / 2) * new_dd + plus;

            output.append(QString("   Throwing this weapon does %1d%2%3%4 damage (%5 avg) against creatures who do not %6.<br>")
                            .arg(new_dd) .arg(ds) .arg(plus_minus) .arg(plus) .arg(average) .arg(bi->brand_resist));
        }
    }

    /* Check for increased damage due to monster susceptibility */
    if (game_mode != GAME_NPPMORIA) for (i = 0; i < N_ELEMENTS(mon_suscept); i++)
    {
        const mon_susceptibility_struct *ms = &mon_suscept[i];
        if (f1 & (ms->brand_flag))
        {
            average = ((ds + 1) / 2) * dd;

            output.append(QString("   Throwing this weapon does an additional %1d%2 damage (%3 avg) against creatures who are susceptible to %4.<br>")
                            .arg(dd) .arg(ds) .arg(average) .arg(ms->brand_susceptibility));
        }
    }

    return (output);
}



/*
 * Describe 'ignores' of an object.
 */
static QString describe_ignores(object_type *o_ptr, u32b f3)
{
    QString output;
    output.clear();

    QVector<QString> list;
    list.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect the ignores */
    if (f3 & (TR3_IGNORE_ACID)) list.append("acid");
    if (f3 & (TR3_IGNORE_ELEC)) list.append("electricity");
    if (f3 & (TR3_IGNORE_FIRE)) list.append("fire");
    if (f3 & (TR3_IGNORE_COLD)) list.append("cold");

    // Nothing to report
    if (!list.size()) return (output);

    /* Describe ignores */
    if (list.size())
        output.append("It cannot be harmed by the elements.  ");
    else
        output.append(output_desc_list("It cannot be harmed by ", list, TRUE, TRUE));

    return (output);
}


/*
 * Describe stat sustains.
 */
static QString describe_sustains(object_type *o_ptr, u32b f2)
{
    QString output;
    output.clear();

    QVector<QString> list;
    list.clear();

    /* Unused parameter */
    (void)o_ptr;

    /* Collect the sustains */
    if (f2 & (TR2_SUST_STR)) list.append(stat_names_full[A_STR]);
    if (f2 & (TR2_SUST_INT)) list.append(stat_names_full[A_INT]);
    if (f2 & (TR2_SUST_WIS)) list.append(stat_names_full[A_WIS]);
    if (f2 & (TR2_SUST_DEX)) list.append(stat_names_full[A_DEX]);
    if (f2 & (TR2_SUST_CON)) list.append(stat_names_full[A_CON]);
    if (f2 & (TR2_SUST_CHR)) list.append(stat_names_full[A_CHR]);

    // Nothing to report
    if (!list.size()) return (output);

    /* Describe sustains */
    if (list.size() == A_MAX) output.append("It sustains all your stats.  ");
    else output.append(output_desc_list("It sustains your ", list, FALSE, TRUE));

    /* We are done here */
    return (output);
}


/*
 * Describe miscellaneous powers such as see invisible, free action,
 * permanent light, etc; also note curses and penalties.
 */
static QString describe_misc_magic(object_type *o_ptr, u32b f3)
{
    QString output;
    output.clear();

    QVector<QString> good;
    QVector<QString> bad;
    good.clear();
    bad.clear();

    /* Throwing weapons. */
    if (f3 & (TR3_THROWING))
    {
        if (o_ptr->ident & IDENT_PERFECT_BALANCE)
        {
            good.append(color_string("can be thrown hard and fast", TERM_GREEN));
        }
        else good.append(color_string("can be thrown effectively", TERM_GREEN));
    }

    /* Collect stuff which can't be categorized */
    if (f3 & (TR3_BLESSED))     good.append(color_string("is blessed by the gods", TERM_GREEN));
    if (f3 & (TR3_IMPACT))      good.append(color_string("creates earthquakes on impact", TERM_GREEN));
    if (f3 & (TR3_SLOW_DIGEST)) good.append(color_string("slows your metabolism", TERM_GREEN));
    if (f3 & (TR3_FEATHER))     good.append(color_string("makes you fall like a feather", TERM_GREEN));
    if (((o_ptr->tval == TV_LIGHT) && o_ptr->is_artifact()) || (f3 & (TR3_LIGHT)))
        good.append(color_string("lights the dungeon around you", TERM_GREEN));
    if (f3 & (TR3_REGEN))       good.append(color_string("speeds your regeneration", TERM_GREEN));

    /* Describe */
    output.append(output_desc_list("It ", good, FALSE, TRUE));

    /* Collect granted powers */
    good.clear();
    if (f3 & (TR3_FREE_ACT))  good.append(color_string("immunity to paralysis", TERM_GREEN));
    if (f3 & (TR3_TELEPATHY)) good.append(color_string("the power of telepathy", TERM_GREEN));
    if (f3 & (TR3_SEE_INVIS)) good.append(color_string("the ability to see invisible things", TERM_GREEN));

    /* Collect penalties */
    if (f3 & (TR3_AGGRAVATE)) bad.append(color_string("aggravates creatures around you", TERM_L_RED));
    if (f3 & (TR3_DRAIN_EXP)) bad.append(color_string("drains experience", TERM_L_RED));
    if (f3 & (TR3_TELEPORT))  bad.append(color_string("induces random teleportation", TERM_L_RED));

    /* Deal with cursed stuff */
    if (o_ptr->is_cursed())
    {
        if (f3 & (TR3_PERMA_CURSE)) bad.append(color_string("is permanently cursed", TERM_L_RED));
        else if (f3 & (TR3_HEAVY_CURSE)) bad.append(color_string("is heavily cursed", TERM_L_RED));
        else if (o_ptr->is_known()) bad.append(color_string("is cursed", TERM_L_RED));
    }

    /* Describe */
    if (good.size())
    {
        /* Output intro */
        output.append("It grants you ");

        /* Output list */
        output.append(output_list(good, FALSE));

        /* Output end (if needed) */
        if (!bad.size()) output.append(".  ");
    }

    if (bad.size())
    {
        /* Output intro */
        if (good.size()) output.append(", but it also ");
        else output.append("It ");

        /* Output list */
        output.append(output_list(bad, FALSE));

        /* Output end */
        output.append(".  ");
    }

    /* Return output */
    return (output);
}


static QString act_description[ACT_MAX] =
{
    "illumination",
    "magic mapping",
    "clairvoyance",
    "protection from evil",
    "dispel evil (x5)",
    "heal (500)",
    "heal (1000)",
    "cure wounds (4d8)",
    "haste self (20+d20 turns)",
    "haste self (75+d75 turns)",
    "fire bolt (9d8)",
    "fire ball (72)",
    "large fire ball (120)",
    "frost bolt (6d8)",
    "frost ball (48)",
    "frost ball (100)",
    "frost bolt (12d8)",
    "large frost ball (200)",
    "acid bolt (5d8)",
    "recharge item I",
    "mass sleep",
    "lightning bolt (4d8)",
    "large lightning ball (250)",
    "banishment",
    "mass banishment",
    "*identify*",
    "drain life (90)",
    "drain life (120)",
    "bizarre things",
    "star ball (150)",
    "berserk rage, bless, and resistance",
    "phase door",
    "door and trap destruction",
    "detection",
    "resistance (20+d20 turns)",
    "teleport",
    "restore life levels",
    "magic missile (2d6)",
    "a magical arrow (150)",
    "remove fear and cure poison",
    "stinking cloud (12)",
    "stone to mud",
    "teleport away",
    "word of recall",
    "confuse monster",
    "probing",
    "fire branding of bolts",
    "starlight (10d8)",
    "mana bolt (12d8)",
    "berserk rage (50+d50 turns)",
    "resist acid (20+d20 turns)",
    "resist electricity (20+d20 turns)",
    "resist fire (20+d20 turns)",
    "resist cold (20+d20 turns)",
    "resist poison (20+d20 turns)"
};

/*
 * Determine the "Activation" (if any) for an artifact
 */
static QString describe_item_activation(object_type *o_ptr)
{
    QString output;
    output.clear();

    u16b value;

    /* Require activation ability */
    if (!(o_ptr->known_obj_flags_3 & TR3_ACTIVATE)) return(output);

    /* Artifact activations */
    if ((o_ptr->art_num) && (o_ptr->art_num < z_info->art_norm_max))
    {
        artifact_type *a_ptr = &a_info[o_ptr->art_num];

        bool drag_armor = FALSE;

        /* Paranoia */
        if (a_ptr->activation >= ACT_MAX)
        {
            if ((a_ptr->tval == TV_DRAG_ARMOR) ||
                (a_ptr->tval == TV_DRAG_SHIELD))
                 drag_armor = TRUE;
            else return(output);
        }

        /* Some artifacts can be activated */
        if (!drag_armor)
        {
            output.append(act_description[a_ptr->activation]);

            /* Output the number of turns */
            if (a_ptr->time && a_ptr->randtime)
                output.append(QString(" every %1+d%2 turns.") .arg(a_ptr->time) .arg(a_ptr->randtime));
            else if (a_ptr->time)
                output.append(QString(" every %1 turns.") .arg(a_ptr->time));
            else if (a_ptr->randtime)
                output.append(QString(" every d%1 turns.") .arg(a_ptr->randtime));

            return(output);
        }
    }

    /* Now do the rings */
    if (o_ptr->tval == TV_RING)
    {
        /* Branch on the sub-type */
        switch (o_ptr->sval)
        {
            case SV_RING_ACID:
            {
                output.append(QString("acid resistance (20+d20 turns) and acid ball (70) every 50+d50 turns"));
                break;
            }
            case SV_RING_FLAMES:
            {
                output.append(QString("fire resistance (20+d20 turns) and fire ball (80) every 50+d50 turns."));
                break;
            }
            case SV_RING_ICE:
            {
                output.append(QString("cold resistance (20+d20 turns) and cold ball (75) every 50+d50 turns."));
                break;
            }

            case SV_RING_LIGHTNING:
            {
                output.append(QString("electricity resistance (20+d20 turns) and electricity ball (85) every 50+d50 turns."));
                break;
            }
        }

        return (output);
    }

    /* Require dragon scale mail */
    if ((o_ptr->tval != TV_DRAG_ARMOR) &&
        (o_ptr->tval != TV_DRAG_SHIELD)) return(output);

    /*Bigger the dragon scale mail, the bigger the damage & re-charge*/
    value = o_ptr->sval;

    /*Armor is more powerful than shields*/
    if (o_ptr->tval == TV_DRAG_ARMOR) value *= 2;

    /* Branch on the sub-type */
    switch (o_ptr->ego_num)
    {

        case EGO_DRAGON_BLUE:
        {
            value *= 50;

            output.append(QString("electricity resistance (10+d10 turns) and breathe lightning (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_WHITE:
        {
            value *= 50;

            output.append(QString("cold resistance (10+d10 turns) and breathe frost (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));

            break;
        }
        case EGO_DRAGON_BLACK:
        {
            value *= 50;

            output.append(QString("acid resistance (10+d10 turns) and breathe acid (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));

            break;
        }
        case EGO_DRAGON_GREEN:
        {
            value *= 50;

            output.append(QString("poison resistance (10+d10 turns) and breathe poison gas (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));

            break;
        }
        case EGO_DRAGON_RED:
        {
            value *= 50;

            output.append(QString("fire resistance (10+d10 turns) and breathe fire (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));

            break;
        }
        case EGO_DRAGON_MULTIHUED:
        {
            value *= 75;

            output.append(QString("resistance (20+d20 turns) and breathe multi-hued (%1) every %2+d%3 turns.") .arg(value) .arg(value * 3 / 4) .arg(value * 3 / 4));

            break;
        }
        case EGO_DRAGON_BRONZE:
        {
            value *= 50;

            output.append(QString("breathe confusion (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_GOLD:
        {
            value *= 50;

            output.append(QString("breathe sound (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_CHAOS:
        {
            value *= 60;

            output.append(QString("breathe chaos/disenchant (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_LAW:
        {
            value *= 60;

            output.append(QString("breathe sound/shards (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
        }
        case EGO_DRAGON_BALANCE:
        {
            value *= 75;

            output.append(QString("breathe balance (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_PSEUDO:
        {
            value *= 65;

            output.append(QString("breathe light/darkness (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        case EGO_DRAGON_POWER:
        {
            value *= 100;

            output.append(QString("breathe the elements (%1) every %2+d%3 turns.") .arg(value) .arg(value) .arg(value));
            break;
        }
        default:
        {
            break;
        }
    }


    return (output);
}

/*
 * Describe object nativity.
 */
static QString describe_nativity(object_type *o_ptr, u32b fn)
{
    QString output;
    output.clear();

    QVector<QString> vp;
    vp.clear();

    /* Unused parameter */
    (void)o_ptr;

    if (fn & (TN1_NATIVE_LAVA))  vp.append("lava");
    if (fn & (TN1_NATIVE_ICE))  vp.append("ice");
    if (fn & (TN1_NATIVE_OIL))  vp.append("oil");
    if (fn & (TN1_NATIVE_FIRE)) vp.append("fire");
    if (fn & (TN1_NATIVE_SAND)) vp.append("sand");
    if (fn & (TN1_NATIVE_FOREST)) vp.append("forest");
    if (fn & (TN1_NATIVE_WATER)) vp.append("water");
    if (fn & (TN1_NATIVE_ACID)) vp.append("acid");
    if (fn & (TN1_NATIVE_MUD)) vp.append("mud");

    // Nothing to report
    if (!vp.size()) return (output);

    /* Describe nativities */
    output.append(output_desc_list("It makes you native to terrains made of ", vp, FALSE, TRUE));

    /* We are done here */
    return (output);
}




/*
 * Describe an object's activation, if any.
 */
static QString describe_activation(object_type *o_ptr, u32b f3)
{
    QString output;
    output.clear();

    /* Check for the activation flag */
    if (f3 & TR3_ACTIVATE)
    {
        char act_desc[120];

        u16b size;

        output.append("It activates for ");

        /*get the size of the file*/
        size = strlen(act_desc);

        output.append(describe_item_activation(o_ptr));

        /*if the previous function added length, we have an activation, so print it out*/
        if (strlen(act_desc) > size)
        {

            output.append(".  ");

            /*print it out*/
            output.append(act_desc);
        }
    }

    /* No activation */
    return (output);
}


/*
 * Output object information
 * With extra_info true, it gives information about the weapon attack damage.
 * False is intended for things like character dumps.
 * is_real tells whether the object if a template or a real in-game weapon.
 */
QString object_info_out(object_type *o_ptr,  bool extra_info, bool is_real)
{
    QString output;
    output.clear();
    QString next_output;

    // Make sure the flags are up-to-date
    o_ptr->update_object_flags();

    /* Describe the object */
    next_output = describe_stats(o_ptr, o_ptr->known_obj_flags_1);
    next_output.append(describe_secondary(o_ptr, o_ptr->known_obj_flags_1));
    next_output.append(describe_slay(o_ptr, o_ptr->known_obj_flags_1));
    next_output.append(describe_brand(o_ptr, o_ptr->known_obj_flags_1));
    output = next_output;
    if (next_output.length()) output.append("<br>");
    next_output = describe_immune(o_ptr, o_ptr->known_obj_flags_2);
    next_output.append(describe_resist(o_ptr, o_ptr->known_obj_flags_2, o_ptr->known_obj_flags_3));
    next_output.append(describe_sustains(o_ptr, o_ptr->known_obj_flags_2));
    next_output.append(describe_misc_magic(o_ptr, o_ptr->known_obj_flags_3));
    next_output.append(describe_nativity(o_ptr, o_ptr->known_obj_flags_native));
    next_output.append(describe_ignores(o_ptr, o_ptr->known_obj_flags_3));
    output.append(next_output);
    if (next_output.length()) output.append("<br>");
    next_output = describe_activation(o_ptr, o_ptr->known_obj_flags_3);
    output.append(next_output);
    if (next_output.length()) output.append("<br>");
    next_output.clear();
    next_output.append(describe_weapon(o_ptr, o_ptr->known_obj_flags_1, extra_info, is_real));
    next_output.append(describe_bow_slot(o_ptr, o_ptr->known_obj_flags_3, extra_info));
    next_output.append(describe_ammo(o_ptr, o_ptr->known_obj_flags_1, o_ptr->known_obj_flags_3, extra_info));
    next_output.append(describe_throwing_weapon(o_ptr, o_ptr->known_obj_flags_1, o_ptr->known_obj_flags_3, extra_info));
    output.append(next_output);

    /* Unknown extra powers (artifact) */
    if (o_ptr->is_known() && (!(o_ptr->ident & IDENT_MENTAL)) &&
        (o_ptr->has_hidden_powers() || o_ptr->is_artifact()))
    {
        output.append("<br>It might have hidden powers.");
    }

    /* We are done. */
    return (output);
}



/*
 * Header for additional information when printing to screen.
 */
QString screen_out_head(object_type *o_ptr)
{
    QString output;
    output.clear();
    QString o_name;

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Description */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
    o_name = capitalize_first(o_name);

    QChar d_char = k_ptr->get_char();
    QColor d_color = k_ptr->get_color();

    QString obj_symbol = color_string(d_char, d_color);

    /* Print, in colour */
    output.append(QString("<b><h1><span style='background-color: black;'>'%1'</span> - %2</h1></b><br><br>") .arg(obj_symbol) .arg(o_name));

    /* Display the known artifact description */
    if (!birth_rand_artifacts &&
        o_ptr->is_known_artifact() && !a_info[o_ptr->art_num].a_text.isEmpty())
    {
        output.append("<br><br>   ");
        output.append(a_info[o_ptr->art_num].a_text);
    }
    /* Display the known object description */
    else if (o_ptr->is_aware() || o_ptr->is_known())
    {
        if (!k_info[o_ptr->k_idx].k_text.isEmpty())
        {
            output.append("<br><br>   ");
            output.append(k_info[o_ptr->k_idx].k_text);
        }

        /* Display an additional ego-item description */
        if (o_ptr->is_ego_item() && (o_ptr->is_known()) &&
                !e_info[o_ptr->ego_num].e_text.isEmpty())
        {
            output.append("<br><br>   ");
            output.append(e_info[o_ptr->ego_num].e_text);
        }
    }

    output.append("<br><br>");

    return (output);
}


/*
 * Place an item description on the screen.
 */
QString get_object_description(object_type *o_ptr)
{
    QString output = screen_out_head(o_ptr);
    QString o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Dump the info */
    output.append(object_info_out(o_ptr, TRUE, TRUE));

    if (!o_ptr->is_known())
    {
        output.append("<br>   This item has not been identified.");
    }
    else if (output.isEmpty() && (o_ptr->tval != cp_ptr->spell_book))
    {
        output.append("<br>   This item does not seem to possess any special abilities.");
    }

    QString buf;
    int price;

    buf.clear();

    /* Show object history if possible */
    buf.append(format_object_history(o_ptr));

    if (!buf.isEmpty())
    {
        buf.append("<br>   ");

        buf = capitalize_first(buf);

        output.append(color_string(buf, TERM_BLUE));
    }

    if (o_ptr->number > 1)
    {
        output.append(QString("<br>   They weigh %1.<br>") .arg(format_object_weight(o_ptr)));
    }
    else output.append(QString("<br>   It weighs %1.<br>") .arg(format_object_weight(o_ptr)));

    /* Print resale value */
    output.append("<br>  ");
    price = object_value(o_ptr);
    if (price > 0)
    {
        if (o_ptr->number > 1)
        {
            output.append(QString("They would fetch %1 gold apiece in an average shop.<br>") .arg(price));
        }
        else
        {
            output.append(QString("It would fetch %1 gold in an average shop.<br>") .arg(price));
        }
    }
    else
    {
        if (o_ptr->number > 1)	output.append(QString("They have no value.<br>"));
        else 					output.append(QString("It has no value.<br>"));
    }

    return (output);
}

/*
 * Place an item description on the screen.
 */
void object_info_screen(object_type *o_ptr)
{
    /* Hack -- Browse book */
    if (o_ptr->tval == cp_ptr->spell_book)
    {
        /* Call the aux function */
        do_cmd_browse(o_ptr->sval);
        return;
    }

    QString output = get_object_description(o_ptr);

    /* Finally, display it */
    display_info_window(DISPLAY_INFO_OBJECT, o_ptr->k_idx, output);

    return;
}

/* Append the depth of an history item to the given buffer */
static QString history_depth(s16b depth)
{
    QString output;
    output.clear();

    if (depth == 0)
    {
        output.append(" in the town.");
    }
    else output.append(QString(" at a depth of %1 ft.") .arg(depth * 50));

    return(output);
}

/*
 * Format the item history as text and put it on the given buffer
 */
QString format_object_history(object_type *o_ptr)
{
    QString output;
    output.clear();

    if (o_ptr->number > 1) output.append("They were");
    else output.append("It was");

    switch (o_ptr->origin_nature)
    {
        case ORIGIN_NONE: case ORIGIN_MIXED:
        {
            /* Don't display anything */
            output.clear();
            return (output);
        }
        case ORIGIN_BIRTH:
        {
            output.append(" an inheritance from your family.");
            break;
        }
        case ORIGIN_STORE:
        {
            /* Hack -- Don't display store objects inside stores */
            if (o_ptr->ident & (IDENT_STORE))
            {
                output.clear();
                return (output);
            }

            output.append(" bought in a store.");
            break;
        }
        case ORIGIN_MORGOTH:
        {
            output.append("It is your bounty from defeating Morgoth!");
            break;
        }
        case ORIGIN_CHEAT:
        {
            output.append("-- Created by debug option --");
            break;
        }
        case ORIGIN_FLOOR:
        {
            output.append(" lying on the floor");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_ACQUIRE:
        {
            output.append(" conjured forth by magic");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_CHEST:
        {
            output.append(" found in a chest");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_MAGIC:
        {
            output.append(" created magically");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_DROP_UNKNOWN:
        {
            output.append(" dropped by an unknown monster");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_REWARD:
        {
            output.append(" a reward from the guild for your exploits");
            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
        case ORIGIN_DROP_KNOWN:
        {
            monster_race *r_ptr = &r_info[o_ptr->origin_r_idx];
            QString name = r_ptr->r_name_full;
            QString article = "";

            /* Get an article for non-uniques */
            if (!(r_ptr->flags1 & (RF1_UNIQUE)))
            {
                article = (begins_with_vowel(name) ? "an " : "a ");
            }

            /* Stored name */
            if (!o_ptr->origin_m_name.isEmpty())
            {
                name = o_ptr->origin_m_name;
            }

            output.append(QString(" dropped by %1%2") .arg(article) .arg(name));

            output.append(history_depth(o_ptr->origin_dlvl));
            break;
        }
    }

    output.prepend("<br>");

    return (output);
}




/*
 * Check whether the history is interesting
 */
bool history_interesting(object_type *o_ptr)
{
    /* Empty slots are always boring */
    if (!o_ptr->k_idx) return FALSE;

    /* Items with no, or mixed, origins are always boring */
    if (!o_ptr->origin_nature || (o_ptr->origin_nature == ORIGIN_MIXED)) return FALSE;

    /* Artifacts are always interesting */
    if (o_ptr->art_num) return TRUE;

    /* Ego items are interesting if they're good */
    /*if (o_ptr->name2 && (object_value(o_ptr) > 0)) return TRUE;*/

    /* Hack -- Valuable objects are always interesting -DG */
    if (object_value(o_ptr) >= 10000) return TRUE;

    /* Objects dropped by uniques are always interesting */
    if ((o_ptr->origin_r_idx > 0) && (r_info[o_ptr->origin_r_idx].flags1 & (RF1_UNIQUE))) return TRUE;

    /* Cheat items are always interesting */
    if (o_ptr->origin_nature == ORIGIN_CHEAT) return TRUE;

    /* Rewards are always interesting */
    if (o_ptr->origin_nature == ORIGIN_REWARD) return TRUE;

    /* Some other origins usually are boring */
    if ((o_ptr->origin_nature == ORIGIN_BIRTH) || (o_ptr->origin_nature == ORIGIN_STORE))
        return FALSE;

    /* Objects OOD by more than ten levels are interesting */
    if ((o_ptr->origin_dlvl + INTEREST_OFFSET) < k_info[o_ptr->k_idx].k_level) return TRUE;

    return FALSE;
}

// Provide an object description for character dumps and dialog boxes.
QString identify_random_gen(object_type *o_ptr)
{
    QString obj_string;
    obj_string.clear();
    obj_string.append(object_info_out(o_ptr, TRUE, TRUE));
    obj_string.remove(QString("<br>"));
    if (history_interesting(o_ptr))
    {
        QString obj_history = QString("  %1<br>") .arg(format_object_history(o_ptr));
        obj_string.append(obj_history);
    }

    //Remove the html breaks and excess spaces.
    obj_string.replace(QString("   "), QString("  "));

    return(obj_string);
}

/*
 * Show artifact lore
 */
void desc_art_fake(int a_idx)
{
    object_type *o_ptr;
    object_type object_type_body;
    bool lost = TRUE;
    int i, j;

    /* Get local object */
    o_ptr = &object_type_body;
    o_ptr->object_wipe();

    /* Look for the artifact, either in inventory, store or the object list */
    for (i = 0; i < z_info->o_max; i++)
    {
        if (o_list[i].art_num == a_idx)
        {
            o_ptr = &o_list[i];
            lost = FALSE;
            break;
        }
    }

    if (lost)
    {
        for (i = 0; i < INVEN_TOTAL; i++)
        {
            if (inventory[i].art_num == a_idx)
            {
                o_ptr = &inventory[i];
                lost = FALSE;
                break;
            }
        }
    }

    if (lost)
    {
        for (j = 0; j < MAX_STORES; j++)
        {
            for (i = 0; i < store[j].stock_size; i++)
            {
                if (store[j].stock[i].art_num == a_idx)
                {
                    o_ptr = &store[j].stock[i];
                    lost = FALSE;
                    break;
                }
            }
            if (!lost) break;
        }
    }

    /* If it's been lost, make a fake artifact for it */
    if (lost)
    {
        make_fake_artifact(o_ptr, a_idx);
    }

    /* Print the artifact information */
    object_info_screen(o_ptr);

}
