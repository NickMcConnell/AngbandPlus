/* File: borg4.c */
/*  Purpose: Notice and Power code for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"
#include "borg3.h"
#include "borg4.h"

bool borg_full_damage;  /* make danger = full damage. */

/*
 * Note that we assume that any item with quantity zero does not exist,
 * thus, when simulating possible worlds, we do not actually have to
 * "optimize" empty slots.
 *
 * XXX XXX XXX Also, we could reward equipment based on possible enchantment,
 * up to the maximal amount available in the home, which would induce item
 * switching when the item could be enchanted sufficiently.
 */


/*
 * The "notice" functions examine various aspects of the player inventory,
 * the player equipment, or the home contents, and extract various numerical
 * quantities based on those aspects, adjusting them for various "abilities",
 * such as the ability to cast certain spells, etc.
 *
 * The "power" functions use the numerical quantities described above, and
 * use them to do two different things:  (1) rank the "value" of having
 * various abilities relative to the possible "money" reward of carrying
 * sellable items instead, and (2) rank the value of various abilities
 * relative to each other, which is used to determine what to wear/buy,
 * and in what order to wear/buy those items.
 *
 * These functions use some very heuristic values, by the way...
 *
 * We should probably take account of things like possible enchanting
 * (especially when in town), and items which may be found soon.
 *
 * We consider several things:
 *   (1) the actual "power" of the current weapon and bow
 *   (2) the various "flags" imparted by the equipment
 *   (3) the various abilities imparted by the equipment
 *   (4) the penalties induced by heavy armor or gloves or edged weapons
 *   (5) the abilities required to enter the "max_depth" dungeon level
 *   (6) the various abilities of some useful inventory items
 *
 * Note the use of special "item counters" for evaluating the value of
 * a collection of items of the given type.  Basically, the first item
 * of the given type is always the most valuable, with subsequent items
 * being worth less, until the "limit" is reached, after which point any
 * extra items are only worth as much as they can be sold for.
 */



/*
 * Helper function -- notice the player equipment
 */
static void borg_notice_aux1(void)
{
    int         i, hold;

    int         extra_blows = 0;

    int         extra_shots = 0;
    int         extra_might = 0;
    int         my_num_fire;

    auto_item       *item;

    u32b f1, f2, f3;

    /* Start with a single blow per turn */
    borg_skill[BI_BLOWS] = 1;

    /* Start with a single shot per turn */
    my_num_fire = 1;

    /* Reset the "ammo" tval to darts by default */
    my_ammo_tval = 0;

    /* Reset the "ammo" sides for darts*/
    my_ammo_sides = 4;

    /* Reset the shooting power */
    my_ammo_power = 0;

    /* Reset the shooting range */
    my_ammo_range = 0;


    /* Clear all the flags */
    borg_skill[BI_SPEED] = 110;
    borg_skill[BI_HP] = auto_mhp;
    borg_skill[BI_SP] = auto_msp;
    borg_skill[BI_CLEVEL] = auto_max_level;
    borg_skill[BI_ARMOR] = 0;

    /* Base infravision (purely racial) */
    borg_skill[BI_INFRA] = rb_ptr->infra;

    /* Base skill -- disarming */
    borg_skill[BI_DIS] = rb_ptr->r_dis + cb_ptr->c_dis;

    /* Base skill -- magic devices */
    borg_skill[BI_DEV] = rb_ptr->r_dev + cb_ptr->c_dev;

    /* Base skill -- saving throw */
    borg_skill[BI_SAV] = rb_ptr->r_sav + cb_ptr->c_sav;

    /* Base skill -- stealth */
    borg_skill[BI_STL] = rb_ptr->r_stl + cb_ptr->c_stl;

    /* Base skill -- searching ability */
    borg_skill[BI_SRCH] = rb_ptr->r_srh + cb_ptr->c_srh;

    /* Base skill -- searching frequency */
    borg_skill[BI_SRCHFREQ] = rb_ptr->r_fos + cb_ptr->c_fos;

    /* Base skill -- combat (normal) */
    borg_skill[BI_THN] = rb_ptr->r_thn + cb_ptr->c_thn;

    /* Base skill -- combat (shooting) */
    borg_skill[BI_THB] = rb_ptr->r_thb + cb_ptr->c_thb;

    /* Base skill -- combat (throwing) */
    borg_skill[BI_THT] = rb_ptr->r_thb + cb_ptr->c_thb;

    /* Racial Skills */

    /* Extract the player flags */
    player_flags(&f1, &f2, &f3);

    /* Good flags */
    if (f3 & (TR3_SLOW_DIGEST)) borg_skill[BI_SDIG] = TRUE;
    if (f3 & (TR3_FEATHER)) borg_skill[BI_FEATH] = TRUE;
    if (f3 & (TR3_LITE)) borg_skill[BI_LITE] = TRUE;
    if (f3 & (TR3_REGEN)) borg_skill[BI_REG] = TRUE;
    if (f3 & (TR3_TELEPATHY)) borg_skill[BI_ESP] = TRUE;
    if (f3 & (TR3_SEE_INVIS)) borg_skill[BI_SINV] = TRUE;
    if (f3 & (TR3_FREE_ACT)) borg_skill[BI_FRACT] = TRUE;
    if (f3 & (TR3_HOLD_LIFE)) borg_skill[BI_HLIFE] = TRUE;

    /* Weird flags */

    /* Bad flags */
    if (f3 & (TR3_IMPACT)) borg_skill[BI_W_IMPACT] = TRUE;
    if (f3 & (TR3_AGGRAVATE)) borg_skill[BI_CRSAGRV] = TRUE;
    if (f3 & (TR3_TELEPORT)) borg_skill[BI_CRSTELE] = TRUE;

    /* Immunity flags */
    if (f2 & (TR2_IM_FIRE)) borg_skill[BI_IFIRE] = TRUE;
    if (f2 & (TR2_IM_ACID)) borg_skill[BI_IACID] = TRUE;
    if (f2 & (TR2_IM_COLD)) borg_skill[BI_ICOLD] = TRUE;
    if (f2 & (TR2_IM_ELEC)) borg_skill[BI_IELEC] = TRUE;

    /* Resistance flags */
    if (f2 & (TR2_RES_ACID)) borg_skill[BI_RACID] = TRUE;
    if (f2 & (TR2_RES_ELEC)) borg_skill[BI_RELEC] = TRUE;
    if (f2 & (TR2_RES_FIRE)) borg_skill[BI_RFIRE] = TRUE;
    if (f2 & (TR2_RES_COLD)) borg_skill[BI_RCOLD] = TRUE;
    if (f2 & (TR2_RES_POIS)) borg_skill[BI_RPOIS] = TRUE;
    if (f2 & (TR2_RES_FEAR)) borg_skill[BI_RFEAR] = TRUE;
    if (f2 & (TR2_RES_LITE)) borg_skill[BI_RLITE] = TRUE;
    if (f2 & (TR2_RES_DARK)) borg_skill[BI_RDARK] = TRUE;
    if (f2 & (TR2_RES_BLIND)) borg_skill[BI_RBLIND] = TRUE;
    if (f2 & (TR2_RES_CONFU)) borg_skill[BI_RCONF] = TRUE;
    if (f2 & (TR2_RES_SOUND)) borg_skill[BI_RSND] = TRUE;
    if (f2 & (TR2_RES_SHARD)) borg_skill[BI_RSHRD] = TRUE;
    if (f2 & (TR2_RES_NEXUS)) borg_skill[BI_RNXUS] = TRUE;
    if (f2 & (TR2_RES_NETHR)) borg_skill[BI_RNTHR] = TRUE;
    if (f2 & (TR2_RES_CHAOS)) borg_skill[BI_RKAOS] = TRUE;
    if (f2 & (TR2_RES_DISEN)) borg_skill[BI_RDIS] = TRUE;

    /* Sustain flags */
    if (f2 & (TR2_SUST_STR)) borg_skill[BI_SSTR] = TRUE;
    if (f2 & (TR2_SUST_INT)) borg_skill[BI_SINT] = TRUE;
    if (f2 & (TR2_SUST_WIS)) borg_skill[BI_SWIS] = TRUE;
    if (f2 & (TR2_SUST_DEX)) borg_skill[BI_SDEX] = TRUE;
    if (f2 & (TR2_SUST_CON)) borg_skill[BI_SCON] = TRUE;
    if (f2 & (TR2_SUST_CHR)) borg_skill[BI_SCHR] = TRUE;



    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) my_stat_add[i] = 0;

    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* track number of items the borg has on him */
        /* Count up how many artifacts the borg has on him */
        borg_has_on[item->kind] += item->iqty;
        if (item->name1)
            borg_artifact[item->name1] += item->iqty;

        /* Affect stats */
        if (item->flags1 & TR1_STR) my_stat_add[A_STR] += item->pval;
        if (item->flags1 & TR1_INT) my_stat_add[A_INT] += item->pval;
        if (item->flags1 & TR1_WIS) my_stat_add[A_WIS] += item->pval;
        if (item->flags1 & TR1_DEX) my_stat_add[A_DEX] += item->pval;
        if (item->flags1 & TR1_CON) my_stat_add[A_CON] += item->pval;
        if (item->flags1 & TR1_CHR) my_stat_add[A_CHR] += item->pval;

        /* various slays */
        if (item->flags1 & TR1_SLAY_ANIMAL) borg_skill[BI_WS_ANIMAL] = TRUE;
        if (item->flags1 & TR1_SLAY_EVIL)   borg_skill[BI_WS_EVIL] = TRUE;

        if (item->flags1 & TR1_SLAY_UNDEAD) borg_skill[BI_WS_UNDEAD] = TRUE;
        if (item->flags1 & TR1_SLAY_DEMON)  borg_skill[BI_WS_DEMON] = TRUE;
        if (item->flags1 & TR1_SLAY_ORC)    borg_skill[BI_WS_ORC] = TRUE;
        if (item->flags1 & TR1_SLAY_TROLL)  borg_skill[BI_WS_TROLL] = TRUE;
        if (item->flags1 & TR1_SLAY_GIANT)  borg_skill[BI_WS_GIANT] = TRUE;
        if (item->flags1 & TR1_SLAY_DRAGON) borg_skill[BI_WS_DRAGON] = TRUE;
        if (item->flags1 & TR1_KILL_DRAGON) borg_skill[BI_WK_DRAGON] = TRUE;
        if (item->flags3 & TR3_IMPACT)      borg_skill[BI_W_IMPACT] = TRUE;
        if (item->flags1 & TR1_BRAND_ACID)  borg_skill[BI_WB_ACID] = TRUE;
        if (item->flags1 & TR1_BRAND_ELEC)  borg_skill[BI_WB_ELEC] = TRUE;
        if (item->flags1 & TR1_BRAND_FIRE)  borg_skill[BI_WB_FIRE] = TRUE;
        if (item->flags1 & TR1_BRAND_COLD)  borg_skill[BI_WB_COLD] = TRUE;

        /* Affect infravision */
        if (item->flags1 & TR1_INFRA) borg_skill[BI_INFRA] += item->pval;

        /* Affect stealth */
        if (item->flags1 & TR1_STEALTH) borg_skill[BI_STL] += item->pval;

        /* Affect searching ability (factor of five) */
        if (item->flags1 & TR1_SEARCH) borg_skill[BI_SRCH] += (item->pval * 5);

        /* Affect searching frequency (factor of five) */
        if (item->flags1 & TR1_SEARCH) borg_skill[BI_SRCHFREQ] += (item->pval * 5);

        /* Affect digging (factor of 20) */
        if (item->flags1 & TR1_TUNNEL) borg_skill[BI_DIG] += (item->pval * 20);

        /* Affect speed */
        if (item->flags1 & TR1_SPEED) borg_skill[BI_SPEED] += item->pval;

        /* Affect blows */
        if (item->flags1 & TR1_BLOWS) extra_blows += item->pval;

        /* Boost shots */
        if (item->flags1 & TR1_SHOTS) extra_shots++;

        /* Boost might */
        if (item->flags1 & TR1_MIGHT) extra_might++;

        /* Various flags */
        if (item->flags3 & TR3_SLOW_DIGEST) borg_skill[BI_SDIG] = TRUE;
        if (item->flags3 & TR3_AGGRAVATE) borg_skill[BI_CRSAGRV] = TRUE;
        if (item->flags3 & TR3_TELEPORT) borg_skill[BI_CRSTELE] = TRUE;
        if (item->flags3 & TR3_REGEN) borg_skill[BI_REG] = TRUE;
        if (item->flags3 & TR3_TELEPATHY) borg_skill[BI_ESP] = TRUE;
        if (item->flags3 & TR3_LITE) borg_skill[BI_LITE] = TRUE;
        if (item->flags3 & TR3_SEE_INVIS) borg_skill[BI_SINV] = TRUE;
        if (item->flags3 & TR3_FEATHER) borg_skill[BI_FEATH] = TRUE;
        if (item->flags3 & TR3_FREE_ACT) borg_skill[BI_FRACT] = TRUE;
        if (item->flags3 & TR3_HOLD_LIFE) borg_skill[BI_HLIFE] = TRUE;

        /* Immunity flags */
        /* if you are immune you automaticly resist */
        if (item->flags2 & TR2_IM_FIRE)
        {
            borg_skill[BI_IFIRE] = TRUE;
            borg_skill[BI_RFIRE] = TRUE;
        }
        if (item->flags2 & TR2_IM_ACID)
        {
            borg_skill[BI_IACID] = TRUE;
            borg_skill[BI_RACID] = TRUE;
        }
        if (item->flags2 & TR2_IM_COLD)
        {
            borg_skill[BI_ICOLD] = TRUE;
            borg_skill[BI_RCOLD] = TRUE;
        }
        if (item->flags2 & TR2_IM_ELEC)
        {
            borg_skill[BI_IELEC] = TRUE;
            borg_skill[BI_RELEC] = TRUE;
        }

        /* Resistance flags */
        if (item->flags2 & TR2_RES_ACID) borg_skill[BI_RACID] = TRUE;
        if (item->flags2 & TR2_RES_ELEC) borg_skill[BI_RELEC] = TRUE;
        if (item->flags2 & TR2_RES_FIRE) borg_skill[BI_RFIRE] = TRUE;
        if (item->flags2 & TR2_RES_COLD) borg_skill[BI_RCOLD] = TRUE;
        if (item->flags2 & TR2_RES_POIS) borg_skill[BI_RPOIS] = TRUE;
        if (item->flags2 & TR2_RES_CONFU) borg_skill[BI_RCONF] = TRUE;
        if (item->flags2 & TR2_RES_SOUND) borg_skill[BI_RSND] = TRUE;
        if (item->flags2 & TR2_RES_LITE) borg_skill[BI_RLITE] = TRUE;
        if (item->flags2 & TR2_RES_DARK) borg_skill[BI_RDARK] = TRUE;
        if (item->flags2 & TR2_RES_CHAOS) borg_skill[BI_RKAOS] = TRUE;
        if (item->flags2 & TR2_RES_DISEN) borg_skill[BI_RDIS] = TRUE;
        if (item->flags2 & TR2_RES_SHARD) borg_skill[BI_RSHRD] = TRUE;
        if (item->flags2 & TR2_RES_NEXUS) borg_skill[BI_RNXUS] = TRUE;
        if (item->flags2 & TR2_RES_BLIND) borg_skill[BI_RBLIND] = TRUE;
        if (item->flags2 & TR2_RES_NETHR) borg_skill[BI_RNTHR] = TRUE;

        /* Sustain flags */
        if (item->flags2 & TR2_SUST_STR) borg_skill[BI_SSTR] = TRUE;
        if (item->flags2 & TR2_SUST_INT) borg_skill[BI_SINT] = TRUE;
        if (item->flags2 & TR2_SUST_WIS) borg_skill[BI_SWIS] = TRUE;
        if (item->flags2 & TR2_SUST_DEX) borg_skill[BI_SDEX] = TRUE;
        if (item->flags2 & TR2_SUST_CON) borg_skill[BI_SCON] = TRUE;
        if (item->flags2 & TR2_SUST_CHR) borg_skill[BI_SCHR] = TRUE;


        /* Hack -- The borg will miss read acid damaged items such as
         * Leather Gloves [2,-2] and falsely assume they help his power.
         * this hack rewrites the bonus to an extremely negative value
         * thus encouraging him to remove the non-helpful-non-harmful but
         * heavy-none-the-less item.
         */
        if ((!item->name1 && !item->name2) &&
             item->ac >= 1 && item->to_a + item->ac <= 0)
        {
            item->to_a -= 10;
        }

        /* Modify the base armor class */
        borg_skill[BI_ARMOR] += item->ac;

        /* Apply the bonuses to armor class */
        borg_skill[BI_ARMOR] += item->to_a;

        /* Hack -- do not apply "weapon" bonuses */
        if (i == INVEN_WIELD) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (i == INVEN_BOW) continue;

        /* Apply the bonuses to hit/damage */
        borg_skill[BI_TOHIT] += item->to_h;
        borg_skill[BI_TODAM] += item->to_d;
    }

    /* Update "stats" */
    for (i = 0; i < 6; i++)
    {
        int use, ind;

        /* Extract the new "use_stat" value for the stat */
        use = modify_stat_value(my_stat_cur[i], my_stat_add[i]);

        /* Save the stat */
        my_stat_use[i] = use;

        /* Values: 3, ..., 17 */
        if (use <= 17) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (37);

        /* Save the index */
        if (ind > 37)
            my_stat_ind[i] = 37;
        else
            my_stat_ind[i] = ind;
        borg_skill[BI_STR+i] = my_stat_ind[i];
        borg_skill[BI_CSTR+i] = auto_stat[i];
    }

    borg_skill[BI_HP_ADJ] =
        (((adj_con_mhp[my_stat_ind[A_CON]] -128) * borg_skill[BI_CLEVEL]) / 2);

    /* 'Mana' is actually the 'mana adjustment' */
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        borg_skill[BI_SP_ADJ] =
            ((adj_mag_mana[my_stat_ind[A_WIS]] * borg_skill[BI_CLEVEL]) / 2);
        borg_skill[BI_FAIL1] = adj_mag_stat[my_stat_ind[A_WIS]];
        borg_skill[BI_FAIL2] = adj_mag_fail[my_stat_ind[A_WIS]];
    }
    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        borg_skill[BI_SP_ADJ] =
            ((adj_mag_mana[my_stat_ind[A_INT]] * borg_skill[BI_CLEVEL]) / 2);
        borg_skill[BI_FAIL1] = adj_mag_stat[my_stat_ind[A_INT]];
        borg_skill[BI_FAIL2] = adj_mag_fail[my_stat_ind[A_INT]];
    }


    /* Bloating slows the player down (a little) */
    if (do_gorged) borg_skill[BI_SPEED] -= 10;



    /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
    borg_skill[BI_ARMOR] += ((int)(adj_dex_ta[my_stat_ind[A_DEX]]) - 128);
    borg_skill[BI_TODAM] += ((int)(adj_str_td[my_stat_ind[A_STR]]) - 128);
    borg_skill[BI_TOHIT] += ((int)(adj_dex_th[my_stat_ind[A_DEX]]) - 128);
    borg_skill[BI_TOHIT] += ((int)(adj_str_th[my_stat_ind[A_STR]]) - 128);


    /* Obtain the "hold" value */
    hold = adj_str_hold[my_stat_ind[A_STR]];


    /* Examine the "current bow" */
    item = &auto_items[INVEN_BOW];

    /* attacking with bare hands */
    if (item->iqty == 0)
    {
        item->ds = 0;
        item->dd = 0;
        item->to_d = 0;
        item->to_h = 0;
        item->weight = 0;
    }


    /* and assume we can enchant up to +8 if borg_skill[BI_CLEVEL] > 25 */
     borg_skill[BI_BTOHIT] = item->to_h;
    if (borg_skill[BI_BTOHIT] < 8 && borg_skill[BI_CLEVEL] >= 25)
        borg_skill[BI_BTOHIT] = 8;
        borg_skill[BI_BTODAM] = item->to_d;
    if (borg_skill[BI_BTODAM] < 8 && borg_skill[BI_CLEVEL] >= 25)
        borg_skill[BI_BTODAM] = 8;

    /* It is hard to carholdry a heavy bow */
    if (hold < item->weight / 10)
    {
        borg_skill[BI_HEAVYBOW] = TRUE;
        /* Hard to wield a heavy bow */
        borg_skill[BI_TOHIT] += 2 * (hold - item->weight / 10);
    }

    /* Compute "extra shots" if needed */
    if (item->iqty && (hold >= item->weight / 10))
    {
        /* Take note of required "tval" for missiles */
        switch (item->sval)
        {
            case SV_SLING:
            my_ammo_tval = TV_SHOT;
            my_ammo_sides = 3;
            my_ammo_power = 2;
            break;

            case SV_SHORT_BOW:
            my_ammo_tval = TV_ARROW;
            my_ammo_sides = 4;
            my_ammo_power = 2;
            break;

            case SV_LONG_BOW:
            my_ammo_tval = TV_ARROW;
            my_ammo_sides = 4;
            my_ammo_power = 3;
            break;

            case SV_LIGHT_XBOW:
            my_ammo_tval = TV_BOLT;
            my_ammo_sides = 5;
            my_ammo_power = 3;
            break;

            case SV_HEAVY_XBOW:
            my_ammo_tval = TV_BOLT;
            my_ammo_sides = 5;
            my_ammo_power = 4;
            break;
        }

        /* Add in extra power */
        my_ammo_power += extra_might;

        /* Calculate total range */
        my_ammo_range = 10 + my_ammo_power * 5;

        /* Hack -- Reward High Level Rangers using Bows */
        if ((auto_class == CLASS_RANGER) && (my_ammo_tval == TV_ARROW))
        {
            /* Extra shot at level 20 */
            if (borg_skill[BI_CLEVEL] >= 20) my_num_fire++;

            /* Extra shot at level 40 */
            if (borg_skill[BI_CLEVEL] >= 40) my_num_fire++;
        }

        /* Add in the "bonus shots" */
        my_num_fire += extra_shots;

        /* Require at least one shot */
        if (my_num_fire < 1) my_num_fire = 1;
    }
    borg_skill[BI_SHOTS] = my_num_fire;

    /* Calculate "average" damage per "normal" shot (times 2) */
    borg_skill[BI_BMAXDAM] = (my_ammo_sides + borg_skill[BI_BTODAM]) * my_ammo_power;
    borg_skill[BI_BMAXDAM] *= borg_skill[BI_SHOTS];

    /* Examine the "main weapon" */
    item = &auto_items[INVEN_WIELD];

    /* attacking with bare hands */
    if (item->iqty == 0)
    {
        item->ds = 0;
        item->dd = 0;
        item->to_d = 0;
        item->to_h = 0;
        item->weight = 0;
    }

    /* and assume we can enchant up to +8 if borg_skill[BI_CLEVEL] > 25 */
     borg_skill[BI_WTOHIT] = item->to_h;
    if (borg_skill[BI_WTOHIT] < 8 && borg_skill[BI_CLEVEL] >= 25)
        borg_skill[BI_WTOHIT] = 8;
        borg_skill[BI_WTODAM] = item->to_d;
    if (borg_skill[BI_WTODAM] < 8 && borg_skill[BI_CLEVEL] >= 25)
        borg_skill[BI_WTODAM] = 8;

    /* It is hard to hold a heavy weapon */
    if (hold < item->weight / 10)
    {
        borg_skill[BI_HEAVYWEPON] = TRUE;

        /* Hard to wield a heavy weapon */
        borg_skill[BI_TOHIT] += 2 * (hold - item->weight / 10);
    }

    /* Normal weapons */
    if (item->iqty && (hold >= item->weight / 10))
    {
        int str_index, dex_index;

        int num = 0, wgt = 0, mul = 0, div = 0;

        /* Analyze the class */
        switch (auto_class)
        {
            /* Warrior */
            case 0: num = 6; wgt = 30; mul = 5; break;

            /* Mage */
            case 1: num = 4; wgt = 40; mul = 2; break;

            /* Priest (was mul = 3.5) */
            case 2: num = 5; wgt = 35; mul = 3; break;

            /* Rogue */
            case 3: num = 5; wgt = 30; mul = 3; break;

            /* Ranger */
            case 4: num = 5; wgt = 35; mul = 4; break;

            /* Paladin */
            case 5: num = 5; wgt = 30; mul = 4; break;

        }

        /* Enforce a minimum "weight" */
        div = ((item->weight < wgt) ? wgt : item->weight);

        /* Access the strength vs weight */
        str_index = (adj_str_blow[my_stat_ind[A_STR]] * mul / div);

        /* Maximal value */
        if (str_index > 11) str_index = 11;

        /* Index by dexterity */
        dex_index = (adj_dex_blow[my_stat_ind[A_DEX]]);

        /* Maximal value */
        if (dex_index > 11) dex_index = 11;

        /* Use the blows table */
        borg_skill[BI_BLOWS] = blows_table[str_index][dex_index];

        /* Maximal value */
        if (borg_skill[BI_BLOWS] > num) borg_skill[BI_BLOWS] = num;

        /* Add in the "bonus blows" */
        borg_skill[BI_BLOWS] += extra_blows;

        /* Require at least one blow */
        if (borg_skill[BI_BLOWS] < 1) borg_skill[BI_BLOWS] = 1;

        /* Boost digging skill by weapon weight */
        borg_skill[BI_DIG] += (item->weight / 10);
    }

    /* apw priest weapon penalty for non-blessed edged weapons */
    if ((auto_class == CLASS_PRIEST) &&
        ((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
        (!(item->flags3 & TR3_BLESSED)))
    {
        /* Reduce the real bonuses */
        borg_skill[BI_TOHIT] -= 2;
        borg_skill[BI_TODAM] -= 2;
    }

    /* Calculate "max" damage per "normal" blow  */
    /* and assume we can enchant up to +8 if borg_skill[BI_CLEVEL] > 25 */
    borg_skill[BI_WMAXDAM] =
        (item->dd * item->ds + borg_skill[BI_TODAM] + borg_skill[BI_WTODAM]);
    /* Calculate base damage, used to calculating slays */
    borg_skill[BI_WBASEDAM] =
        (item->dd * item->ds);

     /* Hack -- Reward High Level Warriors with Res Fear */
     if (auto_class == CLASS_WARRIOR)
     {
         /* Resist fear at level 30 */
         if (borg_skill[BI_CLEVEL] >= 30) borg_skill[BI_RFEAR] = TRUE;
     }

    /* Affect Skill -- stealth (bonus one) */
    borg_skill[BI_STL] += 1;

    /* Affect Skill -- disarming (DEX and INT) */
    borg_skill[BI_DIS] += adj_dex_dis[my_stat_ind[A_DEX]];
    borg_skill[BI_DIS] += adj_int_dis[my_stat_ind[A_INT]];

    /* Affect Skill -- magic devices (INT) */
    borg_skill[BI_DEV] += adj_int_dev[my_stat_ind[A_INT]];

    /* Affect Skill -- saving throw (WIS) */
    borg_skill[BI_SAV] += adj_wis_sav[my_stat_ind[A_WIS]];

    /* Affect Skill -- digging (STR) */
    borg_skill[BI_DIG] += adj_str_dig[my_stat_ind[A_STR]];


    /* Affect Skill -- disarming (Level, by Class) */
    borg_skill[BI_DIS] += (cb_ptr->x_dis * auto_max_level / 10);

    /* Affect Skill -- magic devices (Level, by Class) */
    borg_skill[BI_DEV] += (cb_ptr->x_dev * auto_max_level / 10);

    /* Affect Skill -- saving throw (Level, by Class) */
    borg_skill[BI_SAV] += (cb_ptr->x_sav * auto_max_level / 10);

    /* Affect Skill -- stealth (Level, by Class) */
    borg_skill[BI_STL] += (cb_ptr->x_stl * auto_max_level / 10);

    /* Affect Skill -- search ability (Level, by Class) */
    borg_skill[BI_SRCH] += (cb_ptr->x_srh * auto_max_level / 10);

    /* Affect Skill -- search frequency (Level, by Class) */
    borg_skill[BI_SRCHFREQ] += (cb_ptr->x_fos * auto_max_level / 10);

    /* Affect Skill -- combat (normal) (Level, by Class) */
    borg_skill[BI_THN] += (cb_ptr->x_thn * auto_max_level / 10);

    /* Affect Skill -- combat (shooting) (Level, by Class) */
    borg_skill[BI_THB] += (cb_ptr->x_thb * auto_max_level / 10);

    /* Affect Skill -- combat (throwing) (Level, by Class) */
    borg_skill[BI_THT] += (cb_ptr->x_thb * auto_max_level / 10);

    /* Limit Skill -- stealth from 0 to 30 */
    if (borg_skill[BI_STL] > 30) borg_skill[BI_STL] = 30;
    if (borg_skill[BI_STL] < 0) borg_skill[BI_STL] = 0;

    /* Limit Skill -- digging from 1 up */
    if (borg_skill[BI_DIG] < 1) borg_skill[BI_DIG] = 1;

    /*** Count needed enchantment ***/

    /* Assume no enchantment needed */
    my_need_enchant_to_a = 0;
    my_need_enchant_to_h = 0;
    my_need_enchant_to_d = 0;
    my_need_brand_weapon = 0;

    /* Hack -- enchant all the equipment (weapons) */
    for (i = INVEN_WIELD; i <= INVEN_BOW; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip "unknown" items */
        if (!item->able) continue;

        /* Enchant all weapons (to hit) */
        if ((borg_prayer_okay_fail(7, 3, 40) ||
             amt_enchant_weapon >=1 ) )
        {
            if (item->to_h < 15)
            {
                my_need_enchant_to_h += (15 - item->to_h);
            }

            /* Enchant all weapons (to damage) */
            if (item->to_d < 15)
            {
                my_need_enchant_to_d += (15 - item->to_d);
            }
        }
        else
        {
            if (item->to_h < 8)
            {
                my_need_enchant_to_h += (8 - item->to_h);
            }

            /* Enchant all weapons (to damage) */
            if (item->to_d < 8)
            {
                my_need_enchant_to_d += (8 - item->to_d);
            }
        }
    }

    /* Hack -- enchant all the equipment (armor) */
    for (i = INVEN_BODY; i <= INVEN_FEET; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip "unknown" items */
        if (!item->able) continue;

        /* Note need for enchantment */
        if ((borg_prayer_okay_fail(7, 4, 40) ||
            amt_enchant_armor >=1 ))
        {
            if (item->to_a < 15)
            {
                my_need_enchant_to_a += (15 - item->to_a);
            }
        }
        else
        {
            if (item->to_a < 10)
            {
                my_need_enchant_to_a += (10 - item->to_a);
            }
        }
    }


    /* Examine the lite */
    item = &auto_items[INVEN_LITE];

    /* Assume normal lite radius */
    borg_skill[BI_CUR_LITE] = 0;

    /* Glowing player has light */
    if (borg_skill[BI_LITE]) borg_skill[BI_CUR_LITE] = 1;

    /* Lite */
    if (item->tval == TV_LITE)
    {
        /* Torches -- radius one */
        if (item->sval == SV_LITE_TORCH) borg_skill[BI_CUR_LITE] = 1;

        /* Lanterns -- radius two */
        if (item->sval == SV_LITE_LANTERN) borg_skill[BI_CUR_LITE] = 2;

        /* No fuel means no radius */
        if (!item->pval) borg_skill[BI_CUR_LITE] = 0;

        /* Artifact lites -- radius three */
        /* HACK assume non-torch/non lantern lite is artifact */
        if ((item->sval != SV_LITE_TORCH) &&
            (item->sval != SV_LITE_LANTERN))
        {
            borg_skill[BI_CUR_LITE] = 3;

            /* Artifact lites -- assume glowing */
            borg_skill[BI_LITE] = TRUE;
        }
    }
}


/*
 * Helper function -- notice the player inventory
 */
static void borg_notice_aux2(void)
{
    int i;

    auto_item *item;


    /*** Reset counters ***/


    /* Reset basic */
    amt_phase = 0;
    amt_food_lowcal = 0;
    amt_food_hical = 0;

    /* Reset healing */
    amt_mana = 0;
    amt_ez_heal_star = 0;
    amt_ez_heal_life = 0;
    amt_rod_heal = 0;
    amt_pot_heal = 0;
    amt_slow_poison =0;
    amt_cure_confusion = 0;
    amt_cure_blind = 0;

    /* Reset books */
    amt_book[0] = 0;
    amt_book[1] = 0;
    amt_book[2] = 0;
    amt_book[3] = 0;
    amt_book[4] = 0;
    amt_book[5] = 0;
    amt_book[6] = 0;
    amt_book[7] = 0;
    amt_book[8] = 0;

    /* Reset various */
    amt_add_stat[A_STR] = 0;
    amt_add_stat[A_INT] = 0;
    amt_add_stat[A_WIS] = 0;
    amt_add_stat[A_DEX] = 0;
    amt_add_stat[A_CON] = 0;
    amt_add_stat[A_CHR] = 0;
    amt_fix_stat[A_STR] = 0;
    amt_fix_stat[A_INT] = 0;
    amt_fix_stat[A_WIS] = 0;
    amt_fix_stat[A_DEX] = 0;
    amt_fix_stat[A_CON] = 0;
    amt_fix_stat[A_CHR] = 0;
    amt_fix_stat[6] = 0;

    amt_fix_exp = 0;
    amt_cool_staff = 0;
    amt_digger = 0;

    /* Reset enchantment */
    amt_enchant_to_a = 0;
    amt_enchant_to_d = 0;
    amt_enchant_to_h = 0;

    amt_brand_weapon = 0;
    amt_enchant_weapon = 0;
    amt_enchant_armor = 0;

    /*** Process the inventory ***/

    /* Scan the inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Hack -- skip un-aware items */
        if (!item->kind) continue;

        /* count up the items on the borg (do not count artifacts  */
        /* that are not being wielded) */
        borg_has[item->kind] += item->iqty;

        /* Analyze the item */
        switch (item->tval)
        {
            /* Books */
            case TV_MAGIC_BOOK:
            case TV_PRAYER_BOOK:
            /* Skip incorrect books */
            if (item->tval != mb_ptr->spell_book) break;
            /* Count the books */
            amt_book[item->sval] += item->iqty;
            break;


            /* Food */
            case TV_FOOD:
            /* Analyze */
            switch (item->sval)
            {
                case SV_FOOD_WAYBREAD:
                    amt_food_hical += item->iqty;
                    break;
                case SV_FOOD_RATION:
                    amt_food_hical += item->iqty;
                    break;
                case SV_FOOD_JERKY:
                    amt_food_lowcal += item->iqty;
                    break;
                case SV_FOOD_BISCUIT:
                    amt_food_lowcal += item->iqty;
                    break;
                case SV_FOOD_SLIME_MOLD:
                    amt_food_lowcal += item->iqty;
                    break;

                case SV_FOOD_RESTORE_STR:
                    amt_fix_stat[A_STR] += item->iqty;
                    break;
                case SV_FOOD_RESTORE_CON:
                    amt_fix_stat[A_CON] += item->iqty;
                    break;
                case SV_FOOD_RESTORING:
                    amt_fix_stat[A_STR] += item->iqty;
                    amt_fix_stat[A_INT] += item->iqty;
                    amt_fix_stat[A_WIS] += item->iqty;
                    amt_fix_stat[A_DEX] += item->iqty;
                    amt_fix_stat[A_CON] += item->iqty;
                    amt_fix_stat[A_CHR] += item->iqty;
                    amt_fix_stat[6]     += item->iqty;
                    break;

                case SV_FOOD_CURE_CONFUSION:
                amt_cure_confusion += item->iqty;
                break;

                case SV_FOOD_CURE_BLINDNESS:
                amt_cure_blind += item->iqty;
                break;

                case SV_FOOD_CURE_POISON:
                borg_skill[BI_ACUREPOIS] += item->iqty;
                break;

            }
            break;


            /* Potions */
            case TV_POTION:
            /* Analyze */
            switch (item->sval)
            {
                case SV_POTION_HEALING:
                borg_skill[BI_AHEAL] += item->iqty;
                amt_pot_heal += item->iqty;
                break;
                case SV_POTION_STAR_HEALING:
                amt_ez_heal_star += item->iqty;
                break;
                case SV_POTION_LIFE:
                amt_ez_heal_life += item->iqty;
                break;
                case SV_POTION_CURE_CRITICAL:
                borg_skill[BI_ACCW] += item->iqty;
                break;
                case SV_POTION_CURE_SERIOUS:
                borg_skill[BI_ACSW] += item->iqty;
                break;
                case SV_POTION_CURE_LIGHT:
                if (do_cut) borg_skill[BI_ACSW] += item->iqty;
                break;
                case SV_POTION_CURE_POISON:
                borg_skill[BI_ACUREPOIS] += item->iqty;
                break;
                case SV_POTION_SLOW_POISON:
                amt_slow_poison += item->iqty;
                break;

                case SV_POTION_RESIST_HEAT:
                borg_skill[BI_ARESHEAT] += item->iqty;
                break;
                case SV_POTION_RESIST_COLD:
                borg_skill[BI_ARESCOLD] += item->iqty;
                break;

                case SV_POTION_INC_STR:
                amt_add_stat[A_STR] += item->iqty;
                break;
                case SV_POTION_INC_INT:
                amt_add_stat[A_INT] += item->iqty;
                break;
                case SV_POTION_INC_WIS:
                amt_add_stat[A_WIS] += item->iqty;
                break;
                case SV_POTION_INC_DEX:
                amt_add_stat[A_DEX] += item->iqty;
                break;
                case SV_POTION_INC_CON:
                amt_add_stat[A_CON] += item->iqty;
                break;

                case SV_POTION_INC_CHR:
                amt_add_stat[A_CHR] += item->iqty;
                break;

                case SV_POTION_RES_STR:
                amt_fix_stat[A_STR] += item->iqty;
                break;

                case SV_POTION_RES_INT:
                amt_fix_stat[A_INT] += item->iqty;
                break;

                case SV_POTION_RES_WIS:
                amt_fix_stat[A_WIS] += item->iqty;
                break;

                case SV_POTION_RES_DEX:
                amt_fix_stat[A_DEX] += item->iqty;
                break;

                case SV_POTION_RES_CON:
                amt_fix_stat[A_CON] += item->iqty;
                break;

                case SV_POTION_RES_CHR:
                amt_fix_stat[A_CHR] += item->iqty;
                break;

                case SV_POTION_RESTORE_EXP:
                amt_fix_exp += item->iqty;
                break;

                case SV_POTION_SPEED:
                borg_skill[BI_ASPEED] += item->iqty;
                break;

                case SV_POTION_RESTORE_MANA:
                amt_mana += item->iqty;
                break;



            }

            break;



            /* Scrolls */
            case TV_SCROLL:


            /* Analyze the scroll */
            switch (item->sval)
            {
                case SV_SCROLL_IDENTIFY:
                borg_skill[BI_AID] += item->iqty;
                break;

                case SV_SCROLL_RECHARGING:
                borg_skill[BI_ARECHARGE] += item->iqty;
                break;

                case SV_SCROLL_PHASE_DOOR:
                amt_phase += item->iqty;
                break;

                case SV_SCROLL_TELEPORT:
                borg_skill[BI_AESCAPE] += item->iqty;
                borg_skill[BI_ATELEPORT] += item->iqty;
                break;

                case SV_SCROLL_WORD_OF_RECALL:
                borg_skill[BI_RECALL] += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_ARMOR:
                amt_enchant_to_a += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
                amt_enchant_to_h += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
                amt_enchant_to_d += item->iqty;
                break;

                case SV_SCROLL_STAR_ENCHANT_WEAPON:
                amt_enchant_weapon += item->iqty;
                break;

                case SV_SCROLL_PROTECTION_FROM_EVIL:
                borg_skill[BI_APFE] += item->iqty;
                break;

                case SV_SCROLL_STAR_ENCHANT_ARMOR:
                amt_enchant_armor += item->iqty;
                break;

                case SV_SCROLL_RUNE_OF_PROTECTION:
                borg_skill[BI_AGLYPH] += item->iqty;
                break;

                case SV_SCROLL_TELEPORT_LEVEL:
                borg_skill[BI_ATELEPORTLVL] += item->iqty;
                break;
            }
            break;


            /* Rods */
            case TV_ROD:


            /* Analyze */
            switch (item->sval)
            {
                case SV_ROD_IDENTIFY:
                if (borg_skill[BI_DEV] - item->level > 7)
                {
                    borg_skill[BI_AID] += item->iqty * 100;
                }
                else
                {
                    borg_skill[BI_AID] += item->iqty;
                }
                break;

                case SV_ROD_RECALL:
                /* Don't count on it if I suck at activations */
                if (borg_skill[BI_DEV] - item->level > 7)
                {
                    borg_skill[BI_RECALL] += item->iqty * 100;
                }
                else
                {
                    borg_skill[BI_RECALL] += item->iqty;
                }
                break;

                case SV_ROD_DETECT_TRAP:
                borg_skill[BI_ADETTRAP] += item->iqty * 100;
                break;

                case SV_ROD_DETECT_DOOR:
                borg_skill[BI_ADETDOOR] += item->iqty * 100;
                break;

                case SV_ROD_DETECTION:
                borg_skill[BI_ADETTRAP] += item->iqty * 100;
                borg_skill[BI_ADETDOOR] += item->iqty * 100;
                borg_skill[BI_ADETEVIL] += item->iqty * 100;
                break;

                case SV_ROD_SPEED:
                /* Don't count on it if I suck at activations */
                if (borg_skill[BI_DEV] - item->level > 7)
                {
                    borg_skill[BI_ASPEED] += item->iqty * 100;
                }
                else
                {
                    borg_skill[BI_ASPEED] += item->iqty;
                }
                break;

                case SV_ROD_MAPPING:
                borg_skill[BI_AMAGICMAP] += item->iqty * 100;
                break;

                case SV_ROD_HEALING:
                /* only +2 per rod because of long charge time. */
                /* Don't count on it if I suck at activations */
                if (borg_skill[BI_DEV] - item->level > 7)
                {
                    borg_skill[BI_AHEAL] += item->iqty *2;
                    amt_rod_heal +=item->iqty;
                }
                else
                {
                    borg_skill[BI_AHEAL] += item->iqty;
                    amt_rod_heal +=item->iqty;
                }
                break;
            }

            break;


            /* Staffs */
            case TV_STAFF:
            /* Staves should not be carried to Morgoth, he drains
             * them to heal himself- not goo at all
             */
            if (auto_max_depth >= 99 && !borg_king)
            {
                /* skip these */
                break;
            }
            /* Analyze */
            switch (item->sval)
            {
                case SV_STAFF_IDENTIFY:
                borg_skill[BI_AID] += item->iqty * item->pval;
                break;

                case SV_STAFF_TELEPORTATION:
                borg_skill[BI_ATELEPORT] += item->iqty * item->pval;
                break;

                case SV_STAFF_SPEED:
                borg_skill[BI_ASPEED] += item->iqty * item->pval;
                break;

                case SV_STAFF_HEALING:
                borg_skill[BI_AHEAL] += item->iqty * item->pval;
                break;

                case SV_STAFF_THE_MAGI:
                borg_skill[BI_ASTFMAGI] += item->iqty * item->pval;
                break;

                case SV_STAFF_DESTRUCTION:
                borg_skill[BI_ASTFDEST] +=item->iqty * item->pval;
                break;

                case SV_STAFF_POWER:
                amt_cool_staff +=item->iqty;
                break;

                case SV_STAFF_HOLINESS:
                amt_cool_staff +=item->iqty;
                borg_skill[BI_AHEAL] +=item->iqty * item->pval;
                break;
            }

            break;


            /* Flasks */
            case TV_FLASK:

            /* Use as fuel if we equip a lantern */
            if (borg_skill[BI_CUR_LITE] == 2) borg_skill[BI_AFUEL] += item->iqty;

            /* Count as Missiles */
            if (borg_skill[BI_CLEVEL] < 15 ) borg_skill[BI_AMISSILES] += item->iqty;

            break;


            /* Torches */
            case TV_LITE:


            /* Use as fuel if it is a torch and we carry a torch */
            if ((item->sval == SV_LITE_TORCH) &&
                (borg_skill[BI_CUR_LITE] <= 1) )
                {
                    borg_skill[BI_AFUEL] += item->iqty;
                }
            break;


            /* Weapons */
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
                /* These items are checked a bit later in a sub routine
                 * to notice the flags.  It is done outside this switch.
                 */
                 break;

            /* Shovels and such */
            case TV_DIGGING:

                /* Hack -- ignore worthless ones (including cursed) */
                if (item->value <= 0) break;
                if (item->cursed) break;

                /* Do not carry if weak, won't be able to dig anyway */
                if (borg_skill[BI_DIG] < 30) break;

                amt_digger += item->iqty;
               break;

            /* Missiles */

            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT:
            /* Hack -- ignore invalid missiles */
            if (item->tval != my_ammo_tval) break;

            /* Hack -- ignore worthless missiles */
            if (item->value <= 0) break;

            /* Count them */
            borg_skill[BI_AMISSILES] += item->iqty;

            if ( borg_equips_artifact(ART_CUBRAGOL, INVEN_BOW) &&
              item->iqty >=5 &&
              /* Skip artifacts and ego-items */
              !artifact_p(item) &&
              !ego_item_p(item) &&
              item->able &&
              item->tval == TV_BOLT)
              {
                my_need_brand_weapon +=10L;
              }

            /* if we have shit loads of cash (as we will at level 35),  */
            /* enchant missiles */
            if (borg_skill[BI_CLEVEL] > 35)
            {
                if (borg_prayer_okay_fail(7, 3, 40) && item->iqty >= 5)
                {
                    if (item->to_h < 10)
                    {
                        my_need_enchant_to_h += (10 - item->to_h);
                    }

                    if (item->to_d < 10)
                    {
                        my_need_enchant_to_d += (10 - item->to_d);
                    }
                }
                else
                {
                    if (item->to_h < 8)
                    {
                        my_need_enchant_to_h += (8 - item->to_h);
                    }

                    if (item->to_d < 8)
                    {
                        my_need_enchant_to_d += (8 - item->to_d);
                    }
                }
            }

            break;
        }
    }


    /*** Process the Spells and Prayers ***/
    /*  apw  artifact activations are accounted here
     *  But some artifacts are not counted for two reasons .
     *  1.  Some spells-powers are needed instantly and are considered in
     *  the borg preparation code.  An artifact maybe non-charged at the
     *  moment he needes it.  Then he would need the spell and not be able
     *  to cast it. (ie. teleport, phase)
     *  2.  An artifact may grant a power then he assumes he has infinite
     *  amounts.  He then sells off his scrolls with the duplicate power.
     *  When it comes time to upgrade and swap out the artifact, he wont
     *  because his power drops since he does not have the scrolls anymore.
     *  and he does not buy items first.
     *
     *  A possible solution would be to have him keep a few scrolls as a
     *  back-up, or to remove the bonus for level preparation from borg_power.
     *  Right now I think it is better that he not consider the artifacts
     *  Whose powers are considered in borg_prep.
     */

    /* Handle "satisfy hunger" -> infinite food */
    if (borg_spell_legal_fail(2, 0, 10) || borg_prayer_legal_fail(1, 5, 10))
    {
        borg_skill[BI_FOOD] += 1000;
    }

    /* Handle "identify" -> infinite identifies */
    if (borg_spell_legal(2, 4) || borg_prayer_legal(5, 2))
/*         borg_equips_artifact(ART_ERIRIL, INVEN_WIELD)) */
    {
        borg_skill[BI_AID] += 1000;
    }

    /* Handle "detect traps" */
    if (borg_prayer_legal(0, 5))
    {
        borg_skill[BI_ADETTRAP] += 1000;
    }

    /* Handle "detect doors" */
    if (borg_prayer_legal(0, 6))
    {
        borg_skill[BI_ADETDOOR] += 1000;
    }

    /* Handle "detect evil & monsters" */
    if (borg_prayer_legal(0, 0) ||
        borg_spell_legal(0, 1))
    {
        borg_skill[BI_ADETEVIL] += 1000;
    }

    /* Handle "detection" */
    if (borg_prayer_legal(5, 1) ||
    borg_equips_artifact(ART_HOLHENNETH, INVEN_HEAD))
    {
        borg_skill[BI_ADETDOOR] += 1000;
        borg_skill[BI_ADETTRAP] += 1000;
        borg_skill[BI_ADETEVIL] += 1000;

    }

    /* Handle "magic mapping" */
    if (borg_prayer_legal(2, 6) ||
    borg_equips_artifact(ART_ELENDIL, INVEN_LITE))
    {
        borg_skill[BI_ADETDOOR] += 1000;
        borg_skill[BI_ADETTRAP] += 1000;
        borg_skill[BI_AMAGICMAP] += 1000;
    }

    /* apw Handle "protection from evil" */
    if (borg_prayer_legal(2, 4) ||
    borg_equips_artifact(ART_CARLAMMAS, INVEN_HEAD))
    {
        borg_skill[BI_APFE] += 1000;
    }

    /* apw Handle "rune of protection" glyph" */
    if (borg_prayer_legal(3, 4))
    {
        borg_skill[BI_AGLYPH] += 1000;
    }

    /* Handle "detect traps/doors" */
    if (borg_spell_legal(0, 7))
    {
        borg_skill[BI_ADETDOOR] += 1000;
        borg_skill[BI_ADETTRAP] += 1000;
    }

    /* Handle "enchant weapon" */
    if (borg_prayer_legal_fail(7, 3, 40))
    {
        amt_enchant_to_h += 1000;
        amt_enchant_to_d += 1000;
        amt_enchant_weapon +=1000;
    }

    /* apw Handle "Brand Weapon (bolts)" */
    if ( borg_equips_artifact(ART_CUBRAGOL, INVEN_BOW))
    {
        amt_brand_weapon += 1000;
    }

    /* Handle "enchant armor" */
    if (borg_prayer_legal_fail(7, 4, 40))
    {
        amt_enchant_to_a += 1000;
        amt_enchant_armor +=1000;
    }

    /* Handle Diggers */
    if (borg_spell_legal_fail(1, 8, 40))
    {
        amt_digger += 1;
    }

    /* Handle recall */
    if (borg_prayer_legal_fail(4, 4, 40) || borg_spell_legal_fail(5, 4,40) ||
        (auto_depth == 100 && (borg_prayer_legal(4, 4) || borg_spell_legal(5, 4))))
    /*  Avavir not counted because it may not be charged */
    {
        borg_skill[BI_RECALL] += 1000;
    }

    /* Handle teleport_level */
    if (borg_prayer_legal_fail(4, 3, 40) || borg_spell_legal_fail(5, 2,40))
    {
        borg_skill[BI_ATELEPORTLVL] += 1000;
    }

    /* Handle teleport spell carefully */
    if (((borg_prayer_okay_fail(1, 1, 1) || borg_prayer_okay_fail(4, 1, 1)) && borg_skill[BI_RBLIND] && borg_skill[BI_RCONF]) ||
        (borg_spell_okay_fail(1, 5, 1) && borg_skill[BI_RBLIND] && borg_skill[BI_RCONF]))
    {
        borg_skill[BI_ATELEPORT] += 1000;
    }

    /* Handle GOI spell carefully */
    if (borg_spell_legal_fail(7, 4, 4) && borg_skill[BI_RBLIND] &&
        borg_skill[BI_RCONF] && borg_skill[BI_ESP] &&
        auto_mhp >= 650)
    {
        borg_skill[BI_AXGOI] += 1000;
    }
    /* Handle GOI spell just to see if legal */
    if (borg_spell_legal(7, 4))
    {
        borg_skill[BI_AGOI] += 1000;
    }

    /* Handle Holy Word prayer just to see if legal */
    if (borg_prayer_legal(3, 5))
    {
        borg_skill[BI_AHWORD] += 1000;
    }

    /* speed spells */
    if ( borg_spell_legal( 3, 3 ) ||
         borg_spell_legal( 7, 3 ) ||
         borg_equips_artifact(ART_TARATOL, INVEN_WIELD) ||
         borg_equips_artifact(ART_FEANOR, INVEN_FEET) ||
         borg_equips_artifact(ART_TULKAS, INVEN_RIGHT) )
    {
        borg_skill[BI_ASPEED] += 1000;
    }

    /* Handle "cure light wounds" */
    if (borg_equips_artifact(ART_LOTHARANG, INVEN_WIELD))
    {
        borg_skill[BI_ACSW] += 1000;
    }


    /* Handle "heal" */
    if (borg_equips_artifact(ART_SOULKEEPER,INVEN_BODY) ||
        borg_equips_artifact(ART_GONDOR,INVEN_HEAD) ||
        borg_prayer_legal(3, 2) ||
        borg_prayer_legal(6, 2))
    {
        borg_skill[BI_AHEAL] += 1000;
    }

#if 0
    /* Handle "phase" */
    if (borg_equips_artifact(ART_BELEGENNON, INVEN_BODY))
    {
        amt_phase += 1000;
    }

    /* Handle "escape" */
    if (borg_equips_artifact(ART_COLANNON,INVEN_OUTER))
    {
        borg_skill[BI_AESCAPE] += 1000;
    }
#endif

    /* Handle "fix exp" */
    if (borg_equips_artifact(ART_LUTHIEN, INVEN_OUTER))
    {
        amt_fix_exp += 1000;
    }

    /* Handle "fix exp" */
    if (borg_equips_artifact(ART_THINGOL, INVEN_OUTER) ||
        borg_spell_legal(3,1) ||
        borg_prayer_legal(7,1))
    {
        borg_skill[BI_ARECHARGE] += 1000;
    }

    /*** Process the Needs ***/

    /* No need for fuel */
    if ((auto_items[INVEN_LITE].sval != SV_LITE_TORCH) &&
        (auto_items[INVEN_LITE].sval != SV_LITE_LANTERN)) borg_skill[BI_AFUEL] += 1000;

    /* No need to *buy* stat increase potions */
    if (my_stat_cur[A_STR] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_STR] + cp_ptr->c_adj[A_STR]))
        amt_add_stat[A_STR] += 1000;

    if (my_stat_cur[A_INT] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_INT] + cp_ptr->c_adj[A_INT]))
         amt_add_stat[A_INT] += 1000;

    if (my_stat_cur[A_WIS] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_WIS] + cp_ptr->c_adj[A_WIS]))
        amt_add_stat[A_WIS] += 1000;

    if (my_stat_cur[A_DEX] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_DEX] + cp_ptr->c_adj[A_DEX]))
         amt_add_stat[A_DEX] += 1000;

    if (my_stat_cur[A_CON] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_CON] + cp_ptr->c_adj[A_CON]))
        amt_add_stat[A_CON] += 1000;

    if (my_stat_cur[A_CHR] >= (18+100) + 10 * adult_maximize *
        (rp_ptr->r_adj[A_CHR] + cp_ptr->c_adj[A_CHR]))
         amt_add_stat[A_CHR] += 1000;

    /* No need to *buy* stat repair potions */
    if (!do_fix_stat[A_STR]) amt_fix_stat[A_STR] += 1000;
    if (!do_fix_stat[A_INT]) amt_fix_stat[A_INT] += 1000;
    if (!do_fix_stat[A_WIS]) amt_fix_stat[A_WIS] += 1000;
    if (!do_fix_stat[A_DEX]) amt_fix_stat[A_DEX] += 1000;
    if (!do_fix_stat[A_CON]) amt_fix_stat[A_CON] += 1000;
    if (!do_fix_stat[A_CHR]) amt_fix_stat[A_CHR] += 1000;


    /* No need for experience repair */
    if (!do_fix_exp) amt_fix_exp += 1000;

    /* Correct the high and low calorie foods */
    borg_skill[BI_FOOD] += amt_food_hical;
    if (amt_food_hical <= 3) borg_skill[BI_FOOD] += amt_food_lowcal;

    /* Correct the heal pots */
    borg_skill[BI_AEZHEAL] += amt_ez_heal_star + amt_ez_heal_life;

    /* If weak, do not count food spells */
   if (do_weak && (borg_skill[BI_FOOD] >= 1000))
        borg_skill[BI_FOOD] -= 1000;

}


/*
 * Helper function -- notice the player swap weapon
 */
void borg_notice_weapon_swap(void)
{
    int i;
    int b_i = 0;

    s32b v =-1L;
    s32b b_v = 0L;

    int dam, damage;
    auto_item *item;

    weapon_swap =0;

    /*** Process the inventory ***/
    for (i = 0; i < INVEN_PACK; i++)
    {
        item = &auto_items[i];

        /* reset counter */
        v= -1L;
        dam =0;
        damage =0;

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Hack -- skip un-aware items */
        if (!item->kind) continue;

        /* Dont carry swaps until dlevel 50.  They are heavy.
         * Unless the item is a digger, then carry it */
        if (auto_max_depth < 54 && item->tval !=TV_DIGGING) continue;

        /* priest weapon penalty for non-blessed edged weapons */
        if (auto_class == CLASS_PRIEST &&
            (item->tval == TV_SWORD || item->tval == TV_POLEARM) &&
            (!(item->flags3 & TR3_BLESSED))) continue;

        /* Require ID, "known" (or average, good, etc) */
        if (!item->able &&
            !streq(item->note, "{good}") &&
            !streq(item->note, "{excellent}") &&
            !streq(item->note, "{terrible}") &&
            !streq(item->note, "{special}")) continue;

       /* Clear all the swap weapon flags as I look at each one. */
        weapon_swap_digger = 0;
        weapon_swap_slay_animal = FALSE;
        weapon_swap_slay_evil = FALSE;
        weapon_swap_slay_undead = FALSE;
        weapon_swap_slay_demon = FALSE;
        weapon_swap_slay_orc = FALSE;
        weapon_swap_slay_troll = FALSE;
        weapon_swap_slay_giant = FALSE;
        weapon_swap_slay_dragon = FALSE;
        weapon_swap_kill_dragon = FALSE;
        weapon_swap_impact = FALSE;
        weapon_swap_brand_acid = FALSE;
        weapon_swap_brand_elec = FALSE;
        weapon_swap_brand_fire = FALSE;
        weapon_swap_brand_cold = FALSE;
        weapon_swap_see_infra = FALSE;
        weapon_swap_slow_digest = FALSE;
        weapon_swap_aggravate = FALSE;
        weapon_swap_teleport = FALSE;
        weapon_swap_regenerate = FALSE;
        weapon_swap_telepathy = FALSE;
        weapon_swap_lite = FALSE;
        weapon_swap_see_invis = FALSE;
        weapon_swap_ffall = FALSE;
        weapon_swap_free_act = FALSE;
        weapon_swap_hold_life = FALSE;
        weapon_swap_immune_fire = FALSE;
        weapon_swap_immune_acid = FALSE;
        weapon_swap_immune_cold = FALSE;
        weapon_swap_immune_elec = FALSE;
        weapon_swap_resist_acid = FALSE;
        weapon_swap_resist_elec = FALSE;
        weapon_swap_resist_fire = FALSE;
        weapon_swap_resist_cold = FALSE;
        weapon_swap_resist_pois = FALSE;
        weapon_swap_resist_conf = FALSE;
        weapon_swap_resist_sound = FALSE;
        weapon_swap_resist_lite = FALSE;
        weapon_swap_resist_dark = FALSE;
        weapon_swap_resist_chaos = FALSE;
        weapon_swap_resist_disen = FALSE;
        weapon_swap_resist_shard = FALSE;
        weapon_swap_resist_nexus = FALSE;
        weapon_swap_resist_blind = FALSE;
        weapon_swap_resist_neth = FALSE;
        decurse_weapon_swap =-1;

        /* Analyze the item */
        switch (item->tval)
        {

            /* weapons */
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
            case TV_DIGGING:
            {

            /* Digging */
            if (item->flags1 & TR1_TUNNEL)
            {
                /* Don't notice digger if we can turn stone to mud,
                 * or I am using one.
                 */
                /* Hack -- ignore worthless ones (including cursed) */
                if (item->value <= 0) break;
                if (item->cursed) break;
                if (!borg_spell_okay_fail(1, 8, 40) &&
                    !(auto_items[INVEN_WIELD].flags1 & TR1_TUNNEL))
                weapon_swap_digger = item->pval;
            }

            /* various slays */
            if (item->flags1 & TR1_SLAY_ANIMAL) weapon_swap_slay_animal = TRUE;
            if (item->flags1 & TR1_SLAY_EVIL)   weapon_swap_slay_evil = TRUE;
            if (item->flags1 & TR1_SLAY_UNDEAD) weapon_swap_slay_undead = TRUE;
            if (item->flags1 & TR1_SLAY_DEMON)  weapon_swap_slay_demon = TRUE;
            if (item->flags1 & TR1_SLAY_ORC)    weapon_swap_slay_orc = TRUE;
            if (item->flags1 & TR1_SLAY_TROLL)  weapon_swap_slay_troll = TRUE;
            if (item->flags1 & TR1_SLAY_GIANT)  weapon_swap_slay_giant = TRUE;
            if (item->flags1 & TR1_SLAY_DRAGON) weapon_swap_slay_dragon = TRUE;
            if (item->flags1 & TR1_KILL_DRAGON) weapon_swap_kill_dragon = TRUE;
            if (item->flags3 & TR3_IMPACT)      weapon_swap_impact = TRUE;
            if (item->flags1 & TR1_BRAND_ACID)  weapon_swap_brand_acid = TRUE;
            if (item->flags1 & TR1_BRAND_ELEC)  weapon_swap_brand_elec = TRUE;
            if (item->flags1 & TR1_BRAND_FIRE)  weapon_swap_brand_fire = TRUE;
            if (item->flags1 & TR1_BRAND_COLD)  weapon_swap_brand_cold = TRUE;

            /* Affect infravision */
            if (item->flags1 & TR1_INFRA) weapon_swap_see_infra += item->pval;

            /* Affect speed */

            /* Various flags */
            if (item->flags3 & TR3_SLOW_DIGEST) weapon_swap_slow_digest = TRUE;
            if (item->flags3 & TR3_AGGRAVATE) weapon_swap_aggravate = TRUE;
            if (item->flags3 & TR3_TELEPORT) weapon_swap_teleport = TRUE;
            if (item->flags3 & TR3_REGEN) weapon_swap_regenerate = TRUE;
            if (item->flags3 & TR3_TELEPATHY) weapon_swap_telepathy = TRUE;
            if (item->flags3 & TR3_LITE) weapon_swap_lite = TRUE;
            if (item->flags3 & TR3_SEE_INVIS) weapon_swap_see_invis = TRUE;
            if (item->flags3 & TR3_FEATHER) weapon_swap_ffall = TRUE;
            if (item->flags3 & TR3_FREE_ACT) weapon_swap_free_act = TRUE;
            if (item->flags3 & TR3_HOLD_LIFE) weapon_swap_hold_life = TRUE;

            /* Immunity flags */
            /* if you are immune you automaticly resist */
            if (item->flags2 & TR2_IM_FIRE)
            {
                weapon_swap_immune_fire = TRUE;
                weapon_swap_resist_fire = TRUE;
            }
            if (item->flags2 & TR2_IM_ACID)
            {
                weapon_swap_immune_acid = TRUE;
                weapon_swap_resist_acid = TRUE;
            }
            if (item->flags2 & TR2_IM_COLD)
            {
                weapon_swap_immune_cold = TRUE;
                weapon_swap_resist_cold = TRUE;
            }
            if (item->flags2 & TR2_IM_ELEC)
            {
                weapon_swap_immune_elec = TRUE;
                weapon_swap_resist_elec = TRUE;
            }

            /* Resistance flags */
            if (item->flags2 & TR2_RES_ACID) weapon_swap_resist_acid = TRUE;
            if (item->flags2 & TR2_RES_ELEC) weapon_swap_resist_elec = TRUE;
            if (item->flags2 & TR2_RES_FIRE) weapon_swap_resist_fire = TRUE;
            if (item->flags2 & TR2_RES_COLD) weapon_swap_resist_cold = TRUE;
            if (item->flags2 & TR2_RES_POIS) weapon_swap_resist_pois = TRUE;
            if (item->flags2 & TR2_RES_CONFU) weapon_swap_resist_conf = TRUE;
            if (item->flags2 & TR2_RES_SOUND) weapon_swap_resist_sound = TRUE;
            if (item->flags2 & TR2_RES_LITE) weapon_swap_resist_lite = TRUE;
            if (item->flags2 & TR2_RES_DARK) weapon_swap_resist_dark = TRUE;
            if (item->flags2 & TR2_RES_CHAOS) weapon_swap_resist_chaos = TRUE;
            if (item->flags2 & TR2_RES_DISEN) weapon_swap_resist_disen = TRUE;
            if (item->flags2 & TR2_RES_SHARD) weapon_swap_resist_shard = TRUE;
            if (item->flags2 & TR2_RES_NEXUS) weapon_swap_resist_nexus = TRUE;
            if (item->flags2 & TR2_RES_BLIND) weapon_swap_resist_blind = TRUE;
            if (item->flags2 & TR2_RES_NETHR) weapon_swap_resist_neth = TRUE;
            if (item->cursed) decurse_weapon_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_weapon_swap = 1;
            if (item->discount == INSCRIP_UNCURSED)
            {
                decurse_weapon_swap = -1;
            }
            /* Sustain flags */

            /* calculating the value of the swap weapon. */
            damage = (item->dd * (item->ds) *25L);

            /* Reward "damage" and increased blows per round*/
            v += damage * (borg_skill[BI_BLOWS]+1);

            /* Reward "bonus to hit" */
            v += ((borg_skill[BI_TOHIT] + item->to_h)*25L);

            /* Reward "bonus to dam" */
            v += ((borg_skill[BI_TODAM] + item->to_d)*30L);

            dam = damage * borg_skill[BI_BLOWS];

            /* assume 2x base damage for x% of creatures */
            dam = damage * 2 * borg_skill[BI_BLOWS];
            /* rewared SAnimal if no electric brand */
            if (!borg_skill[BI_WS_ANIMAL] && !borg_skill[BI_WB_ELEC] && weapon_swap_slay_animal) v += (dam*2) /2;
            if (!borg_skill[BI_WS_EVIL] && weapon_swap_slay_evil) v +=  (dam*7) /2;

            /* assume 3x base damage for x% of creatures */
            dam = damage *3*borg_skill[BI_BLOWS];

            /* half of the reward now for SOrc and STroll*/
            if (!borg_skill[BI_WS_ORC] && weapon_swap_slay_orc) v += (dam*1) /2;
            if (!borg_skill[BI_WS_TROLL] && weapon_swap_slay_troll) v += (dam*2) /2;

            if (!borg_skill[BI_WS_UNDEAD] && weapon_swap_slay_undead) v += (dam*5) /2;
            if (!borg_skill[BI_WS_DEMON] && weapon_swap_slay_demon) v += (dam*3) /2;
            if (!borg_skill[BI_WS_GIANT] && weapon_swap_slay_giant) v += (dam*4) /2;
            if (!borg_skill[BI_WS_DRAGON] && !borg_skill[BI_WK_DRAGON] && weapon_swap_slay_dragon) v += (dam*6) /2;
            if (!borg_skill[BI_WB_ACID] && weapon_swap_brand_acid) v += (dam*4) /2;
            if (!borg_skill[BI_WB_ELEC] && weapon_swap_brand_elec) v += (dam*5) /2;
            if (!borg_skill[BI_WB_FIRE] && weapon_swap_brand_fire) v += (dam*3) /2;
            if (!borg_skill[BI_WB_COLD] && weapon_swap_brand_cold) v += (dam*3) /2;
            /* Orcs and Trolls get the second half of the reward if SEvil is not possesed. */
            if (!borg_skill[BI_WS_ORC] && !borg_skill[BI_WS_EVIL] && weapon_swap_slay_orc) v += (dam*1) /2;
            if (!borg_skill[BI_WS_TROLL] && !borg_skill[BI_WS_EVIL] && weapon_swap_slay_troll) v += (dam*1) /2;

            /* assume 5x base damage for x% of creatures */
            dam = damage  * 5 * borg_skill[BI_BLOWS];
            if (!borg_skill[BI_WK_DRAGON] && weapon_swap_kill_dragon) v += (dam*5) /2;

            /* reward the Tunnel factor when low level */
            if (auto_max_depth <= 54 && weapon_swap_digger) v += (weapon_swap_digger * 3500L) + 1000L;

            /* Other Skills */
            if (!borg_skill[BI_SDIG] && weapon_swap_slow_digest) v += 10L;
            if (weapon_swap_aggravate) v -= 8000L;
            if (weapon_swap_teleport) v -= 100000L;
            if (decurse_weapon_swap != -1) v -= 5000L;
            if (!borg_skill[BI_REG] && weapon_swap_regenerate) v += 2000L;
            if (!borg_skill[BI_ESP] && weapon_swap_telepathy) v += 5000L;
            if (!borg_skill[BI_LITE] && weapon_swap_lite) v += 2000L;
            if (!borg_skill[BI_SINV] && weapon_swap_see_invis) v += 50000L;
            if (!borg_skill[BI_FEATH] && weapon_swap_ffall) v += 10L;
            if (!borg_skill[BI_FRACT] && weapon_swap_free_act) v += 10000L;
            if (!borg_skill[BI_HLIFE] && (auto_max_level < 50) && weapon_swap_hold_life) v += 2000L;
            if (!borg_skill[BI_IFIRE] && weapon_swap_immune_fire) v += 70000L;
            if (!borg_skill[BI_IACID] && weapon_swap_immune_acid) v += 30000L;
            if (!borg_skill[BI_ICOLD] && weapon_swap_immune_cold) v += 50000L;
            if (!borg_skill[BI_IELEC] && weapon_swap_immune_elec) v += 25000L;
            if (!borg_skill[BI_RFIRE] && weapon_swap_resist_fire) v += 8000L;
            if (!borg_skill[BI_RACID] && weapon_swap_resist_acid) v += 6000L;
            if (!borg_skill[BI_RCOLD] && weapon_swap_resist_cold) v += 4000L;
            if (!borg_skill[BI_RELEC] && weapon_swap_resist_elec) v += 3000L;
            /* extra bonus for getting all basic resist */
            if (weapon_swap_resist_fire &&
                weapon_swap_resist_acid &&
                weapon_swap_resist_elec &&
                weapon_swap_resist_cold) v +=  10000L;
            if (!borg_skill[BI_RPOIS] && weapon_swap_resist_pois) v += 20000L;
            if (!borg_skill[BI_RCONF] && weapon_swap_resist_conf) v += 5000L;
            if (!borg_skill[BI_RSND] && weapon_swap_resist_sound) v += 2000L;
            if (!borg_skill[BI_RLITE] && weapon_swap_resist_lite) v += 800L;
            if (!borg_skill[BI_RDARK] && weapon_swap_resist_dark) v += 800L;
            if (!borg_skill[BI_RKAOS] && weapon_swap_resist_chaos) v += 8000L;
            if (!borg_skill[BI_RDIS] && weapon_swap_resist_disen) v += 5000L;
            if (!borg_skill[BI_RSHRD] && weapon_swap_resist_shard) v += 100L;
            if (!borg_skill[BI_RNXUS] && weapon_swap_resist_nexus) v += 100L;
            if (!borg_skill[BI_RBLIND] && weapon_swap_resist_blind) v += 5000L;
            if (!borg_skill[BI_RNTHR] && weapon_swap_resist_neth) v += 5500L;
            if (!borg_skill[BI_RFEAR] && weapon_swap_resist_fear) v += 5500L;

            /* Special concern if Tarrasque is alive */
            if (auto_max_depth >= 75 &&
               ((!borg_skill[BI_ICOLD] && weapon_swap_immune_cold) ||
                (!borg_skill[BI_IFIRE] && weapon_swap_immune_fire)))
            {
               /* If Tarraseque is alive */
               if (auto_race_death[539] == 0)
               {
                   if (!borg_skill[BI_ICOLD] && weapon_swap_immune_cold) v  += 90000L;
                   if (!borg_skill[BI_IFIRE] && weapon_swap_immune_fire) v  += 90000L;
               }

            }


            /*  Mega-Hack -- resists (level 60) */
            /* its possible that he will get a sword and a cloak
             * both with the same high resist and keep each based
             * on that resist.  We want him to check to see
             * that the other swap does not already have the high resist.
             */
            if (!borg_skill[BI_RNTHR]  && (auto_max_depth+1 >= 55) &&
                weapon_swap_resist_neth) v += 100000L;
            if (!borg_skill[BI_RKAOS] && (auto_max_depth+1 >= 60) &&
                weapon_swap_resist_chaos) v += 100000L;
            if (!borg_skill[BI_RDIS] && (auto_max_depth+1 >= 60) &&
                weapon_swap_resist_disen) v += 100000L;

            /* some artifacts would make good back ups for their activation */


            /* skip usless ones */
            if (v <= 1000) continue;

            /* collect the best one */
            if (v < b_v) continue;

            /* track it */
            b_i = i;
            b_v = v;
        }


        }
    }
    /* mark the swap item and its value */
    weapon_swap_value = b_v;
    weapon_swap = b_i;

    /* Now that we know who the best swap is lets set our swap
     * flags and get a move on
     */
    /*** Process the best inven item ***/

    item = &auto_items[b_i];

   /* Clear all the swap weapon flags as I look at each one. */
    weapon_swap_slay_animal = FALSE;
    weapon_swap_slay_evil = FALSE;
    weapon_swap_slay_undead = FALSE;
    weapon_swap_slay_demon = FALSE;
    weapon_swap_slay_orc = FALSE;
    weapon_swap_slay_troll = FALSE;
    weapon_swap_slay_giant = FALSE;
    weapon_swap_slay_dragon = FALSE;
    weapon_swap_kill_dragon = FALSE;
    weapon_swap_impact = FALSE;
    weapon_swap_brand_acid = FALSE;
    weapon_swap_brand_elec = FALSE;
    weapon_swap_brand_fire = FALSE;
    weapon_swap_brand_cold = FALSE;
    weapon_swap_see_infra = FALSE;
    weapon_swap_slow_digest = FALSE;
    weapon_swap_aggravate = FALSE;
    weapon_swap_teleport = FALSE;
    weapon_swap_regenerate = FALSE;
    weapon_swap_telepathy = FALSE;
    weapon_swap_lite = FALSE;
    weapon_swap_see_invis = FALSE;
    weapon_swap_ffall = FALSE;
    weapon_swap_free_act = FALSE;
    weapon_swap_hold_life = FALSE;
    weapon_swap_immune_fire = FALSE;
    weapon_swap_immune_acid = FALSE;
    weapon_swap_immune_cold = FALSE;
    weapon_swap_immune_elec = FALSE;
    weapon_swap_resist_acid = FALSE;
    weapon_swap_resist_elec = FALSE;
    weapon_swap_resist_fire = FALSE;
    weapon_swap_resist_cold = FALSE;
    weapon_swap_resist_pois = FALSE;
    weapon_swap_resist_conf = FALSE;
    weapon_swap_resist_sound = FALSE;
    weapon_swap_resist_lite = FALSE;
    weapon_swap_resist_dark = FALSE;
    weapon_swap_resist_chaos = FALSE;
    weapon_swap_resist_disen = FALSE;
    weapon_swap_resist_shard = FALSE;
    weapon_swap_resist_nexus = FALSE;
    weapon_swap_resist_blind = FALSE;
    weapon_swap_resist_neth = FALSE;
    decurse_weapon_swap = -1;

    /* Assume no enchantment needed */
    enchant_weapon_swap_to_h = 0;
    enchant_weapon_swap_to_d = 0;

    /* Enchant swap weapons (to hit) */
    if ((borg_prayer_okay_fail(7, 3, 40) ||
        amt_enchant_weapon >=1 ) )
    {
        if (item->to_h < 10)
        {
            enchant_weapon_swap_to_h += (10 - item->to_h);
        }

        /* Enchant my swap (to damage) */
        if (item->to_d < 10)
        {
            enchant_weapon_swap_to_d += (10 - item->to_d);
        }
    }
    else
    {
        if (item->to_h < 8)
        {
            enchant_weapon_swap_to_h += (8 - item->to_h);
        }

        /* Enchant my swap (to damage) */
        if (item->to_d < 8)
        {
            enchant_weapon_swap_to_d += (8 - item->to_d);
        }
    }

    /* various slays */
    if (item->flags1 & TR1_SLAY_ANIMAL) weapon_swap_slay_animal = TRUE;
    if (item->flags1 & TR1_SLAY_EVIL)   weapon_swap_slay_evil = TRUE;
    if (item->flags1 & TR1_SLAY_UNDEAD) weapon_swap_slay_undead = TRUE;
    if (item->flags1 & TR1_SLAY_DEMON)  weapon_swap_slay_demon = TRUE;
    if (item->flags1 & TR1_SLAY_ORC)    weapon_swap_slay_orc = TRUE;
    if (item->flags1 & TR1_SLAY_TROLL)  weapon_swap_slay_troll = TRUE;
    if (item->flags1 & TR1_SLAY_GIANT)  weapon_swap_slay_giant = TRUE;
    if (item->flags1 & TR1_SLAY_DRAGON) weapon_swap_slay_dragon = TRUE;
    if (item->flags1 & TR1_KILL_DRAGON) weapon_swap_kill_dragon = TRUE;
    if (item->flags3 & TR3_IMPACT)      weapon_swap_impact = TRUE;
    if (item->flags1 & TR1_BRAND_ACID)  weapon_swap_brand_acid = TRUE;
    if (item->flags1 & TR1_BRAND_ELEC)  weapon_swap_brand_elec = TRUE;
    if (item->flags1 & TR1_BRAND_FIRE)  weapon_swap_brand_fire = TRUE;
    if (item->flags1 & TR1_BRAND_COLD)  weapon_swap_brand_cold = TRUE;

    /* Affect infravision */
    if (item->flags1 & TR1_INFRA) weapon_swap_see_infra += item->pval;
    /* Affect various skills */
    /* Affect speed */

    /* Various flags */
    if (item->flags3 & TR3_SLOW_DIGEST) weapon_swap_slow_digest = TRUE;
    if (item->flags3 & TR3_AGGRAVATE) weapon_swap_aggravate = TRUE;
    if (item->flags3 & TR3_TELEPORT) weapon_swap_teleport = TRUE;
    if (item->flags3 & TR3_REGEN) weapon_swap_regenerate = TRUE;
    if (item->flags3 & TR3_TELEPATHY) weapon_swap_telepathy = TRUE;
    if (item->flags3 & TR3_LITE) weapon_swap_lite = TRUE;
    if (item->flags3 & TR3_SEE_INVIS) weapon_swap_see_invis = TRUE;
    if (item->flags3 & TR3_FEATHER) weapon_swap_ffall = TRUE;
    if (item->flags3 & TR3_FREE_ACT) weapon_swap_free_act = TRUE;
    if (item->flags3 & TR3_HOLD_LIFE) weapon_swap_hold_life = TRUE;

    /* Immunity flags */
    /* if you are immune you automaticly resist */
    if (item->flags2 & TR2_IM_FIRE)
    {
        weapon_swap_immune_fire = TRUE;
        weapon_swap_resist_fire = TRUE;
    }
    if (item->flags2 & TR2_IM_ACID)
    {
        weapon_swap_immune_acid = TRUE;
        weapon_swap_resist_acid = TRUE;
    }
    if (item->flags2 & TR2_IM_COLD)
    {
        weapon_swap_immune_cold = TRUE;
        weapon_swap_resist_cold = TRUE;
    }
    if (item->flags2 & TR2_IM_ELEC)
    {
        weapon_swap_immune_elec = TRUE;
        weapon_swap_resist_elec = TRUE;
    }

    /* Resistance flags */
    if (item->flags2 & TR2_RES_ACID) weapon_swap_resist_acid = TRUE;
    if (item->flags2 & TR2_RES_ELEC) weapon_swap_resist_elec = TRUE;
    if (item->flags2 & TR2_RES_FIRE) weapon_swap_resist_fire = TRUE;
    if (item->flags2 & TR2_RES_COLD) weapon_swap_resist_cold = TRUE;
    if (item->flags2 & TR2_RES_POIS) weapon_swap_resist_pois = TRUE;
    if (item->flags2 & TR2_RES_CONFU) weapon_swap_resist_conf = TRUE;
    if (item->flags2 & TR2_RES_SOUND) weapon_swap_resist_sound = TRUE;
    if (item->flags2 & TR2_RES_LITE) weapon_swap_resist_lite = TRUE;
    if (item->flags2 & TR2_RES_DARK) weapon_swap_resist_dark = TRUE;
    if (item->flags2 & TR2_RES_CHAOS) weapon_swap_resist_chaos = TRUE;
    if (item->flags2 & TR2_RES_DISEN) weapon_swap_resist_disen = TRUE;
    if (item->flags2 & TR2_RES_SHARD) weapon_swap_resist_shard = TRUE;
    if (item->flags2 & TR2_RES_NEXUS) weapon_swap_resist_nexus = TRUE;
    if (item->flags2 & TR2_RES_BLIND) weapon_swap_resist_blind = TRUE;
    if (item->flags2 & TR2_RES_NETHR) weapon_swap_resist_neth = TRUE;
    if (item->cursed) decurse_weapon_swap = 0;
    if (item->flags3 & TR3_HEAVY_CURSE) decurse_weapon_swap = 1;
    if (item->discount == INSCRIP_UNCURSED)
    {
       decurse_weapon_swap = -1;
    }

}

/*
 * Helper function -- notice the player swap armour
 */
void borg_notice_armour_swap(void)
{
    int i;
    int b_i = 0;
    s32b v = -1L;
    s32b b_v = 0L;
    int dam, damage;

    auto_item *item;

    armour_swap = 0;

    /* borg option to not use them */
    if (!borg_uses_swaps) return;

    /*** Process the inventory ***/
    for (i = 0; i < INVEN_PACK; i++)
    {
        item = &auto_items[i];

        /* reset counter */
        v= -1L;
        dam =0;
        damage =0;

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Hack -- skip un-aware items */
        if (!item->kind) continue;

        /* Dont carry swaps until dlevel 50.  They are heavy */
        if (auto_max_depth < 50) continue;

        /* Require "known" (or average, good, etc) */
        if (!item->able &&
            !streq(item->note, "{good}") &&
            !streq(item->note, "{excellent}") &&
            !streq(item->note, "{terrible}") &&
            !streq(item->note, "{special}")) continue;

        /* One Ring is not a swap */
        if (item->name1 == ART_POWER) continue;

        /* Clear all the swap weapon flags as I look at each one. */
        armour_swap_slay_animal = FALSE;
        armour_swap_slay_evil = FALSE;
        armour_swap_slay_undead = FALSE;
        armour_swap_slay_demon = FALSE;
        armour_swap_slay_orc = FALSE;
        armour_swap_slay_troll = FALSE;
        armour_swap_slay_giant = FALSE;
        armour_swap_slay_dragon = FALSE;
        armour_swap_kill_dragon = FALSE;
        armour_swap_impact = FALSE;
        armour_swap_brand_acid = FALSE;
        armour_swap_brand_elec = FALSE;
        armour_swap_brand_fire = FALSE;
        armour_swap_brand_cold = FALSE;
        armour_swap_see_infra = FALSE;
        armour_swap_slow_digest = FALSE;
        armour_swap_aggravate = FALSE;
        armour_swap_teleport = FALSE;
        armour_swap_regenerate = FALSE;
        armour_swap_telepathy = FALSE;
        armour_swap_lite = FALSE;
        armour_swap_see_invis = FALSE;
        armour_swap_ffall = FALSE;
        armour_swap_free_act = FALSE;
        armour_swap_hold_life = FALSE;
        armour_swap_immune_fire = FALSE;
        armour_swap_immune_acid = FALSE;
        armour_swap_immune_cold = FALSE;
        armour_swap_immune_elec = FALSE;
        armour_swap_resist_acid = FALSE;
        armour_swap_resist_elec = FALSE;
        armour_swap_resist_fire = FALSE;
        armour_swap_resist_cold = FALSE;
        armour_swap_resist_pois = FALSE;
        armour_swap_resist_conf = FALSE;
        armour_swap_resist_sound = FALSE;
        armour_swap_resist_lite = FALSE;
        armour_swap_resist_dark = FALSE;
        armour_swap_resist_chaos = FALSE;
        armour_swap_resist_disen = FALSE;
        armour_swap_resist_shard = FALSE;
        armour_swap_resist_nexus = FALSE;
        armour_swap_resist_blind = FALSE;
        armour_swap_resist_neth = FALSE;
        decurse_armour_swap = -1;

        /* Analyze the item */
        switch (item->tval)
        {
            /* ARMOUR TYPE STUFF */
            case TV_RING:
            case TV_AMULET:
            case TV_BOOTS:
            case TV_HELM:
            case TV_CROWN:
            case TV_SHIELD:
            case TV_CLOAK:
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
            case TV_DRAG_ARMOR:
            {
            /* various slays */
            /* as of 280, armours dont have slays but random artifacts might.
             */
            if (item->flags1 & TR1_SLAY_ANIMAL) armour_swap_slay_animal = TRUE;
            if (item->flags1 & TR1_SLAY_EVIL)   armour_swap_slay_evil = TRUE;
            if (item->flags1 & TR1_SLAY_UNDEAD) armour_swap_slay_undead = TRUE;
            if (item->flags1 & TR1_SLAY_DEMON)  armour_swap_slay_demon = TRUE;
            if (item->flags1 & TR1_SLAY_ORC)    armour_swap_slay_orc = TRUE;
            if (item->flags1 & TR1_SLAY_TROLL)  armour_swap_slay_troll = TRUE;
            if (item->flags1 & TR1_SLAY_GIANT)  armour_swap_slay_giant = TRUE;
            if (item->flags1 & TR1_SLAY_DRAGON) armour_swap_slay_dragon = TRUE;
            if (item->flags1 & TR1_KILL_DRAGON) armour_swap_kill_dragon = TRUE;
            if (item->flags3 & TR3_IMPACT)      armour_swap_impact = TRUE;
            if (item->flags1 & TR1_BRAND_ACID)  armour_swap_brand_acid = TRUE;
            if (item->flags1 & TR1_BRAND_ELEC)  armour_swap_brand_elec = TRUE;
            if (item->flags1 & TR1_BRAND_FIRE)  armour_swap_brand_fire = TRUE;
            if (item->flags1 & TR1_BRAND_COLD)  armour_swap_brand_cold = TRUE;

            /* Affect infravision */
            if (item->flags1 & TR1_INFRA) armour_swap_see_infra += item->pval;
            /* Affect various skills */
            /* Affect speed */

            /* Various flags */
            if (item->flags3 & TR3_SLOW_DIGEST) armour_swap_slow_digest = TRUE;
            if (item->flags3 & TR3_AGGRAVATE) armour_swap_aggravate = TRUE;
            if (item->flags3 & TR3_TELEPORT) armour_swap_teleport = TRUE;
            if (item->flags3 & TR3_REGEN) armour_swap_regenerate = TRUE;
            if (item->flags3 & TR3_TELEPATHY) armour_swap_telepathy = TRUE;
            if (item->flags3 & TR3_LITE) armour_swap_lite = TRUE;
            if (item->flags3 & TR3_SEE_INVIS) armour_swap_see_invis = TRUE;
            if (item->flags3 & TR3_FEATHER) armour_swap_ffall = TRUE;
            if (item->flags3 & TR3_FREE_ACT) armour_swap_free_act = TRUE;
            if (item->flags3 & TR3_HOLD_LIFE) armour_swap_hold_life = TRUE;

            /* Immunity flags */
            /* if you are immune you automaticly resist */
            if (item->flags2 & TR2_IM_FIRE)
            {
                armour_swap_immune_fire = TRUE;
                armour_swap_resist_fire = TRUE;
            }
            if (item->flags2 & TR2_IM_ACID)
            {
                armour_swap_immune_acid = TRUE;
                armour_swap_resist_acid = TRUE;
            }
            if (item->flags2 & TR2_IM_COLD)
            {
                armour_swap_immune_cold = TRUE;
                armour_swap_resist_cold = TRUE;
            }
            if (item->flags2 & TR2_IM_ELEC)
            {
                armour_swap_immune_elec = TRUE;
                armour_swap_resist_elec = TRUE;
            }

            /* Resistance flags */
            if (item->flags2 & TR2_RES_ACID) armour_swap_resist_acid = TRUE;
            if (item->flags2 & TR2_RES_ELEC) armour_swap_resist_elec = TRUE;
            if (item->flags2 & TR2_RES_FIRE) armour_swap_resist_fire = TRUE;
            if (item->flags2 & TR2_RES_COLD) armour_swap_resist_cold = TRUE;
            if (item->flags2 & TR2_RES_POIS) armour_swap_resist_pois = TRUE;
            if (item->flags2 & TR2_RES_CONFU) armour_swap_resist_conf = TRUE;
            if (item->flags2 & TR2_RES_SOUND) armour_swap_resist_sound = TRUE;
            if (item->flags2 & TR2_RES_LITE) armour_swap_resist_lite = TRUE;
            if (item->flags2 & TR2_RES_DARK) armour_swap_resist_dark = TRUE;
            if (item->flags2 & TR2_RES_CHAOS) armour_swap_resist_chaos = TRUE;
            if (item->flags2 & TR2_RES_DISEN) armour_swap_resist_disen = TRUE;
            if (item->flags2 & TR2_RES_SHARD) armour_swap_resist_shard = TRUE;
            if (item->flags2 & TR2_RES_NEXUS) armour_swap_resist_nexus = TRUE;
            if (item->flags2 & TR2_RES_BLIND) armour_swap_resist_blind = TRUE;
            if (item->flags2 & TR2_RES_NETHR) armour_swap_resist_neth = TRUE;
            if (item->cursed) decurse_armour_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_armour_swap = 1;

            /* Sustain flags */

            /* calculating the value of the swap weapon. */
            damage = (item->dd * item->ds *35L);
            /* Reward "damage" and increased blows per round*/
            v += damage * (borg_skill[BI_BLOWS]+1);

            /* Reward "bonus to hit" */
            v += ((borg_skill[BI_TOHIT] + item->to_h)*35L);

            /* Reward "bonus to dam" */
            v += ((borg_skill[BI_TODAM] + item->to_d)*35L);

            dam = damage * borg_skill[BI_BLOWS];

            /* assume 2x base damage for x% of creatures */
            dam = damage * 2 * borg_skill[BI_BLOWS];

            if (!borg_skill[BI_WS_ANIMAL] && !borg_skill[BI_WB_ELEC] && armour_swap_slay_animal) v += (dam*2) /2;
            if (!borg_skill[BI_WS_EVIL] && armour_swap_slay_evil) v +=  (dam*7) /2;
            /* assume 3x base damage for x% of creatures */
            dam = damage *3*borg_skill[BI_BLOWS];

            if (!borg_skill[BI_WS_UNDEAD] && armour_swap_slay_undead) v += (dam*5) /2;
            if (!borg_skill[BI_WS_DEMON] && armour_swap_slay_demon) v += (dam*3) /2;
            if (!borg_skill[BI_WS_GIANT] && armour_swap_slay_giant) v += (dam*4) /2;
            if (!borg_skill[BI_WS_DRAGON] && !borg_skill[BI_WK_DRAGON] && armour_swap_slay_dragon) v += (dam*6) /2;
            if (!borg_skill[BI_WB_ACID] && armour_swap_brand_acid) v += (dam*4) /2;
            if (!borg_skill[BI_WB_ELEC] && armour_swap_brand_elec) v += (dam*5) /2;
            if (!borg_skill[BI_WB_FIRE] && armour_swap_brand_fire) v += (dam*3) /2;
            if (!borg_skill[BI_WB_COLD] && armour_swap_brand_cold) v += (dam*3) /2;
            /* SOrc and STroll get 1/2 reward now */
            if (!borg_skill[BI_WS_ORC] && armour_swap_slay_orc) v += (dam*1) /2;
            if (!borg_skill[BI_WS_TROLL] && armour_swap_slay_troll) v += (dam*2) /2;
            /* SOrc and STroll get 2/2 reward if slay evil not possesed */
            if (!borg_skill[BI_WS_ORC] && !borg_skill[BI_WS_EVIL] && armour_swap_slay_orc) v += (dam*1) /2;
            if (!borg_skill[BI_WS_TROLL] && !borg_skill[BI_WS_EVIL] && armour_swap_slay_troll) v += (dam*1) /2;

            /* assume 5x base damage for x% of creatures */
            dam = damage  * 5 * borg_skill[BI_BLOWS];
            if (!borg_skill[BI_WK_DRAGON] && armour_swap_kill_dragon) v += (dam*5) /2;


            if (!borg_skill[BI_SDIG] && armour_swap_slow_digest) v += 10L;
            if (armour_swap_aggravate) v -= 8000L;
            if (armour_swap_teleport) v -= 100000L;
            if (decurse_armour_swap != -1) v -= 5000L;
            if (!borg_skill[BI_REG] && armour_swap_regenerate) v += 2000L;
            if (!borg_skill[BI_ESP] && armour_swap_telepathy) v += 5000L;
            if (!borg_skill[BI_LITE] && armour_swap_lite) v += 2000L;
            if (!borg_skill[BI_SINV] && armour_swap_see_invis) v += 50000L;
            if (!borg_skill[BI_FEATH] && armour_swap_ffall) v += 10L;
            if (!borg_skill[BI_FRACT] && armour_swap_free_act) v += 10000L;
            if (!borg_skill[BI_HLIFE] && (auto_max_level < 50) && armour_swap_hold_life) v += 2000L;
            if (!borg_skill[BI_IFIRE] && armour_swap_immune_fire) v += 70000L;
            if (!borg_skill[BI_IACID] && armour_swap_immune_acid) v += 30000L;
            if (!borg_skill[BI_ICOLD] && armour_swap_immune_cold) v += 50000L;
            if (!borg_skill[BI_IELEC] && armour_swap_immune_elec) v += 25000L;
            if (!borg_skill[BI_RFIRE] && armour_swap_resist_fire) v += 8000L;
            if (!borg_skill[BI_RACID] && armour_swap_resist_acid) v += 6000L;
            if (!borg_skill[BI_RCOLD] && armour_swap_resist_cold) v += 4000L;
            if (!borg_skill[BI_RELEC] && armour_swap_resist_elec) v += 3000L;
            /* extra bonus for getting all basic resist */
            if (armour_swap_resist_fire &&
                armour_swap_resist_acid &&
                armour_swap_resist_elec &&
                armour_swap_resist_cold) v +=  10000L;
            if (!borg_skill[BI_RPOIS] && armour_swap_resist_pois) v += 20000L;
            if (!borg_skill[BI_RCONF] && armour_swap_resist_conf) v += 5000L;
            if (!borg_skill[BI_RSND] && armour_swap_resist_sound) v += 2000L;
            if (!borg_skill[BI_RLITE] && armour_swap_resist_lite) v += 800L;
            if (!borg_skill[BI_RDARK] && armour_swap_resist_dark) v += 800L;
            if (!borg_skill[BI_RKAOS] && armour_swap_resist_chaos) v += 8000L;
            if (!borg_skill[BI_RDIS] && armour_swap_resist_disen) v += 5000L;
            if (!borg_skill[BI_RSHRD] && armour_swap_resist_shard) v += 100L;
            if (!borg_skill[BI_RNXUS] && armour_swap_resist_nexus) v += 100L;
            if (!borg_skill[BI_RBLIND] && armour_swap_resist_blind) v += 5000L;
            if (!borg_skill[BI_RNTHR] && armour_swap_resist_neth) v += 5500L;
            /* Special concern if Tarraseque is alive */
            if (auto_max_depth >= 75 &&
               ((!borg_skill[BI_ICOLD] && armour_swap_immune_cold) ||
                (!borg_skill[BI_IFIRE] && armour_swap_immune_fire)))
            {
               /* If Tarraseque is alive */
               if (auto_race_death[539] == 0)
               {
                  if (!borg_skill[BI_ICOLD] && armour_swap_immune_cold) v  += 90000L;
                  if (!borg_skill[BI_IFIRE] && armour_swap_immune_fire) v  += 90000L;
               }

            }



            /*  Mega-Hack -- resists (level 60) */
            /* Its possible that he will get a sword and a cloak
             * both with the same high resist and keep each based
             * on that resist.  We want him to check to see
             * that the other swap does not already have the high resist.
             */
            if (!borg_skill[BI_RNTHR]  && auto_max_depth+1 >= 55  &&
                !weapon_swap_resist_neth &&
                armour_swap_resist_neth) v += 105000L;
            if (!borg_skill[BI_RKAOS] && auto_max_depth+1 >= 60 &&
                !weapon_swap_resist_chaos &&
                armour_swap_resist_chaos) v += 104000L;
            if (!borg_skill[BI_RDIS] && auto_max_depth+1 >= 60 &&
                !weapon_swap_resist_disen &&
                armour_swap_resist_disen) v += 100000L;

            /* some artifacts would make good back ups for their activation */

            }

            /* skip usless ones */
            if (v <= 1000) continue;

            /* collect the best one */
            if ((b_i >=0) && (v < b_v)) continue;

            /* track it */
            b_i = i;
            b_v = v;
            armour_swap_value = v;
            armour_swap = i;
        }
    }

        /* Now that we know who the best swap is lets set our swap
         * flags and get a move on
         */
        /*** Process the best inven item ***/

        item = &auto_items[b_i];

       /* Clear all the swap weapon flags as I look at each one. */
        armour_swap_slay_animal = FALSE;
        armour_swap_slay_evil = FALSE;
        armour_swap_slay_undead = FALSE;
        armour_swap_slay_demon = FALSE;
        armour_swap_slay_orc = FALSE;
        armour_swap_slay_troll = FALSE;
        armour_swap_slay_giant = FALSE;
        armour_swap_slay_dragon = FALSE;
        armour_swap_kill_dragon = FALSE;
        armour_swap_impact = FALSE;
        armour_swap_brand_acid = FALSE;
        armour_swap_brand_elec = FALSE;
        armour_swap_brand_fire = FALSE;
        armour_swap_brand_cold = FALSE;
        armour_swap_see_infra = FALSE;
        armour_swap_slow_digest = FALSE;
        armour_swap_aggravate = FALSE;
        armour_swap_teleport = FALSE;
        armour_swap_regenerate = FALSE;
        armour_swap_telepathy = FALSE;
        armour_swap_lite = FALSE;
        armour_swap_see_invis = FALSE;
        armour_swap_ffall = FALSE;
        armour_swap_free_act = FALSE;
        armour_swap_hold_life = FALSE;
        armour_swap_immune_fire = FALSE;
        armour_swap_immune_acid = FALSE;
        armour_swap_immune_cold = FALSE;
        armour_swap_immune_elec = FALSE;
        armour_swap_resist_acid = FALSE;
        armour_swap_resist_elec = FALSE;
        armour_swap_resist_fire = FALSE;
        armour_swap_resist_cold = FALSE;
        armour_swap_resist_pois = FALSE;
        armour_swap_resist_conf = FALSE;
        armour_swap_resist_sound = FALSE;
        armour_swap_resist_lite = FALSE;
        armour_swap_resist_dark = FALSE;
        armour_swap_resist_chaos = FALSE;
        armour_swap_resist_disen = FALSE;
        armour_swap_resist_shard = FALSE;
        armour_swap_resist_nexus = FALSE;
        armour_swap_resist_blind = FALSE;
        armour_swap_resist_neth = FALSE;
        decurse_armour_swap = -1;

        /* various slays */
            if (item->flags1 & TR1_SLAY_ANIMAL) armour_swap_slay_animal = TRUE;
            if (item->flags1 & TR1_SLAY_EVIL)   armour_swap_slay_evil = TRUE;
            if (item->flags1 & TR1_SLAY_UNDEAD) armour_swap_slay_undead = TRUE;
            if (item->flags1 & TR1_SLAY_DEMON)  armour_swap_slay_demon = TRUE;
            if (item->flags1 & TR1_SLAY_ORC)    armour_swap_slay_orc = TRUE;
            if (item->flags1 & TR1_SLAY_TROLL)  armour_swap_slay_troll = TRUE;
            if (item->flags1 & TR1_SLAY_GIANT)  armour_swap_slay_giant = TRUE;
            if (item->flags1 & TR1_SLAY_DRAGON) armour_swap_slay_dragon = TRUE;
            if (item->flags1 & TR1_KILL_DRAGON) armour_swap_kill_dragon = TRUE;
            if (item->flags3 & TR3_IMPACT)      armour_swap_impact = TRUE;
            if (item->flags1 & TR1_BRAND_ACID)  armour_swap_brand_acid = TRUE;
            if (item->flags1 & TR1_BRAND_ELEC)  armour_swap_brand_elec = TRUE;
            if (item->flags1 & TR1_BRAND_FIRE)  armour_swap_brand_fire = TRUE;
            if (item->flags1 & TR1_BRAND_COLD)  armour_swap_brand_cold = TRUE;

            /* Affect infravision */
            if (item->flags1 & TR1_INFRA) armour_swap_see_infra += item->pval;
            /* Affect various skills */
            /* Affect speed */

            /* Various flags */
            if (item->flags3 & TR3_SLOW_DIGEST) armour_swap_slow_digest = TRUE;
            if (item->flags3 & TR3_AGGRAVATE) armour_swap_aggravate = TRUE;
            if (item->flags3 & TR3_TELEPORT) armour_swap_teleport = TRUE;
            if (item->flags3 & TR3_REGEN) armour_swap_regenerate = TRUE;
            if (item->flags3 & TR3_TELEPATHY) armour_swap_telepathy = TRUE;
            if (item->flags3 & TR3_LITE) armour_swap_lite = TRUE;
            if (item->flags3 & TR3_SEE_INVIS) armour_swap_see_invis = TRUE;
            if (item->flags3 & TR3_FEATHER) armour_swap_ffall = TRUE;
            if (item->flags3 & TR3_FREE_ACT) armour_swap_free_act = TRUE;
            if (item->flags3 & TR3_HOLD_LIFE) armour_swap_hold_life = TRUE;

            /* Immunity flags */
            /* if you are immune you automaticly resist */
            if (item->flags2 & TR2_IM_FIRE)
            {
                armour_swap_immune_fire = TRUE;
                armour_swap_resist_fire = TRUE;
            }
            if (item->flags2 & TR2_IM_ACID)
            {
                armour_swap_immune_acid = TRUE;
                armour_swap_resist_acid = TRUE;
            }
            if (item->flags2 & TR2_IM_COLD)
            {
                armour_swap_immune_cold = TRUE;
                armour_swap_resist_cold = TRUE;
            }
            if (item->flags2 & TR2_IM_ELEC)
            {
                armour_swap_immune_elec = TRUE;
                armour_swap_resist_elec = TRUE;
            }

            /* Resistance flags */
            if (item->flags2 & TR2_RES_ACID) armour_swap_resist_acid = TRUE;
            if (item->flags2 & TR2_RES_ELEC) armour_swap_resist_elec = TRUE;
            if (item->flags2 & TR2_RES_FIRE) armour_swap_resist_fire = TRUE;
            if (item->flags2 & TR2_RES_COLD) armour_swap_resist_cold = TRUE;
            if (item->flags2 & TR2_RES_POIS) armour_swap_resist_pois = TRUE;
            if (item->flags2 & TR2_RES_CONFU) armour_swap_resist_conf = TRUE;
            if (item->flags2 & TR2_RES_SOUND) armour_swap_resist_sound = TRUE;
            if (item->flags2 & TR2_RES_LITE) armour_swap_resist_lite = TRUE;
            if (item->flags2 & TR2_RES_DARK) armour_swap_resist_dark = TRUE;
            if (item->flags2 & TR2_RES_CHAOS) armour_swap_resist_chaos = TRUE;
            if (item->flags2 & TR2_RES_DISEN) armour_swap_resist_disen = TRUE;
            if (item->flags2 & TR2_RES_SHARD) armour_swap_resist_shard = TRUE;
            if (item->flags2 & TR2_RES_NEXUS) armour_swap_resist_nexus = TRUE;
            if (item->flags2 & TR2_RES_BLIND) armour_swap_resist_blind = TRUE;
            if (item->flags2 & TR2_RES_NETHR) armour_swap_resist_neth = TRUE;
            if (item->cursed) decurse_armour_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_armour_swap = 1;

        enchant_armour_swap_to_a = 0;

        /* dont look for enchantment on non armours */
        if (item->tval >= TV_LITE) return;

        /* Hack -- enchant the swap equipment (armor) */
        /* Note need for enchantment */
        if ((borg_prayer_okay_fail(7, 4, 40) ||
            amt_enchant_armor >=1 ))
        {
            if (item->to_a < 10)
            {
                enchant_armour_swap_to_a += (10 - item->to_a);
            }
        }
        else
        {
            if (item->to_a < 8)
            {
                enchant_armour_swap_to_a += (8 - item->to_a);
            }
        }

}

/*
 * Analyze the equipment and inventory
 */
void borg_notice(bool notice_swap)
{
    /* Clear out 'has' array */
    memset(borg_has, 0, size_obj*sizeof(int));

    /* Notice the equipment */
    borg_notice_aux1();

    /* Notice the inventory */
    borg_notice_aux2();

    /* Notice and locate my swap weapon */
    if (notice_swap)
    {
        borg_notice_weapon_swap();
        borg_notice_armour_swap();
    }
    borg_skill[BI_SRACID] = borg_skill[BI_RACID]
                            || armour_swap_resist_acid
                            || weapon_swap_resist_acid;
    borg_skill[BI_SRELEC] = borg_skill[BI_RELEC]
                            || armour_swap_resist_elec
                            || weapon_swap_resist_elec;
    borg_skill[BI_SRFIRE] = borg_skill[BI_RFIRE]
                            || armour_swap_resist_fire
                            || weapon_swap_resist_fire;
    borg_skill[BI_SRCOLD] = borg_skill[BI_RCOLD]
                            || armour_swap_resist_cold
                            || weapon_swap_resist_cold;
    borg_skill[BI_SRPOIS] = borg_skill[BI_RPOIS]
                            || armour_swap_resist_pois
                            || weapon_swap_resist_pois;
    borg_skill[BI_SRFEAR] = borg_skill[BI_RFEAR]
                            || armour_swap_resist_fear
                            || weapon_swap_resist_fear;
    borg_skill[BI_SRLITE] = borg_skill[BI_RLITE]
                            || armour_swap_resist_lite
                            || weapon_swap_resist_lite;
    borg_skill[BI_SRDARK] = borg_skill[BI_RDARK]
                            || armour_swap_resist_dark
                            || weapon_swap_resist_dark;
    borg_skill[BI_SRBLIND] = borg_skill[BI_RBLIND]
                            || armour_swap_resist_blind
                            || weapon_swap_resist_blind;
    borg_skill[BI_SRCONF] = borg_skill[BI_RCONF]
                            || armour_swap_resist_conf
                            || weapon_swap_resist_conf;
    borg_skill[BI_SRSND] = borg_skill[BI_RSND]
                            || armour_swap_resist_sound
                            || weapon_swap_resist_sound;
    borg_skill[BI_SRSHRD] = borg_skill[BI_RSHRD]
                            || armour_swap_resist_shard
                            || weapon_swap_resist_shard;
    borg_skill[BI_SRNXUS] = borg_skill[BI_RNXUS]
                            || armour_swap_resist_nexus
                            || weapon_swap_resist_nexus;
    borg_skill[BI_SRNTHR] = borg_skill[BI_RNTHR]
                            || armour_swap_resist_neth
                            || weapon_swap_resist_neth;
    borg_skill[BI_SRKAOS] = borg_skill[BI_RKAOS]
                            || armour_swap_resist_chaos
                            || weapon_swap_resist_chaos;
    borg_skill[BI_SRDIS] = borg_skill[BI_RDIS]
                            || armour_swap_resist_disen
                            || weapon_swap_resist_disen;
    borg_skill[BI_SHLIFE] = borg_skill[BI_HLIFE]
                            || armour_swap_hold_life
                            || weapon_swap_hold_life;
    borg_skill[BI_SFRACT] = borg_skill[BI_FRACT]
                            || armour_swap_free_act
                            || weapon_swap_free_act;

}

/*
 * Helper function -- notice the home equipment
 */
static void borg_notice_home_aux1(auto_item *in_item, bool no_items)
{

    /*** Reset counters ***/

    /* Reset basic */
    num_food = 0;
    num_mold = 0;
    num_ident = 0;
    num_star_ident = 0;
    num_recall = 0;
    num_phase = 0;
    num_escape = 0;
    num_teleport = 0;
    num_teleport_level =0;

    num_artifact = 0;

    num_invisible = 0;
    num_pfe =0;
    num_glyph = 0;
    num_genocide = 0;
    num_mass_genocide = 0;
    num_berserk = 0;
    num_pot_rheat = 0;
    num_pot_rcold = 0;
    num_speed = 0;

    num_slow_digest = 0;
    num_regenerate = 0;
    num_telepathy = 0;
    num_see_inv = 0;
    num_ffall = 0;
    num_free_act = 0;
    num_hold_life = 0;
    num_immune_acid = 0;
    num_immune_elec = 0;
    num_immune_fire = 0;
    num_immune_cold = 0;
    num_resist_acid = 0;
    num_resist_elec = 0;
    num_resist_fire = 0;
    num_resist_cold = 0;
    num_resist_pois = 0;
    num_resist_conf = 0;
    num_resist_sound = 0;
    num_resist_lite = 0;
    num_resist_dark = 0;
    num_resist_chaos = 0;
    num_resist_disen = 0;
    num_resist_shard = 0;
    num_resist_nexus = 0;
    num_resist_blind = 0;
    num_resist_neth = 0;
    num_sustain_str = 0;
    num_sustain_int = 0;
    num_sustain_wis = 0;
    num_sustain_dex =0;
    num_sustain_con = 0;
    num_sustain_all = 0;

    home_stat_add[A_STR] = 0;
    home_stat_add[A_INT] = 0;
    home_stat_add[A_WIS] = 0;
    home_stat_add[A_DEX] = 0;
    home_stat_add[A_CON] = 0;
    home_stat_add[A_CHR] = 0;

    num_weapons = 0;

    num_bow =0;
    num_rings = 0;
    num_neck = 0;
    num_armor = 0;
    num_cloaks = 0;
    num_shields = 0;
    num_hats = 0;
    num_gloves = 0;
    num_boots = 0;
    num_lite = 0;
    num_speed = 0;
    num_edged_weapon = 0;
    num_bad_gloves= 0;

    /* Reset healing */
    num_cure_critical = 0;
    num_cure_serious = 0;
    num_fix_exp = 0;
    num_mana = 0;
    num_heal = 0;
    num_ez_heal = 0;
    if (!in_item && !no_items) num_ez_heal_true = 0;
    if (!in_item && !no_items) num_heal_true = 0;


    /* Reset missiles */
    num_missile = 0;

    /* Reset books */
    num_book[0] = 0;
    num_book[1] = 0;
    num_book[2] = 0;
    num_book[3] = 0;
    num_book[4] = 0;
    num_book[5] = 0;
    num_book[6] = 0;
    num_book[7] = 0;
    num_book[8] = 0;

    /* Reset various */
    num_fix_stat[A_STR] = 0;
    num_fix_stat[A_INT] = 0;
    num_fix_stat[A_WIS] = 0;
    num_fix_stat[A_DEX] = 0;
    num_fix_stat[A_CON] = 0;
    num_fix_stat[A_CHR] = 0;
    num_fix_stat[6] = 0;

    /* Reset enchantment */
    num_enchant_to_a = 0;
    num_enchant_to_d = 0;
    num_enchant_to_h = 0;

    home_slot_free = 0;
    home_damage = 0;

    num_duplicate_items = 0;
}


/*
 * This checks for duplicate items in the home
 */
static void borg_notice_home_dupe(auto_item *item, bool check_sval, int i)
{
/* eventually check for power overlap... armor of resistence is same as weak elvenkind.*/
/*  two armors of elvenkind that resist poison is a dupe.  AJG*/

    int dupe_count, x;
    auto_item *item2;

    /* check for a duplicate.  */
    /* be carefull about extra powers (elvenkind/magi) */
    switch (item->name2)
    {
        case EGO_BLESS_BLADE:
        case EGO_PERMANENCE:
        case EGO_ELVENKIND:
        case EGO_MAGI:
        case EGO_AMAN:
        case EGO_HA:
            return;
            break;
        case EGO_DF:
        default:
            break;
    }

    /* if this is a stack of items then all after the first are a */
    /* duplicate */
    dupe_count = item->iqty-1;

    /* Look for other items before this one that are the same */
    for (x = 0; x < i; x++)
    {
        if (x < STORE_INVEN_MAX)
            item2 = &auto_shops[7].ware[x];
        else
            /* Check what the borg has on as well.*/
            item2 = &auto_items[((x-STORE_INVEN_MAX)+INVEN_WIELD)];

        /* if everything matches it is a duplicate item */
        /* Note that we only check sval on certain items.  This */
        /* is because, for example, two pairs of dragon armor */
        /* are not the same unless thier subtype (color) matches */
        /* but a defender is a defender even if one is a dagger and */
        /* one is a mace */
        if ( (item->tval == item2->tval) &&
             (check_sval ? (item->sval == item2->sval) : TRUE) &&
             (item->name1 == item2->name1) &&
             (item->name2 == item2->name2) )
        {
            dupe_count++;
        }
    }

    /* there can be one dupe of rings because there are two ring slots. */
    if (item->tval == TV_RING && dupe_count)
        dupe_count--;

    /* Add this items count to the total duplicate count */
    num_duplicate_items += dupe_count;
}

/*
 * Helper function -- notice the home inventory
 */
static void borg_notice_home_aux2(auto_item *in_item, bool no_items)
{
    int i;

    auto_item *item;

    auto_shop *shop = &auto_shops[7];

    u32b f1, f2, f3;

    /*** Process the inventory ***/

    /* Scan the home */
    for (i = 0; i < (STORE_INVEN_MAX+(INVEN_TOTAL-INVEN_WIELD)); i++)
    {
        if (no_items) break;

        if (!in_item)
            if (i < STORE_INVEN_MAX)
                item = &shop->ware[i];
            else
                item = &auto_items[((i-STORE_INVEN_MAX)+INVEN_WIELD)];
        else
            item = in_item;

        /* Skip empty items */
        if (!item->iqty)
        {
            home_slot_free++;
            continue;
        }

        /* Hack -- skip un-aware items */
        if (!item->kind)
        {
            home_slot_free++;
            continue;
        }

        if (item->flags3 & TR3_SLOW_DIGEST) num_slow_digest += item->iqty;
        if (item->flags3 & TR3_REGEN) num_regenerate += item->iqty;
        if (item->flags3 & TR3_TELEPATHY) num_telepathy += item->iqty;
        if (item->flags3 & TR3_SEE_INVIS) num_see_inv += item->iqty;
        if (item->flags3 & TR3_FEATHER) num_ffall += item->iqty;
        if (item->flags3 & TR3_FREE_ACT) num_free_act += item->iqty;
        if (item->flags3 & TR3_HOLD_LIFE) num_hold_life += item->iqty;
        if (item->flags2 & TR2_IM_FIRE)
        {
            num_immune_fire += item->iqty;
            num_resist_fire += item->iqty;
        }
        if (item->flags2 & TR2_IM_ACID)
        {
            num_immune_acid += item->iqty;
            num_resist_acid += item->iqty;
        }
        if (item->flags2 & TR2_IM_COLD)
        {
            num_immune_cold += item->iqty;
            num_resist_cold += item->iqty;
        }
        if (item->flags2 & TR2_IM_ELEC)
        {
            num_immune_elec += item->iqty;
            num_resist_elec += item->iqty;
        }
        if (item->flags2 & TR2_RES_ACID) num_resist_acid += item->iqty;
        if (item->flags2 & TR2_RES_ELEC) num_resist_elec += item->iqty;
        if (item->flags2 & TR2_RES_FIRE) num_resist_fire += item->iqty;
        if (item->flags2 & TR2_RES_COLD) num_resist_cold += item->iqty;
        if (item->flags2 & TR2_RES_POIS) num_resist_pois += item->iqty;
        if (item->flags2 & TR2_RES_SOUND) num_resist_sound += item->iqty;
        if (item->flags2 & TR2_RES_LITE) num_resist_lite += item->iqty;
        if (item->flags2 & TR2_RES_DARK) num_resist_dark += item->iqty;
        if (item->flags2 & TR2_RES_CHAOS) num_resist_chaos += item->iqty;
        if (item->flags2 & TR2_RES_CONFU) num_resist_conf += item->iqty;
        if (item->flags2 & TR2_RES_DISEN) num_resist_disen += item->iqty;
        if (item->flags2 & TR2_RES_SHARD) num_resist_shard += item->iqty;
        if (item->flags2 & TR2_RES_NEXUS) num_resist_nexus += item->iqty;
        if (item->flags2 & TR2_RES_BLIND) num_resist_blind += item->iqty;
        if (item->flags2 & TR2_RES_NETHR) num_resist_neth += item->iqty;

        /* Count Sustains */
        if (item->flags2 & TR2_SUST_STR) num_sustain_str += item->iqty;
        if (item->flags2 & TR2_SUST_INT) num_sustain_str += item->iqty;
        if (item->flags2 & TR2_SUST_WIS) num_sustain_str += item->iqty;
        if (item->flags2 & TR2_SUST_DEX) num_sustain_str += item->iqty;
        if (item->flags2 & TR2_SUST_CON) num_sustain_str += item->iqty;
        if (item->flags2 & TR2_SUST_STR &&
           item->flags2 & TR2_SUST_INT  &&
           item->flags2 & TR2_SUST_WIS  &&
           item->flags2 & TR2_SUST_DEX  &&
           item->flags2 & TR2_SUST_CON) num_sustain_all +=item->iqty;

        /* count up bonus to stats */
        /* HACK only collect stat rings above +3 */
        if (item->flags1 & TR1_STR)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_STR] += item->pval * item->iqty;
        }
        if (item->flags1 & TR1_INT)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_INT] += item->pval * item->iqty;
        }
        if (item->flags1 & TR1_WIS)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_WIS] += item->pval * item->iqty;
        }
        if (item->flags1 & TR1_DEX)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_DEX] += item->pval * item->iqty;
        }
        if (item->flags1 & TR1_CON)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_CON] += item->pval * item->iqty;
        }
        if (item->flags1 & TR1_CHR)
        {
            if (item->tval != TV_RING || item->pval > 3)
                home_stat_add[A_CHR] += item->pval * item->iqty;
        }

        /* count up bonus to speed */
        if (item->flags1 & TR1_SPEED) num_speed += item->pval * item->iqty;

        /* count artifacts */
        if (item->name1)
        {
            num_artifact += item->iqty;
        }

        /* Analyze the item */
        switch (item->tval)
        {
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
                num_armor += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );
                break;

            case TV_DRAG_ARMOR:
                num_armor += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, TRUE, i );
                break;

            case TV_CLOAK:
                num_cloaks += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );

                break;

            case TV_SHIELD:
                num_shields += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );
                break;

            case TV_HELM:
            case TV_CROWN:
                num_hats += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );

                break;

            case TV_GLOVES:
                num_gloves += item->iqty;

                /* most gloves hurt magic for spell-casters */
                if (mb_ptr->spell_book == TV_MAGIC_BOOK && auto_msp > 3)
                {
                    /* Penalize non-usable gloves */
                    if (item->iqty &&
                        (!(item->flags3 & TR3_FREE_ACT)) &&
                        (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
                    {
                        num_bad_gloves += item->iqty;
                    }
                }

                /* gloves of slaying give a damage bonus */
                home_damage += item->to_d * 3;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );

                break;

            case TV_LITE:
                if (item->name1)
                {
                    num_lite += item->iqty;
                }
                break;

            case TV_BOOTS:
                num_boots += item->iqty;

                /* see if this item is duplicated */
                borg_notice_home_dupe( item, FALSE, i );
                break;

            case TV_SWORD:
            case TV_POLEARM:
            case TV_HAFTED:
            case TV_DIGGING:
            {
                s16b num_blow;

                num_weapons += item->iqty;
                /* apw most edged weapons hurt magic for priests */
                if (auto_class == CLASS_PRIEST)
                {
                    /* Penalize non-blessed edged weapons */
                    if (((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
                        (!(item->flags3 & TR3_BLESSED)))
                    {
                        num_edged_weapon += item->iqty;
                    }
                }


                /* NOTE:  This damage does not take slays into account. */
                /* it is just a rough estimate to make sure the glave of pain*/
                /* is kept if it is found */
                /* It is hard to hold a heavy weapon */
                num_blow = 1;
                if (adj_str_hold[my_stat_ind[A_STR]] >= item->weight / 10)
                {
                    int str_index, dex_index;
                    int num = 0, wgt = 0, mul = 0, div = 0;

                    /* Analyze the class */
                    switch (auto_class)
                    {
                        /* Warrior */
                        case CLASS_WARRIOR: num = 6; wgt = 30; mul = 5; break;

                        /* Mage */
                        case CLASS_MAGE: num = 4; wgt = 40; mul = 2; break;

                        /* Priest (was mul = 3.5) */
                        case CLASS_PRIEST: num = 5; wgt = 35; mul = 3; break;

                        /* Rogue */
                        case CLASS_ROGUE: num = 5; wgt = 30; mul = 3; break;

                        /* Ranger */
                        case CLASS_RANGER: num = 5; wgt = 35; mul = 4; break;

                        /* Paladin */
                        case CLASS_PALADIN: num = 5; wgt = 30; mul = 4; break;

                    }

                    /* Enforce a minimum "weight" */
                    div = ((item->weight < wgt) ? wgt : item->weight);

                    /* Access the strength vs weight */
                    str_index = (adj_str_blow[my_stat_ind[A_STR]] * mul / div);

                    /* Maximal value */
                    if (str_index > 11) str_index = 11;

                    /* Index by dexterity */
                    dex_index = (adj_dex_blow[my_stat_ind[A_DEX]]);

                    /* Maximal value */
                    if (dex_index > 11) dex_index = 11;

                    /* Use the blows table */
                    num_blow = blows_table[str_index][dex_index];

                    /* Maximal value */
                    if (num_blow > num) num_blow = num;

                }

                /* Require at least one blow */
                if (num_blow < 1) num_blow = 1;

                if (item->flags1 & TR1_BLOWS)
                    num_blow += item->pval;
                    num_blow *= item->iqty;
                if ( item->to_d > 8 || borg_skill[BI_CLEVEL] < 15 )
                {
                    home_damage += num_blow * (item->dd * (item->ds) +
                                         (borg_skill[BI_TODAM] + item->to_d));
                }
                else
                {
                    home_damage += num_blow * (item->dd * (item->ds) +
                                        (borg_skill[BI_TODAM] + 8));
                }

                /* see if this item is a duplicate */
                borg_notice_home_dupe( item, FALSE, i );
                break;
            }

            case TV_BOW:
                num_bow += item->iqty;

                /* see if this item is a duplicate */
                borg_notice_home_dupe( item, FALSE, i );
                break;

            case TV_RING:
                num_rings += item->iqty;

                /* see if this item is a duplicate */
                borg_notice_home_dupe( item, TRUE, i );

                break;

            case TV_AMULET:
                num_neck += item->iqty;

                /* see if this item is a duplicate */
                borg_notice_home_dupe( item, TRUE, i );
                break;


            /* Books */
            case TV_MAGIC_BOOK:
            case TV_PRAYER_BOOK:

            /* Skip incorrect books */
            if (item->tval != mb_ptr->spell_book) break;

            /* Count the books */
            num_book[item->sval] += item->iqty;

            break;


            /* Food */
            case TV_FOOD:

            /* Analyze */
            switch (item->sval)
            {
                case SV_FOOD_WAYBREAD:
                num_food += item->iqty;
                break;

                case SV_FOOD_RATION:
                num_food += item->iqty;
                break;

                case SV_FOOD_SLIME_MOLD:
                num_mold += item->iqty;
                break;

#if 0
                /* dont clutter the house with these low calorie foods */
                case SV_FOOD_JERKY:
                    num_food += item->iqty;
                break;
                /* dont clutter the house with these low calorie foods */
                case SV_FOOD_BISCUIT:
                    num_food += item->iqty;
                break;
#endif

                case SV_FOOD_RESTORE_STR:
                num_fix_stat[A_STR] += item->iqty;
                break;

                case SV_FOOD_RESTORE_CON:
                num_fix_stat[A_CON] += item->iqty;
                break;

                case SV_FOOD_RESTORING:
                num_fix_stat[A_STR] += item->iqty;
                num_fix_stat[A_INT] += item->iqty;
                num_fix_stat[A_WIS] += item->iqty;
                num_fix_stat[A_DEX] += item->iqty;
                num_fix_stat[A_CON] += item->iqty;
                num_fix_stat[A_CHR] += item->iqty;
                num_fix_stat[6]     += item->iqty;
                break;
            }

            break;


            /* Potions */
            case TV_POTION:

            /* Analyze */
            switch (item->sval)
            {
                case SV_POTION_CURE_CRITICAL:
                num_cure_critical += item->iqty;
                break;

                case SV_POTION_CURE_SERIOUS:
                num_cure_serious += item->iqty;
                break;

                case SV_POTION_RESIST_HEAT:
                num_pot_rheat += item->iqty;
                break;
                case SV_POTION_RESIST_COLD:
                num_pot_rcold += item->iqty;
                break;

                case SV_POTION_RES_STR:
                num_fix_stat[A_STR] += item->iqty;
                break;

                case SV_POTION_RES_INT:
                num_fix_stat[A_INT] += item->iqty;
                break;

                case SV_POTION_RES_WIS:
                num_fix_stat[A_WIS] += item->iqty;
                break;

                case SV_POTION_RES_DEX:
                num_fix_stat[A_DEX] += item->iqty;
                break;

                case SV_POTION_RES_CON:
                num_fix_stat[A_CON] += item->iqty;
                break;

                case SV_POTION_RES_CHR:
                num_fix_stat[A_CHR] += item->iqty;
                break;

                case SV_POTION_RESTORE_EXP:
                num_fix_exp += item->iqty;
                break;

                case SV_POTION_RESTORE_MANA:
                num_mana += item->iqty;
                break;

                case SV_POTION_HEALING:
                num_heal += item->iqty;
                if (!in_item && !no_items) num_heal_true += item->iqty;
                break;

                case SV_POTION_STAR_HEALING:
                num_ez_heal += item->iqty;
                if (!in_item && !no_items) num_ez_heal_true += item->iqty;
                break;

                case SV_POTION_LIFE:
                num_ez_heal += item->iqty;
                if (!in_item && !no_items) num_ez_heal_true += item->iqty;
                break;

                case SV_POTION_BESERK_STRENGTH:
                num_berserk += item->iqty;
                break;

                case SV_POTION_SPEED:
                num_speed += item->iqty;
                break;

            }

            break;


            /* Scrolls */
            case TV_SCROLL:

            /* Analyze the scroll */
            switch (item->sval)
            {
                case SV_SCROLL_IDENTIFY:
                num_ident += item->iqty;
                break;

                case SV_SCROLL_STAR_IDENTIFY:
                num_star_ident += item->iqty;
                break;

                case SV_SCROLL_PHASE_DOOR:
                num_phase += item->iqty;
                break;

                case SV_SCROLL_TELEPORT:
                num_escape += item->iqty;
                break;

                case SV_SCROLL_WORD_OF_RECALL:
                num_recall += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_ARMOR:
                num_enchant_to_a += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
                num_enchant_to_h += item->iqty;
                break;

                case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
                num_enchant_to_d += item->iqty;
                break;

                /* apw */
                case SV_SCROLL_PROTECTION_FROM_EVIL:
                num_pfe += item->iqty;
                break;

                /* apw */
                case SV_SCROLL_RUNE_OF_PROTECTION:
                num_glyph += item->iqty;
                break;

                /* apw */
                case SV_SCROLL_TELEPORT_LEVEL:
                num_teleport_level += item->iqty;
                break;

            }

            break;


            /* Rods */
            case TV_ROD:

            /* Analyze */
            switch (item->sval)
            {
                case SV_ROD_IDENTIFY:
                num_ident += item->iqty * 100;
                break;

                case SV_ROD_RECALL:
                num_recall += item->iqty * 100;
                break;
            }

            break;


            /* Staffs */
            case TV_STAFF:

            /* only collect staves with more than 3 charges at high level */
            if (item->pval <= 3 && borg_skill[BI_CLEVEL] > 30)
                break;

            /* Analyze */
            switch (item->sval)
            {
                case SV_STAFF_IDENTIFY:
                num_ident += item->iqty * item->pval;
                break;

                case SV_STAFF_TELEPORTATION:
                /* Don't use them deep in the dungeon because the
                 * charges will get drained and he wont have any
                 * scrolls left to read
                 */
                if (auto_max_depth < 97)
                {
                    num_teleport += item->iqty * item->pval;
                }
                break;
            }

            break;


            /* Missiles */
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT:

            /* Hack -- ignore invalid missiles */
            if (item->tval != my_ammo_tval) break;

            /* Hack -- ignore worthless missiles */
            if (item->value <= 0) break;

            /* Count them */
            num_missile += item->iqty;

            break;
        }

        /* if only doing one item, break. */
        if (in_item) break;
    }


    /*** Process the Spells and Prayers ***/
   /* apw, again.  Artifact activation included here */

    /* Handle "satisfy hunger" -> infinite food */
    if (borg_spell_legal(2, 0) || borg_prayer_legal(1, 5))
    {
        num_food += 1000;
    }

    /* Handle "identify" -> infinite identifies */
    if (borg_spell_legal(2, 4) ||
    borg_equips_artifact(ART_ERIRIL ,INVEN_WIELD) ||
    borg_prayer_legal(5, 2))
    {
        num_ident += 1000;
    }

    /* Handle "enchant weapon" */
    if (borg_prayer_okay_fail(7, 3, 40))
    {
        num_enchant_to_h += 1000;
        num_enchant_to_d += 1000;
    }

    /* apw Handle "protection from evil" */
    if (borg_prayer_legal(2, 4) ||
    borg_equips_artifact(ART_CARLAMMAS, INVEN_HEAD))
    {
        num_pfe += 1000;
    }

    /* apw Handle "rune of protection" glyph */
    if (borg_prayer_legal(3, 4))
    {
        num_glyph += 1000;
    }

    /* handle restore */

    /* Handle recall */
    if (borg_prayer_legal(4, 4) ||
        borg_spell_legal(5, 4) ||
        borg_equips_artifact(ART_AVAVIR ,INVEN_WIELD) )
    {
        num_recall += 1000;
    }

    /* Handle teleport_level */
    if (borg_prayer_legal(4, 3) ||
        borg_spell_legal(5, 2))
    {
        num_teleport_level += 1000;
    }

#if 0
    /* Handle Speed */
    if (borg_spell_legal(3, 3) ||
        borg_spell_legal(7, 3))
    {
        num_speed += 1000;
    }

#endif

    /*** Process the Needs ***/

    /* Hack -- No need for stat repair */
    if (borg_skill[BI_SSTR]) num_fix_stat[A_STR] += 1000;
    if (borg_skill[BI_SINT]) num_fix_stat[A_INT] += 1000;
    if (borg_skill[BI_SWIS]) num_fix_stat[A_WIS] += 1000;
    if (borg_skill[BI_SDEX]) num_fix_stat[A_DEX] += 1000;
    if (borg_skill[BI_SCON]) num_fix_stat[A_CON] += 1000;
    if (borg_skill[BI_SCHR]) num_fix_stat[A_CHR] += 1000;
#if 0
    /* race bonuses */
    if (auto_race == RACE_ELF) num_resist_lite = 1000;
    if (auto_race == RACE_GNOME) num_free_act = 1000;
    if (auto_race == RACE_DWARF) num_resist_blind = 1000;
    if (auto_race == RACE_HALF_ORC) num_resist_dark = 1000;
    if (auto_race == RACE_HIGH_ELF) num_resist_lite = 1000;
    if (auto_race == RACE_HIGH_ELF) num_see_inv = 1000;
#endif

    /* Extract the player flags */
    player_flags(&f1, &f2, &f3);

    /* Good flags */
    if (f3 & (TR3_SLOW_DIGEST)) num_slow_digest = TRUE;
    if (f3 & (TR3_FEATHER)) num_ffall = TRUE;
    if (f3 & (TR3_LITE)) num_lite = TRUE;
    if (f3 & (TR3_REGEN)) num_regenerate = TRUE;
    if (f3 & (TR3_TELEPATHY)) num_telepathy = TRUE;
    if (f3 & (TR3_SEE_INVIS)) num_see_inv = TRUE;
    if (f3 & (TR3_FREE_ACT)) num_free_act = TRUE;
    if (f3 & (TR3_HOLD_LIFE)) num_hold_life = TRUE;

    /* Weird flags */

    /* Bad flags */

    /* Immunity flags */
    if (f2 & (TR2_IM_FIRE)) num_immune_fire = TRUE;
    if (f2 & (TR2_IM_ACID)) num_immune_acid = TRUE;
    if (f2 & (TR2_IM_COLD)) num_immune_cold = TRUE;
    if (f2 & (TR2_IM_ELEC)) num_immune_elec = TRUE;

    /* Resistance flags */
    if (f2 & (TR2_RES_ACID)) num_resist_acid = TRUE;
    if (f2 & (TR2_RES_ELEC)) num_resist_elec = TRUE;
    if (f2 & (TR2_RES_FIRE)) num_resist_fire = TRUE;
    if (f2 & (TR2_RES_COLD)) num_resist_cold = TRUE;
    if (f2 & (TR2_RES_POIS)) num_resist_pois = TRUE;
    if (f2 & (TR2_RES_LITE)) num_resist_lite = TRUE;
    if (f2 & (TR2_RES_DARK)) num_resist_dark = TRUE;
    if (f2 & (TR2_RES_BLIND)) num_resist_blind = TRUE;
    if (f2 & (TR2_RES_CONFU)) num_resist_conf = TRUE;
    if (f2 & (TR2_RES_SOUND)) num_resist_sound = TRUE;
    if (f2 & (TR2_RES_SHARD)) num_resist_shard = TRUE;
    if (f2 & (TR2_RES_NEXUS)) num_resist_nexus = TRUE;
    if (f2 & (TR2_RES_NETHR)) num_resist_neth = TRUE;
    if (f2 & (TR2_RES_CHAOS)) num_resist_chaos = TRUE;
    if (f2 & (TR2_RES_DISEN)) num_resist_disen = TRUE;

    /* Sustain flags */
    if (f2 & (TR2_SUST_STR)) num_sustain_str = TRUE;
    if (f2 & (TR2_SUST_INT)) num_sustain_int = TRUE;
    if (f2 & (TR2_SUST_WIS)) num_sustain_wis = TRUE;
    if (f2 & (TR2_SUST_DEX)) num_sustain_dex = TRUE;
    if (f2 & (TR2_SUST_CON)) num_sustain_con = TRUE;

}

/*
 * Extract the bonuses for items in the home.
 *
 * in_item is passed in if you want to pretend that in_item is
 *          the only item in the home.
 * no_items is passed in as TRUE if you want to pretend that the
 *          home is empty.
 */
void borg_notice_home(auto_item *in_item, bool no_items)
{
    /* Notice the home equipment */
    borg_notice_home_aux1(in_item, no_items);

    /* Notice the home inventory */
    borg_notice_home_aux2(in_item, no_items);
}

/*
 * Helper function -- calculate "dynamic power" of equipment
 */
static s32b borg_power_aux1(void)
{
    s32b        value = 0L;

    power_item* Power_item;
    int         iEntry, nEntrys;
    int         nItems;
    bool        Range;

    nEntrys = n_pwr[auto_class];
    borg_skill[BI_DEPTH] = auto_max_depth;
    borg_skill[BI_CDEPTH] = auto_depth;

    for (iEntry = 0; iEntry < nEntrys; iEntry++)
    {
        Power_item = &borg_power_item[auto_class][iEntry];
        if (Power_item->depth > auto_max_depth)
            continue;
        /* do not count special 'go after morgoth' stuff if morgoth is dead */
        if (Power_item->depth > 98 && borg_king)
            continue;
        if (Power_item->item == -1)
        {
            if (Power_item->cnd == -1 ||
                borg_calc_formula(formula[Power_item->cnd]))
                value += borg_calc_formula(formula[Power_item->power]);
                continue;
        }
        nItems = borg_has[Power_item->item];
        Range = (Power_item->from || Power_item->to != 999);
        if (Range && Power_item->from > nItems)
            continue;
        if (!Power_item->each)
        {
            if ((!Range && nItems) || (Range && Power_item->to >= nItems))
            {
                if (Power_item->cnd == -1 ||
                    borg_calc_formula(formula[Power_item->cnd]))
                    value += Power_item->power;
            }
            continue;
        }
        if (Range)
        {
            if (nItems > Power_item->to)
                nItems = Power_item->to;
            if (!Power_item->from)
                Power_item->from = 1;
            nItems -= (Power_item->from - 1);
        }
        if (nItems == 0)
            continue;

        if (Power_item->cnd == -1 ||
            borg_calc_formula(formula[Power_item->cnd]))
            value += nItems * Power_item->power;
     }


    /* Result */
    return (value);
}



/*
 * Helper function -- calculate non dynamic entries of dynamic calcs
 * and the borg_worship_ bonuses
 */
static s32b borg_power_aux2(void)
{
     int         k, carry_capacity, inven_weight, book;
    int         cur_wgt = 0;
    int         max_wgt = 0;
    int         i;

    auto_item       *item;

    s32b        value = 0L;

    /*** Basic abilities ***/

    /* apw
     * In here, we must subtract out the bonus granted from certain
     * Artifacts.  They grant amt_x = 1000 then the power is increased
     * by 1000 times whatever bonus.  In the case of Gondor.  This is
     * 1000 heals times 4000 points per heal.
     *
     */

    /* HACK - a small bonus for adding to stats even above max. */
    /*        This will allow us to swap a ring of int +6 for */
    /*        our ring of int +2 even though we are at max int because */
    /*        we are wielding a weapon that has +4 int */
    /*        later it might be nice to swap to a weapon that does not */
    /*        have an int bonus */
    for (i = 0; i < 6; i++) value += my_stat_add[i];

    /*** Penalize armor weight ***/
    if (my_stat_ind[A_STR] < 15)
    {
        if (auto_items[INVEN_BODY].weight > 200)
            value -= (auto_items[INVEN_BODY].weight - 200) * 15;
        if (auto_items[INVEN_HEAD].weight > 30)
            value -= 250;
        if (auto_items[INVEN_ARM].weight > 10)
            value -= 250;
        if (auto_items[INVEN_FEET].weight > 50)
            value -= 250;
    }

    /* Compute the total armor weight */
    cur_wgt += auto_items[INVEN_BODY].weight;
    cur_wgt += auto_items[INVEN_HEAD].weight;
    cur_wgt += auto_items[INVEN_ARM].weight;
    cur_wgt += auto_items[INVEN_OUTER].weight;
    cur_wgt += auto_items[INVEN_HANDS].weight;
    cur_wgt += auto_items[INVEN_FEET].weight;

    /* Determine the weight allowance */
    max_wgt = mb_ptr->spell_weight;

    /* Hack -- heavy armor hurts magic */
    if (mb_ptr->spell_book &&
        (((cur_wgt - max_wgt) / 10) > 0) &&
        ((adj_mag_mana[my_stat_ind[A_INT]] * borg_skill[BI_CLEVEL]) / 2) < 150)
    {
        /* Mega-Hack -- Penalize heavy armor which hurts mana */
        value -= (((cur_wgt - max_wgt) / 10) * 3600L);
    }


    /*** Borg_worship_ variables ***/

    /* borg_worships_damage */
    if (borg_worships_damage)
    {
        value += ((borg_skill[BI_TOHIT] + auto_items[INVEN_WIELD].to_h)*15L);

        value += auto_items[INVEN_WIELD].dd *
                 auto_items[INVEN_WIELD].ds * 20L *
                 2 * borg_skill[BI_BLOWS];
    }

    /* borg_worships_speed */
    if (borg_worships_speed)
    {
            value += (((borg_skill[BI_SPEED] - 110) * 1500L));
    }

    /* borg_worships_hp */
    if (borg_worships_hp && my_stat_ind[A_CON] <= 37)
    {
        value += (my_stat_ind[A_CON] * 250L);
    }

    /* borg_worships_mana */
    if (borg_worships_mana)
    {
        /* Priests / paladins */
        if (mb_ptr->spell_book == TV_PRAYER_BOOK &&
            my_stat_ind[A_WIS] <= 37 )
            {
                value += (my_stat_ind[A_WIS] * 250L);
            }
         /* Mages / Rogues / Rangers */
        if (mb_ptr->spell_book == TV_MAGIC_BOOK &&
            my_stat_ind[A_INT] <= 37 )
            {
                value += (my_stat_ind[A_INT] * 250L);
            }
    }

    /* borg_worships_ac */
    if (borg_worships_ac) value += ((borg_skill[BI_ARMOR]) * 2500L);

    /* Fudge factors for normalizing the AC-Reward curve */
    if (borg_skill[BI_ARMOR] >= 16 && borg_skill[BI_ARMOR] <=74) value += 28250L;
    if (borg_skill[BI_ARMOR] >= 75) value += 73750L;

    /* borg_worships_gold */
    /* does not need an entry.  It deals
     * with how the borg handles/sells items
     */

    /* extra boost for high damage weapon deep dungeon */
    if (auto_max_depth >= 75)
    {
        value += ((borg_skill[BI_TOHIT] + auto_items[INVEN_WIELD].to_h)*15L);

        value += auto_items[INVEN_WIELD].dd *
                 auto_items[INVEN_WIELD].ds * 20L *
                 2 * borg_skill[BI_BLOWS];
    }

    /*** Penalize bad magic ***/

    /* Hack -- most gloves hurt magic for spell-casters */
    if (mb_ptr->spell_book == TV_MAGIC_BOOK && auto_class == CLASS_MAGE)
    {
        item = &auto_items[INVEN_HANDS];

        /* Penalize non-usable gloves */
        if (item->iqty &&
            (!(item->flags3 & TR3_FREE_ACT)) &&
            (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
        {
            /* Hack -- Major penalty */
            value -= 275000L;
        }
    }

    /* Hack -- most gloves hurt magic for spell-casters */
    if (mb_ptr->spell_book == TV_MAGIC_BOOK && (auto_class == CLASS_RANGER ||
        auto_class == CLASS_ROGUE))
    {
        item = &auto_items[INVEN_HANDS];

        /* Penalize non-usable gloves */
        if (item->iqty &&
            (!(item->flags3 & TR3_FREE_ACT)) &&
            (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
        {
            /* Hack -- Major penalty */
            value -= 15000L;
        }
    }

    /* Hack -- most edged weapons hurt magic for priests */
    if (auto_class == CLASS_PRIEST)
    {
        item = &auto_items[INVEN_WIELD];

        /* Penalize non-blessed edged weapons */
        if (((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
            (!(item->flags3 & TR3_BLESSED)))
        {
            /* Hack -- Major penalty */
            value -= 275000L;

            /* even bigger if End Game */
            if (auto_max_depth >= 80 && !borg_king)
            value -= 100000L;
        }
    }

    /* Reward for multiple resists in a single item */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Good to have one item with multiple high resists */
        /* RNeth and RDisn */
        if (item->flags2 & TR2_RES_NETHR && item->flags2 & TR2_RES_DISEN)
            value +=150000L;
        /* RNeth RDisn RChaos */
        if (item->flags2 & TR2_RES_NETHR && item->flags2 & TR2_RES_CHAOS &&
            item->flags2 & TR2_RES_DISEN)
            value +=100000L;
    }

    /* Not if starving! This consumes more food. Take it off in town.*/
    if (item->flags3 & TR3_REGEN && do_weak && !auto_depth)
    {
        value -=20000L;
    }

    /* Special concern if Tarrasque is alive */
    if (auto_max_depth >= 75 &&
       ((!borg_skill[BI_ICOLD] && weapon_swap_immune_cold) ||
       (!borg_skill[BI_IFIRE] && weapon_swap_immune_fire)))
    {
       /* If Tarraseque is alive */
       if (auto_race_death[539] == 0)
       {
          if (!borg_skill[BI_ICOLD] && weapon_swap_immune_cold) value  += 90000L;
          if (!borg_skill[BI_IFIRE] && weapon_swap_immune_fire) value  += 90000L;
       }
    }


    /* Reward fuel */

    /* Reward food */

    /* Prefere to buy HiCalorie foods over LowCalorie */
    if (amt_food_hical <= 5) value += amt_food_hical * 50;

    /* if hungry, food is THE top priority */
    if ((do_hungry || do_weak) && borg_skill[BI_FOOD]) value += 100000;

    /* Reward Cure Poison and Cuts*/
    if ((do_cut || do_poisoned) && borg_skill[BI_ACCW]) value +=100000;
    if ((do_cut || do_poisoned) && borg_skill[BI_AHEAL]) value +=50000;
    if ((do_cut || do_poisoned) && borg_skill[BI_ACSW])
    {   /* usually takes more than one */
        k = 0;
        for (; k < 5 && k < borg_skill[BI_ACSW]; k++) value += 25000L;
    }
    if (do_poisoned && borg_skill[BI_ACUREPOIS]) value +=15000;
    if (do_poisoned && amt_slow_poison) value +=5000;

    /* Reward cure serious -- only reward serious if low on crits */
    if (borg_skill[BI_ACCW] < 10 && (borg_skill[BI_CLEVEL] < 35 || !borg_skill[BI_RCONF]))
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ACSW]; k++) value += 50L;
        for (; k < 10 && k < borg_skill[BI_ACSW]; k++) value += 5L;
    }

    /* Priests and Paladins tend to rely on Spell but should carry pots */
    if ((auto_class == CLASS_PRIEST ||
         auto_class == CLASS_PALADIN) && borg_skill[BI_CLEVEL] > 2)
    {
        k = 0;
        for (; k < 3 && k < amt_pot_heal; k++) value += 3000L;
    }

    /* Reward *Heal*, Life, and Heal pots only if we actually
     * need them right now.  The borg would prefere to carry
     * the 25 heals and 15 ez_heals as he collects more.  But
     * if he does not have ICold the pots shatter too often,
     * taking forever for him to gather more.  The goal here,
     * is to have him store all the pots in the house until he
     * is ready to take on Morgoth/Sauron.  Generally its not
     * a problem but we cannot make it depth dependant since
     * Sauron is on 99, and Morgoth on 100.
     */
    /* Collecting Potions, prepping for Morgoth fight */
    if (auto_max_depth >= 99)
    {
        /* Sauron is alive -- carry them all*/
        if (auto_race_death[546] == 0)
        {
            k = 0;
            for (; k < 99 && k < amt_pot_heal; k++) value += 8000L;
            k = 0;
            for (; k < 99 && k < borg_skill[BI_AEZHEAL]; k++) value +=10000L;
            k =0;
            for (; k < 99 && k < borg_skill[BI_ASPEED]; k++) value +=8000L;

        }
        /* Sauron is dead -- store them unless I have enough */
        if (auto_race_death[546] != 0)
        {
            /* Must know exact number of Potions  in home */
            borg_notice_home(NULL, FALSE);

            /* Must scum for more pots */
            if ((num_heal_true + amt_pot_heal +
                 num_ez_heal_true + borg_skill[BI_AEZHEAL] < 45) ||
                (num_ez_heal_true + borg_skill[BI_AEZHEAL] < 20) ||
                (num_speed + borg_skill[BI_ASPEED] < 15))
            {
                /* leave these at home so they dont shatter */
            }
            /* I have enough, carry all */
            else
            {

                k = 0;
                for (; k < 99 && k < amt_pot_heal; k++) value += 8000L;
                k = 0;
                for (; k < 99 && k < borg_skill[BI_AEZHEAL]; k++) value +=10000L;
                k =0;
                for (; k < 99 && k < borg_skill[BI_ASPEED]; k++) value +=7000L;
            }
        }
    }

    /* Reward Speed ability */
    if (auto_depth <= 98)
    {
        k =0;
        for (; k < 20 && k < borg_skill[BI_ASPEED]; k++) value += 5000L;
    }
    /* Reward cure serious -- Low Level Characters */
    if (borg_skill[BI_CLEVEL] < 15)
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ACSW]; k++) value += 250L;
        for (; k < 10 && k < borg_skill[BI_ACSW]; k++) value += 55L;
    }

    /* Hack -- Reward add stat */
    if (amt_add_stat[A_STR]) value += 50000;
    if (amt_add_stat[A_INT]) value += 20000;
    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
        if (amt_add_stat[A_INT]) value += 75000;

    if (amt_add_stat[A_WIS]) value += 20000;
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
        if (amt_add_stat[A_WIS]) value += 75000;
    if (amt_add_stat[A_DEX]) value += 50000;
    if (amt_add_stat[A_CON]) value += 50000;
    if (amt_add_stat[A_CHR]) value += 10000;

    /* Hack -- Reward fix stat */
    if (amt_fix_stat[A_STR]) value += 10000;
    if (amt_fix_stat[A_INT]) value += 10000;
    if (amt_fix_stat[A_WIS]) value += 10000;
    if (amt_fix_stat[A_DEX]) value += 10000;
    if (amt_fix_stat[A_CON]) value += 10000;
    if (amt_fix_stat[A_CHR]) value += 10000;

    /* Reward Remove Curse */
    if (borg_wearing_cursed)
    {
        if (borg_has[191]) value += 90000;
        if (borg_has[180]) value += 90000;
    }


    /* Hack -- Restore experience */
    if (amt_fix_exp)
    {
        value += 500000;
    }
    if (borg_equips_artifact(ART_LUTHIEN, INVEN_OUTER))
    {
        value -=500000;
    }

    /* Reward Teleport Level scrolls */
    if (auto_max_depth >= 99)
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ATELEPORTLVL]; k++) value += 5000L;
    }

    /*** Enchantment ***/

    /* Reward enchant armor */
    if (amt_enchant_to_a && my_need_enchant_to_a) value += 14L;

    /* Reward enchant weapon to hit */
    if (amt_enchant_to_h && my_need_enchant_to_h) value += 24L;

    /* Reward enchant weapon to damage */
    if (amt_enchant_to_d && my_need_enchant_to_d) value += 109L;

    /* Reward *enchant weapon* to damage */
    if (amt_enchant_weapon) value += 5000L;

    /* Reward *enchant armour*  */
    if (amt_enchant_armor) value += 5000L;

    /* Reward carrying a shovel if low dlevel */
    if (auto_max_depth <= 54 && auto_items[INVEN_WIELD].tval != TV_DIGGING   &&
        amt_digger == 1) value += 5000L;
    if (amt_digger > 1 || auto_items[INVEN_WIELD].tval == TV_DIGGING) value -= 100000L;



    /*** Hack -- books ***/

    /* Reward low level books */
    for (book = 0; book < 4; book++)
    {
        int what, when = 99;

        /* No copies */
        if (!amt_book[book]) continue;

        /* Scan the spells */
        for (what = 0; what < 9; what++)
        {
            auto_magic *as = &auto_magics[book][what];

            /* Track minimum level */
            if (as->level < when) when = as->level;
        }

        /* Hack -- Ignore "difficult" normal books */
        if ((when > 5) && (when >= auto_max_level + 2)) continue;

        /* Reward the book */
        k = 0;
        for (; k < 1 && k < amt_book[book]; k++) value += 500000L;
        if (auto_max_depth > 5)
            for (; k < 2 && k < amt_book[book]; k++) value += 10000L;
        if (auto_max_depth > 50)
            for (; k < 3 && k < amt_book[book]; k++) value += 2500L;
    }
    /* apw Hack -- Apply "encumbrance" from weight */
    /* Extract the current weight (in tenth pounds) */
    inven_weight = p_ptr->total_weight;

    /* Extract the "weight limit" (in tenth pounds) */
    carry_capacity = adj_str_wgt[my_stat_ind[A_STR]] * 100;

    /* XXX XXX XXX Apply "encumbrance" from weight */
    if (inven_weight > carry_capacity/2) value -= ((inven_weight - (carry_capacity/2)) / (carry_capacity / 10) *1000L);

    /* Reward empty slots (up to 5) */
    k = 1;
        for (; k < 6; k++)
            if (!auto_items[INVEN_PACK-k].iqty)
                value += 400L;

    /* Return the value */
    return (value);
}


/*
 * Helper function -- calculate "power" of equipment
 * Dynamic Calcs off
 */
static s32b borg_power_aux3(void)
{
    int         hold;
    int         damage, dam;

    int         i;

    int         cur_wgt = 0;
    int         max_wgt = 0;

    s32b        value = 0L;

    auto_item       *item;


    /* Obtain the "hold" value (weight limit for weapons) */
    hold = adj_str_hold[my_stat_ind[A_STR]];

    /*** Analyze weapon ***/

    /* Examine current weapon */
    item = &auto_items[INVEN_WIELD];

    /* Calculate "average" damage per "normal" blow  */
    /* and assume we can enchant up to +8 if borg_skill[BI_CLEVEL] > 25 */
    damage = (item->dd * item->ds * 20L);


    /* Reward "damage" and increased blows per round*/
    value += damage * (borg_skill[BI_BLOWS]+1);

    /* Reward "bonus to hit" */
    if ( item->to_h > 8 || borg_skill[BI_CLEVEL] < 25 )
        value += ((borg_skill[BI_TOHIT] + item->to_h)*30L);
    else
        value += ((borg_skill[BI_TOHIT] + 8) * 30L );

    /* Reward "bonus to dam" */
    value += ((borg_skill[BI_TODAM] + item->to_d)*30L);

    /* extra damage for some */
    if (borg_worships_damage)
    {
        value += ((borg_skill[BI_TOHIT] + item->to_h)*15L);
    }

    /* extra boost for deep dungeon */
    if (auto_max_depth >= 75)
    {
        value += ((borg_skill[BI_TOHIT] + item->to_h)*15L);

        value += item->dd *
                 item->ds * 20L *
                 2 * borg_skill[BI_BLOWS];
    }

    /* assume 2x base damage for x% of creatures */
    dam = damage * 2 * borg_skill[BI_BLOWS];
    if (borg_skill[BI_WS_ANIMAL]) value += (dam * 2) / 2;
    if (borg_skill[BI_WS_EVIL])   value += (dam * 7) / 2;

    /* extra damage for some */
    if (borg_worships_damage)
    {
        value += (dam);
    }

    /* assume 3x base damage for x% of creatures */
    dam = damage  * 3 * borg_skill[BI_BLOWS];
    if (borg_skill[BI_WS_UNDEAD]) value += (dam * 5) / 2;
    if (borg_skill[BI_WS_DEMON])  value += (dam * 3) / 2;
    if (borg_skill[BI_WS_DRAGON] && (!borg_skill[BI_WK_DRAGON])) value += (dam * 6) / 2;
    if (borg_skill[BI_WS_GIANT])  value += (dam * 4) / 2;
    if (borg_skill[BI_WB_ACID])  value += (dam * 4) / 2;
    if (borg_skill[BI_WB_ELEC])  value += (dam * 5) / 2;
    if (borg_skill[BI_WB_FIRE])  value += (dam * 3) / 2;
    if (borg_skill[BI_WB_COLD])  value += (dam * 3) / 2;
    /* SOrc and STroll get 1/2 of reward now */
    if (borg_skill[BI_WS_ORC])    value += (dam * 1) / 2;
    if (borg_skill[BI_WS_TROLL])  value += (dam * 2) / 2;
    /* and the other 2/2 if SEvil not possesed */
    if (borg_skill[BI_WS_ORC] && !borg_skill[BI_WS_EVIL])    value += (dam * 1) / 2;
    if (borg_skill[BI_WS_TROLL] && !borg_skill[BI_WS_EVIL])  value += (dam * 1) / 2;

    /* extra damage for some */
    if (borg_worships_damage)
    {
        value += (dam);
    }

    /* assume 5x base damage for x% of creatures */
    dam = damage  * 5 * borg_skill[BI_BLOWS];
    if (borg_skill[BI_WK_DRAGON]) value += (dam * 5) / 2;
    /* extra damage for some */
    if (borg_worships_damage)
    {
        value += (dam);
    }

    /* It is only on Grond */
    if (borg_skill[BI_W_IMPACT]) value += 5000L;

    /* Hack -- It is hard to hold a heavy weapon */
    if (hold < item->weight / 10) value -= 500000L;

    /* HACK -- Borg worships num_blow, even on broken swords. */
    /* kind 47 is a broken sword usually 1d2 in damage */
    if (item->kind == 47 || item->kind == 30 ||item->kind == 390 ) value -=5000L;

    /*** Analyze bow ***/

    /* Examine current bow */
    item = &auto_items[INVEN_BOW];

    /* Calculate "average" damage per "normal" shot (times 2) */
    if ( item->to_d > 8 || borg_skill[BI_CLEVEL] < 25 )
        damage = ((my_ammo_sides) + (item->to_d)) * my_ammo_power;
    else
        damage = (my_ammo_sides + 8) * my_ammo_power;

    /* Reward "damage" */
    if (borg_worships_damage)
    {
        value += (borg_skill[BI_SHOTS] * damage * 11L);
    }
    else
    {
        value += (borg_skill[BI_SHOTS] * damage * 9L);
    }

    /* AJG - slings force you to carry heavy ammo.  Penalty for that unles you have lots of str  */
    if (item->sval == SV_SLING &&
        !item->name1 &&
        my_stat_ind[A_STR] < 14)
    {
        value -= 5000L;
    }


    /* Reward "bonus to hit" */
    if ( item->to_h > 8 || borg_skill[BI_CLEVEL] < 25 )
        value += ((borg_skill[BI_TOHIT] + item->to_h) * 7L);
    else
        value += ((borg_skill[BI_TOHIT] + 8) * 7L );

    /* extra damage for some */
    if (borg_worships_damage)
    {
        value += ((borg_skill[BI_TOHIT] + item->to_h) * 2L);
    }


    /* Prefer bows */
    if (auto_class == CLASS_RANGER && my_ammo_tval == TV_ARROW) value += 30000L;

    /* Hack -- It is hard to hold a heavy weapon */
    if (hold < item->weight / 10) value -= 500000L;



    /*** apw Analyze dragon armour  ***/

    /* Examine current armor */
    item = &auto_items[INVEN_BODY];

    if (item->tval == TV_DRAG_ARMOR)
      {
          switch( item->sval)
          {
              case SV_DRAGON_BLACK:
              case SV_DRAGON_BLUE:
              case SV_DRAGON_WHITE:
              case SV_DRAGON_RED:
                  value += 3100;
                  break;
              case SV_DRAGON_GREEN:
                  value += 4750;
                  break;
              case SV_DRAGON_MULTIHUED:
                  value += 5250;
                  break;
              case SV_DRAGON_SHINING:
                  value += 7550;
                  break;
              case SV_DRAGON_LAW:
                  value += 8650;
                  break;
              case SV_DRAGON_BRONZE:
                  value += 9750;
                  break;
              case SV_DRAGON_GOLD:
                  value += 10850;
                  break;
              case SV_DRAGON_CHAOS:
                  value += 11950;
                  break;
              case SV_DRAGON_BALANCE:
                  value += 13050;
                  break;
              case SV_DRAGON_POWER:
                  value += 20150;
                  break;
          }
      }


    /*** Reward various things ***/

    /* Hack -- Reward light radius */
    value += (borg_skill[BI_CUR_LITE] * 1000000L);


    /* Hack -- Reward speed
     * see if speed can be a bonus if good speed; not +3.
     * reward higher for +10 than +50 speed (decreased return).
     */
    if (borg_worships_speed)
    {
        if (borg_skill[BI_SPEED] >= 150)
            value += (((borg_skill[BI_SPEED] - 120) * 1500L) + 185000L);

        if (borg_skill[BI_SPEED] >= 145 && borg_skill[BI_SPEED] <= 149)
            value += (((borg_skill[BI_SPEED] - 120) * 1500L) + 180000L);

        if (borg_skill[BI_SPEED] >= 140 && borg_skill[BI_SPEED] <= 144)
            value += (((borg_skill[BI_SPEED] - 120) * 1500L) + 175000L);

        if (borg_skill[BI_SPEED] >= 135 && borg_skill[BI_SPEED] <= 139)
            value += (((borg_skill[BI_SPEED] - 120) * 1500L) + 175000L);

        if (borg_skill[BI_SPEED] >= 130 && borg_skill[BI_SPEED] <= 134)
            value += (((borg_skill[BI_SPEED] - 120) * 1500L) + 160000L);

        if (borg_skill[BI_SPEED] >= 125 && borg_skill[BI_SPEED] <= 129)
            value += (((borg_skill[BI_SPEED] - 110) * 1500L) + 135000L);

        if (borg_skill[BI_SPEED] >= 120 && borg_skill[BI_SPEED] <= 124)
            value += (((borg_skill[BI_SPEED] - 110) * 1500L) + 110000L);

        if (borg_skill[BI_SPEED] >= 115 && borg_skill[BI_SPEED] <= 119)
            value += (((borg_skill[BI_SPEED] - 110) * 1500L) +  85000L);

        if (borg_skill[BI_SPEED] >= 110 && borg_skill[BI_SPEED] <= 114)
            value += (((borg_skill[BI_SPEED] - 110) * 1500L) +  65000L);
        else
            value += (((borg_skill[BI_SPEED] -110) *2500L));
    }
    else
    {
        if (borg_skill[BI_SPEED] >= 150)
            value += (((borg_skill[BI_SPEED] - 120) * 1000L) + 185000L);

        if (borg_skill[BI_SPEED] >= 145 && borg_skill[BI_SPEED] <= 149)
            value += (((borg_skill[BI_SPEED] - 120) * 1000L) + 180000L);

        if (borg_skill[BI_SPEED] >= 140 && borg_skill[BI_SPEED] <= 144)
            value += (((borg_skill[BI_SPEED] - 120) * 1000L) + 175000L);

        if (borg_skill[BI_SPEED] >= 135 && borg_skill[BI_SPEED] <= 139)
            value += (((borg_skill[BI_SPEED] - 120) * 1000L) + 165000L);

        if (borg_skill[BI_SPEED] >= 130 && borg_skill[BI_SPEED] <= 134)
            value += (((borg_skill[BI_SPEED] - 120) * 1000L) + 150000L);

        if (borg_skill[BI_SPEED] >= 125 && borg_skill[BI_SPEED] <= 129)
            value += (((borg_skill[BI_SPEED] - 110) * 1000L) + 125000L);

        if (borg_skill[BI_SPEED] >= 120 && borg_skill[BI_SPEED] <= 124)
            value += (((borg_skill[BI_SPEED] - 110) * 1000L) + 100000L);

        if (borg_skill[BI_SPEED] >= 115 && borg_skill[BI_SPEED] <= 119)
            value += (((borg_skill[BI_SPEED] - 110) * 1000L) +  75000L);

        if (borg_skill[BI_SPEED] >= 110 && borg_skill[BI_SPEED] <= 114)
            value += (((borg_skill[BI_SPEED] - 110) * 1000L) +  55000L);
        else
            value += (((borg_skill[BI_SPEED] -110) *2500L));
    }


    /* Hack -- Reward strength bonus */
    value += (my_stat_ind[A_STR] * 100L);

    /* Hack -- Reward intelligence bonus */
    if ((mb_ptr->spell_book == TV_MAGIC_BOOK) &&
        (my_stat_ind[A_INT] <= 37 ))
    {
        value += (my_stat_ind[A_INT] * 500L);

        /* Bonus for sp. */
        if (borg_worships_mana)
        {
            value += ((adj_mag_mana[my_stat_ind[A_INT]] * borg_skill[BI_CLEVEL]) / 2)  * 255L;
        }
        else
        {
            value += ((adj_mag_mana[my_stat_ind[A_INT]] * borg_skill[BI_CLEVEL]) / 2)  * 155L;
        }

        /* bonus for fail rate */
        value += adj_mag_stat[my_stat_ind[A_INT]] * 5010L;

        /* mage should try to get min fail to 0 */
        if (auto_class == CLASS_MAGE)
        {
            /* Bonus for mages to in order to keep GOI fail rate down */
            if (borg_spell_legal(7,4)) value += my_stat_ind[A_INT] * 35000L;

            /* other fail rates */
            if (adj_mag_fail[my_stat_ind[A_INT]] < 1)
                value += 90000L;

        }
    }

    /* Hack -- Reward wisdom bonus */
    if ((mb_ptr->spell_book == TV_PRAYER_BOOK) &&
        (my_stat_ind[A_WIS] <= 37 ))
    {
        value += (my_stat_ind[A_WIS] * 200L);

        /* Bonus for sp. */
        value += ((adj_mag_mana[my_stat_ind[A_WIS]] * borg_skill[BI_CLEVEL]) / 2)  * 150L;

        /* bonus for fail rate */
        value += adj_mag_stat[my_stat_ind[A_WIS]] * 3000L;

        /* priest should try to get min fail to 0 */
        if (auto_class == CLASS_PRIEST)
        {
            /* Bonus for priests to in order to keep Holy Word fail rate down */
            if (borg_prayer_legal(3, 5)) value += my_stat_ind[A_WIS] * 35000L;

            if (adj_mag_fail[my_stat_ind[A_WIS]] < 1)
                value += 70000L;
        }

    }


    /* Dexterity Bonus --good for attacking and ac*/
    if (my_stat_ind[A_DEX] <= 37 )
    {
        /* Hack -- Reward bonus */
        value += (my_stat_ind[A_DEX] * 120L);
    }

    /* Constitution Bonus */
    if (my_stat_ind[A_CON] <= 37 )
    {
        int bonus_hp = (((adj_con_mhp[my_stat_ind[A_CON]] -128) * auto_max_level) / 2);

        if (borg_worships_hp)
        {
            value += (my_stat_ind[A_CON] * 250L);
            /* Hack -- Reward hp bonus */
            /*         This is a bit wierd because we are not really giving a bonus for */
            /*         what hp you have, but for the 'bonus' hp you get */
            /*         getting over 800hp is very important. */
            if (bonus_hp < 800)
                value += bonus_hp * 450L;
            else
                value += (bonus_hp-800) * 100L + (350L * 500);
        }
        else /*does not worship hp */
        {
            value += (my_stat_ind[A_CON] * 150L);
            /* Hack -- Reward hp bonus */
            /*         This is a bit wierd because we are not really giving a bonus for */
            /*         what hp you have, but for the 'bonus' hp you get */
            /*         getting over 500hp is very important. */
            if (bonus_hp < 500)
                value += bonus_hp * 350L;
            else
                value += (bonus_hp-500) * 100L + (350L * 500);
        }

    }


    /* Hack -- Reward charisma bonus up to level 25 */
    if (borg_skill[BI_CLEVEL] < 25)
        value += (my_stat_ind[A_CHR] * 2L);



    /* HACK - a small bonus for adding to stats even above max. */
    /*        This will allow us to swap a ring of int +6 for */
    /*        our ring of int +2 even though we are at max int because */
    /*        we are wielding a weapon that has +4 int */
    /*        later it might be nice to swap to a weapon that does not */
    /*        have an int bonus */
    for (i = 0; i < 6; i++) value += my_stat_add[i];


    /*** Reward current skills ***/

    /* Hack -- tiny rewards */
    value += (borg_skill[BI_DIS] * 2L);
    value += (borg_skill[BI_DEV] * 25L);
    value += (borg_skill[BI_SAV] * 25L);
    /* perfect saves are very nice */
    if (borg_skill[BI_SAV] > 99)
        value += 10000;
    value += (borg_skill[BI_STL] * 2L);
    value += (borg_skill[BI_SRCH] * 1L);
    value += (borg_skill[BI_SRCHFREQ] * 1L);
    value += (borg_skill[BI_THN] * 5L);
    value += (borg_skill[BI_THB] * 35L);
    value += (borg_skill[BI_THT] * 2L);
    value += (borg_skill[BI_DIG] * 2L);


    /*** Reward current flags ***/

    /* Various flags */
    if (borg_skill[BI_SDIG]) value  += 10L;

    /* Feather Fall if low level is nice */
    if (auto_max_depth < 20)
    {
        if (borg_skill[BI_FEATH]) value    += 500L;
    }
    else
    {
        if (borg_skill[BI_FEATH]) value     +=50;
    }
    if (borg_skill[BI_LITE]) value         += 2000L;
    if (borg_skill[BI_ESP])
    {
        if (borg_skill[BI_SINV]) value      += 500L;
    }
    else
        if (borg_skill[BI_SINV]) value      += 5000L;

    if (borg_skill[BI_FRACT]) value     += 10000L;

    /* after you max out you are pretty safe from drainers.*/
    if (auto_max_level < 50)
    {
        if (borg_skill[BI_HLIFE]) value    += 2000L;
    }
    else
    {
        if (borg_skill[BI_HLIFE]) value    += 200L;
    }
    if (borg_skill[BI_REG]) value   += 2000L;
    if (borg_skill[BI_ESP]) value    += 80000L;

    /* Immunity flags */
    if (borg_skill[BI_ICOLD]) value  += 25000L;
    if (borg_skill[BI_IELEC]) value  += 40000L;
    if (borg_skill[BI_IFIRE]) value  += 60000L;
    if (borg_skill[BI_IACID]) value  += 80000L;

    /* Resistance flags */
    if (borg_skill[BI_RCOLD]) value  += 3000L;
    if (borg_skill[BI_RELEC]) value  += 4000L;
    if (borg_skill[BI_RACID]) value  += 6000L;
    if (borg_skill[BI_RFIRE]) value  += 8000L;
    /* extra bonus for getting all basic resist */
    if (borg_skill[BI_RFIRE] &&
        borg_skill[BI_RACID] &&
        borg_skill[BI_RELEC] &&
        borg_skill[BI_RCOLD]) value +=  10000L;
    if (borg_skill[BI_RPOIS]) value  += 20000L;
    if (borg_skill[BI_RSND]) value += 3500L;
    if (borg_skill[BI_RLITE]) value  += 800L;
    if (borg_skill[BI_RDARK]) value  += 800L;
    if (borg_skill[BI_RKAOS]) value += 5000L;

    /* this is way boosted to avoid carrying stuff you don't need */
    if (borg_skill[BI_RCONF]) value  += 80000L;

    /* mages need a slight boost for this */
    if (auto_class == CLASS_MAGE && borg_skill[BI_RCONF]) value +=2000L;
    if (borg_skill[BI_RDIS]) value += 5000L;
    if (borg_skill[BI_RSHRD]) value += 100L;
    if (borg_skill[BI_RNXUS]) value += 100L;
    if (borg_skill[BI_RBLIND]) value += 5000L;
    if (borg_skill[BI_RNTHR]) value  += 5500L;
    if (borg_skill[BI_RFEAR]) value  += 2000L;

    /* Sustain flags */
    if (borg_skill[BI_SSTR]) value += 50L;
    if (borg_skill[BI_SINT]) value += 50L;
    if (borg_skill[BI_SWIS]) value += 50L;
    if (borg_skill[BI_SCON]) value += 50L;
    if (borg_skill[BI_SDEX]) value += 50L;
    /* boost for getting them all */
    if (borg_skill[BI_SSTR] &&
        borg_skill[BI_SINT] &&
        borg_skill[BI_SWIS] &&
        borg_skill[BI_SDEX] &&
        borg_skill[BI_SCON])  value += 1000L;


    /*** XXX XXX XXX Reward "necessary" flags ***/

    /* Mega-Hack -- See invisible (level 10) */
    if ((borg_skill[BI_SINV] || borg_skill[BI_ESP]) && (auto_max_depth+1 >= 10)) value += 100000L;


    /* Mega-Hack -- Free action (level 20) */
    if (borg_skill[BI_FRACT] && (auto_max_depth+1 >= 20)) value += 100000L;


    /*  Mega-Hack -- resists (level 25) */
    if (borg_skill[BI_RFIRE] && (auto_max_depth+1 >= 25)) value += 100000L;


    /*  Mega-Hack -- resists (level 40) */
    if (borg_skill[BI_RPOIS] && (auto_max_depth+1 >= 40)) value += 100000L;
    if (borg_skill[BI_RELEC] && (auto_max_depth+1 >= 40)) value += 100000L;
    if (borg_skill[BI_RACID] && (auto_max_depth+1 >= 40)) value += 100000L;
    if (borg_skill[BI_RCOLD] && (auto_max_depth+1 >= 40)) value += 100000L;


    /* APW Mega-Hack -- Speed / Hold Life (level 46) and maxed out */
    if ((borg_skill[BI_HLIFE] && (auto_max_depth+1 >= 46) && (auto_max_level < 50))) value += 100000L;
    if ((borg_skill[BI_SPEED] >= 115) && (auto_max_depth+1 >=46)) value +=100000L;
    if (borg_skill[BI_RCONF] && (auto_max_depth+1 >= 46)) value += 100000L;

    /*  Mega-Hack -- resist Nether is -very- nice to have at level 50 */
    if (borg_skill[BI_RNTHR]  && (auto_max_depth+1 >= 50)) value += 55000L;

    /*  Mega-Hack -- resist Sound to avoid being KO'd */
    if (borg_skill[BI_RSND]  && (auto_max_depth+1 >= 50)) value += 100000L;

    /*  Mega-Hack -- resists & Telepathy (level 55) */
    if (borg_skill[BI_RBLIND] && (auto_max_depth+1 >= 55)) value += 100000L;
    if (borg_skill[BI_ESP] && (auto_max_depth+1 >= 55)) value += 100000L;
    if (borg_skill[BI_RNTHR]  && (auto_max_depth+1 >= 60)) value += 55000L;


    /*  Mega-Hack -- resists & +10 speed (level 60) */
    if (borg_skill[BI_RKAOS] && (auto_max_depth+1 >= 60)) value += 104000L;
    if (borg_skill[BI_RDIS] && (auto_max_depth+1 >= 60)) value += 90000L;
    if ((borg_skill[BI_SPEED] >= 120) && (auto_max_depth+1 >=60)) value +=100000L;

    /*  Must have +20 speed (level 80) */
    if ((borg_skill[BI_SPEED] >= 130) && (auto_max_depth+1 >=80)) value +=100000L;

    /* Not Req, but a good idea:
     * Extra boost to Nether deeper down
     * RDark for deeper uniques
     * Good to have +30 speed
     */
    if (borg_skill[BI_RNTHR] && (auto_max_depth+1 >= 80)) value += 15000L;
    if (borg_skill[BI_RDARK] && (auto_max_depth+1 >= 80)) value += 25000L;
    if ((borg_skill[BI_SPEED] >= 140) && (auto_max_depth+1 >=80) &&
        auto_class == CLASS_WARRIOR)                value += 100000L;


    /*** Reward powerful armor ***/

    /* Reward armor */
    if (borg_worships_ac)
    {
        if (borg_skill[BI_ARMOR] < 15) value += ((borg_skill[BI_ARMOR]) * 2500L);
        if (borg_skill[BI_ARMOR] >= 15 && borg_skill[BI_ARMOR] < 75) value += ((borg_skill[BI_ARMOR]) * 2000L) + 28250L;
        if (borg_skill[BI_ARMOR] >= 75) value += ((borg_skill[BI_ARMOR]) * 1500L) + 73750L;
    }
    else
    {
        if (borg_skill[BI_ARMOR] < 15)   value += ((borg_skill[BI_ARMOR]) * 2000L);
        if (borg_skill[BI_ARMOR] >= 15 &&
            borg_skill[BI_ARMOR] < 75)   value += ((borg_skill[BI_ARMOR]) * 1500L) + 28350L;
        if (borg_skill[BI_ARMOR] >= 75) value += ((borg_skill[BI_ARMOR]) * 500L) + 73750L;
    }

    /*** Penalize various things ***/

    /* Penalize various flags */
    if (borg_skill[BI_CRSTELE]) value -= 100000L;
    if (borg_skill[BI_CRSAGRV]) value -= 8000L;

    /*** Penalize armor weight ***/
    if (my_stat_ind[A_STR] < 15)
    {
        if (auto_items[INVEN_BODY].weight > 200)
            value -= (auto_items[INVEN_BODY].weight - 200) * 15;
        if (auto_items[INVEN_HEAD].weight > 30)
            value -= 250;
        if (auto_items[INVEN_ARM].weight > 10)
            value -= 250;
        if (auto_items[INVEN_FEET].weight > 50)
            value -= 250;
    }

    /* Compute the total armor weight */
    cur_wgt += auto_items[INVEN_BODY].weight;
    cur_wgt += auto_items[INVEN_HEAD].weight;
    cur_wgt += auto_items[INVEN_ARM].weight;
    cur_wgt += auto_items[INVEN_OUTER].weight;
    cur_wgt += auto_items[INVEN_HANDS].weight;
    cur_wgt += auto_items[INVEN_FEET].weight;

    /* Determine the weight allowance */
    max_wgt = mb_ptr->spell_weight;

    /* Hack -- heavy armor hurts magic */
    if (mb_ptr->spell_book &&
        (((cur_wgt - max_wgt) / 10) > 0) &&
        ((adj_mag_mana[my_stat_ind[A_INT]] * borg_skill[BI_CLEVEL]) / 2) < 150)
    {
        /* Mega-Hack -- Penalize heavy armor which hurts mana */
        value -= (((cur_wgt - max_wgt) / 10) * 360L);
    }


    /*** Penalize bad magic ***/

    /* Hack -- most gloves hurt magic for spell-casters */
    if (mb_ptr->spell_book == TV_MAGIC_BOOK && auto_class == CLASS_MAGE)
    {
        item = &auto_items[INVEN_HANDS];

        /* Penalize non-usable gloves */
        if (item->iqty &&
            (!(item->flags3 & TR3_FREE_ACT)) &&
            (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
        {
            /* Hack -- Major penalty */
            value -= 275000L;
        }
    }

    /* apw Hack -- most edged weapons hurt magic for priests */
    if (auto_class == CLASS_PRIEST)
    {
        item = &auto_items[INVEN_WIELD];

        /* Penalize non-blessed edged weapons */
        if (((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
            (!(item->flags3 & TR3_BLESSED)))
        {
            /* Hack -- Major penalty */
            value -= 75000L;
        }
    }


    /* HUGE MEGA MONDO HACK! prepare for the big fight */
    /* go after Morgoth new priorities. */
    if ((auto_max_depth+1 == 100 || auto_depth == 100) && (!borg_king))
    {
        /* protect from stat drain */
        if (borg_skill[BI_SSTR]) value += 35000L;
        /* extra bonus for spell casters */
        if (mb_ptr->spell_book == TV_MAGIC_BOOK && borg_skill[BI_SINT]) value += 45000L;
        /* extra bonus for spell casters */
        if (mb_ptr->spell_book == TV_PRAYER_BOOK && borg_skill[BI_SWIS]) value += 35000L;
        if (borg_skill[BI_SCON]) value += 55000L;
        if (borg_skill[BI_SDEX]) value += 15000L;
        if (borg_skill[BI_WS_EVIL])  value += 15000L;

        /* Another bonus for resist nether, poison and base four */
        if (borg_skill[BI_RNTHR]) value +=  15000L;
        if (borg_skill[BI_RDIS]) value += 15000L;

        /* to protect against summoned baddies */
        if (borg_skill[BI_RPOIS]) value +=  100000L;
        if (borg_skill[BI_RFIRE] &&
            borg_skill[BI_RACID] &&
            borg_skill[BI_RELEC] &&
            borg_skill[BI_RCOLD]) value += 100000L;
    }

    /* Reward for activatable Artifacts in inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Good to have one item with multiple high resists */
        /* RNeth and RDisn */
        if (item->flags2 & TR2_RES_NETHR && !item->flags2 & TR2_RES_CHAOS &&
            item->flags2 & TR2_RES_DISEN) value +=150000L;
        /* RNeth RDisn RChaos */
        if (item->flags2 & TR2_RES_NETHR && item->flags2 & TR2_RES_CHAOS &&
            item->flags2 & TR2_RES_DISEN) value +=250000L;

        /* This needs to be changed */
        switch (item->name1)
        {
        /* Artifact -- Narthanc- fire bolt 9d8*/
        case ART_NARTHANC:
        value +=(500+(9*(8+1)/2));
        break;

        /* Artifact -- Nimthanc- frost bolt 6d8*/
        case ART_NIMTHANC:
        value +=(500+(6*(8+1)/2));
        break;

        /* Artifact -- Dethanc- electric bolt 4d8*/
        case ART_DETHANC:
        value +=(500+(4*(8+1)/2));
        break;


        /* Artifact -- Rilia- poison gas 12*/
        case ART_RILIA:
        value +=(500+(24));
        break;

        /* Artifact -- Belangil- frost ball 48*/
        case ART_BELANGIL:
        value +=(500+(96));
        break;


        /* Artifact -- Arunruth- frost bolt 12d8*/
        case ART_ARUNRUTH:
        value +=(500+(12*(8+1)/2));
        break;


        /* Artifact -- Ringil- frost ball 100*/
        case ART_RINGIL:
        value +=(500+(200));
        /* extra boost for speed */
        if (!adult_rand_artifacts)
            value +=25000L;
        break;


        /* Artifact -- Anduril- fire ball 72*/
        case ART_ANDURIL:
        value +=(500+(144));
        break;


        /* Artifact -- Theoden- drain Life 120*/
        case ART_THEODEN:
        value +=(500+120);
        break;


        /* Artifact -- Aeglos- frost ball 100*/
        case ART_AEGLOS:
        value +=(500+200);
        break;


        /* Artifact -- Totila- confusion */
        case ART_TOTILA:
        value +=(500+(200));
        break;


        /* Artifact -- Firestar- fire ball 72 */
        case ART_FIRESTAR:
        value +=(500+(144));
        break;


        /* Artifact -- TURMIL- drain life 90 */
        case ART_TURMIL:
        value +=(500+90);
        break;


        /* Artifact -- Razorback- spikes 150 */
        case ART_RAZORBACK:
        value +=(500+(300));
        break;


        /* Artifact -- Cammithrim- Magic Missile 2d6 */
        case ART_CAMMITHRIM:
        value +=(500+(2*(6+1)/2));
        break;

        /* Artifact -- Paurhach- fire bolt 9d8 */
        case ART_PAURHACH:
        value +=(500+(9*(8+1)/2));
        break;

        /* Artifact -- Paurnimmen- frost bolt 6d8 */
        case ART_PAURNIMMEN:
        value +=(500+(6*(8+1)/2));
        break;

        /* Artifact -- Pauraegen- lightning bolt 4d8 */
        case ART_PAURAEGEN:
        value +=(500+(4*(8+1)/2));
        break;

        /* Artifact -- PaurNEN- ACID bolt 5d8 */
        case ART_PAURNEN:
        value +=(500+(5*(8+1)/2));
        break;

        /* Artifact -- FINGOLFIN- MISSILE 150 (bonus for TH TD)*/
        case ART_FINGOLFIN:
        value +=(500+(150));
        if (!adult_rand_artifacts)
            value +=5000;
        break;

        /* Artifact -- INGWE- DISPEL EVIL X5 */
        case ART_INGWE:
        value +=(500+(10 + (borg_skill[BI_CLEVEL]*5)/2));
        break;

        /* Artifact -- NARYA- FIRE BALL 120 */
        case ART_NARYA:
        value +=(500+(240));
        break;

        /* Artifact -- NENYA- COLD BALL 200 */
        case ART_NENYA:
        value +=(500+(400));
        break;

        /* Artifact -- VILYA- ELEC BALL 250 */
        case ART_VILYA:
        value +=(500+(500));
        break;

        /* Artifact -- POWER One Ring-*/
        case ART_POWER:
        value +=(999999);
        break;

        /* Artifact -- Ulmo- tele way */
        case ART_ULMO:
        if (auto_class == CLASS_MAGE)
        {
            value +=500;
        }
        else
            value +=(500+(500));
        break;

        /* Artifact -- Colluin - bladturner Resistance */
        case ART_COLLUIN: case ART_BLADETURNER:
        value +=(500+(150));
        /* extra bonus for the non spell guys */
        if (auto_class == CLASS_WARRIOR || auto_class == CLASS_ROGUE ||
            auto_class == CLASS_PALADIN) value +=25000;
        break;

        /* Artifact -- Holcolleth -- Sleep II */
        case ART_HOLCOLLETH:
        if ((auto_class == CLASS_MAGE) || (auto_class == CLASS_PRIEST) )
        {
            value +=500;
        }
        else
            value +=(500+(200));
        break;

        /* Artifact -- Thingol recharge */
        case ART_THINGOL:
        if (auto_class == CLASS_MAGE)
        {
            value +=500;
        }
        else
            value +=(500+(100));
        break;

        /* Artifact -- Holehenth detection */

        /* Artifact -- Dal fear and poison */
        case ART_DAL:
        if (auto_class == CLASS_MAGE || auto_class == CLASS_PRIEST)
        {
            value +=500;
        }
        else
            value +=(500+(200));
        break;

        /* Artifact -- Carlammas PFE*/

        /* Artifact -- Lotharang- cure light */

        /* Artifact -- Eriril id */

        /* Artifact -- Cubragol brand bolts, bonus for speed */
        case ART_CUBRAGOL:
        value +=(500+(300));
        /* extra boost for speed */
        if (!adult_rand_artifacts)
            value +=25000L;
        break;

        /* Artifact -- Avavir WoR */

        /* Artifact -- Taratol, feanor, tulkas speed */

        /* Artifact -- Soulkeeper, Gondor heal */

        /* Artifact -- Belegonnon   phase */

        /* Artifact -- Colannon teleport */

        /* Artifact -- Luthien RLL */

        /* Artifact -- Celegorm */
        case ART_CELEGORM:
        value +=(500);
        break;

        /* Artifact -- Phial */
        case ART_GALADRIEL:
        if (auto_class == CLASS_WARRIOR) value +=1500;
        value +=(500);
        break;

        /* Artifact -- Star */
        case ART_ELENDIL:
        value +=(1200);
        break;

        /* Artifact -- Arkstone */
        case ART_THRAIN:
        value +=(2000);
        break;


        }

    }


    /* Result */
    return (value);
}



/*
 * Helper function -- calculate power of inventory
 * Dynamic Calcs off
 */
static s32b borg_power_aux4(void)
{
    int         k, carry_capacity, inven_weight, book;

    s32b        value = 0L;


    /*** Basic abilities ***/

    /* apw
     * In here, we must subtract out the bonus granted from certain
     * Artifacts.  They grant amt_x = 1000 then the power is increased
     * by 1000 times whatever bonus.  In the case of Gondor.  This is
     * 1000 heals times 4000 points per heal.
     *
     */

    /* Reward fuel */
    k = 0;
    for (; k < 5 && k < borg_skill[BI_AFUEL]; k++) value += 60000L;
    for (; k < 10 && k < borg_skill[BI_AFUEL]; k++) value += 6000L;

    /* Reward Food */
    /* if hungry, food is THE top priority */
    if ((do_hungry || do_weak) && borg_skill[BI_FOOD]) value += 100000;
    k = 0;
    for (; k < 5 && k < borg_skill[BI_FOOD]; k++) value += 50000L;
    for (; k < 10 && k < borg_skill[BI_FOOD]; k++) value += 200L;
    if (borg_skill[BI_REG] && !borg_skill[BI_SDIG])
    {
        k=0;
        for (; k < 10 && k < borg_skill[BI_FOOD]; k++) value += 500L;
    }
    /* Prefere to buy HiCalorie foods over LowCalorie */
    if (amt_food_hical <= 5) value += amt_food_hical * 50;

    /* Reward Cure Poison and Cuts*/
    if ((do_cut || do_poisoned) && borg_skill[BI_ACCW]) value +=100000;
    if ((do_cut || do_poisoned) && borg_skill[BI_AHEAL]) value +=50000;
    if ((do_cut || do_poisoned) && borg_skill[BI_ACSW])
    {   /* usually takes more than one */
        k = 0;
        for (; k < 5 && k < borg_skill[BI_ACSW]; k++) value += 25000L;
    }
    if (do_poisoned && borg_skill[BI_ACUREPOIS]) value +=15000;
    if (do_poisoned && amt_slow_poison) value +=5000;

    /* Reward Resistance Potions for Warriors */
    if (auto_class == CLASS_WARRIOR)
    {
        k = 0;
        for (; k <  8 && k < borg_skill[BI_ARESHEAT]; k++) value += 500L;
        k = 0;
        for (; k <  8 && k < borg_skill[BI_ARESCOLD]; k++) value += 500L;
    }

    /* Reward ident */
    k = 0;
    for (; k < 10 && k < borg_skill[BI_AID]; k++) value += 6000L;
    for (; k < 15 && k < borg_skill[BI_AID]; k++) value += 600L;

    /*  Reward *id* apw carry lots of these*/
    k = 0;
    for (; k < 8 && k < borg_has[177]; k++) value += 10000L;
    for (; k < 15 && k < borg_has[177]; k++) value += 2000L;

    /*  Reward PFE  carry lots of these*/
    k = 0;
    for (; k < 10 && k < borg_skill[BI_APFE]; k++) value += 10000L;
    for (; k < 25 && k < borg_skill[BI_APFE]; k++) value += 2000L;

    /*  apw Reward Glyph- Rune of Protection-  carry lots of these*/
    k = 0;
    for (; k < 10 && k < borg_skill[BI_AGLYPH]; k++) value += 10000L;
    for (; k < 25 && k < borg_skill[BI_AGLYPH]; k++) value += 2000L;

    /* Reward recall */
    k = 0;
    for (; k < 3 && k < borg_skill[BI_RECALL]; k++) value += 50000L;
    for (; k < 7 && k < borg_skill[BI_RECALL]; k++) value += 5000L;

    /* Reward phase */
    k = 1;
    /* first phase door is very important */
    if (amt_phase) value += 50000;
    for (; k < 15 && k < amt_phase; k++) value += 500L;

    /* Reward escape */
    k = 0;
    for (; k < 5 && k < borg_skill[BI_AESCAPE]; k++) value += 10000L;
    if (auto_depth > 90)
    {
        for (; k < 15 && k < borg_skill[BI_AESCAPE]; k++) value += 10000L;
    }

    /* Reward teleport */
    k = 0;
    for (; k < 10 && k < borg_skill[BI_ATELEPORT]; k++) value += 10000L;

    /* Reward Teleport Level scrolls */
    if (auto_max_depth >= 99)
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ATELEPORTLVL]; k++) value += 5000L;
    }


    /*** Healing ***/
    if (auto_class == CLASS_WARRIOR || auto_class == CLASS_ROGUE)
    {
        k = 0;
        for (; k < 15 && k < borg_skill[BI_AHEAL]; k++) value += 8000L;

        k = 0; /* carry a couple for emergency. Store the rest. */
        for (; k < 2 && k < amt_ez_heal_star; k++) value +=10000L;
        if (amt_ez_heal_star == 0)
        {
            k = 0; /* carry a couple for emergency. Store the rest. */
            for (; k < 2 && k < amt_ez_heal_life; k++) value +=10000L;
        }

        /* These guys need to carry the rods more, town runs low on supply. */
        k = 0;
        for (; k < 4 && k < amt_rod_heal; k++) value +=20000L;
    }
    else
    if (auto_class == CLASS_RANGER || auto_class == CLASS_PALADIN ||
        auto_class == CLASS_MAGE)
    {
        k = 0;
        for (; k < 10 && k < borg_skill[BI_AHEAL]; k++) value += 4000L;

        k = 0; /* carry a couple for emergency. Store the rest. */
        for (; k < 2 && k < amt_ez_heal_star; k++) value +=9000L;
        if (amt_ez_heal_star == 0)
        {
            k = 0; /* carry a couple for emergency. Store the rest. */
            for (; k < 2 && k < amt_ez_heal_life; k++) value +=9000L;
        }

        if (auto_class == CLASS_PALADIN)
        {
            /* Reward heal potions */
            k = 0;
            for (; k < 3 && k < amt_pot_heal; k++) value += 5000L;
        }

    }
    else if (auto_class == CLASS_PRIEST)
    {
        /* Level 1 priests are given a Potion of Healing.  It is better
         * for them to sell that potion and buy equipment or several
         * Cure Crits with it.
         */
        if (borg_skill[BI_CLEVEL] == 1)
        {
            k = 0;
            for (; k < 10 && k < amt_pot_heal; k++) value -= 2000L;
        }
        /* Reward heal potions */
        k = 0;
        for (; k < 5 && k < amt_pot_heal; k++) value += 2000L;

        k = 0; /* carry a couple for emergency. Store the rest. */
        for (; k < 2 && k < amt_ez_heal_star; k++) value +=9000L;
        if (amt_ez_heal_star == 0)
        {
            k = 0; /* carry a couple for emergency. Store the rest. */
            for (; k < 2 && k < amt_ez_heal_life; k++) value +=9000L;
        }
    }


    /* Collecting Potions, prepping for Morgoth/Sauron fight */
    if (auto_max_depth >= 99)
    {
        /* Sauron is alive -- carry them all*/
        if (auto_race_death[546] == 0)
        {
            k = 0;
            for (; k < 99 && k < amt_pot_heal; k++) value += 8000L;
            k = 0;
            for (; k < 99 && k < borg_skill[BI_AEZHEAL]; k++) value +=10000L;
            k = 0;
            for (; k < 99 && k < borg_skill[BI_ASPEED]; k++) value += 8000L;
        }
        /* Sauron is dead -- store them unless I have enough */
        if (auto_race_death[546] != 0)
        {
            /* Must scum for more pots */
            /* Must scum for more pots */
            if ((num_heal_true + amt_pot_heal + num_ez_heal_true + borg_skill[BI_AEZHEAL] < 45) ||
                (num_ez_heal_true + borg_skill[BI_AEZHEAL] < 20) ||
                (num_speed + borg_skill[BI_ASPEED] < 15))
            {
                /* leave pots at home so they dont shatter */
            }
            /* I have enough, carry all pots */
            else
            {
                k = 0;
                for (; k < 99 && k < amt_pot_heal; k++) value += 8000L;
                k = 0;
                for (; k < 99 && k < borg_skill[BI_AEZHEAL]; k++) value +=10000L;
                k = 0;
                for (; k < 99 && k < borg_skill[BI_ASPEED]; k++) value +=8000L;
            }
        }
    }

    /* Restore Mana */
    if (auto_msp > 100)
    {
        k = 0;
        for (; k < 10 && k < amt_mana; k++) value += 4000L;
        k = 0;
        for (; k < 100 && k < borg_skill[BI_ASTFMAGI]; k++) value += 4000L;

        /* If I'm going after morgy, carry real potions */
        if (auto_max_depth >= 99 && (!borg_king))
        {
            k = 0;
            for (; k < 99 && k < amt_mana; k++) value += 5000L;
        }

    }

    /* Reward cure critical.  Heavy reward on first 5 */
    if (borg_skill[BI_CLEVEL] < 35  || !borg_skill[BI_RCONF])
    {
        k = 0;
        for (; k < 10 && k < borg_skill[BI_ACCW]; k++) value += 5000L;
        for (; k < 15 && k < borg_skill[BI_ACCW]; k++) value += 500L;
    }
    else
    {
        /* Reward cure critical.  Later on in game. */
        k = 0;
        for (; k <  10 && k < borg_skill[BI_ACCW]; k++) value += 5000L;
    }

    /* Reward cure serious -- only reward serious if low on crits */
    if (borg_skill[BI_ACCW] < 10 && (borg_skill[BI_CLEVEL] < 35 || !borg_skill[BI_RCONF]))
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ACSW]; k++) value += 50L;
        for (; k < 10 && k < borg_skill[BI_ACSW]; k++) value += 5L;
    }

    /* Reward cure serious -- Low Level Characters */
    if (borg_skill[BI_CLEVEL] < 15)
    {
        k = 0;
        for (; k <  5 && k < borg_skill[BI_ACSW]; k++) value += 250L;
        for (; k < 10 && k < borg_skill[BI_ACSW]; k++) value += 55L;
    }

    /* Reward Cures */
    if (!borg_skill[BI_RCONF])
    {
        k = 0;
        for (; k < 10 && k < amt_cure_confusion; k++) value += 400L;
    }
    if (!borg_skill[BI_RBLIND])
    {
        k = 0;
        for (; k < 5 && k < amt_cure_blind; k++) value += 300L;
    }
    if (!borg_skill[BI_RPOIS])
    {
        k = 0;
        for (; k < 5 && k < borg_skill[BI_ACUREPOIS]; k++) value += 250L;
    }
    /*** Detection ***/

    /* Reward detect trap */
    k = 0;
    for (; k < 1 && k < borg_skill[BI_ADETTRAP]; k++) value += 4000L;

    /* Reward detect door */
    k = 0;
    for (; k < 1 && k < borg_skill[BI_ADETDOOR]; k++) value += 2000L;

    /* Reward detect evil */
    if (!borg_skill[BI_ESP])
    {
        k = 0;
        for (; k < 1 && k < borg_skill[BI_ADETEVIL]; k++) value += 1000L;
    }

    /* Reward magic mapping */
    k = 0;
    for (; k < 1 && k < borg_skill[BI_AMAGICMAP]; k++) value += 4000L;

    /* Genocide scrolls. Just scrolls, mainly used for Morgoth */
    if (auto_max_depth >= 98)
    {
        k = 0;
        for (; k < 10 && k < borg_has[207]; k++) value += 10000L;
        for (; k < 25 && k < borg_has[207]; k++) value += 2000L;
    }

    /* Mass Genocide scrolls. Just scrolls, mainly used for Morgoth */
    if (auto_max_depth >= 98)
    {
        k = 0;
        for (; k < 10 && k < borg_has[200]; k++) value += 10000L;
        for (; k < 25 && k < borg_has[200]; k++) value += 2000L;
    }

    /* Reward speed potions/staves */
    if (auto_max_depth <= 98)
    {
        k = 0;
        for (; k < 20 && k < borg_skill[BI_ASPEED]; k++) value += 5000L;
    }

    /* Reward Recharge ability */
    k = 0;
    for (; k < 5 && k < borg_skill[BI_ARECHARGE]; k++) value += 2000L;

    /*** Missiles ***/

    /* Reward missiles */
    if (auto_class == CLASS_RANGER)
    {
        k = 0;
        for (; k < 30 && k < borg_skill[BI_AMISSILES]; k++) value += 1000L;
        for (; k < 80 && k < borg_skill[BI_AMISSILES]; k++) value += 100L;
    }
    else
    {
        k = 0;
        for (; k < 20 && k < borg_skill[BI_AMISSILES]; k++) value += 1000L;
        for (; k < 50 && k < borg_skill[BI_AMISSILES]; k++) value += 100L;
    }

    /*** Various ***/

    /* These staves are great but do not clutter inven with them */
    /*  -- Reward carrying a staff of holiness/power */
    if (amt_cool_staff) value += 2500L;
    k=0;
    for (; k < 3 && k < amt_cool_staff; k++) value += 500L;

    /*  -- Reward carrying a staff of destruction. */
    if (borg_has[307]) value += 5000L;
    k=0;
    for (; k < 3 && k < borg_has[307]; k++) value += 200L;

    /* Hack -- Reward add stat */
    if (amt_add_stat[A_STR]) value += 50000;
    if (amt_add_stat[A_INT]) value += 20000;
    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
        if (amt_add_stat[A_INT]) value += 50000;

    if (amt_add_stat[A_WIS]) value += 20000;
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
        if (amt_add_stat[A_WIS]) value += 50000;
    if (amt_add_stat[A_DEX]) value += 50000;
    if (amt_add_stat[A_CON]) value += 50000;
    if (amt_add_stat[A_CHR]) value += 10000;

    /* Hack -- Reward fix stat */
    if (amt_fix_stat[A_STR]) value += 10000;
    if (amt_fix_stat[A_INT]) value += 10000;
    if (amt_fix_stat[A_WIS]) value += 10000;
    if (amt_fix_stat[A_DEX]) value += 10000;
    if (amt_fix_stat[A_CON]) value += 10000;
    if (amt_fix_stat[A_CHR]) value += 10000;

    /* Reward Remove Curse */
    if (borg_wearing_cursed)
    {
        if (borg_has[191]) value += 90000;
        if (borg_has[180]) value += 90000;
    }

    /* Going after Morgoth Carry shrooms */
    if (auto_max_depth >= 99 && (!borg_king))
    {
        k = 0;
        for (; k < 35 && k < amt_fix_stat[6]; k++) value += 5000L;
    }

    /* Hack -- Restore experience */
    if (amt_fix_exp) value += 500000;
    if (borg_equips_artifact(ART_LUTHIEN, INVEN_OUTER))
    {
        value -=500000;
    }

    /*** Enchantment ***/

    /* Reward enchant armor */
    if (amt_enchant_to_a && my_need_enchant_to_a) value += 14L;

    /* Reward enchant weapon to hit */
    if (amt_enchant_to_h && my_need_enchant_to_h) value += 24L;

    /* Reward enchant weapon to damage */
    if (amt_enchant_to_d && my_need_enchant_to_d) value += 109L;

    /* Reward *enchant weapon* to damage */
    if (amt_enchant_weapon) value += 5000L;

    /* Reward *enchant armour*  */
    if (amt_enchant_armor) value += 5000L;

    /* Reward carrying a shovel if low level */
    if (auto_max_depth <= 54 && auto_items[INVEN_WIELD].tval != TV_DIGGING   &&
        amt_digger == 1) value += 5000L;
    if (amt_digger > 1 || auto_items[INVEN_WIELD].tval == TV_DIGGING) value -= 100000L;

    /*** Hack -- books ***/

    /* Reward books */
    for (book = 0; book < 9; book++)
    {
        /* No copies */
        if (!amt_book[book]) continue;

        /* The "hard" books */
        if (book >= 4)
        {
            /* Reward the book */
            k = 0;
            for (; k < 1 && k < amt_book[book]; k++) value += 300000L;
        }

        /* The "easy" books */
        else
        {
            int what, when = 99;

            /* Scan the spells */
            for (what = 0; what < 9; what++)
            {
                auto_magic *as = &auto_magics[book][what];

                /* Track minimum level */
                if (as->level < when) when = as->level;
            }

            /* Hack -- Ignore "difficult" normal books */
            if ((when > 5) && (when >= auto_max_level + 2)) continue;

            /* Reward the book */
            k = 0;
            for (; k < 1 && k < amt_book[book]; k++) value += 500000L;
            if (auto_max_depth > 5)
                for (; k < 2 && k < amt_book[book]; k++) value += 10000L;
            if (auto_max_depth > 50)
                for (; k < 3 && k < amt_book[book]; k++) value += 2500L;
        }
    }

    /* apw Hack -- Apply "encumbrance" from weight */
    /* Extract the current weight (in tenth pounds) */
    inven_weight = p_ptr->total_weight;

    /* Extract the "weight limit" (in tenth pounds) */
    carry_capacity = adj_str_wgt[my_stat_ind[A_STR]] * 100;

    /* XXX XXX XXX Apply "encumbrance" from weight */
    if (inven_weight > carry_capacity/2) value -= ((inven_weight - (carry_capacity/2)) / (carry_capacity / 10) *1000L);

    /* Reward empty slots (up to 5) */
    k = 1;
        for (; k < 6; k++)
            if (!auto_items[INVEN_PACK-k].iqty)
                value += 4000L;

    /* Return the value */
    return (value);
}


/*
 * Calculate the "power" of the Borg
 */
s32b borg_power(void)
{
    int i = 1;
    s32b value = 0L;

    if (borg_uses_calcs)
    {
        /* Process the equipment */
        value += borg_power_aux1();

        /* Process the inventory */
        value += borg_power_aux2();
    }
    else
    {
        /* Process the equipment */
        value += borg_power_aux3();

        /* Process the inventory */
        value += borg_power_aux4();
    }
    /* Add a bonus for deep level prep */
    /* Dump prep codes */
    for (i = 1; i <= auto_max_depth+10; i++)
    {
        /* Dump fear code*/
        if ((cptr)NULL != borg_prepared(i)) break;
    }
    value +=((i-1) * 20000L);

    /* Add the value for the swap items */
    value += weapon_swap_value;
    value += armour_swap_value;


    /* Return the value */
    return (value);
}

/*
 * Helper function -- calculate power of equipment in the home
 */
static s32b borg_power_home_aux1(void)
{
    s32b        value = 0L;

    /* This would be better seperated by item type (so 1 bonus for resist cold armor */
    /*   1 bonus for resist cold shield... but that would take a bunch more code. */

    /* try to collect at least 2 of each resist/power (for swapping) */
    /* This can be used to get rid of extra artifacts... */

    /* spare lite sources.  Artifacts only */
    if (num_lite == 1)
        value += 150L;
    else
        if (num_lite == 2)
            value += 170L;
        else
            if (num_lite > 2)
                value += 170L + (num_lite - 2) * 5L;

    if (num_slow_digest == 1)
        value += 50L;
    else
        if (num_slow_digest == 2)
            value += 70L;
        else
            if (num_slow_digest > 2)
                value += 70L + (num_slow_digest - 2) * 5L;

    if (num_regenerate == 1)
        value += 75L;
    else
        if (num_regenerate == 2)
            value += 100L;
        else
            if (num_regenerate > 2)
                value += 100L + (num_regenerate - 2) * 10L;

    if (num_telepathy == 1)
        value += 1000L;
    else
        if (num_telepathy == 2)
            value += 1500L;
        else
            if (num_telepathy > 2)
                value += 1500L + (num_telepathy - 2) * 10L;

    if (num_see_inv == 1)
        value += 800L;
    else
        if (num_see_inv == 2)
            value += 1200L;
        else
            if (num_see_inv > 2)
                value += 1200L + (num_see_inv - 2) * 10L;

    if (num_ffall == 1)
        value += 10L;
    else
        if (num_ffall == 2)
            value += 15L;
        else
            if (num_ffall > 2)
                value += 15L + (num_ffall - 2) * 1L;


    if (num_free_act == 1)
        value += 1000L;
    else
        if (num_free_act == 2)
            value += 1500L;
        else
            if (num_free_act > 2)
                value += 1500L + (num_free_act - 2) * 10L;

    if (num_hold_life == 1)
        value += 1000L;
    else
        if (num_hold_life == 2)
            value += 1500L;
        else
            if (num_hold_life > 2)
                value += 1500L + (num_hold_life - 2) * 10L;

    if (num_resist_acid == 1)
        value += 1000L;
    else
        if (num_resist_acid == 2)
            value += 1500L;
        else
            if (num_resist_acid > 2)
                value += 1500L + (num_resist_acid - 2) * 1L;
    if (num_immune_acid == 1)
        value += 3000L;
    else
        if (num_immune_acid == 2)
            value += 5000L;
        else
            if (num_immune_acid > 2)
                value += 5000L + (num_immune_acid - 2) * 30L;

    if (num_resist_elec == 1)
        value += 1000L;
    else
        if (num_resist_elec == 2)
            value += 1500L;
        else
            if (num_resist_elec > 2)
                value += 1500L + (num_resist_elec - 2) * 1L;
    if (num_immune_elec == 1)
        value += 3000L;
    else
        if (num_immune_elec == 2)
            value += 5000L;
        else
            if (num_immune_elec > 2)
                value += 5000L + (num_immune_elec - 2) * 30L;

    if (num_resist_fire == 1)
        value += 1000L;
    else
        if (num_resist_fire == 2)
            value += 1500L;
        else
            if (num_resist_fire > 2)
                value += 1500L + (num_resist_fire - 2) * 1L;
    if (num_immune_fire == 1)
        value += 3000L;
    else
        if (num_immune_fire == 2)
            value += 5000L;
        else
            if (num_immune_fire > 2)
                value += 5000L + (num_immune_fire - 2) * 30L;

    if (num_resist_cold == 1)
        value += 1000L;
    else
        if (num_resist_cold == 2)
            value += 1500L;
        else
            if (num_resist_cold > 2)
                value += 1500L + (num_resist_cold - 2) * 1L;
    if (num_immune_cold == 1)
        value += 3000L;
    else
        if (num_immune_cold == 2)
            value += 5000L;
        else
            if (num_immune_cold > 2)
                value += 5000L + (num_immune_cold - 2) * 30L;

    if (num_resist_pois == 1)
        value += 5000L;
    else
        if (num_resist_pois == 2)
             value += 9000L;
        else
            if (num_resist_pois > 2)
                value += 9000L + (num_resist_pois - 2) * 40L;

    if (num_resist_conf == 1)
        value += 2000L;
    else
        if (num_resist_conf == 2)
            value += 3500L;
        else
            if (num_resist_conf > 2)
                value += 3500L + (num_resist_conf - 2) * 45L;

    if (num_resist_sound == 1)
        value += 500L;
    else
        if (num_resist_sound == 2)
            value += 700L;
        else
            if (num_resist_sound > 2)
                value += 700L + (num_resist_sound - 2) * 30L;

    if (num_resist_lite == 1)
        value += 100L;
    else
        if (num_resist_lite == 2)
            value += 150L;
        else
            if (num_resist_lite > 2)
                value += 150L + (num_resist_lite - 2) * 1L;

    if (num_resist_dark == 1)
        value += 100L;
    else
        if (num_resist_dark == 2)
            value += 150L;
        else
            if (num_resist_dark > 2)
                value += 150L + (num_resist_dark - 2) * 1L;

    if (num_resist_chaos == 1)
        value += 1000L;
    else
        if (num_resist_chaos == 2)
            value += 1500L;
        else
            if (num_resist_chaos > 2)
                value += 1500L + (num_resist_chaos - 2) * 10L;

    if (num_resist_disen == 1)
        value += 5000L;
    else
        if (num_resist_disen == 2)
            value += 7000L;
        else
            if (num_resist_disen > 2)
                value += 7000L + (num_resist_disen - 2) * 35L;

    if (num_resist_shard == 1)
        value += 100L;
    else
        if (num_resist_shard == 2)
            value += 150L;
        else
            if (num_resist_shard > 2)
                value += 150L + (num_resist_shard - 2) * 1L;

    if (num_resist_nexus == 1)
        value += 200L;
    else
        if (num_resist_nexus == 2)
            value += 300L;
        else
            if (num_resist_nexus > 2)
                value += 300L + (num_resist_nexus - 2) * 2L;

    if (num_resist_blind == 1)
        value += 500L;
    else
        if (num_resist_blind == 2)
            value += 700L;
        else
            if (num_resist_blind > 2)
                value += 700L + (num_resist_blind - 2) * 5L;

    if (num_resist_neth == 1)
        value += 5000L;
    else
        if (num_resist_neth == 2)
            value += 7000L;
        else
            if (num_resist_neth > 2)
                value += 7000L + (num_resist_neth - 2) * 45L;

    /* stat gain items as well...(good to carry ring of dex +6 in */
    /*                            house even if I don't need it right now) */
    if (home_stat_add[A_STR] < 9)
        value += home_stat_add[A_STR] * 300L;
    else
        if (home_stat_add[A_STR] < 15)
            value += 9 * 300L + (home_stat_add[A_STR] - 9) * 200L;
        else
            value += 9 * 300L + 6 * 200L +
                      (home_stat_add[A_STR] - 15) * 1L;

    if (home_stat_add[A_DEX] < 9)
        value += home_stat_add[A_DEX] * 300L;
    else
        if (home_stat_add[A_DEX] < 15)
            value += 9 * 300L + (home_stat_add[A_DEX] - 9) * 200L;
        else
            value += 9 * 300L + 6 * 200L +
                      (home_stat_add[A_DEX] - 15) * 1L;

    /* HACK extra con for thorin and other such things */
    if (home_stat_add[A_CON] < 15)
        value += home_stat_add[A_CON] * 300L;
    else
        if (home_stat_add[A_CON] < 21)
            value += 15 * 300L + (home_stat_add[A_CON] - 15) * 200L;
        else
            value += 15 * 300L + 6 * 200L + (home_stat_add[A_CON] - 21) * 1L;

    /* int and wis are only bonused for spell casters. */
    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (home_stat_add[A_INT] < 20)
            value += home_stat_add[A_INT] * 400L;
        else
            if (home_stat_add[A_INT] < 26)
                value += 20 * 400L + (home_stat_add[A_INT] - 20) * 300L;
            else
                value += 20 * 100L + 6 * 300L +
                         (home_stat_add[A_INT] - 26) * 5L;
    }

    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (home_stat_add[A_WIS] < 20)
            value += home_stat_add[A_WIS] * 400L;
        else
            if (home_stat_add[A_WIS] < 26)
                value += 20 * 400L + (home_stat_add[A_WIS] - 20) * 300L;
            else
                value += 20 * 400L + 6 * 300L +
                        (home_stat_add[A_WIS] - 26) * 3L;
    }

    /* Sustains */
    if (num_sustain_str == 1)
        value += 200L;
    else
        if (num_sustain_str == 2)
            value += 250L;
        else
            if (num_sustain_str > 2)
                value += 250L + (num_sustain_str - 2) * 1L;

    if (num_sustain_int == 1)
        value += 200L;
    else
        if (num_sustain_int == 2)
            value += 250L;
        else
            if (num_sustain_int > 2)
                value += 250L + (num_sustain_int - 2) * 1L;

    if (num_sustain_wis == 1)
        value += 200L;
    else
        if (num_sustain_wis == 2)
            value += 250L;
        else
            if (num_sustain_wis > 2)
                value += 250L + (num_sustain_wis - 2) * 1L;

    if (num_sustain_con == 1)
        value += 200L;
    else
        if (num_sustain_con == 2)
            value += 250L;
        else
            if (num_sustain_con > 2)
                value += 250L + (num_sustain_con - 2) * 1L;

    if (num_sustain_dex == 1)
        value += 200L;
    else
        if (num_sustain_dex == 2)
            value += 250L;
        else
            if (num_sustain_dex > 2)
                value += 250L + (num_sustain_dex - 2) * 1L;

    if (num_sustain_all == 1)
        value += 1000L;
    else
        if (num_sustain_all == 2)
            value += 1500L;
        else
            if (num_sustain_all > 2)
                value += 1500L + (num_sustain_all - 2) * 1L;

    /* do a minus for too many duplicates.  This way we do not store */
    /* useless items and spread out types of items. */
    if (num_weapons > 5)
        value -= (num_weapons - 5) * 2000L;
    else
        if (num_weapons > 1)
            value -= (num_weapons - 1) * 100L;
    if (num_bow > 2)
        value -= (num_bow - 2) * 1000L;
    if (num_rings > 6)
        value -= (num_rings - 6) * 4000L;
    else
        if (num_rings > 4)
            value -= (num_rings - 4) * 2000L;
    if (num_neck > 3)
        value -= (num_neck - 3) * 1500L;
    else
        if (num_neck > 2)
            value -= (num_neck - 2) * 700L;
    if (num_armor > 6)
        value -= (num_armor - 6) * 1000L;
    if (num_cloaks > 3)
        value -= (num_cloaks - 3) * 1000L;
    if (num_shields > 3)
        value -= (num_shields - 3) * 1000L;
    if (num_hats > 4)
        value -= (num_hats - 4) * 1000L;
    if (num_gloves > 3)
        value -= (num_gloves - 3) * 1000L;
    if (num_boots > 2)
        value -= (num_boots - 2) * 1000L;


    value += home_damage;

    /* if edged and priest, dump it   */
    value -= num_edged_weapon * 3000L;

    /* if gloves and mage or ranger and not FA/Dex, dump it. */
    value -= num_bad_gloves * 3000L;

    /* do not allow duplication of items. */
    value -= num_duplicate_items * 50000L;


    /* Return the value */
    return (value);
}


/*
 * Helper function -- calculate power of items in the home
 *
 * The weird calculations help spread out the purchase order
 */
static s32b borg_power_home_aux2(void)
{
    int         k, book;

    s32b        value = 0L;


    /*** Basic abilities ***/

    /* Collect food */
    for (k = 0; k < 50 && k < num_food; k++) value += 8000L - k*10L;

    /* Collect Molds as pets */
    for (k = 0; k < 10 && k < num_mold; k++) value += 10L - k;

    /* Collect ident */
    for (k = 0; k < 50 && k < num_ident; k++) value += 2000L - k*10L;

    /* Collect *id*ent */
    for (k = 0; k < 50 && k < num_star_ident; k++) value += 5000L - k*10L;

    /* apw Collect pfe */
    for (k = 0; k < 100 && k < num_pfe; k++) value += 5000L - k*10L;

    /* apw Collect glyphs */
    for (k = 0; k < 100 && k < num_glyph; k++) value += 5000L - k*10L;

    /* Reward Genocide scrolls. Just scrolls, mainly used for Morgoth */
    for (k = 0; k < 100 && k < num_genocide; k++) value += 5000L - k*10L;

    /* Reward Mass Genocide scrolls. Just scrolls, mainly used for Morgoth */
    for (k = 0; k < 100 && k < num_mass_genocide; k++) value += 5000L - k*10L;

    /* Reward Resistance Potions for Warriors */
    if (auto_class == CLASS_WARRIOR)
    {
        k = 0;
        for (; k <  99 && k < num_pot_rheat; k++) value += 1000L - k*10L;
        for (; k <  99 && k < num_pot_rcold; k++) value +=  1000L - k*10L;
    }

    /* Collect recall */
    for (k = 0; k < 50 && k < num_recall; k++) value += 3000L - k*10L;

    /* Collect escape */
    for (k = 0; k < 50 && k < num_escape; k++) value += 2000L - k*10L;

    /* Collect teleport */
    for (k = 0; k < 50 && k < num_teleport; k++) value += 400L - k*8L;

    /* Collect teleport level scrolls*/
    for (k = 0; k < 99 && k < num_teleport_level; k++) value += 1000L - k*8L;

    /* Collect Speed */
    for (k = 0; k < 99 && k < num_speed; k++) value += 5000L - k*10L;

    /* collect heal/mana/ */
    for (k = 0; k < 99 && k < num_heal; k++) value += 3000L - k*8L;
    for (k = 0; k < 99 && k < num_ez_heal; k++) value += 8000L - k*8L;
    if (auto_msp > 1)
    {
        for (k = 0; k < 99 && k < num_mana; k++) value += 6000L - k*8L;
    }

    /* Level 1 priests are given a Potion of Healing.  It is better
     * for them to sell that potion and buy equipment or several
     * Cure Crits with it.
     */
    if (borg_skill[BI_CLEVEL] == 1)
    {
        k = 0;
        for (; k < 10 && k < num_heal; k++) value -= 5000L;
    }

    /*** Healing ***/

    /* Collect cure critical */
    for (k = 0; k < 99 && k < num_cure_critical; k++) value += 1500L-k*10L;

    /* junk cure serious if we have some in the home */
    if (borg_skill[BI_CLEVEL] > 35)    /* dont bother keeping them if high level */
        for (k = 0; k < 99 && k < num_cure_serious; k++) value -= 1500L-k*10L;

    /*** Various ***/

    /* Fixing Stats */
    if (borg_skill[BI_CLEVEL] == 50) value -= 7500L * num_fix_exp;
    if (borg_skill[BI_CLEVEL] > 35)
       for (k = 0; k < 70 && k < num_fix_exp; k++) value += 5000L - k*10L;
    else
       for (k = 0; k < 5 && k < num_fix_exp; k++) value += 5000L - k*10L;

    /* Keep shrooms in the house */
    for (k = 0; k < 99 && k < num_fix_stat[6]; k++) value += 5000L;


    /*** Hack -- books ***/

    /* Reward books */
    for (book = 0; book < 4; book++)
    {

        if (borg_skill[BI_CLEVEL] > 35)
            /* Collect up to 20 copies of each normal book */
            for (k = 0; k < 20 && k < num_book[book]; k++)
            {
                /* Hack -- only stockpile useful books */
                if (num_book[book]) value += 5000L - k*10L;
            }
        else
            /* Collect up to 5 copies of each normal book */
            for (k = 0; k < 5 && k < num_book[book]; k++)
            {
                /* Hack -- only stockpile useful books */
                if (num_book[book]) value += 5000L - k*10L;
            }
    }

    /* Reward artifacts in the home */
    value += num_artifact * 500L;

    /* Return the value */
    return (value);
}


/*
 * Calculate the "power" of the home
 */
s32b borg_power_home(void)
{
    s32b value = 0L;

    /* Process the home equipment */
    value += borg_power_home_aux1();

    /* Process the home inventory */
    value += borg_power_home_aux2();

    /* Return the value */
    return (value);
}


/*
 * Calculate base danger from a monster's physical attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 * apw. PFE reduces my fear of an area.
 */
static int borg_danger_aux1(int i)
{
    int k, n = 0;
    int pfe = 0;

    int power, chance;

    s16b ac = borg_skill[BI_ARMOR];

    auto_kill *kill = &auto_kills[i];

    monster_race *r_ptr = &r_info[kill->r_idx];

    /* goi gives +100 to ac and deflects almost all missiles and balls*/
    if (borg_goi)
        ac += 100;

    /* shields gives +50 to ac and deflects some missiles and balls*/
    if (borg_shield)
        ac += 50;

    /* apw PFE gives a protection.  */
        /* Hack -- Apply "protection from evil" */
        if ( (borg_prot_from_evil) &&
            (r_ptr->flags3 & RF3_EVIL) &&
            ((borg_skill[BI_CLEVEL]) >= r_ptr->level) )
        {
            pfe = 1;
        }


    /* Mega-Hack -- unknown monsters */
    if (kill->r_idx >= z_info->r_max) return (1000);

    /* Analyze each physical attack */
    for (k = 0; k < 4; k++)
    {
        int z = 0;

        monster_blow *b_ptr = &r_ptr->blow[k];

        power = 0;

        /* Done */
        if (!b_ptr->method) break;

        /* Analyze the attack */
        switch (b_ptr->effect)
        {
            case RBE_HURT:
            z = (b_ptr->d_dice * b_ptr->d_side);
            z -= (z * ((ac < 150) ? ac : 150) / 250);
            /*apw  if invulnurable (or PFE), no damage (carried through)*/
            if ((borg_goi ) && !borg_attacking)
                z = 0;
            if ((pfe) && !borg_attacking)
                z /= 2;
            /* stun */
            if ((b_ptr->d_side < 3) && (z > b_ptr->d_dice * b_ptr->d_side))
                n += 200;
            /* fudge- only mystics kick and they tend to KO.  Avoid close */
            /* combat like the plauge */
            if (b_ptr->method == RBM_KICK)
                n += borg_goi ? 400 * 20 : 400; /* If GOI is on, take that into account */
            power = 60;
            break;

            case RBE_POISON:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 5;
            if (borg_skill[BI_RPOIS]) break;
            if (my_oppose_pois) break;
            if (!borg_full_damage)
                z += 10;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_UN_BONUS:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 20;
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RDIS]) break;
            /* if invulnurable, no damage */
            if (!borg_full_damage)
                z += 500;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_UN_POWER:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            power = 15;
            break;

            case RBE_EAT_GOLD:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if in town and low level avoid them stupid urchins */
            if (borg_skill[BI_CLEVEL] < 5) z += 50;
            /* if invulnurable, no damage */
            power = 5;
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (100 <= adj_dex_safe[my_stat_ind[A_DEX]] + borg_skill[BI_CLEVEL]) break;
            if (auto_gold < 100) break;
            if (auto_gold > 100000) break;
            if (!borg_full_damage)
                z += 5;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EAT_ITEM:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            power = 5;
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (100 <= adj_dex_safe[my_stat_ind[A_DEX]] + borg_skill[BI_CLEVEL]) break;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EAT_FOOD:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 5;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_FOOD] > 5) break;
            if (!borg_full_damage)
                z += 5;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EAT_LITE:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 5;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_CUR_LITE] == 0) break;
            if (borg_skill[BI_AFUEL] > 5) break;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_ACID:
            if (borg_skill[BI_IACID]) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            if (borg_skill[BI_RACID]) z = (z + 2) / 3;
            if (my_oppose_acid) z = (z + 2) / 3;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (!borg_full_damage)
                z += 200; /* We dont want our armour corroded. */
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_ELEC:
            if (borg_skill[BI_IELEC]) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RELEC]) z = (z + 2) / 3;
            if (my_oppose_elec) z = (z + 2) / 3;
            if (!borg_full_damage)
                z += 10;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_FIRE:
            if (borg_skill[BI_IFIRE]) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RFIRE]) z = (z + 2) / 3;
            if (my_oppose_fire) z = (z + 2) / 3;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_COLD:
            if (borg_skill[BI_ICOLD]) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RCOLD]) z = (z + 2) / 3;
            if (my_oppose_cold) z = (z + 2) / 3;
            if (!borg_full_damage)
                z += 15;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_BLIND:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 2;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RBLIND]) break;
            if (!borg_full_damage)
                z += 10;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_CONFUSE:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RCONF]) break;
            if (!borg_full_damage)
                z += 200;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_TERRIFY:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_RFEAR]) break;
            if (!borg_full_damage)
                z += 10;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_PARALYZE:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 2;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_FRACT]) break;
            z += 200;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_STR:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SSTR]) break;
            if (auto_stat[A_STR] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            z += 150;
            /* extra scary to have str drain below 10 */
            if (auto_stat[A_STR] < 10)
                z += 350;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_DEX:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SDEX]) break;
            if (auto_stat[A_DEX] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            z += 150;
            /* extra scary to have drain below 10 */
            if (auto_stat[A_DEX] < 10)
                z += 350;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_CON:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SCON]) break;
            if (auto_stat[A_CON] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            if (!borg_full_damage)
            z += 150;
            /* extra scary to have con drain below 8 */
            if (auto_stat[A_STR] < 8)
                z += 350;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_INT:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SINT]) break;
            if (auto_stat[A_INT] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            z += 150;
            /* extra scary for spell caster */
            if (mb_ptr->spell_book == TV_MAGIC_BOOK)
                z += 350;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_WIS:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SWIS]) break;
            if (auto_stat[A_WIS] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            z += 150;
            /* extra scary for pray'er */
            if (mb_ptr->spell_book == TV_PRAYER_BOOK)
                z += 350;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_CHR:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            if (borg_skill[BI_SCHR]) break;
            if (auto_stat[A_CHR] <= 3) break;
            if (borg_prayer_legal(6, 3)) break;
            z += 50;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_ALL:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 2;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z /= 25;
            /* only morgoth. HACK to make it easier to fight him */
            break;

            case RBE_SHATTER:
            z = (b_ptr->d_dice * b_ptr->d_side);
            z -= (z * ((ac < 150) ? ac : 150) / 250);
            power = 60;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (!borg_full_damage)
                z += 150;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EXP_10:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_HLIFE]) break;
            /* do not worry about drain exp after level 50 */
            if (borg_skill[BI_CLEVEL] == 50) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 100;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EXP_20:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_HLIFE]) break;
            /* do not worry about drain exp after level 50 */
            if (borg_skill[BI_CLEVEL] >= 50) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 150;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EXP_40:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_HLIFE]) break;
            /* do not worry about drain exp after level 50 */
            if (borg_skill[BI_CLEVEL] >= 50) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 200;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EXP_80:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = 0;
            if (borg_skill[BI_HLIFE]) break;
            /* do not worry about drain exp after level 50 */
            if (borg_skill[BI_CLEVEL] >= 50) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 250;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;
        }

        /* if we are doing partial damage reduse for % chance that it will */
        /* hit you. */
        if (!borg_full_damage)
        {
            /* figure out chance that monster will hit you. */
            /* add a 30% bonus in to account for bad luck. */
            if ((r_ptr->level + power) > 0)
                chance  = 130 - (((ac * 300) / 4) / ((r_ptr->level + power) * 3));
            else
                chance = -1;

            /* always have a 5% chance of hitting. */
            if (chance < 0)
                z = (z * 5) / 100;

            if (chance < 100)
                z = (z * chance) / 100;
        }

        /* Add in damage */
        n += z;

    }

    /* apw if invulnurable, very little damage 5% */
    if (borg_goi) n = (n*5/100);

    /* Return danger */
    return (n);
}


/*
 * Calculate base danger from a monster's spell attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 *
 * We reduce the danger if the monster is immobile or not LOS
 */
static int borg_danger_aux2(int i, int y, int x)
{
    int q, k, n= 0, pfe =0, glyph= 0, glyph_check =0;

    int true_borg_goi = borg_goi;

    int spot_x, spot_y, spot_safe=1;

    int lev, hp, total_dam = 0, av;

    byte spell[96], num = 0;

    auto_kill *kill = &auto_kills[i];

    auto_grid   *ag;

    monster_race *r_ptr = &r_info[kill->r_idx];

    /* apw PFE gives a protection.  */
        /* Hack -- Apply "protection from evil" */
        if ( (borg_prot_from_evil) &&
            (r_ptr->flags3 & RF3_EVIL) &&
            ((borg_skill[BI_CLEVEL] ) >= r_ptr->level) )
        {
            pfe = 1;
        }

    /* glyph of warding rune of protection provides some small
     * protection with some ranged atacks; mainly summon attacks.
     * We should reduce the danger commensurate to the probability of the
     * monster breaking the glyph as defined by melee2.c
     */
    if (borg_on_glyph)
    {
        glyph = 1;
    }
    else if (track_glyph_num)
    {
        /* Check all existing glyphs */
        for (glyph_check = 0; glyph_check < track_glyph_num; glyph_check++)
        {
            if ((track_glyph_y[glyph_check] == y) && (track_glyph_x[glyph_check] == x))
            {
                /* Reduce the danger */
                glyph = 1;
            }
        }
    }

    /* This is used to calculate the free squares next to us.
     * This is important when dealing with summoners.
     */
    for (spot_x = -1; spot_x <= 1; spot_x++)
    {
        for (spot_y = -1; spot_y <= 1; spot_y++)
        {
            /* Acquire location */
            x = spot_x + c_x;
            y = spot_y + c_y;

            ag = &auto_grids[y][x];

            /* skip our own spot */
            if (x == c_x && y == c_y) continue;

            /* track spaces already protected */
            if ( ag->feat == FEAT_GLYPH || ag->kill ||
               ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_PERM_SOLID)))
            {   /* track the safe areas for calculating danger */
                spot_safe ++;

                /* Just in case */
                if (spot_safe == 0) spot_safe =1;
            }

        }
    }
    /* HACK- to accomdate for GOI and Create_Door */
    if (borg_create_door)
    {
        borg_goi = 0;
    }
    /* Mega-Hack -- unknown monsters */
    if (kill->r_idx >= z_info->r_max) return (1000);


    /* Extract the "inate" spells */
    for (k = 0; k < 32; k++)
    {
        if (r_ptr->flags4 & (1L << k)) spell[num++] = k + 32 * 3;
    }

    /* Extract the "normal" spells */
    for (k = 0; k < 32; k++)
    {
        if (r_ptr->flags5 & (1L << k)) spell[num++] = k + 32 * 4;
    }

    /* Extract the "bizarre" spells */
    for (k = 0; k < 32; k++)
    {
        if (r_ptr->flags6 & (1L << k)) spell[num++] = k + 32 * 5;
    }

    /* Paranoia -- Nothing to cast */
    if (!num) return (0);


    /* Extract the level */
    lev = r_ptr->level;

    /* Extract hit-points */
    hp = kill->power;


    /* Analyze the spells */
    for (q = 0; q < num; q++)
    {
        int p = 0;

        int z = 0;

        /* Cast the spell. */
        switch (spell[q])
        {
            case 96+0:    /* RF4_SHRIEK */
            /* if looking at full damage, things that are just annoying */
            /* do not count.*/
            if (!borg_full_damage)
                p += 10;
            break;

            case 96+1:    /* RF4_XXX2X4 */
            /* this is now a failed spell attempt for monsters */
            /* used to recognize invisible/ hidden monsters */
                p += 10;
            break;

            case 96+2:    /* RF4_XXX3X4 */
            break;

            case 96+3:    /* RF4_XXX4X4 */
            break;

            case 96+4:    /* RF4_ARROW_1 */
            z = (1 * 6);
            if (borg_goi) {z /=25; break;}
            break;

            case 96+5:    /* RF4_ARROW_2 */
            z = (3 * 6);
            if (borg_goi) {z /=25; break;}
            break;

            case 96+6:    /* RF4_ARROW_3 */
            z = (5 * 6);
            if (borg_goi) {z /=25; break;}
            break;

            case 96+7:    /* RF4_ARROW_4 */
            z = (7 * 6);
            if (borg_goi) {z /=25; break;}
            break;

            case 96+8:    /* RF4_BR_ACID */
            if (borg_skill[BI_IACID]) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RACID]) z = (z + 2) / 3;
            if (my_oppose_acid) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count.*/
            if (!borg_full_damage)
                p += 40;
            break;

            case 96+9:    /* RF4_BR_ELEC */
            if (borg_skill[BI_IELEC]) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RELEC]) z = (z + 2) / 3;
            if (my_oppose_elec) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count.*/
            if (!borg_full_damage)
                p += 20;
            break;

            case 96+10:    /* RF4_BR_FIRE */
            if (borg_skill[BI_IFIRE]) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RFIRE]) z = (z + 2) / 3;
            if (my_oppose_fire) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 96+11:    /* RF4_BR_COLD */
            if (borg_skill[BI_ICOLD]) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RCOLD]) z = (z + 2) / 3;
            if (my_oppose_cold) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 96+12:    /* RF4_BR_POIS */
            z = (hp / 3);
            /* max damage */
            if (z > 800)
                z = 800;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RPOIS]) z = (z + 2) / 3;
            if (my_oppose_pois) z = (z + 2) / 3;
            if (my_oppose_pois) break;
            if (borg_skill[BI_RPOIS]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 96+13:    /* RF4_BR_NETH */
            z = (hp / 6);
            /* max damage */
            if (z > 350)
                z = 550;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RNTHR])
            {
                z = (z*6)/7;
                break;
            }
            if (!borg_full_damage)
                p += 125;
            break;

            case 96+14:    /* RF4_BR_LITE */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RLITE])
            {
                z = (z*2)/3;
                break;
            }
            if (borg_skill[BI_RBLIND]) break;
            p += 20;
            break;

            case 96+15:    /* RF4_BR_DARK */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RDARK]) z = (z*2)/ 3;
            if (borg_skill[BI_RDARK]) break;
            if (borg_skill[BI_RBLIND]) break;
            p += 20;
            break;

            case 96+16:    /* RF4_BR_CONF */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RCONF]) z = z / 2;
            if (borg_skill[BI_RCONF]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 300;
            break;

            case 96+17:    /* RF4_BR_SOUN */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) z = (z*5)/8;
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous dangerousabout this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 96+18:    /* RF4_BR_CHAO */
            z = (hp / 6);
            /* max damage */
            if (z > 600)
                z = 600;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RKAOS]) z = (z*6)/8;
            if (!borg_full_damage)
                p += 100;
            if (borg_skill[BI_RKAOS]) break;
            p += 200;
            break;

            case 96+19:    /* RF4_BR_DISE */
            z = (hp / 6);
            /* max damage */
            if (z > 500)
                z = 500;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RDIS]) z = (z*6)/10;
            if (borg_skill[BI_RDIS]) break;
            p += 500;
            break;

            case 96+20:    /* RF4_BR_NEXU */
            z = (hp / 3);
            /* max damage */
            if (z > 250)
                z = 250;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RNXUS]) z = (z*6)/10;
            if (borg_skill[BI_RNXUS]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 100;
            break;

            case 96+21:    /* RF4_BR_TIME */
            z = (hp / 3);
            /* max damage */
            if (z > 150)
                z = 150;
            if (borg_goi) {z /=25; break;}
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 250;
            break;

            case 96+22:    /* RF4_BR_INER */
            z = (hp / 6);
            /* max damage */
            if (z > 200)
                z = 200;
            if (borg_goi) {z /=25; break;}
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 100;
            break;

            case 96+23:    /* RF4_BR_GRAV */
            z = (hp / 3);
            /* max damage */
            if (z > 200)
                z = 200;
            if (borg_goi) {z /=25; break;}
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 100;
            if (borg_skill[BI_RSND]) break;
            /* Pump this up if you have goi so that the borg is sure */
            /* to be made nervous */
            if (borg_goi)
                p += 100;
            else
                p += 75;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            break;

            case 96+24:    /* RF4_BR_SHAR */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSHRD]) z = (z*6)/10;
            if (borg_skill[BI_RSHRD]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 96+25:    /* RF4_BR_PLAS */
            z = (hp / 6);
            /* max damage */
            if (z > 150)
                z = 150;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) break;
            /* Pump this up if you have goi so that the borg is sure */
            /* to be made nervous */
            if (borg_goi)
                p += 200;
            else
                p += 100;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            break;

            case 96+26:    /* RF4_BR_WALL */
            z = (hp / 6);
            /* max damage */
            if (z > 150)
                z = 150;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 100;
            if (do_heavy_stun)
                p += 500;
            if (!borg_full_damage)
                p += 50;
            break;

            case 96+27:    /* RF4_BR_MANA */
            /* XXX XXX XXX */
            if (borg_goi) {z /=25; break;}
            break;

            case 96+28:    /* RF4_XXX5X4 */
            break;

            case 96+29:    /* RF4_XXX6X4 */
            break;

            case 96+30:    /* RF4_XXX7X4 */
            break;

            case 96+31:    /* RF4_XXX8X4 */
            break;

            case 128+0:    /* RF5_BA_ACID */
            if (borg_skill[BI_IACID]) break;
            z = (lev * 3) / 2 + 15;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RACID]) z = (z + 2) / 3;
            if (my_oppose_acid) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+1:    /* RF5_BA_ELEC */
            if (borg_skill[BI_IELEC]) break;
            z = (lev * 3) / 2 + 8;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RELEC]) z = (z + 2) / 3;
            if (my_oppose_elec) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+2:    /* RF5_BA_FIRE */
            if (borg_skill[BI_IFIRE]) break;
            z = (lev * 7) / 2 + 10;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RFIRE]) z = (z + 2) / 3;
            if (my_oppose_fire) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+3:    /* RF5_BA_COLD */
            if (borg_skill[BI_ICOLD]) break;
            z = (lev * 3) / 2 + 10;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RCOLD]) z = (z + 2) / 3;
            if (my_oppose_cold) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+4:    /* RF5_BA_POIS */
            z = (12 * 2);
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RPOIS]) z = (z + 2) / 3;
            if (my_oppose_pois) z = (z + 2) / 3;
            if (my_oppose_pois) break;
            if (borg_skill[BI_RPOIS]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+5:    /* RF5_BA_NETH */
            z = (50 +50 + (10 * 10) + lev);
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RNTHR]) z = (z*6)/8;
            if (borg_skill[BI_RNTHR]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 250;
            break;

            case 128+6:    /* RF5_BA_WATE */
            z = ((lev * 5) / 2) + 50;
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            if (borg_skill[BI_RCONF]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 128+7:    /* RF5_BA_MANA */
            z = ((lev * 5) + 150);
            if (!borg_full_damage)
                p += 50;
            if (borg_goi) {z /=25; break;}
            break;

            case 128+8:    /* RF5_BA_DARK */
            z = (((lev * 5)) + (50));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RDARK]) z = z / 2;
            if (borg_skill[BI_RDARK]) break;
            if (borg_skill[BI_RBLIND]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+9:    /* RF5_DRAIN_MANA */
            if (auto_msp) p += 10;
            break;

            case 128+10:    /* RF5_MIND_BLAST */
            if (borg_skill[BI_SAV] < 100)
                z = 20;
            break;

            case 128+11:    /* RF5_BRAIN_SMASH */
            z = (12 * 15);
            p += 200 - 2 * borg_skill[BI_SAV];
            if (p < 0) p =0;
            break;

            case 128+12:    /* RF5_CAUSE_1 */
            if (borg_skill[BI_SAV] >= 100) break;
            z = (3 * 8);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - borg_skill[BI_SAV]) / 100;
            break;

            case 128+13:    /* RF5_CAUSE_2 */
            if (borg_skill[BI_SAV] >= 100) break;
            z = (8 * 8);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - borg_skill[BI_SAV]) / 100;
            break;

            case 128+14:    /* RF5_CAUSE_3 */
            if (borg_skill[BI_SAV] >= 100) break;
            z = (10 * 15);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - borg_skill[BI_SAV]) / 100;
            break;

            case 128+15:    /* RF5_CAUSE_4 */
            if (borg_skill[BI_SAV] >= 100) break;
            z = (15 * 15);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 40% for fudge) */
                z = z * (120 - borg_skill[BI_SAV]) / 100;
            break;

            case 128+16:    /* RF5_BO_ACID */
            if (borg_skill[BI_IACID]) break;
            z = ((7 * 8) + (lev / 3));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RACID]) z = (z + 2) / 3;
            if (my_oppose_acid) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+17:    /* RF5_BO_ELEC */
            if (borg_skill[BI_IELEC]) break;
            z = ((4 * 8) + (lev / 3));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RELEC]) z = (z + 2) / 3;
            if (my_oppose_elec) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+18:    /* RF5_BO_FIRE */
            if (borg_skill[BI_IFIRE]) break;
            z = ((9 * 8) + (lev / 3));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RFIRE]) z = (z + 2) / 3;
            if (my_oppose_fire) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+19:    /* RF5_BO_COLD */
            if (borg_skill[BI_ICOLD]) break;
            z = ((6 * 8) + (lev / 3));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RCOLD]) z = (z + 2) / 3;
            if (my_oppose_cold) z = (z + 2) / 3;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+20:    /* RF5_BO_POIS */
            /* XXX XXX XXX */
            break;

            case 128+21:    /* RF5_BO_NETH */
            z = (50 + 30 + (5 * 5) + (lev * 3) / 2);
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RNTHR]) z = (z*6)/8;
            if (borg_skill[BI_RNTHR]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 200;
            break;

            case 128+22:    /* RF5_BO_WATE */
            z = ((10 * 10) + (lev));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            if (borg_skill[BI_RCONF]) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+23:    /* RF5_BO_MANA */
            z = ((lev * 7) / 2) + 50;
            if (!borg_full_damage)
                p += 50;
            if (borg_goi) {z /=25; break;}
            break;

            case 128+24:    /* RF5_BO_PLAS */
            z = (10 + (8 * 7) + (lev));
            if (borg_goi) {z /=25; break;}
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 500;
            if (do_heavy_stun)
                p += 1000;
            break;

            case 128+25:    /* RF5_BO_ICEE */
            z = ((6 * 6) + (lev));
            if (borg_goi) {z /=25; break;}
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            if (borg_skill[BI_RSND]) break;
            /* if already stunned be REALLY nervous about this */
            if (do_stun)
                p += 50;
            if (do_heavy_stun)
                p += 1000;
            break;

            case 128+26:    /* RF5_MISSILE */
            z = ((2 * 6) + (lev / 3));
            if (borg_goi) {z /=25; break;}
            break;

            case 128+27:    /* RF5_SCARE */
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+28:    /* RF5_BLIND */
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+29:    /* RF5_CONF */
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+30:    /* RF5_SLOW */
            if (borg_skill[BI_FRACT]) break;
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 5;
            break;

            case 128+31:    /* RF5_HOLD */
            if (borg_skill[BI_FRACT]) break;
            if (borg_skill[BI_SAV] >= 100) break;
            p += 150;
            break;

            case 160+0:    /* RF6_HASTE */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 160+1:    /* RF6_XXX1X6 */
            break;

            case 160+2:    /* RF6_HEAL */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 160+3:    /* RF6_XXX2X6 */
            break;

            case 160+4:    /* RF6_BLINK */
            break;

            case 160+5:    /* RF6_TPORT */
            break;

            case 160+6:    /* RF6_XXX3X6 */
            break;

            case 160+7:    /* RF6_XXX4X6 */
            break;

            case 160+8:    /* RF6_TELE_TO */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 160+9:    /* RF6_TELE_AWAY */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 160+10:    /* RF6_TELE_LEVEL */
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 160+11:    /* RF6_XXX5 */
            break;

            case 160+12:    /* RF6_DARKNESS */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 5;
            break;

            case 160+13:    /* RF6_TRAPS */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 160+14:    /* RF6_FORGET */
            if (borg_skill[BI_SAV] >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
            {
                /* if you have lots of cash (like you will at level 35) */
                /* this is not very scary... just re-ID. */
                if (borg_skill[BI_CLEVEL] < 35)
                {
                    p += 500;
                }
                else
                {
                    p += 50;
                }
            }
            break;

            case 160+15:    /* RF6_XXX6X6 */
            break;

            /* Summoning is only as dangerous as the monster that is
             * actually summoned but the monsters that summon are a priority
             * to kill.  PFE reduces danger from some evil summoned monsters
             * One Problem with GOI and Create Door is that the GOI reduces
             * the fear so much that the borg won't cast the Create Door,
             * eventhough it would be a good idea.
             */

            case 160+16:    /* S_KIN */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else
            {    p += (lev) * 7;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;

            break;

            case 160+17:    /* S_HI_DEMON */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else
            {    p += (lev) * 12;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;


            case 160+18:    /* RF6_S_MONSTER */
            if (borg_goi || pfe || glyph || borg_create_door || borg_fighting_unique)
                p +=0;
            else
            {    p += (lev) * 5;
                p = p / spot_safe;
            }
            break;

            case 160+19:    /* RF6_S_MONSTERS */
            if (borg_goi || pfe || glyph || borg_create_door || borg_fighting_unique)
                p +=0;
            else
            {    p += (lev) * 7;
                 p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+20:   /* RF6_S_ANT */
            if (borg_goi || pfe || glyph || borg_create_door || borg_fighting_unique)
                p +=0;
            else
            {   p += (lev) * 5;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+21:    /* RF6_S_SPIDER */
            if (borg_goi || pfe || glyph || borg_create_door || borg_fighting_unique)
                p +=0;
            else
            {   p += (lev) * 5;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+22:    /* RF6_S_HOUND */
            if (borg_goi || pfe || glyph || borg_create_door || borg_fighting_unique)
                p +=0;
            else
            {    p += (lev) * 5;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+23:    /* RF6_S_HYDRA */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {   p +=(lev) * 2;
                p = p / spot_safe;
            }
            else
            {   p += (lev) * 5;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+24:    /* RF6_S_ANGEL */
            if (pfe  || borg_fighting_unique)
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev)* 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 7;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+25:    /* RF6_S_DEMON */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 7;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+26:    /* RF6_S_UNDEAD */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 7;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+27:    /* RF6_S_DRAGON */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 7;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+28:    /* RF6_S_HI_UNDEAD */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 12;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+29:    /* RF6_S_HI_DRAGON */
            if (pfe )
            {
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 12;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+30:    /* RF6_S_WRAITH */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door || borg_fighting_unique)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 12;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;

            case 160+31:    /* RF6_S_UNIQUE */
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (borg_goi)
            {
                p =0;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 3;    /* slightly reduced danger for unique */
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 6;
                p = p / spot_safe;
            }
            /* reduce the fear if it is a unique */
            if (r_info->flags1 & RF1_UNIQUE) p = p * 75/100;
            break;
        }

        /* Notice damage */
        p += z;

        /* Track the most dangerous spell */
        if (p > n) n = p;

        /* Track the damage of all the spells, used in averaging */
        total_dam +=p;
    }

    /* Average damage of all the spells & compare to most dangerous spell */
    av = total_dam / num;

    /* HACK- to accomdate for GOI and Create_Door */
    borg_goi = true_borg_goi;

    /* If the most dangerous spell is alot bigger than the average,
     * then return the dangerous one
     */
    if (n >= av * 15/10 || n > auto_chp * 8/10) return (n);
    else
    /* Average Danger */
    return (av);
}


/*
 * Calculate the danger to a grid from a monster  XXX XXX XXX
 *
 * Note that we are paranoid, especially about "monster speed",
 * since even if a monster is slower than us, it will occasionally
 * get one full turn to attack us.
 *
 * Note that we assume that monsters can walk through walls and
 * other monsters to get to the player.  XXX XXX XXX
 *
 * This function ignores possibilities such as movement plus
 * spell attacks, physical attacks and spell attacks together,
 * and other similar situations.  XXX XXX XXX
 *
 * Currently we assume that "sleeping" monsters are less dangerous
 * unless you get near them, which may wake them up.
 *
 * We attempt to take into account things like monsters which sometimes
 * "stumble", and monsters which only "sometimes" use powerful spells.
 */
int borg_danger_aux(int y, int x, int c, int i)
{
    auto_kill *kill = &auto_kills[i];

    monster_race *r_ptr = &r_info[kill->r_idx];

    int x9 = kill->x;
    int y9 = kill->y;

    int ax, ay, d;

    int q=0, r, p, v1=0, v2=0;

    int glyph =0;

    int fake_speed = auto_speed;
    int monster_speed = kill->speed;

    /* Paranoia */
    if (!kill->r_idx) return (0);


    /* Distance components */
    ax = (x9 > x) ? (x9 - x) : (x - x9);
    ay = (y9 > y) ? (y9 - y) : (y - y9);

    /* Distance */
    d = MAX(ax, ay);

    /* Minimal distance */
    if (d < 1) d = 1;

    /* Minimal distance */
    if (d > 20) return (0);

    /* A very speedy borg will miscalculate danger of some monsters */
    if (auto_speed >=135) fake_speed = (borg_fighting_unique ? 120 : 125);

    /* Consider the character haste and slow monster spells */
    if (borg_speed)
        fake_speed += 10;
    if (borg_slow_spell)
        monster_speed -= 10;

    /* Assume monsters are a little fast when you are low level */
    if (auto_mhp < 20)
        monster_speed += 7;

    {
        /* Ugly!  we recalculate the moves */
        int t, e;

        /* Player energy per game turn  */
        e = extract_energy[(fake_speed)];

        /* Game turns per player move  */
        t = (100 + (e - 1)) / e;

        /*  Monster energy per game turn  */
        e = extract_energy[monster_speed];

        /* Monster moves */
        q = c * ((t * e) / 10);
    }

    /* Minimal energy */
    /* allow partial hits when not caculating full possible damage */
    if (borg_full_damage)
        q = (int)((q+9)/10)*10;


    /* Danger from physical attacks */

    /* Physical attacks */
    v1 = borg_danger_aux1(i);

    /* Hack -- Under Stressful Situation.
     */
    if (time_this_panel > 1200 || c_t > 25000)
    {
        /* he might be stuck and could overflow */
        v1 = v1 / 5;
    }

    /* No attacks for some monsters */
    if (r_ptr->flags1 & RF1_NEVER_BLOW)
    {
        v1 = 0;
    }

    /* No movement for some monsters */
    if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (d > 1))
    {
        v1 = 0;
    }

    /* Hack -- Physical attacks require proximity */
    /* If the monster is next to us and gets a partial hit, count it. */
    if (q > 10 || d != 1)
    {
        if (q < (d * 10)  && borg_skill[BI_CLEVEL] > 20)
        {
            v1 = 0;
        }
        else if (q < (d * 10) && borg_skill[BI_CLEVEL] <= 20)
        {   /* reduce damage to 20% if we are weak */
            v1 = (v1 * 2/10);
        }
    }

    /* multipliers yeild some trouble when I am weak */
    if ((r_ptr->flags2 & RF2_MULTIPLY) && (borg_skill[BI_CLEVEL] < 20))
    {   /* extra 50% */
        v1 = v1 + (v1 *15/10);
    }

    /* Friends yeild some trouble when I am weak */
    if ((r_ptr->flags1 & RF1_FRIENDS || r_ptr->flags1 & RF1_ESCORTS) &&
        (borg_skill[BI_CLEVEL] < 20))
    {
        if (borg_skill[BI_CLEVEL] < 15)
        {
            /* extra 80% */
            v1 = v1 + (v1 *18/10);
        }
        else
        {
            /* extra 30% */
            v1 = v1 + (v1 *13/10);
        }

    }

    /* glyph of warding rune of protection reduction here
     * We should reduce the danger commensurate to the probability of the
     * monster breaking the glyph as defined by melee2.c
     */
    if (borg_on_glyph)
    {
        v1 = 0;
    }
    if (track_glyph_num)
    {
        /* Check all existing glyphs */
        for (glyph = 0; glyph < track_glyph_num; glyph++)
        {
            if ((track_glyph_y[glyph] == y) && (track_glyph_x[glyph] == x))
            {
                /* Reduce the danger */
                v1 = 0;
            }
        }
    }



    /* Reduce danger from sleeping monsters */
    if ((!kill->awake) && (d > 1))
    {
        /* Normal reduction of fear */
        if (borg_skill[BI_CLEVEL] >= 10 )
        {
             v1 = v1 / d;
        }
        else
        {
            /* low clevel weaklings should still fear alot*/
            v1 = v1 * 8/10;
        }
    }
     /* Reduce danger from sleeping monsters with the sleep 2 spell*/
    if (borg_sleep_spell_ii)
    {
        if  ( (d == 1) &&
             (kill->awake) &&
             (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
             (!(r_ptr->flags1 & RF1_UNIQUE)) &&
             (kill->level <= (borg_skill[BI_CLEVEL] - 15)))
        {
              v1 = v1 / 3;
        }
    }
     /* Reduce danger from sleeping monsters with the sleep 1,3 spell*/
    if (borg_sleep_spell)
    {
        v1 = v1 / (d+2);
    }
    /* Reduce danger from confused monsters */
    if (kill->confused)
    {
       v1 = v1 / 2;
    }
    if (kill->stunned)
    {
       v1 = v1 * 10 / 13;
    }
     if (borg_confuse_spell)
    {
        v1 = v1 / 6;
    }
     /* Perceive a reduce danger from scared monsters */
    if (borg_fear_mon_spell)
    {
        v1 = 0;
    }

    /* Tweak danger based on the "alertness" of the monster */
    if (!kill->awake)
    {
        /* increase the danger for light sleepers */
        int inc = r_ptr->sleep + 5;

        v1 = v1 + (v1*inc/100);
    }

    /* Danger */
    if (v1)
    {
        /* Attacks after movement */
        r = (q - ((d-1) * 10));

         /* XXX XXX XXX */
        if (c > 1)
        {
            /* Hack -- stumble sometimes XXX XXX XXX */
            if (r_ptr->flags1 & (RF1_RAND_25 | RF1_RAND_50)) r -= (r / 4);
        }
         /* Total danger */
        v1 = v1 * r / 10;

    }

   /* Never cast spells */
    if (!r_ptr->freq_inate && !r_ptr->freq_spell)
    {
        v2 = 0;
    }

    /* Hack -- verify distance */
    else if (distance(y9, x9, y, x) > MAX_RANGE)
    {
        v2 = 0;
    }

    /* Hack -- verify line of sight (both ways)*/
    else if (!borg_projectable(y9, x9, y, x) && !borg_projectable(y, x, y9, x9))
    {
        v2 = 0;
    }

    /* Danger from spell attacks */
    else
    {
        int chance;

        /* Spell attacks */
        v2 = borg_danger_aux2(i,y,x);

        /* Hack -- Under Stressful Situation.
         */
        if (time_this_panel > 1200 || c_t > 25000)
        {
            /* he might be stuck and could overflow */
            v2 = v2 / 5;
        }

        /* multipliers yeild some trouble when I am weak */
        if ((r_ptr->flags2 & RF2_MULTIPLY) && (borg_skill[BI_CLEVEL] < 20))
        {
            v2 = v2 + (v2 *12/10);
        }

        /* Friends yeild some trouble when I am weak */
        if ((r_ptr->flags1 & RF1_FRIENDS || r_ptr->flags1 & RF1_ESCORTS) &&
            (borg_skill[BI_CLEVEL] < 20))
        {
            v2 = v2 + (v2 *12/10);
        }

        /* Reduce danger from sleeping monsters */
        if ((!kill->awake) && (d > 1))
        {
            /* weaklings and should still fear */
            if (borg_skill[BI_CLEVEL] >= 10 )
            {
                 v2 = v2 / d;
            }
            else
            {
                /* only subract 10% of the danger */
                v2 = v2 *9/ 10;
            }
        }

        /* Reduce danger from sleeping monsters with the sleep 2 spell*/
        if (borg_sleep_spell_ii)
        {

            if  ( (d == 1) &&
                  (kill->awake) &&
                  (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
                  (!(r_ptr->flags1 & RF1_UNIQUE)) &&
                  (kill->level <= ((borg_skill[BI_CLEVEL] < 15)  ? borg_skill[BI_CLEVEL] : (((borg_skill[BI_CLEVEL]-10)/4)*3) + 10) ))
            {
                  v2 = v2 / 3;
            }
        }

        /* Reduce danger from sleeping monsters with the sleep 1,3 spell*/
        if (borg_sleep_spell)
        {
            v2 = v2 / (d+2);
        }
        /* Reduce danger from confused monsters */
        if (kill->confused)
        {
           v2 = v2 / 2;
        }
        /* Reduce danger from stunnned monsters  */
        if (kill->stunned)
        {
           v2 = v2 *10/13;
        }
        if (borg_confuse_spell)
        {
            v2 = v2 / 6;
        }

        /* Reduce danger from scared monsters */
        if (borg_fear_mon_spell)
        {
            v2 = v2 * 8/10;
        }
        if (kill->afraid)
        {
            v2 = v2 * 8/10;
        }

        /* Tweak danger based on the "alertness" of the monster */
        if (!kill->awake)
        {
            /* increase the danger for light sleepers */
            int inc = r_ptr->sleep + 5;

            v2 = v2 + (v2*inc/100);
        }

        if (!borg_full_damage)
        {
            /* reduce for frequency. */
            chance = (r_ptr->freq_inate + r_ptr->freq_spell)/2;
            if (chance < 11)
                v2 = (v2 / 5);
            else
            if (chance < 26)
                v2 = (v2 / 3);
            else
            if (chance < 51)
                v2 = ((v2 * 3) / 4) ;
        }

        /* Danger */
        if (v2)
        {
            /* Full power */
            r = q;

            /* Total danger */
            v2 = v2 * r / 10;
        }
    }


    /* Maximal danger */
    p = MAX(v1, v2);

    /* Result */
    return (p);
}


/*
 * Hack -- Calculate the "danger" of the given grid.
 *
 * Currently based on the physical power of nearby monsters, as well
 * as the spell power of monsters which can target the given grid.
 *
 * This function is extremely expensive, mostly due to the number of
 * times it is called, and also to the fact that it calls its helper
 * functions about thirty times each per call.
 *
 * We need to do more intelligent processing with the "c" parameter,
 * since currently the Borg does not realize that backing into a
 * hallway is a good idea, since as far as he can tell, many of
 * the nearby monsters can "squeeze" into a single grid.
 *
 * Note that we also take account of the danger of the "region" in
 * which the grid is located, which allows us to apply some "fear"
 * of invisible monsters and things of that nature.
 */
int borg_danger(int y, int x, int c)
{
    int i, p;

    /* do twice.  Once to get full damage and once to get partial. */
/* !FIX this is very slow.  I need to find a better way of doing this */
/*      perhaps I should calc both at the same time and pass back */
/*      the right one.  AJG */


    /* Base danger (from fear) */
    p = auto_fear_region[y/11][x/11] * c;

    /* Reduce this fear if GOI is up */
    if (borg_goi)
    {
        p = p / 4;
    }


    borg_full_damage = TRUE;
    /* Examine all the monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Collect danger from monster */
        p += borg_danger_aux(y, x, c, i);
    }
    borg_full_damage = FALSE;

    /* if I can't be killed in one round use probablilities */
    if (p < avoidance && p != 0)
    {
        /* Base danger (from fear) */
        p = auto_fear_region[y/11][x/11] * c;

        /* Reduce this fear if GOI is up */
        if (borg_goi)
        {
            p = p / 4;
        }

        /* Examine all the monsters */
        for (i = 1; i < auto_kills_nxt; i++)
        {
            auto_kill *kill = &auto_kills[i];

            /* Skip dead monsters */
            if (!kill->r_idx) continue;

            /* Collect danger from monster */
            p += borg_danger_aux(y, x, c, i);
        }
    }

    /* Return the danger */
    return (p > 2000 ? 2000 : p);
}




/*
 * Determine if the Borg is out of "crucial" supplies.
 *
 * Note that we ignore "restock" issues for the first several turns
 * on each level, to prevent repeated "level bouncing".
 */
cptr borg_restock(int depth)
{

    /* We are now looking at our preparedness */
    if ( -1 == borg_ready_morgoth)
        borg_ready_morgoth = 0;

    /* Always ready for the town */
    if (!depth) return ((cptr)NULL);

    /* Always spend time on a level unless 100*/
    if (c_t - auto_began < 100 && auto_depth != 100) return ((cptr)NULL);


    /*** Level 1 ***/

    /* Must have some lite */
    if (borg_skill[BI_CUR_LITE] < 1) return ("rs my_cur_lite");

    /* Must have "fuel" */
    if (borg_skill[BI_AFUEL] < 1) return ("rs amt_fuel");

    /* Must have "food" */
    if (borg_skill[BI_FOOD] < 1) return ("rs amt_food");

    /* Assume happy at level 1 */
    if (depth <= 1) return ((cptr)NULL);

    /*** Level 2 and 3 ***/

    /* Must have good lite */
    if (borg_skill[BI_CUR_LITE] < 2) return ("rs lite+1");

    /* Must have "fuel" */
    if (borg_skill[BI_AFUEL] < 3) return ("rs fuel+2");

    /* Must have "food" */
    if (borg_skill[BI_FOOD] < 3) return ("rs food+2");

    /* Must have "recall" */
    if (borg_skill[BI_RECALL] < 2) return ("rs recall");

    /* Assume happy at level 3 */
    if (depth <= 3) return ((cptr)NULL);

    /*** Level 3 to 5 ***/

    if (depth <= 5) return ((cptr)NULL);

    /*** Level 6 to 9 ***/

    /* Potions of Critical Wounds */
    if (!borg_skill[BI_RBLIND] &&
        !borg_skill[BI_RCONF] &&borg_skill[BI_ACCW] < 1) return ("rs cure crit");

    /* Assume happy at level 9 */
    if (depth <= 9) return ((cptr)NULL);

    /*** Level 10 - 19  ***/

    /* Must have "phase" */
    if (amt_phase < 1) return ("rs phase");

    /* Must have "cure" */
    if ((auto_max_level < 30) && borg_skill[BI_ACSW] + borg_skill[BI_ACCW] < 4) return ("rs cure");

    /* Must have "teleport" */
    if (borg_skill[BI_ATELEPORT] < 2) return ("rs teleport");

    /* Assume happy at level 19 */
    if (depth <= 19) return ((cptr)NULL);

    /*** Level 20 - 45  ***/

    /* Must have "cure" */
    if ((auto_max_level < 30) && borg_skill[BI_ACSW] + borg_skill[BI_ACCW] < 6) return ("rs cure");

    /* Must have "teleport" */
    if (borg_skill[BI_ATELEPORT] + borg_skill[BI_AESCAPE] < 4) return ("rs teleport");

    /* Assume happy at level 44 */
    if (depth <= 44) return ((cptr)NULL);

    /*** Level 46 - 99  ***/

    /* Must have "Heal" */
    if (borg_skill[BI_AHEAL] + amt_rod_heal + borg_skill[BI_AEZHEAL] < 1) return ("rs heal");

    /* Assume happy at level 99 */
    if (depth <= 99) return ((cptr)NULL);

    /*** Level 100  ***/

    /* Must have "Heal" */
    /* If I just got to dlevel 100 and low on heals, get out now. */
    if (c_t - auto_began < 10 && borg_skill[BI_AEZHEAL] < 15) return ("rs *heal*");

    /* Assume happy */
    return ((cptr)NULL);
}

/*
 * Determine if the Borg meets the "minimum" requirements for a level
 */
static cptr borg_prepared_aux(int depth)
{
    int iEntry, nEntrys;
    req_item* Req_item;
    static char ret_string[2000]; /* hack.  big ass string to return 'why things are bad' string */

    if ( -1 == borg_ready_morgoth)
        borg_ready_morgoth = 0;

    if (borg_king)
    {
        borg_ready_morgoth = 1;
        return ((cptr)NULL);
    }

    /* Always ready for the town */
    if (!depth) return ((cptr)NULL);

    borg_skill[BI_DEPTH] = depth;
    borg_skill[BI_CDEPTH] = auto_depth;

    nEntrys = n_req[auto_class];
    for (iEntry = 0; iEntry < nEntrys; iEntry++)
    {
        Req_item = &borg_required_item[auto_class][iEntry];
        if (Req_item->depth > depth)
            break;




        if (Req_item->item == -1)
        {
                if (!borg_calc_formula(formula[Req_item->number]))
                {
                    /* Some formulas from borg.txt deal with clevel and
                     * depth of preparedness.  The option borg_plays_risky
                     * will remove those requirements.  So those formulas
                     * need to be removed here if that flag is set.
                     */
                    if (borg_plays_risky &&
                        strstr(borg_prt_formula(formula[Req_item->number]), "CLEVEL"))
                    {
                        continue;
                    }

                    if (borg_slow_return)
                        sprintf(ret_string,
                            "Formula [%s] failed",
                            borg_prt_formula(formula[Req_item->number]));
                    else
                        sprintf(ret_string, "Formula failed FORMULA%03d", Req_item->number);
                    return ret_string;
                }
                continue;
            }
        if (borg_has[Req_item->item] < Req_item->number)
        {
            /* Some formulas from borg.txt deal with clevel and
             * depth of preparedness.  The option borg_plays_risky
             * will remove those requirements.  So those formulas
             * need to be removed here if that flag is set.
             */
             if (borg_plays_risky &&
                 strstr(borg_prt_item(Req_item->item), "CLEVEL"))
             {
                  continue;
             }

            sprintf(ret_string, "Number of %s < %d",
                    borg_prt_item(Req_item->item), Req_item->number);
            return ret_string;
        }
    }

    return ((cptr)NULL);
}

/*
 * Determine if the Borg meets the "minimum" requirements for a level
 */
static cptr borg_prepared_aux2(int depth)
{
    if ( -1 == borg_ready_morgoth)
        borg_ready_morgoth = 0;
    if (borg_king)
        {
            borg_ready_morgoth = 1;
            return ((cptr)NULL);
        }

    /* Always ready for the town */
    if (!depth) return ((cptr)NULL);


    /*** Essential Items for Level 1 ***/

    /* Require lite (any) */
    if (borg_skill[BI_CUR_LITE] < 1) return ("1 Lite");

    /* Require food */
    if (borg_skill[BI_FOOD] < 5) return ("5 Food");

    /* Usually ready for level 1 */
    if (depth <= 1) return ((cptr)NULL);


    /*** Essential Items for Level 2 ***/

    /* Require lite (radius two) */
    if (borg_skill[BI_CUR_LITE] < 2) return ("2 Lite");

    /* Require fuel */
    if (borg_skill[BI_AFUEL] < 5) return ("5 Fuel");

    /* Require recall */
    if (borg_skill[BI_RECALL] < 1) return ("1 recall");

    if (!borg_plays_risky)
    {
        /* Require 30 hp */
        if (auto_mhp < 30) return ("30 hp");
    }

    /* Usually ready for level 2 */
    if (depth <= 2) return ((cptr)NULL);

    /*** Essential Items for Level 3 and 4 ***/

    if (!borg_plays_risky)
    {
        /* class specific requirement */
        switch (auto_class)
        {
            case CLASS_WARRIOR:
                if (auto_mhp < 50) return ("50 hp");
                if (auto_max_level < 4) return ("4 clevel");
                break;
            case CLASS_ROGUE:
                if (auto_mhp < 50) return ("50 hp");
                if (auto_max_level < 8) return ("8 clevel");
                break;
            case CLASS_PRIEST:
                if (auto_mhp < 40) return ("40 hp");
                if (auto_max_level < 9) return ("9 level");
                break;
            case CLASS_PALADIN:
                if (auto_mhp < 50) return ("50 hp");
                if (auto_max_level < 4) return ("4 clevel");
                break;
            case CLASS_RANGER:
                if (auto_mhp < 50) return ("50 hp");
                if (auto_max_level < 4) return ("4 clevel");
                break;
            case CLASS_MAGE:
                if (auto_mhp < 60) return ("60 hp");
                if (auto_max_level < 11) return ("11 clevel");
                break;
        }
    }
    /* Scrolls of Word of Recall */
    if (borg_skill[BI_RECALL] < 3) return ("3 recall");

    /* Potions of Cure Serious Wounds */
    if ((auto_max_level < 30) && borg_skill[BI_ACSW] + borg_skill[BI_ACCW] < 2) return ("2 cure");
#if 0
    /* Scrolls of Identify */
    if (amt_ident < 2 && (auto_depth)) return ("2 ident");
#endif
    /* Usually ready for level 3 and 4 */
    if (depth <= 4) return ((cptr)NULL);


    /*** Essential Items for Level 5 to 9 ***/

    if (!borg_plays_risky)
    {
        /* class specific requirement */
        if (auto_depth)
        {
            switch (auto_class)
            {
                case CLASS_WARRIOR:
                    if (auto_mhp < 60) return ("60 hp");
                    if (auto_max_level < 6) return ("6 clevel");
                    break;
                case CLASS_ROGUE:
                    if (auto_mhp < 60) return ("60 hp");
                    if (auto_max_level < 10) return ("10 clevel");
                    break;
                case CLASS_PRIEST:
                    if (auto_mhp < 60) return ("60 hp");
                    if (auto_max_level < 15) return ("15 clevel");
                    break;
                case CLASS_PALADIN:
                    if (auto_mhp < 60) return ("60 hp");
                    if (auto_max_level < 6) return ("6 clevel");
                    break;
                case CLASS_RANGER:
                    if (auto_mhp < 60) return ("60 hp");
                    if (auto_max_level < 6) return ("6 clevel");
                    break;
                case CLASS_MAGE:
                    if (auto_mhp < 80) return ("80 hp");
                    if (auto_max_level < 15) return ("15 level");
                    break;
            }
        }
    }
    /* Scrolls of Word of Recall */
    if (borg_skill[BI_RECALL] < 4) return ("4 recalls");

#if 0
    /* Scrolls of Identify */
    if (amt_ident < 5 && (auto_depth)) return ("5 idents");
#endif
    /* Potions of Cure Serious/Critical Wounds */
    if ((auto_max_level < 30) && borg_skill[BI_ACSW] + borg_skill[BI_ACCW] < 5) return ("5 cures");

    /* Usually ready for level 5 to 9 */
    if (depth <= 9) return ((cptr)NULL);


    /*** Essential Items for Level 10 to 19 ***/


    /* Escape or Teleport */
    if (borg_skill[BI_ATELEPORT] + borg_skill[BI_AESCAPE] < 2) return ("2 teleports");

    if (!borg_plays_risky)
    {
        /* class specific requirement */
        switch (auto_class)
        {
            case CLASS_WARRIOR:
                if (auto_max_level < (depth - 4) && depth <= 19)
                    return ("dlevel - 4 >= clevel");
                break;
            case CLASS_ROGUE:
                if (auto_max_level < depth && depth <= 19) return ("dlevel >= clevel" );
                break;
            case CLASS_PRIEST:
                if (auto_max_level < depth && depth <= 19) return ("dlevel >= clevel" );
                break;
            case CLASS_PALADIN:
                if (auto_max_level < depth && depth <= 19) return ("dlevel >= clevel" );
                break;
            case CLASS_RANGER:
                if (auto_max_level < depth && depth <= 19) return ("dlevel >= clevel" );
                break;
            case CLASS_MAGE:
                if (auto_max_level < (depth + 5) && auto_max_level <= 28)
                    return ("dlevel + 5 > = clevel" );
                break;
        }
    }
#if 0
    /* Identify */
    if (amt_ident < 10) return ("ident10");
#endif
    /* Potions of Cure Critical Wounds */
    if ((auto_max_level < 30) && borg_skill[BI_ACCW] < 5) return ("cure crit5");

    /* See invisible */
    /* or telepathy */
    if ((!borg_skill[BI_SINV] && !borg_skill[BI_ESP])) return ("See Invis : ESP");

    /* Usually ready for level 10 to 19 */
    if (depth <= 19) return ((cptr)NULL);


    /*** Essential Items for Level 20 ***/


    /* Free action */
    if (!borg_skill[BI_FRACT]) return ("FA");

    /* ready for level 20 */
    if (depth <= 20) return ((cptr)NULL);


    /*** Essential Items for Level 25 ***/

    /* must have fire + 2 other basic resists */
    if (!borg_skill[BI_RFIRE] && !weapon_swap_resist_fire &&
        !armour_swap_resist_fire) return ("RF");
    {
        int basics = borg_skill[BI_RACID] + borg_skill[BI_RCOLD] + borg_skill[BI_RELEC];

        if (basics < 2) return ("basic resist2");
    }
    /* have some minimal stats */
    if (auto_stat[A_STR] < 7) return ("low STR");

    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (auto_stat[A_INT] < 7) return ("low INT");
    }
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (auto_stat[A_WIS] < 7) return ("low WIS");
    }
    if (auto_stat[A_DEX] < 7) return ("low DEX");
    if (auto_stat[A_CON] < 7) return ("low CON");

    if (!borg_plays_risky)
    {
        /* class specific requirement */
        switch (auto_class)
        {
            case CLASS_WARRIOR:
                if (auto_max_level < (depth + 5) && auto_max_level <= 38)
                    return ("dlevel + 5 >= clevel" );
                break;
            case CLASS_ROGUE:
                if (auto_max_level < (depth + 10) && auto_max_level <= 43)
                    return ("dlevel + 10 >= clevel" );
                break;
            case CLASS_PRIEST:
                if (auto_max_level < (depth + 13) && auto_max_level <= 46)
                    return ("dlevel + 13 >= clevel" );
                break;
            case CLASS_PALADIN:
                if (auto_max_level < (depth + 7) && auto_max_level <= 40)
                    return ("dlevel + 7 >= clevel" );
                break;
            case CLASS_RANGER:
                if (auto_max_level < (depth + 8) && auto_max_level <= 41 && auto_max_level > 28)
                    return ("dlevel + 8 >= clevel" );
                break;
            case CLASS_MAGE:
                if (auto_max_level < (depth + 8) && auto_max_level <= 38)
                    return ("dlevel + 8 >= clevel" );
                if (((auto_max_level-38) * 2 + 30) < depth &&
                    auto_max_level <= 44 &&
                    auto_max_level > 38)
                    return ("(clevel-38)*2+30 < dlevel" );
                break;
        }
    }

    /* Ready for level 25 */
    if (depth <= 25) return ((cptr)NULL);


/*** Essential Items for Level 25 to 39 ***/


    /* Escape and Teleport */
    if (borg_skill[BI_ATELEPORT] < 2) return ("teleport2");
    if (borg_skill[BI_ATELEPORT] + borg_skill[BI_AESCAPE] < 6) return ("tell&esc6");

    /* Cure Critical Wounds */
    if ((auto_max_level < 30) && (borg_skill[BI_ACCW] + borg_skill[BI_ACSW]) < 10) return ("cure10");

    /* Ready for level 33 */
    if (depth <= 33) return ((cptr)NULL);

    /* Minimal level */
    if (auto_max_level < 40) return ("level 40");

    /* Usually ready for level 20 to 39 */
    if (depth <= 39) return ((cptr)NULL);



/*** Essential Items for Level 40 to 45 ***/

    /* All Basic resistance & poison*/
    if (!borg_skill[BI_RCOLD] && !weapon_swap_resist_cold &&
        !armour_swap_resist_cold) return ("RC");
    if (!borg_skill[BI_RELEC] && !weapon_swap_resist_elec &&
        !armour_swap_resist_elec) return ("RE");
    if (!borg_skill[BI_RELEC] && !weapon_swap_resist_acid &&
        !armour_swap_resist_acid) return ("RA");
    if (!borg_skill[BI_RPOIS] && !weapon_swap_resist_pois &&
        !armour_swap_resist_pois) return ("RPois");

    if (auto_stat[A_STR] < 16) return ("low STR");

    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (auto_stat[A_INT] < 16) return ("low INT");
    }
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (auto_stat[A_WIS] < 16) return ("low WIS");
    }
    if (auto_stat[A_DEX] < 16) return ("low DEX");
    if (auto_stat[A_CON] < 16) return ("low CON");

    if (!borg_plays_risky)
    {
        /* Minimal level */
        if (auto_max_level < 45) return ("level45");
    }

    if (depth <= 45) return ((cptr)NULL);


/*** Essential Items for Level 46 to 55 ***/

    /*  Must have +5 speed after level 46 */
    if (borg_skill[BI_SPEED] < 115) return ("+5 speed");

    /* Potions of heal */
    if (borg_skill[BI_AHEAL] < 1 && (borg_skill[BI_AEZHEAL] < 1) ) return ("1heal");

    if (!borg_skill[BI_RCONF] && !weapon_swap_resist_conf &&
        !armour_swap_resist_conf)  return ("RConf");

    if (!borg_plays_risky)
    {
        /* Minimal hitpoints */
        if (auto_mhp < 500) return ("HP 500");
    }

    /* High stats XXX XXX XXX */
    if (auto_stat[A_STR] < 18+40) return ("low STR");

    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (auto_stat[A_INT] < 18+100) return ("low INT");
    }
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (auto_stat[A_WIS] < 18+100) return ("low WIS");
    }
    if (auto_stat[A_DEX] < 18+60) return ("low DEX");
    if (auto_stat[A_CON] < 18+60) return ("low CON");

    /* Hold Life */
    /* after you max out you are pretty safe from drainers.*/
    if ( (auto_max_level == 50) && (depth <= 60))
        return ((cptr)NULL);
    else
        if ( (!borg_skill[BI_HLIFE] && !weapon_swap_hold_life &&
        !armour_swap_hold_life) && (auto_max_level < 50) ) return ("hold life");

    /* Usually ready for level 46 to 55 */
    if (depth <= 55) return ((cptr)NULL);

/*** Essential Items for Level 55 to 59 ***/

    /* Potions of heal */
    if (borg_skill[BI_AHEAL] < 2 && borg_skill[BI_AEZHEAL] < 1) return ("2heal");

    /* Resists */
    if (!borg_skill[BI_RBLIND] && !weapon_swap_resist_blind &&
        !armour_swap_resist_blind) return ("RBlind");

    /* Must have resist nether */
    if (borg_skill[BI_AXGOI])
        {
            /* this mage will not require RNethr to dive */
        }
        else
        {
            if (!borg_skill[BI_RNTHR] && !weapon_swap_resist_neth &&
            !armour_swap_resist_neth) return ("RNeth");
        }

    /* Telepathy, better have it by now */
    if (!borg_skill[BI_ESP]) return ("ESP");

    /* Usually ready for level 55 to 59 */
    if (depth <= 59) return ((cptr)NULL);



/*** Essential Items for Level 61 to 80 ***/

    /* Must have +10 speed */
    if (borg_skill[BI_SPEED] < 120) return ("+10 speed");


    /* Resists */
    if (borg_skill[BI_AXGOI])
    {
        /* This mage will not require Chaos to dive */
    }
    else
    {
        if (!borg_skill[BI_RKAOS] && !weapon_swap_resist_chaos &&
        !armour_swap_resist_chaos) return ("RChaos");
    }
    if (!borg_skill[BI_RDIS] && !weapon_swap_resist_disen &&
        !armour_swap_resist_disen) return ("RDisen");

    /* Usually ready for level 61 to 80 */
    if (depth <= 80) return ((cptr)NULL);

/*** Essential Items for Level 81-85 ***/
    /* Minimal Speed */
    if (borg_skill[BI_SPEED] < 130) return ("+20 Speed");

    /* Usually ready for level 81 to 85 */
    if (depth <= 85) return ((cptr)NULL);


/*** Essential Items for Level 86-99 ***/


    /* Usually ready for level 86 to 99 */
    if (depth <= 99) return ((cptr)NULL);

/*** Essential Items for Level 100 ***/

    /* must have lots of restore mana to go after MORGOTH */
    if (!borg_king)
    {
        if ((auto_msp > 100) && (amt_mana < 15)) return ("15ResMana");

        /* must have lots of heal */
        if (amt_pot_heal < 15 &&
            (auto_class == CLASS_MAGE || auto_class == CLASS_PRIEST)) return ("15Heal");
        else if (amt_pot_heal < 25) return ("25Heal");

        /* must have lots of ez-heal */
        if (borg_skill[BI_AEZHEAL] < 15) return ("15EZHeal");

        /* must have lots of speed */
        if (borg_skill[BI_ASPEED] < 15) return ("15Speed");

      }

    /* Its good to be the king */
    if (depth <= 127) return ((cptr)NULL);

    /* all bases covered */
    return ((cptr)NULL);
}


/*
 * Determine if the Borg is "prepared" for the given level
 *
 * This routine does not help him decide how to get ready for the
 * given level, so it must work closely with "borg_power()".
 *
 * Note that we ignore any "town fear", and we allow fear of one
 * level up to and including the relevant depth.
 *
 * This now returns a string with the reason you are not prepared.
 *
 */
cptr borg_prepared(int depth)
{
    cptr reason;


    /* -1 is unknown. */
    borg_ready_morgoth = -1;

    /* Town and First level */
    if (depth == 1) return ((cptr)NULL);

    /* Not prepared if I need to restock */
    if ((reason = borg_restock(depth))) return (reason);

    /* Must meet minimal requirements */
    if (borg_uses_calcs)
    {
        if ((reason = borg_prepared_aux(depth))) return (reason);
    }

    if (!borg_uses_calcs)
    {
        if ((reason = borg_prepared_aux2(depth)))   return (reason);
    }


    /* Once Morgoth is dead */
    if (borg_king)
    {
        return ((cptr)NULL);
    }

    /* Always okay from town */
    if (!auto_depth) return ((cptr)NULL);

    /* check to make sure the borg does not go below where 2 living */
    /* uniques are. */

    if (auto_max_depth <= 98)
    {
          int numb_live_unique = 0, i;
          int living_unique_index = 0;

          auto_kill *kill;
          monster_race *r_ptr;

          /* BIG HACK, should check to make sure he has seen the unique. */
          /* !FIX change this to use the 'list of uniques (|) command AJG */
          for (i = 1; i < z_info->r_max-1; i++)
          {
              /* If any have been killed it is not a live unique */
              if (auto_race_death[i] != 0) continue;

              r_ptr = &r_info[i];

              /* Skip non-monsters */
              if (!r_ptr->name) continue;

              /* Skip non-uniques */
              if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

              /* skip if deeper than dlevel */
              if ( r_ptr->level > depth ) break;

              numb_live_unique++;
              if (i < living_unique_index ||
                  living_unique_index == 0) living_unique_index = i;
              continue;
          }

          if (numb_live_unique < 3 || borg_plays_risky)
          {
              return ((cptr)NULL);
          }
      /* scum for the uniques and report */
      r_ptr = &r_info[living_unique_index];
      if (borg_scums_uniques && auto_max_level >= 35) auto_scum = TRUE;
#if 0
      return ("Living uniques >= 3");
#endif
      return (format("Must kill %s.", r_name + r_ptr->name));

    }
    else
      /* check to make sure the borg does not go to level 100 */
      /* unless all the uniques are dead. */
    {
          int numb_live_unique = 0, i;
          auto_kill *kill;
          monster_race *r_ptr;
          int living_unique_index = 0;


          /* BIG HACK, should check to make sure he has seen the unique. */
          /* !FIX change this to use the 'list of uniques (|) command AJG */
          for (i = 1; i < z_info->r_max-1; i++)
          {
              /* If any have been killed it is not a live unique */
              if (auto_race_death[i] != 0) continue;

              r_ptr = &r_info[i];

              /* Skip non-monsters */
              if (!r_ptr->name) continue;

              /* Skip non-uniques */
              if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

              /* skip if deeper than dlevel */
              if ( r_ptr->level >= depth ) break;

              /* skip the check on Questors */
              if (r_ptr->flags1 & RF1_QUESTOR) continue;

              numb_live_unique++;
              living_unique_index = i;
              continue;
          }

          if (numb_live_unique < 1)
          {
              if (depth > 99) borg_ready_morgoth = 1;
              return ((cptr)NULL);
          }
          /* Under special cases allow the borg to dive to 99 then quickly
           * get his butt to dlevel 98
           */
          if (auto_max_depth == 99 && depth <= 98 &&
              (rand_int(100) < 3 ||
               borg_prayer_okay_fail(4, 3, 20) ||
               borg_spell_okay_fail(5, 2, 20)))
          {
              if (borg_scums_uniques && auto_max_level >= 35) auto_scum = TRUE;
              return ((cptr)NULL);
          }
      /* Report */
      r_ptr = &r_info[living_unique_index];

      if (borg_scums_uniques && auto_max_level >= 35) auto_scum = TRUE;
      return (format("%s still alive!", r_name + r_ptr->name));

    }

}

/*
 * Initialize this file
 */
void borg_init_4(void)
{
    /* Do nothing? */
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
