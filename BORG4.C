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
    int         wield_skill;

    int         extra_blows = 0;

    int         extra_shots = 0;
    int         extra_might = 0;

    auto_item       *item;

    /* Clear the armor class */
    my_ac = 0;

    /* Clear the bonuses */
    my_to_hit = my_to_dam = my_to_ac = 0;


    /* Start with a blow per turn */
    my_num_blow = 2;

    /* Start with a single shot per turn */
    my_num_fire = 1;


    /* Assume normal other */
    my_other = 0;


    /* Reset the "ammo" tval to darts by default */
    my_ammo_tval = 0;

    /* Reset the "ammo" sides for darts*/
    my_ammo_sides = 4;

    /* Reset the shooting power */
    my_ammo_power = 0;

    /* Reset the shooting range */
    my_ammo_range = 0;

    my_speed = 110;

    /* Clear all the flags */
    my_see_inv = FALSE;
    my_teleport = FALSE;
    my_free_act = FALSE;
    my_slow_digest = FALSE;
    my_aggravate = FALSE;
    my_regenerate = FALSE;
    my_ffall = FALSE;
    my_hold_life = FALSE;
    my_telepathy = FALSE;
    my_inviso = FALSE;
    my_lite = FALSE;

    my_slay_animal = FALSE;
    my_slay_evil = FALSE;
    my_slay_undead = FALSE;
    my_slay_demon = FALSE;
    my_slay_orc = FALSE;
    my_slay_troll = FALSE;
    my_slay_giant = FALSE;
    my_slay_dragon = FALSE;
    my_kill_dragon = FALSE;
    my_impact = FALSE;
    my_brand_acid = FALSE;
    my_brand_elec = FALSE;
    my_brand_fire = FALSE;
    my_brand_cold = FALSE;
    my_stunning = FALSE;

    my_immune_acid = FALSE;
    my_immune_elec = FALSE;
    my_immune_fire = FALSE;
    my_immune_cold = FALSE;

    my_resist_acid = 0;
    my_resist_elec = 0;
    my_resist_fire = 0;
    my_resist_cold = 0;
    my_resist_pois = 0;

    my_resist_conf = 0;
    my_resist_sound = 0;
    my_resist_lite = 0;
    my_resist_dark = 0;
    my_resist_chaos = 0;
    my_resist_disen = 0;
    my_resist_shard = 0;
    my_resist_nexus = 0;
    my_resist_blind = 0;
    my_resist_neth = 0;
    my_resist_fear = 0;

    my_sustain_str = FALSE;
    my_sustain_int = FALSE;
    my_sustain_wis = FALSE;
    my_sustain_con = FALSE;
    my_sustain_dex = FALSE;
    my_sustain_chr = FALSE;

    wield_skill = weapon_skill(&inventory[INVEN_WIELD]);

    /* Base infravision (purely racial) */
    my_see_infra = rb_ptr->infra;

    /* Base skill -- disarming */
    my_skill_dis = rb_ptr->r_dis + cb_ptr->c_dis;

    /* Base skill -- magic devices */
    my_skill_dev = rb_ptr->r_dev + cb_ptr->c_dev;

    /* Base skill -- saving throw */
    my_skill_sav = rb_ptr->r_sav + cb_ptr->c_sav;

    /* Base skill -- stealth */
    my_skill_stl = rb_ptr->r_stl + cb_ptr->c_stl;

    /* Base skill -- searching ability */
    my_skill_srh = rb_ptr->r_srh + cb_ptr->c_srh;

    /* Base skill -- searching frequency */
    my_skill_fos = rb_ptr->r_fos + cb_ptr->c_fos;

    /* Base skill -- combat (normal) */
    my_skill_thn = rb_ptr->r_thn[wield_skill] + cb_ptr->c_thn[wield_skill];

    /* Base skill -- combat (shooting) */
    my_skill_thb = rb_ptr->r_thb + cb_ptr->c_thb;

    /* Base skill -- combat (throwing) */
    my_skill_tht = rb_ptr->r_thb + cb_ptr->c_thb;

    /* Base skill -- Digging */
    my_skill_dig = 0;


    /* Elf */
    if (auto_race == RACE_ELF) my_resist_lite = 1;

    /* Hobbit */
    if (auto_race == RACE_HOBBIT) my_sustain_dex = TRUE;

    /* Gnome */
    if (auto_race == RACE_GNOME) my_free_act = TRUE;

    /* Dwarf */
    if (auto_race == RACE_DWARF) my_resist_blind = 1;

    /* Half-Orc */
    if (auto_race == RACE_HALF_ORC) my_resist_dark = 1;

    /* Half-Troll */
    if (auto_race == RACE_HALF_TROLL) my_sustain_str = TRUE;

    /* Dunadan */
    if (auto_race == RACE_DUNADAN) my_sustain_con = TRUE;

    /* High Elf */
    if (auto_race == RACE_HIGH_ELF) my_resist_lite = 1;
    if (auto_race == RACE_HIGH_ELF) my_see_inv = TRUE;

    /* Pixie */
    if (auto_race == RACE_PIXIE) my_ffall = TRUE;
    if (auto_race == RACE_PIXIE && auto_level > 40) my_telepathy = TRUE;

	/* Yeek */

	/* Crystal Dragon (dragons get a bunch, eh?)*/
    if (auto_race == RACE_CRYSTALDRAG) my_free_act = TRUE;
    if (auto_race == RACE_CRYSTALDRAG) my_ffall = TRUE;
    if (auto_race == RACE_CRYSTALDRAG) my_resist_shard = 1;
    if (auto_race == RACE_CRYSTALDRAG && auto_level > 40) my_resist_shard = 1;
    if (auto_race == RACE_CRYSTALDRAG) my_resist_fear = TRUE;

	/* Copper Dragon */
    if (auto_race == RACE_COPPERDRAG) my_free_act = TRUE;
    if (auto_race == RACE_COPPERDRAG) my_ffall = TRUE;
    if (auto_race == RACE_COPPERDRAG) my_resist_disen =1;
    if (auto_race == RACE_COPPERDRAG && auto_level > 40) my_resist_disen = 2;
    if (auto_race == RACE_COPPERDRAG) my_resist_fear = TRUE;
    if (auto_race == RACE_COPPERDRAG) my_regenerate = TRUE;

	/* Bronze Dragon */
    if (auto_race == RACE_BRONZEDRAG) my_ffall = TRUE;
    if (auto_race == RACE_BRONZEDRAG) my_resist_conf = 1;
    if (auto_race == RACE_BRONZEDRAG && auto_level > 40) my_resist_conf = 2;
    if (auto_race == RACE_BRONZEDRAG) my_resist_fear = TRUE;
    if (auto_race == RACE_BRONZEDRAG) my_regenerate = TRUE;

	/* Gold [Brass] Dragon */
    if (auto_race == RACE_GOLDDRAG) my_ffall = TRUE;
    if (auto_race == RACE_GOLDDRAG) my_resist_sound = 1;
    if (auto_race == RACE_GOLDDRAG && auto_level > 40) my_resist_sound = 2;
    if (auto_race == RACE_GOLDDRAG) my_resist_fear = TRUE;
    if (auto_race == RACE_GOLDDRAG) my_regenerate = TRUE;

	/* Pseudo Dragon */
    if (auto_race == RACE_PSEUDODRAG) my_ffall = TRUE;
    if (auto_race == RACE_PSEUDODRAG) my_resist_lite = 1;
    if (auto_race == RACE_PSEUDODRAG) my_resist_dark = 1;
    if (auto_race == RACE_PSEUDODRAG) my_resist_blind = 1;
    if (auto_race == RACE_PSEUDODRAG) my_resist_fear = TRUE;
    if (auto_race == RACE_PSEUDODRAG) my_regenerate = TRUE;
    if (auto_race == RACE_PSEUDODRAG) my_see_inv = TRUE;
    if (auto_race == RACE_PSEUDODRAG && auto_level >= 40) my_inviso = TRUE;

	/* Multi-hued Dragon */
    if (auto_race == RACE_MULTIHUEDDRAG) my_ffall = TRUE;
    if (auto_race == RACE_MULTIHUEDDRAG) my_resist_fear = TRUE;
    if (auto_race == RACE_MULTIHUEDDRAG) my_regenerate = TRUE;
    if (auto_race == RACE_MULTIHUEDDRAG) my_resist_acid = 1;
    if (auto_race == RACE_MULTIHUEDDRAG) my_resist_elec = 1;
    if (auto_race == RACE_MULTIHUEDDRAG) my_resist_cold = 1;
    if (auto_race == RACE_MULTIHUEDDRAG) my_resist_fire = 1;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 25) my_resist_acid = 2;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 30) my_resist_elec = 2;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 35) my_resist_cold = 2;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 40) my_resist_fire = 2;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 45) my_resist_pois = 1;


    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) my_stat_add[i] = 0;

    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Affect stats */
        if (item->flags1 & TR1_STR) my_stat_add[A_STR] += item->pval;
        if (item->flags1 & TR1_INT) my_stat_add[A_INT] += item->pval;
        if (item->flags1 & TR1_WIS) my_stat_add[A_WIS] += item->pval;
        if (item->flags1 & TR1_DEX) my_stat_add[A_DEX] += item->pval;
        if (item->flags1 & TR1_CON) my_stat_add[A_CON] += item->pval;
        if (item->flags1 & TR1_CHR) my_stat_add[A_CHR] += item->pval;

        /* various slays */
        if (item->flags1 & TR1_SLAY_ANIMAL) my_slay_animal = TRUE;
        if (item->flags1 & TR1_SLAY_EVIL)   my_slay_evil = TRUE;

        if (item->flags1 & TR1_SLAY_UNDEAD) my_slay_undead = TRUE;
        if (item->flags1 & TR1_SLAY_DEMON)  my_slay_demon = TRUE;
        if (item->flags1 & TR1_SLAY_ORC)    my_slay_orc = TRUE;
        if (item->flags1 & TR1_SLAY_TROLL)  my_slay_troll = TRUE;
        if (item->flags1 & TR1_SLAY_GIANT)  my_slay_giant = TRUE;
        if (item->flags1 & TR1_SLAY_DRAGON) my_slay_dragon = TRUE;
        if (item->flags1 & TR1_KILL_DRAGON) my_kill_dragon = TRUE;
        if (item->flags3 & TR3_IMPACT)      my_impact = TRUE;
        if (item->flags1 & TR1_BRAND_ACID)  my_brand_acid = TRUE;
        if (item->flags1 & TR1_BRAND_ELEC)  my_brand_elec = TRUE;
        if (item->flags1 & TR1_BRAND_FIRE)  my_brand_fire = TRUE;
        if (item->flags1 & TR1_BRAND_COLD)  my_brand_cold = TRUE;

        /* Affect infravision */
        if (item->flags1 & TR1_INFRA) my_see_infra += item->pval;

        /* Affect stealth */
        if (item->flags1 & TR1_STEALTH) my_skill_stl += item->pval;

        /* Affect searching ability (factor of five) */
        if (item->flags1 & TR1_SEARCH) my_skill_srh += (item->pval * 5);

        /* Affect searching frequency (factor of five) */
        if (item->flags1 & TR1_SEARCH) my_skill_fos += (item->pval * 5);

        /* Affect digging (factor of 20) */
        if (item->flags1 & TR1_TUNNEL) my_skill_dig += (item->pval * 20);

        /* Affect speed */
        if (item->flags1 & TR1_SPEED) my_speed += item->pval;

        /* Boost shots */
        if (item->flags1 & TR1_SHOTS) extra_shots++;

        /* Boost might */
        if (item->flags1 & TR1_MIGHT) extra_might++;

        /* Various flags */
        if (item->flags3 & TR3_SLOW_DIGEST) my_slow_digest = TRUE;
        if (item->flags3 & TR3_AGGRAVATE) my_aggravate = TRUE;
        if (item->flags3 & TR3_TELEPORT) my_teleport = TRUE;
        if (item->flags3 & TR3_REGEN) my_regenerate = TRUE;
        if (item->flags3 & TR3_TELEPATHY) my_telepathy = TRUE;
        if (item->flags3 & TR3_INVIS) my_inviso = TRUE;
        if (item->flags3 & TR3_LITE) my_lite = TRUE;
        if (item->flags3 & TR3_SEE_INVIS) my_see_inv = TRUE;
        if (item->flags3 & TR3_FEATHER) my_ffall = TRUE;
        if (item->flags3 & TR3_FREE_ACT) my_free_act = TRUE;
        if (item->flags3 & TR3_HOLD_LIFE) my_hold_life = TRUE;

        /* Immunity flags */
        /* if you are immune you automaticly resist */
        if (item->flags2 & TR2_IM_FIRE)
        {
            my_immune_fire = TRUE;
            my_resist_fire +=10;
        }
        if (item->flags2 & TR2_IM_ACID)
        {
            my_immune_acid = TRUE;
            my_resist_acid += 10;
        }
        if (item->flags2 & TR2_IM_COLD)
        {
            my_immune_cold = TRUE;
            my_resist_cold += 10;
        }
        if (item->flags2 & TR2_IM_ELEC)
        {
            my_immune_elec = TRUE;
            my_resist_elec += 10;
        }

        /* Resistance flags */
        if (item->flags2 & TR2_RES_ACID) my_resist_acid ++;
        if (item->flags2 & TR2_RES_ELEC) my_resist_elec ++;
        if (item->flags2 & TR2_RES_FIRE) my_resist_fire ++;
        if (item->flags2 & TR2_RES_COLD) my_resist_cold ++;
        if (item->flags2 & TR2_RES_POIS) my_resist_pois ++;
        if (item->flags2 & TR2_RES_CONFU) my_resist_conf ++;
        if (item->flags2 & TR2_RES_SOUND) my_resist_sound ++;
        if (item->flags2 & TR2_RES_LITE) my_resist_lite ++;
        if (item->flags2 & TR2_RES_DARK) my_resist_dark ++;
        if (item->flags2 & TR2_RES_CHAOS) my_resist_chaos ++;
        if (item->flags2 & TR2_RES_DISEN) my_resist_disen ++;
        if (item->flags2 & TR2_RES_SHARD) my_resist_shard ++;
        if (item->flags2 & TR2_RES_NEXUS) my_resist_nexus ++;
        if (item->flags2 & TR2_RES_BLIND) my_resist_blind ++;
        if (item->flags2 & TR2_RES_NETHR) my_resist_neth ++;

        /* Sustain flags */
        if (item->flags2 & TR2_SUST_STR) my_sustain_str = TRUE;
        if (item->flags2 & TR2_SUST_INT) my_sustain_int = TRUE;
        if (item->flags2 & TR2_SUST_WIS) my_sustain_wis = TRUE;
        if (item->flags2 & TR2_SUST_DEX) my_sustain_dex = TRUE;
        if (item->flags2 & TR2_SUST_CON) my_sustain_con = TRUE;
        if (item->flags2 & TR2_SUST_CHR) my_sustain_chr = TRUE;

        /* Modify the base armor class */
        my_ac += item->ac;

        /* Apply the bonuses to armor class */
        my_to_ac += item->to_a;

        /* Hack -- do not apply "weapon" bonuses */
        if (i == INVEN_WIELD) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (i == INVEN_BOW) continue;

        /* Apply the bonuses to hit/damage */
        my_to_hit += item->to_h;
        my_to_dam += item->to_d;
    }

    /* Temporary Resists */
    if (my_oppose_acid) my_resist_acid ++;
    if (my_oppose_elec) my_resist_elec ++;
    if (my_oppose_cold) my_resist_cold ++;
    if (my_oppose_fire) my_resist_fire ++;
    if (my_oppose_pois) my_resist_pois ++;
    if (borg_prot_from_evil) my_resist_neth ++;

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
        my_stat_ind[i] = ind;
    }



    /* Bloating slows the player down (a little) */
    if (do_gorged) my_speed -= 10;



    /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
    my_to_ac += ((int)(adj_dex_ta[my_stat_ind[A_DEX]]) - 128);
    my_to_dam += ((int)(adj_str_td[my_stat_ind[A_STR]]) - 128);
    my_to_hit += ((int)(adj_dex_th[my_stat_ind[A_DEX]]) - 128);
    my_to_hit += ((int)(adj_str_th[my_stat_ind[A_STR]]) - 128);


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

    /* It is hard to carholdry a heavy bow */
    if (hold < item->weight / 10)
    {
        /* Hard to wield a heavy bow */
        my_to_hit += 2 * (hold - item->weight / 10);
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
            if (auto_level >= 20) my_num_fire++;

			/* Extra shot at level 40 */
            if (auto_level >= 40) my_num_fire++;
		}

        /* Add in the "bonus shots" */
        my_num_fire += extra_shots;

        /* Require at least one shot */
        if (my_num_fire < 1) my_num_fire = 1;
    }




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

    /* It is hard to hold a heavy weapon */
    if (hold < item->weight / 10)
    {
        /* Hard to wield a heavy weapon */
        my_to_hit += 2 * (hold - item->weight / 10);
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
        my_num_blow = blows_table[str_index][dex_index];

        /* Maximal value */
        if (my_num_blow > num) my_num_blow = num;

        /* Add in the "bonus blows" */
        my_num_blow += extra_blows;

        /* Require at least one blow */
        if (my_num_blow < 2) my_num_blow = 2;

        /* Boost digging skill by weapon weight */
        my_skill_dig += (item->weight / 10);
    }

    /*  priest weapon penalty for non-blessed edged weapons */
    if ((auto_class == CLASS_PRIEST) &&
        ((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
        (!(item->flags3 & TR3_BLESSED)))
    {
        /* Reduce the real bonuses */
        my_to_hit -= 2;
        my_to_dam -= 2;
    }

    /* Hack -- Reward High Level Warriors with Res Fear */
    if (auto_class == CLASS_WARRIOR)
    {
        /* Resist fear at level 30 */
        if (auto_level >= 30) my_resist_fear = TRUE;
    }


    /* Affect Skill -- stealth (bonus one) */
    my_skill_stl += 1;

    /* Affect Skill -- disarming (DEX and INT) */
    my_skill_dis += adj_dex_dis[my_stat_ind[A_DEX]];
    my_skill_dis += adj_int_dis[my_stat_ind[A_INT]];

    /* Affect Skill -- magic devices (INT) */
    my_skill_dev += adj_int_dev[my_stat_ind[A_INT]];

    /* Affect Skill -- saving throw (WIS) */
    my_skill_sav += adj_wis_sav[my_stat_ind[A_WIS]];

    /* Affect Skill -- digging (STR) */
    my_skill_dig += adj_str_dig[my_stat_ind[A_STR]];


    /* Affect Skill -- disarming (Level, by Class) */
    my_skill_dis += (cb_ptr->x_dis * auto_max_level / 10);

    /* Affect Skill -- magic devices (Level, by Class) */
    my_skill_dev += (cb_ptr->x_dev * auto_max_level / 10);

    /* Affect Skill -- saving throw (Level, by Class) */
    my_skill_sav += (cb_ptr->x_sav * auto_max_level / 10);

    /* Affect Skill -- stealth (Level, by Class) */
    my_skill_stl += (cb_ptr->x_stl * auto_max_level / 10);

    /* Affect Skill -- search ability (Level, by Class) */
    my_skill_srh += (cb_ptr->x_srh * auto_max_level / 10);

    /* Affect Skill -- search frequency (Level, by Class) */
    my_skill_fos += (cb_ptr->x_fos * auto_max_level / 10);

    /* Affect Skill -- combat (normal) (Level, by Class) */
    my_skill_thn += (cb_ptr->x_thn[wield_skill] * auto_max_level / 10);

    /* Affect Skill -- combat (shooting) (Level, by Class) */
    my_skill_thb += (cb_ptr->x_thb * auto_max_level / 10);

    /* Affect Skill -- combat (throwing) (Level, by Class) */
    my_skill_tht += (cb_ptr->x_thb * auto_max_level / 10);

    /* Limit Skill -- stealth from 0 to 30 */
    if (my_skill_stl > 30) my_skill_stl = 30;
    if (my_skill_stl < 0) my_skill_stl = 0;

    /* Limit Skill -- digging from 1 up */
    if (my_skill_dig < 1) my_skill_dig = 1;


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

        /* Skip dragons */
        if (auto_race >= RACE_MIN_DRAGON) continue;

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

        /* Skip dragons */
        if (auto_race >= RACE_MIN_DRAGON &&
            (i != INVEN_D_OUTER && i != INVEN_D_HEAD)) continue;

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
    my_cur_lite = 0;

    /* Glowing player has light */
    if (my_lite) my_cur_lite = 1;

    /* Lite */
    if (item->tval == TV_LITE)
    {
        /* Torches -- radius one */
        if (item->sval == SV_LITE_TORCH) my_cur_lite = 1;

        /* Lanterns -- radius two */
        if (item->sval == SV_LITE_LANTERN) my_cur_lite = 2;

        /* No fuel means no radius */
        if (!item->pval) my_cur_lite = 0;

        /* Artifact lites -- radius three */
        /* HACK assume non-torch/non lantern lite is artifact */
        if ((item->sval != SV_LITE_TORCH) &&
            (item->sval != SV_LITE_LANTERN))
        {
            my_cur_lite = 3;

            /* Artifact lites -- assume glowing */
            my_lite = TRUE;
        }
    }
}


/*
 * Helper function -- notice the player inventory
 */
static void borg_notice_aux2(void)
{
    int i;
    int amt_food_hical;
    int amt_food_lowcal;

    auto_item *item;


    /*** Reset counters ***/


    /* Reset basic */
    amt_fuel = 0;
    amt_food = 0;
    amt_food_lowcal = 0;
    amt_food_hical = 0;
    amt_ident = 0;
    amt_star_ident = 0;
    amt_recall = 0;
    amt_phase = 0;
    amt_escape = 0;
    amt_teleport = 0;

    /* Reset healing */
    amt_mana = 0;
    amt_ez_mana = 0;
    amt_heal = 0;
    amt_ez_heal = 0;
    amt_ez_heal_star = 0;
    amt_ez_heal_life = 0;
    amt_rod_heal = 0;
    amt_pot_heal =0;
    amt_cure_critical = 0;
    amt_cure_serious = 0;
    amt_cure_poison = 0;
    amt_slow_poison =0;
    amt_cure_confusion = 0;
    amt_cure_blind = 0;

    /* Reset detection */
    amt_detect_trap = 0;
    amt_detect_door = 0;
    amt_detect_evil = 0;
    amt_mapping =0;

    /* Reset missiles */
    amt_missile = 0;

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
    amt_fix_exp = 0;

    amt_speed = 0;
    amt_pfe   =0;
    amt_cool_staff = 0;
    amt_glyph = 0;

    /* Reset enchantment */
    amt_enchant_to_a = 0;
    amt_enchant_to_d = 0;
    amt_enchant_to_h = 0;
    amt_recharge     = 0;

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
                amt_slow_poison +=item->iqty;

                case SV_FOOD_RATION:
                amt_food_hical += item->iqty;
                break;
                case SV_FOOD_JERKY:
                    amt_food_lowcal += item->iqty;
                break;
                case SV_FOOD_BISCUIT:
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
                break;

                case SV_FOOD_CURE_CONFUSION:
                amt_cure_confusion +=item->iqty;
                break;

                case SV_FOOD_CURE_BLINDNESS:
                amt_cure_blind +=item->iqty;
                break;

                case SV_FOOD_CURE_POISON:
                amt_cure_poison +=item->iqty;
                break;

            }
            break;


            /* Potions */
            case TV_POTION:
            /* Analyze */
            switch (item->sval)
            {
                case SV_POTION_HEALING:
                amt_heal += item->iqty;
                amt_pot_heal +=item->iqty;
                break;
                case SV_POTION_STAR_HEALING:
                amt_ez_heal_star += item->iqty;
                break;
                case SV_POTION_LIFE:
                amt_ez_heal_life += item->iqty;
                break;
                case SV_POTION_CURE_CRITICAL:
                amt_cure_critical += item->iqty;
                break;
                case SV_POTION_CURE_SERIOUS:
                amt_cure_serious += item->iqty;
                break;
                case SV_POTION_CURE_LIGHT:
                if (do_cut) amt_cure_serious += item->iqty;
                break;
                case SV_POTION_CURE_POISON:
                amt_cure_poison += item->iqty;
                break;
                case SV_POTION_SLOW_POISON:
                amt_slow_poison += item->iqty;
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
                amt_speed += item->iqty;
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
                amt_ident += item->iqty;
                break;

                case SV_SCROLL_STAR_IDENTIFY:
                amt_star_ident += item->iqty;
                break;

                case SV_SCROLL_RECHARGING:
                amt_recharge += item->iqty;
                break;

                case SV_SCROLL_PHASE_DOOR:
                amt_phase += item->iqty;
                break;

                case SV_SCROLL_TELEPORT:
                amt_escape += item->iqty;
                amt_teleport += item->iqty;
                break;

                case SV_SCROLL_WORD_OF_RECALL:
                amt_recall += item->iqty;
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
                amt_pfe += item->iqty;
                break;

                case SV_SCROLL_STAR_ENCHANT_ARMOR:
                amt_enchant_armor += item->iqty;
                break;

                case SV_SCROLL_RUNE_OF_PROTECTION:
                amt_glyph += item->iqty;
                break;

            }
            break;


            /* Rods */
            case TV_ROD:


            /* Analyze */
            switch (item->sval)
            {
                case SV_ROD_IDENTIFY:
                amt_ident += item->iqty * 100;
                break;

                case SV_ROD_RECALL:
                /* Don't count on it if I suck at activations */
                if (my_skill_dev - item->level > 7)
                {
                    amt_recall += item->iqty * 100;
                }
                else
                {
                    amt_recall += item->iqty;
                }
                break;

                case SV_ROD_DETECT_TRAP:
                amt_detect_trap += item->iqty * 100;
                break;

                case SV_ROD_DETECT_DOOR:
                amt_detect_door += item->iqty * 100;
                break;

                case SV_ROD_DETECTION:
                amt_detect_trap += item->iqty * 100;
                amt_detect_door += item->iqty * 100;
                amt_detect_evil += item->iqty * 100;
                break;

                case SV_ROD_SPEED:
                /* Don't count on it if I suck at activations */
                if (my_skill_dev - item->level > 7)
                {
                    amt_speed += item->iqty * 100;
                }
                else
                {
                    amt_speed += item->iqty;
                }
                break;

                case SV_ROD_MAPPING:
                amt_mapping += item->iqty * 100;
                break;

                case SV_ROD_HEALING:
                /* only +2 per rod because of long charge time. */
                /* Don't count on it if I suck at activations */
                if (my_skill_dev - item->level > 7)
                {
                    amt_heal += item->iqty *2;
                    amt_rod_heal +=item->iqty;
                }
                else
                {
                    amt_heal += item->iqty;
                    amt_rod_heal +=item->iqty;
                }
                break;
            }

            break;


            /* Staffs */
            case TV_STAFF:

            /* Analyze */
            switch (item->sval)
            {
                case SV_STAFF_IDENTIFY:
                amt_ident += item->iqty * item->pval;
                break;

                case SV_STAFF_TELEPORTATION:
                amt_teleport += item->iqty * item->pval;
                break;

                case SV_STAFF_SPEED:
                amt_speed += item->iqty * item->pval;
                break;

                case SV_STAFF_HEALING:
                amt_heal += item->iqty * item->pval;
                break;

                case SV_STAFF_THE_MAGI:
                amt_ez_mana += item->iqty * item->pval;
                break;

                case SV_STAFF_POWER:
                case SV_STAFF_DESTRUCTION:
                amt_cool_staff +=item->iqty;
                break;

                case SV_STAFF_HOLINESS:
                amt_cool_staff +=item->iqty;
                amt_heal +=item->iqty * item->pval;
                break;
            }

            break;


            /* Flasks */
            case TV_FLASK:

            /* Use as fuel if we equip a lantern */
            if (my_cur_lite == 2) amt_fuel += item->iqty;

            /* Count as Missiles */
            if (auto_level < 10 ) amt_missile += item->iqty;

            break;


            /* Torches */
            case TV_LITE:


            /* Use as fuel if it is a torch and we carry a torch */
            if ((item->sval == SV_LITE_TORCH) &&
                (my_cur_lite <= 1) )
                {
                    amt_fuel += item->iqty;
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

            /* Missiles */

            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT:
            /* Hack -- ignore invalid missiles */
            if (item->tval != my_ammo_tval) break;

            /* Hack -- ignore worthless missiles */
            if (item->value <= 0) break;

            /* Count them */
            amt_missile += item->iqty;

            /* my_need_brand_weapon */
            if ( (borg_equips_artifact(ART_CUBRAGOL, INVEN_BOW) ||
                  borg_prayer_okay_fail(7, 5, 35)) &&
              item->iqty >=5 &&
              /* Skip artifacts and ego-items */
              !artifact_p(item) &&
              !ego_item_p(item) &&
              item->able)
              {
                my_need_brand_weapon +=10L;
              }

            /* if we have shit loads of cash (as we will at level 35),  */
            /* enchant missiles */
            if (auto_level > 35)
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
    /*  Artifact activations are accounted here
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
    if (borg_spell_legal_fail(2, 7, 10) || borg_prayer_legal_fail(2, 0, 10))
	{
		amt_food += 1000;
	}

    /* Handle "identify" -> infinite identifies */
    if (borg_spell_legal(2, 1) || borg_prayer_legal(5, 2))
    {
        amt_ident += 1000;
    }

    /* Handle "detect traps" */
    if (borg_prayer_legal(0, 6) ||
        borg_spell_legal(0,6))
    {
        amt_detect_trap += 1000;
    }

    /* Handle "detect doors" */
    if (borg_prayer_legal(0, 6) ||
        borg_spell_legal(0,6))
    {
        amt_detect_door += 1000;
    }

    /* Handle "detect evil" */
    if (borg_prayer_legal(0, 0) ||
        borg_spell_legal(0,1))
    {
        amt_detect_evil += 1000;
    }

    /* Handle "detection" */
    if (borg_prayer_legal(5, 1) ||
    borg_equips_artifact(ART_HOLHENNETH, INVEN_HEAD))
    {
        amt_detect_door += 1000;
        amt_detect_trap += 1000;
        amt_detect_evil += 1000;
    }

    /* Handle "magic mapping" */
    if (borg_prayer_legal(1, 5) ||
        borg_spell_legal(6,1) ||
    borg_equips_artifact(ART_ELENDIL, INVEN_LITE))
    {
        amt_detect_door += 1000;
        amt_detect_trap += 1000;
    }

    /*  Handle "protection from evil" */
    if (borg_prayer_legal(2, 4) ||
        borg_prayer_legal(6,3) ||
    borg_equips_artifact(ART_CARLAMMAS, INVEN_HEAD))
    {
        amt_pfe += 1000;
    }

    /*  Handle "rune of protection" glyph" */
    if (borg_prayer_legal(3, 6))
    {
        amt_glyph += 1000;
    }

    /* Handle "detect traps/doors" */
    if (borg_spell_legal(0, 6))
    {
        amt_detect_door += 1000;
        amt_detect_trap += 1000;
    }

    /* Handle "enchant weapon" */
    if (borg_prayer_okay_fail(7, 3, 40))
    {
        amt_enchant_to_h += 1000;
        amt_enchant_to_d += 1000;
        amt_enchant_weapon +=1000;
    }

    /* Handle "Brand Weapon (bolts)" */
    if ( borg_equips_artifact(ART_CUBRAGOL, INVEN_BOW) ||
         borg_prayer_legal(7, 5))
    {
        amt_brand_weapon += 1000;
    }

    /* Handle "enchant armor" */
    if (borg_prayer_okay_fail(7, 4, 40))
    {
        amt_enchant_to_a += 1000;
        amt_enchant_armor +=1000;
    }

    /* Handle recall */
    if (borg_prayer_legal(4, 4) || borg_spell_legal(5, 4))
    {
        amt_recall += 1000;
    }

    /* speed spells */
    if ( borg_spell_legal( 3, 3 ) ||
         borg_equips_artifact(ART_TARATOL, INVEN_WIELD) ||
         borg_equips_artifact(ART_FEANOR, INVEN_FEET) ||
         borg_equips_artifact(ART_TULKAS, INVEN_RIGHT) )
    {
        amt_speed += 1000;
    }

    /* Handle "cure light wounds" */
    if (borg_equips_artifact(ART_LOTHARANG, INVEN_WIELD))
    {
        amt_cure_serious += 1000;
    }


    /* Handle "heal" */
    if (borg_equips_artifact(ART_SOULKEEPER,INVEN_BODY) ||
        borg_equips_artifact(ART_GONDOR,INVEN_HEAD) ||
        borg_prayer_legal(3, 4) ||
        borg_prayer_legal(6, 2))
    {
        amt_heal += 1000;
    }

    /* Handle "Slow Poison" */
    if (borg_spell_legal(1, 3) ||
        borg_prayer_legal(0, 7))
    {
        amt_slow_poison += 1000;
    }

#if 0
    /* We cant rely on having mana, Keep potions */
    /* Handle "Cure Poison" */
    if (borg_spell_legal(4, 4) ||
        borg_prayer_legal(3, 0))
    {
        amt_cure_poison += 1000;
    }
#endif
#if 0
    /* Handle "phase" */
    if (borg_equips_artifact(ART_BELEGENNON, INVEN_BODY))
    {
        amt_phase += 1000;
    }

    /* Handle "escape" */
    if (borg_equips_artifact(ART_COLANNON,INVEN_OUTER))
    {
        amt_escape += 1000;
    }
#endif

    /* Handle "fix exp" */
    if (borg_equips_artifact(ART_LUTHIEN, INVEN_OUTER))
    {
        amt_fix_exp += 1000;
    }

    /* Handle "Recharge" */
    if (borg_equips_artifact(ART_THINGOL, INVEN_OUTER) ||
        borg_spell_legal(6,3 ) ||
        borg_prayer_legal(7,1))
    {
        amt_recharge += 1000;
    }

    /*** Process the Needs ***/

    /* No need for fuel */
    if ((auto_items[INVEN_LITE].sval != SV_LITE_TORCH) &&
        (auto_items[INVEN_LITE].sval != SV_LITE_LANTERN)) amt_fuel += 1000;

    /* No need to *buy* stat increase potions */
    if (my_stat_cur[A_STR] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_STR] + cp_ptr->c_adj[A_STR]))
        amt_add_stat[A_STR] += 1000;

    if (my_stat_cur[A_INT] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_INT] + cp_ptr->c_adj[A_INT]))
         amt_add_stat[A_INT] += 1000;

    if (my_stat_cur[A_WIS] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_WIS] + cp_ptr->c_adj[A_WIS]))
        amt_add_stat[A_WIS] += 1000;

    if (my_stat_cur[A_DEX] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_DEX] + cp_ptr->c_adj[A_DEX]))
         amt_add_stat[A_DEX] += 1000;

    if (my_stat_cur[A_CON] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_CON] + cp_ptr->c_adj[A_CON]))
        amt_add_stat[A_CON] += 1000;

    if (my_stat_cur[A_CHR] >= (18+100) + 10 * p_ptr->maximise *
        (rp_ptr->r_adj[A_CHR] + cp_ptr->c_adj[A_CHR]))
         amt_add_stat[A_CHR] += 1000;

    /* No need to *buy* stat repair potions XXX XXX XXX */
    if (!do_fix_stat[A_STR]) amt_fix_stat[A_STR] += 1000;
    if (!do_fix_stat[A_INT]) amt_fix_stat[A_INT] += 1000;
    if (!do_fix_stat[A_WIS]) amt_fix_stat[A_WIS] += 1000;
    if (!do_fix_stat[A_DEX]) amt_fix_stat[A_DEX] += 1000;
    if (!do_fix_stat[A_CON]) amt_fix_stat[A_CON] += 1000;
    if (!do_fix_stat[A_CHR]) amt_fix_stat[A_CHR] += 1000;

    /* No need for experience repair */
    if (!do_fix_exp) amt_fix_exp += 1000;

    /* Correct the high and low calorie foods */
    amt_food += amt_food_hical;
    if (amt_food_hical <= 0 || do_weak) amt_food += amt_food_lowcal;

    /* Correct the ez_heals */
    amt_ez_heal += amt_ez_heal_star + amt_ez_heal_life;
}


/*
 * Helper function -- notice the player swap weapon
 */
static void borg_notice_weapon_swap(void)
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

        /* Dont carry swaps until dlevel 50.  They are heavy */
        if (auto_max_depth < 50) continue;

        /*  priest weapon penalty for non-blessed edged weapons */
        if ((auto_class == CLASS_PRIEST) &&
            ((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
            (!(item->flags3 & TR3_BLESSED))) continue;

        /* Dragons wont carry swaps */
        if (auto_race >= RACE_MIN_DRAGON) continue;

        /* Require "known" (or average, good, etc) */
        if (!item->able &&
            !streq(item->note, "{good}") &&
            !streq(item->note, "{excellent}") &&
            !streq(item->note, "{terrible}") &&
            !streq(item->note, "{special}")) continue;

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
        decurse_weapon_swap =-1;

        /* Analyze the item */
        switch (item->tval)
        {
            /* weapons */
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
            {
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
            if (item->flags3 & TR3_LIGHT_CURSE) decurse_weapon_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_weapon_swap = 1;

            /* Sustain flags */

            /* calculating the value of the swap weapon. */
            damage = (item->dd * (item->ds) + (my_to_dam + item->to_d) *35L);

            /* Reward "damage" and increased blows per round*/
            v += damage * (my_num_blow+1);

            /* Reward "bonus to hit" */
            v += ((my_to_hit + item->to_h)*35L);

            dam = damage * my_num_blow;

            /* assume 2x base damage for x% of creatures */
            dam = damage * 2 * my_num_blow;
            if (!my_slay_animal && weapon_swap_slay_animal) v += (dam*2) /2;
            if (!my_slay_evil && weapon_swap_slay_evil) v +=  (dam*7) /2;

            /* assume 3x base damage for x% of creatures */
            dam = damage *3*my_num_blow;
            if (!my_slay_undead && weapon_swap_slay_undead) v += (dam*5) /2;
            if (!my_slay_demon && weapon_swap_slay_demon) v += (dam*3) /2;
            if (!my_slay_orc && weapon_swap_slay_orc) v += (dam*2) /2;
            if (!my_slay_troll && weapon_swap_slay_troll) v += (dam*3) /2;
            if (!my_slay_giant && weapon_swap_slay_giant) v += (dam*4) /2;
            if (!my_slay_dragon && !my_kill_dragon && weapon_swap_slay_dragon) v += (dam*6) /2;
            if (!my_brand_acid && weapon_swap_brand_acid) v += (dam*4) /2;
            if (!my_brand_elec && weapon_swap_brand_elec) v += (dam*4) /2;
            if (!my_brand_fire && weapon_swap_brand_fire) v += (dam*3) /2;
            if (!my_brand_cold && weapon_swap_brand_cold) v += (dam*3) /2;

            /* assume 5x base damage for x% of creatures */
            dam = damage  * 5 * my_num_blow;
            if (!my_kill_dragon && weapon_swap_kill_dragon) v += (dam*5) /2;


            if (!my_slow_digest && weapon_swap_slow_digest) v += 10L;
            if (weapon_swap_aggravate) v -= 6000L;
            if (weapon_swap_teleport) v -= 100000L;
            if (decurse_weapon_swap != -1) v -= 5000L;
            if (!my_regenerate && weapon_swap_regenerate) v += 2000L;
            if (!my_telepathy && weapon_swap_telepathy) v += 5000L;
            if (!my_lite && weapon_swap_lite) v += 2000L;
            if (!my_see_inv && weapon_swap_see_invis) v += 50000L;
            if (!my_ffall && weapon_swap_ffall) v += 10L;
            if (!my_free_act && weapon_swap_free_act) v += 10000L;
            if (!my_hold_life && (auto_level < 50) && weapon_swap_hold_life) v += 2000L;
            if (!my_immune_fire && weapon_swap_immune_fire) v += 60000L;
            if (!my_immune_acid && weapon_swap_immune_acid) v += 80000L;
            if (!my_immune_cold && weapon_swap_immune_cold) v += 25000L;
            if (!my_immune_elec && weapon_swap_immune_elec) v += 40000L;
            if (!my_resist_fire && weapon_swap_resist_fire) v += 8000L;
            if (!my_resist_acid && weapon_swap_resist_acid) v += 6000L;
            if (!my_resist_cold && weapon_swap_resist_cold) v += 4000L;
            if (!my_resist_elec && weapon_swap_resist_elec) v += 3000L;
            /* extra bonus for getting all basic resist */
            if (weapon_swap_resist_fire &&
                weapon_swap_resist_acid &&
                weapon_swap_resist_elec &&
                weapon_swap_resist_cold) v +=  10000L;
            if (!my_resist_pois && weapon_swap_resist_pois) v += 20000L;
            if (!my_resist_conf && weapon_swap_resist_conf) v += 5000L;
            if (!my_resist_sound && weapon_swap_resist_sound) v += 2000L;
            if (!my_resist_lite && weapon_swap_resist_lite) v += 800L;
            if (!my_resist_dark && weapon_swap_resist_dark) v += 800L;
            if (!my_resist_chaos && weapon_swap_resist_chaos) v += 8000L;
            if (!my_resist_disen && weapon_swap_resist_disen) v += 5000L;
            if (!my_resist_shard && weapon_swap_resist_shard) v += 100L;
            if (!my_resist_nexus && weapon_swap_resist_nexus) v += 100L;
            if (!my_resist_blind && weapon_swap_resist_blind) v += 5000L;
            if (!my_resist_neth && weapon_swap_resist_neth) v += 5500L;

            /*  Mega-Hack -- resists (level 60)
             *  its possible that he will get a sword and a cloak
             *  both with the same high resist and keep each based
             *  on that resist.  We want him to check to see
             *  that the other swap does not already have the high resist.
             */
            if (!my_resist_neth  && (auto_max_depth+1 >= 55) &&
                weapon_swap_resist_neth) v += 105000L;
            if (!my_resist_chaos && (auto_max_depth+1 >= 60) &&
                weapon_swap_resist_chaos) v += 102000L;
            if (!my_resist_disen && (auto_max_depth+1 >= 60) &&
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
            if (item->to_h < 15)
            {
                enchant_weapon_swap_to_h += (15 - item->to_h);
            }

            /* Enchant my swap (to damage) */
            if (item->to_d < 15)
            {
                enchant_weapon_swap_to_d += (15 - item->to_d);
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
            if (item->flags3 & TR3_LIGHT_CURSE) decurse_weapon_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_weapon_swap = 1;


}

/*
 * Helper function -- notice the player swap armour
 */
static void borg_notice_armour_swap(void)
{
    int i;
    int b_i = 0;
    s32b v = -1L;
    s32b b_v = 0L;
    int dam, damage;

    auto_item *item;

    armour_swap = 0;
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

        /* Dragons wont carry swaps BUT THEY COULD CARRY RINGS, CLOAKS, ETC */
        if (auto_race >= RACE_MIN_DRAGON) continue;


        /* Require "known" (or average, good, etc) */
        if (!item->able &&
            !streq(item->note, "{good}") &&
            !streq(item->note, "{excellent}") &&
            !streq(item->note, "{terrible}") &&
            !streq(item->note, "{special}")) continue;

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
            if (item->flags3 & TR3_LIGHT_CURSE) decurse_armour_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_armour_swap = 1;

            /* Sustain flags */

            /* calculating the value of the swap weapon. */
            damage = (item->dd * (item->ds) + (my_to_dam + item->to_d) *35L);
            /* Reward "damage" and increased blows per round*/
            v += damage * (my_num_blow+1);

            /* Reward "bonus to hit" */
            v += ((my_to_hit + item->to_h)*35L);
            dam = damage * my_num_blow;
            /* assume 2x base damage for x% of creatures */
            dam = damage * 2 * my_num_blow;

            if (!my_slay_animal && armour_swap_slay_animal) v += (dam*2) /2;
            if (!my_slay_evil && armour_swap_slay_evil) v +=  (dam*7) /2;
            /* assume 3x base damage for x% of creatures */
            dam = damage *3*my_num_blow;

            if (!my_slay_undead && armour_swap_slay_undead) v += (dam*5) /2;
            if (!my_slay_demon && armour_swap_slay_demon) v += (dam*3) /2;
            if (!my_slay_orc && armour_swap_slay_orc) v += (dam*2) /2;
            if (!my_slay_troll && armour_swap_slay_troll) v += (dam*3) /2;
            if (!my_slay_giant && armour_swap_slay_giant) v += (dam*4) /2;
            if (!my_slay_dragon && !my_kill_dragon && armour_swap_slay_dragon) v += (dam*6) /2;
            if (!my_brand_acid && armour_swap_brand_acid) v += (dam*4) /2;
            if (!my_brand_elec && armour_swap_brand_elec) v += (dam*4) /2;
            if (!my_brand_fire && armour_swap_brand_fire) v += (dam*3) /2;
            if (!my_brand_cold && armour_swap_brand_cold) v += (dam*3) /2;
            /* assume 5x base damage for x% of creatures */
            dam = damage  * 5 * my_num_blow;
            if (!my_kill_dragon && armour_swap_kill_dragon) v += (dam*5) /2;


            if (!my_slow_digest && armour_swap_slow_digest) v += 10L;
            if (armour_swap_aggravate) v -= 6000L;
            if (armour_swap_teleport) v -= 100000L;
            if (decurse_armour_swap != -1) v -= 5000L;
            if (!my_regenerate && armour_swap_regenerate) v += 2000L;
            if (!my_telepathy && armour_swap_telepathy) v += 5000L;
            if (!my_lite && armour_swap_lite) v += 2000L;
            if (!my_see_inv && armour_swap_see_invis) v += 50000L;
            if (!my_ffall && armour_swap_ffall) v += 10L;
            if (!my_free_act && armour_swap_free_act) v += 10000L;
            if (!my_hold_life && (auto_level < 50) && armour_swap_hold_life) v += 2000L;
            if (!my_immune_fire && armour_swap_immune_fire) v += 60000L;
            if (!my_immune_acid && armour_swap_immune_acid) v += 80000L;
            if (!my_immune_cold && armour_swap_immune_cold) v += 25000L;
            if (!my_immune_elec && armour_swap_immune_elec) v += 40000L;
            if (!my_resist_fire && armour_swap_resist_fire) v += 8000L;
            if (!my_resist_acid && armour_swap_resist_acid) v += 6000L;
            if (!my_resist_cold && armour_swap_resist_cold) v += 4000L;
            if (!my_resist_elec && armour_swap_resist_elec) v += 3000L;
            /* extra bonus for getting all basic resist */
            if (armour_swap_resist_fire &&
                armour_swap_resist_acid &&
                armour_swap_resist_elec &&
                armour_swap_resist_cold) v +=  10000L;
            if (!my_resist_pois && armour_swap_resist_pois) v += 20000L;
            if (!my_resist_conf && armour_swap_resist_conf) v += 5000L;
            if (!my_resist_sound && armour_swap_resist_sound) v += 2000L;
            if (!my_resist_lite && armour_swap_resist_lite) v += 800L;
            if (!my_resist_dark && armour_swap_resist_dark) v += 800L;
            if (!my_resist_chaos && armour_swap_resist_chaos) v += 8000L;
            if (!my_resist_disen && armour_swap_resist_disen) v += 5000L;
            if (!my_resist_shard && armour_swap_resist_shard) v += 100L;
            if (!my_resist_nexus && armour_swap_resist_nexus) v += 100L;
            if (!my_resist_blind && armour_swap_resist_blind) v += 5000L;
            if (!my_resist_neth && armour_swap_resist_neth) v += 5500L;

            /*  Mega-Hack -- resists (level 60)
             * its possible that he will get a sword and a cloak
             * both with the same high resist and keep each based
             * on that resist.  We want him to check to see
             * that the other swap does not already have the high resist.
             */
            if (!my_resist_neth  && auto_max_depth+1 >= 55  &&
                !weapon_swap_resist_neth &&
                armour_swap_resist_neth) v += 105000L;
            if (!my_resist_chaos && auto_max_depth+1 >= 60 &&
                !weapon_swap_resist_chaos &&
                armour_swap_resist_chaos) v += 102000L;
            if (!my_resist_disen && auto_max_depth+1 >= 60 &&
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
            if (item->flags3 & TR3_LIGHT_CURSE) decurse_armour_swap = 0;
            if (item->flags3 & TR3_HEAVY_CURSE) decurse_armour_swap = 1;

        enchant_armour_swap_to_a = 0;

        /* dont look for enchantment on non armours */
        if (item->tval >= TV_LITE) return;

        /* Hack -- enchant the swap equipment (armor) */
        /* Note need for enchantment */
        if ((borg_prayer_okay_fail(7, 4, 40) ||
            amt_enchant_armor >=1 ))
        {
            if (item->to_a < 15)
            {
                enchant_armour_swap_to_a += (15 - item->to_a);
            }
        }
        else
        {
            if (item->to_a < 10)
            {
                enchant_armour_swap_to_a += (10 - item->to_a);
            }
        }

}

/*
 * Analyze the equipment and inventory
 */
void borg_notice(bool notice_swap)
{
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

}

/*
 * Helper function -- notice the home equipment
 */
static void borg_notice_home_aux1(auto_item* in_item, bool no_items)
{

    /*** Reset counters ***/

    /* Reset basic */
    num_food = 0;
    num_ident = 0;
    num_star_ident = 0;
    num_recall = 0;
    num_phase = 0;
    num_escape = 0;
    num_teleport = 0;

    num_artifact = 0;

    num_invisible = 0;
    num_pfe =0;
    num_glyph = 0;

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
    num_cure_poison = 0;

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
    num_fix_exp = 0;
    num_mana = 0;
    num_heal = 0;
    num_ez_heal = 0;

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
        if (item->flags2 & TR2_RES_DISEN) num_resist_disen += item->iqty;
        if (item->flags2 & TR2_RES_SHARD) num_resist_shard += item->iqty;
        if (item->flags2 & TR2_RES_NEXUS) num_resist_nexus += item->iqty;
        if (item->flags2 & TR2_RES_BLIND) num_resist_blind += item->iqty;
        if (item->flags2 & TR2_RES_NETHR) num_resist_neth += item->iqty;
        if (item->flags2 & TR2_RES_CONFU) num_resist_conf += item->iqty;

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
        if (item->name1) num_artifact += item->iqty;

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
                if (mb_ptr->spell_book == TV_MAGIC_BOOK && auto_class == CLASS_MAGE)
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
                /*  most edged weapons hurt magic for priests */
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

                num_blow *= item->iqty;
                if ( item->to_d > 8 || auto_level < 15 )
                {
                    home_damage += num_blow * (item->dd * (item->ds) +
                                         (my_to_dam + item->to_d));
                }
                else
                {
                    home_damage += num_blow * (item->dd * (item->ds) +
                                        (my_to_dam + 8));
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
                num_food += item->iqty * 2;
                break;

                case SV_FOOD_RATION:
                num_food += item->iqty;
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
                break;
            }

            break;


            /* Potions */
            case TV_POTION:

            /* Analyze */
            switch (item->sval)
            {
                case SV_POTION_CURE_POISON:
                num_cure_poison += item->iqty;
                break;

                case SV_POTION_CURE_CRITICAL:
                num_cure_critical += item->iqty;
                break;

                case SV_POTION_CURE_SERIOUS:
                num_cure_serious += item->iqty;
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
                break;

                case SV_POTION_STAR_HEALING:
                num_ez_heal += item->iqty;
                break;
                case SV_POTION_LIFE:
                num_ez_heal += item->iqty;
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

                /*  */
                case SV_SCROLL_PROTECTION_FROM_EVIL:
                num_pfe += item->iqty;
                break;

                /*  */
                case SV_SCROLL_RUNE_OF_PROTECTION:
                num_glyph += item->iqty;
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
            if (item->pval <= 3 && auto_level > 30)
                break;

            /* Analyze */
            switch (item->sval)
            {
                case SV_STAFF_IDENTIFY:
                num_ident += item->iqty * item->pval;
                break;

                case SV_STAFF_TELEPORTATION:
                num_teleport += item->iqty * item->pval;
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
   /* , again.  Artifact activation included here */

    /* Handle "satisfy hunger" -> infinite food */
    if (borg_spell_legal(2, 7) || borg_prayer_legal(2, 0))
    {
        num_food += 1000;
    }

    /* Handle "identify" -> infinite identifies */
    if (borg_spell_legal(2, 1) ||
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

    /*  Handle "protection from evil" */
    if (borg_prayer_legal(2, 4) ||
        borg_prayer_legal(6, 3) ||
    borg_equips_artifact(ART_CARLAMMAS, INVEN_HEAD))
    {
        num_pfe += 1000;
    }

    /*  Handle "rune of protection" glyph */
    if (borg_prayer_legal(3, 6))
    {
        num_glyph += 1000;
    }

    /* handle restore */
    if (borg_prayer_legal(6, 5) ||
        borg_equips_artifact(ART_LUTHIEN ,INVEN_OUTER))
    {
        num_fix_exp += 1000;
    }

    /* Handle recall */
    if (borg_prayer_legal(4, 4) ||
        borg_spell_legal(5, 4) ||
        borg_equips_artifact(ART_AVAVIR ,INVEN_WIELD) )
    {
        num_recall += 1000;
    }

    /* Handle "Cure Poison" */
    if (borg_spell_legal(4, 4) ||
        borg_prayer_legal(3, 0))
    {
        num_cure_poison += 1000;
    }

    /*** Process the Needs ***/

    /* Hack -- No need for stat repair */
    if (my_sustain_str) num_fix_stat[A_STR] += 1000;
    if (my_sustain_int) num_fix_stat[A_INT] += 1000;
    if (my_sustain_wis) num_fix_stat[A_WIS] += 1000;
    if (my_sustain_dex) num_fix_stat[A_DEX] += 1000;
    if (my_sustain_con) num_fix_stat[A_CON] += 1000;
    if (my_sustain_chr) num_fix_stat[A_CHR] += 1000;

    /* race bonuses */
    if (auto_race == RACE_ELF) num_resist_lite = 1000;
    if (auto_race == RACE_GNOME) num_free_act = 1000;
    if (auto_race == RACE_DWARF) num_resist_blind = 1000;
    if (auto_race == RACE_HALF_ORC) num_resist_dark = 1000;
    if (auto_race == RACE_HIGH_ELF) num_resist_lite = 1000;
    if (auto_race == RACE_HIGH_ELF) num_see_inv = 1000;
    if (auto_race == RACE_CRYSTALDRAG) num_free_act = 1000;
    if (auto_race == RACE_CRYSTALDRAG) num_resist_shard = 1000;
    if (auto_race == RACE_COPPERDRAG) num_free_act = 1000;
    if (auto_race == RACE_COPPERDRAG) num_ffall = 1000;
    if (auto_race == RACE_COPPERDRAG) num_resist_disen =1000;
    if (auto_race == RACE_COPPERDRAG && auto_level > 40) num_resist_disen = 1000;
    if (auto_race == RACE_COPPERDRAG) num_regenerate = 1000;
    if (auto_race == RACE_BRONZEDRAG) num_ffall = 1000;
    if (auto_race == RACE_BRONZEDRAG) num_resist_conf = 1000;
    if (auto_race == RACE_BRONZEDRAG && auto_level > 40) num_resist_conf = 1000;
    if (auto_race == RACE_GOLDDRAG) num_ffall = 1000;
    if (auto_race == RACE_GOLDDRAG) num_resist_sound = 1000;
    if (auto_race == RACE_GOLDDRAG && auto_level > 40) num_resist_sound = 1000;
    if (auto_race == RACE_PSEUDODRAG) num_resist_lite = 1000;
    if (auto_race == RACE_PSEUDODRAG) num_resist_dark = 1000;
    if (auto_race == RACE_PSEUDODRAG) num_resist_blind = 1000;
    if (auto_race == RACE_PSEUDODRAG) num_regenerate = 1000;
    if (auto_race == RACE_PSEUDODRAG) num_see_inv = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_ffall = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_regenerate = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_resist_acid = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_resist_elec = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_resist_cold = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG) num_resist_fire = 1000;
    if (auto_race == RACE_MULTIHUEDDRAG && auto_level >= 45) num_resist_pois = 1000;

}

/*
 * Extract the bonuses for items in the home.
 *
 * in_item is passed in if you want to pretent that in_item is
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
 * Helper function -- calculate "power" of equipment
 */
static s32b borg_power_aux1(void)
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
    /* and assume we can enchant up to +8 if auto_level > 25 */
    if (item->to_d > 8 || auto_level < 25 )
    {
        damage = (item->dd * (item->ds) + (my_to_dam + item->to_d) *35L);
    }
    else
    {
        damage = (item->dd * (item->ds) + (my_to_dam + 8) *35L);
    }

    /* Reward "damage" and increased blows per round*/
    value += damage * (my_num_blow+1);

    /* Reward "bonus to hit" */
    if ( item->to_h > 8 || auto_level < 25 )
        value += ((my_to_hit + item->to_h)*35L);
    else
        value += ((my_to_hit + 8) * 35L );


    /* Calculation of slays */
    dam = damage * my_num_blow;

    /* assume 2x base damage for x% of creatures */
    dam = damage * 2 * my_num_blow;
    if (my_slay_animal) value += (dam * 2) / 2;
    if (my_slay_evil)   value += (dam * 7) / 2;

    /* assume 3x base damage for x% of creatures */
    dam = damage  * 3 * my_num_blow;
    if (my_slay_undead) value += (dam * 5) / 2;
    if (my_slay_demon)  value += (dam * 3) / 2;
    if (my_slay_orc)    value += (dam * 2) / 2;
    if (my_slay_troll)  value += (dam * 3) / 2;
    if (my_slay_dragon && (!my_kill_dragon)) value += (dam * 6) / 2;
    if (my_slay_giant)  value += (dam * 4) / 2;
    if (my_brand_acid)  value += (dam * 4) / 2;
    if (my_brand_elec)  value += (dam * 4) / 2;
    if (my_brand_fire)  value += (dam * 3) / 2;
    if (my_brand_cold)  value += (dam * 3) / 2;

    /* assume 5x base damage for x% of creatures */
    dam = damage  * 5 * my_num_blow;
    if (my_kill_dragon) value += (dam * 5) / 2;

    /* It is only on Grond */
    if (my_impact) value += 5000L;

    /* Hack -- It is hard to hold a heavy weapon */
    if (hold < item->weight / 10) value -= 500000L;

    /* Hack -- borg worships broken swords for swing bonus */
    if (item->kind == 47 || item->kind == 30) value -= 5000L;


    /*** Analyze bow ***/

    /* Examine current bow */
    item = &auto_items[INVEN_BOW];

    /* Calculate "average" damage per "normal" shot (times 2) */
    if ( item->to_d > 8 || auto_level < 25 )
        damage = ((my_ammo_sides) + (item->to_d)) * my_ammo_power;
    else
        damage = (my_ammo_sides + 8) * my_ammo_power;

    /* Reward "damage" */
    value += (my_num_fire * damage * 7L);

    /* Reward "bonus to hit" */
    if ( item->to_h > 8 || auto_level < 25 )
        value += ((my_to_hit + item->to_h) * 7L);
    else
        value += ((my_to_hit + 8) * 7L );

    /* Prefer bows */
    if (auto_class == CLASS_RANGER && my_ammo_tval == TV_ARROW) value += 30000L;

    /* Hack -- It is hard to hold a heavy weapon */
    if (hold < item->weight / 10) value -= 500000L;



    /***  Analyze dragon armour  ***/

    /* Examine current armor */
    item = &auto_items[INVEN_BODY];

    if (item->tval == TV_DRAG_ARMOR)
        value +=((550 * item->sval)+2000L);



    /*** Reward various things ***/

    /* Hack -- Reward light radius */
    value += (my_cur_lite * 1000000L);

    /* HACK reward upgrading artifact lites. (for activation) */
    if (item->tval == TV_LITE  &&
        (item->sval == SV_LITE_GALADRIEL ||
         item->sval == SV_LITE_THRAIN  ||
         item->sval == SV_LITE_ELENDIL) )
         {
            value += (auto_items[INVEN_LITE].sval * 2000);
         }

    /* Hack -- Reward speed
     * see if speed can be a bonus if good speed; not +3
     * reward higher for +10 than +50 speed (decreased return).
     */
    if (my_speed >= 150)
          value += (((my_speed - 120) * 1000L) + 185000L);

    if (my_speed >= 145 && my_speed <= 149)
          value += (((my_speed - 120) * 1000L) + 180000L);

    if (my_speed >= 140 && my_speed <= 144)
          value += (((my_speed - 120) * 1000L) + 175000L);

    if (my_speed >= 135 && my_speed <= 139)
          value += (((my_speed - 120) * 1000L) + 165000L);

    if (my_speed >= 130 && my_speed <= 134)
          value += (((my_speed - 120) * 1000L) + 150000L);

    if (my_speed >= 125 && my_speed <= 129)
          value += (((my_speed - 110) * 1000L) + 125000L);

    if (my_speed >= 120 && my_speed <= 124)
          value += (((my_speed - 110) * 1000L) + 100000L);

    if (my_speed >= 115 && my_speed <= 119)
          value += (((my_speed - 110) * 1000L) +  75000L);

    if (my_speed >= 110 && my_speed <= 114)
          value += (((my_speed - 110) * 1000L) +  55000L);
    else
          value += (((my_speed -110) *2500L));

    /* Hack -- Reward strength bonus */
    value += (my_stat_ind[A_STR] * 100L);

    /* Hack -- Reward intelligence bonus */
    if ((mb_ptr->spell_book == TV_MAGIC_BOOK) &&
        (my_stat_ind[A_INT] <= 37 ))
    {
        value += (my_stat_ind[A_INT] * 200L);

        /* Bonus for sp. */
        value += ((adj_mag_mana[my_stat_ind[A_INT]] * auto_level) / 2)  * 155L;

        /* bonus for fail rate */
        value += adj_mag_stat[my_stat_ind[A_INT]] * 3010L;

        /* mage should try to get min fail to 0 */
        if (auto_class == CLASS_MAGE)
        {
            if (adj_mag_fail[my_stat_ind[A_INT]] < 1)
                value += 70000L;

        }
    }

    /* Hack -- Reward wisdom bonus */
    if ((mb_ptr->spell_book == TV_PRAYER_BOOK) &&
        (my_stat_ind[A_WIS] <= 37 ))
    {
        value += (my_stat_ind[A_WIS] * 200L);

        /* Bonus for sp. */
        value += ((adj_mag_mana[my_stat_ind[A_WIS]] * auto_level) / 2)  * 150L;

        /* bonus for fail rate */
        value += adj_mag_stat[my_stat_ind[A_WIS]] * 3000L;

        /* priest should try to get min fail to 0 */
        if (auto_class == CLASS_PRIEST)
        {
            if (adj_mag_fail[my_stat_ind[A_WIS]] < 1)
                value += 70000L;
        }

    }


    /* Dexterity Bonus --good for attacking and ac*/
    if (my_stat_ind[A_DEX] <= 37 )
    {
        /* Hack -- Reward bonus */
        value += (my_stat_ind[A_DEX] * 200L);
    }

    /* Constitution Bonus */
    if (my_stat_ind[A_CON] <= 37 )
    {
        /* Hack -- Reward bonus */
        value += (my_stat_ind[A_CON] * 150L);
        value += ((adj_con_mhp[my_stat_ind[A_CON]] * auto_level) / 2)  * 150L;
    }

    /* Hack -- Reward charisma bonus up to level 25 */
    if (auto_level < 25)
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
    value += (my_skill_dis * 2L);
    value += (my_skill_dev * 25L);
    value += (my_skill_sav * 25L);
    /* perfect saves are very nice */
    if (my_skill_sav > 99)
        value += 10000;
    value += (my_skill_stl * 2L);
    value += (my_skill_srh * 1L);
    value += (my_skill_fos * 1L);
    value += (my_skill_thn * 5L);
    value += (my_skill_thb * 35L);
    value += (my_skill_tht * 2L);
    value += (my_skill_dig * 2L);


    /*** Reward current flags ***/

    /* Various flags */
    if (my_slow_digest) value  += 10L;

    if (my_ffall) value        += 10L;
    if (my_lite) value         += 2000L;
    if (my_telepathy)
    {
        if (my_see_inv) value      += 500L;
    }
    else
        if (my_see_inv) value      += 5000L;

    if (my_free_act) value     += 10000L;

    /* after you max out you are pretty safe from drainers.*/
    if (auto_level < 50)
    {
        if (my_hold_life) value    += 2000L;
    }
    else
    {
        if (my_hold_life) value    += 200L;
    }
    if (my_regenerate) value   += 2000L;
    if (my_telepathy) value    += 80000L;
    if (my_inviso && auto_depth <= 50) value += 50000L;
    if (my_inviso && auto_depth >= 51) value += 10000L;

    /* Immunity flags */
    if (my_immune_cold) value  += 25000L;
    if (my_immune_elec) value  += 40000L;
    if (my_immune_fire) value  += 60000L;
    if (my_immune_acid) value  += 80000L;

    /* Resistance flags -- First resist is important */
    if (my_resist_cold) value  += 3500L;
    if (my_resist_elec) value  += 4500L;
    if (my_resist_acid) value  += 6000L;
    if (my_resist_fire) value  += 8000L;
    /* extra bonus for getting all basic resist */
    if (my_resist_fire &&
        my_resist_acid &&
        my_resist_elec &&
        my_resist_cold) value +=  10000L;
    if (my_resist_pois) value  += 20000L;
    if (my_resist_sound) value += 2000L;
    if (my_resist_lite) value  += 800L;
    if (my_resist_dark) value  += 800L;
    if (my_resist_chaos) value += 8000L;
    if (my_resist_conf) value  += 5000L;
    /* mages need a slight boost for this */
    if (auto_class == CLASS_MAGE && my_resist_conf) value +=2000L;
    if (my_resist_disen) value += 5000L;
    if (my_resist_shard) value += 100L;
    if (my_resist_nexus) value += 100L;
    if (my_resist_blind) value += 5000L;
    if (my_resist_neth) value  += 5500L;
    if (my_resist_fear) value  += 2000L;

    /* Rewards for having multiple protection */
    if (my_resist_cold >= 2) value  += 1500L * my_resist_cold-1;
    if (my_resist_elec >= 2) value  += 2000L * my_resist_elec-1;
    if (my_resist_acid >= 2) value  += 2500L * my_resist_acid-1;
    if (my_resist_fire >= 2) value  += 3000L * my_resist_fire-1;
    if (my_resist_pois >= 2) value  += 8000L * my_resist_pois-1;
    if (my_resist_sound >= 2) value += 1000L * my_resist_sound-1;
    if (my_resist_lite >= 2) value  += 400L * my_resist_lite-1;
    if (my_resist_dark >= 2) value  += 400L * my_resist_dark-1;
    if (my_resist_chaos >= 2) value += 4000L * my_resist_chaos-1;
    if (my_resist_conf >= 2) value  += 2500L * my_resist_conf-1;
    if (my_resist_disen >= 2) value += 2500L * my_resist_disen-1;
    if (my_resist_shard >= 2) value += 50L * my_resist_shard-1;
    if (my_resist_nexus >= 2) value += 50L * my_resist_nexus-1;
    if (my_resist_neth >= 2) value  += 3000L * my_resist_neth-1;

    /* Dragons tend to wear lots of Rings of speed.  We need to enhance
     * the value of resists if speed is high enough.
     */
     if (my_speed >= 140)
     {
        /* Rewards for having multiple protection */
        if (my_resist_cold >= 2) value  += 12000L * my_resist_cold-1;
        if (my_resist_elec >= 2) value  += 15000L * my_resist_elec-1;
        if (my_resist_acid >= 2) value  += 17000L * my_resist_acid-1;
        if (my_resist_fire >= 2) value  += 20000L * my_resist_fire-1;
     }

    /* Sustain flags */
    if (my_sustain_str) value += 50L;
    if (my_sustain_int) value += 50L;
    if (my_sustain_wis) value += 50L;
    if (my_sustain_con) value += 50L;
    if (my_sustain_dex) value += 50L;


    /*** XXX XXX XXX Reward "necessary" flags ***/

    /* Mega-Hack -- See invisible (level 10) */
    if ((my_see_inv || my_telepathy) && (auto_max_depth+1 >= 10)) value += 100000L;


    /* Mega-Hack -- Free action (level 20) */
    if (my_free_act && (auto_max_depth+1 >= 20)) value += 100000L;


    /* Mega-Hack -- resists (level 25) */
    if (my_resist_fire && (auto_max_depth+1 >= 25)) value += 100000L;


    /* Mega-Hack -- resists (level 40) */
    if (my_resist_pois && (auto_max_depth+1 >= 40)) value += 150000L;
    if (my_resist_elec && (auto_max_depth+1 >= 40)) value += 100000L;
    if (my_resist_acid && (auto_max_depth+1 >= 40)) value += 100000L;
    if (my_resist_cold && (auto_max_depth+1 >= 40)) value += 100000L;


    /* APW Mega-Hack -- Speed / Hold Life (level 46) and maxed out */
    if ((my_hold_life && (auto_max_depth+1 >= 46) && (auto_level < 50))) value += 100000L;
    if ((my_speed >= 115) && (auto_max_depth+1 >=46)) value +=100000L;
    if (my_resist_conf && (auto_max_depth+1 >= 46)) value += 100000L;


    /* Mega-Hack -- resists (level 55) */
    if (my_resist_blind && (auto_max_depth+1 >= 55)) value += 100000L;

    /* Mega-Hack -- Telepathy (level 55) */
    if (my_telepathy && (auto_max_depth+1 >= 55)) value += 100000L;
	if (my_resist_neth  && (auto_max_depth+1 >= 55)) value += 105000L;

    /*  Mega-Hack -- resists (level 60) */

    if (my_resist_chaos && (auto_max_depth+1 >= 60)) value += 102000L;
    if (my_resist_disen && (auto_max_depth+1 >= 60)) value += 100000L;

    /*  Must have +10 speed after (level 60) */
    if ((my_speed >= 120) && (auto_max_depth+1 >=60)) value +=100000L;

    /*  Must have +20 speed (level 80) */
    if ((my_speed >= 130) && (auto_max_depth+1 >=80)) value +=100000L;



    /*  Not Req. but good to have +30 speed (level 80) */
    if ((my_speed >= 140) && (auto_max_depth+1 >=80) &&
        auto_class == CLASS_WARRIOR) value +=100000L;


    /*** Reward powerful armor ***/

    /* Reward armor */
    if (auto_level < 15) value += (((my_ac + my_to_ac)) * 2000L) + 100000L;
    if (auto_level >= 15 && auto_level < 35) value += (((my_ac + my_to_ac)) * 1500L) + 100000L;
    if (auto_level >= 35) value += (((my_ac + my_to_ac)) * 1000L) + 100000L;

    /*** Penalize various things ***/

    /* Penalize various flags */
    if (my_teleport) value -= 100000L;
    if (my_aggravate) value -= 6000L;

    /*** Penalize armor weight ***/

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
        (cur_wgt - max_wgt / 10) > 0)
    {
        /* Mega-Hack -- Penalize heavy armor which hurts mana */
		/* Penalty greater for low level guys */
		if (auto_level < 35)
		{
			value -= (((cur_wgt - max_wgt) / 10) * 2500L);
		}
		else
		{
			value -= (((cur_wgt - max_wgt) / 10) * 1000L);
		}

    }


    /*** Penalize bad magic ***/

    /* Hack -- most gloves hurt magic for spell-casters */
    if (mb_ptr->spell_book == TV_MAGIC_BOOK && auto_class ==CLASS_MAGE &&
        auto_race < RACE_MIN_DRAGON)
    {
        item = &auto_items[INVEN_HANDS];

        /* Penalize non-usable gloves */
        if (item->iqty &&
            (!(item->flags3 & TR3_FREE_ACT)) &&
            (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
        {
            /* Hack -- Major penalty */
            value -= 175000L;
        }
    }

    /*  Hack -- most edged weapons hurt magic for priests */
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
    if (auto_max_depth+1 == 100 && (!borg_king))
    {
        /* protect from stat drain */
        if (my_sustain_str) value += 15000L;
        if (my_sustain_int) value += 15000L;
        if (my_sustain_wis) value += 15000L;
        if (my_sustain_con) value += 15000L;
        if (my_sustain_dex) value += 15000L;
        if (my_slay_evil)  value += 150000L;

        /* Another bonus for resist nether, poison and base four */
        if (my_resist_neth) value +=  105000L;
        if (my_resist_disen) value += 100000L;

        /* to protect against summoned baddies */
        if (my_resist_pois) value +=  100000L;
        if (my_resist_fire &&
            my_resist_acid &&
            my_resist_elec &&
            my_resist_cold) value += 100000L;

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
        value +=(500+(150)+5000);
		break;

        /* Artifact -- INGWE- DISPEL EVIL X5 */
        case ART_INGWE:
        value +=(500+(10 + (auto_level*5)/2));
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
        value +=(500);
		break;


        }
    }


    /* Result */
    return (value);
}



/*
 * Helper function -- calculate power of inventory
 */
static s32b borg_power_aux2(void)
{
    int         k, carry_capacity, inven_weight, book;

    s32b        value = 0L;


    /*** Basic abilities ***/

    /*
     * In here, we must subtract out the bonus granted from certain
     * Artifacts.  They grant amt_x = 1000 then the power is increased
     * by 1000 times whatever bonus.  In the case of Gondor.  This is
     * 1000 heals times 4000 points per heal.
     *
     */

    /* Reward fuel */
    k = 0;
    for (; k < 5 && k < amt_fuel; k++) value += 60000L;
    for (; k < 10 && k < amt_fuel; k++) value += 6000L;

    /* Reward food */
    /* if weak, do not count food spells */
    if (do_weak && (amt_food >= 1000))
        amt_food -= 1000;

    /* if hungry, food is THE top priority */
    if ((do_hungry || do_weak) && amt_food) value += 100000;
    k = 0;
    for (; k < 5 && k < amt_food; k++) value += 50000L;
    for (; k < 10 && k < amt_food; k++) value += 500L;

    /* If we have no high calorie, buy low cal stuff */
    if (amt_food <= 3 && amt_food_hical <=1)
    {
        k = 0;
        for (; k < 10 && k < amt_food_lowcal; k++) value += 50000L;
    }

    /* Reward Cure Poison and Cuts*/
    if ((do_cut || do_poisoned) && amt_cure_critical) value +=100000;
    if ((do_cut || do_poisoned) && amt_heal) value +=50000;
    if (do_poisoned && amt_cure_poison) value +=15000;
    if (do_poisoned && amt_slow_poison) value +=5000;
    if ((do_cut || do_poisoned) && amt_cure_serious)
    {   /* usually takes more than one */
        k = 0;
        for (; k < 5 && k < amt_cure_serious; k++) value += 25000L;
    }

    /* Reward ident */
    k = 0;
    for (; k < 15 && k < amt_ident; k++) value += 6000L;
    for (; k < 30 && k < amt_ident; k++) value += 600L;

    /*  Reward *id*  carry lots of these*/
    k = 0;
    for (; k < 8 && k < amt_star_ident; k++) value += 10000L;
    for (; k < 15 && k < amt_star_ident; k++) value += 2000L;

    /*   Reward PFE  carry lots of these*/
    k = 0;
    for (; k < 10 && k < amt_pfe; k++) value += 10000L;
    for (; k < 25 && k < amt_pfe; k++) value += 2000L;

    /*   Reward Glyph- Rune of Protection-  carry lots of these*/
    k = 0;
    for (; k < 10 && k < amt_glyph; k++) value += 10000L;
    for (; k < 25 && k < amt_glyph; k++) value += 2000L;

    /* Reward recall */
    k = 0;
    for (; k < 3 && k < amt_recall; k++) value += 50000L;
    for (; k < 5 && k < amt_recall; k++) value += 5000L;

    /* Reward phase */
    k = 1;
    /* first phase door is very important */
    if (amt_phase) value += 50000;
    for (; k < 15 && k < amt_phase; k++) value += 500L;

    /* Reward escape */
    k = 0;
    for (; k < 5 && k < amt_escape; k++) value += 10000L;
    if (auto_depth > 90)
    {
        k = 0;
        for (; k < 15 && k < amt_escape; k++) value += 10000L;
    }

    /* Reward teleport */
    k = 0;
    for (; k < 10 && k < amt_teleport; k++) value += 10000L;

    /*** Healing ***/
    if (auto_class == CLASS_WARRIOR || auto_class == CLASS_ROGUE)
    {
        k = 0;
        for (; k < 20 && k < amt_heal; k++) value += 8000L;
        k = 0;
        for (; k < 2 && k < amt_ez_heal_star; k++) value +=10000L;
        /* if I don't have any *heals* then go for life pots */
        if (amt_ez_heal_star == 0)
        {
            k = 0;
            for (; k < 2 && k < amt_ez_heal_life; k++) value +=10000L;
        }
        /*  these guys need to carry the rods more, town runs low on supply */
        k = 0;
        for (; k < 4 && k < amt_rod_heal; k++) value +=20000L;

    }
    else
    if (auto_class == CLASS_RANGER || auto_class == CLASS_PALADIN ||
        auto_class == CLASS_MAGE)
    {
        k = 0;
        for (; k < 20 && k < amt_heal; k++) value += 4000L;
        k = 0;
        for (; k < 2 && k < amt_ez_heal_star; k++) value +=9000L;
        /* if I don't have any *heals* then go for life pots */
        if (amt_ez_heal_star == 0)
        {
            k = 0;
            for (; k < 2 && k < amt_ez_heal_life; k++) value +=9000L;
        }
        for (; k < 2 && k < amt_ez_heal; k++) value += 9000L;
    }
    else if (auto_class == CLASS_PRIEST)
    {
        /* Level 1 priests are given a Potion of Healing.  It is better
         * for them to sell that potion and buy equipment or several
         * Cure Crits with it.
         */
        if (auto_level == 1)
        {
            k = 0;
            for (; k < 10 && k < amt_pot_heal; k++) value -= 2000L;
        }

        /* Reward heal potions */
        k = 0;
        for (; k < 10 && k < amt_pot_heal; k++) value += 2000L;
        k = 0;
        for (; k < 2 && k < amt_ez_heal; k++) value += 9000L;
    }

    /* If Im going after morgy, carry real potions */
    if (auto_max_depth >= 99 && (!borg_king))
    {
        k = 0;
        for (; k < 99 && k < amt_pot_heal; k++) value += 8000L;
        k = 0;
        for (; k < 99 && k < amt_ez_heal; k++) value +=10000L;
    }

    /* Restore Mana */
    if (auto_msp > 100)
    {
        k = 0;
        for (; k < 15 && k < amt_mana; k++) value += 4000L;
        k = 0;
        for (; k < 100 && k < amt_ez_mana; k++) value += 4000L;
    }

    /* Reward cure critical.  Heavy reward on first 5 */
    k = 0;
    for (; k <  5 && k < amt_cure_critical; k++) value += 5000L;
    for (; k < 15 && k < amt_cure_critical; k++) value += 500L;


    /* Reward cure serious */
    /* only reward serious if low on crits */
    if (amt_cure_critical < 10 && (auto_level < 30 || !my_resist_conf))
    {
        k = 0;
        for (; k <  5 && k < amt_cure_serious; k++) value += 50L;
        for (; k < 10 && k < amt_cure_serious; k++) value += 5L;
    }

    /* Reward cure serious -- Low Level Characters */
    if (auto_level < 15)
    {
        k = 0;
        for (; k <  5 && k < amt_cure_serious; k++) value += 250L;
        for (; k < 10 && k < amt_cure_serious; k++) value += 55L;
    }


    /* Reward cure poison.  Heavy reward on first 5 */
    k = 0;
    for (; k <  5 && k < amt_cure_poison; k++) value += 5000L;
    for (; k < 10 && k < amt_cure_poison; k++) value += 500L;


    /* Reward slow poison */
    /* only reward slow poison if low on cures */
    if (amt_cure_poison < 5 && (auto_level < 30 || !my_resist_pois))
    {
        k = 0;
        for (; k <  5 && k < amt_slow_poison; k++) value += 100L;
        for (; k < 10 && k < amt_slow_poison; k++) value += 25L;
    }

    /* Reward Cures */
    if (!my_resist_conf)
    {
        k = 0;
        for (; k < 10 && k < amt_cure_confusion; k++) value += 4000L;
    }
    if (!my_resist_blind)
    {
        k=0;
        for (; k < 5 && k < amt_cure_blind; k++) value += 3000L;
    }

    /*** Detection ***/

    /* Reward detect trap */
    k = 0;
    for (; k < 1 && k < amt_detect_trap; k++) value += 4000L;

    /* Reward detect door */
    k = 0;
    for (; k < 1 && k < amt_detect_door; k++) value += 2000L;

    /* Reward detect evil */
    if (!my_telepathy)
    {
        k = 0;
        for (; k < 1 && k < amt_detect_evil; k++) value += 1000L;
    }

    /* Reward magic mapping */
    k = 0;
    for (; k < 1 && k < amt_mapping; k++) value += 4000L;



    /* Reward speed potions/staves */
    k = 0;
    for (; k < 20 && k < amt_speed; k++) value += 5000L;

    /* Reward Recharge ability */
    k = 0;
    for (; k < 5 && k < amt_recharge; k++) value += 5000L;

    /*** Missiles ***/

    /* Reward missiles */
    if (auto_class == CLASS_RANGER)
    {
        k = 0;
        for (; k < 30 && k < amt_missile; k++) value += 1000L;
        for (; k < 80 && k < amt_missile; k++) value += 100L;
    }
    else
    {
        k = 0;
        for (; k < 10 && k < amt_missile; k++) value += 1000L;
        for (; k < 50 && k < amt_missile; k++) value += 100L;
    }

    /*** Various ***/

    /*   -- Reward carrying a staff of holiness/power/destruction */
    k = 0;
    for (; k < 1 && k < amt_cool_staff; k++) value += 2000L;

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

    /* Reward *enchant weapon* */
    if (amt_enchant_weapon && (my_need_enchant_to_d ||
        my_need_enchant_to_h)) value += 200L;

    /* Reward *enchant armour* */
    if (amt_enchant_armor && my_need_enchant_to_a) value += 200L;

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

    /*  Hack -- Apply "encumbrance" from weight */
	/* Extract the current weight (in tenth pounds) */
    inven_weight = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
    carry_capacity = adj_str_wgt[my_stat_ind[A_STR]] * 100;

	/* XXX XXX XXX Apply "encumbrance" from weight */
    if (inven_weight > carry_capacity/2) value -= ((inven_weight - (carry_capacity/2)) / (carry_capacity / 10) *1000L);


    /* Return the value */
    return (value);
}



/*
 * Calculate the "power" of the Borg
 */
s32b borg_power(void)
{
    int i;
    s32b value = 0L;


    /* Process the equipment */
    value += borg_power_aux1();

    /* Process the inventory */
    value += borg_power_aux2();

    /* Add a bonus for deep level prep */
    /* Dump prep codes */
    for (i = 1; i <= 127; i++)
      {
         /* Dump fear code*/
         if ((cptr)NULL != borg_prepared(i)) break;
      }
    value +=((i-1) * 7500L);

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

/* !FIX These number are heavily weighted toward the mage... they should adjusted */
/*      to be less mageocentric.  Also I just made them up.  They seem to work but  */
/*      should probably be based on something more real. */

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
        value += 100L;
    else
        if (num_resist_sound == 2)
            value += 150L;
        else
            if (num_resist_sound > 2)
                value += 150L + (num_resist_sound - 2) * 1L;

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

    value += num_speed * 1000L;

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
    for (k = 0; k < 10 && k < num_food; k++) value += 8000L - k*10L;

    /* Collect ident */
    for (k = 0; k < 50 && k < num_ident; k++) value += 2000L - k*10L;

    /* Collect *id*ent */
    for (k = 0; k < 50 && k < num_star_ident; k++) value += 5000L - k*10L;

    /*  Collect pfe */
    for (k = 0; k < 100 && k < num_pfe; k++) value += 5000L - k*10L;

    /*  Collect glyphs */
    for (k = 0; k < 100 && k < num_glyph; k++) value += 5000L - k*10L;

    /* Collect recall */
    for (k = 0; k < 50 && k < num_recall; k++) value += 3000L - k*10L;

    /* Collect escape */
    for (k = 0; k < 50 && k < num_escape; k++) value += 2000L - k*10L;

    /* Collect teleport */
    for (k = 0; k < 50 && k < num_teleport; k++) value += 400L - k*8L;

    /* collect heal/mana/ */
    for (k = 0; k < 99 && k < num_heal; k++) value += 3000L - k*8L;
    for (k = 0; k < 99 && k < num_ez_heal; k++) value += 8000L - k*8L;
    if (auto_msp > 1)
    {
        for (k = 0; k < 99 && k < num_mana; k++) value += 4000L - k*8L;
    }

    /*** Healing ***/


    /* Level 1 priests are given a Potion of Healing.  It is better
     * for them to sell that potion and buy equipment or several
     * Cure Crits with it.
     */
    if (auto_level == 1)
    {
        k = 0;
        for (; k < 10 && k < num_heal; k++) value -= 5000L;
    }

     /* Collect cure critical */
    if (auto_level > 35)
        for (k = 0; k < 99 && k < num_cure_critical; k++) value -= 1500L-k*10L;
    else
        for (k = 0; k < 99 && k < num_cure_critical; k++) value += 1500L-k*10L;
    /* junk cure serious if we have some in the home */
    if (auto_level > 35)
        for (k = 0; k < 99 && k < num_cure_serious; k++) value -= 1500L-k*10L;

    /* Reward cure poison. */
    if (!my_resist_pois)
    {
        k = 0;
        for (; k <  15 && k < amt_cure_poison; k++) value += 5000L;
    }

    /*** Various ***/
    if (auto_level == 50) value -= 7500L * num_fix_exp;
    if (auto_level > 35)
       for (k = 0; k < 70 && k < num_fix_exp; k++) value += 5000L - k*10L;
    else
       for (k = 0; k < 5 && k < num_fix_exp; k++) value += 5000L - k*10L;


    /*** Hack -- books ***/

    /* Reward books */
    for (book = 0; book < 4; book++)
    {

        if (auto_level > 35)
            /* Collect up to 5 copies of each normal book */
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
 * . PFE reduces my fear of an area.
 */
static int borg_danger_aux1(int i)
{
    int k, n = 0;
    int pfe = 0;

    int power, chance;

    s16b ac = my_ac;

    auto_kill *kill = &auto_kills[i];

    monster_race *r_ptr = &r_info[kill->r_idx];

    /* goi gives +100 to ac and deflects almost all missiles and balls*/
    if (borg_goi)
        ac += 100;

    /* shields gives +50 to ac and deflects some missiles and balls*/
    if (borg_shield)
        ac += 50;

    /*  PFE gives a protection.  */
        /* Hack -- Apply "protection from evil" */
        if ( (borg_prot_from_evil) &&
            (r_ptr->flags3 & RF3_EVIL) &&
            ((auto_level) >= r_ptr->level) )
        {
            pfe = 1;
        }


    /* Mega-Hack -- unknown monsters */
    if (kill->r_idx >= MAX_R_IDX) return (1000);

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
            /*  if invulnurable (or PFE), no damage (carried through)*/
            if ((borg_goi ) && !borg_attacking)
                z = z*2/3;
            if ((pfe) && !borg_attacking)
                z /= 2;
            /* stun */
            if ((b_ptr->d_side < 3) && (z > 19))
                n += 200;
            power = 60;
            break;

            case RBE_POISON:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 5;
            if (my_resist_pois) break;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_UN_BONUS:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 20;
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_resist_disen) break;
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
                z = z*2/3;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            power = 15;
            break;

            case RBE_EAT_GOLD:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if in town and low level avoid them stupid urchins */
            if (auto_level < 3) z += 50;
            /* if invulnurable, no damage */
            power = 5;
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (100 <= adj_dex_safe[my_stat_ind[A_DEX]] + auto_level) break;
            if (auto_gold < 100) break;
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
                z = z*2/3;
            if (100 <= adj_dex_safe[my_stat_ind[A_DEX]] + auto_level) break;
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
                z = z*2/3;
            if (amt_food > 5) break;
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
                z = z*2/3;
            if (amt_fuel > 5) break;
            if (!my_cur_lite) break;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_ACID:
            if (my_immune_acid) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            if (my_resist_acid) z = (z + 2) / 2;
            if (my_resist_acid >= 2) z = (z + 2) / 2;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (!borg_full_damage)
                z += 200;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_ELEC:
            if (my_immune_elec) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_resist_elec) z = (z + 2) / 2;
            if (my_resist_elec >= 2) z = (z + 2) / 2;
            if (!borg_full_damage)
                z += 10;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_FIRE:
            if (my_immune_fire) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_resist_fire) z = (z + 2) / 2;
            if (my_resist_fire >= 2) z = (z + 2) / 2;
            if (!borg_full_damage)
                z += 20;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_COLD:
            if (my_immune_cold) break;
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 10;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_resist_cold) z = (z + 2) / 2;
            if (my_resist_cold >= 2) z = (z + 2) / 2;
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
                z = z*2/3;
            if (my_resist_blind) break;
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
                z = z*2/3;
            if (my_resist_conf) break;
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
                z = z*2/3;
            if (my_resist_fear) break;
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
                z = z*2/3;
            if (my_free_act) break;
            z += 200;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_STR:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_str) break;
            if (auto_stat[A_STR] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
            {
                z += 50;
                /* extra scary to have str drain below 10 */
                if (auto_stat[A_STR] < 10)
                    z += 250;
            }
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_DEX:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_dex) break;
            if (auto_stat[A_DEX] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
            {
                z += 50;
                /* extra scary to have drain below 10 */
                if (auto_stat[A_DEX] < 10)
                    z += 250;
            }
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_CON:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_con) break;
            if (auto_stat[A_CON] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
            {
                z += 50;
                /* extra scary to have con drain below 8 */
                if (auto_stat[A_STR] < 8)
                    z += 250;
            }
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_INT:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_int) break;
            if (auto_stat[A_INT] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
            {
                z += 5;
                /* extra scary for spell caster */
                if (mb_ptr->spell_book == TV_MAGIC_BOOK)
                    z += 100;
            }
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_WIS:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_wis) break;
            if (auto_stat[A_WIS] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
            {
                z += 5;
                /* extra scary for pray'er */
                if (mb_ptr->spell_book == TV_PRAYER_BOOK)
                    z += 100;
            }
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_CHR:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_sustain_chr) break;
            if (auto_stat[A_CHR] <= 3) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 5;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_LOSE_ALL:
            z = (b_ptr->d_dice * b_ptr->d_side);
            power = 2;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            /* only morgoth. HACK to make it easier to fight him */
            break;

            case RBE_SHATTER:
            z = (b_ptr->d_dice * b_ptr->d_side);
            z -= (z * ((ac < 150) ? ac : 150) / 250);
            power = 60;
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (!borg_full_damage)
                z += 150;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_EXP_10:
            z = (b_ptr->d_dice * b_ptr->d_side);
            /* if invulnurable, no damage */
            if ((borg_goi) && !borg_attacking)
                z = z*2/3;
            if (my_hold_life) break;
            /* do not worry about drain exp after level 50 */
            if (auto_level == 50) break;
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
                z = z*2/3;
            if (my_hold_life) break;
            /* do not worry about drain exp after level 50 */
            if (auto_level >= 50) break;
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
                z = z*2/3;
            if (my_hold_life) break;
            /* do not worry about drain exp after level 50 */
            if (auto_level >= 50) break;
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
                z = z*2/3;
            if (my_hold_life) break;
            /* do not worry about drain exp after level 50 */
            if (auto_level >= 50) break;
            if (borg_prayer_legal(6, 4)) break;
            if (!borg_full_damage)
                z += 250;
            if ((pfe) && !borg_attacking)
                z /= 2;
            break;

            case RBE_NASTY_POIS:
            /* this is a real pain in the butt */
            z +=750;
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

    /*  if invulnurable, very little damage 10% */
    if (borg_goi) n = (n*10/100);

    }


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
    int q, k, n = 0, pfe =0, glyph= 0, glyph_check =0;

    int spot_x, spot_y, spot_safe=1;

    int lev, hp, total_dam = 0, av;

    byte spell[96], num = 0;

    auto_kill *kill = &auto_kills[i];

    auto_grid   *ag;

    monster_race *r_ptr = &r_info[kill->r_idx];

    /* PFE gives a protection.  */
        /* Hack -- Apply "protection from evil" */
        if ( (borg_prot_from_evil) &&
            (r_ptr->flags3 & RF3_EVIL) &&
            ((auto_level ) >= r_ptr->level) )
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
            if (x== c_x && y==c_y) continue;

            /* track spaces already protected */
            if ( ag->feat == FEAT_GLYPH || ag->kill ||
               ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_PERM_SOLID)))
            {
                /* track the safe areas for calculating danger */
                spot_safe ++;

                /* Just in case */
                if (spot_safe == 0) spot_safe = 1;
            }

        }
    }
    /* Mega-Hack -- unknown monsters */
    if (kill->r_idx >= MAX_R_IDX) return (1000);


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
            if (borg_goi) z /= 2;
            break;

            case 96+5:    /* RF4_ARROW_2 */
            z = (3 * 6);
            if (borg_goi) z /= 2;
            break;

            case 96+6:    /* RF4_ARROW_3 */
            z = (5 * 6);
            if (borg_goi) z /= 2;
            break;

            case 96+7:    /* RF4_ARROW_4 */
            z = (7 * 6);
            if (borg_goi) z /= 2;
            break;

            case 96+8:    /* RF4_BR_ACID */
            if (my_immune_acid) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) z /= 2;
            if (my_resist_acid) z = (z + 2) / 2;
            if (my_resist_acid >= 2) z = (z + 2) / 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count.*/
            if (!borg_full_damage)
                p += 40;
            break;

            case 96+9:    /* RF4_BR_ELEC */
            if (my_immune_elec) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) z /= 2;
            if (my_resist_elec) z = (z + 2) / 2;
            if (my_resist_elec >= 2) z = (z + 2) / 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count.*/
            if (!borg_full_damage)
                p += 20;
            break;

            case 96+10:    /* RF4_BR_FIRE */
            if (my_immune_fire) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) z /= 2;
            if (my_resist_fire) z = (z + 2) / 2;
            if (my_resist_fire >= 2) z = (z + 2) / 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 96+11:    /* RF4_BR_COLD */
            if (my_immune_cold) break;
            z = (hp / 3);
            /* max damage */
            if (z > 1600)
                z = 1600;
            if (borg_goi) z /= 2;
            if (my_resist_cold) z = (z + 2) / 2;
            if (my_resist_cold >= 2) z = (z + 2) / 2;
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
            if (borg_goi) z /= 2;
            if (my_resist_pois) z = (z + 2) / 2;
            if (my_resist_pois >= 2) z = (z + 2) / 2;
            if (my_resist_pois) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 25;
            break;

            case 96+13:    /* RF4_BR_NETH */
            z = (hp / 6);
            /* max damage */
            if (z > 300)
                z = 550;
            if (borg_goi) z /= 2;
            if (my_resist_neth) z = (z*6)/7;
            if (my_resist_neth >= 2) z = z*6/8;
            if (my_resist_neth) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 120;
            break;

            case 96+14:    /* RF4_BR_LITE */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) z /= 2;
            if (my_resist_lite) z = (z*2)/3;
            if (my_resist_lite >= 2) z = z*2/3 ;
            if (my_resist_lite) break;
            if (my_resist_blind) break;
            p += 20;
            break;

            case 96+15:    /* RF4_BR_DARK */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) z /= 2;
            if (my_resist_dark) z = (z*2)/ 2;
            if (my_resist_dark >= 2) z = z*2/3 ;
            if (my_resist_dark) break;
            if (my_resist_blind) break;
            p += 20;
            break;

            case 96+16:    /* RF4_BR_CONF */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) z /= 2;
            if (my_resist_conf) z = z / 2;
            if (my_resist_conf >= 2) z = z*2/3 ;
            if (my_resist_conf) break;
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
            if (borg_goi) z /= 2;
            if (my_resist_sound) z = (z*5)/8;
            if (my_resist_sound >= 2) z = z*5/8 ;
            if (my_resist_sound) break;
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
            if (borg_goi) z /= 2;
            if (my_resist_chaos) z = (z*6)/8;
            if (my_resist_chaos >= 2) z = z*6/8 ;
            if (my_resist_chaos) break;
            p += 200;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 100;
            if (my_resist_conf) break;
            p += 50;
            break;

            case 96+19:    /* RF4_BR_DISE */
            z = (hp / 6);
            /* max damage */
            if (z > 500)
                z = 500;
            if (borg_goi) z /= 2;
            if (my_resist_disen) z = (z*6)/10;
            if (my_resist_disen >= 2) z = z*6/10 ;
            if (my_resist_disen) break;
            p += 500;
            break;

            case 96+20:    /* RF4_BR_NEXU */
            z = (hp / 3);
            /* max damage */
            if (z > 250)
                z = 250;
            if (borg_goi) z /= 2;
            if (my_resist_nexus) z = (z*6)/10;
            if (my_resist_nexus >= 2) z = z*6/10 ;
            if (my_resist_nexus) break;
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
            if (borg_goi) z /= 2;
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
            if (borg_goi) z /= 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 150;
            break;

            case 96+23:    /* RF4_BR_GRAV */
            z = (hp / 3);
            /* max damage */
            if (z > 200)
                z = 200;
            if (borg_goi) z /= 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 150;
            if (my_resist_sound) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 150;
            break;

            case 96+24:    /* RF4_BR_SHAR */
            z = (hp / 6);
            /* max damage */
            if (z > 400)
                z = 400;
            if (borg_goi) z /= 2;
            if (my_resist_shard) z = (z*6)/10;
            if (my_resist_shard >= 2) z = z*6/10 ;
            if (my_resist_shard) break;
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
            if (borg_goi) z /= 2;
            if (my_resist_sound) break;
            p += 100;
            break;

            case 96+26:    /* RF4_BR_WALL */
            z = (hp / 6);
            /* max damage */
            if (z > 150)
                z = 150;
            if (borg_goi) z /= 2;
            if (my_resist_sound) break;
            p += 50;
            break;

            case 96+27:    /* RF4_BR_MANA */
            /* XXX XXX XXX */
            if (borg_goi) z /= 2;
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
            if (my_immune_acid) break;
            z = (lev * 3) / 2 + 15;
            if (borg_goi) z /= 2;
            if (my_resist_acid) z = (z + 2) / 2;
            if (my_resist_acid >= 2) z = (z +2)/3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+1:    /* RF5_BA_ELEC */
            if (my_immune_elec) break;
            z = (lev * 3) / 2 + 8;
            if (borg_goi) z /= 2;
            if (my_resist_elec) z = (z + 2) / 2;
            if (my_resist_elec >= 2) z = (z +2)/3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+2:    /* RF5_BA_FIRE */
            if (my_immune_fire) break;
            z = (lev * 7) / 2 + 10;
            if (borg_goi) z /= 2;
            if (my_resist_fire) z = (z + 2) / 2;
            if (my_resist_fire >= 2) z = (z +2)/3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+3:    /* RF5_BA_COLD */
            if (my_immune_cold) break;
            z = (lev * 3) / 2 + 10;
            if (borg_goi) z /= 2;
            if (my_resist_cold) z = (z + 2) / 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+4:    /* RF5_BA_POIS */
            z = (12 * 2);
            if (borg_goi) z /= 2;
            if (my_resist_pois) z = (z + 2) / 2;
            if (my_resist_pois >= 2) z = (z +2)/3 ;
            if (my_resist_pois) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 25;
            break;

            case 128+5:    /* RF5_BA_NETH */
            z = (50 + (10 * 10) + lev+ 50);
            if (borg_goi) z /= 2;
            if (my_resist_neth) z = (z*6)/8;
            if (my_resist_neth >= 2) z = z * 6/10 ;
            if (my_resist_neth) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 200;
            break;

            case 128+6:    /* RF5_BA_WATE */
            z = ((lev * 5) / 2) + 50;
            if (borg_goi) z /= 2;
            if (my_resist_sound) break;
            if (my_resist_conf) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 50;
            break;

            case 128+7:    /* RF5_BA_MANA */
            z = (((lev * 5)) + (50));
            if (!borg_full_damage)
                p += 50;
            if (borg_goi) z /= 2;
            break;

            case 128+8:    /* RF5_BA_DARK */
            z = (((lev * 5)) + (50));
            if (borg_goi) z /= 2;
            if (my_resist_dark) z = z / 2;
            if (my_resist_dark >= 2) z = z * 6/10 ;
            if (my_resist_dark) break;
            if (my_resist_blind) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+9:    /* RF5_DRAIN_MANA */
            if (auto_msp) p += 10;
            break;

            case 128+10:    /* RF5_MIND_BLAST */
            if (my_skill_sav < 100)
                z = 20;
            break;

            case 128+11:    /* RF5_BRAIN_SMASH */
            z = (12 * 15);
            p += 200 - 2 * my_skill_sav;
            if (p < 0) p =0;
            break;

            case 128+12:    /* RF5_CAUSE_1 */
            if (my_skill_sav >= 100) break;
            z = (3 * 8);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - my_skill_sav) / 100;
            break;

            case 128+13:    /* RF5_CAUSE_2 */
            if (my_skill_sav >= 100) break;
            z = (8 * 8);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - my_skill_sav) / 100;
            break;

            case 128+14:    /* RF5_CAUSE_3 */
            if (my_skill_sav >= 100) break;
            z = (10 * 15);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 20% for fudge) */
                z = z * (120 - my_skill_sav) / 100;
            break;

            case 128+15:    /* RF5_CAUSE_4 */
            if (my_skill_sav >= 100) break;
            z = (15 * 15);
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                /* reduce by % chance of save  (add 40% for fudge) */
                z = z * (120 - my_skill_sav) / 100;
            break;

            case 128+16:    /* RF5_BO_ACID */
            if (my_immune_acid) break;
            z = ((7 * 8) + (lev / 3));
            if (borg_goi) z /= 2;
            if (my_resist_acid) z = (z + 2) / 2;
            if (my_resist_acid >= 2) z = (z +2) /3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+17:    /* RF5_BO_ELEC */
            if (my_immune_elec) break;
            z = ((4 * 8) + (lev / 3));
            if (borg_goi) z /= 2;
            if (my_resist_elec) z = (z + 2) / 2;
            if (my_resist_elec >= 2) z = (z +2) /3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+18:    /* RF5_BO_FIRE */
            if (my_immune_fire) break;
            z = ((9 * 8) + (lev / 3));
            if (borg_goi) z /= 2;
            if (my_resist_fire) z = (z + 2) / 2;
            if (my_resist_fire >= 2) z = (z +2) /3 ;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 40;
            break;

            case 128+19:    /* RF5_BO_COLD */
            if (my_immune_cold) break;
            z = ((6 * 8) + (lev / 3));
            if (borg_goi) z /= 2;
            if (my_resist_cold) z = (z + 2) / 2;
            if (my_resist_cold >= 2) z = (z +2) /3 ;
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
            if (borg_goi) z /= 2;
            if (my_resist_neth) z = (z*6)/8;
            if (my_resist_neth >= 2) z = z*6/10;
            if (my_resist_neth) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 200;
            break;

            case 128+22:    /* RF5_BO_WATE */
            z = ((10 * 10) + (lev));
            if (borg_goi) z /= 2;
            if (my_resist_sound) break;
            if (my_resist_conf) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+23:    /* RF5_BO_MANA */
            z = ((lev * 7) / 2) + 50;
            if (!borg_full_damage)
                p += 50;
            if (borg_goi) z /= 2;
            break;

            case 128+24:    /* RF5_BO_PLAS */
            z = (10 + (8 * 7) + (lev));
            if (borg_goi) z /= 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+25:    /* RF5_BO_ICEE */
            z = ((6 * 6) + (lev));
            if (borg_goi) z /= 2;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 20;
            break;

            case 128+26:    /* RF5_MISSILE */
            z = ((2 * 6) + (lev / 3));
            if (borg_goi) z /= 2;
            break;

            case 128+27:    /* RF5_SCARE */
            if (my_skill_sav >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+28:    /* RF5_BLIND */
            if (my_skill_sav >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+29:    /* RF5_CONF */
            if (my_skill_sav >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 10;
            break;

            case 128+30:    /* RF5_SLOW */
            if (my_free_act) break;
            if (my_skill_sav >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
                p += 5;
            break;

            case 128+31:    /* RF5_HOLD */
            if (my_free_act) break;
            if (my_skill_sav >= 100) break;
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
            if (borg_lock) break;
            if (!borg_full_damage)
                p += 20;
            break;

            case 160+9:    /* RF6_TELE_AWAY */
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (borg_lock) break;
            if (!borg_full_damage)
                p += 10;
            break;

            case 160+10:    /* RF6_TELE_LEVEL */
            if (my_skill_sav >= 100) break;
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
            if (my_skill_sav >= 100) break;
            /* if looking at full damage, things that are just annoying */
            /* do not count. */
            if (!borg_full_damage)
            {
                /* if you have lots of cash (like you will at level 35) */
                /* this is not very scary... just re-ID. */
                if (auto_level < 35)
                    p += 500;
                else
                    p += 50;
            }
            break;

            case 160+15:    /* RF6_XXX6X6 */
            break;

            /* Summoning is only as dangerious as the monster that is */
            /* actually summoned but the monsters that summon are a priority */
            /* to kill.  PFE reduces danger from some evil summoned monsters */

            case 160+16:    /* S_KIN */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev );
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 15;
                p = p / spot_safe;
            }
            break;

            case 160+17:    /* S_HI_DEMON */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev );
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 25;
                p = p / spot_safe;
            }
            break;


            case 160+18:    /* RF6_S_MONSTER */
            if (borg_lock) break;
            if (pfe || glyph || borg_create_door)
                p +=0;
            else
            {    p += (lev) * 5;
                p = p / spot_safe;
            }
            break;

            case 160+19:    /* RF6_S_MONSTERS */
            if (borg_lock) break;
            if (pfe || glyph || borg_create_door)
                p +=0;
            else
            {    p += (lev) * 10;
                 p = p / spot_safe;
            }
            break;

            case 160+20:   /* RF6_S_ANT */
            if (borg_lock) break;
            if (pfe || glyph || borg_create_door)
                p +=0;
            else
            {   p += (lev) * 10;
                p = p / spot_safe;
            }
            break;

            case 160+21:    /* RF6_S_SPIDER */
            if (borg_lock) break;
            if (pfe || glyph || borg_create_door)
                p +=0;
            else
            {   p += (lev) * 10;
                p = p / spot_safe;
            }
            break;

            case 160+22:    /* RF6_S_HOUND */
            if (borg_lock) break;
            if (pfe || glyph || borg_create_door)
                p +=0;
            else
            {    p += (lev) * 10;
                p = p / spot_safe;
            }
            break;

            case 160+23:    /* RF6_S_HYDRA */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {   p +=(lev) * 2;
                p = p / spot_safe;
            }
            else
            {   p += (lev) * 10;
                p = p / spot_safe;
            }
            break;

            case 160+24:    /* RF6_S_ANGEL */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev)* 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 15;
                p = p / spot_safe;
            }
            break;

            case 160+25:    /* RF6_S_DEMON */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 15;
                p = p / spot_safe;
            }
            break;

            case 160+26:    /* RF6_S_UNDEAD */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 15;
                p = p / spot_safe;
            }
            break;

            case 160+27:    /* RF6_S_DRAGON */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 3;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 15;
                p = p / spot_safe;
            }
            break;

            case 160+28:    /* RF6_S_HI_UNDEAD */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 25;
                p = p / spot_safe;
            }
            break;

            case 160+29:    /* RF6_S_HI_DRAGON */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 25;
                p = p / spot_safe;
            }
            break;

            case 160+30:    /* RF6_S_WRAITH */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 6;
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 25;
                p = p / spot_safe;
            }
            break;

            case 160+31:    /* RF6_S_UNIQUE */
            if (borg_lock) break;
            if (pfe )
            {    p +=(lev);
                p = p / spot_safe;
            }
            else if (glyph || borg_create_door)
            {    p +=(lev) * 5;    /* slightly reduced danger for unique */
                p = p / spot_safe;
            }
            else
            {    p += (lev) * 22;
                p = p / spot_safe;
            }
            break;
        }

        /* Notice damage */
        p += z;


        /* Track most dangerous spell */
        if (p > n) n = p;

        /* Track the damage of all the spells, used in averaging */
        total_dam +=p;
    }
    /* Average damage of all the spells & compare to most dangerous spell */
    av = total_dam / num;

    /* if the most dangerous spell is a bigger than the average, skip it */
    if (n < (av * 13/10))
    {
        return (av);
    }
    else
    /* Danger */
    return (n);
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

    int chance;

    int ax, ay, d;

    int q=0, r, p, v1=0, v2=0;

    int glyph =0;

    int fake_speed = auto_speed;

    /* Paranoia */
    if (!kill->r_idx) return (0);

    /* Paralyzed ones are no danger */
    if (kill->invuln) return (0);

    /* Distance components */
    ax = (x9 > x) ? (x9 - x) : (x - x9);
    ay = (y9 > y) ? (y9 - y) : (y - y9);

    /* Distance */
    d = MAX(ax, ay);

    /* Minimal distance */
    if (d < 1) d = 1;

    /* Minimal distance */
    if (d > 20) return (0);

    /* A very speedy borg can miscalculate the danger of some monsters */
    if (my_speed >= 135) fake_speed = (borg_fighting_unique ? 120 : 125);

    /* Consider the character haste and slow monster spells */
    if (!borg_speed && !borg_slow_spell)
    {
            /* Ugly!  we recalculate the moves */
            int t, e;

            /* Player energy per game turn  */
            e = extract_energy[(fake_speed)];

            /* Game turns per player move  */
            t = (100 + (e - 1)) / e;

            /*  Monster energy per game turn  */
            e = extract_energy[kill->speed];

            /* Monster moves */
            q = c * ((t * e) / 10);
    }
    else
    {
        /* I am speedy */
        if (borg_speed && !borg_slow_spell)
        {
            /* Ugly!  we recalculate the moves */
            int t, e;

            /* Player energy per game turn  */
            e = extract_energy[(fake_speed + 10)];

            /* Game turns per player move  */
            t = (100 + (e - 1)) / e;

            /*  Monster energy per game turn  */
            e = extract_energy[kill->speed];

            /* Monster moves */
            q = c * ((t * e) / 10);

        }
        /* I am casting a slow spell onto a monster */
        if (!borg_speed && borg_slow_spell)
        {
            /* Ugly!  we recalculate the moves */
            int t, e;

            /* Player energy per game turn  */
            e = extract_energy[(fake_speed )];

            /* Game turns per player move  */
            t = (100 + (e - 1)) / e;

            /*  Monster energy per game turn  */
            e = extract_energy[(kill->speed - 10)];

            /* Monster moves */
            q = c * ((t * e) / 10);

        }
        /* I am casting a slow spell and I am speedy*/
        if (borg_slow_spell && borg_speed)
        {
            /* Ugly!  we recalculate the moves */
            int t, e;

            /* Player energy per game turn  */
            e = extract_energy[(fake_speed + 10)];

            /* Game turns per player move  */
            t = (100 + (e - 1)) / e;

            /*  Monster energy per game turn  */
            e = extract_energy[(kill->speed - 10)];

            /* Monster moves */
            q = c * ((t * e) / 10);

        }
    }
    /* Minimal energy */
    /* allow partial hits when not caculating full possible damage */
    if (borg_full_damage)
        if (q < 10) q = 10;


    /** Danger from physical attacks **/

    /* Physical attacks */
    v1 = borg_danger_aux1(i);


    /* Hack -- he is about to overflow, reduce the danger */
    if (time_this_panel > 1200 || c_t > 25000)
    {
        v1 = v1/5;;
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

    /* Inviso helps sometimes */
    if (!(r_ptr->flags2 & RF2_SEE_INVIS) && (my_inviso))
    {
        v1 = v1*8/10;
    }

    /* Hack -- Physical attacks require proximity */
    if (q < (d * 10)  && auto_level > 20)
    {
        v1 = 0;
    }
    else if (q < (d * 10)  && auto_level <= 20)
    {   /* reduce damage to 20% if we are weak */
        v1 = v1 * 2/10;
    }

    /* multipliers yeild some trouble when I am weak */
    if ((r_ptr->flags2 & RF2_MULTIPLY) && (auto_level < 20))
    {   /* extra 50% */
        v1 = v1 + (v1 *15/10);
    }

    /* Friends yeild some trouble when I am weak */
    if ((r_ptr->flags1 & RF1_FRIENDS || r_ptr->flags1 & RF1_ESCORTS) &&
        (auto_level < 35))
    {
        if (auto_level < 15)
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
        /* weaklings should still fear */
        if (auto_level >= 10 )
        {
             v1 = v1 / d;
        }
        else
        {   /* only subract 20% of the danger */
            v1 = v1 * 8/10;
        }
    }
     /* Reduce danger from sleeping monsters with the sleep 2 spell*/
    if (borg_sleep_spell_ii)
    {
         if  ( (d == 1) &&
              (kill->awake) &&
              (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
              (kill->level <= auto_level) )
        {
              v1 = v1 / (d+2);
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

    /* Paralyzed Monsters are no danger */
    if (kill->invuln) v1 = 0;

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

    /** Spell attacks **/

    v2 = borg_danger_aux2(i,y,x);

    /* Hack -- he is about to over flow, reduce the danger */
    if (time_this_panel > 1200 || c_t > 25000)
    {
        v2 =v2 /5;
    }

    /* Never cast spells */
    if (!r_ptr->freq_inate && !r_ptr->freq_spell)
    {
        v2 = 0;
    }

    /* Hack -- verify distance */
    if (distance(y9, x9, y, x) > MAX_RANGE)
    {
        v2 = 0;
    }

    /* Hack -- verify line of sight on most monsters */
    /* Uniques who can pass/kill wall are extremely dangerous */
    if (r_ptr->flags1 & RF1_UNIQUE && (r_ptr->flags2 & RF2_PASS_WALL ||
        r_ptr->flags2 & RF2_KILL_WALL) && distance(y9, x9, y, x) < MAX_RANGE - 5)
    {
        /* leave the v2 value as is */
    }
    /* non unique, non pass/kill wall */
    else if (!borg_projectable(y9, x9, y, x))
    {
        v2 = 0;
    }

        /* multipliers yeild some trouble when I am weak */
        if ((r_ptr->flags2 & RF2_MULTIPLY) && (auto_level < 20))
        {
            v2 = v2 + (v2 *12/10);
        }

        /* Inviso helps sometimes */
        if (!(r_ptr->flags2 & RF2_SEE_INVIS) && (my_inviso))
        {
            v2 = v2*8/10;
        }

        /* Friends yeild some trouble when I am weak */
        if ((r_ptr->flags1 & RF1_FRIENDS || r_ptr->flags1 & RF1_ESCORTS) &&
            (auto_level < 20))
        {
            v2 = v2 + (v2 *12/10);
        }

        /* Reduce danger from sleeping monsters */
        if ((!kill->awake) && (d > 1))
        {
            /* weaklings should still fear */
            if (auto_level >= 10 )
            {
                 v2 = v2 / d;
            }
            else
            {   /* only subract 20% of the danger */
                v2 = v2 *8/10;
            }

        }
        /* Reduce danger from sleeping monsters with the sleep 2 spell*/
        if (borg_sleep_spell_ii)
        {

            if  ( (d == 1) &&
                  (kill->awake) &&
                  (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
                  (kill->level <= auto_level) )
            {
                  v2 = v2 / (d+2);
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
            v2 = 0;
        }
        if (kill->afraid)
        {
            v2 = 0;
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

    	/* Paralyzed Monsters are invulnerable */
    	if (kill->invuln) v2 = 0;


        /* Danger */
        if (v2)
        {
            /* Full power */
            r = q;

            /* Total danger */
            v2 = v2 * r / 10;
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

    borg_full_damage = TRUE;

    /* Base danger (from fear) */
    p = auto_fear_region[y/11][x/11] * c;

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
    return (p);
}




/*
 * Determine if the Borg is out of "crucial" supplies.
 *
 * Note that we ignore "restock" issues for the first several turns
 * on each level, to prevent repeated "level bouncing".
 */
cptr borg_restock(int depth)
{

    /* Always ready for the town */
    if (!depth) return (FALSE);

    /* Always spend time on a level */
    if (c_t - auto_began < 100) return (FALSE);


    /*** Level 1 ***/

    /* Must have some lite */
    if (my_cur_lite < 1) return ("rs my_cur_lite");

    /* Must have "fuel" */
    if (amt_fuel < 1) return ("rs amt_fuel");

    /* Must have "food" */
    if (amt_food < 1) return ("rs amt_food");

    /* Assume happy at level 1 */
    if (depth <= 1) return (FALSE);


    /*** Level 2 to 9  ***/

    /* Must have good lite */
    if (my_cur_lite < 2) return ("rs lite+1");

    /* Must have "fuel" */
    if (amt_fuel < 3) return ("rs fuel+2");

    /* Must have "food" */
    if (amt_food < 3) return ("rs food+2");

    /* Must have "recall" */
    if (amt_recall < 2) return ("rs recall");

    /* Assume happy at level 9 */
    if (depth <= 9) return (FALSE);

    /*** Level 10 - 19  ***/

    /* Must have "phase" */
    if (amt_phase < 1) return ("rs phase");

    /* Must have "cure" */
    if ((auto_level < 30) && amt_cure_serious + amt_cure_critical < 4) return ("rs cure4");

    /* Must have "teleport" */
    if (amt_teleport < 2) return ("rs teleport");

    /* Assume happy at level 19 */
    if (depth <= 19) return (FALSE);

    /*** Level 20 - 45  ***/

    /* Must have "cure" */
    if ((auto_level < 30) && amt_cure_serious + amt_cure_critical < 6) return ("rs cure");

    /* Must have "teleport" */
    if (amt_teleport + amt_escape < 4) return ("rs teleport");

    /* Assume happy at level 44 */
    if (depth <= 44) return (FALSE);

    /*** Level 46 - 100  ***/

    /* Must have "Heal" */
    if (amt_heal + amt_rod_heal + amt_ez_heal < 1) return ("rs heal");

    /* Assume happy */
    return (FALSE);
}


/*
 * Determine if the Borg meets the "minimum" requirements for a level
 */
static cptr borg_prepared_aux(int depth)
{
    if ( -1 == borg_ready_morgoth)
        borg_ready_morgoth = 0;
    if (borg_king)
        {
            borg_ready_morgoth = 2;
            return ((cptr)NULL);
        }

    /* Always ready for the town */
    if (!depth) return ((cptr)NULL);

    /*** Essential Items for Level 1 ***/

    /* Require lite (any) */
    if (my_cur_lite < 1) return ("Lite");

    /* Require food */
    if (amt_food < 5) return ("Food");

    /* Usually ready for level 1 */
    if (depth <= 1) return ((cptr)NULL);


    /*** Essential Items for Level 2 ***/

    /* Require lite (radius two) */
    if (my_cur_lite < 2) return ("Lite2");

    /* Require fuel */
    if (amt_fuel < 5) return ("Fuel5");

    /* Require recall */
    if (amt_recall < 1) return ("amt_recall");

    /* Usually ready for level 2 */
    if (depth <= 2) return ((cptr)NULL);


    /*** Essential Items for Level 3 and 4 ***/

    /* Scrolls of Word of Recall */
    if (amt_recall < 3) return ("recall3");

    /* Potions of Cure Serious Wounds */
    if ((auto_level < 30) && amt_cure_serious + amt_cure_critical < 2) return ("cure2");

    /* Usually ready for level 3 and 4 */
    if (depth <= 4) return ((cptr)NULL);


    /*** Essential Items for Level 5 to 9 ***/

    /* class specific requirement */
    if (auto_depth)
    {
        switch (auto_class)
        {
            case CLASS_WARRIOR:
                if (auto_mhp < 50) return ("hp");
                if (auto_level < 4) return ("level");
                break;
            case CLASS_ROGUE:
                if (auto_mhp < 50) return ("hp");
                if (auto_level < 8) return ("level");
                break;
            case CLASS_PRIEST:
                if (auto_mhp < 50) return ("hp");
                if (auto_level < 13) return ("level");
                break;
            case CLASS_PALADIN:
                if (auto_mhp < 50) return ("hp");
                if (auto_level < 4) return ("level");
                break;
            case CLASS_RANGER:
                if (auto_mhp < 50) return ("hp");
                if (auto_level < 4) return ("level");
                break;
            case CLASS_MAGE:
                if (auto_mhp < 90) return ("hp");
                if (auto_level < 15) return ("level");
                break;
        }
    }

    /* Scrolls of Word of Recall */
    if (amt_recall < 4) return ("recall4");

    /* Potions of Cure Serious/Critical Wounds */
    if ((auto_level < 30) && amt_cure_serious + amt_cure_critical < 5) return ("cure5");

    /* Usually ready for level 5 to 9 */
    if (depth <= 9) return ((cptr)NULL);


    /*** Essential Items for Level 10 to 19 ***/


    /* Escape or Teleport */
    if (amt_teleport + amt_escape < 2) return ("tele&esc2");

    /* Slow Poison */
    if (amt_cure_poison + amt_slow_poison < 2) return ("2anti-pois");

    /* class specific requirement */
    switch (auto_class)
    {
        case CLASS_WARRIOR:
            if (auto_level < (depth - 4) && depth <= 19)
                return ("level");
            break;
        case CLASS_ROGUE:
            if (auto_level < depth && depth <= 19) return ("level");
            break;
        case CLASS_PRIEST:
            if (auto_level < depth && depth <= 19) return ("level");
            break;
        case CLASS_PALADIN:
            if (auto_level < depth && depth <= 19) return ("level");
            break;
        case CLASS_RANGER:
            if (auto_level < depth && depth <= 19) return ("level");
            break;
        case CLASS_MAGE:
            if (auto_level < (depth + 5) && depth <= 19)
                return ("level");
            break;
    }

    /* Potions of Cure Critical Wounds */
    if ((auto_level < 30) && amt_cure_critical < 5) return ("cure crit5");

    /* See invisible */
    /* or telepathy */
    if ((!my_see_inv && !my_telepathy)) return ("See Invis : ESP");

    /* Usually ready for level 10 to 19 */
    if (depth <= 19) return ((cptr)NULL);


    /*** Essential Items for Level 20 ***/


    /* Free action */
    if (!my_free_act) return ("FA");

    /* Slow Poison */
    if (amt_cure_poison + amt_slow_poison < 3) return ("3anti-pois");

    /* ready for level 20 */
    if (depth <= 20) return ((cptr)NULL);


    /*** Essential Items for Level 25 ***/

    /* must have fire + 2 other basic resists */
    if (!my_resist_fire && !weapon_swap_resist_fire &&
        !armour_swap_resist_fire) return ("RF");
    {
        int basics = my_resist_acid + my_resist_cold + my_resist_elec;

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

    /* class specific requirement */
    switch (auto_class)
    {
        case CLASS_WARRIOR:
            if (auto_level < (depth + 5) && depth <= 25)
                return ("level");
            break;
        case CLASS_ROGUE:
            if (auto_level < (depth +10) && depth <= 25) return ("level");
            break;
        case CLASS_PRIEST:
            if (auto_level < (depth +13) && depth <= 25) return ("level");
            break;
        case CLASS_PALADIN:
            if (auto_level < (depth + 7) && depth <= 25) return ("level");
            break;
        case CLASS_RANGER:
            if (auto_level < (depth + 8) && depth <= 25) return ("level");
            break;
        case CLASS_MAGE:
            if (auto_level < (depth + 15) && depth <= 25)
                return ("level");
            break;
    }

    /* Ready for level 25 */
    if (depth <= 25) return ((cptr)NULL);


/*** Essential Items for Level 20 to 39 ***/


    /* Escape and Teleport */
    if (amt_teleport < 2) return ("teleport2");
    if (amt_teleport + amt_escape < 6) return ("tele&esc6");

    /* Cure Critical Wounds */
    if ((auto_level < 30) && (amt_cure_critical + amt_cure_serious) < 10) return ("cure10");

    /* Poison */
    if (amt_cure_poison + amt_slow_poison < 5) return ("5anti-pois");

    /* Minimal level */
    if (auto_level < 40) return ("level40");

    /* Usually ready for level 20 to 39 */
    if (depth <= 39) return ((cptr)NULL);



/*** Essential Items for Level 40 to 45 ***/

    /* All Basic resistance & poison*/
    if (!my_resist_cold && !weapon_swap_resist_cold &&
        !armour_swap_resist_cold) return ("RC");
    if (!my_resist_elec && !weapon_swap_resist_elec &&
        !armour_swap_resist_elec) return ("RE");
    if (!my_resist_acid && !weapon_swap_resist_acid &&
        !armour_swap_resist_acid) return ("RA");
    if (!my_resist_pois && !weapon_swap_resist_pois &&
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

    if (depth <= 45) return ((cptr)NULL);


/*** Essential Items for Level 46 to 55 ***/

    /*  Must have +5 speed after level 46 */
    if (my_speed < 115) return ("+5 speed");

    /* Potions of heal */
    if (amt_heal < 1 && (amt_ez_heal < 1) ) return ("1heal");

    if (!my_resist_conf && !weapon_swap_resist_conf &&
        !armour_swap_resist_conf)  return ("RConf");

    /* Minimal level */
    if (auto_level < 45) return ("level45");

    /* Minimal hitpoints */
    if (auto_mhp < 500) return ("HP 500");

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
       if ( (auto_level == 50) && (depth <= 60)) return ((cptr)NULL);
       else
        if ( (!my_hold_life && !weapon_swap_hold_life &&
        !armour_swap_hold_life) && (auto_level < 50) ) return ("hold life");

    /*   Usually ready for level 46 to 55 */
    if (depth <= 55) return ((cptr)NULL);


/*** Essential Items for Level 55 to 59 ***/


    /* Potions of heal */
    if (amt_heal < 2 && amt_ez_heal < 1) return ("2heal");

    /* Resists */
    if (!my_resist_blind && !weapon_swap_resist_blind &&
        !armour_swap_resist_blind) return ("RBlind");

    /* Telepathy, better have it by now */
    if (!my_telepathy) return ("ESP");

	/*  Must have resist nether */
    if (!my_resist_neth && !weapon_swap_resist_neth &&
        !armour_swap_resist_neth) return ("RNeth");

    /*    Usually ready for level 55 to 59 */
    if (depth <= 59) return ((cptr)NULL);



/*** Essential Items for Level 61 to 80 ***/

    /* Must have +10 speed */
    if (my_speed < 120) return ("+10 speed");


    /* Resists */
    if (!my_resist_chaos && !weapon_swap_resist_chaos &&
        !armour_swap_resist_chaos) return ("RChaos");
    if (!my_resist_disen && !weapon_swap_resist_disen &&
        !armour_swap_resist_disen) return ("RDisen");

    /*  Usually ready for level 61 to 80 */
    if (depth <= 80) return ((cptr)NULL);

/*** Essential Items for Level 81-85 ***/
    /* Minimal level */
    if (auto_level < 45) return ("Level 45");


    /* Minimal Speed */
    if (my_speed < 130) return ("+20 Speed");

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
        if (amt_pot_heal < 45) return ("45Heal");

        /* must have lots of ez-heal */
        if (amt_ez_heal < 25) return ("25EZHeal");

        /* Ready for morgoth! */
        borg_ready_morgoth = 1;
    }

    /* Its good to be to the king */
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

    /* generally do not scum */
    auto_scum = FALSE;

    /* Town and First level */
    if (depth <= 1) return ((cptr)NULL);

    /* Not prepared if I need to restock */
    if ((reason = borg_restock(depth))) return (reason);

    /* Must meet minimal requirements */
    if ((reason = borg_prepared_aux(depth))) return (reason);

    /* Once Morgoth is dead */
    if (depth >= 100 && borg_king)
    {
        auto_scum = TRUE;
        return ((cptr)NULL);
    }

    /* Always okay from town */
      if (!auto_depth) return ((cptr)NULL);

      /* check fear of level. */
      if (depth <= fear_depth) return ((cptr)NULL);

      /* check to make sure the borg does not go below where 2 living */
      /* uniques are. */

    if (auto_max_depth <= 99)
    {
          int numb_live_unique = 0, i;
          monster_race *r_ptr;

          /* BIG HACK, should check to make sure he has seen the unique. */
          /* !FIX change this to use the 'list of uniques (|) command AJG */
          for (i = 1; i < MAX_R_IDX-1; i++)
          {
              /* If any have been killed it is not a live unique */
              if (auto_race_death[i] != 0) continue;

              r_ptr = &r_info[i];

              /* Skip non-monsters */
              if (!r_ptr->name) continue;

              /* Skip non-uniques */
              if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

              /* skip if deeper than fear level */
              if ( r_ptr->level > fear_depth ) break;

              numb_live_unique++;
              continue;
          }

          if (numb_live_unique < 3)
          {
              auto_fear_depth = 0;
              fear_depth++;

              return ((cptr)NULL);
          }
      /* scum for the uniques and report */
      auto_scum = TRUE;
      return ("Live Uniques >= 3");
    }
    else
      /* check to make sure the borg does not go to level 100 */
      /* unless all the uniques are dead. */
    {
          int numb_live_unique = 0, i;
          monster_race *r_ptr;

          /* BIG HACK, should check to make sure he has seen the unique. */
          /* !FIX change this to use the 'list of uniques (|) command AJG */
          for (i = 1; i < MAX_R_IDX-1; i++)
          {
              /* If any have been killed it is not a live unique */
              if (auto_race_death[i] != 0) continue;

              r_ptr = &r_info[i];

              /* Skip non-monsters */
              if (!r_ptr->name) continue;

              /* Skip non-uniques */
              if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

              /* skip if deeper than fear level */
              if ( r_ptr->level > fear_depth ) break;

              numb_live_unique++;
              continue;
          }

          if (numb_live_unique < 1)
          {
              auto_fear_depth = 0;
              fear_depth++;

              return ((cptr)NULL);
          }
      /* scum for remaining uniques and report */
      auto_scum = TRUE;
      return ("Uniques still alive");

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
