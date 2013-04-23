/* File: borg4.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


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
	int			i, hold;

	int			pounds;

	int			extra_blows = 0;

	int			extra_shots = 0;
	int			extra_might = 0;

	u32b		f1, f2, f3;

	auto_item		*item;

        
	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) borg_stat_add[i] = 0;


	/* Clear the armor class */
	b_ptr->ac = 0;

	/* Clear the bonuses */
	b_ptr->to_h = b_ptr->to_d = b_ptr->to_a = 0;


	/* Reset blows */
	b_ptr->num_blow = 1;

	/* Reset shots */
	b_ptr->num_fire = 0;


	/* Assume normal speed */
	b_ptr->pspeed = 110;


	/* Reset the "ammo" tval */
	b_ptr->ammo_tval = 0;

	/* Reset the "ammo" sides */
	xb_ptr->ammo_sides = 0;

	/* Reset the shooting power */
	b_ptr->ammo_mult = 0;

	/* Reset the shooting range */
	xb_ptr->ammo_range = 0;


	/* Clear all the flags */
	b_ptr->see_inv = FALSE;
	b_ptr->teleport = FALSE;
	b_ptr->free_act = FALSE;
	b_ptr->slow_digest = FALSE;
	b_ptr->aggravate = FALSE;
	b_ptr->regenerate = FALSE;
	b_ptr->ffall = FALSE;
	b_ptr->hold_life = FALSE;
	b_ptr->telepathy = FALSE;
	b_ptr->lite = FALSE;

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
	my_brand_pois = FALSE;
    my_stunning = FALSE;

	b_ptr->immune_acid = FALSE;
	b_ptr->immune_elec = FALSE;
	b_ptr->immune_fire = FALSE;
	b_ptr->immune_cold = FALSE;

	b_ptr->resist_acid = FALSE;
	b_ptr->resist_elec = FALSE;
	b_ptr->resist_fire = FALSE;
	b_ptr->resist_cold = FALSE;
	b_ptr->resist_pois = FALSE;
	b_ptr->resist_fear = FALSE;
	b_ptr->resist_lite = FALSE;
	b_ptr->resist_dark = FALSE;
	b_ptr->resist_blind = FALSE;
	b_ptr->resist_confu = FALSE;
	b_ptr->resist_sound = FALSE;
	b_ptr->resist_shard = FALSE;
	b_ptr->resist_nexus = FALSE;
	b_ptr->resist_nethr = FALSE;
	b_ptr->resist_chaos = FALSE;
	b_ptr->resist_disen = FALSE;

	b_ptr->sustain_str = FALSE;
	b_ptr->sustain_int = FALSE;
	b_ptr->sustain_wis = FALSE;
	b_ptr->sustain_con = FALSE;
	b_ptr->sustain_dex = FALSE;
	b_ptr->sustain_chr = FALSE;


	/* Base infravision (purely racial) */
	b_ptr->see_infra = rb_ptr->infra;

	/* Base skill -- disarming */
	b_ptr->skill_dis = rb_ptr->r_dis + cb_ptr->c_dis;

	/* Base skill -- magic devices */
	b_ptr->skill_dev = rb_ptr->r_dev + cb_ptr->c_dev;

	/* Base skill -- saving throw */
	b_ptr->skill_sav = rb_ptr->r_sav + cb_ptr->c_sav;

	/* Base skill -- stealth */
	b_ptr->skill_stl = rb_ptr->r_stl + cb_ptr->c_stl;

	/* Base skill -- searching ability */
	b_ptr->skill_srh = rb_ptr->r_srh + cb_ptr->c_srh;

	/* Base skill -- searching frequency */
	b_ptr->skill_fos = rb_ptr->r_fos + cb_ptr->c_fos;

	/* Base skill -- combat (normal) */
	b_ptr->skill_thn = rb_ptr->r_thn + cb_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	b_ptr->skill_thb = rb_ptr->r_thb + cb_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	b_ptr->skill_tht = rb_ptr->r_thb + cb_ptr->c_thb;

	/* Base skill -- Digging */
	b_ptr->skill_dig = 0;

	/* Hack, use player flags to extract the race intrinsics */
	player_flags(&f1, &f2, &f3);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) b_ptr->slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) b_ptr->ffall = TRUE;
	if (f3 & (TR3_LITE)) b_ptr->lite = TRUE;
	if (f3 & (TR3_REGEN)) b_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) b_ptr->telepathy = TRUE;
	if (f3 & (TR3_SEE_INVIS)) b_ptr->see_inv = TRUE;
	if (f3 & (TR3_FREE_ACT)) b_ptr->free_act = TRUE;
	if (f3 & (TR3_HOLD_LIFE)) b_ptr->hold_life = TRUE;

	/* Weird flags */
	if (f3 & (TR3_BLESSED)) b_ptr->bless_blade = TRUE;

	/* Bad flags */
	if (f3 & (TR3_IMPACT)) b_ptr->impact = TRUE;
	if (f3 & (TR3_AGGRAVATE)) b_ptr->aggravate = TRUE;
	if (f3 & (TR3_TELEPORT)) b_ptr->teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) b_ptr->exp_drain = TRUE;

	/* Immunity flags */
	if (f2 & (TR2_IM_FIRE)) b_ptr->immune_fire = TRUE;
	if (f2 & (TR2_IM_ACID)) b_ptr->immune_acid = TRUE;
	if (f2 & (TR2_IM_COLD)) b_ptr->immune_cold = TRUE;
	if (f2 & (TR2_IM_ELEC)) b_ptr->immune_elec = TRUE;

	/* Resistance flags */
	if (f2 & (TR2_RES_ACID)) b_ptr->resist_acid = TRUE;
	if (f2 & (TR2_RES_ELEC)) b_ptr->resist_elec = TRUE;
	if (f2 & (TR2_RES_FIRE)) b_ptr->resist_fire = TRUE;
	if (f2 & (TR2_RES_COLD)) b_ptr->resist_cold = TRUE;
	if (f2 & (TR2_RES_POIS)) b_ptr->resist_pois = TRUE;
	if (f2 & (TR2_RES_FEAR)) b_ptr->resist_fear = TRUE;
	if (f2 & (TR2_RES_LITE)) b_ptr->resist_lite = TRUE;
	if (f2 & (TR2_RES_DARK)) b_ptr->resist_dark = TRUE;
	if (f2 & (TR2_RES_BLIND)) b_ptr->resist_blind = TRUE;
	if (f2 & (TR2_RES_CONFU)) b_ptr->resist_confu = TRUE;
	if (f2 & (TR2_RES_SOUND)) b_ptr->resist_sound = TRUE;
	if (f2 & (TR2_RES_SHARD)) b_ptr->resist_shard = TRUE;
	if (f2 & (TR2_RES_NEXUS)) b_ptr->resist_nexus = TRUE;
	if (f2 & (TR2_RES_NETHR)) b_ptr->resist_nethr = TRUE;
	if (f2 & (TR2_RES_CHAOS)) b_ptr->resist_chaos = TRUE;
	if (f2 & (TR2_RES_DISEN)) b_ptr->resist_disen = TRUE;

	/* Sustain flags */
	if (f2 & (TR2_SUST_STR)) b_ptr->sustain_str = TRUE;
	if (f2 & (TR2_SUST_INT)) b_ptr->sustain_int = TRUE;
	if (f2 & (TR2_SUST_WIS)) b_ptr->sustain_wis = TRUE;
	if (f2 & (TR2_SUST_DEX)) b_ptr->sustain_dex = TRUE;
	if (f2 & (TR2_SUST_CON)) b_ptr->sustain_con = TRUE;
	if (f2 & (TR2_SUST_CHR)) b_ptr->sustain_chr = TRUE;

#if 0	
	/* Elf */
	if (b_ptr->prace == 2) b_ptr->resist_lite = TRUE;

	/* Hobbit */
	if (b_ptr->prace == 3) b_ptr->sustain_dex = TRUE;

	/* Gnome */
	if (b_ptr->prace == 4) b_ptr->free_act = TRUE;

	/* Dwarf */
	if (b_ptr->prace == 5) b_ptr->resist_blind = TRUE;

	/* Half-Orc */
	if (b_ptr->prace == 6) b_ptr->resist_dark = TRUE;

	/* Half-Troll */
	if (b_ptr->prace == 7) b_ptr->sustain_str = TRUE;

	/* Dunadan */
	if (b_ptr->prace == 8) b_ptr->sustain_con = TRUE;

	/* High Elf */
	if (b_ptr->prace == 9) b_ptr->resist_lite = TRUE;
	if (b_ptr->prace == 9) b_ptr->see_inv = TRUE;

	/* Warrior */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		if (b_ptr->lev >= 30) b_ptr->resist_fear = TRUE;
	}
#endif

	/*** Analyze equipment ***/

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Affect stats */
		if (item->flags1 & TR1_STR) borg_stat_add[A_STR] += item->pval;
		if (item->flags1 & TR1_INT) borg_stat_add[A_INT] += item->pval;
		if (item->flags1 & TR1_WIS) borg_stat_add[A_WIS] += item->pval;
		if (item->flags1 & TR1_DEX) borg_stat_add[A_DEX] += item->pval;
		if (item->flags1 & TR1_CON) borg_stat_add[A_CON] += item->pval;
		if (item->flags1 & TR1_CHR) borg_stat_add[A_CHR] += item->pval;

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
		if (item->flags1 & TR1_BRAND_POIS)  my_brand_pois = TRUE;

		/* Affect infravision */
		if (item->flags1 & TR1_INFRA) b_ptr->see_infra += item->pval;

		/* Affect stealth */
		if (item->flags1 & TR1_STEALTH) b_ptr->skill_stl += item->pval;

		/* Affect searching ability (factor of five) */
		if (item->flags1 & TR1_SEARCH) b_ptr->skill_srh += (item->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (item->flags1 & TR1_SEARCH) b_ptr->skill_fos += (item->pval * 5);

		/* Affect digging (factor of 20) */
		if (item->flags1 & TR1_TUNNEL) b_ptr->skill_dig += (item->pval * 20);

		/* Affect speed */
		if (item->flags1 & TR1_SPEED) b_ptr->pspeed += item->pval;

		/* Affect blows */
		if (item->flags1 & TR1_BLOWS) extra_blows += item->pval;

		/* Boost shots */
		if (item->flags1 & TR1_SHOTS) extra_shots += item->pval;

		/* Boost might */
		if (item->flags1 & TR1_MIGHT) extra_might += item->pval;

		/* Good flags */
		if (item->flags3 & TR3_SLOW_DIGEST) b_ptr->slow_digest = TRUE;
		if (item->flags3 & TR3_FEATHER) b_ptr->ffall = TRUE;
		if (item->flags3 & TR3_LITE) b_ptr->lite = TRUE;
		if (item->flags3 & TR3_REGEN) b_ptr->regenerate = TRUE;
		if (item->flags3 & TR3_TELEPATHY) b_ptr->telepathy = TRUE;
		if (item->flags3 & TR3_SEE_INVIS) b_ptr->see_inv = TRUE;
		if (item->flags3 & TR3_FREE_ACT) b_ptr->free_act = TRUE;
		if (item->flags3 & TR3_HOLD_LIFE) b_ptr->hold_life = TRUE;

		/* Bad flags */
		if (item->flags3 & TR3_TELEPORT) b_ptr->teleport = TRUE;
		if (item->flags3 & TR3_AGGRAVATE) b_ptr->aggravate = TRUE;

		/* Immunity flags */
		/* Immunity provides resistance aswell */
		if (item->flags2 & TR2_IM_FIRE)
		{
			b_ptr->immune_fire = TRUE;
			b_ptr->resist_fire = TRUE;
		}
		if (item->flags2 & TR2_IM_ACID) 
		{
			b_ptr->immune_acid = TRUE;
			b_ptr->resist_acid = TRUE;
		}
		if (item->flags2 & TR2_IM_COLD) 
		{
			b_ptr->immune_cold = TRUE;
			b_ptr->resist_cold = TRUE;
		}
		if (item->flags2 & TR2_IM_ELEC) 
		{
			b_ptr->immune_elec = TRUE;
			b_ptr->resist_elec = TRUE;
		}

		/* Resistance flags */
		if (item->flags2 & TR2_RES_ACID) b_ptr->resist_acid = TRUE;
		if (item->flags2 & TR2_RES_ELEC) b_ptr->resist_elec = TRUE;
		if (item->flags2 & TR2_RES_FIRE) b_ptr->resist_fire = TRUE;
		if (item->flags2 & TR2_RES_COLD) b_ptr->resist_cold = TRUE;
		if (item->flags2 & TR2_RES_POIS) b_ptr->resist_pois = TRUE;
		if (item->flags2 & TR2_RES_FEAR) b_ptr->resist_fear = TRUE;
		if (item->flags2 & TR2_RES_LITE) b_ptr->resist_lite = TRUE;
		if (item->flags2 & TR2_RES_DARK) b_ptr->resist_dark = TRUE;
		if (item->flags2 & TR2_RES_BLIND) b_ptr->resist_blind = TRUE;
		if (item->flags2 & TR2_RES_CONFU) b_ptr->resist_confu = TRUE;
		if (item->flags2 & TR2_RES_SOUND) b_ptr->resist_sound = TRUE;
		if (item->flags2 & TR2_RES_SHARD) b_ptr->resist_shard = TRUE;
		if (item->flags2 & TR2_RES_NEXUS) b_ptr->resist_nexus = TRUE;
		if (item->flags2 & TR2_RES_NETHR) b_ptr->resist_nethr = TRUE;
		if (item->flags2 & TR2_RES_CHAOS) b_ptr->resist_chaos = TRUE;
		if (item->flags2 & TR2_RES_DISEN) b_ptr->resist_disen = TRUE;

		/* Sustain flags */
		if (item->flags2 & TR2_SUST_STR) b_ptr->sustain_str = TRUE;
		if (item->flags2 & TR2_SUST_INT) b_ptr->sustain_int = TRUE;
		if (item->flags2 & TR2_SUST_WIS) b_ptr->sustain_wis = TRUE;
		if (item->flags2 & TR2_SUST_DEX) b_ptr->sustain_dex = TRUE;
		if (item->flags2 & TR2_SUST_CON) b_ptr->sustain_con = TRUE;
		if (item->flags2 & TR2_SUST_CHR) b_ptr->sustain_chr = TRUE;

		/* Modify the base armor class */
		b_ptr->ac += item->ac;

		/* Apply the bonuses to armor class */
		b_ptr->to_a += item->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		b_ptr->to_h += item->to_h;
		b_ptr->to_d += item->to_d;
	}

	
	/* Update "stats" */
	for (i = 0; i < 6; i++)
	{
		int use, ind;

		/* Extract the new "use_stat" value for the stat */
		use = modify_stat_value(b_ptr->stat_cur[i], borg_stat_add[i]);

		/* Save the stat */
		b_ptr->stat_use[i] = use;

		/* Values: 3, ..., 17 */
		if (use <= 17) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Save the index */
		b_ptr->stat_ind[i] = ind;
	}


#if 0

	/* XXX XXX XXX */

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX Hack -- Apply "encumbrance" from weight */
	if (j > i/2) b_ptr->pspeed -= ((j - (i/2)) / (i / 10));

#endif

	/* Bloating slows the player down (a little) */
	if (borg_base_is_gorged) b_ptr->pspeed -= 10;



	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	b_ptr->to_a += ((int)(adj_dex_ta[b_ptr->stat_ind[A_DEX]]) - 128);
	b_ptr->to_d += ((int)(adj_str_td[b_ptr->stat_ind[A_STR]]) - 128);
	b_ptr->to_h += ((int)(adj_dex_th[b_ptr->stat_ind[A_DEX]]) - 128);
	b_ptr->to_h += ((int)(adj_str_th[b_ptr->stat_ind[A_STR]]) - 128);


	/* Obtain the "hold" value */
	hold = adj_str_hold[b_ptr->stat_ind[A_STR]];


	/* Examine the "current bow" */
	item = &borg_items[INVEN_BOW];

	/* Weight in pounds */
	pounds = item->weight / 10;

	/* It is hard to carholdry a heavy bow */
	if (hold < pounds)
	{
		/* Hard to wield a heavy bow */
		b_ptr->to_h += 2 * (hold - pounds);
	}

	/* Compute "extra shots" if needed */
	if (item->iqty)
	{
		/* Single shot */
		b_ptr->num_fire = 1;
		
		/* Analyze bow */
		switch (item->sval)
		{
			case SV_SLING:
			{
				b_ptr->ammo_tval = TV_SHOT;
				xb_ptr->ammo_sides = 3;
				b_ptr->ammo_mult = 2;
				break;
			}

			case SV_SHORT_BOW:
			{
				b_ptr->ammo_tval = TV_ARROW;
				xb_ptr->ammo_sides = 4;
				b_ptr->ammo_mult = 2;
				break;
			}

			case SV_LONG_BOW:
			{
				b_ptr->ammo_tval = TV_ARROW;
				xb_ptr->ammo_sides = 4;
				b_ptr->ammo_mult = 3;
				break;
			}

			case SV_LIGHT_XBOW:
			{
				b_ptr->ammo_tval = TV_BOLT;
				xb_ptr->ammo_sides = 5;
				b_ptr->ammo_mult = 3;
				break;
			}

			case SV_HEAVY_XBOW:
			{
				b_ptr->ammo_tval = TV_BOLT;
				xb_ptr->ammo_sides = 5;
				b_ptr->ammo_mult = 4;
				break;
			}
		}

		/* Light */
		if (hold >= pounds)
		{
			/* Add in extra power */
			b_ptr->ammo_mult += extra_might;

			/* Calculate total range */
			xb_ptr->ammo_range = 10 + b_ptr->ammo_mult * 5;

			/* Hack -- High Level Rangers shoot arrows quickly */
			if ((b_ptr->pclass == CLASS_RANGER) && (b_ptr->ammo_tval == TV_ARROW))
			{
				/* Extra shot at level 20 */
				if (b_ptr->max_lev >= 20) b_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (b_ptr->max_lev >= 40) b_ptr->num_fire++;
			}

			/* Add in the "bonus shots" */
			b_ptr->num_fire += extra_shots;
		}
	}

	/* Paranoia */
	/* Require at least one shot */
	if (b_ptr->num_fire < 1) b_ptr->num_fire = 1;


	/* Examine the "main weapon" */
	item = &borg_items[INVEN_WIELD];

	/* Weight in pounds */
	pounds = item->weight / 10;

	/* It is hard to hold a heavy weapon */
	if (hold < pounds)
	{
		/* Hard to wield a heavy weapon */
		b_ptr->to_h += 2 * (hold - pounds);
	}

	/* Normal weapons */
	if (item->iqty && (hold >= pounds))
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		/* Analyze the class */
		switch (b_ptr->pclass)
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
		str_index = (adj_str_blow[b_ptr->stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[b_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		b_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (b_ptr->num_blow > num) b_ptr->num_blow = num;

		/* Add in the "bonus blows" */
		b_ptr->num_blow += extra_blows;

		/* Require at least one blow */
		if (b_ptr->num_blow < 1) b_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight in pounds */
		b_ptr->skill_dig += pounds;
	}

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((b_ptr->pclass == 2) &&
	    ((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
	    (!(item->flags3 & TR3_BLESSED)))
	{
		/* Reduce the real bonuses */
		b_ptr->to_h -= 2;
		b_ptr->to_d -= 2;
	}


	/* Affect Skill -- stealth (bonus one) */
	b_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	b_ptr->skill_dis += adj_dex_dis[b_ptr->stat_ind[A_DEX]];
	b_ptr->skill_dis += adj_int_dis[b_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	b_ptr->skill_dev += adj_int_dev[b_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	b_ptr->skill_sav += adj_wis_sav[b_ptr->stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	b_ptr->skill_dig += adj_str_dig[b_ptr->stat_ind[A_STR]];


	/* Affect Skill -- disarming (Level, by Class) */
	b_ptr->skill_dis += (cb_ptr->x_dis * b_ptr->max_lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	b_ptr->skill_dev += (cb_ptr->x_dev * b_ptr->max_lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	b_ptr->skill_sav += (cb_ptr->x_sav * b_ptr->max_lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	b_ptr->skill_stl += (cb_ptr->x_stl * b_ptr->max_lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	b_ptr->skill_srh += (cb_ptr->x_srh * b_ptr->max_lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	b_ptr->skill_fos += (cb_ptr->x_fos * b_ptr->max_lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	b_ptr->skill_thn += (cb_ptr->x_thn * b_ptr->max_lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	b_ptr->skill_thb += (cb_ptr->x_thb * b_ptr->max_lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	b_ptr->skill_tht += (cb_ptr->x_thb * b_ptr->max_lev / 10);

	/* Limit Skill -- stealth from 0 to 30 */
	if (b_ptr->skill_stl > 30) b_ptr->skill_stl = 30;
	if (b_ptr->skill_stl < 0) b_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (b_ptr->skill_dig < 1) b_ptr->skill_dig = 1;


	/*** Count needed enchantment ***/

	/* Default enchantment max */
	xb_ptr->max_enchant_armor = 8;
	xb_ptr->max_enchant_weapon = 8;
    xb_ptr->max_enchant_weapon_bolt = 8;

	/* Priests with Enchant Weapon spell can go higher */
	if (borg_prayer_legal(7, 3))
	{
		xb_ptr->max_enchant_weapon++;
        xb_ptr->max_enchant_weapon_bolt++;

		/* And even higher in town */
		if (!b_ptr->depth) xb_ptr->max_enchant_weapon += 2;

        /* But not so high for arrows */
        if (!b_ptr->depth) xb_ptr->max_enchant_weapon_bolt += 1;
	}
	/* Rich characters can go higher */
    else if (b_ptr->au > 1000000)
	{
		xb_ptr->max_enchant_weapon++;
	}

	/* Priests with Enchant Armor spell can go higher */
	if (borg_prayer_legal(7, 4))
	{
		xb_ptr->max_enchant_armor++;

		/* And even higher in town */
		if (!b_ptr->depth) xb_ptr->max_enchant_armor += 2;
	}
	/* Rich characters can go higher */
    else if (b_ptr->au > 1000000)
	{
		xb_ptr->max_enchant_armor++;
	}

	/* Assume no enchantment needed */
	xb_ptr->need_enchant_to_a = 0;
	xb_ptr->need_enchant_to_h = 0;
	xb_ptr->need_enchant_to_d = 0;
    xb_ptr->need_enchant_brand = FALSE;

	/* Hack -- enchant all the equipment (weapons) */
	for (i = INVEN_WIELD; i <= INVEN_BOW; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip "unknown" items */
		if (!item->able) continue;

		/* Enchant all weapons (to hit) */
		if (item->to_h < xb_ptr->max_enchant_weapon)
		{
			xb_ptr->need_enchant_to_h += (xb_ptr->max_enchant_weapon - item->to_h);
		}

		/* Enchant all weapons (to damage) */
		if (item->to_d < xb_ptr->max_enchant_weapon)
		{
			xb_ptr->need_enchant_to_d += (xb_ptr->max_enchant_weapon - item->to_d);
		}
	}

	/* Hack -- enchant missiles */
	for (i = 0; i < INVEN_WIELD; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip "unknown" items */
		if (!item->able) continue;

		/* Skip all but the correct missile type */
		if (item->tval != b_ptr->ammo_tval) continue;

		/* Skip cursed ammo */
		if (item->value <= 0) continue;

		/* Only enchant in batches of at least 20 */
		if (item->iqty < 20) continue;

		/* Enchant all missiles (to hit) */
        if (item->to_h < xb_ptr->max_enchant_weapon_bolt)
		{
            xb_ptr->need_enchant_to_h += (xb_ptr->max_enchant_weapon_bolt - item->to_h);
		}

		/* Enchant all missiles (to damage) */
        if (item->to_d < xb_ptr->max_enchant_weapon_bolt)
		{
            xb_ptr->need_enchant_to_d += (xb_ptr->max_enchant_weapon_bolt - item->to_d);
		}

        /* Enchant all missiles (brand) */
        if (!item->name1 && !item->name2)
            xb_ptr->need_enchant_brand = TRUE;
	}

	/* Hack -- enchant all the equipment (armor) */
	for (i = INVEN_BODY; i <= INVEN_FEET; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip "unknown" items */
		if (!item->able) continue;

		/* Note need for enchantment */
		if (item->to_a < xb_ptr->max_enchant_armor)
		{
			xb_ptr->need_enchant_to_a += (xb_ptr->max_enchant_armor - item->to_a);
		}
	}


	/* Examine the lite */
	item = &borg_items[INVEN_LITE];

	/* Assume no lite */
	b_ptr->cur_lite = 0;

	/* Glowing player has lite */
	if (b_ptr->lite) b_ptr->cur_lite = 1;

	/* Actual Lite */
	if (item->tval == TV_LITE)
	{
		/* Torch */
		if (item->sval == SV_LITE_TORCH)
		{
			/* Radius one with fuel */
			if (item->pval) b_ptr->cur_lite = 1;
		}

		/* Lantern */
		else if (item->sval == SV_LITE_LANTERN)
		{
			/* Radius two with fuel */
			if (item->pval) b_ptr->cur_lite = 2;
		}

		/* Hack -- Artifact */
		else
		{
			/* Radius three */
			b_ptr->cur_lite = 3;

			/* Hack -- glowing */
			b_ptr->lite = TRUE;
		}
	}
}


/*
 * Helper function -- determine if an item is easily stackable
 * -RML
 */
static int borg_obj_stack(byte tval, byte sval, byte discount)
{
    int stack = 10;

    switch (tval)
    {
        case TV_SPIKE:
        case TV_FLASK:
        case TV_FOOD:
        stack = 1;
        break;

        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        stack = 0;
        break;

        case TV_MAGIC_BOOK:
        /* Low-level books of correct type are stackable */
        if (mb_ptr->spell_book == TV_MAGIC_BOOK && sval < 5)
            stack = 1;
        break;

        case TV_PRAYER_BOOK:
        /* Low-level books of correct type are stackable */
        if (mb_ptr->spell_book == TV_PRAYER_BOOK && sval < 5)
            stack = 1;
        break;

        case TV_LITE:
        if (sval == SV_LITE_TORCH)
            stack = 1;
        break;

        case TV_SCROLL:
        case TV_POTION:
        /* XXX Should only consider common ones stackable */
        stack = 1;
        break;

		case TV_ROD:
		case TV_WAND:
		/* We have added Leon's wand and rod stacking */
		stack = 1;
		break;
    }

   

    return FALSE;
}


/* XXX XXX XXX Needs to be in borg1.c -RML */
static sint amt_stack;
static sint num_stack;

/*
 * XXX Extremely hackish -RML
 */
bool borg_need_star_ident(void)
{
    int i;
    auto_item *item;


	/* Scan the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];

        /* Skip empty items */
		if (!item->iqty) continue;

        /* Skip un-aware items */
		if (!item->kind) continue;

        /* Check for art */
        if (item->name1 && item->do_star)
            return (TRUE);
    }

    /* Scan the equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		item = &borg_items[i];

        /* Skip empty items */
		if (!item->iqty) continue;

        /* Skip un-aware items */
		if (!item->kind) continue;

        /* Check for art */
        if (item->name1 && item->do_star)
            return (TRUE);
    }

	/* Must be in town */
	if (b_ptr->depth) return (FALSE);

	/* Must have complete information */
	for (i = 0; i < BORG_MAX_SHOP; i++)
	{
		auto_shop *shop = &borg_shops[i];

		/* Skip "visited" shops */
		if (!shop->when) return (FALSE);
	}

    /* Scan the home */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
        auto_shop *shop = &borg_shops[7];

        item = &shop->ware[i];

        /* Skip empty items */
		if (!item->iqty) continue;

        /* Skip un-aware items */
		if (!item->kind) continue;

        /* Check for art */
        if (item->name1 && item->do_star)
            return (TRUE);
    }

    /* Assume no need */
    return (FALSE);
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
	amt_fuel = 0;
	amt_food = 0;
	amt_ident = 0;
    amt_star_ident = 0;
	amt_recall = 0;
	amt_phase = 0;
	amt_escape = 0;
	amt_teleport = 0;
	amt_teleport_staff = 0;

	/* Reset healing */
	amt_pot_heal = 0;
	amt_pot_star_heal = 0;
	amt_pot_life = 0;
	amt_ez_heal = 0;
	amt_cure_critical = 0;
	amt_cure_serious = 0;

	/* Reset detection */
	amt_detect_trap = 0;
	amt_detect_door = 0;

	/* Reset missiles */
	amt_missile = 0;

	/* Reset speed */
	amt_speed = 0;
	amt_rod_speed = 0;

	/* Reset mana */
	amt_mana = 0;
	amt_ez_mana = 0;

    /* Reset attack spells -RML */
	amt_elec_ball = 0;
	amt_fire_ball = 0;
	amt_acid_ball = 0;
	amt_cold_ball = 0;

    amt_ball_attack = 0;
    amt_teleport_away = 0;

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

	/* Reset enchantment */
	amt_enchant_to_a = 0;
	amt_enchant_to_d = 0;
	amt_enchant_to_h = 0;

	/* reset uncurse */
	amt_decurse = 0;
	amt_star_decurse = 0;

    /* Reset amount of stuff */
    amt_stack = 0;
    

	amt_pfe = 0;
    amt_cool_staff = 0;
    amt_glyph = 0;
    amt_brand_weapon = 0;
    amt_enchant_weapon = 0;
    amt_enchant_armor = 0;

	/*** Process the inventory ***/

	/* Scan the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];

		/* Empty items are a free slot */
		if (!item->iqty) continue;

		/* Hack -- skip un-aware items */
		if (!item->kind) continue;


        /* Calculate space */
        amt_stack += item->iqty * borg_obj_stack(item->tval, item->sval,
                                                 item->discount);

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
				case SV_FOOD_RATION:
				amt_food += item->iqty;
				break;
			}

			break;


			/* Potions */
			case TV_POTION:

			/* Analyze */
			switch (item->sval)
			{
				case SV_POTION_HEALING:
				amt_pot_heal += item->iqty;
				break;
				
				case SV_POTION_STAR_HEALING:
                amt_pot_star_heal += item->iqty;
                break;
                
				case SV_POTION_LIFE:
                amt_pot_life += item->iqty;
                break;

				case SV_POTION_CURE_CRITICAL:
				amt_cure_critical += item->iqty;
				break;

				case SV_POTION_CURE_SERIOUS:
				amt_cure_serious += item->iqty;
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

				case SV_SCROLL_PHASE_DOOR:
				amt_phase += item->iqty;
				break;

				case SV_SCROLL_TELEPORT:
				amt_escape += item->iqty;
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

				case SV_SCROLL_REMOVE_CURSE:
				amt_decurse += item->iqty;
				break;

				case SV_SCROLL_STAR_REMOVE_CURSE:
				amt_star_decurse += item->iqty;
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

				case SV_SCROLL_SATISFY_HUNGER:
				amt_food += item->iqty;
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
				amt_recall += item->iqty * 100;
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
				break;

				case SV_ROD_SPEED:
				amt_rod_speed += item->iqty;
				break;

				case SV_ROD_HEALING:
				amt_ez_heal += item->iqty;
				break;

                /* Attack rods -RML */
                case SV_ROD_ACID_BALL:
				amt_acid_ball += item->iqty * 10;
				break;

                case SV_ROD_ELEC_BALL:
				amt_elec_ball += item->iqty * 10;
				break;

                case SV_ROD_FIRE_BALL:
				amt_fire_ball += item->iqty * 10;
				break;

                case SV_ROD_COLD_BALL:
				amt_cold_ball += item->iqty * 10;
                break;

                /* Teleport away -RML */
                case SV_ROD_TELEPORT_AWAY:
                amt_teleport_away += item->iqty * 100;
                break;
			}

			break;


            /* Wands -RML */
            case TV_WAND:

			/* Analyze */
			switch (item->sval)
			{
                case SV_WAND_TELEPORT_AWAY:
                amt_teleport_away += item->pval;
                break;

                case SV_WAND_STINKING_CLOUD:
                /* XXX Use until powerful */
                if (b_ptr->lev < 25)
                    amt_ball_attack += item->pval;
                break;

                case SV_WAND_ACID_BALL:
				amt_acid_ball += item->pval;
				break;

                case SV_WAND_ELEC_BALL:
				amt_elec_ball += item->pval;
				break;

                case SV_WAND_FIRE_BALL:
				amt_fire_ball += item->pval;
				break;

                case SV_WAND_COLD_BALL:
				amt_cold_ball += item->pval;
				break;

                case SV_WAND_DRAGON_FIRE:
                case SV_WAND_DRAGON_COLD:
                case SV_WAND_DRAGON_BREATH:
                amt_ball_attack += item->pval;
                break;
			}

			break;


			/* Staffs */
			case TV_STAFF:

			/* Analyze */
			switch (item->sval)
			{
				case SV_STAFF_IDENTIFY:
				/* Hack -- penalize lots of staffs with few charges */
				if ((item->iqty < 5) && (item->pval > 4))
				{
					amt_ident += item->iqty * item->pval;
				}
				break;

				case SV_STAFF_TELEPORTATION:
				/* Don't use them deep in the dungeon because the
                 * charges will get drained and he wont have any
                 * scrolls to read
                 */
                if (b_ptr->max_depth < 90)
                {
                    amt_teleport += item->iqty * item->pval;
                    /* do not count staffs with no changes */
                    if (item->pval)
                        amt_teleport_staff += item->iqty;
				}
				break;

				case SV_STAFF_SPEED:
				amt_speed += item->iqty * item->pval;
				break;

				case SV_STAFF_HEALING:
				amt_ez_heal += item->iqty * item->pval;
				break;

				case SV_STAFF_THE_MAGI:
				amt_ez_mana += item->iqty * item->pval;
				break;
	
				case SV_STAFF_HOLINESS:
				case SV_STAFF_POWER:
                amt_cool_staff +=item->iqty * item->pval;
				break;

				case SV_STAFF_REMOVE_CURSE:
				amt_decurse += item->iqty * item->pval;
				break;
			}

			break;


			/* Flasks */
			case TV_FLASK:

			/* Use as fuel (etc) */
			amt_fuel += item->iqty;

			/* Hack -- use as missiles until powerful */
            if (b_ptr->lev < 10) amt_ball_attack += item->iqty;

			break;


            /* Torches */
            case TV_LITE:

            switch (item->sval)
            {
                /* Use torches for light at low levels */
                case SV_LITE_TORCH:
                if (b_ptr->lev >= 5) break;
                if (b_ptr->cur_lite > 1) break;
                if (item->pval < 500) break;
                amt_fuel += item->iqty;
                break;
            }

            break;

			/* Missiles */
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:

			/* Hack -- ignore invalid missiles */
			if (item->tval != b_ptr->ammo_tval) break;

			/* Hack -- ignore worthless missiles */
			if (item->value <= 0) break;

			/* Count them */
			amt_missile += item->iqty;

			break;
		}
	}


	/*** Process the Spells and Prayers ***/

	/* Handle "satisfy hunger" -> infinite food */
	if ((borg_spell_fail(2, 0, FALSE) < 15) || (borg_prayer_fail(1, 5, FALSE) < 15))
	{
		amt_food += 1000;
	}

	/* Handle "identify" -> infinite identifies */
	if ((borg_spell_fail(2, 4, FALSE) < 50) || 
		(borg_prayer_fail(5, 2, FALSE) < 50))
	{
		amt_ident += 1000;
	}

	/* Handle "word of recall" -> infinite recall */
	if (borg_spell_legal(5, 4) || borg_prayer_legal(4, 4))
	{
		amt_recall += 1000;
	}

#if 0
	/* This is commented out, because we really can't rely on magic for this!! */
	/* Handle "phase door" */
	/* Heavily underrate it, so we need scrolls!! DvE */
	if (borg_spell_legal(0, 2) || borg_prayer_legal(4, 0))
	{
		amt_phase += b_ptr->lev/10;
	}

	/* Handle "teleport" */
	/* Heavily underrate it, so we need scrolls or staffs!! DvE */
	if (borg_spell_legal(1, 5) || borg_prayer_legal(1, 1) || borg_prayer_legal(4, 1))
	{
		amt_teleport += b_ptr->lev / 25;
		amt_escape += b_ptr->lev / 25;
	}

	/* Handle "healing" */
	if (borg_prayer_legal(3, 2))
	{
		amt_ez_heal += b_ptr->lev / 2;
	}
#endif

	/* Handle "haste self" */
	if ((borg_spell_fail(3, 3, FALSE) < 20) ||
		(borg_spell_fail(7, 3, FALSE) < 20))
	{
		amt_speed += 1000;
	}

	/* Handle "detect traps" */
	if (borg_prayer_legal(0, 5))
	{
		amt_detect_trap += 1000;
	}

	/* Handle "detect doors" */
	if (borg_prayer_legal(0, 6))
	{
		amt_detect_door += 1000;
	}

	/* Handle "detection" */
	if (borg_prayer_legal(5, 1))
	{
		amt_detect_door += 1000;
		amt_detect_trap += 1000;
	}

	/* Handle "detect traps/doors" */
	if (borg_spell_legal(0, 7))
	{
		amt_detect_door += 1000;
		amt_detect_trap += 1000;
	}

	/* Handle "enchant weapon" */
	if (borg_prayer_legal(7, 3))
	{
		amt_enchant_to_h += 1000;
		amt_enchant_to_d += 1000;
		amt_enchant_weapon +=1000;
	}

	/* Handle "enchant armor" */
	if (borg_prayer_legal(7, 4))
	{
		amt_enchant_to_a += 1000;
		amt_enchant_armor +=1000;
	}

	/* handle "remove curse" */
	if (borg_prayer_legal(1, 6))
	{
		amt_decurse += 1000;
	}

	/* Handle "dispel curse" */
	if (borg_prayer_legal(7, 2))
	{
		amt_star_decurse += 1000;
	}

	/* Handle "restoration" */
	if (borg_prayer_legal(6, 3))
	{
		amt_fix_stat[A_STR] += 1000;
		amt_fix_stat[A_INT] += 1000;
		amt_fix_stat[A_WIS] += 1000;
		amt_fix_stat[A_DEX] += 1000;
		amt_fix_stat[A_CON] += 1000;
		amt_fix_stat[A_CHR] += 1000;
	}

	/* Handle "rememberance" */
	if (borg_prayer_legal(6, 4))
	{
		amt_fix_exp += 1000;
	}

   /* apw Handle "protection from evil" */
    if (borg_prayer_legal(2, 4))
    {
        amt_pfe += 1000;
    }

    /* apw Handle "rune of protection" glyph" */
    if (borg_prayer_legal(3, 4))
    {
        amt_glyph += 1000;
    }

    /* Handle stinking cloud (until powerful) -RML */
    if (p_ptr->lev < 25 && borg_spell_legal(0, 8))
    {
        amt_ball_attack += 100;
    }

    /* Handle frost ball -RML */
    if (borg_spell_fail(3, 0, FALSE) < 10)
    {
        amt_cold_ball += 100;
    }

    /* Handle fire ball -RML */
    if (borg_spell_fail(3, 4, FALSE) < 10)
    {
        amt_fire_ball += 100;
    }

    /* Handle cloud kill -RML */
    if (borg_spell_fail(8, 1, FALSE) < 10)
    {
        amt_ball_attack += 100;
    }

    /* Handle acid ball -RML */
    if (borg_spell_fail(8, 2, FALSE) < 10)
    {
        amt_acid_ball += 100;
    }

    /* Handle ice storm -RML */
    if (borg_spell_fail(8, 3, FALSE) < 10)
    {
        amt_ball_attack += 100;
    }

    /* Handle meteor swarm -RML */
    if (borg_spell_fail(8, 4, FALSE) < 10)
    {
        amt_ball_attack += 100;
    }

    /* Handle mana storm -RML */
    if (borg_spell_fail(8, 5, FALSE) < 10)
    {
        amt_ball_attack += 100;
    }

    /* Handle orb of draining -RML */
    if (borg_prayer_fail(2, 1, FALSE) < 10)
    {
        amt_ball_attack += 100;
    }

    /* Handle teleport other (mage) -RML */
    if (borg_spell_fail(3, 2, FALSE) < 10)
    {
        amt_teleport_away += 100;
    }

    /* Handle teleport other (priest) -RML */
    if (borg_prayer_fail(4, 2, FALSE) < 10)
    {
        amt_teleport_away += 100;
    }

	/*** Process the Needs ***/

	/* No need for fuel XXX XXX XXX */
	if (borg_items[INVEN_LITE].name1) amt_fuel += 1000;

	/* No need to *buy* stat increase potions */
    if (b_ptr->stat_cur[A_STR] >= 18+80 || borg_stat_maxed[A_STR]) amt_add_stat[A_STR] += 1000;
    if (b_ptr->stat_cur[A_INT] >= 18+80 || borg_stat_maxed[A_INT]) amt_add_stat[A_INT] += 1000;
    if (b_ptr->stat_cur[A_WIS] >= 18+80 || borg_stat_maxed[A_WIS]) amt_add_stat[A_WIS] += 1000;
    if (b_ptr->stat_cur[A_DEX] >= 18+80 || borg_stat_maxed[A_DEX]) amt_add_stat[A_DEX] += 1000;
    if (b_ptr->stat_cur[A_CON] >= 18+80 || borg_stat_maxed[A_CON]) amt_add_stat[A_CON] += 1000;
    if (b_ptr->stat_cur[A_CHR] >= 18+80 || borg_stat_maxed[A_CHR]) amt_add_stat[A_CHR] += 1000;

	/* No need to *buy* stat repair potions XXX XXX XXX */
	if (!borg_base_fix_stat[A_STR]) amt_fix_stat[A_STR] += 1000;
	if (!borg_base_fix_stat[A_INT]) amt_fix_stat[A_INT] += 1000;
	if (!borg_base_fix_stat[A_WIS]) amt_fix_stat[A_WIS] += 1000;
	if (!borg_base_fix_stat[A_DEX]) amt_fix_stat[A_DEX] += 1000;
	if (!borg_base_fix_stat[A_CON]) amt_fix_stat[A_CON] += 1000;
	if (!borg_base_fix_stat[A_CHR]) amt_fix_stat[A_CHR] += 1000;

	/* No need for experience repair below level 20 */
	if (borg_happy_depth < 20) amt_fix_exp += 1000;

    /* No need for temporary speed */
    if (b_ptr->pspeed >= 150)
	{
		amt_speed += 1000;
		amt_rod_speed += 1000;
	}
}

/*
 * Helper function -- clear the borg swap weapon flags
 */
static void borg_clear_swap_weapon(void)
{
	borg_weapon_swap_slay_animal = FALSE;
	borg_weapon_swap_slay_evil = FALSE;
	borg_weapon_swap_slay_undead = FALSE;
	borg_weapon_swap_slay_demon = FALSE;
	borg_weapon_swap_slay_orc = FALSE;
	borg_weapon_swap_slay_troll = FALSE;
	borg_weapon_swap_slay_giant = FALSE;
	borg_weapon_swap_slay_dragon = FALSE;
	borg_weapon_swap_kill_dragon = FALSE;
	borg_weapon_swap_impact = FALSE;
	borg_weapon_swap_brand_acid = FALSE;
	borg_weapon_swap_brand_elec = FALSE;
	borg_weapon_swap_brand_fire = FALSE;
	borg_weapon_swap_brand_cold = FALSE;
	borg_weapon_swap_brand_pois = FALSE;
	borg_weapon_swap_see_infra = FALSE;
	borg_weapon_swap_slow_digest = FALSE;
	borg_weapon_swap_aggravate = FALSE;
	borg_weapon_swap_teleport = FALSE;
	borg_weapon_swap_regenerate = FALSE;
	borg_weapon_swap_telepathy = FALSE;
	borg_weapon_swap_lite = FALSE;
	borg_weapon_swap_see_invis = FALSE;
	borg_weapon_swap_ffall = FALSE;
	borg_weapon_swap_free_act = FALSE;
	borg_weapon_swap_hold_life = FALSE;
	borg_weapon_swap_immune_fire = FALSE;
	borg_weapon_swap_immune_acid = FALSE;
	borg_weapon_swap_immune_cold = FALSE;
	borg_weapon_swap_immune_elec = FALSE;
	borg_weapon_swap_resist_acid = FALSE;
	borg_weapon_swap_resist_elec = FALSE;
	borg_weapon_swap_resist_fire = FALSE;
	borg_weapon_swap_resist_cold = FALSE;
	borg_weapon_swap_resist_pois = FALSE;
	borg_weapon_swap_resist_conf = FALSE;
	borg_weapon_swap_resist_sound = FALSE;
	borg_weapon_swap_resist_lite = FALSE;
	borg_weapon_swap_resist_dark = FALSE;
	borg_weapon_swap_resist_chaos = FALSE;
	borg_weapon_swap_resist_disen = FALSE;
	borg_weapon_swap_resist_shard = FALSE;
	borg_weapon_swap_resist_nexus = FALSE;
	borg_weapon_swap_resist_blind = FALSE;
	borg_weapon_swap_resist_neth = FALSE;
	borg_weapon_swap_speed = 0;
	borg_weapon_swap_blows = 0;
	decurse_borg_weapon_swap =-1;
}

static void borg_set_swap_weapon_flags(auto_item *item)
{
	/* various slays */
	if (item->flags1 & TR1_SLAY_ANIMAL) borg_weapon_swap_slay_animal = TRUE;
	if (item->flags1 & TR1_SLAY_EVIL)   borg_weapon_swap_slay_evil = TRUE;
	if (item->flags1 & TR1_SLAY_UNDEAD) borg_weapon_swap_slay_undead = TRUE;
	if (item->flags1 & TR1_SLAY_DEMON)  borg_weapon_swap_slay_demon = TRUE;
	if (item->flags1 & TR1_SLAY_ORC)    borg_weapon_swap_slay_orc = TRUE;
	if (item->flags1 & TR1_SLAY_TROLL)  borg_weapon_swap_slay_troll = TRUE;
	if (item->flags1 & TR1_SLAY_GIANT)  borg_weapon_swap_slay_giant = TRUE;
	if (item->flags1 & TR1_SLAY_DRAGON) borg_weapon_swap_slay_dragon = TRUE;
	if (item->flags1 & TR1_KILL_DRAGON) borg_weapon_swap_kill_dragon = TRUE;
	if (item->flags3 & TR3_IMPACT)      borg_weapon_swap_impact = TRUE;
	if (item->flags1 & TR1_BRAND_ACID)  borg_weapon_swap_brand_acid = TRUE;
	if (item->flags1 & TR1_BRAND_ELEC)  borg_weapon_swap_brand_elec = TRUE;
	if (item->flags1 & TR1_BRAND_FIRE)  borg_weapon_swap_brand_fire = TRUE;
	if (item->flags1 & TR1_BRAND_COLD)  borg_weapon_swap_brand_cold = TRUE;
	if (item->flags1 & TR1_BRAND_POIS)	borg_weapon_swap_brand_pois = TRUE;

	/* Affect infravision */
	if (item->flags1 & TR1_INFRA) borg_weapon_swap_see_infra += item->pval;
		
	/* Affect various skills */
	
	/* Affect speed */
	if (item->flags1 & TR1_SPEED) borg_weapon_swap_speed = item->pval;

	/* Affect blows */
	if (item->flags1 & TR1_BLOWS) borg_weapon_swap_blows = item->pval;

	/* Various flags */
	if (item->flags3 & TR3_SLOW_DIGEST) borg_weapon_swap_slow_digest = TRUE;
	if (item->flags3 & TR3_AGGRAVATE) borg_weapon_swap_aggravate = TRUE;
	if (item->flags3 & TR3_TELEPORT) borg_weapon_swap_teleport = TRUE;
	if (item->flags3 & TR3_REGEN) borg_weapon_swap_regenerate = TRUE;
	if (item->flags3 & TR3_TELEPATHY) borg_weapon_swap_telepathy = TRUE;
	if (item->flags3 & TR3_LITE) borg_weapon_swap_lite = TRUE;
	if (item->flags3 & TR3_SEE_INVIS) borg_weapon_swap_see_invis = TRUE;
	if (item->flags3 & TR3_FEATHER) borg_weapon_swap_ffall = TRUE;
	if (item->flags3 & TR3_FREE_ACT) borg_weapon_swap_free_act = TRUE;
	if (item->flags3 & TR3_HOLD_LIFE) borg_weapon_swap_hold_life = TRUE;

	/* Immunity flags */
	/* if you are immune you automaticly resist */
	if (item->flags2 & TR2_IM_FIRE)
	{
		borg_weapon_swap_immune_fire = TRUE;
		borg_weapon_swap_resist_fire = TRUE;
	}
	if (item->flags2 & TR2_IM_ACID)
	{
		borg_weapon_swap_immune_acid = TRUE;
		borg_weapon_swap_resist_acid = TRUE;
	}
	if (item->flags2 & TR2_IM_COLD)
	{
		borg_weapon_swap_immune_cold = TRUE;
		borg_weapon_swap_resist_cold = TRUE;
	}
	if (item->flags2 & TR2_IM_ELEC)
	{
		borg_weapon_swap_immune_elec = TRUE;
		borg_weapon_swap_resist_elec = TRUE;
	}

	/* Resistance flags */
	if (item->flags2 & TR2_RES_ACID) borg_weapon_swap_resist_acid = TRUE;
	if (item->flags2 & TR2_RES_ELEC) borg_weapon_swap_resist_elec = TRUE;
	if (item->flags2 & TR2_RES_FIRE) borg_weapon_swap_resist_fire = TRUE;
	if (item->flags2 & TR2_RES_COLD) borg_weapon_swap_resist_cold = TRUE;
	if (item->flags2 & TR2_RES_POIS) borg_weapon_swap_resist_pois = TRUE;
	if (item->flags2 & TR2_RES_CONFU) borg_weapon_swap_resist_conf = TRUE;
	if (item->flags2 & TR2_RES_SOUND) borg_weapon_swap_resist_sound = TRUE;
	if (item->flags2 & TR2_RES_LITE) borg_weapon_swap_resist_lite = TRUE;
	if (item->flags2 & TR2_RES_DARK) borg_weapon_swap_resist_dark = TRUE;
	if (item->flags2 & TR2_RES_CHAOS) borg_weapon_swap_resist_chaos = TRUE;
	if (item->flags2 & TR2_RES_DISEN) borg_weapon_swap_resist_disen = TRUE;
	if (item->flags2 & TR2_RES_SHARD) borg_weapon_swap_resist_shard = TRUE;
	if (item->flags2 & TR2_RES_NEXUS) borg_weapon_swap_resist_nexus = TRUE;
	if (item->flags2 & TR2_RES_BLIND) borg_weapon_swap_resist_blind = TRUE;
	if (item->flags2 & TR2_RES_NETHR) borg_weapon_swap_resist_neth = TRUE;
	if (item->flags3 & TR3_LIGHT_CURSE) decurse_borg_weapon_swap = 0;
	if (item->flags3 & TR3_HEAVY_CURSE) decurse_borg_weapon_swap = 1;

	/* XXX XXX XX -- Sustain flags */
}

/*
 * Helper function -- notice the player swap weapon
 */
static void borg_notice_borg_weapon_swap(void)
{
	int i;
	int b_i = 0;

	s32b v =-1L;
	s32b b_v = 0L;

	int dam, damage;
	auto_item *item;

	/* Hack -- Reset the borg_armour_swap */
	borg_weapon_swap = 0;


	/*** Process the inventory ***/
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];

		/* reset counter */
		v= -1L;
		dam =0;
		damage =0;

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- skip un-aware items */
		if (!item->kind) continue;

		if (!item->able && 
			!streq(item->note, "{average}") &&
			!streq(item->note, "{good}") &&
			!streq(item->note, "{excellent}") &&
			!streq(item->note, "{special}")) continue;



		/* Clear all the swap weapon flags as I look at each one. */
		borg_clear_swap_weapon();

		/* Analyze the item */
		switch (item->tval)
		{
			/* weapons */
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				borg_set_swap_weapon_flags(item);
#if 0
				/* calculating the value of the swap weapon. */
				damage = (item->dd * (item->ds) + (b_ptr->to_d + item->to_d) *35L);
				/* Reward "damage" */
//				v += (damage);

				/* Reward "bonus to hit" */
//				v += ((b_ptr->to_h + item->to_h)*25L);
				dam = damage * (1 + borg_weapon_swap_blows);
			
				/* assume 2x base damage for x% of creatures */
				dam = damage * 2 * (1 + borg_weapon_swap_blows);

				if (!my_slay_animal && borg_weapon_swap_slay_animal) v += (dam*2) /2;
				if (!my_slay_evil && borg_weapon_swap_slay_evil) v +=  (dam*5) /2;
			
				/* assume 3x base damage for x% of creatures */
				dam = damage * 3 * b_ptr->num_blow;
				if (!my_slay_undead && borg_weapon_swap_slay_undead) v += (dam*5) /2;
				if (!my_slay_demon && borg_weapon_swap_slay_demon) v += (dam*3) /2;
				if (!my_slay_orc && borg_weapon_swap_slay_orc) v += (dam*2) /2;
				if (!my_slay_troll && borg_weapon_swap_slay_troll) v += (dam*3) /2;
				if (!my_slay_giant && borg_weapon_swap_slay_giant) v += (dam*4) /2;
				if (!my_slay_dragon && !my_kill_dragon && borg_weapon_swap_slay_dragon) v += (dam*6) /2;
				if (!my_brand_acid && borg_weapon_swap_brand_acid) v += (dam*4) /2;
				if (!my_brand_elec && borg_weapon_swap_brand_elec) v += (dam*4) /2;
				if (!my_brand_fire && borg_weapon_swap_brand_fire) v += (dam*3) /2;
				if (!my_brand_cold && borg_weapon_swap_brand_cold) v += (dam*3) /2;
				if (!my_brand_pois && borg_weapon_swap_brand_pois) v += (dam*4) /2;

				/* assume 5x base damage for x% of creatures */
				dam = damage  * 5 * (1 + borg_weapon_swap_blows);
				if (!my_kill_dragon && borg_weapon_swap_kill_dragon) v += (dam*11) /2;
#endif
				/* Award speed */
				v += borg_weapon_swap_speed * 500L;

				if (borg_weapon_swap_aggravate) v -= 50000L;
				if (borg_weapon_swap_teleport) v -= 100000L;
				if (decurse_borg_weapon_swap != -1) v -= 5000L;
				if (!b_ptr->lite && borg_weapon_swap_lite) v += 1000L;
				if (!b_ptr->see_inv && borg_weapon_swap_see_invis) v += 2500L;
				if (!b_ptr->free_act && borg_weapon_swap_free_act) v += 5000L;
				if (!b_ptr->hold_life && (b_ptr->lev < 50) && borg_weapon_swap_hold_life) v += 5000L;
				if (!b_ptr->immune_fire && borg_weapon_swap_immune_fire) v += 30000L;
				if (!b_ptr->immune_acid && borg_weapon_swap_immune_acid) v += 40000L;
				if (!b_ptr->immune_cold && borg_weapon_swap_immune_cold) v += 15000L;
				if (!b_ptr->immune_elec && borg_weapon_swap_immune_elec) v += 2000L;
				if (!b_ptr->resist_fire && borg_weapon_swap_resist_fire) v += 3000L;
				if (!b_ptr->resist_acid && borg_weapon_swap_resist_acid) v += 4000L;
				if (!b_ptr->resist_cold && borg_weapon_swap_resist_cold) v += 1500L;
				if (!b_ptr->resist_elec && borg_weapon_swap_resist_elec) v += 2000L;
#if 0
				/* extra bonus for getting all basic resist */
				if (borg_weapon_swap_resist_fire &&
					borg_weapon_swap_resist_acid &&
					borg_weapon_swap_resist_elec &&
					borg_weapon_swap_resist_cold) v +=  5000L;
#endif
				if (!b_ptr->resist_pois && borg_weapon_swap_resist_pois) v += 10000L;
				if (!b_ptr->resist_confu && borg_weapon_swap_resist_conf) v += 2500L;
				if (!b_ptr->resist_sound && borg_weapon_swap_resist_sound) v += 500L;
				if (!b_ptr->resist_lite && borg_weapon_swap_resist_lite) v += 100L;
				if (!b_ptr->resist_dark && borg_weapon_swap_resist_dark) v += 100L;
				if (!b_ptr->resist_chaos && borg_weapon_swap_resist_chaos) v += 1000L;
				if (!b_ptr->resist_disen && borg_weapon_swap_resist_disen) v += 2500L;
				if (!b_ptr->resist_shard && borg_weapon_swap_resist_shard) v += 1000L;
				if (!b_ptr->resist_nexus && borg_weapon_swap_resist_nexus) v += 50L;
				if (!b_ptr->resist_blind && borg_weapon_swap_resist_blind) v += 2500L;
				if (!b_ptr->resist_nethr && borg_weapon_swap_resist_neth) v += 2750L;
				if (!b_ptr->resist_fear && borg_weapon_swap_resist_fear) v += 1000L;

				// Mega-Hack -- resists (level 60)
				// its possible that he will get a sword and a cloak
				// both with the same high resist and keep each based
				// on that resist.  We want him to check to see
				// that the other swap does not already have the high resist.
				if (!b_ptr->resist_nethr  && (b_ptr->max_depth+1 >= 60) &&
					borg_weapon_swap_resist_neth) v += 100000L;
				if (!b_ptr->resist_disen && (b_ptr->max_depth+1 >= 60) &&
					borg_weapon_swap_resist_disen) v += 100000L;

				/* some artifacts would make good back ups for their activation */

				/* skip usless ones */
				if (v <= 0) continue;

				/* collect the best one */
				if (v < b_v) continue;
				
				/* track it */
				b_i = i;
				b_v = v;
			}
		}
	}

	/* mark the swap item and its value */
	if (b_i != 0) borg_weapon_swap_value = b_v;
	else borg_weapon_swap_value = 0;
	borg_weapon_swap = b_i;

	/* Now that we know who the best swap is lets set our swap
	 * flags and get a move on
	 */
	
	/*** Process the best inven item ***/
	item = &borg_items[b_i];
	xb_ptr->need_enchant_to_d += (xb_ptr->max_enchant_weapon - item->to_d);
	xb_ptr->need_enchant_to_h += (xb_ptr->max_enchant_weapon - item->to_h);

	/* Clear all the swap weapon flags */
	borg_clear_swap_weapon();
	borg_set_swap_weapon_flags(item);
}

static void borg_reset_swap_armour_flags(void)
{
	borg_armour_swap_slay_animal = FALSE;
	borg_armour_swap_slay_evil = FALSE;
	borg_armour_swap_slay_undead = FALSE;
	borg_armour_swap_slay_demon = FALSE;
	borg_armour_swap_slay_orc = FALSE;
	borg_armour_swap_slay_troll = FALSE;
	borg_armour_swap_slay_giant = FALSE;
	borg_armour_swap_slay_dragon = FALSE;
	borg_armour_swap_kill_dragon = FALSE;
	borg_armour_swap_impact = FALSE;
	borg_armour_swap_brand_acid = FALSE;
	borg_armour_swap_brand_elec = FALSE;
	borg_armour_swap_brand_fire = FALSE;
	borg_armour_swap_brand_cold = FALSE;
	borg_armour_swap_see_infra = FALSE;
	borg_armour_swap_slow_digest = FALSE;
	borg_armour_swap_aggravate = FALSE;
	borg_armour_swap_teleport = FALSE;
	borg_armour_swap_regenerate = FALSE;
	borg_armour_swap_telepathy = FALSE;
	borg_armour_swap_lite = FALSE;
	borg_armour_swap_see_invis = FALSE;
	borg_armour_swap_ffall = FALSE;
	borg_armour_swap_free_act = FALSE;
	borg_armour_swap_hold_life = FALSE;
	borg_armour_swap_immune_fire = FALSE;
	borg_armour_swap_immune_acid = FALSE;
	borg_armour_swap_immune_cold = FALSE;
	borg_armour_swap_immune_elec = FALSE;
	borg_armour_swap_resist_acid = FALSE;
	borg_armour_swap_resist_elec = FALSE;
	borg_armour_swap_resist_fire = FALSE;
	borg_armour_swap_resist_cold = FALSE;
	borg_armour_swap_resist_pois = FALSE;
	borg_armour_swap_resist_conf = FALSE;
	borg_armour_swap_resist_sound = FALSE;
	borg_armour_swap_resist_lite = FALSE;
	borg_armour_swap_resist_dark = FALSE;
	borg_armour_swap_resist_chaos = FALSE;
	borg_armour_swap_resist_disen = FALSE;
	borg_armour_swap_resist_shard = FALSE;
	borg_armour_swap_resist_nexus = FALSE;
	borg_armour_swap_resist_blind = FALSE;
	borg_armour_swap_resist_neth = FALSE;
	borg_armour_swap_blows = 0;
	borg_armour_swap_speed = 0;
	decurse_borg_armour_swap = -1;
}

static void borg_set_swap_armour_flags(auto_item *item)
{
	if (item->flags1 & TR1_SLAY_ANIMAL) borg_armour_swap_slay_animal = TRUE;
	if (item->flags1 & TR1_SLAY_EVIL)   borg_armour_swap_slay_evil = TRUE;
	if (item->flags1 & TR1_SLAY_UNDEAD) borg_armour_swap_slay_undead = TRUE;
	if (item->flags1 & TR1_SLAY_DEMON)  borg_armour_swap_slay_demon = TRUE;
	if (item->flags1 & TR1_SLAY_ORC)    borg_armour_swap_slay_orc = TRUE;
	if (item->flags1 & TR1_SLAY_TROLL)  borg_armour_swap_slay_troll = TRUE;
	if (item->flags1 & TR1_SLAY_GIANT)  borg_armour_swap_slay_giant = TRUE;
	if (item->flags1 & TR1_SLAY_DRAGON) borg_armour_swap_slay_dragon = TRUE;
	if (item->flags1 & TR1_KILL_DRAGON) borg_armour_swap_kill_dragon = TRUE;
	if (item->flags3 & TR3_IMPACT)      borg_armour_swap_impact = TRUE;
	if (item->flags1 & TR1_BRAND_ACID)  borg_armour_swap_brand_acid = TRUE;
	if (item->flags1 & TR1_BRAND_ELEC)  borg_armour_swap_brand_elec = TRUE;
	if (item->flags1 & TR1_BRAND_FIRE)  borg_armour_swap_brand_fire = TRUE;
	if (item->flags1 & TR1_BRAND_COLD)  borg_armour_swap_brand_cold = TRUE;

	/* Affect infravision */
	if (item->flags1 & TR1_INFRA) borg_armour_swap_see_infra += item->pval;
	/* Affect various skills */
	/* Affect speed */
	if (item->flags1 & TR1_SPEED) borg_armour_swap_speed += item->pval;

	/* Affect blows */
	if (item->flags1 & TR1_BLOWS) borg_armour_swap_blows = item->pval;


	/* Various flags */
	if (item->flags3 & TR3_SLOW_DIGEST) borg_armour_swap_slow_digest = TRUE;
	if (item->flags3 & TR3_AGGRAVATE) borg_armour_swap_aggravate = TRUE;
	if (item->flags3 & TR3_TELEPORT) borg_armour_swap_teleport = TRUE;
	if (item->flags3 & TR3_REGEN) borg_armour_swap_regenerate = TRUE;
	if (item->flags3 & TR3_TELEPATHY) borg_armour_swap_telepathy = TRUE;
	if (item->flags3 & TR3_LITE) borg_armour_swap_lite = TRUE;
	if (item->flags3 & TR3_SEE_INVIS) borg_armour_swap_see_invis = TRUE;
	if (item->flags3 & TR3_FEATHER) borg_armour_swap_ffall = TRUE;
	if (item->flags3 & TR3_FREE_ACT) borg_armour_swap_free_act = TRUE;
	if (item->flags3 & TR3_HOLD_LIFE) borg_armour_swap_hold_life = TRUE;

	/* Immunity flags */
	/* if you are immune you automaticly resist */
	if (item->flags2 & TR2_IM_FIRE)
	{
		borg_armour_swap_immune_fire = TRUE;
		borg_armour_swap_resist_fire = TRUE;
	}
	if (item->flags2 & TR2_IM_ACID)
	{
		borg_armour_swap_immune_acid = TRUE;
		borg_armour_swap_resist_acid = TRUE;
	}
	if (item->flags2 & TR2_IM_COLD)
	{
		borg_armour_swap_immune_cold = TRUE;
		borg_armour_swap_resist_cold = TRUE;
	}
	if (item->flags2 & TR2_IM_ELEC)
	{
		borg_armour_swap_immune_elec = TRUE;
		borg_armour_swap_resist_elec = TRUE;
	}

	/* Resistance flags */
	if (item->flags2 & TR2_RES_ACID) borg_armour_swap_resist_acid = TRUE;
	if (item->flags2 & TR2_RES_ELEC) borg_armour_swap_resist_elec = TRUE;
	if (item->flags2 & TR2_RES_FIRE) borg_armour_swap_resist_fire = TRUE;
	if (item->flags2 & TR2_RES_COLD) borg_armour_swap_resist_cold = TRUE;
	if (item->flags2 & TR2_RES_POIS) borg_armour_swap_resist_pois = TRUE;
	if (item->flags2 & TR2_RES_CONFU) borg_armour_swap_resist_conf = TRUE;
	if (item->flags2 & TR2_RES_SOUND) borg_armour_swap_resist_sound = TRUE;
	if (item->flags2 & TR2_RES_LITE) borg_armour_swap_resist_lite = TRUE;
	if (item->flags2 & TR2_RES_DARK) borg_armour_swap_resist_dark = TRUE;
	if (item->flags2 & TR2_RES_CHAOS) borg_armour_swap_resist_chaos = TRUE;
	if (item->flags2 & TR2_RES_DISEN) borg_armour_swap_resist_disen = TRUE;
	if (item->flags2 & TR2_RES_SHARD) borg_armour_swap_resist_shard = TRUE;
	if (item->flags2 & TR2_RES_NEXUS) borg_armour_swap_resist_nexus = TRUE;
	if (item->flags2 & TR2_RES_BLIND) borg_armour_swap_resist_blind = TRUE;
	if (item->flags2 & TR2_RES_NETHR) borg_armour_swap_resist_neth = TRUE;
	if (item->flags3 & TR3_LIGHT_CURSE) decurse_borg_armour_swap = 0;
	if (item->flags3 & TR3_HEAVY_CURSE) decurse_borg_armour_swap = 1;

	if (item->flags2 & TR2_RES_NETHR) borg_armour_swap_resist_neth = TRUE;

	/* Sustain flags */
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

	/* Hack -- Reset the borg_armour_swap */
	borg_armour_swap = 0;


	/*** Process the inventory ***/
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];

		/* reset counter */
		v= -1L;
		dam =0;
		damage =0;

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- skip un-aware items */
		if (!item->kind) continue;

		if (!item->able && 
			!streq(item->note, "{average}") &&
			!streq(item->note, "{good}") &&
			!streq(item->note, "{excellent}") &&
			!streq(item->note, "{special}")) continue;



		/* Clear all the swap weapon flags as I look at each one. */
		borg_reset_swap_armour_flags();

		/* Analyze the item */
		switch (item->tval)
		{
			/* ARMOUR TYPE STUFF */
			case TV_BOOTS:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_RING:
			case TV_AMULET:
			{
				borg_set_swap_armour_flags(item);

				/* Award speed */
				v += borg_weapon_swap_speed * 500L;
				
				/* Hack -- award extra blows */
				v += borg_armour_swap_blows * 500L;
				
				if (borg_armour_swap_aggravate) v -= 50000L;
				if (borg_armour_swap_teleport) v -= 100000L;
				if (decurse_borg_armour_swap != -1) v -= 5000L;
				if (!b_ptr->lite && borg_armour_swap_lite) v += 1000L;
				if (!b_ptr->see_inv && borg_armour_swap_see_invis) v += 2500L;
				if (!b_ptr->free_act && borg_armour_swap_free_act) v += 5000L;
				if (!b_ptr->hold_life && (b_ptr->lev < 50) && borg_armour_swap_hold_life) v += 5000L;
				if (!b_ptr->immune_fire && borg_armour_swap_immune_fire) v += 30000L;
				if (!b_ptr->immune_acid && borg_armour_swap_immune_acid) v += 40000L;
				if (!b_ptr->immune_cold && borg_armour_swap_immune_cold) v += 15000L;
				if (!b_ptr->immune_elec && borg_armour_swap_immune_elec) v += 20000L;
				if (!b_ptr->resist_fire && borg_armour_swap_resist_fire) v += 3000L;
				if (!b_ptr->resist_acid && borg_armour_swap_resist_acid) v += 4000L;
				if (!b_ptr->resist_cold && borg_armour_swap_resist_cold) v += 1500L;
				if (!b_ptr->resist_elec && borg_armour_swap_resist_elec) v += 2000L;
				if (!b_ptr->resist_pois && borg_armour_swap_resist_pois) v += 10000L;
				if (!b_ptr->resist_confu && borg_armour_swap_resist_conf) v += 2500L;
				if (!b_ptr->resist_sound && borg_armour_swap_resist_sound) v += 500L;
				if (!b_ptr->resist_lite && borg_armour_swap_resist_lite) v += 100L;
				if (!b_ptr->resist_dark && borg_armour_swap_resist_dark) v += 100L;
				if (!b_ptr->resist_chaos && borg_armour_swap_resist_chaos) v += 1000L;
				if (!b_ptr->resist_disen && borg_armour_swap_resist_disen) v += 2500L;
				if (!b_ptr->resist_shard && borg_armour_swap_resist_shard) v += 1000L;
				if (!b_ptr->resist_nexus && borg_armour_swap_resist_nexus) v += 50L;
				if (!b_ptr->resist_blind && borg_armour_swap_resist_blind) v += 2500L;
				if (!b_ptr->resist_nethr && borg_armour_swap_resist_neth) v += 2750L;
				if (!b_ptr->resist_fear && borg_armour_swap_resist_fear) v += 1000L;

				// Mega-Hack -- resists (level 60)
				// its possible that he will get a sword and a cloak
				// both with the same high resist and keep each based
				// on that resist.  We want him to check to see
				// that the other swap does not already have the high resist.
				if (!b_ptr->resist_nethr  && b_ptr->max_depth+1 >= 60  &&
					!borg_weapon_swap_resist_neth &&
					borg_armour_swap_resist_neth) v += 50000L;
				if (!b_ptr->resist_disen && b_ptr->max_depth+1 >= 60 &&
					!borg_weapon_swap_resist_disen &&
					borg_armour_swap_resist_disen) v += 50000L;

				/* some artifacts would make good back ups for their activation */
			}

			/* skip usless ones */
			if (v <= 0) continue;

			/* collect the best one */
			if ((b_i >=0) && (v < b_v)) continue;
            
			/* track it */
			b_i = i;
			b_v = v;
			
		}
	}
	if (b_i != 0) borg_armour_swap_value = b_v;
	else borg_armour_swap_value = 0;
	borg_armour_swap = b_i;

	/* Now that we know who the best swap is lets set our swap
	 * flags and get a move on
	 */

	/*** Process the best inven item ***/
	item = &borg_items[b_i];
	xb_ptr->need_enchant_to_a += (xb_ptr->max_enchant_armor - item->to_a);

	/* Clear all the swap weapon flags */
	borg_reset_swap_armour_flags();
	borg_set_swap_armour_flags(item);	
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
        borg_notice_borg_weapon_swap();
        borg_notice_armour_swap();
    }
}

/* XXX Should be in borg1.c, or something */
static sint home_see_inv[INVEN_TOTAL-INVEN_WIELD];
static sint home_teleport[INVEN_TOTAL-INVEN_WIELD];
static sint home_free_act[INVEN_TOTAL-INVEN_WIELD];
static sint home_slow_digest[INVEN_TOTAL-INVEN_WIELD];
static sint home_aggravate[INVEN_TOTAL-INVEN_WIELD];
static sint home_regenerate[INVEN_TOTAL-INVEN_WIELD];
static sint home_ffall[INVEN_TOTAL-INVEN_WIELD];
static sint home_hold_life[INVEN_TOTAL-INVEN_WIELD];
static sint home_telepathy[INVEN_TOTAL-INVEN_WIELD];
static sint home_lite[INVEN_TOTAL-INVEN_WIELD];

static sint home_immune_acid[INVEN_TOTAL-INVEN_WIELD];
static sint home_immune_elec[INVEN_TOTAL-INVEN_WIELD];
static sint home_immune_fire[INVEN_TOTAL-INVEN_WIELD];
static sint home_immune_cold[INVEN_TOTAL-INVEN_WIELD];

static sint home_resist_acid[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_elec[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_fire[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_cold[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_pois[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_fear[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_lite[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_dark[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_blind[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_confu[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_sound[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_shard[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_nexus[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_nethr[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_chaos[INVEN_TOTAL-INVEN_WIELD];
static sint home_resist_disen[INVEN_TOTAL-INVEN_WIELD];

static sint home_sustain_str[INVEN_TOTAL-INVEN_WIELD];
static sint home_sustain_int[INVEN_TOTAL-INVEN_WIELD];
static sint home_sustain_wis[INVEN_TOTAL-INVEN_WIELD];
static sint home_sustain_con[INVEN_TOTAL-INVEN_WIELD];
static sint home_sustain_dex[INVEN_TOTAL-INVEN_WIELD];
static sint home_sustain_chr[INVEN_TOTAL-INVEN_WIELD];

static sint home_boost_stat[INVEN_TOTAL-INVEN_WIELD][6];
static sint home_boost_speed[INVEN_TOTAL-INVEN_WIELD];

static sint home_edged_unblessed[INVEN_TOTAL-INVEN_WIELD];

static sint home_ac[INVEN_TOTAL-INVEN_WIELD];
static sint home_damage[INVEN_TOTAL-INVEN_WIELD];

/*
 * Helper function -- determine if an item is worth storing in the home
 *
 * Created by -RML
 */
static bool borg_home_item_good(const auto_item *item)
{
    /* Hack -- ignore lite sources */
    if (item->tval == TV_LITE) return (FALSE);

    /* Artifacts are good */
    if (item->name1) return (TRUE);

#if 0
    /* Rings of Speed are good */
    if (item->tval == TV_RING && item->sval == SV_RING_SPEED) return (TRUE);
#endif

#if 0
    /* Boots of Speed are good */
    if (item->name2 == EGO_SPEED) return (TRUE);
#endif

    /* Cloaks of Aman are good */
    if (item->name2 == EGO_AMAN) return (TRUE);

	/* Elvenkind Armor is good */
	if (item->name2 == EGO_ELVENKIND) return (TRUE);

    /* Dragon Scale Mail is good */
    if (item->tval == TV_DRAG_ARMOR) return (TRUE);

    /* Ego-items are good, for a while */
    if (b_ptr->lev < 40 && item->name2) return (TRUE);

    /* Assume bad */
    return (FALSE);
}

/*
 * Helper function -- notice the home equipment
 *
 * Extensively modified by -RML
 */
static void borg_notice_home_aux1(void)
{
	int i;

    int slot;

    int ac;

	u32b	f1, f2, f3;

	auto_item *item;

	auto_shop *shop = &borg_shops[7];


    for (slot = 0; slot < INVEN_TOTAL-INVEN_WIELD; slot++)
    {

        /* Clear all the flags */
        home_see_inv[slot] = 0;
        home_teleport[slot] = 0;
        home_free_act[slot] = 0;
        home_slow_digest[slot] = 0;
        home_aggravate[slot] = 0;
        home_regenerate[slot] = 0;
        home_ffall[slot] = 0;
        home_hold_life[slot] = 0;
        home_telepathy[slot] = 0;
        home_lite[slot] = 0;

        home_immune_acid[slot] = 0;
        home_immune_elec[slot] = 0;
        home_immune_fire[slot] = 0;
        home_immune_cold[slot] = 0;

        home_resist_acid[slot] = 0;
        home_resist_elec[slot] = 0;
        home_resist_fire[slot] = 0;
        home_resist_cold[slot] = 0;
        home_resist_pois[slot] = 0;
        home_resist_fear[slot] = 0;
        home_resist_lite[slot] = 0;
        home_resist_dark[slot] = 0;
        home_resist_blind[slot] = 0;
        home_resist_confu[slot] = 0;
        home_resist_sound[slot] = 0;
        home_resist_shard[slot] = 0;
        home_resist_nexus[slot] = 0;
        home_resist_nethr[slot] = 0;
        home_resist_chaos[slot] = 0;
        home_resist_disen[slot] = 0;

        home_sustain_str[slot] = 0;
        home_sustain_int[slot] = 0;
        home_sustain_wis[slot] = 0;
        home_sustain_con[slot] = 0;
        home_sustain_dex[slot] = 0;
        home_sustain_chr[slot] = 0;

        for (i = 0; i < 6; i++)
            home_boost_stat[slot][i] = 0;

        home_boost_speed[slot] = 0;

        home_edged_unblessed[slot] = 0;

        home_ac[slot] = 0;
        home_damage[slot] = 0;

    }
	
	/* Hack, use player flags to extract the race intrinsics */
	player_flags(&f1, &f2, &f3);


    for (slot = 0; slot < INVEN_TOTAL-INVEN_WIELD; slot++)
    {

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) home_slow_digest[slot] = 100;
		if (f3 & (TR3_FEATHER)) home_ffall[slot] = 100;
		if (f3 & (TR3_LITE)) home_lite[slot] = 100;
		if (f3 & (TR3_REGEN)) home_regenerate[slot] = 100;
		if (f3 & (TR3_TELEPATHY)) home_telepathy[slot] = 100;
		if (f3 & (TR3_SEE_INVIS)) home_see_inv[slot] = 100;
		if (f3 & (TR3_FREE_ACT)) home_free_act[slot] += 100;
		if (f3 & (TR3_HOLD_LIFE)) home_hold_life[slot] = 100;

		/* Weird flags */
		if (f3 & (TR3_BLESSED)) b_ptr->bless_blade = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) home_immune_fire[slot] = 100;
		if (f2 & (TR2_IM_ACID)) home_immune_acid[slot] = 100;
		if (f2 & (TR2_IM_COLD)) home_immune_cold[slot] = 100;
		if (f2 & (TR2_IM_ELEC)) home_immune_elec[slot] = 100;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID)) home_resist_acid[slot] += 100;
		if (f2 & (TR2_RES_ELEC)) home_resist_elec[slot] += 100;
		if (f2 & (TR2_RES_FIRE)) home_resist_fire[slot] += 100;
		if (f2 & (TR2_RES_COLD)) home_resist_cold[slot] += 100;
		if (f2 & (TR2_RES_POIS)) home_resist_pois[slot] += 100;
		if (f2 & (TR2_RES_FEAR)) home_resist_fear[slot] += 100;
		if (f2 & (TR2_RES_LITE)) home_resist_lite[slot] += 100;
		if (f2 & (TR2_RES_DARK)) home_resist_dark[slot] += 100;
		if (f2 & (TR2_RES_BLIND)) home_resist_blind[slot] += 100;
		if (f2 & (TR2_RES_CONFU)) home_resist_confu[slot] += 100;
		if (f2 & (TR2_RES_SOUND)) home_resist_sound[slot] += 100;
		if (f2 & (TR2_RES_SHARD)) home_resist_shard[slot] += 100;
		if (f2 & (TR2_RES_NEXUS)) home_resist_nexus[slot] += 100;
		if (f2 & (TR2_RES_NETHR)) home_resist_nethr[slot] += 100;
		if (f2 & (TR2_RES_CHAOS)) home_resist_chaos[slot] += 100;
		if (f2 & (TR2_RES_DISEN)) home_resist_disen[slot] += 100;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) home_sustain_str[slot] += 100;
		if (f2 & (TR2_SUST_INT)) home_sustain_int[slot] += 100;
		if (f2 & (TR2_SUST_WIS)) home_sustain_wis[slot] += 100;
		if (f2 & (TR2_SUST_DEX)) home_sustain_dex[slot] += 100;
		if (f2 & (TR2_SUST_CON)) home_sustain_con[slot] += 100;
		if (f2 & (TR2_SUST_CHR)) home_sustain_chr[slot] += 100;

#if 0	
        /* Elf */
        if (b_ptr->prace == 2) home_resist_lite[slot] += 100;

        /* Hobbit */
        if (b_ptr->prace == 3) home_sustain_dex[slot] += 100;

        /* Gnome */
        if (b_ptr->prace == 4) home_free_act[slot] += 100;

        /* Dwarf */
        if (b_ptr->prace == 5) home_resist_blind[slot] += 100;

        /* Half-Orc */
        if (b_ptr->prace == 6) home_resist_dark[slot] += 100;

        /* Half-Troll */
        if (b_ptr->prace == 7) home_sustain_str[slot] += 100;

        /* Dunadan */
        if (b_ptr->prace == 8) home_sustain_con[slot] += 100;

        /* High Elf */
        if (b_ptr->prace == 9) home_resist_lite[slot] += 100;
        if (b_ptr->prace == 9) home_see_inv[slot] += 100;

        /* Warrior */
        if (p_ptr->pclass == CLASS_WARRIOR)
        {
            if (b_ptr->lev >= 30) home_resist_fear[slot] += 100;
        }
#endif
    }

    /* Scan the home */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
		item = &shop->ware[i];

		/* Empty items are a free slot */
		if (!item->iqty) continue;

		/* Hack -- skip un-aware items */
		if (!item->kind) continue;

        /* Skip non-good items */
        if (!borg_home_item_good(item)) continue;

        slot = borg_wield_slot(item);

        /* Skip unwieldable items */
        if (!slot) continue;

        /* Find slot index */
        slot = slot - INVEN_WIELD;

		/* Affect stats */
        if (item->flags1 & TR1_STR)
        {
            if (item->pval > home_boost_stat[slot][A_STR])
                home_boost_stat[slot][A_STR] = item->pval;
        }

        if (item->flags1 & TR1_INT)
        {
            if (item->pval > home_boost_stat[slot][A_INT])
                home_boost_stat[slot][A_INT] = item->pval;
        }

        if (item->flags1 & TR1_WIS)
        {
            if (item->pval > home_boost_stat[slot][A_WIS])
                home_boost_stat[slot][A_WIS] = item->pval;
        }

        if (item->flags1 & TR1_DEX)
        {
            if (item->pval > home_boost_stat[slot][A_DEX])
                home_boost_stat[slot][A_DEX] = item->pval;
        }

        if (item->flags1 & TR1_CON)
        {
            if (item->pval > home_boost_stat[slot][A_CON])
                home_boost_stat[slot][A_CON] = item->pval;
        }

        if (item->flags1 & TR1_CHR)
        {
            if (item->pval > home_boost_stat[slot][A_CHR])
                home_boost_stat[slot][A_CHR] = item->pval;
        }


#if 0
		/* Affect infravision */
		if (item->flags1 & TR1_INFRA) b_ptr->see_infra += item->pval;
#endif

#if 0
		/* Affect stealth */
		if (item->flags1 & TR1_STEALTH) b_ptr->skill_stl += item->pval;
#endif

#if 0
		/* Affect searching ability (factor of five) */
		if (item->flags1 & TR1_SEARCH) b_ptr->skill_srh += (item->pval * 5);
#endif

#if 0
		/* Affect searching frequency (factor of five) */
		if (item->flags1 & TR1_SEARCH) b_ptr->skill_fos += (item->pval * 5);
#endif

#if 0
		/* Affect digging (factor of 20) */
		if (item->flags1 & TR1_TUNNEL) b_ptr->skill_dig += (item->pval * 20);
#endif

		/* Affect speed */
        if (item->flags1 & TR1_SPEED)
        {
            if (item->pval > home_boost_speed[slot])
                home_boost_speed[slot] = item->pval;
        }

#if 0
		/* Affect blows */
		if (item->flags1 & TR1_BLOWS) extra_blows += item->pval;
#endif

#if 0
		/* Boost shots */
		if (item->flags1 & TR1_SHOTS) extra_shots += item->pval;
#endif

#if 0
		/* Boost might */
		if (item->flags1 & TR1_MIGHT) extra_might += item->pval;
#endif

		/* Good flags */
        if (item->flags3 & TR3_SLOW_DIGEST) home_slow_digest[slot] += 1;
        if (item->flags3 & TR3_FEATHER) home_ffall[slot] += 1;
        if (item->flags3 & TR3_LITE) home_lite[slot] += 1;
        if (item->flags3 & TR3_REGEN) home_regenerate[slot] += 1;
        if (item->flags3 & TR3_TELEPATHY) home_telepathy[slot] += 1;
        if (item->flags3 & TR3_SEE_INVIS) home_see_inv[slot] += 1;
        if (item->flags3 & TR3_FREE_ACT) home_free_act[slot] += 1;
        if (item->flags3 & TR3_HOLD_LIFE) home_hold_life[slot] += 1;

		/* Bad flags */
        if (item->flags3 & TR3_TELEPORT) home_teleport[slot] += 1;
        if (item->flags3 & TR3_AGGRAVATE) home_aggravate[slot] += 1;

		/* Immunity flags */
        if (item->flags2 & TR2_IM_FIRE) home_immune_fire[slot] += 1;
        if (item->flags2 & TR2_IM_ACID) home_immune_acid[slot] += 1;
        if (item->flags2 & TR2_IM_COLD) home_immune_cold[slot] += 1;
        if (item->flags2 & TR2_IM_ELEC) home_immune_elec[slot] += 1;

		/* Resistance flags */
        if (item->flags2 & TR2_RES_ACID) home_resist_acid[slot] += 1;
        if (item->flags2 & TR2_RES_ELEC) home_resist_elec[slot] += 1;
        if (item->flags2 & TR2_RES_FIRE) home_resist_fire[slot] += 1;
        if (item->flags2 & TR2_RES_COLD) home_resist_cold[slot] += 1;
        if (item->flags2 & TR2_RES_POIS) home_resist_pois[slot] += 1;
        if (item->flags2 & TR2_RES_FEAR) home_resist_fear[slot] += 1;
        if (item->flags2 & TR2_RES_LITE) home_resist_lite[slot] += 1;
        if (item->flags2 & TR2_RES_DARK) home_resist_dark[slot] += 1;
        if (item->flags2 & TR2_RES_BLIND) home_resist_blind[slot] += 1;
        if (item->flags2 & TR2_RES_CONFU) home_resist_confu[slot] += 1;
        if (item->flags2 & TR2_RES_SOUND) home_resist_sound[slot] += 1;
        if (item->flags2 & TR2_RES_SHARD) home_resist_shard[slot] += 1;
        if (item->flags2 & TR2_RES_NEXUS) home_resist_nexus[slot] += 1;
        if (item->flags2 & TR2_RES_NETHR) home_resist_nethr[slot] += 1;
        if (item->flags2 & TR2_RES_CHAOS) home_resist_chaos[slot] += 1;
        if (item->flags2 & TR2_RES_DISEN) home_resist_disen[slot] += 1;

		/* Sustain flags */
        if (item->flags2 & TR2_SUST_STR) home_sustain_str[slot] += 1;
        if (item->flags2 & TR2_SUST_INT) home_sustain_int[slot] += 1;
        if (item->flags2 & TR2_SUST_WIS) home_sustain_wis[slot] += 1;
        if (item->flags2 & TR2_SUST_DEX) home_sustain_dex[slot] += 1;
        if (item->flags2 & TR2_SUST_CON) home_sustain_con[slot] += 1;
        if (item->flags2 & TR2_SUST_CHR) home_sustain_chr[slot] += 1;

        /* Priests are penalized for non-blessed edged weapons */
        if ((b_ptr->pclass == 2) &&
            ((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
            (!(item->flags3 & TR3_BLESSED)))
        {
            home_edged_unblessed[slot] += 1;
        }

        /* Reward high AC or AC bonus */
        ac = item->ac + item->to_a;
        if (ac > home_ac[slot]) home_ac[slot] = ac;

        /* XXX XXX XXX Need to reward damage from weapons */
    }
}


/*
 * Helper function -- notice the home inventory
 */
static void borg_notice_home_aux2(void)
{
	int i;

	auto_item *item;

	auto_shop *shop = &borg_shops[7];


	/*** Reset counters ***/

	/* Reset basic */
	num_fuel = 0;
	num_food = 0;
	num_ident = 0;
    num_star_ident = 0;
	num_recall = 0;
	num_phase = 0;
	num_escape = 0;
	num_teleport = 0;

	/* Reset healing */
	num_pot_heal = 0;
	num_pot_star_heal = 0;
	num_pot_life = 0;
	num_ez_heal = 0;
	num_cure_critical = 0;
	num_cure_serious = 0;

	/* Reset missiles */
	num_missile = 0;

	/* Reset speed */
	num_speed = 0;

	/* Reset mana */
	num_restore_mana = 0;
	num_staff_mana = 0;

    /* Reset attack spells -RML */
    num_ball_attack = 0;
    num_teleport_away = 0;

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

	/* Reset enchantment */
	num_enchant_to_a = 0;
	num_enchant_to_d = 0;
	num_enchant_to_h = 0;

	/* Reset remove curse */
	num_decurse = 0;
	num_star_decurse = 0;


    /* Reset space -RML */
    num_stack = 0;

	/*** Process the inventory ***/

	/* Scan the home */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
		item = &shop->ware[i];

		/* Empty items are a free slot */
		if (!item->iqty) continue;

		/* Hack -- skip un-aware items */
		if (!item->kind) continue;

        /* Calculate space */
        amt_stack += item->iqty * borg_obj_stack(item->tval, item->sval,
                                                 item->discount);

		/* Analyze the item */
		switch (item->tval)
		{
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
				case SV_FOOD_RATION:
				num_food += item->iqty;
				break;
			}

			break;


			/* Potions */
			case TV_POTION:

			/* Analyze */
			switch (item->sval)
			{
				case SV_POTION_HEALING:
				num_pot_heal += item->iqty;
				break;

				case SV_POTION_STAR_HEALING:
				num_pot_star_heal += item->iqty;
				break;

				case SV_POTION_LIFE:
				num_pot_life += item->iqty;

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

				case SV_POTION_SPEED:
				num_speed += item->iqty;
				break;

				case SV_POTION_RESTORE_MANA:
				num_restore_mana += item->iqty;
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

				case SV_SCROLL_REMOVE_CURSE:
				num_decurse += item->iqty;
				break;

				case SV_SCROLL_STAR_REMOVE_CURSE:
				num_star_decurse += item->iqty;
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

				case SV_ROD_SPEED:
				num_speed += item->iqty * 100;
				break;

				case SV_ROD_HEALING:
				num_ez_heal += item->iqty * 100;
				break;

                /* Attack rods -RML */
                case SV_ROD_ACID_BALL:
                case SV_ROD_ELEC_BALL:
                case SV_ROD_FIRE_BALL:
                case SV_ROD_COLD_BALL:
                num_ball_attack += item->iqty * 100;
                break;

                /* Teleport away -RML */
                case SV_ROD_TELEPORT_AWAY:
                num_teleport_away += item->iqty * 100;
                break;
			}

			break;


            /* Wands -RML */
            case TV_WAND:

			/* Analyze */
			switch (item->sval)
			{
                case SV_WAND_TELEPORT_AWAY:
                num_teleport_away += item->pval;
                break;

                case SV_WAND_STINKING_CLOUD:
                /* XXX Use until powerful */
                if (b_ptr->lev < 25)
                    num_ball_attack += item->pval;
                break;

                case SV_WAND_ACID_BALL:
                case SV_WAND_ELEC_BALL:
                case SV_WAND_FIRE_BALL:
                case SV_WAND_COLD_BALL:
                case SV_WAND_DRAGON_FIRE:
                case SV_WAND_DRAGON_COLD:
                case SV_WAND_DRAGON_BREATH:
                num_ball_attack += item->pval;
                break;
			}

			break;


			/* Staffs */
			case TV_STAFF:

			/* Analyze */
			switch (item->sval)
			{
				case SV_STAFF_IDENTIFY:
				/* Hack -- penalize lots of staffs with few charges */
				if ((item->iqty < 5) && (item->pval > 4))
				{
					num_ident += item->iqty * item->pval;
				}
				break;

				case SV_STAFF_TELEPORTATION:
				num_teleport += item->iqty * item->pval;
				break;

				case SV_STAFF_SPEED:
				num_speed += item->iqty * item->pval;
				break;

				case SV_STAFF_HEALING:
				num_ez_heal += item->iqty * item->pval;
				break;

				case SV_STAFF_THE_MAGI:
				num_staff_mana += item->iqty * item->pval;
				break;

				case SV_STAFF_REMOVE_CURSE:
				num_decurse += item->iqty * item->pval;
				break;
			}

			break;


			/* Flasks */
			case TV_FLASK:

			/* Use as fuel for lanterns */
			num_fuel += item->iqty;

			break;


			/* Missiles */
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:

			/* Hack -- ignore invalid missiles */
			if (item->tval != b_ptr->ammo_tval) break;

			/* Hack -- ignore worthless missiles */
			if (item->value <= 0) break;

			/* Count them */
			num_missile += item->iqty;

			break;
		}
	}


	/*** Process the Spells and Prayers ***/

	/* Handle "satisfy hunger" -> infinite food */
	if ((borg_spell_fail(2, 0, FALSE) < 15) || 
		(borg_prayer_fail(1, 5, FALSE) < 15))
	{
		num_food += 1000;
	}

	/* Handle "identify" -> infinite identifies */
	if ((borg_spell_fail(2, 4, FALSE) < 50) || 
		(borg_prayer_fail(5, 2,FALSE) < 50))
	{
		num_ident += 1000;
	}

	/* Handle "word of recall" -> infinite recall */
	if (borg_spell_legal(5, 4) || borg_prayer_legal(4, 4))
	{
		num_recall += 1000;
	}

	/* Handle "phase door" */
	if ((borg_spell_fail(0, 2, FALSE) < 10) || 
		(borg_prayer_fail(4, 0, FALSE) < 10))
	{
		num_phase += b_ptr->lev;
	}

	/* Handle "teleport" */
	if ((borg_spell_fail(1, 5, FALSE) < 6) || 
		(borg_prayer_fail(1, 1, FALSE) < 6))
	{
		num_teleport += b_ptr->lev / 5;
		num_escape += b_ptr->lev / 5;
	}

	/* Handle "healing" */
	if (borg_prayer_fail(3, 2, FALSE) < 6)
	{
		num_ez_heal += b_ptr->lev / 2;
	}

	/* Handle "haste self" */
	if (borg_spell_fail(3, 3, FALSE) < 10)
	{
		num_speed += 1000;
	}

	/* Handle "enchant weapon" */
	if (borg_prayer_legal(7, 3))
	{
		num_enchant_to_h += 1000;
		num_enchant_to_d += 1000;
	}

	/* Handle "enchant armor" */
	if (borg_prayer_legal(7, 4))
	{
		num_enchant_to_a += 1000;
	}

	/* handle "remove curse" */
	if (borg_prayer_legal(1, 6))
	{
		num_decurse += 1000;
	}

	/* Handle "dispel curse" */
	if (borg_prayer_legal(7, 2))
	{
		num_star_decurse += 1000;
	}

	/* Handle "restoration" */
	if (borg_prayer_legal(6, 3))
	{
		num_fix_stat[A_STR] += 1000;
		num_fix_stat[A_INT] += 1000;
		num_fix_stat[A_WIS] += 1000;
		num_fix_stat[A_DEX] += 1000;
		num_fix_stat[A_CON] += 1000;
		num_fix_stat[A_CHR] += 1000;
	}

	/* Handle "rememberance" */
	if (borg_prayer_legal(6, 4))
	{
		num_fix_exp += 1000;
	}

    /* Handle stinking cloud (until powerful) -RML */
    if (p_ptr->lev < 25 && borg_spell_legal(0, 8))
    {
        num_ball_attack += 1000;
    }

    /* Handle frost ball -RML */
    if (borg_spell_legal(3, 0))
    {
        num_ball_attack += 1000;
    }

    /* Handle fire ball -RML */
    if (borg_spell_legal(3, 4))
    {
        num_ball_attack += 1000;
    }

    /* Handle cloud kill -RML */
    if (borg_spell_legal(8, 1))
    {                        
        num_ball_attack += 1000;
    }

    /* Handle acid ball -RML */
    if (borg_spell_legal(8, 2))
    {
        num_ball_attack += 1000;
    }

    /* Handle ice storm -RML */
    if (borg_spell_legal(8, 3))
    {
        num_ball_attack += 1000;
    }

    /* Handle meteor swarm -RML */
    if (borg_spell_legal(8, 4))
    {
        num_ball_attack += 1000;
    }

    /* Handle mana storm -RML */
    if (borg_spell_legal(8, 5))
    {
        num_ball_attack += 1000;
    }

    /* Handle orb of draining -RML */
    if (borg_prayer_legal(2, 1))
    {
        num_ball_attack += 1000;
    }

    /* Handle teleport other (mage) -RML */
    if (borg_spell_fail(3, 2, FALSE) < 10)
    {
        num_teleport_away += 1000;
    }

    /* Handle teleport other (priest) -RML */
    if (borg_prayer_fail(4, 2, FALSE) < 10)
    {
        num_teleport_away += 1000;
    }

	/*** Process the Needs ***/

	/* No need for fuel XXX XXX XXX */
	if (borg_items[INVEN_LITE].name1) num_fuel += 1000;

	/* Hack -- No need for stat repair */
	if (b_ptr->sustain_str) num_fix_stat[A_STR] += 1000;
	if (b_ptr->sustain_int) num_fix_stat[A_INT] += 1000;
	if (b_ptr->sustain_wis) num_fix_stat[A_WIS] += 1000;
	if (b_ptr->sustain_dex) num_fix_stat[A_DEX] += 1000;
	if (b_ptr->sustain_con) num_fix_stat[A_CON] += 1000;
	if (b_ptr->sustain_chr) num_fix_stat[A_CHR] += 1000;

	/* No need for cure serious wounds */
	if (b_ptr->lev > 30) num_cure_serious += 1000;

	/* Warriors need no mana */
	if (b_ptr->pclass == CLASS_WARRIOR) num_restore_mana += 1000;
}


/*
 * Extract various bonuses
 */
void borg_notice_home(void)
{
    /* borg_note("# Noticing home"); */

	/* Notice the home equipment */
	borg_notice_home_aux1();

	/* Notice the home inventory */
	borg_notice_home_aux2();
}



/*
 * Helper function -- calcuate extra "power" of an artifact
 *
 * This only takes into account artifact activations that the
 * Borg likes to use.
 */
static s32b borg_power_aux_artifact(int name1)
{
	/* Analyze */
	switch (name1)
	{
		/* Phial */
		case ART_GALADRIEL:
		{
			/* Extra power if we cannot call light */
			if (!borg_spell_legal(0, 3) &&
			    !borg_prayer_legal(0, 4))
			{
				return 100L;
			}

			return 10L;
		}

		/* Star */
		case ART_ELENDIL:
		{
			/* Extra power if we cannot magic map */
			if (!borg_prayer_legal(2, 6))
			{
				return 50L;
			}

			return 20L;
		}

		/* Arkenstone */
		case ART_THRAIN:
		{
			/* Extra power if we cannot cast clairvoyance */
			if (!borg_prayer_legal(5, 4))
			{
				return 2000L;
			}

			return 200L;
		}

		/* Ingwe */
		case ART_INGWE:
		{
			/* Dispel Evil */
			return 5000L;
		}

		/* Narya */
		case ART_NARYA:
		{
			/* Fireball */
			return 500L;
		}

		/* Nenya */
		case ART_NENYA:
		{
			/* coldball */
			return 1000L;
		}

		/* Vilya */
		case ART_VILYA:
		{
			/* Elec ball */
			return 1500L;
		}

		/* Bladeturner */
		case ART_BLADETURNER:
		{
			/* Extra power if we cannot cast resistance */
			if (!borg_spell_legal(4, 4))
			{
				return 100000L;
			}

			return 50000L;
		}

		/* Colluin */
		case ART_COLLUIN:
		{
			/* Extra power if we cannot cast resistance */
			if (!borg_spell_legal(4, 4))
			{
				return 10000L;
			}

			return 1000L;
		}

		/* Cammithrim */
		case ART_CAMMITHRIM:
		{
			return 10L;
		}

		/* Paurhach */
		case ART_PAURHACH:
		{
			return 100L;
		}

		/* Paurnimmen */
		case ART_PAURNIMMEN:
		{
			return 70L;
		}

		/* Pauraegen */
		case ART_PAURAEGEN:
		{
			return 50L;
		}

		/* Paurnen */
		case ART_PAURNEN:
		{
			return 60L;
		}

		/* Fingolfin */
		case ART_FINGOLFIN:
		{
			return 500L;
		}

		/* Narthanc */
		case ART_NARTHANC:
		{
			return 100L;
		}

		/* Nimthanc */
		case ART_NIMTHANC:
		{
			return 70L;
		}

		/* Dethanc */
		case ART_DETHANC:
		{
			return 50L;
		}

		/* Rilia */
		case ART_RILIA:
		{
			return 20L;
		}

		/* Belangil */
		case ART_BELANGIL:
		{
			return 50L;
		}

		/* Arunruth */
		case ART_ARUNRUTH:
		{
			return 150L;
		}

		/* Ringil */
		case ART_RINGIL:
		{
			return 200L;
		}

		/* Anduril */
		case ART_ANDURIL:
		{
			return 175L;
		}

		/* Theoden */
		case ART_THEODEN:
		{
			return 250L;
		}

		/* Aeglos */
		case ART_AEGLOS:
		{
			return 250L;
		}

		/* Firestar */
		case ART_FIRESTAR:
		{
			return 200L;
		}

		/* Eriril */
		case ART_ERIRIL:
		{
			/* Extra power if we cannot cast identify */
			if (!borg_spell_legal(2, 4) &&
			    !borg_prayer_legal(5, 2))
			{
				return 5000L;
			}

			return 500L;
		}

		/* Turmil */
		case ART_TURMIL:
		{
			return 150L;
		}

        /* Cubragol */
        case ART_CUBRAGOL:
        {
            return 500L;
        }

		/* Carlammas */
		case ART_CARLAMMAS:
		{
			/* Protection from Evil */
			return 100;
		}

		/* Soulkeeper */
		case ART_SOULKEEPER:
		{
			/* Heal (1000) */
			return (50000);
		}

		/* Celeborn */
		case ART_CELEBORN:
		{
			/* Genocide */
			return (500000);
		}


	}

	/* No extra power */
	return 0L;
}

/*
 * Helper function -- calculate "power" of equipment
 */
static s32b borg_power_aux1(void)
{
	int			hold;
	int			pounds;
	int			damage, dam;

	int			i, speed;

	int			cur_wgt = 0;
	int			max_wgt = 0;

	s32b		value = 0L;

	auto_item		*item;

	/* Obtain the "hold" value */
	hold = adj_str_hold[b_ptr->stat_ind[A_STR]];

	/*** Analyze weapon ***/

	/* Examine current weapon */
	item = &borg_items[INVEN_WIELD];

	/* Weight in pounds */
	pounds = item->weight / 10;

	/* Calculate "average" damage per "normal" blow (times 2) */
	damage = (item->dd * (item->ds + 1) + ((b_ptr->to_d + item->to_d) * 2));

	/* Reward "damage" (not for low-level mages DvE)*/
	if ((b_ptr->pclass != CLASS_MAGE) || (b_ptr->lev > 35))
	{
		value += (b_ptr->num_blow * damage * 500L);
	
		/* assume 2x base damage for x% of creatures */
	    dam = damage * 2 * b_ptr->num_blow;
	    if (my_slay_animal) value += (dam * 2) / 2;
	    if (my_slay_evil)   value += (dam * 5) / 2;

		/* assume 3x base damage for x% of creatures */
		dam = damage  * 3 * b_ptr->num_blow;
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
		if (my_brand_pois)	value += (dam * 4) / 2;

		/* assume 5x base damage for x% of creatures */
		dam = damage  * 5 * b_ptr->num_blow;
		if (my_kill_dragon) value += (dam * 11) / 2;

		/* what ever... Will never get this anyway.  it is only on Grond */
		if (my_impact) value += 5000L;
	}

	/* Reward "bonus to hit" */
	value += ((b_ptr->to_h + item->to_h) * 100L);

	/* Hack -- It is hard to hold a heavy weapon */
	if (hold < pounds) value -= 500000L;

	/*** Analyze bow ***/

	/* Examine current bow */
	item = &borg_items[INVEN_BOW];

	/* Weight in pounds */
	pounds = item->weight / 10;

	/* Calculate "average" damage per "normal" shot (times 2) */
	damage = ((xb_ptr->ammo_sides + 1) + (item->to_d * 2)) * b_ptr->ammo_mult;

	/* Reward "damage" */
	value += (b_ptr->num_fire * damage * 500L);

	/* Reward "bonus to hit" */
	value += ((b_ptr->to_h + item->to_h) * 100L);

	/* Analyze the class */
	switch (b_ptr->pclass)
	{
		/* Prefer crossbows, then bows */
		case CLASS_WARRIOR:
		case CLASS_ROGUE:
		case CLASS_PRIEST:
		case CLASS_PALADIN:
		if (b_ptr->ammo_tval == TV_BOLT) value += 200000L;

		/* Prefer bows */
		case CLASS_MAGE:
		case CLASS_RANGER:
		if (b_ptr->ammo_tval == TV_ARROW) value += 100000L;
	}

	/* Hack -- It is hard to hold a heavy weapon */
	if (hold < pounds) value -= 500000L;


	/*** Reward various things ***/

	/* Hack -- Reward light radius */
	value += (b_ptr->cur_lite * 1000000L);

	/* Hack -- Reward speed */
	speed = b_ptr->pspeed - 110;
	i = 0;
	for (; i < 20 && i < speed; i++) value += 100000L;
	for (; i < 25 && i < speed; i++) value += 75000L;
	for (; i < 30 && i < speed; i++) value += 20000L;
	for (; i < 35 && i < speed; i++) value += 5000L;
	for (; i < speed; i++) value += 500L;

	/* Reward stat bonuses from equipment */
	for (i = 0; i < 6; i++) value += 10 * borg_stat_add[i];

	/* Hack -- Reward strength bonus */
	value += (b_ptr->stat_ind[A_STR] * 1000L);

	/* Hack -- Reward extra hit points */
	value += (adj_con_mhp[b_ptr->stat_ind[A_CON]] * b_ptr->lev * 1000L);

	/* Hack -- Reward intelligence bonus */
	if (mb_ptr->spell_book == TV_MAGIC_BOOK)
	{
		value += (b_ptr->stat_ind[A_INT] * 500L);

		/* Mages, Rangers and rogues like high INT even better DvE */
		if ((b_ptr->pclass == CLASS_MAGE) || 
			(b_ptr->pclass == CLASS_RANGER) ||
			(b_ptr->pclass == CLASS_ROGUE))
		{
			value += (b_ptr->stat_ind[A_INT] * 1000L);
		}

		/* Hack -- Give bonus to low fail rate */
		if (b_ptr->pclass == CLASS_MAGE)
		{
			int minfail = adj_mag_fail[b_ptr->stat_ind[A_INT]];

			/* Increase bonus greatly with low fail rates */
			value += 500000L / (minfail * minfail + 1);
		}
	}

	/* Hack -- Reward wisdom bonus */
	if (mb_ptr->spell_book == TV_PRAYER_BOOK)
	{
		value += (b_ptr->stat_ind[A_WIS] * 500L);

		/* Priests and paladins like high WIS even better */
		if ((b_ptr->pclass == CLASS_PRIEST) ||
			(b_ptr->pclass == CLASS_PALADIN))
		{
			value += (b_ptr->stat_ind[A_WIS] * 1000L);
		}

		/* Hack -- Give bonus to low fail rate */
		if (b_ptr->pclass == CLASS_PRIEST)
		{
			int minfail = adj_mag_fail[b_ptr->stat_ind[A_WIS]];

			/* Increase bonus greatly with low fail rates */
			value += 500000L / (minfail * minfail + 1);
		}
	}

	/* Hack -- Reward charisma bonus */
	value += (b_ptr->stat_ind[A_CHR] * 10L);


	/*** Reward current skills ***/

	/* Hack -- tiny rewards */
	value += (b_ptr->skill_dis * 1L);
	value += (b_ptr->skill_dev * 1L);
	value += (b_ptr->skill_sav * 1L);
	value += (b_ptr->skill_stl * 1L);
	value += (b_ptr->skill_srh * 1L);
	value += (b_ptr->skill_fos * 1L);
	value += (b_ptr->skill_thn * 1L);
	value += (b_ptr->skill_thb * 1L);
	value += (b_ptr->skill_tht * 1L);
	value += (b_ptr->skill_dig * 1L);

	/* Hack -- Reward perfect saving throw */
	if (b_ptr->skill_sav >= 100) value += 50000L;


	/*** Reward current flags ***/

	/* Various flags */
	if (b_ptr->see_inv) value += 5000L;
	if (b_ptr->free_act) value += 10000L;
	if (b_ptr->slow_digest) value += 10L;
	if (b_ptr->regenerate) value += 5000L;
	if (b_ptr->ffall) value += 10L;
	if (b_ptr->hold_life) value += 1000L;
	if (b_ptr->telepathy) value += 50000L;
	if (b_ptr->lite) value += 2000L;

	/* Immunity flags */
	if (b_ptr->immune_acid) value += 80000L;
	if (b_ptr->immune_elec) value += 40000L;
	if (b_ptr->immune_fire) value += 60000L;
	if (b_ptr->immune_cold) value += 30000L;

	/* Hack -- Magic users love immune fire */
	if (b_ptr->msp > 100)
	{
		if (b_ptr->immune_fire) value += 60000L;
	}

	/* Resistance flags */
	if (b_ptr->resist_acid) value += 8000L;
	if (b_ptr->resist_elec) value += 4000L;
	if (b_ptr->resist_fire) value += 6000L;
	if (b_ptr->resist_cold) value += 3000L;
	if (b_ptr->resist_pois) value += 20000L;
	if (b_ptr->resist_fear) value += 2000L;
	if (b_ptr->resist_lite) value += 200L;
	if (b_ptr->resist_dark) value += 200L;
	if (b_ptr->resist_blind) value += 5000L;
	if (b_ptr->resist_confu) value += 5000L;
	if (b_ptr->resist_sound) value += 1000L;
	if (b_ptr->resist_shard) value += 2000L;
	if (b_ptr->resist_nexus) value += 100L;
	if (b_ptr->resist_nethr) value += 5500L;
	if (b_ptr->resist_chaos) value += 2000L;
	if (b_ptr->resist_disen) value += 5000L;

	/* Sustain flags */
	if (b_ptr->sustain_str) value += 500L;
	if (b_ptr->sustain_int) value += 50L;
	if (b_ptr->sustain_wis) value += 50L;
	if (b_ptr->sustain_con) value += 500L;
	if (b_ptr->sustain_dex) value += 500L;
	if (b_ptr->sustain_chr) value += 50L;


	/*** XXX XXX XXX Reward "necessary" flags ***/

	/* Mega-Hack -- See invisible (level 10) */
	if (b_ptr->see_inv && (b_ptr->max_depth + 1 >= 10)) value += 100000L;

	/* Mega-Hack -- Free action, resist fire & acid (level 20) */
	if (b_ptr->max_depth + 1 >= 20)
	{
		if (b_ptr->free_act) value += 100000L;
		if (b_ptr->resist_fire) value += 100000L;
		if (b_ptr->resist_acid) value += 100000L;
	}

	/* Mega-Hack -- Resist cold, elec & pois (level 40) */
	if (b_ptr->max_depth+1 >= 40)
	{
		if (b_ptr->resist_cold) value += 100000L;
		if (b_ptr->resist_elec) value += 100000L;
		if (b_ptr->resist_pois) value += 500000l;
	}

	/* Mega-Hack -- Resist blind & confu (Level 55) */
	if (b_ptr->max_depth + 1 >= 55)
	{
		if (b_ptr->resist_blind) value += 100000L;
		if (b_ptr->resist_confu) value += 100000L;
	}

	/* Mega-Hack -- Resist Nether,Chaos & Disenchant (level 60) */
	if (b_ptr->max_depth + 1 >= 60)
	{
		if (b_ptr->resist_nethr) value += 500000L;
		if (b_ptr->resist_disen) value += 200000L;
		if (b_ptr->resist_chaos) value += 200000L;
	}

	/* Mega-Hack -- Sustain stats are nice at level 100 */
	if (b_ptr->max_depth+1 >= 100)
	{
		if (b_ptr->sustain_str) value += 10000L;
		if (b_ptr->sustain_int) value += 10000L;
		if (b_ptr->sustain_wis) value += 10000L;
		if (b_ptr->sustain_dex) value += 10000L;
		if (b_ptr->sustain_con) value += 10000L;
		if (b_ptr->sustain_chr) value += 10000L;
	}


	/*** Reward powerful armor ***/

	/* Reward armor */
    i = 0;
	for ( ; i < 20 && i < b_ptr->ac + b_ptr->to_a; i++) value += 10000L;
	for ( ; i < 50 && i < b_ptr->ac + b_ptr->to_a; i++) value += 2500L;
	for ( ; i < 100 && i < b_ptr->ac + b_ptr->to_a; i++) value += 1500L;
	for ( ; i < 200 && i < b_ptr->ac + b_ptr->to_a; i++) value += 1000L;
	
	/*** Reward extra artifact powers ***/

	/* Check equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Reward each item */
		value += borg_power_aux_artifact(borg_items[i].name1);
	}

	/*** Penalize various things ***/

	/* Penalize various flags */
	if (b_ptr->teleport) value -= 100000L;
	if (b_ptr->aggravate) value -= 50000L;


	/*** Penalize armor weight ***/

	/* Compute the total armor weight */
	cur_wgt += borg_items[INVEN_BODY].weight;
	cur_wgt += borg_items[INVEN_HEAD].weight;
	cur_wgt += borg_items[INVEN_ARM].weight;
	cur_wgt += borg_items[INVEN_OUTER].weight;
	cur_wgt += borg_items[INVEN_HANDS].weight;
	cur_wgt += borg_items[INVEN_FEET].weight;
	
	/* Determine the weight allowance */
	max_wgt = mb_ptr->spell_weight;

	/* Hack -- heavy armor hurts magic */
	if ((mb_ptr->spell_book) &&
	    (((cur_wgt - max_wgt) / 10) > 0))
	{
		/* Mega-Hack -- Penalize heavy armor which hurts mana */
		value -= (((cur_wgt - max_wgt) / 10) * 100000L);
	}

	/* Determine maximum carrying capacity */
	max_wgt = adj_str_wgt[b_ptr->stat_ind[A_STR]] * 100;

	/* Penalize wearing heavy armor that could slow us */
	if (cur_wgt > max_wgt / 8)
	{
		/* Penalize based on difference */
		value -= (cur_wgt - (max_wgt / 8)) * 50000L;
	}

	/*** Penalize bad magic ***/

	/* Hack -- most gloves hurt magic for spell-casters */
	if (mb_ptr->spell_book == TV_MAGIC_BOOK)
	{
		item = &borg_items[INVEN_HANDS];

		/* Penalize non-usable gloves */
		if (item->iqty &&
		    (!(item->flags3 & TR3_FREE_ACT)) &&
		    (!((item->flags1 & TR1_DEX) && (item->pval > 0))))
		{
			/* Hack -- Major penalty */
			value -= 500000L;
		}
	}

	/* Hack -- most edged weapons hurt magic for priests */
	if (b_ptr->pclass == CLASS_PRIEST)
	{
		item = &borg_items[INVEN_WIELD];

		/* Penalize non-blessed edged weapons */
		if (((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
		    (!(item->flags3 & TR3_BLESSED)))
		{
			/* Hack -- Major penalty */
			value -= 500000L;
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
	int			k, book, max;

	s32b		value = 0L;


	/*** Basic abilities ***/

	/* Reward fuel */
	k = 0;
	for (; k < 5 && k < amt_fuel; k++) value += 60000L;
	for (; k < 10 && k < amt_fuel; k++) value += 60L;

	/* Reward food */
	k = 0;
	for (; k < 5 && k < amt_food; k++) value += 50000L;
	for (; k < 10 && k < amt_food; k++) value += 5000L;

	/* Reward ident */
	k = 0;
	for (; k < 5 && k < amt_ident; k++) value += 51000L;
	for (; k < 20 && k < amt_ident; k++) value += 6000L;
	for (; k < 40 && k < amt_ident; k++) value += 600L;

	/*  Reward *id* apw carry lots of these*/
    k = 0;
    for (; k < 8 && k < amt_star_ident; k++) value += 10000L;
    for (; k < 15 && k < amt_star_ident; k++) value += 2000L;

    /*  apw Reward PFE  carry lots of these*/
    k = 0;
    for (; k < 10 && k < amt_pfe; k++) value += 10000L;
    for (; k < 25 && k < amt_pfe; k++) value += 2000L;

    /*  apw Reward Glyph- Rune of Protection-  carry lots of these*/
    k = 0;
    for (; k < 10 && k < amt_glyph; k++) value += 10000L;
    for (; k < 25 && k < amt_glyph; k++) value += 2000L;

	/* Reward recall */
	k = 0;
	for (; k < 5 && k < amt_recall; k++) value += 50000L;
	for (; k < 10 && k < amt_recall; k++) value += 5000L;
	

	/* Reward phase */
    /* Low level mages *need* phase door */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 15))
	{
		for (k = 0; k < 30 && k < amt_phase; k++) value += 55000L;
	}
	else
	{
		for (k = 0; k < 15 && k < amt_phase; k++) value += 1000L;
	}


	/* Reward escape */
	k = 0;
	for (; k < 5 && k < amt_escape; k++) value += 10000L;

	/* Reward teleport */
	k = 0;
	for (; k < 5 && k < amt_teleport; k++) value += 10000L;
	
	/* Reward teleport staves */ 
    /* But don't use them at the end of the game*/
    if (b_ptr->max_depth < 95)
    {
        k = 0;
        for (; k < 2 && k < amt_teleport_staff; k++) value += 2000L;
    }


	/*** Healing ***/

	/* Reward healing */
	if ((b_ptr->max_depth > 59) || (num_pot_heal == 99))
	{
		for (k = 0; k < 15 && k < amt_pot_heal; k++) value += 8000L;
	}

	if ((b_ptr->max_depth > 79) || (num_pot_star_heal == 99))
	{
		for (k = 0; k < 15 && k < amt_pot_star_heal; k++) value += 10000L;
	}

	if ((b_ptr->max_depth > 98) || (num_pot_life == 99 ))
	{
		for (k = 0; k < 15 && k < amt_pot_life; k++) value += 12000L;
	}
	
	for (k = 0; k < 20 && k < amt_ez_heal; k++) value +=10000L;
    
	/* Restore Mana */
    if (b_ptr->msp > 100)
    {
		if ((b_ptr->max_depth > 79) || (num_restore_mana + amt_mana > 99))
		{			
			for (k = 0; k < 15 && k < amt_mana; k++) value += 8000L;
		}
        
		for (k = 0; k < 100 && k < amt_ez_mana; k++) value += 8000L;
    }

	/* Reward cure critical.  Heavy reward on first 5 */
	k = 0;
	for (; k <  5 && k < amt_cure_critical; k++) value += 5000L;
	for (; k < 15 && k < amt_cure_critical; k++) value += 1000L;

	/* only reward serious if low on crits */
	if (amt_cure_critical < 10)
	{
		k = 0;
		for (; k <  5 && k < amt_cure_serious; k++) value += 50L;
		for (; k < 10 && k < amt_cure_serious; k++) value += 5L;
	}


	/*** Detection ***/

	/* Reward detect trap */
	k = 0;
	for (; k < 1 && k < amt_detect_trap; k++) value += 4000L;

	/* Reward detect door */
	k = 0;
	for (; k < 1 && k < amt_detect_door; k++) value += 2000L;


	/*** Missiles ***/

	/* Reward missiles */
	k = 0;
	max = adj_str_wgt[b_ptr->stat_ind[A_STR]] * 3;

	for (; k < 10 && k < amt_missile && k < max; k++) value += 10000L;
	for (; k < 30 && k < amt_missile && k < max; k++) value += 500L;
	for (; k < 90 && k < amt_missile && k < max; k++) value += 1L;


	/*** Speed ***/

	/* Reward speed */
	k = 0;
	for (; k < 20 && k < amt_speed; k++) value += 1000L;
	k = 0;
	for (; k < 10 && k < amt_rod_speed; k++) value += 10000L;
	
    /*** Misc -RML ***/

    /* Reward ball attack spells */
    k = 0;
    for (; k < 5 && k < amt_ball_attack; k++) value += 2500L;
    for (; k < 10 && k < amt_ball_attack; k++) value += 1000L;
    for (; k < 20 && k < amt_ball_attack; k++) value += 500L;

	/* Reward the elemental balls DvE */
    k = 0;
    for (; k < 5 && k < amt_elec_ball; k++) value += 1000L;
    for (; k < 10 && k < amt_elec_ball; k++) value += 750L;
    for (; k < 20 && k < amt_elec_ball; k++) value += 300L;
	
	k = 0;
    for (; k < 5 && k < amt_cold_ball; k++) value += 1500L;
    for (; k < 10 && k < amt_cold_ball; k++) value += 1000L;
    for (; k < 20 && k < amt_cold_ball; k++) value += 500L;

	k = 0;
    for (; k < 5 && k < amt_acid_ball; k++) value += 2000L;
    for (; k < 10 && k < amt_acid_ball; k++) value += 1500L;
    for (; k < 20 && k < amt_acid_ball; k++) value += 1000L;

	k = 0;
    for (; k < 5 && k < amt_fire_ball; k++) value += 2500L;
    for (; k < 10 && k < amt_fire_ball; k++) value += 2000L;
    for (; k < 20 && k < amt_fire_ball; k++) value += 1500L;

	/* Reward teleport away */
    k = 0;
    for (; k < 5 && k < amt_teleport_away; k++) value += 1000L;
    for (; k < 10 && k < amt_teleport_away; k++) value += 250L;


	/*** Various ***/

	/* apw  -- Reward carrying a staff of holiness/power */
    k = 0;
    for (; k < 1 && k < amt_cool_staff; k++) value += 2000L;

	/* Reward remove curse & *remove curse* */
	/* Only need one */
	if ((decurse_borg_weapon_swap == 0) ||
		(decurse_borg_armour_swap == 0))		
	{
		if (amt_decurse) value += 10000L;
	}

	if ((decurse_borg_weapon_swap == 1) ||
		(decurse_borg_armour_swap == 1) ||
		borg_cursed)
	{
		if (amt_star_decurse) value += 100000L;
	}

	/* Hack -- Reward add stat */
	if (amt_add_stat[A_STR]) value += 50000;
	if (amt_add_stat[A_INT]) value += 50000;
	if (amt_add_stat[A_WIS]) value += 50000;
	if (amt_add_stat[A_DEX]) value += 50000;
	if (amt_add_stat[A_CON]) value += 50000;
	if (amt_add_stat[A_CHR]) value += 50000;

	/* Hack -- Reward fix stat */
	if (amt_fix_stat[A_STR]) value += 10000;
	if (amt_fix_stat[A_INT]) value += 10000;
	if (amt_fix_stat[A_WIS]) value += 10000;
	if (amt_fix_stat[A_DEX]) value += 10000;
	if (amt_fix_stat[A_CON]) value += 10000;
	if (amt_fix_stat[A_CHR]) value += 10000;

	/* Hack -- Restore experience */
	k = 0;
    for (; k < 5 && k < amt_fix_exp; k++) value += 500L;
    for (; k < 10 && k < amt_fix_exp; k++) value += 100L;

	/*** Enchantment ***/

	/* Reward enchant armor */
	if (amt_enchant_to_a && xb_ptr->need_enchant_to_a) value += 300L;

	/* Reward enchant weapon to hit */
	if (amt_enchant_to_h && xb_ptr->need_enchant_to_h) value += 100L;

	/* Reward enchant weapon to damage */
	if (amt_enchant_to_d && xb_ptr->need_enchant_to_d) value += 500L;
	
	/* Reward *enchant weapon*  */
    k = 0;
    for (; k < 1 && k < amt_enchant_weapon; k++) value += 1000L;

    /* Reward *enchant armor*  */
    k = 0;
    for (; k < 1 && k < amt_enchant_armor; k++) value += 1000L;


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
				auto_magic *as = &borg_magics[book][what];

				/* Track minimum level */
				if (as->level < when) when = as->level;
			}

			/* Hack -- Ignore "difficult" normal books DvE*/
			if (when >= b_ptr->max_lev + 1) continue;

			/* Reward the book */
			k = 0;
			for (; k < 1 && k < amt_book[book]; k++) value += 500000L;
			for (; k < 2 && k < amt_book[book]; k++) value += 1000L;
		}
	}

#if 0
    /* Penalize having too much stuff -RML */
    k = 100;
    for (; k < 150 && k < amt_stack; k++) value -= 10L;
    for (; k < 200 && k < amt_stack; k++) value -= 25L;
    for (; k < amt_stack; k++) value -= 50L;
#endif

    /* Return the value */
    return (value);

}


/*
 * Calculate the "power" of the Borg
 */
s32b borg_power(void)
{
	s32b value = 0L;

	/* Process the equipment */
	value += borg_power_aux1();

	/* Process the inventory */
	value += borg_power_aux2();

	/* Add the swap power */
	value += borg_weapon_swap_value;
	value += borg_armour_swap_value;

	/* Return the value */
	return (value);
}




/*
 * Helper function -- calculate power of equipment in the home
 *
 * Extensively modified by -RML
 */
static s32b borg_power_home_aux1(void)
{
    int i, k;

    int slot;

	s32b		value = 0L;

    for (slot = 0; slot < INVEN_TOTAL-INVEN_WIELD; slot++)
    {

        /* Reward speed */
        i = 0;
        for (; i < 4 && i < home_boost_speed[slot]; i++) value += 10000L;
        for (; i < 5 && i < home_boost_speed[slot]; i++) value += 7500L;
        for (; i < 6 && i < home_boost_speed[slot]; i++) value += 4000L;
        for (; i < 7 && i < home_boost_speed[slot]; i++) value += 1500L;
        for (; i < home_boost_speed[slot]; i++) value += 500L;

        /* Reward stat bonuses */
        for (i = 0; i < 6; i++) value += 10L * home_boost_stat[slot][i];

        /* Reward strength bonus */
        value += 50L * home_boost_stat[slot][A_STR];

        /* Reward constitution bonus */
        value += 50L * home_boost_stat[slot][A_STR];

        /* Reward intelligence bonus */
        if (mb_ptr->spell_book == TV_MAGIC_BOOK)
        {
            value += 25L * home_boost_stat[slot][A_INT];

            /* Mages like high INT even better */
            if (b_ptr->pclass == CLASS_MAGE)
                value += 50L * home_boost_stat[slot][A_INT];
        }

        /* Reward wisdom bonus */
        if (mb_ptr->spell_book == TV_PRAYER_BOOK)
        {
            value += 25L * home_boost_stat[slot][A_WIS];

            /* Priests like high WIS even better */
            if (b_ptr->pclass == CLASS_PRIEST)
                value += 50L * home_boost_stat[slot][A_WIS];
        }

        /* Reward dexterity bonus */
        value += 10L * home_boost_stat[slot][A_DEX];

        /* Reward AC */
        value += 1L * home_ac[slot];

        /*** Reward flags ***/

        /* Various flags */
        k = 0;
        for (; k < 1 && k < home_see_inv[slot]; k++) value += 100L;

        k = 0;
        for (; k < 1 && k < home_free_act[slot]; k++) value += 200L;

        k = 0;
        for (; k < 1 && k < home_slow_digest[slot]; k++) value += 1L;

        k = 0;
        for (; k < 1 && k < home_regenerate[slot]; k++) value += 1000L;

        k = 0;
        for (; k < 1 && k < home_ffall[slot]; k++) value += 1L;

        k = 0;
        for (; k < 1 && k < home_hold_life[slot]; k++) value += 200L;

        k = 0;
        for (; k < 1 && k < home_telepathy[slot]; k++) value += 1000L;

#if 0
        k = 0;
        for (; k < 1 && k < home_lite[slot]; k++) value += 40L;
#endif

        /* Immunity flags */
        k = 0;
        for (; k < 1 && k < home_immune_acid[slot]; k++) value += 1500L;

        k = 0;
        for (; k < 1 && k < home_immune_elec[slot]; k++) value += 800L;

        k = 0;
        for (; k < 1 && k < home_immune_fire[slot]; k++) value += 1200L;

        k = 0;
        for (; k < 1 && k < home_immune_cold[slot]; k++) value += 600L;

        /* Resistance flags */
        k = 0;
        for (; k < 1 && k < home_resist_acid[slot]; k++) value += 20L;

        k = 0;
        for (; k < 1 && k < home_resist_elec[slot]; k++) value += 10L;

        k = 0;
        for (; k < 1 && k < home_resist_fire[slot]; k++) value += 20L;

        k = 0;
        for (; k < 1 && k < home_resist_cold[slot]; k++) value += 10L;

        k = 0;
        for (; k < 1 && k < home_resist_pois[slot]; k++) value += 1000L;

        k = 0;
        for (; k < 1 && k < home_resist_fear[slot]; k++) value += 20L;

        k = 0;
        for (; k < 1 && k < home_resist_lite[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_resist_dark[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_resist_blind[slot]; k++) value += 50L;

        k = 0;
        for (; k < 1 && k < home_resist_confu[slot]; k++) value += 100L;

        k = 0;
        for (; k < 1 && k < home_resist_sound[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_resist_shard[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_resist_nexus[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_resist_nethr[slot]; k++) value += 500L;

        k = 0;
        for (; k < 1 && k < home_resist_chaos[slot]; k++) value += 50L;

        k = 0;
        for (; k < 1 && k < home_resist_disen[slot]; k++) value += 1000L;

#if 0
        /* Sustain flags */
        k = 0;
        for (; k < 1 && k < home_sustain_str[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_sustain_int[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_sustain_wis[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_sustain_con[slot]; k++) value += 5L;

        k = 0;
        for (; k < 1 && k < home_sustain_dex[slot]; k++) value += 5L;
    
        k = 0;
        for (; k < 1 && k < home_sustain_chr[slot]; k++) value += 5L;
#endif

        /* Bad things (teleport, aggravate) */
        for (; k < 2 && k < home_teleport[slot]; k++) value -= 1000L;
        for (; k < 2 && k < home_aggravate[slot]; k++) value -= 250L;

        /* Edged weapons are bad for priests */
        for (; k < 2 && k < home_edged_unblessed[slot]; k++) value -= 200L;
    }

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
	int			i, k, book;

	s32b		value = 0L;


	/*** Basic abilities ***/

	/* Collect fuel */
	for (k = 0; k < 10 && k < num_fuel; k++) value += 1000L - k*10L;

	/* Collect food */
	for (k = 0; k < 10 && k < num_food; k++) value += 800L - k*10L;

	/* Collect ident */
	for (k = 0; k < 99 && k < num_ident; k++) value += 500L;

    /* Collect *ident* */
    for (k = 0; k < num_star_ident; k++) value += 750;

	/* Collect recall */
	/* Only if deep enough DvE */
	if (borg_happy_depth > 5)
	{
		for (k = 0; k < 50 && k < num_recall; k++) value += 500L - k*10L;
	}

	/* Collect phase */
	for (k = 0; k < 30 && k < num_phase; k++) value += 200L;

	/* Collect escape */
	for (k = 0; k < 99 && k < num_escape; k++) value += 200L;

	/* Collect teleport */
	for (k = 0; k < 10 && k < num_teleport; k++) value += 200L;


	/*** Healing ***/

	/* Collect healing */
	for (k = 0; k < 99 && k < num_pot_heal; k++) value += 300L;
	for (k = 0; k < 99 && k < num_pot_star_heal; k++) value += 400L;
	for (k = 0; k < 99 && k < num_pot_life; k++) value += 500L;
	for (k = 0; k < 99 && k < num_ez_heal; k++) value += 300L;

	/* Collect cure critical */
	for (k = 0; k < 99 && k < num_cure_critical; k++) value += 150L;

	/*** Missiles ***/

	/* Collect missiles */
	for (k = 0; k < 99 && k < num_missile; k++) value += 10L;


	/*** Speed ***/

	/* Collect speed */
    if (b_ptr->pspeed < 150)
        for (k = 0; k < 99 && k < num_speed; k++) value += 50L;
    
	/*** Restore mana ***/

	/* Reward restore mana */
	for (k = 0; k < 99 && k < num_restore_mana; k++) value += 500L;
	for (k = 0; k < 99 && k < num_staff_mana; k++) value += 300L;


    /*** Misc -RML ***/

    /* Reward ball attack spells */
    k = 0;
    for (; k < 20 && k < num_ball_attack; k++) value += 100L - k*2L;

    /* Reward teleport away */
    k = 0;
    for (; k < 50 && k < num_teleport_away; k++) value += 500L - k*5L;

	/*** Various ***/

	/* Hack -- Collect restore life levels */
	for (k = 0; k < 50 && k < num_fix_exp; k++) value += 500L - k*10L;


	/*** Enchantment ***/

	/* Reward enchant armor */
	for (k = 0; k < 99 && k < num_enchant_to_a; k++) value += 20L - k*1L;


	/*** Hack -- books ***/

	/* Reward books */
	for (book = 0; book < 4; book++)
	{
		/* Collect up to 5 copies of each normal book */
		for (k = 0; k < 5 && k < num_book[book]; k++)
		{
			/* Hack -- only stockpile useful books */
			if (amt_book[book]) value += 500L - k*10L;
		}
	}

	/* Reward remove curse & *remove curse* */
	/* Only need one */
	if (num_decurse) value += 10L;
	if (num_star_decurse) value += 100L;
	


	/*** Mega-Hack -- artifacts ***/

	/* Reward artifacts in the home */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
		auto_item *item = &borg_shops[7].ware[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-artifacts */
		if (!item->name1) continue;

		/* Reward artifacts */
        value += item->value / 250L;
	}

#if 0
    /* Penalize having too much stuff -RML */
    k = 200;
    for (; k < 300 && k < num_stack; k++) value -= 5L;
    for (; k < 400 && k < num_stack; k++) value -= 10L;
    for (; k < num_stack; k++) value -= 25L;
#endif

	/* Return the value */
	return (value);
}


/*
 * Calculate the "power" of the home
 */
s32b borg_power_home(void)
{
	s32b value = 0L;

    /* borg_note("# Powering home"); */

	/* Process the home equipment */
	value += borg_power_home_aux1();

	/* Process the home inventory */
	value += borg_power_home_aux2();

	/* Return the value */
	return (value);
}


/*
 * The borg_danger() function has been split into two seperate
 * functions.  The first, borg_max_danger(), acts much like the
 * original borg_danger, but the effect from "annoyance" is not
 * as great as before.  The max danger is used to determine when
 * the Borg should flee.
 *
 * The second function is still called borg_danger, but instead
 * of assuming the worst, it tries to predict the average danger
 * of a grid.
 *
 * The combination of these two functions should result in a
 * borg that still refuses to allow any possibility of dying, but
 * is also less paranoid about many common, non-threatening
 * situations.
 */

/*
 * Calculate maximum base danger from a monster's physical attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 *
 * We do not take "annoyance" into account in this function.
 */
static int borg_max_danger_aux1(int i, int y, int x)
{
	int k, n = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];

	
	/* Mega-Hack -- unknown monsters */
	if (kill->r_idx >= z_info->r_max) return (1000);


	/* Analyze each physical attack */
	for (k = 0; k < 4; k++)
	{
		int z = 0;

		int method = r_ptr->blow[k].method;
		int effect = r_ptr->blow[k].effect;
		int d_dice = r_ptr->blow[k].d_dice;
		int d_side = r_ptr->blow[k].d_side;
		
		/* Done */
		if (!method) break;

		/* Analyze the attack */
		switch (effect)
		{
			case RBE_HURT:
			z = (d_dice * d_side);
			z -= (z * ((b_ptr->ac < 150) ? b_ptr->ac : 150) / 250);
			break;

			case RBE_POISON:
			z = (d_dice * d_side);
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			break;

			case RBE_UN_BONUS:
			z = (d_dice * d_side);
			if (b_ptr->resist_disen) break;
			break;

			case RBE_UN_POWER:
			z = (d_dice * d_side);
			break;

			case RBE_EAT_GOLD:
			z = (d_dice * d_side);
			break;

			case RBE_EAT_ITEM:
			z = (d_dice * d_side);
			break;

			case RBE_EAT_FOOD:
			z = (d_dice * d_side);
			break;

			case RBE_EAT_LITE:
			z = (d_dice * d_side);
			break;

			case RBE_ACID:
			if (b_ptr->immune_acid) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			break;

			case RBE_ELEC:
			if (b_ptr->immune_elec) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			break;

			case RBE_FIRE:
			if (b_ptr->immune_fire) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			break;

			case RBE_COLD:
			if (b_ptr->immune_cold) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			break;

			case RBE_BLIND:
			z = (d_dice * d_side);
			break;

			case RBE_CONFUSE:
			z = (d_dice * d_side);
			break;

			case RBE_TERRIFY:
			z = (d_dice * d_side);
			break;

			case RBE_PARALYZE:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_STR:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_DEX:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_CON:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_INT:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_WIS:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_CHR:
			z = (d_dice * d_side);
			break;

			case RBE_LOSE_ALL:
			z = (d_dice * d_side);
			break;

			case RBE_SHATTER:
			z = (d_dice * d_side);
			z -= (z * ((b_ptr->ac < 150) ? b_ptr->ac : 150) / 250);
			break;

			case RBE_EXP_10:
			z = (d_dice * d_side);
			break;

			case RBE_EXP_20:
			z = (d_dice * d_side);
			break;

			case RBE_EXP_40:
			z = (d_dice * d_side);
			break;

			case RBE_EXP_80:
			z = (d_dice * d_side);
			break;
		}

		/* Hack -- Apply invulnerability */
		if (b_ptr->invuln) z = 0;

		/* Add in damage */
		n += z;
	}

	/* Check for protection from evil */
	if ((b_ptr->protevil > 0) &&
	    (r_ptr->flags3 & (RF3_EVIL)) &&
	    (b_ptr->lev == 50) &&
	    (r_ptr->level <= 50))
	{
		/* No damage */
		n = 0;
	}

	/* Hack -- Low level mages are easily killed */
	/* So we double the danger DvE */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 15))
	{
		n *= 2;
	}

	/* Return danger */
	return (n);
}


/*
 * Calculate maximum base danger from a monster's spell attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 */
static int borg_max_danger_aux2(int i, int y, int x)
{
	int q, k, n = 0;

	int lev, hp;

	byte spell[96], num = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


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
	hp = kill->curhp;


	/* Analyze the spells */
	for (q = 0; q < num; q++)
	{
		int z = 0;

		/* Cast the spell. */
		switch (spell[q])
		{
			case 96+0:    /* RF4_SHRIEK */
			break;

			case 96+1:    /* RF4_XXX2X4 */
			break;

			case 96+2:    /* RF4_XXX3X4 */
			break;

			case 96+3:    /* RF4_XXX4X4 */
			break;

			case 96+4:    /* RF4_ARROW_1 */
			z = (1 * 6);
			break;

			case 96+5:    /* RF4_ARROW_2 */
			z = (3 * 6);
			break;

			case 96+6:    /* RF4_ARROW_3 */
			z = (5 * 6);
			break;

			case 96+7:    /* RF4_ARROW_4 */
			z = (7 * 6);
			break;

			case 96+8:    /* RF4_BR_ACID */
			if (b_ptr->immune_acid) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			break;

			case 96+9:    /* RF4_BR_ELEC */
			if (b_ptr->immune_elec) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			break;

			case 96+10:    /* RF4_BR_FIRE */
			if (b_ptr->immune_fire) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			break;

			case 96+11:    /* RF4_BR_COLD */
			if (b_ptr->immune_cold) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			break;

			case 96+12:    /* RF4_BR_POIS */
			z = (hp / 3);
			if (z > 800) z = 800;
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->oppose_pois) z = (z + 2) / 3;
			break;

			case 96+13:    /* RF4_BR_NETH */
			z = (hp / 6);
			if (z > 550) z = 550;
			if (b_ptr->resist_nethr) z = z * 6 / 7;
			break;

			case 96+14:    /* RF4_BR_LITE */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_lite) z = z * 6 / 7;
			break;

			case 96+15:    /* RF4_BR_DARK */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_dark) z = z * 6 / 7;
			break;

			case 96+16:    /* RF4_BR_CONF */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_confu) z = z * 6 / 7;
			break;

			case 96+17:    /* RF4_BR_SOUN */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_sound) z = z * 6 / 7;
			break;

			case 96+18:    /* RF4_BR_CHAO */
			z = (hp / 6);
			if (z > 600) z = 600;
			if (b_ptr->resist_chaos) z = z * 6 / 7;
			break;

			case 96+19:    /* RF4_BR_DISE */
			z = (hp / 6);
			if (z > 500) z = 500;
			if (b_ptr->resist_disen) z = z * 6 / 7;
			break;

			case 96+20:    /* RF4_BR_NEXU */
			z = (hp / 3);
			if (z > 250) z = 250;
			if (b_ptr->resist_nexus) z = z * 6 / 7;
			break;

			case 96+21:    /* RF4_BR_TIME */
			z = (hp / 3);
			if (z > 150) z = 150;
			break;

			case 96+22:    /* RF4_BR_INER */
			z = (hp / 6);
			if (z > 200) z = 200;
			break;

			case 96+23:    /* RF4_BR_GRAV */
			z = (hp / 3);
			if (z > 200) z = 200;
			break;

			case 96+24:    /* RF4_BR_SHAR */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_shard) z = z * 6 / 7;
			break;

			case 96+25:    /* RF4_BR_PLAS */
			z = (hp / 6);
			if (z > 150) z = 150;
			break;

			case 96+26:    /* RF4_BR_WALL */
			z = (hp / 6);
			if (z > 200) z = 200;
			break;

			case 96+27:    /* RF4_BR_MANA */
			/* XXX XXX XXX */
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
			if (b_ptr->immune_acid) break;
			z = (lev * 3) + 15;
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			break;

			case 128+1:    /* RF5_BA_ELEC */
			if (b_ptr->immune_elec) break;
			z = (lev * 3) / 2 + 8;
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			break;

			case 128+2:    /* RF5_BA_FIRE */
			if (b_ptr->immune_fire) break;
			z = (lev * 7) / 2 + 10;
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			break;

			case 128+3:    /* RF5_BA_COLD */
			if (b_ptr->immune_cold) break;
			z = (lev * 3) / 2 + 10;
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			break;

			case 128+4:    /* RF5_BA_POIS */
			z = (12 * 2);
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->oppose_pois) z = (z + 2) / 3;
			break;

			case 128+5:    /* RF5_BA_NETH */
			z = (50 + (10 * 10) + lev);
			if (b_ptr->resist_nethr) z = z * 6 / 7;
			break;

			case 128+6:    /* RF5_BA_WATE */
			z = ((lev * 5) / 2) + 50;
			break;

			case 128+7:    /* RF5_BA_MANA */
			z = ((lev * 5) + (10 * 10));
			break;

			case 128+8:    /* RF5_BA_DARK */
			z = ((lev * 5) + (10 * 10));
			if (b_ptr->resist_dark) z = z * 6 / 7;
			break;

			case 128+9:    /* RF5_DRAIN_MANA */
			break;

			case 128+10:    /* RF5_MIND_BLAST */
			if (b_ptr->skill_sav >= 100) break;
			z = 20;
			break;

			case 128+11:    /* RF5_BRAIN_SMASH */
			if (b_ptr->skill_sav >= 100) break;
			z = (12 * 15);
			break;

			case 128+12:    /* RF5_CAUSE_1 */
			if (b_ptr->skill_sav >= 100) break;
			z = (3 * 8);
			break;

			case 128+13:    /* RF5_CAUSE_2 */
			if (b_ptr->skill_sav >= 100) break;
			z = (8 * 8);
			break;

			case 128+14:    /* RF5_CAUSE_3 */
			if (b_ptr->skill_sav >= 100) break;
			z = (10 * 15);
			break;

			case 128+15:    /* RF5_CAUSE_4 */
			if (b_ptr->skill_sav >= 100) break;
			z = (15 * 15);
			break;

			case 128+16:    /* RF5_BO_ACID */
			if (b_ptr->immune_acid) break;
			z = ((7 * 8) + (lev / 3));
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			break;

			case 128+17:    /* RF5_BO_ELEC */
			if (b_ptr->immune_elec) break;
			z = ((4 * 8) + (lev / 3));
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			break;

			case 128+18:    /* RF5_BO_FIRE */
			if (b_ptr->immune_fire) break;
			z = ((9 * 8) + (lev / 3));
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			break;

			case 128+19:    /* RF5_BO_COLD */
			if (b_ptr->immune_cold) break;
			z = ((6 * 8) + (lev / 3));
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			break;

			case 128+20:    /* RF5_BO_POIS */
			/* XXX XXX XXX */
			break;

			case 128+21:    /* RF5_BO_NETH */
			z = (30 + (5 * 5) + (lev * 3) / 2);
			if (b_ptr->resist_nethr) z = z * 6 / 7;
			break;

			case 128+22:    /* RF5_BO_WATE */
			z = ((10 * 10) + (lev));
			break;

			case 128+23:    /* RF5_BO_MANA */
			z = ((lev * 7) / 2) + 50;
			break;

			case 128+24:    /* RF5_BO_PLAS */
			z = (10 + (8 * 7) + (lev));
			break;

			case 128+25:    /* RF5_BO_ICEE */
			z = ((6 * 6) + (lev));
			break;

			case 128+26:    /* RF5_MISSILE */
			z = ((2 * 6) + (lev / 3));
			break;

			case 128+27:    /* RF5_SCARE */
			break;

			case 128+28:    /* RF5_BLIND */
			break;

			case 128+29:    /* RF5_CONF */
			break;

			case 128+30:    /* RF5_SLOW */
			break;

			case 128+31:    /* RF5_HOLD */
			break;



			case 160+0:    /* RF6_HASTE */
			break;

			case 160+1:    /* RF6_XXX1X6 */
			break;

			case 160+2:    /* RF6_HEAL */
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
			break;

			case 160+9:    /* RF6_TELE_AWAY */
			break;

			case 160+10:    /* RF6_TELE_LEVEL */
			break;

			case 160+11:    /* RF6_XXX5 */
			break;

			case 160+12:    /* RF6_DARKNESS */
			break;

			case 160+13:    /* RF6_TRAPS */
			break;

			case 160+14:    /* RF6_FORGET */
			break;

			case 160+15:    /* RF6_XXX6X6 */
			break;

			case 160+16:    /* RF6_S_KIN */
			break;

			case 160+17:    /* RF6_S_HI_DEMON */
			break;

			case 160+18:    /* RF6_S_MONSTER */
			break;

			case 160+19:    /* RF6_S_MONSTERS */
			break;

			case 160+20:    /* RF6_S_ANT */
			break;

			case 160+21:    /* RF6_S_SPIDER */
			break;

			case 160+22:    /* RF6_S_HOUND */
			break;

			case 160+23:    /* RF6_S_HYDRA */
			break;

			case 160+24:    /* RF6_S_ANGEL */
			break;

			case 160+25:    /* RF6_S_DEMON */
			break;

			case 160+26:    /* RF6_S_UNDEAD */
			break;

			case 160+27:    /* RF6_S_DRAGON */
			break;

			case 160+28:    /* RF6_S_HI_UNDEAD */
			break;

			case 160+29:    /* RF6_S_HI_DRAGON */
			break;

			case 160+30:    /* RF6_S_WRAITH */
			break;

			case 160+31:    /* RF6_S_UNIQUE */
			break;
		}

		/* Hack -- Apply invulnerability */
		if (b_ptr->invuln) z = 0;

		/* Track most dangerous spell */
		if (z > n) n = z;
	}

	/* Danger */
	return (n);
}


/*
 * Calculate the maximum danger to a grid from a monster  XXX XXX XXX
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
int borg_max_danger_aux(int y, int x, int c, int i)
{
	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];

	int x9 = kill->x;
	int y9 = kill->y;

	int ax, ay, d;

	int q, r, p, v1, v2;


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


	/* Total energy */
	q = c * kill->moves;

	/* Minimal energy */
	if (q < 10) q = 10;


	/* Hack -- reproducers are dangerous, boost speed */
	if (r_ptr->flags2 & RF2_MULTIPLY) q = q * 2 + 40;


	/* No attacks for some monsters */
	if (r_ptr->flags1 & RF1_NEVER_BLOW)
	{
		v1 = 0;
	}

	/* No movement for some monsters */
	else if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (d > 1))
	{
		v1 = 0;
	}

	/* Hack -- Physical attacks require proximity */
	else if (q < (d * 10))
	{
		v1 = 0;
	}

	/* Danger from physical attacks */
	else
	{
		/* Physical attacks */
		v1 = borg_max_danger_aux1(i, y, x);

		/* Reduce danger from sleeping monsters */
		if ((!kill->awake) && (d > 1)) v1 = v1 / d;

		/* Danger */
		if (v1)
		{
			/* Attacks after movement */
			r = (q - ((d-1) * 10));

			/* Hack -- stumble sometimes XXX XXX XXX */
			/* if (r_ptr->flags1 & (RF1_RAND_25 | RF1_RAND_50)) r -= (r / 4); */

			/* Total danger */
			v1 = v1 * r / 10;
		}
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

	/* Hack -- verify line of sight */
	else if (c < 2 && !borg_projectable(y9, x9, y, x))
	{
		v2 = 0;
	}

	/* Danger from spell attacks */
	else
	{
		/* Spell attacks */
		v2 = borg_max_danger_aux2(i, y, x);

		/* Reduce danger from sleeping monsters */
		if ((!kill->awake) && (d > 1)) v2 = v2 / d;

		/* Danger */
		if (v2)
		{
			/* Full power */
			r = q;

			/* XXX XXX XXX */
			if (c > 1)
			{
				/* Hack -- low frequency spells */
				if (r_ptr->freq_spell < 25) r -= (r / 4);
			}

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
 * Hack -- Calculate the maximum "danger" of the given grid.
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
 * the nearby monsters can "squeeze" into a single grid.  We also
 * assume that monsters can walk through walls, which is rarely true.
 *
 * Note that we also take account of the danger of the "region" in
 * which the grid is located, which allows us to apply some "fear"
 * of invisible monsters and things of that nature.  XXX XXX
 *
 * Note that this function is used to calculate the "danger cache",
 * stored in "borg_cave_danger", but that cache is neither complete
 * nor up to date in many cases, and only caches "borg_danger(y,x,1).
 */
int borg_max_danger(int y, int x, int c)
{
	int i, p;


	/* Base danger (from fear) */
	p = borg_fear_region[y/11][x/11] * c;


	/* Examine all the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Collect danger from monster */
		p += borg_max_danger_aux(y, x, c, i);
	}


	/* Return the danger */
	return (p);
}


/*
 * Count the number of filled grids surrounding a grid.
 *
 * A grid is filled if it will block a monster from being summoned
 * in that grid.
 */
static int borg_filled_grids(int cy, int cx)
{
	int i, n = 0;

	/* Count glyphs */
	for (i = 0; i < 9; i++)
	{
		int y, x;

		/* Get coordinates */
		y = cy + ddy_ddd[i];
		x = cx + ddx_ddd[i];

		/* Skip out of bounds */
		if (!in_bounds(y, x)) continue;

		/* Check for a glyph of warding */
		if (borg_cave_feat[y][x] == FEAT_GLYPH) n++;

		/* Check for wall */
		if (!borg_cave_floor_bold(y, x)) n++;
	}

	/* Return count */
	return (n);
}

/*
 * Calculate estimated base danger from a monster's physical attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 */
static int borg_danger_aux1(int i, int y, int x)
{
	int k, n = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


	/* Mega-Hack -- unknown monsters */
	if (kill->r_idx >= z_info->r_max) return (1000);


	/* Analyze each physical attack */
	for (k = 0; k < 4; k++)
	{
		int z = 0;
		int a = 0;
		int power = 0;

		int method = r_ptr->blow[k].method;
		int effect = r_ptr->blow[k].effect;
		int d_dice = r_ptr->blow[k].d_dice;
		int d_side = r_ptr->blow[k].d_side;
		
		/* Done */
		if (!method) break;

		/* Analyze the attack */
		switch (effect)
		{
			case RBE_HURT:
			z = (d_dice * d_side);
			power = 60;
			z -= (z * ((b_ptr->ac < 150) ? b_ptr->ac : 150) / 250);
			a = 0;
			break;

			case RBE_POISON:
			z = (d_dice * d_side);
			power = 5;
			a = 10;
			if (b_ptr->resist_pois) 
			{
				z = (z + 2) / 3;
				a = 5;
			}
			break;

			case RBE_UN_BONUS:
			z = (d_dice * d_side);
			power = 10;
			if (b_ptr->resist_disen) break;
			a = 50;
			break;

			case RBE_UN_POWER:
			z = (d_dice * d_side);
			power = 15;
			a = 100;
			break;

			case RBE_EAT_GOLD:
			z = (d_dice * d_side);
			power = 5;
			if (100 <= adj_dex_safe[b_ptr->stat_ind[A_DEX]] + b_ptr->lev) break;
			if (b_ptr->au < 100) break;
			a = 10;
			break;

			case RBE_EAT_ITEM:
			z = (d_dice * d_side);
			power = 5;
			if (100 <= adj_dex_safe[b_ptr->stat_ind[A_DEX]] + b_ptr->lev) break;
			a = 20;
			break;

			case RBE_EAT_FOOD:
			z = (d_dice * d_side);
			power = 5;
			if (amt_food > 5) break;
			a = 5;
			break;

			case RBE_EAT_LITE:
			z = (d_dice * d_side);
			power = 5;
			if (amt_fuel > 5) break;
			a = 20;
			break;

			case RBE_ACID:
			power = 0;
			if (b_ptr->immune_acid) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			a = 40;
			break;

			case RBE_ELEC:
			power = 10;
			if (b_ptr->immune_elec) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			a = 20;
			break;

			case RBE_FIRE:
			power = 10;
			if (b_ptr->immune_fire) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			a = 40;
			break;

			case RBE_COLD:
			power = 10;
			if (b_ptr->immune_cold) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			a = 20;
			break;

			case RBE_BLIND:
			z = (d_dice * d_side);
			power = 2;
			if (b_ptr->resist_blind) break;
			a = 10;
			break;

			case RBE_CONFUSE:
			z = (d_dice * d_side);
			power = 10;
			if (b_ptr->resist_confu) break;
			a = 10;
			break;

			case RBE_TERRIFY:
			z = (d_dice * d_side);
			power = 10;
			if (b_ptr->resist_fear) break;
			if (b_ptr->skill_sav >= 100) break;
			a = 10;
			break;

			case RBE_PARALYZE:
			z = (d_dice * d_side);
			power = 2;
			if (b_ptr->free_act) break;
			if (b_ptr->skill_sav >= 100) break;
			a = 50;
			break;

			case RBE_LOSE_STR:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_str) break;
			if (borg_base_stat[A_STR] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_DEX:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_dex) break;
			if (borg_base_stat[A_DEX] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_CON:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_con) break;
			if (borg_base_stat[A_CON] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_INT:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_int) break;
			if (borg_base_stat[A_INT] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_WIS:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_wis) break;
			if (borg_base_stat[A_WIS] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_CHR:
			z = (d_dice * d_side);
			power = 0;
			if (b_ptr->sustain_chr) break;
			if (borg_base_stat[A_CHR] <= 3) break;
			a = 100;
			break;

			case RBE_LOSE_ALL:
			z = (d_dice * d_side);
			power = 2;
			a = 200;
			break;

			case RBE_SHATTER:
			z = (d_dice * d_side);
			z -= (z * ((b_ptr->ac < 150) ? b_ptr->ac : 150) / 250);
			power = 60;
			a = 100;
			break;

			case RBE_EXP_10:
			z = (d_dice * d_side);
			power = 5;
			if (b_ptr->hold_life) break;
			a = 100;
			break;

			case RBE_EXP_20:
			z = (d_dice * d_side);
			power = 5;
			if (b_ptr->hold_life) break;
			a = 150;
			break;

			case RBE_EXP_40:
			z = (d_dice * d_side);
			power = 5;
			if (b_ptr->hold_life) break;
			a = 200;
			break;

			case RBE_EXP_80:
			z = (d_dice * d_side);
			power = 5;
			if (b_ptr->hold_life) break;
			a = 250;
			break;
		}

		/* Hack -- Apply danger from "stunning" */
		switch (method)
		{
			case RBM_PUNCH:
			case RBM_KICK:
			case RBM_BUTT:
			case RBM_CRUSH:
			if (d_side == 1 && d_dice >= 15)
			{
				a += d_dice * 25;
			}
		}

		/* Apply armor class */
		if (b_ptr->ac > 0)
		{
			int chance = power + (r_ptr->level * 3);
			int num;

			/* Compute numerator */
			num = chance - (b_ptr->ac * 3 / 4);

			/* No chance yields 5% chance of hit */
			if (num <= 0)
			{
				/* Round up */
				z = (z + 19) / 20;
				a = (a + 19) / 20;
			}

			else
			{
				/* Reduce danger */
				z = z * (num + chance - 1) / chance;
				a = a * (num + chance - 1) / chance;
			}
		}

		/* Hack -- Apply invulnerability */
		if (b_ptr->invuln) z = 0;

		/* Add in damage */
		n += z;

		/* Add in annoynace */
		n += a;
	}

	/* Glyphs of warding help reduce the danger */
	if (borg_cave_feat[y][x] == FEAT_GLYPH)
	{
		/* Reduce danger based on monster level */
		n = n * r_ptr->level * 2 / BREAK_GLYPH;
	}

	/* Check for protection from evil */
	if ((b_ptr->protevil > 0) &&
	    (r_ptr->flags3 & (RF3_EVIL)) &&
	    (b_ptr->lev >= r_ptr->level))
	{
		/* Take much less damage, on average */
		n = n * (50 - b_ptr->lev) / 100;
	}

	/* Hack -- Low level mages are easily killed */
	/* So we double the danger DvE */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 15))
	{
		n *= 2;
	}

	/* Return danger */
	return (n);
}


/*
 * Calculate estimated base danger from a monster's spell attacks
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 *
 * We reduce the danger from distant "sleeping" monsters.
 */
static int borg_danger_aux2(int i, int y, int x)
{
	int q, k, n = 0;

	int lev, hp;

	byte spell[96], num = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


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
	hp = kill->curhp;


	/* Analyze the spells */
	for (q = 0; q < num; q++)
	{
		int p = 0;

		int z = 0;

		/* Cast the spell. */
		switch (spell[q])
		{
			case 96+0:    /* RF4_SHRIEK */
			/* p += 1; */
			break;

			case 96+1:    /* RF4_XXX2X4 */
			break;

			case 96+2:    /* RF4_XXX3X4 */
			break;

			case 96+3:    /* RF4_XXX4X4 */
			break;

			case 96+4:    /* RF4_ARROW_1 */
			z = (1 * 6);
			break;

			case 96+5:    /* RF4_ARROW_2 */
			z = (3 * 6);
			break;

			case 96+6:    /* RF4_ARROW_3 */
			z = (5 * 6);
			break;

			case 96+7:    /* RF4_ARROW_4 */
			z = (7 * 6);
			break;

			case 96+8:    /* RF4_BR_ACID */
			if (b_ptr->immune_acid) break;
			z = (hp / 3);
			p += 40;
			if (z > 1600) z = 1600;
			if (b_ptr->resist_acid) 
			{
				z = (z + 2) / 3;
				p -= 20;
			}
			if (b_ptr->oppose_acid) 
			{
				z = (z + 2) / 3;
				p -= 30;
			}
			break;

			case 96+9:    /* RF4_BR_ELEC */
			if (b_ptr->immune_elec) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 96+10:    /* RF4_BR_FIRE */
			if (b_ptr->immune_fire) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 96+11:    /* RF4_BR_COLD */
			if (b_ptr->immune_cold) break;
			z = (hp / 3);
			if (z > 1600) z = 1600;
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 96+12:    /* RF4_BR_POIS */
			z = (hp / 3);
			if (z > 800) z = 800;
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->oppose_pois) z = (z + 2) / 3;
			if (b_ptr->resist_pois) break;
			if (b_ptr->oppose_pois) break;
			p += 20;
			break;

			case 96+13:    /* RF4_BR_NETH */
			z = (hp / 6);
			if (z > 550) z = 550;
			if (b_ptr->resist_nethr) z = z * 12 / 19;
			if (b_ptr->resist_nethr) break;
			p += 50;
			if (b_ptr->hold_life) break;
			p += 150;
			break;

			case 96+14:    /* RF4_BR_LITE */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_lite) z = z * 12 / 19;
			if (b_ptr->resist_lite) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 96+15:    /* RF4_BR_DARK */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_dark) z = z * 12 / 19;
			if (b_ptr->resist_dark) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 96+16:    /* RF4_BR_CONF */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_confu) z = z * 12 / 19;
			if (b_ptr->resist_confu) break;
			p += 100;
			break;

			case 96+17:    /* RF4_BR_SOUN */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_sound) z = z * 12 / 19;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+18:    /* RF4_BR_CHAO */
			z = (hp / 6);
			if (z > 600) z = 600;
			if (b_ptr->resist_chaos) z = z * 12 / 19;
			if (b_ptr->resist_chaos) break;
			p += 200;
			if (b_ptr->resist_nethr) break;
			if (b_ptr->hold_life) break;
			p += 100;
			if (b_ptr->resist_confu) break;
			p += 50;
			break;

			case 96+19:    /* RF4_BR_DISE */
			z = (hp / 6);
			if (z > 500) z = 500;
			if (b_ptr->resist_disen) z = z * 12 / 19;
			if (b_ptr->resist_disen) break;
			p += 500;
			break;

			case 96+20:    /* RF4_BR_NEXU */
			z = (hp / 3);
			if (z > 250) z = 250;
			if (b_ptr->resist_nexus) z = z * 12 / 19;
			if (b_ptr->resist_nexus) break;
			p += 100;
			break;

			case 96+21:    /* RF4_BR_TIME */
			z = (hp / 3);
			if (z > 150) z = 150;
			p += 200;
			break;

			case 96+22:    /* RF4_BR_INER */
			z = (hp / 6);
			if (z > 200) z = 200;
			p += 50;
			break;

			case 96+23:    /* RF4_BR_GRAV */
			z = (hp / 3);
			if (z > 200) z = 200;
			p += 50;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+24:    /* RF4_BR_SHAR */
			z = (hp / 6);
			if (z > 400) z = 400;
			if (b_ptr->resist_shard) z = z * 12 / 19;
			if (b_ptr->resist_shard) break;
			p += 50;
			break;

			case 96+25:    /* RF4_BR_PLAS */
			z = (hp / 6);
			if (z > 150) z = 150;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+26:    /* RF4_BR_WALL */
			z = (hp / 6);
			if (z > 200) z = 200;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+27:    /* RF4_BR_MANA */
			/* XXX XXX XXX */
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
			if (b_ptr->immune_acid) break;
			z = (lev * 3) + 15;
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+1:    /* RF5_BA_ELEC */
			if (b_ptr->immune_elec) break;
			z = (lev * 3) / 2 + 8;
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+2:    /* RF5_BA_FIRE */
			if (b_ptr->immune_fire) break;
			z = (lev * 7) / 2 + 10;
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+3:    /* RF5_BA_COLD */
			if (b_ptr->immune_cold) break;
			z = (lev * 3) / 2 + 10;
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+4:    /* RF5_BA_POIS */
			z = (12 * 2);
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->oppose_pois) z = (z + 2) / 3;
			if (b_ptr->resist_pois) break;
			if (b_ptr->oppose_pois) break;
			p += 20;
			break;

			case 128+5:    /* RF5_BA_NETH */
			z = (50 + (10 * 10) + lev);
			if (b_ptr->resist_nethr) z = z * 12 / 19;
			if (b_ptr->resist_nethr) break;
			p += 200;
			break;

			case 128+6:    /* RF5_BA_WATE */
			z = ((lev * 5) / 2) + 50;
			p += 50;
			break;

			case 128+7:    /* RF5_BA_MANA */
			z = ((lev * 5) + (10 * 10));
			break;

			case 128+8:    /* RF5_BA_DARK */
			z = ((lev * 5) + (10 * 10));
			if (b_ptr->resist_dark) z = z * 12 / 19;
			if (b_ptr->resist_dark) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 128+9:    /* RF5_DRAIN_MANA */
			/* if (b_ptr->msp) p += 10; */
			break;

			case 128+10:    /* RF5_MIND_BLAST */
			if (b_ptr->skill_sav >= 100) break;
			z = 20;
			break;

			case 128+11:    /* RF5_BRAIN_SMASH */
			if (b_ptr->skill_sav >= 100) break;
			z = (12 * 15);
			p += 100;
			break;

			case 128+12:    /* RF5_CAUSE_1 */
			if (b_ptr->skill_sav >= 100) break;
			z = (3 * 8);
			break;

			case 128+13:    /* RF5_CAUSE_2 */
			if (b_ptr->skill_sav >= 100) break;
			z = (8 * 8);
			break;

			case 128+14:    /* RF5_CAUSE_3 */
			if (b_ptr->skill_sav >= 100) break;
			z = (10 * 15);
			break;

			case 128+15:    /* RF5_CAUSE_4 */
			if (b_ptr->skill_sav >= 100) break;
			z = (15 * 15);
			p += 50;
			break;

			case 128+16:    /* RF5_BO_ACID */
			if (b_ptr->immune_acid) break;
			z = ((7 * 8) + (lev / 3));
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			if (b_ptr->oppose_acid) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+17:    /* RF5_BO_ELEC */
			if (b_ptr->immune_elec) break;
			z = ((4 * 8) + (lev / 3));
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			if (b_ptr->oppose_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+18:    /* RF5_BO_FIRE */
			if (b_ptr->immune_fire) break;
			z = ((9 * 8) + (lev / 3));
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			if (b_ptr->oppose_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+19:    /* RF5_BO_COLD */
			if (b_ptr->immune_cold) break;
			z = ((6 * 8) + (lev / 3));
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			if (b_ptr->oppose_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+20:    /* RF5_BO_POIS */
			/* XXX XXX XXX */
			break;

			case 128+21:    /* RF5_BO_NETH */
			z = (30 + (5 * 5) + (lev * 3) / 2);
			if (b_ptr->resist_nethr) z = z * 12 / 19;
			if (b_ptr->resist_nethr) break;
			p += 200;
			break;

			case 128+22:    /* RF5_BO_WATE */
			z = ((10 * 10) + (lev));
			p += 20;
			break;

			case 128+23:    /* RF5_BO_MANA */
			z = ((lev * 7) / 2) + 50;
			break;

			case 128+24:    /* RF5_BO_PLAS */
			z = (10 + (8 * 7) + (lev));
			p += 20;
			break;

			case 128+25:    /* RF5_BO_ICEE */
			z = ((6 * 6) + (lev));
			p += 20;
			break;

			case 128+26:    /* RF5_MISSILE */
			z = ((2 * 6) + (lev / 3));
			break;

			case 128+27:    /* RF5_SCARE */
			if (b_ptr->resist_fear) break;
			if (b_ptr->skill_sav >= 100) break;
			p += 10;
			break;

			case 128+28:    /* RF5_BLIND */
			if (b_ptr->resist_blind) break;
			if (b_ptr->skill_sav >= 100) break;
			p += 10;
			break;

			case 128+29:    /* RF5_CONF */
			if (b_ptr->resist_confu) break;
			if (b_ptr->skill_sav >= 100) break;
			p += 10;
			break;

			case 128+30:    /* RF5_SLOW */
			if (b_ptr->free_act) break;
			if (b_ptr->skill_sav >= 100) break;
			p += 5;
			break;

			case 128+31:    /* RF5_HOLD */
			if (b_ptr->free_act) break;
			if (b_ptr->skill_sav >= 100) break;
			p += 20;
			break;



			case 160+0:    /* RF6_HASTE */
			p += 10;
			break;

			case 160+1:    /* RF6_XXX1X6 */
			break;

			case 160+2:    /* RF6_HEAL */
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
			p += 20;
			break;

			case 160+9:    /* RF6_TELE_AWAY */
			p += 10;
			break;

			case 160+10:    /* RF6_TELE_LEVEL */
			if (b_ptr->skill_sav >= 100) break;
			p += 50;
			break;

			case 160+11:    /* RF6_XXX5 */
			break;

			case 160+12:    /* RF6_DARKNESS */
			p += 5;
			break;

			case 160+13:    /* RF6_TRAPS */
			p += 50;
			break;

			case 160+14:    /* RF6_FORGET */
			if (b_ptr->skill_sav >= 100) break;
			p += 50;
			break;

			case 160+15:    /* RF6_XXX6X6 */
			break;

			case 160+16:    /* RF6_S_KIN */
			if (borg_create_door)
				p += 0;
			else
				p += (lev) * 30 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+17:    /* RF6_S_HI_DEMON */
			if (borg_create_door)
				p += (lev) * 10 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 50 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+18:    /* RF6_S_MONSTER */
			if (borg_create_door)
				p +=0;
			else
				p += (lev) * 10 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+19:    /* RF6_S_MONSTERS */
			if (borg_create_door)
				p +=0;
			else
				p += (lev) * 20 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+20:    /* RF6_S_ANT */
			if (borg_create_door)
				p +=0;
			else
				p += (lev) * 20 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+21:    /* RF6_S_SPIDER */
			if (borg_create_door)
				p +=0;
			else
				p += (lev) * 20 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+22:    /* RF6_S_HOUND */
			if (borg_create_door)
				p +=0;
			else
				p += (lev) * 20 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+23:    /* RF6_S_HYDRA */
			if (borg_create_door)
				p += (lev) * 4 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 20 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+24:    /* RF6_S_ANGEL */
			if (borg_create_door)
				p += (lev) * 6 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 30 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+25:    /* RF6_S_DEMON */
			if (borg_create_door)
				p += (lev) * 6 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 30 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+26:    /* RF6_S_UNDEAD */
			if (borg_create_door)
				p += (lev) * 6 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 30 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+27:    /* RF6_S_DRAGON */
			if (borg_create_door)
				p += (lev) * 6 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 30 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+28:    /* RF6_S_HI_UNDEAD */
			if (borg_create_door)
				p += (lev) * 10 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 50 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+29:    /* RF6_S_HI_DRAGON */
			if (borg_create_door)
				p += (lev) * 10 / (borg_filled_grids(y, x) +1);
			else
				p += (lev) * 50 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+30:    /* RF6_S_WRAITH */
			if (borg_create_door)
				p += (lev) * 10 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 50 / (borg_filled_grids(y, x) + 1);
			break;

			case 160+31:    /* RF6_S_UNIQUE */
			if (borg_create_door)
				p += (lev) * 10 / (borg_filled_grids(y, x) + 1);
			else
				p += (lev) * 50 / (borg_filled_grids(y, x) + 1);
			break;
		}

		/* Notice damage */
		p += z;

		/* Track most dangerous spell */
		if (p > n) n = p;
	}

	/* Danger */
	return (n);
}


/*
 * Calculate the estimated danger to a grid from a monster  XXX XXX XXX
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
	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];

	int x9 = kill->x;
	int y9 = kill->y;

	int ax, ay, d;

	int q, r, p, v1, v2;


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


	/* Estimated total energy */
	/* Take slow spell into account */
	if (borg_slow_spell) q = c * (kill->moves - 1);
	else q = c * kill->moves;

	/* Minimal energy */
	if (q < 10) q = 10;


	/* Hack -- reproducers are dangerous, boost speed */
	if (r_ptr->flags2 & RF2_MULTIPLY) q = q * 2 + 40;


	/* No attacks for some monsters */
	if (r_ptr->flags1 & RF1_NEVER_BLOW)
	{
		v1 = 0;
	}

	/* No movement for some monsters */
	else if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (d > 1))
	{
		v1 = 0;
	}

	/* Hack -- Physical attacks require proximity */
	else if (q < (d * 10))
	{
		v1 = 0;
	}

	/* Danger from physical attacks */
	else
	{
		/* Physical attacks */
		v1 = borg_danger_aux1(i, y, x);

		/* Reduce danger from sleeping monsters */
		// if ((!kill->awake) && (d > 1)) v1 = v1 / d;

		/* Danger */
		if (v1)
		{
			/* Attacks after movement */
			r = (q - ((d-1) * 10));

			/* Hack -- stumble sometimes XXX XXX XXX */
			/* if (r_ptr->flags1 & (RF1_RAND_25 | RF1_RAND_50)) r -= (r / 4); */

			/* Total danger */
			v1 = v1 * r / 10;
		}

		/* Reduce danger from sleeping monsters with the sleep 1,3 spell */
        if (borg_sleep_spell)
        {
            v1 = v1 / (d+2);
        }

		/* Reduce danger from sleeping monsters with the sleep 2 spell */ 
		if (borg_sleep_spell_ii)
        {
            if  ( (d == 1) &&
                  (kill->awake) &&
                  (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
				  (!(r_ptr->flags1 & RF1_UNIQUE)) &&
                  (kill->level <= b_ptr->lev) )
            {
                  v1 = v1 / (d+2);
            }
        }

		/* Reduce danger from confusing monster with spell */
		if (borg_confuse_spell)
        {
            v1 = v1 / 6;
        }
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

	/* Hack -- verify line of sight */
	else if (!borg_projectable(y9, x9, y, x))
	{
		v2 = 0;
	}

	/* Danger from spell attacks */
	else
	{
		/* Spell attacks */
		v2 = borg_danger_aux2(i, y, x);

		/* Reduce danger from sleeping monsters */
		if ((!kill->awake) && (d > 1)) v2 = v2 / d;

		/* Danger */
		if (v2)
		{
			/* Full power */
			r = q;

			/* XXX XXX XXX */
			if (c > 1)
			{
				/* Hack -- low frequency spells */
				r = r * r_ptr->freq_spell / 100;
			}

			/* Total danger */
			v2 = v2 * r / 10;
		}

		/* Reduce danger from sleeping monsters with the sleep 2 spell*/
        if (borg_sleep_spell_ii)
        {

            if  ( (d == 1) &&
                  (kill->awake) &&
                  (!(r_ptr->flags3 & RF3_NO_SLEEP)) &&
				  (!(r_ptr->flags1 & RF1_UNIQUE)) &&
                  (kill->level <= b_ptr->lev) )
            {
                  v2 = v2 / (d+2);
            }
        }

		/* Reduce danger from sleeping monsters with the sleep 1,3 spell*/
        if (borg_sleep_spell)
        {
            v2 = v2 / (d+2);
        }

		/* Reduce danger from confusing monster with spell */
		if (borg_confuse_spell)
        {
            v2 = v2 / 6;
        }
	}


	/* Maximal danger */
	p = MAX(v1, v2);

	/* Hack -- reproducers are dangerous for low level chars DvE*/
	if ((r_ptr->flags2 & RF2_MULTIPLY) && (b_ptr->lev < 10)) p *= 2;

	/* Result */
	return (p);
}


/*
 * Hack -- Calculate the estimated "danger" of the given grid.
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
 * the nearby monsters can "squeeze" into a single grid.  We also
 * assume that monsters can walk through walls, which is rarely true.
 *
 * Note that we also take account of the danger of the "region" in
 * which the grid is located, which allows us to apply some "fear"
 * of invisible monsters and things of that nature.  XXX XXX
 *
 * Note that this function is used to calculate the "danger cache",
 * stored in "borg_cave_danger", but that cache is neither complete
 * nor up to date in many cases, and only caches "borg_danger(y,x,1).
 */
int borg_danger(int y, int x, int c)
{
	int i, p;


	/* Base danger (from fear) */
	p = borg_fear_region[y/11][x/11] * c;


	/* Examine all the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Collect danger from monster */
		p += borg_danger_aux(y, x, c, i);
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
bool borg_restock(void)
{
	/* Always ready for the town */
	if (!b_ptr->depth) return (FALSE);

	/* Always spend time on a level */
	if (borg_time - borg_when_began < 100) return (FALSE);


	/*** Level 1 ***/

	/* Must have some lite */
	if (b_ptr->cur_lite < 1)
	{
		borg_note("# Restock lite");
		return (TRUE);
	}

	/* Must have "fuel" */
	if (amt_fuel < 1)
	{
		borg_note("# Restock fuel");
		return (TRUE);
	}

	/* Must have "food" */
	if (amt_food < 1)
	{
		borg_note(format("# Restock 1 food, carrying %d",amt_food));
		return (TRUE);
	}

	/* Assume happy at level 1 */
	if (b_ptr->depth <= 1) return (FALSE);


	/*** Level 2 to 9 ***/

	/* Must have good lite */
	if (b_ptr->cur_lite < 2)
	{
		borg_note("# Restock good lite");
		return (TRUE);
	}

	/* Must have "fuel" */
	if (amt_fuel < 3)
	{
		borg_note("# Restock fuel");
		return (TRUE);
	}

	/* Must have "food" */
	if (amt_food < 3)
	{
		borg_note("# Restock food");
		return (TRUE);
	}

	/* Must have "recall" */
	if (amt_recall < 2)
	{
		borg_note("# Restock recall");
		return (TRUE);
	}

	/* Assume happy at depth 9 */
	if (b_ptr->depth <= 9) return (FALSE);

	/*** Level 10 and below ***/

	/* Must have teleport */
	if (amt_teleport + amt_escape < 2) 
	{
		borg_note("# Restock teleport");
		return (TRUE);
	}

	/* Assume happy */
	return (FALSE);
}


/*
 * De termine if the Borg meets the "minimum" requirements for a level
 */
static bool borg_prepared_aux(int depth)
{
	borg_reason = "";
	if (borg_ready_morgoth == -1)
        borg_ready_morgoth = 0;
    
	if (borg_king)
    {
		borg_ready_morgoth = 2;
		return (TRUE);
    }

	/* Always ready for the town */
	if (!depth) return (TRUE);

	/*** Essential Items for Level 1 ***/

	/* Require lite (any) */
	if (b_ptr->cur_lite < 1) 
	{
		borg_reason = "need lite";
		return (FALSE);
	}

	/* Require food */
	if (amt_food < 5) 
	{
		borg_reason = "need 5 food";
		return (FALSE);
	}

	/* Usually ready for level 1 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 1) return (TRUE);


	/*** Essential Items for Level 2 ***/

	/* Require lite (radius two) */
	if (b_ptr->cur_lite < 2)
	{
		borg_reason = "Need Lite (radius 2)";
		return (FALSE);
	}

	/* Require fuel */
	if (amt_fuel < 5)
	{
		borg_reason = "Need 5 flasks of oil";
		return (FALSE);
	}

	/* Require recall */
	if (amt_recall < 1)
	{
		borg_reason = "Need Word of Recall";
		return (FALSE);
	}

	/* Scrolls of Identify (for identification) */
	if (amt_ident < 2 && (b_ptr->depth)) 
	{
		borg_reason = "Need 2 identify";
		return (FALSE);
	}

	/* mages require phase door scrolls */
	if ((b_ptr->pclass == CLASS_MAGE) && (amt_phase < 5))
	{
		borg_reason = "Need 5 scrolls of phase door";
		return (FALSE);
	}

	/* Usually ready for level 2 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 2) return (TRUE);


	/*** Essential Items for Level 3 and 4 ***/

	/* Scrolls of Word of Recall */
	if (amt_recall < 3) 
	{
		borg_reason = "Need 3 Word of Recall";
		return (FALSE);
	}

	/* Scrolls of Identify */
	if (amt_ident < 10)
	{
		borg_reason = "Need 10 identify";
		return (FALSE);
	}

	/* Potions of Cure Serious Wounds */
	if ((b_ptr->lev < 35) && (amt_cure_serious + amt_cure_critical < 2)) 
	{
		borg_reason = "Need 2 CCW or CSW";
		return (FALSE);
	}

	/* Scrolls of Identify */
    if (amt_ident < 5 && (b_ptr->depth))
	{
		borg_reason = "Need 5 identify";
		return (FALSE);
	}

	/* Usually ready for level 3 and 4 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 4) return (TRUE);


	/*** Essential Items for Level 5 to 9 ***/

	if (b_ptr->mhp < 50)
	{
		borg_reason = "Need 50 HP";
		return (FALSE);
	}

	/* class specific requirement */
    if (b_ptr->depth)
    {

        switch (b_ptr->pclass)
        {
            case CLASS_WARRIOR:
                if (b_ptr->lev < 4)
				{
					borg_reason = "Need to be level 4";
					return (FALSE);
				}
                break;
            case CLASS_ROGUE:
                if (b_ptr->lev < 8)
				{
					borg_reason = "Need to be level 8";
					return (FALSE);
				}
                break;
            case CLASS_PRIEST:
                if (b_ptr->lev < 13) 
				{
					borg_reason = "Need to be level 13";
					return (FALSE);
				}
                break;
            case CLASS_PALADIN:
                if (b_ptr->lev < 4)
				{
					borg_reason = "Need to be level 4";
					return (FALSE);
				}
                break;
            case CLASS_RANGER:
                if (b_ptr->lev < 4) 
				{
					borg_reason = "Need to be level 4";
					return (FALSE);
				}
                break;
            case CLASS_MAGE:
                if (b_ptr->lev < 15)
				{
					borg_reason = "Need to be level 15";
					return (FALSE);
				}
                break;
        }
    }


	/* Scrolls of Word of Recall */
	if (amt_recall < 4)
	{
		borg_reason = "Need 4 Word of Recall";
		return (FALSE);
	}

	/* Scrolls of Identify */
	if (amt_ident < 15 && (b_ptr->depth)) 
	{
		borg_reason = "Need 15 identify";
		return (FALSE);
	}


	/* Potions of Cure Serious/Critical Wounds */
	if ((b_ptr->lev < 35) && amt_cure_serious + amt_cure_critical < 5)
	{
		borg_reason = "Need 5 CSW or CCW";
		return (FALSE);
	}
	
	/* Usually ready for level 5 to 9 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 9) return (TRUE);


	/*** Essential Items for Level 10 to 19 ***/

	/* Escape or Teleport */
	if (amt_teleport + amt_escape < 2) 
	{
		borg_reason = "Need 2 teleport";
		return (FALSE);
	}

	/* clevel minimal equal to dlevel */
	if (b_ptr->lev < depth && depth <= 19)
	{
		borg_reason = "Need to be level 19";
		return (FALSE);
	}
    

	/* Identify */
	if (amt_ident < 20)
	{
		borg_reason = "Need 20 identify";
		return (FALSE);
	}

	/* Potions of Cure Critical Wounds */
	if ((b_ptr->lev < 35) && amt_cure_critical < 5)
	{
		borg_reason = "Need 5 CCW";
		return (FALSE);
	}

	/* See invisible */
	if (!b_ptr->see_inv && !b_ptr->telepathy) 
	{
		borg_reason = "Need see invisible or telepathy";
		return (FALSE);
	}

	/* Usually ready for level 10 to 19 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 19) return (TRUE);

	/*** Essential Items for Level 20 ***/

	/* Free action */
	if (!b_ptr->free_act)
	{
		borg_reason = "Need free action";
		return (FALSE);
	}

	/* Basic resistance XXX XXX XXX */
	if (!b_ptr->resist_acid)
	{
		borg_reason = "Need resist acid";
		return (FALSE);
	}
	if (!b_ptr->resist_fire) 
	{
		borg_reason = "Need resist fire";
		return (FALSE);
	}

	/* Usually ready for level 20 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 20) return (TRUE);

	/*** Essential Items for Level 25 ***/

	/* have some minimal stats */
#if 0
    if (borg_base_stat[A_STR] < 14)
	{
		borg_reason = "Need STR of 14";
		return (FALSE);
	}

    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (borg_base_stat[A_INT] < 14)
		{
			borg_reason = "Need INT of 14";
			return (FALSE);
		}
    }
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (borg_base_stat[A_WIS] < 14)
		{
			borg_reason = "Need WIS of 14";
			return (FALSE);
		}
    }
    if (borg_base_stat[A_DEX] < 14)
	{
		borg_reason = "Need DEX of 14";
		return (FALSE);
	}

    if (borg_base_stat[A_CON] < 14)
	{
		borg_reason = "Need CON of 14";
		return (FALSE);
	}
#endif

	/* Mages need to be level 30 by now */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 30))
	{
		borg_reason = "Need to be level 30";
		return(FALSE);
	}

    /* Ready for level 25 */
	borg_reason = format("Ready for level %d",depth);
    if (depth <= 25) return (TRUE);

	/*** Essential Items for Level 26 to 39 ***/

    /* Escape and Teleport */
    if (amt_teleport < 2) 
	{
		borg_reason = "Need 2 teleport (staff)";
		return (FALSE);
	}
    if (amt_teleport + amt_escape < 6) 
	{
		borg_reason = "Need 6 teleport (total)";
		return (FALSE);
	}

    /* Cure Critical Wounds */
    if ((b_ptr->lev < 35) && amt_cure_critical < 10) 
	{
		borg_reason = "Need 10 CCW";
		return (FALSE);
	}

    /* Usually ready for level 26 to 39 */
	borg_reason = format("Ready for level %d",depth);
    if (depth <= 39) return (TRUE);

    /*** Essential Items for Level 40 to 45 ***/

	/* Minimal level */
    if (b_ptr->lev < 40) 
	{
		borg_reason = "Need to be level 40";
		return (FALSE);
	}

	/* All Basic resistance & poison*/
    if (!b_ptr->resist_cold)
	{
		borg_reason = "Need resist cold";
		return (FALSE);
	}
    if (!b_ptr->resist_elec)
	{
		borg_reason = "Need resist elec";
		return (FALSE);
	}
    if (!b_ptr->resist_pois)
	{
		borg_reason = "Need resist poison";
		return (FALSE);
	}

#if 0
	/* High stats XXX XXX XXX */
	if (borg_base_stat[A_STR] < 16)
	{
		borg_reason = "Need STR of 16";
		return (FALSE);
	}
	if (mb_ptr->spell_book == TV_MAGIC_BOOK)
	{
		if (borg_base_stat[A_INT] < 18+50)
		{
			borg_reason = "Need INT of 18/50";
			return (FALSE);
		}
	}
	if (mb_ptr->spell_book == TV_PRAYER_BOOK)
	{
		if (borg_base_stat[A_WIS] < 18+50)
		{
			borg_reason = "Need WIS of 18/50";
			return (FALSE);
		}
	}
	if (borg_base_stat[A_DEX] < 16)
	{
		borg_reason = "Need DEX of 16";
		return (FALSE);
	}
	if (borg_base_stat[A_CON] < 16)
	{
		borg_reason = "Need CON of 16";
		return (FALSE);
	}
#endif

	/* Usually ready for level 40 to 45 */
	borg_reason = format("Ready for level %d",depth);
	if (depth <= 45) return (TRUE);

	/*** Essential Items for Level 46 to 55 ***/

	/* Mages need to be level 50 by now */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 50))
	{
		borg_reason = "Need to be level 50";
		return (FALSE);
	}

    // Must have +5 speed after level 46
    if (b_ptr->pspeed < 115) 
	{
		borg_reason = "Need +5 Speed";
		return (FALSE);
	}

    /* Hold Life */
    /* after you max out you are pretty safe from drainers.*/
       if ((b_ptr->lev == 50) && (depth <= 60)) return (TRUE);
       else
        if ( (!b_ptr->hold_life && !borg_weapon_swap_hold_life &&
        !borg_armour_swap_hold_life) && (b_ptr->lev < 50) ) 
		{
			borg_reason = "Need hold life";
			return (FALSE);
		}

    /* Minimal level */
    if (b_ptr->lev < 45) 
	{
		borg_reason = "Need to be level 45";
		return (FALSE);
	}

    /* Minimal hitpoints */
    if (b_ptr->mhp < 500)
	{
		borg_reason = "Need 500 HP";
		return (FALSE);
	}

#if 0
    /* High stats XXX XXX XXX */
    if (borg_base_stat[A_STR] < 18+40)
	{
		borg_reason = "Need STR of 18/40";
		return (FALSE);
	}

    if (mb_ptr->spell_book == TV_MAGIC_BOOK)
    {
        if (borg_base_stat[A_INT] < 18+100) 
		{
			borg_reason = "Need INT of 18/100";
			return (FALSE);
		}
    }
    if (mb_ptr->spell_book == TV_PRAYER_BOOK)
    {
        if (borg_base_stat[A_WIS] < 18+100)
		{
			borg_reason = "Need WIS 18/100";
			return (FALSE);
		}
    }
    if (borg_base_stat[A_DEX] < 18+60)
	{
		borg_reason = "Need DEX of 18/60";
		return (FALSE);
	}

    if (borg_base_stat[A_CON] < 18+60) 
	{
		borg_reason = "Need CON of 18/60";
		return (FALSE);
	}
#endif

    //   Usually ready for level 46 to 55
	borg_reason = format("Ready for level %d",depth);
    if (depth <= 55) return (TRUE);


	/*** Essential Items for Level 55 to 59 ***/

    /* Resists */
    if (!b_ptr->resist_blind && !borg_weapon_swap_resist_blind &&
        !borg_armour_swap_resist_blind)
	{
		borg_reason = "Need resist blind";
		return (FALSE);
	}

    if (!b_ptr->resist_confu && !borg_weapon_swap_resist_conf &&
        !borg_armour_swap_resist_conf)  
	{
		borg_reason = "Need resist confusion";
		return (FALSE);
	}

    //   Usually ready for level 55 to 59
	borg_reason = "Ready for level 60";
    if (depth <= 59) return (TRUE);

	/*** Essential Items for Level 61 to 80 ***/

    // Must have +10 speed
    if (b_ptr->pspeed < 120)
	{
		borg_reason = "Need +10 speed";
		return (FALSE);
	}

    // Must have resist nether
    if (!b_ptr->resist_nethr && !borg_weapon_swap_resist_neth &&
        !borg_armour_swap_resist_neth)
	{
		borg_reason = "Need resist nether";
		return (FALSE);
	}

    /* Resists */
    if (!b_ptr->resist_chaos && !borg_weapon_swap_resist_chaos &&
        !borg_armour_swap_resist_chaos)
	{
		borg_reason = "Need resist chaos";
		return (FALSE);
	}

    if (!b_ptr->resist_disen && !borg_weapon_swap_resist_disen &&
        !borg_armour_swap_resist_disen)
	{
		borg_reason = "Need resist disenchantment";
		return (FALSE);
	}

    //   Usually ready for level 61 to 80
	borg_reason = format("Ready for level %d",depth);
    if (depth <= 80) return (TRUE);

	/*** Essential Items for Level 81-99 ***/

    /* Telepathy, better have it by now */
    if (!b_ptr->telepathy)
	{
		borg_reason = "Need telepathy";
		return (FALSE);
	}

    /* Minimal Speed */
    if (b_ptr->pspeed < 130) 
	{
		borg_reason = "Need +20 speed";
		return (FALSE);
	}

    /* Usually ready for level 81 to 99 */
	borg_reason = format("Ready for level %d",depth);
    if (depth <= 85) return (FALSE);

	/*** Essential Items for Level 100 ***/

    /* must have lots of restore mana to go after MORGOTH */
    if (!borg_king)
    {
        if ((b_ptr->msp > 100) && (amt_mana < 15)) 
		{
			borg_reason = "Need 15 restore mana";
			return (FALSE);
		}

        /* must have lots of heal */
        if (amt_pot_heal + amt_pot_star_heal + amt_pot_life < 20)
		{
			borg_reason = "Need 20 Healing, *Healing* or Life";
			return (FALSE);
		}

        /* Ready for morgoth! */
        borg_ready_morgoth = 1;
		borg_reason = "Ready for Morgoth";
    }

    /* Its good to be to the king */
    if (depth <= 127) return (TRUE);

    /* all bases covered */
    return (TRUE);
}



/*
 * Determine if the Borg is "prepared" for the given depth
 *
 * The Borg is always ready for the town and the first depth.
 *
 * The Borg must have proper equipment (etc) for deeper levels.
 *
 * The Borg has a favorite depth, and is never prepared for any
 * depth below this depth until he has completed his favorite
 * depth at least NNN times.
 *
 * This routine does not help him decide how to get ready for the
 * given level, so it must work closely with "borg_power()".
 */
bool borg_prepared(int depth)
{
	int live_unique, i;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	/* Town and First level */
	if (depth <= 1) return (TRUE);

	/* Must meet minimal requirements */
	if (!borg_prepared_aux(depth)) return (FALSE);

	/* check to make sure the borg does not go below where 2 living */
	/* uniques are. Copied from APW*/

	live_unique = 0;
	for (i = 1; i < z_info->r_max - 1; i++)
	{
		/* If any have been killed it is not a live unique */
		if (borg_race_death[i] != 0) continue;

		r_ptr = &r_info[i];
		l_ptr = &l_list[i];

		/* If any have been killed it is not a live unique */
		if (l_ptr->r_pkills != 0) continue;

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip non-uniques */
		if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

		/* Skip unseen uniques */
		if (!(l_ptr->r_sights)) continue;

		/* skip if deeper than fear level */
		if (r_ptr->level > depth) break;

		live_unique++;
		/* continue; */
	}

	if ((live_unique > 2) && (b_ptr->max_depth < 100))
	{
		/* To many uniques still alive */
		borg_reason = "To many uniques alive";
		return (FALSE);
	}

	/* check to make sure the borg does not go to level 100 */
	/* unless all the uniques are dead. */
	if ((live_unique > 1) && (b_ptr->max_depth >= 99))
    {
		borg_reason = "Uniques still alive";
		return (FALSE);
	
    }

	/* Favorite level */
	if (depth <= borg_happy_depth + 1) return (TRUE);

	/* Run away */
	return (FALSE);
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

