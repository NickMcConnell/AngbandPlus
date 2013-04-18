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

	int			stat_add[6];

	auto_item		*item;


	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) stat_add[i] = 0;


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


	/* Elf */
	if (b_ptr->prace == RACE_ELF) b_ptr->resist_lite = TRUE;

	/* Hobbit */
	if (b_ptr->prace == RACE_HOBBIT) b_ptr->sustain_dex = TRUE;

	/* Gnome */
	if (b_ptr->prace == RACE_GNOME) b_ptr->free_act = TRUE;

	/* Dwarf */
	if (b_ptr->prace == RACE_DWARF) b_ptr->resist_blind = TRUE;

	/* Half-Orc */
	if (b_ptr->prace == RACE_HALF_ORC) b_ptr->resist_dark = TRUE;

	/* Half-Troll */
	if (b_ptr->prace == RACE_HALF_TROLL) b_ptr->sustain_str = TRUE;

	/* Dunadan */
	if (b_ptr->prace == RACE_DUNADAN) b_ptr->sustain_con = TRUE;

	/* High Elf */
	if (b_ptr->prace == RACE_HIGH_ELF) b_ptr->resist_lite = TRUE;
	if (b_ptr->prace == RACE_HIGH_ELF) b_ptr->see_inv = TRUE;

	/* Warrior */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		if (b_ptr->lev >= 30) b_ptr->resist_fear = TRUE;
	}


	/*** Analyze equipment ***/

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Affect stats */
		if (item->flags1 & TR1_STR) stat_add[A_STR] += item->pval;
		if (item->flags1 & TR1_INT) stat_add[A_INT] += item->pval;
		if (item->flags1 & TR1_WIS) stat_add[A_WIS] += item->pval;
		if (item->flags1 & TR1_DEX) stat_add[A_DEX] += item->pval;
		if (item->flags1 & TR1_CON) stat_add[A_CON] += item->pval;
		if (item->flags1 & TR1_CHR) stat_add[A_CHR] += item->pval;

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
		if (item->flags2 & TR2_IM_FIRE) b_ptr->immune_fire = TRUE;
		if (item->flags2 & TR2_IM_ACID) b_ptr->immune_acid = TRUE;
		if (item->flags2 & TR2_IM_COLD) b_ptr->immune_cold = TRUE;
		if (item->flags2 & TR2_IM_ELEC) b_ptr->immune_elec = TRUE;

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

	/* Hack -- Resist Chaos yields Resist Confusion */
	if (b_ptr->resist_chaos) b_ptr->resist_confu = TRUE;


	/* Update "stats" */
	for (i = 0; i < 6; i++)
	{
		int use, ind;

		/* Extract the new "use_stat" value for the stat */
		use = modify_stat_value(b_ptr->stat_cur[i], stat_add[i]);

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

	/* Assume no enchantment needed */
	xb_ptr->need_enchant_to_a = 0;
	xb_ptr->need_enchant_to_h = 0;
	xb_ptr->need_enchant_to_d = 0;

	/* Hack -- enchant all the equipment (weapons) */
	for (i = INVEN_WIELD; i <= INVEN_BOW; i++)
	{
		item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip "unknown" items */
		if (!item->able) continue;

		/* Enchant all weapons (to hit) */
		if (item->to_h < 8)
		{
			xb_ptr->need_enchant_to_h += (8 - item->to_h);
		}

		/* Enchant all weapons (to damage) */
		if (item->to_d < 8)
		{
			xb_ptr->need_enchant_to_d += (8 - item->to_d);
		}
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
		if (item->to_a < 8)
		{
			xb_ptr->need_enchant_to_a += (8 - item->to_a);
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
	amt_recall = 0;
	amt_phase = 0;
	amt_escape = 0;
	amt_teleport = 0;

	/* Reset healing */
	amt_cure_critical = 0;
	amt_cure_serious = 0;

	/* Reset detection */
	amt_detect_trap = 0;
	amt_detect_door = 0;

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

	/* Reset enchantment */
	amt_enchant_to_a = 0;
	amt_enchant_to_d = 0;
	amt_enchant_to_h = 0;


	/*** Process the inventory ***/

	/* Scan the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];

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
			}

			break;


			/* Flasks */
			case TV_FLASK:

			/* Use as fuel (etc) */
			amt_fuel += item->iqty;

			/* Hack -- use as missiles until powerful */
			if (b_ptr->lev < 10) amt_missile += item->iqty;

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
	if (borg_spell_legal(2, 0) || borg_prayer_legal(1, 5))
	{
		amt_food += 1000;
	}

	/* Handle "identify" -> infinite identifies */
	if (borg_spell_legal(2, 4) || borg_prayer_legal(5, 2))
	{
		amt_ident += 1000;
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
	}

	/* Handle "enchant armor" */
	if (borg_prayer_legal(7, 4))
	{
		amt_enchant_to_a += 1000;
	}


	/*** Process the Needs ***/

	/* No need for fuel XXX XXX XXX */
	if (borg_items[INVEN_LITE].name1) amt_fuel += 1000;

	/* No need to *buy* stat increase potions */
	if (borg_stat_max[A_STR]) amt_add_stat[A_STR] += 1000;
	if (borg_stat_max[A_INT]) amt_add_stat[A_INT] += 1000;
	if (borg_stat_max[A_WIS]) amt_add_stat[A_WIS] += 1000;
	if (borg_stat_max[A_DEX]) amt_add_stat[A_DEX] += 1000;
	if (borg_stat_max[A_CON]) amt_add_stat[A_CON] += 1000;
	if (borg_stat_max[A_CHR]) amt_add_stat[A_CHR] += 1000;

	/* No need to *buy* stat repair potions XXX XXX XXX */
	if (!borg_base_fix_stat[A_STR]) amt_fix_stat[A_STR] += 1000;
	if (!borg_base_fix_stat[A_INT]) amt_fix_stat[A_INT] += 1000;
	if (!borg_base_fix_stat[A_WIS]) amt_fix_stat[A_WIS] += 1000;
	if (!borg_base_fix_stat[A_DEX]) amt_fix_stat[A_DEX] += 1000;
	if (!borg_base_fix_stat[A_CON]) amt_fix_stat[A_CON] += 1000;
	if (!borg_base_fix_stat[A_CHR]) amt_fix_stat[A_CHR] += 1000;

	/* No need for experience repair */
	if (!borg_base_fix_exp) amt_fix_exp += 1000;
}


/*
 * Analyze the equipment and inventory
 */
void borg_notice(void)
{
	/* Notice the equipment */
	borg_notice_aux1();

	/* Notice the inventory */
	borg_notice_aux2();
}



/*
 * Helper function -- notice the home equipment
 */
static void borg_notice_home_aux1(void)
{
	/* Nothing */
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
	num_recall = 0;
	num_phase = 0;
	num_escape = 0;
	num_teleport = 0;

	/* Reset healing */
	num_cure_critical = 0;
	num_cure_serious = 0;

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

	/* Reset enchantment */
	num_enchant_to_a = 0;
	num_enchant_to_d = 0;
	num_enchant_to_h = 0;


	/*** Process the inventory ***/

	/* Scan the home */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
		item = &shop->ware[i];

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
	if (borg_spell_legal(2, 0) || borg_prayer_legal(1, 5))
	{
		num_food += 1000;
	}

	/* Handle "identify" -> infinite identifies */
	if (borg_spell_legal(2, 4) || borg_prayer_legal(5, 2))
	{
		num_ident += 1000;
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
}


/*
 * Extract various bonuses
 */
void borg_notice_home(void)
{
	/* Notice the home equipment */
	borg_notice_home_aux1();

	/* Notice the home inventory */
	borg_notice_home_aux2();
}





/*
 * Helper function -- calculate "power" of equipment
 */
static s32b borg_power_aux1(void)
{
	int			hold;
	int			pounds;
	int			damage;

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

	/* XXX XXX XXX reward "extra" damage from "slaying" flags */

	/* Reward "damage" */
	value += (b_ptr->num_blow * damage * 500L);

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
	value += ((b_ptr->pspeed - 100) * 100000L);

	/* Hack -- Reward strength bonus */
	value += (b_ptr->stat_ind[A_STR] * 500L);

	/* Hack -- Reward intelligence bonus */
	if (mb_ptr->spell_book == TV_MAGIC_BOOK)
	{
		value += (b_ptr->stat_ind[A_INT] * 200L);
	}

	/* Hack -- Reward wisdom bonus */
	if (mb_ptr->spell_book == TV_PRAYER_BOOK)
	{
		value += (b_ptr->stat_ind[A_WIS] * 200L);
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


	/*** Reward current flags ***/

	/* Various flags */
	if (b_ptr->see_inv) value += 5000L;
	if (b_ptr->free_act) value += 10000L;
	if (b_ptr->slow_digest) value += 10L;
	if (b_ptr->regenerate) value += 50000L;
	if (b_ptr->ffall) value += 10L;
	if (b_ptr->hold_life) value += 10000L;
	if (b_ptr->telepathy) value += 50000L;
	if (b_ptr->lite) value += 2000L;

	/* Immunity flags */
	if (b_ptr->immune_acid) value += 80000L;
	if (b_ptr->immune_elec) value += 40000L;
	if (b_ptr->immune_fire) value += 60000L;
	if (b_ptr->immune_cold) value += 30000L;

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
	if (b_ptr->resist_sound) value += 100L;
	if (b_ptr->resist_shard) value += 100L;
	if (b_ptr->resist_nexus) value += 100L;
	if (b_ptr->resist_nethr) value += 2000L;
	if (b_ptr->resist_chaos) value += 2000L;
	if (b_ptr->resist_disen) value += 5000L;

	/* Sustain flags */
	if (b_ptr->sustain_str) value += 50L;
	if (b_ptr->sustain_int) value += 50L;
	if (b_ptr->sustain_wis) value += 50L;
	if (b_ptr->sustain_con) value += 50L;
	if (b_ptr->sustain_dex) value += 50L;
	if (b_ptr->sustain_chr) value += 50L;


	/*** XXX XXX XXX Reward "necessary" flags ***/

	/* Mega-Hack -- See invisible (level 10) */
	if (b_ptr->see_inv && (b_ptr->max_depth+1 >= 10)) value += 100000L;

	/* Mega-Hack -- Free action (level 20) */
	if (b_ptr->free_act && (b_ptr->max_depth+1 >= 20)) value += 100000L;


	/*** Reward powerful armor ***/

	/* Reward armor */
	value += ((b_ptr->ac + b_ptr->to_a) * 300L);


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
	    (((cur_wgt - max_wgt) / 10) > 0) &&
	    (b_ptr->max_lev < 20))
	{
		/* Mega-Hack -- Penalize heavy armor which hurts mana */
		value -= (((cur_wgt - max_wgt) / 10) * (20 - b_ptr->max_lev) * 10L);
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
			value -= 50000L;
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
			value -= 50000L;
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
	int			k, book;

	s32b		value = 0L;


	/*** Basic abilities ***/

	/* Reward fuel */
	k = 0;
	for (; k < 5 && k < amt_fuel; k++) value += 60000L;
	for (; k < 10 && k < amt_fuel; k++) value += 6000L;

	/* Reward food */
	k = 0;
	for (; k < 5 && k < amt_food; k++) value += 50000L;
	for (; k < 10 && k < amt_food; k++) value += 5000L;

	/* Reward ident */
	k = 0;
	for (; k < 20 && k < amt_ident; k++) value += 6000L;
	for (; k < 40 && k < amt_ident; k++) value += 600L;

	/* Reward recall */
	k = 0;
	for (; k < 3 && k < amt_recall; k++) value += 50000L;
	for (; k < 5 && k < amt_recall; k++) value += 5000L;

	/* Reward phase */
	k = 0;
	for (; k < 20 && k < amt_phase; k++) value += 500L;

	/* Reward escape */
	k = 0;
	for (; k < 5 && k < amt_escape; k++) value += 10000L;

	/* Reward teleport */
	k = 0;
	for (; k < 5 && k < amt_teleport; k++) value += 10000L;


	/*** Healing ***/

	/* Reward cure critical */
	k = 0;
	for (; k <  5 && k < amt_cure_critical; k++) value += 5000L;
	for (; k < 20 && k < amt_cure_critical; k++) value += 500L;

	/* Reward cure serious */
	k = 0;
	for (; k <  5 && k < amt_cure_serious; k++) value += 500L;
	for (; k < 10 && k < amt_cure_serious; k++) value += 50L;


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
	for (; k < 10 && k < amt_missile; k++) value += 1000L;
	for (; k < 30 && k < amt_missile; k++) value += 100L;


	/*** Various ***/

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
	if (amt_fix_exp) value += 500000;


	/*** Enchantment ***/

	/* Reward enchant armor */
	if (amt_enchant_to_a && xb_ptr->need_enchant_to_a) value += 300L;

	/* Reward enchant weapon to hit */
	if (amt_enchant_to_h && xb_ptr->need_enchant_to_h) value += 100L;

	/* Reward enchant weapon to damage */
	if (amt_enchant_to_d && xb_ptr->need_enchant_to_d) value += 500L;


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

			/* Hack -- Ignore "difficult" normal books */
			if ((when > 5) && (when >= b_ptr->max_lev + 2)) continue;

			/* Reward the book */
			k = 0;
			for (; k < 1 && k < amt_book[book]; k++) value += 500000L;
			for (; k < 2 && k < amt_book[book]; k++) value += 100000L;
		}
	}


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

	/* Return the value */
	return (value);
}




/*
 * Helper function -- calculate power of equipment in the home
 */
static s32b borg_power_home_aux1(void)
{
	s32b		value = 0L;

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
	for (k = 0; k < 10 && k < num_ident; k++) value += 200L - k*10L;

	/* Collect recall */
	for (k = 0; k < 10 && k < num_recall; k++) value += 300L - k*10L;

	/* Collect phase */
	for (k = 0; k < 10 && k < num_phase; k++) value += 200L - k*10L;

	/* Collect escape */
	for (k = 0; k < 10 && k < num_escape; k++) value += 200L - k*10L;

	/* Collect teleport */
	for (k = 0; k < 10 && k < num_teleport; k++) value += 200L - k*10L;


	/*** Healing ***/

	/* Collect cure critical */
	for (k = 0; k < 10 && k < num_cure_critical; k++) value += 150L - k*10L;

	/* Collect cure serious */
	for (k = 0; k < 10 && k < num_cure_serious; k++) value += 120L - k*10L;


	/*** Missiles ***/

	/* Collect missiles */
	for (k = 0; k < 50 && k < num_missile; k++) value += 10L;


	/*** Various ***/

	/* Hack -- Collect restore life levels */
	for (k = 0; k < 5 && k < num_fix_exp; k++) value += 500L - k*10L;


	/*** Enchantment ***/

	/* Reward enchant armor */
	for (k = 0; k < 5 && k < num_enchant_to_a; k++) value += 20L - k*1L;


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
		value += 100L;
	}


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
 */
static int borg_danger_aux1(int i)
{
	int k, n = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


	/* Mega-Hack -- unknown monsters */
	if (kill->r_idx >= MAX_R_IDX) return (1000);


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
			n += z;
			break;

			case RBE_POISON:
			z = (d_dice * d_side);
			if (b_ptr->resist_pois) break;
			n += 10;
			break;

			case RBE_UN_BONUS:
			z = (d_dice * d_side);
			if (b_ptr->resist_disen) break;
			n += 500;
			break;

			case RBE_UN_POWER:
			z = (d_dice * d_side);
			n += 100;
			break;

			case RBE_EAT_GOLD:
			z = (d_dice * d_side);
			if (100 <= adj_dex_safe[b_ptr->stat_ind[A_DEX]] + b_ptr->lev) break;
			if (b_ptr->au < 100) break;
			n += 10;
			break;

			case RBE_EAT_ITEM:
			z = (d_dice * d_side);
			if (100 <= adj_dex_safe[b_ptr->stat_ind[A_DEX]] + b_ptr->lev) break;
			n += 20;
			break;

			case RBE_EAT_FOOD:
			z = (d_dice * d_side);
			if (amt_food > 5) break;
			n += 5;
			break;

			case RBE_EAT_LITE:
			z = (d_dice * d_side);
			if (amt_fuel > 5) break;
			n += 20;
			break;

			case RBE_ACID:
			if (b_ptr->immune_acid) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			n += 40;
			break;

			case RBE_ELEC:
			if (b_ptr->immune_elec) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			n += 20;
			break;

			case RBE_FIRE:
			if (b_ptr->immune_fire) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			n += 40;
			break;

			case RBE_COLD:
			if (b_ptr->immune_cold) break;
			z = (d_dice * d_side);
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			n += 20;
			break;

			case RBE_BLIND:
			z = (d_dice * d_side);
			if (b_ptr->resist_blind) break;
			n += 10;
			break;

			case RBE_CONFUSE:
			z = (d_dice * d_side);
			if (b_ptr->resist_confu) break;
			n += 10;
			break;

			case RBE_TERRIFY:
			z = (d_dice * d_side);
			if (b_ptr->resist_fear) break;
			n += 10;
			break;

			case RBE_PARALYZE:
			z = (d_dice * d_side);
			if (b_ptr->free_act) break;
			n += 1000;
			break;

			case RBE_LOSE_STR:
			z = (d_dice * d_side);
			if (b_ptr->sustain_str) break;
			if (borg_base_stat[A_STR] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_DEX:
			z = (d_dice * d_side);
			if (b_ptr->sustain_dex) break;
			if (borg_base_stat[A_DEX] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_CON:
			z = (d_dice * d_side);
			if (b_ptr->sustain_con) break;
			if (borg_base_stat[A_CON] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_INT:
			z = (d_dice * d_side);
			if (b_ptr->sustain_int) break;
			if (borg_base_stat[A_INT] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_WIS:
			z = (d_dice * d_side);
			if (b_ptr->sustain_wis) break;
			if (borg_base_stat[A_WIS] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_CHR:
			z = (d_dice * d_side);
			if (b_ptr->sustain_chr) break;
			if (borg_base_stat[A_CHR] <= 3) break;
			n += 50;
			break;

			case RBE_LOSE_ALL:
			z = (d_dice * d_side);
			n += 200;
			break;

			case RBE_SHATTER:
			z = (d_dice * d_side);
			z -= (z * ((b_ptr->ac < 150) ? b_ptr->ac : 150) / 250);
			n += 100;
			break;

			case RBE_EXP_10:
			z = (d_dice * d_side);
			if (b_ptr->hold_life) break;
			n += 100;
			break;

			case RBE_EXP_20:
			z = (d_dice * d_side);
			if (b_ptr->hold_life) break;
			n += 150;
			break;

			case RBE_EXP_40:
			z = (d_dice * d_side);
			if (b_ptr->hold_life) break;
			n += 200;
			break;

			case RBE_EXP_80:
			z = (d_dice * d_side);
			if (b_ptr->hold_life) break;
			n += 250;
			break;
		}

		/* Add in damage */
		n += z;
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
 */
static int borg_danger_aux2(int i)
{
	int q, k, n = 0;

	int lev, hp;

	byte spell[96], num = 0;

	auto_kill *kill = &borg_kills[i];

	monster_race *r_ptr = &r_info[kill->r_idx];


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
			p += 10;
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
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			p += 40;
			break;

			case 96+9:    /* RF4_BR_ELEC */
			if (b_ptr->immune_elec) break;
			z = (hp / 3);
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 96+10:    /* RF4_BR_FIRE */
			if (b_ptr->immune_fire) break;
			z = (hp / 3);
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 96+11:    /* RF4_BR_COLD */
			if (b_ptr->immune_cold) break;
			z = (hp / 3);
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 96+12:    /* RF4_BR_POIS */
			z = (hp / 3);
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->resist_pois) break;
			p += 20;
			break;

			case 96+13:    /* RF4_BR_NETH */
			z = (hp / 6);
			if (b_ptr->resist_nethr) z = z / 2;
			if (b_ptr->resist_nethr) break;
			p += 50;
			if (b_ptr->hold_life) break;
			p += 150;
			break;

			case 96+14:    /* RF4_BR_LITE */
			z = (hp / 6);
			if (b_ptr->resist_lite) z = z / 2;
			if (b_ptr->resist_lite) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 96+15:    /* RF4_BR_DARK */
			z = (hp / 6);
			if (b_ptr->resist_dark) z = z / 2;
			if (b_ptr->resist_dark) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 96+16:    /* RF4_BR_CONF */
			z = (hp / 6);
			if (b_ptr->resist_confu) z = z / 2;
			if (b_ptr->resist_confu) break;
			p += 100;
			break;

			case 96+17:    /* RF4_BR_SOUN */
			z = (hp / 6);
			if (b_ptr->resist_sound) z = z / 2;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+18:    /* RF4_BR_CHAO */
			z = (hp / 6);
			if (b_ptr->resist_chaos) z = z / 2;
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
			if (b_ptr->resist_disen) z = z / 2;
			if (b_ptr->resist_disen) break;
			p += 500;
			break;

			case 96+20:    /* RF4_BR_NEXU */
			z = (hp / 3);
			if (b_ptr->resist_nexus) z = z / 2;
			if (b_ptr->resist_nexus) break;
			p += 100;
			break;

			case 96+21:    /* RF4_BR_TIME */
			z = (hp / 3);
			p += 200;
			break;

			case 96+22:    /* RF4_BR_INER */
			z = (hp / 6);
			p += 50;
			break;

			case 96+23:    /* RF4_BR_GRAV */
			z = (hp / 3);
			p += 50;
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+24:    /* RF4_BR_SHAR */
			z = (hp / 6);
			if (b_ptr->resist_shard) z = z / 2;
			if (b_ptr->resist_shard) break;
			p += 50;
			break;

			case 96+25:    /* RF4_BR_PLAS */
			z = (hp / 6);
			if (b_ptr->resist_sound) break;
			p += 50;
			break;

			case 96+26:    /* RF4_BR_WALL */
			z = (hp / 6);
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
			p += 40;
			break;

			case 128+1:    /* RF5_BA_ELEC */
			if (b_ptr->immune_elec) break;
			z = (lev * 3) / 2 + 8;
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+2:    /* RF5_BA_FIRE */
			if (b_ptr->immune_fire) break;
			z = (lev * 7) / 2 + 10;
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+3:    /* RF5_BA_COLD */
			if (b_ptr->immune_cold) break;
			z = (lev * 3) / 2 + 10;
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+4:    /* RF5_BA_POIS */
			z = (12 * 2);
			if (b_ptr->resist_pois) z = (z + 2) / 3;
			if (b_ptr->resist_pois) break;
			p += 20;
			break;

			case 128+5:    /* RF5_BA_NETH */
			z = (50 + (10 * 10) + lev);
			if (b_ptr->resist_nethr) z = z / 2;
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
			if (b_ptr->resist_dark) z = z / 2;
			if (b_ptr->resist_dark) break;
			if (b_ptr->resist_blind) break;
			p += 20;
			break;

			case 128+9:    /* RF5_DRAIN_MANA */
			if (b_ptr->msp) p += 10;
			break;

			case 128+10:    /* RF5_MIND_BLAST */
			z = 20;
			break;

			case 128+11:    /* RF5_BRAIN_SMASH */
			z = (12 * 15);
			p += 100;
			break;

			case 128+12:    /* RF5_CAUSE_1 */
			z = (3 * 8);
			break;

			case 128+13:    /* RF5_CAUSE_2 */
			z = (8 * 8);
			break;

			case 128+14:    /* RF5_CAUSE_3 */
			z = (10 * 15);
			break;

			case 128+15:    /* RF5_CAUSE_4 */
			z = (15 * 15);
			p += 50;
			break;

			case 128+16:    /* RF5_BO_ACID */
			if (b_ptr->immune_acid) break;
			z = ((7 * 8) + (lev / 3));
			if (b_ptr->resist_acid) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+17:    /* RF5_BO_ELEC */
			if (b_ptr->immune_elec) break;
			z = ((4 * 8) + (lev / 3));
			if (b_ptr->resist_elec) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+18:    /* RF5_BO_FIRE */
			if (b_ptr->immune_fire) break;
			z = ((9 * 8) + (lev / 3));
			if (b_ptr->resist_fire) z = (z + 2) / 3;
			p += 40;
			break;

			case 128+19:    /* RF5_BO_COLD */
			if (b_ptr->immune_cold) break;
			z = ((6 * 8) + (lev / 3));
			if (b_ptr->resist_cold) z = (z + 2) / 3;
			p += 20;
			break;

			case 128+20:    /* RF5_BO_POIS */
			/* XXX XXX XXX */
			break;

			case 128+21:    /* RF5_BO_NETH */
			z = (30 + (5 * 5) + (lev * 3) / 2);
			if (b_ptr->resist_nethr) z = z / 2;
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
			p += 10;
			break;

			case 128+28:    /* RF5_BLIND */
			p += 10;
			break;

			case 128+29:    /* RF5_CONF */
			p += 10;
			break;

			case 128+30:    /* RF5_SLOW */
			p += 5;
			break;

			case 128+31:    /* RF5_HOLD */
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
			p += 500;
			break;

			case 160+15:    /* RF6_XXX6X6 */
			break;

			case 160+16:    /* RF6_XXX7X6 */
			break;

			case 160+17:    /* RF6_XXX8X6 */
			break;

			case 160+18:    /* RF6_S_MONSTER */
			p += (lev) * 10;
			break;

			case 160+19:    /* RF6_S_MONSTERS */
			p += (lev) * 20;
			break;

			case 160+20:    /* RF6_S_ANT */
			p += (lev) * 20;
			break;

			case 160+21:    /* RF6_S_SPIDER */
			p += (lev) * 20;
			break;

			case 160+22:    /* RF6_S_HOUND */
			p += (lev) * 20;
			break;

			case 160+23:    /* RF6_S_HYDRA */
			p += (lev) * 20;
			break;

			case 160+24:    /* RF6_S_ANGEL */
			p += (lev) * 30;
			break;

			case 160+25:    /* RF6_S_DEMON */
			p += (lev) * 30;
			break;

			case 160+26:    /* RF6_S_UNDEAD */
			p += (lev) * 30;
			break;

			case 160+27:    /* RF6_S_DRAGON */
			p += (lev) * 30;
			break;

			case 160+28:    /* RF6_S_HI_UNDEAD */
			p += (lev) * 50;
			break;

			case 160+29:    /* RF6_S_HI_DRAGON */
			p += (lev) * 50;
			break;

			case 160+30:    /* RF6_S_WRAITH */
			p += (lev) * 50;
			break;

			case 160+31:    /* RF6_S_UNIQUE */
			p += (lev) * 50;
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
		v1 = borg_danger_aux1(i);

		/* Reduce danger from sleeping monsters */
		if ((!kill->awake) && (d > 1)) v1 = v1 / d;

		/* Danger */
		if (v1)
		{
			/* Attacks after movement */
			r = (q - ((d-1) * 10));

			/* Hack -- stumble sometimes XXX XXX XXX */
			if (r_ptr->flags1 & (RF1_RAND_25)) r -= (r / 4);
			if (r_ptr->flags1 & (RF1_RAND_50)) r -= (r / 2);

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
	else if (!borg_projectable(y9, x9, y, x))
	{
		v2 = 0;
	}

	/* Danger from spell attacks */
	else
	{
		/* Spell attacks */
		v2 = borg_danger_aux2(i);

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

	/* Hack -- assume reproducers always breed */
	if (r_ptr->flags2 & RF2_MULTIPLY) p *= 2;

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
	if (b_ptr->cur_lite < 1) return (TRUE);

	/* Must have "fuel" */
	if (amt_fuel < 1) return (TRUE);

	/* Must have "food" */
	if (amt_food < 1) return (TRUE);

	/* Assume happy at level 1 */
	if (b_ptr->depth <= 1) return (FALSE);


	/*** Level 2 and below ***/

	/* Must have good lite */
	if (b_ptr->cur_lite < 2) return (TRUE);

	/* Must have "fuel" */
	if (amt_fuel < 3) return (TRUE);

	/* Must have "food" */
	if (amt_food < 3) return (TRUE);

	/* Must have "recall" */
	if (amt_recall < 2) return (TRUE);


	/* Assume happy */
	return (FALSE);
}


/*
 * Determine if the Borg meets the "minimum" requirements for a level
 */
static bool borg_prepared_aux(int depth)
{
	/* Always ready for the town */
	if (!depth) return (TRUE);


	/*** Essential Items for Level 1 ***/

	/* Require lite (any) */
	if (b_ptr->cur_lite < 1) return (FALSE);

	/* Require food */
	if (amt_food < 5) return (FALSE);

	/* Usually ready for level 1 */
	if (depth <= 1) return (TRUE);


	/*** Essential Items for Level 2 ***/

	/* Require lite (radius two) */
	if (b_ptr->cur_lite < 2) return (FALSE);

	/* Require fuel */
	if (amt_fuel < 5) return (FALSE);

	/* Require recall */
	if (amt_recall < 2) return (FALSE);

	/* Scrolls of Identify (for identification) */
	if (amt_ident < 5) return (FALSE);

	/* Usually ready for level 2 */
	if (depth <= 2) return (TRUE);


	/*** Essential Items for Level 3 and 4 ***/

	/* Scrolls of Word of Recall */
	if (amt_recall < 3) return (FALSE);

	/* Scrolls of Identify */
	if (amt_ident < 10) return (FALSE);

	/* Potions of Cure Serious Wounds */
	if (amt_cure_serious + amt_cure_critical < 2) return (FALSE);

	/* Usually ready for level 3 and 4 */
	if (depth <= 4) return (TRUE);


	/*** Essential Items for Level 5 to 9 ***/

	/* Scrolls of Word of Recall */
	if (amt_recall < 4) return (FALSE);

	/* Scrolls of Identify */
	if (amt_ident < 15) return (FALSE);

	/* Potions of Cure Serious/Critical Wounds */
	if (amt_cure_serious + amt_cure_critical < 5) return (FALSE);

	/* Usually ready for level 5 to 9 */
	if (depth <= 9) return (TRUE);


	/*** Essential Items for Level 10 to 19 ***/

	/* Escape or Teleport */
	if (amt_teleport + amt_escape < 2) return (FALSE);

	/* Identify */
	if (amt_ident < 20) return (FALSE);

	/* Potions of Cure Critical Wounds */
	if (amt_cure_critical < 5) return (FALSE);

	/* See invisible */
	if (!b_ptr->see_inv) return (FALSE);

	/* Usually ready for level 10 to 19 */
	if (depth <= 19) return (TRUE);


	/*** Essential Items for Level 20 to 39 ***/

	/* Escape and Teleport */
	if (amt_escape < 2) return (FALSE);
	if (amt_teleport < 2) return (FALSE);

	/* Cure Critical Wounds */
	if (amt_cure_critical < 10) return (FALSE);

	/* Free action */
	if (!b_ptr->free_act) return (FALSE);

	/* Basic resistance XXX XXX XXX */
	if (!b_ptr->resist_acid) return (FALSE);
	if (!b_ptr->resist_fire) return (FALSE);

	/* Usually ready for level 20 to 39 */
	if (depth <= 39) return (TRUE);


	/*** Essential Items for Level 40 to 99 ***/

	/* Minimal level */
	if (b_ptr->lev < 25) return (FALSE);

	/* Minimal hitpoints */
	if (b_ptr->mhp < 250) return (FALSE);

	/* High stats XXX XXX XXX */
	if (borg_base_stat[A_STR] < 18+50) return (FALSE);
	if (borg_base_stat[A_INT] < 18+50) return (FALSE);
	if (borg_base_stat[A_WIS] < 18+50) return (FALSE);
	if (borg_base_stat[A_DEX] < 18+50) return (FALSE);
	if (borg_base_stat[A_CON] < 18+50) return (FALSE);
	if (borg_base_stat[A_CHR] < 18+50) return (FALSE);

#if 0
	/* XXX XXX XXX Hold Life */
	if (!b_ptr->hold_life) return (FALSE);

	/* XXX XXX XXX Resist Disenchantment */
	if (!b_ptr->resist_disen) return (FALSE);
#endif

	/* Usually ready for level 40 to 99 */
	if (depth <= 99) return (TRUE);


	/*** Essential Items for Level 100 ***/

	/* Assume ready */
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
	/* Town and First level */
	if (depth <= 1) return (TRUE);

	/* Must meet minimal requirements */
	if (!borg_prepared_aux(depth)) return (FALSE);

	/* Favorite level */
	if (depth <= borg_happy_depth) return (TRUE);

	/* Check uniques XXX XXX XXX */

	/* Run away */
	return (FALSE);
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

