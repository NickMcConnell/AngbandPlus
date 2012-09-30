/* File: zborg4.c */
/*  Purpose: Notice and Power code for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"


/*
 * Various "amounts" (for the home)
 */

int num_food;
int num_food_scroll;
int num_ident;
int num_star_ident;
int num_remove_curse;
int num_star_remove_curse;
int num_recall;
int num_phase;
int num_escape;
int num_teleport;
int num_berserk;
int num_teleport_level;

int num_cure_critical;
int num_cure_serious;
int num_cure_light;

int num_pot_resist;

int num_missile;

int num_book[8][4];	/* [realm][book] */

int num_fix_stat[7];	/* #7 is to fix all stats */

int num_fix_exp;
int num_mana;
int num_heal;
int num_ez_heal;
int num_glyph;
int num_mass_genocide;
int num_goi_pot;

int num_brand_weapon;	/*apw brand bolts */
int num_genocide;

s16b home_slot_free;
s16b home_damage;
s16b num_duplicate_items;
int num_slow_digest;
int num_regenerate;
int num_telepathy;
int num_lite;
int num_see_inv;
int num_invisible;	/* apw */

int num_ffall;
int num_free_act;
int num_hold_life;
int num_immune_acid;
int num_immune_elec;
int num_immune_fire;
int num_immune_cold;
int num_resist_acid;
int num_resist_elec;
int num_resist_fire;
int num_resist_cold;
int num_resist_pois;
int num_resist_conf;
int num_resist_sound;
int num_resist_lite;
int num_resist_dark;
int num_resist_chaos;
int num_resist_disen;
int num_resist_shard;
int num_resist_nexus;
int num_resist_blind;
int num_resist_neth;
int num_sustain_str;
int num_sustain_int;
int num_sustain_wis;
int num_sustain_dex;
int num_sustain_con;
int num_sustain_all;

int num_artifact;
int num_bad_curse;
int num_speed;
int num_edged_weapon;
int num_bad_gloves;
int num_weapons;
int num_bow;
int num_rings;
int num_neck;
int num_armor;
int num_cloaks;
int num_shields;
int num_hats;
int num_gloves;
int num_boots;

/*
 * Remember items in the home.  (Only one home at a time)
 */
list_item *borg_home;
int home_num;


/* The shop that corresponds to the current home */
int home_shop = -1;

/* Include shop items in power calculation */
int use_shop;

void borg_list_info(byte list_type, vptr dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Don't do anything if the borg is inactive */
	if (!borg_active)
	{
		/* Done */
		return;
	}

	/* Notice changes */
	switch (list_type)
	{
		case LIST_INVEN:
		{
			/* Inventory changed so goals must change. */
			goal_shop = -1;

			/* Note changed inventory */
			borg_do_destroy = TRUE;
			break;
		}

		case LIST_EQUIP:
		{
			/* Equipment changed so goals must change. */
			goal_shop = -1;

			/* Note changed inventory */
			borg_do_destroy = TRUE;
			break;
		}

		case LIST_FLOOR:
		{

			break;
		}

		case LIST_STORE:
		{
			break;
		}

		case LIST_HOME:
		{
			/* Number of items */
			home_num = cur_num;

			/* Save items for later... */
			C_COPY(borg_home, cur_list, cur_num, list_item);

			break;
		}
		default:
		{
			/* Paranoia */
			quit_fmt("Unrecognised list type %d", list_type);
		}
	}
}


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
 * Notice player flags
 */
static void borg_notice_player(void)
{
	object_flags oflags;
	object_flags *of_ptr = &oflags;

	/* Recalc some Variables */
	bp_ptr->ac = 0;
	bp_ptr->speed = 110;

	/* Start with a single blow per turn */
	bp_ptr->blows = 1;

	/* Base infravision (purely racial) */
	bp_ptr->see_infra = rb_ptr->infra;

	/* Base skill -- disarming */
	bp_ptr->skill_dis = rb_ptr->r_dis + cb_ptr->c_dis;

	/* Base skill -- magic devices */
	bp_ptr->skill_dev = rb_ptr->r_dev + cb_ptr->c_dev;

	/* Base skill -- saving throw */
	bp_ptr->skill_sav = rb_ptr->r_sav + cb_ptr->c_sav;

	/* Base skill -- stealth */
	bp_ptr->skill_stl = rb_ptr->r_stl + cb_ptr->c_stl;

	/* Base skill -- searching ability */
	bp_ptr->skill_sns = rb_ptr->r_sns + cb_ptr->c_sns;

	/* Base skill -- searching frequency */
	bp_ptr->skill_fos = rb_ptr->r_fos + cb_ptr->c_fos;

	/* Base skill -- combat (normal) */
	bp_ptr->skill_thn = rb_ptr->r_thn + cb_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	bp_ptr->skill_thb = rb_ptr->r_thb + cb_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	bp_ptr->skill_tht = rb_ptr->r_thb + cb_ptr->c_thb;

	/* Racial Skills */

	/* Extract the player flags */
	player_flags(of_ptr);

	bp_ptr->flags[0] |= oflags.flags[0];
	bp_ptr->flags[1] |= oflags.flags[1];
	bp_ptr->flags[2] |= oflags.flags[2];
	bp_ptr->flags[3] |= oflags.flags[3];

	/* Mutation flags */
	bp_ptr->muta1 = p_ptr->muta1;
	bp_ptr->muta2 = p_ptr->muta2;
	bp_ptr->muta3 = p_ptr->muta3;

	/* Sustain flags */
	if (FLAG(of_ptr, TR_SUST_STR)) bp_ptr->sust[A_STR] = TRUE;
	if (FLAG(of_ptr, TR_SUST_INT)) bp_ptr->sust[A_INT] = TRUE;
	if (FLAG(of_ptr, TR_SUST_WIS)) bp_ptr->sust[A_WIS] = TRUE;
	if (FLAG(of_ptr, TR_SUST_DEX)) bp_ptr->sust[A_DEX] = TRUE;
	if (FLAG(of_ptr, TR_SUST_CON)) bp_ptr->sust[A_CON] = TRUE;
	if (FLAG(of_ptr, TR_SUST_CHR)) bp_ptr->sust[A_CHR] = TRUE;

	/* Bloating slows the player down (a little) */
	if (bp_ptr->status.gorged) bp_ptr->speed -= 10;
}

/*
 * Find which item goes in an equipment slot.
 *
 * Normally, this is just the item already there,
 * however, sometimes we want to simulate another
 * item being in that location.
 *
 * Note: there must only be one TREAT_AS_SWAP
 * item in the inventory at any time.
 */
list_item *look_up_equip_slot(int slot)
{
	list_item *l_ptr;

	int i;

	/* check for valid slots */
	if (slot < 0 || slot > equip_num) return (NULL);

	/* Look in equipment */
	l_ptr = &equipment[slot];

	/* Does it exist and are we aware? */
	if (l_ptr->k_idx)
	{
		/* Normal item? */
		if (l_ptr->treat_as == TREAT_AS_NORM) return (l_ptr);

		/* Missing item? */
		if (l_ptr->treat_as == TREAT_AS_GONE) return (NULL);

		/* Assume TREAT_AS_SWAP */
	}
	else
	{
		/* Is it an empty slot or an unknown ring or amulet */
		if (l_ptr->tval == TV_RING || l_ptr->tval == TV_AMULET) return (l_ptr);

		/* Optimise common case of empty slot */
		if (l_ptr->treat_as != TREAT_AS_SWAP) return (NULL);
	}

	/* Look at current shop */
	if (use_shop)
	{
		for (i = 0; i < cur_num; i++)
		{
			l_ptr = &cur_list[i];

			/* Does it exist and are we aware? */
			if (l_ptr->k_idx)
			{
				/* The item to swap with */
				if (l_ptr->treat_as == TREAT_AS_SWAP) return (l_ptr);
			}
		}
	}
	else
	{
		/* Otherwise, scan the inventory */
		for (i = 0; i < inven_num; i++)
		{
			l_ptr = &inventory[i];

			/* Does it exist and are we aware? */
			if (l_ptr->k_idx)
			{
				/* The item to swap with */
				if (l_ptr->treat_as != TREAT_AS_NORM) return (l_ptr);
			}
		}
	}

	/* No match! */
	return (NULL);
}


/* Does this item have some bad curse that the borg can't handle */
bool borg_test_bad_curse(list_item *l_ptr)
{
	int i;

	list_item temp;


	/* Just checking */
	if (!l_ptr) return (FALSE);

	/* No borg can handle not teleporting */
	if (KN_FLAG(l_ptr, TR_NO_TELE)) return (TRUE);

	/* The borg can't keep up with this drain */
	if (KN_FLAG(l_ptr, TR_DRAIN_EXP) && bp_ptr->lev < 50) return (TRUE);

	/* This curse is meaningless for warriors */
	if (KN_FLAG(l_ptr, TR_NO_MAGIC) && borg_class != CLASS_WARRIOR) return (TRUE);

	/* This curse is no problem if all stats are sustained */
	if (KN_FLAG(l_ptr, TR_DRAIN_STATS) ||
		KN_FLAG(l_ptr, TR_TY_CURSE))
	{
		/* Clear */
		temp.kn_flags[1] = 0;

		/* Check the equipment */
		for (i = 0; i < equip_num; i++)
		{
			l_ptr = look_up_equip_slot(i);

			/* No empty slots */
			if (!l_ptr) continue;

			/* Copy the flags */
			temp.kn_flags[1] |= l_ptr->kn_flags[1];
		}

		/* If there are enough sustains this curse can be ignored */
		if (!KN_FLAG(&temp, TR_SUST_STR) ||
			!KN_FLAG(&temp, TR_SUST_INT) ||
			!KN_FLAG(&temp, TR_SUST_WIS) ||
			!KN_FLAG(&temp, TR_SUST_DEX) ||
			!KN_FLAG(&temp, TR_SUST_CON))
		{
			/* not enough sustains */
			return (TRUE);
		}

		/* Only high level borgs can handle topi */
		if (KN_FLAG(l_ptr, TR_TY_CURSE) &&
			bp_ptr->lev < 50) return (TRUE);
	}

	/* No curse */
	return (FALSE);
}
					


/* Determine if this item should be id'd or something. */
static void borg_notice_improve_item(list_item *l_ptr, bool equip)
{
	/* Paranoia */
	if (!l_ptr) return;

	/* Does it need to be id'd? */
	if (!borg_obj_known_p(l_ptr))
	{
		/* Count it */
		bp_ptr->able.id_item += 1;
	}
	/* Do the other checks only for identified items */
	else
	{
		/* Does it need to be star_id'd? */
		if (!borg_obj_known_full(l_ptr) && 
			borg_obj_star_id_able(l_ptr)) bp_ptr->able.star_id_item += 1;

		/* All equipment or interesting items from the inventory */
		if (equip || borg_obj_is_ego_art(l_ptr))
		{
			/* Check for cursed items */
			if (KN_FLAG(l_ptr, TR_CURSED)) bp_ptr->status.cursed = TRUE;

			/* Maybe try a *remove curse* on this item */
			if (KN_FLAG(l_ptr, TR_HEAVY_CURSE)) bp_ptr->status.heavy_curse = TRUE;
		}
	}
}


/*
 * Notice the effects of equipment
 */
static void borg_notice_equip(int *extra_blows, int *extra_shots,
                              int *extra_might)
{
	int i;

	list_item *l_ptr;

	/* Scan the equipment */
	for (i = 0; i < equip_num; i++)
	{
		l_ptr = look_up_equip_slot(i);

		/* Ignore empty slots */
		if (!l_ptr) continue;

		/* There is no flag for an amulet of sustenance */
		if (i == EQUIP_NECK)
		{
			/* Not too much or the borg will never replace this amulet */
			if (k_info[l_ptr->k_idx].sval == SV_AMULET_SUSTENANCE)
			{
				bp_ptr->food += 5;
				SET_FLAG(bp_ptr, TR_SLOW_DIGEST);
			}

			/* You can only see the luck flag when it has *id* */
			if (k_info[l_ptr->k_idx].sval == SV_AMULET_LUCK)
			{
				SET_FLAG(bp_ptr, TR_LUCK_10);
			}
		}

		/* If the borg has a cloak that is not identified */
		if (i == EQUIP_OUTER && !borg_obj_known_p(l_ptr))
		{
			/* Shadow cloak */
			if (k_info[l_ptr->k_idx].sval == SV_SHADOW_CLOAK)
			{
				/* Add the dark and light flags */
				SET_FLAG(bp_ptr, TR_RES_DARK);
				SET_FLAG(bp_ptr, TR_RES_LITE);
			}

			/* Elven cloak */
			if (k_info[l_ptr->k_idx].sval == SV_ELVEN_CLOAK)
			{
				/* Surely not a bad cloak */
				bp_ptr->skill_stl += 1;
			}
		}

		/* Does this item need id or remove curse? */
		borg_notice_improve_item(l_ptr, TRUE);

		/* Affect stats */
		if (KN_FLAG(l_ptr, TR_STR)) my_stat_add[A_STR] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_INT)) my_stat_add[A_INT] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_WIS)) my_stat_add[A_WIS] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_DEX)) my_stat_add[A_DEX] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_CON)) my_stat_add[A_CON] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_CHR)) my_stat_add[A_CHR] += l_ptr->pval;

		/* Affect flags */
		bp_ptr->flags[0] |= l_ptr->kn_flags[0];
		bp_ptr->flags[1] |= l_ptr->kn_flags[1];
		bp_ptr->flags[2] |= l_ptr->kn_flags[2];
		bp_ptr->flags[3] |= l_ptr->kn_flags[3];

		/* Affect infravision */
		if (KN_FLAG(l_ptr, TR_INFRA)) bp_ptr->see_infra += l_ptr->pval;

		/* Affect stealth */
		if (KN_FLAG(l_ptr, TR_STEALTH)) bp_ptr->skill_stl += l_ptr->pval;

		/* Affect saving throw */
		if (KN_FLAG(l_ptr, TR_LUCK_10)) bp_ptr->skill_sav += 10;

		/* Affect searching ability (factor of five) */
		if (KN_FLAG(l_ptr, TR_SEARCH)) bp_ptr->skill_sns += l_ptr->pval * 5;

		/* Affect searching frequency (factor of five) */
		if (KN_FLAG(l_ptr, TR_SEARCH)) bp_ptr->skill_fos += l_ptr->pval * 5;

		/* Affect digging (factor of 20) */
		if (KN_FLAG(l_ptr, TR_TUNNEL)) bp_ptr->skill_dig += l_ptr->pval * 20;

		/* Affect speed */
		if (KN_FLAG(l_ptr, TR_SPEED)) bp_ptr->speed += l_ptr->pval;

		/* Affect spell points */
		if (KN_FLAG(l_ptr, TR_SP)) bp_ptr->mana_bonus += l_ptr->pval;

		/* Affect blows */
		if (KN_FLAG(l_ptr, TR_BLOWS)) *extra_blows += l_ptr->pval;

		/* Boost shots */
		if (KN_FLAG(l_ptr, TR_XTRA_SHOTS)) (*extra_shots)++;

		/* Boost might */
		if (KN_FLAG(l_ptr, TR_XTRA_MIGHT)) (*extra_might)++;

		/* Immunity flags */
		if (KN_FLAG(l_ptr, TR_IM_FIRE)) my_oppose_fire = TRUE;
		if (KN_FLAG(l_ptr, TR_IM_ACID)) my_oppose_elec = TRUE;
		if (KN_FLAG(l_ptr, TR_IM_COLD)) my_oppose_elec = TRUE;
		if (KN_FLAG(l_ptr, TR_IM_ELEC)) my_oppose_elec = TRUE;

		/* Sustain flags */
		if (KN_FLAG(l_ptr, TR_SUST_STR)) bp_ptr->sust[A_STR] = TRUE;
		if (KN_FLAG(l_ptr, TR_SUST_INT)) bp_ptr->sust[A_INT] = TRUE;
		if (KN_FLAG(l_ptr, TR_SUST_WIS)) bp_ptr->sust[A_WIS] = TRUE;
		if (KN_FLAG(l_ptr, TR_SUST_DEX)) bp_ptr->sust[A_DEX] = TRUE;
		if (KN_FLAG(l_ptr, TR_SUST_CON)) bp_ptr->sust[A_CON] = TRUE;
		if (KN_FLAG(l_ptr, TR_SUST_CHR)) bp_ptr->sust[A_CHR] = TRUE;

		/* Modify the base armor class */
		bp_ptr->ac += l_ptr->ac;

		/* Apply the bonuses to armor class */
		bp_ptr->ac += l_ptr->to_a;

		/* Keep track of weight */
		bp_ptr->weight += l_ptr->weight;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == EQUIP_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == EQUIP_BOW) continue;

		/* Apply the bonuses to hit/damage */
		bp_ptr->to_h += l_ptr->to_h;
		bp_ptr->to_d += l_ptr->to_d;
	}
}


/*
 * Recalculate player stats
 */
static void borg_notice_stats(void)
{
	int i;

	/* Update stats */
	for (i = 0; i < A_MAX; i++) my_stat_ind[i] = MIN(37, p_ptr->stat[i].ind);

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	bp_ptr->ac += ((int)(adj_dex_ta[my_stat_ind[A_DEX]]) - 128);
	bp_ptr->to_d += ((int)(adj_str_td[my_stat_ind[A_STR]]) - 128);
	bp_ptr->to_h += ((int)(adj_dex_th[my_stat_ind[A_DEX]]) - 128);
}


/*
 * Examine bow
 */
static void borg_notice_shooter(int hold, int extra_might, int extra_shots)
{
	list_item *l_ptr;

	/* Start with a single shot per turn */
	int my_num_fire = 1;

	/* Reset the "ammo" tval to darts by default */
	my_ammo_tval = 0;

	/* Reset the shooting power */
	my_ammo_power = 0;

	/* Reset the shooting range */
	my_ammo_range = 0;

	/* Examine the "current bow" */
	l_ptr = look_up_equip_slot(EQUIP_BOW);

	/* No bow? */
	if (!l_ptr) return;


	bp_ptr->b_to_d = l_ptr->to_d;
	if (bp_ptr->b_to_d < 8 && bp_ptr->lev >= 25)
		bp_ptr->b_to_d = 8;

	/* It is hard to carry a heavy bow */
	if (hold < l_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		bp_ptr->to_h += 2 * (hold - l_ptr->weight / 10);
	}

	/* Compute "extra shots" if needed */
	if (l_ptr->k_idx && (hold >= l_ptr->weight / 10))
	{
		/* Take note of required "tval" for missiles */
		switch (k_info[l_ptr->k_idx].sval)
		{
			case SV_SLING:
			{
				my_ammo_tval = TV_SHOT;
				my_ammo_power = 16;
				if (extra_might) my_ammo_power = 24;
				break;
			}

			case SV_SHORT_BOW:
			{
				my_ammo_tval = TV_ARROW;
				if (extra_might)
				{
					my_ammo_power = 18;
					my_ammo_range = 20;
				}
				else
				{
					my_ammo_power = 12;
					my_ammo_range = 15;
				}
				break;
			}

			case SV_LONG_BOW:
			{
				my_ammo_tval = TV_ARROW;
				if (borg_stat[A_STR] >= 160)
				{
					if (extra_might)
					{
						my_ammo_power = 24;
						my_ammo_range = 25;
					}
					else
					{
						my_ammo_power = 18;
						my_ammo_range = 20;
					}
				}
				else
				{
					if (extra_might)
					{
						my_ammo_power = 18;
						my_ammo_range = 20;
					}
					else
					{
						my_ammo_power = 12;
						my_ammo_range = 15;
					}
				}
				break;
			}

			case SV_LIGHT_XBOW:
			{
				my_ammo_tval = TV_BOLT;
				if (extra_might)
				{
					my_ammo_power = 28;
					my_ammo_range = 30;
				}
				else
				{
					my_ammo_power = 23;
					my_ammo_range = 25;
				}
				break;
			}

			case SV_HEAVY_XBOW:
			{
				my_ammo_tval = TV_BOLT;
				if (extra_might)
				{
					my_ammo_power = 29;
					my_ammo_range = 35;
				}
				else
				{
					my_ammo_power = 24;
					my_ammo_range = 30;
				}
				break;
			}
		}

		/* Hack -- Reward High Level Rangers using Bows */
		if ((borg_class == CLASS_RANGER) && (my_ammo_tval == TV_ARROW))
		{
			/* Extra shot at level 15 */
			if (bp_ptr->lev >= 15) my_num_fire++;

			/* Extra shot at level 30 */
			if (bp_ptr->lev >= 30) my_num_fire++;

			/* Extra shot at level 45 */
			if (bp_ptr->lev >= 45) my_num_fire++;
		}

		/* Hack -- Reward High Level Rangers using XBows */
		if ((borg_class == CLASS_RANGER) && (my_ammo_tval == TV_BOLT))
		{
			/* Extra shot at level 30 */
			if (bp_ptr->lev >= 30) my_num_fire++;
		}

		/* Hack -- Reward High Level Rogues using Slings */
		if ((borg_class == CLASS_RANGER) && (my_ammo_tval == TV_SHOT))
		{
			/* Extra shot at level 20 */
			if (bp_ptr->lev >= 20) my_num_fire++;

			/* Extra shot at level 40 */
			if (bp_ptr->lev >= 40) my_num_fire++;
		}

		/* Hack -- Reward High Level Warriors */
		if (borg_class == CLASS_WARRIOR)
		{
			/* Extra shot at level 40 */
			if (bp_ptr->lev >= 40) my_num_fire++;
		}

		/* Add in the "bonus shots" */
		my_num_fire += extra_shots;

		/* Require at least one shot */
		if (my_num_fire < 1) my_num_fire = 1;
	}

	/* Calculate "average" damage per "normal" shot (times 2) */
	bp_ptr->b_max_dam = my_ammo_power *
		(bp_ptr->b_to_d + 100) * 3 / 100;
	bp_ptr->b_max_dam *= (my_num_fire + 1) / 2;
}


/*
 * Examine sword
 */
static void borg_notice_weapon(int hold, int extra_blows)
{
	list_item *l_ptr;

	/* Examine the "main weapon" */
	l_ptr = look_up_equip_slot(EQUIP_WIELD);

	/* No weapon? */
	if (!l_ptr) return;

	bp_ptr->w_to_d = l_ptr->to_d;
	if (bp_ptr->w_to_d < 8 && bp_ptr->lev >= 25)
		bp_ptr->w_to_d = 8;

	/* It is hard to hold a heavy weapon */
	if (hold < l_ptr->weight / 10)
	{
		bp_ptr->status.hvy_weapon = TRUE;

		/* Hard to wield a heavy weapon */
		bp_ptr->to_h += 2 * (hold - l_ptr->weight / 10);
	}
	else
	{
		bp_ptr->status.hvy_weapon = FALSE;
	}

	/* Normal weapons */
	if (l_ptr->k_idx && (hold >= l_ptr->weight / 10))
	{
		int str_index, dex_index;
		int num = 0, wgt = 0, mul = 0, div = 0;

		/* Analyze the class */
		switch (borg_class)
		{
			case CLASS_WARRIOR:
			{
				/* Warrior */
				num = 5;
				wgt = 30;
				mul = 5;
				break;
			}

			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			{
				/* Mage */
				num = 2;
				wgt = 40;
				mul = 2;
				break;
			}

			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
			{
				/* Priest, Mindcrafter */
				num = 4;
				wgt = 35;
				mul = 3;
				break;
			}

			case CLASS_ROGUE:
			{
				/* Rogue */
				num = 4;
				wgt = 30;
				mul = 3;
				break;
			}

			case CLASS_RANGER:
			{
				/* Ranger */
				num = 4;
				wgt = 35;
				mul = 4;
				break;
			}

			case CLASS_PALADIN:
			{
				/* Paladin */
				num = 4;
				wgt = 30;
				mul = 4;
				break;
			}

			case CLASS_WARRIOR_MAGE:
			{
				/* Warrior-Mage */
				num = 4;
				wgt = 35;
				mul = 3;
				break;
			}

			case CLASS_CHAOS_WARRIOR:
			{
				/* Chaos Warrior */
				num = 4;
				wgt = 30;
				mul = 4;
				break;
			}

			case CLASS_MONK:
			{
				/* Monk */
				num = ((p_ptr->lev < 40) ? 2 : 3);
				wgt = 40;
				mul = 4;
				break;
			}
		}

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((l_ptr->weight < wgt) ? wgt : l_ptr->weight);

		/* Get the strength vs weight */
		str_index = (adj_str_blow[my_stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[my_stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		bp_ptr->blows = blows_table[str_index][dex_index];

		/* Maximal value */
		if (bp_ptr->blows > num) bp_ptr->blows = num;

		/* Add in the "bonus blows" */
		bp_ptr->blows += extra_blows;

		/* Require at least one blow */
		if (bp_ptr->blows < 1) bp_ptr->blows = 1;

		/* Boost digging skill by weapon weight */
		bp_ptr->skill_dig += (l_ptr->weight / 10);
	}

	/* priest weapon penalty for non-blessed edged weapons */
	if ((borg_class == CLASS_PRIEST) &&
		((l_ptr->tval == TV_SWORD) || (l_ptr->tval == TV_POLEARM)) &&
		!KN_FLAG(l_ptr, TR_BLESSED))
	{
		/* Reduce the real bonuses */
		bp_ptr->to_h -= 5;
		bp_ptr->to_d -= 5;
	}
}


/*
 * Notice skills
 */
static void borg_notice_skills(void)
{
	/* Affect Skill -- stealth (bonus one) */
	bp_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	bp_ptr->skill_dis += adj_dex_dis[my_stat_ind[A_DEX]];
	bp_ptr->skill_dis += adj_int_dis[my_stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	bp_ptr->skill_dev += adj_int_dev[my_stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	bp_ptr->skill_sav += adj_wis_sav[my_stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	bp_ptr->skill_dig += adj_str_dig[my_stat_ind[A_STR]];

	/* Affect Skill -- disarming (Level, by Class) */
	bp_ptr->skill_dis += (cb_ptr->x_dis * bp_ptr->max_lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	bp_ptr->skill_dev += (cb_ptr->x_dev * bp_ptr->max_lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	bp_ptr->skill_sav += (cb_ptr->x_sav * bp_ptr->max_lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	bp_ptr->skill_stl += (cb_ptr->x_stl * bp_ptr->max_lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	bp_ptr->skill_sns += (cb_ptr->x_sns * bp_ptr->max_lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	bp_ptr->skill_fos += (cb_ptr->x_fos * bp_ptr->max_lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	bp_ptr->skill_thn += (cb_ptr->x_thn * bp_ptr->max_lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	bp_ptr->skill_thb += (cb_ptr->x_thb * bp_ptr->max_lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	bp_ptr->skill_tht += (cb_ptr->x_thb * bp_ptr->max_lev / 10);

	/* Limit Skill -- stealth from 0 to 30 */
	if (bp_ptr->skill_stl > 30) bp_ptr->skill_stl = 30;
	if (bp_ptr->skill_stl < 0) bp_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (bp_ptr->skill_dig < 1) bp_ptr->skill_dig = 1;
}


/*
 * Monks are special
 */
static void borg_recalc_monk(int extra_blows)
{
	int monk_arm_wgt = 0;
	int ma = MAX_MA - 1;
	const martial_arts *ma_ptr = &ma_blows[MAX_MA];

	int i;

	list_item *l_ptr;

	/* Weigh the armor */
	for (i = EQUIP_BODY; i <= EQUIP_FEET; i++)
	{
		l_ptr = look_up_equip_slot(i);

		/* Add up the total */
		if (l_ptr) monk_arm_wgt += l_ptr->weight;
	}

	/* Consider the Martial Arts */
	if (!look_up_equip_slot(EQUIP_WIELD))
	{
		bp_ptr->blows = 2;

		if (bp_ptr->lev > 9) bp_ptr->blows++;
		if (bp_ptr->lev > 14) bp_ptr->blows++;
		if (bp_ptr->lev > 24) bp_ptr->blows++;
		if (bp_ptr->lev > 34) bp_ptr->blows++;
		if (bp_ptr->lev > 44) bp_ptr->blows++;
		if (bp_ptr->lev > 49) bp_ptr->blows++;

		if (monk_arm_wgt < (100 + (bp_ptr->lev * 4)))
		{
			bp_ptr->to_h += (bp_ptr->lev / 3);
			bp_ptr->to_d += (bp_ptr->lev / 3);
		}
		else
		{
			bp_ptr->blows /= 2;
		}

		bp_ptr->blows += extra_blows;

		/* Calculate best Monk Attacks */
		while (ma != 0)
		{
			ma_ptr = &ma_blows[ma];

			/* Can do this attack */
			if (bp_ptr->lev >= ma_ptr->min_level)
				break;

			/* Reduce the ma level and try again */
			ma--;
		}

	}

	/** Monk Armour **/

	/* Unencumbered Monks become faster every 10 levels */
	if (monk_arm_wgt < (100 + (bp_ptr->lev * 4)))
	{
		bp_ptr->speed += (bp_ptr->lev) / 10;

		if (!look_up_equip_slot(EQUIP_BODY))
		{
			bp_ptr->ac += (bp_ptr->lev * 3) / 2;
		}
		if (!look_up_equip_slot(EQUIP_OUTER) && (bp_ptr->lev > 15))
		{
			bp_ptr->ac += ((bp_ptr->lev - 13) / 3);
		}
		if (!look_up_equip_slot(EQUIP_ARM) && (bp_ptr->lev > 10))
		{
			bp_ptr->ac += ((bp_ptr->lev - 8) / 3);
		}
		if (!look_up_equip_slot(EQUIP_HEAD) && (bp_ptr->lev > 4))
		{
			bp_ptr->ac += (bp_ptr->lev - 2) / 3;
		}
		if (!look_up_equip_slot(EQUIP_HANDS))
		{
			bp_ptr->ac += (bp_ptr->lev / 2);
		}
		if (!look_up_equip_slot(EQUIP_FEET))
		{
			bp_ptr->ac += (bp_ptr->lev / 3);
		}
	}
}

/* Return the item in the equipment with the lowest ac */
int borg_notice_enchant_ac(void)
{
	int i, b_i = -1;
	int best = 15;
	list_item *l_ptr;

	/* Hack -- enchant all the equipment (armor) */
	for (i = EQUIP_BODY; i <= EQUIP_FEET; i++)
	{
		l_ptr = look_up_equip_slot(i);

		/* Skip empty items */
		if (!l_ptr) continue;

		/* Skip "unknown" items */
		if (!borg_obj_known_p(l_ptr)) continue;

		/* Skip items with ac higher then current best */
		if (l_ptr->to_a >= best) continue;

		/* Remember this one */
		b_i = i;
		best = l_ptr->to_a;
	}

	return (b_i);
}


/* Return the item with the lowest to_hit */
int borg_notice_enchant_hit(bool *inven)
{
	int i, ammo = -1, weapon = -1;
	int best_a = 15, best_w = 15;

	/* look through inventory for ammo */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Only enchant if qty >= 5 */
		if (l_ptr->number < 5) continue;

		/* Skip non-identified items  */
		if (!borg_obj_known_p(l_ptr)) continue;

		/* Make sure it is the right type if missile */
		if (l_ptr->tval != my_ammo_tval) continue;

		/* Find the least enchanted item */
		if (l_ptr->to_h >= best_a) continue;

		/* Save the info  */
		ammo = i;
		best_a = l_ptr->to_h;
	}

	/* Look for a weapon that needs enchanting */
	for (i = EQUIP_WIELD; i <= EQUIP_BOW; i++)
	{
		list_item *l_ptr = look_up_equip_slot(i);

		/* Skip empty slots */
		if (!l_ptr) continue;

		/* Skip non-identified items */
		if (!borg_obj_known_p(l_ptr)) continue;

		/* Find the least enchanted item */
		if (l_ptr->to_h >= best_w) continue;

		/* Save the info */
		weapon = i;
		best_w = l_ptr->to_h;
	}

	/* If the weapon is high and the ammo is low */
	if (best_w >= 10 && best_a < 10)
	{
		/* Return the ammo */
		*inven = TRUE;
		return (ammo);
	}

	/* Otherwise the weapon */
	*inven = FALSE;
	return (weapon);
}

/* Return the item with the lowest to_dam bonus */
int borg_notice_enchant_dam(bool *inven)
{
	int i, ammo = -1, weapon = -1;
	int best_a = 25, best_w = 25;

	/* look through inventory for ammo */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Only enchant if qty >= 5 */
		if (l_ptr->number < 5) continue;

		/* Skip non-identified items  */
		if (!borg_obj_known_p(l_ptr)) continue;

		/* Make sure it is the right type if missile */
		if (l_ptr->tval != my_ammo_tval) continue;

		/* Find the least enchanted item */
		if (l_ptr->to_d >= best_a) continue;

		/* Save the info  */
		ammo = i;
		best_a = l_ptr->to_d;
	}

	/* Look for a weapon that needs enchanting */
	for (i = EQUIP_WIELD; i <= EQUIP_BOW; i++)
	{
		list_item *l_ptr = look_up_equip_slot(i);

		/* Skip empty slots */
		if (!l_ptr) continue;

		/* Skip non-identified items */
		if (!borg_obj_known_p(l_ptr)) continue;

		/* Find the least enchanted item */
		if (l_ptr->to_d >= best_w) continue;

		/* Save the info */
		weapon = i;
		best_w = l_ptr->to_d;
	}

	/* If the weapon is high and the ammo is low */
	if (best_w >= 10 && best_a < 10)
	{
		/* Return the ammo */
		*inven = TRUE;
		return (ammo);
	}

	/* Otherwise the weapon */
	*inven = FALSE;
	return (weapon);
}


static s16b borg_notice_artify_item(list_item *l_ptr)
{
	s16b value = 0;

	list_item *q_ptr;

	/* Just making sure */
	if (!l_ptr) return (-1);

	/* There can be only one */
	if (l_ptr->number != 1) return (-1);

	/* It needs to be identified already */
	if (!borg_obj_known_p(l_ptr)) return (-1);

	/* No double features */
	if (borg_obj_is_ego_art(l_ptr)) return (-1);

	/* The values have a factor to compare bows with armours or weapons */
	switch (l_ptr->tval)
	{
		case TV_BOW:
		{
			/* Value of the multiplier */
			switch (k_info[l_ptr->k_idx].sval)
			{
				case SV_HEAVY_XBOW: value += 2;
				case SV_LIGHT_XBOW: value += 2;
				case SV_LONG_BOW:   value += 2;
				case SV_SHORT_BOW:
				case SV_SLING:		value += 4;
			}

			break;
		}

		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* Product of the hit dice */
			value = (l_ptr->dd * l_ptr->ds + 4) * 2 / 5;

			break;
		}

		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		{
			/* Armour count */
			value = l_ptr->ac;

			break;
		}

		/* Aim for a high base ac */
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			/* Armour count */
			value = (l_ptr->ac + 4) / 5;

			break;
		}

		default: return (-1);
	}

	/* Check out the future slot */
	q_ptr = &equipment[borg_wield_slot(l_ptr)];

	if (q_ptr)
	{
		/* Boost the value if the current slot is taken by a non-artifact */
		if (!KN_FLAG(q_ptr, TR_INSTA_ART)) value *= 10;

		/* Boost the value if the current slot is taken by a non-ego */
		if (!borg_obj_is_ego_art(q_ptr)) value *=10;
	}

	/* Tell the result */
	return (value);
}


/* Report which item should be artified */
int borg_notice_create_artifact(bool *b_inven)
{
	bool inven;
	int i, slot = -1;
	int v = 0, b_v = 0;

	list_item *l_ptr;

	/* Initialize */
	*b_inven = FALSE;

	/* Does the borg have the scroll? */
	if (!borg_read_scroll_fail(SV_SCROLL_ARTIFACT)) return (-1);

	/* don't bother if the level is too low */
	if (bp_ptr->lev < 15) return (-1);

	/* Do it at town level in order to check shops first */
	if (bp_ptr->depth) return (-1);

	/* Check the available items */
	for (i = 0; i < equip_num + inven_num; i++)
	{
		/* Set flag */
		inven = i >= equip_num;

		/* Get an item */
		if (inven)
			l_ptr = &inventory[i - equip_num];
		else
			l_ptr = look_up_equip_slot(i);

		/* Is it really an item? */
		if (!l_ptr) continue;

		/* Now assign value */
		v = borg_notice_artify_item(l_ptr);

		/* Is it better than before? */
		if (v <= b_v) continue;

		/* Remember the best item */
		*b_inven = inven;
		b_v = v;

		/* Which slot is that? */
		if (inven)
			slot = i - equip_num;
		else
			slot = i;
	}

	/* Report whatever */
	return (slot);
}


/*
 * Notice changes in lighting
 */
static void borg_notice_lite(void)
{
	list_item *l_ptr;

	/* Assume normal lite radius */
	bp_ptr->cur_lite = 0;

	/* Glowing player has light */
	if (bp_ptr->britelite) bp_ptr->cur_lite = 1;

	/* Examine the lite */
	l_ptr = look_up_equip_slot(EQUIP_LITE);

	/* Item missing? */
	if (!l_ptr) return;

	/* Lite */
	if (l_ptr->tval == TV_LITE)
	{
		object_kind *k_ptr = &k_info[l_ptr->k_idx];

		/* If it is a torch with fuel or everburning */
		if ((k_ptr->sval == SV_LITE_TORCH) &&
			(l_ptr->timeout ||
			KN_FLAG(l_ptr, TR_LITE)))

		{
			/* Torches -- radius one */
			bp_ptr->cur_lite += 1;
		}
		
		/* If it is a Lantern */
		if (k_ptr->sval == SV_LITE_LANTERN)
		{
			/* And it has fuel */
			if (l_ptr->timeout)
			{
				/* Lanterns -- radius two */
				bp_ptr->cur_lite += 2;
			}
			/* No fuel */
			else
			{
				/* Is it everburning? */
				if (KN_FLAG(l_ptr, TR_LITE))
				{
					/* Unfueled Lantern of Everburning still has radius 1 */
					bp_ptr->cur_lite += 1;
				}
			}
		}
		
		if (KN_FLAG(l_ptr, TR_LITE))
		{
			/* Permanently glowing */
			bp_ptr->britelite = TRUE;
			
			/* A Lantern of Everburning needs fuel only when it is empty. */
			if (k_ptr->sval != SV_LITE_LANTERN || l_ptr->timeout)
			{
				/* No need for fuel */
				bp_ptr->able.fuel += 1000;
				amt_lantern = 1000;
				amt_flask = 1000;
			}
		}
		
		/* Artifact lites -- radius three */
		if (KN_FLAG(l_ptr, TR_INSTA_ART))
		{
			bp_ptr->cur_lite += 3;
			
			/* Permanently glowing */
			bp_ptr->britelite = TRUE;
			
			/* No need for fuel */
			bp_ptr->able.fuel += 1000;

			/* Vampires need to be concerned with Artifacts Lites */
			if (FLAG(bp_ptr, TR_HURT_LITE) && !FLAG(bp_ptr, TR_RES_LITE))
			{
				bp_ptr->cur_lite = 1;
			}
		}
	}
}


/*
 * Helper function -- notice the player equipment
 */
static void borg_notice_aux1(void)
{
	int i, hold;

	int extra_blows = 0;

	int extra_shots = 0;
	int extra_might = 0;

	/* Notice player flags */
	borg_notice_player();

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) my_stat_add[i] = 0;

	/* Notice equipment */
	borg_notice_equip(&extra_blows, &extra_shots, &extra_might);

	/* Recalculate the stats */
	borg_notice_stats();

	/* Obtain the "hold" value */
	hold = adj_str_hold[my_stat_ind[A_STR]];

	/* Examine ranged weapon */
	borg_notice_shooter(hold, extra_might, extra_shots);

	/* Examine melee weapon */
	borg_notice_weapon(hold, extra_blows);

	/* Recalculate skills */
	borg_notice_skills();

	/* Monks get bonus for not using weapon or armour */
	if (borg_class == CLASS_MONK)
	{
		borg_recalc_monk(extra_blows);
	}

	/* Examine lite */
	borg_notice_lite();
}


/*
 * Notice food
 */
static void borg_notice_food(list_item *l_ptr, int number)
{
	int sval = k_info[l_ptr->k_idx].sval;

	/* Analyze */
	switch (sval)
	{
		case SV_FOOD_WAYBREAD:
		{
			bp_ptr->food += number;
			bp_ptr->able.cure_pois += number;
			break;
		}
		case SV_FOOD_RATION:
		{
			bp_ptr->food += number;
			break;
		}
		case SV_FOOD_JERKY:
		{
			amt_food_lowcal += number;
			break;
		}
		case SV_FOOD_BISCUIT:
		{
			amt_food_lowcal += number;
			break;
		}
		case SV_FOOD_SLIME_MOLD:
		{
			amt_food_lowcal += number;
			break;
		}

		case SV_FOOD_RESTORE_STR:
		{
			amt_fix_stat[A_STR] += number;
			break;
		}
		case SV_FOOD_RESTORE_CON:
		{
			amt_fix_stat[A_CON] += number;
			break;
		}
		case SV_FOOD_RESTORING:
		{
			amt_fix_stat[A_STR] += number;
			amt_fix_stat[A_INT] += number;
			amt_fix_stat[A_WIS] += number;
			amt_fix_stat[A_DEX] += number;
			amt_fix_stat[A_CON] += number;
			amt_fix_stat[A_CHR] += number;
			amt_fix_stat[6] += number;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			bp_ptr->able.cure_conf += number;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			bp_ptr->able.cure_blind += number;
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			bp_ptr->able.cure_pois += number;
			break;
		}
	}
}


/*
 * Notice Potions
 */
static void borg_notice_potions(list_item *l_ptr, int number)
{
	int sval = k_info[l_ptr->k_idx].sval;

	/* Analyze */
	switch (sval)
	{
		case SV_POTION_HEALING:
		{
			bp_ptr->able.heal += number;
			break;
		}
		case SV_POTION_STAR_HEALING:
		{
			amt_star_heal += number;
			break;
		}
		case SV_POTION_LIFE:
		{
			amt_life += number;
			break;
		}
		case SV_POTION_CURING:
		{
			amt_pot_curing += number;

			/* Fall through */
		}
		case SV_POTION_CURE_CRITICAL:
		{
			bp_ptr->able.ccw += number;
			bp_ptr->able.cure_pois += number;
			bp_ptr->able.cure_blind += number;
			bp_ptr->able.cure_conf += number;
			break;
		}
		case SV_POTION_CURE_SERIOUS:
		{
			bp_ptr->able.csw += number;
			bp_ptr->able.cure_conf += number;
			bp_ptr->able.cure_blind += number;
			break;
		}
		case SV_POTION_CURE_LIGHT:
		{
			bp_ptr->able.clw += number;
			bp_ptr->able.cure_blind += number;
			break;
		}
		case SV_POTION_CURE_POISON:
		{
			bp_ptr->able.cure_pois += number;
			break;
		}
		case SV_POTION_SLOW_POISON:
		{
			amt_slow_poison += number;
			break;
		}
		case SV_POTION_RESIST_HEAT:
		{
			bp_ptr->able.res_heat += number;
			break;
		}
		case SV_POTION_RESIST_COLD:
		{
			bp_ptr->able.res_cold += number;
			break;
		}
		case SV_POTION_RESISTANCE:
		{
			bp_ptr->able.res_all += number;
			bp_ptr->able.res_cold += number;
			bp_ptr->able.res_heat += number;
			break;
		}
		case SV_POTION_INC_STR:
		{
			amt_add_stat[A_STR] += number;
			break;
		}
		case SV_POTION_INC_INT:
		{
			amt_add_stat[A_INT] += number;
			break;
		}
		case SV_POTION_INC_WIS:
		{
			amt_add_stat[A_WIS] += number;
			break;
		}
		case SV_POTION_INC_DEX:
		{
			amt_add_stat[A_DEX] += number;
			break;
		}
		case SV_POTION_INC_CON:
		{
			amt_add_stat[A_CON] += number;
			break;
		}
		case SV_POTION_INC_CHR:
		{
			amt_add_stat[A_CHR] += number;
			break;
		}
		case SV_POTION_RES_STR:
		{
			amt_fix_stat[A_STR] += number;
			break;
		}
		case SV_POTION_RES_INT:
		{
			amt_fix_stat[A_INT] += number;
			break;
		}
		case SV_POTION_RES_WIS:
		{
			amt_fix_stat[A_WIS] += number;
			break;
		}
		case SV_POTION_RES_DEX:
		{
			amt_fix_stat[A_DEX] += number;
			break;
		}
		case SV_POTION_RES_CON:
		{
			amt_fix_stat[A_CON] += number;
			break;
		}
		case SV_POTION_RES_CHR:
		{
			amt_fix_stat[A_CHR] += number;
			break;
		}
		case SV_POTION_RESTORE_EXP:
		{
			amt_fix_exp += number;
			break;
		}
		case SV_POTION_SPEED:
		{
			bp_ptr->able.speed += number;
			break;
		}
		case SV_POTION_BERSERK_STRENGTH:
		{
			bp_ptr->able.berserk += number;
			break;
		}
		case SV_POTION_POISON:
		{
			bp_ptr->able.poison += number;
			break;
		}
		case SV_POTION_RESTORE_MANA:
		{
			if (borg_class == CLASS_WARRIOR)
				bp_ptr->able.death += number;
			else
				bp_ptr->able.mana += number;

			break;
		}
		case SV_POTION_DETONATIONS:
		case SV_POTION_DEATH:
		case SV_POTION_RUINATION:
		{
			bp_ptr->able.death += number;
			break;
		}
		case SV_POTION_INVULNERABILITY:
		{
			bp_ptr->able.invulnerability += number;
			break;
		}
	}
}


/*
 * Notice scrolls
 */
static void borg_notice_scrolls(list_item *l_ptr, int number)
{
	int sval = k_info[l_ptr->k_idx].sval;

	/* Analyze the scroll */
	switch (sval)
	{
		case SV_SCROLL_IDENTIFY:
		{
			bp_ptr->able.id += number;
			break;
		}
		case SV_SCROLL_STAR_IDENTIFY:
		{
			bp_ptr->able.star_id += number;
			break;
		}
		case SV_SCROLL_REMOVE_CURSE:
		{
			bp_ptr->able.remove_curse += number;
			break;
		}
		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			bp_ptr->able.star_remove_curse += number;
			break;
		}
		case SV_SCROLL_RECHARGING:
		{
			bp_ptr->able.recharge += number;
			break;
		}
		case SV_SCROLL_PHASE_DOOR:
		{
			bp_ptr->able.phase += number;
			break;
		}
		case SV_SCROLL_TELEPORT:
		{
			bp_ptr->able.escape += number;
			bp_ptr->able.teleport += number;
			break;
		}
		case SV_SCROLL_WORD_OF_RECALL:
		{
			bp_ptr->recall += number;
			break;
		}
		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		case SV_SCROLL_ENCHANT_ARMOR:
		{
			/* Get the best item to enchant */
			int slot = borg_notice_enchant_ac();

			/* If there is an item with low ac or the borg has loads of cash */
			if (slot != -1 &&
				(equipment[slot].to_a < 10 || borg_gold > 10000))
			{
				/* count the scroll */
				amt_enchant_to_a += number;
			}
			break;
		}
		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			int hit, slot;
			bool inven;

			/* Get the best item to enchant */
			slot = borg_notice_enchant_hit(&inven);

			/* If there is no item */
			if (slot != -1)
			{
				/* find out the to_hit value */
				hit = (inven) ? inventory[slot].to_h : equipment[slot].to_h;

				/* If the item has low to_hit or the borg is rich */
				if (hit < 10 || borg_gold > 10000)
				{
					/* Count the scroll */
					amt_enchant_to_h += number;
				}
			}

			/* Fall through for a scroll of *enchant weapon* */
			if (sval == SV_SCROLL_ENCHANT_WEAPON_TO_HIT) break;
		}
		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			int dam, slot;
			bool inven;

			/* Get the best item to enchant */
			slot = borg_notice_enchant_dam(&inven);

			/* If there is no item */
			if (slot != -1)
			{
				/* find out the to_dam value */
				dam = (inven) ? inventory[slot].to_d : equipment[slot].to_d;

				/* If the item has low to_dam or the borg is loaded */
				if (dam < 10 || borg_gold > 10000)
				{
					/* Count the scroll */
					amt_enchant_to_d += number;
				}
			}

			break;
		}
		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			bp_ptr->able.glyph += number;
			break;
		}
		case SV_SCROLL_TELEPORT_LEVEL:
		{
			bp_ptr->able.teleport_level += number;
			break;
		}
		case SV_SCROLL_SATISFY_HUNGER:
		{
			amt_food_scroll += number;
			bp_ptr->food += number;
			break;
		}
		case SV_SCROLL_ICE:
		{
			if (FLAG(bp_ptr, TR_RES_COLD)) bp_ptr->able.logrus += number;
			break;
		}
		case SV_SCROLL_FIRE:
		{
			if (FLAG(bp_ptr, TR_RES_FIRE)) bp_ptr->able.logrus += number;
			break;
		}
		case SV_SCROLL_CHAOS:
		{
			if (FLAG(bp_ptr, TR_RES_CHAOS)) bp_ptr->able.logrus += number;
			break;
		}
		case SV_SCROLL_DISPEL_UNDEAD:
		{
			bp_ptr->able.logrus += number;
			break;
		}
		case SV_SCROLL_LIGHT:
		{
			bp_ptr->able.lite += number;
			break;
		}
		case SV_SCROLL_GENOCIDE:
		{
			bp_ptr->able.genocide += number;
			break;
		}
		case SV_SCROLL_MASS_GENOCIDE:
		{
			bp_ptr->able.mass_genocide += number;
			break;
		}
		case SV_SCROLL_ARTIFACT:
		{
			bp_ptr->able.artifact += number;
			break;
		}
		case SV_SCROLL_STAR_ACQUIREMENT:
		case SV_SCROLL_ACQUIREMENT:
		{
			bp_ptr->able.acquire += number;
			break;
		}
		case SV_SCROLL_MUNDANITY:
		{
			int i;
			list_item *l_ptr;

			/* Check the equipment */
			for (i = 0; i < equip_num; i++)
			{
				l_ptr = look_up_equip_slot(i);

				/* No empty slots */
				if (!l_ptr) continue;

				/* If there is a nasty curse count the mundanity scroll */
				if (borg_test_bad_curse(l_ptr)) bp_ptr->able.mundane += number;

			break;
			}
		}
	}
}


/*
 * Notice rods
 */
static void borg_notice_rods(list_item *l_ptr, int number)
{
	object_kind *k_ptr = &k_info[l_ptr->k_idx];
	int sval = k_ptr->sval;

	/* Analyze */
	switch (sval)
	{
		case SV_ROD_IDENTIFY:
		{
			if (borg_use_item_fail(l_ptr, TRUE))
			{
				bp_ptr->able.id += number * 100;
			}
			else
			{
				bp_ptr->able.id += number;
			}
			break;
		}

		case SV_ROD_RECALL:
		{
			/* Don't count on it if I suck at activations */
			if (borg_use_item_fail(l_ptr, FALSE))
			{
				bp_ptr->recall += number * 100;
			}
			break;
		}

		case SV_ROD_DETECT_TRAP:
		{
			bp_ptr->able.det_trap += number * 100;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			bp_ptr->able.det_door += number * 100;
			break;
		}

		case SV_ROD_DETECTION:
		{
			bp_ptr->able.det_trap += number * 100;
			bp_ptr->able.det_door += number * 100;
			bp_ptr->able.det_evil += number * 100;
			break;
		}

		case SV_ROD_SPEED:
		{
			/* Don't count on it if I suck at activations */
			if (borg_use_item_fail(l_ptr, FALSE))
			{
				bp_ptr->able.speed += number * 100;
			}
			else
			{
				bp_ptr->able.speed += number;
			}
			break;
		}

		case SV_ROD_MAPPING:
		{
			bp_ptr->able.magic_map += number * 100;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			bp_ptr->able.lite += number * 100;
			break;
		}

		case SV_ROD_HEALING:
		{
			/* Don't count on it if I suck at activations */
			if (borg_use_item_fail(l_ptr, FALSE))
			{
				bp_ptr->able.heal += number;
				amt_rod_heal += number;
			}
			break;
		}

		case SV_ROD_CURING:
		{
			/* Don't count on it if I suck at activations */
			if (borg_use_item_fail(l_ptr, FALSE))
			{
				bp_ptr->able.ccw += number;
				bp_ptr->able.cure_pois += number;
				bp_ptr->able.cure_blind += number;
				bp_ptr->able.cure_conf += number;
			}
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			bp_ptr->able.teleport_away += number * 100;
			break;
		}

		case SV_ROD_PESTICIDE:
		{
			/* Only for small borgs */
			if (bp_ptr->lev > 25) break;

			/* Fall through */
		}
		case SV_ROD_FIRE_BALL:
		case SV_ROD_ACID_BALL:
		case SV_ROD_ELEC_BALL:
		case SV_ROD_COLD_BALL:
		case SV_ROD_HAVOC:
		{
			bp_ptr->able.ball += 5 * number;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		case SV_ROD_ACID_BOLT:
		case SV_ROD_ELEC_BOLT:
		case SV_ROD_COLD_BOLT:
		case SV_ROD_DRAIN_LIFE:
		case SV_ROD_LITE:
		{
			bp_ptr->able.bolt += 5 * number;
			break;
		}
	}
}


/*
 * Notice wands.  Separates the wands in ball or bolt wands.
 * There is a wand feature that makes it hard to count charges:
 * If you have a Wand of Foo (4 charges) and a Wand of Foo (8 charges)
 * this is shown as 2 Wands of Foo (12 charges).  If you sell 1 in a shop
 * you sell the Wand of Foo (4 charges).  But the borg can only guess that
 * it is selling 6 charges.  I don't think this will cause a loop.
 */
static void borg_notice_wands(list_item *l_ptr, int number)
{
	int sval = k_info[l_ptr->k_idx].sval;
	int pval = 0, non_empty = 0;


	/* Is this is a pile a wands with unknown charges? */
	if (!borg_obj_known_p(l_ptr) && !strstr(l_ptr->o_name, "{empty}"))
	{
		/* Set the number of wands, later we guess how many charges there are */
		non_empty = number;
	}
	/* This pile of wands has known pval or is empty */
	else
	{
		/* Counting this wand while getting it from a shop or home */
		if (l_ptr->treat_as == TREAT_AS_SHOP)
		{
			/* We get 1 wand and not all the charges */
			pval = l_ptr->pval / l_ptr->number;
		}
		/* Counting this pile while selling one */
		else if (l_ptr->treat_as == TREAT_AS_LESS)
		{
			/* We get all the charges except for the charges of one wand */
			pval = l_ptr->pval - l_ptr->pval / l_ptr->number;
		}
		/* Just count the stack will you */
		else
		{
			/* All the charges */
			pval = l_ptr->pval;
		}
	}

	/* What sort of wand is this? */
	switch (sval)
	{
		case SV_WAND_TELEPORT_AWAY:
		{
			/* count the charges */
			bp_ptr->able.teleport_away += pval + 5 * non_empty;

			break;
		}

		/* Ball Wands */
		case SV_WAND_ACID_BALL:
		case SV_WAND_ELEC_BALL:
		case SV_WAND_FIRE_BALL:
		case SV_WAND_COLD_BALL:
		case SV_WAND_ANNIHILATION:
		case SV_WAND_DRAGON_FIRE:
		case SV_WAND_DRAGON_COLD:
		case SV_WAND_DRAGON_BREATH:
		case SV_WAND_ROCKETS:
		{
			/* count the charges */
			bp_ptr->able.ball += pval + 5 * non_empty;

			break;
		}

		/* Bolt wands */
		case SV_WAND_LITE:
		case SV_WAND_DRAIN_LIFE:
		case SV_WAND_STINKING_CLOUD:
		case SV_WAND_MAGIC_MISSILE:
		case SV_WAND_ACID_BOLT:
		case SV_WAND_FIRE_BOLT:
		case SV_WAND_COLD_BOLT:
		{
			/* count the charges */
			bp_ptr->able.bolt += pval + 2 * non_empty;

			break;
		}

		/* Don't bother with keeping the rest of the wands */
		default:
		{
			/* Nothing */
			break;
		}
	}
}


/*
 * Notice staves
 */
static void borg_notice_staves(list_item *l_ptr, int number)
{
	int sval = k_info[l_ptr->k_idx].sval;

	/*
	 * Staves should not be carried to The Serpent, he drains
	 * them to heal himself- not good at all
	 */
	if ((bp_ptr->max_depth >= 99) && !bp_ptr->winner)
	{
		/* skip these */
		return;
	}

	/* Analyze */
	switch (sval)
	{
		case SV_STAFF_IDENTIFY:
		{
			bp_ptr->able.id += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_CURE_LIGHT:
		{
			bp_ptr->able.clw += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_CURING:
		{
			bp_ptr->able.ccw += number * l_ptr->pval;
			bp_ptr->able.cure_pois += number;
			bp_ptr->able.cure_blind += number;
			bp_ptr->able.cure_conf += number;
			break;
		}
		case SV_STAFF_TELEPORTATION:
		{
			bp_ptr->able.teleport += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_SPEED:
		{
			bp_ptr->able.speed += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_HEALING:
		{
			bp_ptr->able.heal += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_REMOVE_CURSE:
		{
			bp_ptr->able.remove_curse += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_DESTRUCTION:
		{
			bp_ptr->able.staff_dest += number * l_ptr->pval;

			/* Add a token charge to keep the staff */
			if (!bp_ptr->able.staff_dest) bp_ptr->able.staff_dest = 1;

			break;
		}
		case SV_STAFF_THE_MAGI:
		{
			bp_ptr->able.staff_magi += number * l_ptr->pval;
			break;
		}
		case SV_STAFF_POWER:
		{
			bp_ptr->able.staff_cool += number * l_ptr->pval;

			/* Add a token charge to keep the staff */
			if (!bp_ptr->able.staff_cool) bp_ptr->able.staff_cool = 1;

			break;
		}
		case SV_STAFF_HOLINESS:
		{
			bp_ptr->able.staff_cool += number * l_ptr->pval;
			bp_ptr->able.heal += number * l_ptr->pval;

			/* Add a token charge to keep the staff */
			if (!bp_ptr->able.staff_cool) bp_ptr->able.staff_cool = 1;

			break;
		}
		case SV_STAFF_LITE:
		{
			bp_ptr->able.lite += number * l_ptr->pval;
			break;
		}
	}
}


/*
 * Examine an item in the inventory
 */
static void borg_notice_inven_item(list_item *l_ptr)
{
	int number;

	object_kind *k_ptr;

	if (l_ptr->treat_as == TREAT_AS_LESS)
	{
		/* Pretend we have less items */
		number = l_ptr->number - 1;
	}
	else
	{
		/* Is this a home or shop item? */
		if (l_ptr->treat_as == TREAT_AS_SHOP)
		{
			/* You can buy only one item from a shop */
			number = 1;
		}
		else
		{
			/* Count the whole pile */
			number = l_ptr->number;
		}
	}

	/* Does this item need id or remove curse? */
	borg_notice_improve_item(l_ptr, FALSE);
	
	/* Keep track of weight */
	bp_ptr->weight += l_ptr->weight * number;

	/* Get item type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Keep track of (base) value */
	bp_ptr->value += k_ptr->cost * number;

	/* Analyze the item */
	switch (l_ptr->tval)
	{
		case TV_LIFE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_LIFE))
				amt_book[REALM_LIFE][k_ptr->sval] += number;
			break;
		}
		case TV_SORCERY_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_SORCERY))
				amt_book[REALM_SORCERY][k_ptr->sval] += number;
			break;
		}
		case TV_NATURE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_NATURE))
				amt_book[REALM_NATURE][k_ptr->sval] += number;
			break;
		}
		case TV_CHAOS_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_CHAOS))
				amt_book[REALM_CHAOS][k_ptr->sval] += number;
			break;
		}
		case TV_DEATH_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_DEATH))
				amt_book[REALM_DEATH][k_ptr->sval] += number;
			break;
		}
		case TV_TRUMP_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_TRUMP))
				amt_book[REALM_TRUMP][k_ptr->sval] += number;
			break;
		}
		case TV_ARCANE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_ARCANE))
				amt_book[REALM_ARCANE][k_ptr->sval] += number;
			break;
		}

		case TV_FOOD:
		{
			/* Food */
			borg_notice_food(l_ptr, number);
			break;
		}

		case TV_POTION:
		{
			/* Potions */
			borg_notice_potions(l_ptr, number);
			break;
		}

		case TV_SCROLL:
		{
			/* Scrolls */
			borg_notice_scrolls(l_ptr, number);
			break;
		}

		case TV_ROD:
		{
			/* Rods */
			borg_notice_rods(l_ptr, number);
			break;
		}

		case TV_WAND:
		{
			/* Wands */
			borg_notice_wands(l_ptr, number);
			break;
		}

		case TV_STAFF:
		{
			/* Staffs */
			borg_notice_staves(l_ptr, number);
			break;
		}


		case TV_FLASK:
		{
			bp_ptr->able.fuel += number;
			amt_flask += number;

			break;
		}

		case TV_LITE:
		{
			/* If not empty */
			if (l_ptr->timeout)
			{
				/* Count whatever it is as 1 fuel */
				bp_ptr->able.fuel += number;

				/* Count a lantern as lantern fuel */
				if (k_ptr->sval == SV_LITE_LANTERN) amt_lantern += number;
				
				/* Count a torch as torch fuel */
				if (k_ptr->sval == SV_LITE_TORCH) amt_torch += number;
			}
			
			break;
		}

		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			/* Can the borg make a better artifact of this item? */
			bp_ptr->able.artify_item = MAX(bp_ptr->able.artify_item,
										   borg_notice_artify_item(l_ptr));

			break;
		}

		case TV_DIGGING:
		{
			/* Shovels and such */

			/* Hack -- ignore worthless ones (including cursed) */
			if (KN_FLAG(l_ptr, TR_CURSED)) break;

			/* Do not carry if weak, won't be able to dig anyway */
			if (bp_ptr->skill_dig < 30) break;

			amt_digger += number;
			break;
		}

		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Missiles */

			/* Hack -- ignore invalid missiles */
			if (l_ptr->tval != my_ammo_tval) break;

			/* Ignore bad missiles */
			if (l_ptr->to_h < -5) break;

			/* Count them */
			bp_ptr->able.missile += number;

			break;
		}
	}
}


/*
 * Notice the inventory
 */
static void borg_notice_inven(void)
{
	list_item *l_ptr;

	int i;

	/* Scan the inventory */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Pretend item isn't there */
		if (l_ptr->treat_as == TREAT_AS_GONE) continue;
		if (l_ptr->treat_as == TREAT_AS_SWAP) continue;
		if ((l_ptr->treat_as == TREAT_AS_LESS) && (l_ptr->number == 1))
		{
			continue;
		}

		/* Unaware item? */
		if (!l_ptr->k_idx) continue;

		/* Examine the item */
		borg_notice_inven_item(l_ptr);
	}

	/* Search equipment for swapped items */
	for (i = 0; i < equip_num; i++)
	{
		/* Don't use look_up_equip_slot here! */
		l_ptr = &equipment[i];

		/* A known item? */
		if (l_ptr->k_idx)
		{
			if ((l_ptr->treat_as == TREAT_AS_SWAP) ||
				(l_ptr->treat_as == TREAT_AS_GONE))
			{
				/* Examine the item */
				borg_notice_inven_item(l_ptr);

				/* Done (Only one extra item) */
				return;
			}
		}
	}

	/* Scan home for swapped items */
	for (i = 0; i < home_num; i++)
	{
		l_ptr = &borg_home[i];

		/* Hack - only 'LESS' items are treated as going into inven */
		if (l_ptr->treat_as == TREAT_AS_LESS)
		{
			/* Hack - fix the treat_as value */
			l_ptr->treat_as = TREAT_AS_SHOP;

			/* Examine the item */
			borg_notice_inven_item(l_ptr);

			/* Restore treat_as value */
			l_ptr->treat_as = TREAT_AS_LESS;

			/* Done (Only one extra item) */
			return;
		}

		/* Sometimes want to grab a whole pile */
		if (l_ptr->treat_as == TREAT_AS_SWAP)
		{
			/* Examine the item */
			borg_notice_inven_item(l_ptr);

			/* Done (Only one extra item) */
			return;
		}
	}

	/* Scan current shop? */
	if (use_shop)
	{
		for (i = 0; i < cur_num; i++)
		{
			l_ptr = &cur_list[i];

			/* A known item? */
			if (l_ptr->k_idx)
			{
				/* Hack - only 'LESS' items are treated as going into inven */
				if (l_ptr->treat_as == TREAT_AS_LESS)
				{
					/* Hack - fix the treat_as value */
					l_ptr->treat_as = TREAT_AS_SHOP;

					/* Examine the item */
					borg_notice_inven_item(l_ptr);

					/* Restore treat_as value */
					l_ptr->treat_as = TREAT_AS_LESS;

					/* Done (Only one extra item) */
					return;
				}
			}
		}
	}
}


/*
 * Helper function -- notice the player inventory
 */
static void borg_notice_aux2(void)
{
	int carry_capacity;

	/*** Process the inventory ***/
	borg_notice_inven();


	/*
	 *Process the Spells and Prayers and Artifact Activations.
	 *
	 *  Some artifacts are treated differently because:
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
	 */

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_SATISFY))
	{
		amt_food_scroll += 5;
		bp_ptr->food += 5;
	}

	/* Handle "satisfy hunger" -> infinite food */
	if (borg_spell_legal_fail(REALM_LIFE, 0, 7, 40) ||
		borg_spell_legal_fail(REALM_ARCANE, 2, 6, 40) ||
		borg_spell_legal_fail(REALM_NATURE, 0, 3, 40) ||
		borg_racial_check(RACE_HOBBIT, TRUE))
	{
		amt_food_scroll += 1000;
		bp_ptr->food += 1000;
	}

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_SATISFY)) bp_ptr->able.id += 5;

	/* Handle "identify" -> infinite identifies */
	if (borg_activate_fail(BORG_ACT_IDENTIFY) ||
		borg_spell_legal_fail(REALM_SORCERY, 1, 1, 60) ||
		borg_spell_legal_fail(REALM_ARCANE, 3, 2, 60) ||
		borg_mindcr_legal_fail(MIND_PSYCHOMETRY, 25, 60))
	{
		bp_ptr->able.id += 1000;
	}

	/* Handle "*identify*" -> infinite *identifies* */
	if (borg_activate_fail(BORG_ACT_STAR_IDENTIFY) ||
		borg_spell_legal_fail(REALM_SORCERY, 1, 7, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 2, 5, 60) ||
		borg_spell_legal_fail(REALM_DEATH, 3, 2, 60) ||
		borg_spell_legal_fail(REALM_TRUMP, 3, 1, 60) ||
		borg_spell_legal_fail(REALM_LIFE, 3, 5, 60))
	{
		bp_ptr->able.id += 1000;
		bp_ptr->able.star_id += 1000;
	}

	/* Handle "remove_curse" -> infinite remove curses */
	if (borg_activate_fail(BORG_ACT_REMOVE_CURSE) ||
		borg_spell_legal_fail(REALM_LIFE, 1, 0, 60))
	{
		bp_ptr->able.remove_curse += 1000;
	}

	/* Handle "*remove_curse*" -> infinite *remove curses* */
	if (borg_activate_fail(BORG_ACT_STAR_REMOVE_CURSE) ||
		borg_spell_legal_fail(REALM_LIFE, 2, 1, 60))
	{
		bp_ptr->able.remove_curse += 1000;
		bp_ptr->able.star_remove_curse += 1000;
	}

	/* Handle "detect traps, doors, stairs" */
	if (borg_activate_fail(BORG_ACT_DETECT_TRAP_DOOR) ||
		borg_spell_legal_fail(REALM_LIFE, 0, 5, 60) ||
		borg_spell_legal_fail(REALM_SORCERY, 0, 2, 60) ||
		borg_spell_legal_fail(REALM_ARCANE, 1, 0, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 1, 2, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 0, 2, 60) ||
		borg_mindcr_legal_fail(MIND_PRECOGNIT, 5, 60) ||
		borg_racial_check(RACE_DWARF, TRUE) ||
		borg_racial_check(RACE_NIBELUNG, TRUE))
	{
		bp_ptr->able.det_trap += 1000;
		bp_ptr->able.det_door += 1000;
	}

	/* Handle "detect evil & monsters" */
	if (borg_activate_fail(BORG_ACT_DETECT_MONSTERS) ||
		borg_activate_fail(BORG_ACT_DETECT_EVIL) ||
		borg_racial_check(RACE_GHOUL_POWER2, TRUE) ||
		borg_spell_legal_fail(REALM_LIFE, 0, 0, 60) ||
		borg_spell_legal_fail(REALM_SORCERY, 0, 0, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 0, 0, 60) ||
		borg_spell_legal_fail(REALM_DEATH, 0, 0, 60) ||
		borg_spell_legal_fail(REALM_DEATH, 0, 2, 60) ||
		borg_mindcr_legal_fail(MIND_PRECOGNIT, 1, 60))
	{
		bp_ptr->able.det_evil += 1000;
	}

	/* Handle "detection" */
	if (borg_activate_fail(BORG_ACT_DETECTION) ||
		borg_mindcr_legal_fail(MIND_PRECOGNIT, 30, 60))
	{
		bp_ptr->able.det_door += 1000;
		bp_ptr->able.det_trap += 1000;
		bp_ptr->able.det_evil += 1000;
	}

	/* Handle "magic mapping" */
	if (borg_activate_fail(BORG_ACT_MAGIC_MAPPING) ||
		borg_spell_legal_fail(REALM_SORCERY, 1, 0, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 1, 2, 60) ||
		borg_mindcr_legal_fail(MIND_PRECOGNIT, 20, 60))
	{
		bp_ptr->able.magic_map += 1000;
	}

	/* Handle "light" */
	if (borg_activate_fail(BORG_ACT_LIGHT) ||
		borg_spell_legal_fail(REALM_LIFE, 0, 4, 60) ||
		borg_spell_legal_fail(REALM_SORCERY, 0, 3, 60) ||
		borg_spell_legal_fail(REALM_NATURE, 0, 4, 60) ||
		borg_spell_legal_fail(REALM_CHAOS, 0, 2, 60) ||
		borg_spell_legal_fail(REALM_ARCANE, 0, 5, 60) ||
		borg_mutation_check(MUT1_ILLUMINE, TRUE))
	{
		bp_ptr->able.lite += 1000;
	}

	/* Handle "phlogiston" */
	if (borg_spell_legal_fail(REALM_ARCANE, 1, 1, 40))
	{
		/* Not too much or you'll be casting phlogiston in the dark */
		bp_ptr->able.fuel += 5;
		amt_lantern += 5;
		amt_torch += 5;
	}

	/* Handle "rune of protection" glyph" */
	if (borg_spell_legal_fail(REALM_LIFE, 1, 7, 20) ||
		borg_spell_legal_fail(REALM_LIFE, 2, 7, 20))
	{
		bp_ptr->able.glyph += 1000;
	}

	/* Handle "enchant weapon" */
	if (borg_spell_legal_fail(REALM_SORCERY, 3, 4, 40))
	{
		amt_enchant_to_h += 1000;
		amt_enchant_to_d += 1000;
	}

	/* Handle "enchant armor" */
	if (borg_spell_legal_fail(REALM_SORCERY, 3, 5, 40))
	{
		amt_enchant_to_a += 1000;
	}

	/* Handle Diggers */
	if (borg_activate_fail(BORG_ACT_STONE_TO_MUD) ||
		borg_spell_legal_fail(REALM_NATURE, 1, 0, 40) ||
		borg_spell_legal_fail(REALM_CHAOS, 0, 6, 40) ||
		borg_mutation_check(MUT1_EAT_ROCK, TRUE) ||
		borg_racial_check(RACE_HALF_GIANT, TRUE))
	{
		amt_digger += 1;
	}

	/* Not too many, this artifact may dissappear */
	if (borg_activate_fail(BORG_ACT_WORD_OF_RECALL)) bp_ptr->recall += 3;

	/* Handle recall */
	if (borg_spell_legal_fail(REALM_ARCANE, 3, 6, 40) ||
		borg_spell_legal_fail(REALM_SORCERY, 2, 7, 40) ||
		borg_spell_legal_fail(REALM_TRUMP, 1, 6, 40) ||
		borg_mutation_check(MUT1_RECALL, TRUE))
	{
		bp_ptr->recall += 1000;
	}

	/* Handle teleport_level */
	if (borg_activate_fail(BORG_ACT_TELEPORT_LEVEL) ||
		borg_spell_legal_fail(REALM_SORCERY, 2, 6, 40) ||
		borg_spell_legal_fail(REALM_ARCANE, 3, 1, 40) ||
		borg_spell_legal_fail(REALM_TRUMP, 1, 5, 40))
	{
		bp_ptr->able.teleport_level += 1000;
	}

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_PHASE_DOOR)) bp_ptr->able.phase += 5;

	/* Handle phase door */
	if (borg_spell_legal_fail(REALM_SORCERY, 0, 1, 40) ||
		borg_spell_legal_fail(REALM_ARCANE, 0, 4, 40) ||
		borg_spell_legal_fail(REALM_TRUMP, 0, 0, 40) ||
		borg_mindcr_legal_fail(MIND_MINOR_DISP, 3, 40) ||
		borg_mutation_check(MUT1_BLINK, TRUE))
	{
		bp_ptr->able.phase += 1000;
	}

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_PHASE_DOOR)) bp_ptr->able.teleport += 5;

	/* Handle teleport spell carefully */
	if (((borg_spell_okay_fail(REALM_ARCANE, 2, 3, 5) ||
		 borg_spell_okay_fail(REALM_LIFE, 4, 1, 5) ||
		 borg_spell_okay_fail(REALM_TRUMP, 0, 4, 5) ||
		 borg_spell_okay_fail(REALM_CHAOS, 0, 7, 5) ||
		 borg_mindcr_okay_fail(MIND_MAJOR_DISP, 7, 5) ||
		 borg_mutation_check(MUT1_VTELEPORT, TRUE)) &&
		 FLAG(bp_ptr, TR_RES_BLIND) &&
		 FLAG(bp_ptr, TR_RES_CONF)) 
		)
	{
		bp_ptr->able.teleport += 1000;
	}

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_PHASE_DOOR)) bp_ptr->able.speed += 5;

	/* speed spells */
	if (borg_spell_legal_fail(REALM_SORCERY, 1, 5, 40) ||
		borg_spell_legal_fail(REALM_DEATH, 2, 3, 40) ||
		borg_mindcr_legal_fail(MIND_ADRENALINE, 35, 40))
	{
		bp_ptr->able.speed += 1000;
	}

	/* berserk spells */
	if (borg_activate_fail(BORG_ACT_BERSERKER) ||
		borg_activate_fail(BORG_ACT_HEROISM) ||
		borg_spell_legal_fail(REALM_DEATH, 2, 0, 40) ||
		borg_mindcr_legal_fail(MIND_ADRENALINE, 35, 40) ||
		borg_mutation_check(MUT1_BERSERK, TRUE))
	{
		bp_ptr->able.berserk += 1000;
	}

	/* Handle "heal" */
	if (borg_spell_legal_fail(REALM_LIFE, 1, 6, 5) ||
		borg_spell_legal_fail(REALM_NATURE, 1, 7, 5))
	{
		bp_ptr->able.heal += 1000;
	}

	/* Not too many because of the possible swap */
	if (borg_activate_fail(BORG_ACT_HEAL_BIG)) bp_ptr->able.easy_heal += 5;

	/* Handle big healing spell */
	if (borg_spell_legal_fail(REALM_LIFE, 3, 4, 2))
	{
		bp_ptr->able.easy_heal += 1000;
	}

	/* Handle "fix exp" */
	if (borg_activate_fail(BORG_ACT_RESTORE_LIFE) ||
		borg_spell_legal_fail(REALM_LIFE, 3, 3, 60) ||
		borg_spell_legal_fail(REALM_DEATH, 1, 7, 60) ||
		borg_racial_check(RACE_AMBERITE_POWER2, FALSE) ||
		borg_racial_check(RACE_SKELETON, FALSE) ||
		borg_racial_check(RACE_ZOMBIE, FALSE))
	{
		amt_fix_exp += 1000;
	}

	/* Handle "recharge" */
	if (borg_activate_fail(BORG_ACT_RECHARGE) ||
		borg_spell_legal_fail(REALM_ARCANE, 3, 0, 60) ||
		borg_spell_legal_fail(REALM_SORCERY, 0, 7, 60))
	{
		bp_ptr->able.recharge += 1000;
	}

	/* Handle resistance */
	if (borg_activate_fail(BORG_ACT_RESISTANCE) ||
		borg_spell_legal_fail(REALM_NATURE, 2, 3, 40) ||
		borg_mindcr_legal_fail(MIND_CHAR_ARMOUR, 33, 40))
	{
		bp_ptr->able.res_all += 1000;
	}


	/*** Process the Needs ***/

	/* No need to *buy* stat increase potions */
	if (my_stat_cur[A_STR] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_STR] + cp_ptr->c_adj[A_STR]))
		amt_add_stat[A_STR] += 1000;

	if (my_stat_cur[A_INT] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_INT] + cp_ptr->c_adj[A_INT]))
		amt_add_stat[A_INT] += 1000;

	if (my_stat_cur[A_WIS] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_WIS] + cp_ptr->c_adj[A_WIS]))
		amt_add_stat[A_WIS] += 1000;

	if (my_stat_cur[A_DEX] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_DEX] + cp_ptr->c_adj[A_DEX]))
		amt_add_stat[A_DEX] += 1000;

	if (my_stat_cur[A_CON] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_CON] + cp_ptr->c_adj[A_CON]))
		amt_add_stat[A_CON] += 1000;

	if (my_stat_cur[A_CHR] >= 180 + 100 + 10 *
		(rp_ptr->r_adj[A_CHR] + cp_ptr->c_adj[A_CHR]))
		amt_add_stat[A_CHR] += 1000;

	/* Add star_healing and life potions into easy_heal */
	bp_ptr->able.easy_heal = amt_star_heal + amt_life;


	/*
	 * Correct bp_ptr->encumber from total weight to the degree
	 * of being overweight.
	 */
	/* Extract the "weight limit" (in tenth pounds) */
	carry_capacity = (adj_str_wgt[my_stat_ind[A_STR]] * 100) / 2;

	/* 0 if not encumbered, otherwise the encumberment */
	bp_ptr->encumber = MAX(0, bp_ptr->weight - carry_capacity);
}


/*
 * Update the Borg based on the current "frame"
 *
 * Assumes the Borg is actually in the dungeon.
 *
 * Note: everything starts off as FALSE by default
 * since we are wiped in borg_notice().
 */
void borg_update_frame(void)
{
	int i;

	dun_type *d_ptr = dungeon();

	s32b len = 10L * TOWN_DAWN;
	s32b tick = turn % len + len / 4;

	bp_ptr->hour = (24 * tick / len) % 24;

	/* Note "Lev" vs "LEV" */
	if (p_ptr->lev < p_ptr->max_lev) bp_ptr->status.fixlvl = TRUE;

	/* Extract "LEVEL xxxxxx" */
	bp_ptr->lev = p_ptr->lev;

	/* cheat the max clevel */
	bp_ptr->max_lev = p_ptr->max_lev;

	/* Note "Winner" */
	bp_ptr->winner = (char) p_ptr->state.total_winner;

	/* Assume experience is fine */
	bp_ptr->status.fixexp = FALSE;

	/* Note "Exp" vs "EXP" and am I lower than level 50 */
	if ((p_ptr->exp < p_ptr->max_exp) &&
		(bp_ptr->lev != 50)) bp_ptr->status.fixexp = TRUE;

	/* Extract "AU xxxxxxxxx" */
	borg_gold = p_ptr->au;

	/* Extract "Fast (+x)" or "Slow (-x)" */
	bp_ptr->speed = p_ptr->pspeed;

	/* Check my float for decrementing variables */
	if (bp_ptr->speed > 110)
	{
		borg_game_ratio = 100000 / (((bp_ptr->speed - 110) * 10) + 100);
	}
	else
	{
		borg_game_ratio = 1000;
	}


	/*
	 * If hasting, it doesn't count as 'borg_speed'.
	 * The speed gained from hasting is counted seperately.
	 */
	if (borg_speed) bp_ptr->speed -= 10;

	/* Extract "Cur AC xxxxx" */
	bp_ptr->ac = p_ptr->dis_ac + p_ptr->dis_to_a;

	/* Extract "Cur HP xxxxx" */
	bp_ptr->chp = p_ptr->chp;

	/* Extract "Max HP xxxxx" */
	bp_ptr->mhp = p_ptr->mhp;

	/* Extract "Cur SP xxxxx" (or zero) */
	bp_ptr->csp = p_ptr->csp;

	/* Extract "Max SP xxxxx" (or zero) */
	bp_ptr->msp = p_ptr->msp;

	/* Check for "Weak" */
	if (p_ptr->food < PY_FOOD_WEAK)
	{
		bp_ptr->status.weak = TRUE;
		bp_ptr->status.hungry = TRUE;
	}

	/* Check for "Hungry" */
	else if (p_ptr->food < PY_FOOD_ALERT) bp_ptr->status.hungry = TRUE;

	/* Check for "Normal" */
	else if (p_ptr->food < PY_FOOD_FULL) /* Nothing */ ;

	/* Check for "Full" */
	else if (p_ptr->food < PY_FOOD_MAX) bp_ptr->status.full = TRUE;

	/* Check for "Gorged" */
	else
	{
		bp_ptr->status.gorged = TRUE;
		bp_ptr->status.full = TRUE;
	}

	/* Check for "Blind" */
	if (p_ptr->tim.blind) bp_ptr->status.blind = TRUE;

	/* Check for "Confused" */
	if (p_ptr->tim.confused) bp_ptr->status.confused = TRUE;

	/* Check for "Afraid" */
	if (p_ptr->tim.afraid) bp_ptr->status.afraid = TRUE;

	/* Check for "Poisoned" */
	if (p_ptr->tim.poisoned) bp_ptr->status.poisoned = TRUE;

	/* Check for any text */
	if (p_ptr->tim.cut) bp_ptr->status.cut = TRUE;

	/* Check for Stun */
	if (p_ptr->tim.stun && (p_ptr->tim.stun <= 50)) bp_ptr->status.stun = TRUE;

	/* Check for Heavy Stun */
	if (p_ptr->tim.stun > 50) bp_ptr->status.heavy_stun = TRUE;

	/* XXX XXX XXX Parse "State" */
	if (p_ptr->state.searching) bp_ptr->status.search = TRUE;

	/* Check for "Study" */
	if (p_ptr->new_spells) bp_ptr->status.study = TRUE;

	/* Check for hallucination */
	if (p_ptr->tim.image) bp_ptr->status.image = TRUE;

	/* Parse stats */
	for (i = 0; i < A_MAX; i++)
	{
		bp_ptr->status.fixstat[i] = (p_ptr->stat[i].cur < p_ptr->stat[i].max);

		borg_stat[i] = p_ptr->stat[i].cur;
	}

	/* Hack -- Access depth */
	bp_ptr->depth = p_ptr->depth;

	/* If this is the first borg run then avoid reinitialization */
	if (old_depth == 128) old_depth = bp_ptr->depth;

	/* How deep can the borg expect to go down here?  */
	bp_ptr->max_depth = (d_ptr) ? d_ptr->recall_depth : 0;

	/* Hack -- Realms */
	bp_ptr->realm1 = p_ptr->spell.r[0].realm;
	bp_ptr->realm2 = p_ptr->spell.r[1].realm;

	/* Hack -- Mana increases */
	switch (borg_class)
	{
		case CLASS_PRIEST:
		case CLASS_PALADIN:
		case CLASS_MONK:
		case CLASS_MINDCRAFTER:
		{
			bp_ptr->wismana = 1;
			break;
		}

		case CLASS_MAGE:
		case CLASS_ROGUE:
		case CLASS_RANGER:
		case CLASS_WARRIOR_MAGE:
		case CLASS_CHAOS_WARRIOR:
		case CLASS_HIGH_MAGE:
		{
			bp_ptr->intmana = 1;
			break;
		}

		default:
		{
			bp_ptr->wismana = 0;
			bp_ptr->intmana = 0;
		}
	}
}


/*
 * This procedure sets to zero the various variables that are used for
 * determining a borg value.  This has to be done asap so that these can be
 * used both in the equipment and the inventory appraisal.
 */
static void borg_clear_vars(void)
{
	int i, ii;

	/* clear the struct */
	(void) WIPE(bp_ptr, borg_player);

	/* Reset basic */
	amt_food_scroll = 0;
	amt_food_lowcal = 0;
	amt_torch = 0;
	amt_lantern = 0;
	amt_flask = 0;

	/* Reset healing */
	amt_slow_poison = 0;
	amt_pot_curing = 0;
	amt_star_heal = 0;
	amt_life = 0;
	amt_rod_heal = 0;

	/* Reset books */
	for (i = 0; i < MAX_REALM; i++)
	{
		for (ii = 0; ii < 4; ii++)
		{
			amt_book[i][ii] = 0;
		}
	}

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
	amt_digger = 0;

	/* Reset enchantment */
	amt_enchant_to_a = 0;
	amt_enchant_to_d = 0;
	amt_enchant_to_h = 0;

	amt_brand_weapon = 0;
}

/*
 * Analyze the equipment and inventory
 */
void borg_notice(void)
{
	/* Clear out the player information */
	borg_clear_vars();

	/*
	 * Many of our variables are tied to borg_player.
	 * So we must update the frame the cheat in all
	 * the non-inventory details now.
	 */
	borg_update_frame();

	/* Notice the equipment */
	borg_notice_aux1();

	/* Notice the inventory */
	borg_notice_aux2();
}


/*
 * Helper function -- clear home values
 */
static void borg_notice_home_clear(void)
{
	int i, ii;

	/*** Reset counters ***/

	/* Reset basic */
	num_food = 0;
	num_food_scroll = 0;
	num_ident = 0;
	num_star_ident = 0;
	num_remove_curse = 0;
	num_star_remove_curse = 0;
	num_recall = 0;
	num_phase = 0;
	num_escape = 0;
	num_teleport = 0;
	num_teleport_level = 0;

	num_invisible = 0;
	num_glyph = 0;
	num_genocide = 0;
	num_mass_genocide = 0;
	num_berserk = 0;
	num_pot_resist = 0;
	num_goi_pot = 0;

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
	num_sustain_dex = 0;
	num_sustain_con = 0;
	num_sustain_all = 0;

	home_stat_add[A_STR] = 0;
	home_stat_add[A_INT] = 0;
	home_stat_add[A_WIS] = 0;
	home_stat_add[A_DEX] = 0;
	home_stat_add[A_CON] = 0;
	home_stat_add[A_CHR] = 0;

	num_artifact = 0;
	num_bad_curse = 0;
	num_weapons = 0;

	num_bow = 0;
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
	num_bad_gloves = 0;

	/* Reset healing */
	num_cure_critical = 0;
	num_cure_serious = 0;
	num_cure_light = 0;
	num_fix_exp = 0;
	num_mana = 0;
	num_heal = 0;
	num_ez_heal = 0;


	/* Reset missiles */
	num_missile = 0;

	/* Reset books */
	for (i = 0; i < MAX_REALM; i++)
	{
		for (ii = 0; ii < 4; ii++)
		{
			num_book[i][ii] = 0;
		}
	}

	/* Reset various */
	num_fix_stat[A_STR] = 0;
	num_fix_stat[A_INT] = 0;
	num_fix_stat[A_WIS] = 0;
	num_fix_stat[A_DEX] = 0;
	num_fix_stat[A_CON] = 0;
	num_fix_stat[A_CHR] = 0;
	num_fix_stat[6] = 0;

	home_slot_free = 0;
	home_damage = 0;

	num_duplicate_items = 0;
}


/*
 * Notice flags on item
 */
static void borg_notice_home_flags(list_item *l_ptr)
{
	if (KN_FLAG(l_ptr, TR_SLOW_DIGEST)) num_slow_digest += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_REGEN)) num_regenerate += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_TELEPATHY)) num_telepathy += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SEE_INVIS)) num_see_inv += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_FEATHER)) num_ffall += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_FREE_ACT)) num_free_act += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_HOLD_LIFE)) num_hold_life += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_IM_FIRE))
	{
		num_immune_fire += l_ptr->number;
		num_resist_fire += l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_IM_ACID))
	{
		num_immune_acid += l_ptr->number;
		num_resist_acid += l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_IM_COLD))
	{
		num_immune_cold += l_ptr->number;
		num_resist_cold += l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_IM_ELEC))
	{
		num_immune_elec += l_ptr->number;
		num_resist_elec += l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_RES_ACID)) num_resist_acid += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_ELEC)) num_resist_elec += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_FIRE)) num_resist_fire += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_COLD)) num_resist_cold += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_POIS)) num_resist_pois += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_SOUND)) num_resist_sound += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_LITE)) num_resist_lite += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_DARK)) num_resist_dark += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_CHAOS)) num_resist_chaos += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_CONF)) num_resist_conf += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_DISEN)) num_resist_disen += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_SHARDS)) num_resist_shard += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_NEXUS)) num_resist_nexus += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_BLIND)) num_resist_blind += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_RES_NETHER)) num_resist_neth += l_ptr->number;

	/* Count Sustains */
	if (KN_FLAG(l_ptr, TR_SUST_STR)) num_sustain_str += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SUST_INT)) num_sustain_int += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SUST_WIS)) num_sustain_wis += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SUST_DEX)) num_sustain_dex += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SUST_CON)) num_sustain_con += l_ptr->number;
	if (KN_FLAG(l_ptr, TR_SUST_STR) &&
		KN_FLAG(l_ptr, TR_SUST_INT) &&
		KN_FLAG(l_ptr, TR_SUST_WIS) &&
		KN_FLAG(l_ptr, TR_SUST_DEX) &&
		KN_FLAG(l_ptr, TR_SUST_CON)) num_sustain_all += l_ptr->number;

	/* count up bonus to stats */
	if (KN_FLAG(l_ptr, TR_STR))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_STR] += l_ptr->pval * l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_INT))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_INT] += l_ptr->pval * l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_WIS))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_WIS] += l_ptr->pval * l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_DEX))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_DEX] += l_ptr->pval * l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_CON))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_CON] += l_ptr->pval * l_ptr->number;
	}
	if (KN_FLAG(l_ptr, TR_CHR))
	{
		if (l_ptr->tval != TV_RING)
			home_stat_add[A_CHR] += l_ptr->pval * l_ptr->number;
	}

	/* count up bonus to speed */
	if (KN_FLAG(l_ptr, TR_SPEED)) num_speed += l_ptr->pval * l_ptr->number;
}


/*
 * This checks for duplicate items in the home
 */
static void borg_notice_home_dupe(list_item *l_ptr, bool check_sval, int i)
{
	int dupe_count, x;
	list_item *w_ptr;

	dupe_count = l_ptr->number - 1;

	/* Avoid trouble */
	if (dupe_count <= 0) return;

	/* Look for other items before this one that are the same */
	for (x = 0; x < i; x++)
	{
		if (x < home_num)
			w_ptr = &borg_home[x];
		else
			/* Check what the borg has on as well. */
			w_ptr = look_up_equip_slot(x - home_num);

		if (!w_ptr) continue;

		/* Don't count items we are swapping */
		if (w_ptr->treat_as == TREAT_AS_SWAP) continue;

		/*
		 * If everything matches it is a duplicate item
		 * Note that we only check sval on certain items.  This
		 * is because, for example, two pairs of dragon armor
		 * are not the same unless their subtype (color) matches
		 * but a defender is a defender even if one is a dagger and
		 * one is a mace
		 */
		if (l_ptr->tval == w_ptr->tval)
		{
			if (check_sval &&
				(k_info[l_ptr->k_idx].sval != k_info[w_ptr->k_idx].sval))
			{
				/* Svals don't match when required */
				continue;
			}

			/* Does only one have an xtra name? */
			if ((l_ptr->xtra_name == NULL) != (w_ptr->xtra_name == NULL))
			{
				continue;
			}

			/* Do the xtra names match? */
			if ((l_ptr->xtra_name == w_ptr->xtra_name) ||
				(streq(l_ptr->xtra_name, w_ptr->xtra_name)))
			{
				/* Count duplicate items */
				dupe_count++;
			}
		}
	}

	/* There can be one dupe of rings because there are two ring slots. */
	if (l_ptr->tval == TV_RING) dupe_count--;

	/* Add this items count to the total duplicate count */
	num_duplicate_items += dupe_count;
}

/*
 * Examine weapons in the home
 */
static void borg_notice_home_weapon(list_item *l_ptr)
{
	s16b num_blow;

	int str_index, dex_index;
	int num = 0, wgt = 0, mul = 0, div = 0;

	num_weapons += l_ptr->number;

	/* apw most edged weapons hurt magic for priests */
	if (borg_class == CLASS_PRIEST)
	{
		/* Penalize non-blessed edged weapons */
		if (((l_ptr->tval == TV_SWORD) || (l_ptr->tval == TV_POLEARM))
			&& !KN_FLAG(l_ptr, TR_BLESSED))
		{
			num_edged_weapon += l_ptr->number;
		}
	}


	/*
	 * NOTE:  This damage does not take slays into account.
	 * It is just a rough estimate to make sure the glave of pain
	 * is kept if it is found.
	 * It is hard to hold a heavy weapon.
	 */
	num_blow = 1;

	if (adj_str_hold[my_stat_ind[A_STR]] >= l_ptr->weight / 10)
	{

		/* Analyze the class */
		switch (borg_class)
		{
			case CLASS_WARRIOR:
			{
				/* Warrior */
				num = 5;
				wgt = 30;
				mul = 5;
				break;
			}

			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			{
				/* Mage */
				num = 2;
				wgt = 40;
				mul = 2;
				break;
			}

			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
			{
				/* Priest, Mindcrafter */
				num = 4;
				wgt = 35;
				mul = 3;
				break;
			}

			case CLASS_ROGUE:
			{
				/* Rogue */
				num = 4;
				wgt = 30;
				mul = 3;
				break;
			}

			case CLASS_RANGER:
			{
				/* Ranger */
				num = 4;
				wgt = 35;
				mul = 4;
				break;
			}

			case CLASS_PALADIN:
			{
				/* Paladin */
				num = 4;
				wgt = 30;
				mul = 4;
				break;
			}

			case CLASS_WARRIOR_MAGE:
			{
				/* Warrior-Mage */
				num = 4;
				wgt = 35;
				mul = 3;
				break;
			}

			case CLASS_CHAOS_WARRIOR:
			{
				/* Chaos Warrior */
				num = 4;
				wgt = 30;
				mul = 4;
				break;
			}

			case CLASS_MONK:
			{
				/* Monk */
				num = ((p_ptr->lev < 40) ? 2 : 3);
				wgt = 40;
				mul = 4;
				break;
			}
		}

		/* Enforce a minimum "weight" */
		div = ((l_ptr->weight < wgt) ? wgt : l_ptr->weight);

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

	if (KN_FLAG(l_ptr, TR_BLOWS)) num_blow += l_ptr->pval;

	num_blow *= l_ptr->number;

	/*
	 * It should include bp_ptr->to_d in the home damage calculation too but
	 * that value is not necessarily finished when it is used here.  This can
	 * cause home loops so I delete the reference to it
	 */
	if (l_ptr->to_d > 8 || bp_ptr->lev < 15)
	{
		home_damage += num_blow * (l_ptr->dd * l_ptr->ds + l_ptr->to_d);
	}
	else
	{
		home_damage += num_blow * (l_ptr->dd * l_ptr->ds + 8);
	}
}

/*
 * Examine potions in the home
 */
static void borg_notice_home_potion(list_item *l_ptr)
{
	/* Analyze */
	switch (k_info[l_ptr->k_idx].sval)
	{
		case SV_POTION_CURING:
		{
			num_cure_critical += l_ptr->number;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			num_cure_critical += l_ptr->number;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			num_cure_serious += l_ptr->number;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			num_cure_light += l_ptr->number;
			break;
		}

		case SV_POTION_RESISTANCE:
		{
			num_pot_resist += l_ptr->number;
			break;
		}

		case SV_POTION_RES_STR:
		{
			num_fix_stat[A_STR] += l_ptr->number;
			break;
		}

		case SV_POTION_RES_INT:
		{
			num_fix_stat[A_INT] += l_ptr->number;
			break;
		}

		case SV_POTION_RES_WIS:
		{
			num_fix_stat[A_WIS] += l_ptr->number;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			num_fix_stat[A_DEX] += l_ptr->number;
			break;
		}

		case SV_POTION_RES_CON:
		{
			num_fix_stat[A_CON] += l_ptr->number;
			break;
		}

		case SV_POTION_RES_CHR:
		{
			num_fix_stat[A_CHR] += l_ptr->number;
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			num_fix_exp += l_ptr->number;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			num_mana += l_ptr->number;
			break;
		}

		case SV_POTION_HEALING:
		{
			num_heal += l_ptr->number;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			num_ez_heal += l_ptr->number;
			break;
		}

		case SV_POTION_LIFE:
		{
			num_ez_heal += l_ptr->number;
			break;
		}

		case SV_POTION_BERSERK_STRENGTH:
		{
			num_berserk += l_ptr->number;
			break;
		}

		case SV_POTION_SPEED:
		{
			num_speed += l_ptr->number;
			break;
		}

		case SV_POTION_INVULNERABILITY:
		{
			num_goi_pot += l_ptr->number;
			break;
		}
	}
}

/*
 * Examine scrolls in the home
 */
static void borg_notice_home_scroll(list_item *l_ptr)
{
	/* Analyze the scroll */
	switch (k_info[l_ptr->k_idx].sval)
	{
		case SV_SCROLL_IDENTIFY:
		{
			num_ident += l_ptr->number;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			num_star_ident += l_ptr->number;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			num_remove_curse += l_ptr->number;
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			num_star_remove_curse += l_ptr->number;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			num_phase += l_ptr->number;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			num_escape += l_ptr->number;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			num_recall += l_ptr->number;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			num_glyph += l_ptr->number;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			num_teleport_level += l_ptr->number;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			num_food_scroll += l_ptr->number;
			num_food += l_ptr->number;
			break;
		}
	}
}

/*
 * Include effects of spells on home items
 */
static void borg_notice_home_spells(void)
{
	/* Handle "satisfy hunger" -> infinite food */
	if (borg_spell_legal_fail(REALM_LIFE, 0, 7, 40) ||
		borg_spell_legal_fail(REALM_ARCANE, 2, 6, 40) ||
		borg_spell_legal_fail(REALM_NATURE, 0, 3, 40))
	{
		num_food += 1000;
		num_food_scroll += 1000;
	}

	/* Handle "identify" -> infinite identifies */
	if (borg_spell_legal_fail(REALM_SORCERY, 1, 1, 60) ||
		borg_spell_legal_fail(REALM_ARCANE, 3, 2, 60) ||
		borg_mindcr_legal_fail(MIND_PSYCHOMETRY, 25, 60) ||
		borg_equips_rod_fail(SV_ROD_IDENTIFY))
	{
		num_ident += 1000;
	}
	/* Handle "*identify*" -> infinite *identifies* */
	if (borg_spell_legal_fail(REALM_NATURE, 2, 5, 60) ||
		borg_spell_legal_fail(REALM_SORCERY, 1, 7, 60) ||
		borg_spell_legal_fail(REALM_DEATH, 3, 2, 60) ||
		borg_spell_legal_fail(REALM_TRUMP, 3, 1, 60) ||
		borg_spell_legal_fail(REALM_LIFE, 3, 5, 60))
	{
		num_ident += 1000;
		num_star_ident += 1000;
	}

	/* Handle "remove curse" -> infinite remove curses */
	if (borg_spell_legal_fail(REALM_LIFE, 1, 0, 40) ||
		borg_spell_legal_fail(REALM_LIFE, 2, 1, 40))
	{
		num_remove_curse += 1000;
	}

	/* Handle "*remove curse*" -> infinite *remove curses* */
	if (borg_spell_legal_fail(REALM_LIFE, 2, 1, 40))
	{
		num_star_remove_curse += 1000;
	}

	/* apw Handle "rune of protection" glyph */
	if (borg_spell_legal_fail(REALM_LIFE, 1, 7, 40) ||
		borg_spell_legal_fail(REALM_LIFE, 2, 7, 40))
	{
		num_glyph += 1000;
	}

	/* Handle recall */
	if (borg_spell_legal_fail(REALM_ARCANE, 3, 6, 40) ||
		borg_spell_legal_fail(REALM_SORCERY, 2, 7, 40) ||
		borg_spell_legal_fail(REALM_TRUMP, 1, 6, 40) ||
		borg_mutation_check(MUT1_RECALL, TRUE) ||
		borg_equips_rod_fail(SV_ROD_RECALL))
	{
		num_recall += 1000;
	}

	/* Handle teleport_level */
	if (borg_spell_legal_fail(REALM_SORCERY, 2, 6, 40) ||
		borg_spell_legal_fail(REALM_TRUMP, 1, 5, 40))
	{
		num_teleport_level += 1000;
	}

	/* Handle resistance */
	if (borg_mindcr_legal_fail(MIND_CHAR_ARMOUR, 33, 60))
	{
		num_pot_resist += 1000;
	}
	
	/* Handle speed */
	if (borg_mindcr_legal_fail(MIND_ADRENALINE, 25, 40) ||
		borg_spell_legal_fail(REALM_SORCERY, 1, 5, 40))
	{
		num_speed += 1000;
	}

}


/*
 * Innate abilities of the player can affect home item choice
 */
static void borg_notice_home_player(void)
{
	object_flags oflags;
	object_flags *of_ptr = &oflags;

	int i;

	/* Hack -- No need for stat repair */
	for (i = 0; i < A_MAX; i++)
	{
		if (bp_ptr->sust[i]) num_fix_stat[i] += 1000;
	}

	/* Extract the player flags */
	player_flags(of_ptr);

	/* Good flags */
	if (FLAG(of_ptr, TR_SLOW_DIGEST)) num_slow_digest = TRUE;
	if (FLAG(of_ptr, TR_FEATHER)) num_ffall = TRUE;
	if (FLAG(of_ptr, TR_LITE)) num_lite = TRUE;
	if (FLAG(of_ptr, TR_REGEN)) num_regenerate = TRUE;
	if (FLAG(of_ptr, TR_TELEPATHY)) num_telepathy = TRUE;
	if (FLAG(of_ptr, TR_SEE_INVIS)) num_see_inv = TRUE;
	if (FLAG(of_ptr, TR_FREE_ACT)) num_free_act = TRUE;
	if (FLAG(of_ptr, TR_HOLD_LIFE)) num_hold_life = TRUE;

	/* Weird flags */

	/* Bad flags */

	/* Immunity flags */
	if (FLAG(of_ptr, TR_IM_FIRE)) num_immune_fire = TRUE;
	if (FLAG(of_ptr, TR_IM_ACID)) num_immune_acid = TRUE;
	if (FLAG(of_ptr, TR_IM_COLD)) num_immune_cold = TRUE;
	if (FLAG(of_ptr, TR_IM_ELEC)) num_immune_elec = TRUE;

	/* Resistance flags */
	if (FLAG(of_ptr, TR_RES_ACID)) num_resist_acid = TRUE;
	if (FLAG(of_ptr, TR_RES_ELEC)) num_resist_elec = TRUE;
	if (FLAG(of_ptr, TR_RES_FIRE)) num_resist_fire = TRUE;
	if (FLAG(of_ptr, TR_RES_COLD)) num_resist_cold = TRUE;
	if (FLAG(of_ptr, TR_RES_POIS)) num_resist_pois = TRUE;
	if (FLAG(of_ptr, TR_RES_LITE)) num_resist_lite = TRUE;
	if (FLAG(of_ptr, TR_RES_DARK)) num_resist_dark = TRUE;
	if (FLAG(of_ptr, TR_RES_BLIND)) num_resist_blind = TRUE;
	if (FLAG(of_ptr, TR_RES_CONF)) num_resist_conf = TRUE;
	if (FLAG(of_ptr, TR_RES_SOUND)) num_resist_sound = TRUE;
	if (FLAG(of_ptr, TR_RES_SHARDS)) num_resist_shard = TRUE;
	if (FLAG(of_ptr, TR_RES_NEXUS)) num_resist_nexus = TRUE;
	if (FLAG(of_ptr, TR_RES_NETHER)) num_resist_neth = TRUE;
	if (FLAG(of_ptr, TR_RES_CHAOS)) num_resist_chaos = TRUE;
	if (FLAG(of_ptr, TR_RES_DISEN)) num_resist_disen = TRUE;

	/* Sustain flags */
	if (FLAG(of_ptr, TR_SUST_STR)) num_sustain_str = TRUE;
	if (FLAG(of_ptr, TR_SUST_INT)) num_sustain_int = TRUE;
	if (FLAG(of_ptr, TR_SUST_WIS)) num_sustain_wis = TRUE;
	if (FLAG(of_ptr, TR_SUST_DEX)) num_sustain_dex = TRUE;
	if (FLAG(of_ptr, TR_SUST_CON)) num_sustain_con = TRUE;
}


/*
 * Notice a particular item
 */
static void borg_notice_home_item(list_item *l_ptr, int i)
{
	/* Just checking */
	if (!l_ptr) return;

	 /* If this item needs a scroll of *id* */
	if (KN_FLAG(l_ptr, TR_INSTA_ART) && !borg_obj_known_full(l_ptr))
	{
		/* count it */
		num_artifact += l_ptr->number;
	}

	/* If this item has some really bad flag count it */
	if (borg_test_bad_curse(l_ptr)) num_bad_curse += l_ptr->number;

	/* Analyze the item */
	switch (l_ptr->tval)
	{
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			num_armor += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);
			break;
		}

		case TV_DRAG_ARMOR:
		{
			num_armor += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, TRUE, i);
			break;
		}

		case TV_CLOAK:
		{
			num_cloaks += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);

			break;
		}

		case TV_SHIELD:
		{
			num_shields += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);
			break;
		}

		case TV_HELM:
		case TV_CROWN:
		{
			num_hats += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);

			break;
		}

		case TV_GLOVES:
		{
			num_gloves += l_ptr->number;

			/* most gloves hurt magic for spell-casters */
			if (bp_ptr->intmana && bp_ptr->msp > 3)
			{
				/* Penalize non-usable gloves */
				if (l_ptr->number &&
					!KN_FLAG(l_ptr, TR_FREE_ACT) &&
					!(KN_FLAG(l_ptr, TR_DEX) && (l_ptr->pval > 0)))
				{
					num_bad_gloves += l_ptr->number;
				}
			}

			/* gloves of slaying give a damage bonus */
			home_damage += l_ptr->to_d * 3;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);

			break;
		}

		case TV_LITE:
		{
			if (KN_FLAG(l_ptr, TR_INSTA_ART))
			{
				num_lite += l_ptr->number;
			}
			break;
		}

		case TV_BOOTS:
		{
			num_boots += l_ptr->number;

			/* see if this item is duplicated */
			borg_notice_home_dupe(l_ptr, FALSE, i);
			break;
		}

		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		{
			/* Look at weapon information */
			borg_notice_home_weapon(l_ptr);

			/* see if this item is a duplicate */
			borg_notice_home_dupe(l_ptr, FALSE, i);
			break;
		}

		case TV_BOW:
		{
			num_bow += l_ptr->number;

			/* see if this item is a duplicate */
			borg_notice_home_dupe(l_ptr, FALSE, i);
			break;
		}

		case TV_RING:
		{
			num_rings += l_ptr->number;

			/* see if this item is a duplicate */
			borg_notice_home_dupe(l_ptr, TRUE, i);

			break;
		}

		case TV_AMULET:
		{
			num_neck += l_ptr->number;

			/* see if this item is a duplicate */
			borg_notice_home_dupe(l_ptr, TRUE, i);
			break;
		}

		case TV_LIFE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_LIFE))
				num_book[REALM_LIFE][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}

		case TV_SORCERY_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_SORCERY))
				num_book[REALM_SORCERY][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}

		case TV_NATURE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_NATURE))
				num_book[REALM_NATURE][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}
		case TV_CHAOS_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_CHAOS))
				num_book[REALM_CHAOS][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}
		case TV_DEATH_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_DEATH))
				num_book[REALM_DEATH][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}
		case TV_TRUMP_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_TRUMP))
				num_book[REALM_TRUMP][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}
		case TV_ARCANE_BOOK:
		{
			/* Count good books */
			if (borg_has_realm(REALM_ARCANE))
				num_book[REALM_ARCANE][k_info[l_ptr->k_idx].sval] +=
					l_ptr->number;
			break;
		}

		case TV_FOOD:
		{
			/* Food */

			/* Analyze */
			switch (k_info[l_ptr->k_idx].sval)
			{
				case SV_FOOD_RATION:
				{
					/* If the borg can digest food collect some at home */
					if (!FLAG(bp_ptr, TR_CANT_EAT)) num_food += l_ptr->number;
					break;
				}

				case SV_FOOD_RESTORE_STR:
				{
					num_fix_stat[A_STR] += l_ptr->number;
					break;
				}

				case SV_FOOD_RESTORE_CON:
				{
					num_fix_stat[A_CON] += l_ptr->number;
					break;
				}

				case SV_FOOD_RESTORING:
				{
					num_fix_stat[A_STR] += l_ptr->number;
					num_fix_stat[A_INT] += l_ptr->number;
					num_fix_stat[A_WIS] += l_ptr->number;
					num_fix_stat[A_DEX] += l_ptr->number;
					num_fix_stat[A_CON] += l_ptr->number;
					num_fix_stat[A_CHR] += l_ptr->number;
					num_fix_stat[6] += l_ptr->number;
					break;
				}
			}

			break;
		}


		case TV_POTION:
		{
			/* Potions */
			borg_notice_home_potion(l_ptr);

			break;
		}


		case TV_SCROLL:
		{
			/* Scrolls */
			borg_notice_home_scroll(l_ptr);

			break;
		}


		case TV_ROD:
		{
			/* Rods */

			/* Analyze */
			switch (k_info[l_ptr->k_idx].sval)
			{
				case SV_ROD_IDENTIFY:
				{
					num_ident += l_ptr->number * 100;
					break;
				}

				case SV_ROD_RECALL:
				{
					num_recall += l_ptr->number * 50;
					break;
				}

				case SV_ROD_CURING:
				{
					num_cure_critical += l_ptr->number * 20;
					break;
				}
			}

			break;
		}

		case TV_STAFF:
		{
			/* Staffs */

			/* Only collect staves with more than 3 charges at high level */
			if (l_ptr->pval <= 3 && bp_ptr->lev > 30)
				break;

			/* Analyze */
			switch (k_info[l_ptr->k_idx].sval)
			{
				case SV_STAFF_IDENTIFY:
				{
					num_ident += l_ptr->number * l_ptr->pval;
					break;
				}

				case SV_STAFF_CURING:
				{
					num_cure_critical += l_ptr->number * l_ptr->pval;
					break;
				}

				case SV_STAFF_TELEPORTATION:
				{
					/*
					 * Don't use them deep in the dungeon because the
					 * charges will get drained and he wont have any
					 * scrolls left to read
					 */
					if (bp_ptr->max_depth < 97)
					{
						num_teleport += l_ptr->number * l_ptr->pval;
					}
					break;
				}
			}

			break;
		}

		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Missiles */

			/* Hack -- ignore invalid missiles */
			if (l_ptr->tval != my_ammo_tval) break;

			/* Count them */
			num_missile += l_ptr->number;

			break;
		}
	}
}


/*
 * Helper function -- notice the home inventory
 */
static void borg_notice_home_aux(void)
{
	int i;
	int num;

	list_item *l_ptr;

	/*** Process the inventory ***/

	/* Scan the home */
	for (i = 0; i < (home_num + EQUIP_MAX); i++)
	{
		if (i < home_num)
			l_ptr = &borg_home[i];
		else
			l_ptr = look_up_equip_slot(i - home_num);

		/* Skip empty / unaware items */
		if (!l_ptr || !l_ptr->k_idx) continue;

		/* Don't count items we are swapping or dropping */
		if (l_ptr->treat_as == TREAT_AS_SWAP) continue;
		if (l_ptr->treat_as == TREAT_AS_GONE) continue;

		/* Save number of items */
		num = l_ptr->number;

		/* Hack - simulate change in number of items */
		if (l_ptr->treat_as == TREAT_AS_LESS) l_ptr->number--;
		if (l_ptr->treat_as == TREAT_AS_MORE) l_ptr->number++;

		/* Notice item flags */
		borg_notice_home_flags(l_ptr);

		/* Notice the item itself */
		borg_notice_home_item(l_ptr, i);

		/* Hack - revert change in number of items */
		l_ptr->number = num;
	}

	/* Scan the inventory for virtual home items */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Ignore normal items */
		if (l_ptr->treat_as == TREAT_AS_NORM) continue;

		/* Save number of items */
		num = l_ptr->number;

		/* Hack - simulate change in number of items */
		if (l_ptr->treat_as == TREAT_AS_LESS) l_ptr->number = 1;
		if (l_ptr->treat_as == TREAT_AS_SWAP) l_ptr->number = 1;
		if (l_ptr->treat_as == TREAT_AS_MORE) l_ptr->number++;

		/* Notice item flags */
		borg_notice_home_flags(l_ptr);

		/* Notice the item itself */
		borg_notice_home_item(l_ptr, home_num + EQUIP_MAX - 1);

		/* Hack - revert change in number of items */
		l_ptr->number = num;

		/* Hack - assume only one swap at a time */
		break;
	}

	/* Scan for virtual home items from stores */
	if (use_shop)
	{
		for (i = 0; i < cur_num; i++)
		{
			l_ptr = &cur_list[i];

			/* Ignore normal items */
			if (l_ptr->treat_as == TREAT_AS_NORM) continue;

			/* Save number of items */
			num = l_ptr->number;

			/* Hack - simulate change in number of items */
			if (l_ptr->treat_as == TREAT_AS_LESS) l_ptr->number = 1;
			if (l_ptr->treat_as == TREAT_AS_MORE) l_ptr->number++;

			/* Notice item flags */
			borg_notice_home_flags(l_ptr);

			/* Notice the item itself */
			borg_notice_home_item(l_ptr, home_num + EQUIP_MAX - 1);

			/* Hack - revert change in number of items */
			l_ptr->number = num;
		}
	}

	/*** Process the Spells and Prayers ***/
	borg_notice_home_spells();

	/*** Process the player abilities ***/
	borg_notice_home_player();
}


/*
 * Extract the bonuses for items in the home.
 */
void borg_notice_home(void)
{
	/* Notice the home equipment */
	borg_notice_home_clear();

	/* Notice the home inventory */
	borg_notice_home_aux();
}





/*
 * Helper function -- calculate power of equipment in the home
 */
static s32b borg_power_home_aux1(void)
{
	s32b value = 0L;

	/*
	 * This would be better seperated by item type
	 * (so 1 bonus for resist cold armor
	 *   1 bonus for resist cold shield...
	 * but that would take a bunch more code.
	 *
	 * Try to collect at least 2 of each resist/power (for swapping)
	 * This can be used to get rid of extra artifacts...
	 */

	/* Spare lite sources.  Artifacts only */
	if (num_lite == 1)
		value += 150L;
	else if (num_lite == 2)
		value += 170L;
	else if (num_lite > 2)
		value += 170L + (num_lite - 2) * 5L;

	if (num_slow_digest == 1)
		value += 50L;
	else if (num_slow_digest == 2)
		value += 70L;
	else if (num_slow_digest > 2)
		value += 70L + (num_slow_digest - 2) * 5L;

	if (num_regenerate == 1)
		value += 75L;
	else if (num_regenerate == 2)
		value += 100L;
	else if (num_regenerate > 2)
		value += 100L + (num_regenerate - 2) * 10L;

	if (num_telepathy == 1)
		value += 1000L;
	else if (num_telepathy == 2)
		value += 1500L;
	else if (num_telepathy > 2)
		value += 1500L + (num_telepathy - 2) * 10L;

	if (num_see_inv == 1)
		value += 800L;
	else if (num_see_inv == 2)
		value += 1200L;
	else if (num_see_inv > 2)
		value += 1200L + (num_see_inv - 2) * 10L;

	if (num_ffall == 1)
		value += 10L;
	else if (num_ffall == 2)
		value += 15L;
	else if (num_ffall > 2)
		value += 15L + (num_ffall - 2) * 1L;


	if (num_free_act == 1)
		value += 1000L;
	else if (num_free_act == 2)
		value += 1500L;
	else if (num_free_act > 2)
		value += 1500L + (num_free_act - 2) * 10L;

	if (num_hold_life == 1)
		value += 1000L;
	else if (num_hold_life == 2)
		value += 1500L;
	else if (num_hold_life > 2)
		value += 1500L + (num_hold_life - 2) * 10L;

	if (num_resist_acid == 1)
		value += 1000L;
	else if (num_resist_acid == 2)
		value += 1500L;
	else if (num_resist_acid > 2)
		value += 1500L + (num_resist_acid - 2) * 1L;
	if (num_immune_acid == 1)
		value += 3000L;
	else if (num_immune_acid == 2)
		value += 5000L;
	else if (num_immune_acid > 2)
		value += 5000L + (num_immune_acid - 2) * 30L;

	if (num_resist_elec == 1)
		value += 1000L;
	else if (num_resist_elec == 2)
		value += 1500L;
	else if (num_resist_elec > 2)
		value += 1500L + (num_resist_elec - 2) * 1L;
	if (num_immune_elec == 1)
		value += 3000L;
	else if (num_immune_elec == 2)
		value += 5000L;
	else if (num_immune_elec > 2)
		value += 5000L + (num_immune_elec - 2) * 30L;

	if (num_resist_fire == 1)
		value += 1000L;
	else if (num_resist_fire == 2)
		value += 1500L;
	else if (num_resist_fire > 2)
		value += 1500L + (num_resist_fire - 2) * 1L;
	if (num_immune_fire == 1)
		value += 3000L;
	else if (num_immune_fire == 2)
		value += 5000L;
	else if (num_immune_fire > 2)
		value += 5000L + (num_immune_fire - 2) * 30L;

	if (num_resist_cold == 1)
		value += 1000L;
	else if (num_resist_cold == 2)
		value += 1500L;
	else if (num_resist_cold > 2)
		value += 1500L + (num_resist_cold - 2) * 1L;
	if (num_immune_cold == 1)
		value += 3000L;
	else if (num_immune_cold == 2)
		value += 5000L;
	else if (num_immune_cold > 2)
		value += 5000L + (num_immune_cold - 2) * 30L;

	if (num_resist_pois == 1)
		value += 5000L;
	else if (num_resist_pois == 2)
		value += 9000L;
	else if (num_resist_pois > 2)
		value += 9000L + (num_resist_pois - 2) * 40L;

	if (num_resist_conf == 1)
		value += 2000L;
	else if (num_resist_conf == 2)
		value += 3500L;
	else if (num_resist_conf > 2)
		value += 3500L + (num_resist_conf - 2) * 45L;

	if (num_resist_sound == 1)
		value += 500L;
	else if (num_resist_sound == 2)
		value += 700L;
	else if (num_resist_sound > 2)
		value += 700L + (num_resist_sound - 2) * 30L;

	if (num_resist_lite == 1)
		value += 100L;
	else if (num_resist_lite == 2)
		value += 150L;
	else if (num_resist_lite > 2)
		value += 150L + (num_resist_lite - 2) * 1L;

	if (num_resist_dark == 1)
		value += 100L;
	else if (num_resist_dark == 2)
		value += 150L;
	else if (num_resist_dark > 2)
		value += 150L + (num_resist_dark - 2) * 1L;

	if (num_resist_chaos == 1)
		value += 1000L;
	else if (num_resist_chaos == 2)
		value += 1500L;
	else if (num_resist_chaos > 2)
		value += 1500L + (num_resist_chaos - 2) * 10L;

	if (num_resist_disen == 1)
		value += 5000L;
	else if (num_resist_disen == 2)
		value += 7000L;
	else if (num_resist_disen > 2)
		value += 7000L + (num_resist_disen - 2) * 35L;

	if (num_resist_shard == 1)
		value += 100L;
	else if (num_resist_shard == 2)
		value += 150L;
	else if (num_resist_shard > 2)
		value += 150L + (num_resist_shard - 2) * 1L;

	if (num_resist_nexus == 1)
		value += 200L;
	else if (num_resist_nexus == 2)
		value += 300L;
	else if (num_resist_nexus > 2)
		value += 300L + (num_resist_nexus - 2) * 2L;

	if (num_resist_blind == 1)
		value += 500L;
	else if (num_resist_blind == 2)
		value += 700L;
	else if (num_resist_blind > 2)
		value += 700L + (num_resist_blind - 2) * 5L;

	if (num_resist_neth == 1)
		value += 5000L;
	else if (num_resist_neth == 2)
		value += 7000L;
	else if (num_resist_neth > 2)
		value += 7000L + (num_resist_neth - 2) * 45L;

	/*
	 * Stat gain items as well...
	 * (good to carry ring of dex +6 in
	 * house even if I don't need it right now)
	 */
	if (home_stat_add[A_STR] < 9)
		value += home_stat_add[A_STR] * 300L;
	else if (home_stat_add[A_STR] < 15)
		value += 9 * 300L + (home_stat_add[A_STR] - 9) * 200L;
	else
		value += 9 * 300L + 6 * 200L + (home_stat_add[A_STR] - 15) * 1L;

	if (home_stat_add[A_DEX] < 9)
		value += home_stat_add[A_DEX] * 300L;
	else if (home_stat_add[A_DEX] < 15)
		value += 9 * 300L + (home_stat_add[A_DEX] - 9) * 200L;
	else
		value += 9 * 300L + 6 * 200L + (home_stat_add[A_DEX] - 15) * 1L;

	if (home_stat_add[A_CON] < 15)
		value += home_stat_add[A_CON] * 300L;
	else if (home_stat_add[A_CON] < 21)
		value += 15 * 300L + (home_stat_add[A_CON] - 15) * 200L;
	else
		value += 15 * 300L + 6 * 200L + (home_stat_add[A_CON] - 21) * 1L;

	/* int and wis are only bonused for spell casters. */
	if (bp_ptr->intmana)
	{
		if (home_stat_add[A_INT] < 20)
			value += home_stat_add[A_INT] * 400L;
		else if (home_stat_add[A_INT] < 26)
			value += 20 * 400L + (home_stat_add[A_INT] - 20) * 300L;
		else
			value += 20 * 100L + 6 * 300L + (home_stat_add[A_INT] - 26) * 5L;
	}

	if (bp_ptr->wismana)
	{
		if (home_stat_add[A_WIS] < 20)
			value += home_stat_add[A_WIS] * 400L;
		else if (home_stat_add[A_WIS] < 26)
			value += 20 * 400L + (home_stat_add[A_WIS] - 20) * 300L;
		else
			value += 20 * 400L + 6 * 300L + (home_stat_add[A_WIS] - 26) * 3L;
	}

	/* Sustains */
	if (num_sustain_str == 1)
		value += 200L;
	else if (num_sustain_str == 2)
		value += 250L;
	else if (num_sustain_str > 2)
		value += 250L + (num_sustain_str - 2) * 1L;

	if (num_sustain_int == 1)
		value += 200L;
	else if (num_sustain_int == 2)
		value += 250L;
	else if (num_sustain_int > 2)
		value += 250L + (num_sustain_int - 2) * 1L;

	if (num_sustain_wis == 1)
		value += 200L;
	else if (num_sustain_wis == 2)
		value += 250L;
	else if (num_sustain_wis > 2)
		value += 250L + (num_sustain_wis - 2) * 1L;

	if (num_sustain_con == 1)
		value += 200L;
	else if (num_sustain_con == 2)
		value += 250L;
	else if (num_sustain_con > 2)
		value += 250L + (num_sustain_con - 2) * 1L;

	if (num_sustain_dex == 1)
		value += 200L;
	else if (num_sustain_dex == 2)
		value += 250L;
	else if (num_sustain_dex > 2)
		value += 250L + (num_sustain_dex - 2) * 1L;

	if (num_sustain_all == 1)
		value += 1000L;
	else if (num_sustain_all == 2)
		value += 1500L;
	else if (num_sustain_all > 2)
		value += 1500L + (num_sustain_all - 2) * 1L;

	/* Count the un*id*'d artifacts stored at home */
	value += MIN(num_artifact, 7) * 100000;

	/*
	 * Do a minus for too many duplicates.
	 * This way we do not store useless items
	 * and spread out types of items.
	 */
	if (num_weapons > 5)
		value -= (num_weapons - 5) * 2000L;
	else if (num_weapons > 1)
		value -= (num_weapons - 1) * 100L;
	if (num_bow > 2)
		value -= (num_bow - 2) * 1000L;
	if (num_rings > 6)
		value -= (num_rings - 6) * 4000L;
	else if (num_rings > 4)
		value -= (num_rings - 4) * 2000L;
	if (num_neck > 3)
		value -= (num_neck - 3) * 1500L;
	else if (num_neck > 2)
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

	/* If edged and priest, dump it   */
	value -= num_edged_weapon * 100000L;

	/* If gloves and mage or ranger and not FA/Dex, dump it. */
	value -= num_bad_gloves * 100000L;

	/* Do not allow duplication of items. */
	value -= num_duplicate_items * 5000L;

	/* Do not allow bad flags */
	value -= num_bad_curse * 100000;

	/* Return the value */
	return (value);
}


/*
 * Helper function -- calculate power of items in the home.
 * If the value here is higher than the value for the same item in the
 * the borg will drop it at home.  So this way the borg collects things at home
 * There are two values for most items.  The idea here is that once the borg
 * has stockpiled a bit at home he can carry more of these items in his inv.
 * Look at num_food below.  That corresponds to bp_ptr->food that counts the
 * amount of food in the inv.  Having food in the inv is very important so the
 * borg gets 10000 per food item in the inv, but only for the first five.  Then
 * the borg collects some for the home and once he 10 at home he takes along
 * more in the inv.
 * The usage of 'pile' is a try to keep the home not too full, otherwise the
 * borg may spend all his money on building stock for boring items.  Otherwise
 * he'd only go down to lvl 5 when he has 99 scrolls of Word of Recall.
 */
static s32b borg_power_home_aux2(void)
{
	int book = 0, realm = 0;
	int pile = 2 * bp_ptr->lev - 1;

	s32b value = 0L;

	/*** Basic abilities ***/

	/* Collect food */
	value += 500 * MIN(num_food, 10);
	value += 50 * MIN_FLOOR(num_food, 10, pile);

	/* Emphasize on collecting scrolls of food above mere rations */
	value += 10 * MIN(num_food_scroll, pile);

	/* Collect ident */
	value += 1500 * MIN(num_ident, 20);
	value += 150 * MIN_FLOOR(num_ident, 20, pile);

	/* Collect *id*ent */
	value += 1700 * MIN(num_star_ident, 10);
	value += 170 * MIN_FLOOR(num_star_ident, 10, pile);

	/* Collect remove curse */
	value += 500 * MIN(num_remove_curse, 5);
	value += 75 * MIN_FLOOR(num_remove_curse, 5, pile);

	/* Collect *remove curse* */
	value += 5000 * MIN(num_star_remove_curse, 5);
	value += 750 * MIN_FLOOR(num_star_remove_curse, 5, pile);

	/* apw Collect glyphs */
	value += 1000 * MIN(num_glyph, pile);

	/* Reward Genocide scrolls. Just scrolls, mainly used for the Serpent */
	value += 1000 * MIN(num_genocide, pile);

	/* Reward Mass Genocide scrolls. Just scrolls, mainly used for Serpent */
	value += 1000 * MIN(num_mass_genocide, pile);

	/* Reward Resistance Potions */
	value += 200 * MIN(num_pot_resist, pile);

	/* Collect recall */
	value += 1700 * MIN(num_recall, 20);
	value += 70 * MIN_FLOOR(num_recall, 20, pile);

	/* Collect phase door */
	value += 1700 * MIN(num_phase, 20);
	value += 70 * MIN_FLOOR(num_phase, 20, pile);

	/* Collect escape */
	value += 3000 * MIN(num_escape, 20);
	value += 300 * MIN_FLOOR(num_escape, 20, pile);

	/* Collect teleport */
	value += 1000 * MIN(num_teleport, 20);
	value += 100 * MIN_FLOOR(num_teleport, 20, pile);

	/* Collect teleport level scrolls */
	value += 1000 * MIN(num_teleport_level, 20);
	value += 100 * MIN_FLOOR(num_teleport_level, 20, pile);

	/* Collect Speed */
	value += 3000 * MIN(num_speed, 20);
	value += 300 * MIN_FLOOR(num_speed, 20, pile);

	/* Collect Berserk */
	value += 400 * MIN(num_berserk, 20);
	value += 40 * MIN_FLOOR(num_berserk, 20, pile);

	/* Collect Invuln Potions (As if you'd ever find so many potions) */
	value += 5000 * MIN(num_goi_pot, pile);

	/* Collect heal */
	value += 3000 * MIN(num_heal, 99);
	value += 8000 * MIN(num_ez_heal, 99);

	/* Potion of Mana */
	if (borg_class != CLASS_WARRIOR)
	{
		value += 2000 * MIN(num_mana, 99);
	}

	/* Collect cure critical */
	value += 3500 * MIN(num_cure_critical, 99);

	/* Collect cure serious - but they aren't as good */
	if (bp_ptr->mhp < 500) value += 400 * MIN(num_cure_serious, 99);

	/* Borgs with low HP collect cure light wounds */
	if (bp_ptr->mhp < 250) value += 200 * MIN(num_cure_light, 99);

	/*** Various ***/

	/* Fixing Stats */
	if (bp_ptr->lev == 50) value += 50L * MIN(num_fix_exp, bp_ptr->lev * 2 -1);
	else
	{
		value += 5000 * MIN(num_fix_exp, 5);
		value += 500 * MIN_FLOOR(num_fix_exp, 5, pile);
	}

	/* Keep shrooms in the house */
	value += 5000 * MIN(num_fix_stat[6], pile);

	/*** Hack -- books ***/

	/* Scan Realms */
	for (realm = 0; realm < MAX_REALM; realm++)
	{
		/* Only my realms */
		if (!borg_has_realm(realm)) continue;

		/* Scan town books */
		for (book = 0; book < 4; book++)
		{
			/* Does the borg have this book yet? */
			if (!num_book[realm][book]) continue;

			/* Does the borg use this book yet? */
			if (!borg_uses_book(realm, book))
			{
				/* Reward keeping the first unused book in the house */
				value += 5000 * MIN(num_book[realm][book], 1);
			}

			/* Is this a town book? */
			if (book < 2 || realm == REALM_ARCANE)
			{
				/* Assign value to keep extra books */
				value += 100 * MIN(num_book[realm][book], pile);
			}
		}
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

	/* Notice changes to the home */
	borg_notice_home();

	/* Process the home equipment */
	value += borg_power_home_aux1();

	/* Process the home inventory */
	value += borg_power_home_aux2();

	/* Return the value */
	return (value);
}









/*
 * Initialize this file
 */
void borg_init_4(void)
{
	/* Make the home inventory array */
	C_MAKE(borg_home, STORE_INVEN_MAX, list_item);
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
