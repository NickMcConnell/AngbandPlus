/* File: obj-info.c */

/*
 * Copyright (c) 2002 Andrew Sidwell, Robert Ruehlmann
 * 						Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"


static void output_list(cptr list[], int n)
{
	int i;

	cptr conjunction = "and ";
	if (n < 0)
	{
		n = -n;
		conjunction = "or ";
	}


	for (i = 0; i < n; i++)
	{
		if (i != 0)

		{
			text_out((i == 1 && i == n - 1) ? " " : ", ");
			if (i == n - 1)	text_out(conjunction);

		}
		text_out(list[i]);
	}
}


static void output_desc_list(cptr intro, cptr list[], int n)
{
	if (n != 0)
	{
		/* Output intro */
		text_out(intro);

		/* Output list */
		output_list(list, n);

		/* Output end */
		text_out(".  ");
	}
}


/*
 * Describe stat modifications.
 */
static bool describe_stats(const object_type *o_ptr, u32b f1)
{
	cptr descs[A_MAX];
	int cnt = 0;
	int pval = (o_ptr->pval > 0 ? o_ptr->pval : -o_ptr->pval);

	/* Abort if the pval is zero */
	if (!pval) return (FALSE);

	/* Collect stat bonuses */
	if (f1 & (TR1_STR)) descs[cnt++] = stat_names_full[A_STR];
	if (f1 & (TR1_INT)) descs[cnt++] = stat_names_full[A_INT];
	if (f1 & (TR1_WIS)) descs[cnt++] = stat_names_full[A_WIS];
	if (f1 & (TR1_DEX)) descs[cnt++] = stat_names_full[A_DEX];
	if (f1 & (TR1_CON)) descs[cnt++] = stat_names_full[A_CON];
	if (f1 & (TR1_CHR)) descs[cnt++] = stat_names_full[A_CHR];

	/* Skip */
	if (cnt == 0) return (FALSE);

	/* Shorten to "all stats", if appropriate. */
	if (cnt == A_MAX)
	{
		text_out(format("It %s all your stats", (o_ptr->pval > 0 ? "increases" : "decreases")));
	}
	else
	{
		text_out(format("It %s your ", (o_ptr->pval > 0 ? "increases" : "decreases")));

		/* Output list */
		output_list(descs, cnt);
	}

	/* Output end */
	text_out(format(" by %i.  ", pval));

	/* We found something */
	return (TRUE);
}


/*
 * Describe "secondary bonuses" of an item.
 */
static bool describe_secondary(const object_type *o_ptr, u32b f1)
{
	cptr descs[8];
	int cnt = 0;
	int pval = (o_ptr->pval > 0 ? o_ptr->pval : -o_ptr->pval);

	/* Collect */
	if (f1 & (TR1_STEALTH)) descs[cnt++] = "stealth";
	if (f1 & (TR1_SEARCH))  descs[cnt++] = "searching";
	if (f1 & (TR1_INFRA))   descs[cnt++] = "infravision";
	if (f1 & (TR1_TUNNEL))  descs[cnt++] = "tunneling";
	if (f1 & (TR1_SPEED))   descs[cnt++] = "speed";
	if (f1 & (TR1_BLOWS))   descs[cnt++] = "attack speed";
	if (f1 & (TR1_SHOTS))   descs[cnt++] = "shooting speed";
	if (f1 & (TR1_MIGHT))   descs[cnt++] = "shooting power";

	/* Skip */
	if (!cnt) return (FALSE);

	/* Start */
	text_out(format("It %s your ", (o_ptr->pval > 0 ? "increases" : "decreases")));

	/* Output list */
	output_list(descs, cnt);

	/* Output end */
	text_out(format(" by %i.  ", pval));

	/* We found something */
	return (TRUE);
}

/*
 * Describe the special slays and executes of an item.
 */
static bool describe_slay(const object_type *o_ptr, u32b f1)
{
	cptr slays[8], execs[3];
	int slcnt = 0, excnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect brands */
	if (f1 & (TR1_SLAY_ANIMAL)) slays[slcnt++] = "animals";
	if (f1 & (TR1_SLAY_ORC))    slays[slcnt++] = "orcs";
	if (f1 & (TR1_SLAY_TROLL))  slays[slcnt++] = "trolls";
	if (f1 & (TR1_SLAY_GIANT))  slays[slcnt++] = "giants";

	/* Dragon slay/execute */
	if (f1 & TR1_KILL_DRAGON)
		execs[excnt++] = "dragons";
	else if (f1 & TR1_SLAY_DRAGON)
		slays[slcnt++] = "dragons";

	/* Demon slay/execute */
	if (f1 & TR1_KILL_DEMON)
		execs[excnt++] = "demons";
	else if (f1 & TR1_SLAY_DEMON)
		slays[slcnt++] = "demons";

	/* Undead slay/execute */
	if (f1 & TR1_KILL_UNDEAD)
		execs[excnt++] = "undead";
	else if (f1 & TR1_SLAY_UNDEAD)
		slays[slcnt++] = "undead";

	if (f1 & (TR1_SLAY_EVIL)) slays[slcnt++] = "all evil creatures";

	/* Describe */
	if (slcnt)
	{
		/* Output intro */
		text_out("It slays ");

		/* Output list */
		output_list(slays, slcnt);

		/* Output end (if needed) */
		if (!excnt) text_out(".  ");
	}

	if (excnt)
	{
		/* Output intro */
		if (slcnt) text_out(", and it is especially deadly against ");
		else text_out("It is especially deadly against ");

		/* Output list */
		output_list(execs, excnt);

		/* Output end */
		text_out(".  ");
	}

	/* We are done here */
	return ((excnt || slcnt) ? TRUE : FALSE);
}


/*
 * Describe elemental brands.
 */
static bool describe_brand(const object_type *o_ptr, u32b f1)
{
	cptr descs[5];
	int cnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect brands */
	if (f1 & (TR1_BRAND_ACID)) descs[cnt++] = "acid";
	if (f1 & (TR1_BRAND_ELEC)) descs[cnt++] = "electricity";
	if (f1 & (TR1_BRAND_FIRE)) descs[cnt++] = "fire";
	if (f1 & (TR1_BRAND_COLD)) descs[cnt++] = "frost";
	if (f1 & (TR1_BRAND_POIS)) descs[cnt++] = "poison";

	/* Describe brands */
	output_desc_list("It is branded with ", descs, cnt);

	/* We are done here */
	return (cnt ? TRUE : FALSE);
}


/*
 * Describe immunities granted by an object.
 *
 * ToDo - Merge intro describe_resist() below.
 */
static bool describe_immune(const object_type *o_ptr, u32b f2)
{
	cptr descs[5];
	int cnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect immunities */
	if (f2 & (TR2_IM_ACID)) descs[cnt++] = "acid";
	if (f2 & (TR2_IM_ELEC)) descs[cnt++] = "lightning";
	if (f2 & (TR2_IM_FIRE)) descs[cnt++] = "fire";
	if (f2 & (TR2_IM_COLD)) descs[cnt++] = "cold";
	if (f2 & (TR2_IM_POIS)) descs[cnt++] = "poison";

	/* Describe immunities */
	output_desc_list("It provides immunity to ", descs, cnt);

	/* We are done here */
	return (cnt ? TRUE : FALSE);
}


/*
 * Describe resistances granted by an object.
 */
static bool describe_resist(const object_type *o_ptr, u32b f2, u32b f3)
{
	cptr vp[17];
	int vn = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect resistances */
	if ((f2 & (TR2_RES_ACID)) && !(f2 & (TR2_IM_ACID)))
		vp[vn++] = "acid";
	if ((f2 & (TR2_RES_ELEC)) && !(f2 & (TR2_IM_ELEC)))
		vp[vn++] = "lightning";
	if ((f2 & (TR2_RES_FIRE)) && !(f2 & (TR2_IM_FIRE)))
		vp[vn++] = "fire";
	if ((f2 & (TR2_RES_COLD)) && !(f2 & (TR2_IM_COLD)))
		vp[vn++] = "cold";
	if ((f2 & (TR2_RES_POIS)) && !(f2 & (TR2_IM_POIS)))
		vp[vn++] = "poison";

	if (f2 & (TR2_RES_FEAR))  vp[vn++] = "fear";
	if (f2 & (TR2_RES_LIGHT))  vp[vn++] = "light";
	if (f2 & (TR2_RES_DARK))  vp[vn++] = "dark";
	if (f2 & (TR2_RES_BLIND)) vp[vn++] = "blindness";
	if (f2 & (TR2_RES_CONFU)) vp[vn++] = "confusion";
	if (f2 & (TR2_RES_SOUND)) vp[vn++] = "sound";
	if (f2 & (TR2_RES_SHARD)) vp[vn++] = "shards";
	if (f2 & (TR2_RES_NEXUS)) vp[vn++] = "nexus" ;
	if (f2 & (TR2_RES_NETHR)) vp[vn++] = "nether";
	if (f2 & (TR2_RES_CHAOS)) vp[vn++] = "chaos";
	if (f2 & (TR2_RES_DISEN)) vp[vn++] = "disenchantment";
	if (f3 & (TR3_HOLD_LIFE)) vp[vn++] = "life draining";

	/* Describe resistances */
	output_desc_list("It provides resistance to ", vp, vn);

	/* We are done here */
	return (vn ? TRUE : FALSE);
}



/*return the number of blows a player gets with a weapon*/
int get_num_blows(const object_type *o_ptr, u32b f1, int str_add, int dex_add)
{

	int i, str_index, dex_index, blows, div_weight;

	int str = 0;
	int dex = 0;

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b wf1, wf2, wf3, wfn;
		object_type *i_ptr = &inventory[i];

		/* Hack -- do not apply wielded "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags_known(i_ptr, &wf1, &wf2, &wf3, &wfn);

		/* Affect stats */
		if (wf1 & (TR1_STR)) str_add += i_ptr->pval;
		if (wf1 & (TR1_DEX)) dex_add += i_ptr->pval;
	}

	/* add in the strength of the examined weapon*/
	if (f1 & (TR1_STR)) str_add += o_ptr->pval;

	/* add in the dex of the examined weapon*/
	if (f1 & (TR1_DEX)) dex_add += o_ptr->pval;

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, use, ind;

		/*stat modifiers from equipment later*/
		if 		(i == A_STR) add = str_add;
		else if (i == A_DEX) add = dex_add;
		/*only do dex and strength*/
		else continue;

		/* Maximize mode */
		if (adult_maximize)
		{
			/* Modify the stats for race/class */
			add += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], add);

		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/*record the values*/
		if (i == A_STR) str = ind;
		else dex = ind;
	}

	/* Enforce a minimum "weight" (tenth pounds) */
	div_weight = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

	/* Get the strength vs weight */
	str_index = (adj_str_blow[str] * cp_ptr->att_multiply / div_weight);

	/* Maximal value */
	if (str_index > 11) str_index = 11;
	if (str_index < 0) str_index = 0;

	/* Index by dexterity */
	dex_index = (adj_dex_blow[dex]);

	/* Maximal value */
	if (dex_index > 11) dex_index = 11;
	if (dex_index < 0) dex_index = 0;

	/* Use the blows table */
	blows = blows_table[str_index][dex_index];

	/* Maximal value */
	if (blows > cp_ptr->max_attacks) blows = cp_ptr->max_attacks;

	/* Add in the "bonus blows"*/
	if (f1 & (TR1_BLOWS)) blows += o_ptr->pval;

	/* Require at least one blow */
	if (blows < 1) blows = 1;

	/*add extra attack for those who have the flag*/
	if ((p_ptr->lev >= LEV_EXTRA_COMBAT) && (cp_ptr->flags & CF_EXTRA_ATTACK))
	{
		blows += 1;
	}

	return(blows);
}

/*
 * Describe 'number of attacks recieved with a weapon
 */
static bool describe_attacks (const object_type *o_ptr, u32b f1, bool extra_info)
{

	int old_blows, new_blows, i;
	int n = o_ptr->tval;
	int str_plus = 0;
	int dex_plus = 0;
	int str_done = -1;
	int average;
	int percent;

	/* Obtain the "hold" value */
	int hold = adj_str_hold[p_ptr->state.stat_ind[A_STR]];

	/* Calculate str and dex adjustments */
	int to_dam = ((int)(adj_str_td[p_ptr->state.stat_ind[A_STR]]) - 128);

	int to_hit = ((int)(adj_dex_th[p_ptr->state.stat_ind[A_DEX]]) - 128);
	to_hit 	  += ((int)(adj_str_th[p_ptr->state.stat_ind[A_STR]]) - 128);

	/* First check if we need this function */
	if ((n != TV_DIGGING) && (n != TV_HAFTED) &&
		(n != TV_POLEARM) && (n != TV_SWORD))
	{
		return (FALSE);
	}

	/* No descriptions of quest items */
	if (o_ptr->ident & IDENT_QUEST) return (FALSE);

	/*get the number of blows*/
	n = get_num_blows(o_ptr, f1, 0, 0);

	/*print out the number of attacks*/
	if (n == 1) text_out("It gives you one attack per turn.  ");
	else text_out(format("It gives you %d attacks per turn.  ", n));

	old_blows = n;

	text_out("\n");

	if (!extra_info) return (TRUE);

	/* Apply the mental bonuses tp hit/damage, if known */
	if (object_known_p(o_ptr))
	{
		to_dam += o_ptr->to_d;
		to_hit += o_ptr->to_h;
	}

	/* Scan the equipment, skipping the weapon and bow slot */
	for (i = INVEN_LEFT; i < INVEN_TOTAL; i++)
	{
		object_type *j_ptr = &inventory[i];
		u32b jf1, jf2, jf3, jfn;

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Only known */
		if (!object_known_p(o_ptr)) continue;

		/* Extract the item flags */
		object_flags(j_ptr, &jf1, &jf2, &jf3, &jfn);

		/* Apply the bonuses to hit/damage */
		to_dam += j_ptr->to_d;
		to_hit += j_ptr->to_h;
	}

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!p_ptr->state.bless_blade) &&
		((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		text_out("You are uncomfortable wielding this weapon.  ");

		to_dam -= 2;
		to_hit -= 2;
	}

	text_out("\n\n");

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight / 10)
	{
		text_out("You have trouble wielding this weapon.  ");
		to_hit += 2 * (hold - o_ptr->weight / 10);
	}

	text_out("\n");

	/* Now calculate and print out average damage */
	text_out("Average base damage/hit: ");

	average = (o_ptr->dd * o_ptr->ds) / 2 + to_dam;

	if (average < 0) text_out_c(TERM_L_RED, "%d", 0);
	else text_out_c(TERM_L_GREEN, "%d", average);
	text_out(" based on a damage calculation of %dd%d", o_ptr->dd, o_ptr->ds);

	if (to_dam < 0) text_out_c(TERM_L_RED, " - %d", (0 - to_dam));
	else if (to_dam > 0)text_out_c(TERM_L_GREEN, " + %d", to_dam);
	text_out(" damage.");

	text_out("\n\n");

	if (f1 & (TR1_KILL_MASK | TR1_SLAY_MASK))
	{
		text_out("This damage calculation can be increased due to its special slaying abilities.");
		text_out("\n\n");
	}
	if (f1 & (TR1_BRAND_MASK))
	{
		text_out("This damage calculation can be increased due to elemental branding.");
		text_out("\n\n");
	}


	percent = ((o_ptr->weight + ((to_hit + o_ptr->to_h) * 5) + (p_ptr->lev * 3)) * 100) / 5000;
	text_out("You have a %d%% chance of inflicting extra damage with each hit from this weapon.", percent);
	text_out("\n\n");

	/* Then we check for extra "real" blows */
	for (dex_plus = 0; dex_plus < 8; dex_plus++)
	{
		for (str_plus = 0; str_plus < 8; str_plus++)
		{

			new_blows = get_num_blows(o_ptr, f1, str_plus, dex_plus);

			/* Test to make sure that this extra blow is a
			 * new str/dex combination, not a repeat
			 */
			if ((new_blows > old_blows) &&
				((str_plus < str_done) || (str_done == -1)))
			{
				text_out("With an additional %d str and %d dex it gives you %d attacks per turn.\n",
						str_plus, dex_plus, new_blows);
				str_done = str_plus;
				break;
			}
		}
	}

	return (TRUE);
}



/*
 * Describe 'ignores' of an object.
 */
static bool describe_ignores(const object_type *o_ptr, u32b f3)
{
	cptr list[4];
	int n = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect the ignores */
	if (f3 & (TR3_IGNORE_ACID)) list[n++] = "acid";
	if (f3 & (TR3_IGNORE_ELEC)) list[n++] = "electricity";
	if (f3 & (TR3_IGNORE_FIRE)) list[n++] = "fire";
	if (f3 & (TR3_IGNORE_COLD)) list[n++] = "cold";

	/* Describe ignores */
	if (n == 4)
		text_out("It cannot be harmed by the elements.  ");
	else
		output_desc_list("It cannot be harmed by ", list, - n);

	return ((n > 0) ? TRUE : FALSE);
}


/*
 * Describe stat sustains.
 */
static bool describe_sustains(const object_type *o_ptr, u32b f2)
{
	cptr list[A_MAX];
	int n = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect the sustains */
	if (f2 & (TR2_SUST_STR)) list[n++] = stat_names_full[A_STR];
	if (f2 & (TR2_SUST_INT)) list[n++] = stat_names_full[A_INT];
	if (f2 & (TR2_SUST_WIS)) list[n++] = stat_names_full[A_WIS];
	if (f2 & (TR2_SUST_DEX)) list[n++] = stat_names_full[A_DEX];
	if (f2 & (TR2_SUST_CON)) list[n++] = stat_names_full[A_CON];
	if (f2 & (TR2_SUST_CHR)) list[n++] = stat_names_full[A_CHR];

	/* Describe immunities */
	if (n == A_MAX)
		text_out("It sustains all your stats.  ");
	else
		output_desc_list("It sustains your ", list, n);

	/* We are done here */
	return (n ? TRUE : FALSE);
}


/*
 * Describe miscellaneous powers such as see invisible, free action,
 * permanent light, etc; also note curses and penalties.
 */
static bool describe_misc_magic(const object_type *o_ptr, u32b f3)
{
	cptr good[7], bad[4];
	int gc = 0, bc = 0;
	bool something = FALSE;

	/* Throwing weapons. */
	if (f3 & (TR3_THROWING))
	{
		if (o_ptr->ident & IDENT_PERFECT_BALANCE)
		{
			good[gc++] = ("can be thrown hard and fast");
		}
		else good[gc++] = ("can be thrown effectively");
	}

	/* Collect stuff which can't be categorized */
	if (f3 & (TR3_BLESSED))     good[gc++] = "is blessed by the gods";
	if (f3 & (TR3_IMPACT))      good[gc++] = "creates earthquakes on impact";
	if (f3 & (TR3_SLOW_DIGEST)) good[gc++] = "slows your metabolism";
	if (f3 & (TR3_FEATHER))     good[gc++] = "makes you fall like a feather";
	if (((o_ptr->tval == TV_LIGHT) && artifact_p(o_ptr)) || (f3 & (TR3_LIGHT)))
		good[gc++] = "lights the dungeon around you";
	if (f3 & (TR3_REGEN))       good[gc++] = "speeds your regeneration";

	/* Describe */
	output_desc_list("It ", good, gc);

	/* Set "something" */
	if (gc) something = TRUE;

	/* Collect granted powers */
	gc = 0;
	if (f3 & (TR3_FREE_ACT))  good[gc++] = "immunity to paralysis";
	if (f3 & (TR3_TELEPATHY)) good[gc++] = "the power of telepathy";
	if (f3 & (TR3_SEE_INVIS)) good[gc++] = "the ability to see invisible things";

	/* Collect penalties */
	if (f3 & (TR3_AGGRAVATE)) bad[bc++] = "aggravates creatures around you";
	if (f3 & (TR3_DRAIN_EXP)) bad[bc++] = "drains experience";
	if (f3 & (TR3_TELEPORT))  bad[bc++] = "induces random teleportation";

	/* Deal with cursed stuff */
	if (cursed_p(o_ptr))
	{
		if (f3 & (TR3_PERMA_CURSE)) bad[bc++] = "is permanently cursed";
		else if (f3 & (TR3_HEAVY_CURSE)) bad[bc++] = "is heavily cursed";
		else if (object_known_p(o_ptr)) bad[bc++] = "is cursed";
	}

	/* Describe */
	if (gc)
	{
		/* Output intro */
		text_out("It grants you ");

		/* Output list */
		output_list(good, gc);

		/* Output end (if needed) */
		if (!bc) text_out(".  ");
	}

	if (bc)
	{
		/* Output intro */
		if (gc) text_out(", but it also ");
		else text_out("It ");

		/* Output list */
		output_list(bad, bc);

		/* Output end */
		text_out(".  ");
	}

	/* Set "something" */
	if (gc || bc) something = TRUE;

	/* Return "something" */
	return (something);
}


static cptr act_description[ACT_MAX] =
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
static void describe_item_activation(const object_type *o_ptr, char *random_name, size_t max)
{
	u32b f1, f2, f3, fn;

	u16b value;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Require activation ability */
	if (!(f3 & TR3_ACTIVATE)) return;

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
			else return;
		}

		/* Some artifacts can be activated */
		if (!drag_armor)
		{
			my_strcat(random_name, act_description[a_ptr->activation], max);

			/* Output the number of turns */
			if (a_ptr->time && a_ptr->randtime)
				my_strcat(random_name, format(" every %d+d%d turns", a_ptr->time, a_ptr->randtime), max);
			else if (a_ptr->time)
				my_strcat(random_name, format(" every %d turns", a_ptr->time), max);
			else if (a_ptr->randtime)
				my_strcat(random_name, format(" every d%d turns", a_ptr->randtime), max);

			return;
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
				my_strcat(random_name, "acid resistance (20+d20 turns) and acid ball (70) every 50+d50 turns", max);
				break;
			}
			case SV_RING_FLAMES:
			{
				my_strcat(random_name, "fire resistance (20+d20 turns) and fire ball (80) every 50+d50 turns", max);
				break;
			}
			case SV_RING_ICE:
			{
				my_strcat(random_name, "cold resistance (20+d20 turns) and cold ball (75) every 50+d50 turns", max);
				break;
			}

			case SV_RING_LIGHTNING:
			{
				my_strcat(random_name, "electricity resistance (20+d20 turns) and electricity ball (85) every 50+d50 turns", max);
				break;
			}
		}

		return;
	}

	/* Require dragon scale mail */
	if ((o_ptr->tval != TV_DRAG_ARMOR) &&
		(o_ptr->tval != TV_DRAG_SHIELD)) return;

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

			my_strcat(random_name, format("electricity resistance (10+d10 turns) and breathe lightning (%d) every %d+d%d turns", value, value, value), max);

			break;
		}
		case EGO_DRAGON_WHITE:
		{
			value *= 50;

			my_strcat(random_name, format("cold resistance (10+d10 turns) and breathe frost (%d) every %d+d%d turns", value, value, value), max);

			break;
		}
		case EGO_DRAGON_BLACK:
		{
			value *= 50;

			my_strcat(random_name, format("acid resistance (10+d10 turns) and breathe acid (%d) every %d+d%d turns", value, value, value), max);

			break;
		}
		case EGO_DRAGON_GREEN:
		{
			value *= 50;

			my_strcat(random_name, format("poison resistance (10+d10 turns) and breathe poison gas (%d) every %d+d%d turns", value, value, value), max);

			break;
		}
		case EGO_DRAGON_RED:
		{
			value *= 50;

			my_strcat(random_name, format("fire resistance (10+d10 turns) and breathe fire (%d) every %d+d%d turns", value, value, value), max);

			break;
		}
		case EGO_DRAGON_MULTIHUED:
		{
			value *= 75;

			my_strcat(random_name, format("resistance (20+d20 turns) and breathe multi-hued (%d) every %d+d%d turns", value,
 							(value * 3 / 4), (value * 3 / 4)), max);

			break;
		}
		case EGO_DRAGON_BRONZE:
		{
			value *= 50;

			my_strcat(random_name, format("breathe confusion (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_GOLD:
		{
			value *= 50;

			my_strcat(random_name, format("breathe sound (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_CHAOS:
		{
			value *= 60;

			my_strcat(random_name, format("breathe chaos/disenchant (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_LAW:
		{
			value *= 60;

			my_strcat(random_name, format("breathe sound/shards (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_BALANCE:
		{
			value *= 75;

			my_strcat(random_name, format("breathe balance (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_PSEUDO:
		{
			value *= 65;

			my_strcat(random_name, format("breathe light/darkness (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		case EGO_DRAGON_POWER:
		{
			value *= 100;

			my_strcat(random_name, format("breathe the elements (%d) every %d+d%d turns", value, value, value), max);
			break;
		}
		default:
		{
			break;
		}
	}
}

/*
 * Describe object nativity.
 */
static bool describe_nativity(const object_type *o_ptr, u32b fn)
{
	cptr vp[NUM_NATIVE];
	int vn = 0;

	/* Unused parameter */
	(void)o_ptr;

	if (fn & (TN1_NATIVE_LAVA))  vp[vn++] = "lava";
	if (fn & (TN1_NATIVE_ICE))  vp[vn++] = "ice";
	if (fn & (TN1_NATIVE_OIL))  vp[vn++] = "oil";
	if (fn & (TN1_NATIVE_FIRE)) vp[vn++] = "fire";
	if (fn & (TN1_NATIVE_SAND)) vp[vn++] = "sand";
	if (fn & (TN1_NATIVE_FOREST)) vp[vn++] = "forest";
	if (fn & (TN1_NATIVE_WATER)) vp[vn++] = "water";
	if (fn & (TN1_NATIVE_ACID)) vp[vn++] = "acid" ;
	if (fn & (TN1_NATIVE_MUD)) vp[vn++] = "mud";

	/* Describe nativities */
	output_desc_list("It makes you native to terrains made of ", vp, vn);

	/* We are done here */
	return (vn ? TRUE : FALSE);
}




/*
 * Describe an object's activation, if any.
 */
static bool describe_activation(const object_type *o_ptr, u32b f3)
{
	/* Check for the activation flag */
	if (f3 & TR3_ACTIVATE)
	{
		char act_desc[120];

		u16b size;

		my_strcpy(act_desc, "It activates for ", sizeof(act_desc));

		/*get the size of the file*/
		size = strlen(act_desc);

		describe_item_activation(o_ptr, act_desc, sizeof(act_desc));

		/*if the previous function added length, we have an activation, so print it out*/
		if (strlen(act_desc) > size)
		{

			my_strcat(act_desc, format(".  "), sizeof(act_desc));

			/*print it out*/
			text_out(act_desc);

			return (TRUE);
		}
	}

	/* No activation */
	return (FALSE);
}


/*
 * Output object information
 * With extra_info true, it gives information about the weapon attack damage.
 * False is intended for things like character dumps.
 */
bool object_info_out(const object_type *o_ptr,  bool extra_info)
{
	u32b f1, f2, f3, fn;
	bool something = FALSE;

	/* Grab the object flags */
	object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

	/* Describe the object */
	if (describe_stats(o_ptr, f1)) something = TRUE;

	if (describe_secondary(o_ptr, f1)) something = TRUE;
	if (describe_slay(o_ptr, f1)) something = TRUE;
	if (describe_brand(o_ptr, f1)) something = TRUE;
	if (describe_immune(o_ptr, f2)) something = TRUE;
	if (describe_resist(o_ptr, f2, f3)) something = TRUE;
	if (describe_sustains(o_ptr, f2)) something = TRUE;
	if (describe_misc_magic(o_ptr, f3)) something = TRUE;
	if (describe_nativity(o_ptr, fn)) something = TRUE;
	if (describe_activation(o_ptr, f3)) something = TRUE;
	if (describe_ignores(o_ptr, f3)) something = TRUE;
	if (describe_attacks(o_ptr, f1, extra_info)) something = TRUE;

	/* Unknown extra powers (artifact) */
	if (object_known_p(o_ptr) && (!(o_ptr->ident & IDENT_MENTAL)) &&
	    (object_has_hidden_powers(o_ptr) || artifact_p(o_ptr)))
	{
		/* Hack -- Put this in a separate paragraph if screen dump */
		if (something && text_out_hook == text_out_to_screen)
		{
			text_out("\n\n   ");
		}

		text_out("It might have hidden powers.");
 		something = TRUE;

	}

	/* We are done. */
	return something;
}



/*
 * Header for additional information when printing to screen.
 */
bool screen_out_head(const object_type *o_ptr)
{
	char *o_name;
	int name_size = Term->wid;

	bool has_description = FALSE;

	/* Allocate memory to the size of the screen */
	o_name = C_RNEW(name_size, char);

	/* Description */
	object_desc(o_name, name_size, o_ptr, ODESC_PREFIX | ODESC_SINGULAR | ODESC_FULL);

	/* Print, in colour */
	text_out_c(TERM_YELLOW, "%^s", o_name);

	/* Free up the memory */
	FREE(o_name);

	/* Display the known artifact description */
	if (!adult_rand_artifacts && o_ptr->art_num &&
	    object_known_p(o_ptr) && a_info[o_ptr->art_num].text)

	{
		text_out("\n\n   ");
		text_out(a_text + a_info[o_ptr->art_num].text);
		has_description = TRUE;
	}
	/* Display the known object description */
	else if (object_aware_p(o_ptr) || object_known_p(o_ptr))
	{
		if (k_info[o_ptr->k_idx].text)
		{
			text_out("\n\n   ");
			text_out(k_text + k_info[o_ptr->k_idx].text);
			has_description = TRUE;
		}

		/* Display an additional ego-item description */
		if (o_ptr->ego_num && object_known_p(o_ptr) && e_info[o_ptr->ego_num].text)
		{
			text_out("\n\n   ");
			text_out(e_text + e_info[o_ptr->ego_num].text);
			has_description = TRUE;
		}
	}

	return (has_description);
}


/*
 * Place an item description on the screen.
 */
void object_info_screen(const object_type *o_ptr)
{
	bool has_description, has_info;

	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;

	/* Save the screen */
	screen_save();

	has_description = screen_out_head(o_ptr);

	object_info_out_flags = object_flags_known;

	text_out("\n\n   ");

	/* Dump the info */
	has_info = object_info_out(o_ptr, TRUE);

	if (!object_known_p(o_ptr))
	{
		text_out("\n\n   This item has not been identified.");
	}
	else if ((!has_description) && (!has_info) && (o_ptr->tval != cp_ptr->spell_book))
	{
		text_out("\n\n   This item does not seem to possess any special abilities.");
	}

	if (o_ptr->tval != cp_ptr->spell_book)
	{
		char buf[200];
		int price;
		s16b weight;

		/* Show object history if possible */
		if (format_object_history(buf, sizeof(buf), o_ptr))
		{
			text_out("\n\n   ");
			text_out_c(TERM_YELLOW, buf);
		}

		weight = o_ptr->weight * o_ptr->number;
		if (o_ptr->number > 1)
		{
			text_out_c(TERM_YELLOW, "\n\n   They weigh %1d.%1d lb.", weight / 10, weight % 10);
		}
		else text_out_c(TERM_YELLOW, "\n\n   It weighs %1d.%1d lb.", weight / 10, weight % 10);

		/* Print resale value */
		text_out("\n\n   ");
		price = object_value(o_ptr);
		if (price > 0)
		{
			if (o_ptr->number > 1)
			{
				text_out_c(TERM_YELLOW, "They would fetch %i gold apiece in an average shop.", price);
			}
			else
			{
				text_out_c(TERM_YELLOW, "It would fetch %i gold in an average shop.", price);
			}
		}
		else
		{
			if (o_ptr->number > 1)	text_out_c(TERM_YELLOW, "They have no value.");
			else 					text_out_c(TERM_YELLOW, "It has no value.");
		}

		text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");

		/* Wait for input */
		(void)inkey();
	}

	/* Load the screen */
	screen_load();

	/* Hack -- Browse book, then prompt for a command */
	if (o_ptr->tval == cp_ptr->spell_book)
	{
		/* Call the aux function */
		do_cmd_browse_aux(o_ptr);
	}

	return;
}

/* Append the depth of an history item to the given buffer */
static void history_depth(char *buf, size_t max, s16b depth)
{
	if (depth == 0)
	{
		my_strcat(buf, " in the town.", max);
	}
	else my_strcat(buf, format(" at a depth of %d ft.", depth * 50), max);

}

/*
 * Format the item history as text and put it on the given buffer
 */
bool format_object_history(char *buf, size_t max, const object_type *o_ptr)
{
	*buf = '\0';

	if (o_ptr->number > 1) my_strcat(buf, "They were", max);
	else my_strcat(buf, "It was", max);

	switch (o_ptr->origin_nature)
	{
		case ORIGIN_NONE: case ORIGIN_MIXED:
		{
			/* Don't display anything */
			return (FALSE);
		}
		case ORIGIN_BIRTH:
		{
			my_strcat(buf, " an inheritance from your family.", max);
			break;
		}
		case ORIGIN_STORE:
		{
			/* Hack -- Don't display store objects inside stores */
			if (o_ptr->ident & (IDENT_STORE)) return (FALSE);

			my_strcat(buf, " bought in a store.", max);
			break;
		}
		case ORIGIN_MORGOTH:
		{
			my_strcpy(buf, "It is one of your prizes for victory!", max);
			break;
		}
		case ORIGIN_CHEAT:
		{
			my_strcpy(buf, "-- Created by debug option --", max);
			break;
		}
		case ORIGIN_FLOOR:
		{
			my_strcat(buf, " lying on the floor", max);
			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_ACQUIRE:
		{
			my_strcat(buf, " conjured forth by magic", max);
			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_CHEST:
		{
			my_strcat(buf, " found in a chest", max);
			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_DROP_UNKNOWN:
		{
			my_strcat(buf, " dropped by an unknown monster", max);
			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_REWARD:
		{
			my_strcat(buf, " a reward for your exploits", max);
			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_DROP_KNOWN:
		{
			monster_race *r_ptr = &r_info[o_ptr->origin_r_idx];
			cptr name = r_name + r_ptr->name;
			cptr article = "";

			/* Get an article for non-uniques */
			if (!(r_ptr->flags1 & (RF1_UNIQUE)))
			{
				article = (my_is_vowel(name[0]) ? "an " : "a ");
			}

			/* Stored name */
			if (o_ptr->origin_m_name)
			{
				name = quark_str(o_ptr->origin_m_name);
			}

			my_strcat(buf, format(" dropped by %s%s", article, name), max);

			history_depth(buf, max, o_ptr->origin_dlvl);
			break;
		}
	}

	return (TRUE);
}




/*
 * Check whether the history is interesting
 */
bool history_interesting(const object_type *o_ptr)
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
