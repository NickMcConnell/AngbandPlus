/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* obj-info.c: object information dump */

#include "posband.h"

static void output_list(cptr list[], int n)
{
	int i;

	for (i = 0; i < n; i++)
	{
		text_out(list[i]);

		if (i < (n - 2))
		{
			text_out(", ");
		}
		else if (i < (n - 1))
		{
			if (n > 2) text_out(",");

			text_out(" and ");
		}
	}
}


static void output_desc_list(cptr intro, cptr list[], int n)
{
	if (n > 0)
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
	if (!o_ptr->pval) return (FALSE);

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
	
	if (!o_ptr->pval) return (FALSE);

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
	cptr descs[4];
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

	if (f2 & (TR2_RES_POIS))  vp[vn++] = "poison";
	if (f2 & (TR2_RES_FEAR))  vp[vn++] = "fear";
	if (f2 & (TR2_RES_LITE))  vp[vn++] = "light";
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
static int get_num_blows(const object_type *o_ptr, u32b f1)
{
	int i, str_index, dex_index, blows, div_weight;

	int str = 0;
	int dex = 0;

	monster_race *r_ptr = &r_info[p_ptr->m_r_idx];

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, use, ind;

		/*only do dex and strength*/
		if ((i != A_STR) && (i != A_DEX)) continue;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

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

	/* Scan the equipment */
	for (i = INVEN_EQUIP; i < INVEN_TOTAL; i++)
	{

		u32b wf1, wf2, wf3, wf4;
		object_type *i_ptr = &inventory[i];

		/* Hack -- do not apply wielded "weapon" bonuses */
		if (r_ptr->body.weapon_mask & EQUIP_SLOT(i)) continue;

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags_known(i_ptr, &wf1, &wf2, &wf3, &wf4);

		/* Affect stats */
		if (wf1 & (TR1_STR)) str += i_ptr->pval;
		if (wf1 & (TR1_DEX)) dex += i_ptr->pval;

	}

	/* Enforce a minimum "weight" (tenth pounds) */
	div_weight = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

	/* add in the strength of the examined weapon*/
	if (f1 & (TR1_STR)) str += o_ptr->pval;

	/* Maximal and maximal value */
	if (str > 37) str = 37;
	if (str < 0) str = 0;

	/* Get the strength vs weight */
	str_index = (adj_str_blow[str] * cp_ptr->att_multiply / div_weight);

	/* Maximal value */
	if (str_index > 11) str_index = 11;
	if (str_index < 0) str_index = 0;

	/* add in the dex of the examined weapon*/
	if (f1 & (TR1_DEX)) dex += o_ptr->pval;

	/* Maximal and maximal value */
	if (dex > 37) dex = 37;
	if (dex < 0) dex = 0;

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
	if ((p_ptr->lev > 25) && (cp_ptr->flags & CF_EXTRA_ATTACK))
		blows += 1;

	return(blows);
}

/*
 * Describe 'number of attacks recieved with a weapon
 */
static bool describe_attacks (const object_type *o_ptr, u32b f1)
{
	int n = o_ptr->tval, i, nwpns = 0;
	monster_race *r_ptr = &r_info[p_ptr->m_r_idx];
	
	/* Ensure that we can use this weapon! */
	/* Hack -- count the weapon slots */
	for (i = INVEN_EQUIP; i < INVEN_TOTAL; i++)
	{
	    	if (r_ptr->body.weapon_mask & EQUIP_SLOT(i))
		{
			nwpns++;
		}
	}
	if (!nwpns) return (FALSE);

	if ((n == TV_DIGGING) || (n == TV_HAFTED) ||
		(n == TV_POLEARM) || (n == TV_SWORD))
	{
		/*get the number of attacks*/
		n = get_num_blows(o_ptr, f1);

		/*print out the number of attacks*/
		if (n == 1) text_out("It gives you one attack per turn.  ");
		else text_out(format("It gives you %d attacks per turn.  ", n));

		return (TRUE);
	}

	return (FALSE);
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
		output_desc_list("It cannot be harmed by ", list, n);

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
static bool describe_misc_magic(const object_type *o_ptr, u32b f3, u32b f4)
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
	if (f3 & (TR3_INVISIBILITY)) good[gc++] = "turns you invisible";
	if (f3 & (TR3_BLESSED))     good[gc++] = "is blessed by the gods";
	if (f3 & (TR3_IMPACT))      good[gc++] = "creates earthquakes on impact";
	if (f3 & (TR3_SLOW_DIGEST)) good[gc++] = "slows your metabolism";
	if (f3 & (TR3_FEATHER))     good[gc++] = "allows you to levitate";
	if (((o_ptr->tval == TV_LITE) && artifact_p(o_ptr)) || (f3 & (TR3_LITE)))
		good[gc++] = "lights the dungeon around you";
	if (f4 & (TR4_SUPER_REGEN)) good[gc++] = "enormously speeds your regeneration";
	else if (f3 & (TR3_REGEN))       good[gc++] = "speeds your regeneration";
	if (f4 & (TR4_AURA_FEAR)) good[gc++] = "surrounds you with an aura of despair";

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

/*
 * Describe weird properties granted by an object.
 */
static bool describe_weird(const object_type *o_ptr, u32b f4)
{
	cptr descs[10];
	int cnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect flags */
	if (f4 & (TR4_ACID_TOUCH)) descs[cnt++] = "very acidic";
	if (f4 & (TR4_ELEC_TOUCH)) descs[cnt++] = "shocking";
	if (f4 & (TR4_FIRE_TOUCH)) descs[cnt++] = "extremely hot";
	if (f4 & (TR4_COLD_TOUCH)) descs[cnt++] = "extremely cold";
	if (f4 & (TR4_PASS_WALL)) descs[cnt++] = "immaterial";
	if (f4 & (TR4_UNDEAD_RACE)) descs[cnt++] = "undead";

	/* Describe immunities */
	output_desc_list("It renders your body ", descs, cnt);

	/* We are done here */
	return (cnt ? TRUE : FALSE);
}



/*
 * Describe an object's activation, if any.
 */
static bool describe_activation(const object_type *o_ptr, u32b f3)
{
	/* Check for the activation flag */
	if (f3 & TR3_ACTIVATE)
	{
		text_out("It activates for ");
		describe_item_activation(o_ptr);
		text_out(".  ");

		return (TRUE);
	}

	/* No activation */
	return (FALSE);
}


/*
 * Output object information
 */
bool object_info_out(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;
	bool something = FALSE;

	/* Grab the object flags */
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Describe the object */
	if (describe_stats(o_ptr, f1)) something = TRUE;
	if (describe_secondary(o_ptr, f1)) something = TRUE;
	if (describe_slay(o_ptr, f1)) something = TRUE;
	if (describe_brand(o_ptr, f1)) something = TRUE;
	if (describe_immune(o_ptr, f2)) something = TRUE;
	if (describe_resist(o_ptr, f2, f3)) something = TRUE;
	if (describe_sustains(o_ptr, f2)) something = TRUE;
	if (describe_misc_magic(o_ptr, f3, f4)) something = TRUE;
	if (describe_weird(o_ptr, f4)) something = TRUE;
	if (describe_activation(o_ptr, f3)) something = TRUE;
	if (describe_ignores(o_ptr, f3)) something = TRUE;
	if (describe_attacks(o_ptr, f1)) something = TRUE;

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (object_known_p(o_ptr) && (!(o_ptr->ident & IDENT_MENTAL)) &&
	    ((o_ptr->xtra1) || artifact_p(o_ptr)))
	{
		/* Hack -- Put this in a separate paragraph if screen dump */
		if (something && text_out_hook == text_out_to_screen)
			text_out("\n\n   ");

		text_out("It might have hidden powers.");
 		something = TRUE;

	}

	/* We are done. */
	return something;
}


/*
 * Header for additional information when printing to screen.
 *
 * Header for additional information when printing to screen.
 */
static bool screen_out_head(const object_type *o_ptr)
{
	char *o_name;
	int name_size = Term->wid;

	bool has_description = FALSE;

	/* Allocate memory to the size of the screen */
	o_name = C_RNEW(name_size, char);

	/* Description */
	object_desc(o_name, name_size, o_ptr, TRUE, 3);

	/* Print, in colour */
	text_out_c(TERM_YELLOW, format("%^s\n\n   ", o_name));

	/* Free up the memory */
	FREE(o_name);

	/* Display the known artifact description */
	if (!adult_rand_artifacts && o_ptr->name1 &&
	    object_known_p(o_ptr) && a_info[o_ptr->name1].text)

	{
		text_out(a_text + a_info[o_ptr->name1].text);
		text_out("\n\n   ");
		has_description = TRUE;
	}
	
	/* Display random artifact description */
	if (o_ptr->rart_desc && object_known_p(o_ptr))

	{
		text_out(quark_str(o_ptr->rart_desc));
		text_out("\n\n   ");
		has_description = TRUE;
	}

	/* Display the known unique artifact description */
	if (o_ptr->name3 &&
	    object_known_p(o_ptr) && u_info[o_ptr->name3].text)

	{
		text_out(u_text + u_info[o_ptr->name3].text);
		text_out("\n\n   ");
		has_description = TRUE;
	}

	/* Display the known object description */
	else if ((object_aware_p(o_ptr) || object_known_p(o_ptr)) && !o_ptr->rart_name)
	{
		if (k_info[o_ptr->k_idx].text)
		{
			text_out(k_text + k_info[o_ptr->k_idx].text);
			text_out("\n\n   ");
			has_description = TRUE;
		}

		/* Display an additional ego-item description */
		if (o_ptr->name2 && object_known_p(o_ptr) && e_info[o_ptr->name2].text)
		{
			text_out(e_text + e_info[o_ptr->name2].text);
			text_out("\n\n   ");
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

	/* Dump the info */
	has_info = object_info_out(o_ptr);

	if (!object_known_p(o_ptr))
	{
		if (has_info)
			text_out("\n\n   ");
		text_out("This item has not been identified.");
		has_info = TRUE;

	}
	else if (!has_description && !has_info)
	{
		text_out("This item does not seem to possess any special abilities.");
	}

	/* Descriptions end with "\n\n   ", other info does not */
	if (has_description && !has_info)
	{
		/* Back up over the "   " at the beginning of the line */
		int x, y;
		Term_locate(&x, &y);
		Term_gotoxy(0, y);
	}
	else
	{
		text_out("\n\n");
	}

	text_out_c(TERM_L_BLUE, "[Press any key to continue]\n");

	/* Wait for input */
	(void)inkey();

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

