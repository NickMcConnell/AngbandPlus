/* File: obj-info.c */

/*
 * Copyright (c) 2002 Andrew Sidwell, Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"

/* TRUE if a paragraph break should be output before next p_text_out() */
static bool new_paragraph = FALSE;


static void p_text_out(cptr str)
{
	if (new_paragraph)
	{
		text_out("\n\n   ");
		new_paragraph = FALSE;
	}

	text_out(str);
}


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
			p_text_out((i == 1 && i == n - 1) ? " " : ", ");

			if (i == n - 1) p_text_out(conjunction);
		}

		p_text_out(list[i]);
	}
}


static void output_desc_list(cptr intro, cptr list[], int n)
{
	if (n != 0)
	{
		/* Output intro */
		p_text_out(intro);

		/* Output list */
		output_list(list, n);

		/* Output end */
		p_text_out(".  ");
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
		p_text_out(format("It %s all your stats", (o_ptr->pval > 0 ? "increases" : "decreases")));
	}
	else
	{
		p_text_out(format("It %s your ", (o_ptr->pval > 0 ? "increases" : "decreases")));

		/* Output list */
		output_list(descs, cnt);
	}

	/* Output end */
	p_text_out(format(" by %i.  ", pval));

	/* We found something */
	return (TRUE);
}


/*
 * Describe "secondary bonuses" of an item.
 */
static bool describe_secondary(const object_type *o_ptr, u32b f1, u32b f2)
{
	cptr descs[8];
	int cnt = 0;
	int pval = (o_ptr->pval > 0 ? o_ptr->pval : -o_ptr->pval);

	/* Collect */
	if (f1 & (TR1_STEALTH)) descs[cnt++] = "stealth";
/*	if (f1 & (TR1_INFRA))   descs[cnt++] = "alertness"; */
	if (f1 & (TR1_TUNNEL))  descs[cnt++] = "tunneling";
	if (f1 & (TR1_SPEED))   descs[cnt++] = "speed";
	/* Thrown weapons get different note for extra blows */
	if ((f1 & (TR1_BLOWS)) && !(f2 & (TR2_THROWN))) descs[cnt++] = "attack speed";
	if (f1 & (TR1_SHOTS))   descs[cnt++] = "shooting speed";
	if (f1 & (TR1_MIGHT))   descs[cnt++] = "shooting power";

	/* Skip */
	if ((!cnt) && (!f1 & (TR1_INFRA))) return (FALSE);

	/* Start */
	if (cnt)
    {
       p_text_out(format("It %s your ", (o_ptr->pval > 0 ? "increases" : "decreases")));

	   /* Output list */
	   output_list(descs, cnt);

	   /* Output end */
	   p_text_out(format(" by %i.  ", pval));
    }

    /* handle alertness factor */
	if (f1 & (TR1_INFRA))
	{
	   text_out(format(" It %s your alertness", (o_ptr->pval > 0 ? "increases" : "decreases")));
	   text_out(format(" by %i.  ", (pval * 5)));
    }

	/* We found something */
	return (TRUE);
}


/*
 * Describe the special slays and executes of an item.
 */
static bool describe_slay(const object_type *o_ptr, u32b f1, u32b f2)
{
	cptr slays[8], execs[3];
	int slcnt = 0, excnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect slays */
	if (f1 & (TR1_SLAY_ANIMAL)) slays[slcnt++] = "animals";
	if (f1 & (TR1_SLAY_ORC))    slays[slcnt++] = "orcs";
	if (f1 & (TR1_SLAY_TROLL))  slays[slcnt++] = "trolls";
	if (f1 & (TR1_SLAY_GIANT))  slays[slcnt++] = "giants";
	if (f2 & (TR2_SLAY_BUG))    slays[slcnt++] = "bugs";
	if (f2 & (TR2_SLAY_SILVER)) slays[slcnt++] = "silver monsters";
	if (f2 & (TR2_SLAY_LITE))   slays[slcnt++] = "creatures of light";


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
		p_text_out("It slays ");

		/* Output list */
		output_list(slays, slcnt);

		/* Output end (if needed) */
		if (!excnt) p_text_out(".  ");
	}

	if (excnt)
	{
		/* Output intro */
		if (slcnt) p_text_out(", and is especially deadly against ");
		else p_text_out("It is especially deadly against ");

		/* Output list */
		output_list(execs, excnt);

		/* Output end */
		p_text_out(".  ");
	}

	/* We are done here */
	return ((excnt || slcnt) ? TRUE : FALSE);
}


/*
 * Describe elemental brands.
 */
static bool describe_brand(const object_type *o_ptr, u32b f1, u32b f2)
{
	cptr descs[5];
	int cnt = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect brands */
	if (f1 & (TR1_BRAND_ACID)) descs[cnt++] = "acid";
	if (f1 & (TR1_BRAND_ELEC)) descs[cnt++] = "lightning";
	if (f1 & (TR1_BRAND_FIRE)) descs[cnt++] = "fire";
	if (f1 & (TR1_BRAND_COLD)) descs[cnt++] = "frost";
	if (f1 & (TR1_BRAND_POIS)) descs[cnt++] = "poison";
	if (f2 & (TR2_COAT_ACID)) output_desc_list("It is coated with acid", descs, cnt);

	/* Describe brands */
	if (o_ptr->tval == TV_RING) output_desc_list("It brands your melee blows with ", descs, cnt);
	else output_desc_list("It is branded with ", descs, cnt);

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
	if (f2 & (TR2_IM_ELEC)) descs[cnt++] = "electricity";
	if (f2 & (TR2_IM_FIRE)) descs[cnt++] = "fire";
	if (f2 & (TR2_IM_COLD)) descs[cnt++] = "cold";

	/* Describe immunities */
	output_desc_list("It provides immunity to ", descs, cnt);

	/* We are done here */
	return (cnt ? TRUE : FALSE);
}


/*
 * Describe resistances granted by an object.
 */
static bool describe_resist(const object_type *o_ptr, u32b f2, u32b f3, u32b f4)
{
	cptr vp[17];
	int vn = 0;

	/* Unused parameter */
	(void)o_ptr;

	/* Collect resistances */
	if ((f2 & (TR2_RES_ACID)) && !(f2 & (TR2_IM_ACID)))
		vp[vn++] = "acid";
	if ((f2 & (TR2_RES_ELEC)) && !(f2 & (TR2_IM_ELEC)))
		vp[vn++] = "electricity";
	if ((f2 & (TR2_RES_FIRE)) && !(f2 & (TR2_IM_FIRE)))
		vp[vn++] = "fire";
	if ((f2 & (TR2_RES_COLD)) && !(f2 & (TR2_IM_COLD)))
		vp[vn++] = "cold";

	if (f4 & (TR4_RES_POIS))  vp[vn++] = "poison";
	if (f4 & (TR4_RES_FEAR))  vp[vn++] = "fear";
	if (f4 & (TR4_RES_LITE))  vp[vn++] = "light";
	if (f4 & (TR4_RES_DARK))  vp[vn++] = "dark";
	if (f4 & (TR4_RES_BLIND)) vp[vn++] = "blindness";
	if (f4 & (TR4_RES_CONFU)) vp[vn++] = "confusion";
	if (f4 & (TR4_RES_SOUND)) vp[vn++] = "sound";
	if (f4 & (TR4_RES_SHARD)) vp[vn++] = "shards";
	if (f4 & (TR4_RES_NEXUS)) vp[vn++] = "nexus" ;
	if (f4 & (TR4_RES_NETHR)) vp[vn++] = "nether";
	if (f4 & (TR4_RES_CHAOS)) vp[vn++] = "chaos";
	if (f4 & (TR4_RES_DISEN)) vp[vn++] = "disenchantment";
	if (f3 & (TR3_HOLD_LIFE)) vp[vn++] = "life draining";
	if (f4 & (TR4_RES_CHARM)) vp[vn++] = "charming";

	/* Describe resistances */
	output_desc_list("It provides resistance to ", vp, vn);

	/* We are done here */
	return (vn ? TRUE : FALSE);
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
	if (f3 & (TR3_IGNORE_ELEC)) list[n++] = "lightning";
	if (f3 & (TR3_IGNORE_FIRE)) list[n++] = "fire";
	if (f3 & (TR3_IGNORE_COLD)) list[n++] = "cold";

	/* Describe ignores */
	if (n == 4)
		p_text_out("It cannot be harmed by the elements.  ");
	else
		output_desc_list("It cannot be harmed by ", list, -n);

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
		p_text_out("It sustains all your stats.  ");
	else
		output_desc_list("It sustains your ", list, n);

	/* We are done here */
	return (n ? TRUE : FALSE);
}

/*
 * Describe miscellaneous powers such as see invisible, free action,
 * permanent light, etc; also note curses and penalties.
 */
static bool describe_misc_magic(const object_type *o_ptr, u32b f2, u32b f3, u32b f4)
{
	cptr good[6], bad[4];
	int gc = 0, bc = 0;
	bool something = FALSE;

	/* Describe lights */
	if (o_ptr->tval == TV_LITE || (f3 & TR3_LITE))
	{
		bool artifact = artifact_p(o_ptr);
		bool no_fuel = (f3 & TR3_NO_FUEL) ? TRUE : FALSE;
		int rad = 0;

		if (artifact)
			rad = 3;
		else if (o_ptr->tval == TV_LITE)
		{
			rad = 2;

		   if ((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN))
		   {
              if (f3 & TR3_DARKVIS)
              rad--;
           }
        }

		if (f3 & TR3_LITE) rad++;

		p_text_out("It usually provides light of radius ");
		text_out_c(TERM_L_GREEN, format("%d", rad));
		if (no_fuel && !artifact)
			text_out(", and never needs refuelling");
		else if (o_ptr->tval == TV_LITE && o_ptr->sval == SV_LITE_TORCH)
			text_out(", though this is reduced when running of out fuel");
		text_out(".  ");

		something = TRUE;
	}

	/* Collect stuff which can't be categorized */
	if (f3 & (TR3_BLESSED))     good[gc++] = "is blessed by the gods";
	if (f2 & (TR2_LIGHTNESS))   good[gc++] = "is lighter than most armor of its type";
	if (f3 & (TR3_IMPACT))      good[gc++] = "creates earthquakes on impact";
	if (f2 & (TR2_EXTRA_CRIT))  good[gc++] = "increases likelihood of critical hits";
	if (f3 & (TR3_SLOW_DIGEST)) good[gc++] = "slows your metabolism";
	if (f2 & (TR2_NICE))        good[gc++] = "makes animals and light fairies less aggresive";
	if (f3 & (TR3_FEATHER))     good[gc++] = "makes you fall like a feather";
	if (f3 & (TR3_REGEN))       good[gc++] = "speeds your regeneration";
	if (f2 & (TR2_MAGIC_MASTERY))  good[gc++] = "raises your magic device skill";
	if (f3 & (TR3_BR_SHIELD))   good[gc++] = "provides damages reduction against monster breath";
	if (f2 & (TR2_THROWN))      good[gc++] = "is balanced for throwing and can't be used for melee";
	if (f2 & (TR2_RTURN))       good[gc++] = "(usually) returns to your hand when thrown";
	if ((f3 & (TR3_GOOD_WEAP)) && (cp_ptr->spell_book == TV_DARK_BOOK))
	{
       good[gc++] = "is good and hates your black magic";
    }
	else if ((f3 & (TR3_GOOD_WEAP)) && (cp_ptr->spell_book == TV_PRAYER_BOOK))
	{
       good[gc++] = "is good and assists in your prayers";
    }
	else if (f3 & (TR3_GOOD_WEAP))   good[gc++] = "is good and hates evil";
	/* the following maybe should be listed with the penalties instead */
	if (!o_ptr->blessed)
	{
	   if ((f3 & (TR3_BAD_WEAP)) && (cp_ptr->spell_book == TV_DARK_BOOK))
	   {
          good[gc++] = "is evil and assists in your black magic";
       }
	   else if ((f3 & (TR3_BAD_WEAP)) && (cp_ptr->spell_book == TV_PRAYER_BOOK))
	   {
          good[gc++] = "is evil and hinders your prayers";
       }
	   else if (f3 & (TR3_BAD_WEAP))    good[gc++] = "is evil and hates good";
	   if (f2 & (TR2_CORRUPT))     good[gc++] = "corrupts those who wield it for too long";
	   if (((f3 & (TR3_BAD_WEAP)) || (f3 & (TR3_GOOD_WEAP))) && 
       (goodweap > 0) && (badweap > 0))
       {
          good[gc++] = "is in conflict with something else that you're wearing or wielding";
       }
    }

	/* Describe */
	output_desc_list("It ", good, gc);

	/* Set "something" */
	if (gc) something = TRUE;

	/* Collect granted powers */
	gc = 0;
	if (f3 & (TR3_FREE_ACT))  good[gc++] = "immunity to paralysis";
	if (f3 & (TR3_TELEPATHY)) good[gc++] = "the power of telepathy";
	if (f3 & (TR3_TCONTROL))  good[gc++] = "teleport control";
	if (f3 & (TR3_DARKVIS))   good[gc++] = "darkvision";
	if (f3 & (TR3_SEE_INVIS)) good[gc++] = "the ability to see invisible things";

	/* Collect penalties */
	if (f3 & (TR3_AGGRAVATE)) bad[bc++] = "aggravates creatures around you";
	if (f3 & (TR3_DRAIN_EXP)) bad[bc++] = "drains experience";
	if (f3 & (TR3_TELEPORT))  bad[bc++] = "induces random teleportation";
 	if (f3 & (TR3_STOPREGEN)) bad[bc++] = "prevents hit point regeneration";
	if (f2 & (TR2_DANGER))    bad[bc++] = "sometimes hits yourself";
	if (f2 & (TR2_PEACE))     bad[bc++] = "makes you less aggresive in combat";

	/* Deal with cursed stuff */
	if (cursed_p(o_ptr))
	{
		if (f3 & (TR3_PERMA_CURSE)) bad[bc++] = "is heavily cursed and recurses itself";
		else if (f3 & (TR3_HEAVY_CURSE)) bad[bc++] = "is heavily cursed";
		else if (object_known_p(o_ptr)) bad[bc++] = "is cursed";
	}

	/* Describe */
	if (gc)
	{
		/* Output intro */
		p_text_out("It grants you ");

		/* Output list */
		output_list(good, gc);

		/* Output end (if needed) */
		if (!bc) p_text_out(".  ");
	}

	if (bc)
	{
		/* Output intro */
		if (gc) p_text_out(", but it also ");
		else p_text_out("It ");

		/* Output list */
		output_list(bad, bc);

		/* Output end */
		p_text_out(".  ");
	}

	/* Set "something" */
	if (gc || bc) something = TRUE;

	/* weird "constant activation" flag */
	if (f2 & (TR2_CONSTANTA))
	{
		p_text_out("It provides its activation effect constantly while you are wielding it,");
		text_out(" but a charge is used up every time you wield it.  ");
		something = TRUE;
	}

	/* Return "something" */
	return (something);
}


/*
 * Describe an object's activation, if any.
 */
static bool describe_activation(const object_type *o_ptr, u32b f3)
{
	/* Check for the activation flag */
	if (f3 & TR3_ACTIVATE)
	{
		p_text_out("It activates for ");
		describe_item_activation(o_ptr);
		p_text_out(".  ");

		return (TRUE);
	}

	/* No activation */
	return (FALSE);
}

#if 0 /* shouldn't need this */
/*
 * number of blows (from xtra1.c  L~3126)
 */
static int find_blows(const object_type *o_ptr, u32b f1)
{
	int blows, str_index, dex_index, div;

	/* Enforce a minimum "weight" (tenth pounds) */
	div = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

	/* Get the strength vs weight */
	str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * cp_ptr->att_multiply / div);

	/* Maximal value */
	if (str_index > 11) str_index = 11;

	/* Index by dexterity */
	dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

	/* Maximal value */
	if (dex_index > 11) dex_index = 11;

	/* Use the blows table */
	blows = blows_table[str_index][dex_index];

	/* Maximal value */
	if (blows > cp_ptr->max_attacks) p_ptr->num_blow = cp_ptr->max_attacks;

	/* Add in the "bonus blows" */
	if (f1 & (TR1_BLOWS)) blows += o_ptr->pval;

    /* timed extra attack (from spell or mushroom only) */
	if (p_ptr->timed[TMD_XATTACK])
	{
	   blows += 1;
    }
		
	/* peace */
    if (p_ptr->peace) blows -= 1;

	/* Require at least one blow */
	if (blows < 1) blows = 1;
	
	return blows;
}
#endif

/*
 * find chance of critical hit  for describe_attack()
 */
static int crit_chance(const object_type *o_ptr, u32b f2)
{
    int i, bonus;
    int weight = o_ptr->weight;
    int plus = o_ptr->to_h;
	int excrit = 0;
	if (f2 & TR2_EXTRA_CRIT) excrit = 10;
	bonus = p_ptr->to_h + o_ptr->to_h;
	if (bonus-1 > 10) excrit += (bonus-1)/10;
	
	/* hack: crit bonus for stilettos, more for bad classes */
	if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_STILETTO))
	{
       excrit += 2;
       if (cp_ptr->spell_book == TV_DARK_BOOK) excrit += 2;
    }

	/* Extract "blow" power */
	i = weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3);
	if (weight > 150) excrit += 2;
	else if (weight > 90) excrit += 1;
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	i += (goodluck * 3);
	i -= (badluck * 2);
	if (p_ptr->peace) i -= 50;
	
	i = 5000/i;
	return i;
}

/*
 * find chance of critical hit for ranged weapons  for describe_attack()
 */
static int critshot_chance(const object_type *o_ptr, u32b f2)
{
    int i, bonus;
    int weight = o_ptr->weight;
	int excrit = 0;
	if (f2 & TR2_EXTRA_CRIT) excrit = 12;

	if (f2 & TR2_THROWN)
	{
	   bonus = p_ptr->to_h + o_ptr->to_h + p_ptr->skills[SKILL_THT];
	   if (bonus-1 > 12) excrit += (bonus-1)/12;
    }
    else /* missile ammo */
    {
	   /* get the launcher */
       object_type *j_ptr = &inventory[INVEN_BOW];

       bonus = p_ptr->to_h + o_ptr->to_h + j_ptr->to_h;
	   if ((bonus + p_ptr->skills[SKILL_THB])-1 > 12) excrit += ((bonus + p_ptr->skills[SKILL_THB])-1)/12;
    }

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + o_ptr->to_h) * 4) + (p_ptr->lev * 2));
	if (p_ptr->peace) i = ((i * 9) / 10);
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	
	i = 5000/i;
	return i;
}

/* find out if a thrown weapon is getting a brand from a ring */
int thrown_brand(void)
{
    int ringbrand = 0;
    int brandl = 0;
    int brandr = 0;
	u32b f1, f2, f3, f4;

	/* Examine the current rings */
    object_type *j_ptr = &inventory[INVEN_LEFT];
    object_type *r_ptr = &inventory[INVEN_RIGHT];

	/* Extract the flags (left hand ring) */
	object_flags(j_ptr, &f1, &f2, &f3, &f4);

    if (f1 & TR1_BRAND_COLD) brandl = 1;
    if (f1 & TR1_BRAND_FIRE) brandl = 2;
    if (f1 & TR1_BRAND_ACID) brandl = 3;
    if (f1 & TR1_BRAND_ELEC) brandl = 4;
    if (f1 & TR1_BRAND_POIS) brandl = 5;

	/* Extract the flags (right hand ring) */
	object_flags(r_ptr, &f1, &f2, &f3, &f4);

    if (f1 & TR1_BRAND_COLD) brandr = 1;
    if (f1 & TR1_BRAND_FIRE) brandr = 2;
    if (f1 & TR1_BRAND_ACID) brandr = 3;
    if (f1 & TR1_BRAND_ELEC) brandr = 4;
    if (f1 & TR1_BRAND_POIS) brandr = 5;
    
    if (brandr == brandl) ringbrand = brandr;
    else if ((brandl) && (!brandr)) ringbrand = brandl;
    else if ((brandr) && (!brandl)) ringbrand = brandr;
    else if ((brandr) && (brandl)) /* also ugly.. */
    {
       if ((brandl == 1) && (brandr == 2)) ringbrand = 12;
       if ((brandl == 1) && (brandr == 3)) ringbrand = 13;
       if ((brandl == 1) && (brandr == 4)) ringbrand = 14;
       if ((brandl == 1) && (brandr == 5)) ringbrand = 15;
       if ((brandl == 2) && (brandr == 1)) ringbrand = 12;
       if ((brandl == 2) && (brandr == 3)) ringbrand = 23;
       if ((brandl == 2) && (brandr == 4)) ringbrand = 24;
       if ((brandl == 2) && (brandr == 5)) ringbrand = 25;
       if ((brandl == 3) && (brandr == 1)) ringbrand = 13;
       if ((brandl == 3) && (brandr == 2)) ringbrand = 23;
       if ((brandl == 3) && (brandr == 4)) ringbrand = 34;
       if ((brandl == 3) && (brandr == 5)) ringbrand = 35;
       if ((brandl == 4) && (brandr == 1)) ringbrand = 14;
       if ((brandl == 4) && (brandr == 2)) ringbrand = 24;
       if ((brandl == 4) && (brandr == 3)) ringbrand = 34;
       if ((brandl == 4) && (brandr == 5)) ringbrand = 45;
       if ((brandl == 5) && (brandr == 1)) ringbrand = 15;
       if ((brandl == 5) && (brandr == 2)) ringbrand = 25;
       if ((brandl == 5) && (brandr == 3)) ringbrand = 35;
       if ((brandl == 5) && (brandr == 4)) ringbrand = 45;
    }
    
    return ringbrand;
}

/*
 * list[] and mult[] must be > 11 in size (?)
 */
static int collect_slays(const char *desc[], int mult[], u32b f1, u32b f2, bool weapon)
{
	int cnt = 0;

	/* Collect slays */
	if (f1 & TR1_SLAY_ANIMAL) { mult[cnt] = 2; desc[cnt++] = "animals"; }
	if (f1 & TR1_SLAY_EVIL)   { mult[cnt] = 2; desc[cnt++] = "evil creatures"; }

	if (f1 & TR1_SLAY_ORC)    { mult[cnt] = 3; desc[cnt++] = "orcs"; }
	if (f1 & TR1_SLAY_TROLL)  { mult[cnt] = 3; desc[cnt++] = "trolls"; }
	if (f1 & TR1_SLAY_GIANT)  { mult[cnt] = 3; desc[cnt++] = "giants"; }
	if (f1 & TR1_SLAY_DRAGON) { mult[cnt] = 3; desc[cnt++] = "dragons"; }
	if (f1 & TR1_SLAY_DEMON)  { mult[cnt] = 3; desc[cnt++] = "demons"; }
	if (f1 & TR1_SLAY_UNDEAD) { mult[cnt] = 3; desc[cnt++] = "undead"; }
	if (f2 & TR2_SLAY_SILVER) { mult[cnt] = 3; desc[cnt++] = "silver creatures"; }
	if (f2 & TR2_SLAY_BUG)    { mult[cnt] = 3; desc[cnt++] = "bugs"; }
	if (f2 & TR2_SLAY_LITE)   { mult[cnt] = 3; desc[cnt++] = "creatures of light"; }

    if (weapon)
    {
	   if (p_ptr->brand_acid)  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; }
	   if (p_ptr->brand_elec)  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; }
	   if (p_ptr->brand_fire)  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; }
	   if (p_ptr->brand_cold)  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; }
	   if (p_ptr->brand_pois)  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; }
    }
    /* Get brands from rings for thrown weapons (ugly) */
    /* but if I use p_ptr->brand_xxxx it includes the brand from the wielded melee weapon */
    else if (f2 & TR2_THROWN) 
    {
       int ringbrand = thrown_brand();
       if ((ringbrand == 1) || ((ringbrand > 10) && (ringbrand < 20)) || (f1 & TR1_BRAND_COLD))  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; }
       if ((ringbrand == 2) || (ringbrand == 12) || (ringbrand == 23) || (ringbrand == 24) || (ringbrand == 25) || (f1 & TR1_BRAND_FIRE))  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; }
       if ((ringbrand == 3) || (ringbrand == 13) || (ringbrand == 23) || (ringbrand == 34) || (ringbrand == 35) || (f1 & TR1_BRAND_ACID))  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; }
       if ((ringbrand == 4) || (ringbrand == 14) || (ringbrand == 24) || (ringbrand == 34) || (ringbrand == 45) || (f1 & TR1_BRAND_ELEC))  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; }
       if ((ringbrand == 5) || (ringbrand == 15) || (ringbrand == 25) || (ringbrand == 35) || (ringbrand == 45) || (f1 & TR1_BRAND_POIS))  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; }
       /* 1=cold, 2=fire, 3=acid, 4=elec, 5=pois */
       /* 1,2,3,4,5,12,13,14,15,23,24,25,34,35,45*/
    }
    else /* ammo */
    {
	   if (f1 & TR1_BRAND_ACID)  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; }
	   if (f1 & TR1_BRAND_ELEC)  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; }
	   if (f1 & TR1_BRAND_FIRE)  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; }
	   if (f1 & TR1_BRAND_COLD)  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; }
	   if (f1 & TR1_BRAND_POIS)  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; }
    }


	if (f1 & TR1_KILL_DRAGON) { mult[cnt] = 5; desc[cnt++] = "dragons"; }
	if (f1 & TR1_KILL_DEMON)  { mult[cnt] = 5; desc[cnt++] = "demons"; }
	if (f1 & TR1_KILL_UNDEAD) { mult[cnt] = 5; desc[cnt++] = "undead"; }

	return cnt;
}


/*
 * Describe weapon damage
 */
void describe_attack(const object_type *o_ptr)
{
    object_type *j_ptr;
	const char *desc[18];
	bool ammo, weapon, edged;
	int mult[16];
	int dam, cnt, total_dam, critc, ptodam;
	int xtra_dam = 0;
    int strdam = 0;
	int strdec, strb;
    int ringbrand = 0;
    int brandodd = 0;

	/* Grab the object flags */
	u32b f1, f2, f3, f4;
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* get the launcher */
	j_ptr = &inventory[INVEN_BOW];
    
	ammo   = (p_ptr->ammo_tval == o_ptr->tval) && (j_ptr->k_idx);
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	
	if (o_ptr->tval == TV_BOW)
	{
        int multl;
		switch (o_ptr->sval)
		{
			/* ML2 */
            case SV_SLING:
			case SV_SHORT_BOW:
			case SV_MINI_XBOW:
            {
                 multl = 2;
                 break;
            }
			/* ML3 */
			case SV_HANDHELDC:
			case SV_LONG_BOW:
			case SV_LIGHT_XBOW:
            {
                 multl = 3;
                 break;
            }

			/* ML4 */
			case SV_GREAT_BOW:
			case SV_HEAVY_XBOW:
            {
                 multl = 4;
                 break;
            }
        }
		if ((f1 & (TR1_MIGHT)) && (object_known_p(o_ptr))) multl += o_ptr->pval;
        
	    new_paragraph = TRUE;
	    p_text_out(format("A launcher with an multiplier level (ML) of %d ", multl));
	    new_paragraph = FALSE;
	    text_out("actually multiplies damage by ");
	    if (multl == 4) text_out("x3.5\n");
        else if (multl == 3) text_out("x2.625\n");
        else if (multl == 5) text_out("x4.375\n");
        else if (multl > 5) text_out("x5.25\n");
        else /* 2 */ text_out("x1.75\n");
        text_out("Examine ammo to see average damage.\n");
            
        /* no more info for bows */
        return;
    }

    if (weapon)
    {
	   /*
	    * Get the player's hypothetical state, were they to be
	    * wielding this item. (mostly copied from V3.1.0)
	    */
	   object_type inven[INVEN_TOTAL];
	   memcpy(inven, inventory, INVEN_TOTAL * sizeof(object_type));
	   inven[INVEN_WIELD] = *o_ptr;
       calc_bonuses(inven, TRUE);

       /* show number of blows */
	   /* blows = find_blows(o_ptr, f1); */
	   new_paragraph = TRUE;
	   p_text_out("With this weapon, you would currently get ");
	   new_paragraph = FALSE;
	   if (p_ptr->num_blow > 1) text_out(format("%d blows per round.\n", p_ptr->num_blow));
	   else text_out(format("%d blow per round.\n", p_ptr->num_blow));

	   /* shield slot weapons */
       if (f4 & (TR4_WIELD_SHIELD)) text_out("You can wield this weapon in your off hand for defence. ");
       /* maybe later I'll have duel-wielding */
       /* if (f4 & (TR4_WIELD_SHIELD)) text_out("You can wield this weapon in your off hand without special training."); */

	   /* Warn about heavy weapons */
	   if (p_ptr->heavy_wield)
		  text_out_c(TERM_L_RED, "You are too weak to use this weapon effectively.\n");
    }
	else if (f2 & TR2_THROWN)
	{
		int throws = thits_thrown(o_ptr->weight);
		if (throws > 1) text_out(format("  You can throw %d of these weapons in one turn.\n", throws));
	}

    /* describe damage only if identified */
    if (!object_known_p(o_ptr))
    {
        /* done if no edged penalty or isn't a melee weapon */
        if ((!cp_ptr->flags & CF_BLESS_WEAPON) || (!weapon)) return;

        edged = FALSE;
        if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)) edged = TRUE;
        /* don't need ID to get warning about edged penalty */
        if ((f3 & TR3_BAD_WEAP) && (!edged))
        {
           /* icky, but would give away unknown BAD_WEAP flag */
        }
        else if (p_ptr->icky_wield)
        {
           text_out("  This weapon would violate your restriction against edged");
           text_out(" weapons and would hinder your magic.\n");
        }
        return;
    }

    if (weapon)
    {
       dam = ((o_ptr->ds + 1) * o_ptr->dd * 5);
       /* remove strength bonus from xtra_dam */
       ptodam = p_ptr->dis_to_d - ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	   xtra_dam = ptodam * 10;
	   xtra_dam += o_ptr->to_d * 10;
       text_out("Each blow will do an average damage of ");

	   /* complex strength bonus by weight */
       strb = 10 * ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	   if ((o_ptr->weight / 10) < 2) strb = strb / 6;
	   else if ((o_ptr->weight / 10) < 3) strb = strb / 4;
	   else if ((o_ptr->weight / 10) < 4) strb = strb / 2;
	   else if ((o_ptr->weight / 10) < 5) strb = (strb * 2) / 3;
	   else if ((o_ptr->weight / 10) < 6) strb = (strb * 3) / 4;
	   else if ((o_ptr->weight / 10) < 7) strb = (strb * 5) / 6;
	   else if ((o_ptr->weight / 10) > 30) strb = (strb * 12) / 5;
	   else if ((o_ptr->weight / 10) > 26) strb = (strb * 7) / 3;
	   else if ((o_ptr->weight / 10) > 23) strb = (strb * 9) / 4;
	   else if ((o_ptr->weight / 10) > 20) strb = (strb * 13) / 6;
	   else if ((o_ptr->weight / 10) > 17) strb = strb * 2;
	   else if ((o_ptr->weight / 10) > 15) strb = (strb * 7) / 4;
	   else if ((o_ptr->weight / 10) > 12) strb = (strb * 3) / 2;
	   else if ((o_ptr->weight / 10) > 10) strb = (strb * 5) / 4;
	   strdec = (strb / 10);
	   dam += strdec * 10;
	   /* decimal */
	   strb -= strdec * 10;
	   dam += strb;

#if oldbreak
       /* figure strength bonus */
       if ((o_ptr->weight / 10) > 4)
       {
          strdam = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
                   
          /* double strength bonus for very heavy weapons (heavier than 15 lb) */
          if ((o_ptr->weight / 10) > 15) strdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

          dam += strdam * 10;
       }
#endif
       
       /* find chance of critical hit */
       critc = crit_chance(o_ptr, f2);
    }
    else if (f2 & TR2_THROWN)
    {
		/* Calculate damage */
		dam = ((o_ptr->ds + 1) * o_ptr->dd * 5);
		if (object_known_p(o_ptr)) dam += (o_ptr->to_d * 10);

		/* thrown weapon multiplier */
        dam = (dam * 7) / 5;
		
        strdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

	    new_paragraph = TRUE;
		p_text_out("This weapon can be thrown for an average damage of ");
	    new_paragraph = FALSE;
       
        /* find chance of critical hit */
        critc = critshot_chance(o_ptr, f2);
    }
    else /* ammo */
    {
		u32b f[4];
		/* Calculate damage */
		dam = ((o_ptr->ds + 1) * o_ptr->dd * 5);
		if (object_known_p(o_ptr)) dam += (o_ptr->to_d * 10);
		if (object_known_p(j_ptr)) dam += (j_ptr->to_d * 10);

	    /* multiplier is now (tmul * .875) based on x2 to x1.75 */
	    if (p_ptr->ammo_mult == 2) dam = ((dam * 7) / 4);       /* (x1.75) */
	    else if (p_ptr->ammo_mult == 3) dam = ((dam * 21) / 8); /* (x2.625) */
	    else if (p_ptr->ammo_mult == 4) dam = ((dam * 7) / 2);  /* (x3.5) */
	    else if (p_ptr->ammo_mult == 5) dam = ((dam * 35) / 8); /* (x4.375) */
	    else if (p_ptr->ammo_mult > 5) dam = ((dam * 21) / 4);  /* (x5.25) */
		/* dam *= p_ptr->ammo_mult; */

		/* Apply brands from the shooter to the ammo */
		object_flags(j_ptr, &f[0], &f[1], &f[2], &f[3]);
		f1 |= f[0];
		f1 |= f[1];
		
        /* strength bonus to slings for certain races / classes */
		if ((p_ptr->pclass == 3) || (p_ptr->pclass == 10) || (p_ptr->pclass == 18) ||
		   (p_ptr->prace == 3) || (p_ptr->prace == 14))
		{
		   if ((o_ptr->tval == TV_SHOT) && (p_ptr->ammo_mult < 3))
		   {
              strdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
           }
        }

	    new_paragraph = TRUE;
		p_text_out("Fired from your current missile launcher, this missile will ");
	    new_paragraph = FALSE;
		text_out("inflict an average damage of ");
       
        /* find chance of critical hit */
        critc = critshot_chance(o_ptr, f2);
    }

    /* Collect slays */
	cnt = collect_slays(desc, mult, f1, f2, weapon);
	   
	if (cnt)
	{
	    int i;

		for (i = 0; i < cnt; i++)
		{
		   /* Include bonus damage and slay in stated average */
		   int slay_dam = dam * mult[i] + xtra_dam;
    
           /* strength bonus after multipliers for slings and thrown weapons */
           /* but before multipliers for melee weapons (kindof ugly) */
           if (((o_ptr->tval == TV_SHOT) || (f2 & TR2_THROWN)) && (strdam > 0))
              slay_dam += strdam;

		   if (slay_dam <= 0)
			    text_out(format("%d", 0));
		   else if (slay_dam % 10)
				text_out(format("%d.%d", slay_dam / 10, slay_dam % 10));
		   else text_out(format("%d", slay_dam / 10));

		   text_out(format(" against %s, ", desc[i]));
		}

	    text_out("and ");
    }
       
    /* Include bonus damage in stated average */
    total_dam = dam + xtra_dam;
    
    
    /* strength bonus after multipliers for slings and thrown weapons */
    /* but before multipliers for melee weapons (kindof ugly) */
    if (((o_ptr->tval == TV_SHOT) || (f2 & TR2_THROWN)) && (strdam))
       total_dam += strdam;

	if (total_dam <= 0)
	   text_out(format("%d", 0));
	else if (total_dam % 10) text_out(format("%d.%d",
	           total_dam / 10, total_dam % 10));
	else text_out(format("%d", total_dam / 10));

    if (cnt) text_out(" against other monsters");
    if ((weapon) && ((p_ptr->timed[TMD_HIT_ELEMENT]) || (p_ptr->timed[TMD_BALROG])))
       text_out(". This does not figure in 'spririt of the balrog' or 'elemental strike' effects.");
    else text_out(".");
    
    /* note to-hit penalty from TMD_XATTACK */
    if (((ammo) || (f2 & TR2_THROWN)) && (p_ptr->timed[TMD_XATTACK]))
    {
       text_out("  Your to-hit with ranged weapons is currently 1/3 of what it");
       text_out(" usually is because of the magic which is giving you an extra melee blow.");
    }
    
    /* Are you getting an elemental brand from a ring? (this is ugly..) */
    if (f2 & TR2_THROWN) ringbrand = thrown_brand();

    /* Get odds of thrown weapon getting branded by elemental ring */
    if (ringbrand)
    {
       brandodd = ((p_ptr->skills[SKILL_THT] + goodluck) / 2);
       if (p_ptr->lev < 20) brandodd += 5;
       /* if ringbrand > 10 then */
       /* character is wearing more than one elemental ring */
       /* so more likely to get brand from ring */
       if (ringbrand > 10) brandodd += 10;

       /* percentile */
       if (brandodd > 100) brandodd = 100;
    }
    
    if (weapon)
    {
       if (f3 & TR3_BAD_WEAP)
       {
          /* already has a message */
       }
       else if (p_ptr->icky_wield)
       {
          text_out("  This weapon violates your restriction against edged weapons");
          text_out(" and would hinder your magic. ");
       }

       text_out("  Your chance of scoring a critical hit with this weapon is 1 in ");
       text_out(format("%d.\n", critc));
       
       /* re-calulate bonuses with real wielded weapon to prevent wierd messages */
       calc_bonuses(inventory, TRUE);
    }
    else if (f2 & TR2_THROWN)
    {
       text_out("  Your chance of scoring a critical hit when throwing this ");
       text_out(format("weapon is 1 in %d.", critc));
       if (f1 & TR1_BLOWS)
       {
           if (o_ptr->pval > 1) text_out(format("  This weapon attacks your foe %d times when you throw it.", o_ptr->pval + 1));
           else if (o_ptr->pval == 1) text_out("  This weapon attacks your foe twice when you throw it.");
       }
       if (ringbrand)
       {
           text_out("  Thrown weapons often get the brand from an elemental ring");
           text_out(" but not always. It depends on luck and skill with throwing");
           text_out(format(" weapons. You have a %d percent chance of getting the", brandodd));
           text_out(" brand from a ring when throwing this weapon. Note: Weapons");
           text_out(" not meant for throwing never get the brand from a ring.");
       }
       text_out("\n");
    }
    else if (ammo)
    {
       text_out("  Your chance of scoring a critical shot shooting this ammo ");
       text_out(format("from your current launcher is 1 in %d.\n", critc));
    }
    
    return;
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
	if (describe_secondary(o_ptr, f1, f2)) something = TRUE;
	if (describe_slay(o_ptr, f1, f2)) something = TRUE;
	if (describe_brand(o_ptr, f1, f2)) something = TRUE;
	if (describe_immune(o_ptr, f2)) something = TRUE;
    /* partial poison resistance */
	if (f2 & (TR2_PR_POIS))
    {
       p_text_out("It provides partial resistance to poison. ");
       something = TRUE;
    }
	if (describe_resist(o_ptr, f2, f3, f4)) something = TRUE;
	if (describe_sustains(o_ptr, f2)) something = TRUE;
	if (describe_misc_magic(o_ptr, f2, f3, f4)) something = TRUE;
	if (describe_activation(o_ptr, f3)) something = TRUE;
	if (describe_ignores(o_ptr, f3)) something = TRUE;

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (object_known_p(o_ptr) && (!(o_ptr->ident & IDENT_MENTAL)) &&
	    ((o_ptr->xtra1) || artifact_p(o_ptr)))
	{
		/* Hack -- Put this in a separate paragraph if screen dump */
		if (text_out_hook == text_out_to_screen)
			new_paragraph = TRUE;

		p_text_out("It might have hidden powers.");
		something = TRUE;
	}

	/* We are done. */
	return something;
}


/*
 * Header for additional information when printing to screen.
 *
 * Return TRUE if an object description was displayed.
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
	text_out_c(TERM_YELLOW, format("%^s", o_name));

	/* Free up the memory */
	FREE(o_name);

	/* Display the known artifact description */
	if (!adult_randarts && o_ptr->name1 &&
	    object_known_p(o_ptr) && a_info[o_ptr->name1].text)
	{
		p_text_out("\n\n   ");
		p_text_out(a_text + a_info[o_ptr->name1].text);
		has_description = TRUE;
	}

	/* Display the known object description */
	else if (object_aware_p(o_ptr) || object_known_p(o_ptr))
	{
		if (k_info[o_ptr->k_idx].text)
		{
			p_text_out("\n\n   ");
			p_text_out(k_text + k_info[o_ptr->k_idx].text);
			has_description = TRUE;
		}

		/* Display an additional ego-item description */
		if (o_ptr->name2 && object_known_p(o_ptr) && e_info[o_ptr->name2].text)
		{
			p_text_out("\n\n   ");
			p_text_out(e_text + e_info[o_ptr->name2].text);
			has_description = TRUE;
		}
	}

	return (has_description);
}

/* EFGchange notice obvious effects */
typedef struct {u32b flag; char *name;} flagname;
static flagname boostconv[] =
{
	{ TR1_STR,      "strength" },
	{ TR1_INT,      "intelligence" },
	{ TR1_WIS,      "wisdom" },
	{ TR1_DEX,      "dexterity" },
	{ TR1_CON,      "constitution" },
	{ TR1_CHR,      "charisma" },
	{ TR1_STEALTH,  "stealth" }, /* unused */
	{ TR1_SPEED,    "speed" },
	{ TR1_BLOWS,    "blows" },
	{ TR1_SHOTS,    "shots" },
	{ TR1_INFRA,    "alertness" },
};

/* version of function in cmd3.c tweaked for object descriptions
 * (changed msg_format to text_out)
 */
static bool obvious_excellent(const object_type *o_ptr)
{
	bool ret = FALSE;
	int i;
	char *desc;

	/* the player should be informed of items that obviously boost */
	/* ??? should check for tried, print this when "I"nspecting a tried object */

	/* get object name & flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	desc = (o_ptr->pval >= 0) ? "boosts" : "reduces";
	for (i = 0; i < sizeof(boostconv)/sizeof(flagname); i++)
	{
		if (f1 & boostconv[i].flag)
		{
			if (o_ptr->pval > 0) 
			{
				ret = TRUE;
			}
			text_out(format("This item %s your ", desc));
			text_out(format("%s ", boostconv[i].name));
			text_out(format("by %d.", abs(o_ptr->pval)));
		}
	}
	if (f3 & (TR3_ACTIVATE))
	{
		ret = TRUE;
		text_out("This item can be activated.");
	}
	if (f3 & (TR3_DARKVIS))
	{
		ret = TRUE;
		text_out("This item grants darkvision.");
	}
	if (f3 & (TR3_TELEPATHY))
	{
		ret = TRUE;
		text_out("This item provides ESP.");
	}
	if (f3 & (TR3_LITE))
	{
		ret = TRUE;
		text_out("This item provides permanent light.");
	}
	if (f3 & (TR3_DRAIN_EXP))
	{
		text_out("This item drains experience.");
	}
	if (f3 & (TR3_STOPREGEN))
	{
		text_out("This item prevents hit point regeneration.");
	}
	return ret;
}


/*
 * Place an item description on the screen.
 */
void object_info_screen(const object_type *o_ptr)
{
	bool has_description, has_info;
	bool weapon, ammo;
	object_type *j_ptr;
    u32b f1, f2, f3, f4;

	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;

	/* Save the screen */
	screen_save();

	has_description = screen_out_head(o_ptr);

	object_info_out_flags = object_flags_known;

	/* Dump the info */
	new_paragraph = TRUE;
	has_info = object_info_out(o_ptr);
	new_paragraph = FALSE;
	
    if ((!object_known_p(o_ptr)) || (!has_description && !has_info))
       p_text_out("\n\n   ");

	/* note what you found out by trying on an unIDed object */
    if ((o_ptr->pseudo == INSCRIP_TRIED) && (!object_known_p(o_ptr)))
    {
        if (obvious_excellent(o_ptr)) p_text_out("\n\n   ");
    }

	if (!object_known_p(o_ptr))
		text_out("This item has not been identified.");
	else if (!has_description && !has_info)
		text_out("This item does not seem to possess any special abilities.");

	j_ptr = &inventory[INVEN_BOW];
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	ammo   = (p_ptr->ammo_tval == o_ptr->tval) && (j_ptr->k_idx);

	/* get object flags */
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);
	
    /* describe weapon attacks */
    if ((ammo) || (weapon) || (o_ptr->tval == TV_BOW) || (f2 & TR2_THROWN))
    {
	    describe_attack(o_ptr);
    }

    /* describe blessed status */
    if (o_ptr->blessed > 1)
    {
       if (weapon) text_out("  This weapon has been temporarily blessed.\n");
       else if (o_ptr->tval == TV_BOW) text_out("  This bow has been temporarily blessed.\n");
       else text_out("  This object has been temporarily blessed.\n");
    }
    else if (((f3 & (TR3_BAD_WEAP)) || (f2 & (TR2_CORRUPT))) && (o_ptr->blessed))
    {
       text_out("  This object has had an evil enchantment removed from it,");
       text_out(" but the temporary blessing has worn off.\n");
    }

	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");

	/* Wait for input */
	(void)anykey();

	/* Load the screen */
	screen_load();

	/* Hack -- Browse book, then prompt for a command */
	if (o_ptr->tval == cp_ptr->spell_book)
	{
		/* Call the aux function */
		do_cmd_browse_aux(o_ptr);
	}
}
