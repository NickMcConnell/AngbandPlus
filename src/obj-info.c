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
static bool describe_secondary(const object_type *o_ptr, u32b f1, u32b f2, u32b f3)
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
	if ((f1 & (TR1_BLOWS)) && !(f3 & (TR3_THROWN))) descs[cnt++] = "attack speed";
	if (f1 & (TR1_SHOTS))   descs[cnt++] = "shooting speed";
	if (f1 & (TR1_MIGHT))   descs[cnt++] = "shooting power";

	/* Skip */
	if ((!cnt) && (!f1 & (TR1_INFRA)) && (!f1 & (TR1_MAGIC_MASTERY)) && (!f3 & (TR3_THROWMULT)) &&
       (!f1 & (TR1_EQLUCK)))
		return (FALSE);

	/* Start */
	if (cnt)
    {
       p_text_out(format("It %s your ", (o_ptr->pval > 0 ? "increases" : "decreases")));

	   /* Output list */
	   output_list(descs, cnt);

	   /* Output end */
	   p_text_out(format(" by %i.  ", pval));
    }
    else
    {
	   p_text_out("  ");
    }

	/* throwing power */
	if (f3 & (TR3_THROWMULT))
	{
		int tpval = pval;
		if (tpval < 2) tpval = 1;
		p_text_out(format("It %s your throwing power", (o_ptr->pval > 0 ? "increases" : "decreases")));
		p_text_out(format(" by %i.  ", (tpval)));

		/* message about gauntlets of throwing (or randart gloves with THROWMULT) */
		if ((wield_slot(o_ptr) == INVEN_HANDS) && (o_ptr->to_d))
		{
			p_text_out("It also applies its damage bonus to throwing weapons instead of melee.  ");
		}
	}

    /* Lucky or unlucky equipment (separate because it always 'increases') */
	if (f1 & (TR1_EQLUCK))
    {
		p_text_out(format("It increases your %s", (o_ptr->pval > 0 ? "good luck" : "bad luck")));
		p_text_out(format(" by %i.  ", pval));
    }

    /* handle alertness factor */
	if (f1 & (TR1_INFRA))
	{
		p_text_out(format("It %s your alertness", (o_ptr->pval > 0 ? "increases" : "decreases")));
		p_text_out(format(" by %i.  ", (pval * 5)));
    }

	/* magic mastery (factor of 8 unless pval is 1) */
	if (f1 & (TR1_MAGIC_MASTERY))
	{
		if (o_ptr->pval == 1)
		{
			p_text_out(" It increases your magic device skill by 9.  ");
		}
		else
		{
			p_text_out(format(" It %s your magic device skill", (o_ptr->pval > 0 ? "increases" : "decreases")));
			p_text_out(format(" by %i.  ", (pval * 8)));
		}
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
	if (f2 & (TR2_SLAY_ANIMAL)) slays[slcnt++] = "animals";
	if (f1 & (TR1_SLAY_ORC))    slays[slcnt++] = "orcs";
	if (f1 & (TR1_SLAY_TROLL))  slays[slcnt++] = "trolls";
	if (f1 & (TR1_SLAY_GIANT))  slays[slcnt++] = "giants";
	if (f1 & (TR1_SLAY_BUG))    slays[slcnt++] = "bugs";
	if (f1 & (TR1_SLAY_WERE))   slays[slcnt++] = "creatures vulnerable to silver";
	if (f1 & (TR1_SLAY_SILVER)) slays[slcnt++] = "silver monsters";
	if (f1 & (TR1_SLAY_LITE))   slays[slcnt++] = "creatures of light";


	/* Dragon slay/execute */
	if (f2 & TR2_KILL_DRAGON)
		execs[excnt++] = "dragons";
	else if (f1 & TR1_SLAY_DRAGON)
		slays[slcnt++] = "dragons";

	/* Demon slay/execute */
	if (f2 & TR2_KILL_DEMON)
		execs[excnt++] = "demons";
	else if (f1 & TR1_SLAY_DEMON)
		slays[slcnt++] = "demons";

	/* Undead slay/execute */
	if (f2 & TR2_KILL_UNDEAD)
		execs[excnt++] = "undead";
	else if (f1 & TR1_SLAY_UNDEAD)
		slays[slcnt++] = "undead";

	if (f1 & (TR1_SLAY_EVIL)) slays[slcnt++] = "all evil creatures";

	/* Describe */
	if (slcnt)
	{
		/* Output intro */
		if (o_ptr->tval == TV_LITE) p_text_out("It causes you to slay ");
		else p_text_out("It slays ");

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
	bool permbrand = FALSE;
	bool tbrand = FALSE;

	if ((f1 & (TR1_BRAND_ACID)) || (f1 & (TR1_BRAND_ELEC)) ||
		(f1 & (TR1_BRAND_FIRE)) || (f1 & (TR1_BRAND_COLD)) ||
		(f1 & (TR1_BRAND_POIS))) permbrand = TRUE;

	/* Collect brands */
	if (f1 & (TR1_BRAND_ACID)) descs[cnt++] = "acid";
	if (f1 & (TR1_BRAND_ELEC)) descs[cnt++] = "lightning";
	if (f1 & (TR1_BRAND_FIRE)) descs[cnt++] = "fire";
	if (f1 & (TR1_BRAND_COLD)) descs[cnt++] = "frost";
	if (f1 & (TR1_BRAND_POIS)) descs[cnt++] = "poison";

	/* Describe brands */
	if (o_ptr->tval == TV_RING) output_desc_list("It brands your melee blows with ", descs, cnt);
	else output_desc_list("It is branded with ", descs, cnt);
	
	/* temporary branding spells */
	if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
	{
		if (permbrand) p_text_out("It is also enchanted with a temporary ");
		else p_text_out("It is enchanted with a temporary ");
        if (o_ptr->thisbrand == 1) text_out("frost ");
		else if (o_ptr->thisbrand == 2) text_out("fire ");
		else if (o_ptr->thisbrand == 3) text_out("lightning ");
		else if (o_ptr->thisbrand == 4) text_out("acid ");
		else if (o_ptr->thisbrand == 5) text_out("poison ");
		text_out("brand.  ");
		tbrand = TRUE;
	}

	/* We are done here */
	return ((cnt || tbrand) ? TRUE : FALSE);
}


/*
 * Describe immunities granted by an object.
 *
 * ToDo - Merge into describe_resist() below.
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
	if (f4 & (TR4_RES_STATC)) vp[vn++] = "static";
	if (f4 & (TR4_RES_SLIME)) vp[vn++] = "slime";
	if (f4 & (TR4_RES_SILVR)) vp[vn++] = "silver";

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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	bool weapon, armor = FALSE;
	if ((wield_slot(o_ptr) >= INVEN_BODY) && (wield_slot(o_ptr) <= INVEN_FEET))
		armor = TRUE;
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);

	/* Describe lights */
	if (o_ptr->tval == TV_LITE || (f3 & TR3_LITE))
	{
		bool artifact = artifact_p(o_ptr);
		bool no_fuel = (f3 & TR3_NO_FUEL) ? TRUE : FALSE;
		int rad = 0;

		if (o_ptr->tval == TV_LITE)
		{
			rad = 2;
			if (artifact) rad = 3;

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
	/* don't use the LIGHTNESS flag because randarts don't */
	if ((armor) && (o_ptr->weight < k_ptr->weight))
								good[gc++] = "is lighter than most armor of its type";
	if (f2 & (TR2_IMPACT))      good[gc++] = "creates earthquakes on impact";
	if (f2 & (TR2_EXTRA_CRIT))  good[gc++] = "increases likelihood of critical hits";
	if (f3 & (TR3_SLOW_DIGEST)) good[gc++] = "slows your metabolism";
	if (f4 & (TR4_NICE))        good[gc++] = "makes animals and light fairies less aggresive";
	if (f3 & (TR3_FEATHER))     good[gc++] = "makes you fall like a feather";
	if (f3 & (TR3_REGEN))       good[gc++] = "speeds your regeneration";
	if (f3 & (TR3_BR_SHIELD))   good[gc++] = "provides damage reduction against monster breath";
	if (f3 & (TR3_THROWN))      good[gc++] = "is balanced for throwing and can't be used for melee";
	/* not as good as primary throwing weapons, but can be wielded for melee (spear, dagger..) */
	else if ((weapon) && (f3 & (TR3_PTHROW))) good[gc++] = "is better for throwing than most melee weapons";
	/* else if (f3 & (TR3_PTHROW)) good[gc++] = "can be thrown for damage"; (redundant) */
	if (f3 & (TR3_RTURN))       good[gc++] = "(usually) returns to your hand when thrown";
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
	   if ((f3 & (TR3_BAD_WEAP)) && (goodweap))
       {
          good[gc++] = "conflicts with something that you're wearing or wielding";
       }
    }
   if ((f3 & (TR3_GOOD_WEAP)) && (badweap))
   {
      good[gc++] = "conflicts with something that you're wearing or wielding";
   }

	/* Describe */
	output_desc_list("It ", good, gc);

	/* Set "something" */
	if (gc) something = TRUE;

	/* Collect granted powers */
	gc = 0;
	if (f3 & (TR3_TELEPATHY)) good[gc++] = "the power of telepathy";
	if (f3 & (TR3_TCONTROL))  good[gc++] = "teleport control";
	if (f3 & (TR3_DARKVIS))   good[gc++] = "darkvision";
	if (f3 & (TR3_FREE_ACT))  good[gc++] = "immunity to paralysis";
	if (f3 & (TR3_SEE_INVIS)) good[gc++] = "the ability to see invisible things";

	/* Collect penalties */
 	if (f2 & (TR2_R_ANNOY))   bad[bc++] = "does something annoying every now and then";
	if (f2 & (TR2_AGGRAVATE)) bad[bc++] = "aggravates creatures around you";
	if (f2 & (TR2_DRAIN_EXP)) bad[bc++] = "drains experience";
	if (f2 & (TR2_TELEPORT))  bad[bc++] = "induces random teleportation";
 	if (f2 & (TR2_STOPREGEN)) bad[bc++] = "prevents hit point regeneration";
	if (f2 & (TR2_DANGER))    bad[bc++] = "sometimes causes you to hit yourself";
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

	/* let the reader know that golems are too heavy for feather falling */
	if ((f3 & (TR3_FEATHER)) && (p_ptr->prace == 16))
	{
		text_out(" (but feather falling has no effect for golems.) ");
	}
    
	if (o_ptr->esprace)
	{
		bool showraceesp = FALSE;
		/* on artifacts and *Slay* egos, racial ESP is obvious */
		if (((o_ptr->name2 >= EGO_KILL_ANIMAL) && (o_ptr->name2 <= EGO_KILL_DRAGON)) ||
			(o_ptr->name2 == EGO_SLAY_TWO) || (artifact_p(o_ptr)))
		{
			showraceesp = TRUE;
		}
		/* otherwise it's hidden */
		else if (o_ptr->ident & IDENT_MENTAL)
		{
			showraceesp = TRUE;
		}
		
		if (showraceesp)
		{
			cptr espfoo = NULL;
			switch (o_ptr->esprace)
			{
				case 1: { espfoo = "orcs"; break; }
				case 2: { espfoo = "trolls"; break; }
				case 3: { espfoo = "giants"; break; }
				case 4: { espfoo = "undead"; break; }
				case 5: { espfoo = "dragons"; break; }
				case 6: { espfoo = "demons"; break; }
				case 7: { espfoo = "light fairies"; break; }
				case 8: { espfoo = "bugs"; break; }
				case 9: { espfoo = "silver monsters"; break; }
				case 10: { espfoo = "most animals"; break; }
				case 11: { espfoo = "constructs"; break; }
				case 12: { espfoo = "dwarves and dwarvish ghosts"; break; }
				case 13: { espfoo = "dark elves and other servants of Achrya"; break; }
				case 16: { espfoo = "creatures vulnerable to silver"; break; }
				case 70: { espfoo = "all monsters (even mindless ones)"; break; }
				/* special for Sting */
				case 15: { espfoo = "orcs and spiders"; break; }
				/* special for Ratagast */
				case 71: { espfoo = "evil wizards"; break; }
			}
			/* special for Thranduil (replaces full TELEPATHY) */
			/* description refers to a forest domain */
			if (o_ptr->esprace == 75)
			{
				p_text_out(" It senses the presence of animals, bugs, orcs, centaurs, ");
				text_out("fairies, tree monsters, and some other forest-dwelling monsters.  ");
			}
			else if (o_ptr->esprace)
			{
				text_out(format(" It senses the presence of %s.  ", espfoo));
			}
		}
	}

	/* Double weapon */
	if (o_ptr->sbdd)
	{
		p_text_out("It is a double weapon (easier to get multiple attacks,");
		text_out(" especially if you are not using a shield).  ");
		something = TRUE;
	}
	/* weird "constant activation" flag */
	if (f2 & (TR2_CONSTANTA))
	{
		p_text_out("It provides its activation effect constantly while you are wielding it,");
		text_out(" but a charge is used up every time you wield it.  ");
		something = TRUE;
	}
    
    /* describe exploding ammo */
    if (f4 & TR4_EXPLODE_A)
    {
       text_out("  This projectile (almost always) explodes in a radius 2-3 ");
       text_out("ball of shards (or its element if it is branded) when it hits a ");
       text_out("monster, damaging any creatures adjacent to the target monster.  ");
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

#if 0
/*
 * number of blows (from xtra1.c  L~3126)
 * (don't need this)
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
	int excrit = o_ptr->crc - 5;
	if (f2 & TR2_EXTRA_CRIT) excrit += 10;
	bonus = p_ptr->to_h + o_ptr->to_h;
	if (bonus-1 > 10) excrit += (bonus-1)/10;

	/* Extract "blow" power */
	i = weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3);
	if (weight > 150) excrit += 2;
	else if (weight > 90) excrit += 1;
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	else if (excrit < 0) i -= excrit * (10 + (badluck/3));
	else
	{
		i += (goodluck * 3);
		i -= (badluck * 2);
	}
	if (p_ptr->peace) i -= 50;
	
	i = 5000/i;
	return i;
}

/*
 * find chance of critical hit for ranged weapons  for describe_attack()
 */
static int critshot_chance(const object_type *o_ptr, u32b f2, u32b f3)
{
    int i, bonus;
    int weight = o_ptr->weight;
	int excrit = o_ptr->crc - 5;

	if ((f3 & TR3_THROWN) || (f3 & TR3_PTHROW))
	{
		if (f2 & TR2_EXTRA_CRIT) excrit += 12;
		bonus = p_ptr->to_h + p_ptr->skills[SKILL_THT];
	    if (f3 & TR3_THROWN) bonus += o_ptr->to_h;
	    else /* (f3 & TR3_PTHROW) */ bonus += (o_ptr->to_h + 1) / 2;
	    if (bonus-1 > 12) excrit += (bonus-1)/12;
    }
    else /* missile ammo */
    {
	   /* get the launcher */
       object_type *j_ptr = &inventory[INVEN_BOW];

       if (f2 & TR2_EXTRA_CRIT) excrit += 11;
	   bonus = p_ptr->to_h + o_ptr->to_h + j_ptr->to_h;
	   if ((bonus + p_ptr->skills[SKILL_THB])-1 > 12) excrit += ((bonus + p_ptr->skills[SKILL_THB])-1)/12;
    }
		
	/* sniper's eye: only happens if target is visible, but we'll include it here anyway */
    if (p_ptr->timed[TMD_SNIPER]) excrit += 3;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + o_ptr->to_h) * 4) + (p_ptr->lev * 2));
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	else if (excrit < 0) i -= excrit * (10 + (badluck/3));
	else
	{
		i += (goodluck * 3);
		i -= (badluck * 2);
	}
	if (p_ptr->peace) i = ((i * 9) / 10);
	
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
    object_type *r_ptr = &inventory[INVEN_RIGHT]; /* r_ptr should be for monster race only XX */

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
 * collect slays  for describe_attack()
 * list[] and mult[] must be > 11 in size (?)
 */
static int collect_slays(const char *desc[], int mult[], u32b f1, u32b f2, u32b f3, u32b f4, bool weapon, int tbrand)
{
	int cnt = 0;
	int isbrand = 0;
	bool hasslay = FALSE;
	bool exslay = FALSE;

	/* Collect x2 slays */
	/* exception for grenades of Slay animal */
	/* hacky magical mult value 92 means x2.5 multiplier */
	if ((f4 & TR4_EXPLODE_A) && (f2 & TR2_SLAY_ANIMAL) && (f3 & TR3_THROWN)) 
    	{ mult[cnt] = 92; desc[cnt++] = "animals"; exslay = TRUE; }
	else if (f2 & TR2_SLAY_ANIMAL) { mult[cnt] = 2; desc[cnt++] = "animals"; exslay = TRUE; }
	if (f1 & TR1_SLAY_EVIL)   { mult[cnt] = 2; desc[cnt++] = "evil creatures"; exslay = TRUE; }

	/* Collect x3 slays */
	if (f1 & TR1_SLAY_WERE)   { mult[cnt] = 3; desc[cnt++] = "creatures vulnerable to silver"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_ORC)    { mult[cnt] = 3; desc[cnt++] = "orcs"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_TROLL)  { mult[cnt] = 3; desc[cnt++] = "trolls"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_GIANT)  { mult[cnt] = 3; desc[cnt++] = "giants"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_DRAGON) { mult[cnt] = 3; desc[cnt++] = "dragons"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_DEMON)  { mult[cnt] = 3; desc[cnt++] = "demons"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_UNDEAD) { mult[cnt] = 3; desc[cnt++] = "undead"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_SILVER) { mult[cnt] = 3; desc[cnt++] = "silver creatures"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_BUG)    { mult[cnt] = 3; desc[cnt++] = "bugs"; hasslay = TRUE; }
	if (f1 & TR1_SLAY_LITE)   { mult[cnt] = 3; desc[cnt++] = "creatures of light"; hasslay = TRUE; }

    if (weapon)
    {
	   if ((f1 & TR1_BRAND_ACID) || (tbrand == 4))  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; isbrand++; }
	   else if (p_ptr->brand_acid)  { mult[cnt] = 2; desc[cnt++] = "acid-vulnerable creatures"; }
	   if ((f1 & TR1_BRAND_ELEC) || (tbrand == 3))  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; isbrand++; }
	   else if (p_ptr->brand_elec)  { mult[cnt] = 2; desc[cnt++] = "electricity-vulnerable creatures"; }
	   if ((f1 & TR1_BRAND_FIRE) || (tbrand == 2))  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; isbrand++; }
	   else if (p_ptr->brand_fire)  { mult[cnt] = 2; desc[cnt++] = "fire-vulnerable creatures"; }
	   if ((f1 & TR1_BRAND_COLD) || (tbrand == 1))  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; isbrand++; }
	   else if (p_ptr->brand_cold)  { mult[cnt] = 2; desc[cnt++] = "frost-vulnerable creatures"; }
	   if ((f1 & TR1_BRAND_POIS) || (tbrand == 5))  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; isbrand++; }
	   else if (p_ptr->brand_pois)  { mult[cnt] = 2; desc[cnt++] = "poison-vulnerable creatures"; }
    }
    /* Get brands from rings for thrown weapons (ugly) */
    /* but if I use p_ptr->brand_xxxx it includes the brand from the wielded melee weapon */
    else if (f3 & TR3_THROWN) 
    {
       int ringbrand = thrown_brand();
       if ((ringbrand == 1) || ((ringbrand > 10) && (ringbrand < 20)) || 
          (f1 & TR1_BRAND_COLD) || (tbrand == 1))  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; isbrand++; }
       if ((ringbrand == 2) || (ringbrand == 12) || (ringbrand == 23) || 
          (ringbrand == 24) || (ringbrand == 25) || (f1 & TR1_BRAND_FIRE) || (tbrand == 2))  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; isbrand++; }
       if ((ringbrand == 3) || (ringbrand == 13) || (ringbrand == 23) || 
          (ringbrand == 34) || (ringbrand == 35) || (f1 & TR1_BRAND_ACID) || (tbrand == 4))  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; isbrand++; }
       if ((ringbrand == 4) || (ringbrand == 14) || (ringbrand == 24) || 
          (ringbrand == 34) || (ringbrand == 45) || (f1 & TR1_BRAND_ELEC) || (tbrand == 3))  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; isbrand++; }
       if ((ringbrand == 5) || (ringbrand == 15) || (ringbrand == 25) || 
          (ringbrand == 35) || (ringbrand == 45) || (f1 & TR1_BRAND_POIS) || (tbrand == 5))  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; isbrand++; }
       /* 1=cold, 2=fire, 3=acid, 4=elec, 5=pois */
       /* 1,2,3,4,5,12,13,14,15,23,24,25,34,35,45*/
    }
    else /* ammo */
    {
	   if ((f1 & TR1_BRAND_ACID) || (tbrand == 4))  { mult[cnt] = 3; desc[cnt++] = "acid-vulnerable creatures"; isbrand++; }
	   if ((f1 & TR1_BRAND_ELEC) || (tbrand == 3))  { mult[cnt] = 3; desc[cnt++] = "electricity-vulnerable creatures"; isbrand++; }
	   if ((f1 & TR1_BRAND_FIRE) || (tbrand == 2))  { mult[cnt] = 3; desc[cnt++] = "fire-vulnerable creatures"; isbrand++; }
	   if ((f1 & TR1_BRAND_COLD) || (tbrand == 1))  { mult[cnt] = 3; desc[cnt++] = "frost-vulnerable creatures"; isbrand++; }
	   if ((f1 & TR1_BRAND_POIS) || (tbrand == 5))  { mult[cnt] = 3; desc[cnt++] = "poison-vulnerable creatures"; isbrand++; }
	}

	if (f2 & TR2_KILL_DRAGON) { mult[cnt] = 5; desc[cnt++] = "dragons"; exslay = TRUE; }
	if (f2 & TR2_KILL_DEMON)  { mult[cnt] = 5; desc[cnt++] = "demons"; exslay = TRUE; }
	if (f2 & TR2_KILL_UNDEAD) { mult[cnt] = 5; desc[cnt++] = "undead"; exslay = TRUE; }

	/* stacking of brands and slays */
	/* uses a magic mult number (see relevant part of describe_attack() ) */
	if ((hasslay) && (isbrand))
	{
		mult[cnt] = 9;
		if (exslay) desc[cnt++] = "when both a slay and brand apply (slay animal, slay evil, and execute ('KILL') slays don't stack)";
		else desc[cnt++] = "when both a slay and brand apply";
	}
	if (isbrand > 1)
	{
		mult[cnt] = 9;
		if (isbrand == 2) desc[cnt++] = "when both brands apply";
		else desc[cnt++] = "when two different brands apply";
	}
	if (isbrand > 2)
	{
		mult[cnt] = 10;
		if (isbrand == 3) desc[cnt++] = "when all brands apply";
		else desc[cnt++] = "when three different brands apply";
	}
	if (isbrand > 3)
	{
		mult[cnt] = 11;
		if (isbrand == 4) desc[cnt++] = "when all brands apply";
		desc[cnt++] = "when four different brands apply";
	}

	return cnt;
}


/*
 * Describe weapon damage
 */
void describe_attack(const object_type *o_ptr)
{
    object_type *j_ptr;
	const char *desc[18];
	bool ammo, weapon, edged, thrower, strong_throw;
    bool throwglove = FALSE;
	int mult[16];
	int dam, sbdam, cnt, total_dam, total_sbdam, critc, ptodam, tmpblow;
	int xtra_dam = 0;
    int strdam = 0;
	int strdec, strb;
    int ringbrand = 0;
    int brandodd = 0;
	object_type *g_ptr; /* g_ptr to check gloves */
	u32b f1, f2, f3, f4;
	u32b f[4];

	/*** Hacky: Check for to_dam bonus from gauntlets of throwing ***/
	g_ptr = &inventory[INVEN_HANDS];
	if (g_ptr->k_idx)
	{
		/* Extract the gloves' flags */
		object_flags(g_ptr, &f1, &f2, &f3, &f4);

		/* add to_dam from gloves if appropriate (later) */
		if ((f3 & (TR3_THROWMULT)) && (object_known_p(g_ptr))) throwglove = TRUE;

		/* reset flags before getting thrown object flags */
		f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;
	}


	/* Grab the object flags */
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* get the launcher */
	j_ptr = &inventory[INVEN_BOW];
    
	ammo   = (p_ptr->ammo_tval == o_ptr->tval) && (j_ptr->k_idx);
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	
	if ((f3 & TR3_THROWN) || (f3 & TR3_PTHROW)) thrower = TRUE;
	else thrower = FALSE;

	/* don't need the bow if we're looking at a weapon */
	if (weapon)
	{
		/* check for slays on the light source */
		j_ptr = &inventory[INVEN_LITE];
	}
	
	if (o_ptr->tval == TV_BOW)
	{
        int multl, bowrange;
		switch (o_ptr->sval)
		{
			/* ML2 */
            case SV_SLING:
			case SV_MINI_XBOW:
			case SV_SHORT_BOW:
            {
                 multl = 2;
                 if (o_ptr->sval == SV_SHORT_BOW) bowrange = 17;
                 else bowrange = 16;
                 break;
            }
			/* ML3 */
			case SV_HANDHELDC:
            {
                 multl = 3;
                 bowrange = 19;
                 break;
            }
			case SV_LONG_BOW:
            {
                 multl = 3;
                 bowrange = 20;
                 break;
            }
			case SV_LIGHT_XBOW:
            {
                 multl = 3;
                 bowrange = 18;
                 break;
            }
			/* ML4 */
			case SV_GREAT_BOW:
			case SV_HEAVY_XBOW:
            {
                 multl = 4;
                 if (o_ptr->sval == SV_HEAVY_XBOW) bowrange = 20;
                 else bowrange = 21;
                 break;
            }
        }
		if ((f1 & (TR1_MIGHT)) && (object_known_p(o_ptr))) 
		{
			multl += o_ptr->pval;
			bowrange += o_ptr->pval;
		}
        
	    new_paragraph = TRUE;
	    p_text_out(format("A launcher with an multiplier level (ML) of %d ", multl));
	    new_paragraph = FALSE;
	    text_out("actually multiplies damage by ");
	    if (multl == 4) text_out("x3.5");
        else if (multl == 3) text_out("x2.625");
        else if (multl == 5) text_out("x4.375");
        else if (multl > 5) text_out("x5.25");
        else /* 2 */ text_out("x1.75");
	    text_out(format(". The range on this launcher is %d spaces.\n", bowrange));
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
	   if (p_ptr->num_blow > 1) text_out(format("%d blows per round.", p_ptr->num_blow));
	   else text_out(format("%d blow per round.", p_ptr->num_blow));

	   /* double weapons (tell how many blows you would get if */
	   /* you were/weren't wearing a shield) */
	   if (o_ptr->sbdd)
	   {
			object_type *oarm_ptr;
			int str_index, dex_index, div, maxblows, wspdm;
			bool yshield = FALSE;
			bool staffbonus = FALSE;
#if nextbreaksave
#else
			object_kind *k_ptr = &k_info[o_ptr->k_idx];
#endif
			div = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);
			oarm_ptr = &inventory[INVEN_ARM];
			if (oarm_ptr->k_idx) yshield = TRUE;
			/* priest & druids get bonus with staffs (but are worse with icky double weapons) */
			if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!p_ptr->icky_wield) && (o_ptr->sbdd))
			{
				if ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_STAFF)) staffbonus = TRUE;
			}
			/* new weapon speed modifier: modifies effective weapon weight for # of blows */
#if nextbreaksave
			wspdm = o_ptr->spdm;
#else
			wspdm = k_ptr->spdm;
#endif
			/* apply priest / druid bonus with double-weapon staffs */
			if ((staffbonus) && (adj_str_blow[p_ptr->stat_ind[A_STR]] < 100)) wspdm += 2;
			/* apply weapon speed modifier */
			if (wspdm > 0)
			{
				while ((div >= 15) && (wspdm > 0))
				{
					div -= 10; /* (1lb) */
					wspdm--;
				}
			}	
			else if (wspdm < 0)
			{
				div += ABS(wspdm * 10);
				wspdm = 0;
			}
	
			if (yshield)
			{
				div = ((o_ptr->weight/3 < cp_ptr->min_weight/2) ? cp_ptr->min_weight/2 : o_ptr->weight/3);
			}
			else
			{
				div = (div * 4) / 5;
			}
			str_index = (adj_str_blow[p_ptr->stat_ind[A_STR] + wspdm] * cp_ptr->att_multiply / (div*10));
			if (str_index > 11) str_index = 11;
			dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);
			if (dex_index > 12) dex_index = 12;
			tmpblow = blows_table[str_index][dex_index];
			
			if (yshield) maxblows = cp_ptr->max_attacks;
			else maxblows = cp_ptr->max_attacks + 1;
			if (tmpblow > maxblows) tmpblow = maxblows;
			tmpblow += p_ptr->extra_blows;
			
			if (p_ptr->timed[TMD_XATTACK]) tmpblow += 1;
			if (p_ptr->peace) tmpblow -= 1;
			if (tmpblow < 1) tmpblow = 1;
			if ((yshield) && (p_ptr->num_blow != tmpblow))
			{
				if (tmpblow > 1) text_out(format("  You would get %d blows with this weapon if you weren't wearing a shield.", tmpblow));
				else text_out("  You would get one blow with this weapon if you weren't wearing a shield.");
			}
			else if (p_ptr->num_blow != tmpblow)
			{
				if (tmpblow > 1) text_out(format("  You would get %d blows with this weapon if you were wearing a shield.", tmpblow));
				else text_out("  You would get one blow with this weapon if you were wearing a shield.");
			}
	   }
	   
	   /* shield slot weapons */
       if (f4 & (TR4_WIELD_SHIELD)) text_out("  You can wield this weapon in your off hand for defence.");
       /* maybe later I'll have duel-wielding */
       /* if (f4 & (TR4_WIELD_SHIELD)) text_out("You can wield this weapon in your off hand without special training."); */

	   /* Warn about heavy weapons */
	   if (p_ptr->heavy_wield)
		  text_out_c(TERM_L_RED, "  You are too weak to use this weapon effectively.");
    }
	if (thrower)
	{
		int throws = thits_thrown(o_ptr->weight);
		if (throws > 1)
        {
            if (f3 & TR3_PTHROW) text_out("  ");
            text_out(format("You can throw %d of these weapons in one turn.", throws));
        }
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
		int wweight = o_ptr->weight / 10;
		object_type *oarm_ptr = &inventory[INVEN_ARM];
		dam = ((o_ptr->ds + 1) * o_ptr->dd * 5);
	   if (o_ptr->sbdd) sbdam = ((o_ptr->sbds + 1) * o_ptr->sbdd * 5);
       /* remove strength bonus from xtra_dam */
       ptodam = p_ptr->dis_to_d - ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	   xtra_dam = ptodam * 10;
	   xtra_dam += o_ptr->to_d * 10;
	    new_paragraph = TRUE;
		p_text_out("Each blow will do an average damage of ");
	    new_paragraph = FALSE;

		/* not wielding a shield raises strength bonus */
		if ((!oarm_ptr->k_idx) && (!p_ptr->heavy_wield) && (wweight >= 8))
			wweight += 2;

	   /* complex strength bonus by weight */
       strb = 10 * ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	   if (wweight < 2) strb = strb / 6;
	   else if (wweight < 3) strb = strb / 4;
	   else if (wweight < 4) strb = strb / 2;
	   else if (wweight < 5) strb = (strb * 3) / 5;
	   else if (wweight < 6) strb = (strb * 3) / 4;
	   else if (wweight < 7) strb = (strb * 5) / 6;
	   /* barbarians & hulks like heavy weapons */
		if ((cp_ptr->flags & CF_HEAVY_BONUS) || (p_ptr->prace == 17)) 
		{
			if (wweight > 26) strb = (strb * 13) / 6;
			else if (wweight > 20) strb = strb * 2;
			else if (wweight > 17) strb = (strb * 7) / 4;
			else if (wweight > 15) strb = (strb * 3) / 2;
			else if (wweight > 12) strb = (strb * 4) / 3;
			else if (wweight > 10) strb = (strb * 5) / 4;
			else if (wweight == 10) strb = (strb * 6) / 5;
			if (wweight > 10) strb += 1;
		}
		else
		{
			if (wweight > 25) strb = strb * 2;
			else if (wweight > 21) strb = (strb * 7) / 4;
			else if (wweight > 17) strb = (strb * 3) / 2;
			else if (wweight > 15) strb = (strb * 4) / 3;
			else if (wweight > 12) strb = (strb * 5) / 4;
			else if (wweight > 10) strb = (strb * 6) / 5;
		}
	   strdec = (strb / 10);
	   dam += strdec * 10;
	   sbdam += strdec * 5;
	   /* decimal */
	   strb -= strdec * 10;
	   dam += strb;
	   sbdam += strb/2;

#if oldbreak
       /* figure strength bonus */
       if (wweight > 4)
       {
          strdam = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
                   
          /* double strength bonus for very heavy weapons (heavier than 15 lb) */
          if (wweight > 15) strdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

          dam += strdam * 10;
       }
#endif

		/* check for slays on the light source */
		object_flags(j_ptr, &f[0], &f[1], &f[2], &f[3]);
		f1 |= f[0];
		f2 |= f[1];
       
       /* find chance of critical hit */
       critc = crit_chance(o_ptr, f2);
    }
    else if (ammo)
    {
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
        critc = critshot_chance(o_ptr, f2, f3);
    }
    
    /* exclude thrown weapons */
    /* (don't use thrown flag or iron spikes will get garbage in the description) */
    if ((!(wield_slot(o_ptr) == INVEN_QUIVER)) || (ammo))
    {
        /* Collect slays */
        if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
           cnt = collect_slays(desc, mult, f1, f2, f3, f4, weapon, o_ptr->thisbrand);
	    else cnt = collect_slays(desc, mult, f1, f2, f3, f4, weapon, 0);
	   
	    if (cnt)
	    {
	        int i;

	    	for (i = 0; i < cnt; i++)
    		{
	    	   /* Include bonus damage and slay in stated average */
	    	   int slay_dam = dam * mult[i] + xtra_dam;
	    	   int slay_sbdam = sbdam * mult[i] + xtra_dam;

	    	   /* special message for slay stacking which adds 25% */
	    	   if (mult[i] == 9) slay_dam = dam / 4;
	    	   if (mult[i] == 10) slay_dam = (dam / 4) * 2;
		       if (mult[i] == 11) slay_dam = (dam / 4) * 3;
    
               /* strength bonus after multipliers for slings */
               /* but before multipliers for melee weapons */
               if ((o_ptr->tval == TV_SHOT) && (strdam))
                  slay_dam += strdam;

		    	if (mult[i] >= 9) text_out("+");
		    	if (slay_dam <= 0)
		    		text_out(format("%d", 0));
	    		else if (slay_dam % 10)
	    			text_out(format("%d.%d", slay_dam / 10, slay_dam % 10));
	    		else text_out(format("%d", slay_dam / 10));
	    		if ((o_ptr->sbdd) && (mult[i] < 9) && ((p_ptr->num_blow > 1) || (tmpblow > 1)))
	    	   {
	    		   if (slay_sbdam <= 0)
	    			    text_out(format(" (%d", 0));
	    		   else if (slay_sbdam % 10)
	    				text_out(format(" (%d.%d", slay_sbdam / 10, slay_sbdam % 10));
	    		   else text_out(format(" (%d", slay_sbdam / 10));
	    		   text_out(" on the 2nd hit)");
		       }
    
	    	   if (mult[i] >= 9) text_out(format(" %s, ", desc[i]));
	    	   else text_out(format(" against %s, ", desc[i]));
	    	}
	        text_out("and ");
        }
       
        /* Include bonus damage in stated average */
        total_dam = dam + xtra_dam;
    
        /* strength bonus after multipliers for slings */
        /* but before multipliers for melee weapons */
        if ((o_ptr->tval == TV_SHOT) && (strdam)) total_dam += strdam;

	    if (total_dam <= 0)
	       text_out(format("%d", 0));
    	else if (total_dam % 10) text_out(format("%d.%d",
	               total_dam / 10, total_dam % 10));
    	else text_out(format("%d", total_dam / 10));
	    if ((o_ptr->sbdd) && ((p_ptr->num_blow > 1) || (tmpblow > 1)))
	    {
	    	total_sbdam = sbdam + xtra_dam;
	    	if (total_sbdam <= 0)
	    	   text_out(format(" (%d", 0));
	    	else if (total_sbdam % 10) text_out(format(" (%d.%d",
	    	           total_sbdam / 10, total_sbdam % 10));
	    	else text_out(format(" (%d", total_sbdam / 10));
    		text_out(format(" on the 2nd hit)", total_sbdam));
    	}

        if (cnt) text_out(" against other monsters");

    	if ((f1 & TR1_TUNNEL) || (f3 & TR3_LITE))
    	{
	    	int digrock;
	    	if ((f1 & TR1_TUNNEL) && (o_ptr->pval > 1)) digrock = (dam / 4) * o_ptr->pval;
	    	else digrock = (dam / 4);
	    	if ((digrock == (dam / 4)) && ((dam / 4) > 0))
	    	{
	    		if ((dam / 4) % 10)
	    			text_out(format(" (add +%d.%d", (dam / 4) / 10, (dam / 4) % 10));
	    		else text_out(format(" (add +%d", (dam / 4) / 10));
		    	if ((f1 & TR1_TUNNEL) && (f3 & TR3_LITE))
		    	{
	    			text_out(" damage against creatures vulnerable to light or rock remover)");
	    		}
	    		else if (f3 & TR3_LITE)
	    		{
		    		text_out(" damage against creatures vulnerable to light)");
			    }
    			else if (f1 & TR1_TUNNEL)
	    		{
		    		text_out(" damage against creatures vulnerable to rock remover)");
			    }
    		}
	    	else if ((f1 & TR1_TUNNEL) && (f3 & TR3_LITE) && (o_ptr->pval > 1))
		    {
			    if ((dam / 4) % 10)
    				text_out(format(" (add +%d.%d", (dam / 4) / 10, (dam / 4) % 10));
	    		else text_out(format(" (add +%d", (dam / 4) / 10));
		    	text_out(" damage against creatures vulnerable to light and");
			    if ((dam / 4) % 10)
	    			text_out(format(" add +%d.%d", digrock / 10, digrock % 10));
		    	else text_out(format(" add +%d", digrock / 10));
			    text_out(" damage against creatures vulnerable to rock remover)");
    		}
	    	else if ((f1 & TR1_TUNNEL) && (o_ptr->pval > 1))
		    {
			    if ((dam / 4) % 10)
				    text_out(format(" (add +%d.%d", digrock / 10, digrock % 10));
	    		else text_out(format(" (add +%d", digrock / 10));
	    		text_out(" damage against creatures vulnerable to rock remover)");
	    	}
    	}

		/* factors not included */
		if ((weapon) && (p_ptr->timed[TMD_BALROG]))
           text_out(". This does not figure in the 'spririt of the balrog' effect.");
		else text_out(".");
	}

	/* note about the weakness of SLAY_WERE weapons */
	if (f1 & TR1_SLAY_WERE)
	{
		text_out("  Note that no multipliers from a silver weapon work against silver monsters.");
	}

	if ((p_ptr->timed[TMD_MIGHTY_HURL]) || (cp_ptr->flags & CF_HEAVY_BONUS) ||
		(p_ptr->prace == 17)) strong_throw = TRUE;
    else strong_throw = FALSE;
    
    /* can be too heavy to throw effectively even if it's meant for throwing */
    if ((!strong_throw) && (o_ptr->weight >= 200))
    {
	    /* (if STR is less than 18/150) */
	    if (((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) < 10) thrower = FALSE;
    }
    /* strong throw characters get no penalty for throwing normal melee weapons */
    /* so show damage for them too */
    else if ((weapon) && (strong_throw)) thrower = TRUE;

    if (thrower)
    {
		int throwok;
        /* Calculate damage */
		dam = ((o_ptr->ds + 1) * o_ptr->dd * 5);
		if (object_known_p(o_ptr)) dam += (o_ptr->to_d * 10);

		/* add to_dam from gloves if appropriate */
		if (throwglove) dam += (g_ptr->to_d * 10);

  	    if (f3 & TR3_THROWN) throwok = 2; /* throwing weapon */
	    else /* (f3 & TR3_PTHROW) */ throwok = 1; /* semi-thrower */
	
	    /* bonus to thrown weapon multiplier from equipment */
        throwok += p_ptr->throwmult;

	    if ((!strong_throw) && (o_ptr->weight >= 150))
	    {
		    /* (if STR is less than 18/50) */
		    if (((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) < 5) throwok -= 1;
        }

        if (((strong_throw) || (p_ptr->pclass == 0) ||
                (cp_ptr->flags & CF_KNIGHT)) && (throwok < 2))
        {
            dam = (dam * 5) / 4; /* semi-throwers x1.25 */
        }
        else if (throwok > 0)
        {
		    /* throwing weapons:  throwok == 2 + THROWMULT bonus (minimum THROWMULT bonus is 2) */
		    /* semi-throwing weapons:  throwok == 1 + THROWMULT bonus */
		    if (throwok == 4) dam = ((dam * 7) / 4);       /* (x1.75) (same as ML2 for bows) */
		    else if (throwok == 5) dam = ((dam * 21) / 8); /* (x2.625) (same as ML3) */
		    else if (throwok >= 6) dam = ((dam * 7) / 2);  /* (x3.5) (same as ML4) */
		    /* normal multiplier for semi-throwing weapons:  x1.2 */
		    else if (throwok == 1) dam = (dam * 6) / 5;
		    /* normal throwing weapon multiplier:  x1.4  */
		    /* (same for THROWMULT bonus of 2 with a semi-thrower) */
		    else /* throwok == 2 or 3 */ dam = (dam * 7) / 5;
        }
		
		/* DJA: add (partial) strength bonus for certain classes */
		if ((strong_throw) || (cp_ptr->flags & CF_KNIGHT) || (p_ptr->pclass == 0))
		{
			int strb;
			int eweight = o_ptr->weight;
			if (!(f3 & TR3_THROWN)) eweight = eweight / 2; /* less if not meant for throwing */
			/* complex strength bonus by weight (different than melee) */
			strb = 10 * ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
			if (eweight < 25) strb = strb / 2;
			else if (eweight < 40) strb = (strb * 2) / 3;
			else if (eweight < 50) strb = (strb * 3) / 4;
			else if (eweight < 60) strb = (strb * 5) / 6;
			strb = strb / 10;
			if (p_ptr->timed[TMD_MIGHTY_HURL]) strb = strb * 2;
			dam += strb;
			/* doesn't use strdam because STR bonus is now added before slays */
		}

        /* strdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128); */

	    if (f3 & TR3_THROWN) new_paragraph = TRUE;
	    else text_out("  ");
		p_text_out("This weapon can be thrown for an average damage of ");
	    new_paragraph = FALSE;
       
        /* find chance of critical hit */
        if (f3 & TR3_THROWN) critc = critshot_chance(o_ptr, f2, f3);

        /* Collect slays */
        if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
           cnt = collect_slays(desc, mult, f1, f2, f3, f4, weapon, o_ptr->thisbrand);
	    else cnt = collect_slays(desc, mult, f1, f2, f3, f4, weapon, 0);
	   
	    /* have to do all this again for thrown weapons */
	    /* because some weapons can be used both for melee and for throwing */
        if (cnt)
	    {
	        int i;

		    for (i = 0; i < cnt; i++)
		    {
		       /* Include bonus damage and slay in stated average */
		       int slay_dam = dam * mult[i];
		       int slay_sbdam = sbdam * mult[i];

		       /* special message for slay stacking which adds 25% */
		       if (mult[i] == 9) slay_dam = dam / 4;
		       if (mult[i] == 10) slay_dam = (dam / 4) * 2;
		       if (mult[i] == 11) slay_dam = (dam / 4) * 3;
		       if (mult[i] == 92) slay_dam = (dam / 4) * 10; /* x2.5 */
    
               /* strength bonus after multipliers for slings */
               /* but before multipliers for melee weapons */
               if ((o_ptr->tval == TV_SHOT) && (strdam))
                  slay_dam += strdam;

			    if ((mult[i] >= 9) && (mult[i] < 90)) text_out("+");
			    if (slay_dam <= 0)
			    	text_out(format("%d", 0));
			    else if (slay_dam % 10)
			    	text_out(format("%d.%d", slay_dam / 10, slay_dam % 10));
			    else text_out(format("%d", slay_dam / 10));
			    if ((o_ptr->sbdd) && (mult[i] < 9) && ((p_ptr->num_blow > 1) || (tmpblow > 1)))
		      {
			       if (slay_sbdam <= 0)
			    	    text_out(format(" (%d", 0));
			       else if (slay_sbdam % 10)
			    		text_out(format(" (%d.%d", slay_sbdam / 10, slay_sbdam % 10));
			       else text_out(format(" (%d", slay_sbdam / 10));
			       text_out(" on the 2nd hit)");
		       }

		       if ((mult[i] >= 9) && (mult[i] < 90)) text_out(format(" %s, ", desc[i]));
		       else text_out(format(" against %s, ", desc[i]));
		    }
	        text_out("and ");
        }
       
        /* Include bonus damage in stated average */
        total_dam = dam;
        /* exploding stuff does x2 against everything that isn't */
        /* vulnerable to a brand on the weapon */
        if (f4 & TR4_EXPLODE_A) total_dam *= 2;
    
	    if (total_dam <= 0)
	       text_out(format("%d", 0));
	    else if (total_dam % 10) text_out(format("%d.%d",
	               total_dam / 10, total_dam % 10));
	    else text_out(format("%d", total_dam / 10));

        if (cnt) text_out(" against other monsters");
        text_out(".");
    }
    
    /* note to-hit penalty from TMD_XATTACK */
    if (((ammo) || (f3 & TR3_THROWN)) && (p_ptr->timed[TMD_XATTACK]))
    {
       text_out("  Your to-hit with ranged weapons is currently 1/3 of what it");
       text_out(" usually is because of the magic which is giving you an extra melee blow.");
    }
    
    /* Are you getting an elemental brand from a ring? */
    if (thrower)
	{
		ringbrand = thrown_brand();

		/* Get odds of thrown weapon getting branded by elemental ring */
		if (ringbrand)
		{
		   brandodd = ((p_ptr->skills[SKILL_THT] + goodluck) / 2) - 5;
		   if (p_ptr->lev < 20) brandodd += 5;
		   /* if ringbrand > 10 then */
		   /* character is wearing more than one elemental ring */
		   /* so more likely to get brand from ring */
		   if (ringbrand > 10) brandodd += 10;
		   
		   /* less likely for semi-throwers */
           if (!(f3 & (TR3_THROWN))) brandodd = brandodd/2;
	
	       /* percentile */
	       if (brandodd > 100) brandodd = 100;
	    }
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
          text_out(" and would hinder your magic.");
       }

       text_out("  Your chance of scoring a critical hit with this weapon is 1 in ");
       text_out(format("%d", critc));
       if (f3 & TR3_PTHROW) text_out(" (less when thrown)");
       text_out(".\n");
       
       /* re-calulate bonuses with real wielded weapon to prevent wierd messages */
       calc_bonuses(inventory, TRUE);
    }
    else if (f3 & TR3_THROWN)
    {
		int xstrb;
		xstrb = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
		text_out("  Your chance of scoring a critical hit when throwing this ");
		text_out(format("weapon is 1 in %d.", critc));

		if ((!strong_throw) && (o_ptr->weight >= 200) && (xstrb < 10))
		{
			text_out("  This weapon is too heavy to be thrown effectively with your strength.");
		}
		if (f1 & TR1_BLOWS)
		{
			if (o_ptr->pval > 1) text_out(format("  This weapon attacks your foe %d times when you throw it.", o_ptr->pval + 1));
			else if (o_ptr->pval == 1) text_out("  This weapon attacks your foe twice when you throw it.");
		}
		if ((ringbrand) && (o_ptr->tval != TV_FLASK))
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
 * description of wand / rod enhancement.
 */
void dsc_enhance(object_type *o_ptr)
{
	cptr type;
	bool zapper = TRUE;
	bool recall = FALSE;
	int special = 0;
	s16b amount;

	/* enhanced items only */
	if (!o_ptr->enhance) return;

	/* wasted enhancement */
	if (!item_tester_hook_bigwand(o_ptr))
	{
		p_text_out("  You wasted a spell on this item.  This item cannot be enhanced.");
		/* unenhance */
		o_ptr->enhance = 0;
		o_ptr->enhancenum = 0;
		return;
	}

	if (o_ptr->tval == TV_WAND)
	{
		type = "wand";
		/* check type */
		switch (o_ptr->sval)
		{
			case SV_WAND_SLEEP_MONSTER:
			case SV_WAND_SLOW_MONSTER:
			case SV_WAND_CONFUSE_MONSTER:
			case SV_WAND_FEAR_MONSTER:
			case SV_WAND_POLYMORPH:
				zapper = FALSE;
		}
	}
	else if (o_ptr->tval == TV_ROD)
	{
		type = "rod";
		/* check type */
		switch (o_ptr->sval)
		{
			case SV_ROD_RECALL:
			{
				recall = TRUE;
				break;
			}
			case SV_ROD_SLEEP_MONSTER:
			case SV_ROD_SLOW_MONSTER:
			case SV_ROD_POLYMORPH:
				zapper = FALSE;
		}
	}
	else if (o_ptr->tval == TV_STAFF)
	{
		type = "staff";
		if (!(o_ptr->sval == SV_STAFF_STRIKING)) zapper = FALSE;
		if (o_ptr->sval == SV_STAFF_CURE_LIGHT) special = 1;
		if (o_ptr->sval == SV_STAFF_DARKNESS) special = 2;
		if (o_ptr->sval == SV_STAFF_STARLITE) special = 3;
	}
	else if (o_ptr->tval == TV_RING)
	{
		type = "ring";
	}
	else 
	{
		type = "item";
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];
			if ((a_ptr->activation == ACT_CURE_WOUNDS) || 
				(a_ptr->activation == ACT_CONFUSE))
				zapper = FALSE;
			else if (a_ptr->activation == ACT_WOR) recall = TRUE;
		}
	}

	/* paranoia */
	if (o_ptr->enhancenum > o_ptr->number) o_ptr->enhancenum = o_ptr->number;

	/* start description */
	if (o_ptr->number == 1)
		p_text_out(format("  This %s is enhanced", type));
	else if (o_ptr->number == o_ptr->enhancenum)
		p_text_out(format("  These %ss are enhanced", type));
	else if (o_ptr->enhancenum < o_ptr->number)
		p_text_out(format("  %d of these %ss are enhanced", o_ptr->enhancenum, type));

	/* distribute enhancement */
	amount = o_ptr->enhance/o_ptr->enhancenum;

	/* describe enhancement */
	if (!object_aware_p(o_ptr)) text_out(format(" by +%d.", amount));
	else if (recall) text_out(" with reduced recall time.");
	else if (zapper) text_out(format(" with +%d to damage.", amount));
	else if (special == 1) text_out(format(" with +%d to curing amount.", (amount+1)/2));
	else if (special == 2) 
	{
		text_out(" with better chance of stronger darkness damage to all monsters in sight");
		text_out(" as well as a chance to prevent causing blindness.");
	}
	else if (special == 3) text_out(" with better chance of the more powerful rays of light.");
	else text_out(format(" with +%d to effectiveness.", amount));
	if ((cheat_xtra) && ((special == 2) || (special == 3) || (recall)))
		text_out(format(" (by %d)", amount));
}

/*
 * figure and display chance of magic device failure.
 */
void device_chance(const object_type *o_ptr)
{
	int lev, chance, odds, power;
	int artstaff = 0;
	bool fluke = FALSE;
	bool charged = FALSE;
	bool allcharging = FALSE;
	bool staticrune = FALSE;

	/* Extract the flags (to check for activation) */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* does it use charges? */
	if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
		charged = TRUE;

	/* just in case of an activatable artifact staff */
	/* (there is only 1 at the moment) */
	if ((charged) && (f3 & (TR3_ACTIVATE)) && (artifact_p(o_ptr)))
		artstaff = 1;

	/* see if there are any rods in the stack which are not currently charging */
	if ((o_ptr->timeout) && (o_ptr->tval == TV_ROD))
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		if (k_ptr->pval == 0) k_ptr->pval = 1;
		power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

		/* all rods in the stack are charging */
		if (power >= o_ptr->number) allcharging = TRUE;
	}

	/* 100% chance of failure */
	if ((charged) && (o_ptr->charges <= 0))
	{
	    p_text_out("  This device is out of charges.");
		if (artstaff) artstaff = 2;
		else return;
	}
	if (((!charged) || (artstaff)) && (o_ptr->timeout) && (allcharging))
	{
		new_paragraph = TRUE;
	    p_text_out("  This device is charging.");
		new_paragraph = FALSE;

		/* only show normal staff use odds for charging artifact staves */
		if (artstaff == 1) artstaff = 0;
		else return;
	}

	/* artstaff == 2 means out of charges so skip the odds for staff use */
	if (artstaff == 2)
	{
		artstaff = a_info[o_ptr->name1].level;
	}
	else
	{
		/* Extract the item diffuculty (now separated from item level) */
		lev = k_info[o_ptr->k_idx].extra;
	
		/* cursed items are harder to use */
		/* (currently only staffs are ever cursed but that will likely change) */
		if (cursed_p(o_ptr)) lev += 10 + (badluck+1)/3;

		/* blessed items are easier to activate */
		if ((o_ptr->blessed > 1) && (lev > 23)) lev -= 12;
		else if ((o_ptr->blessed > 1) && (lev > 12)) lev = 12;
		else if ((o_ptr->blessed) && (lev > 4)) lev -= 4;

#ifdef roomrunes /* static rune */
		if (p_ptr->roomeffect == 14)
		{ 
			if (!p_ptr->resist_static) { lev += 20 + (badluck+1)/2;  staticrune = TRUE; }
			else if (p_ptr->resist_static < 2) lev += 5 + (badluck+2)/3;
		}
#endif
	
		/* Hack -- use artifact level instead (bless/curse doesn't affect artifact activation) */
		/* if artstaff then it has two different difficulty levels */
		if (artstaff) artstaff = a_info[o_ptr->name1].level;
		else if (artifact_p(o_ptr)) lev = a_info[o_ptr->name1].level;

		/* Base chance of success */
		chance = p_ptr->skills[SKILL_DEV];

		/* Confusion hurts skill */
		if (p_ptr->timed[TMD_CONFUSED])
		{
		   if (goodluck > 16) chance = (chance * 8) / 9;
		   else if (goodluck > 9) chance = (chance * 3) / 4;
		   else if (goodluck > 2) chance = (chance * 2) / 3;
		   else if (chance >= 60) chance = ((chance * 2) / 3) - (5 + (badluck/2));
		   else chance = (chance / 2) - (badluck/3);
		}

		/* High level objects are harder */
		/* no limit now that difficulty is separated from depth */
		/* (very few devices have difficulty > 50) */
		if ((artifact_p(o_ptr)) && (!artstaff)) chance = chance - ((lev > 50) ? 50 : lev);
		else chance -= lev;


		/* Check for amnesia (only for activations) */
		if ((!charged) && (o_ptr->tval != TV_ROD) && 
			(p_ptr->timed[TMD_AMNESIA]))
		{
			/* this is an approximation ..but that's alright because */
			/* usually when you (I)nspect an object, you aren't hallucenating */
			chance = chance / 2;
		}
	
		/* Give everyone a (slight) chance */
		/* Odds of success (33% success rate (1 in 3) at best) */
		if (chance < USE_DEVICE)
		{
			fluke = TRUE; /* you'd need a fluke */
			if (chance + 1 < 2) chance = 2;
			else chance += 1;
			if (lev < 9) lev = 9;
			/* if (rand_int(lev) < chance) */
			odds = ((chance-1) * 100 / lev);
			if (charged) p_text_out(format("  You have about a %d percent chance of success when using this device.", odds));
			else p_text_out(format("  You have about a %d percent chance of success when activating this device.", odds));
			/* text_out(format("  chance is %d", chance)); * for testing */
		}
	
		/* Odds of failure */
		if (!fluke)
		{
			/* if (randint(chance) < USE_DEVICE) */
			odds = ((USE_DEVICE-1) * 100 / chance);
			
			if (charged) p_text_out(format("  You have about a %d percent chance of failure when using this device.", odds));
			else p_text_out(format("  You have about a %d percent chance of failure when activating this device.", odds));
			/* for testing */
			/* text_out(format("  chance is %d", chance));
			 * text_out(format("  lev is %d", lev));
			 */
		}
		
		if (staticrune)
		{
			text_out(" It would be easier to use anywhere but this room because this room is full of magical static.");
		}

		/* Done for almost every object */
		if (!artstaff) return;
	}

	/** the rest of this funcion only applies to artifact staffs **/

	/* reset chance */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->timed[TMD_CONFUSED])
    {
       if (goodluck > 16) chance = (chance * 8) / 9;
       else if (goodluck > 9) chance = (chance * 3) / 4;
       else if (goodluck > 2) chance = (chance * 2) / 3;
	   else if (chance >= 60) chance = ((chance * 2) / 3) - (5 + (badluck/2));
       else chance = (chance / 2) - (badluck/3);
    }

	/* now use the other difficulty level */
	chance = chance - ((artstaff > 50) ? 50 : artstaff);

	/* amnesia penalty */
	if (p_ptr->timed[TMD_AMNESIA]) chance = chance / 2;

	/* Give everyone a (slight) chance */
	/* Odds of success (33% success rate (1 in 3) at best) */
	if (chance < USE_DEVICE)
	{
		fluke = TRUE; /* you'd need a fluke */
		if (chance + 1 < 2) chance = 2;
		else chance += 1;
		if (artstaff < 9) artstaff = 9;
		/* if (rand_int(lev) < chance) */
		odds = ((chance-1) * 100 / artstaff);
		text_out(format("  You have about a %d percent chance of success when using the activation on this device.", odds));
	}

	/* Odds of failure */
	if (!fluke)
	{
		/* if (randint(chance) < USE_DEVICE) */
		odds = ((USE_DEVICE-1) * 100 / chance);
		
		text_out(format("  You have about a %d percent chance of failure when using the activation on this device.", odds));
	}
	return;
}


/*
 * Output object information
 */
bool object_info_out(const object_type *o_ptr)
{
	bool something = FALSE;
	bool bigslay = FALSE;

	/* Grab the object flags */
	u32b f1, f2, f3, f4;
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* on *Slay* egos, racial ESP is obvious */
	if (((o_ptr->name2 >= EGO_KILL_ANIMAL) && (o_ptr->name2 <= EGO_KILL_DRAGON)) ||
		(o_ptr->name2 == EGO_SLAY_TWO)) bigslay = TRUE;
			
	/* Describe the object */
	if (describe_stats(o_ptr, f1)) something = TRUE;
	if (describe_secondary(o_ptr, f1, f2, f3)) something = TRUE;
	if (describe_slay(o_ptr, f1, f2)) something = TRUE;
	if (describe_brand(o_ptr, f1, f2)) something = TRUE;
	/* acid coating */
	if ((o_ptr->timedbrand) && (o_ptr->thisbrand == 6))
    {
       text_out(" It is coated with acid. ");
       something = TRUE;
    }
	if (describe_immune(o_ptr, f2)) something = TRUE;
    /* partial poison resistance */
	if (f2 & (TR2_PR_POIS))
	{
		text_out(" It provides partial resistance to poison. ");
		something = TRUE;
	}
	if (describe_resist(o_ptr, f2, f3, f4)) something = TRUE;
	if (describe_sustains(o_ptr, f2)) something = TRUE;
	if (describe_misc_magic(o_ptr, f2, f3, f4)) something = TRUE;
	if (describe_activation(o_ptr, f3)) something = TRUE;
	if (describe_ignores(o_ptr, f3)) something = TRUE;

	/* special note about ego/artifact lights which don't have NO_FUEL */
	if ((object_known_p(o_ptr)) && (o_ptr->tval == TV_LITE) &&
		((ego_item_p(o_ptr)) || (artifact_p(o_ptr))))
	{
		if (!(f3 & TR3_NO_FUEL))
			p_text_out("It may lose some of its benefits while it is out of fuel.");
	}

	/* Unknown extra powers (ego-item with random extras) */
	/* esprace should be obvious */
	if ((object_known_p(o_ptr)) && (!(o_ptr->ident & IDENT_MENTAL)) &&
		(!is_ego_randart(o_ptr)) &&
		((o_ptr->randsus) || (o_ptr->randsus2) || 
		(o_ptr->randres) ||	(o_ptr->randres2) || (o_ptr->randres3) || 
		(o_ptr->randpow2) || (o_ptr->randpow) || 
		((o_ptr->esprace) && (!bigslay))))
	{
		/* Hack -- Put this in a separate paragraph if screen dump */
		if (text_out_hook == text_out_to_screen) new_paragraph = TRUE;

		p_text_out("It might have hidden powers.");
		something = TRUE;
	}
	
	/* recognise pseudo randarts */
	if ((cheat_xtra) && (is_ego_randart(o_ptr)))
	{
		p_text_out(format("  This is one of the 'of randomness' pseudo-randart egos (ego item index %d).", o_ptr->name2));
	}

	/* We are done. */
	return something;
}


/*
 * Header for additional information when printing to screen.
 *
 * Return TRUE if an object description was displayed.
 */
static bool screen_out_head(object_type *o_ptr)
{
	char *o_name;
	int name_size = Term->wid;
	bool has_description = FALSE;

	/* See if the object is "aware" */
	bool aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "known" */
	bool known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* Allocate memory to the size of the screen */
	o_name = C_RNEW(name_size, char);

	/* Description */
	/* (changed to make sure the player can see the whole inscription) */
	if ((o_ptr->note) || (o_ptr->pseudo) || (cursed_p(o_ptr) && known) ||
		(!known && (o_ptr->ident & (IDENT_EMPTY))) ||
		(!aware && object_tried_p(o_ptr)))
		object_desc(o_name, name_size, o_ptr, TRUE, 5);
	else object_desc(o_name, name_size, o_ptr, TRUE, 3);

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
	if (f2 & (TR2_DRAIN_EXP))
	{
		text_out("This item drains experience.");
	}
	if (f2 & (TR2_STOPREGEN))
	{
		text_out("This item prevents hit point regeneration.");
	}
	return ret;
}


/* return whether the PC's class can use this spellbook */
bool junkbook(const object_type *o_ptr)
{
	int jb1, jb2, jb3, jb4, jb5, jb6;
	int badsv = cp_ptr->useless_books;

	/* not a spellbook so it can't be a junkbook */
	if (!((o_ptr->tval >= TV_MAGIC_BOOK) && (o_ptr->tval < TV_GOLD)))
		return FALSE;

	/* wrong magic realm */
	if (!(o_ptr->tval == cp_ptr->spell_book)) return TRUE;

	/* (now it gets complicated and hacky) */
	if (badsv > 99999)
	{
		/* get the sixth digit */
		jb6 = ((int)(badsv / 100000)) - 1;
		/* get rid of the sixth digit */
		badsv -= (jb6+1) * 100000;
	}
	/* don't block the sval 0 spellbook by default */
	else jb6 = -1;
	if (badsv > 9999)
	{
		/* get the fifth digit */
		jb5 = ((int)(badsv / 10000)) - 1;
		/* get rid of that digit */
		badsv -= (jb5+1) * 10000;
	}
	else jb5 = -1;
	if (badsv > 999)
	{
		/* get the fourth digit */
		jb4 = ((int)(badsv / 1000)) - 1;
		/* get rid of that digit */
		badsv -= (jb4+1) * 1000;
	}
	else jb4 = -1;
	if (badsv > 99)
	{
		/* get the third digit */
		jb3 = ((int)(badsv / 100)) - 1;
		/* get rid of that digit */
		badsv -= (jb3+1)* 100;
	}
	else jb3 = -1;
	if (badsv > 9)
	{
		/* get the second digit */
		jb2 = ((int)(badsv / 10)) - 1;
		/* get rid of that digit */
		badsv -= (jb2+1) * 10;
	}
	else jb2 = -1;
	/* there should only be one digit left */
	if (badsv > 0) jb1 = badsv - 1;
	else jb1 = -1;

	/* check for matches */
	if ((jb1 == o_ptr->sval) || (jb2 == o_ptr->sval) || (jb3 == o_ptr->sval) ||
		(jb4 == o_ptr->sval) || (jb5 == o_ptr->sval) || (jb6 == o_ptr->sval))
		return TRUE;

	/* otherwise it's not a junk book */
	return FALSE;
}

/*
 * Place an item description on the screen.
 */
void object_info_screen(object_type *o_ptr)
{
	bool has_description, has_info;
	bool weapon, ammo;
	object_type *j_ptr;
    u32b f1, f2, f3, f4;
		
	/* don't give away disguised multi-hued poison (pval is its disguise) */
	if ((o_ptr->tval == TV_POTION) && 
		(o_ptr->sval == SV_POTION_MULTIHUED_POISON) && (o_ptr->pval))
	{
		/* this calls object_info_screen() for a fake object of the */
		/* kind of potion it is disguising as (hope that works...) */
		desc_obj_fake(o_ptr->pval);
		return;
	}

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

	/* note what you found out by trying on an unIDed object */
    if ((o_ptr->pseudo == INSCRIP_TRIED) && (!object_known_p(o_ptr)))
    {
        if (obvious_excellent(o_ptr)) p_text_out("");
    }

	if (!object_known_p(o_ptr))
		text_out("\n\n  This item has not been identified.");
	else if (!has_description && !has_info)
		text_out("\n\n  This item does not seem to possess any special abilities.");

	j_ptr = &inventory[INVEN_BOW];
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	ammo   = (p_ptr->ammo_tval == o_ptr->tval) && (j_ptr->k_idx);

	/* get object flags */
	object_info_out_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* chance of failure for magic devices */
	if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND) || 
		(o_ptr->tval == TV_ROD) || (f3 & (TR3_ACTIVATE)))
	{
        if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
        {
           	new_paragraph = TRUE;
            p_text_out(format("  This device has %d charges left.", o_ptr->charges));
           	new_paragraph = FALSE;
        }                 
		if (object_known_p(o_ptr)) device_chance(o_ptr);
		/* wand/rod enhancement */
		dsc_enhance(o_ptr);
	}
	
    /* describe weapon attacks */
    if ((ammo) || (weapon) || (o_ptr->tval == TV_BOW) || (is_throwing_weapon(o_ptr)))
    {
	    describe_attack(o_ptr);
	}
    
	/* temporary branding spell */
    if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
    {
		s32b day, hour, minute, sec, bturn;
		/* convert duration to time */
		bturn = o_ptr->timedbrand * 10;
		if (bturn >= 60) minute = bturn / 60;
		else minute = 0;
		if (minute >= 60) hour = minute / 60;
		else hour = 0;
		if (hour >= 24) day = (hour / 24) + 1;
		else day = 0;
		if (bturn >= 60) sec = bturn - (minute * 60);
		else sec = bturn;
		if (minute >= 60) minute -= hour * 60;
		if (hour >= 24) hour -= (day-1) * 24;
		
		text_out("  The temporary branding enchantment on this weapon will wear off in ");
		if (day) text_out(format("%d days, ", day));
		if ((hour) && (day)) text_out(format("%d hours, and ", hour));
		else if (hour) text_out(format("%d hours and ", hour));
		if ((minute) && (hour)) text_out(format("%d minutes.", minute));
		else if (minute) text_out(format("%d minutes and ", minute));
		if (!hour) text_out(format("%d seconds.", sec));
	}

	/* describe blessed status */
	if (o_ptr->blessed > 1)
	{
		s32b day, hour, minute, sec, bturn;
		/* convert duration to time */
		bturn = (o_ptr->blessed - 1) * 10;
		if (bturn >= 60) minute = bturn / 60;
		else minute = 0;
		if (minute >= 60) hour = minute / 60;
		else hour = 0;
		if (hour >= 24) day = (hour / 24) + 1;
		else day = 0;
		if (bturn >= 60) sec = bturn - (minute * 60);
		else sec = bturn;
		if (minute >= 60) minute -= hour * 60;
		if (hour >= 24) hour -= (day-1) * 24;
		
		if (weapon) text_out("  This weapon has been temporarily blessed.");
		else if (o_ptr->tval == TV_BOW) text_out("  This bow has been temporarily blessed.");
		else text_out("  This object has been temporarily blessed.");
		text_out(" The blessing will fade in ");
		if (day) text_out(format("%d days, ", day));
		if ((hour) && (day)) text_out(format("%d hours, and ", hour));
		else if (hour) text_out(format("%d hours and ", hour));
		if ((minute) && (hour)) text_out(format("%d minutes.", minute));
		else if (minute) text_out(format("%d minutes and ", minute));
		if (!hour) text_out(format("%d seconds.", sec));
    }
    else if (((f3 & (TR3_BAD_WEAP)) || (f2 & (TR2_CORRUPT))) && (o_ptr->blessed))
    {
       text_out("  This object has had an evil enchantment removed from it,");
       text_out(" but the temporary blessing has worn off.");
    }

	if (junkbook(o_ptr)) text_out("  Your class cannot use this spellbook.");

	/* describe object origin */
	if (o_ptr->dlevel)
	{
		if (o_ptr->drop_ridx < 0)
		{
			text_out("  It was dropped by an unknown monster");
			if (o_ptr->vcode == 2) text_out(" in a vault");
		}
		else if (o_ptr->drop_ridx)
		{
			monster_race *r_ptr = &r_info[o_ptr->drop_ridx];
			cptr name = (r_name + r_ptr->name);
			if (r_ptr->flags1 & (RF1_UNIQUE))
				text_out(format("  It was dropped by %s", name));
			else text_out(format("  It was dropped by a(n) %s", name));
			if (o_ptr->vcode == 2) text_out(" in a vault");
		}
		else if (o_ptr->vcode == 1) text_out(" It was found in a vault");
		else if (o_ptr->vcode == 3) text_out(" It was found in a pile of rubble");
		else if (o_ptr->vcode == 4) text_out(" It was found in a chest");
		else if (o_ptr->vcode == 5) text_out(" It was found in a chest in a vault");
		else if (o_ptr->vcode == 6) text_out(" It was found in a silver-locked chest");
		else if (o_ptr->vcode == 7) text_out(" It was found in a gold-locked chest");
		else if (o_ptr->vcode == 8) text_out(" It was found in a pile of rubble in a vault");
		else if (o_ptr->vcode == 9) text_out(" It was created by aquirement magic");
		else if (o_ptr->vcode == 10) text_out(" It was conjured up by magic");
		else if ((o_ptr->vcode == 11) && (cheat_peek)) 
			text_out(" It was generated as a great item outside of a vault");
		else text_out(" It was found on the floor");

		text_out(format(" on level %d.", o_ptr->dlevel));
	}
	else if ((o_ptr->vcode == 13) && (!(o_ptr->ident & IDENT_STORE)))
	{
		text_out(" It was bought from the black market.");
	}
	else if ((o_ptr->vcode == 12) && (!(o_ptr->ident & IDENT_STORE)))
	{
		/* only egos, artifacts, TV_SPECIAL, TV_CHEST and a few powerful items are tracked */
		/* of these, only egos can be generated in stores other than the BM */
		if ((o_ptr->tval >= TV_SHOT) && (o_ptr->tval <= TV_SWORD))
			text_out(" It was bought from the weapon shop.");
		else if ((o_ptr->tval >= TV_BOOTS) && (o_ptr->tval <= TV_HARD_ARMOR))
			text_out(" It was bought from the armoury.");
		else if (o_ptr->tval == TV_STAFF)
			text_out(" It was bought from the magic shop.");
	}

	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");

	/* Wait for input */
	(void)anykey();

	/* Load the screen */
	screen_load();

	/* Hack -- Browse book, then prompt for a command */
	if ((o_ptr->tval == cp_ptr->spell_book) && (!junkbook(o_ptr)))
	{
		/* Call the aux function */
		do_cmd_browse_aux(o_ptr);
	}
}
