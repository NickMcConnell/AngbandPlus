/* File: object1.c */

/* Purpose: Object code, part 1 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "script.h"


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(void)
{
	int i;

	/* Extract some info about terrain features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;

		/* No extra information */
		f_ptr->w_attr = 0;
		f_ptr->w_char = 0;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Extract default attr/char code for fields */
	for (i = 0; i < z_info->t_max; i++)
	{
		field_thaum *t_ptr = &t_info[i];

		/* Default attr/char */
		t_ptr->f_attr = t_ptr->d_attr;
		t_ptr->f_char = t_ptr->d_char;
	}


	if (use_graphics)
	{
		/* Process "graf.prf" */
		(void)process_pref_file("graf.prf");
	}

	/* Normal symbols */
	else
	{
		/* Process "font.prf" */
		(void)process_pref_file("font.prf");
	}

	/* Reset the fake monochrome flag */
	fake_monochrome = (!use_graphics
					   || streq(ANGBAND_SYS, "ibm")) ? TRUE : FALSE;

	/* Fields have to notice the change of visuals. */
	init_fields();
	
	/* Update map to notice change in visuals */
	update_overhead_map();
}


/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, object_flags *of_ptr)
{
	const object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool known = object_known_p(o_ptr);

	/* Clear */
	of_ptr->flags[0] = 0L;
	of_ptr->flags[1] = 0L;
	of_ptr->flags[2] = 0L;
	of_ptr->flags[3] = 0L;
	
	if (cursed_p(o_ptr) && (known || (o_ptr->info & (OB_SENSE))))
	{
		SET_FLAG(of_ptr, TR_CURSED);
	}

	/* Must be identified */
	if (!known) return;

	/* Base object */
	of_ptr->flags[0] = k_ptr->flags[0];
	of_ptr->flags[1] = k_ptr->flags[1];
	of_ptr->flags[2] = k_ptr->flags[2];
	of_ptr->flags[3] = k_ptr->flags[3];

	/* Show modifications to stats (can't use FLAG() here.) */
	of_ptr->flags[0] |= (o_ptr->flags[0] & TR0_EASY_MASK);

	/* 
	 * *Identify* sets these flags,
	 * and ego items have some set on creation.
	 */
	of_ptr->flags[0] |= o_ptr->kn_flags[0];
	of_ptr->flags[1] |= o_ptr->kn_flags[1];
	of_ptr->flags[2] |= o_ptr->kn_flags[2];
	of_ptr->flags[3] |= o_ptr->kn_flags[3];

	/* We now now whether or not it is an artifact */
	COPY_FLAG(o_ptr, of_ptr, TR_INSTA_ART);
}


static char string_buf[200];
/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(const object_type *o_ptr)
{
	cptr desc = NULL;
	
	/* Empty string */
	string_buf[0] = '\0';
	
	/* Require activation ability */
	if (!(FLAG(o_ptr, TR_ACTIVATE))) return ("nothing");

	/* Get description and copy to temporary buffer */
	/* Lua better not try to modify the object ... */
	apply_object_trigger(TRIGGER_DESC, (object_type *) o_ptr, ":s", LUA_RETURN(desc));
	
	if (desc)
	{
		strncpy(string_buf, desc, 199);

		/* Free string allocated to hold return value */
		string_free(desc);
	}
	
	/* Return the description */
	return string_buf;
}

#define TR0_STAT_MASK \
	(TR0_STR | TR0_INT | TR0_WIS | TR0_DEX | TR0_CON | TR0_CHR)
#define TR1_SUST_MASK \
	(TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS | \
	 TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR)

/*
 * Fully describe the known information about an item
 */
static void roff_obj_aux(const object_type *o_ptr)
{
	object_kind *k_ptr;
	bonuses_type b;
	
	int i, n;

	object_flags oflags;
	object_flags *of_ptr = &oflags;

	int vn;
	cptr vp[80];

	k_ptr = &k_info[o_ptr->k_idx];

	/* Extract the flags */
	object_flags_known(o_ptr, of_ptr);

	/* Extract the bonuses */
	object_bonuses_known(o_ptr, &b);

	/* Make sure the examination starts top left */
	Term_gotoxy(0, 0);

	/* Show the item in a message including its pack letter */
	item_describe_roff((object_type *)o_ptr);

	/* Start the description a bit lower */
	roff("\n\n");

	/* If you don't know anything about the item */
	if (!object_known_p(o_ptr) && !object_aware_p(o_ptr))
	{
		/* say so */
		roff("You see nothing special.");
	}

	/* Hack.  Not all armour and weapons have a description in k_idx.txt */
	if (o_ptr->tval >= TV_HAFTED &&
		o_ptr->tval <= TV_DRAG_ARMOR &&
		!k_ptr->text)
	{
		/* If the object has no id or has no interesting flags */
		if (!object_known_p(o_ptr) ||
			(o_ptr->flags[0] == 0 &&
			 o_ptr->flags[1] == TR2_SHOW_MODS &&
			 o_ptr->flags[2] == 0 &&
			 o_ptr->flags[3] == 0))
		{
			/* say nothing is known */
			roff("You see nothing special.");
		}
	}


	/* Indicate if fully known */
	if (object_known_full(o_ptr))
	{
		roff("You have full knowledge of this item.  ");
	}

	/* Add the 'description' if any */
	if (object_known_p(o_ptr) || object_aware_p(o_ptr))
	{
		artifact_type *a_ptr = NULL;
		if (o_ptr->a_idx) a_ptr = &a_info[o_ptr->a_idx];

		if (a_ptr && a_ptr->text)
		{
			roff("%s  ", a_text + a_ptr->text);
		}
		else if (k_ptr->text)
		{
			roff("%s  ", k_text + k_ptr->text);
		}
	}

	/* Mega-Hack -- describe activation if item is identified */
	if ((FLAG(o_ptr, TR_ACTIVATE)) && object_known_p(o_ptr))
	{
		roff("It can be activated for ");
		roff(CLR_UMBER "%s", item_activation(o_ptr));
		roff(" if it is being worn.  ");
	}

	/* Figurines, a hack */
	if (o_ptr->tval == TV_FIGURINE)
	{
		roff("It will transform into a pet when thrown.  ");
	}

	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
		if (FLAG(o_ptr, TR_INSTA_ART))
		{
			roff("It provides light (radius 3).  ");
		}
		else if (o_ptr->sval == SV_LITE_LANTERN)
		{
			roff("It provides light (radius 2) when fueled.  ");
		}
		else
		{
			roff("It provides light (radius 1) when fueled.  ");
		}
	}


	/* And then describe it fully */

	for (i = 99; i >= -99; i--)
	{
		if (!i) continue;
	
		/* Collect stat boosts */
		vn = 0;

		if (b.stat[A_STR] == i) vp[vn++] = "strength";
		if (b.stat[A_INT] == i) vp[vn++] = "intelligence";
		if (b.stat[A_WIS] == i) vp[vn++] = "wisdom";
		if (b.stat[A_DEX] == i) vp[vn++] = "dexterity";
		if (b.stat[A_CON] == i) vp[vn++] = "constitution";
		if (b.stat[A_CHR] == i) vp[vn++] = "charisma";

		/* All stats is handled specially */
		if (vn == 6)
		{
			vn = 0;
			vp[vn++] = "all your stats";
		}

		if (b.pspeed == i)   vp[vn++] = "speed";
		if (b.skills[SKILL_STL] == i) vp[vn++] = "stealth";
		if (b.skills[SKILL_SNS] / 5 == i)  vp[vn++] = "perception";
		if (b.skills[SKILL_DIG] / 20 == i)  vp[vn++] = "ability to dig";
		if (b.skills[SKILL_SAV] == i) vp[vn++] = "saving throws";

		/* Describe stat boosts */
		if (vn > 0)
		{
			if (i > 0)
				roff("It increases ");
			else
				roff("It decreases ");

			/* Omit "your" for "all stats" */
			if (strncmp(vp[0], "all ", 4) != 0)
				roff("your ");

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				if (n > 0 && n == vn - 1) roff(" and ");
				else if (n > 0)  roff(", ");

				roff(CLR_L_GREEN "%s", vp[n]);
			}

			roff(" by %+i.  ", i);
		}
	}

	if (b.sp_bonus)
	{
		if (b.sp_bonus > 0)
		{
			roff("It increases your ");
		}
		else
		{
			roff("It decreases your ");
		}
		roff(CLR_L_GREEN "maximum sp" CLR_DEFAULT " by %i per level.  ", b.sp_bonus);
	}

	if (b.see_infra)
	{
		if (b.see_infra > 0)
		{
			roff("It increases your ");
			roff(CLR_L_GREEN "infravision");
			roff(" by %i feet.  ", b.see_infra * 10);
		}
		else
		{
			roff("It decreases your ");
			roff(CLR_L_GREEN "infravision");
			roff(" by %i feet.  ", -b.see_infra * 10);
		}
	}

	/* Food that can be thrown is worth noting */
	if (k_ptr->ds > 1 && k_ptr->dd > 1 && k_ptr->tval == TV_FOOD)
	{
		roff("It can be thrown for %id%i damage.  ",
			k_ptr->dd, k_ptr->ds);
	}

	if (b.extra_blows)
	{
		if (b.extra_blows > 0)
		{
			roff("It provides %i extra ", b.extra_blows);
		}
		else
		{
			roff("It provides %i fewer ", -b.extra_blows);
		}
		roff(CLR_L_GREEN "blows per turn" CLR_DEFAULT ".  ");
	}

	if (b.extra_shots)
	{
		if (b.extra_shots > 0)
		{
			roff("It provides %i extra ", b.extra_shots);
		}
		else
		{
			roff("It provides %i fewer ", -b.extra_shots);
		}
		roff(CLR_L_GREEN "shots per turn" CLR_DEFAULT ".  ");
	}

	/* Collect brands */
	vn = 0;
	if (FLAG(of_ptr, TR_BRAND_ACID)) vp[vn++] = "acid";
	if (FLAG(of_ptr, TR_BRAND_ELEC)) vp[vn++] = "electricity";
	if (FLAG(of_ptr, TR_BRAND_FIRE)) vp[vn++] = "fire";
	if (FLAG(of_ptr, TR_BRAND_COLD)) vp[vn++] = "frost";

	/* Describe brands */
	if (vn)
	{
		roff("It does extra damage from ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");

			roff(CLR_VIOLET "%s", vp[n]);
		}

		roff(".  ");
	}

	if (FLAG(of_ptr, TR_BRAND_POIS))
	{
		roff("It " CLR_VIOLET "poisons" CLR_DEFAULT " your foes.  ");
	}

	if (FLAG(of_ptr, TR_CHAOTIC))
	{
		roff("It produces " CLR_VIOLET "chaotic effects" CLR_DEFAULT ".  ");
	}

	if (FLAG(of_ptr, TR_VAMPIRIC))
	{
		roff("It " CLR_VIOLET "drains life" CLR_DEFAULT " from your foes.  ");
	}

	if (FLAG(of_ptr, TR_IMPACT))
	{
		roff("It can cause " CLR_VIOLET "earthquakes" CLR_DEFAULT ".  ");
	}

	if (FLAG(of_ptr, TR_VORPAL))
	{
		roff("It is very sharp and can cut your foes.  ");
	}

	if (o_ptr->tval >= TV_DIGGING && o_ptr->tval <= TV_SWORD)
	{
		if (FLAG(of_ptr, TR_KILL_DRAGON))
		{
			roff("It is a great bane of " CLR_YELLOW "dragons" CLR_DEFAULT ".  ");
		}

		/* Collect slays */
		vn = 0;
		if (FLAG(of_ptr, TR_SLAY_DRAGON) && !FLAG(of_ptr, TR_KILL_DRAGON))
			vp[vn++] = "dragons";
		if (FLAG(of_ptr, TR_SLAY_ORC))    vp[vn++] = "orcs";
		if (FLAG(of_ptr, TR_SLAY_TROLL))  vp[vn++] = "trolls";
		if (FLAG(of_ptr, TR_SLAY_GIANT))  vp[vn++] = "giants";
		if (FLAG(of_ptr, TR_SLAY_DEMON))  vp[vn++] = "demons";
		if (FLAG(of_ptr, TR_SLAY_UNDEAD)) vp[vn++] = "the undead";
		if (FLAG(of_ptr, TR_SLAY_EVIL))   vp[vn++] = "evil monsters";
		if (FLAG(of_ptr, TR_SLAY_ANIMAL)) vp[vn++] = "natural creatures";

		/* Print slays */
		if (vn)
		{
			roff("It is especially deadly against ");

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				if (n > 0 && n == vn - 1) roff(" and ");
				else if (n > 0)  roff(", ");
	
				roff(CLR_YELLOW "%s", vp[n]);
			}

			roff(".  ");
		}
	}

	if (FLAG(of_ptr, TR_GHOUL_TOUCH))
	{
		roff("It gives you a paralyzing touch.  ");
	}
	if (FLAG(of_ptr, TR_PSI_CRIT))
	{
		roff("It uses psychic energy to strike great blows.  ");
	}
	if (FLAG(of_ptr, TR_RETURN))
	{
		roff("It returns when thrown.  ");
	}
	if (FLAG(of_ptr, TR_EXPLODE))
	{
		roff("It explodes when fired.  ");
	}

	/* Collect sustains */
	if ((of_ptr->flags[1] & TR1_SUST_MASK) == TR1_SUST_MASK)
	{
		/* Handle all stats specially */
		roff("It sustains " CLR_GREEN "all your stats" CLR_DEFAULT ".  ");
	}
	else
	{
		vn = 0;
		if (FLAG(of_ptr, TR_SUST_STR)) vp[vn++] = "strength";
		if (FLAG(of_ptr, TR_SUST_INT)) vp[vn++] = "intelligence";
		if (FLAG(of_ptr, TR_SUST_WIS)) vp[vn++] = "wisdom";
		if (FLAG(of_ptr, TR_SUST_DEX)) vp[vn++] = "dexterity";
		if (FLAG(of_ptr, TR_SUST_CON)) vp[vn++] = "constitution";
		if (FLAG(of_ptr, TR_SUST_CHR)) vp[vn++] = "charisma";

		/* Print sustains */
		if (vn)
		{
			roff("It sustains your ");

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				if (n > 0 && n == vn - 1) roff(" and ");
				else if (n > 0)  roff(", ");
	
				roff(CLR_GREEN "%s", vp[n]);
			}

			roff(".  ");
		}
	}
	
	/* Collect immunities */
	vn = 0;
	if (FLAG(of_ptr, TR_IM_ACID)) vp[vn++] = "acid";
	if (FLAG(of_ptr, TR_IM_ELEC)) vp[vn++] = "electricity";
	if (FLAG(of_ptr, TR_IM_FIRE)) vp[vn++] = "fire";
	if (FLAG(of_ptr, TR_IM_COLD)) vp[vn++] = "cold";
	if (FLAG(of_ptr, TR_IM_POIS)) vp[vn++] = "poison";
	if (FLAG(of_ptr, TR_IM_LITE)) vp[vn++] = "light";
	if (FLAG(of_ptr, TR_IM_DARK)) vp[vn++] = "darkness";
	if (FLAG(of_ptr, TR_FREE_ACT)) vp[vn++] = "paralysis";

	/* Print immunities */
	if (vn)
	{
		roff("It provides immunity to ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_BLUE "%s", vp[n]);
		}

		roff(".  ");
	}

	/* Collect resistances */
	vn = 0;
	if (FLAG(of_ptr, TR_RES_ACID) &&
		!FLAG(of_ptr, TR_IM_ACID))   vp[vn++] = "acid";
	if (FLAG(of_ptr, TR_RES_ELEC) &&
		!FLAG(of_ptr, TR_IM_ELEC))   vp[vn++] = "electricity";
	if (FLAG(of_ptr, TR_RES_FIRE) &&
		!FLAG(of_ptr, TR_IM_FIRE))   vp[vn++] = "fire";
	if (FLAG(of_ptr, TR_RES_COLD) &&
		!FLAG(of_ptr, TR_IM_COLD))   vp[vn++] = "cold";
	if (FLAG(of_ptr, TR_RES_POIS) &&
		!FLAG(of_ptr, TR_IM_POIS))   vp[vn++] = "poison";
	if (FLAG(of_ptr, TR_RES_LITE) &&
		!FLAG(of_ptr, TR_IM_LITE))   vp[vn++] = "bright light";
	if (FLAG(of_ptr, TR_RES_DARK) &&
		!FLAG(of_ptr, TR_IM_DARK))   vp[vn++] = "magical darkness";
	if (FLAG(of_ptr, TR_RES_FEAR))   vp[vn++] = "fear";
	if (FLAG(of_ptr, TR_RES_BLIND))  vp[vn++] = "blindness";
	if (FLAG(of_ptr, TR_RES_CONF))   vp[vn++] = "confusion";
	if (FLAG(of_ptr, TR_RES_SOUND))  vp[vn++] = "sound";
	if (FLAG(of_ptr, TR_RES_SHARDS)) vp[vn++] = "shards";
	if (FLAG(of_ptr, TR_RES_NETHER)) vp[vn++] = "nether";
	if (FLAG(of_ptr, TR_RES_NEXUS))  vp[vn++] = "nexus";
	if (FLAG(of_ptr, TR_RES_CHAOS))  vp[vn++] = "chaos";
	if (FLAG(of_ptr, TR_RES_DISEN))  vp[vn++] = "disenchantment";
	if (FLAG(of_ptr, TR_HOLD_LIFE))  vp[vn++] = "life draining";

	/* Print resistances */
	if (vn)
	{
		roff("It provides resistance to ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_L_BLUE "%s", vp[n]);
		}

		roff(".  ");
	}

	if (FLAG(of_ptr, TR_THROW))
	{
		roff("It is perfectly balanced for throwing.  ");
	}

	if (FLAG(of_ptr, TR_WILD_SHOT))
	{
		roff("Its shots are not hindered by trees.  ");
	}

	if (FLAG(of_ptr, TR_EASY_ENCHANT))
	{
		roff("It is easy to enchant.  ");
	}

	/* Collect miscellaneous */
	vn = 0;
	if (FLAG(of_ptr, TR_XXX7))        vp[vn++] = "renders you XXX7'ed";
	if (FLAG(of_ptr, TR_FEATHER))     vp[vn++] = "allows you to levitate";
	if (FLAG(of_ptr, TR_LITE))        vp[vn++] = "provides permanent light";
	if (FLAG(of_ptr, TR_SEE_INVIS))   vp[vn++] = "allows you to see invisible monsters";
	if (FLAG(of_ptr, TR_TELEPATHY))   vp[vn++] = "gives telepathic powers";
	if (FLAG(of_ptr, TR_SLOW_DIGEST)) vp[vn++] = "slows your metabolism";
	if (FLAG(of_ptr, TR_REGEN))       vp[vn++] = "speeds your regenerative powers";
	if (FLAG(of_ptr, TR_REFLECT))     vp[vn++] = "reflects bolts and arrows";
	if (FLAG(of_ptr, TR_WILD_WALK))     vp[vn++] = "allows you to walk the wild unhindered";
	if (FLAG(of_ptr, TR_MUTATE))        vp[vn++] = "causes mutations";
	if (FLAG(of_ptr, TR_PATRON))        vp[vn++] = "attracts the attention of chaos gods";
	if (FLAG(of_ptr, TR_STRANGE_LUCK))  vp[vn++] = "warps fate around you";
	if (FLAG(of_ptr, TR_PASS_WALL))     vp[vn++] = "allows you to pass through solid rock";
	if (FLAG(of_ptr, TR_NO_TELE))     vp[vn++] = "prevents teleportation";

	/* Print miscellaneous */
	if (vn)
	{
		roff("It ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff("%s", vp[n]);
		}

		roff(".  ");
	}

	/* Collect "produces" */
	vn = 0;
	if (FLAG(of_ptr, TR_SH_FIRE))  vp[vn++] = "a fiery sheath";
	if (FLAG(of_ptr, TR_SH_ELEC))  vp[vn++] = "an electric sheath";
	if (FLAG(of_ptr, TR_SH_ACID))  vp[vn++] = "an acidic sheath";
	if (FLAG(of_ptr, TR_SH_COLD))  vp[vn++] = "a freezing sheath";
	if (FLAG(of_ptr, TR_NO_MAGIC)) vp[vn++] = "an anti-magic shell";

	/* Print "produces" */
	if (vn)
	{
		roff("It produces ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_VIOLET "%s", vp[n]);
		}

		roff(".  ");
	}

	if (FLAG(of_ptr, TR_XTRA_MIGHT))
	{
		roff("It fires missiles with " CLR_GREEN "extra might" CLR_DEFAULT ".  ");
	}

	/* Collect curses */
	vn = 0;
	if (FLAG(of_ptr, TR_DRAIN_EXP))   vp[vn++] = "drains your experience";
	if (FLAG(of_ptr, TR_DRAIN_STATS)) vp[vn++] = "drains your stats";
	if (FLAG(of_ptr, TR_TELEPORT))    vp[vn++] = "induces random teleportation";
	if (FLAG(of_ptr, TR_AGGRAVATE))     vp[vn++] = "aggravates nearby creatures";
	if (FLAG(of_ptr, TR_AUTO_CURSE))  vp[vn++] = "becomes cursed randomly";
	if (FLAG(of_ptr, TR_CANT_EAT))    vp[vn++] = "makes you unable to eat normal food";
	if (FLAG(of_ptr, TR_SLOW_HEAL))   vp[vn++] = "slows your healing";

	/* Print curses */
	if (vn)
	{
		roff("It ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_RED "%s", vp[n]);
		}

		roff(".  ");
	}

	if (FLAG(of_ptr, TR_BLESSED))
	{
		roff("It has been blessed by the gods.  ");
	}

	/* Collect protections */
	vn = 0;
	if (FLAG(of_ptr, TR_SLAY_ANIMAL)) vp[vn++] = "natural creatures";
	if (FLAG(of_ptr, TR_SLAY_EVIL))   vp[vn++] = "evil monsters";
	if (FLAG(of_ptr, TR_SLAY_UNDEAD)) vp[vn++] = "the undead";
	if (FLAG(of_ptr, TR_SLAY_DEMON))  vp[vn++] = "demons";
	if (FLAG(of_ptr, TR_SLAY_ORC))    vp[vn++] = "orcs";
	if (FLAG(of_ptr, TR_SLAY_TROLL))  vp[vn++] = "trolls";
	if (FLAG(of_ptr, TR_SLAY_GIANT))  vp[vn++] = "giants";
	if (FLAG(of_ptr, TR_SLAY_DRAGON)) vp[vn++] = "dragons";

	/* Print protections */
	if (vn)
	{
		roff("It provides protection from ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_BLUE "%s", vp[n]);
		}

		roff(".  ");
	}

	/* Collect vulnerabilities */
	vn = 0;
	if (FLAG(of_ptr, TR_HURT_ACID) &&
		!FLAG(of_ptr, TR_IM_ACID)) vp[vn++] = "acid";
	if (FLAG(of_ptr, TR_HURT_ELEC) &&
		!FLAG(of_ptr, TR_IM_ELEC)) vp[vn++] = "lightning";
	if (FLAG(of_ptr, TR_HURT_FIRE) &&
		!FLAG(of_ptr, TR_IM_FIRE)) vp[vn++] = "fire";
	if (FLAG(of_ptr, TR_HURT_COLD) &&
		!FLAG(of_ptr, TR_IM_COLD)) vp[vn++] = "frost";
	if (FLAG(of_ptr, TR_HURT_LITE) &&
		!FLAG(of_ptr, TR_IM_LITE)) vp[vn++] = "bright light";
	if (FLAG(of_ptr, TR_HURT_DARK) &&
		!FLAG(of_ptr, TR_IM_DARK)) vp[vn++] = "magical darkness";

	/* Print vulnerabilities */
	if (vn)
	{
		roff("It renders you vulnerable to ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			if (n > 0 && n == vn - 1) roff(" and ");
			else if (n > 0)  roff(", ");
	
			roff(CLR_RED "%s", vp[n]);
		}

		roff(".  ");
	}

	if (cursed_p(o_ptr))
	{
		if (FLAG(of_ptr, TR_PERMA_CURSE))
		{
			roff(CLR_L_RED "It is permanently cursed.  ");
		}
		else if (FLAG(of_ptr, TR_HEAVY_CURSE))
		{
			roff(CLR_L_RED "It is heavily cursed.  ");
		}
		else if (FLAG(of_ptr, TR_CURSED))
		{
			roff(CLR_RED "It is cursed.  ");
		}
	}

	if (FLAG(of_ptr, TR_TY_CURSE))
	{
		roff(CLR_L_RED "It carries an ancient foul curse.  ");
	}

	if ((of_ptr->flags[2] & TR2_IGNORE_MASK) == TR2_IGNORE_MASK)
	{
		roff("It cannot be harmed by the elements.  ");
	}
	else
	{
		/* Collect ignores */
		vn = 0;
		if (FLAG(of_ptr, TR_IGNORE_ACID)) vp[vn++] = "acid";
		if (FLAG(of_ptr, TR_IGNORE_ELEC)) vp[vn++] = "electricity";
		if (FLAG(of_ptr, TR_IGNORE_FIRE)) vp[vn++] = "fire";
		if (FLAG(of_ptr, TR_IGNORE_COLD)) vp[vn++] = "cold";

		/* Print ignores */
		if (vn)
		{
			roff("It cannot be harmed by ");

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				if (n > 0 && n == vn - 1) roff(" or ");
				else if (n > 0)  roff(", ");
	
				roff("%s", vp[n]);
			}

			roff(".  ");
		}
	}

	/* If it is a weapon */
	if (o_ptr->tval >= TV_BOW && o_ptr->tval <= TV_SWORD)
	{
		/* Obtain the "hold" value */
		int hold = adj_str_hold[p_ptr->stat[A_STR].ind];

		/* If you are not strong enough for this weapon */
		if (hold < o_ptr->weight / 10)
		{
			roff("You are not strong enough to wield this weapon effectively.  ");
		}
	}
	
	/* Final blank line */
	roff("\n");
}

static const object_type *resize_o_ptr;

static void resize_ident_fully(void)
{
	/* Recall object */
	roff_obj_aux(resize_o_ptr);
}


void identify_fully_aux(const object_type *o_ptr)
{
	void (*old_hook) (void);

	/* Books, a hack */
	if ((o_ptr->tval >= TV_BOOKS_MIN) && (o_ptr->tval <= TV_BOOKS_MAX))
	{
		do_cmd_browse_aux(o_ptr);
		return;
	}

	/* Save the screen */
	screen_save();

	/* Recall object */
	roff_obj_aux(o_ptr);

	/* Remember what the resize hook was */
	old_hook = angband_term[0]->resize_hook;

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = resize_ident_fully;

	/* Remember essentials for resizing */
	resize_o_ptr = o_ptr;

	/* Wait for the player to read the info */
	(void)inkey();
	
	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = old_hook;

	/* The size may have changed during the object description */
	angband_term[0]->resize_hook();

	/* Hack - Flush it */
	Term_fresh();

	/* Restore the screen */
	screen_load();
}

/*
 * Convert a label into the object pointer
 *  to an item in a list.
 * Return NULL if the label does not indicate a real item.
 */
static object_type *label_to_list(int c, s16b list_start)
{
	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1);

	/* Return the item */
	return (get_list_item(list_start, i));
}

/*
 * Convert a label into the object pointer
 *  to an item in the equipment.
 * Return NULL if the label does not indicate a real item.
 */
static object_type *label_to_equip(int c)
{
	object_type *o_ptr;

	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i >= EQUIP_MAX)) return (NULL);

	/* Get the item */
	o_ptr = &p_ptr->equipment[i];

	/* Empty slots can never be chosen */
	if (!o_ptr->k_idx) return (NULL);

	/* Return the item */
	return (o_ptr);
}

/*
 * The the "choosable" range of objects from the list.
 */
static void get_label_bounds(s16b list, int *n1, int *n2)
{
	object_type *o_ptr;

	int i = -1;

	*n1 = -1;
	*n2 = -1;

	OBJ_ITT_START (list, o_ptr)
	{
		i++;

		if (item_tester_okay(o_ptr))
		{
			/* Get lower bounds */
			if (*n1 == -1) *n1 = i;

			/* Get higher bounds */
			*n2 = i;
		}
	}
	OBJ_ITT_END;
}


/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (EQUIP_WIELD);
		}

		case TV_BOW:
		{
			return (EQUIP_BOW);
		}

		case TV_RING:
		{
			/* Use the right hand first */
			if (!p_ptr->equipment[EQUIP_RIGHT].k_idx) return (EQUIP_RIGHT);

			/* Use the left hand for swapping (by default) */
			return (EQUIP_LEFT);
		}

		case TV_AMULET:
		{
			return (EQUIP_NECK);
		}

		case TV_LITE:
		{
			return (EQUIP_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (EQUIP_BODY);
		}

		case TV_CLOAK:
		{
			return (EQUIP_OUTER);
		}

		case TV_SHIELD:
		{
			return (EQUIP_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (EQUIP_HEAD);
		}

		case TV_GLOVES:
		{
			return (EQUIP_HANDS);
		}

		case TV_BOOTS:
		{
			return (EQUIP_FEET);
		}
	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
cptr mention_use(int i)
{
	cptr p;

	/* Examine the location */
	switch (i)
	{
		case EQUIP_WIELD:
		{
			p = "Wielding";
			break;
		}
		case EQUIP_BOW:
		{
			p = "Shooting";
			break;
		}
		case EQUIP_LEFT:
		{
			p = "On left hand";
			break;
		}
		case EQUIP_RIGHT:
		{
			p = "On right hand";
			break;
		}
		case EQUIP_NECK:
		{
			p = "Around neck";
			break;
		}
		case EQUIP_LITE:
		{
			p = "Light source";
			break;
		}
		case EQUIP_BODY:
		{
			p = "On body";
			break;
		}
		case EQUIP_OUTER:
		{
			p = "About body";
			break;
		}
		case EQUIP_ARM:
		{
			p = "On arm";
			break;
		}
		case EQUIP_HEAD:
		{
			p = "On head";
			break;
		}
		case EQUIP_HANDS:
		{
			p = "On hands";
			break;
		}
		case EQUIP_FEET:
		{
			p = "On feet";
			break;
		}
		default:
		{
			p = "In pack";
			break;
		}
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr;
		o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat[A_STR].ind] < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr;
		o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat[A_STR].ind] < o_ptr->weight / 10)
		{
			p = "Just holding";
		}
	}

	/* Return the result */
	return (p);
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(int i)
{
	cptr p;

	switch (i)
	{
		case EQUIP_WIELD:
		{
			p = "attacking monsters with";
			break;
		}
		case EQUIP_BOW:
		{
			p = "shooting missiles with";
			break;
		}
		case EQUIP_LEFT:
		{
			p = "wearing on your left hand";
			break;
		}
		case EQUIP_RIGHT:
		{
			p = "wearing on your right hand";
			break;
		}
		case EQUIP_NECK:
		{
			p = "wearing around your neck";
			break;
		}
		case EQUIP_LITE:
		{
			p = "using to light the way";
			break;
		}
		case EQUIP_BODY:
		{
			p = "wearing on your body";
			break;
		}
		case EQUIP_OUTER:
		{
			p = "wearing on your back";
			break;
		}
		case EQUIP_ARM:
		{
			p = "wearing on your arm";
			break;
		}
		case EQUIP_HEAD:
		{
			p = "wearing on your head";
			break;
		}
		case EQUIP_HANDS:
		{
			p = "wearing on your hands";
			break;
		}
		case EQUIP_FEET:
		{
			p = "wearing on your feet";
			break;
		}
		default:
		{
			p = "invalid item to describe_use()!";
			break;
		}
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat[A_STR].ind] < o_ptr->weight / 10)
		{
			p = "just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat[A_STR].ind] < o_ptr->weight / 10)
		{
			p = "just holding";
		}
	}

	/* Return the result */
	return (p);
}

/*
 * Hook to specify "tval"
 */
bool item_tester_hook_tval(const object_type *o_ptr, byte tval)
{
	return (o_ptr->tval == tval);
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "melee weapon"
 */
bool item_tester_hook_melee_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "non-sword melee"
 */
bool item_tester_hook_nonsword(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_POLEARM))
	{
		return (TRUE);
	}

	return (FALSE);
}



/*
 * Hook to specify "ammo"
 */
bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "Bows+arrows"
 */
bool item_tester_hook_fletcher(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "armour" without acid resistance
 */
bool item_tester_hook_armour_no_acid(const object_type *o_ptr)
{
	/* If this item is an armour and is not acid proof */
	if (item_tester_hook_armour(o_ptr) &&
		!FLAG(o_ptr, TR_IGNORE_ACID)) return (TRUE);

	return (FALSE);
}

/*
 * Hook to specify "soft armour"
 */
bool item_tester_hook_soft_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SOFT_ARMOR:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "hard armour"
 */
bool item_tester_hook_hard_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_CROWN:
		case TV_HELM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "helm or crown"
 */
bool item_tester_hook_helm(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_CROWN:
		case TV_HELM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "pure hard armour"
 */
bool item_tester_hook_pure_hard_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Check if an object is weapon or armour (but not arrow, bolt, or shot)
 */
bool item_tester_hook_weapon_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOW:
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * The "wearable" tester
 */
bool item_tester_hook_wear(const object_type *o_ptr)
{
	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= EQUIP_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


/*
 * Determine if something is rechargable.
 */
bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Determine if something is a 'jewel'
 */
bool item_tester_hook_jewel(const object_type *o_ptr)
{
	/* Rings */
	if (o_ptr->tval == TV_RING) return (TRUE);

	/* Amulets */
	if (o_ptr->tval == TV_AMULET) return (TRUE);

	/* Nope */
	return (FALSE);
}

bool item_tester_hook_is_blessed(const object_type *o_ptr)
{
	object_flags oflags;
	object_flags_known(o_ptr, &oflags);

	/* Is it blessed? */
	if (FLAG(&oflags, TR_BLESSED)) return (TRUE);

	/* Check for unallowable weapons */
	if ((o_ptr->tval == TV_SWORD)
		|| (o_ptr->tval == TV_POLEARM)) return (FALSE);

	/* Everthing else is ok */
	return (TRUE);
}

bool item_tester_hook_is_good(const object_type *o_ptr)
{
	if (!object_known_p(o_ptr)) return (FALSE);

	if (cursed_p(o_ptr)) return (FALSE);

	/* Ego item or artifact */
	if (o_ptr->xtra_name) return (TRUE);

	/* Positve AC bonus */
	if (o_ptr->to_a > 0) return (TRUE);

	/* Good attack + defence */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (TRUE);

	/* Everthing else isn't good */
	return (FALSE);
}


bool item_tester_hook_is_great(const object_type *o_ptr)
{
	if (!object_known_p(o_ptr)) return (FALSE);

	if (cursed_p(o_ptr)) return (FALSE);

	/* Ego item or artifact */
	if (o_ptr->xtra_name) return (TRUE);

	/* Everthing else isn't great */
	return (FALSE);
}


bool item_tester_hook_is_book(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
		case TV_LIFE_BOOK:

			/* It is a book */
			return (TRUE);
	}

	/* It isn't a book */
	return (FALSE);
}


/* Hack: Check if a spellbook is one of the realms we can use. -- TY */

static bool check_book_realm(const byte book_tval)
{
	return (REALM1_BOOK == book_tval || REALM2_BOOK == book_tval);
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(const object_type *o_ptr)
{
	/* Paranoia */
	if (!o_ptr) return (FALSE);

	/* Hack -- allow listing empty slots */
	if (item_tester_full) return (TRUE);

	/* Require an item */
	if (!o_ptr->k_idx) return (FALSE);

	/* Hack -- ignore "gold" */
	if (o_ptr->tval == TV_GOLD) return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		/* Is it a spellbook? If so, we need a hack -- TY */
		if ((item_tester_tval <= TV_BOOKS_MAX) &&
			(item_tester_tval >= TV_BOOKS_MIN))
			return check_book_realm(o_ptr->tval);
		else if (item_tester_tval != o_ptr->tval) return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook) (o_ptr)) return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}

static bool item_is_recharging(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	int power;

	/* Is the item recharging? */
	if (o_ptr->timeout &&
		((o_ptr->tval != TV_LITE) || (FLAG(o_ptr, TR_INSTA_ART))))
	{
		if (o_ptr->tval == TV_ROD)
		{
			/* Paranoia. */
			if (!k_ptr->pval) return (FALSE);

			/*
			 * Find out how many rods are charging, by dividing
			 * current timeout by each rod's maximum timeout.
			 */
			power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			/* Only darken fully-charging stacks. */
			if (power >= o_ptr->number) return (TRUE);
		}
		else
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	int i = 0;
	object_type *o_ptr;
	cptr attr;

	char tmp_val[80];

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Display the pack */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = ' ';
		tmp_val[2] = 0;

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr))
		{
			/* Prepare an "index" */
			tmp_val[0] = I2A(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		prtf(0, i, tmp_val);

		/* Get a color */
		attr = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) attr = CLR_L_DARK;

		/* Display the entry itself */
		put_fstr(3, i, "%s" CLR_SET_DEFAULT "%v", attr, OBJECT_FMT(o_ptr, TRUE, 3));

		/* Display the weight if needed */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			prtf(wid - 9, i, "%3d.%1d lb", wgt / 10, wgt % 10);
		}

		/* Count items in inventory */
		i++;
	}
	OBJ_ITT_END;

	/* Erase the rest of the window */
    clear_from(i);
}


/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	int i;
	object_type *o_ptr;
	cptr attr;
	char tmp_val[80];

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Display the equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Examine the item */
		o_ptr = &p_ptr->equipment[i];

		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = ' ';
		tmp_val[2] = 0;

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr))
		{
			/* Prepare an "index" */
			tmp_val[0] = I2A(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		prtf(0, i, tmp_val);

		/* Get the color */
		attr = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) attr = CLR_L_DARK;

		/* Display the entry itself */
		put_fstr(3, i, "%s" CLR_SET_DEFAULT "%v", attr, OBJECT_FMT(o_ptr, TRUE, 3));

		/* Display the slot description (if needed) */
		if (show_labels)
		{
			prtf(wid - 19, i, "<--");
			put_fstr(wid - 15, i, mention_use(i));
		}

		/* Display the weight (if needed) */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = (show_labels ? wid - 28 : wid - 9);
			put_fstr(col, i, "%3d.%1d lb", wgt / 10, wgt % 10);
		}
	}

	/* Erase the rest of the window */
    clear_from(EQUIP_MAX);
}


/*
 * Display a list of objects.
 * This can be used to show the player's inventory,
 * or to show a list of floor items.
 *
 * Hack -- do not display "trailing" empty slots
 */
void show_list(s16b o_list_ptr, bool store)
{
	int i, j;
	int k, l;

	int col, len, lim;
	object_type *o_ptr;

	object_type *out_object[23];
	int out_index[23];
	cptr out_color[23];
	char out_desc[23][256];

	byte a;
	char c;

	int wid, hgt;
	
	int extra = 0;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Default "max-length" */
	len = wid - 51;

	/* Maximum space allowed for descriptions */
	lim = wid - 6;

	/* Initialise counters */
	i = -1;
	j = 0;
	k = -1;
	
	/* How much extra room do we need? */
	if (show_weights) extra += 9;
	if (store) extra += 11;
	
	/* Notice the extra space required */
	lim -= extra;

	/* Display the inventory */
	OBJ_ITT_START (o_list_ptr, o_ptr)
	{
		/* Paranoia - don't display too many items */
		if (k >= INVEN_PACK - 1) break;
	
		i++;

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Advance to next "line" */
		k++;

		/* Save the object index, color, and description */
		out_index[k] = i;
		out_object[k] = o_ptr;
		out_color[k] = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) out_color[k] = CLR_L_DARK;

		/* Save the name for later */
		l = strnfmt(out_desc[k], lim, "%v", OBJECT_FMT(o_ptr, TRUE, 3));

		/* Find the predicted "line length" */
		l += 5;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;
		
		/* Account for the price */
		if (store) l += 10;

		/* Account for icon if displayed */
		l += 2;

		/* Maintain the maximum length */
		if (l > len) len = l;
	}
	OBJ_ITT_END;

	/* Hack -- Find a column to start in and to put weights */
	if (len > wid - 4)
	{
		col = 0;
		lim = wid - extra;
	}
	else
	{
		col = (wid - len - 1) / 2;
		lim = col + len - extra;
	}

	/* Output each entry */
	for (j = 0; j <= k; j++)
	{
		/* Get the item */
		o_ptr = out_object[j];

		/* Clear the line */
		prtf(col ? col - 2 : col, j + 1, "");

		/* Clear the line with the (possibly indented) index */
		put_fstr(col, j + 1, "%c)", I2A(out_index[j]));

		/* Display graphics for object, if desired */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Fake monochrome */
		if (!use_color)
		{
			/* Hack - no equippy char */
			a = TERM_WHITE;
			c = ' ';
		}

		Term_draw(col + 3, j + 1, a, c);

		/* Display the entry itself */
		put_fstr(col + 5, j + 1, "%s" CLR_SET_DEFAULT "%s",
				 out_color[j], out_desc[j]);

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			put_fstr(lim, j + 1, "%3d.%1d lb", wgt / 10, wgt % 10);
			
			if (store)
			{
				/* Extract the price */
				u32b price = price_item(o_ptr, TRUE);

				/* Actually draw the price */
				put_fstr(lim + 9, j + 1, "%9ld  ", (long)price);
			}
		}
		else if (store)
		{
			/* Extract the price */
			u32b price = price_item(o_ptr, TRUE);

			/* Actually draw the price */
			put_fstr(lim, j + 1, "%9ld  ", (long)price);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < hgt - 2)) prtf(col ? col - 2 : col, j + 1, "");
}


/*
 * Display the equipment.
 */
void show_equip(bool store)
{
	int i, j, k, l;
	int col, len, lim;

	object_type *o_ptr;

	char o_name[256];
	int out_index[EQUIP_MAX];
	cptr out_color[EQUIP_MAX];
	char out_desc[EQUIP_MAX][256];

	byte a;
	char c;

	int wid, hgt;
	
	int extra = 0;
	
	/* Get size */
	Term_get_size(&wid, &hgt);
	
	/* Maximal length */
	len = wid - 51;

	/* Maximum space allowed for descriptions */
	lim = wid - 6;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= (14 + 2);
	
	/* How much extra room do we need? */
	if (show_weights) extra += 9;
	if (store) extra += 11;
	
	/* Notice the extra space required */
	lim -= extra;

	/* Scan the equipment list */
	for (k = 0, i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3, 256);

		/* Truncate the description */
		o_name[lim] = '\0';

		/* Save the color */
		out_index[k] = i;
		out_color[k] = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) out_color[k] = CLR_L_DARK;

		(void)strcpy(out_desc[k], o_name);

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;
		
		/* Increase length for price (if needed) */
		if (store) l += 11;

		/* old show_equip_graph option perm. on. */
		l += 2;

		/* Maintain the max-length */
		if (l > len) len = l;

		/* Advance the entry */
		k++;
	}

	/* Hack -- Find a column to start in and to put weights */
	if (len > wid - 4)
	{
		col = 0;
		lim = wid - extra;
	}
	else
	{
		col = (wid - len - 1) / 2;
		lim = col + len - extra;
	}

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &p_ptr->equipment[i];

		/* Clear the line */
		prtf(col ? col - 2 : col, j + 1, "");

		/* Clear the line with the (possibly indented) index */
		put_fstr(col, j + 1,"%c)", I2A(i));

		/* Show_equip_graph perm. on. */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Hack - if no object, don't display the 'nothing' symbol */
		if (!o_ptr->number) c = ' ';

		/* Fake monochrome */
		if (!use_color)
		{
			/* Hack - no equippy char */
			a = TERM_WHITE;
			c = ' ';
		}

		Term_draw(col + 3, j + 1, a, c);

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			put_fstr(col + 5, j + 1, "%-14s: ", mention_use(i));

			/* Display the entry itself */
			put_fstr(col + 21, j + 1, "%s" CLR_SET_DEFAULT "%s",
					 out_color[j], out_desc[j]);
		}

		/* No labels */
		else
		{
			/* Display the entry itself */
			put_fstr(col + 5, j + 1, "%s" CLR_SET_DEFAULT "%s",
					 out_color[j], out_desc[j]);
		}

		/* Item here? */
		if (o_ptr->number)
		{
			/* Display the weight if needed */
			if (show_weights)
			{
				int wgt = o_ptr->weight * o_ptr->number;
				put_fstr(lim, j + 1, "%3d.%1d lb", wgt / 10, wgt % 10);
			
				if (store)
				{
					/* Extract the price */
					u32b price = price_item(o_ptr, TRUE);

					/* Actually draw the price */
					put_fstr(lim + 9, j + 1, "%9ld  ", (long)price);
				}
			}
			else if (store)
			{
				/* Extract the price */
				u32b price = price_item(o_ptr, TRUE);

				/* Actually draw the price */
				put_fstr(lim, j + 1, "%9ld  ", (long)price);
			}
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < hgt - 2)) prtf(col ? col - 2 : col, j + 1, "");
}


/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Flip inven to equip */
		if (window_flag[j] & (PW_INVEN))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_INVEN);
			window_flag[j] |= (PW_EQUIP);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}

		/* Flip inven to equip */
		else if (window_flag[j] & (PW_EQUIP))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_EQUIP);
			window_flag[j] |= (PW_INVEN);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
		}
	}
}



/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, object_type *o_ptr)
{
	/* Query */
	return (get_check("%s %v? ", prompt, OBJECT_FMT(o_ptr, TRUE, 3)));
}


/*
 * Hack -- allow user to "prevent" certain choices
 */
static bool get_item_allow(object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->inscription) return (TRUE);

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->inscription), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == p_ptr->cmd.cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify("Really try", o_ptr)) return (FALSE);
		}

		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
}


/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" cmd.cmd code.
 */
static object_type *get_tag(bool *inven, char tag)
{
	int i;

	cptr s;

	object_type *o_ptr;


	/* Check inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Skip empty inscriptions */
		if (!o_ptr->inscription) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->inscription), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*inven = TRUE;

				/* Success */
				return (o_ptr);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->cmd.cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*inven = TRUE;

				/* Success */
				return (o_ptr);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}
	OBJ_ITT_END;

	/* Check equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		object_type *o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->inscription) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->inscription), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*inven = FALSE;

				/* Success */
				return (o_ptr);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->cmd.cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*inven = FALSE;

				/* Success */
				return (o_ptr);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}

	/* No such tag */
	return (NULL);
}

/*
 * test_floor --
 *
 * Return the first valid item at a location +
 * Return the number of valid items at the location.
 *
 * Valid flags are:
 *
 *		mode & 0x01 -- Item tester
 *		mode & 0x02 -- Marked items only
 */
object_type *test_floor(int *num, cave_type *c_ptr, int mode)
{
	object_type *q_ptr = NULL;
	object_type *o_ptr;

	/* No items yet */
	*num = 0;

	/* Scan all objects in the grid */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Item tester */
		if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

		/* Marked */
		if ((mode & 0x02) && !(o_ptr->info & OB_SEEN)) continue;

		/* Save first item */
		if (!q_ptr) q_ptr = o_ptr;

		/* Count objects */
		(*num)++;
	}
	OBJ_ITT_END;

	/* First item in list, if it exists */
	return (q_ptr);
}

/*
 * Toggle the inventory and equipment terms when needed
 */
static bool toggle_windows(bool toggle, int command_wrk)
{
	bool ni = FALSE;
	bool ne = FALSE;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Count windows displaying inven */
		if (window_flag[j] & (PW_INVEN)) ni = TRUE;

		/* Count windows displaying equip */
		if (window_flag[j] & (PW_EQUIP)) ne = TRUE;
	}

	/* Toggle if needed */
	if ((command_wrk == (USE_EQUIP) && ni && !ne) ||
		(command_wrk == (USE_INVEN) && !ni && ne))
	{
		/* Toggle */
		toggle_inven_equip();

		/* Track toggles */
		toggle = !toggle;
	}

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Redraw windows */
	window_stuff();

	return (toggle);
}


/*
 * Show the prompt for items
 */
static void show_item_prompt(bool inven, bool equip, bool floor, bool store,
							cptr pmt, int command_wrk)
{
	int i;

	int n1, n2;

	char out_val[160];

	object_type *eo_ptr;
	
	int len = 0;

	switch (command_wrk)
	{
		case USE_INVEN:
		{
			/* Extract the legal requests */
			get_label_bounds(p_ptr->inventory, &n1, &n2);

			/* Redraw */
			show_list(p_ptr->inventory, store);

			/* Begin the prompt */
			len = strnfmt(out_val, 160, "Inven:");

			/* Append */
			if (equip) strnfcat(out_val, 160, &len, " / for Equip,");

			/* Append */
			if (floor) strnfcat(out_val, 160, &len, " - for floor,");

			break;
		}

		case USE_EQUIP:
		{
			/* Nothing yet */
			n1 = -1;
			n2 = -1;

			/* Test for usable equipment */
			for (i = 0; i < EQUIP_MAX; i++)
			{
				eo_ptr = &p_ptr->equipment[i];

				if (item_tester_okay(eo_ptr))
				{
					/* Get lower bound */
					if (n1 == -1) n1 = i;

					/* Get higher bound */
					n2 = i;
				}
			}

			/* Redraw */
			show_equip(store);

			/* Begin the prompt */
			len = strnfmt(out_val, 160, "Equip:");

			/* Append */
			if (inven) strnfcat(out_val, 160, &len, " / for Inven,");

			/* Append */
			if (floor) strnfcat(out_val, 160, &len, " - for floor,");

			break;
		}

		case USE_FLOOR:
		{
			cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

			/* Extract the legal requests */
			get_label_bounds(p_ptr->inventory, &n1, &n2);

			if (easy_floor)
			{
				/* Redraw */
				show_list(c_ptr->o_idx, store);

				/* Begin the prompt */
				len = strnfmt(out_val, 160, "Floor:");

				/* Append */
				if (inven)
				{
					strnfcat(out_val, 160, &len, " / for Inven,");
				}
				else if (equip)
				{
					strnfcat(out_val, 160, &len, " / for Equip,");
				}
			}
			else
			{
				/* Begin the prompt */
				len = strnfmt(out_val, 160, "Top item on floor: '-'");
			}

			break;
		}
	}

	/* Show the prompt */
	prtf(0, 0, "(%s ESC) %s", out_val, pmt);
}

/*
 * Remember the object we selected so that
 * we can repeat the action if required.
 */
static void save_object_choice(object_type *o_ptr, int command_wrk)
{
	int index = -1;

	cave_type *c_ptr;

	/* Paranoia */
	if (!o_ptr) return;

	/* Save type of prompt */
	repeat_push(command_wrk);

	switch (command_wrk)
	{
		case USE_INVEN:
		{
			index = get_item_position(p_ptr->inventory, o_ptr);
			break;
		}

		case USE_EQUIP:
		{
			index = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);
			break;
		}

		case USE_FLOOR:
		{
			c_ptr = area(p_ptr->px, p_ptr->py);

			index = get_item_position(c_ptr->o_idx, o_ptr);
			break;
		}
	}

	/* Save the index */
	repeat_push(index);
}

/*
 * Recall which object we have previously used.
 */
static object_type *recall_object_choice(int *command_wrk)
{
	int type;
	int index;

	cave_type *c_ptr;
	object_type *o_ptr = NULL;

	/* Get type of prompt */
	if (!repeat_pull(&type))
	{
		/* Not a valid repeat - return invalid object */
		return (NULL);
	}

	/* Paranoia */
	if (type == -1)
	{
		/* Invalid repeat - reset it */
		repeat_clear();
		return (NULL);
	}

	/* Set type of prompt */
	*command_wrk = type;

	/* Get index */
	if (!repeat_pull(&index))
	{
		/* Not a valid repeat - return invalid object */
		return (NULL);
	}

	/* Paranoia */
	if (index == -1)
	{
		/* Invalid repeat - reset it */
		repeat_clear();
		return (NULL);
	}

	/* Get item */
	switch (type)
	{
		case USE_INVEN:
		{
			o_ptr = get_list_item(p_ptr->inventory, index);
			break;
		}

		case USE_EQUIP:
		{
			if ((index < 0) || (index >= EQUIP_MAX))
			{
				o_ptr = NULL;
			}
			else
			{
				o_ptr = &p_ptr->equipment[index];
			}

			break;
		}

		case USE_FLOOR:
		{
			c_ptr = area(p_ptr->px, p_ptr->py);

			o_ptr = get_list_item(c_ptr->o_idx, index);
			break;
		}

		default:
		{
			/* Invalid repeat - reset it */
			repeat_clear();
			return (NULL);
		}
	}


	/* Validate the item */
	if (item_tester_okay(o_ptr))
	{
		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Success */
		return (o_ptr);
	}

	/* Invalid repeat - reset it */
	repeat_clear();
	return (NULL);
}


/*
 * Let the user select an item and return a pointer to it.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * This version of get_item() includes the modifications due to the
 * easy_floor flag.  This flag changes how items on the floor are treated.
 */
object_type *get_item(cptr pmt, cptr str, int mode)
{
	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

	char which;

	int i;

	bool done = FALSE;

	bool equip = FALSE;
	bool inven = FALSE;
	bool floor = FALSE;
	bool store = FALSE;

	bool allow_equip = FALSE;
	bool allow_inven = FALSE;
	bool allow_floor = FALSE;

	int command_wrk;

	bool toggle = FALSE;

	int floor_num;

	/* First Floor item */
	object_type *fo_ptr;

	/* Temp item */
	object_type *q_ptr;

	object_type *o_ptr;

	/* Extract args */
	if (mode & (USE_EQUIP)) equip = TRUE;
	if (mode & (USE_INVEN)) inven = TRUE;
	if (mode & (USE_FLOOR)) floor = TRUE;
	if (mode & (USE_STORE)) store = TRUE;

	/* Paranoia XXX XXX XXX */
	message_flush();

	/* Not done */
	done = FALSE;

	/* Test for equipment */
	if (equip)
	{
		for (i = 0; i < EQUIP_MAX; i++)
		{
			q_ptr = &p_ptr->equipment[i];

			/* Only want valid items */
			if (q_ptr->k_idx && item_tester_okay(q_ptr))
			{
				allow_equip = TRUE;

				break;
			}
		}
	}

	/* Scan all objects in the grid */
	fo_ptr = test_floor(&floor_num, c_ptr, 0x01);

	/* Accept floor */
	if (floor_num && floor) allow_floor = TRUE;

	/* Scan inventory */
	if (inven)
	{
		OBJ_ITT_START (p_ptr->inventory, q_ptr)
		{
			/* Only want valid items */
			if (item_tester_okay(q_ptr))
			{
				allow_inven = TRUE;

				break;
			}
		}
		OBJ_ITT_END;
	}

	/* Require at least one legal choice */
	if (!allow_inven && !allow_equip && !allow_floor)
	{
		/* Warning if needed */
		if (str) msgf(str);

		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Done */
		return (FALSE);
	}

	/* Analyze choices */

	/* Use inventory if allowed */
	if (allow_inven)
	{
		command_wrk = (USE_INVEN);
	}

	/* Use equipment if allowed */
	else if (allow_equip)
	{
		command_wrk = (USE_EQUIP);
	}

	/* Use floor if allowed */
	else if (allow_floor)
	{
		command_wrk = (USE_FLOOR);
	}

	/* Get the saved item index */
	o_ptr = recall_object_choice(&command_wrk);

	if (o_ptr)
	{
		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Done */
		return (o_ptr);
	}

	/* Hack -- start out in "display" mode */

	/* Save screen */
	screen_save();

	/* Repeat until done */
	while (!done)
	{
		/* Make sure object is in the unselected state */
		o_ptr = NULL;
		
		/* Activate the correct term info */
		toggle = toggle_windows(toggle, command_wrk);

		/* Display the prompt */
		show_item_prompt(allow_inven, allow_equip, allow_floor, store, pmt,
						 command_wrk);

		/* Get a key */
		which = inkey();

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				done = TRUE;
				break;
			}

			case '/':
			{
				if (command_wrk == (USE_INVEN))
				{
					if (!allow_equip)
					{
						bell("Cannot switch item selector!");
						break;
					}
					command_wrk = (USE_EQUIP);
				}
				else if (command_wrk == (USE_EQUIP))
				{
					if (!allow_inven)
					{
						bell("Cannot switch item selector!");
						break;
					}
					command_wrk = (USE_INVEN);
				}
				else if (command_wrk == (USE_FLOOR))
				{
					if (allow_inven)
					{
						command_wrk = (USE_INVEN);
					}
					else if (allow_equip)
					{
						command_wrk = (USE_EQUIP);
					}
					else
					{
						bell("Cannot switch item selector!");
						break;
					}
				}

				/* Hack -- Fix screen */

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				/* Need to redraw */
				break;
			}

			case '-':
			{
				if (!allow_floor)
				{
					bell("Cannot select floor!");
					break;
				}

				if (!easy_floor)
				{
					/* Scan all objects in the grid */
					OBJ_ITT_START (c_ptr->o_idx, o_ptr)
					{
						/* Valid items only */
						if (!item_tester_okay(o_ptr)) continue;
					
						/* Allow player to "refuse" certain actions */
						if (!get_item_allow(o_ptr)) continue;

						/* Accept that choice */
						done = TRUE;
						break;
					}
					OBJ_ITT_END;
				}

				/*
				 * If we are already examining the floor, and there
				 * is only one item, we will always select it.
				 * If we aren't examining the floor and there is only
				 * one item, we will select it if floor_query_flag
				 * is FALSE.
				 */
				else if (floor_num == 1)
				{
					if ((command_wrk == (USE_FLOOR)) || (!carry_query_flag))
					{
						/* Allow player to "refuse" certain actions */
						if (!get_item_allow(fo_ptr)) continue;

						/* We use the first floor item */
						o_ptr = fo_ptr;

						/* Accept that choice */
						done = TRUE;

						break;
					}
				}

				/* Hack -- Fix screen */

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				command_wrk = (USE_FLOOR);

				break;
			}

			case '0':
			case '1':  case '2':  case '3':
			case '4':  case '5':  case '6':
			case '7':  case '8':  case '9':
			{
				bool was_inven;

				/* Look up the tag */
				o_ptr = get_tag(&was_inven, which);

				if (!o_ptr)
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Hack -- Validate the item */
				if (was_inven ? !inven : !equip)
				{
					bell("Illegal object choice (tag)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Validate the item */
				if (!item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (tag)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;

				/* Accept that choice */
				done = TRUE;
				break;
			}

			case '\n':
			case '\r':
			{
				/* No object selected yet */
				o_ptr = NULL;

				/* Choose "default" floor item */
				if (command_wrk == (USE_FLOOR))
				{
					if (floor_num == 1)
					{
						o_ptr = fo_ptr;
					}
				}

				/* Validate the item */
				if (!o_ptr || !item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (default)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;

				/* Accept that choice */
				done = TRUE;
				break;
			}

			default:
			{
				int ver;

				/* Extract "query" setting */
				ver = isupper(which);
				which = tolower(which);

				/* Convert letter to inventory index */
				if (command_wrk == (USE_INVEN))
				{
					o_ptr = label_to_list(which, p_ptr->inventory);
				}

				/* Convert letter to equipment index */
				else if (command_wrk == (USE_EQUIP))
				{
					o_ptr = label_to_equip(which);
				}

				/* Convert letter to floor index */
				else if (command_wrk == USE_FLOOR)
				{
					o_ptr = label_to_list(which, c_ptr->o_idx);
				}

				/* Make sure selection is in bounds */
				if (!o_ptr)
				{
					bell("Illegal object choice (bounds)!");
					break;
				}

				/* Validate the item */
				if (!item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (normal)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Verify the item */
				if (ver && !verify("Try", o_ptr))
				{
					done = TRUE;
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;
					

				/* Accept that choice */
				done = TRUE;
				break;
			}
		}
	}

	/* Fix the screen */

	/* Load screen */
	screen_load();

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;


	/* Clean up */

	/* Toggle again if needed */
	if (toggle) toggle_inven_equip();

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Window stuff */
	window_stuff();

	/* Clear the prompt line */
	clear_msg();

	/* Save this object */
	save_object_choice(o_ptr, command_wrk);

	/* Done */
	return (o_ptr);
}
