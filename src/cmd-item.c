/* File: cmd-item.c */
/* Purpose: Item code */


/*
 * This fantabular file includes code for eating food, drinking tonics,
 * rigging a mechanism, aiming ray guns, using tools, zapping apparatuses,
 * and activating artifacts.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of ray guns, tools,
 * and apparatuses.  Note the overly paranoid warning about potential pack
 * overflow, which allows the player to use and drop a stacked item.
 *
 * In all "unstacking" scenarios, the "used" object is "carried" as if
 * the player had just picked it up.  In particular, this means that if
 * the use of an item induces pack overflow, that item will be dropped.
 *
 * For simplicity, these routines induce a full "pack reorganization"
 * which not only combines similar items, but also reorganizes various
 * items to obey the current "sorting" method.  This may require about
 * 400 item comparisons, but only occasionally.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a ray gun/tool to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the tools/ray guns, so this is not a problem.
 * But, for example, a "tool of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a tool "negative" charges, or "turning a tool into a stick".
 * It seems as though a "apparatus of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/Tonics/mechanisms no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */

/*
 * Remember if object will be identified and/or used up.
 */
static int obj_ident;
static int obj_used_up;


/*
 * Hook to specify non-empty oil containers (flasks/lanterns)
 */
static bool item_tester_hook_oils(const object_type *o_ptr)
{
	if (((o_ptr->tval == TV_FLASK) ||
	     ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))) &&
		o_ptr->pval) return (TRUE);
	return (FALSE);
}

/*
 * Curse the players armor
 */
static bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) || (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = 0; /* EGO_BLASTED; <- There isn't a cursed armor type now */
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	return (TRUE);
}


/*
 * Curse the players weapon
 */
static bool curse_weapon(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);


	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw */
	if (artifact_p(o_ptr) || (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = 0; /* EGO_SHATTERED;*/
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Notice */
	return (TRUE);
}

/* Read from a parchment or text */

void show_book_number(int num)
{
	char buf[1024];

	path_build(buf, 1024, ANGBAND_DIR_FILE, format("book-%d.txt", num));

	screen_save();

	show_file(buf, "Arcane Knowledge", 0, 0);

	screen_load();
}
	
/*
 * An "item_tester_hook" for refilling torches ????
 */
static bool item_tester_readable(const object_type *o_ptr)
{
	/* Scrolls and Parchments are okay */
	if ((o_ptr->tval == TV_MECHANISM) || (o_ptr->tval == TV_TEXT)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}

/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Hook to determine if an object is a magical device.
 */
static bool item_tester_hook_device(const object_type *o_ptr)
{
	/* Must be a staff, wand, or rod */
	if ((o_ptr->tval == TV_TOOL) ||
	    (o_ptr->tval == TV_RAY) ||
	    (o_ptr->tval == TV_APPARATUS))
	{
		return (TRUE);
	}

	/* Disallow anything else */
	return (FALSE);
}


/*
 * Can we read scrolls?
 */
static bool can_read_scroll(void)
{
	/* Check some conditions */
	if (p_ptr->blind)
	{
		msg_print("You can't see anything.");
		return (FALSE);
	}
	if (no_lite())
	{
		msg_print("You have no light to operate the mechanism.");
		return (FALSE);
	}
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return (FALSE);
	}
	return (TRUE);
}


/*
 * Eat some food, quaff a potion, or read a scroll
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  Identifying them still takes a turn.
 *
 * See the functions "potion_smash_effect()" and "scroll_read_effect()"
 * for what happens when a potions and scrolls are thrown or activated.
 */
void use_object(int tval)
{
	int item;

	object_type *o_ptr;
	object_kind *k_ptr;

	cptr q, s;
	u32b f1, f2, f3;
	player_flags(&f1, &f2, &f3);


	/* Output text based on the object kind */
	if (tval == TV_FOOD)
	{
		/* Restrict choices to food */
		if (f3 & (TR3_AUTOMATA))
		{
			item_tester_hook = item_tester_hook_oils;
		}
		else 
		{
			item_tester_tval = TV_FOOD;
		}

		/* Get an item */
		if (f3 & (TR3_AUTOMATA))
		{
			s = "You have no oil!";
			q = "Drink from which item? ";
		}
		else 
		{
			s = "You have nothing to eat.";
			q = "Eat which item? ";
		}
	}
	else if (tval == TV_MECHANISM)
	{
		/* Check some conditions */
		if (!can_read_scroll()) return;

		/* Restrict choices to scrolls */
		item_tester_hook = item_tester_readable;

		/* Get an item */
		q = "Rig which mechanism?";
		s = "You have no mechanism's to rig.";
	}
	else if (tval == TV_TONIC)
	{
		/* Check to insure that Tonics can be quaffed! */
		if (f3 & (TR3_AUTOMATA))
		{
			/* msg_print("You cannot drink tonics!"); */
			use_object(TV_FOOD);
			return;
		}
	
		/* Behavior if you can drink tonics */
		else
		{
			/* Restrict choices to Tonics */
			item_tester_tval = TV_TONIC;
		}

		/* Get an item */
		q = "Quaff which tonic?";
		s = "You have no tonics to quaff.";
	}
	else
	{
		msg_print("Unknown object type in \"use_object()\".");
		return;
	}


	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];


	/* Handle some things */
	if (o_ptr->tval == TV_FOOD)
	{
		/* Sound */
		sound(MSG_EAT);
	}
	else if (o_ptr->tval == TV_TONIC)
	{
		/* Sound */
		sound(MSG_QUAFF);
	}
	else if (o_ptr->tval == TV_MECHANISM)
	{
		/* No sound effect */
	}

	/* Assume the scroll will get used up */
	if (o_ptr->tval == TV_TEXT)
	{
		obj_used_up = FALSE;
	}
	else
	{
		obj_used_up = TRUE;
	}

	/* Assume no identification */
	obj_ident = FALSE;


	/* Eat the food, quaff the potion, read the scroll, or use the object */
	(void)do_object(OBJECT_USE, o_ptr);

	/* We have tried it */
	/* RMG: currently there are only "normal" food items with no side effects so no "trying" them */
	if (o_ptr->tval != TV_FOOD)
	{
		object_tried(o_ptr);
	}

	/* Always have some chance to learn about the object */
	if (!obj_ident)
	{
		int chance;

		if (o_ptr->tval == TV_FOOD)
		{
			/* Chance decreases with level */
			chance = 25 - (k_ptr->level / 2);
		}
		else
		{
			chance = 10;
		}

		/* Attempt to learn something */
		if (rand_int(100) < chance) obj_ident = TRUE;
	}

	/* An identification was made */
	if (obj_ident)
	{
		/* Newly learnt object */
		if (!object_aware_p(o_ptr))
		{
			/* Become aware of the object kind */
			object_aware(o_ptr); 

			/* Gain a significant amount of exp */
			gain_exp(3 * (k_ptr->level * k_ptr->level) + 1);

			/* Learning something always takes time */
			p_ptr->energy_use = 100;

			/* Chance to learn more about (known) items of this type */
			if (obj_used_up)
			{
				object_known(o_ptr);
			}
		}
	}

	/* Hack -- allow certain items to be "preserved" */
	if (!obj_used_up) return;

	/* Special message only prints once */
	/* k_info[o_ptr->k_idx].special |= (SPECIAL_MESSAGE); */

	/* Take a turn (unless object was neither used nor identified) */
	p_ptr->energy_use = 100;

	/* Food can feed the player */
	if (f3 & (TR3_AUTOMATA))
	{
		(void)set_food(p_ptr->food + (o_ptr->pval / 2));
	}
	else if (o_ptr->tval == TV_FOOD)
	{
		(void)set_food(p_ptr->food + o_ptr->pval);
	}

	/* Potions can feed the player */
	else if (o_ptr->tval == TV_TONIC)
	{
		(void)set_food(p_ptr->food + o_ptr->pval);
	}


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Empty lanterns rather than destroy them */
	if (o_ptr->tval == TV_LITE && o_ptr->sval == SV_LITE_LANTERN)
	{
		o_ptr->pval = 0;
	}
	else
	{
		/* Reduce food in the pack */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Reduce food on the floor */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}

	/* We quaffed a potion */
	if (tval == TV_TONIC)
	{
		/* Nothing */
	}

	/* We read a scroll */
	else if (tval == TV_MECHANISM)
	{
		/* Nothing */
	}
}

/*
 * Use a staff, wand, or rod.
 */
void use_device(int tval)
{
	int devicegood, devicenorm, devicepoor, advdevice;

	/* Assume no identification/usage */
	bool ident = FALSE;
	bool used  = FALSE;

	int item;
	int skill, lev, chance;

	object_type *o_ptr;
	object_kind *k_ptr;

	cptr q, s, p;

	/* Get the device factor */
	devicegood = p_ptr->skills[SK_DEVICE_GOOD].skill_rank;
	devicenorm = p_ptr->skills[SK_DEVICE_NORM].skill_rank;
	devicepoor = p_ptr->skills[SK_DEVICE_POOR].skill_rank;
	
	if (p_ptr->skills[SK_ADV_DEVICE].skill_max > 0)	advdevice = p_ptr->skills[SK_ADV_DEVICE].skill_rank;
	else advdevice = 0;
	
	/* Base chance of success */
	/* This needs to be displayed on the confirmation of use item */
	
	/* paranoia */
	skill = 0;
	
	/* Gives a max skill result (unmodified by items) of 100 / 2 + 20*/
	/* or 70, 20 points higher than a level 50 device - giving a */
	/* 100% success rate for the deepest device. */
	if (devicegood > 0) skill = ((5 * devicegood) / 2) + advdevice;

	/* Unmodified 60 / 2 + 20 == 50, giving a 100% success rate on */
	/* items level 30 and below -- and eaisly modified up to perfect */
	if (devicenorm > 0) skill = ((3 * devicenorm) / 2) + advdevice;

	/* You suck at devices 20+10 = 30 */
	if (devicepoor > 0) skill = devicepoor + (advdevice/2);

	/* Restrict choices (unless tval is zero) */
	item_tester_tval = tval;

	/* Prepare the hook (must be a magical device) */
	item_tester_hook = item_tester_hook_device;

	/* Output text based on the object kind */
	if (tval == 0)
	{
		q = "Use which magical device?";
		s = "You have no magical device to use.";
	}
	else if (tval == TV_TOOL)
	{
		q = "Use which tool?";
		s = "You have no tool to use.";
	}
	else if (tval == TV_RAY)
	{
		q = "Aim which ray gun?";
		s = "You have no ray gun to aim.";
	}
	else if (tval == TV_APPARATUS)
	{
		q = "Zap which apparatus?";
		s = "You have no apparatus to zap.";
	}
	else
	{
		msg_print("Unknown object type in \"use_device()\".");
		return;
	}

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Get name */
	if      (o_ptr->tval == TV_TOOL) 		p = "tool";
	else if (o_ptr->tval == TV_RAY) 		p = "ray gun";
	else if (o_ptr->tval == TV_APPARATUS)   p = "apparatus";
	else                             		p = "device";


	/* Notice that staffs or wands are empty - require knowledge */
	if ((o_ptr->tval == TV_TOOL) || (o_ptr->tval == TV_RAY))
	{
		if (o_ptr->pval <= 0)
		{
			if (flush_failure) flush();
			msg_format("Your %s is empty.", p);
			o_ptr->ident |= (IDENT_EMPTY);
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
			p_ptr->window |= (PW_INVEN);
			return;
		}
	}

	/* Notice that rods are empty - currently requires no knowledge  -LM- */
	else if (o_ptr->tval == TV_APPARATUS)
	{
		/* A single rod is still charging */
		if (o_ptr->pval)
		{
			if (flush_failure) flush();
			msg_print("The apparatus is still charging.");
			return;
		}

#if 0
		/* A stack of rods lacks enough energy */
		else if ((o_ptr->number > 1) && (k_ptr->pval * o_ptr->number - o_ptr->pval > k_ptr->pval) 
			(o_ptr->timeout > o_ptr->pval - k_ptr->pval))
		{
			if (flush_failure) flush();
			msg_print("The rods are all still charging.");
			return;
		}
#endif
	}

	/* Take a turn */
	if (p_ptr->skills[SK_SPEED_DEVICE].skill_max > 0)
	{
		p_ptr->energy_use = (100 - (p_ptr->skills[SK_SPEED_DEVICE].skill_rank * 4));
	}
	else
	{
		p_ptr->energy_use = 100;
	}

	/* Confusion and hallucination make things harder */
	if ((p_ptr->confused) || (p_ptr->image)) skill /= 2;

	/* Stunning makes things a little harder */
	if (p_ptr->stun) skill -= p_ptr->stun / 2;

	/* Blindness makes things a little harder */
	if (p_ptr->blind) skill -= p_ptr->blind / 3;

	/* Get the object level (no maximum) */
	lev = k_info[o_ptr->k_idx].level;

	/*
	 * Determine percentage chance of success.
	 * Is zero if skill is 10 below level, is perfect if it is 20 above.
	 * ok, strange.
	 */
	if (skill < lev) chance = 10 + skill - lev;
	else             chance = 10 + rsqrt((skill - lev) * 405);

	/* Roll for failure */
	if (chance <= rand_int(100))
	{
		if (flush_failure) flush();
		msg_format("You failed to use the %s properly.", p);
		return;
	}


	/* Notice empty staffs or wands */
	if ((o_ptr->tval == TV_TOOL) || (o_ptr->tval == TV_RAY))
	{
		if (o_ptr->pval <= 0)
		{
			if (flush_failure) flush();
			msg_format("The %s has no charges left.", p);
			o_ptr->ident |= (IDENT_EMPTY);
			return;
		}
		else
		{
			/* Player remembers that device is not empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Use device */
	(void)do_device(OBJECT_USE, o_ptr, &ident, &used, FALSE);

	/* Note cancel */
	if (!used)
	{
		/* Do not take a turn  XXX XXX */
		p_ptr->energy_use = 0;
		return;
	}


	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident)
	{
		/* Newly learnt object */
		if (!object_aware_p(o_ptr))
		{
			object_aware(o_ptr);
			gain_exp(3 * (lev * lev) + 1);
		}

	}

	/* Special message only prints once */
	/* k_info[o_ptr->k_idx].special |= (SPECIAL_MESSAGE); */


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Use a staff or wand */
	if ((o_ptr->tval == TV_TOOL) || (o_ptr->tval == TV_RAY))
	{
		/* Use charge */
		o_ptr->pval--;

		/* Describe charges in the pack */
		if (item >= 0)
		{
			inven_item_charges(item);
		}
		/* Describe charges on the floor */
		else
		{
			floor_item_charges(0 - item);
		}
	}

	/* Use a rod - Recharging must take place in activation */
	else if (o_ptr->tval == TV_APPARATUS)
	{
		/* Empty lanterns rather than destroy them */
		if ((o_ptr->sval == SV_APPARATUS_PISTOL_AMMO) ||
			(o_ptr->sval == SV_APPARATUS_RIFLE_AMMO) ||
			(o_ptr->sval == SV_APPARATUS_SHOTGUN_AMMO)) 
		{
			if ((one_in_(8)))
			{
				msg_format("The %s falls apart!", p);				
				/* Reduce food in the pack */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}
	
				/* Reduce food on the floor */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
		}
	}
}

/*
 * Activate a wielded object.
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 *
 * We do not allow items to be activated from inventory, even though
 * that would be more convenient, because it just feels weird to be
 * able to zap DSM like rods.
 */
void do_cmd_activate(void)
{
	int item;
	int skill, lev, chance;
	int devicegood, devicenorm, devicepoor, advdevice;

	object_type *o_ptr;


	cptr q, s;

	/* Paranoia */
	lev = 40;
	
	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get the device factor */
	devicegood = p_ptr->skills[SK_DEVICE_GOOD].skill_rank;
	devicenorm = p_ptr->skills[SK_DEVICE_NORM].skill_rank;
	devicepoor = p_ptr->skills[SK_DEVICE_POOR].skill_rank;
	
	if (p_ptr->skills[SK_ADV_DEVICE].skill_max > 0)	advdevice = p_ptr->skills[SK_ADV_DEVICE].skill_rank;
	else advdevice = 0;

	/* Base chance of success */
	/* This needs to be displayed on the confirmation of use item */

	/* Gives a max skill result (unmodified by items) of 140 / 2 */
	/* or 70, 20 points higher than a level 50 device - giving a */
	/* 100% success rate for the deepest device. */
	if (devicegood > 0) skill = ((7 * devicegood) / 2) + advdevice;

	/* Unmodified 100 / 2 == 50, giving a 100% success rate on */
	/* items level 30 and below -- and eaisly modified up to perfect */
	if (devicenorm > 0) skill = ((5 * devicenorm) / 2) + advdevice;

	/* You suck at devices */
	if (devicepoor > 0) skill = devicepoor + advdevice;

	/* Get an item */
	q = "Activate which item?";
	s = "You have nothing to activate.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confusion and hallucination make things harder */
	if ((p_ptr->confused) || (p_ptr->image)) skill /= 2;

	/* Stunning makes things a little harder */
	if (p_ptr->stun) skill -= p_ptr->stun / 2;

	/* Blindness makes things a little harder */
	if (p_ptr->blind) skill -= skill / 3;

	/* Object is an artifact - use artifact level */
	if (artifact_p(o_ptr))
	{
		lev = a_info[o_ptr->name1].level;
		/* artifacts are eaiser to use */
		lev /= 3;
	}

	/* Never get too difficult */
	if (lev > 40) lev = 40;

	/*
	 * Determine percentage chance of success.
	 * Is zero if skill is 10 below level, is perfect if it is 15 above.
	 */
	if (skill < lev) chance = 10 + skill - lev;
	else             chance = 10 + rsqrt((skill - lev) * 540);

	/* Roll for failure */
	if (chance <= rand_int(100))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout > 0)
	{
		/* Other items just fail */
		msg_print("It whines, glows and fades...");
		return;
	}

	/* Wonder Twin Powers... Activate! */
	message(MSG_ZAP, 0, "You activate it...");

	/* Activate */
	(void)do_activation_aux(OBJECT_USE, o_ptr);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Done */
	return;
}


/*
 * Handle the effects of individual foods and mushrooms, potions, and
 * scrolls.
 */
cptr do_object(int mode, object_type *o_ptr)
{
	
	u32b f1, f2, f3;

	/* Get the object kind */
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Modes of operation */
	bool use  = (mode == OBJECT_USE);
	bool info = (mode == OBJECT_INFO);

	/* Output special message only once */
	/* u16b msg = !(k_ptr->special & (SPECIAL_MESSAGE)); */

	/* Note whether we are aware of the effects of this item */
	u16b aware = TRUE;
	int i, pow, pow1, dur1, dur2;
	int power1, power2, healfac;
	int py = p_ptr->py;
	int px = p_ptr->px;

	cptr extra = "";

	player_flags(&f1, &f2, &f3);
	power1 = p_ptr->skills[SK_POW_DEVICE].skill_rank;
	power2 = p_ptr->skills[SK_POW2_DEVICE].skill_rank;
	healfac = p_ptr->skills[SK_HEAL_DEVICE].skill_rank;

	/* Unused parameter */
	/* (void)use; */


	/* In order to get the unaware bonus, an object must be unsensed too */
	if ((!object_aware_p(o_ptr)) && (!(o_ptr->ident & (IDENT_SENSE))))
	{
		aware = FALSE;
	}


	/* Jump to the right category of item */
	if      (o_ptr->tval == TV_FOOD)   goto do_food;
	else if (o_ptr->tval == TV_TONIC) goto do_tonic;
	else if (o_ptr->tval == TV_TEXT) show_book_number(o_ptr->sval);
	else if (o_ptr->tval == TV_MECHANISM) goto do_mechanism;
	else return ("");


	/*** Handle Food and Mushrooms ***/
	do_food:

	/* Analyze the food */
	switch (o_ptr->sval)
	{
	}

	/* End of Food and Mushroom section */
	return ("");



	/*** Handle Potions ***/
	do_tonic:

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_TONIC_WATER:
		case SV_TONIC_APPLE_JUICE:
		case SV_TONIC_SLIME_MOLD:
		{
			if (info) return (format("(food value: %d)", o_ptr->pval));

			msg_print("You feel less thirsty.");
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_SLOWNESS:
		{
			if (info) return ("");

			if (set_slow(p_ptr->slow + rand_range(15, 40))) obj_ident = TRUE;

			break;
		}
		case SV_TONIC_SALT_WATER:
		{
			if (info) return (format("(but it does cure poison)"));

			msg_print("The tonic makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_POISON:
		{
			if (info) return ("");

			if (!(resist_effect(RS_PSN)))
			{
				if (!aware)
				{
						msg_print("This tonic must have been diluted!");
						set_poisoned(p_ptr->poisoned + rand_range(2, 4));
						obj_ident = TRUE;
				}
				else set_poisoned(p_ptr->poisoned + rand_range(10, 35));
			}
			break;
		}
		case SV_TONIC_BLINDNESS:
		{
			if (info) return ("");

			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_range(50, 100)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}
#if 0
		case SV_TONIC_MUTAGEN:
		{
			if (info) return ("");

			if(randint(50) < 45) gain_random_mutation(0);
			else lose_mutation(0);
			obj_ident = TRUE;
			break;
		}
#endif
		case SV_TONIC_CONFUSION:
		{
			if (info) return ("");

			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_range(20, 35)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}
#if 0
		case SV_TONIC_MUTAGEN_ANTIDOTE:
		{
			if (info) return ("");

			if(randint(50) < 45) lose_mutation(0);
			else gain_random_mutation(0);
			
			obj_ident = TRUE;
			break;
		}
#endif
		case SV_TONIC_SLEEP:
		{
			if (info) return ("");

			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_range(4, 8)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}
		case SV_TONIC_LOSE_MEMORIES:
		{
			if (info) return ("");

			if (!p_ptr->hold_life)
			{
				msg_print("Arghh!  It was a Potion of Lose Memories!");
				lose_exp(p_ptr->exp / 4);
				obj_ident = TRUE;
			}
			break;
		}
		case SV_TONIC_SKILL:
		{
			if (info) return("");
			p_ptr->free_skpts += rand_range(3, 5);
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_DEC_MUS:
		case SV_TONIC_DEC_SCH:
		case SV_TONIC_DEC_EGO:
		case SV_TONIC_DEC_AGI:
		case SV_TONIC_DEC_VIG:
		case SV_TONIC_DEC_CHR:
		{
			int stat = o_ptr->sval - SV_TONIC_DEC_MUS;

			if (info) return ("");

			if (do_dec_stat(stat)) obj_ident = TRUE;

			break;
		}
		case SV_TONIC_INFRAVISION:
		{
			if (info) return ("");
			if (set_tim_infra(p_ptr->tim_infra + rand_range(100, 200)))
			{
				obj_ident = TRUE;
			}
			break;
		}
		case SV_TONIC_DETECT_INVIS:
		{
			if (info) return ("");
			if (set_tim_invis(p_ptr->tim_invis + rand_range(12, 24)))
			{
				obj_ident = TRUE;
			}
			break;
		}
		case SV_TONIC_SLOW_POISON:
		{
			if (info) return ("");

			pow = aware ? p_ptr->poisoned / 2 : 0;

			if (set_poisoned(pow)) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_POISON:
		{
			if (info) return ("");
			if (set_poisoned(0)) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_BOLDNESS:
		{
			if (info) return ("");
			if (set_afraid(0)) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_SPEED:
		{
			dur1 = (aware ? 20 : 60);
			dur2 = (aware ? 30 : 90);

			if (p_ptr->fast)
			{
				dur1 = (aware ?  1 :  2);
				dur2 = (aware ? 10 : 20);
				extra = " longer";
			}

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_fast(p_ptr->fast + rand_range(dur1, dur2))) obj_ident = TRUE;

			break;
		}
		case SV_TONIC_HEROISM:
		{
			if (info) return ("");
			if (hp_player(10)) obj_ident = TRUE;
			if (set_afraid(0)) obj_ident = TRUE;
			if (set_hero(p_ptr->hero + rand_range(25, 50))) obj_ident = TRUE;
			break;
		}

		case SV_TONIC_BERSERK_STRENGTH:
		{
			if (info) return ("");
			if (hp_player(30)) obj_ident = TRUE;
			if (set_afraid(0)) obj_ident = TRUE;
			if (set_shero(p_ptr->shero + rand_range(25, 50))) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_LIGHT_HP:
		{

			pow1 = get_heal_amount(10, 20);
			if (info) return (format("(heal 10 percent (%d) of hps)", pow1));

			if (heal_player(10, 20)) obj_ident = TRUE;
			if (set_blind(p_ptr->blind - 5)) obj_ident = TRUE;
			if (set_stun(p_ptr->stun - 3)) obj_ident = TRUE;
			if (set_confused(p_ptr->confused - 3)) obj_ident = TRUE;
			if (set_poisoned(p_ptr->poisoned - 3)) obj_ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) obj_ident = TRUE;
			if (wp_player(1)) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_SERIOUS_HP:
		{

			pow1 = get_heal_amount(15, 30);
			if (info) return (format("(heal 15 percent (%d) of hps)", pow1));

			if (heal_player(15, 30)) obj_ident = TRUE;
			if (set_blind(p_ptr->blind / 2 - 5)) obj_ident = TRUE;
			if (set_stun(2 * p_ptr->stun / 3 - 3)) obj_ident = TRUE;
			if (set_confused(2 * p_ptr->confused / 3 - 3)) obj_ident = TRUE;
			if (set_poisoned(3 * p_ptr->poisoned / 4 - 3)) obj_ident = TRUE;
			if (set_cut(2 * p_ptr->cut / 3 - 10)) obj_ident = TRUE;
			if (wp_player(rand_range(1, 2))) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_CRITICAL_HP:
		{

			pow1 = get_heal_amount(25, 40);
			if (info) return (format("(heal 25 percent (%d) of hps)", pow1));

			if (heal_player(25, 40)) obj_ident = TRUE;
			if (set_blind(p_ptr->blind / 2 - 25)) obj_ident = TRUE;
			if (set_stun(2 * p_ptr->stun / 3 - 10)) obj_ident = TRUE;
			if (set_confused(2 * p_ptr->confused / 3 - 10)) obj_ident = TRUE;
			if (set_poisoned(3 * p_ptr->poisoned / 4 - 15)) obj_ident = TRUE;
			if (set_cut(2 * p_ptr->cut / 3 - 50)) obj_ident = TRUE;
			if (wp_player(rand_range(2, 3))) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_MORTAL_HP:
		{

			pow1 = get_heal_amount(40, 50);
			if (info) return (format("(heal 40 percent (%d) of hps)", pow1));

			if (heal_player(40, 50)) obj_ident = TRUE;
			if (set_blind((p_ptr->blind / 3))) obj_ident = TRUE;
			if (set_stun((p_ptr->stun / 3) - 40)) obj_ident = TRUE;
			if (set_confused((p_ptr->confused / 3) - 40)) obj_ident = TRUE;
			if (set_poisoned((p_ptr->poisoned / 2) - 40)) obj_ident = TRUE;
			if (set_cut((p_ptr->cut / 3) - 200)) obj_ident = TRUE;
			if (wp_player(rand_range(6, 10))) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_HP_HEALING:
		{

			pow1 = get_heal_amount(80, 300);
			if (info) return (format("(heal 80 percent (%d) of hps)", pow1));

			if (heal_player(80, 300)) obj_ident = TRUE;
			if (set_blind(0)) obj_ident = TRUE;
			if (set_stun(0)) obj_ident = TRUE;
			if (set_confused(0)) obj_ident = TRUE;
			if (set_poisoned(0)) obj_ident = TRUE;
			if (set_cut(0)) obj_ident = TRUE;
			if (wp_player(rand_range(12, 24))) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_HP_STAR_HEALING:
		{

			pow1 = get_heal_amount(100, 300);
			if (info) return (format("(heal 100 percent (%d) of hps)", pow1));

			if (heal_player(100, 300)) obj_ident = TRUE;
			if (set_blind(0)) obj_ident = TRUE;
			if (set_stun(0)) obj_ident = TRUE;
			if (set_confused(0)) obj_ident = TRUE;
			if (set_poisoned(0)) obj_ident = TRUE;
			if (set_cut(0)) obj_ident = TRUE;
			if (wp_player(1000)) obj_ident = TRUE;
			break;
		}
		case SV_TONIC_CURE_MINOR_SP:
		{

			pow1 = get_heal_sp_amount(5, 10);
			if (info) return (format("(heal 5 percent (%d) of sps)", pow1));

			if (heal_player_sp(5, 10)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_CURE_LIGHT_SP:
		{

			pow1 = get_heal_sp_amount(10, 20);
			if (info) return (format("(heal 10 percent (%d) of sps)", pow1));

			if (heal_player_sp(10, 20)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_CURE_SERIOUS_SP:
		{

			pow1 = get_heal_sp_amount(15, 30);
			if (info) return (format("(heal 15 percent (%d) of sps)", pow1));

			if (heal_player_sp(15, 30)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_CURE_CRITICAL_SP:
		{

			pow1 = get_heal_sp_amount(25, 40);
			if (info) return (format("(heal 25 percent (%d) of sps)", pow1));

			if (heal_player_sp(25, 40)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_CURE_MORTAL_SP:
		{

			pow1 = get_heal_sp_amount(40, 50);
			if (info) return (format("(heal 40 percent (%d) of sps)", pow1));

			if (heal_player_sp(40, 50)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_SP_HEALING:
		{

			pow1 = get_heal_sp_amount(80, 100);
			if (info) return (format("(heal 80 percent (%d) of sps)", pow1));

			if (heal_player_sp(80, 100)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_SP_STAR_HEALING:
		{

			pow1 = get_heal_sp_amount(100, 300);
			if (info) return (format("(heal 5 percent (%d) of sps)", pow1));

			if (heal_player_sp(100, 300)) obj_ident = TRUE;
			break;
		}		
		case SV_TONIC_LIFE:
		{
			if (info) return ("(heal ****)");

			msg_print("You feel life flow through your body!");

			restore_level();
			/* Restore all stats */
			for (i = 0; i < A_MAX; i++)
			{
				if (res_stat(i))  obj_ident = TRUE;
			}
			hp_player(5000);
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			obj_ident = TRUE;
			break;
		}

		case SV_TONIC_RESTORE_EXP:
		{
			if (info) return ("");

			if (restore_level()) obj_ident = TRUE;
			break;
		}

		case SV_TONIC_RES_MUS:
		case SV_TONIC_RES_SCH:
		case SV_TONIC_RES_EGO:
		case SV_TONIC_RES_AGI:
		case SV_TONIC_RES_VIG:
		case SV_TONIC_RES_CHR:
		{
			int stat = o_ptr->sval - SV_TONIC_RES_MUS;

			if (info) return ("");

			if (do_res_stat(stat)) obj_ident = TRUE;

			if ((!aware) && (one_in_(3)))
			{
				msg_print("The liquid coursing down your throat gains in power!");
				(void)do_inc_stat(stat);
				obj_ident = TRUE;
			}
			break;
		}
		case SV_TONIC_INC_MUS:
		case SV_TONIC_INC_SCH:
		case SV_TONIC_INC_EGO:
		case SV_TONIC_INC_AGI:
		case SV_TONIC_INC_VIG:
		case SV_TONIC_INC_CHR:
		{
			int stat = o_ptr->sval - SV_TONIC_INC_MUS;

			if (info) return ("");

			if (do_inc_stat(stat))
				obj_ident = TRUE;
			break;
		}
		case SV_TONIC_AUGMENTATION:
		{
			if (info) return ("");

			/* Restore and increase all stats by one */
			for (i = 0; i < A_MAX; i++)
			{
				res_stat(i);
				if (inc_stat(i)) obj_ident = TRUE;
			}

			/* Message -- if we gained anything */
			if (obj_ident) msg_print("You feel power flow through your body!");
			break;
		}

		case SV_TONIC_ENLIGHTENMENT:
		{
			if (info) return ("");

			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_STAR_ENLIGHTENMENT:
		{
			if (info) return ("");

			msg_print("You begin to feel more enlightened...");
			message_flush();
			wiz_lite();
			msg_print("You suddenly see a vision of the entire dungeon!");
			(void)do_inc_stat(A_SCH);
			(void)do_inc_stat(A_EGO);
			(void)detect_all();
			identify_pack();
			self_knowledge();
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_SELF_KNOWLEDGE:
		{
			if (info) return ("");

			msg_print("You begin to know yourself a little better...");
			message_flush();
			self_knowledge();
			obj_ident = TRUE;
			break;
		}
		case SV_TONIC_EXPERIENCE:
		{
			if (info) return ("");

			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				obj_ident = TRUE;
			}
			break;
		}
	}

	/* End of Potion section */
	return ("");




	/*** Handle Scrolls ***/
	do_mechanism:

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_MECHANISM_DARKNESS:
		{
			if (info) return ("");

			if (!p_ptr->resist_blind)
			{
				msg_print("A veil of darkness surrounds you.");
				if (set_blind(p_ptr->blind + rand_range(4, 8))) obj_ident = TRUE;
			}
			if (unlite_area(10, 3)) obj_ident = TRUE;
			break;
		}

		case SV_MECHANISM_AGGRAVATE_MONSTER:
		{
			if (info) return ("");

			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(-1);
			obj_ident = TRUE;
			break;
		}

		case SV_MECHANISM_CURSE_ARMOR:
		{
			if (info) return ("");

			if (curse_armor()) obj_ident = TRUE;
			break;
		}

		case SV_MECHANISM_CURSE_WEAPON:
		{
			if (info) return ("");

			if (curse_weapon()) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_SUMMON_MONSTER:
		{
			if (info) return ("");

			for (i = 0; i < rand_range(3, 5); i++)
			{
				if (summon_specific(py, px, (p_ptr->depth + 2), 0L, FALSE))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}
		case SV_MECHANISM_SUMMON_UNDEAD:
		{
			if (info) return ("");

			for (i = 0; i < rand_range(2, 3); i++)
			{
				if (summon_specific(py, px, (p_ptr->depth + 3), SUMMON_UNDEAD, FALSE))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_MECHANISM_SUMMON_AUTOMATA:
		{
			if (info) return ("");

			for (i = 0; i < rand_range(2, 3); i++)
			{
				if (summon_specific(py, px, (p_ptr->depth + 3), SUMMON_AUTOMATA, FALSE))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_MECHANISM_TRAP_CREATION:
		{
			if (info) return ("");

			/* Identify if "tried" */
			if (trap_creation(py, px)) obj_ident = TRUE;

			/* If scroll is known, traps give no exp  XXX XXX */

			break;
		}

		case SV_MECHANISM_PHASE_DOOR:
		{
			if (info) return ("(range 10)");

			teleport_player(10);
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_TELEPORT:
		{
			if (info) return ("(range 100)");

			teleport_player(100);
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_TELEPORT_LEVEL:
		{
			if (info) return ("");

			(void)teleport_player_level(TRUE);
			obj_ident = TRUE;
			break;
		}

		case SV_MECHANISM_WORD_OF_RECALL:
		{
			if (info) return ("");

			set_recall();
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_IDENTIFY:
		{
			if (info) return ("");

			obj_ident = TRUE;
			if (!ident_spell()) obj_used_up = FALSE;
			break;
		}
		case SV_MECHANISM_STAR_IDENTIFY:
		{
			if (info) return ("");

			obj_ident = TRUE;
			if (!identify_fully()) obj_used_up = FALSE;
			break;
		}

		case SV_MECHANISM_REMOVE_CURSE:
		{
			if (info) return ("");

			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				obj_ident = TRUE;
			}
			break;
		}
		case SV_MECHANISM_STAR_REMOVE_CURSE:
		{
			if (info) return ("");

			if (remove_all_curse())
			{
				msg_print("A great power breaks the curses on your gear!");
				obj_ident = TRUE;
			}
			break;
		}
		case SV_MECHANISM_POWER_CELL:
		{
			if (info) return ("");

			if (!recharge(200)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_LIGHT:
		{
			if (info) return ("");

			if (lite_area(damroll(2, 8), 2)) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_MAPPING:
		{
			if (info) return ("");

			map_area();

			/* Message */
			msg_print("You see a map of your surroundings.");

			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_DETECT_GOLD:
		{
			if (info) return ("");

			if (detect_treasure(FALSE)) obj_ident = TRUE;
			if (detect_objects_gold(FALSE)) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_DETECT_ITEM:
		{
			if (info) return ("");

			if (detect_objects_normal(FALSE)) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_DETECT_TRAP:
		{
			if (info) return ("");

			if (detect_traps(FALSE)) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_DETECT_DOOR:
		{
			if (info) return ("");

			if (detect_doors(FALSE)) obj_ident = TRUE;
			if (detect_stairs(FALSE)) obj_ident = TRUE;
			break;
		}

		case SV_MECHANISM_DETECT_INVIS:
		{
			if (info) return ("");

			if (detect_monsters_invis(FALSE)) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_RUNE_OF_PROTECTION:
		{
			if (info) return ("");

			warding_glyph();
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_TRAP_DOOR_DESTRUCTION:
		{
			if (info) return ("");
			if (destroy_doors_touch()) obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_STAR_DESTRUCTION:
		{
			if (info) return ("");

			msg_print("KABOOM!");
			destroy_area(py, px, 15, TRUE);
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_ACQUIREMENT:
		{
			if (info) return ("");

			acquirement(py, px, (aware ? 1 : 2), TRUE);
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_STAR_ACQUIREMENT:
		{
			if (info) return ("");

			acquirement(py, px, (aware ? 3 : 4), TRUE);
			obj_ident = TRUE;
			break;
		}
		case SV_MECHANISM_MECHA_HEALING:		
		{
			if (f3 & (TR3_AUTOMATA))
			{
				pow1 = get_heal_amount(15, 30);
				if (info) return (format("(heal 15 percent (%d) of hps)", pow1));
	
				if (heal_player(15, 30)) obj_ident = TRUE;
				if (set_blind(p_ptr->blind / 2 - 5)) obj_ident = TRUE;
				if (set_confused(2 * p_ptr->confused / 3 - 3)) obj_ident = TRUE;
				if (set_poisoned(3 * p_ptr->poisoned / 4 - 3)) obj_ident = TRUE;
				if (set_cut(2 * p_ptr->cut / 3 - 10)) obj_ident = TRUE;
				if (wp_player(rand_range(4, 12))) obj_ident = TRUE;
				break;
			}
		}
		case SV_MECHANISM_MECHA_STAR_HEALING:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				pow1 = get_heal_amount(80, 300);
				if (info) return (format("(heal 80 percent (%d) of hps)", pow1));
	
				if (heal_player(80, 300)) obj_ident = TRUE;
				if (set_blind(0)) obj_ident = TRUE;
				if (set_confused(0)) obj_ident = TRUE;
				if (set_poisoned(0)) obj_ident = TRUE;
				if (set_cut(0)) obj_ident = TRUE;
				if (wp_player(150)) obj_ident = TRUE;
				break;
			}
		}
		case SV_MECHANISM_MECHA_ENERGY:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				pow1 = get_heal_sp_amount(15, 30);
				if (info) return (format("(heal 15 percent (%d) of sps)", pow1));
	
				if (heal_player_sp(15, 30)) obj_ident = TRUE;
				break;
			}
		}
		case SV_MECHANISM_MECHA_STAR_ENERGY:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				pow1 = get_heal_sp_amount(80, 100);
				if (info) return (format("(heal 80 percent (%d) of sps)", pow1));
	
				if (heal_player_sp(80, 100)) obj_ident = TRUE;
				break;
			}
		}
	}
	return ("");
}


/*
 * Process the effects of individual magical devices.  Return any
 * information string (info mode only).  If the device got used, which does
 * not happen if the user cancels at some point, set "used" to TRUE.  If
 * the effects were (probably) noticeable, set "ident" to TRUE.
 *
 * Allow uncontrolled usage of most magical devices.  Devices that require
 * a direction should be given one automatically, but those that require
 * other kinds of user interaction should usually not go off.
 */
cptr do_device(int mode, object_type *o_ptr, bool *ident, bool *used,
	bool uncontrolled)
{
	int i;
	
	/* Get the object kind */
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Modes of operation */
	bool use  = (mode == OBJECT_USE);
	bool info = (mode == OBJECT_INFO);

	/* Output special message only once */
	/* u16b msg = !(k_ptr->special & (SPECIAL_MESSAGE)); */
	int k, dur1, dur2, dice, sides, beam;
	int power1, power2, healfac, efficiency;
	int py = p_ptr->py;
	int px = p_ptr->px;
	int devicegood, devicenorm, devicepoor, advdevice;
	int skill, lev, chance;

	int sval = o_ptr->sval;

	/* Allow pre-set targets (in uncontrolled mode) */
	int dir = 0;

	/* Usually ask for a direction when appropriate */
	bool need_dir = (uncontrolled ? FALSE : TRUE);

	/* Determine device skill (0 to 100) */
	/* int power = get_skill(S_DEVICE, 0, 100); */

	char buf[80];

	/* Get the device factor */
	devicegood = p_ptr->skills[SK_DEVICE_GOOD].skill_rank;
	devicenorm = p_ptr->skills[SK_DEVICE_NORM].skill_rank;
	devicepoor = p_ptr->skills[SK_DEVICE_POOR].skill_rank;
	
	if (p_ptr->skills[SK_ADV_DEVICE].skill_max > 0)	advdevice = p_ptr->skills[SK_ADV_DEVICE].skill_rank;
	else advdevice = 0;

	/* Base chance of success */
	/* This needs to be displayed on the confirmation of use item */

	/* paranoia*/
	skill = 0;
	
	/* Gives a max skill result (unmodified by items) of 140 / 2 */
	/* or 70, 20 points higher than a level 50 device - giving a */
	/* 100% success rate for the deepest device. */
	if (devicegood > 0) skill = ((5 * devicegood) / 2) + advdevice;

	/* Unmodified 100 / 2 == 50, giving a 100% success rate on */
	/* items level 30 and below -- and eaisly modified up to perfect */
	if (devicenorm > 0) skill = ((3 * devicenorm) / 2) + advdevice;

	/* You suck at devices */
	if (devicepoor > 0) skill = devicepoor + (advdevice/2);

	/* Confusion and hallucination make things harder */
	if ((p_ptr->confused) || (p_ptr->image)) skill /= 2;

	/* Stunning makes things a little harder */
	if (p_ptr->stun) skill -= p_ptr->stun / 2;

	/* Blindness makes things a little harder */
	if (p_ptr->blind) skill -= skill / 3;

	/* Get the object level (no maximum) */
	lev = k_info[o_ptr->k_idx].level;

	/*
	 * Determine percentage chance of success.
	 * Is zero if skill is 10 below level, is perfect if it is 20 above.
	 * ok, strange.
	 */
	if (skill < lev) chance = 10 + skill - lev;
	else             chance = 10 + rsqrt((skill - lev) * 405);

	power1 = p_ptr->skills[SK_POW_DEVICE].skill_rank;
	power2 = (p_ptr->skills[SK_POW2_DEVICE].skill_rank + 1 / 2);
	healfac = p_ptr->skills[SK_HEAL_DEVICE].skill_rank;
	efficiency = p_ptr->skills[SK_EFF_DEVICE].skill_rank;
	
	/* paranoia */
	if (power1 < 1) power1 = 1;
	if (power2 < 1) power2 = 1;
	if (healfac < 1) healfac = 1;
	if (efficiency < 1) efficiency = 1;

	/* Unknown wands and rods always need to be "used in a direction" */
	if ((use) && (need_dir) && (!object_aware_p(o_ptr)) &&
	    ((o_ptr->tval == TV_RAY) || (o_ptr->tval == TV_APPARATUS)))
	{
		if (!get_aim_dir(&dir)) return ("");
		need_dir = FALSE;
	}

	/* Sound */
	if (use) sound(MSG_ZAP);


	/* Jump to the right category of item */
	if      (o_ptr->tval == TV_TOOL) goto do_tool;
	else if (o_ptr->tval == TV_RAY)  goto do_ray;
	else if (o_ptr->tval == TV_APPARATUS)   goto do_apparatus;
	else return ("");


	/*** Handle Staffs ***/
	do_tool:

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_TOOL_ANTI_PHOTON:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				/* Darkness */
				if (unlite_area(10, 3)) *ident = TRUE;

				if (!p_ptr->resist_blind)
				{
					msg_print("A veil of darkness surrounds you.");
					if (set_blind(p_ptr->blind + rand_range(6, 12))) *ident = TRUE;
				}
			}
			break;
		}

		case SV_TOOL_SUMMONING:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				for (k = 0; k < randint(4); k++)
				{
					if (summon_specific(py, px, (p_ptr->depth + 3), 0L, FALSE))
					{
						*ident = TRUE;
					}
				}
			}
			break;
		}

		case SV_TOOL_OBJECT_ANALYSIS:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				*ident = TRUE;
				if ((uncontrolled) || (!ident_spell())) return ("");
			}
			break;
		}
		case SV_TOOL_REMOVE_CURSE:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (remove_curse())
				{
					if (!p_ptr->blind) msg_print("The tool hums for a second...");

					msg_print("A curse is broken!");

					*ident = TRUE;
				}
			}
			break;
		}

		case SV_TOOL_PHOTON_GEN:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (lite_area(damroll(3 + power1, 4 + power2), 2 + (power2 / 5)))
				{
					/* Note effects if not blind */
					if (!p_ptr->blind) *ident = TRUE;
				}
			}
			break;
		}
		
		case SV_TOOL_MAPPING:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				/* Use extended mapping */
				map_area();

				/* Message */
				msg_print("A map of your surroundings is displayed.");

				*ident = TRUE;
			}
			break;
		}
		case SV_TOOL_DETECT_GOLD:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (detect_treasure(TRUE)) *ident = TRUE;
				if (detect_objects_gold(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_TOOL_DETECT_ITEM:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (detect_objects_normal(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_TOOL_DETECT_TRAP:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (detect_traps(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_TOOL_DETECT_DOOR:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (detect_doors(TRUE)) *ident = TRUE;
				if (detect_stairs(TRUE)) *ident = TRUE;
			}
			break;
		}
		case SV_TOOL_PROBING:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (probing()) *ident = TRUE;
			}
			break;
		}
		case SV_TOOL_CURE_LIGHT_HP:
		{
			if (info) return (format("(heal about %d percent) (You have a %d%% chance to use this item successfully)", healfac * 2, chance));
			if (use)
			{
				if (heal_player(healfac * 2, rand_range(10, 20))) *ident = TRUE;
			} 
			break;
		}
		case SV_TOOL_CURE_LIGHT_SP:
		{
			if (info) return (format("(heal about %d percent) (You have a %d%% chance to use this item successfully)", healfac * 2, chance));
			if (use)
			{
				if (heal_player_sp(healfac * 2, rand_range(10, 20))) *ident = TRUE;
			} 
			break;
		}
		case SV_TOOL_HEALING:
		{
			if (info) return (format("(heal about %d percent) (You have a %d%% chance to use this item successfully)", healfac * 3, chance));
			if (use)
			{
				if (heal_player(healfac * 3, rand_range(10, 20))) *ident = TRUE;
				if (heal_player_sp(healfac * 3, rand_range(10, 20))) *ident = TRUE;
			} 
			break;
		}
		case SV_TOOL_CURING:
		{
			if (info) return (format("(reduce cuts, poison, stun, blind, and confusion) (You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if (set_stun(0)) *ident = TRUE;
				if (set_cut(0)) *ident = TRUE;
				if (set_blind(0)) *ident = TRUE;
				if (set_poisoned(0)) *ident = TRUE;
				if (set_confused(0)) *ident = TRUE;
			}
			break;
		}
		case SV_TOOL_SPEED:
		{
			cptr extra = (p_ptr->fast ? " longer" : "");

			dur1 = (p_ptr->fast ?  1 : 20);
			dur2 = (p_ptr->fast ? 10 : 45);

			if (info) return (format("(duration: %d-%d%s) (You have a %d%% chance to use this item successfully)", dur1, dur2, extra, chance));
			if (use)
			{
				if (set_fast(p_ptr->fast + rand_range(dur1, dur2)))
					*ident = TRUE;
			}
			break;
		}
		case SV_TOOL_EARTHQUAKES:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				msg_print("The earth shakes!");
				earthquake(py, px, 10);
				*ident = TRUE;
			}
			break;
		}

		case SV_TOOL_DESTRUCTION:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				msg_print("KABOOM!");
				destroy_area(py, px, 15, TRUE);
				*ident = TRUE;
			}
			break;
		}
		
		case SV_TOOL_TELEPORT:
		{
			if (info) return (format("Teleports you (range 100) (You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				teleport_player(100);
				*ident = TRUE;
			}
		}
		


	}

	/* We used the magical device */
	*used = TRUE;

	/* End of Staffs section */
	return ("");



	/*** Handle Wands ***/
	do_ray:

	dice = 1 + power1;
	sides = 6 + power2;
	beam = (power1 * 2) + (power2 * 3);

	/* Analyze the wand */
	switch (sval)
	{
		case SV_RAY_FIRE_BOLT:
		{
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_FIRE, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_EARTH_BOLT:
		{
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_EARTH, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_AIR_BOLT:
		{
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_WIND, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_WATER_BOLT:
		{
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_STEAM, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_ELEC_BOLT:
		{
			dice += 2;
			sides += 2;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_ELEC, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_ICE_BOLT:
		{
			dice += 2;
			sides += 2;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_ICE, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_ACID_BOLT:
		{
			dice += 2;
			sides += 2;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_ACID, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_POISON_BOLT:
		{
			dice += 2;
			sides += 2;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_POISON, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_TIME_BOLT:
		{
			dice += 8;
			sides += 8;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_TIME, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_ETHER_BOLT:
		{
			dice += 10;
			sides += 10;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_ETHER, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_SONIC_BOLT:
		{
			dice += 12;
			sides += 12;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_SONIC, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_EMP_BOLT:
		{
			dice += 14;
			sides += 14;
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_EMP, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}		
		case SV_RAY_LIGHT:
		{
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				fire_bolt_or_beam(beam, GF_LIGHT, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}
		case SV_RAY_CLONE_MONSTER:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (clone_monster(dir)) *ident = TRUE;
			}
			break;
		}
		case SV_RAY_TELEPORT_AWAY:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (teleport_monster(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_RAY_SLEEP_MONSTER:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (sleep_monster(dir)) *ident = TRUE;
			}
			break;
		}
		case SV_RAY_CONFUSE_MONSTER:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (confuse_monster(dir, damroll(dice, sides))) *ident = TRUE;
			}
			break;
		}
		case SV_RAY_SLOW_MONSTER:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (slow_monster(dir)) *ident = TRUE;
			}
			break;
		}
		case SV_RAY_FEAR_MONSTER:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (fear_monster(dir, damroll(dice, sides))) *ident = TRUE;
			}
			break;
		}

		case SV_RAY_POLYMORPH:
		{
			if (info) return (format("(You have a %d%% chance to use this item successfully)", chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (poly_monster(dir)) *ident = TRUE;
			}
			break;
		}
		case SV_RAY_STONE_TO_MUD:
			if (info) return (format("(damage: %dd%d) (You have a %d%% chance to use this item successfully)", dice, sides, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (wall_to_mud(dir)) *ident = TRUE;
			}
			break;
		
	}
	/* We used the magical device */
	*used = TRUE;

	/* End of Wands section */
	return ("");



	/*** Handle Rods ***/
	do_apparatus:

	/* Display time to recharge */
	if (o_ptr->pval < 1) sprintf(buf, "(charged)");
	else sprintf(buf, "in another %d turns", o_ptr->pval);

	/* Analyze the rod */
	switch (o_ptr->sval)
	{
		case SV_APPARATUS_RECALL:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				set_recall();
				*ident = TRUE;
				o_ptr->pval = 60 - (efficiency * 2);
			}
			break;
		}
		case SV_APPARATUS_LIGHT:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				if (lite_area(damroll(2, 8), 2)) *ident = TRUE;
				o_ptr->pval = 10 - (efficiency / 3);
			}
			break;
		}
		case SV_APPARATUS_PISTOL_AMMO:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				object_type *i_ptr;
				object_type object_type_body;
				required_tval = TV_AMMO;

				/* Get local object */
				i_ptr = &object_type_body;
		
				/* Wipe the object */
				object_wipe(i_ptr);

				if (!make_object(i_ptr, FALSE, FALSE, TRUE)) break;
				
				/* Insure that the bullets can not be sold for money "Worthless" */
				i_ptr->ident |= (IDENT_BROKEN);

				object_aware(i_ptr);
				object_known(i_ptr);

				/* Drop the object */
				drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

				/* Cancel tval forcing */
				required_tval = 0;

				*ident = TRUE;
				o_ptr->pval = 500 - (efficiency * 10);
			}
			break;
		}
		case SV_APPARATUS_RIFLE_AMMO:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				object_type *i_ptr;
				object_type object_type_body;
				required_tval = TV_BULLET;

				/* Get local object */
				i_ptr = &object_type_body;
		
				/* Wipe the object */
				object_wipe(i_ptr);

				if (!make_object(i_ptr, FALSE, FALSE, TRUE)) break;
		
				/* Insure that the bullets can not be sold for money "Worthless" */
				i_ptr->ident |= (IDENT_BROKEN);

				object_aware(i_ptr);
				object_known(i_ptr);

				/* Drop the object */
				drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

				/* Cancel tval forcing */
				required_tval = 0;

				*ident = TRUE;
				o_ptr->pval = 600 - (efficiency * 10);
			}
			break;
		}
		case SV_APPARATUS_SHOTGUN_AMMO:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				object_type *i_ptr;
				object_type object_type_body;
				required_tval = TV_SHOT;

				/* Get local object */
				i_ptr = &object_type_body;
		
				/* Wipe the object */
				object_wipe(i_ptr);

				if (!make_object(i_ptr, FALSE, FALSE, TRUE)) break;

				/* Insure that the bullets can not be sold for money "Worthless" */
				i_ptr->ident |= (IDENT_BROKEN);

				object_aware(i_ptr);
				object_known(i_ptr);

				/* Drop the object */
				drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

				/* Cancel tval forcing */
				required_tval = 0;

				*ident = TRUE;
				o_ptr->pval = 700 - (efficiency * 10);
			}
			break;
		}
		case SV_APPARATUS_RESTORE:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				for (i = 0; i < A_MAX; i++)
				{
					if (res_stat(i))  *ident = TRUE;
				}
				o_ptr->pval = 1000 - (efficiency * 45);
			}
			break;
		}
		case SV_APPARATUS_DISARMING:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				if (disarm_trap(dir)) *ident = TRUE;
				o_ptr->pval = 5 - (efficiency/5);
			}
			break;
		}
		case SV_APPARATUS_ANTI_TELEPORT:
		{
			if (info) return (format("%s (You have a %d%% chance to use this item successfully)", buf, chance));
			if (use)
			{
				(void)set_tim_no_tele(p_ptr->tim_no_tele + randint(100) + 50);
				*ident = TRUE;
				o_ptr->pval = 200 - (efficiency * 5);
			}
			break;			
		}
	}


	/* We used the magical device */
	*used = TRUE;

	/* Return */
	return ("");
}

/*
 * Handle activations:  effects, information display.
 */
cptr do_activation_aux(int mode, object_type *o_ptr)
{
	artifact_type *a_ptr = &a_info[o_ptr->name1];

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Modes of operation */
	bool info = (mode == OBJECT_INFO);
	bool act =  (mode == OBJECT_USE);

	int i, dir, b, k, ty, tx;

	int dam;
	int dice, sides;
	int timeout1 = 0;
	int timeout2 = 0;

	/* set artifact recharge values */
	timeout1 = a_ptr->time;
	timeout2 = a_ptr->randtime;
	
	/* No activation */
	if (!o_ptr->name1) return ("");

	/* Process activation */
	switch (o_ptr->name1)
	{
		case ART_EDISON_LITE:
		{
			dice = 4; sides = 20;
			if (info) return (format("increases vision and courage plus illumination every %d-%d turns", timeout1, timeout2));

			msg_print("The electric light fills the room with brightness...");

			/* Cure moderate fear */
			if (p_ptr->afraid < 30) set_afraid(0);

			/* Cure minor blindness */
			if (p_ptr->blind < 10) set_blind(0);

			/* Light up the room */
			lite_room(py, px);
	
			/* Burst of light */
			fire_star(GF_LIGHT, 0, damroll(dice, sides), 4);
			
			break;
		}
		
		case ART_HOLMES:
		{
			if (info) return (format("detects all, and maps your surroundings every %d-%d turns", timeout1, timeout2));
			if (act) 
			{	
				msg_print("The pocket lantern floods your mind with knowledge...");
				detect_all();
				map_area();
			}	
			break;
		}
		
		case ART_TESLA_LITE:
		{
			if (info) return (format("discharges a lightning storm every %d-%d turns", timeout1, timeout2));
			if (act) 
			{	
				msg_print("You feel your hair stand on end.");
				msg_print("The photic illuminator jerks in your hand!");
				beam_burst(p_ptr->py, p_ptr->px, GF_VOLT, rand_range(10, 14), rand_range(200, 400));
			}	
			break;

		}
		
		case ART_ARBACES:
		{
			if (info) return (format("invokes the evil eye every %d-%d turns", timeout1, timeout2));
			if (act) 
			{	
				u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_JUMP | PROJECT_HIDE;
				msg_print("You glare at your opponent.");
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt_beam_special(GF_PSI, dir, rand_range(350, 800), MAX_RANGE, flg);
			}	
			break;
		}
		
		case ART_DEE:
		{
			if (info) return (format("summons supernatural aid every %d-%d turns", timeout1, timeout2));
			if (act) 
			{	
				
				msg_print("You summon a servant to do your bidding.");
				summon_specific(p_ptr->py, p_ptr->px, (p_ptr->depth + 10), 0, TRUE);
			}
			break;
		}		
		
		case ART_NEMO_NAUTILUS:
		{
			if (info) return (format("heals you and increases your resistance every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int time = 50;
				msg_print("You are bathed in righteous fury!");
				(void)heal_player(80, 300);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
				(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
				(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
				(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
				(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
				(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
				(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
				(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
				(void)set_tim_res(RS_TIM, p_ptr->tim_res[RS_TIM] + time);
				(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
				(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
				(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);			
			}
			break;
		}
		
		case ART_MATERIALISMUS:
		{
			if (info) return (format("produces fear every %d-%d turns", timeout1, timeout2));			
			if (act)
			{
				msg_print("physical movement produces psychical passion!");
				project_los(GF_FEAR, rand_range(50, 100));
			}
			break;
		}
		case ART_EDISON:
		{
			if (info) return (format("does some healing and possible teleport away every %d-%d turns", timeout1, timeout2));			
			if (act)
			{
				int mystery = randint(8);
				if (!get_aim_dir(&dir)) return ("");
				dam = rand_range(100, 600);
				/* notice the fall though */
				switch (mystery)
				{
					case 8: remove_all_curse();
					case 7: identify_pack();
					case 6: hp_player(500);
					case 5: for (i = 0; i < A_MAX; i++) do_res_stat(i);
					case 4: fire_arc(GF_AWAY_ALL, dir, dam, 8, 75);
					case 3: heal_player_sp(60, 100);
					case 2: restore_level();
					case 1: hp_player(500);
				}
			}			
			break;
		}
		case ART_TESLA_RING:
		{
			if (info) return (format("does something violent every %d-%d turns", timeout1, timeout2));			
			if (act)
			{
				int mystery = randint(8);
				dam = rand_range(500, 1500);
				
				if (mystery == 1) 
				{
					if (!get_aim_dir(&dir)) return ("");
					msg_print("A beam of plasma is fired from the ring!"); 
					fire_beam(GF_PLASMA, dir, dam);
				}
				else if (mystery == 2) 
				{
					if (!get_aim_dir(&dir)) return ("");
					msg_print("The ring summons a windstorm!"); 
					fire_orb(GF_GALE, dir, dam, 4);
				}
				else if (mystery == 3) 
				{
					if (!get_rep_dir(&dir)) return ("");
					msg_print("Poison attacks your enemies!"); 
					fire_arc(GF_CONTAGION, dir, dam, 4, 60);
				}
				else if (mystery == 4) 
				{
					/* Something is wrong with this. */
					msg_print("The ring makes a sonic boom!"); 
					fire_star(GF_SONIC, 0, dam, rand_range(6, 9));
				}
				else if (mystery == 5) 
				{
					if (!get_aim_dir(&dir)) return ("");
					msg_print("Rockets fire!"); 
					fire_barrage(GF_SHARDS, dir, 15, 100, 10, 2, 3);
				}
				else if (mystery == 6) 
				{
					msg_print("An explosion destroys the walls around you!"); 
					beam_burst(p_ptr->py, p_ptr->px, GF_KILL_WALL, 10, 100);
					project_star(-1, 8, p_ptr->py, p_ptr->px, dam, GF_HURT, 0L); 
				} 
				else if (mystery == 7) 
				{
					msg_print("A storm of energy occurs!"); 
					fire_storm(-1, -1, p_ptr->py, p_ptr->px, dam, 5, 6, 50, 1, TRUE);
				}
				else if (mystery == 8) 
				{
					msg_print("Monsters disintegrate around you!"); 
					dispel_monsters(1000);
				}
			}			
			break;
		}
		
		case ART_DUPIN:
		{
			if (info) return (format("locate hidden objects every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("A letter purloined indeed!");
				(void)detect_objects_magic(FALSE);
				(void)detect_objects_normal(FALSE);
			}
			break;
		}
		case ART_LOUP_BLANC:
		{
			if (info) return (format("rage every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Vengance will be mine!");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 100 + randint(50));

			}
			break;
		}
		case ART_ISIS_CROWN:
		{
			if (info) return (format("summons the undead and protects you from evil every %d-%d turns", timeout1, timeout2));
			if (act) 
			{	
				
				(void)set_protevil(p_ptr->protevil + randint(125) + 125);
				msg_print("You summon a servant to do your bidding.");
				for (i = 0; i < 3; i++)
				{	
					summon_specific(p_ptr->py, p_ptr->px, (p_ptr->depth + 10), SUMMON_UNDEAD, TRUE);
				}
			}
			break;
		}		
		case ART_BULL_PHAROAH:
		{
			if (info) return (format("summons a great storm every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				/* Get a direction */
				if (!get_hack_dir(&dir)) return FALSE;
	
				 for (b = 0; b < 6; b++)
				 {
					 /* Get a new effect index */
					 k = effect_prep();
	
					 /* Note failure XXX */
					 if (k < 0) break;
	
					 /* We want a spirit, */
					 x_list[k].index = EFFECT_SEEKER_VORTEX;
	
					 /* Of fire */
					 x_list[k].type = GF_STEAM;
	
					/* Use the given direction */
					ty = py + ddy[dir];
					tx = px + ddx[dir];
					
					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						ty = p_ptr->target_row;
						tx = p_ptr->target_col;
					}
					 /* That starts at the character location. */
					 x_list[k].y0 = ty;
					 x_list[k].x0 = tx;
	
					 /* Moves with a speed that depends on the wind, */
					 x_list[k].time_delay = 3;
	
					 /* Does damage, 1 + 9 + 6 / 2 d 3 or 8d3 */
					 x_list[k].power = damroll(9, 3);
	
					 /* And lasts for a certain period of time. */
					 x_list[k].lifespan = 40;
				}
	
				k = effect_prep();
	
				/* Note failure XXX */
				if (k < 0) break;
	
				/* Use the given direction */
				ty = py + ddy[dir];
				tx = px + ddx[dir];
				
				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
				}
	
				/* We want an lingering cloud, */
				x_list[k].index = EFFECT_IRREGULAR_CLOUD;
	
				/* Of ICE */
				x_list[k].type = GF_STORM;
	
				/* That starts at the monster location. */
				x_list[k].y0 = x_list[k].y1 = ty;
				x_list[k].x0 = x_list[k].x1 = tx;
	
				/* It attacks every 8 -> 5 game turns, */
				x_list[k].time_delay = 5;
	
				/* Does damage, has a large radius, */
				/* 13d11 == 143 dam */
				x_list[k].power = damroll(15, 10);
				x_list[k].power2 = 10;
	
				/* And lasts for about 10 attacks */
				x_list[k].lifespan = 50;
				break;
			}			
		}
		case ART_EYE_RA:
		{
			if (info)  return (format("fires a beam of blinding light every %d-%d turns", timeout1, timeout2));	
			if (act)
			{ 	
				if (!get_aim_dir(&dir)) return ("");
				msg_print("The blinding light of Ra shoots forth!");
				fire_beam(GF_BRILLIANCE, dir, rand_range(400, 800));
			}
			break;
		}
		case ART_PECTORAL_KHEPER:
		{
			if (info)  return (format("creates a sanctuary every %d-%d turns", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("The scarab protects you!");
				for (b = 0; b < 4; b++)
				{
					/* Get a new effect index */
					k = effect_prep();
	
					/* Note failure XXX */
					if (k < 0) break;
	
					ty = py + ddy_cdd[(b * 2) % 8];
					tx = px + ddx_cdd[(b * 2) % 8];

					/* We want an advancing wall, */
					x_list[k].index = EFFECT_STATIC_WALL;
	
					/* Of fire */
					x_list[k].type = GF_FORCE;
					
					/* That starts at the character location. */
					x_list[k].y0 = 	py;
					x_list[k].x0 =  px;
	
					/* It advances one grid every two or three game turns, */
					x_list[k].time_delay = 5;
	
					/* Heads for the target grid, */
					x_list[k].y1 = ty;
					x_list[k].x1 = tx;
	
					/* Does damage, */
					x_list[k].power = 20;
	
					/* And lasts for a certain period of time. */
					x_list[k].lifespan = 25;
				}
				for (b = 0; b < 4; b++)
				{
					/* Get a new effect index */
					k = effect_prep();
	
					/* Note failure XXX */
					if (k < 0) break;
	
					ty = py + ddy_cdddouble[(b * 2) % 8];
					tx = px + ddx_cdddouble[(b * 2) % 8];

					/* We want an advancing wall, */
					x_list[k].index = EFFECT_STATIC_WALL;
	
					/* Of fire */
					x_list[k].type = GF_FORCE;
					
					/* That starts at the character location. */
					x_list[k].y0 = 	py;
					x_list[k].x0 =  px;
	
					/* It advances one grid every two or three game turns, */
					x_list[k].time_delay = 5;
	
					/* Heads for the target grid, */
					x_list[k].y1 = ty;
					x_list[k].x1 = tx;
	
					/* Does damage, */
					x_list[k].power = 20;
	
					/* And lasts for a certain period of time. */
					x_list[k].lifespan = 25;
				}
			}
			break;
		}
		/* case ART_LOCKET_DEFARGE: NO ACTIVATION */
		case ART_SLIPPERS_DORTHY:
		{
			if (info) return (format("returns home every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Click! Click! Click!");
				set_recall();
				break;		
			}
		}
		
		/* END INSTA_ARTS */
		case ART_HOLMES_PEA:
		{
			if (info)  return (format("calculates an escape every %d-%d turns with a range of 10", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("The pea jacket calculates a quick escape");
				teleport_player(10);
			}
			break;
		}
		/* case ART_BARR: NO ACTIVATION */
		case ART_BELL:
		{
			if (info)  return (format("dispels the supernatural (undead) every %d-%d turns", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("The supernatural dissolves before your eyes!");
				project_los(GF_DISP_UNDEAD, 1000);
			}
		}
		/* case ART_BLAKE_TWEED: NO ACTIVATION */
		case ART_SAWYER_JACKET:
		{
			if (info)  return (format("gets you out of trouble every %d-%d turns with a range of 100", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("You squirm out of trouble!");
				teleport_player(100);
			}
			break;
			
		}
		case ART_NOBEL_SUIT:
		{
			if (info) return (format("resist explosives every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int time = (randint(50) + 50);
				msg_print("If I have a thousand ideas and only one turns out to be good, I am satisfied!");
				(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
				(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
				(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
				(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
				(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
				(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);
				break;
			}
			break;
		}
		/* case ART_J_HARKER_VEST: NO ACTIVATION */
		/* case ART_RACKHAM_SILK: NO ACTIVATION */
		/* case ART_EMMA_ROBE: NO ACTIVATION */
		case ART_FREUD_TWEED:
		{
			if (info) return (format("cures mental deficiencies every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Sometimes a cigar is just a cigar.");
				(void)set_confused(0);
				(void)set_afraid(0);
				(void)do_res_stat(A_SCH);
				(void)do_res_stat(A_EGO);
			}
			break;
		}	
		case ART_VICTOR_SMOKE:
		{
			if (info) return (format("create a monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (one_in_(40))
				{
					msg_print("The monster turns on its creator!");
					for (i = 0; i < 3; i++)
					{	
						summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_UNDEAD, FALSE);
					}
				}
				else
				{
					msg_print("You create a monster.");
					for (i = 0; i < 3; i++)
					{	
						summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_UNDEAD, TRUE);
					}
				}
				break;
			}
			break;
		}
		case ART_POIROT_WAISTCOAT:
		{
			if (info) return (format("map your surroundings every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("I am Hercule Poirot and I know.");
				map_area();
				break;
			}
			break;
		}
		case ART_HARRY_TROUSERS:
		{
			if (info) return (format("flee in panic every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You cry out!");
				set_afraid(randint(20)+ 2);
				set_fast(randint(40)+ 2);
				break;		
			}
		}
		case ART_LIVINGSTONE_TROUSERS:
		{
			if (info) return (format("examine a monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("I am prepared to go anywhere, provided it be forward.");
				probing();
			}
			break;
		}
		/* case ART_M_HARKER_PETTYCOATS: NO ACTIVATION */
		/* case ART_ROSA_PETTYCOATS: NO ACTIVATION */
		case ART_SEXTON_BOWLER:
		{
			if (info) return (format("protects you from wrongdoers every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Wrongdoers beware!");
				(void)set_protevil(p_ptr->protevil + randint(25) + 25);
			}
			break;
		}
		case ART_BUCKET:
		{
			if (info) return (format("sends your enemies away every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Begone!");
				if (!get_aim_dir(&dir)) return ("");
				teleport_monster(dir);
			}
			break;
		}
		case ART_HOLMES_DEER:
		{
			if (info) return (format("examine a monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Elementary!");
				probing();
			}
			break;
		}
		case ART_EDISON_HAT:
		{
			if (info) return (format("whisk yourself away every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Its obvious that we don't know one millionth of one percent about anything.");
				teleport_player(80);
			}
			break;
		}
		case ART_GAUSS_CAP:
		{
			if (info) return (format("detect objects every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("God does arithmetic");
				detect_objects_normal(TRUE);
				break;
			}
			break;
		}
		case ART_HARDWIGG_SPECTACLES:
		{
			if (info) return (format("detect stairs/traps every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)detect_traps(FALSE);
				(void)detect_stairs(FALSE);
				break;
			}
			break;
		}
		case ART_ALERIEL:
		{
			if (info) return (format("resist hostile environments every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int time = (randint(50) + 50);
				msg_print("By the power of the 10 spheres!");
				(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
				(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
				(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
				(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
				(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
				(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
				(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
				(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
				(void)set_tim_res(RS_TIM, p_ptr->tim_res[RS_TIM] + time);
				(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
				(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
				(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);
			}
			break;
		}
		/* case ART_NEMO_CLOAK: NO ACTIVATION */
		case ART_DRACULA_CLOAK:
		{
			if (info) return (format("drain life every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("I am Dracula, and I bid you welcome . . . ");
				if (!get_aim_dir(&dir)) return ("");
				drain_life(dir, rand_range(400, 800));	
			}
			break;
		}
		/* case ART_HEARTH_CLOAK: NO ACTIVATION */
		/* case ART_INNOMINATO_CLOAK: NO ACTIVATION */
		case ART_IGNAZ_CLOAK:
		{
			if (info) return (format("heal yourself every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("How difficult is it to just wash your hands?!");
				heal_player(100, 1000);
				wp_player(20);
				break;			
			}
			break;
		}
		/* case ART_MYCROFT_MANTLE: NO ACTIVATION */

		case ART_IMMORTAL_MANTLE:
		{
			if (info) return (format("immortality every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)set_invuln(p_ptr->invuln + randint(5) + 5);
				break;		
			}
		}
		
		case ART_FOGG_SILK_GLOVES:
		{
			if (info) return (format("haste every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You are too slow.");
				if (!p_ptr->fast) (void)set_fast(randint(75) + 75);
				else (void)set_fast(p_ptr->fast + 5);
				break;
			}
			break;
		}
		case ART_SHADOW_GLOVES:
		{
			if (info) return (format("cloud men's minds every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Who knows what evil lurks in the hearts of men!");
				set_tim_invisiblity(p_ptr->tim_invisiblity + 25 + rand_int(30));
				break;
			}
			break;
		}
		case ART_NOBEL_GLOVES:
		{
			if (info) return (format("detonate a charge every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("BOOM!");
				if (!get_aim_dir(&dir)) return ("");
				fire_ball_special(GF_ROCKET, dir, damroll(22, 25), 4, PROJECT_WALL, 0L);
				break;
			}
			break;
		}
		case ART_PUCK_GLOVES:
		{
			if (info)  return (format("sleeps nearby monsters every %d-%d turns", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("So, good night unto you all.");
				project_los(GF_SLEEP, (30 + randint(40)));
			}
			break;
		}
		case ART_HOUDINI_GLOVES: 
		{
			if (info)  return (format("calculates an escape every %d-%d turns with a range of 4", timeout1, timeout2));	
			if (act)
			{ 	
				msg_print("No bonds can hold me!");
				teleport_player(4);
			}
			break;
		}
		
		case ART_BOOTS_BJELKE:
		{

			if (info) return (format("heroism every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				set_hero(randint(40)+ 2);
				break;		
			}
		}
		case ART_BOOTS_SEVEN_LG:
		{
			if (info) return (format("teleport player every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				teleport_player(120);
				break;		
			}
		}
		case ART_BOOTS_NILS:			
		{
			if (info) return (format("recall to the surface every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				set_recall();
				break;		
			}
		}

		
		/* case ART_JOHN_HENRY_HAMMER: NO ACTIVATION */
		
		case ART_CUTLASS_NEMO:
		{
			if (info) return (format("binds monsters every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Strike, mad vessel! Shower your useless shot! And then, you will not escape the spur of the Nautilus.");
				if (!get_aim_dir(&dir)) return (""); 
				fire_bolt(GF_STASIS, dir, rand_range(10, 20));
				break;
			}
			break;
		}
		case ART_CANE_HYDE:
		{
			if (info) return (format("go berzerk every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int time = 150 + randint(50);
				msg_print("I wish to see or hear no more of Dr. Jekyll.");
				if (!p_ptr->fast) (void)set_fast(time);
				else (void)set_fast(p_ptr->fast + randint(5));
				if (!p_ptr->shero) (void)set_shero(time);
				else (void)set_shero(p_ptr->shero + randint(5));
				break;
			}
			break;
		}
		case ART_WHIP_QUATERMAIN:
		{
			if (info) return (format("map your surroundings every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Dear me!");
				map_area();
				break;
			}
			break;
		}
		
		case ART_QSTAFF_CROWLEY:
		{
			if (info) return (format("summon demons every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (one_in_(20))
				{
					msg_print("The demons are hostile!");
					for (i = 0; i < 3; i++)
					{	
						summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DEMON, FALSE);
					}
				}
				else
				{
					msg_print("You summon dark servants.");
					for (i = 0; i < 3; i++)
					{	
						summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DEMON, TRUE);
					}
				}
				break;
			}
			break;
		}
		/* case ART_PICK_BRUNEL: NO ACTIVATION */
		/* case ART_SCYTHE_CYRUS: NO ACTIVATION */
		/* case ART_HAMMER_HELSING: NO ACTIVATION */
		case ART_MACE_RAMSES:
		{
			if (info) return (format("berzerk and heal every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)heal_player(25, 40);
				(void)wp_player(rand_range(5, 10));
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 90 + randint(10));

				break;
			}
			break;
		}
		case ART_SCORPION_CLUB:
		{
			if (info) return (format("conjure a contagion cloud every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int k, ty, tx;
				
				/* Get a new effect index */
				k = effect_prep();
	
				/* Note failure XXX */
				if (k < 0) break;
	
				/* Get a direction */
				if (!get_hack_dir(&dir)) return FALSE;
	
				/* Use the given direction */
				ty = py + ddy[dir];
				tx = px + ddx[dir];
				
				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
				}
	
				/* We want an lingering cloud, */
				x_list[k].index = EFFECT_IRREGULAR_CLOUD;
	
				x_list[k].type = GF_CONTAGION;
	
				/* That starts at the monster location. */
				x_list[k].y0 = x_list[k].y1 = ty;
				x_list[k].x0 = x_list[k].x1 = tx;
	
				/* It attacks every 8 -> 5 game turns, */
				x_list[k].time_delay = 10;
	
				/* Does damage, has a large radius, */
				x_list[k].power = damroll(8, 10);
				x_list[k].power2 = 5;
	
				/* And lasts for about 10 attacks */
				x_list[k].lifespan = 10;
				break;
			}
			break;
		}
		case ART_ARNE_BROKEN_DAGGER:
		{
			if (info) return (format("turn stone to mud every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				wall_to_mud(dir);
				break;
			}
			break;
		}
		case ART_CANE_ARRONAX:
		{

			if (info) return (format("detect animals every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)detect_animals(FALSE);
				break;		
			}
		}
		/* ART_HATCHET_OF_CONSEIL - no activation */
		case ART_HARPOON_OF_NED:
		{

			if (info) return (format("satisfy hunger every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;		
			}
		}
		/* case ART_AXE_NELKO: NO ACTIVATION */
		/* case ART_AXE_AK: NO ACTIVATION */
		/* case ART_MALLET_QUEEN_HEART: NO ACTIVATION */
		case ART_SHOVEL_FRANK_CALVERT:
		{
			if (info) return (format("detect objects every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				detect_objects_normal(TRUE);
				break;
			}
		}
		case ART_SHOVEL_HEINRICH_SCHLIEMANN:
		{
			if (info) return (format("fetch objects every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_hack_dir(&dir)) return ("");
				fetch(dir, 1000, FALSE);
				break;
			}
		}
		case ART_PICK_GUY_FAWKES:
		{
			if (info) return (format("cause an explosion every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_FORCE, dir, damroll(12, 32), 4);
				break;
			}
		}
		case ART_SABRE_CLUBS:
		{
			if (info) return (format("shoot a bolt of acid every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_LIQUESCE, dir, damroll(6, 8));
				break;
			}
		}
		case ART_SABRE_SPADES:
		{
			if (info) return (format("shoot a bolt of ice every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_GLACIAL, dir, damroll(7, 8));
				break;
			}
		}
		case ART_SABRE_HEARTS:
		{
			if (info) return (format("shoot a bolt of fire every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_PLASMA, dir, damroll(9, 8));
				break;
			}
		}
		case ART_SABRE_DIAMONDS:
		{
			if (info) return (format("shoot a bolt of lightning every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_VOLT, dir, damroll(8, 8));
				break;
			}
		}
		case ART_SABRE_CUPS:	
		{
			if (info) return (format("shoot a bolt of poison every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_CONTAGION, dir, damroll(5, 8));
				break;
			}
		}
		/* case ART_CLUB_HERC: NO ACTIVATION */
		case ART_SPEAR_GEORGE:
        {
			if (info) return (format("become a patriot every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				int time1 = 25 + randint(25);
				int time2 = 20 + randint(20);
				int time3 = 12 + randint(30);
				if (p_ptr->prace == RACE_BRITISH) 
				{
					time1 *= 3;
					time2 *= 3;
					time3 *= 3;
					msg_print("For England!");
				}				
				if (!p_ptr->protevil) (void)set_protevil(time1);
				else (void)set_protevil(p_ptr->protevil + randint(5));
				if (!p_ptr->shero) (void)set_shero(time2);
				else (void)set_shero(p_ptr->shero + randint(5));
				if (!p_ptr->blessed) (void)set_blessed(time2);
				else (void)set_blessed(p_ptr->blessed + randint(5));
				if (!p_ptr->shield) (void)set_shield(time2);
				else (void)set_shield(p_ptr->shield + randint(5));
				if (!p_ptr->tim_vigor) (void)set_tim_vigor(time3);
				else (void)set_tim_vigor(p_ptr->tim_vigor + randint(5));
				if (!p_ptr->tim_muscle) (void)set_tim_muscle(time3);
				else (void)set_tim_muscle(p_ptr->tim_muscle + randint(5));
				break;
			}
		}
		/* case ART_RAPIER_BARSOOM: NO ACTIVATION  	*/		
		/* case ART_SABER_KADABRA: NO ACTIVATION 	*/
		/* case ART_BROAD_AQUILONIA: NO ACTIVATION 	*/
		/* case ART_HAMMER_SNAEFELLSJOKULL: NO ACTIVATION  */
		/* case ART_DIRK_RED_NAIL: NO ACTIVATION  */
		/* case ART_BASTARD_VALERIA: NO ACTIVATION */
		/* case ART_CUTLASS_KORAD: NO ACTIVATION */
		/* case ART_PNEUMATIC_TOONOL: NO ACTIVATION */
		case ART_GREAT_AXE_KULL:
		{
			if (info) return (format("rage every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("By this axe, I RULE!");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 100 + randint(50));
			}
			break;
		}	
		/* case ART_CARTER_PISTOL: NO ACTIVATION */
		/* case ART_PISTOL_QUATERMAIN: NO ACTIVATION */
		/* case ART_PISTOL_STYLE: NO ACTIVATION */
		/* case ART_SAWED_OFF_DOC_HOLLIDAY: NO ACTIVATION */
		/* case ART_PISTOL_NED_BUNTLINE: NO ACTIVATION */
		/* case ART_PISTOL_MAUSER: NO ACTIVATION */
		/* case ART_REVOLVER_WATSON: NO ACTIVATION */
		case ART_SHOTGUN_MARQUIS_TOUR:
		{

			if (info) return (format("teleport every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				teleport_player(20);
				break;		
			}
		}		
		default:
		{
			if (info) return ("This artifact cannot be activated");
			if (act) msg_print("This artifact cannot be activated");
			break;
		}
	}

	/* Activating the item */
	if (act)
	{
		/* Set timeout */
		o_ptr->timeout = rand_range(timeout1, timeout2);
	}


	/* Return */
	return ("");
}
