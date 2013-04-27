/* File: cmd6.c */

/* Purpose: Object commands */

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
 * This file includes code for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of wands, staffs,
 * and rods.  Note the overly paranoid warning about potential pack
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
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */

static void do_cmd_eat_food_aux(object_type *o_ptr)
{
	bool ident;

	/* Sound */
	sound(SOUND_EAT);

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Eat the food */
	(void)use_object(o_ptr, &ident, FALSE);
	
	if (!(object_aware_p(o_ptr)))
	{
		chg_virtue(V_PATIENCE, -1);
		chg_virtue(V_CHANCE, 1);
	}

	/* We have tried it */
	object_tried(o_ptr);

	/* The player is now aware of the object */
	if (ident && !object_aware_p(o_ptr))
	{
		/* Object level */
		int lev = get_object_level(o_ptr);

		object_aware(o_ptr);
		gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
	}
	
	/* Notice changes */
	notice_item();	

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Food can feed the player */
	if (p_ptr->rp.prace == RACE_VAMPIRE)
	{
		/* Reduced nutritional benefit */
		(void)set_food(p_ptr->food + (o_ptr->pval / 10));
		msgf
			("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)	/* Hungry */
			msgf("Your hunger can only be satisfied with fresh blood!");
	}
	else if (FLAG(p_ptr, TR_CANT_EAT))
	{
		if (p_ptr->rp.prace == RACE_SKELETON)
		{
			if (!((o_ptr->sval == SV_FOOD_WAYBREAD) ||
				  (o_ptr->sval < SV_FOOD_BISCUIT)))
			{
				object_type *q_ptr;
	
				msgf("The food falls through your jaws!");

				/* Create the item */
				q_ptr = object_prep(lookup_kind(o_ptr->tval, o_ptr->sval));

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, p_ptr->px, p_ptr->py);
			}
			else
			{
				msgf("The food falls through your jaws and vanishes!");
			}
		}
		else if ((p_ptr->rp.prace == RACE_GOLEM) ||
				 (p_ptr->rp.prace == RACE_ZOMBIE) ||
				 (p_ptr->rp.prace == RACE_SPECTRE) || (p_ptr->rp.prace == RACE_GHOUL))
		{
			msgf("The food of mortals is poor sustenance for you.");
			(void)set_food(p_ptr->food + ((o_ptr->pval) / 20));
		}
		else
		{
			msgf("This food is poor sustenance for you.");
			set_food(p_ptr->food + ((o_ptr->pval) / 20));
		}
	}
	else
	{
		(void)set_food(p_ptr->food + o_ptr->pval);
	}

	/* Destroy a food item */
	item_increase(o_ptr, -1);

	make_noise(1);
}


/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
	object_type *o_ptr;
	cptr q, s;


	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item */
	q = "Eat which item? ";
	s = "You have nothing to eat.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Eat the object */
	do_cmd_eat_food_aux(o_ptr);
}


/*
 * Quaff a potion (from the pack or the floor)
 */
static void do_cmd_quaff_potion_aux(object_type *o_ptr)
{
	bool ident;

	/* Sound */
	sound(SOUND_QUAFF);

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Quaff the potion */
	(void)use_object(o_ptr, &ident, FALSE);

	if (p_ptr->rp.prace == RACE_SKELETON)
	{
		msgf("Some of the fluid falls through your jaws!");
		(void)potion_smash_effect(0, p_ptr->px, p_ptr->py, o_ptr);
	}

	if (!(object_aware_p(o_ptr)))
	{
		chg_virtue(V_PATIENCE, -1);
		chg_virtue(V_CHANCE, 1);
	}

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		/* Object level */
		int lev = get_object_level(o_ptr);

		object_aware(o_ptr);
		gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
	}

	/* Notice changes */
	notice_item();

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Potions can feed the player */
	switch (p_ptr->rp.prace)
	{
		case RACE_VAMPIRE:
			(void)set_food(p_ptr->food + (o_ptr->pval / 10));
			break;
		case RACE_SKELETON:
			/* Do nothing */
			break;
		case RACE_GOLEM:
		case RACE_ZOMBIE:
		case RACE_SPECTRE:
		case RACE_GHOUL:
			(void)set_food(p_ptr->food + ((o_ptr->pval) / 20));
			break;
		default:
			(void)set_food(p_ptr->food + o_ptr->pval);
	}

	/* Reduce and describe items */
	item_increase(o_ptr, -1);

	make_noise(1);
}


void do_cmd_quaff_potion(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item */
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Quaff the potion */
	do_cmd_quaff_potion_aux(o_ptr);
}


/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
static void do_cmd_read_scroll_aux(object_type *o_ptr)
{
	object_type temp, *j_ptr;
	bool ident, used_up;

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Copy item into temp */
	COPY(&temp, o_ptr, object_type);

	/* Read the scroll */
	used_up = use_object(o_ptr, &ident, FALSE);
	
	/*
	 * Counter the side effect of use_object on an identify scroll
	 * by finding the original item
	 */
	OBJ_ITT_START (p_ptr->inventory, j_ptr)
	{
		/* retrieve the pointer of the original scroll */
		if (object_equal(&temp, j_ptr)) o_ptr = j_ptr;
	}
	OBJ_ITT_END;

	/* Hack - the scroll may already be destroyed by its effect */
	if (o_ptr->k_idx)
	{
		if (!(object_aware_p(o_ptr)))
		{
			chg_virtue(V_PATIENCE, -1);
			chg_virtue(V_CHANCE, 1);
		}

		/* The item was tried */
		object_tried(o_ptr);

		/* An identification was made */
		if (ident && !object_aware_p(o_ptr))
		{
			/* Object level */
			int lev = get_object_level(o_ptr);

			object_aware(o_ptr);
			gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
		}
		
		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Hack.  Do the sorting now */
		o_ptr = reorder_pack_watch(o_ptr);

		/* Hack.  Do the combining now */
		o_ptr = combine_pack_watch(o_ptr);

		/* Hack -- allow certain scrolls to be "preserved" */
		if (!used_up) return;

		sound(SOUND_SCROLL);
	
		/* Destroy a scroll */
		item_increase(o_ptr, -1);
	}
	
	make_noise(1);
}


void do_cmd_read_scroll(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Check some conditions */
	if (p_ptr->tim.blind)
	{
		msgf("You can't see anything.");
		return;
	}
	if (no_lite())
	{
		msgf("You have no light to read by.");
		return;
	}
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}


	/* Restrict choices to scrolls */
	item_tester_tval = TV_SCROLL;

	/* Get an item */
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Read the scroll */
	do_cmd_read_scroll_aux(o_ptr);
}


/*
 * Use a staff.
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
static void do_cmd_use_staff_aux(object_type *o_ptr)
{
	int chance, lev;
	bool ident, use_charge;

	/* Mega-Hack -- refuse to use a pile from the ground */
	if (floor_item(o_ptr) && (o_ptr->number > 1))
	{
		msgf("You must first pick up the staffs.");
		return;
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->tim.confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - lev / 2;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msgf("You failed to use the staff properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msgf("The staff has no charges left.");
		o_ptr->info |= (OB_EMPTY);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();

		return;
	}


	/* Sound */
	sound(SOUND_ZAP);

	/* Use the staff */
	use_charge = use_object(o_ptr, &ident, FALSE);
	
	/* Hack - the staff may destroy itself when activated on the ground */
	if (o_ptr->k_idx)
	{
		if (!(object_aware_p(o_ptr)))
		{
			chg_virtue(V_PATIENCE, -1);
			chg_virtue(V_CHANCE, 1);
		}

		/* Tried the item */
		object_tried(o_ptr);

		/* An identification was made */
		if (ident && !object_aware_p(o_ptr))
		{
			object_aware(o_ptr);
			gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
		}
		
		/* Notice changes */
		notice_item();

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);


		/* Hack -- some uses are "free" */
		if (!use_charge) return;

		/* XXX Hack -- unstack if necessary */
		if (o_ptr->number > 1)
		{
			/* Split object */
			o_ptr = item_split(o_ptr, 1);

			/* Use a single charge */
			o_ptr->pval--;

			/* Unstack the used item */
			o_ptr = inven_carry(o_ptr);

			/* Notice weight changes */
			p_ptr->update |= PU_WEIGHT;

			/* Paranoia */
			if (!o_ptr)
			{
				msgf("Too many dungeon objects - staff lost!");

				make_noise(1);

				/* Exit */
				return;
			}

			/* Message */
			msgf("You unstack your staff.");
		}
		else
		{
			/* Use a single charge */
			o_ptr->pval--;
		}

		/* Describe charges in the pack */
		if (o_ptr) item_charges(o_ptr);
	}

	make_noise(1);
}


void do_cmd_use_staff(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	do_cmd_use_staff_aux(o_ptr);
}


/*
 * Aim a wand (from the pack or floor).
 *
 * Use a single charge from a single item.
 * Handle "unstacking" in a logical manner.
 *
 * For simplicity, you cannot use a stack of items from the
 * ground.  This would require too much nasty code.
 *
 * There are no wands which can "destroy" themselves, in the inventory
 * or on the ground, so we can ignore this possibility.  Note that this
 * required giving "wand of wonder" the ability to ignore destruction
 * by electric balls.
 *
 * All wands can be "cancelled" at the "Direction?" prompt for free.
 *
 * Note that the basic "bolt" wands do slightly less damage than the
 * basic "bolt" rods, but the basic "ball" wands do the same damage
 * as the basic "ball" rods.
 */
static void do_cmd_aim_wand_aux(object_type *o_ptr)
{
	bool use_charge;
	int chance, dir, lev;
	bool ident;

	/* Mega-Hack -- refuse to use a pile from the ground */
	if (floor_item(o_ptr) && (o_ptr->number > 1))
	{
		msgf("You must first pick up the wands.");
		return;
	}

	/* Notice empty wandss */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msgf("The wand has no charges left.");
		o_ptr->info |= (OB_EMPTY);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		
		/* Notice changes */
		notice_item();

		return;
	}

	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;
	
	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Take a turn */
	p_ptr->state.energy_use = MIN(75, 200 - 5 * p_ptr->skills[SKILL_DEV] / 8);

	/* Get the object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->tim.confused) chance /= 2;

	/* Hight level objects are harder */
	chance = chance - lev / 2;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msgf("You failed to use the wand properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	/* Aim the wand */
	use_charge = use_object(o_ptr, &ident, dir);
	
	/* Hack - wands may destroy themselves if activated on the ground */
	if (o_ptr->k_idx)
	{
		/* Mark it as tried */
		object_tried(o_ptr);

		/* Apply identification */
		if (ident && !object_aware_p(o_ptr))
		{
			int lev = get_object_level(o_ptr);

			object_aware(o_ptr);
			gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
		}
		
		/* Notice changes */
		notice_item();

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Hack -- some uses are "free" */
		if (!use_charge) return;

		/* Use a single charge */
		o_ptr->pval--;
		o_ptr->ac++;

		/* Describe the charges */
		item_charges(o_ptr);
	}

	make_noise(1);
}


void do_cmd_aim_wand(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item */
	q = "Aim which wand? ";
	s = "You have no wand to aim.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Aim the wand */
	do_cmd_aim_wand_aux(o_ptr);
}


/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 *
 * pvals are defined for each rod in k_info. -LM-
 */
static void do_cmd_zap_rod_aux(object_type *o_ptr)
{
	int chance, dir, lev;
	bool ident;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Mega-Hack -- refuse to use a pile from the ground */
	if (floor_item(o_ptr) && (o_ptr->number > 1))
	{
		msgf("You must first pick up the rods.");
		return;
	}

	/* A single rod is still charging */
	if ((o_ptr->number == 1) && (o_ptr->timeout))
	{
		if (flush_failure) flush();
		msgf("The rod is still charging.");
		return;
	}
	/* A stack of rods lacks enough energy. */
	else if ((o_ptr->number > 1)
			 && (o_ptr->timeout > (o_ptr->number - 1) * k_ptr->pval))
	{
		if (flush_failure) flush();
		msgf("The rods are all still charging.");
		return;
	}

	/* Get a direction (unless KNOWN not to need it) */
	if (((o_ptr->sval >= SV_ROD_MIN_DIRECTION) && (o_ptr->sval != SV_ROD_HAVOC))
		|| !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}

	/* Take a turn */
	p_ptr->state.energy_use = MIN(75, 200 - 5 * p_ptr->skills[SKILL_DEV] / 8);

	/* Not identified yet */
	ident = FALSE;
	
	/* Is Identity known? */
	ident = object_aware_p(o_ptr);

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->tim.confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - lev / 2;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msgf("You failed to use the rod properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	/* Increase the timeout by the rod kind's pval. -LM- */
	o_ptr->timeout += k_ptr->pval;

	/* Zap the rod */
	use_charge = use_object(o_ptr, &ident, dir);

	if (!(object_aware_p(o_ptr)))
	{
		chg_virtue(V_PATIENCE, -1);
		chg_virtue(V_CHANCE, 1);
	}

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
	}
	
	/* Notice changes */
	notice_item();

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
		o_ptr->timeout -= k_ptr->pval;
		return;
	}

	make_noise(1);
}


void do_cmd_zap_rod(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item */
	q = "Zap which rod? ";
	s = "You have no rod to zap.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Zap the rod */
	do_cmd_zap_rod_aux(o_ptr);
}


/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	/* Check statues */
	if (o_ptr->tval == TV_STATUE) return (TRUE);

	/* Ignore dungeon objects */
	if (o_ptr->iy || o_ptr->ix) return (FALSE);

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Check activation flag */
	if (FLAG(o_ptr, TR_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/*
 * Hack -- activate the ring of power
 */
void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint1(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			msgf("You are surrounded by a malignant aura.");
			sound(SOUND_EVIL);

			/* Decrease all stats (permanently) */
			(void)dec_stat(A_STR, 50, TRUE);
			(void)dec_stat(A_INT, 50, TRUE);
			(void)dec_stat(A_WIS, 50, TRUE);
			(void)dec_stat(A_DEX, 50, TRUE);
			(void)dec_stat(A_CON, 50, TRUE);
			(void)dec_stat(A_CHR, 50, TRUE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->exp / 4);
			check_experience();

			break;
		}

		case 3:
		{
			/* Message */
			msgf("You are surrounded by a powerful aura.");

			/* Dispel monsters */
			(void)dispel_monsters(1000);
			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			(void)fire_ball(GF_MANA, dir, 300, 3);
			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			(void)fire_bolt(GF_MANA, dir, 250);
			break;
		}
	}
}


/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static void do_cmd_activate_aux(object_type *o_ptr)
{
	int lev, chance;

	/* Take a turn */
	p_ptr->state.energy_use = MIN(75, 200 - 5 * p_ptr->skills[SKILL_DEV] / 8);

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->tim.confused) chance /= 2;

	/* Cursed items are difficult to activate */
	if (cursed_p(o_ptr)) chance /= 3;

	/* High level objects are harder */
	chance = chance - lev / 2;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msgf("You failed to activate it properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msgf("It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	msgf(MSGT_ZAP, "You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

	/* Activate the object */
	apply_object_trigger(TRIGGER_USE, o_ptr, ""); 

	/* Notice changes */
	notice_item();

	make_noise(3);

	return;
}


void do_cmd_activate(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
	s = "You have nothing to activate.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Activate the item */
	do_cmd_activate_aux(o_ptr);
}
