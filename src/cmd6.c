/* File: cmd6.c */

/* Purpose: Object commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 *
 *
 * James E. Wilson and Robert A. Koeneke released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */ 


#include "angband.h"

extern void do_cmd_rerate(void);
static bool activate_random_artefact(object_type * o_ptr);


/*
* This file includes code for eating food, drinking potions,
* reading scrolls, aiming wands, using staffs, zapping rods,
* and activating artefacts.
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

/*
* Eat some food (from the pack or floor)
*/
void do_cmd_eat_food(int item)
{
	int			ident, lev, counter;

	object_type		*o_ptr;


	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item if we weren't passed one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Eat which item? ","You have nothing to eat.",  USE_INVEN | USE_FLOOR))
		{
			return;
		}
	}

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

	item_tester_tval = TV_FOOD;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't eat that!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Sound */
	sound(SOUND_EAT);

	/* Take a turn */
	energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Analyze the food */
	switch (o_ptr->sval)
	{
	case SV_FOOD_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				set_timed_effect( TIMED_POISONED , p_ptr->poisoned + rand_int(10) + 10);
				do_dec_stat(A_CON);
				do_dec_stat(A_STR);
				ident = TRUE;
			}
			break;
		}

	case SV_FOOD_THIRD_SIGHT:
		{
			if (!p_ptr->resist_blind)
			{
				counter = rand_int(200) + 200;
				if (set_timed_effect( TIMED_BLIND , p_ptr->blind   + counter )) ident = TRUE;
				if (set_timed_effect( TIMED_ESP   , p_ptr->tim_esp + counter )) ident = TRUE;
				if (!p_ptr->resist_conf)
				{
					if (set_timed_effect( TIMED_CONFUSED , p_ptr->confused + rand_int(5) + 5)) ident = TRUE;
				}
			}
			break;
		}

	case SV_FOOD_FRIGHT:
		{
			if (!p_ptr->resist_fear)
			{
				if (set_timed_effect( TIMED_AFRAID , p_ptr->afraid + rand_int(50) + 50)) ident = TRUE;
				if (set_timed_effect( TIMED_FAST , p_ptr->fast + rand_int(50) + 50)) ident = TRUE;
			}
			break;
		}

	case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_conf)
			{
				if (set_timed_effect( TIMED_CONFUSED , p_ptr->confused + rand_int(10) + 10)) ident = TRUE;
			}
			break;
		}

	case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->resist_chaos)
			{
				if (!p_ptr->resist_conf)
				{
					if (set_timed_effect( TIMED_CONFUSED , p_ptr->confused + rand_int(5) + 5)) ident = TRUE;
				}
				if (set_timed_effect( TIMED_IMAGE , p_ptr->image + rand_int(250) + 250)) ident = TRUE;
			}
			break;
		}

	case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->free_act)
			{
				if (set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + rand_int(10) + 10))ident = TRUE;
			}
			break;
		}

	case SV_FOOD_WEAKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food.");
			(void)do_dec_stat(A_STR);
			ident = TRUE;
			break;
		}

	case SV_FOOD_SICKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food.");
			(void)do_dec_stat(A_CON);
			ident = TRUE;
			break;
		}

	case SV_FOOD_STUPIDITY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_INT);
			ident = TRUE;
			break;
		}

	case SV_FOOD_NAIVETY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_WIS);
			ident = TRUE;
			break;
		}

	case SV_FOOD_UNHEALTH:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_CON);
			ident = TRUE;
			break;
		}

	case SV_FOOD_DISEASE:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_STR);
			ident = TRUE;
			break;
		}

	case SV_FOOD_CURE_POISON:
		{
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_OPPOSE_POIS , p_ptr->oppose_pois + 50 ) ) ident = TRUE;
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

	case SV_FOOD_CURE_BLINDNESS:
		{
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_OPPOSE_BLIND , p_ptr->oppose_blind + 50 ) ) ident = TRUE;
			break;
		}

	case SV_FOOD_CURE_PARANOIA:
		{
			if (set_timed_effect( TIMED_AFRAID , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_OPPOSE_FEAR , p_ptr->oppose_fear + 50 ) ) ident = TRUE;
			if (hp_player(damroll(4, 8))) ident = TRUE;
			break;
		}

	case SV_FOOD_CURE_CONFUSION:
		{
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_OPPOSE_CONF , p_ptr->oppose_conf + 50 ) ) ident = TRUE;
			if (hp_player(damroll(4, 8))) ident = TRUE;
			break;
		}

	case SV_FOOD_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			break;
		}

	case SV_FOOD_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

	case SV_FOOD_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

	case SV_FOOD_RESTORING:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHA)) ident = TRUE;
			break;
		}


	case SV_FOOD_RATION:
	case SV_FOOD_BISCUIT:
	case SV_FOOD_JERKY:
	case SV_FOOD_SLIME_MOLD:
		{
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}

	case SV_FOOD_AMBROSIA:
		{
			msg_print("That tastes divine.");
			(void)set_timed_effect( TIMED_POISONED , 0);
			(void)hp_player(damroll(8, 4));
			(void)do_res_stat(A_CHA);
			ident = TRUE;
			break;
		}

	case SV_FOOD_PINT_OF_ALE:
	case SV_FOOD_PINT_OF_WINE:
		{
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* We have tried it */
	object_tried(o_ptr);

	/* The player is now aware of the object */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Food can feed the player */
	if (p_ptr->prace == VAMPIRE) {
		/* Reduced nutritional benefit */
		(void)set_food(p_ptr->food + (o_ptr->pval / 10));
		msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
			msg_print("Your hunger can only be satisfied with fresh blood!");
	}
	else if (p_ptr->prace == SKELETON)
	{
		if (!((o_ptr->sval == SV_FOOD_RATION) || (o_ptr->sval < SV_FOOD_BISCUIT)))
		{
			object_type forge;
			object_type * q_ptr = & forge;

			msg_print("The food falls through your jaws!");

			/* Create the item */
			object_prep(q_ptr, lookup_kind(o_ptr->tval, o_ptr->sval));

			/* Drop the object from heaven */
			drop_near(q_ptr, -1, py, px);
		}
		else
		{
			msg_print("The food falls through your jaws and vanishes!");
		}
	}
	else if (!rp_ptr->rations)
	{
		msg_print("The food of mortals is poor sustenance for you.");
		set_food(p_ptr->food + ((o_ptr->pval) / 20));
	}

	else {
		(void)set_food(p_ptr->food + o_ptr->pval);
	}

	/* Destroy a food in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a food on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}


/*
* Quaff a potion (from the pack or the floor)
*/
void do_cmd_quaff_potion(int item)
{
	int		ident, lev;

	object_type	*o_ptr;
	
	char line[80];

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item if we weren't passed one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Quaff which potion? ", "You have no potions to quaff.", USE_EQUIP | USE_INVEN | USE_FLOOR))
		{
			return;
		}
	}

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

	item_tester_tval = TV_POTION;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a potion!");
		return;
	}
	item_tester_tval = 0;

	/* Sound */
	sound(SOUND_QUAFF);


	/* Take a turn */
	if (item < INVEN_POUCH_1)
	{
		energy_use = 100;
	} else {
		/* Potions in a pouch are immediately accessible */
		energy_use = 10;
	}

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
	case SV_POTION_WATER:
	case SV_POTION_APPLE_JUICE:
	case SV_POTION_SLIME_MOLD:
		{
			msg_print("You feel less thirsty.");
			ident = TRUE;
			break;
		}

	case SV_POTION_SLOWNESS:
		{
			if (set_timed_effect( TIMED_SLOW, p_ptr->slow + randint(25) + 15)) ident = TRUE;
			break;
		}

	case SV_POTION_SALT_WATER:
		{
			msg_print("The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_timed_effect( TIMED_POISONED , 0);
			(void)set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + 4);
			ident = TRUE;
			break;
		}

	case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_timed_effect( TIMED_POISONED , p_ptr->poisoned + rand_int(15) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_POTION_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (set_timed_effect( TIMED_BLIND , p_ptr->blind + rand_int(100) + 100))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_POTION_CONFUSION: /* Booze */
		{
			if (!((p_ptr->resist_conf) || (p_ptr->resist_chaos)))
			{
				if (set_timed_effect( TIMED_CONFUSED , p_ptr->confused + rand_int(20) + 15))
				{
					ident = TRUE;
				}
				if (randint(2)==1)
				{
					if (set_timed_effect( TIMED_IMAGE , p_ptr->image + rand_int(150) + 150))
					{
						ident = TRUE;
					}
				}
				if (randint(13)==1)
				{
					ident = TRUE;
					if(randint(3)==1) lose_all_info();
					else wiz_dark();
					teleport_player(100);
					wiz_dark();
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
				}
			}
			break;
		}

	case SV_POTION_SLEEP:
		{
			if (!p_ptr->free_act)
			{
				if (set_timed_effect( TIMED_PARALYZED , p_ptr->paralyzed + rand_int(4) + 4))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_POTION_LOSE_MEMORIES:
		{
			if (!p_ptr->hold_life && (p_ptr->exp > 0))
			{
				msg_print("You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;
		}

	case SV_POTION_RUINATION:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHA, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			ident = TRUE;
			break;
		}

	case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR)) ident = TRUE;
			break;
		}

	case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT)) ident = TRUE;
			break;
		}

	case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS)) ident = TRUE;
			break;
		}

	case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX)) ident = TRUE;
			break;
		}

	case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON)) ident = TRUE;
			break;
		}

	case SV_POTION_DEC_CHA:
		{
			if (do_dec_stat(A_CHA)) ident = TRUE;
			break;
		}

	case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_timed_effect( TIMED_STUN, p_ptr->stun + 75);
			(void)set_timed_effect( TIMED_CUT, p_ptr->cut + 5000);
			ident = TRUE;
			break;
		}

	case SV_POTION_IOCAINE:
		{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Iocaine");
			ident = TRUE;
			break;
		}

	case SV_POTION_INFRAVISION:
		{
			if (set_timed_effect( TIMED_TIM_INFRA, p_ptr->tim_infra + 100 + randint(100))) ident = TRUE;
			break;
		}

	case SV_POTION_DETECT_INVIS:
		{
			if (set_timed_effect( TIMED_TIM_INVIS, p_ptr->tim_invis + 12 + randint(12))) ident = TRUE;
			break;
		}

	case SV_POTION_SLOW_POISON:
		{
			if (set_timed_effect( TIMED_POISONED , p_ptr->poisoned / 2)) ident = TRUE;
			break;
		}

	case SV_POTION_CURE_POISON:
		{
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			break;
		}

	case SV_POTION_BOLDNESS:
		{
			if (set_timed_effect( TIMED_AFRAID , 0)) ident = TRUE;
			break;
		}

	case SV_POTION_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_timed_effect( TIMED_FAST, randint(25) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
			}
			break;
		}

	case SV_POTION_RESIST_HEAT:
		{
			if (set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(10) + 10)) ident = TRUE;
			break;
		}

	case SV_POTION_RESIST_COLD:
		{
			if (set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(10) + 10)) ident = TRUE;
			break;
		}

	case SV_POTION_HEROISM:
		{
			if (set_timed_effect( TIMED_AFRAID , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_HERO, p_ptr->hero + randint(25) + 25)) ident = TRUE;
			if (hp_player(10)) ident = TRUE;
			break;
		}

	case SV_POTION_BESERK_STRENGTH:
		{
			if (set_timed_effect( TIMED_AFRAID , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(25) + 25)) ident = TRUE;
			if (hp_player(30)) ident = TRUE;
			break;
		}

	case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, p_ptr->cut - 10)) ident = TRUE;
			break;
		}

	case SV_POTION_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 50)) ident = TRUE;
			break;
		}

	case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			break;
		}

	case SV_POTION_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			break;
		}

	case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			break;
		}

	case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
			restore_level();
			hp_player(5000);
			(void)set_timed_effect( TIMED_POISONED , 0);
			(void)set_timed_effect( TIMED_BLIND , 0);
			(void)set_timed_effect( TIMED_CONFUSED , 0);
			(void)set_timed_effect( TIMED_IMAGE , 0);
			(void)set_timed_effect( TIMED_STUN, 0);
			(void)set_timed_effect( TIMED_CUT, 0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHA);
			ident = TRUE;
			break;
		}

	case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
				ident = TRUE;
			}
			break;
		}

	case SV_POTION_RESTORE_EXP:
		{
			if (restore_level()) ident = TRUE;
			break;
		}

	case SV_POTION_RES_STR:
		{
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		}

	case SV_POTION_RES_INT:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			break;
		}

	case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) ident = TRUE;
			break;
		}

	case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) ident = TRUE;
			break;
		}

	case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		}

	case SV_POTION_RES_CHA:
		{
			if (do_res_stat(A_CHA)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_STR:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_INT:
		{
			if (do_inc_stat(A_INT)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_WIS:
		{
			if (do_inc_stat(A_WIS)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_DEX:
		{
			if (do_inc_stat(A_DEX)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_CON:
		{
			if (do_inc_stat(A_CON)) ident = TRUE;
			break;
		}

	case SV_POTION_INC_CHA:
		{
			if (do_inc_stat(A_CHA)) ident = TRUE;
			break;
		}

	case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			if (do_inc_stat(A_INT)) ident = TRUE;
			if (do_inc_stat(A_WIS)) ident = TRUE;
			if (do_inc_stat(A_DEX)) ident = TRUE;
			if (do_inc_stat(A_CON)) ident = TRUE;
			if (do_inc_stat(A_CHA)) ident = TRUE;
			break;
		}

	case SV_POTION_ENLIGHTENMENT:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			ident = TRUE;
			break;
		}

	case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			msg_print(NULL);
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			self_knowledge();
			ident = TRUE;
			break;
		}

	case SV_POTION_SELF_KNOWLEDGE:
		{
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
			self_knowledge();
			ident = TRUE;
			break;
		}

	case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				ident = TRUE;
			}
			break;
		}

	case SV_POTION_RESISTANCE:
		{
			(void)spell_basic_resistance(20);
			ident = TRUE;
			break;
		}

	case SV_POTION_CURING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_IMAGE , 0)) ident = TRUE;
			break;
		}

	case SV_POTION_INVULNERABILITY:
		{
			(void)set_timed_effect( TIMED_INVULN, p_ptr->invuln + randint(7) + 7);
			ident = TRUE;
			break;
		}

	case SV_POTION_NEW_LIFE:
		{
			do_cmd_rerate();
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
				msg_print("You are cured of all corruptions.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			ident = TRUE;
			break;
		}


	}

	if ((p_ptr->prace == SKELETON) && (randint(12)==1))
	{
		msg_print("Some of the fluid falls through your jaws!");
		potion_smash_effect(0, py, px, o_ptr, NULL);
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}
	
	/* Check if the alchemy formula is learnt */
	/* TODO race/class alchemy skill assignment */
	if (!((potion_alch[o_ptr->sval].known1) && (potion_alch[o_ptr->sval].known2)) &&
		(rand_int(100) <= /*p_ptr->skill[SK_ALC]*/ 60 - 30 ))
	{
		object_kind *k_ptr;
		
		int k;
		bool item_known1 = FALSE;
		bool item_known2 = FALSE;
		
		bool learn = FALSE;
		
		/* Check if the components are known */
		for (k = 1; k < MAX_K_IDX; k++)
		{
			k_ptr = &k_info[k];
			
			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval1)) 
				item_known1 = (k_ptr->tried || k_ptr->aware);
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[o_ptr->sval].sval2)) 
				item_known2 = (k_ptr->tried || k_ptr->aware);
		}
		
		/* 
			* Learn, if you are aware of the component potion, but
		 * you are not yet aware it is part of the potion.
		 */
		if ((!(potion_alch[o_ptr->sval].known1)) && item_known1) 
		{
			potion_alch[o_ptr->sval].known1 = TRUE;
			learn = TRUE;
		}
		else if ((!(potion_alch[o_ptr->sval].known2)) && item_known2) 
		{
			potion_alch[o_ptr->sval].known2 = TRUE;
			learn = TRUE;
		}
		
		/* Message, if something new was learned */
		if (learn)
		{
			msg_print("You have gained alchemical knowledge!");
			alchemy_describe(line, sizeof(line), o_ptr->sval);
			msg_print(line);
		}
	}
	
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Potions can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);


	/* Destroy a potion in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}


/*
* Curse the players armour
*/
bool curse_armour(void)
{
	object_type *o_ptr;

	char o_name[80];

	/* Curse the body armour */
	o_ptr = &inventory[INVEN_BODY];

	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);

	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Attempt a saving throw for artefacts */
	if (((o_ptr->art_name) || artefact_p(o_ptr) ) && (rand_int(100) < 50))
	{  /* Cool */
		msg_format("A %s tries to %s, but your unique %s resists the effects!",
			"terrible black aura", "surround your armour", o_name);
	/* Attempt another saving throw for players with resistance to disenchantment */
	}else if( p_ptr->resist_disen && (rand_int(100) < 50) )
	{   /* Cool */
		msg_format("A %s tries to %s %s, but your resistance to disenchantment staves off the effects!",
			"terrible black aura", "surround your ", o_name);		
	}
	else /* not artefact or failed save... */
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armour */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - rand_s16b(5) - rand_s16b(5);
		o_ptr->to_h = 0 - rand_s16b(5);
		o_ptr->to_d = 0 - rand_s16b(5);
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it, break it and trick squelch rules */
		o_ptr->ident |= ( IDENT_CURSED | IDENT_BROKEN | IDENT_STOREB );

		/* Consider some benefits ;] */
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_IM_COLD;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_FEAR;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_POIS;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_DARK;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_NETHER;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_DISEN;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_HOLD_LIFE;
		if( randint(3)!=3 ) o_ptr->art_flags3 |= TR3_IGNORE_ACID;
		if( randint(3)!=3 ) o_ptr->art_flags3 |= TR3_SLOW_DIGEST;
		/* This one is so awesome, only 1 in 3.. */
		if( randint(3)==3 ) o_ptr->art_flags3 |= TR3_WRAITH;
		/* These ones are so crap, only 1 in 5 */
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_HEAVY_CURSE;
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_DRAIN_EXP;
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_AGGRAVATE;
				
		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	return (TRUE);
}


/*
* Curse the players weapon
*/
bool curse_weapon(void)
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
	if ((artefact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 50))
	{	/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
			"terrible black aura", "surround your weapon", o_name);
	/* Attempt another saving throw for players with resistance to disenchantment */
	}else if( p_ptr->resist_disen && (rand_int(100) < 50) )
	{   /* Cool */
		msg_format("A %s tries to %s %s, but your resistance to disenchantment staves off the effects!",
			"terrible black aura", "surround your ", o_name);		
	}
	/* not artefact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - rand_s16b(5) - rand_s16b(5);
		o_ptr->to_d = 0 - rand_s16b(5) - rand_s16b(5);
		o_ptr->to_a = 0 - rand_s16b(5);
		o_ptr->ac = 0;
		o_ptr->dd = o_ptr->dd /2;
		o_ptr->ds = o_ptr->ds /2;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it, break it and trick squelch rules */
		o_ptr->ident |= ( IDENT_CURSED | IDENT_BROKEN | IDENT_STOREB );

		/* Consider some benefits ;] */
		if( randint(3)!=3 ) o_ptr->art_flags1 |= TR1_BRAND_POIS;
		if( randint(3)!=3 ) o_ptr->art_flags1 |= TR1_BRAND_COLD;
		if( randint(3)!=3 ) o_ptr->art_flags1 |= TR1_VORPAL;
		if( randint(3)!=3 ) o_ptr->art_flags1 |= TR1_VAMPIRIC;

		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_IM_COLD;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_FEAR;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_POIS;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_DARK;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_NETHER;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_RES_DISEN;
		if( randint(3)!=3 ) o_ptr->art_flags2 |= TR2_HOLD_LIFE;
		if( randint(3)!=3 ) o_ptr->art_flags3 |= TR3_IGNORE_ACID;
		if( randint(3)!=3 ) o_ptr->art_flags3 |= TR3_SLOW_DIGEST;
		/* This one is so awesome, only 1 in 3.. */
		if( randint(3)==3 ) o_ptr->art_flags3 |= TR3_WRAITH;
		if( randint(3)==3 ) o_ptr->art_flags3 |= TR3_XP;
		/* These ones are so crap, only 1 in 5 */
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_HEAVY_CURSE;
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_DRAIN_EXP;
		if( randint(5)==1 ) o_ptr->art_flags3 |= TR3_AGGRAVATE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
	}

	/* Notice */
	return (TRUE);
}


/*
* Read a scroll (from the pack or floor).
*
* Certain scrolls can be "aborted" without losing the scroll.  These
* include scrolls with no effects but recharge or identify, which are
* cancelled before use.  XXX Reading them still takes a turn, though.
*/
void do_cmd_read_scroll(int item)
{
	int k, used_up, ident, lev;

	object_type *o_ptr;

	char  Rumor[80] ;

	/* Check some conditions */
	if (p_ptr->blind)
	{
		msg_print("You can't see anything.");
		return;
	}
	if (no_lite())
	{
		msg_print("You have no light to read by.");
		return;
	}
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Restrict choices to scrolls */
	item_tester_tval = TV_SCROLL;

	/* Get an item if we weren't passed one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Read which scroll? ", "You have no scrolls to read.", USE_EQUIP | USE_FLOOR | USE_INVEN))
		{
			return;
		}
	}

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

	item_tester_tval = TV_SCROLL;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a scroll!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Take a turn */
	if (item < INVEN_POUCH_1)
	{
		energy_use = 100;
	} else {
		/* Scrolls in a pouch are immediately accessible */
		energy_use = 10;
	}

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
	case SV_SCROLL_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				(void)set_timed_effect( TIMED_BLIND , p_ptr->blind + 3 + randint(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

	case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(1);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armour()) ident = TRUE;
			break;
		}

	case SV_SCROLL_CURSE_WEAPON:
		{
			if (curse_weapon()) ident = TRUE;
			break;
		}

	case SV_SCROLL_SUMMON_MONSTER:
		{
			for (k = 0; k < randint(3); k++)
			{
				/* The first shot of summoning has a 1 in 3 chance to summon a unique */
				/* But only we use it knowingly */
				if(k == 0 && randint(3) == 1 && object_aware_p(o_ptr))
				{
					if (summon_specific(py, px, dun_level, FILTER_UNIQUE))
					{
						ident = TRUE;
					}
				}
				else if (summon_specific(py, px, dun_level, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_SCROLL_SUMMON_UNDEAD:
		{
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(py, px, dun_level, FILTER_UNDEAD))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation()) ident = TRUE;
			break;
		}

	case SV_SCROLL_PHASE_DOOR:
		{
			teleport_player(10);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_TELEPORT:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			ident = TRUE;
			break;
		}

	case SV_SCROLL_WORD_OF_RECALL:
		{
			do_recall(NULL);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			break;
		}

	case SV_SCROLL_STAR_IDENTIFY:
		{
			ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			break;
		}

	case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				ident = TRUE;
			}
			break;
		}

	case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_all_curse();
			ident = TRUE;
			break;
		}

	case SV_SCROLL_ENCHANT_ARMOR:
		{
			ident = TRUE;
			if (!enchant_spell(0, 0, 1)) used_up = FALSE;
			break;
		}

	case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (!enchant_spell(1, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

	case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (!enchant_spell(0, 1, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

	case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (!enchant_spell(0, 0, 15)) used_up = FALSE;
			ident = TRUE;
			break;
		}

	case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(5, 5, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

	case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60)) used_up = FALSE;
			ident = TRUE;
			break;
		}

	case SV_SCROLL_LIGHT:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

	case SV_SCROLL_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

	case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

	case SV_SCROLL_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

	case SV_SCROLL_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

	case SV_SCROLL_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

	case SV_SCROLL_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			break;
		}

	case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
			break;
		}

	case SV_SCROLL_BLESSING:
		{
			if (set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(12) + 6)) ident = TRUE;
			break;
		}

	case SV_SCROLL_HOLY_CHANT:
		{
			if (set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(24) + 12)) ident = TRUE;
			break;
		}

	case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(48) + 24)) ident = TRUE;
			break;
		}

	case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				ident = TRUE;
			}
			break;
		}

	case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + k)) ident = TRUE;
			break;
		}

	case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			ident = TRUE;
			break;
		}

	case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

	case SV_SCROLL_STAR_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) ident = TRUE;
			break;
		}

	case SV_SCROLL_GENOCIDE:
		{
			(void)genocide(TRUE);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_MASS_GENOCIDE:
		{
			(void)mass_genocide(TRUE);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(py, px, 1, TRUE);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(py, px, randint(2) + 1, TRUE);
			ident = TRUE;
			break;
		}

	case SV_SCROLL_FIRE:
		{
			fire_ball(GF_FIRE, 0,
				150, 4); /* Note: "Double" damage since it is centered on
						 the player ... */
			if (!(p_ptr->oppose_fire || p_ptr->resist_fire || p_ptr->immune_fire))
				take_hit(50+randint(50), "a Scroll of Fire");
			ident = TRUE;
			break;
		}


	case SV_SCROLL_ICE:
		{
			fire_ball(GF_ICE, 0, 175, 4);
			if (!(p_ptr->oppose_cold || p_ptr->resist_cold || p_ptr->immune_cold))
				take_hit(100+randint(100), "a Scroll of Ice");
			ident = TRUE;
			break;
		}

	case SV_SCROLL_CHAOS:
		{
			fire_ball(GF_CHAOS, 0,
				222, 4);
			if (!p_ptr->resist_chaos)
				take_hit(111+randint(111), "a Scroll of Primal Chaos");
			ident = TRUE;
			break;
		}

	case SV_SCROLL_RUMOR:
		{
			msg_print("There are words of wisdom on the scroll. It reads:");
			msg_print(NULL);
			get_rnd_line("rumors.txt", Rumor);
			msg_format("%s", Rumor);
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			ident = TRUE;
			break;
		}

	case SV_SCROLL_ARTIFACT:
		{
			(void) artefact_scroll();
			ident = TRUE;
			break;
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up) return;


	/* Destroy a scroll in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a scroll on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}







/*
* Use a staff.			-RAK-
*
* One charge of one staff disappears.
*
* Hack -- staffs of identify can be "cancelled".
*/
void do_cmd_use_staff(int item)
{
	int			ident, chance, k, lev;

	object_type		*o_ptr;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item if we weren't already passed one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Use which staff? ", "You have no staff to use.", USE_INVEN | USE_FLOOR))
		{
			return;
		}
	}

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

	item_tester_tval = TV_STAFF;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a staff!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the staffs.");
		return;
	}


	/* Take a turn */
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the staff properly.");
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The staff has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
	case SV_STAFF_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				if (set_timed_effect( TIMED_BLIND , p_ptr->blind + 3 + randint(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

	case SV_STAFF_SLOWNESS:
		{
			if (set_timed_effect( TIMED_SLOW, p_ptr->slow + randint(30) + 15)) ident = TRUE;
			break;
		}

	case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) ident = TRUE;
			break;
		}

	case SV_STAFF_SUMMONING:
		{
			for (k = 0; k < randint(4); k++)
			{
				/* The first shot of summoning has a 1 in 3 chance to summon a unique */
				/* But only we use it knowingly */
				if(k == 0 && randint(3) == 1 && object_aware_p(o_ptr))
				{
					if (summon_specific(py, px, dun_level, FILTER_UNIQUE))
					{
						ident = TRUE;
					}
				} 
				else if (summon_specific(py, px, dun_level, 0))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_STAFF_TELEPORTATION:
		{
			teleport_player(100);
			ident = TRUE;
			break;
		}

	case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			ident = TRUE;
			break;
		}

	case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->blind)
				{
					msg_print("The staff glows blue for a moment...");
				}
				ident = TRUE;
			}
			break;
		}

	case SV_STAFF_STARLITE:
		{
			if (!p_ptr->blind)
			{
				msg_print("The end of the staff glows brightly...");
			}
			for (k = 0; k < 8; k++) lite_line(ddd[k]);
			ident = TRUE;
			break;
		}

	case SV_STAFF_LITE:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

	case SV_STAFF_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_GOLD:
		{
			if (detect_treasure()) ident = TRUE;
			if (detect_objects_gold()) ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_ITEM:
		{
			if (detect_objects_normal()) ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_INVIS:
		{
			if (detect_monsters_invis()) ident = TRUE;
			/* Konijn upgraded the staff and included the potion effect */
			if (set_timed_effect( TIMED_TIM_INVIS, p_ptr->tim_invis + 12 + randint(12))) ident = TRUE;
			break;
		}

	case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil()) ident = TRUE;
			break;
		}

	case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(randint(10)+25)) ident = TRUE;
			break;
		}

	case SV_STAFF_CURING:
		{
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_IMAGE , 0)) ident = TRUE;
			if (hp_player(300)) ident = TRUE;
			break;
		}

	case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			break;
		}

	case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				ident = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
			break;
		}

	case SV_STAFF_SLEEP_MONSTERS:
		{
			if (sleep_monsters()) ident = TRUE;
			break;
		}

	case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters()) ident = TRUE;
			break;
		}

	case SV_STAFF_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_timed_effect( TIMED_FAST, randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
			}
			break;
		}

	case SV_STAFF_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

	case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) ident = TRUE;
			break;
		}

	case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) ident = TRUE;
			break;
		}

	case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) ident = TRUE;
			k = 3 * p_ptr->lev;
			if (set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + k)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_AFRAID , 0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			break;
		}

	case SV_STAFF_GENOCIDE:
		{
			(void)genocide(TRUE);
			ident = TRUE;
			break;
		}

	case SV_STAFF_EARTHQUAKES:
		{
			earthquake(py, px, 10);
			ident = TRUE;
			break;
		}

	case SV_STAFF_DESTRUCTION:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, 5, 300, 5);
			break;
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		o_ptr->ident |= (IDENT_SENSE);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Hack -- some uses are "free" */
	if (!use_charge) return;


	/* Use a single charge */
	o_ptr->pval--;

	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		total_weight -= q_ptr->weight;
		item = inven_carry(q_ptr, FALSE);

		/* Message */
		msg_print("You unstack your staff.");
	}

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


/*
* Aim a wand (from the pack, pouch or floor).
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
void do_cmd_aim_wand(int item)
{
	int			lev, ident, chance, dir, sval;

	object_type		*o_ptr;


	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item if we weren't passed one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Aim which wand? ", "You have no wand to aim.", USE_EQUIP | USE_INVEN | USE_FLOOR))
		{
			return;
		}
	}

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

	item_tester_tval = TV_WAND;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a wand!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}

	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;

	/* Take a turn */
	if (item < INVEN_POUCH_1)
	{
		energy_use = 100;
	} else {
		/* Wands in a pouch are immediately accessible */
		energy_use = 10;
	}

	/* Not identified yet */
	ident = FALSE;

	/* Get the level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the wand properly.");
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_print("The wand has no charges left.");
		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	/* XXX Hack -- Extract the "sval" effect */
	sval = o_ptr->sval;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

	/* Analyze the wand */
	switch (sval)
	{
	case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_TRAP_DOOR_DEST:
		{
			if (destroy_door(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			break;
		}

	case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, 10)) ident = TRUE;
			break;
		}

	case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, 10)) ident = TRUE;
			break;
		}

	case SV_WAND_DRAIN_LIFE:
		{
			if (drain_life(dir, 75)) ident = TRUE;
			break;
		}

	case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

	case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12, 2);
			ident = TRUE;
			break;
		}

	case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6));
			ident = TRUE;
			break;
		}

	case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

	case SV_WAND_CHARM_MONSTER:
		{
			if (charm_monster(dir, 45))
				ident = TRUE;
			break;
		}

	case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(6, 8));
			ident = TRUE;
			break;
		}

	case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

	case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60, 2);
			ident = TRUE;
			break;
		}

	case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32, 2);
			ident = TRUE;
			break;
		}

	case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72, 2);
			ident = TRUE;
			break;
		}

	case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48, 2);
			ident = TRUE;
			break;
		}

	case SV_WAND_WONDER:
		{
			msg_print("Oops.  Wand of wonder activated.");
			break;
		}

	case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 100, 3);
			ident = TRUE;
			break;
		}

	case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 80, 3);
			ident = TRUE;
			break;
		}

	case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
			case 1 : { fire_ball(GF_ACID, dir, 100, -3); break;	}
			case 2 : { fire_ball(GF_ELEC, dir, 80, -3);  break;	}
			case 3 : { fire_ball(GF_FIRE, dir, 100, -3); break;	}
			case 4 : { fire_ball(GF_COLD, dir, 80, -3);	 break;	}
			default: { fire_ball(GF_POIS, dir, 60, -3);  break;	}
			}
			ident = TRUE;
			break;
		}
	case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 125)) ident = TRUE;
			break;
		}
	case SV_WAND_SHARD:
		{
			fire_ball(GF_SHARD, dir, 75 + (randint(50)), 2);
			ident = TRUE;
			break;
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		o_ptr->ident |= (IDENT_SENSE);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

    /* Stacking of wands : amount of wands is in number, amount of charges is in pval */
	/* Use a single charge */
	o_ptr->pval--;

	/* Screw the unstacking */
	/* Hack -- unstack if necessary */

	/* Describe the charges in the pack */
	if (item >= 0)
	{
		inven_item_charges(item);
	}

	/* Describe the charges on the floor */
	else
	{
		floor_item_charges(0 - item);
	}
}





/*
* Activate (zap) a Rod
*
* Unstack fully charged rods as needed.
*
* Hack -- rods of perception/genocide can be "cancelled"
* All rods can be cancelled at the "Direction?" prompt
*/
void do_cmd_zap_rod(int item)
{
	int	ident, chance, dir, lev;

	object_type		*o_ptr;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item if we do not already have one */
	if(item == -999)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Zap which rod? ", "You have no rod to zap.", USE_INVEN | USE_FLOOR))
		{
			return;
		}
	}

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

	item_tester_tval = TV_ROD;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("That is not a rod!");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the rods.");
		return;
	}

	/* TODO: This is a pretty major und crappy hack.. Hack^3 */
	/* Get a direction (unless KNOWN not to need it) */
	if (((o_ptr->sval >= SV_ROD_MIN_DIRECTION) && !(o_ptr->sval == SV_ROD_HAVOC))
		|| !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}


	/* Take a turn */
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the rod properly.");
		return;
	}

	/* Still charging */
	if (o_ptr->pval)
	{
		if (flush_failure) flush();
		msg_print("The rod is still charging.");
		return;
	}


	/* Sound */
	sound(SOUND_ZAP);


	/* Analyze the rod */
	switch (o_ptr->sval)
	{
	case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			o_ptr->pval = (10 + (rand_s16b(10)));
			break;
		}

	case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			o_ptr->pval = 70;
			break;
		}

	case SV_ROD_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) use_charge = FALSE;
			o_ptr->pval = 10;
			break;
		}

	case SV_ROD_RECALL:
		{
			do_recall( NULL );
			ident = TRUE;
			o_ptr->pval = 60;
			break;
		}

	case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			o_ptr->pval = 10 + (rand_s16b(11));
			break;
		}

	case SV_ROD_MAPPING:
		{
			map_area();
			ident = TRUE;
			o_ptr->pval = 99;
			break;
		}

	case SV_ROD_DETECTION:
		{
			detect_all();
			ident = TRUE;
			o_ptr->pval = 99;
			break;
		}

	case SV_ROD_PROBING:
		{
			probing();
			ident = TRUE;
			o_ptr->pval = 50;
			break;
		}

	case SV_ROD_CURING:
		{
			if (set_timed_effect( TIMED_BLIND , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_POISONED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CONFUSED , 0)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_IMAGE , 0)) ident = TRUE;
			/* I always thought Curing should not suck.. */
			if (hp_player(500)) ident = TRUE;
			o_ptr->pval = 999;
			break;
		}

	case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_timed_effect( TIMED_STUN, 0)) ident = TRUE;
			if (set_timed_effect( TIMED_CUT, 0)) ident = TRUE;
			o_ptr->pval = 999;
			break;
		}

	case SV_ROD_RESTORATION:
		{
			if (restore_level()) ident = TRUE;
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHA)) ident = TRUE;
			o_ptr->pval = 999;
			break;
		}

	case SV_ROD_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_timed_effect( TIMED_FAST, randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
			}
			o_ptr->pval = 99;
			break;
		}

	case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

	case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			o_ptr->pval = 15 + (rand_s16b(15));
			break;
		}

	case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			o_ptr->pval = 9;
			break;
		}

	case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			o_ptr->pval = 18;
			break;
		}

	case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			o_ptr->pval = 20;
			break;
		}

	case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 75)) ident = TRUE;
			o_ptr->pval = 23;
			break;
		}

	case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

	case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(6, 8));
			ident = TRUE;
			o_ptr->pval = 12;
			break;
		}

	case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3, 8));
			ident = TRUE;
			o_ptr->pval = 11;
			break;
		}

	case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(8, 8));
			ident = TRUE;
			o_ptr->pval = 15;
			break;
		}

	case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(5, 8));
			ident = TRUE;
			o_ptr->pval = 13;
			break;
		}

	case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60, 2);
			ident = TRUE;
			o_ptr->pval = 27;
			break;
		}

	case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32, 2);
			ident = TRUE;
			o_ptr->pval = 23;
			break;
		}

	case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72, 2);
			ident = TRUE;
			o_ptr->pval = 30;
			break;
		}

	case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48, 2);
			ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

	case SV_ROD_HAVOC:
		{
			call_chaos();
			ident = TRUE;
			o_ptr->pval = 250;
			break;
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
		o_ptr->pval = 0;
		return;
	}


	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain a local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity */
		q_ptr->number = 1;

		/* Restore "charge" */
		o_ptr->pval = 0;

		/* Unstack the used item */
		o_ptr->number--;
		total_weight -= q_ptr->weight;
		item = inven_carry(q_ptr, FALSE);

		/* Message */
		msg_print("You unstack your rod.");
	}
}




/*
* Hook to determine if an object is activatable
*/
static bool item_tester_hook_activate(object_type *o_ptr)
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
* Hack -- activate the ring of power
*/
static void ring_of_power()
{
	/* Pick a random effect */
	/* Effects are 'alter reality , mana ball and *destruction* */
	switch (randint(3))
	{
	case 1:
	    {
			msg_print("You modify the Inferno.");
			if (autosave_l)
			{
				is_autosave = TRUE;
				msg_print("Autosaving the game...");
				do_cmd_save_game();
				is_autosave = FALSE;
			}
			new_level_flag = TRUE;			
			break;
    	}
	case 2:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, 5, 300, 5);
			break;
		}

	case 3:
		{
			/* *Destroy* */
			destroy_area(py, px, 15, TRUE);		
			break;
		}
	}
}


/*
* Enchant some bolts
*/
static bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artefacts and ego-items */
		if (o_ptr->art_name || artefact_p(o_ptr) || ego_item_p(o_ptr))
			continue;

		/* Skip cursed/broken items */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) continue;

		/* Randomize */
		if (rand_int(100) < 75) continue;

		/* Message */
		msg_print("Your bolts are covered in a fiery aura!");

		/* Ego-item */
		o_ptr->name2 = EGO_FLAME;

		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Notice */
		return (TRUE);
	}

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msg_print("The fiery enchantment failed.");

	/* Notice */
	return (TRUE);
}


/*
* Activate a wielded object.  Wielded objects never stack.
* And even if they did, activatable objects never stack.
*
* Currently, only (some) artefacts, and Dragon Scale Mail, can be activated.
* But one could, for example, easily make an activatable "Ring of Plasma".
*
* Note that it always takes a turn to activate an artefact, even if
* the user hits "escape" at the "direction" prompt.
*/
void do_cmd_activate(int item)
{
	int         i, k, dir, lev, chance;
	int effects_timeout; /* Used for having effects in concert */

	object_type *o_ptr;


	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item if we weren't passed one from the inventory*/
	if(item == -999)
	{
		/* Get an item (from equip) */
		if (!get_item(&item, "Activate which item? ", "You have nothing to activate.", USE_EQUIP))
		{
			return;
		}
	}

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

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;
	/* Verify that the item can be used */
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't activate that!");
		item_tester_hook = 0;
		return;
	}
	/* Clear the hook */
	item_tester_hook = 0;

	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artefact level instead */
	if (artefact_p(o_ptr)) lev = a_info[o_ptr->name1].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msg_print("It whines, glows and fades...");
		return;
	}


	/* Activate the artefact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);


	if (o_ptr->art_name)
	{
		(void) activate_random_artefact(o_ptr);
		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}


	/* Artifacts */
	else if (o_ptr->name1)
	{
		/* Choose effect */
		switch (o_ptr->name1)
		{
		case ART_BEATRICE:
			{
				msg_print("The phial wells with clear light...");
				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = rand_s16b(10) + 10;
				break;
			}
        /*The gems of Eos are set in the shape of the cold animal which stings people...*/
		case ART_EOS:
			{
				msg_print("The gem glitters brightly...");
				map_area();
				lite_area(damroll(2, 15), 3);
				msg_print("The gem stings you...");
				take_hit(damroll(3,8), "the Gem of Eos'");				
				o_ptr->timeout = rand_s16b(20) + 20;
				break;
			}

		case ART_HIPPO:
			{
				msg_print("The gem flashes bright white!");
				wiz_lite();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				msg_print("The gem restores your vitality...");				
                /*
				 Removed recall for thematic reasons
				 Added cure blindness,confusion,stun , cure poison, cure cuts
				*/
				set_timed_effect( TIMED_BLIND , 0);
				set_timed_effect( TIMED_CONFUSED , 0);
				set_timed_effect( TIMED_POISONED , 0);
				set_timed_effect( TIMED_STUN, 0);
				set_timed_effect( TIMED_CUT, 0);
				o_ptr->timeout = rand_s16b(50) + 50;
				break;
			}

		case ART_DOOM:
		    {
				msg_print( "You understand perfectly the essence of fire..." );
				fire_ball( GF_FIRE , 5 , 250 , 5 );
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(50) + 450);
				o_ptr->timeout = rand_s16b(50) + 450;
				break;
			}			
			

		case ART_AMULET_MICHAEL : case ART_AMULET_RAPHAEL:
			{
				msg_print("The amulet emanates a stunning light");
				k = 3 * p_ptr->lev;
				(void)set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + k);
				dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case ART_RING_GEORGE:
			{

				msg_print("The ring calls out the Dragon...");
				effects_timeout = randint(25) + 100; 
				(void)set_timed_effect( TIMED_SHIELD, p_ptr->shield + effects_timeout);
				(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + effects_timeout);
				(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + effects_timeout);
				o_ptr->timeout = rand_s16b(100) + 100;
				break;
			}


		case ART_RING_GABRIEL:
			{
				msg_print("The ring glows brightly...");
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 125);
				o_ptr->timeout = rand_s16b(150) + 150;
				break;
			}

		case ART_RING_RAPHAEL:
			{
				msg_print("The ring glows deep red...");
				fire_ball(GF_FIRE, 5, 250, 5);
				(void)hp_player(700);
				(void)set_timed_effect( TIMED_PROTEVIL, randint(25) + 100);
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(50) + 250);				
				o_ptr->timeout = rand_s16b(700) + 250;				
				break;
			}

		case ART_RING_MICHAEL:
			{
				msg_print("The ring glows bright white...");
				fire_ball(GF_COLD, 5 , 250, 5);
				effects_timeout = randint(25) + 100; 
                set_timed_effect( TIMED_FAST, effects_timeout);
				set_timed_effect( TIMED_HERO, effects_timeout);
				set_timed_effect( TIMED_BLESSED, effects_timeout);
				set_timed_effect( TIMED_SHERO, effects_timeout);
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(50) + 250);				
				o_ptr->timeout = rand_s16b(250) + 250;
				break;
			}

		case ART_EMMANUEL:
			{
				msg_print("The ring glows deep blue...");
				fire_ball(GF_ELEC, 5, 250, 5);
				(void)hp_player(700);
				(void)set_timed_effect( TIMED_CUT, 0);				
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(50) + 250);
				o_ptr->timeout = rand_s16b(700) + 250;
				break;
			}

		case ART_FIRST:
			{
				msg_print("The ring glows intensely black...");
				ring_of_power();
				/* Not a bug, not 'broken'*/
				o_ptr->timeout = 10;
				break;
			}

		case ART_BAPHOMET:
			{
				msg_print("Your armour is surrounded by lightning...");
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				o_ptr->timeout = 1000;
				break;
			}
			
		case ART_ASMODAI:
		{
			msg_print("Your armour is surrounded by lightning...");
			for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
			o_ptr->timeout = 1000;
			break;
		}			

		case ART_SAMAEL:
			{
				fire_ball(GF_MISSILE, 5, 300, -4);
				msg_print("Your armour glows many colours...");
				(void)set_timed_effect( TIMED_AFRAID , 0);
				(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(50) + 50);
				(void)hp_player(30);
				(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(50) + 50);
				(void)spell_basic_resistance(50);
				o_ptr->timeout = 400;
				break;
			}
			
		case ART_ABADDON:
		{
			fire_ball(GF_MISSILE, 5, 300, -4);
			msg_print("Your armour glows many colours...");
			(void)set_timed_effect( TIMED_AFRAID , 0);
			(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(50) + 50);
			(void)hp_player(30);
			(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(50) + 50);
			(void)spell_basic_resistance(50);
			o_ptr->timeout = 400;
			break;
		}
			
		case ART_BLOOD_MICHAEL:
		{
			fire_ball(GF_MISSILE, 5, 300, -4);
			msg_print("Your armour glows many colours...");
			(void)set_timed_effect( TIMED_AFRAID , 0);
			(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(50) + 50);
			(void)hp_player(30);
			(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(50) + 50);
			(void)spell_basic_resistance(50);
			o_ptr->timeout = 400;
			break;
		}			

		case ART_CORSON:
			{
				msg_print("Your armour calls the power of the earth...");
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_timed_effect( TIMED_CUT, 0);
				o_ptr->timeout = 888;
				break;
			}
			
		case ART_AMAYMON:
		{
			msg_print("You breathe poisonous nether");
            if (!get_aim_dir(&dir)) return;
			fire_ball(GF_NETHER  , dir, 200, 5);			
			fire_ball(GF_POIS  , dir, 500, 5);
			o_ptr->timeout = 888;
			break;
		}			

		case ART_ZIMINAR:
		{
			msg_print("You feel inclined to write an email to konijn@gmail.com to give feedback");
			o_ptr->timeout = 50;
			break;
		}				
			
		case ART_GAEP:
		{   
			/*TODO: You buckle down and run to the <dir>*/
			(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 125);
			o_ptr->timeout = 50;
			break;
		}							
			
		case ART_ROBE_MICHAEL:
			{
				msg_print("A heavenly choir sings...");
				(void)set_timed_effect( TIMED_POISONED , 0);
				(void)set_timed_effect( TIMED_CUT, 0);
				(void)set_timed_effect( TIMED_STUN, 0);
				(void)set_timed_effect( TIMED_CONFUSED , 0);
				(void)set_timed_effect( TIMED_BLIND , 0);
				(void)set_timed_effect( TIMED_HERO, p_ptr->hero + randint(25) + 25);
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}
		case ART_VEPAR:
		{
			msg_print("Your armour crackles with lighning...");
			for (i = 0; i < 4; i++) 
			{
				fire_ball(GF_ELEC, 5, 150, 5);
				fire_ball(GF_WATER, 5, 150, 5);
			}
			o_ptr->timeout = 500;
			break;
		}
			
/*
		case ART_ORCS:
			{
				msg_print("Your armour glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}
*/			

		case ART_PRAVUIL:
			{
				msg_print("You feel the Word elevate you...");
				inc_stat(A_INT);
				inc_stat(A_WIS);
				o_ptr->timeout = 3333;
				break;
			}

		case ART_LAMMASU: case ART_MASK:

			{
				turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 3 * (p_ptr->lev + 10);

				break;

			}


		case ART_SKULLKEEPER:
			{
				msg_print("Your helm glows bright white...");
				msg_print("An image forms in your mind...");
				detect_all();
				o_ptr->timeout = rand_s16b(55) + 55;
				break;
			}

		case ART_SUN:
			{
				msg_print("Your crown glows deep yellow...");
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_timed_effect( TIMED_CUT, 0);
				o_ptr->timeout = 250;
				break;
			}


		case ART_JOSEPH:
			{
				msg_print("Your cloak glows many colours...");
				spell_basic_resistance(20);
				o_ptr->timeout = 111;
				break;
			}

		case ART_DRAEBOR:
			{
				msg_print("You're feeling impish...");
				fire_ball(GF_CONFUSION  , 5, 200, 5);					
				teleport_player(10);
				o_ptr->timeout = 155;
				break;
			}

		case ART_BARD:
			{
				msg_print("Your cloak glows bright yellow...");
				recharge(60);
				o_ptr->timeout = 70;
				break;
			}

		case ART_SHIFTER:
			{
				msg_print("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

		case ART_LIFE:
			{
				msg_print("Your cloak glows a deep red...");
				restore_level();
				o_ptr->timeout = 450;
				break;
			}


		case ART_LIGHT:
			{
				msg_print("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				o_ptr->timeout = 2;
				break;
			}

		case ART_IRONFIST:
			{
				msg_print("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_s16b(8) + 8;
				break;
			}

		case ART_GHOULS:
			{
				msg_print("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_s16b(7) + 7;
				break;
			}

		case ART_FURFICER:
			{
				msg_print("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_s16b(6) + 6;
				break;
			}

		case ART_DEAD:
			{
				msg_print("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				o_ptr->timeout = rand_s16b(5) + 5;
				break;
			}

		case ART_GRIMREAPER:
			{
				msg_print("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 450);
				o_ptr->timeout = rand_s16b(90) + 90;
				break;
			}


		case ART_BOOTS_FURCIFER:
			{
				msg_print("A wind swirls around your boots...");
        		(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 50);
				o_ptr->timeout = 200;
				break;
			}
			
		case ART_BOOTS_GABRIEL:
		{
			msg_print("A wind swirls around your boots...");
			(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 50);
			o_ptr->timeout = 200;
			break;
		}
		case ART_DANCING:
			{
				msg_print("Your boots glow deep blue...");
				(void)set_timed_effect( TIMED_AFRAID , 0);
				(void)set_timed_effect( TIMED_POISONED , 0);
				o_ptr->timeout = 5;
				break;
			}


		case ART_DAGGER_INFERNO:
			{
				msg_print("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_s16b(8) + 8;
				break;
			}

		case ART_COCYTUS:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_s16b(7) + 7;
				break;
			}

		case ART_DAGGER_FURCIFER:
			{
				msg_print("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_s16b(6) + 6;
				break;
			}

		case ART_KINSLAYER:
			{
				msg_print("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 12, 3);
				o_ptr->timeout = rand_s16b(4) + 4;
				break;
			}

		case ART_FROST:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2);
				o_ptr->timeout = rand_s16b(5) + 5;
				break;
			}

		case ART_ANDROMALIUS:
			{
				switch(randint(13))
				{
				case 1: case 2: case 3: case 4: case 5:
					teleport_player(10);
					break;
				case 6: case 7: case 8: case 9: case 10:
					teleport_player(222);
					break;
				case 11: case 12:
					(void)stair_creation();
					break;
				default:
					if(get_check("Leave this level? "))
					{
						if (autosave_l)
						{
							is_autosave = TRUE;
							msg_print("Autosaving the game...");
							do_cmd_save_game();
							is_autosave = FALSE;
						}

						new_level_flag = TRUE;
						came_from=START_RANDOM;
					}
				}
				o_ptr->timeout = 35;
				break;
			}

		case ART_MICHAEL:
			{
				msg_print("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = 300;
				break;
			}

		case ART_DAWN:
			{
				msg_print("Your sword flickers black for a moment...");
				(void)summon_specific_friendly(py, px, dun_level, FILTER_REAVER, TRUE);
				o_ptr->timeout = 500 + rand_s16b(500);
				break;
			}

		case ART_RASHAVERAK:
			{
				msg_print("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 2);
				o_ptr->timeout = 400;
				break;
			}


		case ART_ELIGOR:
			{
				msg_print("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 120);
				o_ptr->timeout = 400;
				break;
			}

		case ART_ODIN:
			{
				msg_print("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 100, 3);
				o_ptr->timeout = 500;
				break;
			}

		case ART_DESTINY:
			{
				msg_print("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

		case ART_TROLLS:
			{
				msg_print("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

		case ART_RONOVE:
			{
				msg_print("Your battle axe radiates deep purple...");
				hp_player(damroll(4, 8));
				(void)set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 50);
				o_ptr->timeout = rand_s16b(3) + 3;
				break;
			}

		case ART_TRITONS:
			{
				msg_print("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

		case ART_AZRAEL:
			{
				do_recall( "Your scythe glows soft white..." );
				o_ptr->timeout = 200;
				break;
			}

		case ART_PHENEX:
			{
				msg_print("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				o_ptr->timeout = 15;
				break;
			}

		case ART_MORNINGSTAR:
			{
				msg_print("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 3);
				o_ptr->timeout = 100;
				break;
			}

		case ART_BELETH:
			{
				msg_print("Your mace glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_timed_effect( TIMED_FAST, randint(20) + 20);
				}
				else
				{
					(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_s16b(100) + 100;
				break;
			}

		case ART_ERIRIL:
			{
				msg_print("Your quarterstaff glows yellow...");
				if (!ident_spell()) return;
				o_ptr->timeout = 10;
				break;
			}

		case ART_ATAL:
			{
				msg_print("Your quarterstaff glows brightly...");
				detect_all();
				probing();
				identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

		case ART_JUSTICE:
			{
				msg_print("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}


		case ART_DEATH:
			{
				msg_print("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	else if (o_ptr->name2 == EGO_PLANAR)
	{
		teleport_player(100);
		o_ptr->timeout = 50 + rand_s16b(50);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
		case SV_DRAGON_BLUE:
			{
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_WHITE:
			{
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 110, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_BLACK:
			{
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 130, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_GREEN:
			{
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_RED:
			{
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				msg_format("You breathe %s.",
					((chance == 1) ? "lightning" :
				((chance == 2) ? "frost" :
				((chance == 3) ? "acid" :
				((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				((chance == 2) ? GF_COLD :
				((chance == 3) ? GF_ACID :
				((chance == 4) ? GF_POIS : GF_FIRE)))),
					dir, 250, -2);
				o_ptr->timeout = rand_s16b(225) + 225;
				break;
			}

		case SV_DRAGON_BRONZE:
			{
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_GOLD:
			{
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, -2);
				o_ptr->timeout = rand_s16b(450) + 450;
				break;
			}

		case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
					dir, 220, -2);
				o_ptr->timeout = rand_s16b(300) + 300;
				break;
			}

		case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
					dir, 230, -2);
				o_ptr->timeout = rand_s16b(300) + 300;
				break;
			}

		case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
					((chance == 1) ? "chaos" :
				((chance == 2) ? "disenchantment" :
				((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				((chance == 2) ? GF_DISENCHANT :
				((chance == 3) ? GF_SOUND : GF_SHARDS))),
					dir, 250, -2);
				o_ptr->timeout = rand_s16b(300) + 300;
				break;
			}

		case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, -2);
				o_ptr->timeout = rand_s16b(300) + 300;
				break;
			}

		case SV_DRAGON_POWER:
			{
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, -3);
				o_ptr->timeout = rand_s16b(300) + 300;
				break;
			}
		}



		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if (o_ptr->tval == TV_RING)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;
		switch (o_ptr->sval)
		{
		case SV_RING_ACID:
			{
				fire_ball(GF_ACID, dir, 50, 2);
				(void)set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(20) + 20);
				o_ptr->timeout = rand_s16b(50) + 50;
				break;
			}

		case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 50, 2);
				(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(20) + 20);
				o_ptr->timeout = rand_s16b(50) + 50;
				break;
			}

		case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 50, 2);
				(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(20) + 20);
				o_ptr->timeout = rand_s16b(50) + 50;
				break;
			}
		}


		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;

	}


	/* Mistake */
	msg_print("Oops.  That object cannot be activated.");
}


static bool activate_random_artefact(object_type * o_ptr)
{
	int plev = p_ptr->lev;
	int k, dir, dummy = 0;

	if (!(o_ptr->art_name)) return FALSE; /* oops? */


	/* Activate for attack */
	switch (o_ptr->xtra2)
	{
	case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

	case ACT_BO_MISS_1:
		{
			msg_print("It glows extremely brightly...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_MISSILE, dir, damroll(2, 6));
			o_ptr->timeout = 2;
			break;
		}

	case ACT_BA_POIS_1:
		{
			msg_print("It throbs deep green...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 12, 3);
			o_ptr->timeout = rand_s16b(4) + 4;
			break;
		}


	case ACT_BO_ELEC_1:
		{
			msg_print("It is covered in sparks...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ELEC, dir, damroll(4, 8));
			o_ptr->timeout = rand_s16b(6) + 6;
			break;
		}

	case ACT_BO_ACID_1:
		{
			msg_print("It is covered in acid...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ACID, dir, damroll(5, 8));
			o_ptr->timeout = rand_s16b(5) + 5;
			break;
		}

	case ACT_BO_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_COLD, dir, damroll(6, 8));
			o_ptr->timeout = rand_s16b(7) + 7;
			break;
		}

	case ACT_BO_FIRE_1:
		{
			msg_print("It is covered in fire...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(9, 8));
			o_ptr->timeout = rand_s16b(8) + 8;
			break;
		}

	case ACT_BA_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 48, 2);
			o_ptr->timeout = 400;
			break;
		}

	case ACT_BA_FIRE_1:
		{
			msg_print("It glows an intense red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 72, 2);
			o_ptr->timeout = 400;
			break;
		}

	case ACT_DRAIN_1:
		{

			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 100))
				o_ptr->timeout = rand_s16b(100) + 100;
			break;
		}

	case ACT_BA_COLD_2:
		{
			msg_print("It glows an intense blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 300;
			break;
		}


	case ACT_BA_ELEC_2:
		{
			msg_print("It crackles with electricity...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 100, 3);
			o_ptr->timeout = 500;
			break;
		}


	case ACT_DRAIN_2:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			drain_life(dir, 120);
			o_ptr->timeout = 400;
			break;
		}

	case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 50))
					hp_player(50);
			}

			o_ptr->timeout = 400;
			break;
		}

	case ACT_BO_MISS_2:
		{
			msg_print("It grows magical spikes...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ARROW, dir, 150);
			o_ptr->timeout = rand_s16b(90) + 90;
			break;
		}

	case ACT_BA_FIRE_2:
		{
			msg_print("It glows deep red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 120, 3);
			o_ptr->timeout = rand_s16b(225) + 225;
			break;
		}

	case ACT_BA_COLD_3:
		{
			msg_print("It glows bright white...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 200, 3);
			o_ptr->timeout = rand_s16b(325) + 325;
			break;
		}

	case ACT_BA_ELEC_3:
		{
			msg_print("It glows deep blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 250, 3);
			o_ptr->timeout = rand_s16b(425) + 425;
			break;
		}

	case ACT_WHIRLWIND:
		{
			{
				int y = 0, x = 0;
				cave_type       *c_ptr;
				monster_type    *m_ptr;

				for (dir = 0; dir <= 9; dir++) {
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					/* Get the monster */
					m_ptr = &m_list[c_ptr->m_idx];

					/* Hack -- attack monsters */
					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
						py_attack(y, x);
				}
			}
			o_ptr->timeout = 250;
			break;
		}

	case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 100))
					hp_player(100);
			}

			o_ptr->timeout = 400;
			break;
		}


	case ACT_CALL_CHAOS:
		{
			msg_print("It glows in scintillating colours...");
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

	case ACT_SHARD:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_SHARD, dir,
				120 + (plev), 2);
			o_ptr->timeout = 400;
			break;
		}

	case ACT_DISP_EVIL:
		{
			msg_print("It floods the area with goodness...");
			dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = rand_s16b(300) + 300;
			break;
		}


	case ACT_DISP_GOOD:
		{
			msg_print("It floods the area with evil...");
			dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = rand_s16b(300) + 300;
			break;
		}

	case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 300, -4);
			o_ptr->timeout = 500;
			break;
		}
		/* Activate for other offensive action */

	case ACT_CONFUSE:
		{
			msg_print("It glows in scintillating colours...");
			if (!get_aim_dir(&dir)) return FALSE;
			confuse_monster(dir, 20);
			o_ptr->timeout = 15;
			break;
		}

	case ACT_SLEEP:
		{
			msg_print("It glows deep blue...");
			sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

	case ACT_QUAKE:
		{
			earthquake(py, px, 10);
			o_ptr->timeout = 50;
			break;
		}

	case ACT_TERROR:
		{
			turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}
	case ACT_TELE_AWAY:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

	case ACT_BANISH_EVIL:
		{
			if (banish_evil(100))
			{
				msg_print("The power of the artefact banishes evil!");
			}
			o_ptr->timeout = 250 + rand_s16b(250);
			break;
		}

	case ACT_GENOCIDE:
		{
			msg_print("It glows deep blue...");
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

	case ACT_MASS_GENO:
		{
			msg_print("It lets out a long, shrill note...");
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}


		/* Activate for summoning / charming */


	case ACT_CHARM_ANIMAL:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void) charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

	case ACT_CHARM_UNDEAD:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}


	case ACT_CHARM_OTHER:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void) charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}


	case ACT_CHARM_ANIMALS:
		{
			(void) charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

	case ACT_CHARM_OTHERS:
		{
			charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}


	case ACT_SUMMON_ANIMAL:
		{
			(void)summon_specific_friendly(py, px, plev, FILTER_ANIMAL_RANGER, TRUE);
			o_ptr->timeout = 200 + rand_s16b(300);
			break;
		}

	case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific_friendly(py, px, dun_level, FILTER_PHANTOM, TRUE);
			o_ptr->timeout = 200 + rand_s16b(200);
			break;
		}

	case ACT_SUMMON_ELEMENTAL:
		{
			if (randint(3) == 1) {
				if (summon_specific((int)py,(int)px, (int)(plev * 1.5),
					FILTER_ELEMENTAL)) {
						msg_print("An elemental materializes...");
						msg_print("You fail to control it!");
					}
			} else {
				if (summon_specific_friendly((int)py, (int)px, (int)(plev * 1.5),
					FILTER_ELEMENTAL,(bool)(plev == 50 ? TRUE : FALSE))) {
						msg_print("An elemental materializes...");
						msg_print("It seems obedient to you.");
					}
			}
			o_ptr->timeout = 750;
			break;
		}

	case ACT_SUMMON_DEMON:
		{
			if (randint(3) == 1) {
				if (summon_specific((int)py, (int)px, (int)(plev * 1.5),
					FILTER_DEMON)) {
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
					}
			} else {
				if (summon_specific_friendly((int)py, (int)px, (int)(plev * 1.5),
					FILTER_DEMON, (bool)(plev == 50 ? TRUE : FALSE))) {
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'What is thy bidding... Master?'");
					}
			}
			o_ptr->timeout = 666 + rand_s16b(333);
			break;
		}

	case ACT_SUMMON_UNDEAD:
		{
			if (randint(3) == 1) {
				if (summon_specific((int)py, (int)px, (int)(plev * 1.5),
					(plev > 47 ? FILTER_HI_UNDEAD : FILTER_UNDEAD))) {
						msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
						msg_print("'The dead arise... to punish you for disturbing them!'");
					}
			} else {
				if (summon_specific_friendly((int)py,(int)px, (int)(plev * 1.5),
					(plev > 47 ? FILTER_HI_UNDEAD_NO_UNIQUES : FILTER_UNDEAD),
					(bool)(((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE))) {
						msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
						msg_print("Ancient, long-dead forms arise from the ground to serve you!");
					}
			}
			o_ptr->timeout = 666 + rand_s16b(333);
			break;
		}

		/* Activate for healing */


	case ACT_CURE_LW:
		{
			(void)set_timed_effect( TIMED_AFRAID , 0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

	case ACT_CURE_MW:
		{
			msg_print("It radiates deep purple...");
			hp_player(damroll(4, 8));
			(void)set_timed_effect( TIMED_CUT, (p_ptr->cut / 2) - 50);
			o_ptr->timeout = rand_s16b(3) + 3;
			break;
		}

	case ACT_CURE_POISON:
		{
			msg_print("It glows deep blue...");
			(void)set_timed_effect( TIMED_AFRAID , 0);
			(void)set_timed_effect( TIMED_POISONED , 0);
			o_ptr->timeout = 5;
			break;
		}

	case ACT_REST_LIFE:
		{
			msg_print("It glows a deep red...");
			restore_level();
			o_ptr->timeout = 450;
			break;
		}

	case ACT_REST_ALL:
		{
			msg_print("It glows a deep green...");
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHA);
			(void)restore_level();
			o_ptr->timeout = 750;
			break;
		}


	case ACT_CURE_700:
		{
			msg_print("It glows deep blue...");
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_timed_effect( TIMED_CUT, 0);
			o_ptr->timeout = 250;
			break;
		}

	case ACT_CURE_1000:
		{
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_timed_effect( TIMED_CUT, 0);
			o_ptr->timeout = 888;
			break;
		}

		/* Activate for timed effect */

	case ACT_ESP:
		{
			(void)set_timed_effect( TIMED_ESP, p_ptr->tim_esp + randint(30) + 25);
			o_ptr->timeout = 200;
			break;
		}

	case ACT_BERSERK:
		{
			(void)set_timed_effect( TIMED_SHERO, p_ptr->shero + randint(50) + 50);
			(void)set_timed_effect( TIMED_BLESSED, p_ptr->blessed + randint(50) + 50);
			o_ptr->timeout = 100 + rand_s16b(100);
			break;
		}

	case ACT_PROT_EVIL:
		{
			msg_print("It lets out a shrill wail...");
			k = 3 * p_ptr->lev;
			(void)set_timed_effect( TIMED_PROTEVIL, p_ptr->protevil + randint(25) + k);
			o_ptr->timeout = rand_s16b(225) + 225;
			break;
		}

	case ACT_RESIST_ALL:
		{
			msg_print("It glows many colours...");
			(void)set_timed_effect( TIMED_OPPOSE_ACID, p_ptr->oppose_acid + randint(40) + 40);
			(void)set_timed_effect( TIMED_OPPOSE_ELEC, p_ptr->oppose_elec + randint(40) + 40);
			(void)set_timed_effect( TIMED_OPPOSE_FIRE, p_ptr->oppose_fire + randint(40) + 40);
			(void)set_timed_effect( TIMED_OPPOSE_COLD, p_ptr->oppose_cold + randint(40) + 40);
			(void)set_timed_effect( TIMED_OPPOSE_POIS, p_ptr->oppose_pois + randint(40) + 40);
			o_ptr->timeout = 200;
			break;
		}


	case ACT_SPEED:
		{
			msg_print("It glows bright green...");
			if (!p_ptr->fast)
			{
				(void)set_timed_effect( TIMED_FAST, randint(20) + 20);
			}
			else
			{
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
			}
			o_ptr->timeout = 250;
			break;
		}

	case ACT_XTRA_SPEED:
		{
			msg_print("It glows brightly...");
			if (!p_ptr->fast)
			{
				(void)set_timed_effect( TIMED_FAST, randint(75) + 75);
			}
			else
			{
				(void)set_timed_effect( TIMED_FAST, p_ptr->fast + 5);
			}
			o_ptr->timeout = rand_s16b(200) + 200;
			break;
		}

	case ACT_WRAITH:
		{
			set_timed_effect( TIMED_WRAITH_FORM, p_ptr->wraith_form + randint(plev/2) + (plev/2));
			o_ptr->timeout = 1000;
			break;
		}


	case ACT_INVULN:
		{
			(void)set_timed_effect( TIMED_INVULN, p_ptr->invuln + randint(8) + 8);
			o_ptr->timeout = 1000;
			break;
		}



		/* Activate for general purpose effect (detection etc.) */

	case ACT_LIGHT:
		{
			msg_print("It wells with clear light...");
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_s16b(10) + 10;
			break;
		}

	case ACT_MAP_LIGHT:
		{
			msg_print("It shines brightly...");
			map_area();
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_s16b(50) + 50;
			break;
		}

	case ACT_DETECT_ALL:
		{
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
			detect_all();
			o_ptr->timeout = rand_s16b(55) + 55;
			break;
		}

	case ACT_DETECT_XTRA:
		{
			msg_print("It glows brightly...");
			detect_all();
			probing();
			identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

	case ACT_ID_FULL:
		{
			msg_print("It glows yellow...");
			identify_fully();
			o_ptr->timeout = 750;
			break;
		}


	case ACT_ID_PLAIN:
		{

			if (!ident_spell()) return FALSE;
			o_ptr->timeout = 10;
			break;
		}

	case ACT_RUNE_EXPLO:
		{
			msg_print("It glows bright red...");
			explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

	case ACT_RUNE_PROT:
		{
			msg_print("It glows light blue...");
			warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

	case ACT_SATIATE:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

	case ACT_DEST_DOOR:
		{
			msg_print("It glows bright red...");
			destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

	case ACT_STONE_MUD:
		{
			msg_print("It pulsates...");
			if (!get_aim_dir(&dir)) return FALSE;
			wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

	case ACT_RECHARGE:
		{

			recharge(60);
			o_ptr->timeout = 70;
			break;
		}

	case ACT_ALCHEMY:
		{
			msg_print("It glows bright yellow...");
			(void) alchemy();
			o_ptr->timeout = 500;
			break;
		}


	case ACT_DIM_DOOR:
		{
			msg_print("You open a dimensional gate. ");
			if (!spell_dimensional_gate( plev )) return FALSE;
			break;
		}
	case ACT_TELEPORT:
		{
			msg_print("It twists space around you...");
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}
	case ACT_RECALL:
		{
			do_recall( "It glows soft white..." );			
			o_ptr->timeout = 200;
			break;
		}
	default:
		msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
		return FALSE;
	}
	return TRUE;
}

/* 
* Mix two potions to (maybe) create a third 
 */
void do_cmd_mix(void)
{
	int item1, item2, k_idx;
	int i1, i2, sv;
	int chance = 0;
	int penalty = 0;
	int roll;
	
	bool found = FALSE;
	
	cptr q, r; /*s UNUSED*/
	object_kind *k_ptr;
	object_type object_type_body;
	object_type *o_ptr1, *o_ptr2, *to_ptr;
	
	/* Get an item */
	q = "Mix which potion? ";
	/*s = "You don't have enough potions to mix."; UNUSED TODO*/
	r = "With which potion? ";
	item_tester_tval = TV_POTION;

		
	if(!get_item(&item1, q, "You dont have a potion",  USE_EQUIP | USE_INVEN | USE_FLOOR )) return;
	item_tester_tval = TV_POTION;
	if (!get_item(&item2, r, "You dont have another potion", USE_EQUIP | USE_INVEN | USE_FLOOR )) return;
	
	if (item1 >= 0)
	{
		o_ptr1 = &inventory[item1];
	}
	
	/* Get the item (on the floor) */
	else
	{
		o_ptr1 = &o_list[0 - item1];
	}
	
	if (item2 >= 0)
	{
		o_ptr2= &inventory[item2];
	}
	
	/* Get the item (on the floor) */
	else
	{
		o_ptr2 = &o_list[0 - item2];
	}
	
	if (o_ptr1->sval==o_ptr2->sval)
	{
		msg_print("You must mix two different potions!");
		msg_print(NULL);
		return;
	}
	
	/* Take a turn */
	energy_use = 100;
	
	/* Extract the tval/sval codes */
	for (sv = 0; sv < SV_POTION_MAX; sv++)
	{
		if (((potion_alch[sv].sval1 == o_ptr1->sval) && (potion_alch[sv].sval2 == o_ptr2->sval))
			|| ((potion_alch[sv].sval2 == o_ptr1->sval) && (potion_alch[sv].sval1 == o_ptr2->sval)))
			found = TRUE;
		
		if (found) break;
	}
	
	/* Found a potion? */
	if (found)
	{
		/* Look for it */
		for (k_idx = 1; k_idx < MAX_K_IDX; k_idx++)
		{
			k_ptr = &k_info[k_idx];
			
			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == sv)) break;
		}
		
		/* If there's no potion, always fail */
		if (k_idx != MAX_K_IDX) 
		{
			chance = 25 + (/*p_ptr->skill[SK_ALC]*/ 60) - ( k_ptr->pval /  5);
			penalty = ((k_ptr->pval * k_ptr->pval) / 2);
			
			/* Always 5% chance of success or failure*/
			if (chance < 5) chance = 5;
			if (chance > 95) chance = 95;
		}
	}
	else
	{
	  k_idx   = 0; /* Paranoia : if this falls through, we get water*/
	  k_ptr = &k_info[k_idx]; /* Paranoia : if this falls through, we get water*/
	  chance  = 5;  /* It is good I am paranoid in 5% of the cases*/
	  penalty = ((k_ptr->pval * k_ptr->pval) / 2);	  
	}
	
	/*** Skill check ***/
	roll = rand_int(100);	
	
	if (roll < chance)
	{
		/*** Create new potion ****/
		
		/* Get local object */
		to_ptr = &object_type_body;
		
		/* Wipe the object */
		object_wipe(to_ptr);
		
		/* Prepare the ojbect */
		object_prep(to_ptr, k_idx);
		
		drop_near(to_ptr, -1, py, px); /* drop the object */
		potion_alch[sv].known1 = TRUE;
		potion_alch[sv].known2 = TRUE;
	}
	else if ((roll < chance + 30) && (roll < 99)) 
	{
		msg_print("You have wasted the potions.");
		msg_print(NULL);
	}
	else 
	{
		msg_print("The potions explode in your hands!");
		msg_print(NULL);
		take_hit(damroll(4,8) + penalty, "carelessly mixing potions");
	}
	
	/* Hack - make sure the potions are destroyed in order */
	i1= (item1 > item2) ? item1 : item2;
	i2= (item1 > item2) ? item2 : item1;
	
	/* Destroy a potion in the pack */
	if (i1 >= 0)
	{
		inven_item_increase(i1, -1);
		inven_item_describe(i1);
		inven_item_optimize(i1); 
	}
	
	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - i1, -1);
		floor_item_describe(0 - i1);
		floor_item_optimize(0 - i1);
	}
	
	/* Destroy a potion in the pack */
	if (i2 >= 0)
	{
		inven_item_increase(i2, -1);
		inven_item_describe(i2);
		inven_item_optimize(i2);
	}
	
	/* Destroy a potion on the floor */
	else
	{
		floor_item_increase(0 - i2, -1);
		floor_item_describe(0 - i2);
		floor_item_optimize(0 - i2);
	}
}
