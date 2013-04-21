/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



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






/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
	int item, ident, lev;

	object_type *o_ptr;

	cptr q, s;


	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item */
	q = "Eat which item? ";
	s = "You have nothing to eat.";
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


	/* Sound */
	sound(SOUND_EAT);


	/* Take a turn */
	p_ptr->energy_use = 100;

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
				if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_BLINDNESS:
		{
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(200) + 200))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (!p_ptr->resist_fear)
			{
				if (set_afraid(p_ptr->afraid + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_int(10) + 10))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->resist_chaos)
			{
				if (set_image(p_ptr->image + rand_int(250) + 250))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10))
				{
					ident = TRUE;
				}
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
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (set_blind(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_PARANOIA:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (set_confused(0)) ident = TRUE;
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
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		}


		case SV_FOOD_RATION:
		case SV_FOOD_BISCUIT:
		case SV_FOOD_JERKY:
		case SV_FOOD_SLIME_MOLD:
		{
			mprint(MSG_TEMP, "That tastes good.");
			ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
			mprint(MSG_TEMP, "That tastes good.");
			(void)set_poisoned(0);
			(void)hp_player(damroll(4, 8));
			ident = TRUE;
			break;
		}

		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
		{
			mprint(MSG_TEMP, "That tastes good.");
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);


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
void do_cmd_quaff_potion(void)
{
	int item, ident, lev;

	object_type *o_ptr;

	cptr q, s;


	/* Inside arena */
	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item */
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";
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


	/* Sound */
	sound(SOUND_QUAFF);


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;


	/* See if this potion changes shape. */
	if (o_ptr->sval > SV_POTION_MORPH_HEAD &&
	    o_ptr->sval < SV_POTION_MORPH_TAIL) {
	  msg_print("Your body starts twisting in weird ways!");
	  change_shape(o_ptr->sval - SV_POTION_MORPH_HEAD);
	  ident = TRUE;
	} else {

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			mprint(MSG_TEMP, "You feel less thirsty.");
			ident = TRUE;
			break;
		}

		case SV_POTION_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(25) + 15)) ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			mprint(MSG_STUPID, "The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
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
				if (set_blind(p_ptr->blind + rand_int(100) + 100))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_int(20) + 15))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4))
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
				mprint(MSG_WARNING, "You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			mprint(MSG_DEADLY, "Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
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

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			mprint(MSG_DEADLY, "Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			mprint(MSG_DEADLY, "A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			ident = TRUE;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			if (set_tim_infra(p_ptr->tim_infra + 100 + randint(100)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (set_tim_invis(p_ptr->tim_invis + 12 + randint(12)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (set_poisoned(p_ptr->poisoned / 2)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (set_poisoned(0)) ident = TRUE;
			break;
		}

		case SV_POTION_BOLDNESS:
		{
			if (set_afraid(0)) ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(25) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (hp_player(10)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint(25) + 25)) ident = TRUE;
			break;
		}

		case SV_POTION_BESERK_STRENGTH:
		{
			if (hp_player(30)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (set_shero(p_ptr->shero + randint(25) + 25)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
			break;
		}

	case SV_POTION_INSANE_LIGHT:
	  {
	    if (heal_insanity(damroll(4, 8))) ident = TRUE;
	    break;
	  }


		case SV_POTION_CURE_SERIOUS:
		{
			if (hp_player(damroll(4, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
			break;
		}

	case SV_POTION_INSANE_SERIOUS:
	  {
	    if (heal_insanity(damroll(8, 8))) ident = TRUE;
	    break;
	  }

		case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

	case SV_POTION_INSANE_CRIT:
	  {
	    if (heal_insanity(damroll(12, 8))) ident = TRUE;
	    break;
	  }

		case SV_POTION_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

	case SV_POTION_INSANE_CURE:
	  {
	    if (heal_insanity(600)) ident = TRUE;
	    break;
	  }

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			mprint(MSG_BIG_BONUS, "You feel life flow through your body!");
			restore_level();
			hp_player(5000);
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);
			ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				mprint(MSG_BONUS, "Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_SPELL | PW_PLAYER);
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

		case SV_POTION_RES_CHR:
		{
			if (do_res_stat(A_CHR)) ident = TRUE;
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

		case SV_POTION_INC_CHR:
		{
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) ident = TRUE;
			if (do_inc_stat(A_INT)) ident = TRUE;
			if (do_inc_stat(A_WIS)) ident = TRUE;
			if (do_inc_stat(A_DEX)) ident = TRUE;
			if (do_inc_stat(A_CON)) ident = TRUE;
			if (do_inc_stat(A_CHR)) ident = TRUE;
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			mprint(MSG_BONUS, "An image of your surroundings forms in your mind...");
			wiz_lite();
			ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			mprint(MSG_BONUS, "You begin to feel more enlightened...");
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
			mprint(MSG_BONUS, "You begin to know yourself a little better...");
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
				mprint(MSG_BIG_BONUS, "You feel more experienced.");
				gain_exp(ee);
				ident = TRUE;
			}
			break;
		}

	case SV_POTION_MUTATION:
	  {
	    int i;
	    int j = randint(4);

	    mprint(MSG_WARNING, "You mutate!");

	    for (i = 0; i < j; i++) {
	      generate_mutation();
	    }
	    ident = TRUE;
	    break;
	  }

	case SV_POTION_CURE_MUTATION:
	  {
	    int i;
	    int j = randint(4);

	    for (i = 0; i < j; i++) {
	      remove_mutation();
	    }
	    ident = TRUE;
	    break;
	  }

	}
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

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


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
 * Curse the players armor
 */
bool curse_armor(void)
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
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		mformat(MSG_WARNING, "A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
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
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
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
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}

	/* not artifact or failed save... */
	else
	{
		/* Oops */
		mformat(MSG_WARNING, "A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
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
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	}

	/* Notice */
	return (TRUE);
}


void show_book_number(int num) {
  char buf[1024];

  path_build(buf, 1024, ANGBAND_DIR_FILE, format("book-%d.txt", num));

  character_icky = TRUE;
  Term_save();
  
  show_file(buf, "Arcane Knowledge", 0, 0);

  Term_load();
  character_icky = FALSE;
}

static bool item_tester_hook_read(object_type* o_ptr) {
  if (o_ptr->tval == TV_SCROLL || o_ptr->tval == TV_TEXT) return TRUE;
  return FALSE;
}

/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, k, used_up, ident, lev;

	object_type *o_ptr;

	cptr q, s;


	/* Inside arena */
	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}

	/* Check some conditions */
	if (p_ptr->blind)
	{
		mprint(MSG_TEMP, "You can't see anything.");
		return;
	}
	if (no_lite())
	{
		mprint(MSG_TEMP, "You have no light to read by.");
		return;
	}
	if (p_ptr->confused)
	{
		mprint(MSG_TEMP, "You are too confused!");
		return;
	}


	/* Restrict choices to scrolls */
	item_tester_hook = item_tester_hook_read;

	/* Get an item */
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";
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


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;

	/* Display some text */

	if (o_ptr->tval == TV_TEXT) {
	  show_book_number(o_ptr->sval);
	} else {

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			mprint(MSG_WARNING, "There is a high pitched humming noise.");
			aggravate_monsters(1);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armor()) ident = TRUE;
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
				if (summon_specific(py, px, p_ptr->depth, 0))
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
				if (summon_specific(py, px, p_ptr->depth, SUMMON_UNDEAD))
				{
					ident = TRUE;
				}
			}
			break;
		}

	case SV_SCROLL_SUMMON_PET:
	  {
	    for (k = 0; k < randint(3); k++) {
	      summon_pet_monster();
	      ident = TRUE;
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
			if (p_ptr->word_recall == 0)
			{
				p_ptr->word_recall = randint(20) + 15;
				mprint(MSG_BONUS, "The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
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
				mprint(MSG_BONUS, "You feel as if someone is watching over you.");
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
			if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
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
			if (set_blessed(p_ptr->blessed + randint(12) + 6)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (set_blessed(p_ptr->blessed + randint(24) + 12)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_blessed(p_ptr->blessed + randint(48) + 24)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				mprint(MSG_BONUS, "Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
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
			(void)genocide();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			(void)mass_genocide();
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

	case SV_SCROLL_RECIPE:
	  {
	    int num = rand_range(2, 4);
	    int j;
	    int foo = num;

	    while (1) {
	      j = randint(MAX_RECIPES);

	      if (recipe_info[j].ingrs) {
		recipe_recall[j] = 1;
		num--;
	      }

	      if (num == 0) break;
	    }

	    mformat(MSG_BONUS, "You learn %d new arcane formulae.", foo);
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


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
}







/*
 * Use a staff
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
void do_cmd_use_staff(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, ident, chance, k, lev;

	object_type *o_ptr;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;

	cptr q, s;


	/* Inside arena */
	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
	q = "Use which staff? ";
	s = "You have no staff to use.";
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


	/* Mega-Hack -- refuse to use a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		mprint(MSG_TEMP, "You must first pick up the staffs.");
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

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
		mprint(MSG_TEMP, "The staff has no charges left.");
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
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + 3 + randint(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint(30) + 15)) ident = TRUE;
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
				if (summon_specific(py, px, p_ptr->depth, 0))
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
					mprint(MSG_BONUS, "The staff glows blue for a moment...");
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
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil()) ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(randint(8))) ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
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
				mprint(MSG_BONUS, "Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_SPELL | PW_PLAYER);
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
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
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
			if (set_protevil(p_ptr->protevil + randint(25) + k)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_GENOCIDE:
		{
			(void)genocide();
			ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			earthquake();
			ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			ident = TRUE;
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
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	/* Hack -- some uses are "free" */
	if (!use_charge) return;


	/* Use a single charge */
	o_ptr->pval--;

	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		p_ptr->total_weight -= i_ptr->weight;
		item = inven_carry(i_ptr);

		/* Message */
		mprint(MSG_TEMP, "You unstack your staff.");
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
void do_cmd_aim_wand(void)
{
	int item, lev, ident, chance, dir, sval;

	object_type *o_ptr;

	cptr q, s;


	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Inside arena */
	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}


	/* Get an item */
	q = "Aim which wand? ";
	s = "You have no wand to aim.";
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


	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		mprint(MSG_TEMP, "You must first pick up the wands.");
		return;
	}


	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

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
		mprint(MSG_TEMP, "The wand has no charges left.");
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
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(5, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			fire_bolt_or_beam(20, GF_ELEC, dir, damroll(3, 8));
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
				case 1:
				{
					fire_ball(GF_ACID, dir, 100, 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 80, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 100, 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 80, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 60, 3);
					break;
				}
			}

			ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 125)) ident = TRUE;
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
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	/* Use a single charge */
	o_ptr->pval--;

	/* Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Restore the charges */
		o_ptr->pval++;

		/* Unstack the used item */
		o_ptr->number--;
		p_ptr->total_weight -= i_ptr->weight;
		item = inven_carry(i_ptr);

		/* Message */
		mprint(MSG_TEMP, "You unstack your wand.");
	}

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
void do_cmd_zap_rod(void)
{
	int item, ident, chance, dir, lev;

	object_type *o_ptr;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

	cptr q, s;


	/* Inside arena */
	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}

	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item */
	q = "Zap which rod? ";
	s = "You have no rod to zap.";
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


	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		mprint(MSG_TEMP, "You must first pick up the rods.");
		return;
	}


	/* Get a direction (unless KNOWN not to need it) */
	if ((o_ptr->sval >= SV_ROD_MIN_DIRECTION) || !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

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
		mprint(MSG_TEMP, "The rod is still charging.");
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
			o_ptr->pval = 50;
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
			if (p_ptr->word_recall == 0)
			{
				mprint(MSG_BONUS, "The air about you becomes charged...");
				p_ptr->word_recall = 15 + randint(20);
			}
			else
			{
				msg_print("A tension leaves the air around you...");
				p_ptr->word_recall = 0;
			}
			ident = TRUE;
			o_ptr->pval = 60;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			o_ptr->pval = 30;
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
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			o_ptr->pval = 999;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
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
			if (do_res_stat(A_CHR)) ident = TRUE;
			o_ptr->pval = 999;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (!p_ptr->fast)
			{
				if (set_fast(randint(30) + 15)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
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
			o_ptr->pval = 30;
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- deal with cancelled zap */
	if (!use_charge)
	{
		o_ptr->pval = 0;
		return;
	}


	/* XXX Hack -- unstack if necessary */
	if ((item >= 0) && (o_ptr->number > 1))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Restore "charge" */
		o_ptr->pval = 0;

		/* Unstack the used item */
		o_ptr->number--;
		p_ptr->total_weight -= i_ptr->weight;
		item = inven_carry(i_ptr);

		/* Message */
		mprint(MSG_TEMP, "You unstack your rod.");
	}
}




/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
	u32b f1, f2, f3;

	if (o_ptr->tval == TV_RANDART) return TRUE;

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
static void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			mprint(MSG_DEADLY, "You are surrounded by a malignant aura.");

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
			mprint(MSG_BIG_BONUS, "You are surrounded by a powerful aura.");

			/* Dispel monsters */
			dispel_monsters(1000);

			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 300, 3);

			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			fire_bolt(GF_MANA, dir, 250);

			break;
		}
	}
}




/*
 * Enchant some bolts -- merged with the mage spell -GJW	-KMW-
 */
static bool brand_bolts(void)
{
	brand_ammo (1, 1);
	return (TRUE);
}


/*
 * Activate a random artifact.
 */

static void random_artifact_effect(byte type, byte arg) {
  int foo, bar, i;


  switch (type) {
  case T_ACT_DEATH:
    mprint(MSG_DEADLY, "You feel a deathly chill come over you...");
    take_hit(5000, "an ancient artifact");
    break;

  case T_ACT_RUIN:
    mprint(MSG_DEADLY, "Your nerves and muscles feel weak and lifeless!");
    take_hit(damroll(10, 10), "an ancient artifact");
    (void)dec_stat(A_DEX, 25, TRUE);
    (void)dec_stat(A_WIS, 25, TRUE);
    (void)dec_stat(A_CON, 25, TRUE);
    (void)dec_stat(A_STR, 25, TRUE);
    (void)dec_stat(A_CHR, 25, TRUE);
    (void)dec_stat(A_INT, 25, TRUE);
    break;

  case T_ACT_DESTRUC:
    destroy_area(p_ptr->py, p_ptr->px, (arg/5)+1, TRUE);
    break;

  case T_ACT_STUPID:
    do_dec_stat(A_INT);
    break;

  case T_ACT_UNHEALTH:
    do_dec_stat(A_CON);
    break;

  case T_ACT_UGLY:
    do_dec_stat(A_CHR);
    break;
      
  case T_ACT_CLUMSY:
    do_dec_stat(A_DEX);
    break;

  case T_ACT_NAIVE:
    do_dec_stat(A_WIS);
    break;

  case T_ACT_STAT_LOSS:
    do_dec_stat(A_INT);
    do_dec_stat(A_WIS);
    do_dec_stat(A_CON);
    do_dec_stat(A_CHR);
    do_dec_stat(A_STR);
    do_dec_stat(A_DEX);
    break;

  case T_ACT_HUGE_STAT_LOSS:
    mprint(MSG_DEADLY, "Your nerves and muscles feel weak and lifeless!");
    (void)dec_stat(A_DEX, 15, TRUE);
    (void)dec_stat(A_WIS, 15, TRUE);
    (void)dec_stat(A_CON, 15, TRUE);
    (void)dec_stat(A_STR, 15, TRUE);
    (void)dec_stat(A_CHR, 15, TRUE);
    (void)dec_stat(A_INT, 15, TRUE);
    break;

  case T_ACT_EXP_LOSS:
    if (p_ptr->hold_life) {
      mprint(MSG_URGENT, "You feel your life slipping away!");
      lose_exp(p_ptr->exp / (40));
    } else {
      mprint(MSG_DEADLY, "You feel your life draining away!");
      lose_exp(p_ptr->exp / 4);
    }
    break;

  case T_ACT_HUGE_EXP_LOSS:
    mprint(MSG_DEADLY, "You are surrounded by a malignant aura!");

    if (p_ptr->hold_life) {
      p_ptr->exp -= (p_ptr->exp / (20));
      p_ptr->max_exp -= (p_ptr->max_exp / (20));
    } else {
      p_ptr->exp -= (p_ptr->exp / 2);
      p_ptr->max_exp -= (p_ptr->max_exp / 2);
    }
    check_experience();
    break;

  case T_ACT_TELE:
    teleport_player(arg+10);
    break;

  case T_ACT_SUMMON:
    foo = (arg / 10)+1;
    bar = arg;

    if (bar >= MAX_DEPTH) bar = MAX_DEPTH - 1;

    if (foo == 1) {
      mprint(MSG_STUPID, "A dimensional gate appears, and a monster tries to enter it.");
    } else {
      mprint(MSG_STUPID, "A dimensional gate appears, and several monsters try to enter it.");
    }

    for (i = 0; i < foo; i++) {
      summon_specific(p_ptr->py, p_ptr->px, bar, 0);
    }
    break;

  case T_ACT_PARALYZE:
    set_paralyzed(p_ptr->paralyzed + arg*5+10);
    break;

  case T_ACT_HALLUC:
    set_image(p_ptr->image + arg*5+10);
    break;

  case T_ACT_POIS:
    set_poisoned(p_ptr->poisoned + arg*5+10);
    break;

  case T_ACT_HUNGER:
    set_food(p_ptr->food - arg*10);
    break;

  case T_ACT_STUN:
    set_stun(p_ptr->stun + arg*5+10);
    break;

  case T_ACT_CUT:
    set_cut(p_ptr->cut + arg*5+10);
    break;

  case T_ACT_FEAR:
    set_afraid(p_ptr->afraid + arg*5+10);
    break;

  case T_ACT_CONFUSE:
    set_confused(p_ptr->confused + arg*5+10);
    break;

  case T_ACT_BLIND:
    set_blind(p_ptr->blind + arg*5+10);
    break;

  case T_ACT_PET_SUMMON:
    foo = (arg / 10)+1;
    bar = arg;

    if (bar >= MAX_DEPTH) bar = MAX_DEPTH - 1;

    if (foo == 1) {
      mprint(MSG_BONUS, "A dimensional gate appears, and an ally tries to enters it.");
    } else {
      mprint(MSG_BIG_BONUS, "A dimensional gate appears, and several allies try to enter it.");
    }

    for (i = 0; i < foo; i++) {
      summon_specific_friendly(p_ptr->py, p_ptr->px, bar, 0);
    }
    break;

  case T_ACT_CURE_PARALYZE:
    set_paralyzed(p_ptr->paralyzed - arg*5+10);
    break;

  case T_ACT_CURE_HALLUC:
    set_image(p_ptr->image - arg*5+10);
    break;

  case T_ACT_CURE_POIS:
    set_poisoned(p_ptr->poisoned - arg*5+10);
    break;

  case T_ACT_CURE_HUNGER:
    set_food(p_ptr->food + arg*10);
    break;

  case T_ACT_CURE_STUN:
    set_stun(p_ptr->stun - arg*5+10);
    break;

  case T_ACT_CURE_CUT:
    set_cut(p_ptr->cut - arg*5+10);
    break;

  case T_ACT_CURE_FEAR:
    set_afraid(p_ptr->afraid - arg*5+10);
    break;

  case T_ACT_CURE_CONFUSE:
    set_confused(p_ptr->confused - arg*5+10);
    break;

  case T_ACT_CURE_BLIND:
    set_blind(p_ptr->blind - arg*5+10);
    break;

  case T_ACT_CURE_L_WOUND:
    hp_player(damroll(2, 8));
    set_blind(0);
    set_cut(p_ptr->cut - 10);
    break;

  case T_ACT_CURE_S_WOUND:
    hp_player(damroll(4, 8));
    set_blind(0);
    set_confused(0);
    set_cut((p_ptr->cut / 2) - 50);
    break;

  case T_ACT_CURE_C_WOUND:
    hp_player(damroll(6, 8));
    set_blind(0);
    set_confused(0);
    set_poisoned(0);
    set_stun(0);
    set_cut(0);
    break;

  case T_ACT_CURE:
    hp_player(arg+100);
    set_blind(0);
    set_confused(0);
    set_poisoned(0);
    set_stun(0);
    set_cut(0);
    break;

  case T_ACT_GENO:
    genocide();
    break;

  case T_ACT_MASS_GENO:
    mass_genocide();
    break;

  case T_ACT_RESTORE:
    do_res_stat(A_STR);
    do_res_stat(A_INT);
    do_res_stat(A_WIS);
    do_res_stat(A_DEX);
    do_res_stat(A_CON);
    do_res_stat(A_CHR);
    break;

  case T_ACT_LIGHT:
    lite_area(arg, (arg/15)+1);
    break;

  case T_ACT_DARKNESS:
    unlite_area(arg, (arg/15)+1);
    break;

  case T_ACT_TELEPORT:
    teleport_player(arg+10);
    break;

  case T_ACT_TPORT_VERT:
    teleport_player_level();
    break;

  case T_ACT_ACQUIREMENT:
    object_level = (arg/5)+1;
    acquirement(p_ptr->py, p_ptr->px, 1, FALSE);
    object_level = p_ptr->depth;
    break;

  case T_ACT_WONDER:
    random_artifact_effect(rand_int(MAX_T_ACT), arg);
    break;

  case T_ACT_AGGRAVATE:
    aggravate_monsters(1);
    break;

  case T_ACT_MUTATE:
    mprint(MSG_WARNING, "Your body mutates!");
    generate_mutation();
    break;

  case T_ACT_CURE_INSANITY:
    heal_insanity(damroll((arg/10)+1, 8));
    break;

  case T_ACT_CURE_MUTATE:
    remove_mutation();
    break;
  }
}

/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(void)
{
	int item, i, k, dir, lev, chance;

	object_type *o_ptr;

	cptr q, s;


	if (p_ptr->inside_special == 1) {
	  msg_print("The arena absorbs all attempted magic!");
	  msg_print(NULL);
	  return;
	}

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
	s = "You have nothing to activate.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

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

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr)) {
	  if (o_ptr->tval == TV_RANDART) {
	    lev = random_artifacts[o_ptr->sval].level;
	  } else {
	    lev = a_info[o_ptr->name1].level;
	  }
	}

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
		mprint(MSG_TEMP, "It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	mprint(MSG_TEMP, "You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

	if (o_ptr->tval == TV_RANDART) {
	  random_artifact* rart = &random_artifacts[o_ptr->sval];
	  random_artifact_effect(rart->tact, rart->sact);

	  o_ptr->timeout = rart->sact*5 + randnor(0, 10);
	  
	  /* Window stuff */
	  p_ptr->window |= (PW_INVEN | PW_EQUIP);

	  /* Done */
	  return;
	}

	/* Artifacts */
	if (o_ptr->name1)
	{
		/* Choose effect */
		switch (o_ptr->name1)
		{
			case ART_GALADRIEL:
			{
				mprint(MSG_BONUS, "The phial wells with clear light...");
				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = rand_int(10) + 10;
				break;
			}

			case ART_ELENDIL:
			{
				mprint(MSG_BONUS, "The star shines brightly...");
				map_area();
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case ART_THRAIN:
			{
				mprint(MSG_BONUS, "The stone glows a deep green...");
				wiz_lite();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}


			case ART_CARLAMMAS:
			{
				mprint(MSG_BONUS, "The amulet lets out a shrill wail...");
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint(25) + k);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_INGWE:
			{
				mprint(MSG_BONUS, "The amulet floods the area with goodness...");
				dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}


			case ART_TULKAS:
			{
				mprint(MSG_BONUS, "The ring glows brightly...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(75) + 75);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_int(150) + 150;
				break;
			}

			case ART_NARYA:
			{
				mprint(MSG_BONUS, "The ring glows deep red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 120, 3);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_NENYA:
			{
				mprint(MSG_BONUS, "The ring glows bright white...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 200, 3);
				o_ptr->timeout = rand_int(325) + 325;
				break;
			}

			case ART_VILYA:
			{
				mprint(MSG_BONUS, "The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 250, 3);
				o_ptr->timeout = rand_int(425) + 425;
				break;
			}

			case ART_POWER:
			{
				mprint(MSG_WARNING, "The ring glows intensely black...");
				if (!get_aim_dir(&dir)) return;
				ring_of_power(dir);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}


			case ART_RAZORBACK:
			{
				mprint(MSG_BONUS, "Your armor is surrounded by lightning...");
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_BLADETURNER:
			{
				mprint(MSG_BONUS, "Your armor glows many colours...");
				(void)hp_player(30);
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + randint(50) + 50);
				(void)set_blessed(p_ptr->blessed + randint(50) + 50);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(50) + 50);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(50) + 50);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(50) + 50);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(50) + 50);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(50) + 50);
				o_ptr->timeout = 400;
				break;
			}


			case ART_SOULKEEPER:
			{
				mprint(MSG_BIG_BONUS, "Your armor glows a bright white...");
				mprint(MSG_BIG_BONUS, "You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				mprint(MSG_BONUS, "Your armor twists space around you...");
				teleport_player(10);
				o_ptr->timeout = 2;
				break;
			}

			case ART_CELEBORN:
			{
				mprint(MSG_BONUS, "Your armor glows deep blue...");
				(void)genocide();
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				mprint(MSG_BONUS, "Your armor glows bright red...");
				destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}


			case ART_HOLHENNETH:
			{
				mprint(MSG_BONUS, "Your helm glows bright white...");
				mprint(MSG_BONUS, "An image forms in your mind...");
				detect_all();
				o_ptr->timeout = rand_int(55) + 55;
				break;
			}

			case ART_GONDOR:
			{
				mprint(MSG_BONUS, "Your crown glows deep blue...");
				mprint(MSG_BONUS, "You feel a warm tingling inside...");
				(void)hp_player(500);
				(void)set_cut(0);
				o_ptr->timeout = 500;
				break;
			}


			case ART_COLLUIN:
			{
				mprint(MSG_BONUS, "Your cloak glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				o_ptr->timeout = 111;
				break;
			}

			case ART_HOLCOLLETH:
			{
				mprint(MSG_BONUS, "Your cloak glows deep blue...");
				sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

			case ART_THINGOL:
			{
				mprint(MSG_BONUS, "Your cloak glows bright yellow...");
				recharge(60);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
				mprint(MSG_BONUS, "Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				mprint(MSG_BONUS, "Your cloak glows a deep red...");
				restore_level();
				o_ptr->timeout = 450;
				break;
			}


			case ART_CAMMITHRIM:
			{
				mprint(MSG_BONUS, "Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				mprint(MSG_BONUS, "Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_PAURNIMMEN:
			{
				mprint(MSG_BONUS, "Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_PAURAEGEN:
			{
				mprint(MSG_BONUS, "Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_PAURNEN:
			{
				mprint(MSG_BONUS, "Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_FINGOLFIN:
			{
				mprint(MSG_BONUS, "Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 150);
				o_ptr->timeout = rand_int(90) + 90;
				break;
			}


			case ART_FEANOR:
			{
				mprint(MSG_BIG_BONUS, "Your boots glow bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = 200;
				break;
			}

			case ART_DAL:
			{
				mprint(MSG_BONUS, "Your boots glow deep blue...");
				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}


			case ART_NARTHANC:
			{
				mprint(MSG_BONUS, "Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_NIMTHANC:
			{
				mprint(MSG_BONUS, "Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_DETHANC:
			{
				mprint(MSG_BONUS, "Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_RILIA:
			{
				mprint(MSG_BONUS, "Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 12, 3);
				o_ptr->timeout = rand_int(4) + 4;
				break;
			}

			case ART_BELANGIL:
			{
				mprint(MSG_BONUS, "Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2);
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_ARUNRUTH:
			{
				mprint(MSG_BONUS, "Your sword glows a pale blue...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(12, 8));
				o_ptr->timeout = 500;
				break;
			}

			case ART_RINGIL:
			{
				mprint(MSG_BONUS, "Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = 300;
				break;
			}

			case ART_ANDURIL:
			{
				mprint(MSG_BONUS, "Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 2);
				o_ptr->timeout = 400;
				break;
			}


			case ART_THEODEN:
			{
				mprint(MSG_BONUS, "Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 120);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				mprint(MSG_BONUS, "Your spear glows a bright white...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				mprint(MSG_BONUS, "Your spear pulsates...");
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				mprint(MSG_BONUS, "Your axe lets out a long, shrill note...");
				(void)mass_genocide();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				mprint(MSG_BONUS, "Your battle axe radiates deep purple...");
				hp_player(damroll(4, 8));
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = rand_int(3) + 3;
				break;
			}

			case ART_ULMO:
			{
				mprint(MSG_BONUS, "Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_AVAVIR:
			{
				mprint(MSG_BONUS, "Your scythe glows soft white...");
				if (p_ptr->word_recall == 0)
				{
					p_ptr->word_recall = randint(20) + 15;
					mprint(MSG_BONUS, "The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				o_ptr->timeout = 200;
				break;
			}


			case ART_TOTILA:
			{
				mprint(MSG_BONUS, "Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				o_ptr->timeout = 15;
				break;
			}

			case ART_FIRESTAR:
			{
				mprint(MSG_BONUS, "Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_TARATOL:
			{
				mprint(MSG_BIG_BONUS, "Your mace glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}

			case ART_ERIRIL:
			{
				mprint(MSG_BONUS, "Your quarterstaff glows yellow...");
				if (!ident_spell()) return;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				mprint(MSG_BONUS, "Your quarterstaff glows brightly...");
				probing();
				o_ptr->timeout = 20;
				break;
			}

			case ART_TURMIL:
			{
				mprint(MSG_BONUS, "Your hammer glows white...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}


			case ART_CUBRAGOL:
			{
				mprint(MSG_BONUS, "Your crossbow glows deep red...");
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
				mprint(MSG_BONUS, "You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_WHITE:
			{
				mprint(MSG_BONUS, "You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_BLACK:
			{
				mprint(MSG_BONUS, "You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_GREEN:
			{
				mprint(MSG_BONUS, "You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_RED:
			{
				mprint(MSG_BONUS, "You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				mformat(MSG_BONUS, "You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				mprint(MSG_BONUS, "You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_GOLD:
			{
				mprint(MSG_BONUS, "You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				mformat(MSG_BONUS, "You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 220, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				mformat(MSG_BONUS, "You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARD),
				          dir, 230, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				mformat(MSG_BONUS, "You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARD))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				mformat(MSG_BONUS, "You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_POWER:
			{
				mprint(MSG_BONUS, "You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}


	/* Mistake */
	mprint(MSG_TEMP, "Oops.  That object cannot be activated.");
}



/* Test for an ingredient. */

static bool item_tester_hook_ingr(object_type *o_ptr) {
	if (o_ptr->tval == TV_INGRED) return (TRUE);
	return (FALSE);
}


/* Make something using alchemical formulae. */

void do_cmd_brew_stuff(void) {
  int item, i, level;
  long mask, ingrs;
  bool made_ok = FALSE;
  object_type tmp_obj;
  cptr q, s;

  mask = 0;
  q = "Combine what? ";
  s = "You don't have any ingredients left!";

  while (TRUE) {
    item_tester_hook = item_tester_hook_ingr;
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) break;
    mask |= (1L << inventory[item].sval);
    inven_item_increase(item, -1);
    inven_item_optimize(item);
  }

  p_ptr->energy_use = 100;

  if (mask) {
    for (i = 0; i < MAX_RECIPES; i++) {

      ingrs = recipe_info[i].ingrs;
      if (ingrs == 0) break;

      if (mask == ingrs) {
	object_prep(&tmp_obj, recipe_info[i].result_kind);
	apply_magic(&tmp_obj, p_ptr->lev, TRUE, FALSE, FALSE);
	object_aware(&tmp_obj);
	object_known(&tmp_obj);

	level = k_info[tmp_obj.k_idx].level;

	/* All items should be possible */
	if (level > 50) level = 50;

	if (p_ptr->lev > level+randnor(0, 7)) {
	  made_ok = TRUE;
	}

	if (level <= p_ptr->lev && made_ok) {
	  mprint(MSG_BONUS, "You have made something.");
	} else if (level > p_ptr->lev && made_ok) {
	  mprint(MSG_BONUS, "Somehow you managed to make something.");
	} else if (level <= p_ptr->lev && !made_ok) {
	  mprint(MSG_WARNING, "You messed up somewhere.");
	} else if (level > p_ptr->lev && !made_ok) {
	  mprint(MSG_WARNING, "The mix explodes in your face!");
	}

	if (made_ok) {
	  drop_near(&tmp_obj, -1, p_ptr->py, p_ptr->px);
	  recipe_recall[i] = 1;

	  return;
	} else {
	  nasty_side_effect();

	  if (!recipe_recall[i]) recipe_recall[i] = -1;

	  return;
	}
      }
    }

    msg_print("You mix something gloppy and evil-smelling.");
  }
}


/*
 * Handle sacrifices.
 * Grace is increased by value of sacrifice.
 */

void do_cmd_sacrifice(void) {
  int item, val, numb = 1;
  cptr q, s;

  byte on_what = cave_feat[p_ptr->py][p_ptr->px];
  byte what_god;

  /* Check valididty */

  if (on_what < FEAT_ALTAR_HEAD || on_what > FEAT_ALTAR_TAIL) {
    show_god_info(FALSE);
    return;
  }

  what_god = on_what-FEAT_ALTAR_HEAD+1;

  q = "Sacrifice what? ";
  s = "You don't have anything to sacrifice.";

  /* Get sacrifice */

  if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

  if (inventory[item].number > 1) {
    numb = get_quantity(NULL, inventory[item].number);
    if (numb <= 0) return;
  }

  p_ptr->energy_use = 100;

  val = object_value(&inventory[item])*numb;

  /* Modify grace */

  if (val) {
    if (p_ptr->pgod == 0) {
      p_ptr->pgod = what_god;
      set_grace(p_ptr->grace + val);
      p_ptr->god_favor = -60000;

    } else if (p_ptr->pgod != what_god) {
      mformat(MSG_DEADLY, "%s thunders in outrage at your blasphemy!", 
		 deity_info[p_ptr->pgod-1].name);
      set_grace(p_ptr->grace - val*10);

      if (val > 2500) {
	mformat(MSG_WARNING, "You feel %s abandon you.", deity_info[p_ptr->pgod-1].name);
	p_ptr->pgod = what_god;
	set_grace(val);
	p_ptr->god_favor = -60000;
      }

    } else {
      set_grace(p_ptr->grace + val*5);
    }

    inven_item_increase(item, -numb);
    inven_item_optimize(item);

  } else {
    msg_format("%s is not interested in your trifles.", 
	       deity_info[what_god-1].name);
  }
}


/* 
 * Handle CLI commands 
 */

void do_cmd_cli(void) {
  char buff[80];
  int i;

  strcpy(buff, "");
  
  if (!get_string_cli("Command: ", buff, 30)) return;

  for (i = 0; i < MAX_COMMANDS && cli_info[i].comm1; i++) {
    if (!strcmp(buff, cli_info[i].comm1)) {
      cli_info[i].func();
      return;
    } else if (cli_info[i].comm2 && !strcmp(buff, cli_info[i].comm2)) {
      cli_info[i].func();
      return;
    }
  }

  mformat(MSG_TEMP, "No such command: %s", buff);
}

/* Front-end to spell learning commands */

void do_cmd_gain_helper(void) {
  if (p_ptr->pclass == CLASS_CORRUPTED) {
    do_cmd_study();
  } else if (p_ptr->prace == RACE_GHOST) {
    msg_print("You try to recall a spell from your past...");
    do_cmd_study();
  } else if (p_ptr->prace == RACE_MUNCHKIN ||
	     p_ptr->munchkin) {
    do_cmd_study();
  } else {
    mprint(MSG_TEMP, "You must visit your superiors and be taught.");
    msg_print(NULL);
  }
}

/* Save and quit */

void do_cmd_save_quit(void) {
  /* Stop playing */
  p_ptr->playing = FALSE;

  /* Leaving */
  p_ptr->leaving = TRUE;
}


/* 
 * Print CLI help 
 */

void do_cmd_command_help(void) {
  int i;
  FILE* fff;
  char file_name[1024];

  if (path_temp(file_name, 1024)) return;

  fff = my_fopen(file_name, "w");

  for (i = 0; i < MAX_COMMANDS && cli_info[i].comm1; i++) {
    if (cli_info[i].comm2) {
      fprintf(fff, "    %s, %s -- %s\n", cli_info[i].comm1,
	      cli_info[i].comm2, cli_info[i].descrip);
    } else {
      fprintf(fff, "    %s -- %s\n", cli_info[i].comm1, 
	      cli_info[i].descrip);
    }
  }

  my_fclose(fff);
  
  show_file(file_name, "Command Line Interface Help", 0, 0);

  fd_kill(file_name);
  do_cmd_redraw();
}


/*
 * Move up or down 50'
 * If the argument is true, that means to move down, up otherwise.
 */

static bool tport_vertically(bool how) {
  if (p_ptr->inside_special > 0) { /* arena or quest -KMW- */
    mprint(MSG_TEMP, "There is no effect.");
    return FALSE;
  }

  /* Go down */

  if (how) {
    if (p_ptr->inside_special == 2 || p_ptr->depth >= MAX_DEPTH-1) {
      mformat(MSG_TEMP, "The floor is impermeable.");
      return FALSE;
    }

    msg_print("You sink through the floor.");
    p_ptr->depth++;
    p_ptr->leaving = TRUE;
  } else {
    if (!p_ptr->depth) {
      mprint(MSG_TEMP, "The only thing above you is air.");
      return FALSE;
    }

    msg_print("You rise through the ceiling.");
    p_ptr->depth--;
    p_ptr->leaving = TRUE;
  }
  return TRUE;
}

/*
 * Do a special ``movement'' action. Meant to be used for ``immovable''
 * characters.
 */

void do_cmd_immovable_special(void) {
  int i;
  int foo = p_ptr->immov_cntr;
  int lose_sp = 0;
  int lose_hp = 0;
  bool did_act = FALSE;
  bool did_load = FALSE;

  if (foo > 1) {
    if (p_ptr->csp > foo/2) {

      mformat(MSG_WARNING, "This will drain %d mana points!", foo/2);
      if (!get_check("Proceed? "))
	return;

      lose_sp = foo/2;

    } else if (p_ptr->chp > foo/2) {

      mformat(MSG_WARNING, "Warning: This will drain %d hit points!", foo/2);
      if (!get_check("Proceed? "))
	return;

      lose_hp = foo/2;

    } else {
      msg_print("You can't use your powers yet.");
      return;
    }
  }

  /* Enter "icky" mode */
  character_icky = TRUE;

  /* Save the screen */
  Term_save();


  /* Interact until done */
  while (1) {
    /* Clear screen */
    Term_clear();

    /* Ask for a choice */
    prt("Do what special action:", 2, 0);

    /* Give some choices */
    prt("(1) Teleport to a specific place.", 4, 5);
    prt("(2) Fetch an item.", 5, 5);
    prt("(3) Go up 50'", 6, 5);
    prt("(4) Go down 50'", 7, 5);

    /* Prompt */
    prt("Command: ", 9, 0);

    /* Prompt */
    i = inkey();

    /* Done */
    if (i == ESCAPE) break;

    /* Tele-to */
    if (i == '1') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!target_set(TARGET_GRID)) break;

      /* Teleport to the target */
      teleport_player_to(p_ptr->target_row, p_ptr->target_col); 

      did_act = TRUE;
      break;
    }

    /* Fetch item */
    else if (i == '2') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!target_set(TARGET_GRID)) return;

      if (!fetch_item(p_ptr->lev*15, -1, -1)) return;

      did_act = TRUE;
      break;
    }

    /* Move up */
    else if (i == '3') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!tport_vertically(FALSE)) return;

      did_act = TRUE;
      break;
    }

    /* Move down */
    else if (i == '4') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!tport_vertically(TRUE)) return;

      did_act = TRUE;
      break;
    }

    /* Unknown option */
    else {
      bell();
    }

  }

  /* Check if screen was restored before */
  if (!did_load) {
    /* Restore the screen */
    Term_load();

    /* Leave "icky" mode */
    character_icky = FALSE;
  }

  /* Apply stat losses if something was done */
  if (did_act) {
    p_ptr->immov_cntr += 101-(p_ptr->lev*2);

    if (lose_sp) {
      p_ptr->csp -= lose_sp;
      p_ptr->redraw |= (PR_MANA);
    }

    if (lose_hp) {
      p_ptr->chp -= lose_hp;
      p_ptr->redraw |= (PR_HP);
    }
  }
}


/*
 * Figure out your shape-change chance.
 */

static int get_mimic_change_chance(int chance) {
  chance -= (p_ptr->lev*3);
  chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

  if (chance < 2) chance = 2;

  /* Stunning makes spells harder */
  if (p_ptr->stun > 50) chance += 25;
  else if (p_ptr->stun) chance += 15;

  /* Always a 5 percent chance of working */
  if (chance > 95) chance = 95;

  /* Return the chance */
  return (chance);
}

/* 
 * Change your shape, using mimic books. Change into abomination
 * when failed.
 */

void do_cmd_change_shape(void) {

  int item, chance;
  object_type *o_ptr;     
  cptr q, s;

  if (cp_ptr->spell_book != TV_MIMIC_BOOK) {
    mprint(MSG_TEMP, "You do not have shape-shifting abilities.");
    return;
  }

  if (p_ptr->blind || no_lite()) {
    mprint(MSG_TEMP, "You cannot see!");
    return;
  }

  if (p_ptr->confused) {
    mprint(MSG_TEMP, "You are too confused!");
    return;
  }

  item_tester_tval = TV_MIMIC_BOOK;
  
  /* Get an item */
  q = "Use which book? ";
  s = "You have no books of lore!";
  if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
  
  /* Get the item (in the pack) */
  if (item >= 0) {
    o_ptr = &inventory[item];
  }

  /* Get the item (on the floor) */
  else {
    o_ptr = &o_list[0 - item];
  }
  
  /* Track the object kind */
  object_kind_track(o_ptr->k_idx);

  /* Hack -- Handle stuff */
  handle_stuff();

  chance = get_mimic_change_chance(o_ptr->pval);

  if (chance > 75) {
    mprint(MSG_WARNING, "You feel uneasy with this shape-change.");
    if (!get_check("Try it anyway? ")) {
      return;
    }
  }

  if (randint(100) < chance) {
    mprint(MSG_WARNING, "Your shape-change goes horribly wrong!");

    if (randint(100) < p_ptr->skill_sav) {
      msg_print("You manage to wrest your body back under control.");
    } else {
      change_shape(SHAPE_ABOMINATION);
    }
  } else {
    change_shape(o_ptr->sval);
  }
}
  
/* Front-end to spell casting commands */

void do_cmd_cast_helper(void) {
  if (p_ptr->pclass == CLASS_BEASTMASTER || p_ptr->shape == SHAPE_QUYL) {
    summon_pet_monster();
  } else if (p_ptr->pclass == CLASS_LYCANTH) {
    change_shape(SHAPE_WOLF);
  } else if (p_ptr->pclass == CLASS_MIMIC) {
    do_cmd_change_shape();
  } else {
    do_cmd_cast_power();
  }
}

/*
 * Command to ask favors from your god.
 */

void do_cmd_pray(void) {
  int level;
  cptr name;

  if (p_ptr->pgod == 0) {
    mprint(MSG_TEMP, "Pray hard enough and your prayers might be answered.");
    return;
  }
  
  if (confirm_prayers && !get_check("Are you sure you want to disturb your God? ")) return;

  level = interpret_grace() - interpret_favor();
  name = deity_info[p_ptr->pgod-1].name;

  if (p_ptr->pclass == CLASS_PRIEST && magik(30)) {
    level++;
  }

  if (p_ptr->pclass == CLASS_PALADIN && magik(10)) {
    level++;
  }

  if (level < 0) level = 0;
  if (level > 10) level = 10;

  p_ptr->energy_use = 100;

  switch (level) {
  case 10:
    mformat(MSG_BIG_BONUS, "%s thunders: ``Thou hast pleaseth me, mortal.''", name);
    great_side_effect();
    break;

  case 9:
    mformat(MSG_BIG_BONUS, "%s booms out: ``Thy deeds earn thou a worthy reward.''", name);
    good_side_effect();
    break;

  case 8:
    mformat(MSG_BONUS, "%s thunders: ``Well done, motal.''", name);
    ok_side_effect();
    break;

  case 7:
  case 6:
    mformat(MSG_STUPID, "%s hisses: ``Thou shouldst not have done that, mortal!''", name);
    neutral_side_effect();
    if (magik(30)) set_grace(p_ptr->grace - 1000);
    break;

  case 5:
  case 4:
  case 3:
    mformat(MSG_URGENT, "%s quakes in rage: ``Thou art supremely insolent, mortal!''", name);
    nasty_side_effect();
    set_grace(p_ptr->grace - 5000);
    break;

  case 2:
  case 1:
  case 0:
    mformat(MSG_DEADLY, "%s whipsers: ``Prepare to die, mortal...''", name);
    deadly_side_effect(TRUE);
    set_grace(p_ptr->grace - 20000);
    break;
  }

  p_ptr->god_favor = 25000;
}

