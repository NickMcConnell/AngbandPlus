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
	int             item, ident, lev;

	object_type     *o_ptr;


	/* Restrict choices to food */
	item_tester_tval = TV_FOOD;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Eat which item? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to eat.");
		return;
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
				if (set_poisoned(p_ptr->poisoned + rand_int(20) + 20 + (p_ptr->lev * 2)))
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
				if (set_blind(p_ptr->blind + rand_int(200) + 200 + (p_ptr->lev * 2)))
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
				if (set_afraid(p_ptr->afraid + rand_int(20) + 20 + (p_ptr->lev * 2)))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (!p_ptr->resist_conf)
			{
				if (set_confused(p_ptr->confused + rand_int(20) + 20 + (p_ptr->lev * 2)))
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
				if (set_image(p_ptr->image + rand_int(150) + 150))
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
				if (set_paralyzed(p_ptr->paralyzed + rand_int(15) + 15 + (p_ptr->lev * 2)))
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
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
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
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
			msg_print("That tastes good.");
			(void)set_poisoned(0);
			(void)hp_player(damroll(4, 8));
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
	if (p_ptr->prace == RACE_VAMPIRE)
	{
		/* Reduced nutritional benefit */
		(void)set_food(p_ptr->food + (o_ptr->pval / 10));
		msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
			msg_print("Your hunger can only be satisfied with fresh blood!");
	}
	else if ((p_ptr->prace == RACE_GOLEM) ||
		 (p_ptr->prace == RACE_SPECTRE))
	{
		msg_print("The food of mortals is poor sustenance for you.");
		set_food(p_ptr->food + ((o_ptr->pval) / 20));
	}
	else
	{
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
void do_cmd_quaff_potion(void)
{
	int		item, ident, lev;
	object_type	*o_ptr;

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Quaff which potion? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no potions to quaff.");
		return;
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

	/* Sound */
	sound(SOUND_QUAFF);

	/* Take a turn */
	energy_use = 100;

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
			if (!p_ptr->free_act)
			{
				if (set_slow(p_ptr->slow + randint(50) + 25 + (p_ptr->lev * 2)))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			msg_print("The potion makes you vomit!");
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
				if (set_poisoned(p_ptr->poisoned + rand_int(25) + 20 + (p_ptr->lev * 2)))
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
				if (set_blind(p_ptr->blind + rand_int(100) + 100 + (p_ptr->lev * 2)))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION: /* Booze */
		{
			if (!p_ptr->resist_conf)
			{
				if (set_confused(p_ptr->confused + rand_int(25) + 25 + (p_ptr->lev * 2)))
				{
					ident = TRUE;
				}

				if (randint(2)==1 && !p_ptr->resist_chaos)
				{
					if (set_image(p_ptr->image + rand_int(150) + 150))
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
				if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10 + (p_ptr->lev * 2)))
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
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			msg_print("A feeling of Death flows through your body.");
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
			if (set_tim_invis(p_ptr->tim_invis + 25 + randint(50)))
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
			if (set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint(25) + 25)) ident = TRUE;
			if (hp_player(10)) ident = TRUE;
			break;
		}

		case SV_POTION_BESERK_STRENGTH:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_shero(p_ptr->shero + randint(25) + 25)) ident = TRUE;
			if (hp_player(30)) ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
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

		case SV_POTION_CURE_CRITICAL:
		{
			if (hp_player(damroll(8, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
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
			if (set_image(0)) ident = TRUE;
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
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);
			restore_level();
			hp_player(5000);
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
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

		case SV_POTION_MUTATION:
		{
			if ((p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3) &&
			    (randint(2) == 1))
			{
				lose_mutation(0);
				ident = TRUE;
				break;
			}
			else
			{
				if (wizard)
				{
					char	ppp[80];
					char	tmp_val[160];
					int	k = 181;

					sprintf(ppp, "Gain which mutation? (1-%d, 0 for random): ", k);
					sprintf(tmp_val, "%d", k);
					if (!get_string(ppp, tmp_val, 10)) return;
					k = atoi(tmp_val);
					gain_random_mutation(k);
				}
				else
				{
					gain_random_mutation(0);
				}
				ident = TRUE;
				break;
			}
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
			(void)detect_monsters_normal();
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
			identify_pack();
			ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				if (p_ptr->exp <= 1000)
				{
					msg_print("You feel more experienced.");
					gain_exp(200);
					ident = TRUE;
				}
				else if (p_ptr->exp <= 10000)
				{
					msg_print("You feel more experienced.");
					gain_exp(1000);
					ident = TRUE;
				}
				else if (p_ptr->exp <= 25000)
				{
					msg_print("You feel more experienced.");
					gain_exp(5000);
					ident = TRUE;
				}
				else if (p_ptr->exp <= 100000L)
				{
					msg_print("You feel more experienced.");
					gain_exp(25000);
					ident = TRUE;
				}
				else if (p_ptr->exp <= 1000000L)
				{
					msg_print("You feel more experienced.");
					gain_exp(100000L);
					ident = TRUE;
				}
				else if (p_ptr->exp >= 1000001L)
				{
					msg_print("You feel more experienced.");
					gain_exp(250000L);
					ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_RESISTANCE:
		{
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			ident = TRUE;
			break;
		}

		case SV_POTION_CURING:
		{
			if (hp_player(damroll(12, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_POTION_INVULNERABILITY:
		{
			(void)set_invuln(p_ptr->invuln + randint(10) + 10);
			ident = TRUE;
			break;
		}

		case SV_POTION_NEW_LIFE:
		{
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
				msg_print("You are cured of all mutations.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			do_poly_self();
			ident = TRUE;
			break;
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
	if (((o_ptr->art_name) || artifact_p(o_ptr)) && (rand_int(100) < 80))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your armor", o_name);
	}
	else /* not artifact or failed save... */
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

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
	if (((o_ptr->art_name) || artifact_p(o_ptr)) && (rand_int(100) < 80))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
		           "terrible black aura", "surround your weapon", o_name);
	}
	else /* not artifact or failed save... */
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
		o_ptr->art_flags1 = 0;
		o_ptr->art_flags2 = 0;
		o_ptr->art_flags3 = 0;

		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);

		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);

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
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(void)
{
	int			item, k, used_up, ident, lev;

	object_type		*o_ptr;

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

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Read which scroll? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no scrolls to read.");
		return;
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


	/* Take a turn */
	energy_use = 100;

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
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(1, FALSE);
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
				if (summon_specific(py, px, dun_level, 0))
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
				if (summon_specific(py, px, dun_level, SUMMON_UNDEAD))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_PET:
		{
			if (summon_specific_friendly(py, px, dun_level, SUMMON_NO_UNIQUES, TRUE))
			{
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
			/* Astral beings don't WoR! -- Gumby */
			if (p_ptr->astral)
			{
				msg_print("You feel a terrible sense of loss.");
				ident = TRUE;
				break;
			}

			if (dun_level && (p_ptr->max_dlv > dun_level))
			{
				if (get_check("Reset recall depth? "))
					p_ptr->max_dlv = dun_level;
			}

			if (p_ptr->word_recall == 0)
			{
				p_ptr->word_recall = randint(20) + 15;
				msg_print("The air about you becomes charged...");
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
				msg_print("You feel as if someone is watching over you.");
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			if (remove_all_curse())
				msg_print("You feel as if someone is closely watching over you.");
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

		case SV_SCROLL_PROTECT_ARMOUR:
		{
			ident = TRUE;
			if (!rustproof()) used_up = FALSE;
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
			if (!recharge(75)) used_up = FALSE;
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

		case SV_SCROLL_SET_RECALL:
		{
			command_arg = p_ptr->max_dlv;

			/* Ask for level */
			if (get_check("Change recall level? "))
			{
				char	ppp[80];
				char	tmp_val[160];

				/* Prompt */
				sprintf(ppp, "Set recall (cur. %d) to level (1-%d): ", p_ptr->max_dlv, 97);

				/* Default */
				sprintf(tmp_val, "%d", dun_level);

				/* Ask for a level */
				if (!get_string(ppp, tmp_val, 10)) return;

				/* Extract request */
				command_arg = atoi(tmp_val);
			}

			/* Paranoia */
			if (command_arg < 1) command_arg = 1;

			/* Paranoia */
			if (command_arg > 97) command_arg = 97;

			/* Accept request */
			msg_format("Your next recall will be to dungeon level %d.", command_arg);

			/* Change recall depth */
			p_ptr->max_dlv = command_arg;

			ident = TRUE;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (set_blessed(p_ptr->blessed + randint(20) + 10)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (set_blessed(p_ptr->blessed + randint(40) + 20)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_blessed(p_ptr->blessed + randint(60) + 30)) ident = TRUE;
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
			if (set_protevil(p_ptr->protevil + randint(25) + (2 * p_ptr->lev))) ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			glyph_creation();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_ANIMALS:
		{
			if (dispel_animals(100)) ident = TRUE;
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
			if (dispel_undead(100)) ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESS_WEAPON:
		{
			if (!bless_weapon()) used_up = FALSE;
			ident = TRUE;
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
			fire_ball(GF_FIRE, 0, 175, 4);
			ident = TRUE;
			break;
		}


		case SV_SCROLL_ICE:
		{
			fire_ball(GF_ICE, 0, 200, 4);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CHAOS:
		{
			fire_ball(GF_CHAOS, 0, 225, 4);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RUMOR:
		{
			msg_print("There is message on the scroll. It says:");
			msg_print(NULL);
			switch(randint(20))
			{
				case 1:
					get_rnd_line("chainswd.txt", Rumor);
					break;
				case 2:
					get_rnd_line("error.txt", Rumor);
					break;
				case 3:
				case 4:
				case 5:
					get_rnd_line("death.txt", Rumor);
					break;
				default:
					get_rnd_line("rumors.txt", Rumor);
			}
			msg_format("%s", Rumor);
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ARTIFACT:
		{
			ident = TRUE;
			if (!artifact_scroll()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_BRAND_WEAPON:
		{
			if (!brand_weapon(rand_int(5))) used_up = FALSE;
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
void do_cmd_use_staff(void)
{
	int			item, ident, chance, k, lev;

	object_type		*o_ptr;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Use which staff? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no staff to use.");
		return;
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
				if (summon_specific(py, px, dun_level, 0))
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
			if (lite_area(damroll(3, 8), 2)) ident = TRUE;
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

		case SV_STAFF_DETECT_MONS:
		{
			if (detect_monsters_normal()) ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (hp_player(damroll(16, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
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
			if (dispel_evil(150)) ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(200)) ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(200)) ident = TRUE;
			if (set_protevil(p_ptr->protevil + randint(25) + (2 * p_ptr->lev))) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
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
			destroy_area(py, px, 15, TRUE);
			ident = TRUE;
			break;
		}

		case SV_STAFF_ALTERATION:
		{
			do_poly_self();
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
	int		item, lev, ident, chance, dir, sval;
	object_type	*o_ptr;

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Aim which wand? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no wand to aim.");
		return;
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

	/* Mega-Hack -- refuse to aim a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the wands.");
		return;
	}

	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;

	/* Take a turn */
	energy_use = 100;

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
			if (drain_life(dir, 125)) ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 20, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(3, 6));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(8, 8));
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
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(11, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(10, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 75, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 85, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 110, 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 100, 2);
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
			fire_ball(GF_FIRE, dir, 150, 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 150, 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 150, 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 150, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 150, 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 150, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 150, 3);
					break;
				}
			}

			ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 200)) ident = TRUE;
			break;
		}

		case SV_WAND_ROCKETS:
		{
			msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir, 150 + (randint(50)), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_STRIKING:
		{
			fire_bolt(GF_FORCE, dir, damroll(8, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_APPORTATION:
		{
			fetch(dir, p_ptr->lev * 10, FALSE);
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
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Use a single charge */
	o_ptr->pval--;

	/* Hack -- unstack if necessary */
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
		msg_print("You unstack your wand.");
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
	int                 item, ident, chance, dir, lev;

	object_type		*o_ptr;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Zap which rod? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no rod to zap.");
		return;
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


	/* Mega-Hack -- refuse to zap a pile from the ground */
	if ((item < 0) && (o_ptr->number > 1))
	{
		msg_print("You must first pick up the rods.");
		return;
	}


	/* Get a direction (unless KNOWN not to need it) */
	if (((o_ptr->sval >= SV_ROD_MIN_DIRECTION) && !(o_ptr->sval == SV_ROD_HAVOC)) ||
	     !object_aware_p(o_ptr))
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
			o_ptr->pval = (10 + randint(10));
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			o_ptr->pval = 50;
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
			/* Astral beings don't WoR! -- G */
			if (p_ptr->astral)
			{
				msg_print("You feel a terrible sense of loss.");
				ident = TRUE;
				break;
			}

			if (dun_level && (p_ptr->max_dlv > dun_level))
			{
				if (get_check("Reset recall depth? "))
					p_ptr->max_dlv = dun_level;
			}
            
			if (p_ptr->word_recall == 0)
			{
				msg_print("The air about you becomes charged...");
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
			if (lite_area(damroll(3, 8), 2)) ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_MAPPING:
		{
			map_area();
			ident = TRUE;
			o_ptr->pval = 20;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all();
			ident = TRUE;
			o_ptr->pval = 50;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_CURING:
		{
			if (hp_player(damroll(16, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			o_ptr->pval = 50;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			o_ptr->pval = 150;
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
			o_ptr->pval = 200;
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
			o_ptr->pval = 100;
			break;
		}

		case SV_ROD_PESTICIDE:
		{
			msg_print("You spray some pesticide into the air...");
			dispel_animals(5);
			ident = TRUE;
			o_ptr->pval = randint(2) + 2;
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			o_ptr->pval = 15;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			o_ptr->pval = 15;
			break;
		}

		case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 100)) ident = TRUE;
			o_ptr->pval = 20;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			o_ptr->pval = 20;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(7, 8));
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(8, 8));
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(10, 8));
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(9, 8));
			ident = TRUE;
			o_ptr->pval = 10;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 65, 2);
			ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 75, 2);
			ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 100, 2);
			ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 85, 2);
			ident = TRUE;
			o_ptr->pval = 25;
			break;
		}

		case SV_ROD_HAVOC:
		{
			call_chaos();
			ident = TRUE;
			o_ptr->pval = 100;
			break;
		}

		case SV_ROD_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 20, 2);
			ident = TRUE;
			o_ptr->pval = 15;
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
