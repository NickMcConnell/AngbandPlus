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
 * Determine the effects of eating a corpse. A corpse can be
 * eaten whole or cut into pieces for later.
 */
static void corpse_effect(object_type *o_ptr, bool cutting)
{
        monster_race *r_ptr = &r_info[o_ptr->pval2];

	/* Assume no bad effects */
	bool harmful = FALSE;

	byte method, effect, d_dice, d_side;

        int i, dam, idam = 0, mdam, brpow, brdam = 0;

	/* How much of the monster's breath attack remains */
        brpow = (o_ptr->pval > r_ptr->weight ? (o_ptr->pval - r_ptr->weight) / 5 : 0);
        brpow = (brpow > r_ptr->weight / 5 ? r_ptr->weight / 5 : brpow);

        if(o_ptr->weight <= 0) o_ptr->weight = 1;
        if(o_ptr->pval <= 0) o_ptr->pval = 1;
}

/*
 * Hook to determine if an object is eatable
 */
static bool item_tester_hook_eatable(object_type *o_ptr)
{
        if ((o_ptr->tval==TV_FOOD) || (o_ptr->tval==TV_CORPSE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
        int             item, ident, lev, fval = 0;

	object_type     *o_ptr;

        monster_race* r_ptr;

	cptr q, s;

        bool destroy = TRUE;

        /* Skeletons doN't have to eat at all! */
        if (p_ptr->prace == RACE_SKELETON)
        {
                msg_print("As a skeleton, you can't eat!");
                return;
        }

        /* Restrict choices to food */
        item_tester_hook = item_tester_hook_eatable;

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
	energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

#ifdef USE_PYTHON
        if (perform_event(EVENT_EAT_FOOD, Py_BuildValue("(ii)", o_ptr->tval, o_ptr->sval))) return;
#endif

	/* Analyze the food */
        if(o_ptr->tval==TV_FOOD){
	switch (o_ptr->sval)
	{
		case SV_FOOD_POISON:
		{
			if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
			{
				ident = TRUE;
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
			if (!p_ptr->resist_conf)
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
				if (set_image(p_ptr->image + rand_int(250) + 250))
				{
					ident = TRUE;
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
			(void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food.");
			(void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_STUPIDITY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_INT, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_NAIVETY:
		{
			take_hit(damroll(8, 8), "poisonous food.");
			(void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_CON, STAT_DEC_NORMAL);
			ident = TRUE;
			break;
		}

		case SV_FOOD_DISEASE:
		{
			take_hit(damroll(10, 10), "poisonous food.");
			(void)do_dec_stat(A_STR, STAT_DEC_NORMAL);
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
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
                        msg_print("That tastes very good.");
			(void)set_poisoned(0);
			(void)hp_player(damroll(4, 8));
                        set_food(PY_FOOD_MAX - 1);
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

                case SV_FOOD_ATHELAS:
                {
                        msg_print("A fresh, clean essence rises, driving away wounds and poison.");
                        (void)set_poisoned(0);
                        (void)set_stun(0);
                        (void)set_cut(0);
                        ident = TRUE;
                        break;
                }
	}
        }
        else{
                r_ptr = &r_info[o_ptr->pval2];
		switch (o_ptr->sval)
      {
        case SV_CORPSE_CORPSE:
      	{
         	/* Not all is edible. Apologies if messy. */
                if (((r_ptr->flags9 & RF9_DROP_SKELETON) && (o_ptr->weight <= (r_ptr->weight * 3) / 5)) ||
                                        (!(r_ptr->flags9 & RF9_DROP_SKELETON) && (o_ptr->weight <= (r_ptr->weight * 7) / 20)))
         	{
         		msg_print("There is not enough meat.");
            	return;
         	}
				if (!o_ptr->timeout) msg_print("Ugh! Raw meat!");
				else msg_print("That tastes good.");

				/* A pound of raw meat */
				o_ptr->pval -= 10;
                                o_ptr->weight -= 10;

				/* Corpses still have meat on them */
				destroy = FALSE;

				ident = TRUE;
				break;
			}
                        case SV_CORPSE_HEAD:
			{
				msg_print("You feel rather sick.");

				/* A pound of raw meat */
				o_ptr->pval -= 10;
                                o_ptr->weight -= 10;

				/* Corpses still have meat on them */
				destroy = FALSE;

				ident = TRUE;
				break;
			}
                        case SV_CORPSE_MEAT:
			{
				/* Just meat */
				if (!o_ptr->timeout) msg_print("You hurriedly swallow the meat.");
				else msg_print("That tastes good.");

				ident = TRUE;

				/* Those darn microorganisms */
                                if (!o_ptr->timeout && (o_ptr->weight > o_ptr->pval))
				{
                                        set_poisoned(p_ptr->poisoned + rand_int(o_ptr->weight - o_ptr->pval) +
                                        (o_ptr->weight - o_ptr->pval));
				}
				break;
			}
		}

		corpse_effect(o_ptr, FALSE);

		/* Less nutritious than food rations, but much more of it. */
		fval = (o_ptr->timeout) ? 2000 : 2500;

		/* Those darn microorganisms */
                if (!o_ptr->timeout && (o_ptr->weight - o_ptr->pval > 10))
		{
                        set_poisoned(p_ptr->poisoned + rand_int(o_ptr->weight - o_ptr->pval) +
                        (o_ptr->weight - o_ptr->pval));
		}

		/* Partially cured */
                if (o_ptr->weight > o_ptr->timeout)
		{
			/* Adjust the "timeout" without overflowing */
                        o_ptr->timeout = (o_ptr->timeout * ((100 * o_ptr->timeout) / o_ptr->weight)) / 100;
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

        if(!fval) fval = o_ptr->pval;

	/* Food can feed the player */
        if ((p_ptr->prace == RACE_VAMPIRE))
	{
                /* No effect */
                msg_print("Normal food has no effect on you.");
		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
			msg_print("Your hunger can only be satisfied with fresh blood!");
	}
	else
	{
                (void)set_food(p_ptr->food + fval);
	}


	/* Destroy a food in the pack */
        if(destroy)
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
 * Cut a corpse up for convenient storage
 */
void do_cmd_cut_corpse(void)
{
        int item, meat = 0, not_meat = 0;

	object_type *o_ptr;
	object_type *i_ptr;

	object_type object_type_body;

	monster_race *r_ptr;

	cptr q, s;


	/* Restrict choices to corpses */
        item_tester_tval = TV_CORPSE;

	/* Get an item */
	q = "Hack up which corpse? ";
	s = "You have no corpses.";
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

        r_ptr = &r_info[o_ptr->pval2];

        if (o_ptr->sval != SV_CORPSE_CORPSE && o_ptr->sval != SV_CORPSE_HEAD)
        {
		msg_print ("You cannot split that.");
		return;
	}

	switch (o_ptr->sval)
        {
                case SV_CORPSE_CORPSE:
                {
                        if (r_ptr->flags9 & RF9_DROP_SKELETON)
			{
                               not_meat = (r_ptr->weight * 3) / 5;
			}
                        else
			{
                               not_meat = (r_ptr->weight * 7) / 20;
			}
                        meat = r_ptr->weight + r_ptr->weight / 10 - not_meat;
                        break;
                }
      case SV_CORPSE_HEAD:
      {
                                not_meat = r_ptr->weight / 150;
         meat = r_ptr->weight / 30 + r_ptr->weight / 300 - not_meat;
         break;
      }
   }

   if (o_ptr->weight <= not_meat || meat < 10)
   {
		msg_print("There is not enough meat.");
		return;
	}

	/* Hacking 10 pounds off */
   if (meat > 100) meat = 100;

	/* Take a turn */
        energy_use = 100;

   o_ptr->pval -= meat;
        o_ptr->weight -= meat;

	msg_print("You hack some meat off the corpse.");

	corpse_effect(o_ptr, TRUE);

	/* Get local object */
	i_ptr = &object_type_body;

   /* Make some meat */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_MEAT));

	i_ptr->number = meat / 10;
        i_ptr->pval2 = o_ptr->pval2;

   /* Length of time before decay */
	i_ptr->pval = 1000 + rand_int(1000);

	if (inven_carry_okay(i_ptr))
	{
                inven_carry(i_ptr,TRUE);
	}
	else
	{
                drop_near(i_ptr, 0, py, px);
	}
}


/*
 * Use a potion to cure some meat
 *
 * Salt water works well.
 */
void do_cmd_cure_meat(void)
{
	int item, num, cure;

	object_type *o_ptr;
	object_type *i_ptr;

	cptr q, s;

	/* Restrict choices to corpses */
        item_tester_tval = TV_CORPSE;
        item_tester_hook = item_tester_hook_eatable;

	/* Get some meat */
	q = "Cure which meat? ";
	s = "You have no meat to cure.";
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

	/* Restrict choices to potions */
	item_tester_tval = TV_POTION;

	/* Get a potion */
	q = "Use which potion? ";
	s = "You have no potions to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		i_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		i_ptr = &o_list[0 - item];
	}

	if (i_ptr->number > 1)
	{
		/* Get a number */
		get_count(1, i_ptr->number);

		/* Save it */
                num = command_arg;
	}
	else
	{
		num = 1;
	}

	if (num == 0) return;

	/* Take a turn */
        energy_use = 100;

	q = "You soak the meat.";
	s = "You soak the meat.";

	switch (i_ptr->sval)
	{
		case SV_POTION_SALT_WATER:
		{
			q = "You salt the meat.";
			cure = 200 * num;
			break;
		}
		case SV_POTION_POISON:
		{
			q = "You poison the meat.";
			cure = 0;
			o_ptr->pval /= 2;
                        if (o_ptr->pval > o_ptr->weight) o_ptr->pval = o_ptr->weight;

			break;
		}
                case SV_POTION_CONFUSION:
		{
			cure = 80 * num;
			break;
		}
		case SV_POTION_SLOW_POISON:
		{
			cure = 20 * num;
			break;
		}
		case SV_POTION_CURE_POISON:
		{
			cure = 45 * num;
			break;
		}
		case SV_POTION_DEATH:
		{
			q = "You ruin the meat.";
			cure = 0;
			o_ptr->pval /= 10;
                        if (o_ptr->pval > o_ptr->weight) o_ptr->pval = o_ptr->weight / 2;

			break;
		}
		default:
		{
			cure = 0;
			break;
		}
	}

	/* Message */
	if (object_known_p(i_ptr)) msg_print(q);
	else msg_print(s);

	/* The meat is already spoiling */
        if (((o_ptr->sval == SV_CORPSE_MEAT) && (o_ptr->weight > o_ptr->pval)) ||
                 o_ptr->weight - o_ptr->pval > 10)
	{
                cure = (cure * o_ptr->pval) / (o_ptr->weight * 20);
	}

	/* Cure the meat */
	o_ptr->timeout += cure / o_ptr->number;

	if (o_ptr->timeout > o_ptr->pval) o_ptr->timeout = o_ptr->pval;

	/* Use up the potions in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, 0 - num);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Use up the potions on the floor */
	else
	{
		floor_item_increase(0 - item, 0 - num);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}


/*
 * Hook to determine if an object is quaffable
 */
static bool item_tester_hook_quaffable(object_type *o_ptr)
{
        if ((o_ptr->tval==TV_POTION)||(o_ptr->tval==TV_POTION2)) return (TRUE);

	/* Assume not */
	return (FALSE);
}
/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
	int		item, ident, lev;

	object_type	*o_ptr;
        object_type     *q_ptr,forge;

	cptr q, s;

	/* Restrict choices to potions */
        item_tester_hook = item_tester_hook_quaffable;

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
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

#ifdef USE_PYTHON
        if (perform_event(EVENT_QUAFF_POTION, Py_BuildValue("(ii)", o_ptr->tval, o_ptr->sval))) return;
#endif

	/* Analyze the potion */
        if(o_ptr->tval==TV_POTION)
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
			if (set_slow(p_ptr->slow + randint(25) + 15)) ident = TRUE;
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
			if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
			{
				ident = TRUE;
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

		case SV_POTION_CONFUSION: /* Booze */
		{
			if (!((p_ptr->resist_conf)))
			{
				if (set_confused(p_ptr->confused + rand_int(20) + 15))
				{
					ident = TRUE;
				}
				if (randint(2)==1)
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
				msg_print("You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(500, "a potion of Ruination");
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
			if (do_dec_stat(A_STR, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR, STAT_DEC_NORMAL)) ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
                        take_hit((300 * p_ptr->skill[10]), "a potion of Detonation");
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
			if (set_tim_infra(p_ptr->tim_infra + 100 + randint(100) + p_ptr->skill[10]))
			{
				ident = TRUE;
			}
                        
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (set_tim_invis(p_ptr->tim_invis + 12 + randint(12) + p_ptr->skill[10]))
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

		case SV_POTION_HEROISM:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint(25) + 25 + p_ptr->skill[10])) ident = TRUE;
                        
			break;
		}

		case SV_POTION_BESERK_STRENGTH:
		{
			if (set_afraid(0)) ident = TRUE;
			if (set_shero(p_ptr->shero + randint(25) + 25 + p_ptr->skill[10])) ident = TRUE;
                        
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
                        if (hp_player(damroll(2, 8) * (p_ptr->skill[10]+1))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
                        
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
                        if (hp_player(damroll(4, 10) * (p_ptr->skill[10]+1))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
                        
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
                        if (hp_player(damroll(6, 12) * (p_ptr->skill[10]+1))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
                        
			break;
		}

		case SV_POTION_HEALING:
		{
                        if (hp_player(300) * (p_ptr->skill[10]+1)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
                        
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
                        if (hp_player(1200) * (p_ptr->skill[10]+1)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
                        
			break;
		}

		case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
			restore_level();
                        hp_player(5000 * (p_ptr->skill[10]+1));
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
		case SV_POTION_CURE_MUTATIONS:
		{
			msg_print("Your body returns to it's original self!");
                        p_ptr->stat_mut[A_STR] = 0;
			p_ptr->stat_mut[A_INT] = 0;
			p_ptr->stat_mut[A_WIS] = 0;
			p_ptr->stat_mut[A_DEX] = 0;
			p_ptr->stat_mut[A_CON] = 0;
			p_ptr->stat_mut[A_CHR] = 0;
			p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
			p_ptr->update |= PU_BONUS;
			update_and_handle();
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
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			ident = TRUE;
                        
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			msg_print(NULL);
                        wiz_lite_extra();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
                        self_knowledge(NULL);
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

		case SV_POTION_CURING:
		{
                        if (hp_player(50) * (p_ptr->skill[10] +1)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
                        
			break;
		}

                case SV_POTION_STONESKIN:
		{
                        p_ptr->ac_boost = 300 * (p_ptr->skill[10] + 1);
                        (void)set_ac_boost(50 + p_ptr->skill[10]);
                        ident = TRUE;
                        
			break;
		}
                case SV_POTION_FULL_RESTORE:
                      {
                        msg_print("You are completely healed!");
                        p_ptr->chp = p_ptr->mhp;
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        update_and_handle();
                        ident = TRUE;
                        
                        break;
                      }
                case SV_POTION_MUTATION:
                {
                        msg_print("You fell a change coming over you !");
                        gain_random_mutation(0);
                        ident = TRUE;
                        
                        break;
                }
                case SV_POTION_INVIS:
		{
			int t = 30 + randint(30);

                        if (set_invis(p_ptr->tim_invis + t + p_ptr->skill[10], 35))
			{
				ident = TRUE;
			}
                        set_tim_invis(p_ptr->tim_invis + t + p_ptr->skill[10]);
                        
			break;
		}
                case SV_POTION_STAR_MUTATION:
                {
                        msg_print("You fell a couple of changes coming over you !");
                        gain_random_mutation(0);
                        gain_random_mutation(0);
                        gain_random_mutation(0);
                        gain_random_mutation(0);
                        ident = TRUE;
                        
                        break;
                }

                case SV_POTION_SEX_CHANGING:
                {
                        if (p_ptr->psex == SEX_FEMALE)
			{
				msg_print("Your sex has changed! You are now a male!");
				p_ptr->psex = SEX_MALE;
			}
			else
			{
				msg_print("Your sex has changed! You are now a female!");
				p_ptr->psex = SEX_FEMALE;
			}              
                        break;
                }

                case SV_POTION_POWER:
                {
                        
                        msg_print("You feel that your health greatly improved!");
                        p_ptr->mhp += 300;
                        p_ptr->update |= PU_BONUS;
                        p_ptr->update |= PU_HP;
                        break;
                }
                case SV_POTION_MOLOTOV:
		{
                        msg_print("Explosions rupture your body!");
                        take_hit((30 * p_ptr->skill[10]), "a potion of Molotov Cocktail");
                        (void)set_stun(p_ptr->stun + 30);
                        (void)set_cut(p_ptr->cut + 100);
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
	if (((o_ptr->art_name) || artifact_p(o_ptr)) && (rand_int(100) < 50))
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
                o_ptr->art_flags4 = 0;

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
	if ((artifact_p(o_ptr) || o_ptr->art_name) && (rand_int(100) < 50))
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
                o_ptr->art_flags4 = 0;


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

	/* Notice */
	return (TRUE);
}


/*
 * Hook to determine if an object is readable
 */
static bool item_tester_hook_readable(object_type *o_ptr)
{
        if ((o_ptr->tval==TV_SCROLL)||(o_ptr->tval==TV_PARCHEMENT)) return (TRUE);

	/* Assume not */
	return (FALSE);
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

        int                     item, k, used_up, ident, lev;

	object_type		*o_ptr;
        object_type     *q_ptr,forge;

        char  Rumor[80];

	cptr q, s;

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
        item_tester_hook = item_tester_hook_readable;

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
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = k_info[o_ptr->k_idx].level;

	/* Assume the scroll will get used up */
	used_up = TRUE;

#ifdef USE_PYTHON
        if (perform_event(EVENT_READ_SCROLL, Py_BuildValue("(ii)", o_ptr->tval, o_ptr->sval))) return;
#endif

	/* Analyze the scroll */
        if(o_ptr->tval==TV_SCROLL){
	switch (o_ptr->sval)
	{
                break;
                case SV_SCROLL_RESET_RECALL:
                {
                        char buf[80], buf2[80];
                        int i, amt;

                        if(!get_string("Reset to which dungeon? ", buf, 80)) break;

                        /* Find the index corresponding to the name */
                        for(i = 1; i < max_d_idx; i++)
                        {
                                sprintf(buf2, "%s", d_info[i].name + d_name);

                                /* Lowercase the name */
                                strlower(buf);
                                strlower(buf2);

                                if(strstr(buf2, buf)) break;
                        }

                        amt = get_quantity(format("Reset to which level(%d-%d)? ", d_info[i].mindepth, d_info[i].maxdepth) , d_info[i].maxdepth);

                        /* Mega hack -- Forbid levels 99 and 100 */
                        if((amt == 99) || (amt == 100)) amt = 98;

                        if ((amt > d_info[i].mindepth) && (i < max_d_idx))
			{
                                p_ptr->recall_dungeon = i;
                                max_dlv[p_ptr->recall_dungeon] = amt;
                                msg_format("Recall reseted to %s at level %d.", d_info[i].name + d_name, amt);
			}
                        else
                                msg_print("Recall NOT reseted(bad dungeon or level).");
			break;
                }
		case SV_SCROLL_DARKNESS:
		{
			if (!(p_ptr->resist_blind))
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
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
				if (summon_specific(py, px, dun_level, 0, 0))
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
				if (summon_specific(py, px, dun_level, SUMMON_UNDEAD, 0))
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
                        recall_player();
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
                        if (!enchant_spell(0, 0, 1, 0)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
                        if (!enchant_spell(1, 0, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
                        if (!enchant_spell(0, 1, 0, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}
                             
                case SV_SCROLL_ENCHANT_WEAPON_PVAL:
		{
                        if (!enchant_spell(0, 0, 0, 1)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
                        if (!enchant_spell(0, 0, randint(3) + 2, 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
                        if (!enchant_spell(randint(3), randint(3), 0, 0)) used_up = FALSE;
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

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && !(p_ptr->inside_quest) && dun_level)
				destroy_area(py, px, 15, TRUE);
			else
				msg_print("The dungeon trembles...");
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
			acquirement(py, px, 1, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(py, px, randint(2) + 1, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ARTIFACT:
		{
			(void) artifact_scroll();
			ident = TRUE;
			break;
		}
                case SV_SCROLL_REPAIR:
		{
                        (void) repair_weapon();
			ident = TRUE;
			break;
		}
                case SV_SCROLL_ETERNALITY:
		{
                        (void) object_eternality();
			ident = TRUE;
			break;
		}
                case 57:
		{
                        (void) make_item_magic();
			ident = TRUE;
			break;
		}
                case 58:
		{
                        (void) recharge_crystal();
			ident = TRUE;
			break;
		}


	}
        }else{
			/* Save screen */
			screen_save();

                        q=format("book-%d.txt",o_ptr->sval);

                        /* Peruse the help file */
                        (void)show_file(q, NULL, 0, 0);

			/* Load screen */
			screen_load();

                        used_up=FALSE;
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

	sound(SOUND_SCROLL);

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

	cptr q, s;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


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
        chance = p_ptr->stat_ind[A_INT] * 3;

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
		sound(SOUND_FAIL);
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


#ifdef USE_PYTHON
        if (perform_event(EVENT_USE_STAFF, Py_BuildValue("(ii)", o_ptr->tval, o_ptr->sval))) return;
#endif

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
                case SV_STAFF_WISHING:
		{
                        make_wish();
                        ident = TRUE;
			break;
		}

		case SV_STAFF_DARKNESS:
		{
			if (!(p_ptr->resist_blind))
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
				if (summon_specific(py, px, dun_level, 0, 0))
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
			if (set_image(0)) ident = TRUE;
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
			if (set_poisoned(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
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
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && !(p_ptr->inside_quest) && dun_level)
				earthquake(py, px, 10);
			else
				msg_print("The dungeon trembles...");
			ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && !(p_ptr->inside_quest) && dun_level)
			{
				destroy_area(py, px, 15, TRUE);
				ident = TRUE;
			}
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

	int			item, lev, ident, chance, dir, sval;

	object_type		*o_ptr;

	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

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
		msg_print("You must first pick up the wands.");
		return;
	}


	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return;


	/* Take a turn */
        if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_HELLQUEEN))
        {
                energy_use = 75;
                if (p_ptr->lev>=35) energy_use = 33;
                else if (p_ptr->lev>=15) energy_use = 50;
        }
        else energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Get the level */
	lev = k_info[o_ptr->k_idx].level;

	/* Base chance of success */
        chance = p_ptr->stat_ind[A_INT] * 3;

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
		sound(SOUND_FAIL);
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


#ifdef USE_PYTHON
        if (perform_event(EVENT_AIM_WAND, Py_BuildValue("(ii)", o_ptr->tval, o_ptr->sval))) return;
#endif
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
                        fire_ball(GF_POIS, dir, (p_ptr->lev * 15), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
                        fire_bolt_or_beam(20, GF_MISSILE, dir, 15);
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
                        fire_bolt_or_beam(20, GF_ACID, dir, (p_ptr->lev * 15));
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
                        fire_bolt_or_beam(20, GF_FIRE, dir, (p_ptr->lev * 25));
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
                        fire_bolt_or_beam(20, GF_COLD, dir, (p_ptr->lev * 25));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
                        fire_ball(GF_ACID, dir, (p_ptr->lev * 30), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
                        fire_ball(GF_ELEC, dir, (p_ptr->lev * 30), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
                        fire_ball(GF_FIRE, dir, (p_ptr->lev * 30), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
                        fire_ball(GF_COLD, dir, (p_ptr->lev * 30), 2);
			ident = TRUE;
			break;
		}

                case SV_WAND_WALL_CREATION:
		{
                        project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
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
                        fire_ball(GF_FIRE, dir, (p_ptr->lev * 40), 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
                        fire_ball(GF_COLD, dir, (p_ptr->lev * 40), 3);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
                                        fire_ball(GF_ACID, dir, (p_ptr->lev * 40), 3);
					break;
				}

				case 2:
				{
                                        fire_ball(GF_ELEC, dir, (p_ptr->lev * 40), 3);
					break;
				}

				case 3:
				{
                                        fire_ball(GF_FIRE, dir, (p_ptr->lev * 40), 3);
					break;
				}

				case 4:
				{
                                        fire_ball(GF_COLD, dir, (p_ptr->lev * 40), 3);
					break;
				}

				default:
				{
                                        fire_ball(GF_POIS, dir, (p_ptr->lev * 40), 3);
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

		case SV_WAND_ROCKETS:
		{
			msg_print("You launch a rocket!");
                        fire_ball(GF_ROCKET, dir, (p_ptr->lev * 50), 2);
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

        int                 dir;
	s32b dam;

	object_type		*o_ptr;
	object_type		*q_ptr;

        /* First, let's check if the player is wearing a rod */
        o_ptr = &inventory[INVEN_WIELD];
	q_ptr = &inventory[INVEN_WIELD+1];
        if ((o_ptr->tval != TV_ROD) && (q_ptr->tval != TV_ROD))
        {
                msg_print("You must wear a rod first!");
        }
        else
        {
                /* Now, check for a crystal */
                o_ptr = &inventory[INVEN_TOOL];
                if (o_ptr->tval != TV_CRYSTAL || o_ptr->pval <= 0)
                {
                        msg_print("You must wear a charged crystal!");
                }
                else
                {
			dam = o_ptr->branddam * (p_ptr->skill[17] + 1);
                        if (!get_aim_dir(&dir)) return;
			fire_ball(o_ptr->brandtype, dir, dam, o_ptr->brandrad);
                        energy_use = 100;

                }
        }

}

/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
        u32b f1, f2, f3, f4;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Enchant some bolts
 */
bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (o_ptr->art_name || artifact_p(o_ptr) || ego_item_p(o_ptr))
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
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(void)
{
        int             item, i, k, dir, lev, chance,ii,ij,plev=p_ptr->lev;
        char ch,spell_choice;

	object_type     *o_ptr;

	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
        command_see = TRUE;
        command_wrk = USE_EQUIP;
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

#ifdef USE_PYTHON
        if (perform_event(EVENT_ACTIVATE_ART, Py_BuildValue("(iiiii)", o_ptr->tval, o_ptr->sval, o_ptr->name1, o_ptr->name2, o_ptr->xtra2))) return;
#endif

        if((o_ptr->tval != TV_RANDART)&&(o_ptr->tval != TV_EGG))
                if(item < INVEN_WIELD){msg_print("You must wear it to activate it.");return;}

	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
        if (artifact_p(o_ptr)){
                if (o_ptr->tval == TV_RANDART) {
                        lev = random_artifacts[o_ptr->sval].level;
                } else {
                        lev = a_info[o_ptr->name1].level;
                }
        }

	/* Base chance of success */
        chance = p_ptr->stat_ind[A_INT] * 3;

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
		sound(SOUND_FAIL);
		return;
	}

        if ((o_ptr->tval == TV_EGG)&&(o_ptr->timeout))
        {
                msg_print("You resume the development of the egg...");
                o_ptr->timeout = 0;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
        }

	/* Check the recharge */
        if ((o_ptr->timeout)&&((o_ptr->name2!=EGO_MSTAFF_POWER)||o_ptr->pval))
	{
		msg_print("It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

        if (o_ptr->tval == TV_EGG)
	{
                msg_print("You stop the development of the egg.");
                o_ptr->timeout = 1;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

        else if (o_ptr->tval == TV_AMULET && o_ptr->sval == 18)
	{
                cptr str;
                msg_print("Your skin become very pale...You no longer have blood...YOU TURNED INTO A VAMPIRE!!");
                p_ptr->prace = RACE_VAMPIRE;
                rp_ptr = &race_info[p_ptr->prace];
                str = rp_ptr->title;
                c_put_str(TERM_L_BLUE, str, 1, 0);
                get_history();
        }

        else if (o_ptr->tval == TV_CROWN && o_ptr->sval == SV_PROMOTION_CROWN)
	{
                if (p_ptr->psex == SEX_MALE && p_ptr->stat_ind[A_STR] >= 60 && p_ptr->stat_ind[A_CON] >= 60 && p_ptr->stat_ind[A_DEX] >= 40)
                {
                cptr str;
                msg_print("You are promoted to a Battle Master!");
                p_ptr->pclass = CLASS_MASTER;
                cp_ptr = &class_info[p_ptr->pclass];
                str = cp_ptr->title;
                update_and_handle();
                msg_print("Your crown become a normal crown...You can no longer use it!");
                o_ptr->sval = 10;
                }
                else msg_print("You activate it...no effects! At least for now...");
        }
        else if (o_ptr->tval == TV_HELM && o_ptr->sval == SV_VALKYRIAN_HELM)
	{
                if (p_ptr->psex == SEX_FEMALE && p_ptr->stat_ind[A_STR] >= 60)
                {
                cptr str;
                msg_print("You just became a Valkyrie!");
                p_ptr->pclass = CLASS_VALKYRIE;
                cp_ptr = &class_info[p_ptr->pclass];
                str = cp_ptr->title;
                update_and_handle();
                }
                else msg_print("The Helm didn't have any effects on you! Maybe is you try again with different stats...");
        }                  

        else if (o_ptr->name2 == EGO_INST_DRAGONKIND)
	{
                fire_ball(o_ptr->pval2, 5, 300, 7);

                o_ptr->timeout = 100;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	else if (o_ptr->name2 == EGO_TRUMP)
	{
		teleport_player(100);
		o_ptr->timeout = 50 + randint(50);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (o_ptr->name2 == EGO_JUMP)
	{
                teleport_player(10);
                o_ptr->timeout = 10 + randint(10);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        
        else if (o_ptr->name2 == EGO_NOLDOR)
	{
                detect_treasure();
                o_ptr->timeout = 10 + randint(20);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}
        else if (o_ptr->name2 == EGO_SPECTRAL)
        {
                if (!p_ptr->wraith_form)
                        set_shadow(20 + randint(20));
                else
                        set_shadow(p_ptr->wraith_form + randint(20));
                o_ptr->timeout = 50 + randint(50);

		/* Window stuff */
                p_ptr->window |= PW_INVEN | PW_EQUIP;

		/* Done */
                return;
        }

        /* Hack -- Amulet of the Serpents can be activated as well */
        if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_SERPENT))
        {
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

                msg_print("You breathe venom...");
                fire_ball(GF_POIS, dir, 100, 2);
                o_ptr->timeout = rand_int(60) + 40;

		/* Window stuff */
                p_ptr->window |= PW_INVEN | PW_EQUIP;

		/* Done */
                return;
        }

	if (o_ptr->tval == TV_RING)
	{
		switch (o_ptr->sval)
		{

                        /* Yes, this can be activated but at the cost of it's destruction */
                        case SV_RING_TELEPORTATION:
			{
                                if(get_check("This will destroy the ring, do you want to continue ?"))
                                {
                                        msg_print("The ring explode into a space distorsion.");
                                        teleport_player(200);
                                        o_ptr->k_idx = 0;
                                        inven_item_optimize(INVEN_WIELD + item);
                                }
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

void do_cmd_asmodis(object_type *o_ptr)
{
        char ch = 0;
        int energy_amount = 0;
        int dir;
        energy_amount = get_quantity("Use how much energy(energy = damages)? ", o_ptr->xtra2);
        if (!get_aim_dir(&dir)) return;
        fire_beam(GF_MANA, dir, energy_amount);
        o_ptr->xtra2 -= energy_amount;
}

/* Use a licialhyd! */
void do_cmd_use_licialhyd(void)
{
	int			item, ident, chance, k, lev;

	object_type		*o_ptr;

	cptr q, s;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to wands */
	item_tester_tval = TV_LICIALHYD;

	/* Get an item */
	q = "Use which licialhyd? ";
	s = "You have no licialhyds.";
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
	energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Analyze the staff */
	switch (o_ptr->pval2)
	{
                case LICIAL_HEALING:
		{
                        p_ptr->chp += o_ptr->pval3;
			if (p_ptr->chp > p_ptr->mhp) p_ptr->mhp = p_ptr->chp;
			msg_print("Your wounds are healed!");
			update_and_handle();
			break;
		}

		case LICIAL_MANA:
		{
                        p_ptr->csp += o_ptr->pval3;
			if (p_ptr->csp > p_ptr->msp) p_ptr->msp = p_ptr->csp;
			msg_print("Your mana is restored!");
			update_and_handle();
			break;
		}
		
		case LICIAL_STR:
		{
                        p_ptr->str_boost = o_ptr->pval3;
                        (void)set_str_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_INT:
		{
                        p_ptr->int_boost = o_ptr->pval3;
                        (void)set_int_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_WIS:
		{
                        p_ptr->wis_boost = o_ptr->pval3;
                        (void)set_wis_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_DEX:
		{
                        p_ptr->dex_boost = o_ptr->pval3;
                        (void)set_dex_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_CON:
		{
                        p_ptr->con_boost = o_ptr->pval3;
                        (void)set_con_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_CHR:
		{
                        p_ptr->chr_boost = o_ptr->pval3;
                        (void)set_chr_boost(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_RESTORE:
		{
                        (void)set_stun(0);
                        (void)set_poisoned(0);
                        (void)set_confused(0);
                        (void)set_paralyzed(0);
                        (void)set_blind(0);
                        (void)set_afraid(0);
			(void)do_res_stat(A_STR);
                        (void)do_res_stat(A_INT);
                        (void)do_res_stat(A_WIS);
                        (void)do_res_stat(A_DEX);
                        (void)do_res_stat(A_CON);
                        (void)do_res_stat(A_CHR);
                        restore_level();
			update_and_handle();
			break;
		}

		case LICIAL_PRES:
		{
			p_ptr->pres = o_ptr->pval3;
                        if (p_ptr->pres > 100) p_ptr->pres = 100;
                        (void)set_pres(o_ptr->pval3);
			update_and_handle();
			break;
		}

		case LICIAL_MRES:
		{
			p_ptr->mres = o_ptr->pval3;
                        if (p_ptr->mres > 100) p_ptr->mres = 100;
                        (void)set_mres(o_ptr->pval3);
			update_and_handle();
			break;
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Destroy a licialhyd in the pack */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Destroy a licialhyd on the floor */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
}