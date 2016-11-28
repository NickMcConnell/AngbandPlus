/* File: cmd6.c */

/* Purpose: Object commands */

/* Code for eating food, drinking potions, reading scrolls, aiming wands,
 * using staffs, zapping rods, and activating anything that can be
 * activated.  Also defines all effects of the items listed, and all
 * activations.
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


static bool activate_random_artifact(object_type * o_ptr);


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
 * Hook to determine if an object is eatable
 */
static bool item_tester_hook_eatable(object_type *o_ptr)
{
	if ((o_ptr->tval==TV_FOOD) || (o_ptr->tval==TV_CORPSE))
	   return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
	int             item, ident, lev, dam;

	object_type     *o_ptr;
        monster_race    *r_ptr;
	
	bool nofood = FALSE;

	cptr q, s;

	/* Restrict choices to food and corpses */
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

	/* Analyze the food */
	if(o_ptr->tval==TV_FOOD){
	switch (o_ptr->sval)
	     {
		case SV_FOOD_POISON:
		{
			if (!(p_resist_pois || p_ptr->oppose_pois))
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
			if (!(p_resist_blind || p_ptr->tim_res_blind))
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
			if (!p_resist_fear)
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
			if (!(p_resist_conf || p_ptr->tim_res_conf))
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
			if (!p_resist_chaos)
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
			if (!p_free_act)
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

		case SV_FOOD_WAYBREAD:
		{
			msg_print("That tastes good.");
			(void)set_poisoned(0);
			(void)hp_player(damroll(4, 8));
			ident = TRUE;
			break;
		}
		
		default:
		{
			msg_print("That tastes good.");
			ident = TRUE;
			break;
		}


	   }
	}
	else if (o_ptr->tval == TV_CORPSE)
	{
		r_ptr = &r_info[o_ptr->pval];

                /* Now effects are based on flags */

                if (r_ptr->flags9 & RF9_EAT_DRAIN_EXP)
                {
       		   msg_print("A black aura surrounds the corpse!");
		   if (p_hold_life && (rand_int(100) < 50))
		      {
			   msg_print("You keep hold of your life force!");
		      }
		   else
		      {
			   dam = damroll((r_ptr->level/2), 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
		           if (p_hold_life)
		             {
			           msg_print("You feel your life slipping away!");
			           lose_exp(dam/10);
			      }
			   else
			      {
			           msg_print("You feel your life draining away!");
			           lose_exp(dam);
			      }
    	                           nofood = TRUE;
		       }
	        }
                
		if (r_ptr->flags9 & RF9_EAT_INSANITY)
                {
		   msg_print("You feel your sanity slipping away!");
		   take_sanity_hit(damroll(r_ptr->level/2+1, r_ptr->level/10+1),
                           "eating an insane monster");
 	           nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_POISONOUS)
		{
		   if (!(p_resist_pois || p_ptr->oppose_pois))
		   {
			   set_poisoned(p_ptr->poisoned + (r_ptr->level/2) + 10);
			   nofood = TRUE;
		   }
        	}
		if (r_ptr->flags9 & RF9_EAT_SUPER_POISONOUS)
		{
		   if (!(p_resist_pois || p_ptr->oppose_pois))
		   {
			   set_poisoned(p_ptr->poisoned + (r_ptr->level/2) * 3 + 20);
			   nofood = TRUE;
		   }
        	}
		if (r_ptr->flags9 & RF9_EAT_GIVE_ESP)
			(void)set_tim_esp(p_ptr->tim_esp + randint(30));
		if (r_ptr->flags9 & RF9_EAT_HEAL_FEAR)
			(void)set_afraid(0);
		if (r_ptr->flags9 & RF9_EAT_HEAL_STUN)
			(void)set_stun(0);                       
		if (r_ptr->flags9 & RF9_EAT_HEAL_CONF)
			(void)set_confused(0);
		if (r_ptr->flags9 & RF9_EAT_ELEC_RES)
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(650) + 400);
		if (r_ptr->flags9 & RF9_EAT_ACID_RES)
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(500) + 350);
		if (r_ptr->flags9 & RF9_EAT_COLD_RES)
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(500) + 250);
		if (r_ptr->flags9 & RF9_EAT_FIRE_RES)
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(450) + 200);
		if (r_ptr->flags9 & RF9_EAT_POIS_RES)
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(400));
		if (r_ptr->flags9 & RF9_EAT_LOSE_STR)
                {
			  do_dec_stat(A_STR, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_LOSE_DEX)
                {
			  do_dec_stat(A_DEX, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_LOSE_CON)
                {
			  do_dec_stat(A_CON, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_LOSE_INT)
                {
			  do_dec_stat(A_INT, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_LOSE_WIS)
                {
			  do_dec_stat(A_WIS, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_LOSE_CHR)
                {
			  do_dec_stat(A_CHR, STAT_DEC_NORMAL);
                          nofood = TRUE;
                }
		if (r_ptr->flags9 & RF9_EAT_GIVE_STR)
			  do_inc_stat(A_STR);
		if (r_ptr->flags9 & RF9_EAT_GIVE_CON)
			  do_inc_stat(A_CON);
		if (r_ptr->flags9 & RF9_EAT_GIVE_DEX)
			  do_inc_stat(A_DEX);
		if (r_ptr->flags9 & RF9_EAT_GIVE_INT)
			  do_inc_stat(A_INT);
		if (r_ptr->flags9 & RF9_EAT_GIVE_WIS)
			  do_inc_stat(A_WIS);
		if (r_ptr->flags9 & RF9_EAT_BLIND)
			(void)set_blind(p_ptr->blind + randint(20) + 20);
		if (r_ptr->flags9 & RF9_EAT_CONF)
			(void)set_confused(p_ptr->confused + randint(20) + 20);
		if (r_ptr->flags9 & RF9_EAT_VOMIT)
		   {
        		   msg_print("This tasted foul!");
	        	   (void)set_food(PY_FOOD_STARVE - 1);
		           (void)set_poisoned(10);
        		   if (!p_free_act)
	        	   (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
              	           nofood = TRUE;
		   }
		if (r_ptr->flags9 & RF9_EAT_TELEPORT)
		   {
	        	   msg_print("You feel dizzy for a moment.");
        		   teleport_player(100);
		   }
		if (r_ptr->flags9 & RF9_EAT_BERSERKER)
		   {
	        	   hp_player(30);
        		   set_afraid(0);
                       	   set_shero(p_ptr->shero + randint(25) + 25);
        		   if (!p_ptr->fast)
        		   {
	        		   (void)set_fast(p_ptr->fast + randint(20 + (p_ptr->lev)) + p_ptr->lev);
        		   }
		   }
		if (r_ptr->flags9 & RF9_EAT_MANA)
            	   {
		   if (p_ptr->csp < p_ptr->msp)
		     {
        		     p_ptr->csp = p_ptr->msp;
	        	     p_ptr->csp_frac = 0;
		             msg_print("Your feel your head clear.");
        		     p_ptr->redraw = (PR_MANA);
		     }
		   }
		if (r_ptr->flags9 & RF9_EAT_DRAIN_MANA)
		   {
                        p_ptr->csp = 0;
	        	p_ptr->csp_frac = 0;
		        msg_print("Your feel drained.");
        		p_ptr->redraw = (PR_MANA);
		   }
		if (r_ptr->flags9 & RF9_EAT_INFLATE)
		   {
		   msg_print("This seems to blow up in your stomach...");
		   (void)set_food(p_ptr->food*5);
  	           nofood = TRUE;
		   }
		if (r_ptr->flags9 & RF9_EAT_FORGET)
		   {
		   msg_print("You suddenly think of ages past and have trouble remembering what you are doing.");
		   (void)lose_all_info();
		   nofood = TRUE;
		   }
		if (r_ptr->flags9 & RF9_EAT_BLINK)
		   {
		   msg_print("You suddenly stand somewhere else.");
		   teleport_player(10);
		   }
		if (r_ptr->flags9 & RF9_EAT_SLEEP)
		   {
		   ident=set_paralyzed(p_ptr->paralyzed + rand_int(20) + 10);
		   nofood = TRUE;
		   }
		if (r_ptr->flags9 & RF9_EAT_NEXUS)
		 {
		    if (p_resist_nexus)
		    {
		       msg_print("Your bowels scramble wildly.");
		       take_hit(damroll((r_ptr->level/5)+1, 6), "scrambled bowels");
		       nofood = TRUE;
		    }
		    else
		    {
		       s16b max1, cur1, max2, cur2, ii, jj;
		       switch (randint(7))
		       {
			  case 1:
			  case 2:
			  case 3: teleport_player(100);
				  break;

			  case 4:
			  case 5: teleport_player(10);
				  break;

			  case 6: if (rand_int(100) < p_ptr->skill_sav)
				  {
				     msg_print("You resist the effects!");
				     break;
				  }
				  /* Teleport Level */
				  teleport_player_level();
				  break;

			  case 7: if (rand_int(100) < p_ptr->skill_sav)
				  {
				      msg_print("You resist the effects!");
				      break;
				  }

				  msg_print("Your body starts to scramble...");

				  /* Pick a pair of stats, except luck */
				  ii = rand_int(6);
				  for (jj = ii; jj == ii; jj = rand_int(6)) ;

				  max1 = p_ptr->stat_max[ii];
				  cur1 = p_ptr->stat_cur[ii];
				  max2 = p_ptr->stat_max[jj];
				  cur2 = p_ptr->stat_cur[jj];

				  p_ptr->stat_max[ii] = max2;
				  p_ptr->stat_cur[ii] = cur2;
				  p_ptr->stat_max[jj] = max1;
				  p_ptr->stat_cur[jj] = cur1;

				  p_ptr->update = (PU_BONUS);
				  break;

		       } /* switch */
		    }
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

	if (!nofood)
	
	{
	
	/* Food can feed the player */
	if (p_ptr->prace == RACE_VAMPIRE)
	{
		/* Reduced nutritional benefit */
		if(o_ptr->tval==TV_CORPSE){
			switch(o_ptr->sval){
				case SV_CORPSE_CORPSE:
					(void)set_food(p_ptr->food + o_ptr->weight / 2 + 1);
					break;
				case SV_CORPSE_HEART: /* Like to devour hearts */
					(void)set_food(p_ptr->food + o_ptr->weight * 3 / 2);
					break;
				case SV_CORPSE_HEAD:
					(void)set_food(p_ptr->food + o_ptr->weight / 12 + 1);
					break;
				case SV_CORPSE_ARM:
					(void)set_food(p_ptr->food + o_ptr->weight / 5 + 1);
					break;
				case SV_CORPSE_LEG:
					(void)set_food(p_ptr->food + o_ptr->weight / 4 + 1);
					break;
			}
		}
		else (void)set_food(p_ptr->food + (o_ptr->pval / 10));
		msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
			msg_print("Your hunger can only be satisfied with fresh blood!");
	}
	else if ((p_ptr->prace == RACE_GOLEM) ||
		 (p_ptr->prace == RACE_ZOMBIE) ||
		 (p_ptr->prace == RACE_SPECTRE) ||
		 (p_ptr->prace == RACE_SKELETON))
	{
		msg_print("The food of mortals is poor sustenance for you.");
		if(o_ptr->tval==TV_CORPSE){
			switch(o_ptr->sval){
				case SV_CORPSE_CORPSE:
					(void)set_food(p_ptr->food + o_ptr->weight / 3 + 2);
					break;
				case SV_CORPSE_HEART:
					(void)set_food(p_ptr->food + o_ptr->weight / 12 + 1);
					break;
				case SV_CORPSE_HEAD:
					(void)set_food(p_ptr->food + o_ptr->weight / 10 + 1);
					break;
				case SV_CORPSE_ARM:
					(void)set_food(p_ptr->food + o_ptr->weight / 8 + 1);
					break;
				case SV_CORPSE_LEG:
					(void)set_food(p_ptr->food + o_ptr->weight / 8 + 1);
					break;
			}
		}
		else set_food(p_ptr->food + ((o_ptr->pval) / 20));
	}
	else
	{
		if(o_ptr->tval==TV_CORPSE){
			switch(o_ptr->sval){
				case SV_CORPSE_CORPSE:
					(void)set_food(p_ptr->food + o_ptr->weight * 2 + 1);
					break;
				case SV_CORPSE_HEART:
					(void)set_food(p_ptr->food + o_ptr->weight * 1 / 6 + 1);
					break;
				case SV_CORPSE_HEAD:
					(void)set_food(p_ptr->food + o_ptr->weight * 2 / 5 + 1);
					break;
				case SV_CORPSE_ARM:
					(void)set_food(p_ptr->food + o_ptr->weight * 2 / 3 + 1);
					break;
				case SV_CORPSE_LEG:
					(void)set_food(p_ptr->food + o_ptr->weight + 1);
					break;
			}
		}
		else 
		{
		(void)set_food(p_ptr->food + o_ptr->pval);
		if (p_ptr->sign == SIGN_TAURUS)
		   (void)set_food(p_ptr->food + p_ptr->food / 10);
		}
	}

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
        
        /* Time passes... */
        sc_time += randint(o_ptr->weight * 3);
}




/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
	int             item, ident, lev;

	object_type     *o_ptr;

	cptr q, s;

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
			if (!(p_resist_pois || p_ptr->oppose_pois))
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
			if (!(p_resist_blind || p_ptr->tim_res_blind))
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
			if (!((p_resist_conf) || (p_resist_chaos)))
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
			if (!p_free_act)
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
			if (!p_hold_life && (p_ptr->exp > 0))
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
			if (!p_ptr->prace == RACE_GOLEM)
			{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			}
			break;
		}

		case SV_POTION_DEATH:
		{
			if (!(p_ptr->prace == RACE_GOLEM ||
			     p_ptr->prace == RACE_SKELETON ||
			     p_ptr->prace == RACE_ZOMBIE ||
			     p_ptr->prace == RACE_SPECTRE ||
			     p_ptr->prace == RACE_VAMPIRE))
			{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			ident = TRUE;
			}
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
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
/* Hah!			if (set_poisoned(0)) ident = TRUE; */
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
/*			if (set_poisoned(0)) ident = TRUE; */
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
/* 			if (set_poisoned(0)) ident = TRUE; */
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_stone(0)) ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
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
			if (hp_player(50)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			if (set_stone(0)) ident = TRUE;
			break;
		}

		case SV_POTION_INVULNERABILITY:
		{
			(void)set_invuln(p_ptr->invuln + randint(7) + 7);
			ident = TRUE;
			break;
		}

		case SV_POTION_NEW_LIFE:
		{
			do_cmd_rerate();
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
				msg_print("You are cured of all mutations.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			ident = TRUE;
			break;
		}
		case SV_POTION_BLOOD:
		{
			msg_print("You feel the blood of life running through your veins!");
			ident = TRUE;
			p_ptr->allow_one_death = TRUE;
			break;
		}
		case SV_POTION_INVISIBILITY:
		{
			if (set_tim_invisible(p_ptr->tim_invis + 30 + randint(30)))
				ident = TRUE;
			break;
		}
		case SV_POTION_INSANE_LIGHT:
		{
			if (heal_insanity(damroll(4, 8))) ident = TRUE;
			break;
		}
		case SV_POTION_INSANE_SERIOUS:
		{
			if (heal_insanity(damroll(6, 8))) ident = TRUE;
			break;
		}
		case SV_POTION_INSANE_CRIT:
		{
			if (heal_insanity(damroll(8, 8))) ident = TRUE;
			break;
		}
		case SV_POTION_INSANE_CURE:
		{
			msg_print("You feel sane again.");
			ident = TRUE;
			heal_insanity(5000);
			break;
		}
		case SV_POTION_EXTRA_ATTACKS:
		{
			msg_print("You swallow this strange potion...");
			if (gain_random_mutation(rand_range(81,89)))
			   ident = TRUE;
			break;
		}
		case SV_POTION_CLEAR_MIND:
		{
			if (set_oppose_conf(p_ptr->tim_res_conf + randint(10) + 10))
				ident = TRUE;
			if (set_oppose_blind(p_ptr->tim_res_blind + randint(10) + 10))
				ident = TRUE;
			break;
		}
	}

	if ((p_ptr->prace == RACE_SKELETON) && (randint(12)==1))
	{
		msg_print("Some of the fluid falls through your jaws!");
		potion_smash_effect(0, py, px, o_ptr->sval);
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
        
        /* Time passes... */
        sc_time += randint(4) + 1;
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
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
void do_cmd_read_scroll(void)
{
	int                     item, k, used_up, ident, lev;

	object_type             *o_ptr;

	char  Rumor[90] ;

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
	item_tester_tval = TV_SCROLL;

	/* Don't save while reading the scroll */
	can_save = FALSE;

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
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_HIGH_MAGE))
   {
      energy_use = 75;
      if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else if (p_ptr->pclass == CLASS_PRIEST)
   {
      energy_use = 100;
      if (p_ptr->lev>=25) energy_use = 50;
   }
   else
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
			if (!(p_resist_blind || p_ptr->tim_res_blind) && !(p_resist_dark))
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
				if (summon_specific(py, px, dun_level, 0, TRUE, FALSE, FALSE))
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
				if (summon_specific(py, px, dun_level, SUMMON_UNDEAD, TRUE, FALSE, FALSE))
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
				msg_print("You feel as if someone is relieving you.");
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			if (remove_all_curse());
			{
				msg_print("You feel as if someone is really relieving you.");
				ident = TRUE;
			}
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

		case SV_SCROLL_HARDENING:
		{
			if (!harden()) used_up = FALSE;
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

		case SV_SCROLL_REPAIR:
		{
			if (!repair_spell()) used_up = FALSE;
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
			if (detect_monsters_invis(TRUE)) ident = TRUE;
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
				msg_print("Your hands begin to glow.");
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
			if (destroy_area(py, px, 15, TRUE))
				ident = TRUE;
			else
				msg_print("The dungeon trembles...");
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

		case SV_SCROLL_SUPER_GENOCIDE:
		{
			(void)super_genocide(TRUE);
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

		case SV_SCROLL_FIRE:
		{
			fire_ball(GF_FIRE, 0, 150, 4);
			/* Note: "Double" damage since it is centered on the player ... */
			if (!(p_ptr->oppose_fire || p_resist_fire || p_immune_fire))
                                take_hit(50+randint(50)+(p_sensible_fire)?20:0, "a Scroll of Fire");
			ident = TRUE;
			break;
		}


		case SV_SCROLL_ICE:
		{
			fire_ball(GF_ICE, 0, 175, 4);
			if (!(p_ptr->oppose_cold || p_resist_cold || p_immune_cold))
                                take_hit(100+randint(100)+(p_sensible_cold)?50:0, "a Scroll of Ice");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_CHAOS:
		{
			fire_ball(GF_CHAOS, 0, 222, 4);
			if (!p_resist_chaos)
				take_hit(111+randint(111), "a Scroll of Logrus");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RUMOR:
		{
			errr err = 0;
                        
			msg_print("There is message on the scroll. It says:");
			msg_print(NULL);
			switch(randint(20))
			{
				case 1:
					err = get_rnd_line("chainswd.txt",  Rumor);
					break;
				case 2:
					err = get_rnd_line("error.txt", Rumor);
					break;
				case 3:
				case 4:
				case 5:
					err = get_rnd_line("death.txt",  Rumor);
					break;
				default:
					err = get_rnd_line("rumors.txt", Rumor);
					break;
			}
                        
			/* An error occured */
			if (err) strcpy(Rumor, "Some rumors are wrong.");
                        
			msg_format("%s", Rumor);
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ARTIFACT:
		{
			if (!artifact_scroll()) used_up = FALSE;
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
	if (!used_up) 
        {
	        can_save = TRUE;
                return;
        }
        
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
        
        can_save = TRUE;
        
        /* Time passes... */
        sc_time += randint(10);
       
}







/*
 * Use a staff.                 -RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
void do_cmd_use_staff(void)
{
	int                     item, ident, chance, k, lev;

	object_type             *o_ptr;

	cptr q, s;

	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


	/* Restrict choices to staffs */
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
    if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_HIGH_MAGE))
    {
       energy_use = 75;
       if (p_ptr->lev>=35) energy_use = 33;
       else if (p_ptr->lev>=15) energy_use = 50;
    }
    else energy_use = 100;

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

	/* Give everyone a (slight) chance, better for rings and amulets */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		if ((!(o_ptr->name1)) && ((o_ptr->tval == TV_RING) ||
			(o_ptr->tval == TV_AMULET)))
		{
			if (randint(3) >= 2) chance = USE_DEVICE * 2;
		}
		else if ((chance < USE_DEVICE) &&
			(rand_int(USE_DEVICE - chance + 1) == 0))
		{
			chance = USE_DEVICE;
		}
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


	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (!(p_resist_blind || p_ptr->tim_res_blind) && !(p_resist_dark))
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
				if (summon_specific(py, px, dun_level, 0, TRUE, FALSE, FALSE))
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
			if (detect_monsters_invis(TRUE)) ident = TRUE;
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

		case SV_STAFF_POLYMORPH:
		{
			if (polymorph_player(0)) ident = TRUE;
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
			(void)genocide(TRUE);
			ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
				earthquake(py, px, 10);
			else
				msg_print("The dungeon trembles...");
			ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			if (destroy_area(py, px, 15, TRUE))
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
        
        /* Time passes... */
        sc_time += randint(10);       
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
	int                     item, lev, ident, chance, dir, sval;

	object_type             *o_ptr;

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
        if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_HIGH_MAGE))
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

		case SV_WAND_ROCKETS:
		{
			msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir, 75 + (randint(50)), 2);
			ident = TRUE;
			break;
		}

		case SV_WAND_STRIKING:
		{
			fire_bolt(GF_METEOR, dir, damroll(13, 9));
			break;
		}

		case SV_WAND_STORMS:
		{
			fire_bolt(GF_WATER, dir, damroll(15, 10));
			fire_bolt(GF_ELEC, dir, damroll(15, 10));
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

        /* Time passes... */
        sc_time += randint(6);
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

	object_type             *o_ptr;
	object_kind 		*k_ptr;

	cptr q, s;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

	
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
		k_ptr = &k_info[o_ptr->k_idx];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
		k_ptr = &k_info[o_ptr->k_idx];
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
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_HIGH_MAGE))
   {
      energy_use = 75;
      if (p_ptr->lev>=35) energy_use = 33;
      else if (p_ptr->lev>=15) energy_use = 50;
   }
   else energy_use = 100;

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
		sound(SOUND_FAIL);
		return;
	}

	/* A single rod is still charging */
	if ((o_ptr->number == 1) && (o_ptr->timeout))
	{
		if (flush_failure) flush();
		msg_print("The rod is still charging.");
		return;
	}
	
	/* A stack of rods lacks enough energy. */
	else if ((o_ptr->number > 1) && (o_ptr->timeout > o_ptr->pval - k_ptr->pval))
	{
		if (flush_failure) flush();
		msg_print("The rods are all still charging.");
		return;
	}

	/* Increase the timeout by the rod kind's pval. -LM- */
	o_ptr->timeout += k_ptr->pval;

	/* Sound */
	sound(SOUND_ZAP);


	/* Analyze the rod */
	switch (o_ptr->sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps()) ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors()) ident = TRUE;
			if (detect_stairs()) ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			ident = TRUE;
			if (!ident_spell()) use_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			recall_player();
			ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
			map_area();
			ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all();
			ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
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
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 75)) ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(6, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(3, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(8, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(5, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 32, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 72, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 48, 2);
			ident = TRUE;
			break;
		}

		case SV_ROD_HAVOC:
		{
			call_chaos();
			ident = TRUE;
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
		o_ptr->timeout -= k_ptr->pval;
		return;
	}
	
        /* Time passes... */
        sc_time += randint(4);
}




/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
	u64b f1, f2, f3;

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
void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			msg_print("You are surrounded by a malignant aura.");
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
			msg_print("You are surrounded by a powerful aura.");

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
	int             item, i, k, dir, lev, chance;

	object_type     *o_ptr;

	cptr q, s;

	u64b f1, f2, f3;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item? ";
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
	energy_use = 100;

	object_flags(o_ptr, &f1, &f2, &f3);

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr)) lev = a_info[o_ptr->name1].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* So does stunning */
	if (p_ptr->stun) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE) ||
	    ((o_ptr->ident & IDENT_CURSED) && (randint(3) != 1)))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		sound(SOUND_FAIL);
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msg_print("It whines, glows and fades...");
		return;
	}


	/* Activate the artifact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

	if (o_ptr->art_name)
	{
		(void) activate_random_artifact(o_ptr);

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
			case ART_GALADRIEL:
			{
				msg_print("The phial wells with clear light...");
				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = rand_int(10) + 10;
				break;
			}

			case ART_ELENDIL:
			{
				msg_print("The star shines brightly...");
				map_area();
				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case ART_THRAIN:
			{
				msg_print("The Arkstone flashes bright red!");
				wiz_lite();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();

				if (get_check("Activate recall? "))
				{
					recall_player();
				}

				o_ptr->timeout = rand_int(20) + 20;
				break;
			}


			case ART_CARLAMMAS:
			{
				msg_print("The amulet lets out a shrill wail...");
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint(25) + k);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_INGWE:
			{
				msg_print("The amulet floods the area with goodness...");
				dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case ART_BARAHIR:
			{
				msg_print("You order Frakir to strangle your opponent.");
				if (!get_aim_dir(&dir)) return;
				if (drain_life(dir, 100))
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}


			case ART_TULKAS:
			{
				msg_print("The ring glows brightly...");
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

			case ART_BOROMIR:
			{
				msg_print("You wind a mighty blast; your enemies tremble!");
				(void)fear_monster(dir, (3 * p_ptr->lev / 2) + 10);
				o_ptr->timeout = rand_int(40) + 40;
				break;
			}
			case ART_FARAMIR:
			{
				msg_print("You exterminate small life.");
				(void)dispel_monsters(4);
				o_ptr->timeout = rand_int(55) + 55;
				break;
			}

			case ART_NARYA:
			{
				msg_print("The ring glows deep red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 120, 3);
				o_ptr->timeout = rand_int(225) + 225;
				break;
			}

			case ART_NENYA:
			{
				msg_print("The ring glows bright white...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 200, 3);
				o_ptr->timeout = rand_int(325) + 325;
				break;
			}

			case ART_VILYA:
			{
				msg_print("The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 250, 3);
				o_ptr->timeout = rand_int(425) + 425;
				break;
			}

		/* The Stone of Lore is perilous, for the sake of game balance. */
		case ART_STONE_LORE:
		{
			msg_print("The stone reveals hidden mysteries...");
			if (!ident_spell()) return;

			if (mp_ptr->spell_book)
			{
				/* Sufficient mana */
				if (20 <= p_ptr->csp)
				{
					/* Use some mana */
					p_ptr->csp -= 20;
				}

				/* Over-exert the player */
				else
				{
					int oops = 20 - p_ptr->csp;

					/* No mana left */
					p_ptr->csp = 0;
					p_ptr->csp_frac = 0;

					/* Message */
					msg_print("You are too weak to control the stone!");

					/* Hack -- Bypass free action */
					(void)set_paralyzed(p_ptr->paralyzed +
						randint(5 * oops + 1));

					/* Confusing. */
					(void)set_confused(p_ptr->confused +
						randint(5 * oops + 1));
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);
			}

			take_hit(damroll(1, 12), "perilous secrets");

			/* Confusing. */
			if (rand_int(5) == 0) (void)set_confused(p_ptr->confused +
				randint(10));

			/* Exercise a little care... */
			if (rand_int(20) == 0) take_hit(damroll(4, 10), "perilous secrets");
			o_ptr->timeout = 0;
			break;
			}

			case ART_POWER:
			{
				msg_print("The ring glows intensely black...");
				if (!get_aim_dir(&dir)) return;
				ring_of_power(dir);
				o_ptr->timeout = rand_int(450) + 450;
				break;
			}

			case ART_RAZORBACK:
			{
				msg_print("Your armor is surrounded by lightning...");
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_BLADETURNER:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 4);
				msg_print("Your armor glows many colours...");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + randint(50) + 50);
				(void)hp_player(30);
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
				msg_print("Your armor glows a bright white...");
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				msg_print("A heavenly choir sings...");
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_CELEBORN:
			{
				msg_print("Your armor glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				msg_print("Your armor glows bright red...");
				destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}

		case ART_HIMRING:
		{
			msg_print("A shrill wailing sound surrounds you.");
			(void)set_protevil(p_ptr->protevil +
				randint(25) + p_ptr->lev);
			o_ptr->timeout = rand_int(200) + 200;
			break;
		}

			case ART_DOR:
			case ART_GORLIM:
			{
#if 0
				for (i = 0; i < 8; i++) fear_monster(ddd[i], (p_ptr->lev)+10);
#else
				turn_monsters(40 + p_ptr->lev);
#endif
				o_ptr->timeout = 3 * (p_ptr->lev + 10);

				break;
			}

			case ART_ICANUS:
			{

				msg_print("The robe pulsates with raw mana...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, 120);
				o_ptr->timeout = rand_int(120) + 120;
				break;
			}
			
			case ART_HOLHENNETH:
			{
				msg_print("Your helm glows bright white...");
				msg_print("An image forms in your mind...");
				detect_all();
				o_ptr->timeout = rand_int(55) + 55;
				break;
			}

			case ART_GONDOR:
			{
				msg_print("Your crown glows deep blue...");
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}

			case ART_COLLUIN:
			{
				msg_print("Your cloak glows many colours...");
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
				msg_print("Your cloak glows deep blue...");
				sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

		case ART_GIL_GALAD:
		{
			msg_print("Your shield gleams with blinding light...");
			fire_ball(GF_LITE, 0, 150, 6);
			confuse_monsters(3 * p_ptr->lev / 2);
			o_ptr->timeout = 350;
			break;
		}

			case ART_THINGOL:
			{
				msg_print("Your cloak glows bright yellow...");
				recharge(60);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
				msg_print("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				msg_print("Your cloak glows a deep red...");
				restore_level();
				o_ptr->timeout = 450;
				break;
			}


			case ART_CAMMITHRIM:
			{
				msg_print("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				msg_print("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_PAURNIMMEN:
			{
				msg_print("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_PAURAEGEN:
			{
				msg_print("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_PAURNEN:
			{
				msg_print("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_FINGOLFIN:
			{
				msg_print("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 150);
				o_ptr->timeout = rand_int(90) + 90;
				break;
			}


			case ART_FEANOR:
			{
				msg_print("Your boots glow bright green...");
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
				msg_print("Your boots glow deep blue...");
				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}


			case ART_NARTHANC:
			{
				msg_print("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}

			case ART_NIMTHANC:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}

			case ART_DETHANC:
			{
				msg_print("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}

			case ART_RILIA:
			{
				msg_print("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 12, 3);
				o_ptr->timeout = rand_int(4) + 4;
				break;
			}

			case ART_BELANGIL:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2);
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}

			case ART_ANGURIEL:
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

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_RINGIL:
			{
				msg_print("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = 300;
				break;
			}

			case ART_DAWN:
			{
				msg_print("You summon the Legion of the Dawn.");
				(void)summon_specific(py, px, dun_level, SUMMON_DAWN, TRUE, TRUE, TRUE);
				o_ptr->timeout = 500 + randint(500);
				break;
			}

			case ART_ANDURIL:
			{
				msg_print("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 2);
				o_ptr->timeout = 400;
				break;
			}


			case ART_THEODEN:
			{
				msg_print("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 120);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				msg_print("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 100, 3);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				msg_print("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				msg_print("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				msg_print("Your battle axe radiates deep purple...");
				hp_player(damroll(4, 8));
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = rand_int(3) + 3;
				break;
			}

			case ART_ULMO:
			{
				msg_print("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

                        case ART_ARUNRUTH:
			{
        			msg_print("Your sword glows a pale blue...");
        			if (!get_aim_dir(&dir)) return;
        			fire_bolt(GF_COLD, dir, damroll(12, 8));
        			o_ptr->timeout = 500;
        			break;
			}
			
			case ART_AVAVIR:
			{
				if (dun_level && (p_ptr->max_dlv > dun_level))
				{
					if (get_check("Reset recall depth? "))
					p_ptr->max_dlv = dun_level;
				}

				msg_print("Your scythe glows soft white...");
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = randint(20) + 15;
					msg_print("The air about you becomes charged...");
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
				msg_print("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				o_ptr->timeout = 15;
				break;
			}

			case ART_FIRESTAR:
			{
				msg_print("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_TARATOL:
			{
				msg_print("Your mace glows bright green...");
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
				msg_print("Your quarterstaff glows yellow...");
				if (!ident_spell()) return;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				msg_print("Your quarterstaff glows brightly...");
				detect_all();
				probing();
				identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
				msg_print("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}


			case ART_CUBRAGOL:
			{
				msg_print("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}
                        case ART_GOTHMOG:
                        {
                                msg_print("Your lochaber axe erupts in fire...");
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_FIRE, dir, 300, 4);
                                o_ptr->timeout = 200+rand_int(200);
                                break;
                        }
                        case ART_PALANTIR:
                        {
                                monster_type *m_ptr;
                                monster_race *r_ptr;
                                int i;

                                msg_print("Some strange places show up in your mind. And you see ...");

                                /* Process the monsters (backwards) */
                                for (i = m_max - 1; i >= 1; i--)
                                {
                                        /* Access the monster */
                                        m_ptr = &m_list[i];

                                        /* Ignore "dead" monsters */
                                        if (!m_ptr->r_idx) continue;

                                        r_ptr = &r_info[m_ptr->r_idx];

                                        if(r_ptr->flags1 & RF1_UNIQUE)
                                        {
                                                msg_format("%s. ",r_name + r_ptr->name);
                                        }
                                }
                                o_ptr->timeout = 200;
                                break;
                        }
		        case ART_HURIN:
	    if (!p_ptr->fast)
	    {
	       (void)set_fast(randint(50) + 50);
	    }
	    else
	    {
	       (void)set_fast(p_ptr->fast + 5);
	    }
	    hp_player(30);
	    set_afraid(0);
	    set_shero(p_ptr->shero + randint(50) + 50);
	    o_ptr->timeout = rand_int(200) + 100;
	    break;

		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

                /* Time passes... */
                sc_time += randint(10);

		/* Done */
		return;
	}

	if (o_ptr->name2 == EGO_SPECTRAL)
	{
	if (!p_ptr->wraith_form)
	  set_shadow(20 + randint(20));
	else
	  set_shadow(p_ptr->wraith_form + randint(20));

	  o_ptr->timeout = 50 + randint(50);
	  p_ptr->window |= PW_INVEN | PW_EQUIP;

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
				fire_ball(GF_ELEC, dir, 100, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_WHITE:
			{
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_BLACK:
			{
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_GREEN:
			{
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_RED:
			{
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
				o_ptr->timeout = rand_int(45) + 45;
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
					  dir, 250, 2);
				o_ptr->timeout = rand_int(22) + 25;
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_GOLD:
			{
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
				o_ptr->timeout = rand_int(45) + 45;
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					   ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
					  dir, 220, 2);
				o_ptr->timeout = rand_int(30) + 30;
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					   ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
					  dir, 230, 2);
				o_ptr->timeout = rand_int(30) + 30;
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
					  dir, 250, 2);
				o_ptr->timeout = rand_int(30) + 30;
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
					   ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 200, 2);
				o_ptr->timeout = rand_int(30) + 30;
				break;
			}

			case SV_DRAGON_POWER:
			{
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 300, 3);
				o_ptr->timeout = rand_int(30) + 30;
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
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 50, 2);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 50, 2);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				o_ptr->timeout = rand_int(50) + 50;
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



static bool activate_random_artifact(object_type * o_ptr)
{
	int plev = p_ptr->lev;
	int ii = 0, ij = 0, k, dir, dummy = 0;

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
			o_ptr->timeout = rand_int(4) + 4;
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msg_print("It is covered in sparks...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ELEC, dir, damroll(4, 8));
			o_ptr->timeout = rand_int(6) + 6;
			break;
		}

		case ACT_BO_ACID_1:
		{
			msg_print("It is covered in acid...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ACID, dir, damroll(5, 8));
			o_ptr->timeout = rand_int(5) + 5;
			break;
		}

		case ACT_BO_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_COLD, dir, damroll(6, 8));
			o_ptr->timeout = rand_int(7) + 7;
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msg_print("It is covered in fire...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(9, 8));
			o_ptr->timeout = rand_int(8) + 8;
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
			o_ptr->timeout = rand_int(100) + 100;
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
			o_ptr->timeout = rand_int(90) + 90;
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msg_print("It glows deep red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 120, 3);
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_BA_COLD_3:
		{
			msg_print("It glows bright white...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 200, 3);
			o_ptr->timeout = rand_int(325) + 325;
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msg_print("It glows deep blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 250, 3);
			o_ptr->timeout = rand_int(425) + 425;
			break;
		}

		case ACT_WHIRLWIND:
		{
			{
				int y = 0, x = 0;
				cave_type       *c_ptr;
				monster_type    *m_ptr;

				for (dir = 0; dir <= 9; dir++)
				{
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

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir, 120 + (plev), 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msg_print("It fills the area with goodness...");
			dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_GOOD:
		{
			msg_print("It fills the area with evil...");
			dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_AQUATIC:
		{
			msg_print("It draws away water from the area...");
			dispel_aquatic(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_DEMONS:
		{
			msg_print("It fills the area with holy forces...");
			dispel_demons(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_LIVING:
		{
			msg_print("It fills the area with stench of death...");
			dispel_living(p_ptr->lev * 5);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 300, 4);
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
			/* Prevent destruction of quest levels and town */
			if (!is_quest(dun_level) && dun_level)
			{
				earthquake(py, px, 10);
				o_ptr->timeout = 50;
			}
			break;
		}

		case ACT_TERROR:
		{
                        int i;
                        
			for (i = 0; i < 8; i++) fear_monster(ddd[i], (p_ptr->lev)+10);
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
				msg_print("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = 250 + randint(250);
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
			(void)summon_specific(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE);
			o_ptr->timeout = 200 + randint(300);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific(py, px, dun_level, SUMMON_PHANTOM, TRUE, TRUE, TRUE);
			o_ptr->timeout = 200 + randint(200);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = (randint(3) == 1);
			bool group = !(pet && (plev < 50));

			if (summon_specific(py, px, ((plev * 3) / 2), SUMMON_ELEMENTAL, group, FALSE, pet))
			{
				msg_print("An elemental materializes...");

				if (pet)
					msg_print("It seems obedient to you.");
				else
					msg_print("You fail to control it!");
			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = (randint(3) == 1);
			bool group = !(pet && (plev < 50));

			if (summon_specific(py, px, ((plev * 3) / 2), SUMMON_DEMON, group, FALSE, pet))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}

			o_ptr->timeout = 666 + randint(333);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = (randint(3) == 1);
			bool group;
			int type;

			if (pet)
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);
				group = (((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE);
			}
			else
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
				group = TRUE;
			}

			if (summon_specific(py, px, ((plev * 3) / 2), type,
						group, FALSE, pet))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet)
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
				else
					msg_print("'The dead arise... to punish you for disturbing them!'");
			}

			o_ptr->timeout = 666 + randint(333);
			break;
		}

		/* Activate for healing */

		case ACT_CURE_LW:
		{
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msg_print("It radiates deep purple...");
			hp_player(damroll(4, 8));
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = rand_int(3) + 3;
			break;
		}

		case ACT_CURE_POISON:
		{
			msg_print("It glows deep blue...");
			(void)set_afraid(0);
			(void)set_poisoned(0);
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
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_CURE_700:
		{
			msg_print("It glows deep blue...");
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

		/* Activate for timed effect */

		case ACT_ESP:
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_shero(p_ptr->shero + randint(50) + 50);
			(void)set_blessed(p_ptr->blessed + randint(50) + 50);
			o_ptr->timeout = 100 + randint(100);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msg_print("It lets out a shrill wail...");
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint(25) + k);
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_RESIST_ALL:
		{
			msg_print("It glows many colours...");
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(40) + 40);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(40) + 40);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(40) + 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(40) + 40);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(40) + 40);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msg_print("It glows bright green...");
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + 20);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
			msg_print("It glows brightly...");
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(75) + 75);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = rand_int(200) + 200;
			break;
		}

		case ACT_WRAITH:
		{
			set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msg_print("It wells with clear light...");
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_int(10) + 10;
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msg_print("It shines brightly...");
			map_area();
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_int(50) + 50;
			break;
		}

		case ACT_DETECT_ALL:
		{
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
			detect_all();
			o_ptr->timeout = rand_int(55) + 55;
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
			msg_print("You open a dimensional gate. Choose a destination.");
			if (!tgt_pt(&ii,&ij)) return FALSE;
			p_ptr->energy -= 60 - plev;
			if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
			    (distance(ij,ii,py,px) > plev + 2) ||
			    (!rand_int(plev * plev / 2)))
			{
				msg_print("You fail to exit the astral plane correctly!");
				p_ptr->energy -= 100;
				teleport_player(10);
			}
			else teleport_player_to(ij,ii);
			o_ptr->timeout = 100;
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
			if (dun_level && (p_ptr->max_dlv > dun_level))
			{
				if (get_check("Reset recall depth? "))
				p_ptr->max_dlv = dun_level;
			}

			msg_print("It glows soft white...");
			if (!p_ptr->word_recall)
			{
				p_ptr->word_recall = randint(20) + 15;
				msg_print("The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			o_ptr->timeout = 200;
			break;
		}

		default:
		{
			msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
			return FALSE;
		}
	}

	return TRUE;
}

